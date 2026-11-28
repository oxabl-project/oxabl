# One lexed and parsed unit for each text

Issue #502 (part of #498). Status: design, and the first and the third slice
are done ([Slices](#slices)).

This design makes each run of oxabl lex and parse each distinct text one time.
A run is one `oxabl check` (all its roots), one pass of `check --watch`, or one
revision of a buffer in the daemon. A **unit** is a text with its tokens, its
tree and, when a stage asks, its line table (`SourceMap`).

## The problem

Each stage lexes and parses the text that it gets. No stage can give its tokens
or its tree to the next stage. Lexing and parsing are 38% of the CPU of a
whole-workspace `oxabl check` on a real workspace (#498). Include text is 90%
of all lexed bytes, and the 10 most shared includes are 53%.

### What `oxabl check` lexes and parses today

The table gives the full-file calls of `tokenize` and `parse_program` for one
root, with the function that calls them. The numbers are callgrind counts of
the release binary at commit `89ab4c3`, with `-j 1`.

| Text | Lexed by | Parsed by | When |
| --- | --- | --- | --- |
| The root as written | `oxabl_index::may_be_expanded` | the same | The `SHARED` producer scan, for a root with a `{` (#501, #574) |
| The root as written | `oxabl_index::Written::parse` | the same | The producer scan, for a root that may say `NEW SHARED`; and the index, when another root `RUN`s the root or names its class |
| The root as written | `oxabl_formatter::format_source_inner` | the same | The format check |
| The format candidate | `oxabl_formatter::guard::preserves_with_input_tokens` | not parsed | The format check |
| The expanded root | `oxabl_analyze::collect_from_expanded_with_summaries` | the same | The analysis and the lint rules |
| The expanded root | `ResolveWalker::receiver_is_no_class` (`names_after_widget_keywords`) | not parsed | The resolve pass, for a bare receiver that does not resolve in a file with a `USING` |
| Each `RUN` target and class the root reaches | `Written::parse` (and the expanded text of the target, when its own text does not give its facts) | the same | `BatchIndex::facts_for`, one time for each **thread** that reaches the file |
| Each `RUN` target the root reaches with arguments (`strict.calls`) | `program_signatures_at` | the same | `BatchIndex::signatures_of`, only when the parse of `facts_for` did not give the parameter lists: they are not whole in the text as written, or the target is a class |

The table is the state before the first slice. Since the third slice, a root
that expands to itself is one text: the format check and the analysis read the
lex and the parse that the analysis made, and the two rows for the root as
written and the expanded root are one lex and one parse.

The floor is one lex and one parse of each distinct text: the expanded root,
the root as written (the formatter must see the raw text, KTD4), and each
indexed file. The format candidate is a new text, so it adds one lex when it is
not the root.

Measured before the first slice (instructions, callgrind, `-j 1` unless the row
says otherwise):

| Run | Instructions | `tokenize` full calls | `parse_program` full calls |
| --- | --- | --- | --- |
| `check resources/lsp_bench.p -I resources` | 94.9 M | 4 | 3 |
| `check big.p` (1 MB, generated) | 1,191.6 M | 5 | 3 |
| `check src`, generated `ci` workspace (300 roots) | 1,171.4 M | 1,407 | 1,107 |
| `check`, 220 roots of a real workspace, `-j 1` | 8,691 M | 1,109 | 889 |
| the same, `-j 4` | 9,010 M | 1,175 | 955 |

On the real roots, `-j 4` costs 3.7% more instructions than `-j 1`: each of
the four threads indexes the `RUN` targets and classes that its roots reach
(`Written::parse` goes from 240 to 306 calls). For one large real root,
`facts_for` is 26% to 44% of its instructions (#498).

### What the daemon lexes and parses today

For one revision of a buffer the daemon keeps the expansion (`expanded_text`)
and the diagnostics in salsa (`crates/oxabl_daemon/src/db.rs`), but no tokens
and no tree. Each feature lexes and parses again:

| Feature | Text | Lex and parse at |
| --- | --- | --- |
| Diagnostics | expanded | `oxabl_analyze::collect_from_expanded_with_summaries` |
| Navigation model (definition, highlights, semantic tokens, references in the file) | expanded | `oxabl_pipeline::navigate` (`navigation_model` query) |
| Effect summaries, effect lenses | expanded | `oxabl_pipeline::effects` |
| Inlay hints (database) | expanded | `oxabl_analyze::database_hints_from_expanded` |
| Outline, breadcrumbs | as written | `oxabl_pipeline::outline` |
| Folding, selection ranges | as written | `oxabl_pipeline::syntax` |
| Format Document | as written | `oxabl_formatter::format_source_inner` |

The diagnostics and the navigation model also run the semantic passes two
times on one expansion.

## Rules

1. **One unit for each distinct text in a run.** A stage that needs the tokens
   or the tree of a text takes them from its unit. It does not lex or parse
   the text again.
2. **Two kinds of unit, two types.** `RawUnit` holds the text as written, lexed
   and parsed with preprocessing off: every span is a byte offset in the file.
   `ExpandedUnit` holds the expanded text and its `ExpandedFile` (the map back
   to the files). The formatter takes only a `RawUnit`. The analysis takes only
   an `ExpandedUnit`. A function cannot take one for the other.
3. **A unit lives as long as its root, not as long as the run.** Peak memory
   must not grow with the number of roots. Facts (the declarations that the
   index keeps) can live for the run. Trees and tokens cannot.
4. **The same output.** Findings, format output, exit codes and the order of
   the output do not change, at any thread count.
5. **Fail closed.** A unit that a stage cannot trust (another text, tokens that
   do not end at the end of the text) is not used: the stage lexes again. It
   does not guess.

## The design

### The unit

```text
RawUnit      { text: Arc<str>, tokens: Vec<Token>, program: Program, map: OnceLock<SourceMap> }
ExpandedUnit { expanded: Arc<ExpandedFile>, tokens: Vec<Token>, program: Program, map: OnceLock<SourceMap> }
```

The unit is made by one function for each kind (`RawUnit::parse(text)`,
`ExpandedUnit::parse(expanded)`) in `oxabl_pipeline` (`src/unit.rs`). Each is
`Send + Sync + 'static`, and a static assertion fails the build when a field
stops being so. The crates below the pipeline keep functions that take tokens
and a tree (`oxabl_formatter::format_with_tokens` exists, #579;
`oxabl_analyze::collect_from_parsed` is the sibling of
`collect_from_expanded_with_summaries` that takes the tokens and the program).
They do not know the unit type.

A `RawUnit` keeps the comment table of its parse, because the formatter needs
it. The analysis does not read comments, and the statements and pools of a
parse with the table are equal to those of a parse without it
(`without_comments_skips_comment_table_but_keeps_identical_statements`), so the
analysis can read a `RawUnit`.

`oxabl_daemon::db` has an `ExpandedUnit` of its own, an inspection type for a
debug request. It is a different type. Until slice 5 the daemon names the
pipeline type by its path. Slice 5 renames the daemon's inspection type, so
that its new `expanded_unit` query, the `compute_expanded_unit` function and the
language server's `expanded_unit` module each have one meaning.

`RawUnit::parse` and `ExpandedUnit::parse` never stop. A request that an edit
may cancel makes its unit with `RawUnit::parse_between` or
`ExpandedUnit::parse_between`. They call a checkpoint after the lex, before each
top-level statement and after the parse, as `collect_from_expanded_with_summaries`
does with `WorkspaceIndex::checkpoint`. The daemon must use them (slice 5):
`edit_waits_lsp.rs` fails when a large root cannot be stopped.

`LazyRawUnit` holds the text of a root and the `RawUnit` that a stage may make
for it. See the next section for why the unit is not made up front.

### Who owns it: the command line

`CheckPass::root` (`crates/oxabl/src/main.rs`) reads the source and owns the
root's units on its worker thread. It holds a `LazyRawUnit` and gives it to the
analysis (`LintPipeline::run_with_unit`) and, after it, to the format check
(`FormatPipeline::format_unit`). The unit is dropped when the root is done,
before the worker takes the next root. No unit goes into a map that the run
keeps.

When the expansion did not change the text (no include, no directive, no
reference), the expanded text is the raw text. Then the analysis makes the
`RawUnit` and reads its tokens and its tree, and the format check finds the unit
made. The expansion says this at no cost: `ExpandedFile::is_identity` is set
where `expand_source_recording` copies the text. It is true when preprocessing
is off, and when the expansion is one chunk that is the whole root. The analysis
also checks that the text of the unit is the expanded text byte for byte
(`RawUnit::serves`, rule 5), and falls back to its own lex and parse when it is
not, or when the unit could not be made.

The unit is made by the analysis, on demand, and not up front, for two reasons.
A root with an include or a directive does not need the raw tokens until the
format check. A unit made before the analysis would keep two token vectors and
two trees alive during the analysis of such a root (rule 3). It would also copy
the text of every root into the unit, which a root that expands to itself repays
and any other root does not: on `lsp_bench.p` the copy alone was +0.6% of the
instructions. So a root that expands to itself is lexed and parsed once, by the
analysis, and any other root costs what it cost before: one lex and parse of the
expansion, then one raw lex and parse for the format check. With the analysis
off, the format check formats the text alone, once. The lexer or the parser can
panic: `LazyRawUnit` keeps the contained panic. The analysis fails the run with
it, and the format check reports it as an internal panic
(`FormatPipeline::format_lazy`). Neither stage lexes the text again, because it
would panic the same way. `check` then reports two failures and exits 1, and the
default hook prints one panic message for the root. The test site
`OXABL-TEST-PANIC:unit` in `RawUnit::parse_between` reaches this path.

The raw unit parses with the comment table, and the analysis alone parsed
without it. A panic that only the comment table causes now fails the analysis as
well as the format check. This can only add failures, never remove one: every
panic of the parse without comments also happens with them. It is accepted to
avoid a second run.

When the shared unit panics, the identity root still counts as known with no
includes in the includers map. This is a true fact from a successful expansion.
An analysis panic differs: it leaves the root unknown.

The `ExpandedUnit` for a root with includes is not used by `check` yet. The
analysis of such a root still lexes and parses its expansion inside
`collect_from_expanded_with_summaries`, one time. Nothing else in the run reads
the expansion's tokens, so the unit has no second reader here. The daemon has
several (slice 5).

### The index: `facts_for`

The index keeps facts, not units. Facts are small (names, signatures, members),
and the run reads them many times.

- **One index of facts for the run** (first slice). A walk on several threads
  gives each thread a `BatchIndex` of its own, because one index for all
  threads makes them wait on its lock (2.4 times slower at 24 threads). Each
  thread indexed every file that its roots reach. `oxabl_index::SharedFacts`
  is one map, for the run, from a normalized path to a slot: the id of the file
  and its facts in a `OnceLock`. Each thread's index still keeps its own memo
  and answers, so its answers do not change. On a miss it takes the slot of the
  run: the first index that reaches the file indexes it, and an index that
  reaches it meanwhile waits for that one. The index of a file does not call
  the index again, so a wait cannot be a deadlock. The ids come from the slot,
  so one file has one id in every thread. The ids never reach the output (the
  output of `-j 1` and `-j 16` is the same, and their ids are different). A
  second index of the run must not mint ids of its own, so
  `BatchIndex::with_shared_facts` refuses an index that has done a lookup.
- **The parameter lists of a target** (`strict.calls`) live in the same slot.
  When the parse that gives the facts of a program shows the lists of its main
  block and of all its internal procedures whole, the slot keeps them, and no
  stage reads or parses the target again. When the text as written does not
  show them (an `{include}` or an `&IF` may hold a parameter) or the target is
  a class, the first index that asks reads the target again, with its
  expansion, and the slot keeps the answer for the run. This second read is
  left work: one more parse for each such target, one time for the run.
  Every index computes the lists from the facts parse, also where nothing
  reads them: an index with no `SharedFacts` (the daemon) drops them and reads
  the target again when it asks, and a run without `strict.calls` keeps them
  in the slot until the end of the run. This is left work too; the cost is
  small (one walk of the top-level statements of each file).
- **Key.** The run assumes that no file changes while it runs, as the memo of
  each index does. So the key is the path. The daemon keys by file and disk
  revision (salsa inputs), which is the content stamp.
- **The facts of a root.** A root that another root `RUN`s or names as a class
  is indexed from its text as written. Its `RawUnit` is the same parse. When
  the worker that checks the root makes its `RawUnit` and the run's slot for
  the root is empty, the worker can fill the slot from the unit (the declare
  pass of `Written::facts`). This saves the lex and the parse of the index,
  but adds the declare pass to roots that no other root reaches. Measure the
  share of roots that the index reaches before this slice.
- **Files that are not roots** (classes and targets in an include path that
  the walk does not cover) keep their own parse in the index. It is one parse
  for the run now.

### A token cache for shared include text

Include text is 90% of the lexed bytes on a real workspace, and the 10 most
shared includes are 53%. A cache of the tokens of each include file would make
the lex of an expanded root a copy of tokens. It does not pay yet, for these
reasons:

- The lexer runs on the expanded text, not on each file. A token can start in
  one chunk and end in the next (a `{&name}` in the middle of a word, an
  include that ends without a line end). Tokens of a chunk are the tokens of
  the expanded text only when the lexer is at a token boundary at both ends of
  the chunk. The splice must check this, and lex again when it is not true.
- Token offsets are offsets in the expanded text. A cached token needs a shift
  for each place where the chunk is, and the token vector is still made for
  each root (#503: the token vector is 18% to 27% of the peak heap).
- The parse of the expanded text is not shared, and it costs about as much
  as its lex.

Measure the lex of the expanded text after the other slices. Do this slice only
if the share of lexing of include chunks that start and end at a statement
boundary is high.

### The daemon

The workspace pass of the daemon (`oxabl_daemon::methods`, the workspace
snapshot) builds the call graph with `ReverseGraph::build_with`, one
`LintPipeline` for each thread, and gives its indexes no `SharedFacts`. It
has the cost that `check` had before the first slice. A slice for it must share
the facts of the pass across its threads. The salsa inputs give the key (file
and revision) that the run key of `check` assumes away.

The daemon is the only crate that knows salsa. Two tracked queries:

```text
#[salsa::tracked(returns(clone), no_eq, lru = 16)]
fn expanded_unit(db, buffer) -> Arc<ExpandedUnit>   // reads expanded_text
#[salsa::tracked(returns(clone), no_eq, lru = 16)]
fn raw_unit(db, buffer) -> Arc<RawUnit>             // reads buffer.text
```

- Salsa keeps a value that is `Send` and `Sync`. The tokens and the tree hold
  atoms and owned values. Add a static assertion on both unit types in that
  slice, so a field that is not `Sync` does not compile.
- `no_eq`: a unit is never compared. The queries that read it (diagnostics,
  outline) are compared and back-date as today.
- The same `lru` as `expanded_text`, so the daemon keeps a unit for the 16 most
  recent buffers.
- Diagnostics, the navigation model, the effect summaries, the inlay hints read
  `expanded_unit`. The outline, folding and Format Document read `raw_unit`.
  Completion keeps its own bases (`oxabl_daemon::completion`).

### Memory

- The command line keeps a root's units only while the worker checks the root.
  This does not add to the peak: the units replace texts and trees that each
  stage made and dropped.
- `SharedFacts` keeps one copy of the facts of each indexed file. Before, each
  thread kept its own copy. On a real workspace (16 threads) the peak RSS of
  `oxabl check` went from 886–932 MB to 701–719 MB.
- The daemon keeps at most 16 units of each kind, as `expanded_text`.

### The guard

A test build counts what it lexes and parses: `oxabl_lexer`'s `test-counters`
feature counts the bytes that each lexer is given, and `oxabl_parser`'s
`test-counters` feature counts the calls of `parse_program` and the bytes that
their tokens cover. With `OXABL_TEST_LEX_PARSE` set, the `oxabl` binary prints
the counts. `crates/oxabl/tests/check_lex_parse_counts.rs` runs `oxabl check`
and fails when:

- a root that expands to itself and is formatted is lexed more than once
  (with the short texts that some passes lex alone, 1.5 times its size) or
  parsed more than once;
- the format candidate of such a root with drift is not lexed, or the root is
  parsed more than once;
- a root with an include is lexed or parsed more than once as expanded and once
  as written (the raw text of the root is short in the fixture, and the test
  checks that the include was expanded);
- a `RUN` target that many roots reach is parsed more than one time for the
  run, at `-j 1` and at `-j 4`, with the default rules and with
  `strict.calls` and arguments on the `RUN` statements.

`crates/oxabl_pipeline/tests/raw_unit.rs` tests the unit itself: that the stages
of an identity root together lex it once, that their answers equal those of the
separate stages, that a root with an include does not make its raw unit for the
analysis, that a unit serves only its own text, and that the format through a
unit equals the format of the text.

The guard does not cover `check --watch`: a pass of the watch ends on a
signal and prints no counts. `with_shared_facts` on the handles of
`crates/oxabl/src/watch.rs` has no test of its own.

Each slice makes the bounds smaller.

## Slices

1. **Done.** `check` and each pass of `check --watch` share the facts of the
   indexed files across threads (`SharedFacts`). The format check does not lex
   a candidate that is the source. The resolve pass reads the tokens of its
   context, and does not lex the expanded text again. The counters and the
   guard. Each change can merge alone, and the three are one pull request.
2. **The producer scan.** #581 (a token scan for `may_be_expanded`) and #582
   (one expansion, no barrier). After these, no stage parses a root for the
   scan.
3. **Done, for the unit of a root that expands to itself.** `CheckPass::root`
   holds a `LazyRawUnit`. The analysis makes the `RawUnit` when the expansion
   did not change the text and reads its tokens and tree; the format check
   (`format_with_tokens`) reads the unit when it is there. The `ExpandedUnit` of
   a root with includes has no second reader in `check`, so `check` does not
   make it. Slice 5 uses it. TX004 does its work for a root twice (see
   [The TX004 pass](#the-tx004-pass)); that part of #251 is not done.
4. **The facts of a root from its raw unit**, if the measure in
   [The index](#the-index-facts_for) says it pays.
5. **The daemon units** (`expanded_unit`, `raw_unit`). One lex and one parse
   of the buffer for each revision, for any number of feature requests. Add
   the daemon bench of #499 (diagnostics, the outline and the links of one
   revision) first.
6. **A token cache for include text**, only if the measure says it pays.

## The TX004 pass

#251 asks for the work of TX004 to be done once for each root. It is not, and
the reason is the design, not a missing call. TX004 needs the transitive effects
of each callee, so a first pass (`build_workspace_effect_summaries_parallel`,
`oxabl_pipeline::effects::direct`) expands, lexes and parses every root,
and builds its flow graphs, scopes and database effects, before the first root
is linted. The lint run of the root then lexes and parses its text again and
builds its flow graphs and scopes again for the rules that read them (TX001 to
TX003 and TX005 read the scopes, and TX004 reads them to find the transaction
around a `RUN`). The two passes have the same text and different results, and
one of them is not over when the other starts.

Callgrind, release binary, `-j 1 --no-format`, the generated `ci` workspace
(300 roots, no `RUN` in a transaction), instructions:

| Rules | Instructions |
| --- | --- |
| the default rules | 575 M |
| `strict.transactions` without TX004 | 955 M |
| TX004 alone | 1,674 M |
| `strict.transactions` | 1,774 M |

TX004 alone adds 1,099 M. The first pass is 881 M of the 1,674 M (its
`direct` calls are 695 M, of which the effects and their scopes are 424 M), the
lint run of the roots is 782 M, and the scopes that TX004's rule builds in the
lint run are 289 M. Doing the flow graphs and scopes once would save about the
289 M, which is 17% of the run. It would also save the lex, parse and expansion
of the first pass, about the same again. To do it, one of these must hold:

- **Keep the products.** The first pass keeps each root's tokens, tree, flow
  graphs and scopes until the lint run of the root reads them. Their size is
  the size of the workspace's analysis (rule 3 forbids it: the peak must not
  grow with the number of roots), and the lint run of a root can start only
  after the first pass ends for all roots.
- **Order the work.** Lint a root when the summaries of its callees are done
  (the strongly connected components of the call graph, as `oxabl build`
  schedules compile jobs), and compute the direct effects of a root and its lint
  run in one visit of its worker. A root's units then live from its direct
  effects to its lint run, and not longer. Callers that wait for a callee hold
  their products, and a cycle must be cut. This is a design of its own, with its
  own memory rules and its own tests of the order of the output.
- **Make the first pass smaller.** The effects need the scopes and the scopes need
  the flow graphs, so there is no cheaper first pass than the one that exists.
  A pass that kept only what the summaries read (a root's call sites and
  their transaction blocks, a few spans) would let TX004 read them and need no
  scopes in the lint run, but TX001 to TX003 and TX005 still need the scopes.

None of these is a part of the raw unit. #251 stays open with this as its
design.

## Measurements of the first slice

Callgrind, release binary, instructions. Base `89ab4c3`.

| Run | Base | Slice 1 | Change |
| --- | --- | --- | --- |
| `check resources/lsp_bench.p -I resources` | 94.9 M | 80.2 M | −15.5% |
| `check big.p` | 1,191.6 M | 1,130.3 M | −5.1% |
| `check -j 1 src`, generated `ci` (300 roots) | 1,171.4 M | 1,077.9 M | −8.0% |
| `check -j 1`, 220 real roots | 8,691 M | 8,617 M | −0.9% |
| `check -j 4`, the same 220 real roots | 9,010 M | 8,657 M | −3.9% |
| `check -j 1`, four large real roots | 2,210 / 619 / 479 / 395 M | 2,123 / 599 / 476 / 396 M | −3.9% / −3.3% / −0.6% / +0.1% |

Whole real workspace (about 10,400 roots, 16 threads, a loaded machine, two
runs each): 183–186 CPU-s and 886–932 MB peak RSS before; 168–171 CPU-s and
701–719 MB after.

The output of `check` (text and `--json`) and of `format --check` is the same,
byte for byte, with the same exit code, on: `resources`, `crates`,
`crates/oxabl/tests/fixtures`, `lsp_bench.p`, `big.p`, the generated `ci`
(also with `--include-findings 100`) and `medium` workspaces, at `-j 1` and
`-j 4`, and the real workspace at the default `-j`, at `-j 3` and with `--json`.

## Measurements of the third slice

Callgrind, release binary, instructions, `-j 1` for the walk. Base `edee92a0`.
`check-big` is `big.p` of `scripts/bench/cli.sh` (1 MB, a root that expands to
itself); `check-lsp` is `resources/lsp_bench.p` with its includes, so it does not
expand to itself; the 300 roots of the `ci` workspace have includes for the most
part (30 of them expand to themselves).

| Run | Base | Slice 3 | Change |
| --- | --- | --- | --- |
| `check big.p` | 586.5 M | 484.9 M | −17.3% |
| `check -j 1 src`, generated `ci` (300 roots) | 722.8 M | 714.6 M | −1.1% |
| `check resources/lsp_bench.p -I resources` | 44.22 M | 44.18 M | −0.1% |
| `format --check -j 1 src`, generated `ci` | 142.77 M | 142.77 M | 0 |

`big.p` goes from 3 full lexes to 2 (the root once for both stages, and the
format candidate, because `big.p` has drift). The 300 roots go from 837 to 807
full lexes. A root that does not expand to itself is lexed and parsed exactly as
it was. The change in `check-lsp` is below the noise of the hash maps.

The output of `check` (text and `--json`, with and without `--no-preprocess`)
is the same, byte for byte, with the same exit code, on: `resources`, `crates`,
`crates/oxabl/tests/fixtures`, `lsp_bench.p`, the generated `ci` workspace, and
the same workspace with `strict.transactions`, `strict.database`,
`strict.calls` and `strict.handles` on (1,828 lines of findings), at `-j 1` and
`-j 4`.
