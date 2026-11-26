# One lexed and parsed unit for each text

Issue #502 (part of #498). Status: design, and the first slice is done
([Slices](#slices)).

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
`ExpandedUnit::parse(expanded)`) in `oxabl_pipeline`. The crates below the
pipeline keep functions that take tokens and a tree
(`oxabl_formatter::format_with_tokens` exists, #579;
`collect_from_expanded_with_summaries` gets a sibling that takes the tokens and
the program). They do not know the unit type.

### Who owns it: the command line

`CheckPass::root` (`crates/oxabl/src/main.rs`) reads the source and owns the
root's units on its worker thread. It makes the `RawUnit` one time and gives a
borrow to the format check and to the index. It makes the `ExpandedUnit` in the
lint run. Both are dropped when the root is done, before the worker takes the
next root. No unit goes into a map that the run keeps.

When the expansion did not change the text (no include, no directive, no
reference), the expanded text is the raw text. Then the analysis can use the
tokens and the tree of the `RawUnit`. The preprocessor must say this at no cost
(a flag that it sets when it copies the whole text as one chunk).

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

- a root that is formatted is lexed more than two times, or parsed more than
  two times (the analysis and the format check);
- a `RUN` target that many roots reach is parsed more than one time for the
  run, at `-j 1` and at `-j 4`, with the default rules and with
  `strict.calls` and arguments on the `RUN` statements.

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
3. **The raw unit of a root.** `CheckPass::root` makes the `RawUnit` and gives
   it to the format check (`format_with_tokens`) and, when the expansion did
   not change the text, to the analysis.
4. **The facts of a root from its raw unit**, if the measure in
   [The index](#the-index-facts_for) says it pays.
5. **The daemon units** (`expanded_unit`, `raw_unit`). One lex and one parse
   of the buffer for each revision, for any number of feature requests. Add
   the daemon bench of #499 (diagnostics, the outline and the links of one
   revision) first.
6. **A token cache for include text**, only if the measure says it pays.

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
