---
title: "Strict analysis: answers to the open questions"
status: proposed
date: 2026-10-14
---

# Strict analysis: answers to the open questions

This note answers two of the three open questions in
[`strict-analysis-plan.md`](strict-analysis-plan.md): where annotations may appear, and where
diagnostics inside include files are reported. The third (PROPATH and include resolution parity
with the AVM) stays open. Section 4 records one ABL fact that a rule depends on. Section 5
lists the block semantics that the flow graph (`oxabl_flow`) models without a reference,
section 6 the handle semantics that `oxabl_handles` relies on, section 7 the buffer-scope,
transaction and lock semantics that `oxabl_scope` relies on, section 8 the database effects,
index selection and call graph that `oxabl_effects` and `oxabl_index` rely on, and section 9 the
unknown-value and record-availability semantics that `oxabl_nulls` relies on.

**How to read the confidence marks.** No OpenEdge installation or ABL reference was available
while this was written. Each claim is marked:

- **Verified** — checked against the parser, the code in this repository, or a large real
  corpus. Corpus results are proportions only.
- **Recalled** — from memory of the OpenEdge documentation. Check against a licensed install
  before relying on it.
- **Proposed** — a design choice for oxabl, not a fact about ABL.

## 1. What shipped: annotations parse

**Verified.** `@Name.` and `@Name(attr="value", ...).` now parse in the default parser path. They
are legal ABL, so this adds no syntax.

- AST: `StatementKind::Annotation(Annotation)`. An `Annotation` has a dotted `name`, an optional
  `qualifier` word, and `attributes` in source order.
- A name segment may be a reserved word (`@openapi.openedge.export`). Segments must touch each
  other, so the period that ends `@Test.` is never read as part of the name.
- An attribute value is a quoted string. An unquoted value is kept verbatim as
  `AnnotationValue::Bare` instead of a parse error. Treat `Bare` as unverified.
- `associate_annotations(&[Statement])` pairs each run of consecutive annotations with the next
  statement in the same list. Empty statements between them are skipped. A run at the end of the
  list has no target (a dangling annotation).
- A lone `@` is still a parse error, reported at the `@`. The field-format `@` in `DISPLAY` is
  unchanged, because it only occurs inside a statement.
- Semantic analysis and lint treat annotations as metadata. They resolve no names, report no
  findings, and do not count an attribute name as a use of a variable. The formatter indents an
  annotation with the member it precedes and never changes its text.

**Corpus effect (verified).** Over 10,619 checked files, `PARSE001` fell from 406 to 45, and files
with a parse error fell from 37 to 18. Every removed error was `Unexpected token At`. No
diagnostic was added, and every other rule's count is unchanged. 21 files (0.2%) hold
annotations. All of them are bare annotations (no attributes, no qualifier) directly before a
method in a class, and none is left without a target. Roughly nine in ten are the ABLUnit
`Test` annotation; the rest are `Setup` and `TearDown`.

## 2. Which statements can an annotation legally precede?

### What the compiler does

- **Recalled.** An annotation is a statement of its own. The compiler records it and does not
  attach it to the next statement. This matches how the parser models it.
- **Recalled.** The compiler ignores annotation names it does not know. It does not check what
  follows an annotation. So for an arbitrary annotation there is probably no illegal position at
  the language level, only positions where some consumer reads it.
- **Recalled, uncertain.** The ABLUnit runner reads `Test`, `Setup`, `TearDown`, `Before`,
  `After`, `BeforeClass`, `AfterClass` and `Ignore`. `Test` accepts attributes such as
  `expected`. These are runner conventions, not compiler features. Whether the compiler itself
  gives meaning to any annotation (for example `Deprecated`) is **not confirmed**. Do not rely on
  it.
- **Recalled, uncertain.** Product tooling for REST and service interfaces uses dotted names
  (`openapi.openedge.export`, `openapi.openedge.entity.primarykey`). The first can carry a
  `FILE` qualifier before the attribute list. The parser accepts this form. The corpus has no
  use of it.

### Positions seen in practice

| Position | Confidence |
|---|---|
| Before a method in a class | Verified: all annotations in the corpus. Recalled: ABLUnit documents it. |
| Before an internal procedure in a `.p` file | Recalled: procedure-based ABLUnit suites. Not seen in the corpus. |
| Before a class, constructor, user-defined function | Recalled. Not seen in the corpus. |
| Before `DEFINE TEMP-TABLE` or other `DEFINE` | Recalled: service tooling. Not seen in the corpus. |
| Before a property | Uncertain. |
| Inside a parameter list, or before an expression | Not possible. Annotations are statements, and a method's parameters are inside parentheses. |
| Before a statement inside a block (`DO`, `FOR EACH`, ...) | Parses. Meaning unknown. |

The last two rows matter for the plan. Layer 1 cannot annotate a method parameter or a value
inside an expression. That limit is the reason strictabl needs its own syntax for nullable types
in expressions.

### What oxabl should do

**Proposed.**

1. **Parser: accept an annotation anywhere a statement can start, with no diagnostic.** The
   compiler does not reject any position, so a parser error would be a false positive on legal
   code. This is what ships.
2. **Annotations outside the `strict.` namespace: never diagnose.** They belong to other tools.
3. **Annotations inside `strict.`: check them in the layer-1 checker (milestone M6), not in the
   parser.** The checker reads the pairs from `associate_annotations` and reports:
   - a `strict.*` annotation with no target (dangling, or followed only by `END`);
   - a `strict.*` annotation whose target kind does not fit (see the table);
   - an unknown name in the `strict.` namespace, which catches typos like `strict.effcts`;
   - an unknown attribute, or a `Bare` value where a quoted string is required.

   Report these as warnings in the strict-annotation rule group, off by default like the rest of
   the strict groups.
4. **Target kinds for the planned annotations:**

   | Annotation | Accepted targets |
   |---|---|
   | `@strict.effects(...)` | procedure, function, method, constructor |
   | `@strict.nullable.` | `DEFINE VARIABLE`, `DEFINE PARAMETER`, `DEFINE PROPERTY`, temp-table field definitions |
   | `@strict.owns.` | `DEFINE VARIABLE`, `DEFINE PROPERTY` |

   `DEFINE PARAMETER` is a statement inside a procedure body, so it can be annotated. A method
   parameter cannot. Closing that gap is a strictabl feature.
5. **Association rule for M6:** a run of annotations applies to the next non-empty statement in
   the same statement list. Blank lines and comments do not break the run. After preprocessing,
   the target is the first statement of an include's expansion. An annotation before an `&IF`
   targets the conditional; M6 must decide whether to look into its branches (open).

**Corpus delta of H5 and H5a (verified).** Distinct normal-path leak sites fell from 1771 to 1750
on the first corpus and from 291 to 280 on the second; none were added. Of 10 moved sites sampled,
all 10 were false positives: 3 `CREATE ... NO-ERROR` or `RUN ... PERSISTENT SET ... NO-ERROR`
followed by `IF ERROR-STATUS:ERROR`, and 7 freed under a `VALID-OBJECT` guard (mostly in
`FINALLY` blocks).

**Still to verify against a licensed install:** the compiler's treatment of an annotation in a
position it does not know, the exact list of annotations it reads itself, and whether unquoted
attribute values compile.

## 3. Where are diagnostics inside include files reported?

### Current behaviour (verified)

oxabl reports a diagnostic that comes from an include **nowhere** when it checks the file that
includes it. It reports the diagnostic in the include only when someone checks the include as its
own root. Since M1, a directory walk does that for the context-free strict rules.

- **Dropped at the includer.** The pipeline lints the expanded text, then maps each span back to
  real bytes. `resolve_diagnostic` (`crates/oxabl_analyze/src/collect.rs`) returns `None`
  for any span whose first byte is not in the root file, and `ExpandedFile::resolve_span`
  (`collect.rs`) is where that origin check happens. A semantic or lint finding inside an
  include is therefore dropped. So is a label that points into an include. A parse error inside
  an include is dropped at the same place (the parse-error loop of
  `collect_from_expanded_selecting`), although `parse_recovered` is set first. The edge set still
  learns that the parse recovered, and the CLI suppresses format drift for the file, but no
  diagnostic is shown.
- **Preprocessor diagnostics follow the same rule.** Only loud diagnostics (`is_loud`) whose own span is in
  the root file are kept, on the success path and on the error path of `expand_source_with`.
- **One exception, shown at the use site.** An unresolvable include found below the root is
  reported as `PREPROC007` at the outermost `{...}` in the root file that led to it, with the
  note "reached through this include" (`nested_unresolved_include_diagnostics`). It is reported
  once per root site and missing name. The reason is that such a gap hides symbols from the includer's own analysis.
- **Kept as data, shown on request.** The pipeline keeps the semantic and lint findings that sit
  in an include in `CollectedDiagnostics::include_origin` (`IncludeOriginDiagnostic` in
  `crates/oxabl_analyze/src/collect.rs`), apart from the root-origin `diagnostics`. Each row has the
  include's physical path, a span in the include's own bytes, and the include chain of the root that
  produced it. It is a finding about one root after expansion, so "unused" means unused in that
  root. Rows are never in `diagnostics` and never in two rows. One file that a root expands several
  times gives one row per distinct finding (same path, span, code, severity, message), listing each
  expansion. A finding in `{&name}` or `{N}` substitution text has no physical file and is not kept.
  Parse errors in an include are not kept. `oxabl check --include-findings` shows the rows; a plain
  run does not.
- **Use-site coordinates already exist.** `ExpandedFile::resolve_include_site`
  (`collect.rs`) maps any expanded span to the root-level `{...}` that brought it in, at any
  depth. The dependency section uses it for `include_site` on every edge row
  (`DependencyEdgeRow::include_site` in `crates/oxabl_analyze/src/lib.rs`).
- **Walks do not check includes as roots.** `.i` is not a root extension, so `oxabl check <dir>`
  and workspace discovery do not volunteer include files as roots
  (`ROOT_EXTENSIONS` and `is_root_file` in `crates/oxabl_workspace/src/discovery.rs`, the reasoning
  in the module docs, "R9"). A path named on the command line, or a buffer open in the language server, is a root regardless
  (`discover_path`). Since M1, `check <dir>` also lints the `.i` files of the walked tree for
  context-free rules only (see "What shipped in M1" below).
- **Checking an include as a root is deliberately quieter.** The pipeline marks it
  `SourceContext::IncludeFragment` (`LintPipeline::collect_selecting` in
  `crates/oxabl_pipeline/src/lint.rs`). Unresolved local names count as external
  (`soften_includer_dependent_misses`, called from `analyze_file` in
  `crates/oxabl_semantic/src/lib.rs`), and the unused-variable, dead-store and block-variable rules
  stay silent (`RuleMeta::requires_compilation_unit` in `crates/oxabl_lint/src/registry.rs`). A fragment
  often does not parse alone or does not see its includer's declarations.

Confirmed on a synthetic example: an unused variable, an undefined name and a syntax error inside
an include produce no finding when the includer is checked. The syntax error appears only when
the include is named directly.

**Net effect, before the context-free rules.** In a directory run, a finding that lives in an
include was never shown. For the current rules that is an acceptable loss, because the rules are
noisy in fragments. For strict rules it is not. `RUN VALUE`, a shared variable, or a missing
`NO-UNDO` in a widely included file is the kind of thing the strict groups exist to find. On a
large real corpus, about two thirds of the `RUN VALUE` sites sit in `.i` files (verified, as a
text count).

### Recommendation (proposed)

Split by whether the finding depends on the code around the include.

1. **Context-free rules report in the include, once.** A rule is context-free when its answer
   depends only on the include's own tokens. Today that is CL001, CL002, CL005 and TX006. The
   finding sits at the include's own path and line, and is the same however many files include
   it. The strict rule groups should run on every include reachable from a root in the run, even
   though plain walks skip `.i`. The run collects the set of included files from the expansions
   it already does, so no second walk is needed. A shared include is checked once, not once per
   includer.
2. **Context-dependent rules report at the use site.** A rule is context-dependent when the
   answer changes with the includer: flow, scope, transaction and handle rules from M2 onward.
   The finding is a property of one includer plus one include. Report it at the root-level
   `{...}` site (the `resolve_include_site` coordinate), with the include's path and line in the
   message, such as "via path/to/file.i:12". This is the model `PREPROC007` already uses.
3. **Never both for the same finding.** Reporting in the include and at every site would count one
   problem many times and break the baseline's "only new violations fail" promise.
4. **Baseline keys follow the report location.** An include finding is keyed by the include's
   path, the rule and the normalized line text, so it does not change when a new program starts
   including the file. A use-site finding is keyed by the includer's path, as for any other
   finding.
5. **Keep the current drop for everything else.** The existing rules (unused variable,
   undefined symbol) must keep dropping include-origin findings in the includer. Their include
   quirks are the reason R8 exists.

Work this needs, in order: carry the origin file on a finding instead of discarding it in
`resolve_diagnostic`; let the strict rule groups declare whether each rule is context-free;
collect the include set from the expansions of one run; add the "via" text for use-site
findings.

### What shipped in M1 (verified)

Recommendation 1 is implemented in a simpler form, and recommendations 2 and 3 are not needed yet,
because all four M1 rules are context-free (CL001, CL002, CL005 and TX006 are context-free over the
fragment's own text, expanded with no arguments and no includer defines; `RuleMeta::context_free`
is set for each). That is exact for a fragment that no includer parametrises. It is not exact for
one that does: see "Parametrised fragments" below.

- `oxabl check <dir>` also walks the `.i` files of the tree (`walk_include_fragments`). It checks
  each one as a file of its own, with `SourceContext::IncludeFragment`, and reports **only** the
  findings of context-free rules. Preprocessor, parse, semantic and every other lint finding of
  the fragment is dropped, so a fragment that does not parse or resolve alone adds no noise
  (`RuleSelection::ContextFree` in `oxabl_analyze`, `LintPipeline::run_context_free`).
- It reads an include file only when some context-free rule is enabled for that file under the
  configuration, so a tree without strict rules pays nothing.
- A finding in an include is reported once, in the include, at the include's own path and line,
  however many programs include it. A root's own run still drops findings that come from an
  include (`resolve_diagnostic` is unchanged), so nothing is reported twice.
- The baseline keys such a finding by the include's path.
- In the language server and the daemon, an open `.i` buffer is a root and is linted with its
  per-path configuration, so it shows the context-free findings (and the other fragment-safe
  rules, as before).
- The set of included files is the `.i` files under the walked directory, not the includes
  reachable from the roots. The reasons: it needs no second expansion pass, and it does not depend
  on include path resolution. The limits follow.

Limits of the shipped form:

- An include outside the walked directory (found through an include path that points elsewhere)
  is not checked. Neither is any include when `check` is given one file instead of a directory.
- Only the `.i` extension counts. Include files with another extension are not found.
- An include that no program includes is checked all the same.
- `files_checked` in the JSON report still counts roots. `include_files_checked` counts the
  include files, and is omitted when it is zero.

### Parametrised fragments (proposed; confidence medium)

The walk expands each fragment alone, so `{1}`, `{&name}` and `&IF DEFINED(...)` resolve against
no includer. A fragment `DEFINE {&mod} VARIABLE x ...`, included with `&mod="NEW SHARED"`, is a
`NEW SHARED` definition only in that includer. The run then reports a finding that the compiled
code does not have (TX006 on `DEFINE VARIABLE y AS INTEGER {&nu}.` when the includer passes
`NO-UNDO`), and it misses one that the compiled code has (CL005 on the `{&mod}` definition, CL001
on a `RUN VALUE` under an `&IF` that is true only in the includer). On a large real corpus a
share of the include findings sit on a line with a reference; the share is small for TX006 and
large for CL005.

M1 keeps the finding and lowers its confidence. A finding of a context-free rule in an include
fragment is lowered when the part of its statement that the rule reads can change with the
includer:

- has a help that ends with a note that it depends on the including file. TX006 chooses the rest
  from the modifier slot of the `DEFINE` (the words between `DEFINE` and `VARIABLE` or
  `TEMP-TABLE`):
  - a **literal `SHARED`** (`DEFINE {1} SHARED VARIABLE ...`, the commonest form) keeps the
    "producer and every consumer" help. The definition is one side of a pair in every includer, so
    "add `NO-UNDO` here" is the unsafe advice of section 4;
  - a **reference or `&IF` in the slot**, with no literal `SHARED`, gets a third help: "if an
    includer makes this definition `SHARED`, add `NO-UNDO` to the producer and every consumer; if
    none does, add it to the definition";
  - any other lowered finding gets the plain help. It is never the plain help for a modifier that a
    reference supplies;
- is **not autofixable**. `oxabl_lint::fragment::autofixable` is `false` for it, and the future
  autofix must ask that function first. The answer is in the help text, so it survives every
  client. Replace it with a typed field when a diagnostic gains one.

What counts (`IncludeDependence::scan` and `IncludeDependence::dependence`, in the fragment's own
text; a statement ends at a period, at a colon followed by white space, where a
`&SCOPED-DEFINE` or `&GLOBAL-DEFINE` line starts, or at a reference alone on its line):

- `{&name}`, `{n}`, `{*}` and `{file.i <args>}` always count, and so does a statement that starts
  under an `&IF` or has an `&IF` inside it. The condition of an `&IF` is not part of a statement.
- An argument-less `{file.i}` counts only when it did not resolve, or when its own text has a
  reference or an `&IF`. The pipeline answers from the expansion and from one read of the include
  file (`ExpansionProbe`). Confidence high that the rest expands the same in every includer: the
  expansion is textual, and an includer's `&GLOBAL-DEFINE` reaches a nested include only through a
  reference or an `&IF`, which count. The check does not follow includes nested in that include.
- A comment is not part of a statement. A reference alone on its line is decided from the next
  token that is not a comment, and `&UNDEFINE` takes only its name token (it has no end-of-line
  token in the lexer, so it never opens a define line). Confidence high.
- A `{&name}` after a `&SCOPED-DEFINE name ...` of the same fragment, at `&IF` depth 0 and with a
  value that holds no reference, is not a reference: it is the innermost definition, so no
  includer changes it. A later `&UNDEFINE`, or a redefinition under an `&IF` or with a reference,
  makes it count again. A TX006 finding whose slot holds only such references keeps the help that
  the rule chose from the expanded text. Confidence high.
- A `{&name}` after a `&GLOBAL-DEFINE name ...` of the same fragment still counts. Reading: an
  includer's `&SCOPED-DEFINE` of the same name, made before the include, may be found first and
  shadow the global definition, so the text is not the same in every includer. Confidence low
  (no OpenEdge install here; the precedence of a scoped over a global name is not confirmed, and
  neither is whether a scoped name of the includer reaches an include file). A later correction
  shows as a snapshot diff in `a_reference_to_a_name_the_fragment_defines_is_not_a_dependence`.
- An include guard is not a dependence: an `&IF DEFINED(x) = 0 &THEN` (or `NOT DEFINED(x)`) that
  encloses the whole file, has no `&ELSE`, and defines `x` inside; or `&IF DEFINED(x) &THEN &ELSE`
  with an empty `&THEN` and the define in the `&ELSE`. Confidence medium: it gives one
  answer for every includer that includes the file once, and an includer that already defined `x`
  gets no text at all, which is the purpose of a guard.
- `RUN VALUE(...)` (CL001) counts only for a reference or `&IF` in its target, up to the matching
  parenthesis. An argument or `NO-ERROR` cannot change whether the target is dynamic. TX006 and
  CL005 read the modifier slot, the type and the options, so a reference anywhere in the statement
  counts. Not done: a narrower scan for them. The options and the `INITIAL` or `LABEL` values are
  not separable without parsing the fragment, and a fragment often does not parse.

On a large real corpus (counts only), the lowered share of
include findings went from 1,938 of 25,531 (7.6 %) to 711 (2.8 %). CL001 went from 509 of 527 to
none, because the nested include sat in the argument list after the target. TX006 kept 439 of
24,373 lowered (76 with the shared help, 101 with the third help, 262 with the plain help); CL005
269 of 279 and CL002 3. Comments in the scan caused most of the drop in TX006 (1,098 to 502), and
local scoped defines the rest (to 439). Findings themselves did not
change. Pinned by `a_nested_include_without_arguments_lowers_a_finding_only_when_it_may_change`,
`an_include_guard_does_not_lower_the_findings_inside_it`,
`a_define_line_does_not_lower_the_definition_after_it` and
`a_lowered_tx006_help_follows_the_modifier_slot` in `oxabl_pipeline`, by the unit tests of
`oxabl_lint::fragment`, and by `the_includer_note_marks_only_what_an_includer_can_change` in
`oxabl`. A later correction shows as a snapshot diff in those.

Not done: finding the includer's arguments (the other branch is "recommendation 2" above, which
reports the finding at the use site) and so recovering the missing findings. The `run` that an
editor starts through the incremental path does not lower the confidence; the editor does not show
help.

Open points for later: how the language server shows an include finding while the includer is
open, how an include with several roots in one run picks its context for rules that are "mostly"
context-free, and whether to follow includes reachable from the roots instead of walking `.i`
files.

### Handle rules in includes (M2)

HD001 to HD005 are not context-free in the strict sense: whether an object leaks can depend on the
includer. They are registered as context-free, so that they follow the shipped M1 route: a finding
whose create or delete sits in an include is reported once, in the include, when the include is checked
alone. The analysis of a root runs over the expanded text, so the root sees the create in an include
and the delete after it together. A root's finding whose anchor sits in an include is therefore not
dropped at once. It is routed (`RuleMeta::use_site_in_includer`, set for the five rules):

- The root run keeps the finding aside, with the include file, the anchor in the include's own bytes,
  and the root's outermost `{...}` site (`CollectedDiagnostics::include_findings`).
- `LintPipeline::run` runs the include alone (`run_context_free`, as a walk does). If that run has the
  same rule at the same anchor, the include reports it, and the root stays silent. This is the "never
  both" rule of recommendation 3.
- Otherwise the include cannot see the finding. Either it creates an object whose variable the root
  defines (alone, the name does not resolve, so the object escapes), or it deletes or overwrites an
  object that the root creates (alone, there is no create). The root reports it at the `{...}` site, with "(in file.i:line, reached through this include)" in the message (the path
  is relative to the working directory when it lies below it) and
  `oxabl_lint::fragment::USE_SITE_NOTE` at the end of the help. The finding is not autofixable. Its
  baseline key is the includer's path, as for any other finding of the root.
- The run of one include is made once per run handle, however many roots include it.
- An include whose own configuration has no context-free rule on is never run alone, and its findings
  are dropped with it. This follows the per-directory overrides: a rule that the user turned off for the
  include is not shown at the use site.

Confidence: high that the two cases of the review (a create that nothing deletes, a delete of an object
that the root creates) are reported. Medium for the comparison: it is by rule and anchor, so a finding
that the include shows alone with another anchor is reported twice. Limits: the include has to be
readable through the same file system and path that the preprocessor used. The editor and the daemon
(`collect`, the incremental path) do not run the includes, so they do not show these findings. An include
that nobody walks (given as one file to `check`, outside the walked directory, or with another extension)
still counts as "shown alone", so the root stays silent about a finding that the include would show.

An include that is checked alone cannot see its includer. Its findings are therefore lower confidence
in two cases, with the help ending in `oxabl_lint::fragment::FRAGMENT_NOTE` (and not autofixable):

- HD001, HD003 and HD004 for an object still live at the end of the include's main block. The includer
  may free it later.
- HD005 always. The includer may create the widget pool.

A statement that holds a reference or an `&IF` is also lowered by the shared fragment rule above.
HD002 is not lowered: only an error path is claimed, and an includer rarely changes that.

HD005 is reported once per file, on the file's own first create. A root leaves out the creates that an
include wrote and counts only its own, so it reports even when an include creates earlier. The include
reports its own creates (lower confidence) when it is checked.

## 4. Shared definitions and `NO-UNDO`

TX006 reports a variable or temp-table defined without `NO-UNDO`, and its help used to say "add
`NO-UNDO` to the definition". A `SHARED` definition must match its `NEW SHARED` (or `NEW GLOBAL
SHARED`) producer in another file.

- **Recalled, uncertain.** The compiler checks that a `SHARED` variable or temp-table
  has the same data type and the same field list as its producer. Whether it also compares
  `NO-UNDO` is not confirmed. A mismatch would be a compile error or, for a producer in a
  procedure found at run time, a run-time error.
- **Proposed.** Assume the check includes `NO-UNDO`. For a shared definition the help is "Add
  `NO-UNDO` to the `NEW SHARED` producer and to every `SHARED` consumer", and the finding is **not
  autofixable**, because a fix would edit every consumer and the rule sees one file. The finding is
  still reported on the producer and on each consumer. If the compiler does not compare `NO-UNDO`,
  the finding becomes a plain one again, and the autofix can apply.
- The reading is pinned by `a_shared_definition_asks_for_the_producer_and_every_consumer` in
  `crates/oxabl_lint/tests/syntactic_rules/tx006_missing_no_undo.rs`; a correction shows as a
  change to that test.
- **Still to check against a licensed install:** compile a producer with `NO-UNDO` and a consumer
  without it, and the reverse, for a variable and for a temp-table.

## 5. Flow graph: block semantics modelled without a reference

`oxabl_flow` builds a control-flow graph with ABL's block rules: which block an `UNDO` undoes,
what a condition does to a block, what `CATCH` and `FINALLY` do. No ABL reference was available,
so each rule below is a reading, not a fact. Every row has a fixture in
`crates/oxabl_flow/tests/fixtures/`. If a rule turns out wrong, fix it in `oxabl_flow` and
re-bless the fixtures (`OXABL_BLESS=1 cargo test -p oxabl_flow --test snapshots`). The diff shows
every graph that changes.

The marks are the ones defined at the top. A rule marked **Changed** was different in an earlier
draft, and its row says what it was.

| # | The model | Confidence | Fixture |
|---|---|---|---|
| F1 | Default `ON ERROR` is `UNDO, RETRY` for a routine, `REPEAT`, `FOR` and `DO TRANSACTION`. Default `ON ENDKEY` is `UNDO, LEAVE` for the same blocks. A plain `DO` handles neither. STOP and QUIT have no default: they pass to the enclosing block, and out of the unit when none handles them. | Recalled. High for ERROR, medium for ENDKEY. | `phrases.p`, `loops.p` |
| F2 | `RETRY` in a block with no input-blocking statement does not retry: an iterating block goes to its next iteration, any other block is left (infinite-loop protection). | Recalled. | `statements.p`, `loops.p` |
| F3 | After a `CATCH` ends, its block goes on with the next iteration if it iterates, else it is left. | Recalled. | `defaults_catch_end.p` |
| F4 | `CATCH` and `FINALLY` blocks have an implicit `ON ERROR UNDO, THROW`. A condition raised inside them skips their owner's handlers and `CATCH` blocks. | Recalled. | `undo_catch.p`, `catch_finally.p` |
| F5 | An unlabelled `UNDO` in a `CATCH` undoes the block the `CATCH` belongs to, so `UNDO, LEAVE` leaves that block and `UNDO, NEXT` goes to its next iteration. A plain `DO` nested in the `CATCH` does not change this. The other reading, that the `CATCH` is the undone block, differs only for an iterating owner: `UNDO, LEAVE` would then end the `CATCH`, and the loop would go on. **Changed:** it used the closest block outside the owner, which left the enclosing loop. | Proposed. Low: roughly 60/40 over the other reading. | `undo_catch.p` |
| F6 | `UNDO [label], THROW` undoes the labelled block, else the closest with the error property, and raises ERROR in the block that encloses it. The undone block's own `CATCH` blocks do not see it. This is the graph of `ON ERROR UNDO, THROW`, and it makes `CATCH e: UNDO, THROW e.` a rethrow. The bare `THROW` statement raises in the current block, so its own `CATCH` sees it. `UNDO, THROW v`, `UNDO, RETURN v` and `UNDO, RETURN ERROR v` first evaluate `v` in the current block, which can raise ERROR, as `RETURN v` does. **Changed:** `UNDO, THROW` was a bare `THROW`, and lost its label. | Recalled. Medium. | `undo_throw.p`, `throws.p`, `undo_value.p` |
| F7 | `UNDO.` with no action does the `ON ERROR` action of the block it undoes, and a plain block with no action gives `RETRY`. Two readings differ when that action is `THROW` (`BLOCK-LEVEL`, `ROUTINE-LEVEL` or a phrase): the documented default of the statement is `RETRY` whatever the block says, or the block's `THROW` wins. The model keeps **both** successors: the `RETRY` outcome (in an iterating block, the next iteration) and the `THROW` outcome. A rule that reasons about "every path" then sees the loop go on and the loop exit. Taking only one outcome would hide the other. **Changed:** the first draft turned `THROW` into `RETRY`; the second kept only `THROW`, which dropped the continue-the-loop path. | Proposed. Low. | `defaults_undo_statement.p` |
| F8 | `ON <condition> UNDO` with no action takes the action of the condition: ERROR `RETRY`, ENDKEY `LEAVE`, STOP `RETRY`, QUIT `LEAVE`. **Changed:** all four were `RETRY`. For a block with no input, `RETRY` becomes `NEXT` in a loop, so the two readings differ in loop exits. | Recalled. ERROR: high. ENDKEY: medium. STOP: low. QUIT: Proposed, low. | `defaults_on_phrase.p` |
| F9 | `RETURN ERROR` is not caught by a `CATCH` in the same routine. The `FINALLY` blocks it leaves still run. | Recalled. | `defaults_catch_end.p` |
| F10 | `ROUTINE-LEVEL` and `BLOCK-LEVEL` do not apply to a destructor. They do apply to the block of an `ON ... DO` trigger, as a routine block. `BLOCK-LEVEL` also reaches `REPEAT`, `FOR` and `DO TRANSACTION`, never a plain `DO`. | Recalled, uncertain for the destructor. Proposed for triggers: not checked. | `defaults_directive_class.cls`, `defaults_directive_trigger.p`, `block_level.p`, `routine_level.p` |
| F11 | `FIND NEXT` and `FIND PREV` may raise both ERROR and ENDKEY. `IMPORT` and `PAUSE` raise ENDKEY. | Proposed for `FIND` (it is the safe side). Recalled for the others. | `input.p` |
| F12 | `FOR FIRST` and `FOR LAST` run their body once: the end of the body goes to the point after the block, not back to the header. The block still counts as iterating, so `NEXT` and `RETRY` work. A joined `EACH` (`FOR FIRST a, EACH b OF a`) iterates, and keeps its loop-back. **Changed:** every `FOR` looped back. | Recalled. Medium. | `for_first.p`, `loops.p` |
| F13 | STOP sources. Every statement under `STOP-AFTER` may raise STOP, and the header of an iterating block too. `RUN`, a `FIND` or `FOR` with a lock, and `WAIT-FOR` may raise STOP, but only where a block or `CATCH` of the unit handles STOP. STOP runs the `FINALLY` blocks it leaves, as any jump does. Not modelled: STOP from a function or method call, a user interrupt, a lock wait in other statements, and an unhandled STOP from `RUN` and the others (it leaves the unit). **Changed:** only the `STOP` statement raised STOP, so an `ON STOP` phrase on a `STOP-AFTER` block was dead. The handler-only limit keeps the graph of ordinary code unchanged. Raising STOP from every such statement adds a STOP exit to each unit that has a `RUN` or a lock wait. On a large corpus that grew the graphs by 0.8% in nodes and 1.6% in edges, and no rule asks for those paths yet. | Recalled for `STOP-AFTER` and for lock waits. Proposed for the rest. Low for the `FINALLY` rule. | `stop.p` |
| F14 | `UNDO [label], RETURN NO-APPLY.` parses (it was a parse error) and is a return: it undoes the block as `UNDO, RETURN` does, then leaves the routine. `NO-APPLY` is the keyword that cancels the UI event of a trigger. | Recalled, medium: the form is legal ABL, and the graph is that of `UNDO, RETURN`. | `undo_return_no_apply.p` |

**Still to verify against a licensed install:** F5, F7 (when it settles, keep one successor) and F8 for STOP and QUIT first, because
the readings differ in graph shape. Then F10 (destructor and trigger blocks) and F13.

## 6. Handle lifetimes: the semantics `oxabl_handles` relies on

`oxabl_handles` follows each dynamic object from its create over every path of its unit's flow
graph and gives it a verdict: freed, pool-freed, escapes, leaks or unknown. The rules below are
readings, marked as in section 5. Each has a fixture in `crates/oxabl_handles/tests/fixtures/`
(re-bless with `OXABL_BLESS=1 cargo test -p oxabl_handles --test snapshots`), and the rule
queries are pinned by `crates/oxabl_handles/tests/rules.rs`.

| # | The model | Confidence | Fixture |
|---|---|---|---|
| H1 | Class instances (`NEW`, `DYNAMIC-NEW`) are reference-counted and freed by the AVM: verdict "collected", not followed. Cycles between objects leak, and are out of scope (the plan defers them). | Recalled, high. | `create_kinds.p` |
| H2 | Handle-based objects (`CREATE QUERY`, `BUFFER`, `TEMP-TABLE`, `DATASET`, `DATA-SOURCE`, `CALL`, X-, SAX-, SOAP-, socket and server objects, `CLIENT-PRINCIPAL`), widgets, COM objects, persistent procedures and MEMPTRs are not collected. They live until deleted, until their widget pool is deleted, or until the session ends. | Recalled, high. | `create_kinds.p` |
| H3 | Frees: `DELETE OBJECT`, `DELETE PROCEDURE`, `DELETE WIDGET` (each handle of `DELETE WIDGET h1 h2`), `RELEASE OBJECT` (COM) and `SET-SIZE(m) = 0`. A delete that raises (an invalid handle) is still a free on its error edge. `h:HANDLE` is the same object as `h`, so `DELETE OBJECT h:HANDLE` is a free and `h2 = h:HANDLE` is an alias. A delete whose operand names a holder but is not the holder (`DELETE OBJECT WIDGET-HANDLE(STRING(h))`, `h:PARENT`) makes the object unknown (`unmapped-free`), never "no effect". | Recalled, high. | `create_kinds.p`, `frees.p`, `new_kinds.p`, `memptr.p`; `model_fixes.rs` |
| H4 | `APPLY "CLOSE" TO hProc` frees a persistent procedure, and no other kind of object (a window or a query is not deleted by CLOSE). This is a convention, not a language rule: the procedure's `CLOSE` trigger usually runs `DELETE PROCEDURE THIS-PROCEDURE`. `RUN destroy IN h` follows the same kind of convention but shows nothing in the caller, so it makes the object unknown, not freed. The convention is read widely: any internal procedure name that starts with `destroy` (`destroyObject`, `DestroyThis`; user names, case-insensitive), and ADM's `RUN dispatch IN h ('destroy')`. | Proposed, medium. Low for the wider names and for `dispatch`. | `frees.p`, `persistent.p`, `destroy_names.p`; `model_fixes.rs` |
| H5 | A live object's handle is valid and not unknown. Each test of a holder then has a fixed value: `VALID-HANDLE(h)`, `VALID-OBJECT(h)` and `h <> ?` are true, `h = ?` is false, `NOT` flips. An `IF` whose whole condition has a fixed value this way, through `AND`, `OR` and `NOT`, has a branch that cannot happen while `h` holds a live object: `IF VALID-HANDLE(h) THEN DELETE OBJECT h.` frees on every path, and so does `IF h <> ? AND VALID-HANDLE(h)`. The false branch of `VALID-HANDLE(h) AND x` can happen (`x` is false), so it stays. A test of another holder is no guard. `VALID-OBJECT` is taken as a guard on handles too: real code tests procedure and widget handles with it. | Recalled, high for `VALID-HANDLE`, `<> ?` and `= ?`. Medium for `VALID-OBJECT` on a handle (the corpus uses it for cleanup of handles; its behaviour on an invalid handle is not confirmed). | `guards.p`, `guards_compound.p` |
| H5a | After `CREATE ... NO-ERROR` or `RUN ... PERSISTENT SET h NO-ERROR`, the object may not exist. The branch of an `IF` that tests `ERROR-STATUS:ERROR` as true cannot hold the object, so a path that leaves there is no leak. This holds only when the `IF` is the next statement after the create: any other statement may change the error status. `NOT VALID-HANDLE(h)` after the create is H5. A create without `NO-ERROR` gets no such rule. | Proposed, medium. | `no_error.p` |
| H6 | An object created without `IN WIDGET-POOL` goes into the most recent unnamed pool, else the session pool. An unnamed pool is deleted when the procedure or trigger that created it ends; one created in the main block of a persistent procedure lives until that procedure is deleted. Model: a `CREATE WIDGET-POOL.` that dominates the create in the same unit is the unit's pool; one in the main block also covers the file's other units (the file pool). The caller's pool is not knowable: "unscoped" (HD005). | Recalled, medium. | `pools.p`, `unscoped.p` |
| H6a | A class with `USE-WIDGET-POOL` has an unnamed pool for each instance. An object a unit of the class creates without `IN WIDGET-POOL` goes there, unless a `CREATE WIDGET-POOL` of the unit dominates the create. The pool is deleted with the instance, so the object is pool-freed (placement "class"). `NEW` and static members are not told apart: a static method may use the session pool in a real run. | Proposed, low. | `class_pool.cls` |
| H7 | A named pool is session-wide by name. `DELETE WIDGET-POOL name` deletes it, and so does the end of the procedure that created it, unless it is `PERSISTENT`. A pool name no statement of the file creates belongs to someone else: the object escapes to it. | Recalled, medium. | `pools.p` |
| H8 | Widget pools hold handle-based objects and widgets, not persistent procedures, MEMPTRs, COM objects or class instances. Server and socket objects, and a `CREATE VALUE` whose type is not known, are taken as not pooled, so HD005 does not report them. | Recalled. Medium; low for the server, socket, X-, SAX- and SOAP- objects. | `pools.p`, `new_kinds.p` |
| H9 | An object left in a pool this procedure deletes is freed with it, so it is not an HD001 leak, also when a loop overwrites its handle. It can still grow memory until the procedure ends. | Proposed. | `pools.p` |
| H10 | A statement that raises does not complete: a `CREATE` that raises makes nothing, and an assignment that raises stores nothing. The assignment of a plain variable or literal to a plain variable, and `RETURN` of one, cannot raise, although the flow graph gives every assignment a raise edge. So does a test with only variable references, literals, comparisons and `AND`/`OR`/`NOT`: `IF`, `ELSE IF`, `CASE` (its `WHEN` values too), `DO WHILE`, `REPEAT WHILE`. `VALID-HANDLE(v)`, `VALID-OBJECT(v)` and `ERROR-STATUS:ERROR` read a value and do not raise, so a `FINALLY` that tests `IF VALID-HANDLE(h)` before its delete adds no raise edge. A `DO i = a TO b [BY c]` header whose bounds are variables or literals does not raise either, so an error in a loop is taken from a statement of its body, which is what HD002 names. Any other call, attribute, method, arithmetic or field in the test can raise. `h:HANDLE` counts as a plain variable. This list is for the handle walk only; `oxabl_flow` keeps the raise edges. | Proposed, high for assignments. Medium for tests (an invalid-handle read is excluded by the no-call rule, a `?` comparison on a variable is taken as safe). | `alias.p`, `escapes.p`, `cannot_raise_tests.p`, `finally.p` |
| H11 | Overwriting the last variable that holds a live object (a new create into it, `h = ?`, an `OUTPUT` argument) loses the object there. An `OUTPUT` parameter starts at its initial value in the callee, so the old value is not seen. A create in a loop whose object is still live on the next iteration is lost at that create. | Recalled for `OUTPUT`, high. Proposed for the rest. | `loops_overwrite.p`, `alias.p` |
| H12 | `SET-SIZE` on a MEMPTR that already has a size raises an error and frees nothing, so it does not lose the old allocation. Assigning a MEMPTR copies the pointer, not the memory, so both variables name one allocation. `COPY-LOB ... TO m` allocates `m` when the definition `m` resolves to from the unit (its own, an enclosing unit's, or a member) is `AS MEMPTR`. The parser skips the statement, so the target is read from its tokens. It is the same allocation as an earlier `SET-SIZE` or `COPY-LOB` of `m` that dominates it, not a second object. `COPY-LOB FROM m` is a use. | Recalled, medium for `SET-SIZE`. Proposed, low for `COPY-LOB`. | `memptr.p`; `model_fixes.rs` |
| H13 | `RUN p PERSISTENT` without `SET` keeps no handle in this file (a singleton or a self-managing super procedure): verdict "uncaptured", for HD003 to treat on its own. | Proposed. | `persistent.p` |
| H14 | An `INPUT` handle argument goes by what the callee shows. A procedure, function or method this file defines is matched by name, and its parameter by position. A method is matched only when no subclass can override it: a `PRIVATE` or `STATIC` method, or any method of a `FINAL` class. A call of any other method, or of an `ABSTRACT` one, is unknown (`callee`), because the code that runs may be a subclass's in another file; interface methods are never read. A prototype is not a definition: `PROCEDURE p IN SUPER`, `PROCEDURE p IN h` and `FUNCTION f ... IN h` / `IN SUPER` / `MAP TO` run code of another file (`PROCEDURE p EXTERNAL` is H14a). A `FUNCTION f ... FORWARD` is matched through its definition. A body whose parameter at that position is not declared `HANDLE`, `WIDGET-HANDLE`, `COM-HANDLE`, `MEMPTR` or as a table, dataset or buffer parameter (`CHARACTER`, a class, `LIKE`) is not the callee: an inherited overload may take the handle, so the object is unknown (`callee`). What the defined callee does with the parameter, or with a local variable it copies the parameter into, is read without regard to order: a free hands the object over (escape `freed-by-callee`); a store where it outlives the call (a member, an outer or shared variable, an `OUTPUT` parameter, a field, an attribute), `RETURN`, or any other escape of this list keeps it (escape `kept-by-callee`); a pass to a callee the file does not define, a skipped statement or a dynamic call that names it makes the object unknown (`callee`); otherwise the callee borrows it, a use. A pass on to another callee of the file takes that callee's fate, up to four calls deep, then unknown; a call back into a callee whose parameter is being read (recursion) adds nothing. One fate reads at most 1,000 callee bodies, then it is unknown, so callees that pass a handle to each other cost a bounded time. If the callee is not in the file (`RUN x.p`, `RUN p IN h`, a static class call), the object is unknown (`callee`): the callee may delete or keep it. Built-in functions borrow. A method call resolves by its receiver: on a variable declared `AS HANDLE` or `WIDGET-HANDLE`, a buffer, table or dataset parameter (any `TABLE`/`DATASET` form), a static buffer, temp-table, dataset, data-source or frame, a system handle, a chain of attributes from one of these, or a chain whose last link is a `...HANDLE` attribute (`BUFFER b:HANDLE`), a method that looks built in (a hyphenated name such as `SET-BUFFERS`, or `FILL`, `READ`, `WRITE`, `CONNECT`, `DISCONNECT`, `INVOKE`, `SAVE`, `LOAD`, `PARSE`, `SELECT`) borrows. A call with no receiver, on `THIS-OBJECT`, or on a variable whose declared type is this class names a method of the file, read only when the method is matched as above. Any other method call (`oRepo:Save(h)` on another class, `CAST(...)`, `SUPER:`, a name the file does not declare) is unknown. A `TABLE`, `TABLE-HANDLE`, `DATASET` or `DATASET-HANDLE` argument passes a copy, so the callee cannot free the caller's object: a borrow (the keyword is read from the text before the argument, since the parser drops it). A call with no receiver in a class may name one of the class's methods. A built-in function missing from the lexer registry is unknown too, which errs towards no report. An `INPUT-OUTPUT` argument escapes. Passing to a constructor (`NEW C(h)`), `ADD-SUPER-PROCEDURE`, `PUBLISH` and `SUBSCRIBE PROCEDURE` escapes. `ADD-BUFFER` on a query this unit creates is a use (a query does not own its buffers), and on a dataset this unit creates it is H15. On any other receiver (a parameter, a static dataset, a variable of another unit) the receiver may be a dataset, so the object is unknown (`add-buffer`; in a callee, `callee`). `SET-BUFFERS` is a use. | Proposed, medium. Recalled for queries, high. | `escapes.p`, `dataset_member.p`; `model_fixes.rs`, `callees.rs` |
| H14a | A `PROCEDURE p EXTERNAL "lib"` prototype (a shared-library entry point) borrows every `INPUT` argument. Only `SET-SIZE(m) = 0` frees an ABL MEMPTR: a library that frees the pointer corrupts the AVM's bookkeeping, and one that keeps the pointer does not free it either. A handle reaches a library only as a `LONG` or `INT64`, not as a handle. | Recalled, medium. | `callees.rs` |
| H15 | `hDs:ADD-BUFFER(h)` or `hDs:ADD-BUFFER(h:DEFAULT-BUFFER-HANDLE)`, where this unit creates a dataset into `hDs`, makes the dataset the owner. Whether deleting a dynamic dataset deletes the temp-tables of the buffers added to it is not confirmed. | Proposed, low. | `dataset_member.p` |
| H16 | Storing the handle where it outlives the unit escapes it: an `OUTPUT` parameter, `RETURN`, a class member, a variable of an enclosing unit, a shared variable, a field, an array element or `EXTENT` variable, another object's attribute. The escape records whether another unit of the file frees that variable: the name must resolve to the same definition from that unit, so a local of the same name in another unit does not count; a class-level check reads it (a member no unit frees is a leak of the class). | Proposed. | `escapes.p`, `class.cls`; `model_fixes.rs` |
| H17 | A widget put under a parent (`CREATE BUTTON h ASSIGN FRAME = f`, `PARENT = w`, or a later `h:FRAME = f`) is owned by the parent: deleting a frame or window deletes the widgets in it. The parent keeps its own owner. The deferred `TRIGGERS` bodies of a widget create are not read. | Recalled, medium. | `widgets.p` |
| H18 | A main-block variable that another unit of the file frees by name (a cleanup procedure, a trigger) owns the object at the main block's end: escape. | Proposed. | `other_unit.p`; `model_fixes.rs` |
| H19 | A `DELETE PROCEDURE` of a handle that walks a procedure list (`FIRST-PROCEDURE`, `NEXT-SIBLING`, ...) may delete any persistent procedure: unknown, not freed. | Proposed. | `sweep.p` |
| H20 | A statement the parser skips that names a holder, a dynamic call with a holder as argument (`RUN VALUE`, `DYNAMIC-FUNCTION` except its `IN` handle, `DYNAMIC-INVOKE`), or an unexpanded include, while the object lives, makes a would-be leak unknown. A file whose parse was recovered gets no HD001-HD005 finding at all: recovery can close a unit early and move a free out of it, and no statement left behind names the holder. An include fragment run alone is exempt: it is partial by nature, its parse often recovers, and its findings are reported once, in the include; a gate there would move them to every includer. | Proposed. | `opaque.p`; `recovered_parse.rs`, `handles_recovered_cli.rs` |
| H21 | A handle derived from a live object (`h:DEFAULT-BUFFER-HANDLE`, `h:HANDLE`, `h:BUFFER-HANDLE`, `h:GET-BUFFER-HANDLE(n)`, `h:TABLE-HANDLE`, also `QUERY`, `DATASET`, `PARENT`, `FRAME`, `WINDOW`; any attribute or method with `HANDLE` in its name) that reaches an escape sink (`RETURN`, an `OUTPUT` parameter, a member, an outer or shared variable, a field, an array element, another object's attribute) hands the root object over: escape `derived-handle`. The caller reaches the object through the derived handle and deletes it. A derived handle assigned to a local variable, or passed `INPUT`, is a use. Other attributes (`NUM-RESULTS`) are not derived handles. | Proposed, low. | `derived_escape.p` |
| H22 | The walk of one object stops at 100,000 states, and the objects of one file share 600,000. Past either, the object is unknown (`too-complex`), so a generated file with many aliased holders costs a bounded time. The order is the objects' order of analysis, so a file's later objects are the ones a spent budget hits. | Proposed. | `model_fixes.rs` |

Not modelled, known gaps:

- A statement between a `NO-ERROR` create and the test of `ERROR-STATUS:ERROR` ends H5a, also when it
  cannot change the status. A leak is then reported on the failed path. The test of a saved status
  (`lErr = ERROR-STATUS:ERROR` first) is not read.
- `UNDO` restores a variable without `NO-UNDO`, a handle variable included. Objects are not undone,
  and the model does not restore variables either. This is safe to leave: the only effect on a
  verdict is a miss. A transaction block that creates into a variable and then undoes the variable
  loses the handle in a real run, while the model still sees it, so a later `IF VALID-HANDLE(h)
  THEN DELETE OBJECT h` is taken as a free (a missed leak, not a false report). The opposite case
  needs an old live handle overwritten inside a block that is then undone, which is rare. Modelling
  it needs the CFG to know which edges undo which block, and the holder set to remember the value at
  each block entry. `undo.p` pins the current verdicts.
- Statements other than `SET-SIZE` and `COPY-LOB ... TO m` that allocate a MEMPTR.
- A call in a statement the effect reader does not scan (`FOR EACH ... WHERE f(h)`, `DISPLAY f(h)`) is a use of `h`, not unknown.
- `CREATE-LIKE` on a dataset, and the widget-pool option
  of a widget's `ASSIGN` phrase.
- A caller's widget pool. Each callee of the file is read without order or paths, and a pass on is
  followed up to four calls deep: a free on one branch only counts as a free, and a parameter used
  in a trigger the callee defines is not seen. A receiver typed `LIKE` another variable, a database buffer, and a member a class inherits
  are not resolved, so their method calls are unknown.
- More than 64 variables of one unit that hold the same object on one path.
- Which method body runs is read from this file only (H14). A method a subclass could override is
  unknown even when no subclass does, so a leak through it is missed. A `STATIC` method called
  through its class name (`Svc:Work(h)`) is not matched.
- A `VAR` with an access modifier in a class body (`VAR PRIVATE HANDLE h.`) does not parse as a
  declaration, so that member is not resolved.

**Corpus check (verified, proportions only).** Over two large real corpora, of 20 sampled objects
that leak on a normal path, 18 looked real on reading the code (never deleted, or skipped by an
early `RETURN` or a conditional cleanup). One was a procedure-list sweep (now H19) and one a
`VALID-OBJECT` test on a handle. Includes multiply the raw counts: one leak in a widely included
file is counted once per includer, so the corpus runner also counts distinct sites.

**Still to verify against a licensed install:** H6 and H7 (pool scope, and the persistent
procedure case), H8 for the less common object kinds, H11 for a MEMPTR passed `OUTPUT`, H12, and H15.

## 7. Buffer scopes and transactions: the semantics `oxabl_scope` relies on

`oxabl_scope` replays the compiler's decisions for one file: the block each buffer is scoped to,
the blocks that are transaction blocks, and the lock each record read takes. The `COMPILE ...
LISTING` output is the oracle (plan, M3 exit gate), and no OpenEdge install was available while
this was written. This section therefore uses its own marks:

- **Documented** — the OpenEdge documentation says this, and the wording is remembered with high
  confidence. It is still not checked on this host.
- **Recalled** — remembered with less certainty, or remembered as behaviour rather than read.
- **Inferred** — derived from the other rows, or a choice oxabl makes where the source is silent.

Each row has a fixture in `crates/oxabl_scope/tests/fixtures/` (re-bless with
`OXABL_BLESS=1 cargo test -p oxabl_scope --test snapshots`). The LISTING harness
(`crates/oxabl/examples/scope_listing.rs`, section 7.5) measures every row that the listing shows.
Where the model cannot decide, the answer is **Unknown** with a reason, never a guess.

`oxabl_scope` resolves buffer and field names itself, against the file's definitions and the
schema, rather than through `oxabl_semantic`. The semantic resolver binds a name through the
scope tree of the file; the compiler's choice of a buffer for an unqualified field follows the
record phrases and record blocks around the reference (S11), which the scope tree does not
carry.

### 7.1 Buffers

| # | The model | Confidence | Fixture |
|---|---|---|---|
| B1 | Every database table and every temp-table has a default buffer with the table's name. A table name in a record phrase, a field qualifier or a buffer position (`DISPLAY t`, `AVAILABLE t`, `BUFFER t:HANDLE`, `RELEASE t`) names that buffer. `db.table` names the same buffer as `table`. | Documented. | `free.p` |
| B2 | A default buffer belongs to the procedure file, not to the internal procedure, function or trigger that uses it. A free reference in an internal procedure to a buffer the procedure does not define therefore scopes the buffer to the main procedure block, and the record stays in the buffer after the call returns. A `FOR` block in the internal procedure still scopes the buffer to that block, unless a scope at the main block absorbs it (S5). The same holds for a buffer the main block defines with `DEFINE BUFFER`. | Recalled, medium. The "define a buffer in every internal procedure" practice rests on it. | `units.p` |
| B3 | In a class, a default buffer, or a buffer the class body defines, that a method uses belongs to the class: the method's free references scope it to the class's main block. | Inferred, low: the class rule is the procedure rule carried over. | `class.cls` |
| B4 | `DEFINE BUFFER b FOR t` makes a new buffer owned by the unit that defines it. Its scopes lie in that unit (or, for a main-block definition, as in B2). `FOR TEMP-TABLE tt` and a `t` that names a temp-table of the file give a temp-table buffer; otherwise `t` is a database table. | Documented. | `buffer_kinds.p` |
| B5 | A buffer parameter (`DEFINE PARAMETER BUFFER b FOR t`) and the buffers of a database trigger (`TRIGGER PROCEDURE FOR WRITE OF t NEW BUFFER n OLD BUFFER o`, the table's own buffer included) and of an `ON WRITE OF t NEW BUFFER n` trigger hold the caller's record. Their scope is the caller's: the model gives them no scope (`external`). | Recalled, medium. | `buffer_kinds.p` |
| B6 | `DEFINE SHARED BUFFER b` uses the producer's buffer, and its scope is the producer's (`external`). `DEFINE NEW SHARED BUFFER b` and `NEW GLOBAL SHARED` are scoped to the main block of the procedure that defines them, whatever the references. | Recalled, medium for `SHARED`; low for `NEW SHARED`. | `buffer_kinds.p` |
| B7 | Temp-table buffers are scoped by the same rules as database buffers. A temp-table write never makes a block a transaction block (T3). A temp-table without `NO-UNDO` is undone with the transaction or sub-transaction that changed it; one with `NO-UNDO` is not. | Documented for scoping; Recalled, medium, that temp-table writes start no transaction. | `temp_tables.p` |
| B8 | Dynamic buffers (`CREATE BUFFER h FOR TABLE ...`) are run-time objects: they have no compile-time scope and are not modelled. A dynamic method on a handle (`h:BUFFER-CREATE()`, `h:FIND-FIRST(..., EXCLUSIVE-LOCK)`, `h:BUFFER-FIELD(...):BUFFER-VALUE = v`) may update a database record that the compiler cannot see (T7). | Documented that the compiler cannot see the table; the effect on transactions is Inferred. | `opaque.p` |

### 7.2 Scopes

| # | The model | Confidence | Fixture |
|---|---|---|---|
| S1 | **Strong scope.** `DO FOR b` and `REPEAT FOR b` scope `b` to the block. Every reference to `b` inside the block, a `FOR EACH b` included, belongs to that scope. | Documented. | `strong.p` |
| S2 | **Weak scope.** A `FOR` block scopes the buffer of each of its record phrases (`EACH`, `FIRST`, `LAST`, joined phrases included) to the block. A reference to `b` inside the block body belongs to that scope. | Documented. | `weak.p` |
| S3 | **Free reference.** Any other reference: `FIND`, `CREATE`, `DELETE`, `RELEASE`, `VALIDATE`, `BUFFER-COPY`, `BUFFER-COMPARE`, a field (qualified or not), a buffer name in an expression (`AVAILABLE b`, `BUFFER b:HANDLE`, `RUN p (BUFFER b)`), a statement the parser skips that names the buffer or one of its fields (`UPDATE`, `PUT`, `EXPORT`, `ENABLE`), and `OPEN QUERY` / `GET` (S10). Definitions are no reference: `FORM`, `DEFINE FRAME`, `DEFINE BROWSE`, `DEFINE QUERY`, `DEFINE DATA-SOURCE`, `LIKE`. The field list of `BUFFER-COPY` and `BUFFER-COMPARE` (`USING`, `EXCEPT`) names fields, not references. | Documented for the statement forms; Inferred for the skipped forms and the definitions. | `free.p` |
| S4 | A free reference scopes the buffer to the nearest enclosing block that has record scoping: the routine block (procedure, function, method, constructor, destructor, property accessor, trigger), a `REPEAT`, a `FOR`, or a `DO FOR` / `REPEAT FOR` of that buffer (for other buffers it is a plain `DO` or `REPEAT`). A plain `DO` has no record scoping, also with `TRANSACTION` or an `ON ERROR` phrase, so its free references go to the block around it. `CATCH` and `FINALLY` have none either. | Documented for `REPEAT`, `FOR` and the routine block; Recalled, medium, that `DO TRANSACTION` and `DO ON ERROR` do not scope records (the other reading gives them record scoping; the harness will tell); Inferred for `CATCH` and `FINALLY`. | `free.p`, `raise.p` |
| S5 | **Raising.** The scopes of one buffer never overlap. When the block of a free-reference scope encloses another scope of the same buffer (a weak scope, or the scope of free references in a deeper block), the outer scope absorbs the inner one: the buffer is scoped once, to the outer block. A free reference at procedure level thus raises every weak scope of that buffer in the procedure to the procedure. The model reports the scope as `raised` and names the free reference that holds it there. | Documented for a weak scope raised by a free reference; Inferred for the general merge. | `raise.p` |
| S6 | Two scopes whose blocks do not nest stay separate: two `FOR EACH b` in a row, or `FIND b` in two sibling `REPEAT` blocks, give two scopes. | Documented for `FOR`; Recalled for `REPEAT`. | `weak.p`, `raise.p` |
| S7 | **Conflicts.** A free-reference scope that would absorb a strong scope (a reference to `b` outside a `DO FOR b` whose scope encloses it), a strong scope nested in a weak scope of the same buffer, and a weak scope nested in another weak scope of the same buffer are compile errors, as far as remembered. The model answers Unknown (`conflict`) for that buffer. | Recalled, low. | `conflict.p` |
| S8 | The header of a block (`WHERE`, `OF`, `WHILE`, `BY`) runs before the body. A reference there to a buffer that is not one of the block's own record phrases belongs to the enclosing block, so `FOR EACH o OF c` is a free reference to `c` in the block around the `FOR`. | Inferred. | `weak.p` |
| S9 | `CAN-FIND` reads with a buffer of its own: it is no reference to `b` and changes no scope. References to other buffers inside it (its `WHERE`) are free references. | Recalled, high. | `locks.p` |
| S10 | `OPEN QUERY q FOR EACH b` and `GET ... q` are free references to `b` where they stand; `GET` reads every buffer `DEFINE QUERY q FOR ...` names. `DEFINE QUERY` itself is none. | Inferred, low. | `queries.p` |
| S11 | **Unqualified fields.** A field written without its buffer resolves, in order, to the record phrases of its own statement (`FIND`, `FOR`, `CAN-FIND`, the target of `BUFFER-COPY ... ASSIGN`); then to the buffers of the enclosing `FOR`, `DO FOR` and `REPEAT FOR` blocks, innermost first; then to the buffers the file names. At each level, one buffer with the field (exact name before abbreviation) wins; two are ambiguous, which the model answers Unknown (`ambiguous-field`) for those buffers. An abbreviation counts only at the first two levels, and a word the lexer reads as a keyword (`ROW`, `AVAILABLE`) only by its exact name: on a large real corpus, abbreviations against every buffer of the file were mostly keywords and frame options. A declared variable, parameter or property of the same name wins over a field. A field of a table no buffer of the file names is not found. A table the loaded schema does not have makes its buffer Unknown (`unknown-table`) where one of its fields could be meant; with no schema loaded, every unresolved name does (`no-schema`). | Recalled, low (the compiler's search order is not remembered exactly; an ambiguous name is a compile error). | `fields.p`, `no_schema.p` |
| S12 | A buffer with no reference has no scope. A `DEFINE BUFFER` alone is no reference. `DEFINE VARIABLE v LIKE t.f` is no reference. | Recalled, medium. | `buffer_kinds.p` |

### 7.3 Transactions

| # | The model | Confidence | Fixture |
|---|---|---|---|
| T1 | **Explicit.** A `DO`, `REPEAT` or `FOR` block with the `TRANSACTION` keyword is a transaction block. | Documented. | `txn_explicit.p` |
| T2 | **Implicit.** A routine block, a `FOR`, a `REPEAT`, and a `DO` with an `ON ERROR` or `ON ENDKEY` phrase is a transaction block when it *directly* updates the database or directly reads with `EXCLUSIVE-LOCK`. Directly: the statement is in the block, or in plain `DO` blocks inside it, and not inside a nested block that is itself transaction-capable. A plain `DO` hands its statements to the block around it. A `FOR ... EXCLUSIVE-LOCK` makes its own block a transaction block (the read is part of each iteration). | Documented for the procedure, `FOR EACH`, `REPEAT` and `DO ON ERROR`; Recalled for `ON ENDKEY`; Inferred for functions, methods, constructors, destructors, property accessors and `ON ... DO` triggers (taken as procedure blocks). | `txn_implicit.p` |
| T3 | **Updates.** `CREATE b`, `DELETE b`, an assignment to a field (`=`, `ASSIGN`, the first argument of `ENTRY`, `SUBSTRING` or `OVERLAY` on the left of `=`), `BUFFER-COPY ... TO b`, `UPDATE`, `SET`, `INSERT` and `IMPORT` naming a field or buffer, `COPY-LOB ... TO b.f`, and `FIND`, `FOR`, `OPEN QUERY`, `GET` or `FIND CURRENT` with `EXCLUSIVE-LOCK`, where `b` is a database buffer. Writes to temp-table buffers do not count. `PROMPT-FOR` only reads input into the frame. A statement form the model does not know (`RAW-TRANSFER` among them) that names a database buffer gives Unknown (T7). | Documented for the database statements; Recalled, medium, for temp-tables; Inferred for `PROMPT-FOR` and `COPY-LOB`. | `txn_implicit.p`, `temp_tables.p`, `opaque.p` |
| T3a | **The screen form of `ASSIGN`.** An `ASSIGN` element with no `=` moves the screen value into the record: `ASSIGN b.f`, `ASSIGN FRAME fr b.f`, `ASSIGN INPUT b.f`, `ASSIGN BROWSE br b.f`, and `ASSIGN b [EXCEPT b.f ...]` for every field of the record. Each such field or record is an update, like `b.f = v`. The fields after `EXCEPT` are not. Where the text after an `=` or `WHEN` cannot be read as an expression, the rest of the statement is read as a form the model does not know: a database buffer it names gives Unknown for the block's transaction and for `written` (L3). | Documented. | `screen_assign.p` |
| T4 | **Sub-transactions.** A transaction block inside an active transaction is a sub-transaction: `UNDO` of it undoes only its own changes, and the commit happens at the end of the outermost transaction block. The model records, for each transaction block, the closest enclosing transaction block of the same unit (`in`). | Documented. | `txn_nested.p` |
| T5 | **Calls.** A procedure, function or method called while a transaction is active runs inside it; its own transaction blocks become sub-transactions. The `TRANSACTION` function is true when a transaction is active. The compiler cannot see the caller, so it decides each unit as if no transaction were active, and the model does the same. | Documented for the inheritance; Inferred for the listing. | `txn_nested.p` |
| T6 | **The listing's `Tran` column** is taken to say "this block is a transaction block", nested or not. The other reading (only the outermost block of each unit) is reported by the harness too. | Inferred, low. | (harness) |
| T7 | **Unknown.** A transaction-capable block that is not a transaction block is Unknown when its direct statements include a dynamic buffer method that may update (B8, reason `dynamic-buffer`), a statement whose form the model does not know and that names a database buffer (`skipped-statement`), an unexpanded include (`unexpanded-include`), a write to a table the loaded schema does not have (`unknown-table`), or, with no schema loaded, an assignment to a name that resolves to nothing (`no-schema`). The compiler does not see the first: the listing then says `No`, and the harness counts the block as unknown, not as a disagreement. A recovered parse makes every capable block that is not a transaction block Unknown (`recovered-parse`). | Inferred. | `opaque.p` |
| T8 | `UNDO` restores the database records, the temp-tables without `NO-UNDO` and the variables without `NO-UNDO` changed in the undone transaction or sub-transaction. Section 5 (F5-F8) says which block an `UNDO` undoes. | Documented. | (section 5) |

### 7.4 Locks

| # | The model | Confidence | Fixture |
|---|---|---|---|
| L1 | `FIND`, a `FOR` record phrase, `OPEN QUERY` and `CAN-FIND` without a lock phrase read with `SHARE-LOCK` (the model's `default share`). A `GET` without one takes the lock its query was opened with: the last `OPEN QUERY` of that query before it in the file, else Unknown (`query-lock`). The `-NL` startup option, if remembered right, makes the default `NO-LOCK` for some of them; the model does not read startup options. | Documented for `FIND` and `FOR`; Recalled for `CAN-FIND`, `OPEN QUERY` and `GET`; Recalled, low, for `-NL`. | `locks.p`, `queries.p` |
| L2 | Temp-table records have no locks: the lock of a read on a temp-table buffer is `n/a`, whatever the phrase. | Documented. | `locks.p` |
| L3 | A record read with `SHARE-LOCK` and then changed in a transaction is upgraded to `EXCLUSIVE-LOCK` at the change. A change to a record read with `NO-LOCK` is a run-time error. The model marks a read `written` when a write to the same buffer, in the same scope of that buffer, may run after the read: the input LK001 needs to know whether `NO-LOCK` is a safe fix. A write in the read's routine may run after it when it comes later in the text, or when an iterating block (`REPEAT`, `FOR`, `DO WHILE`, `DO ... TO`) inside the scope holds both the read and the write: the next iteration writes the record the read gave. The scope block itself counts as iterating, on purpose: when a `REPEAT` or `FOR` scopes the buffer, the record may be released at the end of each iteration, so the earlier write could not touch it (Recalled, to be confirmed by the `LISTING`/oracle gate), but the model does not rely on that. The cost is a `written` that is Yes where No is true, so LK001 suggests `EXCLUSIVE-LOCK` where `NO-LOCK` is safe: the safe direction. A write in another routine of a scope raised across routines (B2) runs whenever that routine is called, so the answer is Unknown (`unit-order`). When no such write exists but the buffer is passed to a call that may run after the read (`RUN p (BUFFER b)`, `f(BUFFER b)`), or it is a shared buffer and such a `RUN` is in the unit, the answer is Unknown (`callee`); a statement form the model does not know that names the buffer gives Unknown (`skipped-statement`). | Recalled, medium; the `written` test is Inferred (it reads blocks, not paths). | `locks.p`, `written_order.p` |
| L4 | **Lifetime.** A lock taken in a transaction is held until the transaction ends. If the buffer's scope is still open then, an `EXCLUSIVE-LOCK` is downgraded to `SHARE-LOCK`, held until the scope ends, the buffer reads another record, or `RELEASE`. A `SHARE-LOCK` taken outside a transaction is released when the buffer's record changes or its scope ends. This is why LK002 compares a buffer's scope with its transaction block. | Documented for the end of the transaction; Recalled, medium, for the downgrade. | LK002 tests in `oxabl_lint` |
| L5 | A lock phrase that the text does not show gives Unknown (`unexpanded-include`). That is a read with no visible lock phrase where, after the record phrase, the preprocessor replaced a reference with nothing (an include it did not find, a `{&name}` with no value, a `{N}` with no argument; not inside a string), or a reference stays unexpanded (`--no-preprocess`). The hidden text may be `EXCLUSIVE-LOCK`, so the block's transaction is Unknown too unless an update decides it. In an include checked alone, such a reference anywhere in the read does the same, because the includer's definitions are missing. An undefined `{&name}` expands to nothing in the compiler as well; it still counts, because the author put the reference where a lock phrase goes. | Inferred. | `hidden_lock.p` |

### 7.5 What `COMPILE ... LISTING` shows

The harness parses the listing. The text format below is remembered, not read from a real
listing on this host, so the parser is lenient: it finds the table by its header, reads each row
from its words rather than fixed columns, and skips page headers. The listings in
`crates/oxabl_scope/tests/listings/` are **synthetic**: they were written from this description.

| # | The format | Confidence |
|---|---|---|
| X1 | `COMPILE f.p LISTING f.lst` writes the source with three left columns: the include level (`{}`), the line number in the file the line comes from, and the block depth (`Blk`). Lines from an include file carry the include's own line numbers. Each page starts with a header line (file name, date and time, `PROGRESS(R) Page n`), and pages are separated by a form feed. | Recalled, medium. |
| X2 | After the source comes the block table, with the header `File Name  Line Blk. Type  Tran  Blk. Label` and a line of dashes. Each block of the file is one row: the file the block statement is written in (an include's path for a block in an include), its line in that file, the block type, `Yes` or `No` in `Tran`, and the label. The main procedure block is a row with line `0`. | Recalled, medium. |
| X3 | Block types: `Procedure` (the main block and each internal procedure), `Function`, `Method`, `Constructor`, `Destructor`, `Property`, `Trigger`, `Do`, `For`, `Repeat`, `Catch`, `Finally`, `Editing`. The label column holds the block label, or the routine's kind and name (`Procedure ip-name`). | Recalled, low for the routine kinds and the label column. |
| X4 | Under a row, `    Buffers: db.table` lists the buffers scoped to that block, and `    Frames:  name` the frames. A second buffer is either on the same line after a comma or alone on the next line, under the first. Named and temp-table buffers are written without the database name (`bCust`, `ttOrder`), default database buffers with it (`sports2000.Customer`). | Recalled, low for the separator and the database prefix; the harness compares the last dotted segment, case-insensitively. |
| X5 | The header and its dashes repeat on each page of the table. | Recalled, medium. |

### 7.6 Corpus check

Verified, proportions only. Over the compilation units (`.p`, `.w`, `.cls`) of two large real
corpora, with their schemas loaded, the engine ran without a panic, in about the time of
expanding and parsing the same files.

- **Buffers.** 85 % (first corpus) and 68 % (second) of the buffers have a known scope; 4 % and
  32 % belong to the caller or producer (parameters, shared buffers, and the buffers of session
  triggers, which a common include defines in most programs of the second corpus). Unknown
  answers are 10.5 % and 0.2 % of the buffers. In the first corpus most come from the 7 % of
  files whose parse was recovered; then a table the schema does not have, an unexpanded
  include, and an unqualified field that two buffers have.
- **Scopes.** Of the known scopes, about 60 % (first) and 77 % (second) are raised, and 75 % and
  87 % of those only because a unit other than the owner references the buffer (B2). Weak scopes
  are 12 % and 9 %, strong scopes almost none. B2 is therefore the rule that decides most
  answers, and the first one to check against a listing.
- **Transactions.** Implicit transaction blocks outnumber explicit ones, by 1.6 times and 5.6
  times. In the second corpus most implicit ones are the routine blocks of session triggers
  (`ON WRITE OF t ... DO:`) brought in by a common include, counted once per program. Apart from
  recovered parses, Unknown transaction answers come mostly from dynamic buffer methods (B8): on
  reading, most of those methods work on temp-table handles, which the model cannot tell apart
  from database handles without following the handle.
- **Locks.** Explicit `NO-LOCK` dominates; reads with the default `SHARE-LOCK` are under 1 % of
  `FIND` and `FOR` reads, but 70 % and 85 % of `CAN-FIND` reads on database tables.

Reading 15 sampled answers per corpus against the code: 14 of the 30 follow from the documented
rules (S1, S2, S4, S5, T1, T2) and look right; 13 follow from B2 (an internal procedure, a
function or a UI trigger that uses the file's default buffer or temp-table, scoped to the main
block); 2 follow from B3 (a class method). One looked doubtful: a free reference in one internal
procedure raises the scope to the main block and so absorbs the `FOR EACH` scopes of the same
buffer in sibling internal procedures. That is what B2 with S5 gives, but it is the case to check
first. One sample showed a bug, now fixed: a variable named like a buffer hid the buffer in
`buffer.field`.

**Still to verify against a licensed install**, in this order, because each changes many
answers: B2 (default buffers in internal procedures and triggers, and whether a raise absorbs
scopes in sibling procedures), S4 (`DO TRANSACTION` and `DO ON ERROR` without record scoping), T6
(the meaning of `Tran`), X3 and X4 (the format), then B3, S7, S10 and S11.
`scripts/gen-scope-listings.sh` writes the listings, and the harness reports each disagreement
with both sides.

### 7.7 The lock rules (M3)

LK001, LK002 and LK003 (`strict.locks`, preview, warning) select findings from `oxabl_scope`;
`oxabl_lint` computes one analysis per file through `RunShared`, and only when one of the three
rules is enabled. They are routed like the `HD` rules (section 3, "Handle rules in includes").

Common to the three rules:

- **Unknown is silent.** A file with a file-level Unknown (recovered parse, unexpanded include,
  past the work budget) gets no finding. A buffer whose scope is Unknown or external gets no LK002
  finding and no `NO-LOCK` suggestion in LK001.
- **Database tables only.** A read counts only when its table is in the loaded schema
  (`RecordRead::table` is `Table::Database`). With no schema, every table is `Unknown` and the
  rules are silent: the name can be a temp-table that the file does not show.

LK001 rests on L1 and L2. Its help suggests `NO-LOCK` only when `written` is No (L3) and the
buffer's scope is known; it suggests `EXCLUSIVE-LOCK` when `written` is Yes, and no lock when
`written` is Unknown. No autofix yet. `GET` is not reported (it takes the lock of its query). A
`CAN-FIND` keeps no record, so its lock outlives nothing; its cost is the wait while another
session holds the record with `EXCLUSIVE-LOCK` (Recalled, high). `NO-LOCK` answers without the
wait, but may see a change another session has not committed (a dirty read; Recalled, high), so
its answer can differ from the committed one for an "already exists" check. Its message names
the wait, and its help always suggests `NO-LOCK` and says so. A read whose lock phrase the text may hide (L5) is not
reported. An include that did not arrive (`PREPROC007`) after a read, in its scope or in an
iterating block of it, may write the record: `written` is Unknown (`unexpanded-include`), so the
help suggests no lock. The rest of the file is judged as before. LK002 is not reported for a
transaction that holds such an include after its last read: the include may `RELEASE` the
record.

LK002 rests on L4 (Documented, and Recalled medium for the downgrade), S1, S2, S4 (Recalled
medium for `DO TRANSACTION`), the documented half of S5 (a weak scope raised by a free
reference), B2 (Recalled medium), T1 and the documented half of T2. Those meet the bar
"Documented, or Recalled with medium confidence or more", so the rule ships. Findings that need
an inferred rule are left out: the general merge of S5 (a free scope absorbed by an outer free
scope), B3 (classes), and T2 for functions, methods, property accessors and `ON ... DO` triggers.
Also left out: a transaction whose last read into the buffer is `NO-LOCK`, or that releases or
deletes the record after it; a transaction whose last read is a `FOR EACH` or `PRESELECT` phrase
(whether the buffer still holds a record after the last iteration is not decided); a scope in
which nothing runs after the transaction block. A record that `CREATE` makes is held the same way
but is not reported. Medium confidence overall: B2 and S4 decide most findings, and section 7.6
lists both as the first to check against a listing.

LK003 uses the flow graph of the unit (`oxabl_flow::UnitBuilder`, built only when a candidate
exists). The upgrade is `FIND CURRENT b EXCLUSIVE-LOCK` or `FIND b2 WHERE ROWID(b2) = ROWID(b)
EXCLUSIVE-LOCK` (or `RECID`) after a `NO-LOCK` read of `b` in the same loop (its own record
phrase, or a `FIND` or `GET` before it). "Every iteration": the end of the loop body is reachable
from its start on normal edges, but not around the upgrade. An upgrade with `NO-WAIT` is not
reported: the loop then skips records that another session holds, which a loop that reads with
`EXCLUSIVE-LOCK` cannot do.

**Corpus check (verified, counts only).** Over two large real corpora, with their schemas, the
three rules ran with no panic. LK001 gave 5,321 and 466 findings, 96 % and 82 % of them on
`CAN-FIND`. LK002 gave 1,013 and 270; LK003 119 and 32. Fifteen findings per rule per corpus were
read against the code:

- LK001: 30 of 30 correct (one on a dictionary table, `_File`, counted unsure).
- LK002: in a first sample, 25 correct and 5 unsure, all with a `FOR EACH` phrase as the last
  read; the rule now leaves those out. A second sample: 30 of 30 correct, if B2 and S4 hold. Most
  are an internal procedure of a persistent or server-side procedure that locks the file's default
  buffer, so the record stays `SHARE-LOCK`ed after the call returns.
- LK003: in a first sample, 9 of 30 were deliberate: `NO-WAIT` with a `LOCKED` test that skips
  records another session holds. The rule now leaves those out. A second sample: 30 of 30
  correct.

A finding in an include that does not parse alone is reported at each `{...}` site: one widely
included fragment gave 1,131 of the 1,164 use-site findings of LK001 in the first corpus.

After the review fixes (the screen form of `ASSIGN`, `written` by blocks rather than text order,
the lock phrase that unexpanded text may hide, the CAN-FIND message), the counts are LK001 5,323
and 466 (1,173 at use sites in the first corpus), the others unchanged. In the first corpus, 7
LK001 findings in includes checked alone moved to 9 use sites: each `CAN-FIND` there has a `{1}`
or `{&name}` after its table, which the include alone cannot read and its includers give a value.
The help changed for 29 and 5 LK001 findings, from a lock suggestion to none: most have a write in
another routine of a scope raised across routines (`unit-order`). No write that only the loop
rule or the screen form of `ASSIGN` sees changed a finding on these corpora.

### 7.8 The transaction rules (M3)

TX001 to TX005 (`strict.transactions`, preview, warning) select findings from the same
`oxabl_scope` analysis as the lock rules, made once per file through `RunShared`. They are routed
like the `HD` and `LK` rules. TX006, in the same group, is syntactic and does not use the analysis.

Common to the five rules:

- **Unknown is silent.** A file with a file-level Unknown gets no finding. A buffer whose scope
  is Unknown or external gets no TX005 finding. A block inside a block whose transaction status
  is Unknown gets no TX001 finding.
- **Which blocks are transactions.** A block counts when T1 makes it one (`TRANSACTION`), or the
  documented half of T2 (a procedure or main block, a `FOR`, a `REPEAT`, a `DO ON ERROR` or
  `DO ON ENDKEY`) with a first update of a table of the loaded schema. The implicit routine
  blocks of functions, methods, property accessors and `ON ... DO` triggers are inferred and do
  not count. With no schema, only `TRANSACTION` blocks count.
- **Include files.** In an include checked alone, the main block is not judged: the includer
  decides which block its statements are in.

| # | The model | Confidence |
|---|---|---|
| U1 | `WAIT-FOR`, `UPDATE`, `SET`, `PROMPT-FOR`, `INSERT` (the screen form), `CHOOSE` and `READKEY` wait for the user. `PAUSE` with no time waits for a key; `PAUSE n` waits at most `n` seconds; `PAUSE ... BEFORE-HIDE` sets an option. `MESSAGE ... VIEW-AS ALERT-BOX` shows a modal box, and `MESSAGE ... SET` or `UPDATE` waits for input. | Documented for the statements; Recalled, high, for the `MESSAGE` forms. |
| U2 | In a session with no user (batch, AppServer) the statements of U1 do not wait: an alert box is written to the log. | Recalled, medium. |
| U3 | `RUN ... ASYNCHRONOUS` and `RUN ... ON SERVER h` run the callee in another session, outside the caller's transaction. `RUN ... ON [SERVER] SESSION` names the local session: the callee runs in the caller's transaction. | Documented. |

- **TX001** rests on T2 and T3. One finding per block, at its first update, for a block that no
  other transaction block of its routine encloses (a sub-transaction is not reported). The main
  block is left to TX002, a `TRIGGER PROCEDURE` to the transaction of the write that fires it.
- **TX002** is the main block of a procedure file that is a transaction block, once per file. An
  internal procedure that is one is reported by TX001.
- **TX003** rests on U1 with the transaction blocks above. The statement must be in the
  transaction block or in a block inside it of the same routine: a trigger or an internal
  procedure that the transaction calls is not followed. `PAUSE n` is not reported: lock-retry
  loops sleep with it.
- **TX004** rests on T5 and U3. The shared summary provider (8.6) suppresses calls whose
  complete transitive summary has no database writes. Missing and partial summaries remain
  Unknown; known writes retain the warning. Direct file-only entry points have no provider.
  An internal procedure with a body, and one declared `EXTERNAL`, keep their local exemption.
  In an include checked alone, a bare name may belong to the includer and is not reported.
- **TX005** rests on S2, S4, the documented half of S5 (a weak scope raised by a free reference)
  and B2 (Recalled, medium). It is reported when the raising reference and the `FOR` block are in
  the same routine, or the reference is in the main block and the `FOR` block in a routine inside
  it. Left out: a reference in one internal procedure that raises a `FOR` block in another (B2
  with S5, the case section 7.6 marks to check first), the general merge of S5, a raise held by a
  reference in a block header (S8) or in a statement the parser skips (S3; `DISABLE TRIGGERS FOR
  LOAD OF t` among them), a raise to a block other than a routine block, and class files (B3).

**Overlap with LK002.** A raised scope that keeps a record locked after a transaction gives both
a TX005 finding, at the reference that raises the scope, and an LK002 finding, at the read in the
transaction. Both are kept: one shows the cause, the other the lock. The help of LK002 names TX005
when TX005 reports the raise, and the help of TX005 names LK002.

**Overlap with LK003.** The fix LK003 suggests, `FOR EACH ... EXCLUSIVE-LOCK`, makes the block a
transaction block implicitly, which TX001 reports. The fixed example of LK003 therefore adds
`TRANSACTION`.

**Corpus check (verified, counts only).** Over the same two corpora, with their schemas, the five
rules ran with no panic. TX001 gave 4,738 and 1,089 findings, TX002 250 and 73, TX003 342 and 184,
TX004 6,391 and 3,043, TX005 1,410 and 309. The LK counts did not change. Fifteen findings per
rule per corpus were read against the code, then the false-positive classes were fixed and a new
sample of 15 was read for each rule that changed:

- TX001: 29 of 30 correct. One was a `COPY-LOB ... TO FILE` that the engine read as a write to a
  field; `oxabl_scope` now reads `TO FILE` as no write. A second sample of 15: 15 correct.
- TX002: 30 of 30 correct.
- TX003: 22 of 30 correct; 8 were `PAUSE n` sleeps in lock-retry loops, now left out. A second
  sample: 30 of 30 correct by the rule. About 70 % of all TX003 findings are `MESSAGE ...
  VIEW-AS ALERT-BOX`, and many of those are in code that runs on an AppServer, where U2 says the
  box does not wait.
- TX004: 26 of 30 correct; 4 were a `RUN` of a bare name in an include checked alone, which the
  includer defines, now left out. A second sample of 15: 15 correct. About 30 % (first corpus)
  and 78 % (second) are `RUN ... IN h`; in the second, most of those call one error-dialog
  procedure, which is itself user interaction in a transaction.
- TX005: 26 of 30 correct. Three were `DISABLE TRIGGERS FOR LOAD OF t` and one an `OUTPUT TO
  t.d` whose file name the engine read as a field of `t`. `oxabl_scope` now reads an `OUTPUT`
  or `INPUT` target written without `VALUE` as a file name, and TX005 leaves out raises held by a
  skipped statement. A second sample: 30 of 30 correct, if B2 holds.

Medium confidence overall: B2 and S4 decide most TX005 findings, and T2 for internal procedures
(the largest share of TX001) is documented but not yet checked against a listing.

## 8. Database effects and the call graph

`oxabl_effects` lists, per unit, what the unit does to the database: every record read with its
lock (from `oxabl_scope`), every create, update and delete, the access shape of each, and for every
search the index the compiler is expected to use. `oxabl_index::calls` lists the calls of a file and
what each calls; the transitive summaries combine both over the call graph. The oracle is the
compiler's `COMPILE ... XREF` output (plan, M4 exit gate: "DB002 agrees with XREF `WHOLE-INDEX`
entries"). No OpenEdge install was available while this was written, so the marks of section 7
apply: **Documented**, **Recalled**, **Inferred**.

Each row has a fixture in `crates/oxabl_effects/tests/fixtures/` (re-bless with
`OXABL_BLESS=1 cargo test -p oxabl_effects --test snapshots`). The XREF harness
(`crates/oxabl/examples/effects_xref.rs`, written by `scripts/gen-xref.sh` on a licensed install)
measures every row of 8.2 that XREF shows. Where the model cannot decide, the answer is **Unknown**
with a reason, never a guess.

### 8.1 What XREF shows

The harness parses the text form. The format below is remembered, not read from a real XREF file on
this host, so the parser is lenient: it splits on white space, keeps double-quoted fields whole, and
skips a line it cannot read. The files in `crates/oxabl_effects/tests/xref/` are **synthetic**.

| # | The format | Confidence |
|---|---|---|
| R1 | `COMPILE f.p XREF f.xref` writes one reference per line: `<compiled file> <source file> <line> <TYPE> <details>`. The source file is the include a line comes from, and the line is in that file. A file name with a space is in double quotes. | Recalled, medium. |
| R2 | `SEARCH db.table index` for each index a `FIND`, `FOR`, `CAN-FIND`, `PRESELECT` or `OPEN QUERY` phrase uses, with `WHOLE-INDEX` after the index name when no criterion brackets it. A phrase that uses several indexes has one row per index. A temp-table is written without a database name. | Documented for `WHOLE-INDEX`; Recalled, medium, for the rest. |
| R3 | `TABLE-SCAN` takes the place of the index name (or follows it) for a `TABLE-SCAN` phrase. | Recalled, low. |
| R4 | The line of a `SEARCH` row is the line the statement starts on, not the line of the phrase. | Recalled, low. The harness can place a search on either (`OXABL_EFFECTS_XREF_LINE`). |
| R5 | `ACCESS db.table field` for each field read, `UPDATE db.table field` for each field written, `CREATE db.table` and `DELETE db.table` for the statements, `REFERENCE db.table` for a table named elsewhere. | Recalled, medium. The harness does not compare them yet. |
| R6 | `RUN name` for each `RUN` with a literal name (as written); `PROCEDURE name,,`, `FUNCTION name,type,params` and `METHOD ...` rows define the units; `INCLUDE file` for each include. | Recalled, medium for `RUN` and `INCLUDE`, low for the unit rows. |
| R7 | `SORT-ACCESS db.table field` for a `BY` that no index gives in order (the client sorts). | Recalled, medium. |
| R8 | `COMPILE ... XREF-XML` writes the same facts as XML. The harness reads the text form. | Documented. |

### 8.2 Which index a search uses

The compiler picks the index of a `FIND`, `CAN-FIND` or `FOR`/`PRESELECT`/`OPEN QUERY` phrase from
its `WHERE`, `OF`, `USE-INDEX`, `TABLE-SCAN` and `BY`. The model reads them from the tokens of the
phrase (the parser keeps only the first phrase's `WHERE`).

| # | The model | Confidence | Fixture |
|---|---|---|---|
| I1 | A criterion is **active** when it is a conjunct of the `WHERE` at the top level: joined to the rest by `AND`, not under `OR` or `NOT`, not inside `IF ... THEN ... ELSE`. Parentheses around a conjunct are looked through. | Documented. | `index.p` |
| I2 | `USE-INDEX i` makes the search use `i`. The bracket is still decided by I3 and I4 on `i`'s components, so `USE-INDEX` with no matching criterion is `WHOLE-INDEX`. A name the table does not have is Unknown (`unknown-index`). | Documented. | `index.p` |
| I3 | An **equality match** is an active `field = expr` (or `EQ`), on either side, where the field is of the phrase's table and `expr` does not name the phrase's own record. The equality matches of an index are those on its leading components, without a gap. | Documented for the leading components; Inferred for "does not name its own record" (the record is not there before the search). | `index.p` |
| I4 | A **range match** is an active `<`, `<=`, `>`, `>=` (either side) or `field BEGINS expr`, on the first component, or on the component after the equality matches. | Documented. | `index.p` |
| I5 | Not matches: `<>`, `MATCHES`, a field inside a function (`SUBSTRING(f, 1, 2) = x`), a field of an array element, a field compared with a field of its own record, a bare logical field (`WHERE t.active`). | Documented for `<>`, `MATCHES` and functions; Inferred for the rest. | `index.p` |
| I6 | The choice, in order: `TABLE-SCAN`; `USE-INDEX`; `FIND t constant` (I11); a `CONTAINS` (I14); a unique index whose every component has an equality match; the most equality matches; then a range match on the next component; then the most `BY` fields in index order (I12); then the primary index; then the first by name. With no match at all, the primary index, `WHOLE-INDEX`. Inactive indexes are never chosen; word indexes only by `CONTAINS`. | Documented for unique-equality, equality, range and "primary when nothing matches"; Recalled, low, for the order of the tie breaks. | `index.p` |
| I7 | **WHOLE-INDEX**: the chosen index has neither an equality match on its first component nor a range match on it. | Documented. | `index.p` |
| I8 | `TABLE-SCAN` (OpenEdge 11.? and later, Type II areas) reads the table without an index. | Documented. | `index.p` |
| I9 | **Several indexes.** A `FOR`, `PRESELECT` or `OPEN QUERY` phrase may use several indexes: for an `OR` whose every branch brackets some index, and for an `AND` of equality matches that cover all components of two or more indexes with no unique one among them. The model answers Unknown (`multi-index`) there. A `FIND` and `CAN-FIND` always use one index, so an `OR` there makes its criteria inactive. | Recalled, medium for the `OR` case, low for the `AND` case. | `index.p` |
| I10 | `t OF p` adds equality matches on the components of a unique index of `p` that `t` also has (the fields the two tables share), else of a unique index of `t` that `p` has. With neither, Unknown (`of-join`). | Recalled, medium for "a common field in a unique index"; Inferred for which index's fields. | `index.p` |
| I11 | `FIND t constant` finds by the primary index, which must have one component: an equality match. | Documented. | `index.p` |
| I12 | `BY` fields of the phrase's table count as sort matches in the order of an index's components from the first; they decide only between indexes the criteria leave tied, and choose the index when nothing matches. They apply to the first phrase of the statement. A `BY` on an expression matches nothing. | Recalled, low. | `index.p` |
| I13 | Temp-tables follow the same rules over the indexes their `DEFINE TEMP-TABLE` gives. With no `INDEX` phrase the table has one index, `default`, with no components: every search is `WHOLE-INDEX`. When no index is marked `PRIMARY`, the first is primary. `LIKE t` with no `INDEX` and no `USE-INDEX` copies `t`'s indexes; with `USE-INDEX` only those; with `INDEX` only its own. | Recalled, medium for `default` and the first-is-primary rule; low for `LIKE`. | `temp_tables.p` |
| I14 | `field CONTAINS expr` uses the word index on that field (a word index is never chosen otherwise). A `CONTAINS` on a field with no word index is a compile error; the model answers Unknown (`no-word-index`). | Documented. | `index.p` |
| I15 | `WHERE ROWID(b) = r` (or `RECID`) on the phrase's own buffer fetches one record by its address: no index (`rowid`). Whether XREF writes a `SEARCH` row for it is not known; the harness counts these apart. | Recalled for the access; unknown for XREF. | `index.p` |
| I16 | An unqualified name in a criterion is a field of the phrase's table (exact name, else a unique abbreviation) unless the file declares a variable or parameter of that name, which wins. A qualified name is a field of the phrase when its qualifier is the phrase's buffer, or its table for a default buffer. | Inferred (follows S11). | `index.p` |
| I17 | A phrase whose text the preprocessor did not show (an elided reference, an unexpanded `{...}`, a lock phrase L5 makes Unknown) is Unknown (`unexpanded`). With no schema loaded, every database search is Unknown (`no-schema`). | Inferred. | `no_schema.p` |

### 8.3 Access shapes

| # | The model | Confidence | Fixture |
|---|---|---|---|
| A1 | A **record loop** is a `FOR` block with an `EACH` phrase, or a `REPEAT` or `DO WHILE` block whose body drives a query: `GET NEXT`/`PREV`, `FIND NEXT`/`PREV`, or `h:GET-NEXT()`/`GET-PREV()` on a dynamic query (in its header too: `DO WHILE q:GET-NEXT():`). The innermost iterating block around the driving statement is the loop. | Documented for `FOR EACH`; Inferred for query loops. | `shapes.p` |
| A2 | Not record loops: `FOR FIRST` and `FOR LAST` with no `EACH` phrase (one record), `DO i = 1 TO n`, a `REPEAT` that drives no query. `FOR t` with no qualifier is not counted. | Documented for `FOR FIRST`/`LAST`; Inferred for the rest. | `shapes.p` |
| A3 | A statement's shape is **once** with no record loop of its unit around it, **per row** with one, **nested(d)** with `d`. A `FOR` phrase does not count its own block; a joined phrase counts the `EACH` phrases before it in the same statement. The statement that drives a query loop does not count that loop. | Inferred. | `shapes.p` |
| A4 | Shapes stop at the unit: a statement in an internal procedure has the shape it has there. Through a call, the callee's shapes add the depth of the call site (8.6). | Inferred. | `calls.p` |
| A5 | A `CAN-FIND` in the header of a `FOR` block is placed in the block around it (S8): its shape does not count the loop, though the compiler evaluates it per candidate row. | Inferred; known to under-count. | `shapes.p` |

### 8.4 Operations

| # | The model | Confidence |
|---|---|---|
| O1 | Reads are the record reads of `oxabl_scope` (section 7.4): `FIND`, `FOR`, `PRESELECT` and `OPEN QUERY` phrases, `CAN-FIND`, `GET`, with their lock. `GET` and `FIND CURRENT` do not search, so they have no index. | Documented. |
| O2 | Writes are the database updates of `oxabl_scope` (T3): `CREATE` is create, `DELETE` is delete, an assignment, `BUFFER-COPY` to the buffer or an input statement is update. An `EXCLUSIVE-LOCK` read is a read with that lock, not a write. Temp-table writes are not database effects. | Documented (T3). |
| O3 | A unit's effects are **Unknown** in part when it holds a dynamic query or buffer (`CREATE QUERY`, `CREATE BUFFER`, and the methods `QUERY-PREPARE`, `QUERY-OPEN`, `GET-*`, `FIND-*`, `BUFFER-CREATE`, `BUFFER-DELETE`, `FILL`, `SAVE-ROW-CHANGES`), a statement the parser skips that names a database buffer, a write to a table the schema does not have, an unexpanded include, or when the parse recovered. Most dynamic access works on temp-tables, which the model cannot tell apart without following the handle. | Inferred. |

### 8.5 The call graph

`FileCalls::extract` reads the calls of one file; `resolve_program` resolves a program name over
the search path through the run index (`WorkspaceIndex::program`), which memoizes it.

| # | The model | Confidence | Fixture |
|---|---|---|---|
| C1 | `RUN name` runs the internal procedure of that name when the file defines one with a body, else the program `name` over PROPATH. `RUN name IN THIS-PROCEDURE` is the internal procedure. | Documented. | `calls.p` |
| C2 | A program name resolves to one file on the search path (the run index's rule: the spellings tried in order, exactly one match). Several matches are Unknown (`ambiguous`), none `not-found`, an unparseable or self match `unusable`. | Proposed (the index's existing policy). | (index tests) |
| C3 | `RUN p IN h` runs the internal procedure `p` of the program `h` holds. When every write of `h` in the file is `RUN prog PERSISTENT SET h` of one program, the target is `p` in `prog`; any other write of `h` (an assignment, an `OUTPUT` argument, a parameter or shared definition) makes it Unknown (`handle`). | Documented for the run-time meaning; Inferred for the file-level check (it does not follow the order of the writes). | `calls.p` |
| C4 | `RUN SUPER`, `SUPER(...)`, a routine declared `IN SUPER` and the super-procedure stack are run-time: Unknown (`super`). A routine declared `IN h` is Unknown (`handle`). | Documented. | `calls.p` |
| C5 | A call of a user-defined function the file defines (or declares `FORWARD`) is a call of that function. `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW` and `RUN VALUE(...)` are Unknown (`dynamic`). `PUBLISH` is Unknown (`event`). | Documented. | `calls.p` |
| C6 | A method call `x:m(...)`: on `THIS-OBJECT` (or with no receiver, in a class) a method of the file's class is a call of it; on a handle (a system handle, a name declared `HANDLE`, `WIDGET-HANDLE` or `COM-HANDLE`, a `BUFFER`, `QUERY`, `FRAME` ... reference, or a chain from one) it is a built-in method and no call; on anything else it is Unknown (`method`). Inherited methods, static calls and objects of other classes are not followed in this slice. | Documented for the meaning; Inferred for the receiver test. | `calls.p` |
| C7 | `RUN ... PERSISTENT` runs the program's main block now. `RUN ... ASYNCHRONOUS` and `RUN ... ON SERVER h` run in another session (section 7.8, U3): the edge is kept, marked remote. A procedure declared `EXTERNAL` (a shared library) has no database effects. `ON event ... PERSISTENT RUN p` runs on an event, not at the statement: it is no call site. | Documented. | `calls.p` |
| C8 | Bounds: at most 20,000 call sites per file and 2,000,000 edges per graph; past them the file or graph is marked truncated and the summaries partial (`bound`). | Proposed. | — |

### 8.6 Transitive summaries

`summary::summarize` solves the components of the call graph callees first. A component with a
cycle (recursion, or procedures that call each other) is iterated to a fixed point; depths are
capped at 4, so it ends. A summary keeps one effect per table, operation, lock and depth, and
names the first callee on the way (`via`). It is **partial**, with every reason, when an effect may
be missing: a call whose target is Unknown (C2-C6, by reason), a unit whose own effects are Unknown
in part (O3), or a bound. Recursion alone does not make a summary partial (the fixed point covers it);
it is flagged `recursive`. Temp-table effects are left out.


**Built in slice 2.** `LintPipeline` builds a lazy `WorkspaceEffects` over the run's batch index.
It uses the known compilation units and follows reachable programs outside that set.
Each file supplies direct facts once. `summary::summarize` solves the graph once for the run.
The first root uses the supplied expansion, including unsaved text. A changed root expansion
invalidates that run cache. Unreadable files, missing targets, lost include text, recovered parses,
and call-site bounds cannot produce a complete answer. Remote calls do not add local effects.

The lint registry accepts `RunShared` with a `CalleeSummaries` provider. TX004 asks by expanded
call-site offset. It suppresses a finding only for a complete summary without database writes.
A missing or partial answer is Unknown and cannot suppress a finding. Known writes retain the
warning. Direct file-only rule entry points keep their conservative result.

The daemon caches the reachable graph in a tracked query per buffer revision. It records disk
revisions for callee bodies, expanded includes, and every existence check, including failed search
candidates. A body change, include change, or newly created target invalidates the dependent graph.
The schema revision and preprocessor environment also invalidate it. Cycles are solved in the
shared pipeline, outside recursive tracked queries. The daemon protocol is unchanged.

With TX004 off, a normal lint run builds no graph or summaries. The other strict analyses keep
their existing lazy rule gates. `explain` explicitly requests summaries, regardless of lint settings.

### 8.7 Corpus check

Verified, counts only. Over the compilation units (`.p`, `.w`, `.cls`) of the two large real
corpora of section 7.6, expanded as `oxabl check` expands them and with their schemas, the call
graph, the effects and the summaries ran with no panic (`crates/oxabl/examples/effects_corpus.rs`).
Counts are per expanded unit, so an internal procedure that a common include brings into many
programs counts once per program.

- **Call graph.** Of the `RUN` sites, 95.8 % (first corpus, 298,446) and 70.9 % (second, 36,579)
  resolve to a routine. The Unknown ones are, in the first corpus, `RUN p IN h` with a handle the
  file does not set by one `PERSISTENT SET` (6,406), a program not on the search path (5,287) and
  `RUN VALUE` (1,090); in the second, the same handles (8,198), a name two files on the search path
  match (4,249: the index declines to pick one) and programs not on the path (1,984). Calls of the
  file's own functions resolve (243,897 and 29,389); method calls on objects mostly do not (8,924 and
  1,010 resolve, 301,725 and 30,668 are Unknown `method`), and `SUPER(...)` gives 6,830 in the first.
- **Index choice.** Of the searches of database tables (491,833 and 31,482), the model names no
  index for 0.6 % and 2.6 %: almost all `multi-index` (I9), then `unexpanded` (I17) and `of-join`
  (I10). `WHOLE-INDEX` is 1.7 % and 1.5 % of them, `ROWID` 5.0 % and 9.0 %.
- **Unit effects.** 6.5 % and 11.5 % of the units have effects the model cannot see in full,
  mostly dynamic queries and buffers (O3), then recovered parses.
- **Summaries.** 71.8 % and 72.1 % are complete. The partial ones mostly call a method of an object
  (22 % and 15 % of all summaries), hold dynamic access themselves or through a callee (10 % and
  8 %), or call a super procedure (5 %, first corpus). 1,392 summaries of the first corpus reached
  the bound of 512 effects.
- **Cost.** The calls and effects took 12.6 s and 1.0 s, against 28.6 s and 2.4 s for the scope
  analysis and 38.1 s and 3.6 s to expand and parse; solving every summary took 0.6 s and 0.05 s.
  With `strict.access` off, `oxabl check` computes none of it: its output on both trees is byte for
  byte the output before this change, and its wall time is the same within the noise of a shared
  host (the order of two runs decided which was faster).

**DB001.** 20,290 and 2,457 findings, 386 and 87 of them at the `{...}` site of an include
(`crates/oxabl/examples/db_corpus.rs`). Fifteen findings per corpus were read against the code: in
the first sample of the second corpus 14 were right and 1 was a find by `ROWID` that locks the
loop's record again, a class DB001 now leaves to LK003; a new sample of 15 there, and the 15 of the
first corpus, were all right. Every one is a `FIND` or `CAN-FIND` in the body of a `FOR EACH`
(over a database table or a temp-table) or of a `REPEAT` / `DO WHILE` that `GET NEXT` or a dynamic
`GET-NEXT()` drives. Many look up a code or master record by a key of the row; some do not depend
on the row at all (a lookup that a read before the loop would answer once).

**TX004 and the summaries.** The brief asked whether a callee summary with no database write could
silence TX004. Of the TX004 findings in the programs' own text (4,927 and 2,909), the callee of 631
(12.8 %) and 322 (11.1 %) has a complete summary with no create, update or delete; 2,163 and 283 have
a callee that writes; 1,204 and 91 a partial one; 929 and 2,213 an Unknown callee. Most of the
second corpus's silenced findings are label-printing procedures of a persistent library.
Slice 2 now applies this test through the shared provider. With every strict group enabled,
the second corpus gives 3,311 TX004 findings before and 2,976 after (335 removed, 10.1 %).
On the tracked files of the first corpus it gives 6,487 before and 6,149 after (338 removed,
5.2 %). All other strict findings are unchanged. These totals include fragments and include use sites.
Ten removed findings were read against the caller and callee source; all ten have no database
writes, including transitive calls. `strict_corpus` measures this through the actual lint pipeline.

### 8.8 The access rules (M4)

DB001 (`strict.access`, preview, warning) selects findings from `oxabl_effects`; `oxabl_lint`
computes one analysis per file through `RunShared`, over the scope analysis the `LK` and `TX` rules
use, and only when the rule is enabled. It is routed like the `HD`, `LK` and `TX` rules.

- **DB001** rests on A1-A3 and O1. A `FIND` or `CAN-FIND` of a table of the loaded schema whose
  innermost record loop is in the same unit. Left out: `FIND NEXT`/`PREV` that drives its loop,
  `FIND CURRENT`, a find by `ROWID`/`RECID` (a re-read to lock, LK003's domain), a `CAN-FIND` in a
  loop's header (A5), a read in a routine the loop calls (A4), temp-tables, no schema, and files with
  a file-level Unknown (section 7.7).

What the later slices need:

- **DB002** (whole-index scan): the `WHOLE-INDEX` answer of 8.2 and the XREF gate. Before it ships,
  run `effects_xref` on a licensed install and settle R2-R4, I6 (tie breaks), I9, I10, I12, I13 and
  I15; then decide whether `FIND FIRST`/`LAST` with no criteria (one record) and small code tables
  are reported.
- **DB003** (order from index): the `BY` fields (I12), `FIRST-OF`/`LAST-OF`, and whether the chosen
  index gives the order; `SORT-ACCESS` rows (R7) are its oracle.
- **DB004** (missing fields list): the fields used in the scope of each read (the `ACCESS` rows, R5,
  are the oracle) and which code runs client-server.
- **Built: `oxabl explain <file>[:line]`.** Prints each unit's direct database effects and
  transitive summary. Direct effects include the table, operation, lock, access shape, likely
  index and selection reason. Partial summaries list all reasons. The optional one-based line
  selects the innermost unit in root coordinates; output lines refer to expanded text.
  `--json` uses an independent document with `version: 1`, `file`, `coordinates`, and `units`.
  Each unit has `kind`, `name`, `line`, `effects`, and `summary`. A summary has `complete`,
  `partial`, `recursive`, and `effects`. An effect has `table`, `operation`, `lock`, `shape`,
  `line`, `index`, `reason`, and `via`. Inherited effects have no search site or index;
  `via` names the first callee. Unknown indexes have a null index and an explicit reason.
  Direct temp-table reads use the `tt:` prefix; summaries exclude temp-table effects.
  This new document does not change the existing analyze or check envelopes.
- **Still needed: summaries in `analyze`.** Add a section from the shared provider, with an
  additive envelope change and a section version bump.
- **Built: LSP code lens.** `textDocument/codeLens` renders the same direct effects and
  transitive summaries as `explain`. Each main block, procedure, function, method or trigger
  with database effects or calls gets one lens. Titles list reads, writes and call counts;
  unresolved direct calls have an unknown count. Incomplete summaries say `partial`.
  The renderer uses root header offsets from `resolve_root_span`; an include-only unit has
  no lens. The existing `root_units` selection spans keep their include-site fallback for
  `explain`, but code lenses never use that fallback.
  The client must offer code-lens support. Static clients get the capability at initialization
  only when the declared workspace enables DB001 (`strict.access`) or TX004
  (`strict.transactions`). Dynamic clients register only open files whose resolved lint
  settings enable either rule, including file overrides. Requests also check file settings.
  A worker reads the daemon's revision-tracked reachable graph cache. A request returns only
  ready lenses, or an empty list while the worker runs. The LSP thread builds no graph.
  Clients with `workspace.codeLens.refreshSupport` receive a refresh request when results
  become ready. Other clients see them on their next request. Edits, closes and watched-file
  changes discard rendered lenses and obsolete worker results. No `oxabl/*` method or JSON
  envelope changed; the daemon protocol version is unchanged.

## 9. Unknown values and records that may be missing

`oxabl_nulls` follows three things through the flow graph of each unit (`oxabl_flow`): the unknown
value `?` in local variables (the second slice), records a `FIND ... NO-ERROR` may have
failed to find (the first slice) and database fields the schema starts at `?` (the third slice,
section 9.11). Buffers resolve through `oxabl_scope` (section 7): its names, its default buffers and its
reading of unqualified fields. No OpenEdge install was available while this was written, so the
marks of section 7 apply: **Documented**, **Recalled**, **Inferred**; a design choice of oxabl is
**Proposed**.

Each modelled row has a fixture in `crates/oxabl_nulls/tests/fixtures/` (re-bless
with `OXABL_BLESS=1 cargo test -p oxabl_nulls --test snapshots`). Where the model cannot decide,
the answer is **Unknown**, which never makes a finding.

### 9.1 The unknown value

The second slice models local values for NL001 and NL002 (section 9.9); the third models
database fields for NL004 (section 9.11). The confidence marks still apply; these checks have no
licensed runtime oracle.

| # | ABL | Confidence |
|---|---|---|
| V1 | `?` is the unknown value. A variable, field or parameter of any data type can hold it. | Documented. |
| V2 | **Sources.** The literal `?`; `INITIAL ?`; the default of a `DATE`, `DATETIME`, `DATETIME-TZ`, `HANDLE`, `ROWID`, `RECID` and object-reference variable (the other types start at `0`, `""` or `no`); a database field whose stored value is `?` (any field that is not `MANDATORY`, and a field whose schema `INITIAL` is `?`); a user-defined function that ends without `RETURN` a value; `ROWID(b)` and `RECID(b)` with no record in `b`; an `OUTPUT` parameter the callee does not set. | Documented for the literal, `INITIAL ?`, the `DATE` and `HANDLE` defaults and non-mandatory fields; Recalled, medium, for the other defaults, function returns and `ROWID`/`RECID`. |
| V3 | **Arithmetic propagates.** `+`, `-`, `*`, `/`, `MODULO` and unary `-` with a `?` operand give `?`. Division by zero gives `?`, not an error. | Documented for `?` operands; Recalled, medium, for division by zero. |
| V4 | **String `+` propagates.** `"a" + ?` is `?`, not `"a"` or `"a?"`. | Documented. |
| V5 | **`=` and `<>` absorb.** `? = ?` is `yes`; `x = ?` is `yes` exactly when `x` is `?`; `<>` is the negation. They never give `?`. | Documented. |
| V6 | **Ordering propagates.** `<`, `<=`, `>`, `>=`, `BEGINS`, `MATCHES` and `CONTAINS` with a `?` operand give `?`. | Recalled, low. |
| V7 | **Logic.** `NOT ?` is `?`. `AND` and `OR` are three-valued: `? AND no` is `no`, `? AND yes` is `?`, `? OR yes` is `yes`, `? OR no` is `?`. | Recalled, low. |
| V8 | **Conditions.** An `IF` statement whose condition is `?` runs its `ELSE` branch (or nothing); `DO WHILE ?` and `REPEAT WHILE ?` end the loop; a `WHERE` that is `?` for a record does not select it. `?` behaves as `no` there, so a comparison that silently gives `?` (V6) skips code. | Recalled, medium. |
| V9 | **`IF ... THEN ... ELSE` as a function** takes the `ELSE` value when its condition is `?` (V8). The result is `?` only when the chosen value is. | Recalled, medium. |
| V10 | **Built-ins.** Most built-in functions give `?` when an argument is `?`: `ENTRY`, `NUM-ENTRIES`, `LOOKUP`, `SUBSTRING`, `LENGTH`, `TRIM`, `CAPS`, `INDEX`, `REPLACE`, `MAXIMUM`, `MINIMUM`, `ABSOLUTE`, `ROUND`, `TRUNCATE`, `INTEGER`, `DECIMAL`, `DATE` and the date parts. | Recalled, medium for the general rule; low for each function. |
| V11 | **Built-ins that absorb.** `STRING(?)` is the string `"?"`; `VALID-HANDLE(?)` and `VALID-OBJECT(?)` are `no`; `AVAILABLE`, `CAN-FIND` and `LOCKED` never give `?`; `SUBSTITUTE` writes `?` for an unknown argument. | Recalled, high for `STRING` and `VALID-HANDLE`; low for `SUBSTITUTE`. |
| V12 | **Errors are not `?`.** A conversion that cannot read its argument (`INTEGER("abc")`, `DATE("x")`) and `ENTRY` past the end of a list raise ERROR; they do not give `?`. | Recalled, medium. |
| V13 | **Mandatory fields.** Assigning `?` to a `MANDATORY` field raises ERROR when the record is validated (at the latest when it is written), not at the assignment. | Recalled, medium. |

### 9.2 A failed `FIND`

| # | ABL | Confidence | Fixture |
|---|---|---|---|
| N1 | `FIND` (unique, `FIRST`, `LAST`, `NEXT`, `PREV`) that finds no record raises ERROR (`** <table> record not on file`), and so does a unique `FIND` that matches two records. With `NO-ERROR` it raises nothing, and the buffer has no record: `AVAILABLE b` is `no`. | Documented. | `finds.p` |
| N2 | The record the buffer held before a failed `FIND` is released: after the statement the buffer is empty, whatever it held. | Recalled, medium. | `finds.p` |
| N3 | **A field of an empty buffer is an error, not `?`.** Reading or assigning `b.f` when `b` has no record raises ERROR (`** No <table> record is available`). | Documented. | `finds.p` |
| N4 | `NO-WAIT NO-ERROR` on a record another session locks leaves the buffer empty, with `LOCKED b` true. | Recalled, medium. | — |
| N5 | `FIND NEXT` and `FIND PREV` with `NO-ERROR` past the last record leave the buffer empty (the usual end of a `REPEAT` that walks an index). | Documented. | `loops.p` |
| N6 | `FIND CURRENT b NO-ERROR` fails, and leaves the buffer empty, when `b` has no record or another session deleted it. The model takes it as no source and no refill: it re-reads the record the buffer holds, and the buffer stays as it was. On both corpora these were re-reads to change the lock right after the record was used, and two of the first fifteen findings of the second corpus were such re-reads. | Recalled, low; the model's choice is Proposed. | `finds.p` |
| N7 | `NO-ERROR` on a statement also suppresses the error of N3: `ASSIGN x = b.f NO-ERROR` with no record raises nothing, and `x` is unchanged. | Recalled, medium. | `finds.p` |
| N8 | **`ERROR-STATUS`.** Each statement with `NO-ERROR` resets `ERROR-STATUS`; a statement without it leaves it alone. After a `FIND ... NO-ERROR` that found no record, `ERROR-STATUS:ERROR` is `yes`; after one that found a record, `no`. | Recalled, medium. | `guards.p` |
| N9 | `CAN-FIND` reads with a buffer of its own (S9): it neither fills nor empties `b`. | Recalled, high. | `guards.p` |
| N10 | `AND` and `OR` stop early: `AVAILABLE b AND b.f = x` does not read `b.f` when `b` is empty. | Recalled, medium. | `guards.p` |
| N11 | `RELEASE b` and `DELETE b` also leave the buffer empty. | Documented. | — |
| N12 | **Frame fields.** A field reference that names the widget of a frame, not the record, needs no record: the target of `@` in `DISPLAY`, a widget attribute or method (`b.f:SCREEN-VALUE`, `b.f:SENSITIVE`, `b.f:LABEL`), `b.f IN FRAME f`, the `INPUT [FRAME f] b.f` function, and the fields of `ENABLE`, `DISABLE`, `PROMPT-FOR`, `HIDE`, `VIEW`, `CLEAR`, `WAIT-FOR` and `APPLY ... TO`. A database field cannot hold a handle, so `b.f:attr` on one is always the widget; a temp-table field may (`tt.h:NAME` reads the record), so the model takes `tt.f:attr` as the widget only with `IN FRAME` or `IN BROWSE`. `INPUT` after `(` or `,` is the mode of a parameter, which reads the record. | Documented for the statements; Recalled, high, for the attributes. | `cases.rs` |

### 9.3 Guards

| # | The model | Confidence | Fixture |
|---|---|---|---|
| G1 | `AVAILABLE b`, `AVAIL b` and `AVAILABLE(b)` in the condition of an `IF` statement: on the `THEN` edge `b` has a record; on the `ELSE` edge (taken with or without an `ELSE`) it is empty. `NOT` swaps the edges. The operand is always a buffer, also where a variable of the same name hides the table from the scope analysis (S11): the model then looks the name up among the buffers the unit can see. | Documented. | `guards.p` |
| G2 | `AND` and `OR` combine: the `THEN` edge of `A AND B` has what either side gives; its `ELSE` edge only what both give on their `ELSE` edges. `OR` is the dual. Parentheses are looked through. | Inferred from G1. | `guards.p` |
| G3 | `DO WHILE` and `REPEAT WHILE` guard the body by their condition; the edge that leaves the loop is guarded by the negation only when the header has no `TO` and no record phrase (a `TO` limit leaves too, and `FOR EACH ... WHILE` also ends when its records run out). | Inferred from G1. | `loops.p`, `cases.rs` |
| G4 | An arm that does not reach the code after the `IF` (`LEAVE`, `NEXT`, `RETURN`, `RETURN ERROR`, `UNDO, ...`, `THROW`, `STOP`, `QUIT`, a `DO` block that ends with one) guards the rest: the flow graph only reaches it from the other arm. An arm that rejoins keeps the buffer maybe empty. | Documented (the flow graph's block semantics, section 5). | `guards.p` |
| G5 | `ERROR-STATUS:ERROR` in a condition, alone or compared with a logical constant (`= TRUE`, `EQ NO`, `<> FALSE`, ...): its false edge has a record in `b` when, on every path, the last statement with `NO-ERROR` is the `FIND` of `b` or `b` was refilled or guarded after that `FIND` (N8). A chain `IF NOT ERROR-STATUS:ERROR THEN FIND c ... NO-ERROR.` keeps the link. Any other `NO-ERROR` statement after an unguarded `FIND` breaks it. A call (`RUN`, a function or method) between the `FIND` and a test of `ERROR-STATUS` may or may not run a statement with `NO-ERROR`: the finds the test would link are Unknown from the call on. A call with no such test after it changes nothing here. | Inferred from N8. | `guards.p`, `cases.rs` |
| G6 | `CAN-FIND` is no guard (N9). `LOCKED b` (and `LOCKED(b)`) being true says `b` is empty; being false says nothing; so it proves nothing on either edge and is not Unknown. After `FIND ... NO-WAIT NO-ERROR`, `IF LOCKED b THEN RETURN.` still leaves `b` empty when no record matched. | Recalled, high. | `cases.rs` |
| G7 | A condition that names `b` at record level in a form the model does not read (`ROWID(b) = ?`, `RECID(b) <> ?`, `BUFFER b:AVAILABLE`, `AVAILABLE b = no`, `NEW b`) is Unknown for `b` on both edges; an Unknown operand stays Unknown through `AND`, `OR` and `NOT`. `ERROR-STATUS` in a form the model does not read (`ERROR-STATUS:NUM-MESSAGES > 0`, `ERROR-STATUS:ERROR = lFlag`) is Unknown on both edges for every find it links (G5). A `CASE` on `AVAILABLE b` or on `ERROR-STATUS` is Unknown in every branch. | Proposed. | `guards.p`, `cases.rs` |
| G8 | **Inside one statement.** A field of `b` in a statement that also tests `AVAILABLE b` anywhere (the `IF ... THEN ... ELSE` function, `AND`, `OR`, `DISPLAY ... WHEN`) is taken as guarded (N10). The test is by statement, not by operand, so `NOT AVAILABLE b AND b.f = x` is missed. In the same way a field read that runs only on some paths of its statement (`IF lFlag AND b.f > 0`, `IF x THEN b.f ELSE ...`, `DISPLAY b.f WHEN x`) proves `b` after the statement (K3) on every path: it is reported itself, but a later use on a path where it did not run is not. | Proposed (conservative). | `guards.p` |
| G9 | **Dependent finds.** `FIND c WHERE c.x = b.f NO-ERROR` with `b` empty raises the error of N3, which `NO-ERROR` suppresses. Whether `c` is then empty, or keeps the record it held before because the statement was abandoned (N7), is not known (no install was available to check). The model therefore uses the dependency only when, on every path into that `FIND`, `c` is empty (in the main block, which runs first, or in a routine for a buffer the routine defines and no other unit uses, before anything fills it; a default buffer belongs to the procedure file (B2) and may hold the record of an earlier call) or already holds a record of an earlier such `FIND` that read `b`. A guard or completed use that later proves `c` then proves `b`, and the model forgets the failed finds of `b`. On a path where the `FIND` did not run and `c` is still empty, the proof of `c` runs on no path, so it proves `b` too (`IF AVAILABLE b THEN FIND c ... . IF AVAILABLE c THEN b.f`). It follows each `FIND c` site, not the buffer: anything that may change `c` (a refill, a call that may change it, an Unknown) ends the dependency, and so does another `FIND b ... NO-ERROR` unless `c` is still empty. An `ERROR-STATUS` test proves it only when no other find of `c` may still have failed. | Inferred from N3; where `c` may hold another record the answer depends on N7 for `FIND`, which is unchecked, and the use is reported. | `guards.p`, `cases.rs` |

### 9.4 Where a record comes back

| # | The model | Confidence | Fixture |
|---|---|---|---|
| K1 | `FIND` without `NO-ERROR` completes only with a record (N1): after it, on the normal edge, `b` has one. Its raise edge keeps what held before. | Documented. | `finds.p` |
| K2 | `CREATE b` and `INSERT b` give `b` a new record. | Documented. | `finds.p` |
| K3 | A field reference to `b` in a statement without `NO-ERROR` that completes proves a record (N3): after it `b` has one, so a missing record is reported once per path, at its first use. A frame-field reference (N12) proves nothing. For a reference that runs only on some paths of its statement, see G8. | Documented (from N3). | `finds.p` |
| K4 | A `FOR` or `PRESELECT` phrase of `b`, an `OPEN QUERY` or `GET` that reads `b`, and any other write to `b` as a whole (`BUFFER-COPY ... TO b`, `DELETE b`, `RELEASE b`): Unknown. The `FOR` body has a record; after the block, and after a `GET`, whether it has one is not decided, so the model forgets the failed `FIND`. `RELEASE b` empties `b` for certain (N11), but a later use is no failed `FIND`'s fault. A `REPOSITION` statement may fill the buffers of its query (with a browse): the model forgets every buffer, as for the dynamic `REPOSITION-*` methods. | Documented for the `FOR` body; Inferred for the rest. | `loops.p`, `cases.rs` |
| K5 | A statement that may change `b` where the file does not show it: `b` passed as `BUFFER b` to a call, a statement form the model does not know that names `b` (also a temp-table: `RAW-TRANSFER x TO tt`, a word the parser does not know), a dynamic method on any handle (`FIND-*`, `GET-*`, `BUFFER-CREATE`, `BUFFER-COPY`, `FILL`, `QUERY-OPEN`, `REPOSITION-*`, `SYNCHRONIZE`, `RAW-TRANSFER`: the handle may be `BUFFER b:HANDLE`), and an unexpanded `{...}` or an elided `{&name}` in the statement. Unknown for `b`, or for every buffer when the target cannot be told. A method is a name written right against the colon after its operand (`h:FIND-FIRST`); the colon that ends a block header or a label is none (`DO: find-count = 1.`). | Inferred. | `unknown.p`, `cases.rs` |
| K6 | A statement that runs other code of the file or another program (`RUN`, a user-defined or dynamic function, `SUPER`, a method call, `PUBLISH`, `APPLY`, `WAIT-FOR`, an input statement whose triggers may run) may change a shared buffer and any buffer another routine of the file names: Unknown for those. | Inferred. | `unknown.p` |
| K7 | Each routine starts with no failed `FIND` known: a buffer of the main block that an internal procedure uses may hold any record when the procedure is called. | Proposed (conservative). | `units.p` |
| K8 | **Error paths are not followed.** The flow graph lets every statement without `NO-ERROR` raise ERROR (section 5), also an `IF AVAILABLE b` test that cannot. On a raise edge the model forgets every failed `FIND`: where a statement raised, the error is the first problem. A `CATCH` or `ON ERROR` arm that reads a field of a buffer a `FIND` in its block may have left empty is therefore not reported. | Proposed (conservative). On the second corpus this removed 42 of 825 findings; those read were raise paths from statements that cannot raise. | `finds.p` |

### 9.5 What the model leaves Unknown

- **The whole file,** when the scope analysis has a file-level Unknown (section 7.7): a recovered
  parse, an unexpanded include, or a file past the work budget.
- **A buffer** whose table the loaded schema does not have. With no schema, database buffers are
  never judged (a name may be a temp-table the file does not show); temp-table buffers are.
- **A statement** that holds an elided reference (an undefined `{&name}`, a `{N}` with no
  argument): it may hide `NO-ERROR`, a guard or a write. The model forgets every failed `FIND` there
  and reports nothing in it.
- **A branch of a statement-level `&IF`** that the preprocessor left in the text (its test was
  not decided): the branch may never be compiled, so a use in it is not reported. Its statements
  still refill and forget.
- **Correlated guards.** A guard through a variable (`IF NOT AVAILABLE b THEN lError = TRUE.`
  ... `IF lError THEN RETURN.`) or a loop that runs only with a record is not seen: the model is
  path-insensitive. These are the false positives left (section 9.7).
- Everything in K4 to K8.

### 9.6 The rule NL003 (M5, first slice)

NL003 `field-after-failed-find` (`strict.nulls`, preview, warning) selects findings from
`oxabl_nulls`; `oxabl_lint` computes one analysis per file through `RunShared`, over the scope
analysis the `LK`, `TX` and `DB` rules use, and only when the rule is enabled. It is routed like
them.

A field of `b` is reported where `b` may be empty because a `FIND ... NO-ERROR` of `b` may have
failed (N1, N5) and no guard (G1-G9) or refill (K1-K8) lies on some path between them. It rests on
N1, N3 (Documented) and N7, N10 (Recalled, medium). The message quotes the `FIND`. One finding per
statement and buffer, at its first field reference; a statement in a `FINALLY` block counts once.

Left out:

- a reference in a statement with `NO-ERROR` (N7), and the search criteria of a `FIND`, `FOR`,
  `CAN-FIND` or `OPEN QUERY` of `b` itself;
- a frame-field reference (N12), which needs no record;
- record-level uses of an empty buffer (`DISPLAY b`, `BUFFER-COPY b TO c`, `DELETE b`), which
  raise the same error: a later slice may add them;
- a `FOR FIRST` or `FOR LAST` that finds nothing (it runs no iteration, so no field is read);
- `FIND CURRENT ... NO-ERROR` as a source (N6), and `RELEASE` and `DELETE` (N11);
- error paths (K8);
- everything section 9.5 leaves Unknown.

### 9.7 Corpus check

Verified, counts only. Over the two corpora of section 7.6, as `oxabl check` expands them, with
their schemas and `strict.nulls` switched on (`crates/oxabl/examples/nulls_corpus.rs`): no panic.
NL003 gives 9,154 findings in the first corpus (10,619 programs and 3,549 include files checked)
and 714 in the second (2,526 and 782). 3,823 and 49 of them are at the `{...}` site of an
include; two include lines of the first corpus account for 3,583 of those, so the findings have
5,370 and 701 distinct anchors.

Triage. Fifteen random findings per corpus were read against the code, in rounds; each round's
wrong ones changed the model:

- **`FIND CURRENT ... NO-ERROR`** after the record was used (2 of the first 15, second corpus):
  now no source (N6).
- **Raise paths** (an `IF NOT AVAILABLE` test that "may raise" into an `ON ERROR` arm that
  rejoins; 1 of the first 15, first corpus): error paths are no longer followed (K8).
- **A table name hidden by a variable of the same name** in `AVAILABLE` (3 of the first 15,
  first corpus): G1 now looks the name up.
- **`ERROR-STATUS` chains** (`IF NOT ERROR-STATUS:ERROR THEN FIND ... NO-ERROR.` repeated, then
  one test): G5 now keeps the link (1 of 15).
- **Dependent finds** (`FIND c WHERE ROWID(c) = TO-ROWID(b.id) NO-ERROR.` and a guard of `c`
  only; 4 of 15, second corpus): G9.
- **A loop over `RECID(b)`** that runs only with a record (1 of 15): K4 covers a loop header that
  names `b`.

In the last round, 14 of 15 were right in each corpus; the one wrong finding of each was a
correlated guard (a variable, a dependent find proved by a later use), and the final model drops
both without dropping a right one. Correlated guards through a variable (section 9.5) are the
known class left. That is a false-positive rate near 7 % on a sample this small, so the rule
stays a preview warning.

The right findings are of four kinds: a control or setup record found with `NO-ERROR` and used at
once (the most common; the record exists in every working database, but the code does not say
so); a record found by `ROWID` or `RECID` passed in by the caller; a not-available arm that
reports the problem and then falls through to the use; and real bugs. The samples held at least
eight that look like bugs: four tests of the wrong buffer (`FIND bOrder ... IF NOT AVAILABLE
Order`), one `IF NOT AVAILABLE b THEN x = b.f`, one message in the not-available arm that names a
field of the missing record, and two not-available arms that set an error flag and fall through
to the use.

**Review round.** An adversarial review of the first slice changed the model as rows N12, G3,
G5-G9, K4 and K5 now say. The counts moved to 9,774 findings in the first corpus (3,837 at a use
site) and 825 in the second (55), from 9,154 and 714. Measured one change at a time against the
final model:

| Change | First corpus | Second corpus |
|---|---|---|
| Frame fields are no record reads (N12): all were `@` targets | -48 | 0 |
| G9 only where `c` was empty (or already depended on `b`) before the dependent `FIND` | +308 | +45 |
| `LOCKED b` proves nothing (G6), no longer Unknown | +161 | +61 |
| A call before an `ERROR-STATUS` test makes it Unknown (G5) | 0 | -1 |
| The rest: G9 per `FIND` site, not per buffer (+218 and +8), and the Unknown cases of G7, K4 and K5 (-7 and -2), with the include routing that follows | +199 | +6 |

A first cut of N12 took any `tt.f:attr` as a widget: on a temp-table field that holds a handle
(`tbuffer.bufferhandle:BUFFER-FIELD(...)`) that hid 67 real reads, so a temp-table field now needs
`IN FRAME`; `INPUT` after `(` or `,` is a parameter mode (33 hidden reads in the second corpus).
The G9 additions are records found with `NO-ERROR` and not tested, whose dependent `FIND` ran
where `c` may hold a record (a loop, a file-level buffer an internal procedure also fills); the
`LOCKED` additions are the `EXCLUSIVE-LOCK NO-WAIT NO-ERROR` + `IF LOCKED b THEN ...` idiom,
where no record matched. Fifteen new random findings per corpus, read against the code: 15 of 15
right in the first corpus; 12 of 15 in the second, where the three wrong ones were in the
earlier output too and are correlated guards (two through a variable, one through an
`ERROR-STATUS` test of a lock `FIND` keyed on `RECID(b)`). Four sampled G9 additions and two
`LOCKED` additions were right.

**Cost.** With `strict.nulls` off nothing is computed: `oxabl check` gives byte-identical output
on both trees before and after this change, and its wall time is the same within the noise. With
the group on, the scope analysis the rule reads dominates (it is shared with `strict.locks`); the
failed-find analysis on top of it is within the noise on the first corpus and under a second on
the second (12.1 s and 12.9 s with `strict.locks` on, in one process).

### 9.8 What NL004 and later value analysis need

NL001 and NL002 have local variable facts and expression walks (section 9.9). NL004 reads the
schema facts of V2 (`MANDATORY`, schema `INITIAL`) and the field references of `oxabl_scope`, and
applies the same sinks to fields (section 9.11). Most fields are not mandatory, so its trigger is
narrow: schema `INITIAL ?`. An annotation such as `@strict.nullable` could widen it. Missing
records remain the separate NL003 analysis; a missing record raises ERROR instead of supplying
`?`.

Later work must connect function returns and procedure `OUTPUT` parameters to the call graph
(section 8.5). Unseen call results and output values remain Unknown. Local `LIKE` declarations,
array elements, shared state, and references to variables owned by another unit also need
resolved type and storage facts. They do not supply a default `?` source in this slice.

From the review of the first slice: V2 says an `OUTPUT` parameter the callee does not set is
`?`; as recalled, an `OUTPUT` parameter starts at the initial value of its data type (`0`, `""`,
`no`), so it is `?` only for the types whose default is `?`. Section 9.9 models it that way;
check it on an install. Check V6, V7 and V8 there too (NL001 relies on V6 and V8), and V3's
division by zero (now Unknown). Also check G9's premise there (`FIND FIRST c NO-LOCK.`, then `FIND c
WHERE c.x = b.f NO-ERROR` with `b` empty, then `AVAILABLE c`): if `c` is emptied, G9 can apply
where `c` held a record too. One include line can carry thousands of NL003 use-site copies
(3,583 from two lines in the first corpus); one finding per include line per run, or a summary
line, would cut that noise.

### 9.9 Local unknown-value facts, NL001 and NL002

`FileNulls::with_values` adds a forward dataflow over each unit's flow graph. It reads the
parsed expressions. `value_before(unit, node, name)` gives the value of a local before a flow
node: unreachable, known, possibly `?`, or Unknown. Internally, separate bits record known
values, proven `?` sources, and paths whose value is Unknown. A join keeps a proven source even
when another path is Unknown. An Unknown path alone never supplies a source or a finding.

**Termination.** A narrowing that the edge alone decides may turn an Unknown local into a source
(V5) or into a known value. A narrowing that depends on the fact (the decided side of a false
`A AND B` or a true `A OR B`, and `a = b`) is weak: it only removes values, so a larger fact never
gives a smaller result. With that the statement and edge transfers are monotone; a property test
checks them on the solved facts of a hundred random programs, and a unit test checks the
expression transfer on every pair of lattice states. The atoms of the correlated tests below
(logical flags, `AVAILABLE`, counted loops) are even, so `a ^ 1` is the opposite of `a`. The
shared solver of `oxabl_flow`
also joins the previous fact into the new one at every loop head, so a transfer that is not
monotone still ends on this finite lattice, and each solve runs within a work budget (64 visits
per node, and 1,000,000 units of work). A visit costs the size of its fact: a unit per few locals,
and for each source of `?` a unit per word of its conditions. So a unit with many locals and a long
loop costs a bounded time (about a second at most, in release), not minutes. A unit past the budget
is Unknown for every local and gives no finding; so does the failed-find analysis for its unit
(which charges the size of its sets and the walk of its pairs). `FileNulls::exhausted` lists those
units. Random programs (300, fixed seed) must reach a fixed point
within the budget. The first version of this section said the transfers were monotone when they
were not: a `REPEAT` around `DO WHILE i <> ? AND d <> ?: i = ?. END.` never ended (a regression
test now).

**Sources** are V1 and the local part of V2: the literal `?`, `INITIAL ?`, the default of
`DATE`, `DATETIME`, `DATETIME-TZ`, `HANDLE`, `ROWID`, `RECID` and object references, and an
assignment from a possibly `?` expression. A test `x = ?` (or `x <> ?`) on a value the model
cannot decide makes `x` a `?` source on the edge where the test says so (V5, Documented): the
code itself says that `x` can be `?`. `INPUT` and `INPUT-OUTPUT` parameters start Unknown;
`OUTPUT` parameters start at the default of their type (section 9.8). Method and constructor
parameters count. An unseen function or method result is Unknown. `IMPORT`, `UPDATE`, `SET`,
`MESSAGE ... UPDATE` and call `OUTPUT` arguments forget the value. An unseen call in an
expression also forgets its variable arguments (the AST has no argument modes there), and any
call forgets the locals another unit of the file writes. So do `WAIT-FOR`, `APPLY`, `ENABLE`,
`PUBLISH`, `UPDATE` and `SET`, which may run triggers (K6); `CHOOSE`, `PROMPT-FOR`, `INSERT` and
`READKEY` are opaque statements. Opaque statements, elided preprocessor
text, explicit `UNDO` and error paths forget every local. `LIKE`, `EXTENT` and shared variables
are Unknown.

**Propagation.** V3 arithmetic, unary minus and V4 string `+` propagate `?`; a division by a
constant zero (the medium part of V3) is Unknown, so it makes no finding. V5 `=` and `<>` absorb. The V10 functions propagate
an argument that is already a `?` source; they never make one. `STRING`, `VALID-HANDLE`,
`VALID-OBJECT`, `AVAILABLE`, `CAN-FIND` and `LOCKED` absorb (V11). `SUBSTITUTE` is Unknown
(low confidence). V9 takes the `ELSE` value for a definite `?` condition and joins the arms
otherwise. V12 conversion errors are no source; V13 is why a `MANDATORY` field is no source of NL004.

**V6 and V7 never make a value.** The result of an ordering comparison, `BEGINS`, `MATCHES`,
`NOT`, `AND` or `OR` is Unknown as a value. The model uses V6, V7 and V8 to remove sources: the
edge a `?` condition cannot take (the `THEN` edge, the loop body) drops the sources that would
make the condition `?`. If they are wrong there, the model misses findings. NL001 is different:
it reports a `?` source that reaches an ordering comparison or a condition, and that use is a
defect only if V6 (the comparison gives `?`) and V8 (a `?` condition skips the `THEN` branch or
ends the loop) hold. Both are Recalled (low and medium) and unchecked, so NL001 can be wrong
where they are; it stays preview until they are checked on an install, and its help says so.
NL002 rests on V3 and V4, Documented for `?` operands.

**Guards and correlated tests.** `x = ?`, `EQ`, `<>`, `NE`, `NOT`, the true arms of `AND` and
the false arms of `OR` narrow both edges, and a false `A AND B` (or a true `A OR B`) whose one
side is decided narrows the other. The expression walk narrows each operand of `AND`, `OR` and
the `IF` function by the others, and an inner test re-applies the enclosing ones, so
`(AVAILABLE b AND NOT AVAILABLE c) OR (AVAILABLE b AND x > y)` reads `x > y` with `c` available.
A true ordering test proves its operands known in that arm, so `x > limit OR x = ?` does not
report. `RETURN`, `LEAVE` and `NEXT` use the exits of the flow graph.

Each source carries the conditions that hold on every path that carries it: the locals that are
`?` there (pairs such as `a = ? AND b = ?`), the result of `AVAILABLE b`, and the value of a
logical local. A join keeps a condition only where every such path has it; a write to the local
or a statement that may change the record in `b` removes it (anything but an assignment, `IF`,
`DO`, `REPEAT`, `CASE`, `DISPLAY`, `MESSAGE`, `PUT`, `EXPORT`, `LEAVE` or `NEXT` without an
unseen call). A test that contradicts the conditions of a source removes the source on that
edge. So `IF AVAILABLE b THEN d = b.d.` and then `IF AVAILABLE b AND d > TODAY` or
`IF found THEN ... d` after `found = FALSE. IF ... THEN ASSIGN found = TRUE d = TODAY.` do not
report. A condition can name the locals a test of the unit reads (up to 256); value facts cover
every local.

**Counted loops.** A `DO i = a TO b` body runs only when `a` and `b` are not `?` (`i <= ?` is
`?`, V6 and V8), so the body edge proves both known: `DO i = 1 TO NUM-ENTRIES(list)` reads `list`
as known in the body. A `TO` loop can end before its `WHILE` condition is false, so its exit
does not narrow by the `WHILE` condition. A `DO v = a TO b [BY k]` without `WHILE` whose
constant bounds give an iteration leaves only after one: a source that only the entering paths
carry is gone at its exit (`NEXT` and `RETRY` still come from iterations that may skip the rest
of the body). `REPEAT v = a TO b` writes `v` at its head; the AST does not keep that phrase, so
the loop variable is read from the tokens. An integer local that is never written keeps its
initial value, which decides constant tests.

**`NOT` binds looser than a comparison** (Documented in the operator precedence table of the
ABL reference: `NOT` comes after the relational operators and before `AND`). The parser reads `NOT a BEGINS b` as
`(NOT a) BEGINS b`; this analysis reads such a comparison as `NOT (a BEGINS b)`.

**The rules.** NL001 reports a local with a proven possible source of `?` that is an operand of
`<`, `<=`, `>`, `>=`, `BEGINS` or `MATCHES` (also through the V10 functions, such as
`LENGTH(x) > 0`), or that is the whole condition of an `IF` or `WHILE`. `=` and `<>` never
report. NL002 reports such a local in arithmetic or string `+` whose result is assigned,
displayed, written by `MESSAGE`, `PUT` or `EXPORT`; an absorbing function stops the chain and a
propagating one carries it to the sink. Each rule reports a local once per unit, at its first
use: later uses repeat the same source. Fields are Unknown here; NL004 reads them (section 9.11).
Both rules are opt-in
`strict.nulls`, preview, Warning, without an autofix, and use the include routing of NL003.
With the group off, no null analysis is computed.

The `value_*` and `values` fixtures cover defaults, parameters, assignments, joins, built-ins,
low-confidence results, guards, correlated tests, `NOT`, loops and unreachable paths. The lint
tests check the opt-in policy, the first-use rule and both documentation examples.

### 9.10 Corpus check of NL001 and NL002

Over the two corpora of section 7.6, as `oxabl check` expands them, with their schemas and
`strict.nulls` on (`crates/oxabl/examples/nulls_corpus.rs`): no panic. NL001 gives 93 findings
in the first corpus and 16 in the second; NL002 gives 44 and 28. None is at an include site in
the first corpus; three NL002 findings are in the second.

Triage. Fifteen random findings per rule were read against the code in the first corpus, in
rounds, each round's wrong ones changing the model; in the second corpus every finding was read
(16 of NL001 and 28 of NL002).

| Round | NL001 wrong | NL002 wrong | What was wrong, and the change |
|---|---|---|---|
| 1, first corpus | 4 of 15 | 4 of 15 | A value set under `IF AVAILABLE b` and read under a second test of `b` (now a condition of the source); a body of `DO i = 1 TO NUM-ENTRIES(x)` with `x` `?` (four, one variable: counted loops); an `IF ... ELSE IF` chain whose arms cover every case (two); a `FOR EACH` that must find the record a `FIND` found |
| 1, second corpus | 0 of 16 | 1 of 28 | A variable set in both arms of `IF NOT AVAILABLE b` / `IF AVAILABLE b` (conditions of the source) |
| 2, first corpus | 1 of 15 | 2 of 15 | A pair of tests in a unit with more than 32 locals (conditions only named the first 32; now the tested locals); `NOT x BEGINS "/"` read with the wrong precedence; a `CASE` without `OTHERWISE` that leaves both a date `?` and a loop limit `0` |
| 3, first corpus | 0 of 15 | 1 of 18 | The same `CASE` pattern in a copy of that file |

The rules also reported the same variable many times in one unit (up to six uses of one source);
it now reports the first use. The wrong findings left are correlations the model does not
follow: an `IF` chain that covers every case, a loop limit that is `0` exactly when a date is
`?`, and a record loop that must run. That is near 5 % on these samples.

Most right findings are of three kinds: an option or setting read with `NO-ERROR` that sets the
variable to `?` when the stored value is missing or bad, and then a test that treats `?` as
`no`; a `DATE` or `DATETIME` that stays at its default when a lookup finds no record, then
compared or subtracted; and a running total that turns `?` after one unknown term. Many of these
rely on `?` behaving as `no` on purpose. The samples also held real bugs: a date compared before
it is assigned (twice, a dead check), a flag tested where a test for `?` was meant, a fallback
for `?` that the next statement overwrites with the `?` value, and validation that reports a `?`
input and then goes on to compare it.

**Cost.** With `strict.nulls` off nothing is computed: `oxabl check --json --no-format .` gives
byte-identical output on both trees before and after this change, in the same wall time within
the noise. With the group on, in one process, the value analysis adds about 40 % to the run of
NL003 alone (first corpus 170 s to 242 s, second 12 s to 17 s; the group off takes 60 s and 5 s).
Most of it is in the shared solver: every statement that may raise has an edge whose fact the
solver clones and this analysis replaces, and the solver also computes the fact after every node,
which this analysis does not read. A solver hook for both would remove about a third of it.

### 9.11 Nullable database fields, NL004

**Trigger.** A field is a source of `?` when the loaded schema gives it `INITIAL ?` (the bare `?`
a `.df` writes for the unknown value; `INITIAL "?"` is a string, which `Field::initial_is_unknown`
tells apart), does not make it `MANDATORY` (V13) and gives it no extent. Any record created
without assigning it holds `?` (V1, V2). A field with no `INITIAL` attribute in the `.df` is no
source: a dump always writes it, and the model does not read the type default. A temp-table
field, a table the schema does not have, a buffer with an Unknown reason of its own
(`oxabl_scope`), a file with a file-level Unknown and a run with no schema give no finding.
In the two schemas of section 7.6 about 15 % (4,400 of 30,191) and 17 % (408 of 2,461) of the
fields have `INITIAL ?`, and 40 and 17 of them are `MANDATORY`; most are dates.

**Model.** The analysis is a second solve of the value dataflow of section 9.9 per unit, over the
nullable fields the unit references and no locals (`FileNulls::with_fields`). The first solve is
unchanged, so NL001, NL002 and NL003 give the same findings as before (section 9.12), and a local
that is assigned from a field is Unknown, not a source: `d = b.f. IF d < TODAY ...` gives no
finding. The variable is the pair buffer and field, found by the span of the reference that
`oxabl_scope` resolved (so `b.f`, `db.b.f` and an unqualified `f` are one variable). Its value is
"possibly `?`" (known or `?`), not "certainly `?`", so a true ordering test or `b.f <> ?` proves
it known and `b.f = ?` makes it a proven source, as for a local. The guards and sinks are those
of NL001 and NL002 (the `IF` and `WHILE` condition, ordering operands, arithmetic and string `+`
that is stored or displayed).

The value is set again where the record changes:

| What | Value after it |
|---|---|
| Entry to the unit | possibly `?` |
| `FIND`, `GET`, `CREATE`; the body of a `FOR` | possibly `?` again (a guard before does not cover the next record) |
| The `WHERE` of a `FIND` or `FOR EACH` | holds of the record: an ordering test proves its operands known, and a field `=` a known value is known (fields only; for locals the model does not narrow this way) |
| Exit of a `FOR` loop | Unknown (no record) |
| `b.f = x`, `ASSIGN`, `UPDATE`, `SET`, `IMPORT` of the field | the value assigned; Unknown if the value is |
| Other writes to the record (`BUFFER-COPY ... TO b`, `DELETE b`, `UPDATE b`), a `BUFFER b` passed to a call, a statement the scope analysis marks opaque | Unknown for every field of `b` |
| An unseen call, `RUN` or trigger-running statement | Unknown for the fields of a buffer that is shared, a parameter or a trigger buffer, or that more than one unit of the file uses |
| `ASSIGN x = v WHEN c` | Unknown for `x`, and `v` is no sink: the parser drops `c`, so the model reads the `WHEN` token and does not follow the condition |
| `DISPLAY v WHEN c` | `v` is read with `c` true |

NL004 reports a field once per unit, kind and first use, like NL001 and NL002. It is a preview
`strict.nulls` rule, Warning, without an autofix, routed like NL003 (use site in the includer).
The comparison finding rests on V6 and V8 as NL001 does, and its help says so. The unit budget of
section 9.9 applies to the field solve too: a unit past it is in `FileNulls::exhausted` and gives
no field finding.

**Left.** A local that holds a field (`d = b.f`), a field copied to a field of another buffer,
function and method calls that return a field, `LIKE` locals, array fields and fields of
temp-tables are Unknown. Correlations with locals are not followed: `IF b.f = ? AND (x <> ? OR
y <> ?) THEN NEXT.` then `IF x <> ? AND b.f < x` reads `b.f` as possibly `?`. A `WHERE` that
equates a field to a local (`b.f = x`) proves nothing because `x` may be `?`. The `WHEN` of an
`ASSIGN` pair is dropped by the parser (NL002 has the same blind spot for locals; the field
analysis treats it as no sink, and the local analysis was left as it was). An abbreviated field
name has no scope reference, so it is never judged. Whether a field is `?` in practice (an
invariant of the application, a write trigger) is outside the model; those findings are right
by the rule and may still be harmless.

### 9.12 Corpus check of NL004

Over the two corpora of section 7.6, with their schemas and `strict.nulls` on
(`crates/oxabl/examples/nulls_corpus.rs`): no panic. NL004 gives 595 findings in the first corpus
and 165 in the second (16 and 21 at a use site). NL001, NL002 and NL003 give
exactly the findings they gave before (93, 44 and 9,774; 16, 28 and 825), line for line. With
`strict.nulls` off, `oxabl check --json --no-format .` is byte-identical on both trees. The counts
by top directory (directories with none left out) are:

- First corpus: `oe` 103, `mf` 75, `crm_webhost` 42, `po` 38, `ar` 35, `ap` 29, `ms` 27, `ic` 25,
  `wam_tmpl` 25, `rm` 20, `wam` 19, `sa` 18, `gl` 13, `zf` 13, `dotnet` 12, `vp_webhost` 11,
  `cv` 9, `web` 8, `zt` 7, `dotnetrm` 6, `inc` 6, and 5 or fewer in 17 more.
- Second corpus: `wms_webhost` 72, `rf` 49, `remote` 38, `eod` 4, `ifaces` 2.

Triage.

| Round | Wrong | What was wrong, and the change |
|---|---|---|
| 1, first model, about 25 read in the two corpora | 1 of 25 | `ASSIGN t.d = b.x - b.y WHEN b.x <> ? AND b.y <> ?` (the guard is a `WHEN`; see the table above) |
| 2, 15 new random findings per corpus | 0 of 15, 0 of 15 | |
| 2, the findings whose preceding 40 lines test the same field against `?` (15 and 6) | 1 of 21 | a guard through a local: `IF f = ? AND (x <> ? OR y <> ?) THEN NEXT.` then `IF x <> ? AND f < x` |

That is a false-positive rate under 3 % (2 of 76) on the findings read. The one wrong finding left is
a correlation with a local, a known class (sections 9.7 and 9.10). "Right" means that the field
can hold `?` there and nothing in the unit says otherwise, not that the code is wrong: most
right findings are of four kinds. A date field read after a `FIND` or in a `FOR EACH` and
compared with a date or subtracted (`due-date`, `plan-pay-dt`, `period-end + 1`, `street-date`);
a number (WMS `item_qty`, `sugg_qty`) added to a running total or subtracted from another; a
character field concatenated into a message or a page; and a validation that reports a `?`
field and then compares it anyway (`IF f = 0 OR f = ? THEN log-error.` and then `IF f > g`).

**Cost.** With `strict.nulls` off nothing is computed. With the group on, in one process, the
field solve adds about 16 % to the run of the first corpus (209 s to 243 s) and 4 % to 8 % to
the second (13.4 s and 14.8 s to 14.0 s and 16.0 s); the local solve and NL003 take the rest.
The field solve runs only over the units that reference a nullable field, and only when a rule
that reads it is enabled. No unit of either corpus ran past its work budget.
