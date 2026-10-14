---
title: "Strict analysis: answers to the open questions"
status: proposed
date: 2026-10-14
---

# Strict analysis: answers to the open questions

This note answers two of the three open questions in
[`strict-analysis-plan.md`](strict-analysis-plan.md): where annotations may appear, and where
diagnostics inside include files are reported. The third (PROPATH and include resolution parity
with the AVM) stays open.

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

**Still to verify against a licensed install:** the compiler's treatment of an annotation in a
position it does not know, the exact list of annotations it reads itself, and whether unquoted
attribute values compile.

## 3. Where are diagnostics inside include files reported?

### Current behaviour (verified)

oxabl reports a diagnostic that comes from an include **nowhere** when it checks the file that
includes it. It reports the diagnostic in the include only when someone checks the include as its
own root.

- **Dropped at the includer.** The pipeline lints the expanded text, then maps each span back to
  real bytes. `resolve_diagnostic` (`crates/oxabl_analyze/src/collect.rs:971`) returns `None`
  for any span whose first byte is not in the root file, and `resolve_span`
  (`collect.rs:407`) is where that origin check happens. A semantic or lint finding inside an
  include is therefore dropped. So is a label that points into an include. A parse error inside
  an include is dropped at the same place (`collect.rs:925-926`), although the recovery flag is
  set first (`collect.rs:922`). The edge set still learns that the parse recovered, and the CLI
  suppresses format drift for the file, but no diagnostic is shown.
- **Preprocessor diagnostics follow the same rule.** Only loud diagnostics whose own span is in
  the root file are kept (`collect.rs:529`, `collect.rs:604`).
- **One exception, shown at the use site.** An unresolvable include found below the root is
  reported as `PREPROC007` at the outermost `{...}` in the root file that led to it, with the
  note "reached through this include" (`collect.rs:622`). It is reported once per root site and
  missing name. The reason is that such a gap hides symbols from the includer's own analysis.
- **Use-site coordinates already exist.** `ExpandedFile::resolve_include_site`
  (`collect.rs:318`) maps any expanded span to the root-level `{...}` that brought it in, at any
  depth. The dependency section uses it for `include_site` on every edge row
  (`crates/oxabl_analyze/src/lib.rs:135`, `:832`).
- **Walks do not check includes.** `.i` is not a root extension, so `oxabl check <dir>` and
  workspace discovery skip include files (`crates/oxabl_workspace/src/discovery.rs:44-47`, the
  reasoning in the module docs, "R9"). A path named on the command line, or a buffer open in the
  language server, is a root regardless (`discovery.rs:54`).
- **Checking an include as a root is deliberately quieter.** The pipeline marks it
  `SourceContext::IncludeFragment` (`crates/oxabl_pipeline/src/lint.rs:819`). Unresolved local
  names count as external, and the unused-variable, dead-store and block-variable rules stay
  silent (`crates/oxabl_semantic/src/lib.rs:207`, `crates/oxabl_lint/src/lib.rs:54`). A fragment
  often does not parse alone or does not see its includer's declarations.

Confirmed on a synthetic example: an unused variable, an undefined name and a syntax error inside
an include produce no finding when the includer is checked. The syntax error appears only when
the include is named directly.

**Net effect.** In a directory run, a finding that lives in an include is never shown. For the
current rules that is an acceptable loss, because the rules are noisy in fragments. For strict
rules it is not. `RUN VALUE`, a shared variable, or a missing `NO-UNDO` in a widely included
file is the kind of thing the strict groups exist to find.

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
findings. None of this is needed before M1 ends. The three syntactic rules in M1 can report at
the root file first and move to the include later, so long as the baseline key is chosen as
above from the start.

Open points for later: how the language server shows an include finding while the includer is
open, and how an include with several roots in one run picks its context for rules that are
"mostly" context-free.
