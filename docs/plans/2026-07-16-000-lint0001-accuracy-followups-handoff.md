---
title: "Handoff: LINT0001 accuracy — built-in fix + four follow-up plans"
type: handoff
status: active
date: 2026-07-16
origin: GitHub #58
related: ["GitHub #56", "docs/plans/2026-04-25-001-feat-semantic-v1-1-followups-plan.md"]
---

# Handoff: undefined-symbol (LINT0001) accuracy work

## TL;DR

`oxabl_lint`'s `LINT0001` (undefined-symbol) produced ~12.8M findings on a
~10k-file ABL corpus — **98.7% false positives** — because the semantic
resolver had no model of what counts as "defined" beyond locally-declared
symbols. The **dominant cause (built-in ABL functions) is now fixed and
merged into this branch.** Three further false-positive/fidelity gaps and one
correctness gap are **spec'd as four standalone feature-branch plans**, ready
to implement in order. A fifth item (cross-file SHARED) is deferred.

## What shipped (this branch: `fix/lint0001-builtin-registry`)

**Built-in ABL function registry.** Calls to built-ins (`LENGTH`, `ENTRY`,
`SUBSTRING`, `TRIM`, `ROUND`, `NUM-ENTRIES`, `STRING`, `AVAILABLE`, ...) were
the single largest false-positive class. Fix:

- `crates/oxabl_codegen`: new `parse_builtin_functions()` collects every
  `"<NAME> function"` title from `resources/abl_keyword_index.json` (walking
  raw entries, not the collapsed one-type-per-name map — so function/statement
  homonyms like `ENTRY`/`LENGTH`/`AVAILABLE` keep their function role). New
  `generate_builtins_rs()` emits a sorted, deduped, whitespace-free registry.
  New `builtins` codegen command (also part of `all`).
- `crates/oxabl_lexer/src/builtins.rs` (generated): `BUILTIN_FUNCTIONS: &[&str]`
  (224 names) + `is_builtin_function(name)` via `binary_search`.
- `crates/oxabl_semantic/src/resolve.rs`: `resolve_expr_ident` consults the
  registry after local-scope lookup fails; a match records
  `Unresolved{External}` (skip-listed by every lint rule) instead of
  `NotInScope`. Local declarations still resolve first, so shadowing is intact.
- Tests: lexer registry (membership, sorted/deduped/no-whitespace invariants),
  lint (built-ins don't fire, case-insensitive, non-built-ins still fire).

Full workspace suite green (958 tests). `cargo fmt` / `cargo clippy -D warnings`
clean. Verified end-to-end through the real `analyze_file` pipeline.

**Not yet abbreviation-aware:** abbreviated built-in calls (e.g. `AVAIL(x)`)
still fire — addressed by follow-up plan #001.

## Follow-up plans (each = its own feature branch + its own benchmark)

All four live in `docs/plans/2026-07-16-00{1..4}-*.md`. Each was researched
(Haiku), spec'd (Opus), and reviewed against the live source (Fable); every
plan's critical review findings were folded back in. Recommended order below is
cheapest/highest-confidence first; reorder to `002` first if you want to
front-load the biggest accuracy win.

### 1. Built-in function abbreviations — `feat/lint-builtin-fn-abbreviations` (~0.5d, low risk)
`docs/plans/2026-07-16-001-feat-lint-builtin-fn-abbreviations-plan.md`

Only the **16** built-in functions that are *also* reserved keywords with a
documented `min_abbreviation` (AVAILABLE→AVAIL, AMBIGUOUS→AMBIG, ...) can be
abbreviated in ABL; functions/statements otherwise cannot, so LENGTH/SUBSTRING
need nothing. Extend `generate_builtins_rs` to emit each abbreviation's prefix
range, selecting the set by joining `min_abbreviation.is_some()` with membership
in the `parse_builtin_functions` set (**not** `keyword_type`, whose collapsed
map drops AVAILABLE/AMBIGUOUS/TERMINAL). No dedicated bench (O(log n) lookup) —
guarded by registry invariant + coverage tests.

### 2. Schema-backed resolution — `feat/semantic-schema-resolution` (~3–5d, medium risk) — HIGHEST VALUE
`docs/plans/2026-07-16-002-feat-semantic-schema-backed-resolution-plan.md`

Make a loaded `.df` Schema drive single-file resolution: add `table_id` to
`Symbol` (populated for `DEFINE BUFFER`, `FOR EACH`, temp-tables), real field
lookup in `resolve_field_access` (both the resolved-qualifier **and** the
bare-`Customer.Name` `None` branch), a schema fallback for bare table names,
type validated fields (`SchemaType→ResolvedType`), make `LINT0003` actually
fire, and wire `--schema` into the `analyze` subcommand. Design: **Option A** —
synthesize field/table symbols and reuse `Resolution::Resolved(SymbolId)`, no
new `Resolution` variant (preserves the cross-file sketch invariant). Requires
threading `&mut SymbolTable` into `ResolveWalker` and a real declare-time
`SchemaRevision` staleness guard. New schema-loaded resolve benchmark.

### 3. Include-path config + loud unresolved-include diagnostic — `feat/analysis-include-path-config` (~1–2d) — refs #56
`docs/plans/2026-07-16-003-feat-analysis-include-path-config-plan.md`

Includes already expand inline and `--include-path` is already wired, so this is
**not** new resolution machinery. It is: (a) auto-load `oxabl.toml`
(`WorkspaceConfig::from_path` exists) and merge with CLI flags; (b) a **loud**
`PREPROC007` diagnostic when an include can't be resolved (today it degrades
silently → cascading FPs — the #56 "loud beats silent" principle), surfaced in
both text and JSON output, with correct FileId/span handling for nested
includes; (c) PROPATH first-match-wins fidelity (docs + tests; `resolve_include`
already implements it). Helpers move into `oxabl_workspace` for testability.
New multi-dir include-resolution benchmark.

### 4. Within-file SHARED variable flags — `feat/semantic-within-file-shared` (~1–2d, low risk)
`docs/plans/2026-07-16-004-feat-semantic-within-file-shared-plan.md`

Retrofit `is_shared`/`is_new_shared`/`is_new_global_shared` onto
`VariableDeclaration`/`DefineTempTable`/`DefineBuffer` (parser already captures
them; only `DefineDataset` stores them today) and apply the matching
`SymbolFlags` in the declare pass. **Honest framing:** this is model
*correctness*/foundation, **not** an undefined-symbol FP win — a consumer's
`DEFINE SHARED VARIABLE y.` already declares `y` locally. Must **retrofit
`DefineDataset` in lockstep** (the third GLOBAL flag) to avoid regressing
`NEW GLOBAL SHARED DATASET`. Requires an `ast-invariants.md` §12 update
(CLAUDE.md mandate). No dedicated bench; guarded by a `size_of::<StatementKind>()`
assertion.

## Deferred — item 5: cross-file SHARED resolution

`DEFINE SHARED VARIABLE y` bound to a `NEW SHARED` producer in another
compilation unit needs a project-wide symbol index that does not exist today
(`oxabl_workspace` is file-discovery only). High effort, low FP value (the local
re-declaration means these rarely misfire). Reserve for the "whole-codebase
model" pass alongside the cross-file work sketched in
`docs/design/semantic-v1-cross-file-sketch.md` and the v1.1 combined plan.

## Working notes for the next session

- Each feature is intentionally its own branch off `master` so it carries an
  independent CodSpeed run — regressions are attributable per-feature rather
  than smeared across one big benchmark.
- Prior art: `docs/plans/2026-04-25-001-feat-semantic-v1-1-followups-plan.md`
  sketches schema/cross-file/flow as one combined "v1.1" doc; the four plans
  above are the narrower, independently-shippable slices of that ambition.
- Regenerate the built-in registry with `cargo run -p oxabl_codegen -- builtins`
  (or `-- all`); it carries a DO-NOT-EDIT header.
- Order of attack: start with **#1**, do not begin until explicitly asked.
