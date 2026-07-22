---
date: 2026-04-16
topic: semantic-layer
---

# Semantic Layer (v1)

## Problem Frame
The lexer, parser, and preprocessor pipeline now successfully consumes our real-world ABL corpus. The next compiler stage — semantic analysis — turns the AST into a meaningful program model: who declared what, what scope a name resolves in, what type each expression has, and which database tables/fields are referenced. Without this layer, oxabl can prove ABL is *syntactically* valid but cannot answer any meaningful question about it. Every downstream product surface we care about (linter, LSP, refactoring, codemods) sits on top of this foundation, so the v1 design needs to be a credible foundation, not just a one-shot analyzer.

## Requirements

### Core
- R1. Build a symbol table and lexical scope model for ABL programs: variables, parameters, temp-tables, buffers, properties, methods, classes, interfaces, procedures, functions, streams, frames, events.
- R2. Resolve every identifier reference in the AST to a declaration (or to "unresolved" with a structured reason).
- R3. Annotate every expression node with an inferred type drawn from ABL's type system (INTEGER, INT64, DECIMAL, CHARACTER, LOGICAL, DATE/TIME variants, HANDLE, ROWID, RECID, RAW, MEMPTR, LONGCHAR, CLOB, BLOB, COM-HANDLE, class types, table/buffer types, unknown/`?`).
- R4. Type-check operations and assignments using ABL's coercion rules, producing diagnostics on incompatibility.

### Schema integration
- R5. Load and parse Progress `.df` schema files when available, exposing tables, fields, indexes, and field datatypes to the resolver.
- R6. Resolve `DEFINE BUFFER … FOR <table>`, `… LIKE <table>.<field>`, bare-field/`table.field` references, and FOR EACH/FIND targets against loaded schema.
- R7. When no schema is loaded, all schema-dependent checks are silently skipped (not reported as errors). Pure source-level analysis still runs and still produces useful diagnostics.

### Proof-point linter
- R8. Ship a minimal linter consuming the semantic layer with these v1 rules:
  - `undefined-symbol` — reference to an identifier with no in-scope declaration.
  - `unused-variable` — declared variable or parameter that is never read.
  - `unknown-table-or-field` — buffer/field reference unmatched in the loaded `.df` schema. Suppressed entirely when no schema is loaded.
  - `type-mismatch-assignment` — assignment whose RHS type is not compatible with the LHS under ABL coercion rules.
- R9. Ship an internal `analyze` dump tool (CLI) that prints the resolved symbol table, scopes, and type-annotated AST for a given file. Used for validation and golden-file testing of the semantic layer; not a primary user-facing surface.

### Architectural guardrails
- R10. v1 may operate on a single file at a time, but the IR/API design must not preclude later cross-file resolution (`RUN "foo.p"`, `USING …`, class inheritance, include-file symbol leakage). No assumptions baked in that "one AST = one analysis unit forever".
- R11. Likewise, the design must not preclude later flow analysis (definite assignment, unreachable code, NO-UNDO enforcement). Avoid IR shapes that would force a rewrite to add it.

## Success Criteria
- The four v1 lint rules produce true positives and meaningfully few false positives on the ABL corpus, both with and without `.df` schema loaded.
- The `analyze` dump tool produces stable, golden-tested output on a representative fixture set covering each AST construct the parser supports.
- Onboarding a fifth lint rule in a follow-up does not require any change to the semantic layer's public API — only new code that consumes it.
- The deferred capabilities (cross-file, flow analysis) can be added without an IR rewrite; this is verified during planning by sketching how each would attach.

## Scope Boundaries
- No cross-file resolution in v1. `RUN "foo.p"`, USING-resolved class references, and include-file symbol leakage are recognized as unresolved-cross-file rather than analyzed. Architected for, not delivered.
- No flow analysis in v1. Same posture: don't preclude it.
- No LSP or refactoring tooling in v1. The semantic layer is built so they become possible; building them is separate work.
- No incremental / on-edit re-analysis in v1. Whole-file re-analysis is acceptable. Architecture should not assume immutability that would later block incrementality.
- No autofixes in the v1 linter. Diagnostics only.
- No `.df` *writing* or schema migration tooling. Read-only consumption of `.df`.
- No coverage of dynamic constructs beyond reporting them as unresolved-by-design (e.g. `RUN VALUE(x)`, dynamic queries, dynamic temp-tables).

## Key Decisions
- **Foundation-first, not feature-first.** The semantic layer is built as a shared model, with a small linter + dump tool as the first consumers proving the API is real. We are deliberately *not* shipping an LSP first because it bakes architectural choices (incrementality, cross-file) in too early.
- **Schema is first-class but optional.** Treat loaded `.df` schema as gospel for table/field resolution. Absence of schema degrades gracefully — schema-dependent checks are skipped silently rather than emitting noise. This lets oxabl run usefully on any ABL file without requiring a project setup.
- **v1 covers symbols+scopes and type annotation/checking; cross-file and flow analysis are deferred.** The cut is chosen so v1 is shippable in a reasonable horizon while still being a credible foundation. The deferred items are explicit non-goals for v1, not forgotten — the IR design is the contract that they remain reachable.
- **Four v1 lint rules chosen to validate the breadth of the semantic layer**, not to be a marketable lint suite: one rule per major capability (resolution, declaration tracking, schema integration, type checking).

## Dependencies / Assumptions
- The parser produces an AST stable and complete enough across the corpus to drive resolution. (Current state: yes.)
- The preprocessor produces a post-expansion token/AST stream the semantic layer can consume without re-running preprocessing — verify the seam during planning.
- ABL's type and coercion rules can be encoded from the Progress documentation we already reference for keyword data; spike during planning to confirm coverage gaps.
- A `.df` parser does not yet exist in oxabl. New work, scope to be sized in planning.

## Outstanding Questions

### Deferred to Planning
- [Affects R1-R4][Technical] New `oxabl_semantic` crate vs. extending `oxabl_ast` vs. an `oxabl_hir` IR layered on AST. Decide based on whether type/scope info attaches in-place or in a side table.
- [Affects R5-R7][Technical] Where should the `.df` parser live — its own crate (`oxabl_schema`?) or inside the semantic crate? How is the schema discovered (CLI flag, config file, search up the tree)?
- [Affects R3, R4][Needs research] Catalog ABL's type coercion and operator typing rules to a fidelity sufficient for v1 — what exists in Progress docs vs. what we infer from corpus behavior.
- [Affects R2, R6][Technical] Diagnostic / error-reporting format. Does the parser already have one we should reuse (likely yes), or do we need a unified diagnostics crate?
- [Affects R8][Technical] Linter packaging — separate `oxabl_lint` crate that depends on `oxabl_semantic`, or a `lint` subcommand of an `oxabl` binary?
- [Affects R10][Needs research] Sketch how cross-file resolution would attach to the v1 IR; this is the architectural-guardrail check, not a v1 deliverable.
- [Affects R11][Needs research] Same sketch for flow analysis: does the v1 IR carry enough position/CFG-friendly info, or do we need to plan a separate CFG construction phase up front?
- [Affects R9][Technical] Dump tool format — human-readable text, structured JSON, or both? Affects how golden tests are written.
- [Affects R7][User decision] Should the absence-of-schema mode be the default, or should oxabl warn once that schema-dependent checks are disabled? (Leaning silent-default.)

## Next Steps
→ `/ce:plan` for structured implementation planning. Recommended planning approach: deepen-plan with parallel research agents on (a) IR shape options, (b) `.df` format and parser scope, (c) ABL type/coercion rules, since each is a meaningful unknown.
