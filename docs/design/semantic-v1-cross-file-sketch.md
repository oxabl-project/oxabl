---
title: "Cross-File Resolution Sketch (R10)"
status: superseded
date: 2026-04-17
superseded: 2026-07-30
parent: docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md
---

# Cross-File Resolution Sketch (R10)

> **Superseded — this is not the live design.** Cross-file resolution shipped, and it did not ship as the post-hoc side table designed below. The live design of record is the code plus its module docs: the seam is `oxabl_semantic::index` (`WorkspaceIndex` and its four queries), the implementation is the `oxabl_index` crate (`index_file` plus `BatchIndex`), and the two consumers are `oxabl_pipeline` (batch) and `oxabl_lsp` (salsa-backed, per-file inputs). `CLAUDE.md` and `HANDOFF.md` carry the settled decisions.
>
> **Read it for what it got right, because that part is load-bearing.** The central claim — R10, that cross-file resolution is reachable without reshaping the per-file model's public fields — **held.** `Semantic` gained two additive fields (`index_revision`, and `AnalysisContext`'s `index`/`index_loaded`); `scope_tree`, `symbols`, `references` and `types` kept their shapes, `Resolution` kept `Resolved(SymbolId)`, and no lint rule signature changed. The reasoning below is what produced that outcome and is worth keeping in the record.
>
> **What differs is the mechanism, and the difference is the whole point.** This sketch computes a `CrossFileResolutions` side table *after* every per-file `Semantic` already exists, then reads it back through an `effective_resolution` wrapper. What shipped is an **index consulted during resolve**: the resolve pass asks `WorkspaceIndex` at the moment a name fails locally, and writes the answer straight into the ordinary `references` and `symbols` tables — a cross-file hit is a `Resolution::Resolved(SymbolId)` against a symbol the index synthesized, indistinguishable in shape from a local one. Three reasons the post-hoc table was not it:
>
> 1. **It requires every file's `Semantic` to exist first.** The editor's per-keystroke path analyzes exactly one buffer; a whole-workspace pre-pass on each edit cannot meet the interactivity budget, and there is no `Semantic` for a dependency the run never asked about. The index instead answers *four narrow questions* about a file and extracts its facts lazily, on first ask, memoized per run.
> 2. **Every consumer would have to learn a second lookup.** `effective_resolution(sem, xfr, file, node)` is a call each rule, the analyze dump, and every future consumer must remember to use instead of reading `references` — and forgetting it silently degrades to single-file behavior. Resolving during the pass means there is no second path to forget.
> 3. **A wrapper cannot say why a miss missed.** The variant vocabulary turned out to matter more than the plumbing: `External` ("we did not look" — no index attached) is genuinely different from `NotFoundInWorkspace` (searched the configured paths; absent) and from `Unknowable` (not statically knowable). That distinction lives on `UnresolvedReason` in the per-file model, which a post-hoc overlay is the wrong place to compute.
>
> **The two pinned tests named below still exist and still pass** — `resolve_new_class_unknown_is_external` (`crates/oxabl_semantic/src/resolve.rs`) and `cross_file_class_assignment_silent` (`crates/oxabl_lint/src/rules/type_mismatch_assignment.rs`) — so this file is live documentation of a real invariant, not a dead artifact. One refinement: the invariant they pin is now "a cross-file name is never `NotInScope`", not "is always `External`". With no index attached `External` is still the answer; with one attached the same reference lands on `NotFoundInWorkspace` or `Unknowable` instead. Every one of the three is skip-listed by every rule, so the silence the tests assert is unchanged.

## Purpose

This appendix proves that R10 ("cross-file resolution is reachable without an
IR rewrite") remains viable after the v1 ship. The v1
`oxabl_semantic::Semantic` type is designed so cross-file work adds a *side
table* without changing any per-file public type signature.

It is an illustrated commentary on the architecture, not an implementation
plan. Reviewers are invited to stress-test it.

## v1 per-file shape — reviewed for cross-file extensibility

```rust
pub struct Semantic {
    pub scope_tree: ScopeTree,
    pub symbols: SymbolTable,
    pub references: NodeIndexVec<Resolution>,
    pub types: NodeIndexVec<ResolvedType>,
    pub schema_revision: SchemaRevision,
    pub diagnostics: Vec<Diagnostic>,
}

pub enum Resolution {
    Resolved(SymbolId),
    Unresolved { name: OxablAtom, reason: UnresolvedReason },
}

pub enum UnresolvedReason {
    NotInScope,
    External,   // USING, RUN "name", DYNAMIC-FUNCTION, NEW ExternalClass
    NoSchema,
}
```

The contract:
- Every cross-file unresolved reference produces `Unresolved { reason:
  External }`, never `NotInScope`. This is a **stable invariant** of the
  resolve pass and is pinned by tests (`resolve_new_class_unknown_is_external`,
  `cross_file_class_assignment_silent`).
- `SymbolId` is dense within a single `SymbolTable`. Cross-file resolution
  introduces a disambiguating `(FileId, SymbolId)` key; the per-file
  `Resolution::Resolved(SymbolId)` survives unchanged because the ambiguating
  scope only lives in the new workspace-level map.

## Where cross-file fits

```
┌─────────────────────────┐        ┌─────────────────────────────┐
│  Semantic per file      │        │  Workspace-level side table │
│  (v1 shape, unchanged)  │   ──▶  │  CrossFileResolutions       │
└─────────────────────────┘        └─────────────────────────────┘
        ▲                                         │
        │                                         ▼
        └─────── consumers (lint, analyze) ◀─────┘
```

### New type (added post-v1, no v1 changes required)

```rust
pub struct CrossFileResolutions {
    /// For each External reference node in each file, maps to its
    /// cross-file resolution (or leaves it External if still unresolved).
    pub resolved_external: FxHashMap<(FileId, NodeId), CrossFileSymbol>,
}

pub struct CrossFileSymbol {
    pub file: FileId,
    pub symbol: SymbolId,
}

impl Workspace {
    pub fn resolve_cross_file(
        files: &FxHashMap<FileId, Semantic>,
    ) -> CrossFileResolutions {
        // ... walk each file's External references; look them up in
        //     the union of symbol tables; record hits.
        todo!()
    }
}
```

This side table **does not modify** any per-file `Semantic`. Lint rules
consume it via a thin wrapper:

```rust
pub fn effective_resolution<'a>(
    sem: &'a Semantic,
    xfr: &'a CrossFileResolutions,
    file: FileId,
    node: NodeId,
) -> Option<Resolved> {
    match sem.references.get(node) {
        Some(Resolution::Resolved(s)) =>
            Some(Resolved::Local(*s)),
        Some(Resolution::Unresolved { reason: UnresolvedReason::External, .. }) => {
            xfr.resolved_external.get(&(file, node))
                .map(|xf| Resolved::CrossFile(*xf))
        }
        _ => None,
    }
}
```

## Two concrete entry points walked

### 1. `USING pkg.Name` + `NEW Name(...)`

v1 behavior (tested):
- Parser emits `StatementKind::Using { type_name: "pkg.Name" }` at file top.
- Declare pass does not bind USING names locally (intentional — it's a hint for
  the cross-file resolver, not a declaration).
- Resolve pass sees `NEW Name(...)`, fails the local Types lookup,
  records `Unresolved { name: "name", reason: External }` at the NEW expression's
  NodeId.

Cross-file flow (post-v1):
- `CrossFileResolutions::resolve_cross_file` walks every `Using` statement,
  maps each fully-qualified name to its declaring file via a workspace-level
  name index, and for each `Unresolved External` reference whose atom matches
  a USING-imported short name, emits a `CrossFileSymbol`.
- `LINT0001 undefined-symbol` continues to *skip* `External` in v1; post-v1 it
  consults `CrossFileResolutions` first, and only then decides: if still
  External, downgrade to `LINT0001 cross-file-unresolved` (new code) or let it
  slide — that's a product choice, not a representation choice.

### 2. `RUN "other.p"`

v1 behavior (tested):
- `RunTarget::Literal("other.p")` resolves to no symbol locally; resolve pass
  does **not** record a reference entry (there's no NodeId to bind against
  the bare string — RUN's target is a plain `String`, not an `Identifier`).
- This is an acknowledged v1 limitation: the diagnostic surface for undefined
  procedures is zero because there's no NodeId to hang the resolution on.

Cross-file flow (post-v1):
- Promote `RunTarget::Literal` from `String` to `RunTarget::Literal { path:
  String, id: NodeId }` — a non-breaking AST addition (tuple → struct variant).
- Resolve records `Unresolved { name: <path>, reason: External }` at the new
  NodeId.
- `CrossFileResolutions::resolve_cross_file` walks each file's Run nodes,
  tries `path == other_file.path.file_name()`, emits CrossFileSymbol on hit.

## What cross-file does *not* touch

- **Per-file `Semantic` type signature**: unchanged.
- **`NodeIndexVec<Resolution>` side table**: unchanged. No new variants on
  `Resolution`; cross-file lives in its own map.
- **Lint rule signatures**: `run(program, sem, ctx)` remains. The new
  cross-file map lives on `AnalysisContext` as an `Option<&CrossFileResolutions>`
  field that pre-v1 callers can ignore.
- **Tests**: existing v1 tests stay green because the invariants they pin
  (External emitted for cross-file-ish refs, local resolution preferred) are
  exactly what cross-file preserves.

## Breaking-change budget

None required. The migration is additive: a new crate
`oxabl_workspace_semantic` (or equivalent module) exposes
`CrossFileResolutions`, and the existing lint rules opt into consulting it via
a one-line config on `AnalysisContext`.

---

**Reviewers**: stress-test by trying to design a cross-file integration that
*would* require changing `Semantic`'s public fields. If you find one, the v1
shape needs adjustment before ship.

---

## Scored against what shipped

Kept, and correct:

- **R10 held.** No per-file public field was reshaped to make cross-file resolution work.
- **`RunTarget::Literal` was promoted** almost exactly as sketched — tuple variant to struct variant carrying its own `NodeId` — and `StatementKind::Using` got the same treatment. Both also carry a `name_span` the sketch did not anticipate, so a "could not be located" diagnostic underlines the name rather than the statement (see `docs/design/ast-invariants.md` §1 and §2).
- **`Resolution::Resolved(SymbolId)` survived unchanged**, and no new `Resolution` variant was added.

Wrong, or overtaken:

- **The `CrossFileResolutions` side table, `CrossFileSymbol`, `resolve_cross_file`, `effective_resolution`, and the `oxabl_workspace_semantic` crate do not exist.** Nothing named here shipped; see the banner above for the three reasons.
- **"Breaking-change budget: none required" did not survive.** Giving `Using` and `RunTarget::Literal` their own ids, and adding the two new `UnresolvedReason` variants, are breaking changes to public enums — shipped as such, deliberately, rather than worked around.
- **The disambiguating `(FileId, SymbolId)` key was not needed.** A cross-file symbol is synthesized *into the analysed file's own* `SymbolTable`, so one dense `SymbolId` space still covers everything a consumer reads. The origin it came from is recorded as a fact on the row, not as part of its identity.
- **The lint half was deferred, not delivered.** This sketch's "post-v1 LINT0001 consults the map first" is still future work. Attaching an index deliberately adds **no** new diagnostic today; two mechanisms hold that line on purpose, and turning the rules onto the cross-file population is its own follow-up. `CLAUDE.md` and `HANDOFF.md` name both mechanisms — do not remove either as dead weight.
