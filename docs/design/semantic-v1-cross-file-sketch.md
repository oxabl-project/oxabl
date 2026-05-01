---
title: "Cross-File Resolution Sketch (R10)"
status: draft
date: 2026-04-17
parent: docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md
---

# Cross-File Resolution Sketch (R10)

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
