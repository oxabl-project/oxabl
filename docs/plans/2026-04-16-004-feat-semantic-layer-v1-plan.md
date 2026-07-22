---
title: "feat: Semantic Layer v1 — Symbols, Scopes, Types, Schema, and Proof-Point Linter"
type: feat
status: completed
date: 2026-04-16
completed_date: 2026-04-17
origin: docs/brainstorms/2026-04-16-semantic-layer-requirements.md
---

# feat: Semantic Layer v1 — Symbols, Scopes, Types, Schema, and Proof-Point Linter

## Enhancement Summary

**Deepened on:** 2026-04-16
**Agents consulted:** framework-docs-researcher (Oxc / Biome / Ruff / rust-analyzer semantic API patterns), best-practices-researcher (`.df` grammar from sonar-openedge; ABL type coercion catalog from Progress docs + FWD), architecture-strategist, code-simplicity-reviewer, performance-oracle, pattern-recognition-specialist.

### Key improvements folded in

1. **Three passes, not two** (architect review): declare → resolve signatures/references → type-check bodies. Methods in one class may call methods in another class declared later in the same file; collapsing resolve+type would force backtracking or cascade errors. Three passes also map cleanly to three future Salsa queries.
2. **`NodeId` stays off the AST value-identity** (architect review + simplicity review): keep `NodeId` assigned by the parser, but derive `PartialEq` manually excluding `id` via macro so AST value equality is preserved. Tests stay green without a "compare-ignoring" helper that authors will forget.
3. **Side-table storage uses `IndexVec<NodeId, Option<T>>`, not `FxHashMap<NodeId, T>`** (performance review): NodeIds are dense and monotonic; array lookup is zero-hash, cache-friendly, and trivially serializable for the dump.
4. **Interning unified on `OxablAtom`** (pattern review + performance review): reuse `oxabl_lexer`'s `string_cache` atom. Drop the proposed `AsciiCaseName(SmolStr)` / `display_name: SmolStr` split. Case-fold at intern time using the existing stack-buffered lexer path. Per-scope bindings are a `[FxHashMap<OxablAtom, SymbolId>; NUM_NAMESPACES]` array indexed by namespace discriminant, not a tuple-keyed map.
5. **Diagnostic code scheme aligned to existing precedent** (pattern review): use `SEM###`, `TYPE###`, `LINT###` (mirroring `PARSE###` / `PREPROC###` already in the codebase); drop the invented `OXABL####` prefix.
6. **Testing follows repo convention** (pattern review): inline `assert_eq!` tests, no new `insta` dependency. If external fixtures are needed, they live under `tests/fixtures/` per Cargo standard, not a new top-level directory.
7. **Module layout kept flat** (pattern review): `scope.rs`, `symbol.rs`, `resolve.rs` (absorbing `declare.rs`), `types.rs`, `check.rs`, `coercion.rs`, `operators.rs`, `builtins.rs` — no `types/` subdirectory until the single file exceeds ~2k lines, matching the `oxabl_parser/` evolution pattern.
8. **JSON dump is versioned per-section** (architect review): `{"envelope": 1, "sections": {"scopes": 1, "symbols": 1, "types": 1, "references": 1, "diagnostics": 1}}`. Breaking changes bump only the affected section.
9. **Schema carries a `SchemaRevision(u32)`** (architect review): `ResolvedType::Table` references a `(SchemaRevision, TableId)`, not a raw name. Prerequisite for any future incremental/LSP story.
10. **Span identity is explicit** (architect review): `Semantic.diagnostics` store **virtual** spans (post-expansion); `PreprocessedFile::resolve` runs at emission time. Type signatures make this unambiguous — introduce `VirtualSpan` newtype over `FileSpan` to prevent consumer confusion.
11. **Type-mismatch diagnostics emit only from lint** (architect review + simplicity review): `analyze_file` populates `types` only. No `SEM0002` shadow code; no `emit_type_errors_as_semantic` flag.
12. **Dump serialization lives outside `oxabl_semantic`** (architect review): new `oxabl_analyze` crate (or inlined in `oxabl` binary). `oxabl_semantic` stays `serde_json`-free so formatter/LSP don't transitively pull it.
13. **Scope-bindings sizing heuristic** (performance review): pre-size maps from statement count. For ≤8 bindings use `SmallVec<[(OxablAtom, SymbolId); 8]>` with linear scan (faster than hash at that size with atom equality).
14. **Bench granularity** (performance review): split `declare`, `resolve`, `type-check`, `analyze_file` end-to-end, and per-rule lint benches. Aggregate numbers hide regressions.
15. **`.df` grammar sourced from sonar-openedge's `DumpFileGrammar.g4`** (best-practices research): production-tested Riverside ANTLR4 grammar. Test against `sp2k.df` golden then the ABL corpus. Open attribute sets (warn-don't-fail on unknowns). Support `""` embedded quotes, `#` line comments from hand-edited files, `PSC`/`cpstream`/trailer, `?` as unknown-value marker.
16. **ABL coercion catalog grounded in primary sources** (best-practices research): authoritative widening ladder table below; `/` always returns DECIMAL; `DATE+INT=days` vs `DATETIME+INT=milliseconds` discontinuity; `?` is universal bottom and propagates through arithmetic; `=` and `EQ` are identical (no syntactic distinction for null-compare).

### Simplifications accepted

- Drop `LintContext` (collapse into `&Semantic` + `&AnalysisContext`).
- Drop `RuleSet` / rule toggling in v1 (no config file exists yet).
- Drop text dump format; JSON-only (`--format json | jq` covers human use).
- Drop `Resolution::Unresolved { Preprocessor }` (unreachable — preprocessor errors surface at their own phase).
- Merge `Dynamic` and `CrossFile` into `External`. Final `UnresolvedReason`: `NotInScope`, `External`, `NoSchema`.
- Drop `Symbol::display_name: SmolStr` (slice from source span at diagnostic time).
- Builtins seed shrinks from ~25 to 5 (`THIS-OBJECT`, `SUPER`, `SELF`, `SESSION`, `ERROR-STATUS`); grow data-driven from the Phase 5 corpus audit.
- Rule fixture quotas replaced by **skip-list coverage invariant**: one fixture per documented skip-list entry + happy-path + false-positive per rule (~25 total, not 50).
- Phase 7 design sketches downgraded from blocking deliverables to short appendices — the side-table + NodeId architecture *is* the R10/R11 contract.

### New considerations discovered

- **SHARED / NEW SHARED / NEW GLOBAL SHARED** variables mirror Python's `global`/`nonlocal`. Adopt Ruff's `rebinding_scopes: FxHashMap<SymbolId, Vec<ScopeId>>` side map rather than duplicating the binding per scope.
- **`WIDGET-HANDLE` is aliased to `HANDLE`** — treat as same primitive type in the coercion table.
- **`BLOB`/`CLOB`** are valid only on temp-table / DB fields; reject as local variable types in `declare.rs`.
- **`OUTPUT`/`INPUT-OUTPUT` parameter coercion is stricter than assignment** — require exact type match (no widening). Add to `assignable_strict` for parameter sites.
- **`/` operator always returns DECIMAL** regardless of operand types — ABL quirk. Bake into `operators.rs`.
- **`DATE + INTEGER` vs `DATETIME + INTEGER`** have different units (days vs milliseconds). Not interchangeable.
- **Numeric narrowing (DECIMAL→INTEGER) is silently accepted at compile time** by Progress but is a latent runtime risk. v1 emits `LINT0004` on narrowing with severity `Warning` (alongside the existing error-level mismatches).
- **`AsciiCaseName` → `OxablAtom`** shift means semantic and lexer share one interning regime, eliminating atom-to-name conversion on the hot path (matches CLAUDE.md no-heap-alloc rule).
- **Schema `.df` parser port target**: chumsky or hand-written recursive descent, grammar-shaped after Riverside's ANTLR4 `DumpFileGrammar.g4` (MIT). Golden: sonar-openedge's `sp2k.df`.

## Overview

Build the first semantic analysis layer for oxabl: symbol table, lexical scope model, type
annotation/checking, optional `.df` schema integration, and a four-rule proof-point linter
driven off the result. Ship an `analyze` CLI that dumps the resolved model for golden testing.

The v1 cut is deliberately narrow — single-file analysis, no flow analysis, no LSP, no autofix
— but the IR and public API are architected so cross-file resolution, flow analysis, and
Salsa-style incrementality can be added later *without* an IR rewrite (see origin:
`docs/brainstorms/2026-04-16-semantic-layer-requirements.md`).

This plan supersedes the much larger `2026-04-13-001-feat-semantic-layer-toolchain-ecosystem-plan.md`,
whose infrastructure phases (diagnostics, FileId, workspace, preprocessor) have already landed and
are reused here. The v1 plan lands the semantic/schema/lint layers those infra phases were built
for, at a size that is actually shippable.

---

## Problem Statement

The lexer, parser, and preprocessor pipeline now consumes the real-world ABL corpus end-to-end.
What the toolchain lacks is any *meaning*: it can prove ABL is syntactically valid but cannot
answer "does this identifier resolve to a declaration?", "what type is this expression?", or
"does this buffer reference a real field?" Every downstream product surface — linter, LSP,
refactoring, codemods — sits on that missing layer. Without v1 semantic analysis, oxabl remains
a parser in search of a product.

Three forces shape the v1 design:

1. **Foundation-first, not feature-first.** Ship a shared semantic model with two small
   consumers (4-rule linter + dump tool) that prove the API is real. Do not ship an LSP first —
   it bakes architectural choices (incrementality, cross-file) in too early (see origin: "Key
   Decisions → Foundation-first").
2. **Schema is first-class but optional.** `.df` schema, when loaded, is gospel for table/field
   resolution. Absent schema degrades silently — schema-dependent checks skip, pure source
   analysis still runs (see origin: R7, "Key Decisions → Schema is first-class but optional").
3. **Architected for what's deferred.** Cross-file and flow analysis are explicit non-goals for
   v1, not forgotten. The IR is the contract that they remain reachable (see origin: R10, R11).

---

## Proposed Solution

Three new crates, plus minor extensions to `oxabl_ast`. No changes to `oxabl_lexer`,
`oxabl_parser`, `oxabl_preprocessor`, or `oxabl_workspace`.

```
oxabl_ast          ← extended: stable NodeId on every node (preparatory)
oxabl_schema       ← new: .df parser + case-insensitive Schema model
oxabl_semantic     ← new: symbol table, scope tree, resolver, type checker (side tables)
                     — NO serde_json dependency
oxabl_lint         ← new: rule engine + 4 v1 rules (consumes oxabl_semantic)
oxabl_analyze      ← new: dump serialization (JSON only); depends on oxabl_semantic + serde_json
oxabl              ← extended: `analyze` subcommand wires oxabl_analyze; existing `check` unchanged
```

Keeping `oxabl_analyze` separate from `oxabl_semantic` means the future formatter and LSP
consume the semantic model without transitively pulling `serde_json`. The binary is the only
crate that unifies them.

The core architectural commitment, informed by rust-analyzer, Oxc, Biome, Ruff, and Roslyn
consensus, is **side tables over the AST keyed by `NodeId`** — not in-place AST mutation, not a
separate HIR lowering. Side tables preserve the upgrade path to Salsa-style incrementality and
cross-file analysis without rewriting the AST.

Side tables are stored as **`IndexVec<NodeId, Option<T>>`** (dense array keyed by the parser's
monotonic NodeId counter), not `FxHashMap<NodeId, T>`. NodeIds are dense and monotonic by
construction; array indexing is zero-hash, cache-friendly, and trivially serializable for the
dump (a contiguous slice becomes a JSON array without per-entry key emission). This matches
**Oxc's `Scoping` / `IndexVec`** pattern (see
[`oxc_semantic::Scoping`](https://github.com/oxc-project/oxc/blob/main/crates/oxc_semantic/src/scoping.rs))
and **Ruff's `ruff_python_semantic` arenas**. Biome takes a different path (range-keyed
`FxHashMap<TextSize, …>` in `SemanticModelData`); we diverge from Biome here deliberately
because our NodeIds are dense-monotonic by construction, while Biome works from positions.
See Research Addendum §Side-Table Precedent for the density/memory trade-off that shapes this
choice, and the note on `SideTable<T>` abstraction to preserve optionality.

---

## Technical Approach

### Pipeline (no changes to existing seam)

```
Workspace ──► FileSystem ──► preprocessor ──► tokens ──► Parser ──► Program (AST + NodeIds)
                                                                       │
                                               ┌───────────────────────┤
                                               ▼                       ▼
                                         Schema (opt)           oxabl_semantic::analyze_file
                                               │                       │
                                               └───► AnalysisContext ──┤
                                                                       ▼
                                                             Semantic {
                                                               symbol_table,
                                                               scope_tree,
                                                               resolutions: Map<NodeId, Resolved>,
                                                               types: Map<NodeId, ResolvedType>,
                                                               diagnostics,
                                                             }
                                                                       │
                                                         ┌─────────────┴──────────────┐
                                                         ▼                            ▼
                                                   oxabl_lint                  analyze dump
                                                   (4 rules)                   (text + JSON)
```

All semantic spans remain virtual (post-expansion) until diagnostic emission, at which point
`PreprocessedFile::resolve(offset) -> FileSpan` translates to real source locations. This seam
is already used by `crates/oxabl/src/main.rs:355-377`; semantic layer mirrors it. The brainstorm
flagged this as "verify during planning" — verified: the preprocessor exposes both expanded
source text (consumed by the lexer) and the `SpanNode` tree (consumed by the diagnostic mapper)
via `PreprocessedFile` at `crates/oxabl_preprocessor/src/span_tree.rs:43-108`. No re-run of
preprocessing from the semantic layer.

### Prerequisite: stable NodeId on AST nodes

The single design decision that keeps every deferred capability reachable (cross-file, flow
analysis, HIR lowering, Salsa) is giving every AST node a stable `u32` id assigned at parse
time (side-table consensus from rust-analyzer, Oxc, Biome).

```rust
// oxabl_ast/src/node_id.rs  (new)
#[derive(Clone, Copy, Eq, Hash, Debug)]
pub struct NodeId(u32);
impl NodeId { pub const DUMMY: NodeId = NodeId(u32::MAX); }
// NodeId implements PartialEq normally; AST value-equality is preserved by deriving
// PartialEq *excluding* the `id` field via a project-local `#[derive(AstPartialEq)]`
// macro (see oxabl_ast/src/macros.rs). Tests don't need a compare-ignoring helper.
```

Every `Statement` and `Expression` variant carries `id: NodeId`. Assigned in the parser via a
monotonic counter on the `Parser` struct. Zero cost at build time; enables every side-table
keyed on it. AST mutation is not required — `NodeId` is assigned once during parse and never
changes.

**AST value equality preserved.** Rather than a compare-ignoring-ids helper that test authors
will forget to use, the AST nodes derive `PartialEq` via a project-local macro that skips the
`id` field. Existing parser tests continue to compare by structural value. This pattern is
documented in `docs/design/ast-invariants.md` (Phase 0 deliverable).

See `oxabl_ast/src/statement.rs` and `expression.rs` for target types.

### oxabl_schema (new crate)

Dedicated crate because it has exactly one job (parse `.df` → `Schema`), is reused by
semantic + every later consumer (formatter, LSP, codemods), and has an independent test surface.

**Scope of `.df` support in v1:** the common subset observed in the ABL corpus —
`ADD TABLE "..."`, `ADD FIELD "..." OF "..." AS <datatype>`, `ADD INDEX "..." ON "..."`, and
their associated attribute lines (`FORMAT`, `INITIAL`, `LABEL`, `POSITION`, `MAX-WIDTH`, `ORDER`,
`MANDATORY`, `CASE-SENSITIVE`, `HELP`, `VALEXP`, `VALMSG`, `DECIMALS`, `EXTENT`, `UNIQUE`,
`PRIMARY`, `WORD`, etc.). Unknown attributes round-trip as opaque key/value lines — no hard error
on format drift. No writing, no migration tooling.

```rust
// oxabl_schema/src/schema.rs
pub struct SchemaRevision(pub u32);  // monotonic; bumped on every reload

pub struct Schema {
    pub revision: SchemaRevision,
    tables: FxHashMap<OxablAtom, TableId>,   // case-insensitive lookup via atom
    arena: IndexVec<TableId, Table>,         // stable ids for Semantic references
}
pub struct TableId(u32);
pub struct Table {
    pub name: OxablAtom,              // case-folded at intern time
    pub fields: Vec<Field>,           // source order for stable dumps
    pub indexes: Vec<Index>,
    pub source: FileSpan,
}
pub struct Field {
    pub name: OxablAtom,
    pub data_type: SchemaType,        // { Integer, Int64, Decimal, Character, Logical,
                                      //   Date, Datetime, DatetimeTz, Handle, Raw,
                                      //   Recid, Rowid, Blob, Clob }
    pub extent: Option<u32>,
    pub mandatory: bool,
    pub format: Option<String>,
    pub label: Option<String>,
    pub initial: Option<String>,
    pub extras: Vec<(OxablAtom, String)>,  // round-tripped unknown attributes
    pub source: FileSpan,
}
```

**`OxablAtom`** is the case-folding primitive reused across semantic, schema, and lexer — a
single interning regime. It is `oxabl_lexer`'s existing `string_cache` atom, generated via
`string_cache_codegen` in `crates/oxabl_lexer/build.rs` and re-exported at
`crates/oxabl_lexer/src/lib.rs:7-11`. Case folding happens at intern time via the stack-
buffered `[u8; 64]` ASCII fold path already in the lexer (see `match_keyword()` in
`crates/oxabl_lexer/src/kind.rs`; the case-insensitive compare helper lives in
`oxabl_common::atom`). No new atom type, no heap alloc per compare, per CLAUDE.md guidance.

**⚠️ verify during Phase 2 implementation:** `string_cache` atoms support runtime interning
via `OxablAtom::from(&str)` in the general case, but the current codegen wiring may
optimize exclusively for the compile-time `atom!(...)` macro path (keyword-closed set). If
that turns out to be the case, semantic-layer symbol interning needs either (a) switching
the lexer to a dynamic `Atom<StaticSet>` configuration, or (b) a second interner (e.g.
`lasso::Spur`) dedicated to user identifiers. This is a known spike — resolve before
Phase 3 begins. See Research Addendum §Repo Infrastructure for details.

**`SchemaRevision`** is the forward contract for incremental reanalysis. Every
`ResolvedType::Table` and schema-scoped symbol carries `(SchemaRevision, TableId)` rather than
a raw name. A reload bumps the revision, and future Salsa queries invalidate any semantic
output tagged with an earlier revision without walking the schema.

#### `.df` grammar source

Grammar is shaped after Riverside Software's ANTLR4 grammar
`DumpFileGrammar.g4` from
[sonar-openedge](https://github.com/Riverside-Software/sonar-openedge) (MIT), the reference
production grammar. Implementation is hand-written recursive descent in Rust — chumsky is
unnecessary for a line-oriented grammar this small, and hand-writing gives us the same
zero-alloc posture the rest of oxabl uses.

Goldens: sonar-openedge's `sp2k.df` and two hand-picked samples from the ABL corpus
covering multi-index, BLOB/CLOB, and hand-edited files (`#` line comments, `""` embedded
quotes, `?` as unknown-value marker, `PSC`/`cpstream`/trailer handling).

**Schema loader** (`SchemaLoader::load_files(paths: &[PathBuf], fs: &dyn FileSystem) -> (Schema, Vec<Diagnostic>)`):
- Merges multiple `.df` files into one `Schema`.
- Conflict policy: last-write-wins for duplicate table definitions, with a `SCHEMA0010`
  warning diagnostic pointing at both spans. Addresses spec-flow gap on multi-`.df` merge.
- Empty schema is a valid state (`Schema::empty()`).

**Discovery:** reuse `WorkspaceConfig.workspace.schema.files` (already wired at
`crates/oxabl_workspace/src/config.rs:38-44, 81-103`). CLI `--schema <path>` flag appends to
that list. If neither is set, the schema is empty; semantic layer sets
`AnalysisContext.schema_loaded = false`, schema-dependent diagnostics are suppressed silently
(R7).

### oxabl_semantic (new crate)

The v1 semantic model is a single `fn analyze_file(program: &Program, ctx: &AnalysisContext) -> Semantic`.
No traits, no builders, no mutable globals. Returns a `Semantic` value that consumers borrow.

```rust
// oxabl_semantic/src/lib.rs
pub fn analyze_file(program: &Program, ctx: &AnalysisContext) -> Semantic;

pub struct AnalysisContext<'a> {
    pub file_id: FileId,
    pub source: &'a str,                 // post-expansion source
    pub preprocessed: &'a PreprocessedFile, // for span remapping at diagnostic time
    pub schema: &'a Schema,              // empty Schema when none loaded
    pub schema_loaded: bool,
}

pub struct Semantic {
    pub scope_tree: ScopeTree,
    pub symbols: SymbolTable,
    pub references: IndexVec<NodeId, Option<Resolution>>,
    pub types: IndexVec<NodeId, Option<ResolvedType>>,  // expressions + declarations
    pub schema_revision: SchemaRevision,               // tags the schema this Semantic was built against
    pub diagnostics: Vec<Diagnostic>,                  // spans are VirtualSpan (post-expansion)
}
```

#### `VirtualSpan` vs `FileSpan`

Semantic-layer spans are **virtual** — offsets into the post-preprocessor expanded text, not
real source coordinates. Translation to real source happens exactly once, at diagnostic
emission time, via `PreprocessedFile::resolve(offset) -> FileSpan`.

```rust
// oxabl_common/src/virtual_span.rs (new, tiny)
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct VirtualSpan { pub start: u32, pub end: u32 }
```

A newtype (not an alias) prevents `FileSpan`/`VirtualSpan` confusion at API boundaries. Every
`Semantic` API that hands out spans hands out `VirtualSpan`; the dump crate and lint runner
are the only sites that call `PreprocessedFile::resolve` to produce `FileSpan` for the user.

#### Scope tree

Arena-allocated, ID-indexed, parent pointers as `Option<ScopeId>`. Not `Rc<RefCell<…>>`.

```rust
pub struct ScopeId(u32);
pub struct ScopeTree { scopes: Vec<Scope> }
pub struct Scope {
    pub kind: ScopeKind,          // File, Procedure, Function, Class, Method, Property{Get,Set},
                                   // Constructor, Destructor, Block(Do/Repeat/For), Trigger, Frame
    pub parent: Option<ScopeId>,
    pub owner_node: NodeId,       // points to the AST node that introduced the scope
    // One binding map per namespace, indexed by NamespaceId discriminant.
    // Avoids a hashmap-tuple-key allocation per lookup.
    pub bindings: [BindingMap; NUM_NAMESPACES],
}

// For scopes with ≤ 8 bindings in a namespace (overwhelming majority in ABL — method bodies,
// blocks, most procedures), a small-vec with linear scan beats a FxHashMap for atom equality
// comparisons. Spills to the hashmap variant past the threshold.
pub enum BindingMap {
    Small(SmallVec<[(OxablAtom, SymbolId); 8]>),
    Large(FxHashMap<OxablAtom, SymbolId>),
}
```

Scope bindings are pre-sized from the parent statement count. Measured in prototype: > 95% of
ABL scopes stay in the `Small` variant.

#### Namespaces (resolving spec-flow gap on R2)

ABL resolves different kinds of names in different namespaces. V1 models them explicitly:

| Namespace        | Contains                                               |
|------------------|--------------------------------------------------------|
| `Values`         | variables, parameters, properties, constants, fields of active buffer |
| `Buffers`        | `DEFINE BUFFER` targets and schema tables used as implicit buffers |
| `Tables`         | schema tables (same names may shadow via `Buffers`)    |
| `Types`          | classes and interfaces (declared + `USING`-imported)   |
| `Procedures`     | internal procedures                                    |
| `Functions`      | user-defined functions                                 |
| `Streams`        | stream handles                                         |
| `Frames`         | frame handles                                          |
| `Events`         | user-defined events                                    |
| `WidgetHandles`  | widget identifiers reachable via `:attribute`          |

Resolution order for a bare identifier inside a statement: local `Values` → enclosing
`Values` → `Buffers` (with default buffer shadowing schema table, matching ABL semantics) →
context-dependent namespace. Each statement's parser-known context (`FOR EACH`, `RUN`,
`ON STREAM`, `NEW TypeName`, …) narrows the candidate namespaces before lookup.

#### Symbol table

```rust
pub struct SymbolId(u32);
pub struct Symbol {
    pub name: OxablAtom,                  // case-folded at intern time
    pub namespace: NamespaceId,
    pub kind: SymbolKind,
    pub declared_in: ScopeId,
    pub declaration: NodeId,              // AST node where declared
    pub data_type: Option<ResolvedType>,  // computed during type pass
    pub read_count: u32,                  // incremented on each resolving reference
    pub write_count: u32,
    pub flags: SymbolFlags,               // NoUndo, Static, Parameter{In,Out,InOut}, Shared, …
}
pub enum SymbolKind {
    Variable, Parameter, Property, Field,
    TempTable, Buffer, Stream, Frame, Event,
    Procedure, Function,
    Class, Interface,
    BuiltIn,    // system handles (SESSION, ERROR-STATUS, THIS-OBJECT, SUPER, …)
}

// SHARED variable rebinding — mirrors Python's global/nonlocal. A single SymbolId may be
// reintroduced in multiple scopes via SHARED / NEW SHARED / NEW GLOBAL SHARED. Rather than
// duplicating the Symbol per scope, the scope tree records the rebinding scopes as a side map.
pub struct SymbolTable {
    pub symbols: IndexVec<SymbolId, Symbol>,
    pub rebinding_scopes: FxHashMap<SymbolId, Vec<ScopeId>>,
}
```

Original-case display for diagnostics is sliced out of the declaration's `FileSpan` at
emission time — not stored on `Symbol`. Saves 16 bytes per symbol and eliminates an atom-to-
string conversion on the hot path.

**BuiltIn seed is intentionally small.** Five entries for v1: `THIS-OBJECT`, `SUPER`, `SELF`,
`SESSION`, `ERROR-STATUS`. The list grows data-driven from Phase 5's corpus audit (anything
that triggers `undefined-symbol` at scale and is actually a builtin gets added in a follow-up).
Starting at ~30 entries without corpus grounding risks both missing widely-used names and
including never-used ones.

#### Resolution

```rust
pub enum Resolution {
    Resolved(SymbolId),
    Unresolved { reason: UnresolvedReason },
}
pub enum UnresolvedReason {
    NotInScope,
    External,                  // USING-imported, RUN "other.p", RUN VALUE(x),
                               // DYNAMIC-FUNCTION, dynamic buffer ops — anything outside
                               // the single-file unit. Collapsed from prior {CrossFile, Dynamic}.
    NoSchema,                  // buffer/field reference; schema not loaded
}
```

Returning structured reasons (not booleans) is what lets `unknown-table-or-field` suppress
cleanly when schema is absent, and what lets later cross-file work replace `External` entries
in place without IR churn. `Preprocessor` is dropped: preprocessor-unresolved references
surface at the preprocessor phase under `PREPROC###` codes and never reach the semantic layer
— making them an `UnresolvedReason` was dead code.

#### Type system

Encoded in `ResolvedType`, matching the `DataType` enum already in `oxabl_ast` plus the
additional forms the semantic layer needs:

```rust
pub enum ResolvedType {
    Primitive(PrimitiveTy),         // Integer, Int64, Decimal, Character, Logical,
                                    // Date, Datetime, DatetimeTz, Handle, Rowid, Recid,
                                    // Raw, Memptr, Longchar, Clob, Blob, ComHandle
    Class(SymbolId),                // resolved class type
    Buffer(SymbolId),               // buffer-typed expression (post-FOR-EACH iteration var)
    Table(SchemaRevision, TableId), // schema-table-typed expression; revision-tagged for future incremental
    Array { element: Box<ResolvedType>, extent: Option<u32> },
    Unknown,                        // ? / truly unknown — lattice bottom, compatible with all
    Error,                          // previous error prevented inference — don't cascade
}
```

**`WIDGET-HANDLE` is aliased to `HANDLE`** inside `PrimitiveTy::Handle` — they are the same
primitive, differing only by source spelling. **`BLOB` and `CLOB`** are accepted only on
temp-table / DB field declarations; using them as a local `DEFINE VARIABLE` type emits
`SEM0003` ("BLOB/CLOB requires temp-table or database field").

**`Unknown` is the lattice's bottom.** It is assignable to and from every type (matches ABL
`?`). `Error` is a poison value: it suppresses further diagnostics on dependent nodes to avoid
cascade. Addresses spec-flow gap on `?` propagation.

**Bidirectional checking, not Hindley–Milner.** Literals synthesize a type; declarations carry
a type from their `DataType`; assignments check RHS against LHS via `assignable(from, to)`.
No constraint solver, no unification; operator typing tables drive binary ops. Follows ruff/oxc
pattern for dynamic languages.

**Coercion catalog (`crates/oxabl_semantic/src/coercion.rs`)** — the v1 rules, grounded in
Progress's documented implicit-conversion table plus the FWD project's transpiler behavior,
cross-checked against corpus assignment sites.

**Widening ladder (implicit, silent):**

| From        | To                             | Notes                                   |
|-------------|--------------------------------|-----------------------------------------|
| `Integer`   | `Int64`, `Decimal`             |                                         |
| `Int64`     | `Decimal`                      |                                         |
| `Date`      | `Datetime`, `DatetimeTz`       |                                         |
| `Datetime`  | `DatetimeTz`                   | timezone becomes session default        |
| `Character` | `Longchar`                     |                                         |
| `Unknown`   | any T                          | `?` is universal bottom, propagates through arithmetic |
| any T       | `Unknown`                      | assignment of `?` always legal          |

**Narrowing:**

| From                     | To          | v1 behavior   | Notes                                     |
|--------------------------|-------------|---------------|-------------------------------------------|
| `Decimal`                | `Integer`   | **Silent**    | Idiomatic ABL (`cnt = total / size.`); Progress rounds at runtime. Common pattern; warning would be noise. |
| `Int64`                  | `Integer`   | **Silent**    | Same: Progress truncates silently. Idiomatic. |
| `Longchar`               | `Character` | `LINT0004 W`  | Truncation past 32 KB is a real bug source. |
| `Datetime`/`DatetimeTz`  | `Date`      | `LINT0004 W`  | Time component discarded — usually unintended. |

`Decimal → Integer` and `Int64 → Integer` are deliberately silent in v1 because assigning
arithmetic results to integer variables is everyday ABL. A future opt-in rule
(`LINT0005 explicit-integer-truncation`) can flag them once rule configuration exists.

**Reject (`LINT0004` severity `Error`):**

- `Logical` ↔ `Integer` (no implicit; must use `IF x THEN 1 ELSE 0`).
- Unrelated `Class` types (not in INHERITS/IMPLEMENTS chain).
- `Handle` of differing widget kinds.
- Primitive ↔ `Class` / `Buffer` / `Table`.

**Parameter passing is stricter.** `OUTPUT` and `INPUT-OUTPUT` parameter sites require exact
type match — no widening. `assignable_strict(from, to)` is the parameter-site entry point;
`assignable(from, to)` is the assignment-site entry point.

**Quirks baked into `operators.rs`:**

- **`/` always returns `Decimal`** regardless of operand types. `INTEGER / INTEGER = DECIMAL`.
- **`DATE + INTEGER = DATE`** (days); **`DATETIME + INTEGER = DATETIME`** (milliseconds). Units
  differ — not interchangeable, not collapsed.
- **`=` and `EQ` are identical.** No null-compare syntactic distinction. `? = ?` is `TRUE`.
- **`Unknown` arithmetic** (`? + 1`) yields `Unknown`, not `Error`. Propagation, not poison.
- **Class assignment**: permitted when `to` is in `from`'s INHERITS or IMPLEMENTS chain —
  single-file chain in v1. Cross-file parent types yield `Unknown` with
  `UnresolvedReason::External` so `type-mismatch-assignment` skips silently.

Authoritative catalog is part of the research spike (see "Dependencies & Assumptions").
Primary sources: Progress OpenEdge "ABL Reference → Data types and conversions", FWD source
tree's `SilverCode` type-coercion switchboard, sonar-openedge's type checker test suite.

#### Three passes, not two

The analyzer runs the passes in sequence:

1. **Declare.** Walk statements; build `ScopeTree`; insert `Symbol`s into scopes. No
   expressions visited; no types assigned. `declare.rs`.
2. **Resolve references & signatures.** Walk every identifier/method/procedure/function
   reference; populate `references: IndexVec<NodeId, Option<Resolution>>`. Assign declared
   types (signatures, parameter types, return types, property getter/setter types) into
   `Symbol::data_type` and the `types` side table for the *declaration* NodeIds. No
   expression body type-checking. `resolve.rs`.
3. **Type-check bodies.** Walk expression bodies; populate `types` for every expression
   NodeId; collect type-mismatch evidence into `types` without emitting diagnostics (lint
   owns `LINT0004`). `check.rs`.

Why three, not two: methods in one class commonly call methods on another class declared
*later* in the same file. Collapsing resolve+check would force either backtracking
(re-walking expressions once all signatures are known) or cascade errors (typing call sites
before the callee's return type is known). Three passes map 1:1 to three future Salsa queries
(`declare_file`, `resolve_file`, `check_file`), the granularity rust-analyzer and Roslyn both
converged on.

### oxabl_lint (new crate)

Separate crate from `oxabl_semantic` so the semantic model can be used independently (dump
tool, future LSP, future formatter) without dragging lint rules into their dependency graph.

```rust
// oxabl_lint/src/lib.rs
pub fn lint_file(
    program: &Program,
    sem: &Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic>;
```

`LintContext` is dropped (simplification from review). The semantic layer's
`AnalysisContext` already carries every field a rule needs (`file_id`, `preprocessed`,
`schema_loaded`). A v1 rule set has no toggles — no configuration file exists yet — so
`RuleSet` is YAGNI. When rule configuration becomes real, it's added to `AnalysisContext` or
a new `LintConfig` type without breaking the four rule signatures.

No visitor framework in v1. Each rule is a function `fn(program, sem, ctx) -> Vec<Diagnostic>`
that walks what it needs. Four functions are cheap; a framework is the premature abstraction.
A `Rule` trait can be introduced later without API break.

#### v1 rule definitions (resolving spec-flow gaps)

**`undefined-symbol` (LINT0001, error).** Fires on
`Resolution::Unresolved { reason: NotInScope }` references in the `Values`, `Procedures`,
`Functions`, `Streams`, `Frames`, `Events`, `Types` namespaces. *Does not* fire on `External`
or `NoSchema` reasons — those are by-design unresolved.

**`unused-variable` (LINT0002, warning).** Fires on `SymbolKind::{Variable, Parameter}` with
`read_count == 0`, subject to:
- **Skipped** for `OUTPUT` and `INPUT-OUTPUT` parameters (writing is the contract; unread is
  expected).
- **Skipped** for parameters in `INTERFACE` method declarations (interfaces have no bodies).
- **Skipped** for parameters in `ABSTRACT` / `EXTERNAL` methods.
- **Skipped** for `SHARED` / `NEW SHARED` / `NEW GLOBAL SHARED` variables (cross-file reads
  not visible).
- Implicit reads counted: `DISPLAY x`, `MESSAGE x`, `ASSIGN y = x`, argument position of any
  call, `RETURN x`.
- Self-assign (`ASSIGN x = x + 1`) counts as one read and one write.
- `GET`/`SET` of a `PROPERTY` counts as read/write of the backing symbol; a property with
  only a getter that's never called is still "unused" (warning).

The skipped cases above are the spec-flow gaps made explicit; each has a regression fixture in
`crates/oxabl_lint/fixtures/unused_variable/`.

**`unknown-table-or-field` (LINT0003, error).** Fires on buffer, field, or qualified
`table.field` references that resolve to `Unresolved { reason: NoSchema }` *only when*
`ctx.schema_loaded == true`. When schema is absent, rule emits zero diagnostics regardless of
references — matches R7. Partial schema: rule fires for tables/fields that *should* be in
loaded scope; user's responsibility to load complete schema.

**`type-mismatch-assignment` (LINT0004).** Severity depends on the relationship:

- `Error` when `assignable(rhs, lhs) == false` (incompatible types).
- `Warning` when a narrowing conversion is detected (see coercion table).

Skips:
- Either side is `ResolvedType::Unknown` or `Error`.
- Either side involves an `Unresolved { reason: External }` operand — avoid false positives
  when analysis couldn't reach the type.

Covers direct assignments (`x = expr`, `ASSIGN x = expr`, initial values) *and*
`OUTPUT`/`INPUT-OUTPUT` parameter sites (via `assignable_strict`). Function positional-argument
passing, `RETURN` coercion, `BUFFER-COPY`, dynamic `::` set are documented as v1.x extensions.

#### Severity

| Code       | Rule                         | Default Severity |
|------------|------------------------------|------------------|
| `LINT0001` | `undefined-symbol`           | Error            |
| `LINT0002` | `unused-variable`            | Warning          |
| `LINT0003` | `unknown-table-or-field`     | Error            |
| `LINT0004` | `type-mismatch-assignment`   | Error / Warning (on narrowing) |

#### Suppression

Deferred. No `// noqa` mechanism in v1. Acknowledged in a follow-up doc, not a TODO in code.

### Diagnostic code inventory

Aligned to existing precedent (`PARSE###`, `PREPROC###`):

| Prefix     | Owner             | Range    | Notes                                 |
|------------|-------------------|----------|---------------------------------------|
| `SEM###`   | `oxabl_semantic`  | 0001+    | Declaration / resolution errors       |
| `TYPE###`  | `oxabl_semantic`  | 0001+    | Type-system invariant violations (internal-facing; rarely surfaced) |
| `LINT####` | `oxabl_lint`      | 0001+    | User-facing rule diagnostics          |
| `SCHEMA###`| `oxabl_schema`    | 0001+    | `.df` parse + load diagnostics        |

**V1 reserved codes:**

| Code        | Phase        | Meaning                                                   |
|-------------|--------------|-----------------------------------------------------------|
| `SEM0001`   | Declare      | Duplicate declaration in the same scope                   |
| `SEM0002`   | Declare      | Redeclaration across SHARED / NEW SHARED boundary mismatch |
| `SEM0003`   | Declare      | `BLOB`/`CLOB` used as local variable type                 |
| `SEM0010`   | Resolve      | Override without matching signature (single-file only)    |
| `SCHEMA0001`| Parse        | `.df` syntax error                                        |
| `SCHEMA0010`| Load         | Duplicate table across merged `.df` files (warning)       |
| `SCHEMA0011`| Load         | Duplicate field within a table                            |
| `LINT0001`  | Lint         | `undefined-symbol`                                        |
| `LINT0002`  | Lint         | `unused-variable`                                         |
| `LINT0003`  | Lint         | `unknown-table-or-field`                                  |
| `LINT0004`  | Lint         | `type-mismatch-assignment`                                |

The `OXABL####` prefix from the earlier draft is dropped; it had no precedent. `TYPE###` is
reserved but unused in v1 — placeholder for type-system invariant panics lifted to
diagnostics (e.g., "operator table returned `Error` in a non-error context").

### `analyze` CLI (`oxabl analyze <path>`)

Extend `crates/oxabl/src/main.rs`: add an `Analyze` clap variant next to `Check`. Flags:

- `path` — positional; file or directory.
- `--format text|json` — default `text`.
- `--schema <path>` — repeatable; extends `[workspace.schema] files`.
- `-I/--include-path` — as `check`.
- `--preprocess` — as `check`, default on.
- `--no-lint` — run semantic only; skip lint rules.

**Output shape (JSON):**

```json
{
  "envelope": 1,
  "sections": {
    "scopes": 1,
    "symbols": 1,
    "types": 1,
    "references": 1,
    "diagnostics": 1
  },
  "file": "path/to/file.p",
  "schema_revision": 7,
  "scopes": [ { "id": 0, "kind": "File", "parent": null, "bindings": [...] }, ... ],
  "symbols": [ { "id": 0, "name": "customer", "kind": "Buffer", "scope": 1, "type": "Buffer(schema:customer)", "reads": 3, "writes": 1, "span": {...} }, ... ],
  "references": [ { "node": 42, "resolution": {"kind": "Resolved", "symbol": 0}, "span": {...} }, ... ],
  "types": [ { "node": 17, "type": "Integer" }, ... ],
  "diagnostics": [ { "code": "SEM0001", "severity": "Error", "message": "...", "span": {...} }, ... ]
}
```

**Per-section versioning.** `envelope: 1` is the top-level container shape. Each section
carries its own integer version under `sections`. A breaking change to `symbols` bumps
`sections.symbols` only; consumers that don't read `symbols` don't re-baseline their goldens.
This is the architect review's key upgrade over a single monolithic version field.

Text format (`--format text`) is pretty-printed for humans and *explicitly* not
stability-guaranteed; goldens run against JSON. Text dump is provided for one-off debugging,
not automated downstream consumers — if a consumer needs stable output, it uses JSON.

**Dump lives in `oxabl_analyze`.** The `oxabl_semantic` crate is `serde_json`-free so future
formatter/LSP don't transitively pull it. `oxabl_analyze` is a thin crate that depends on
`oxabl_semantic` + `serde_json` and owns `dump_json()` / `dump_text()`.

### Data flow summary

```
oxabl_common  (unchanged)  ─┐
oxabl_lexer   (unchanged)   │
oxabl_parser  (unchanged*)  │   * tiny: calls NodeIdAllocator
oxabl_ast     (+NodeId)     ├──►  oxabl_semantic ──► oxabl_lint
oxabl_preproc (unchanged)   │            ▲
oxabl_workspace (unchanged) │            │
oxabl_schema  (new)  ───────┘            │
                                         │
oxabl binary  (+analyze subcommand) ─────┘
```

---

## Implementation Phases

Each phase ends in a commit-worthy state with green CI.

### Phase 0 — Freeze and document AST invariants (prerequisite to everything)

**Goal:** before the semantic layer reads a single AST node, enumerate — and commit to — every
property the AST guarantees. Every subsequent phase cites these invariants; violations become
parser bugs, not resolver bugs.

Tasks:

- Write `docs/design/ast-invariants.md`. Enumerate, at minimum:
  - Span invariants (every node has one; in-source-order; non-overlapping siblings; post-
    expansion offsets; `PreprocessedFile::resolve` is the only translation to real source).
  - `NodeId` invariants (once Phase 1 lands: dense, unique, monotonic; `NodeId(0)` is the
    `Program` root).
  - Identifier casing (`name` preserves source casing; case-insensitive compare lives in
    `oxabl_common::atom` helpers — never at the AST layer).
  - Operator precedence baked into tree shape.
  - Postfix chain left-nesting (`a.b:c[i]` shape).
  - `PreprocIf<T>` wraps unexpanded branches; the AST never commits to preprocessor truth.
  - Declaration nodes have a populated `Identifier`.
  - Error recovery uses `Statement::Empty`, never truncated nodes.
  - `body: None` vs `body: Some(vec![])` distinction for interface/abstract methods.
  - `Program.errors` non-empty does not invalidate `Program.statements`.
- Add a short stanza to `CLAUDE.md` pointing to the invariants doc.
- Cross-check the doc against the existing parser: for each invariant, find the parser code
  that upholds it and either link to it or add a `debug_assert!` that verifies it in debug
  builds. Gaps become parser bugs filed immediately.
- Future invariant changes must edit this doc in the same PR; a reviewer-enforced rule.

Deliverables: one markdown doc, optional debug asserts, one CLAUDE.md pointer. Zero code
refactors.

Estimated effort: half a day.

### Phase 1 — NodeId on AST (prerequisite)

**Goal:** every AST node carries a stable `NodeId`, assigned by the parser.

Tasks:

- `crates/oxabl_ast/src/node_id.rs` — define `NodeId(u32)`, `NodeIdAllocator`.
- Add `id: NodeId` field to every `Statement` and `Expression` variant.
- `crates/oxabl_parser/src/parser/mod.rs` — `Parser` owns a `NodeIdAllocator`; helper
  `self.new_id()` called at each node construction site.
- Update all parser tests to ignore `id` in equality (derive `PartialEq` excluding `id` via a
  small helper, or use a compare-ignoring-ids function in tests).
- Benchmark: confirm parser bench regression < 2% (node-id allocation is a single `u32`
  counter increment).

Deliverables: `cargo test -p oxabl_ast -p oxabl_parser` green. No API consumer changes beyond
the new public `NodeId` type.

Estimated effort: small. Mechanical churn across parser, but no algorithmic change.

### Phase 2 — `oxabl_schema` crate  ✅

**Goal:** load `.df` files into a `Schema` with diagnostics. No integration with semantic yet.

Tasks:

- [x] New `crates/oxabl_schema/` workspace member.
- [x] `schema.rs`: types (`Schema`, `SchemaRevision`, `Table`, `TableId`, `Field`, `Index`,
  `SchemaType`). Case-insensitive keying via `OxablAtom` (reused from `oxabl_lexer`).
- [x] `parser.rs`: a dedicated `.df` parser (line-oriented; not reusing the ABL lexer — format is
  not ABL). Tokenizes `ADD TABLE "name"`, indented attribute lines, `ADD FIELD`, `ADD INDEX`.
  Unknown attributes captured as opaque strings.
- [x] `loader.rs`: `SchemaLoader::load_files(paths, &dyn FileSystem)`. Merges with last-write-wins
  + `SCHEMA0010` conflict diagnostic. Adds `SCHEMA0011` (duplicate field),
  `SCHEMA0012` (field-type conflict → `SchemaType::Error`), `SCHEMA0030`
  (workspace-root containment), `SCHEMA0031` (soft caps).
- [x] Tests: 40 inline unit tests + 5 integration tests against Riverside
  Software's `sp2k.df` golden (MIT; vendored under `fixtures/`). BOM/CRLF,
  multi-line quoted strings, `#` comments, footer trailer, embedded `""`.
- [x] Benchmark: `cargo bench -p oxabl_schema --bench schema_bench` on a 5 MB merged `.df` fixture.
  Target: < 100 ms load. Actual: **7.2 ms** (~694 MB/s).
- [x] Spike: OxablAtom supports runtime interning (lexer already uses
  `OxablAtom::from(&str)` for string literals); unified regime kept, no
  `lasso` fallback needed.

Deliverables: standalone crate, no other crate depends on it yet. Consumable for spike work.

Estimated effort: medium. `.df` grammar is small but attribute variety is long-tailed.

### Phase 3 — `oxabl_semantic` crate skeleton + declare pass  ✅

**Goal:** first of three passes. Symbol table + scope tree populated for every AST construct
the parser supports. No types, no references yet.

**Module layout.** Flat, per the `oxabl_parser` evolution pattern — no subdirectories until a
single file exceeds ~2k lines:

```
crates/oxabl_semantic/src/
  lib.rs         ← public API, `analyze_file`
  scope.rs
  symbol.rs
  namespace.rs
  resolve.rs     ← absorbs the prior `declare.rs`; declare + resolve share walker infra
  types.rs       ← ResolvedType, PrimitiveTy
  coercion.rs    ← assignable / assignable_strict
  operators.rs   ← binary/unary op typing tables
  check.rs
  builtins.rs
  virtual_span.rs (re-export)
```

Tasks:

- [x] New `crates/oxabl_semantic/` workspace member (no `serde_json` dep).
- [x] `scope.rs`: `ScopeId`, `ScopeTree`, `Scope`, `ScopeKind`, `BindingMap`.
- [x] `symbol.rs`: `SymbolId`, `Symbol`, `SymbolKind`, `SymbolFlags`, `SymbolTable`,
  `rebinding_scopes`.
- [x] `namespace.rs`: `NamespaceId`, `NUM_NAMESPACES` constant, resolution-order table.
- [x] `resolve.rs` (declare half): `declare_pass(program, ctx) -> (ScopeTree, SymbolTable,
  Vec<Diagnostic>)`. One case per statement kind the parser emits. Diagnostics:
  `SEM0001`/`SEM0003` emitted in v1 (`SEM0002` reserved; SHARED-mismatch surfaces once
  Phase 4a wires cross-scope lookup).
- [x] `builtins.rs`: seeds five entries (`THIS-OBJECT`, `SUPER`, `SELF`, `SESSION`,
  `ERROR-STATUS`). Grows from Phase 5 corpus audit, not from a speculative catalog.
- [x] Tests: 47 inline `assert_eq!` tests per declaration construct
  (VARIABLE/PARAMETER/TEMP-TABLE/BUFFER/STREAM/FRAME/EVENT/PROPERTY/DATASET/DATA-SOURCE,
  PROCEDURE, FUNCTION, CLASS + METHOD, INTERFACE, CONSTRUCTOR/DESTRUCTOR, CATCH variable,
  DO-counter variable, FOR EACH implicit buffer, PreprocIf branches, ON trigger, TRIGGER
  PROCEDURE, SEM0001/SEM0003). Plus 19 scope/namespace/type/index_vec/builtins tests — 66
  total in the crate. No `insta` dependency.
- [x] `VirtualSpan` newtype added to `oxabl_common`; re-exported.
- [x] `NodeIndexVec<T>` side table seeded (`references` + `types` reserved for Phase 4).

Deliverables: `fn declare_pass(program, ctx) -> (...)` callable and tested.

Estimated effort: ~3 days. Breadth of ABL declaration forms is the cost, not algorithmic depth.

### Phase 4a — resolve pass (references + signatures)  ✅

**Goal:** second pass. Every identifier reference resolved (or structured-unresolved); every
declaration typed. No expression-body type-checking yet.

Tasks:

- [x] `resolve.rs` (resolve half): walks reference positions only. For each identifier reference,
  consults scope chain with namespace-narrowing rules, populates
  `references: IndexVec<NodeId, Option<Resolution>>`. Handles qualified `table.field`,
  `object:member`, `array[i]`, `buffer.field`.
- [x] Signature typing: for each `Symbol` with declared type (parameters, return type, properties,
  variable declarations, temp-table fields), populate `Symbol::data_type` and the `types`
  side table at the declaration's NodeId. Class-typed declarations upgrade from
  `Unknown` to `Class(SymbolId)` when the class is declared locally.
- [x] Schema integration: when `schema_loaded`, `table.field` references consult the `Schema`;
  unresolved fields become `Resolution::Unresolved { reason: NotInScope }` in the field
  namespace — picked up by `unknown-table-or-field`. When schema is absent, they become
  `reason: NoSchema`. v1 treats field-under-buffer as `External` until schema-backed
  field lookup wires in Phase 4b.
- [x] External-ness detection: `USING`-imported names, `RUN "name"`, `NEW ClassName` for
  non-local types produce `Unresolved { reason: External }`. Dynamic forms (`RUN VALUE(x)`,
  `DYNAMIC-FUNCTION`, dynamic buffer ops) surface their expressions normally.
- [x] Idempotent read/write counts (plan §C7): per-symbol counts accumulate into a local
  `FxHashMap<SymbolId, (u32, u32)>` and write back once at end-of-pass, so re-running
  `resolve_pass` is a no-op.
- [x] `Resolution::Unresolved` carries `name: OxablAtom` (plan §C5) so lint diagnostics
  don't reslice the source span per emission.
- [x] Tests: 50 inline unit tests across reference forms, including namespace shadowing
  (variable vs buffer), scope-walk lookup, NEW class upgrades, schema-loaded vs
  schema-absent field access, RUN OUTPUT write-count, and idempotence.

### Phase 4b — type-check pass (expression bodies)  ✅

**Goal:** third pass. Every expression NodeId carries a `ResolvedType`. No diagnostics emitted
by the semantic layer for type mismatches — lint owns `LINT0004` as the single user-facing
channel.

Tasks:

- [x] `types.rs`: `ResolvedType`, `PrimitiveTy` (already shipped in Phase 3; unchanged).
- [x] `coercion.rs`: `assignable`, `assignable_strict`, `is_narrowing_warning`,
  `widen_primitive`. Primitive widening ladders, silent numeric narrowing,
  Longchar→Character / Datetime→Date warnings, Unknown as universal bottom, Error
  poisoning without cascade, array/Class/Buffer/Table compared by identity.
- [x] `operators.rs`: binary + unary typing tables.
  - `+`: numeric ladder, string concat (Character/Longchar), Date/Datetime + Integer/Int64
    preserves date-like type.
  - `-`: numeric pairs; `Date - Date → Integer` (days); `Datetime - Datetime → Int64` (ms).
  - `*`: numeric widen.
  - `/`: **always returns `Decimal`** (ABL quirk baked in).
  - `MODULO`: `Integer×Integer → Integer`, `Int64×Int64 → Int64`, widen mixed to Int64.
  - Comparisons: return `Logical` iff operands share a widening ladder, else Error.
  - `AND`/`OR`: Logical × Logical → Logical.
  - `BEGINS`/`MATCHES`/`CONTAINS`: Character × Character → Logical.
  - Unary `Negate`: numeric only; `Not`: Logical only.
  - `Unknown` propagates as Unknown; `Error` poisons without cascade.
- [x] `check.rs`: bottom-up type synthesis walker. Literals synthesize (Integer literals
  outside i32 range become Int64); identifiers consult `Symbol::data_type` via the resolve
  side table; NEW / method / function call / member-access expressions take their type
  from the resolved symbol (Class/Interface → `Class(SymbolId)`, Buffer/TempTable →
  `Buffer(SymbolId)`). Ternary widens branches via `widen_primitive`;
  FieldAccess/MemberAccess/MethodCall surface `Unknown` in v1 (schema-backed field types
  and cross-class method return types deferred). **Emits no diagnostics** — type-mismatch
  evidence lives in the populated `types` side table.
- [x] Poison-propagation: `Error` cascades through operator evaluation; `Unknown` is the
  lattice bottom and widens to anything.
- [x] `analyze_file` now runs all three passes (declare → resolve → check).
- [x] Tests: 79 unit tests across coercion (20 cases), operators (24 variants including
  `/` → Decimal, DATE+INT preserve-Date, DATETIME-DATETIME → Int64), and end-to-end check
  (35 integration tests via `analyze_file` covering literals, identifier types,
  arithmetic/comparison/logical/string ops, ternary, array access, NEW class → Class
  symbol, CAN-FIND → Logical, function call return types, Unknown propagation, Error
  poisoning, no-diagnostics invariant, full `analyze_file` smoke test).

Deliverables: `fn analyze_file(program, ctx) -> Semantic` usable end-to-end.

Estimated effort: Phase 4a ~2 days; Phase 4b ~3 days. The coercion catalog is the risk — see
spike in Dependencies.

### Phase 5 — `oxabl_lint` crate + 4 rules  ✅

Tasks:

- [x] New `crates/oxabl_lint/` workspace member depending on `oxabl_semantic`.
- [x] `lib.rs`: `lint_file(program, sem, ctx) -> Vec<Diagnostic>`. `LintContext` /
  `RuleSet` dropped per simplification — `AnalysisContext` already carries every field a
  rule needs and rule-toggle config isn't real yet.
- [x] `rules/undefined_symbol.rs` (LINT0001), `rules/unused_variable.rs` (LINT0002),
  `rules/unknown_table_or_field.rs` (LINT0003), `rules/type_mismatch_assignment.rs`
  (LINT0004) — each a standalone `run(program, sem, ctx) -> Vec<Diagnostic>`.
- [x] Inline tests per rule with `assert_eq!` against codes + message substrings. No
  `insta` dependency added.
- [x] **Skip-list coverage invariant** — every documented skip fires in a regression test:
  - `undefined-symbol` (LINT0001): skips `External` (NEW class USING-import) and `NoSchema`
    (field access when schema absent); resolves against builtins (SESSION); 9 tests.
  - `unused-variable` (LINT0002): skips OUTPUT, INPUT-OUTPUT, INTERFACE method params,
    ABSTRACT method params; covers true positive (unused INPUT), used param, procedure
    declaration no-op, write-without-read-still-warns; 11 tests.
  - `unknown-table-or-field` (LINT0003): no-fire when schema absent, no-fire when qualifier
    resolves to local buffer under schema, no-fire on non-field expressions, no-fire on
    local variable, fires on unknown qualifier under schema; 5 tests.
  - `type-mismatch-assignment` (LINT0004): widening silent, silent Decimal→Integer,
    error on Logical↔Integer, narrowing Longchar→Character warns, skip Unknown literal,
    skip unresolved identifier, Assignment + Assign multi-target, class upcast
    single-file OK, cross-file Class silent, Int widening, no-init no-op, Character
    assignment; 15 tests.
  - Total: 40 inline rule tests.
- [x] `NodeIndexVec::insert` now guards against `NodeId::DUMMY` — hand-constructed AST in
  tests used to OOM on `u32::MAX` indexing.

Deliverables: `cargo test -p oxabl_lint` green (40/40), workspace still passes (933 total).

Deferred to follow-up:
- Corpus `corpus_lint_audit` binary (depends on Phase 6 analyze CLI end-to-end; will land
  in Phase 6's audit step against corpus sampled files).

### Phase 6 — `oxabl_analyze` crate + `oxabl analyze` subcommand + goldens  ✅

Tasks:

- [x] New `crates/oxabl_analyze/` workspace member. Deps: `oxabl_semantic`,
  `oxabl_lint`, `serde`, `serde_json`. Keeps `serde_json` off `oxabl_semantic`'s
  dependency graph.
- [x] `lib.rs`: `fn dump_json(program, sem, ctx, include_lint) -> serde_json::Value` and
  `fn dump_text(program, sem, ctx) -> String`. Per-section versioning envelope
  (`envelope: 1`, `sections: { scopes, symbols, types, references, diagnostics }`).
  Diagnostics are tagged `source: "semantic" | "lint"`.
- [x] `crates/oxabl/src/main.rs`: `Cli::Analyze` variant wired; `run_analyze(path,
  format, no_lint, preprocess, include_paths)` returns `ExitCode`. Supports
  `--format json|text`, `--no-lint`, `--preprocess`, `--include-path`.
- [x] Fixture tests — *property-based* rather than brittle exact-JSON goldens.
  NodeIds aren't stable across parser changes, so goldens would rot fast. Tests
  in `tests/fixture_tests.rs` assert shape invariants (envelope sections,
  builtins seeded, procedure scope has params, OUTPUT param skipped by LINT0002,
  function return type is Decimal, etc.). 5 fixtures × 12 shape checks = 12 tests;
  plus 8 unit tests on the dump itself = 20 analyze tests total.
- [x] Text-format smoke test: `dump_text_contains_scopes_and_symbols_headers`.

Deliverables shipped:
- `oxabl analyze path/to/file.p --format json` returns a stable versioned document.
- `oxabl analyze path/to/file.p --format text` renders a human-oriented summary.
- `cargo test -p oxabl_analyze` green (20 tests).

Fixture goldens vs property-based: the plan targeted ≥30 exact-JSON goldens, but the
semantic dump includes parser-assigned NodeIds that churn when the parser grows new
statement kinds. Property-based shape assertions across 5 canonical fixtures
(simple_variable, procedure_with_params, function_with_return, unused_variable,
undefined_symbol) cover construct diversity without the maintenance cost of exact
diffs. More fixtures can grow organically — the runner in
`tests/fixture_tests.rs::every_fixture_*` iterates every file in
`tests/fixtures/`, so adding a `.p` there automatically extends the property checks.

Deferred to follow-up:
- `corpus_lint_audit` binary against sampled corpus (scope creep for v1 ship).
- Exact-JSON goldens under a stable NodeId allocator (blocked on parser's
  NodeId-minting determinism under feature growth).

### Phase 7 — Architectural guardrail appendices  ✅

Two short written sketches that prove R10 and R11 remain reachable. **Non-blocking** for
the v1 merge — the side-table + NodeId architecture *is* the contract, and these docs are
the illustrated commentary on it. They're still written at v1 time (while context is fresh)
but a reviewer is not expected to block merge on sketch wording. Stored in `docs/design/`.

**`docs/design/semantic-v1-cross-file-sketch.md`** (R10): shows how
`Resolution::Unresolved { reason: External }` entries become `Resolved(SymbolId)` when a
multi-file `SymbolTable` (keyed by `(FileId, NodeId)`) is introduced, without changing any
v1 public type signatures. Specifically: `Semantic` per file remains unchanged; a new
`Workspace::resolve_cross_file(&[Semantic]) -> CrossFileResolutions` *side table* fills in the
unresolveds. USING imports and `RUN "name"` are the two concrete entry points walked.

**`docs/design/semantic-v1-flow-analysis-sketch.md`** (R11): shows how a CFG can be built over
the existing `Statement` tree because blocks retain source order and every statement carries a
`NodeId`. `CfgBuilder::build(program, scope_tree) -> Cfg` is the attachment point. Definite
assignment becomes a dataflow pass parameterized over `Cfg`; no AST changes required.

These are intentionally not code — they're the written contract that the IR survives later
work. The reviewers for the v1 merge are expected to stress-test them.

Estimated effort: small.

### Phase 8 — Benchmarks, CI, release  ✅

Tasks:

- [x] `crates/oxabl_semantic/benches/semantic_bench.rs`: **per-pass granularity** —
  separate benches for `declare_pass`, `resolve_pass`, `check_pass`, and `analyze_file`
  end-to-end, on two fixtures (tiny + medium). Uses `iter_batched` for resolve/check so
  setup (prior-pass output) isn't counted. Aggregate-only numbers hide per-pass
  regressions.
- [x] `.github/workflows/codspeed.yml`: unchanged; CodSpeed auto-discovers
  `[[bench]] name = "semantic_bench"` from Cargo metadata.
- [x] Release Please config updated: added `oxabl_preprocessor`, `oxabl_schema`,
  `oxabl_semantic`, `oxabl_lint`, `oxabl_analyze` to `release-please-config.json` and
  `.release-please-manifest.json` at `0.1.0` each. Pre-1.0, `feat:` bumps minor.
- [x] CI (`.github/workflows/ci.yml`): no changes needed; uses `cargo test --workspace`
  and `cargo clippy --workspace` which auto-include the new crates.

Deferred to follow-up:
- `crates/oxabl_lint/benches/lint_bench.rs` per-rule benches. The individual rule
  functions are stable enough to bench today; they can land as a small follow-up PR
  once the `oxabl analyze` corpus run is wired.
- `README.md` Current Status update (trivial; can go with the release commit).

---

## Alternative Approaches Considered

**A. In-place AST annotation.** Add `Option<Resolution>`, `Option<ResolvedType>` fields to every
AST node. Rejected: (1) mutates AST invariant, precluding Salsa's input hashing model; (2)
couples every non-semantic consumer to semantic types; (3) makes cross-file analysis impossible
without splitting fields per file. Industry consensus (rust-analyzer, Oxc, Biome, Ruff, Roslyn)
is uniform against in-place annotation.

**B. Full HIR lowering from the start.** Introduce `oxabl_hir` that lowers `oxabl_ast` into a
desugared form; run semantic on HIR. Rejected for v1: ABL has little desugaring pressure at this
stage (few derived forms that demand normalization), and a HIR doubles the maintenance surface.
Side tables over AST are the rust-analyzer bridge: HIR can still be introduced later without
breaking v1 callers.

**C. Salsa-based incremental queries in v1.** The 2026-04-13 plan took this path. Rejected for
this v1 scope: Salsa is the right endgame but pays zero dividend for the four lint rules and
dump tool. Introducing Salsa now bakes in query granularity choices before we know the real
query set. The side-table design is *Salsa-ready* (`fn analyze_file(program, ctx) -> Semantic`
is already the right query shape) without paying the upfront cost.

**D. `.df` parser inside `oxabl_semantic`.** Rejected: `.df` is usable by formatter, LSP,
codemods, migration tools, and documentation — none of which should depend on the semantic
layer. Separate crate.

**E. Rule framework (traits/visitor) in `oxabl_lint` from day one.** Rejected: four rules do not
justify abstraction; YAGNI. A trait can be introduced when the fifth rule demands it, without
breaking the 4-rule call sites.

**F. Linter as `oxabl` binary subcommand only, no separate crate.** Rejected: the linter is
itself a library consumer of the semantic layer — the v1 contract is that its API is reusable.
Collapsing into the binary hides that.

---

## System-Wide Impact

### Interaction Graph

- `oxabl analyze foo.p` → `Workspace::from_path(cwd)` → `FileSystem::read` for the target file
  → `Preprocessor::process` → `PreprocessedFile::to_text` → `tokenize` → `Parser::parse_program`
  (now NodeId-stamped) → `SchemaLoader::load_files` (if any) → `semantic::analyze_file` →
  `lint::lint_file` → diagnostic remap via `PreprocessedFile::resolve` → `dump_json` →
  stdout/file.
- `oxabl check` pipeline is untouched (same preprocessor/parser path; lint not invoked).
- CI: `cargo test` picks up three new crates automatically; `cargo clippy` strict-warnings gate
  applies to them.

### Error Propagation

- `.df` parse errors → `SCHEMA0001..SCHEMA00xx` diagnostics, returned alongside the partial
  `Schema`. The schema loader is fault-tolerant: one bad `.df` does not prevent resolution
  against the others.
- Preprocessor diagnostics already surface through the existing path; semantic layer appends
  its own without shadowing.
- Inside semantic, the `ResolvedType::Error` sentinel stops cascading errors at their first
  point of failure (rust-analyzer pattern).
- `oxabl analyze` exit codes: `0` if no errors, `1` if any semantic/lint diagnostic at
  `Error` severity, `2` on I/O / schema load failure.

### State Lifecycle Risks

None — semantic analysis is pure over `(program, ctx)`. No state persists between files. No
caches to invalidate. Adding caching is explicitly future work (Salsa).

### API Surface Parity

The `Semantic` struct is the only public semantic API. Any later consumer (LSP, formatter,
refactorer) must go through it. Cross-file and flow-analysis additions extend — never replace —
this surface. Confirmed by the two design sketches in Phase 7.

### Integration Test Scenarios

1. **Preprocessor-expanded declaration**: an include file defines a variable; the caller
   references it. Dump shows the variable symbol declared at its real (in-include) source
   span, referenced at the caller's span. Verifies the `resolve`-remapping seam end-to-end.
2. **Schema absent → partial → loaded**: three runs of the same file produce (a) zero
   `unknown-table-or-field` diagnostics, (b) partial diagnostics, (c) zero diagnostics again.
   Verifies R7 silent-degradation.
3. **`?` propagation**: `DEF VAR x AS INT. x = ?. x = x + 1.` — no type-mismatch, `x`'s static
   type remains `Integer`, `?` narrows at the assignment site.
4. **Single-file INHERITS**: `CLASS Sub INHERITS Super` where both in same file; method
   override resolves; assignment of `Sub` to `Super` passes; assignment of sibling class fails.
5. **Unused OUTPUT parameter silence**: a method with `OUTPUT` parameter never written inside
   is not flagged — `unused-variable` is tracking reads, but the rule skip list covers this
   explicitly. Fixture confirms the skip.

---

## Acceptance Criteria

### Functional Requirements

- [ ] Every AST node has a stable `NodeId` after parse.
- [ ] `oxabl_schema` parses the corpus `.df` sample set without error; conflicts produce
  `SCHEMA0010` warnings.
- [ ] `semantic::analyze_file` produces a `Semantic` whose `references` map is populated for
  every identifier reference in the parser's supported constructs.
- [ ] `Semantic::types` is populated for every expression node and every declaration node
  whose type is determinable (or `Unknown`/`Error`).
- [ ] All four v1 lint rules produce diagnostics only via the rule-specific logic above; no
  rule fires on its documented skip list (verified by negative-case fixtures).
- [ ] `oxabl analyze` exits 0 on clean input, 1 on lint/semantic error.
- [ ] Dump JSON includes `envelope: 1` + per-section versions and is stable across benign
  refactors (enforced via golden-file diff on parsed `serde_json::Value`).

### Non-Functional Requirements

- [ ] `analyze_file` runs within 3× parse time on every corpus fixture.
- [ ] `schema_loader` loads a 5 MB merged `.df` in under 100 ms.
- [ ] New benches run on CodSpeed CI; regressions surface automatically.
- [ ] `cargo clippy -D warnings` clean on all three new crates.

### Quality Gates

- [ ] ≥ 40 unit tests in `oxabl_semantic` (declare pass).
- [ ] ≥ 40 unit tests in `oxabl_semantic` (resolve pass).
- [ ] ≥ 30 unit tests in `oxabl_semantic` (type-check pass).
- [ ] ≥ 27 rule tests across the four lint rules, one per documented skip-list entry + happy
  path + false-positive avoidance (see Phase 5 skip-list coverage invariant).
- [ ] ≥ 30 `oxabl_analyze` golden-dump snapshots.
- [ ] Corpus audit baseline TSV committed; each rule's precision ≥ 0.9 on a 100-file manual
  audit sample.

### Architectural Guardrails (R10, R11)

- [ ] `docs/design/semantic-v1-cross-file-sketch.md` written (non-blocking appendix).
- [ ] `docs/design/semantic-v1-flow-analysis-sketch.md` written (non-blocking appendix).
- [ ] The side-table + NodeId architecture demonstrably satisfies R10/R11 per the sketches.
  If a sketch surfaces an IR change required to v1 types, revise Phases 3–4b before merge.

---

## Success Metrics

- True-positive / false-positive rate per rule on the sampled corpus audit (target ≥ 0.9
  precision each).
- Corpus coverage: % of corpus files where `oxabl analyze` produces a parseable, non-error
  dump (target ≥ the parser's current success rate, since semantic degrades gracefully where
  parse succeeds).
- Third-party consumability: a fifth lint rule can be added in a follow-up PR touching only
  `oxabl_lint` — verified after merge by a throwaway rule PR (e.g., `shared-variable-without-`
  `no-undo`) that compiles and runs without editing `oxabl_semantic` or `oxabl_ast`.

---

## Dependencies & Prerequisites

- AST is parse-stable (confirmed: 434 parser tests).
- Preprocessor seam exposes both expanded text and `SpanNode` tree for remapping (confirmed:
  `crates/oxabl_preprocessor/src/span_tree.rs:43-108`; already used by
  `crates/oxabl/src/main.rs:355-377`).
- ABL type/coercion catalog is sufficient to encode v1 from Progress documentation plus corpus
  confirmation. **Spike required in Phase 3/4 bridge**: before writing `coercion.rs`, harvest
  all assignment and operator sites in a corpus sample, classify LHS/RHS declared types, and
  diff against the drafted rule table. Budget: 1 day; outcome is either a confirmed catalog or
  a list of gaps to flag in `SemanticLimitations` doc and suppress from `type-mismatch-`
  `assignment` with `External`/`Unknown` reasons.

## Risk Analysis & Mitigation

| Risk                                                           | Mitigation                                                                                          |
|----------------------------------------------------------------|-----------------------------------------------------------------------------------------------------|
| Coercion catalog incomplete → high false-positive rate         | Spike in Phase 3/4 bridge; `Unknown`/`Error` lattice suppresses cascades; audit TSV in Phase 5.      |
| `.df` format drift across Progress versions                    | Opaque attribute pass-through; don't error on unknowns.                                             |
| OO-ABL single-file shadowing subtleties (INHERITS, PROPERTY)   | Explicit fixture set in Phase 3 per rule; deferred items (cross-file inheritance) land as `External`. |
| Namespace ambiguity (`customer` → buffer? table? variable?)    | Explicit namespace table; resolution order documented; default-buffer shadow fixture.                |
| NodeId churn breaks downstream test equality                   | Tests use `EqIgnoreIds` helper; document the pattern in the PR.                                     |
| Corpus audit surfaces many false positives                     | `Unresolved::External` / `NoSchema` reasons skip lint; suppression pragmas deferred but flagged. |
| Dump format instability churns goldens                         | Per-section versioning envelope; additive fields don't bump; breaking changes bump one section only. |

## Resource Requirements

Single engineer, sequential phases. Rough per-phase effort:

| Phase                   | Effort      |
|-------------------------|-------------|
| 0 — AST invariants doc  | 0.5 day     |
| 1 — NodeId on AST       | 1 day       |
| 2 — `oxabl_schema`      | 2 days      |
| 3 — Declare pass        | 3 days      |
| 4a — Resolve pass       | 2 days      |
| 4b — Type-check pass    | 3 days      |
| 5 — `oxabl_lint`        | 3 days      |
| 6 — `oxabl_analyze`+CLI | 2 days      |
| 7 — Guardrail sketches  | 0.5 day     |
| 8 — Benches/CI/release  | 1 day       |
| **Total**               | **~18 days**|

Gated by test/bench cycles. Cross-phase review happens at Phase 6 (first end-to-end output).

## Future Considerations

- **Cross-file resolution** (R10 sketch): the `Workspace::resolve_cross_file` side table is the
  attachment point; no change to `Semantic`.
- **Flow analysis** (R11 sketch): `CfgBuilder` over `Program` + `ScopeTree`; definite
  assignment / unreachable code / NO-UNDO enforcement as dataflow passes.
- **Salsa incrementality**: `fn analyze_file(program, ctx) -> Semantic` is already a valid
  Salsa tracked function. Lift it into a `#[salsa::tracked]` when the LSP is on deck.
- **LSP**: consumes `Semantic` + `Workspace` + `Schema`; `analyze` CLI's JSON schema is the
  prototype for LSP `hover` / `goto-definition` payloads.
- **Lint rule suppression pragmas**: `// oxabl:disable <code>` comments recognized at
  tokenization layer; rules check per-diagnostic.
- **Autofix**: add `fn fix(&Diagnostic, &Semantic) -> Option<Edit>` to the rule signature.
- **Additional lint rules**: `no-undo-on-temp-table`, `unused-import-using`,
  `widget-handle-leak`, `shadowed-variable`, `deprecated-keyword`.

## Documentation Plan

- `README.md` — update crate listing + `Current Status`.
- `CLAUDE.md` — add bullet under "Architecture" describing `oxabl_semantic`, `oxabl_schema`,
  `oxabl_lint`, and the side-table design convention.
- `docs/design/semantic-v1-cross-file-sketch.md` — Phase 7 deliverable.
- `docs/design/semantic-v1-flow-analysis-sketch.md` — Phase 7 deliverable.
- `docs/design/semantic-v1-api.md` — narrative on `Semantic`, `Resolution`, `ResolvedType`,
  namespace rules; the reference doc for anyone writing a new lint rule.
- `crates/oxabl_lint/docs/rules/<rule-code>.md` — per-rule page (fires-when, skip list,
  examples) so rule behavior is discoverable without reading source.

---

## Sources & References

### Origin

**Origin document:** [docs/brainstorms/2026-04-16-semantic-layer-requirements.md](../brainstorms/2026-04-16-semantic-layer-requirements.md)

Key decisions carried forward from origin:

1. **Foundation-first, not feature-first** — v1 ships semantic + 4-rule linter + dump tool;
   no LSP (see origin: Key Decisions).
2. **Schema is first-class but optional** — silent degradation when `.df` absent (see origin:
   R7, Key Decisions). Implemented via `Resolution::Unresolved { reason: NoSchema }` + rule
   suppression.
3. **v1 covers symbols+scopes and type annotation/checking; cross-file + flow deferred** (see
   origin: R10, R11). Architectural-guardrail sketches in Phase 7 are the contract that
   deferred items remain reachable.
4. **Four lint rules chosen for breadth, not marketability** — one per major capability (see
   origin: Key Decisions). Plan's rule severities and skip lists are explicit resolutions of
   spec-flow gaps flagged during planning.

All Outstanding Questions from the origin are answered in this plan:

- IR placement → side tables in new `oxabl_semantic` crate with `NodeId` on AST.
- `.df` parser home → new `oxabl_schema` crate; discovery via `[workspace.schema]` +
  `--schema` flag.
- Type/coercion rules → `coercion.rs` catalog derived from Progress docs + corpus spike.
- Diagnostic format → reuse `oxabl_common::Diagnostic`; no new crate.
- Lint packaging → new `oxabl_lint` crate; `analyze` is a subcommand of `oxabl` binary.
- Cross-file & flow sketches → Phase 7 deliverables.
- Dump format → JSON (golden-tested) + text (human-only), versioned.
- Silent-default schema absent → confirmed (silent, no warn-once).

### Internal References

- Prior, larger-scope plan (partly landed): [2026-04-13-001-feat-semantic-layer-toolchain-ecosystem-plan.md](./2026-04-13-001-feat-semantic-layer-toolchain-ecosystem-plan.md).
  Its Phase 4 (`.df` sketch) and Phase 5 (`SymbolTable`/`ScopeTree` sketch) are reference
  material for this plan.
- Diagnostics: `crates/oxabl_common/src/diagnostic.rs`.
- FileId / FileSpan: `crates/oxabl_common/src/file_id.rs`, `file_span.rs`.
- Workspace + schema config: `crates/oxabl_workspace/src/config.rs:38-44, 81-103`.
- Preprocessor seam: `crates/oxabl_preprocessor/src/span_tree.rs:43-108`; reference use at
  `crates/oxabl/src/main.rs:355-377`.
- AST statement/expression shapes: `crates/oxabl_ast/src/statement.rs`, `expression.rs`.
- Parser entry: `crates/oxabl_parser/src/parser/mod.rs:91-145`.
- Corpus failure notes: `docs/solutions/corpus-remaining-failures.md`.
- CLAUDE.md conventions (benchmarks, case-insensitive ASCII folding, clippy -D warnings).

### External References

- rust-analyzer architecture: https://github.com/rust-lang/rust-analyzer/blob/master/docs/book/src/contributing/architecture.md
- Oxc `oxc_semantic`: https://github.com/oxc-project/oxc/tree/main/crates/oxc_semantic
- Biome `biome_js_semantic`: https://github.com/biomejs/biome/tree/main/crates/biome_js_semantic
- Ruff `ruff_python_semantic`: https://github.com/astral-sh/ruff/tree/main/crates/ruff_python_semantic
- Roslyn overview: https://github.com/dotnet/roslyn/blob/main/docs/wiki/Roslyn-Overview.md
- Salsa book: https://salsa-rs.github.io/salsa/
- Bidirectional typing survey — Dunfield & Krishnaswami: https://arxiv.org/abs/1908.05839
- Diagnostics libraries: [miette](https://github.com/zkat/miette),
  [codespan-reporting](https://github.com/brendanzab/codespan-reporting).

### Related Work

- Source document: [docs/brainstorms/2026-04-16-semantic-layer-requirements.md](../brainstorms/2026-04-16-semantic-layer-requirements.md)
- Preceding preprocessor benchmark plan: [2026-04-16-003-feat-preprocessor-benchmarks-plan.md](./2026-04-16-003-feat-preprocessor-benchmarks-plan.md)

---

## Research-Deepening Addendum (2026-04-17)

Second round of parallel research + review agents against the plan. Twelve agents consulted:
architecture-strategist, performance-oracle, code-simplicity-reviewer, pattern-recognition-
specialist, security-sentinel, data-integrity-guardian, kieran-typescript-reviewer (applied
to Rust type signatures), code-reviewer, spec-flow-analyzer, best-practices-researcher
(bidirectional typing), framework-docs-researcher (rust-analyzer / Oxc / Biome / Ruff 2026
source), repo-research-analyst, learnings-researcher.

This addendum resolves **factual errors** in the plan body, surfaces **critical gaps**
(blockers), records **simplicity-vs-integrity tensions** that require explicit decisions
before Phase 3, enumerates **new considerations** missed by the first round, and appends
**citations to primary sources**.

### Factual Corrections to the Plan Body

Applied inline above:

1. **§Technical Approach "Side tables":** the "Biome and Oxc converged" claim was wrong —
   only Oxc did. Biome uses `FxHashMap<TextSize, …>` keyed by byte offset. Plan corrected
   to cite Oxc + Ruff as precedent and frame Biome as deliberate divergence.
2. **§`OxablAtom` citation:** file path `crates/oxabl_lexer/src/keyword.rs` doesn't exist —
   corrected to `crates/oxabl_lexer/src/kind.rs` where `match_keyword()` actually lives.

Flagged but not auto-applied (decision-required):

3. **§`oxabl_common::Diagnostic` shape:** `code: DiagnosticCode` wraps `&'static str`, not
   `String`. `Severity` has four tiers (Error / Warning / Info / Hint). `help` is
   `Option<String>`, not a multi-note `Vec<String>`. Implications:
   - Every `SEM####` / `LINT####` / `SCHEMA####` code must be a literal (or `Box::leak`'d).
     Fine for hand-authored codes; the plan's code inventory is compatible.
   - If a rule wants multi-line remediation or several "notes", it must either concatenate
     into `help` or use `Vec<Label>` with secondary spans. Plan should pick one style.
   - `Severity::Hint` exists; v1 uses only Error / Warning. Document this explicitly.
4. **§Phase 1 "PartialEq excluding id":** Phase 1 task list (line ~760) offers two options
   ("derive excluding id *or* use a compare-ignoring function") that contradict Enhancement
   Summary #2 ("no compare-ignoring helper that authors will forget"). Risk-table row also
   recommends `EqIgnoreIds` helper. Three sources, three answers — **pick one** before
   starting Phase 1: project-local `AstPartialEq` derive macro is the recommended answer.

### Critical Gaps — Add Before Starting Phase 3

**C1. `OxablAtom` may be keyword-closed (blocker for Phase 2/3).**

`OxablAtom` is generated by `string_cache_codegen` seeded with the keyword set.
`string_cache` supports dynamic `Atom::from(&str)` in the general case, but the current
codegen wiring may compile a `StaticAtomSet` optimized for compile-time `atom!(…)` use
only. If user identifiers (variable names, table names from `.df`) cannot be interned at
runtime into the same atom type, the "unified interning regime" collapses. **Resolve by
spike at the start of Phase 2:** write a test that interns a non-keyword string at
runtime, verify pointer equality across repeated interns, and confirm the byte-equivalence
contract against case-folded input. If it fails, the plan has two fallbacks:

- (a) Reconfigure the lexer's `string_cache_codegen` output to `AtomType::new(…,
  "keyword_atom!")` with a separate dynamic `Atom<DynamicSet>` for identifiers.
- (b) Use `lasso::Spur` or `ustr::Ustr` as a general-purpose interner in `oxabl_semantic`
  and `oxabl_schema`; the lexer stays keyword-only.

Fallback (b) is simpler and has no lexer risk. Re-evaluate the "no new atom type" claim in
Enhancement Summary #4 if (b) is chosen.

**C2. Schema `.df` field-type conflict (data-integrity bomb).**

`SCHEMA0010` (warning) covers duplicate tables; `SCHEMA0011` covers duplicate fields *in a
table*. Neither covers the case where file A defines `Cust.Name AS CHARACTER` and file B
defines `Cust.Name AS INTEGER` under last-write-wins: B silently wins; every semantic
analysis against A's code is now type-checking against B's schema. Add:

- **`SCHEMA0012` (Error) — "field type conflict across merged .df files."** Fires when two
  merged `.df` files declare the same `Table.Field` with incompatible `SchemaType`.
- Resolution policy: **refuse the merge for the conflicting field only.** Mark the field
  as `SchemaType::Error` (new variant) in the loaded `Schema`. Every
  `ResolvedType::Table(rev, id).field(name)` that resolves to an error-typed field yields
  `ResolvedType::Error`, suppressing downstream cascade in `LINT0004`.

**C3. `schema_revision` mismatch across `Semantic` values (future cross-file hazard).**

The cross-file sketch in Phase 7 has `Workspace::resolve_cross_file(&[Semantic])` accept a
slice of `Semantic`s but no revision-consistency invariant. Two files analyzed against
different `SchemaRevision` values must not be mixed. Add:

- **`SEM0020` (Error) — "schema revision mismatch across aggregated files."** Enforced by
  the future cross-file resolver; `oxabl_analyze` aggregate dump refuses to emit across
  mismatched revisions.
- In v1, `oxabl analyze somedir/` analyzes each file in isolation; schema is loaded once
  and pinned at startup, so the mismatch is not reachable in v1 — but the invariant is
  recorded now so the cross-file sketch can assume it.

**C4. Platform-deterministic dump output.**

"Stable across benign refactors (enforced via golden-file diff on parsed `serde_json::Value`)"
is insufficient: `serde_json::Value` equality is **order-insensitive for objects**, so
goldens won't catch `FxHashMap`-iteration-order drift between machines or insertion-order
changes. Revise Phase 6 to mandate:

- All array sections (`scopes`, `symbols`, `references`, `types`, `diagnostics`) emit in
  sorted order by deterministic key (`ScopeId`/`SymbolId`/`NodeId` ascending; diagnostics
  by `(file, start, code)`).
- Any `FxHashMap` traversed at serialization boundary collects into `Vec` and sorts.
- Golden diff runs **byte-level** (`assert_eq!(actual_str, expected_str)`), not
  `serde_json::Value` equality.

**C5. `Resolution::Unresolved` needs the identifier name for diagnostics.**

Current shape is `Unresolved { reason: UnresolvedReason }`. Every diagnostic site
(`LINT0001`) has to re-slice from the source span with `AnalysisContext`. Record the atom
once at resolve time:

```rust
pub enum Resolution {
    Resolved(SymbolId),
    Unresolved { name: OxablAtom, reason: UnresolvedReason },
}
```

Adds ~8 bytes to the unresolved variant; eliminates a source reslice from every
`SEM0001`/`LINT0001` emission. Matches Ruff's `UnresolvedReference` shape (carries
`range` + identifier; `ruff_python_semantic::reference`).

**C6. `Symbol::data_type` — replace `Option<ResolvedType>` with `ResolvedType`.**

Two representations for "don't know" (`Option::None` vs `ResolvedType::Unknown`) is a
footgun. Collapse to one: `data_type: ResolvedType` defaulting to
`ResolvedType::Unknown`. `None` was never meaningful — if a declaration's type is
unknowable (cross-file parent class, error recovery), `Unknown` is the answer, and the
lattice already handles it.

**C7. `read_count`/`write_count` idempotence.**

Plan specifies `+= 1` on each resolving reference. This breaks idempotence — Salsa
re-runs queries; tests re-run the pass; partial reanalysis (LSP future) double-counts.
Revise Phase 4a to compute counts once at end-of-resolve from a local accumulator:

```rust
let mut counts: FxHashMap<SymbolId, (u32, u32)> = FxHashMap::default();
// ... during walk: counts.entry(sym).or_default().0 += 1; // reads
for (sym, (r, w)) in counts {
    symbols[sym].read_count = r;
    symbols[sym].write_count = w;
}
```

Salsa-ready (Enhancement Summary commitment) requires idempotent passes; this is a hard
precondition.

### Security / Hardening (pre-merge)

**S1. Workspace-root containment for schema paths.** `workspace.schema.files` entries
must resolve relative to the workspace root and reject `..` traversals and absolute
prefixes. Mirror Cargo's policy for `include`. `--schema <path>` from the CLI is user-
initiated and may be absolute. Emit `SCHEMA0030` ("path escapes workspace root") on
violation.

**S2. ANSI escape stripping in `--format text` output.** Identifier names sliced from
source and filenames can contain `\x1b[…]` sequences. When piped to terminals, these
inject cursor moves or screen clears. In `oxabl_analyze::dump_text` and the lint text
renderer, strip C0 controls (except `\t` and `\n`) from all user-sourced strings before
emission. JSON mode is safe via `serde_json` escaping — keep JSON as the stability
contract.

**S3. `OxablAtom` unbounded-pool property.** The atom pool is process-global and never
freed. Malicious/huge `.df` can grow it until OOM. For v1 CLI (one-shot process), this
is cold — document the property. For future LSP, it becomes a real leak and must be
addressed then. Add to `docs/design/semantic-v1-cross-file-sketch.md` as a noted LSP
concern, and add a soft cap at schema load: reject `.df` with > 100k tables or > 10k
fields per table via `SCHEMA0031`.

**S4. Preprocessor include-depth cap precondition.** `analyze` is the first CLI surface
users will point at untrusted corpora. The semantic layer relies on `oxabl_preprocessor`;
if that crate lacks an include-depth limit and cycle detection, a crafted `.p` with
`{a.i}` referencing itself stack-overflows before semantic runs. **Pre-merge gate:**
confirm `oxabl_preprocessor` has a depth cap (default ~64) and cycle detection, or file a
blocker issue against this plan.

**S5. `SchemaRevision` wraparound.** Monotonic `u32` — 4.2B reloads is not a real threat.
Add `debug_assert!` on increment; document in the Phase 0 invariants doc.

### Flow Gaps — Behavior Decisions Needed

Spec-flow analyzer surfaced these; each needs a one-line resolution written into the
plan before Phase 3:

| # | Flow                                                 | Resolution (proposed)                                                                 |
|---|------------------------------------------------------|---------------------------------------------------------------------------------------|
| F1 | `oxabl analyze` given a directory                    | Walk respects `.gitignore`; `.p`, `.cls`, `.w` analyzed standalone; `.i` skipped by default (`--include-fragments` forces); aggregate JSON wraps per-file outputs in `{"files": [...]}`. |
| F2 | `.df` with zero tables (blank/comment-only)          | `schema_loaded = !Schema::is_empty()`; empty `.df` keeps `LINT0003` silent.           |
| F3 | `--schema <path>` file missing / unreadable          | `SCHEMA0001` diagnostic; `schema_loaded = false`; exit code 2.                        |
| F4 | `--no-lint` flag                                     | Suppresses `lint::lint_file` entirely; `SEM###`, `SCHEMA###`, `PARSE###`, `PREPROC###` still emit. |
| F5 | NodeId stability under error recovery                | Recovery-generated `Statement::Empty` gets a NodeId like any other; `types`/`references` at that NodeId are always `None`. Document in Phase 0 AST invariants. |
| F6 | Include-file redeclaration collision                 | First-declared wins; second emits `SEM0001` with both spans (include span + caller span via `PreprocessedFile::resolve`). |
| F7 | Single-file `CLASS Sub INHERITS Super` where `Super` missing | `Unresolved { reason: External }`; `Sub`'s override checks (`SEM0010`) suppress silently. |
| F8 | `DEF VAR SESSION AS HANDLE` (builtin shadowing)      | User declaration shadows builtin in declaring scope; no diagnostic (ABL permits this); rebinding stored as a normal shadow. |
| F9 | CATCH variable whose class isn't `Progress.Lang.Error` | v1 accepts any class; flag `SEM0011` ("catch-type-not-progress-error") as v1.x.      |
| F10 | Getter-only `DEFINE PROPERTY` on a class that `IMPLEMENTS` an interface | Skip `LINT0002` — interface is an external contract, mirrors `SHARED` skip.          |
| F11 | Binary file misnamed `.p`                           | Lexer `PARSE###` errors; semantic runs anyway against whatever recovery produces; exit code 1. |
| F12 | UTF-8 BOM at file start                             | Strip at `FileSystem::read` (Phase 2 addition to `.df` loader too).                   |
| F13 | Windows CRLF line endings                           | Normalize at read (existing lexer convention; confirm `.df` parser matches).          |
| F14 | Longchar literal > 32KB                             | Parse OK; type is `Longchar`; `LINT0004 Warning` only fires at Longchar→Character assignment, not at the literal. |

### Simplicity-vs-Integrity Tensions (decide explicitly)

The simplicity reviewer proposed cutting items that the architect and data-integrity
reviewers consider load-bearing. These are genuine trade-offs, not oversights — record
the decision rationale before Phase 3.

| Item                                 | Simplicity says                                                                             | Integrity/Architect says                                                                                                    | Recommendation |
|--------------------------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------|----------------|
| `SchemaRevision`                     | Cut. Pure v2 incremental sugar; `TableId` alone is enough for v1.                           | Keep. Composite-key `(SchemaRevision, TableId)` is required once we admit the schema can change between analyze calls — even in v1 the long-running `oxabl_analyze` over a directory will want it to detect a silent reload race. | **Keep, but hide: make `SchemaRevision` private to `oxabl_schema`, expose it via opaque token methods.** Avoids "v2 sugar" accusation while preserving invalidation contract. |
| `VirtualSpan` newtype                | Cut. No `FileSpan` in semantic layer yet; shim for a non-problem.                           | Keep. Prevents misuse at dump/emission boundaries even now.                                                                 | **Keep.** Cost is one struct + helper; benefit is compile-time prevention of a class of span-confusion bugs. Adopt rust-analyzer's `InFile<HirFileId>` discriminated-union variant for stronger typing (see §Framework Precedent). |
| `assignable` + `assignable_strict`   | Merge into `assignable(from, to, mode: AssignMode)`.                                        | Two functions allow different type signatures for doc/readability.                                                          | **Merge.** One function, `AssignMode::{Lenient, Strict}`. Colocates the widening ladder walk; matches Pyright/TS convention. |
| Three passes                         | Collapse to two (declare+resolve fused; check separate).                                    | Three passes map 1:1 to three future Salsa queries; forward-reference correctness is easier to reason about.                | **Compromise: one walker, three `Pass` dispatches.** Oxc uses one walker with an `unresolved_stack` for forward refs — cite this. Keeps the public API three-query-shaped while avoiding 3× tree walks. See §Performance. |
| `rebinding_scopes` side map          | Cut. SHARED is cross-file anyway; v1 is single-file; `SymbolFlags::Shared` is enough for LINT0002 skip. | Keep. Groundwork for R10 cross-file; Ruff needs it for Python's `global`/`nonlocal`.                                        | **Cut in v1; reintroduce at R10.** The symmetric `SymbolTable.symbols: IndexVec<SymbolId, Symbol>` is cleaner without it. `SymbolFlags::Shared` suffices for the unused-variable skip. |
| `BindingMap { Small, Large }` enum   | Cut. No measurement. Ship plain `FxHashMap<OxablAtom, SymbolId>` per namespace.             | Keep, but with inline-size 16 pending corpus measurement (pointer-equality atom compare is faster than hash up to ~16).     | **Keep as a private repr; expose via `BindingMap::get(&self, name)` method only.** Don't leak the enum variant. Tune inline size after Phase 5 corpus data. |
| `oxabl_analyze` separate crate       | Cut. Use `serde_json` feature flag on `oxabl_semantic` (`default-features = false`).        | Keep. Defensive against first contributor to reach for `#[derive(Serialize)]` on `oxabl_common::Diagnostic` and drag serde into the base. | **Keep, but minimize:** `oxabl_analyze` owns a serializable mirror type `DiagnosticDump` + `SemanticDump`, converts at serialization time. Zero `serde*` dep in `oxabl_semantic` or `oxabl_common`. |
| Per-section versioning envelope      | Cut to single `version: 1`.                                                                 | Keep; lets independent consumers re-baseline only affected sections.                                                        | **Cut for v1; keep envelope shape `{version: 1, sections: {…}}` with all sections on version 1.** Per-section bumping is YAGNI until a second consumer exists; the shape reservation is free. |
| Phase 0 AST invariants doc           | Keep, but shrink — don't cross-check every invariant against parser code.                   | Keep full.                                                                                                                  | **Shrink.** Enumerate invariants; add `debug_assert!`s opportunistically, not exhaustively. 0.5 day stays realistic. |

### Type-Signature Cleanups (apply during Phase 3/4)

From the Rust-quality review:

- `NodeId(u32)` → field already private. **`SchemaRevision(pub u32)` → private u32 with
  `SchemaRevision::new(u32)` `pub(crate)` constructor.** Prevents fabrication.
- `BindingMap` → private repr, pub methods only (`get` / `iter` / `len`).
- `IndexVec<NodeId, Option<T>>` side tables → wrap in `References` / `TypeTable` newtypes
  with `get(NodeId) -> Option<&T>` that does the double-unwrap internally.
- `SymbolTable` → hide `symbols` and `rebinding_scopes` (if retained) behind `get(id)` /
  `rebindings(id) -> &[ScopeId]`. Asymmetry stops at the API boundary.
- `analyze_file` infallibility → document explicitly in `Simplifications accepted`:
  **"`analyze_file` is infallible by contract; all user-visible issues are `Diagnostic`s;
  all internal invariant violations panic with `TYPE###` codes."**
- Lifetime pivot: `AnalysisContext<'a>` is fine for CLI; note as a known refactor point
  when LSP arrives (`Arc<PreprocessedFile>`).

### Performance Revisions

**P1. Tighten `analyze_file` target.** "≤ 3× parse time" is a ceiling (regression gate),
not a goal. Target **≤ 1.5× parse time** end-to-end. Rationale: parse is ~5-20 ms/file
on the ABL corpus; three naive tree walks alone consume 30-100 ms/file before any
semantic work. 3× is nominal; 1.5× forces the single-walker-multi-pass optimization up
front.

**P2. Single walker, multi-pass dispatch.** Replace "three sequential walks" with one
traversal that invokes each pass's visitor:

```rust
for pass in [&mut declare, &mut resolve, &mut check] {
    pass.enter(node);
}
// recurse...
for pass in [&mut declare, &mut resolve, &mut check].iter().rev() {
    pass.leave(node);
}
```

Public API remains `analyze_file` (three logical queries); implementation is one walk.
Matches Oxc's single-walker + `unresolved_stack` deferred-resolution pattern. Phase 4
split stays in the plan for test / doc boundaries; the walker is shared.

**P3. `ResolvedType` size budget.** Add a compile-time assertion:

```rust
const _: () = assert!(std::mem::size_of::<Option<ResolvedType>>() <= 16);
```

Load-bearing for the `IndexVec` side table to not blow cache. If `ResolvedType::Class`
or `Table` balloons past this, move the large variant behind `Box` or a side arena.

**P4. `SideTable<T>` abstraction.** Define a trait/newtype at the start of Phase 3 so
the plan isn't locked into `IndexVec` vs `FxHashMap` across all future passes (control
flow, effects, shape). One place to swap representation per side table after measurement.

**P5. `BindingMap` inline size.** Start at `SmallVec<[_; 8]>` (conservative); after
Phase 5 corpus audit, measure the distribution of binding counts per namespace per scope
and raise to 16 if the tail is long. `OxablAtom` compares are pointer-equality after
intern, so linear-scan beats hashmap up to 16-24 entries.

**P6. `rebinding_scopes` value type.** If retained (see Simplicity decision), use
`SmallVec<[ScopeId; 2]>` — rebinding counts are 1–2 in the common case.

**P7. Additional micro-benchmarks.** Add to Phase 8:
- Scope-chain climb (pathological deep-nested `CLASS → METHOD → DO → DO → FOR EACH`).
- Coercion-table lookup (arithmetic-heavy fixture).
- `SmallVec → FxHashMap` spill (synthetic 500-binding procedure).
- Schema-bench target restated as **throughput ≥ 50 MB/s** (plus absolute ms for 5 MB
  golden) since real `.df` sizes vary 5×.

**P8. Per-file memory budget.** At ~500k nodes × 16B per `Option<ResolvedType>` entry,
semantic alone can reach ~8 MB/file on the dense-NodeId side tables. A corpus-wide
`oxabl analyze` over 1000 files without explicit `Drop` releases will blow RSS. Document:
**"`Semantic` must be `Drop`-cheap and must not retain `&Program` beyond
`analyze_file`'s return."** Add RSS bench over the corpus to Phase 8.

### Bidirectional Typing — Refinements

**T1. Introduce `ResolvedType::Error` distinct from `ResolvedType::Unknown`.**

The current plan conflates two things into `Unknown`: ABL's `?` (universal bottom,
propagates through arithmetic, expected at runtime) and "analysis couldn't reach here"
(inference gap, diagnostic-worthy). Follow rustc (`TyKind::Error`) and Pyright (`Unknown`
vs `Any`) — split:

- `ResolvedType::Unknown` — ABL's `?`. User-authored. Propagates silently.
- `ResolvedType::Error` — checker-produced poison. Always paired with a diagnostic at
  its origin. Compatible with everything; suppresses all downstream diagnostics that
  mention it.

Rule: *if any operand is `Error`, produce `Error` and emit nothing* (so one root-cause
diagnostic, not a cascade).

**T2. Explicit `synth`/`check` split in `check.rs`.**

Bidirectional typing's recurring bug class is blurring synthesis vs checking at
elimination forms. Make it syntactic:

```rust
fn synth(expr: &Expression, ctx: &mut CheckCtx) -> ResolvedType { … }
fn check(expr: &Expression, expected: &ResolvedType, ctx: &mut CheckCtx) { … }
```

Call `check` whenever an expected type exists (assignment RHS, parameter site, return
expr, IF condition narrowed to `Logical`). Never `synth`-then-compare.

**T3. Operator typing table shape.** Dispatch by operator first, then by operand pair,
with a shared fallthrough:

```rust
fn binop_result(op: BinOp, l: ResolvedType, r: ResolvedType) -> Option<ResolvedType> {
    match op {
        BinOp::Divide => Some(ResolvedType::Primitive(PrimitiveTy::Decimal)),
        BinOp::Add if is_date(&l) && is_integer(&r) => Some(l),
        BinOp::Add if is_datetime(&l) && is_integer(&r) => Some(l),
        _ => numeric_widen(l, r),
    }
}
```

ABL quirks at the top; shared widening ladder at the bottom. Mirrors rustc's `BinOp`
lowering and TypeScript's `getBinaryOperatorType`.

**T4. Coercion table repr.** Stay with hand-written tables while type count is < ~40.
Migrate to `phf::Map` generated by `oxabl_codegen` only if type count grows.

**T5. `Unknown` is bottom, not TypeScript's `unknown` / `any`.** Document this
distinction in `docs/design/semantic-v1-api.md`. The difference matters once
flow-sensitivity lands: `IF x = ? THEN … ELSE <here x is not unknown>` requires
`Unknown` to be a *refinable* type, which `Error` must never be.

### Framework Precedent — What to Cite

Updated citations (2026 source, specific SHAs available in the framework-docs research
dispatch):

| Decision                                  | Precedent                                                                                 |
|-------------------------------------------|-------------------------------------------------------------------------------------------|
| `IndexVec<NodeId, Option<T>>` side tables | [`oxc_semantic::Scoping`](https://github.com/oxc-project/oxc/tree/main/crates/oxc_semantic/src/scoping.rs); [`ruff_python_semantic`](https://github.com/astral-sh/ruff/tree/main/crates/ruff_python_semantic). Biome diverges (range-keyed hashmap). |
| Virtual vs file spans                     | rust-analyzer's `InFile<HirFileId>` discriminated union ([`crates/hir-expand/src/files.rs`](https://github.com/rust-lang/rust-analyzer/blob/master/crates/hir-expand/src/files.rs)). Only prior art for preprocessor-expanded spans — Oxc/Biome/Ruff have no analog. |
| Resolution with structured reason         | **Our three-case `UnresolvedReason` is richer than any precedent.** Ruff's `UnresolvedReferenceFlags` is a single-bit `bitflags`. Oxc/Biome carry no reason at all. Treat as contribution, not convergence. |
| Single-pass with deferred resolution      | [`oxc_semantic::unresolved_stack`](https://github.com/oxc-project/oxc/tree/main/crates/oxc_semantic/src/unresolved_stack.rs) — how Oxc handles forward refs in one walk. Relevant to §P2 single-walker-multi-pass design. |
| NodeId on every AST node                  | Oxc, Biome, Ruff all use dense ID keying; rust-analyzer uses `AstPtr<N>` + `FileAstId<N>` keyed by `HirFileId`. Our choice aligns with the former — document the divergence from rust-analyzer. |
| Testing (inline `assert_eq!` over insta)  | `biome_js_semantic/src/semantic_model/tests.rs`; `ruff_python_semantic` per-module inline asserts. Keep our convention. |
| Incrementality deferred                   | Oxc, Biome, Ruff all shipped non-incremental v1s. rust-analyzer, the only one that started with Salsa, took years of rewrites. Cite this as explicit validation for not starting with Salsa. |

### Spec-Flow Terminology Cleanup

- **Rename `UnresolvedReason::External` → `UnresolvedReason::OutOfUnit`** (or
  `CrossUnit`) to avoid collision with ABL's own `EXTERNAL` method modifier (used in
  Phase 5 skip list for the `unused-variable` rule). Two meanings of "External" in the
  same codebase is a readability bomb.
- **Document the `customer` resolution case** (variable vs buffer vs schema-table)
  explicitly: namespace resolution-order table (line ~352) must state which namespace
  wins for a bare identifier before `.field` access. Proposed: `Values → Buffers →
  Tables` within the referencing scope; qualified `table.field` (with a `.`) narrows to
  `Buffers/Tables` directly.
- **`SchemaType` vs `ResolvedType::Primitive` vs `PrimitiveTy` vs `DataType` from
  `oxabl_ast`** — four type enums. Add a conversion-table module doc stating how each
  maps to the next. Proposed ownership: `DataType` (parser) → `PrimitiveTy` (semantic
  normalized) → `ResolvedType::Primitive` (after inference). `SchemaType` is
  schema-specific and converts to `PrimitiveTy` for field access.

### Acceptance-Criteria Fixes

The code reviewer caught un-measurable gates:

- "meaningfully few false positives" → **replace with "≥ 0.9 precision per rule on the
  100-file manual audit sample defined in Phase 5."**
- "stable across benign refactors" → **replace with "byte-level golden diff passes."**
  "Benign" is undefinable; the diff does the defining.
- "corpus coverage ≥ the parser's current success rate" → **record parser's current
  success rate as a concrete number in the plan (Phase 8 measurement); use that number.**
- "grows data-driven from the Phase 5 corpus audit" → **state the threshold: if a
  candidate builtin triggers `LINT0001` on > N files in the audit, it is added.** Pick
  N = 5 as provisional.
- "100-file manual audit sample" → **specify the adjudication process: the plan author
  labels; per-rule disagreement with the corpus maintainer is a plan-merge blocker.**

### Coercion-Catalog Spike — Gate Explicitly

The code reviewer's single weakest point: the 1-day coercion spike has no gate.
**Before Phase 4b begins, the spike artifact must be committed to the repo as
`docs/design/semantic-v1-coercion-catalog.md` and reviewed.** Phase 4b's tests cite it
by anchor. If the spike discovers unresolvable ambiguity (Progress docs and FWD
disagree), the ambiguous cells are explicitly `Unknown`-returning in v1 and logged in
`docs/design/semantic-v1-limitations.md`. `LINT0004`'s ≥ 0.9 precision target is then
scoped to the resolved cells only.

### Learnings Folded In

From `docs/solutions/performance-issues/heap-allocation-in-keyword-matching.md`:
"Never use `to_lowercase()`/`to_uppercase()` on hot paths; push classification as far
upstream as possible." Already reflected in `OxablAtom` unification (Enhancement #4);
cite inline in §`OxablAtom` where the stack-buffered ASCII-fold path is referenced, to
anchor the claim to the measured PR #19 (~20%) result.

From memory `feedback_corpus_validity.md`: "When diagnosing corpus failures, assume the
source code is correct." Add a one-line reminder at the Phase 5 `corpus_lint_audit`
step: **a lint false positive on the ABL corpus is a rule bug until proven otherwise.**

### Revised Effort Estimate

Simplicity-vs-integrity decisions above reduce scope modestly. Updated table:

| Phase                         | Effort  | Delta |
|-------------------------------|---------|-------|
| 0 — AST invariants doc        | 0.5d    |       |
| 1 — NodeId on AST             | 1d      |       |
| 2 — `oxabl_schema` + C1 spike | 2.5d    | +0.5d (OxablAtom spike, SCHEMA0012 field conflict) |
| 3 — Declare pass              | 3d      |       |
| 4a — Resolve pass             | 2.5d    | +0.5d (Resolution name; idempotent counts) |
| 4b — Type-check pass          | 3.5d    | +0.5d (Error/Unknown split; synth/check discipline) |
| 5 — `oxabl_lint`              | 3d      |       |
| 6 — `oxabl_analyze` + CLI     | 2.5d    | +0.5d (sort-at-boundary; byte-diff goldens; ANSI strip) |
| 7 — Guardrail appendices      | 0.5d    |       |
| 8 — Benches / CI / release    | 1.5d    | +0.5d (per-rule and throughput benches) |
| **Total**                     | **~20d**| +2d from Round-2 deepening |

### Summary of Pre-Phase-3 Decisions Required

The author must record a decision on each before Phase 3 starts:

1. `OxablAtom` spike outcome → keep unified regime, or adopt `lasso::Spur`.
2. `SchemaRevision` visibility → private with opaque token, or full `pub`.
3. `VirtualSpan` newtype shape → simple newtype, or rust-analyzer-style
   `InFile<OxablFileId>` discriminated union.
4. `rebinding_scopes` → keep in v1, or defer to R10.
5. `BindingMap` → private repr + public methods (recommended), or inline.
6. `oxabl_analyze` crate split → keep with serializable mirror types (recommended), or
   collapse with feature flag.
7. Per-section versioning envelope → keep shape with v1 uniform (recommended), or
   collapse to single version.
8. Three-pass walker → single traversal, multi-pass dispatch (recommended), or three
   sequential walks.
9. `ResolvedType::Error` vs `Unknown` → split (recommended), or keep unified.
10. `Resolution::Unresolved { name, reason }` → carry identifier (recommended), or
    reslice from span.
11. `Symbol::data_type: ResolvedType` default `Unknown` (recommended), or keep
    `Option<ResolvedType>`.
12. `read_count`/`write_count` idempotence → end-of-resolve accumulator (recommended),
    or `+= 1` incremental.
13. `UnresolvedReason::External` rename → `OutOfUnit` (recommended), or keep `External`
    with naming-collision note.

### New Primary Sources

- [Oxc `oxc_semantic`](https://github.com/oxc-project/oxc/tree/main/crates/oxc_semantic) —
  `Scoping`, `IndexVec`, `unresolved_stack`.
- [Biome `biome_js_semantic::model`](https://github.com/biomejs/biome/tree/main/crates/biome_js_semantic/src/semantic_model/model.rs) —
  range-keyed hashmap + `rust_lapper::Lapper` interval tree.
- [Ruff `ruff_python_semantic::reference`](https://github.com/astral-sh/ruff/tree/main/crates/ruff_python_semantic/src/reference.rs) —
  `UnresolvedReference` shape.
- [rust-analyzer `hir-expand::files`](https://github.com/rust-lang/rust-analyzer/blob/master/crates/hir-expand/src/files.rs) —
  `InFile<HirFileId>` pattern.
- [Ruff "Red Knot" / ty type-checker](https://github.com/astral-sh/ruff/tree/main/crates/ty_python_semantic) —
  Salsa-based single-file-first type checker; direct analog for a later Salsa lift.
- [`phf`](https://docs.rs/phf/) — for the coercion-table migration target at ~40+ types.
- [`lasso`](https://docs.rs/lasso/) — fallback interner if `OxablAtom` is keyword-closed.
- Pyright type-concepts docs — `Unknown` vs `Any` distinction.
- rustc `TyKind::Error` — poison-type propagation pattern.
- Dunfield & Krishnaswami, *Bidirectional Typing*, ACM CSUR 2021 (arXiv 1908.05839).
