---
title: "feat: Semantic Layer v1 — Symbols, Scopes, Types, Schema, and Proof-Point Linter"
type: feat
status: active
date: 2026-04-16
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
15. **`.df` grammar sourced from sonar-openedge's `DumpFileGrammar.g4`** (best-practices research): production-tested Riverside ANTLR4 grammar. Test against `sp2k.df` golden then pcna-erp. Open attribute sets (warn-don't-fail on unknowns). Support `""` embedded quotes, `#` line comments from hand-edited files, `PSC`/`cpstream`/trailer, `?` as unknown-value marker.
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
oxabl_lint         ← new: rule engine + 4 v1 rules (consumes oxabl_semantic)
oxabl               ← extended: `analyze` subcommand; existing `check` unchanged
```

The core architectural commitment, informed by rust-analyzer, Oxc, Biome, Ruff, and Roslyn
consensus, is **side tables over the AST keyed by `NodeId`** — not in-place AST mutation, not a
separate HIR lowering. Side tables preserve the upgrade path to Salsa-style incrementality and
cross-file analysis without rewriting the AST.

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
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NodeId(u32);
impl NodeId { pub const DUMMY: NodeId = NodeId(u32::MAX); }
```

Every `Statement` and `Expression` variant carries `id: NodeId`. Assigned in the parser via a
monotonic counter on the `Parser` struct. Zero cost at build time; enables every side-table
keyed on it. AST mutation is not required — `NodeId` is assigned once during parse and never
changes. See `oxabl_ast/src/statement.rs` and `expression.rs` for target types.

### oxabl_schema (new crate)

Dedicated crate because it has exactly one job (parse `.df` → `Schema`), is reused by
semantic + every later consumer (formatter, LSP, codemods), and has an independent test surface.

**Scope of `.df` support in v1:** the common subset observed in the pcna-erp corpus —
`ADD TABLE "..."`, `ADD FIELD "..." OF "..." AS <datatype>`, `ADD INDEX "..." ON "..."`, and
their associated attribute lines (`FORMAT`, `INITIAL`, `LABEL`, `POSITION`, `MAX-WIDTH`, `ORDER`,
`MANDATORY`, `CASE-SENSITIVE`, `HELP`, `VALEXP`, `VALMSG`, `DECIMALS`, `EXTENT`, `UNIQUE`,
`PRIMARY`, `WORD`, etc.). Unknown attributes round-trip as opaque key/value lines — no hard error
on format drift. No writing, no migration tooling.

```rust
// oxabl_schema/src/schema.rs
pub struct Schema { tables: FxHashMap<AsciiCaseName, Table>, /* ... */ }
pub struct Table {
    pub name: AsciiCaseName,
    pub fields: Vec<Field>,       // source order for stable dumps
    pub indexes: Vec<Index>,
    pub source: FileSpan,
}
pub struct Field {
    pub name: AsciiCaseName,
    pub data_type: SchemaType,    // { Integer, Int64, Decimal, Character, Logical,
                                  //   Date, Datetime, DatetimeTz, Handle, Raw,
                                  //   Recid, Rowid, Blob, Clob }
    pub extent: Option<u32>,
    pub mandatory: bool,
    pub format: Option<String>,
    // ...
    pub source: FileSpan,
}
pub struct AsciiCaseName(SmolStr);   // stores lowered bytes; PartialEq/Hash case-insensitive
```

**`AsciiCaseName`** is the case-folding primitive for the whole semantic layer — interned
lowercase (ASCII fold only; no heap alloc per compare, per CLAUDE.md guidance). Reused by
`oxabl_semantic` for identifier keys.

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
    pub references: FxHashMap<NodeId, Resolution>,
    pub types: FxHashMap<NodeId, ResolvedType>,     // expressions + declarations
    pub diagnostics: Vec<Diagnostic>,
}
```

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
    pub bindings: FxHashMap<(NamespaceId, AsciiCaseName), SymbolId>,
}
```

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
    pub name: AsciiCaseName,
    pub display_name: SmolStr,        // original-case for diagnostics
    pub namespace: NamespaceId,
    pub kind: SymbolKind,
    pub declared_in: ScopeId,
    pub declaration: NodeId,          // AST node where declared
    pub data_type: Option<ResolvedType>,  // computed during type pass
    pub read_count: u32,              // incremented on each resolving reference
    pub write_count: u32,
    pub flags: SymbolFlags,           // NoUndo, Static, Parameter{In,Out,InOut}, Shared, …
}
pub enum SymbolKind {
    Variable, Parameter, Property, Field,
    TempTable, Buffer, Stream, Frame, Event,
    Procedure, Function,
    Class, Interface,
    BuiltIn,    // system handles (SESSION, ERROR-STATUS, THIS-OBJECT, SUPER, …)
}
```

BuiltIn symbols (SESSION, ERROR-STATUS, THIS-OBJECT, SUPER, a short list of always-resolved
handles) are seeded into the file's root scope so reads of them never trigger
`undefined-symbol`. List lives in `oxabl_semantic/src/builtins.rs` — a table of maybe 20–30
entries for v1, extensible without breaking changes.

#### Resolution

```rust
pub enum Resolution {
    Resolved(SymbolId),
    Unresolved { reason: UnresolvedReason },
}
pub enum UnresolvedReason {
    NotInScope,
    CrossFile,                 // USING-imported, RUN "other.p", etc — v1 does not chase
    Dynamic,                   // RUN VALUE(x), DYNAMIC-FUNCTION, dynamic buffer ops
    Preprocessor,              // identifier produced by unresolved {&name}
    NoSchema,                  // buffer/field reference; schema not loaded
}
```

Returning structured reasons (not booleans) is what lets `unknown-table-or-field` suppress
cleanly when schema is absent, and what lets later cross-file work replace `CrossFile` entries
in place without IR churn.

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
    Table(AsciiCaseName),           // schema-table-typed expression
    Array { element: Box<ResolvedType>, extent: Option<u32> },
    Unknown,                        // ? / truly unknown — lattice bottom, compatible with all
    Error,                          // previous error prevented inference — don't cascade
}
```

**`Unknown` is the lattice's bottom.** It is assignable to and from every type (matches ABL
`?`). `Error` is a poison value: it suppresses further diagnostics on dependent nodes to avoid
cascade. Addresses spec-flow gap on `?` propagation.

**Bidirectional checking, not Hindley–Milner.** Literals synthesize a type; declarations carry
a type from their `DataType`; assignments check RHS against LHS via `assignable(from, to)`.
No constraint solver, no unification; operator typing tables drive binary ops. Follows ruff/oxc
pattern for dynamic languages.

**Coercion catalog (`crates/oxabl_semantic/src/coercion.rs`)** — the v1 coercion rules, derived
from the Progress documentation referenced by `oxabl_codegen` plus confirmations from the
pcna-erp corpus. Indicative, not exhaustive:

- `Integer ⟶ Int64 ⟶ Decimal` (widening, implicit).
- `Logical ⟷ Character` on output-only contexts (DISPLAY). Not assignment.
- `Date ⟶ Datetime ⟶ DatetimeTz` (widening).
- `Handle` is nominal; no silent coercion across handle kinds.
- `Unknown` ⟷ T for every T.
- Class assignment: only when `to` is a superclass/interface of `from` — single-file chain
  only in v1; cross-file chain yields `Unknown` (no false `type-mismatch`) with an
  `UnresolvedReason::CrossFile` note so rules don't cascade.

This catalog is a v1 best-effort; its correctness is part of the research spike (see
"Dependencies & Assumptions" below).

#### Two passes, not one

The analyzer runs the passes in sequence:

1. **Scope+declare**: walk statements building `ScopeTree` and inserting `Symbol`s into scopes.
2. **Resolve+type**: walk expressions and references, populating `references` and `types`.

Two passes because (a) forward references inside a scope (method-before-declaration,
function-before-declaration) need all declarations visible first, and (b) it maps cleanly to
two future Salsa queries. One unified pass closes that door.

### oxabl_lint (new crate)

Separate crate from `oxabl_semantic` so the semantic model can be used independently (dump
tool, future LSP, future formatter) without dragging lint rules into their dependency graph.

```rust
// oxabl_lint/src/lib.rs
pub fn lint_file(program: &Program, sem: &Semantic, ctx: &LintContext) -> Vec<Diagnostic>;

pub struct LintContext<'a> {
    pub file_id: FileId,
    pub preprocessed: &'a PreprocessedFile,
    pub schema_loaded: bool,
    pub enabled: RuleSet,          // default = all v1 rules
}
```

No visitor framework in v1. Each rule is a function `fn(program, sem, ctx) -> Vec<Diagnostic>`
that walks what it needs. Four functions are cheap; a framework is the premature abstraction.
A `Rule` trait can be introduced later without API break.

#### v1 rule definitions (resolving spec-flow gaps)

**`undefined-symbol` (LNT0001, error).** Fires on `Resolution::Unresolved { reason: NotInScope }`
references in the `Values`, `Procedures`, `Functions`, `Streams`, `Frames`, `Events`, `Types`
namespaces. *Does not* fire on `CrossFile`, `Dynamic`, `Preprocessor`, or `NoSchema` reasons —
those are by-design unresolved.

**`unused-variable` (LNT0002, warning).** Fires on `SymbolKind::{Variable, Parameter}` with
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

**`unknown-table-or-field` (LNT0003, error).** Fires on buffer, field, or qualified
`table.field` references that resolve to `Unresolved { reason: NoSchema }` *only when*
`ctx.schema_loaded == true`. When schema is absent, rule emits zero diagnostics regardless of
references — matches R7. Partial schema: rule fires for tables/fields that *should* be in
loaded scope; user's responsibility to load complete schema.

**`type-mismatch-assignment` (LNT0004, error).** Fires when `assignable(rhs_ty, lhs_ty)` returns
`false`, skipping:
- Either side is `ResolvedType::Unknown` or `Error`.
- Either side involves an `Unresolved { reason: CrossFile | Dynamic | Preprocessor }`
  operand — avoid false positives when analysis couldn't reach the type.

Only direct assignments (`x = expr`, `ASSIGN x = expr`, initial values) in v1. Function
argument passing, `RETURN` coercion, `BUFFER-COPY`, dynamic `::` set are documented as v1.x
extensions.

#### Severity

| Rule                         | Severity |
|------------------------------|----------|
| `undefined-symbol`           | Error    |
| `unused-variable`            | Warning  |
| `unknown-table-or-field`     | Error    |
| `type-mismatch-assignment`   | Error    |

#### Suppression

Deferred. No `// noqa` mechanism in v1. Acknowledged in a follow-up doc, not a TODO in code.

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
  "oxabl_analyze_version": 1,
  "file": "path/to/file.p",
  "scopes": [ { "id": 0, "kind": "File", "parent": null, "bindings": [...] }, ... ],
  "symbols": [ { "id": 0, "name": "customer", "kind": "Buffer", "scope": 1, "type": "Buffer(schema:customer)", "reads": 3, "writes": 1, "span": {...} }, ... ],
  "references": [ { "node": 42, "resolution": {"kind": "Resolved", "symbol": 0}, "span": {...} }, ... ],
  "types": [ { "node": 17, "type": "Integer" }, ... ],
  "diagnostics": [ { "code": "SEM0001", "severity": "Error", "message": "...", "span": {...} }, ... ]
}
```

`oxabl_analyze_version: 1` is the stability contract — non-additive changes bump the integer
and require golden re-baselining, additive fields do not. Text format is pretty-printed for
humans and is *explicitly* not stability-guaranteed; goldens run against JSON.

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

### Phase 2 — `oxabl_schema` crate

**Goal:** load `.df` files into a `Schema` with diagnostics. No integration with semantic yet.

Tasks:

- New `crates/oxabl_schema/` workspace member.
- `schema.rs`: types (`Schema`, `Table`, `Field`, `Index`, `SchemaType`, `AsciiCaseName`).
- `parser.rs`: a dedicated `.df` parser (line-oriented; not reusing the ABL lexer — format is
  not ABL). Tokenizes `ADD TABLE "name"`, indented attribute lines, `ADD FIELD`, `ADD INDEX`.
  Unknown attributes captured as opaque strings.
- `loader.rs`: `SchemaLoader::load_files(paths, &dyn FileSystem)`. Merges with last-write-wins
  + `SCHEMA0010` conflict diagnostic.
- Tests: unit tests with inline `.df` strings; corpus sample `.df` fixtures in
  `crates/oxabl_schema/fixtures/` drawn from pcna-erp (2–3 representative files).
- Benchmark: `cargo bench -p oxabl_schema --bench schema_bench` on a 5 MB merged `.df` fixture.
  Target: < 100 ms load.

Deliverables: standalone crate, no other crate depends on it yet. Consumable for spike work.

Estimated effort: medium. `.df` grammar is small but attribute variety is long-tailed.

### Phase 3 — `oxabl_semantic` crate, scope & declaration pass

**Goal:** symbol table + scope tree populated for every AST construct the parser supports.
No types, no references yet.

Tasks:

- New `crates/oxabl_semantic/` workspace member.
- `scope.rs`: `ScopeId`, `ScopeTree`, `Scope`, `ScopeKind`.
- `symbol.rs`: `SymbolId`, `Symbol`, `SymbolKind`, `SymbolFlags`, `NamespaceId`.
- `declare.rs`: `Declarator` walks the `Program`, pushes scopes, inserts declarations. One
  case per statement kind the parser emits. Returns `(ScopeTree, SymbolTable)` plus
  diagnostics (e.g. duplicate declaration in the same scope → `SEM0001`).
- `builtins.rs`: seeds `THIS-OBJECT`, `SUPER`, `SESSION`, `ERROR-STATUS`, `DATASERVERS`,
  `SOURCE-PROCEDURE`, `TARGET-PROCEDURE`, `SELF`, `ACTIVE-WINDOW`, `RETURN-VALUE`, and
  ~10–20 more into the root scope of every file.
- Tests: unit tests per construct (DEFINE VARIABLE, PROCEDURE, FUNCTION, CLASS + METHOD,
  INTERFACE, DEFINE PROPERTY GET/SET, DEFINE TEMP-TABLE, DEFINE BUFFER, FOR EACH implicit
  buffer, CATCH variable, DO-counter variable, etc.). Minimum 40 tests.
- Snapshot tests against the `analyze` dump format are added in Phase 6 — this phase uses
  hand-written assertions.

Deliverables: `fn build_scope_tree(program) -> (ScopeTree, SymbolTable, Vec<Diagnostic>)`
callable and tested.

Estimated effort: large. Breadth of ABL declaration forms is the cost, not algorithmic depth.

### Phase 4 — `oxabl_semantic` crate, resolve & type pass

**Goal:** every identifier reference resolved (or structured-unresolved); every expression and
every declaration typed.

Tasks:

- `resolve.rs`: walks expressions. For each identifier reference, consults scope chain with
  namespace-narrowing rules, populates `references: Map<NodeId, Resolution>`. Handles qualified
  `table.field`, `object:member`, `array[i]`, `buffer.field`.
- `types/mod.rs`: `ResolvedType`, `PrimitiveTy`.
- `types/coercion.rs`: `assignable(from, to) -> bool` catalog.
- `types/operators.rs`: binary/unary operator typing tables (e.g., `+` on `Integer×Integer →
  Integer`, `+` on `Character×Character → Character`, `+` on `Date×Integer → Date`, etc.).
- `check.rs`: bidirectional pass. Literals synthesize; declarations provide expected types;
  assignments call `assignable`; emits `SEM0002` type-error diagnostics (but see next note).
  *Note:* the lint rule `type-mismatch-assignment` (LNT0004) is the user-facing diagnostic;
  `SEM0002` is emitted only when lint is disabled and type info is still needed (e.g., by the
  dump tool). To avoid double-reporting, `analyze_file` owns one of the two paths based on
  `ctx.emit_type_errors_as_semantic` flag (default `false` when lint runs).
- Schema integration: when `schema_loaded`, `table.field` references consult the `Schema`;
  unresolved fields become `Resolution::Unresolved { reason: NotInScope }` in the field
  namespace — picked up by `unknown-table-or-field`. When not loaded, unresolved fields become
  `reason: NoSchema`.
- Tests: 60+ unit tests across resolution forms + coercion cases.

Deliverables: `fn analyze_file(program, ctx) -> Semantic` usable end-to-end.

Estimated effort: large. The type/coercion catalog is the risky part — see spike in
Dependencies.

### Phase 5 — `oxabl_lint` crate + 4 rules

Tasks:

- New `crates/oxabl_lint/` workspace member depending on `oxabl_semantic`.
- `lib.rs`: `lint_file`, `LintContext`, `RuleSet`.
- `rules/undefined_symbol.rs`, `rules/unused_variable.rs`,
  `rules/unknown_table_or_field.rs`, `rules/type_mismatch_assignment.rs` — one per rule,
  independent.
- Each rule has a `fixtures/<rule_name>/` directory with `.p`/`.cls` inputs and
  `.expected.txt` diagnostic outputs, driven by a shared `insta`-based snapshot harness.
- Coverage targets per rule (true-positive smoke + false-positive-avoidance cases):
  - `undefined-symbol`: 10+ fixtures.
  - `unused-variable`: 15+ fixtures (OUTPUT parameter, ABSTRACT method, SHARED, property
    getter-only, etc.).
  - `unknown-table-or-field`: 10+ fixtures (with and without schema; partial schema case).
  - `type-mismatch-assignment`: 15+ fixtures (numeric widening, date widening, `?` assignment,
    class-upcast, cross-file silence).
- Corpus validation: a test binary `corpus_lint_audit` runs the four rules over a sampled
  pcna-erp subset and emits a per-rule diagnostic count + a reviewer-friendly TSV. Used to
  verify "meaningfully few false positives" (origin: Success Criteria).

Deliverables: `cargo test -p oxabl_lint` green, corpus audit output committed as a baseline
under `crates/oxabl_lint/audit/`.

Estimated effort: medium.

### Phase 6 — `oxabl analyze` subcommand + dump format + golden tests

Tasks:

- `crates/oxabl/src/main.rs`: extend `Cli` enum with `Analyze` variant. `run_analyze(path,
  format, schema_paths, includes, preprocess, no_lint) -> Result`.
- `analyze_dump` module (inside `oxabl_semantic` or new `oxabl_semantic_dump` submodule):
  `fn dump_json(sem, program) -> serde_json::Value` and `fn dump_text(sem, program) -> String`.
  Use `serde` (already a likely dep through codspeed; otherwise add).
- `insta`-powered golden tests in `crates/oxabl/tests/analyze/` with JSON snapshots.
  Fixture set covers each AST construct the parser supports (variables, functions, procedures,
  classes+methods, interfaces, properties, temp-tables, buffers, FOR EACH, CASE, CATCH,
  preprocessor-expanded code, schema-loaded and schema-absent runs). Target: 40+ golden files.
- Text-format integration test: smoke-only (output shape, not exact content).
- Diagnostic span remapping: every diagnostic in the dump resolves virtual spans via
  `PreprocessedFile::resolve` before serialization.

Deliverables: `oxabl analyze some_file.p --format json` returns a stable document; goldens
green.

Estimated effort: medium.

### Phase 7 — Architectural guardrail verification (required deliverable)

Ship two short written sketches that prove R10 and R11 remain reachable. Stored in
`docs/design/` alongside code, reviewed with the plan's merge.

**`docs/design/semantic-v1-cross-file-sketch.md`** (R10): shows how
`Resolution::Unresolved { reason: CrossFile }` entries become `Resolved(SymbolId)` when a
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

### Phase 8 — Benchmarks, CI, release

Tasks:

- `crates/oxabl_semantic/benches/semantic_bench.rs`: `analyze_file` on the existing parser
  fixture set. Target: ≤ 3× parser time on the same file.
- `crates/oxabl_schema/benches/schema_bench.rs`: already in Phase 2.
- `crates/oxabl_lint/benches/lint_bench.rs`: full pipeline on a 1000-line fixture.
- `.github/workflows/codspeed.yml`: no change; CodSpeed auto-discovers the new benches.
- Release Please: one minor bump (this is pre-1.0, `feat:` bumps minor).
- Docs: update root `README.md` `Current Status` bullets to mention
  `oxabl_schema`, `oxabl_semantic`, `oxabl_lint`, `oxabl analyze`.

Estimated effort: small.

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
- [ ] `oxabl_schema` parses the pcna-erp `.df` sample corpus without error; conflicts produce
  `SCHEMA0010` warnings.
- [ ] `semantic::analyze_file` produces a `Semantic` whose `references` map is populated for
  every identifier reference in the parser's supported constructs.
- [ ] `Semantic::types` is populated for every expression node and every declaration node
  whose type is determinable (or `Unknown`/`Error`).
- [ ] All four v1 lint rules produce diagnostics only via the rule-specific logic above; no
  rule fires on its documented skip list (verified by negative-case fixtures).
- [ ] `oxabl analyze` exits 0 on clean input, 1 on lint/semantic error.
- [ ] Dump JSON includes `oxabl_analyze_version: 1` and is stable across benign refactors
  (enforced via `insta` snapshots).

### Non-Functional Requirements

- [ ] `analyze_file` runs within 3× parse time on every corpus fixture.
- [ ] `schema_loader` loads a 5 MB merged `.df` in under 100 ms.
- [ ] New benches run on CodSpeed CI; regressions surface automatically.
- [ ] `cargo clippy -D warnings` clean on all three new crates.

### Quality Gates

- [ ] ≥ 40 unit tests in `oxabl_semantic` (declare pass).
- [ ] ≥ 60 unit tests in `oxabl_semantic` (resolve + type passes).
- [ ] ≥ 50 rule fixtures across the four lint rules (see Phase 5 minimums).
- [ ] ≥ 40 `oxabl analyze` golden-dump snapshots.
- [ ] Corpus audit baseline TSV committed; each rule's precision ≥ 0.9 on a 100-file manual
  audit sample.

### Architectural Guardrails (R10, R11)

- [ ] `docs/design/semantic-v1-cross-file-sketch.md` reviewed and merged.
- [ ] `docs/design/semantic-v1-flow-analysis-sketch.md` reviewed and merged.
- [ ] Neither sketch identifies an IR change required to v1 types. If either does, block merge
  and revise Phases 3–4.

---

## Success Metrics

- True-positive / false-positive rate per rule on the sampled pcna-erp audit (target ≥ 0.9
  precision each).
- Corpus coverage: % of pcna-erp files where `oxabl analyze` produces a parseable, non-error
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
  `assignment` with `CrossFile`/`Unknown` reasons.

## Risk Analysis & Mitigation

| Risk                                                           | Mitigation                                                                                          |
|----------------------------------------------------------------|-----------------------------------------------------------------------------------------------------|
| Coercion catalog incomplete → high false-positive rate         | Spike in Phase 3/4 bridge; `Unknown`/`Error` lattice suppresses cascades; audit TSV in Phase 5.      |
| `.df` format drift across Progress versions                    | Opaque attribute pass-through; don't error on unknowns.                                             |
| OO-ABL single-file shadowing subtleties (INHERITS, PROPERTY)   | Explicit fixture set in Phase 3 per rule; deferred items (cross-file inheritance) land as `CrossFile`. |
| Namespace ambiguity (`customer` → buffer? table? variable?)    | Explicit namespace table; resolution order documented; default-buffer shadow fixture.                |
| NodeId churn breaks downstream test equality                   | Tests use `EqIgnoreIds` helper; document the pattern in the PR.                                     |
| Corpus audit surfaces many false positives                     | `Unresolved::CrossFile`/`Dynamic`/`Preprocessor` reasons skip lint; suppression pragmas deferred but flagged. |
| Dump format instability churns goldens                         | Versioned with `oxabl_analyze_version`; additive fields don't bump.                                 |

## Resource Requirements

Single engineer, sequential phases. Phases 1, 2, 7, 8 are each ~1 day; Phase 3 is ~3 days;
Phase 4 is ~4 days; Phase 5 is ~3 days; Phase 6 is ~2 days. Total rough order: ~15 engineering
days, gated by test/bench cycles. Cross-phase review happens at Phase 7.

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
