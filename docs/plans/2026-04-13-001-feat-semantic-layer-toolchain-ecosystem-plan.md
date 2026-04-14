---
title: "feat: Semantic Layer & Full Toolchain Ecosystem"
type: feat
status: active
date: 2026-04-13
origin: docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md
---

# feat: Semantic Layer & Full Toolchain Ecosystem

## Overview

The oxabl parser is essentially complete (~14k file corpus). This plan defines the full roadmap to
transform oxabl from a parser into a complete ABL tooling platform: the foundation for linters,
language servers, formatters, and static analysis tools.

Seven new crates are introduced in strict dependency order. Each is a prerequisite for the next.
The architecture is shaped by one central decision: **Salsa-style incremental query computation**
(see brainstorm: docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md) because
ABL codebases are large, include-heavy, and deeply cross-referenced — the Oxc "just be fast enough"
model does not scale to them.

---

## Problem Statement

A parser and AST alone cannot power a tooling ecosystem. The gap between "can parse" and "can
lint / provide IDE support / format reliably" requires:

1. **Structured diagnostics** — typed error codes, severity levels, multi-span labels (not plain
   `message: String`)
2. **Multi-file identity** — `Span` today is file-agnostic (`{start: u32, end: u32}`); cross-file
   analysis requires a `FileId`
3. **A real preprocessor** — `{include.i}`, `&IF/&THEN/&ENDIF`, `&SCOPED-DEFINE` must be expanded
   before semantic analysis; this closes the current ~100-file parse gap
4. **A project/workspace model** — the preprocessor needs PROPATH; the linter needs source roots;
   all tools need `oxabl.toml`
5. **Scope and symbol resolution** — name binding: which declaration does this reference resolve to?
6. **Type analysis** — what is the type of this expression?
7. **Cross-file analysis** — call graph (RUN), include graph, class hierarchy (INHERITS/IMPLEMENTS)
8. **A visitor/walker** — the AST has no traversal API; linter rules and semantic passes need one
9. **An incremental computation layer** — Salsa; required for responsive LSP over large files and
   large include graphs

---

## Proposed Solution

Seven new crates, in implementation order:

```
oxabl_common       ← extended: FileId, FileSpan, Diagnostic types
oxabl_workspace    ← new: oxabl.toml, FileSystem trait, FileSet
oxabl_preprocessor ← new: include expansion, &IF, virtual file span tree
oxabl_schema       ← new: .df parser, schema model
oxabl_semantic     ← new: Salsa queries — scope, symbols, types, cross-file
oxabl_linter       ← new: rule engine, visitor, built-in ABL rules
oxabl_lsp          ← new: LSP server (tower-lsp)
oxabl_formatter    ← new: token-stream formatter
```

The existing crates (`oxabl_lexer`, `oxabl_ast`, `oxabl_parser`) are **unchanged in API** — they
remain the stable foundation. Each new crate wraps or consumes them without modification.

---

## Technical Approach

### Dependency Graph

```
oxabl_common (extended)
  └── oxabl_workspace
        ├── oxabl_preprocessor
        │     └── [feeds into] oxabl_semantic
        ├── oxabl_schema
        │     └── [feeds into] oxabl_semantic
        └── oxabl_semantic (Salsa)
              ├── oxabl_linter
              │     └── oxabl_lsp
              └── oxabl_formatter
```

### Key Cross-Cutting Concerns

**FileId and FileSpan** — introduced in `oxabl_common` as the first step. Every crate above it uses
`FileId` to identify source files. `Span` stays as-is (byte offsets within a single file);
`FileSpan { file: FileId, span: Span }` is the multi-file span used by diagnostics and the semantic
layer.

**Diagnostic type** — introduced in `oxabl_common` alongside `FileId`. Replaces bare string errors.
Severity-aware, code-carrying, LSP-compatible.

**FileSystem trait** — introduced in `oxabl_workspace`. Every component that reads files (the
preprocessor, the workspace scanner, the LSP server's document sync) uses this trait rather than
`std::fs` directly. An `InMemoryFileSystem` implementation enables hermetic unit tests.

**Salsa database** — `oxabl_semantic` defines the Salsa `#[salsa::db]` trait. The LSP server
creates and owns the database. The linter creates a throwaway database per run. Both see the same
query API.

---

## Implementation Phases

---

### Phase 1 — oxabl_common Extensions

**Goal:** Add the primitives that every subsequent crate depends on.

**New types in `oxabl_common`:**

```
crates/oxabl_common/src/file_id.rs     — FileId, FileSet
crates/oxabl_common/src/file_span.rs   — FileSpan { file: FileId, span: Span }
crates/oxabl_common/src/diagnostic.rs  — Diagnostic, Severity, DiagnosticCode, Label
```

**`FileId`:**
- Newtype over `u32`: `pub struct FileId(u32);`
- `FileSet` maps `FileId ↔ PathBuf`; constructed by `oxabl_workspace` and threaded through the
  system
- `FileId::UNKNOWN` (0) as sentinel for synthetic / in-memory files

**`FileSpan`:**
- `pub struct FileSpan { pub file: FileId, pub span: Span }`
- Implements `From<(FileId, Span)>` for ergonomics
- Used by `Diagnostic` labels and the semantic layer's `ReferenceMap`

**`Diagnostic`:**
- ```rust
  pub struct Diagnostic {
      pub severity: Severity,
      pub code: DiagnosticCode,
      pub message: String,
      pub span: FileSpan,        // primary span
      pub labels: Vec<Label>,    // secondary annotations
      pub help: Option<String>,  // suggestion / fix hint
  }
  pub enum Severity { Error, Warning, Info, Hint }
  pub struct DiagnosticCode(pub &'static str);  // e.g. "ABL0001"
  pub struct Label { pub span: FileSpan, pub message: String }
  ```
- Designed to map 1:1 to LSP `Diagnostic` and Rust's `ariadne`/`miette` rendering
- Replaces `ParseError { message: String, span: Span }` as the project-wide error vocabulary

**Acceptance Criteria:**
- [x] `FileId`, `FileSet`, `FileSpan`, `Diagnostic` exported from `oxabl_common`
- [x] `ParseError` in `oxabl_parser` updated to convert to `Diagnostic` (add `impl From<ParseError>
  for Diagnostic`)
- [x] All existing tests pass unchanged
- [x] No new heap allocations on the diagnostic hot path for the `message: String` (already heap,
  that's fine — it's not a hot path)

---

### Phase 2 — oxabl_workspace

**Goal:** Parse `oxabl.toml`, resolve the file set, and provide a `FileSystem` abstraction.

**New crate:** `crates/oxabl_workspace/`

**`oxabl.toml` format (initial):**
```toml
[workspace]
name = "my-abl-project"

[workspace.sources]
directories = ["src/", "procedures/"]
include_paths = ["src/include/", "/shared/abl/"]  # PROPATH equivalent

[workspace.schema]
files = ["schema/sports2000.df"]  # optional; omit if no connected database

[workspace.linter]
# future: rule enable/disable overrides
```

**Key types:**

```
crates/oxabl_workspace/src/
  config.rs      — WorkspaceConfig (deserializes oxabl.toml via serde + toml)
  file_system.rs — FileSystem trait + RealFileSystem + InMemoryFileSystem
  file_set.rs    — FileSet builder: discovers .p/.w/.cls files, assigns FileIds
  workspace.rs   — Workspace { config, file_set, file_system } — the top-level handle
```

**`FileSystem` trait:**
```rust
pub trait FileSystem: Send + Sync {
    fn read(&self, path: &Path) -> Result<Arc<str>, IoError>;
    fn exists(&self, path: &Path) -> bool;
    fn resolve_include(&self, include_paths: &[PathBuf], name: &str) -> Option<PathBuf>;
}
```

**`Workspace::from_path(root: &Path)`** — discovers `oxabl.toml`, builds config, builds file set.
**`Workspace::in_memory(config, files)`** — for LSP (files come from document sync, not disk).

**Acceptance Criteria:**
- [x] `cargo test -p oxabl_workspace` passes with `InMemoryFileSystem` tests
- [x] `Workspace::from_path` discovers all `.p`, `.w`, `.cls`, `.i` files under declared source dirs
- [x] `FileSystem::resolve_include` searches include paths in order, returns `None` if not found
- [x] `oxabl.toml` parse errors produce readable diagnostics (not panics)
- [x] `RealFileSystem` and `InMemoryFileSystem` both implement the trait identically from the
  caller's perspective

---

### Phase 3 — oxabl_preprocessor

**Goal:** Expand include files, evaluate `&IF`/`&THEN`/`&ENDIF`, track preprocessor variables.
Use a **virtual file span tree** — never materialize the expanded source as a `String`.
(see brainstorm: docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md §6)

**New crate:** `crates/oxabl_preprocessor/`

**Core data structure — the span tree:**

```rust
/// A node in the virtual file expansion tree.
pub enum SpanNode {
    /// A contiguous slice of a real file.
    Chunk { file: FileId, start: u32, end: u32 },
    /// An expanded include site.
    Include {
        /// The span in the *parent* file where `{file.i}` appeared.
        site: FileSpan,
        /// The content of the included file, itself a span tree.
        children: Vec<SpanNode>,
    },
}

/// The preprocessed representation of a source file.
pub struct PreprocessedFile {
    /// The root span tree (may be deeply nested for include-heavy files).
    pub tree: Vec<SpanNode>,
    /// Preprocessor variable state after processing this file.
    pub vars: PreprocVarTable,
    /// All include files transitively referenced (for change tracking).
    pub dependencies: Vec<FileId>,
}
```

**Position resolution:**

```rust
impl PreprocessedFile {
    /// Map a virtual (logical) byte offset back to its real source location.
    /// O(log n) via precomputed cumulative-length prefix sums over the span tree.
    pub fn resolve(&self, virtual_offset: u32) -> FileSpan { ... }

    /// Produce the logical source text for lexing.
    /// The only place a String is materialized — called by the lexer.
    pub fn to_text(&self) -> Arc<str> { ... }
}
```

Note: `to_text()` does materialize a string for the lexer — but it is allocated once per file per
preprocessing run, not repeatedly. The key benefit of the tree is **position resolution without
re-scanning the string** and **Salsa-compatible change tracking** via `dependencies`.

**`PreprocVarTable`:**
- `HashMap<Arc<str>, Arc<str>>` — preprocessor variable name → value
- Scoped: `&SCOPED-DEFINE` is file-local, `&GLOBAL-DEFINE` propagates to callers

**`Preprocessor`:**
```rust
pub struct Preprocessor<'fs> {
    fs: &'fs dyn FileSystem,
    include_paths: &'fs [PathBuf],
}
impl Preprocessor<'_> {
    pub fn process(&self, file: FileId, source: &str) -> Result<PreprocessedFile, Vec<Diagnostic>>;
}
```

**`&IF` evaluation:**
- Evaluate `&IF condition` at preprocess time using the current `PreprocVarTable`
- Conditions: `DEFINED(name)`, `"{&name}" = "value"`, `NOT`, `AND`, `OR`
- Unsatisfied branches are excluded from the span tree (their `SpanNode`s are dropped)

**Acceptance Criteria:**
- [ ] `{include.i}` expanded recursively; cyclic includes detected and errored
- [ ] `&SCOPED-DEFINE` / `&GLOBAL-DEFINE` / `&UNDEFINE` tracked correctly
- [ ] `&IF` / `&ELSEIF` / `&ELSE` / `&ENDIF` prune the span tree at preprocess time
- [ ] `resolve(offset)` returns the correct `(FileId, real_offset)` for all positions in the tree,
  including nested includes
- [ ] `dependencies` accurately lists all transitively included `FileId`s
- [ ] The ~100 currently-failing corpus files parse successfully after preprocessing
- [ ] No corpus regression on previously-passing files

---

### Phase 4 — oxabl_schema

**Goal:** Parse Progress `.df` schema dump files and provide an optional schema model.

**New crate:** `crates/oxabl_schema/`

**`.df` format primer:**
Progress `.df` files use a structured text format:
```
ADD TABLE "customer"
  AREA "Schema Area"
  DUMP-NAME "customer"

ADD FIELD "cust-num" OF "customer" AS integer
  FORMAT ">>>>>>9"
  INITIAL "0"
  LABEL "Cust Num"
  COLUMN-LABEL "Cust#"

ADD INDEX "cust-num" ON "customer"
  AREA "Schema Area"
  UNIQUE
  PRIMARY
  INDEX-FIELD "cust-num" ASCENDING
```

**Key types:**
```rust
pub struct SchemaDatabase {
    pub tables: HashMap<AsciiCaseName, Table>,
}
pub struct Table {
    pub name: String,
    pub fields: HashMap<AsciiCaseName, Field>,
    pub indexes: Vec<Index>,
}
pub struct Field {
    pub name: String,
    pub data_type: FieldType,
    pub label: Option<String>,
    pub initial: Option<String>,
}
pub enum FieldType { Integer, Int64, Decimal, Character, Logical, Date, DateTime, DateTimeTz, Recid, Raw }
```

Note: `AsciiCaseName` is a wrapper that normalizes to lowercase for case-insensitive lookup
(ABL identifiers are case-insensitive).

**`SchemaDatabase::from_df(source: &str)`** — parses a `.df` file.
**`SchemaDatabase::empty()`** — the "no schema" sentinel; all lookups return `None`.

**Integration with semantic layer:**
- `Workspace` loads all `.df` files declared in `oxabl.toml` and produces a `SchemaDatabase`
- When schema is absent, semantic analysis continues without database field types
- Rules that require schema declare `fn requires_schema() -> bool { true }` and are skipped when
  `SchemaDatabase::is_empty()`

**Acceptance Criteria:**
- [ ] Parses `sports2000.df` and produces correct table/field/index model
- [ ] All lookups are case-insensitive
- [ ] Missing schema produces no errors — semantic layer degrades gracefully
- [ ] `.df` parse errors surface as `Diagnostic`s with the schema file's `FileId`

---

### Phase 5 — oxabl_semantic (Salsa)

**Goal:** Incremental semantic analysis — scope resolution, symbol table, type inference,
cross-file analysis. Built entirely on the `salsa` crate.

**New crate:** `crates/oxabl_semantic/`

**Salsa database definition:**

```rust
#[salsa::db]
pub trait SemanticDb: salsa::Database {
    // === Inputs (mutable; setting these triggers re-computation) ===
    fn file_text(&self, file: FileId) -> Arc<str>;
    fn workspace_config(&self) -> Arc<WorkspaceConfig>;

    // === Derived: preprocessing ===
    fn preprocessed(&self, file: FileId) -> Arc<PreprocessedFile>;

    // === Derived: parsing (from preprocessed text) ===
    fn parse(&self, file: FileId) -> Arc<Program>;
    fn parse_diagnostics(&self, file: FileId) -> Arc<Vec<Diagnostic>>;

    // === Derived: single-file semantics ===
    fn scope_tree(&self, file: FileId) -> Arc<ScopeTree>;
    fn symbol_table(&self, file: FileId) -> Arc<SymbolTable>;
    fn type_map(&self, file: FileId) -> Arc<TypeMap>;
    fn reference_map(&self, file: FileId) -> Arc<ReferenceMap>;
    fn semantic_diagnostics(&self, file: FileId) -> Arc<Vec<Diagnostic>>;

    // === Derived: cross-file ===
    fn include_graph(&self) -> Arc<IncludeGraph>;
    fn call_graph(&self) -> Arc<CallGraph>;
    fn class_hierarchy(&self) -> Arc<ClassHierarchy>;
}
```

**Scope & Symbol Table — key types:**

```rust
pub struct ScopeTree {
    pub root: ScopeId,
    scopes: Vec<Scope>,
}
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,  // File | Procedure | Function | Method | DoBlock | Class | Interface
    pub symbols: HashMap<AsciiCaseName, SymbolId>,
    pub span: Span,
}
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}
pub struct Symbol {
    pub id: SymbolId,
    pub kind: SymbolKind,
    pub name: String,
    pub declared_at: FileSpan,
    pub scope: ScopeId,
    pub type_: Option<ResolvedType>,
}
pub enum SymbolKind {
    Variable, Parameter, TempTable, TempTableField, Buffer, Stream, Frame, Event,
    Procedure, Function, Method, Property, Constructor, Destructor, Class, Interface,
}
```

**Type inference — `ResolvedType`:**
```rust
pub enum ResolvedType {
    Integer, Int64, Decimal, Character, Logical, Date, DateTime, DateTimeTz,
    Handle, Rowid, Recid, Raw, Memptr, Longchar, Clob, Blob,
    Class(Arc<str>),     // OO-ABL class reference
    TempTable(SymbolId), // reference to a DEFINE TEMP-TABLE symbol
    DbTable(Arc<str>),   // database table (from SchemaDatabase)
    Unknown,             // unresolvable — not an error, just insufficient info
}
```

**`ReferenceMap`:** `HashMap<Span, SymbolId>` — maps every identifier reference span to its
resolved symbol. Powers go-to-definition and find-all-references in the LSP.

**`TypeMap`:** `HashMap<Span, ResolvedType>` — maps every expression span to its inferred type.
Powers hover and completion in the LSP.

**Visitor trait (defined here, used by linter):**

```rust
pub trait AstVisitor: Sized {
    // Statements — default impl walks children
    fn visit_statement(&mut self, stmt: &Statement) { walk_statement(self, stmt); }
    fn visit_define_variable(&mut self, stmt: &DefineVariableStatement) { /* no children */ }
    fn visit_assign(&mut self, stmt: &AssignStatement) { walk_assign(self, stmt); }
    fn visit_if(&mut self, stmt: &IfStatement) { walk_if(self, stmt); }
    fn visit_do(&mut self, stmt: &DoStatement) { walk_do(self, stmt); }
    fn visit_for_each(&mut self, stmt: &ForEachStatement) { walk_for_each(self, stmt); }
    fn visit_find(&mut self, stmt: &FindStatement) { /* ... */ }
    fn visit_run(&mut self, stmt: &RunStatement) { /* ... */ }
    // ... one method per Statement variant
    // Expressions
    fn visit_expression(&mut self, expr: &Expression) { walk_expression(self, expr); }
    fn visit_function_call(&mut self, expr: &FunctionCallExpression) { walk_function_call(self, expr); }
    // ... one method per Expression variant
}
// Free functions that implement the default walking behaviour
pub fn walk_statement<V: AstVisitor>(v: &mut V, stmt: &Statement) { ... }
pub fn walk_expression<V: AstVisitor>(v: &mut V, expr: &Expression) { ... }
```

**Cross-file analysis:**
- **`IncludeGraph`**: `FileId → Vec<FileId>` — built from `PreprocessedFile::dependencies`
- **`CallGraph`**: `(FileId, ProcedureName) → Vec<(FileId, ProcedureName)>` — built from
  `RunStatement` AST nodes, resolved via `SymbolTable` and file set
- **`ClassHierarchy`**: `ClassName → { inherits: Option<ClassName>, implements: Vec<ClassName> }`
  — built from `ClassStatement` AST nodes across the file set

**Acceptance Criteria:**
- [ ] Salsa database correctly invalidates downstream queries when a file's text changes
- [ ] `scope_tree` correctly models all scope kinds: file, procedure, function, method, do-block,
  class, interface
- [ ] `symbol_table` contains all declared symbols with correct `SymbolKind` and `declared_at`
- [ ] `reference_map` resolves all local variable references; unresolved references remain as
  `Unknown` (not errors)
- [ ] `type_map` infers types for all constant expressions and declared-type variables
- [ ] `include_graph` correctly tracks all transitive include dependencies
- [ ] Changing an include file triggers re-analysis of all files that include it (Salsa
  invalidation)
- [ ] `AstVisitor` trait covers all Statement and Expression variants

---

### Phase 6 — oxabl_linter

**Goal:** Rule engine with a visitor-based API and an initial set of built-in ABL-specific rules.

**New crate:** `crates/oxabl_linter/`

**Rule trait:**
```rust
pub trait Rule: Send + Sync {
    /// Unique identifier, e.g. `"ABL0042"`.
    fn id(&self) -> &'static str;
    /// Human-readable name.
    fn name(&self) -> &'static str;
    /// Default severity.
    fn severity(&self) -> Severity;
    /// True if this rule requires a loaded SchemaDatabase to function.
    fn requires_schema(&self) -> bool { false }
    /// Check a single file. Emit diagnostics via `ctx`.
    fn check(&self, ctx: &mut RuleContext<'_>);
}

pub struct RuleContext<'db> {
    pub file: FileId,
    pub program: &'db Program,
    pub symbols: &'db SymbolTable,
    pub scope_tree: &'db ScopeTree,
    pub type_map: &'db TypeMap,
    pub schema: &'db SchemaDatabase,  // may be empty
    diagnostics: Vec<Diagnostic>,
}
impl RuleContext<'_> {
    pub fn emit(&mut self, diag: Diagnostic) { self.diagnostics.push(diag); }
}
```

**`LintPass`** — runs all enabled rules over all files in the workspace:
```rust
pub struct LintPass {
    rules: Vec<Box<dyn Rule>>,
}
impl LintPass {
    pub fn run(&self, db: &dyn SemanticDb, files: &[FileId]) -> Vec<Diagnostic>;
}
```

**Initial built-in rules:**

| ID | Name | Severity | Requires schema |
|----|------|----------|-----------------|
| `ABL0001` | `find-no-error` | Warning | No |
| `ABL0002` | `for-each-no-error` | Warning | No |
| `ABL0003` | `for-each-no-lock` | Warning | No |
| `ABL0004` | `empty-catch-block` | Warning | No |
| `ABL0005` | `unused-variable` | Info | No |
| `ABL0006` | `undefined-variable` | Error | No |
| `ABL0007` | `run-unresolved` | Warning | No |
| `ABL0008` | `error-swallowed` | Warning | No |
| `ABL0009` | `deprecated-data-type` | Warning | No |
| `ABL0010` | `direct-db-write-no-transaction` | Error | Yes |

Note on `ABL0001` / `ABL0002`: ABL's `FIND` and `FOR EACH` statements default to `EXCLUSIVE-LOCK`
if no lock phrase is specified, and will throw errors if the record does not exist and `NO-ERROR`
is absent. These are the most common sources of runtime errors in ABL codebases and the highest
organizational value lint rules.

**WASM plugin interface (deferred):**
The `Rule` trait is designed to be FFI-safe in the future. No raw lifetimes or generics in the
public surface. `RuleContext` fields are all pointer-sized types or `&'static`. When WASM support
is added, a `WasmRule` adapter will wrap a WASM module that implements the same interface via a
stable ABI.

**`oxabl.toml` linter config (future):**
```toml
[linter]
disable = ["ABL0005"]          # turn off globally
[linter.override]
"ABL0003" = "error"            # escalate to error
```

**Acceptance Criteria:**
- [ ] `LintPass::run` produces correct diagnostics for each built-in rule on test fixtures
- [ ] Rules that require schema are automatically skipped when `SchemaDatabase::is_empty()`
- [ ] `RuleContext::emit` produces `Diagnostic`s with correct `code`, `severity`, and `span`
- [ ] Output format renders correctly in terminal (via `ariadne` or similar)
- [ ] `oxabl check` CLI updated to run the linter and output diagnostics

---

### Phase 7 — oxabl_lsp

**Goal:** LSP server providing go-to-definition, hover, diagnostics, and completions.

**New crate:** `crates/oxabl_lsp/`

**Dependencies:** `tower-lsp` (the standard Rust LSP server framework).

**Salsa database lifecycle:**
- The LSP server creates a single `SemanticDb` instance at startup
- `textDocument/didOpen` and `textDocument/didChange` call `db.set_file_text(file_id, new_text)`
- Salsa automatically invalidates all downstream queries for that file and its dependents
- `textDocument/didClose` removes the file from the in-memory file set

**Implemented LSP capabilities:**

| Capability | Powered by |
|------------|-----------|
| `textDocument/publishDiagnostics` | `parse_diagnostics` + `semantic_diagnostics` + `LintPass` |
| `textDocument/definition` | `reference_map` — `Span → SymbolId → declared_at: FileSpan` |
| `textDocument/hover` | `type_map` — `Span → ResolvedType` + symbol documentation |
| `textDocument/completion` | `scope_tree` — symbols in scope at cursor position |
| `workspace/didChangeWatchedFiles` | Re-scan file set; update `FileId` mappings |

**Acceptance Criteria:**
- [ ] LSP server starts and responds to `initialize` / `initialized` handshake
- [ ] Diagnostics publish within 100ms of document change on a typical ABL file (<1000 lines)
- [ ] Go-to-definition navigates to correct declaration span for local variables, procedures,
  functions, methods, and class references
- [ ] Hover shows resolved type for variables and return type for function calls
- [ ] Completions list symbols in scope at cursor position

---

### Phase 8 — oxabl_formatter

**Goal:** Token-stream formatter that produces canonical ABL style.

**New crate:** `crates/oxabl_formatter/`

**Approach:** Consume both the token stream (for whitespace/comment fidelity) and the AST (for
structural indentation decisions). No separate CST needed — comments and whitespace are already
tokenized by `oxabl_lexer`; the parser skips them, but they remain available in the raw
`Vec<Token>`.
(see brainstorm: docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md §resolved Q1)

**Core algorithm:**
1. Walk the token stream linearly
2. For each non-comment, non-whitespace token: emit with normalized spacing
3. For comment tokens: emit verbatim (preserving the original text)
4. Use the AST to determine expected indent level at each statement boundary
5. Normalize line endings and trailing whitespace

**`oxabl.toml` formatter config (future):**
```toml
[formatter]
indent_size = 4         # default: 4
max_line_length = 120   # default: 120
```

**Acceptance Criteria:**
- [ ] `oxabl format <file>` rewrites the file with canonical indentation
- [ ] Comments are preserved verbatim at their original relative positions
- [ ] Formatting is idempotent: `format(format(x)) == format(x)`
- [ ] No changes to non-whitespace tokens (formatter must not alter code semantics)

---

## Alternative Approaches Considered

**Oxc model (non-incremental fast batch analysis):**
Re-analyze the whole file from scratch on every change. Simpler architecture, no Salsa dependency.
Rejected because ABL files are large, include-heavy, and deeply cross-referenced — the re-analysis
cost per keystroke would be unacceptable for an LSP serving a large enterprise codebase.
(see brainstorm: docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md §Why This Approach)

**tree-sitter for the parser:**
Would give a CST and incremental parsing for free. Rejected because we already have a complete,
fast Rust parser with 400+ tests; the migration cost is not justified, and tree-sitter grammars
are harder to maintain than typed Rust ASTs.

**Materializing the expanded preprocessor source:**
Simpler to implement — just concatenate chunks. Rejected because it loses position information
(errors report wrong file/line), doubles memory usage for include-heavy files, and breaks Salsa's
change tracking (include file change invalidates the whole expanded string).

**WASM linter rules from day one:**
Provides extensibility sooner. Rejected because the `Rule` visitor API will evolve during initial
rule development; locking it into a stable ABI before it's settled creates painful
backward-compatibility obligations. WASM is the target once the API stabilizes.

---

## System-Wide Impact

### Interaction Graph

Adding `FileId` to `oxabl_common` is the only change that touches existing crates:
- `oxabl_parser`'s `ParseError` gains a `From<ParseError> for Diagnostic` impl (additive)
- `oxabl_lexer` and `oxabl_ast` are **untouched**
- The CLI binary `oxabl/src/main.rs` gains new subcommands (`check`, `format`, `lsp`) but existing
  `parse` / `check` commands remain

### Error Propagation

- Parse errors: `ParseError → Diagnostic` (via `From` impl); surfaced via `parse_diagnostics(file)`
- Preprocessor errors (cycle detection, missing include): emitted as `Diagnostic { severity: Error }`
- Semantic errors (undefined references): emitted as `Diagnostic { severity: Error | Warning }`
- Linter diagnostics: emitted via `RuleContext::emit`; severity per rule default, overridable
- All diagnostic streams merge at the LSP / CLI output layer

### State Lifecycle Risks

- Salsa database is long-lived in the LSP; file text inputs must be set before any query runs
- Include file cycles must be detected in the preprocessor to prevent infinite recursion
- `SchemaDatabase` is loaded once at workspace initialization; no hot-reload in Phase 4 (future)

### API Surface Parity

The linter CLI (`oxabl check`) and the LSP server both run the same `LintPass`. No rule logic
lives in the CLI or LSP — all rules are in `oxabl_linter`. This ensures parity: a rule that
catches a bug in CI also highlights it inline in the IDE.

### Integration Test Scenarios

1. **Include cycle detection**: `a.i` includes `b.i` includes `a.i` → preprocessor emits error,
   analysis continues on what was resolved before the cycle
2. **Renamed variable**: LSP edits `x` declaration, `reference_map` updates, all references now
   resolve to the new declaration (Salsa invalidation chain)
3. **Schema-required rule without schema**: `ABL0010` declared with `requires_schema = true`;
   running lint without `.df` files → rule is silently skipped, no panic
4. **Large include file change**: `shared/global.i` is included in 500 files; editing it triggers
   re-analysis of all 500 — Salsa processes them incrementally, not sequentially
5. **OO-ABL class hierarchy**: class `CustomerService` INHERITS `BaseService`; `reference_map`
   resolves a `SUPER:method()` call to `BaseService`'s `method` declaration

---

## Acceptance Criteria

### Functional

- [ ] `oxabl check <project>` runs all lint rules and exits non-zero on errors
- [ ] `oxabl format <file>` formats a file idempotently
- [ ] LSP server implements definition, hover, completion, and diagnostics
- [ ] All ~14k corpus files analyze without panic (errors for truly unresolvable cases are fine)
- [ ] Previously-failing ~100 preprocessor-dependent files now parse and analyze correctly

### Non-Functional

- [ ] LSP diagnostics latency < 100ms for files under 1000 lines
- [ ] Linting the full 14k-file corpus completes in under 60 seconds on a modern machine
- [ ] No new heap allocations added to the lexer or parser hot paths
- [ ] CodSpeed benchmarks show no regression in parser throughput

### Quality Gates

- [ ] `cargo test` passes across all crates
- [ ] `cargo clippy -D warnings` clean
- [ ] `cargo fmt --check` clean
- [ ] Each new crate has a benchmark if it introduces a hot path

---

## Dependencies & Prerequisites

**New external dependencies to evaluate:**

| Crate | Purpose | Notes |
|-------|---------|-------|
| `salsa` | Incremental query computation | Latest version from crates.io |
| `tower-lsp` | LSP server framework | Standard Rust LSP library |
| `toml` | `oxabl.toml` parsing | Already likely in workspace; confirm |
| `serde` + `serde_derive` | Config deserialization | Likely already present in `oxabl` |
| `ariadne` or `miette` | Diagnostic terminal rendering | Evaluate both; pick one |

**Internal prerequisites (in order):**
1. Phase 1 (`oxabl_common` extensions) must complete before any subsequent phase
2. Phase 2 (`oxabl_workspace`) must complete before Phase 3 and Phase 4
3. Phase 3 (`oxabl_preprocessor`) and Phase 4 (`oxabl_schema`) can proceed in parallel
4. Phase 5 (`oxabl_semantic`) requires Phases 2, 3, and 4
5. Phases 6, 7, 8 each require Phase 5

---

## Future Considerations

- **WASM rule plugins**: once the `Rule` visitor API stabilizes in Phase 6, expose a WASM binding
  for third-party custom rules declared in `oxabl.toml`
- **`oxabl.pf` compatibility shim**: for adoption in existing Progress projects without an
  `oxabl.toml`, a shim that reads `.pf` startup parameter files for PROPATH
- **Live database schema introspection**: connect to a running Progress OpenEdge instance to query
  the schema at analysis time (supplements `.df` static files)
- **Codemods / transforms**: once the semantic layer and cross-file analysis exist, automated
  refactoring (rename symbol, extract procedure) becomes tractable
- **`{&PROPATH}` and runtime-only preprocessor values**: some preprocessor conditions can only be
  evaluated with runtime environment information; document these as "analysis limitations" rather
  than errors

---

## Sources & References

### Origin

- **Brainstorm document:** [docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md](docs/brainstorms/2026-04-12-semantic-layer-and-ecosystem-brainstorm.md)

  Key decisions carried forward:
  - Salsa over Oxc-model because ABL files are large and include-heavy
  - Virtual file span tree (never materialize expanded source) for preprocessor position mapping
  - `oxabl.toml` as the project manifest (no `.pf` inheritance)
  - Schema analysis is optional; analysis degrades gracefully without `.df` files
  - Token stream is sufficient for the formatter (no separate CST)
  - Built-in Rust linter rules first; WASM plugin interface deferred until API stabilizes

### Internal References

- Current `Span` type: `crates/oxabl_ast/src/span.rs`
- Current `SourceMap`: `crates/oxabl_common/src/source_map.rs`
- Current `ParseError` / `Program`: `crates/oxabl_parser/src/parser/mod.rs`
- Full `Statement` variant inventory: `crates/oxabl_ast/src/statement.rs`
- Full `Expression` variant inventory: `crates/oxabl_ast/src/expression.rs`
- Workspace `Cargo.toml`: `Cargo.toml`
- Umbrella re-export hub: `crates/oxabl/src/lib.rs`

### External References

- [salsa crate (crates.io)](https://crates.io/crates/salsa) — incremental query framework
- [tower-lsp crate (crates.io)](https://crates.io/crates/tower-lsp) — LSP server framework
- [rust-analyzer architecture](https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html) — Salsa integration reference
- [Oxc architecture](https://oxc.rs/docs/learn/architecture/overview.html) — reference for what we deliberately diverged from
