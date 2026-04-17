---
date: 2026-04-12
topic: semantic-layer-and-ecosystem
---

# Oxabl: Semantic Layer & Full Toolchain Ecosystem

## What We're Building

The parser is essentially complete (~14k file corpus, ~100 known unresolvable edge cases). The next
phase transforms oxabl from a parser into a complete tooling platform — the foundation for linters,
language servers, formatters, and static analysis tools. Think Oxc for JavaScript, but for Progress
ABL.

The guiding principle: every tool in the ecosystem deserves the best possible foundation. No
shortcuts that constrain future tools. No architectural decisions that require painful rewrites later.

## Why This Approach

ABL codebases have characteristics that make the "just be fast" non-incremental approach (Oxc's
model) unsuitable:

- Large, sprawling files with thousands of lines
- Massive include files reused across hundreds of calling files
- Deep `RUN` statement chains forming implicit call graphs
- Enterprise codebases with thousands of files and complex interdependencies

The right model is **Salsa-style incremental query computation** — the same system that powers
rust-analyzer. When a file changes, only the queries that depend on that file are invalidated and
re-run. Cross-file analysis (call graphs, include graphs, class hierarchies) scales gracefully
because unchanged files are served from cache.

## Key Decisions

### 1. Incremental Computation: Salsa

Use the `salsa` crate as the backbone for all semantic analysis. Every semantic computation
is a memoized query with declared inputs. The framework handles cache invalidation automatically.

This shapes every crate in the semantic layer — queries instead of functions, input tracked
through Salsa's machinery.

### 2. Full Preprocessor

Build a real ABL preprocessor as a distinct phase before lexing/parsing. Required for complete
analysis and eliminates the current ~100-file gap.

Responsibilities:
- Include file expansion (`{file.i}`, `{file.i &name=value}`, `{0}`, `{1}`, etc.)
- Preprocessor variable tracking (`&SCOPED-DEFINE`, `&GLOBAL-DEFINE`, `&UNDEFINE`)
- Conditional compilation (`&IF`/`&ELSEIF`/`&ELSE`/`&ENDIF`)
- Source mapping: expanded positions → original source positions (critical for error reporting)
- File system abstraction layer (real FS + in-memory for tests and LSP)

### 3. Project Model: oxabl.toml

A clean project manifest lives at the workspace root. No inheritance from Progress legacy config
(`.pf` startup files) — oxabl has its own model.

Declares:
- Source directories and file globs
- PROPATH / include search paths  
- Database schema files (`.df`)
- Compiler options and dialect settings

### 4. Database Schema: Optional, Graceful Degradation

Two sources of schema information:
- **DEFINE TEMP-TABLE** — already in the AST, resolved from source
- **`.df` schema dump files** — Progress DataDefinition format, declared in `oxabl.toml`

Schema information is optional. Code that doesn't touch the database analyzes fully without it.
When schema is absent, field references on connected database tables are treated as unresolved
identifiers rather than hard errors. Rules that require schema knowledge declare that dependency
explicitly and are skipped when schema is unavailable.

### 5. Semantic Layer: Three Passes

Built as a Salsa query group:

1. **Scope & Symbol Table** — lexical scopes, symbol declarations, name binding within a file
2. **Type Analysis** — expression types using symbols + schema (degrades gracefully without schema)
3. **Cross-file Analysis** — call graph (RUN statements), include graph, class inheritance hierarchy

### 6. Preprocessor Source Mapping: Virtual File Abstraction

The expanded source is never materialized as a flat string. Instead it's a lazy tree of spans
pointing into real files — each `{include.i}` reference is a node that delegates into the included
file's span range. The preprocessor produces a **virtual file ID** (a Salsa input); the lexer and
parser operate on virtual files. Position resolution (`virtual_offset → (real_file, real_offset)`)
is a Salsa query that walks the span tree.

This integrates naturally with Salsa: real files are tracked inputs, virtual files are derived
queries. When an include file changes, only the virtual files that reference it are invalidated.
Nested includes and repeated inclusions of the same file are handled correctly because each
expansion site is a distinct node in the span tree.

### 7. Linter Rule Architecture

**Phase 1 (now):** Built-in rules only, compiled into the binary. Rules are written in Rust
against an internal visitor API. Focus on essential, common-sense ABL rules — unsafe `FIND` without
`NO-ERROR`, unreachable code, unused variables, deprecated patterns, etc.

**Phase 2 (later):** WASM plugin interface, once the rule visitor API has stabilized. Organizations
can write company-specific rules (custom naming conventions, forbidden table access patterns, etc.)
in Rust or any WASM-targeting language, distributed as `.wasm` files declared in `oxabl.toml`.

The visitor API is designed from day one to be serialization-friendly (clean types, no raw
pointers in the public surface) so the WASM boundary is straightforward to add later.

### 8. Toolchain Targets

All of the following are first-class targets — the semantic layer is designed to serve all of them
without compromise:

- **Linter** (`oxabl_linter`) — highest organizational value; batch analysis, rule engine,
  built-in ABL-specific rules with WASM extensibility planned
- **LSP server** (`oxabl_lsp`) — go-to-definition, hover types, inline diagnostics, completions;
  Salsa makes this responsive even on large files
- **Formatter** (`oxabl_formatter`) — works over the token stream (comments/whitespace already
  tokenized); no separate CST mode needed

## Proposed Crate Structure

```
crates/
  oxabl_preprocessor   # New: include expansion, conditional compilation, source mapping
  oxabl_workspace      # New: oxabl.toml, PROPATH resolution, file system abstraction
  oxabl_schema         # New: .df parser, database/temp-table schema model
  oxabl_semantic       # New: Salsa query groups — scope, symbols, types, cross-file
  oxabl_linter         # New: rule engine, visitor pattern, built-in + custom rules
  oxabl_lsp            # New: LSP server wired to semantic layer
  oxabl_formatter      # New: code formatter (pending CST decision)
  oxabl_lexer          # Existing (complete)
  oxabl_ast            # Existing (complete)
  oxabl_parser         # Existing (near-complete)
  oxabl_common         # Existing (SourceMap, utilities)
```

## Resolved Questions

1. **Formatter representation** — Token stream is sufficient. Comments and whitespace are already
   tokenized by the lexer; the parser skips them but they're available. No separate CST mode needed.

2. **Salsa version** — Use the latest `salsa` crate from crates.io. No reason to fork or pin an
   older version upfront.

3. **Preprocessor source mapping** — Virtual file abstraction (Option B). Expanded source is never
   materialized; it's a lazy span tree integrated with Salsa's input tracking. See section 6 above.

4. **Rule extensibility** — Built-in compiled rules first. WASM plugin interface added later once
   the visitor API stabilizes. Visitor API designed to be serialization-friendly from day one.

## Open Questions

- None. All major architectural decisions resolved.

## Suggested Implementation Order

1. `oxabl_workspace` + `oxabl.toml` — everything else depends on project context
2. `oxabl_preprocessor` — closes the ~100-file gap; prerequisite for complete semantic analysis
3. `oxabl_schema` — `.df` parser and schema model
4. `oxabl_semantic` (scope + symbols, single file) — first Salsa integration
5. `oxabl_semantic` (types, single file)
6. `oxabl_semantic` (cross-file: call graph, include graph, class hierarchy)
7. `oxabl_linter` — rule engine on top of semantic layer
8. `oxabl_lsp` — LSP server
9. `oxabl_formatter` — after CST question is resolved

## Next Steps

→ `/ce:plan` for implementation details on the first milestone (`oxabl_workspace`)
