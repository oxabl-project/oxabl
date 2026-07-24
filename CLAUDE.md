# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Oxabl is a high-performance tooling suite for Progress ABL (Advanced Business Language), written in Rust. The project aims to provide blazingly fast developer tools for ABL code (formatting, linting, parsing, etc.). This is an unofficial project with no affiliation with Progress Software.

## Build Commands

```bash
# Build the entire workspace
cargo build

# Build a specific crate
cargo build -p oxabl_lexer

# Run tests for the entire workspace
cargo test

# Run tests for a specific crate
cargo test -p oxabl_lexer

# Run a single test
cargo test -p oxabl_lexer -- test_name

# Check code without building
cargo check
```

## Code Generation

The lexer uses generated code for ABL keywords, operators, and the keyword matching function. The `oxabl_codegen` crate parses reference HTML/JSON files from `resources/` and generates Rust code.

```bash
# Write all generated files (default)
cargo run -p oxabl_codegen

# Write specific file(s)
cargo run -p oxabl_codegen -- kind     # Writes kind.rs (enum + match function)
cargo run -p oxabl_codegen -- atoms    # Writes build.rs (complete file)
cargo run -p oxabl_codegen -- callable # Writes callable.rs (is_callable_kind)
cargo run -p oxabl_codegen -- builtins # Writes builtins.rs (built-in function registry)

# Show statistics without writing files
cargo run -p oxabl_codegen -- summary
```

Generated files are written directly to their target locations and include a "DO NOT EDIT" header.

## Architecture

### Workspace Structure

- `crates/oxabl` - Main library (future unified API)
- `crates/oxabl_lexer` - Tokenizer for ABL source code (MVP complete)
- `crates/oxabl_ast` - AST node definitions (expressions, statements, literals)
- `crates/oxabl_parser` - Parser for ABL source code (actively developed)
- `crates/oxabl_common` - Shared utilities including `SourceMap`
- `crates/oxabl_codegen` - Code generation tool for lexer keywords

### Lexer (`oxabl_lexer`)

The lexer should classify tokens as distinctly as possible so the parser dispatches on `Kind` enum variants (O(1) integer comparison), never on runtime string comparison or `to_uppercase()` allocations. When a new keyword is needed by the parser, add it to `keyword_overrides.toml` and regenerate — do not use `eq_ignore_ascii_case()` workarounds. This principle yielded ~8% parsing performance improvement when applied to data type and statement keywords.

**Avoid heap allocations on hot paths.** ABL keywords are ASCII-only, so case-insensitive matching uses a `[u8; 64]` stack buffer with `to_ascii_lowercase()` byte folding — never `s.to_lowercase()` which heap-allocates a `String`. Eliminating the `to_lowercase()` allocation in `match_keyword()` (called on every token) yielded a ~20% overall performance improvement. The codegen emits a compile-time assertion that the longest keyword fits in the buffer. The same principle applies anywhere in the lexer: prefer `eq_ignore_ascii_case()` (zero-allocation byte comparison) over `to_lowercase()` + match.

The lexer tokenizes ABL source code into a stream of tokens. Key components:

- **Token**: Contains `kind` (token type), `start`/`end` byte offsets, and `value` (for literals)
- **Kind**: Enum of all token types (operators, keywords, identifiers, literals)
- **TokenValue**: Enum for literal values (Integer, BigInt, Decimal, String, Boolean, None)

The lexer uses `string_cache` for interned strings (atoms). Atoms are generated at build time via `build.rs` which includes all ABL keywords.

ABL-specific features handled:
- Case-insensitive keywords with abbreviations (e.g., `def`, `defi`, `define` all map to `Kind::Define`)
- Tilde (`~`) as escape character in strings
- Hyphens allowed in identifiers (`my-variable-name`)
- Preprocessor directives (`&if`, `&scoped-define`) and references (`{&variable}`)
- Include file references (`{file.i}`, `{file.i args}`, `{file.i &name=value}`)
- Include positional argument references (`{0}`, `{1}`, `{2}`)
- Line (`//`) and block (`/* */`) comments

### Source Map (`oxabl_common`)

Converts byte offsets (stored in tokens) to human-readable line/column positions. Uses binary search over precomputed line start offsets for O(log n) lookups.

### AST (`oxabl_ast`)

Defines AST nodes for the parser. Key types:

- **Literals**: Integer, Decimal, String, Boolean, Unknown (ABL's `?` literal)
- **Expressions**: Arithmetic, comparison, logical, string comparison (BEGINS/MATCHES/CONTAINS), unary, ternary (IF/THEN/ELSE), function calls, postfix operations (member access, method calls, array access, field access)
- **Statements**: VariableDeclaration, Assignment, ExpressionStatement, Block, Do, If, Repeat, Leave, Next, Return, IncludeReference, IncludeArgReference, Empty, Class, Method, Property, Constructor, Destructor, Interface, Using
- **Data Types**: Integer, Int64, Decimal, Character, Logical, Date, DateTime, DateTimeTz, Handle, Rowid, Recid, Raw, Memptr, Longchar, Clob, Blob, Com, Class

AST invariants relied on by semantic/lint/analyze passes — span rules, NodeId behavior *(Phase 1)*, identifier casing, operator-precedence-in-tree-shape, postfix left-nesting, `PreprocIf<T>` branch treatment, declaration/recovery invariants, and property GET/SET body tri-state — are enumerated in [`docs/design/ast-invariants.md`](docs/design/ast-invariants.md). Any change to `oxabl_ast` that adds, removes, or reshapes a public type must update that doc in the same PR.

### Parser (`oxabl_parser`)

Parses ABL source code into an AST. Key capabilities:

- **Expression parsing** with proper operator precedence (ternary → or → and → comparison → additive → multiplicative → unary → postfix → primary)
- **Statement parsing**: DEFINE VARIABLE/VAR/PARAMETER/TEMP-TABLE/BUFFER/PROPERTY/STREAM/FRAME/EVENT, DO blocks (with counting loops), IF/THEN/ELSE, REPEAT, FOR EACH, FIND, CASE, PROCEDURE, FUNCTION, RUN, DISPLAY (with STREAM clause), MESSAGE, ASSIGN, CREATE, DELETE, RELEASE, VALIDATE, BUFFER-COPY, BUFFER-COMPARE, INPUT/OUTPUT/INPUT-OUTPUT stream I/O (FROM/TO/THROUGH/CLOSE), CATCH/FINALLY/THROW, PUBLISH/SUBSCRIBE/UNSUBSCRIBE, ON (UI/developer event triggers with IN FRAME/IN BROWSE, database event triggers, key remapping), TRIGGER PROCEDURE, LEAVE, NEXT, RETURN
- **OO-ABL**: CLASS (with ABSTRACT/FINAL, INHERITS, IMPLEMENTS), INTERFACE, METHOD (with access modifiers, STATIC/ABSTRACT/OVERRIDE), DEFINE PROPERTY (auto and computed GET/SET), CONSTRUCTOR, DESTRUCTOR, USING
- **Postfix operations**: Method calls (object:method()), member access (object.member), array access (arr[i]), field access (table.field)
- **Function calls** with argument lists
- **Preprocessor**: &IF/&ELSEIF/&ELSE/&ENDIF at statement, expression, and data type levels via generic `PreprocIf<T>`, &SCOPED-DEFINE/&GLOBAL-DEFINE with `PreprocEnd` lexer token, &UNDEFINE, &MESSAGE, `{&variable}` references
- **Error recovery** via `parse_program()` with synchronization on period boundaries
- **Include file references**: `{file.i}`, `{file.i args}`, `{file.i &name=value}` at both statement and expression positions
- **Include argument references**: `{0}`, `{1}`, `{2}` at both statement and expression positions

Not yet implemented: DO/FOR/REPEAT block-header ON phrases (ON ERROR UNDO, ON ENDKEY UNDO).

### Code Generation (`oxabl_codegen`)

Parses ABL keyword reference data from:
- `resources/abl_keyword_index.html` - Keyword list with reserved status and abbreviations
- `resources/abl_keyword_index.json` - Keyword types and documentation URLs
- `resources/keyword_overrides.toml` - Manual additions, overrides, and removals

Generates:
- `Kind` enum with categorized token types
- Atom list for `string_cache_codegen`
- `match_keyword()` function handling abbreviations and case-insensitive matching

## CI & Release Process

### CI (`.github/workflows/ci.yml`)

Runs on every push and PR to `master`. All checks must pass before merging:

- `cargo check` — fast compilation check
- `cargo test` — full test suite
- `cargo fmt --check` — formatting enforcement
- `cargo clippy -D warnings` — lint enforcement

### Automated Releases (`.github/workflows/release.yml`)

Uses [Release Please](https://github.com/googleapis/release-please) for fully automated versioning and changelogs.

**How it works:**

1. Write commits using [Conventional Commits](https://www.conventionalcommits.org/) format:
   - `feat: add X` — bumps minor version
   - `fix: correct Y` — bumps patch version
   - `feat!: breaking change` or footer `BREAKING CHANGE:` — bumps major version
   - Other prefixes (`chore:`, `docs:`, `refactor:`, `test:`) don't trigger a release but appear in the changelog
2. Release Please accumulates merged commits and maintains an open PR (e.g. "chore(main): release 0.2.0") with a generated changelog and version bumps across all workspace `Cargo.toml` files
3. When you merge that release PR, a GitHub Release and git tag are created automatically
4. A build+test verification step runs against the release

**Config files:**

- `release-please-config.json` — release type and which `Cargo.toml` files to update
- `.release-please-manifest.json` — tracks the current version

While pre-1.0, `bump-minor-pre-major` is enabled so breaking changes bump minor instead of major.

## Benchmarks

Benchmarks use `codspeed-criterion-compat` (Criterion with CodSpeed integration). CodSpeed CI runs on every push/PR via `.github/workflows/codspeed.yml` and auto-discovers all `[[bench]]` targets.

```bash
# Run benchmarks for a specific crate
cargo bench -p oxabl_lexer --bench lexer_bench
cargo bench -p oxabl_parser --bench parser_bench
cargo bench -p oxabl_common --bench source_map_bench
```

**When the `/codspeed` skills are available, use them for benchmark work** — setup, optimization, and flamegraph analysis.

**When implementing new features**, consider whether a new benchmark is warranted. If the feature adds a new parsing construct, expression type, or hot path, add a benchmark or extend an existing fixture file to cover it. This ensures CodSpeed catches regressions as the codebase grows.

## Current Status

- `oxabl_lexer`: MVP complete with 49 tests
- `oxabl_common/source_map`: Implemented with 10 tests
- `oxabl_ast`: Implemented with expressions, statements, and data types (parser-assigned `NodeId` on every node); `VariableDeclaration`/`DefineTempTable`/`DefineBuffer`/`DefineDataset` carry `is_shared`/`is_new_shared`/`is_new_global_shared` (ast-invariants.md §12)
- `oxabl_parser`: Actively developed with 454 tests; parses expressions, control flow, variable declarations, include file references, functions, procedures, temp-tables, error handling, OO-ABL (CLASS, METHOD, PROPERTY, INTERFACE), preprocessor directives, stream I/O, frame definitions, ON triggers (UI events, database events, key remapping), TRIGGER PROCEDURE, embedded SQL (SELECT/INSERT), implicit output/display juxtaposition, widget `:attribute` access, and bare `.field` access; captures the `[NEW [GLOBAL]] SHARED` prefix on variable/temp-table/buffer/dataset defines
- `oxabl_schema`: `.df` parser + case-insensitive `Schema` model
- `oxabl_semantic`: Declare + resolve + check passes, side tables over NodeId; schema-backed resolution (buffer `table_id` links, field validation/typing via synthesized symbols, bare-table default-buffer fallback); declare pass maps SHARED/NEW SHARED/NEW GLOBAL SHARED to `SymbolFlags`; ~210 tests
- `oxabl_lint`: 6 rules — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002; narrowed to "never referenced at all" — a variable passed as a write-back `OUTPUT`/`INPUT-OUTPUT` `RUN` argument counts as used, and a `TABLE FOR`/`DATASET FOR` parameter's read-count question is redirected to the table it names, so a used table no longer looks untouched), `unknown-table-or-field` (LINT0003, live under a loaded schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005, INFO: a block-defined variable read outside its block and never assigned outside it — may still hold its default value), `assigned-but-never-read` (LINT0006, WARN: a dead store — written and never read — reported at the write site, falling back to the declaration when the write form is not an assignment; write-back `RUN` arguments stay with #125); per-rule severity via `[workspace.lint]`. LINT0002 and LINT0006 split one population by whether anything ever wrote to the symbol, so a symbol yields exactly one diagnostic and silencing `unused-variable` no longer covers the write-only half
- `oxabl_analyze`: JSON/text dump of the semantic model with per-section versioning; `oxabl analyze <file>` subcommand with `--schema <df>`; 21 tests
- `oxabl_workspace`: file-system abstraction + `oxabl.toml` config; `resolved_include_paths` auto-discovers `oxabl.toml` (nearest-ancestor walk) and merges `[workspace.sources].include_paths` with CLI `-I` flags (CLI-first, first-match-wins PROPATH)
- Preprocessor include resolution: an unresolvable `{include}` emits a **loud** `PREPROC007` warning ("symbols it declares cannot be checked") that the CLI always surfaces (stderr for `check`, stderr + `preproc_diagnostics` JSON for `analyze`) — one honest diagnostic at the true cause instead of a flood of downstream `undefined-symbol` false positives
- Workspace total: ~1000 tests passing
