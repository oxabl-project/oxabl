# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Agent skills

### Issue tracker

Issues and PRDs use GitHub Issues; small fixes completed immediately need no issue. See `docs/agents/issue-tracker.md`.

### Triage labels

Triage uses the canonical `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, and `wontfix` labels. See `docs/agents/triage-labels.md`.

### Domain docs

This is a single-context repository. See `docs/agents/domain.md`.

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

- `crates/oxabl` - Umbrella library (the curated public API) and the CLI binary
- `crates/oxabl_pipeline` - The shared lint and format run every client drives; sits *beneath* `oxabl_lsp`, `oxabl_wasm`, and the umbrella
- `crates/oxabl_index` - Cross-file fact extraction plus the in-run batch index; sits *beneath* `oxabl_pipeline`
- `crates/oxabl_lexer` - Tokenizer for ABL source code (MVP complete)
- `crates/oxabl_ast` - AST node definitions (expressions, statements, literals)
- `crates/oxabl_parser` - Parser for ABL source code (actively developed)
- `crates/oxabl_common` - Shared utilities including `SourceMap` and the `catch_panic` guard
- `crates/oxabl_codegen` - Code generation tool for lexer keywords

(Not exhaustive — the workspace also holds `oxabl_preprocessor`, `oxabl_semantic`, `oxabl_schema`, `oxabl_lint`, `oxabl_analyze`, `oxabl_formatter`, `oxabl_style`, `oxabl_workspace`, `oxabl_lsp`, and `oxabl_wasm`; see the status section below.)

### Lexer (`oxabl_lexer`)

The lexer should classify tokens as distinctly as possible so the parser dispatches on `Kind` enum variants (O(1) integer comparison), never on runtime string comparison or `to_uppercase()` allocations. When a new keyword is needed by the parser, add it to `keyword_overrides.toml` and regenerate — do not use `eq_ignore_ascii_case()` workarounds.

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

### Automated Releases (`.github/workflows/release.yml`)

Uses [Release Please](https://github.com/googleapis/release-please) for fully automated versioning and changelogs. Use conventional commits.

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

## ASD-STE100 Simplified Technical English

Always respond using ASD-STE100 Simplified Technical English. It is a controlled writing standard. Aerospace and defense groups made it. It helps people write clear technical text.

Key rules:
- **Use approved words only.** The standard gives a word list. Each word has one meaning.
- **Use one word for one idea.** Do not use two words for the same thing.
- **Write short sentences.** Use 20 words or less for instructions.
- **Use active voice.** Write "Turn the switch", not "The switch must be turned".
- **Write short paragraphs.** Keep one topic in each paragraph.

The goal is easy reading. Many readers are not native English speakers. Clear text helps them do the work in a safe and correct way.
