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
# Show summary and usage
cargo run -p oxabl_codegen

# Generate kind.rs (token type enum)
cargo run -p oxabl_codegen -- kind > crates/oxabl_lexer/src/kind.rs

# Append keyword match function to kind.rs
cargo run -p oxabl_codegen -- match >> crates/oxabl_lexer/src/kind.rs

# Generate atom list (inner content only, for manual insertion into build.rs)
cargo run -p oxabl_codegen -- atoms
```

**Important**:
- The `atoms` command only outputs the `string_cache_codegen` call, not the full `build.rs` file. You must manually wrap it with `use std::env; use std::path::Path;` and `fn main() { ... }`.
- Use `>` to overwrite or `>>` to append. The codegen does not clear files automatically.

## Architecture

### Workspace Structure

- `crates/oxabl` - Main library (future unified API)
- `crates/oxabl_lexer` - Tokenizer for ABL source code (MVP complete)
- `crates/oxabl_parser` - Parser (not yet started, depends on lexer)
- `crates/oxabl_common` - Shared utilities including `SourceMap`
- `crates/oxabl_codegen` - Code generation tool for lexer keywords

### Lexer (`oxabl_lexer`)

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
- Line (`//`) and block (`/* */`) comments

### Source Map (`oxabl_common`)

Converts byte offsets (stored in tokens) to human-readable line/column positions. Uses binary search over precomputed line start offsets for O(log n) lookups.

### Code Generation (`oxabl_codegen`)

Parses ABL keyword reference data from:
- `resources/abl_keyword_index.html` - Keyword list with reserved status and abbreviations
- `resources/abl_keyword_index.json` - Keyword types and documentation URLs
- `resources/keyword_overrides.toml` - Manual additions, overrides, and removals

Generates:
- `Kind` enum with categorized token types
- Atom list for `string_cache_codegen`
- `match_keyword()` function handling abbreviations and case-insensitive matching

## Current Status

- `oxabl_lexer`: MVP complete with 27 tests
- `oxabl_common/source_map`: Implemented but needs test coverage
- `oxabl_ast`: Not started
- `oxabl_parser`: Not started
