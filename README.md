# Oxabl

[![CodSpeed](https://img.shields.io/endpoint?url=https://codspeed.io/badge.json)](https://codspeed.io/oxabl-project/oxabl?utm_source=badge)


High performance oxidized tooling for Progress ABL, written in Rust.

## Unofficial

No affiliation with Progress.

## Project Roadmap

**Goal**: A high performance suite of tooling to make ABL development fast and effective, without a dependency on the ABL compiler.

### Tooling

These are the current high-priority goals for oxabl tooling. As it stands, oxabl has laid the groundwork for high-performance pre-processing, parsing, and semantic analysis. Next comes tooling on top of that.

- Library / embedding API
  - Use oxabl as a single Rust dependency — no wiring up the individual `oxabl_*` sub-crates
  - Curated umbrella surface: `oxabl::parse`, `analyze` + `AnalyzeOptions`, `format_source`, `render_diagnostics`, serde-serializable diagnostics, a streaming `Lexer` iterator, and `Schema::from_df_dir`, plus named modules (`oxabl::ast`, `parser`, `semantic`, `lint`, `schema`, `formatter`, `workspace`, …)
  - Status: Shipped — the CLI and LSP are the reference consumers; internal recovery helpers are kept off the public surface
- LSP
  - Work with oxabl to parse, lint, and format code directly in your editor
  - sub-millisecond time for single file operations
  - format/lint on save
  - Status: Experimental; usable in-editor via the sideloadable VS Code extension (`clients/vscode/`)
- Formatter
  - Format ABL code from the CLI or LSP
  - `oxabl format <path> [--check|--stdout] [--style <preset|path>]`
  - Status: CLI available (`oxabl format`); LSP integration shipped — format-on-save through the VS Code extension
- Try-it-yourself
  - A "try it in 10 seconds" demo in the browser
  - Make WASM a compile target in the release pipeline, past a code block into the browser, lint and format instantly
  - Status: Unstarted
- Conformance harness
  - A real-world test suite to ensure oxabl is conformant with all ABL fragments and the compiler
  - Status: oxabl uses a private corpus and has started building a public, open-source corpus that will feature several real-world example ABL projects to make use of as many ABL built-ins as possible.
- Linter
  - Lint rule engine
  - Public API for creating new lint rules and submitting them upstream for inclusion in oxabl's default rule set
  - Status: a first set of rules ships today — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002), `unknown-table-or-field` (LINT0003, live under a loaded `.df` schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005), and `assigned-but-never-read` (LINT0006) — configurable per-project via `oxabl.toml`. LINT0002 and LINT0006 divide one population between them: a variable never referenced at all is LINT0002's, while one that is written and never read is a dead store reported by LINT0006 at the assignment, so silencing `unused-variable` alone no longer silences the write-only half. Surfaced in-editor through the VS Code extension and as diagnostics from `oxabl check`. Still experimental; no public API for extending yet.
- Easy one-line installer and VS Code extension for getting started
  - Status: VS Code extension available (experimental, sideload) — build a VSIX with `clients/vscode/scripts/build-vsix.sh`; it launches `oxabl lsp` for format-on-save, live diagnostics, and `oxabl.toml` schema completion. One-line installer: not started.
- Build system
  - Incremental compiling (via the Progress compiler)
  - Remote cache
  - Status: Experimental, not available

## Project Status

The lexer, source map, ast, and parser crates are passing 100% of our test suite against our corpus.

Requirements:
- `oxabl_lexer`: Stable in `crates/oxabl_lexer`.
  - Produces tokens against all known ABL keywords, primitive datatypes, operators, and identifiers.
  - Correctly tokenises our corpus.
  - Benchmarks and token dumps in `crates/oxabl_lexer/benches` and `crates/oxabl_lexer/examples` using a test file in `resources/bench_keywords.abl`.
  - No new features planned, the lexer is complete, and will (most likely) only receive bug fixes and performance improvements.
- `source_map`: Stable in `crates/oxabl_common`.
  - Produces line and column numbers from byte offsets stored in tokens.
  - Used in our token dumps and benchmarks.
  - Souce maps generated from the corpus line up to it's source accurately.
  - No new features planned, the source_map is complete, and will (most likely) only receive bug fixes and performance improvemnets.
- `oxabl_ast`: Implemented in `crates/oxabl_ast`
  - Defines literals, statements, expressions, variable definitions, control flow, and data types.
  - MVP complete, still getting new features to support better diagnostics and formatting.
- `oxabl_parser`: MVP has been completed, parses 100% of our corpus code base. Parses:
  - Expressions with proper operator precedence
  - Declarations
  - Statements
  - OO-ABL
  - Preprocessor statements
  - Include file references and positional argument references
  - Postfix operations, and
  - Has error recovery via synchronization on period boundaries
  - Still getting new features, bug fixes, and improvements.
- `oxabl check` / `oxabl analyze`: CLI entry points in `crates/oxabl`.
  - `check` walks a directory (or single file) for ABL files (`.p`, `.w`, `.cls`) and reports parse pass/fail counts, error locations, and top error patterns.
  - `analyze` runs the full parse → semantic → lint pipeline over a file and dumps the resolved model + diagnostics (text or `--json`, with `--schema <file|dir>` for schema-backed resolution).
  - Both are transitional scaffolding: the intended direction is a ruff/cargo-shaped `check` that surfaces lint + format issues, built on shared library pipelines every client drives identically (see #120). Their `--json` shapes are not yet a stable contract.
  - Usage: `cargo run -p oxabl -- check <path> --preprocess -I <include-path>`

Current Work: cross-file/workspace semantic resolution (the ceiling on lint accuracy), continued dogfood-driven trust-hardening, and reshaping the CLI onto shared lint/format pipelines. Shipped: the semantic layer, the layout formatter (CLI + LSP), the language server, the VS Code extension, and the curated `oxabl` public API.

## Benchmarks

Oxabl's priority is correctness and speed. A typical file can be parsed in sub-millisecond time, a 10k file codebase is done in a few seconds (with preprocessing off)

Benchmarks are run with `cargo bench -p <crate>`. Each crate has its own benchmark so we can track the performance of individual components in the toolset.

These are not sanitized benchmarks — they were run on real hardware with normal background processes, similar to how a developer would actually use the tools. The CI benchmarks are run by CodSpeed.

### Intel i7-8550U Laptop

**Hardware:** Intel Core i7-8550U (8) @ 4.00 GHz, 15.37 GiB RAM, Linux 6.19.10-arch1-1

#### Source Map (`oxabl_common`)

| Benchmark                | Time (min)  | Time (avg)  | Time (max)  | Throughput (avg)  |
| ------------------------ | ----------- | ----------- | ----------- | ----------------- |
| source_map/construction  | 22.341 µs   | 22.636 µs   | 23.002 µs   | 728.99 MiB/s      |
| source_map/lookup        | 98.447 ns   | 98.853 ns   | 99.361 ns   | 50.580 Melem/s    |

#### Lexer (`oxabl_lexer`)

| Benchmark            | Time (min)  | Time (avg)  | Time (max)  | Throughput (avg)  |
| -------------------- | ----------- | ----------- | ----------- | ----------------- |
| lexer/keywords       | 248.71 µs   | 249.82 µs   | 250.95 µs   | 66.053 MiB/s      |
| lexer/strings        | 24.602 µs   | 24.727 µs   | 24.862 µs   | 81.301 MiB/s      |
| lexer/comments       | 13.988 µs   | 14.026 µs   | 14.069 µs   | 175.70 MiB/s      |
| lexer/numeric        | 23.294 µs   | 23.396 µs   | 23.523 µs   | 71.701 MiB/s      |
| lexer/preprocessor   | 31.775 µs   | 31.925 µs   | 32.079 µs   | 80.476 MiB/s      |

#### Parser (`oxabl_parser`)

| Benchmark              | Time (min)  | Time (avg)  | Time (max)  | Throughput (avg)  |
| ---------------------- | ----------- | ----------- | ----------- | ----------------- |
| parser/full_program    | 305.47 µs   | 306.08 µs   | 306.75 µs   | 53.913 MiB/s      |
| parser/expressions     | 93.108 µs   | 93.943 µs   | 94.915 µs   | 24.506 MiB/s      |
| parser/declarations    | 54.445 µs   | 54.707 µs   | 55.016 µs   | 46.928 MiB/s      |
| parser/control_flow    | 82.733 µs   | 83.156 µs   | 83.626 µs   | 28.075 MiB/s      |
| parser/oo_abl          | 76.691 µs   | 76.973 µs   | 77.287 µs   | 51.281 MiB/s      |
| parser/temp_tables     | 50.913 µs   | 51.169 µs   | 51.424 µs   | 50.303 MiB/s      |
| parser/procs_funcs     | 87.470 µs   | 88.238 µs   | 88.983 µs   | 39.352 MiB/s      |
| parser/datasets        | 46.195 µs   | 46.292 µs   | 46.405 µs   | 54.531 MiB/s      |

### Token Dumps

**Full token dump**:
`cargo run -p oxabl_lexer --example dump_tokens`

**Just errors**
`cargo run -p oxabl_lexer --example dump_tokens -- --errors`

**Just summary**
`cargo run -p oxabl_lexer --example dump_tokens -- --summary`

## Optimizations

The current speeds of oxabl are for enough for interactive use. The goal is to get these speeds faster, or maintain the current speed, never decline.

## Contributing

### CI

Every push and PR to `master` runs the following checks (all must pass):

- `cargo check` — compilation
- `cargo test` — full test suite
- `cargo fmt --check` — formatting
- `cargo clippy -D warnings` — lints

### Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/) format. This drives the automated release process:

- `feat: add X` — new feature (bumps minor version)
- `fix: correct Y` — bug fix (bumps patch version)
- `feat!: breaking change` — breaking change (bumps major, or minor while pre-1.0)
- `chore:`, `docs:`, `refactor:`, `test:` — won't trigger a release, but appear in the changelog

### Releases

Releases are fully automated via [Release Please](https://github.com/googleapis/release-please):

1. Merge PRs with conventional commit messages into `master`
2. Release Please accumulates commits and maintains an open release PR with a generated changelog and version bumps across all `Cargo.toml` files
3. When the release PR is merged, a GitHub Release and git tag are created automatically

No manual version bumping is needed.

## CodeGen

We generate code for all the keywords and operators to use within the project. Use these commands to generate the code:

```rust
cargo run -p oxabl_codegen -- <command>
```

Valid commands are:
- `kind`
  - generates the `kind.rs` file for the lexer.
- `atoms`
  - generates the `build.rs` file for the lexer.
- `summary`
  - outputs status and usage
- No command
  - generates all files

Commands write generated files directly to their target locations. Generated files include a "DO NOT EDIT" header. No manual file redirection is needed.

# License

[MIT[(https://github.com/oxabl-project/oxabl/blob/master/LICENSE)
