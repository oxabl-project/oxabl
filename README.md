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
  - Curated umbrella surface: `oxabl::try_parse`, `try_analyze` + `AnalyzeOptions`, `try_format_source`, `render_diagnostics`, serde-serializable diagnostics, a streaming `Lexer` iterator, and `Schema::from_df_dir`, plus named modules (`oxabl::ast`, `parser`, `semantic`, `lint`, `schema`, `formatter`, `workspace`, …)
  - **Fallible by default.** The lexer and parser can panic on some malformed input, so the entry points that matter come in guarded form: `try_parse`, `try_analyze` / `try_analyze_with_fs`, and `try_format_source` return the panic as an `InternalPanic` carrying its message instead of unwinding into your process. The panicking originals (`parse`, `analyze`, `format_source`) remain for compatibility but are deprecated. A recovered *parse error* is not a panic — it still arrives in `Program.errors`, and a formatter *bail* still arrives as a `FormatBail` — so the guard adds one arm and changes nothing else. The guarantee depends on the unwinding panic strategy; a `panic = "abort"` profile silently reduces every guard to a pass-through, and the guard is a documented pass-through on `wasm32-unknown-unknown` (see below).
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
  - WASM is a compile target in CI and the release pipeline; paste a code block into the browser, lint and format instantly
  - Status: Shipped — the browser playground runs the real thing, entirely client-side. `oxabl_wasm` is a thin JSON adapter over the same `oxabl_pipeline` lint and format handles the CLI and the language server drive, so a diagnostic and a reformat in the browser match what you get after installing. That match is now a test, not a claim: a cross-client parity suite asserts the same source yields identical codes, severities, byte spans, and sources through the pipeline, the CLI, the LSP, and these bindings. It caught a real divergence on its first run — two rules came back at a different severity in the browser because a directly-constructed config and a resolved one carried different default severity tables. The first slice is deliberately single-file: no includes, no `.df` schema (so `unknown-table-or-field` is inert), and no `oxabl.toml`, so per-rule severity and style config don't apply. Those are *absent capabilities*, not different behavior — the parity suite asserts they are unavailable rather than asserting a different answer. The website serves the released artifact and the UI around it; this repo owns the build.
- Conformance harness
  - A real-world test suite to ensure oxabl is conformant with all ABL fragments and the compiler
  - Status: oxabl is building a public, open-source corpus featuring several real-world example ABL projects, to exercise as many ABL built-ins as possible.
- Linter
  - Lint rule engine
  - Public API for creating new lint rules and submitting them upstream for inclusion in oxabl's default rule set
  - Status: a first set of rules ships today — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002), `unknown-table-or-field` (LINT0003, live under a loaded `.df` schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005), and `assigned-but-never-read` (LINT0006) — configurable per-project via `oxabl.toml`. LINT0002 and LINT0006 divide one population between them: a variable never referenced at all is LINT0002's, while one that is written and never read is a dead store reported by LINT0006 at the assignment, so silencing `unused-variable` alone no longer silences the write-only half. Around thirty ABL statement forms are recognized by the parser but not modelled (`PUT`, `EXPORT`, `UPDATE`, `ENABLE`, embedded SQL, …); their identifiers are now harvested lexically and best-effort-resolved, so all three count-gated rules stay quiet about a variable one of them touches rather than reporting it wrongly. That suppression is coarse — per-symbol and file-wide — so `oxabl analyze` reports how many symbols it could not fully judge. The handful of forms that name a *table* without reading a field of it (`DEFINE BUFFER`, `DEFINE PARAMETER BUFFER`, `EMPTY TEMP-TABLE`, `DEFINE QUERY`, `OPEN QUERY`) are treated more precisely: they credit a real read on the table, so a temp-table used only that way no longer looks untouched. Surfaced in-editor through the VS Code extension and as diagnostics from `oxabl check`. Still experimental; no public API for extending yet.
  - **An `.i` opened directly is analyzed as a fragment.** Names its missing includer could supply stay silent, as do the three whole-unit count rules, while parse errors and locally provable findings still surface. The CLI and analyze coverage channels say that the includer was absent; an `.i` expanded through a `.p`/`.w`/`.cls`/`.v` root remains ordinary textual input to that complete compilation unit.
  - **`undefined-symbol` now reports names absent from your configured search paths.** With a workspace index attached — which is every `oxabl check` run — a `USING` import, a `NEW`, or a literal `RUN` target that no configured path supplies is reported at error severity. ABL cannot reference a symbol or a procedure whose code is not on the PROPATH, so the name genuinely is undefined rather than merely unseen. Set your paths in `[workspace.sources].include_paths` in `oxabl.toml` (relative to that file, searched in order, first match wins), or pass directories with `-I`; every such finding carries a help line naming that configuration, because a missing source root produces the same finding as missing code. Four things stay deliberately silent: names in the `Progress.*`, `OpenEdge.*`, `System.*`, and `Microsoft.*` namespaces, which ship with the AVM and have no source on any path; a runtime-computed `RUN VALUE(...)` target, which no indexing could resolve; a member a class declares but does not expose to the caller, which exists and so is an access question rather than a missing name; and every cross-file name at all when no path is configured, since "we did not look" is then the only truthful answer. The `AS CLASS pkg.Missing` declaration spelling is also silent for now — the AST carries no span for that name, so there is nothing to underline.
- Easy one-line installer and VS Code extension for getting started
  - Status: VS Code extension available (experimental, sideload) — build a VSIX with `clients/vscode/scripts/build-vsix.sh`; it launches `oxabl lsp` for format-on-save, live diagnostics, and `oxabl.toml` schema completion. One-line installer: not started.
- Build system
  - Incremental compiling (via the Progress compiler)
  - Remote cache
  - Status: Experimental, not available

## Project Status

The lexer, source map, ast, and parser crates are stable and green across the workspace test suite.

Requirements:
- `oxabl_lexer`: Stable in `crates/oxabl_lexer`.
  - Produces tokens against all known ABL keywords, primitive datatypes, operators, and identifiers.
  - Correctly tokenises the full keyword, operator, and literal surface, including abbreviations.
  - Benchmarks and token dumps in `crates/oxabl_lexer/benches` and `crates/oxabl_lexer/examples` using a test file in `resources/bench_keywords.abl`.
  - No new features planned, the lexer is complete, and will (most likely) only receive bug fixes and performance improvements.
- `source_map`: Stable in `crates/oxabl_common`.
  - Produces line and column numbers from byte offsets stored in tokens.
  - Used in our token dumps and benchmarks.
  - Source maps line up to their source accurately.
  - No new features planned, the source_map is complete, and will (most likely) only receive bug fixes and performance improvements.
- `oxabl_ast`: Implemented in `crates/oxabl_ast`
  - Defines literals, statements, expressions, variable definitions, control flow, and data types.
  - MVP complete, still getting new features to support better diagnostics and formatting.
- `oxabl_parser`: MVP has been completed. Parses:
  - Expressions with proper operator precedence
  - Declarations
  - Statements
  - OO-ABL
  - Preprocessor statements
  - Include file references and positional argument references
  - Postfix operations, and
  - Has error recovery via synchronization on period boundaries
  - Still getting new features, bug fixes, and improvements.
- CLI entry points in `crates/oxabl`. The advertised surface is exactly four commands — `check`, `format`, `lsp`, and `schema` — plus two **hidden but fully supported** instruments described below.
  - `check` walks a directory (or single file) for ABL files and reports lint findings plus formatting drift in two channels. This is the pre-commit gate: exit 1 when either channel has something to say.
  - `format` fixes layout in place, or reports/prints without writing (`--check`, `--stdout`).
  - Usage: `cargo run -p oxabl -- check <path> --preprocess -I <include-path>`
  - Their `--json` shapes are not yet a stable contract.
- Hidden commands. Both are real, documented, and reachable — `oxabl <command> --help` works — they simply do not appear in `oxabl --help`, because each answers a question about *oxabl* rather than about your source, and neither belongs in the surface a new user reads.
  - `oxabl conformance <path>` — the parser-refinement instrument: how many files parse, which fail and where, and the ranked error patterns behind the failures.
  - `oxabl analyze <file>` — a single file's resolved semantic model, dumped as a per-section-versioned JSON envelope or as text (`--schema <file|dir>` for schema-backed resolution). Introspection, not a gate: it exits 0 whatever it finds. Eight sections are emitted — `scopes`, `symbols`, `types`, `references`, `diagnostics`, `preproc`, `coverage`, and `dependencies` — each carrying its own version under `sections`, so a change to one leaves the others' numbers alone. `dependencies` is where cross-file resolution is observable: which files the run's workspace index actually consulted, and which cross-file names came back empty and why.
- `oxabl_wasm`: browser bindings in `crates/oxabl_wasm`.
  - Three `wasm-bindgen` exports — `analyze_source`, `format_source`, and `version()` (a crate version plus a build identifier, so a crash report names the exact artifact a hand-vendored copy is running) — the first two returning JSON: diagnostics carry source/severity/code/message, byte offsets, and line/column positions; a format result carries the new source, a `changed` flag, and an `error` string when the formatter declines to format (in which case the original source comes back untouched). Line and column come from the shared position helper the CLI's text output uses, so the two cannot drift.
  - Contains no ABL behavior. It is a transport adapter over the shared pipelines, which keeps every client on one implementation. A refusal collapses to one `error` string here because the wire shape has one field for it, while the pipeline keeps a deliberate bail and a contained panic apart for the clients that can tell them apart.
  - CLI-only dependencies live behind the `oxabl` crate's default-on `cli` feature so the library compiles for `wasm32`; a CI job builds the wasm target on every push.

Current Work: continued dogfood-driven trust-hardening, and typing `:`-qualified member and method-call expressions — the half of the cross-file population the type lattice still does not judge. Shipped: the semantic layer, the layout formatter (CLI + LSP), the language server, the VS Code extension, the curated `oxabl` public API, the browser WASM playground, the shared lint/format pipelines every client now drives, and cross-file/workspace resolution — inheritance chains, `USING` imports, literal `RUN` targets, and `SHARED` producers now link across files, with the CLI and the editor reaching identical answers from the same shared index. (The browser playground stays single-file: its exports take one source and no sibling files, so there is nothing for an index to look at.)

### WebAssembly browser package

The browser package is owned by this repository. It contains no ABL-specific
implementation: `crates/oxabl_wasm` only converts the shared analysis and
formatting results into a browser-friendly JSON wire shape.

```bash
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli --version 0.2.126 --locked
./scripts/build-wasm.sh
```

The output is written to `target/wasm-web` by default. Pass a directory as the
first argument to stage it for a static-site consumer. Releases attach the
packaged browser artifact as `oxabl-wasm-web.tar.gz`.

The CLI version must match the `wasm-bindgen` crate version, which is pinned
exactly (`=0.2.126`) rather than as a caret range — see the recovery notes below.
A mismatch fails at bindgen time, which is *after* `cargo build` succeeds, so the
CI wasm job will not catch it; only this script or a release will.

#### Crash recovery in the browser

`wasm32-unknown-unknown` builds with `-Cpanic=abort` on stable Rust, so a Rust
panic there cannot be caught inside wasm: it lowers to an `unreachable` trap that
reaches JS as a `WebAssembly.RuntimeError`. `catch_panic` is therefore a
documented pass-through on this target, and the browser gets a different
mechanism with two halves:

1. A panic hook stashes the formatted panic message on
   `globalThis.__oxablPanicMessage` before the abort. std runs the hook to
   completion before the panic runtime aborts, so the write lands before the trap.
2. The consumer's `catch` reads that message and calls `__wbg_reset_state()`,
   generated by `--experimental-reset-state-function`. That builds a fresh
   instance from the already-compiled module with Rust statics reset and re-runs
   the start function — which re-arms the hook, so a second crash is reported and
   recovered just like the first.

So a visitor sees a readable message naming the failure and an engine that keeps
working, instead of a raw trap string and a demo bricked until page reload.

Two build-time assertions in `scripts/build-wasm.sh` protect this, because **no
CI job runs `wasm-bindgen` at all** — the `wasm` job stops at `cargo build`:

- `__wbg_reset_state` is exported. Drop the flag and the artifact still builds
  perfectly; the consumer just calls a function that is not there and recovery
  silently stops working. This assertion is the only thing that turns that into a
  failure.
- No exception-handling instructions were injected. `--force-enable-abort-handler`
  would reach the same reinit machinery, but it injects `try_table`/exnref
  instructions and two `WebAssembly.Tag` imports, raising the browser floor to
  roughly Chrome 128 / Firefox 131 / Safari 18.4 — where the module fails to
  *instantiate* rather than degrading. That is a bad trade on a first-contact
  surface, so the assertion pins the decision.

`./scripts/build-wasm.sh <dir> --verify` additionally enables the `debug-panic`
feature, adding a `debug_panic()` export. No ABL input reaches a parser panic, so
that export is the only way to exercise the crash path by hand. It must never be
used for a release build, and the release workflow does not.

Hangs are **not** covered: an infinite loop freezes the main thread, is
indistinguishable from a trap to a visitor, and needs a Web Worker with a timeout.

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
