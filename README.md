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
  - Status: a first set of rules ships today — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002), `unknown-table-or-field` (LINT0003, live under a loaded `.df` schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005), and `assigned-but-never-read` (LINT0006) — configurable per-project via `oxabl.toml`. LINT0002 and LINT0006 divide one population between them: a variable never referenced at all is LINT0002's, while one that is written and never read is a dead store reported by LINT0006 at the assignment, so silencing `unused-variable` alone no longer silences the write-only half. Around thirty ABL statement forms are recognized by the parser but not modelled (`PUT`, `EXPORT`, `UPDATE`, `ENABLE`, embedded SQL, …); their identifiers are now harvested lexically and best-effort-resolved, so all three count-gated rules stay quiet about a variable one of them touches rather than reporting it wrongly. That suppression is coarse — per-symbol and file-wide — so `oxabl analyze` reports how many symbols it could not fully judge. The handful of forms that name a *table* without reading a field of it (`DEFINE BUFFER`, `DEFINE PARAMETER BUFFER`, `EMPTY TEMP-TABLE`, `DEFINE QUERY`, `OPEN QUERY`) are treated more precisely: they credit a real read on the table, so a temp-table used only that way no longer looks untouched. Surfaced in-editor through the VS Code extension and as diagnostics from `oxabl check`. Still experimental. `oxabl_lint::Registry` lists the rules with their metadata (code, name, group, description, default severity, stability), accepts your own rules, and runs a subset; `lint_file` stays the run-everything entry point. See [Lint configuration](#lint-configuration) for rule groups, per-directory settings and baselines.
  - **An `.i` opened directly is analyzed as a fragment.** Names its missing includer could supply stay silent, as do the three whole-unit count rules, while parse errors and locally provable findings still surface. The CLI and analyze coverage channels say that the includer was absent; an `.i` expanded through a `.p`/`.w`/`.cls`/`.v` root remains ordinary textual input to that complete compilation unit.
  - **`undefined-symbol` now reports names absent from your configured search paths.** With a workspace index attached — which is every `oxabl check` run — a `USING` import, a `NEW`, or a literal `RUN` target that no configured path supplies is reported at error severity. ABL cannot reference a symbol or a procedure whose code is not on the PROPATH, so the name genuinely is undefined rather than merely unseen. Set your paths in `[workspace.sources].include_paths` in `oxabl.toml` (relative to that file, searched in order, first match wins), or pass directories with `-I`; every such finding carries a help line naming that configuration, because a missing source root produces the same finding as missing code. Four things stay deliberately silent: names in the `Progress.*`, `OpenEdge.*`, `System.*`, and `Microsoft.*` namespaces, which ship with the AVM and have no source on any path; a runtime-computed `RUN VALUE(...)` target, which no indexing could resolve; a member a class declares but does not expose to the caller, which exists and so is an access question rather than a missing name; and every cross-file name at all when no path is configured, since "we did not look" is then the only truthful answer. The `AS CLASS pkg.Missing` declaration spelling is also silent for now — the AST carries no span for that name, so there is nothing to underline.
  - **The preprocessor answers the AVM's predefined names from `[workspace.preprocessor]` in `oxabl.toml`.** The AVM reads `{&OPSYS}`, `{&WINDOW-SYSTEM}`, `{&BATCH-MODE}`, `{&PROCESS-ARCHITECTURE}` and `{&PROVERSION}` from the machine that compiles. oxabl has no such machine, so you state the environment and every `&IF` over these names decides against it. The defaults describe a 64-bit Unix batch compile with a character UI: `opsys = "UNIX"`, `window-system = "TTY"`, `batch-mode = true` (expands to `yes`), `process-architecture = 64`. You can override each key (for example `opsys = "WIN32"`, `window-system = "MS-WINDOWS"`, `batch-mode = false`). `proversion` has no default because a tool cannot guess it: set it (for example `proversion = "12.8"`), or an `&IF` condition that reads `{&PROVERSION}` or the `PROVERSION` function raises the always-loud warning `PREPROC009` instead of silently deciding against an empty string. The `OPSYS` and `PROVERSION` functions in an `&IF` read the same table. `databases` lists the logical names of the databases connected when the code compiles, in the order of connection (for example `databases = ["ledger", "stock"]`), and the `DBTYPE`, `CONNECTED` and `LDBNAME` functions of an `&IF` answer from it. The list is complete: a database that is not in it is not connected. Use `[]` for a compile with no database. `DICTDB`, the alias that the AVM gives the first connected database, answers as the first name in the list. Without the key, each `[workspace.schema]` file names one database: the file name without `.df` is the logical name, so `schema/ledger.df` connects `ledger`. A file name is not always the logical name (a versioned dump file, for example), and the files are in the order of `[workspace.schema]`, not of connection. So this list is not complete and not ordered: a name that is not in it is a guess (`PREPROC010`), not an absent database, and so are `LDBNAME("DICTDB")` and `DBTYPE("DICTDB")`. `CONNECTED("DICTDB")` is true when the list is not empty. Set the key when a logical name differs from its file name. `DBTYPE("ledger")` answers `PROGRESS` for a listed database, or the type that `database-types` gives (`database-types = { stock = "ORACLE" }`), and `?` for one that a stated list does not hold. `CONNECTED("ledger")` is true for a listed one, and false for one that a stated list does not hold. `LDBNAME("ledger")` answers the name as listed, or `?` for one that a stated list does not hold. Only the logical-name form is modelled: `LDBNAME(1)` and `DBTYPE(1)` depend on the order of connection, so they are a guess. With neither a list nor a schema, every call to one of these functions is a guess too. `{&FILE-NAME}` (the name the file was analysed under, or the include name as written), `{&LINE-NUMBER}` (the line in the current file) and `{&SEQUENCE}` (0, 1, 2, … per reference, shared by the whole compilation unit) are computed from the source and need no configuration. An `&IF` condition is evaluated for the operators and functions whose result oxabl can compute exactly: comparisons (two strings compare as text, two numbers as numbers, decimals exactly; a string against a number is a guess), `BEGINS`, `MATCHES`, `AND`, `OR`, `NOT`, the unknown value `?` (equal only to itself; every other comparison with it is unknown, and `NOT`, `AND` and `OR` follow the three-valued rules, and an unknown condition is false, so `&ELSE` or `&ELSEIF` is taken), integer `+ - * MODULO`, `DEFINED` (0 undefined, 1 `&GLOBAL-DEFINE`, 2 include argument, 3 `&SCOPED-DEFINE`), `OPSYS`, `PROVERSION`, `DBTYPE`, `CONNECTED`, `LDBNAME`, `LOGICAL`, `INTEGER`, `DECIMAL` (whole values), `STRING`, `SUBSTRING`, `LENGTH`, `TRIM`, `LEFT-TRIM`, `RIGHT-TRIM`, `LC`, `INDEX`, `R-INDEX`, `ENTRY`, `NUM-ENTRIES`, `LOOKUP`, `ABSOLUTE`, `MAXIMUM` and `MINIMUM` (all numbers or all strings). Any other name, function or shape (`SEARCH`, `KEYWORD`, `/`, an empty expression, …) raises the always-loud warning `PREPROC010`, and the file's edge set is `Degraded` with reason `preprocessor_loss`: the branch is a guess, so the includes inside it may or may not be dependencies. `SEARCH(...)` asks whether a file exists, which no evaluator can know, so it is always such a guess. When an operand settles an `AND` (false) or an `OR` (true) without a guess, the guesses in the other operands of that chain, before it or after it, cannot change the result and raise nothing. A syntax error, or a comparison of a string with a number, is different: the AVM rejects the condition whatever the other operands are, so it always raises `PREPROC010`. An include reference with whitespace inside the braces (`{ name.i }`) expands like `{name.i}`; one whose brace never closes raises `PREPROC008` and is reported as an unresolved include.
  - **Case-insensitive lookup is opt-in.** A class or `RUN` target is found by its name as written, then by its lower-case fold. Set `case_insensitive_lookup = true` under `[workspace.sources]` to also match a file whose path differs in case from both (`pkg.UTIL` for `pkg/Util.cls`). It lists directories on a miss, so it costs more. Two distinct matches leave the name unresolved. A custom `FileSystem` supplies its own matching by overriding `find_ignoring_case`.
  - **A `RUN` target with no extension is tried with the extensions in `[workspace.sources].run_extensions`.** `RUN post-order` can name a program file, but it can also name an internal procedure. oxabl looks for `post-order.p` and then `post-order.w`, and the first file it finds is the dependency. To change the extensions or their order, set the list in `oxabl.toml` (for example `run_extensions = ["w", "p", "pp"]`). A leading dot is ignored, and an entry that is empty or holds a path separator is dropped with a warning. Each extension is tried as written and then in lower case. The default is `["p", "w"]`. A bare name that no file supplies is an internal procedure of a running program, so the dependency edge set has no row for it. A target with an extension or a path (`RUN a/b.p`) still gets a row when no configured path supplies it.
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

### Lint configuration

Lint rules are in groups. The `LINT` rules are in the `default` group, which is always on. The `strict.transactions`, `strict.locks`, `strict.access`, `strict.handles`, `strict.nulls` and `strict.calls` groups are off until you enable them. The plan for all strict rules is in `docs/design/strict-analysis-plan.md`. These strict rules are available now. They are in preview and report at `warn`:

| Code | Name | Group | Reports |
|---|---|---|---|
| CL001 | `run-value` | `strict.calls` | `RUN VALUE(...)` with a computed target |
| CL002 | `dynamic-invoke` | `strict.calls` | `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `DYNAMIC-PROPERTY`, `CREATE CALL` |
| CL005 | `shared-state` | `strict.calls` | `NEW SHARED`, `NEW GLOBAL SHARED` and `SHARED` definitions |
| TX006 | `missing-no-undo` | `strict.transactions` | a variable, parameter, temp-table or property without `NO-UNDO` |

These four rules are context-free: each finding depends only on the text of its own file, expanded with no arguments and no includer defines. For most files that is the whole answer. It is not for a parametrised include fragment: `{1}`, `{&name}` and `&IF DEFINED(...)` resolve against no includer, so a statement that holds one can compile differently in each includer.

**Include files.** A strict rule reports code from an include in the include file, once, and never in the program that includes it. `oxabl check <dir>` therefore also checks the `.i` files of the tree, but only for context-free rules, and only where the configuration enables one for that file. A directory that holds only such include files is checked too; `check` exits 2 for "no ABL files" only when it finds nothing to check. Other findings of a fragment (parse errors, undefined names, unused variables) stay hidden, because a fragment often does not parse or resolve alone. A baseline keys such a finding by the include's path. A finding is lower confidence when the part of its statement that the rule reads can change with the includer: a `{&name}`, `{n}`, `{*}` or `{file.i <args>}` reference, an `&IF`, or an argument-less `{file.i}` that did not resolve or has a reference or an `&IF` of its own. An include guard does not count (also the `&IF DEFINED(x) &THEN &ELSE <define x> &ENDIF` form); a comment is not part of a statement; a `{&name}` that an earlier `&SCOPED-DEFINE` in the same fragment gave a fixed value outside any `&IF` does not count (a `&GLOBAL-DEFINE` still does); and `RUN VALUE` (CL001) counts only for its target. The help of such a finding says that it depends on the including file, and no autofix may change it. A TX006 finding on a definition with a literal `SHARED` keeps the "producer and every consumer" help; one whose modifier slot holds a reference says that an includer may make it `SHARED`. The run cannot see what an includer's arguments would add, for example a `NEW SHARED` modifier that comes from `{&mod}`, so such a finding can also be missing. Limits:

- `oxabl check <file>` checks that one file. It does not check the includes of a program. Name the `.i` file to check it.
- `check` does not find an include outside the directory that it walks, or an include with an extension other than `.i`.
- In the editor, an open `.i` file shows its context-free findings. The editor does not show the help, so it does not show the lower-confidence note.
- The text summary and `--json` count these files apart from the roots (`include_files_checked`).

See `docs/design/strict-analysis-open-questions.md` for the reasons and for the plan for rules that depend on the includer.

Configure lint in `[workspace.lint]` in `oxabl.toml`:

```toml
[workspace.lint]
enable = ["strict.calls"]       # turn on groups; "strict" selects every strict.* group
unused-variable = "info"        # a rule by kebab name ...
LINT0006 = "off"                # ... or by code

[[workspace.lint.overrides]]
files = ["legacy/**", "old/report.p"]
disable = ["strict"]            # turn off groups for these files
undefined-symbol = "warn"

[[workspace.lint.overrides]]
files = ["legacy/new"]          # a directory selects every file under it
enable = ["strict.calls"]
```

- **Severities:** `off`, `hint`, `info`, `warn` (or `warning`) and `error`. A rule that you do not configure uses its default severity.
- **Patterns:** globs relative to the directory that holds `oxabl.toml`, written with `/` and matched case-insensitively. `*` stays in one directory and `**` crosses directories.
- **Precedence**, lowest first: the rule default, then `[workspace.lint]`, then each override whose `files` match, in file order. A later override wins, so put broad patterns before narrow ones. In one table, a setting for a rule wins over a setting for its group, and `disable` wins over `enable`. A group toggle turns rules on or off but keeps the severity that an earlier table set. A severity on a rule turns the rule on, also when its group is off. A group toggle in an override replaces a setting for a single rule in `[workspace.lint]`, because an override is more specific than the global table. It does not replace a setting for a single rule in the same override or in an earlier override: overrides rank equal, so that setting stands, whatever the order. A later rule setting always wins. Example: with `CL001 = "off"` in `[workspace.lint]`, an override that enables `strict.calls` turns `CL001` on for its files. With `CL001 = "off"` in an override for `a/**`, a later override for `**/*.p` that enables `strict.calls` leaves `CL001` off in `a/`.
- **Mistakes:** an unknown rule or group name, a toggle of the `default` group, a bad pattern, and an override with no `files` patterns are warnings. oxabl ignores that entry and applies the rest. A value that is not a severity is a config error.

The CLI, the language server and the daemon apply the same settings to each file.

#### Baseline

Use a baseline to adopt a rule on existing code. The gate then fails only on new findings:

```sh
oxabl check src --baseline oxabl-baseline.json --update-baseline   # record the current findings
oxabl check src --baseline oxabl-baseline.json                     # fail only on new findings
```

- A finding matches an entry by rule code, workspace-relative path, a hash of the flagged line and span, and an occurrence number for identical findings. Whitespace runs are collapsed and letters are folded to lower case before hashing, so a change of keyword case does not change the match. The file has no line numbers, so edits that move a finding to a different line do not change the match.
- Paths are made relative to the workspace root (the directory with `oxabl.toml`, else the directory with the baseline file) after the file system resolves them, so `src/sub/../a.p` and `src/a.p` are one file, and a `..` that follows a symlink names the file that is read. On Windows the path also takes the case of the file system, so `SRC\A.P` and `src\a.p` are one file. A finding in a file outside the root is reported but never recorded; `check` says how many.
- Identical findings in one file (same rule, same text) are told apart by their order only. If you add a third identical line above two baselined ones, the count of new findings is right, but the finding that `check` shows as new is the last copy, not the line that you added.
- `check` writes the counts to stderr: `new`, `baselined`, and `stale` (fixed findings that are still in the file). With `--json`, the report has a `baseline` key with the same counts, and `diagnostics` holds only new findings.
- `--update-baseline` adds the new findings and removes the stale entries. It keeps the entries of files that the run did not check. It cannot be used with `--no-lint`.
- Exit codes do not change: `1` for a new finding, format drift or a failed file, `2` for a missing or malformed baseline. Stale entries do not fail the run.
- The file is pretty-printed JSON, sorted by path, code, hash and occurrence:

```json
{
  "version": 2,
  "entries": [
    { "path": "src/orders.p", "code": "LINT0002", "hash": "5f0c3a5e9d2b7c11", "occurrence": 0, "message": "..." }
  ]
}
```

The `message` field is only for the reader; matching does not use it. A version 1 file (which hashed the text with its case kept) is refused with an error: delete it and record the baseline again with `--update-baseline`.

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

[MIT](https://github.com/oxabl-project/oxabl/blob/master/LICENSE)
