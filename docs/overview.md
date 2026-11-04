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
  - **The preprocessor answers the AVM's predefined names from `[workspace.preprocessor]` in `oxabl.toml`.** The AVM reads `{&OPSYS}`, `{&WINDOW-SYSTEM}`, `{&BATCH-MODE}`, `{&PROCESS-ARCHITECTURE}` and `{&PROVERSION}` from the machine that compiles. oxabl has no such machine, so you state the environment and every `&IF` over these names decides against it. The defaults describe a 64-bit Unix batch compile with a character UI: `opsys = "UNIX"`, `window-system = "TTY"`, `batch-mode = true` (expands to `yes`), `process-architecture = 64`. You can override each key (for example `opsys = "WIN32"`, `window-system = "MS-WINDOWS"`, `batch-mode = false`). `proversion` has no default because a tool cannot guess it: set it (for example `proversion = "12.8"`), or an `&IF` condition that reads `{&PROVERSION}` or the `PROVERSION` function raises the always-loud warning `PREPROC009` instead of silently deciding against an empty string. The `OPSYS` and `PROVERSION` functions in an `&IF` read the same table. `databases` lists the logical names of the databases connected when the code compiles, in the order of connection (for example `databases = ["ledger", "stock"]`), and the `DBTYPE`, `CONNECTED` and `LDBNAME` functions of an `&IF` answer from it. The list is complete: a database that is not in it is not connected. Use `[]` for a compile with no database. `DICTDB`, the alias that the AVM gives the first connected database, answers as the first name in the list. Without the key, each `[workspace.schema]` file names one database: the file name without `.df` is the logical name, so `schema/ledger.df` connects `ledger`. A file name is not always the logical name (a versioned dump file, for example), and the files are in the order of `[workspace.schema]`, not of connection. So this list is not complete and not ordered: a name that is not in it is a guess (`PREPROC010`), not an absent database, and so are `LDBNAME("DICTDB")` and `DBTYPE("DICTDB")`. `CONNECTED("DICTDB")` is true when the list is not empty. Set the key when a logical name differs from its file name. `DBTYPE("ledger")` answers `PROGRESS` for a listed database, or the type that `database-types` gives (`database-types = { stock = "ORACLE" }`), and `?` for one that a stated list does not hold. `CONNECTED("ledger")` is true for a listed one, and false for one that a stated list does not hold. `LDBNAME("ledger")` answers the name as listed, or `?` for one that a stated list does not hold. Only the logical-name form is modelled: `LDBNAME(1)` and `DBTYPE(1)` depend on the order of connection, so they are a guess. With neither a list nor a schema, every call to one of these functions is a guess too. `{&FILE-NAME}` (the name the file was analysed under, or the include name as written), `{&LINE-NUMBER}` (the line in the current file) and `{&SEQUENCE}` (0, 1, 2, … per reference, shared by the whole compilation unit) are computed from the source and need no configuration. An `&IF` condition is evaluated for the operators and functions whose result oxabl can compute exactly: comparisons (two strings compare as text, two numbers as numbers, decimals exactly; a string against a number is a guess), `BEGINS`, `MATCHES`, `AND`, `OR`, `NOT`, the unknown value `?` (equal only to itself; every other comparison with it is unknown, and `NOT`, `AND` and `OR` follow the three-valued rules, and an unknown condition is false, so `&ELSE` or `&ELSEIF` is taken), integer `+ - * MODULO`, `DEFINED` (0 undefined, 1 `&GLOBAL-DEFINE`, 2 include argument, 3 `&SCOPED-DEFINE`), `OPSYS`, `PROVERSION`, `DBTYPE`, `CONNECTED`, `LDBNAME`, `LOGICAL`, `INTEGER`, `DECIMAL` (whole values), `STRING`, `SUBSTRING`, `LENGTH`, `TRIM`, `LEFT-TRIM`, `RIGHT-TRIM`, `LC`, `INDEX`, `R-INDEX`, `ENTRY`, `NUM-ENTRIES`, `LOOKUP`, `ABSOLUTE`, `MAXIMUM` and `MINIMUM` (all numbers or all strings). `DEFINED` gives the kind of the newest definition of the name, as the AVM does. A scoped define or an include argument hides an older definition until it ends or is undefined. A global define of a name that has a scoped define or an include argument is an AVM error and is ignored. An `&UNDEFINE` in an include removes the visible definition wherever it was made. A condition with nothing in it (`&IF {&undefined} &THEN`) is false, as in the AVM. Any other name, function or shape (`SEARCH`, `KEYWORD`, `/`, an empty operand inside an expression, …) raises the always-loud warning `PREPROC010`, and the file's edge set is `Degraded` with reason `preprocessor_loss`: the branch is a guess, so the includes inside it may or may not be dependencies. `SEARCH(...)` asks whether a file exists, which no evaluator can know, so it is always such a guess. When an operand settles an `AND` (false) or an `OR` (true) without a guess, the guesses in the other operands of that chain, before it or after it, cannot change the result and raise nothing. A syntax error, or a comparison of a string with a number, is different: the AVM rejects the condition whatever the other operands are, so it always raises `PREPROC010`. An include reference with whitespace inside the braces (`{ name.i }`) expands like `{name.i}`; one whose brace never closes raises `PREPROC008` and is reported as an unresolved include.
  - **Case-insensitive lookup is opt-in.** A class or `RUN` target is found by its name as written, then by its lower-case fold. Set `case_insensitive_lookup = true` under `[workspace.sources]` to also match a file whose path differs in case from both (`pkg.UTIL` for `pkg/Util.cls`). It lists directories on a miss, so it costs more. Two distinct matches leave the name unresolved. A custom `FileSystem` supplies its own matching by overriding `find_ignoring_case`.
  - **Each root can search its own directory first.** Some shops compile every program with its own directory, and fixed subdirectories of it, first on the PROPATH. Set `root_relative_include_paths = [".", "rf"]` under `[workspace.sources]` and each root file searches `<its dir>` and `<its dir>/rf` before `include_paths`, for includes, classes and `RUN` targets alike. A class or program found this way reads its own includes with the paths of its own directory. One `oxabl check` run handles roots in any number of directories. The default is empty.
  - **Include path entries can name the OpenEdge installation.** An entry in `[workspace.sources].include_paths` that is `$DLC` or starts with `$DLC/` names a directory under `[workspace.sources].dlc`, the host directory that holds the include sources of the OpenEdge installation your workers use (`include_paths = ["src", "$DLC/src"]`, `dlc = "/opt/oe128-sources"`). `dlc` is a path that is specific to one machine, so `OXABL_DLC_SOURCES` overrides it. With neither set, the includes under a `$DLC/` entry do not resolve. An entry `$LIB:<name>` names a procedure library of `[build.libraries]`. A library holds r-code, so the analysis does not search it. `oxabl check`, the language server, the daemon and `oxabl build` all read these entries the same way.
  - **The files a directory walk picks up are set by `[workspace.sources].root_extensions`.** The default is `["p", "w", "cls", "v"]`, matched without regard to case. Use it for shop conventions, for example `root_extensions = ["p", "w", "cls", "v", "pp"]`. The list replaces the default, so keep the entries you need. `i` is rejected with a warning, because an include file is never a root. A file you name on the command line is always checked.
  - **Source files are UTF-8 unless `[workspace.sources].encoding` says otherwise.** Many ABL trees are stored in a single-byte code page. Set `encoding = "windows-1252"` or `encoding = "iso-8859-1"` (the default is `"utf-8"`), and every file that oxabl reads from disk is decoded that way: roots, include files, classes and `.df` schema files, in `oxabl check`, `analyze`, `format`, the language server and the daemon. Offsets and columns in a report count the decoded text. A UTF-8 byte order mark always means UTF-8, is removed whatever the setting says, and `oxabl format` writes it back. `oxabl format` also writes a file back in its own encoding. With `"utf-8"`, a file that is not valid UTF-8 cannot be read, and `check` reports it as a failure that names the setting. For a tree that mixes UTF-8 files and single-byte ones, keep `encoding = "utf-8"` and set `fallback_encoding = "windows-1252"`: a file that is not valid UTF-8 is then decoded with the fallback. A single-byte encoding decodes every file, so the fallback never applies to it. Under `windows-1252`, the five bytes the code page leaves undefined (0x81, 0x8D, 0x8F, 0x90, 0x9D) decode to the C1 control of the same value. A custom `FileSystem` decodes for itself; `oxabl_workspace::EncodedFileSystem` is the disk filesystem that follows the setting, and `RealFileSystem` stays UTF-8.
  - **A `RUN` target with no extension is tried with the extensions in `[workspace.sources].run_extensions`.** `RUN post-order` can name a program file, but it can also name an internal procedure. oxabl looks for `post-order.p` and then `post-order.w`, and the first file it finds is the dependency. To change the extensions or their order, set the list in `oxabl.toml` (for example `run_extensions = ["w", "p", "pp"]`). A leading dot is ignored, and an entry that is empty or holds a path separator is dropped with a warning. Each extension is tried as written and then in lower case. The default is `["p", "w"]`. A bare name that no file supplies is an internal procedure of a running program, so the dependency edge set has no row for it. A target with an extension or a path (`RUN a/b.p`) still gets a row when no configured path supplies it.
- Easy one-line installer and VS Code extension for getting started
  - Status: VS Code extension available (experimental, sideload) — build a VSIX with `clients/vscode/scripts/build-vsix.sh`; it launches `oxabl lsp` for format-on-save, live diagnostics, and `oxabl.toml` schema completion. One-line installer: not started.
- Build system
  - Incremental compiling (via the Progress compiler)
  - Remote cache
  - Status: Experimental, not available. `oxabl build` (a hidden command, checked against OpenEdge 12.8 by an opt-in suite) refreshes a persisted dependency graph, says which units must rebuild and why (`--plan`), and compiles them through AVM workers that your own command starts, in dependency order, with retry, crash handling and cancellation. An overlay file (`--config-overlay`) lets a tool that owns the environment of a workspace give the machine-specific settings for one run, and `--print-config` shows where each setting came from. A local cache of compile outputs lets two copies of a workspace share compiles, and a binary built with the `remote-cache` feature also shares them through an oxabl-cache server. See [`oxabl build --plan`](#oxabl-build---plan) and [`oxabl build`](#oxabl-build).

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
  - `oxabl explain <file>[:line]` — show each unit's direct database effects and transitive summary. Add `--json` for the version 1 explain document. Use `-I` and `--schema` to override project paths and schema. Preprocessing is on. A line selects the innermost unit at that root line; printed lines refer to expanded text. Partial summaries list their reasons. Index choices remain estimates until the XREF gate passes. Exit codes: 0 for a report, 2 for a bad line or unreadable file, 3 for fatal preprocessing failure, 4 for an internal panic, 6 for serialization failure.
  - `oxabl build [--plan [--probe] [--all] [--files <path>... | --since <rev>]] [--json] [--events] [--report <file>] [--fail-fast] [--no-cache] [--no-remote-cache] [--wait-lock <seconds>] [--workers <n>] [--root <dir>] [--config-overlay <file>] [--print-config]` — which compile units must rebuild and why (`--plan`), or compile them. See [`oxabl build --plan`](#oxabl-build---plan) and [`oxabl build`](#oxabl-build). Exit codes: 0 when everything compiled or a plan was printed, 1 when a unit failed to compile, 2 for a usage or configuration error, 3 when the store or the workspace cannot be used or no worker can start, 130 when a signal stops the command.
  - `oxabl analyze <file>` — a single file's resolved semantic model, dumped as a per-section-versioned JSON envelope or as text (`--schema <file|dir>` for schema-backed resolution). Introspection, not a gate: it exits 0 whatever it finds. Eight sections are emitted — `scopes`, `symbols`, `types`, `references`, `diagnostics`, `preproc`, `coverage`, and `dependencies` — each carrying its own version under `sections`, so a change to one leaves the others' numbers alone. `dependencies` is where cross-file resolution is observable: which files the run's workspace index actually consulted, and which cross-file names came back empty and why.
- `oxabl_wasm`: browser bindings in `crates/oxabl_wasm`.
  - Three `wasm-bindgen` exports — `analyze_source`, `format_source`, and `version()` (a crate version plus a build identifier, so a crash report names the exact artifact a hand-vendored copy is running) — the first two returning JSON: diagnostics carry source/severity/code/message, byte offsets, and line/column positions; a format result carries the new source, a `changed` flag, and an `error` string when the formatter declines to format (in which case the original source comes back untouched). Line and column come from the shared position helper the CLI's text output uses, so the two cannot drift.
  - Contains no ABL behavior. It is a transport adapter over the shared pipelines, which keeps every client on one implementation. A refusal collapses to one `error` string here because the wire shape has one field for it, while the pipeline keeps a deliberate bail and a contained panic apart for the clients that can tell them apart.
  - CLI-only dependencies live behind the `oxabl` crate's default-on `cli` feature so the library compiles for `wasm32`; a CI job builds the wasm target on every push.

Current Work: continued dogfood-driven trust-hardening, and typing `:`-qualified member and method-call expressions — the half of the cross-file population the type lattice still does not judge. Shipped: the semantic layer, the layout formatter (CLI + LSP), the language server, the VS Code extension, the curated `oxabl` public API, the browser WASM playground, the shared lint/format pipelines every client now drives, and cross-file/workspace resolution — inheritance chains, `USING` imports, literal `RUN` targets, and `SHARED` producers now link across files, with the CLI and the editor reaching identical answers from the same shared index. (The browser playground stays single-file: its exports take one source and no sibling files, so there is nothing for an index to look at.)

### `oxabl build --plan`

`oxabl build --plan` reads `oxabl.toml` and refreshes a graph of the working tree. It then lists the compile units that a change reaches. It compiles nothing. It starts no worker unless you add `--probe` (see [Compile keys](#compile-keys)). The command is hidden from `oxabl --help` until the build is complete. The design is in `docs/design/build-tool.md`.

Until a build has recorded results, a plan lists what changed since the last refresh. After that it compares each unit with its last result, see [What the plan compares](#oxabl-build---plan).

```sh
oxabl build --plan                       # the units that changed since the last run
oxabl build --plan --all                 # also list the units that did not change
oxabl build --plan --files inc/ledger.i  # treat these files as changed (paths are relative to the current directory)
oxabl build --plan --since origin/main   # treat every file that differs from a git revision as changed
oxabl build --plan --json                # the plan as one JSON document
oxabl build --plan --probe --json        # the same, with the compile key of each unit
```

A compile unit is a file under `[workspace.sources].directories` with a root extension (`p`, `w`, `cls`, `v`, or what `root_extensions` says), minus the globs in `[build].exclude`. Without `directories`, the whole workspace is walked. One line names each unit that must rebuild, with the first reason that applies:

| Reason | Meaning |
|---|---|
| `new` | The store had not seen the unit. |
| `named` | `--files` named a file in its closure. A file that is not in the graph is listed as `unknown-file` and changes nothing. |
| `source-changed` | The bytes of the unit changed. |
| `dependency-changed` | A file that stayed in its closure changed. |
| `closure-changed` | Its closure gained or lost a file. This includes a new file that now shadows an include. |
| `unkeyable` | The unit has no compile key, so it compiles on every build. The rules that apply follow the reason. Only with `--probe`. |

Once a build has recorded results, these reasons come from them (the first that applies is shown):

| Reason | Meaning |
|---|---|
| `new` | The unit has no result. |
| `failed` | The last compile failed. |
| `source-changed`, `dependency-changed`, `closure-changed` | As above, now against the last result: a file counts when it changed in a later refresh than the one the result was made from. With `--probe` the key sections that differ name the reason. |
| `propath-changed` | The unit PROPATH differs from the last compile (`--probe` only). |
| `environment-changed` | The environment differs from the last compile; the fields are listed, for example `cpcoll` or `schema: ledger` (`--probe` only). |
| `options-changed` | `[build.compile]` or the requested outputs differ (`--probe` only). |
| `worker-changed` | The worker program is another one (`--probe` only). |
| `unverified` | The last compile has no key, and the reason is in the details. |
| `output-missing`, `output-changed` | An output of the last compile is gone, or was changed outside oxabl. An output with another time stamp and the same bytes is current. |

**`--since <rev>`.** Oxabl runs the `git` binary (no libgit2) in the workspace root: `git diff --name-only --no-renames --relative <rev>` for committed, staged and unstaged edits and deletions (a rename is the old path deleted and the new one added), and `git ls-files --others --exclude-standard` for untracked files that git does not ignore. Each file counts as if `--files` had named it. A file that is not in the graph (a `README`, a build script) is dropped without a message. Exit code 2 when the root is not in a git work tree, when `<rev>` is not a commit, or together with `--files`. Only files under the workspace root are looked at. The graph is still the graph of the working tree: an edit that git does not show (an ignored file) is seen by the refresh like any other.

**`--json`.** One document on stdout, `format` 1. The shape is not stable before 1.0, like `analyze --json`; `format` changes with it.

```json
{
  "format": 1,
  "environment": { "source": "none", "identity": null, "at": null },
  "store": { "generation": 118, "committed": true },
  "units": [
    {
      "path": "ar/post.p",
      "compile_name": null,
      "action": "rebuild",
      "reasons": [{ "kind": "dependency-changed", "paths": ["inc/ledger.i"] }],
      "key": null,
      "unkeyable": ["environment-unknown"]
    }
  ],
  "removed": ["ar/old.p"],
  "unknown_files": ["notes.txt"],
  "summary": { "rebuild": 1, "unkeyable": 0, "up_to_date": 20613, "skipped": 0 }
}
```

- `units` is sorted by path and holds the units to rebuild (`action: "rebuild"`) and the units that cannot compile where they are (`action: "skip"`, `reasons: []`, the reason in `unkeyable`). With `--all` it also holds the others (`action: "up-to-date"`, `reasons: []`).
- Every reason has `kind` and `paths`. `named` also has `via`, one path from the unit to each named file. `closure-changed` also has `gained` and `lost`, and its `paths` is both lists. `unkeyable` also has `details`, the rules that apply. `new` and `source-changed` have no paths.
- `store.committed` is `false` when the plan found the lock taken and wrote nothing.
- `removed` lists units of the store that are gone from the working tree. `unknown_files` lists the files `--files` named that are not in the graph (always empty with `--since`).
- Without `--probe`, `environment` is `{ "source": "none", "identity": null, "at": null }` and every unit has `compile_name: null`, `key: null` and `unkeyable: ["environment-unknown"]`. That is a statement about the plan, not a rebuild reason: the plan still lists only what changed.
- With `--probe`, `environment` is `{ "source": "probe", "identity": "<hex>", "at": "<RFC 3339 time>" }`, and the document has two more fields. `propaths` lists the distinct unit PROPATHs, each as a list of `{ "entry": ..., "digest": ... }` (`dir`, `absent` or `sha256:<hex>`). Each unit has `propath`, an index into `propaths`, `compile_name`, and `key` (64 hex digits) or `null` with the reasons in `unkeyable`.

### Compile keys

A compile key is 32 bytes. It names one compile: the unit, its closure, its PROPATH, the environment of the worker, the compile options, the requested outputs and the worker program. `oxabl build --plan --probe` starts the worker pool, reads the environment, and computes the key of every unit. The pool starts at the same time as the refresh and stops when the keys are done. A failed probe fails the plan with the exit codes of `oxabl build --probe`. The key is BLAKE3 over a length-prefixed encoding of its inputs, with its own context string; `docs/design/build-tool/compile-key.md` gives the encoding and a worked example.

The PROPATH of a unit is `$OUT`, the `root_relative_include_paths` joined to the base directory of the unit (its own directory, or the package root of a class), `include_paths` (under the root, under `$DLC`, or `$LIB:<name>` from `[build.libraries]`), and the entries of the worker's own startup PROPATH that are under its `$DLC`. An `include_paths` entry that has none of these forms is a usage error. The compile name is the unit path below the first entry that holds it.

**A unit without a key.** Each reason is listed. The unit is in the rebuild set at every plan and compiles at every build.

| Rule | Reason | When |
|---|---|---|
| K1 | `degraded: <path>: <reason>` | The unit, or a class in its closure, has an edge set that may miss dependencies. An include is judged inside the unit that includes it, not as a root. |
| K2 | `unresolved-include: <name> in <path>` | An include does not resolve, in any branch. |
| K3 | `unresolved-class: <name> in <path>` | A class does not resolve, unless its name starts with a prefix of `[build.external_packages]` and a listed library is on the PROPATH with a SHA-256 digest. A directory never supplies a prefix. |
| K4 | `computed-name: <name> in <path>` | An include or class name is computed. This includes a class unit whose `CLASS` statement oxabl cannot read in its own text. |
| K5 | `outside-workspace: <path>`, `unreadable: <path>` | A file in the closure is outside the root and `$DLC`, or cannot be read. |
| K6 | `environment-unknown`, `unhashable-parameter: <name>`, `unhashable-library: $LIB:<name>`, `dlc-mirror-mismatch: <path>` | The environment is not known, a startup parameter names a directory, a `$LIB:` entry is a directory, or the worker's copy of a `$DLC` file differs from `[workspace.sources].dlc`. |
| K7 | `not-on-propath`, `shadowed-by: <path>`, `output-collision: <path>`, `package-mismatch: <name>`, `package-root-not-on-propath: <dir>` | The unit cannot compile where it is. Its action is `skip`. Of units whose r-code lands on one path (compared ignoring ASCII case), the smallest path compiles. |
| K8 | `class-conflict: <name>` | A name accepted as external is also the name of a workspace class. |
| K9 | `per-unit-propath-classes` | The units have more than one distinct PROPATH, and the unit is a class or reaches one. |

K3 sees only the class names that the analysis records. It records none from `OpenEdge.*` and `Progress.*`, so a prefix that names those namespaces does not change the key of a unit that uses them; the library digests on the PROPATH are in every key all the same.

**Parity with another tool.** `scripts/plan-parity.sh <plan.json> <list.txt>` compares the rebuild set of `oxabl build --plan --json` with a list of paths (one root-relative path per line) that another tool rebuilds for the same change. It prints the paths it **missed** (in the list, not in oxabl's set) and the **extras** (in oxabl's set, not in the list) grouped by oxabl's first reason, and exits 1 when anything is missed. It needs `jq`. `scripts/parity/run.sh` runs the scenarios under `scripts/parity/scenarios/` on the synthetic corpus in `scripts/parity/corpus/` (`OXABL=target/debug/oxabl scripts/parity/run.sh`); `cargo test -p oxabl` runs it too. A scenario is `edit.sh` (applied to a copy of the corpus after a first plan) and `expected.txt` (the list).

The closure of a unit is every file the compiler can read for it: its includes at any depth, the classes it names and the includes and classes of those, including the ones in branches an `&IF` did not take.

**What the plan compares.** Before a build has recorded results, a plan lists what changed since the last refresh that reached the store. A run that holds the workspace lock writes its refresh, so the next plan starts from it. Run `oxabl build --plan` again and the answer is "0 to rebuild" until a file changes. From the first build on, a plan compares every unit with its last result: a unit with no result is `new`, one that failed is `failed`, and a unit whose key, whose outputs or whose files differ from the result is in the set, so a build that was cancelled leaves its remaining units in the next plan. Without `--probe` there is no key, so the plan cannot see an environment, option or worker change; it says `environment from build at <time>` for the environment it did not probe. A plan that finds the lock taken (a build is running) reads a snapshot of the store and writes nothing. It ends with `(not committed)`.

**The store.** The state is in `[build].state` (default `.oxabl/build`), which must be under the workspace root: `state.db` (SQLite, WAL) and `build.lock`. Set `[build].store` to put `state.db` in another local directory when the root is on a network file system. SQLite WAL cannot run on NFS, SMB or FUSE, so oxabl stops with exit code 3 for a store there. Add `.oxabl/` to the ignore file of your version control. The store is a cache of what the files say:

- A store with another format number is deleted and made again. There are no migrations.
- A store that is damaged or cut short is moved to `state.db.corrupt-<seconds>` (only the newest such set stays) and made again.
- A file at the store path that is not an oxabl store stops the run with exit code 3. Oxabl never deletes a file it did not create.

**How fast.** A file is read and hashed only when its size, times or inode changed. A stat taken in the two seconds before a file's last change is not trusted at the next run. A run with no change stats the files and reads the store, and that is all it does. On a synthetic tree of 28,000 files that takes about 0.2 s. An analysis runs again only for a file whose bytes changed, or that looked for a path, found a path absent, or listed a directory, where something changed since.

**Configuration.** `[build]` and its tables (`workers`, `outputs`, `compile`, `verify`, `cache`) are read strictly, as are the top level of `oxabl.toml`, `[workspace]`, `[workspace.sources]` and `[workspace.schema]`: an unknown table or key is a usage error (exit code 2) that names it. A duration is an integer and `s`, `m` or `h` (`"90s"`); a size is an integer and `KiB`, `MiB`, `GiB` or `TiB` (`"20GiB"`). `oxabl build` without `--plan` needs `[build.workers].command` and `[build].output`, and stops with exit code 2 without them.

### `oxabl build`

`oxabl build` compiles the units that the plan lists. It is hidden from `oxabl --help`. It needs `[build.workers].command` (the command that starts one AVM worker, see `docs/design/build-tool/worker-protocol.md`) and `[build].output` (the directory for the r-code). Oxabl ships no Progress code but its own small worker program. The build runs in this order:

1. It takes the workspace lock (`<state>/build.lock`) and, in the r-code directory, `.oxabl-build.lock`. A second build stops with exit code 3 and the process id of the first.
2. It refreshes the graph and, at the same time, starts the workers, probes the environment and hashes the worker-side files. It computes the compile key of every unit and the plan.
3. It deletes the outputs that no result owns, then looks up the keyable units of the rebuild set in the cache and compiles. Nothing compiles before this.
4. The lookup and the compiles overlap: a unit that waits for a lookup is **pending**, and its users wait for it as they wait for a compile. A hit is installed at the final paths and recorded; a miss becomes a compile. Each worker takes the ready job of highest priority: the longest chain of units that waits for it, by last compile time. A job is a set of units that cannot be ordered, a cycle of classes, and goes to one worker in one request, with `MULTI-COMPILE` on. A unit compiles after every unit that it reads at compile time: classes before their users. A failed unit does not hold its users back; they compile and are marked **tainted** in the report. A tainted unit is recorded but never offered to a cache.
5. Each unit that compiled has its outputs moved from the worker's stage (`<state>/stage/<build>/w<n>-<s>`) to their final paths by `rename`, or by copy, `fsync` and rename across file systems. The result is recorded after the outputs are in place. Results are written to the store in batches of 500 units or two seconds, after `syncfs` on the output file system. A unit that failed loses its old outputs, so no user compiles against stale r-code.
6. Each unit that failed compiles once more, in the same order. A unit that failed because its worker was lost twice is not tried again. A unit that compiles on the second try counts as one success.
7. The last schema check runs, the uploader finishes, and the summary says what the cache did.

**Output directories.** `[build].output` and the directories of `[build.outputs]` belong to oxabl. Each must be absent or empty the first time (oxabl writes `.oxabl-output` into it), and must not lie inside another. A directory that holds files and has no marker stops the build with exit code 2 and nothing is deleted. Oxabl deletes every file in a marked directory that no result owns.

**Workers that fail.** A worker is lost when its output ends, its process exits, a unit takes longer than `[build.workers].unit_timeout`, or it breaks the protocol. Oxabl stops its process group, starts another worker in the slot and puts the unit that was running, and the units of the same request that had not started, back in the queue. A unit that loses its worker twice in one build fails with `worker lost twice while compiling this unit`. When no replacement can start the build goes on with the other workers; with none left it stops with exit code 3.

**Cancel.** The first `SIGINT` or `SIGTERM` stops dispatch. Busy workers have `[build.workers].cancel_grace` (30 s) to finish their unit; what they finish is installed and recorded. Then the workers get `QUIT`, and any that is still busy is stopped. A second signal stops every worker at once. The exit code is 130 and the store is consistent: the next build compiles what was not recorded. If oxabl is killed, the workers read the end of their input and quit after their current unit.

**`--fail-fast`** stops dispatch at the first failed unit; the units not started are `not-run` in the report, and there is no retry pass.

**`--events`** writes one JSON object per line to stdout while the build runs; the human output goes to stderr. **`--report <file>`** writes the result of the build as JSON, and **`--json`** prints the same document instead of the text. [What `oxabl build` reports](build-output.md) lists every event with its fields, the fields of the report and the exit codes.

**The overlay.** `--config-overlay <file>` (or `OXABL_BUILD_OVERLAY`) names a TOML file with the schema of `oxabl.toml`. Oxabl merges it over `oxabl.toml` key by key (an array replaces) and then reads the result strictly. It may set `[workspace] name`, `[workspace.sources]`, `[workspace.preprocessor]`, `[workspace.schema]` and `[build]`; any other table, an unknown key, a key whose name looks like a secret and a trust key of the remote cache (`tenant`, `cache`, `owner_keys`, `min_roster_version`) are exit code 2. Without an `oxabl.toml` the overlay is the whole configuration. For one setting the order is: flag, environment variable, overlay, `oxabl.toml`, default. `oxabl build --print-config` prints the effective settings, each with its source, and stops. A tool that owns the environment of a workspace (containers, databases, PROPATH) writes an overlay for each run; see [Drive `oxabl build` from a wrapper](build-wrapper.md).

**Exit codes.** 0 every unit compiled, 1 a unit failed after the retry pass (a skipped unit is not a failure), 2 a usage or configuration error (no worker command, no `[build].output`, a bad overlay, an output directory that oxabl does not own, workers that disagree), 3 the store, a lock or the workers could not be used, 130 cancelled.

**Caches.** A cache is an optimisation: any cache error is a miss, never a failed build and never a wrong hit. `--no-cache` turns off both caches, `--no-remote-cache` the remote one. A workspace that turns the remote cache on (`[build.cache].remote = true`, or a `.oxabl/cache.toml` file) needs a binary that was built with the remote cache; without it the build stops with exit code 2 and says so, so that a CI job does not run slowly with no signal, unless you pass `--no-remote-cache`. The build ends with one line, for example `local: 12 sets offered, 12 stored`.

**The local cache** is a directory of entries named by compile key, `$XDG_CACHE_HOME/oxabl/artifacts` (`~/.cache/oxabl/artifacts`; `~/Library/Caches/oxabl/artifacts` on macOS) or `[build.cache].local`; `OXABL_BUILD_CACHE_DIR` beats both. Two copies of one workspace, or two branches, share its entries, because a key depends on content and not on the path of the workspace. An entry holds every output file of one unit and a manifest with the size and the BLAKE3 hash of each file. A read copies each file to the stage and hashes it; a file or a manifest that differs, or an entry that lists other files than the build asks for, is a miss, and a damaged entry moves to `v1/quarantine/`, which the next eviction deletes. An entry is built in `v1/tmp/` (by reflink, hard link or copy) and renamed into place whole, after one `syncfs` for a batch. A hard-linked entry shares its inode with the output, so an edit in place of the output makes the next read a miss, never a wrong hit. When the recorded size passes `[build.cache].local_max_size` (20 GiB), the end of a build removes the entries with the oldest `last-used` until 90 % of it is left. The directory is created with mode `0700`; a directory that another user owns, or that the group or others can write, is not used (one warning, every key a miss).

**Who the local cache trusts.** It trusts every process that runs as the user, so it is on only in `developer` mode. The mode is `OXABL_CACHE_MODE` (exactly `ci`, `developer` or `read-only`; another value is exit code 2), else `read-only` when `CI` is set, else `developer`. In `ci` and `read-only` mode the shared cache is neither read nor written. A CI system that does not set `CI` must set `OXABL_CACHE_MODE`. In those modes `[build.cache].local_job_dir` names a directory that serves as the local cache of one job: it must be absent or empty the first time (oxabl writes `.oxabl-cache-job` into it); a directory with files and no marker, the workspace root, an ancestor of it, `$HOME` and `/` stop the build with exit code 2, and nothing is deleted; at the start of each build oxabl deletes only `v1/` beneath it. A local entry that came from a remote cache records the tier and the signer in its manifest, but nothing authenticates that line, so the report shows it as `origin_unverified`.

**The remote cache** is in the binary only when it is built with `--features remote-cache` (`cargo install oxabl --features remote-cache`). The feature is off by default, so the library, the WebAssembly build and a plain `cargo install oxabl` carry no network or crypto code. It is on for a workspace when `[build.cache].remote` is `true`, or when `.oxabl/cache.toml` exists and `remote` is not set; `remote = true` without that file is exit code 2. A binary without the feature stops with exit code 2 and the message `this oxabl was built without remote-cache; rebuild with --features remote-cache or pass --no-remote-cache`.

The file `.oxabl/cache.toml` is part of the repository. It holds `server`, `tenant`, `cache`, `owner_keys` and `min_roster_version` (see the oxabl-cache client documentation), and no secret. `tenant`, `cache`, `owner_keys` and `min_roster_version` are the root of trust: no oxabl file, flag or environment variable can set them. naming one in `oxabl.toml` or in an overlay is exit code 2, and the message names the key; one of `OXABL_CACHE_TENANT`, `OXABL_CACHE_CACHE`, `OXABL_CACHE_OWNER_KEYS` and `OXABL_CACHE_MIN_ROSTER_VERSION` in the environment stops the build with exit code 2 that names the variable, before oxabl compiles or reads any cache. `OXABL_CACHE_SERVER` changes the server only. The mode (`OXABL_CACHE_MODE`, `CI`) and the credentials (`OXABL_CACHE_TOKEN`, `OXABL_CACHE_IDENTITY`, OIDC) come from the environment; oxabl passes them to the client and never reads or prints a credential. Any other problem with the remote cache gives one warning, and the build runs without it: a configuration the client rejects, a server that is down, an answer that fails verification, or no progress for `[build.cache].idle_timeout` (30 s; a slow transfer that keeps sending is not cut off). Each key that the remote cache cannot give is compiled.

A mode that writes (`ci`, `developer`) offers every verified set; the credentials decide where it goes. In `read-only` mode nothing is offered and the summary says `remote: read-only, 0 sets offered`. A `ci` identity that is not a `ci` member of the roster gives one warning, and every offered set shows as `not stored (Unavailable)`. The files of an offered set are read from the output directories; an output that is a symbolic link is `not stored (Unreadable)`, and a link in a directory above it is allowed. Remote hits are also stored in the local cache, with their tier and signer as `origin_unverified`.

**Verification.** A key is computed before the compile, so a set may go to a cache only after the unit passes three checks, which run while the build runs:

- **V1, inputs.** The unit and every file of its closure are hashed again after the unit finished and must equal the hashes of the key. A `$DLC` file is hashed again by the worker that compiled, from the copy the AVM compiles with. A file that changed during the compile (an edit while the unit compiles) keeps the unit out of the caches; the summary counts such units, and the next build compiles them again.
- **V2, schema.** Before a set is released the databases are imaged again, split across the idle workers, by a check that started after the unit finished. It must give the schema identities of the probe. Checks start when `[build.verify].batch_units` sets are waiting (500) or the oldest has waited `batch_interval` (`"60s"`), and once at the end. A check that differs, or fails, stops every held and every later write of the build. A cancel releases only the sets that a check already released.
- **V3, reads.** The compile also writes a private `XREF-XML` (the one you ask for with `[build.outputs].xref_format = "xml"` serves, otherwise a file under the stage that is not an output). Every `Source` file name must be the unit or in its closure, as the compiler resolved it. R-code under the output directory and members of a procedure library are skipped when the closure holds a class: class resolution is protected by the plan rules. A unit that fails V3 is not written; its result gets no key and the file in its note, so it compiles at each build and the plan says `unverified: <file>`. The analysis missed an input: please report it. On OpenEdge 12.8 the compiler lists a `Source` element only for a file that holds a reference. An include with only preprocessor definitions has none, and no class file has one (not a class from source, from the output directory or from a procedure library), so V3 cannot check how those were resolved. The plan rules protect them: classes are keyed only with one PROPATH id and without an external name collision, and every closure file counts by its path and bytes.

With `[build.verify].reads = false`, or `[build.outputs].xref_format = "text"`, nothing is written to any cache, and the build says so. A cache that does not write (`read-only`, `--no-cache`) skips verification.

**The seam.** `oxabl::build::cache::ArtifactCache` has `fetch` (all keys at once, one outcome per key as each completes, every set installed below a stage and complete before it is reported), `uploader` (one `push` per verified set, with the artifact paths and the paths of the files in the output directories, never their bytes; `finish` returns one `StoreOutcome` per key), `writes` and `fill`. A panic in a cache call is caught and is a miss. `Layered` asks a near cache first and a far cache for the misses, and stores what the far cache gave in the near one.

**The worker program.** `PROPATH`, `OPTIONS` and `COMPILE` requests are answered by `OK`, `MSG`, `DONE` and `END`. The worker writes one small procedure for each shape of `COMPILE` statement into its stage, because the phrases (`XREF-XML`, `MIN-SIZE`, `GENERATE-MD5`, ...) are keywords; `languages` and `options` hold only letters, digits and `-_:,.`. The stage path that oxabl sends starts with `./`, because the compiler removes a leading `.` from a relative XREF file name. A failed unit always has a reason: the compiler messages, the error that stopped the compile, or a note that the worker gave none. The PROPATH entries that oxabl sends are relative to the worker's working directory (the output directory first), `$DLC` entries (the worker expands them) or the paths of `[build.libraries]`.

#### Testing against a real AVM

The black-box tests of `oxabl build` use a fake AVM. Two opt-in test files run it against OpenEdge. Every test in them is `#[ignore]`, and a test fails, never skips, when its variables are missing.

- `crates/oxabl/tests/build_openedge.rs` needs an installation or container with databases. `OXABL_OPENEDGE_COMMAND` is the worker command as a JSON array with `{program}` and `{root}` (it connects the databases through its `.pf` and starts in `{root}`). `OXABL_OPENEDGE_WORKSPACE` is a host directory that the worker sees, `OXABL_OPENEDGE_WORKER_ROOT` is the same directory as the worker sees it, `OXABL_OPENEDGE_PROVERSION` is the version as `PROVERSION` prints it (`12.8`), and `OXABL_OPENEDGE_DATABASES` lists the connected databases in order. `OXABL_OPENEDGE_WORKERS` (default `1,4,8`) gives the worker counts of the timing test.
- `crates/oxabl/tests/build_openedge_scratch.rs` creates scratch databases from the empty database of the installation and never touches another. `OXABL_OPENEDGE_EXEC` is the prefix that runs a program where OpenEdge is (`["docker","exec","-i","-w","{root}","<container>"]`), `OXABL_OPENEDGE_SCRATCH` is a directory there, and `OXABL_OPENEDGE_DLC` (default `/psc/dlc`) is the installation. It also reads the workspace, worker root and version variables above.

```bash
cargo test -p oxabl --test build_openedge -- --ignored --nocapture --test-threads 1
cargo test -p oxabl --test build_openedge_scratch -- --ignored --nocapture --test-threads 1
```

The compile tests build the fixture in `crates/oxabl/tests/fixtures/openedge/`. They compare r-code with a mask: r-code is not byte-deterministic, so two files are equivalent when they have the same size and differ only where repeated compiles of one unit in one root differ (and two bytes around each such byte). The tests print `EVIDENCE` lines for what the compiler lists in `XREF-XML` and which class a unit is compiled against.

### Lint configuration

Lint rules are in groups. The `LINT` rules are in the `default` group, which is always on. The `strict.transactions`, `strict.locks`, `strict.access`, `strict.handles`, `strict.nulls` and `strict.calls` groups are off until you enable them. The plan for all strict rules is in `docs/design/strict-analysis-plan.md`. These strict rules are available now. They are in preview and report at `warn`:

| Code | Name | Group | Reports |
|---|---|---|---|
| CL001 | `run-value` | `strict.calls` | `RUN VALUE(...)` with a computed target |
| CL002 | `dynamic-invoke` | `strict.calls` | `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `DYNAMIC-PROPERTY`, `CREATE CALL` |
| CL005 | `shared-state` | `strict.calls` | `NEW SHARED`, `NEW GLOBAL SHARED` and `SHARED` definitions |
| TX006 | `missing-no-undo` | `strict.transactions` | a variable, parameter, temp-table or property without `NO-UNDO` |
| HD001 | `handle-not-deleted` | `strict.handles` | a dynamic object (query, buffer, temp-table, dataset, widget, ...) that some path loses without a delete |
| HD002 | `delete-not-in-finally` | `strict.handles` | a delete that an error between the create and the delete skips |
| HD003 | `persistent-procedure-leak` | `strict.handles` | `RUN ... PERSISTENT SET h` whose procedure nothing deletes |
| HD004 | `memptr-not-freed` | `strict.handles` | `SET-SIZE(m) = n` with no `SET-SIZE(m) = 0` on a normal path |
| HD005 | `missing-widget-pool` | `strict.handles` | dynamic objects created with no widget pool scoped to the procedure, once per file |
| LK001 | `default-lock` | `strict.locks` | a `FIND`, `FOR` or `PRESELECT` phrase, `CAN-FIND` or `OPEN QUERY` phrase on a database table with no lock phrase (it takes `SHARE-LOCK`) |
| LK002 | `lock-outlives-transaction` | `strict.locks` | a record still locked after its transaction ends, because the buffer's scope is wider (a `DO TRANSACTION` reading into a procedure-wide buffer, or an internal procedure using the file's buffer) |
| LK003 | `lock-upgrade-in-loop` | `strict.locks` | a `NO-LOCK` read locked again with `FIND CURRENT ... EXCLUSIVE-LOCK` (or a re-find by `ROWID`) on every iteration of a loop |
| TX001 | `implicit-transaction` | `strict.transactions` | a `FOR`, `REPEAT`, `DO ON ERROR` or internal procedure that is a transaction block only because it updates the database, with no `TRANSACTION` keyword |
| TX002 | `procedure-wide-transaction` | `strict.transactions` | a main block that updates the database directly, so one transaction spans the whole procedure (once per file) |
| TX003 | `transaction-spans-ui` | `strict.transactions` | `WAIT-FOR`, `UPDATE`, `SET`, `PROMPT-FOR`, `PAUSE` (with no time), `MESSAGE ... VIEW-AS ALERT-BOX` and other input statements inside a transaction |
| TX004 | `transaction-spans-call` | `strict.transactions` | a `RUN` inside a transaction whose callee is not in the file (another program, `RUN ... IN h`, `RUN VALUE`) |
| TX005 | `scope-widened` | `strict.transactions` | a reference outside a `FOR` block that raises the block's buffer scope to the procedure |
| DB001 | `find-in-loop` | `strict.access` | a `FIND` or `CAN-FIND` of a database table that runs once per row of a `FOR EACH` or query loop (N+1) |

The four rules before `HD001` are context-free: each finding depends only on the text of its own file, expanded with no arguments and no includer defines. The `HD` rules read the lifetimes of dynamic objects in one file (`oxabl_handles`) and are checked the same way. They are in preview. An include that ends with an object still live may leave the delete, or the widget pool, to its includer. Such a finding says so in its help, and no autofix may change it. `HD005` is reported once per file, at the file's own first create: a program reports its own first create, and an include that is checked alone reports its own. A `HD` finding whose anchor is in an include is reported in the include when the include alone shows it. Otherwise (the object is created or named in the program, or deleted in an include that has no create) it is reported in the program, at the `{...}` that includes the file, with the include's path and line in the message and a lower-confidence note in the help. It is never reported in both places. The `LK` rules and `TX001`-`TX005` read the buffer scopes, transaction blocks and locks of one file (`oxabl_scope`) and are routed like the `HD` rules. They report only on tables of the loaded schema, so they are silent when no schema is configured (a `DO TRANSACTION` needs no schema for `TX003` and `TX004`). They never report when the analysis cannot decide (a recovered parse, an unexpanded include). `LK002` and the `TX` rules rest on rules of the scope model that are not yet checked against a compiler listing (`docs/design/strict-analysis-open-questions.md`, sections 7.7 and 7.8). `DB001` reads the database effects of one file (`oxabl_effects`), over the same scope analysis, and is routed and silenced the same way (section 8.8). For most files that is the whole answer. It is not for a parametrised include fragment: `{1}`, `{&name}` and `&IF DEFINED(...)` resolve against no includer, so a statement that holds one can compile differently in each includer.

**Include files.** A strict rule reports code from an include in the include file, once, and not in the program that includes it. The one exception is a `HD`, `LK`, `TX001`-`TX005` or `DB` finding that the include cannot show alone (see above). `oxabl check <dir>` therefore also checks the `.i` files of the tree, but only for context-free rules, and only where the configuration enables one for that file. A directory that holds only such include files is checked too; `check` exits 2 for "no ABL files" only when it finds nothing to check. Other findings of a fragment (parse errors, undefined names, unused variables) stay hidden, because a fragment often does not parse or resolve alone. A baseline keys such a finding by the include's path. A finding is lower confidence when the part of its statement that the rule reads can change with the includer: a `{&name}`, `{n}`, `{*}` or `{file.i <args>}` reference, an `&IF`, or an argument-less `{file.i}` that did not resolve or has a reference or an `&IF` of its own. An include guard does not count (also the `&IF DEFINED(x) &THEN &ELSE <define x> &ENDIF` form); a comment is not part of a statement; a `{&name}` that an earlier `&SCOPED-DEFINE` in the same fragment gave a fixed value outside any `&IF` does not count (a `&GLOBAL-DEFINE` still does); and `RUN VALUE` (CL001) counts only for its target. The help of such a finding says that it depends on the including file, and no autofix may change it. A TX006 finding on a definition with a literal `SHARED` keeps the "producer and every consumer" help; one whose modifier slot holds a reference says that an includer may make it `SHARED`. The run cannot see what an includer's arguments would add, for example a `NEW SHARED` modifier that comes from `{&mod}`, so such a finding can also be missing. Limits:

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
