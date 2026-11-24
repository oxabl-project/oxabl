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
  - document outline (`textDocument/documentSymbol`): the Outline view, breadcrumbs and "Go to Symbol in File", from `oxabl_pipeline::outline` on the raw buffer (`docs/design/document-outline.md`)
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
  - Status: a first set of rules ships today — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002), `unknown-table-or-field` (LINT0003, live under a loaded `.df` schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005), `assigned-but-never-read` (LINT0006), and the suppression rules `invalid-suppression` (LINT0007) and `unused-suppression` (LINT0008) — configurable per-project via `oxabl.toml`. LINT0002 and LINT0006 divide one population between them: a variable never referenced at all is LINT0002's, while one that is written and never read is a dead store reported by LINT0006 at the assignment, so silencing `unused-variable` alone no longer silences the write-only half. Around thirty ABL statement forms are recognized by the parser but not modelled (`PUT`, `EXPORT`, `UPDATE`, `ENABLE`, embedded SQL, …); their identifiers are now harvested lexically and best-effort-resolved, so all three count-gated rules stay quiet about a variable one of them touches rather than reporting it wrongly. That suppression is coarse — per-symbol and file-wide — so `oxabl analyze` reports how many symbols it could not fully judge. The handful of forms that name a *table* without reading a field of it (`DEFINE BUFFER`, `DEFINE PARAMETER BUFFER`, `EMPTY TEMP-TABLE`, `DEFINE QUERY`, `OPEN QUERY`) are treated more precisely: they credit a real read on the table, so a temp-table used only that way no longer looks untouched. Surfaced in-editor through the VS Code extension and as diagnostics from `oxabl check`. Still experimental. `oxabl_lint::Registry` lists the rules with their metadata (code, name, group, description, default severity, stability), accepts your own rules, and runs a subset; `lint_file` stays the run-everything entry point. See [Lint configuration](#lint-configuration) for rule groups, per-directory settings and baselines.
  - **An `.i` opened directly is analyzed as a fragment.** Names its missing includer could supply stay silent, as do the three whole-unit count rules, while parse errors and locally provable findings still surface. The CLI and analyze coverage channels say that the includer was absent; an `.i` expanded through a `.p`/`.w`/`.cls`/`.v` root remains ordinary textual input to that complete compilation unit.
  - **`undefined-symbol` now reports names absent from your configured search paths.** With a workspace index attached — which is every `oxabl check` run — a `USING` import, a `NEW`, or a literal `RUN` target that no configured path supplies is reported at error severity. ABL cannot reference a symbol or a procedure whose code is not on the PROPATH, so the name genuinely is undefined rather than merely unseen. Set your paths in `[workspace.sources].include_paths` in `oxabl.toml` (relative to that file, searched in order, first match wins), or pass directories with `-I`; every such finding carries a help line naming that configuration, because a missing source root produces the same finding as missing code. Four things stay deliberately silent: names in the `Progress.*`, `OpenEdge.*`, `System.*`, and `Microsoft.*` namespaces, which ship with the AVM and have no source on any path; a runtime-computed `RUN VALUE(...)` target, which no indexing could resolve; a member a class declares but does not expose to the caller, which exists and so is an access question rather than a missing name; and every cross-file name at all when no path is configured, since "we did not look" is then the only truthful answer. The `AS CLASS pkg.Missing` declaration spelling is also silent for now — the AST carries no span for that name, so there is nothing to underline.
  - **The preprocessor answers the AVM's predefined names from `[workspace.preprocessor]` in `oxabl.toml`.** The AVM reads `{&OPSYS}`, `{&WINDOW-SYSTEM}`, `{&BATCH-MODE}`, `{&PROCESS-ARCHITECTURE}` and `{&PROVERSION}` from the machine that compiles. oxabl has no such machine, so you state the environment and every `&IF` over these names decides against it. The defaults describe a 64-bit Unix batch compile with a character UI: `opsys = "UNIX"`, `window-system = "TTY"`, `batch-mode = true` (expands to `yes`), `process-architecture = 64`. You can override each key (for example `opsys = "WIN32"`, `window-system = "MS-WINDOWS"`, `batch-mode = false`). `proversion` has no default because a tool cannot guess it: set it (for example `proversion = "12.8"`), or an `&IF` condition that reads `{&PROVERSION}` or the `PROVERSION` function raises the always-loud warning `PREPROC009` instead of silently deciding against an empty string. The `OPSYS` and `PROVERSION` functions in an `&IF` read the same table. `databases` lists the logical names of the databases connected when the code compiles, in the order of connection (for example `databases = ["ledger", "stock"]`), and the `DBTYPE`, `CONNECTED` and `LDBNAME` functions of an `&IF` answer from it. The list is complete: a database that is not in it is not connected. Use `[]` for a compile with no database. `DICTDB`, the alias that the AVM gives the first connected database, answers as the first name in the list. Without the key, each `[workspace.schema]` file names one database: the file name without `.df` is the logical name, so `schema/ledger.df` connects `ledger`. A file name is not always the logical name (a versioned dump file, for example), and the files are in the order of `[workspace.schema]`, not of connection. So this list is not complete and not ordered: a name that is not in it is a guess (`PREPROC010`), not an absent database, and so are `LDBNAME("DICTDB")` and `DBTYPE("DICTDB")`. `CONNECTED("DICTDB")` is true when the list is not empty. Set the key when a logical name differs from its file name. `DBTYPE("ledger")` answers `PROGRESS` for a listed database, or the type that `database-types` gives (`database-types = { stock = "ORACLE" }`), and `?` for one that a stated list does not hold. `CONNECTED("ledger")` is true for a listed one, and false for one that a stated list does not hold. `LDBNAME("ledger")` answers the name as listed, or `?` for one that a stated list does not hold. Only the logical-name form is modelled: `LDBNAME(1)` and `DBTYPE(1)` depend on the order of connection, so they are a guess. With neither a list nor a schema, every call to one of these functions is a guess too. `{&FILE-NAME}` (the name the file was analysed under, or the include name as written), `{&LINE-NUMBER}` (the line in the current file) and `{&SEQUENCE}` (0, 1, 2, … per reference, shared by the whole compilation unit) are computed from the source and need no configuration. An `&IF` condition is evaluated for the operators and functions whose result oxabl can compute exactly: comparisons (two strings compare as text, two numbers as numbers, decimals exactly; a string against a number is a guess), `BEGINS`, `MATCHES`, `AND`, `OR`, `NOT`, the unknown value `?` (equal only to itself; every other comparison with it is unknown, and `NOT`, `AND` and `OR` follow the three-valued rules, and an unknown condition is false, so `&ELSE` or `&ELSEIF` is taken), integer `+ - * MODULO`, `DEFINED` (0 undefined, 1 `&GLOBAL-DEFINE`, 2 include argument, 3 `&SCOPED-DEFINE`), `OPSYS`, `PROVERSION`, `DBTYPE`, `CONNECTED`, `LDBNAME`, `LOGICAL`, `INTEGER`, `DECIMAL` (whole values), `STRING`, `SUBSTRING`, `LENGTH`, `TRIM`, `LEFT-TRIM`, `RIGHT-TRIM`, `LC`, `INDEX`, `R-INDEX`, `ENTRY`, `NUM-ENTRIES`, `LOOKUP`, `ABSOLUTE`, `MAXIMUM` and `MINIMUM` (all numbers or all strings). `DEFINED` gives the kind of the newest definition of the name, as the AVM does. A scoped define or an include argument hides an older definition until it ends or is undefined. A global define of a name that has a scoped define or an include argument is an AVM error and is ignored. An `&UNDEFINE` in an include removes the visible definition wherever it was made. A condition with nothing in it (`&IF {&undefined} &THEN`) is false, as in the AVM. Any other name, function or shape (`SEARCH`, `KEYWORD`, `/`, an empty operand inside an expression, …) raises the always-loud warning `PREPROC010`, and the file's edge set is `Degraded` with reason `preprocessor_loss`: the branch is a guess, so the includes inside it may or may not be dependencies. `SEARCH(...)` asks whether a file exists, which no evaluator can know, so it is always such a guess. When an operand settles an `AND` (false) or an `OR` (true) without a guess, the guesses in the other operands of that chain, before it or after it, cannot change the result and raise nothing. A syntax error, or a comparison of a string with a number, is different: the AVM rejects the condition whatever the other operands are, so it always raises `PREPROC010`. An include reference with whitespace inside the braces (`{ name.i }`) expands like `{name.i}`; one whose brace never closes raises `PREPROC008` and is reported as an unresolved include. One run reports at most 100 of them; the 100th says that the rest are not reported. A damaged file can hold a run of thousands of `{`.
  - **The preprocessor stops at an expansion limit.** A define or an include that expands the previous one two times doubles at each level, so a file of a few hundred bytes can ask for gigabytes. One run expands at most 32 MiB (`MAX_EXPANSION_BYTES`): the text of each include, of each `{&name}` and `{N}` value, and of each value that a define builds. Each include or reference also counts 256 bytes, so empty expansions repeated without end stop too. Past the limit, the preprocessor reports the error `PREPROC011` once, at the site in the root file that led there, and expands nothing more. The text of the root file is kept and analysed, and the file's edge set is `Degraded` with reason `preprocessor_loss`.
  - **Case-insensitive lookup is opt-in.** A class or `RUN` target is found by its name as written, then by its lower-case fold. Set `case_insensitive_lookup = true` under `[workspace.sources]` to also match a file whose path differs in case from both (`pkg.UTIL` for `pkg/Util.cls`). It lists directories on a miss, so it costs more. Two distinct matches leave the name unresolved. A custom `FileSystem` supplies its own matching by overriding `find_ignoring_case`.
  - **Each root can search its own directory first.** Some shops compile every program with its own directory, and fixed subdirectories of it, first on the PROPATH. Set `root_relative_include_paths = [".", "rf"]` under `[workspace.sources]` and each root file searches `<its dir>` and `<its dir>/rf` before `include_paths`, for includes, classes and `RUN` targets alike. A class or program found this way reads its own includes with the paths of its own directory. One `oxabl check` run handles roots in any number of directories. The default is empty.
  - **Include path entries can name the OpenEdge installation.** An entry in `[workspace.sources].include_paths` that is `$DLC` or starts with `$DLC/` names a directory under `[workspace.sources].dlc`, the host directory that holds the include sources of the OpenEdge installation your workers use (`include_paths = ["src", "$DLC/src"]`, `dlc = "/opt/oe128-sources"`). `dlc` is a path that is specific to one machine, so `OXABL_DLC_SOURCES` overrides it. With neither set, the includes under a `$DLC/` entry do not resolve. An entry `$LIB:<name>` names a procedure library of `[build.libraries]`. A library holds r-code, so the analysis does not search it. `oxabl check`, the language server, the daemon and `oxabl build` all read these entries the same way.
  - **The files a directory walk picks up are set by `[workspace.sources].root_extensions`.** The default is `["p", "w", "cls", "v"]`, matched without regard to case. Use it for shop conventions, for example `root_extensions = ["p", "w", "cls", "v", "pp"]`. The list replaces the default, so keep the entries you need. `i` is rejected with a warning, because an include file is never a root. A file you name on the command line is always checked.
  - **Source files are UTF-8 unless `[workspace.sources].encoding` says otherwise.** Many ABL trees are stored in a single-byte code page. Set `encoding = "windows-1252"` or `encoding = "iso-8859-1"` (the default is `"utf-8"`; `utf8`, `latin1` and `cp1252` are accepted as aliases), and every file that oxabl reads from disk is decoded that way: roots, include files, classes and `.df` schema files, in `oxabl check`, `analyze`, `format`, the language server and the daemon. Offsets and columns in a report count the decoded text. A UTF-8 byte order mark always means UTF-8, is removed whatever the setting says, and `oxabl format` writes it back. `oxabl format` also writes a file back in its own encoding. With `"utf-8"`, a file that is not valid UTF-8 cannot be read, and `check` reports it as a failure that names the setting. For a tree that mixes UTF-8 files and single-byte ones, keep `encoding = "utf-8"` and set `fallback_encoding = "windows-1252"`: a file that is not valid UTF-8 is then decoded with the fallback. A single-byte encoding decodes every file, so the fallback never applies to it. Under `windows-1252`, the five bytes the code page leaves undefined (0x81, 0x8D, 0x8F, 0x90, 0x9D) decode to the C1 control of the same value. A custom `FileSystem` decodes for itself; `oxabl_workspace::EncodedFileSystem` is the disk filesystem that follows the setting, and `RealFileSystem` stays UTF-8.
  - **A `RUN` target with no extension is tried with the extensions in `[workspace.sources].run_extensions`.** `RUN post-order` can name a program file, but it can also name an internal procedure. oxabl looks for `post-order.p` and then `post-order.w`, and the first file it finds is the dependency. To change the extensions or their order, set the list in `oxabl.toml` (for example `run_extensions = ["w", "p", "pp"]`). A leading dot is ignored, and an entry that is empty or holds a path separator is dropped with a warning. Each extension is tried as written and then in lower case. The default is `["p", "w"]`. A bare name that no file supplies is an internal procedure of a running program, so the dependency edge set has no row for it. A target with an extension or a path (`RUN a/b.p`) still gets a row when no configured path supplies it.
- Easy one-line installer and VS Code extension for getting started
  - Status: VS Code extension available (experimental). Each release of the `oxabl_vscode` component carries the VSIX on GitHub Releases, and the extension updates itself from there (`oxabl.updates`). It highlights ABL with its own TextMate grammar, with no `oxabl` binary; with the binary it launches `oxabl lsp` for format-on-save, live diagnostics, the document outline, and `oxabl.toml` schema completion. See `clients/vscode/README.md` and `docs/design/vscode-extension.md`. One-line installer: not started.
- Build system
  - Incremental compiling (via the Progress compiler)
  - Remote cache
  - Status: Experimental. `oxabl build` (checked against OpenEdge 12.8 by an opt-in suite) refreshes a persisted dependency graph, says which units must rebuild and why (`--plan`), and compiles them through AVM workers that your own command starts, in dependency order, with retry, crash handling and cancellation. An overlay file (`--config-overlay`) lets a tool that owns the environment of a workspace give the machine-specific settings for one run, and `--print-config` shows where each setting came from. A local cache of compile outputs lets two copies of a workspace share compiles, and a binary built with the `remote-cache` feature also shares them through an oxabl-cache server. See [`oxabl build --plan`](#oxabl-build---plan) and [`oxabl build`](#oxabl-build).

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
- CLI entry points in `crates/oxabl`. The advertised surface is six commands — `check`, `explain`, `format`, `lsp`, `schema`, and `build` (experimental) — plus four **hidden** ones described below. `oxabl --version` (and `-V`) prints the crate version.
  - `check` walks a directory (or single file) for ABL files and reports lint findings plus formatting drift in two channels. This is the pre-commit gate: exit 1 when either channel has something to say.
  - `format` fixes layout in place, or reports/prints without writing (`--check`, `--stdout`). Like `check`, it exits 2 on an `oxabl.toml` it cannot use; `--style` skips the file.
  - `explain <file>[:line]` — show each unit's direct database effects and transitive summary. Add `--json` for the version 1 explain document. Use `-I` and `--schema` to override project paths and schema. Preprocessing is on. A line selects the innermost unit at that root line; printed lines refer to expanded text. Partial summaries list their reasons. Index choices remain estimates until the XREF gate passes. Exit codes: 0 for a report, 2 for a bad line or unreadable file, 3 for fatal preprocessing failure, 4 for an internal panic, 6 for serialization failure.
  - `build [--plan [--probe] [--all] [--files <path>... | --since <rev>]] [--json] [--events] [--report <file>] [--fail-fast] [--no-cache] [--no-remote-cache] [--wait-lock <seconds>] [--yield] [--workers <n>] [--root <dir>] [--config-overlay <file>] [--print-config]` — which compile units must rebuild and why (`--plan`), or compile them. See [`oxabl build --plan`](#oxabl-build---plan) and [`oxabl build`](#oxabl-build). Exit codes: 0 when everything compiled or a plan was printed, 1 when a unit failed to compile, 2 for a usage or configuration error, 3 when the store or the workspace cannot be used or no worker can start, 130 when a signal stops the command.
  - `self-update [--check [--json]]` (hidden from `--help`, like the other tool commands) — check for a newer release and install it (see [Install for developers](#install-for-developers)). Exit codes: 0 when it updated or found nothing newer, 2 when the binary is not managed by an installer or was built without the `self-update` feature, 3 when the check could not be established.
  - Usage: `cargo run -p oxabl -- check <path> --preprocess -I <include-path>`
  - Their `--json` shapes are not yet a stable contract.
- Hidden commands. They are real and reachable — `oxabl <command> --help` works — but they do not appear in `oxabl --help`: `conformance` and `analyze` answer a question about *oxabl* rather than about your source, and `daemon` is started by editor clients, not by hand.
  - `oxabl conformance <path>` — the parser-refinement instrument: how many files parse, which fail and where, and the ranked error patterns behind the failures.
  - `oxabl daemon <workspace-root>` — serve one workspace through the shared oxabl daemon, for clients that speak its `oxabl/*` methods. Its registration names the version of the binary. `oxabl lsp` stops an older daemon, for example one that outlived an upgrade, and starts its own; the other clients of that daemon lose their connection. A newer daemon is left alone, and `oxabl lsp` exits naming both versions. Two builds of one version are not told apart: after a local build, stop the daemon by hand. On a system other than Linux, `oxabl lsp` cannot confirm which process listens on the daemon's socket, so it does not stop an older daemon: it names the pid, and you stop it.
  - `oxabl analyze <file>` — a single file's resolved semantic model, dumped as a per-section-versioned JSON envelope or as text (`--schema <file|dir>` for schema-backed resolution). Introspection, not a gate: it exits 0 whatever it finds. Eight sections are emitted — `scopes`, `symbols`, `types`, `references`, `diagnostics`, `preproc`, `coverage`, and `dependencies` — each carrying its own version under `sections`, so a change to one leaves the others' numbers alone. `dependencies` is where cross-file resolution is observable: which files the run's workspace index actually consulted, and which cross-file names came back empty and why.
- `oxabl_wasm`: browser bindings in `crates/oxabl_wasm`.
  - Three `wasm-bindgen` exports — `analyze_source`, `format_source`, and `version()` (a crate version plus a build identifier, so a crash report names the exact artifact a hand-vendored copy is running) — the first two returning JSON: diagnostics carry source/severity/code/message, byte offsets, and line/column positions; a format result carries the new source, a `changed` flag, and an `error` string when the formatter declines to format (in which case the original source comes back untouched). Line and column come from the shared position helper the CLI's text output uses, so the two cannot drift.
  - Contains no ABL behavior. It is a transport adapter over the shared pipelines, which keeps every client on one implementation. A refusal collapses to one `error` string here because the wire shape has one field for it, while the pipeline keeps a deliberate bail and a contained panic apart for the clients that can tell them apart.
  - CLI-only dependencies live behind the `oxabl` crate's default-on `cli` feature so the library compiles for `wasm32`; a CI job builds the wasm target on every push.

Current Work: continued dogfood-driven trust-hardening, and typing `:`-qualified member and method-call expressions — the half of the cross-file population the type lattice still does not judge. Shipped: the semantic layer, the layout formatter (CLI + LSP), the language server, the VS Code extension, the curated `oxabl` public API, the browser WASM playground, the shared lint/format pipelines every client now drives, and cross-file/workspace resolution — inheritance chains, `USING` imports, literal `RUN` targets, and `SHARED` producers now link across files, with the CLI and the editor reaching identical answers from the same shared index. (The browser playground stays single-file: its exports take one source and no sibling files, so there is nothing for an index to look at.)

### `oxabl build --plan`

`oxabl build --plan` reads `oxabl.toml` and refreshes a graph of the working tree. It then lists the compile units that a change reaches. It compiles nothing. It starts no worker unless you add `--probe` (see [Compile keys](#compile-keys)). The command is listed in `oxabl --help`. The design is in `docs/design/build-tool.md`.

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
- Without `--probe`, `environment` is `{ "source": "none", "identity": null, "at": null }` and every unit has `compile_name: null`, `key: null` and `unkeyable: ["environment-unknown"]`. That is a statement about the plan, not a rebuild reason: the plan still lists only what changed. After a build, a unit that the paths and the configuration alone show cannot compile (`not-on-propath`, `shadowed-by`, `output-collision`, `package-mismatch`, `package-root-not-on-propath`) is a `skip` with that reason in `unkeyable`, as with `--probe`.
- With `--probe`, `environment` is `{ "source": "probe", "identity": "<hex>", "at": "<RFC 3339 time>" }`, and the document has two more fields. `propaths` lists the distinct unit PROPATHs, each as a list of `{ "entry": ..., "digest": ... }` (`dir`, `absent` or `sha256:<hex>`). Each unit has `propath`, an index into `propaths`, `compile_name`, and `key` (64 hex digits) or `null` with the reasons in `unkeyable`. `closure` says where the closure of the key comes from: `parsed` (the analysis of the source), `observed` (what the compiler read at the last compile, see below), or `null` without a key. With more than one PROPATH id, each unit that is a class or reaches one has `class_resolution`: `{ "uniform": <bool>, "differing": [...] }`. Each entry of `differing` is `{ "kind": "class" | "include", "name": ..., "via": <the file that writes the name>, "files": [...], "failed": [...] }`: `files` holds the file the analysis resolved the name to and every other file the name can resolve to, and `failed` names the conditions it fails (`U1`, `U3`, `rcode`, `name`; see `docs/design/build-tool/compile-key.md`, "Uniform resolution"). An entry gives the unit the reason `per-unit-propath-classes`, or `class-rcode` for `rcode`. The document then also has `class_resolution`: `{ "units": ..., "uniform": ..., "released": ... }`, the number of such units, how many of them are uniform, and how many have a key.

### Compile keys

A compile key is 32 bytes. It names one compile: the unit, its closure, its PROPATH, the environment of the worker, the compile options, the requested outputs and the worker program. `oxabl build --plan --probe` starts the worker pool, reads the environment, and computes the key of every unit. The pool starts at the same time as the refresh and stops when the keys are done. A failed probe fails the plan with the exit codes of `oxabl build --probe`. The key is BLAKE3 over a length-prefixed encoding of its inputs, with its own context string; `docs/design/build-tool/compile-key.md` gives the encoding and a worked example.

The PROPATH of a unit is `$OUT`, the `root_relative_include_paths` joined to the base directory of the unit (its own directory, or the package root of a class), `include_paths` (under the root, under `$DLC`, or `$LIB:<name>` from `[build.libraries]`), and the entries of the worker's own startup PROPATH that are under its `$DLC`. An `include_paths` entry that has none of these forms is a usage error. The compile name is the unit path below the first entry that holds it.

**A unit without a key.** Each reason is listed. The unit is in the rebuild set at every plan and compiles at every build.

| Rule | Reason | When |
|---|---|---|
| K1 | `degraded: <path>: <reason>` | The unit, or a class in its closure, has an edge set that may miss dependencies. An include is judged inside the unit that includes it, not as a root. |
| K2 | `unresolved-include: <name> in <path>` | An include does not resolve, in any branch. |
| K3 | `unresolved-class: <name> in <path>` | A class does not resolve, unless its name starts with a prefix of `[build.external_packages]` and a listed library is on the PROPATH with a SHA-256 digest. A directory never supplies a prefix. A name that a workspace superclass declares (a variable, buffer, temp-table and so on) is a member, not a class. |
| K4 | `computed-name: <name> in <path>`, `conditional-name: class statement in <path>` | An include or class name is computed. This includes a class unit whose `CLASS` statement oxabl cannot read in its own text. A class unit whose `CLASS`, `INTERFACE` or `ENUM` statement, or its name, is inside an `&IF` block is `conditional-name`: the branch the compile takes picks the name. Oxabl takes the name from the compile name when only one package root is possible: the class name is the compile name without its extension, with `/` as `.`. The compiler checks the `CLASS` name against the compile name (error 12629), and a unit that fails to compile is never cached, so a wrong guess cannot give a wrong cache hit. The unit keeps `computed-name` or `conditional-name` when more than one directory could be its package root (with `root_relative_include_paths`, or when two `include_paths` entries hold the file), when its path has a `.` in a name part, or, with `root_relative_include_paths`, when the package root is not the unit's own directory. |
| K5 | `outside-workspace: <path>`, `unreadable: <path>` | A file in the closure is outside the root and `$DLC`, or cannot be read. |
| K6 | `environment-unknown`, `unhashable-parameter: <name>`, `unhashable-library: $LIB:<name>`, `dlc-mirror-mismatch: <path>` | The environment is not known, a startup parameter names a directory, a `$LIB:` entry is a directory, or the worker's copy of a `$DLC` file differs from `[workspace.sources].dlc`. |
| K7 | `not-on-propath`, `shadowed-by: <path>`, `output-collision: <path>`, `package-mismatch: <name>`, `package-root-not-on-propath: <dir>`, `outside-root` | The unit cannot compile where it is. Its action is `skip`. Of units whose r-code lands on one path (compared ignoring ASCII case), the smallest path compiles. With the r-code beside the source, a unit outside the root is `outside-root`. |
| K8 | `class-conflict: <name>` | A name accepted as external is also the name of a workspace class. |
| K9 | `per-unit-propath-classes: <name> (<files>)` | The units have more than one distinct PROPATH, and a class name the unit reaches, or an include inside a class whose source its compile reads, resolves to different files: for another PROPATH, or on its own PROPATH against what the analysis found. A user reads a class unit of the build from its r-code in `$OUT`, which comes first, so only the file of that class unit counts for it, and not the includes of the class. |
| K10 | `class-propath: <name> in <class> finds <file>, analysed <file>` | With `root_relative_include_paths`, a class that is not compiled as a unit on the PROPATH of the unit (a class outside `directories`, for example) is compiled under the PROPATH of the unit that uses it, and that PROPATH finds another file for an include or class name of the class than the analysis did. The analysis searches the includes of a class from its package root. |
| K11 | `class-rcode: <file>` | The compile of the unit binds class r-code that oxabl does not hash: a class that is not a unit of this build, and the first PROPATH entry that holds `<pkg>/<Name>.r` or `<pkg>/<Name>.cls` holds the r-code (the compiler takes the r-code first in an entry, whatever the dates). R-code beside a class unit does no harm, because the build compiles the class into `$OUT`, which comes first. The build prints one warning that counts these units and names the r-code. Delete the stray r-code, or make the class a unit. |

**A unit that oxabl cannot read.** A source with a construct the analysis does not model, or an include name it cannot compute, has no key from its source: K1, K2 or K4 in the unit itself. When no other rule applies and the unit is not a class, the build reads the compiler's `XREF-XML` after a successful compile and stores what it read: the **observed closure**. The `XREF-XML` does not show every use of a class, so the closure also holds every `.cls` below the workspace entries of the unit PROPATH and every class unit, each with what it reads. The next plan keys the unit by those files, in a key that never equals a key from the source, so the unit compiles again only when one of those files changes. Such a unit compiles after the class units of the build. The unit has no key, and compiles, when a file of the closure is gone (`observed-missing: <path>`) or unreadable (`observed-unreadable: <path>`), when a new file can shadow one of its includes (`observed-shadowed: <path>`), when a class was added or removed (`observed-classes-changed`), when a workspace class has no key (`observed-class: <class>: <reason>`), or when the unit or a file it read looks encrypted (`observed-encrypted: <path>`): the compiler reports no include made inside encrypted source, so an encrypted unit, or a unit that includes an encrypted file, compiles at each build. A compile whose files the host resolves to other paths gives no observed closure, and the build prints a note. With `xref_format = "text"` nothing is observed. A machine with no store compiles such a unit once. `docs/design/build-tool/compile-key.md` gives the rules.

K3 sees only the class names that the analysis records. It records none from `OpenEdge.*` and `Progress.*`, so a prefix that names those namespaces does not change the key of a unit that uses them; the library digests on the PROPATH are in every key all the same.

**Parity with another tool.** `scripts/plan-parity.sh <plan.json> <list.txt>` compares the rebuild set of `oxabl build --plan --json` with a list of paths (one root-relative path per line) that another tool rebuilds for the same change. It prints the paths it **missed** (in the list, not in oxabl's set) and the **extras** (in oxabl's set, not in the list) grouped by oxabl's first reason, and exits 1 when anything is missed. It needs `jq`. `scripts/parity/run.sh` runs the scenarios under `scripts/parity/scenarios/` on the synthetic corpus in `scripts/parity/corpus/` (`OXABL=target/debug/oxabl scripts/parity/run.sh`); `cargo test -p oxabl` runs it too. A scenario is `edit.sh` (applied to a copy of the corpus after a first plan) and `expected.txt` (the list).

The closure of a unit is every file the compiler can read for it: its includes at any depth, the classes it names and the includes and classes of those, including the ones in branches an `&IF` did not take.

**What the plan compares.** Before a build has recorded results, a plan lists what changed since the last refresh that reached the store. A run that holds the workspace lock writes its refresh, so the next plan starts from it. Run `oxabl build --plan` again and the answer is "0 to rebuild" until a file changes. From the first build on, a plan compares every unit with its last result: a unit with no result is `new`, one that failed is `failed`, and a unit whose key, whose outputs or whose files differ from the result is in the set, so a build that was cancelled leaves its remaining units in the next plan. Without `--probe` there is no key, so the plan cannot see an environment, option or worker change; it says `environment from build at <time>` for the environment it did not probe. A plan that finds the lock taken (a build is running) reads a snapshot of the store and writes nothing. It ends with `(not committed)`.

**The store.** The state is in `[build].state` (default `.oxabl/build`), which must be under the workspace root: `state.db` (SQLite, WAL) and `build.lock`. Set `[build].store` to put `state.db` in another local directory when the root is on a network file system. SQLite WAL cannot run on NFS, SMB or FUSE, so oxabl stops with exit code 3 for a store there. A build writes a `.gitignore` that holds `*` in `.oxabl/`, so version control ignores it; see [Outputs and options](build-wrapper.md#outputs-and-options). The store is a cache of what the files say:

- A store with another format number is deleted and made again. There are no migrations.
- A store that is damaged or cut short is moved to `state.db.corrupt-<seconds>` (only the newest such set stays) and made again.
- A file at the store path that is not an oxabl store stops the run with exit code 3. Oxabl never deletes a file it did not create.

**How fast.** A file is read and hashed only when its size, times or inode changed. A stat taken in the two seconds before a file's last change is not trusted at the next run. A run with no change stats the files and reads the store, and that is all it does. On a synthetic tree of 28,000 files that takes about 0.2 s. An analysis runs again only for a file whose bytes changed, or that looked for a path, found a path absent, or listed a directory, where something changed since.

**Configuration.** `[build]` and its tables (`workers`, `outputs`, `compile`, `verify`, `cache`) are read strictly, as are the top level of `oxabl.toml`, `[workspace]`, `[workspace.sources]` and `[workspace.schema]`: an unknown table or key is a usage error (exit code 2) that names it. A duration is an integer and `s`, `m` or `h` (`"90s"`); a size is an integer and `KiB`, `MiB`, `GiB` or `TiB` (`"20GiB"`). `oxabl build` without `--plan` needs `[build.workers].command` and `[build].output` (or `[build].rcode = "beside-source"` and no `[build].output`), and stops with exit code 2 without them.

### `oxabl build`

`oxabl build` compiles the units that the plan lists. It needs `[build.workers].command` (the command that starts one AVM worker, see `docs/design/build-tool/worker-protocol.md`) and `[build].output` (the directory for the r-code), or `[build].rcode = "beside-source"`, which puts the r-code of each unit beside its source (`a/x.p` gives `a/x.r`) and has no `[build].output`. Oxabl ships no Progress code but its own small worker program. The build runs in this order:

1. It takes the workspace lock (`<state>/build.lock`) and, in the r-code directory, `.oxabl-build.lock`. A second build stops with exit code 3 and the process id of the first, unless the first build runs with `--yield` (see below).
2. It refreshes the graph and, at the same time, starts the workers, probes the environment and hashes the worker-side files. It computes the compile key of every unit and the plan.
3. It deletes the outputs that no result owns, then looks up the keyable units of the rebuild set in the cache and compiles. Nothing compiles before this.
4. The lookup and the compiles overlap: a unit that waits for a lookup is **pending**, and its users wait for it as they wait for a compile. A hit is installed at the final paths and recorded; a miss becomes a compile. Each worker takes the ready job of highest priority: the longest chain of units that waits for it, by last compile time. A job is a set of units that cannot be ordered, a cycle of classes, and goes to one worker, with `MULTI-COMPILE` on. A job of more than one unit compiles in two passes (three when a unit fails in the first): the compiler binds the first r-code it finds for a class, also r-code of an older compile, so only the last pass, which reads this build's r-code of the cycle, counts. When a unit failed in the pass before the last, the units of the job are recorded without a key. A unit compiles after every unit that it reads at compile time: classes before their users. A failed unit does not hold its users back; they compile and are marked **tainted** in the report. A tainted unit is recorded without a key, so the next build compiles it again, and it is never offered to a cache.
5. Each unit that compiled has its outputs moved from the worker's stage (`<state>/stage/<build>/w<n>-<s>`) to their final paths by `rename`, or by copy, `fsync` and rename across file systems. The result is recorded after the outputs are in place. Results are written to the store in batches of 500 units or two seconds, after `syncfs` on the output file system. A unit that failed loses its old outputs, so no user compiles against stale r-code.
6. Each unit that failed compiles once more, in the same order. A unit that failed because its worker was lost twice is not tried again. A unit that compiles on the second try counts as one success.
7. The last schema check runs, the uploader finishes, and the summary says what the cache did.

**R-code beside the source.** With `[build].rcode = "beside-source"`, a runtime whose PROPATH holds only the source directories runs the r-code of the build. Oxabl owns the r-code beside every unit: before the first compile it deletes the r-code beside each skipped unit and each class unit it builds, and a unit that fails loses its r-code. It creates no directory there, and a source directory it cannot write stops the build with exit code 3. The unit PROPATH has no output entry. The manifest `<state>/installed` lists the r-code that oxabl installed beside the sources. A build deletes the r-code in it that no unit expects now (a deleted source, a skipped unit, or the mode `directory` now), when the bytes are the ones oxabl installed, and a directory that this empties. A switch to `beside-source` deletes the r-code in the old `[build].output` and that directory. When the store is gone, oxabl does not know the old output directory: delete it by hand. A `.r` file that was changed or deleted by hand is installed again. The code base's `.gitignore` must ignore `*.r`. See `docs/design/build-tool.md`, "R-code beside the source".

**Output directories.** `[build].output` and the directories of `[build.outputs]` belong to oxabl. Each must be absent or empty the first time (oxabl writes `.oxabl-output` into it), and must not lie inside another. A directory that holds files and has no marker stops the build with exit code 2 and nothing is deleted. Oxabl deletes every file in a marked directory that no result owns.

**Workers that fail.** A worker is lost when its output ends, its process exits, a unit takes longer than `[build.workers].unit_timeout`, or it breaks the protocol. Oxabl stops its process group, starts another worker in the slot and puts the unit that was running, and the units of the same request that had not started, back in the queue. A unit that loses its worker twice in one build fails with `worker lost twice while compiling this unit`. When no replacement can start the build goes on with the other workers; with none left it stops with exit code 3.

**Cancel.** The first `SIGINT` or `SIGTERM` stops dispatch. Busy workers have `[build.workers].cancel_grace` (30 s) to finish their unit; what they finish is installed and recorded. Then the workers get `QUIT`, and any that is still busy is stopped. A second signal stops every worker at once. The exit code is 130 and the store is consistent: the next build compiles what was not recorded. If oxabl is killed, the workers read the end of their input and quit after their current unit.

**`--yield`** makes a build step aside for any newer build. It writes `<pid> yield` (one line) into the workspace lock instead of the pid alone. A later `oxabl build`, with or without `--yield`, that finds the lock busy and the holder marked this way checks that the pid is an `oxabl build --yield` process (so a reused pid is safe), sends it one `SIGINT`, prints `stopping the build (pid N) that yields to a newer one` to stderr, and waits for the lock for `[build.workers].cancel_grace` plus 30 s (or `--wait-lock`, if that is longer). The stopped build exits with 130 like any cancel. A holder that is not marked keeps the old behaviour: exit code 3, or wait with `--wait-lock`. An editor or a wrapper uses `--yield`, so a build from a terminal or an agent does not wait for it.

**`--fail-fast`** stops dispatch at the first failed unit; the units not started are `not-run` in the report, and there is no retry pass.

**`--events`** writes one JSON object per line to stdout while the build runs; the human output goes to stderr. **`--report <file>`** writes the result of the build as JSON, and **`--json`** prints the same document instead of the text. [What `oxabl build` reports](build-output.md) lists every event with its fields, the fields of the report and the exit codes.

**The overlay.** `--config-overlay <file>` (or `OXABL_BUILD_OVERLAY`) names a TOML file with the schema of `oxabl.toml`. Oxabl merges it over `oxabl.toml` key by key (an array replaces) and then reads the result strictly. It may set `[workspace] name`, `[workspace.sources]`, `[workspace.preprocessor]`, `[workspace.schema]` and `[build]`; any other table, an unknown key, a key whose name looks like a secret and a trust key of the remote cache (`tenant`, `cache`, `owner_keys`, `min_roster_version`) are exit code 2. Without an `oxabl.toml` the overlay is the whole configuration. For one setting the order is: flag, environment variable, overlay, `oxabl.toml`, default. `oxabl build --print-config` prints the effective settings, each with its source, and stops. A tool that owns the environment of a workspace (containers, databases, PROPATH) writes an overlay for each run; see [Drive `oxabl build` from a wrapper](build-wrapper.md).

**Exit codes.** 0 every unit compiled, 1 a unit failed after the retry pass (a skipped unit is not a failure), 2 a usage or configuration error (no worker command, no `[build].output` and no `[build].rcode = "beside-source"`, or both, a bad overlay, an output directory that oxabl does not own, workers that disagree), 3 the store, a lock or the workers could not be used, 130 cancelled.

**Caches.** A cache is an optimisation: any cache error is a miss, never a failed build and never a wrong hit. `--no-cache` turns off both caches, `--no-remote-cache` the remote one. A workspace that turns the remote cache on (`[build.cache].remote = true`, or a `.oxabl/cache.toml` file) needs a binary that was built with the remote cache; without it the build stops with exit code 2 and says so, so that a CI job does not run slowly with no signal, unless you pass `--no-remote-cache`. The build ends with one line, for example `local: 12 sets offered, 12 stored`.

**The local cache** is a directory of entries named by compile key, `$XDG_CACHE_HOME/oxabl/artifacts` (`~/.cache/oxabl/artifacts`; `~/Library/Caches/oxabl/artifacts` on macOS) or `[build.cache].local`; `OXABL_BUILD_CACHE_DIR` beats both. Two copies of one workspace, or two branches, share its entries, because a key depends on content and not on the path of the workspace. An entry holds every output file of one unit and a manifest with the size and the BLAKE3 hash of each file. A read copies each file to the stage and hashes it; a file or a manifest that differs, or an entry that lists other files than the build asks for, is a miss, and a damaged entry moves to `v1/quarantine/`, which the next eviction deletes. An entry is built in `v1/tmp/` (by reflink, hard link or copy) and renamed into place whole, after one `syncfs` for a batch. A hard-linked entry shares its inode with the output, so an edit in place of the output makes the next read a miss, never a wrong hit. When the recorded size passes `[build.cache].local_max_size` (20 GiB), the end of a build removes the entries with the oldest `last-used` until 90 % of it is left. The directory is created with mode `0700`; a directory that another user owns, or that the group or others can write, is not used (one warning, every key a miss).

**Who the local cache trusts.** It trusts every process that runs as the user, so it is on only in `developer` mode. The mode is `OXABL_CACHE_MODE` (exactly `ci`, `developer` or `read-only`; another value is exit code 2), else `read-only` when `CI` is set, else `developer`. In `ci` and `read-only` mode the shared cache is neither read nor written. A CI system that does not set `CI` must set `OXABL_CACHE_MODE`. In those modes `[build.cache].local_job_dir` names a directory that serves as the local cache of one job: it must be absent or empty the first time (oxabl writes `.oxabl-cache-job` into it); a directory with files and no marker, the workspace root, an ancestor of it, `$HOME` and `/` stop the build with exit code 2, and nothing is deleted; at the start of each build oxabl deletes only `v1/` beneath it. A local entry that came from a remote cache records the tier and the signer in its manifest, but nothing authenticates that line, so the report shows it as `origin_unverified`.

**The remote cache** is in the binary only when it is built with `--features remote-cache` (`cargo install oxabl --features remote-cache`). The feature is off by default, so the library, the WebAssembly build and a plain `cargo install oxabl` carry no network or crypto code. It is on for a workspace when `[build.cache].remote` is `true`, or when `.oxabl/cache.toml` exists and `remote` is not set; `remote = true` without that file is exit code 2. A binary without the feature stops with exit code 2 and the message `this oxabl was built without remote-cache; rebuild with --features remote-cache or pass --no-remote-cache`.

The file `.oxabl/cache.toml` is part of the repository. It holds `server`, `tenant`, `cache`, `owner_keys` and `min_roster_version` (see the oxabl-cache client documentation), and no secret. `tenant`, `cache`, `owner_keys` and `min_roster_version` are the root of trust: no oxabl file, flag or environment variable can set them. naming one in `oxabl.toml` or in an overlay is exit code 2, and the message names the key; one of `OXABL_CACHE_TENANT`, `OXABL_CACHE_CACHE`, `OXABL_CACHE_OWNER_KEYS` and `OXABL_CACHE_MIN_ROSTER_VERSION` in the environment stops the build with exit code 2 that names the variable, before oxabl compiles or reads any cache. `OXABL_CACHE_SERVER` changes the server only. The mode (`OXABL_CACHE_MODE`, `CI`) and the credentials (`OXABL_CACHE_TOKEN`, `OXABL_CACHE_IDENTITY`, OIDC) come from the environment; oxabl passes them to the client and never reads or prints a credential. Any other problem with the remote cache gives one warning, and the build runs without it: a configuration the client rejects, a server that is down, an answer that fails verification, or no progress for `[build.cache].idle_timeout` (30 s; a slow transfer that keeps sending is not cut off). Each key that the remote cache cannot give is compiled.

A mode that writes (`ci`, `developer`) offers every verified set; the credentials decide where it goes. In `read-only` mode nothing is offered and the summary says `remote: read-only, 0 sets offered`. A `ci` identity that is not a `ci` member of the roster gives one warning, and every offered set shows as `not stored (Unavailable)`. The files of an offered set are read from the output directories; an output that is a symbolic link is `not stored (Unreadable)`, and a link in a directory above it is allowed. Remote hits are also stored in the local cache, with their tier and signer as `origin_unverified`.

**Verification.** A key is computed before the compile, so a set may go to a cache only after the unit passes three checks, which run while the build runs:

- **V1, inputs.** The unit and every file of its closure are hashed again after the unit finished and must equal the hashes of the key. A `$DLC` file is hashed again by the worker that compiled, from the copy the AVM compiles with. A file that changed during the compile (an edit while the unit compiles) keeps the unit out of the caches; the summary counts such units, and the next build compiles them again.
- **V2, schema.** Before a set is released the databases are imaged again, split across the idle workers, by a check that started after the unit finished. It must give the schema identities of the probe. A check starts when a set waits and `[build.verify].imaging_budget` (30) times the duration of the last imaging has passed since it ended, so the checks cost about 3% of a build, and once at the end. `batch_units` and `batch_interval` are ignored. A check that differs, or fails, stops every held and every later write of the build. A cancel releases only the sets that a check already released.
- **V3, reads.** The compile also writes a private text `XREF` (an `xref` output that you ask for serves, in either `xref_format`, otherwise a file under the stage that is not an output). Every file in the second column of its rows (or every `Source` of an `XREF-XML`) must be the unit or in its closure, as the compiler resolved it. A file that is absent, cut off or not in the shape of the format fails V3. R-code under the output directory and members of a procedure library are skipped when the closure holds a class: class resolution is protected by the plan rules. With the r-code beside the source no r-code is skipped. A unit that fails V3 is not written; its result gets no key and the file in its note, so it compiles at each build and the plan says `unverified: <file>`. The analysis missed an input: please report it. On OpenEdge 12.8 the compiler names a file only when it holds a reference. An include with only preprocessor definitions has none, and no class file has one (not a class from source, from the output directory or from a procedure library), so V3 cannot check how those were resolved. The plan rules protect them: classes are keyed only with one PROPATH id and without an external name collision, and every closure file counts by its path and bytes.

With `[build.verify].reads = false`, nothing is written to any cache, and the build says so. A cache that does not write (`read-only`, `--no-cache`) skips verification. The cache mode sets the verification policy: `ci` runs every check at full strength and refuses a setting that would lower one while the build still writes; `--print-config` shows the mode as `verification.mode`, and the report holds the policy and the number of V2 checks.

**The seam.** `oxabl::build::cache::ArtifactCache` has `fetch` (all keys at once, one outcome per key as each completes, every set installed below a stage and complete before it is reported), `uploader` (one `push` per verified set, with the artifact paths and the paths of the files in the output directories, never their bytes; `finish` returns one `StoreOutcome` per key), `writes` and `fill`. A panic in a cache call is caught and is a miss. `Layered` asks a near cache first and a far cache for the misses, and stores what the far cache gave in the near one.

**The worker program.** `PROPATH`, `OPTIONS` and `COMPILE` requests are answered by `OK`, `MSG`, `DONE` and `END`. The worker writes one small procedure for each shape of `COMPILE` statement into its stage, because the phrases (`XREF-XML`, `MIN-SIZE`, `GENERATE-MD5`, ...) are keywords; `languages` and `options` hold only letters, digits and `-_:,.`. The stage path that oxabl sends starts with `./`, because the compiler removes a leading `.` from a relative XREF file name. `DONE` counts the `MSG` lines of the unit, and a count that differs from the lines that came is a protocol error (the worker is lost). A message that has no number, row or column has `0` there, and a message with no file has the unit as its file. The text report lists a compiled unit that has messages, such as a warning with no position, as `warning <number> <file>[:row:column]: <text>`. The compile key hashes the compile part of the worker program only; the driver part (protocol, schema image, messages) can change with no rebuild. A failed unit always has a reason: the compiler messages, the error that stopped the compile, or a note that the worker gave none. The PROPATH entries that oxabl sends are relative to the worker's working directory (the output directory first, when there is one), `$DLC` entries (the worker expands them) or the paths of `[build.libraries]`.

#### Install for developers

On Linux x86_64, one command installs the static release binary (the one CI uses, see below) in `~/.local/bin`:

```sh
curl -fsSL https://raw.githubusercontent.com/oxabl-project/oxabl/master/scripts/install.sh | sh
```

The script checks the SHA-256 of the download, starts the new binary once with `--version`, and then replaces the old one in one step. If a check fails, the old binary stays. It also writes the install record `${XDG_DATA_HOME:-~/.local/share}/oxabl/install.json`. Set `OXABL_INSTALL_DIR` to install somewhere else and `OXABL_VERSION=X.Y.Z` to pin a version. On Windows, run the script in WSL. On macOS, use `cargo install oxabl`. The script warns you when the install directory is not on `PATH`.

**Self-update.** A binary that the script (or the VS Code extension) installed is *managed*: the install record names its path. A managed binary built with the `self-update` feature (the release binary has it) updates itself:

- `oxabl self-update` checks and installs now. It prints `oxabl was updated from 5.1.0 to 5.2.0` or `oxabl is up to date (5.1.0)`. Exit code 2 means the binary is not managed. Exit code 3 means oxabl could not establish the newest release (network, API, an incomplete release or a failed check). That is never "up to date".
- `oxabl self-update --check [--json]` installs nothing. The JSON is `{"installed":"5.1.0","latest":"5.2.0","update":true,"managed":true}`.
- At the start of `oxabl lsp`, and after `build`, `check` and `format`, a managed binary starts a detached `oxabl self-update --background` when a check is due: at most one success a day, and after a failure a back-off of 15 minutes that doubles up to a day. The command never waits for the network. The state, the lock and the log are in `${XDG_STATE_HOME:-~/.local/state}/oxabl/` (`update-state.json`, `update.lock`, `update.log`).
- It is off when `CI` or `OXABL_NO_SELF_UPDATE` is set, when the binary is not managed, and when the binary has no `self-update` feature. The feature is off by default, so a plain `cargo install oxabl` carries no network or crypto code; `cargo install oxabl --features self-update` adds it.

Oxabl finds the newest release from the tags `oxabl-vX.Y.Z` (the highest plain version) and reads that release: it must not be a draft or a pre-release, and it must have both assets. It trusts HTTPS to GitHub and the SHA-256 of the release; there are no signatures. A running `oxabl lsp` or build keeps its old file; nothing restarts it. For tests, `OXABL_UPDATE_API_URL` replaces `https://api.github.com`. Oxabl uses it only for a loopback host (`127.0.0.1`, `localhost`, `[::1]`).

#### Install the binary in CI

Each release of the `oxabl` crate attaches `oxabl-x86_64-unknown-linux-musl` and `oxabl-x86_64-unknown-linux-musl.sha256` to its GitHub release (the tag is `oxabl-v<version>`). The binary has the `remote-cache` and `self-update` features (set `CI` or `OXABL_NO_SELF_UPDATE` and it does not update itself). It is static, so it does not need glibc and it starts in any Linux image, such as the usual OpenEdge images (Ubuntu 22.04). It uses the jemalloc allocator.

Download the binary, check the SHA-256, and run it. This is an example for a GitHub Actions or Gitea Actions job:

```yaml
- name: Install oxabl
  run: |
    version=3.0.2   # the release you want
    base=https://github.com/oxabl-project/oxabl/releases/download/oxabl-v$version
    curl -fsSL -O "$base/oxabl-x86_64-unknown-linux-musl"
    curl -fsSL -O "$base/oxabl-x86_64-unknown-linux-musl.sha256"
    sha256sum -c oxabl-x86_64-unknown-linux-musl.sha256
    install -m 0755 oxabl-x86_64-unknown-linux-musl /usr/local/bin/oxabl
    oxabl --version
- run: oxabl build
```

The `sha256sum -c` step fails the job when the file is not the one that the release holds. Pin the version: do not download "latest" in a job that you cannot repeat.

To build the same binary yourself, run `scripts/build-musl.sh <out dir>` (it needs Docker), then `scripts/check-musl.sh <out dir>/oxabl-x86_64-unknown-linux-musl`. CI runs both scripts on each pull request.

#### Testing against a real AVM

The black-box tests of `oxabl build` use a fake AVM. The fake also stands in for the runtime: a test asks it which file a program runs, with the runtime PROPATH in its `PROPATH` variable. One table of runtime cases (`crates/oxabl/tests/common/runtime_cases.rs`) runs against the fake in CI and against OpenEdge in `build_openedge/runtime.rs`, so the fake keeps the measured rule. Two opt-in test files run the tests against OpenEdge. Every test in them is `#[ignore]`, and a test fails, never skips, when its variables are missing.

- `crates/oxabl/tests/build_openedge.rs` needs an installation or container with databases. `OXABL_OPENEDGE_COMMAND` is the worker command as a JSON array with `{program}` and `{root}` (it connects the databases through its `.pf` and starts in `{root}`). `OXABL_OPENEDGE_WORKSPACE` is a host directory that the worker sees, `OXABL_OPENEDGE_WORKER_ROOT` is the same directory as the worker sees it, `OXABL_OPENEDGE_PROVERSION` is the version as `PROVERSION` prints it (`12.8`), and `OXABL_OPENEDGE_DATABASES` lists the connected databases in order. `OXABL_OPENEDGE_WORKERS` (default `1,4,8`) gives the worker counts of the timing test. The runtime tests (`build_openedge/runtime.rs`) set the runtime `PROPATH` in the environment: they put `env PROPATH=...` in front of the AVM executable, so the command must name `_progres` (or `pro`, `mpro`, `bpro`), and `env` must exist where it runs.
- `crates/oxabl/tests/build_openedge_scratch.rs` creates scratch databases from the empty database of the installation and never touches another. `OXABL_OPENEDGE_EXEC` is the prefix that runs a program where OpenEdge is (`["docker","exec","-i","-w","{root}","<container>"]`), `OXABL_OPENEDGE_SCRATCH` is a directory there, and `OXABL_OPENEDGE_DLC` (default `/psc/dlc`) is the installation. It also reads the workspace, worker root and version variables above.

```bash
cargo test -p oxabl --test build_openedge -- --ignored --nocapture --test-threads 1
cargo test -p oxabl --test build_openedge_scratch -- --ignored --nocapture --test-threads 1
```

The compile tests build the fixture in `crates/oxabl/tests/fixtures/openedge/`. They compare r-code with a mask: r-code is not byte-deterministic, so two files are equivalent when they have the same size and differ only where repeated compiles of one unit in one root differ (and two bytes around each such byte). The tests print `EVIDENCE` lines for what the compiler lists in `XREF-XML` and which class a unit is compiled against.

### Feature switches

The `[workspace.features]` table in `oxabl.toml` turns oxabl features on or off for a project. `oxabl.toml` is the one place that decides. In an `oxabl.toml`, both keys default to `true`.

| Key | Covers in the editor | Covers in the CLI |
|---|---|---|
| `diagnostics` | Every diagnostic oxabl reports for a source file (parse, preprocessor, semantic and lint findings) and the code lenses of the effect rules. | `oxabl check` does not run or report the diagnostics channel. |
| `format` | Formatting, and so format on save. | `oxabl check` does not run or report the format-drift channel. `oxabl format` refuses to run. |

With `diagnostics = false`, the language server publishes an empty list for each open document. This clears what it published before. With `format = false`, `textDocument/formatting` returns no edits. The server keeps the formatting capability, because the file can change while the server runs. A change to `oxabl.toml` applies at once, with no reload.

`oxabl check` writes one note on stderr for each feature that is off, for example `note: diagnostics are off in oxabl.toml ([workspace.features]); not checked`. If both features are off, `check` exits 2. A gate that checks nothing must not pass. `check --no-format` with `diagnostics = false` also checks nothing, so it also exits 2. `--update-baseline` exits 2 with `diagnostics = false`, because it would erase the recorded findings. `--json`, `--watch` and `--baseline` follow the same rules.

`oxabl format` (also `--check` and `--stdout`) exits 2 with `format = false`. It writes nothing. `--style` skips `oxabl.toml`, so it also skips the switch. No command-line flag overrides `[workspace.features]`. `build`, `explain`, `analyze`, `lsp` and `daemon` do not read it.

**An `oxabl.toml` that oxabl cannot read turns both features off.** The language server then publishes no diagnostics and returns no formatting edits until the file is fixed. It sends one `window/showMessage` warning that names the problem, and it keeps the `window/logMessage` lines. A project that turned a feature off does not get it back from a typo.

**With no `oxabl.toml` at all, the language server turns both features off,** and the CLI keeps both on. An editor offers only what needs no setup, the document outline, until a project adds an `oxabl.toml`. So installing the CLI for the outline does not start formatting or linting code that no project configured. The server shows no message about it.

A project that wants only the highlighting and the outline in the editor turns both off:

```toml
[workspace]
name = "my-project"

[workspace.features]
diagnostics = false
format = false
```

Remove the table, or set a key to `true`, to turn the feature on.

### Lint configuration

Lint rules are in groups. The `LINT` rules are in the `default` group, which is always on. The `strict.transactions`, `strict.locks`, `strict.access`, `strict.handles`, `strict.nulls`, `strict.calls` and `security` groups are off until you enable them. The plan for all strict rules is in `docs/design/strict-analysis-plan.md`. These strict rules are available now. They are in preview and report at `warn`:

| Code | Name | Group | Reports |
|---|---|---|---|
| CL001 | `run-value` | `strict.calls` | `RUN VALUE(...)` with a computed target |
| CL002 | `dynamic-invoke` | `strict.calls` | `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `DYNAMIC-PROPERTY`, `CREATE CALL` |
| CL003 | `signature-mismatch` | `strict.calls` | a `RUN` whose arguments differ from the callee's parameters in count, kind, mode or type |
| CL004 | `unresolved-call` | `strict.calls` | a `RUN` of a name that is neither a procedure of the file nor a program on the search paths |
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
| NL001 | `unknown-in-comparison` | `strict.nulls` | a possibly unknown variable used in an ordering comparison or condition |
| NL002 | `unknown-propagation` | `strict.nulls` | a possibly unknown variable in arithmetic that is stored or displayed |
| NL003 | `field-after-failed-find` | `strict.nulls` | a buffer field used after a `FIND ... NO-ERROR` that may have failed, with no `AVAILABLE` test |
| NL004 | `nullable-db-field` | `strict.nulls` | a database field that starts at `?` and is not mandatory, used without a test for `?` |

The six rules before `HD001` and the `NL` rules are context-free: each finding depends only on the text of its own file, expanded with no arguments and no includer defines. The `HD` rules read the lifetimes of dynamic objects in one file (`oxabl_handles`) and are checked the same way. They are in preview. An include that ends with an object still live may leave the delete, or the widget pool, to its includer. Such a finding says so in its help, and no autofix may change it. `HD005` is reported once per file, at the file's own first create: a program reports its own first create, and an include that is checked alone reports its own. A `HD` finding whose anchor is in an include is reported in the include when the include alone shows it. Otherwise (the object is created or named in the program, or deleted in an include that has no create) it is reported in the program, at the `{...}` that includes the file, with the include's path and line in the message and a lower-confidence note in the help. It is never reported in both places. The `LK` rules and `TX001`-`TX005` read the buffer scopes, transaction blocks and locks of one file (`oxabl_scope`) and are routed like the `HD` rules. They report only on tables of the loaded schema, so they are silent when no schema is configured (a `DO TRANSACTION` needs no schema for `TX003` and `TX004`). They never report when the analysis cannot decide (a recovered parse, an unexpanded include). `LK002` and the `TX` rules rest on rules of the scope model that are not yet checked against a compiler listing (`docs/design/strict-analysis-open-questions.md`, sections 7.7 and 7.8). `DB001` reads the database effects of one file (`oxabl_effects`), over the same scope analysis, and is routed and silenced the same way (section 8.8). For most files that is the whole answer. It is not for a parametrised include fragment: `{1}`, `{&name}` and `&IF DEFINED(...)` resolve against no includer, so a statement that holds one can compile differently in each includer.

**Include files.** A strict rule reports code from an include in the include file, once, and not in the program that includes it. The one exception is a `HD`, `LK`, `TX001`-`TX005` or `DB` finding that the include cannot show alone (see above). `oxabl check <dir>` therefore also checks the `.i` files of the tree, but only for context-free rules, and only where the configuration enables one for that file. A directory that holds only such include files is checked too; `check` exits 2 for "no ABL files" only when it finds nothing to check. Other findings of a fragment (parse errors, undefined names, unused variables) stay hidden, because a fragment often does not parse or resolve alone. A baseline keys such a finding by the include's path. A finding is lower confidence when the part of its statement that the rule reads can change with the includer: a `{&name}`, `{n}`, `{*}` or `{file.i <args>}` reference, an `&IF`, or an argument-less `{file.i}` that did not resolve or has a reference or an `&IF` of its own. An include guard does not count (also the `&IF DEFINED(x) &THEN &ELSE <define x> &ENDIF` form); a comment is not part of a statement; a `{&name}` that an earlier `&SCOPED-DEFINE` in the same fragment gave a fixed value outside any `&IF` does not count (a `&GLOBAL-DEFINE` still does); and `RUN VALUE` (CL001) counts only for its target. The help of such a finding says that it depends on the including file, and no autofix may change it. A TX006 finding on a definition with a literal `SHARED` keeps the "producer and every consumer" help; one whose modifier slot holds a reference says that an includer may make it `SHARED`. The run cannot see what an includer's arguments would add, for example a `NEW SHARED` modifier that comes from `{&mod}`, so such a finding can also be missing. Limits:

- `oxabl check <file>` checks that one file. It does not check the includes of a program. Name the `.i` file to check it.
- `check` does not find an include outside the directory that it walks, or an include with an extension other than `.i`.
- In the editor, an open `.i` file shows its context-free findings. The editor does not show the help, so it does not show the lower-confidence note.
- The text summary and `--json` count these files apart from the roots (`include_files_checked`).

See `docs/design/strict-analysis-open-questions.md` for the reasons and for the plan for rules that depend on the includer.

#### Security rules

The `security` group finds code where a value that is not a literal reaches a dangerous call. Turn it on with `enable = ["security"]`; `enable = ["strict"]` does not select it. The rules are in preview. By default they are sink rules (stage 1): each one reports every call whose argument is not a literal, and does not follow the value back to its source. They are complete and noisy on purpose. Use a baseline for the code that exists, and see `crates/oxabl_lint/src/rules/security/mod.rs` for the design. Ten of the rules also have a taint mode (stage 2, below) that reports a sink only when untrusted input reaches it. With an entry-point file (stage 3, below), every finding says how a remote caller reaches it, and three more rules read the call graph.

| Code | Name | Group | Reports | Severity |
|---|---|---|---|---|
| ABLSEC001 | `dynamic-query` | `security` | `QUERY-PREPARE`, `FIND-FIRST`, `FIND-LAST` and `FIND-UNIQUE` on a buffer, and `FILL-WHERE-STRING`, given a query string that is not a literal | `error` |
| ABLSEC002 | `foreign-sql` | `security` | `RUN STORED-PROC send-sql-statement`, `NEW SqlCommand(...)` (and the ODBC, OLE DB, Oracle, MySQL and Npgsql classes and data adapters) and `CommandText` set from a string that is not a literal | `error` |
| ABLSEC003 | `os-command` | `security` | `OS-COMMAND`, `UNIX`, `DOS` and `INPUT`, `OUTPUT` or `INPUT-OUTPUT THROUGH` with a `VALUE(...)` that is not a literal | `error` |
| ABLSEC004 | `dynamic-code` | `security` | `RUN VALUE`, `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `Progress.Lang.Class:GetClass`, `COMPILE VALUE` and `CALL-NAME` with a name that is not a literal | `error` |
| ABLSEC005 | `path-traversal` | `security` | `INPUT FROM` and `OUTPUT TO` with `VALUE(...)` or `OS-DIR(...)`, `OS-DELETE`, `OS-COPY`, `OS-RENAME`, `OS-CREATE-DIR`, `COPY-LOB ... FILE` and `FILE-INFO:FILE-NAME` with a path that is not a literal | `warning` |
| ABLSEC006 | `xss-output` | `security` | `{&OUT}`, `PUT STREAM WebStream`, `Write` on a response writer and `Entity` of a response given a value that is not a literal | `warning` |
| ABLSEC007 | `header-injection` | `security` | `SetHeader`, `AddHeader`, `SetCookie`, `Location`, `HttpHeader` and `Cookie` objects, `HttpHeaderBuilder`, and WebSpeed `output-http-header` and `set-cookie` given a value that is not a literal | `warning` |
| ABLSEC008 | `hardcoded-secret` | `security` | `CONNECT` with a literal `-P`, an `oech1::` string, `ENCRYPT`, `DECRYPT` and `GENERATE-PBE-KEY` with a literal key, and a literal assigned to a password-like name | `error` |
| ABLSEC009 | `weak-crypto` | `security` | `ENCODE`, `MD5-DIGEST`, `SHA1-DIGEST` and `MESSAGE-DIGEST("MD5")` of a password-like or token-like name, and `RANDOM` that feeds a token, nonce or identifier | `warning` |
| ABLSEC010 | `client-identity` | `security` | an `IF`, `CASE`, `WHILE` or `WHERE` that decides on a user, company or role that the client sent (`get-value("role")`, `poRequest:GetPathParameter("tenant")`), and such a value passed to an `auth-routines` routine | `warning` |
| ABLSEC011 | `missing-authorization` | `security` | a database write or a `sensitive-calls` call that an entry point reaches on a path with no `auth-routines` check (needs entry points) | `warning` |
| ABLSEC012 | `cross-request-state` | `security` | a read of `NEW GLOBAL SHARED` or `STATIC` state that a request makes before it sets it, where other reached code writes it (needs entry points) | `warning` |
| ABLSEC013 | `information-leak` | `security` | `ERROR-STATUS:GET-MESSAGE`, `GetMessage` and `CallStack` of a caught error assigned to an output parameter or written to a web response, and a password-like name passed to `MESSAGE` or `LOG-MANAGER:WRITE-MESSAGE` | `warning` |
| ABLSEC014 | `unsafe-deserialization` | `security` | `Deserialize` of a value that is not a literal | `warning` |
| ABLSEC015 | `untrusted-xml` | `security` | `X-DOCUMENT:LOAD` of a source that is not a literal, and `SAX-PARSE`, `SAX-PARSE-FIRST` | `info` |
| ABLSEC016 | `resource-exhaustion` | `security` | `SET-SIZE(m) = expr`, a socket `READ` with a byte count that is not a literal and has no cap, and `FILL` with no `BATCH-SIZE` in the file | `info` |
| ABLSEC017 | `integrity-bypass` | `security` | `DISABLE TRIGGERS`, and `CREATE`, `DELETE` and field assignments on `_User` | `warning` |

The catalogue rates ABLSEC002 to ABLSEC004 critical; ABLSEC001, ABLSEC005, ABLSEC006, ABLSEC008, ABLSEC009, ABLSEC010, ABLSEC011 and ABLSEC014 high; ABLSEC007, ABLSEC012, ABLSEC013, ABLSEC016 and ABLSEC017 medium; and ABLSEC015 "needs review". oxabl has `error`, `warning`, `info` and `hint`, so critical impact and ABLSEC001 and ABLSEC008 report as `error`; the other high and medium impact rules report as `warning`, because their patterns are often benign or heuristic; and ABLSEC015 and ABLSEC016 report as `info`. Change that per rule in `oxabl.toml` as for any rule. SARIF output also carries the catalogue rating as `security-severity`: `9.5` for critical, `8.0` for high, `5.5` for medium and `3.0` for needs review. `// oxabl-ignore ABLSEC001: <reason>` suppresses one finding.

An argument counts as a literal when it is a string or number literal, a `+` of literals, or text that a preprocessor name expanded to one. A variable, a field and a function call are not literals. `QUOTER(x)` around a value does not make a string a literal: in stage 1 the rules still report it, and the help of the finding says so. Taint mode accepts sanitisers.

##### Taint mode (stage 2)

In taint mode a rule reports a sink only when a value from an untrusted source can reach its argument, on some path, without a sanitiser that protects that kind of sink. The analysis looks at one procedure, function or method at a time, over its control-flow graph. Stage 1 stays the default until the precision of stage 2 is measured on real code. Turn taint mode on in `oxabl.toml`:

```toml
[workspace.lint]
enable = ["security"]

[workspace.lint.security]
mode = "taint"                 # "sink" (stage 1, the default) or "taint"
parameters = "external"        # which INPUT parameters are untrusted: "external" (default), "all", "none"
database-fields = false        # a database field read is untrusted
sources = ["read-request-body"]  # project functions and methods whose result is untrusted

[workspace.lint.security.modes]
ABLSEC005 = "sink"             # one rule, by code or name; beats `mode`

[[workspace.lint.security.validators]]
name = "is-valid-code"         # a project function that validates or cleans a value
protects = ["query", "path"]   # the sink kinds it protects, or "all"
```

The table is global: an override does not read it. `check` warns about a rule in `modes` that is unknown or not a security rule, and about an unknown sink kind.

| Rule | Sink kind | In taint mode |
|---|---|---|
| ABLSEC001 `dynamic-query` | `query` | the query or where string |
| ABLSEC002 `foreign-sql` | `sql` | the SQL text |
| ABLSEC003 `os-command` | `command` | each `VALUE(...)` |
| ABLSEC004 `dynamic-code` | `code` | the name of what runs |
| ABLSEC005 `path-traversal` | `path` | the path |
| ABLSEC006 `xss-output` | `html` | the written value |
| ABLSEC007 `header-injection` | `header` | each non-literal argument |
| ABLSEC014 `unsafe-deserialization` | `deserialize` | the data |
| ABLSEC015 `untrusted-xml` | `xml` | the `LOAD` source; `SAX-PARSE` stays a review |
| ABLSEC016 `resource-exhaustion` | `size` | the `SET-SIZE` length and the socket `READ` byte count; `FILL` stays a review |

ABLSEC008, ABLSEC009, ABLSEC013 and ABLSEC017 do not ask where a value comes from, so they are the same in both modes. A taint finding is always a stage 1 finding too: stage 2 only removes findings.

Sources: the `INPUT` and `INPUT-OUTPUT` parameters of a unit that code outside the file can call (the main block, a `PROCEDURE` or `FUNCTION` that is not `PRIVATE`, a `METHOD`, `CONSTRUCTOR` or property setter that is not `PRIVATE`), including `INPUT TABLE` and `INPUT DATASET` (every table of the dataset); a `PUBLIC` or `PROTECTED` class variable, and such a property whose `SET` is automatic and not `PRIVATE`; WebSpeed input (`get-value`, `get-field`, `get-cgi`, `get-user-field`, `get-cookie`, `get-long-value`, and any `WEB-CONTEXT:` member); a member or method of an object whose name has the word `request` or `req` (`poRequest:GetPathParameter(...)`); `IMPORT`, `SET`, `UPDATE` and `MESSAGE ... UPDATE` targets; `COPY-LOB FROM FILE`; `READ-JSON`, `READ-XML` and the `LOAD` of an X-document from a file (a source type of `"FILE"`, or one that is not a literal); a socket `READ` into a `MEMPTR`; `OS-GETENV`; `SESSION:PARAMETER` (or `SESSION:PARAM`); configured `sources`; and database fields when `database-fields = true`.

Sanitisers are specific to a sink kind:

| Code | Clears |
|---|---|
| `QUOTER(x)` | `query` only; not a command, a path or foreign SQL |
| `INTEGER`, `INT64`, `DECIMAL`, `DATE`, `DATETIME` conversions, arithmetic, and functions that return a number (`LENGTH`, `NUM-ENTRIES`, `LOOKUP`, `INDEX`, ...) | every kind but `size`: a number cannot inject text, but it can still be a huge size |
| a comparison or a function that returns a logical | every injection kind; it keeps the client identity of ABLSEC010 |
| `html-encode(x)` | `html` only; it keeps CR and LF, so it does not protect a header |
| a configured validator used as a wrapper, `v(x)` | the kinds it protects |

`url-encode(x)` clears nothing: it does not stop a redirect to another site, and it is not HTML encoding. A project header check goes in `validators` with `protects = ["header"]`.

A guard cleans a variable on the path it holds on: `IF x = "a" OR x = "b"`, `IF LOOKUP(x, "a,b") > 0` (or `= 0 THEN RETURN`), `IF CAN-DO("a,b", x)` with no pattern characters, `IF v(x)` for a validator, combined with `NOT`, `AND` and `OR`; and `CASE x` in a `WHEN` whose values are all literals. A literal has no taint, and assigning one replaces the taint of a variable. An `UNDO` gives a variable that is not `NO-UNDO` back what it held at the start of the undone block, so `c = "ls"` in a block that an error undoes does not hide what `c` held before. A call of code that the file does not hold (`RUN other.p`, a function declared `IN h`, `IN SUPER` or `EXTERNAL`, a `DYNAMIC-FUNCTION` or `DYNAMIC-INVOKE` of another program, a method of an object) returns what its arguments and receiver carry and Unknown, and its `OUTPUT` arguments take the same, so what code elsewhere gives back is never clean. A function that a closed file does not declare cannot be called there, so such a call returns what its arguments carry; in an include file checked on its own it is Unknown too.

A value that the runtime fills, not the program, is Unknown unless the analysis models it: a built-in function on no list of the analysis (`FRAME-VALUE`, `OS-DIR`), a member of `ERROR-STATUS`, `SELF`, `FOCUS`, `LAST-EVENT`, `CLIPBOARD` or `SOURCE-PROCEDURE` that is not a number, a logical or a name (`ERROR-STATUS:GET-MESSAGE(1)`, `SELF:SCREEN-VALUE`), anything read through a handle variable that holds a copy of one or a handle it reaches (`h = SOURCE-PROCEDURE`, `h = SELF:HANDLE`, `h = SELF:PARENT`), the `PRIVATE-DATA` of `SESSION` and `THIS-PROCEDURE`, and a handle either reaches (`SESSION:FIRST-CHILD`), when the file does not set it, a built-in attribute or method of a handle that the analysis does not name (`h:BUFFER-VALUE`, `h:SCREEN-VALUE`, `hNode:NODE-VALUE`), the variable of a `CATCH`, and what a built-in method of a handle writes into an argument (`hNode:NODE-VALUE-TO-LONGCHAR(lc)`, a method that no list names). A node that `GET-DOCUMENT-ELEMENT` or `GET-CHILD` writes holds what its document holds, and so does the text that `SAVE` or `WRITE-JSON` writes; through a handle variable, that text also holds Unknown, since another handle of the same object may have put data into it (`hRoot:APPEND-CHILD(hText)`, `h2 = h1`). A method that puts its arguments into its receiver (`APPEND-CHILD`, `SET-ATTRIBUTE`, `ADD-LAST`) adds them to a receiver that is a handle variable, and a member read through a handle variable that the file copies to or from another is Unknown. A built-in function whose result is made of its arguments (`ENTRY`, `SUBSTRING`, `STRING`) or holds no input (`TODAY`), and a member of a handle that holds what the program set on it (`FILE-INFO:FULL-PATHNAME`, `h:PRIVATE-DATA`) or a setting of the session (`SESSION:TEMP-DIRECTORY`), are not. `docs/design/security-taint.md` ("Built-in reads") has the lists.

A variable that a unit reads and does not declare (a main block variable in a procedure, a class member in a method) and every temp-table or database field holds what any unit of the file writes to it. A main block also sees what an internal procedure, a function or a method writes to its variables after it calls it.

Calls of the file's own procedures, functions and methods are followed: a call returns what the routine returns for these arguments (so `f(pc)` is tainted when `f` returns `pc`, and `f("x")` is not), an `OUTPUT` parameter brings back what the routine set it to, and a `PRIVATE` routine sees what its callers pass. `DYNAMIC-FUNCTION("f", ...)` with the literal name of a function of the file and no `IN` handle calls that function with the arguments after the name; any other `DYNAMIC-FUNCTION`, and `DYNAMIC-INVOKE`, `DYNAMIC-PROPERTY` and `SUPER`, return Unknown. After `RUN p`, `RETURN-VALUE` holds what `p` gave back with `RETURN`, `RETURN ERROR` or the `RETURN` action of a block's `ON` phrase; after a `RUN` of another program, what its arguments carry and Unknown. A `JsonObject` or `JsonArray` (or a variable whose type the file does not declare) takes what `Add`, `Set` and `Insert` give it, and `Write(target)` puts it into `target`, so a request value added to a JSON object and written to the response is an XSS finding.

A taint finding names its path in the message, `` `QUERY-PREPARE` takes a query built from untrusted input: input parameter `pcName` → `cWhere` ``, and each step is a label of the diagnostic, source first. SARIF output turns the steps into `codeFlows`.

When the analysis cannot judge a sink, the finding is reported as in stage 1: the argument holds an unexpanded `{...}` reference or text the preprocessor removed, the parse recovered, the unit costs more than its work budget, or an include that did not arrive sits in the unit (it can set any variable there). So is a sink that a value it cannot judge reaches: a file-level variable or result of a unit past its work budget (a large main block holds most of them); any file-level variable, routine result or parameter when the summaries do not settle in eight rounds (a chain of calls or variables deeper than that); any file-level variable, field or parameter in a file where an include did not arrive, and in a procedure, function or method of an include file checked on its own, because code outside the file can set them first; and, in an include file checked on its own, a file where an include did not arrive, or a class that inherits, a variable that the file does not declare, and a field `t.f` of a table `t` that the file does not define and the schema does not know. A missing include so turns many findings back into stage 1 findings: set `include_paths` so that every include arrives (`check` reports `PREPROC007` for one that does not).

The limits of stage 2 are listed once, in `docs/design/security-taint.md` ("Known limits"); the corpus under `crates/oxabl_lint/tests/security_corpus/taint` has a case for each. In short: taint does not cross files (what `RUN other.p (OUTPUT c)` gives back is Unknown, and the external `INPUT` parameters of that program are sources of their own); a check in a project function is not seen unless `validators` names it; `SHARED` variables and temp-tables are Unknown; and statements that the parser does not model change no variable, except `COPY-LOB`.

##### Entry points and reachability (stage 3)

A finding matters more when a remote caller can reach it. Name the entry points of the application in a JSON file and point `oxabl.toml` at it:

```toml
[workspace.lint.security.reach]
entry-points = "security/entry-points.json"   # relative to oxabl.toml
auth-routines = ["auth/require-role.p", "check-access", "auth/guard.i", "Guard:Require"]
sensitive-calls = ["post-ledger"]             # sensitive besides database writes
raise-reachable = false                       # a reachable finding reports one level up
lower-unreachable = false                     # an unreachable finding reports one level down
```

```json
{
  "version": 1,
  "entries": [
    { "kind": "REST", "name": "GET /orders/{id}", "program": "web/orders.p", "procedure": "get-order" },
    { "kind": "WebSpeed", "name": "/report.w", "program": "web/report.w" },
    { "kind": "REST", "name": "POST /orders", "class": "app.OrderService", "method": "Create" },
    { "kind": "REST", "name": "GET /status", "class": "app.StatusService", "auth": true }
  ]
}
```

- An entry names a `program` (found over the search path as `RUN` finds it), optionally with one of its internal `procedure`s, or a `class`, optionally with one `method`. A class with no method stands for its constructors and every method that is not `PRIVATE`.
- `kind` and `name` are free text; findings name the entry as `kind name`. `auth: true` says that the framework authorises the caller before the entry runs (a PASOE security configuration, an API gateway).
- A file that is missing, or has an unknown key, a wrong `version` or an entry with no target, is a config error of `check` (exit 2). Other clients warn, and turn reachability off: no finding is labelled, and ABLSEC011 and ABLSEC012 are silent.
- An entry whose target is not found is a warning of `check` (`entry point ... its target was not found`). That entry may reach any routine, so a finding that no other entry reaches is not labelled at all, rather than labelled unreachable.

From the entries, the pipeline reads every file they reach once (`RUN p`, `RUN p IN h` for a persistent handle the file sets, internal procedures and functions, methods on `THIS-OBJECT`, `NEW pkg.Cls(...)`, `o:Method(...)` where the file declares `o` as a class, and `pkg.Cls:Method(...)`), and builds the graph of their routines. Includes need no edge: the rules read the expanded text. A call into a `Progress.*`, `OpenEdge.*` or .NET class is no edge. Every other call that does not resolve is an edge to Unknown, with its reason: `dynamic` (`RUN VALUE`, `DYNAMIC-*`), `handle`, `super`, `event` (`PUBLISH`), `method` (an object whose class the file does not declare), `not-found`, `ambiguous`, `no-routine`.

Every finding of the `security` group then has a status:

- **reachable**: `(reachable from REST GET /orders/{id})` is added to the message, with up to four entries; the JSON report and SARIF also carry one call path from the nearest entry.
- **unknown**: no entry reaches the routine through a known call, but a reached routine makes an Unknown call: `(reachability unknown: dynamic call)`. A front controller that runs `RUN VALUE(page)` makes every page it can run unknown; list each page as an entry to make it reachable.
- **unreachable**: no entry reaches it and every reached call is known: `(unreachable from the entry points)`.

`raise-reachable` and `lower-unreachable` move the severity one level (`info` → `warning` → `error`, and back); an unknown or unlabelled finding never moves. Without an entry-point file that loads, nothing is labelled.

Three rules use what stage 3 knows:

- **ABLSEC010 `client-identity`** needs no entry points, but it runs only in taint mode (`mode = "taint"`, or `ABLSEC010 = "taint"` in `[workspace.lint.security.modes]`); in sink mode it is silent, so turning on the `security` group in sink mode adds nothing of it to a baseline. It reports once per routine and identity source: at the first decision that uses the source, with the number of decisions in the message (`...; 12 decisions of this routine use web input `get-value` for `company``) and the others, up to 20, as SARIF `relatedLocations` and as `related` (message, byte span, start and end line and column) in `--json`. A decision added or removed below the first does not move the finding, so its baseline entry stays. A decision is the condition of `IF` (the statement and the expression, `RUN VALUE(IF c = "admin" THEN "purge.p" ELSE "noop.p")`), the selector and the `WHEN` values of `CASE` (`CASE TRUE: WHEN role = "admin"`), the condition of `DO WHILE` and `REPEAT WHILE`, a `WHERE` of `FOR EACH` (every joined phrase too), `FIND`, `CAN-FIND`, `OPEN QUERY` and `PRESELECT`, the key of `FIND Customer cKey`, the `WHILE` of `FOR EACH`, the `WHEN` of an `ASSIGN` pair and of a `DISPLAY` item, the condition of `DELETE ... VALIDATE(condition, message)`, the `WHEN` and `VALIDATE(condition, message)` of a format phrase of `UPDATE`, `SET`, `DISPLAY`, `ENABLE` and `FORM`, an `IF` expression in a statement the parser does not model, and an argument of an `auth-routines` routine. After `RUN p`, a decision on `RETURN-VALUE` decides on what `p` returned, so `RETURN STRING(p = "admin")` passes on the identity of its argument. A value is an identity when the key it is read by has a word of `identity-keys` (`[workspace.lint.security]`; the default list is `user`, `username`, `userid`, `login`, `company`, `tenant`, `role`, `roles`, `owner`, `admin`), as in `get-value("user")`, `get-field("company-id")`, `poRequest:GetHeader("X-User-Id")` and `oBody:GetCharacter("ownerId")` on a body read from the request. `identity-sources` names project functions whose result is the client's claim. A comparison, a number and a guard do not clean an identity, and a logical value made from one holds it (`l = role = "admin". IF l ...`, `IF isAdmin(role)` with a function of the file that returns `p = "admin"`, an `OUTPUT` parameter that a procedure sets that way); a validator with `protects = ["identity"]` does (`"all"` is every injection kind and does not). A routine that the analysis did not finish is not judged, and is not hidden either: it gets one finding per reason, at its first such decision, with the number of such decisions. It says `authorization could not be judged: analysis budget exceeded` when the routine ran past its work budget or reads a value of one that did (check it by hand or split the routine), and `authorization could not be judged: the values of this file did not settle in the analysis` when the summaries of its file did not stop changing in the rounds (check it by hand). A decision on a value that the analysis cannot see (a preprocessor reference that did not expand, a routine where an include did not arrive) says `authorization could not be judged: the analysis cannot see a value it reads`. The identity is followed inside one file: a shared variable, and a parameter of a routine that code outside the file calls, are not an identity (a value read from one by an identity key is).
- **ABLSEC011 `missing-authorization`** reports, once per routine, a database write (`CREATE`, `DELETE`, a field assignment; not a temp-table) or a `sensitive-calls` call that an entry reaches on a path that passes no `auth-routines` check. A check counts when its statement comes before the operation on every path through its routine (it dominates it), or before the call that leads there. An `auth-routines` entry names a procedure, function, method (`method` or `Cls:method`), program (with or without its extension) or include file.
- **ABLSEC012 `cross-request-state`** reports a read of session state (a `NEW GLOBAL SHARED` variable and its `SHARED` consumers; a `STATIC` variable or property, as `name` in its class and `Cls:name` elsewhere) when other reached code writes it and an entry reaches the read on a path that writes it nowhere before.

```toml
[workspace.lint.security]
identity-keys = ["user", "company", "branch"]   # replaces the default list
identity-sources = ["request-user"]
```

Limits of stage 3 (the synthetic web application in `crates/oxabl_pipeline/tests/reach_corpus` has a case for each rule):

- A check is trusted by name and by position: one that does not stop the request when it fails still counts.
- A method call on an object whose class the file does not declare is Unknown, and a call is resolved to the declared class only: an override in a subclass or an interface implementation is not followed.
- Taint does not cross files, and the labels say how a routine is reached, not whether the tainted value came from that entry.
- State held inside a singleton's own instance variables, a persistent procedure's variables and `SESSION` attributes is not session state to ABLSEC012. A static member is named by its class's last name segment, so two classes of that name in different packages share their statics.
- `oxabl check` builds the graph once per run. The editor and the daemon do not build it yet, so there they show the findings with no reachability, and ABLSEC011 and ABLSEC012 are silent. On a generated project of 30 000 files the graph takes about 2.5 seconds to build, and a lint of every file with entry points takes about twice as long as one without.

Limits:

- ABLSEC004 reports a subset of what `run-value` (CL001) and `dynamic-invoke` (CL002) report. Those rules are about analysis: a callee that is not in the text hides from tools, including `RUN VALUE("a" + ".p")`. ABLSEC004 is about injection and reports only a name that is not a literal. With both groups on, `RUN VALUE(c)` has two findings, and a fix to the name clears both.
- Rules read the text after preprocessing. A sink in an include file is reported once, in the include file, like a strict finding; the include-file notes above apply. A sink written in your own file whose argument is `{&NAME}` is reported at the sink, and is a literal only when `NAME` expands to one. A sink whose whole text is the body of a `&GLOBAL-DEFINE` or `&SCOPED-DEFINE` is not reported by `check`: the expansion does not belong to the file that uses it.
- `OS-COMMAND`, `UNIX`, `DOS`, `OS-DELETE`, `OS-COPY`, `OS-RENAME`, `OS-CREATE-DIR`, `COPY-LOB` and `COMPILE` are not modelled by the parser. The rules read their text, and look only at the operands listed above.
- Method names such as `FIND-FIRST` and `CommandText` are matched by spelling, because the rules have no type information.
- Embedded SQL (`EXEC SQL`) and a command or path set through a class the table does not list are not seen. In stage 1, a value that is built in one procedure and used in another is a finding only where it is used.
- ABLSEC008 is the one rule that reports a literal. A finding never repeats the secret: the message shows `"****"` and `oech1::****`. The output shows the source line of a finding as it does for any other rule, so the text report, a SARIF log and the hash of a baseline entry or SARIF fingerprint (which covers the flagged line) can hold the literal. Masking it in the output is not done yet (issue #488). A name is password-like when one of its words is `password`, `pwd`, `passphrase`, `secret`, `apikey` or one of a few more; a name that also holds a word such as `label`, `prompt`, `href`, `hide`, `show`, `display`, `style`, `css` or `id` is not (`cForgotPasswordHref`, `cHideChngPwd`). A literal that is text about a password is not a secret: markup (`'<input type="password">'`, `&lt;`), a template (`SUBSTITUTE`'s `&1`, `%s`, `{{name}}`, `${name}`), a label or prompt (ending in `:`, or words with a space that include a password word), or a link (`javascript:...`, or an `http(s)://` URL with no `user:password@` and no password-named query parameter). A lone `<`, `&` or `{` in a value does not count, and the word `"password"` alone is still reported. The lists are in `oxabl_lint::security::names::Heuristics`, and a program that embeds `oxabl_lint` passes its own to `run_with`; `oxabl.toml` does not set them yet.
- ABLSEC009 and ABLSEC013 read names with the same lists. They are heuristics: a hash of a password in a variable with a neutral name is not seen, and a digest of a value named `cTokenCacheKey` is reported.
- ABLSEC013 follows error text only inside one statement, to an `OUTPUT` parameter of the file or to a web response. A detail kept in a local variable and returned later is not seen.
- ABLSEC006 and ABLSEC013 take a web response from the name of the object (`oResp`, `oResponse`, `oWriter`) and from `{&OUT}`, which the web include defines as `PUT STREAM WebStream UNFORMATTED`. When the include that defines `{&OUT}` is not on the include path, the preprocessor expands the name to nothing and the write is not seen.
- A class name that comes from data (`Progress.Lang.Class:GetClass`, `DYNAMIC-NEW`) is reported by ABLSEC004 only. ABLSEC014 reports `Deserialize`, so each call has one owner.
- ABLSEC015 reports every `SAX-PARSE` and `SAX-PARSE-FIRST`, because the input is set by an earlier call that the rule does not follow. It is a prompt for review.
- ABLSEC016 treats one `BATCH-SIZE` assignment (other than `0`) anywhere in the file as the cap of every `FILL` in it. The parser drops a statement with a `READ-AVAILABLE` or `READ-EXACT-NUM` argument, so socket reads are found by lexing the file for `:READ(`.
- ABLSEC017 does not know where a program lives. List the programs where `DISABLE TRIGGERS` and `_User` writes are expected in an override: `[[workspace.lint.overrides]]` with `files = ["utils/load/**"]` and `ABLSEC017 = "off"`.

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

- A finding matches an entry by rule code, workspace-relative path, a hash of the flagged line and span, and an occurrence number for identical findings. Whitespace runs are collapsed and letters are folded to lower case before hashing, so a change of keyword case does not change the match. The file has no line numbers, so edits that move a finding to a different line do not change the match. Only the first line of the flagged span is hashed: a change on a later line of a multi-line statement keeps the match. The exact algorithm is in the documentation of `crates/oxabl/src/baseline.rs`.
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

#### Suppressing a finding

Put an `oxabl-ignore` comment on the line before a finding, or at the end of its line. The comment names one rule and must give a reason:

```abl
/* oxabl-ignore LINT0002: the report layout reads this by name */
DEFINE VARIABLE v-layout AS CHARACTER NO-UNDO.

RUN VALUE(c). // oxabl-ignore CL001: c comes from a fixed list
```

- A comment that is alone on its line covers the next line (the line after its last line, for a block comment). A comment after code covers its own line. `/* */` and `//` comments both work. A comment inside a string is not a comment.
- The rule can be its code or its name. Only findings of that rule on that line are suppressed.
- A comment with no reason, an unknown rule, or no rule does not suppress anything. `check` reports it as `invalid-suppression` (LINT0007, warning).
- A valid comment that covers no finding is reported as `unused-suppression` (LINT0008, info). It is not reported when its rule is off for the file, so a suppression of a `strict` rule does not warn in a tree that does not enable the group.
- Both are rules in the `default` group, so a severity in `[workspace.lint]` applies to them (`LINT0008 = "off"`). They are on without any configuration: a tree that already holds an `oxabl-ignore` comment with no rule or no reason gets a new LINT0007 warning, and `check` exits 1 on it. Fix the comment, or set `LINT0007 = "off"`.
- Suppressed findings do not fail the gate and are not recorded in a baseline. `check` counts them on stderr; `--json` has a `suppressed` key; SARIF keeps them (see below).
- Suppressions apply to `oxabl check`. The language server does not read them yet.

#### SARIF output

`oxabl check --format sarif` writes one [SARIF 2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/) log to stdout, for code review and code scanning tools. `--format` takes `text` (the default), `json` (the same as `--json`) and `sarif`. `--json` and `--format` cannot be used together.

- `tool.driver` has the name, version and `informationUri`. `rules` lists every rule of `oxabl_lint::Registry`: `id` (the code), `name`, `shortDescription`, `help`, `defaultConfiguration.level`, and `properties.tags` (the group, and `preview` for a preview rule). A rule that declares a security severity (`RuleMeta::with_security_severity`) also has `properties.security-severity`.
- Each result has `ruleId`, `level` (`error`, `warning` or `note` for `info` and `hint`), `message`, a `physicalLocation` with a workspace-relative URI and a 1-based region (columns count UTF-16 code units), and `partialFingerprints`. A finding in an include file has no region.
- Every URI is relative to `%SRCROOT%`, which `run.originalUriBaseIds` gives as the `file:` URI of the workspace root.
- The `oxablFinding/v1` fingerprint is a hash of the rule, the workspace-relative path, the baseline hash of the finding (the normalised flagged line and the first line of the span), and an occurrence number. It has no line number, so it does not change when the finding moves.
- With `--baseline`, a result that the baseline does not cover has `baselineState: "new"`, and one that it covers has `"unchanged"`. Without `--baseline`, results have no `baselineState`.
- A finding that an `oxabl-ignore` comment suppresses stays in `results` with `suppressions: [{ "kind": "inSource", "justification": "<reason>" }]`.
- A security finding of taint mode has `codeFlows` with one `threadFlow`: each step of the path in the checked file, source first, with its label as the message, and the sink last. A finding in an include file has no `codeFlows`.
- With an entry-point file, a security finding has `properties.reachability` (`status`, `entries` with `label` and `target`, `moreEntries`, `unknownCalls`, and `path` with its `entry` and `calls`) and a `relatedLocations` entry for each call on that path, by file and line.
- A failed file is a `toolExecutionNotifications` entry of level `error`. Format drift, a configuration warning and an entry point whose target was not found are entries of level `warning`, so a consumer sees when reachability labels are degraded.
- The exit code does not change with the format. A suppressed or baselined finding does not fail the run.

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
- `Self-update` — the self-update tests, and clippy with the `remote-cache` and `self-update` features
- `Install script` — `scripts/install.sh` under `shellcheck`, dash and bash, against a fake release server

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
4. The release of the `oxabl` crate also gets a static Linux binary with the remote cache (`scripts/build-musl.sh`) and its `.sha256` file

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
