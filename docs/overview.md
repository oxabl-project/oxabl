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
  - Status: a first set of rules ships today — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002), `unknown-table-or-field` (LINT0003, live under a loaded `.df` schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005), `assigned-but-never-read` (LINT0006), and the suppression rules `invalid-suppression` (LINT0007) and `unused-suppression` (LINT0008) — configurable per-project via `oxabl.toml`. LINT0002 and LINT0006 divide one population between them: a variable never referenced at all is LINT0002's, while one that is written and never read is a dead store reported by LINT0006 at the assignment, so silencing `unused-variable` alone no longer silences the write-only half. Around thirty ABL statement forms are recognized by the parser but not modelled (`PUT`, `EXPORT`, `UPDATE`, `ENABLE`, embedded SQL, …); their identifiers are now harvested lexically and best-effort-resolved, so all three count-gated rules stay quiet about a variable one of them touches rather than reporting it wrongly. That suppression is coarse — per-symbol and file-wide — so `oxabl analyze` reports how many symbols it could not fully judge. The handful of forms that name a *table* without reading a field of it (`DEFINE BUFFER`, `DEFINE PARAMETER BUFFER`, `EMPTY TEMP-TABLE`, `DEFINE QUERY`, `OPEN QUERY`) are treated more precisely: they credit a real read on the table, so a temp-table used only that way no longer looks untouched. Surfaced in-editor through the VS Code extension and as diagnostics from `oxabl check`. Still experimental. `oxabl_lint::Registry` lists the rules with their metadata (code, name, group, description, default severity, stability), accepts your own rules, and runs a subset; `lint_file` stays the run-everything entry point. See [Lint configuration](#lint-configuration) for rule groups, per-directory settings and baselines.
  - **An `.i` opened directly is analyzed as a fragment.** Names its missing includer could supply stay silent, as do the three whole-unit count rules, while parse errors and locally provable findings still surface. The CLI and analyze coverage channels say that the includer was absent; an `.i` expanded through a `.p`/`.w`/`.cls`/`.v` root remains ordinary textual input to that complete compilation unit.
  - **`undefined-symbol` now reports names absent from your configured search paths.** With a workspace index attached — which is every `oxabl check` run — a `USING` import, a `NEW`, or a literal `RUN` target that no configured path supplies is reported at error severity. ABL cannot reference a symbol or a procedure whose code is not on the PROPATH, so the name genuinely is undefined rather than merely unseen. Set your paths in `[workspace.sources].include_paths` in `oxabl.toml` (relative to that file, searched in order, first match wins), or pass directories with `-I`; every such finding carries a help line naming that configuration, because a missing source root produces the same finding as missing code. Four things stay deliberately silent: names in the `Progress.*`, `OpenEdge.*`, `System.*`, and `Microsoft.*` namespaces, which ship with the AVM and have no source on any path; a runtime-computed `RUN VALUE(...)` target, which no indexing could resolve; a member a class declares but does not expose to the caller, which exists and so is an access question rather than a missing name; and every cross-file name at all when no path is configured, since "we did not look" is then the only truthful answer. The `AS CLASS pkg.Missing` declaration spelling is also silent for now — the AST carries no span for that name, so there is nothing to underline.
  - **The preprocessor answers the AVM's predefined names from `[workspace.preprocessor]` in `oxabl.toml`.** The AVM reads `{&OPSYS}`, `{&WINDOW-SYSTEM}`, `{&BATCH-MODE}`, `{&PROCESS-ARCHITECTURE}` and `{&PROVERSION}` from the machine that compiles. oxabl has no such machine, so you state the environment and every `&IF` over these names decides against it. The defaults describe a 64-bit Unix batch compile with a character UI: `opsys = "UNIX"`, `window-system = "TTY"`, `batch-mode = true` (expands to `yes`), `process-architecture = 64`. You can override each key (for example `opsys = "WIN32"`, `window-system = "MS-WINDOWS"`, `batch-mode = false`). `proversion` has no default because a tool cannot guess it: set it (for example `proversion = "12.8"`), or an `&IF` condition that reads `{&PROVERSION}` or the `PROVERSION` function raises the always-loud warning `PREPROC009` instead of silently deciding against an empty string. The `OPSYS` and `PROVERSION` functions in an `&IF` read the same table. `databases` lists the logical names of the databases connected when the code compiles, in the order of connection (for example `databases = ["ledger", "stock"]`), and the `DBTYPE`, `CONNECTED` and `LDBNAME` functions of an `&IF` answer from it. The list is complete: a database that is not in it is not connected. Use `[]` for a compile with no database. `DICTDB`, the alias that the AVM gives the first connected database, answers as the first name in the list. Without the key, each `[workspace.schema]` file names one database: the file name without `.df` is the logical name, so `schema/ledger.df` connects `ledger`. A file name is not always the logical name (a versioned dump file, for example), and the files are in the order of `[workspace.schema]`, not of connection. So this list is not complete and not ordered: a name that is not in it is a guess (`PREPROC010`), not an absent database, and so are `LDBNAME("DICTDB")` and `DBTYPE("DICTDB")`. `CONNECTED("DICTDB")` is true when the list is not empty. Set the key when a logical name differs from its file name. `DBTYPE("ledger")` answers `PROGRESS` for a listed database, or the type that `database-types` gives (`database-types = { stock = "ORACLE" }`), and `?` for one that a stated list does not hold. `CONNECTED("ledger")` is true for a listed one, and false for one that a stated list does not hold. `LDBNAME("ledger")` answers the name as listed, or `?` for one that a stated list does not hold. Only the logical-name form is modelled: `LDBNAME(1)` and `DBTYPE(1)` depend on the order of connection, so they are a guess. With neither a list nor a schema, every call to one of these functions is a guess too. `{&FILE-NAME}` (the name the file was analysed under, or the include name as written), `{&LINE-NUMBER}` (the line in the current file) and `{&SEQUENCE}` (0, 1, 2, … per reference, shared by the whole compilation unit) are computed from the source and need no configuration. An `&IF` condition is evaluated for the operators and functions whose result oxabl can compute exactly: comparisons (two strings compare as text, two numbers as numbers, decimals exactly; a string against a number is a guess), `BEGINS`, `MATCHES`, `AND`, `OR`, `NOT`, the unknown value `?` (equal only to itself; every other comparison with it is unknown, and `NOT`, `AND` and `OR` follow the three-valued rules, and an unknown condition is false, so `&ELSE` or `&ELSEIF` is taken), integer `+ - * MODULO`, `DEFINED` (0 undefined, 1 `&GLOBAL-DEFINE`, 2 include argument, 3 `&SCOPED-DEFINE`), `OPSYS`, `PROVERSION`, `DBTYPE`, `CONNECTED`, `LDBNAME`, `LOGICAL`, `INTEGER`, `DECIMAL` (whole values), `STRING`, `SUBSTRING`, `LENGTH`, `TRIM`, `LEFT-TRIM`, `RIGHT-TRIM`, `LC`, `INDEX`, `R-INDEX`, `ENTRY`, `NUM-ENTRIES`, `LOOKUP`, `ABSOLUTE`, `MAXIMUM` and `MINIMUM` (all numbers or all strings). `DEFINED` gives the kind of the newest definition of the name, as the AVM does. A scoped define or an include argument hides an older definition until it ends or is undefined. A global define of a name that has a scoped define or an include argument is an AVM error and is ignored. An `&UNDEFINE` in an include removes the visible definition wherever it was made. A condition with nothing in it (`&IF {&undefined} &THEN`) is false, as in the AVM. Any other name, function or shape (`SEARCH`, `KEYWORD`, `/`, an empty operand inside an expression, …) raises the always-loud warning `PREPROC010`, and the file's edge set is `Degraded` with reason `preprocessor_loss`: the branch is a guess, so the includes inside it may or may not be dependencies. `SEARCH(...)` asks whether a file exists, which no evaluator can know, so it is always such a guess. When an operand settles an `AND` (false) or an `OR` (true) without a guess, the guesses in the other operands of that chain, before it or after it, cannot change the result and raise nothing. A syntax error, or a comparison of a string with a number, is different: the AVM rejects the condition whatever the other operands are, so it always raises `PREPROC010`. An include reference with whitespace inside the braces (`{ name.i }`) expands like `{name.i}`; one whose brace never closes raises `PREPROC008` and is reported as an unresolved include.
  - **Case-insensitive lookup is opt-in.** A class or `RUN` target is found by its name as written, then by its lower-case fold. Set `case_insensitive_lookup = true` under `[workspace.sources]` to also match a file whose path differs in case from both (`pkg.UTIL` for `pkg/Util.cls`). It lists directories on a miss, so it costs more. Two distinct matches leave the name unresolved. A custom `FileSystem` supplies its own matching by overriding `find_ignoring_case`.
  - **Each root can search its own directory first.** Some shops compile every program with its own directory, and fixed subdirectories of it, first on the PROPATH. Set `root_relative_include_paths = [".", "rf"]` under `[workspace.sources]` and each root file searches `<its dir>` and `<its dir>/rf` before `include_paths`, for includes, classes and `RUN` targets alike. A class or program found this way reads its own includes with the paths of its own directory. One `oxabl check` run handles roots in any number of directories. The default is empty.
  - **The files a directory walk picks up are set by `[workspace.sources].root_extensions`.** The default is `["p", "w", "cls", "v"]`, matched without regard to case. Use it for shop conventions, for example `root_extensions = ["p", "w", "cls", "v", "pp"]`. The list replaces the default, so keep the entries you need. `i` is rejected with a warning, because an include file is never a root. A file you name on the command line is always checked.
  - **Source files are UTF-8 unless `[workspace.sources].encoding` says otherwise.** Many ABL trees are stored in a single-byte code page. Set `encoding = "windows-1252"` or `encoding = "iso-8859-1"` (the default is `"utf-8"`), and every file that oxabl reads from disk is decoded that way: roots, include files, classes and `.df` schema files, in `oxabl check`, `analyze`, `format`, the language server and the daemon. Offsets and columns in a report count the decoded text. A UTF-8 byte order mark always means UTF-8, is removed whatever the setting says, and `oxabl format` writes it back. `oxabl format` also writes a file back in its own encoding. With `"utf-8"`, a file that is not valid UTF-8 cannot be read, and `check` reports it as a failure that names the setting. For a tree that mixes UTF-8 files and single-byte ones, keep `encoding = "utf-8"` and set `fallback_encoding = "windows-1252"`: a file that is not valid UTF-8 is then decoded with the fallback. A single-byte encoding decodes every file, so the fallback never applies to it. Under `windows-1252`, the five bytes the code page leaves undefined (0x81, 0x8D, 0x8F, 0x90, 0x9D) decode to the C1 control of the same value. A custom `FileSystem` decodes for itself; `oxabl_workspace::EncodedFileSystem` is the disk filesystem that follows the setting, and `RealFileSystem` stays UTF-8.
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
  - `oxabl explain <file>[:line]` — show each unit's direct database effects and transitive summary. Add `--json` for the version 1 explain document. Use `-I` and `--schema` to override project paths and schema. Preprocessing is on. A line selects the innermost unit at that root line; printed lines refer to expanded text. Partial summaries list their reasons. Index choices remain estimates until the XREF gate passes. Exit codes: 0 for a report, 2 for a bad line or unreadable file, 3 for fatal preprocessing failure, 4 for an internal panic, 6 for serialization failure.
  - `oxabl analyze <file>` — a single file's resolved semantic model, dumped as a per-section-versioned JSON envelope or as text (`--schema <file|dir>` for schema-backed resolution). Introspection, not a gate: it exits 0 whatever it finds. Eight sections are emitted — `scopes`, `symbols`, `types`, `references`, `diagnostics`, `preproc`, `coverage`, and `dependencies` — each carrying its own version under `sections`, so a change to one leaves the others' numbers alone. `dependencies` is where cross-file resolution is observable: which files the run's workspace index actually consulted, and which cross-file names came back empty and why.
- `oxabl_wasm`: browser bindings in `crates/oxabl_wasm`.
  - Three `wasm-bindgen` exports — `analyze_source`, `format_source`, and `version()` (a crate version plus a build identifier, so a crash report names the exact artifact a hand-vendored copy is running) — the first two returning JSON: diagnostics carry source/severity/code/message, byte offsets, and line/column positions; a format result carries the new source, a `changed` flag, and an `error` string when the formatter declines to format (in which case the original source comes back untouched). Line and column come from the shared position helper the CLI's text output uses, so the two cannot drift.
  - Contains no ABL behavior. It is a transport adapter over the shared pipelines, which keeps every client on one implementation. A refusal collapses to one `error` string here because the wire shape has one field for it, while the pipeline keeps a deliberate bail and a contained panic apart for the clients that can tell them apart.
  - CLI-only dependencies live behind the `oxabl` crate's default-on `cli` feature so the library compiles for `wasm32`; a CI job builds the wasm target on every push.

Current Work: continued dogfood-driven trust-hardening, and typing `:`-qualified member and method-call expressions — the half of the cross-file population the type lattice still does not judge. Shipped: the semantic layer, the layout formatter (CLI + LSP), the language server, the VS Code extension, the curated `oxabl` public API, the browser WASM playground, the shared lint/format pipelines every client now drives, and cross-file/workspace resolution — inheritance chains, `USING` imports, literal `RUN` targets, and `SHARED` producers now link across files, with the CLI and the editor reaching identical answers from the same shared index. (The browser playground stays single-file: its exports take one source and no sibling files, so there is nothing for an index to look at.)

### Lint configuration

Lint rules are in groups. The `LINT` rules are in the `default` group, which is always on. The `strict.transactions`, `strict.locks`, `strict.access`, `strict.handles`, `strict.nulls`, `strict.calls` and `security` groups are off until you enable them. The plan for all strict rules is in `docs/design/strict-analysis-plan.md`. These strict rules are available now. They are in preview and report at `warn`:

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

#### Security rules

The `security` group finds code where a value that is not a literal reaches a dangerous call. Turn it on with `enable = ["security"]`; `enable = ["strict"]` does not select it. The rules are in preview. By default they are sink rules (stage 1): each one reports every call whose argument is not a literal, and does not follow the value back to its source. They are complete and noisy on purpose. Use a baseline for the code that exists, and see `crates/oxabl_lint/src/rules/security/mod.rs` for the design. Ten of the rules also have a taint mode (stage 2, below) that reports a sink only when untrusted input reaches it. With an entry-point file (stage 3, below), every finding says how a remote caller reaches it, and three more rules read the call graph.

| Code | Name | Reports | Severity |
|---|---|---|---|
| ABLSEC001 | `dynamic-query` | `QUERY-PREPARE`, `FIND-FIRST`, `FIND-LAST` and `FIND-UNIQUE` on a buffer, and `FILL-WHERE-STRING`, given a query string that is not a literal | `error` |
| ABLSEC002 | `foreign-sql` | `RUN STORED-PROC send-sql-statement`, `NEW SqlCommand(...)` (and the ODBC, OLE DB, Oracle, MySQL and Npgsql classes and data adapters) and `CommandText` set from a string that is not a literal | `error` |
| ABLSEC003 | `os-command` | `OS-COMMAND`, `UNIX`, `DOS` and `INPUT`, `OUTPUT` or `INPUT-OUTPUT THROUGH` with a `VALUE(...)` that is not a literal | `error` |
| ABLSEC004 | `dynamic-code` | `RUN VALUE`, `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`, `Progress.Lang.Class:GetClass`, `COMPILE VALUE` and `CALL-NAME` with a name that is not a literal | `error` |
| ABLSEC005 | `path-traversal` | `INPUT FROM` and `OUTPUT TO` with `VALUE(...)` or `OS-DIR(...)`, `OS-DELETE`, `OS-COPY`, `OS-RENAME`, `OS-CREATE-DIR`, `COPY-LOB ... FILE` and `FILE-INFO:FILE-NAME` with a path that is not a literal | `warning` |
| ABLSEC006 | `xss-output` | `{&OUT}`, `PUT STREAM WebStream`, `Write` on a response writer and `Entity` of a response given a value that is not a literal | `warning` |
| ABLSEC007 | `header-injection` | `SetHeader`, `AddHeader`, `SetCookie`, `Location`, `HttpHeader` and `Cookie` objects, `HttpHeaderBuilder`, and WebSpeed `output-http-header` and `set-cookie` given a value that is not a literal | `warning` |
| ABLSEC008 | `hardcoded-secret` | `CONNECT` with a literal `-P`, an `oech1::` string, `ENCRYPT`, `DECRYPT` and `GENERATE-PBE-KEY` with a literal key, and a literal assigned to a password-like name | `error` |
| ABLSEC009 | `weak-crypto` | `ENCODE`, `MD5-DIGEST`, `SHA1-DIGEST` and `MESSAGE-DIGEST("MD5")` of a password-like or token-like name, and `RANDOM` that feeds a token, nonce or identifier | `warning` |
| ABLSEC010 | `client-identity` | an `IF`, `CASE`, `WHILE` or `WHERE` that decides on a user, company or role that the client sent (`get-value("role")`, `poRequest:GetPathParameter("tenant")`), and such a value passed to an `auth-routines` routine | `warning` |
| ABLSEC011 | `missing-authorization` | a database write or a `sensitive-calls` call that an entry point reaches on a path with no `auth-routines` check (needs entry points) | `warning` |
| ABLSEC012 | `cross-request-state` | a read of `NEW GLOBAL SHARED` or `STATIC` state that a request makes before it sets it, where other reached code writes it (needs entry points) | `warning` |
| ABLSEC013 | `information-leak` | `ERROR-STATUS:GET-MESSAGE`, `GetMessage` and `CallStack` of a caught error assigned to an output parameter or written to a web response, and a password-like name passed to `MESSAGE` or `LOG-MANAGER:WRITE-MESSAGE` | `warning` |
| ABLSEC014 | `unsafe-deserialization` | `Deserialize` of a value that is not a literal | `warning` |
| ABLSEC015 | `untrusted-xml` | `X-DOCUMENT:LOAD` of a source that is not a literal, and `SAX-PARSE`, `SAX-PARSE-FIRST` | `info` |
| ABLSEC016 | `resource-exhaustion` | `SET-SIZE(m) = expr`, a socket `READ` with a byte count that is not a literal and has no cap, and `FILL` with no `BATCH-SIZE` in the file | `info` |
| ABLSEC017 | `integrity-bypass` | `DISABLE TRIGGERS`, and `CREATE`, `DELETE` and field assignments on `_User` | `warning` |

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

Sources: the `INPUT` and `INPUT-OUTPUT` parameters of a unit that code outside the file can call (the main block, a `PROCEDURE` or `FUNCTION` that is not `PRIVATE`, a `METHOD`, `CONSTRUCTOR` or property setter that is not `PRIVATE`), including `INPUT TABLE`; WebSpeed input (`get-value`, `get-field`, `get-cgi`, `get-user-field`, `get-cookie`, `get-long-value`, and any `WEB-CONTEXT:` member); a member or method of an object whose name has the word `request` or `req` (`poRequest:GetPathParameter(...)`); `IMPORT`, `SET`, `UPDATE` and `MESSAGE ... UPDATE` targets; `COPY-LOB FROM FILE`; a socket `READ` into a `MEMPTR`; `OS-GETENV`; `SESSION:PARAMETER`; configured `sources`; and database fields when `database-fields = true`.

Sanitisers are specific to a sink kind:

| Code | Clears |
|---|---|
| `QUOTER(x)` | `query` only; not a command, a path or foreign SQL |
| `INTEGER`, `INT64`, `DECIMAL`, `DATE`, `DATETIME` conversions, arithmetic, and functions that return a number (`LENGTH`, `NUM-ENTRIES`, `LOOKUP`, `INDEX`, ...) | every kind but `size`: a number cannot inject text, but it can still be a huge size |
| a comparison or a function that returns a logical | every kind |
| `html-encode(x)`, `url-encode(x)` | `html`, `header` |
| a configured validator used as a wrapper, `v(x)` | the kinds it protects |

A guard cleans a variable on the path it holds on: `IF x = "a" OR x = "b"`, `IF LOOKUP(x, "a,b") > 0` (or `= 0 THEN RETURN`), `IF CAN-DO("a,b", x)` with no pattern characters, `IF v(x)` for a validator, combined with `NOT`, `AND` and `OR`; and `CASE x` in a `WHEN` whose values are all literals. A literal has no taint, and assigning one replaces the taint of a variable. An unknown call (a user function, a method, `RUN`, `NEW`) returns what its arguments and receiver carry, and its `OUTPUT` arguments take the same.

A variable that a unit reads and does not declare (a main block variable in a procedure, a class member in a method) and every temp-table or database field holds what any unit of the file writes to it. A main block also sees what an internal procedure writes to its variables after it runs that procedure.

Calls of the file's own procedures, functions and methods are followed: a call returns what the routine returns for these arguments (so `f(pc)` is tainted when `f` returns `pc`, and `f("x")` is not), an `OUTPUT` parameter brings back what the routine set it to, and a `PRIVATE` routine sees what its callers pass. A `JsonObject` or `JsonArray` (or a variable whose type the file does not declare) takes what `Add`, `Set` and `Insert` give it, and `Write(target)` puts it into `target`, so a request value added to a JSON object and written to the response is an XSS finding.

A taint finding names its path in the message, `` `QUERY-PREPARE` takes a query built from untrusted input: input parameter `pcName` → `cWhere` ``, and each step is a label of the diagnostic, source first. SARIF output turns the steps into `codeFlows`.

When the analysis cannot judge a sink, the finding is reported as in stage 1: the argument holds an unexpanded `{...}` reference or text the preprocessor removed, the parse recovered, or the unit costs more than its work budget.

Limits of stage 2 (the corpus under `crates/oxabl_lint/tests/security_corpus/taint` has a case for each):

- Taint does not cross files: what a program of another file returns (`RUN other.p (OUTPUT c)`) is what an unknown call returns. The external `INPUT` parameters of that program are sources of their own.
- A check in a project function is not seen unless `validators` names it, so the sink after it is still reported. An allow-list held in a variable is not a literal list.
- A main block does not see what a function or a method writes to its variables, only what a `RUN` of an internal procedure writes. `SHARED` variables are clean.
- Statements that the parser does not model change no variable, except `COPY-LOB`.
- A validator is trusted by name; the analysis does not check it.

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
- An unknown key, a wrong `version` or an entry with no target makes the file a config warning, and no entry is read. An entry whose target is not found is a warning of `check` (`entry point ... its target was not found`).

From the entries, the pipeline reads every file they reach once (`RUN p`, `RUN p IN h` for a persistent handle the file sets, internal procedures and functions, methods on `THIS-OBJECT`, `NEW pkg.Cls(...)`, `o:Method(...)` where the file declares `o` as a class, and `pkg.Cls:Method(...)`), and builds the graph of their routines. Includes need no edge: the rules read the expanded text. A call into a `Progress.*`, `OpenEdge.*` or .NET class is no edge. Every other call that does not resolve is an edge to Unknown, with its reason: `dynamic` (`RUN VALUE`, `DYNAMIC-*`), `handle`, `super`, `event` (`PUBLISH`), `method` (an object whose class the file does not declare), `not-found`, `ambiguous`, `no-routine`.

Every finding of the `security` group then has a status:

- **reachable**: `(reachable from REST GET /orders/{id})` is added to the message, with up to four entries; the JSON report and SARIF also carry one call path from the nearest entry.
- **unknown**: no entry reaches the routine through a known call, but a reached routine makes an Unknown call: `(reachability unknown: dynamic call)`. A front controller that runs `RUN VALUE(page)` makes every page it can run unknown; list each page as an entry to make it reachable.
- **unreachable**: no entry reaches it and every reached call is known: `(unreachable from the entry points)`.

`raise-reachable` and `lower-unreachable` move the severity one level (`info` → `warning` → `error`, and back); an unknown finding never moves. Without an entry-point file nothing is labelled.

Three rules use what stage 3 knows:

- **ABLSEC010 `client-identity`** needs no entry points, but it runs only in taint mode (`mode = "taint"`, or `ABLSEC010 = "taint"` in `[workspace.lint.security.modes]`); in sink mode it is silent, so turning on the `security` group in sink mode adds nothing of it to a baseline. It reports once per routine and identity source: at the first decision that uses the source, with the number of decisions in the message (`...; 12 decisions of this routine use web input `get-value` for `company``) and the others, up to 20, as SARIF `relatedLocations` and as `related` (message, byte span, start and end line and column) in `--json`. A decision added or removed below the first does not move the finding, so its baseline entry stays. A value is an identity when the key it is read by has a word of `identity-keys` (`[workspace.lint.security]`; the default list is `user`, `username`, `userid`, `login`, `company`, `tenant`, `role`, `roles`, `owner`, `admin`), as in `get-value("user")`, `get-field("company-id")`, `poRequest:GetHeader("X-User-Id")` and `oBody:GetCharacter("ownerId")` on a body read from the request. `identity-sources` names project functions whose result is the client's claim. A comparison, a number and a guard do not clean an identity; a validator with `protects = ["identity"]` does (`"all"` is every injection kind and does not).
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
- ABLSEC008 is the one rule that reports a literal. A finding never repeats the secret: the message shows `"****"` and `oech1::****`. The text output of `oxabl check` still prints the source line of a finding, as it does for every rule, so a secret is visible there; the JSON and SARIF reports hold the message only. A name is password-like when one of its words is `password`, `pwd`, `passphrase`, `secret`, `apikey` or one of a few more; a name that also holds a word such as `label`, `prompt`, `href`, `hide`, `show`, `display`, `style`, `css` or `id` is not (`cForgotPasswordHref`, `cHideChngPwd`). A literal that is text about a password is not a secret: markup (`'<input type="password">'`, `&lt;`), a template (`SUBSTITUTE`'s `&1`, `%s`, `{{name}}`, `${name}`), a label or prompt (ending in `:`, or words with a space that include a password word), or a link (`javascript:...`, or an `http(s)://` URL with no `user:password@` and no password-named query parameter). A lone `<`, `&` or `{` in a value does not count, and the word `"password"` alone is still reported. The lists are in `oxabl_lint::security::names::Heuristics`, and a program that embeds `oxabl_lint` passes its own to `run_with`; `oxabl.toml` does not set them yet.
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
- Both are rules in the `default` group, so a severity in `[workspace.lint]` applies to them (`LINT0008 = "off"`).
- Suppressed findings do not fail the gate and are not recorded in a baseline. `check` counts them on stderr; `--json` has a `suppressed` key; SARIF keeps them (see below).
- Suppressions apply to `oxabl check`. The language server does not read them yet.

#### SARIF output

`oxabl check --format sarif` writes one [SARIF 2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/) log to stdout, for code review and code scanning tools. `--format` takes `text` (the default), `json` (the same as `--json`) and `sarif`. `--json` and `--format` cannot be used together.

- `tool.driver` has the name, version and `informationUri`. `rules` lists every rule of `oxabl_lint::Registry`: `id` (the code), `name`, `shortDescription`, `help`, `defaultConfiguration.level`, and `properties.tags` (the group, and `preview` for a preview rule). A rule that declares a security severity (`RuleMeta::with_security_severity`) also has `properties.security-severity`.
- Each result has `ruleId`, `level` (`error`, `warning` or `note` for `info` and `hint`), `message`, a `physicalLocation` with a workspace-relative URI and a 1-based region (columns count UTF-16 code units), and `partialFingerprints`. A finding in an include file has no region.
- The `oxablFinding/v1` fingerprint is a hash of the rule, the workspace-relative path, the normalised text of the flagged line and span, and an occurrence number. It has no line number, so it does not change when the finding moves.
- With `--baseline`, a result that the baseline does not cover has `baselineState: "new"`, and one that it covers has `"unchanged"`. Without `--baseline`, results have no `baselineState`.
- A finding that an `oxabl-ignore` comment suppresses stays in `results` with `suppressions: [{ "kind": "inSource", "justification": "<reason>" }]`.
- A security finding of taint mode has `codeFlows` with one `threadFlow`: each step of the path in the checked file, source first, with its label as the message, and the sink last. A finding in an include file has no `codeFlows`.
- With an entry-point file, a security finding has `properties.reachability` (`status`, `entries` with `label` and `target`, `moreEntries`, `unknownCalls`, and `path` with its `entry` and `calls`) and a `relatedLocations` entry for each call on that path, by file and line.
- A failed file is a `toolExecutionNotifications` entry of level `error`; format drift is one of level `warning`.
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
