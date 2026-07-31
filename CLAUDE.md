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
- `oxabl_ast`: Implemented with expressions, statements, and data types (parser-assigned `NodeId` on every node); `VariableDeclaration`/`DefineTempTable`/`DefineBuffer`/`DefineDataset` carry `is_shared`/`is_new_shared`/`is_new_global_shared` (ast-invariants.md §12); `StatementKind::Skipped { names, may_reference_tables }` distinguishes a recognized-but-unmodelled statement form from an error-recovery `Empty`, carries the identifiers the parser passed over, and marks the three forms whose grammar names a table so the resolve pass offers those names to the buffer/table namespaces as well (ast-invariants.md §8); **two non-wrapper nodes carry a `NodeId` and a `name_span` of their own** — `StatementKind::Using` and `RunTarget::Literal` — because each names a cross-file target as a bare `String` that workspace resolution has to key a `references` entry on, and a "could not be located" diagnostic should underline the name rather than the statement (`RunTarget::Dynamic` gets no id: it names nothing at parse time, and `Identifier` stays deliberately id-free). Both ids and both spans participate in `PartialEq` because `StatementKind`/`RunTarget` derive it — compare the `type_name`/`name` field, or use `RunTarget::literal(name)`, in hand-built test AST (ast-invariants.md §1, §2)
- `oxabl_parser`: Actively developed with 535 tests; parses expressions, control flow, variable declarations, include file references, functions, procedures, temp-tables, error handling, OO-ABL (CLASS, METHOD, PROPERTY, INTERFACE), preprocessor directives, stream I/O, frame definitions, ON triggers (UI events, database events, key remapping), TRIGGER PROCEDURE, embedded SQL (SELECT/INSERT), implicit output/display juxtaposition, widget `:attribute` access, bare `.field` access, and the directionless `DEFINE PARAMETER BUFFER b FOR [TEMP-TABLE] tt` (buffer parameters carry no direction in ABL; only the direction-led spelling parsed before); captures the `[NEW [GLOBAL]] SHARED` prefix on variable/temp-table/buffer/dataset defines; `DEFINE QUERY`, `OPEN QUERY`, and `EMPTY TEMP-TABLE` emit a table-marked `Skipped` node, the last of these carrying its exactly-parsed table name rather than a lexical harvest; the ~29 other recognized-but-unmodelled dispatch sites (`PUT`, `EXPORT`, `UPDATE`/`SET`, `ENABLE`/`DISABLE`, `APPLY`, embedded SQL, …) emit `StatementKind::Skipped` with a lexically harvested identifier list, and the four skip helpers return their token range as `#[must_use]` so a new form cannot be added the old way without a clippy failure
- `oxabl_common/panic_guard`: `catch_panic` + `InternalPanic` — the one guard every client shares (it lives here, not in `oxabl`, because `oxabl` optionally depends on `oxabl_lsp`, so the LSP cannot depend back on the umbrella). Requires the unwinding panic strategy and is a **documented pass-through on `wasm32-unknown-unknown`**, where stable Rust builds `-Cpanic=abort`. Ships a test-only `test-panics` feature: `panic_if_injected` makes a guarded site panic when the source carries an `OXABL-TEST-PANIC:<site>` comment marker, which is how the guards are tested at all — no ABL input panics today
- `oxabl_schema`: `.df` parser + case-insensitive `Schema` model
- `oxabl_semantic`: Declare + resolve + check passes, side tables over NodeId; schema-backed resolution (buffer `table_id` links, field validation/typing via synthesized symbols, bare-table default-buffer fallback); declare pass maps SHARED/NEW SHARED/NEW GLOBAL SHARED to `SymbolFlags`; the `Skipped` arm best-effort-resolves harvested names in `Values` via `lookup_statement_ident` (writes no `references` entry, emits no diagnostic) and flushes hits to `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT` with `read_count`/`write_count` left exact; a `may_reference_tables`-marked `Skipped` node runs a second, independent lookup over `[Buffers, Tables]` crediting `AccessMode::Read`, and `DefineBuffer`/buffer-shaped `DefineParameter` credit their target the same way, guarded against self-crediting the `DEFINE BUFFER Customer FOR Customer` idiom (a bare schema table is still credited by nobody, as with `FIND`/`FOR EACH`). **Cross-file resolution is consulted during resolve, not bolted on after it.** `index.rs` defines the seam — `WorkspaceIndex` with exactly four queries (`class`, `class_members`, `program`, `shared_producer`), an `IndexAnswer` of `Found`/`NotFound`/`Unknowable`, and `NullIndex` as the index that knows nothing — and the resolve pass asks it at the moment a name fails locally, writing a hit into the ordinary `references`/`symbols` tables as an index-synthesized symbol. So a cross-file resolution is shape-identical to a local one and no consumer needs a second lookup path. The design sketch at `docs/design/semantic-v1-cross-file-sketch.md` proposed a post-hoc side table instead and is **superseded**; its R10 claim (no per-file public field reshaped) held anyway. `WorkspaceIndex` deliberately carries **no `Send + Sync` bound** — the language server's implementation borrows a salsa database, and salsa makes a database `Send` but deliberately **not** `Sync`, so no snapshot-backed implementation could ever satisfy it; `oxabl_index::BatchIndex` still pins both for itself in a test. 299 tests
- Unresolved-reason model: `UnresolvedReason` distinguishes **`External`** ("we did not look" — no index attached, the pre-existing suppression state) from **`NotFoundInWorkspace`** (searched the configured paths and the name is genuinely absent) and **`Unknowable`** (not statically knowable — a runtime-computed target, so no amount of indexing helps). A file that *was* located but could not be parsed folds into `NotFoundInWorkspace`: a broken file is knowably unusable, and knowable-versus-unknowable is the only distinction a consumer branches on. `AnalysisContext::index_loaded` is what decides which of the three a miss becomes, and it is **derived from the handle** (only `NullIndex` may report `IndexRevision::ABSENT`), so `with_index(&NullIndex)` cannot be talked into claiming a fact about the workspace. All three reasons are skip-listed by every rule
- **Attaching an index adds no new diagnostic, on purpose.** Two mechanisms hold that line and a reader who takes them for oversights will re-introduce a wave of findings: (1) `crates/oxabl_semantic/src/check.rs` types an index-synthesized **class** symbol as `ResolvedType::Unknown`, so a cross-file type never reaches the type lattice; (2) an inherited member's declared type lives in `SymbolTable::inherited_member_types`, a side map, **not** on `Symbol::data_type` — putting it on the symbol let it reach the lattice and produced new `type-mismatch-assignment` findings, and the analyze envelope reads the side map for **display only**. Turning the rules onto the cross-file population is the scheduled follow-up and needs its own dogfood pass; removing either mechanism belongs to that work, not to a tidy-up
- `oxabl_index`: fact extraction plus the in-run batch cache. One routine, `index_file`, tokenizes/parses/declare-passes a referenced file and projects it to `FileFacts` — a parse that recovered *any* error yields `FileFacts::unparseable` rather than partial facts, because a *wrong* fact mis-attributes symbols across the program graph while a missing one just leaves a name silent. `BatchIndex` is a plain memo over it, keyed by the **lexically normalized** resolved path so two spellings of one file cannot mint two `IndexedFileId`s (which would make `shared_producer` see two producers where there is one). `search` is public because the language server's cache must use the *same* name-to-path policy — two candidate spellings tried in order, **exactly one match** or `Unknowable`, `.i` never a root, no escaping the configured paths. **Neither this crate nor `oxabl_pipeline` may ever gain a `salsa` dependency**: the pipeline depends on this crate, the umbrella re-exports the pipeline unconditionally, and the browser bundle is built through the umbrella, so a `salsa` edge here lands in the WASM payload. No panic guard anywhere in it — every query is total in its *answers*, but `Cancelled` travels as a panic payload, and a guard would turn a cancelled recompute into `NotFound` and freeze a buffer on stale results. No include expansion either: a declaration that only exists after an `{include}` splice is invisible to the index, which is the conservative direction; 48 tests
- `oxabl_lint`: 6 rules — `undefined-symbol` (LINT0001), `unused-variable` (LINT0002; narrowed to "never referenced at all" — a variable passed as a write-back `OUTPUT`/`INPUT-OUTPUT` `RUN` argument counts as used, and a `TABLE FOR`/`DATASET FOR` parameter's read-count question is redirected to the table it names, so a used table no longer looks untouched), `unknown-table-or-field` (LINT0003, live under a loaded schema), `type-mismatch-assignment` (LINT0004), `block-var-used-outside` (LINT0005, INFO: a block-defined variable read outside its block and never assigned outside it — may still hold its default value), `assigned-but-never-read` (LINT0006, WARN: a dead store — written and never read — reported at the write site, falling back to the declaration when the write form is not an assignment; write-back `RUN` arguments stay with #125); per-rule severity via `[workspace.lint]`. LINT0002 and LINT0006 split one population by whether anything ever wrote to the symbol, so a symbol yields exactly one diagnostic and silencing `unused-variable` no longer covers the write-only half. All three count-gated rules (LINT0002, LINT0005, LINT0006) reach their exemptions through the single `unused_symbol_shared::is_skipped` predicate, and all three decline to fire for a symbol carrying `TOUCHED_BY_UNMODELLED_STATEMENT` — a variable named inside a standalone unmodelled form is no longer reported. Two gaps remain by design: skipped *tails* inside modelled statements (#134) and keyword collisions from the deliberately broad harvest filter; the suppression is per-symbol and file-wide, and #136 drains it by head-parsing the forms. The `TABLE FOR` redirect now gets a real answer from the five forms that name a table without reading a field of it (#130), so a temp-table used only by `DEFINE BUFFER`, `DEFINE PARAMETER BUFFER`, `EMPTY TEMP-TABLE`, `DEFINE QUERY`, or `OPEN QUERY` no longer looks untouched. **No rule changed when cross-file resolution landed, and that is deliberate** — the reasons live under `oxabl_semantic` above; `External`, `NotFoundInWorkspace`, and `Unknowable` are all skip-listed, so attaching an index can only *remove* an `undefined-symbol` false positive, never add a finding. `crates/oxabl_lint/tests/cross_file_*.rs` pin that silence and the reason for it, so the day a rule is turned onto the cross-file population the tests say what changes
- `oxabl_analyze`: JSON/text dump of the semantic model with per-section versioning; `touched_by_unmodelled_statement` appears in the symbol flag list, and `unjudged_symbol_count` backs the coverage note so a file the count-gated rules could only partly judge says so instead of looking clean. The envelope emits **eight** versioned sections — `scopes` 1, `symbols` 3, `types` 1, `references` 2, `diagnostics` 1, `preproc` 1, `coverage` 1 (an object, so the next coverage fact is an added key rather than a ninth section), and `dependencies` 1. `preproc`/`coverage` used to be keys the CLI spliced into the returned `Value` after the library handed it back; they are library-emitted now, which also made them visible in `--format text` for the first time. The version map is one private helper because it had two call sites that could drift. **`dependencies` is where cross-file resolution is observable** — `index_revision`, the `files` the run actually consulted (each row saying whether it came in `via` a `class`, a `program`, or a `shared_producer`), and the `unresolved` lookups with their reason and span. It is its own section because it is a property of neither a symbol nor a reference. `symbols` went to 3 and `references` to 2 for added *row* keys, not new sections: a symbol row now carries `origin`, `data_type_source` (which is how an inherited member's type is displayed without living on `Symbol::data_type`), and `supertypes`; a resolved reference row carries the `origin` of the symbol it resolved to, so a cross-file resolution is distinguishable from a local one; 30+ tests
- `oxabl_pipeline`: **the shared run every client drives** (#120). `PipelineConfig::resolve` reads `oxabl.toml` **once** into include paths, lint severities, style, and schema, returning non-fatal problems as `ConfigWarning` **data** so surfacing them is a client's choice; an inner `resolve_from_config` over an already-parsed value makes a second parse impossible by construction. `LintPipeline` exposes `expand`/`collect` as separate phases — the LSP needs the intermediate for watcher matching and salsa early cutoff — plus a composed `run` for non-incremental callers. Only `run` is guarded, reusing `oxabl_common::catch_panic`; the two phases stay **deliberately unguarded** because a guard inside them would swallow salsa's `Cancelled` (which travels as a panic payload) and publish stale diagnostics. `LintResult` distinguishes a run that computed zero diagnostics from one that never got to look, keeps `labels`/`help`, and stays byte-span-only. `FormatPipeline` takes a `StyleGuide` alone — no filesystem, no include paths, **nowhere to put a preprocess flag**, so "the formatter never sees expanded macros" is structural rather than documented; its refusal variant carries the formatter's own `FormatFailure` so the bail-versus-panic split survives without string-matching. `position` gives the byte-offset clients one line/column derivation and documents why the LSP must **not** use it (a byte column is a different number from a UTF-16 column). Owns the one root `FileId`. **Never gains a `salsa` dependency** — the umbrella re-exports this crate unconditionally and the browser bundle is built through the umbrella. A `test-support`-gated `fixtures` module holds the cross-client parity table. **Owns the run's cross-file index.** `LintPipeline::new` builds a `BatchIndex` over the run's *own* `FileSystem` and resolved include paths — unconditionally, because the index reads nothing until a name is looked up, so there is no I/O cost for a client to weigh and nothing to forget. The index is a **run-level** handle reused across many edits of one buffer or across every file of a walk (`with_file` shares it; a per-file sibling borrows rather than rebuilds), and `with_index` lets the language server substitute its own. `with_known_files` seeds it with a list the caller already has: a `SHARED` name maps onto no path, so `shared_producer` can only answer from files the run has already indexed and nothing pulls a producer in unless something happens to `RUN` it — the CLI walk therefore hands over the file list it just enumerated, which makes the producer link work without a directory scan. Clients with no such list (the language server, the browser) simply do not call it
- `oxabl_workspace`: file-system abstraction + `oxabl.toml` config; `resolved_include_paths` auto-discovers `oxabl.toml` (nearest-ancestor walk) and merges `[workspace.sources].include_paths` with CLI `-I` flags (CLI-first, first-match-wins PROPATH). `discovery` owns the **one** root-file policy — `p`/`w`/`cls`/`v` matched case-insensitively, `.i` never a root — replacing two private walkers that disagreed on both the extension set and case sensitivity
- CLI surface: the advertised commands are exactly `check`, `format`, `lsp`, `schema`. `check` is the lint-and-format gate — lint findings and format drift in **two channels** (a finding is span-anchored; drift is a per-file boolean, and merging them would mean synthesizing spans), the drift channel naming the drifting paths, plus a non-findings coverage line that never moves the exit code. A per-file internal panic is reported and the walk **continues** (exit 1, not `analyze`'s 4), with those failures under their own `--json` key so an oxabl bug is distinguishable from an unused variable. `conformance` (the parse-conformance walk `check` used to be) and `analyze` are **hidden but fully supported**; both are documented in the README, because a hidden undocumented command is an undiscoverable one. Exit codes are not uniformly 0/1/2 — `analyze` also uses 4 (contained panic), 6 (serialize failure), and 7 (unsupported `--format`) — and the whole contract is pinned by tests
- Preprocessor include resolution: an unresolvable `{include}` emits a **loud** `PREPROC007` warning ("symbols it declares cannot be checked") that the CLI always surfaces (stderr for `check`, stderr + the `preproc` envelope section for `analyze`), spanned on the include reference itself and carrying a remediation help line. The include body is elided, so each reference to a symbol it declared becomes a real `undefined-symbol` finding. **That combination is the intended behavior, not a gap** — the symbols genuinely are not declared in anything oxabl can see, the same way names from an unimported module are genuinely undeclared, and the `PREPROC007` on the include is what explains the findings. Do not add suppression: it would be per-file and coarse, the same evidence-destroying shape as `TOUCHED_BY_UNMODELLED_STATEMENT` that #136 exists to drain. One real gap remains, filed as **#142**: a **nested** unresolvable include (reached through an include that *does* resolve) emits no `PREPROC007` in any shared-pipeline client, because `expand_source` filters loud preproc diagnostics to root-origin only (`oxabl_analyze/src/collect.rs:231`) — so the `undefined-symbol` findings arrive with nothing naming the cause. `conformance --preprocess` still prints it, and `render_diagnostics` already degrades to `(in included file)` for a foreign-file span, so the CLI half is a filter relaxation; the LSP half needs the primary span re-anchored to the include site
- Cross-client parity: one source string is asserted to yield identical codes, severities, byte spans, and sources through four entry points — the composed vs two-phase run, the CLI binary, the LSP's salsa queries over a rope, and the WASM exports. Byte spans, not rendered positions, so encoding conversion is not mistaken for a pipeline difference; and where a client is deliberately less capable the suite asserts the **capability is unavailable** rather than a different answer. It caught a real divergence on its first run: two default severity tables meant `unknown-table-or-field` and `type-mismatch-assignment` came back `error` in the browser and `warning` everywhere else under the same empty environment. One derived table now, so it cannot drift again. Extended to cross-file: a fixture row can declare **sibling files** and the cross-file resolutions they enable, and each row is asserted twice — with the siblings withheld and with them supplied — so the suite pins the *direction* of the effect. Attaching an index may only remove an `undefined-symbol` false positive, never add a diagnostic; where a row's resolutions are all silent/unresolvable/unknowable, the two answers are asserted **equal**. Sibling files are not a browser capability gap: the browser leg supplies a filesystem through an internal seam, so cross-file rows are fully comparable there
- Language-server cross-file index (`oxabl_lsp/src/db.rs`): `SnapshotIndex` implements `WorkspaceIndex` by calling salsa tracked queries (`indexed_facts` → `indexed_class` / `indexed_class_members` / `indexed_program` / `indexed_defines_shared`), and it is why the trait carries no `Send + Sync` bound — the view borrows the database. **Invalidation is per-file, not global.** Each indexed file gets its own `IndexedFile` salsa input carrying a bumpable `disk_revision`; bumping one invalidates exactly that file's dependents, whereas reusing the single `SchemaHandle` revision every buffer already reads would invalidate *every* open buffer on any dependency edit. **Salsa's own dependency graph is the reverse-dependency map** — there is no hand-maintained one to keep correct. The index queries carry **no cancellation catch**, deliberately: `Cancelled` travels as a panic payload, so catching it would hand back `NotFound` and freeze a buffer on stale results. Include paths, the schema *value*, and lint severities remain plain db configuration rather than salsa inputs; the schema *revision* handle is the one auto-invalidating exception. Measured warm single-edit cycle: **2.72ms** against the 50ms interactivity gate
- Cross-file limitations worth knowing before filing a bug: `shared_producer` can answer only from files the run has already indexed (a `SHARED` name maps to no path and the filesystem trait exposes no listing), which is exactly why the CLI walk seeds the index with the files it enumerated; **same-file** `CLASS Child INHERITS Base` member resolution still does not work (pre-existing, untouched by this work — the index is for *other* files); an unqualified `INHERITS` name is **not** resolved through a `USING` import, the supertype name being taken verbatim; and cross-file `USING` resolution is not observable in the diagnostic channel at all, only in the `analyze` envelope's `dependencies` section
- Workspace total: 1967 tests passing
- Fallible public surface: `oxabl::try_parse`, `try_analyze` / `try_analyze_with_fs`, and `try_format_source` are the canonical entry points, their bodies now delegating to `oxabl_pipeline`; the panicking originals stay `#[deprecated]`. `AnalyzeOptions` is **not** deprecated — it is the live options struct and the browser's only configuration handle, and it converts into a `PipelineConfig`. `FormatFailure` (`#[non_exhaustive]`) separates a formatter bail from a contained panic, leaving the `PartialEq` `FormatBail` untouched. Every in-repo call site is guarded through the shared helper — no `catch_unwind` remains at a call site, including the LSP's two diagnostics paths, where `analyze_guarded`'s single guard spans `compute_diagnostics` **and** `buffer_dependencies` (a panic in expansion one line later would otherwise kill the worker or the main loop)
- Browser crash recovery (`oxabl_wasm` + the website): on `wasm32-unknown-unknown` a panic traps rather than unwinding, so `catch_panic` is inert there and the browser uses a panic hook (stashing the message on `globalThis.__oxablPanicMessage` before the abort) plus `__wbg_reset_state()` from `--experimental-reset-state-function`. The reset re-runs the start function, which re-arms the hook, so recovery survives repeated crashes. `scripts/build-wasm.sh` asserts the export exists **and** that no exception-handling instructions were injected (which would raise the browser floor); nothing else would catch either regression, since no CI job runs `wasm-bindgen`. `--verify` adds a `debug_panic()` export for manual checks and must never ship
