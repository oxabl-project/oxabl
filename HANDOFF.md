# Handoff: the daemon and reverse index have shipped; correctness backlog is next

**Date:** 2026-10-07
**Branch:** `master`. Nothing is in flight on a long-lived branch except the formatter fixes (#199–#204).
**Since the last handoff (2026-07-31):** the reverse dependency index, the workspace daemon, daemon hardening, include roots as fragments, the nested-include diagnostic, and the OpenEdge metaschema all merged. Details are under **What shipped**.

Nothing from a real ABL codebase belongs in this repo. Report A/B results as proportions, never as counts or paths. `docs/plans/` is gitignored; plans are local, and module docs cite their labels (R6, KTD1, ...).

---

## Current state

Open issues, by theme. Get the live list with `gh issue list --state open --limit 100`.

| Theme | Issues | Status |
|-------|--------|--------|
| Formatter correctness | #199 (not idempotent after `IF ... THEN` wrap), #200 (TAB inside a string literal changes its value), #201 (comment in a `FINALLY`/`CATCH` block over-indented), #202 (`FIELDS`/`EXCEPT` shifts `FOR EACH` continuation), #203 (statement after `WHEN ... THEN` moved to the `WHEN` column), #204 (first-line comment indented when the file has a routine with parameters) | **In flight.** Tracked by #78. These break the "never mangle code" commitment, so they come first. |
| Daemon and dependency graph | #170 (cold workspace pass under one minute), #161 / #162 / #163 (session core, `oxabl/*` methods, parity leg), #103 (reverse edges), #56 (extraction fidelity vs the AVM compiler) | #161–#163 and the reverse-edge half of #103 are delivered by #164/#165. Their issues stay open. #170 is the one real gap. #56 needs an AVM compiler for its XREF-diff check. |
| Parser false positives | #198 (`GET-CODEPAGE`), #194 (keyword-named `USING` class), #193 (`@Test` annotations), #192 (`JsonDataType:NULL`), #191 (`SERIALIZABLE` modifier), #184 (`ACCUM ... TOTAL BY`), #181 (OE12 short-form `VAR`), #159 (`DEFINE WORKFILE`), #139 (buffer parameter target), #136 (head-parse unmodelled statements), #108 (unresolvable include as call argument) | Open. Each is a real construct that reports as an error or a false `undefined-symbol`. |
| Lexer built-ins | #183 (`SET-SIZE`, `PUT-BYTE`, `WIDGET-H`; widen into an audit), #156 (`FIX-CODEPAGE`) | Open. Same root cause: the known-built-in list is incomplete. |
| Semantic and schema false positives | #185 (sequences not modelled), #180 (`&IF`/`&ELSE` arms share a scope with preprocessing off), #179 (named buffer re-declared from the same-named table), #178 (inherited member with unresolvable supertype), #160 (field vs temp-table buffer), #157 (arguments of a `:`-qualified call not walked), #155 (type name in `CAST`/`TYPE-OF`), #154 (typecheck built-in classes), #134 (reads in skipped tails) | Open. |
| Lint false positives and rule work | #197 (call args before more member access not credited), #196 (`EXTERNAL` prototype parameters unused), #195 (`CATCH` variable unused), #182 (scalar into `EXTENT` variable), #131 (LINT0006 write sites), #124 (path-aware LINT0005), #126 (CFG and dataflow), #132 (no `oxabl_lint` benchmark), #57 (public rule API) | Open. |
| Editor include context | #167 (retain include-origin diagnostics with compilation context), #168 (select an includer context for open fragments), #169 (inspect an expanded compilation unit) | Open, `ready-for-agent`. Follows from #166. |
| Config and workspace | #143 (extension set configurable), #118 (schema auto-discovery), #86 (`oxabl_style` from `oxabl.toml`), #144 (`check --watch`) | Open. |
| Umbrella trackers and API | #102 (cross-file resolution), #77 (LSP), #78 (formatter), #55 (public API), #117 (AST `Serialize`/`Display`) | Open. #102 and #55 are delivered in substance and can close. |

---

## What shipped since July

- **Reverse dependency edges and impact (#164).** A typed edge set per file with six kinds: `direct_include`, `transitive_include`, `schema_table`, `class`, `program`, `shared_producer`. `oxabl_pipeline` inverts them into a workspace graph. It answers two separate questions: dependents grouped by cause, and the rebuild set as the transitive closure. The `analyze` envelope `dependencies` section is at version 3.
- **Daemon session core and wire contract (#165).** New crates `oxabl_daemon` (the only crate that may know salsa) and `oxabl_daemon_protocol` (serde only). One salsa instance serves one workspace root and several clients. Methods: `oxabl/handshake`, `oxabl/impact`, `oxabl/symbolSearch`, `oxabl/freshness`, `oxabl/reindex`. The hidden `oxabl daemon` command launches it. `oxabl_lsp` is now a stdio shim that joins or starts the daemon. The parity table has a daemon leg.
- **Daemon hardening (#173, #174, #186, #187).** Symlink-safe registry. Liveness probe by non-blocking connect, not by lock. A read query no longer rewrites a peer's configuration. The workspace pass retry is bounded, and a superseded answer says so. An unconfigured workspace refuses cross-file questions. Roots are canonical. Freshness detects added files and edits that keep length (inode and `ctime` stamps). Spans ending on an include boundary keep their extent. **Contract version is 4, and the socket and lock moved to `$XDG_RUNTIME_DIR/oxabl/daemon`.** This is a breaking change: an older client no longer finds a running daemon.
- **Include roots as fragments (#166).** An `.i` opened as a root is a fragment. Findings that need the whole compilation unit (LINT0002, LINT0005, LINT0006) are withheld. Locally provable findings stay. The coverage report shows the reduction.
- **Nested unresolvable include (#177, closes #142).** `PREPROC007` is re-anchored to the outermost include site, with the nested name in the message. One missing include reached twice through one site reports once.
- **OpenEdge metaschema (#189).** `crates/oxabl_schema/resources/metaschema.df` ships the dictionary and virtual system tables. It is borrowed by every schema, not copied. User tables win a name collision. `scripts/gen-metaschema-df.sh` regenerates it (needs a licensed install; see `METASCHEMA.md`).
- **Release (#188).** The crates.io publish step waits out 429 rate limits and retries. It fails at once on any other error.

---

## Next

1. **Formatter correctness, #199–#204 (in flight).** Finish these before new formatter features. #199 and #202 break idempotence. #200 changes compiled meaning. Add each case to the idempotence and semantic-preservation tests.
2. **Cold-pass speed, #170.** The cold workspace pass misses the 60 s target by a wide margin. Query latency is fine. Profile first. Keep correctness, freshness, parity, unresolved reporting, and explicit reindex unchanged. Remember that the pass is sequential because a salsa database is `Send` but not `Sync`.
3. **Parser and lint false-positive backlog, #178–#198.** Trust is lost to false positives, not to missed findings. Start with the parser errors (#191–#194, #198, #181): they turn valid code into an error. Then the false `undefined-symbol` and unused-variable groups. #183 and #156 are one audit of the known-built-in list.
4. **Editor include context, #167–#169.** Do #167 first: it keeps include-origin diagnostics with the compilation context. #168 and #169 build on it.
5. **After those:** #136 and #134 (drain the unmodelled-statement suppression), #57 (public rule API), #126 then #124 (CFG and path-aware LINT0005), #132 (lint benchmarks), #144 (`check --watch`).
6. **Housekeeping.** Close #102, #55, and #161–#163 if their acceptance is met. Re-scope #103 to the cache-warming remainder, or close it. Confirm or close #108.

---

## Decisions and gotchas

### Cross-file index

- **`WorkspaceIndex` has four queries** (`class`, `class_members`, `program`, `shared_producer`) plus the defaulted `searches_any_path`. Answers are `Found`, `NotFound`, `Unusable`, or `Unknowable`. A client that cannot answer says `NotFound`. There is no narrower trait.
- **It has no `Send + Sync` bound, on purpose.** A salsa-backed index borrows the database handle, and salsa makes a database `Send` but not `Sync`. `BatchIndex` pins its own `Send + Sync` in a test.
- **No `salsa` in `oxabl_index` or `oxabl_pipeline`.** The umbrella re-exports the pipeline, and the browser bundle builds through the umbrella. Only `oxabl_daemon` depends on salsa (pinned `=0.28.1`).
- **Unresolved reasons are four-valued.** `AbsentFromWorkspace` is a real path-search miss. `PresentButUnusable` means the name exists and cannot be used from here. `undefined-symbol` reports only the first. Its reason match is exhaustive, so a new variant fails to compile in that rule.
- **`index_loaded` is derived from the handle.** Only `NullIndex` reports `IndexRevision::ABSENT`. An index with no search path answers `NotFound` without looking, so that miss stays `External`.
- **A recovered parse yields no facts.** `index_file` returns `FileFacts::unparseable` when the parse recovered any error. A wrong fact mis-attributes symbols. A missing one stays silent.
- **Index path keys are lexically normalized.** Two spellings of one file must not get two `IndexedFileId`s, or `shared_producer` answers `Unknowable` for a name with one producer.
- **The index never catches panics.** `Cancelled` travels as a panic payload. A guard would turn a cancelled recompute into `NotFound` and freeze a buffer on stale results.
- **No include expansion during indexing.** A declaration that exists only after an `{include}` is invisible. A located file that does not visibly declare its class answers `Unusable`, not `NotFound`.
- **`search` is public** so the daemon's cache uses the same name-to-path policy: two spellings tried in order, exactly one match or `Unknowable`, `.i` never a root, no escape from the configured paths. A `RUN` target accepts any extension except `.i` (`ExtensionPolicy::AnyButInclude`).
- **`shared_producer` needs a seed.** A `SHARED` name maps to no path. `LintPipeline::with_known_files` hands the index the walk's file list. The daemon and the browser do not call it.
- **The daemon invalidates per file.** Each indexed file is its own salsa input with a bumpable disk revision. Salsa's dependency graph is the in-editor reverse map.

### Dependency edges and impact

- **Under-reporting a dependency is the dangerous failure.** The graph reports its gaps as data: unresolved includes and references live in their own collection with no accessor that merges them with edges. A file the pass cannot read or analyse is recorded as unanalysed, never as "depends on nothing". The workspace unresolved ratio travels with the answer.
- **Schema edges carry the folded table name and no CRC.** `TableId` is an index into one arena and is not workspace-stable. A schema revision mismatch yields no schema edges, not wrong ones. CRC comparison stays with the build tool or XREF.
- **Two known fidelity gaps (#56).** A class named only by `USING`, `NEW`, or `AS CLASS` leaves no unresolved row when it fails to resolve. A missed `RUN` target or `SHARED` producer leaves none either. Parity fixtures pin the current shape.
- **The rebuild-set completeness check against a full compile is not done.** It needs the AVM compiler (#56). Report completeness and tightness separately. A miss on completeness is the finding that matters.
- **Truncation stays visible.** Symbol search reports the total over every match, before the bound applies.

### Daemon

- **`oxabl_daemon` owns four supersession decisions** as one returned `Disposition`: a superseded buffer version drops, a superseded configuration re-arms, a cancellation re-arms, and a genuine panic fails one request and is never retried.
- **Query handlers are read-only for session configuration.** A client asking for impact must not replace what an editor client resolved.
- **A daemon serves only the root it was bound to.** The handshake and the LSP `initialize` both check it.
- **The registry creates and validates, never repairs.** It refuses a symlink at any component and refuses a directory not owned by the user. A `XDG_RUNTIME_DIR` set to an unusable value is refused, not skipped. Tests must override every base variable, and one test asserts the real runtime directory is untouched.
- **Freshness compares the discovered file count**, not the tracked count. The tracked set includes include targets outside the tree. The directory walk runs only when every stamp is clean, at most once per interval.
- **A contract change bumps `CONTRACT_VERSION`** in `oxabl_daemon_protocol`. Do not ship a release between the first and last change of one breaking series.

### Shared pipeline

- **`PipelineConfig::resolve` reads `oxabl.toml` once.** `resolve_from_config` takes an already-parsed value. Do not add a convenience wrapper that re-reads the file. Non-fatal problems are `ConfigWarning` data.
- **`LintPipeline::expand` and `collect` are unguarded; only `run` is guarded.** Salsa's `Cancelled` travels as a panic payload, and a guard inside the phases would publish stale diagnostics. Leave this alone.
- **`FormatPipeline` takes a `StyleGuide` alone.** It has no filesystem and no preprocess flag, so the formatter cannot see expanded macros.
- **Results are byte-span-only.** The editor's rope is the position oracle under a negotiated encoding. A byte column is not a UTF-16 column.
- **`check` preprocesses by default**, to match the editor. It reports lint findings and format drift in two channels that are never merged.
- **The visible CLI is `check`, `format`, `lsp`, `schema`.** `conformance`, `analyze`, and `daemon` are hidden. `conformance` and `analyze` are supported and documented in the README. Exit codes are not uniformly 0/1/2; tests pin them.
- **One root-file policy** lives in `oxabl_workspace::discovery`: `p`, `w`, `cls`, `v`, case-insensitive, `.i` never a root.
- **`check.rs` types every `MethodCall` and `MemberAccess` as `Unknown`.** A cross-file type reaches the type lattice only through an unqualified reference. This is the larger half of the cross-file population, and it is closed on purpose. Open it with its own plan and A/B.
- **An unresolvable `{include}` emits a loud `PREPROC007`, and the `undefined-symbol` findings that follow are correct.** Do not suppress them. The warning explains them.

### Parity suite

- It asserts that one source gives identical codes, severities, byte spans, and sources through the composed run, the CLI binary, the LSP, the WASM exports, and a daemon session. It also observes dependency edges.
- A fixture row declares sibling files and runs twice, with siblings withheld and supplied, so it pins the direction of each effect. `CrossFileEffect` has six variants. `Judged` is a finding that cross-file resolution produces. `ResolvedFromWorkspaceMiss` is a finding that exists only because a path was searched, so it does not arrive in the browser.
- A client with less capability must report the capability as unavailable, not a different answer.

### Lint accuracy

Read this before you triage a "LINT0006 is wrong" report.

- **Unmodelled statements are credited, not parsed (#128/#137).** About thirty forms (`PUT`, `EXPORT`, `UPDATE`, `SET`, ...) emit `StatementKind::Skipped { names }`. Resolve credits the names it finds and sets `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`. LINT0002, LINT0005, and LINT0006 all consult it through the one `is_skipped` predicate in `unused_symbol_shared.rs`. The mark is per symbol and file-wide, so it destroys evidence. #136 is the planned drain. `DELETE OBJECT` is already head-parsed. `COMPILE` emits `Skipped` with an empty name list on purpose.
- **Table-use forms credit a read on the table (#130).** `Skipped` carries `may_reference_tables`. The same-name guard is load-bearing: `DEFINE BUFFER Customer FOR Customer.` must not credit the new buffer for existing. Both credit paths use `resolve_statement_ident`, which is silent on a miss. A bare schema table is credited by nobody.
- **`FOR EACH tt:` declares a block-scoped buffer.** `backing_read_count` sums reads over ancestor-or-self and descendant `Buffers` bindings. Keep the descendant half.
- **`is_table_like_param` is outside `is_skipped` on purpose.** LINT0006 skips those symbols. LINT0002 must still report a genuinely unused one.
- **Test helper:** `Expression::new` carries `NodeId::DUMMY`, which the `references` table drops. Use the `ident_expr` helper that allocates real ids when a test asserts a write-site span.
- **Audit both sides of a count-gated predicate.** The false positives came from the unaudited read side.
- **`ParameterType::Buffer` records the wrong target in an inline procedure signature** (#139). The parser discards the table name and sets `target` to the buffer's own name.
- **New parser lookahead must allow a comment in the gap.** Use `peek_nth_non_comment`.
- **LINT0001 ratio:** quote any A/B ratio together with the input manifest that produced it.

### A/B tooling

- `scripts/lint-ab-diff.sh` writes an input manifest per collection (hashes, counts, revisions; no paths) and refuses to diff two sides whose inputs disagree. The `oxabl.toml` that drives a run is found by walking up from each file, so it may not appear on the command line.
- A JSON consumer with defaulted key lookups fails silently when a `--json` shape changes. Read keys strictly.

### Browser client

- `oxabl_wasm` is a transport adapter over `oxabl_pipeline` and has no ABL behavior. Its exports are `analyze_source`, `format_source`, and `version()`.
- Any native-only dependency of `crates/oxabl` goes behind the `cli` feature, or the wasm build breaks. CI has a `WebAssembly client` job for this.
- `wasm-bindgen` is pinned exactly (`=0.2.126`). The pin sites are the crate, `scripts/build-wasm.sh`, and `.github/workflows/release.yml`. A mismatch fails at bindgen time, after `cargo build` succeeds.
- `catch_panic` passes through on `wasm32-unknown-unknown`. Recovery uses the panic hook and `__wbg_reset_state`, which `build-wasm.sh` checks.
- Absent capabilities (includes, `.df` schema, `oxabl.toml`) stay unavailable, not stubbed. The wire shape is not a stable contract.

---

## Related docs

- `STRATEGY.md`: tracks, metrics, and what we do not work on.
- `crates/oxabl_schema/METASCHEMA.md`: how the metaschema catalog is made.
- `docs/design/ast-invariants.md`: node id and span rules (§1–§2 cover `StatementKind::Using` and `RunTarget::Literal`).
- `docs/design/semantic-v1-cross-file-sketch.md`: superseded. Read its banner, not its body.
