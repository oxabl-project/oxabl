---
artifact_contract: ce-unified-plan/v1
artifact_readiness: requirements-only
product_contract_source: ce-brainstorm
date: 2026-07-22
---

# Track A: Diagnostics-to-Editor (LSP) - Plan

## Goal Capsule

**Objective.** Ship an LSP server (as `oxabl lsp`) that surfaces oxabl's existing parse + lint + preprocessor diagnostics live in an editor, on open/change/save, with per-keystroke-debounced feedback that feels instant. The track is gated on making the preprocessor fast enough for interactive use.

**Product authority.** The project owner. Architecture transport/sync decisions are pre-settled (see below); the perf bar and v1 scope were confirmed by the project owner on 2026-07-22.

**Open blockers.** None. The prerequisite once believed to gate this track (preprocessor performance) was tested with a build-and-measure salsa spike on 2026-07-22 and found already met (~15ms p95 vs a 50ms bar, ~3x headroom, zero caching). All Track A decisions are settled: v1 adopts **coarse salsa 0.28** as its substrate (the project owner approved on the spike).

**Settled decisions (baked in, not re-litigated).**
- Transport: **`lsp-server`** (rust-analyzer's crate, hands-on thread ownership) — chosen partly to lay groundwork for a possible future **salsa** adoption. Not `async-lsp`.
- **Single binary**: the LSP is a sub-command `oxabl lsp`.
- **Incremental text sync** (`TextDocumentSyncKind::Incremental`) backed by a **rope (`ropey`)** per open buffer.
- **Debounce** diagnostics ~150-300ms after the last edit.

---

## Product Contract

### Problem & Value

oxabl already has a working diagnostics engine — parser, `oxabl_semantic`, and `oxabl_lint` (4 rules) run today via `oxabl analyze`/`oxabl check`. That value is trapped behind a CLI. The fastest visible, shippable win is to pipe those exact diagnostics into an editor live. Diagnostics reuse the existing pipeline verbatim and — unlike formatting — do **not** require trivia/comment preservation, so this track carries almost no new analysis risk. The one real risk is performance: the preprocessor is currently too slow to run per-keystroke, and without preprocessing the linter flags every include-resident symbol as undefined (issue #77). So a *useful* LSP is gated on preprocessor speed.

### Primary Actor & Outcome

**Actor:** an ABL developer editing a `.p`/`.cls`/`.w`/`.i` file in an editor with an LSP client (VS Code is the convergence target; the extension itself is out of scope here).

**Outcome:** as they type, red/yellow squiggles for parse errors, lint violations, and loud preprocessor problems appear within a debounce cycle and update without perceptible lag — even on a large, heavily-edited, include-heavy file.

### Scope

**In scope (v1):**
- `oxabl lsp` sub-command speaking LSP over stdio via `lsp-server`.
- Server lifecycle: `initialize`/`initialized`/`shutdown`/`exit`; advertise only the capabilities v1 implements.
- Document sync: `didOpen`/`didChange` (incremental)/`didSave`/`didClose`, each open buffer held as a `ropey` rope.
- Debounced diagnostics computation (150-300ms) and `textDocument/publishDiagnostics`.
- Diagnostics surfaced = **parse errors + all 4 lint rules + loud PREPROC diagnostics** (e.g. PREPROC007 unresolvable include, PREPROC002 unclosed `&IF`), all computed on the open buffer.
- Schema-dependent rules (LINT0003 unknown-table-or-field, LINT0004 type-mismatch) go live **only when a `.df` schema is discovered** via the existing `oxabl.toml`/workspace config; otherwise they stay dark (no false positives). The `.df` is a salsa input → schema changes **hot-reload** (A2).
- **Coarse salsa 0.28 caching substrate** (settled) — per-file tracked queries; write-on-main / read-on-snapshot threading with `Cancelled`-on-`set_input`.
- **`[lint]` config surface in `oxabl.toml`** (A3) — per-rule 5-level severity (`off|hint|info|warn|error`) mapping to `DiagnosticSeverity`, resolved via the workspace discovery walk.
- **Two synthetic CodSpeed benchmark gates** (A4) — WARM ≤50ms p95 and COLD-open ~500ms–1s, plus a `finished in N` timing line.

**Out of scope (v1):**
- Formatting (Track B; converges later at the VS Code extension).
- Any non-diagnostic LSP feature: hover, go-to-definition, completion, rename, code actions, semantic tokens, symbols.
- Cross-buffer/workspace-wide diagnostics. Editing an include does **not** re-lint other open buffers.
- The VS Code extension itself (shared convergence point; this doc only records the client-side requirements the server imposes).
- **Fine-grained** salsa incrementality (per-include inputs, per-procedure tracked entities). Coarse per-file salsa **is the settled v1 substrate** (in scope — see caching strategy); only the invasive fine-grained variant is deferred.

### The Preprocessor-Performance Prerequisite (the crux)

> **Status update (2026-07-22): largely de-risked by the salsa spike.** The full cycle already runs at ~15ms p95 on the target fixture with *zero* caching, and preprocessing is only ~2ms of it (warm OS cache). The "gate" is effectively already open. What survives is not a blocker but a discipline: keep a committed real-disk benchmark so the bar can't regress. See "Spike Findings" and the revised caching strategy below.

This was framed as the gating workstream; the spike shows it lands comfortably.

**Definition of "fast enough" (confirmed).**
- **Bar: the full interactive cycle — `preprocess → tokenize → parse → semantic → lint → publish` — completes in ≤ 50ms at p95.**
- **Held to a specific fixture:** a representative large, high-churn ABL file (kept outside the repo), **with its real include graph, measured on real disk in that environment.** Rationale (the project owner): this is not the largest corpus file but it is both large and high-churn — active development happens in it constantly. If a developer on a small file gets sub-second feedback but this common, heavily-edited file does not, the LSP has failed its purpose. Hold the bar to the realistic worst case, not the easy case.
- 50ms leaves comfortable headroom under the 150-300ms debounce and reads as instant.

**Current cost (verified in `crates/oxabl_preprocessor/src/preprocessor.rs`).** Each `Preprocessor::process()` call is single-pass and stateless across calls: it constructs a fresh `ProcessContext`, re-resolves every include by iterating the PROPATH with repeated `exists()` stats (`resolve_include`), re-reads every include's contents from disk (`fs.read`), and re-expands the whole graph recursively. Nothing is cached between calls. The `check` pipeline does exactly this per file (`main.rs` ~L540-600). Today's committed benches are all in-memory (`InMemoryFileSystem`) and do **not** model real disk-stat/read latency. The spike closed that gap ad hoc (real-disk measurement of the target file) and found the re-read cost is ~2ms warm — small, not the bottleneck. A committed real-disk benchmark should replace the spike's throwaway harness.

**Measurement protocol — SETTLED (A4).** Two fixed-synthetic CodSpeed gates, both CI-ready:
- **(a) WARM steady-state** — single-edit full-cycle on a warm buffer/salsa state (the per-keystroke case). **Gate: ≤50ms p95.**
- **(b) COLD-open** — first open of a file with caches/salsa cold (the one-time open cost). **Gate: looser, ~500ms–1s.**
- Report p95, not just mean — interactive feel is governed by the tail.
- **Hard requirement:** the LSP (and/or the bench harness) emits a `finished in N` timing line so the project owner can eyeball latency manually against the target file locally.
- **No real-edit trace** — synthetic edit sequences only (see the no-PII constraint below).

**Project constraint — no proprietary data in the repo.** No proprietary data, PII, or private corpus may enter the oxabl repo. Benchmark fixtures are **synthetic workloads only**. the target file and its include graph are referenced by **absolute path for local runs only** and are **never vendored** into the repo. The committed CodSpeed gates therefore run against a synthetic fixture that approximates the real file's shape (size, include fan-out); the real file is for local eyeballing, not CI.

**Caching / incrementalization substrate — SETTLED (the project owner approved on the spike, 2026-07-22).**

**Decision: adopt coarse salsa now as the v1 caching substrate.** salsa **0.28** via the new `#[salsa::db]` / `#[salsa::input]` / `#[salsa::tracked]` API, with per-file tracked queries (`Buffer` input → `expanded_text` tracked → `lint_diags` tracked, as built in the spike). The two hand-rolled caches stay **retired**; fine-grained salsa (per-include inputs, per-procedure tracked entities) stays **deferred** to a measured future need. This is an ownership/substrate choice, not a perf fix — the spike proved perf needs no help today (see below) — chosen because the lift is small and clean and it removes any future two-cache→salsa migration.

Grounding from the spike (full details in "Spike Findings"):
- **≤50ms bar already met with zero caching** — full cycle raw p95 = ~15ms, ~3x under budget.
- **Preprocessing is not the bottleneck** — ~2ms p95 warm; parse+semantic+lint (~10ms) dominates.
- **The two hand-rolled caches would save ~2ms we don't need** — retired.
- **Coarse salsa adds ~0.4ms p95 overhead and no typing speedup** (every keystroke changes the buffer text → full recompute); its incremental value only appears at finer granularity, which is the invasive part and is unwarranted at 3x under budget.

**v1 skeleton requirement — threading discipline.** salsa 0.28 `Storage` is `Clone`/snapshot-based; mutating an input (`set_input` on `&mut db`) cancels in-flight reads via `Cancelled` unwinding. The v1 skeleton **must** implement the rust-analyzer pattern: writes (buffer edits, schema changes) on the main loop; diagnostics computed on a cloned snapshot on a worker thread; handle the `Cancelled` unwind. The spike was single-threaded, so this is designed-for but still to be implemented.

Include-invalidation in the coarse model: because includes are read *inside* the `expanded_text` query on every recompute (not modeled as salsa inputs), an open buffer's own edits pick up include changes for free each cycle — no cache to invalidate. The only residual is an include changing on disk while its dependent buffer sits idle. See settled A1.

### Spike Findings (2026-07-22)

Throwaway salsa spike lives outside the tree in a local scratch directory (not committed; scratch). It path-depends on the oxabl crates + salsa 0.28.1 and measures the full cycle on the target file (a large real-world ABL file, ~466KB / ~12k lines, expanding to ~733KB with ~17 real includes, single-dir PROPATH) on real disk, 200 iterations each, warm OS page cache.

| Measurement | p50 | p95 | p99 | mean |
|---|---|---|---|---|
| RAW full cycle (preprocess→parse→sem→lint) | 13.4ms | **14.9ms** | 16.0ms | 13.4ms |
| — preprocess-only | 2.0ms | 2.2ms | 2.3ms | 2.0ms |
| — parse+sem+lint (no pp) | 10.1ms | 11.3ms | 12.4ms | 10.2ms |
| SALSA full cycle (keystroke = text mutated each iter) | 13.4ms | **15.3ms** | 17.0ms | 13.6ms |
| SALSA memo-hit (unchanged input) | ~0ms | ~0ms | ~0ms | ~0ms |
| SALSA set-same-value + query | 2.0ms | 2.2ms | 2.3ms | 2.0ms |

Readings:
1. **≤50ms bar: met with ~3x headroom**, raw and under coarse salsa alike.
2. **Salsa per-query bookkeeping is negligible** — ~0.4ms p95 over raw (~2-3% of cycle); memo-hit is sub-microsecond.
3. **Preprocessing (~2ms) is not the bottleneck** once the OS cache is warm; parse+sem+lint (~10ms) dominates.
4. The `set-same-value` row (2.2ms, not 15ms) is salsa's **early-cutoff firewall** in action: bumping the input re-ran `expanded_text` (2ms) but salsa saw byte-identical output and skipped the 10ms parse+sem+lint. Confirms the machinery works; also confirms that at *coarse* granularity a real keystroke (which does change the text) gets no such shortcut.

Caveats: measured with a warm OS page cache (realistic for a live session; a first-ever cold read of the include graph is a one-time cost, not per-keystroke). PROPATH here is a single directory — a deep PROPATH would add `exists()`-stat cost to the ~2ms preprocess step, still small when warm. No `.df` schema loaded (LINT0003/0004 dark); adding schema-backed resolution over a large `.df` adds semantic cost not captured here.

### LSP Server Skeleton Requirements

- **Lifecycle:** standard `lsp-server` main loop owning its own threads. Handle `initialize` (return `ServerCapabilities`), `initialized`, `shutdown`, `exit`. Clean shutdown on client disconnect.
- **Capabilities advertised (v1):** `textDocumentSync = Incremental` (with open/close notifications), `diagnosticProvider` semantics via push (`publishDiagnostics`). Advertise nothing else — no hover/completion/etc. — so clients don't offer features that don't exist.
- **Document store:** map of URI → `ropey::Rope` + version. `didChange` applies incremental content changes to the rope by range; `didClose` drops the entry.
- **Debounce:** per-document timer; a burst of `didChange` events collapses to one diagnostics computation ~150-300ms after the last edit. In-flight computations for a superseded version are dropped/ignored.
- **Diagnostics publishing:** on each computation, run the pipeline on the current rope snapshot, map internal spans → LSP `Range` (line/character), and publish. Clear diagnostics on close.
- **Span/position mapping:** internal byte offsets → LSP UTF-16 line/character positions. The existing `SourceMap` gives line/col from byte offset; the rope gives efficient offset↔position and UTF-16 handling. Diagnostics arising inside an expanded include must map back to the **open buffer's** coordinates (or be suppressed if they don't belong to the open buffer) — the preprocessor already tracks source provenance via its span tree / `resolve()`.

### Wiring to the Existing Pipeline

The LSP reuses the exact chain already driven by `oxabl analyze`/`check` — no new analysis logic:

1. Take the open buffer's text from the rope.
2. `Preprocessor::process()` → expanded text + span tree + preproc diagnostics. (Under coarse salsa this is the `expanded_text` tracked query; it re-reads includes from disk each recompute — ~2ms, no separate cache.)
3. `tokenize()` → `Parser::parse_statements()` → AST + parse diagnostics.
4. `oxabl_semantic` passes → semantic model.
5. `oxabl_lint::lint_file(program, sem, ctx)` → lint diagnostics (schema-gated rules honor `schema_loaded`).
6. Merge parse + lint + surfaced-preproc diagnostics; map spans; publish.

Schema discovery reuses the existing `oxabl.toml` workspace config path (nearest-ancestor walk, `resolved_include_paths`) so the LSP's PROPATH and `.df` come from the same source the CLI already uses. This keeps CLI and LSP diagnostics identical for the same file — a correctness anchor and a testing lever.

**Schema hot-reload (settled, A2).** The `.df` schema is a **salsa input**. When the schema file changes on disk, the LSP re-`set_input`s it and the dependent `lint_diags` queries recompute automatically — schema-dependent rules (LINT0003 unknown-table-or-field, LINT0004 type-mismatch) update **live**, no restart. This is the direct payoff of having salsa in v1 (the earlier "restart-only unless salsa" fallback resolves to hot-reload).

### Lint Configuration Surface (settled, A3)

v1 ships a **config surface** for the lint rules — a `[lint]` table in `oxabl.toml`, mirroring the existing `[style]` convention:

- **Shape:** a typed `LintConfig` struct with `#[serde(default)]` partial override, `from_toml`/`to_toml`, resolved on `oxabl_workspace` via the same nearest-ancestor discovery walk as `resolved_include_paths` / `resolved_style` (#86).
- **Precedence:** CLI > `oxabl.toml [lint]` > default.
- **Keys:** one per rule, using kebab rule-names — `undefined-symbol`, `unused-variable`, `unknown-table-or-field`, `type-mismatch-assignment`.
- **Value:** a 5-level enum `off | hint | info | warn | error`, mapping 1:1 to LSP `DiagnosticSeverity`. A single knob controls both enable/disable (`off`) and severity — no separate on/off flag.
- **Safe default (no `[lint]` table present):** all four rules on; `undefined-symbol = error`; the other three = `warn`.
- **Boundary:** this is the **config surface only**. The #57 Rust `Rule`-trait / registry (custom-rule *registration*) stays deferred, but `[lint]` is designed so that future registry can consume it. Inline `// oxabl-disable` pragmas are deferred to v2.

This makes rule severity live-configurable through the same config the LSP already discovers, and — combined with the 5-level→`DiagnosticSeverity` mapping — lets the editor render each rule at the user's chosen severity.

### Salsa Composition & Effort (from the spike)

Salsa-now is settled (see caching substrate above). These are the concrete integration facts the spike established:

- **No existing crate entry points change.** `tokenize`, `Parser::parse_statements`, `analyze_file`, and `lint_file` are already pure functions over owned inputs returning owned outputs (`Statement` and `Semantic` carry no lifetimes). The spike **wrapped** them in `#[salsa::tracked]` functions without editing any of them. The pipeline is already salsa-shaped.
- **No NodeId side-table conflict.** `Semantic` owns its `NodeIndexVec` side tables (`references`, `types`) keyed by parser-assigned `NodeId`; they are produced and consumed within a single tracked query, so they stay internally consistent inside one memo. Coarse granularity never shares NodeId across queries. (Fine-grained per-entity tracking *would* need care here — another reason to keep v1 coarse.)
- **API recommendation: salsa 0.28.x, the new `#[salsa::db]` / `#[salsa::input]` / `#[salsa::tracked]` API** (the lineage rust-analyzer runs today). The `returns(ref)` attribute and the `Update` return-type model worked out of the box; the only fixes needed were importing `salsa::Setter` and cloning a `PathBuf`. Avoid the legacy salsa-2022/0.16 macro-database API.
- **The one place friction would appear** is if we later store rich intermediates (`Vec<Statement>`, `Semantic`) in memos for cross-query reuse — those types would need `Update` impls (derive or manual). The spike sidestepped this by having each tracked query return owned summaries / re-derive within the query, which is sufficient at coarse granularity.
- **Threading & cancellation with `lsp-server`:** salsa 0.28's `Storage` is `Clone` and snapshot-based, and mutating an input (`set_input` on `&mut db`) cancels in-flight reads on worker threads via `Cancelled` unwinding — this is exactly the rust-analyzer pattern `lsp-server` was chosen to enable (writes on the main loop, diagnostics computed on a snapshot on a worker). The spike did **not** exercise threads (single-threaded measurement), so this composes *by design* but is unproven here; the v1 skeleton must implement the write-on-main / read-on-snapshot discipline and handle the `Cancelled` unwind. Debounce reduces how often cancellation actually fires.

### VS Code Client-Side Requirements (convergence note — extension out of scope)

Recorded so Track A and Track B converge cleanly at the shared extension:
- Client launches `oxabl lsp` over **stdio**.
- Client must send incremental `didChange` (server advertises `Incremental`); if a client only supports full sync the server must still function (accept full-text changes) — confirm graceful handling.
- Diagnostics are **push** (`publishDiagnostics`); the extension needs no pull-diagnostics support for v1.
- Document selector: `.p`, `.i`, `.cls`, `.w` (ABL language IDs).
- The extension should surface where the `.df` schema / `oxabl.toml` is discovered so a developer can tell whether schema-gated rules are live.

### Success Criteria

- `oxabl lsp` starts, completes the LSP handshake, and a real editor shows squiggles for a file with known parse/lint/preproc issues.
- Diagnostics update within one debounce cycle of a stopped edit burst.
- **The ≤50ms p95 full-cycle bar is met on the target file with its real include graph on real disk** — already demonstrated by the spike at ~15ms p95 (raw) / ~15ms p95 (coarse salsa); the v1 skeleton must keep it there, guarded by a real-disk benchmark.
- LSP diagnostics for a given file match `oxabl analyze`/`check` output for that file (same engine, same results).
- No false-positive `undefined-symbol` flood from include-resident code once preprocessing is on.

### Known Limitations (accepted for v1)

- Opening a bare `.i` include directly: best-effort parse with `undefined-symbol` noise **suppressed** (an include is not standalone-parseable). Known limitation.
- No cross-buffer invalidation: editing an include does not re-lint other open buffers in the same session.
- Single open buffer is the unit of analysis; no workspace-wide diagnostics.
- Diagnostics only — no navigation/completion/hover.

### Approaches Considered (revised post-spike)

The caching question that framed the original A/B/C is now answered by measurement: the bar is met raw, and preprocessing is not the bottleneck. The live decision has shifted to **what substrate v1 runs on**:

- **A — Raw pipeline, no substrate.** Ship the existing pure-function pipeline behind the debounce; no caches, no salsa. *Pros:* zero new machinery, 15ms p95 (3x under bar), strict YAGNI. *Cons:* leaves a salsa migration for later if the workload grows; no framework for future incremental features (hover/completion). *Best when* you want the smallest possible v1 and are willing to adopt salsa later on evidence.
- **B — Coarse salsa now (CHOSEN).** Adopt salsa 0.28 as the substrate with per-file tracked queries. *Pros:* proven cheap to stand up (spike), composes cleanly with `lsp-server`, no crate-entry-point changes, removes the future migration, gives a home for later incremental features (and enables schema hot-reload, A2). *Cons:* ~0.4ms overhead and one dependency for no *current* perf gain. **Selected by the project owner on the spike.**
- **C — Two hand-rolled caches (retired).** The original Phase-1 plan (resolved-path + mtime-content caches). *Retired:* targets a ~2ms cost that isn't the bottleneck; pure carrying cost for no meaningful gain.
- **D — No preprocessing in the LSP (rejected).** *Rejected:* reproduces the include-resident false-positive flood of issue #77.
- **Deferred — fine-grained salsa** (per-include inputs, per-procedure tracked entities). The only path that would actually *reduce* the typing-workload cycle, but the invasive one (routes include reads through salsa inputs, splits the AST into tracked entities, interacts with NodeId). Unwarranted at 3x under budget; the escalation path if a future workload demands it.

Decision: **B (coarse salsa now)** — selected by the project owner. Do not build C; do not build fine-grained salsa in v1.

### Resolved Decisions (all Track A open questions settled by the project owner, 2026-07-22)

1. **Include-change-while-buffer-idle (A1) — SETTLED.** In the coarse model includes are re-read inside the preprocess query every recompute, so a buffer's own edits pick up include changes for free — there is **no include cache to invalidate**. The only survivor: an include changes on disk while its dependent buffer sits idle (salsa does not watch the filesystem). Handling: an LSP `didChangeWatchedFiles` watcher on `*.i` files that re-`set_input`s any open buffer depending on the changed include, forcing recompute. `(mtime, size)` stat and content-hash are **not needed unless fine-grained** (only relevant if includes ever become salsa inputs).
2. **Schema staleness (A2) — SETTLED: hot-reload.** The `.df` is a salsa input; the LSP `set_input`s it on change and dependent lint queries recompute live. Schema-dependent rules (LINT0003/0004) update without restart. (See "Schema hot-reload" above.)
3. **Lint config surface (A3) — SETTLED.** A `[lint]` table in `oxabl.toml` mirroring `[style]`; typed `LintConfig`, per-rule kebab keys, 5-level `off|hint|info|warn|error` value mapping 1:1 to `DiagnosticSeverity`; safe default all-on with `undefined-symbol=error`, rest `warn`. Config surface only; #57 registry deferred; inline pragmas deferred to v2. (See "Lint Configuration Surface" above.)
4. **Benchmark protocol (A4) — SETTLED.** Two fixed-synthetic CodSpeed gates: WARM steady-state single-edit full-cycle (≤50ms p95) and COLD-open (~500ms–1s); a `finished in N` timing line for manual eyeballing; no real-edit trace; no proprietary data/PII in-repo (synthetic fixtures; real file by absolute path locally, never vendored). (See "Measurement protocol" above.)
