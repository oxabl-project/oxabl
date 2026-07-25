# Handoff: the browser WASM playground shipped (#133); next is panic-safety (#119), then uncredited reads (#128/#130)

**Date:** 2026-07-24
**Branch:** `master` — clean at `6614f10`. PR #133 merged; no open work branch from this session.
**This session:** Shipped **#133** — `crates/oxabl_wasm`, a thin `wasm-bindgen` adapter that puts the shared analysis and formatter pipelines in the browser. The playground is live and working: paste ABL, get the same lint diagnostics and the same safe formatter output the CLI and editor give, entirely client-side. This is the "try it in 10 seconds" slice off the roadmap.
**Prior context:** #129 (table-parameter FP + the LINT0006 dead-store split) shipped in the previous session; its lint-accuracy follow-up map is intact and reproduced below, because nothing in #133 touched it.

---

## Current state

| Item | Status |
|------|--------|
| #133 browser WASM adapter | **Done — merged, playground working end to end.** |
| #129 table-parameter FP + LINT0006 split | Done — merged, dogfooded (FP count down). |
| #119 panic-safe parse/format | **Open — planned, not built.** Plan at `docs/plans/2026-07-24-001-fix-panic-safe-parse-format-plan.md` (gitignored, local only); decisions and open items in `OPEN.md`. #133 made the documented panic contract a live defect: `oxabl_wasm` has no guard and a panic traps the wasm module. |
| #128 / #130 | Open — uncredited *reads*; between them they own the one remaining known FP class. Now also the playground's visible FP surface. |
| #131 / #132 | Open — LINT0006 write-site span breadth; `oxabl_lint` benchmark coverage. |
| #125 | Open and **unblocked** — small, template is fresh. |
| #124 / #126 | Open — the rest of the flow-analysis cluster. |
| #102 / #103 cross-file resolution | Open — still the **top strategic thread**, but it does nothing for the playground. |
| #57 public lint-rule API | Open — blocked on #102. |
| #120 CLI reshape onto shared pipelines | Open — the WASM crate is a third consumer, but see the ranking note under **Next**. |
| #108 unresolvable-include-as-argument | Open — deferred pending a fully-wired re-dogfood. |

---

## What shipped this session — #133

`crates/oxabl_wasm` exposes exactly two `#[wasm_bindgen]` functions, each returning a JSON string:

- `analyze_source(source) -> { diagnostics: [{ source, severity, code, message, start, end, help }] }`, where `start`/`end` are `{ byte, line, column }` resolved through `SourceMap`.
- `format_source(source) -> { source, changed, error }`.

Both delegate straight to the umbrella crate — `oxabl::analyze_with_fs` with `AnalyzeOptions::default()` over an `InMemoryFileSystem`, and `oxabl::format_source` with `StyleGuide::default_base()`. The crate contains **no ABL behavior at all**; it is a transport boundary and must stay one.

**Decisions / gotchas future sessions should know:**

- **The umbrella crate now has a `cli` feature, on by default.** `clap`, `walkdir`, `indicatif`, `schemars`, `serde_json`, and `oxabl_lsp` are optional dependencies gated behind it, and the `oxabl` binary carries `required-features = ["cli"]`. `oxabl_wasm` depends on `oxabl` with `default-features = false, features = ["serde"]`. **Any new native-only dependency added to `crates/oxabl` must go behind `cli` or the wasm build breaks.** The new CI job is what catches this, so don't ignore it.
- **CI gained a `WebAssembly client` job** that runs `cargo build -p oxabl_wasm --target wasm32-unknown-unknown --release`. It builds only — no tests execute on the wasm target. The crate's three unit tests run natively under `cargo test --workspace`, which is enough because the crate is pure translation.
- **The release workflow now packages the browser artifact**: `./scripts/build-wasm.sh target/wasm-web`, tarred and uploaded to the GitHub Release as `oxabl-wasm-web.tar.gz`. `wasm-bindgen-cli` is pinned to **0.2.108** and must match the `wasm-bindgen` crate version — a mismatch fails at bindgen time, not build time.
- **`--no-typescript`** is passed to `wasm-bindgen`, so consumers get no `.d.ts`. Deliberate for the MVP (the wire shape is JSON strings, not typed objects), but it is the obvious next ergonomic step if the website grows.
- **The MVP's absent capabilities are absent on purpose, not stubbed.** No include resolution (empty in-memory FS), no `.df` schema (so LINT0003 is inert), no `oxabl.toml` (so per-rule severity config and `[workspace.style]` do not apply). The rule is: a project capability the browser can't honestly provide stays *unavailable* rather than getting a second, divergent implementation in the wasm layer.
- **A formatter bail is a first-class result, not an error path.** On any `Err`, the response returns the *original* source with `changed: false` and the message in `error` — the same never-mangle contract the LSP honors by returning no edits. A test pins this.
- **The website is a separate, static consumer.** It serves the released artifact plus the UI around it; the Oxabl repo owns the build and versioning. Keep browser-side product logic out of this repo, and keep ABL logic out of the website.
- **The wire shape is not a stable contract**, same as `--json` on `check`/`analyze`. It converges into #120's shared-pipeline work; don't let a website expectation freeze it prematurely.

**Verification:** `cargo test --workspace` green — **1485 tests, 0 failures** — plus `cargo clippy --workspace --all-targets -- -D warnings` and `cargo fmt --check`. CI green including the new wasm job, and the generated JS/WASM package was smoke-tested in Node before merge. All fixtures synthetic.

---

## Lint-accuracy map, carried forward from #129

Untouched by this session; read this before triaging any "LINT0006 is wrong" report.

**A large set of ABL statements is invisible to the resolve pass**, so they credit no reads: the parser skips them to `StatementKind::Empty`. `PUT`, `EXPORT`, `UPDATE`, `SET`, `PROMPT-FOR`, `GET-KEY-VALUE`, `IMPORT`, `COPY-LOB`, `HIDE` and more (the skip list in `oxabl_parser/src/parser/statements.rs` is authoritative). A variable whose only read lives in one of them looks write-only:

```abl
v-total = 42.
PUT v-total.        /* real read the model cannot see → false LINT0006 */
```

This is a property of the semantic model, not of the rule, and it **predates** #129 — the same variable already warned as unused on the prior master. Severity stayed `warn` deliberately: demoting LINT0006 would not fix the class (the identical FP stays loud under LINT0002) and would quietly demote every genuine dead store.

**#128** covers crediting reads in the parser-skipped forms; prefer the cheap avenue — have `skip_to_statement_end` record the identifier tokens it skips and best-effort-resolve them as `AccessMode::Read`. Over-crediting reads yields only false *negatives* for these rules, which is the safe direction, and keeps the parser change small. Do not let that issue default to full per-form statement parsing. **#130** is the sibling: `DEFINE BUFFER b FOR tt`, `EMPTY TEMP-TABLE tt` and `DEFINE QUERY q FOR tt` *are* parsed but credit no read to the table.

Other #129 facts worth keeping:

- **`FOR EACH tt:` declares a fresh block-scoped buffer symbol and credits its reads there**, not to the `DEFINE TEMP-TABLE`. Those block scopes are *descendants* of a parameter's scope and invisible to an ancestor walk, so `backing_read_count` sums reads across ancestor-or-self **and** descendant `Buffers` bindings. Any change here must keep the descendant half.
- The backing-table matching is **name-keyed, not identity-keyed, deliberately**. The imprecision only ever produces silence, never a false claim. The shadowing case still has no test.
- **`is_table_like_param` is deliberately outside the shared `is_skipped` predicate** in `rules/unused_symbol_shared.rs` — LINT0006 skips those symbols, LINT0002 must still report a genuinely-unused one. A future third rule in this family must call both.
- **`NodeId::DUMMY`**: `Expression::new` carries `DUMMY`, which the `references` side table silently drops. Any test asserting a write-site span must use the `ident_expr` helper that allocates real ids.
- **Process lesson (recorded in #128):** for any count-gated rule, audit **both** sides of the predicate. #129's audit covered the write side it incremented; the false positives came from the unaudited read side.

### Cheap test additions still worth picking up

| Test | Why it matters |
|------|----------------|
| A same-named `DEFINE BUFFER tt FOR <other-table>` shadowing the parameter's name | Pins the name-keyed imprecision as a deliberate choice; currently unpinned either way. |
| `unused-variable = "off"` with `assigned-but-never-read` left at default | The exact config that regressed when LINT0006 landed, and the one combination with no test. |
| A dead store inside an `ON` trigger or `TRIGGER PROCEDURE` body | LINT0006's walk doesn't descend those bodies, so the span falls back to the declaration. Span quality only. Related to #131. |

---

## Next

**The playground shipping reordered this list.** #133 put the single-file engine in front of strangers with `AnalyzeOptions::default()`, no schema, and no includes (`crates/oxabl_wasm/src/lib.rs:82-86`), so two threads that were ordinary follow-ups became first-contact quality issues, and the top strategic thread now helps everything *except* the newest surface. Correctness before ergonomics: fix what the demo gets wrong before reshaping how it is delivered.

1. **#119 — panic-safe parse/format entry points.** The most urgent item, and not previously on this list. `oxabl::parse` and `oxabl::format_source` ship a documented panic contract; the CLI and LSP each wrap calls in `catch_unwind`, but **`oxabl_wasm` does not — and on `wasm32-unknown-unknown` it cannot meaningfully, because a panic traps the module.** So one malformed paste doesn't render an error in the playground, it **bricks the demo until the visitor reloads the page**, with no explanation. Note the asymmetry this session already created: a formatter *bail* is handled as a first-class result (original source, `changed: false`, message in `error`), but the *panic* path is unguarded — the right instinct applied to one of the two failure modes. A `try_parse`/`try_format` variant also retires **seven** hand-rolled `catch_unwind` guards — not three, as this list originally said — and closes **three** call sites that had no guard at all, including `run_analyze` in the CLI and both of the LSP's diagnostics paths, one of which runs on the main loop where a panic kills the server. Hardening the lexer/parser to be panic-free remains the better long-term end state. Small, contained, and it protects what was just shipped.
2. **#128 + #130 — uncredited reads.** The highest-value *lint-accuracy* work and cheap relative to payoff: between them they close the last known FP class, and #128 alone improves LINT0002, LINT0005 and LINT0006 at once. Do #128 via the lexical fallback first. #133 raised the stakes — `v-total = 42. PUT v-total.` is exactly the shape a first-time visitor pastes, and the false LINT0006 is now the first impression rather than a dogfood annoyance.
3. **#125 — OUTPUT dead-store advisory.** Unblocked, small, and LINT0006's two-stage shape is a working template.
4. **#102 — workspace-wide cross-file semantic resolution** remains the top *strategic* thread (with #103 as the fast-follow). The engine analyses one file at a time, so inherited members from a parent `.cls`, `USING`-imported types, `RUN` targets, and cross-file `SHARED` vars all resolve to `Unknown`/`External` → `undefined-symbol` false positives on real OO ABL. It is the ceiling on lint effectiveness and **blocks #57**. It sits below the items above only on sequencing, not importance: it is weeks of architecture, and it does nothing for the playground, whose MVP has no workspace, includes, or PROPATH by design. Take it through `/ce-brainstorm` → `/ce-plan` before building.
5. **#120 — reshape `check`/`analyze` onto shared pipelines.** Worth doing, but the "three clients hand-map the same diagnostics" framing is the weakest part of its case: those mappings differ because the transports genuinely differ (`lsp_types::Diagnostic`, a JSON wire shape with `SourceMap`-resolved positions, rendered text), so a shared pipeline hoists the *collection* while each client still needs its final hop — that deletes some code, not a class of bugs. #120's real value has always been the **ruff-shaped `check`**, a product change #133 didn't make more urgent. Two further reasons it sits here: sequencing it before #119/#128 reshapes delivery while the diagnostics are still wrong, forcing a re-verify across all three clients afterward; and it is a `/ce-strategy`-then-plan thread in the same weight class as #102, competing for the same design attention. **The one argument that does carry weight:** the wire shape is not a stable contract, and every week the playground is live the website hardens around today's shape — a real closing window, but one this repo controls. Consider folding it into #102's strategy pass, since both want one.
6. **#126 — CFG + dataflow scaffolding** still absorbs and retires `PASSED_AS_OUTPUT_ARG` and `PARAM_TABLE_LIKE`, and #124 waits on it. Check #126 before starting #131 — widening LINT0006's write-site walk form-by-form is exactly the per-shape treadmill def-use records exist to end.
7. **#132 — `oxabl_lint` benchmarks.** Still the only crate with no bench target, so no rule's cost is measured and CodSpeed can't catch a regression in any of them.
8. **Playground follow-ups worth filing** (none filed yet): TypeScript typings for the wasm package (`--no-typescript` today); a wasm bundle-size budget in CI so the demo's load cost can't silently balloon; and deciding whether the second browser slice adds schema upload / a synthetic include map or stays deliberately single-file. Panic safety belonged on this list and is now item 1 as #119.
9. **Re-dogfood in a fully-wired workspace** (include paths + `.df` schema in `oxabl.toml`) to separate real bugs from config noise; then confirm/close **#108** and re-check the held block-scope false positive.
10. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#133** | **Merged** — browser WASM adapter + playground (this session) |
| **#119** | Open — **panic-safe parse/format; now the most urgent item.** A panic traps the wasm module, so it bricks the playground until reload |
| **#128 / #130** | Open — uncredited reads (parser-skipped statements / table-use forms); the last known FP class, and the playground's visible FP surface |
| **#102 / #103** | Open — cross-file resolution + background index (**the top strategic thread**; sequenced after the two above, and no help to the playground) |
| #131 | Open — widen LINT0006's write-site walk beyond assignment and `ASSIGN` targets |
| #132 | Open — `oxabl_lint` has no benchmark coverage at all |
| #125 | Open — **unblocked**; callee-written dead-store advisory |
| #124 / #126 | Open — path-aware LINT0005 and the CFG scaffolding that retires both stopgap flags |
| #120 | Open — reshape `check`/`analyze` onto shared lint/format pipelines; three client consumers, but the dedup argument is weaker than the ruff-shaped-`check` one (see **Next** item 5) |
| #129 | Merged — table-parameter FP + LINT0006 dead-store split |
| #127 | Merged — LINT0002 OUTPUT-argument FP; #129 builds on its flag and supersedes its `write_count` note |
| #121 / #122 / #123 | Merged — preprocessor define-time refs, routine-scoped `DEFINE VARIABLE`, LINT0005 |
| #113 / #114 / #115 / #116 | Merged — the four #55 public-API waves |
| #55 | Improve the public API — done across the four waves; can be closed |
| #117 / #118 | Filed — deferred #55 follow-ups (#119 was one of these; #133 promoted it, see above) |
| #104 | Merged — VS Code extension + `oxabl schema` + CI (the dogfood loop) |
| #57 | Open — public lint-rule API; blocked on #102 |
| #108 | Open — unresolvable-include-as-argument → misleading comma error (deferred) |
| #56 | Open — dependency-extraction fidelity vs AVM (converges with #102) |
| `STRATEGY.md` | Browser try-it-out and Public API & client architecture tracks cover this session |
