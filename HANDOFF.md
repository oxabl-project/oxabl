# Handoff: table-use forms now credit their table (#130); the remaining uncredited-read half is #134

**Date:** 2026-07-29
**Branch:** `evanbrobertson/semantic-credit-table-reads-in-define-buffer-emp`, ahead of `master`.
**This session:** Shipped **#130** — the five statement forms that name a table without reading a field of it now credit a read on that table, removing the residual LINT0002 false positive on a `TABLE FOR tt` parameter whose temp-table is used only that way. Details under the lint-accuracy map below.
**Prior context:** #133 (browser WASM playground), #135 (panic-safe entry points, closing #119), and #137 (#128's unmodelled-statement crediting) all shipped in preceding sessions; #128's follow-up map is reproduced below because #130 builds directly on its carrier.

---

## Current state

| Item | Status |
|------|--------|
| #133 browser WASM adapter | **Done — merged, playground working end to end.** |
| #129 table-parameter FP + LINT0006 split | Done — merged, dogfooded (FP count down). |
| #128 | **Done — standalone unmodelled forms credited** (`StatementKind::Skipped` + `TOUCHED_BY_UNMODELLED_STATEMENT`). Bounded triage; #136 is the scheduled drain. |
| #130 | **Done — table-use forms credited.** All five shapes (`DEFINE BUFFER`, `DEFINE PARAMETER BUFFER`, `EMPTY TEMP-TABLE`, `DEFINE QUERY`, `OPEN QUERY`) now credit a read on the table they name. |
| #134 | Open — the last uncredited-read half: skipped *tails* inside modelled statements. |
| #136 | Open and **scheduled** (`hermes`) — head-parse the unmodelled forms; retires #128's flag and #130's query approximation. |
| #119 panic-safe parse/format | **Done — shipped as #135** (`catch_panic` + browser crash recovery). |
| #131 / #132 | Open — LINT0006 write-site span breadth; `oxabl_lint` benchmark coverage. |
| #125 | Open and **unblocked** — small, template is fresh. |
| #124 / #126 | Open — the rest of the flow-analysis cluster. |
| #102 / #103 cross-file resolution | Open — still the **top strategic thread**, but it does nothing for the playground. |
| #57 public lint-rule API | Open — blocked on #102. |
| #120 CLI reshape onto shared pipelines | Open — the WASM crate is a third consumer, but see the ranking note under **Next**. |
| #108 unresolvable-include-as-argument | Open — deferred pending a fully-wired re-dogfood. |

---

## What shipped this session — #130

Five statement forms name a table without reading a field of it, so nothing in the expression walk ever saw them and the backing symbol's `read_count` stayed at zero. That is what made LINT0002's table-parameter redirect ask the right symbol and still get the wrong answer: a `TABLE FOR tt` parameter whose temp-table was used only that way got reported as unused.

| Form | How it credits now |
|---|---|
| `DEFINE BUFFER b FOR tt.` | Direct AST resolution of the target |
| `DEFINE PARAMETER BUFFER b FOR tt.` | Same, via `ParameterType::Buffer` |
| `EMPTY TEMP-TABLE tt.` | Marked `Skipped` carrying its exactly-parsed table name |
| `DEFINE QUERY q FOR tt.` | Marked `Skipped`, lexical harvest |
| `OPEN QUERY q FOR EACH tt.` | Marked `Skipped`, lexical harvest |

**Decisions / gotchas future sessions should know:**

- **`StatementKind::Skipped` gained `may_reference_tables`.** A marked node keeps #128's value-namespace treatment *unchanged* and additionally resolves the same names in `[Buffers, Tables]` as `AccessMode::Read`. The two paths are independent on purpose: a token can resolve in both namespaces under shadowing, and they record different facts — the value side records that counts cannot be judged, the table side records a real read. Only three forms set the marker, so ordinary #128 forms pay no extra namespace walk.
- **The same-name guard is load-bearing.** `DEFINE BUFFER Customer FOR Customer.` is the standard ABL block-scoping idiom, and the declare pass has already bound the new buffer under that folded name. An unguarded lookup resolves the target to the buffer being declared and credits it a read *for existing*, which would silence every count-gated rule for that symbol.
- **Both credit paths go through `resolve_statement_ident`**, which is silent on a miss. That is what lets the deliberately over-inclusive query harvest run without creating a `references` entry or an `undefined-symbol` diagnostic for every stray token.
- **The query forms stay lexically harvested until #136 head-parses them**, so every identifier inside a `DEFINE QUERY` / `OPEN QUERY` is a table candidate. Over-crediting can silence a diagnostic but cannot invent one — the same conservative direction #128 chose, but bounded to two forms.
- **A bare *schema* table is still credited by nobody.** `synth_table_buffer_symbol` inserts into the `SymbolTable` without binding into the `ScopeTree`, and nothing declares into `NamespaceId::Tables` at all — so `DEFINE BUFFER bCust FOR Customer.` under a loaded schema credits nothing, exactly as `FIND` and `FOR EACH` leave it today. Deliberately out of scope; that boundary now has a test (`schema_only_targets_retain_current_no_credit_behavior`) rather than being folklore.
- **The parser accepted only the *invalid* spelling of a buffer parameter.** Buffer parameters carry no direction in ABL — the buffer binds to the caller's — but only `DEFINE INPUT PARAMETER BUFFER b FOR tt` parsed, and the valid directionless `DEFINE PARAMETER BUFFER b FOR tt` was a parse error. Fixed here: an end-to-end pin against a source form real code never contains proves nothing. Both spellings now produce the same node.
- **A comment between `OPEN` and `QUERY` defeated the marker** in the first cut, because the split used a raw one-token lookahead while every other keyword boundary in the parser tolerates an interleaved comment. Caught in review; it now uses `peek_nth_non_comment`. Worth remembering as a class: a new lookahead in this parser should assume a comment can sit in the gap.

**Follow-ups this deliberately did not do:**

1. **Whether an unused buffer symbol deserves its own diagnostic.** Crediting a buffer definition's target removes a table-parameter false positive, but a buffer that is bound and never used stays silent under the current rule set — `is_candidate` excludes `Buffer`/`TempTable`, so nothing can pick it back up.
2. **Binding synthesized schema default buffers into the scope model**, so statement-position table references can credit schema tables consistently. Broader than #130 because it also changes `FIND` and `FOR EACH`, with visible `oxabl analyze` consequences.
3. **#136 retires the query approximation** by head-parsing those forms, at which point `may_reference_tables` narrows to whatever is left.

**Found and filed, not fixed:** `ParameterType::Buffer` records the wrong target in a procedure signature. `crates/oxabl_parser/src/parser/mod.rs:1567` — the inline parameter-list path (`PROCEDURE p (BUFFER b FOR tt)`) parses the table name and then discards it with `.ok()`, setting `target` to the *buffer's own name*. Two consequences: the declare pass calls `schema_table_id` on the buffer name, so that shape never links its schema table; and #130's new credit is skipped by the same-name guard, so it credits nothing. Pre-existing and distinct from #130.

**Verification:** `cargo test --workspace` green — **1581 tests, 0 failures** — plus `cargo clippy --workspace --all-targets -- -D warnings` and `cargo fmt --check`. All fixtures synthetic.

---

## Carried forward: the browser WASM adapter (#133)

`crates/oxabl_wasm` exposes exactly two `#[wasm_bindgen]` functions, each returning a JSON string:

- `analyze_source(source) -> { diagnostics: [{ source, severity, code, message, start, end, help }] }`, where `start`/`end` are `{ byte, line, column }` resolved through `SourceMap`.
- `format_source(source) -> { source, changed, error }`.

Both delegate straight to the umbrella crate — `oxabl::analyze_with_fs` with `AnalyzeOptions::default()` over an `InMemoryFileSystem`, and `oxabl::format_source` with `StyleGuide::default_base()`. The crate contains **no ABL behavior at all**; it is a transport boundary and must stay one.

**Decisions / gotchas future sessions should know:**

- **The umbrella crate now has a `cli` feature, on by default.** `clap`, `walkdir`, `indicatif`, `schemars`, `serde_json`, and `oxabl_lsp` are optional dependencies gated behind it, and the `oxabl` binary carries `required-features = ["cli"]`. `oxabl_wasm` depends on `oxabl` with `default-features = false, features = ["serde"]`. **Any new native-only dependency added to `crates/oxabl` must go behind `cli` or the wasm build breaks.** The new CI job is what catches this, so don't ignore it.
- **CI gained a `WebAssembly client` job** that runs `cargo build -p oxabl_wasm --target wasm32-unknown-unknown --release`. It builds only — no tests execute on the wasm target. The crate's three unit tests run natively under `cargo test --workspace`, which is enough because the crate is pure translation.
- **The release workflow now packages the browser artifact**: `./scripts/build-wasm.sh target/wasm-web`, tarred and uploaded to the GitHub Release as `oxabl-wasm-web.tar.gz`. `wasm-bindgen-cli` is pinned to **0.2.126** and must match the `wasm-bindgen` crate version — a mismatch fails at bindgen time, not build time. The crate is pinned **exactly** (`=0.2.126`, not a caret range) because browser crash recovery rides on `__wbg_reset_state`, which is generated glue rather than a semver-stable API: an exact pin stops a routine `cargo update` inside 0.2.x from moving that machinery underneath the exact-pinned CLI. Three pin sites move together — the crate, `scripts/build-wasm.sh`, and `.github/workflows/release.yml`.
- **`--no-typescript`** is passed to `wasm-bindgen`, so consumers get no `.d.ts`. Deliberate for the MVP (the wire shape is JSON strings, not typed objects), but it is the obvious next ergonomic step if the website grows.
- **The MVP's absent capabilities are absent on purpose, not stubbed.** No include resolution (empty in-memory FS), no `.df` schema (so LINT0003 is inert), no `oxabl.toml` (so per-rule severity config and `[workspace.style]` do not apply). The rule is: a project capability the browser can't honestly provide stays *unavailable* rather than getting a second, divergent implementation in the wasm layer.
- **A formatter bail is a first-class result, not an error path.** On any `Err`, the response returns the *original* source with `changed: false` and the message in `error` — the same never-mangle contract the LSP honors by returning no edits. A test pins this.
- **The website is a separate, static consumer.** It serves the released artifact plus the UI around it; the Oxabl repo owns the build and versioning. Keep browser-side product logic out of this repo, and keep ABL logic out of the website.
- **The wire shape is not a stable contract**, same as `--json` on `check`/`analyze`. It converges into #120's shared-pipeline work; don't let a website expectation freeze it prematurely.

**Verification:** `cargo test --workspace` green — **1485 tests, 0 failures** — plus `cargo clippy --workspace --all-targets -- -D warnings` and `cargo fmt --check`. CI green including the new wasm job, and the generated JS/WASM package was smoke-tested in Node before merge. All fixtures synthetic.

---

## Lint-accuracy map, carried forward from #129

Untouched by this session; read this before triaging any "LINT0006 is wrong" report.

**Standalone unmodelled forms are now credited (#128, done).** Around thirty ABL statement forms are recognized by the parser and then discarded — `PUT`, `EXPORT`, `UPDATE`, `SET`, `PROMPT-FOR`, `ENABLE`, `GET-KEY-VALUE`, `IMPORT`, `COPY-LOB`, `HIDE`, embedded SQL and more (the skip list in `oxabl_parser/src/parser/statements.rs` is authoritative). They used to reach `StatementKind::Empty` and credit nothing, so a variable whose only read lives in one of them looked write-only:

```abl
v-total = 42.
PUT v-total.        /* real read the model could not see → false LINT0006 */
```

Those forms now emit `StatementKind::Skipped { names }` carrying the identifier tokens the skip passed over. The resolve pass best-effort-resolves them in `NamespaceId::Values` through `lookup_statement_ident`, a lookup that writes no side table, and records hits as `SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`. All three count-gated rules — LINT0002, LINT0005, LINT0006 — consult that flag through the one shared `is_skipped` predicate. `read_count` and `write_count` stay exact; the flag says the counts are *incomplete*, it does not fabricate an access.

**Know what this cost.** The mark is per-symbol and file-wide, so a genuine dead store on line 40 becomes unjudgeable because of an `ENABLE` mention on line 900. That is real evidence destruction, not just caution, and it is why `oxabl analyze` now prints a count of symbols it could not fully judge (stderr, plus an `unjudged_symbols` JSON field) rather than letting a partly-checked file look clean. It is also why the retirement path is **scheduled**, not wished for.

**#136 is the actual fix, and it is filed and labelled.** Variable traffic lives in each statement's *head*; the awkwardness lives in its *tail*. One shared parse-head/skip-tail combinator covers the large majority, and head-parsing a form buys exact lint crediting, formatter coverage (`tree.rs` routes these through pass-through arms today) and future LSP fidelity from one piece of work — where the flag buys only the first, once. `StatementKind::Skipped` doubles as the instrument for prioritizing: count which dispatch keywords actually produce `Skipped` nodes on real code and work the list in frequency order.

**One sibling remains.** **#134** — skipped *tails* inside modelled statements (`DISPLAY … WITH FRAME` options, `DEFINE VARIABLE … VIEW-AS`, `RUN` trailing content, and ten more): same root cause, but the statement node is already occupied so neither #128's nor #130's payload shape transfers. Pinned by a deliberately-failing-later test in `crates/oxabl_lint/tests/issue128_skipped_statement_reads.rs`.

**#130 shipped (this session)** and closes the table half: the five forms that name a table without reading a field of it now credit a read on it. Mechanics, the deliberate limits, and the follow-ups are in **What shipped this session** above — read that before touching `may_reference_tables` or the buffer-target guard.

Other #129 facts worth keeping:

- **`FOR EACH tt:` declares a fresh block-scoped buffer symbol and credits its reads there**, not to the `DEFINE TEMP-TABLE`. Those block scopes are *descendants* of a parameter's scope and invisible to an ancestor walk, so `backing_read_count` sums reads across ancestor-or-self **and** descendant `Buffers` bindings. Any change here must keep the descendant half.
- The backing-table matching is **name-keyed, not identity-keyed, deliberately**. The imprecision only ever produces silence, never a false claim. The shadowing case still has no test.
- **`is_table_like_param` is deliberately outside the shared `is_skipped` predicate** in `rules/unused_symbol_shared.rs` — LINT0006 skips those symbols, LINT0002 must still report a genuinely-unused one. A future rule in this family must call both. `is_skipped` now has three callers: #128 wired LINT0005's `is_hazard` through it rather than giving that rule a parallel clause, so there is exactly one suppression path to keep correct.
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

The two items that headed this list — **#119** (panic safety) and **#128 + #130** (uncredited reads) — have all shipped, as #135, #137, and this session. Between them they closed the FP class this list called the last known one. What remains:

1. **#134 — skipped tails inside modelled statements.** The one uncredited-read half left. Same root cause as #128/#130, but the statement node is already occupied so neither carrier transfers. Pinned by a deliberately-failing-later test in `crates/oxabl_lint/tests/issue128_skipped_statement_reads.rs`.
2. **#136 — head-parse the unmodelled forms.** Scheduled (`hermes`). The real drain: it retires #128's file-wide flag *and* #130's query approximation, and picks up formatter and LSP coverage on the way.
3. **#125 — OUTPUT dead-store advisory.** Unblocked, small, and LINT0006's two-stage shape is a working template.
4. **#102 — workspace-wide cross-file semantic resolution** remains the top *strategic* thread (with #103 as the fast-follow). The engine analyses one file at a time, so inherited members from a parent `.cls`, `USING`-imported types, `RUN` targets, and cross-file `SHARED` vars all resolve to `Unknown`/`External` → `undefined-symbol` false positives on real OO ABL. It is the ceiling on lint effectiveness and **blocks #57**. It sits below the items above only on sequencing, not importance: it is weeks of architecture, and it does nothing for the playground, whose MVP has no workspace, includes, or PROPATH by design. Take it through `/ce-brainstorm` → `/ce-plan` before building.
5. **#120 — reshape `check`/`analyze` onto shared pipelines.** Worth doing, but the "three clients hand-map the same diagnostics" framing is the weakest part of its case: those mappings differ because the transports genuinely differ (`lsp_types::Diagnostic`, a JSON wire shape with `SourceMap`-resolved positions, rendered text), so a shared pipeline hoists the *collection* while each client still needs its final hop — that deletes some code, not a class of bugs. #120's real value has always been the **ruff-shaped `check`**, a product change #133 didn't make more urgent. Two further reasons it sits here: sequencing it before #119/#128 reshapes delivery while the diagnostics are still wrong, forcing a re-verify across all three clients afterward; and it is a `/ce-strategy`-then-plan thread in the same weight class as #102, competing for the same design attention. **The one argument that does carry weight:** the wire shape is not a stable contract, and every week the playground is live the website hardens around today's shape — a real closing window, but one this repo controls. Consider folding it into #102's strategy pass, since both want one.
6. **#126 — CFG + dataflow scaffolding** still absorbs and retires `PASSED_AS_OUTPUT_ARG` and `PARAM_TABLE_LIKE`, and #124 waits on it. Check #126 before starting #131 — widening LINT0006's write-site walk form-by-form is exactly the per-shape treadmill def-use records exist to end.
7. **#132 — `oxabl_lint` benchmarks.** Still the only crate with no bench target, so no rule's cost is measured and CodSpeed can't catch a regression in any of them.
8. **Playground follow-ups worth filing** (none filed yet): TypeScript typings for the wasm package (`--no-typescript` today); a wasm bundle-size budget in CI so the demo's load cost can't silently balloon; and deciding whether the second browser slice adds schema upload / a synthetic include map or stays deliberately single-file. Panic safety belonged on this list and shipped as #135.
9. **Re-dogfood in a fully-wired workspace** (include paths + `.df` schema in `oxabl.toml`) to separate real bugs from config noise; then confirm/close **#108** and re-check the held block-scope false positive.
10. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#130** | **This session** — table-use forms credit a read on the table they name; `Skipped` gained `may_reference_tables` |
| **#128** | **Merged** — standalone unmodelled forms credited; `StatementKind::Skipped` + `TOUCHED_BY_UNMODELLED_STATEMENT` |
| **#134** | Open — the one remaining uncredited-read half now that #128 and #130 have both shipped; skipped tails inside *modelled* statements, same root cause but the statement node is already occupied |
| **#136** | Open, **scheduled** — head-parse the unmodelled forms; the drain that retires #128's flag and #130's query approximation |
| **#119** | **Merged as #135** — panic-safe parse/analyze/format plus browser crash recovery |
| **#133** | **Merged** — browser WASM adapter + playground |
| **#102 / #103** | Open — cross-file resolution + background index (**the top strategic thread**; sequenced after the above, and no help to the playground) |
| #131 | Open — widen LINT0006's write-site walk beyond assignment and `ASSIGN` targets |
| #132 | Open — `oxabl_lint` has no benchmark coverage at all |
| #125 | Open — **unblocked**; callee-written dead-store advisory |
| #124 / #126 | Open — path-aware LINT0005 and the CFG scaffolding that retires the two older stopgap flags (#128's third flag drains via #136 instead) |
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
