# Handoff: LINT0002 OUTPUT-argument false positive fixed (#127); next is cross-file resolution (#102)

**Date:** 2026-07-24
**Branch:** `master` — clean at `a6eabb7`. PR #127 merged; no open work branch from this session.
**This session:** Fixed the **LINT0002 `OUTPUT`-argument false positive (#127)** — a variable used only as an `OUTPUT` argument to a `RUN` was reported unused. Dogfooded after merge: the false positive is gone and no genuine unused-variable reports were lost.
**Prior context:** #55 (public API) shipped across PRs #113–#116. Three trust fixes merged after that handoff was last written and are recorded below for the first time: #121, #122, #123.

---

## Current state

| Item | Status |
|------|--------|
| #127 LINT0002 OUTPUT-argument FP | **Done — merged, dogfooded clean.** |
| #121 / #122 / #123 | **Merged** (recorded here late — see below). |
| #124 / #125 / #126 | **Open** — the flow-analysis cluster this session's fix leans on. |
| #55 public API (Waves 1–4) | Done — PRs #113/#114/#115/#116 merged. |
| #117–#120 follow-ups | Filed (deferred #55 scope). |
| #102 / #103 cross-file resolution | Open — still the **top strategic thread**. |
| #57 public lint-rule API | Open — blocked on #102. |
| #108 unresolvable-include-as-argument | Open — deferred pending a fully-wired re-dogfood. |
| Held block-scope false positive | Partly addressed by #122/#123; re-check in a workspace that *has* includes. |

---

## What shipped this session — #127 LINT0002 OUTPUT-argument false positive

`unused-variable` fired on a variable whose only appearance was as an `OUTPUT` argument to a `RUN` — the ABL shape where a callee's signature requires an out-param the call site discards:

```abl
DEFINE VARIABLE v-error AS CHARACTER NO-UNDO.
RUN calc.p (INPUT "ctx", OUTPUT v-error).   /* warned: unused variable `v-error` */
```

Investigation narrowed the gap well below "all argument positions": every other argument position already counts as a use via `read_count`. `INPUT` args are `Read`; `INPUT-OUTPUT`/`RETURN` are `ReadWrite`; function/method/procedure args are all walked as `Read`. Only `OUTPUT` bumps `write_count` alone, and the rule keys on `read_count == 0`.

**The fix:** the resolve pass records `SymbolFlags::PASSED_AS_OUTPUT_ARG` (`1 << 19`) for bare-identifier write-back (`OUTPUT`/`INPUT-OUTPUT`/`RETURN`) `RUN` arguments; `unused_variable::is_skipped` consults it. This mirrors the `READ_OUTSIDE_BLOCK`/`WRITE_OUTSIDE_BLOCK` precedent exactly — a resolve-computed usage fact in the existing per-symbol `u32`, accumulated in a walker-local `FxHashSet<SymbolId>` and flushed once at pass end. No new `Symbol` field (keeps `Symbol` lean, per commit 82e5568) and no new side table on `Semantic`.

**Decisions / gotchas future sessions should know:**

- **`write_count > 0` is not a usable signal and never will be.** A plain local assignment (`x = 1`) also bumps it, and `assign_counts_as_write_but_not_read_warns` pins that an assigned-but-never-read variable must still warn. The distinction between "written by a callee via an OUTPUT argument" and "assigned locally" did not exist in the model and had to be introduced. Any future attempt to shortcut this with `write_count` will silently break that contract.
- **The hook reuses the resolution `walk_expression` just recorded** — it looks up `self.references.get(arg.expression.id)` for `Resolution::Resolved(sym)` rather than resolving twice. Non-identifier and unresolved arguments are a natural lookup miss and a no-op.
- **`NodeId::DUMMY` silently swallows reference inserts** (`index_vec.rs:53-57`). Hand-built AST via `Expression::new` carries `DUMMY`, so any unit test whose assertion depends on resolution readback must use `Expression::with_id` with a real id. The first pass of these tests passed for the wrong reason until this was caught. This trap applies to **any** future test touching the `references` side table, not just this rule.
- **`RETURN` is accepted defensively.** The AST models the direction, but `parse_run_arguments` (`oxabl_parser/src/parser/statements.rs`) never produces `Return` for a `RUN` argument, so only `OUTPUT` and `INPUT-OUTPUT` are reachable from real source today. The `RETURN` branch and its unit test are correct-but-unreachable; don't document it as user-facing behavior.
- **Non-identifier write-back lvalues are deliberately not covered.** `OUTPUT arr[i]`, member/qualified targets, and similar resolve the underlying symbol as a `Write` and so share the OUTPUT-only shape, but the bare-identifier hook does not flag them — they may still warn. Covering each shape one at a time is the per-shape treadmill #126 exists to end. Whole-array `OUTPUT arr` **is** covered.
- **Method/function-call OUTPUT directions are not modeled in v1** — `MethodCall`/`FunctionCall` arguments are `Vec<Expression>` with no per-argument direction, so they're walked as `Read` and already spare the variable. No false positive exists there today.
- **The flag is a stopgap with a named exit.** #125 (dead-store advisory) builds on it; #126's CFG def-use records absorb it and delete the standalone bit. The flag's doc comment carries both cross-references — keep them accurate if the bit moves.
- **`oxabl_analyze`'s `symbol_flags_list` does not emit the new flag** — nor does it emit `READ_OUTSIDE_BLOCK`/`WRITE_OUTSIDE_BLOCK`. Pre-existing gap, not a regression, but it means a suppression reason is invisible in the analyze dump. Worth closing when someone next touches that dump.

**Verification:** built proof-first — the integration test was written and observed failing against the current rule before either implementation unit landed. `cargo test --workspace`, `cargo clippy --workspace --all-targets -- -D warnings`, `cargo fmt --check` all green; CI green on all seven checks. Tests span three layers: resolve-pass unit tests (each direction sets/doesn't set the flag; non-identifier and unresolved args are no-ops), rule unit tests, and a new integration file `crates/oxabl_lint/tests/lint0002_output_argument.rs` driving synthetic ABL through lex → parse → analyze → lint. That file includes a **sibling-scope case using the same variable name on both sides**, which pins that the exemption is keyed on `SymbolId` and not on the name — a name-keyed mis-attribution would pass every distinct-name discrimination test. All fixtures synthetic; no corpus, no PII.

**Known coverage gap:** `crates/oxabl_semantic/benches/semantic_bench.rs` has no `RUN` statement in its fixtures, so CodSpeed does not exercise the new path. Impact is expected to be negligible (one `mode != Read` comparison, then an O(1) `NodeIndexVec` index and a set insert, only on write-back RUN arguments), but the fixture gap is real if this path ever grows.

---

## Recorded late — #121, #122, #123

These merged after the previous handoff was written and were never captured in it. Summarized from commit history and the current `CLAUDE.md` status section, not from having run those sessions — treat the detail as thinner than the #127 notes above.

| PR | Change |
|----|--------|
| #121 | `fix(preprocessor)`: resolve named `{&…}` references inside define values **at define time** rather than at use. |
| #122 | `fix(semantic)`: scope `DEFINE VARIABLE` to the **routine**, not the enclosing block — the hoisting behavior LINT0005 then builds on. |
| #123 | `feat(lint)`: `block-var-used-outside` advisory (**LINT0005**, INFO) — a block-defined variable read outside its block and never assigned outside it may still hold its default value. Introduced the `READ_OUTSIDE_BLOCK`/`WRITE_OUTSIDE_BLOCK` flag pattern that #127 then mirrored. |

---

## Next

1. **#102 — workspace-wide cross-file semantic resolution** remains the top strategic thread (with #103 background index as the fast-follow). The engine analyses one file at a time, so inherited members from a parent `.cls`, `USING`-imported types, `RUN` targets, and cross-file `SHARED` vars all resolve to `Unknown`/`External` → `undefined-symbol` false positives on real OO ABL. #102 is the ceiling on lint effectiveness and **blocks #57**. Genuine architecture (cross-file salsa graph, class/inherited-member index, includes-as-tracked-inputs with an expansion cache, invalidation model, AVM-parity-vs-explicit-"unknown" decision) — take it through `/ce-brainstorm` → `/ce-plan` before building.
2. **#126 — CFG + dataflow scaffolding** is now the second real thread, and it has two named consumers waiting: #124 (path-aware LINT0005) and #125 (OUTPUT dead-store advisory). #127's flag is explicitly a stopgap that #126 folds in and deletes. Worth planning once #102's shape is understood, since both touch the semantic model.
3. **Re-dogfood in a fully-wired workspace** (include paths + `.df` schema in `oxabl.toml`) to separate real bugs from config noise; then confirm/close **#108** and re-check the held "visible-earlier / undefined-later" block-scope false positive, which #122/#123 may have already resolved.
4. **#120** — when ready to reshape the CLI into a lint/format-first tool, do a `/ce-strategy` pass then a plan.
5. **Deferred client work (from #104's plan):** parser-driven syntax highlighting via LSP semantic tokens, quick-fix code actions to toggle a rule in `oxabl.toml`, server-side `oxabl.toml` validation diagnostics, and Marketplace publish (publisher identity, icon, CI publish).

---

## Related

| Issue/PR | Relation |
|----------|----------|
| **#127** | **Merged** — LINT0002 OUTPUT-argument false positive (this session) |
| **#125 / #126** | Open — dead-store advisory and CFG scaffolding; #127's flag is the stopgap they retire |
| #124 | Open — path-aware LINT0005; sibling consumer of #126 |
| #121 / #122 / #123 | Merged — preprocessor define-time refs, routine-scoped `DEFINE VARIABLE`, LINT0005 |
| #113 / #114 / #115 / #116 | Merged — the four #55 public-API waves |
| #55 | Improve the public API — done across the four waves; can be closed |
| #117 / #118 / #119 / #120 | Filed — deferred #55 follow-ups (AST serde/Display, schema auto-discovery, panic-catching parse, shared-pipeline CLI redesign) |
| #112 | Merged — #60 field read/write counts + clippy housekeeping |
| #104 | Merged — VS Code extension + `oxabl schema` + CI (the dogfood loop) |
| **#102 / #103** | Open — cross-file resolution + background index (**the strategic thread, next**) |
| #57 | Open — public lint-rule API; blocked on #102 |
| #108 | Open — unresolvable-include-as-argument → misleading comma error (deferred) |
| #56 | Open — dependency-extraction fidelity vs AVM (converges with #102) |
| `STRATEGY.md` | Public API & client architecture track; the umbrella is the shared client surface |
