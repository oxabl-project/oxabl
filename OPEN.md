# Open items — #119 panic-safe parse/format

**Date:** 2026-07-24
**Branch:** `evanbrobertson/119-panic-safe-parse-format`
**Status:** planned, not implemented. No code written.

**Nothing is blocking.** All three of the original open questions are resolved, and the review gap flagged in the first pass has been closed. What remains below is the record of what was decided and why.

---

## Where the plan is

`docs/plans/2026-07-24-001-fix-panic-safe-parse-format-plan.md` — 16 requirements, 9 units, two repos.

**Not in this branch, by decision.** `docs/plans/` is gitignored (`.gitignore:6`) under the repo's artifact policy; plans are point-in-time and go stale. Confirmed to leave it local rather than force-add. Durable decisions migrate to `docs/solutions/` after the work lands.

---

## The plan changed shape twice during review

Both changes came from review, not from the original research, and both are worth knowing before reading the plan.

### 1. The recovery mechanism flipped

The plan originally used `--force-enable-abort-handler` + `set_on_abort` → `schedule_reinit()`. **It now uses `--experimental-reset-state-function` and calls `__wbg_reset_state()` from the playground's existing `catch` block.**

Why: the abort-handler flag injects `try_table`/exnref instructions and two `WebAssembly.Tag` imports, which raises the browser floor to roughly Chrome 128 / Firefox 131 / Safari 18.4 — below which the module fails to *instantiate*. The plan was accepting a deterministic demo loss for some visitors in order to recover from a panic no known input produces.

Verified in the 0.2.126 generator source that the cost is avoidable:
- `transforms/mod.rs:66-74` — `detect_exception_handling_version` matches `(has_try_table, has_try, enable_abort_handler)`. Only the abort-handler arm injects anything.
- `js/mod.rs:352` — `generate_reinit` is `aux.uses_reinit || config.generate_reset_state`, independent of the abort handler.

So reset-state gives the identical reinit machinery with **no** injection and **no** floor change. It's also simpler: recovery becomes one explicit call in a `catch` that already exists, and the plan no longer depends on the `#[doc(hidden)]` `handler` module.

Two independent reviewers raised this separately and the source confirmed it.

### 2. The Definition of Done split into two increments

The native track (U3, U4) now ships **alone**, without waiting on the browser track. The most concretely damaging gap in the whole plan is an unguarded panic on the LSP main loop — it kills the language server for the primary user, the in-editor daily dogfooder. That fix needs no dependency bump, no second repo, and no manual browser check, and gating it behind them would stall editor protection on browser-track risk.

---

## Resolved questions — do not re-litigate

- **Does the abort handler fire on stable `panic=abort`?** Yes, at 0.2.125+. The contradicting claim in wasm-bindgen's `src/handler.rs` module doc is stale — it predates the flag. **But the mechanism was rejected anyway** on the browser-floor cost above.
- **Does `--experimental-reset-state-function` avoid the exception-handling injection?** Yes — see the source citations above. This is why the mechanism flipped.
- **Does the panic hook's JS call complete before the trap?** Yes, guaranteed by program order on every engine. Std runs the hook to completion before aborting.
- **Does `#[wasm_bindgen(start)]` re-run after `__wbg_reset_state()`?** Yes, unconditionally — `js/mod.rs:4444` calls `wasm.__wbindgen_start()` as the reset's last step. The panic hook is a static, so it resets with the instance and is re-armed before anything else touches the module. No `reinstall()` export needed. Corroborating: `set_on_reinit` was *removed* in 0.2.118 because the start re-run subsumes it.

  Caveat that survived: **upstream has no regression test for this property** (its `termination_reinit` tests register from an explicit export, not a start function). That's why U5 keeps the manual crash-recover-crash check.

- **Should `version()` carry a wire-shape revision?** No. A build identifier answers "which artifact," which is what R13 needs. A wire-shape revision presupposes a versioning scheme for a contract this plan leaves explicitly unfrozen; adding one now would pre-commit. Revisit under #120.

---

## Review coverage — now complete

Six personas plus an independent cross-model pass. **31 findings applied in total.**

| Pass | Findings | Notable |
|---|---|---|
| feasibility | 11 | `try_analyze` can't live in `oxabl_analyze`; the proposed `AnalyzeResponse.error` field could never be non-null |
| coherence | 5 | U3/U4 sequencing contradiction; R9 owned by two units |
| scope-guardian | 2 | rejection-caching fix had no requirement trace |
| adversarial | 8 | the mechanism flip; premise rests on an unreachable defect; browser tests had no panic trigger |
| product-lens | 4 | the mechanism flip (independently); DoD coupling the native fix to the browser bet |
| design-lens | 3 | clearing `diagnostics` surfaces the *cheerful green* empty state next to the red crash dot |
| cross-model (GPT-5.6-luna, xhigh, via codex) | 13 | `independence_verified: true` on all three legs |

The cross-model legs corroborated three in-process findings — the objective/hang overclaim reached triple agreement — and added four of their own that survived: U1b depended on a module U6 creates, U4's tests had no panic-injection seam, only one of the three fallible signatures was specified, and the panicking APIs needed a deprecation policy or a future consumer just recreates the defect.

Note the trio peers reviewed the Product Contract slice as it stood *before* the mechanism flip, so their browser-floor findings were already resolved by it.

---

## Things the review surfaced that are worth carrying forward regardless

- **The panic surface is not five sites.** Five `unreachable!()` macros are the only explicit panic *macros* in non-test parser code, which is narrower than "the only way it can panic" — indexing, `unwrap`, debug overflow, and dependency panics all bypass that count. U0 exists to settle reachability, but a verdict of "all five unreachable" means the guard's value is containment of an unenumerated surface, not protection against a known input. That's a legitimate justification; it's just a different one, and the plan now says so.
- **`catch_unwind` assumes unwinding, natively too.** No `[profile]` sets `panic = "abort"` today, but one added later — here or downstream — silently reduces every native guard to a pass-through. U7 documents the guarantee as conditional.
- **A hang is not covered and never was.** The original objective claimed malformed input never leaves a client dead; a parser infinite loop still freezes the browser's main thread. Notably this is the one failure class the repo has actually *reproduced* (`docs/solutions/logic-errors/recursive-descent-skip-to-sync-infinite-loop.md`), unlike the panic class. Objective narrowed; hang deferred with the Worker work.

---

## The other repo

Website work (U1b, U6) is **unstarted**. `~/personal/oxabl_web` is clean on `agent/wasm-playground` at `21c2d7e`.

The repos are coupled by a **manual copy** — a human runs `scripts/build-wasm.sh` into `[web] src/wasm/` and commits. Nothing records provenance and nothing enforces wire-shape lockstep. U6 now adds a load-time guard that detects a stale artifact and says so, rather than rendering a confident crash state that can never heal. That detects a mismatch; it does not prevent one.

---

## What the reachability check found

I ran U0's analysis rather than leaving it for the implementer. **All five `unreachable!()` sites look unreachable from any input** — each is a guarded dispatch arm whose guard and match arms are in sync:

| Site | Guard | Verdict |
|---|---|---|
| `expressions.rs:143` | `is_comparison_operator()` at `:79-98`, 15 kinds | match lists the same 15 |
| `expressions.rs:175` | `check(Add) \|\| check(Minus)` | match is `Add \| Minus \| _`, same token |
| `expressions.rs:231` | `check(Star) \|\| check(Slash) \|\| check(Modulo)` | match is those three |
| `statements.rs:1260` | single caller at `:895` checks the four directions | match is the same four |
| `statements.rs:2214` | `is_non_equals_comparison_operator()` at `:2176`, 14 kinds | match covers all 14 |

The wider surface also looks clean: the parser has **no** production `unwrap`/`expect`/`panic!` (all 13 hits are under `#[cfg(test)]`), and although token indexing is unguarded (`peek_at` is `&self.tokens[self.current + n]`), it holds — one terminal `Eof` sentinel, `check_at` returns `Option` via `.get()`, `&&` short-circuits before the one `peek_at(2)` at `statements.rs:864`, and `peek_nth_non_comment` returns early on `Eof` by design (`mod.rs:660-662`).

**So the browser track's justification is containment of an unenumerated surface, not repair of an observed defect.** State it that way in the PR. Caveats: absence of a found reproducer is not proof, and this sweep covered `oxabl_parser` only — the lexer, preprocessor, and formatter also run inside `oxabl::parse` and `format_source` and were not swept. Debug-build arithmetic overflow is also unchecked.

The native increment is unaffected: any *future* panic on the LSP main loop still kills the server, which is reason enough on its own.

## Sequencing when you pick this up

**U0 is largely answered above** — confirm the table if you want, but it gates nothing and U2 can start immediately.

Browser track: `U2` → `U1` → `U5` → `U6` → **`U1b` last** (it classifies failures inside the module U6 creates). Native track: `U3` → `U4`, concurrent and shipping alone. `U7` last.

**The cheapest genuinely-useful start is still U3 + U4.** No browser, no dependency bump, no second repo — and it closes the LSP main-loop hole, the highest-severity item in the plan. It is increment one and ships on its own.
