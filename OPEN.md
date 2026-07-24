# Open items — #119 panic-safe parse/format

**Date:** 2026-07-24
**Branch:** `evanbrobertson/119-panic-safe-parse-format`
**Status:** planned, not implemented. No code written.

---

## Where the plan is

`docs/plans/2026-07-24-001-fix-panic-safe-parse-format-plan.md`

**It is not in this branch.** `docs/plans/` is gitignored (`.gitignore:6`) by the repo's artifact policy — plans are point-in-time and go stale, so only durable artifacts get committed. The file exists locally in this worktree only. If you want it shared, either commit it deliberately with `git add -f` or move the durable parts into `docs/solutions/` after the work lands.

The plan is `implementation-ready`: 8 units (U1, U1b, U2-U7), 15 requirements, spanning **two repos**.

---

## Decisions I made without you

Flagging these because they were judgement calls, not mechanical steps.

1. **Kept the plan out of the commit.** Per the artifact git policy above. Say the word and I'll force-add it.
2. **Skipped ce-doc-review's cross-model peer pass.** It egresses document content to external model providers. With you away I had no authorization for outward-facing data movement, and the pass is explicitly additive and non-blocking. Not run.
3. **Trimmed the review team.** The skill's criteria selected six personas; I ran three (feasibility, coherence, scope-guardian) and skipped product-lens, design-lens, and adversarial. Reasoning: the premise was already affirmed at the scoping gate — which is exactly what adversarial re-litigates — and the UI surface is small and derivative of an existing pattern. This is reduced coverage, not full coverage.
4. **Applied all 18 review findings rather than deferring any.** Every one had a decisive fix. Two were reclassified rather than taken at face value — see below.

---

## What the research changed

Worth knowing before reading the plan, because it contradicts #119's issue body.

- **`catch_unwind` is confirmed inert on `wasm32-unknown-unknown`** (default `-Cpanic=abort`; unwinding needs nightly `-Zbuild-std`). So #119's option (a) — a `try_*` wrapper — fixes the CLI and LSP and does **nothing** for the browser. The issue body offers only that or full parser hardening; both are incomplete. The browser needs a third mechanism.
- **The browser mechanism exists and is stable.** `wasm-bindgen` 0.2.125 added `--force-enable-abort-handler`, which makes `set_on_abort` fire on `panic=abort`; `schedule_reinit()` then gives the next call a fresh instance. Requires bumping 0.2.108 → 0.2.126.
- **The native gap is wider than #119 or HANDOFF.md said.** HANDOFF claimed "three hand-rolled guards"; it is **seven**, plus **three unguarded call sites** — `crates/oxabl/src/main.rs:512` (`oxabl::analyze` in the `analyze` subcommand) and `crates/oxabl_lsp/src/lib.rs:246`/`:422`. The `:422` site is on the LSP main loop, so a parser panic there kills the server. All three verified directly, not taken on a subagent's word.
- **The website already try/catches everything**, so the playground's real defect is narrower and worse than "unguarded": it shows the raw string `unreachable executed` beside a **green** health dot, above **stale diagnostics from the last good run**, and stays dead until reload.

---

## Genuinely open

Neither blocks starting work.

1. **Does `#[wasm_bindgen(start)]` re-run on the instance `schedule_reinit()` creates?** Decides whether the panic hook and `set_on_abort` need explicit re-registration after each recovery, since the reinit resets Rust statics and the hook *is* a static. U5 covers both arms and its crash-recover-crash scenario proves whichever landed. **If this is wrong, the playground heals from its first crash and bricks on the second** — the exact defect #119 exists to close. Do not skip that second-crash test.
2. **Should `version()` carry a wire-shape revision as well as a build SHA?** A build ID answers "which artifact," not "which contract." Matters once #120 freezes the shapes. Deferred.

## Resolved, for the record

Both of the plan's original open questions are closed, so don't re-litigate them:

- **Does the abort handler fire on stable `panic=abort`?** Yes, ~90% confidence. The contradicting claim in wasm-bindgen's `src/handler.rs` module doc is **stale** — it predates the 0.2.125 flag. The code path was traced end to end in upstream sources; the guide and `rt/mod.rs` agree.
- **Does the panic hook's JS call complete before the trap?** Yes, guaranteed. On `panic=abort` std runs the hook to completion before aborting; a synchronous wasm→JS call inside it returns first by program order, engine-independent.

---

## Two findings I reclassified rather than accepting

Recording these so the reasoning survives.

- **Scope review flagged the `wasmPromise` rejection-caching fix as scope creep** (correctly — it served no requirement). I kept it and added **R14** instead of dropping it, because U6 rewrites the exact function that caches the rejection; shipping a rewritten loader that still poisons itself would be knowingly leaving it broken. The objection was about traceability, and a requirement fixes that.
- **Feasibility flagged the proposed `AnalyzeResponse.error` field as having no producer.** That one I accepted and **dropped the field** — on wasm a panic aborts before the function returns, so it could never be non-null. Net effect: this plan does not change the wire shape at all, which is the opposite of what I originally wrote.

---

## The other repo

The website work (U1b, U6) is **unstarted**. `~/personal/oxabl_web` is clean on branch `agent/wasm-playground` at `21c2d7e`. Nothing committed there.

Note the two repos are coupled by a **manual copy** — a human runs `scripts/build-wasm.sh` pointed at `[web] src/wasm/` and commits the result. Nothing records which oxabl commit produced the vendored artifact, and nothing enforces that the wire shapes match. U5's `version()` export improves diagnosis but not the coupling.

---

## Sequencing when you pick this up

`U2` (bump) → `U1` (glue assertion) → `U1b` (engine floor) → `U5` (wasm) → `U6` (website), with `U3` (fallible API) → `U4` (migrate guards) running as an independent concurrent track, and `U7` (docs) last.

The cheapest genuinely-useful starting point is **U3 + U4**: they need no browser, no dependency bump, and they close the LSP main-loop hole, which is the highest-severity item in the whole plan.
