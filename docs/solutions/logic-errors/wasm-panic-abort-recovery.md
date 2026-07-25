# A wasm panic traps rather than unwinds, so recovery is reinstantiation

**Symptom.** The browser playground shows the raw string `unreachable executed`
in its status line, beside a **green** status dot, above **stale diagnostics from
the previous run**. The `try`/`catch` around the call was working — it caught
something — but every part of what it then displayed was wrong.

**Cause.** `wasm32-unknown-unknown` builds with `-Cpanic=abort` on stable Rust.
`std::panic::catch_unwind` still *compiles* there, so a guard written the way the
native clients write theirs looks correct and does nothing: a panic never unwinds,
it lowers to an `unreachable` trap that surfaces to JS as
`WebAssembly.RuntimeError` whose message is the bare trap text. Adding `try_*`
wrappers to the shared API does not help the browser at all.

## What actually works

Two halves, and neither is a `catch`:

1. **A panic hook, inside the artifact.** `std::panic::set_hook` runs to
   completion *before* the panic runtime aborts, so a synchronous wasm→JS call
   from inside the hook returns before the trap, by program order, on every
   engine. That is the only window in which the panic message can escape. The
   hook writes it to a `globalThis` key.

   The shim must be `#[wasm_bindgen(inline_js = "…")]` compiled into the
   artifact, **not** a path import of a consumer module: a `--target web` build
   bakes its import specifiers at bindgen time, so importing a website path would
   hardcode that site's directory layout into the crate and break the artifact for
   everyone else.

   Reading the message back through a second export call is the wrong shape
   regardless — it re-enters an instance already deemed untrustworthy, and the
   reset discards the statics anyway.

2. **Reinstantiation, from the consumer.** `wasm-bindgen`'s
   `--experimental-reset-state-function` exports `__wbg_reset_state()`, which
   builds a fresh `WebAssembly.Instance` from the already-compiled module with
   Rust statics reset. Its last step is an unconditional `__wbindgen_start()`,
   which is what **re-arms the panic hook** — the hook is a static, so it dies
   with the old instance. No `reinstall()` export is needed; upstream removed
   `set_on_reinit` in 0.2.118 for exactly this reason.

## Prefer reset-state over the abort handler

`--force-enable-abort-handler` reaches the same reinit machinery, and is the
option the guide leads with. It was rejected: it makes the generator inject
`try_table`/exnref instructions and two `WebAssembly.Tag` imports, raising the
browser floor to roughly Chrome 128 / Firefox 131 / Safari 18.4 — where the module
fails to **instantiate**, a hard failure rather than degradation. Trading a
deterministic demo loss for some visitors against recovery from a panic no known
input produces is the wrong direction of expected value on a first-contact
surface. `generate_reinit` is `aux.uses_reinit || config.generate_reset_state`,
independent of the abort handler, so reset-state alone injects nothing and leaves
the floor where it was.

Recovery also ends up *more* legible this way: it is one call in a `catch` block
the consumer already had, rather than behavior implicit in generated glue.

## Two traps worth knowing

**The flag is silently load-bearing.** Drop
`--experimental-reset-state-function` from the build and nothing fails: the
artifact builds, the consumer calls a function that is not there, and recovery
quietly stops working. A build-script assertion is the only thing that converts
that into a failure — and it must match the **export statement**, not the bare
symbol name. `wasm-bindgen` copies Rust doc comments into the glue as JSDoc, so a
grep for `__wbg_reset_state` is satisfied by a doc comment that merely mentions
it. That false negative is real: it passed a build that had no such export.

**Do not assert on `__wbg_call_guard`.** It looks like abort-handler machinery and
is not — it ships with reset-state and only tests the reinit flag. The
abort-handler-only symbols are `__wbg_handle_catch` and `__wbg_call_abort_hook`,
plus the `WebAssembly.Tag` import.

## A trap does not necessarily brick the instance

Measured, against a deliberate `panic!` in an exported function: after the trap,
three subsequent calls to another export **succeeded** with no reset at all. So
"the module is dead until reload" is not automatic — how much damage a panic does
depends on where it happened and what it left half-done (a leaked allocation, a
partly-updated static).

This does not make the reset pointless; it makes it *unconditional insurance*.
The reset guarantees a clean statics slate regardless of where the panic was,
rather than leaving the next call's correctness to depend on the previous panic's
location. But it does mean a report of "the playground is bricked until reload"
should be checked rather than assumed — the observable defect may be entirely in
the consumer's own state handling (a status string never cleared, diagnostics
never dropped), which is a different fix.

## The presentation is half the bug

Recovering the engine and still rendering it badly fixes nothing a visitor can
see. Three specific traps, all of which are "stale data presented as current":

- **An empty diagnostics array is not a neutral state.** It renders the cheerful
  "No diagnostics" box, which beside a red error dot tells the visitor everything
  is fine. A crash needs its own explicit flag and its own panel content.
- **Clear the panic stash before every call, not after.** Otherwise a visitor can
  be shown the *previous* crash's message as the cause of the current one. And a
  `RuntimeError` can arrive from a non-panic trap — a stack overflow or OOM — where
  the hook never ran and nothing was stashed; that needs a fixed fallback, never
  the last message that happened to be lying around.
- **Not every failure is retryable.** A compile or instantiation failure
  (`CompileError`/`LinkError`) is terminal on that engine — no reload-free path can
  succeed — so the controls stay disabled permanently, which is the opposite of the
  crash state where retrying is the entire point. Route on error *type*, not on
  string matching, which is brittle across engines.

Related: a caching loader that caches its own *rejection* makes a flaky network as
permanent as a panic. Clear the cached promise on rejection.

## Not covered: hangs

None of this touches an infinite loop. A hung module is indistinguishable from a
trapped one to a visitor, freezes the main thread, and needs a Web Worker with a
timeout. That is the failure class this repo has actually reproduced (see
`recursive-descent-skip-to-sync-infinite-loop.md`), unlike the panic class.
