---
title: "fix: reconcile multiple FUNCTION prototypes + definition (SEM0001) (#69 follow-up)"
type: fix
status: active
date: 2026-07-18
origin: GitHub #69 A/B results — double-prototype case still fires
branch: fix/function-forward-prototype-sem0001-v2
---

# fix: reconcile multiple FUNCTION prototypes + definition (SEM0001) (#69 follow-up)

## Context

PR #73 (first iteration) reconciles a single FUNCTION prototype (FORWARD / IN
SUPER / MAP TO) with a subsequent full definition. Corpus A/B shows a −4,788
SEM0001 reduction, but the targeted WebSpeed/ADM modules barely moved because
real code commonly emits **two prototypes** before the definition:

```abl
FUNCTION getVal RETURNS CHARACTER (INPUT s AS CHARACTER) FORWARD.
FUNCTION getVal RETURNS CHARACTER (INPUT s AS CHARACTER) IN SUPER.
FUNCTION getVal RETURNS CHARACTER (INPUT s AS CHARACTER):
  RETURN s.
END FUNCTION.
```

The current `try_merge_function_prototype` treats `(true, true)` (two
prototypes) as a collision → SEM0001. That loses the bulk of the intended
WebSpeed/ADM win.

## Root cause

`try_merge_function_prototype`'s `(true, true)` arm returns `None`, causing
SEM0001 for prototype-vs-prototype. In ABL semantics, multiple forward
declarations of the same function are idempotent — only the name matters for
disambiguation (ABL has no user-defined function overloading).

## Approach

### Change: prototype + prototype → merge (idempotent)

In `try_merge_function_prototype`, change `(true, true)` from `None` to
`Some(prior)`. The second prototype is silently ignored; the symbol keeps its
PROTOTYPE flag and original declaration node.

Updated matrix:

| Prior | Incoming | Action |
|-------|----------|--------|
| PROTOTYPE | definition | Merge: clear PROTOTYPE, update declaration (unchanged) |
| definition | PROTOTYPE | Ignore prototype (unchanged) |
| PROTOTYPE | PROTOTYPE | **Merge**: ignore second prototype (was SEM0001 → now clean) |
| definition | definition | SEM0001 (true duplicate — unchanged) |

This handles N ≥ 2 prototypes transitively: each additional prototype sees a
prior PROTOTYPE and merges.

### Test changes

1. `function_two_prototypes_still_sem0001` → `function_two_prototypes_no_sem0001`
   — expects 0 SEM0001 for FORWARD + FORWARD (or FORWARD + IN SUPER).
2. `function_forward_and_in_super_then_definition_no_sem0001` (new) — 3 stmts:
   FORWARD, IN SUPER, definition → 0 SEM0001, one symbol.
3. All other existing tests pass unchanged: single-proto+def, def+proto,
   two-defs, method dup.

## Non-goals

- Signature-based overload resolution (ABL doesn't support it for user functions).
- METHOD … FORWARD reconciliation (already out of scope for #69).
- #68 behavior (unchanged).

## Risks

| Risk | Mitigation |
|------|------------|
| Truly accidental duplicate prototypes silenced | In ABL, repeated prototypes are idiomatic (WebSpeed emits them). Silencing matches ABL compiler behavior. True duplicate *definitions* still fire. |
| N prototypes with partial body (empty-body edge case) | Empty-body is already the PROTOTYPE signal after #68; no false negatives expected. |

## Success criteria

1. FORWARD + IN SUPER + definition → 0 SEM0001.
2. N FORWARD prototypes + definition → 0 SEM0001.
3. Single prototype + definition → still 0 SEM0001 (unchanged).
4. True duplicate definitions → still SEM0001.
5. `cargo fmt` / clippy `-D warnings` / `cargo test --workspace` green.
6. Downstream corpus: large additional SEM0001 drop on WebSpeed/ADM modules.

## Related

- #69 (this issue)
- PR #73 (first iteration — single prototype + definition)
- #68 FUNCTION signature params (merged)
