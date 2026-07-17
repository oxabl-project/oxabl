---
title: "fix: Undefined {&macro} expands to empty (not preserved)"
type: fix
status: ready
date: 2026-07-17
origin: GitHub #64
branch: fix/undefined-preproc-macro-empty
---

# fix: Undefined `{&macro}` expands to empty (#64)

## Problem

When `{&name}` is **not** defined (no `&GLOBAL-DEFINE` / `&SCOPED-DEFINE` / include
arg), ABL expands it to the **empty string**. oxabl currently **preserves** the
literal `{&name}` text (intentional since 2026-04-15 "preserve undefined refs").

That corrupts declaration idioms:

```
{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.
```

with only `var-type` supplied → should become:

```
DEFINE NEW GLOBAL SHARED VARIABLE myvar AS CHARACTER NO-UNDO.
```

Instead `batch_global_alt` stays as text, the line never parses as DEFINE, `myvar`
never registers, and every use is LINT0001. **Silent** — no PARSE001.

This is the dominant shared/include LINT0001 driver for the downstream consumer
(distinct from #58 resolution-model gaps).

## Repro (must pass after fix)

**def.i**
```
{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.
```

**host.p**
```
{def.i &var-type = "DEFINE NEW GLOBAL SHARED VARIABLE "}
DISPLAY myvar.
```

`oxabl analyze host.p --preprocess -I <dir>` → no LINT0001 on `myvar`.

**Control** (already works today):
```
&GLOBAL-DEFINE batch_global_alt
{def.i &var-type = "DEFINE NEW GLOBAL SHARED VARIABLE "}
DISPLAY myvar.
```

## Goals

- Undefined `{&name}` in emitting context expands to empty (omit text, advance past ref).
- Same for `expand_preproc_vars` (dynamic include names / args).
- Optional: soft diagnostic (Warning) is OK but **must not** be required for
  correctness; prefer silent empty expand matching AVM (or PREPROC-style info
  only if it does not spam batch output).
- Update tests that currently assert preservation.

## Non-Goals

- Changing missing positional `{N}` behavior (unless same bug is proven).
- Cross-file SHARED resolution (#58 ambient).
- Reverting skip-`.i`-standalone policy.

## Design

### Site 1 — main expand loop

`crates/oxabl_preprocessor/src/preprocessor.rs` ~324–367:

```rust
if let Some(val) = self.vars.get(var_name).cloned() {
    // emit expanded value (existing)
} else {
    // CURRENT: preserve as-is (advance i, leave chunk covering {&name})
    // NEW: treat as empty expansion —
    //   1. flush chunk before the reference (if i > chunk_start)
    //   2. do NOT emit any content for the ref
    //   3. i = ref_end; chunk_start = i; continue
}
```

Empty expansion must **break the chunk** before the ref (same as defined path),
then skip the ref with zero-length emit. Otherwise `{&x}` bytes remain in the
chunk.

### Site 2 — `expand_preproc_vars`

~675–680: undefined name currently leaves `{&name}` in output. Change to push
nothing and set `changed = true` so callers get a substituted string.

### Site 3 — tests to rewrite

Flip assertions that expect preservation:

| Test | Old | New |
|------|-----|-----|
| `undefined after &UNDEFINE` (`{&X}rest`) | `"{&X}rest"` | `"rest"` |
| comment-scoped define does not define → `{&FOO}` | contains `{&FOO}` | empty / no ref |
| `outside={&arg}` after include | preserves `{&arg}` | `outside=` |
| `MESSAGE '{&not-a-ref}'` | preserves | `MESSAGE ''` (empty expand inside quotes too — matches AVM) |

Add **#64 end-to-end** tests:

1. Preprocessor-only: undefined mid-declaration expands empty.
2. Integration via analyze/lint (in `oxabl` or preprocessor + parser test):
   include-arg define + undefined slot → symbol `myvar` declared, no LINT0001.

### Condition evaluator

`condition.rs` already expands missing to empty (`undefined_variable_expands_to_empty`).
No change.

## Implementation steps

1. Change undefined branch in main loop to empty expand (chunk flush + skip).
2. Change `expand_preproc_vars` undefined arm to empty + `changed = true`.
3. Update unit tests listed above.
4. Add repro test from issue #64 (in-memory FS with def.i + host.p).
5. `cargo test -p oxabl_preprocessor` and a focused semantic/lint integration if easy.
6. Document in a one-line comment: "ABL expands undefined {&name} to empty; do not preserve."

## Risk

- **Medium-low.** Deliberately reverses 2026-04-15 preserve policy. That policy
  helped incomplete fragments tokenize as Preprop; empty expand is correct ABL
  and unblocks real corpora. Any consumer relying on preserved `{&x}` text in
  preprocessed output will see empties instead (correct vs AVM).
- Watch **dynamic includes** `{{&frame}.f}` when frame undefined → empty name →
  already PREPROC007 path; ensure no panic.

## Effort

~0.5–1 day.
