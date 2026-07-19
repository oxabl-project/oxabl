# Handoff: #68 + #69 closed — #74 fix pushed, awaiting A/B

**Date:** 2026-07-19  
**Branch:** `master`  
**Merge (#68):** `8699d86` (PR #70)  
**Merge (#69):** `beda23c` (PR #73)  
**Closed:** #68, #69  
**Open:** #74 — PR https://github.com/oxabl-project/oxabl/pull/75  
**Pin for #74:** `9fcaf13d44c0eb5cd64f559b3a550e1aaf76c345`

---

## Current state

| Item | Status |
|------|--------|
| #68 FUNCTION signature params → LINT0001 | **Closed + merged** |
| #69 FUNCTION FORWARD/IN SUPER SEM0001 | **Closed + merged** |
| #74 dot in preproc macro names | **Fix pushed** — PR #75, awaiting A/B |
| Working tree | On `fix/dot-in-preproc-macro-names` |

---

## Downstream pin

```toml
# #74 candidate (PR #75)
rev = "9fcaf13d44c0eb5cd64f559b3a550e1aaf76c345"
```

### Smoke

```bash
# #74 DEFINED matches after &GLOBAL-DEFINE with dot
cat > /tmp/dot_direct.p << 'SRC'
&GLOBAL-DEFINE foo.i true
&IF DEFINED(foo.i) = 0 &THEN
DEFINE VARIABLE shouldNotExist AS INTEGER NO-UNDO.
&ENDIF
SRC
oxabl analyze --preprocess /tmp/dot_direct.p
# expect: no 'shouldNotExist' variable

# #74 Include-once guard
mkdir -p /tmp/dotinc
cat > /tmp/dotinc/guarded.i << 'SRC'
&IF DEFINED(guarded.i) &THEN
    .
&ELSE
   &GLOBAL-DEFINE guarded.i true
DEFINE TEMP-TABLE ttX NO-UNDO FIELD f1 AS CHARACTER.
DEFINE VARIABLE vv AS INTEGER NO-UNDO.
&ENDIF
SRC
cat > /tmp/dotinc/mid.i << 'SRC'
{guarded.i}
SRC
cat > /tmp/main.p << 'SRC'
{guarded.i}
{mid.i}
vv = 1.
SRC
oxabl analyze --preprocess -I /tmp/dotinc /tmp/main.p
# expect: no SEM0001
```

---

## What landed

### #68 (PR #70)
Function signature params bound into function scope; type-only prototype params.

### #69 (PR #73)
Empty-body FUNCTION → `SymbolFlags::PROTOTYPE`; single + multi prototype merge;
true duplicate definitions still SEM0001.

### #74 (PR #75 — open)
`parse_define_body()` and `parse_undefine_body()` now accept `.` in macro names
alongside `[a-zA-Z0-9_-]`. Filename-based include-once guards now fire
correctly, preventing guarded includes from expanding twice.

---

## Next

1. Consumer A/B on #74 pin `9fcaf13` — SEM0001 drop from include-once guards.
2. On green: merge PR #75, close #74, refresh HANDOFF.
3. Continue monitoring for new issues >#74.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| #68 | Closed — FUNCTION signature params |
| PR #70 | Merged — #68 |
| #69 | Closed — SEM0001 FORWARD/IN SUPER |
| PR #73 | Merged — #69 |
| #74 | Open — dot in preproc macro names |
| PR #75 | Open — #74 fix |
