# Handoff: #68 merged — #69 open on PR #73

**Date:** 2026-07-19  
**Branch:** `master`  
**Merge commit (#68):** `8699d86109b9e0035693a1db51c8423e3bf74ea9` (PR #70)  
**Closed:** #68  
**Open:** #69 — PR https://github.com/oxabl-project/oxabl/pull/73  
**Pin for #69 A/B:** `db1d2e42cc71678d57b55123243020be764ff3c4`

---

## Current state

| Item | Status |
|------|--------|
| #68 FUNCTION signature params → LINT0001 | **Closed + merged** |
| #68 type-only prototype PARSE001 follow-up | **Included in merge** |
| #69 FUNCTION FORWARD/IN SUPER SEM0001 | **Open** — PR #73 on master |
| Working tree | HANDOFF refresh pending commit |

### #68 corpus A/B (green)

| code | baseline | candidate | delta |
|------|---------:|----------:|------:|
| LINT0001 | 77,233 | 66,556 | **−10,677** |
| LINT0002 | 71,783 | 72,186 | +403 (intended) |
| LINT0004 | 2,626 | 1,965 | −661 |
| PARSE001 | 71 | 71 | **0** |
| total | 207,829 | 196,894 | **−10,935** |

---

## Downstream pin

```toml
# master after #68
rev = "8699d86109b9e0035693a1db51c8423e3bf74ea9"

# #69 candidate (PR #73)
rev = "db1d2e42cc71678d57b55123243020be764ff3c4"
```

### Smoke

```bash
# #68 function params
cat > /tmp/func_params.p << 'EOF'
FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER):
  RETURN sValue + "x".
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_params.p
# diagnostics []

# #68 type-only prototype
cat > /tmp/type_only.p << 'EOF'
FUNCTION f RETURNS CHARACTER (INPUT CHARACTER) FORWARD.
EOF
oxabl check --preprocess /tmp/type_only.p
# exit 0

# #69 FORWARD + definition
cat > /tmp/func_fwd.p << 'EOF'
FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER) FORWARD.

FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER):
  RETURN sValue.
END FUNCTION.
EOF
oxabl analyze --preprocess /tmp/func_fwd.p
# expect no SEM0001 (on #69 pin)
```

---

## What landed (#68)

1. `parse_function` uses `parse_parenthesized_params`; prepends `DefineParameter` onto body.
2. `TABLE-HANDLE` in parenthesized param lists.
3. Type-only (unnamed) prototype params, e.g. `(INPUT CHARACTER) FORWARD.`

Plan: `docs/plans/2026-07-18-005-fix-function-signature-params-plan.md`.

---

## Next

1. Consumer A/B on #69 pin `db1d2e4` — SEM0001 drop.
2. On green: merge PR #73, close #69, refresh HANDOFF.
3. Residual: METHOD … FORWARD reconciliation (noted as #69 non-goal follow-up).

---

## Related

| Issue/PR | Relation |
|----------|----------|
| #68 | Closed — FUNCTION signature params |
| PR #70 | Merged — #68 |
| #69 | Open — SEM0001 FORWARD/IN SUPER |
| PR #73 | Open — #69 on master |
| PR #71 | Superseded (stack base deleted) |
| #65 / #66 / PR #67 | Prior preproc/parser quality |
