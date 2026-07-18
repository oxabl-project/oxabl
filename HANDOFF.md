# Handoff: #66 fix pushed — awaiting corpus A/B

**Date:** 2026-07-18  
**Branch:** `fix/inline-preproc-if-expression` (pushed to `origin`)  
**Branch tip:** `4f78083` — fix: parse ADM2 xp-property BUFFER-FIELD fast path (#66)  
**Functional pin (#65 only):** `da7fa4d5650839e162bb4da29d161177e8e12bfd`  
**PR:** https://github.com/oxabl-project/oxabl/pull/67  
**Issue:** https://github.com/oxabl-project/oxabl/issues/66 (open — waiting on corpus confirm)

**Plan:** `docs/plans/2026-07-18-004-fix-xp-property-buffer-field-plan.md`  
(Fable review: PASS_WITH_AMENDMENTS; amendments applied before implement)

---

## Current state

| Item | Status |
|------|--------|
| #65 (mid-line `&IF` / ADM2 accessor preproc) | **Closed** |
| #66 (xp-property BUFFER-FIELD path) | **Fix pushed** — unit green; corpus A/B pending |
| PREPROC002 | stay 0 (no preproc changes in #66) |
| PR #67 | Open (`fix/inline-preproc-if-expression` → `master`) |
| Watcher | Polling #66 every 2m; idle-exit after 2h no updates |

---

## What #66 landed

Parser-only (no lexer/codegen; no include-arg quote-policy change):

1. **Unquoted comma-list assignment values** — `BUFFER-VALUE = a,b,c` folds to one synthetic character string. Lookahead-first; only sequences that previously errored.
2. **Bare multi-pair BUFFER-FIELD assigns** — consecutive `h:BUFFER-FIELD(…):BUFFER-VALUE = …` without leading `ASSIGN` → one `StatementKind::Assign`. Non-allocating lookahead (NodeId-safe).

### Files

| Area | Path |
|------|------|
| Parser | `crates/oxabl_parser/src/parser/statements.rs` |
| Parser tests | `crates/oxabl_parser/src/parser/tests.rs` (+8) |
| Preproc fixtures | `crates/oxabl_preprocessor/src/preprocessor.rs` (+2) |
| Plan | `docs/plans/2026-07-18-004-fix-xp-property-buffer-field-plan.md` |

### Tests at tip

- `oxabl_parser`: **470** passing  
- `oxabl_preprocessor`: **143** passing  
- workspace tests + clippy `-D warnings` + fmt green  

---

## Downstream pin & smoke

```toml
rev = "4f78083fab9ddf52110b6a48b0b98105827b6f8c"
# or branch = "fix/inline-preproc-if-expression"
```

```bash
# 1. Unquoted comma-list
cat > /tmp/bf_comma.p << 'EOF'
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
ASSIGN ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest.
EOF
oxabl check /tmp/bf_comma.p   # exit 0

# 2. Bare consecutive BUFFER-FIELD assigns
cat > /tmp/bf_bare.p << 'EOF'
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U
ghProp:BUFFER-FIELD('ContainerType':U):BUFFER-VALUE = '':U.
EOF
oxabl check /tmp/bf_bare.p   # exit 0

# 3. xp-path stub
mkdir -p /tmp/tty
cat > /tmp/tty/set << 'EOF'
&IF DEFINED(xp{1}) <> 0 &THEN
ASSIGN ghProp:BUFFER-FIELD('{1}':U):BUFFER-VALUE = {2}.
&ELSE
DYNAMIC-FUNC("set{1}":U IN TARGET-PROCEDURE, {2})
&ENDIF
EOF
cat > /tmp/xp_host.p << 'EOF'
&GLOBAL-DEFINE xpDataSourceEvents yes
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
{set DataSourceEvents dataAvailable,confirmContinue,isUpdatePending,buildDataRequest}
EOF
oxabl check /tmp/xp_host.p --preprocess -I /tmp/tty   # exit 0
```

Corpus success bar (same 9-module sample, schema-loaded):

| Signal | Pass |
|--------|------|
| PREPROC002 | **0** |
| PARSE001 (deduped) | ≤ **9** |
| Net parse fails | ≤ 0 |

---

## Next when A/B returns

1. If green → close #66, merge PR #67.  
2. If residual PARSE001 → triage comment, another fix loop on same branch.  
3. After merge: bump downstream oxabl pin; re-smoke WebSpeed entry points with `$DLC/tty` on `-I`.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| #65 | Closed — preprocessor stack |
| #66 | Open — this fix awaiting corpus |
| PR #67 | Merge vehicle for #65+#66 stack |
| #58 / #64 / #62 | Prior preproc/lint quality work |
