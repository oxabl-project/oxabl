# Handoff: #65 + #66 merged to master

**Date:** 2026-07-18  
**Branch:** `master`  
**Merge commit:** `872c7411bdd0d9d8a91a695993fbc390f159ebe6` (PR #67)  
**Closed:** #65, #66  
**PR:** https://github.com/oxabl-project/oxabl/pull/67 (merged)

---

## Current state

| Item | Status |
|------|--------|
| #65 (mid-line `&IF` / ADM2 accessor preproc) | **Closed + merged** |
| #66 (xp-property BUFFER-FIELD path) | **Closed + merged** — corpus GREEN |
| PREPROC002 (9-module sample) | **0** |
| PARSE001 (deduped, schema-loaded) | **6** (≤ baseline 9) |
| PR #67 | **Merged** |

---

## Downstream pin

```toml
# Cargo.toml — all oxabl_* crates share one rev
rev = "872c7411bdd0d9d8a91a695993fbc390f159ebe6"
```

Or track `master`.

### Smoke

```bash
# #65 fnarg expression position
mkdir -p /tmp/tty
cat > /tmp/tty/fnarg << 'EOF'
&IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF
EOF
cat > /tmp/host.p << 'EOF'
DEFINE VARIABLE cQ AS CHARACTER NO-UNDO.
IF NOT {fnarg setOpenQuery cQ} THEN RETURN.
EOF
oxabl analyze /tmp/host.p --preprocess -I /tmp/tty
# exit 0

# #66 unquoted comma-list BUFFER-VALUE
cat > /tmp/bf_comma.p << 'EOF'
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
ASSIGN ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest.
EOF
oxabl check /tmp/bf_comma.p
# exit 0

# #66 bare multi BUFFER-FIELD assigns
cat > /tmp/bf_bare.p << 'EOF'
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U
ghProp:BUFFER-FIELD('ContainerType':U):BUFFER-VALUE = '':U.
EOF
oxabl check /tmp/bf_bare.p
# exit 0
```

With real OpenEdge: put `$DLC/tty` (and `gui`/`src`/`adm2` as needed) on `-I`.

---

## What landed

### #65 (preprocessor + companions)

| Round | Fix |
|-------|-----|
| 1 | Mid-line `&IF`: keep branch body after `&THEN`/`&ELSE`/`&ENDIF` |
| 2 | Line-oriented directives stop at same-line `&ENDIF` (get/set shape) |
| 3 | `PROCEDURE … IN SUPER:` prototypes |
| 4 | `{N}` in define values; `DYNAMIC-FUNC` abbrev |

### #66 (parser)

| Slice | Fix |
|-------|-----|
| A | Unquoted comma-separated ident lists as assignment values → synthetic string |
| B | Bare multi-pair BUFFER-FIELD assigns without leading `ASSIGN` |
| C | xp-path set stub + quote-strip characterization |

Corpus residual PARSE001 (6) is unrelated ambient noise (non-accessor assigns, malformed RUN trailing comma, `&ELSE`/positional edges) — not a #66 regression.

---

## Next (optional)

1. Bump downstream oxabl pin to `872c741` / master; re-smoke WebSpeed entry points.
2. Residual 6 PARSE001: track only if they matter for lint/analyze accuracy.
3. CodSpeed “Performance Analysis” showed FAIL on the PR check (benchmark job itself passed) — investigate if a real regression surfaces on master, not a merge blocker.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| #65 | Closed — preprocessor stack |
| #66 | Closed — xp-property BUFFER-FIELD path |
| PR #67 | Merged — both stacks |
| #58 / #64 / #62 | Prior preproc/lint quality work |
