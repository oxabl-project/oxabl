# Handoff: #65 + #66 done — tree clean, skill extracted

**Date:** 2026-07-18  
**Branch:** `master`  
**Merge commit (feature stack):** `872c7411bdd0d9d8a91a695993fbc390f159ebe6` (PR #67)  
**Closed:** #65, #66  
**PR:** https://github.com/oxabl-project/oxabl/pull/67 (merged)  
**Skill:** `.grok/skills/gh-issue-loop/` (also mirrored to `~/.grok/skills/gh-issue-loop/`)

---

## Current state

| Item | Status |
|------|--------|
| #65 mid-line `&IF` / ADM2 preproc | **Closed + merged** |
| #66 xp-property BUFFER-FIELD path | **Closed + merged** — corpus GREEN |
| PREPROC002 (9-module sample) | **0** |
| PARSE001 (deduped, schema-loaded) | **6** (≤ baseline 9) |
| PR #67 | **Merged** |
| Working tree | Clean after committing leftover plans + skill |
| Local branch `fix/inline-preproc-if-expression` | Deleted (merged) |

---

## Downstream pin

```toml
# Cargo.toml — all oxabl_* crates share one rev
rev = "872c7411bdd0d9d8a91a695993fbc390f159ebe6"
```

Or track `master` (includes HANDOFF/skill docs after this cleanup commit).

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

## What landed (#65 / #66)

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

Plans: `docs/plans/2026-07-17-003-…` through `2026-07-18-004-fix-xp-property-buffer-field-plan.md`.

---

## Reusable skill: `/gh-issue-loop`

Distilled from this session’s AFK loop (plan → Fable verify → implement → push →
issue comment → poll 2m / idle 2h → merge on green).

| Path | Role |
|------|------|
| `.grok/skills/gh-issue-loop/SKILL.md` | Full workflow (project, committed) |
| `.grok/skills/gh-issue-loop/scripts/watch-issue.sh` | Issue poller for `monitor` |
| `~/.grok/skills/gh-issue-loop/` | User-scope mirror (cross-project) |

**Invoke:** `/gh-issue-loop` or natural language (“AFK issue loop on #N”, “watch the issue”).

**Watcher example:**

```bash
.grok/skills/gh-issue-loop/scripts/watch-issue.sh 66 \
  --poll-secs 120 \
  --idle-hours 2 \
  --baseline-comment-id <id-of-your-fix-comment>
```

---

## Next (optional)

1. Bump downstream oxabl pin to `872c741` / latest `master`; re-smoke WebSpeed entry points with `$DLC/tty` on `-I`.
2. Residual 6 PARSE001: track only if they matter for lint/analyze accuracy.
3. CodSpeed “Performance Analysis” was FAIL on PR #67 while the benchmark job itself passed — investigate only if master shows a real regression.
4. Backlog plans now on master (`docs/plans/2026-07-16-005` … `010`, semantic v1.1 followups) — pick next focus when ready.

---

## Related

| Issue/PR | Relation |
|----------|----------|
| #65 | Closed — preprocessor stack |
| #66 | Closed — xp-property BUFFER-FIELD path |
| PR #67 | Merged — both stacks |
| #58 / #64 / #62 | Prior preproc/lint quality work |
