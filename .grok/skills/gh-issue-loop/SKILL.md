---
name: gh-issue-loop
description: >
  AFK-friendly GitHub issue loop: plan, peer-verify, implement, push, comment
  with consumer smoke steps, watch the issue for follow-ups, re-run the loop on
  new work, and close+merge when the issue resolves cleanly. Use when the user
  says "AFK issue loop", "watch the issue", "issue watch", "consumer A/B loop",
  "/gh-issue-loop", or asks to plan → verify → ship → poll a GH issue until done.
metadata:
  short-description: "Plan/verify/ship + GH issue watch-merge loop"
---

# /gh-issue-loop — AFK GitHub Issue Loop

Run a full consumer-driven issue cycle while the user is AFK. Proven on
oxabl #65/#66 (plan → Fable review → implement → push → issue comment →
poll for A/B → merge).

## Inputs (from the user or HANDOFF)

| Input | Default / discovery |
|-------|---------------------|
| Issue number or URL | Required (`#N` or `owner/repo#N`) |
| Branch | Current branch, or create `fix/…` / `feat/…` from HANDOFF |
| Peer reviewer | Claude Code **Fable** via CLI (`claude -p --model fable`) |
| Idle watch timeout | **2 hours** of no issue updates |
| Poll interval | **2 minutes** |
| GPG | If signing fails (e.g. 1Password socket), retry with `--no-gpg-sign` |
| Merge when | Issue closed cleanly **or** consumer reports success bar met |

Repo/remote: `gh repo view --json nameWithOwner -q .nameWithOwner` if not given.

## Phase 0 — Orient

1. Read `HANDOFF.md` if present (branch tip, pin SHA, open issue, success bar).
2. Fetch the issue:
   ```bash
   gh issue view N --json title,body,state,comments,labels,url
   ```
3. Restate the **success bar** (metrics, smoke commands, non-goals) before coding.
4. Confirm working tree is clean enough to branch; note unrelated untracked files
   and leave them alone unless the user asked to clean them.

## Phase 1 — Plan

1. Reproduce failures with minimal fixtures (prefer no private corpus).
2. Decide ownership (parser vs preproc vs lint, etc.) **before** coding.
3. Write a plan under `docs/plans/YYYY-MM-DD-NNN-<type>-<slug>-plan.md` using the
   repo's existing plan frontmatter (`title`, `type`, `status`, `date`, `origin`,
   `branch`).
4. Include: context, failure modes, approach slices, non-goals, tests, risks,
   success criteria, **downstream smoke** commands.

## Phase 2 — Peer verify the plan

Hand the plan to a strong reviewer **before implement**. Default: Claude Code Fable.

```bash
claude -p --model fable --dangerously-skip-permissions --effort high <<'PROMPT'
You are verifying an implementation plan. Read:
1. <path-to-plan>
2. HANDOFF.md (if present)
3. The linked GH issue context in the plan
4. Skim the code paths the plan cites

Check: root cause, ownership, safety of heuristics, missing tests, risk to
existing green paths. Do NOT implement.

## Verdict: PASS | PASS_WITH_AMENDMENTS | FAIL
## Summary
## Amendments (if any)
## Risks accepted
## Ready to implement: YES | NO
PROMPT
```

- **PASS / PASS_WITH_AMENDMENTS + Ready YES** → apply amendments to the plan, then implement.
- **FAIL or Ready NO** → revise plan and re-verify (max 2 review rounds unless user overrides).

If Fable is unavailable, use the best available plan-review model and note the substitute in the issue comment.

## Phase 3 — Implement

1. Execute plan slices in order; keep diffs scoped to the issue.
2. Add unit/fixture tests for every failure mode + at least one negative test
   for each heuristic.
3. Gate before push:
   ```bash
   cargo fmt          # or project equivalent
   cargo clippy --workspace --all-targets -- -D warnings
   cargo test --workspace
   ```
   (Adapt to the repo's CI: `npm test`, `pytest`, etc. Match `.github/workflows`.)
4. Commit with Conventional Commits; **no AI attribution** footers.
   ```bash
   # If GPG/1Password fails:
   git commit --no-gpg-sign -m "fix: … (#N)"
   ```
5. Push the branch: `git push -u origin HEAD`.
6. Open a PR if none exists (`gh pr create`), linking the issue.

## Phase 4 — Update the issue (consumer contract)

Comment on the issue with:

- Pin SHA / branch
- What changed (short)
- How to test (copy-paste smoke)
- Success bar to re-run (corpus A/B, metrics table)
- Ask for results / close criteria

```bash
gh issue comment N --body "$(cat <<'EOF'
## Fix landed (#N)
**Pin:** `SHA`
…
### How to test
```bash
…
```
Please re-run A/B and comment results. If green, we close and merge.
EOF
)"
```

## Phase 5 — Watch

Poll the issue until one of:

| Event | Action |
|-------|--------|
| New comment with **more work** / fail A/B | Return to Phase 1 (or 3 if tiny) |
| New comment **GREEN** / success bar met | Phase 6 |
| Issue **closed** as resolved | Phase 6 (merge if PR open) |
| **Idle timeout** (default 2h, no updates) | Stop; leave branch+PR; update HANDOFF |

### Watcher (preferred)

Use the project script if present:

```bash
# From repo root — streams lines suitable for `monitor`
.grok/skills/gh-issue-loop/scripts/watch-issue.sh N \
  --idle-hours 2 \
  --poll-secs 120 \
  --baseline-comment-id <last-comment-id-you-posted>
```

Or start via the **monitor** tool so each stdout line becomes a chat event:

- `NEW_COMMENT …` — read full body with `gh api …/comments/{id}`
- `STATE_CHANGE state=closed` — proceed to merge if green
- `WATCHER_IDLE_TIMEOUT` — stop cleanly
- `WATCHER_HEARTBEAT` — ignore unless debugging

### Manual poll (fallback)

```bash
gh api repos/OWNER/REPO/issues/N/comments --jq '.[-1] | {id, user: .user.login, updated_at, body}'
gh issue view N --json state,updatedAt
```

Sleep 120s between polls; track last comment id; reset idle timer on any change.

## Phase 6 — Close and merge

When the consumer confirms success (or issue already closed as resolved):

1. Confirm CI on the PR: required checks green. Soft failures (e.g. optional
   CodSpeed analysis) may be non-blocking if CI workflows themselves passed —
   note them, don't block merge unless branch protection requires them.
2. Merge:
   ```bash
   gh pr merge --squash --delete-branch
   # only if blocked on optional checks and policy allows:
   # gh pr merge --squash --delete-branch --admin
   ```
3. If the issue is still open and criteria are met: `gh issue close N --reason completed` with a one-line summary.
4. Fast-forward local `master` (or default branch); prune merged branch.
5. Refresh `HANDOFF.md`: pin SHA, closed issues, residual work, smoke commands.
6. Commit+push HANDOFF if the repo keeps it on the default branch.

## Phase 7 — Cleanup (when user asks or at end of loop)

```bash
git status
git remote prune origin
git branch -d fix/… 2>/dev/null || true   # after merge
# Commit only intentional leftovers (plans, HANDOFF) — do not force-add secrets
```

## Decision heuristics

- **More work in comment:** If they cite new failure modes or a failed bar → new
  plan slice or amend plan; re-verify only if approach changes materially.
- **Green with residual noise:** If residuals are explicitly out of scope and
  under the numeric bar → merge; track residuals in HANDOFF / follow-up issue.
- **Ambiguous comment:** Prefer one clarifying `gh issue comment` over coding
  the wrong thing — but if AFK with a clear success table, trust the table.
- **Never** force-push shared branches; **never** add Co-authored-by / AI
  attribution.

## Anti-patterns

- Implementing before plan peer-review when the user asked for verify-first
- Polling in a tight loop (burn rate) — minimum 60s, default 120s
- Merging on unit green alone when the issue success bar is **corpus A/B**
- Leaving the watch running past idle timeout without updating HANDOFF

## Example one-liner (user prompt shape)

> Review HANDOFF, take issue #N, plan, Fable-verify, implement if positive,
> tests+clippy green, push, comment smoke on the issue, watch every 2m for 2h.
> On more work, loop; on clean resolve, close and merge. GPG fail → `--no-gpg-sign`.
