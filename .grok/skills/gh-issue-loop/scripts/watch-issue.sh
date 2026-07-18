#!/usr/bin/env bash
# Poll a GitHub issue until idle timeout, close, or new activity.
# Designed for Grok `monitor` / agent watchers: one event per stdout line.
#
# Usage:
#   watch-issue.sh <issue-number> [options]
#
# Options:
#   --repo OWNER/REPO       default: gh repo view of cwd
#   --poll-secs N           default: 120
#   --idle-hours N          default: 2
#   --baseline-comment-id N last comment id already handled (skip as "new")
#   --baseline-updated ISO  issue updated_at already seen
#
# Events (stdout):
#   WATCHER_START …
#   NEW_COMMENT id=… user=… updated=… body=…
#   STATE_CHANGE state=… updated=…
#   ISSUE_TOUCHED updated_at=… comments=…
#   WATCHER_HEARTBEAT idle_s=… state=… comments=… last_comment=…
#   WATCHER_IDLE_TIMEOUT …
#   ISSUE_CLOSED …

set -euo pipefail

usage() {
  sed -n '2,20p' "$0" | sed 's/^# \?//'
  exit 2
}

ISSUE=""
REPO=""
POLL_SECS=120
IDLE_HOURS=2
LAST_COMMENT_ID=0
LAST_UPDATED=""
LAST_STATE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo) REPO="$2"; shift 2 ;;
    --poll-secs) POLL_SECS="$2"; shift 2 ;;
    --idle-hours) IDLE_HOURS="$2"; shift 2 ;;
    --baseline-comment-id) LAST_COMMENT_ID="$2"; shift 2 ;;
    --baseline-updated) LAST_UPDATED="$2"; shift 2 ;;
    -h|--help) usage ;;
    -*)
      echo "unknown option: $1" >&2
      usage
      ;;
    *)
      if [[ -z "$ISSUE" ]]; then
        ISSUE="$1"
        shift
      else
        echo "unexpected arg: $1" >&2
        usage
      fi
      ;;
  esac
done

[[ -n "$ISSUE" ]] || usage

if [[ -z "$REPO" ]]; then
  REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)
fi

IDLE_LIMIT_SECS=$((IDLE_HOURS * 60 * 60))
LAST_ACTIVITY_EPOCH=$(date +%s)

# Seed from API if baselines not provided
if [[ "$LAST_COMMENT_ID" == "0" || -z "$LAST_UPDATED" || -z "$LAST_STATE" ]]; then
  meta=$(gh api "repos/${REPO}/issues/${ISSUE}" --jq '{state, updated_at, comments}')
  LAST_STATE=$(echo "$meta" | jq -r .state)
  if [[ -z "$LAST_UPDATED" ]]; then
    LAST_UPDATED=$(echo "$meta" | jq -r .updated_at)
  fi
  if [[ "$LAST_COMMENT_ID" == "0" ]]; then
    LAST_COMMENT_ID=$(gh api "repos/${REPO}/issues/${ISSUE}/comments" \
      --jq '.[-1].id // 0')
  fi
fi

if [[ -z "$LAST_STATE" ]]; then
  LAST_STATE=$(gh api "repos/${REPO}/issues/${ISSUE}" --jq .state)
fi

echo "WATCHER_START issue=#${ISSUE} repo=${REPO} poll=${POLL_SECS}s idle_limit=${IDLE_LIMIT_SECS}s last_comment=${LAST_COMMENT_ID}"

while true; do
  sleep "$POLL_SECS"

  meta=$(gh api "repos/${REPO}/issues/${ISSUE}" --jq '{state, updated_at, comments}' 2>/dev/null || echo "")
  if [[ -z "$meta" ]]; then
    echo "WATCHER_WARN failed to fetch issue metadata; will retry"
    continue
  fi

  state=$(echo "$meta" | jq -r .state)
  updated_at=$(echo "$meta" | jq -r .updated_at)
  comments=$(echo "$meta" | jq -r .comments)

  comments_json=$(gh api "repos/${REPO}/issues/${ISSUE}/comments" --jq '.' 2>/dev/null || echo "[]")
  latest_id=$(echo "$comments_json" | jq -r '.[-1].id // 0')
  latest_user=$(echo "$comments_json" | jq -r '.[-1].user.login // "none"')
  latest_updated=$(echo "$comments_json" | jq -r '.[-1].updated_at // "none"')
  latest_body=$(echo "$comments_json" | jq -r '.[-1].body // ""' | head -c 2000)

  now=$(date +%s)
  changed=0

  if [[ "$latest_id" != "$LAST_COMMENT_ID" && "$latest_id" != "0" ]]; then
    changed=1
    LAST_COMMENT_ID=$latest_id
    LAST_ACTIVITY_EPOCH=$now
    body_oneline=$(printf '%s' "$latest_body" | tr '\n' ' ' | head -c 1500)
    echo "NEW_COMMENT id=${latest_id} user=${latest_user} updated=${latest_updated} body=${body_oneline}"
  fi

  if [[ "$state" != "$LAST_STATE" ]]; then
    changed=1
    LAST_STATE=$state
    LAST_ACTIVITY_EPOCH=$now
    echo "STATE_CHANGE state=${state} updated=${updated_at}"
  fi

  if [[ "$updated_at" != "$LAST_UPDATED" ]]; then
    LAST_UPDATED=$updated_at
    if [[ $changed -eq 0 ]]; then
      LAST_ACTIVITY_EPOCH=$now
      echo "ISSUE_TOUCHED updated_at=${updated_at} comments=${comments}"
    fi
  fi

  idle=$((now - LAST_ACTIVITY_EPOCH))
  echo "WATCHER_HEARTBEAT idle_s=${idle} state=${state} comments=${comments} last_comment=${LAST_COMMENT_ID}"

  if [[ $idle -ge $IDLE_LIMIT_SECS ]]; then
    echo "WATCHER_IDLE_TIMEOUT idle_s=${idle} (>= ${IDLE_LIMIT_SECS}). Giving up."
    exit 0
  fi

  if [[ "$state" == "closed" ]]; then
    echo "ISSUE_CLOSED — watcher stopping (orchestrator should merge if green)."
    exit 0
  fi
done
