#!/usr/bin/env bash
# Compare the rebuild set of `oxabl build --plan --json` with a list of paths
# from another tool: one root-relative path per line, blank lines ignored.
#
#   scripts/plan-parity.sh <oxabl-plan.json> <other-rebuild.txt>
#
# missed: in the list, not in the oxabl rebuild set. Exit 1 when there is any.
# extras: in the oxabl rebuild set, not in the list, grouped by first reason.
# Exit 2 on a usage error or a plan document that is not format 1.
set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "usage: plan-parity.sh <oxabl-plan.json> <other-rebuild.txt>" >&2
    exit 2
fi
plan=$1
list=$2
for file in "$plan" "$list"; do
    [ -r "$file" ] || { echo "plan-parity: cannot read $file" >&2; exit 2; }
done
command -v jq >/dev/null || { echo "plan-parity: jq is required" >&2; exit 2; }

format=$(jq -r '.format // empty' "$plan") || { echo "plan-parity: $plan is not JSON" >&2; exit 2; }
if [ "$format" != "1" ]; then
    echo "plan-parity: $plan is not a format 1 plan document" >&2
    exit 2
fi

ours=$(jq -r '.units[] | select(.action == "rebuild") | .path' "$plan" | LC_ALL=C sort -u)
theirs=$(sed -e 's#^\./##' -e '/^[[:space:]]*$/d' "$list" | LC_ALL=C sort -u)

missed=$(LC_ALL=C comm -13 <(printf '%s\n' "$ours") <(printf '%s\n' "$theirs") | sed '/^$/d')
extras=$(LC_ALL=C comm -23 <(printf '%s\n' "$ours") <(printf '%s\n' "$theirs") | sed '/^$/d')

count() { if [ -z "$1" ]; then echo 0; else printf '%s\n' "$1" | wc -l | tr -d ' '; fi; }

echo "oxabl rebuilds $(count "$ours"), the list has $(count "$theirs")"
echo "missed: $(count "$missed")"
if [ -n "$missed" ]; then
    printf '%s\n' "$missed" | sed 's/^/  /'
fi
echo "extras: $(count "$extras")"
if [ -n "$extras" ]; then
    printf '%s\n' "$extras" | jq -R -s -r --slurpfile plan "$plan" '
        split("\n") | map(select(length > 0)) as $paths
        | ($plan[0].units | map({key: .path, value: (.reasons[0].kind // "none")}) | from_entries) as $reason
        | $paths | group_by($reason[.]) | .[]
        | "  \($reason[.[0]]): \(length)\n" + (map("    " + .) | join("\n"))'
fi

[ -z "$missed" ]
