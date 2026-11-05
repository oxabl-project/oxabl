#!/usr/bin/env bash
# Run every scenario in scripts/parity/scenarios on a copy of the synthetic
# corpus and compare oxabl's plan with the scenario's expected.txt.
#
#   OXABL=target/debug/oxabl scripts/parity/run.sh [scenario...]
#
# A scenario directory holds `edit.sh` (run in the corpus copy after a first
# plan) and `expected.txt` (the paths another tool rebuilds for that edit).
# Exit 1 when any scenario misses a path; 2 on a usage error.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
oxabl=${OXABL:-oxabl}
parity=$here/../plan-parity.sh

if [ "$#" -gt 0 ]; then
    scenarios=("$@")
else
    scenarios=()
    for dir in "$here"/scenarios/*/; do
        scenarios+=("$(basename "$dir")")
    done
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
failed=0

for name in "${scenarios[@]}"; do
    dir=$here/scenarios/$name
    [ -f "$dir/edit.sh" ] && [ -f "$dir/expected.txt" ] || { echo "run.sh: no scenario $name" >&2; exit 2; }
    root=$work/$name
    cp -r "$here/corpus" "$root"
    (cd "$root" && "$oxabl" build --plan --root "$root" >/dev/null)
    (cd "$root" && bash "$dir/edit.sh")
    (cd "$root" && "$oxabl" build --plan --json --root "$root" >"$work/$name.json")
    echo "== $name"
    "$parity" "$work/$name.json" "$dir/expected.txt" || failed=1
done

exit "$failed"
