#!/usr/bin/env bash
# Lint A/B diff for the three count-gated rules (#128 merge evidence, R17).
#
# `corpus-ab-gate.sh` diffs *parse* outcomes. This diffs *lint* outcomes, which
# is what a suppression change moves. Run it on the pre-change build and the
# post-change build over the same tree, then diff: every diagnostic that
# disappeared is either a killed false positive (good) or a lost true positive
# (the cost). Sample and classify by hand — the script cannot tell you which is
# which, and that judgement is the whole point of the exercise.
#
# Nothing from the corpus belongs in this repo. This script writes its output to
# $OUT_DIR (default under target/, which is gitignored) and prints only counts.
# Do not paste file names, paths or snippets from its output into a commit
# message, a PR body, an issue, or a test fixture.
#
# Usage:
#   export CORPUS_ROOT=/path/to/abl/corpus       # required
#   export INCLUDE_PATHS="-I $CORPUS_ROOT"       # optional extra -I flags
#   export SCHEMA=/path/to/schema.df             # optional; enables LINT0003
#   export OXABL_BIN=./target/release/oxabl      # optional
#   export OUT_DIR=./target/lint-ab              # optional
#
#   git stash && cargo build --release -p oxabl
#   ./scripts/lint-ab-diff.sh collect before
#   git stash pop && cargo build --release -p oxabl
#   ./scripts/lint-ab-diff.sh collect after
#   ./scripts/lint-ab-diff.sh diff
#   ./scripts/lint-ab-diff.sh sample 20     # 20 random disappeared diagnostics
#
# Exit codes:
#   0  ok
#   2  usage / missing CORPUS_ROOT
#   3  oxabl binary missing

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT_DIR="${OUT_DIR:-$ROOT/target/lint-ab}"
OXABL_BIN="${OXABL_BIN:-$ROOT/target/release/oxabl}"
if [[ ! -x "$OXABL_BIN" ]]; then
  OXABL_BIN="$ROOT/target/debug/oxabl"
fi

# The rules this change can move. LINT0001/LINT0003 are listed so the run
# doubles as the R8 check: their counts must be identical between the two sides.
RULES="LINT0001 LINT0002 LINT0003 LINT0005 LINT0006"

usage() {
  sed -n '2,33p' "$0" | sed 's/^# \?//'
  exit 2
}

need_corpus() {
  if [[ -z "${CORPUS_ROOT:-}" ]]; then
    echo "error: CORPUS_ROOT is not set." >&2
    echo "  Point it at the ABL code corpus kept outside this repo." >&2
    exit 2
  fi
  [[ -d "$CORPUS_ROOT" ]] || { echo "error: CORPUS_ROOT does not exist: $CORPUS_ROOT" >&2; exit 2; }
}

need_oxabl() {
  [[ -x "$OXABL_BIN" ]] || {
    echo "error: oxabl binary not found at $OXABL_BIN" >&2
    echo "  Build with: cargo build --release -p oxabl" >&2
    exit 3
  }
}

# Walk the corpus, run `analyze --format json` per file, and emit one JSONL
# record per diagnostic: {file, code, line_hint, message}.
#
# `analyze` is per-file (unlike `check`, which takes a directory and runs no
# lint at all), so this loops. Slow but simple; a corpus run is a one-off.
collect() {
  local label=$1
  need_corpus
  need_oxabl
  mkdir -p "$OUT_DIR"
  local out="$OUT_DIR/$label.jsonl"
  : >"$out"

  local -a inc=()
  if [[ -n "${INCLUDE_PATHS:-}" ]]; then
    # shellcheck disable=SC2206
    inc=($INCLUDE_PATHS)
  else
    inc=(-I "$CORPUS_ROOT")
  fi
  local -a schema=()
  [[ -n "${SCHEMA:-}" ]] && schema=(--schema "$SCHEMA")

  echo "=== $label: oxabl analyze over $CORPUS_ROOT ===" >&2
  echo "    bin=$OXABL_BIN" >&2

  local n=0
  while IFS= read -r -d '' f; do
    n=$((n + 1))
    "$OXABL_BIN" analyze --format json --preprocess "${inc[@]}" "${schema[@]}" "$f" 2>/dev/null \
      | python3 -c '
import json, sys
try:
    d = json.load(sys.stdin)
except Exception:
    sys.exit(0)
path = sys.argv[1]
for diag in d.get("diagnostics", []):
    code = (diag.get("code") or "")
    if code not in sys.argv[2].split():
        continue
    span = diag.get("span") or {}
    print(json.dumps({
        "file": path,
        "code": code,
        "start": span.get("start"),
        "message": diag.get("message", ""),
    }))
print(json.dumps({"file": path, "code": "_UNJUDGED", "start": 0,
                  "message": str(d.get("unjudged_symbols", 0))}))
' "$f" "$RULES" >>"$out" || true
    if (( n % 200 == 0 )); then echo "    …$n files" >&2; fi
  done < <(find "$CORPUS_ROOT" -type f \( -name '*.p' -o -name '*.w' -o -name '*.cls' -o -name '*.v' \) -print0)

  echo "    $n files → $(wc -l <"$out") records in $out" >&2
}

diff_sides() {
  local before="$OUT_DIR/before.jsonl" after="$OUT_DIR/after.jsonl"
  for f in "$before" "$after"; do
    [[ -s "$f" ]] || { echo "error: missing or empty $f — run 'collect before' and 'collect after' first" >&2; exit 2; }
  done
  python3 - "$before" "$after" "$OUT_DIR/disappeared.jsonl" "$OUT_DIR/appeared.jsonl" <<'PY'
import collections, json, sys

before_path, after_path, gone_path, new_path = sys.argv[1:5]

def load(p):
    rows = []
    with open(p) as fh:
        for line in fh:
            line = line.strip()
            if line:
                rows.append(json.loads(line))
    return rows

def key(r):
    return (r["file"], r["code"], r["start"], r["message"])

before, after = load(before_path), load(after_path)

def counts(rows):
    c = collections.Counter(r["code"] for r in rows if r["code"] != "_UNJUDGED")
    return c

cb, ca = counts(before), counts(after)
bset = {key(r) for r in before if r["code"] != "_UNJUDGED"}
aset = {key(r) for r in after if r["code"] != "_UNJUDGED"}

gone = [r for r in before if r["code"] != "_UNJUDGED" and key(r) not in aset]
new = [r for r in after if r["code"] != "_UNJUDGED" and key(r) not in bset]

with open(gone_path, "w") as fh:
    for r in gone:
        fh.write(json.dumps(r) + "\n")
with open(new_path, "w") as fh:
    for r in new:
        fh.write(json.dumps(r) + "\n")

print("rule       before    after     delta")
for code in sorted(set(cb) | set(ca)):
    b, a = cb.get(code, 0), ca.get(code, 0)
    print(f"{code:<10} {b:>7}  {a:>7}  {a - b:>+8}")

print()
print(f"disappeared: {len(gone)}  -> {gone_path}")
print(f"appeared:    {len(new)}   -> {new_path}")

unjudged = sum(int(r["message"]) for r in after if r["code"] == "_UNJUDGED")
files_with = sum(1 for r in after if r["code"] == "_UNJUDGED" and int(r["message"]) > 0)
print(f"unjudged symbols (after): {unjudged} across {files_with} file(s)")

# R8 is a hard gate, not a judgement call: this change must not move LINT0001
# or LINT0003 by a single diagnostic.
bad = [c for c in ("LINT0001", "LINT0003") if cb.get(c, 0) != ca.get(c, 0)]
if bad:
    print()
    print(f"REGRESSION: {', '.join(bad)} moved. R8 says these must be byte-identical.")
    sys.exit(1)
PY
}

sample() {
  local n=${1:-20}
  local gone="$OUT_DIR/disappeared.jsonl"
  [[ -s "$gone" ]] || { echo "error: run 'diff' first" >&2; exit 2; }
  echo "Classify each as [K]illed false positive or [L]ost true positive." >&2
  echo "Open the file at the offset and decide whether the variable is really used." >&2
  echo >&2
  python3 - "$gone" "$n" <<'PY'
import json, random, sys

rows = [json.loads(line) for line in open(sys.argv[1]) if line.strip()]
# Fixed seed so a re-run shows the same sample — classification is manual work
# and should not have to start over.
random.seed(128)
for r in random.sample(rows, min(int(sys.argv[2]), len(rows))):
    print("{}  {}:{}".format(r["code"], r["file"], r["start"]))
    print("    " + r["message"])
    print()
PY
}

case "${1:-}" in
  collect) shift; collect "${1:?usage: collect before|after}" ;;
  diff)    diff_sides ;;
  sample)  shift; sample "${1:-20}" ;;
  *)       usage ;;
esac
