#!/usr/bin/env bash
# Corpus A/B gate for preprocessor / parse regressions (oxabl#65 criterion 7).
#
# Same role as the 9-module private-corpus A/B used for #58: micro-tests cannot prove
# that a global change to &IF / &ELSE / &ENDIF end-offsets is net-safe on a
# real tree. This script runs `oxabl conformance --preprocess --json` over a module
# list and diffs pass/fail + error-pattern counts between baseline and candidate.
#
# Usage:
#   export CORPUS_ROOT=/path/to/abl/corpus       # required
#   export INCLUDE_PATHS="-I $CORPUS_ROOT"          # optional extra -I flags
#   export MODULES="ar ap gl …"                    # space-separated subdirs
#   export OXABL_BIN=./target/release/oxabl        # optional
#   export OUT_DIR=./target/corpus-ab              # optional
#
#   ./scripts/corpus-ab-gate.sh baseline   # write $OUT_DIR/ab-baseline.json
#   ./scripts/corpus-ab-gate.sh candidate  # write $OUT_DIR/ab-candidate.json
#   ./scripts/corpus-ab-gate.sh diff       # compare the two; exit 1 on regression
#
# Exit codes:
#   0  ok / no regression
#   1  regression (parse fails climbed) or check failures in a single run when
#      STRICT=1
#   2  usage / missing CORPUS_ROOT / missing modules
#   3  oxabl binary missing
#   4  the gate itself broke: a sub-run exited unexpectedly, produced no report,
#      or the aggregation examined zero files. Kept distinct from 1 on purpose —
#      "the parser regressed" and "the gate measured nothing" are different
#      answers, and a gate that cannot tell them apart reports PASS for a run
#      that verified nothing.

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT_DIR="${OUT_DIR:-$ROOT/target/corpus-ab}"
OXABL_BIN="${OXABL_BIN:-$ROOT/target/release/oxabl}"
if [[ ! -x "$OXABL_BIN" ]]; then
  OXABL_BIN="$ROOT/target/debug/oxabl"
fi

usage() {
  sed -n '2,30p' "$0" | sed 's/^# \?//'
  exit 2
}

need_corpus() {
  if [[ -z "${CORPUS_ROOT:-}" ]]; then
    echo "error: CORPUS_ROOT is not set." >&2
    echo "  Point it at the private ABL code corpus root used for the #58 A/B." >&2
    echo "  Example: export CORPUS_ROOT=/path/to/abl/corpus" >&2
    exit 2
  fi
  if [[ ! -d "$CORPUS_ROOT" ]]; then
    echo "error: CORPUS_ROOT does not exist: $CORPUS_ROOT" >&2
    exit 2
  fi
}

need_oxabl() {
  if [[ ! -x "$OXABL_BIN" ]]; then
    echo "error: oxabl binary not found at $OXABL_BIN" >&2
    echo "  Build with: cargo build --release -p oxabl" >&2
    exit 3
  fi
}

# Default module list is empty — callers must supply the same 9 modules used
# for the #58 gate. Leaving a hardcoded customer list out of the open-source
# tree is intentional.
run_check_modules() {
  local label=$1
  local out_json=$2
  need_corpus
  need_oxabl
  mkdir -p "$OUT_DIR"

  if [[ -z "${MODULES:-}" ]]; then
    # If MODULES unset, check the whole CORPUS_ROOT (full-tree mode).
    echo "warning: MODULES unset — checking entire CORPUS_ROOT" >&2
    MODULES="."
  fi

  local -a paths=()
  for m in $MODULES; do
    if [[ "$m" == "." ]]; then
      paths+=("$CORPUS_ROOT")
    else
      local p="$CORPUS_ROOT/$m"
      if [[ ! -d "$p" ]]; then
        echo "error: module directory missing: $p" >&2
        exit 2
      fi
      paths+=("$p")
    fi
  done

  # shellcheck disable=SC2086 # INCLUDE_PATHS is intentionally word-split
  local -a inc=()
  if [[ -n "${INCLUDE_PATHS:-}" ]]; then
    # shellcheck disable=SC2206
    inc=($INCLUDE_PATHS)
  else
    inc=(-I "$CORPUS_ROOT")
  fi

  local tmp_dir
  tmp_dir=$(mktemp -d)
  trap 'rm -rf "$tmp_dir"' RETURN

  local total_passed=0 total_failed=0 total_files=0
  local stderr_log="$tmp_dir/stderr.txt"
  : >"$stderr_log"

  echo "=== $label: oxabl conformance --preprocess over ${#paths[@]} path(s) ===" >&2
  echo "    bin=$OXABL_BIN" >&2
  echo "    corpus=$CORPUS_ROOT" >&2

  # Aggregate per-path JSON into one summary. oxabl conformance --json is one path
  # at a time; we merge.
  python3 - "$out_json" <<'PY' &
import json, sys, os
# placeholder — filled after runs via rewrite below
open(sys.argv[1], "w").write("{}")
PY
  wait || true

  local combined="$tmp_dir/combined.jsonl"
  : >"$combined"

  # A sub-run that never produced a report contributes nothing to the totals,
  # and the aggregation's zero-defaults turn that silence into "no failures".
  # So every sub-run is checked here instead: nothing may be skipped quietly.
  local gate_broken=0

  for p in "${paths[@]}"; do
    echo "--- checking $p ---" >&2
    local raw="$tmp_dir/$(echo "$p" | tr '/' '_').json"
    # Capture stdout JSON and stderr (PREPROC007 / other loud preproc lines).
    set +e
    "$OXABL_BIN" conformance --preprocess --json "${inc[@]}" "$p" \
      >"$raw" 2>>"$stderr_log"
    local rc=$?
    set -e
    echo "{\"path\": $(python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$p"), \"rc\": $rc}" >>"$combined"
    # `conformance` exits 0 when every file parsed and 1 when some did not.
    # Both are ordinary results this gate is built to measure. Anything else —
    # 2 usage, 6 serialize, or a signal (128+n) — means the measurement did not
    # happen, which is not a result at all.
    if [[ $rc -ne 0 && $rc -ne 1 ]]; then
      echo "error: conformance exited $rc for $p (expected 0 or 1)" >&2
      gate_broken=1
    fi
    if [[ -s "$raw" ]]; then
      python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); print(json.dumps({"path":sys.argv[2],"report":d}))' \
        "$raw" "$p" >>"$combined.reports"
    else
      echo "error: conformance produced no report for $p" >&2
      gate_broken=1
    fi
  done

  if [[ $gate_broken -ne 0 ]]; then
    echo "error: $label run is incomplete — refusing to write a summary that would" >&2
    echo "  read as zero failures for the paths that produced no data." >&2
    exit 4
  fi

  python3 - "$out_json" "$combined" "$combined.reports" "$stderr_log" <<'PY'
import json, sys, re, collections
from pathlib import Path

out_path, combined_path, reports_path, stderr_path = sys.argv[1:5]

passed = failed = files = 0
error_patterns = collections.Counter()
reports = []
if Path(reports_path).exists():
    for line in open(reports_path):
        line = line.strip()
        if not line:
            continue
        entry = json.loads(line)
        reports.append(entry)
        r = entry.get("report") or {}
        # Support both flat and nested shapes from oxabl conformance --json.
        p = int(r.get("passed", 0) or 0)
        f = int(r.get("failed", 0) or 0)
        t = int(r.get("total", p + f) or 0)
        passed += p
        failed += f
        files += t
        # oxabl conformance --json: error_patterns is [{pattern, count}, ...]
        pats = r.get("error_patterns") or []
        if isinstance(pats, dict):
            for k, v in pats.items():
                error_patterns[k] += int(v)
        elif isinstance(pats, list):
            for item in pats:
                if isinstance(item, dict):
                    error_patterns[item.get("pattern") or item.get("message") or "?"] += int(
                        item.get("count", 1)
                    )
                else:
                    error_patterns[str(item)] += 1

# stderr: count loud preprocess codes
preproc = collections.Counter()
if Path(stderr_path).exists():
    text = open(stderr_path, errors="replace").read()
    for m in re.finditer(r"\[preprocess (PREPROC\d+)\]", text):
        preproc[m.group(1)] += 1
    for m in re.finditer(r"\b(PREPROC\d+)\b", text):
        # also bare codes if present
        preproc[m.group(1)] += 0  # already counted above when tagged

# Heuristic: messages that look like parse errors
parse001 = sum(c for msg, c in error_patterns.items() if "PARSE" in msg.upper() or "Unexpected" in msg or "Expected" in msg or "parse error" in msg.lower())

summary = {
    "files": files,
    "passed": passed,
    "failed": failed,
    "parse_signal": parse001 if parse001 else failed,  # fallback: all fails
    "error_patterns": dict(error_patterns.most_common(50)),
    "preproc_codes": dict(preproc),
    "reports": reports,
}
Path(out_path).write_text(json.dumps(summary, indent=2) + "\n")
print(json.dumps({k: summary[k] for k in ("files", "passed", "failed", "parse_signal", "preproc_codes")}, indent=2))
PY

  # A gate that examined zero files has not verified anything, and its summary
  # would diff cleanly against any other summary. Fail loudly instead.
  local files
  files=$(python3 -c 'import json,sys; print(int(json.load(open(sys.argv[1])).get("files") or 0))' "$out_json")
  if [[ "$files" -eq 0 ]]; then
    echo "error: $label run examined 0 files — nothing was verified." >&2
    echo "  Check MODULES / CORPUS_ROOT and the conformance report shape." >&2
    exit 4
  fi

  echo "Wrote $out_json" >&2
}

diff_reports() {
  local a="$OUT_DIR/ab-baseline.json"
  local b="$OUT_DIR/ab-candidate.json"
  if [[ ! -f "$a" || ! -f "$b" ]]; then
    echo "error: need both $a and $b (run baseline then candidate)" >&2
    exit 2
  fi
  python3 - "$a" "$b" <<'PY'
import json, sys
a = json.load(open(sys.argv[1]))
b = json.load(open(sys.argv[2]))

def n(d, k):
    return int(d.get(k) or 0)

print("=== Corpus A/B diff (criterion 7) ===")
print(f"  files:   A={n(a,'files')}  B={n(b,'files')}")
print(f"  passed:  A={n(a,'passed')}  B={n(b,'passed')}  Δ={n(b,'passed')-n(a,'passed'):+d}")
print(f"  failed:  A={n(a,'failed')}  B={n(b,'failed')}  Δ={n(b,'failed')-n(a,'failed'):+d}")
print(f"  parse_signal: A={n(a,'parse_signal')}  B={n(b,'parse_signal')}  Δ={n(b,'parse_signal')-n(a,'parse_signal'):+d}")
print(f"  preproc A: {a.get('preproc_codes')}")
print(f"  preproc B: {b.get('preproc_codes')}")

fail_delta = n(b, "failed") - n(a, "failed")
parse_delta = n(b, "parse_signal") - n(a, "parse_signal")

# New error patterns
ap = a.get("error_patterns") or {}
bp = b.get("error_patterns") or {}
new = {k: bp[k] for k in bp if k not in ap}
climbed = {k: bp[k] - ap.get(k, 0) for k in bp if bp[k] > ap.get(k, 0)}
if climbed:
    print("  patterns that climbed:")
    for k, d in sorted(climbed.items(), key=lambda kv: -kv[1])[:15]:
        print(f"    +{d}  {k[:120]}")
if new:
    print("  new patterns (not in baseline):")
    for k, c in sorted(new.items(), key=lambda kv: -kv[1])[:15]:
        print(f"    {c}  {k[:120]}")

pre_a = (a.get("preproc_codes") or {}).get("PREPROC002", 0)
pre_b = (b.get("preproc_codes") or {}).get("PREPROC002", 0)
print(f"  PREPROC002: A={pre_a}  B={pre_b}  Δ={pre_b-pre_a:+d}")

regressed = fail_delta > 0 or parse_delta > 0
if regressed:
    print("RESULT: FAIL — parse failures climbed (criterion 7)")
    sys.exit(1)
print("RESULT: PASS — no net-new parse failures")
if pre_b > pre_a:
    print("note: PREPROC002 rose; investigate even if parse count is flat")
sys.exit(0)
PY
}

cmd=${1:-}
case "$cmd" in
  baseline)  run_check_modules baseline "$OUT_DIR/ab-baseline.json" ;;
  candidate) run_check_modules candidate "$OUT_DIR/ab-candidate.json" ;;
  diff)      diff_reports ;;
  *)         usage ;;
esac
