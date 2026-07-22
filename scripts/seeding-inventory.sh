#!/usr/bin/env bash
# seeding-inventory.sh — deterministic, repeatable extraction of the span-seeding
# placement inventory for a target module, using the oxabl parser/AST.
#
# It builds (idempotently) and runs the `seeding_inventory` example, which walks
# the `oxabl_ast` for every ABL file under a path and, for each PROCEDURE,
# FUNCTION, and class METHOD, emits one CSV row with the exact placement the
# TypeScript span applier needs:
#
#   file,kind,span_name,seedable,flag_reason,is_abstract,has_finally,
#   extent_line_start,extent_col_start,extent_line_end,extent_col_end,
#   decl_line,decl_col,end_line,end_col,merge_line,merge_col
#
# Non-seedable routines (abstract methods, interface members, empty/ambiguous
# placement) are emitted with seedable=false and a flag_reason, never dropped,
# so the downstream report reconciles.
#
# Usage:
#   scripts/seeding-inventory.sh <scan-root> [output.csv]
#
# Or via env vars (positionals win over env):
#   SCAN_ROOT=/path/to/module OUT_CSV=module-seeding.csv scripts/seeding-inventory.sh
#
# Env knobs:
#   SCAN_ROOT   directory (or file) to scan      (default: .)
#   OUT_CSV     output CSV path                   (default: seeding_inventory.csv)
#   EXTS        comma-separated extensions        (default: p,w,cls,i,v)
#   OXABL_DIR   oxabl checkout                     (default: dir above this script)
#
# Exit codes: 0 ok · 2 usage/no files · 1 build or write failure.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OXABL_DIR="${OXABL_DIR:-$(cd "$SCRIPT_DIR/.." && pwd)}"

SCAN_ROOT="${1:-${SCAN_ROOT:-.}}"
OUT_CSV="${2:-${OUT_CSV:-seeding_inventory.csv}}"
EXTS="${EXTS:-p,w,cls,i,v}"

if [[ ! -d "$SCAN_ROOT" && ! -f "$SCAN_ROOT" ]]; then
  echo "error: scan root does not exist: $SCAN_ROOT" >&2
  exit 2
fi

mkdir -p "$(dirname "$OUT_CSV")"
OUT_CSV_ABS="$(cd "$(dirname "$OUT_CSV")" && pwd)/$(basename "$OUT_CSV")"
SCAN_ROOT_ABS="$(cd "$SCAN_ROOT" 2>/dev/null && pwd || echo "$SCAN_ROOT")"

echo "Building seeding_inventory (release)…" >&2
cargo build --release --manifest-path "$OXABL_DIR/Cargo.toml" \
  --example seeding_inventory >&2

BIN="$OXABL_DIR/target/release/examples/seeding_inventory"

echo "Inventorying routines in $SCAN_ROOT_ABS -> $OUT_CSV_ABS" >&2
"$BIN" "$SCAN_ROOT_ABS" --ext "$EXTS" --out "$OUT_CSV_ABS"

echo "Wrote $OUT_CSV_ABS" >&2
