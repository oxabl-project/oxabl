#!/usr/bin/env bash
# extract-statements.sh — deterministic, repeatable extraction of ABL statements
# of a chosen kind into a CSV, using the oxabl parser/AST.
#
# It builds (idempotently) and runs the `find_statements` example, which walks
# the `oxabl_ast` for every ABL file under a path, finds statements matching a
# `StatementKind` variant name, and writes:  file,kind,line_start,line_end,content
#
# The statement we look for is a single knob — change STMT_KIND (or --kind) to
# retarget: Message, Display, Run, Assignment, Case, ForEach, Procedure, …
# (any `oxabl_ast::StatementKind` variant name).
#
# Usage:
#   scripts/extract-statements.sh <scan-root> [output.csv] [statement-kind]
#
# Or via env vars (flags/positionals win over env):
#   STMT_KIND=Display SCAN_ROOT=/path/to/erp OUT_CSV=display.csv \
#     scripts/extract-statements.sh
#
# Env knobs:
#   STMT_KIND   statement variant to match      (default: Message)
#   SCAN_ROOT   directory (or file) to scan      (default: .)
#   OUT_CSV     output CSV path                   (default: <kind>_statements.csv)
#   EXTS        comma-separated extensions        (default: p,w,cls,i,v)
#   OXABL_DIR   oxabl checkout                     (default: dir above this script)
#
# Exit codes: 0 ok · 2 usage/no files · 1 build or write failure.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OXABL_DIR="${OXABL_DIR:-$(cd "$SCRIPT_DIR/.." && pwd)}"

SCAN_ROOT="${1:-${SCAN_ROOT:-.}}"
STMT_KIND="${3:-${STMT_KIND:-Message}}"
OUT_CSV="${2:-${OUT_CSV:-${STMT_KIND}_statements.csv}}"
EXTS="${EXTS:-p,w,cls,i,v}"

if [[ ! -d "$SCAN_ROOT" && ! -f "$SCAN_ROOT" ]]; then
  echo "error: scan root does not exist: $SCAN_ROOT" >&2
  exit 2
fi

# Resolve OUT_CSV to an absolute path *before* we build (build runs in $OXABL_DIR).
mkdir -p "$(dirname "$OUT_CSV")"
OUT_CSV_ABS="$(cd "$(dirname "$OUT_CSV")" && pwd)/$(basename "$OUT_CSV")"
SCAN_ROOT_ABS="$(cd "$SCAN_ROOT" 2>/dev/null && pwd || echo "$SCAN_ROOT")"

echo "Building find_statements (release)…" >&2
cargo build --release --manifest-path "$OXABL_DIR/Cargo.toml" \
  --example find_statements >&2

BIN="$OXABL_DIR/target/release/examples/find_statements"

echo "Extracting '$STMT_KIND' statements from $SCAN_ROOT_ABS -> $OUT_CSV_ABS" >&2
"$BIN" "$SCAN_ROOT_ABS" --kind "$STMT_KIND" --ext "$EXTS" --out "$OUT_CSV_ABS"

echo "Wrote $OUT_CSV_ABS" >&2
