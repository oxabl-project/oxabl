#!/usr/bin/env bash
# Write the `COMPILE ... LISTING` output of ABL source files, for the
# scope_listing harness (crates/oxabl/examples/scope_listing.rs). Requires $DLC
# to point at an OpenEdge installation with a licence that permits compiling;
# the listings cannot be produced without one.
#
#   DLC=/path/to/dlc scripts/gen-scope-listings.sh <src-dir> <file-list> <out-dir> [_progres args...]
#
# <file-list> holds one source path per line, relative to <src-dir> (for
# example the output of `cd src && find . -name '*.p' | shuf -n 1000`). Each
# listing is written to <out-dir>/<path>.lst, so the harness finds it with
# OXABL_SCOPE_LISTING_DIR=<out-dir>. The compile runs in <src-dir>, with
# <src-dir> first on PROPATH, then $PROPATH. Pass the database connection the
# code compiles against as the trailing arguments (`-db sports2000 -1`, or
# `-pf connect.pf`); the compiler needs the schema. A file that does not
# compile is listed on stderr and gets no listing.
set -euo pipefail

: "${DLC:?set DLC to an OpenEdge installation directory}"
if [ "$#" -lt 3 ]; then
  sed -n '2,15p' "$0" >&2
  exit 2
fi

src="$(cd "$1" && pwd)"
list="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
mkdir -p "$3"
out="$(cd "$3" && pwd)"
shift 3

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# One directory per listing, made here: OS-CREATE-DIR does not make parents.
while IFS= read -r f || [ -n "$f" ]; do
  f="${f#./}"
  [ -n "$f" ] && mkdir -p "$out/$(dirname "$f")"
done < "$list"

cat > "$work/compile-listings.p" <<'ABL'
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOk   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iBad  AS INTEGER   NO-UNDO.
DEFINE STREAM sList.

INPUT STREAM sList FROM VALUE(OS-GETENV("OXABL_LIST")).
REPEAT:
  IMPORT STREAM sList UNFORMATTED cFile.
  IF cFile BEGINS "./" THEN cFile = SUBSTRING(cFile, 3).
  IF TRIM(cFile) = "" THEN NEXT.
  COMPILE VALUE(cFile) LISTING VALUE(OS-GETENV("OXABL_OUT") + "/" + cFile + ".lst") NO-ERROR.
  IF COMPILER:ERROR OR ERROR-STATUS:ERROR THEN DO:
    iBad = iBad + 1.
    PUT UNFORMATTED "FAILED " cFile SKIP.
  END.
  ELSE iOk = iOk + 1.
END.
INPUT STREAM sList CLOSE.
PUT UNFORMATTED "compiled " iOk ", failed " iBad SKIP.
QUIT.
ABL

cd "$src"
OXABL_LIST="$list" \
OXABL_OUT="$out" \
PROPATH="$src${PROPATH:+,$PROPATH}" \
PROCFG="$DLC/progress.cfg" \
PROMSGS="$DLC/promsgs" \
PROTERMCAP="$DLC/protermcap" \
  "$DLC/bin/_progres" -b -p "$work/compile-listings.p" "$@" \
  | while IFS= read -r line; do
      case "$line" in
        FAILED*) echo "$line" >&2 ;;
        *) echo "$line" ;;
      esac
    done
