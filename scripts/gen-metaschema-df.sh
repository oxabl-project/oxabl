#!/usr/bin/env bash
# Regenerate crates/oxabl_schema/resources/metaschema.df from a live OpenEdge
# installation. Requires $DLC to point at an OpenEdge install with a licence
# that permits running _progres; there is no way to derive this data without
# one, which is why the result is checked in.
#
#   DLC=/path/to/dlc scripts/gen-metaschema-df.sh
#
# The dump runs against a scratch copy of the shipped `empty.db`, which has no
# application schema -- so every table it reports is one that every OpenEdge
# database has.
set -euo pipefail

: "${DLC:?set DLC to an OpenEdge installation directory}"

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cp "$DLC"/empty.db "$DLC"/empty.b1 "$DLC"/empty.d1 "$work/"

version="$(sed -n 's/^OpenEdge Release \([0-9.]*\).*/\1/p;q' "$DLC/version")"
version="${version:-unknown}"

OXABL_METASCHEMA_OUT="$work/metaschema.txt" \
PROCFG="$DLC/progress.cfg" \
PROMSGS="$DLC/promsgs" \
PROTERMCAP="$DLC/protermcap" \
  "$DLC/bin/_progres" -b -db "$work/empty.db" -1 \
    -p "$repo_root/scripts/dump-metaschema.p"

python3 "$repo_root/scripts/gen-metaschema-df.py" \
  "$work/metaschema.txt" \
  "$repo_root/crates/oxabl_schema/resources/metaschema.df" \
  "$version"
