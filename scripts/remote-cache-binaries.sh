#!/usr/bin/env bash
# Builds oxabl-cache-server and oxabl-cache-admin at the revision that Cargo.toml
# pins for the oxabl-cache client, into <install dir>/bin. The remote-cache tests
# take both from OXABL_CACHE_SERVER_BIN and OXABL_CACHE_ADMIN_BIN. It builds
# nothing when <install dir> already holds that revision.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
dir=${1:?usage: remote-cache-binaries.sh <install dir>}
rev=$(sed -n 's/^oxabl-cache-client = .*rev = "\([0-9a-f]\{40\}\)".*/\1/p' "$root/Cargo.toml")
if [ -z "$rev" ]; then
  echo "Cargo.toml pins no oxabl-cache revision" >&2
  exit 1
fi
if [ "$(cat "$dir/rev" 2>/dev/null)" = "$rev" ] \
  && [ -x "$dir/bin/oxabl-cache-server" ] && [ -x "$dir/bin/oxabl-cache-admin" ]; then
  echo "oxabl-cache $rev is built in $dir"
  exit 0
fi
rm -f "$dir/rev"
cargo install --git https://github.com/oxabl-project/oxabl-cache --rev "$rev" --locked --force \
  --root "$dir" oxabl-cache-server oxabl-cache-admin
echo "$rev" >"$dir/rev"
