#!/usr/bin/env bash
#
# Package the extension as a VSIX and write its SHA-256. CI and the release
# workflow both call this, so a release ships what CI built.
#
#   bash clients/vscode/scripts/package-vsix.sh <out-dir>
#
# writes <out-dir>/oxabl-vscode-<version>.vsix and
# <out-dir>/oxabl-vscode-<version>.vsix.sha256 (sha256sum format). The
# committed schemas/oxabl.schema.json is packaged as is: CI's schema-drift job
# keeps it current. The script fails when the VSIX misses a file the extension
# loads, or carries one it must not ship.

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <out-dir>" >&2
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXT_DIR="$(dirname "$SCRIPT_DIR")"
mkdir -p "$1"
OUT_DIR="$(cd "$1" && pwd)"

cd "$EXT_DIR"
pnpm install --frozen-lockfile
pnpm run build

version="$(node -p "require('./package.json').version")"
vsix="oxabl-vscode-$version.vsix"
pnpm exec vsce package --no-dependencies --out "$OUT_DIR/$vsix"

entries="$(unzip -Z1 "$OUT_DIR/$vsix")"
missing=0
for required in \
  extension/package.json \
  extension/out/extension.js \
  extension/language-configuration.json \
  extension/syntaxes/abl.tmLanguage.json \
  extension/syntaxes/abl.markdown.tmLanguage.json \
  extension/schemas/oxabl.schema.json; do
  if ! grep -qxF "$required" <<<"$entries"; then
    echo "!! $vsix is missing $required" >&2
    missing=1
  fi
done
if grep -E '^extension/(src|test|node_modules|scripts|\.vscode-test|dist)/' <<<"$entries" >&2; then
  echo "!! $vsix ships development files (listed above)" >&2
  missing=1
fi
if [[ "$missing" -ne 0 ]]; then
  exit 1
fi

(cd "$OUT_DIR" && sha256sum "$vsix" >"$vsix.sha256")
echo "==> $OUT_DIR/$vsix"
