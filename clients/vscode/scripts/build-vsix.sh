#!/usr/bin/env bash
#
# Build a sideloadable .vsix for the oxabl VS Code extension (U6).
#
# Steps (all via pnpm, per repo convention — never npm/npx):
#   1. Regenerate schemas/oxabl.schema.json from the Rust config structs.
#      We call `cargo run -p oxabl -- schema` directly, NOT a PATH `oxabl`: a
#      fresh clone / CI has no installed binary (U5).
#   2. Package it with scripts/package-vsix.sh into dist/, the script CI and
#      the release use (a single esbuild bundle, `vsce package
#      --no-dependencies`: vsce cannot walk pnpm's symlinked node_modules).
#   3. With --install, sideload the built VSIX via `code --install-extension`.
#
# Usage: bash clients/vscode/scripts/build-vsix.sh [--install]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXT_DIR="$(dirname "$SCRIPT_DIR")"
REPO_ROOT="$(cd "$EXT_DIR/../.." && pwd)"

INSTALL=0
for arg in "$@"; do
  case "$arg" in
    --install) INSTALL=1 ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

cd "$EXT_DIR"

echo "==> Regenerating oxabl.toml JSON schema (cargo run -p oxabl -- schema)"
mkdir -p schemas
( cd "$REPO_ROOT" && cargo run --quiet -p oxabl -- schema ) > schemas/oxabl.schema.json

echo "==> Packaging VSIX (scripts/package-vsix.sh dist)"
bash "$SCRIPT_DIR/package-vsix.sh" dist

VSIX="$(ls -t dist/*.vsix | head -n1)"
echo "==> Built: $VSIX"

if [[ "$INSTALL" -eq 1 ]]; then
  if command -v code >/dev/null 2>&1; then
    echo "==> Installing into VS Code (code --install-extension)"
    code --install-extension "$VSIX" --force
  else
    echo "!! 'code' CLI not found on PATH; skipping install. VSIX is at $VSIX" >&2
  fi
fi
