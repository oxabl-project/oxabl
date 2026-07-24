#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output_dir="${1:-$repo_root/target/wasm-web}"

if ! command -v wasm-bindgen >/dev/null 2>&1; then
  echo "wasm-bindgen CLI is required (cargo install wasm-bindgen-cli --version 0.2.108 --locked)" >&2
  exit 1
fi

mkdir -p "$output_dir"

cargo build \
  --manifest-path "$repo_root/Cargo.toml" \
  --package oxabl_wasm \
  --target wasm32-unknown-unknown \
  --release

wasm-bindgen \
  "$repo_root/target/wasm32-unknown-unknown/release/oxabl_wasm.wasm" \
  --target web \
  --out-dir "$output_dir" \
  --out-name oxabl_wasm \
  --no-typescript
