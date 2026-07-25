#!/usr/bin/env bash
#
# Build the browser artifact into $output_dir (default target/wasm-web).
#
# Usage: build-wasm.sh [output_dir] [--verify]
#
#   --verify  also enable the `debug-panic` cargo feature, adding a
#             `debug_panic()` export. No ABL input reaches a parser panic, so
#             that export is the only way to exercise the playground's
#             crash-and-recover path by hand. NEVER use it for a release build —
#             the release job in .github/workflows/release.yml does not.
#
# After bindgen, two assertions guard the recovery machinery. Neither is
# cosmetic: no CI job runs wasm-bindgen at all (the `wasm` job stops at `cargo
# build`), so this script is the only thing standing between a silent
# regression and a deploy.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

output_dir=""
verify=0
for arg in "$@"; do
  case "$arg" in
    --verify) verify=1 ;;
    *) output_dir="$arg" ;;
  esac
done
output_dir="${output_dir:-$repo_root/target/wasm-web}"

if ! command -v wasm-bindgen >/dev/null 2>&1; then
  echo "wasm-bindgen CLI is required (cargo install wasm-bindgen-cli --version 0.2.126 --locked)" >&2
  exit 1
fi

mkdir -p "$output_dir"

cargo_features=()
if [[ $verify -eq 1 ]]; then
  cargo_features=(--features debug-panic)
  echo "build-wasm: VERIFICATION build — includes the debug_panic() export. Do not ship." >&2
fi

cargo build \
  --manifest-path "$repo_root/Cargo.toml" \
  --package oxabl_wasm \
  --target wasm32-unknown-unknown \
  --release \
  "${cargo_features[@]}"

# `--experimental-reset-state-function` exports `__wbg_reset_state`, which the
# website calls to swap in a fresh instance (with Rust statics reset and the
# start function re-run) after a panic traps the module. It is the whole browser
# recovery mechanism. Deliberately NOT paired with `--force-enable-abort-handler`:
# that flag would make the generator inject try_table/exnref instructions and two
# WebAssembly.Tag imports, raising the browser floor to roughly Chrome 128 /
# Firefox 131 / Safari 18.4, where the module fails to *instantiate*. Assertion 2
# below is what keeps that from creeping back in.
wasm-bindgen \
  "$repo_root/target/wasm32-unknown-unknown/release/oxabl_wasm.wasm" \
  --target web \
  --out-dir "$output_dir" \
  --out-name oxabl_wasm \
  --no-typescript \
  --experimental-reset-state-function

glue="$output_dir/oxabl_wasm.js"
wasm="$output_dir/oxabl_wasm_bg.wasm"

if [[ ! -f $glue ]]; then
  echo "build-wasm: expected generated glue at $glue" >&2
  exit 1
fi

# Assertion 1 — the recovery entry point is actually exported.
#
# This is the failure mode that fails silently otherwise: drop
# `--experimental-reset-state-function` above and the artifact still builds
# fine, the website's guard calls a function that is not there, and the
# playground quietly stops healing. Nothing else would catch it.
#
# Match the *export statement*, not the bare name. wasm-bindgen copies Rust doc
# comments into the glue as JSDoc, so a plain name match is satisfied by a doc
# comment that merely mentions `__wbg_reset_state` — which is how this assertion
# first passed a build that had no such export at all.
if ! grep -qE '^export function __wbg_reset_state *\(' "$glue"; then
  echo "build-wasm: FAILED — __wbg_reset_state is not exported by $glue." >&2
  echo "  The browser recovery path calls it after a panic traps the module." >&2
  echo "  Was --experimental-reset-state-function dropped from the wasm-bindgen" >&2
  echo "  invocation, or withdrawn upstream from the pinned 0.2.126 CLI?" >&2
  exit 1
fi

# Assertion 2 — the browser floor did not move.
#
# Exception-handling instructions would only appear via
# `--force-enable-abort-handler`, which was rejected: it costs visitors on older
# engines a hard instantiation failure and buys nothing reset-state does not
# already provide. This pins that decision so a future change cannot quietly
# trade the floor away.
if grep -q 'WebAssembly.Tag' "$glue"; then
  echo "build-wasm: FAILED — $glue imports WebAssembly.Tag." >&2
  echo "  That means exception-handling instructions were injected and the" >&2
  echo "  browser floor just rose to ~Chrome 128 / Firefox 131 / Safari 18.4," >&2
  echo "  where the module fails to instantiate rather than degrading." >&2
  echo "  Remove --force-enable-abort-handler; recovery uses reset-state." >&2
  exit 1
fi

# The same check from the other side, by symbol. Only these two come from the
# abort handler. Note what is deliberately NOT in this list: `__wbg_call_guard`
# and `__wbg_reinit_scheduled` are *reset-state* machinery and are expected here
# — `__wbg_call_guard` only tests the scheduled flag and calls
# `__wbg_reset_state`. Matching on those would fail every correct build.
if grep -qE '__wbg_handle_catch|__wbg_call_abort_hook' "$glue"; then
  echo "build-wasm: FAILED — $glue carries abort-handler machinery." >&2
  echo "  Same cause and same cost as the WebAssembly.Tag check above." >&2
  exit 1
fi

if [[ -f $wasm ]] && command -v wasm-objdump >/dev/null 2>&1; then
  if wasm-objdump -x "$wasm" 2>/dev/null | grep -qE 'try_table|exnref'; then
    echo "build-wasm: FAILED — $wasm contains exception-handling instructions." >&2
    exit 1
  fi
fi

echo "build-wasm: ok — reset-state exported, no exception-handling injected."
