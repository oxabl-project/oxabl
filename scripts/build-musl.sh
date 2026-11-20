#!/usr/bin/env bash
# Builds the static oxabl binary with the remote cache and self-update, for
# x86_64-unknown-linux-musl, in a rust:alpine container. It writes
# oxabl-x86_64-unknown-linux-musl and its .sha256 file into <out dir>. The binary
# does not need glibc, so it starts in any Linux image.
#
# OXABL_MUSL_CACHE names a directory for the cargo registry and the target
# directory between runs. It defaults to <out dir>/.cache.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:?usage: build-musl.sh <out dir>}
mkdir -p "$out"
out=$(cd "$out" && pwd)
cache=${OXABL_MUSL_CACHE:-$out/.cache}
mkdir -p "$cache/registry" "$cache/target"
name=oxabl-x86_64-unknown-linux-musl

# musl-dev, perl, make and cmake build jemalloc and the crypto crates. The
# container user is root, so the script gives the files back at the end.
docker run --rm \
  -v "$root:/src:ro" \
  -v "$cache/registry:/usr/local/cargo/registry" \
  -v "$cache/target:/target" \
  -v "$out:/out" \
  -e CARGO_TARGET_DIR=/target \
  -e CARGO_BUILD_JOBS="${CARGO_BUILD_JOBS:-8}" \
  -e CARGO_TERM_COLOR=always \
  -e "OWNER=$(id -u):$(id -g)" \
  -e "NAME=$name" \
  -w /src \
  rust:1-alpine \
  sh -euc '
    apk add --no-cache musl-dev perl make cmake
    cargo build --release --locked -p oxabl --features remote-cache,self-update
    cp /target/release/oxabl "/out/$NAME"
    chown -R "$OWNER" /out /target /usr/local/cargo/registry
  '

(cd "$out" && sha256sum "$name" >"$name.sha256")
"$out/$name" --version
