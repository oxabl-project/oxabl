#!/usr/bin/env bash
# Runs one target for a time box, with memory and time limits, and keeps going
# after a finding. Needs cargo-fuzz and the nightly toolchain.
#   fuzz/run.sh <target> [seconds]   artifacts land in fuzz/artifacts/<target>
set -euo pipefail
target=${1:?target: lex | preprocess_parse | lint | format | outline | navigate}
seconds=${2:-600}
cd "$(dirname "$0")"
# A main thread of the analysis stack size (64 MiB): the targets then run each
# input directly, without a hop to a worker thread (src/lib.rs).
ulimit -s 65536 2>/dev/null || true
# -a keeps debug assertions on: a debug_assert in the parser or a pass is a
# finding too.
# The host triple, spelled out: cargo-fuzz builds for the triple it was built
# for, and a prebuilt cargo-fuzz (cargo-binstall, as in CI) is a musl binary,
# where the sanitizer cannot link.
host=$(rustc +nightly -vV | sed -n 's/^host: //p')
exec nice -n 10 cargo +nightly fuzz run "$target" --target "$host" -O -a -- \
  -max_total_time="$seconds" -rss_limit_mb=2048 -timeout=10 -max_len=65536 \
  -fork=2 -ignore_crashes=1 -ignore_timeouts=1 -ignore_ooms=1
