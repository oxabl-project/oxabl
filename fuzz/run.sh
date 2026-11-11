#!/usr/bin/env bash
# Runs one target for a time box, with memory and time limits, and keeps going
# after a finding. Needs cargo-fuzz and the nightly toolchain.
#   fuzz/run.sh <target> [seconds]   artifacts land in fuzz/artifacts/<target>
set -euo pipefail
target=${1:?target: lex | preprocess_parse | lint | format}
seconds=${2:-600}
cd "$(dirname "$0")"
exec nice -n 10 cargo +nightly fuzz run "$target" -O -- \
  -max_total_time="$seconds" -rss_limit_mb=2048 -timeout=10 -max_len=65536 \
  -fork=2 -ignore_crashes=1 -ignore_timeouts=1 -ignore_ooms=1
