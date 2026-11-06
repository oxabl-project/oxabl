#!/usr/bin/env bash
# Starts a binary from build-musl.sh in an ubuntu:22.04 container (glibc 2.35, the
# usual OpenEdge image) and checks that it is static. Fails when the binary does not
# start, when `oxabl build` is missing, or when it links a shared library.
set -euo pipefail

bin=${1:?usage: check-musl.sh <binary>}
bin=$(cd "$(dirname "$bin")" && pwd)/$(basename "$bin")

docker run --rm -v "$bin:/oxabl:ro" ubuntu:22.04 bash -euc '
  apt-get update -qq
  apt-get install -y -qq --no-install-recommends file >/dev/null
  /oxabl --version
  /oxabl build --help >/dev/null
  file -L /oxabl
  file -L /oxabl | grep -q "statically linked"
  # A static-pie file makes ldd print "statically linked"; a plain static one
  # makes it exit with "not a dynamic executable".
  ldd /oxabl 2>&1 | grep -Eq "statically linked|not a dynamic executable"
'
