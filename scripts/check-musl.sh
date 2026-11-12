#!/usr/bin/env bash
# Starts a binary from build-musl.sh in an ubuntu:22.04 container (glibc 2.35, the
# usual OpenEdge image) and checks that it is static. Fails when the binary does not
# start, when `oxabl build` or `oxabl self-update` is missing, or when it links a
# shared library.
set -euo pipefail

bin=${1:?usage: check-musl.sh <binary>}
bin=$(cd "$(dirname "$bin")" && pwd)/$(basename "$bin")

docker run --rm -v "$bin:/oxabl:ro" ubuntu:22.04 bash -euc '
  apt-get update -qq
  apt-get install -y -qq --no-install-recommends file >/dev/null
  /oxabl --version
  /oxabl build --help >/dev/null
  /oxabl self-update --help | grep -q -- --check
  # The release binary has the self-update feature: against a closed loopback
  # port the check is "not established" (3). Without the feature the command
  # exits 2. No network is used.
  code=0
  OXABL_UPDATE_API_URL=http://127.0.0.1:9 /oxabl self-update --check >/dev/null 2>&1 || code=$?
  [ "$code" -eq 3 ]
  file -L /oxabl
  # file says "static-pie linked" for a static PIE, "statically linked" otherwise.
  file -L /oxabl | grep -Eq "static-pie linked|statically linked"
  # A static-pie file makes ldd print "statically linked"; a plain static one
  # makes it exit with "not a dynamic executable".
  ldd /oxabl 2>&1 | grep -Eq "statically linked|not a dynamic executable"
'
