#!/usr/bin/env bash
# acme-build: a made-up wrapper that owns the environment of a workspace and
# hands the compile to `oxabl build`.
#
#   acme_build.sh <oxabl> <root> <worker>
#
# <worker> starts one worker; it stands for the `docker exec` of a real wrapper.
set -euo pipefail

oxabl=$1
root=$2
worker=$3
state="$root/.acme"
mkdir -p "$state"

cat >"$state/overlay.toml" <<TOML
[workspace.sources]
include_paths = ["src"]

[workspace.preprocessor]
proversion = "12.8"
databases = []

[build]
output = "build/r"

[build.workers]
command = ["$worker", "{program}"]
root = "$root"
count = 2
unit_timeout = "2m"
TOML

coproc BUILD {
  "$oxabl" build --root "$root" --config-overlay "$state/overlay.toml" \
    --events --report "$state/report.json" 2>"$state/oxabl.log"
}
build=$BUILD_PID

while IFS= read -r line <&"${BUILD[0]}"; do
  [[ $line =~ \"event\":\"([a-z-]+)\" ]] || continue
  event=${BASH_REMATCH[1]}
  path=
  if [[ $line =~ \"path\":\"([^\"]*)\" ]]; then path=${BASH_REMATCH[1]}; fi
  case $event in
    build-started)
      [[ $line =~ \"rebuild\":([0-9]+) ]] && echo "rebuild ${BASH_REMATCH[1]}"
      ;;
    unit-fetched) echo "fetched $path" ;;
    message)
      [[ $line =~ \"type\":\"([a-z]+)\" ]] && echo "message ${BASH_REMATCH[1]} $path"
      ;;
    unit-finished)
      [[ $line =~ \"status\":\"([a-z]+)\" ]] && echo "finished ${BASH_REMATCH[1]} $path"
      ;;
    worker-lost) echo "worker lost" ;;
    summary)
      [[ $line =~ \"compiled\":([0-9]+) ]] && compiled=${BASH_REMATCH[1]}
      [[ $line =~ \"failed\":([0-9]+) ]] && failed=${BASH_REMATCH[1]}
      echo "summary compiled=$compiled failed=$failed"
      ;;
  esac
done

status=0
wait "$build" || status=$?

echo "oxabl exit $status"
echo "report ok=$(grep -c '"status": "ok"' "$state/report.json" || true)" \
  "failed=$(grep -c '"status": "fail"' "$state/report.json" || true)" \
  "exit=$(sed -n 's/^  "exit": \([0-9]*\),$/\1/p' "$state/report.json")"
exit "$status"
