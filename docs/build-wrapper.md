# Drive `oxabl build` from a wrapper

A wrapper is a tool that knows the environment of a workspace: its containers, its databases and its PROPATH. It does not compile. It tells `oxabl build` what the environment is, and `oxabl build` does the rest: the dependency graph, the plan, the compile order, the workers, the caches and the outputs.

This guide uses a made-up wrapper named `acme-build`. It shows what a wrapper writes into an **overlay file**, how it runs `oxabl build`, and how it reads the result. For the format of the events and the report, see [What `oxabl build` reports](build-output.md).

## Who does what

| The wrapper | `oxabl build` |
| --- | --- |
| Finds the workspace root. | Reads `oxabl.toml` and the overlay. |
| Starts the container and the databases, or checks that they run. | Starts the workers with the command that the wrapper gives. |
| Copies the OpenEdge include sources out of the container, once for each OpenEdge version. | Refreshes the dependency graph, makes the plan and the compile keys. |
| Writes the overlay file. | Compiles in dependency order, retries, handles a lost worker and a cancel. |
| Runs `oxabl build` and shows the events in its own interface. | Fills and reads the compile caches. |
| Keeps the report. | Installs the r-code and the other outputs. |

Oxabl does not start containers, does not connect databases and never sees a database password.

## The overlay

An overlay is a TOML file with the same schema as `oxabl.toml`. You name it with `--config-overlay <file>` or with the environment variable `OXABL_BUILD_OVERLAY` (the flag wins). Oxabl merges it over `oxabl.toml` before it reads any value:

- Tables merge key by key. The overlay changes only the keys that it names.
- An array or a single value in the overlay **replaces** the value in `oxabl.toml`. Arrays do not join.
- A relative path in the overlay is relative to the workspace root, not to the folder of the overlay.
- After the merge, oxabl reads the result strictly: an unknown key is an error.

The repository commits `oxabl.toml` for what is true for every machine (the source directories, for example). The wrapper writes the overlay for what is true for one machine and one run.

### What an overlay can set

An overlay can set:

- `[workspace]` `name`
- `[workspace.sources]`
- `[workspace.preprocessor]`
- `[workspace.schema]`
- `[build]` and its tables

Oxabl stops with exit code 2, and builds nothing, when the overlay:

- sets another table, for example `[workspace.lint]` or `[workspace.style]`;
- has a key that does not exist, in any table;
- has a key whose name looks like a secret (it contains `password`, `passwd`, `secret`, `token`, `apikey`, `privatekey`, `accesskey` or `credential`, in any case, with or without `-` or `_`). Oxabl checks `oxabl.toml` in the same way;
- names a trust setting of the remote cache (`tenant`, `cache`, `owner_keys` or `min_roster_version`) in `[build.cache]`, or has a `[build.cache.remote]` table. These settings live only in `.oxabl/cache.toml` (see [The remote cache](#the-remote-cache)). Oxabl refuses them in `oxabl.toml` too.

The error names the file and the key. It never prints the value of a key that looks like a secret.

### No `oxabl.toml`

When the workspace has no `oxabl.toml`, the overlay is the whole configuration. It must set `[workspace]` `name`. Pass `--root`, because oxabl finds a root only by looking for `oxabl.toml`.

### Which value wins

For one setting, the first of these that has a value wins:

1. A command-line flag (`--workers`, `--no-cache`, `--no-remote-cache`).
2. An environment variable (`OXABL_DLC_SOURCES` for `[workspace.sources].dlc`, `OXABL_BUILD_CACHE_DIR` for `[build.cache].local`).
3. The overlay.
4. `oxabl.toml`.
5. The default.

### Check the result

`oxabl build --root <root> --config-overlay <file> --print-config` prints the settings that `oxabl build` reads, and stops. It starts no worker and writes nothing. Each line has the dotted name of a setting, its value and where the value came from:

```text
# root: /srv/acme/ledger
# oxabl.toml: /srv/acme/ledger/oxabl.toml
# overlay: /srv/acme/ledger/.acme/overlay.toml (--config-overlay)
build.cache.local_max_size = "20GiB"                      # default
build.output = "build/r"                                  # overlay
build.workers.count = 4                                   # flag --workers
workspace.sources.dlc = "/var/cache/acme/oe-12.8-sources" # environment OXABL_DLC_SOURCES
workspace.sources.include_paths = ["src","lib"]           # overlay
workspace.sources.directories = ["src"]                   # oxabl.toml
```

The source is `flag <name>`, `environment <name>`, `overlay`, `oxabl.toml` or `default`. A setting that has no value shows `(unset)`. Add `--json` for one JSON document: `settings` maps each name to `{ "value": ..., "source": ... }`. The command shows `[workspace]` `name`, `sources`, `schema` and `preprocessor`, and `[build]`. It does not show `[workspace.lint]` and `[workspace.style]`, which `oxabl build` does not read.

## An overlay for `acme-build`

The workspace is in `/srv/acme/ledger`. The wrapper runs the AVM in a container named `acme-abl`, which mounts the workspace at `/work`. This is the overlay that the wrapper writes for one run:

```toml
[workspace.sources]
include_paths = ["src", "lib", "$DLC/src", "$LIB:acme-util"]
dlc = "/var/cache/acme/oe-12.8-sources"

[workspace.preprocessor]
proversion = "12.8"
databases = ["ledger", "stock"]

[workspace.schema]
files = ["schema/ledger.df", "schema/stock.df"]

[build]
output = "build/r"

[build.libraries]
acme-util = "/opt/acme/libs/acme-util.pl"

[build.outputs]
xref = "build/xref"

[build.compile]
options = ["require-full-names"]

[build.workers]
command = ["docker", "exec", "-i", "-w", "{root}", "acme-abl",
           "_progres", "-b", "-q", "-pf", "/work/.acme/build.pf", "-p", "{program}"]
root = "/work"
count = 4

[build.cache]
local = "/var/cache/acme/oxabl"
```

### The worker command

`[build.workers].command` is a list of arguments. Oxabl runs it with no shell. It must start one AVM in batch mode (`_progres -b`) that runs the worker program `{program}`. The worker talks to oxabl on its standard input and output. This is why `docker exec` needs `-i`.

| Placeholder | Value |
| --- | --- |
| `{program}` | The path of the worker program, as the worker sees it. Oxabl writes the program under its state directory. |
| `{root}` | The workspace root as the worker sees it (`[build.workers].root`; the host root when you do not set it). |
| `{worker}` | The number of the worker, from 1. |
| `{build}` | The id of the build. |

Write a literal `{` as `{{`. The working directory of the AVM must be `{root}`: add `-w {root}` to `docker exec`. Oxabl checks this when the worker starts.

`root` is the path of the workspace inside the container. Oxabl uses it for the paths it sends to the worker. The host paths stay on the host.

`count` is the number of workers. The default is the number of CPUs, at most 8. `--workers <n>` overrides it for one run. The command connects the databases itself. Put connection parameters, passwords included, in a `.pf` file or in a script that the command runs. Never put them in the command: oxabl logs the command with its arguments, and `[build.workers].env` too.

### The PROPATH

Oxabl does not read a PROPATH from the AVM. It builds one for each worker from the settings, and the analysis and the compile search the same entries.

For a compile, the PROPATH is, in this order:

1. The output directory (`[build].output`), so that a unit that uses a class reads the r-code that this build made.
2. `[workspace.sources].root_relative_include_paths`, relative to the directory of the unit.
3. `[workspace.sources].include_paths`, in order.
4. The default entries of the OpenEdge installation (its `.pl` libraries). You do not list them.

Write each entry in `include_paths` and `root_relative_include_paths` in one of three forms:

- A path relative to the root, such as `src`.
- `$DLC` or `$DLC/<dir>`: a directory in the OpenEdge installation. The analysis finds it under `[workspace.sources].dlc`, a host directory that holds a copy of the include sources of that installation. The worker resolves the same entry under its own `$DLC`. Without `dlc`, the units that include a file from it have no compile key and compile at each build. `OXABL_DLC_SOURCES` overrides `dlc`, which is useful because `dlc` belongs to one machine.
- `$LIB:<name>`: a procedure library that you name in `[build.libraries]`. Give its path **as the worker sees it**. The worker reads the library and oxabl puts its checksum in the compile key. A `$LIB:` entry that names a directory of r-code, not a library, has no checksum, so every unit that has it on its PROPATH has no compile key.

Do not use an absolute path in these lists. Oxabl and the worker may see the same directory under different names.

When every unit has the same PROPATH (`include_paths` only), every unit can have a compile key. When you set `root_relative_include_paths`, the PROPATH is different for the units of different directories. A unit that reaches a class then has no compile key. It compiles at each build, and no cache holds its result. If your code base has one global PROPATH, do not use `root_relative_include_paths`.

### The environment of the compile

`[workspace.preprocessor]` states the compile environment to the analysis. `proversion` is required: it is the OpenEdge version of the workers, for example `"12.8"`. Without it the build stops with exit code 2. `databases` lists the logical names of the databases that the worker connects, in the order of connection. The other keys have defaults for a 64-bit Unix batch compile.

The workers report their own environment when they start. If it differs from `[workspace.preprocessor]`, the build stops with exit code 2 and shows the value on each side. Fix the overlay. Do not hide the difference.

`[workspace.schema].files` lists `.df` dump files for the analysis (relative to the root). The compile itself uses the databases that the worker connects. Oxabl images each database on the workers and puts the schema in the compile key, so a schema change recompiles the units that it can change.

### Outputs and options

- `[build].output` is the r-code directory. It must be under the root. Oxabl owns it: the first build needs it absent or empty, and oxabl deletes files in it that it did not make. The same holds for each directory of `[build.outputs]` (`xref`, `debug_list`, `listing`). Do not point two settings at one directory, and do not put a directory inside another.
- `[build.compile]` sets the options of the `COMPILE` statement.
- `[build].state` is the directory for the graph store, the locks and the worker logs (default `.oxabl/build`). Keep it, and the output directories, out of version control.

### The local cache

The local cache keeps compile results so that two copies of a workspace can share them. `[build.cache].local` is its directory. It is used only in `developer` mode. In `ci` and `read-only` mode, `[build.cache].local_job_dir` names a directory for one job. Read [Caches](overview.md#oxabl-build) for the rules. `--no-cache` turns every cache off for one run.

The mode comes from `OXABL_CACHE_MODE` (`ci`, `developer` or `read-only`). When it is not set and `CI` is set, the mode is `read-only`. A wrapper that runs in CI must set `OXABL_CACHE_MODE`.

### The remote cache

The repository commits the settings of the remote cache in `.oxabl/cache.toml`. The wrapper does not write them. This keeps the root of trust (`tenant`, `owner_keys`, `min_roster_version`) in the repository where a reviewer can see it. An overlay, a flag or an environment variable cannot set it. If one of `OXABL_CACHE_TENANT`, `OXABL_CACHE_CACHE`, `OXABL_CACHE_OWNER_KEYS` or `OXABL_CACHE_MIN_ROSTER_VERSION` is set, the build stops with exit code 2.

The wrapper passes the mode and the credentials in the environment of the process: `OXABL_CACHE_MODE`, and `OXABL_CACHE_TOKEN`, `OXABL_CACHE_IDENTITY` or the OIDC variables. Oxabl passes them to the cache client. Oxabl never reads, stores or prints a credential. A wrapper must not write one into the overlay.

The remote cache needs an `oxabl` binary that was built with the `remote-cache` feature. Without it, a workspace that turns the remote cache on stops with exit code 2. `--no-remote-cache` builds without it.

## Run the build

```text
oxabl build --root <root> --config-overlay <file> --events --report <file>
```

- Run one build for each workspace at a time. A second build stops with exit code 3 and the process id of the first. `--wait-lock <seconds>` waits for the lock instead.
- Read **stdout** for the events and keep **stderr** for the human text. Read stdout until it closes.
- Keep the report file. It has the result of each unit.
- Use the exit code of the process to decide what to do (see [Exit codes](build-output.md#exit-codes)). The `summary` event has the same code.
- To cancel, send `SIGINT` or `SIGTERM` to `oxabl`, not to the workers. Oxabl lets busy workers finish their unit and stops them. A second signal stops them at once. Exit code 130 means the cancel worked. The next build compiles what was not recorded. If you kill `oxabl` with `SIGKILL`, the workers see the end of their input and stop after their current unit.
- The first build of a workspace compiles every unit that no cache has. Later builds compile only what changed.

## An example wrapper

This script is a small `acme-build`. It writes an overlay, runs `oxabl build`, reads the events line by line and prints one line for each event that matters. It then checks the report. It uses only `bash`. A real wrapper can parse each line with a JSON library instead. The tests of oxabl run this script.

```bash
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
```

The argument `worker` stands for the program that starts one AVM. A real wrapper puts its `docker exec` command in the overlay instead, as in the overlay above.
