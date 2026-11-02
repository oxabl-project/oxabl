---
title: "oxabl build: configuration, CLI and the handover"
status: proposed
date: 2026-11-02
---

# Configuration, CLI and the handover

Part of [the oxabl build PRD](../build-tool.md).

## Principles

- One schema. `oxabl.toml`, the overlay file and the JSON Schema that the editor uses all
  come from the same Rust types in `oxabl_workspace::config`. `oxabl_build::config` lowers
  them into typed settings.
- **Strict.** Every build table has `deny_unknown_fields`. Slice 2 also puts
  `deny_unknown_fields` on the top-level `WorkspaceConfig` and on `WorkspaceSection`, so a
  misspelled table such as `[biuld]` is an error and not a silent no-op.
- The PROPATH is `[workspace.sources]`. The build adds no second search path, so the
  analysis and the compile search the same entries.
- Secrets are never in a file. Oxabl defines no secret setting. The remote cache client
  reads its credentials from its own environment variables.
- Paths in the file are relative to the directory of `oxabl.toml`, unless a key says it
  takes a host path.
- Key names in `[build]` tables use `snake_case`, like `[workspace.sources]`.
- A duration is an integer and a unit: `"90s"`, `"10m"`, `"1h"`. A size is an integer and a
  unit: `"512MiB"`, `"20GiB"` (`KiB`, `MiB`, `GiB`, `TiB`).

## `oxabl.toml`

### New key in `[workspace.sources]`

```toml
[workspace.sources]
include_paths = ["src", "lib", "$DLC/src", "$DLC/tty"]
dlc = "/opt/oe128-sources"   # host path; machine-specific, usually set by OXABL_DLC_SOURCES
```

`dlc` is the host directory that holds the include sources of the OpenEdge installation
that the workers use. An `include_paths` entry that starts with `$DLC/` resolves under it
for the analysis. The worker resolves the same entry under its own `$DLC`. When an entry
needs `dlc` and it is not set, the includes under it do not resolve and the units that use
them are unkeyable (`unresolved-include`). `oxabl check` and the language server use the
same key, so they resolve these includes too.

### `[build]`

```toml
[build]
output = "build/r"
state = ".oxabl/build"
store = ""
exclude = ["**/test-fixtures/**"]
external_packages = ["OpenEdge.", "Progress."]
```

| Key | Default | Meaning |
| --- | --- | --- |
| `output` | required for `oxabl build` | r-code output directory. Must be under the root (the workers put it on their PROPATH through the root). Oxabl owns it: it must be empty at the first build, and oxabl deletes files it did not produce ([output ownership](../build-tool.md#output-ownership)). The same holds for the `[build.outputs]` directories. |
| `state` | `.oxabl/build` | State directory. Must be under the root. |
| `store` | `""` (in `state`) | Host directory for `state.db` only, for a root on a network filesystem. Absolute or relative. |
| `exclude` | `[]` | Glob patterns (root-relative, `**` allowed) of root files that are not units. |
| `external_packages` | `["OpenEdge.", "Progress."]` | Class name prefixes that may be unresolved without making a unit unkeyable ([compile-key.md](compile-key.md#the-fail-closed-rule)). |

### `[build.outputs]`

```toml
[build.outputs]
xref = "build/xref"
xref_format = "xml"
debug_list = "build/dbg"
listing = ""
```

Each directory is optional (`""` or absent means off) and must be under the root.
`xref_format` is `"xml"` (`XREF-XML`) or `"text"` (`XREF`), default `"xml"`.

### `[build.compile]`

```toml
[build.compile]
min_size = false
generate_md5 = false
stream_io = false
v6frame = false
attr_space = false
languages = []
options = ["require-full-names", "require-field-qualifiers"]
```

These map to the `COMPILE` phrases and to the options of the key
([compile-key.md](compile-key.md#compile-options)). An `options` entry that the AVM
rejects makes each compile fail with the AVM's message.

### `[build.workers]`

```toml
[build.workers]
command = ["docker", "exec", "-i", "-w", "{root}", "abl-dev",
           "_progres", "-b", "-q", "-pf", "/work/build.pf", "-p", "{program}"]
root = "/work"
count = 8
start_timeout = "60s"
unit_timeout = "10m"
cancel_grace = "30s"
env = { TERM = "dumb" }
```

| Key | Default | Meaning |
| --- | --- | --- |
| `command` | none; `oxabl build` stops with exit code 2 without it | The argument list that starts one worker ([worker-protocol.md](worker-protocol.md#the-worker-command)). |
| `root` | the host root | The workspace root as the worker sees it. |
| `count` | CPUs, at most 8 | Number of workers. |
| `start_timeout` | `60s` | Time for one handshake. |
| `unit_timeout` | `10m` | Time for one unit. |
| `cancel_grace` | `30s` | Time for busy workers after a cancel. |
| `env` | `{}` | Extra environment variables for the command. Never a secret; values are logged. |

### `[build.verify]`

```toml
[build.verify]
reads = true
```

`reads` turns on the private XREF and the read check ([cache.md](cache.md#verification)).
With `reads = false`, no cache write happens.

### `[build.cache]` and `[build.cache.remote]`

```toml
[build.cache]
enabled = true
local = ""
local_max_size = "20GiB"

[build.cache.remote]
write = true
timeout = "30s"
# The keys below are defined and checked by the oxabl-cache client.
url = "https://cache.example.com"
cache = "acme-ledger"
```

| Key | Default | Meaning |
| --- | --- | --- |
| `enabled` | `true` | Both caches. |
| `local` | the user cache directory | Host path of the local cache. |
| `local_max_size` | `20GiB` | Eviction limit. |
| `remote` | absent | The remote cache. Absent means off. |
| `remote.write` | `true` | Store verified artifact sets. |
| `remote.timeout` | `30s` | Per call; a timeout is a miss. |

## Environment variables

| Variable | Meaning |
| --- | --- |
| `OXABL_DLC_SOURCES` | Overrides `[workspace.sources].dlc`. |
| `OXABL_CACHE_DIR` | Overrides `[build.cache].local`. |
| `OXABL_BUILD_OVERLAY` | A path, as `--config-overlay`. |
| client-defined | Credentials and key material of the remote cache. Oxabl does not read them. |

Precedence for one key: CLI flag, then environment variable, then overlay, then
`oxabl.toml`, then the default.

## CLI flags

| Flag | Meaning |
| --- | --- |
| `--plan` | Print the plan; compile nothing; start no worker. |
| `--all` | With `--plan`, also list up-to-date units. |
| `--probe` | With `--plan`, start one worker to probe the environment. |
| `--files <path>...` | Treat these files as changed. |
| `--since <rev>` | Treat the files changed since `<rev>` as changed. |
| `--json` | JSON plan, or JSON summary at the end of a build. |
| `--events` | JSON lines on stdout while the build runs; human output goes to stderr. |
| `--report <file>` | Per-unit results as JSON. |
| `--root <dir>` | Workspace root (default: the nearest ancestor with `oxabl.toml`). |
| `--config-overlay <file>` | Overlay file. |
| `--print-config` | Print the effective configuration, with the source of each overridden key, and stop. |
| `--workers <n>` | Overrides `[build.workers].count`. |
| `--no-cache`, `--no-remote-cache` | Turn off caches. |
| `--fail-fast` | Stop dispatch at the first failure. |
| `--wait-lock <seconds>` | Wait for the workspace lock. |

## The overlay

`--config-overlay <file>` (or `OXABL_BUILD_OVERLAY`) names a TOML file with the same schema.
Oxabl merges it over `oxabl.toml`: tables merge key by key, and an array or a scalar in the
overlay replaces the value. The merged document is then parsed strictly. The overlay may
set only `[workspace]` `name`, `[workspace.sources]`, `[workspace.preprocessor]`,
`[workspace.schema]` and the `[build]` tables; any other key is an error. When the root has
no `oxabl.toml`, the overlay is the whole configuration and must set `[workspace] name`.

The overlay exists for machine-specific values (worker command, DLC sources, cache
directory) and for a wrapper that generates them per run. A repository commits
`oxabl.toml`; a wrapper writes the overlay.

## Event stream

With `--events`, each line on stdout is one JSON object. Paths are canonical.

```json
{"event":"build-started","build":"01JC0Z3R6P0B4M6X3V8H1N2K5Q","units":20614,"rebuild":42,"workers":8}
{"event":"unit-fetched","path":"ar/post.p","cache":"local"}
{"event":"unit-started","path":"ar/close.p","worker":3}
{"event":"message","path":"ar/close.p","type":"warning","number":214,"file":"ar/close.p","row":12,"column":5,"text":"..."}
{"event":"unit-finished","path":"ar/close.p","status":"ok","ms":812}
{"event":"worker-lost","worker":3,"reason":"timeout","unit":"ar/slow.p"}
{"event":"summary","compiled":30,"fetched":11,"failed":1,"skipped":0,"exit":1}
```

The object shape has a `format` field in `build-started` (`1`). New fields can appear in a
format; a change to a field is a new format.

## The handover from the downstream build tool

The downstream build tool becomes an environment wrapper. Per build, it:

1. finds the workspace root;
2. finds or starts the container, and the databases, for the workspace;
3. mirrors the OpenEdge include sources out of the container once per OpenEdge version, and
   passes the host directory as `[workspace.sources].dlc`;
4. writes an overlay with:
   - `[workspace.sources].include_paths` and `root_relative_include_paths`: its PROPATH,
     with root-relative and `$DLC/` entries only;
   - `[workspace.preprocessor]`: `proversion` and `databases` (logical names in connection
     order), and `[workspace.schema].files` (the `.df` dumps for the analysis);
   - `[build.workers]`: `command` (its container exec and `.pf`), `root` (the root inside
     the container) and `count`;
   - `[build]`, `[build.outputs]`, `[build.compile]`: its output directories and options;
   - `[build.cache]`: its cache settings, with credentials in the environment;
5. runs `oxabl build --root <root> --config-overlay <file> --events --report <file>`, shows
   the events in its own interface, and keeps the report.

What stays in the wrapper: container and database lifecycle, its own commands and user
interface, the mapping from its work items to workspaces, and any environment-specific
PROPATH rules (it expresses them as `include_paths` and `root_relative_include_paths`).

What leaves the wrapper: the dependency graph and its stores, closure hashing, the
scheduler, the worker pool and the worker program, the retry pass, crash and orphan
handling, and the drift check (oxabl has one source of truth, so it has nothing to drift
from).

Oxabl reads none of the wrapper's state. The first `oxabl build` in a workspace analyses
everything and compiles every unit that has no output record, unless the local or the
remote cache has its key.
