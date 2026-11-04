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
- **Strict.** Every build table has `deny_unknown_fields`, and so do the top-level
  `WorkspaceConfig`, `WorkspaceSection`, `[workspace.sources]` and `[workspace.schema]`. A
  misspelled table such as `[biuld]`, or a key such as `include_pahts`, is an error and not
  a silent no-op. The same types serve every client, so a typo in those tables affects
  every command: `oxabl check` and `oxabl build` print the parse error and exit with code 2;
  `oxabl format`, `analyze`, `explain` and `conformance`, the language server and the daemon
  print it as a warning and use the default configuration. Only keys that were already
  wrong are affected; no valid configuration changes meaning.
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
include_paths = ["src", "lib", "$DLC/src", "$LIB:acme-util"]
dlc = "/opt/oe128-sources"   # host path; machine-specific, usually set by OXABL_DLC_SOURCES
```

`dlc` is the host directory that holds the include sources of the OpenEdge installation
that the workers use. An `include_paths` entry that starts with `$DLC/` resolves under it
for the analysis. The worker resolves the same entry under its own `$DLC`. When an entry
needs `dlc` and it is not set, the includes under it do not resolve and the units that use
them are unkeyable (`unresolved-include`). `oxabl check` and the language server use the
same key, so they resolve these includes too.

An entry `$LIB:<name>` names a procedure library from `[build.libraries]`. The analysis does
not search it (a `.pl` holds r-code, not source).

The worker PROPATH also gets the AVM's own default entries under `$DLC` (the `.pl` files of
the installation) after `include_paths`, without listing them
([compile-key.md](compile-key.md#the-unit-propath-and-the-compile-name)). So the minimum
`include_paths` is the workspace's own source directories.

### `[build]`

```toml
[build]
output = "build/r"
state = ".oxabl/build"
store = ""
exclude = ["**/test-fixtures/**"]

[build.libraries]
acme-util = "/opt/libs/acme-util.pl"

[build.external_packages]
"OpenEdge." = ["$DLC"]
"Progress." = ["$DLC"]
"Acme.Util." = ["$LIB:acme-util"]
```

| Key | Default | Meaning |
| --- | --- | --- |
| `output` | required for `oxabl build` | r-code output directory. Must be under the root (the workers put it on their PROPATH through the root). Oxabl owns it: it must be empty at the first build, and oxabl deletes files it did not produce ([output ownership](../build-tool.md#output-ownership)). The same holds for the `[build.outputs]` directories. |
| `state` | `.oxabl/build` | State directory. Must be under the root. |
| `store` | `""` (in `state`) | Host directory for `state.db` only, for a root on a network filesystem. Absolute or relative. |
| `exclude` | `[]` | Glob patterns (root-relative, `**` allowed) of root files that are not units. |
| `[build.libraries]` | `{}` | Procedure libraries outside the root and outside `$DLC`: a logical name and the library's path **as the worker sees it**. `include_paths` names them as `$LIB:<name>`. The worker reports the SHA-256 of each library's bytes, and the key holds it ([compile-key.md](compile-key.md#the-unit-propath-and-the-compile-name)). A `$LIB:` entry that is a directory (a folder of r-code) has no such hash: every unit with it on its PROPATH is unkeyable (K6). |
| `[build.external_packages]` | `{ "OpenEdge." = ["$DLC"], "Progress." = ["$DLC"] }` | Class name prefix to the library entries that supply it. `"$DLC"` means any `.pl` library under `$DLC` on the unit PROPATH with a `sha256:` digest; directory entries do not count. An unresolved class under a prefix keeps the unit keyable only when one of the listed libraries is on the unit PROPATH (so its bytes are in the key); any other unresolved class makes the unit unkeyable ([compile-key.md](compile-key.md#the-fail-closed-rule)). |

### `[build.outputs]`

```toml
[build.outputs]
xref = "build/xref"
xref_format = "xml"
debug_list = "build/dbg"
listing = ""
```

Each directory is optional (`""` or absent means off) and must be under the root.
`xref_format` is `"xml"` (`XREF-XML`) or `"text"` (`XREF`), default `"xml"`. With `"text"`, oxabl
writes no cache entries, because verification needs the compiler-resolved file names that
only `XREF-XML` has and one compile gives only one form ([cache.md](cache.md#verification)).

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
batch_units = 500
batch_interval = "60s"
```

`reads` turns on the private XREF and the read check ([cache.md](cache.md#verification)).
With `reads = false`, no cache write happens. `batch_units` and `batch_interval` decide when
a schema check (V2) releases the held sets: after that many units, or when the oldest held
unit has waited that long, and always at the end of the build.

### `[build.cache]`

```toml
[build.cache]
enabled = true
remote = true
idle_timeout = "30s"
local = ""
local_job_dir = ""
local_max_size = "20GiB"
```

| Key | Default | Meaning |
| --- | --- | --- |
| `enabled` | `true` | Both caches. |
| `remote` | `true` when `.oxabl/cache.toml` exists, else `false` | Use the remote cache. `true` without `.oxabl/cache.toml` is a configuration error. |
| `idle_timeout` | `30s` | A remote or local fetch with no progress for this long ends; the keys not yet reported are misses. |
| `local` | the user cache directory | Host path of the shared local cache, used only in `developer` mode. |
| `local_job_dir` | `""` | In `ci` and `read-only` modes, the per-job local cache directory. It must be absent or empty the first time (then oxabl marks it with `.oxabl-cache-job`); the workspace root, its ancestors, `$HOME` and `/` are refused; oxabl deletes only `v1/` beneath it at the start of each build. Empty means no local cache outside `developer` mode ([cache.md](cache.md#trust)). |
| `local_max_size` | `20GiB` | Eviction limit. |

There is no `[build.cache.remote]` table. The remote cache's own settings (`server`,
`cache`, `owner_keys`, `min_roster_version`) are in `.oxabl/cache.toml`, a file that the
oxabl-cache client owns and that the repository commits
([cache.md](cache.md#configuration-and-trust-root)). No oxabl file, overlay, flag or
environment variable can set `tenant`, `owner_keys` or `min_roster_version`; a trust
variable in the environment stops the build with exit code 2
([cache.md](cache.md#configuration-and-trust-root)).

## Environment variables

| Variable | Meaning |
| --- | --- |
| `OXABL_DLC_SOURCES` | Overrides `[workspace.sources].dlc`. |
| `OXABL_BUILD_CACHE_DIR` | Overrides `[build.cache].local`. Oxabl's own variables never use the client's `OXABL_CACHE_` prefix. |
| `OXABL_BUILD_OVERLAY` | A path, as `--config-overlay`. |
| `CI` | When set and `OXABL_CACHE_MODE` is not, the mode is `read-only` and the shared local cache is off ([cache.md](cache.md#trust)). |
| `OXABL_CACHE_MODE` | Read by the client (`ci`, `developer`, `read-only`); oxabl also reads it, because the shared local cache is on only in `developer` mode. CI systems that do not set `CI` must set it. |
| `OXABL_CACHE_SERVER`, `OXABL_CACHE_IDENTITY`, `OXABL_CACHE_TOKEN`, OIDC variables | Read by the client. Oxabl never reads a credential. |

Precedence for one key: CLI flag, then environment variable, then overlay, then
`oxabl.toml`, then the default.

## CLI flags

| Flag | Meaning |
| --- | --- |
| `--plan` | Print the plan; compile nothing; start no worker. |
| `--all` | With `--plan`, also list up-to-date units. |
| `--probe` | With `--plan`, start the pool to probe the environment. |
| `--files <path>...` | Treat these files as changed. |
| `--since <rev>` | Treat the files changed since `<rev>` as changed. |
| `--json` | JSON plan, or JSON summary at the end of a build. |
| `--events` | JSON lines on stdout while the build runs; human output goes to stderr. |
| `--report <file>` | Per-unit results as JSON. |
| `--root <dir>` | Workspace root (default: the nearest ancestor with `oxabl.toml`). |
| `--config-overlay <file>` | Overlay file. |
| `--print-config` | Print the effective configuration, with the source of each value, and stop ([below](#the-effective-configuration)). |
| `--workers <n>` | Overrides `[build.workers].count`. |
| `--no-cache`, `--no-remote-cache` | Turn off caches. |
| `--fail-fast` | Stop dispatch at the first failure. |
| `--wait-lock <seconds>` | Wait for the workspace lock. |

## The overlay

`--config-overlay <file>` (or `OXABL_BUILD_OVERLAY`) names a TOML file with the same schema.
Oxabl merges it over `oxabl.toml`: tables merge key by key, and an array or a scalar in the
overlay replaces the value. The merged document is then parsed strictly. The overlay may
set only `[workspace]` `name`, `[workspace.sources]`, `[workspace.preprocessor]`,
`[workspace.schema]` and the `[build]` tables; any other table (`[workspace.lint]`,
`[workspace.style]`, a misspelled table) or unknown key is exit code 2, and the error names
the file and the key. When the root has no `oxabl.toml`, the overlay is the whole
configuration, `--root` is required (root discovery looks for `oxabl.toml`), and the overlay
must set `[workspace] name`. Relative paths in the overlay are relative to the root. An
error in the merged document names both files; serde gives no line number for it, because
the merged document has no source text.

Two kinds of key are refused, in the overlay **and** in `oxabl.toml`, with exit code 2:

- **Secret-looking keys.** A key whose name, with `-` and `_` removed and in any case,
  contains `password`, `passwd`, `secret`, `token`, `apikey`, `privatekey`, `accesskey` or
  `credential`, at any depth. It matters most for `[build.workers.env]`, whose keys are
  free. The error never prints the value.
- **Trust keys.** `tenant`, `cache`, `owner_keys` and `min_roster_version` in
  `[build.cache]`, and any `[build.cache.remote]` table. The message says that only
  `.oxabl/cache.toml` sets them ([cache.md](cache.md#configuration-and-trust-root)).

The overlay exists for machine-specific values (worker command, DLC sources, cache
directory) and for a wrapper that generates them per run. A repository commits
`oxabl.toml`; a wrapper writes the overlay. `docs/build-wrapper.md` is the guide for wrapper
authors and `docs/build-output.md` documents every event and the report.

## The effective configuration

`oxabl build --print-config` prints every setting of `[workspace]` `name`, `sources`,
`schema`, `preprocessor` and of `[build]`, sorted by key, with its value and its source:
`flag --workers`, `flag --no-cache`, `flag --no-remote-cache`, `environment
OXABL_DLC_SOURCES`, `environment OXABL_BUILD_CACHE_DIR`, `overlay`, `oxabl.toml` or
`default`. An unset value shows `(unset)`. With `--json` it prints one JSON document with the
same values and sources. `[workspace.lint]` and `[workspace.style]` are not shown, because a
build does not read them. It stops before any lock, worker or store write, and it cannot be
combined with `--plan`, `--probe`, `--all`, `--files`, `--since`, `--events`, `--report`,
`--fail-fast` or `--wait-lock` (exit code 2).

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
     with root-relative, `$DLC/` and `$LIB:` entries only, and `[build.libraries]` for each
     third-party library at its path in the container;
   - `[workspace.preprocessor]`: `proversion` and `databases` (logical names in connection
     order), and `[workspace.schema].files` (the `.df` dumps for the analysis);
   - `[build.workers]`: `command` (its container exec and `.pf`), `root` (the root inside
     the container) and `count`;
   - `[build]`, `[build.outputs]`, `[build.compile]`: its output directories and options;
   - `[build.cache]`: its local cache settings; the remote cache settings stay in the
     repository's `.oxabl/cache.toml`, and the mode and credentials go in the environment;
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
