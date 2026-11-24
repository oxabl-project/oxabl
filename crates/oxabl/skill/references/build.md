# oxabl build

Read this when you must compile ABL, read a build's results, or set up
`[build]` in `oxabl.toml`.

## What it needs

- An `oxabl.toml` at the workspace root (the nearest one above the current
  directory, or `--root DIR`).
- `[build.workers].command`: the command that starts one batch OpenEdge
  session. oxabl fills in `{root}` and `{program}`.
- `[build].output = "DIR"` for r-code, or `[build].rcode = "beside-source"`.
  Not both.
- OpenEdge (an AVM) the command can start: on the host or in a container.

```toml
[workspace]
name = "acme"

[build]
rcode = "beside-source"

[build.workers]
command = ["_progres", "-b", "-q", "-pf", "build.pf", "-p", "{program}"]
unit_timeout = "10m"
```

Never put passwords in `oxabl.toml`; use the project's existing `.pf`.

## No OpenEdge available

`oxabl build` exits 2 with `oxabl build needs a worker command` when
`[build.workers].command` is unset, and 3 when no worker can start. Do not
write a worker command you cannot test. Instead:

```sh
oxabl check src/orders/post.p
oxabl build --plan --files src/orders/post.p
```

Then tell the person the code was checked but not compiled, and which units
the plan says must rebuild. `check` finds syntax and semantic errors without
OpenEdge; it is not the ABL compiler.

## Commands

```sh
oxabl build --print-config
oxabl build --plan --files inc/ledger.i
oxabl build --plan --since origin/main --json
oxabl build --probe
oxabl build
oxabl build --report build-report.json
oxabl build --json
oxabl build --events --fail-fast
```

- `--print-config`: the effective settings, each with where it came from
  (flag, environment, overlay, `oxabl.toml`, default). Run it first when a
  setting looks wrong.
- `--plan`: list the units a change reaches, and why. Compiles nothing, starts
  no worker. A plain `--plan` lists what changed since the last refresh, so a
  second run can be empty: name the change with `--files PATH...` or
  `--since REV` (needs git; committed, staged, unstaged and untracked files).
  `--all` adds the up-to-date units.
- `--probe`: start the workers and print the environment and database schema
  identity. Use it to test a new `[build.workers].command`.
- `build` alone compiles every unit that changed since its last recorded
  result (it refreshes the graph from the working tree). `--files`, `--since`
  and `--all` work only with `--plan`. Failed units are retried once.
- `--fail-fast` stops at the first failure. `--no-cache` skips the compile
  caches. `--workers N` overrides the worker count.
- A second build in the same workspace exits 3 while the first holds the lock;
  `--wait-lock SECONDS` waits instead. Do not use `--yield` (it is for editors).

Plan output (`reason` is the first that applies: `new`, `named`,
`source-changed`, `dependency-changed`, `closure-changed`, `failed`, ...):

```text
rebuild    src/update.p  named: inc/defs.i
1 to rebuild, 1 unchanged, store generation 1
```

## Exit codes

| Code | Meaning |
|---|---|
| 0 | Everything compiled or came from a cache, or a plan, probe or config was printed. |
| 1 | A unit failed to compile after the retry. |
| 2 | Usage or configuration error: no worker command, no output setting, unknown key, an output directory oxabl does not own, workers that disagree. Nothing compiled. |
| 3 | The store, the workspace lock or the workers could not be used. Nothing compiled. |
| 130 | Cancelled by a signal. |

## Reading the results

- Text (default) on stdout: `failed    <unit>` (or `compiled  <unit>` when it
  has messages), then one line per compiler message,
  `  <error|warning> <number> <file>:<row>:<column>: <text>`, then a
  `compiled N, failed N, ...` summary.
- `--json`: the report as one JSON document on stdout (with `--plan`, the
  plan; with `--probe`, the probe).
- `--report FILE`: the same report written to a file; works with any output.
- `--events`: one JSON object per line on stdout while the build runs, human
  text on stderr. Not combinable with `--json`.

Report fields: `exit`, `summary` (`compiled`, `fetched`, `failed`, `skipped`,
`not_run`, ...), and `units[]` with `path`, `status` (`ok`, `fail`,
`skipped`, `not-run`), `reason`, `messages[]` (`type` `warning`|`error`,
`number`, `file`, `row`, `column`, `text`). A message's `file` can be an
include. Row and column `0` mean no position. Up-to-date units are not listed.

Events: `build-started`, `unit-started`, `message`, `unit-finished`
(`status` `ok`|`fail`), `unit-fetched`, `worker-lost`, `summary` (last, with
`exit`).

## Files it writes

The graph and state go to `[build].state` (default `.oxabl/build`), also on
`--plan`. Keep it out of commits. `.oxabl/cache.toml`, if present, is the
project's remote-cache setting and belongs in the repository.

Docs: https://docs.oxabl.org/toolchain/commands/build/
