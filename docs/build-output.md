# What `oxabl build` reports

This page is for people who run `oxabl build` and for tools that read what it prints. It covers the event stream (`--events`), the report file (`--report`) and the exit codes. For the other parts of the command, see [`oxabl build`](overview.md#oxabl-build). To drive `oxabl build` from another tool, see the [wrapper guide](build-wrapper.md).

## Where the output goes

| Flag | stdout | stderr |
| --- | --- | --- |
| none | The summary text. | Warnings and errors. |
| `--json` | The report as one JSON document. | Warnings and errors. |
| `--events` | One JSON object per line, while the build runs. | The summary text, warnings and errors. |
| `--report <file>` | No change. | No change. |

`--report <file>` writes the report to a file and works with the other flags. `--events` and `--json` cannot be used together.

A tool that reads the output must read stdout until the end of the stream. It must keep stdout and stderr apart: stdout holds only JSON when `--events` or `--json` is on.

## The event stream

`--events` writes one JSON object on each line of stdout. Oxabl writes the line and flushes it at once, so a reader sees an event as soon as it happens. Each object has an `event` field. Paths are relative to the workspace root and use `/`.

The order of the keys in an object is not defined. A new field can appear at any time in the same `format`. A change to an existing field gets a new `format` number (shown in `build-started`).

### Order of events

1. `build-started`, once.
2. Events for the units, in the order they happen. Units run at the same time on different workers, so events of different units mix.
3. `summary`, once, last.

For one unit, the order is:

- A unit that a cache gives: `unit-fetched`.
- A unit that compiles: `unit-started`, then its `message` events, then `unit-finished`.

A unit that fails compiles a second time in the retry pass. It then has a second `unit-started`, second `message` events and a second `unit-finished`. A unit that cannot compile where it is (a skipped unit) has no event. It is in the report.

If oxabl stops before the build starts, there is no event. The error goes to stderr and the exit code says why (see [Exit codes](#exit-codes)). This happens for a configuration error, a worker that does not start, or a lock that another build holds. If a signal cancels the build, the stream still ends with a `summary` whose `exit` is `130`.

### `build-started`

The build began. The refresh, the probe and the plan are done.

```json
{"build":"01M6J9YBYMXZSXNTHHPMRGQZG7","event":"build-started","format":1,"rebuild":4,"units":4,"workers":2}
```

| Field | Meaning |
| --- | --- |
| `format` | The version of the event shapes. It is `1`. |
| `build` | The id of this build (26 characters). The same id is in the report and in the log directory of the workers. |
| `units` | The number of compile units in the workspace. |
| `rebuild` | The number of units in the rebuild set. |
| `workers` | The number of workers that started. |

### `unit-started`

A worker began to compile a unit.

```json
{"event":"unit-started","path":"src/ar/post.p","worker":1}
```

| Field | Meaning |
| --- | --- |
| `path` | The unit. |
| `worker` | The number of the worker, from 1. |

### `message`

The compiler gave a message for a unit. Oxabl writes the messages of a unit just before its `unit-finished`.

```json
{"column":1,"event":"message","file":"src/ar/close.p","number":2345,"path":"src/ar/close.p","row":1,"text":"Variable x is not used","type":"warning"}
```

| Field | Meaning |
| --- | --- |
| `path` | The unit that was compiled. |
| `type` | `warning` or `error`. A warning does not fail the unit. |
| `number` | The number of the compiler message. `0` when the compiler gave none. |
| `file` | The file that the message is about. It can be an include file. |
| `row`, `column` | The position in `file`. `0` when the compiler gave none. |
| `text` | The text of the message. |

### `unit-finished`

A compile ended.

```json
{"event":"unit-finished","ms":812,"path":"src/ar/post.p","status":"ok"}
```

| Field | Meaning |
| --- | --- |
| `path` | The unit. |
| `status` | `ok` or `fail`. |
| `ms` | The time of the compile, in milliseconds. |

A unit with `status` `fail` can still pass in the retry pass. Use the `summary` and the report for the final result.

### `unit-fetched`

A cache gave the outputs of a unit. No compile ran. There is no `unit-started` or `unit-finished` for the unit.

```json
{"cache":"local","event":"unit-fetched","path":"src/ar/post.p"}
```

| Field | Meaning |
| --- | --- |
| `path` | The unit. |
| `cache` | `local` or `remote`. |

### `worker-lost`

A worker stopped working: it exited, it broke the protocol, or a unit took longer than `unit_timeout`. Oxabl starts another worker and puts the unit back in the queue. A unit that loses its worker twice fails.

```json
{"event":"worker-lost","reason":"timeout","unit":"src/ar/slow.p","worker":3}
```

| Field | Meaning |
| --- | --- |
| `worker` | The number of the worker. |
| `reason` | `timeout`, `exited`, `protocol` or `spawn`. |
| `unit` | The unit that the worker was compiling, or `null` when it was compiling none. |

### `summary`

The build ended.

```json
{"compiled":3,"event":"summary","exit":1,"failed":1,"fetched":0,"skipped":0}
```

| Field | Meaning |
| --- | --- |
| `compiled` | Units that compiled without an error. |
| `fetched` | Units that a cache gave. |
| `failed` | Units that failed after the retry pass. |
| `skipped` | Units that cannot compile where they are. |
| `exit` | The exit code of the build. It equals the exit code of the process. |

The report has more counts (`not_run`, `tainted`, `up_to_date`).

## The report

`--report <file>` writes one JSON document when the build ends. `--json` prints the same document to stdout. A build that stops with an error before it starts writes no report.

```json
{
  "build": "01M6J9YBYMXZSXNTHHPMRGQZG7",
  "cache": {
    "already_present": 0,
    "changed_inputs": 0,
    "label": "none",
    "not_stored": [],
    "offered": 0,
    "schema_dropped": 0,
    "stored": 0,
    "writes": false
  },
  "environment": "a33724bac811a0901d4c94fa2ec3e74d5952dff47a09362e9223a40ffd24fdac",
  "exit": 1,
  "format": 1,
  "notes": [],
  "store": { "generation": 1 },
  "summary": {
    "compiled": 3,
    "failed": 1,
    "fetched": 0,
    "not_run": 0,
    "skipped": 0,
    "tainted": 0,
    "up_to_date": 0
  },
  "timing": {
    "elapsed_ms": 275,
    "setup": { "dlc_hash_ms": 12, "imaging_ms": 40, "keys_ms": 30, "plan_ms": 5, "prune_ms": 4, "refresh_ms": 120, "workers_ms": 150 },
    "setup_ms": 249
  },
  "units": [
    {
      "messages": [
        { "column": 1, "file": "src/ar/bad.p", "number": 1234, "row": 1, "text": "Syntax error", "type": "error" }
      ],
      "ms": 4,
      "note": null,
      "origin_unverified": null,
      "path": "src/ar/bad.p",
      "reason": "new",
      "skipped": null,
      "source": "compiled",
      "status": "fail",
      "tainted": false
    }
  ]
}
```

| Field | Meaning |
| --- | --- |
| `format` | The version of the report. It is `1`. |
| `exit` | The exit code of the build. |
| `build` | The id of the build. |
| `environment` | The identity of the compile environment (64 hex characters). |
| `store.generation` | The generation of the graph store after the build. |
| `summary` | The counts: `compiled`, `fetched`, `failed`, `skipped`, `not_run` (units that did not run because of `--fail-fast` or a cancel), `tainted` (units that compiled after a unit in their closure failed), `up_to_date` (units that needed no work). |
| `timing` | `setup_ms` is the time before the first compile (refresh, probe, keys). `elapsed_ms` is the total. `setup` splits `setup_ms` into phases, in milliseconds: `refresh_ms` (the refresh of the graph), `workers_ms` (the start and the handshake of the workers) and `imaging_ms` (the schema image of every database), `keys_ms` (the keys, with the `$DLC` hash phase, which `dlc_hash_ms` gives alone), `plan_ms`, and `prune_ms` (the pruning of the output directories). The refresh runs at the same time as the worker start and the imaging, so the phases do not add up to `setup_ms`. |
| `notes` | Warnings of the build, as text. Oxabl prints the same warnings on stderr. |
| `cache` | What the caches did (see below). |
| `units` | One entry for each unit that compiled, was fetched, failed, was skipped or did not run. Units that were up to date are not listed. |

Each entry of `units`:

| Field | Meaning |
| --- | --- |
| `path` | The unit. |
| `status` | `ok`, `fail`, `skipped` or `not-run`. |
| `source` | `compiled`, `local-cache` or `remote-cache`. `null` for a skipped unit and for a unit that did not run. A unit that failed has `compiled`. |
| `reason` | Why the plan put the unit in the rebuild set, for example `new`, `source-changed` or `dependency-changed`. |
| `ms` | The time of the compile, in milliseconds. `0` for a fetched unit. |
| `tainted` | `true` when a unit in its closure failed. Oxabl records a tainted unit but never offers it to a cache. |
| `skipped` | Why a skipped unit cannot compile, or `null`. |
| `note` | Extra information, for example why a unit has no compile key. `null` when there is none. |
| `messages` | The compiler messages. Each one has the fields of the `message` event, without `path`. |
| `origin_unverified` | For a unit from the local cache that says it came from a remote cache: `remote <tier> <signer>`. Nothing proves this text. `null` otherwise. |

A failed unit always has a reason in `note` or in its first message. When the compiler gives no message, oxabl writes one.

The `cache` object:

| Field | Meaning |
| --- | --- |
| `label` | The caches in use, or `none`. |
| `writes` | `true` when a cache stores compile results. |
| `offered` | Results that passed verification and went to the cache. |
| `stored` | Results that the cache stored. |
| `already_present` | Results that the cache already had. |
| `not_stored` | A list of `{ "reason": ..., "count": ... }` for results that the cache did not store. |
| `changed_inputs` | Results not offered because a source file changed during the build. |
| `schema_dropped` | Results not offered because the schema check failed. |

## Exit codes

| Code | Meaning |
| --- | --- |
| 0 | Every unit compiled or came from a cache. A skipped unit is not a failure. |
| 1 | At least one unit failed to compile after the retry pass. |
| 2 | A usage or configuration error. For example: no worker command, no `[build].output`, an unknown key in `oxabl.toml` or in the overlay, an output directory that oxabl does not own, or workers that disagree about the environment. Nothing compiled. |
| 3 | Oxabl could not use something it needs: the store, a lock (another build runs in the workspace), or the workers (none started, or none was left). |
| 130 | A `SIGINT` or `SIGTERM` cancelled the build. The store is consistent. The next build compiles what was not recorded. |

`oxabl build --plan` and `oxabl build --print-config` exit with 0 when they printed their answer, with 2 for a usage or configuration error and with 3 when the store cannot be read.
