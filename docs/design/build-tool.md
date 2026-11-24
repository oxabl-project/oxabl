---
title: "oxabl build: the ABL build tool"
status: proposed
date: 2026-11-02
---

# oxabl build: the ABL build tool

This is the product requirements document (PRD) for `oxabl build`. It covers GitHub issues
#252 (graph store, planner, AVM worker protocol) and #253 (compile cache). It makes the
decisions. Implementation agents build it in the slices in [Slices](#slices). A reviewer can
attack each decision, because each one gives its reason and the alternatives it rejects.

The design has seven parts. This file holds the overview, the crate layout, the build flow,
the planner, the scheduler and the slices. The detail is in these files:

| File | Contents |
| --- | --- |
| [build-tool/store.md](build-tool/store.md) | The persisted graph store and the incremental refresh |
| [build-tool/compile-key.md](build-tool/compile-key.md) | The compile key specification and the fail-closed rule |
| [build-tool/worker-protocol.md](build-tool/worker-protocol.md) | The AVM worker protocol, the worker program and the worker pool |
| [build-tool/cache.md](build-tool/cache.md) | The cache seam, the local cache, the remote cache and verification |
| [build-tool/config.md](build-tool/config.md) | `oxabl.toml` sections, environment variables, CLI flags, the handover |
| [build-tool/testing.md](build-tool/testing.md) | Black-box tests, the fake AVM, the OpenEdge end-to-end suite, parity |

## Purpose

Oxabl already extracts the dependency facts of ABL source (`oxabl_index`, `oxabl_pipeline`
edge sets with completeness). A downstream build tool uses these facts today. It also owns
the persisted graph, the planner, the compile scheduling and the AVM compile workers. This
PRD moves those parts into oxabl. The downstream tool then becomes a thin wrapper that finds
the environment (containers, PROPATH, databases) and calls `oxabl build`.

A second goal is a compile cache. A compile key that is derived only from content lets CI
compile once and lets every developer and every CI job fetch the r-code. The remote cache
service is a separate product (oxabl-cache). This PRD defines the oxabl side of that seam.

## Goals

1. `oxabl build --plan` tells which compile units must compile and why, without an AVM.
2. `oxabl build` compiles that set through AVM worker processes that a configured command
   starts. Oxabl ships no Progress code except its own small worker program.
3. Persisted state survives restarts. A store format change rebuilds the store safely.
4. A compile key per unit, exact and canonical, so that two machines with the same inputs
   get the same key, and a change to any input changes the key.
5. **Fail closed.** A unit whose inputs oxabl cannot fully know gets no key. It is never
   fetched from a cache and never written to a cache. Locally it compiles on every build.
6. A cache seam with a local directory cache and the remote cache. A cache error is a miss.
   It is never a failed build and never a wrong hit.
7. The rebuild set is a superset of the downstream tool's rebuild set on a real corpus:
   0 missed, extras listed.

## Non-goals

- Oxabl does not start containers, find databases or manage OpenEdge licences. The worker
  command does that. The downstream wrapper or the user writes it.
- No Windows support in the first release. The protocol does not prevent it. The
  end-to-end suite runs on Linux only.
- No persistent worker pool between builds in the first release (see
  [Follow-ups](#follow-ups)).
- No signature-aware keys in the first release. Keys use the full source of every
  dependency. [compile-key.md](build-tool/compile-key.md#follow-up-signature-aware-keys)
  describes the refinement and the proof it needs.
- No deployment, packaging (`.pl` libraries) or runtime PROPATH management.
- No cache keys for units that reach a class when units have different PROPATHs
  (`root_relative_include_paths`). They compile locally on every build (K9). Real codebases
  checked use one global PROPATH.

## Vocabulary

| Term | Meaning |
| --- | --- |
| **Workspace root** | The directory that holds `oxabl.toml`. All canonical paths are relative to it. |
| **Compile unit** (unit) | A root file (`.p`, `.w`, `.cls`, and the other `root_extensions`) under `[workspace.sources].directories` that `oxabl build` compiles. |
| **Node** | Any file in the graph: a unit, an include, a class source that is not a unit, a file under `$DLC`. |
| **Compile name** | The name that the worker gives to `COMPILE`: the unit path relative to the first PROPATH entry that holds it. A class has its package path. |
| **Closure** | The set of files whose bytes the compile of a unit can read. See [compile-key.md](build-tool/compile-key.md#the-closure). |
| **Environment** | What the AVM session adds to the compile: OpenEdge version, predefined names, code pages, startup parameters, connected databases and their schema. |
| **Compile key** | A 32-byte BLAKE3 value over the unit, its closure, its PROPATH, the environment, the compile options, the requested outputs and the worker program. |
| **Keyable** | A unit that has a compile key. A unit is unkeyable when one of its inputs is unknown. |
| **Artifact set** | The output files of one unit for one key: the `.r` and the optional XREF, debug-list and listing files. |
| **Worker** | One AVM process (`_progres -b`) that runs the oxabl worker program and compiles units on request. |

## Decisions in brief

| # | Decision | Reason | Rejected |
| --- | --- | --- | --- |
| D1 | New crate `oxabl_build` holds the store, refresh, keys, planner, scheduler, worker pool, output promotion and the cache trait with the local cache. | One crate owns one build. Each concern is a module with small files. | A worker-protocol crate: only one Rust speaker exists, and the fake AVM must not share oxabl's codec (see D9). |
| D2 | New crate `oxabl_build_remote` wraps `oxabl-cache-client` behind the umbrella feature `remote-cache`. | Network and crypto dependencies stay out of every other build. | A feature inside `oxabl_build`: a feature leaks into every crate that depends on `oxabl_build` with default features. |
| D3 | The persisted graph lives in its own SQLite store, not in the daemon's salsa world. The daemon does not run builds in the first release. | A build must run in CI with no daemon and must survive restarts. Salsa is in memory. Two copies of the graph drift, and the downstream tool needed a drift self-check for that reason. | Both (salsa plus store): two sources of truth. Salsa only: not persisted. |
| D4 | Refresh is hash-before-parse with an observation set per analysed file: the files it read, the paths it found absent and the directories it listed. | It re-analyses exactly the files whose inputs changed, including a new file that now shadows an include. | Per-edge-kind tracking: it misses inputs that the analysis read but did not turn into an edge. |
| D5 | The compile key uses the full source of every file in the closure and a full schema image per database. | Conservative and simple to prove. | Signature-aware keys now: they need a proof that does not exist yet. Per-table schema CRCs: the CRC does not cover labels, formats and validation that the compiler puts in r-code. |
| D6 | The environment comes from a probe of the real worker at each build, never from configuration alone. The predefined names that oxabl analysed with must equal what the worker reports, or the build stops. | The AVM is the only source of truth for what it compiles with. | Trust `oxabl.toml`: a stale value gives wrong keys with no signal. |
| D7 | The worker protocol is line-based text over the worker's stdin and stdout. | It works through any command (`docker exec -i`, `kubectl exec -i`, a local `_progres`) with no ports and no network setup. End of stdin stops the worker, so a dead oxabl leaves no orphan workers. | TCP: port races, bind addresses, a firewall and an orphan registry. |
| D8 | Workers live for one build. Classes are keyed only when the build has one PROPATH id; with per-unit PROPATHs, every unit that reaches a class is unkeyable (K9) and still compiles as the AVM resolves it. | No daemon is needed. The shared `$OUT` tree, `MULTI-COMPILE` sessions and in-memory class compiles all resolve classes in ways that per-unit PROPATHs make unsound, and real codebases checked use one global PROPATH. | A persistent pool: it needs a daemon (follow-up). Keying classes under per-unit PROPATHs: each design attempt found a new hole (follow-up with its proof list). An output tree per PROPATH id: one class name with two `.r` files is ambiguous at run time anyway. |
| D9 | A fake AVM (`oxabl_fake_avm`, a test-only binary) implements the protocol from the document, with no oxabl dependency. | It stands in for Progress, not for oxabl. A shared codec would hide a codec bug on both sides. | A mock inside the tests: it fakes the program under test. |
| D10 | Each cache write needs verification when its unit completes: the inputs hash again to the same values, a later schema image gives the same identities, and the compiler opened no file outside the closure (from the compiler-resolved cross-reference). Verified sets stream to the cache during the build. | A file edit or a schema change during the build must never put a wrong entry in a cache; streaming bounds memory and overlaps upload with compiling. | Trust the dispatch-time snapshot: an edit between hash and compile gives a wrong entry. One upload after the build: all bytes in memory and no overlap. |
| D13 | The remote cache's configuration and root of trust (`tenant`, `owner_keys`, `min_roster_version`) live only in the client's committed `.oxabl/cache.toml`. `oxabl.toml` has build-side switches only. | Whoever controls the environment of a build must not be able to replace the pinned owner keys. | Remote settings in `oxabl.toml`: the overlay could replace the root of trust. |
| D14 | The local cache trusts every process of the OS user, so the shared one is on only in `developer` mode; `ci` and `read-only` use a marked per-job directory or none. | On a shared CI runner, an untrusted job could plant an entry that a trusted job installs without a signature check. | A local cache with signatures: it needs the remote keys on every machine and duplicates the client. |
| D11 | Store format changes rebuild the store. There are no migrations. | The store is a derived cache. Migrations were the largest source of complexity in the downstream store. | Versioned migrations. |
| D12 | One writer per workspace through a lock file. `--plan` reads a snapshot with no lock. | Simple and enough: one build per workspace at a time. | Multi-writer SQLite: id minting races and interleaved refreshes. |

## Architecture

### Crates

```text
oxabl (umbrella, CLI)
 ├── oxabl_build_remote        [feature remote-cache] → oxabl-cache-client (git rev, then crates.io)
 │    └── oxabl_build
 ├── oxabl_build               [feature cli]
 │    ├── oxabl_pipeline       edges_of, SharedRun, observed analysis
 │    │    └── oxabl_index, oxabl_workspace, ...
 │    ├── oxabl_effects        XREF reader (verification)
 │    ├── rusqlite (bundled), blake3, rustix (flock, process groups, statx)
 └── oxabl_daemon              [feature cli] (unchanged; no build edge)
```

Rules:

- `oxabl_build` never depends on `salsa`, `oxabl_daemon` or `oxabl_lsp`.
- `oxabl_pipeline` and `oxabl_index` never depend on `oxabl_build`.
- `oxabl_wasm` never reaches `oxabl_build`. The umbrella re-exports `oxabl_build` as
  `oxabl::build` only under the `cli` feature, because `rusqlite` and process control do
  not build for `wasm32`.
- Only `oxabl build` reads `[build.workers].command`. `oxabl check`, `oxabl lsp` and the
  daemon never run it. A cloned repository can run its build command only when the user
  runs `oxabl build`, the same as a `Makefile`.
- `oxabl_fake_avm` is a workspace member with `publish = false` and no dependency on any
  oxabl crate.

### `oxabl_build` modules

Each file holds one responsibility. Target: no file over about 400 lines.

| Module | Files | Responsibility |
| --- | --- | --- |
| `config` | `build.rs`, `workers.rs`, `cache.rs`, `outputs.rs`, `compile.rs` | Lower `[build]` sections into typed settings. |
| `paths` | `canonical.rs`, `propath.rs` | Canonical paths (root-relative, `$DLC/`), the unit PROPATH, compile names. |
| `store` | `open.rs`, `ddl.rs`, `paths.rs`, `files.rs`, `analyses.rs`, `results.rs`, `meta.rs`, `blob.rs` | The SQLite store. |
| `refresh` | `walk.rs`, `stat.rs`, `analyse.rs`, `listing.rs`, `generation.rs` | Hash-before-parse refresh. |
| `graph` | `memory.rs`, `reverse.rs`, `scc.rs` | The in-memory graph loaded from the store. |
| `key` | `encode.rs`, `closure.rs`, `environment.rs`, `schema_image.rs`, `keyability.rs`, `compile_key.rs` | The compile key. |
| `plan` | `decide.rs`, `reasons.rs`, `since.rs`, `text.rs`, `json.rs` | The rebuild set and the reason per unit. |
| `schedule` | `ready.rs`, `priority.rs`, `retry.rs` | Ready queue over the condensation, critical-path priority, retry pass. |
| `worker` | `program.rs`, `command.rs`, `frame.rs`, `handshake.rs`, `session.rs`, `pool.rs`, `kill.rs` | Worker program, spawn, protocol, crash and cancel. |
| `output` | `stage.rs`, `promote.rs`, `prune.rs` | Staging, atomic promotion, removal of orphan outputs. |
| `verify` | `policy.rs`, `rehash.rs`, `reads.rs`, `release.rs` | Verification before a cache write, and the policy that the cache mode sets for it. |
| `cache` | `seam.rs`, `local.rs`, `layered.rs` | The `ArtifactCache` trait, the local cache, the local-then-remote chain. |
| `lock.rs`, `events.rs`, `run.rs` | | The workspace lock, the event stream, the build orchestration. |

The worker program is two files, `crates/oxabl_build/resources/oxabl_worker_compile.p` and
`oxabl_worker_driver.p`, embedded with `include_str!` and joined (see
[worker-protocol.md](build-tool/worker-protocol.md#two-parts)). The compile key hashes only
the compile part.

### Why not the daemon

The daemon holds one salsa instance per workspace for editors. A build has other needs:
it runs in CI where no daemon runs, it must keep its state across restarts, and its
answer must not depend on whether an editor is open. So the store is the only source of
truth for a build. Later, the daemon can host `oxabl build --watch` and a warm worker pool.
It then calls `oxabl_build` and reads the same store. It does not keep a second graph.

## The build flow

`oxabl build` does these steps in this order. Steps 2 and 3 run at the same time.

1. **Start.** Resolve `oxabl.toml` and the overlay (see
   [config.md](build-tool/config.md)). Take the workspace lock. Open the store. Fail with
   exit code 2 when `[build.workers].command` is not set: `oxabl build needs a worker
   command; set [build.workers].command (see docs/design/build-tool/config.md) or run
   oxabl build --plan`.
2. **Refresh.** Bring the graph up to date with the working tree
   ([store.md](build-tool/store.md#refresh)).
3. **Start workers and probe.** Start the worker pool. Each worker reports its environment
   and the fingerprint of each database, and all of them must be equal
   ([worker-protocol.md](build-tool/worker-protocol.md#handshake)). Then the workers image
   every database together, each a share of the tables, and oxabl combines the shares into
   the schema identities ([worker-protocol.md](build-tool/worker-protocol.md#schema-image)).
   First a raw pass hashes the metaschema records of each database; a database whose raw
   digest equals the one the last build stored with its image takes that image, and only the
   others are imaged ([compile-key.md](build-tool/compile-key.md#the-raw-digest)).
   `--probe` always images. Check the
   predefined names against `[workspace.preprocessor]`. A difference stops the build with
   exit code 2 and names the value on both sides.
4. **Key and plan.** Compute the closure and keyability of every unit, including the class
   conflicts over the whole graph. Then the workers hash the `$DLC/...` files of all
   closures, each a share (the DLC hash phase, outside `start_timeout`). Then compute the
   compile keys and decide the rebuild set ([Planner](#planner)).
5. **Prune.** Delete outputs that the store owns whose unit no longer exists, and every
   other file in the output directories that the store does not own (see
   [Output ownership](#output-ownership)).
6. **Fetch.** Look up every keyable unit of the rebuild set in the local cache, then in the
   remote cache, in one streaming call. Each hit is installed into the stage, promoted and
   recorded as it arrives; each miss becomes a compile job
   ([cache.md](build-tool/cache.md#build-flow-with-a-cache)).
7. **Compile.** At the same time, schedule the compile jobs over the workers in dependency
   order. A job waits for the fetches it depends on. Promote and record each result as it
   arrives. Verify each eligible unit as it completes, and stream it to the caches.
8. **Retry.** Compile each failed unit once more, in dependency order.
9. **Finish writes.** Release the last verified sets and finish the remote uploader.
10. **Finish.** Stop the workers. Release the lock. Print the summary. Write `--report`.

When the rebuild set is empty after step 4, oxabl stops the workers and does not compile.
The probe in step 3 is still needed: a schema change in the database must rebuild the units
even when no file changed.

### Exit codes

| Code | Meaning |
| --- | --- |
| 0 | Every unit in the rebuild set compiled or was fetched. |
| 1 | At least one unit failed to compile after the retry pass. |
| 2 | Usage or configuration error, including no worker command and an environment mismatch. |
| 3 | Infrastructure error: workers could not start, the store or the lock could not open. |
| 130 | Cancelled (SIGINT or SIGTERM). |

`oxabl build --plan` returns 0 when it printed a plan, 2 on usage or configuration errors,
3 when the store cannot be read. The plan content never changes the exit code; a script
reads `--json`.

## Planner

### Change sources

The planner has three sources of change. All three feed one decision.

| Mode | Changed files | Use |
| --- | --- | --- |
| default | The refresh: files whose content changed since the store last saw them, plus new and deleted files. | Normal builds. |
| `--files <path>...` | The named files, as if each one changed. | What-if, editor integration, parity checks. |
| `--since <rev>` | `git diff --name-only --no-renames <rev>` plus `git ls-files --others --exclude-standard`, as if each one changed. Oxabl runs the `git` binary; no libgit2. | CI: what a branch changes. |

`--files` and `--since` still refresh the store first, so the graph is the graph of the
working tree. They then add the named files to the change set. A named file that is not in
the graph is reported as `unknown-file` and changes nothing.

### Decision per unit

A unit is **up to date** when all of these are true:

1. The store has a result for it with status `success`.
2. The unit is keyable and its current compile key equals the key of that result.
3. Each output file in that result exists and its size, mtime and inode equal the
   recorded values. When the stat differs, oxabl hashes the file; an equal hash refreshes
   the stat and the output is current.
4. Neither the unit nor any file in its closure is in the `--files` or `--since` change set.

Any other unit is in the rebuild set. The planner gives the first reason that applies, plus
details:

| Reason | Detail |
| --- | --- |
| `new` | No result in the store. |
| `failed` | The last compile failed; the detail is the recorded failure reason (see [Failure reasons](#failure-reasons)). |
| `unkeyable` | The keyability reasons, for example `unresolved-include: inc/x.i`. See [compile-key.md](build-tool/compile-key.md#the-fail-closed-rule). Such a unit compiles on every build. |
| `named` | `--files` or `--since` named a file in its closure; the detail names the file and the edge path from the unit to it. |
| `source-changed` | The unit's own bytes changed. |
| `dependency-changed` | The files in its closure that changed since its result (by generation, see [store.md](build-tool/store.md#generations)). |
| `closure-changed` | The closure gained or lost a file, or a name now resolves to a different file. |
| `environment-changed` | The environment fields that differ: for example `schema: ledger`, `proversion`. |
| `options-changed` | The compile options or the requested outputs differ. |
| `propath-changed` | The unit PROPATH differs (an entry, its order, or a library digest). The PROPATH is its own key section. |
| `worker-changed` | The worker program changed (a new oxabl release that changes it). |
| `unverified` | The last compile read a file outside the closure (check V3 in [cache.md](build-tool/cache.md#verification)); the file is named. The unit compiles at each build until the analysis finds that file. A result with no key and no named file, in a plan with keys, is `unverified: the last compile had no key`; without `--probe` it is `unkeyable: no key at the last build`. |
| `output-missing`, `output-changed` | An output was deleted or changed outside oxabl. |

`--plan` does not start a worker. It uses the environment of the last build and says so:
`environment: from build at 2026-11-02T10:31:00Z (not probed)`. With `--probe`, it starts
the pool (`[build.workers].count`, or `--workers`; all workers must agree, so the probe uses
the pool, not one worker), probes, and stops it. With no stored environment and no `--probe`, keys are
`null` and every unit that is not otherwise in the set is reported `environment-unknown`.

`environment.source` in `--json` is `"stored"` (the environment of the last build),
`"probe"` (`--probe`), or `"none"` (no build yet and no probe).

**Without `--probe` there are no keys.** The plan then compares each unit with its last
result through generations: the unit and its closure files with `changed_gen >
compiled_gen` give `source-changed` or `dependency-changed`, and a closure that the refresh
changed gives `closure-changed`. When the refresh did not see the change itself, the
closure component only says that it differs: the reason lists the closure files with a newer
`changed_gen` (a new file counts as changed), or is an empty `closure-changed`.
`environment-changed`, `options-changed`, `worker-changed` and `propath-changed` need the keys,
so they need `--probe`, except where a stored component already differs. A store with no
completed build yet plans "what changed since the last refresh", as slices 2 to 5 do.

**`oxabl build` always compares each unit with its stored result**, whatever the store
holds. The "what changed since the last refresh" rule above applies to `--plan` only. A
build that used it would skip every unit that has never been built once a `--plan` had
refreshed the store, so a wrapper that plans first and then builds would compile nothing.
A unit with no result is always `new` for a build.

### Probe output

`oxabl build --probe` (with or without `--plan`) prints the environment: every environment
field, the canonical startup parameters, the library digests, the environment identity, and
per database `schema_identity`, `entries`, `records` and `fingerprint` (`guid`, `stamp`).
`--json` prints the same as one JSON object. The fingerprint is shown for diagnosis only;
it never decides anything except the agreement between workers.

### Output

Text output, one line per unit in the rebuild set, sorted by path:

```text
rebuild  ar/post.p          dependency-changed: inc/ledger.i
rebuild  ar/Invoice.cls     source-changed
rebuild  gl/close.p         unkeyable: unresolved-include: {&tmpl}.i
3 to rebuild (1 unkeyable), 20,611 up to date, environment from build at 2026-11-02T10:31:00Z
```

`--all` adds up-to-date units. `--json` prints one JSON document:

```json
{
  "format": 1,
  "environment": { "source": "stored", "identity": "<hex>", "at": "2026-11-02T10:31:00Z" },
  "store": { "generation": 118, "committed": true },
  "units": [
    {
      "path": "ar/post.p",
      "compile_name": "ar/post.p",
      "action": "rebuild",
      "reasons": [{ "kind": "dependency-changed", "paths": ["inc/ledger.i"] }],
      "key": "<64 hex or null>",
      "unkeyable": []
    }
  ],
  "summary": { "rebuild": 3, "unkeyable": 1, "up_to_date": 20611, "skipped": 0 }
}
```

`store.committed` is `false` when the plan could not take the lock and refreshed in memory
only. `action` is `rebuild`, `up-to-date` or `skip`. A unit is skipped, with a reason, when it
cannot compile under its name: `not-on-propath` (no PROPATH entry holds it) or
`shadowed-by: <path>` (an earlier PROPATH entry holds another file with the same compile
name), `output-collision: <path>` (another unit compiles to the same `.r`),
`package-mismatch` or `package-root-not-on-propath` (a class unit whose package root is not
where its compile must start;
[compile-key.md](build-tool/compile-key.md#the-unit-propath-and-the-compile-name)). These
reasons come from the paths and the configuration only, so a plan without `--probe` reports
them too once the store has a build: the worker's DLC default entries come after every
workspace entry and cannot hold or shadow a unit under the root. Before the first build such
a plan lists only what changed since the last refresh (see below), whatever the build would
skip. The `format` number changes when the shape changes. The shape is unstable before
1.0, like `analyze --json`.

### Parity with the downstream tool

The downstream tool keys a class's dependents on the class signature and keeps degraded
units incremental with the union of two extractors. Oxabl keys on full source and rebuilds
degraded units every time. Both choices only add units. So the oxabl rebuild set must be a
superset of the downstream set. The parity check
([testing.md](build-tool/testing.md#planner-parity)) runs both on a corpus with the same
edits and must report 0 missed units. It lists the extras by reason.

## Scheduler

The scheduler orders compiles so that a unit compiles after every unit it reads at compile
time. The compiler reads the r-code or the source of each class a unit uses. An include is
never compiled on its own.

1. **Ordering graph.** Nodes are all graph nodes. Edges are the compile-time edges: include
   edges, class edges and their conditional forms. `RUN` edges, shared-producer edges and
   schema-table edges are not in it: the compiler does not read a `RUN` target, and those
   edges make most of the cycles in real code.
   A unit keyed by its [observed closure](build-tool/compile-key.md#the-observed-closure)
   also has an edge to each class unit of the pass, because its compile can read any class
   r-code in `$OUT`. In a job, such units compile last.
2. **Condensation.** Compute the strongly connected components (Tarjan, iterative). A
   component with units of the rebuild set is a **job**. A component with none is
   **pass-through**: it finishes as soon as all the components it points at finish.
3. **Ready queue.** A job is ready when every component it points at has finished. There
   is no level barrier: a worker that becomes idle takes the ready job with the highest
   priority. Two rules make "the highest-priority ready job" exact, because workers and
   the coordinator run on different threads:
   - **No job goes out while a completion is unhandled.** A worker counts each completion
     it reports before it sends it; the coordinator releases the count after it has handled
     the message, and so after it has queued the dependents that the completion made ready.
     While the count is above 0, the queue hands out no job (schema-check tasks are not
     held). So a dependent that a finished job made ready competes with the jobs already
     queued. The coordinator therefore does no slow work: the store batches (`syncfs` and
     commit) are written on a thread of their own, and so are the sets the cache takes.
     With them on the coordinator, every worker waited for each `syncfs` and each copy.
   - **Jobs that become ready together are queued at once**, under one lock, so a woken
     worker never takes a lower-priority job before a higher-priority one of the same step
     is in the queue. The coordinator also queues the first ready jobs before any worker
     starts.

   A job that waits for a cache answer is not ready until the answer arrives (rule 6), so
   with a cache the order across lookups follows the order of the answers.
4. **Priority.** The longest path from the job to a component that nothing depends on,
   weighted by the last compile duration of each unit (`results.duration_ms`, 1 ms when
   unknown). Ties break by path. This starts the long chains first.
5. **Cycles.** All units of one job go to one worker. The job compiles in passes, with a
   rebind of the session before each later pass, so that the last pass reads only this
   build's r-code of the cycle ([compile-key.md](build-tool/compile-key.md#passes-over-a-cycle)).
6. **Fetches.** A unit whose key is being fetched is a pending component. A hit finishes
   it; a miss makes it a job. Jobs that wait on no pending component start at once.
7. **Failure does not block.** A failed job finishes. Its dependents still compile; the
   compiler can often resolve from source. They are **tainted**: they are recorded, but
   they are never written to a cache. The failed unit's old outputs are deleted, so no
   dependent compiles against stale r-code.
8. **Retry pass.** After the queue is empty, each failed unit compiles once more, in the
   same order rules, on the same pool. A unit can fail only because something it uses was
   not compiled yet through a reference that the graph does not model. A unit that
   succeeds on retry counts as one success. The retry pass skips units that failed because
   their worker was lost twice: they already had two attempts. Loss counts are per build,
   across both passes.

### Failure reasons

A failed unit always carries a reason, in `--report`, in the `unit-finished` event and in
the store (`results.note`), so the plan can show it. The reason is the first error message
of the compile when there is one. Otherwise oxabl writes one of its own:

| Situation | Reason |
| --- | --- |
| The worker reported `fail` with no error message | `the compiler reported a failure without a message` |
| The worker reported `ok`, but an expected output is missing in the stage | `no output: <artifact path>` |
| The worker was lost twice while compiling the unit | `worker lost twice while compiling this unit` |
| The compile name or a PROPATH entry cannot be sent in the worker's code page | `path-encoding: <name>` |
| A unit in its closure failed in this build (the unit itself compiled) | not a failure; the unit is tainted |

Worker crash, timeout and cancel rules are in
[worker-protocol.md](build-tool/worker-protocol.md#pool-lifecycle).

### Output promotion

The worker compiles into a stage directory under the state directory:
`<state>/stage/<build-id>/w<n>-<s>/`, one for each worker process. When a unit completes, oxabl moves each expected
output from the stage to its final path with `rename(2)`. The stage and the outputs are
under the workspace root, so a rename is on one filesystem in the normal case. When it is
not, oxabl copies to `<final>.oxabl-tmp-<random>` in the final directory, calls `fsync`,
and renames. Oxabl records the result only after every output is in place. A crash between
the two leaves an output with no record, and the next build compiles the unit again. A
fetched artifact set takes the same path from its fetch stage.

Files that the worker wrote and that are not expected outputs of the unit (for example the
r-code of a super class that `COMPILE ... SAVE` also writes) are discarded. The super
class is its own unit.

At start, oxabl deletes every stage directory of a build that does not hold the lock.

### Output ownership

The output directories belong to oxabl. The compiler reads r-code from the output directory
(see below), so a `.r` file that oxabl did not produce could stand in for a class source
that the key covers, and the key would be wrong. So:

- On the first build, each output directory must be absent or empty. Oxabl then writes the
  marker file `.oxabl-output` into it. A directory with files and no marker stops the build
  with exit code 2: `build/r is not empty and is not managed by oxabl; empty it or choose
  another output directory`.
- In a directory with the marker, the prune step deletes every file that no result owns,
  except oxabl's own files (`.oxabl-output`, `.oxabl-build.lock`).
- R-code that a result of the store holds at a path that no unit expects now (after a
  change of `[build].output` or of `[build].rcode`) is deleted before the first compile, when
  its bytes are still the recorded ones. A directory that this empties is deleted with
  oxabl's own files in it, and a `.gitignore` that oxabl wrote.

### Worker PROPATH and the output directory

The worker PROPATH for a unit is: the output directory, then the unit's root-relative
entries, then `[workspace.sources].include_paths`, then the DLC default entries
([compile-key.md](build-tool/compile-key.md#the-unit-propath-and-the-compile-name)). The output directory comes first, so a
unit that uses a class reads the r-code that this build just made for it. The scheduler
order makes that r-code current. The prune step and the delete-on-failure rule make sure
no stale or orphan r-code is there. Because every unit reads the one `$OUT` tree, a unit
whose PROPATH would find another file for a class than the one `$OUT` holds, or than other
units find, has no key (K9,
[compile-key.md](build-tool/compile-key.md#per-unit-propath-classes)); a K3-accepted external
name that a workspace class also has is K8, and class r-code that oxabl does not hash is K11.

### R-code beside the source

With `[build].rcode = "beside-source"` there is no output directory for the r-code. The
r-code of each unit is at its unit path with the extension `.r` (`a/x.p` gives `a/x.r`,
`a/pkg/Foo.cls` gives `a/pkg/Foo.r`, `rf/x.w` gives `rf/x.r`). For a class this is
`<package root>/<pkg>/<Name>.r`, where the compiler and the runtime look. A runtime whose
PROPATH holds only the source directories runs it, and so does a program that starts by the
full path of its source (measured: [compile-key.md](build-tool/compile-key.md#measured-runtime-and-r-code-beside-the-source)).

- **One build tool.** Oxabl owns the r-code beside every unit of the workspace. R-code
  there before the first build is stale: oxabl replaces it when the unit compiles, and
  deletes it before the first compile or fetch when the unit is skipped (unless a unit that
  compiles has the same file) or is a class unit of the build. So no user binds class
  r-code that is older than its class, also when the analysis misses the edge, and a
  skipped unit never runs old r-code. A unit that fails loses its r-code.
- **The worker is unchanged.** It compiles `SAVE INTO` its stage. Oxabl renames the staged
  file to the path beside the source (one file system: the stage is under the root). A
  runtime that has the old file open keeps it. Oxabl creates no directory beside the
  sources. Before the first compile, it checks that it can write each directory it installs
  into; a directory it cannot write stops the build with exit code 3 and its name. A file it
  cannot delete stops the build in the same way, because the runtime would run it.
- **The unit PROPATH has no `$OUT`.** A user binds a class unit from its r-code beside the
  source, which the scheduler compiles first. This changes the `propath` part of every key
  once; the keys of `directory` mode do not change.
- **A cycle starts from the sources.** At each attempt of a job with more than one unit,
  oxabl deletes the r-code beside each member before the first pass. Stale r-code that does
  not fit the new sources would fail every compile of the cycle (error 12927).
- **The analysis does not see r-code.** The digest of a directory listing leaves out names
  that end in `.r` and the temporary names of an install (`*.oxabl-tmp-*`), in both modes.
  `RUN x.r` is never searched. So r-code that a build writes starts no analysis.
- **Version control.** Oxabl writes no `.gitignore` into the source directories. The code
  base's own `.gitignore` must ignore `*.r`.
- **The manifest.** `<state>/installed` lists each path beside a source that oxabl wrote,
  with the BLAKE3 hash and the size it installed. It is a file of its own, not part of the
  store, so a new store format does not lose it. Before its first delete or rename, a build
  appends a line with the path alone (an intent) for each unit it builds, and calls `fsync`
  once. At its end it writes the whole file again (temporary file, `fsync`, `rename`). So a
  kill at any moment leaves each path that oxabl wrote in the manifest.
- **Orphans, in both modes.** Before the first compile, a build deletes each manifest path
  that is not the r-code path of a unit that compiles now (the source is gone, the unit is
  skipped, or the mode is `directory` now). It deletes the file when its bytes have the
  recorded hash or its line is an intent: nothing but oxabl writes there. Other bytes stay,
  with a warning. R-code that is not in the manifest and not beside a unit (r-code shipped
  without its source) is never touched. A directory that such a delete empties is removed,
  because `git` cannot remove a directory that holds an ignored file. A delete that fails
  stops the build with exit code 3.
- **A change of mode.** A build also deletes the r-code of each result in the store whose
  path no unit expects now, when the file still has the recorded bytes, and then each
  directory that this empties, with the marker, the lock and the `.gitignore` that oxabl
  wrote. So a switch to `beside-source` removes the copy in the old `[build].output`, and a
  switch back removes the r-code beside the sources (through the manifest). Each switch
  compiles or fetches every unit. An overlay can set `build.rcode`, so it can cause a
  switch. When the store is gone (a new store format deletes it), oxabl does not know the
  old output directory: delete it by hand.
- **Intact r-code.** Each recorded output is compared with its size, modification time,
  change time and inode. A row taken less than two seconds after the last change of the
  file (racy) does not vouch for the bytes, and the file is hashed again. A missing or
  changed file compiles again, or comes from a cache. The next build hashes once each
  output that the build before it installed.
- **Temporary files.** An install across file systems writes `x.r.oxabl-tmp-<pid>` beside
  the source. A build deletes such files in the directories of the manifest paths before it
  installs anything.
- **Caches.** A cache entry holds the r-code by the unit path (`r/<unit stem>.r`), and a
  fetch installs it beside the source of the workspace that fetches, with the same rename
  as a compile. So a second copy of a project fetches its r-code into its own source
  directories. The keys of the two modes differ (no `$OUT` in the unit PROPATH), so a
  cache never gives r-code of one mode to the other. The keys of `directory` mode are the
  keys of oxabl 5.6.0. A user that oxabl cannot key compiles after the fetch of each class
  unit of the build is installed or answered as a miss. On one file system, the local cache
  stores a reflink or a hard link of the r-code beside the source. On another file system
  it stores a copy, up to `[build.cache].local_max_size`.
- **One root for each source file.** Two workspace roots whose sources overlap write the
  same r-code with different keys, and each build of one root sees the other's r-code as a
  change. A source file must belong to one root.

## CLI surface

```text
oxabl build [--plan [--all] [--probe]] [--files <path>... | --since <rev>]
            [--json] [--events] [--report <file>]
            [--root <dir>] [--config-overlay <file>]
            [--workers <n>] [--no-cache] [--no-remote-cache] [--fail-fast]
```

- `--json` applies to `--plan` output and to the final build summary.
- `--events` writes one JSON object per line to stdout as the build runs (`unit-started`,
  `unit-finished`, `unit-fetched`, `message`, `worker-lost`, `summary`). The downstream
  wrapper reads it for its own interface. The shape is in
  [config.md](build-tool/config.md#event-stream).
- `--report <file>` writes the final per-unit results (status, source: compiled, local
  cache or remote cache, duration, compiler messages) as JSON.
- `--no-cache` disables both caches. `--no-remote-cache` disables only the remote one.
- A build stops dispatch on the first failure only with `--fail-fast`; the default keeps
  going, because the full error list is more useful for a large tree.

`oxabl build` leaves hidden `--help` with slice 10: the OpenEdge suite passed on a real AVM,
so the worker program's `COMPILE` path, class order and cycles, the private text `XREF` and
check V3 on real file names, cache sharing between two roots and the signal handling are
proven. Unhiding adds `build` to the advertised commands in `docs/overview.md` and needs a
`feat` commit for release-please. The documentation states the known limits at the same
time: per-unit PROPATH layouts can fail to compile valid class users (K9 keeps them out of
the caches; [compile-key.md](build-tool/compile-key.md#follow-up-classes-under-per-unit-propaths)),
`.v` files are units under the default `root_extensions`, and units whose parse recovers
have no key. Publishing the umbrella still waits for the remote cache client
([cache.md](build-tool/cache.md#crate-and-feature)).

## Security

- Oxabl never sends source, paths or keys to a service. The remote cache client receives a
  key and the artifact bytes. The client encrypts both before they leave the machine
  (the oxabl-cache design).
- Secrets (cache tokens, key material) come from environment variables that the client
  defines. They are never in `oxabl.toml`, never in the overlay file, and never in logs.
- Startup parameters that hold connection details (`-U`, `-P`, `-db`, `-H`, `-S`) are
  removed before oxabl hashes, stores or prints the environment.
- The worker command is logged with its arguments. The documentation tells users to put
  passwords in a `.pf` file or a wrapper script, never in the command.

## Metrics

| Metric | Target | How measured |
| --- | --- | --- |
| No-change `--plan` on about 28,000 files | under 1 s warm, without workers | `oxabl build --plan` wall time on the corpus |
| No-change `oxabl build` | under 1 s plus one worker start and probe | wall time; the probe time is reported in `--report` |
| Full refresh, cold store | not slower than the downstream full graph build | same machine, same corpus |
| Store size | under 300 MB for about 28,000 files | file size after a full build |
| Rebuild-set parity | 0 missed | the parity script |
| Wrong cache hits | 0 | black-box adversarial tests, and the r-code oracle in the OpenEdge suite |

**Real-code trial (slice 10, once, outside the suite).** A 7,678-unit subset of a large
real-world ABL codebase, 4 workers, a developer-mode local cache, OpenEdge 12.8.9:

| Measure | Result |
| --- | --- |
| Units compiled | 7,309 (17 genuine failures, 40 tainted users, 26 skipped: 25 `output-collision`, 1 `package-mismatch`) |
| Keyed units that passed V1, V2 and V3 | all 7,055; no read outside the closure |
| Units without a key | 250: 225 `degraded` (parser recovery), 25 `output-collision`, 4 `unresolved-include`, a few unresolved classes |
| Cold build | 854 s (setup 56 s) |
| A second root, same cache | 7,381 fetched, 254 compiled, 170 s |
| No-change build | 7 s |
| Edit of one include | `--plan` named exactly its 4 includers |

## Slices

Each slice is one pull request that one agent can finish. Each one ships on its own and has
a black-box acceptance test. The tests are in [testing.md](build-tool/testing.md).

1. **Observed analysis.** `oxabl_workspace` gains an observing file system that records
   reads, absent paths and directory listings. `oxabl_pipeline` gains
   `LintPipeline::edges_of_observed`, which returns the edge set and the observation set,
   including what memoized index entries observed for other files. *Acceptance:* a pipeline
   integration test over an in-memory tree shows that an include read through a shared
   index entry is in the observation set of each root that used it, and that a file
   created at a recorded absent path is one the observation set names.
2. **Store and refresh, `oxabl build --plan --files`.** `oxabl_build` with `store`,
   `refresh`, `graph` and `plan` (reasons `named`, `source-changed`,
   `dependency-changed`, `closure-changed`, `new`), no keys yet. Strict parsing of `[build]`
   and top-level `oxabl.toml` keys. `[workspace.sources].dlc` and `$DLC/` include path
   entries, for every client. *Acceptance:* CLI tests on a fixture workspace: an
   include edit names its includers; a second run with no change reports no change; a new
   file that shadows an include re-analyses its includers; a store with another format
   number is rebuilt; a truncated store file is moved aside and rebuilt; two plans during
   one locked build both succeed.
3. **`--since`, `--json` and the parity script.** *Acceptance:* CLI tests with a git
   fixture (commit, edit, untracked file, deletion); the JSON shape test; the parity script
   reports 0 missed on the synthetic corpus; the parity run on the private corpus is
   reported in the pull request.
4. **Worker program, protocol and pool; `oxabl build --probe`.** Includes `HASH`,
   `HASHSET`, `FINGERPRINT`, the schema image hashed on the workers and split across them,
   SIGINT and SIGTERM, and the `.pf`-aware parameter parse. `oxabl_fake_avm`,
   `worker` module, the environment and the schema image. `oxabl build` without a command
   fails with exit 2. *Acceptance:* fake-AVM tests: handshake, noise lines, a worker that
   exits during handshake, a slow start past `start_timeout`, a wrong program token,
   differing environments between two workers; the opt-in OpenEdge test probes a real AVM
   and checks that two databases loaded from one `.df` give one schema identity.
5. **Compile keys and the full plan.** `key` module, keyability, the remaining plan
   reasons, keys in `--json`, the golden key vectors. *Acceptance:* the golden vector test;
   CLI tests that each input class in the key spec changes the key (one test per row of the
   input table) and that each unkeyable condition gives `null` and its reason.
6. **Compile.** `schedule`, `output`, `run.rs`: the full local `oxabl build` with retry,
   crash handling, cancellation, `--events` and `--report`. *Acceptance:* fake-AVM tests:
   class before dependent; a cycle in one request; a crash once re-dispatches, twice fails;
   a hang past `unit_timeout`; SIGINT stops all worker processes and leaves a consistent
   store; a second build compiles nothing; an include edit compiles only its includers; a
   failure deletes the old output and taints dependents.
7. **Verification and the local cache.** `verify`, `cache::seam` (streaming fetch and
   uploader), `cache::local`. *Acceptance:* two workspace copies share a local cache, and
   the second compiles nothing with byte-identical outputs; a corrupted cache file is a miss
   and is quarantined; an edit during the compile blocks the cache write; the XREF read
   check blocks the write of a unit whose compile opened a file outside its closure; with
   `CI` set the local cache is not read.
8. **Remote cache.** `oxabl_build_remote`, the `remote-cache` feature, `.oxabl/cache.toml`
   through `Config::from_repo`. *Acceptance:* tests against the real oxabl-cache server
   binary: CI writes, a second workspace fetches everything; a server that returns garbage
   gives misses and a correct build; a server that is down gives a correct build; a slow
   server that keeps sending is not cut off; `owner_keys` in `oxabl.toml` is exit 2 (the
   overlay case lands with slice 9);
   each trust variable in the environment (`TrustOverride`) is exit 2; a non-default output
   layout uploads and installs correctly.
9. **Handover.** `--config-overlay`, the event stream documentation, the downstream wrapper
   guide, `--print-config`. *Acceptance:* overlay tests (overlay wins key by key; an
   unknown key, a secret-looking key and a trust key are refused); a wrapper-shaped test (a
   script that writes an overlay, runs `oxabl build --events --report` and reads the
   events while the build runs). That the downstream tool's own suite passes when it calls
   `oxabl build` is checked in that tool's repository, not here.
10. **OpenEdge end-to-end suite.** The opt-in suite of
    [testing.md](build-tool/testing.md#opt-in-openedge-suite), including the r-code
    portability check. *Acceptance:* the suite passes on a machine with OpenEdge; its
    output is attached to the pull request. Done on OpenEdge 12.8.9: 25 tests in
    `build_openedge` and 3 in `build_openedge_scratch` pass, with no `oxabl build` bug
    found; see the real-code trial under [Metrics](#metrics). `oxabl build` leaves hidden
    `--help` with this slice.

Slices 1 to 3 need no AVM. Slice 4 can start at the same time as slice 2.

## Follow-ups

These are out of scope. Each needs its own design note before code.

- **A fresh worker session per PROPATH id.** So that per-unit PROPATH layouts compile
  class users correctly, not only without a key (`per_unit_propath_class_binding`).
- **A V3 source for classes and definitions-only includes.** the cross-reference lists neither
  ([cache.md](build-tool/cache.md#what-v3-covers)); until another source exists, the plan's
  closure rules are the only guard for those reads.
- **Parser recoveries in real code.** 225 of 7,678 units of the real-code trial have no key
  because the parse recovered; each fix there turns into cache hits.

- **Classes under per-unit PROPATHs.** Key class users when `root_relative_include_paths`
  gives units different PROPATHs. See
  [compile-key.md](build-tool/compile-key.md#follow-up-classes-under-per-unit-propaths) for
  the six things it must prove.
- **Signature-aware keys.** See
  [compile-key.md](build-tool/compile-key.md#follow-up-signature-aware-keys).
- **Per-table schema keys.** Key a unit on the schema image of the tables it reads, not on
  the whole database. Needs the same oracle proof as signature-aware keys.
- **Compiler-observed closure for degraded units.** Use the XREF read set of a successful
  compile as the closure of a degraded unit until its own bytes change. Needs a proof that
  XREF lists every file the compiler read, including absent-path probes.
- **Warm pool and watch mode in the daemon.** `oxabl build --watch` with a pool that lives
  across builds. It needs a class-cache reset between builds (`MULTI-COMPILE` off and on)
  and a proof that the reset clears the session cache.
- **Windows workers.** Needs a check that the worker program reads stdin in batch mode on
  Windows.
