---
title: "oxabl build: the graph store and the incremental refresh"
status: proposed
date: 2026-11-02
---

# The graph store and the incremental refresh

Part of [the oxabl build PRD](../build-tool.md).

## Where the state lives

| Path | Contents |
| --- | --- |
| `<state>/state.db` (and `-wal`, `-shm`) | The SQLite store. |
| `<state>/build.lock` | The workspace lock. |
| `<state>/stage/<build-id>/` | Compile and fetch stages of one build. |
| `<state>/worker/<program-hash>/oxabl_worker.p` | The worker program, written once per hash. |
| `<state>/logs/<build-id>/` | Worker logs of the last five builds. |

`<state>` is `[build].state`, default `.oxabl/build`. It must be under the workspace root,
because the workers read the program and write the stage through the root. The store file
alone can move to another local directory with `[build].store`, for a root on a network
filesystem.

## Format and engine

**SQLite through `rusqlite` with the `bundled` feature.** One file, transactions, snapshot
reads in WAL mode, and decades of crash testing.

Rejected:

- **redb or another embedded key-value store.** Crash-safe too, but it gives no snapshot
  reader across processes, and the reverse queries would need hand-built indexes.
- **bincode or JSON files with temp-and-rename.** A full rewrite on every change, and a
  rename gives no consistency between two files.

### Connection settings

Each connection sets, in this order: `busy_timeout = 5000`, `journal_mode = WAL`,
`synchronous = NORMAL`, `foreign_keys = ON`. The file carries `application_id =
0x4F58424C` (`OXBL`) and `user_version = STORE_FORMAT`.

### Versioning

`STORE_FORMAT` is one integer in `store/ddl.rs`. Any change to the DDL, a blob encoding or
the meaning of a column increments it. There are no migrations. Format 2 adds the
`observed` table: a store of format 1 is replaced by step 3 below.

On open:

1. The file does not exist: create it with the current DDL.
2. `application_id` is not `OXBL` and the file is not empty: stop with exit code 3. Oxabl
   never deletes a file it did not create.
3. `user_version` is not `STORE_FORMAT`, higher or lower: delete `state.db`, `-wal` and
   `-shm`, and create a new store. The next refresh analyses all files again. Outputs on
   disk have no records, so units compile again, unless the local cache has their keys.

A new store loses no correctness. It only costs time, and the local cache removes most of
that cost, because compile keys do not depend on the store format.

### Crash detection

`meta.clean` is `0` while a process has the store open for writing, and `1` after a clean
close. When the writer opens a store with `clean = 0`, it runs `PRAGMA quick_check`. When
the check fails, or when SQLite reports `SQLITE_CORRUPT` or `SQLITE_NOTADB` on open, oxabl
renames the three files to `state.db.corrupt-<unix-seconds>` (it keeps only the newest
such set) and creates a new store. It prints one warning line.

### Network filesystems

On Linux, oxabl checks the filesystem of the store directory with `statfs`. NFS, SMB, CIFS
and FUSE stop the open with exit code 3 and the message `the build store needs a local
filesystem (WAL); set [build].store to a local directory`. WAL needs shared memory that
these filesystems do not give.

## Schema

```sql
CREATE TABLE meta (
    key   TEXT PRIMARY KEY,
    value BLOB NOT NULL
) WITHOUT ROWID;

CREATE TABLE paths (
    id   INTEGER PRIMARY KEY,
    path TEXT NOT NULL UNIQUE
);

CREATE TABLE files (
    path_id     INTEGER PRIMARY KEY REFERENCES paths(id),
    present     INTEGER NOT NULL,
    size        INTEGER,
    mtime_ns    INTEGER,
    ctime_ns    INTEGER,
    ino         INTEGER,
    dev         INTEGER,
    hash        BLOB,
    observed_ns INTEGER NOT NULL,
    changed_gen INTEGER NOT NULL,
    tracked_by  INTEGER NOT NULL
);

CREATE TABLE listings (
    path_id     INTEGER PRIMARY KEY REFERENCES paths(id),
    mtime_ns    INTEGER NOT NULL,
    ctime_ns    INTEGER NOT NULL,
    ino         INTEGER NOT NULL,
    observed_ns INTEGER NOT NULL,
    digest      BLOB NOT NULL,
    changed_gen INTEGER NOT NULL
);

CREATE TABLE analyses (
    path_id      INTEGER PRIMARY KEY REFERENCES paths(id),
    analysed_gen INTEGER NOT NULL,
    content_hash BLOB NOT NULL,
    completeness BLOB NOT NULL,
    edges        BLOB NOT NULL,
    unresolved   BLOB NOT NULL,
    observations BLOB NOT NULL
);

CREATE TABLE results (
    unit_id      INTEGER PRIMARY KEY REFERENCES paths(id),
    status       INTEGER NOT NULL,
    key          BLOB,
    components   BLOB NOT NULL,
    compiled_gen INTEGER NOT NULL,
    origin       INTEGER NOT NULL,
    duration_ms  INTEGER NOT NULL,
    finished_at  INTEGER NOT NULL,
    note         TEXT
);

CREATE TABLE outputs (
    unit_id  INTEGER NOT NULL REFERENCES paths(id),
    path     TEXT NOT NULL,
    size     INTEGER NOT NULL,
    mtime_ns INTEGER NOT NULL,
    ino      INTEGER NOT NULL,
    hash     BLOB NOT NULL,
    PRIMARY KEY (unit_id, path)
) WITHOUT ROWID;

CREATE UNIQUE INDEX outputs_by_path ON outputs(path);

CREATE TABLE observed (
    unit_id INTEGER PRIMARY KEY REFERENCES paths(id),
    closure BLOB NOT NULL
);

CREATE TABLE environments (
    identity   BLOB PRIMARY KEY,
    record     TEXT NOT NULL,
    first_seen INTEGER NOT NULL
) WITHOUT ROWID;
```

### Columns

- `paths.path` is a canonical path ([compile-key.md](compile-key.md#canonical-paths)):
  root-relative, or `$DLC/` plus a path under the host DLC source directory. Ids are never
  reused in one store. A path that leaves the graph keeps its id until the store is
  rebuilt. Only the writer mints ids. A reader that meets a path with no id uses a
  temporary id in memory, so there is no race between two processes that mint ids.
- `files.present`: `0` absent, `1` regular file. `hash` is the BLAKE3 hash of the raw
  bytes, `NULL` when absent. `observed_ns` is the wall clock when oxabl took the stat.
  `tracked_by` is a bit set: `1` a unit found by the walk, `2` a file an analysis read,
  `4` a path an analysis found absent, `8` a file or an absent path of an observed closure.
- `listings` holds the directories that a case-insensitive lookup listed. `digest` is the
  BLAKE3 hash of the sorted entry names, each followed by a NUL byte.
- `analyses` holds one row per analysed node: every unit and every class source that a
  unit reaches. Include files are not analysed alone; the unit that includes them reports
  their edges.
- `results.status`: `1` success, `2` failed. `key` is `NULL` for an unkeyable unit.
  `components` holds the component digests of the key
  ([compile-key.md](compile-key.md#component-digests)), which the planner compares to name a
  reason. `origin`: `1` compiled, `2` local cache, `3` remote cache. `note` holds the
  file that check V3 found outside the closure, for the `unverified` reason.
- `observed` holds the [observed closure](compile-key.md#the-observed-closure) of each unit
  that has one: the files its last observed compile read, the paths that must stay
  absent and the class sources the compile could read, as canonical paths (see [Blob encoding](#blob-encoding)). A row goes when its unit
  leaves the working tree, when its result goes, when a compile of the unit gives no
  observed closure, and when the unit compiles with a key from its own source.
- `environments` holds each environment record that a result refers to, as canonical JSON
  without dropped parameters. The planner compares two records field by field to name an
  `environment-changed` reason. Rows that no result refers to are deleted at commit.
- `outputs` holds each output file of a result: its root-relative final path, its stat
  and the BLAKE3 hash of its bytes. The planner skips colliding units before any compile
  (`output-collision`, [compile-key.md](compile-key.md#the-unit-propath-and-the-compile-name)),
  so two units never reach one output. `outputs_by_path` is the guard behind that rule: an
  insert that breaks it is a bug, and the build stops with exit code 3 and names both units.

### Meta keys

| Key | Value |
| --- | --- |
| `clean` | `0` or `1` |
| `generation` | The current generation, a decimal integer. |
| `analysis_identity` | 32 bytes, see [Analysis identity](#analysis-identity). |
| `binary_hash` | The hash of the executable that last wrote the store, with its stat: six little-endian 8-byte fields (`size`, `mtime_ns`, `ctime_ns`, `ino`, `dev`, `observed_ns`), the 32-byte hash, then the path in UTF-8. |
| `environment` | The canonical environment record of the last probe (JSON, no secrets). |
| `environment_identity` | 32 bytes. |
| `environment_at` | RFC 3339 time of the last probe. |

### Blob encoding

The blobs in `analyses` use one small encoding, in `store/blob.rs`. It is not a general
format.

- A blob starts with one byte, the blob version (`1`).
- `varint` is unsigned LEB128.
- `edges` and `observations`: `varint count`, then the entries sorted by `(kind,
  path_id)`. Each entry is `u8 kind` and `varint delta`, where `delta` is the path id minus
  the previous path id of the same kind (the first of each kind is the id itself). A class
  edge (kinds `3` and `7`) also carries `varint length` and the UTF-8 class name that
  reached the target, because the class-conflict rule compares names
  ([compile-key.md](compile-key.md#class-conflicts)); class edges sort by `(kind, path_id,
  name)`, so one target reached by two spellings gives two entries with delta `0`.
- `unresolved`: `varint count`, then each entry as `u8 kind`, `u8 reason`, `varint length`
  and the UTF-8 name, sorted by `(kind, name)`.
- `completeness`: `varint count`, then one `u8` per degraded reason, sorted. Count 0 means
  complete.

Edge kinds: `1` direct include, `2` transitive include, `3` class, `4` program, `5` shared
producer, `6` conditional include (resolved), `7` conditional class (resolved), `8`
conditional program (resolved). Schema-table edges are not stored; the key covers each
database as a whole.

Observation kinds: `1` read, `2` absent, `3` listed.

- `observed.closure`: `varint count`, then each entry as `u8 kind` (`1` a file of the
  closure, `2` an absent path, `3` a listed class source), `varint length` and the UTF-8
  canonical path, sorted by `(kind, path)`. Paths, not path ids: a build records the closure, and only a refresh mints
  path ids.

Unresolved kinds are the edge kinds. Reasons: `1` absent from workspace, `2` present but
unusable, `3` unknowable, `4` external, `5` computed name.

One blob per row instead of one row per edge: a unit has hundreds of edges and
observations. Rows would be about 10 million for a large tree, and a change would rewrite
indexed rows. The downstream tool needed digest triggers to make that fast. A blob is
written once per analysed file, and the full set loads in milliseconds. The reverse index
is built in memory after load (`graph/reverse.rs`).

## Generations

`meta.generation` counts refreshes that changed something. Each refresh that writes uses
`G = generation + 1` and stores it at the end.

- `files.changed_gen` and `listings.changed_gen` are the generation in which the content,
  the presence or the listing last changed.
- `analyses.analysed_gen` is the generation in which the node was last analysed.
- `results.compiled_gen` is the generation of the snapshot that the result's key was
  computed from.

So "what changed since this unit last compiled" is the set of closure files with
`changed_gen > compiled_gen`. The planner names them in `dependency-changed` with no
closure history in the store.

## Refresh

The refresh brings the store up to date with the working tree. It reads every file at most
once. It analyses a node only when an input of that analysis changed.

### Analysis identity

The analysis identity is BLAKE3 in derive-key mode with the context
`oxabl 2026-11 analysis identity v1`, over the canonical encoding
([compile-key.md](compile-key.md#encoding)) of:

1. the `oxabl_build` crate version (`CARGO_PKG_VERSION`), because any oxabl release can
   change what the analysis finds;
2. the BLAKE3 hash of the running executable (see below), because two builds of one version
   can analyse differently: every build between two releases has the version of the last
   release;
3. `[workspace.sources]`: `directories`, `include_paths`, `root_relative_include_paths`,
   `run_extensions`, `case_insensitive_lookup`, `encoding`, `fallback_encoding`,
   `root_extensions`, and `[build].exclude`, each in its canonical form;
4. `[workspace.preprocessor]`: every field, in declaration order;
5. the content hash of each `[workspace.schema]` file, in order.

The executable is the one that runs the refresh, so a caller other than the `oxabl` binary
(the language server, the daemon) uses its own executable. On Linux oxabl opens
`/proc/self/exe`, which names the running file even when its path was replaced; elsewhere it
opens `std::env::current_exe()`. A build that is the same file shares a store with itself:
a release build and a debug build of one commit do not, because their bytes differ.

The hash is cached in `meta.binary_hash` with the stat of the executable. The hash is reused
when `size`, `mtime_ns`, `ctime_ns`, `ino` and `dev` are equal and the stored stat was not
racy (the rule in [Steps](#steps), step 4). Otherwise oxabl reads the executable and hashes it
again, so a plan with no change does one `open` and `fstat` for this input. A refresh stores
the row again only when it hashed the executable again; that never moves the generation.

If oxabl cannot open, stat or read the executable, the identity takes a random value that no
stored identity equals. Every node is analysed again and the version alone is never trusted.

When the stored identity differs, the refresh deletes every `analyses` row and analyses all
nodes. It keeps `paths`, `files`, `listings`, `results` and `outputs`. Keys that come out
equal keep their results, so a new oxabl release that finds the same edges compiles
nothing.

### Steps

1. **Load.** Read `paths`, `files`, `listings` and `analyses` in one read transaction.
2. **Walk.** Walk `[workspace.sources].directories` with the shared walk primitive and
   `root_extensions`, minus `[build].exclude`. Each file found is a unit.
3. **Stat.** Stat every unit and every tracked path, in parallel with scoped threads. The
   files and absent paths of each stored observed closure are tracked paths too. For a
   present file, the stored hash is reused when `size`, `mtime_ns`, `ctime_ns`, `ino` and
   `dev` are equal to the stored values **and** the stored stat is not racy. Otherwise oxabl
   reads and hashes the file. A different hash, or a change of presence, sets `changed_gen =
   G`.
4. **Racy rule.** A stat is racy when `observed_ns - mtime_ns < 2 s`. A file written in the
   same clock tick as the stat can change again without a new mtime. A racy entry is
   always hashed again at the next refresh. Directories follow the same rule.
5. **Listings.** Stat each listed directory. When `mtime_ns`, `ctime_ns` and `ino` are equal
   and not racy, the listing is current. Otherwise list it again, and when the digest
   differs set `changed_gen = G`.
6. **Stale nodes.** A node needs analysis when it is new, when its `content_hash` differs
   from its file hash, or when an observation of it names a path or a listing with
   `changed_gen > analysed_gen`. A path that is now absent and was read counts as changed.
7. **Analyse.** Analyse the stale nodes through one `SharedRun` on scoped threads with
   `edges_of_observed` (slice 1). The run reads files through a **snapshot file system**:
   for a file hashed in step 3 it serves exactly the bytes it hashed. A file that the run
   reads for the first time is read, hashed and added to the snapshot then. So the content
   hashes and the analysis agree, even when a file changes during the refresh.
8. **Close the graph.** Each class target of a new edge that is a source file and has no
   analysis is analysed too (repeat step 7 for those). Each new read or absent observation
   adds a `files` row from the snapshot.
9. **Settle.** When the snapshot read in step 7 finds that a file changed after step 3
   (its hash differs from the stored hash with an equal stat), mark it changed and go back
   to step 6. After three rounds, stop with exit code 3: `the workspace kept changing
   during the refresh`.
10. **Drop.** A node that is no longer a unit and that no edge reaches loses its `analyses`
    row. A unit that is gone loses its `observed` row. A `files` row whose `tracked_by` is
    now `0` is deleted.
11. **Commit.** Write all changes and `generation = G` in one `IMMEDIATE` transaction. When
    nothing changed, write nothing.

### The observation set

The analysis of one node observes:

- **read**: each file whose bytes it read: the node, its includes at any depth, and each
  class or program source whose facts it took from the index;
- **absent**: each path it checked and did not find: the search path entries before the
  hit for an include, a class or a `RUN` target, and every entry for a name that did not
  resolve;
- **listed**: each directory that a case-insensitive lookup listed.

`oxabl_workspace` records these in an observing wrapper around the `FileSystem` trait. A
memoized index entry keeps the observations made while it was computed, and every node
that uses the entry adds them to its own set. Without that, the second node to use a
shared class would have an incomplete set, because the shared index reads the class only
once.

So a new file that now shadows an include (it appears in an earlier search directory) is a
change to an absent path, and every node that searched there is analysed again. The same
holds for a deleted include, a new class file and a renamed directory.

## Concurrency

- **One writer.** `oxabl build` takes an exclusive `flock` on `<state>/build.lock` before it
  opens the store for writing, and holds it to the end. The file holds the process id for
  the message. The kernel releases the lock when the process dies, so a stale lock is not
  possible. A second `oxabl build` stops at once with exit code 3: `another oxabl build
  (pid 4711) holds .oxabl/build/build.lock`. `--wait-lock <seconds>` waits instead.
- **Readers.** `oxabl build --plan` tries the lock without waiting. With the lock, it runs the
  refresh and commits it, and the next build starts from there. Without the lock, it runs
  the refresh in memory on a read snapshot and commits nothing. Its output is correct for
  the working tree either way.
- **One write connection.** In the writer process, one thread owns the write connection
  and takes write jobs from a channel. Parallel stages read through their own read-only
  connections. No transaction is open while a compile runs.
- **Output directory.** The output directory has its own lock file,
  `<output>/.oxabl-build.lock`, held for the whole build. Two workspaces that point at one
  output directory cannot build at the same time.

## Crash safety

Every write is in a transaction. WAL with `synchronous = NORMAL` can lose the last
transactions after a power loss, but it does not corrupt the file. Each loss is in the safe
direction, because the store never records a fact before it is true on disk:

| Lost write | Effect at the next build |
| --- | --- |
| A refresh | The stats are old, so the files are hashed again and the nodes analysed again. |
| A result | The unit has no result, so it compiles again or is fetched again. |
| A result delete (after a failure) | Not possible: the old outputs are deleted before the result changes, and the planner checks outputs. |

Output durability: the store records a result only after the outputs are at their final
paths. Results are committed in batches (every 500 units or every 2 seconds). Before each
batch commit, oxabl calls `syncfs(2)` on the output filesystem on Linux, or `fsync` on
each new output file elsewhere. A committed result therefore never names an output whose
bytes are not durable.

Nothing in the stage is synced file by file: the worker writes compiled outputs without
`fsync`, and the cache client installs fetched sets with `Durability::Caller`. The stage and
the outputs are on one filesystem in the normal case, so the one `syncfs` before the batch
commit covers both. When they are not, oxabl also calls `syncfs` on the stage filesystem
before it promotes the batch. A cold fetch of 30,000 files therefore costs a few `syncfs`
calls, not 30,000 `fsync` calls. Slice 7 measures promotion on a cold fetch and reports it.

## Memory and size

For about 28,000 files and an average of 300 edges and observations per analysed node:

- `analyses` blobs: about 28,000 x 300 x 2 bytes = about 17 MB.
- `files` rows including absent probes: about 80,000 rows, about 10 MB.
- In memory after load: the forward and reverse adjacency as `u32` arrays (CSR layout),
  about 70 MB.

Slice 2 measures the real numbers on the corpus and reports them in the pull request.
