---
title: "oxabl build: the AVM worker protocol"
status: proposed
date: 2026-11-02
---

# The AVM worker protocol

Part of [the oxabl build PRD](../build-tool.md).

Oxabl compiles through AVM processes that a configured command starts. Each process runs
the oxabl worker program, a small ABL procedure that ships inside oxabl. Oxabl ships no
other Progress code. Without a worker command, `oxabl build` stops with exit code 2.

## Transport: lines over stdin and stdout

Oxabl writes requests to the worker's stdin and reads responses from its stdout. One request
is in flight per worker.

Reasons:

- **Any command works.** `docker exec -i`, `kubectl exec -i`, `ssh`, `podman exec -i` and a
  local `_progres` all carry stdin and stdout. No port, bind address or firewall rule is
  needed, and no address inside a container must be known.
- **No orphans.** When oxabl dies, the pipe closes. The worker reads end-of-file, and it
  quits after its current compile. A container exec forwards the end-of-file. No registry
  of started workers is needed.
- **No attribution problem.** Requests and responses use one channel in strict order. A
  response line carries its request id.

Rejected: **TCP** (the worker listens, or connects back). It needs a free port (a race
between allocation and bind), the address of the container, a firewall opening, and a
registry to find and stop orphan workers after a crash. These were the largest parts of
the downstream worker code. Stdin also avoids a second channel for results.

The worker writes each protocol line with `PUT UNFORMATTED <line> SKIP` to the default
output stream. This output path has been checked on a real AVM in batch mode. `MESSAGE` is
not used, because its batch-mode wrapping and buffering of long lines is not checked.

Known risk: the AVM must read stdin line by line in batch mode (`IMPORT UNFORMATTED` on the
default input stream). Slice 4 checks it on a real AVM first. If it fails, the same line
protocol runs over a TCP connection that the worker opens to oxabl, and this section is
updated.

## The worker command

```toml
[build.workers]
command = ["docker", "exec", "-i", "-w", "{root}", "abl-dev",
           "_progres", "-b", "-q", "-pf", "/work/build.pf", "-p", "{program}"]
root = "/work"
```

`command` is an argument list. Oxabl runs it directly, with no shell. Placeholders in any
argument:

| Placeholder | Value |
| --- | --- |
| `{program}` | The worker program, as the worker sees it: `<worker root>/<state>/worker/<program hash>/oxabl_worker.p`. |
| `{root}` | The worker root: `[build.workers].root`, default the host workspace root. |
| `{worker}` | The worker number in this build, from 1. |
| `{build}` | The build id (a ULID). |

A literal `{` is written `{{`. Any other placeholder is a configuration error.

Oxabl starts the command with its working directory at the host workspace root, in a new
process group, with stdin and stdout as pipes and stderr to the worker log
(`<state>/logs/<build-id>/worker-<n>.log`; oxabl keeps the logs of the last five builds).
The environment is oxabl's own environment plus `[build.workers].env`. The working
directory of the AVM must be the worker root. For a local `_progres` it is, because the
host root and the worker root are equal. A container command must set it (`-w {root}`).
The handshake checks it.

The command connects the databases (through its `.pf` or its arguments). Oxabl never
connects a database and never sees a password.

### The worker program

The program is `crates/oxabl_build/resources/oxabl_worker.p`, embedded in the binary.
Before the first start in a build, oxabl writes it to
`<state>/worker/<BLAKE3 hash in hex>/oxabl_worker.p` with temp-and-rename, when that file
is not already present with the right hash. The state directory is under the root, so the
worker sees the file at `{program}`.

The AVM compiles the program from source at start. It is about 400 lines and compiles in
well under a second.

## Framing

- A line is ASCII. It ends with `LF`. A `CR` before the `LF` is removed.
- A protocol line starts with `OXW1` and a `TAB`. The fields follow, separated by `TAB`.
  The first field is the message type.
- A field is **percent-encoded**: the bytes `%`, `TAB`, `LF`, `CR`, and every byte outside
  `0x20` to `0x7E`, are written as `%` and two upper-case hex digits. Everything else is
  literal.
- The worker encodes the bytes of its strings in its `-cpinternal`. Oxabl decodes a field
  with the code page that the worker reported. A path that oxabl sends must be ASCII unless
  the worker's `cpinternal` is `UTF-8`; otherwise the unit fails at dispatch with
  `path-encoding: <name>` (the code page is known only after the handshake, after the plan).
- A line is at most 30,000 bytes, below the 32,000-byte limit of an ABL `CHARACTER`.
- **Noise.** A stdout line that does not start with `OXW1` and `TAB` is noise (for example a
  message that the AVM prints at start). Oxabl writes it to the worker log and never parses
  it. Oxabl's lines to the worker never contain noise.

## Messages

Oxabl to the worker:

| Type | Fields | Meaning |
| --- | --- | --- |
| `HELLO` | `version` `token` | Start of the session. `version` is `1`. `token` is 32 random hex digits. |
| `SCHEMA` | `ldb` `part` `parts` | Image the share `part` (from 0) of `parts` of one database ([Schema image](#schema-image)). |
| `FINGERPRINT` | `ldb` | Report the fingerprint of one database. |
| `HASH` | `id` `path` | Hash one worker-side file ([Hashing worker-side files](#hashing-worker-side-files)). |
| `HASHSET` | `id` `count` | Hash a set of worker-side files and answer with one digest of the list. `count` `PATH` lines follow. |
| `PATH` | `id` `path` | One path of a `HASHSET`, in sorted order. |
| `PROPATH` | `id` `entry`... | Define a PROPATH. Entries are worker-side paths; a `$DLC/...` entry is sent as written and the worker expands it with `OS-GETENV("DLC")`. |
| `OPTIONS` | `id` `name=value`... | Define a set of compile options. |
| `COMPILE` | `req` `propath-id` `options-id` `stage` `xref` `debug-list` `listing` `count` | Start a request. `xref` is `none`, `xref` or `xref-xml` (slice 7 adds `private`, a private `XREF-XML` for verification). `debug-list` and `listing` are `0` or `1`. `count` `UNIT` lines follow. `stage` is relative to the worker root and starts with `./` (see [Compiling](#compiling)). |
| `UNIT` | `req` `compile-name` | One unit of the request, in compile order. |
| `PING` | `n` | Liveness. |
| `QUIT` | | End the session. |

The worker to oxabl:

| Type | Fields | Meaning |
| --- | --- | --- |
| `HELLO` | `version` `token` | The echo of oxabl's `HELLO`. |
| `ENV` | `name` `value` | One environment field. |
| `DB` | `number` `ldb` `dbtype` `aliases` | One connected database, in connection order. `aliases` is comma-separated. |
| `ENV-END` | `count` | End of the environment; `count` is the number of `ENV` and `DB` lines. |
| `SENT` | `ldb` `kind` `name` `records` `sha256` | One schema image entry: its kind (`db`, `system`, `table`, `sequence`), its name (empty for `db` and `system`), its record count and the lower-case hex SHA-256 of its canonical text ([compile-key.md](compile-key.md#schema-identity)). |
| `SCHEMA-END` | `ldb` `entries` `tables` | End of a share: the number of `SENT` lines of this share, and the number of user tables of the whole database as this worker counted them. |
| `FPRINT` | `ldb` `guid` `stamp` | `_Db._Db-guid` and `_DbStatus._DbStatus-CacheStamp`, each `?` when unavailable. |
| `HASHED` | `id` `kind` `size` `sha256` | `kind` is `file`, `dir` or `none`; `size` and `sha256` (lower-case hex) are `-` unless `kind` is `file`. |
| `HASHSET-DONE` | `id` `sha256` | The digest of a `HASHSET` list. |
| `OK` | `id` | A `PROPATH` or `OPTIONS` is defined. |
| `MSG` | `req` `compile-name` `type` `number` `file` `row` `column` `text` | One compiler message. `type` is `error` or `warning`. |
| `DONE` | `req` `compile-name` `status` `ms` | One unit is complete. `status` is `ok` or `fail`. |
| `END` | `req` | The request is complete. |
| `PONG` | `n` | Answer to `PING`. |
| `ERROR` | `text` | A protocol error. The worker quits after it. |
| `BYE` | | Answer to `QUIT`. The worker quits after it. |

Any line that breaks this table (an unknown type, a wrong field count, a `DONE` for a unit
that is not in flight, a `count` that does not match) is a **protocol error**. Oxabl treats
it as a lost worker.

## Handshake

1. Oxabl starts the command and sends `HELLO 1 <token>`.
2. The worker answers `HELLO 1 <token>`, then one `ENV` line per field, then one `DB` line per
   database, then `ENV-END`.
3. Oxabl checks:
   - the version and the token are its own;
   - every field below is present once;
   - `cwd` equals the worker root (after removing a trailing `/`);
   - the predefined names equal what oxabl analysed with (see below).
4. Oxabl sends `FINGERPRINT <ldb>` for each database and reads the fingerprints. Schema
   images are not part of the handshake; the pool builds them after it
   ([Schema image](#schema-image)).
5. Oxabl sends `HASH` for each library entry of every unit PROPATH (including the DLC
   default entries from the `propath` field) and for each startup parameter value
   ([compile-key.md](compile-key.md#startup-parameters)), and reads the answers.

The `$DLC/...` closure files are **not** hashed in the handshake: the closures exist only
after the refresh and the closure step. They are hashed in a separate phase (see
[Hashing worker-side files](#hashing-worker-side-files)).

The whole handshake must end within `[build.workers].start_timeout` (default 60 s).

`ENV` fields:

| Name | Value in the worker |
| --- | --- |
| `program` | The program's own version constant (`1`). |
| `dlc` | `OS-GETENV("DLC")` |
| `dlc_version` | The first line of `$DLC/version` |
| `proversion` | `PROVERSION` |
| `opsys` | `OPSYS` |
| `window_system` | `SESSION:WINDOW-SYSTEM` |
| `batch_mode` | `STRING(SESSION:BATCH-MODE, "yes/no")` |
| `process_architecture` | `STRING(PROCESS-ARCHITECTURE)` |
| `cpinternal`, `cpstream`, `cpcoll`, `cpcase` | `SESSION:CPINTERNAL` and the others |
| `date_format`, `year_offset` | `SESSION:DATE-FORMAT`, `STRING(SESSION:YEAR-OFFSET)` |
| `numeric_decimal_point`, `numeric_separator` | `SESSION:NUMERIC-DECIMAL-POINT`, `SESSION:NUMERIC-SEPARATOR` |
| `startup_parameters` | `SESSION:STARTUP-PARAMETERS` |
| `propath` | `PROPATH` at start, before the worker changes it |
| `cwd` | `FILE-INFO:FULL-PATHNAME` of `"."` |

### Predefined names must agree

Oxabl analysed the source with `[workspace.preprocessor]`. The compile uses the AVM's real
values. When they differ, the analysis may have taken the wrong `&IF` branch, so every edge
set is suspect. The build stops with exit code 2 before it compiles anything, and the
message names each difference:

| `[workspace.preprocessor]` | Worker field | Rule |
| --- | --- | --- |
| `opsys` | `opsys` | equal |
| `window-system` | `window_system` | equal |
| `batch-mode` | `batch_mode` | equal |
| `process-architecture` | `process_architecture` | equal |
| `proversion` | `proversion` | equal; an unset `proversion` is a configuration error for `oxabl build` |
| `databases` | `DB` lines | the same logical names in the same order (ASCII case-insensitive); when `databases` is unset, the names inferred from `[workspace.schema]` must be the same set |
| `database-types` | `DB` `dbtype` | equal for each named database; an unnamed one must be `PROGRESS` |

### All workers agree

Every worker does the full handshake, including the fingerprints of every database. The
handshakes run at the same time. All environment profiles must be equal: the fields, the
databases, the library and parameter digests and the fingerprints. When they are not, the
build stops with exit code 2: `workers see different environments`, and the fields that
differ (for example `fingerprint ledger`). This catches a pool where each worker connects
to its own copy of a database. The schema identity inside the environment identity comes
from the imaging step below, which all workers share; the fingerprint agreement is what
ties every worker to the same databases.

## Compiling

For each `UNIT` of a request, the worker:

1. Sets `PROPATH` to the entries of the request's PROPATH, joined with `,`. `$DLC/...`
   entries are expanded with `OS-GETENV("DLC")`.
2. Computes the output paths under the stage directory of the request:
   - r-code: `COMPILE ... SAVE INTO <stage>/r`, so the `.r` is at `<stage>/r/<compile stem>.r`;
   - XREF: `<stage>/xref/<compile name>.xref` (or `.xref.xml`);
   - private XREF (slice 7): `<stage>/verify/<compile name>.xref.xml` (always `XREF-XML`);
   - debug list: `<stage>/dbg/<compile name>.dbg`;
   - listing: `<stage>/lst/<compile name>.lst`.
3. Creates each directory those paths need, with `OS-CREATE-DIR`. `SAVE INTO` creates package
   directories for classes but not directories for procedures.
4. Compiles the unit through a **generated compile procedure**. The phrases of `COMPILE`
   (`MIN-SIZE`, `GENERATE-MD5`, `STREAM-IO`, `V6FRAME`, `ATTR-SPACE`, `XREF`, `XREF-XML`,
   `DEBUG-LIST`, `LISTING`) are keywords, not run-time values, so one fixed statement cannot
   take them. For each **shape** (the options set, the `xref` value and the debug-list and
   listing flags) the worker writes, once per session, a procedure
   `<stage>/gen/c-<options id>-<xref>-<d><l>.p` that holds one
   `COMPILE VALUE(<name>) SAVE INTO VALUE(<dir>) ... NO-ERROR` statement with exactly those
   phrases (and `LANGUAGES (...)` and `OPTIONS "..."` written as literals), and `RUN`s it by
   its absolute path with the compile name and the output paths as parameters.
   `COMPILER:MULTI-COMPILE` is `TRUE` for the whole session. Because the languages and the
   options are written into source, `[build.compile].languages` and `.options` may hold only
   letters, digits and `-`, `_`, `:`, `,` and `.`; any other character is a configuration
   error (exit code 2).
5. Emits one `MSG` line for each message from `COMPILER:NUM-MESSAGES`, with
   `COMPILER:GET-MESSAGE-TYPE`, `GET-NUMBER`, `GET-FILE-NAME`, `GET-ROW`, `GET-COLUMN` and
   `GET-MESSAGE`.
6. Emits `DONE` with `fail` when `COMPILER:ERROR` is true, or when any step before the
   compile raised an error (then with a `MSG` line of number `0` that holds the error
   text), and `ok` otherwise, plus the elapsed milliseconds from `ETIME`.

**The stage path starts with `./`.** Oxabl sends the stage relative to the worker root and
always with a leading `./` (`./.oxabl/build/stage/<build-id>/w1-1`). OpenEdge 12.8 strips a
leading `.` from a relative `XREF` or `XREF-XML` file name, so `.oxabl/...` would be written
to `oxabl/...` (verified on a real AVM); `./.oxabl/...` is kept as written. `SAVE INTO`,
`LISTING` and `DEBUG-LIST` are not affected, and use the same `./` path for uniformity.

After the last unit, the worker emits `END`.

The stage of a request is `<state>/stage/<build-id>/w<n>-<s>`, where `n` is the worker
number and `s` counts the workers started for that slot. Each worker has a stage of its
own, so a worker that oxabl gave up on can never write into a stage that oxabl still reads.

## Schema image

After the handshake, the pool images every database, **split across all workers**. The
entry texts, their order and the excluded fields are defined in
[compile-key.md](compile-key.md#schema-identity).

- **Shares.** Oxabl sends each worker `p` (from 0) of `N` ready workers
  `SCHEMA <ldb> <p> <N>` for every database. Worker `p` images the user tables whose place
  `i` (from 0) in the list of user tables sorted by `_File-Name` has `i mod N = p`. Share 0
  also sends the `db` entry, the `system` entry and every `sequence` entry. Each worker
  handles the databases one after the other; the workers run in parallel.
- **Answer.** One `SENT` line per entry, then `SCHEMA-END <ldb> <entries> <tables>`. The
  worker builds each entry's canonical text in a `LONGCHAR` and hashes it with
  `MESSAGE-DIGEST("SHA-256", ...)`; it reads the records with dynamic buffers
  (`CREATE BUFFER ... FOR TABLE "<ldb>._File"`), so one program covers every OpenEdge
  version. It reads all records of a table into a temp-table first, so `RECID` and `ROWID`
  fields can be mapped to the row they point at.
- **Combine.** Oxabl joins the shares of a database and fails unless all of these hold:
  - every worker reports the same `tables`;
  - no entry occurs in two shares, and no entry repeats in one share;
  - `db`, `system` and `sequence` entries occur only in share 0, and there is exactly one
    `db` entry and exactly one `system` entry;
  - the number of `table` entries equals `tables`;
  - each `SCHEMA-END` count equals the `SENT` lines of its share;
  - every `SENT` names the database of the request and carries a 64-digit lower-case hex
    hash.

  A broken line (a bad hash, an entry of another database, a wrong count) is a protocol
  error, and the worker is lost. Shares that do not add up stop the build with exit code 3:
  `cannot image the databases`. The identity does not depend on the split.
- **Every build images every database.** The fingerprint is never trusted to skip it
  ([compile-key.md](compile-key.md#database-fingerprints)).
- **Cost, measured on OpenEdge 12.8.9** with 5 databases, the largest with 1,229 entries
  (starting the workers, the handshake and the imaging together):

  | Workers | Probe wall time |
  | --- | --- |
  | 1 | 22.2 s |
  | 4 | 7.2 s |
  | 8 | 6.2 s |

  About 3 s of that is starting the AVM, compiling the worker program and connecting the
  databases; the rest is the largest table's share. More than 8 workers helps little. The
  identity is equal for 1, 4 and 8 workers.

## Hashing worker-side files

Some inputs are files whose compile-time copy only the worker can see: procedure libraries
(the DLC default entries, `$LIB:` entries, a `.pl` under `$DLC`), file-valued startup
parameters, and each `$DLC/...` file in a closure (the host mirror can differ from the copy
the AVM compiles with; [compile-key.md](compile-key.md#hash-function)). For
`HASH <id> <path>`, the worker resolves the path (relative paths against its working
directory, `$DLC/` against `OS-GETENV("DLC")`, `$LIB:<name>` as oxabl expanded it from
`[build.libraries]`), checks it with `FILE-INFO`, and for a file reads its bytes with
`COPY-LOB FROM FILE` into a `MEMPTR` and answers with `MESSAGE-DIGEST("SHA-256", ...)` in
hex. `MESSAGE-DIGEST` with SHA-256 works on OpenEdge 12.8.9 (measured in slice 4); on a
version where it does not, every `HASH` fails and units with a library on their PROPATH are
unkeyable (K6).

Library entries and parameter values are hashed in the handshake by every worker, and the
answers are part of the environment check: all workers must agree.

`$DLC/...` closure files are hashed in the **DLC hash phase**, after the closure step of the
plan ([build-tool.md](../build-tool.md#the-build-flow), step 4) and before the keys are
computed. **Nothing compiles before step 7**: every worker is idle during the phase.

1. The workers share the distinct `$DLC/...` paths of every closure of the whole graph,
   sorted by bytes: worker *i* of *n* takes every *n*-th path from position *i*. Each worker
   hashes its share with `HASH`, at the same time as the others, outside `start_timeout`;
   each answer has `unit_timeout`. These digests go into the keys.
2. Then each worker checks that it sees the same copies, in one round trip: oxabl
   sends `HASHSET <id> <count>` followed by `count` lines `PATH <id> <path>`, with all the
   paths sorted by bytes. The worker hashes each path as for `HASH` and answers
   `HASHSET-DONE <id> <sha256>`, the SHA-256 of the lines `<path> TAB <digest> LF` in that
   order, where `<digest>` is the lower-case hex SHA-256, or `dir` or `none`. Oxabl computes
   the same value from the answers of all shares. Every worker hashed every path once, so
   any difference between two workers shows here. Any difference stops the build with exit
   code 2: `workers see different $DLC files`, with the workers grouped by what they see,
   like the other environment checks. A pool with one worker sends no `HASHSET`. A pool is
   one environment; a command that reaches different containers per slot (for example a
   `kubectl exec` through a service) must reach identical installations.
3. A worker lost during the phase hands its remaining work to another worker; when no
   worker is left, every unit with a `$DLC` file in its closure is unkeyable (K6).

The phase costs one round trip per distinct `$DLC` path divided by the number of workers,
plus one round trip on each worker, in parallel. Each round trip through a slow transport
(for example `docker exec`) adds its latency: with 2,000 paths, 8 workers and 5 ms a round
trip, a no-op build of 10,300 units on the fake AVM takes 1.6 s (1.3 s of it the phase),
against 10.7 s when one worker hashed every path.

## Pool lifecycle

### Size

`[build.workers].count`, or `--workers`. Default: the number of CPUs, at most 8. Each worker
holds one database connection per database, so the count must fit the database's `-n`.
Oxabl never starts more workers than the plan has jobs.

### Start

Oxabl starts all workers at the same time, in parallel with the refresh. A worker that does
not finish the handshake in time, exits, or sends a protocol error is killed and started
again, at most twice per slot. When no worker finishes the handshake, the build stops with
exit code 3, and the message holds the last 20 lines of the last worker's log. When some
workers start, the build goes on with them and prints a warning.

### Dispatch

An idle worker takes the next ready job ([the scheduler](../build-tool.md#scheduler)). Oxabl sends `PROPATH` and `OPTIONS` lines the first time a
request needs an id that the worker does not know yet, then `COMPILE` and the `UNIT` lines.

### Classes and sessions

`COMPILER:MULTI-COMPILE` keeps each class that a session has compiled, by class name, and
does not drop it when `PROPATH` changes. All units also share one `$OUT` tree, where one
class name has one `.r`. With one PROPATH id in the build, every unit resolves a class name
to the same file, so sessions are shared freely and the keys are sound. With more than one
PROPATH id, the plan makes every unit that reaches a class unkeyable (K9,
[compile-key.md](compile-key.md#per-unit-propath-classes)); any worker may compile it, as the
AVM would. The pool has no session rule of its own.

### Timeouts

`[build.workers].unit_timeout` (default 10 minutes) limits the time from the `COMPILE` line,
or from the previous `DONE`, to the next `DONE`. When it passes, the worker is lost.

### A lost worker

A worker is **lost** when its stdout ends, its process exits, a timeout passes, or it breaks
the protocol. Then oxabl:

1. sends `SIGTERM` to its process group, waits up to 5 s, sends `SIGKILL`, and reaps it;
2. never reads its stage again;
3. puts the unit that was in flight back in the queue, together with the units of the same
   request that had not started, as one job with the lost unit first (a request holds more
   than one unit only for a cycle of classes, which must stay together); only the lost unit
   counts a loss;
4. marks a unit that is lost a second time in one build as failed, with the message
   `worker lost twice while compiling this unit`;
5. starts a new worker in the slot, with a new stage (`w<n>-<s+1>`). The new worker must
   report the same environment profile as the pool. When three starts in a row fail, the
   slot is dead and is left out of later passes. When no worker is left, the build records
   what it finished and stops with exit code 3.

A container exec can leave the AVM running after its client is killed. That AVM finishes
its current compile into its own stage, which oxabl no longer reads. It then reads the
end-of-file and quits. So oxabl can dispatch the unit again at once: the two compiles never
write to one place.

### Cancel

Oxabl catches `SIGINT` and `SIGTERM` alike. On the first signal of either kind, oxabl stops
starting workers and stops dispatching. Busy workers finish their
current unit within `[build.workers].cancel_grace` (default 30 s); their results are
promoted and recorded. Then every worker gets `QUIT`, and any that is still busy is killed.
A second signal kills every worker at once. Oxabl uploads the units that already passed
verification, V2 included, then finishes the uploader; it uploads nothing for units that
were still compiling ([cache.md](cache.md#build-flow-with-a-cache)). The exit code is 130.

### Shutdown

At the end of a build, oxabl sends `QUIT` to each worker, waits up to 10 s for `BYE` and the
process exit, then kills the process group.

### Orphans

When oxabl dies without a shutdown (for example `SIGKILL`), the stdin pipes close. Each
worker reads end-of-file at its next `IMPORT`: the `ENDKEY` condition ends its loop and it
quits. A worker in a long compile quits when that compile ends. Its connection to the
database ends with it.

## The worker program

The program has these parts. Each is one internal procedure.

| Part | Responsibility |
| --- | --- |
| main loop | `REPEAT ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE`: `IMPORT UNFORMATTED` one line, dispatch on the type, `QUIT` on end-of-file. |
| `emit` | Percent-encode the fields and write one line with `PUT UNFORMATTED ... SKIP`. |
| `decode` | Split a line on `TAB` and decode each field. |
| `hello` | Check the version, echo the token, emit the environment and the databases. |
| `schemaImage` | Image one share of one database: read each table's records with dynamic buffers into a temp-table, map `RECID` and `ROWID` pointers, build each entry's canonical text in a `LONGCHAR`, and send one `SENT` per entry. |
| `fingerprint` | Answer `FINGERPRINT` from `_Db` and `_DbStatus`. |
| `hashFile` | Answer one `HASH`, or one `HASHSET`. |
| `compileRequest` | Read the `UNIT` lines, compile each unit, emit `MSG` and `DONE`, then `END`. |

The program reads no file except the files that `HASH` names, and writes no file except the
compile outputs in the stage and its generated compile procedures under `<stage>/gen/`. It
connects no database. It runs no other program.
