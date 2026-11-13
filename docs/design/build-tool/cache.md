---
title: "oxabl build: the cache seam, the local cache and the remote cache"
status: proposed
date: 2026-11-02
---

# The cache seam, the local cache and the remote cache

Part of [the oxabl build PRD](../build-tool.md).

## The rule

A cache is an optimisation. **Any cache error is a miss.** It is never a failed build and
never a wrong hit. A wrong hit is not possible when five things hold, and this document
makes each one hold:

1. The key is complete, or there is no key ([compile-key.md](compile-key.md)).
2. Only verified artifact sets are written ([Verification](#verification)).
3. A read checks every byte before it installs ([Local cache](#local-cache); the remote
   client does the same in its own design).
4. Oxabl installs a fetched set only when its file list is exactly the expected outputs of
   the unit.
5. A cache that another party can write to is never read without that party's trust check
   ([Local cache trust](#trust)).

## The seam

The seam is fixed. Oxabl computes a 32-byte plaintext `CompileKey` for each keyable unit.
It hands a cache the key and the artifact files of the unit. The cache does everything
else: for the remote cache that is lookup ids, compression, encryption, signing, transfer,
verification and atomic install. The cache never defines what goes into a key.

### Artifact paths

An artifact path is relative to the **artifact root**: a logical tree with one top
directory per output kind.

| Kind | Artifact path | Final path |
| --- | --- | --- |
| `rcode` | `r/<compile stem>.r` | `[build].output/<compile stem>.r` |
| `xref` / `xref-xml` | `xref/<compile name>.xref` or `.xref.xml` | `[build.outputs].xref/...` |
| `debug-list` | `dbg/<compile name>.dbg` | `[build.outputs].debug_list/...` |
| `listing` | `lst/<compile name>.lst` | `[build.outputs].listing/...` |

The output directories are configuration and are not in the key. So the artifact path is
relative to the artifact root, not to the workspace root, and two workspaces with different
output directories share entries. Oxabl creates only canonical relative paths
([compile-key.md](compile-key.md#canonical-paths)) whose first component is one of the four
kinds. The client treats them as opaque relative paths and validates them on its own: no
`..`, no absolute path, no duplicate.

### The trait

`oxabl_build::cache::seam`:

```rust
pub struct CompileKey(pub [u8; 32]);

pub struct ArtifactPath(String);

pub struct ArtifactFileRef {
    pub path: ArtifactPath,
    pub disk_path: PathBuf,
    pub size: u64,
    pub blake3: [u8; 32],
}

pub struct ArtifactSetRef {
    pub key: CompileKey,
    pub files: Vec<ArtifactFileRef>,
}

pub struct FetchRequest {
    pub key: CompileKey,
    pub expect: Vec<ArtifactPath>,
}

pub enum FetchOutcome {
    Installed { key: CompileKey, files: Vec<ArtifactPath>, origin: Origin },
    Miss { key: CompileKey, reason: MissReason },
}

pub trait ArtifactCache: Send + Sync {
    fn label(&self) -> &str;
    fn fetch(
        &self,
        requests: Vec<FetchRequest>,
        stage: &Path,
        on_outcome: &mut dyn FnMut(FetchOutcome),
    );
    fn uploader(&self) -> Box<dyn ArtifactUploader>;
    fn cancel_fetch(&self);
    fn notes(&self) -> Vec<String>;
}

pub trait ArtifactUploader: Send {
    fn push(&mut self, set: ArtifactSetRef);
    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)>;
}

pub enum StoreOutcome {
    Stored,
    AlreadyPresent,
    NotStored { reason: NotStoredReason },
}

pub enum NotStoredReason {
    ChangedOnDisk,
    Unreadable,
    Rejected,
    TooLarge,
    QuotaExceeded,
    Unavailable,
}
```

`StoreOutcome` belongs to `oxabl_build::cache`, because the local cache returns it too and
only `oxabl_build_remote` depends on the client. The remote adapter maps the client's
outcomes:

| Client outcome | `StoreOutcome` |
| --- | --- |
| `Committed` | `Stored` |
| `Existed` | `AlreadyPresent` |
| `Changed` (a file's size or hash differed when read) | `NotStored { ChangedOnDisk }` |
| `Failed(Unreadable)` | `NotStored { Unreadable }` |
| `Failed(Rejected)` | `NotStored { Rejected }` |
| `Failed(TooLarge)` | `NotStored { TooLarge }` |
| `Failed(QuotaExceeded)` | `NotStored { QuotaExceeded }` |
| `Failed(Unavailable)` | `NotStored { Unavailable }` |

The mapping is total over the client's `StoreFailure` (`Rejected`, `TooLarge`,
`Unreadable`, `QuotaExceeded`, `Unavailable`); a new client variant is a compile error in
the adapter, not a silent default.

The local cache returns `Stored`, `AlreadyPresent` (another process won the rename), or
`NotStored` with `ChangedOnDisk` or `Unreadable`. The summary counts each variant; no
variant changes the exit code.

**The uploader is chosen by mode, not by error.** The adapter reads `client.mode()` before
it calls the client's `uploader()`:

- In a mode that does not write (`read-only`), or when `[build.cache] remote` is off, it
  returns a no-op uploader whose `push` does nothing and whose `finish` returns an empty
  list. The summary says `remote: read-only, 0 sets offered`.
- In a writing mode (`ci`, `developer`), it calls `uploader()`. An `Err` there is never
  read as "read-only": oxabl prints one warning with the error, and the uploader it returns
  reports every pushed set as `NotStored { Unavailable }`. So a CI job whose uploads cannot
  start shows the failure in its summary on every key, and never looks like a read-only
  run. The exit code does not change.

- **`fetch` streams.** It takes all requests in one call (the cache batches on the wire)
  and calls `on_outcome` once per key, as each one completes. Every key of one call is
  installed into one tree, `stage/<artifact path>`: artifact paths are unique per build
  after the output-collision rule
  ([compile-key.md](compile-key.md#the-unit-propath-and-the-compile-name)), so no two keys
  share a path. An installed set is complete and verified at its artifact paths before its
  outcome is reported, and it stays installed whatever happens to later keys. Oxabl
  promotes from there. `expect` is the list of
  artifact paths oxabl requires; a set with another list is a `Miss` with reason
  `PathMismatch`. A key the call never reports (a crash of the adapter, a timeout) is a
  miss. The remote adapter prints one note with their number ("the remote cache gave no
  answer for N of M keys"): the client answers every key it takes, so for the remote
  cache a key without an answer is a fault, never a plain miss. `Origin` names the cache, and for a remote hit its tier and signer.
- **`fetch` has an idle timeout, not a call timeout.** The call ends with a miss for each
  key not yet reported when no outcome and no byte progress has arrived for
  `[build.cache].idle_timeout` (default 30 s). A slow link that keeps moving is never cut
  off. The client's outcome buffer is bounded, and the client counts the time it waits on
  a full buffer as no progress. Promoting a set is slower than downloading it, so the
  remote adapter drains the client's stream into its own queue at once: a stall of the
  build (a long `syncfs`) never ends a fetch, and never loses an outcome.
- **Durability.** The adapter installs into the stage without `fsync`
  (`Durability::Caller` in the client). Oxabl calls `syncfs(2)` on the stage filesystem
  once before it promotes a batch of fetched sets
  ([store.md](store.md#crash-safety)).
- **`uploader` streams.** `uploader()` takes no root. Oxabl `push`es one set at a time
  during the build, as soon as the unit passes verification. Each `ArtifactFileRef` names
  two places: `path`, the artifact path (relative to the logical artifact root, validated
  by the cache), and `disk_path`, the absolute path of the promoted file in its configured
  output directory. The cache opens `disk_path` with `O_NOFOLLOW`, which refuses a symbolic link in the
  **final component only**: a directory above it may be a symbolic link (oxabl resolves
  paths lexically, and a workspace often sits under a linked directory), and the verified
  size and BLAKE3 hash bind the content anyway. The cache reads the file lazily, and
  applies its own back-pressure, so memory stays bounded. Output directories need not share
  a parent. `push` may block for
  back-pressure. The cache must check, when it reads a file, that its size and BLAKE3 hash
  equal `size` and `blake3`, and must drop the set otherwise: a file that changed after
  verification is never stored. `finish` flushes and returns one outcome per key.
- **`cancel_fetch` ends a fetch at once.** `fetch` blocks its thread, and a remote fetch can
  run for minutes. On a cancel, a watcher thread calls `cancel_fetch` within 20 ms; the
  cache ends its stream, reports every key not yet reported as a miss and returns. Sets
  already installed stay installed. `Layered` forwards it to both caches. Without it,
  Ctrl-C during a long download would wait for the whole download.
- **`notes` carries the warnings of a cache** (a remote cache that is not used and why, a
  count of misses that were not plain absence, an uploader that could not start). The build
  prints each once and puts them in the report. `MissReason` has, besides plain absence,
  `PathMismatch`, `Untrusted` and `InstallFailed`.
- **Errors are misses.** The trait methods return no `Result`: a failure is a `Miss` or a
  failed `StoreOutcome`. `run.rs` also wraps each call in `catch_panic`; a panic ends the
  call, and every key not yet reported is a miss.
- The trait is blocking. The remote adapter runs its client on a runtime that it owns.
  `oxabl_build` has no async runtime.

`cache::layered` chains the local cache and the remote cache: a key goes to the remote cache
only after the local cache reports it as a miss.

## Build flow with a cache

After the plan ([build-tool.md](../build-tool.md#the-build-flow), step 6):

1. `K` = the keyable units of the rebuild set. Each one is a **pending** node in the
   scheduler: its dependents wait for it, as they wait for a compile.
2. **Fetch.** One layered `fetch` for all of `K`, into `<state>/stage/<build-id>/fetch/`.
   The scheduler starts compiling units that do not wait on a pending node at the same
   time, so fetching and compiling overlap.
3. **Outcome.** The thread that runs `fetch` only reads outcomes. A `Miss` turns the node
   into a compile job at once. An `Installed` outcome goes to the installer thread, which
   collects a promote batch (500 sets, or what arrived within 2 s of the first). For each
   batch it calls `syncfs` on the stage once, renames each file to its final path (on
   several threads), and answers each unit; a batch whose `syncfs` fails is a miss for each
   unit. The build records the results with `origin` `2` (local) or `3` (remote). Oxabl
   hashes each promoted file itself for the `outputs` table; the client reports no per-file
   hash, and oxabl does not need one. The node is finished. Without a cancel, the installer
   answers every set it takes, and the fetch ends only after it has stopped. After a
   cancel, it starts no new batch: a file it renamed but did not answer has no record, so
   the next build compiles its unit again.
4. **Local fill.** After promotion, each remote hit is also stored in the local cache (when
   the local cache is on), by reflink or hard link **from the final path**, and by copy
   otherwise. A filler thread does this in batches of the same size (`fill` takes a batch),
   with one `syncfs` on the cache filesystem for each batch, so the installer never waits
   for it. The promoted file is the only copy left once the stage file is renamed away,
   and the verified local read covers any later edit of it. The local entry records the
   remote tier and signer.

   A whole-tree fetch of 10,000 sets of a few KB to tens of KB from a local server on the
   same machine (`build_remote_fetch_tree` with `OXABL_FETCH_TREE_UNITS=10000`, debug
   build) takes about 14 s, of which the download is about 7 s. With one set at a time
   and one `syncfs` per local fill it took about 100-110 s, about 90 s of them in the fill.
5. **Compile** the rest.
6. **Verify and write** each compiled unit as it completes
   ([Verification](#verification)): a unit that passes goes to the local cache and is
   `push`ed to the remote uploader at once.

   **Until V1 to V3 exist (slice 6), the hook pushes at unit completion:** a unit is pushed
   when it completes `ok`, has a key, is not tainted and no cancel has arrived. Slice 7 puts
   verification between completion and `push`. `finish` runs once, also after a cancel.
   This interim hook is tested only through the public `oxabl::build` API with a recording
   cache; no real cache is wired to it before slice 7.
7. At the end of the build, `finish` the uploader and print the store outcomes in the
   summary.

**Cancel.** A cancelled build uploads the units that already passed verification, V2
included (sets already pushed, and held sets that a V2 imaging has released), then calls
`finish`. It uploads nothing for units that are still compiling, and nothing for held
sets that no V2 imaging has released yet: oxabl starts no new imaging after a cancel.

### Which units are written

A unit's artifact set is written when all of these hold:

- the unit compiled in this build with status `ok`;
- the unit is keyable;
- the unit is not tainted (no unit in its closure failed in this build);
- the unit passed [verification](#verification).

Fetched units are not written again, except the local fill of remote hits in step 4.

## Verification

A key is computed before the compile, from the bytes that the refresh hashed. A file can
change between the hash and the moment the AVM reads it. A database schema can change
during a build. The analysis can miss a file that the compiler reads. In each case the
local result is still safe: the next build computes a new key, or compiles again (see V3).
But a cache entry with such a key would be a wrong hit for someone else. So every cache
write needs these checks. They run per unit, when the unit completes, so uploads overlap
with compiling.

| Check | How | On failure |
| --- | --- | --- |
| V1 inputs | Hash again every file in the unit's closure, and the unit, with hashes taken after the unit's `DONE`. A file hashed after that moment for another unit is reused. A `$DLC/...` file is hashed again by an idle worker (`HASH`), from the copy the AVM compiles with, not from the host mirror. Every hash must equal the hash the key used. | The unit is not written. |
| V2 schema | The databases are imaged again before held sets are released, split across the idle workers as in the probe ([worker-protocol.md](worker-protocol.md#schema-image)). An imaging starts when a set waits and `[build.verify].imaging_budget` times the duration of the last imaging has passed since it ended ([pace](#the-pace-of-v2)), and once at the end. A unit is released only after an imaging that **started after the unit finished** gives the schema identities of the probe. When the raw digest pins the image of every database ([compile-key.md](compile-key.md#the-raw-digest)), each check is a raw pass in place of the imaging, in every cache mode: an equal digest means equal metaschema records, so the identities of the probe. A changed digest is a changed schema; the build does not image again. | Every held and later unit of this build is not written. |
| V3 reads | Read the unit's cross-reference: the private text `XREF`, or the `XREF-XML` or text `XREF` that the user asked for. Take the file in the second column of every row (text) or the file name of every `Source` element (XML): these are the files that hold a reference, as the compiler resolved them. Map each name to a canonical path: a relative name is relative to the worker root, an absolute name under the worker's `dlc` becomes `$DLC/...`. Never resolve a name again on the host. Every one must be the unit or in its closure; a name that does not map fails the check. | The unit is not written. Its result is recorded with `key = NULL` and the file in `note`, so it compiles again at each build (`unverified`). The summary names the file and asks for a bug report, because the analysis missed an input. |

For a unit keyed by its [observed closure](compile-key.md#the-observed-closure), the closure
of V1 and V3 is the observed one, with its class part. Before them, the build observes the compile again: when it
read another closure than the key holds, the key does not describe the outputs, so the build
stores the new closure and writes nothing.

### The verification policy

`verify::policy::Policy` is the one value that every check reads. A build makes it once, at
the start, from the cache mode ([Trust](#trust)) and `[build.verify]`. `--print-config`
shows the mode as `verification.mode`, next to the `[build.verify]` settings it reads, and
the report holds the policy and the number of V2 imagings (`cache.verification`,
`cache.imaging_rounds`).

A cache that other people read is written only by a `ci` build: a `developer` build writes
its own namespace, and only that developer reads it. So the modes differ in how much a
wrong entry can cost, and the policy follows:

| Mode | Policy |
| --- | --- |
| `ci` | Every check, always, at full strength. No setting lowers one: a setting that would, while the build still writes a cache, is refused with exit code 2, in `Policy::resolve` and nowhere else. No such setting exists yet. |
| `developer` | The same checks as `ci` today. A cheaper form of a check (a sample, a reused pass) belongs here only, as a new field of the policy. |
| `read-only` | Nothing is written, so nothing is verified. |

`reads = false` lowers no check: it turns the cache write off, in every mode, and the build
says so in a note. Only a check that runs while a cache is still
written must be refused in `ci`.

### The pace of V2

An imaging costs every worker for its whole duration (3.6 s on 16 workers is about the work
of 100 compiles on the measured databases), so the number of units is the wrong trigger:
more workers make the same unit count a larger share of the build. A check starts when a
set waits and `imaging_budget` times the duration of the last imaging has passed since that
imaging ended. The imaging of the probe is the first one. When the checks are raw passes,
the duration of a raw pass takes the place of the imaging (the raw passes of the start are
the first one), so the checks come more often at the same share of the build. With the default of 30 the checks
cost at most about 1 / 31 of a build, for any database size and worker count. `0` starts a
check whenever a set waits. The check at the end stays, and the rule that a unit is released
only after an imaging that started after it finished does not change.

`batch_units` and `batch_interval` are still read, so that old files load, and ignored; the
build says so in a note. A longest wait (`batch_interval`) would force a check whatever it
costs, and so break the bound. Waiting costs no memory worth a bound: a held set is a key and
the paths of its files, never their bytes. The price is time and cancel. The uploads of held
sets start at the release instead of streaming, so a `ci` build ends later by the upload of
the last sets, and a build that is cancelled keeps the sets that no imaging has released.

### What V3 covers

Measured on OpenEdge 12.8.9 (`xref_xml_classes`, `xref_xml_resolution` and their text
forms `xref_text_classes`, `xref_text_resolution`; the text `XREF` and the `XREF-XML` name the
same files for all 545 units of a real code base):

- **Classes: not covered.** For a unit that uses a class from source (not a unit), from
  `$OUT` r-code or from a `.pl`, the cross-reference lists only the unit itself (as a
  `Source`, or in the second column). It names no class file at all (the reference appears as `Reference-type="NEW"` with the class
  name, never with a file).
- **Includes with only preprocessor definitions: not covered.** The compiler lists a
  file that holds a reference (a `Source`, or a row in the second column). An include that
  holds only `&GLOBAL-DEFINE`s is not listed. Its `INCLUDE` row gives the name as written,
  and neither the listing, the debug list nor `STRING-XREF` gives its resolved path.
- **Other includes: covered.** Every include that holds a reference is a `Source`, with its
  name as the compiler resolved it.

So V3 cannot see a divergent resolution of a class or of a definitions-only include. For
those reads the **closure that the plan builds is the only guard**: K1 and K2 make a unit
unkeyable when its analysis (with includes judged in the includer's context) is degraded or
leaves an include unresolved, and class resolution is keyed only with one PROPATH id (K9)
and without an external name collision (K8,
[compile-key.md](compile-key.md#per-unit-propath-classes)). A V3 source for these reads is
an open follow-up ([build-tool.md](../build-tool.md#follow-ups)).

V2 has one residual case: a schema that changes and changes back between two imagings, with
a compile in between. This needs two dictionary loads into the build's database during
the build; CI builds use a database of their own, and the case is stated, not hidden.

The private cross-reference is the text `XREF`: the compiler writes it in about a seventh of
the time and a sixth of the bytes of the `XREF-XML` (545 units, 16 workers: +1 s against +9 s
on a 14 s compile). It is on when any cache write is on (`[build.verify].reads`, default
`true`). With no cache that stores (`read-only`, `--no-cache`, no cache configured), only a
request that holds a unit keyed by its observed closure asks for it
([compile-key.md](compile-key.md#the-observed-closure)): the other compiles skip the
cross-reference and the read check. When the user requests an `xref` output, in either
`xref_format`, that file serves as the private one, so one compile gives both and the
directory of the output makes no difference. With `reads = false`, oxabl writes nothing to any
cache. The reader is `observe::xref` in `oxabl_build`; the opt-in OpenEdge tests check it
against real compiler output.

The text reader is strict, because a row of the text form is not a line. A name with a space
is in double quotes with `""` for a quote, a `STRING` row holds the value of a string, which
can hold line breaks, and the line of a row is a number or `IMPLICIT`. The reader follows
quotes across lines, takes the type of a row only from its fourth field, and fails on a row
of another shape, a quote that is not closed, an empty file, or a file that does not end with
a line break. A failure is a failed V3, as for an absent file: the unit is not cached and
has no key. A name that holds a space and is not quoted cannot be told from two fields, so
it fails too. The compiler writes no `COMPILE` row for encrypted source, so the first row is
not checked.

Verification costs one hash of each closure file per unit completion window (BLAKE3 on all
cores, about 1 s for 500 MB of source), one schema imaging per `imaging_budget` (about 6 s with 8 workers on the measured databases),
and one XREF read per unit.

## Local cache

`cache::local`.

### Trust

The local cache trusts **every process that runs as the OS user**. Any such process can
write an entry, and a local hit is not checked against a signer. This is right for a
developer's own machine. It is wrong for a CI runner that runs untrusted jobs (pull requests
from forks) and trusted jobs (protected branches) as one user, because an untrusted job
could plant an entry that a trusted job would install.

So the local cache is on **only in `developer` mode**. Oxabl resolves the mode as the client
does: `OXABL_CACHE_MODE` when set; otherwise `read-only` when the `CI` variable is set;
otherwise `developer`.

- **`ci` and `read-only` modes.** The shared local cache is off. A read-only packaging job on
  a shared runner is exactly the job that must not install a planted entry. A CI system that
  does not set `CI` (for example Jenkins, TeamCity or Bamboo agents) must set
  `OXABL_CACHE_MODE`; the user documentation says so, and without it the job runs in
  `developer` mode with the shared local cache on.
- **Per-job directory.** In `ci` and `read-only` modes, `[build.cache].local_job_dir` can name
  a directory that serves as the local cache for one job. Oxabl treats it like an output
  directory ([output ownership](../build-tool.md#output-ownership)):
  - the first time, it must be absent or empty; oxabl then writes the marker
    `.oxabl-cache-job` into it. A directory with files and no marker stops the build with
    exit code 2, and nothing is deleted;
  - oxabl refuses (exit code 2) the workspace root, any ancestor of it, `$HOME` and `/`;
  - at the start of each build oxabl deletes only `v1/` beneath it, never anything else.
- **`developer` mode.** The shared local cache is on by default. The user documentation says
  that it trusts every process of the user.
- **Provenance is informational.** A local entry that came from a remote hit records the
  tier and the signer in its manifest. Nothing authenticates that line: a process of the
  same user can write any value into it. `--report` therefore shows it as
  `origin (unverified): remote <tier> <signer>`, and it never changes what oxabl trusts.

### Location

- `[build.cache].local`, default `$XDG_CACHE_HOME/oxabl/artifacts` (`~/.cache/oxabl/artifacts`
  when `XDG_CACHE_HOME` is not set; `~/Library/Caches/oxabl/artifacts` on macOS).
- One cache serves every workspace of the user on the machine: keys are complete, so two
  worktrees or two branches share entries safely. A branch switch and back costs a fetch
  from local disk, not a compile.
- Oxabl creates the directory with mode `0700` and treats a directory that another user
  owns, or that the group or others can write, as unusable (every key a miss, one warning).

### Layout

```text
<dir>/v1/entries/<first 2 hex>/<64 hex>/manifest
<dir>/v1/entries/<first 2 hex>/<64 hex>/files/<artifact path>
<dir>/v1/entries/<first 2 hex>/<64 hex>/last-used
<dir>/v1/tmp/
<dir>/v1/quarantine/
<dir>/v1/lock
<dir>/v1/size
```

The manifest is text:

```text
oxabl-artifacts 1
key <64 hex>
origin <compiled | remote <tier> <signer id>>
file <size> <BLAKE3 hex> <percent-encoded artifact path>
...
end <BLAKE3 hex of all the lines above, each with its LF>
```

### Write

1. Create `tmp/<random>/` and put each file there by reflink, then hard link, then copy, in
   that order of preference (the entries of one batch on several threads), from the promoted output at its final path, for a compiled
   unit and for a remote hit alike (the stage file has been renamed away by then). Write the
   manifest last.
2. Call `syncfs` on the cache filesystem once per batch of entries, then `rename` each
   directory to `entries/<hh>/<hex>`. When the target exists, another process won; delete
   the temporary directory. An entry is complete or absent; never partial.
3. Add the entry size to `size` under the lock.

A hard-linked file shares its inode with the output. A later in-place edit of the output
would change the entry; the verified read below turns that into a miss, never a wrong
hit.

### Read

1. Read the manifest. Check the `end` line.
2. Copy each file to the fetch stage while hashing it. Its size and hash must match.
3. On any mismatch or read error, rename the entry to `quarantine/` and report a miss.
   Quarantined entries are deleted at the next eviction.
4. Set the mtime of `last-used` to now (best effort).

### Eviction

`[build.cache].local_max_size`, default 20 GiB. At the end of a build, when `size` is over
the limit, oxabl takes an exclusive `flock` on `lock`, removes entries with the oldest
`last-used` until the total is under 90 % of the limit (rename to `tmp/` first, then delete),
and recomputes `size` from the remaining manifests every hundredth eviction. A reader that
loses an entry during eviction gets a read error, which is a miss.

## Remote cache

### Crate and feature

`crates/oxabl_build_remote` holds one type, `RemoteCache`, which implements `ArtifactCache`
over the client of the oxabl-cache project.

- Dependency: `oxabl-cache-client` (`oxabl_cache_client`), declared once, in the root
  `Cargo.toml` under `[workspace.dependencies]`; `oxabl_build_remote` takes it with
  `workspace = true`. Only this crate depends on it, and CI fails when `cargo tree -p
  oxabl_build` or a default `cargo tree -p oxabl` names an `oxabl-cache` crate. Every crate
  of the oxabl-cache project is named `oxabl-cache-<part>`.
- **Until the client is published, it is a git dependency pinned to an exact `rev`** of the
  oxabl-cache repository. Publishing `oxabl-cache-client` (and `oxabl-cache-crypto` and
  `oxabl-cache-format`, which it depends on) is the project owner's decision. After it, the
  switch is one line: the `rev` becomes a version. **Until then the umbrella cannot be
  published with `cargo publish`**: crates.io refuses a crate with a git dependency, and the
  umbrella needs `oxabl_build_remote` on the registry even as an optional dependency, so the
  release's publish job fails on a branch that contains the remote cache.
- The umbrella has the feature `remote-cache = ["cli", "dep:oxabl_build_remote"]`. It is not
  a default feature, so `oxabl_wasm`, library users and a plain `cargo install oxabl` do not
  build the network and crypto stack. The release workflow builds one published binary
  with `--features remote-cache`: a static `x86_64-unknown-linux-musl` binary that is
  attached to the GitHub release of the `oxabl` crate with a `.sha256` file
  (`scripts/build-musl.sh`; [install in CI](../../overview.md#install-the-binary-in-ci)).
  CI builds it on every pull request and starts it in an `ubuntu:22.04` container.
- A binary without the feature, in a workspace where the remote cache is on (see below),
  stops with exit code 2: `this oxabl was built without remote-cache; rebuild with
  --features remote-cache or pass --no-remote-cache`. A silent fallback would make a CI job
  slow forever with no signal.

Rejected: a git dependency as the lasting form (crates.io cannot publish a crate with one;
it is the interim form only); a feature inside
`oxabl_build` (every dependent of `oxabl_build` would then decide it through feature
unification).

### Configuration and trust root

The client owns its configuration. It is one file, **`.oxabl/cache.toml`**, committed in the
customer's repository, with the keys `server`, `tenant` (the `t_...` id), `cache` (the
`c_...` id), `owner_keys` and `min_roster_version`. The adapter loads it with
`Config::from_repo(root)`.

- `tenant`, `owner_keys` and `min_roster_version` are the root of trust. Nothing in oxabl
  can set or override them: not `oxabl.toml`, not the overlay, not a flag, not an
  environment variable. The oxabl configuration schema has no key for them, so an overlay
  or an `oxabl.toml` that names them is an unknown-key error (exit code 2).
- **`TrustOverride` is exit 2, not a miss.** When `Config::from_repo` fails with
  `ConfigError::TrustOverride` (the environment sets one of the trust variables that the
  client refuses, such as `OXABL_CACHE_OWNER_KEYS`, `OXABL_CACHE_MIN_ROSTER_VERSION`,
  `OXABL_CACHE_CACHE` or `OXABL_CACHE_TENANT`), `oxabl build` stops with exit code 2 and
  names the variable. This is the one cache error that is not a miss: an attempt to replace
  the root of trust must be loud. Every other configuration error of the client is reported
  once, and the build runs without the remote cache.
- The OIDC audience is always `oxabl-cache:<tenant id>`, derived by the client from
  `tenant`. The client never takes an audience from the server.
- `server` may be overridden by `OXABL_CACHE_SERVER`; the client reads that variable. The
  server is untrusted anyway.
- Mode (`ci`, `developer`, `read-only`) and credentials come from the environment
  (`OXABL_CACHE_MODE`, which is exactly `ci`, `developer` or `read-only`;
  `OXABL_CACHE_IDENTITY`; `OXABL_CACHE_TOKEN`; OIDC with any issuer that publishes discovery
  and JWKS; Gitea Actions uses scoped tokens) or the client's flags. Oxabl passes them
  through and never reads, stores or prints a credential. Oxabl itself reads only
  `OXABL_CACHE_MODE` and `CI`, for the local cache rule.

`oxabl.toml` holds only the build-side switches ([config.md](config.md#buildcache)):
`[build.cache] remote` (default: on when `.oxabl/cache.toml` exists) and the idle timeout.

### Write tiers

Oxabl `push`es every verified set. The mode and the credentials decide what happens: a CI
identity writes to the shared tier, a developer writes only to their own tier, and
`read-only` writes nothing. Oxabl has no tier logic.
