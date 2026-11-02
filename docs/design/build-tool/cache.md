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
}

pub trait ArtifactUploader: Send {
    fn push(&mut self, set: ArtifactSetRef);
    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)>;
}
```

- **`fetch` streams.** It takes all requests in one call (the cache batches on the wire)
  and calls `on_outcome` once per key, as each one completes. An installed set is complete
  and verified under `stage/<key hex>/` at its artifact paths before its outcome is
  reported, and it stays installed whatever happens to later keys. `expect` is the list of
  artifact paths oxabl requires; a set with another list is a `Miss` with reason
  `PathMismatch`. A key the call never reports (a crash of the adapter, a timeout) is a
  miss. `Origin` names the cache, and for a remote hit its tier and signer.
- **`fetch` has an idle timeout, not a call timeout.** The call ends with a miss for each
  key not yet reported when no outcome and no byte progress has arrived for
  `[build.cache].idle_timeout` (default 30 s). A slow link that keeps moving is never cut
  off.
- **Durability.** The adapter installs into the stage without `fsync`
  (`Durability::Caller` in the client). Oxabl calls `syncfs(2)` on the stage filesystem
  once before it promotes a batch of fetched sets
  ([store.md](store.md#crash-safety)).
- **`uploader` streams.** Oxabl `push`es one set at a time during the build, as soon as the
  unit passes verification. A set names files on disk, not bytes; the cache reads them
  lazily and applies its own back-pressure, so memory stays bounded. `push` may block for
  back-pressure. The cache must check, when it reads a file, that its size and BLAKE3 hash
  equal `size` and `blake3`, and must drop the set otherwise: a file that changed after
  verification is never stored. `finish` flushes and returns one outcome per key.
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
3. **Outcome.** An `Installed` outcome goes to a promote batch. Before each promote batch
   (every 500 sets or every 2 s), oxabl calls `syncfs` on the stage, renames each file to
   its final path, and records the results with `origin` `2` (local) or `3` (remote). The
   node is finished. A `Miss` turns the node into a compile job.
4. **Local fill.** Each remote hit is also stored in the local cache (when the local cache
   is on), by reflink or hard link from the stage where the filesystem allows it, and by
   copy otherwise. The local entry records the remote tier and signer.
5. **Compile** the rest.
6. **Verify and write** each compiled unit as it completes
   ([Verification](#verification)): a unit that passes goes to the local cache and is
   `push`ed to the remote uploader at once.
7. At the end of the build, `finish` the uploader and print the store outcomes in the
   summary.

A cancelled build stops pushing at once and calls `finish` with what was pushed. Sets
already pushed passed verification, so they are valid entries.

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
| V1 inputs | Hash again every file in the unit's closure, and the unit, with hashes taken after the unit's `DONE`. A file hashed after that moment for another unit is reused. Every hash must equal the hash the key used. | The unit is not written. |
| V2 schema | The schema images are probed again by one idle worker before each upload batch is released (every 60 s or 500 units, and at the end). A unit is released only after a probe that **started after the unit finished** gives the images of the handshake. | Every held and later unit of this build is not written. |
| V3 reads | Read the unit's private `XREF-XML`. Take the file name of every `Source` element: these are the files the compiler opened, as the compiler resolved them. Map each name to a canonical path: a relative name is relative to the worker root, an absolute name under the worker's `dlc` becomes `$DLC/...`. Never resolve a name again on the host. Every one must be the unit or in its closure; a name that does not map fails the check. | The unit is not written. Its result is recorded with `key = NULL` and the file in `note`, so it compiles again at each build (`unverified`). The summary names the file and asks for a bug report, because the analysis missed an input. |

V2 has one residual case: a schema that changes and changes back between two probes, with
a compile in between. This needs two dictionary loads into the build's database during
the build; CI builds use a database of their own, and the case is stated, not hidden.

The private `XREF-XML` is on when any cache write is on (`[build.verify].reads`, default
`true`). When the user requests `xref-xml`, that output serves as the private one. When the
user requests text XREF (`xref_format = "text"`), one compile cannot give both forms, so
oxabl writes no cache entries for that configuration and says so at the start of each
build. With `reads = false`, oxabl writes nothing to any cache. The `XREF-XML` reader is in
`oxabl_effects`; slice 10 checks it against real compiler output.

Verification costs one hash of each closure file per unit completion window (BLAKE3 on all
cores, about 1 s for 500 MB of source), one schema probe per upload batch, and one XREF read
per unit.

## Local cache

`cache::local`.

### Trust

The local cache trusts **every process that runs as the OS user**. Any such process can
write an entry, and a local hit is not checked against a signer. This is right for a
developer's own machine. It is wrong for a CI runner that runs untrusted jobs (pull requests
from forks) and trusted jobs (protected branches) as one user, because an untrusted job
could plant an entry that a trusted job would install.

So:

- **CI and `ci` mode.** When `OXABL_CACHE_MODE` is `ci`, or the `CI` environment variable is
  set, the local cache is **off**, unless `[build.cache].local_job_dir` names a directory.
  That directory is the local cache for the job, and oxabl deletes everything in it at the
  start of each build. A CI job then gets its cache from the remote cache, where the
  signature and tier checks run.
- **Developer mode.** The local cache is on by default. The documentation for users says
  that it trusts every process of the user.
- **Provenance.** A local entry that came from a remote hit records the tier and the signer
  in its manifest. `--report` shows them for a local hit, so a user can see where an
  installed artifact came from.

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
   that order of preference, from the promoted output (a compiled unit) or the stage (a
   remote hit). Write the manifest last.
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

- Dependency: the published crate `oxabl-cache-client` (`oxabl_cache_client`), a normal
  semver range. Only this crate depends on it. Every crate of the oxabl-cache project is
  named `oxabl-cache-<part>`.
- The umbrella has the feature `remote-cache = ["cli", "dep:oxabl_build_remote"]`. It is not
  a default feature, so `oxabl_wasm`, library users and a plain `cargo install oxabl` do not
  build the network and crypto stack. The release workflow builds the published binaries
  with `--features remote-cache`.
- A binary without the feature, in a workspace where the remote cache is on (see below),
  stops with exit code 2: `this oxabl was built without remote-cache; rebuild with
  --features remote-cache or pass --no-remote-cache`. A silent fallback would make a CI job
  slow forever with no signal.

Rejected: a git dependency (crates.io cannot publish a crate with one); a feature inside
`oxabl_build` (every dependent of `oxabl_build` would then decide it through feature
unification).

### Configuration and trust root

The client owns its configuration. It is one file, **`.oxabl/cache.toml`**, committed in the
customer's repository, with the keys `server`, `cache` (the `c_...` id), `owner_keys` and
`min_roster_version`. The adapter loads it with `Config::from_repo(root)`.

- `owner_keys` and `min_roster_version` are the root of trust. Nothing in oxabl can set or
  override them: not `oxabl.toml`, not the overlay, not a flag, not an environment variable.
  The oxabl configuration schema has no key for them, so an overlay or an `oxabl.toml` that
  names them is an unknown-key error (exit code 2).
- `server` may be overridden by `OXABL_CACHE_SERVER`; the client reads that variable. The
  server is untrusted anyway.
- Mode (`ci`, `developer`, `read-only`) and credentials come from the environment
  (`OXABL_CACHE_MODE`, `OXABL_CACHE_IDENTITY`, `OXABL_CACHE_TOKEN`, OIDC detection) or the
  client's flags. Oxabl passes them through and never reads, stores or prints a credential.

`oxabl.toml` holds only the build-side switches ([config.md](config.md#buildcache)):
`[build.cache] remote` (default: on when `.oxabl/cache.toml` exists) and the idle timeout.

### Write tiers

Oxabl `push`es every verified set. The mode and the credentials decide what happens: a CI
identity writes to the shared tier, a developer writes only to their own tier, and
`read-only` writes nothing. Oxabl has no tier logic.
