---
title: "oxabl build: the cache seam, the local cache and the remote cache"
status: proposed
date: 2026-11-02
---

# The cache seam, the local cache and the remote cache

Part of [the oxabl build PRD](../build-tool.md).

## The rule

A cache is an optimisation. **Any cache error is a miss.** It is never a failed build and
never a wrong hit. A wrong hit is not possible when four things hold, and this document
makes each one hold:

1. The key is complete, or there is no key ([compile-key.md](compile-key.md)).
2. Only verified artifact sets are written ([Verification](#verification)).
3. A read checks every byte before it installs ([Local cache](#local-cache); the remote
   client does the same in its own design).
4. Oxabl installs a fetched set only when its file list is exactly the expected outputs of
   the unit.

## The seam

The seam is fixed. Oxabl computes a 32-byte plaintext `CompileKey` for each keyable unit.
It hands a cache the key and a list of artifact files: (path, bytes). The cache does
everything else: for the remote cache that is lookup ids, compression, encryption,
signing, transfer, verification and atomic install. The cache never defines what goes
into a key.

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
relative to the artifact root, not to the workspace root. Two workspaces with different
output directories share entries. An artifact path is a canonical relative path
([compile-key.md](compile-key.md#canonical-paths)) whose first component is one of the four
kinds.

### The trait

`oxabl_build::cache::seam`:

```rust
pub struct CompileKey(pub [u8; 32]);

pub struct ArtifactPath(String);

pub struct ArtifactFile {
    pub path: ArtifactPath,
    pub bytes: Vec<u8>,
}

pub struct ArtifactSet {
    pub key: CompileKey,
    pub files: Vec<ArtifactFile>,
}

pub enum Fetched {
    Installed { key: CompileKey, dir: PathBuf, files: Vec<ArtifactPath> },
    Miss { key: CompileKey },
}

pub trait ArtifactCache: Send + Sync {
    fn label(&self) -> &str;
    fn contains(&self, keys: &[CompileKey]) -> Result<Vec<bool>, CacheError>;
    fn fetch(&self, keys: &[CompileKey], stage: &Path) -> Result<Vec<Fetched>, CacheError>;
    fn store(&self, sets: Vec<ArtifactSet>) -> Result<StoreSummary, CacheError>;
}
```

- `contains` answers for many keys at once.
- `fetch` installs each hit atomically under `stage/<key hex>/`, at its artifact paths, and
  returns the directory. A file is never at its path until it is complete and verified.
- `store` writes complete sets. It is idempotent; a key that is already present is left as
  it is (the first accepted write wins).
- The trait is blocking. The remote adapter runs its client on a runtime that it owns.
  `oxabl_build` has no async runtime.

`cache::layered` chains the local cache and the remote cache. `run.rs` calls every cache
method through one wrapper that turns `Err`, a panic (`catch_panic`) and a timeout into a
miss, counts it, and prints one warning per cache per build.

## Build flow with a cache

After the plan ([build-tool.md](../build-tool.md#the-build-flow), step 6):

1. `K` = the keyable units of the rebuild set.
2. **Local.** `local.contains(K)` gives the hits `L`. `local.fetch(L)` installs them under
   `<state>/stage/<build-id>/fetch-local/`.
3. **Remote.** `remote.contains(K - L)` gives `R`. `remote.fetch(R)` installs them under
   `<state>/stage/<build-id>/fetch-remote/`. Each remote hit is also stored in the local
   cache, so the next build on this machine needs no network.
4. **Check.** A fetched set whose file list is not exactly the expected artifact paths of
   the unit (the `.r` and each requested output) is a miss.
5. **Promote and record** each fetched set, as for a compiled unit, with `origin` `2`
   (local) or `3` (remote). The units leave the rebuild set before the first dispatch, so
   the scheduler treats them as finished.
6. **Compile** the rest.
7. **Verify and write** after the retry pass: each eligible unit's set is written to the
   local cache, and to the remote cache when `[build.cache.remote].write` is on.

A cancelled build writes nothing to a cache.

### Which units are written

A unit's artifact set is written when all of these hold:

- the unit compiled in this build with status `ok`;
- the unit is keyable;
- the unit is not tainted (no unit in its closure failed in this build);
- the unit passed [verification](#verification).

Fetched units are not written again, except the local fill of remote hits in step 3.

## Verification

A key is computed before the compile, from the bytes that the refresh hashed. A file can
change between the hash and the moment the AVM reads it. A database schema can change during
a build. The analysis can miss a file that the compiler reads. In each case, the local result
is still safe: the next build computes a new key, or compiles again (see V3). But a cache
entry with such a key would be a wrong hit for someone else. So every cache write needs
these checks, after the retry pass:

| Check | How | On failure |
| --- | --- | --- |
| V1 inputs | Read and hash again every file in the closures of the units to write, and each unit. Each file is hashed once. Every hash must equal the hash that the key used. | The unit is not written. |
| V2 schema | One worker emits the schema images again before shutdown. They must equal the images of the handshake. | No unit of this build is written. |
| V3 reads | Read the unit's XREF (the requested one, or the private one). Take every source file the compiler read: the source file column of each row, and the target of each `INCLUDE` row resolved on the unit PROPATH. Map each name to a canonical path: a relative name is relative to the worker root, and an absolute name under the worker's `dlc` becomes `$DLC/...`. Every one must be the unit or in its closure; a name that does not map fails the check. | The unit is not written. Its result is recorded with `key = NULL` and the file in `note`, so it compiles again at each build (`unverified`). The summary names the file and asks for a bug report, because the analysis missed an input. |

The private XREF is on when any cache write is on and no XREF output is requested
(`[build.verify].reads`, default `true`). With it off, oxabl writes nothing to any cache.
The XREF reader is the text reader in `oxabl_effects`, plus an XREF-XML reader for the
`Source` elements. Slice 10 checks both readers against real compiler output.

Verification costs one hash pass over the source tree (BLAKE3 on all cores, about 1 s for
500 MB of source on a current machine), one schema probe, and one XREF read per unit.

## Local cache

`cache::local`. It is on by default.

### Location and trust

- `[build.cache].local`, default `$XDG_CACHE_HOME/oxabl/artifacts` (`~/.cache/oxabl/artifacts`
  when `XDG_CACHE_HOME` is not set; `~/Library/Caches/oxabl/artifacts` on macOS).
- One cache serves every workspace of the user on the machine: keys are complete, so two
  worktrees or two branches share entries safely. A branch switch and back costs a fetch
  from local disk, not a compile.
- The cache is for one user. Oxabl creates the directory with mode `0700` and refuses (as a
  cache error, so a miss for the whole build) a directory that another user owns or that
  the group or others can write. A shared cache between people is the remote cache.

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
file <size> <BLAKE3 hex> <percent-encoded artifact path>
...
end <BLAKE3 hex of all the lines above, each with its LF>
```

### Write

1. Create `tmp/<random>/` and write each file and then the manifest. `fsync` each file and
   each directory.
2. `rename` the directory to `entries/<hh>/<hex>`. When the target exists, another process
   won; delete the temporary directory. The entry is complete or absent; never partial.
3. Add the entry size to `size` under the lock.

### Read

1. Read the manifest. Check the `end` line.
2. Copy each file to the fetch stage while hashing it. Its size and hash must match.
3. On any mismatch or read error, rename the entry to `quarantine/` and return a miss.
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
over `oxabl_cache_client`.

- Dependency: `oxabl-cache-client` from crates.io, a normal semver range. Only this crate
  depends on it.
- The umbrella has the feature `remote-cache = ["cli", "dep:oxabl_build_remote"]`. It is not
  a default feature, so `oxabl_wasm`, library users and a plain `cargo install oxabl` do not
  build the network and crypto stack. The release workflow builds the published binaries
  with `--features remote-cache`.
- A binary without the feature and a configuration with `[build.cache.remote]` stops with
  exit code 2: `this oxabl was built without remote-cache; rebuild with --features
  remote-cache or pass --no-remote-cache`. A silent fallback would make a CI job slow
  forever with no signal.

Rejected: a git dependency (crates.io cannot publish a crate with one); a feature inside
`oxabl_build` (every dependent of `oxabl_build` would then decide it through feature
unification).

### Configuration and secrets

Oxabl owns two keys of `[build.cache.remote]`: `write` (default `true`) and `timeout`
(default `"30s"`, per call). It passes the rest of the table, unchanged, to the client's
configuration type, which parses it strictly (an unknown key is an error) and documents it
(server URL, cache id, trust policy). Credentials and key material come only from the
environment variables that the client defines. Oxabl never reads them, never stores them,
and never prints them. The client's configuration type has no field for a secret, so a
secret written into `oxabl.toml` or an overlay is an unknown-key error.

### Write tiers

Oxabl calls `store` when `write` is on. The client's credentials decide the tier: a CI
identity writes to the shared tier, a developer writes only to their own tier, by server
policy. Oxabl has no tier logic. A developer who does not want uploads sets `write = false`.

### Timeouts and batching

Oxabl calls `contains` once with all keys, `fetch` once with all hits, and `store` once with
all sets. The client batches on the wire. A call that passes `timeout` is a miss (for
`store`, a warning).
