---
title: "oxabl build: testing"
status: proposed
date: 2026-11-02
---

# Testing

Part of [the oxabl build PRD](../build-tool.md).

## Rules

- **No unit tests.** Tests drive the product the way a user or a wrapper does: the
  `oxabl` binary (`CARGO_BIN_EXE_oxabl`) over temporary directories, and, for slice 1 only,
  the public API of `oxabl_pipeline`. A test knows nothing of the internal modules.
- **A stub fakes the environment, never the program under test.** The fake AVM stands in
  for Progress. Nothing stands in for oxabl.
- **A check that cannot fail is worse than none.** Each acceptance test is broken once on
  purpose during its slice (remove the guarded behaviour, see the test go red with the
  assertion that names it), and the pull request says so.
- **Opt-in is never a silent pass.** The OpenEdge suite is `#[ignore]`. When it runs and
  its environment variables are missing, it fails; it does not skip.
- All fixtures are synthetic ABL written for the test.

## Where tests live

| Tests | Location |
| --- | --- |
| Observed analysis (slice 1) | `crates/oxabl_pipeline/tests/observations.rs` |
| CLI build and plan | `crates/oxabl/tests/build_*.rs`, one file per behaviour group |
| Golden key vectors | `crates/oxabl/tests/build_key_vectors.rs` with `crates/oxabl_build/tests/key_vectors.json` |
| Remote cache | `crates/oxabl/tests/build_remote_*.rs` (only the package that owns the `oxabl` binary gets `CARGO_BIN_EXE_oxabl`) |
| OpenEdge suite | `crates/oxabl/tests/build_openedge.rs` and `crates/oxabl/tests/build_openedge_scratch.rs` |
| Parity | `scripts/plan-parity.sh` |

## The fake AVM

`crates/oxabl_fake_avm` is a binary crate with `publish = false`. It depends on no oxabl
crate; a CI step checks that `cargo tree -p oxabl_fake_avm` names none. It implements
protocol 1 from [worker-protocol.md](worker-protocol.md) on its own, so a codec bug in
oxabl cannot hide behind the same bug in the fake.

The tests use it as the worker command:
`command = ["<path to oxabl_fake_avm>", "{program}"]`. It checks that `{program}` exists and
starts with the worker program's header line.

### What it does

- **Environment.** It reads the environment fields, the databases (each with its schema
  entries as entry texts, and its fingerprint `guid` and `stamp`) and an optional `dlc_dir`
  from a JSON file: `--env <file>` on its command line (so `{worker}` can pick a file per
  worker), or `FAKE_AVM_ENV`. A test changes the environment by changing that file.
- **Schema image.** It follows the spec on its own: it hashes each entry text with SHA-256,
  answers `SCHEMA <ldb> <part> <parts>` with its share of `SENT` entries and a
  `SCHEMA-END` with the counts, answers `FINGERPRINT`, and logs `schema <ldb>` per share.
- **Raw digest.** It answers `RAWDIGEST <ldb> <part> <parts> <buckets>` with its pieces, as
  the worker splits them: the records of a table go into the piece of its place, the others
  into `head`; each piece hashes the canonical text of its records. It logs `raw <ldb>` per
  share.
- **Compile.** It resolves the compile name on the request PROPATH (the first entry that
  has the file; `$OUT`-mapped entries first). It reads the source and follows its includes:
  every `{name}` and `{name args}` is resolved on the PROPATH and read, recursively. For each
  `INHERITS X`, `IMPLEMENTS X` and `NEW X(`, it reads `X.r` from the output entry when that
  exists, and the source otherwise. A `FAKE-READ <path>` line makes it read that file
  directly; the oxabl analysis does not see it, which tests the read check.
- **Class cache.** Like `MULTI-COMPILE`, it keeps each class it has compiled from source by
  class name for the rest of its session, and does not drop it when the PROPATH changes. So
  the fake behaves like the AVM when one class name has two files, and the per-unit PROPATH
  test sees the AVM-like output while it checks that no key and no cache write exist.
- **Startup parameters.** `FAKE_AVM_ENV` gives the command-line parameters and the `.pf`
  contents separately; the fake reports `startup_parameters` with `.pf` contents expanded,
  `-pf <path>` and `(end .pf)` items, as the AVM does.
- **Hashing.** It answers `HASH` with the SHA-256 of the file, `dir` or `none`, and
  `HASHSET` with the digest of the list. `FAKE_AVM_DLC` points it at its own copy of the
  `$DLC` files, so two workers can see different copies.
- **Outputs.** The `.r` is text: a header, the compile name, one line `read <path> <hash>` for
  each file read, and `compiled-at <nanoseconds>`. The last line makes outputs of one
  source differ between compiles, like real r-code. A test can read a `.r` and check that it
  matches the current files: that is the wrong-hit oracle. It writes the text XREF or the
  `XREF-XML` as requested, and the text XREF for the `private` mode; the text XREF has a
  `COMPILE` row, an `INCLUDE` or `NEW` row for each reference, and a row in each other file
  read, which puts that file in the second column (names with a space are quoted;
  `FAKE-XREF-RAW <text>` in a source ends the file with `<text>`, where `\n` is a line break,
  for a malformed or multi-line row); the `XREF-XML` has a `Source` element for each file it opened, with
  the name as it resolved it, an `INCLUDE` reference for each include with the text between
  its braces (name and arguments, as OpenEdge writes it), and a `NEW` reference for each
  class. A quoted include name (`{"x.i" &a = 1}`) is read without its quotes. Debug lists and listings hold the
  source text.
- **Faults**, from text in the source or from environment variables:

| Trigger | Effect |
| --- | --- |
| `FAKE-FAIL` in a source | `MSG` error 1234 at that line, `DONE fail`. |
| `FAKE-FAIL-ONCE` in a source | `MSG` error and `DONE fail` the first time (marker in `FAKE_AVM_STATE`), `DONE ok` after; tests the retry pass. |
| `FAKE-FAIL-SILENT` in a source | `DONE fail` with no `MSG` line; tests that a failed unit still carries a reason. |
| `FAKE-NO-OUTPUT` in a source | `DONE ok` but no `.r` in the stage; tests the `no output` failure. |
| `FAKE-WARN` in a source | `MSG` warning, `DONE ok`. |
| `FAKE-CRASH` in a source | Exit 1 while compiling it. |
| `FAKE-CRASH-ONCE` in a source | Exit 1 the first time, using a marker file in `FAKE_AVM_STATE`. |
| `FAKE-HANG` in a source | Sleep forever while compiling it. |
| `FAKE-SLOW` in a source | Sleep 2 s after reading the source and before writing outputs. |
| `FAKE_AVM_NOISE=1` | Print noise lines before and between protocol lines. |
| `FAKE_AVM_START_DELAY=<ms>` | Sleep before the handshake. |
| `FAKE_AVM_REQUEST_DELAY_MS=<ms>` | Sleep before each request, as a slow transport (for example `docker exec`) adds to every round trip; to measure the setup phases. |
| `FAKE_AVM_BAD_TOKEN=1` | Echo a wrong token. |
| `FAKE_AVM_EXIT_IN_HANDSHAKE=1` | Exit after `HELLO`. |
| `FAKE_AVM_EXIT_IN_HANDSHAKE_ONCE=1` | Exit after `HELLO` the first time only (marker in `FAKE_AVM_STATE`), to test a start retry that succeeds. |
| `FAKE_AVM_SCHEMA_FAULT=<fault>` | `duplicate-entry`, `bad-hash` (a hash that is not 64 lower-case hex digits) or `bad-count` (a wrong `SCHEMA-END` count). |
| `FAKE_AVM_SKIP_SHARE=1` | Answer its share with no table entries. |
| `FAKE_AVM_COUNT_SKEW=1` | Report a `tables` count one higher than the database has. |
| `FAKE_AVM_SCHEMA_CHANGE_AFTER=<n>` | Emit a different schema image (and raw digest) after `n` compiles. |
| `FAKE_AVM_RAW_SALT=<text>`, `FAKE_AVM_RAW_CHANGE_FILE=<path>`, `FAKE_AVM_RAW_CHANGE_FROM=<n>` | Change the raw digest and not the image: always, while the file exists, or from the `n`-th raw request of the process on. |
| `FAKE_AVM_RAW_FAULT=<fault>`, `FAKE_AVM_RAW_FAULT_SHARE=<p>` | Break the raw digest answer of share `p` (default 0): `missing-piece`, `extra-piece`, `bounds`, `bad-hash`, `bad-count`, `error` or `exit`. |
| `FAKE_AVM_COMPILE_MS=<n>` | Sleep `n` ms in each compile, to measure what oxabl spends between compiles. |
| `FAKE-RESOLVE-OTHER <name> <path>` in a source | Resolve include `<name>` to `<path>` instead of the first PROPATH match, as a divergent AVM would. |

- **Log.** It appends one line per compiled unit to `FAKE_AVM_LOG` (`<worker pid> <compile
  name>`), so a test counts compiles and sees which worker compiled what. It also writes its
  pid there at start and `exit` at quit, so a test can check that no worker stays alive.
  `FAKE_AVM_XREF_LOG` gets `<worker pid> <compile name> <XREF mode>` for each compiled
  unit, so a test sees which units got the private XREF.

## Scenarios per slice

Each scenario is one test. "Build" means `oxabl build` with the fake AVM.

### Slice 1: observed analysis

- A root that includes `a.i`, which includes `b.i`: the observation set reads both.
- Two roots that use one class through one `SharedRun`: both observation sets read the
  class, in either order of analysis.
- An include found in the third search directory: the first two paths are absent
  observations. Create the file in the first: the observation set names that path.
- A case-insensitive lookup: the listed directory is in the set.

### Slice 2: store, refresh, `--plan --files`

- Edit an include: the plan lists exactly its includers, reason `dependency-changed`.
- No change: the second plan reports nothing changed, and `--plan --json` shows the same
  store generation as the first.
- Create a file that shadows an include: its includers are analysed again and listed with
  `closure-changed`.
- Delete an include: its includers are listed; their edge sets are now degraded.
- Write a store with another `user_version`: the next plan rebuilds the store and prints
  the same plan as a fresh workspace.
- Truncate `state.db`: the next plan moves it aside (one `state.db.corrupt-*` file) and
  succeeds.
- Hold the lock with a running build: `--plan` succeeds twice and does not wait.
- A misspelled table (`[biuld]`) and an unknown key in `[build]`: exit code 2 with the key in
  the message.
- Edit a file in the same second as the previous refresh (set mtime equal to it): the racy
  rule hashes it again and the plan sees the change.

### Slice 3: `--since`, `--json`, parity

- A git fixture: commit, then edit one file, add an untracked include, delete a class:
  `--since HEAD` names the right units with `named` reasons.
- `--json` shape: field names and types, `format: 1`.
- `scripts/plan-parity.sh` on a synthetic pair of plans reports a planted miss and exits 1;
  with no miss it exits 0 and lists extras by reason.

### Slice 4: worker protocol

- The handshake fields reach `oxabl build --plan --probe --json` (`environment.source:
  "probe"`).
- Noise lines everywhere: the build still works and the log holds the noise.
- A start delay past `start_timeout`, a bad token, an exit during the handshake: each slot
  is retried twice; with every slot failing, exit code 3 and the log tail in the message.
- Two workers with different environment files: exit code 2, `workers see different
  environments`, and the field.
- `[workspace.preprocessor].proversion` differs from the worker: exit code 2 naming both
  values.
- No `[build.workers].command`: exit code 2 and the message from the PRD.
- A four-entry image with a 20 KB field, non-ASCII characters and `%`: the probe's schema
  identity equals an identity that the test computes on its own from the entry texts.
- A duplicate entry, a bad hash or a `SCHEMA-END` with a wrong count: a protocol error,
  and the worker is lost.
- The image split across 1, 2, 3 and 8 workers: the same identity, and each worker asks for
  its share of each database exactly once.
- A worker that skips its share, and workers that count different `tables`: exit code 3,
  `cannot image the databases`.
- Two workers with different fingerprints for one database: exit code 2, `workers see
  different environments`, field `fingerprint <db>`.
- A change of the schema context string changes the identity (the version is in the hash).
- SIGTERM during the start: the probe stops like on SIGINT, within 10 s, and every worker
  process group is gone.
- A parameter set only in the `.pf` and the same parameter set on the command line: equal
  canonical parameters and equal environment identities; `-pf` and `(end .pf)` are absent.
- A file-valued parameter (`-compileroptionsfile`): changing the file's bytes changes the
  environment identity; `-assemblies <dir>` makes every unit `environment-unknown`.

### Slice 5: keys

- The golden vectors: every key in `key_vectors.json` equals the key in `--plan --json`.
- One test per input of the key: the unit bytes, a closure file's bytes, a closure path, the
  unit PROPATH, each environment section, one dropped and one kept startup parameter, the
  schema image, each compile option, the outputs, the worker program. Each changes the key,
  except the dropped parameter, which must not.
- One test per fail-closed rule K1 to K7: the key is `null` and the reason is listed.
- A library on the PROPATH (`$LIB:` and a `$DLC/` `.pl`): changing its bytes changes the keys
  of the units with it on their PROPATH, and only theirs.
- An unresolved class under a prefix of `[build.external_packages]` whose library is not on
  the unit PROPATH: unkeyable (K3). With the library on the PROPATH: keyable.
- `x.p` and `x.w` in one directory: `x.p` compiles, `x.w` is skipped with
  `output-collision: <dir>/x.p`.
- The DLC default entries from the worker's startup PROPATH appear in every unit PROPATH in
  `--plan --json`.
- A `$LIB:` entry that the worker reports as a directory: its users are unkeyable with
  `unhashable-library`, also when an `external_packages` prefix maps to it. A prefix mapped
  to `"$DLC"` with only `$DLC/tty` (a directory) on the PROPATH: unkeyable (K3).
- A `$DLC/...` include whose worker-side copy differs from the host mirror (the fake hashes
  its own copy): its users are unkeyable with `dlc-mirror-mismatch`. Changing the
  worker-side copy alone changes the key.
- A workspace copied to another path: every key is equal.

### Slice 6: compile

- A class and its user: the class compiles first (the fake log order), and the user's `.r`
  reads the class's new `.r`.
- Priority: `main.p` waits on `pkg/User.cls`, which waits on `pkg/Base.cls`, and `a.p` waits
  on nothing; one worker. The order starts with `pkg/Base.cls`, and `pkg/User.cls` compiles
  before `a.p` (the completion of `Base` makes `User` ready before the worker takes its next
  job). Three deterministic tests through the public `oxabl::build::schedule::Queue` back
  the two rules of [scheduler rule 3](../build-tool.md#scheduler): no job goes out while a
  reported completion is unhandled; a job that a completion made ready goes before a
  lower-priority one; jobs that become ready together go out by priority whatever their push
  order.
- Plan, then build: `oxabl build --plan` on a fresh store, then `oxabl build`: every unit
  with no result compiles.
- `FAKE-FAIL-ONCE`: the unit fails, compiles on the retry pass and counts as one success;
  `FAKE-FAIL` compiles twice and fails. A unit lost twice is not retried.
- `FAKE-FAIL-SILENT` and `FAKE-NO-OUTPUT`: each failed unit has its reason (`the compiler
  reported a failure without a message`, `no output: r/<stem>.r`) in `--report`, the event
  and the next plan's `failed` reason.
- A worker lost during a cycle request: the in-flight unit and the units of the request
  that had not started go back as one job, the lost unit first.
- A plan after a build without `--probe`: `environment.source` is `"stored"`; a changed
  `include_paths` order gives `propath-changed` with `--probe`.
- The stage path in each `COMPILE` starts with `./`, and an `XREF-XML` output lands under
  `.oxabl/build/stage/...` (the opt-in OpenEdge test `compile_a_class_and_its_user` checks it on
  a real AVM).
- `[build.compile].options` with a character outside letters, digits and `-_:,.`: exit code
  2.
- Two classes in a cycle: one request with both.
- `FAKE-CRASH-ONCE`: the unit compiles on the second dispatch; one `worker-lost` event.
- `FAKE-CRASH`: the unit fails with `worker lost twice`; the other units compile.
- `FAKE-HANG` with `unit_timeout = "2s"`: the unit fails, the build ends.
- SIGINT during a build: exit code 130; within `cancel_grace` plus 10 s every fake worker
  logged `exit` or is gone; a following build compiles only what was not recorded. With a
  recording test cache: every unit released by V2 before the signal is pushed, `finish` is
  called once, and no unit that was still compiling at the signal is pushed.
- SIGKILL of oxabl during a build: every fake worker quits after its current unit (the
  stdin end-of-file).
- A second build: no compile in the fake log.
- An include edit: only its includers in the fake log.
- `FAKE-FAIL` in a class: its old `.r` is gone, its users compile and are tainted (no
  cache write), and the exit code is 1.
- `--events` and `--report`: the shapes, and one `unit-finished` per compiled unit.
- Per-unit PROPATH classes. `root_relative_include_paths = ["."]`, `directories = ["a",
  "b"]`, units `a/x.p` and `b/y.p` each doing `NEW pkg.Foo()`, class units `a/pkg/Foo.cls`
  and `b/pkg/Foo.cls`, and a unit `a/plain.p` that reaches no class. Exact outcome:
  - the build has two PROPATH ids;
  - both class units have the compile name `pkg/Foo.cls` and collide on `pkg/Foo.r`, so
    `a/pkg/Foo.cls` (the smaller path) compiles and `b/pkg/Foo.cls` is skipped with
    `output-collision: a/pkg/Foo.cls`;
  - `a/x.p`, `b/y.p` and `a/pkg/Foo.cls` are unkeyable with `per-unit-propath-classes`;
    `a/plain.p` is keyable;
  - one warning gives the number of units without a key;
  - `a/pkg/Foo.cls`, `a/x.p`, `b/y.p` and `a/plain.p` compile; the exit code is 0, because a
    skipped unit is not a failure;
  - nothing is written to any cache for the three unkeyable units; `a/plain.p` is written.

  The same workspace with `root_relative_include_paths = []` and `include_paths = ["a"]`
  (one PROPATH id, `b/` removed) has no warning and keyable users of `pkg.Foo`.
- Per-unit PROPATH classes, incremental. After the build above, touch only `b/y.p`: the
  plan still reports `per-unit-propath-classes` for it, although the other units are up to
  date and not in the rebuild set (PROPATH ids are counted over the whole graph).
- Class conflict with a library (one PROPATH id). `include_paths = ["src", "$LIB:acme"]`
  (`acme` is a `.pl`, digest `sha256:`), `case_insensitive_lookup = false`,
  `[build.external_packages] "Acme.Util." = ["$LIB:acme"]`, a class unit
  `src/Acme/Util/X.cls`, and `src/q.p` doing `NEW acme.util.x()`, which the analysis does not
  resolve (the case differs) and K3 accepts. `src/q.p` is unkeyable with
  `class-conflict: Acme.Util.X`, the warning names `src/Acme/Util/X.cls` and `$LIB:acme`, and
  nothing is cached for `src/q.p`. Without `src/Acme/Util/X.cls`, `src/q.p` is keyable.
- Class conflict, incremental: after that build, touch only `src/q.p`: still
  `class-conflict`, although `src/Acme/Util/X.cls` is up to date.
- A class unit `src/x/Foo.cls` that declares `pkg.Foo`: skipped with `package-mismatch`.
  A class unit `a/pkg/Foo.cls` with no `.` in `root_relative_include_paths` and no `a` in
  `include_paths`: skipped with `package-root-not-on-propath: a`.
- An output directory with a foreign file and no marker: exit code 2. A foreign `.r` put
  into a managed output directory: deleted by the next build, and no unit reads it.

### Slice 7: verification and the local cache

- Two copies of one workspace at different paths, one local cache: the second build has no
  compile in the fake log, and every output file is byte-identical to the first copy's.
- Flip one byte of a cached `.r`: the next fetch is a miss, the entry is in `quarantine/`,
  the unit compiles, and its `.r` passes the wrong-hit oracle.
- Remove one file from a cached entry: a miss, never a partial install.
- Edit a closure file while the fake compiles (the fake sleeps on `FAKE-SLOW`, the test
  edits): no cache entry for that unit; the next build compiles it again.
- `FAKE-READ` of a file outside the closure: no cache entry, the result has no key, and the
  summary names the file.
- `FAKE_AVM_SCHEMA_CHANGE_AFTER`: no cache entry for the units released after the change.
- The raw digest (`build_schema_reuse.rs`): an equal digest reuses the stored image and
  images nothing; a changed digest, a broken answer of either share (each `FAKE_AVM_RAW_FAULT`)
  and a lost worker image again; a stored image of another binding (each field) or with
  entries that do not give its identity is void; the digest is the same for 1 to 9 workers
  and a pass uses at most 8; the probe always images; a change between the two raw passes
  of the start stores nothing; the V2 checks are raw passes that image nothing, and a
  changed digest during the build stores nothing from that point.
- Two workers whose `$DLC` copies differ in one closure file (two fake environment
  directories): exit code 2, `workers see different $DLC files`, before any compile.
- No compile starts before step 7: with a fake that logs the time of each request,
  every `COMPILE` comes after the last `HASH`/`HASHSET` of the DLC hash phase and after the
  prune.
- `FAKE-RESOLVE-OTHER` for an include that holds only preprocessor definitions: the
  cross-reference names the other file, so check V3 fails and nothing is written.
- With `CI=1` and no `OXABL_CACHE_MODE`, and with `OXABL_CACHE_MODE=read-only` or `ci`: a
  planted entry in the shared local cache is never read. With `OXABL_CACHE_MODE=developer`
  it is (the documented trust).
- `local_job_dir`: a non-empty directory with no marker gives exit 2 and nothing in it is
  deleted; the workspace root, its parent, `$HOME` and `/` give exit 2; with a marker, only
  `v1/` is deleted at the start and a file next to it survives.
- A local entry whose manifest claims a remote origin: `--report` shows it as
  `origin (unverified)`.
- Uploads stream: with a recording test cache (a test-only `ArtifactCache` adapter that
  stands in for the remote service, not for oxabl), the first `push` arrives before the last
  unit compiles, and the cache sees file paths, not bytes. An output changed after its
  verification is dropped by the size and hash check.
- A fetch stream that stops after half the keys: the installed half stays installed and
  recorded; the rest compiles.
- A cache directory writable by others: a warning, a correct build, no cache use.
- Eviction: with a tiny `local_max_size`, the oldest entries go and a later fetch of one of
  them is a miss, not an error.

### The observed closure

`crates/oxabl/tests/build_observed.rs`, with a unit that starts with bytes the analysis
cannot parse (as an encrypted source looks to it) and that the fake compiles:

- It compiles in the first build and is not cached; the plan then keys it with
  `closure: "observed"`, the second build compiles nothing, and a second plan does not move
  the store generation. A change to an include that only the fake sees (in a comment) gives
  `dependency-changed`; that build compiles it and caches it; the next compiles nothing.
- A file of its observed closure that is gone gives no key (`observed-missing`), and the unit
  compiles; the new closure keys it again.
- A new file in an earlier PROPATH entry gives no key (`observed-shadowed`), and the compile
  reads the new file.
- With the same files and bytes, the observed-closure key differs from the key of the parsed
  closure (`databases = []` lets the analysis read the unit).
- A unit that uses a class gets a key; an edit to another workspace class, or to an include
  of the class it uses, rebuilds it. A new `.cls` gives `observed-classes-changed`, the
  compile records it, and the next build compiles nothing. A degraded workspace class gives
  `observed-class: <class>: degraded...`.
- Two class units skipped with `package-root-not-on-propath` leave the observed key in
  place; an edit to one of them still rebuilds the unit.
- With two workers, a unit that qualifies starts only after a slow class unit finished
  (`--events`).
- A compile that reads a file where the host resolves another (`FAKE-RESOLVE-OTHER`) gives
  no observed key and a note.
- `observe::xref` has a unit test for the `INCLUDE` shapes that OpenEdge 12.8 writes.
- With `xref_format = "text"` the unit is observed from that file, as from the private one.
- A compile that reads another closure than its key holds is not cached, and its closure
  keys the unit.
- A file that the refresh did not hash and that changed just before the build gives no key
  after the first compile; the second compile, under a key made before it, does.

### Slice 8: remote cache

These start the real oxabl-cache binaries: the server (`OXABL_CACHE_SERVER_BIN`), the admin
tool (`OXABL_CACHE_ADMIN_BIN`) and the member CLI (`OXABL_CACHE_BIN`).
`scripts/remote-cache-binaries.sh <dir>` builds all three at the revision pinned in the root
`Cargo.toml`; the CI job `remote-cache` runs it (cached on that file and the script) and then
the tests with `--features remote-cache`. A missing variable or file fails every test with a
message that says how to build them; nothing skips. The tests are `required-features =
["remote-cache"]`; `build_remote_absent.rs`, built only without the feature, checks the exit
2 of a binary without it.

**Rosters are set up only through the binaries,** as a customer would: the admin tool makes
the tenant; the member CLI makes the owner identity (`identity new` with a passphrase file),
the cache is created over HTTP with the owner key the CLI printed, then `init` (with a
`cache:admin` token file) and `member add` for a `ci`, a `developer` and a `reader` member.
Secret files are mode `0600`. No test links the cache's crypto or format crates.

- A CI-mode build writes; a second workspace with a fresh local cache fetches everything and
  compiles nothing; outputs are byte-identical.
- A proxy in front of the server that returns garbage bodies: misses, a correct build,
  exit code 0, one warning.
- The server is down: a correct build, exit code 0, one warning.
- A server that sends slowly but without a gap longer than `idle_timeout`: every key is
  fetched. A server that stops sending: the keys not yet reported are misses after
  `idle_timeout`, and the build is correct.
- A binary without the feature and a workspace with `.oxabl/cache.toml`: exit code 2.
- A non-default output layout (`output = "rcode"`, `xref = "reports/xref"`): CI writes, a
  second workspace with the default layout fetches every unit and installs the files at its
  own paths; the uploads read the files from both directories.
- Each trust variable the client refuses (`OXABL_CACHE_OWNER_KEYS`,
  `OXABL_CACHE_MIN_ROSTER_VERSION`, `OXABL_CACHE_CACHE`, `OXABL_CACHE_TENANT`), one test
  each: exit code 2 naming the variable, no compile and no cache use.
- `tenant` in `oxabl.toml` or in the overlay: exit code 2.
- A workspace under a symbolically linked directory: uploads succeed (a link above the
  final component is allowed). An output file that is itself a symbolic link: that set is
  `NotStored { Unreadable }`, the build exits 0.
- `OXABL_CACHE_MODE=read-only`: the uploader is a no-op, the summary says `read-only, 0
  sets offered`, and the build exits 0.
- `OXABL_CACHE_MODE=ci` with an uploader that cannot start (the test server refuses the
  credentials for writing): one warning with the error, every pushed set reported as
  `NotStored { Unavailable }`, the summary does not say `read-only`, and the build exits 0.
- `owner_keys` or `min_roster_version` in `oxabl.toml`, in the overlay, or as a flag: exit
  code 2. `OXABL_CACHE_SERVER` changes the server and nothing else.

### Slice 9: handover

- An overlay over `oxabl.toml`: overlay values win key by key, arrays replace;
  `--print-config` shows the sources. The overlay reaches the analysis (an include that
  resolves only through the overlay's `include_paths`), the workers and the outputs.
- Precedence flag, environment, overlay, `oxabl.toml`, default, for `dlc`, `count`, the
  cache directory and the two cache flags; `--config-overlay` beats `OXABL_BUILD_OVERLAY`.
- `--print-config` stops before any lock, worker or store write; its `--json` shape; its
  conflicts with `--plan`, `--probe` and the other build flags (exit 2).
- An overlay with `[workspace.lint]`, `[workspace.style]`, a misspelled table or an unknown
  key in any allowed table: exit code 2 naming the file and the key.
- A secret-looking key (five spellings), in the overlay and in `oxabl.toml`: exit 2, and the
  value is never printed.
- Each trust key (`tenant`, `cache`, `owner_keys`, `min_roster_version` in `[build.cache]`)
  and a `[build.cache.remote]` table, in the overlay and in `oxabl.toml`: exit 2 with a
  message that names `.oxabl/cache.toml`.
- No `oxabl.toml` and a complete overlay: the build works; without `[workspace] name`, exit 2.
- A typo in `[workspace.sources]` or `[workspace.schema]`: exit 2 for `oxabl check` and
  `oxabl build`; a warning and the default configuration for `format`, `analyze`, `explain`
  and `conformance`.
- Wrapper: a shell script writes an overlay, runs `oxabl build --root --config-overlay
  --events --report`, reads the events line by line while the build runs, checks the report
  and exits with the build's code: a build, a no-change build, a failing unit. A test checks
  that `docs/build-wrapper.md` holds the script verbatim.

## Opt-in OpenEdge suite

Two test files, every test `#[ignore]`. When a test runs and its variables are missing, it
fails; it never skips.

- `crates/oxabl/tests/build_openedge.rs` runs against an existing OpenEdge container or
  installation. Variables: `OXABL_OPENEDGE_COMMAND` (the worker command as a JSON array with
  `{program}` and `{root}`; it connects the databases through its `.pf` and starts in
  `{root}`), `OXABL_OPENEDGE_WORKSPACE` (the host directory the worker sees),
  `OXABL_OPENEDGE_WORKER_ROOT` (the same directory as the worker sees it),
  `OXABL_OPENEDGE_PROVERSION` (exactly as `PROVERSION` prints it, for example `12.8`),
  `OXABL_OPENEDGE_DATABASES` (the connection order, at least one database) and
  `OXABL_OPENEDGE_WORKERS` (worker counts for the timing test, default `1,4,8`). That two
  databases loaded from one `.df` give one identity is proven by the scratch suite, so no
  variable names such databases.
- `crates/oxabl/tests/build_openedge_scratch.rs` creates scratch databases from the empty
  database of the installation and loads `.df` edits into them, never touching existing
  databases. Variables: `OXABL_OPENEDGE_EXEC` (the exec prefix as a JSON array with
  `{root}`), `OXABL_OPENEDGE_SCRATCH` (a directory inside the container),
  `OXABL_OPENEDGE_WORKSPACE`, `OXABL_OPENEDGE_WORKER_ROOT` and `OXABL_OPENEDGE_PROVERSION`.
  It probes single-user (`-1`) with one worker; run it with `--test-threads 1`.

```bash
cargo test -p oxabl --test build_openedge -- --ignored --nocapture
cargo test -p oxabl --test build_openedge_scratch -- --ignored --nocapture --test-threads 1
```

The compile tests of later slices use a small synthetic fixture project
(`crates/oxabl/tests/fixtures/openedge/`): procedures, includes, a class hierarchy, a cycle
of two classes, a temp-table, and a procedure that reads a database table.

### The r-code oracle

r-code is not byte-deterministic, so the suite compares r-code with a **mask**. The varying
bytes are 6-byte time stamps at 3 to 5 places of a file, whose first byte flips between two
values and whose last byte changes slowly, so two or three compiles miss some of them
(measured: three samples left 3 varying bytes outside the mask, in class r-code). The mask
is built once per run, in a root of its own, from **8 compiles 1.2 s apart**, and covers 2
bytes on each side of every byte that varied. Two r-code files are **equivalent** when they
have the same size and differ only at masked offsets. The mask must be under 1 % of the file
for every unit above 20,000 bytes, and at least one such unit must exist (the fixture's
`bulk.p`, about 190 KB of r-code, has 0.64 %); small units sit at 0.7 to 1.1 %. A tamper
control in `portable_rcode` flips one unmasked byte and expects the oracle to refuse it.

### Tests

| Test | What it proves |
| --- | --- |
| `transport` | The worker reads stdin in batch mode, and each `PUT UNFORMATTED ... SKIP` line reaches stdout at once, including a 30,000-byte line. |
| `probe_fields` | Every handshake field is present and has the expected form. |
| `probe_a_real_avm` | The fields, at least one entry per database, and an identity that is stable across two probes. |
| `two_workers_agree_on_one_environment` | Two workers give one environment, fingerprints included. |
| `probe_time_by_worker_count` | Prints `TIMING workers=N wall=...` for each `OXABL_OPENEDGE_WORKERS` count and asserts equal identities. |
| `databases_loaded_from_one_df_give_one_schema_identity` (scratch) | Two databases created from the empty database at different times, with one `.df` loaded: different cache stamps, one schema identity. |
| `schema_edit_table` (scratch) | Each `.df` edit of the [fingerprint table](compile-key.md#database-fingerprints) changes the schema identity; the test records whether the cache stamp moved. |
| `schema_long_field` (scratch) | A field description of 30,600 bytes hashes without error and changes the identity. A longer value cannot be made: the `.df` load (error 63), an ABL assignment (9324) and the dictionary (12370) refuse more than about 32,000 bytes. The worker reads every character value through a `LONGCHAR`, so this exercises the only path. |
| `a_system_table_edit_changes_the_key_of_a_unit_that_reads_it` (scratch) | An edit of the metaschema table `_User` (a field order) on one of two databases loaded from one `.df` changes the schema identity and the key of a unit that reads `_User`: the `system` entry of the [schema image](compile-key.md#schema-identity) holds its CRC. |
| `the_raw_digest_follows_every_schema_edit_and_no_row_change` (scratch) | Each `.df` edit of `schema_edit_table`, an initial value, mandatory, `CAN-READ`, a table and a field trigger, a sequence change and a direct ABL assignment to `_Field` change the raw digest and image again; a created row and `NEXT-VALUE` do not, and the stored image is reused. |
| `the_raw_digest_is_the_same_under_utf8_and_iso8859_1` (scratch) | One database gives one raw digest under `-cpinternal UTF-8` and `ISO8859-1` (`LENGTH(raw)` without `"RAW"` fails it). |
| `the_raw_digest_on_a_served_database` (scratch) | `proutil idxactivate` changes the digest; on a served database the digest equals the single-user one and is the same for 1, 2 and 8 workers; the second build reuses the image; an SQL `CREATE TABLE` (`sqlexp` over TCP) changes the digest. |
| `startup_pf` | A parameter set only in a `.pf` is in the probe, and its canonical form equals the same parameter on the command line. |
| `param_file_hash` | Changing the bytes of a `-compileroptionsfile` file changes the environment identity. The AVM reads that file at its start and refuses a line that is not an option (error 18408), so the test changes a `#` comment line. |
| `library_hash` | `HASH` gives the SHA-256 of a `.pl`; a changed `.pl` changes the keys of its users. |
| `dlc_default_entries` | A unit that uses an `OpenEdge.*` class compiles with no `$DLC` entry in `include_paths`. |
| `compile_fixture` | The fixture builds; outputs exist; a planted error gives a `MSG` with row and column. |
| `class_order` | The cycle and the hierarchy compile; users of a changed class compile after it. |
| `portable_rcode` | The fixture copied to two roots gives equivalent r-code for every unit. |
| `xref_does_not_change_rcode` | With and without `XREF`, equivalent r-code. |
| `two_workspaces_share_compiled_units_through_the_local_cache` | A first workspace compiles, a second one at another path compiles nothing, fetches every unit from the local cache, and the fetched r-code runs (passed on OpenEdge 12.8 with slice 7). |
| `run_target_not_read` | Editing a `RUN` target leaves the caller's r-code equivalent. |
| `per_unit_propath_classes` | The slice-6 per-unit PROPATH layout on a real AVM: `a/pkg/Foo.cls` compiles, `b/pkg/Foo.cls` is skipped with `output-collision`, `a/x.p`, `b/y.p` and `a/pkg/Foo.cls` are unkeyable with `per-unit-propath-classes`, nothing is written to any cache for them (`a/plain.p` is), the exit code is 0. Recorded: both users run with `a`'s `Foo`. |
| `per_unit_propath_class_binding` | The same layout, where `b/pkg/Foo.cls` has one more method and `b/y.p` calls it: `b/y.p` fails with error 12927 (`Could not locate element 'Extra' in class 'pkg.Foo'`), because `MULTI-COMPILE` bound it to `a`'s `Foo`. Pins the [follow-up](compile-key.md#follow-up-classes-under-per-unit-propaths). |
| `one_propath_gives_keyable_class_users` | With one PROPATH id, class users are keyable and stored. |
| `xref_xml_classes` | A unit using a class from source, from `$OUT` r-code and from a `.pl`: in all three cases the `XREF-XML` `Source` elements list only the unit itself, no class file ([cache.md](cache.md#what-v3-covers)). Goes red if a later OpenEdge lists one. |
| `mixed_provenance` | Classes `t.B`, `t.A INHERITS t.B` and a program doing `NEW t.A()`: compile all three, compile `B` again, run the program with the old `A.r` and the new `B.r`: the result is correct. A regression guard for the per-unit key (measured to pass on OpenEdge 12.8). |
| `xref_xml_resolution` | The `XREF-XML` `Source` elements name each include that holds a reference, as the compiler resolved it (`src/defs.i`, `src/ttrows.i`); an include with only `&GLOBAL-DEFINE`s (`consts.i`) is **not** listed; every fixture unit passes V3 and is stored. Goes red if a later OpenEdge lists the definitions-only include. |
| `xref_xml_observed_closure` | The `XREF-XML` of `main.p` has an `INCLUDE` reference for `defs.i`, `ttrows.i` and the definitions-only `consts.i`. The [observed closure](compile-key.md#the-observed-closure) is sound only while this holds. Its class part does not rely on the `XREF-XML`: a class used only as a type gives no reference (measured on 12.8). |
| `xref_text_classes` | The same three class cases with `xref_format = "text"`: the second column names only the unit. |
| `xref_text_resolution` | `main.p` with a requested text `XREF`: the second column names `src/main.p`, `src/defs.i`, `src/ttrows.i` and equals the `XREF-XML` `Source` list; every fixture unit passes V3 and is stored, with a requested and with the private text `XREF`. |
| `xref_text_observed_closure` | The text `XREF` of `main.p` has an `INCLUDE` row for `defs.i`, `ttrows.i` and the definitions-only `consts.i`. |
| `xref_text_unusual_names` | A skipped `&IF` include, a unit with a space in its name and an include path of 300 characters: the text `XREF` reads back and the units are stored. |
| `xref_readers` | The text reader finds every include of the fixture (its `INCLUDE` rows give the name as written, not as resolved); the XML reader finds the includes that hold a reference. |
| `orphans` | After `SIGKILL` of oxabl during a 3,000-unit build, no `_progres` of that build is alive within 120 s. |
| `cancel_leaves_no_worker` | `SIGINT` during a build gives exit code 130, and no worker of the build stays alive. |

If `portable_rcode` or `xref_does_not_change_rcode` fails, the key specification changes as
[compile-key.md](compile-key.md) says, before any cache slice ships.

## Planner parity

`scripts/plan-parity.sh <oxabl-plan.json> <downstream-rebuild.txt>` compares oxabl's rebuild
set (units with `action: rebuild`) with a list of paths from the downstream tool (one
root-relative path per line). It prints:

- **missed**: paths in the downstream list that oxabl does not rebuild. Any missed path
  makes the script exit 1.
- **extras**: paths oxabl rebuilds that the downstream tool does not, grouped by oxabl's
  first reason.

The private corpus stays outside this repository. Slice 3 runs the script on it for a set of
edits (an include used by many units, a class body edit, a class signature edit, a new file
that shadows an include, a deleted include, a schema change) and reports the counts in the
pull request with no paths. The class body edit is expected to give extras (oxabl keys on
full source); every other row is expected to give few.
