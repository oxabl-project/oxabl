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
| Golden key vectors | `crates/oxabl/tests/build_key_vectors.rs` with `crates/oxabl_build/tests/key_vectors.json`; they pin the worker digest of release 5.3.2 |
| Worker program: golden digest of the compile part, the alias, the driver lint | `crates/oxabl_build/tests/worker_program.rs` |
| Compiler messages without a position, and the `DONE` message count | `crates/oxabl/tests/build_compile_messages.rs` |
| Remote cache | `crates/oxabl/tests/build_remote_*.rs` (only the package that owns the `oxabl` binary gets `CARGO_BIN_EXE_oxabl`) |
| OpenEdge suite | `crates/oxabl/tests/build_openedge.rs` and `crates/oxabl/tests/build_openedge_scratch.rs` |
| The fake runtime and the shared case table of the runtime rule | `crates/oxabl/tests/build_runtime_fake.rs` with `crates/oxabl/tests/common/runtime_cases.rs` |
| R-code beside the source (`[build].rcode = "beside-source"`), asked of the fake runtime | `crates/oxabl/tests/build_beside_source.rs` |
| Parity | `scripts/plan-parity.sh` |

## The fake AVM

`crates/oxabl_fake_avm` is a binary crate with `publish = false`. It depends on no oxabl
crate; a CI step checks that `cargo tree -p oxabl_fake_avm` names none. It implements
protocol 1 from [worker-protocol.md](worker-protocol.md) on its own, so a codec bug in
oxabl cannot hide behind the same bug in the fake.

The tests use it as the worker command:
`command = ["<path to oxabl_fake_avm>", "{program}"]`. It checks that `{program}` exists and
starts with the worker program's header line. `DONE` carries the number of `MSG` lines, as
the protocol says; the fake implements that on its own.

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
  `INHERITS X`, `IMPLEMENTS X` and `NEW X(`, it searches the PROPATH as the AVM does: the
  first entry (the output entry first) that holds `X.r` or `X.cls`, exact in case, the
  r-code first in an entry. A `FAKE-READ <path>` line makes it read that file
  directly; the oxabl analysis does not see it, which tests the read check. A `NEW X` in a
  comment is a class use that the fake sees and the analysis does not.
- **Error 12927.** Binding the class r-code of another unit of the same request fails the
  compile with errors 12927 and 196 when that r-code was compiled from a source of that
  unit whose bytes have changed since (the first `read` line of the r-code). This is the
  model of stale r-code of a cycle that does not fit the new sources (E8 cycle in
  `build_openedge/runtime.rs`). A compile with `-c` is one unit alone and never fails so.
- **Class cache.** As measured on the AVM, a session keeps each class it has bound (r-code or
  source) by class name while the PROPATH of the requests stays the same and the bound file
  does not change. A request with another PROPATH empties it. So stray r-code bound once
  stays bound, and the rebind before a later pass over a cycle is needed in the fake too.
- **Startup parameters.** `FAKE_AVM_ENV` gives the command-line parameters and the `.pf`
  contents separately; the fake reports `startup_parameters` with `.pf` contents expanded,
  `-pf <path>` and `(end .pf)` items, as the AVM does.
- **Hashing.** It answers `HASH` with the SHA-256 of the file, `dir` or `none`, and
  `HASHSET` with the digest of the list. `FAKE_AVM_DLC` points it at its own copy of the
  `$DLC` files, so two workers can see different copies.
- **Outputs.** The `.r` is text: a header, the compile name, one line `read <path> <hash>` for
  each file read, one line `say <text>` for each `FAKE-SAY <text>` in the unit and its
  includes, one line `uses <class>` for each class that the unit and its includes name, and
  `compiled-at <nanoseconds>`. The last line makes outputs of one
  source differ between compiles, like real r-code. A test can read a `.r` and check that it
  matches the current files: that is the wrong-hit oracle. It writes the text XREF or the
  `XREF-XML` as requested, and the text XREF for the `private` mode. As measured, neither
  names class r-code that the compile bound (the `.r` keeps its `read` line). The text XREF has a
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
| `FAKE-FAIL-TWICE` in a source | Like `FAKE-FAIL-ONCE`, for the first two compiles; tests a cycle whose last pass is not settled. |
| `FAKE-FAIL-SILENT` in a source | `DONE fail` with no `MSG` line; tests that a failed unit still carries a reason. |
| `FAKE-NO-OUTPUT` in a source | `DONE ok` but no `.r` in the stage; tests the `no output` failure. |
| `FAKE-WARN` in a source | `MSG` warning, `DONE ok`. |
| `FAKE-WARN-NOPOS` in a source | `MSG` warning 214 with an empty file and row and column 0, `DONE ok`; tests a warning that has no position. |
| `FAKE_AVM_DROP_MSG=1` | Send one `MSG` line fewer than the `messages` count of `DONE`; tests that the count is a protocol check. |
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
| `FAKE-SAY <text>` in a source | The program prints `SAYS <text>` when the runtime runs it (a `say` line of its r-code). |
| `FAKE-RESOLVE-OTHER <name> <path>` in a source | Resolve include `<name>` to `<path>` instead of the first PROPATH match, as a divergent AVM would. |

- **Runtime.** `oxabl_fake_avm -p <program>` runs a program, and `oxabl_fake_avm -new
  <class>` makes an object of a class, as a project's runtime does. The PROPATH comes from
  the `PROPATH` variable (entries split at `,` and `:`); an unset or empty variable is an
  error, so a test cannot forget it. The fake finds the file by the measured rule: entry by
  entry, `<stem>.r` before the source in each entry, no date check; a full path runs the
  `.r` beside it first, whatever the PROPATH holds. It prints `RUN <name> <file> rcode` or
  `RUN <name> <file> compiled-on-the-fly`, then `SAYS <text>` for each `say` line and the
  `read <path> <hash>` lines (from the r-code, or from a compile in memory for a source).
  Then it loads each class in a `uses` line by the same rule and prints the same lines after
  `NEW <class> <file> ...`. A name it cannot find is `** "<name>" was not found. (293)` and
  exit code 1; a `.r` that is not fake r-code is error 2888. So a test asks the runtime
  which file runs and from which source hash, not the disk.
- **A project's own compile.** `oxabl_fake_avm -c <name> [-save-into <dir>]` writes the
  r-code of `<name>` as `COMPILE <name> SAVE [INTO <dir>]` does, with the PROPATH from the
  `PROPATH` variable: beside the source it found, or below `<dir>`. Tests use it to make
  r-code of another build tool.
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
- `a/x.p` and `b/x.p` with `root_relative_include_paths = ["."]`: both compile as `x.p`,
  their r-code and XREF go to `a/` and `b/` of the output directories, and a second copy of
  the workspace fetches both from the local cache, each the r-code of its own source.
- `-trig <dir>` in the startup parameters: the keys are the ones without it.
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
- Two classes in a cycle: both compile on one worker, in two passes (four compiles).
- Cycles (`build_class_cycles.rs`), `pkg.A` and `pkg.B` read each other, `src/user.p` reads
  `pkg.A`, and stray r-code lies beside both sources:
  - every unit is keyed; `pkg/A.cls` and `pkg/B.cls` compile twice, `user.p` once; the fake
    r-code of `A`, `B` and `user` records `build/r/pkg/<other>.r` and no `src/...r`; a
    second build compiles nothing;
  - `FAKE-FAIL-ONCE` in `A`: `A` compiles three times and both classes are up to date;
  - `FAKE-FAIL-TWICE` in `A`: `A` compiles three times and is `ok`, but the last pass is not
    settled: `A` and `B` rebuild at the next plan with the note `a class of its cycle did
    not compile in the pass before, ...`, and only `user.p` is written to the cache.
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
  cache write, recorded without a key), and the exit code is 1.
- `--events` and `--report`: the shapes, and one `unit-finished` per compiled unit.
- Per-unit PROPATH classes. `root_relative_include_paths = ["."]`, `directories = ["a",
  "b"]`, units `a/x.p` and `b/y.p` each doing `NEW pkg.Foo()`, class units `a/pkg/Foo.cls`
  and `b/pkg/Foo.cls`, and a unit `a/plain.p` that reaches no class. Exact outcome:
  - the build has two PROPATH ids;
  - both class units have the compile name `pkg/Foo.cls` and collide on `pkg/Foo.r`, so
    `a/pkg/Foo.cls` (the smaller path) compiles and `b/pkg/Foo.cls` is skipped with
    `output-collision: a/pkg/Foo.cls`;
  - `b/y.p` is unkeyable with `per-unit-propath-classes: pkg.Foo (a/pkg/Foo.cls,
    b/pkg/Foo.cls)`: it binds `a/pkg/Foo.cls` from `$OUT`, not the class its analysis
    found; `a/x.p`, `a/pkg/Foo.cls` and `a/plain.p` are keyed;
  - one warning gives the number of units without a key and names the edge;
  - `a/pkg/Foo.cls`, `a/x.p`, `b/y.p` and `a/plain.p` compile; the exit code is 0, because a
    skipped unit is not a failure;
  - nothing is written to any cache for `b/y.p`; the three keyed units are written.

  The same workspace with `root_relative_include_paths = []` and `include_paths = ["a"]`
  (one PROPATH id, `b/` removed) has no warning and keyable users of `pkg.Foo`.
- Per-unit PROPATH classes, incremental. After the build above, touch only `b/y.p`: the
  plan still reports `per-unit-propath-classes` for it, and `a/x.p` keeps its key.
- Keys under per-unit PROPATHs (`build_class_keys.rs`), `directories = ["a", "b", "lib"]`,
  `root_relative_include_paths = ["."]`, `include_paths = ["lib"]`, users `a/x.p` and
  `b/y.p` of the class unit `lib/pkg/Foo.cls`, which includes `{v.i}`:
  - every unit is keyed (`released` 3), the build has no warning, and the fake r-code of
    each user records `build/r/pkg/Foo.r` with its current hash and no class source; a
    second build compiles 0; a second workspace on the same local cache fetches all 4 units,
    compiles 0, and its users record the fetched `pkg/Foo.r`;
  - stray `lib/pkg/Foo.r` and `a/pkg/Foo.r`: still keyed, users bind `$OUT`;
  - `a/v.i` shadowing `lib/v.i`: still keyed, and `pkg/Foo.r` records `lib/v.i`;
  - `a/pkg/Foo.cls` added after a cached build: `lib/pkg/Foo.cls` is skipped, `b/y.p` is
    unkeyable with `per-unit-propath-classes: pkg.Foo (a/pkg/Foo.cls, lib/pkg/Foo.cls)`,
    one warning names it, nothing is fetched, and only `a/x.p` and `a/pkg/Foo.cls` are
    written to the cache;
  - `FAKE-FAIL` in `lib/pkg/Foo.cls`, with stray `lib/pkg/Foo.r` that the users bind once
    `$OUT` holds no r-code of it: both users compile `ok` and tainted, only
    `a/plain.p` is cached, and the next plan rebuilds both users with
    `a unit it reads failed in this build`;
  - a class unit `lib/pkg/A.cls` that uses `ext/pkg/B.cls` (not a unit) with `{w.i}`, where
    `A`'s PROPATH finds `lib/w.i` and the analysis `ext/w.i`: `A` and its user `a/x.p`
    both have `class-propath: w.i in ext/pkg/B.cls finds lib/w.i, analysed ext/w.i`.
- Class conflict with a library (one PROPATH id). `include_paths = ["src", "$LIB:acme"]`
  (`acme` is a `.pl`, digest `sha256:`), `case_insensitive_lookup = false`,
  `[build.external_packages] "Acme.Util." = ["$LIB:acme"]`, a class unit
  `src/Acme/Util/X.cls`, and `src/q.p` doing `NEW acme.util.x()`, which the analysis does not
  resolve (the case differs) and K3 accepts. `src/q.p` is unkeyable with
  `class-conflict: Acme.Util.X`, the warning names `src/Acme/Util/X.cls` and `$LIB:acme`, and
  nothing is cached for `src/q.p`. Without `src/Acme/Util/X.cls`, `src/q.p` is keyable.
- Class conflict, incremental: after that build, touch only `src/q.p`: still
  `class-conflict`, although `src/Acme/Util/X.cls` is up to date.
- Uniform resolution (`build_class_resolution.rs`), the `class_resolution` of
  `--plan --probe --json`. A shared layout: `directories = ["a", "b", "lib"]`,
  `root_relative_include_paths = ["."]`, `include_paths = ["lib"]`, `a/x.p` and `b/y.p` use
  `pkg.Foo`, a class unit `lib/pkg/Foo.cls`; a source layout is the same without `lib` in
  `directories`, so `lib/pkg/Foo.cls` is not a unit. Exact outcomes:
  - shared layout as is: three PROPATH ids, the three units are uniform; `a/plain.p` has no
    `class_resolution`;
  - the per-unit layout above (`a/pkg/Foo.cls` and `b/pkg/Foo.cls`): `a/x.p` and
    `a/pkg/Foo.cls` are uniform (`$OUT` holds `a/pkg/Foo.cls`), `b/y.p` fails `U1` with both
    files named;
  - `{v.i}` in the class, with `lib/v.i` and `a/v.i`: in the source layout the include fails
    `U1` for both users and also `U3` for `a/x.p`; in the shared layout all three units are
    uniform, because users read the class unit's r-code from `$OUT`;
  - stray `a/pkg/Foo.r` and `lib/pkg/Foo.r` beside the class unit: uniform;
  - source layout with `a/pkg/Foo.r`: `a/x.p` fails `rcode`, `b/y.p` fails `U1`;
  - `b/V.i` and `a/Pkg/foo.r` (case differs only): uniform, the match is exact in case.
- Class r-code (`build_class_rcode.rs`), K11:
  - a unit whose include defines a variable `wsobj` and uses `wsobj:setmsg(...)` in an
    `&IF 0` branch of an internal procedure, beside a class `w.wsobj` with stray
    `w/wsobj.r`: keyed (the name is the variable); with the same branch before the
    definition too: `class-rcode: w/wsobj.r`;
  - one PROPATH id, `include_paths = ["src", "lib"]`, `src/x.p` uses `pkg.Bar`, a class
    that is not a unit, with `lib/pkg/Bar.r` beside `lib/pkg/Bar.cls`: `src/x.p` is
    unkeyable with `class-rcode: lib/pkg/Bar.r`, `src/plain.p` is keyed, and the build
    warns once, naming the file. The fake r-code of `x.r` records `lib/pkg/Bar.r`; an edit
    of `lib/pkg/Bar.r` compiles `x.p` again and the new hash is recorded;
  - a unit that oxabl cannot read in that layout has no observed key:
    `observed-class: lib/pkg/Bar.cls: class-rcode: lib/pkg/Bar.r`;
  - `src/pkg/Bar.r` beside a class unit `src/pkg/Bar.cls`: both units keyed and up to date
    after a build, and `x.r` records `build/r/pkg/Bar.r`, never `src/pkg/Bar.r`;
  - two directories with their own PROPATHs: `lib/pkg/Bar.r` gives both users
    `class-rcode`; `a/pkg/Bar.r` gives it to `a/x.p` only.
- Package root (`build_package_root.rs`), `root_relative_include_paths = ["."]`:
  - `a/pkg/Foo.cls` includes `{x.i}`, with `a/x.i` and `a/pkg/x.i`: `a/x.p` and
    `a/pkg/Foo.cls` are keyed, and `pkg/Foo.r` records `a/x.i`. An edit of `a/x.i` makes
    both rebuild, the build compiles both, and the r-code records the new hash. An edit of
    `a/pkg/x.i` leaves both up to date;
  - the same layout with a second directory `b`: two PROPATH ids, and `a/x.p` and
    `a/pkg/Foo.cls` are uniform (no `U3`);
  - `a/x.p` uses `pkg.Bar` from `lib/pkg/Bar.cls` (`include_paths = ["lib"]`, not a source
    directory), which includes `{v.i}`: with `lib/v.i` only, `a/x.p` is keyed; with
    `a/v.i` too, `a/x.p` is unkeyable with `class-propath: v.i in lib/pkg/Bar.cls finds
    a/v.i, analysed lib/v.i`, the compile records `a/v.i`, and an edit of `a/v.i` makes it
    rebuild and compile again;
  - in that layout, a unit that oxabl cannot read gets no observed key:
    `observed-class: lib/pkg/Bar.cls: class-propath: ...`;
  - a class whose name is not written out, whose only package root `a` oxabl derives
    (`root_relative_include_paths = ["rf"]`, `include_paths = ["a"]`): `computed-name`.
- A class unit `src/x/Foo.cls` that declares `pkg.Foo`: skipped with `package-mismatch`.
  A class unit `a/pkg/Foo.cls` with no `.` in `root_relative_include_paths` and no `a` in
  `include_paths`: skipped with `package-root-not-on-propath: a`.
- An output directory with a foreign file and no marker: exit code 2. A foreign `.r` put
  into a managed output directory: deleted by the next build, and no unit reads it.
- Version control (`build_gitignore.rs`), in a git repository: with `output = ".oxabl/r"`
  a build leaves nothing under `.oxabl/` in `git status --untracked-files=all`, writes
  `.oxabl/.gitignore` with `*`, and writes no file in the root, in `.oxabl/r` or in
  `.oxabl/build`. An existing `.oxabl/.gitignore` is unchanged. With `output = "build"` there
  is no `build/.gitignore` (and the build works), and `.oxabl/` still gets one. With a file of
  the user in `out/` next to `out/r`, there is no `out/.gitignore`.

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

### The runtime oracle

A test that asks what a project runs takes the runtime PROPATH from the environment the
program starts in, as a project configures it: the `PROPATH` variable, or a `.pf` or `.ini`
that the run command passes. `runtime.rs` sets the variable with `env` in front of the AVM
executable of `OXABL_OPENEDGE_COMMAND`. No program in such a test sets `PROPATH`, and no test
derives the runtime PROPATH from oxabl's output directory. A test names the layout of the
project (the source entries alone, or an output entry before each source entry) and shows
that the other layout fails it. `run_from_rcode` in `compile.rs` sets `PROPATH` to
`build/r`, so it is only for tests about the r-code itself.

The fake AVM's runtime follows the same rule, and the same oracle applies to it: the test
sets its `PROPATH` variable as the project's environment would. One table of cases,
`crates/oxabl/tests/common/runtime_cases.rs` (E1, E1b, E1c and E4: which entry wins, r-code
beside a newer source, each name of a program, a name with no r-code, a class), runs
against the fake in `build_runtime_fake.rs` (public CI) and against OpenEdge in
`build_openedge/runtime.rs`. When both pass, the fake encodes the measured rule, and a
fake-AVM test that asks the fake runtime what runs proves what a real runtime runs. A
change of the rule changes the table, and both runs must pass again.

| Fake-AVM test (`build_runtime_fake.rs`) | What it proves |
| --- | --- |
| `the_shared_case_table_holds_on_the_fake_runtime` | Every case of the shared table on the fake runtime. |
| `the_fake_runtime_runs_nothing_without_a_propath` | An unset or empty `PROPATH` is exit code 1, not a run on a default PROPATH. |
| `the_runtime_ignores_an_output_directory_it_does_not_search` | The incident on the fake: r-code of the project's own compile beside the sources, an edit of a program, an include and a class, `oxabl build` into `build/r`, and a runtime PROPATH of `src` alone: all three print the **old** text, and `hello.p` runs `src/hello.r`. Red with `build/r` first. |
| `an_edit_shows_in_a_runtime_that_searches_the_output_first` | The same project with the runtime PROPATH `build/r`, then `src`: the new text, `hello.p` runs `build/r/hello.r`, and that r-code records the current hash of `src/hello.p`. Red with `src` alone. |

With `[build].rcode = "beside-source"` the same oracle answers. Each test of
`build_beside_source.rs` asks the fake runtime, with the runtime PROPATH of the project:

| Fake-AVM test (`build_beside_source.rs`) | What it proves |
| --- | --- |
| `an_edit_runs_on_a_runtime_that_searches_only_the_sources` | The pair of the incident: the same project, r-code of the project's own tool beside the sources, runtime PROPATH `src` alone. After the edit and `oxabl build`, all three print the new text, `hello.p` runs `src/hello.r`, which records the current hash of `src/hello.p`, and there is no output directory. |
| `the_unit_propath_has_no_output_entry` | No PROPATH of the plan holds `$OUT`. |
| `output_and_beside_source_together_are_refused` | `[build].output` with `rcode = "beside-source"` is exit code 2 for `--plan` and a build, the message names both keys, and nothing is written. |
| `the_effective_configuration_shows_where_the_rcode_goes` | `--print-config` shows `build.rcode` with its value and source, `directory` by default. |
| `a_second_build_compiles_and_analyses_nothing` | After the first build writes r-code into `src`, the second build compiles nothing and makes no analysis again: a user of `pkg.greet` that a case-blind lookup found by listing `src` and `src/pkg` (the listing filter; the test checks that the store holds both listings), a `RUN hello.r.` (never searched) and a user of a class whose r-code is now beside it (the r-code beside a class unit is that class, present or not). |
| `a_linked_directory_is_one_place_for_rcode` | A directory linked into the sources gives one unit and one r-code file; the second build compiles nothing, and both spellings run it. |
| `units_collide_only_in_one_directory` | `x.p` and `x.w` in one directory collide (`x.w` is skipped), and the r-code of `x.p` survives the delete of the skipped unit's r-code over two builds; `one/y.p` and `two/y.p` each run their own. |
| `classes_of_one_name_in_two_roots_keep_their_own_outputs` | `a/pkg/Foo.cls` and `b/pkg/Foo.cls` under per-directory PROPATHs both compile, each with its own XREF (the output name is the unit path), and each package root runs its own class. |
| `a_skipped_unit_keeps_no_rcode` | The r-code of the project's own tool beside a unit that is `shadowed-by` another is deleted: the runtime compiles the current source. |
| `a_failed_unit_keeps_no_rcode` | A unit that fails loses the r-code of the project's own tool: the runtime never runs it. |
| `a_user_that_compiles_first_binds_no_old_class_rcode` | A user that the analysis does not see use a class compiles before it and binds the class source, not the r-code of the project's own tool: the build deletes the r-code of each class it builds before the first compile. |
| `a_cycle_with_incompatible_old_rcode_compiles_from_the_sources` | A cycle whose r-code of the project's own tool does not fit the new sources (the fake's error 12927) compiles in two passes and runs the new text of both classes. |
| `a_read_only_source_directory_stops_the_build` | A source directory that the build cannot write stops it with exit code 3 before any compile, naming the directory once. Fails, never passes, under a user that can write a read-only directory. |
| `rcode_a_compile_names_is_outside_the_closure` | V3 with no output directory: a compile that names r-code (here a class's r-code beside its source) fails the read check. |
| `a_unit_outside_the_root_is_skipped` | A unit of a source directory outside the root is skipped (`outside-root`) and gets no r-code beside it. |

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
| `the_trigger_directory_changes_no_rcode_and_no_key` (scratch) | A table with a create and a delete trigger: a unit that creates and deletes its rows, and a trigger procedure, give r-code equivalent to the build without `-trig` with the trigger directory and with a directory of broken triggers, and the same keys. Pins `-trig` in the drop list. |
| `the_raw_digest_on_a_served_database` (scratch) | `proutil idxactivate` changes the digest; on a served database the digest equals the single-user one and is the same for 1, 2 and 8 workers; the second build reuses the image; an SQL `CREATE TABLE` (`sqlexp` over TCP) changes the digest. |
| `startup_pf` | A parameter set only in a `.pf` is in the probe, and its canonical form equals the same parameter on the command line. |
| `param_file_hash` | Changing the bytes of a `-compileroptionsfile` file changes the environment identity. The AVM reads that file at its start and refuses a line that is not an option (error 18408), so the test changes a `#` comment line. |
| `library_hash` | `HASH` gives the SHA-256 of a `.pl`; a changed `.pl` changes the keys of its users. |
| `dlc_default_entries` | A unit that uses an `OpenEdge.*` class compiles with no `$DLC` entry in `include_paths`. |
| `compile_fixture` | The fixture builds; outputs exist; a planted error gives a `MSG` with row and column. |
| `class_order` | The cycle and the hierarchy compile; users of a changed class compile after it. |
| `warning_without_position` | A `DO TRANSACTION` inside another one gives warning 214, which has no row and no column: the unit is `ok`, and the report holds the warning with row `0`, column `0` and the unit as its file; the text report prints it with no position. On OpenEdge 12.8 the compiler gives `GET-NUMBER` 214, `GET-MESSAGE-TYPE` 2, `GET-FILE-NAME` the source as found on the PROPATH, and `?` for `GET-ROW` and `GET-COLUMN`. |
| `the_worker_refuses_a_line_with_an_unknown_value` | The shipped worker program, with one request changed to build a line that holds the unknown value, answers `ERROR` and quits, and sends no line. |
| `portable_rcode` | The fixture copied to two roots gives equivalent r-code for every unit. |
| `xref_does_not_change_rcode` | With and without `XREF`, equivalent r-code. |
| `two_workspaces_share_compiled_units_through_the_local_cache` | A first workspace compiles, a second one at another path compiles nothing, fetches every unit from the local cache, and the fetched r-code runs (passed on OpenEdge 12.8 with slice 7). |
| `run_target_not_read` | Editing a `RUN` target leaves the caller's r-code equivalent. |
| `per_unit_propath_classes` | The slice-6 per-unit PROPATH layout on a real AVM: `a/pkg/Foo.cls` compiles, `b/pkg/Foo.cls` is skipped with `output-collision`, `b/y.p` is unkeyable with `per-unit-propath-classes: pkg.Foo (a/pkg/Foo.cls, b/pkg/Foo.cls)` and is not cached, `a/x.p`, `a/pkg/Foo.cls` and `a/plain.p` are keyed and cached (3 stored), the exit code is 0. Recorded: both users run with `a`'s `Foo`. |
| `per_unit_propath_uniform_classes` | Users `a/x.p` and `b/y.p` of the class unit `lib/pkg/Foo.cls` (three PROPATH ids), whose include `{v.i}` gives the method `Place`; `a/v.i` declares another method and `a/pkg/Foo.r` is not r-code, both first on `a`'s PROPATH. Every unit is keyed, there is no warning, both programs print `lib`; a second build compiles 0; a second workspace on the same local cache fetches 3 and compiles 0, and both programs print `lib` again. |
| `procedures_of_one_name_in_two_directories` | `a/x.p` and `b/x.p` with `root_relative_include_paths = ["."]`: both compile, their r-code is `a/x.r` and `b/x.r`, and each runs from its own r-code. |
| `per_unit_propath_class_binding` | The same layout, where `b/pkg/Foo.cls` has one more method and `b/y.p` calls it: `b/y.p` fails with error 12927 (`Could not locate element 'Extra' in class 'pkg.Foo'`). The cause is the `$OUT` r-code of `a`'s `Foo`, which comes first on the PROPATH (`out_rcode_beats_a_later_source_in_a_fresh_session`), not the session (`session_binds_a_class_again_after_a_propath_change`). Pins the [follow-up](compile-key.md#follow-up-classes-under-per-unit-propaths). |
| `class_includes_are_searched_where_the_compiler_searches` | `root_relative_include_paths = ["."]`, `include_paths = ["lib"]`. `a/pkg/Foo.cls` includes `{x.i}`; `a/x.i` and `a/pkg/x.i` declare different methods, and the user calls the one in `a/x.i`. `lib/pkg/Bar.cls` (not a unit) includes `{v.i}`; `a/v.i` and `lib/v.i` declare different methods, and `a/z.p` calls the one in `a/v.i`. All compile, so the compiler reads the include of a class unit from its package root, and the include of a class that is not a unit from the user's PROPATH. `a/z.p` is unkeyable with `class-propath`. After an edit of `a/x.i`, `a/x.p` and `a/pkg/Foo.cls` rebuild and the user prints the new text. |
| `one_propath_gives_keyable_class_users` | With one PROPATH id, class users are keyable and stored. |
| `class_source_alone_binds` | Control of the class-binding tests: an entry with `pkg/Foo.cls` and no r-code binds the source. |
| `stale_rcode_beside_source_binds_without_out` | No earlier entry holds r-code of `pkg.Foo`; one entry holds stale `pkg/Foo.r` beside the current `pkg/Foo.cls`. The compiler binds the r-code, with the source newer or older than the r-code and with `MULTI-COMPILE` on and off. Stray class r-code in a source entry is bound unless an earlier entry holds the class. |
| `out_rcode_beats_stale_rcode_and_source` | `$OUT` holds r-code of `pkg.Foo`; a later entry holds stale `pkg/Foo.r` and the current `pkg/Foo.cls`. The `$OUT` r-code binds. |
| `first_entry_with_the_class_decides` | An earlier entry with only stale `pkg/Foo.r` beats a later entry with the source; an earlier entry with only the source beats a later entry with stale r-code. |
| `class_file_name_case_is_exact` | `Pkg/foo.cls`, `pkg/Foo.cls` referenced as `PKG.FOO`, `Pkg/foo.r` and `pkg/Foo.r` referenced as `PKG.FOO` are all not found on Linux (error 5638). A case-only match never binds. |
| `out_rcode_beats_a_later_source_in_a_fresh_session` | `$OUT` holds r-code of `pkg.Foo`; a later entry holds a `pkg/Foo.cls` with another member. In a fresh session the `$OUT` r-code binds (the user of the later member fails with error 12927), with `MULTI-COMPILE` on and off and with the source newer or older. |
| `class_unit_users_read_only_its_rcode` | `pkg.Foo` includes `x.i`; its r-code in `$OUT` was built with one `x.i`, and a later entry holds the source with another. A plain user and a class that inherits `pkg.Foo` bind the r-code; the `FileID` log names `out/pkg/Foo.r` and neither `Foo.cls` nor `x.i`. Without the r-code, both read the source. |
| `fileid_log_names_compiler_reads` | The `FileID` log of `LOG-MANAGER` names the unit, a definitions-only include, a class source with its includes and a class r-code. A class that an earlier compile of the session bound is not named again, with `MULTI-COMPILE` on and off. |
| `build_class_unit_binds_out_not_stray_rcode` | `oxabl build` with a class unit and stray `pkg/Foo.r` in its source directory: users bind the r-code the build wrote to `$OUT`. |
| `build_class_that_is_not_a_unit_binds_stray_rcode` | `oxabl build` with a class in an include-only directory next to stray `pkg/Foo.r`: users bind the stray r-code, and `$OUT` has no r-code of the class. |
| `a_cycle_binds_the_first_rcode_it_finds_for_the_other_class` | `pkg.A` and `pkg.B` call a member only the current other has. Stale r-code of the other beside its source (`cold`) or in the output directory (`warm`): both compiles fail with 12927 in pass 1, in pass 2 of the same session and in a fresh session. With none (`clean`) both compile. |
| `a_second_pass_after_a_propath_reset_binds_only_this_builds_rcode` | Under the r-code oracle (masks from four clean compiles of the cycle): with r-code first from a compatible `B` (one more method), beside the source or in `$OUT`, pass 1 gives an `A.r` that is not equivalent to a clean one. Pass 2 is equivalent in a fresh session and after `PROPATH = ""` and back; in the same session, also after `MULTI-COMPILE` off and on, it is not when the bound r-code was stray. Pins the [passes over a cycle](compile-key.md#passes-over-a-cycle). |
| `build_cycle_with_stray_rcode_gives_clean_rcode` | `oxabl build` over the cycle and a user, with current `A.r` and compatible `B.r` beside the sources: every unit is keyed and `ok`, and the r-code is equivalent to a clean build's. With one pass, `A.r` is not. |
| `a_variable_hides_a_class_of_the_same_name` | A class `wsobj` with a static method and a variable `wsobj AS Progress.Lang.Object`: with the variable defined before the use, also inside an internal procedure, `wsobj:ToString()` compiles and `wsobj:OnlyStatic()` fails (12927); with no variable it is the other way round (14633). |
| `session_binds_a_class_again_after_a_propath_change` | A session that compiled a user of `pkg.Foo` through one PROPATH binds the class again when the PROPATH changes, for source and r-code, with `MULTI-COMPILE` on and off. The same PROPATH keeps the first class. |
| `xref_xml_classes` | A unit using a class from source, from `$OUT` r-code and from a `.pl`: in all three cases the `XREF-XML` `Source` elements list only the unit itself, no class file ([cache.md](cache.md#what-v3-covers)). Goes red if a later OpenEdge lists one. |
| `mixed_provenance` | Classes `t.B`, `t.A INHERITS t.B` and a program doing `NEW t.A()`: compile all three, compile `B` again, run the program with the old `A.r` and the new `B.r`: the result is correct. A regression guard for the per-unit key (measured to pass on OpenEdge 12.8). |
| `xref_xml_resolution` | The `XREF-XML` `Source` elements name each include that holds a reference, as the compiler resolved it (`src/defs.i`, `src/ttrows.i`); an include with only `&GLOBAL-DEFINE`s (`consts.i`) is **not** listed; every fixture unit passes V3 and is stored. Goes red if a later OpenEdge lists the definitions-only include. |
| `xref_xml_observed_closure` | The `XREF-XML` of `main.p` has an `INCLUDE` reference for `defs.i`, `ttrows.i` and the definitions-only `consts.i`. The [observed closure](compile-key.md#the-observed-closure) is sound only while this holds. Its class part does not rely on the `XREF-XML`: a class used only as a type gives no reference (measured on 12.8). |
| `xref_text_classes` | The same three class cases with `xref_format = "text"`: the second column names only the unit. |
| `xref_text_resolution` | `main.p` with a requested text `XREF`: the second column names `src/main.p`, `src/defs.i`, `src/ttrows.i` and equals the `XREF-XML` `Source` list; every fixture unit passes V3 and is stored, with a requested and with the private text `XREF`. |
| `xref_text_observed_closure` | The text `XREF` of `main.p` has an `INCLUDE` row for `defs.i`, `ttrows.i` and the definitions-only `consts.i`. |
| `xref_text_unusual_names` | A skipped `&IF` include, a unit with a space in its name and an include path of 200 characters: the text `XREF` reads back and the units are stored. |
| `xref_readers` | The text reader finds every include of the fixture (its `INCLUDE` rows give the name as written, not as resolved); the XML reader finds the includes that hold a reference. |
| `the_shared_case_table_holds_on_openedge` | The shared case table of `common/runtime_cases.rs` on the AVM: `-p` with the name or the full path, and `DYNAMIC-NEW` for a class. A name with no `.r` and no source prints error 293. |
| `the_runtime_searches_entry_by_entry` | E1: `a/x.p` and, in a later entry, `b/x.r` of another source. `RUN x.p` on `a,b` runs `a/x.p`; on `b,a` it runs `b/x.r`. The runtime searches entry by entry, r-code before source in each entry. |
| `rcode_beside_a_newer_source_runs` | E1b: `a/x.r` of an old source beside a newer, edited `a/x.p`: `RUN x.p` runs the old r-code. The runtime does not compare times. |
| `every_name_of_a_program_runs_the_rcode_beside_it` | E1c: with the E1b setup, `RUN x`, `RUN x.r`, `RUN` of the full path of `a/x.p`, `-p <full path>` and `-p x.p` all run `a/x.r`. With `o/x.r` in an entry before `a`, `-p x.p` runs `o/x.r`, and the full path of `a/x.p` (`-p` and `RUN VALUE`) still runs `a/x.r`. `RUN x` with no `x.r` is error 293. |
| `compile_save_replaces_the_rcode_file` | E3: `COMPILE ... SAVE` and `SAVE INTO` over an existing `.r` give a new inode; a hard link to the old file keeps the old bytes. |
| `class_rcode_beside_a_newer_source_runs` | E4: `a/pkg/Foo.r` of an older source beside `a/pkg/Foo.cls`: `DYNAMIC-NEW "pkg.Foo"` and a user compiled on the fly both get the old class. |
| `the_compiler_opens_no_procedure_rcode` | E7: `u.p` runs `v.p`; random bytes at `u.r` and `v.r` beside the sources. The compile of `u.p` by name opens `u.r` (the `FileID` log), not `v.r`, and its r-code is equivalent to a compile without them under the r-code oracle. |
| `stale_rcode_beside_a_procedure_changes_no_compile` | E9: `COMPILE x.p SAVE` by name with stale r-code of another version at `a/x.r`: the compile opens it, succeeds, and the r-code is equivalent to a compile without it; the program runs the new text. |
| `class_users_bind_the_rcode_compiled_beside_the_class` | E8: compile PROPATH of the source entries alone. `a/pkg/Foo.cls` compiled `SAVE INTO a` gives `a/pkg/Foo.r`; users in `a/` (PROPATH `a`) and `b/` (PROPATH `b,a`) bind it (the `FileID` log names `a/pkg/Foo.r`, not `Foo.cls`). With stale `a/pkg/Foo.r` first, the users bind the stale class until the class compiles over it, then the new one. |
| `a_cycle_in_place_after_two_passes` | E8: the cycle `pkg.A` <-> `pkg.B` compiled in place (`SAVE INTO` the source entry, PROPATH that entry alone), two passes in fresh sessions. With compatible stale r-code beside both, the r-code after pass 2 is equivalent to a clean build's (one pass is not). With stale r-code that lacks the members the other calls, all four compiles fail with 12927 and the stale files stay. |
| `the_runtime_ignores_an_output_directory_it_does_not_search` | The incident, pinned: r-code of the project's own `COMPILE ... SAVE` beside the sources, an edit of a program, an include and a class, `oxabl build` into `build/r`, and a runtime PROPATH of the source directory alone: all three print the **old** text. Red with `build/r` first. |
| `an_edit_of_a_program_shows_in_the_runtime` | The same project with the runtime PROPATH `build/r`, then `src` (one output entry before each source entry): the program, the include and the class print the new text. Red with `src` alone. |
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
