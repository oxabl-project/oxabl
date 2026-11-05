---
title: "oxabl build: the compile key specification"
status: proposed
date: 2026-11-02
---

# The compile key specification

Part of [the oxabl build PRD](../build-tool.md).

A compile key is 32 bytes. It names one compile: one unit, with everything the AVM can read
or use when it compiles that unit. Two implementations that follow this document must
produce the same key for the same inputs. The [worked example](#worked-example) gives
exact values to check against.

## Properties

1. **Complete.** A change to any input that can change the r-code, the XREF, the debug
   list or the listing changes the key. Where oxabl cannot prove that an input is
   complete, the unit gets no key ([the fail-closed rule](#the-fail-closed-rule)).
2. **Portable.** The key does not contain the workspace root, the host, the user, the
   time, the branch or the output directory. Two checkouts of one commit at different
   paths on different machines get the same keys.
3. **Canonical.** Every input has one encoding. Field boundaries are explicit.
4. **Conservative first.** A dependency counts by its full bytes, and a database counts by
   its full schema image. [Signature-aware keys](#follow-up-signature-aware-keys) are a
   follow-up.

A key maps to *any valid output* of its inputs. r-code is not byte-deterministic: two
compiles of one source differ in a header timestamp and other fields. So a cache never
checks an entry by compiling again. The first accepted write wins, and trust in an entry
comes from who wrote it (the oxabl-cache design), not from a comparison.

## Hash function

- **Content hash** of a file: `BLAKE3(bytes)`, the standard unkeyed BLAKE3 hash of the raw
  file bytes, 32 bytes. No decoding, no line-ending change, no BOM removal: the compiler
  reads bytes. `b3sum` gives the same value.

  Exception: a file under `$DLC` is hashed **by the worker**, from the copy that the AVM
  compiles with, as `SHA-256(bytes)` (the worker computes SHA-256 natively). In a closure
  entry its algorithm is `sha256`; for every other file it is `blake3`. One worker hashes
  each `$DLC/...` closure file once per build, in the DLC hash phase between the closure
  step and the key step (`HASH`,
  [worker-protocol.md](worker-protocol.md#hashing-worker-side-files)). Oxabl also hashes the
  host mirror's copy with SHA-256; when the two differ, every unit with that file in its
  closure is unkeyable (K6, `dlc-mirror-mismatch: <path>`), because the analysis read
  other bytes than the compiler will. So a patch to an include inside the OpenEdge
  installation changes the key even when the first line of `$DLC/version` does not change.
- **Derived hashes** use BLAKE3 in derive-key mode (`blake3::Hasher::new_derive_key`) with
  a context string. The context string gives domain separation and carries the spec
  version:

| Value | Context string |
| --- | --- |
| Compile key | `oxabl 2026-11 compile key v1` |
| Environment identity | `oxabl 2026-11 compile environment v1` |
| Schema identity | `oxabl 2026-11 schema image v2` |

A change to this specification that changes any encoded byte uses new context strings
(`v2`). Old and new keys can then never be equal.

Rejected: SHA-256 (about 5 times slower than BLAKE3 on one core, and BLAKE3 also runs on
many cores); a plain hash with a prefix (derive-key mode gives the same separation by
construction).

## Encoding

All material is a byte string built from these primitives:

| Primitive | Bytes |
| --- | --- |
| `u64(x)` | 8 bytes, little-endian. |
| `bytes(b)` | `u64(length of b)` then `b`. |
| `str(s)` | `bytes(UTF-8 of s)`. No Unicode normalisation. A string that is not valid UTF-8 cannot be encoded; see K5. |
| `hash(h)` | The 32 raw bytes, with no length prefix. |
| `list(f, xs)` | `u64(number of xs)`, then `f(x)` for each `x` in the order the rule gives. |
| `section(name)` | `str(name)`. Each section of material starts with its name. |

Every variable-length field has a length prefix, and every list has a count, so no two
different inputs encode to the same bytes.

## Canonical paths

A canonical path is a UTF-8 string with `/` as the separator, no `.` or `..` component, no
empty component, no leading `./` and no trailing `/`. Case is the case on disk.

- A file or directory under the workspace root: its path relative to the root. The root
  itself is `.`.
- A file or directory under the host DLC source directory (`[workspace.sources].dlc`): `$DLC/`
  followed by its path relative to that directory. `$DLC` alone is the directory itself.
- The output directory, as a PROPATH entry only: `$OUT`.
- A procedure library outside the root and outside `$DLC`, named in `[build.libraries]`, as a
  PROPATH entry only: `$LIB:<name>`.

Any other path (another absolute directory, a path through a symbolic link that leaves the
root) has no canonical form. Oxabl resolves symbolic links lexically: a path is canonical
when its lexical form is under the root, and the file it names is read through the link.

## The unit PROPATH and the compile name

The **unit PROPATH** is the search path that the worker sets before it compiles the unit,
as a list of canonical paths, in order:

1. `$OUT`.
2. Each `[workspace.sources].root_relative_include_paths` entry, joined to the unit's
   **base directory** and made canonical (`.` with a procedure in `src/ar` gives `src/ar`).
   The base directory of a procedure (`.p`, `.w`, ...) is its own directory. The base
   directory of a class unit is its **package root**: its directory minus one component for
   each package segment of the class name it declares (`a/pkg/Foo.cls`, declaring
   `pkg.Foo`, has the package root `a`). A class unit whose directory does not end with its
   package path is skipped with `package-mismatch: <declared name>`, because the compiler
   would reject it.
3. Each `[workspace.sources].include_paths` entry, made canonical. An entry that is not
   under the root, not under `$DLC` and not a `$LIB:` name is a configuration error for
   `oxabl build` (exit code 2), because it has no portable form.
4. The **DLC default entries**: each entry of the worker's own startup PROPATH (reported in
   the handshake) that is under the worker's `$DLC`, in its order, as `$DLC/...`. These are
   the libraries of the installation (`OpenEdge.Core.pl`, `netlib/OpenEdge.Net.pl`, ...),
   which the AVM would search if oxabl did not set PROPATH. Startup entries outside `$DLC`
   are dropped, with one warning per build that names them.

Duplicates after the first are removed.

Each entry is a directory or a procedure library. In the key, an entry carries a **digest**:

- `dir` for a directory under the root or under `$DLC`. Its source files enter the key
  through the closure, file by file. A `$LIB:` entry that the worker reports as a directory
  (a folder of r-code) has no digest that covers its bytes: every unit with it on its
  PROPATH is unkeyable (K6, `unhashable-library: $LIB:<name>`).
- `sha256:<64 lower-case hex>` for a library (a `.pl` file, or any entry that the worker
  reports as a file): the SHA-256 of the library's bytes, which the worker computes and
  reports ([worker-protocol.md](worker-protocol.md#hashing-worker-side-files)). A library
  holds r-code that the compiler can read, so its bytes are an input. SHA-256 is used
  because ABL computes it natively (`MESSAGE-DIGEST`); BLAKE3 stays the hash for every file
  oxabl reads itself.
- `absent` for an entry that the worker reports as missing. An entry that the worker cannot
  hash makes the environment unknown for the units that use it (K6).

The **compile name** is the unit path relative to the first entry after `$OUT` that holds
it. For a class unit it must be the package path with the extension (`pkg/Foo.cls`): the
first entry that holds the class unit must be its package root, or an `include_paths`
entry that is the same directory. Otherwise the unit is skipped with
`package-root-not-on-propath: <package root>` (for example, a class `a/pkg/Foo.cls` with no
`.` in `root_relative_include_paths` and no `a` in `include_paths`). A unit that no entry
holds is skipped (`not-on-propath`).

A unit whose compile name resolves, on the unit PROPATH, to a different file first is
skipped (`shadowed-by`).

Two units whose r-code would land on one path collide: `x.p` and `x.w` in one directory, or
`x.p` and `x.cls`. The compile stems are compared ignoring ASCII case, because r-code is
often deployed to case-insensitive systems. In each colliding group, the unit with the
smallest unit path (by bytes) compiles, and every other unit is skipped with
`output-collision: <path of the unit that compiles>`. So two keys never install over one
final path.

The worker compiles `COMPILE VALUE(<compile name>)` with the unit PROPATH as worker-side
paths (`$OUT` is the worker-side output directory, a root-relative entry is relative to the
worker's working directory, which is the worker root). Every entry the worker sees is
relative or under the DLC, so the paths that the r-code embeds are relative, and the
r-code does not depend on where the root is. The OpenEdge suite proves this
(`portable_rcode`, [testing.md](testing.md#opt-in-openedge-suite)). If that test fails,
the spec moves to `v2` and adds the worker root to the key.

## The closure

The closure is the set of files whose bytes the compile of a unit can read.

Let `E(n)` be the stored edges of an analysed node `n` ([store.md](store.md#blob-encoding)).
The **compile-time kinds** are: direct include, transitive include, class, conditional
include (resolved) and conditional class (resolved).

`closure(U)` is the smallest set `S` of canonical paths such that, for each analysed node
`n` in `{U}` and in `S`, every edge of `E(n)` whose kind is a compile-time kind has its
target in `S`. `U` itself is never in `S`.

- Include files are not analysed alone. The analysis of the unit (or of the class) that
  includes them reports their own includes and the classes they name as edges of that
  unit, so they are in `S` through the unit.
- Each class source in `S` is analysed as a node, so its includes, its super class, its
  interfaces and the classes it names are in `S`.
- Conditional includes and classes (a name in an `&IF` branch that oxabl did not take) are
  in `S` when they resolve. Oxabl's `&IF` evaluation can differ from the AVM's; this keeps
  the key a superset of what any branch reads.
- Not in `S`: `RUN` targets, shared producers and schema tables. The compiler does not open
  a `RUN` target or a shared producer. Schema enters the key through the environment.

The closure is computed iteratively with a visited set over the in-memory graph, so cycles
end.

The closure assumes that the compile resolves each class name to the file, and each
include inside a class to the file, that the analysis resolved. That holds when the build
has **one PROPATH id**: then every unit, and every class compiled in memory for it, searches
the same entries, and a class name resolves to one file. With more than one PROPATH id it
does not hold in general: the shared `$OUT` tree has one `.r` per class name, a
`MULTI-COMPILE` session keeps a class by name across PROPATH changes, and a class that is
not a unit is compiled in memory under each user's own PROPATH. So the key covers classes
only with one PROPATH id ([K9](#per-unit-propath-classes)), and one narrow conflict remains
to check even then ([K8](#class-conflicts)).

### Per-unit PROPATH classes

A **PROPATH id** is one distinct unit PROPATH (the list of entries with their digests). The
build has more than one PROPATH id when `root_relative_include_paths` is non-empty and gives
units in different directories different PROPATHs. Oxabl counts the PROPATH ids over the
**whole graph** (every unit, whatever the rebuild set).

With more than one PROPATH id, every unit whose closure reaches a class source (a class unit
or a class that is not a unit), and every class unit, is unkeyable under K9
(`per-unit-propath-classes`). These units still compile locally, as the AVM resolves them,
and are never fetched from or written to a cache. Units that reach no class keep their keys,
because includes resolve per unit and the closure follows each unit's own PROPATH. The build
prints one warning with the number of such units.

Real codebases checked use a single global PROPATH, so K9 costs nothing there. Keying
classes under per-unit PROPATHs is a [follow-up](#follow-up-classes-under-per-unit-propaths).

### Class conflicts

With one PROPATH id, a class name resolves to one file, with one exception: a class name that
K3 accepted as external (unresolved, but supplied by a hashed library) can also be the name
of a workspace class: a class unit, or the target of a class edge, that the analysis of
this unit did not reach (for example through a spelling that differs in case with
`case_insensitive_lookup = false`). The compiler can then read that class's `.r` from `$OUT`
in place of the library class that the key covers.

So, over the **whole graph**, a name that K3 accepted is **in conflict** when any class unit
or class edge target has the same name (ignoring ASCII case, as the AVM does). Every unit
whose closure holds such an accepted name is unkeyable under K8 (`class-conflict: <name>`).
The build prints one warning per name, for example:

```text
warning: class Acme.Util.X is accepted as external ($LIB:acme) but src/Acme/Util/X.cls
         also has that name; 3 units that use it are compiled without a key
```

The build does not stop; these units compile as the AVM resolves them and are never cached.

### Follow-up: classes under per-unit PROPATHs

Keying classes when units have different PROPATHs needs all of these, each proven on a real
AVM; every earlier attempt to design it found a new hole:

1. **The shared `$OUT` tree.** One class name has one `.r` there, whichever source produced
   it, so a unit whose PROPATH resolves the name to another source compiles against the
   wrong class. A fix needs either a proof that the compiler prefers the source on the
   unit's PROPATH over `$OUT`, or a per-unit view of `$OUT`, which conflicts with one
   deployable output tree.
2. **`MULTI-COMPILE` sessions** keep a class by name across PROPATH changes. Measured on
   OpenEdge 12.8.9 (`per_unit_propath_class_binding`): with `a/pkg/Foo.cls` and a
   `b/pkg/Foo.cls` that has one more method, `b/y.p` calling that method **fails to
   compile** with error 12927 (`Could not locate element 'Extra' in class 'pkg.Foo'`): the
   session bound it to `a`'s `Foo`, the class it compiled first. A user that calls no
   differing member silently binds to the other class. So under per-unit PROPATHs a valid
   program can fail locally, not only lose its key. Sessions must be separated by PROPATH id
   (a fresh worker session per id), or toggling `MULTI-COMPILE` must be proven to clear them.
3. **External names.** A K3-accepted name must not collide with a workspace class under any
   PROPATH id.
4. **Class units.** A class unit's package root must be an entry of every user's PROPATH, and
   its own compile and its users' must agree on its includes.
5. **Classes that are not units** are compiled in memory under each user's PROPATH, so
   their includes and class references must be analysed per reaching PROPATH id (edges
   stored per id), not once on the class's own PROPATH.
6. **V3 coverage of classes.** `xref_xml_classes` shows that `XREF-XML` names no class file
   at all ([cache.md](cache.md#what-v3-covers)), so another source of what the compiler
   opened inside each class is needed before a divergence can be caught.

Until all six exist, K9 holds.

## The observed closure

Some units have no key only because oxabl cannot read their closure from their source: an
`XCODE`-encrypted source, or a source that includes an encrypted include. The analysis of
such a unit is degraded (K1), or finds include names that do not resolve (K2) or that it
cannot compute (K4). The compiler reads these files without a problem, and its `XREF-XML`
names the includes it read. Such a unit gets a key from its **observed closure**: the files
that the compiler read at the last successful compile of the unit, and every class source
that the compile could read.

### Which units qualify

A unit qualifies when each reason that it has no key is one of these, found in the
analysis of the unit itself (not of a class source in its closure):

- K1 `degraded: <unit>: <reason>`;
- K2 `unresolved-include: <name> in <unit>`;
- K4 `computed-name: <name> in <unit>` for an include.

Any other reason keeps the unit unkeyable: K3, K4 for a class name or a class statement,
and K5 to K9. A class unit never qualifies: its closure is an input of each user of the
class, so it must come from the analysis. A unit whose include the compiler cannot find
either fails to compile, so it records no observed closure.

### Classes: the class part

The `XREF-XML` cannot tell which classes a compile used. It names no class file
([cache.md](cache.md#what-v3-covers)). A class that is used only as a type gives no reference
at all. Measured on OpenEdge 12.8, these compile clean and give no class reference:
`DEFINE VARIABLE o AS pkg.Leaf` (only `STRING "o"`), `USING pkg.*.` with
`DEFINE VARIABLE o AS Leaf`, `TYPE-OF(x, pkg.Leaf)`, `DEFINE INPUT PARAMETER ip AS
pkg.Leaf`, `FUNCTION f RETURNS pkg.Leaf` (only `FUNCTION f`), and a variable of an
interface type. Only some uses give a reference: `NEW`, `INVOKE`, `CAST` and `ACCESS`. So no
list of reference types can show that a compile used no class.

So an observed closure holds every class that the compile could read. That is the **class
part** of the unit PROPATH:

- the **listing**: every class source (`.cls`) below the workspace entries of the unit
  PROPATH (the entries under the root), and every class unit of the workspace, whose r-code
  is in `$OUT`. The listing is in the stored closure, so a class that is added or removed
  changes the observed closure;
- for each listed class, the class itself and its closure as the analysis found it
  ([The closure](#the-closure)): its includes and the classes it reads. The bytes of a class do
  not cover an include that the class reads.

Each listed class must pass the rules of the key as if it were the unit (K1 to K9, with the
unit PROPATH), and must not hold a name that K8 finds in conflict. Otherwise no unit on that
PROPATH gets a key from an observed closure: `observed-class: <class>: <reason>`. A listed
class that oxabl did not analyse (a `.cls` outside `[workspace.sources].directories` that no
analysis reaches) gives `observed-class-unanalysed: <class>`, and a directory that cannot be
listed gives `observed-classes-unlisted: <why>`.

Classes under `$DLC` entries and in `$LIB:` libraries are not listed. They have the trust
that a parsed closure gives them, and no more: the digests of the PROPATH entries, the
`$DLC` version in the environment, and `[build.external_packages]`.

### What a compile records

After a successful compile of a unit that qualifies, the build reads the unit's
`XREF-XML`: the private one, or the one the user asked for. With `xref_format = "text"` no
compile is observed, and such a unit compiles at each build. The build records nothing,
and the unit keeps no key, when one of these steps fails:

1. **Each include, resolved on the host.** Each `INCLUDE` reference gives an include name.
   OpenEdge writes the text of the include, with its arguments, and sometimes in quotes
   (`gl/global-input.i &PASS`, `"se/decrypt.i  &company = v-company"`). The name is the
   first word, without the quotes. The build resolves the name on the unit PROPATH on the
   host, as the compiler does: the first entry that holds the file. `$OUT` is the r-code
   directory, and a `$LIB:` entry is passed over, because the key holds its bytes. The file
   goes into the closure, and each path searched before it goes into the **absent set**.
   A name that resolves to nothing, that leaves its entry, or that is absolute with no
   canonical form, records nothing. This step covers an include that holds only
   preprocessor definitions, which has no `Source` element.
2. **The compiler agrees.** Each `Source` element must map to a canonical path, as V3 maps
   it. It must be the unit, a file that step 1 found, or a listed class. R-code in `$OUT`
   and a member of a library pass, as in V3. Otherwise the host and the compiler resolve
   differently.

The store keeps the closure, the absent set and the listing of the class part, as the plan
found it ([store.md](store.md#columns)). The refresh tracks their files as it tracks the
files that an analysis read.

### The observed-closure key

At the next plan, a unit that qualifies and has a stored observed closure gets a key: the
material of [the key](#the-key), with the observed files and the class part as the closure,
each file by content (a `$DLC/...` file by the worker's SHA-256, as in a parsed closure). The
section is named `observed-closure` in place of `closure`, so an observed-closure key never
equals a key from the parsed closure. The absent set and the listing are not in the
material: they are checked. The unit has no key when:

- a file of the closure is absent now (`observed-missing: <path>`), cannot be read, or is a
  `$DLC/...` file whose worker copy differs from the host mirror
  (`observed-unreadable: <path>`);
- a path of the absent set holds something now (`observed-shadowed: <path>`): an include can
  resolve to it first;
- the listing now differs from the stored one (`observed-classes-changed: +<class>` or
  `-<class>`);
- the class part has a reason (see above).

These reasons follow the other reasons of the unit. The unit compiles, and the compile
records a new observed closure.

**Why this is sound.** A compile is a function of the unit, the files it reads and the
environment. The compiler decides which file to read next only from the bytes it read
already, from the PROPATH and from the environment, and the key holds all of them. When the
unit, each include it read and each class it could read are the same, and no new file can
shadow an include, it reads the same files again and makes equivalent r-code. ccache's
manifest mode uses the same argument.

### Order of the compiles

The compile of a unit that qualifies can read class r-code from `$OUT`, which is first on
its PROPATH. So it starts only after every class unit of the rebuild set has finished in this
build. Otherwise it could read the `.r` of the last build and be cached under a key that
holds the new class source. The scheduler adds an edge from each such unit to each class unit
of the pass. When the edges close a cycle, the units are in one job, and in a job the units
that qualify compile last.

### The key after the compile

So that the next build compiles nothing, the build also keys the unit right after a compile
that recorded a new observed closure, from the bytes on disk. It does so only when each
file is the one the compiler read: the refresh of this build hashed the same bytes before
the compile, or the file did not change (its `ctime` and `mtime`) since two seconds before
the build started. Each path of the absent set must still be absent, the classes listed
again after the compile must be the ones the plan listed, and a `$DLC/...` file gives no key
here. Otherwise the result has no key: the next plan keys the unit from the stored closure,
and the unit compiles once more under that key.

### Verification and the cache

When a unit compiles under an observed-closure key, the build reads its `XREF-XML` again.

- **The same closure** (the same files, absent set and listing): the key stands. V1, V2 and
  V3 apply as for any keyed unit, with the observed files and the class part as the closure,
  and the outputs are written to the local and the remote cache.
- **Another closure**: the key does not describe this compile. The build stores the new
  closure, writes nothing to a cache, and records the key made after the compile, or none.

A compile with no key before it is never written to a cache.

### Limits

- A machine with no store (a fresh CI runner) has no observed closure, so it compiles such a
  unit once. The remote cache serves such a unit only under a key that a machine with a store
  wrote.
- Any change to any workspace class on the PROPATH, or to what it reads, compiles such a
  unit again, and one workspace class without a key leaves such units without a key.
- With more than one PROPATH id, each class unit is unkeyable under K9, so a unit that
  qualifies gets no key when the workspace has a class unit.
- The rules trust the compiler's `XREF-XML` to give an `INCLUDE` reference for each include,
  the definitions-only ones too. The opt-in OpenEdge test `xref_xml_observed_closure`
  ([testing.md](testing.md#tests)) pins it.

## Environment identity

The environment identity is a hash of what the AVM session brings to the compile. It comes
from the worker's handshake ([worker-protocol.md](worker-protocol.md#handshake)), at
each build. It never comes from configuration alone.

Material, in this order:

```text
section("openedge")    str(dlc_version) str(proversion)
section("platform")    str(opsys) str(window_system) str(batch_mode) str(process_architecture)
section("session")     str(cpinternal) str(cpstream) str(cpcoll) str(cpcase)
                       str(date_format) str(year_offset)
                       str(numeric_decimal_point) str(numeric_separator)
section("parameters")  list(param, parameters)        param = str(name) str(value)
section("databases")   list(database, databases)      database = str(logical_name) str(dbtype)
                                                                 list(str, aliases)
                                                                 hash(schema_identity)
```

| Field | Source in the worker | Canonical form |
| --- | --- | --- |
| `dlc_version` | The first line of `$DLC/version` | Trailing white space removed. |
| `proversion` | `PROVERSION` | As reported. |
| `opsys`, `window_system` | `OPSYS`, `SESSION:WINDOW-SYSTEM` | As reported. |
| `batch_mode` | `SESSION:BATCH-MODE` | `yes` or `no`. |
| `process_architecture` | `PROCESS-ARCHITECTURE` | Decimal. |
| `cpinternal` ... `cpcase` | `SESSION:CPINTERNAL`, `CPSTREAM`, `CPCOLL`, `CPCASE` | As reported. |
| `date_format`, `year_offset` | `SESSION:DATE-FORMAT`, `SESSION:YEAR-OFFSET` | As reported; offset in decimal. |
| `numeric_decimal_point`, `numeric_separator` | `SESSION:NUMERIC-DECIMAL-POINT`, `SESSION:NUMERIC-SEPARATOR` | As reported. |
| `parameters` | `SESSION:STARTUP-PARAMETERS` | See [Startup parameters](#startup-parameters). |
| `databases` | `NUM-DBS`, `LDBNAME(i)`, `DBTYPE(i)`, `NUM-ALIASES`, `ALIAS(j)` | In connection order. Logical names and aliases in ASCII lower case. Aliases of the database sorted by bytes. |
| `schema_identity` | The schema image of the database | See [Schema identity](#schema-identity). |

### Startup parameters

The worker reports `SESSION:STARTUP-PARAMETERS`, a comma-separated list. The AVM expands
each `.pf` file into it: `-pf /work/build.pf,-inp 77777,-tok 4321,(end .pf),-Mm 8192` for a
`.pf` that holds `-inp` and `-tok` (measured on OpenEdge 12.8). Oxabl parses it:

1. Split on `,`.
2. Remove each item that is exactly `(end .pf)`, and each item whose name is `-pf`. The
   parameters that a `.pf` held stay, so a parameter has one canonical form whether it came
   from a `.pf` or from the command line.
3. An item that does not start with `-` continues the value of the item before it (the comma
   is kept). This handles a value that holds a comma.
4. Split each item at the first space into a name and a value. An item with no space has
   the value `""`.
5. Remove every parameter whose name is in the drop list.
6. **File values.** For each remaining value that is not `""`, oxabl asks the worker to hash
   the path that the value names (relative to the worker root when relative). When the
   worker reports a file, the value becomes `sha256:<hex>` of its bytes, so
   `-compileroptionsfile`, `-ininame` and any other file-valued parameter count by content,
   not by path. When it reports a directory (`-assemblies`), the environment is unknown for
   every unit (K6, `unhashable-parameter: -assemblies`). When nothing exists at the path, the
   value stays as written. This rule needs no list of file-valued parameters, so a parameter
   that oxabl does not know is still keyed by content.
7. For a name that occurs more than once, keep the last one (the AVM uses the last one).
8. Sort by name, by bytes. Names and values keep their case.

The **drop list** holds only parameters that name a connection, a file of this process, a
log or a secret:

```text
-p -param -pf -b -q -T -db -ld -H -S -N -U -P -1 -ct -dt
-clientlog -logginglevel -logentrytypes -logthreshold -numlogfiles
-basekey -keyvalue
```

Every other parameter stays, with its value. Some of them do not change r-code (`-mmax`,
`-s`, `-inp`). Keeping them costs cache misses when two machines start the AVM differently.
It never gives a wrong hit. A parameter moves to the drop list only with an OpenEdge-suite
test that shows it does not change the r-code.

Dropped parameters are never stored, printed or sent. `-U`, `-P` and `-keyvalue` can hold
secrets.

### Schema identity

The worker builds a **schema image** of each connected database and hashes it on the worker
side ([worker-protocol.md](worker-protocol.md#schema-image)). The image is a list of
**entries**:

| Entry kind | Name | Records in the entry, in this order |
| --- | --- | --- |
| `db` | `""` | the one `_Db` record of the database |
| `table` | the table name | the `_File` record of the table; its `_Field` records by field name; its `_Index` records by index name, each followed by its `_Index-Field` records by `_Index-Seq`; its `_File-Trig` records by `_Event`; its `_Field-Trig` records by `_Field-Recid`, then `_Event` |
| `sequence` | the sequence name | the `_Sequence` record |

There is one `db` entry, one `table` entry per user table (`_File._Tbl-Type = "T"`), and one
`sequence` entry per sequence.

**Canonical entry text.** Each record is one line: its fields, each percent-encoded as in
the protocol framing ([worker-protocol.md](worker-protocol.md#framing)), joined by `TAB`,
ended by `LF`. The fields of a record are:

1. the record type (`db`, `table`, `field`, `index`, `index-field`, `table-trigger`,
   `field-trigger`, `sequence`);
2. its name fields: none for `db`; the table name for `table`; table and field for
   `field`; table and index for `index`; table, index, `_Index-Seq` and field name for
   `index-field`; table and event for `table-trigger`; table, field and event for
   `field-trigger`; the name for `sequence`;
3. one `name=value` pair for each field of the metaschema buffer, in buffer order
   (`BUFFER-FIELD(i)` for `i` from 1 to `NUM-FIELDS`), with the field name in ASCII lower
   case, except the excluded fields below. Each pair is one field of the line, encoded as
   a whole.

A value is `STRING(value)`; `HEX-ENCODE(value)` for a `RAW` field; `?` for the unknown value
**of every data type** (character, integer, date, logical and the others). An array field
is **one** pair `name[]=v1%00v2%00...`: each element is percent-encoded and the elements are
joined with `%00`, which cannot occur inside an encoded value. A `RECID` or `ROWID` field is
written as the row it points at: `table/<table>`, `field/<table>/<field>`,
`index/<table>/<index>` or `db` (for example `_prime-index=index/customer/custnum`). A
pointer to a row that is not in the image keeps its raw value; that is sound, because a
different value can only cause a miss.

**Excluded fields:**

- fields of data type `BLOB` or `CLOB`;
- `_last-change`, `_last-modified` and `_mod-sequence`: time stamps and counters that
  differ between two databases loaded from one `.df` (measured on OpenEdge 12.8);
- `_ianum` and every field with `area` in its name (storage areas).

The canonical text is ASCII, because the encoding replaces every other byte, so its bytes do
not depend on the worker's code page. The worker hashes it with
`MESSAGE-DIGEST("SHA-256", ...)` over a `LONGCHAR` and reports, per entry, the kind, the
name, the number of records and the lower-case hex SHA-256.

**Identity.** Oxabl encodes each entry and sorts the encoded entries by bytes:

```text
encode(entry)   = str(kind) str(name) u64(records) str(sha256 hex)
schema_identity = derive_key("oxabl 2026-11 schema image v2",
                             u64(number of entries) || encoded entries in sorted order)
```

The identity does not depend on the order in which entries arrive, nor on how the imaging is
split between workers ([worker-protocol.md](worker-protocol.md#schema-image)).

The image covers the CRC of each table and also what the CRC does not cover and the
compiler puts into r-code: labels, formats, initial values, validation expressions, help,
view-as phrases and compile-time security (`_Can-*`). The OpenEdge suite proves on scratch
databases that two databases loaded from one `.df` give one identity, and that each of these
`.df` edits changes it: field label, field format, add index, change primary index,
validation expression and message, field help, add field, add table, add sequence, drop
index, drop field.

**Every build images every database.** No cheaper test is trusted to skip imaging; see
[Database fingerprints](#database-fingerprints).

Rejected: the table CRCs alone (they do not cover labels, formats and validation); the
`.df` files of `[workspace.schema]` (they can differ from the database that the AVM
compiles against, and nothing would tell); sending every record to oxabl and hashing there
(version 1 of this spec: 6.6 million lines and 296 MB for one real database, 10 minutes per
probe, against about 50 s with hashing on the worker, and 6 s with the work split across 8
workers).

### Database fingerprints

The worker also reports a **fingerprint** per database: `_Db._Db-guid` and
`_DbStatus._DbStatus-CacheStamp` (`?` when unavailable). It is part of the environment
agreement: every worker must report the same fingerprints, which catches two workers that
reach different databases. It is **never** used to skip imaging, because it does not see
every change that changes r-code. Measured on scratch databases, one `.df` edit per probe
(OpenEdge 12.8.9; reproduced by `schema_edit_table` in the slice 10 suite). The stamp does
**not** move for a primary-index change, an added sequence or a dropped field, all of which
change r-code:

| Edit | Schema identity changed | Cache stamp changed |
| --- | --- | --- |
| field label | yes | yes |
| field format | yes | yes |
| add index | yes | yes |
| change primary index | yes | **no** |
| validation expression and message | yes | yes |
| field help | yes | yes |
| add field | yes | yes |
| add table | yes | yes |
| add sequence | yes | **no** |
| drop index | yes | yes |
| drop field | yes | **no** |

The stamp also has one-second resolution, and the GUID stayed the same for databases
created from one empty database, so neither identifies a database or its schema. A cheaper
change test that can be trusted to skip imaging is an open follow-up
([build-tool.md](../build-tool.md#follow-ups)).

## Compile options

`[build.compile]` lowers to exactly these pairs, all present, sorted by name:

| Name | Value |
| --- | --- |
| `attr-space` | `true` or `false` |
| `generate-md5` | `true` or `false` |
| `languages` | The configured languages joined with `:`, in configured order; `""` when none. |
| `min-size` | `true` or `false` |
| `options` | The `OPTIONS` entries in ASCII lower case, sorted, without duplicates, joined with `,`; `""` when none. |
| `stream-io` | `true` or `false` |
| `v6frame` | `true` or `false` |

Material: `list(pair, pairs)` with `pair = str(name) str(value)`.

## Outputs

The requested output kinds, sorted by bytes. `rcode` is always present. The others are
`debug-list`, `listing`, and one of `xref` or `xref-xml`.

The private `XREF-XML` that verification uses ([cache.md](cache.md#verification)) is not a
requested output and is not in the key. The OpenEdge suite proves that XREF does not change
the r-code (`xref_does_not_change_rcode`). If that test fails, verification compiles into
a separate private compile instead.

## Worker program

`hash(BLAKE3(bytes of the embedded worker program))`. A change to the worker program can
change how a unit compiles, so it changes every key.

## The key

```text
material =
    section("unit")         str(unit_path) str(compile_name) hash(content(unit))
    section("closure")      list(entry, closure sorted by path bytes)
                            entry = str(path) str(algorithm) hash(content(path))
                            (section("observed-closure") for the observed closure)
    section("propath")      list(propath_entry, unit PROPATH in search order)
                            propath_entry = str(entry) str(digest)
    section("environment")  hash(environment_identity)
    section("options")      list(pair, compile options)
    section("outputs")      list(str, output kinds)
    section("worker")       hash(content(worker program))

compile_key = derive_key("oxabl 2026-11 compile key v1", material)
```

`unit_path` and each closure path are canonical paths. The unit path is in the key, because
the r-code names its source. For a unit keyed by [the observed closure](#the-observed-closure),
the closure is the observed one and the section is named `observed-closure`.

### What the key leaves out, and why

| Left out | Why |
| --- | --- |
| The workspace root and the worker root | Workers compile with relative paths; `portable_rcode` proves the r-code does not depend on them. |
| The output directory location | Only the token `$OUT` is in the PROPATH. |
| The host, the user, the time | Not compile inputs. |
| The oxabl version | It changes keys only through the worker program, the spec version and the closure it finds. |
| File stats | Content decides. |
| `[workspace.sources].encoding` | The AVM decodes source with `-cpstream`, which the environment holds. |
| Branch and commit | Content addressing shares entries between branches. |

### Component digests

`results.components` keeps, for the planner's reasons only, the BLAKE3 hash of the encoded
bytes of each section above (seven hashes, in section order). The environment section is
the environment identity itself, which also keys the `environments` table of the store, so
the planner can name the fields that changed. Component digests are not part of the key
specification.

## The fail-closed rule

A unit is **unkeyable** when any of these is true. An unkeyable unit has no key. It is never
fetched from a cache and never written to a cache. It compiles on every build, and the plan
gives each reason. The one exception is a unit that has no key only because oxabl cannot
read its closure: [the observed closure](#the-observed-closure) can key it.

| Rule | Condition | Reason text |
| --- | --- | --- |
| K1 | The unit, or a class source in its closure, has a degraded edge set. | `degraded: <path>: <reason>` |
| K2 | The unit, or a class source in its closure, has an unresolved include (any kind, any reason). | `unresolved-include: <name> in <path>` |
| K3 | ... has an unresolved class, unless its reason is *absent from workspace* or *external*, its name starts with a prefix of `[build.external_packages]` (compared ignoring ASCII case), and one of the library entries listed for that prefix is on the unit PROPATH (so the library's bytes are in the key). | `unresolved-class: <name> in <path>` |
| K4 | ... has a reference with a computed name in an include or class position (`{{&x}.i}`, a conditional include with `{&...}` in its name). A class unit whose `CLASS` name is not written out is also K4 (`computed-name: class statement in <path>`, `conditional-name: class statement in <path>`), unless exactly one directory can be its package root. | `computed-name: <name> in <path>` |
| K5 | A file in the closure, or the unit, is absent, cannot be read, or has no canonical path. | `outside-workspace: <path>`, `unreadable: <path>` |
| K6 | The environment was not established in this build (no probe, a failed probe, or a schema imaging failure for any database). | `environment-unknown` |
| K7 | The unit is skipped (`not-on-propath`, `shadowed-by`, `output-collision`, `package-mismatch`, `package-root-not-on-propath`). | the skip reason |
| K8 | The unit's closure holds a class name that K3 accepted as external, and a class unit or class edge target in the whole graph has that name ([Class conflicts](#class-conflicts)). | `class-conflict: <name>` |
| K9 | The build has more than one PROPATH id, and the unit is a class unit or its closure reaches a class source ([Per-unit PROPATH classes](#per-unit-propath-classes)). | `per-unit-propath-classes` |

**An include is judged in its includer's context.** K1 to K4 read the analyses of the
unit and of the class sources in its closure only. A closure file reached only through
include edges contributes nothing of its own standalone analysis, even when it is also a
unit (a `.v` file is a root and is often included): a `.v` that degrades on its own because
it uses `{&table}` does not make its includer unkeyable, because the includer's analysis
expands it with the includer's definitions, and anything degraded there is a K1 of the
includer. K5 and K6 still check every closure file. A file that is both an include target and
a class source is always judged by its own analysis too (fail-closed).

**A class name that is not written out.** The compiler checks the `CLASS` name against
the compile name (the name given to `COMPILE`, without its extension, `/` as `.`) and
refuses a unit with error 12629 otherwise. This holds when the name is `{&n}`, is set in
an `&IF`, or comes from an include. Oxabl chooses the compile name, so for such a unit it
derives the class name from it. A unit whose real name differs fails to compile, and a
failed unit is never cached: the derivation cannot give a wrong cache hit. The package
root is the one directory that is the unit's directory or an ancestor of it and is on the
unit PROPATH built for it. With two or more such directories (`root_relative_include_paths`,
or two `include_paths` entries that hold the file), or when a name part has a `.`, no one
name is known and K4 stays. The derived name also serves the class-conflict check (K8).

`[build.external_packages]` maps a class name prefix to the library entries that supply
it. The default maps `OpenEdge.` and `Progress.` to `$DLC`, which means any `.pl` library
under `$DLC` on the unit PROPATH whose digest is `sha256:`; a `dir` entry such as `$DLC/tty`
does not count. A prefix is accepted only through an entry with a `sha256:` digest, so the
library's bytes are in the key. A prefix whose libraries are not on the unit PROPATH, or
have no `sha256:` digest, leaves the unit unkeyable: no prefix is trusted by name alone. A workspace that ships its own classes under such a prefix has them as files,
and then they are edges, not unresolved names.

Two more rules govern cache **writes** only. They do not remove the key:

- **Taint.** A unit whose closure holds a unit that failed in this build is not written to a
  cache. It is recorded locally with its key.
- **Verification.** A unit is written to a cache only after it passes verification
  ([cache.md](cache.md#verification)).

A degraded unit compiling on every build is a cost the plan shows. The fix is in the
workspace configuration (include paths, the DLC sources, the preprocessor values) or in
oxabl's analysis. A degraded unit is never made incremental by a guess.

## Worked example

These inputs are illustrative. They are not real OpenEdge output.

Schema image of database `ledger`, three entries. Entry texts (`\t` is `TAB`, `\n` is `LF`):

```text
db         ""          1 record    db\t_db-coll-name=ICU-UCA\t_db-type=PROGRESS\t_db-xl-name=UTF-8\n
table      customer    5 records   table\tcustomer\t_can-read=*\t_crc=12345\t_dump-name=customer\t_prime-index=index/customer/custnum\n
                                   field\tcustomer\tcustnum\t_data-type=integer\t_decimals=?\t_format=>>>>9\t_label=Cust Num\t_view-as=?\n
                                   field\tcustomer\tname\t_data-type=character\t_decimals=?\t_format=x(30)\t_label=Name\t_view-as=?\n
                                   index\tcustomer\tcustnum\t_active=yes\t_unique=yes\n
                                   index-field\tcustomer\tcustnum\t1\tcustnum\t_ascending=yes\n
sequence   next-cust   1 record    sequence\tnext-cust\t_cycle-ok=no\t_seq-incr=1\t_seq-init=1\n
```

| Entry | SHA-256 of the entry text |
| --- | --- |
| `db` | `481e13c6d939c677a3c1490550c757a7c14e2f239df9870a19d3ac2251ff3678` |
| `table customer` | `38c22afa820d544f4ffa4550cf322aee17d2e7a7539f96bf253d494f20216aaa` |
| `sequence next-cust` | `563c917df85ba47de60ec29c499935236d5d2cc3e440c4fc9f6e348357302295` |

Environment:

```text
openedge     "OpenEdge Release 12.8.9", "12.8"
platform     "UNIX", "TTY", "yes", "64"
session      "UTF-8", "UTF-8", "ICU-UCA", "BASIC", "mdy", "1950", ".", ","
parameters   [("-cpinternal","UTF-8"), ("-cpstream","UTF-8"), ("-d","mdy"), ("-inp","32000")]
databases    [("ledger", "PROGRESS", ["dictdb"], <schema identity>)]
```

Unit `src/ar/post.p`, compile name `ar/post.p`, bytes
`{inc/ledger.i}\n{adecomm/x.i}\nMESSAGE "post".\n`.

Closure, sorted by path bytes:

| Path | Algorithm | Bytes | Hash |
| --- | --- | --- | --- |
| `$DLC/src/adecomm/x.i` | `sha256` (worker) | `DEFINE VARIABLE y AS CHARACTER NO-UNDO.\n` | `63789d696f620f34d75a81ee06dbac5cc9f6be2bccee4052f067e021ffa1acdd` |
| `src/inc/ledger.i` | `blake3` | `DEFINE VARIABLE x AS INTEGER NO-UNDO.\n` | `0b547367c7b6f2702a1cedf8d6e7a6c4a646ba1e89229a91052712e69427b058` |

Unit PROPATH: `$OUT` (`dir`), `src` (`dir`), `$DLC/src` (`dir`),
`$DLC/tty/netlib/OpenEdge.Net.pl` (`sha256:` of the bytes `library bytes`, which is
`8c45ce4e5a5cb0c03670ca09e153a24b1c7b7f94141c4da68f31ce8e91bc1bf8`). Default compile
options. Outputs: `rcode`. Worker program bytes: `/* oxabl worker */\n`.

| Value | Hex |
| --- | --- |
| schema identity | `8a495ef6b5c1d99915758a8773441ab7adf1b6abe407bf19c1a4f097ac62a199` |
| environment identity | `88379412ea5b9c91d75b53fed5c30c3ed18048293664ed0ec0deff2ec441a079` |
| content of the unit | `46fc37de67aabc0750c364e9958c080394046a163ac10ffa9f2f482e811d3b9a` |
| content of the worker program | `c90f59b20862aa76cdd3a16915d2662583c01e49f381d7db0810e5d9bc397fb8` |
| key material length | 817 bytes |
| compile key | `1e7e5c8c6213fb125ce83de8bf24bd702a339b554d7a39794204687c8aba304a` |

Slice 5 adds `crates/oxabl_build/tests/key_vectors.json` with this example and one vector
per rule of the encoding (an empty closure, two databases, a parameter that appears twice,
a value with a comma). The test drives `oxabl build --plan --json` with the fake AVM and
compares every key.

## Follow-up: signature-aware keys

The downstream tool keys a class's dependents on the class **signature** (its name,
inheritance, and visible members), so a method body edit rebuilds only that class. Oxabl
will do the same, as a `v2` key, only with this proof:

1. **A signature extractor** that keeps everything the compiler reads from a class when it
   compiles a user of it: the header, `USING`, every non-private member and its full
   declaration, temp-table and dataset definitions of any access, members from includes,
   preprocessor text, and any text the parser could not structure. It errs towards keeping
   text.
2. **An r-code equivalence oracle.** A normaliser that removes the known non-deterministic
   fields from r-code, proven on the corpus: compiling one source twice gives equal
   normalised r-code for every unit.
3. **A mutation campaign.** For a sample of at least 1,000 class edits that leave the
   signature unchanged (body edits, private member edits, comment edits), recompile every
   dependent with a real AVM and show equal normalised r-code. Zero counterexamples.
4. **Order still holds.** Dependents compile against the class's new r-code, so the
   scheduler order does not change.

Until all four exist, the closure counts each class by its full bytes.
