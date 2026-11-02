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
- **Derived hashes** use BLAKE3 in derive-key mode (`blake3::Hasher::new_derive_key`) with
  a context string. The context string gives domain separation and carries the spec
  version:

| Value | Context string |
| --- | --- |
| Compile key | `oxabl 2026-11 compile key v1` |
| Environment identity | `oxabl 2026-11 compile environment v1` |
| Schema identity | `oxabl 2026-11 schema image v1` |

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
   directory and made canonical (`.` with a unit in `src/ar` gives `src/ar`).
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

- `dir` for a directory. Its files enter the key through the closure, file by file.
- `sha256:<64 lower-case hex>` for a library (a `.pl` file, or any entry that the worker
  reports as a file): the SHA-256 of the library's bytes, which the worker computes and
  reports ([worker-protocol.md](worker-protocol.md#hashing-worker-side-files)). A library
  holds r-code that the compiler can read, so its bytes are an input. SHA-256 is used
  because ABL computes it natively (`MESSAGE-DIGEST`); BLAKE3 stays the hash for every file
  oxabl reads itself.
- `absent` for an entry that the worker reports as missing. An entry that the worker cannot
  hash makes the environment unknown for the units that use it (K6).

The **compile name** is the unit path relative to the first entry after `$OUT` that holds
it. For a class it is the package path with the extension (`acme/ar/Invoice.cls`). A unit
that no entry holds is skipped (`not-on-propath`). A unit whose compile name resolves, on
the unit PROPATH, to a different file first is skipped (`shadowed-by`).

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

The closure assumes that the compile resolves each class name to the file that the analysis
resolved under the unit PROPATH. A `MULTI-COMPILE` session keeps a class it has compiled
under its name, even after PROPATH changes. The worker pool therefore never lets one session
compile two units whose PROPATHs resolve one class name to different files
([worker-protocol.md](worker-protocol.md#class-resolution-groups)).

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

The worker emits a **schema image** of each connected database: a set of records, each a
list of strings ([worker-protocol.md](worker-protocol.md#schema-image)). Oxabl encodes each
record as `list(str, fields)`, sorts the encoded records by bytes, and hashes:

```text
schema_identity = derive_key("oxabl 2026-11 schema image v1",
                             u64(number of records) || encoded records in sorted order)
```

Oxabl sorts, so the order in which the worker emits records does not matter.

The image holds every user table (`_File._Tbl-Type = "T"`), and the fields, indexes, index
fields, table triggers and field triggers of those tables, and every sequence, plus one
database record. Each record carries every scalar field of its metaschema table **except**:

- fields of data type `BLOB` or `CLOB`;
- `_Last-change` (a time stamp that differs between two databases loaded from one `.df`);
- storage area numbers (`_ianum` and other area fields).

A field value is `STRING(value)`, `?` for the unknown value, and `HEX-ENCODE(value)` for a
`RAW` field. A `RECID` or `ROWID` field is **mapped to the row it points at**: its value is
the record type and the name fields of that row, joined with `/` (for example
`_prime-index=index/customer/custnum`). So a change of a table's primary index changes the
image. A pointer to a row that is not in the image keeps its raw value; that is sound (a
different value can only cause a miss), and the OpenEdge suite shows whether it costs
portability. Each array element is a separate field `name[i]=value`. The field names are in
ASCII lower case.

The image covers the CRC of each table and also what the CRC does not cover and the
compiler puts into r-code: labels, formats, initial values, validation expressions, help,
view-as phrases and compile-time security (`_Can-*`). So two databases loaded from one
`.df` give one identity (slice 4 proves it on OpenEdge), and any dictionary change that
the compiler can see changes it.

Rejected: the table CRCs alone (they do not cover labels, formats and validation); the
`.df` files of `[workspace.schema]` (they can differ from the database that the AVM
compiles against, and nothing would tell).

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
                            entry = str(path) hash(content(path))
    section("propath")      list(propath_entry, unit PROPATH in search order)
                            propath_entry = str(entry) str(digest)
    section("environment")  hash(environment_identity)
    section("options")      list(pair, compile options)
    section("outputs")      list(str, output kinds)
    section("worker")       hash(content(worker program))

compile_key = derive_key("oxabl 2026-11 compile key v1", material)
```

`unit_path` and each closure path are canonical paths. The unit path is in the key, because
the r-code names its source.

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
gives each reason.

| Rule | Condition | Reason text |
| --- | --- | --- |
| K1 | The unit, or an analysed node in its closure, has a degraded edge set. | `degraded: <path>: <reason>` |
| K2 | The unit, or an analysed node in its closure, has an unresolved include (any kind, any reason). | `unresolved-include: <name> in <path>` |
| K3 | ... has an unresolved class, unless its reason is *absent from workspace* or *external*, its name starts with a prefix of `[build.external_packages]`, and one of the library entries listed for that prefix is on the unit PROPATH (so the library's bytes are in the key). | `unresolved-class: <name> in <path>` |
| K4 | ... has a reference with a computed name in an include or class position (`{{&x}.i}`, a conditional include with `{&...}` in its name). | `computed-name: <name> in <path>` |
| K5 | A file in the closure, or the unit, is absent, cannot be read, or has no canonical path. | `outside-workspace: <path>`, `unreadable: <path>` |
| K6 | The environment was not established in this build (no probe, a failed probe, or a schema probe failure for any database). | `environment-unknown` |
| K7 | The unit is skipped (`not-on-propath`, `shadowed-by`, `output-collision`). | the skip reason |

`[build.external_packages]` maps a class name prefix to the library entries that supply
it. The default maps `OpenEdge.` and `Progress.` to `$DLC`, which means any library under
`$DLC` on the unit PROPATH; the DLC default entries are always there. A prefix whose
libraries are not on the unit PROPATH leaves the unit unkeyable: no prefix is trusted by
name alone. A workspace that ships its own classes under such a prefix has them as files,
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

Schema image of database `ledger`, two records:

```text
["db", "_db-coll-name=ICU-UCA", "_db-type=PROGRESS", "_db-xl-name=UTF-8"]
["table", "customer", "_can-read=*", "_crc=12345", "_dump-name=customer", "_prime-index=custnum"]
```

Environment:

```text
openedge     "OpenEdge Release 12.8.9", "12.8"
platform     "UNIX", "TTY", "yes", "64"
session      "UTF-8", "UTF-8", "ICU-UCA", "BASIC", "mdy", "1950", ".", ","
parameters   [("-cpinternal","UTF-8"), ("-cpstream","UTF-8"), ("-d","mdy"), ("-inp","32000")]
databases    [("ledger", "PROGRESS", ["dictdb"], <schema identity>)]
```

Unit `src/ar/post.p`, compile name `ar/post.p`, bytes `{inc/ledger.i}\nMESSAGE "post".\n`.
Closure: `src/inc/ledger.i`, bytes `DEFINE VARIABLE x AS INTEGER NO-UNDO.\n`. Unit PROPATH:
`$OUT` (`dir`), `src` (`dir`), `$DLC/tty/netlib/OpenEdge.Net.pl` (`sha256:` of the bytes
`library bytes`, which is
`8c45ce4e5a5cb0c03670ca09e153a24b1c7b7f94141c4da68f31ce8e91bc1bf8`). Default compile options. Outputs: `rcode`. Worker program bytes:
`/* oxabl worker */\n`.

| Value | Hex |
| --- | --- |
| schema identity | `5d563b8b7413121dc18d6061596536064071f7cce3024ee25178547e2adef0ed` |
| environment identity | `0c88b89888c58e11a288151cfe11997e8301e65b69305e50500a3f20fb743420` |
| content of the unit | `0396bf3a7c10884079571b007003baaba08e654f666a1cbc9b95f142cb6aea21` |
| content of the include | `0b547367c7b6f2702a1cedf8d6e7a6c4a646ba1e89229a91052712e69427b058` |
| content of the worker program | `c90f59b20862aa76cdd3a16915d2662583c01e49f381d7db0810e5d9bc397fb8` |
| key material length | 702 bytes |
| compile key | `fa30683ea4089459e871b5b3ca40b50ee4ed09aa8aa04f38ed5f863dfe4300a2` |

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
