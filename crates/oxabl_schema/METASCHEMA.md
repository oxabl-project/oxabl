# The built-in OpenEdge metaschema

Every OpenEdge database carries a dictionary of itself: `_file`, `_field`,
`_index`, `_index-field`, `_db`, `_sequence`, `_user` and their kin, plus the
virtual system tables (`_connect`, `_lock`, `_trans`, `_myconnection`, …) that
expose live server state. ABL reads them directly — dynamic field lookup,
generic maintenance screens, permission mapping, dictionary-driven UI all
start with a `FOR EACH _file`.

None of them appear in a `.df` dump, because a `.df` describes the
*application* schema rather than the dictionary that stores it. So a project
cannot supply them however carefully it configures `[workspace.schema]`. They
belong to oxabl, the same way built-in functions do.

## How it is modelled

`resources/metaschema.df` is a generated `.df` covering table names, field
names, data types, extents, mandatory flags and index shapes. It is parsed
once per process, behind a `LazyLock`, through the ordinary `.df` parser.

Reusing the `.df` format and the existing parser is deliberate. There is no
second data model to keep in step with `Table`/`Field`, a metaschema table is
indistinguishable from a user table once loaded, and the checked-in artifact
is legible in the domain's own notation rather than as a Rust literal.

The catalog is **borrowed, not copied**. `SchemaLoader` turns it on for any
load it performs, and a `Schema` with it enabled hands out `TableId`s whose
top bit says which arena they address — the schema's own, or the one shared
static catalog. Enabling it therefore costs nothing per table, and `len`,
`is_empty` and `tables` keep reporting the schema's own tables only.

That last part is load-bearing. `Schema::is_empty()` is read elsewhere as "no
user schema configured"; a catalog that made every schema look non-empty would
silently switch schema-dependent diagnostics on for projects that never
configured one. Because the catalog is borrowed rather than merged, it cannot
do that, and no gate is needed to prevent it.

Lookups check the schema's own tables first, so a `.df` that defines a table
named `_file` — pathological but legal — shadows the dictionary's outright,
with no diagnostic: that is not a conflict between two things the user wrote.
`SchemaLoader::load_files_without_metaschema` is the escape hatch for a caller
that wants only what the `.df` files said.

One asymmetry to know about: `tables()` does not enumerate catalog tables, so
a metaschema table can be the target of a schema dependency edge without
appearing in a table listing.

## Database qualifiers

`dictdb._file` resolves. `dictdb` is not special-cased: ABL lets any table be
qualified with the logical name of the database it lives in, so the resolver
strips a single leading qualifier and resolves the bare table name. The
qualifier itself is recorded as `UnresolvedReason::External` — oxabl models one
flat table namespace and has no notion of which databases are connected, so
there is nothing to check it against, and "we did not look" is the honest
answer.

In expression position the arm takes the chain only when the field is a real
field of the named table. A package-qualified type name (`acme.security.Auth`)
has the same three-part shape, and a package segment can collide with a table
name; without that check, such a reference would mint a buffer and make the
file declare a schema dependency on a table it never reads. The cost is that a
database-qualified reference to a field that does *not* exist falls back to
the older, noisier diagnostics — which only affects code that is already
wrong.

## Which release

The catalog is generated from OpenEdge 12.8 and is not version-keyed. The
metaschema grows across releases and almost never loses anything, so a recent
layout is a superset of an older one; the failure mode against an older
database is that oxabl knows a table the database does not have, which costs a
false *negative* on a reference that would fail at runtime anyway. That is much
cheaper than the false positive this replaces. If a version-keyed catalog is
ever wanted, the generator already takes the version as an argument.

## What is not in it

The dump takes every table with a negative `_File-Number`, which is exactly
the dictionary and the virtual system tables. The **auditing** tables
(`_aud-audit-data`, `_aud-audit-policy`, …) have positive file numbers and so
are not included — they look like metaschema to an author, but they are
created by enabling auditing on a database and do appear in a `.df`.

## Regenerating

Requires an OpenEdge installation whose licence permits running `_progres`;
there is no way to derive this data without one, which is why the result is
checked in.

```sh
DLC=/path/to/dlc scripts/gen-metaschema-df.sh
```

The script copies the shipped `empty.db` (which has no application schema) to
a scratch directory, runs `scripts/dump-metaschema.p` against it to dump every
table with a negative `_File-Number`, and feeds the result to
`scripts/gen-metaschema-df.py`, which writes `resources/metaschema.df`.

`crates/oxabl_schema/src/metaschema.rs` asserts the checked-in file parses
without diagnostics and carries the headline tables and their fields, so a
truncated or malformed regeneration fails CI rather than shipping.
