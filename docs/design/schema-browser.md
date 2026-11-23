# Schema browser

The schema browser shows the `.df` files that `[workspace.schema]` names: the
databases, their tables, fields, indexes and sequences. It needs no database
connection and no OpenEdge installation. `oxabl lsp` answers two requests, and
the VS Code extension shows the answer in the **ABL Schema** view.

## The tree

`oxabl_pipeline::schema_view` makes the tree. It finds the `oxabl.toml` above
the workspace anchor, expands `[workspace.schema] files` as the analysis does
(a directory gives its `.df` files in sorted order), and reads each file with
the configured source encoding. It reads at each request, so the answer always
agrees with the disk. A file that the list names more than once (as itself and
through its directory, or by two spellings) is loaded once, at its last
place, with a warning for each extra name. The configuration does this for
every reader of the list (`schema_files` in `oxabl_pipeline`), so the
analysis, the build and the tree read the files in one order. The last place
gives the same tables and fields as a load at each place, which the analysis
did before: the last load of a file replaces what its earlier loads declared.

- Each `.df` file is one database. Its name is the file name without the
  extension. A file is read alone (`oxabl_pipeline::schema_database`), with the
  merge rules of the loader (`oxabl_schema::SchemaLoader::load_file_alone`, the
  code that `SchemaLoader::load_files` runs for each file). The analysis merges
  all files into one schema in which the last declaration of a table wins. The
  tree shows what each file declares, so a table that two files declare shows
  in both databases.
- A file that adds fields or indexes to a table that an earlier file declares
  shows that table with `declared: false`. Its location is the first `ADD
  FIELD` or `ADD INDEX` of the file that names it. The loader refuses a field or an index of a table that no earlier
  file declares (`SCHEMA0001`, an error), and the tree leaves it out too.
- In one file, a second declaration of a table, sequence (`SCHEMA0010`), field
  or index (`SCHEMA0011`) replaces the first, as the loader does, and gives a
  warning in `problems`. A field declared twice with two types has no type and
  gives the error `SCHEMA0012`. The loader's caps (`SCHEMA0031`) apply.
- The loader does not apply `UPDATE`, `CHANGE`, `DROP` and `RENAME`
  directives. Each one gives the warning `SCHEMA0020` at the directive, and
  the file is not complete: the tree shows the schema without the change.
- `complete` is false when the file cannot be read, when its parse or merge
  reported an error (for example a file that breaks off in a quoted string),
  when it has a directive that the loader does not apply, or when its path is
  not UTF-8 (the error `SCHEMA0021`: the answer writes the path with
  replacement characters, and an editor cannot open the file). The tree then has what the loader read, and
  `problems` says what went wrong.
- The tables are in file order. The tables of the built-in metaschema
  (`_file`, `_field`, ...) are not in the tree: no `.df` declares them, so
  they have no location, and they are the same in every database.

`oxabl_pipeline::schema_table` turns one `Table` of any schema into the node of
the tree: a table of one file, or a table of the merged schema of the analysis.
A hover over a table (#409, not built yet) can use the second.

Each node keeps the byte span of its name. The language server converts the
spans to ranges in the negotiated position encoding (`SchemaDatabase::place`).
The name of each table, field, index and sequence is on its `ADD` line, so the
range is on that line.

## Requests

`oxabl/schema` takes `{}` and answers:

```json
{
  "config": "/abs/oxabl.toml",
  "reason": null,
  "warnings": [],
  "databases": [{
    "name": "sales", "path": "/abs/db/sales.df", "uri": "file:///abs/db/sales.df",
    "complete": true, "problems": [],
    "tables": [{
      "name": "Customer", "declared": true, "label": "Client", "dumpName": "cust",
      "hidden": false, "frozen": false, "range": { "start": {...}, "end": {...} },
      "fields": [{ "name": "CustNum", "dataType": "integer", "format": ">>>>9",
                   "mandatory": true, "caseSensitive": false, "range": {...} }],
      "indexes": [{ "name": "CustNum", "unique": true, "primary": true, "word": false,
                    "inactive": false, "fields": [{ "name": "CustNum", "ascending": true,
                    "abbreviated": false }], "range": {...} }]
    }],
    "sequences": [{ "name": "NextCust", "initial": 1, "increment": 1,
                    "cycleOnLimit": false, "range": {...} }]
  }]
}
```

- An attribute that the `.df` does not set is not in the node: `label`,
  `description`, `dumpName`, `area`, `format`, `initial`, `extent`,
  `decimals`, `help`, `order`, a bound of a sequence (no bound is no limit).
  `dataType` is absent only for a field that the file declares twice with
  types that conflict (`SCHEMA0012`).
- `problems`: `severity`, `code`, `message` and the `range`, when the problem
  has a place in the file.
- `warnings`: problems of the configuration that leave the other files in
  the answer, such as a schema directory that holds no `.df` file or a file
  named twice.
- `directories`: the directory of each configured file entry and each
  configured schema directory, also one that holds no `.df` yet. A client
  watches them.
- With no database, `reason` says why, and the answer is not an error: no
  workspace yet, no `oxabl.toml` above the workspace, an `oxabl.toml` that
  cannot be used, or one whose `[workspace.schema] files` gives no `.df` file
  (with the warning of each directory that matched none).

`oxabl/schemaTable` takes `{ database, table }`: `database` is the `uri` of a
database in the answer above, `table` a table name (case-insensitive). It
answers `{ database: { name, path, uri, complete }, table }` with the table
node, or `null` when the database has no such table. A `database` that is not
a configured `.df` file is an `InvalidParams` error. It reads only that file,
and the earlier files only when that file adds to a table it does not declare.

Both requests run on a worker thread. A newer `oxabl/schema` request makes an
older one stop before its next file, and again before it builds the JSON, and
answer `ContentModified`, so a burst of saves does not stack full reads. A 12 MB `.df` with 2,000 tables and
80,000 fields gives an answer of about 24 MB in 2 to 4 seconds (release build),
most of it to send the JSON.

## VS Code

The view `oxabl.schema` (**ABL Schema**) is in the Explorer. It shows when the
extension is active and asks the server the first time it is visible, so a
window that never shows it reads no `.df`.

- Database, then table, then its fields and indexes. Sequences are in a
  **Sequences** node under the database, and the problems of a file are at
  the top of its database.
- **Filter** (`oxabl: Filter Schema`) filters as you type: a case-insensitive
  substring of a table's name, label, description or dump name keeps the whole
  table, and one of a field's name, label or description keeps that field. An
  ABL abbreviation of a table name is a prefix of the name, so the filter
  finds it too. The filter applies 200 ms after the last key, or at `Enter`. A filter that keeps 50 tables or fewer shows their fields at
  once. The view message says what the filter is, or that nothing matches.
  `Escape` puts the previous filter back; **Clear Filter** removes it.
- A click on a field, index, sequence or problem opens the `.df` with the
  cursor at the name on its `ADD` line. **Open in .df** in the context menu
  does the same for a table and a database.
- **Copy Name** and, on a field, **Copy table.field**.
- **Show Table Details** (inline on a table) opens a page with the table's
  fields and indexes from `oxabl/schemaTable`. Each name links to its line in
  the `.df`. The page has no script.
- A change, creation or deletion of a `.df` or an `oxabl.toml`, a restart of
  the server and **Refresh** ask again (after 300 ms, so a save that writes
  twice asks once). Each request is counted, and an answer that a newer request
  replaced is dropped. An open details page asks again too, and also drops an
  answer that a newer request replaced. A workspace file watcher sees only the
  workspace folders, so the view also watches each of the answer's
  `directories` that is outside them (compared with `path.relative`), also
  one that holds no `.df` yet, and the `oxabl.toml` of the answer when it is
  above them. A `.df` matches in any case of its extension
  (`*.{df,DF,Df,dF}`), as the loader takes it.
- The message above the tree shows the `warnings` of the answer, the reason
  for an empty schema, and what the filter keeps. A server without the request
  gives "This oxabl is too old for the schema view".

The logic without `vscode` (the answer types, the filter, the details page) is
in `src/schemaModel.ts` and has unit tests. `src/schemaView.ts` is the editor
layer.

## Tests

- `crates/oxabl/tests/schema_lsp.rs`: the real `oxabl lsp` over stdio. Two
  `.df` files with a table in both, a table and a field with one name,
  non-ASCII names (ranges in UTF-16 and UTF-8), a field added to a table of
  another file, a file that breaks off and a file that is not there. The
  answer for one table, a change on disk, and each reason for an empty schema.
  Also: `DROP`, `UPDATE` and `RENAME` directives; fields of a table that only
  a later file, or no file, declares; a field, an index and a type declared
  twice in one file; a file named three ways; a schema directory without a
  `.df` beside a database; a path that is not UTF-8; `oxabl/schemaTable` while
  another configured file is a named pipe that no one writes (it must not read
  it); a tree request that a newer one replaces, while it reads its first or
  its last file; a file named before and after its own directory (the tree
  shows the field it adds and go to definition finds it); an extended table that starts with an `ADD INDEX`; and
  the `directories` of the answer.
- `crates/oxabl/tests/schema_named_twice_cli.rs`: `oxabl check` over the same
  two workspaces gives no finding and exit 0, as before the dedupe.
  `OXABL_CHECK_BIN` runs the cases with another build, for a comparison.
- `clients/vscode/src/schemaModel.test.ts`: the filter (a field by name, label
  and description, a table by an abbreviation, a dump name and a label), the
  escaping of the details page, the view message with warnings, the message
  for a server that is too old, the directories outside the workspace, and
  the watchers (any case of `.df`, an `oxabl.toml` above the folder).
- The e2e scenario `schema` (`clients/vscode/test/e2e`): the view, the filter,
  a click that opens the `.df` on the `ADD FIELD` line, **Copy table.field**,
  the details page, a change of the `.df` on disk, a `.df` outside the
  workspace folder and its change, a `.DF` created in an empty schema
  directory outside the workspace, and the warning of a schema directory
  without a `.df`. The scenario `schema-config-above` opens a folder whose
  `oxabl.toml` is one directory above it, and an edit of that file shows.
- The fuzz target `navigate` also checks the tree of the fuzzed `.df`: every
  span is in the file and on character boundaries, the tree and the loader
  hold the same tables and fields (both ways, also when an earlier file
  declares the tables the file extends), every range is placed with the
  language server's `byte_to_position` and agrees with an independent count of
  lines and UTF-16 units, and the tree serializes.

## Known limits

- The detail page does not show the programs that read or write the table.
  The effects of `oxabl abl effects` need a workspace pass; a later change can
  add them.
- `UPDATE`, `CHANGE`, `RENAME` and `DROP` directives in a `.df` are not
  applied, as the loader does not apply them, so a delta file shows only its
  `ADD` directives. The file is marked incomplete with a warning at each one.
- The loader's caps count the tables of all files; the tree counts per file,
  so past 100,000 tables in all the tree can show tables that the analysis
  refused.
- The database name is a guess from the file name. A `.df` holds no logical
  database name.
