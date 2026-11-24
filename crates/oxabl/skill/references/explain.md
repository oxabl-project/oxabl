# oxabl explain

Read this when you need to know what a program does to the database before you
change it, or to judge the impact of a table or index change.

```sh
oxabl explain src/update.p
oxabl explain src/update.p:14 --json
oxabl explain src/update.p -I src/inc --schema db/schema
```

- One file per run. `FILE:LINE` (one-based, a line of the file as written)
  selects the innermost unit at that line; without it every unit is shown.
- Preprocessing is always on. Lines in the output are **expanded** lines
  (after includes), so they can differ from the file's own line numbers.
- `-I` adds include paths before `oxabl.toml`'s; `--schema` replaces its schema.
  Without a schema, tables are unknown (`?:name`) and no index is chosen.
- `RUN` targets found on the include paths are followed: their effects show as
  `transitive` with `via`.
- Exit codes: `0` report, `2` bad line or unreadable file, `3` preprocessing
  failed, `4` internal panic, `6` JSON serialization failure.

## Text output

From a sample `update.p` that includes `defs.i`, runs `audit.p` and has a
`bumpBalance` procedure:

```text
src/update.p (expanded lines)
unit main <main> @1
  direct: read Customer share-lock(default) once @5 index=CustNum (primary; tie=none; whole=true; equality=0; range=false)
  direct: read Customer exclusive-lock once @9 index=CustNum (unique-equality; tie=none; whole=false; equality=1; range=false)
  direct: update Customer n/a once @11
  summary: complete
  transitive: read Customer exclusive-lock once
  transitive: read Customer share-lock(default) once
  transitive: update Customer n/a once
unit procedure bumpBalance @13
  direct: read Customer exclusive-lock once @15 index=CustNum (unique-equality; tie=none; whole=false; equality=1; range=false)
  direct: update Customer n/a once @16
  summary: complete
  transitive: read Customer no-lock once via /work/sample/src/audit.p::main
  transitive: read Customer exclusive-lock once
  transitive: update Customer n/a once
```

Each line is `<direct|transitive>: <operation> <table> <lock> <shape>
[@line] [index=NAME (reason)] [via ROUTINE]`.

- `direct`: what this unit's own statements do. `transitive`: the unit plus
  everything it calls, merged per table, operation and lock.
- `summary: partial` means the transitive list may miss effects; the
  `unknown:` lines under it say why.

## Values

| Field | Values |
|---|---|
| unit kind | `main`, `procedure`, `function`, `method`, `constructor`, `destructor`, `property-get`, `property-set`, `trigger`, `db-trigger` |
| operation | `read`, `create`, `update`, `delete` |
| table | a database table; `tt:NAME` a temp-table (direct only); `?:NAME` (direct) or `?NAME` (transitive) a table the schema does not have, or any table with no schema |
| lock | `no-lock`, `share-lock`, `exclusive-lock`, `share-lock(default)` (no lock phrase), `n/a` (writes, temp-tables), `unknown` / `unknown(...)` |
| shape | `once`, `per-row` (inside one record loop), `nested(N)` (N loops deep). A `FOR EACH`'s own read is `once`; a `FIND` inside it is `per-row`. |
| index reason | `use-index`, `contains`, `key-value`, `unique-equality`, `equality`, `range`, `sort`, `primary`, then `tie=`, `whole=`, `equality=`, `range=`; or `rowid`, `table-scan`, `unknown:<why>` |
| partial reason | `unit:<why>` (own effects unknown: `dynamic`, `skipped-statement`, `unknown-table`, `no-schema`, `unexpanded-include`, `recovered-parse`, `too-complex`), `call:<why>` (a call target unknown: `dynamic`, `handle`, `super`, `event`, `method`, `unexpanded`, `not-found`, `unusable`, `ambiguous`, `no-routine`), `bound` (an analysis limit was reached) |

Index choices are estimates of what the compiler picks.

## JSON (`--json`, version 1)

```text
{ "version": 1, "file": "src/update.p", "coordinates": "expanded-lines",
  "units": [ { "kind": "procedure", "name": "bumpBalance", "line": 13,
    "effects": [ { "table": "Customer", "operation": "read", "lock": "exclusive-lock",
                   "shape": "once", "line": 15, "index": "CustNum",
                   "reason": "unique-equality; tie=none; whole=false; equality=1; range=false",
                   "via": null } ],
    "summary": { "complete": false, "partial": ["call:not-found"], "recursive": false,
                 "effects": [ { "table": "Customer", "operation": "read", "lock": "exclusive-lock",
                                "shape": "once", "line": null, "index": null,
                                "reason": null, "via": null } ] } } ] }
```

`name` is `null` for the main block. Summary effects have no `line`, `index`
or `reason`; `via` names the first routine called on the way
(`<path>::<routine>`). The shape may change before oxabl 1.0: read `version`.

Docs: https://docs.oxabl.org/toolchain/commands/explain/
