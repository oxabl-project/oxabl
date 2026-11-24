---
name: oxabl
description: Lint, format, analyse and compile Progress ABL (OpenEdge 4GL) code with the oxabl CLI. Use whenever you read, edit, review or refactor ABL source (.p, .w, .cls, .i, .v files) in an OpenEdge or Progress project; to lint or check it, format it, see its database effects (which tables a program reads, writes or locks), compile or build it with an OpenEdge AVM; or to set up oxabl.toml, include paths, a .df schema or a lint baseline.
metadata:
  oxabl-version: dev
---

# oxabl

`oxabl` checks, formats, explains and builds Progress ABL. Only `build` needs
OpenEdge. Run `oxabl <cmd> --help` when a flag is unclear: it is the reference.

## After you edit ABL

Run this loop on each file you changed, until `check` exits 0:

```sh
oxabl format src/orders/post.p
oxabl check src/orders/post.p --json
```

- `format` and `check` take **one** path: a file or a directory. Run them once
  per changed file. Do not pass a directory the task did not touch: `format`
  rewrites every `.p/.w/.cls/.v` under it.
- `format` only fixes layout (indentation, blank-line runs, comment placement,
  keyword case when the style asks). It never wraps long lines or moves code.
- A file that does not parse is left unchanged and `format` still exits 0
  (`file has parse errors; left unchanged`). Always run `check` after it.
- `check` exits `0` clean, `1` on any finding (even `hint`), on format drift or
  a file it could not analyse, `2` on a usage or `oxabl.toml` error.
- Read `diagnostics[]` in the JSON: `code`, `severity`, `message`, `help`
  (when present), `path`, `start.line`, `start.column`. `format.drifted` lists
  files `oxabl format` would change. `failures` lists files it could not
  analyse. `preproc` holds include problems; they do not change the exit code.
- Fix the code, not the gate: do not set a rule to `off` or add a baseline
  entry to make your own change pass.

Text output looks like this:

```text
src/update.p:2:17: warning[LINT0002]: unused variable `iUnused`
    2 | DEFINE VARIABLE iUnused AS INTEGER NO-UNDO.
src/update.p:4:10: warning[LK001]: the `FOR` phrase reads `Customer` with no lock phrase, so it takes SHARE-LOCK, the default
    4 | FOR EACH Customer WHERE Customer.Balance > iLimit:
  help: The record is changed later in its scope: add `EXCLUSIVE-LOCK`, the lock the change takes.
```

Rule codes by prefix: `PARSE` syntax, `PREPROC` preprocessor and includes,
`SEM` semantic, `SCHEMA` the `.df` files, `LINT` core rules (always on),
`TX` `LK` `DB` `HD` `NL` `CL` opt-in strict groups. Look a code up in
`references/rules.md`.

## Before you change a program or a table

See what the program does to the database: reads, creates, updates, deletes,
lock modes, loop shape, index choice, and what its `RUN` targets do.

```sh
oxabl explain src/orders/post.p
oxabl explain src/orders/post.p:120 --json
```

`:120` picks the innermost procedure, function or method at that line of the
file. Lines in the output are expanded lines (after includes). A unit marked
`partial` may miss effects; its reasons say why. Details:
`references/explain.md`.

To list the units a change to an include or class reaches (no OpenEdge, no
compile; needs an `oxabl.toml`):

```sh
oxabl build --plan --files src/inc/ledger.i
```

## Compile

`oxabl build` compiles in dependency order on OpenEdge workers. It needs
`[build.workers].command` and `[build].output` (or `rcode = "beside-source"`)
in `oxabl.toml`, and an OpenEdge install that command can start.

```sh
oxabl build --print-config
oxabl build --plan --since origin/main
oxabl build --report build-report.json
```

Exit codes: `0` compiled (or plan printed), `1` a unit failed to compile, `2`
usage or configuration error, `3` store, lock or workers unusable, `130`
cancelled. A build compiles every unit changed since its last result;
`--files` and `--since` only work with `--plan`. With no worker command it
exits 2 and says so. Then do not invent one: run `oxabl check` and
`oxabl build --plan`, and say the code was not compiled. Details, `--json`,
`--events`: `references/build.md`.

## Configuration

- `check`, `format` and `explain` use the nearest `oxabl.toml` at or above the
  path you pass; `build` the nearest above the current directory (or `--root`).
  Relative paths in it are relative to its directory.
- With no `oxabl.toml`, defaults apply: no include paths, no schema.
- `-I DIR` (repeatable) adds include directories before
  `[workspace.sources].include_paths`. `--schema PATH` (a `.df` or a directory
  of them) replaces `[workspace.schema].files`.
- `oxabl.toml` is strict: an unknown key is a config error (exit 2), and the
  error lists the valid keys. `oxabl schema` prints its JSON Schema.
- `[workspace.lint]` `enable = ["strict"]` or one group (`strict.locks`, ...)
  turns on strict rules; `rule-name = "off"|"hint"|"info"|"warn"|"error"`
  sets one rule. `[[workspace.lint.overrides]]` scopes settings to globs.
- Legacy code with many findings: record a baseline once, then gate on new
  findings only.

```sh
oxabl schema
oxabl check src --baseline oxabl-baseline.json --update-baseline
oxabl check src --baseline oxabl-baseline.json
```

Only update a baseline when the person asks; it hides findings.

## Pitfalls

- Missing include paths give `PREPROC007 unresolvable include` and then false
  `LINT0001 undefined symbol` for everything the include declares. Fix the
  include path, not the code. `PREPROC007` alone does not fail `check`: read
  stderr or the JSON `preproc` array, not just the exit code.
- Without a schema, `LINT0003` (unknown field) and most strict rules on
  database tables (e.g. `LK001`, `TX001`, `DB001`, `NL003`) stay silent, and
  `explain` cannot pick indexes. A clean `check` with no schema says nothing
  about field names.
- `check DIR` checks `.i` files only for context-free strict rules. A file you
  name is always checked, whatever its extension; an `.i` is then a fragment
  (no unused-variable rules). `--include-findings` reports findings inside
  includes per including program.
- `oxabl build --plan` writes its graph under `.oxabl/build` (by default). Keep
  it out of commits.
- `oxabl lsp` is for editors. `check --watch` never ends. Do not run either.

## More

- `references/check.md`: check JSON fields, include files, baselines, format.
- `references/rules.md`: every rule code, group and severity.
- `references/explain.md`: the `explain` text and JSON fields.
- `references/build.md`: build flags, JSON report, events, no-OpenEdge fallback.
- Docs: https://docs.oxabl.org/llms.txt,
  https://docs.oxabl.org/toolchain/configure/oxabl-toml/,
  https://docs.oxabl.org/toolchain/commands/check/
- This skill ships with oxabl. Print a file of it, or refresh installed copies
  after an upgrade:

```sh
oxabl skill show references/rules.md
oxabl skill install --refresh
```
