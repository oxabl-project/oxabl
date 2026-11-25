# oxabl rule codes

Read this when `oxabl check` reports a code you do not know, or when you set a rule's severity in `oxabl.toml`. Codes, names, groups and default severities come from the oxabl source (`oxabl_lint` rule registry).

## Configure a rule

Name a rule by code or by kebab name in `[workspace.lint]`:

```toml
[workspace]
name = "acme"

[workspace.lint]
enable = ["strict.locks"]      # a group; "strict" turns on every strict.* group
unused-variable = "info"       # by name
LINT0006 = "off"               # or by code

[[workspace.lint.overrides]]
files = ["legacy/**"]          # globs relative to the oxabl.toml directory
disable = ["strict"]
```

Severities: `off`, `hint`, `info`, `warn` (or `warning`), `error`. Any finding that is reported, `hint` included, makes `oxabl check` exit 1. An unknown rule or group name is a warning and that entry is ignored; a value that is not a severity is a config error (exit 2). The `default` group cannot be toggled, but each of its rules can be set to `off`.

## Core rules (group `default`, always on)

| Code | Name | Default | Reports |
|---|---|---|---|
| LINT0001 | `undefined-symbol` | error | A name that resolves to nothing in scope. |
| LINT0002 | `unused-variable` | warning | A variable or parameter that nothing references. |
| LINT0003 | `unknown-table-or-field` | warning | A field or sequence the loaded schema does not declare. Needs a schema. |
| LINT0004 | `type-mismatch-assignment` | warning | An assignment whose value type does not fit the target. |
| LINT0005 | `block-var-used-outside` | info | A block-defined variable read outside its block before it is set. |
| LINT0006 | `assigned-but-never-read` | warning | A variable that is written but never read. |

`LINT0001` from symbols an include declares usually means the include was not found: look for `PREPROC007` on the same file first.

## Strict rules (opt-in, preview, default warning)

| Code | Name | Group | Reports |
|---|---|---|---|
| TX001 | `implicit-transaction` | `strict.transactions` | A block that becomes a transaction only because it updates the database. |
| TX002 | `procedure-wide-transaction` | `strict.transactions` | A main block that is a transaction block, so the transaction spans the procedure. |
| TX003 | `transaction-spans-ui` | `strict.transactions` | A statement that waits for the user inside a transaction. |
| TX004 | `transaction-spans-call` | `strict.transactions` | A RUN inside a transaction whose callee is not in the file. |
| TX005 | `scope-widened` | `strict.transactions` | A reference outside a FOR block that raises the block's buffer scope to the procedure. |
| TX006 | `missing-no-undo` | `strict.transactions` | A variable, parameter, temp-table or property without NO-UNDO. |
| LK001 | `default-lock` | `strict.locks` | A database read with no lock phrase, which takes SHARE-LOCK. |
| LK002 | `lock-outlives-transaction` | `strict.locks` | A record still locked after its transaction, because the buffer scope is wider. |
| LK003 | `lock-upgrade-in-loop` | `strict.locks` | A NO-LOCK read upgraded to EXCLUSIVE-LOCK on every iteration of a loop. |
| DB001 | `find-in-loop` | `strict.access` | A FIND or CAN-FIND that runs once per row of a FOR EACH or query loop (N+1). |
| HD001 | `handle-not-deleted` | `strict.handles` | A dynamic object that some path loses without deleting it. |
| HD002 | `delete-not-in-finally` | `strict.handles` | A delete that an error between the create and the delete skips. |
| HD003 | `persistent-procedure-leak` | `strict.handles` | A persistent procedure that nothing deletes. |
| HD004 | `memptr-not-freed` | `strict.handles` | A MEMPTR given a size and never set back to 0. |
| HD005 | `missing-widget-pool` | `strict.handles` | Dynamic objects created with no widget pool scoped to the procedure. |
| NL001 | `unknown-in-comparison` | `strict.nulls` | A possibly unknown variable in an ordering comparison or condition. |
| NL002 | `unknown-propagation` | `strict.nulls` | A possibly unknown variable in arithmetic that is stored or displayed. |
| NL003 | `field-after-failed-find` | `strict.nulls` | A buffer field used after a FIND ... NO-ERROR that may have failed, with no AVAILABLE test. |
| NL004 | `nullable-db-field` | `strict.nulls` | A database field that starts at `?` and is not mandatory, used without a test for `?`. |
| CL001 | `run-value` | `strict.calls` | RUN VALUE(...) with a target chosen at run time. |
| CL002 | `dynamic-invoke` | `strict.calls` | DYNAMIC-FUNCTION, DYNAMIC-INVOKE, DYNAMIC-NEW, DYNAMIC-PROPERTY and CALL objects. |
| CL003 | `signature-mismatch` | `strict.calls` | A RUN whose arguments differ from the callee's parameters in count, kind, mode or type. |
| CL004 | `unresolved-call` | `strict.calls` | A RUN of a name that is neither a procedure of the file nor a program on the search paths. |
| CL005 | `shared-state` | `strict.calls` | NEW SHARED and SHARED definitions. |

Rules on database tables skip temp-tables, and skip every table when no schema is loaded (for example LK001, LK002, TX001, DB001, NL003, NL004).

## Other codes

These are not lint rules: `[workspace.lint]` does not configure them.

| Code | Meaning |
|---|---|
| PARSE001 | Syntax error. `oxabl format` leaves the file unchanged until it parses. |
| SEM0001 | Duplicate declaration in the same scope. |
| SEM0002 | SHARED / NEW SHARED declarations that do not match. |
| SEM0003 | BLOB or CLOB used as a local variable type. |
| PREPROC002 | Unclosed `&IF` block. |
| PREPROC003 | Include depth limit exceeded. |
| PREPROC005 | Include file found but could not be read. |
| PREPROC006 | Cyclic include. |
| PREPROC007 | Unresolvable include: its symbols cannot be checked. Add its directory to `include_paths` or pass `-I`. |
| PREPROC008 | Unterminated include reference `{...`. |
| PREPROC009 | A predefined name without a value used in a condition (set it in `[workspace.preprocessor]`). |
| PREPROC010 | A part of an `&IF` condition the evaluator does not model. |
| PREPROC011 | Preprocessor expansion limit exceeded; nothing more is expanded. |
| PREPROC012 | A `~{` in an include argument that is never closed, or an argument that moves the end of an include reference when it fills a `{N}` there: it ends with `~` or holds an unclosed `{` (the compiler stops with error 374), or it holds a `}` that ends the reference early (the compiler reads the rest as code). |
| PREPROC013 | A substituted value that ends with `~`: the compiler joins it with the text after it, which oxabl does not model. |
| SCHEMA0001 | A `.df` file that does not parse. |
| SCHEMA0010 | Duplicate table across merged `.df` files (last one wins). |
| SCHEMA0011 | Duplicate field within a table. |
| SCHEMA0012 | A field type that conflicts across merged `.df` files. |
| SCHEMA0020 | An `UPDATE`, `CHANGE`, `DROP` or `RENAME` directive in a `.df` that the loader skips (schema browser). |
| SCHEMA0021 | A `.df` path that is not UTF-8 (schema browser). |
| SCHEMA0030 | A schema path outside the workspace root. |
| SCHEMA0031 | Too many tables or fields (resource cap). |

The coverage warnings `PREPROC002`, `PREPROC007`, `PREPROC008`, `PREPROC009`, `PREPROC010` and `PREPROC013` do not change the exit code of `oxabl check`: a file whose only problem is a missing include exits 0. They print on stderr, also with `--json`, where they are in the `preproc` array, not in `diagnostics`.

Docs: https://docs.oxabl.org/toolchain/commands/check/
