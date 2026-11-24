# Navigation: go to definition, highlights and document links

`oxabl lsp` answers `textDocument/definition`, `textDocument/documentHighlight`
and `textDocument/documentLink`.
All three analyse one compilation unit per request and give answers in the bytes of
physical files: a declaration written in an include is placed in the include,
never in the expanded text.

## Where the answer comes from

The resolve pass records **navigation sites** when the analysis context asks for
them (`AnalysisContext::record_sites`, off for every other run). A site is a
span of the expanded text and what is written there
(`oxabl_semantic::SiteTarget`):

| Site | Recorded for |
| --- | --- |
| `Reference(node)` | Every expression the walk reaches, the field and each qualifier of a field access. The answer is `Semantic::references[node]`. |
| `Run(node)`, `Using(node)` | A literal `RUN` target and a `USING` name. |
| `Type(resolution)` | A class name in a type position: `NEW`, `CAST`, `INHERITS`, `IMPLEMENTS`, and the type of a `DEFINE`, parameter, property or function. A declared type has no span in the AST, so the pass finds the name after the declared name in the text. |
| `Symbol(sym)` | A name in a statement position (`FIND b`, `CREATE tt`) and a temp-table field named through its qualifier. |
| `Table(id)`, `SchemaField(id)` | The table of `DEFINE BUFFER b FOR t`; the field of `LIKE table.field` (also when the field has its table's name), and a schema field that a temp-table copies with `LIKE`. |

The same run also keeps the file behind each synthesized symbol: the class file
of a class, the file of the nearest class that declares an inherited member, and
the schema table of a schema field. The schema keeps the `.df` file of each table
(`Schema::table_source`).

`oxabl_pipeline::Navigation` reads these. At the cursor it takes the shortest
site that contains it; a declaration's own name counts as a site. Sites of one
length name one thing, and the first with a target wins. A name that resolved to
nothing stops the search, so the cursor on an unresolved argument never answers
with the call around it.

| The name resolves to | Target |
| --- | --- |
| A declaration in the unit | Its name, in the root or in the include that wrote it |
| A program (`RUN x.p`, `RUN x`) | The file |
| An internal procedure run through a handle (`RUN p IN h`) | `PROCEDURE p` in the program that `RUN ... PERSISTENT SET h` runs, or that file when its text declares no `p` (a super procedure of it can supply `p`) |
| A class | The class name in its `.cls` |
| An inherited member | Each declaration of the name in the class that declares it, so each overload |
| A buffer named like its table (`FOR EACH customer`), a table | The `ADD TABLE` in the `.df` |
| A schema field | The `ADD FIELD` in the `.df` |
| An include reference `{x.i}` | The file the preprocessor opened |

## What it never does

- **Guess.** A name that does not resolve has no target. A computed target
  (`RUN VALUE(...)`, `DYNAMIC-NEW`) has none.
- **Follow a handle it cannot see.** `RUN p IN h` has a target only when every
  `RUN ... PERSISTENT SET h` of the unit runs one program and nothing else
  writes `h` (`oxabl_index::calls`, `CallTarget::InProgram`). A handle that two
  programs set, that an assignment or an `OUTPUT` argument writes, or that
  comes from another file (a parameter, a shared variable) gives none. So does
  `RUN p IN h` with a local `p`: the handle, not the unit, supplies it.
  `IN THIS-PROCEDURE` goes to the local procedure.
- **Pick one of several.** A `RUN` target or a class name that two entries of the
  include path supply (the index answers `Unknowable`) has each file as a target
  (`oxabl_index::search::find_candidates_with`). The candidates are the files for
  the name as written; an unqualified class name that two `USING` imports make
  ambiguous gives none.
- **Link to an ambiguous file.** A document link is made only for a name that
  resolves to exactly one file.

## Document links

The links of a file are its own root-level include references, its literal
`RUN` targets that resolve to a program, and its `USING` names that resolve to a
class. A wildcard `USING` has no link. Text in an `&IF` branch that the
preprocessor did not take is not analysed, so it has no link and no definition.

## Document highlights

`textDocument/documentHighlight` marks the other uses of the name at the cursor.
`Navigation::highlights` finds the name in the same way as go to definition: the
shortest site at the cursor. A cursor at the end of a word means that word, not
the call or the statement that holds it (`Double(x|)` is `x`). Then it keeps
each site that names the same thing. Each use has the kind
`Read` or `Write`.

Two sites name the same thing when they have the same identity:

| The name resolves to | Identity |
| --- | --- |
| A declaration in the unit | The symbol. An include that the unit reads twice declares one symbol for each read, so `DEFINE VARIABLE i` in an include read in two procedures is two variables |
| A buffer that the code declares (`DEFINE BUFFER`, a parameter buffer) | The symbol, also when it is named like its table. It is its own record scope |
| A table name that no buffer declares, and the default buffer of a table | The table. All such uses in the unit are one name. `FOR EACH` of a database table uses its default buffer, so the uses inside the loop and those around it are one name |
| A program, or a class from another file | The file |
| An inherited member | The class file and the member name |
| A schema field | The table and the full field name, so an abbreviation counts |
| A `RUN p IN h` target | The targets that go to definition gives, when there is exactly one |

Go to definition on a buffer named like its table goes to the table. A highlight
does not follow it: it marks the uses of that buffer only.

The answer holds:

- Only the uses that are written in the file of the request. The cursor in an
  include gives the uses in that include, and the cursor in the program gives
  the uses in the program. An include that the unit reads twice gives each use
  once.
- The declaration, as a `Write`. A buffer declaration is a `Read`.
- A use that the statement changes, as a `Write`: an assignment target, an
  `ASSIGN`, `UPDATE`, `SET` or `IMPORT` target, an `OUTPUT` or `INPUT-OUTPUT`
  argument, a `CREATE` and a `DELETE`. `RELEASE` and `VALIDATE` do not change
  the record: they are a `Read`. The resolve pass records this on each site (`Site::write`),
  from the access mode that it already uses to count reads and writes. Every
  other use is a `Read`.
- Only the name. A call site covers its whole call, so a span that holds
  another use of the same name is dropped and the name stays.

The answer never guesses:

- A name that does not resolve has no highlight. No other text of the same
  spelling is marked, and neither is a string or a comment.
- A name with more than one possible declaration (a `RUN` target that two
  entries of the include path supply, a `RUN p IN h` with several targets) has
  none. The uses of such a name are not matched by their text either.
- A use that comes from a macro (`{&name}`) or an include argument has no place
  in the file, so it is not shown.
- A `FUNCTION ... FORWARD` declaration is not a use of its function: the cursor
  on it gives none, and the cursor on a call does not mark it.

VS Code marks the same word as plain text when the server answers `null`. That
fallback is the editor's own.

## Includes and configuration

An open include with a selected compilation context (see
`docs/design/include-context.md`) is analysed in that root for go to definition
and for highlights. Without one, and always for its links, the include is
analysed alone.

Local names need no setup. `RUN` targets, include files, classes and inherited
members are found through `[workspace.sources].include_paths`, and tables and
fields through `[workspace.schema]`. No request depends on
`[workspace.features]`.

## Tests

`crates/oxabl/tests/definition_lsp.rs`, `document_highlight_lsp.rs` and
`document_link_lsp.rs` pin the targets, the uses and the links.
`highlight_cost.rs` of `oxabl_pipeline` counts the file system operations of a
highlight in a unit with 300 `RUN ... IN h`. A test of `oxabl_daemon::db` counts
the executions of the memoized queries over five cursor moves.
`document_highlight_lsp.rs` also pins an unsaved include, and that the newest of
a burst of requests is answered. That only one analysis runs at a time depends on
timing and is not pinned.
`navigation_sweep_lsp.rs` asks for a definition and for highlights at every position of
files full of awkward names (a table and field of one name, keywords, hyphens,
non-ASCII, a 200-character name, a `.df` that breaks off) and wants a
well-formed answer with no error each time, with every range inside its file.
The `navigate` fuzz target (`fuzz/README.md`) does the same on fuzzed input.

Every answer goes through the `oxabl lsp` proxy, which restores a `null`
result: `lsp_server` reads `"result": null` back as no result and would write
a response with neither `result` nor `error`, which a client rejects.

## Cost

Go to definition and the links expand, parse and resolve the unit again (no type
check), with the open buffers over the disk, on a worker thread. Nothing is
memoized. A request on a `RUN` target that is not a program also lexes and
parses the expanded unit once more for its call sites (`FileCalls::extract`),
and reads the outline of each program a handle runs. The resolve pass of every
other run records no site: the check of the flag is its whole cost.

A highlight comes on every cursor move, so it has a bound:

- An open file that is its own root is answered from `navigation_model`, a
  memoized query of the daemon (`oxabl_daemon::db`). It reads the expansion that
  diagnostics read (`expanded_text`, one execution for both) and records the
  navigation sites in its own resolve pass. The first highlight of a revision of
  the buffer pays for that pass; every later one pays for a pass over the
  recorded sites. An edit makes the next highlight analyse again. The model
  also keeps the call sites (`FileCalls`) after the first `RUN` that needs
  them. Includes are read from disk, as for diagnostics. When the unit reads
  an include whose open buffer differs from the file, the request is analysed
  as go to definition is, with the open buffers over the disk, so both answer
  from the same text.
- Diagnostics do not record sites, so their own resolve pass is not the one a
  highlight uses. Recording sites in every diagnostics run would add a push for
  each expression to each keystroke.
- A site is compared by what its resolution holds. Only the name at the cursor
  asks the file system, and, when that name is a `RUN p IN h` target, the sites
  that spell it the same way. Each file is read once for a request. A unit with
  many `RUN ... IN h` costs the same as one with none when the cursor is on a
  variable.
- An include with a selected compilation context is analysed in its root for
  each request, as go to definition does, and is not memoized.
- The highlights of one document run one at a time. A request that waits while
  a newer one comes is answered `RequestCancelled` without an analysis, so a
  held arrow key runs at most the analysis in progress and the newest one.
