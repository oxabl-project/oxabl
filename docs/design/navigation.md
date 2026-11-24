# Navigation: go to definition, hover and document links

`oxabl lsp` answers `textDocument/definition`, `textDocument/hover` and
`textDocument/documentLink`. Each analyses one compilation unit per request and
gives answers in the bytes of physical files: a declaration written in an
include is placed in the include, never in the expanded text.

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

## Hover

`Navigation::hover` (in `crates/oxabl_pipeline/src/navigate/hover.rs`) finds
the name at the cursor with the same walk as go to definition
(`Navigation::innermost`), so the two never disagree about what a name means.
The answer is short Markdown: one ABL code block, and a line that says where the
name is declared when that helps.

| The name is | Hover |
| --- | --- |
| A variable, parameter or property | `DEFINE ... VARIABLE x AS t EXTENT n INITIAL v NO-UNDO`; a parameter also names its procedure, function or method; a table or dataset parameter keeps `APPEND`, `BIND`, `BY-VALUE` and `BY-REFERENCE`; a property gives its `GET` and `SET` with their access modifier; a `LIKE` type also gives the type it resolves to |
| A temp-table field | `FIELD f AS t ...` and its temp-table; a field that `LIKE` inherited shows the table that declares it |
| A temp-table | `DEFINE TEMP-TABLE t ...`, the number of fields and indexes it has (inherited and own), and its indexes with the primary one named (see below) |
| An internal procedure, function or method | Its signature with each parameter (a method always with its `()`); a declaration without a body keeps what follows its parameters (`IN SUPER`, `IN h`, `FORWARD`, `MAP TO`, `EXTERNAL`); an inherited method gives each overload and the class that declares it |
| `RUN p IN h` | The signature of `p` in the program the handle runs, when its text declares `p` once |
| A program (`RUN x.p`) | `RUN x.p`, the path relative to its include path entry |
| A class name | The class header with `INHERITS` and `IMPLEMENTS` |
| A table, a buffer named like its table | Its label, description, number of fields and indexes (`oxabl_pipeline::schema_hover::table_markdown`) |
| A schema field | Its type, format, label, initial value, description and the indexes it takes part in (`schema_hover::field_markdown`) |
| A built-in function, statement keyword or system handle | Its title in the Progress documentation and a link to the page |

The declaration text comes from the parsed declaration, not from the symbol
table: the expanded unit is lexed and parsed once more on the first request that
needs it, and a class file or a program is parsed when the hover names a member
or a procedure in it. `EXTENT` and `INITIAL` are read from the tokens of the
declaration, as written. The hover is a summary of the declaration, not a copy
of it: options that do not change what a name holds or what a call does
(`FORMAT`, `LABEL`, `VIEW-AS`, `SERIALIZE-NAME`, ...) are left out.

A temp-table that is defined `LIKE` another table has the fields and indexes of
that table (`navigate/hover/like.rs`). The source is the record that the
analysis resolved for the `LIKE` clause (`SymbolTable::temp_table_like`): a
temp-table of the file, followed through a chain, or a table of the loaded
schema. The rules are the ones that OpenEdge 12.8.9 applies:

- `LIKE t` takes all fields and all indexes of `t`, with their primary and
  unique flags. `LIKE-SEQUENTIAL` takes the same.
- `USE-INDEX` takes only the indexes it names. The one marked `AS PRIMARY` is
  the primary one. When none is marked, the inherited primary index stays
  primary if it is taken, and the first one taken is primary if not.
- Own `FIELD` and `INDEX` definitions come after the inherited ones. An own
  primary index replaces the inherited primary index. When no index is
  primary, the first index is.

When the source is a table that the loaded schema does not have, when no schema
is loaded, or when the chain does not end, the hover does not count the
inherited part as zero. It says `Fields: 1 own + those of `Customer` (not in
the loaded schema)`, gives the same for the indexes, and lists only the own
indexes, as "Own indexes". A temp-table member that a class file declares has
no resolved source in that file, so its `LIKE` is shown the same way.

A type that an `&IF` chooses has no one name, so the hover writes no `AS`
part and says "An `&IF` chooses the type." A schema field whose type the
loader could not settle (two `.df` files that disagree) says "Type: unknown".
Code blocks and code spans use a fence longer than any run of backticks in the
text, so source and schema text cannot break the Markdown.

The documentation data is `crates/oxabl_pipeline/src/keyword_docs.rs`, which
`cargo run -p oxabl_codegen -- keyword-docs` writes from
`resources/abl_keyword_index.json`: each statement, built-in function and system
handle with its title and URL, and each abbreviation of a function. The index
holds no descriptions, so a keyword hover gives the title, the kind and the
link. A built-in function shows its page when the cursor is on the name of a
call the analysis left to the runtime (an unresolved name of the built-in
function registry), written as a call (`name(`). The analysis also leaves a
name unresolved when it cannot see where it is declared (an include analysed
alone, a class whose ancestor oxabl did not read), so a bare `interval` may be
a variable or a property, and gives no hover. In an include analysed alone, and
in a class that inherits another class than `Progress.Lang.Object`, a call may
also be a function of the includer or an inherited method (`Compare(...)`), so
there only a reserved name (`LOOKUP(`), which no routine can have, shows the
page. `keyword_docs.rs` marks each reserved entry. A statement keyword shows its page when it is one of the
first words of a statement: the longest documented head wins, and the words
between `DEFINE` and what it defines (`NEW`, `SHARED`, `PRIVATE`, `INPUT`, ...)
are passed over, so `DEFINE INPUT PARAMETER` is the `DEFINE PARAMETER` page.

No hover is given for a name that does not resolve, a name with several
candidates (an ambiguous `RUN` target or class), an include reference, a
comment, or a statement whose head is not documented.

The two schema functions read one `oxabl_schema::Table` and nothing else, so a
request that shows a whole table (a schema browser) can use the same text.

## Document links

The links of a file are its own root-level include references, its literal
`RUN` targets that resolve to a program, and its `USING` names that resolve to a
class. A wildcard `USING` has no link. Text in an `&IF` branch that the
preprocessor did not take is not analysed, so it has no link, no definition and
no hover.

## Includes and configuration

An open include with a selected compilation context (see
`docs/design/include-context.md`) is analysed in that root for go to definition
and hover. Without one, and always for its links, the include is analysed alone.

Local names need no setup. `RUN` targets, include files, classes and inherited
members are found through `[workspace.sources].include_paths`, and tables and
fields through `[workspace.schema]`. No request depends on
`[workspace.features]`.

## Tests

`crates/oxabl/tests/definition_lsp.rs` and `document_link_lsp.rs` pin the
targets, and `hover_lsp.rs` pins the hover text of each kind of name.
`navigation_sweep_lsp.rs` asks for a definition at every position, and a hover
at every position that can give another answer (each start and end of a name,
and each other character outside a run of spaces), of files full of awkward
names (a table and field of one name, keywords, hyphens, non-ASCII, a
200-character name, a `.df` that breaks off), and wants a well-formed answer
with no error each time, with every range inside its file. It sends the hovers
one at a time, because the server drops a hover that a newer one replaced.
The `navigate` fuzz target (`fuzz/README.md`) does the same on fuzzed input.

Every answer goes through the `oxabl lsp` proxy, which restores a `null`
result: `lsp_server` reads `"result": null` back as no result and would write
a response with neither `result` nor `error`, which a client rejects.

## Cost

Each request expands, parses and resolves the unit again (no type check), with
the open buffers over the disk, on a worker thread. Nothing is memoized. A
request on a `RUN` target that is not a program also lexes and parses the
expanded unit once more for its call sites (`FileCalls::extract`), and reads
the outline of each program a handle runs. A hover parses the expanded unit at
most once more (shared with the call sites), and parses one more file when it
names a class, an inherited member or a procedure run through a handle. A
keyword hover scans the documentation table, about 460 entries.

Hover requests run one at a time (`HoverGate` in
`crates/oxabl_lsp/src/navigation.rs`). An editor asks for a hover at each pause
of the pointer, and only the last one counts: a hover that a newer one replaced
before it started, or that the client cancelled with `$/cancelRequest`, answers
`RequestCanceled` (-32800) and computes nothing. A hover that has started runs
to its end. A ticket that never started (no session, no thread) is withdrawn
and cancels nothing. `crates/oxabl/tests/hover_cancel_lsp.rs` tests both
through the server: a source with the marker `OXABL-TEST-HOLD:hover` makes its
hover wait three seconds before it computes, in test builds only
(`oxabl_common::hold_if_injected`), so the order of events does not depend on
the speed of the machine. The
resolve pass of every other run records no site: the check of the flag is its
whole cost.
