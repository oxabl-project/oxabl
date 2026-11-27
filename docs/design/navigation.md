# Navigation: go to definition, hover, highlights and document links

`oxabl lsp` answers `textDocument/definition`, `textDocument/hover`,
`textDocument/documentHighlight` and `textDocument/documentLink`. Find
references and the call hierarchy use the same resolution; see
`docs/design/references.md`. Go to implementation, go to super method and the
type hierarchy read the cursor the same way; see `docs/design/type-hierarchy.md`. Semantic tokens read the same analysis; see
`docs/design/semantic-tokens.md`. Signature help and the parameter-name hints
read the same sites; see `docs/design/signature-help.md`.
Each analyses one compilation unit per request and gives answers in the bytes of
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
| `Symbol(sym)` | A name in a statement position (`FIND b`, `CREATE tt`), a temp-table field named through its qualifier, and a temp-table, buffer or dataset named by a declaration (`TABLE FOR tt`, `DATASET FOR ds`, `DEFINE DATASET ds FOR tt` and its `DATA-RELATION`). The parameter that `TABLE FOR tt` declares is not a place of its own: the name there is the temp-table. |
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

A member named through a receiver (`obj:Name`, `obj:Greet()`) resolves when
oxabl knows the class of the receiver: a name declared `AS` a class, `NEW`,
`CAST(x, Cls)`, `THIS-OBJECT` and `SUPER` in a class body, a type name for a
static member, an enum member, and each link of a chain (`a:b():c`). The class
can be a class of the analysed file, also with no index, or a workspace class.
A method resolves to the first declaration of its name, which stands for the
overload set. The rules are in `class-members.md`.

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

The signature of a procedure, function or method shows an include in its
parameter list as one entry, as the declaration writes it, with the count of
the parameters it declares: `METHOD PUBLIC VOID SetData ({inc/std-params.i} (2
parameters), INPUT pcDocNum AS CHARACTER)`. It never lists the parameters of
the include. A declaration that is not known whole (see "A parameter include"
in `signature-help.md`) shows the include as the file writes it, with no
count: `METHOD PUBLIC VOID Go ({inc/with-if.i}, INPUT pcB AS INTEGER)`. The
expansion picked one branch of an `&IF`, or one value of a `{&name}`, so its
parameters are not listed as if the declaration wrote them.

A temp-table that is defined `LIKE` another table has the fields and indexes of
that table. One definition gives them: `EffectiveTable::declare` in
`oxabl_schema` (`like.rs`). The hover (`navigate/hover/like.rs`) and the
effects analysis (`oxabl_effects`, for the index of `oxabl explain`, the
`index` inlay hint and `find-in-loop`) both use it. The source is the record that the
analysis resolved for the `LIKE` clause (`SymbolTable::temp_table_like`): a
temp-table of the file, followed through a chain, or a table of the loaded
schema. The rules are the ones that OpenEdge 12.8.9 applies, as its
`INDEX-INFORMATION` reports them for a probe program (`LIKE`, `USE-INDEX` with
one and several indexes, `AS PRIMARY`, `LIKE-SEQUENTIAL`, own primary index,
chains):

- `LIKE t` takes all fields and all indexes of `t`, with their primary and
  unique flags. `LIKE-SEQUENTIAL` takes the same.
- `USE-INDEX` takes only the indexes it names, listed in the order of the
  source table, not the order written. The one marked `AS PRIMARY` is the
  primary one. When none is marked, the inherited primary index stays primary
  if it is taken (`USE-INDEX ia USE-INDEX ib` and `USE-INDEX ib USE-INDEX ia`
  both keep `ia`), and a single non-primary index that is taken becomes
  primary.
- Own `FIELD` and `INDEX` definitions come after the inherited ones. An own
  primary index replaces the inherited primary index. When no index is
  primary, the first index is.

When the source is a table that the loaded schema does not have, when no schema
is loaded, or when the chain does not end, the hover does not count the
inherited part as zero. It says `Fields: 1 own + those of `Customer` (not in
the loaded schema)`, gives the same for the indexes, and lists only the own
indexes, as "Own indexes". A temp-table member that a class file declares has
no resolved source in that file, so its `LIKE` is shown the same way. The
effects analysis names no index for a table whose `LIKE` source it does not
know.

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
a variable or a property, and gives no hover. A call may also be a routine that
the unit cannot see: in an include analysed alone (a function of the includer),
in a unit that misses the text of an include (one that does not resolve, cannot
be read or is elided), and in a class with an ancestor whose members were not
read (an inherited method, `Compare(...)`). The resolver records such a call
(`SymbolTable::may_be_member_call`), from the same rule that softens the misses
of that class, so a class whose ancestors were all read, and `INHERITS Object`
after `USING Progress.Lang.*`, keeps the pages. There only a reserved name
(`LOOKUP(`), which no routine can have, shows the page. `keyword_docs.rs` marks
each entry that the official keyword index (`abl_reserved_keywords.txt`)
reserves; the lexer's overrides (`EXTENT`) do not count. A statement keyword
shows its page when it is one of the
first words of a statement: the longest documented head wins, and the words
between `DEFINE` and what it defines (`NEW`, `SHARED`, `PRIVATE`, `INPUT`, ...)
are passed over, so `DEFINE INPUT PARAMETER` is the `DEFINE PARAMETER` page.

No hover is given for a name that does not resolve, a name with several
candidates (an ambiguous `RUN` target or class), an include reference, a
comment, or a statement whose head is not documented.

The two schema functions read one `oxabl_schema::Table` and nothing else, so a
request that shows a whole table (a schema browser) can use the same text.

### Database effects

A hover on a procedure, function, method or `RUN` target also names the routine
(`Hover::routine`, an `oxabl_pipeline::Routine`). `Hover::add_effects` then adds
its database effects after a rule, from the transitive effect summaries over the
call graph (`WorkspaceEffects::routine_markdown`, the summaries that the effect
code lenses and `TX004` read):

```text
**Database effects**
- `Item`: read EXCLUSIVE-LOCK, update
- `Item` through `ReadItems`: read NO-LOCK
- `Item` through `archive.p`: read EXCLUSIVE-LOCK, delete
- `Ledger` through `archive.p`: create

**Incomplete:** effects may be missing: a dynamic call (`RUN VALUE`, ...).
```

- One line per table and route. A line without `through` is a direct effect
  of the routine. `through x` names a routine it calls, and lists all that `x`
  does, directly and through its own calls, from the summary of `x`: also an
  effect the caller has itself. (The caller's summary keeps one effect per
  table, operation, lock and depth, so it cannot give the routes whole.)
- A read gives its lock: `NO-LOCK`, `SHARE-LOCK`, `EXCLUSIVE-LOCK`, or
  `SHARE-LOCK (default)` when none is written. A table that the schema does
  not have says so. Temp-tables are not listed.
- `**Incomplete:**` names every reason the summary may miss effects: a dynamic
  call, a handle oxabl cannot follow, a program that no file supplies or that
  several files supply, a unit whose own effects are not all known, and a call
  graph or summary that reached a size limit. A method that a subclass can
  override (its class is not `FINAL`, and it is not `FINAL`, `PRIVATE` or
  `STATIC`) adds "an override in a subclass may run instead of this method":
  the call graph follows the declared type, and another body can run. Other
  bodies that oxabl does not see give their own reason: an interface method
  (the implementing class supplies the body), `PROCEDURE p IN SUPER` (a super
  procedure), `PROCEDURE p IN h` (the procedure that the handle names), also
  when the prototype is in the program that `RUN p IN h` runs. A
  routine that runs a program on an AppServer (`RUN ... ON SERVER`), itself or
  through what it calls, also gives a reason: that program changes the
  database, but the summaries leave a remote call out (as `TX004` needs). A
  summary with a reason is never
  shown as complete: with no effect found it says "No known database effects",
  not "No database effects".
- No effects are added when the summaries do not hold the routine, or hold two
  routines of its name (method overloads).

The daemon takes the summaries of the open root buffer from its memoized query
(`effect_summaries`, shared with the code lenses). For a root that is not open,
and for a method of another class file, it takes those of that file from
`file_effect_summaries`, memoized per disk revision of every file the summaries
read, so a later hover reuses them and a change on disk rebuilds them. Both
read the files other than the open root from disk, not from open buffers. When
one of those files is open in the editor with other text than on disk, the
hover gives no effects and says "Not known: `x` has unsaved changes", because
the effects of the saved file are not the ones the editor shows. The check
covers every file that the summaries read, so an unsaved change in any program
the root reaches also stops it, also for a routine that does not reach that
program. The check goes over the open buffers, not over the files read: the
set of files read is built once per memoized summary and kept with it.

## Document links

The links of a file are its own root-level include references, its literal
`RUN` targets that resolve to a program, and its `USING` names that resolve to a
class. A wildcard `USING` has no link. Text in an `&IF` branch that the
preprocessor did not take is not analysed, so it has no link, no definition and
no hover.

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
- The resolver does not read the tables after the first one of a `FOR EACH`
  header (`FOR EACH a, EACH b WHERE ...` and `OF`). A name in the `WHERE` or
  `OF` of such a table is not a use, so the cursor on it gives none, and the
  cursor on the first table does not mark it. Go to definition has the same
  gap. The list is then not complete.

VS Code marks the same word as plain text when the server answers `null`. That
fallback is the editor's own.

## Includes and configuration

An open include with a selected compilation context (see
`docs/design/include-context.md`) is analysed in that root for go to definition
for hover and for highlights. Without one, and always for its links, the include is
analysed alone.

Local names need no setup. `RUN` targets, include files, classes and inherited
members are found through `[workspace.sources].include_paths`, and tables and
fields through `[workspace.schema]`. No request depends on
`[workspace.features]`.

## Tests

`crates/oxabl/tests/definition_lsp.rs`, `document_highlight_lsp.rs` and
`document_link_lsp.rs` pin the targets, the uses and the links, and
`hover_lsp.rs` pins the hover text of each kind of name.
`highlight_cost.rs` of `oxabl_pipeline` counts the file system operations of a
highlight in a unit with 300 `RUN ... IN h`. A test of `oxabl_daemon::db` counts
the executions of the memoized queries over five cursor moves.
`document_highlight_lsp.rs` also pins an unsaved include, a new include that has
no file yet, and that the newest of a burst of requests is answered while older
ones, on a large file, are replaced.
`document_link_lsp.rs` also pins an unsaved include, a new include that has no
file yet, a file that is not open, and that a second request on the same text
of an open file builds no navigation model: a source with the marker
`OXABL-TEST-COUNT:lsp.document-links` makes a test build of the server log the
number of models each links request built.
`navigation_sweep_lsp.rs` asks for a definition and for highlights at every
position, and a hover at every position that can give another answer (each
start and end of a name, and each other character outside a run of spaces), of
files full of awkward names (a table and field of one name, keywords, hyphens,
non-ASCII, a 200-character name, a `.df` that breaks off), and wants a
well-formed answer with no error each time, with every range inside its file.
It sends the hovers one at a time, because the server drops a hover that a newer
one replaced.
The `navigate` fuzz target (`fuzz/README.md`) does the same on fuzzed input.

Every answer goes through the `oxabl lsp` proxy, which restores a `null`
result: `lsp_server` reads `"result": null` back as no result and would write
a response with neither `result` nor `error`, which a client rejects.

## Cost

Go to definition and hover expand, parse and resolve the unit again (no type
check), with the open buffers over the disk, on a worker thread. Nothing is
memoized, except what a highlight, the links of an open file and a hover on a
routine keep (see below). A request on a `RUN` target that is not a program also lexes and parses
the expanded unit once more for its call sites (`FileCalls::extract`), and reads
the outline of each program a handle runs. A hover parses the expanded unit at
most once more (shared with the call sites), and parses one more file when it
names a class, an inherited member or a procedure run through a handle. A
keyword hover scans the documentation table, about 460 entries. A hover on a
routine reads the effect summaries: memoized per open buffer revision and per
disk revision of every other file, but the first one builds the summaries of
every program the root or class reaches.
The resolve pass of every other run records no site: the check of the flag is its
whole cost.

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
the speed of the machine.

A highlight comes on every cursor move, so it has a bound:

- An open file that is its own root is answered from `navigation_model`, a
  memoized query of the daemon (`oxabl_daemon::db`). It reads the expansion that
  diagnostics read (`expanded_text`, one execution for both) and records the
  navigation sites in its own resolve pass. The first highlight of a revision of
  the buffer pays for that pass; every later one pays for a pass over the
  recorded sites. An edit makes the next highlight analyse again. The model
  also keeps the call sites (`FileCalls`) after the first `RUN` that needs
  them. Includes are read from disk, as for diagnostics. When the unit reads
  an include whose open buffer differs from the file, or another open document
  has no file on disk, the request is analysed as go to definition is, with the open buffers over the disk, so both answer
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
- The links of an open file are answered from the same `navigation_model`, with
  the same condition on unsaved includes, so the links, the highlights and the
  semantic tokens of one revision of a buffer pay for one resolve pass. The
  links are always those of the file as its own root, so a selected compilation
  context does not change this. A file that is not open is analysed from disk
  for each request.
- The highlights of one document run one at a time. A request that waits while
  a newer one comes is answered `ContentModified` (-32801) without an analysis,
  so a held arrow key runs at most the analysis in progress and the newest one.
