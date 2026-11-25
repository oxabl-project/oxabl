# Document outline

The document outline is the tree of declarations in one file. An editor shows
it in its Outline view, in the breadcrumbs, in "Go to Symbol in File"
(`Ctrl+Shift+O`) and in sticky scroll.

- `oxabl_pipeline::outline(source)` makes the tree of one file.
- `oxabl_pipeline::outline_with_includes(source, expansion)` adds the
  declarations of the include files under the `{file.i}` symbols (see
  [Include files](#include-files)).
- `oxabl lsp` sends the tree as the answer to `textDocument/documentSymbol`.
- The VS Code extension registers the provider through
  `vscode-languageclient`, which does it when the server advertises
  `documentSymbolProvider`. The extension has one setting for the include
  files, `oxabl.outline.includes`.

## Source

The outline of a file reads the **raw** buffer, like the formatter: no
preprocessing and no include expansion. Every span is a byte range of the
buffer as written, so a client maps it with no source map. This has three
results:

- The `{file.i}` reference shows as an `Include` symbol. The declarations in
  the include file are not in the raw outline. The server adds them under
  that symbol, as [Include files](#include-files) says.
- Both branches of `&IF ... &ELSE` show.
- A file that does not parse cleanly shows the declarations that the parser
  recovered. A syntax error in one procedure does not remove the other
  procedures.

A trigger phrase outside `CREATE` (`DEFINE BUTTON`, `DEFINE FRAME`,
`UPDATE ... TRIGGERS:`) is not read by the parser; its block ends the
enclosing block early.

## Symbols

| Construct | Kind | Name | Detail |
| --- | --- | --- | --- |
| `CLASS` | `Class` | The name as written, `A.B.C` | `ABSTRACT` or `FINAL` when written |
| `INTERFACE` | `Interface` | The name as written | none |
| `ENUM` | `Enum` | The name as written | `FLAGS` when written |
| `DEFINE ENUM` member of an `ENUM` | `EnumMember`, a child of its enum | The member name | none |
| `PROCEDURE` | `Procedure` | The name | `EXTERNAL` for an `EXTERNAL` procedure, `PROTOTYPE` for an `IN` prototype |
| `FUNCTION` | `Function` | The name | The return type, for example `CHARACTER` |
| `METHOD` | `Method` | The name | Access, then `STATIC`, `ABSTRACT`, `OVERRIDE` when written, then the return type or `VOID`: `PUBLIC STATIC CHARACTER` |
| `CONSTRUCTOR` | `Constructor` | The last segment of the class name | The access, for example `PUBLIC` |
| `DESTRUCTOR` | `Destructor` | The last segment of the class name | `DESTRUCTOR` |
| `DEFINE PROPERTY` | `Property` | The name | Access, `STATIC` when written, the type: `PUBLIC CHARACTER` |
| `DEFINE EVENT` | `Event` | The name | The access |
| `DEFINE VARIABLE`, `VAR` | `Variable` | The name. A `VAR` with several names gives one symbol for each name | The type: `INTEGER`, `CHARACTER EXTENT 3`, `LIKE Customer.Name`, a class name |
| `DEFINE PARAMETER`, a parameter of a function, method, constructor or event | `Parameter` | The name | The direction and the type: `INPUT INTEGER`, `OUTPUT TABLE`, `BUFFER FOR Customer` |
| `DEFINE TEMP-TABLE` | `TempTable` | The name | `LIKE t` when written |
| `FIELD` of a temp-table | `Field`, a child of its temp-table | The name | The type |
| `DEFINE BUFFER` | `Buffer` | The name | `FOR Customer`, `FOR TEMP-TABLE tt` |
| `DEFINE DATASET` | `Dataset` | The name | `FOR tt1, tt2` |
| `DEFINE DATA-SOURCE` | `DataSource` | The name | none |
| `DEFINE QUERY` | `Query` | The name | Only when the parser models the statement |
| `DEFINE STREAM` | `Stream` | The name | none |
| `DEFINE FRAME` | `Frame` | The name | none |
| `DEFINE BUTTON`, `BROWSE`, `RECTANGLE`, `IMAGE`, `MENU`, `SUB-MENU` | `Widget` | The name | none |
| `ON ... DO:` (an `ON` statement with a trigger action: a block or one statement) | `Trigger` | The event phrase with its whitespace collapsed to one space, for example `CHOOSE OF btnOk`, at most 80 characters | none |
| `ON ... DO:` in the `TRIGGERS:` phrase of a `CREATE` widget statement | `Trigger` | The event list, as for `ON` | none |
| `TRIGGER PROCEDURE FOR WRITE OF Customer` | `Trigger` | `WRITE OF Customer` | `TRIGGER PROCEDURE` |
| A statement-level `{file.i}`. `~{file.i}` has an escaped brace and gives no symbol | `Include` | The file name as written | none. With the include files on: the path of the file that the symbol shows, or a [marker](#markers) |
| `&SCOPED-DEFINE`, `&GLOBAL-DEFINE` | `PreprocessorDefine` | The macro name | `SCOPED` or `GLOBAL` |
| A labelled block, `main-loop: DO:` | `Label` | The label | none |

A member spans `name [= value]`. The value is a number with an optional sign
(`-1`), a member name, or a list of member names with commas, as in a `FLAGS`
enum. The parser skips an `ENUM` block, so the outline reads it from the
tokens, up to `END [ENUM].`.

A constructor or a destructor that no class contains (a broken file) has the
name `CONSTRUCTOR` or `DESTRUCTOR`.

A detail is `None` when the table says "none". Keywords in a detail are in
upper case. Type names and other names are as written.

An `ON` statement with no trigger action is not a symbol. This covers a key
remap (`ON F1 HELP.`), `REVERT` and `PERSISTENT RUN`. A function
prototype (`FORWARD`, `IN`, `MAP TO`) is not a symbol when the same file
defines a function with the same name (case-insensitive). Otherwise it shows,
with its parameters as children.

## Include files

An include file holds declarations that the program uses: temp-tables,
procedures, variables. The outline shows them under the `Include` symbol of the
`{file.i}` reference that brings them in, so a program whose definitions are in
`{defs.i}` has a useful outline.

`outline_with_includes` starts from the raw outline of the buffer. The
preprocessor has expanded the buffer once, with the open buffers over the
disk, and it says which include files it read, from which reference, and why
it did not read one. For each `Include` symbol:

- The preprocessor expanded the reference. The symbol gets the raw outline
  of that include file as its children, and the symbol shows the path of the
  file. A nested `{file.i}` in the include file does the same, so includes
  nest as they nest in the expansion.
- The preprocessor did not expand the reference. The symbol has no children
  and shows a [marker](#markers).

The raw outline of an include file names declarations as written. A name that
an argument writes (`{&prefix}count`, `DEFINE {1} SHARED VARIABLE`) shows with
its `{...}` as written, as in the raw outline of any file.

The symbols of an include file are left out only when the preprocessor
**decided** that an `&IF` branch is not compiled in that expansion. The
expansion decides, so the same include file can show different symbols under
two references. The buffer itself still shows both branches (see
[Source](#source)). The preprocessor sometimes chooses a branch without the
facts to choose it: a name read after an include that did not arrive, or any
`&IF DEFINED(name)` in an include that is open by itself, because its includer
can define the name. The compiler may compile such a branch. Its symbols stay,
and an `Include` symbol in it has the marker `may not be compiled`, not
`not compiled`. An `Include` symbol in text that is decided not compiled stays,
with the marker `not compiled`.

The raw outline of an include file reads both branches of its `&IF` and does not
expand its macros. When the parser has to recover from a syntax error in the
include file (for example, a version shim that gives a `PROCEDURE` head in each
branch of an `&IF` and one `END PROCEDURE`), the symbol shows the path and
`(partial)`, because the symbols can be fewer than the file declares. Oxabl
does not outline the compiled text of the include.

An include whose text sits inside a statement, for example the fields of a
temp-table (`DEFINE TEMP-TABLE tt NO-UNDO {flds.i}.`), is outlined on its own,
and a piece of a statement gives no symbol. Its symbol stays with the note
`(inside a statement)`, so the reader knows that the include adds parts that
the outline does not list. A note `(partial)` has the same meaning for an include
whose parse needed error recovery.

A reference that has no `Include` symbol in the raw outline gets one. This is a
reference in the middle of a statement, and a reference written through a
`{&name}` value (`&SCOPED-DEFINE inc {defs.i}` then `{&inc}`). The symbol of a
`{&name}` reference is named as written (`{&inc}`) and stands at that
reference. A reference that does not resolve gets the same marker as any other.
An include that a `{&name}` value writes inside another include has no
reference in a file that oxabl can place. It shows last under its includer,
named by its file. So does an include through a value that adds no text.

### Markers

An `Include` symbol that shows no declarations has one of these markers as its
detail. The marker is never absent: a reference that the preprocessor did not
expand and did not report is `not expanded`. Every reference that the
preprocessor reports as not found, unreadable or skipped has a symbol, also
when the raw outline has none.

| Marker | Meaning |
| --- | --- |
| `not found` | No file on the include path has the name of the reference. Without an include path in `oxabl.toml`, no include is found. |
| `cannot be read` | The file exists and could not be read. |
| `encrypted` | The file is encrypted source. Oxabl does not read it. |
| `not compiled` | The reference is in an `&IF` branch that the preprocessor decided is not compiled. |
| `may not be compiled` | The reference is in an `&IF` branch that the preprocessor skipped without the facts to decide. The compiler may expand it. |
| `cannot be outlined` | The preprocessor expanded the include, and oxabl failed to outline its text (a contained internal error). |
| `not expanded` | The preprocessor did not expand the reference: it closes a cycle, it is past the depth limit, or the expansion has no text for it. |
| `too large` | The declarations of this include alone are more than the limit below. |
| `outline limit reached` | The includes before this one used the symbols that the limit allows. This include can be small. |

### Limits

The cost of the outline stays bounded:

- The include files add at most `MAX_INCLUDED_SYMBOLS` (5,000) symbols to one
  outline, and each `Include` symbol of an include file counts as one. An
  include that does not fit has no children and one of two markers: `too large`
  when its declarations alone are more than the limit, and
  `outline limit reached` when the includes before it used the room. The limit
  counts an include once for each reference, so a program that includes one
  file 600 times gets the first references filled and the rest marked. No
  include is shown in part. After the limit is reached, no more include files
  are parsed.
- The raw outline of each include file is made once for each request, for all
  its expansions that read the same text.
- The expansion has the same mode and settings as the one for the diagnostics,
  but it is a separate run: the outline does not use the memoized expansion of
  the diagnostics. A buffer is preprocessed once more for each outline request
  that is not answered from the saved answer. The depth limit (64 includes) and
  the size limit of the preprocessor apply.
- The texts that the outline reads are the texts that the preprocessor read.
  Oxabl does not read an include file a second time, so a file that changes
  during the request cannot make spans and text disagree.
- The tree has at most 32 symbols above any symbol, as for a single file (see
  [Hierarchy](#hierarchy)). An `Include` symbol with that many above it
  gives no symbol of its own, and what it holds goes to the symbol above. This
  happens only for a chain of more than 30 nested includes.

### Default

The include files are **on** by default. The issue for this feature asks for a
useful outline of a program whose definitions are in includes, and a marker on
an include that oxabl cannot read only helps when it shows. An include node is a
tree node that the client can collapse. The setting turns the feature off:

- `outline.includes` (`false`) in the initialization options, and
  `settings.oxabl.outline.includes` in `workspace/didChangeConfiguration`.
- The VS Code setting `oxabl.outline.includes` sends it. A change restarts the
  language client, because the editor asks for the outline again only when the
  document changes or a provider registers.

With the setting off, the outline is the raw outline of the buffer alone, as
before. An editor can also hide the symbols itself: the symbol kind of an
`Include` symbol is `File`, and VS Code has the setting `outline.showFiles`.

## Spans

- `span` is the whole declaration, including its `END ...` and the terminator.
  Exception: each name of a `VAR` with several names spans `name [= initial]`,
  so the symbols do not overlap.
- `selection_span` is the name. A constructor or a destructor has no name: its
  `selection_span` is the first keyword of the statement. A trigger's
  `selection_span` is the event phrase.
- `span` contains `selection_span`, and `selection_span` is never empty.
- A symbol of an include file has its spans in the text of that include file.
  The rules here and in [Rules that every outline obeys](#rules-that-every-outline-obeys)
  hold for each file by itself. A symbol of an include file need not be inside
  the `span` of the symbol that holds it.

## Hierarchy

These symbols contain other symbols: `Class`, `Interface`, `Procedure`,
`Function`, `Method`, `Constructor`, `Destructor`, `Trigger` (an `ON` block),
`Property` with a computed accessor (the `SET` parameters and the declarations
of its `GET` and `SET` bodies), `Label`, `TempTable` (its fields), `Enum` (its members), `Event`
(its parameters) and a function prototype (its parameters). A property with
only `GET.` or `SET.` has no children.

The walker goes into the body of every block statement: `DO`, `REPEAT`,
`FOR EACH`, both branches of `IF`, every `CASE` branch, `CATCH`, `FINALLY`,
every `&IF` branch, and `ON` blocks. It moves the declarations that it finds
up to the nearest symbol that contains symbols, or to the top level. Children
are in source order.

A symbol has at most 32 symbols above it. A symbol that can hold symbols and has
32 above it gives no symbol of its own, and what it holds goes to the 32nd
symbol. The tree gets no deeper, and no symbol that it holds is lost. A
temp-table with no fields stays, because it holds nothing.

The parameters of a function, method, constructor or event come first, then
the declarations of the body. A `DEFINE PARAMETER` statement inside a
procedure body is a child of that procedure.

## Rules that every outline obeys

The fuzz target `outline` and the tests check these rules:

1. `outline` does not panic. `Err` comes only from the panic guard and means
   "the outline is unknown". It never means "no symbols".
2. For every symbol: `span.start <= selection_span.start <
   selection_span.end <= span.end <= source.len()`, and each offset is on a
   UTF-8 character boundary.
3. Each child's `span` is inside its parent's `span`. Siblings are in source
   order and do not overlap.
4. A name is not empty, has no line break and has no leading or trailing
   whitespace.
5. The same input gives the same output.
6. The walker does not overflow the stack on any nesting depth that the parser
   accepts.
7. No symbol has more than 32 symbols above it. A deeper tree is more than the
   JSON reader of a client accepts.

## Performance

One lex and one parse of the raw buffer, then one walk that is linear in the
size of the tree. The walk does not allocate for nodes that are not symbols.
The `outline` bench in `oxabl_pipeline` measures a large file. The language
server keeps the last answer for each open document and its version, because
VS Code asks for the outline, the breadcrumbs and sticky scroll separately.

With the include files on, the server also preprocesses the buffer and reads
and parses each include file that the buffer expands. This work is linear in
the text of the include files, up to the limits in [Limits](#limits). It runs on
a worker thread, so a slow include does not delay an edit. Requests for one
buffer version wait for one run. Any edit, open or close of a document, a save,
a change of a watched file and a change of the configuration drop the saved
answer, because an include file can change without an edit of the buffer. A run that is already running when such an event comes finishes with the files it read before, and its answer can be the old one. The editor shows it until the next edit of the document.

A run stops when nobody needs its answer. An edit of the document replaces its
buffer version: the request for the old version gets `ContentModified` (the
run stops before the expansion and again before the outlines). A
`$/cancelRequest` gives `RequestCancelled` the same way. A stopped run stores
nothing, so the next request for the same version computes the answer. A
stopped run that is inside the expansion ends when the expansion ends.

A release build took about 9 ms for a buffer of 315 symbols that includes 15
files of 600 lines each: 1 ms for the expansion and 8 ms for the outlines, with
the symbol limit reached at the 11th file. The machine ran other work at the
same time.

## Language server

- The server advertises `documentSymbolProvider: true`.
- When the client sets `hierarchicalDocumentSymbolSupport`, the answer is
  `DocumentSymbol[]`. When it does not, the answer is a flat
  `SymbolInformation[]` with `containerName`.
- Ranges use the negotiated position encoding.
- A document that is not open gives `null`. A contained panic gives the error
  `RequestFailed`, never an empty list. Bad params give `InvalidParams`.
- With the include files on, a symbol of an include file has no place in the
  buffer. A client that takes a tree gets, for such a symbol, the range of the
  name in the `{file.i}` reference above it (`range` and `selectionRange`).
  Choosing it puts the cursor on that name, and Go to Definition there opens the
  include file. A client that takes a flat list gets the same range, in the
  URI of the document that it asked about: a flat list is the list of symbols
  of one document, and clients of that kind often use only the range. A flat
  list does not give the location of a declaration in the include file. A symbol
  that has no reference above it (the include is in no symbol of the buffer) is
  at the start of the document.
- All the symbols of one include share the range of its reference. In VS Code,
  a cursor on the file name of the reference is inside every one of them, so the
  breadcrumbs show the last nested declaration of the include.
- The server reads the include files through the open buffers, so an unsaved
  edit of an include file shows in the outline of a file that includes it. The
  editor does not ask again for the outline of the including file when the
  include changes. The new outline shows at the next edit of the including file,
  or when it opens again. The server drops its saved answer when it is told that
  a file changed: a save, an open or a close of a document, and a change of a
  `.i` file or of the configuration that the client watches. The VS Code
  extension watches `**/*.i` in the workspace. An include with another
  extension, or outside the workspace folders, changes the outline at the next
  edit of the including file.
- The outline needs no setup: it works without an `oxabl.toml` and does not
  follow `[workspace.features]`. Without an `oxabl.toml`, the server turns
  diagnostics and formatting off, so the outline is all that an editor gets.
  The include files need the include path of an `oxabl.toml`: without one,
  every include shows `not found`.

## Tests

- `crates/oxabl_pipeline/tests/outline.rs` tests the raw outline.
- `crates/oxabl_pipeline/tests/outline_includes.rs` tests the include symbols
  with the real preprocessor and file system: nesting, each marker, an `&IF`
  that does not compile text, an include read twice, an include in the middle of
  a statement, an argument in a name, and the symbol limit.
- `crates/oxabl_lsp/tests/document_symbol_e2e.rs` tests the answer of the server
  for one file.
- `crates/oxabl/tests/outline_lsp.rs` tests deep outlines through the real
  `oxabl lsp` and its daemon.
- `crates/oxabl/tests/outline_includes_lsp.rs` tests the include symbols through
  the same: the temp-table and the procedure of an include, nesting, where a
  symbol goes in a tree and in a flat list, a marker, the setting, an `&IF`, a
  cycle, an unsaved include, a deep chain of includes, one include read 600
  times, an include over the limit, an `&IF` that the preprocessor only guessed,
  an include written through `{&name}`, a shim that splits a statement, a save,
  an edit that stops a request and a cancelled request.
- The VS Code scenario `outline` (`clients/vscode/test/e2e`) checks the include
  symbols in the Outline view, a marker, the cursor on the reference, Go to
  Definition on it, and the setting.

| `OutlineKind` | LSP `SymbolKind` |
| --- | --- |
| `Class` | `CLASS` |
| `Interface` | `INTERFACE` |
| `Procedure`, `Function` | `FUNCTION` |
| `Method` | `METHOD` |
| `Constructor`, `Destructor` | `CONSTRUCTOR` |
| `Property` | `PROPERTY` |
| `Event`, `Trigger` | `EVENT` |
| `Variable`, `Parameter` | `VARIABLE` |
| `TempTable`, `Dataset` | `STRUCT` |
| `Field` | `FIELD` |
| `Buffer`, `DataSource`, `Query`, `Stream` | `OBJECT` |
| `Frame`, `Widget` | `OBJECT` |
| `Include` | `FILE` |
| `PreprocessorDefine` | `CONSTANT` |
| `Label` | `NAMESPACE` |
| `Enum` | `ENUM` |
| `EnumMember` | `ENUM_MEMBER` |
