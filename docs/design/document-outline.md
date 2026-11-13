# Document outline

The document outline is the tree of declarations in one file. An editor shows
it in its Outline view, in the breadcrumbs, in "Go to Symbol in File"
(`Ctrl+Shift+O`) and in sticky scroll.

- `oxabl_pipeline::outline(source)` makes the tree.
- `oxabl lsp` sends it as the answer to `textDocument/documentSymbol`.
- The VS Code extension needs no code for it: `vscode-languageclient`
  registers the provider when the server advertises `documentSymbolProvider`.

## Source

The outline reads the **raw** buffer, like the formatter: no preprocessing and
no include expansion. Every span is a byte range of the buffer as written, so
a client maps it with no source map. This has three results:

- Declarations inside an include file do not show. The `{file.i}` reference
  shows as an `Include` symbol.
- Both branches of `&IF ... &ELSE` show.
- A file that does not parse cleanly shows the declarations that the parser
  recovered. A syntax error in one procedure does not remove the other
  procedures.

## Symbols

| Construct | Kind | Name | Detail |
| --- | --- | --- | --- |
| `CLASS` | `Class` | The name as written, `A.B.C` | `ABSTRACT` or `FINAL` when written |
| `INTERFACE` | `Interface` | The name as written | none |
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
| A statement-level `{file.i}`. `~{file.i}` has an escaped brace and gives no symbol | `Include` | The file name as written | none |
| `&SCOPED-DEFINE`, `&GLOBAL-DEFINE` | `PreprocessorDefine` | The macro name | `SCOPED` or `GLOBAL` |
| A labelled block, `main-loop: DO:` | `Label` | The label | none |

A constructor or a destructor that no class contains (a broken file) has the
name `CONSTRUCTOR` or `DESTRUCTOR`.

A detail is `None` when the table says "none". Keywords in a detail are in
upper case. Type names and other names are as written.

An `ON` statement with no trigger action is not a symbol. This covers a key
remap (`ON F1 HELP.`), `REVERT` and `PERSISTENT RUN`. A function
prototype (`FORWARD`, `IN`, `MAP TO`) is not a symbol when the same file
defines a function with the same name (case-insensitive). Otherwise it shows,
with its parameters as children.

## Spans

- `span` is the whole declaration, including its `END ...` and the terminator.
  Exception: each name of a `VAR` with several names spans `name [= initial]`,
  so the symbols do not overlap.
- `selection_span` is the name. A constructor or a destructor has no name: its
  `selection_span` is the first keyword of the statement. A trigger's
  `selection_span` is the event phrase.
- `span` contains `selection_span`, and `selection_span` is never empty.

## Hierarchy

These symbols contain other symbols: `Class`, `Interface`, `Procedure`,
`Function`, `Method`, `Constructor`, `Destructor`, `Trigger` (an `ON` block),
`Property` with a computed accessor (the `SET` parameters and the declarations
of its `GET` and `SET` bodies), `Label`, `TempTable` (its fields), `Event`
(its parameters) and a function prototype (its parameters). A property with
only `GET.` or `SET.` has no children.

The walker goes into the body of every block statement: `DO`, `REPEAT`,
`FOR EACH`, both branches of `IF`, every `CASE` branch, `CATCH`, `FINALLY`,
every `&IF` branch, and `ON` blocks. It moves the declarations that it finds
up to the nearest symbol that contains symbols, or to the top level. Children
are in source order.

A symbol has at most 32 symbols above it. A symbol that holds symbols and has
32 above it gives no symbol of its own, and what it holds goes to the 32nd
symbol. The tree gets no deeper, and no symbol that it holds is lost.

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

## Language server

- The server advertises `documentSymbolProvider: true`.
- When the client sets `hierarchicalDocumentSymbolSupport`, the answer is
  `DocumentSymbol[]`. When it does not, the answer is a flat
  `SymbolInformation[]` with `containerName`.
- Ranges use the negotiated position encoding.
- A document that is not open gives `null`. A contained panic gives the error
  `RequestFailed`, never an empty list. Bad params give `InvalidParams`.

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
