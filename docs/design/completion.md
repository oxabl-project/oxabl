# Code completion

`oxabl lsp` answers `textDocument/completion` and `completionItem/resolve`. The
answer depends on what is written before the cursor: members after `obj:`,
tables after `FIND FIRST`, fields after `Customer.`, programs after `RUN`,
classes after `NEW`, include files after `{`, and statements at the start of a
statement. The steps that build it are in `completion-steps.md`.

## The rule that decides every case

Completion fails closed. When oxabl does not know the context at the cursor,
it gives no list (`null`). It never gives a list that looks complete but is
wrong. A list that can miss items says so with `isIncomplete: true`. A name
that oxabl cannot prove is valid at the cursor is not in the list.

The editor can still show its own list when the server answers `null` (VS Code
shows the words of the open files). That fallback is the editor's own, as for
document highlights.

## Terms

| Term | Meaning |
| --- | --- |
| Current text | The text of the document at the request, from the open buffer. |
| Base | One analysis of a compilation unit that completion reads: the navigation model (`oxabl_pipeline::NavigationModel`: scope tree, symbols, references), its expansion, and the text it was made from. |
| Fresh base | A base made from the current text. |
| Stale base | A base made from an older text of the same document. |
| Word | The name, keyword or path that the cursor is in or at the end of. Completion replaces it. |
| Statement window | The tokens of the current text from the start of the statement at the cursor up to the word. |
| Context | What the window says the cursor needs: one row of the table in "Find the context". |
| Collector | The code that makes the items of one context. |
| Class source | The interface that gives the members of a class (see "The class source"). |

## A request, step by step

1. The language server finds the open document, the byte offset of the cursor
   and the selected include context (`docs/design/include-context.md`).
2. The daemon takes the tokens of the current text (below) and finds the word
   and the statement window. A cursor in a comment, or in a string outside a `{...}`
   reference, gives `null`.
3. The daemon reads the window and finds the context. No context gives `null`.
4. The daemon takes the newest base of the document (fresh, else stale, else
   it makes one). It maps the cursor into the base. The programs of `RUN` and
   the files of `{` need no base: when no base can be made (the
   preprocessing of the text failed), they still answer, with
   `isIncomplete: true`. Every other context then gives `null`.
5. The collector of the context makes the items from the base, the schema, the
   workspace index and the file system.
6. The daemon filters the items by the word, ranks them, cuts the list at the
   limit and sets `isIncomplete`.
7. The language server renders the items in the negotiated position encoding.
8. When the base was stale, the daemon starts a fresh base in the background
   for the next request.

`completionItem/resolve` adds documentation and a signature to one item later
(see "Resolve").

## Where the work runs

| Crate | What it does for completion |
| --- | --- |
| `oxabl_lsp` | The capability, the request and resolve handlers, the gate that drops replaced requests, and the rendering of items, ranges and edits in the position encoding. It does no analysis. |
| `oxabl_daemon` | The bases of each document (`completion` module), the background refresh, the class listing of the include paths, and `compute_completion` and `compute_resolve` on a worker thread. It is the only crate that knows salsa, but completion holds no salsa handle (see "No snapshot"). |
| `oxabl_pipeline` | The engine (`completion` module): the context finder, the collectors, keyword tables, keyword case, matching, ranking and the limit. It works on a base and the current text, with no salsa. |
| `oxabl_semantic` | The span of each scope, and the class source with `class_surface`, the members a receiver reaches. |
| `oxabl_workspace` | `FileSystem::list_dir`, to list the entries of a directory on the include path. |
| `oxabl_preprocessor` | Each `&GLOBAL-DEFINE`, `&SCOPED-DEFINE` and `&UNDEFINE` it carries out and the arguments of each include expansion, for `{&` and `{n}` completion; `oxabl_analyze::ExpandedFile` carries them, placed in the expanded text. Only the expansions of completion record them (`with_define_record`), so other runs do not pay for them. |
| `oxabl_codegen` | The static snippets table of the server, from `clients/vscode/snippets/abl.json`. |
| `clients/vscode` | The extension stops its own snippet provider when the server has a completion provider. |

The engine is in `oxabl_pipeline` so that `oxabl_wasm` can use it later with
its own base.

## The base

### Why completion does not analyse the current text

A completion request comes after each key. The analysis of a unit (lex, parse,
declare and resolve) takes about 100 ms for a 10,000-line file in a release
build (`cargo bench -p oxabl_pipeline --bench semantic_tokens_bench`,
`semantic_tokens/analysis`: 99 ms on a loaded machine; lex 2 ms, parse 2 ms,
semantic passes 90 ms). So the request cannot wait for an analysis of the text
that the last key made. It reads the newest base that exists and maps the
cursor into it. The current text is lexed only where it differs from the base's
text (see "Word and window").

### Fresh, stale and cold

The daemon keeps, for each open document and include context, the newest base
it made (`oxabl_daemon::completion::Bases`): the model, the expansion, the
text it read and the configuration it ran under.

| The newest base is | Then |
| --- | --- |
| Fresh (its text is the current text) | Use it. |
| Stale | Use it with the cursor mapped (below). Set `isIncomplete: true`. Start a refresh. |
| Missing (first request after the document opened) | Make a fresh base now, on the request's worker thread. This request can take longer than the budget. |

A refresh analyses the current text on a background thread and stores the
result as the new base. One refresh runs for each document at a time. A
refresh that ends after a newer text has arrived still stores its base,
because that base is newer than the one it replaces. A refresh that ends after
its document closed, or after a configuration change, keeps nothing: it only
fills a slot that is still there.

### No snapshot

Salsa checks for cancellation only at the boundary of a query. The analysis of
a unit has no boundary inside it, so a database snapshot held through one
makes the next `didChange` of any buffer wait until the analysis ends, and the
server answers nothing in that time. So no base is made under a snapshot, and
no request takes one. The language server copies what a request needs on its
main thread (the text of the document from its document store, the
configuration, the generation and schema revision, and the open buffers for an
include in a root context), and the daemon makes a base from these with the
pipeline's own run index (`LintPipeline::navigation_model`), as the inlay
hints do. A base is therefore not the salsa memo of `navigation_model`: each
open document keeps its own model, and its includes are read from disk, as
diagnostics read them.

This costs memory. Each open document keeps two bases, the newest and the one
before it, which a resolve of an older list reads; each is a model and an
expansion of the whole unit. A review measured about 45 MB of resident memory
for each 40,000-line procedure file (debug build, both bases, on top of the
salsa memo that semantic tokens keep). An include in a root context keeps two
analyses of its whole root. The bases go when the document closes or the
configuration or the schema changes.

A refresh claims its slot with a token, the id of the base it makes. Only the
refresh that holds the slot's token ends the claim, so a refresh that ends
after its document closed and opened again leaves the new claim running.

### Map the cursor into a stale base

The daemon compares the base text with the current text: the length of the
common prefix `p` and of the common suffix `s` (the suffix does not overlap the
prefix). The text between them is the changed region.

- A base offset before `p` stays the same. A base offset in the common suffix
  moves by the difference of the two lengths. A base offset in the changed
  region has no place in the current text.
- The cursor is usually in the changed region. Its place in the base is `p`:
  the scope that holds byte `p` of the base holds the cursor.
- A symbol whose declaration is in the changed region of the base is dropped,
  because the current text may no longer declare it. A declaration that the
  current text adds in the changed region is not in the base. So a stale base
  always gives `isIncomplete: true`.
- When the changed region of the base or of the current text holds a token
  that opens or closes a scope (`PROCEDURE`, `FUNCTION`, `METHOD`,
  `CONSTRUCTOR`, `DESTRUCTOR`, `CLASS`, `INTERFACE`, `ENUM`, `END`, `CATCH`,
  `FINALLY`, `ON`, `GET`, `SET`), the scope at `p` may not be the scope of the
  cursor. Then no names of the unit are offered, the statement keywords are
  the ones of the scope kind at `p`, and the list has `isIncomplete: true`.
  Items that do not depend on the scope (tables, programs, files, types)
  stay.

The physical offset then goes to the expanded text of the base through the
expansion (`ExpandedFile::expanded_offsets`, below).

### Physical and expanded offsets

The model counts bytes in the expanded text. `ExpandedFile::expanded_offset`
gives one place for a physical byte. An include that the unit reads twice has
two places, so the engine needs all of them:
`ExpandedFile::expanded_offsets(file, real) -> Vec<u32>` gives every place of a
byte of one file id, and each expansion of an include has its own file id, so
the engine asks for each id of the document (`Navigation::file_ids`). A
physical byte with no place, or one in the text of an `&IF` branch that the
preprocessor did not take (`ExpandedFile::skipped_text`), gives `null`: the
analysed text has no code there.

### The scope at the cursor

The declare pass records the span of each scope (`Scope::span`): the span of
its owner statement, which covers the body and the `END` of a block. The file
scope covers the whole text. A block that the parser closed at the end of the
text without `END` covers up to the end of the text: the parser marks it
(`Program::unclosed_blocks`, for each block kind whose `END` the end of the
text replaced), so that a routine the person is still writing holds the
cursor. Both spans end at `u32::MAX`, so the scope tree needs no text length.
The scope at an offset (`ScopeTree::innermost_at`) is the innermost scope whose
span holds it; completion then steps out of the implicit scope of a
`DEFINE TEMP-TABLE` or a frame.

### A parse error before the cursor

`NavigationModel::recovered` is the first place where the parser recovered.
When it is before the start of the statement window (mapped into the base),
names declared after it can be lost or in the wrong scope. Then the list gets
`isIncomplete: true`. An error in the window or after it does not change the
names that are visible at the cursor, because a name must be declared before
its use.

### Includes

- **An include with a selected root context.** The base is the analysis of
  the root, with the open buffers over the disk, as go to definition makes it
  (`with_navigation`). It is not memoized by salsa, so the daemon keeps it in
  `Bases` under the key of the include and the root, and refreshes it as above.
  When the text of the root, or of another open include the base read,
  changed, the request preprocesses the root again (no analysis) to see that it
  still reads the include, answers from the old base with `isIncomplete: true`,
  and starts a refresh. A base of the same include in another root context is
  dropped when a base is made for the selected one.
  The cursor maps to each place where the root expands the include. When there
  are several, the names are the names visible at every place (same name and
  same kind), so each name is valid in each expansion. When the root no longer
  expands the include, the answer is `null`.
- **An include in fragment mode.** The base is the analysis of the include
  alone (`SourceContext::IncludeFragment`). The names of the includer are not
  known, so every list gets `isIncomplete: true`.
- **A window that reaches the start of the include.** When the window holds
  tokens and reaches the start of the include, the statement may have begun in
  the includer. In a root context, the window goes on into the root only when
  the expanded text before the include ends a statement; if not, the answer is
  `null`. In fragment mode the answer is `null`. An empty window at the start
  of an include is a statement start in both modes.

### What is cached

| Data | Kept | Ends |
| --- | --- | --- |
| The newest base of each open document and include context | `oxabl_daemon::completion::Bases` | Document closed, configuration or schema changed, include context changed |
| The class listing of the include paths | One per session (`ClassListing`) | A watched `.cls` file created or deleted, a change of `oxabl.toml` or of the include paths, or 60 seconds for directories outside the watched folders |
| The bases that resolve may read | The two newest bases of each document, by base id | When a newer base replaces them |
| The directory listings of `RUN` and `{` | `Listings` in `oxabl_daemon::completion`, at most 256 directories | A watched file created, changed or deleted in the directory (`workspace/didChangeWatchedFiles`), a change of the configuration or schema, or 3 seconds. A listing found older than that is freed and read again, never used |
| Keyword tables and snippets | Static data | Never |

## Find the context

### Word and window

A base keeps the tokens of its text (`oxabl_daemon::completion::Base`: lexed
once, in the refresh that makes the base). A request takes its tokens from
them (`oxabl_pipeline::completion::tokens`):

- The text is the base's text: the request lexes nothing.
- The text is newer: the request keeps the tokens of the base that end before
  the line above the first changed byte, and lexes from there
  (`Lexer::starting_at`) up to the end of the changed bytes, the end of the
  word and the two tokens after it. The lexer reads past the end of a token
  only on its own line (and one line more after a `~` at a line end), so
  these tokens are the ones a full lex gives. A `&SCOPED-DEFINE` or
  `&GLOBAL-DEFINE` that no `PreprocEnd` has closed moves the start back to it,
  because a new lexer does not know its mode.
- No base: the request lexes the whole text (`oxabl_lexer::tokenize`). The
  first request of a document does this once, to ask whether it needs a base.

The tokens of a request are always the first tokens of a full lex of the
text, so comments and strings are right. `completion_tokens` (a test of
`oxabl_pipeline`) compares the answers with the tokens of the base and with a
full lex, and counts the bytes that a request lexes; the unit test of
`tokens.rs` tries an edit of many kinds at each byte of texts that depend on
their context.

- The three `{` contexts (below) are checked first, also inside a string,
  because the preprocessor expands `{1}` and `{&name}` in strings. Then a
  cursor inside a `Comment` or `StringLiteral` token gives `null`.
- A `.` right before the cursor (or before the word) that is joined to a name
  on its left (an identifier, or a keyword that is not reserved) is a
  qualifier: the person is typing `Customer.` and a field. Any other `.`
  before whitespace, a comment or the end of the text ends a statement.
- A cursor right after a `.` that ends a statement, or right after a `:` that
  follows a keyword (`DO:`, `REPEAT:`, `ELSE DO:`), with no space between,
  gives `null`: the person ended a statement and did not ask for the next
  one. A `:` joined to a name goes to the "Members" row.
- The word is the identifier-like text that ends at the cursor: letters,
  digits, `-`, `_`, `#`, `$`, `%`. In a path context (`RUN`, `{`) it also
  holds `/`, `\` and `.`. The replace range is the word. When the client
  supports `insertReplaceSupport`, the replace range runs to the end of the
  word after the cursor and the insert range ends at the cursor.
- The window starts after the nearest statement end before the word:
  - a `.` that ends a statement (a `Period` token that is not joined to a name
    on its left and right, as in `Customer.Name`),
  - a `:` that ends a block header: a `Colon` token that is followed by
    whitespace, a comment or the end of the text and is not right before the
    cursor (`DO:`, `FOR EACH Customer:`, `PROCEDURE p:`). A member access has
    no space after its `:`,
  - `THEN`, `ELSE` and `OTHERWISE`, which start a nested statement,
  - a preprocessor directive line (`&IF ... &THEN`, `&ELSE`, `&ENDIF`,
    `&GLOBAL-DEFINE`, ...),
  - the start of the text.
- A window that holds an include or macro reference (`{...}`, `{&name}`) gives
  `null`: oxabl cannot read the statement as the compiler sees it.

A `:` joined to a name on its left (`FOR EACH Customer:`) can be a block colon
or a member access. The finder does not decide from the text. The collector
decides from what the name resolves to (see "Members").

### The contexts

The finder first looks at the text before the cursor for an unclosed `{`
(the three `{` rows), because an include reference is not a statement. Then
it reads the window from its end. The first row that matches wins.

| Cursor after (window ends with) | Context | When oxabl cannot find it |
| --- | --- | --- |
| `{&` | Preprocessor names | Always found |
| `{` followed by digits, in an include | Include arguments | `null` outside an include |
| An unclosed `{` (the text after it has no `}` before the cursor) | Include files | Always found |
| A receiver joined to `:` (`obj:`, `obj:Get():`, `THIS-OBJECT:`, `SUPER:`, `Cls:`, `NEW Cls():`, `CAST(x, Cls):`) | Members | `null` |
| `FIND [FIRST\|LAST\|NEXT\|PREV\|CURRENT]`, `FOR [EACH\|FIRST\|LAST]`, `, EACH`/`, FIRST`/`, LAST` in a `FOR` or `OPEN QUERY` header, `PRESELECT EACH`, `DO FOR`, `REPEAT FOR`, `DEFINE ... BUFFER b FOR [TEMP-TABLE]`, `CREATE`, `DELETE`, `RELEASE`, `VALIDATE`, `BUFFER-COPY`, `BUFFER-COPY a TO`, `BUFFER-COMPARE`, `CAN-FIND([FIRST\|LAST]`, `AVAILABLE`/`AVAIL`, `OF`, `LIKE` | Records | Always found |
| A name joined to `.` (`Customer.`, `b.`, `ttLine.`, `shop.`, `shop.Customer.`) in an expression position | Qualified name: fields of a record, or tables of a database. Right after a record keyword of the row above (except `LIKE`), only a database name qualifies: `FIND FIRST shop.` gives the tables of `shop`, and `RELEASE Customer.` ends a statement | `null` |
| `WHERE`, or a `WHERE` before the cursor in the same `FOR`, `FIND`, `CAN-FIND`, `OPEN QUERY` or `PRESELECT` with an operator or `AND`/`OR`/`NOT`/`(` at the end of the window | Fields of the records of the statement, then values | Values only |
| `RUN` (also `RUN ... ` while the target is still being typed) | Programs and internal procedures | Always found |
| `NEW`, `AS`, `AS CLASS`, `RETURNS`, `INHERITS`, `IMPLEMENTS` and a `,` after it, `CAST(x,`, `TYPE-OF(x,`, `USING`, `CATCH e AS`, the return type of a `METHOD` header | Types | Always found |
| `DEFINE` and its modifiers (`NEW`, `GLOBAL`, `SHARED`, `PRIVATE`, `PROTECTED`, `PUBLIC`, `PACKAGE-PRIVATE`, `PACKAGE-PROTECTED`, `STATIC`, `ABSTRACT`, `OVERRIDE`, `SERIALIZABLE`, `NON-SERIALIZABLE`, `INPUT`, `OUTPUT`, `INPUT-OUTPUT`, `RETURN`) | Keywords after `DEFINE` | Always found |
| Nothing (the word is the first token of the window) | Statement start | Always found |
| `=`, an operator, `(`, `,` in a call, `IF`, `RETURN`, `WHEN`, `THEN` of an expression, `AND`, `OR`, `NOT` | Values (an expression) | Always found |
| Anything else | None | `null` |

Keywords are compared by `Kind`, so `DEF VAR`, `defi buf` and `FIND FIRST`
in any case and any legal abbreviation match. The finder never compares
keyword text.

## The collectors

Each collector says what it gives, where it reads it, and what makes its list
incomplete or `null`.

### Statement start

Statement keywords that are valid in the scope at the cursor, and the names
that can start a statement.

| Scope kind at the cursor | Statement keywords |
| --- | --- |
| File scope of a procedure file (`.p`, `.w`, `.i` root) | Every statement of the keyword documentation index (`keyword_docs.rs`, entries of kind `Statement`), by its first word, except the class-only ones (`CLASS`, `INTERFACE`, `ENUM`, `METHOD`, `CONSTRUCTOR`, `DESTRUCTOR`) and `END`, which closes no block there |
| File scope of a class file before the `CLASS`, `INTERFACE` or `ENUM` statement | `USING`, `CLASS`, `INTERFACE`, `ENUM`, `BLOCK-LEVEL`, `ROUTINE-LEVEL` |
| Class body | `DEFINE`, `METHOD`, `CONSTRUCTOR`, `DESTRUCTOR`, `ON`, `END` |
| Interface body | `DEFINE`, `METHOD`, `END` |
| Enum body | `DEFINE`, `END` |
| A routine body (procedure, function, method, constructor, destructor, property accessor, trigger, `DO`, `FOR`, `REPEAT`, `CATCH`, `FINALLY`) | As the file scope of a procedure file, without `PROCEDURE`, `FUNCTION`, `USING`, `BLOCK-LEVEL` and `ROUTINE-LEVEL` |
| The statement after `THEN`, `ELSE` or `OTHERWISE` at the file scope of a procedure file | As a routine body: it is the branch of another statement |

The names at a statement start are the values that a statement can begin with
(an assignment target, a method call, a record for `Customer.Name = ...`):
variables, parameters, properties, buffers, temp-tables and tables, and
`THIS-OBJECT` and `SUPER` in a class. They come from the "Values" collector.

`WHEN` and `OTHERWISE` are not offered: the base does not say that a `CASE`
block holds the cursor, and a keyword that is not valid there is a wrong item.

Server snippets (see "Snippets") are offered at a statement start in the scope
kinds where their body is valid.

### Keywords after `DEFINE`

The second words of the `DEFINE ...` entries of the keyword documentation
index (for example `VARIABLE`, `BUFFER`, `TEMP-TABLE`, `PARAMETER`,
`DATASET`, `QUERY`, `STREAM`, `FRAME`, `BROWSE`, `BUTTON`, `EVENT`,
`PROPERTY`, `DATA-SOURCE`, `IMAGE`, `MENU`, `SUB-MENU`, `RECTANGLE`,
`WORK-TABLE`), and the modifiers that may still come, in their legal order
(`NEW` only first, `GLOBAL` only after `NEW`, an access modifier only in a
class body, `INPUT` and `OUTPUT` only for `PARAMETER`). The scope kind
filters the second words: `ENUM` only in an enum body; `PROPERTY` only in a
class or interface body; `PARAMETER` only in a procedure, a trigger or the
file scope of a procedure file. A block counts as the routine that holds it.
In a method, constructor, destructor or property accessor no modifier is
offered (a class has no shared variable, and a method has no `DEFINE
PARAMETER`). In a user-defined function `NEW` and `SHARED` are offered, and
`INPUT`, `OUTPUT`, `RETURN` and `PARAMETER` are not.

### Values

The names that are valid in an expression at the cursor:

- Every symbol of the `Values` namespace in the scope at the cursor and its
  ancestors (variables, parameters, properties, class data members), and the
  buffers, temp-tables and default buffers of tables (as records, for
  `Customer.`).
- A name declared in the same routine is valid only when its declaration ends
  before the cursor: ABL needs a definition before its use. Class members,
  internal procedures, and functions declared before the cursor or with a
  `FORWARD` before the cursor, are valid everywhere in their scope. The symbol
  table merges a `FORWARD` with the definition, so for a function the engine
  also looks for a `FUNCTION <name>` statement before the window in the
  current text (only the document's own text: a `FORWARD` in the root before
  an include is not seen, and the function is then missing).
- A name the cursor cannot see (declared after it, or in the text a stale base
  changed) does not hide a name of an outer scope that it can see.
- In a static method or a static property accessor, only the static members of
  the class are offered.
- In a class body: the members that the class inherits, from the class source
  (role `ThisObject`). Until step 5 there are none, and every list in a class
  has `isIncomplete: true`.
- Built-in functions (the `Function` entries of the keyword documentation
  index, which name each function once by its full name, with the title as the
  detail) and user functions, and `THIS-OBJECT`, `SUPER` (when the class names
  a parent) and `NEW` where they are valid.
- A name keeps the spelling of its declaration: the label is the text of its
  name in the analysed text, not the folded name of the symbol table.
- The label and kind identify a name. With several places of the cursor (an
  include that the root reads twice), a name is offered only when each place
  has a name with the same label (without case) and kind.
- Synthesized symbols (schema fields, inherited members that the resolve pass
  made at a use) are never taken from the symbol table: they are not
  declarations, and their name span is a use site.

`isIncomplete: true` when the class of the scope has an incomplete surface,
in fragment mode, and with a stale base.

### Records

Buffers, temp-tables and tables, as the row of the statement allows:

| After | Items |
| --- | --- |
| `DEFINE BUFFER b FOR` | Tables and temp-tables (not buffers) |
| `DEFINE BUFFER b FOR TEMP-TABLE` | Temp-tables only |
| `FIND`, `FOR EACH`, `CREATE`, `DELETE`, `RELEASE`, `AVAILABLE`, `CAN-FIND`, `BUFFER-COPY`, `OF`, ... | Buffers, temp-tables and tables (a table is its default buffer) |
| `LIKE` | Tables, temp-tables and buffers, for `LIKE t.field`; after `LIKE t.` the field context |

- Buffers and temp-tables come from the `Buffers` namespace of the scope at
  the cursor and its ancestors, with the rule of "Values" for declarations
  before the cursor. A class body adds the inherited buffers and temp-tables
  of the class source.
- Tables come from the loaded schema (`Schema::tables`). A table of the
  OpenEdge metaschema (`_file`, `_field`, ...) is offered only when the word
  starts with `_`.
- No schema configured or loaded: only buffers and temp-tables, with
  `isIncomplete: true`.
- With more than one database, the database names come too (kind `Module`,
  detail `database`), for `db.` (see "Database-qualified names").

### Qualified names and fields

For `q.` (and `q.word`) the collector looks at `q` in this order:

1. A buffer, temp-table or default buffer of a table in the scope at the
   cursor: the fields of its table.
2. A database logical name (see "Database-qualified names"): the tables of
   that database.
3. `db.t` with a database `db` and its table `t`: the fields of `t`.
4. Else `null`. When `q` is both a record and a database, the answer is `null`.

A name that is not a name (`1.`, a number) is no qualifier: the `.` ends a
statement. Right after a record keyword, only step 2 applies. That case reads
the schema only, so it needs no base, and a `.` typed there starts no
analysis.

Fields:

- A schema table: every field of the `.df` table, with its type and format in
  the detail (see "Items").
- A temp-table: the fields its `LIKE` clause gives, then its own fields from
  its `TempTable` scope, in the order of `EffectiveTable::declare`. A `LIKE`
  of a schema table gives the schema fields, with format; a `LIKE` of a
  temp-table follows that temp-table (at most 8 levels). A `LIKE` source that
  oxabl does not know (the declaration has a `LIKE` before its first `FIELD`,
  and the analysis resolved none) gives the own fields and
  `isIncomplete: true`.
- A buffer: the fields of its table or temp-table.
- In a `WHERE`, the fields of the records the statement names (the record
  after `FIND ...`, each record after `EACH`, `FIRST` or `LAST` of a `FOR`,
  `PRESELECT` or `OPEN QUERY` header, and after `CAN-FIND(`, with a database
  qualifier when it has one) come first. The values follow. A field is
  inserted without a qualifier only when no other record that the cursor can
  reach has a field of that name: the other records of the statement, and the
  buffers bound in the scope at the cursor and its ancestors. Else the
  compiler finds the bare name ambiguous, so the field is inserted with its
  record (`Order.CustNum`), once for each record of the statement that has
  it. A record whose fields are not all known (an unknown table, a `LIKE`
  that does not resolve) can have any name, so with one in reach every field
  is qualified. A record name that does not resolve adds
  `isIncomplete: true`. On the right of a comparison (`Customer.CustNum >`,
  or `CustNum =` for a field of a record of the statement), the type of that
  field is the expected type of "Match, rank and limit".

### Members

After `receiver:`, the members of the class of the receiver.

**Receiver typing.** The receiver is read from the tokens of the current
text, not from the base, because the statement is not complete. The engine
types these forms and no others:

| Receiver | Class and role |
| --- | --- |
| A name in the scope at the cursor whose declared type is a class (variable, parameter, property, data member) | Its class, role `Instance` |
| `THIS-OBJECT` in a class body | The class of the file, role `ThisObject` |
| `SUPER` in a class body with `INHERITS` | The parent class, role `Super` |
| A type name (`app.Util`, or a simple name that the `USING` imports of the file give) | That class, role `TypeName` (static members only) |
| `NEW Cls(...)` | `Cls`, role `Instance` |
| `CAST(expr, Cls)` | `Cls`, role `Instance` |
| `link:Name` or `link:Name(...)` after one of the above | The type of the member, when it is a class. A method name whose candidates do not all return the same class gives no type. |

Any other receiver gives `null`: a `HANDLE`, a widget, a buffer (`b:BUFFER-FIELD`
is a handle attribute), a system handle (`SESSION:`), an event
(`obj:Done:`), a parenthesized expression, an array element, a name that does
not resolve, and a name declared in the changed region of a stale base. A
name whose type is a buffer or table (`FOR EACH Customer:`) is a block colon,
so the answer is `null` too.

**The surface.** The members come from `class_surface` (below) with the role
of the receiver. Role, access and static rules are the ones of
`class-members.md`: `TypeName` reaches only `STATIC` members; `Instance`,
`ThisObject` and `Super` reach only instance members; outside the class, only
`PUBLIC` members; `Super` reaches only what the class inherits. An enum type
name gives its members as static properties.

- One item for each member name. A method with several overloads is one
  item; its detail gives the first overload and the number of others.
- `isIncomplete: true` when the surface is not complete: an ancestor that was
  not read, a level whose member list is not whole (`members_complete =
  false`), a cycle in the inheritance, and, until #154, every class, because
  each class inherits the members of `Progress.Lang.Object`.

### The class source

Member completion asks one interface for the members of a class. User classes
use it now. The built-in classes of #154 plug into it.

```rust
/// Classes and their own members, by qualified name. The workspace index is
/// one source; the built-in class catalog (#154) is another. A chain asks
/// the workspace first.
pub trait ClassSource {
    /// The class or interface `name`.
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>>;
    /// The members `name` declares itself, in declaration order.
    fn class_members(&self, name: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>>;
    /// The classes, interfaces and enums whose qualified name starts with
    /// `package` (for example `"app.models."`, or `""` for the root), for type
    /// completion. `complete` is false when the source cannot list them all.
    fn classes_in(&self, package: &str) -> ClassList;
}

/// The members `reach` gives for `class`, own and inherited, nearest first.
pub fn class_surface(
    semantic: &Semantic,
    source: &dyn ClassSource,
    class: ClassRef,
    reach: Reach,
) -> ClassSurface;

pub enum ClassRef {
    /// A class declared in the analysed file.
    Local(SymbolId),
    /// A class by qualified name, from a class source.
    Named(IndexName),
}

pub enum Reach { Instance, TypeName, ThisObject, Super }

pub struct ClassSurface {
    pub members: Vec<SurfaceMember>,
    /// False when a member can be missing: see "Members".
    pub complete: bool,
}

pub struct SurfaceMember {
    /// As the declaration or the documentation writes it.
    pub name: String,
    pub kind: SymbolKind,
    pub access: AccessModifier,
    pub is_static: bool,
    /// The nearest class that declares it, qualified.
    pub declared_in: String,
    /// One entry per overload for a method, one entry for any other member.
    pub signatures: Vec<MemberSignature>,
    /// Where the documentation of `completionItem/resolve` comes from.
    pub origin: MemberOrigin,
}

pub struct MemberSignature {
    pub ty: MemberType,
    pub params: Option<Arc<[MemberParam]>>,
}

pub enum MemberOrigin {
    Local(SymbolId),
    Workspace(IndexedFileId),
    Builtin(&'static MemberDoc),
}
```

`WorkspaceIndex` already answers `class` and `class_members`. The daemon adds
`classes_in` from the class listing. `class_surface` walks `INHERITS` and
`IMPLEMENTS` with `lookup_supertype` and the imports of each declaring file,
with a visited set, as `walk_member_surface` and `inherited.rs` do, and
applies the access rules of `class-members.md`. It does not change the
resolve pass.

What #154 must give through `ClassSource` for each built-in class, interface
and enum:

- `class`: the qualified name as documented, the kind, the parent class and
  the interfaces as qualified names that `class` answers again,
  `is_abstract`, `members_complete = true`, and an origin that is not a file
  (`ClassDescriptor` gets `origin: ClassOrigin { File(IndexedFileId), Builtin }`
  in place of `file`).
- `class_members`: each documented method, property and event, with its name
  as documented, kind, access (`PUBLIC` or `PROTECTED`), `is_static`, the type
  (`MemberType::Portable` for a primitive, `Named` with the qualified class
  name, `Untyped` for `VOID`, `Portable(Unknown)` for a generic parameter), the
  parameters of each overload (name, mode, type, extent), and a documentation
  record (`MemberDoc { title, url, summary: Option<&str> }`, the summary from
  #473).
- `classes_in`: the built-in types of a package, for `NEW Progress.Lang.|`.
- Two rules of the language: a class with no `INHERITS` inherits
  `Progress.Lang.Object`, and the package `Progress.Lang` is imported in every
  file. `class_surface` and the type collector apply them when the source
  answers for `Progress.Lang.Object`. Then the surface of a user class can be
  complete.

Signature help (#438) reads the same surface for the parameters.

### Types

Classes, interfaces and enums, and data types where the position allows them.

| After | Items |
| --- | --- |
| `AS` in `DEFINE VARIABLE`, `PARAMETER`, `PROPERTY`, `FIELD`, a routine parameter, `RETURNS` | Data types (`CHARACTER`, `INTEGER`, `INT64`, `DECIMAL`, `LOGICAL`, `DATE`, `DATETIME`, `DATETIME-TZ`, `HANDLE`, `ROWID`, `RECID`, `LONGCHAR`, `MEMPTR`, `RAW`, `COM-HANDLE`; `BLOB` and `CLOB` only for `FIELD`) and all types |
| The return type of a `METHOD` header | Data types, `VOID`, and all types |
| `AS CLASS`, `CAST(x,`, `TYPE-OF(x,`, `CATCH e AS` | All types |
| `NEW` | Classes that are not abstract (enums and interfaces are not offered) |
| `INHERITS` | Classes |
| `IMPLEMENTS`, and `,` after it | Interfaces |
| `USING` | Packages and types, inserted qualified; `pkg.*` for a package |

The types come from:

1. The class of the analysed file.
2. Each `USING pkg.Name` of the file, and the types of each `USING pkg.*`
   package (`classes_in("pkg.")`).
3. The types at the root of the include paths (`classes_in("")`).
4. When the word holds a `.`: the packages and types under that prefix.
5. When the word has at least one character and no `.`: each type in any
   package whose simple name matches the word (from the class listing).
   Accepting one adds a `USING` (below).

Packages are items of kind `Module` that insert `pkg.` and ask the client for
a new list (`command: editor.action.triggerSuggest`).

**The class listing.** `ClassListing` in the daemon is the list of `.cls`
files under each include path entry (and each root-relative entry of the
analysed file), made once on a worker thread, with the qualified name, the
path and the kind of each class. The kind and the declared name come from a
scan of the first tokens of the file up to the `CLASS`, `INTERFACE` or `ENUM`
keyword and its name, with `ABSTRACT`. A file whose declared name is not its
path under the entry (`lib/app/Circle.cls` seen from the entry `.` declares
`app.Circle`, not `lib.app.Circle`) is not listed under that entry, because
the compiler cannot load it by that name. A package is a directory that holds
a listed class at some depth. A file with an include reference or a directive
before the header has no known kind: it is offered in each position, with the
detail `kind not known`. The listing stops at 50,000 files and is then incomplete. While it is
being made, and when it is incomplete, type lists get `isIncomplete: true`.
The same qualified name in two entries counts once (the first entry wins, as
the compiler searches the PROPATH).

Until #154, `Progress.Lang` and the other built-in packages give no items,
so every type list gets `isIncomplete: true`.

### The `USING` edit

A type that a `USING` does not import yet is inserted with its simple name,
and the item has an `additionalTextEdits` entry that adds
`USING pkg.Name.` (the keyword in the project's case).

- The edit goes after the last `USING` statement at the top of the current
  text. With none, it goes at the start of the line of the first statement,
  after a header comment, and ends with a line break.
- No edit, and the simple name, when a `USING pkg.Name` or a `USING pkg.*`
  of the file already imports the type.
- No edit, and the qualified name, when the simple name would mean another
  type (an import or a root type of that name, or the class of the file), when
  the cursor is in an include file, and when an include reference or a
  preprocessor directive comes before the first statement (oxabl cannot tell
  where a `USING` is valid).
- The edit is made from the current text, not from the base, so a stale base
  cannot place it wrong.

### `RUN`

- Internal procedures of the unit (the `Procedures` namespace of the file
  scope), wherever they are declared. The `RUN` statement at the cursor is not
  complete, so the parser reads a `PROCEDURE` right after it as its target
  and loses that one declaration. So when the first token after the cursor is
  `PROCEDURE name`, that name is an item too. No other `PROCEDURE` of the
  text is read: one in an `&IF` branch that is not compiled is not offered.
- Programs on the include path: the directory part of the word is listed in
  each root-relative entry and each include path entry
  (`FileSystem::list_dir`). Files with an extension of `run_extensions`
  (default `p`, `w`) and `.r` are items of kind `File`, with the path relative
  to the entry; directories are items of kind `Folder` that insert `dir/` and
  ask for a new list. A name that two entries give counts once, with the path
  of the first entry in the detail.
- After `RUN x IN h`, `RUN VALUE(`, `PERSISTENT`, `ON SERVER`: no list. The
  target is already written or computed.
- No include path configured: internal procedures only, with
  `isIncomplete: true`. A directory that cannot be listed adds
  `isIncomplete: true`.
- The word is the whole path (`util/he`), and the replace range is the path.
  Names that start with `.` are not offered.
- The cost is bound. A request lists each directory once, and an include
  path entry that is named twice is read once. The daemon keeps each listing
  for three seconds (`Listings` in `oxabl_daemon::completion`, at most 256
  directories, dropped with the bases), so the requests of the next keys do
  not read the disk again. A file event drops the listing of the directory
  of the file and the listings under it at once, so a created or deleted
  file shows in the next answer. A change that no event reports shows after
  three seconds. Each insert frees the listings that expired. The entries are
  filtered by the last part of the word before items are made. At most 1000
  matches of one directory are offered, and they are the best ranked ones
  (the order of the final list, with the name order to break a tie), not the
  first in the alphabet. More make the list incomplete, and the client asks
  again as the word grows.

`list_dir` returns `None` when the file system cannot list (the in-memory
file system of a test can). `None` gives `isIncomplete: true`, never an empty
directory. The one exception is a directory that does not exist: its parent
lists, and holds no entry of that name. That directory is empty, as
`util/` under the entry `lib` when only the entry `.` has it. An entry that
is a file (a procedure library, `lib/app.pl`) cannot be listed, so it makes
the list incomplete.

### Include files, preprocessor names and include arguments

- `{`: the files and directories under each include path entry, by the
  directory part of the word, as for `RUN`. Files with the extension `.i`
  come first; any other file is offered too, because an include can have any
  extension.
- `{&`: the preprocessor names defined at the cursor. The preprocessor records
  each `&GLOBAL-DEFINE`, `&SCOPED-DEFINE` and `&UNDEFINE` it carries out in a
  branch it takes (`PreprocessedFile::defines`) when the run asks for it
  (`Preprocessor::with_define_record`, `ExpandOptions::record_defines`,
  `LintPipeline::with_define_record`; only the bases of completion set it,
  so `check`, `format`, `build` and the lint runs copy no name or value),
  and `ExpandedFile::defines`
  places each in the expanded text, between the text before and after it in
  its file (`PlacedDefine`). A `&SCOPED-DEFINE` holds up to the end of the
  file that writes it; the others hold to the end of the unit. The engine
  replays the directives before the cursor as the preprocessor's table does,
  with a stack for each name: a `&SCOPED-DEFINE` pushes a definition (or
  replaces the one its file made), a `&GLOBAL-DEFINE` sets the global one and
  is ignored over a scoped one, an `&UNDEFINE` removes the newest so that the
  one under it shows again, and a scoped one ends with its file. The names
  are the ones a definition holds at the cursor, with the value as the
  detail. With a stale base, a directive whose name lies in the changed text
  is not replayed; the named arguments the root
  passes to the include (`{x.i &name=value}`, `ExpandedFile::include_arguments`)
  in a root context, unless a directive in the include sets the name again;
  and the names the preprocessor defines itself (`OPSYS`, `WINDOW-SYSTEM`,
  `BATCH-MODE`, `PROCESS-ARCHITECTURE`, `PROVERSION`, `FILE-NAME`,
  `LINE-NUMBER`, `SEQUENCE`). In fragment mode the list has
  `isIncomplete: true`.
- The cursor in a `{...}` reference has no place in the expanded text: the
  expansion replaced the reference. Its place is right after the byte before
  the `{`, or the start of the file's text when the `{` is the first byte.
- Include arguments: in an include with a root context, the items `1` up to
  the number of positional arguments of the reference that includes it (a
  named argument takes a place too, as the compiler numbers them), with
  the argument as the detail; with several references, up to the smallest
  number, and no detail when the references pass different text. `{` alone
  gives these items first and then the include files; `{` and digits gives
  only these items. In fragment mode and outside an include, `{` and digits
  give `null`.

## Keyword case and abbreviations

The case comes from `keyword_case` of `[workspace.style]` of the file's
resolved configuration (`PipelineConfig::style`), the same value the
formatter uses.

| `keyword_case` | Keywords in labels and inserted text |
| --- | --- |
| `Uppercase` | Upper case |
| `Lowercase` | Lower case |
| `Preserve` | The case of the word when it holds a letter (all lower case gives lower case, else upper case); with no letter, the case of the nearest keyword before the cursor in the current text; with none, upper case |

The same case applies to data types, built-in functions, the `USING` of the
edit and the keywords of snippets. Names (variables, tables, fields, members,
classes, files) keep the spelling of their declaration, of the `.df` or of the
disk.

Completion always inserts the full keyword (`DEFINE`, `VARIABLE`). A typed
abbreviation is a prefix of its keyword, so `def` and `defi` match `DEFINE`.
The finder reads abbreviations through the lexer (`Kind::Define` for `def`).
`keyword_abbreviation = "AbbreviateEverything"` does not abbreviate: the
formatter does not abbreviate in that mode either.

## Database-qualified names

The schema model has no database names. As the preprocessor does when
`[workspace.preprocessor].databases` is absent, the logical name of a database
is the file name of its `.df` without the extension
(`schema/shop.df` gives `shop`). The database of a table is the `.df` that
defines it (`Schema::table_source`).

- With more than one `.df`, a table item names its database in the detail
  (`Database table, shop`), and `db.` gives the tables of that database
  (with the name `db` matched without case).
- `db.Customer.` gives the fields of `Customer`.
- Items are inserted without the database name. A table that two `.df` files
  define is one table in the model; its item names the first database.

## Match, rank and limit

**Match.** The word matches a label when its characters appear in the label
in the same order, without case, and its first character is the first
character of the label or of a part of it (after `-`, `_`, `.`, `/`, or an
upper case letter after a lower case one). So `cusnu` and `cn` match
`CustNum`, `cu` matches `bCust` (the part `Cust`), and `us` does not. The match is monotone: a label that matches a word also matches
each prefix of it, so the client can filter a list the server gave without
losing items. A path matches on its last part.

**Rank.** `sortText` orders the items by these keys, in order:

1. Where the match starts: a match from the start of the label comes before
   one that starts at a later part of it (`c` in `iCount`), whatever the fit.
   A fitting name in the middle of a word does not go above a name whose
   start the person typed.
2. Fit: an item whose type fits the expected type comes first. The expected
   type is known on the right of an assignment (`x = |` reads the declared
   type of `x`), of a comparison in a `WHERE` (`Customer.CustNum = |`), and
   after `RETURN` in a function or method with a return type. A type fits when
   `oxabl_semantic::coercion::assignable` says so. In a `WHERE`, the fields of
   the statement's records come next.
3. Match quality: the same text (without case), then a prefix, then a match at
   the starts of parts, then any other match. A field that a `WHERE` inserts
   with its record (`Order.CustNum`) is matched and ranked by the field name
   first, and by the whole label only when the field name does not match.
4. Nearness: a name of the innermost scope, then each outer scope, then
   members the class inherits, then keywords and snippets, then schema tables
   and fields, then workspace types and programs.
5. The label, without case.

**Limit.** At most 200 items. A longer list is cut after ranking and gets
`isIncomplete: true`, so the client asks again as the person types.

**`isIncomplete`.** The protocol has one flag. The server sets it when the
list can change as the person types or can miss items: a stale base, a cut
list, a parse error before the window, fragment mode, an incomplete class
surface, an incomplete class listing, no schema or no include path, a
directory that cannot be listed. The client then asks again at each key
instead of filtering a list it holds as whole.

## The language server

**Capability.** `completionProvider` with `triggerCharacters` `:`, `.`, `{`,
`&`, `/` and `resolveProvider: true`. It is advertised for every client. A
request that a trigger character made (`context.triggerKind` 2) gets a list
only in a context that character starts: `:` members, `.` a qualified name,
`{` include files and arguments, `&` preprocessor names, `/` the path of
`RUN` or of an include. Any other gets `null`, so a `.` that ends a statement
after a record keyword (`RELEASE Customer.`) or a `/` that starts a comment
pops no list.

A `.` after a name in any other statement (`DISPLAY Customer.`) can be the
end of the statement or a qualifier, and the server cannot tell them apart:
it gives the fields. In VS Code the list then opens with its first row
selected, and Enter would write a field name in place of a new line. So the
extension sets `editor.suggest.selectionMode` to `whenQuickSuggestion` for
`[abl]` (`configurationDefaults`): a list that a trigger character opens has
no row selected, and Enter writes a new line. Tab, the arrow keys and a
click still choose an item. A person can set it back in the settings.

The daemon finds the context before it reads a base. A position with no
context, or one whose collector does not exist yet, answers `null` and makes
no base and starts no refresh. A context that reads no analysis (the tables
of a database right after a record keyword, the files after `{` in a file
that is not an include) answers with no base too. An operator that needs a left operand and has
none (`/` or `*` at a statement start, the first character of a comment) is
no context.

**Request.** `textDocument/completion` runs on a worker thread. The requests
of one document run one at a time. A request that a newer one replaced before
it started, or that the client cancelled with `$/cancelRequest`, answers
`RequestCanceled` (-32800) and computes nothing (as `HoverGate` does). A
request holds no database snapshot (see "No snapshot"), so an edit neither
waits for it nor cancels it. A failed analysis answers an error, never an
empty list.

**Answer.** `null` when there is no context. Otherwise a `CompletionList`
with `isIncomplete` and `items`, also when `items` is empty.

**Items.**

| Field | Value |
| --- | --- |
| `label` | The name as declared, or the keyword in the project's case; a path for a program or an include file |
| `kind` | See below |
| `detail` | See below |
| `sortText` | The rank, as fixed-width digits |
| `filterText` | The label. For a path it is the whole path, because the client filters on the text of the replace range |
| `textEdit` | Replaces the word with the inserted text (`InsertReplaceEdit` when the client supports it) |
| `insertTextFormat` | `Snippet` only for a snippet item or a call item with its arguments (`docs/design/call-fill.md`), and only when the client supports snippets |
| `additionalTextEdits` | The `USING` edit |
| `command` | `editor.action.triggerSuggest` for a package or directory item; `editor.action.triggerParameterHints` for a call item that opens its parentheses; `oxabl.fillCall` for a call item whose arguments come after the insert (`docs/design/call-fill.md`) |
| `data` | What resolve needs (below) |

| Item | `kind` | `detail` |
| --- | --- | --- |
| Variable | `Variable` | The type: `CHARACTER`, `INTEGER EXTENT 3`, `app.Circle` |
| Parameter | `Variable` | The mode and type: `INPUT CHARACTER` |
| Property, class data member | `Property` | The type |
| Method | `Method` | `METHOD [STATIC] <return type or VOID> (<params>)` of the first overload, and ` (+N overloads)` |
| User function | `Function` | `FUNCTION <return type> (<params>)` |
| Internal procedure | `Function` | `PROCEDURE` |
| Built-in function | `Function` | The title in the documentation index (`ENTRY function`) |
| Database table | `Struct` | `Database table`, or `Database table, <db>` with more than one database |
| Temp-table | `Struct` | `Temp-table` |
| Buffer | `Struct` | `Buffer for <table>` |
| Schema field | `Field` | The type, `EXTENT n` when it has one, and `FORMAT "<format>"` when it has one: `CHARACTER FORMAT "x(30)"` |
| Temp-table field | `Field` | The type |
| Class, interface, enum | `Class`, `Interface`, `Enum` | The qualified name |
| Enum member | `EnumMember` | The enum |
| Event | `Event` | `EVENT` |
| Package | `Module` | `package` |
| Database | `Module` | `database` |
| Statement or other keyword | `Keyword` | The title in the documentation index, when it has one |
| Data type | `Keyword` | `data type` |
| Snippet | `Snippet` | The snippet's description |
| Program, include file | `File` | The path of the file |
| Directory | `Folder` | none |
| Preprocessor name, include argument | `Constant` | Its value when known |

Types in a detail are upper case. Names in a detail keep their spelling.

### Resolve

`completionItem/resolve` adds `documentation` (Markdown) and, where it is not
set, `detail`. It never changes the label, the inserted text or the edits.

| Item | Documentation |
| --- | --- |
| A symbol of the unit | The hover of its declaration: `Navigation::hover` at its name in the base the item came from |
| A schema table or field | `schema_hover::table_markdown` and `field_markdown` |
| A workspace class member | Each overload's signature and the class that declares it, as the hover of an inherited member |
| A built-in class member (#154) | The `MemberDoc` title, summary and link |
| A keyword or built-in function | The title and the link of `keyword_docs.rs`, as hover |
| A program or include file | Its path relative to its include path entry |

`data` names what resolve reads: `{ "doc": n }` for a keyword or built-in
function, `{ "base", "symbol" }` for a symbol of the unit,
`{ "base", "table" }` and `{ "base", "table", "field" }` for the schema of the
base, and `{ "path", "entry" }` for a program or include file. A table id
is valid only in the schema it came from, so a table or field resolves
against the schema of its base. When the base is no longer kept, a symbol,
table or field item resolves with no documentation. A keyword and a path do
not need the base.

## Snippets and completion in one list

The extension offers the static snippets of `clients/vscode/snippets/abl.json`
itself today (`docs/design/vscode-extension.md`, "Hand-off to the server's
completion"). VS Code asks its providers in groups by score and stops at the
first group with an item, so it does not merge the server's list with the
extension's. With completion:

- The server serves the same snippets. `cargo run -p oxabl_codegen --
  snippets` writes `crates/oxabl_pipeline/src/completion/snippets.rs` from the
  JSON file, and the CI job `Grammar drift` fails when the committed file is
  not the generated one. The `<<...>>` keyword markers are written in the
  project's keyword case.
- A snippet item is offered at a statement start where its body is valid:
  `method` in a class body; `class` at the file scope of a class file before
  the `CLASS`; `proc` and `function` at the file scope of a procedure file; the
  others in a procedure file scope and in routine bodies. A client without
  snippet support gets none.
- With a loaded schema of at most 30 tables, the table placeholder of `find`
  and `foreach` (`${1:Customer}`) becomes a choice of the tables
  (`${1|Customer,Item,Order|}`), sorted, at its first place; its other places
  become `$1`, which mirrors the choice. With more tables, it keeps its
  default text: a longer choice list is not useful, and the field and table
  completion inside the placeholder gives the real names.
- The extension drops its snippet provider when the server's
  `initializeResult.capabilities.completionProvider` is set, and keeps it
  where no server runs. `keywordCaseOf` and `test/keyword-case.json` stay only
  for that case.

## The budget

A completion in a 10,000-line file answers within 100 ms when a base exists.
The request path lexes what the current text changed in the base's text,
compares the two texts, maps the cursor, collects at most a few thousand candidates and
renders at most 200 items. It never runs a semantic analysis when a base
exists.

It is measured in two ways:

- `cargo bench -p oxabl_pipeline --bench completion_bench` (CodSpeed tracks
  it). The file is the 10,000-line file of `semantic_tokens_bench`. The
  benches are `fresh/<context>` (the base is of the text the request reads)
  and `stale/<context>` (the base is of the text before a one-line insert above
  the cursor: lex the change, compare, map and collect). The benches
  `request_fresh/<context>` and `request_stale/<context>` (`define` and
  `expression`) are the whole request of the daemon: `wants_base`, then the
  answer. Each must stay under
  20 ms in a release build. Each later step adds its contexts (`obj:`).

  | Context | Fresh | Stale |
  | --- | --- | --- |
  | `statement`: a statement start in a procedure (`FI`), keywords, snippets and names | 0.6 ms | 1.0 ms |
  | `define`: after `DEFINE ` | 0.05 ms | 0.5 ms |
  | `expression`: `total = c`, names and built-in functions | 0.25 ms | 1.8 ms |
  | `find_first`: after `FIND FIRST `, buffers, temp-tables and tables | 0.22 ms | 2.7 ms |
  | `qualified`: after `DISPLAY Customer.`, the fields | 0.02 ms | 0.5 ms |
  | `where`: after `WHERE Customer.CustNum > `, fields and values, with the check for a field another record has | 0.7 ms | 1.2 ms |
  | `run`: after `RUN `, 721 procedures and 40 programs | 0.2 ms | 0.7 ms |
  | `run_large`: after `RUN ` in a small file, with an include path entry of 20,000 programs named three times; no listing cache | 8.7 ms | |
  | `run_large_word`: the same after `RUN prog1` | 8.7 ms | |

  Measured on a quiet machine, release build (wall time of criterion; the
  `run_large` rows are bound by the directory listing, not by lexing). Before a
  base kept its tokens, a request lexed the 10,000 lines (2.2 to 4.6 ms fresh),
  and a stale one lexed the base text too, to find a token that opens or
  closes a scope in the changed text (4.6 to 6.2 ms). The whole request of the
  daemon (`request_fresh/define`, `request_stale/define`) went from 5.2 ms and
  9.0 ms to 0.04 ms and 0.8 ms.
- A black-box test over stdio (`crates/oxabl/tests/completion_budget_lsp.rs`)
  shows that the request path does not wait for an analysis: the refresh of a
  source with the marker `OXABL-TEST-HOLD:completion.refresh` waits three
  seconds before it analyses, in test builds only
  (`oxabl_common::hold_if_injected`). A completion after an edit answers
  before the hold ends, from the stale base, with `isIncomplete: true`.
- A second black-box test (`crates/oxabl/tests/completion_review_lsp.rs`)
  uses the marker `OXABL-TEST-HOLD:completion.base@<ms>`: the first base of
  the document waits that long, in test builds only. A `.` at the end of a
  statement answers before the hold ends, and a request that needs a base
  waits for it.

The first request after a document opens makes its base and can take longer.

## Out of scope, and the seams left

- **Built-in classes (#154).** Members of `Progress.Lang.*` and the other
  built-in types, and built-in types in type completion. They plug into
  `ClassSource` (see "The class source"). Until then, member and type lists
  have `isIncomplete: true`.
- **.NET types (#465).** `USING System.* FROM ASSEMBLY` and members of .NET
  types. A .NET catalog is one more `ClassSource`. Until then, a receiver of a
  .NET type gives `null`.
- **Signature help and parameter names (#438).** A procedure, a function or
  a program item writes its call with one placeholder for each parameter
  (`docs/design/call-fill.md`). A method item writes its name only until
  member completion takes the seam described there. Signature help reads
  `SurfaceMember::signatures`.
- **One-line descriptions (#473)** fill `MemberDoc::summary` and the keyword
  documentation of resolve.
- **Doc comments (#450)** can add the comment of a declaration to resolve.
- Attributes and methods of handles, widgets, buffers and system handles
  (`hBuffer:`, `SESSION:`), and the methods of events (`Subscribe`): `null`.
- Keyword options in the middle of a statement (`NO-LOCK`, `NO-ERROR` after
  `FIND FIRST Customer`): `null`.
- `WHEN` and `OTHERWISE` in a `CASE` block.
- `RUN p IN h`: the procedures of the program that the handle runs.
- `oxabl_wasm`: the engine needs no salsa, so the browser can use it later.

## Tests

The black-box tests are over stdio against the real server, with synthetic
files only: `crates/oxabl/tests/completion_*_lsp.rs`. A separate tester writes
them from a specification of behaviour; `completion-steps.md` names the file of
each step. The bench
is in `crates/oxabl_pipeline/benches/completion_bench.rs`. The VS Code e2e
scenario `completion` checks that the suggest widget shows the server's items
and the snippets in one list.
