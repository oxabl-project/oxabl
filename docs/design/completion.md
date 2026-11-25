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
2. The daemon lexes the current text and finds the word and the statement
   window. A cursor in a comment, or in a string outside a `{...}`
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
| `oxabl_daemon` | The bases of each document (`completion` module), the background refresh, the class listing of the include paths, and `compute_completion` and `compute_resolve` on a worker snapshot. It is the only crate that knows salsa: a fresh base is the memoized `navigation_model` query, which semantic tokens and highlights also read. |
| `oxabl_pipeline` | The engine (`completion` module): the context finder, the collectors, keyword tables, keyword case, matching, ranking and the limit. It works on a base and the current text, with no salsa. |
| `oxabl_semantic` | The span of each scope, and the class source with `class_surface`, the members a receiver reaches. |
| `oxabl_workspace` | `FileSystem::list_dir`, to list the entries of a directory on the include path. |
| `oxabl_preprocessor` | The place of each `&GLOBAL-DEFINE`, `&SCOPED-DEFINE` and `&UNDEFINE`, for `{&` completion. |
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
cursor into it. Only the lexer runs on the current text (about 2 ms).

### Fresh, stale and cold

The daemon keeps, for each open document and include context, the newest base
it made (`oxabl_daemon::completion::Bases`). A base holds an `Arc` of the
model that the `navigation_model` query returned, so a fresh base costs no
memory of its own.

| The newest base is | Then |
| --- | --- |
| Fresh (its text is the current text) | Use it. |
| Stale | Use it with the cursor mapped (below). Set `isIncomplete: true`. Start a refresh. |
| Missing (first request after the document opened) | Make a fresh base now, on the request's worker thread. This request can take longer than the budget. |

A refresh runs the `navigation_model` query for the current buffer on a worker
snapshot and stores the result as the new base. One refresh runs for each
document at a time. A key that comes during the refresh cancels its snapshot,
as it does for every other query. Then the next request starts a new refresh.
A refresh that ends after a newer text has arrived still stores its base,
because that base is newer than the one it replaces.

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
| A fresh base | The `navigation_model` memo (shared) | The next edit |
| The class listing of the include paths | One per session (`ClassListing`) | A watched `.cls` file created or deleted, a change of `oxabl.toml` or of the include paths, or 60 seconds for directories outside the watched folders |
| The bases that resolve may read | The two newest bases of each document, by base id | When a newer base replaces them |
| Keyword tables and snippets | Static data | Never |

The directory listings of `RUN` and `{` are not cached: one request lists one
directory for each include path entry.

## Find the context

### Word and window

The daemon lexes the whole current text (`oxabl_lexer::tokenize`, about 2 ms
for 10,000 lines). Lexing from the start keeps comments and strings right.

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
| A name joined to `.` (`Customer.`, `b.`, `ttLine.`, `shop.`, `shop.Customer.`) in an expression position (not right after a record keyword of the row above, except `LIKE`) | Qualified name: fields of a record, or tables of a database | `null` |
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
| A routine body (procedure, function, method, constructor, destructor, property accessor, trigger, `DO`, `FOR`, `REPEAT`, `CATCH`, `FINALLY`) | As the file scope of a procedure file, without `PROCEDURE`, `FUNCTION` and `USING` |

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
class or interface body; `PARAMETER` only in a routine body or the file scope
of a procedure file.

### Values

The names that are valid in an expression at the cursor:

- Every symbol of the `Values` namespace in the scope at the cursor and its
  ancestors (variables, parameters, properties, class data members), and the
  buffers, temp-tables and default buffers of tables (as records, for
  `Customer.`).
- A name declared in the same routine is valid only when its declaration ends
  before the cursor: ABL needs a definition before its use. Class members,
  internal procedures, and functions declared before the cursor or with a
  `FORWARD` before the cursor, are valid everywhere in their scope.
- In a class body: the members that the class inherits, from the class source
  (role `ThisObject`).
- Built-in functions (`oxabl_lexer::BUILTIN_FUNCTIONS`) and user functions,
  and `THIS-OBJECT`, `SUPER`, `NEW` where they are valid.
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

### Qualified names and fields

For `q.` (and `q.word`) the collector looks at `q` in this order:

1. A buffer, temp-table or default buffer of a table in the scope at the
   cursor: the fields of its table.
2. A database logical name (see "Database-qualified names"): the tables of
   that database.
3. `db.t` with a database `db` and its table `t`: the fields of `t`.
4. Else `null`. When `q` is both a record and a database, the answer is `null`.

Fields:

- A schema table: every field of the `.df` table, with its type and format in
  the detail (see "Items").
- A temp-table: its own fields from its `TempTable` scope, and the fields its
  `LIKE` clause gives (`EffectiveTable::declare`, as hover reads it). A `LIKE`
  source that oxabl does not know gives the own fields and
  `isIncomplete: true`.
- A buffer: the fields of its table or temp-table.
- In a `WHERE`, the fields of the records the statement names (the record
  after `FIND ...`, each record of a `FOR` header before the cursor) come
  first and are inserted without a qualifier. The values follow.

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
  scope), wherever they are declared.
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

`list_dir` returns `None` when the file system cannot list (the in-memory
file system of a test can). `None` gives `isIncomplete: true`, never an empty
directory.

### Include files, preprocessor names and include arguments

- `{`: the files and directories under each include path entry, by the
  directory part of the word, as for `RUN`. Files with the extension `.i`
  come first; any other file is offered too, because an include can have any
  extension.
- `{&`: the preprocessor names defined at the cursor. The preprocessor records
  the place and scope of each `&GLOBAL-DEFINE`, `&SCOPED-DEFINE` and
  `&UNDEFINE` (`PreprocessedFile::defines`), in expanded offsets. The names
  are the ones defined before the cursor and not undefined, the named
  arguments the root passes to the include (`{x.i &name=value}`) in a root
  context, and the names the preprocessor defines itself (`OPSYS`,
  `WINDOW-SYSTEM`, `BATCH-MODE`, `PROCESS-ARCHITECTURE`, `PROVERSION`,
  `FILE-NAME`, `LINE-NUMBER`, `SEQUENCE`). In fragment mode the list has
  `isIncomplete: true`.
- Include arguments: in an include with a root context, the items `1` up to
  the number of positional arguments of the reference that includes it, with
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
character of the label or of a part of it (after `-`, `_`, `.`, or an upper
case letter after a lower case one). So `cusnu` and `cn` match `CustNum`, and
`nu` does not. The match is monotone: a label that matches a word also matches
each prefix of it, so the client can filter a list the server gave without
losing items. A path matches on its last part.

**Rank.** `sortText` orders the items by these keys, in order:

1. Fit: an item whose type fits the expected type comes first. The expected
   type is known on the right of an assignment (`x = |` reads the declared
   type of `x`), of a comparison in a `WHERE` (`Customer.CustNum = |`), and
   after `RETURN` in a function or method with a return type. A type fits when
   `oxabl_semantic::coercion::assignable` says so. In a `WHERE`, the fields of
   the statement's records come next.
2. Match quality: the same text (without case), then a prefix, then a match at
   the starts of parts, then any other match.
3. Nearness: a name of the innermost scope, then each outer scope, then
   members the class inherits, then keywords and snippets, then schema tables
   and fields, then workspace types and programs.
4. The label, without case.

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
`&`, `/` and `resolveProvider: true`. It is advertised for every client.

**Request.** `textDocument/completion` runs on a worker thread. The requests
of one document run one at a time. A request that a newer one replaced before
it started, or that the client cancelled with `$/cancelRequest`, answers
`RequestCanceled` (-32800) and computes nothing (as `HoverGate` does). A
request whose snapshot an edit cancelled answers `ContentModified` (-32801).
A failed analysis answers an error, never an empty list.

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
| `insertTextFormat` | `Snippet` only for a snippet item, and only when the client supports snippets |
| `additionalTextEdits` | The `USING` edit |
| `command` | `editor.action.triggerSuggest` for a package or directory item |
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

`data` is `{ "base": <base id>, "item": <the item's key> }`. When the base is
no longer kept, a symbol item resolves with no documentation. The other kinds
do not need the base.

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
  (`${1|Customer,Item,Order|}`), sorted. With more tables, it keeps its
  default text: a longer choice list is not useful, and the field and table
  completion inside the placeholder gives the real names.
- The extension drops its snippet provider when the server's
  `initializeResult.capabilities.completionProvider` is set, and keeps it
  where no server runs. `keywordCaseOf` and `test/keyword-case.json` stay only
  for that case.

## The budget

A completion in a 10,000-line file answers within 100 ms when a base exists.
The request path lexes the current text (about 2 ms), compares it with the
base text, maps the cursor, collects at most a few thousand candidates and
renders at most 200 items. It never runs a semantic analysis when a base
exists.

It is measured in two ways:

- `cargo bench -p oxabl_pipeline --bench completion_bench` (CodSpeed tracks
  it). The file is the 10,000-line file of `semantic_tokens_bench`. The
  benches are `fresh/<context>` (the base is of the text the request reads)
  and `stale/<context>` (the base is of the text before a one-line insert above
  the cursor: lex both texts, compare, map and collect). Each must stay under
  20 ms in a release build. Each later step adds its contexts (`FIND FIRST`,
  `Customer.`, `obj:`).

  | Context | Fresh | Stale |
  | --- | --- | --- |
  | `statement`: a statement start in a procedure (`FI`) | 1.6 ms | 3.5 ms |
  | `define`: after `DEFINE ` | 1.5 ms | 3.4 ms |

  Measured on a loaded machine, release build.
- A black-box test over stdio (`crates/oxabl/tests/completion_budget_lsp.rs`)
  shows that the request path does not wait for an analysis: the refresh of a
  source with the marker `OXABL-TEST-HOLD:completion.refresh` waits three
  seconds before it analyses, in test builds only
  (`oxabl_common::hold_if_injected`). A completion after an edit answers
  before the hold ends, from the stale base, with `isIncomplete: true`.

The first request after a document opens makes its base and can take longer.

## Out of scope, and the seams left

- **Built-in classes (#154).** Members of `Progress.Lang.*` and the other
  built-in types, and built-in types in type completion. They plug into
  `ClassSource` (see "The class source"). Until then, member and type lists
  have `isIncomplete: true`.
- **.NET types (#465).** `USING System.* FROM ASSEMBLY` and members of .NET
  types. A .NET catalog is one more `ClassSource`. Until then, a receiver of a
  .NET type gives `null`.
- **Signature help and parameter names (#438).** Completion inserts the name
  of a method or function without parentheses or parameters. Signature help
  reads `SurfaceMember::signatures`.
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
