# Semantic tokens

`oxabl lsp` answers `textDocument/semanticTokens/full`,
`textDocument/semanticTokens/full/delta` and `textDocument/semanticTokens/range`.
A token tells the editor what a name is. The editor can then give a variable, a
parameter, a database field and a temp-table field different colours. The
TextMate grammar of the editor colours text by its shape. It cannot do this,
because it does not know what a name stands for.

The server advertises the capability to a client that sends
`textDocument.semanticTokens` at `initialize`. The user turns the tokens off
with the `semanticTokens` setting (see [The setting](#the-setting)).

## What gets a token

A token is made only for a name that oxabl **resolves to one thing**. A name
that does not resolve has no token, and the grammar of the client colours it.
This rule holds for the whole feature:

- A name that the analysis cannot find (`noSuchThing`), a name that more than
  one declaration can mean, and a computed target (`RUN VALUE(...)`) have no
  token.
- A string, a comment, a keyword and a number have no token. A built-in
  function and a system handle have no token. The grammar knows them.
- A wildcard `USING app.*.` has no token. A program named in `RUN x.p` has no
  token. `RUN p IN h` has none either: the handle supplies `p`, not this file.
- A span that holds more than one name, or a keyword, has no token. The
  analysis spans some arguments with their keyword (`BUFFER b` in
  `RUN p (BUFFER b)`). A token must be one name: it holds no space, bracket,
  quote or operator, it is on one line, and it is at most 255 bytes long.
- A span that two sites name as two different kinds has no token. This holds
  for an include that the root reads more than once: when two expansions give
  the same bytes of the include two different meanings (a variable in one, a
  parameter in the other), the bytes have no token. The bytes also have no
  token when one expansion gives no token for the name (the name does not
  resolve there). A token needs the same meaning from every expansion that
  keeps its bytes. When it does, the token is given once.
- A name that a macro or an include argument writes has no token in the file
  that uses it, because its bytes are not in that file.

The tokens come from the same facts as go to definition and document
highlights: the navigation sites and the declarations of the cached analysis
(`NavigationModel`, see `navigation.md`). For a file that is its own root, and
that reads no include with unsaved changes, the model is cached. A request
after the first costs a pass over those sites and makes no new analysis of the
unit.

Two cases do not use the cached model. They pay a new expansion and analysis of
the whole root on each full, delta and range request:

- an open include with a selected include context (see `include-context.md`);
- a file that reads an open include whose text is not the text on disk. The
  server compares each open include with its file on each request.

## Token types

The legend is `SemanticKind` in `oxabl_pipeline` (`navigate/semantic.rs`). The
position in `SemanticKind::ALL` is the number that the client sees.

| Type | A name that is | Standard LSP type |
| --- | --- | --- |
| `variable` | a variable, stream, frame or widget | yes |
| `parameter` | a parameter of a procedure, function, method or event | yes |
| `property` | a property of a class | yes |
| `function` | an internal procedure or a user-defined function | yes |
| `method` | a method of a class, declared there or inherited | yes |
| `class` | a class in a type position, `NEW`, `CAST`, `INHERITS`, `IMPLEMENTS` or `USING`, or the class that a class file declares | yes |
| `interface` | an interface, in the same places | yes |
| `event` | an event of a class | yes |
| `table` | a database table, its default buffer, or the buffer that `FOR EACH table` opens | no |
| `tempTable` | a temp-table | no |
| `buffer` | a buffer that the code declares (`DEFINE BUFFER`, a buffer parameter), also one with the name of its table | no |
| `dataset` | a dataset | no |
| `databaseField` | a field of a database table | no |
| `tempTableField` | a field of a temp-table, also one that copies a database field with `LIKE` | no |

The VS Code extension gives each type that is not a standard one a super type,
so a theme that knows nothing of oxabl still colours it: `table`, `tempTable`
and `dataset` are `type`, `buffer` is `variable`, and `databaseField` and
`tempTableField` are `property`.

A buffer that the code declares is its own record scope. So it is a `buffer`
even when it has the name of its table. Document highlights use the same rule.

## Modifiers

| Modifier | A name that |
| --- | --- |
| `declaration` | is the place where its symbol is declared. `FOR EACH table` declares nothing. |
| `static` | is a method, property, event or data member (`DEFINE ... STATIC VARIABLE`) of a class that the declaration marks `STATIC`, at its declaration and at each use |

The issue also names three more. They are not given yet:

- `deprecated` needs the `@deprecated` tag of a documentation comment. oxabl
  does not read documentation comments yet.
- `readonly` needs the facts of a constant. ABL has no constant variable, and
  the semantic model does not mark a property that has no setter.
- An unused name: VS Code already fades a name that the `unused` diagnostics tag.
  The tokens add nothing to this.

A preprocessor name (`{&name}`) has no token. The preprocessor replaces it
before the analysis, and the analysis does not keep the place where it was
written. The grammar of the client colours it.

## Which file

The tokens of a request are the tokens of the file of the request, in its own
bytes. A name that an include declares is a token of the include, not of the
program that includes it. An open include with a selected include context (see
`include-context.md`) is analysed in that root. So a name that only the includer
declares has a token in the include. An include that the root reads twice gives
each name once.

## Position encoding

A token has a line, a column and a length. The column and the length are in the
unit that the client chose at `initialize`: UTF-16 code units, or UTF-8 bytes
when the client offers them. Each token is relative to the one before it, as the
protocol says. A token never covers more than one line.

## Full, delta and range

- **Full** gives every token and a `resultId`.
- **Delta** gives the changes to the tokens of the last full or delta answer of
  the file. The client names that answer with `previousResultId`. The server
  keeps the tokens of the last answer of each open file. It computes the new
  tokens as for a full request. It gives one edit: the tokens between the
  common start and the common end of the old and the new tokens. A
  `previousResultId` that the server does not know gets the full tokens.
- **Range** gives the tokens that overlap the range. It gives no `resultId`,
  and it does not change what a delta compares with. It reads the cached
  analysis as a full request does, so the first request after an edit pays the
  analysis in each case.

## Stale requests

An editor asks again after each edit, often while an earlier request still
runs. `crates/oxabl_lsp/src/semantic_tokens.rs` does what rust-analyzer does:

- One worker thread answers the requests in order. A request that waits holds
  no lock and no database snapshot.
- The main loop records the version of a document before it applies an edit.
  The worker checks that the version of the request is still the version of the
  document. Only then does it take a snapshot. It checks again before it
  answers. A request that fails a check answers `ContentModified` (-32801), and
  the client asks again.
- The database cancels an analysis at its next query boundary when any file
  changes. An analysis that runs inside a single query is not interrupted: it
  ends, and the answer is checked against the version as before.
- A cancelled analysis of a document that has the same version as the request
  goes back to the queue with a start time 100 ms later, so it runs behind the
  jobs that came after it. The worker waits only for such a job that is not yet
  due; it never waits before a new job. The change was to
  another file, and the client does not ask again for this one. A job goes back
  at most 15 times. The request answers `ContentModified` when the version of
  its document changed, and when the job used all its attempts.
- The server never sends the tokens of one version of a document as the answer
  to a request for another version.
- A request for an old version that waited in the queue costs no analysis.

`oxabl_common::panic_sites::SEMANTIC_TOKENS` is the hold and panic site for
tests (`OXABL-TEST-HOLD:lsp.semantic-tokens` and
`OXABL-TEST-PANIC:lsp.semantic-tokens` in the source). The hold waits three
seconds before the analysis; a number of milliseconds after `@` gives another
time (`OXABL-TEST-HOLD:lsp.semantic-tokens@600`). `SEMANTIC_SNAPSHOT`
(`OXABL-TEST-HOLD:lsp.semantic-snapshot`) waits while the analysis holds its
snapshot, so that a write to another file cancels it. `SEMANTIC_ANSWER`
(`OXABL-TEST-HOLD:lsp.semantic-answer`) waits after the analysis and before the
answer. So a test can send an edit at each of the three places. The text of a
request is read for these markers in test builds only.

A panic in the analysis fails that one request with `InternalError`
(`catch_panic`, as for every navigation request). It does not stop the server.

When the analysis cannot run (the preprocessing of the unit failed), the answer
holds no token. This makes no claim about any name. The grammar of the client
colours the file until the unit analyses again.

## The setting

The `semanticTokens` object of the initialization options takes `enable`. The
same object in `workspace/didChangeConfiguration` (`settings.oxabl.semanticTokens`)
changes it later. A plain boolean works too. When it is `false`, every request
answers an empty token list, and the grammar of the client colours all the text.
A change of the setting asks the client to ask again
(`workspace/semanticTokens/refresh`). The tokens also refresh with the inlay
hints: when a watched file changes on disk, and when the include context of a
file changes. They refresh too when an edit to an open file settles (after the
debounce) and the last analysis of another open file read that file as an
include: the tokens of the reader change with the include, and the reader has
no edit of its own. The worker records the includes of each analysis, so this
holds when the diagnostics are off. For a file with a selected context, the
recorded files are the context root and the includes that its expansion read,
so an edit to the open root refreshes the tokens too. A file whose tokens the
client never asked for has no recorded includes and gets no refresh. The check
does nothing when the client cannot be asked again or the setting is off.

The VS Code extension has the setting `oxabl.semanticTokens.enable` (default
`true`) and sends it this way. The VS Code setting
`editor.semanticHighlighting.enabled` turns the semantic tokens of all
extensions off, and a theme can ask for them to be off.

## Cost

These times are from a release build of `oxabl lsp`. The file has 12,964 lines
and about 23,000 tokens (temp-tables, buffers, database reads and calls). The
answer is 360 KB of JSON. The machine ran other work at the same time.

| Request | Time |
| --- | --- |
| the first full request (expansion and analysis) | about 320 ms |
| a full request on the same text | about 30 ms |
| a range of 60 lines on the same text | about 3 ms |
| a delta on the same text | about 8 ms |
| any request right after an edit | about 150 ms more, for the analysis |

The analysis after an edit (`navigation_model`) is the one that go to
definition and highlights use. One edit pays it once for all three. The
CodSpeed bench `semantic_tokens_bench` (`crates/oxabl_pipeline/benches`)
measures a file of 10,000 lines in three parts:
`semantic_tokens/analysis` (the model, once for each edit),
`semantic_tokens/tokens` (the tokens of the whole file from the model) and
`semantic_tokens/range` (a range of about 60 lines). On the same machine they
take about 140 ms, 3.5 ms and 3.5 ms.

The pipeline finds the tokens of a range with the same pass as for the whole
file. So a range is not cheaper there. It is cheaper in the encoding and in the
answer.

## Tests

- `crates/oxabl/tests/semantic_tokens_lsp.rs` runs the real `oxabl lsp` and its
  daemon over stdio. It pins:
  - the tokens of one file with each kind of name, and a name that does not
    resolve;
  - a span with a keyword in it, class members, an include, an include that
    the root reads twice, and an include in a selected context;
  - the columns in UTF-16 and in UTF-8;
  - a range, and a delta (the edits turn the old tokens into the new ones);
  - the setting, and a client that does not support the tokens;
  - a file of 10,000 lines, and a panic;
  - the three ways a request can be stale: an edit before the analysis, an edit
    after it, and requests that wait in the queue;
  - a write to another file while the analysis runs: the request gives its
    tokens and not `ContentModified`;
  - an include that the root reads twice with two meanings of a name, and with
    one expansion in which the name does not resolve;
  - a cancelled request that runs again behind a newer request, and requests
    that come together with no delay between them;
  - an include that one read cuts short with `&IF`: the names that both reads
    agree on keep their tokens;
  - the refresh request after an edit to an open include, also with the
    diagnostics off, and after an edit to an include that the context of an
    open include reads.
- `crates/oxabl/tests/navigation_sweep_lsp.rs` asks for the tokens of the files
  full of awkward names that the navigation sweep uses. Each token must be
  inside its file, on one line, one name and in order. The ranges of the lines,
  one by one, must give the tokens of the whole file.
