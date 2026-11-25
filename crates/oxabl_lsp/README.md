# ABL language server

Run `oxabl lsp` to use diagnostics, incremental document sync, and whole-document formatting.

Database effect code lenses are off by default. Enable the access group in `oxabl.toml`:

```toml
[workspace.lint]
enable = ["strict.access"]

[workspace.schema]
files = ["schema.df"]
```

The transaction rule TX004 also enables lenses. File overrides apply to each request.
The client must advertise `textDocument.codeLens` support.

Each main block, procedure, function, method, or trigger with database effects or calls gets one lens.
For example: `reads customer · writes order-line · 2 calls (2 unknown) · partial`.
Reads and writes include effects from called units. `partial` means some effects can be missing.
Units with no database effects and no calls have no lens. Include-only units have no lens.
Positions refer to header lines in the open document, including unsaved text.
Titles use the same analysis as `oxabl explain`. They have no click action.

Requests return ready lenses immediately. A background worker reads the daemon cache or builds the reachable graph.
The first request can return an empty list. Clients with `workspace.codeLens.refreshSupport` receive a refresh request.
Other clients see ready lenses on their next request. Document edits and watched-file changes clear old results.

Clients with dynamic registration receive file-specific registrations when enabled files open.
This also supports file overrides and sessions without a declared workspace root.
Static clients receive the capability at initialization when the declared workspace enables an effect rule.
After changing this setting, restart a static client to update its capabilities.

The daemon protocol and the existing check and analyze JSON documents are unchanged.

## Reference-count code lenses

A separate kind of lens counts the references of a declaration. It is off until the client turns it
on: `referenceLenses` (a boolean, or an object with a boolean `enable`) in the initialization options or in
`workspace/didChangeConfiguration` (`settings.oxabl.referenceLenses`). It does not depend on `oxabl.toml`,
and the effect lenses do not depend on it.

Each procedure, function, method, class, interface and property gets a lens, and each program gets one on
line 1. `textDocument/codeLens` gives the ranges. `codeLens/resolve` runs the search of find references and
fills in the title: `3 references`, and for a program `3 references · used by 2 files`. The count is the
length of the find references answer without the declaration. A count that can be short says so:
`12+ references · incomplete: <reason>`, for example when a call through a handle, a `RUN VALUE`, a
`DYNAMIC-NEW` or a `DYNAMIC-INVOKE` of the workspace can reach the declaration. A declaration that oxabl does not resolve says
`references not known: ...` and shows no number. The command is `oxabl.showReferences`, with the file URI, the position of the name and the list
of places that the count counts. The server keeps resolved titles for an unchanged session and sends a debounced `workspace/codeLens/refresh`
after a save, a change on disk and an edit of another file. See `docs/design/reference-lenses.md`.

## Semantic tokens

The server answers `textDocument/semanticTokens/full`, `full/delta` and `range` for a client
that advertises `textDocument.semanticTokens`. A token is given for a name that oxabl resolves to
one thing, by what it is (`variable`, `parameter`, `property`, `function`, `method`, `class`,
`interface`, `event`, `table`, `tempTable`, `buffer`, `dataset`, `databaseField`,
`tempTableField`), with the modifiers `declaration` and `static`. Any other text has no token.
The client turns the tokens off with `semanticTokens.enable` (a boolean) in its initialization
options or in `workspace/didChangeConfiguration` (`settings.oxabl.semanticTokens`). A request
whose document changed answers `ContentModified`. See `docs/design/semantic-tokens.md`.

## Completion

The server answers `textDocument/completion` and `completionItem/resolve` for every client
(trigger characters `:`, `.`, `{`, `&` and `/`). At a statement start the list holds the
statements that are valid in the scope at the cursor, the snippets of the VS Code extension
where their body is valid (only for a client with `snippetSupport`), and the names in scope.
After `DEFINE` it holds the modifiers that may still come and what `DEFINE` defines. In an
expression it holds the names in scope and the built-in functions, with the names whose type
fits an assignment first. Keywords are in the case of `keyword_case`; `Preserve` follows the
typed word, then the nearest keyword before it, then upper case. Where oxabl does not know the
context (a comment, a string, right after a statement end, a keyword option in the middle of a
statement) the answer is `null`. A list that can miss items has `isIncomplete: true`: a stale
analysis, a parse error before the statement, an include analysed alone, a cut list.

A request never waits for an analysis once the document has one: it reads the newest analysis
and maps the cursor into it, and a newer analysis is made in the background. A request that a
newer one replaced before it started, or that the client cancelled, answers `RequestCanceled`.
Resolve adds Markdown documentation: the keyword's page, or the hover of a name's declaration.
See `docs/design/completion.md`.

## Database inlay hints

Files whose configuration has `diagnostics` on get inlay hints: the index oxabl expects a
search to use, the lock a read takes when it names none, and where a raised buffer scope
(TX005) or an implicit transaction (TX001, TX002) starts and ends. The index kind needs no
rule. Each other kind shows only where its rule is on (LK001, TX005, TX001/TX002), so a
project with no strict rule gets the index hint only. The lock, scope and transaction hints
come from the facts the strict rules read. A fact that is not known gives no hint: a read
that follows an include that did not arrive has no index hint. An include with a selected
include context shows the hints of that root.

The client must advertise `textDocument.inlayHint` support. It can turn each kind off with the
`inlayHints` object (`locks`, `index`, `scope`, `transactions`) in its initialization options or
in `workspace/didChangeConfiguration` (`settings.oxabl.inlayHints`). See
`docs/design/inlay-hints.md`.
