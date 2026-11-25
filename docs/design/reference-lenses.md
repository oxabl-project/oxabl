# Reference-count code lenses

A reference lens is a line of text above a declaration. It says how many
places use the declaration. `oxabl lsp` makes it from the same search that
find references runs (`docs/design/references.md`). The count and the answer
of find references for the same symbol are one answer.

These lenses are not the database effect lenses (`code_lens.rs`). The two
kinds have their own settings. A client can turn one on and the other off.
Both kinds come in one `textDocument/codeLens` answer.

## What gets a lens

| Declaration | Lens text |
| --- | --- |
| Procedure, function, method, property | `N references` |
| Class, interface | `N references`: the places that name the type |
| Program (a root file that is not a class and not an include) | `N references · used by M files`, on line 1 |

`N` is the length of the find references answer for the declaration, without
the declaration itself (`includeDeclaration: false`). `M` is the number of
files that have one of these places. A
constructor, a destructor, an event and a variable have no lens.

The ranges come from the outline of the open text. A declaration that an
include holds gets its lens in the include, not in the file that includes it.
An include is analysed in the selected include context, as for find
references.

## Lazy resolve

`textDocument/codeLens` gives ranges only. A lens has no command and no count.
Its `data` holds the file, the version of the text, the byte of the name and
the kind. The client asks for the count with `codeLens/resolve` when the lens
comes into view. This is the only place where the search runs, so a large file
runs no search for a declaration that nobody sees.

A resolve runs on a worker thread, as a find references request does. It
waits for a workspace pass when other files can name the declaration. At most
two searches run at one time (`oxabl_daemon::references::MAX_SEARCHES`).
`$/cancelRequest` stops a resolve.

A resolve for text that has changed since the lens was made answers
`ContentModified`. The offsets in the data are for the old text. The client
asks for the lenses again.

## An incomplete count says so

A count is exact only when the answer has no gap of the referent (the gaps of
`docs/design/references.md`). When it has one, the title shows `N+`, the word
`incomplete` and the first reason, cut to 60 characters. A second reason adds
`(+K more)`:

```text
12+ references · incomplete: other files are not searched: no include path…
```

A program lens with gaps shows `used by M+ files`. A standing gap (the same for
every request of the workspace) does not mark a count. Find references writes
it to the log once, and so do the lenses: a workspace nearly always has a file
that oxabl cannot analyse, and a mark on every lens would hide the marks that
matter.

When no count is known, the lens says so and opens nothing:

| Case | Title |
| --- | --- |
| The declaration is not analysed (for example, it is in an `&IF` branch that the preprocessor drops) | `references not known: oxabl does not resolve this name` |
| The file gives no analysis | `references not known: the file gives no answer: <reason>` |
| The analysis panics | `references not known: the analysis failed: <message>` |

A number is never shown for these. A request that a client cancels, or that an
edit replaces, is an error, not a title.

## The command

A resolved lens runs the client command `oxabl.showReferences` with two
arguments: the file URI and the position of the name. The VS Code extension
registers it. It asks the language server for the references of that position,
takes out the declaration, and shows the rest with `editor.action.showReferences`.
The list has the length that the lens shows. A client without this command
shows the title and does nothing on a click.

## Settings

The setting is a client setting. It is off until the user turns it on.

| Client | Setting |
| --- | --- |
| Initialization options and `workspace/didChangeConfiguration` | `referenceLenses`: a boolean, or an object with a boolean `enable` (`settings.oxabl.referenceLenses` in the notification) |
| VS Code | `oxabl.referenceLenses.enable` (default `false`), applied at once |

The effect lenses follow `oxabl.toml` (an effect rule, or the transaction rule
TX004), and not this setting. VS Code's `editor.codeLens` turns off both.

The client must advertise `textDocument.codeLens`. With dynamic registration
the server registers `textDocument/codeLens` for each open file that can have
a lens of either kind, with `resolveProvider` on when the setting is on. A
change of the setting registers again. With static capabilities the server
advertises `codeLensProvider` at the handshake when the setting is on or an
effect rule is on; `resolveProvider` is on when the setting is on. A static
client must restart to use a changed setting for the capability. A change that
the server gets with `workspace/didChangeConfiguration` still applies to the
answers, and the server asks for a refresh with `workspace/codeLens/refresh`.

## Cost

A resolve costs one find references request. A file with many declarations
makes many requests, two at a time, as the lenses come into view. A resolve
that scrolls out of view is cancelled by the client. The lens needs no work
until it is seen.

## Tests

`crates/oxabl/tests/reference_lens_lsp.rs` runs the real `oxabl lsp` over
stdio. It checks that every count equals the length of find references for
the same symbol, and that the lens leaves out the declaration. It also checks
the counts of a program, procedures, a class, an interface, a property and
methods; that a listed lens has no count; the command and its arguments; the
count with no include path and with a file that has a parse error; a lens for
text that changed; a declaration that oxabl does not resolve; the setting
(off by default, on from the options, off and on again at run time, and the
registration for a client that registers); and the effect lenses with the
reference lenses beside them. The `reference-lenses` scenario of
`clients/vscode/test/e2e` shows the lenses and the click in a real VS Code.
