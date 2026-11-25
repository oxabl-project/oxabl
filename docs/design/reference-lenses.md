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

A program lens with gaps shows `used by M+ files`.

A standing gap is the same for every request of the workspace (a file that
oxabl cannot analyse, an incomplete dependency set). It does not mark a count,
because a workspace nearly always has one and a mark on every lens would hide
the marks that matter. Find references and the lenses write it to the log once
per session.

One standing gap is the exception: a RUN of an internal procedure or function
can go through a handle, a shared variable or a super procedure that oxabl
does not follow. That is a property of the kind of declaration, not of the
workspace, so every procedure and function lens shows it:

```text
2+ references · incomplete: calls through a handle are not followed
```

A bare number would let a user delete a procedure that a persistent library
or a super procedure still calls. A program, a class and a member of a class
have no such gap.

A method overload has the count of all overloads of its name, as find
references has. Two overloads of one name show the same number.

When no count is known, the lens says so and opens nothing:

| Case | Title |
| --- | --- |
| The declaration is not analysed (for example, it is in an `&IF` branch that the preprocessor drops) | `references not known: oxabl does not resolve this name` |
| The file gives no analysis | `references not known: the file gives no answer: <reason>` |
| The analysis panics | `references not known: the analysis failed: <message>` |

A number is never shown for these. A request that a client cancels, or that an
edit replaces, is an error, not a title.

## The command

A resolved lens runs the client command `oxabl.showReferences` with three
arguments: the file URI, the position of the name (the anchor of the list) and
the list of places, as LSP `Location` values. The list is the one the count
counts, so the list and the number are one answer, a program lens has a list
too, and a click runs no second search. The VS Code extension registers the
command and shows the list with `editor.action.showReferences`. A client
without this command shows the title and does nothing on a click. The answer
to a resolve grows with the count, because it holds every place.

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
client must restart to use a changed setting for the capability. A static
client that got `resolveProvider: false` gets no reference lens when the
setting is turned on later, because it would never resolve one. Turning the
setting off still applies to the answers, and the server asks for a refresh
with `workspace/codeLens/refresh`.

## Cost

A resolve costs one find references request. A file with many declarations
makes many requests, two at a time, as the lenses come into view. A client
can cancel a resolve that scrolls out of view (VS Code does). The lens needs
no work until it is seen.

The server keeps the title of each resolved lens. A second resolve of the
same lens (the same file, unit, declaration and text version) runs no new
search while the session is unchanged: the generations of the buffers, the
configuration and the schema are the same, and no watched file changed on
disk. An answer that a later request can change with no edit is not kept: a
workspace pass that was not ready or that failed, or a title with no count.

An edit changes the version of the text. VS Code asks for the lenses again
after each edit, and then every lens that is in view runs its search again.
This per-edit cost is the reason the setting is off by default.

## Tests

`crates/oxabl/tests/reference_lens_lsp.rs` runs the real `oxabl lsp` over
stdio. It checks that every count equals the length of find references for
the same symbol, and that the lens leaves out the declaration. It also checks
the counts of a program, procedures, a class, an interface, a property and
methods; that a listed lens has no count; the command and its arguments; the
count with no include path and with a file that has a parse error; a lens for
text that changed; a declaration that oxabl does not resolve; the setting
(off by default, on from the options, off and on again at run time, and the
registration for a client that registers; a static client that was not told
that lenses resolve); the list that each lens opens (equal to find references
for a symbol, the places that run a program for a program); a procedure that a
handle can call (never a bare number); the standing gap in the log, once; the
cache (a second resolve runs no search, and a change in another open file, or
of a file on disk, runs a new one); and the effect lenses with the reference
lenses beside them. The `reference-lenses` scenario of
`clients/vscode/test/e2e` shows the lenses in a real VS Code, clicks the
procedure lens and the program lens, and counts the rows of the references
view against the number on the lens.
