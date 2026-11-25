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

A few calls are the exception. Some calls have a target that no search can
follow. A bare number would let a user delete a procedure, a program or a class
that such a call still reaches. So a count is marked when a call of the
workspace can reach that kind of declaration.

Every procedure and function lens is marked. A call can reach an internal
procedure with no static trace in the caller: a `RUN` through a handle, a
shared variable or a super procedure (`SESSION:ADD-SUPER-PROCEDURE`, then a
plain `RUN name`), `PUBLISH` and `SUBSCRIBE`, a persistent trigger
(`ON ... PERSISTENT RUN`), a callback (`SET-CALLBACK-PROCEDURE`), a CALL
object, `RUN VALUE`, `DYNAMIC-FUNCTION`. The lens does not follow these, and
the calls index does not show all of them. So no routine has an exact count.

The other declarations are marked only when the calls index shows a call that
can reach them. Each workspace pass reads the calls of every file
(`FileCalls::dynamic`, `DynamicCalls` in `crates/oxabl_index/src/calls.rs`)
and keeps which of these exist:

| Declaration | Marked when the workspace has | Text |
| --- | --- | --- |
| Procedure, function | Always | `calls through a handle are not followed` |
| Program | A `RUN VALUE`, an `ON ... PERSISTENT RUN VALUE(...)` or a `CREATE CALL` | `a RUN VALUE can run it` |
| Class, interface | A `DYNAMIC-NEW` | `a DYNAMIC-NEW can create it` |
| Class, interface | A `DYNAMIC-INVOKE` or a `CREATE CALL` (a static method by class name) | `a DYNAMIC-INVOKE can name it` |
| Class, interface | A `DYNAMIC-PROPERTY` | `a DYNAMIC-PROPERTY can name it` |
| Method, property | A `DYNAMIC-INVOKE` or a `CREATE CALL` | `a DYNAMIC-INVOKE can call it` |
| Method, property | A `DYNAMIC-PROPERTY` | `a DYNAMIC-PROPERTY can name it` |
| Class, interface, method, property | A `Progress.Lang.Class` call that gives a class or a member by a name (`GetClass`, `GetMethod`, `GetProperty`, `GetConstructor`, `GetEvent`) | `a class reflection call can reach it` |
| Method, property | A `Subscribe` or `Unsubscribe` of a class event with the method name as a string (`Subscribe(THIS-OBJECT, "OnChanged")`): marks the method of that name, or every member when the name is computed | `a class event Subscribe can call it` |
| Method | It overrides a method (`OVERRIDE`, also of a `Progress.Lang.Object` method such as `ToString`) or implements an interface method, or oxabl cannot tell (a supertype that it cannot resolve or read, an overload it cannot match) | `calls through a supertype are not counted` |
| Property | Its class names a parent or an interface | `calls through a supertype are not counted` |
| Program, class, interface, method, property | A file that oxabl cannot analyse, or that has an include that does not resolve or cannot be read, a parse error, or text that the preprocessor drops without the facts to decide (an `&IF` on `PROVERSION` with no `proversion` set) | `a file oxabl cannot read can reach it` |

```text
2+ references · incomplete: calls through a handle are not followed
1+ references · used by 1+ files · incomplete: a RUN VALUE can run it
```

The search of a member reads the files that name its class or a subclass. It
does not read the files that name only a supertype, and a call through the
supertype (a base-class variable, an interface variable, the template method of
a base class, `STRING(obj)`) reaches an override or an implementation. So the
server asks the same question as go to super method
(`oxabl_daemon::hierarchy::may_override`, from `oxabl_index::hierarchy`) for
each member and marks the count when it finds a super-member or cannot tell.
A better count would add the places of the super-member to the search.

A workspace with none of these shows exact counts for programs, classes and
members that override nothing. A `DYNAMIC-NEW` alone does not mark a member: an object that it makes
is used through a typed variable, and that use is a reference. A file that
oxabl cannot read can hold any of these calls, so one such file marks every
count that a dynamic call decides.

Find references does not show these marks. It lists the places and claims no
number. Its standing gap for a `RUN ... IN` a handle is as before. The search
gives the marks in `Search::counts` (`oxabl_pipeline`), the daemon in
`ReferenceAnswer::count_reasons`, and only the title of a lens reads them.

The search reads only the files of the workspace. A caller from outside is
never visible: an AppServer client and a program that starts with `-p`. So a
count of `0` or `1` says only that the workspace has no other use that the
lens follows. Treat it as a hint for a delete, not as a proof. `PUBLISH` and
`SUBSCRIBE` are calls in the workspace that the lens does not follow: they are
the reason that every routine is marked.

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
disk. A kept title also holds the workspace pass that its search read. On a
resolve the server checks, as find references does, that this pass is still
the current one: the pass is built again when a file of the workspace changes,
also with no watcher event (a client with no watcher, an include path outside
the folder). So a count is never older than the answer of find references for
the same symbol. A change on disk that is reported while a search runs
empties the cache and moves a counter that the search checked before it keeps
its title, so the answer of that search is not kept.

An answer that a later request can change with no edit is not kept: a
workspace pass that was not ready or that failed, or a title with no count.
The cache holds only the titles of the current session generations and of the
current version of each file; a new version of a file drops the old ones.
Each title holds every place of its answer, so the cache also stops at 100,000
places in all (a title with more places is not kept) and at 4,096 titles.

An edit changes the version of the text. VS Code asks for the lenses again
after each edit, and then every lens that is in view runs its search again.
This per-edit cost is the reason the setting is off by default.

## The client asks again

A client asks again for the lenses of a file only when that file changes. A
count and its list come from the other files too. So the server sends
`workspace/codeLens/refresh` (when the client supports it and the setting is
on) after a `didSave`, after `workspace/didChangeWatchedFiles`, and after an
edit of a file when the lenses of another file were asked for. The request is
debounced: it comes five debounce windows (1 s) after the last such change,
and a burst of changes sends one. An edit of the only file that has lenses
sends none, as the client asks for that file by itself. A client that does not
support the refresh shows an old count and list until the file changes.

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
cache (a second resolve runs no search; a change in another open file, a
change of a file on disk with and with no watcher event, and a change that
comes while a search runs, run a new one; an answer that a later request can
change is not kept); the counts for each kind of call that a search cannot
follow (a super procedure, `PUBLISH`, a trigger, a callback and a `RUN VALUE`
for routines; `RUN VALUE`, a computed persistent trigger and `CREATE CALL` for
programs; an override, an implementation and a property of a derived class;
text that the preprocessor cannot decide; the dynamic and
reflection calls for classes and members; a file that oxabl cannot read), and
the exact counts of a workspace that has none; the refresh request after a
save, a disk change and an edit of another file, and one request for a burst;
and the effect lenses with the reference lenses beside them. The cache bounds
are tested in `crates/oxabl_lsp/src/reference_lens.rs`, because no client can see
the size of the cache. The `reference-lenses` scenario of
`clients/vscode/test/e2e` shows the lenses in a real VS Code, clicks the
procedure lens and the program lens, and counts the rows of the references
view against the number on the lens.
