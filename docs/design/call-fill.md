# The call fill

When a person chooses a procedure, a function or a program from completion, the item writes the call: one snippet placeholder for each parameter, named after it, and Tab moves from one to the next. `RUN DoWork` becomes `RUN DoWork (${1:piCount}, OUTPUT ${2:pcName}).`, and a function `Twice(${1:piValue})`. This is what rust-analyzer does with its `callable.snippets` setting. Signature help still opens after the insert, because its popup shows the types.

## The rule that decides every case

The parameters come from the resolution of signature help (`docs/design/signature-help.md`), never from a reading of their own. So the fill and signature help never disagree about a callee. What signature help does not resolve gets no fill: the item writes its name only. A fill never guesses.

## Which items

| Item | Context | Where the parameters come from |
| --- | --- | --- |
| Internal procedure of the unit | `RUN` | The completion base (inline) |
| User function of the unit | An expression, a `WHERE` | The completion base (inline) |
| Built-in function in the table of syntaxes | An expression, a `WHERE` | The table of `builtin_signatures.rs` (inline) |
| Program on the include path | `RUN` | An analysis after the insert (the command) |
| `PROCEDURE` that only the text after the cursor shows | `RUN` | An analysis after the insert (the command) |
| Method | Member completion | The seam below |

Other items (variables, keywords, tables, a directory) write their name only.

## Where the parameters come from

A list can hold hundreds of items, for example every program on the include path after `RUN`. To read the parameters of each program, the server must read and parse each file. That is too slow for a list that the client asks for at each key. So there are two paths.

**Inline, from the base.** The completion base (`docs/design/completion.md`, "The base") already holds the declarations of the unit's own procedures and functions. When the daemon makes a base, it also makes a table of them (`DeclaredCallees`): for each procedure and function symbol, the parameters as `Navigation::declared_callee` reads them, and the bytes of its statement. `declared_callee` reads the declaration as signature help reads the declaration of a resolved `RUN` or function call (`Resolver::unit_sig`). The table parses the expanded text again, so a refresh makes it before it keeps the base, off the request path. Only the first base of a document makes it on the request thread, as it makes the base there. A request reads the table only for the items that the cut to 200 keeps. A built-in function needs no base work: its syntaxes are a static table (`Navigation::builtin_callees`).

A stale base can hold an older text of a declaration. When the current text changed bytes of the declaration's statement, or the base is older than a file it read, the item gets no inline fill and takes the command path.

**After the insert, through a command.** An item whose parameters the base cannot give carries the command `oxabl.fillCall`. VS Code runs it after it inserted the name. The extension asks `oxabl/callArguments` with the document version and the cursor. The server answers `null` when its version of the document is another one. Else it opens the call after the name (`RUN prog.p ()` or `f()`, closed as signature help closes a call that is still being typed), runs one analysis of that text, and reads the callees with `Navigation::callees_at`: the same call finder and resolver as `Navigation::signature_help`. It answers `{ "snippet": " (${1:pcIn}, OUTPUT ${2:piOut}).$0", "parameterHints": true }`. The extension inserts the snippet at the cursor only when the document version and the cursor did not change while the server worked, and then shows signature help.

**Why not `completionItem/resolve`.** VS Code does not let resolve change the inserted text: "no property that changes the presentation (label, sorting, filtering etc) or the (primary) insert behaviour (insertText) can be changed" (`CompletionItemProvider.resolveCompletionItem`). `vscode-languageclient` 9 sends `resolveSupport.properties` `documentation`, `detail` and `additionalTextEdits`, and an additional edit is not a snippet. So resolve cannot add placeholders.

**Clients.** The server names `oxabl.fillCall` on an item only when the initialization options say `completion.fillCommand: true`; the extension sends it. Another client gets the inline fill and no command. A client without `snippetSupport` gets no fill.

## The text

| Callee | Text after the name |
| --- | --- |
| `RUN` with parameters | ` (${1:a}, OUTPUT ${2:b}).$0` |
| `RUN` with no parameters | `.$0` |
| Function with parameters | `(${1:a}, OUTPUT ${2:b})$0` |
| Function with no parameters | `()$0` |
| Built-in function whose syntax has no parentheses | Nothing |

- Each placeholder is the parameter name. The mode comes before it when it is not `INPUT` (`OUTPUT`, `INPUT-OUTPUT`), because ABL needs the mode at the call. A table, a buffer and a dataset write their keyword: `OUTPUT TABLE ${1:ttOrder}`, `BUFFER ${2:bOrder}`, `DATASET ${3:dsOrder}`, `TABLE-HANDLE`, `DATASET-HANDLE`. The keywords follow the keyword case of the project, as the other items of completion. An unnamed parameter of a prototype writes its type.
- A built-in function fills only the parameters that its syntax does not put in brackets: `ENTRY(${1:element}, ${2:list})`.
- `RUN` gets its period only when nothing follows the name on its line. A call that already has its parentheses (`RUN Do| (1).`) gets no fill.
- A name with a snippet character (`$`, `}`, `\`) is escaped.
- An item that writes a placeholder or parentheses with the cursor in them asks the client to show signature help (`editor.action.triggerParameterHints`).

## Overloads

A fill is written only when exactly one declaration is known. With several (`DATE` has two syntaxes; a method can have overloads), the item writes the parentheses with the cursor in them, and signature help shows every declaration. The arguments then pick the active one as signature help does.

One item for each overload was the other choice. It puts several rows with the same label in the list, which differ only in their detail, and the person must read the details to choose. Signature help already lists the overloads, and it follows the arguments as the person types them. So oxabl gives one item and the parentheses.

## The setting

`oxabl.completion.callArguments` (the initialization option and the setting `completion.callArguments`):

| Value | What a call item writes |
| --- | --- |
| `fill` (default) | The text above |
| `parentheses` | The parentheses with the cursor in them (`RUN DoWork ($0).`, `Twice($0)`), and signature help. A callee with no parameters writes as with `fill` |
| `none` | The name only, and no command |

The setting applies with no restart: the extension sends it in `workspace/didChangeConfiguration`.

## A parameter include

A parameter list can start with an include of shared parameters (`{params.i}`), which often declares a dozen parameters or more, the same for every routine. Signature help gives the include as one entry (`docs/design/signature-help.md`, "A parameter include"). Until the fill below is built, a callee with a parameter include gets no fill: `Sig::callee` answers `None` for it, because its expanded parameters do not line up with the entries of the label.

The fill writes it as **one** placeholder, not one for each parameter it declares. Its default text is the include reference that the callers of the workspace pass at that place most often, when one reference is clear (more than half of the calls that the workspace pass knows write it). Else it is the include reference as the declaration writes it. `CalleeParameter` then carries the include entry, and `call_fill::render` writes its text as the placeholder. The callers come from the reference graph of the daemon's workspace pass (`docs/design/workspace-pass.md`), so only the command path can read them: an item whose callee has an include entry takes the command path.


## The seam for methods

Member completion (#614) lists the methods of a receiver. A method item that names the method's symbol of the base gets its fill from the same resolution through `call_fill::Engine::fill`: its match on the item kind has one arm to add, `ItemKind::Method` with `ItemData::Symbol`, which reads the declarations as `Resolver::symbol_sigs` reads them (all overloads: one gives a fill, several give the parentheses). A method of another class file, whose declarations the base does not hold, takes the command path (`ItemAction::FillCall`), and `Navigation::callees_at` already resolves `obj:Name(` and `NEW Class(`. No other design is needed.

## Cost

- A request reads one table entry for each kept item: a hash lookup and a clone of the parameter list. The cut to 200 items comes first.
- A base costs one more parse of its expanded text, on the refresh thread. With the setting `none`, or a client without snippets, no table is made.
- The command path costs one analysis of the unit, once for each chosen item, after the insert.

## Gaps

- Signature help does not resolve a `RUN` of a path with a directory (`RUN util/helper.p (`): its call finder joins only the dotted parts of a name. So such a program gets no fill. The fill follows when signature help resolves it.
- A procedure of the unit whose include oxabl cannot find has its other parameters in the expanded text, and signature help shows those. The fill follows.

## Tests

`crates/oxabl/tests/completion_call_fill_lsp.rs` runs through `oxabl lsp`: the text for a procedure, a function, a built-in function, the modes and keywords, the keyword case, the period, a call with its parentheses, overloads, a callee with no known signature, the three values of the setting and its change, a client with no snippets, the command for a program and its answer, a stale version, a client with no command, and an edit of the declaration. The VS Code scenario `call-fill` chooses a procedure, types in the first placeholder, presses Tab and types in the second, then chooses a program and sees the command fill it.
