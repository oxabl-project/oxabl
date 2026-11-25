# Signature help and parameter-name hints

`oxabl lsp` answers `textDocument/signatureHelp`. Inside the parentheses of a
call, it shows the parameters of the routine that the call reaches, and marks
the argument at the cursor. The same calls get the name of the parameter as an
inlay hint before each argument.

- `oxabl_pipeline::Navigation::signature_help` makes the answer.
- `oxabl_pipeline::Navigation::parameter_hints` makes the hints.
- `oxabl_lsp` sends them (`signature_help.rs`, `inlay_hint.rs`).
- The VS Code extension sends the setting `oxabl.inlayHints.parameters`.

## The rule that decides every case

**Fail closed.** A call that oxabl cannot resolve has no signature. A
declaration that oxabl may not read whole has no signature. When the arguments
do not pick one overload, the answer shows all of them and names none as
active. A hint shows only when exactly one declaration is known.

## Which calls

| Call | Reaches |
| --- | --- |
| `f(`, a user function | The `FUNCTION` that the name resolves to. |
| `obj:m(`, `Cls:m(`, `m(` in a class, `THIS-OBJECT:m(`, `SUPER:m(` | Every declaration of `m` that the receiver's class reaches, in the class and in its supertypes (`class-members.md`). |
| `NEW cls(` | The constructors of the class. From outside the class, only `PUBLIC` ones. |
| `SUPER(` | The constructors of the parent class, except `PRIVATE` ones. |
| `THIS-OBJECT(` | The constructors of the class itself. |
| `RUN p (`, `RUN p IN h (`, `RUN prog.p (` | The `PROCEDURE p` of the unit; the procedure that the program of the handle declares; the `DEFINE ... PARAMETER` statements of the main block of the program. |
| `LENGTH(`, `SUBSTRING(`, and the other built-in functions of the table | The documented syntax. |

A call is found in the tokens, not in the syntax tree, so a call that is still
being typed has an answer. The callee must resolve in the analysis, as go to
definition resolves it (`navigation.md`): the callee's name has a navigation
site. The signature text comes from the declaration, so it shows what the code
writes: `Score(INPUT p AS INTEGER, OUTPUT q AS CHARACTER): DECIMAL`.

## Where the cursor is

The scan goes back from the cursor over the tokens of the expanded unit. It
counts the commas at its own level, and skips a pair of parentheses or
brackets it passes. A string is one token, so a comma in a string does not
count. The first unmatched `(` that follows a name that can be a callee opens
the call. A `(` that follows no such name (`(1 + 2) * 3`) is skipped, and the
scan goes on to the call around it. The scan stops at a period that ends a
statement, and after 20,000 tokens.

A call in an argument list is the innermost call: the cursor in
`f(g(1, ‸2), 3)` is in `g`.

## Overloads and the active signature

Each declaration is a signature. When the call reaches several declarations,
the answer lists all of them.

The arguments pick a declaration with the rules of the overload pick of
`class-members.md` ("Overloads"). Here the type of an argument is known only
when its text is one token: a literal, or a name that resolves to a variable,
parameter or property. Every other argument is `Unknown`. The parameter types
are read from the declaration text, so a class name is `Unknown` too. The pick
is sure in two cases:

- exactly one declaration is left after the arguments rule out the others, or
- rule 3 or rule 4 of the pick chooses one (exactly one exact or widening fit,
  and no declaration that may fit).

Then that signature is first, and it is the active signature
(`activeSignature` is `0`). In every other case `activeSignature` is not in
the answer. The other signatures keep the order of their declarations. A call
with an empty argument (`f(1, )`) picks nothing, because its argument count is
not final.

`oxabl_semantic::reachable_overloads` does the pick. It is the pick of the
type-check pass, with the candidate as the result and not its return type.

Each signature has its own `activeParameter`: the number of the argument at the
cursor when the signature has a parameter there, else `null`. A built-in
function with a repeated parameter (`SUBSTITUTE`) keeps its last parameter
active.

## Which declarations of a method

The analysis gives the set of candidates of the call
(`oxabl_semantic::call_candidates`), and the text of each class file gives the
parameters. A declaration is shown when its parameter list agrees with a
candidate: the same modes and shapes, and the same primitive types where both
sides know them. So a `PRIVATE` overload that the caller cannot reach is not
shown beside a `PUBLIC` one of the same name.

A call has no signature when:

- the analysis marks its member as one with an incomplete set of candidates
  (an unread supertype, an include that did not arrive, an inheritance cycle,
  a static against an instance member; `SymbolTable::is_untyped_member`),
- a candidate has no parameter list, or no declaration in the text agrees with
  it,
- the file of a class, constructor or program does not parse, or holds an
  `{include}` or an `&IF` where its members or parameters may be, or
- the receiver is a handle, a built-in class or a .NET class.

### The seam for the members of built-in classes

A method of a built-in class (`Progress.Lang.*`, `OpenEdge.*`) gives no
signature yet. The place for it is `Resolver::builtin_class_sigs` in
`crates/oxabl_pipeline/src/navigate/signature.rs`. `Resolver::symbol_sigs`
reads each candidate of the call (`call_candidates`). A candidate has a
declaration in the unit or in a class file of the workspace. A candidate with
neither goes to `builtin_class_sigs`, which gives `None` today, so the call
has no signature. The catalog of the members of built-in classes fills this
function in with the signature of each such candidate; nothing else needs to
change.

## Built-in functions

The keyword reference in `resources/` holds a title and a link for each
built-in function, and no parameters. `oxabl_lexer` holds only the names. So
the syntax of the common functions is a table written by hand
(`crates/oxabl_pipeline/src/builtin_signatures.rs`), with the label as the
documentation writes it. A function that is not in the table gets no
signature. A test checks that each name is a registered built-in function with
a documentation entry, and that each parameter is in its label.

A built-in name that is not reserved can be the name of a routine that oxabl
does not see (an include read alone, a class with an unread ancestor). It gives
a signature only when the analysis leaves the name unresolved and nothing may
hide a routine (`Navigation::may_hide_routines`, as the hover decides). A
function with two syntaxes (`DATE`, `DATETIME`) shows both, with none active.
Built-in functions get no parameter-name hints.

## A call that is still being typed

An editor closes the parenthesis that it opens, and the period comes last. A
statement with no period swallows the statement after it, and the analysis has
no name for the call. So the server asks twice. When the first answer has no
signature and the cursor is in the root file, it asks again over the text with
the call closed (`oxabl_pipeline::close_open_call`): the parentheses still open
at the end of the cursor's line, a period, and `?` for an empty argument. It
keeps the second answer only when it holds a signature. The cursor and
everything before it keep their place. Closing a call never makes a name
resolve.

## Parameter-name hints

Before each argument of a call, a hint `name:` shows the parameter. When the
parameter has the mode `OUTPUT` or `INPUT-OUTPUT` and the call does not write
it, the hint is `OUTPUT name:` or `INPUT-OUTPUT name:`.

- A call gets hints only when it is closed, has no empty argument, and reaches
  exactly one declaration: the only one known, or the one the arguments pick.
- An argument whose text is the name of its parameter (ignoring case, and the
  mode it writes) gets no hint.
- A parameter with no name (`$N` in a prototype) gets no hint.
- A hint is placed in the bytes of the shown file. An argument written by a
  macro or an include of another file gives none.
- The hint has the LSP kind `Parameter`, and a tooltip with the signature.

The hints share the rules of the database inlay hints (`inlay-hints.md`):

- A file shows them when `diagnostics` is on in `[workspace.features]`.
- They are computed once for each version of the text, and each request returns
  the hints in its range.
- The same worker thread answers, in order. A request whose file changed
  before the worker reached it answers `ContentModified` with no analysis.
- Unlike the database hints, they read other files (class files, programs), so
  the cached hints of a file are kept only while no open buffer, no file on
  disk, the configuration and the schema change. The worker takes its snapshot
  of the session when it reaches the request, because a snapshot that waits in
  the queue would make an edit wait for it.

The setting is `inlayHints.parameters` (a boolean, on at the start), in the
initialization options and in `workspace/didChangeConfiguration`. The VS Code
setting is `oxabl.inlayHints.parameters`.

## Cost

Signature help runs one analysis of the unit for each request, as go to
definition does, on a worker thread with the open buffers over the disk. It
reads a class file once for each request. A request for a call that is still
being typed can run a second analysis. The requests come from the trigger
characters `(` and `,` and from the retrigger character `,`.

`crates/oxabl_pipeline/benches/signature_bench.rs` measures a procedure file
of about 7,000 lines with about 4,200 hints. On a loaded development machine
the hints of the whole file took about 20 ms (`signature/parameter_hints`, with
the navigation model already built). One signature help took about 8 ms
(`signature/signature_help`); most of it is the one parse of the unit, which a
`Navigation` does on its first request. Both are in the 50 ms interactive
budget, and neither runs on the main loop of the server.

## Tests

`crates/oxabl/tests/signature_help_lsp.rs` runs the real server over stdio. It
has a test for each acceptance criterion of the issue, and each fail-closed
case of this document.
