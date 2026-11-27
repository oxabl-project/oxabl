# Signature help and parameter-name hints

`oxabl lsp` answers `textDocument/signatureHelp`. Inside the parentheses of a
call, it shows the parameters of the routine that the call reaches, and marks
the argument at the cursor. The same calls get the name of the parameter as an
inlay hint before each argument.

- `oxabl_pipeline::Navigation::signature_help` makes the answer.
- `oxabl_pipeline::Navigation::parameter_hints` makes the hints.
- `oxabl_lsp` sends them (`signature_help.rs`, `inlay_hint.rs`).
- The VS Code extension sends the setting `oxabl.inlayHints.parameters`. The
  hints are off until it is on.

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
| `NEW cls(` | The constructors of the class. From outside the class, the `PUBLIC` ones, and the `PACKAGE-PRIVATE` and `PACKAGE-PROTECTED` ones when the calling class is in the same package. |
| `SUPER(` | The constructors of the parent class, except the `PRIVATE` ones, and the `PACKAGE-PRIVATE` ones when the calling class is in another package. |
| `THIS-OBJECT(` | The constructors of the class itself. |
| `RUN p (`, `RUN p IN h (`, `RUN prog.p (` | The `PROCEDURE p` of the unit; the procedure that the program of the handle declares; the `DEFINE ... PARAMETER` statements of the main block of the program. |
| `LENGTH(`, `SUBSTRING(`, and the other built-in functions of the table | The documented syntax. |

A call is found in the tokens, not in the syntax tree, so a call that is still
being typed has an answer. The callee must resolve in the analysis, as go to
definition resolves it (`navigation.md`): the callee's name has a navigation
site. The signature text comes from the declaration, so it shows what the code
writes: `Score(INPUT p AS INTEGER, OUTPUT q AS CHARACTER): DECIMAL`.

A function that has a `FORWARD` prototype gets its parameters from the
prototype when its definition leaves out the parameter list (the compiler
allows this). A prototype with no parameters (`()`) says the function has
none.

The package of a class is its name up to the last `.`. A constructor with a
package access is compared with the package of the calling class. When the
caller is a procedure, the class is in a package and the caller is not (or the
reverse), the packages differ. A package access in a class of the unnamed
package is compile error 19477, so such a class does not compile. When both
are in the unnamed package, oxabl gives no signature for the call.

## Where the cursor is

The scan goes back from the cursor over the tokens of the expanded unit. It
counts the commas at its own level, and skips a pair of parentheses or
brackets it passes. A string is one token, so a comma in a string does not
count. The first unmatched `(` that follows a name that can be a callee opens
the call. A `(` that follows no such name (`(1 + 2) * 3`), or follows a
keyword that is no function and has no site in the analysis (`IF (`, `NOT (`,
`AND (`, `WHEN (`), is skipped, and the scan goes on to the call around it.
The analysis decides, not the kind of the token: a user function may have the
name of a keyword that is not reserved (`Total`, `Size`, `Count`), and then
the `(` opens its call. A `(` that follows a name that can be a callee, and
does not resolve, ends the scan with no answer. The scan stops at a period
that ends a statement, and after 20,000 tokens.

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
Each parameter list goes in with a flag that says whether the list states the
modes of its parameters (`Sig::modes_known`). The pick does not read the flag
yet, because every list that has parameters states its modes. It is there for
the candidates of built-in classes, which state none: the type check fits such
a candidate in any mode, and the pick must do the same.

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
- the file of a class, constructor, procedure or program does not parse, or
  holds an `{include}` that is not one of the parameter includes below, a
  `{&name}` reference or an `&IF` where its members or parameters may be, or
- the receiver is a handle, a built-in class or a .NET class.

A file that is read as text is checked in its tokens, not in its statements,
because the parser does not keep a `{&name}` reference as a declaration. The
places are:

- a program (`RUN prog.p`): the whole file, except its procedures and
  functions;
- a procedure (also when `RUN p IN h` reads it), and a function: the whole
  declaration;
- a class: the whole class, except the bodies of its methods and
  constructors, where no member is declared. A reference in a method body does
  not hide the method. For the methods an `{include}` outside a parameter
  list does not count, because the analysis expands it and the methods are
  checked against the analysis; for the constructors it does, because they are
  read from the text alone.

A reference that the compiler expands to nothing still hides the declaration:
oxabl does not expand it. The guards are `program_may_hide_parameters`,
`class_may_hide_members` and `Source::may_expand`.

### A parameter include

Many code bases start each parameter list with an include that declares a block
of shared parameters (`METHOD PUBLIC VOID SetData({inc/std-params.i}, INPUT
pcDocNum AS CHARACTER)`, or `{inc/std-input.i}` with `DEFINE INPUT PARAMETER`
statements at the start of a program or procedure). Such an include is one
entry of the signature, as the declaration writes it, with the count of the
parameters it declares:

```text
SetData({inc/std-params.i} (2 parameters), INPUT pcDocNum AS CHARACTER, OUTPUT plOk AS LOGICAL): VOID
```

The signature never lists the parameters of the include: such an include often
declares a dozen or more. The label of the entry holds the text of the
reference as written (arguments of the include too, on one line). A count of
one reads `(1 parameter)`. A `RETURN` parameter is not counted, as it is no
argument.

The analysis already expands the include, so it knows the parameters. A file
that is read as text holds only the reference, which the parser skips in a
parameter list. So when such a file has an include in a parameter list, or an
include in the main block of a program or in a procedure, oxabl expands it
(`Loaded::view` in `navigate/folding.rs`), and `Folding::entries` groups the
parameters of the expansion by the include that wrote them. The include entry
holds the reference of the root file, never one of an include below it.

The entries are known whole, and the declaration has a signature, only when:

- the expansion arrived whole: no include in the file did not resolve, none
  could not be read, and the expanded text parses;
- the include is one plain `{file}` reference, and neither it nor an include
  below it holds a `{&name}` reference or an `&IF` (the text of the include
  may differ from one compile to another);
- every include that the file writes in the parameter lists of a class, in the
  main block of a program (outside its procedures and functions), or in a
  procedure is the entry of a parameter list. An include that declares no
  parameter, such as one of variables, is not known to declare none, so it
  hides the parameters as before. For a class, an include outside its parameter
  lists and its bodies still hides the constructors.

A `{&name}` reference or an `&IF` in the parameter list is checked in the text
of the file as before.

A method or constructor that an include declares is not read: only the members
that the file writes have a signature. The routines of the analysed unit are
read from its expanded text, and group their includes the same way.

### The active parameter and the arguments

The arguments of a call are counted in the expanded unit, so a caller that
passes an include of arguments (`oDoc:SetData({inc/std-args.i}, "D1",
OUTPUT lOk)`) has the arguments that the include expands to. The active
parameter is the entry that takes the argument at the cursor. The cursor in the
arguments that the include covers marks the include entry. After them, it marks
the next parameter. The overload pick reads the flat list of the parameters,
so an include entry takes as many arguments as it has parameters.

### The seam for the built-in classes

The members of built-in classes (`Progress.Lang.*`, `OpenEdge.*`) give no
signature yet. Two functions of `Resolver` in
`crates/oxabl_pipeline/src/navigate/signature.rs` are the places for them, and
both give `None` today:

- `builtin_class_sigs`, for the methods. `Resolver::symbol_sigs` reads each
  candidate of the call (`call_candidates`). A candidate has a declaration in
  the unit or in a class file of the workspace. A candidate with neither goes
  to `builtin_class_sigs`.
- `builtin_constructor_sigs`, for `NEW` and for `SUPER(` of a class that
  inherits a built-in class. `Resolver::constructor_sigs` calls it for a class
  symbol that has no class file and no declaration in the unit. It must give
  `None` for such a symbol that is not a built-in class.

A signature of a built-in class does not state the mode of its parameters: the
catalog has none (`docs/design/builtin-classes.md`, "Overloads with modes that
are not known", of the built-in classes work). `Sig::modes_known` says so, and
`Param::mode` is `None`. A declaration with no known modes is never the
declaration that the overload pick chooses (`picked_sig`), and a call that
reaches one gets no hint, because a hint would have to name a mode. The
signature help still shows its label, and shows it as the only declaration
when it is the only candidate. The built-in functions are such declarations
today.

**Which reader signature help uses once the catalog lands.** The catalog gives
each built-in member a symbol, with `NodeId::DUMMY`, the parameters as
`method_params`, and the catalog row as `SymbolTable::builtin_member`. The
two functions above read that row, through the candidate symbols that the
call already resolves. They do not use `ClassSource` and `class_surface` of the
completion work (`docs/design/completion.md`, "The class source"). That
interface answers "which members does this receiver reach", by class name, for
a list of items; signature help already has the candidates, from the
analysis, and needs the declared parameters of each. So there is one catalog
row for each built-in member, and the three features read the same rows: the
type check by symbol, completion by `ClassSource`, and signature help by
symbol. The signature text of a built-in member is made from the row
(`name(type name, ...)`, with the types as the catalog spells them), and it
has the same form in the completion item and in the signature.

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
function with two syntaxes (`DATE`) or more (`DATETIME`) shows all of them,
with none active. A function with a repeated parameter keeps its last
parameter active up to the most arguments that the reference gives
(`SUBSTITUTE`: nine `arg`). Built-in functions get no parameter-name hints.

Each row has the title of its reference page in a comment above it, and a
test fails when a row has no such comment or the title is not the one of the
documentation entry of the function. The comment says where the row came from;
a reviewer still checks the row against that page.

## A call that is still being typed

An editor closes the parenthesis that it opens, and the period comes last. A
statement with no period swallows the statement after it, and the analysis has
no name for the call. So the server asks twice. When the first answer has no
signature and the cursor is in the root file, it asks again over the text with
the call closed (`oxabl_pipeline::close_open_call`): the parentheses still open
at the end of the cursor's line, a period, and `?` for an empty argument. It
keeps the second answer only when it holds a signature. The cursor and
everything before it keep their place. Closing a call never makes a name
resolve. A statement that already ends on the cursor's line, with all its
parentheses closed and no empty argument, is not asked again: closing it would
change nothing. So a call that does not resolve (a method of a built-in class,
a function that is not in the table) costs one analysis.

## Parameter-name hints

Before each argument of a call, a hint `name:` shows the parameter. When the
parameter has the mode `OUTPUT` or `INPUT-OUTPUT` and the call does not write
it, the hint is `OUTPUT name:` or `INPUT-OUTPUT name:`.

- A call gets hints only when it is closed, has no empty argument, and reaches
  exactly one declaration: the only one known, or the one the arguments pick.
- An argument that already says the name of its parameter gets no hint. It
  is one name (ignoring case, and the mode it writes) and one of:
  - the same name;
  - the parameter name without its prefix `p`, `ip`, `op` or `iop`
    (`cName` for `pcName`);
  - a name that ends with the word of the parameter after its prefix and its
    type letter (`cCustName` for `pcName`);
  - a parameter name that ends with the argument name (`qty` for `pdQty`).

  An end counts only when it is a whole word of the longer name. A word starts
  at the start of the name, after `-` or `_`, or at an upper-case letter that
  follows a lower-case one. So `iDiscount` does not say `piCount`, `count`
  does not say `discount`, and `age` does not say `message`; `cust-num` says
  `ip-cust-num`.

  A name of fewer than three letters counts only as the same name or as the
  parameter without its prefix. The code of ABL projects names a variable and
  its parameter alike, so a hint for such an argument repeats the text.
- A parameter include gets one hint, on the first argument that stands for its
  parameters, and the hint is the reference of the include: `{inc/std-params.i}:`.
  The other arguments for the include get none. The arguments after them get
  the names of their parameters. An argument that an include of arguments
  wrote gets no hint (see the rule on the bytes of the shown file below).
- A parameter with no name (`$N` in a prototype) gets no hint.
- A hint is placed in the bytes of the shown file. An argument written by a
  macro or an include of another file gives none.
- The hint has the LSP kind `Parameter`, and a tooltip with the signature.

The hints share the rules of the database inlay hints (`inlay-hints.md`):

- A file shows them when `diagnostics` is on in `[workspace.features]`.
- They are off until the client turns them on. A project that has the hints on
  shows about one hint for each line of code (the benchmark below has 4,200
  hints for 7,000 lines), and each edit costs a new analysis of the file.
- They are computed once for each version of the text, and each request returns
  the hints in its range.
- The same worker thread answers, in order. A request whose file changed
  before the worker reached it answers `ContentModified` with no analysis.
- Unlike the database hints, they read other files (class files, programs), so
  the cached hints of a file are kept only while no open buffer, no file on
  disk, the configuration and the schema change. The worker takes its snapshot
  of the session when it reaches the request, because a snapshot that waits in
  the queue would make an edit wait for it.
- The analysis holds that snapshot, so a write to the database waits for it
  and cancels it. When the text of the file is the same, the worker runs the
  analysis again on a new snapshot (four times at most), because the client
  does not ask again after `ContentModified`. Each attempt reads the open
  buffers again, together with its snapshot, so the text of the edit that
  cancelled the earlier attempt is in it. When the text changed, also while
  the analysis ran, the request answers `ContentModified` and the client asks
  again.

**Known limit.** A hint of a call depends on a declaration in another file. An
edit of an open file, or the close of a buffer that has unsaved edits, does not
make the server send `workspace/inlayHint/refresh`. So the hints in another
open file can be stale until that file changes or the client asks again (a
scroll, a new window). The refresh would make the client ask for every visible
file after each edit, and each request is a new analysis of that file on the one
worker: with several large files, the hints of the edited file came seconds
late. A file on disk that changes, the configuration and the settings do send
the refresh.

The setting is `inlayHints.parameters` (a boolean, off at the start), in the
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

`crates/oxabl/tests/signature_help_include_lsp.rs` tests the parameter
includes: a method, a constructor, a program, a procedure of a handle and the
routines of the unit; the active parameter in and after the include, with
plain arguments and with an include of arguments; the hints; the hover; and
each case where the expansion is not known whole.
