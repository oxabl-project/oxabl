# Find references and the call hierarchy

`oxabl lsp` answers `textDocument/references` and the three call hierarchy
requests: `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`
and `callHierarchy/outgoingCalls`. Each answer is in the bytes of physical
files: a name written in an include is placed in the include, as for go to
definition (`docs/design/navigation.md`).

## One resolution

Go to definition finds what the name at the cursor refers to. References use
the same resolution (`oxabl_pipeline::Navigation`): a reference is a name
whose definition is the same declaration, the same program file, or the same
undeclared procedure of a program (`RUN p IN h` where the program that `h`
runs does not declare `p`). There is no second resolver. A name that go to
definition does not resolve has no references.

The code is in `oxabl_pipeline::references` (one compilation unit) and
`oxabl_daemon::references` (the search over the workspace).

## Where the search goes

| The name refers to | Files searched |
| --- | --- |
| A local name: a variable, parameter, buffer, temp-table, field, a procedure or function in an include | The compilation unit only |
| A program (`RUN x.p`) | The program, and each file with a `RUN` edge to it |
| An internal procedure or user-defined function of a program | The program, and each file with a `RUN` edge to it: such a file can set a handle with `RUN ... PERSISTENT SET` |
| A class | The class file, and each file that names the class |
| A member of a class (method, property, event, data member) | The class file, each file that names the class, and each file that names a class in this set, again and again: a class that names the class can inherit the member |

The edges come from the workspace dependency graph (`ReverseGraph`), the graph
of `oxabl/impact`. The unit that asks is analysed first, on its own snapshot.
Then, only when other files can name the referent, the request waits for a
current workspace pass, and analyses each other file on a worker thread with a
fresh snapshot, with the open buffers over the disk. No snapshot is held while
the request waits, so the request never stops an edit. One request analyses at
most 2000 other files (`oxabl_daemon::references::MAX_UNITS`).

`includeDeclaration` adds the declarations of the referent. A program has no
declaration.

## The call hierarchy

**Prepare.** The item is the routine or program that the name at the cursor
refers to. On another name, or on no name, the item is the routine that holds
the cursor: a procedure, function, method, constructor, destructor, property
or `ON` trigger. Outside every routine, the item is the program or class file
itself. In an include analysed alone there is no file item.

**Outgoing calls.** The calls in the body of the item, from the static call
graph (`oxabl_index::calls::FileCalls`): `RUN`, user-defined function calls
and method calls. Go to definition resolves each call. A call in a nested
routine (an `ON` trigger in a procedure) belongs to that routine. The calls of
a program or class file are the calls outside every routine.

A call that does not resolve is an item too. Its kind is `Null`, its name is
the name written or the text of the call (`RUN VALUE(cName)`), its detail
starts with `unresolved:` and says why, and it is placed at the call. It has
no calls of its own. A call is never left out because it does not resolve.

| Call | Detail |
| --- | --- |
| `RUN VALUE(...)`, `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW` | `the target is computed at run time` |
| `RUN p IN h` where `h` is not set by one `RUN ... PERSISTENT SET` | `it runs through a handle that oxabl cannot follow` |
| `RUN SUPER`, a routine declared `IN SUPER` | `a super procedure supplies it at run time` |
| `PUBLISH` | `PUBLISH: the subscribers are known at run time` |
| A method of an object whose class oxabl does not know | `a method of an object whose class oxabl does not know` |
| A `RUN` target that no file on the include path supplies | `no file on the include path supplies it` |
| `PROCEDURE p EXTERNAL` | `a routine of a shared library` (kind `Function`) |

A name that more than one file supplies gives one item for each file, with
the detail `one of N targets the name can mean`.

**Incoming calls.** The references of the item, grouped by the routine that
holds each one, or by the program or class file for code outside every
routine. The search goes where the references of the item go.

**Item data.** The server puts what it needs to answer for an item in the
item's `data`: the unit to analyse, the file, the name, the kind and the byte
spans. A client must send the item back as it got it.

## An incomplete answer says so

An answer that can miss something is never shown as a complete answer. The
reasons, each a line of text, are the *gaps* of the answer:

- No include path is configured: other files are not searched. This is the
  refusal of `oxabl/impact`.
- The workspace index is out of date. A pass whose files changed is run again
  once. When it is still out of date, or edits replaced each pass, it is a gap.
- Files of the workspace that could not be analysed, or whose dependency set
  is incomplete (a recovered parse, an include that was not found): they can
  hold a reference that the graph does not show.
- References in other files that may name the referent and did not resolve.
- Calls in the searched files of a routine with the referent's name, through
  a handle, a super procedure or an object whose class oxabl does not know.
  Any of them can call the referent.
- The search stopped at 2000 files.
- A searched file that could not be analysed.
- A unit with more than 20000 calls (`oxabl_index::calls::MAX_SITES`): the
  rest of its calls are not listed.

The protocol has no field for an incomplete answer, so the server uses the
most visible place that it can:

- **References.** The locations that were found, and a `window/showMessage`
  warning (also sent as `window/logMessage`):
  `oxabl: references may be incomplete: <gaps>`. A list of locations has no
  place for a note, and an error would hide the part that is known.
- **Call hierarchy.** The first call of the answer is an item named
  `incomplete`, of kind `Null`, at the item that was asked about, with the gaps
  as its detail. It has no calls of its own. The same warning is sent.

## Known limits

- A `RUN p IN h` in a file that gets `h` from somewhere else (a parameter, a
  shared variable, `SESSION:FIRST-PROCEDURE`) and does not `RUN` the program
  is not searched and is not counted.
- A dynamic call (`RUN VALUE`, `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `PUBLISH`)
  can call any routine. Outgoing calls show it; incoming calls cannot.
- A method call refers to each overload of the name, so the references of one
  overload include the calls of the others.
- `NEW` (a constructor call) is not an outgoing call.
- A shared variable, a table and a field have references only in the unit
  that asks.
- Text in an `&IF` branch that the preprocessor did not take is not analysed.

## Cost

Each request analyses the unit that asks again (expand, parse, resolve; no type
check). A search over other files analyses each of them the same way, on
`available_parallelism` threads, and reads the outline of each file a
resolution names once per file. Nothing is memoized between requests. The
first request that searches other files waits for the workspace pass that
`workspace/symbol` and `oxabl/impact` also wait for.

Measured with a release build on 16 threads, over a generated workspace of
small programs (each about 50 lines, with an include, a class reference and
eight internal procedures):

| Workspace | Request | Time |
| --- | --- | --- |
| 5,000 programs, 1,000 of them run one program | First references of the program (with the workspace pass) | 0.54 s |
| | Next references of the program (1,000 locations) | 0.14 s |
| | Incoming calls of the program (1,000 callers) | 0.30 s |
| 20,000 programs, 4,000 of them run one program | First references of the program (with the workspace pass) | 1.9 s |
| | Next references (stops at 2,000 files, and says so) | 0.3 s |
| | Incoming calls (stops at 2,000 files, and says so) | 0.7 s |
| | References of a method that 100 files call | 0.06 s |

A real program is larger than these, so a search over 2,000 real files takes
longer. The limit bounds the work of one request.

## Tests

`crates/oxabl/tests/references_lsp.rs` runs the real `oxabl lsp` over stdio:
local names and names declared in an include, a program, an internal procedure
and a method in files that are not open, the call hierarchy in both
directions, dynamic calls as unresolved items, and the gaps (no include path,
a file with an incomplete dependency set, a unit with more than 20000 calls, a
searched file whose analysis panics). The `references` scenario of
`clients/vscode/test/e2e` shows both features in a real VS Code.
