# Go to implementation, go to super method and the type hierarchy

`oxabl lsp` answers `textDocument/implementation`,
`textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes` and
`typeHierarchy/subtypes`. It also answers the custom request
`oxabl/superMethod`. The items are classes, interfaces and enums, found
through `INHERITS` and `IMPLEMENTS` in the files of the workspace, also files
that are not open.

## The rule that decides every case

**Fail closed.** A fact that oxabl does not know is never left out of an
answer. A supertype that oxabl cannot resolve is an item that says so. A list
that can miss a class says so. An empty list that looks complete is the fault
that this design prevents.

## One resolution

The cursor is read with go to definition (`Navigation::definitions`). So these
features never disagree with it about what a name means
(`docs/design/navigation.md`). The code is in `oxabl_pipeline::hierarchy`.

| The name at the cursor | Type hierarchy and implementation | Super method |
| --- | --- | --- |
| A class, interface or enum name: a type in a `DEFINE`, a `NEW`, an `INHERITS`, or the name in its own header | That class | none |
| The name of a built-in type (`Progress.Lang.AppError`): it has no file, so the item is placed at the name in the file that writes it | That type | none |
| A method name: its declaration, or a call whose receiver oxabl resolves | For `implementation`, the method. | The method |
| A method of a built-in class, or a name that does not resolve | `implementation` gives no locations and a gap. Type hierarchy gives the class that holds the cursor | `notAMethod`: the cursor is on no method that oxabl reads |
| Any other name, or no name | The class that holds the cursor (type hierarchy only) | `notAMethod` |

A call of an overloaded method means each overload, as go to definition lists
them. A method that an `{include}` of its class declares is not read: the answer
is empty and says so.

The name of a built-in type is found with the `USING` imports of the file that
writes it (`Navigation::builtin_type_at`), when go to definition finds no
class for it. A class of the workspace wins over a built-in type of the same
name, as in the resolve pass.

**Each class header is resolved as its own file resolves it.** The search
paths of a class file include its root-relative paths, resolved against the
package root of the file (`oxabl_index::search::search_base`), as go to
definition does. The daemon keeps one resolver for each search base
(`HierarchyUnit::resolver`) and the answers of a header never use the paths
of another file.

## The reverse inheritance index

`oxabl_index::hierarchy` holds the forward and the reverse facts. Both
features use it and nothing else.

- **Forward.** `supertypes_of` takes the header of a class
  (`ClassDescriptor`: `INHERITS`, `IMPLEMENTS` and the `USING` imports of the
  file that wrote them) and resolves each name as the resolve pass does
  (`ImportContext::resolve_class`). The answer keeps what did not resolve:

  | `Supertype` | Meaning |
  | --- | --- |
  | `Class` | A class or interface of the workspace, with its file |
  | `Builtin` | A built-in Progress type: a name and a kind, no file |
  | `DotNet` | A type of a .NET assembly, which oxabl does not read |
  | `Unresolved` | No file declares it, no search path is configured, its file cannot be read, two files can supply it, or the file may import more than oxabl read |

  An interface's `INHERITS` list is its supertypes, all with the relation
  `INHERITS`. A class whose header names no parent has the built-in parent
  `Progress.Lang.Object`: the supertypes request lists it, and the subtypes of
  `Progress.Lang.Object` are the classes with no parent.
- **Reverse.** `SubtypeIndex::build` takes the facts of every class file of
  the workspace and keeps, for each supertype (a class and its file, or a
  built-in name), the classes whose header names it. It also keeps each
  header that names a supertype that did not resolve, by the last segment of
  the name, and the class files that could not be read.
  `all_subtypes` walks the index through any number of headers, each class
  once, so a cycle ends.

The index is a value. It reads no file and knows no database. The caller
gives the facts, which an incremental cache has memoized per file.

### The daemon stays incremental

Salsa stays in `oxabl_daemon`. This work adds no tracked query. A request
reads the facts of each class file through `indexed_facts`, which the daemon
memoizes per file and invalidates on a change of that file or of an include it
read. An open buffer is read over the disk, as references read it: for the
class of the cursor, for the classes of the index and for each supertype that
the super method walk reaches. An open class file that is not saved yet counts
as a class of the workspace. So a request after an edit reads again only the
files that changed, and then builds the index from facts. The cold workspace
pass does no new work: the index is built on the first request that needs it.

The build checks `$/cancelRequest` before each class file and stops with the
cancelled error, so a request on a large workspace can be cancelled while it
reads the files.

The files to read come from the workspace pass
(`oxabl_daemon::methods::reference_graph`), as for find references: the files
with the extension `.cls`. The index reads at most
`oxabl_daemon::hierarchy::MAX_CLASSES` class files and the answer says when
it cut the list.

## Supertypes

The item of a supertype is placed at its own class file. A supertype with no
file is placed at the header of the class that names it:

- A built-in type has its kind (class, interface or enum) and the detail
  `<keyword> (built-in type, no source file)`. Its own supertypes come from
  `oxabl_index::hierarchy::builtin_type`, and the answer says that the list is
  what oxabl's table gives.
- A name that did not resolve has the kind `Null` and the detail
  `unresolved: <reason>`. A .NET type has the same kind and says that oxabl
  does not read assemblies.

An unresolved item has no supertypes and no subtypes. A built-in item lists
the classes of the workspace that name it, and says that the built-in types
that extend it are not listed.

**The seam for built-in classes.** Two functions of `oxabl_index::hierarchy`
read the built-in tables, and no other code does:

- `builtin_type(name)` gives the name, the kind and the supertypes of a
  built-in type. Today it reads the table of documented types in
  `oxabl_semantic`.
- `builtin_may_declare(type, method)` says whether a built-in type may declare
  a method of that name. Today oxabl knows only the four methods of
  `Progress.Lang.Object`; every other built-in type may declare any method.
  The super method walk and the walk up from an interface implementation call
  it, and add the gap "oxabl does not know its methods" only when it answers
  `true`.

A catalog of built-in classes that has members (the design of #154 offers one
surface for classes of every origin) changes these two functions: the items,
the reverse index and the gaps stay the same. The places that still name a
built-in type are the walk that resolves a header
(`ImportContext::resolve_class`, which asks `builtin_class` as the resolve pass
does) and the `Progress.Lang.Object` rule for classes with no parent. They
change with the catalog too.

## Subtypes

The direct subtypes are the classes whose header names the item. The editor
asks again for each level. `textDocument/implementation` on a type lists the
classes (not the interfaces) below it, through any number of headers: the
classes that implement or extend it. A class that implements an interface
through its parent is listed.

## Methods

A method of a class overrides or implements a method of a supertype when both
have the same name and the same parameters. Different parameters are an
overload. `same_signature` compares the lists of the index
(`MemberDescriptor::params`): the number of parameters, the mode of each, and
its type. It answers `Yes`, `No` or `Unknown`:

| Parameter | Comparison |
| --- | --- |
| Primitive types | Equal types are `Yes`, different types are `No`; a `LIKE` or `&IF` type is `Unknown` |
| Class types | Each name is resolved through the imports of the file that declares the method (`ImportContext::resolve_class`). The same class is `Yes`, two classes are `No`. A name that resolves in no file is `Yes` when both lists write it alike under equal imports, else `Unknown`. A .NET type, or a name that two files can supply, is `Unknown` |
| `TABLE`, `BUFFER`, `DATASET` | `Unknown`: the table is not in the list |
| A list that an `{include}` or `&IF` hides | `Unknown` |

A method counts only when it is not `PRIVATE` and not `STATIC`. An `Unknown`
match is listed and says that it may be an overload.

- **Implementation of a method.** The classes below the class of the method
  that declare a method like it. An interface that extends another declares
  no body, so it is never listed. For an interface method, a class below the
  interface that declares nothing of the name still implements it through the
  public method it inherits (the compiler accepts this): the nearest
  declaration up its parent chain is listed, also when that class is not
  below the interface. A parent that oxabl cannot resolve on that walk is a
  gap. A class in an inheritance cycle is never listed as its own
  implementation.
- **Super method.** Two searches, because one method can override a class
  method and implement an interface method. The parent chain gives the nearest
  class that declares a method like it. Each interface (those of the class and
  of the classes of the chain before the match) gives its nearest declaration,
  one branch at a time. So a method with `OVERRIDE` in a class that
  implements an interface gives both the overridden method and the interface
  method. A method does not need `OVERRIDE`: a method that implements an
  interface method has none. A cursor on no method that oxabl reads gives the
  reason `notAMethod`, and never an empty list that reads as "overrides
  nothing".
- **The declarations in a class file.** The facts and the outline of the file
  list the methods of a name in the same order. When the two lists have not
  the same length (an `{include}` or an `&IF` adds methods), the overloads
  cannot be told apart: every method of the name counts, and the answer says
  so.

`Progress.Lang.Object` has four methods (`Clone`, `Equals`, `GetClass`,
`ToString`) that oxabl does not model. A super method search for one of these
names that finds nothing, in a class whose parent chain ends there, says so, as
the member lookup does (`docs/design/class-members.md`).

### Why `oxabl/superMethod` and not a code action

A code action is a refactoring: the editor shows it in the lightbulb menu and
applies an edit or a command. Go to super method only moves the cursor, and it
must show a list of reasons when the answer can be short. The extension
already adds commands that send a custom request and show the result in the
editor (`oxabl.selectIncluderContext`, `oxabl.showExpandedUnit`). So the same
shape fits here: the request `oxabl/superMethod` takes a
`TextDocumentPositionParams` and answers `{ "locations": [Location], "gaps":
[string], "notAMethod"?: string }`, and the command `oxabl.goToSuperMethod` ("oxabl: Go to Super
Method", also in the editor context menu of an ABL file) goes to the one
location, or lists several in a peek view. The gaps are in the answer, so the
command shows them with the locations. An empty answer with a gap never reads
"overrides nothing", and an answer with `notAMethod` shows that reason
instead.

## An incomplete answer says so

Each reason an answer can be short is a *gap*, as for find references
(`docs/design/references.md`). A gap is a fact of this answer or a standing
fact of the workspace.

| Gap | Kind |
| --- | --- |
| The workspace index is not ready, is out of date, or the pass ran again and is still behind (`OXABL-TEST-HOLD:daemon.hierarchy-index` and the unit tests of `class_files_of` pin the text) | This answer |
| No include path is configured, so the classes are not searched | This answer |
| The index read only the first `MAX_CLASSES` class files | This answer |
| A header names a supertype that did not resolve, spelled like a class of the answer (the last segment of the name is the same): it can extend that class | This answer |
| A subtype declares the method with parameters oxabl cannot compare, or has members that an `{include}` or `&IF` hides | This answer |
| A class file cannot be read, or the header of the item cannot be read | This answer or a standing gap |
| Files of the workspace that the pass could not analyse | Standing |
| Any other header names a supertype that did not resolve: a class that extends it can be missed | Standing |
| The supertypes of a built-in type are what the table gives | This answer |
| A parent on the walk of an interface implementation does not resolve, or a built-in or .NET parent may declare the method | This answer |
| `implementation` on a method of a built-in class, or on a name that does not resolve | This answer |

The protocol has no field for an incomplete answer, so the server uses the
same places as the call hierarchy:

- **Type hierarchy.** The first item of the list is named `incomplete`, has
  the kind `Null` and lists every gap in its detail. A request with that item
  answers an empty list. The gaps of this answer also come as a
  `window/showMessage` warning, sent also as `window/logMessage`:
  `oxabl: the subtypes may be incomplete: <gaps>`. A standing gap goes to
  `window/logMessage` once per session.
- **Implementation.** The locations that were found. The warning and the log
  carry the gaps.
- **Super method.** The gaps are in the answer; the command shows them.

## Known limits

- A class that no file declares and that a header names is only an
  unresolved item. oxabl cannot know what it extends.
- A class whose header or members come from an `{include}` that oxabl cannot
  read, or from an `&IF`, is a gap, not a guess.
- Facts of a file that is not open come from the disk. A change on disk reaches
  the next request when the editor reports the change
  (`workspace/didChangeWatchedFiles`).
- The classes of a `.NET` assembly and the built-in classes that extend a
  built-in class are not listed (see the seam above).

## Cost

The cold pass is not changed. The workspace pass builds its own graph and does
not fill the memoized facts, so the first request that needs the index reads
the facts of each class file once, on one thread, and builds the index. Later
requests find the facts memoized, but they build the index again: each request
resolves every header (the file probes of `find_path` are not memoized, by
design, so a new file is seen) with a lookup table for each distinct class name
for the length of the request. The memo of facts keeps the cost low for a
file that did not change; it does not make a later request free.

Measured with a release build on a 4-core machine over a generated workspace:
one base class and N classes in chains of ten, each file 362 lines and 15 KB
(40 methods with bodies). Times are for one run.

| Class files | Workspace pass (first request of any kind) | First subtypes request, facts not yet read | Next subtypes or implementation request |
| --- | --- | --- | --- |
| 500 | not measured | 0.22 s in all, with the pass | 0.01 s |
| 2,000 | 0.55 s | 0.76 s in all, so about 0.2 s for the facts | 0.02 s |
| 5,000 | 1.30 s | 1.89 s in all, so about 0.6 s for the facts | 0.04 s |

The first reading of the facts is serial and costs about 0.12 ms for each class
file of this size. It is checked for a cancel before each file, so a client can
stop it.

## Tests

`crates/oxabl/tests/hierarchy_lsp.rs` runs the real `oxabl lsp` over stdio
with synthetic classes and interfaces in files that are not open: an
interface with implementing classes in other files, an interface that extends
another, an abstract method, an override chain of three levels walked down
and up, a method that nothing overrides, overloads that differ by their
parameters, a class with a parent and two interfaces, a supertype that no file
declares, a built-in supertype, an unresolved supertype spelled like the
asked interface, a class file that does not parse, a workspace with no
include path, an open buffer over the file on disk, a class file that
changes or appears on disk, and a super method in an unread class. Later
cases: an interface method that a class implements through the method it
inherits; a method that overrides a class method and implements an interface
method; a class-typed parameter that two files read as two classes; the open
buffer of a parent; root-relative search paths; a built-in type at the cursor
and in a header; a cursor on no method; a cycle; a class that is not saved
yet; the implicit `Progress.Lang.Object`; a supertype name that go to
definition and the hierarchy must read alike; and a cancel in the middle of
the index (`OXABL-TEST-HOLD:daemon.hierarchy-index`). The
`vitest` tests of `clients/vscode/src/superMethodModel.test.ts` pin what the
command shows.
