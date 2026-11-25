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
| A method name: its declaration, or a call whose receiver oxabl resolves | For `implementation`, the method. | The method |
| Any other name, or no name | The class that holds the cursor (type hierarchy only) | none |

A call of an overloaded method means each overload, as go to definition lists
them. A method that an `{include}` of its class declares is not read: the answer
is empty and says so.

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
  `INHERITS`.
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
read. An open buffer is read over the disk, as references read it. So a
request after an edit reads again only the files that changed, and then builds
the index from facts. The cold workspace pass does no new work: the index is
built on the first request that needs it.

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

**The seam for built-in classes.** `builtin_type` is the one function that says
what oxabl knows about a built-in type. Today it reads the table of documented
types in `oxabl_semantic`. A catalog of built-in classes changes this function
and nothing else: the items, the reverse index and the gaps stay the same.

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
| Class types | The same name is `Yes`; names with the same last segment (`Person` and `app.Person`) are `Unknown`; other names are `No` |
| `TABLE`, `BUFFER`, `DATASET` | `Unknown`: the table is not in the list |
| A list that an `{include}` or `&IF` hides | `Unknown` |

A method counts only when it is not `PRIVATE` and not `STATIC`. An `Unknown`
match is listed and says that it may be an overload.

- **Implementation of a method.** The classes below the class of the method
  that declare a method like it. An interface that extends another declares
  no body, so it is never listed.
- **Super method.** The nearest supertypes that declare a method like it. The
  walk goes one level of supertypes at a time (the parent and the interfaces
  together) and stops at the first level that has a match. A method that
  overrides one declaration in a class and one in an interface gives both.
  A method does not need `OVERRIDE`: a method that implements an interface
  method has none.
- **The declarations in a class file.** The facts and the outline of the file
  list the methods of a name in the same order. When the two lists have not
  the same length (an `{include}` or an `&IF` adds methods), the overloads
  cannot be told apart: every method of the name counts, and the answer says
  so.

`Progress.Lang.Object` has four methods (`Clone`, `Equals`, `GetClass`,
`ToString`) that oxabl does not model. A super method search for one of these
names that finds nothing says so, as the member lookup does
(`docs/design/class-members.md`).

### Why `oxabl/superMethod` and not a code action

A code action is a refactoring: the editor shows it in the lightbulb menu and
applies an edit or a command. Go to super method only moves the cursor, and it
must show a list of reasons when the answer can be short. The extension
already adds commands that send a custom request and show the result in the
editor (`oxabl.selectIncluderContext`, `oxabl.showExpandedUnit`). So the same
shape fits here: the request `oxabl/superMethod` takes a
`TextDocumentPositionParams` and answers `{ "locations": [Location], "gaps":
[string] }`, and the command `oxabl.goToSuperMethod` ("oxabl: Go to Super
Method", also in the editor context menu of an ABL file) goes to the one
location, or lists several in a peek view. The gaps are in the answer, so the
command shows them with the locations. An empty answer with a gap never reads
"overrides nothing".

## An incomplete answer says so

Each reason an answer can be short is a *gap*, as for find references
(`docs/design/references.md`). A gap is a fact of this answer or a standing
fact of the workspace.

| Gap | Kind |
| --- | --- |
| The workspace index is not ready, is out of date, or the pass ran again and is still behind | This answer |
| No include path is configured, so the classes are not searched | This answer |
| The index read only the first `MAX_CLASSES` class files | This answer |
| A header names a supertype that did not resolve, spelled like a class of the answer (the last segment of the name is the same): it can extend that class | This answer |
| A subtype declares the method with parameters oxabl cannot compare, or has members that an `{include}` or `&IF` hides | This answer |
| A class file cannot be read, or the header of the item cannot be read | This answer or a standing gap |
| Files of the workspace that the pass could not analyse | Standing |
| Any other header names a supertype that did not resolve: a class that extends it can be missed | Standing |
| The supertypes of a built-in type are what the table gives | This answer |

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

The cold pass is not changed. The first request that needs the index reads the
facts of each class file once and builds the index; later requests read the
memoized facts, and one hash lookup for each distinct class name that a header
names. Measured with a release build over a generated workspace (one base
class and N classes in chains of ten, each with a method):

| Class files | First subtypes request (with the workspace pass) | Next subtypes request | Implementation of a method that all override |
| --- | --- | --- | --- |
| 2,000 | 0.10 s | 0.02 s | not measured |
| 10,000 | 0.48 s | 0.13 s | 0.34 s |

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
changes or appears on disk, and a super method in an unread class. The
`vitest` tests of `clients/vscode/src/superMethodModel.test.ts` pin what the
command shows.
