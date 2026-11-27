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
class for it. The name is resolved again as the resolve pass resolves it, with
the real index of the unit (`ImportContext::resolve_class`), and counts only
when that gives a built-in type. A class of the workspace wins over a built-in
type of the same name. So a class that oxabl cannot use (its file does not
parse, or two files can supply it) is never shown as the built-in type: the
name has no item.

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
files that changed, and then builds the index from facts. The workspace pass
itself reads no facts. After the first request of a session, the daemon
builds the index after each pass, in the background (below).

The files to read come from the workspace pass
(`oxabl_daemon::methods::reference_graph`), as for find references: the files
with the extension `.cls`. The index reads at most
`oxabl_daemon::hierarchy::MAX_CLASSES` class files and the answer says when
it cut the list.

### The kept index, built in the background

The first read of the facts is the cost of the first request. On a large
codebase it takes seconds, and the next request, from memoized facts, takes a
small part of that. The editor shows nothing in that time. So the daemon keeps
the index, and after the first request it reads the facts before the next
request comes.

`crates/oxabl_daemon/src/subtypes.rs` holds, for each session, the index and
the build that makes it.

- **When it is built.** A session builds no index until a request needs one
  (the subtypes, the implementations of a class or of a method that can be
  overridden). That request starts the build and waits for it, with progress.
  So a user who never asks for a type hierarchy pays nothing: a workspace pass
  starts no build. From then on, each workspace pass that ends and is stored
  starts a build on a thread of its own (`oxabl-subtype-index`), so the index
  stays warm and the next request finds it ready. The thread reads the facts of
  each class file of the pass, and of the open buffers that hold a class, and
  builds the index. The build is background work of the daemon: the daemon does
  not exit idle while it runs. It has no other priority than a workspace pass
  has. It is one thread, and it holds no lock of the sessions.
- **What a build costs besides the facts.** The class files of a pass are
  listed once for each graph (`ReverseGraph::tracked_files_where`, which clones
  and sorts only the class files), and canonicalized once for each graph when
  an open class buffer needs it. A build and a request of the same graph use
  that list.
- **When the kept index is current.** The index is kept with a stamp: the
  workspace pass (a graph that is the same one, not an equal one) and the text
  of each open class buffer whose text is not the text that the pass read
  (`KeptPass::read_hash`). A new configuration or schema clears the stored
  pass, so it gives a new graph. A request takes the stamp of the session as it
  is now. It uses the index only when the two stamps are the same, and answers
  from the database snapshot that it took with that stamp, so the answer and
  the index describe one moment. A new pass gives a new graph, so an edit that
  the pass reads, a file that appears and a file that changes on disk all end
  the current index. Any other change is not an input of the index: a buffer
  that is not a class does not change the stamp, and neither does a class file
  that is opened or closed with the text that the pass read.
- **A watched-file event.** `workspace/didChangeWatchedFiles` drops the kept
  index and stops the running build (`forget_subtype_index`), as it clears the
  cache of reference lenses. The stamps of a pass can miss a change on disk
  (for example on a network mount), and the event is the editor's word that a
  file changed.
- **After an edit.** The index is built again when a pass stores a new graph,
  and when a request needs it. The facts of the files that did not change are
  memoized, so the build reads only the files that changed, and then builds the
  index from facts.
- **An edit does not wait for the build.** The build holds a database
  snapshot, and a write to a buffer waits until every snapshot drops. The build
  reads each class file through a salsa query, which checks for a pending
  write, so the build stops at the next file. Nothing waits for it, and no
  request of another feature waits for it.
- **A running build for another stamp is stopped.** Nothing waits for its
  index any more. The one exception: a request whose graph is older than the
  stored pass never stops the build of the stored pass. It waits for that build
  and takes the graph again. A build that a newer build replaced is not kept,
  also when it ends with an index. Each attempt of a request takes the graph
  again. A request that meets edits makes five attempts, and then answers with
  `ReferenceError::Modified`.
- **Cancel.** A request that waits for the build checks `$/cancelRequest` about
  every 50 ms and answers `RequestCancelled` at once. The build goes on, and the
  next request finds its index. The build checks no cancel of a request.
- **A build that fails** (a panic) is not kept, and the request that waited
  for it answers with the reason. The next request starts a new build.

A request for go to implementation uses the same index. A request that needs no
index, such as the supertypes, never waits for it, and neither does the
implementations of a method that nothing can override.

When the pass gives no graph (the workspace has no include path, or the pass
failed), only the open buffers are classes. The index is then built on the
request and not kept.

### Progress

A request that waits shows work-done progress (`$/progress`):

- while it waits for a workspace pass: `Analysing the workspace: <n> of <m>
  files`;
- while it waits for the index: `Reading class files: <n> of <m>`, with the
  count of class files that the build has read and the count that it reads.

The report starts after 250 ms, so a request that waits a moment shows
nothing. A request for a ready index shows nothing. The server uses the
`workDoneToken` of the request. When the request has none and the client said
`window.workDoneProgress`, the server sends `window/workDoneProgress/create`
with a token of its own and starts the report when the client has answered.
A client that does neither gets no progress. The report ends before the answer
goes out, also when the request is cancelled or fails
(`crates/oxabl_lsp/src/work_progress.rs`).

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

**The seam for built-in classes.** The code that reads the tables of built-in
types is in these places:

- `builtin_type(name)` gives the name, the kind and the supertypes of a
  built-in type. Today it reads the table of documented types in
  `oxabl_semantic`.
- `builtin_may_declare(type, method)` says whether a built-in type may declare
  a method of that name. Today oxabl knows only the four methods of
  `Progress.Lang.Object`; every other built-in type may declare any method.
  The super method walk and the walk up from an interface implementation call
  it, and add the gap "oxabl does not know its methods" only when it answers
  `true`.
- `ImportContext::resolve_class` asks `builtin_class`, as the resolve pass
  does, for each name of a header and for the name at the cursor.
- `Navigation::builtin_type_at` keys on a type that the resolve pass leaves
  `Unresolved`, and then asks `resolve_class` for a built-in answer.
- `implicit_object`, `OBJECT` and `is_object` in `oxabl_daemon::hierarchy`
  give a class with no parent the parent `Progress.Lang.Object`, and list the
  classes with no parent as its subtypes.
- `OBJECT_METHODS` in `oxabl_index::hierarchy` holds the four methods of
  `Progress.Lang.Object`. It is a copy of what the resolve pass knows as a
  name rule.

A catalog of built-in classes that has members (the design of #154 offers one
surface for classes of every origin) changes all of these places. In that
design a sure built-in type resolves to a symbol and not to `Unresolved`, so
`builtin_type_at` changes with the resolve pass. The items, the reverse index
and the gaps stay the same. `builtin_may_declare` answers by name only: it
cannot say that a method overrides a given built-in method. A super method
that lists built-in methods is a later change of `super_methods`.

## Subtypes

The direct subtypes are the classes whose header names the item. The editor
asks again for each level. A list is cut at `oxabl_daemon::hierarchy::MAX_LISTED`
classes and the answer says so: the subtypes of `Progress.Lang.Object` are
every class with no parent, and a workspace can have thousands. `textDocument/implementation` on a type lists the
classes (not the interfaces) below it, through any number of headers: the
classes that implement or extend it, also cut at `MAX_LISTED`. A class that implements an interface
through its parent is listed.

## Methods

A method of a class overrides or implements a method of a supertype when both
have the same name and the same parameters. Different parameters are an
overload. `same_signature` compares the lists of the index
(`MemberDescriptor::params`): the number of parameters, the mode of each, and
its type. It answers `Yes`, `No` or `Unknown`:

| Parameter | Comparison |
| --- | --- |
| Primitive types | Equal types are `Yes`, different types are `No`; an `&IF` type is `Unknown` |
| `LIKE` | Two lists that write the same `table.field` (and the same `EXTENT`) are `Yes` when no temp-table or buffer can have the name of the table: none in the subclass (or the class at the cursor) and in every class and interface above it, through any number of headers, and none in the interface or class that declares the other method and every type above it. Every member list must be whole, and every supertype must resolve and be read. A member list is not whole when an `{include}` or an `&IF` can add a member, or when the class holds a preprocessor reference (`{&TTDEF}`, `DEFINE TEMP-TABLE {&TT}`, a value that ends a declaration and starts another) and its expansion does not settle. A temp-table defined `LIKE` or `LIKE-SEQUENTIAL` another table, a static temp-table and a work table count by their own names. An interface counts because oxabl does not model whether a class sees the temp-tables of its interfaces. Then both read the database field. Any other `LIKE` is `Unknown`: a name with no table can be a variable, and a different spelling can name a field of the same type. The rule takes one schema for the two classes: a class compiled against another database, or under another alias, can read another field |
| Class types | Each name is resolved through the imports of the file that declares the method (`ImportContext::resolve_class`). The same class is `Yes`, two classes are `No`. A name that resolves in no file is `Yes` when both lists write it alike under equal imports, else `Unknown`. A .NET type, or a name that two files can supply, is `Unknown` |
| `TABLE`, `BUFFER`, `DATASET` | `Unknown`: the table is not in the list |
| A list that an `{include}` or `&IF` hides | `Unknown` |

The facts of a class file that names an include, or that holds a `{&NAME}` or
`{N}` reference anywhere in a class (its header, a member, a method body),
come from its expanded text when its expansion settles: every include
resolves and the preprocessor gives no warning. A reference that expands to
nothing, such as a Prolint suppression `{&_proparse_ ...}`, then changes no
fact. When the expansion settles and its text does not parse, the facts are
read as written, and the member list is whole only when the value of each
reference in the class has no period or colon that can end a statement. Then a list that an include writes, with the
`{&NAME}` values of the file that includes it, is compared as written there.
One include read with two values of a name gives two lists: an overload, or
`Unknown`, and never `Yes`. When the expansion does not settle (an include
that does not resolve, also one outside the list, which can define a name
that the list reads), a list that holds an include or a preprocessor
reference is `Unknown`. The parser skips such a reference in a parameter
list, so the facts never take the rest of the list for the whole list.

A method counts only when it is not `PRIVATE` and not `STATIC`. An `Unknown`
match is listed and says that it may be an overload.

- **Implementation of a method.** The classes below the class of the method
  that declare a method like it. An interface that extends another declares
  no body, so it is never listed. For an interface method, a class below the
  interface that declares nothing of the name still implements it through the
  public method it inherits (the compiler accepts this): the nearest
  declaration up its parent chain is listed, also when that class is not
  below the interface. Only a public method counts on that walk, because the
  compiler needs a public method for an interface method. A parent that oxabl
  cannot resolve on that walk is a gap. A class in an inheritance cycle is never listed as its own
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
  so. When the outline lists fewer methods of the name than the facts (a
  preprocessor name writes a declaration), the answer gives the methods it
  places and the class name, and says that oxabl cannot place every method.

`Progress.Lang.Object` has four methods (`Clone`, `Equals`, `GetClass`,
`ToString`) that oxabl does not model. A super method search for one of these
names, in a class whose parent chain ends there without a match, says so, as
the member lookup does (`docs/design/class-members.md`). An interface match
does not remove this gap: the same method can also override the `Object`
method.

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
| The workspace index is not ready, is out of date, or the pass ran again and is still behind (the unit tests of `class_files_of` pin the text; no stdio test reaches this state) | This answer |
| No include path is configured, so the classes are not searched | This answer |
| The index read only the first `MAX_CLASSES` class files | This answer |
| The list has more than `MAX_LISTED` classes, so it is cut | This answer |
| The class is an enum: its built-in parent is not listed | This answer |
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

The workspace pass is not changed. It builds its own graph and does not fill
the memoized facts, so the build that follows it reads the facts of each class
file once, on one thread, and builds the index (the table below gives those
times). The daemon keeps the index while its stamp is the same, so a request
that comes after the build reads no file and builds nothing. A request that
comes after an edit builds the index again from the memoized facts: it resolves
every header (the file probes of `find_path` are not memoized, by design, so a
new file is seen) with a lookup table for each distinct class name for the
length of the build. The memo of facts keeps the cost low for a file that did
not change.

The `hierarchy_bench` bench (400 direct subclasses of one class, one thread)
measures the build and the request apart, because CodSpeed counts only the
thread that runs the bench. `build_after_pass` and `build_after_edit` run the
build on the bench thread (`build_subtype_index_here`). `first_subtypes` and
`next_subtypes` time the request on a built index, and fail when the request
starts a build. Counted with callgrind, the first request of a session that
built its index in the background costs 19 million instructions; before the
kept index, the first request built the index itself and cost 61 million. The
cold build costs 47 million, and the build after an edit of one class 10
million. `edit_pass_bench` checks that a pass of a session that never asked
starts no build: its `edit_pass/leaf` count is the count of a pass alone.

A class file with a preprocessor reference is expanded when its facts are
read: one preprocessor run and a second parse. Over 200 generated class files
of 30 methods, half of them with a Prolint line at class level and
`{&LINE-NUMBER}` in some method bodies, the facts cost 977 million
instructions (callgrind) against 679 million when such files were read as
written, about 1.9 times for a file with a reference. A file with no
reference costs the same as before. In return, those files keep whole member
lists.

Measured with a release build on a 4-core machine over a generated workspace:
one base class and N classes in chains of ten, each file 362 lines and 15 KB
(40 methods with bodies). Times are for one run.

| Class files | Workspace pass (first request of any kind) | First subtypes request, facts not yet read | Next subtypes or implementation request |
| --- | --- | --- | --- |
| 500 | not measured | 0.22 s in all, with the pass | 0.01 s |
| 2,000 | 0.55 s | 0.76 s in all, so about 0.2 s for the facts | 0.02 s |
| 5,000 | 1.30 s | 1.89 s in all, so about 0.6 s for the facts | 0.04 s |

The first reading of the facts is serial and costs about 0.12 ms for each class
file of this size. It is checked for a stop before each file, so an edit does
not wait for it.

**Placing the items.** An item of a list is placed at the name of its class.
The name span comes from the lexed head of the file (`class_name_span`), checked
against the name that the index holds, and not from a parse of the whole file.
A header that the head does not show plainly is placed with the outline. A list
is cut at `MAX_LISTED` items and checks for a cancel before each one, so the
placement costs a bounded amount. The index build, the filter over all nodes and
the search bases of root-relative paths still grow with the number of class
files. The `range` of a list item is the name of its class, the same as its
`selectionRange`; an item from prepare or supertypes has the whole declaration
as its `range`.

**Root-relative paths.** With `root_relative_include_paths`, each class file
needs its search base, which comes from its package root. The daemon reads each
file and lexes its head once for each request (`HierarchyUnit::resolver`), and
`search_base` lexes only the head of the file, at most 4 KB, when the head shows
the header.

Measured with 3,000 class files of 186 lines, 2,002 of them with no parent, one
run each, on the same 4-core machine, warm (the second request):

| Request | Build | `include_paths` | `root_relative_include_paths` |
| --- | --- | --- | --- |
| Subtypes of a class with 999 direct subtypes | release | 0.09 s | 0.19 s |
| Subtypes of a class with 999 direct subtypes | debug | 0.55 s | 1.38 s |
| Subtypes of `Progress.Lang.Object`, cut at 1,000 | release | 0.09 s | 0.17 s |
| Subtypes of `Progress.Lang.Object`, cut at 1,000 | debug | 0.57 s | 1.60 s |

The first request, with the facts not yet read, takes 0.42 s (release) and
3.7 s (debug) with `include_paths`, and 0.85 s and 5.2 s with root-relative
paths. Before the cap and the head lexing, a debug build took 6.6 s for the
same request with root-relative paths and 9.0 s for the subtypes of
`Progress.Lang.Object`, with a 1.2 MB answer.

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
definition and the hierarchy must read alike; a workspace class that oxabl cannot read, which is not
taken for the built-in type of the same name; an interface match that does not
hide an `Object` override; parameter lists that one include writes, compared
after preprocessing, with two values of a preprocessor name, and with an
include that does not resolve (also in `reference_lens_lsp.rs`, for the mark of
calls through a supertype); `LIKE` parameters written alike, with a buffer,
a temp-table or an unread include in the subclass, the base, a class between
them, a class above the matched parent, or an interface that the matched
interface extends, a parent that does not resolve, and another `EXTENT`; a temp-table or
buffer whose name, or whole definition, a preprocessor name writes; a
method that the outline does not list, alone or with an overload it lists; an enum; a protected method that does not implement
an interface method; and a list cut at `MAX_LISTED`.

`crates/oxabl/tests/hierarchy_index_lsp.rs` tests the kept index and the
progress through the real `oxabl lsp`. A test build of the daemon writes a line
to stderr for each build as it ends (`built`, `stopped` or `failed`; `built` is
an index that is kept), and one when a build waits at a class file that has
`OXABL-TEST-HOLD:daemon.hierarchy-index@<ms>` (with `OXABL_TEST_INDEX_COUNTS`
set), and the tests count these lines. A class file with
`OXABL-TEST-PANIC:daemon.hierarchy-index` makes the build panic. They test:

- a pass of a session that asked for no index builds none, and the first
  request builds it;
- after the first request, a pass builds the index, and a request after it
  builds nothing and shows no progress; a second request builds nothing;
- an edit of an open class, and a change of a class file on disk, make the next
  request build the index again; a class file opened and closed with the text
  that the pass read does not; a watched-file event does;
- an edit that comes while the build waits at a class file is answered at once,
  and the build is stopped and keeps nothing;
- a newer pass stops the build of an older one, and only the newer index is
  kept, also when the older build ends with an index after the stop;
- a build that panics fails each request that waits for it with the reason,
  and is not kept;
- a request that waits for the build shows `begin`, `report` and `end` on its
  own token, with counts that do not go back; with no token it asks the client
  for one (`window/workDoneProgress/create`) and starts when the client has
  answered; a client that does neither, or that answers `create` with an
  error, gets nothing;
- a request that waits for the workspace pass shows the files analysed
  (`OXABL-TEST-HOLD:daemon.pass-file@<ms>` holds the pass at a file);
- a request that is cancelled while it waits answers at once, after the `end`
  of its progress, and the build goes on: the next request finds its index, and
  only one index was built.

`crates/oxabl_pipeline/tests/class_name_span.rs` and
`crates/oxabl_index/tests/search_base_head.rs` check the two readers of the head
of a file against the readers of the whole file. The
`vitest` tests of `clients/vscode/src/superMethodModel.test.ts` pin what the
command shows.
