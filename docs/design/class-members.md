# Types of class members

This document is the design of GitHub issue #437: the semantic layer gives a
type to a member access (`obj:Name`) and to a method call (`obj:Greet()`) on a
class of the workspace. The implementation steps are in
`docs/design/class-members-steps.md`.

Today the type-check pass gives every `MethodCall` and `MemberAccess` the type
`Unknown`. After this work it gives the declared type of the member when oxabl
knows the class of the receiver, finds exactly one member, and knows that the
member set it searched is complete. In every other case the type stays
`Unknown`.

## The rule that decides every case

**Fail closed.** A fact that oxabl does not know gives `Unknown`, never a
guess. `Unknown` is the bottom of the type lattice: `type-mismatch-assignment`
(LINT0004) and `signature-mismatch` (CL003) do not judge it. So a missing fact
removes a finding and never adds one. Each rule in this document says what it
does when a fact is missing, and the answer is always `Unknown`.

## Terms

| Term | Meaning |
| --- | --- |
| Receiver | The expression on the left of `:` in `obj:Name` or `obj:Greet()`. |
| Receiver class | The class or interface the receiver holds, or names. |
| Receiver role | `Instance` (a value of the class), `TypeName` (a static `Class:Member`), `ThisObject`, or `Super`. |
| Surface | All members a class offers: its own members and the members of its supertypes, as the role can reach them. |
| Candidate | One declaration of a method name on the surface. Several candidates of one name are an overload set. |
| Complete surface | A surface where oxabl read every level. See "When a surface is not complete". |

## What exists before this work

The resolve pass (`crates/oxabl_semantic/src/resolve.rs`) already does part of
the work:

- `resolve_member_reference` resolves a member through a receiver when an index
  is attached and the receiver is a **workspace** class: a variable or
  parameter `AS` that class, a `NEW` of it, or its type name. It records the
  member symbol on the node of the `MemberAccess` or `MethodCall`.
- `walk_member_surface` walks the supertypes of a class through the workspace
  index once per class (breadth first, a visited set stops a cycle) and keeps
  the members by `(class, name)`. `lookup_inherited` mints a symbol for a
  member when a reference names it, with the declared type on
  `Symbol::data_type`.
- An overload set of a workspace class is folded into one symbol. Its type is
  the type all candidates agree on, else `Unknown` (`InheritedMember`). A local
  overload set keeps each declaration (`SymbolTable::method_overloads`), and
  `overload_set_data_type` applies the same agreement rule.
- A class-typed member of a workspace class (`MemberType::Named`) gets the type
  `Unknown`, so a chain stops after one link.
- A local method hides the inherited overloads of its name. In
  `CLASS SubScorer INHERITS Scorer` with an own `DATE Score(DATE)` and an
  inherited `INTEGER Score(INTEGER)`, the call `Score(1)` is typed `DATE` and
  `Score(?)` is reported as a `DATE` into a `LOGICAL`. Both are wrong today:
  the first message names the wrong type, and the second is a false positive.
- A receiver that is a class of the analysed file, `THIS-OBJECT`, `SUPER`, a
  `CAST`, or a member, resolves nothing.

The type-check pass (`crates/oxabl_semantic/src/check.rs`) ignores all of this
and writes `Unknown` for both node kinds.

The parser drops three facts this work needs (`crates/oxabl_parser`):

- `EXTENT` on a property type and on a method return type. A property
  `AS CHARACTER EXTENT 3` is a `CHARACTER` in the AST today.
- The `INPUT`, `OUTPUT` and `INPUT-OUTPUT` keywords of a method call argument.
- The `TABLE`, `TABLE-HANDLE`, `DATASET`, `DATASET-HANDLE` and `DATA-SOURCE`
  keywords of a method call argument (the name after them stays as an
  argument).

The first gap is fixed by this work (step 1). The second and third stay; the
overload rules below do not depend on them.

## Receivers

The resolve pass finds the receiver class and the role with one function,
which replaces `receiver_indexed_class`. It reads only facts the pass already
recorded for the receiver node, so the receiver is always walked first.

| Receiver | Receiver class | Role |
| --- | --- | --- |
| A name whose declaration is `AS Cls` (variable, parameter, property, data member) | `Cls` | Instance |
| `NEW Cls(...)` | `Cls` | Instance |
| `CAST(expr, Cls)` | `Cls` (the type operand, which the pass resolves already) | Instance |
| A member or a call whose type is a class (`a:Prop`, `a:Make()`) | That class | Instance |
| `THIS-OBJECT`, in a class body | The class of the body | ThisObject |
| `SUPER`, in a class body with `INHERITS` | The class of the body; the lookup starts at its parent | Super |
| A type name: `Cls`, `pkg.Cls`, or a name that a `USING` import makes a class | `Cls` | TypeName |
| A member of an enum (`Shade:Dark`) used as a receiver | The enum | Instance |

A type name is found with the order that `resolve_type_name` already uses: the
scope tree first, then the `USING` imports, then the wildcard prefixes, then
the unnamed package, then the implicit `Progress.Lang` import. A name that the
scope tree binds as a variable, a buffer or a frame is never a type name.

A chain (`a:b():c`) is resolved one link at a time: the type of `a:b()` is the
receiver class of `:c`. The resolve pass knows the number of arguments of
`b()` but not their types (the type-check pass computes those later). So a
chain link after a method call has a class only when the candidates of that
arity all return the same class. A link after a call that only the argument
types can pick stays `Unknown`. The type-check pass still picks the type of
the last call; see "Overloads".

### Receivers that stay Unknown

- A receiver whose class oxabl cannot resolve: no index attached, an index
  with no search path, a class no file declares, a file oxabl cannot parse,
  two files that supply the name, or a bare name whose file may import more
  than oxabl read (an `{include}` before the class header).
- A handle (`HANDLE`, `COM-HANDLE`, a system handle such as `SESSION`). Its
  attributes and methods are not class members.
- A built-in class (`Progress.*`, `OpenEdge.*`) and a .NET class. See
  "Out of scope".
- `DYNAMIC-NEW`, `DYNAMIC-CAST`, `DYNAMIC-INVOKE`, `DYNAMIC-PROPERTY`,
  `GET-CLASS`, and `TYPE-OF`.
- A generic type (`List<Thing>`), a class-typed `EXTENT` (`AS Thing EXTENT 3`)
  and its elements, a type an `&IF` chooses, and a `LIKE` type.
- `THIS-OBJECT` and `SUPER` outside a class body, and `SUPER` in a class with
  no `INHERITS`.
- A widget attribute (`:SCREEN-VALUE`), a buffer field through `::`, and an
  event (`obj:Done:Subscribe(...)`).

## Member lookup

### The surface of a class

| Receiver class | Own members come from | Supertypes come from |
| --- | --- | --- |
| A class of the analysed file | Its body scope in the scope tree, as the declare pass bound them | `walk_member_surface` in the `Subclass` role |
| A workspace class | `WorkspaceIndex::class_members` | `walk_member_surface` in the `Outside` role |

Methods are looked up in the `Functions` namespace and only from a
`MethodCall`. Properties, variables, datasets and data-sources are looked up in
the `Values` namespace and only from a `MemberAccess`. So `obj:Name()` never
finds a property `Name`, and `obj:Greet` never finds a method `Greet`.

The nearest declaration of a property or data member wins, as the walk does
today. ABL does not let a subclass declare a property of its parent again
(only `OVERRIDE` of an abstract one, with the same type), so the nearest one
has the type the AVM uses.

A method collects **all** candidates of its name from all levels, nearest
first. For a class of the analysed file this means its own overloads **and**
the inherited ones; today the own ones hide the inherited ones. Two
candidates with the same parameter list (an `OVERRIDE`, or a method and the
interface method it implements) count once: the nearest stays. ABL requires
an override to keep the return type, so the one that stays has the right
type.

### Access

The role decides which members are reachable:

| Role | Reachable |
| --- | --- |
| The receiver class is the class of the analysed file (`THIS-OBJECT`, or a value of that class) | All own members; `PUBLIC` and `PROTECTED` inherited members |
| `Super` | `PUBLIC` and `PROTECTED` members of the parent and its supertypes, never the own members |
| `Instance` or `TypeName` of any other class | `PUBLIC` members only |

`PACKAGE-PRIVATE` and `PACKAGE-PROTECTED` members of another class are not
reachable from outside it: the index does not model packages. A member that
the role cannot reach resolves to `Unresolved { reason: PresentButUnusable }`,
as today. LINT0001 does not report that reason, and its type is `Unknown`.

Access does not change the type of a member. It decides only whether oxabl
gives a type. A compile error in ABL (an inaccessible member) gives `Unknown`.

### Static and instance members

A `TypeName` receiver reaches only `STATIC` members. An `Instance`,
`ThisObject` or `Super` receiver reaches only instance members. The other
combinations keep the resolution (so go to definition still works) and give
the type `Unknown`, because oxabl does not know for sure what the compiler
accepts there. An unqualified name inside the class reaches both, as today.

### Enums

An enum member is a static property whose type is the enum itself:
`Shade:Dark` has the type `Class(Shade)`. The index gives it
`MemberType::Named(<the enum name>)`. A method of an enum
(`Shade:Dark:GetValue()`) comes from `Progress.Lang.Enum`, which this work
does not model, so it is `Unknown`.

### When a surface is not complete

A method call can pick the wrong candidate when oxabl does not see all
candidates. So a method call on an incomplete surface has the type `Unknown`.
A property or data member that oxabl finds still has its type (see "The
surface of a class"). A surface is not complete when:

1. **An `INHERITS` ancestor was not read**: no file supplies it, its file does
   not parse, the index declined to choose between two files, or it is a
   built-in class (`INHERITS Progress.Lang.AppError`). The walk already
   records this as an opaque surface (`CrossFileState::opaque_surfaces`). An
   unread `IMPLEMENTS` interface of a concrete class does not count: the class
   or one of its read ancestors declares the methods of its interfaces. For an
   interface receiver and for an `ABSTRACT` class, every unread supertype
   counts.
2. **A level's member list is not whole.** A class file whose body names an
   `{include}` that the index could not read, or holds an `&IF` around a member
   declaration, may declare more members than the index saw. The index says so
   with `ClassDescriptor::members_complete = false`. For the class of the
   analysed file, the same holds when an include did not arrive inside its body
   (`AnalysisContext::elided_includes`) or when the file is an include fragment
   analysed alone.
3. **The inheritance has a cycle.** `CLASS a INHERITS b` with
   `CLASS b INHERITS a` does not compile. The walk stops on its visited set,
   as today, and every member of a class on the cycle has the type `Unknown`,
   properties too.
4. **The method name is a method of `Progress.Lang.Object`** (`Clone`,
   `Equals`, `GetClass`, `ToString`). Every class inherits these, and oxabl
   does not model their signatures until #154. So `obj:ToString()` is
   `Unknown`, also when the class overrides it.

### Includes that hold part of a class

- A class of the analysed file: the pipeline expands its includes before the
  analysis, so the members an include declares are in the scope tree.
- A workspace class: the index already expands a class file that names an
  include (`oxabl_index::index_file_with_includes`). When every include was
  read, the facts come from the expanded text and are whole. When one was not,
  the facts come from the text as written and the class is marked
  `members_complete = false`.

### Cycles and termination

`walk_member_surface` and `ClassLattice::widens_to` both stop on a visited
set. A chain stops when a link has no class. No new recursion is added: the
resolve pass walks the chain as it walks any nested expression, and
`Parser::MAX_CHAIN` bounds the length of a chain.

## Member types

| Member | Type |
| --- | --- |
| Property, variable data member | Its declared type; `EXTENT n` gives `Array { element, extent }` |
| Method | Its return type; `EXTENT` as above; `VOID` gives no type, so the call is `Unknown` |
| A class-typed property or method (`AS app.Thing`) | `Class(<the symbol of app.Thing>)` |
| Enum member | `Class(<the enum>)` |
| `LIKE`, a type an `&IF` chooses, a generic type, a class-typed `EXTENT` | `Unknown` |

A class name in a member declaration means what the **declaring** file's
imports make it mean. For a workspace class the resolve pass resolves the name
through `ClassDescriptor::imports` with `search_in_imports`, as it does for a
supertype name. So the index carries the imports also for a class that names
no supertype but has a class-typed member or parameter (today it carries them
only for a class with a supertype). A bare name in a file whose imports oxabl
did not read whole (`ImportContext::has_unseen_imports`) gives `Unknown`.

The resolve pass resolves the name when it mints the member symbol, and
records the lookup in `SymbolTable::record_class_lookup` (with no site: this
file does not write the name). A local method's class-typed return type is
upgraded in `upgrade_class_types`, for each declaration of an overload set.

## Overloads

### Candidates

The candidates of a call are the methods of its name on the surface that the
role can reach, with the static rule applied. Each candidate has its
parameter list and its return type. A parameter list is not known when an
`&IF` or an `{include}` supplies part of it in a class file of the workspace,
or when a parameter is `LIKE` something; such a candidate is a "maybe" for
every argument it cannot judge.

The parameter list is a new fact:

- `MemberDescriptor::params: Option<Arc<[MemberParam]>>` from the index, with
  the mode, the shape (value, `BUFFER`, `TABLE`, `TABLE-HANDLE`, `DATASET`,
  `DATASET-HANDLE`) and the type (`MemberType`, so a class name keeps its
  spelling).
- A side map of the symbol table for each candidate symbol, with the class
  names resolved in this file (`CallParam`, with a `ResolvedType`).

One function builds the portable list from the AST parameters, for the declare
pass and for the index, so a local method and a workspace method are read by
the same code.

### How an argument fits a parameter

Each argument gets one fit against each parameter at its position:

| Fit | When |
| --- | --- |
| Exact | The argument type and the parameter type are the same: the same primitive, the same class, or arrays with the same element and extent. |
| Widen | The parameter is `INPUT` and the argument widens to it, or the parameter is `OUTPUT` and the parameter type widens to the argument. Primitive widening: `INTEGER` to `INT64` to `DECIMAL`, `CHARACTER` to `LONGCHAR`, `DATE` to `DATETIME` to `DATETIME-TZ`. Class widening: a subclass to its parent or to an interface it implements, through a complete chain (see "Assignability"). An `INPUT-OUTPUT` parameter takes only Exact. |
| No | The two types are of families that never convert: a class and a primitive, a class whose complete chain does not reach the class it must widen to, a scalar and an array, `LOGICAL` and any other primitive, a character type and a numeric, date or `LOGICAL` type, a numeric type and a date type. |
| Maybe | Anything else: an argument of type `Unknown` or `Error` (also `?`), a `BUFFER`, `TABLE` or `DATASET` shape on either side, a parameter type oxabl does not know, a narrowing between numeric types, a class chain that is not complete. |

"No" removes a candidate, so a "No" must be sure. When oxabl is not sure, the
fit is "Maybe", which never removes a candidate.

The argument mode is not in the AST, so the fit uses the parameter's mode. A
call that compiles writes the mode the parameter has, so this is the mode of
the argument too. Two candidates that differ only by mode both fit, and the
rules below then give `Unknown` unless they return the same type.

### The pick

1. Keep the candidates whose parameter count equals the argument count, or
   whose parameter list is not known.
2. Give each kept candidate the worst fit of its arguments: No, then Maybe,
   then Widen, then Exact. Remove the candidates with a No.
3. If exactly one candidate is Exact and no candidate is Maybe, the call has
   its return type.
4. Else, if no candidate is Exact, exactly one is Widen, and none is Maybe,
   the call has its return type.
5. Else, if at least one candidate is left and all left candidates return the
   same type, the call has that type.
6. Else the call is `Unknown`.

No rule ranks two Widen candidates against each other. ABL picks the one with
the fewest widenings; oxabl does not model that ranking and gives `Unknown`
(rule 6) unless the two return the same type (rule 5).

Rule 5 is the rule that `overload_set_data_type` applies today, and it never
guesses: whichever candidate the compiler picks, the value has that type. It
applies only to a complete surface. The acceptance criterion of #437 reads
"when the arguments do not pick exactly one overload, the result is Unknown".
Rule 5 gives a type in a case the arguments do not pick. This design keeps it,
because the type is a fact, and removing it would make `obj:Same(?)` lose a
type that a local call `Same(?)` has today. If the maintainers choose the
strict reading, remove rule 5; nothing else changes.

| Candidates | Call | Type |
| --- | --- | --- |
| `INTEGER Score(INTEGER)`, `CHARACTER Score(CHARACTER)` | `Score(1)` | `INTEGER` (rule 3) |
| same | `Score("a")` | `CHARACTER` (rule 3) |
| same | `Score(?)` | `Unknown` (both Maybe, types differ) |
| same | `Score(1, 2)` | `Unknown` (no candidate of that count) |
| `INT64 Widen(INT64)`, `DECIMAL Widen(DECIMAL)` | `Widen(iInteger)` | `Unknown` (two Widen) |
| same | `Widen(iInt64)` | `INT64` (rule 3) |
| `LOGICAL Same(INTEGER)`, `LOGICAL Same(CHARACTER)` | `Same(?)` | `LOGICAL` (rule 5) |
| `INTEGER Fill(INPUT CHARACTER)`, `CHARACTER Fill(OUTPUT CHARACTER)` | `Fill(c)` | `Unknown` (two Exact) |
| `INTEGER Pick(app.Person)`, `CHARACTER Pick(app.IShape)` | `Pick(oEmployee)` | `INTEGER` (rule 4: the employee widens to the person; it does not implement the shape) |

The same function serves three places:

- the type-check pass, for the type of a `MethodCall`, with the argument types
  it has just computed;
- the type-check pass, for an unqualified call of a method inside its class
  (`Score(1)`, a `FunctionCall` that resolved to a method), so that `Score(1)`
  and `THIS-OBJECT:Score(1)` always agree;
- the resolve pass, for a chain link, with every argument type `Unknown` (so
  only rule 5 can give a class).

A single non-overloaded method goes through the same rules: with one candidate
of the right count and no No, rule 5 gives its type.

## Assignability

`ClassLattice::widens_to` climbs from a class through
`SymbolTable::resolved_supertypes`. A supertype that did not resolve is not in
that list, so a class whose chain stops at an unresolved workspace class looks
like a class with no parent. Then `oTop = NEW app.Bottom()` is reported as a
mismatch when `app.Bottom INHERITS app.Middle` and no file supplies
`app.Middle`, though `app.Middle` may inherit `app.Top`. This false positive
exists today; member types would add more of it (`oTop = oFactory:MakeBottom()`).

This work makes the lattice fail closed. The resolve pass records each class
whose header names a workspace supertype that did not resolve
(`TypeLookup::NotFound`, `Unusable` or `Unknowable`). `widens_to` gives
"unknown" when its climb reaches such a class before it finds the target, and
`assignable` treats "unknown" as assignable. A built-in or .NET supertype does
not make the chain incomplete for a workspace target: a built-in class cannot
inherit a workspace class or implement a workspace interface.

## Where the work happens

| Pass | Work |
| --- | --- |
| Declare | Records the portable parameter list of each local method (side map, only for methods). Keeps `EXTENT` of properties and method return types. |
| Resolve | Finds the receiver class and role; looks up the member; mints synthesized symbols, one for each candidate of a workspace method, linked as an overload set with `record_method_overload`; resolves class-typed member and parameter types; records which overload sets are on an incomplete surface; records classes with an incomplete chain. |
| Type check | Types `MemberAccess` from the member symbol; types `MethodCall` and method `FunctionCall` with the pick; types `CAST(x, Cls)` as `Class(Cls)` and `THIS-OBJECT` as the class of its body; applies the static rule and the completeness rule. |

The type-check pass keeps its signature (`&SymbolTable`, `&references`): every
fact it needs is in the symbol table after the resolve pass.

New state goes into the boxed, lazily created `CrossFileState` of the resolve
walker or into the boxed side maps of `SymbolTable`, never inline:
`ResolveWalker` has a size assertion (320 bytes) and `Symbol` stays 64 bytes,
because both sizes are measured costs (see the notes beside them).

The resolution of the member on the `MemberAccess` or `MethodCall` node keeps
pointing at the anchor of the overload set. Navigation therefore keeps its
answer "each overload" for a method. Pointing at the picked overload is left
to #438 (signature help).

## Incremental analysis in the daemon

Salsa stays in `oxabl_daemon`. This work adds no query there. The new facts
(`params`, `members_complete`, the wider `imports`) are fields of
`MemberDescriptor` and `ClassDescriptor`, which `oxabl_index::index_file`
produces and which the daemon already memoizes per file and per class
(`indexed_facts`, `indexed_class`, `indexed_class_members`).

- **An edit of a method body** in a class file changes no fact, so the facts
  compare equal and salsa stops there (early cutoff). No dependent buffer is
  analysed again.
- **An edit of a signature, a member type or a `USING` line** changes the facts
  of that class. Every buffer that asked about the class is analysed again,
  and only those.
- **A class reached only as a return type** (`oFactory:Make():Name`) is asked
  through the same `SnapshotIndex::class` and `class_members` calls. Salsa
  records the dependency when the call is made, so an edit of that class
  reaches the buffer.

A class with a class-typed member now carries its imports, so an edit of its
`USING` lines invalidates its descriptor. Before this work it did so only for a
class with a supertype. This is the one place where the cutoff gets coarser,
and it is the correct direction: those lines change what the member types mean.

## Effects on existing rules and features

| Consumer | Effect | Guard against a false positive |
| --- | --- | --- |
| LINT0004 `type-mismatch-assignment` | Judges values and targets that are members (`i = obj:Name.`, `obj:Age = "x".`). | `Unknown` is skipped; the lattice fails closed on an incomplete chain; the completeness and static rules. |
| CL003 `signature-mismatch` | Judges a `RUN` argument that is a member, where a handle meets a type it has no conversion to. | Only `Primitive` types are compared, as today. |
| LINT0001 `undefined-symbol` | No change. A member that is not found records `PresentButUnusable`, which the rule does not report, for every receiver kind. | The rule's reason match is exhaustive; no new reason is added. |
| LINT0002, LINT0006 | A member resolved through `THIS-OBJECT` credits a read or a write to the local symbol. This can only remove a finding of "never read". | Counts are credited as for any reference. |
| Hover, go to definition, highlights, references, semantic tokens | Work on the new receivers (chains, `THIS-OBJECT`, `SUPER`, `CAST`, static, enum, a local class) with no code change, because they read the member resolution and `SymbolTable::member_file`. | A name with no resolution still has no answer. |
| `oxabl analyze` envelope | The `types` section has member types; the dependency section can name classes reached as return types. | None needed. |

Tests that pin the old boundary change in the same step as the behaviour, each
with its reason (listed in the steps document). No other expectation may
change.

## Cost

- **Single-file runs with no classes** (most `.p` files): the type-check pass
  does one more match on a `MethodCall` or `MemberAccess` node, and the
  receiver of a handle stops at its first check. Expected: no measurable change
  on the `tiny`, `medium`, `skipped`, `builtin_classes` and `schema_heavy`
  benches.
- **The declare pass** builds one small parameter list for each method. Files
  with no method pay nothing.
- **A class receiver** walks its surface once per run (already memoized per
  class). Each member reference is then one hash lookup. Each distinct
  class-typed member type costs one class lookup per run (memoized in
  `type_lookups`), plus the lookups of that class's supertypes.
- **The pick** runs once per method call with an overload set: a loop over the
  candidates and their parameters, with no allocation for up to eight
  candidates (`SmallVec`).

New benches, each with its own fixture (AGENTS.md, "Benchmarks"):

- `class_members` in `crates/oxabl_semantic/benches/semantic_bench.rs`: one
  class with properties, overload sets, and a method body of `THIS-OBJECT`
  chains, static members and calls, with no index.
- `class_members_indexed`: a program over two workspace classes through a stub
  `WorkspaceIndex` in the bench (a stub of the environment, not of the code
  under test), so the surface walk and the class-typed member lookup are
  measured.

Measure the existing benches against the base before and after, with
`valgrind --tool=callgrind` on instruction counts when the machine is loaded.
State the numbers in the pull request.

## Out of scope, and the seams left

| Work | Issue | Seam |
| --- | --- | --- |
| Members of built-in classes (`Progress.Lang.*`, `OpenEdge.*`), including the four methods of `Progress.Lang.Object` and the methods of `Progress.Lang.Enum` | #154 | The surface walk treats a built-in ancestor as opaque. #154 gives it members through the same candidate list, and then removes the opaque mark and the `Progress.Lang.Object` name rule. |
| .NET types | #465 | A `FROM ASSEMBLY` import already gives `TypeLookup::External`; a .NET receiver stays `Unknown`. |
| Completion after `obj:` | #436 | The receiver function gives the class and the role; the surface gives the reachable members. |
| Signature help, parameter inlay hints, the picked overload in hover and go to definition | #438 | The pick returns the candidate, not only its type. Argument modes need a change of the AST (`MethodCall::arguments`). |
| Go to implementation, super method, type hierarchy, rename of members | #439, #440, #441 | The candidates of a call and the member file of each. |
| Ranking of several Widen candidates | none | Rule 6 gives `Unknown` today. |
| A chain after a call that only argument types pick | none | The resolve pass would need argument types. |

## Implementation notes

The implementation follows this design. These points are more exact than the
text above, or differ from the steps document:

- **Untyped references.** The resolve pass marks each member reference whose
  type oxabl does not know (`SymbolTable::mark_untyped_member`, by node): a
  static rule mismatch, a class on a cycle, or a method whose candidates may be
  incomplete. The type-check pass gives such a node `Unknown`. The marks are
  per reference, not per overload anchor, because the role decides the static
  rule.
- **The anchor of an inherited overload set** carries the type of its own
  candidate. The agreement type of the set is
  `SymbolTable::overload_set_data_type`, as for a local set.
- **Local and inherited candidates.** For a method of a class of the analysed
  file, the inherited candidates are minted and recorded
  (`SymbolTable::inherited_candidates`) only when one of them has a parameter
  list that no own declaration has. An inherited method that an own method
  overrides is not minted, and nothing reaches it.
- **Interfaces of an abstract class.** `ClassDescriptor::is_abstract` carries
  the `ABSTRACT` of a workspace class, so rule 1 of "When a surface is not
  complete" applies to it as to a local class. An unread supertype counts for
  an interface or an abstract class also when it is a shipped interface.
- **No index.** A class header whose supertype oxabl did not look up (no index
  attached) does not make the lattice "unknown": a single-file run keeps its
  answer. A class of the analysed file is still searched for its own members,
  and its methods are `Unknown` when it inherits a class the file does not
  declare.
- **A class-typed `EXTENT` member** is `MemberType::Portable(Unknown)` in the
  index, the same `Unknown` that a local declaration gets.
- **Chain links.** The resolve pass walks each link of a receiver chain as a
  member reference, so each link also has a navigation site.
- **`DEFINE VARIABLE ... EXTENT n`.** The parser dropped an `EXTENT` that
  follows the type directly, so such a variable was typed as a scalar. The
  parser keeps it now, and an array variable has an array type. This is needed
  for `cTags = oP:Tags.` with `cTags AS CHARACTER EXTENT 3`.

## Tests

The black-box tests are `crates/oxabl/tests/class_members_cli.rs` (through
`oxabl check`) and `crates/oxabl/tests/class_members_lsp.rs` (through
`oxabl lsp`). They cover every acceptance criterion of #437 and every
fail-closed case in this document.
