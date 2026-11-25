# Built-in classes

This document is the design of GitHub issue #154: oxabl knows the documented
built-in classes of OpenEdge (`Progress.*`, `OpenEdge.*`, `Ccs.*`), gives a
type to their members, and reports a call that no overload of a built-in
method can take. The implementation steps are in
`docs/design/builtin-classes-steps.md`.

This work extends the type layer of user classes (#437,
`docs/design/class-members.md`). Read that document first: this document uses
its terms (receiver, role, surface, candidate, complete surface, the pick) and
changes only what it says about built-in classes.

## The rule that decides every case

**Fail closed.** A fact that oxabl does not know gives `Unknown`, never a
guess. The catalog of built-in classes comes from the Progress documentation,
and the documentation has gaps and errors (see "The data"). So the catalog
marks what it does not know, and each rule below says what oxabl does then.
The answer is always `Unknown`, or no finding.

Two kinds of facts have different risks:

- **A type.** oxabl gives a member a type only when the catalog gives the
  member. A wrong type can add a `LINT0004` finding to correct code. A type of
  a member that does not exist can not: correct code does not use that member.
- **An absence.** The new rules report a member that the catalog does not have,
  or a call that no overload takes. Such a finding is correct only when the
  catalog has *every* member of the class. So these rules judge only a class
  whose whole surface the catalog knows ("complete", below).

## What exists before this work

Phase 1 of #154 is on `master` (commit `a528bbc`, merged as `3a25fba`):

- `resources/progress_builtin_classes.json` is the scrape of the Progress
  documentation (`cargo run -p oxabl_codegen -- builtin-classes`). The old
  branch `feat/builtin-class-index` holds nothing that `master` does not have:
  `master` has its two commits and later fixes (generic names `<K,V>`,
  `Progress.Data.DataType`, `Progress.Lang.SoapFaultError`).
- `cargo run -p oxabl_codegen -- builtin-class-table` compiles the names,
  kinds, generic parameters and supertypes (not the members) into
  `crates/oxabl_semantic/src/builtin_classes.rs`: 1248 rows in a sorted static
  slice, a case-insensitive binary search, `builtin_class(name)`.
- The resolve pass asks the table after the workspace index, for each
  candidate spelling of a type name (`search_in_imports`), and answers
  `TypeLookup::Builtin`. A built-in name is therefore no `LINT0001` finding, on
  every client, with or without an index. The reference stays
  `Unresolved { reason: External }`, and the type stays `Unknown`.
- A built-in class has no symbol. A variable `AS Progress.Lang.AppError` has
  the type `Unknown`. A member through it resolves nothing.
- A user class that `INHERITS` a built-in class has an opaque surface: its
  methods are `Unknown`, its properties keep their types.
- The four methods of `Progress.Lang.Object` (`Clone`, `Equals`, `GetClass`,
  `ToString`) are `Unknown` on every class (`method_set_open`, a name rule).
- The bench fixture `builtin_classes` in
  `crates/oxabl_semantic/benches/semantic_bench.rs` measures phase 1.

The first acceptance criterion of #154 (names) is met. The second (members,
arity, types) and the third (no hot path cost) are the subject of this design.

## The data

### What the catalog holds

The scrape merges two documentation bundles:

| Bundle | Types | Renderer | Members |
| --- | --- | --- | --- |
| `openedge-abl-api-reference-128` | `OpenEdge.*`, `Ccs.*` (1140) | Javadoc-like, generated from the ABL source of the classes | Full tables |
| `abl-reference` | `Progress.*` (108) | DITA, one page per member | Signature code blocks, parsed by `parse_abl_signature` |

Counts in the checked-in file (OpenEdge 12.8): 1248 types (889 classes, 293
interfaces, 66 enums), 8852 methods, 3244 properties, 1368 constructors, 79
events, 10992 parameters, 1064 overload sets (the largest has 91 overloads).

Each type has its kind, package, page, supertype, interfaces and members. Each
member has its name, return type (the property type for a property), its
parameters (name and type), and `is_static`, `is_abstract`, `is_final`.

### What the catalog does not hold

| Missing fact | Effect |
| --- | --- |
| Parameter modes. The scraper discards `INPUT`, `OUTPUT`, `INPUT-OUTPUT` (`parse_param`). | The pick treats each position of a built-in candidate as a position of mixed modes (see "Overloads"). |
| Access. The scraper assumes every member is `PUBLIC` (module docs of `progress_docs.rs`). | oxabl may type a call that does not compile. That adds no finding to correct code. |
| `ABSTRACT` of a type. | Not used. |
| Descriptions. | The hover gives the signature and a link (#473 may add one line). |
| Members of `OpenEdge.*` and `Ccs.*` enums (58 enums). | Their member sets are not known. |

### Quirks and errors

The numbers are from a check of the checked-in file by the planner. Step 1 of
`builtin-classes-steps.md` tests the totals that these rules give.

| Quirk | Count | Rule |
| --- | --- | --- |
| Generic types: `HashMap<K,V>`, a parameter `T`, a member type `IList<T>` | 18 generic types; 55 member types name a type parameter, 18 a generic instance | The lookup key is the name without `<...>` (as today). A generic type is not modelled in this work: a type `List<X>` is `Unknown`, and a member type that names a type parameter or a generic instance is `Unknown`. |
| Array markers: `CHARACTER[]`, also `X EXTENT` and `X class EXTENT` | 640 with `[]`, 15 with `EXTENT` | `[]` at the end of a type is an `EXTENT` with no size. The normaliser writes every form as `[]`. A name that ends in `Extent` (`OpenEdge.DataAdmin.IExtent`) is a type, not a marker. |
| `CLASS JsonObject` (a `CLASS` keyword before the type) | 47 | The normaliser removes the keyword. |
| An unqualified type (`IArea`, `List`) | about 300 | The normaliser looks the name up in the package of the type that names it, then as a simple name that exactly one catalog type has. Else the member is damaged. |
| A kind word after the type (`X enumeration`) | 12 | The normaliser removes it. |
| `Enum type`, `Flag enum type`, `flag-enum-type` in an enum | 22 | The enum itself. |
| `parameter-value` (`ParameterList:SetParameter`) | 1 | A parameter of any type: `Unknown`. |
| A handle placeholder as a parameter type (`pds-handle`, `tt-handle`) | 7 | `HANDLE`. |
| `STATIC` as the return type of a constructor | 32 | A static constructor. It is not a candidate of `NEW`. |
| An `OpenEdge.*` or `Ccs.*` method with no return type | 3420 | `VOID` (the Javadoc renderer omits it). |
| A `Progress.*` method with no return type | 1 | Damaged. |
| A primitive return that is an `EXTENT` read as a scalar (`JsonObject:GetNames`, `OpenEdge.Core.String:Split(LONGCHAR)`, ...): the API reference writes the return type without `EXTENT`, the ABL Reference writes `CHARACTER` for `GetNames` | 26 found | An override (`extent_return`) for each one whose documentation comment says `@return <type>[]` or "an EXTENT" (found with a scan of the pages of the complete classes). `CL006` also fails closed for the rest: a scalar primitive that a built-in member returns never rules out a parameter that is an `EXTENT` of the same primitive (`overload::no_fit`). |
| A parameter name that is not an ABL name (`) PUBLIC MemoryOutputStream (INPUT size`) | 1 | Damaged. |
| A member name that is not an ABL name (HTML text, a code sample, `[ return-value = ] class-reference:Invoke`) | 71 in 35 types | Damaged. A name `&ltFile> Files` (7 of the 71: a generic return type that the renderer split) is read as the name `Files` with the generic type, which is `Unknown`. |
| A typo or a fragment as a type (`Progrss.Reflect.Flags`, `Progress.Lang.Class \|`, `INTEGER )`) | 53 members in 28 types | Damaged. |
| A .NET supertype or type (`System.Object`, `Telerik.*`) | 19 supertypes, 8 types | External. A level with a .NET supertype makes the surface opaque. A .NET type is `Unknown`. |
| No supertype on a class that has one (`Progress.Lang.AppError`, the `Progress.Util.*` helpers, `Progress.Data.*`, `Progress.IO.ByteOrder`) | 13 types | The header is not known: an override gives it, else the surface is opaque. |
| An undocumented supertype (`Progress.Web.AbstractWebRouter`) | 1 | The header is not known. |
| A wrong member (`Progress.Lang.Error` and `Progress.Lang.ProError` list `CallStack` as `file-name at line line-number`) | 2 | An override fixes it. |
| The same signature twice | 28 | The normaliser keeps the first. |
| A parameter with no name | 11 | Kept with no name. |

"Damaged" means: the normaliser drops the member, and the type is not
`members_trusted` (see below). It is never a reason to guess a signature.

### Overrides

`resources/progress_builtin_overrides.toml` holds fixes that a person checked
against the Progress documentation. The codegen applies them after the
normaliser. Each entry names a type and one of: a supertype, a member to drop,
a member to add, a member to replace. The codegen fails when an entry matches
nothing, so a later scrape cannot leave a stale fix behind. This work adds three
entries, and no more:

- `Progress.Lang.AppError` inherits `Progress.Lang.ProError`.
- `Progress.Lang.ProError` and `Progress.Lang.Error`: drop
  `file-name at line line-number`, add the property `CallStack AS CHARACTER`.

More fixes are later work, one pull request for each set, each with the page it
was checked against.

### Trust and completeness

The codegen computes two flags for each type, after the normaliser and the
overrides:

- `members_trusted`: no member of the type is damaged.
- `header_known`: every supertype resolves to a catalog type, or the type is
  `Progress.Lang.Object`, or it is an interface with no supertype. A .NET
  supertype, a missing supertype and an undocumented one make it `false`.

A built-in type is **complete** when it is `members_trusted`, `header_known`,
not generic, every supertype is complete, and, for an enum, the catalog lists
its members. The codegen computes this once (the catalog has no cycle; the
codegen fails on one).

With the three overrides: 1200 of 1248 types are `members_trusted` and 1065 are
complete. Of the 108 `Progress.*` types, 36 are complete, among them
`Progress.Lang.Object`, `Progress.Lang.Enum`, `Progress.Lang.FlagsEnum`,
`Progress.Lang.Error`, `Progress.Lang.ProError`, `Progress.Lang.AppError`,
`Progress.Lang.SysError`, `Progress.Json.ObjectModel.JsonObject`,
`Progress.Json.ObjectModel.JsonConstruct` and
`Progress.Json.ObjectModel.JsonDataType`. `Progress.Lang.Class`,
`Progress.Json.ObjectModel.JsonArray`, the `Progress.Reflect.*` classes, the
`Progress.IO` streams and every generic type are not.

### The data form

The codegen compiles the JSON, the normaliser and the overrides into static
Rust tables in `crates/oxabl_semantic/src/builtin_classes.rs`, as today. The
JSON stays the checked-in source of truth of the scrape, and the overrides file
the source of the fixes. Nothing is parsed at run time.

- `BUILTIN_CLASSES` keeps one row for each type and adds: the flags above, the
  documented name, the page URL, and the ranges of its members, constructors
  and enum members in the flat tables below.
- `BUILTIN_MEMBERS`: one flat array of all members, sorted by the case-folded
  name inside the range of each type, so a lookup is a binary search with the
  allocation-free comparator that `builtin_class_in` uses. The sort is stable:
  the overloads of one name keep the order of the catalog. A row has the
  documented name, the kind (method, property, event, constructor, enum
  member), the `STATIC`, `ABSTRACT` and `FINAL` flags, the type, and the range
  of its parameters.
- `BUILTIN_PARAMS`: one flat array of parameters: the name (empty when the
  catalog has none) and the type. A parameter has no mode field yet. The field
  is added when a catalog gives modes.
- A type is a small enum: `Void`, a primitive, an array of a primitive, a
  catalog type (its row index), an array of a catalog type, a parameter shape
  (`TABLE`, `TABLE-HANDLE`, `DATASET`, `DATASET-HANDLE`, `BUFFER`), and
  `Unknown` (a type parameter, a generic instance, a .NET type, any type). The
  codegen resolves each class name to its row once, so no lookup by name runs
  when a member is used.

Measured with a prototype of this layout on the checked-in JSON (a standalone
crate, x86_64, `opt-level = "s"`, LTO):

| Layout | Static data | Compressed (`gzip -9`) | Build time added (debug / release) |
| --- | --- | --- | --- |
| Flat arrays, string pool | 515 KB | 130 KB | 0.7 s / 0.7 s |
| One nested static slice per type (`&'static str` and `&'static [..]`, as the class table today) | 840 KB, and 720 KB of relocations in a shared library | not measured | 1.1 s / 1.2 s |
| The JSON, minified, with `include_str!` | 1.7 MB | 120 KB | parse at first use |

The nested layout has 16-byte pointers and lengths on x86_64. On `wasm32` a
pointer and a length take 8 bytes, the same as an offset pair, and a `wasm32`
module has no relocation entries, so the flat layout with `&'static str` names
costs about the same as a string pool there. The flat layout is the choice:
one array for each kind of row, no nested slices, names as `&'static str` for
readable code. The prototype did not
build for `wasm32` (the target is not installed on the machine of this plan).
Step 2 measures the real `oxabl_wasm` payload before and after, and the pull
request states both numbers.

### One catalog or one for each version

One catalog: the one that the scrape gives (OpenEdge 12.8). The catalog records
its version, and the hover shows it.

A project on an older OpenEdge can use a member that 12.8 has and its version
does not have. oxabl then types a call that does not compile there, which adds
no finding to correct code.

A project on a newer OpenEdge can use a member, or an overload, that a later
version added. The 12.8 catalog does not have it, so `CL007` and the count
verdict of `CL006` would report correct code. So when
`[workspace.preprocessor].proversion` is set and is newer than the catalog (by
major and minor number; a value oxabl cannot read counts as newer),
`CL007` and the count verdict of `CL006` stay silent
(`AnalysisContext::builtins_may_lack_members`). The type verdict of `CL006`
stays: a type that never converts does not convert in a later version either.
With no `proversion`, oxabl assumes the catalog's version. The same limit
holds when the `OpenEdge.*` source on the PROPATH of the project is of another
version and is not in `include_paths`: oxabl reads the 12.8 catalog for it.

The seam for more catalogs: the resolve pass reads the tables through one
`&'static BuiltinCatalog` in `AnalysisContext`, which defaults to the 12.8
catalog. A second catalog is a second static value and a rule in the pipeline
that picks one from `proversion`.

## Name resolution

The order of `resolve_type_name` does not change (`resolve.rs`, its doc
comment): the scope tree, the name as written when it holds a dot, each
explicit `USING`, each wildcard `USING`, the unnamed package, the implicit
`USING Progress.Lang.*`. For each candidate spelling the workspace index is
asked first, then the catalog.

What changes is what a built-in answer gives.

### A symbol for each built-in type

The resolve pass mints one symbol for each built-in type that a file uses, as
it does for a workspace class (`indexed_class_symbol`): kind `Class` (an enum
too) or `Interface`, `NodeId::DUMMY`, the folded name, a memo in the boxed
`CrossFileState`. A side map of `SymbolTable` (`builtin_class(sym)`) gives the
catalog row back. The pass records its supertypes with
`record_resolved_supertypes`, minting the supertypes on the way, and records
`record_incomplete_chain` when the header is not known. So the class lattice
climbs a built-in chain like a workspace one.

The reference of a type name, a `NEW`, a `CAST` operand, a `USING` and a
static receiver becomes `Resolved(<the built-in symbol>)`, and the declared
type becomes `Class(<the built-in symbol>)`. This holds only when the answer is
**sure** (next section). Else the reference stays
`Unresolved { reason: External }` and the type stays `Unknown`, as today.

`LINT0001` does not change: a resolved name is no finding, and an `External`
one is no finding either. The dependency records do not change: a built-in type
is no dependency and no gap (`note_class_site` already returns for it).

### A sure answer

`TypeLookup::Builtin` gets a flag `sure`. An answer is sure when no earlier
candidate spelling could be a workspace class that oxabl did not look up:

- With an index that searches paths, every earlier candidate was looked up,
  and the answer is sure.
- With no index, or an index with no search path, a candidate that is not a
  catalog name was not looked up. If such a candidate comes before the built-in
  one, the answer is not sure.

Examples, with no index: `NEW Progress.Lang.AppError()` is sure (the first
candidate is the qualified name). `USING Progress.Lang.*.` then `AppError` is
sure. `USING acme.*.` and `USING Progress.Lang.*.` then `AppError` is not sure:
`acme.AppError` can exist. `USING acme.AppError.` then `AppError` is not sure.

A bare name in a file whose imports oxabl did not read whole
(`ImportContext::has_unseen_imports`) is not sure.

### A user class with the name of a built-in type

The compiler facts below were measured with OpenEdge 12.8.9 (`COMPILE`, the
PROPATH is the probe folder). oxabl follows them.

| Case | Answer | Compiler fact |
| --- | --- | --- |
| A class of the analysed file named like a built-in type (`CLASS AppError`) | The local class | The scope tree is step 1, as today. |
| A workspace file with the qualified name of an `OpenEdge.*` or `Ccs.*` type (`OpenEdge/Core/String.cls`) | The workspace class, with its members | These types are ABL classes in procedure libraries on the PROPATH (`$DLC/src/OpenEdge.Core.pl`). The first PROPATH entry that has the class wins, and the workspace entries come first. |
| A workspace file with the qualified name of a `Progress.*` type (`Progress/Lang/AppError.cls`) | The built-in type, with its members. The workspace file is ignored. | `NEW Progress.Lang.AppError()` then `e:Foo()` (a method of the workspace file) fails with error 12927 in class `Progress.Lang.AppError`, and `e:AddMessage("x", 1)` compiles. |
| A bare name that a workspace class supplies through an earlier candidate (`USING acme.*.` and `acme/AppError.cls`) | The workspace class | The earlier candidate wins, as today. |
| A bare name that a class in the unnamed package supplies (`AppError.cls` at a path root), with no `USING` | The workspace class | The unnamed package comes before the implicit `USING Progress.Lang.*`: `e:Bar()` (a method of the workspace file) compiles and `e:AddMessage("x", 1)` fails with 12927 in class `AppError`. |
| The same bare name with an explicit `USING Progress.Lang.*.` | The built-in type | An explicit wildcard `USING` comes before the unnamed package: `e:Bar()` fails with 12927. |

So a `Progress.*` candidate is never asked of the workspace index: the search
takes the built-in type at that candidate. An `OpenEdge.*` or `Ccs.*`
candidate asks the index first.

## Members

### Levels

A surface is a list of levels, nearest first (`class-members.md`, "The surface
of a class"). This work adds one kind of level:

| Level | Members come from | Supertypes come from |
| --- | --- | --- |
| A class of the analysed file | Its body scope | `symbols.supertypes` |
| A workspace class | `WorkspaceIndex::class_members` | `ClassDescriptor` |
| **A built-in type** | `BUILTIN_MEMBERS` | The catalog row (already resolved to rows) |

The walk (`walk_member_surface`) reads a built-in level from the static table.
It needs no index. So the members of a built-in receiver, and the built-in
levels of a user class, are found in a single-file run, in the WASM client and
in the language server alike. A workspace level still needs the index: with no
index, a user class with a workspace parent stays as today.

A candidate of a built-in level points at its static row. It holds no
allocation, no file and no imports. When the resolve pass mints the symbol of a
candidate:

- the data type comes from the catalog type: a primitive, an array
  (`Array { extent: None }`), a catalog type (`Class(<its built-in symbol>)`),
  else `Unknown`. `VOID` gives no type.
- `method_params` comes from the catalog parameters, and the symbol is marked
  "modes not known" (a side set of `SymbolTable`).
- `member_file` is not recorded. A side map records the catalog row instead
  (`builtin_member(sym)`), for the hover.

### `Progress.Lang.Object` on every surface

Every class and interface surface ends with the level `Progress.Lang.Object`,
once, after all other levels. A class that names it (`INHERITS
Progress.Lang.Object`) gets it once. Its candidates take part in the pick like
any other. An `OVERRIDE` of `ToString` in a nearer level has the same
parameter list, so the walk keeps the nearer one, which has the same type.

The name rule in `method_set_open` (`clone`, `equals`, `getclass`, `tostring`)
is removed. `obj:ToString()` is `CHARACTER` on every class with a complete
surface, also when the class overrides it.

An interface reference gets these members too. This is a compiler fact
(OpenEdge 12.8.9): with `INTERFACE IFoo` and `DEFINE VARIABLE i AS IFoo`,
`i:ToString()`, `i:GetClass():TypeName` and `i:Equals(i)` compile.

### Enums

- A built-in enum with members in the catalog (the 8 `Progress.*` enums) gives
  each member as a static property of the type of the enum:
  `Progress.ApplicationServer.AdapterTypes:SOAP` has the type
  `Class(<AdapterTypes>)`.
- A built-in enum with no members in the catalog (`OpenEdge.*`, `Ccs.*`) is not
  complete. A member name through it resolves to nothing
  (`PresentButUnusable`), and its type is `Unknown`.
- The methods of an enum value come from its supertypes:
  `Progress.Lang.Enum` (`GetValue`, `CompareTo`, `ToString`, `Equals`) and
  `Progress.Lang.FlagsEnum` (`IsFlagSet`).
- A user `ENUM` gets the supertype `Progress.Lang.Enum`, and a `FLAGS` enum
  `Progress.Lang.FlagsEnum`. So `app.Shade:Dark:GetValue()` is `INT64`. This
  replaces the sentence of `class-members.md`, "Enums", that keeps it `Unknown`.

### Static members, overloads, access

The rules of `class-members.md` hold without a change:

- A `TypeName` receiver (`Progress.Lang.Class:GetClass(...)`,
  `OpenEdge.Core.String:IsQuoted(...)`) reaches the static members; an instance
  receiver the instance members. A set that mixes both for the role gives
  `Unknown` (`OpenEdge.Core.String` has an instance `Split()` and a static
  `Split(LONGCHAR)`: `oStr:Split(",")` is `Unknown`).
- The catalog has no access, so every role reaches every built-in member
  (see "Implementation notes").
- A constructor is a candidate of `NEW` only. A static constructor is not.

### Overloads with modes that are not known

The pick (`class-members.md`, "How an argument fits a parameter") reads the mode
of a parameter only at a position where all candidates have the same mode. A
built-in candidate has no mode. So each position of a built-in candidate is
treated as a position of mixed modes (`fit_any_mode`): the fit is `No` when the
two types never convert in any mode, else `Maybe`. A `Candidate` of the pick
gets a flag for this; the symbol carries it ("modes not known").

The effect: rule 3 and rule 4 of the pick never choose a built-in candidate.
Rule 5 does: a call has a type when the candidates that are left after the `No`
fits agree on their return type.

One row of the fit table of `class-members.md` grows: two arrays never convert
when their elements never convert (today a pair of arrays is always `Maybe`).
So an array of `CHARACTER` never fits an array of a class. Examples:

| Call | Candidates | Type |
| --- | --- | --- |
| `oErr:GetMessage(1)` | `CHARACTER GetMessage(INTEGER)` | `CHARACTER` |
| `oJson:Write(lc, TRUE)` | 4 overloads with 2 parameters, all `LOGICAL` | `LOGICAL` |
| `OpenEdge.Core.String:Join(cArr, ",")`, `cArr AS CHARACTER EXTENT` | `LONGCHAR Join(CHARACTER[], CHARACTER)`, `String Join(Array, CHARACTER)`, `String Join(String[], CHARACTER)` | `LONGCHAR`: the array of `CHARACTER` never converts to a class or to an array of a class |
| `oStr:CompareTo(oStr)` | `INTEGER CompareTo(ILongcharHolder)`, `INTEGER CompareTo(String)` | `INTEGER` |

### Completeness of a surface with built-in levels

`class-members.md`, "When a surface is not complete", gets these causes for a
built-in level:

1. The level is not `members_trusted` (a damaged member can be an overload of
   any name): the method set of every name is open, as for a workspace level
   with `members_complete = false`.
2. The header of the level is not known, or a supertype is .NET or not in the
   catalog: the surface is opaque, as for an unread workspace ancestor.
3. The level is generic: the method set is open, and the members that name a
   type parameter are `Unknown`.

A property that oxabl finds keeps its type on an incomplete surface, as for a
workspace class. So `oObj:GetClass():TypeName` is `CHARACTER`, though
`Progress.Lang.Class` is not trusted.

A user class with a built-in ancestor is complete when its own levels are
complete and the built-in ancestor is complete. So
`CLASS app.OrderError INHERITS Progress.Lang.AppError` now has a complete
surface: its methods have their types.

## Assignability

`ClassLattice::reaches` climbs the built-in chain through the supertypes the
resolve pass recorded. Three rules are new:

- **`Progress.Lang.Object` is the top.** Every class, interface and enum value
  widens to `Progress.Lang.Object`, whether its chain names it or not. Without
  this rule, `oObj = NEW app.Thing()` becomes a false finding the moment
  `Progress.Lang.Object` is a symbol.
- **A built-in chain that is not known** (the header of a level is not known) is
  an incomplete chain: `NotKnown`, which is assignable, as for a workspace class
  with an unresolved supertype.
- **A built-in type never widens to a workspace type.** A built-in class cannot
  inherit a workspace class or implement a workspace interface. This holds
  already (`class-members.md`, "Assignability").

A generic target (`List<X>`) has the type `Unknown`, so no assignment to it is
judged.

## Checks

### What oxabl reports after this work

1. **More `LINT0004` findings**, from the new types. Examples:
   `l = oErr:GetMessage(1).` (`CHARACTER` into `LOGICAL`),
   `i = NEW Progress.Lang.AppError().` (a class into `INTEGER`),
   `oSys = oOrderError.` (`app.ordererror` into `Progress.Lang.SysError`), and
   the narrowing `c = oJson:GetLongchar("k").` (`LONGCHAR` into `CHARACTER`).
   The narrowing findings are the intent of the rule, as for workspace classes.
2. **Two new rules**, in the group `strict.calls`, so they are off unless a
   project turns the group on (decision 1 of the report):

| Code | Name | Reports |
| --- | --- | --- |
| `CL006` | `member-call-mismatch` | A call of a method, or a `NEW`, of a built-in class that no overload takes: no candidate has the number of arguments, or each candidate with that number has an argument whose type never converts to its parameter. |
| `CL007` | `unknown-member` | A member name that a built-in class does not have. |

Messages (the class is the documented name of the receiver's type):

- `CL006`, count: ``no overload of `Progress.Lang.AppError:GetMessage` takes 2 arguments``;
  for `NEW`: ``no constructor of `Progress.Lang.AppError` takes 3 arguments``.
  "1 argument" for one.
- `CL006`, types: ``no overload of `Progress.Lang.AppError:GetMessage` takes arguments of these types: `CHARACTER` ``,
  each argument type as `LINT0004` writes it, `?` for a type oxabl does not
  know, joined with `, `. The `NEW` form starts ``no constructor of `C` ``.
- `CL007`: ``built-in type `Progress.Lang.AppError` has no member `GetMesage` ``.

The span of `CL006` is the whole call or `NEW` expression. The span of `CL007`
is the member name.

### When the new rules stay silent

A finding needs every one of these facts. One missing fact gives no finding:

- The receiver's type is a built-in type (not a user class, also not one that
  inherits a built-in class), and the answer that named it was sure. For `NEW`,
  the named class is a built-in class.
- The built-in type is complete. For `CL006` on `NEW`, the catalog lists at
  least one constructor of the class.
- The receiver role reaches the name with no static mismatch on the anchor. A
  set with both static and instance candidates is judged by count over all
  candidates (the compiler takes no call whose count fits none of them).
- `CL007`: the name is in no namespace of the surface (method, property, event,
  enum member), with any role. A property used as a method is no `CL007`.
- No argument of the call is an include reference or a preprocessor reference
  that did not expand, and the call is not in an include analysed alone.
- `CL006` for types: every argument type that makes a `No` is known. `?`, a
  narrowing, a class pair at a position of unknown mode, a `TABLE`, `DATASET`
  or `BUFFER` argument and an `Unknown` type are `Maybe`, which never removes a
  candidate.
- `SUPER(...)` and `THIS-OBJECT(...)` in a constructor are not judged.

The resolve pass records the `CL007` facts (`SymbolTable::unknown_members`,
by node, with the class) where it records `PresentButUnusable` today. The
type-check pass records the `CL006` facts (`SymbolTable::call_misfits`, by
node: the class, the member, the count or the argument types) from the same
pick that types the call. The rules only read these records. `LINT0001` and its
exhaustive match on `UnresolvedReason` do not change.

### Effects on existing rules and features

| Consumer | Effect | Guard against a false positive |
| --- | --- | --- |
| `LINT0004` | Judges built-in member types, `NEW` of a built-in class, built-in variables and parameters, and the built-in lattice. | A damaged member is dropped; a type the normaliser cannot resolve is `Unknown`; a generic or .NET type is `Unknown`; the lattice fails closed on an unknown header; `Progress.Lang.Object` is the top. |
| `LINT0001` | No change. A built-in type name resolves or stays `External`; a member through a receiver that is not found records `PresentButUnusable`. An unqualified name in a class body that no level of the surface has stays softened, as today, when the class has a built-in ancestor: its built-in levels count as "not read" for that rule. | The reason match is exhaustive; no reason is added. The absence of a member is judged only by `CL007`, which is opt-in. |
| `LINT0002`, `LINT0006` | No change: an argument of a built-in call is a read, as before. | Built-in parameters have no mode, so no `OUTPUT` write is credited. |
| `CL003` | Judges a `RUN` argument that is a built-in member, where a handle meets a type it has no conversion to. | Only primitive types, as today. |
| Hover, definition, highlights, references, semantic tokens | New answers for built-in names (next section). | A name with no resolution still has no answer. |
| `oxabl analyze` | The `types` section has built-in types; the dependency section does not change. | None needed. |

## Hover and go to definition

A built-in type and its members have no source file. Each request answers:

| Request | On a built-in type name | On a built-in member |
| --- | --- | --- |
| Hover | The header and a line with the kind, the version and a link (below) | Each overload, the declaring type, the version, the link (below) |
| Go to definition | No location | No location |
| Document link | None | None |
| Document highlight | Each use of the type in the document | Each use of the member in the document, `Read` (a property that is assigned: `Write`) |
| Find references | No location (a name with no definition has no references, `references.md`) | No location |
| Semantic tokens | `class`, or `interface` | `method`, `property` or `event`; an enum member is `property` |

Hover of a type:

````text
```abl
CLASS Progress.Lang.AppError INHERITS Progress.Lang.ProError
```

Built-in class of OpenEdge 12.8. [Progress documentation](https://docs-be.progress.com/bundle/abl-reference/page/Progress.Lang.AppError-class.html)
````

- The header is `CLASS`, `INTERFACE` or `ENUM`, the documented name with its
  generic parameters (`Progress.Collections.List<T>`), `INHERITS` and
  `IMPLEMENTS` as the catalog gives them, and `FINAL` when the catalog says so.
- The line says "Built-in class", "Built-in interface" or "Built-in enum".
- The URL is `https://docs-be.progress.com/bundle/<bundle>/page/<page>`: the
  bundle `abl-reference` for `Progress.*`, `openedge-abl-api-reference-128` for
  the others. The codegen writes the full URL into the row.

Hover of a member:

````text
```abl
METHOD CHARACTER GetMessage (MessageIndex AS INTEGER)
```

Declared in built-in class `Progress.Lang.ProError` (OpenEdge 12.8). Parameter modes are not known. [Progress documentation](https://docs-be.progress.com/bundle/abl-reference/page/Progress.Lang.ProError-class.html)
````

- A method: `METHOD [STATIC] [ABSTRACT] <return> <Name> (<params>)`. No
  access word: the catalog has no access.
  The return type is `VOID`, a primitive, or a documented type name (with
  `EXTENT` for an array). Each parameter is `<name> AS <type>`, or `<type>`
  alone when the catalog has no name. No mode is written. Each candidate of
  the call's overload set is a code block, nearest level first and in catalog
  order inside a level, joined by a blank line, at most 10; then the line
  `N more overloads.` The candidates of each declaring type form one group
  that ends with its own "Declared in" line.
- A property: `DEFINE [STATIC] [ABSTRACT] PROPERTY <Name> AS <type>`. No `GET` or
  `SET`: the catalog does not give them.
- An event: `DEFINE [STATIC] EVENT <Name> SIGNATURE VOID (<params>)`.
- An enum member: the code block `<Enum>:<Member>`, then
  ``Member of built-in enum `<Enum>` (OpenEdge 12.8).`` and the link.
- The line after the code blocks is
  ``Declared in built-in class `<Type>` (OpenEdge 12.8).``, with `interface` or
  `enum` for a declaring type of that kind. `<Type>` is the type whose level
  declares the member (`Progress.Lang.ProError` for `GetMessage` through an
  `AppError`), not the receiver's type. The link is the page of that type.
  When the overloads were cut at 10, the line `N more overloads.` comes
  before it, as its own paragraph.
- The sentence "Parameter modes are not known." is present when a shown
  signature has a parameter.
- No "Database effects" section: the effects of a built-in method are not
  known, and the hover does not claim that there are none.

The hover reads the catalog through the surface API (below), so it lists the
same candidates as the pick.

## One interface for classes of every origin

Code completion after `obj:` (#436), signature help (#438) and the type
hierarchy (#440) need "the members of class X with their signatures and docs"
and "the supertypes of X", for user and built-in classes alike. This work adds
that interface in `oxabl_semantic` (`surface.rs`), and the resolve pass reads its
levels through it, so the two cannot disagree.

- `SymbolTable::class_origin(sym) -> Option<ClassOrigin>`: `Local(ScopeId)` (a
  class of the analysed file), `Workspace(IndexName)`, `Builtin(&'static
  BuiltinClass)`. Recorded for every class symbol that the resolve pass mints
  or declares.
- `surface(origin, index, catalog, role) -> ClassSurface`: the members the role
  reaches, nearest first, each a `SurfaceMember` with the name, the kind, the
  access, `is_static`, the type as written (`MemberType`), the parameters (with
  `mode: Option<ParamMode>`, `None` for a built-in one), the declaring class
  (`ClassOrigin`), and its origin (`File(IndexedFileId)` or
  `Builtin(&'static BuiltinMember)`, which gives the documentation URL).
  `ClassSurface::complete` says whether the list is whole, by the rules of
  "Completeness".
- `supertypes(origin, index, catalog) -> Supertypes`: the parent and the
  interfaces, each a `ClassOrigin` or an unresolved name, and whether the
  header is known.

The hover of a built-in member is the first consumer. The others come with
their issues.

## Incremental analysis in the daemon

The catalog is static data. It is no salsa input and no query, and it never
changes while the daemon runs. Nothing changes in `oxabl_daemon`: a user class
that inherits a built-in class has the same `ClassDescriptor` facts as before.

## Cost

- **Lexer and parser:** no change. A class name is `Kind::Identifier` already.
- **Resolve, single file with no class:** no new work. The catalog is asked only
  after the scope tree misses a type name, as today.
- **A built-in type used in a file:** one binary search for each candidate
  spelling (memoized per run in `type_lookups`, as today), one symbol for the
  type and one for each supertype (most chains have 2 to 4 levels).
- **A member through a built-in receiver:** the walk of the surface once per
  class per run (memoized, as for a workspace class), with no allocation for a
  built-in level beyond the entries of the inherited map; then one hash lookup
  per reference.
- **Every class surface** gets the level `Progress.Lang.Object`: 6 members, once
  per class per run.
- **Static data:** about 515 KB on `wasm32` (130 KB compressed), measured on a
  prototype. Build time of `oxabl_semantic` grows by about 1 s.

Benches (AGENTS.md, "Benchmarks"):

- The existing `builtin_classes` fixture gets more work (symbols, members). Its
  numbers go up; the pull request states them and why.
- A new fixture and bench names `builtin_members` in
  `crates/oxabl_semantic/benches/semantic_bench.rs`: a program with
  `CATCH e AS Progress.Lang.Error`, `JsonObject` calls with overloads, a static
  call, an enum member, and a user class of the file that inherits
  `Progress.Lang.AppError`. No index.
- `tiny`, `medium`, `skipped`, `schema_heavy`, `class_members` and
  `class_members_indexed` must not change. Measure them on the base and on the
  branch, with `valgrind --tool=callgrind` instruction counts when the machine
  is loaded, and state the numbers in the pull request.

## Out of scope, and the seams left

| Work | Issue | Seam |
| --- | --- | --- |
| .NET types (`System.*`, `Telerik.*`, `FROM ASSEMBLY`) | #465 | A .NET supertype or type is External in the catalog; `ClassOrigin` gets a variant `Assembly`, and an assembly catalog is a fourth level source of the walk. |
| Generic types (`List<T>` with type arguments) | none | `Unknown` for a type parameter and a generic instance; the table keeps the generic parameters, and the type enum gets a variant for a type parameter index when substitution comes. |
| Parameter modes and access of built-in members, and a check of the catalog | none | A dump through `Progress.Reflect` on a licensed OpenEdge (like `scripts/dump-metaschema.p`) can give modes, access and every overload. `BUILTIN_PARAMS` gets `mode`, and "modes not known" goes away for the types the dump covers. |
| More overrides (`Progress.Lang.Class`, `JsonArray`, `Progress.Reflect.*`, `Progress.IO.*`) | none | `resources/progress_builtin_overrides.toml`. |
| A catalog for each OpenEdge version | none | `AnalysisContext::builtins`. |
| Member checks on user classes (`CL006`, `CL007` for a workspace or local receiver) | none | One condition in each rule (the receiver origin); needs a dogfood pass first. |
| Completion, signature help, type hierarchy | #436, #438, #440 | `surface`, `supertypes`, `SurfaceMember`. |
| One-line descriptions in the hover | #473 | The row has the page URL. |

## Open facts

None. The three facts that this design first left open are compiler facts
now (OpenEdge 12.8.9, `COMPILE`), stated in "A user class with the name of a
built-in type" and "`Progress.Lang.Object` on every surface":

- A workspace file with the qualified name of a `Progress.*` type is ignored:
  the compiler binds the built-in type.
- A class in the unnamed package comes before the implicit
  `USING Progress.Lang.*`, and an explicit wildcard `USING` comes before the
  unnamed package.
- An interface reference has the members of `Progress.Lang.Object`.

## Measurement on real code

The private ABL code that the planner could read has 66 ABL files, with 10
references to built-in types in 6 files: 6 to the JSON object model, 2 to
`Progress.Lang.Error`, 2 to an `OpenEdge.Web` class. That sample is too small
to measure false positives, so this design relies on the catalog checks above
and on the opt-in group for the new rules.

## Implementation notes

What the implementation decided differently from the sections above:

- **Access.** The catalog has no access. The hover writes no access word
  (`METHOD CHARACTER GetMessage (...)`), and `SurfaceMember::access` is
  `None` for a built-in member. The roles reach a built-in member as a
  `PUBLIC` one, so an outside role can list a `PROTECTED` member
  (`Progress.Lang.Object:Clone`): no finding comes from it.
- **Hover text.** A type the catalog does not give is written
  `/* type not documented */`; an `ABSTRACT` property has `ABSTRACT`.
- **Data form.** A type row keeps the bundle (a number) and the page name,
  not the URL, and no lowercase key: the lookups fold the name as they
  compare. This saves about 88 KB of URL text and 49 KB of keys. Struct
  literals under `#[rustfmt::skip]` in place of the macros were measured and
  not kept: the same data size, no faster to compile, and twice the source.

- **The sure flag.** `TypeLookup::Builtin` carries `Option<SymbolId>` in place
  of `sure: bool`: the symbol is there when the answer is sure and the type is
  not generic. A `Progress.*` candidate is never sure-or-not by the index: it
  is the built-in type (the compiler facts). So no `shadows_builtin` set is
  needed; a workspace class that wins is typed like any workspace class.
- **The catalog of a type row.** A row keeps `inherits` and `implements` as
  documented (after the overrides) for the hover, and the resolved rows of
  its supertypes for the walk. Each row is one line of a brace-delimited
  macro call (`classes!`, `members!`, `params!`, `enum_names!` in
  `builtin.rs`), which rustfmt leaves as it is.
- **Overrides file.** Each entry has a `checked_against` page and one of
  `inherits`, `drop_member`, `add_member`, `replace_member`. The three changes
  of the design are five entries (a drop and an add for each error class).
  A `inherits` entry on a type that documents a supertype is stale.
- **A property with no type** in the catalog keeps the property, with the type
  `Unknown`; it is not damaged. The counts are the planner's: 1248 types,
  1200 `members_trusted`, 1065 complete, 36 of 108 `Progress.*` complete.
- **`CATCH e AS Cls`.** The error variable gets the type of its class (a
  workspace or built-in class), and its type name is a navigation site.
- **Workspace enums.** A workspace `ENUM` gets `Progress.Lang.Enum` as a
  supertype, both in the surface walk and in the lattice. The index does not
  say whether it is a `FLAGS` enum, so its chain is incomplete past that level:
  a widening to `Progress.Lang.FlagsEnum` is not known, never a finding.
- **`LINT0001` softening.** A class with a built-in ancestor other than
  `Progress.Lang.Object` softens a bare name that no level has
  (`CrossFileState::builtin_ancestors`): the catalog does not say which
  members are protected.
- **`CL006` facts** are read by the rule through
  `oxabl_semantic::builtin_call_misfit`, from the same overload sets and
  `overload::no_fit` that the pick uses, not stored as a side list. `CL007`
  facts are a side list (`SymbolTable::unknown_members`), recorded where the
  member lookup misses.
- **The surface API.** `surface(origin, index, catalog, role)` answers for a
  workspace class and a built-in type. A class of the analysed file needs its
  `Semantic`: `local_surface(sem, origin, index, role)`. A member of a
  workspace class has the folded name that the index keeps. One
  `SurfaceMember` is one overload. `BuiltinMemberRef::doc()` gives the
  documentation (`MemberDoc { title, url, summary }`, `summary` is `None`
  until #473). `BuiltinCatalog::classes_in(package)` lists a package.
  The role type is `SurfaceRole` (`Reach` is the lattice's answer type).
- **Hover of an override.** `obj:ToString()` on a workspace class that
  overrides it shows that class's declaration, as for any workspace method.

## Tests

The black-box tests are `crates/oxabl/tests/builtin_classes_cli.rs` (through
`oxabl check`) and `crates/oxabl/tests/builtin_classes_lsp.rs` (through
`oxabl lsp`). They cover every acceptance criterion of #154 and every
fail-closed case in this document. The parity fixture table
(`crates/oxabl_pipeline/src/fixtures.rs`) gets rows for built-in member types,
so the CLI, the language server, the pipeline and the WASM client agree.
