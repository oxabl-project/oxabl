# Types of class members: implementation steps

The design is `docs/design/class-members.md`. Do the steps in order. Each step
compiles, keeps every test green, and ends with the checks it names. Commit
each step on its own (conventional commits, no ticket key: the repository is
public).

Checks used below:

- `cargo test -p <crate>` for each crate the step touches.
- `cargo clippy -p <crate> --all-targets -- -D warnings` for the same crates
  (see `.github/workflows/ci.yml` for the exact CI flags).
- `rustfmt --edition 2024 <changed files>`; never a writing `cargo fmt` over the
  workspace.

The black-box tests are written in parallel by another agent, from a test
specification of behaviour only, into
`crates/oxabl/tests/class_members_cli.rs` and
`crates/oxabl/tests/class_members_lsp.rs`. They fail until step 8. Run them at
the end of steps 6, 7 and 8 to see progress; do not change them. If one of
them looks wrong, say so in the report with the case name; do not edit it.

## Step 1. Keep `EXTENT` on properties and method return types

The parser drops `EXTENT` after a property type and after a method return
type, so an `EXTENT` property has a scalar type today, locally and in the
index.

- `crates/oxabl_ast/src/statement.rs`: add `extent: Option<ParameterExtent>` to
  `StatementKind::Property` and `StatementKind::Method` (the same type a
  parameter uses: `Indeterminate`, `Fixed(n)`, `Unresolved(span)`). Keep the
  size assertions of `StatementKind` and `Statement` true.
- `crates/oxabl_parser/src/parser/statements.rs`: `parse_define_property` and
  `parse_method` keep the clause (use `parse_extent_clause` if it fits), in
  place of the code that consumes and drops it.
- Every constructor and exhaustive pattern of the two variants (search
  `StatementKind::Property {` and `StatementKind::Method {` in `crates/` and
  `fuzz/`). Most patterns use `..` and need no change.
- `crates/oxabl_semantic/src/resolve.rs` (declare pass): a property and a
  method with `EXTENT` get `ResolvedType::Array { element, extent }`, as a
  variable does (`Fixed(0)` and `Indeterminate` are `extent: None`;
  `Unresolved` gives `Unknown`).
- `crates/oxabl_index/src/lib.rs` (`collect_members`): the same for
  `MemberType` (a primitive array is `MemberType::Portable(Array)`; a
  class-typed array is `MemberType::Portable(Array { element: Unknown })`).
- Check that the formatter, the outline and the hover do not depend on the old
  shape (they read `EXTENT` from tokens).

Verify: `cargo test -p oxabl_ast -p oxabl_parser -p oxabl_semantic -p oxabl_index -p oxabl_formatter -p oxabl_pipeline`;
clippy on the same. Add a parser test for each variant (`EXTENT`, `EXTENT 3`)
and an index test that a property `AS CHARACTER EXTENT 3` gives an array
member type. Break the parser change and see the tests fail.

## Step 2. The class lattice fails closed on an incomplete chain

Today `oTop = NEW app.Bottom().` is reported (LINT0004) when `app.Bottom`
inherits `app.Middle` and no file supplies `app.Middle`. This step removes that
false positive before member types make it more frequent.

- `crates/oxabl_semantic/src/symbol.rs`: a side-map set of class symbols whose
  header names a workspace supertype that did not resolve
  (`record_incomplete_chain(sym)`, `has_incomplete_chain(sym)`), in
  `SymbolSideMaps`.
- `crates/oxabl_semantic/src/resolve.rs`: record it in
  `resolve_supertype_edges` (a local header) and where a synthesized class
  records its supertypes (`synth_class` area, the loop over
  `descriptor.inherits` and `descriptor.implements`), for
  `TypeLookup::NotFound`, `Unusable` and `Unknowable`. Not for `Builtin` and
  `External`.
- `crates/oxabl_semantic/src/coercion.rs`: `ClassLattice` gets a three-way
  answer (`Widens`, `DoesNot`, `NotKnown`): `NotKnown` when the climb meets a
  class with an incomplete chain and has not found the target. `widens_to`
  keeps its meaning for callers that need a yes (`Widens` only). `assignable`
  treats `NotKnown` as assignable. `assignable_strict` does not consult the
  lattice and is unchanged.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint`; clippy. Add to
`crates/oxabl_lint/tests/lint0004_inheritance_assignability.rs` the case above
with a `BatchIndex` over an in-memory workspace: silent with `app.Middle`
absent, still reported when `app.Middle` exists and does not inherit
`app.Top`. Break the lattice change and see the first case fail.

## Step 3. Parameter lists and completeness in the index

- `crates/oxabl_semantic/src/signature.rs`: the portable parameter type
  `MemberParam { mode: ParamMode, shape: MemberParamShape }` with
  `MemberParamShape::{Value(MemberType), Buffer, Table, TableHandle, Dataset,
  DatasetHandle}`, and one function that builds `Option<Vec<MemberParam>>` from
  the `DefineParameter` statements of a method (`None` when a statement is not
  a `DefineParameter`, such as an `&IF` or an include reference). `LIKE` gives
  `Value(Portable(Unknown))`. A class type gives `Value(Named(name))`. Export
  both from `lib.rs`.
- `crates/oxabl_semantic/src/index.rs`:
  - `MemberDescriptor::params: Option<Arc<[MemberParam]>>`: `Some` for a method
    with a known list, `None` otherwise.
  - `ClassDescriptor::members_complete: bool`.
  - Update `NullIndex`, the docs, and every test stub (`lib.rs` `StubIndex`,
    `resolve.rs` test indexes, `oxabl_lint` and `oxabl_pipeline` test helpers).
- `crates/oxabl_index/src/lib.rs`:
  - `collect_members` fills `params` for `StatementKind::Method`.
  - `members_complete = false` when the class body holds a `PreprocIf` that
    holds a member declaration, or an include reference, in the facts that are
    used (the text as written after an expansion that did not settle). Use the
    existing `built_from_includes` and `expand` results in
    `index_file_with_includes` and `index_parsed`.
  - `declared_classes` carries `imports` also when a member type or a
    parameter type is `MemberType::Named`.
  - `declared_enums`: each enum member gets `ty: MemberType::Named(<enum name
    as written>)`.
- `crates/oxabl_daemon`: no code change expected. Run its tests: the facts are
  compared by value, and the new fields take part.

Verify: `cargo test -p oxabl_semantic -p oxabl_index -p oxabl_daemon -p oxabl_lsp`;
clippy. Add index tests: a method's parameter list; an `&IF` in a body; a body
with an include that the index cannot read (`members_complete == false`); with
the include on the path (`true`, and the member is in the list); imports kept
for a class with a class-typed property and no supertype; an enum member's
type.

## Step 4. Local methods: parameter lists and class-typed return types

- `crates/oxabl_semantic/src/symbol.rs`: side maps
  - `method_param_decls: FxHashMap<SymbolId, Arc<[MemberParam]>>` (declare
    pass, portable);
  - `method_params: FxHashMap<SymbolId, Arc<[CallParam]>>` (resolve pass, with
    `CallParam { mode, kind: CallParamKind::{Value(ResolvedType), Buffer,
    Table, TableHandle, Dataset, DatasetHandle} }`), with public read
    accessors. Absent means "list not known".
- `crates/oxabl_semantic/src/resolve.rs`:
  - `declare_method` records the portable list for each declaration (anchor and
    overloads).
  - `upgrade_class_types` / `collect_class_upgrades`: upgrade the class-typed
    return type of every method declaration (each symbol of an overload set,
    found by its declaration node, not by its name), and convert each local
    portable list to `CallParam` (class names through `resolve_type_name` in
    the method's scope).
- No consumer reads the new maps yet.

Verify: `cargo test -p oxabl_semantic`; clippy; `cargo bench -p oxabl_semantic --bench semantic_bench -- declare`
against the base: the declare pass must not regress on the existing fixtures
(they declare no method). Unit tests in `resolve.rs` for both maps, including
a class-typed return type on the second overload.

## Step 5. The inherited surface keeps each candidate

- `crates/oxabl_semantic/src/resolve.rs`:
  - `InheritedMember` keeps, for a method, a list of candidates (return type as
    `MemberType`, params, file, the declaring class's imports), deduplicated by
    parameter list, nearest first. A property keeps the nearest declaration, as
    today.
  - `lookup_inherited` mints one symbol per candidate, links them with
    `record_method_overload(anchor, ...)`, records `method_params` for each
    (class names through `search_in_imports` with the declaring imports), and
    records `member_file` for each when sites are recorded.
  - A `MemberType::Named` return or property type resolves through the
    declaring imports at mint time to `Class(sym)`; any miss gives `Unknown`.
    Record the lookup with `record_class_lookup` (no class site). A bare name
    under `has_unseen_imports` gives `Unknown`.
  - Mark the surface: a side set in `SymbolTable` of overload anchors whose
    candidate set may be incomplete (opaque surface, a level with
    `members_complete == false`, a cycle seen by the walk, a name of a
    `Progress.Lang.Object` method). A cycle also marks the class, so every
    member through it is typed `Unknown`.
- The data type of the anchor stays the agreement type, so the type-check pass
  (unchanged in this step) gives the same answers as before.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint` (the cross-file tests in
`crates/oxabl_lint/tests/cross_file_*.rs` must pass unchanged); clippy.
`twenty_references_to_one_inherited_member_synthesize_it_once` must still hold
per candidate.

## Step 6. Receivers

- `crates/oxabl_semantic/src/resolve.rs`: replace `receiver_indexed_class` with
  one function that returns the receiver class and role for each row of the
  receiver table in the design (declared class type, `NEW`, `CAST`, a member
  with a class type, a call whose arity candidates agree on a class,
  `THIS-OBJECT`, `SUPER`, a type name, an enum member). `NEW` and `CAST` are
  told apart from a type name by the expression kind, not by the symbol kind.
- `resolve_member_reference` handles a class of the analysed file: own
  members from its body scope (`ScopeKind::Class` with `owner_node` equal to
  the class symbol's declaration), then `lookup_inherited` with the
  `Subclass` role. `SUPER` uses only `lookup_inherited`. The access rules of
  the design apply; a member the role cannot reach records
  `PresentButUnusable`.
- For a class of the analysed file, the candidates of a method name are its
  own overload set **and** the inherited candidates of that name (step 5).
  Keep the inherited candidate symbols in a separate side list keyed by the
  local anchor (for example `SymbolTable::inherited_candidates(anchor)`), not
  in `method_overloads`: the hover
  (`crates/oxabl_pipeline/src/navigate/hover.rs`) shows the length of
  `method_overloads` as the number of local overloads. The pick (step 7) reads
  both lists. The unqualified call (`Score(1)`, a `FunctionCall`) and
  `THIS-OBJECT:Score(1)` then read one set.
  Today the own overloads hide the inherited ones (see the design, "What
  exists before this work").
- Remove the `index_loaded` early return for local receivers: a class of the
  analysed file and `THIS-OBJECT` work with no index. Keep it for workspace
  classes.
- Record overload sets of local classes on an incomplete surface (an elided
  include inside the class body, an include fragment, an opaque ancestor) in
  the same side set as step 5.
- Update the doc comment of `resolve_member_reference` (it says the type
  checker types members as `Unknown`).

Verify: `cargo test -p oxabl_semantic -p oxabl_lint -p oxabl_pipeline`;
clippy. Unit tests for each receiver row and for the access table. Also run
`cargo test -p oxabl --test definition_lsp --test hover_lsp --test document_highlight_lsp --test semantic_tokens_lsp --test navigation_sweep_lsp`:
navigation now answers on more names. A changed expectation there is only
acceptable when the old answer was "no answer" for a name this step resolves;
list each one in the report.

## Step 7. The type-check pass and the pick

- New module `crates/oxabl_semantic/src/overload.rs`: the fit table and the
  pick rules 1 to 6 of the design, as one function over the candidates of an
  anchor (`SymbolTable::method_overloads`, or the symbol alone), the argument
  types, the lattice and the completeness mark. No allocation for up to eight
  candidates. The resolve pass (step 6, chain links) calls it with `Unknown`
  arguments; switch that call to this function in this step.
- `crates/oxabl_semantic/src/check.rs`:
  - `MemberAccess`: the type of the member symbol (`type_from_reference`), or
    `Unknown` under the static rule, a cyclic class, or no resolution.
  - `MethodCall`: the pick, with the argument types it computed.
  - `FunctionCall` that resolved to a method with a `method_params` entry: the
    pick. Other function calls keep today's code.
  - `CAST(x, Cls)` with a resolved type operand: `Class(Cls)`.
  - `THIS-OBJECT` inside a class body: `Class(<that class>)`.
- Tests that pin the old boundary, and change here with their reason:
  - `check.rs` `check_method_call_type_is_unknown_v1`: a method call on an
    unresolved receiver stays `Unknown`; rename and keep that assertion.
  - `crates/oxabl_lint/tests/cross_file_inheritance.rs`
    `a_colon_qualified_call_on_this_object_stays_unjudged`: becomes a test that
    `THIS-OBJECT:calc-total()` into a `LOGICAL` is reported.
  - `cross_file_inheritance.rs` `inherited_overloads_do_not_lend_calls_the_first_return_type`
    and `lint0004_inheritance_assignability.rs`
    `an_unresolved_overload_set_does_not_lend_a_call_an_arbitrary_return_type`:
    the call `choose(1)` / `label(1)` now picks the `INTEGER` overload and is
    reported. Keep each test's intent with an argument that picks nothing
    (`?`), which stays silent, and add the case that picks.
  - `cross_file_using.rs` (not `cross_file_inheritance.rs`)
    `attaching_an_index_adds_exactly_the_enumerated_diagnostics`: update only
    the rows whose fixture has a member access or a method call that the
    design now types. The same file has two more tests that pin the old
    boundary and change here: `a_cross_file_class_is_judged_in_both_assignment_directions`
    (its `:`-qualified call) and
    `a_mismatch_through_a_colon_qualified_member_stays_silent_at_the_call_site`.
  - The module docs that state the old boundary: the head of `check.rs`, the
    head of `lint0004_inheritance_assignability.rs`, the comment in
    `type_mismatch_assignment.rs` `check_pair`.
  No other existing expectation may change. If one does, stop and report it.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint -p oxabl_pipeline -p oxabl_analyze`;
clippy. Unit tests in `overload.rs` for each row of the fit table and each
pick rule, and each row of the example table in the design. Break rule 3 (pick
the first Exact even with a Maybe present) and see a test fail.

## Step 8. The fail-closed guards, end to end

Most guards exist after steps 5 to 7. This step checks each one against the
black-box tests and closes what is left:

- static and instance mismatch gives `Unknown`;
- an opaque surface gives `Unknown` for methods and keeps property types;
- `Progress.Lang.Object` method names give `Unknown`;
- a cycle gives `Unknown` for every member;
- an unread include or an `&IF` in a class body gives `Unknown` for methods;
- an include fragment analysed alone gives `Unknown` for methods of its class.

Verify: `cargo build -p oxabl` and
`cargo test -p oxabl --test class_members_cli --test class_members_lsp`; all
cases pass. Then the regression set:
`cargo test -p oxabl --test lint0004_cli --test check_cli --test hover_lsp --test definition_lsp --test semantic_tokens_lsp --test navigation_sweep_lsp`,
`cargo test -p oxabl_lsp`, `cargo test -p oxabl_daemon`.

## Step 9. Benches

- `crates/oxabl_semantic/benches/semantic_bench.rs`: add the fixtures
  `class_members` (no index) and `class_members_indexed` (a stub
  `WorkspaceIndex` in the bench file that answers two classes from fixed
  descriptors), each with its own bench names, through `bench_fixture` and a
  variant of it that attaches the index.
- Measure the existing benches on the base commit and on the branch with the
  same input (`cargo bench -p oxabl_semantic --bench semantic_bench`), or with
  `valgrind --tool=callgrind` instruction counts when the machine is loaded.
  State the before and after numbers of `tiny`, `medium`, `skipped`,
  `builtin_classes` and `schema_heavy` in the pull request.

Verify: `cargo test -p oxabl_semantic --benches` runs every bench once;
`cargo clippy -p oxabl_semantic --all-targets -- -D warnings`.

## Step 10. Documents

- `docs/design/class-members.md`: change what the implementation decided
  differently, if anything.
- `docs/design/navigation.md`: the receivers that now resolve (chains,
  `THIS-OBJECT`, `SUPER`, `CAST`, static, enum, a class of the analysed file).
- `docs/overview.md`, LINT0004 line: members are judged.
- `AGENTS.md`: add `oxabl_semantic` to the workspace list if it is still
  absent, with one line on the member lookup and a pointer to the design.
- `crates/oxabl/skill/references/rules.md` needs no change unless a rule's
  one-line description changes (`skill_rules` checks it).

Verify: `cargo test -p oxabl --test skill_drift --test skill_rules`.

## Step 11. Final checks

- `cargo fmt --all -- --check`, `cargo clippy --workspace --all-targets -- -D warnings`,
  `cargo clippy -p oxabl --all-targets --features remote-cache,self-update --locked -- -D warnings`,
  `cargo test --workspace` (with `CARGO_BUILD_JOBS` as set).
- `cargo fmt --manifest-path fuzz/Cargo.toml -- --check` and
  `cargo clippy --manifest-path fuzz/Cargo.toml --bins -- -D warnings`, as the
  CI job `Fuzz targets` runs them, if step 1 touched the fuzz crate.
- `git log --format='%ae %ce' origin/master..HEAD` shows only the public
  address.
