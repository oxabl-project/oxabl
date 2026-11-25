# Built-in classes: implementation steps

The design is `docs/design/builtin-classes.md`. Read it and
`docs/design/class-members.md` first. Do the steps in order. Each step
compiles, keeps every test green, and ends with the checks it names. Commit
each step on its own (conventional commits, no ticket key: the repository is
public).

Checks used below:

- `cargo test -p <crate>` for each crate the step touches.
- `cargo clippy -p <crate> --all-targets -- -D warnings` for the same crates
  (see `.github/workflows/ci.yml` for the exact CI flags).
- `rustfmt --edition 2024 <changed files>`; never a writing `cargo fmt` over the
  workspace. The generated `builtin_classes.rs` is written in rustfmt layout by
  the codegen (`str_slice` shows how); `cargo fmt --all -- --check` must pass
  on it.

The black-box tests are written in parallel by another agent, from a test
specification of behaviour only, into
`crates/oxabl/tests/builtin_classes_cli.rs` and
`crates/oxabl/tests/builtin_classes_lsp.rs`. They fail until step 8. Run them
at the end of steps 4 to 8 to see progress; do not change them. If one of them
looks wrong, say so in the report with the case name; do not edit it.

**Tests that pin the old boundary** change in the step that changes the
behaviour, each with its reason in the commit message. No other existing
expectation may change. If one does, stop and report it.

## Step 1. Normaliser, overrides and completeness in the codegen

No generated output changes in this step.

- `crates/oxabl_codegen/src/builtin_normalise.rs` (new): reads the
  `BuiltinIndex` of `progress_docs.rs` and gives a normalised model: each type
  with its supertypes resolved to type names of the catalog (or marked
  external, missing or unresolved), and each member with its kind, flags,
  return type and parameters as a typed value (`Void`, primitive, array,
  catalog type, array of a catalog type, shape, `Unknown`). Apply every rule of
  the table "Quirks and errors" of the design, in this order: `CLASS` prefix,
  `EXTENT` forms to `[]`, kind word, self-enum names, `parameter-value`,
  handle placeholders, `STATIC` constructors, missing return types (API bundle:
  `VOID`; `Progress.*`: damaged), `&ltX> Name` names, member names that are not
  ABL names (damaged), unqualified names (same package, then a unique simple
  name, else damaged), generic instances and type parameters (`Unknown`),
  .NET names (external), duplicate signatures (keep the first).
  An ABL name is `[A-Za-z_][A-Za-z0-9_#$%&-]*`. A primitive is one of
  `CHARACTER LONGCHAR INTEGER INT64 DECIMAL LOGICAL DATE DATETIME DATETIME-TZ
  HANDLE WIDGET-HANDLE COM-HANDLE ROWID RECID RAW MEMPTR BLOB CLOB`, any case.
- `resources/progress_builtin_overrides.toml` (new) with exactly the three
  entries of the design ("Overrides"). The codegen parses it with `toml`
  (already a dependency of `oxabl_codegen`) and applies it after the
  normaliser. An entry that matches no type or no
  member is an error.
- The flags `members_trusted`, `header_known` and `complete`, by the rules of
  the design ("Trust and completeness"). A cycle in the catalog is an error.
- Tests in the new module:
  - the counts on the checked-in JSON: 1248 types, 1200 `members_trusted`,
    1065 `complete`, 36 of the 108 `Progress.*` types `complete`;
  - the state of each named type of the design (complete:
    `Progress.Lang.Object`, `Progress.Lang.Enum`, `Progress.Lang.FlagsEnum`,
    `Progress.Lang.Error`, `Progress.Lang.ProError`, `Progress.Lang.AppError`,
    `Progress.Lang.SysError`, `Progress.Json.ObjectModel.JsonObject`,
    `Progress.Json.ObjectModel.JsonConstruct`,
    `Progress.Json.ObjectModel.JsonDataType`, `OpenEdge.Core.String`,
    `Progress.ApplicationServer.AdapterTypes`; not complete:
    `Progress.Lang.Class`, `Progress.Json.ObjectModel.JsonArray`,
    `Progress.Json.ObjectModel.ObjectModelParser`, `Progress.Collections.List`,
    `Progress.Windows.Form`, `Progress.IO.ByteOrder`,
    `OpenEdge.Net.UriSchemeEnum`);
  - one small synthetic index for each normaliser rule;
  - a stale override fails.

  The counts come from a simulation of these rules that the planner ran
  (`builtin-classes.md`). If your rules give other counts, find the rule that
  differs before you change a number, and say which in the report.

Verify: `cargo test -p oxabl_codegen`; clippy. Break one rule (for example,
drop the `CLASS` prefix rule) and see the count test and the
`JsonObject` state fail.

## Step 2. The static tables and the catalog API

- `crates/oxabl_codegen/src/builtin_class_table.rs`: emit, from the model of
  step 1, the flat tables of the design ("The data form"): `BUILTIN_CLASSES`
  with the new fields (documented name, flags, URL, member, constructor and
  enum-member ranges, supertype rows), `BUILTIN_MEMBERS` (sorted by folded
  name inside each type's range), `BUILTIN_PARAMS`, and the type enum. Names
  stay `&'static str`. Keep the existing `builtin_class` and
  `builtin_class_in` functions and their behaviour.
- Regenerate: `cargo run -p oxabl_codegen -- builtin-class-table`. The
  existing test `generated_table_matches_checked_in_catalog` covers drift.
- `crates/oxabl_semantic/src/builtin.rs` (new, hand-written): the API over the
  generated tables: `BuiltinClass::{members_named, constructors, enum_members,
  supertypes, is_complete, members_trusted, header_known, docs_url,
  documented_name}`, `BuiltinMember::{name, kind, is_static, ty, params}`,
  `BuiltinCatalog` (the version and the tables), and `BUILTIN_CATALOG`.
  Export from `lib.rs`. Add `AnalysisContext::builtins: &'static
  BuiltinCatalog` with the 12.8 catalog as its default; no reader yet.
- Unit tests in `builtin.rs`: a case-insensitive member lookup
  (`getmessage` on `Progress.Lang.ProError`), the 17 `Add` overloads of
  `JsonObject`, a static member, the enum members of
  `Progress.ApplicationServer.AdapterTypes`, a supertype row, the URL of a
  `Progress.*` and of an `OpenEdge.*` type.
- Measure and record in the report and later the pull request:
  - the size of `target/wasm32-unknown-unknown/release/oxabl_wasm.wasm`, raw
    and `gzip -9`, before (the base commit) and after, with
    `cargo build -p oxabl_wasm --target wasm32-unknown-unknown --release`. If
    the `wasm32-unknown-unknown` target is not installed, do not install it:
    say so in the report and leave the number to the pull request author;
  - the time of `cargo build -p oxabl_semantic` and
    `cargo build -p oxabl_semantic --release` after `cargo clean -p
    oxabl_semantic`, before and after.

Verify: `cargo test -p oxabl_codegen -p oxabl_semantic`; clippy on both;
`cargo fmt --all -- --check`.

## Step 3. Built-in type symbols, sure answers and the lattice

- `crates/oxabl_semantic/src/resolve.rs`:
  - `TypeLookup::Builtin` gets `sure: bool`. `search_in_imports` sets it to
    `false` when an earlier candidate was not looked up (no index, or an index
    with no search path, and the candidate is not a catalog name), or when the
    name is bare and `has_unseen_imports`. Every `match` on `TypeLookup` keeps
    its arms explicit.
  - `builtin_class_symbol(&'static BuiltinClass) -> SymbolId`: a memo in
    `CrossFileState`; kind `Class` or `Interface`; supertypes recorded with
    `record_resolved_supertypes` (minting them); `record_incomplete_chain` when
    the header is not known.
  - A sure built-in answer gives `Resolved(sym)` and `Class(sym)` in each place
    that handles `TypeLookup::Builtin` today: `walk_expression` (`NEW`),
    `resolve_using`, `try_dotted_receiver_type`, `note_type_site`,
    `resolve_type_operand`, `upgrade_class_decls`, `resolve_local_method_params`,
    `class_type_in_imports`, `resolve_supertype_edges` (a local header that
    names a built-in parent records it), and the supertype loop of
    `indexed_class_symbol`. A dotted receiver that names a built-in type is a
    `TypeName` receiver of that symbol, not a softened path. An answer that is
    not sure keeps today's `External`.
  - The two collisions of the design ("A user class with the name of a
    built-in type"). When oxabl finds the built-in answer first (a bare name
    through `USING Progress.Lang.*` while the unnamed package of the workspace
    also has it), the answer is not sure. When oxabl finds the workspace class
    first (a workspace file with the qualified name of a `Progress.*` type, or
    a bare name in the unnamed package that `Progress.Lang` also has), keep
    the workspace resolution and record that class symbol in a side set of
    `SymbolTable` (`shadows_builtin`): a declared type, a `NEW` type and a
    receiver of a class in that set are `Unknown`, and the surface walk does
    not start from it.
- `crates/oxabl_semantic/src/symbol.rs`: side maps `builtin_class(sym)` and
  `class_origin(sym)` (for every class symbol: local, workspace, built-in), in
  `SymbolSideMaps`. `Symbol` stays 64 bytes; `ResolveWalker` stays within its
  size assertion.
- `crates/oxabl_semantic/src/coercion.rs`: `reaches(from, to)` answers
  `Widens` when `to` is the built-in symbol of `Progress.Lang.Object` and
  `from` is any class symbol.
- `crates/oxabl_semantic/src/types.rs`: `display_abl` writes a built-in class
  with its documented name (`Progress.Lang.AppError`). Workspace classes keep
  today's text.
- Tests that pin the old boundary (change each with its reason): the
  assertions on the reason of a built-in type name in
  `crates/oxabl_lint/tests/lint0001_class_names.rs`
  (`unresolved_reasons(&sem, "jsonobject")`) and
  `crates/oxabl_lint/tests/lint0004_cross_file_reasons.rs`, where a sure
  built-in name is now resolved. Search the tests for `Progress.`,
  `OpenEdge.` and `AppError` and list each changed expectation in the report.
- Unit tests in `resolve.rs`: sure and not sure (each example of the design),
  the two collisions, a local class named `AppError`, the chain of
  `Progress.Lang.AppError` (`AppError`, `ProError`, `Error`, `Object`), an
  unknown header (`Progress.IO.ByteOrder`) records an incomplete chain.
  Unit tests in `coercion.rs`: a local class widens to `Progress.Lang.Object`;
  `AppError` widens to `Progress.Lang.Error`; `SysError` does not widen to
  `AppError`.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint -p oxabl_pipeline`;
clippy. Break the `Progress.Lang.Object` rule in `reaches` and see the unit
test fail.

## Step 4. Built-in levels of a surface

- `crates/oxabl_semantic/src/resolve.rs`:
  - The candidate of the inherited map gets an origin: the present fields for
    a workspace level, or `Builtin(&'static BuiltinMember)`. No allocation for
    a built-in candidate.
  - `walk_member_surface` seeds a built-in receiver with its built-in level,
    reads a built-in level from the catalog (members by kind into the
    namespaces of the design; enum members as static properties of the enum),
    queues its supertype rows directly, and needs no index for it. A workspace
    level still needs the index. After all levels, the walk adds the level
    `Progress.Lang.Object` once, for classes and interfaces.
  - The marks of the design ("Completeness of a surface with built-in
    levels"): a level that is not `members_trusted` goes into
    `partial_surfaces`; an unknown header, a .NET supertype or a supertype
    outside the catalog goes into `opaque_surfaces` and `unread_supertypes`; a
    generic level goes into `partial_surfaces`.
  - The gates `if !self.ctx.index_loaded` in `lookup_inherited`,
    `gather_inherited` and `class_on_cycle` let a surface with built-in levels
    through. A local class with a workspace parent and no index keeps today's
    answer.
  - `lookup_inherited` mints a built-in candidate with its data type from the
    catalog (`Class(builtin_class_symbol(..))` for a catalog type), its
    `method_params` (parameter shapes to `CallParamKind`), the "modes not
    known" mark, and `builtin_member(sym)` instead of `member_file`.
  - `method_set_open`: remove the name rule for `clone`, `equals`, `getclass`,
    `tostring`.
  - `LINT0001` softening: an unqualified name in a class body that no level
    has stays softened when the class has a built-in ancestor (design,
    "Effects on existing rules"). Find the rule that softens the misses of a
    class with an unread ancestor (`may_be_member_call` and the miss path of
    `lookup_inherited_ident`) and keep it true for a built-in ancestor.
  - A user `ENUM` gets the supertype `Progress.Lang.Enum` (`FLAGS`:
    `Progress.Lang.FlagsEnum`) in the surface walk.
- `crates/oxabl_semantic/src/symbol.rs`: side maps `builtin_member(sym)` and
  the set "modes not known", with public read accessors.
- Tests that pin the old boundary (change each with its reason):
  - `crates/oxabl/tests/class_members_cli.rs`
    `an_unknown_class_member_access_or_argument_gives_no_finding`: the lines
    `l = oNamed:ToString().` and `l = oP:ToString().` now give
    `` type mismatch: cannot assign `CHARACTER` to `LOGICAL` ``. Move them to
    a new test that wants those two findings; keep the rest.
  - `class_members_cli.rs`
    `a_method_of_a_class_with_an_unread_ancestor_is_unknown_but_a_property_is_not`:
    `app.AppErr INHERITS Progress.Lang.AppError` now has a complete surface,
    so `l = oErr:Describe().` gives a finding. Keep the intent of the test with
    an ancestor that oxabl cannot read (a workspace class no file supplies),
    and add the built-in case to a new test.
  - `class_members_lsp.rs`, if a case there uses `ToString` or the `AppErr`
    class with the old answer.
  - The unit tests of `resolve.rs` and the lint tests that state that a
    `Progress.Lang.Object` method or a method of a class with a built-in
    ancestor is `Unknown`.
- Unit tests: a built-in receiver with no index; a local class that inherits
  `Progress.Lang.AppError` with no index; `Progress.Lang.Class` (not trusted)
  gives `Unknown` to a method and a type to `TypeName`; a generic level; an
  enum member of a built-in enum; an `OpenEdge.*` enum member that the catalog
  does not list; `app.Shade:Dark:GetValue()`.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint -p oxabl_pipeline`;
`cargo build -p oxabl` and
`cargo test -p oxabl --test class_members_cli --test class_members_lsp`;
clippy. Run `builtin_classes_cli` and `builtin_classes_lsp` and note which
cases pass.

## Step 5. The pick with modes that are not known

- `crates/oxabl_semantic/src/overload.rs`: `Candidate` gets `modes_known:
  bool`. `call_type` sets it from the "modes not known" mark. `pick` uses
  `fit_any_mode` at every position of a candidate whose modes are not known.
  The resolve pass's chain-link call keeps calling the same function.
  `never_converts` answers for two arrays what it answers for their elements
  (design, "Overloads with modes that are not known").
- Unit tests: each row of the table "Overloads with modes that are not known"
  of the design, with candidates built as the resolve pass builds them; a
  pair of arrays whose elements never convert; and two built-in candidates
  `INTEGER F(LONGCHAR)` and `DATE F(CHARACTER)` with a `CHARACTER` argument,
  which give `Unknown` (with known `INPUT` modes, rule 3 would pick `DATE`).
  Break the flag (ignore it) and see the last case fail; break the array rule
  and see the `Join` row fail.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint`; clippy.

## Step 6. `CL006` and `CL007`

- `crates/oxabl_semantic/src/symbol.rs`: `unknown_members` (node, class row,
  name) and `call_misfits` (node, class row, member name or constructor, the
  count or the argument types), boxed side lists.
- `crates/oxabl_semantic/src/resolve.rs`: where a member lookup records
  `PresentButUnusable`, also record `unknown_members` when every condition of
  the design ("When the new rules stay silent") holds for `CL007`. The name is
  absent from every namespace of the surface.
- `crates/oxabl_semantic/src/check.rs` and `overload.rs`: the pick returns why
  no candidate is left (no count, or every candidate of the count has a `No`).
  For a `MethodCall` on a built-in receiver and a `NEW` of a built-in class,
  record `call_misfits` when every condition of the design holds for `CL006`.
  The `NEW` candidates are the constructors that are not static.
- `crates/oxabl_lint/src/rules/`: `member_call_mismatch.rs` (`CL006`) and
  `unknown_member.rs` (`CL007`), group `strict.calls`, preview, default
  warning, with module docs that have a bad and a good `abl` example (as
  `signature_mismatch.rs` has; `crates/oxabl_lint/tests/call_rules/doc_examples.rs`
  runs them). Register them in `registry.rs` and `rules/mod.rs`.
- Messages exactly as the design gives them.
- Docs of the rules: `crates/oxabl/skill/references/rules.md` (the
  `skill_rules` test compares it with the registry), the rule table of
  `docs/overview.md`, the rule list in `README.md`. Regenerate the JSON schema
  if it lists rule names
  (`cargo run -p oxabl -- schema > clients/vscode/schemas/oxabl.schema.json`,
  the CI job `Schema drift`).
- Lint tests in `crates/oxabl_lint/tests/call_rules/`: each finding and each
  silent case of the design.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint`;
`cargo test -p oxabl --test skill_rules --test skill_drift`; clippy. Break
the "complete" condition of `CL007` (judge every built-in class) and see a
silent case fail.

## Step 7. The surface API

- `crates/oxabl_semantic/src/surface.rs` (new): `ClassOrigin`,
  `SurfaceMember`, `ClassSurface`, `Supertypes`, `surface(...)` and
  `supertypes(...)` as the design gives them ("One interface for classes of
  every origin"). Move the level reading of `walk_member_surface` (what one
  level contributes, and what its supertypes are) into this module, and call
  it from the walk, so the two read one code path.
- Unit tests: the surface of `Progress.Lang.AppError` (own members, then
  `ProError`, `Error`, `Object`), of a workspace class through a stub index,
  of a local class that inherits a built-in one; `complete` for each case of
  the design.

Verify: `cargo test -p oxabl_semantic -p oxabl_lint`; clippy;
`cargo test -p oxabl --test class_members_cli --test class_members_lsp`.

## Step 8. Navigation

- `crates/oxabl_pipeline/src/navigate/hover.rs`: a built-in type
  (`symbols.builtin_class(sym)`) and a built-in member
  (`symbols.builtin_member(sym)`, the overloads through `surface`) render as the
  design gives them. No "Database effects" section for a built-in method
  (`symbol_routine` answers `None` for it). A `USING` of a built-in type has
  the hover of the type.
- `crates/oxabl_pipeline/src/navigate/semantic.rs`: check that a built-in type
  and member get the token of their kind with no code change; fix it if not.
- Go to definition, document links and references: no answer for a built-in
  symbol. Check `crates/oxabl_pipeline/src/navigation.rs` and
  `crates/oxabl_pipeline/src/references.rs`; a symbol with
  `NodeId::DUMMY` and no file already has none, so expect no code change.
- Document highlight: each use in the document; check that a resolved symbol
  with no declaration has its uses highlighted.
- Tests: `crates/oxabl/tests/hover_lsp.rs` cases for a built-in type, a method
  with overloads, a static property, an enum member and a `ToString` of a
  workspace class. Run
  `cargo test -p oxabl --test definition_lsp --test hover_lsp --test document_highlight_lsp --test semantic_tokens_lsp --test navigation_sweep_lsp --test references_lsp`.
  A changed expectation there is only acceptable when the old answer was "no
  answer" for a built-in name; list each one in the report.

Verify: `cargo build -p oxabl`;
`cargo test -p oxabl --test builtin_classes_cli --test builtin_classes_lsp`:
all cases pass. Then the regression set:
`cargo test -p oxabl --test class_members_cli --test class_members_lsp --test lint0004_cli --test check_cli`,
`cargo test -p oxabl_lsp`, `cargo test -p oxabl_daemon`.

## Step 9. Parity rows and benches

- `crates/oxabl_pipeline/src/fixtures.rs`: rows with a built-in member type
  (`l = oErr:GetMessage(1).` with its `LINT0004`), a user class of the file
  that inherits `Progress.Lang.AppError`, and an `Object` method. No index is
  needed, so every leg (CLI, language server, pipeline, WASM) gives the same
  findings. Run the legs: `cargo test -p oxabl --test parity_cli`,
  `cargo test -p oxabl_pipeline parity`, `cargo test -p oxabl_lsp --test parity_lsp`,
  `cargo test -p oxabl_wasm`.
- `crates/oxabl_semantic/benches/semantic_bench.rs`: the fixture and bench
  names `builtin_members` (design, "Cost").
- Measure `tiny`, `medium`, `skipped`, `builtin_classes`, `schema_heavy`,
  `class_members`, `class_members_indexed` and `builtin_members` on the base
  and on the branch with the same input (`cargo bench -p oxabl_semantic --bench
  semantic_bench`, or `valgrind --tool=callgrind` instruction counts when the
  machine is loaded). Put the numbers in the report.

Verify: `cargo test -p oxabl_semantic --benches`;
`cargo clippy -p oxabl_semantic --all-targets -- -D warnings`.

## Step 10. Documents

- `docs/design/builtin-classes.md`: change what the implementation decided
  differently, in a section "Implementation notes", as `class-members.md` has.
- `docs/design/class-members.md`: the sentence on `Progress.Lang.Enum` in
  "Enums", cause 4 of "When a surface is not complete" (the `Progress.Lang.Object`
  name rule is gone), the built-in rows of "Receivers that stay Unknown" and
  "Out of scope": point to the new design.
- `docs/design/navigation.md`: the hover row of a built-in type and member,
  and "no location" for definition and references.
- `docs/design/semantic-tokens.md`: built-in types and members have tokens.
- `docs/overview.md`: the `LINT0004` line (built-in members are judged) and
  the two new rules.
- `AGENTS.md`: the `oxabl_semantic` line names the catalog of built-in types
  and points to the design; the "Code Generation" section names
  `builtin-class-table` and the overrides file.
- `crates/oxabl/skill/` if a page names what oxabl knows of built-in classes.

Verify: `cargo test -p oxabl --test skill_drift --test skill_rules`.

## Step 11. Final checks

- `cargo fmt --all -- --check`, `cargo clippy --workspace -- -D warnings`,
  `cargo clippy -p oxabl --all-targets --features remote-cache,self-update --locked -- -D warnings`,
  `cargo test --workspace` (with `CARGO_BUILD_JOBS` as set).
- `cargo build -p oxabl_wasm --target wasm32-unknown-unknown --release` when
  the target is installed (CI runs it in the job `WebAssembly client`).
- `cargo run -p oxabl_codegen -- builtin-class-table` leaves no diff.
- `git log --format='%ae %ce' origin/master..HEAD` shows only the public
  address.
