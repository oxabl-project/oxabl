# Code completion: implementation steps

The design is `docs/design/completion.md`. Each step is one pull request that
ships on its own: it compiles, keeps every test green, has its own black-box
tests and updates the docs it changes. Commit with conventional commits and no
ticket key (the repository is public), for example
`feat(lsp): complete statement keywords`.

Checks used below:

- `cargo test -p <crate>` for each crate the step touches.
- `cargo clippy -p <crate> --all-targets -- -D warnings` for the same crates
  (see `.github/workflows/ci.yml` for the exact CI flags).
- `rustfmt --edition 2024 <changed files>`; never a writing `cargo fmt` over
  the workspace.
- For `clients/vscode`: `pnpm install --frozen-lockfile`, `pnpm run lint`,
  the typecheck and `pnpm test`, as CI runs them, and the e2e command of
  `AGENTS.md`.

The black-box tests come from a separate tester, from a specification of
behaviour only, into `crates/oxabl/tests/completion_*_lsp.rs`. Each step names
its files. Do not change them. If one looks wrong, say so in the pull request
with the case name.

After a test or a guard is written, break the code it protects, see it fail
with the name of the assertion, and restore the code. Say in the pull request
which break you tried.

## Order and parallel work

```text
1 foundations ──> 2 engine and keywords ──> 3 names in scope ──┬──> 4 records and fields
                                                               ├──> 5 members ──> 6 types and USING
                                                               └──> 7 RUN and includes
```

- Steps 1, 2 and 3 go in order.
- After step 3, steps 4, 5 and 7 can run in parallel. Each adds its own
  collector file under `crates/oxabl_pipeline/src/completion/` and one arm in
  the dispatch of `completion/mod.rs`; that arm is the only shared line, and a
  rebase resolves it.
- Step 6 needs the `ClassSource` trait of step 5.
- Step 2 writes the whole context finder (`completion/context.rs`), every row
  of the design's table, so later steps do not edit it. A context with no
  collector yet answers `null`.

## Step 1. Foundations

No change that a client can see. Each part has tests in its crate.

- **Unclosed blocks.** `crates/oxabl_parser/src/parser/mod.rs`: `Program`
  gets `unclosed_blocks: Vec<NodeId>`, the block statements that
  `recover_block_end` closed without `END` at the end of the text. Fill it
  there. Keep the size assertions of the AST true.
- **Scope spans.** `crates/oxabl_semantic/src/scope.rs`: `Scope` gets
  `span: VirtualSpan`. `ScopeTree::push` takes it. The declare pass
  (`crates/oxabl_semantic/src/resolve.rs`, `Walker`) gives each scope the span
  of its owner statement; the file scope covers the whole text; a scope whose
  owner is in `unclosed_blocks` ends at the end of the text (pass the list in
  through `AnalysisContext`, `with_unclosed_blocks`). Add
  `ScopeTree::innermost_at(offset) -> ScopeId`. `oxabl_analyze`
  (`analyze_expanded` in `collect.rs`) passes the parser's list.
- **All places of a physical byte.** `crates/oxabl_analyze/src/collect.rs`:
  `ExpandedFile::expanded_offsets(file, real) -> Vec<u32>`, every chunk that
  holds the byte, in expansion order. `expanded_offset` stays and returns the
  first.
- **Directory listing.** `crates/oxabl_workspace/src/file_system.rs`:
  `FileSystem::list_dir(&self, dir: &Path) -> Option<Vec<DirEntry>>` with
  `DirEntry { name: String, is_dir: bool }`. The default is `None` ("cannot
  list"). Implement it for `RealFileSystem`, `EncodedFileSystem`,
  `InMemoryFileSystem`, and forward it in `CachedFileSystem` and
  `ObservingFileSystem`. The daemon's `OverlayFs`
  (`crates/oxabl_daemon/src/db.rs`) forwards it too.

Verify: `cargo test -p oxabl_parser -p oxabl_semantic -p oxabl_analyze -p oxabl_workspace -p oxabl_daemon`;
clippy on the same. Tests: a parser test that `PROCEDURE p:` with no `END`
at the end of the text is in `unclosed_blocks` and a closed one is not; a
semantic test that the procedure scope of that text holds the last byte; a
test of `innermost_at` for nested blocks; an analyze test that an include
read twice gives two offsets; a workspace test that `list_dir` lists files and
directories of the in-memory and the real file system, and that a missing
directory gives `None`.

## Step 2. The engine, statement keywords and snippets

The first visible step: `textDocument/completion` with statement keywords and
the server's snippets, and the budget. The extension hands its snippets over
in the same step, so no client loses them.

- `crates/oxabl_pipeline/src/completion/` (new):
  - `mod.rs`: the request and answer types, the dispatch to collectors, the
    limit (200) and `isIncomplete`.
  - `context.rs`: the word, the replace range, the statement window, and every
    row of the context table of the design.
  - `rank.rs`: the matcher (monotone, part starts) and the rank keys.
  - `case.rs`: the keyword case of the design (`Preserve` included).
  - `keywords.rs`: the statement keywords by scope kind, and the keywords
    after `DEFINE`, from `keyword_docs.rs`.
  - `snippets.rs`: generated (below).
  - Export from `crates/oxabl_pipeline/src/lib.rs`.
- `crates/oxabl_codegen`: a target `snippets` that writes
  `crates/oxabl_pipeline/src/completion/snippets.rs` from
  `clients/vscode/snippets/abl.json` (prefix, description, body with the
  `<<...>>` markers, and the scope kinds where each is valid). Add it to the
  default target list, to `AGENTS.md` ("Code Generation"), and to the CI job
  `Grammar drift` (regenerate, then `git diff --exit-code` on the file).
- `crates/oxabl_daemon/src/completion.rs` (new): `Bases` (per open document,
  root contexts come in step 3), the map of a stale base, the refresh (one per
  document; `hold_if_injected("completion.refresh", text)` before it
  analyses), `compute_completion` and `compute_resolve` on a worker snapshot.
  Close a document, change the configuration or the schema: drop its bases.
- `crates/oxabl_lsp/src/completion.rs` (new): the handler, a gate like
  `HoverGate` (newest wins, `$/cancelRequest`), rendering (`textEdit`,
  `InsertReplaceEdit` when the client supports it, snippet items only with
  `snippetSupport`), and `completionItem/resolve` for keywords.
  `crates/oxabl_lsp/src/capabilities.rs`: `completionProvider` with the
  trigger characters and `resolveProvider`; update the capability test.
  `crates/oxabl_lsp/src/lib.rs`: dispatch both methods and route
  `$/cancelRequest` to the gate.
- `clients/vscode/src/snippetCompletion.ts` and `src/extension.ts`: do not
  register the snippet provider when the server advertises
  `completionProvider`; register it when no server runs. Update
  `docs/design/vscode-extension.md` ("Snippets", "Hand-off to the server's
  completion") and `clients/vscode/README.md`.
- `crates/oxabl_pipeline/benches/completion_bench.rs` and its `[[bench]]` in
  `crates/oxabl_pipeline/Cargo.toml`: the 10,000-line file of
  `semantic_tokens_bench`, benches `fresh` and `stale` (the design's
  "The budget"; until steps 3 to 5 land, the contexts are a statement start
  and `DEFINE `). Put the numbers in `completion.md`.
- `AGENTS.md`: name the completion module under `crates/oxabl_pipeline` with
  a link to the design.

Tests (tester): `crates/oxabl/tests/completion_keywords_lsp.rs`,
`completion_snippets_lsp.rs`, `completion_budget_lsp.rs`; the VS Code e2e
scenario `completion` (`clients/vscode/test/e2e/scenarios/completion.mjs`) and
its row in `clients/vscode/test/e2e/README.md`.

Verify: `cargo test -p oxabl_pipeline -p oxabl_daemon -p oxabl_lsp -p oxabl_codegen`,
`cargo test -p oxabl --test completion_keywords_lsp --test completion_snippets_lsp --test completion_budget_lsp`
(the dev-dependency on `oxabl_common` arms the hold marker, as for
`hover_cancel_lsp.rs`);
clippy on the same; `cargo run -p oxabl_codegen -- snippets` leaves no diff;
`cargo bench -p oxabl_pipeline --bench completion_bench`; the
`clients/vscode` checks and the e2e scenarios `completion` and `snippets`.
Break: make the request path wait for the refresh, and see the budget test
fail; make `Preserve` always upper case, and see the keyword case test fail.

## Step 3. Names in scope and include contexts

- `crates/oxabl_pipeline/src/completion/names.rs`: the "Values" collector
  (scope at the cursor through `innermost_at`, the declaration-before-use
  rule, functions and `FORWARD`, class members of the own class, built-in
  functions), names at a statement start, the expression context, and the
  fit and nearness rank keys (`coercion::assignable`). Drop synthesized
  symbols and symbols declared in the changed region of a stale base. The
  scope-token rule of a stale base (no names when the changed region opens or
  closes a scope).
- Inherited members of the own class need `class_surface` (step 5). Until
  then, a class whose `INHERITS` names a class gives its own members and
  `isIncomplete: true`.
- `crates/oxabl_daemon/src/completion.rs`: bases for an include with a
  selected root (from `with_navigation`, kept under the key of the include and
  the root), fragment-mode bases, several expansions (names visible at every
  place), the window that reaches the start of an include.
- Resolve for a symbol of the unit: `Navigation::hover` at the name of its
  declaration, in the base the item came from (keep the two newest bases of
  each document by id).

Tests (tester): `crates/oxabl/tests/completion_names_lsp.rs`,
`completion_include_lsp.rs`.

Verify: `cargo test -p oxabl_pipeline -p oxabl_daemon -p oxabl_lsp` and the
two test files; clippy. Extend `completion_bench` with an expression context.
Break: drop the declaration-before-use rule, and see the test of a name
defined after the cursor fail.

## Step 4. Records and fields

Step 3 already answers the "Records" rows with the records in scope and the
tables of the schema (`names.rs`, `Wanted::Records`), because a statement
start offers records too. Step 4 adds `records.rs` with the qualified names
and the fields, and can move the record rows there.

- `crates/oxabl_pipeline/src/completion/records.rs`: the "Records" collector
  (buffers, temp-tables, tables, metaschema tables only after `_`, the rows of
  the design), the "Qualified names and fields" collector (schema fields with
  type and format, temp-table fields with `LIKE` through
  `EffectiveTable::declare`, buffers), fields first in a `WHERE`, the
  database logical names (`.df` file name without the extension) and
  `db.` / `db.t.`.
- Resolve: `schema_hover::table_markdown` and `field_markdown`.
- Server snippets: the table choice of `find` and `foreach` with at most 30
  tables (`completion/snippets.rs` stays generated; the choice is made when
  the item is built).

Tests (tester): `crates/oxabl/tests/completion_records_lsp.rs`.

As built: the record rows stay in `names.rs`; `records.rs` holds the table
items (moved from `names.rs`), the qualified names, the fields and the
`WHERE` collector. Right after a record keyword only a database qualifies
(`FIND FIRST shop.`), and that case needs no base. Fields come from the
semantic record links (`SymbolTable::record_source`, `temp_table_fields`,
`temp_table_like`, now public), which give the type of each field, in the
order of `EffectiveTable::declare`. The match rule now lets the first
character of the word start any part of the label, as the design says (`cu`
matches `bCust`).

Verify: `cargo test -p oxabl_pipeline -p oxabl_daemon` and the test file;
clippy. Extend `completion_bench` with `FIND FIRST` and `Customer.`. Break:
offer tables after an unknown qualifier, and see the `null` case fail.

## Step 5. Members after `obj:` (done)

- `crates/oxabl_semantic/src/surface.rs` (new): `ClassSource` (with
  `classes_in`; the workspace index answers it with an empty, incomplete list
  until step 6), `ClassRef`, `Reach`, `ClassSurface`, `SurfaceMember`,
  `MemberSignature`, `MemberOrigin`, and `class_surface` with the access,
  static and completeness rules of `class-members.md`. It walks supertypes
  with `lookup_supertype` and a visited set. Every class surface is
  incomplete until a source answers for `Progress.Lang.Object`.
- `crates/oxabl_pipeline/src/completion/members.rs`: receiver typing from
  tokens (the forms of the design's table, and `null` for every other form),
  one item per member name, method details with the overload count.
- Step 3's inherited members of the own class now come from `class_surface`
  with role `ThisObject`.
- Resolve: the signature of each overload and the declaring class.
- `docs/design/class-members.md`: a line in "Out of scope, and the seams
  left" that completion reads the same rules through `class_surface`.

Tests (tester): `crates/oxabl/tests/completion_members_lsp.rs`.

Verify: `cargo test -p oxabl_semantic -p oxabl_pipeline -p oxabl_daemon` and
the test file; clippy. Extend `completion_bench` with `obj:`. Break: let
`TypeName` reach instance members, and see the static case fail.

As built: `ClassSource` has one more implementer than the plan says, the
daemon's per-base memo of the index (`ClassMemo`, in `completion/members.rs`):
a request reads each class file once for a base, and a watched file change
drops what the bases kept. The type is `MemberReach`, not `Reach`
(`coercion::Reach` exists), and `class_surface` takes a fifth argument,
`inside`, for the access to the private members of the class the cursor is in.
The trait of the type hierarchy that was also named `ClassSource` is now
`oxabl_index::hierarchy::ClassFiles`. A data member of a class of the analysed
file keeps its access modifier in `SymbolTable::member_access`, and the
imports of the file and the name of each synthesized class symbol are kept in
the symbol table (`imports`, `indexed_class_name`), because the surface walk
needs them after the resolve pass ended. A list of members has every member of
the surface, and the items that match the word come first. The context finder
reads a keyword that touches a `.` or a `:` on its left (`app.Color:`) as a
receiver. At a statement start, a word that is a statement and a name in scope
(`THIS-OBJECT`) is one item. Black-box tests of what the tester's file does not state (the class
of the open file, the inherited names of a class body, the method detail, the
receivers that give `null`, and the facts a base keeps until a file changes)
are in `crates/oxabl/tests/completion_members_extra_lsp.rs`.

## Step 6. Types and the `USING` edit

- `crates/oxabl_daemon/src/completion/listing.rs` (or a module beside
  `completion.rs`): `ClassListing` (the walk of each include path entry with
  `list_dir`, the header scan for kind and `ABSTRACT`, the 50,000-file limit,
  the first entry wins), made on a worker thread at the first type request,
  dropped on a watched `.cls` create or delete and on a configuration change,
  remade after 60 seconds for entries outside the watched folders. The
  workspace `ClassSource::classes_in` answers from it.
- `crates/oxabl_pipeline/src/completion/types.rs`: the "Types" collector, the
  data types by position, packages with `editor.action.triggerSuggest`, and
  the `USING` edit with its rules (made from the current text).

Tests (tester): `crates/oxabl/tests/completion_types_lsp.rs`.

Verify: `cargo test -p oxabl_daemon -p oxabl_pipeline -p oxabl_semantic` and
the test file; clippy. Break: add the `USING` edit in an include file, and see
the include case fail.

## Step 7. `RUN`, include files, preprocessor names and include arguments

- `crates/oxabl_preprocessor/src/span_tree.rs` and `preprocessor.rs`:
  `PreprocessedFile::defines`, each `&GLOBAL-DEFINE`, `&SCOPED-DEFINE` and
  `&UNDEFINE` with its name, scope and expanded offset. Carry it on
  `ExpandedFile`.
- `crates/oxabl_pipeline/src/completion/paths.rs`: `RUN` (internal
  procedures, programs by `run_extensions` and `.r`, directories), `{` (files,
  `.i` first, directories), `{&` (defined names at the cursor, the root's named
  arguments, the names the preprocessor defines), `{n}` (positional arguments
  of the including references).
- Resolve: the path relative to its include path entry.

Tests (tester): `crates/oxabl/tests/completion_paths_lsp.rs`.

As built: the preprocessor keeps `PreprocessedFile::defines` (`DefineSite`,
in the coordinates of the file that writes each) and
`PreprocessedFile::include_arguments`; `define_offsets` places the defines
in the expanded text, and `ExpandedFile::defines` (`PlacedDefine`) and
`ExpandedFile::include_arguments` carry them. `CompletionRequest` has the
configuration and the file system, so `RUN` and `{` list directories with no
base. Crate tests: `crates/oxabl_pipeline/tests/completion_paths.rs` (a
directory that cannot be listed, a directory that does not exist, the scope
of a `&SCOPED-DEFINE`).

After review: the record of the defines and the include arguments is
opt-in (`with_define_record`), so only the bases of completion pay for it.
`{&` replays the directives with a stack for each name. The listings of
`RUN` and `{` are bound: once per directory in a request, kept three seconds
in the daemon, filtered by the word, at most 1000 matches of a directory.
Black-box tests of the review: `crates/oxabl/tests/completion_cases_lsp.rs`.

Verify: `cargo test -p oxabl_preprocessor -p oxabl_analyze -p oxabl_pipeline -p oxabl_daemon`
and the test file; clippy. Break: list directories with `None` treated as
empty, and see the `isIncomplete` case fail.

## When all steps are done

- Every row of the issue's context table has a test file above.
- `completion.md` "The budget" has the bench numbers of step 2 and of each
  later context.
- The roadmap item for snippets (#449) can close: server snippets with the
  tables of the `.df` came in step 4.
