# Quick fixes: implementation steps

The design is `docs/design/quick-fixes.md`. Each step is one pull request that
ships on its own: it compiles, keeps every test green, has its own black-box
tests and updates the docs it changes. Commit with conventional commits and no
ticket key (the repository is public), for example
`feat(lint): add NO-UNDO as a quick fix`.

Checks used below:

- `cargo test -p <crate>` for each crate the step touches.
- `cargo clippy -p <crate> --all-targets -- -D warnings` for the same crates
  (see `.github/workflows/ci.yml` for the exact CI flags).
- `rustfmt --edition 2024 <changed files>`; never a writing `cargo fmt` over
  the workspace.

The black-box tests come from a separate tester, from a specification of
behaviour only. Each step names its test files. Do not change them. If one
looks wrong, say so in the pull request with the case name.

After a test or a guard is written, break the code it protects, see it fail
with the name of the assertion, and restore the code. Say in the pull request
which break you tried. Each step names one break to try.

## Order and parallel work

```text
1 model, JSON, NO-UNDO ──┬──> 2 check --fix ──────┬──> 4 lock fixes (LK001)
                         └──> 3 LSP code actions ─┼──> 5 remove unused variable (LINT0002)
                                                  └──> 6 add USING (LINT0001)
```

- Step 1 goes first. It holds every shared part: the model, the mapping and
  its conditions, the token guard, the keyword case, the JSON rows.
- Steps 2 and 3 can run in parallel. Step 2 changes the CLI
  (`crates/oxabl/src/main.rs`) and the apply engine in `oxabl_pipeline`.
  Step 3 changes `oxabl_lsp` and adds one non-waiting query to
  `oxabl_daemon`. They share no source file. Both add a section to
  `crates/oxabl/skill/references/check.md` or `docs/overview.md`; a rebase
  resolves that.
- Steps 4, 5 and 6 can run in parallel after steps 2 and 3, because each test
  file of theirs uses `--fix` and the code action request. Each changes its own
  rule file. Shared lines: one row each in the rule table of
  `crates/oxabl_lint/src/lib.rs` and of `crates/oxabl/skill/references/rules.md`.
  A rebase resolves them.
- Step 6 shares the class listing with completion step 6
  (`completion-steps.md`). Whichever lands first puts `ClassListing` in
  `oxabl_pipeline`; the other uses it. Do not make two listings.

## Step 1. The fix model, the JSON rows and the `NO-UNDO` fix

What a client sees: `oxabl check --json` gives `version: 6` and a `fixes`
array on each TX006 row that can be fixed.

- `crates/oxabl_common/src/diagnostic.rs`: `Fix`, `Edit`, `FixCondition`,
  `Deferred`, and `Diagnostic::fixes` with `with_fix`. Serialize them under the
  `serde` feature as the other fields. Add `fixes: Vec::new()` to each struct
  literal of `Diagnostic` (`crates/oxabl_schema/src/parser.rs`,
  `crates/oxabl_schema/src/schema.rs`, `crates/oxabl_semantic/src/types.rs`,
  `crates/oxabl_lsp/src/diagnostics.rs`, the tests).
- `crates/oxabl_formatter/src/guard.rs`: make the token stream and
  `tokens_equal` public to the workspace (`pub fn token_stream`,
  `pub fn tokens_equal`), with no change of behaviour.
- `crates/oxabl_pipeline/src/fix/` (new): `guard.rs`, the fix guard of the
  design (expected stream from the edits, token boundaries, TAB columns);
  `case.rs`, the keyword case, which calls the function of
  `completion/case.rs` (move it to a shared place in the crate if needed);
  `line_break` of a text. Export what the CLI and the LSP need.
- `crates/oxabl_analyze/src/collect.rs`: map the edits of each fix in the pass
  that maps the finding: rules 1 to 7 of "From expanded text to the file",
  and every `FixCondition`. Drop the fixes of findings in `include_origin` and
  `include_findings`, and every fix when `parse_recovered` is true.
- `crates/oxabl_pipeline/src/lint.rs` (`LintPipeline::collect` and the other
  paths that return a `LintResult`): write the keywords of each mapped edit in
  the style's keyword case, use the file's line break, and run rule 7 (the
  fix guard on the root's own text). `oxabl_analyze` sits beneath the pipeline
  and has no style, so these three are here.
- `crates/oxabl_lint/src/rules/missing_no_undo.rs`: the fix of the design
  (variables, `DEFINE ... PARAMETER`, temp-tables, properties; none for a
  shared definition and for a temp-table that writes `UNDO`). Find the type
  phrase with the statement's tokens (`leading_tokens` in `syntactic.rs`, or a
  helper beside it). Update the module documentation: the fix is no longer
  "planned".
- `crates/oxabl/src/main.rs`: `CheckJsonDiagnostic::fixes` (`CheckJsonFix`,
  `CheckJsonEdit`), version 6 with its line in the version comment. Drop the
  fixes of a file that another root of the same run includes: the run knows
  each root's include paths (`Expansion::dependency_paths`); filter when the
  report is put together.
- Docs: `crates/oxabl/skill/references/check.md` (the JSON section: version
  6 and `fixes`; it says version 3 today), the TX006 line of
  `crates/oxabl/skill/references/rules.md` ("has a fix"), the module docs
  above.

Tests (tester): `crates/oxabl/tests/quick_fix_json.rs`.

Verify: `cargo test -p oxabl_common -p oxabl_formatter -p oxabl_lint
-p oxabl_analyze -p oxabl_pipeline`, `cargo test -p oxabl --test
quick_fix_json --test check_cli --test skill_drift`, clippy. Break: map an edit
in include text as own text (skip rule 2), and see the include case fail.

## Step 2. `oxabl check --fix`

- `crates/oxabl_pipeline/src/fix/apply.rs`: the apply engine of the design
  (collect, dedupe, drop subsumed fixes, sort, overlap, apply from the end,
  the guard on the whole file). It returns the fixed text or a refusal with
  one of the reason texts of the design. It does not read or write files.
- `crates/oxabl_common/src/panic_guard.rs`: the marker `OXABL-TEST-FAULT` with
  the sites `fix.conflict` and `fix.guard`, the panic site `fix.apply` and
  the hold site `fix.write`, all under the `test-panics` feature, as the other
  markers.
- `crates/oxabl/src/main.rs`: the flags `--fix` and `--fix-only`, their usage
  errors, and the run in two phases: first analyse every file as today; then,
  for each file with preferred fixes, apply, analyse the fixed text again
  (parse and "a fixed finding is still reported"), re-read the file, encode,
  write. The findings, the format channel and the exit code come from the text
  on disk at the end. The stderr lines and the JSON `fix` object of the
  design. Contain a panic of the engine per file (`catch_panic`).
- Docs: `crates/oxabl/skill/references/check.md` (a `--fix` section, with each
  shell example in an `sh` fence: `skill_drift` checks them against
  `--help`), `docs/overview.md` where it lists the `check` flags.

Tests (tester): `crates/oxabl/tests/quick_fix_cli.rs`.

Verify: `cargo test -p oxabl_pipeline`, `cargo test -p oxabl --test
quick_fix_cli --test quick_fix_json --test check_cli --test skill_drift
--test skill_cli`, clippy. Break: write the file when the guard fails, and see
the `fix.guard` case fail.

## Step 3. Code actions in the language server

- `crates/oxabl_lsp/src/capabilities.rs`: `code_action_provider` with the
  kind `quickfix` and `resolve_provider: false`. Update the module text and the
  capability unit test.
- `crates/oxabl_lsp/src/code_action.rs` (new): parse the params, check
  `context.only`, take the document's diagnostics from the memoized query,
  select the findings that touch the range, make one `CodeAction` per fix
  (`diagnostics` made by the same conversion as `publishDiagnostics`,
  `isPreferred`, `documentChanges` with the version or `changes`), sort. An
  include file and a document in an includer context answer `[]`; a document
  that is not open answers `null`.
- `crates/oxabl_lsp/src/lib.rs`: dispatch `textDocument/codeAction` as the
  other document requests (cancellation, `ContentModified`).
- `crates/oxabl_daemon/src/methods.rs`: `included_by_another_unit(host, root,
  file) -> Option<bool>` from the newest workspace snapshot, with no wait and no
  new pass. `Some(true)` drops the fixes of the file; `None` and `Some(false)`
  keep them (the design's known gap).
- Docs: `clients/vscode/README.md` (the feature list: quick fixes),
  the module text of `oxabl_lsp`.

Tests (tester): `crates/oxabl/tests/quick_fix_lsp.rs`.

Verify: `cargo test -p oxabl_lsp -p oxabl_daemon`, `cargo test -p oxabl
--test quick_fix_lsp --test lsp_stdio_smoke`, clippy. Break: send `changes`
to a client that advertises `documentChanges`, and see the version case fail.

## Step 4. Lock fixes (LK001)

- The end of the record phrase. `crates/oxabl_parser` and `crates/oxabl_ast`:
  keep the end offset of each record phrase (`FIND`, `FOR`/`PRESELECT` phrases,
  `CAN-FIND`, `OPEN QUERY`) where the parser reads it. Keep the AST size
  assertions true (`docs/design/ast-invariants.md`); a side table keyed by node
  id is an option. `crates/oxabl_scope/src/model.rs`: `RecordRead::phrase_end:
  Option<u32>`, filled from it. No end, no fix.
- `crates/oxabl_lint/src/rules/default_lock.rs`: the two fixes, the
  preferred one from the help (the same match as `help`), `CAN-FIND` with
  `NO-LOCK` only, the lock before a `NO-WAIT` of the phrase. Update the module
  text ("No autofix is offered yet").
- Docs: the LK001 line of `crates/oxabl/skill/references/rules.md`.

Tests (tester): `crates/oxabl/tests/quick_fix_lock.rs`.

Verify: `cargo test -p oxabl_parser -p oxabl_scope -p oxabl_lint`, `cargo test
-p oxabl --test quick_fix_lock --test inlay_hints_lsp`, clippy, and the parser
benchmark against the base when the parser changes (`AGENTS.md`,
"Benchmarks"). Break: insert after the table name, and see the `WHERE` case
fail.

## Step 5. Remove an unused variable (LINT0002)

- `crates/oxabl_lint/src/rules/unused_variable.rs`: find the declaring
  statement of the symbol (a walk of the program for the
  `VariableDeclaration` whose name span is the declaration span), check the
  rules of the design (variable, one name, own lines, `PRIVATE` in a class),
  make the delete edit with the conditions `OutsideConditionalBranch`,
  `NotInSkippedText(name)` and `WholeExpansion`.
- Docs: the LINT0002 line of `crates/oxabl/skill/references/rules.md`, the
  rule table in `crates/oxabl_lint/src/lib.rs`.

Tests (tester): `crates/oxabl/tests/quick_fix_unused.rs`.

Verify: `cargo test -p oxabl_lint`, `cargo test -p oxabl --test
quick_fix_unused --test check_cli`, clippy. Break: leave out
`NotInSkippedText`, and see the `&IF` case fail.

## Step 6. Add a `USING` (LINT0001)

- `crates/oxabl_pipeline/src/class_listing.rs` (new, or moved from the daemon
  if completion step 6 put it there): `ClassListing` as `completion.md`
  describes it, and `ClassListing::by_simple_name(name) -> Candidates` with
  `complete: bool`.
- `crates/oxabl_pipeline/src/completion/` or `fix/using.rs`: the one
  `USING` edit function, from the root's own text. Completion step 6 uses it
  too.
- `crates/oxabl_lint/src/rules/undefined_symbol.rs`: the deferred fix
  (`Deferred::AddUsing`) on a `NEW`, `CAST`, `TYPE-OF` or `GET-CLASS` finding
  with reason `AbsentFromWorkspace` and no `.` in the name.
- `crates/oxabl_pipeline/src/fix/`: fill a deferred fix from the listing, or
  drop it. Then map its edit with the rules of step 1.
- `crates/oxabl/src/main.rs`: make the listing once a run, at the first
  deferred fix.
- `crates/oxabl_daemon`: keep the listing as completion does (made on a
  worker thread, dropped on a watched `.cls` create or delete and on a
  configuration change). Until the listing is made, a code action request
  makes it on its own thread and waits for it.
- Docs: the LINT0001 line of `crates/oxabl/skill/references/rules.md`.

Tests (tester): `crates/oxabl/tests/quick_fix_using.rs`.

Verify: `cargo test -p oxabl_pipeline -p oxabl_lint -p oxabl_daemon`,
`cargo test -p oxabl --test quick_fix_using --test check_cli`, clippy. Break:
offer the fix with two candidates (take the first), and see the ambiguous case
fail.
