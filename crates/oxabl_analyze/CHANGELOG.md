# Changelog

## [3.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v3.0.0...oxabl_analyze-v3.0.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_lint bumped from 3.0.0 to 3.0.1
    * oxabl_parser bumped from 3.0.0 to 3.0.1
    * oxabl_preprocessor bumped from 0.7.0 to 0.7.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_semantic bumped from 3.0.0 to 3.0.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
  * dev-dependencies
    * oxabl_index bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v2.0.0...oxabl_analyze-v3.0.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **pipeline:** `PreprocessedFile` has a new pub field, `skipped_text`. `MAX_INCLUDE_DEPTH` is now public.
* **pipeline:** CollectedDiagnostics has a new public field, include_origin, so code that builds it with a struct expression must set it.
* **analyze:** `oxabl_analyze::IncludeFinding` and `oxabl_analyze::CollectedDiagnostics` are `#[non_exhaustive]`. Code outside the crate can no longer build them with a struct expression or match them without `..`. Use `CollectedDiagnostics::new`; `IncludeFinding` is produced by the crate only.
* **lint:** `oxabl_analyze::CollectedDiagnostics` has a new public field, `include_findings`. Code that builds it with a struct literal must set it or use `..Default::default()`.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;
* **preprocessor:** `PreprocessedFile` has a new pub field, `skipped_includes`.
* **semantic:** `SymbolKind` has a new `Sequence` variant.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **pipeline:** list class and program names in not-taken &IF branches and the closure of conditional includes ([6b153d6](https://github.com/oxabl-project/oxabl/commit/6b153d631c4c6e4750fa13cc740f14cc9dd203ed))
* **pipeline:** retain include-origin findings with their compilation context ([950e08b](https://github.com/oxabl-project/oxabl/commit/950e08bc61f631828eb57261cc26f00a9b2f166f))
* **preprocessor:** carry include references in not-taken &IF branches ([b1057ac](https://github.com/oxabl-project/oxabl/commit/b1057ac88120afad1ce4d59acb2deb808a3fcc9e))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **semantic,analyze:** analyze a file without the type-check pass ([96c2aa4](https://github.com/oxabl-project/oxabl/commit/96c2aa48e3852106a09c0b73102b1f6a6d339de8)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)


### Bug Fixes

* **analyze:** mark IncludeFinding and CollectedDiagnostics non-exhaustive ([40a0bae](https://github.com/oxabl-project/oxabl/commit/40a0bae77f1a1c6a6cd4a0886522d719b0076b7a))
* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** report handle findings anchored in an include at the use site ([b414a76](https://github.com/oxabl-project/oxabl/commit/b414a76e4179e87c6b78a06d8fa732b883a631af))
* **lint:** skip LINT0004 on an assignment that an empty reference joins ([8ef08c6](https://github.com/oxabl-project/oxabl/commit/8ef08c6a2b52e8a246f5b161fca2cc128c5d101b))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** read a lock that unexpanded text may hide as Unknown ([ca9caae](https://github.com/oxabl-project/oxabl/commit/ca9caae9d8ed232f1a64271222b2fac34c58813e))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **analyze:** stop cloning every lint finding and recounting include expansions ([3fad8ca](https://github.com/oxabl-project/oxabl/commit/3fad8ca548baa586503cc6555e88ca58b010f30c))
* **lint:** pass the parsed tokens to the scope analysis ([901b4a0](https://github.com/oxabl-project/oxabl/commit/901b4a04bcecd4563db55ce99fcaee9ad2d57c02))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_lint bumped from 2.0.0 to 3.0.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v1.1.4...oxabl_analyze-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **index:** oxabl_index::DegradedReason, oxabl_index::Completeness and oxabl_preprocessor::TextLossKind are #[non_exhaustive]; an exhaustive match on any of them outside its crate no longer compiles.
* **index:** oxabl_index::EdgeInputs has a new field include_origins, and oxabl_analyze::ExpandedFile::unresolved_includes and unreadable_includes now also hold references reached only through the root's own {&var} substitutions, with the substitution as their site.
* **preprocessor:** PreprocessedFile gains text_losses, UnresolvedInclude gains unterminated, and ExpandedFile gains text_losses.
* **preprocessor:** PipelineConfig gains the public preprocessor field.
* **index:** DependencyEdge, UnresolvedReference and EdgeInputs gain an include_site field; the analyze envelope's dependency rows gain include_site (dependencies section version 4).
* **preprocessor:** CollectedDiagnostics gains the public parse_recovered field.

### Features

* **cli:** check context-free strict rules in the include files of a walked tree ([7b931f0](https://github.com/oxabl-project/oxabl/commit/7b931f0770f0149ff22c6f0c35658085682797c5))
* **index:** give every dependency edge an include-site origin ([89c08ac](https://github.com/oxabl-project/oxabl/commit/89c08ac453605a2d085d8dd3873675c90a8386a9)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **index:** mark the degradation and text-loss enums non-exhaustive ([6999df2](https://github.com/oxabl-project/oxabl/commit/6999df2dc5410ed79b6d92da6240c0d07e08f449))
* **preprocessor:** answer the AVM's predefined names ([d996c49](https://github.com/oxabl-project/oxabl/commit/d996c49d821b7e086a4e0dfb691962e3f4c6bd8f)), closes [#216](https://github.com/oxabl-project/oxabl/issues/216)
* **preprocessor:** keep unreadable and nested unresolved includes as data ([b56cf8b](https://github.com/oxabl-project/oxabl/commit/b56cf8bb426ff6c6eb1fb983bdc0c8d6a0006085))
* **preprocessor:** record lost text, rescan define values and match braces by quote ([57c7152](https://github.com/oxabl-project/oxabl/commit/57c71523e62e3d89fb796d18f61d6c196d8de3f8))


### Bug Fixes

* **index:** treat an include named through the root's own define as a direct include ([52f7734](https://github.com/oxabl-project/oxabl/commit/52f7734b971871884d32e2410e34ed9efa38c176))
* **preprocessor:** evaluate the functions an &IF names, or say it guessed ([026e29e](https://github.com/oxabl-project/oxabl/commit/026e29e4fc9fff19a17ace4b872f2210d15129da))
* **preprocessor:** expand include references with whitespace inside the braces ([82f05de](https://github.com/oxabl-project/oxabl/commit/82f05dee1506581e02016358d2a747b7a6563f90)), closes [#209](https://github.com/oxabl-project/oxabl/issues/209)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_lint bumped from 1.1.1 to 2.0.0
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_preprocessor bumped from 0.5.2 to 0.6.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_index bumped from 1.0.3 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1

## [1.1.4](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v1.1.3...oxabl_analyze-v1.1.4) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_lint bumped from 1.1.0 to 1.1.1
    * oxabl_parser bumped from 1.0.3 to 1.0.4
    * oxabl_semantic bumped from 1.1.0 to 1.1.1
  * dev-dependencies
    * oxabl_index bumped from 1.0.2 to 1.0.3

## [1.1.3](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v1.1.2...oxabl_analyze-v1.1.3) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_lint bumped from 1.0.3 to 1.1.0
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
  * dev-dependencies
    * oxabl_index bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.1.2](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v1.1.1...oxabl_analyze-v1.1.2) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))
* **daemon:** detect added files in freshness, and correct span extents and lookup cost ([#187](https://github.com/oxabl-project/oxabl/issues/187)) ([d50be6a](https://github.com/oxabl-project/oxabl/commit/d50be6a897742e622b0c4f281de4a6d1d9c2c355))
* **preprocessor:** report a nested unresolvable include instead of dropping it ([#177](https://github.com/oxabl-project/oxabl/issues/177)) ([676dafe](https://github.com/oxabl-project/oxabl/commit/676dafe4f3b5b3dc1d98ed43342033f567a7f778))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_lint bumped from 1.0.2 to 1.0.3
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_preprocessor bumped from 0.5.1 to 0.5.2
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_index bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.2 to 0.2.3

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v1.1.0...oxabl_analyze-v1.1.1) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_lint bumped from 1.0.1 to 1.0.2
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_preprocessor bumped from 0.5.0 to 0.5.1
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_index bumped from 0.1.2 to 1.0.0
    * oxabl_schema bumped from 0.2.1 to 0.2.2

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v1.0.0...oxabl_analyze-v1.1.0) (2026-08-12)


### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_lint bumped from 1.0.0 to 1.0.1
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_preprocessor bumped from 0.4.0 to 0.5.0
    * oxabl_schema bumped from 0.2.0 to 0.2.1
    * oxabl_semantic bumped from 1.0.0 to 1.0.1
    * oxabl_workspace bumped from 1.0.0 to 1.0.1
  * dev-dependencies
    * oxabl_index bumped from 0.1.1 to 0.1.2
    * oxabl_schema bumped from 0.2.0 to 0.2.1

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v0.1.0...oxabl_analyze-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.
* **semantic:** UnresolvedReason gains NotFoundInWorkspace and Unknowable. An exhaustive match on it will no longer compile. Both are skip-listed by every lint rule, so no diagnostic changes.
* **pipeline:** `oxabl check` no longer reports parse conformance. It lints and reports formatting drift, and its --json shape has changed accordingly. The previous behaviour, report text, --json shape, and exit codes are available unchanged as `oxabl conformance`.

### Features

* **ast:** full-fidelity spans on AST wrapper nodes ([#91](https://github.com/oxabl-project/oxabl/issues/91)) ([2e774b1](https://github.com/oxabl-project/oxabl/commit/2e774b1338bd9c9ff0db014e2cdc238db10801a7))
* capture SHARED/NEW SHARED flags on variable, temp-table, and buffer defines ([04abd6c](https://github.com/oxabl-project/oxabl/commit/04abd6c4860d6b3c0049a08c258b65c044b6e3b5))
* capture SHARED/NEW SHARED flags on variable, temp-table, and buffer defines ([2be6087](https://github.com/oxabl-project/oxabl/commit/2be60876efbe135278129e0f282e99794f3de1b9))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **oxabl_analyze:** add dump crate + oxabl analyze subcommand (Phase 6) ([47153ea](https://github.com/oxabl-project/oxabl/commit/47153eae53f9b1306dfa5bc50b919c736ca3a2ed))
* **pipeline:** one shared lint and format run behind every client ([#140](https://github.com/oxabl-project/oxabl/issues/140)) ([58d961e](https://github.com/oxabl-project/oxabl/commit/58d961e7d9b77e8887c78ee69580e60cc07f279f))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))
* seed full ABL system-handle set for LINT0001 ([b461dd7](https://github.com/oxabl-project/oxabl/commit/b461dd7336ff86cdedc874131ef7587933e1d121))
* **semantic:** the cross-file resolution seam, with the rules held still ([#146](https://github.com/oxabl-project/oxabl/issues/146)) ([3cee2c0](https://github.com/oxabl-project/oxabl/commit/3cee2c0a032c3e9d8176ca2740ed9c065e84824e))


### Bug Fixes

* complete shared-flag fields on schema-PR test fixtures after [#63](https://github.com/oxabl-project/oxabl/issues/63) merge ([d78cd4c](https://github.com/oxabl-project/oxabl/commit/d78cd4c777ac887c03b637ae7f5d7dc011a8badf))
* credit table reads in buffer, empty-table, and query forms ([#130](https://github.com/oxabl-project/oxabl/issues/130)) ([#138](https://github.com/oxabl-project/oxabl/issues/138)) ([6e39138](https://github.com/oxabl-project/oxabl/commit/6e3913885f513af727b1aee14973ff7936518132))
* reconcile FUNCTION FORWARD/IN SUPER prototype + definition ([#69](https://github.com/oxabl-project/oxabl/issues/69)) ([#73](https://github.com/oxabl-project/oxabl/issues/73)) ([beda23c](https://github.com/oxabl-project/oxabl/commit/beda23c81b953d8dbb080dfb2af4d7b6b0d1bd04))
* resolve PR [#140](https://github.com/oxabl-project/oxabl/issues/140) review findings ([#141](https://github.com/oxabl-project/oxabl/issues/141)) ([dd0849e](https://github.com/oxabl-project/oxabl/commit/dd0849ea72d38ee68c404af1d2414b093ef64459))
* **semantic:** resolve unqualified FIRST-OF/LAST-OF field against block buffers ([#111](https://github.com/oxabl-project/oxabl/issues/111)) ([6dcf5db](https://github.com/oxabl-project/oxabl/commit/6dcf5db754f9efc65b18c16989d89331e3d3035e)), closes [#107](https://github.com/oxabl-project/oxabl/issues/107)
* **semantic:** scope temp-table fields to their temp-table, not program scope ([#110](https://github.com/oxabl-project/oxabl/issues/110)) ([3d19165](https://github.com/oxabl-project/oxabl/commit/3d19165207ff07e62ae7874b7b43a9faa7b9e900)), closes [#106](https://github.com/oxabl-project/oxabl/issues/106)
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_lint bumped from 0.1.0 to 1.0.0
    * oxabl_parser bumped from 0.6.0 to 1.0.0
    * oxabl_preprocessor bumped from 0.3.1 to 0.4.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
    * oxabl_semantic bumped from 0.1.0 to 1.0.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
  * dev-dependencies
    * oxabl_index bumped from 0.1.0 to 0.1.1
    * oxabl_schema bumped from 0.1.0 to 0.2.0
