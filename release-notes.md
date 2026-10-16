:robot: I have created a release *beep* *boop*
---


<details><summary>oxabl: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v2.0.0...oxabl-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **pipeline:** `ConditionalIncludeClosure` has a new `references` field and is now `#[non_exhaustive]`.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **pipeline:** CollectedDiagnostics has a new public field, include_origin, so code that builds it with a struct expression must set it.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **pipeline:** the edge_set_with callback receives a LintResult without lint findings or type-check diagnostics; edge_set runs no lint rules.
* **lint:** `oxabl_analyze::CollectedDiagnostics` has a new public field, `include_findings`. Code that builds it with a struct literal must set it or use `..Default::default()`.
* **preprocessor:** `PreprocessedFile` has a new pub field, `skipped_includes`.
* **semantic:** `SymbolKind` has a new `Sequence` variant.
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **cli:** add check --watch ([e01b176](https://github.com/oxabl-project/oxabl/commit/e01b176619848dd87a7a5bc1faa5b9f70334513f)), closes [#144](https://github.com/oxabl-project/oxabl/issues/144)
* **cli:** analyse files in parallel with `check --jobs` ([a3a2978](https://github.com/oxabl-project/oxabl/commit/a3a2978132d6c291c1948e428dfa2ff187a5153b))
* **effects:** check index choices against COMPILE ... XREF output ([ed7321b](https://github.com/oxabl-project/oxabl/commit/ed7321bf6a26f5391c215c9de832927bc942d95d))
* **effects:** measure call-graph coverage, summaries and TX004 over a tree ([b3a369c](https://github.com/oxabl-project/oxabl/commit/b3a369ccb65c23defc81ba95ef03f23957d550bf))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **examples:** count conditional references, include closures and degraded roots in the edge census ([04ef3cd](https://github.com/oxabl-project/oxabl/commit/04ef3cd6e86c814e1366fb70f53d773905ab005c))
* **handles:** add oxabl_handles, lifetimes of dynamic objects per unit ([005ae67](https://github.com/oxabl-project/oxabl/commit/005ae67771f889bd80c71d268554ae43b16b3b3d))
* **handles:** read compound guards, failed NO-ERROR creates and class widget pools ([881727e](https://github.com/oxabl-project/oxabl/commit/881727ee87815748f33a87281d9d1ed56c9f4fac))
* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lint:** add NL001 and NL002 unknown-value warnings ([17e0c6c](https://github.com/oxabl-project/oxabl/commit/17e0c6c0c6f94ba01729a4b3bf50b3380722cd28))
* **lint:** add NL004 nullable-db-field ([0ea9cd2](https://github.com/oxabl-project/oxabl/commit/0ea9cd24ad501d88c919c0ec4694a793b5443127))
* **lint:** add the strict.access rule DB001 find-in-loop ([df71f57](https://github.com/oxabl-project/oxabl/commit/df71f57b41299e34f0fd02a86733120a378de266))
* **lint:** add the strict.handles rules HD001-HD005 ([0dd3530](https://github.com/oxabl-project/oxabl/commit/0dd3530cbf90c1f073a5243dc501eb6f1a60b6b1))
* **lint:** add the strict.locks rules LK001-LK003 ([318f55f](https://github.com/oxabl-project/oxabl/commit/318f55fd9a7367bf78a6475fb75056bc494abc2f))
* **lint:** add the strict.transactions rules TX001-TX005 ([1253cc8](https://github.com/oxabl-project/oxabl/commit/1253cc83c520d4f321abafe9c7704b76641905bd))
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **nulls:** count NL003 findings per directory over a tree ([1254ce4](https://github.com/oxabl-project/oxabl/commit/1254ce4793e5b47861d467c2a1d46c6954fc2e1f))
* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **nulls:** treat database fields the schema starts at ? as sources of unknown values ([3b39aba](https://github.com/oxabl-project/oxabl/commit/3b39abaecb3b1477f1a2e26444511e27ca7c6ced))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **pipeline:** list the class and program names a conditional include's target writes ([81462eb](https://github.com/oxabl-project/oxabl/commit/81462eb81a826051c2e285bf6f0d855f5dbd316c))
* **pipeline:** retain include-origin findings with their compilation context ([950e08b](https://github.com/oxabl-project/oxabl/commit/950e08bc61f631828eb57261cc26f00a9b2f166f))
* **preprocessor:** carry include references in not-taken &IF branches ([b1057ac](https://github.com/oxabl-project/oxabl/commit/b1057ac88120afad1ce4d59acb2deb808a3fcc9e))
* **scope:** add oxabl_scope, the compiler's buffer scopes, transaction blocks and locks per unit ([cebe727](https://github.com/oxabl-project/oxabl/commit/cebe727b779e8d0806aa41e60d2968f56ed0cca6))
* **scope:** check the scope engine against COMPILE ... LISTING output ([19f0ae2](https://github.com/oxabl-project/oxabl/commit/19f0ae2d57391ee48d79c747f1a7b81aa9cafe33))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* **cli:** share callee summaries across check --watch handles ([4552d86](https://github.com/oxabl-project/oxabl/commit/4552d863a6b61fba5c70f44954987735293a954a))
* **handles:** model calls, :HANDLE identity, unmapped frees, COPY-LOB, shadowed frees and a work budget ([05f23ef](https://github.com/oxabl-project/oxabl/commit/05f23ef729196a3175317ac9d3b89815c3042c03))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lexer:** stop printing to stdout on an overflowing number literal ([14d970e](https://github.com/oxabl-project/oxabl/commit/14d970efeacf9ac7b8112772348fbb00ff32f1ed))
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** report handle findings anchored in an include at the use site ([b414a76](https://github.com/oxabl-project/oxabl/commit/b414a76e4179e87c6b78a06d8fa732b883a631af))
* **lint:** update example for is_prototype ([601246a](https://github.com/oxabl-project/oxabl/commit/601246ab207e5aa2ace7bb83d551087c8886415b))
* **parser:** read database-qualified names in record statements ([6d8cbf1](https://github.com/oxabl-project/oxabl/commit/6d8cbf10c5e64de5963dde999b0727d24b41e697))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** pair LISTING blocks by the end of the file path ([8965d2d](https://github.com/oxabl-project/oxabl/commit/8965d2d115bf1e3673a1e33c777184d3492ed78a))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **check:** scan the SHARED producer map on every core ([fa951a2](https://github.com/oxabl-project/oxabl/commit/fa951a247a9754d992970c543437ba5d0d159b7d))
* **cli:** build the watch cycle's reverse graph on the check's jobs ([7a20d8e](https://github.com/oxabl-project/oxabl/commit/7a20d8e52629b9026b9573dc04fbb03712d8f6b8))
* **examples:** scan the edge_pass producer map on the run's threads ([4a5bde8](https://github.com/oxabl-project/oxabl/commit/4a5bde8455c6e385080fa93d02bd9d645744ebfb))
* **nulls:** bound the work of the null analyses ([edbd5cb](https://github.com/oxabl-project/oxabl/commit/edbd5cb128509b93fd3f51dee3a939430055b3b2))
* **nulls:** bound the work of the null analyses and report units that fall back ([5cebc88](https://github.com/oxabl-project/oxabl/commit/5cebc88ee58a024e00972a8990f1df7483dd53cc))
* **pipeline:** build edge sets without lint rules or type checks ([bcf44d3](https://github.com/oxabl-project/oxabl/commit/bcf44d36ba12efa7fed1558bac1bb1bf4ddc3733)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_lint bumped from 2.0.0 to 3.0.0
    * oxabl_lsp bumped from 2.0.0 to 3.0.0
    * oxabl_daemon bumped from 0.4.0 to 0.5.0
    * oxabl_formatter bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_handles bumped from 0.1.0 to 0.2.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
    * oxabl_effects bumped from 0.1.0 to 0.2.0
    * oxabl_nulls bumped from 0.1.0 to 0.2.0
    * oxabl_index bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_analyze: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_analyze-v2.0.0...oxabl_analyze-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

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
</details>

<details><summary>oxabl_ast: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_ast-v2.0.0...oxabl_ast-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind::Update and StatementKind::Set gain a fields field.
* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **parser:** StatementKind::Form and StatementKind::DefineFrame have a new `fields: Vec<FrameField>` field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** Statement has a new public field, skipped_tail.
* **parser:** AccessModifier gains a PackageProtected variant.
* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** credit reads inside the skipped tails of modelled statements ([c93aa74](https://github.com/oxabl-project/oxabl/commit/c93aa742c3f49dc3b1621cb641d003860dfe0c83)), closes [#134](https://github.com/oxabl-project/oxabl/issues/134)
* **parser:** declare frame fields of FORM and DEFINE FRAME ([de9817d](https://github.com/oxabl-project/oxabl/commit/de9817d06e20866f0eff98c311ae92c24bba6d9e))
* **parser:** declare frame fields of UPDATE and SET ([ffb8263](https://github.com/oxabl-project/oxabl/commit/ffb82636fd6767d0b1864f48e09ee400396ea734))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **parser:** record USE-WIDGET-POOL on a class ([2b7fbbf](https://github.com/oxabl-project/oxabl/commit/2b7fbbf00dc2cda3a729f7d542c71a7528969490))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **parser:** parse the PACKAGE-PROTECTED access modifier ([95d7b23](https://github.com/oxabl-project/oxabl/commit/95d7b23d30f45c079e1801b5e178100fd13a5e02))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **semantic:** give record-only schema table references an edge ([3f8e090](https://github.com/oxabl-project/oxabl/commit/3f8e090ec4bd58fd724f4e2fb334a693207f401f)), closes [#223](https://github.com/oxabl-project/oxabl/issues/223)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))
</details>

<details><summary>oxabl_codegen: 0.6.0</summary>

## [0.6.0](https://github.com/oxabl-project/oxabl/compare/oxabl_codegen-v0.5.1...oxabl_codegen-v0.6.0) (2026-10-16)


### Features

* **codegen:** merge the Progress.* built-in classes into the index ([2da1d7f](https://github.com/oxabl-project/oxabl/commit/2da1d7f3357db383019f84ddbc164743e2e75153)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* **codegen:** scrape the Progress built-in class index from the ABL API docs ([f7cbbdf](https://github.com/oxabl-project/oxabl/commit/f7cbbdf78e9fb16df886ff060b3e4666e6c82633)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* **semantic:** recognize documented OpenEdge built-in types ([a528bbc](https://github.com/oxabl-project/oxabl/commit/a528bbc0f7953dcc355a88c91e2a5438099f1d37)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)


### Bug Fixes

* **codegen:** decode only a trailing generic in built-in class page names ([307ac14](https://github.com/oxabl-project/oxabl/commit/307ac14fb2aa1b7b6293a7e7afd40150a2427af1)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lexer:** register OVERLAY, OS-DIR, NEW and the byte-order constants as built-ins ([e6552d6](https://github.com/oxabl-project/oxabl/commit/e6552d646caf5666c746efbc7b68b5a2b4e34eec)), closes [#183](https://github.com/oxabl-project/oxabl/issues/183)
* **lexer:** register UPPER, LOWER, SET-SIZE, PUT-* and WIDGET-H* as built-in functions ([5250ddf](https://github.com/oxabl-project/oxabl/commit/5250ddf8acd7de9bd328ffac2a7f04063e425a15))
* **parser:** parse the SEEK statement and register USER as the USERID abbreviation ([73fb2f9](https://github.com/oxabl-project/oxabl/commit/73fb2f99979f98bcf25d7362b82f72683fd400e7))
* **semantic:** read ON ... OF targets and register missing built-ins ([a5899ba](https://github.com/oxabl-project/oxabl/commit/a5899ba049ea4d16c57feaf21f8f709996c8b050))


### Performance Improvements

* **lexer:** narrow keyword dispatch and scan ASCII runs ([78803ae](https://github.com/oxabl-project/oxabl/commit/78803ae6c77cf5229387674ad8f5f765e9c53295))
</details>

<details><summary>oxabl_common: 1.2.1</summary>

## [1.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.2.0...oxabl_common-v1.2.1) (2026-10-16)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_daemon: 0.5.0</summary>

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.4.0...oxabl_daemon-v0.5.0) (2026-10-16)


###   BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **daemon:** `ReverseGraph::build_with` takes the file list, a job count, a factory for per-worker run handles and an `Fn` inspect that returns a value, and returns the graph together with the inspected values. `ReverseGraph` now derives `Debug` and `PartialEq`.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **index:** SourcesConfig and PipelineConfig gain a pub case_insensitive_lookup field.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **index:** add opt-in case-insensitive class and program lookup ([6b14dc8](https://github.com/oxabl-project/oxabl/commit/6b14dc808ed1118c3634eee3a59cd47f6f44afe9)), closes [#214](https://github.com/oxabl-project/oxabl/issues/214)
* **index:** answer parameter lists and workspace procedure names ([b76cbec](https://github.com/oxabl-project/oxabl/commit/b76cbec66a9e4d2e41c275bfd03d442184e32f5b))
* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **lsp:** code lens for database effect summaries ([530aa7b](https://github.com/oxabl-project/oxabl/commit/530aa7b1cd76e46d4b25e3e4db6082f299bd99ec))
* **lsp:** show cached database effect summaries as code lenses ([2924b89](https://github.com/oxabl-project/oxabl/commit/2924b89d1fffc950307c8b3453fce42c8ee7c80c))
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **check:** scan the SHARED producer map on every core ([fa951a2](https://github.com/oxabl-project/oxabl/commit/fa951a247a9754d992970c543437ba5d0d159b7d))
* **daemon:** build the cold workspace pass on every core ([4efe428](https://github.com/oxabl-project/oxabl/commit/4efe428e7774965b106413dc90cf48a6ebc0dd9c)), closes [#170](https://github.com/oxabl-project/oxabl/issues/170)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_effects: 0.2.0</summary>

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.1.0...oxabl_effects-v0.2.0) (2026-10-16)


###   BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** add oxabl_effects, the database effects and likely index per unit ([0d11099](https://github.com/oxabl-project/oxabl/commit/0d11099ad81f0c980040595c0c169547c1def77a))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
</details>

<details><summary>oxabl_flow: 0.3.0</summary>

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_flow-v0.2.0...oxabl_flow-v0.3.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* **flow:** end the dataflow solve at loop heads and bound its work ([cecbfb1](https://github.com/oxabl-project/oxabl/commit/cecbfb1a2aeddc68dbec1ce2a72ac652ed141b3c))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))


### Performance Improvements

* **flow:** charge dataflow visits by the work of their fact ([65fdbe4](https://github.com/oxabl-project/oxabl/commit/65fdbe4537a8bfc7ef08e9e5796371a3bb64779d))
* **flow:** read block headers from the parser's tokens ([a1a0f78](https://github.com/oxabl-project/oxabl/commit/a1a0f78a8215fd984637c916ea2b939c2f87822d))
* **nulls:** bound the work of the null analyses ([edbd5cb](https://github.com/oxabl-project/oxabl/commit/edbd5cb128509b93fd3f51dee3a939430055b3b2))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_formatter: 2.0.0</summary>

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_formatter-v1.0.3...oxabl_formatter-v2.0.0) (2026-10-16)


###   BREAKING CHANGES

* **lexer:** `oxabl_lexer::Kind` gains File, Total, Count, Average, Maximum, Minimum, SubTotal, SubCount, SubAverage, SubMaximum and SubMinimum; these words no longer lex as `Kind::Identifier`.

### Features

* **lexer:** lex FILE and the aggregate words as keywords ([8d3cde1](https://github.com/oxabl-project/oxabl/commit/8d3cde15d110bcc094910cdbc93c9e6e03e6e338))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* **formatter:** keep the spelling of PRINTER and the new format-option words used as names ([f431429](https://github.com/oxabl-project/oxabl/commit/f4314294bd515df4ffdd609e0ba251d04292fd4e))
* **formatter:** keep unreserved class names and recase every other keyword as before ([694ed0f](https://github.com/oxabl-project/oxabl/commit/694ed0fdcff5bfd777655998d6aaaaac180992e7))
* **formatter:** read IMPLEMENTS lists in one pass ([67949ec](https://github.com/oxabl-project/oxabl/commit/67949ec57142dcfab11aaed7fcc65fc0addd50f3))
* **formatter:** recase an unreserved keyword only where it is a keyword ([72753e0](https://github.com/oxabl-project/oxabl/commit/72753e05718f9b3a0b97a91a79ab7db1d712f880))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **parser:** keep parsing frame items after layout options ([ed6761d](https://github.com/oxabl-project/oxabl/commit/ed6761d1e56493ebe696fed4c83030c0ce58af92))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_handles: 0.2.0</summary>

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_handles-v0.1.0...oxabl_handles-v0.2.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;

### Features

* **handles:** add oxabl_handles, lifetimes of dynamic objects per unit ([005ae67](https://github.com/oxabl-project/oxabl/commit/005ae67771f889bd80c71d268554ae43b16b3b3d))
* **handles:** read compound guards, failed NO-ERROR creates and class widget pools ([881727e](https://github.com/oxabl-project/oxabl/commit/881727ee87815748f33a87281d9d1ed56c9f4fac))
* **handles:** read the new create, delete, pool, release and apply forms ([a6b51d2](https://github.com/oxabl-project/oxabl/commit/a6b51d22b9c464baa0e3088a84e2ad5c623730ee))
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* **handles:** do not bind a call to a body whose parameter cannot take the argument ([e58e019](https://github.com/oxabl-project/oxabl/commit/e58e01916ecf0109884b25403d78c83374db01bc))
* **handles:** model calls, :HANDLE identity, unmapped frees, COPY-LOB, shadowed frees and a work budget ([05f23ef](https://github.com/oxabl-project/oxabl/commit/05f23ef729196a3175317ac9d3b89815c3042c03))
* **handles:** read only the callee bodies that run, and bound the read ([613fad3](https://github.com/oxabl-project/oxabl/commit/613fad34c004b5b5853a7d42470f6790a0026d1d))
* **handles:** read what a callee does with a handle, resolve methods by receiver ([9227fe2](https://github.com/oxabl-project/oxabl/commit/9227fe2c7ea3907a02116b1e8c4b8fe6004fa3d5))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))


### Performance Improvements

* **handles:** find a create's unit in the shared CFGs ([c1e5e0a](https://github.com/oxabl-project/oxabl/commit/c1e5e0a5decf8056308a6e58acda786db65982b9))
* **handles:** keep statement facts only for units that create something ([5634d47](https://github.com/oxabl-project/oxabl/commit/5634d47e5eed16f1d442eec5e3204a380237603c))
* **lint:** build the CFGs once for the scope and handle analyses ([b72e92a](https://github.com/oxabl-project/oxabl/commit/b72e92a3a3bab5b0f7fea6a9a1fcf2eeba79a1c7))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_index: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v2.0.0...oxabl_index-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **pipeline:** `ConditionalIncludeClosure` has a new `references` field and is now `#[non_exhaustive]`.
* **parser:** the exhaustive enum oxabl_lexer::Kind has new variants on this branch: Kind::RawTransfer here, and Kind::PackageProtected, Kind::Ordinal, Kind::Cdecl, Kind::Pascal and Kind::Stdcall from the PACKAGE-PROTECTED and EXTERNAL procedure option fixes.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **pipeline:** `PreprocessedFile` has a new pub field, `skipped_text`. `MAX_INCLUDE_DEPTH` is now public.
* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **index:** SourcesConfig and PipelineConfig gain a pub case_insensitive_lookup field.
* **preprocessor:** `PreprocessedFile` has a new pub field, `skipped_includes`.

### Features

* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **index:** add opt-in case-insensitive class and program lookup ([6b14dc8](https://github.com/oxabl-project/oxabl/commit/6b14dc808ed1118c3634eee3a59cd47f6f44afe9)), closes [#214](https://github.com/oxabl-project/oxabl/issues/214)
* **index:** add the static call graph ([8d74de5](https://github.com/oxabl-project/oxabl/commit/8d74de5757bd985b4901be013e0f91034bdc3f79))
* **index:** answer parameter lists and workspace procedure names ([b76cbec](https://github.com/oxabl-project/oxabl/commit/b76cbec66a9e4d2e41c275bfd03d442184e32f5b))
* **index:** find SHARED producers from a map that is built once ([d305b66](https://github.com/oxabl-project/oxabl/commit/d305b664d926772a697d9a2470fc3f24a3b506ce)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)
* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **pipeline:** list class and program names in not-taken &IF branches and the closure of conditional includes ([6b153d6](https://github.com/oxabl-project/oxabl/commit/6b153d631c4c6e4750fa13cc740f14cc9dd203ed))
* **pipeline:** list the class and program names a conditional include's target writes ([81462eb](https://github.com/oxabl-project/oxabl/commit/81462eb81a826051c2e285bf6f0d855f5dbd316c))
* **preprocessor:** carry include references in not-taken &IF branches ([b1057ac](https://github.com/oxabl-project/oxabl/commit/b1057ac88120afad1ce4d59acb2deb808a3fcc9e))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* **index:** answer SHARED producers independently of earlier files ([cee66f4](https://github.com/oxabl-project/oxabl/commit/cee66f4021eba6c82e1e40fea206a676cb7bb8f9)), closes [#224](https://github.com/oxabl-project/oxabl/issues/224)
* **index:** give a SHARED producer the same answer in every seeding mode ([d6bb24f](https://github.com/oxabl-project/oxabl/commit/d6bb24f5efb4ee08b583530b2d66834a17a5f3f6))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **parser,index:** read every RUN target path shape whole ([079e451](https://github.com/oxabl-project/oxabl/commit/079e451c22e1ee7d604f71452314dbcc45eef44e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* **parser:** give RAW-TRANSFER, UNDERLINE and CURRENT-CHANGED their table edge ([d9e83df](https://github.com/oxabl-project/oxabl/commit/d9e83df85b4930f54e3afbeeb15c3099e81fc7b9))
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **semantic:** give record-only schema table references an edge ([3f8e090](https://github.com/oxabl-project/oxabl/commit/3f8e090ec4bd58fd724f4e2fb334a693207f401f)), closes [#223](https://github.com/oxabl-project/oxabl/issues/223)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **check:** scan the SHARED producer map on every core ([fa951a2](https://github.com/oxabl-project/oxabl/commit/fa951a247a9754d992970c543437ba5d0d159b7d))
* **index:** reduce name interning and unrelated span mapping ([ce8616c](https://github.com/oxabl-project/oxabl/commit/ce8616c93eef5aa92c621ee18a848d79963251d9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_schema bumped from 0.3.1 to 0.4.0
</details>

<details><summary>oxabl_lexer: 2.0.0</summary>

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lexer-v1.0.2...oxabl_lexer-v2.0.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** oxabl_lexer::Kind adds Bgcolor, Column, Fgcolor, NoTabStop, Row, SizeChars and SizePixels.
* **lexer:** oxabl_lexer::Kind adds the Printer variant.
* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **lexer:** `oxabl_lexer::Kind` gains File, Total, Count, Average, Maximum, Minimum, SubTotal, SubCount, SubAverage, SubMaximum and SubMinimum; these words no longer lex as `Kind::Identifier`.
* **lexer:** `share`, `share-`, `share-l`, `share-lo` and `share-loc` now lex as `Kind::ShareLock`, not `Kind::Identifier`.
* **parser:** the exhaustive enum oxabl_lexer::Kind has new variants on this branch: Kind::RawTransfer here, and Kind::PackageProtected, Kind::Ordinal, Kind::Cdecl, Kind::Pascal and Kind::Stdcall from the PACKAGE-PROTECTED and EXTERNAL procedure option fixes.
* **lexer:** `Kind` has a new variant, `TypeOf`.
* **parser:** AccessModifier gains a PackageProtected variant.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and

### Features

* **lexer:** add the PRINTER keyword for OUTPUT TO PRINTER ([02425b3](https://github.com/oxabl-project/oxabl/commit/02425b3ed2446746afaca9c868c03ca84effea68))
* **lexer:** lex FILE and the aggregate words as keywords ([8d3cde1](https://github.com/oxabl-project/oxabl/commit/8d3cde15d110bcc094910cdbc93c9e6e03e6e338))
* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **lexer:** lex TYPE-OF as a built-in function keyword ([2bc10ca](https://github.com/oxabl-project/oxabl/commit/2bc10ca5121948713dce1a8bb2e8394def689b1a))
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **parser:** record USE-WIDGET-POOL on a class ([2b7fbbf](https://github.com/oxabl-project/oxabl/commit/2b7fbbf00dc2cda3a729f7d542c71a7528969490))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lexer:** lex SHARE as the abbreviation of SHARE-LOCK ([232929e](https://github.com/oxabl-project/oxabl/commit/232929e0e7d9a87e44f59500c317e8dcd89f9f64))
* **lexer:** register OVERLAY, OS-DIR, NEW and the byte-order constants as built-ins ([e6552d6](https://github.com/oxabl-project/oxabl/commit/e6552d646caf5666c746efbc7b68b5a2b4e34eec)), closes [#183](https://github.com/oxabl-project/oxabl/issues/183)
* **lexer:** register UPPER, LOWER, SET-SIZE, PUT-* and WIDGET-H* as built-in functions ([5250ddf](https://github.com/oxabl-project/oxabl/commit/5250ddf8acd7de9bd328ffac2a7f04063e425a15))
* **lexer:** stop printing to stdout on an overflowing number literal ([14d970e](https://github.com/oxabl-project/oxabl/commit/14d970efeacf9ac7b8112772348fbb00ff32f1ed))
* **parser:** accept EXTERNAL procedure options in any order ([41a4e99](https://github.com/oxabl-project/oxabl/commit/41a4e9941829e19f88e3374101dc1cb0af64690f))
* **parser:** give RAW-TRANSFER, UNDERLINE and CURRENT-CHANGED their table edge ([d9e83df](https://github.com/oxabl-project/oxabl/commit/d9e83df85b4930f54e3afbeeb15c3099e81fc7b9))
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* **parser:** keep parsing frame items after layout options ([ed6761d](https://github.com/oxabl-project/oxabl/commit/ed6761d1e56493ebe696fed4c83030c0ce58af92))
* **parser:** keep parsing frame items after VIEW-AS, SIZE, AT and colour options ([8839b62](https://github.com/oxabl-project/oxabl/commit/8839b623fe5f2e312ac6850e371b07cb88b51257))
* **parser:** parse the PACKAGE-PROTECTED access modifier ([95d7b23](https://github.com/oxabl-project/oxabl/commit/95d7b23d30f45c079e1801b5e178100fd13a5e02))
* **parser:** parse the SEEK statement and register USER as the USERID abbreviation ([73fb2f9](https://github.com/oxabl-project/oxabl/commit/73fb2f99979f98bcf25d7362b82f72683fd400e7))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **semantic:** read ON ... OF targets and register missing built-ins ([a5899ba](https://github.com/oxabl-project/oxabl/commit/a5899ba049ea4d16c57feaf21f8f709996c8b050))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **lexer:** intern literal slices without a temporary allocation ([640be41](https://github.com/oxabl-project/oxabl/commit/640be41aaa5d26d5b8f6fb198e052ea5c6ba0c23))
* **lexer:** narrow keyword dispatch and scan ASCII runs ([78803ae](https://github.com/oxabl-project/oxabl/commit/78803ae6c77cf5229387674ad8f5f765e9c53295))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.2.0 to 1.2.1
</details>

<details><summary>oxabl_lint: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v2.0.0...oxabl_lint-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind::Update and StatementKind::Set gain a fields field.
* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **parser:** StatementKind::Form and StatementKind::DefineFrame have a new `fields: Vec<FrameField>` field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **lexer:** `share`, `share-`, `share-l`, `share-lo` and `share-loc` now lex as `Kind::ShareLock`, not `Kind::Identifier`.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** Statement has a new public field, skipped_tail.
* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **lint:** `oxabl_analyze::CollectedDiagnostics` has a new public field, `include_findings`. Code that builds it with a struct literal must set it or use `..Default::default()`.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **semantic:** `SymbolKind` has a new `Sequence` variant.
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **lint:** add CL003 signature-mismatch and CL004 unresolved-call ([d43dd19](https://github.com/oxabl-project/oxabl/commit/d43dd1908b435043accbfda80847578f005d26e9))
* **lint:** add NL001 and NL002 unknown-value warnings ([17e0c6c](https://github.com/oxabl-project/oxabl/commit/17e0c6c0c6f94ba01729a4b3bf50b3380722cd28))
* **lint:** add NL004 nullable-db-field ([0ea9cd2](https://github.com/oxabl-project/oxabl/commit/0ea9cd24ad501d88c919c0ec4694a793b5443127))
* **lint:** add the strict.access rule DB001 find-in-loop ([df71f57](https://github.com/oxabl-project/oxabl/commit/df71f57b41299e34f0fd02a86733120a378de266))
* **lint:** add the strict.handles rules HD001-HD005 ([0dd3530](https://github.com/oxabl-project/oxabl/commit/0dd3530cbf90c1f073a5243dc501eb6f1a60b6b1))
* **lint:** add the strict.locks rules LK001-LK003 ([318f55f](https://github.com/oxabl-project/oxabl/commit/318f55fd9a7367bf78a6475fb75056bc494abc2f))
* **lint:** add the strict.nulls rule NL003 field-after-failed-find ([66c4579](https://github.com/oxabl-project/oxabl/commit/66c457953ffdb95eabc445fc5847debce0fa9cb8))
* **lint:** add the strict.transactions rules TX001-TX005 ([1253cc8](https://github.com/oxabl-project/oxabl/commit/1253cc83c520d4f321abafe9c7704b76641905bd))
* **lint:** report LINT0006 dead stores at every write form ([248b988](https://github.com/oxabl-project/oxabl/commit/248b988aedfa848e9de487d9e34fc826ca15e8e2)), closes [#131](https://github.com/oxabl-project/oxabl/issues/131)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **nulls:** treat database fields the schema starts at ? as sources of unknown values ([3b39aba](https://github.com/oxabl-project/oxabl/commit/3b39abaecb3b1477f1a2e26444511e27ca7c6ced))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** credit reads inside the skipped tails of modelled statements ([c93aa74](https://github.com/oxabl-project/oxabl/commit/c93aa742c3f49dc3b1621cb641d003860dfe0c83)), closes [#134](https://github.com/oxabl-project/oxabl/issues/134)
* **parser:** declare frame fields of FORM and DEFINE FRAME ([de9817d](https://github.com/oxabl-project/oxabl/commit/de9817d06e20866f0eff98c311ae92c24bba6d9e))
* **parser:** declare frame fields of UPDATE and SET ([ffb8263](https://github.com/oxabl-project/oxabl/commit/ffb82636fd6767d0b1864f48e09ee400396ea734))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **parser:** record USE-WIDGET-POOL on a class ([2b7fbbf](https://github.com/oxabl-project/oxabl/commit/2b7fbbf00dc2cda3a729f7d542c71a7528969490))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **semantic:** recognize documented OpenEdge built-in types ([a528bbc](https://github.com/oxabl-project/oxabl/commit/a528bbc0f7953dcc355a88c91e2a5438099f1d37)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* **semantic:** resolve abbreviated schema table names ([4a6a265](https://github.com/oxabl-project/oxabl/commit/4a6a26530ca683521f958d4270f0fe8eddd9e6eb))
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)


### Bug Fixes

* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* **handles:** model calls, :HANDLE identity, unmapped frees, COPY-LOB, shadowed frees and a work budget ([05f23ef](https://github.com/oxabl-project/oxabl/commit/05f23ef729196a3175317ac9d3b89815c3042c03))
* **handles:** read only the callee bodies that run, and bound the read ([613fad3](https://github.com/oxabl-project/oxabl/commit/613fad34c004b5b5853a7d42470f6790a0026d1d))
* **handles:** read what a callee does with a handle, resolve methods by receiver ([9227fe2](https://github.com/oxabl-project/oxabl/commit/9227fe2c7ea3907a02116b1e8c4b8fe6004fa3d5))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lexer:** lex SHARE as the abbreviation of SHARE-LOCK ([232929e](https://github.com/oxabl-project/oxabl/commit/232929e0e7d9a87e44f59500c317e8dcd89f9f64))
* **lint:** give LK001 a CAN-FIND message and help of its own ([be1ebfd](https://github.com/oxabl-project/oxabl/commit/be1ebfdc70fb8d77c8d68ef0d7f807c46811925c))
* **lint:** judge a scalar assigned to an EXTENT variable against the element type ([7358d88](https://github.com/oxabl-project/oxabl/commit/7358d889f6323c13fc40df9b37861c40e6e62938)), closes [#182](https://github.com/oxabl-project/oxabl/issues/182)
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** name TX005 in the LK002 help only when TX005 reports the raise ([024030a](https://github.com/oxabl-project/oxabl/commit/024030a0ea4f8406f105bb9c8016af8e4b513612))
* **lint:** read RUN ... ON SESSION as the local session in TX004 ([300b444](https://github.com/oxabl-project/oxabl/commit/300b4445f7886e067b9eda11d27bac06e5727b57))
* **lint:** report handle findings anchored in an include at the use site ([b414a76](https://github.com/oxabl-project/oxabl/commit/b414a76e4179e87c6b78a06d8fa732b883a631af))
* **lint:** report LINT0006 at a write inside CREATE ... TRIGGERS ([b8c3b9b](https://github.com/oxabl-project/oxabl/commit/b8c3b9b9ef84854624005f4ad0206e19f628a962))
* **lint:** say "(or 1 other)" in NL003 and use a field the test schema has ([303da06](https://github.com/oxabl-project/oxabl/commit/303da06f5a318379bf0d5b48b37da5cc54bae4a1))
* **lint:** say that NO-LOCK in a CAN-FIND may see an uncommitted change ([d0f846c](https://github.com/oxabl-project/oxabl/commit/d0f846c7840754a60ef67bc6e4aa48ac95df5d19))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **lint:** skip LINT0004 on an assignment that an empty reference joins ([8ef08c6](https://github.com/oxabl-project/oxabl/commit/8ef08c6a2b52e8a246f5b161fca2cc128c5d101b))
* **nulls:** forget locals that triggers or a REPEAT loop may write ([7d8a4c3](https://github.com/oxabl-project/oxabl/commit/7d8a4c34f7393b4061f8036a280d3a6f50db7bfa))
* **parser:** accept OVERRIDE in DEFINE PROPERTY ([91ca632](https://github.com/oxabl-project/oxabl/commit/91ca6324fcdb2d6ade0f63666fdaed82bbaf67cd))
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* **parser:** keep the database qualifier of a buffer or LIKE table ([2302914](https://github.com/oxabl-project/oxabl/commit/23029149faaa223c062e3ed411fb4bf084ff5d29))
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **parser:** model DEFINE WORKFILE/WORK-TABLE and the ACCUM aggregate phrase ([ad51cbc](https://github.com/oxabl-project/oxabl/commit/ad51cbc97efa8f68a6b6e0103f37e3a8977ffe7b)), closes [#159](https://github.com/oxabl-project/oxabl/issues/159) [#184](https://github.com/oxabl-project/oxabl/issues/184)
* **parser:** parse the CHOOSE statement as a skipped statement ([91bdc0b](https://github.com/oxabl-project/oxabl/commit/91bdc0b0d7f840597b5a1e56ba3c3488f4622524))
* **parser:** parse the SEEK statement and register USER as the USERID abbreviation ([73fb2f9](https://github.com/oxabl-project/oxabl/commit/73fb2f99979f98bcf25d7362b82f72683fd400e7))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** read database-qualified names in record statements ([6d8cbf1](https://github.com/oxabl-project/oxabl/commit/6d8cbf10c5e64de5963dde999b0727d24b41e697))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **scope:** count the screen form of ASSIGN and read written in execution order ([bf95fb7](https://github.com/oxabl-project/oxabl/commit/bf95fb795baf4e35355ee3b336d489ca6b5ca2bb))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** read a lock that unexpanded text may hide as Unknown ([ca9caae](https://github.com/oxabl-project/oxabl/commit/ca9caae9d8ed232f1a64271222b2fac34c58813e))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* **semantic:** bind FOR EACH to the buffer already in scope ([cf17969](https://github.com/oxabl-project/oxabl/commit/cf17969bf7c59e7a3cf995e62a4e88ba72c18640)), closes [#179](https://github.com/oxabl-project/oxabl/issues/179)
* **semantic:** credit an assignment through a pseudo-function to its operand ([c778506](https://github.com/oxabl-project/oxabl/commit/c7785061c695c5fece3dbeaa9cc916b06f0ce3e6)), closes [#156](https://github.com/oxabl-project/oxabl/issues/156)
* **semantic:** credit reads in arguments of a method call inside a member-access chain ([73ecd48](https://github.com/oxabl-project/oxabl/commit/73ecd48e9262f1832f9af2b23b126ce817f02903)), closes [#197](https://github.com/oxabl-project/oxabl/issues/197) [#157](https://github.com/oxabl-project/oxabl/issues/157)
* **semantic:** declare the NEW and OLD buffers of a TRIGGER PROCEDURE ([11c7d3a](https://github.com/oxabl-project/oxabl/commit/11c7d3aabbdb72f2a5769e8f6c37088c81e57507))
* **semantic:** declare TRIGGER PROCEDURE buffers only for a table that resolves ([0fb98cf](https://github.com/oxabl-project/oxabl/commit/0fb98cfa7b68ec041e16f2b174515bb276ffae3e))
* **semantic:** do not report a `..` RUN target as absent ([5c3074f](https://github.com/oxabl-project/oxabl/commit/5c3074f68c83213c99bc9c01e17d69e96d32687e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* **semantic:** read ON ... OF targets and register missing built-ins ([a5899ba](https://github.com/oxabl-project/oxabl/commit/a5899ba049ea4d16c57feaf21f8f709996c8b050))
* **semantic:** resolve stream operands and value-before-parenthesis items ([38bc1f1](https://github.com/oxabl-project/oxabl/commit/38bc1f18006d8ae27ff01e720f60dad033277a70))
* **semantic:** resolve the type operand of CAST, TYPE-OF and GET-CLASS as a type ([0e837c6](https://github.com/oxabl-project/oxabl/commit/0e837c6b614ad9a4efbdeaeac40551d9bc7bfb7f)), closes [#155](https://github.com/oxabl-project/oxabl/issues/155)
* **semantic:** resolve unqualified fields against temp-tables in scope ([337f1ac](https://github.com/oxabl-project/oxabl/commit/337f1acc079d6dadda328a28eac683a3094e714b)), closes [#160](https://github.com/oxabl-project/oxabl/issues/160)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **flow:** read block headers from the parser's tokens ([a1a0f78](https://github.com/oxabl-project/oxabl/commit/a1a0f78a8215fd984637c916ea2b939c2f87822d))
* **lint:** assert bench fixtures parse and trigger their target rules ([2edbb48](https://github.com/oxabl-project/oxabl/commit/2edbb48766ff118b6ae779cabfca170acc33807b)), closes [#132](https://github.com/oxabl-project/oxabl/issues/132)
* **lint:** benchmark lint_file and the registry on realistic fixtures ([ae504b1](https://github.com/oxabl-project/oxabl/commit/ae504b1ef57a955992cd513ea2f70638d85f6b37)), closes [#132](https://github.com/oxabl-project/oxabl/issues/132)
* **lint:** build the CFGs once for the scope and handle analyses ([b72e92a](https://github.com/oxabl-project/oxabl/commit/b72e92a3a3bab5b0f7fea6a9a1fcf2eeba79a1c7))
* **lint:** decide on sharing the CFGs at the first handles rule ([eb834d5](https://github.com/oxabl-project/oxabl/commit/eb834d5cae673e2dfc5ae0b162df818bae810098))
* **lint:** find the skipped statements once per file in LK002 ([ada4a20](https://github.com/oxabl-project/oxabl/commit/ada4a205d218e08572e610f3f2adef5202a903a3))
* **lint:** give the realistic bench the parsed tokens, as check does ([ff1b2ff](https://github.com/oxabl-project/oxabl/commit/ff1b2ff3641194928406d7ea7526db2032ae2eb0))
* **lint:** lex a TX003 or TX004 statement only inside a transaction ([bde529f](https://github.com/oxabl-project/oxabl/commit/bde529f86d8f95518e72e7636355fd1489340b29))
* **lint:** pass the parsed tokens to the scope analysis ([901b4a0](https://github.com/oxabl-project/oxabl/commit/901b4a04bcecd4563db55ce99fcaee9ad2d57c02))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_effects bumped from 0.1.0 to 0.2.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_handles bumped from 0.1.0 to 0.2.0
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_nulls bumped from 0.1.0 to 0.2.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_lsp: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v2.0.0...oxabl_lsp-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **analyze:** `oxabl_analyze::IncludeFinding` and `oxabl_analyze::CollectedDiagnostics` are `#[non_exhaustive]`. Code outside the crate can no longer build them with a struct expression or match them without `..`. Use `CollectedDiagnostics::new`; `IncludeFinding` is produced by the crate only.

### Features

* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lsp:** code lens for database effect summaries ([530aa7b](https://github.com/oxabl-project/oxabl/commit/530aa7b1cd76e46d4b25e3e4db6082f299bd99ec))
* **lsp:** show cached database effect summaries as code lenses ([2924b89](https://github.com/oxabl-project/oxabl/commit/2924b89d1fffc950307c8b3453fce42c8ee7c80c))


### Bug Fixes

* **analyze:** mark IncludeFinding and CollectedDiagnostics non-exhaustive ([40a0bae](https://github.com/oxabl-project/oxabl/commit/40a0bae77f1a1c6a6cd4a0886522d719b0076b7a))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_daemon bumped from 0.4.0 to 0.5.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.4.0 to 0.5.0
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_nulls: 0.2.0</summary>

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_nulls-v0.1.0...oxabl_nulls-v0.2.0) (2026-10-16)


### Features

* **nulls:** add oxabl_nulls, the records a failed FIND ... NO-ERROR may leave missing ([a6400ca](https://github.com/oxabl-project/oxabl/commit/a6400ca553a01d5adb1370702a6c7e1b44ea5283))
* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **nulls:** track local unknown values through unit control flow ([3d39621](https://github.com/oxabl-project/oxabl/commit/3d396219ab936312b1e4ca6743847f27a5f0312d))
* **nulls:** treat database fields the schema starts at ? as sources of unknown values ([3b39aba](https://github.com/oxabl-project/oxabl/commit/3b39abaecb3b1477f1a2e26444511e27ca7c6ced))


### Bug Fixes

* **nulls:** a counted loop with constant bounds that run its body exits after an iteration ([356d862](https://github.com/oxabl-project/oxabl/commit/356d862d475d017bb0a4c23536d73c6534d8b68a))
* **nulls:** a literal zero divisor is Unknown, not a source of ? ([f9e4bbf](https://github.com/oxabl-project/oxabl/commit/f9e4bbf36dbdf4807ba9b900a2fece1b98940085))
* **nulls:** forget locals that triggers or a REPEAT loop may write ([7d8a4c3](https://github.com/oxabl-project/oxabl/commit/7d8a4c34f7393b4061f8036a280d3a6f50db7bfa))
* **nulls:** frame fields, per-site dependent finds and unreadable ERROR-STATUS tests ([16b5ce2](https://github.com/oxabl-project/oxabl/commit/16b5ce2abb02fbe2d39ff828822e7949f0422f35))
* **nulls:** make the value transfers monotone and bound both solves ([2268fc1](https://github.com/oxabl-project/oxabl/commit/2268fc18490b07735d6a5004eedf0d5ff4ade8da))
* **nulls:** the OF list of an ON header names widgets, not record reads ([1966e24](https://github.com/oxabl-project/oxabl/commit/1966e242ec18d901da8793495a46bc4a344ab304))
* **scope:** mark a reference in an unknown statement form as opaque for temp-tables too ([d911d47](https://github.com/oxabl-project/oxabl/commit/d911d473d4d41007f4e292cdf597e44c49b1fe59))


### Performance Improvements

* **nulls:** bound the work of the null analyses ([edbd5cb](https://github.com/oxabl-project/oxabl/commit/edbd5cb128509b93fd3f51dee3a939430055b3b2))
* **nulls:** bound the work of the null analyses and report units that fall back ([5cebc88](https://github.com/oxabl-project/oxabl/commit/5cebc88ee58a024e00972a8990f1df7483dd53cc))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
</details>

<details><summary>oxabl_parser: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v2.0.0...oxabl_parser-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** oxabl_lexer::Kind adds Bgcolor, Column, Fgcolor, NoTabStop, Row, SizeChars and SizePixels.
* **lexer:** oxabl_lexer::Kind adds the Printer variant.
* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind::Update and StatementKind::Set gain a fields field.
* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **parser:** StatementKind::Form and StatementKind::DefineFrame have a new `fields: Vec<FrameField>` field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **lexer:** `oxabl_lexer::Kind` gains File, Total, Count, Average, Maximum, Minimum, SubTotal, SubCount, SubAverage, SubMaximum and SubMinimum; these words no longer lex as `Kind::Identifier`.
* **parser:** the exhaustive enum oxabl_lexer::Kind has new variants on this branch: Kind::RawTransfer here, and Kind::PackageProtected, Kind::Ordinal, Kind::Cdecl, Kind::Pascal and Kind::Stdcall from the PACKAGE-PROTECTED and EXTERNAL procedure option fixes.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** Statement has a new public field, skipped_tail.
* **parser:** AccessModifier gains a PackageProtected variant.
* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **lexer:** add the PRINTER keyword for OUTPUT TO PRINTER ([02425b3](https://github.com/oxabl-project/oxabl/commit/02425b3ed2446746afaca9c868c03ca84effea68))
* **lexer:** lex FILE and the aggregate words as keywords ([8d3cde1](https://github.com/oxabl-project/oxabl/commit/8d3cde15d110bcc094910cdbc93c9e6e03e6e338))
* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** credit reads inside the skipped tails of modelled statements ([c93aa74](https://github.com/oxabl-project/oxabl/commit/c93aa742c3f49dc3b1621cb641d003860dfe0c83)), closes [#134](https://github.com/oxabl-project/oxabl/issues/134)
* **parser:** declare frame fields of FORM and DEFINE FRAME ([de9817d](https://github.com/oxabl-project/oxabl/commit/de9817d06e20866f0eff98c311ae92c24bba6d9e))
* **parser:** declare frame fields of UPDATE and SET ([ffb8263](https://github.com/oxabl-project/oxabl/commit/ffb82636fd6767d0b1864f48e09ee400396ea734))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **parser:** record USE-WIDGET-POOL on a class ([2b7fbbf](https://github.com/oxabl-project/oxabl/commit/2b7fbbf00dc2cda3a729f7d542c71a7528969490))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **parser,index:** read every RUN target path shape whole ([079e451](https://github.com/oxabl-project/oxabl/commit/079e451c22e1ee7d604f71452314dbcc45eef44e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* **parser:** accept any word written against a colon as a member name ([33dad7c](https://github.com/oxabl-project/oxabl/commit/33dad7cb079ded432cfd46448912565cce550c79))
* **parser:** accept EXTERNAL procedure options in any order ([41a4e99](https://github.com/oxabl-project/oxabl/commit/41a4e9941829e19f88e3374101dc1cb0af64690f))
* **parser:** accept GET-CODEPAGE(s), :NULL member and SERIALIZABLE class option ([90592e0](https://github.com/oxabl-project/oxabl/commit/90592e0189746319831386e1ec3716ffee7ecea0)), closes [#198](https://github.com/oxabl-project/oxabl/issues/198) [#192](https://github.com/oxabl-project/oxabl/issues/192) [#191](https://github.com/oxabl-project/oxabl/issues/191)
* **parser:** accept OVERRIDE in DEFINE PROPERTY ([91ca632](https://github.com/oxabl-project/oxabl/commit/91ca6324fcdb2d6ade0f63666fdaed82bbaf67cd))
* **parser:** accept OVERRIDE on either side of ABSTRACT in DEFINE PROPERTY ([ed32530](https://github.com/oxabl-project/oxabl/commit/ed32530cb96c71e16b0cf7602c015a8b9f3ec335))
* **parser:** accept TRANSACTION after the ON phrases of a DO block ([4af5116](https://github.com/oxabl-project/oxabl/commit/4af5116cfabd31925534db943e4414fd3a35ce1c))
* **parser:** accept unreserved type keywords as class names ([f8f1b44](https://github.com/oxabl-project/oxabl/commit/f8f1b442cc483f3ed3da37bf944dc2d05ba21814))
* **parser:** drop a dynamic buffer field from the unmodelled harvest ([e780737](https://github.com/oxabl-project/oxabl/commit/e780737730eac9f2804b76180d890f3689fd1bb9)), closes [#223](https://github.com/oxabl-project/oxabl/issues/223)
* **parser:** end a block header at a keyword against its colon ([000e6c2](https://github.com/oxabl-project/oxabl/commit/000e6c26ecaa9ceaf68e28f5fdd0c723a29438ed))
* **parser:** give RAW-TRANSFER, UNDERLINE and CURRENT-CHANGED their table edge ([d9e83df](https://github.com/oxabl-project/oxabl/commit/d9e83df85b4930f54e3afbeeb15c3099e81fc7b9))
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* **parser:** keep parsing frame items after layout options ([ed6761d](https://github.com/oxabl-project/oxabl/commit/ed6761d1e56493ebe696fed4c83030c0ce58af92))
* **parser:** keep parsing frame items after VIEW-AS, SIZE, AT and colour options ([8839b62](https://github.com/oxabl-project/oxabl/commit/8839b623fe5f2e312ac6850e371b07cb88b51257))
* **parser:** keep the body of DO FOR and REPEAT FOR blocks ([57fc6d5](https://github.com/oxabl-project/oxabl/commit/57fc6d5d2371abfad6ea7c4562c3d055a4e053cf))
* **parser:** keep the database qualifier of a buffer or LIKE table ([2302914](https://github.com/oxabl-project/oxabl/commit/23029149faaa223c062e3ed411fb4bf084ff5d29))
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **parser:** keep the table a signature buffer parameter names ([0843777](https://github.com/oxabl-project/oxabl/commit/0843777db35aa2595dd20d5faeaef40448425c8a)), closes [#139](https://github.com/oxabl-project/oxabl/issues/139)
* **parser:** merge class header option loop ([6d432a5](https://github.com/oxabl-project/oxabl/commit/6d432a5bcd97f1fed2ee5a6d40187b328449f520)), closes [#191](https://github.com/oxabl-project/oxabl/issues/191)
* **parser:** model DEFINE WORKFILE/WORK-TABLE and the ACCUM aggregate phrase ([ad51cbc](https://github.com/oxabl-project/oxabl/commit/ad51cbc97efa8f68a6b6e0103f37e3a8977ffe7b)), closes [#159](https://github.com/oxabl-project/oxabl/issues/159) [#184](https://github.com/oxabl-project/oxabl/issues/184)
* **parser:** parse NEW(buffer) and every byte-order constant as an expression ([d23e55f](https://github.com/oxabl-project/oxabl/commit/d23e55f39a72975da0a58fdf05168ed86a33cd12)), closes [#183](https://github.com/oxabl-project/oxabl/issues/183)
* **parser:** parse the CHOOSE statement as a skipped statement ([91bdc0b](https://github.com/oxabl-project/oxabl/commit/91bdc0b0d7f840597b5a1e56ba3c3488f4622524))
* **parser:** parse the PACKAGE-PROTECTED access modifier ([95d7b23](https://github.com/oxabl-project/oxabl/commit/95d7b23d30f45c079e1801b5e178100fd13a5e02))
* **parser:** parse the SEEK statement and register USER as the USERID abbreviation ([73fb2f9](https://github.com/oxabl-project/oxabl/commit/73fb2f99979f98bcf25d7362b82f72683fd400e7))
* **parser:** read a receiverless call to a keyword-named method as a call ([c0c70b4](https://github.com/oxabl-project/oxabl/commit/c0c70b41d2a8035e8e59c68641a581b3360f6aa2))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** read database-qualified names in record statements ([6d8cbf1](https://github.com/oxabl-project/oxabl/commit/6d8cbf10c5e64de5963dde999b0727d24b41e697))
* **parser:** read database-qualified tables in buffer and temp-table definitions ([161028f](https://github.com/oxabl-project/oxabl/commit/161028f56ca62f531c7be020b2d6e9bc528f7cb5))
* **parser:** read past the end of the tokens as the EOF token ([4123f7d](https://github.com/oxabl-project/oxabl/commit/4123f7db57ce689cfa52c41cfec3acabbf79a8b1))
* **parser:** report a missing class name after NEW instead of panicking ([4feba22](https://github.com/oxabl-project/oxabl/commit/4feba2270d3ccd6216536d2c12ad04ef0ee63508))
* **parser:** stop a block header's ON phrase at the end of the file ([f1a7ac0](https://github.com/oxabl-project/oxabl/commit/f1a7ac030398a80308e3cd182b068bcd1fc4dfc4))
* **parser:** take WHILE, TRANSACTION and ON phrases of a DO in any order ([1e807b2](https://github.com/oxabl-project/oxabl/commit/1e807b209f74a0d0ab4380834d80fa48eb1a38e2))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **semantic:** do not report a `..` RUN target as absent ([5c3074f](https://github.com/oxabl-project/oxabl/commit/5c3074f68c83213c99bc9c01e17d69e96d32687e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **parser:** always inline the token cursor reads ([0e99def](https://github.com/oxabl-project/oxabl/commit/0e99def149861b9fd9c9d09530c128b9e17be8e9))
* **parser:** parse a single-name VAR without a declaration list ([c2d0166](https://github.com/oxabl-project/oxabl/commit/c2d0166043749f33332780e33f5d25a7aea844f1))
* **parser:** presize the top-level statement list ([49857f2](https://github.com/oxabl-project/oxabl/commit/49857f253310cfabbfd09e583ef19b352e3cc8ae))
* win back parser, resolve and preprocessor time ([07a6324](https://github.com/oxabl-project/oxabl/commit/07a63246bca88025c833fba6c96e3d003465a819))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
  * dev-dependencies
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_pipeline: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v2.0.0...oxabl_pipeline-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **daemon:** `ReverseGraph::build_with` takes the file list, a job count, a factory for per-worker run handles and an `Fn` inspect that returns a value, and returns the graph together with the inspected values. `ReverseGraph` now derives `Debug` and `PartialEq`.
* **pipeline:** `ConditionalIncludeClosure` has a new `references` field and is now `#[non_exhaustive]`.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **pipeline:** `PreprocessedFile` has a new pub field, `skipped_text`. `MAX_INCLUDE_DEPTH` is now public.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **pipeline:** CollectedDiagnostics has a new public field, include_origin, so code that builds it with a struct expression must set it.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **pipeline:** the edge_set_with callback receives a LintResult without lint findings or type-check diagnostics; edge_set runs no lint rules.
* **index:** SourcesConfig and PipelineConfig gain a pub case_insensitive_lookup field.
* **analyze:** `oxabl_analyze::IncludeFinding` and `oxabl_analyze::CollectedDiagnostics` are `#[non_exhaustive]`. Code outside the crate can no longer build them with a struct expression or match them without `..`. Use `CollectedDiagnostics::new`; `IncludeFinding` is produced by the crate only.
* **lint:** `oxabl_analyze::CollectedDiagnostics` has a new public field, `include_findings`. Code that builds it with a struct literal must set it or use `..Default::default()`.
* **preprocessor:** `PreprocessedFile` has a new pub field, `skipped_includes`.
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **index:** add opt-in case-insensitive class and program lookup ([6b14dc8](https://github.com/oxabl-project/oxabl/commit/6b14dc808ed1118c3634eee3a59cd47f6f44afe9)), closes [#214](https://github.com/oxabl-project/oxabl/issues/214)
* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lint:** add the strict.locks rules LK001-LK003 ([318f55f](https://github.com/oxabl-project/oxabl/commit/318f55fd9a7367bf78a6475fb75056bc494abc2f))
* **lsp:** code lens for database effect summaries ([530aa7b](https://github.com/oxabl-project/oxabl/commit/530aa7b1cd76e46d4b25e3e4db6082f299bd99ec))
* **lsp:** show cached database effect summaries as code lenses ([2924b89](https://github.com/oxabl-project/oxabl/commit/2924b89d1fffc950307c8b3453fce42c8ee7c80c))
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **pipeline:** list class and program names in not-taken &IF branches and the closure of conditional includes ([6b153d6](https://github.com/oxabl-project/oxabl/commit/6b153d631c4c6e4750fa13cc740f14cc9dd203ed))
* **pipeline:** list the class and program names a conditional include's target writes ([81462eb](https://github.com/oxabl-project/oxabl/commit/81462eb81a826051c2e285bf6f0d855f5dbd316c))
* **pipeline:** retain include-origin findings with their compilation context ([950e08b](https://github.com/oxabl-project/oxabl/commit/950e08bc61f631828eb57261cc26f00a9b2f166f))
* **preprocessor:** carry include references in not-taken &IF branches ([b1057ac](https://github.com/oxabl-project/oxabl/commit/b1057ac88120afad1ce4d59acb2deb808a3fcc9e))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* **analyze:** mark IncludeFinding and CollectedDiagnostics non-exhaustive ([40a0bae](https://github.com/oxabl-project/oxabl/commit/40a0bae77f1a1c6a6cd4a0886522d719b0076b7a))
* **index:** answer SHARED producers independently of earlier files ([cee66f4](https://github.com/oxabl-project/oxabl/commit/cee66f4021eba6c82e1e40fea206a676cb7bb8f9)), closes [#224](https://github.com/oxabl-project/oxabl/issues/224)
* **index:** give a SHARED producer the same answer in every seeding mode ([d6bb24f](https://github.com/oxabl-project/oxabl/commit/d6bb24f5efb4ee08b583530b2d66834a17a5f3f6))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** report handle findings anchored in an include at the use site ([b414a76](https://github.com/oxabl-project/oxabl/commit/b414a76e4179e87c6b78a06d8fa732b883a631af))
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* **semantic:** give record-only schema table references an edge ([3f8e090](https://github.com/oxabl-project/oxabl/commit/3f8e090ec4bd58fd724f4e2fb334a693207f401f)), closes [#223](https://github.com/oxabl-project/oxabl/issues/223)
* **semantic:** look up the class-shaped runs of a modelled statement's skipped tail ([0da4553](https://github.com/oxabl-project/oxabl/commit/0da45535c733292776ca1baf659831c4af962cd0))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))


### Performance Improvements

* **check:** scan the SHARED producer map on every core ([fa951a2](https://github.com/oxabl-project/oxabl/commit/fa951a247a9754d992970c543437ba5d0d159b7d))
* **daemon:** build the cold workspace pass on every core ([4efe428](https://github.com/oxabl-project/oxabl/commit/4efe428e7774965b106413dc90cf48a6ebc0dd9c)), closes [#170](https://github.com/oxabl-project/oxabl/issues/170)
* **pipeline:** build edge sets without lint rules or type checks ([bcf44d3](https://github.com/oxabl-project/oxabl/commit/bcf44d36ba12efa7fed1558bac1bb1bf4ddc3733)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.1.0 to 0.2.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_formatter bumped from 1.0.3 to 2.0.0
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_lint bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_schema bumped from 0.3.1 to 0.4.0
</details>

<details><summary>oxabl_preprocessor: 0.7.0</summary>

## [0.7.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.6.0...oxabl_preprocessor-v0.7.0) (2026-10-16)


###   BREAKING CHANGES

* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **pipeline:** `PreprocessedFile` has a new pub field, `skipped_text`. `MAX_INCLUDE_DEPTH` is now public.
* **preprocessor:** `PreprocessedFile` has a new pub field, `skipped_includes`.

### Features

* **pipeline:** list class and program names in not-taken &IF branches and the closure of conditional includes ([6b153d6](https://github.com/oxabl-project/oxabl/commit/6b153d631c4c6e4750fa13cc740f14cc9dd203ed))
* **preprocessor:** carry include references in not-taken &IF branches ([b1057ac](https://github.com/oxabl-project/oxabl/commit/b1057ac88120afad1ce4d59acb2deb808a3fcc9e))
* **preprocessor:** record the references the expansion replaced with no text ([a188a23](https://github.com/oxabl-project/oxabl/commit/a188a230ca9ec01c8c83c0b5b51561a24f50e0d2))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **preprocessor:** keep an unresolved include in list position as text ([42b6893](https://github.com/oxabl-project/oxabl/commit/42b68931041033364791e84887f1e4af0c69028f)), closes [#108](https://github.com/oxabl-project/oxabl/issues/108)
* **preprocessor:** read a doubled quote in an include argument as one quote ([2d80ed1](https://github.com/oxabl-project/oxabl/commit/2d80ed15241694a2e5bd7622b1432253ec4a5cd1))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))


### Performance Improvements

* **preprocessor:** step over plain text in one scan ([570c939](https://github.com/oxabl-project/oxabl/commit/570c93984d2536360bbaf5b94f38ee9026e7bcad))
* win back parser, resolve and preprocessor time ([07a6324](https://github.com/oxabl-project/oxabl/commit/07a63246bca88025c833fba6c96e3d003465a819))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_schema: 0.4.0</summary>

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.3.1...oxabl_schema-v0.4.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **schema:** `ParseOutcome` has a new public `sequences` field.

### Features

* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **schema:** parse ADD SEQUENCE and expose sequences on Schema ([b0555f9](https://github.com/oxabl-project/oxabl/commit/b0555f9c8a44b15865a2d5e0a3729ec1725c87be)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)
* **schema:** tell an unknown INITIAL from a quoted question mark ([b30c0e6](https://github.com/oxabl-project/oxabl/commit/b30c0e6815521936c2810f080720cccb82f87d0a))
* **semantic:** resolve abbreviated schema table names ([4a6a265](https://github.com/oxabl-project/oxabl/commit/4a6a26530ca683521f958d4270f0fe8eddd9e6eb))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_scope: 0.2.0</summary>

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_scope-v0.1.0...oxabl_scope-v0.2.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **lexer:** `oxabl_lexer::Kind` gains File, Total, Count, Average, Maximum, Minimum, SubTotal, SubCount, SubAverage, SubMaximum and SubMinimum; these words no longer lex as `Kind::Identifier`.

### Features

* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **lexer:** lex FILE and the aggregate words as keywords ([8d3cde1](https://github.com/oxabl-project/oxabl/commit/8d3cde15d110bcc094910cdbc93c9e6e03e6e338))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **scope:** add oxabl_scope, the compiler's buffer scopes, transaction blocks and locks per unit ([cebe727](https://github.com/oxabl-project/oxabl/commit/cebe727b779e8d0806aa41e60d2968f56ed0cca6))
* **scope:** check the scope engine against COMPILE ... LISTING output ([19f0ae2](https://github.com/oxabl-project/oxabl/commit/19f0ae2d57391ee48d79c747f1a7b81aa9cafe33))
* **scope:** expose database updates and analyse over a prebuilt flow graph ([773f731](https://github.com/oxabl-project/oxabl/commit/773f731782d3f0970557c4cab6a4c0d5af779a07))
* **scope:** expose the resolved buffer references, with whether each names a field ([d725929](https://github.com/oxabl-project/oxabl/commit/d72592926ffb170a1903abba7074563d199ea1f1))
* **scope:** record the table of each record read ([3339571](https://github.com/oxabl-project/oxabl/commit/333957195343ea7c7f693bc4fc5cdda97749a3d8))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **scope:** count the screen form of ASSIGN and read written in execution order ([bf95fb7](https://github.com/oxabl-project/oxabl/commit/bf95fb795baf4e35355ee3b336d489ca6b5ca2bb))
* **scope:** label an opaque or passing reference in another routine by what it is ([ef972fc](https://github.com/oxabl-project/oxabl/commit/ef972fcb3ebdab5c4bf7c53085d77d301770d8df))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** mark a reference in an unknown statement form as opaque for temp-tables too ([d911d47](https://github.com/oxabl-project/oxabl/commit/d911d473d4d41007f4e292cdf597e44c49b1fe59))
* **scope:** pair a listing path that starts with .. with the resolved path ([eb9cdcb](https://github.com/oxabl-project/oxabl/commit/eb9cdcbfaa31fe98adb812d2a4427e13b97ee9f7))
* **scope:** pair LISTING blocks by the end of the file path ([8965d2d](https://github.com/oxabl-project/oxabl/commit/8965d2d115bf1e3673a1e33c777184d3492ed78a))
* **scope:** read `+` as an operator in the screen form of ASSIGN ([8e04b5d](https://github.com/oxabl-project/oxabl/commit/8e04b5d0fd6881626cd063af8e8adb8e8d977cbb))
* **scope:** read a file name as no record reference ([0d36d78](https://github.com/oxabl-project/oxabl/commit/0d36d7881db1e0d6a05551e9a071fe2ea647b4fa))
* **scope:** read a lock that unexpanded text may hide as Unknown ([ca9caae](https://github.com/oxabl-project/oxabl/commit/ca9caae9d8ed232f1a64271222b2fac34c58813e))


### Performance Improvements

* **flow:** read block headers from the parser's tokens ([a1a0f78](https://github.com/oxabl-project/oxabl/commit/a1a0f78a8215fd984637c916ea2b939c2f87822d))
* **lint:** build the CFGs once for the scope and handle analyses ([b72e92a](https://github.com/oxabl-project/oxabl/commit/b72e92a3a3bab5b0f7fea6a9a1fcf2eeba79a1c7))
* **scope:** fold a reference's words only when it is kept ([45538a6](https://github.com/oxabl-project/oxabl/commit/45538a68193892eab2b917c7a0bc291f8e61e0d9))
* **scope:** index field names for the exact searches of a bare field ([a2f25e4](https://github.com/oxabl-project/oxabl/commit/a2f25e40d04f30eb39289b966aa1af79cd2d8aaa))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
</details>

<details><summary>oxabl_semantic: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v2.0.0...oxabl_semantic-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind::Update and StatementKind::Set gain a fields field.
* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **parser:** StatementKind::Form and StatementKind::DefineFrame have a new `fields: Vec<FrameField>` field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **semantic:** `AnalysisContext` has two new public fields, `tokens` and `elided`. Code that builds it with a struct literal must set them;
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** Statement has a new public field, skipped_tail.
* **parser:** AccessModifier gains a PackageProtected variant.
* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **semantic:** `SymbolKind` has a new `Sequence` variant.
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **lint:** report LINT0006 dead stores at every write form ([248b988](https://github.com/oxabl-project/oxabl/commit/248b988aedfa848e9de487d9e34fc826ca15e8e2)), closes [#131](https://github.com/oxabl-project/oxabl/issues/131)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** credit reads inside the skipped tails of modelled statements ([c93aa74](https://github.com/oxabl-project/oxabl/commit/c93aa742c3f49dc3b1621cb641d003860dfe0c83)), closes [#134](https://github.com/oxabl-project/oxabl/issues/134)
* **parser:** declare frame fields of FORM and DEFINE FRAME ([de9817d](https://github.com/oxabl-project/oxabl/commit/de9817d06e20866f0eff98c311ae92c24bba6d9e))
* **parser:** declare frame fields of UPDATE and SET ([ffb8263](https://github.com/oxabl-project/oxabl/commit/ffb82636fd6767d0b1864f48e09ee400396ea734))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **parser:** record USE-WIDGET-POOL on a class ([2b7fbbf](https://github.com/oxabl-project/oxabl/commit/2b7fbbf00dc2cda3a729f7d542c71a7528969490))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **semantic,analyze:** analyze a file without the type-check pass ([96c2aa4](https://github.com/oxabl-project/oxabl/commit/96c2aa48e3852106a09c0b73102b1f6a6d339de8)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)
* **semantic:** carry the tokens and the elided offsets in AnalysisContext ([a1b1c2c](https://github.com/oxabl-project/oxabl/commit/a1b1c2c83a5321136657740bfb23c965ca394f8b))
* **semantic:** read the parameter lists of procedures ([7d0296c](https://github.com/oxabl-project/oxabl/commit/7d0296cbabdfb5d4e65b75e4d913f8cbe3f3c455))
* **semantic:** recognize documented OpenEdge built-in types ([a528bbc](https://github.com/oxabl-project/oxabl/commit/a528bbc0f7953dcc355a88c91e2a5438099f1d37)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* **semantic:** resolve abbreviated schema table names ([4a6a265](https://github.com/oxabl-project/oxabl/commit/4a6a26530ca683521f958d4270f0fe8eddd9e6eb))
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)


### Bug Fixes

* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **parser:** keep the table a signature buffer parameter names ([0843777](https://github.com/oxabl-project/oxabl/commit/0843777db35aa2595dd20d5faeaef40448425c8a)), closes [#139](https://github.com/oxabl-project/oxabl/issues/139)
* **parser:** parse the PACKAGE-PROTECTED access modifier ([95d7b23](https://github.com/oxabl-project/oxabl/commit/95d7b23d30f45c079e1801b5e178100fd13a5e02))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* **semantic:** bind FOR EACH to the buffer already in scope ([cf17969](https://github.com/oxabl-project/oxabl/commit/cf17969bf7c59e7a3cf995e62a4e88ba72c18640)), closes [#179](https://github.com/oxabl-project/oxabl/issues/179)
* **semantic:** credit an assignment through a pseudo-function to its operand ([c778506](https://github.com/oxabl-project/oxabl/commit/c7785061c695c5fece3dbeaa9cc916b06f0ce3e6)), closes [#156](https://github.com/oxabl-project/oxabl/issues/156)
* **semantic:** credit reads in arguments of a method call inside a member-access chain ([73ecd48](https://github.com/oxabl-project/oxabl/commit/73ecd48e9262f1832f9af2b23b126ce817f02903)), closes [#197](https://github.com/oxabl-project/oxabl/issues/197) [#157](https://github.com/oxabl-project/oxabl/issues/157)
* **semantic:** declare the NEW and OLD buffers of a TRIGGER PROCEDURE ([11c7d3a](https://github.com/oxabl-project/oxabl/commit/11c7d3aabbdb72f2a5769e8f6c37088c81e57507))
* **semantic:** declare TRIGGER PROCEDURE buffers only for a table that resolves ([0fb98cf](https://github.com/oxabl-project/oxabl/commit/0fb98cfa7b68ec041e16f2b174515bb276ffae3e))
* **semantic:** do not flag a declaration repeated in sibling &IF arms as a duplicate ([55a80cd](https://github.com/oxabl-project/oxabl/commit/55a80cd6944b0ad805cb264886aabe5b683b1e8a)), closes [#180](https://github.com/oxabl-project/oxabl/issues/180)
* **semantic:** do not report a `..` RUN target as absent ([5c3074f](https://github.com/oxabl-project/oxabl/commit/5c3074f68c83213c99bc9c01e17d69e96d32687e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* **semantic:** give record-only schema table references an edge ([3f8e090](https://github.com/oxabl-project/oxabl/commit/3f8e090ec4bd58fd724f4e2fb334a693207f401f)), closes [#223](https://github.com/oxabl-project/oxabl/issues/223)
* **semantic:** look up the class-shaped runs of a modelled statement's skipped tail ([0da4553](https://github.com/oxabl-project/oxabl/commit/0da45535c733292776ca1baf659831c4af962cd0))
* **semantic:** read ON ... OF targets and register missing built-ins ([a5899ba](https://github.com/oxabl-project/oxabl/commit/a5899ba049ea4d16c57feaf21f8f709996c8b050))
* **semantic:** resolve stream operands and value-before-parenthesis items ([38bc1f1](https://github.com/oxabl-project/oxabl/commit/38bc1f18006d8ae27ff01e720f60dad033277a70))
* **semantic:** resolve the type operand of CAST, TYPE-OF and GET-CLASS as a type ([0e837c6](https://github.com/oxabl-project/oxabl/commit/0e837c6b614ad9a4efbdeaeac40551d9bc7bfb7f)), closes [#155](https://github.com/oxabl-project/oxabl/issues/155)
* **semantic:** resolve unqualified fields against temp-tables in scope ([337f1ac](https://github.com/oxabl-project/oxabl/commit/337f1acc079d6dadda328a28eac683a3094e714b)), closes [#160](https://github.com/oxabl-project/oxabl/issues/160)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **index:** reduce name interning and unrelated span mapping ([ce8616c](https://github.com/oxabl-project/oxabl/commit/ce8616c93eef5aa92c621ee18a848d79963251d9))
* **semantic:** fold short names inline and skip lexing empty record tails ([f734e57](https://github.com/oxabl-project/oxabl/commit/f734e57cb67a46e668e8758ccbf0905501d54977))
* **semantic:** size the side tables from the program's node count ([7f24230](https://github.com/oxabl-project/oxabl/commit/7f24230a70bddc01484931066effbded3bce7684))
* **semantic:** size the side tables from the program's node count ([4c53517](https://github.com/oxabl-project/oxabl/commit/4c53517be3000ab5a569fbda2e4004dd6487bf71))
* win back parser, resolve and preprocessor time ([07a6324](https://github.com/oxabl-project/oxabl/commit/07a63246bca88025c833fba6c96e3d003465a819))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
</details>

<details><summary>oxabl_wasm: 1.0.5</summary>

## [1.0.5](https://github.com/oxabl-project/oxabl/compare/oxabl_wasm-v1.0.4...oxabl_wasm-v1.0.5) (2026-10-16)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
</details>

<details><summary>oxabl_workspace: 3.0.0</summary>

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v2.0.0...oxabl_workspace-v3.0.0) (2026-10-16)


###   BREAKING CHANGES

* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** SourcesConfig and PipelineConfig gain a pub case_insensitive_lookup field.

### Features

* **index:** add opt-in case-insensitive class and program lookup ([6b14dc8](https://github.com/oxabl-project/oxabl/commit/6b14dc808ed1118c3634eee3a59cd47f6f44afe9)), closes [#214](https://github.com/oxabl-project/oxabl/issues/214)
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_ast bumped from 2.0.0 to 3.0.0
</details>

---
This PR was generated with [Release Please](https://github.com/googleapis/release-please). See [documentation](https://github.com/googleapis/release-please#release-please).