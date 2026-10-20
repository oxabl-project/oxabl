# Changelog

## [3.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v3.0.1...oxabl_index-v3.1.0) (2026-10-20)


### Features

* **index:** let a client cap the threads the known-file scans use ([950eae5](https://github.com/oxabl-project/oxabl/commit/950eae56a6af3b43288c2e66efefbe41ba219def))
* **index:** name the view BatchIndex::searching_from returns ([ef19b8f](https://github.com/oxabl-project/oxabl/commit/ef19b8f0675260ea684f4ca5fb5a77e68d677811))


### Performance Improvements

* **index:** scan known files outside the memo lock, and CachedFileSystem fixes ([e506994](https://github.com/oxabl-project/oxabl/commit/e506994ce48ac4f6dc98644cf9046ebb27d3e439))
* **index:** scan the known files for SHARED producers outside the memo lock ([1967d4b](https://github.com/oxabl-project/oxabl/commit/1967d4bb6c0de58eb66407d3e6d7a851493be4f1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 3.0.1 to 3.0.2
    * oxabl_preprocessor bumped from 0.7.1 to 0.7.2
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_semantic bumped from 3.0.1 to 3.0.2
    * oxabl_workspace bumped from 3.1.0 to 3.2.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.1 to 0.4.2

## [3.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v3.0.0...oxabl_index-v3.0.1) (2026-10-20)


### Bug Fixes

* **index:** answer Unknowable after a known-file producer scan that panicked, and do not run it again ([d3b319e](https://github.com/oxabl-project/oxabl/commit/d3b319e7dcd6a0e8e04191ae56082d919b1fa9ef))
* **index:** mark the known-file producer scan done only once it completes ([ffbecd7](https://github.com/oxabl-project/oxabl/commit/ffbecd712f1f08ddaad0181b7ec93a05aad34d40))


### Performance Improvements

* **index:** index a reached file outside the memo lock, so threads can share one index ([aad45f9](https://github.com/oxabl-project/oxabl/commit/aad45f974618d8ea9e69b3aa2b4514b951e679b9))
* **index:** skip files that cannot say NEW SHARED, and join producer lists without a quadratic scan ([5f9b97d](https://github.com/oxabl-project/oxabl/commit/5f9b97d506deec1683f19878b53e66090ae90b60))
* **pipeline:** expand each conditional include target once per SharedRun ([97ab6e2](https://github.com/oxabl-project/oxabl/commit/97ab6e24ad000dbc5c9f6f2e987c73b2e9bc89ad))
* **pipeline:** share one index and include cache across a workspace walk's threads ([0776695](https://github.com/oxabl-project/oxabl/commit/0776695113f8869a459cf595a9ef000edc660711))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 3.0.0 to 3.0.1
    * oxabl_preprocessor bumped from 0.7.0 to 0.7.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_semantic bumped from 3.0.0 to 3.0.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.0 to 0.4.1

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v2.0.0...oxabl_index-v3.0.0) (2026-10-16)


### ⚠ BREAKING CHANGES

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

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v1.0.3...oxabl_index-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **parser:** DataType has a new variant, Generic, and ExpressionKind::New has a new field, type_arguments. Code that matches DataType exhaustively, or builds or matches New without `..`, must handle them.
* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.
* **index:** oxabl_index::DegradedReason, oxabl_index::Completeness and oxabl_preprocessor::TextLossKind are #[non_exhaustive]; an exhaustive match on any of them outside its crate no longer compiles.
* **index:** oxabl_index::EdgeInputs has a new field include_origins, and oxabl_analyze::ExpandedFile::unresolved_includes and unreadable_includes now also hold references reached only through the root's own {&var} substitutions, with the substitution as their site.
* **index:** oxabl_index::index_file takes a third argument, &PreprocessorConfig.
* **index:** EdgeInputs gains preprocessor_loss, UnresolvedIncludeInput gains unterminated, UnreadableIncludeInput gains include_site, and nested_unresolved_includes takes NestedUnresolvedIncludeInput. DegradedReason gains two variants.
* **workspace:** PipelineConfig gains the public run_extensions field.
* **index:** ClassKind gains the Enum variant.
* **index:** DependencyEdge, UnresolvedReference and EdgeInputs gain an include_site field; the analyze envelope's dependency rows gain include_site (dependencies section version 4).
* **index:** EdgeInputs gains fields for nested unresolved includes, unreadable includes and parse recovery; DependencyEdges carries a completeness signal.

### Features

* **index:** degrade the edge set on preprocessor loss and unmodelled statements ([5fd8261](https://github.com/oxabl-project/oxabl/commit/5fd8261475d3fa49af2d356ee3cbc69ce18ceec6))
* **index:** give every dependency edge an include-site origin ([89c08ac](https://github.com/oxabl-project/oxabl/commit/89c08ac453605a2d085d8dd3873675c90a8386a9)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **index:** mark the degradation and text-loss enums non-exhaustive ([6999df2](https://github.com/oxabl-project/oxabl/commit/6999df2dc5410ed79b6d92da6240c0d07e08f449))
* **index:** record unlinked RUN targets and the files behind unusable answers ([432a323](https://github.com/oxabl-project/oxabl/commit/432a3236ad2bc10887ff945c0b5d00ac26e3b8df)), closes [#212](https://github.com/oxabl-project/oxabl/issues/212) [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **index:** report every silent dependency outcome and a completeness signal ([ca77b1f](https://github.com/oxabl-project/oxabl/commit/ca77b1f84ad2a813716c1384bb34bc0a58c9d973))
* **parser:** keep the type arguments of a generic class name ([6f5e3cb](https://github.com/oxabl-project/oxabl/commit/6f5e3cb11f3e20f4fa2c89e191c6e18c247b4ab9))
* **workspace:** configure the extension order for an extensionless RUN ([77c1e5f](https://github.com/oxabl-project/oxabl/commit/77c1e5fdf2dfee4b9264122c8f838310a2af0bc1))


### Bug Fixes

* **index:** apply the configured predefined names to a file's own directives ([c925a40](https://github.com/oxabl-project/oxabl/commit/c925a40fb7ce99b4b309dd31ac69e13568704c0b))
* **index:** index ENUM declarations and in-file directive-built classes ([feb5ad0](https://github.com/oxabl-project/oxabl/commit/feb5ad055d32985787649b56a8eed0fd1142fd97))
* **index:** look for an include ahead of the class header statement ([b3a8053](https://github.com/oxabl-project/oxabl/commit/b3a8053378bb531e505b5d6d80d863eda4f84c2d))
* **index:** probe every spelling of a run extension and normalize hand-built lists ([94ec29a](https://github.com/oxabl-project/oxabl/commit/94ec29a841e850330176b05130b55b5e14c3d045))
* **index:** treat an include named through the root's own define as a direct include ([52f7734](https://github.com/oxabl-project/oxabl/commit/52f7734b971871884d32e2410e34ed9efa38c176))
* **semantic:** leave no row for a bare RUN name that no file supplies ([4aa0ab0](https://github.com/oxabl-project/oxabl/commit/4aa0ab005e7652db0bf15362d51a86cb9fcd52a1))
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))
* **semantic:** stop reporting classes that are not workspace classes ([d5a78e3](https://github.com/oxabl-project/oxabl/commit/d5a78e3b6f9c293699f7b8039107673cd1b417c9))
* **workspace:** validate run_extensions at config load ([df81dd9](https://github.com/oxabl-project/oxabl/commit/df81dd992aeb3a7bad07fadabdfb5ec46b5106e1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_preprocessor bumped from 0.5.2 to 0.6.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_schema bumped from 0.3.0 to 0.3.1

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v1.0.2...oxabl_index-v1.0.3) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.3 to 1.0.4
    * oxabl_semantic bumped from 1.1.0 to 1.1.1

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v1.0.1...oxabl_index-v1.0.2) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
  * dev-dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v1.0.0...oxabl_index-v1.0.1) (2026-09-21)


### Bug Fixes

* **daemon:** detect added files in freshness, and correct span extents and lookup cost ([#187](https://github.com/oxabl-project/oxabl/issues/187)) ([d50be6a](https://github.com/oxabl-project/oxabl/commit/d50be6a897742e622b0c4f281de4a6d1d9c2c355))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_schema bumped from 0.2.2 to 0.2.3

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_index-v0.1.2...oxabl_index-v1.0.0) (2026-08-14)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.

### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* **semantic:** resolve cross-file names, with the rules still held still ([#147](https://github.com/oxabl-project/oxabl/issues/147)) ([8873de9](https://github.com/oxabl-project/oxabl/commit/8873de9a13f5ef5c30d584f13e63e21cea6300e0))
* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_schema bumped from 0.2.1 to 0.2.2
