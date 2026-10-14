# Changelog

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
