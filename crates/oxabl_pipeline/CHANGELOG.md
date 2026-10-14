# Changelog

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v1.2.1...oxabl_pipeline-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.
* **index:** oxabl_index::DegradedReason, oxabl_index::Completeness and oxabl_preprocessor::TextLossKind are #[non_exhaustive]; an exhaustive match on any of them outside its crate no longer compiles.
* **index:** oxabl_index::EdgeInputs has a new field include_origins, and oxabl_analyze::ExpandedFile::unresolved_includes and unreadable_includes now also hold references reached only through the root's own {&var} substitutions, with the substitution as their site.
* **index:** oxabl_index::index_file takes a third argument, &PreprocessorConfig.
* **index:** EdgeInputs gains preprocessor_loss, UnresolvedIncludeInput gains unterminated, UnreadableIncludeInput gains include_site, and nested_unresolved_includes takes NestedUnresolvedIncludeInput. DegradedReason gains two variants.
* **workspace:** PipelineConfig gains the public run_extensions field.
* **index:** ClassKind gains the Enum variant.
* **preprocessor:** PipelineConfig gains the public preprocessor field.
* **index:** DependencyEdge, UnresolvedReference and EdgeInputs gain an include_site field; the analyze envelope's dependency rows gain include_site (dependencies section version 4).
* **index:** EdgeInputs gains fields for nested unresolved includes, unreadable includes and parse recovery; DependencyEdges carries a completeness signal.

### Features

* **cli:** check context-free strict rules in the include files of a walked tree ([7b931f0](https://github.com/oxabl-project/oxabl/commit/7b931f0770f0149ff22c6f0c35658085682797c5))
* **index:** degrade the edge set on preprocessor loss and unmodelled statements ([5fd8261](https://github.com/oxabl-project/oxabl/commit/5fd8261475d3fa49af2d356ee3cbc69ce18ceec6))
* **index:** give every dependency edge an include-site origin ([89c08ac](https://github.com/oxabl-project/oxabl/commit/89c08ac453605a2d085d8dd3873675c90a8386a9)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **index:** mark the degradation and text-loss enums non-exhaustive ([6999df2](https://github.com/oxabl-project/oxabl/commit/6999df2dc5410ed79b6d92da6240c0d07e08f449))
* **index:** report every silent dependency outcome and a completeness signal ([ca77b1f](https://github.com/oxabl-project/oxabl/commit/ca77b1f84ad2a813716c1384bb34bc0a58c9d973))
* **lint:** register CL001, CL002, CL005 and TX006 in their strict groups ([714834e](https://github.com/oxabl-project/oxabl/commit/714834e13ebb313b70c94ca7dc1438215304e3c4))
* **pipeline:** apply lint overrides per file and warn on unknown names ([e65a26e](https://github.com/oxabl-project/oxabl/commit/e65a26e0e9e68a64895dfe032639c77cbb40f281))
* **pipeline:** name the connected databases after the schema files ([57841d4](https://github.com/oxabl-project/oxabl/commit/57841d4ec9e6ec978d63299fc28884d9e0ef1ed8))
* **preprocessor:** answer the AVM's predefined names ([d996c49](https://github.com/oxabl-project/oxabl/commit/d996c49d821b7e086a4e0dfb691962e3f4c6bd8f)), closes [#216](https://github.com/oxabl-project/oxabl/issues/216)
* **workspace:** configure the extension order for an extensionless RUN ([77c1e5f](https://github.com/oxabl-project/oxabl/commit/77c1e5fdf2dfee4b9264122c8f838310a2af0bc1))


### Bug Fixes

* **index:** apply the configured predefined names to a file's own directives ([c925a40](https://github.com/oxabl-project/oxabl/commit/c925a40fb7ce99b4b309dd31ac69e13568704c0b))
* **index:** index ENUM declarations and in-file directive-built classes ([feb5ad0](https://github.com/oxabl-project/oxabl/commit/feb5ad055d32985787649b56a8eed0fd1142fd97))
* **index:** look for an include ahead of the class header statement ([b3a8053](https://github.com/oxabl-project/oxabl/commit/b3a8053378bb531e505b5d6d80d863eda4f84c2d))
* **index:** probe every spelling of a run extension and normalize hand-built lists ([94ec29a](https://github.com/oxabl-project/oxabl/commit/94ec29a841e850330176b05130b55b5e14c3d045))
* **index:** treat an include named through the root's own define as a direct include ([52f7734](https://github.com/oxabl-project/oxabl/commit/52f7734b971871884d32e2410e34ed9efa38c176))
* **lint:** lower only the findings an includer can change ([4e29dbc](https://github.com/oxabl-project/oxabl/commit/4e29dbc0cf283ae44d61891eb1b44173b0ca53de))
* **lint:** lower the confidence of includer-dependent fragment findings ([4fcec91](https://github.com/oxabl-project/oxabl/commit/4fcec91f4528a01993297ec2456a7555bd969046))
* **lint:** make block-var-used-outside path-aware ([4bc6785](https://github.com/oxabl-project/oxabl/commit/4bc67857b18afb558a3e19d5fe9b09c0f774076a)), closes [#124](https://github.com/oxabl-project/oxabl/issues/124) [#126](https://github.com/oxabl-project/oxabl/issues/126)
* **lint:** rank group toggles below rule settings and warn on an override with no files ([e31d5f8](https://github.com/oxabl-project/oxabl/commit/e31d5f8295aebeb94fbaff09a1106f185b38ed73))
* **parser:** keep the expressions that carry class and program edges ([cf051a0](https://github.com/oxabl-project/oxabl/commit/cf051a05a9fbc4dd8aa380cefd9691f56ac5ac09))
* **parser:** stop an unclosed generic bracket at the statement end ([0b2d4c0](https://github.com/oxabl-project/oxabl/commit/0b2d4c05e1817bf04b68ddd776c1baf0d474bbb6))
* **pipeline:** keep the stem match for an unresolved row when the stem is shared ([4f58a88](https://github.com/oxabl-project/oxabl/commit/4f58a8857b2bdcf0f5a8c60c691fd46dffe9181e))
* **pipeline:** rebuild the files whose unresolved rows name the subject ([be5ba49](https://github.com/oxabl-project/oxabl/commit/be5ba49c27ca01cd68a81c9712ecacadf277b3f5))
* **preprocessor:** close the last silent branch choices in &IF evaluation ([5d9fc53](https://github.com/oxabl-project/oxabl/commit/5d9fc53be1ad38b960e3dbf06a070a4843cc154e))
* **preprocessor:** evaluate DBTYPE, CONNECTED and LDBNAME from the listed databases ([223560c](https://github.com/oxabl-project/oxabl/commit/223560c095e31cf45d2a44db8012a74e9fda1dfa))
* **preprocessor:** evaluate the functions an &IF names, or say it guessed ([026e29e](https://github.com/oxabl-project/oxabl/commit/026e29e4fc9fff19a17ace4b872f2210d15129da))
* **preprocessor:** expand include references with whitespace inside the braces ([82f05de](https://github.com/oxabl-project/oxabl/commit/82f05dee1506581e02016358d2a747b7a6563f90)), closes [#209](https://github.com/oxabl-project/oxabl/issues/209)
* **preprocessor:** read a define comment that spans lines to its close ([fb52456](https://github.com/oxabl-project/oxabl/commit/fb52456764aae62ee347732efc588e04f1a71653))
* **preprocessor:** settle an AND or OR chain by its result, not by operand order ([a8e20d0](https://github.com/oxabl-project/oxabl/commit/a8e20d0b66a94fade1c08c913d4727fff6093f34))
* **preprocessor:** treat a database list inferred from schema files as partial ([c26de87](https://github.com/oxabl-project/oxabl/commit/c26de8751a8a8af649c800503e79f9c8e8eb7da1))
* **semantic:** do not record RUN through a handle as a program reference ([3abc7f0](https://github.com/oxabl-project/oxabl/commit/3abc7f0769981d293042d4babc0c81e7785ee9fa)), closes [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **semantic:** keep missing-class rows under shipped parents and in widget statements ([e00a6d0](https://github.com/oxabl-project/oxabl/commit/e00a6d0ca6cb067fb6daa91509c6ef576ecbce5d))
* **semantic:** leave no row for a bare RUN name that no file supplies ([4aa0ab0](https://github.com/oxabl-project/oxabl/commit/4aa0ab005e7652db0bf15362d51a86cb9fcd52a1))
* **semantic:** let an explicit USING decide whether a missed class is shipped ([7241c73](https://github.com/oxabl-project/oxabl/commit/7241c7301e60c5f927722a7a6daff192206a90fd))
* **semantic:** let an unseen import outrank a shipped wildcard in a parent's header ([2996a8d](https://github.com/oxabl-project/oxabl/commit/2996a8d66c264aacd2bf36c2570cda0d8f5d1433))
* **semantic:** merge the lookups of two spellings of one class ([b949dab](https://github.com/oxabl-project/oxabl/commit/b949dabf0665b8c8d6dfbc1d5084713d0cd0c60f))
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))
* **semantic:** record the parameter types of a function prototype as class sites ([fc78d4c](https://github.com/oxabl-project/oxabl/commit/fc78d4cfe7b1d3b71e0a523c093412b9069b7e3f))
* **semantic:** record the type arguments of a generic class as class sites ([1b854fc](https://github.com/oxabl-project/oxabl/commit/1b854fccc8aaf532ef379f61b6fb3a41975153d6))
* **semantic:** report an unusable class at a naming site as an edge and a row ([658f2b2](https://github.com/oxabl-project/oxabl/commit/658f2b23429d1ab6f5bb07cbf828e4e18a8582e0)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **semantic:** stop reporting classes that are not workspace classes ([d5a78e3](https://github.com/oxabl-project/oxabl/commit/d5a78e3b6f9c293699f7b8039107673cd1b417c9))
* **workspace:** validate run_extensions at config load ([df81dd9](https://github.com/oxabl-project/oxabl/commit/df81dd992aeb3a7bad07fadabdfb5ec46b5106e1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_analyze bumped from 1.1.4 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_formatter bumped from 1.0.2 to 1.0.3
    * oxabl_index bumped from 1.0.3 to 2.0.0
    * oxabl_lint bumped from 1.1.1 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1

## [1.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v1.2.0...oxabl_pipeline-v1.2.1) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.3 to 1.1.4
    * oxabl_formatter bumped from 1.0.1 to 1.0.2
    * oxabl_index bumped from 1.0.2 to 1.0.3
    * oxabl_semantic bumped from 1.1.0 to 1.1.1
  * dev-dependencies
    * oxabl_parser bumped from 1.0.3 to 1.0.4

## [1.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v1.1.2...oxabl_pipeline-v1.2.0) (2026-09-21)


### Features

* **schema:** ship the OpenEdge metaschema so `_file` and friends resolve ([#189](https://github.com/oxabl-project/oxabl/issues/189)) ([0367b60](https://github.com/oxabl-project/oxabl/commit/0367b608273df8e20bb6c89495e27f23d8e50da2)), closes [#151](https://github.com/oxabl-project/oxabl/issues/151)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.2 to 1.1.3
    * oxabl_index bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
  * dev-dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.1.2](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v1.1.1...oxabl_pipeline-v1.1.2) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))
* **preprocessor:** report a nested unresolvable include instead of dropping it ([#177](https://github.com/oxabl-project/oxabl/issues/177)) ([676dafe](https://github.com/oxabl-project/oxabl/commit/676dafe4f3b5b3dc1d98ed43342033f567a7f778))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_formatter bumped from 1.0.0 to 1.0.1
    * oxabl_index bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_schema bumped from 0.2.2 to 0.2.3

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v1.1.0...oxabl_pipeline-v1.1.1) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_formatter bumped from 0.1.2 to 1.0.0
    * oxabl_index bumped from 0.1.2 to 1.0.0
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_style bumped from 0.1.0 to 0.2.0
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.1 to 0.2.2

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v1.0.0...oxabl_pipeline-v1.1.0) (2026-08-12)


### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))
* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.0.0 to 1.1.0
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_formatter bumped from 0.1.1 to 0.1.2
    * oxabl_index bumped from 0.1.1 to 0.1.2
    * oxabl_schema bumped from 0.2.0 to 0.2.1
    * oxabl_semantic bumped from 1.0.0 to 1.0.1
    * oxabl_workspace bumped from 1.0.0 to 1.0.1
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_schema bumped from 0.2.0 to 0.2.1

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_pipeline-v0.1.0...oxabl_pipeline-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.
* **pipeline:** `oxabl check` no longer reports parse conformance. It lints and reports formatting drift, and its --json shape has changed accordingly. The previous behaviour, report text, --json shape, and exit codes are available unchanged as `oxabl conformance`.

### Features

* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **pipeline:** one shared lint and format run behind every client ([#140](https://github.com/oxabl-project/oxabl/issues/140)) ([58d961e](https://github.com/oxabl-project/oxabl/commit/58d961e7d9b77e8887c78ee69580e60cc07f279f))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))


### Bug Fixes

* **pipeline:** unblock the release run by dropping the self dev-dependency ([#145](https://github.com/oxabl-project/oxabl/issues/145)) ([a6878dc](https://github.com/oxabl-project/oxabl/commit/a6878dc011cb3794a43c4e2ad93d5e200fea39bf))
* resolve PR [#140](https://github.com/oxabl-project/oxabl/issues/140) review findings ([#141](https://github.com/oxabl-project/oxabl/issues/141)) ([dd0849e](https://github.com/oxabl-project/oxabl/commit/dd0849ea72d38ee68c404af1d2414b093ef64459))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 0.1.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_formatter bumped from 0.1.0 to 0.1.1
    * oxabl_index bumped from 0.1.0 to 0.1.1
    * oxabl_schema bumped from 0.1.0 to 0.2.0
    * oxabl_semantic bumped from 0.1.0 to 1.0.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
  * dev-dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
