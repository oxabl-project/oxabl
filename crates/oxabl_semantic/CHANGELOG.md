# Changelog

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.1.1...oxabl_semantic-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **parser:** DataType has a new variant, Generic, and ExpressionKind::New has a new field, type_arguments. Code that matches DataType exhaustively, or builds or matches New without `..`, must handle them.
* **parser:** StatementKind::Function has a new field, prototype_parameters. Code that builds the variant or matches it without `..` must name it.
* **parser:** `StatementKind` gains an `Annotation` variant, so exhaustive matches over it need a new arm.
* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.
* **index:** EdgeInputs gains preprocessor_loss, UnresolvedIncludeInput gains unterminated, UnreadableIncludeInput gains include_site, and nested_unresolved_includes takes NestedUnresolvedIncludeInput. DegradedReason gains two variants.
* **index:** ClassKind gains the Enum variant.

### Features

* **index:** degrade the edge set on preprocessor loss and unmodelled statements ([5fd8261](https://github.com/oxabl-project/oxabl/commit/5fd8261475d3fa49af2d356ee3cbc69ce18ceec6))
* **index:** record unlinked RUN targets and the files behind unusable answers ([432a323](https://github.com/oxabl-project/oxabl/commit/432a3236ad2bc10887ff945c0b5d00ac26e3b8df)), closes [#212](https://github.com/oxabl-project/oxabl/issues/212) [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **parser:** keep the parameters of a function prototype ([038c3ff](https://github.com/oxabl-project/oxabl/commit/038c3ff21b19654b47dcb17f5e153d3a42a58e80))
* **parser:** keep the type arguments of a generic class name ([6f5e3cb](https://github.com/oxabl-project/oxabl/commit/6f5e3cb11f3e20f4fa2c89e191c6e18c247b4ab9))
* **parser:** parse ABL annotations as a statement ([0de3bb6](https://github.com/oxabl-project/oxabl/commit/0de3bb6408e74e1bdbc729288713880d2d23bc90))


### Bug Fixes

* **index:** index ENUM declarations and in-file directive-built classes ([feb5ad0](https://github.com/oxabl-project/oxabl/commit/feb5ad055d32985787649b56a8eed0fd1142fd97))
* **semantic:** do not record RUN through a handle as a program reference ([3abc7f0](https://github.com/oxabl-project/oxabl/commit/3abc7f0769981d293042d4babc0c81e7785ee9fa)), closes [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **semantic:** keep missing-class rows under shipped parents and in widget statements ([e00a6d0](https://github.com/oxabl-project/oxabl/commit/e00a6d0ca6cb067fb6daa91509c6ef576ecbce5d))
* **semantic:** leave no row for a bare RUN name that no file supplies ([4aa0ab0](https://github.com/oxabl-project/oxabl/commit/4aa0ab005e7652db0bf15362d51a86cb9fcd52a1))
* **semantic:** let an explicit USING decide whether a missed class is shipped ([7241c73](https://github.com/oxabl-project/oxabl/commit/7241c7301e60c5f927722a7a6daff192206a90fd))
* **semantic:** let an unseen import outrank a shipped wildcard in a parent's header ([2996a8d](https://github.com/oxabl-project/oxabl/commit/2996a8d66c264aacd2bf36c2570cda0d8f5d1433))
* **semantic:** merge the lookups of two spellings of one class ([b949dab](https://github.com/oxabl-project/oxabl/commit/b949dabf0665b8c8d6dfbc1d5084713d0cd0c60f))
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))
* **semantic:** record a class lookup at every site that names a class ([7221537](https://github.com/oxabl-project/oxabl/commit/722153782e3960231a92fc939b9c17e6a35f0d43)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **semantic:** record the parameter types of a function prototype as class sites ([fc78d4c](https://github.com/oxabl-project/oxabl/commit/fc78d4cfe7b1d3b71e0a523c093412b9069b7e3f))
* **semantic:** record the type arguments of a generic class as class sites ([1b854fc](https://github.com/oxabl-project/oxabl/commit/1b854fccc8aaf532ef379f61b6fb3a41975153d6))
* **semantic:** report an unusable class at a naming site as an edge and a row ([658f2b2](https://github.com/oxabl-project/oxabl/commit/658f2b23429d1ab6f5bb07cbf828e4e18a8582e0)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **semantic:** stop reporting classes that are not workspace classes ([d5a78e3](https://github.com/oxabl-project/oxabl/commit/d5a78e3b6f9c293699f7b8039107673cd1b417c9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.3.0 to 0.3.1
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.1.0...oxabl_semantic-v1.1.1) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dev-dependencies
    * oxabl_parser bumped from 1.0.3 to 1.0.4

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.3...oxabl_semantic-v1.1.0) (2026-09-21)


### Features

* **schema:** ship the OpenEdge metaschema so `_file` and friends resolve ([#189](https://github.com/oxabl-project/oxabl/issues/189)) ([0367b60](https://github.com/oxabl-project/oxabl/commit/0367b608273df8e20bb6c89495e27f23d8e50da2)), closes [#151](https://github.com/oxabl-project/oxabl/issues/151)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0
  * dev-dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.2...oxabl_semantic-v1.0.3) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.2 to 0.2.3
  * dev-dependencies
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_schema bumped from 0.2.2 to 0.2.3

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.1...oxabl_semantic-v1.0.2) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.1 to 0.2.2
  * dev-dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.1 to 0.2.2

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.0...oxabl_semantic-v1.0.1) (2026-08-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.0 to 0.2.1
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.0 to 0.2.1

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v0.1.0...oxabl_semantic-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.
* **semantic:** UnresolvedReason gains NotFoundInWorkspace and Unknowable. An exhaustive match on it will no longer compile. Both are skip-listed by every lint rule, so no diagnostic changes.

### Features

* **ast:** full-fidelity spans on AST wrapper nodes ([#91](https://github.com/oxabl-project/oxabl/issues/91)) ([2e774b1](https://github.com/oxabl-project/oxabl/commit/2e774b1338bd9c9ff0db014e2cdc238db10801a7))
* capture SHARED/NEW SHARED flags on variable, temp-table, and buffer defines ([04abd6c](https://github.com/oxabl-project/oxabl/commit/04abd6c4860d6b3c0049a08c258b65c044b6e3b5))
* capture SHARED/NEW SHARED flags on variable, temp-table, and buffer defines ([2be6087](https://github.com/oxabl-project/oxabl/commit/2be60876efbe135278129e0f282e99794f3de1b9))
* close residual LINT0001 language-coverage gaps ([#58](https://github.com/oxabl-project/oxabl/issues/58)) ([b3124fd](https://github.com/oxabl-project/oxabl/commit/b3124fdf3fe6c60472e94e5d6fa4f63b3e84d1a1))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **lint:** add block-var-used-outside advisory (LINT0005) ([#123](https://github.com/oxabl-project/oxabl/issues/123)) ([e870d5b](https://github.com/oxabl-project/oxabl/commit/e870d5bb228cf442af933d9c257ab8b569bba726))
* **lint:** credit table parameters, split dead stores into LINT0006 ([#129](https://github.com/oxabl-project/oxabl/issues/129)) ([d539c31](https://github.com/oxabl-project/oxabl/commit/d539c31d7b90cfac43b54a9370adbe5097130268))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **oxabl_lint:** add lint crate with 4 v1 rules (Phase 5) ([ed159e6](https://github.com/oxabl-project/oxabl/commit/ed159e61419770b4b8341d9d227d4f7cbe607556))
* **oxabl_semantic:** add crate skeleton and declare pass (Phase 3) ([c3db693](https://github.com/oxabl-project/oxabl/commit/c3db69311129aa3a79143112f1975a7f828da99d))
* **oxabl_semantic:** add per-pass semantic benches + release config (Phase 8) ([56cc85b](https://github.com/oxabl-project/oxabl/commit/56cc85bc6083f41de704dfd5df1d47e506cd96c4))
* **oxabl_semantic:** add type-check pass (Phase 4b) ([79435cd](https://github.com/oxabl-project/oxabl/commit/79435cd48c5365b7bcb293accd110cfde9450ffe))
* **oxabl_semantic:** crate skeleton and declare pass (Phase 3) ([f155645](https://github.com/oxabl-project/oxabl/commit/f155645048552a34860a0074527b1777a3c12550))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* resolve abbreviated built-in function calls in undefined-symbol lint ([3f45edd](https://github.com/oxabl-project/oxabl/commit/3f45edd11e2f33f6c924a14d1d43c772ae8bcd47))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))
* seed full ABL system-handle set for LINT0001 ([b461dd7](https://github.com/oxabl-project/oxabl/commit/b461dd7336ff86cdedc874131ef7587933e1d121))
* **semantic:** resolve cross-file names, with the rules still held still ([#147](https://github.com/oxabl-project/oxabl/issues/147)) ([8873de9](https://github.com/oxabl-project/oxabl/commit/8873de9a13f5ef5c30d584f13e63e21cea6300e0))
* **semantic:** the cross-file resolution seam, with the rules held still ([#146](https://github.com/oxabl-project/oxabl/issues/146)) ([3cee2c0](https://github.com/oxabl-project/oxabl/commit/3cee2c0a032c3e9d8176ca2740ed9c065e84824e))


### Bug Fixes

* complete shared-flag fields on schema-PR test fixtures after [#63](https://github.com/oxabl-project/oxabl/issues/63) merge ([d78cd4c](https://github.com/oxabl-project/oxabl/commit/d78cd4c777ac887c03b637ae7f5d7dc011a8badf))
* credit table reads in buffer, empty-table, and query forms ([#130](https://github.com/oxabl-project/oxabl/issues/130)) ([#138](https://github.com/oxabl-project/oxabl/issues/138)) ([6e39138](https://github.com/oxabl-project/oxabl/commit/6e3913885f513af727b1aee14973ff7936518132))
* **lint:** count OUTPUT-argument passing as a use in unused-variable (LINT0002) ([#127](https://github.com/oxabl-project/oxabl/issues/127)) ([a6eabb7](https://github.com/oxabl-project/oxabl/commit/a6eabb7346ba10f31a5d1a83993eebddd3287dae))
* reconcile FUNCTION FORWARD/IN SUPER prototype + definition ([#69](https://github.com/oxabl-project/oxabl/issues/69)) ([#73](https://github.com/oxabl-project/oxabl/issues/73)) ([beda23c](https://github.com/oxabl-project/oxabl/commit/beda23c81b953d8dbb080dfb2af4d7b6b0d1bd04))
* resolve built-in ABL functions in undefined-symbol lint ([c47b55a](https://github.com/oxabl-project/oxabl/commit/c47b55a694ec62297359d96b3bd36881965abc43)), closes [#58](https://github.com/oxabl-project/oxabl/issues/58)
* **semantic:** accumulate read/write counts on schema field symbols ([#60](https://github.com/oxabl-project/oxabl/issues/60)) ([#112](https://github.com/oxabl-project/oxabl/issues/112)) ([c70c768](https://github.com/oxabl-project/oxabl/commit/c70c7681323801dcfc2189e66590a7aa1f8403f3))
* **semantic:** resolve unambiguous abbreviated field references ([#82](https://github.com/oxabl-project/oxabl/issues/82)) ([#84](https://github.com/oxabl-project/oxabl/issues/84)) ([f3a340c](https://github.com/oxabl-project/oxabl/commit/f3a340c44bcb3941bfbf02f86ab4459509a43bf1))
* **semantic:** resolve unqualified FIRST-OF/LAST-OF field against block buffers ([#111](https://github.com/oxabl-project/oxabl/issues/111)) ([6dcf5db](https://github.com/oxabl-project/oxabl/commit/6dcf5db754f9efc65b18c16989d89331e3d3035e)), closes [#107](https://github.com/oxabl-project/oxabl/issues/107)
* **semantic:** scope DEFINE VARIABLE to the routine, not the block ([#122](https://github.com/oxabl-project/oxabl/issues/122)) ([edc98f8](https://github.com/oxabl-project/oxabl/commit/edc98f8de8eb9fd48ef3b5f40b3931707680ec6f))
* **semantic:** scope temp-table fields to their temp-table, not program scope ([#110](https://github.com/oxabl-project/oxabl/issues/110)) ([3d19165](https://github.com/oxabl-project/oxabl/commit/3d19165207ff07e62ae7874b7b43a9faa7b9e900)), closes [#106](https://github.com/oxabl-project/oxabl/issues/106)
* **semantic:** treat DO loop counter as a use of an existing variable ([#83](https://github.com/oxabl-project/oxabl/issues/83)) ([#85](https://github.com/oxabl-project/oxabl/issues/85)) ([9d77fcf](https://github.com/oxabl-project/oxabl/commit/9d77fcf5ae542e8aa39438f186abe046de5c65ea))
* silence LINT0001 for MAX/MIN/ABS/VALUE, NO-APPLY, lock method args ([1e997d2](https://github.com/oxabl-project/oxabl/commit/1e997d201e9d42a0240ca9958523c9c98085e4f4))
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Performance Improvements

* seed ABL system handles once via LazyLock ([#72](https://github.com/oxabl-project/oxabl/issues/72)) ([5fabd77](https://github.com/oxabl-project/oxabl/commit/5fabd77fbc5ba5c093c3e3395ecbb18e62c3c0cd))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_parser bumped from 0.6.0 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
