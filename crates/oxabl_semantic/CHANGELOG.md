# Changelog

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
