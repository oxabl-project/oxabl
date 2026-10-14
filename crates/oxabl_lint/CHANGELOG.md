# Changelog

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.1.1...oxabl_lint-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **parser:** DataType has a new variant, Generic, and ExpressionKind::New has a new field, type_arguments. Code that matches DataType exhaustively, or builds or matches New without `..`, must handle them.
* **parser:** StatementKind::Function has a new field, prototype_parameters. Code that builds the variant or matches it without `..` must name it.
* **lint:** a diagnostic is now reported at its rule's default severity when nothing configures the rule. With an empty LintSeverityMap, unknown-table-or-field and type-mismatch-assignment now report as warnings, as [workspace.lint] already documented.
* **parser:** `StatementKind` gains an `Annotation` variant, so exhaustive matches over it need a new arm.
* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.

### Features

* **cli:** check context-free strict rules in the include files of a walked tree ([7b931f0](https://github.com/oxabl-project/oxabl/commit/7b931f0770f0149ff22c6f0c35658085682797c5))
* **lint:** add a public rule registry with rule groups ([352f942](https://github.com/oxabl-project/oxabl/commit/352f942df9a5db018e04f3a158a95c6c1fc6f99b))
* **lint:** add the syntactic strict rules CL001, CL002, CL005 and TX006 ([1467921](https://github.com/oxabl-project/oxabl/commit/1467921f019e6e4dd5fef38ca163d662aaaeb6b2))
* **lint:** register CL001, CL002, CL005 and TX006 in their strict groups ([714834e](https://github.com/oxabl-project/oxabl/commit/714834e13ebb313b70c94ca7dc1438215304e3c4))
* **parser:** keep the parameters of a function prototype ([038c3ff](https://github.com/oxabl-project/oxabl/commit/038c3ff21b19654b47dcb17f5e153d3a42a58e80))
* **parser:** keep the type arguments of a generic class name ([6f5e3cb](https://github.com/oxabl-project/oxabl/commit/6f5e3cb11f3e20f4fa2c89e191c6e18c247b4ab9))
* **parser:** parse ABL annotations as a statement ([0de3bb6](https://github.com/oxabl-project/oxabl/commit/0de3bb6408e74e1bdbc729288713880d2d23bc90))


### Bug Fixes

* **lint:** ask for NO-UNDO on both sides of a shared definition ([9281841](https://github.com/oxabl-project/oxabl/commit/9281841487ca3f0edb0f67b81c1dc345a6f70228))
* **lint:** lower only the findings an includer can change ([4e29dbc](https://github.com/oxabl-project/oxabl/commit/4e29dbc0cf283ae44d61891eb1b44173b0ca53de))
* **lint:** lower the confidence of includer-dependent fragment findings ([4fcec91](https://github.com/oxabl-project/oxabl/commit/4fcec91f4528a01993297ec2456a7555bd969046))
* **lint:** make block-var-used-outside path-aware ([4bc6785](https://github.com/oxabl-project/oxabl/commit/4bc67857b18afb558a3e19d5fe9b09c0f774076a)), closes [#124](https://github.com/oxabl-project/oxabl/issues/124) [#126](https://github.com/oxabl-project/oxabl/issues/126)
* **lint:** read include fragments without comments, undefines and local defines ([aaf66ed](https://github.com/oxabl-project/oxabl/commit/aaf66ede95ce599dbdc4d66ca20abb3f57d6056a))
* **lint:** reset block-var-used-outside at UNDO statements ([6ec7165](https://github.com/oxabl-project/oxabl/commit/6ec7165d81ce073bc6b340bc0f578b3c2e174fad))
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))


### Performance Improvements

* **lint:** add a benchmark for block-hoisted variables ([d9ff7b6](https://github.com/oxabl-project/oxabl/commit/d9ff7b696d785cc94b5411033d2b440b7012196d))
* **lint:** build one unit and solve once per block for LINT0005 ([4d61b17](https://github.com/oxabl-project/oxabl/commit/4d61b17c80287480aea40a417bc7e4bd93c60c84))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_flow bumped from 0.1.0 to 0.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
  * dev-dependencies
    * oxabl_index bumped from 1.0.3 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_preprocessor bumped from 0.5.2 to 0.6.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.1.0...oxabl_lint-v1.1.1) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.1.0 to 1.1.1
  * dev-dependencies
    * oxabl_index bumped from 1.0.2 to 1.0.3
    * oxabl_parser bumped from 1.0.3 to 1.0.4

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.3...oxabl_lint-v1.1.0) (2026-09-21)


### Features

* **schema:** ship the OpenEdge metaschema so `_file` and friends resolve ([#189](https://github.com/oxabl-project/oxabl/issues/189)) ([0367b60](https://github.com/oxabl-project/oxabl/commit/0367b608273df8e20bb6c89495e27f23d8e50da2)), closes [#151](https://github.com/oxabl-project/oxabl/issues/151)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
  * dev-dependencies
    * oxabl_index bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.2...oxabl_lint-v1.0.3) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_index bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_preprocessor bumped from 0.5.1 to 0.5.2
    * oxabl_workspace bumped from 1.0.2 to 1.0.3

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.1...oxabl_lint-v1.0.2) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_index bumped from 0.1.2 to 1.0.0
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_preprocessor bumped from 0.5.0 to 0.5.1
    * oxabl_workspace bumped from 1.0.1 to 1.0.2

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.0...oxabl_lint-v1.0.1) (2026-08-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_semantic bumped from 1.0.0 to 1.0.1
  * dev-dependencies
    * oxabl_index bumped from 0.1.1 to 0.1.2
    * oxabl_schema bumped from 0.2.0 to 0.2.1
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_preprocessor bumped from 0.4.0 to 0.5.0
    * oxabl_workspace bumped from 1.0.0 to 1.0.1

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v0.1.0...oxabl_lint-v1.0.0) (2026-08-04)


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
* resolve abbreviated built-in function calls in undefined-symbol lint ([3f45edd](https://github.com/oxabl-project/oxabl/commit/3f45edd11e2f33f6c924a14d1d43c772ae8bcd47))
* resolve abbreviated built-in function calls in undefined-symbol lint ([7f763a3](https://github.com/oxabl-project/oxabl/commit/7f763a39c99d66e8f1e45d5e598ab9d18f197f66))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))
* seed full ABL system-handle set for LINT0001 ([b461dd7](https://github.com/oxabl-project/oxabl/commit/b461dd7336ff86cdedc874131ef7587933e1d121))
* **semantic:** resolve cross-file names, with the rules still held still ([#147](https://github.com/oxabl-project/oxabl/issues/147)) ([8873de9](https://github.com/oxabl-project/oxabl/commit/8873de9a13f5ef5c30d584f13e63e21cea6300e0))
* **semantic:** the cross-file resolution seam, with the rules held still ([#146](https://github.com/oxabl-project/oxabl/issues/146)) ([3cee2c0](https://github.com/oxabl-project/oxabl/commit/3cee2c0a032c3e9d8176ca2740ed9c065e84824e))


### Bug Fixes

* bind FUNCTION signature params into function scope ([#68](https://github.com/oxabl-project/oxabl/issues/68)) ([#70](https://github.com/oxabl-project/oxabl/issues/70)) ([8699d86](https://github.com/oxabl-project/oxabl/commit/8699d86109b9e0035693a1db51c8423e3bf74ea9))
* complete shared-flag fields on schema-PR test fixtures after [#63](https://github.com/oxabl-project/oxabl/issues/63) merge ([d78cd4c](https://github.com/oxabl-project/oxabl/commit/d78cd4c777ac887c03b637ae7f5d7dc011a8badf))
* credit table reads in buffer, empty-table, and query forms ([#130](https://github.com/oxabl-project/oxabl/issues/130)) ([#138](https://github.com/oxabl-project/oxabl/issues/138)) ([6e39138](https://github.com/oxabl-project/oxabl/commit/6e3913885f513af727b1aee14973ff7936518132))
* eliminate unused-variable false positives from block-body parse errors ([#79](https://github.com/oxabl-project/oxabl/issues/79)) ([#80](https://github.com/oxabl-project/oxabl/issues/80)) ([e653c36](https://github.com/oxabl-project/oxabl/commit/e653c365373827ab7ec782776e987331732d8694))
* **lint:** count OUTPUT-argument passing as a use in unused-variable (LINT0002) ([#127](https://github.com/oxabl-project/oxabl/issues/127)) ([a6eabb7](https://github.com/oxabl-project/oxabl/commit/a6eabb7346ba10f31a5d1a83993eebddd3287dae))
* resolve built-in ABL functions in undefined-symbol lint ([c47b55a](https://github.com/oxabl-project/oxabl/commit/c47b55a694ec62297359d96b3bd36881965abc43)), closes [#58](https://github.com/oxabl-project/oxabl/issues/58)
* **semantic:** treat DO loop counter as a use of an existing variable ([#83](https://github.com/oxabl-project/oxabl/issues/83)) ([#85](https://github.com/oxabl-project/oxabl/issues/85)) ([9d77fcf](https://github.com/oxabl-project/oxabl/commit/9d77fcf5ae542e8aa39438f186abe046de5c65ea))
* silence LINT0001 for MAX/MIN/ABS/VALUE, NO-APPLY, lock method args ([1e997d2](https://github.com/oxabl-project/oxabl/commit/1e997d201e9d42a0240ca9958523c9c98085e4f4))
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_semantic bumped from 0.1.0 to 1.0.0
  * dev-dependencies
    * oxabl_index bumped from 0.1.0 to 0.1.1
    * oxabl_schema bumped from 0.1.0 to 0.2.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_parser bumped from 0.6.0 to 1.0.0
    * oxabl_preprocessor bumped from 0.3.1 to 0.4.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
