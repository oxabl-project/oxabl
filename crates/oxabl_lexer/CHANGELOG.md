# Changelog

## [0.4.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lexer-v0.4.0...oxabl_lexer-v0.4.1) (2026-04-16)


### Bug Fixes

* **lexer,parser:** smart backslash-escape-quote and NEXT-PROMPT statement ([0f22c76](https://github.com/oxabl-project/oxabl/commit/0f22c769d1c35a8142ecb1a93531f1292daf09a0))
* **lexer:** accept backslash-escaped quotes inside string literals ([40579e7](https://github.com/oxabl-project/oxabl/commit/40579e7c43f77f83e0252b1d71a14d30f2b2fe0a))
* **lexer:** handle include references with preprocessor variable paths ([7c4856d](https://github.com/oxabl-project/oxabl/commit/7c4856d37b8da5ae69da713971b1570746b36757))
* **lexer:** remove backslash-escape-quote tolerance in strings ([09a6ef9](https://github.com/oxabl-project/oxabl/commit/09a6ef930ae110f103755d504bb6f4fed3aa1735))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 0.4.0 to 0.5.0

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lexer-v0.3.1...oxabl_lexer-v0.4.0) (2026-04-13)


### Features

* **ast:** add dataset/data-source AST types and lexer keywords ([96d0416](https://github.com/oxabl-project/oxabl/commit/96d041669d0b2c4b20d14fe40503b11079e4b345))
* **bench:** expand benchmark suite to cover parser, lexer scenarios, and source map ([18c6681](https://github.com/oxabl-project/oxabl/commit/18c6681ab28e2097fdff15c647841d95ff49fb51))
* **bench:** expand benchmark suite to parser, lexer scenarios, and source map ([36abc32](https://github.com/oxabl-project/oxabl/commit/36abc32aebd88514f1fcd203b9da0c9a4eb3d88a))
* **lexer:** add OO-ABL keyword Kind variants ([2380ac8](https://github.com/oxabl-project/oxabl/commit/2380ac8b856175931097d9d2703f0b60ba9b7f11))
* **lexer:** add OO-ABL keyword Kind variants ([c3977f4](https://github.com/oxabl-project/oxabl/commit/c3977f46987e5255766b2c384dc1499cac592364))
* **parser:** add database manipulation statements ([8428b93](https://github.com/oxabl-project/oxabl/commit/8428b93b8848864fc52ec6f95310184130f7c3a8))
* **parser:** add database manipulation statements ([404e85c](https://github.com/oxabl-project/oxabl/commit/404e85c91a57f5f7244cb1680f05d4f5ed0bb1e7))
* **parser:** add dataset and data-source parsing support ([ae26e42](https://github.com/oxabl-project/oxabl/commit/ae26e428db7022c864bde37b877e1f7021ca1b0c))
* **parser:** add dataset benchmark fixture and apply formatting ([e2510d5](https://github.com/oxabl-project/oxabl/commit/e2510d582b67d7906fde12ba94a91ba341c424c4))
* **parser:** Add include file reference parsing ([28cba6b](https://github.com/oxabl-project/oxabl/commit/28cba6b250247e2936c3a19e711465145189e7fe))
* **parser:** add ON trigger and TRIGGER PROCEDURE statement parsing ([0ec08d3](https://github.com/oxabl-project/oxabl/commit/0ec08d30a6f927ff11e54a0cfc2c53c68e8d0af6))
* **parser:** add ON trigger and TRIGGER PROCEDURE statement parsing ([6dfffdc](https://github.com/oxabl-project/oxabl/commit/6dfffdcac671142290109f13294499ba14e8fac7))
* **parser:** add OO-ABL support (CLASS, METHOD, PROPERTY, INTERFACE) ([f2ec7a5](https://github.com/oxabl-project/oxabl/commit/f2ec7a5587330fd7c7a0909c752c0478e78fae02))
* **parser:** add preprocessor statement parsing ([a2f82fd](https://github.com/oxabl-project/oxabl/commit/a2f82fd7032dda5fc7c27c3dccead1cf652eec17))
* **parser:** add preprocessor statement parsing ([eea5606](https://github.com/oxabl-project/oxabl/commit/eea560646d49c24614f9bce123432523781defe1))
* **parser:** add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT parsing ([4eb3af4](https://github.com/oxabl-project/oxabl/commit/4eb3af451f99bf02512e5799b9c25bfd1d0336e7))
* **parser:** add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT parsing ([233af8c](https://github.com/oxabl-project/oxabl/commit/233af8ca8df241e00606241a38b8aaed5e86871c))
* **parser:** add stream and frame parsing ([e959ffd](https://github.com/oxabl-project/oxabl/commit/e959ffd74e5a2459800eb076a4e19547d3b3ae70))
* **parser:** add stream and frame parsing ([8182d4e](https://github.com/oxabl-project/oxabl/commit/8182d4e1b952d7de365348469b9d69ba2a9e599c))
* **parser:** handle real-world ABL patterns from oe100fi.p ([15b4405](https://github.com/oxabl-project/oxabl/commit/15b4405e906e6d4dd1c9853590885a212db12a47))


### Bug Fixes

* **ast, lexer:** resolved some code breakage from the last merge ([f7b44d3](https://github.com/oxabl-project/oxabl/commit/f7b44d37474515ff00c0865d3a6a462af7966ff8))
* **lexer,parser:** &ANALYZE-SUSPEND, quoted procs, CAN-FIND OF, ACCUM label, string :n ([259ea59](https://github.com/oxabl-project/oxabl/commit/259ea594b8d21d5f4fb1cf39aef2ba098584ef73))
* **lexer,parser:** allow '$' in identifiers; add Compile to can_be_identifier; EXTENT before LIKE in temp-table field ([85bf3e4](https://github.com/oxabl-project/oxabl/commit/85bf3e4d083aa51af4706bbcda5377ed4578f22f))
* **lexer,parser:** backslash line-continuation, Add/Write/Save as method names ([92ff17a](https://github.com/oxabl-project/oxabl/commit/92ff17aaeac22cffe4a0ea65372c25e8f2677720))
* **lexer,parser:** backslash string escapes, ASSIGN FRAME clause ([4a9874b](https://github.com/oxabl-project/oxabl/commit/4a9874b6816dd9a5c7c8870260adf4114b3b8454))
* **lexer,parser:** nested include braces, GET/CREATE WIDGET-POOL, VIEW-AS TEXT ([794b1ca](https://github.com/oxabl-project/oxabl/commit/794b1cac9cebb25e12c85006c5ac0fc91d5dde6f))
* **lexer,parser:** tilde line-continuation in skip_whitespace, Progress.* class type ([f6914bf](https://github.com/oxabl-project/oxabl/commit/f6914bf725b0c2a3cfccdadef7a552abcb81de88))
* **lexer,parser:** trailing-minus numbers, MESSAGE AUTO-RETURN modifier ([0829044](https://github.com/oxabl-project/oxabl/commit/0829044eb0967d24ccb948ccc7715c764b54dc5a))
* **lexer/parser:** tilde line continuation, @ field-format operator, DISPLAY @/TO ([4a1b9de](https://github.com/oxabl-project/oxabl/commit/4a1b9de289328fbe1a36b9384f498a6cc74948c2))
* **lexer:** allow '#' character in ABL identifiers ([3ccaf4c](https://github.com/oxabl-project/oxabl/commit/3ccaf4c0062e6bf4e986c9b653a62383a6944da0))
* **lexer:** backslash is not an escape character in ABL strings ([95cfc04](https://github.com/oxabl-project/oxabl/commit/95cfc04230d80a5320c4dddc60324e395eb8a6b6))
* **lexer:** Consume ABL translation suffixes (:U, :T) as part of string literals ([962ad7d](https://github.com/oxabl-project/oxabl/commit/962ad7d25bcad7e445680eaa3194c39fdafabe06))
* **lexer:** Consume ABL translation suffixes (:U, :T) as part of string literals ([af6bdc3](https://github.com/oxabl-project/oxabl/commit/af6bdc3aa1a43dfba36c9e111be2294e16959ed6))
* **parser,lexer:** handle :U1 format suffix, ACCUM statement, and REPOSITION-TO-ROW method calls ([067ac61](https://github.com/oxabl-project/oxabl/commit/067ac61d349a4abc3b3055496cd065a92c7036d5))
* **parser,lexer:** handle dotnetrm/ patterns — 100% success rate ([36c94a9](https://github.com/oxabl-project/oxabl/commit/36c94a9bfa104666c0b760803da98d56f2af89ab))
* **parser:** add THROW statement and ON phrase support ([668fd45](https://github.com/oxabl-project/oxabl/commit/668fd4543479358e0be21318b95f6c955894f13d))
* **parser:** add Xref/XrefXml to can_be_identifier for COMPILE member access ([6b3d050](https://github.com/oxabl-project/oxabl/commit/6b3d0504e4cf4d278952ee1e38c42ce8e2791482))
* **parser:** BY-REFERENCE modifier, inline IF, BELL, QUERY expr, END CASE, RELATION-FIELD ([5ad0fb7](https://github.com/oxabl-project/oxabl/commit/5ad0fb7cbde7c9e6a0479319d474fedaaffc185c))
* **parser:** DELETE/CREATE type prefixes, IMPORT/OS-DELETE stmts, .01 decimal literals ([0503d90](https://github.com/oxabl-project/oxabl/commit/0503d900476dc988528ca0d38eda3a8221bae06a))
* **parser:** EXTERNAL procedure, RETURN param, BREAK BY, identifier keywords ([fa2c220](https://github.com/oxabl-project/oxabl/commit/fa2c220d44a17c0cf293d1dcabf0923431d1298e))
* **parser:** FUNCTION MAP TO, DISPLAY WHEN/TO, tilde line continuation in &DEFINE ([10c6494](https://github.com/oxabl-project/oxabl/commit/10c6494193e49da770fdbbdfb48433e77ffcec4c))
* **parser:** handle many real-world ABL patterns from pcna-erp ([386fbce](https://github.com/oxabl-project/oxabl/commit/386fbce593b5972586760d3f3f5872d76bf69d9a))
* **parser:** LAST-EVENT/CONNECT as identifiers, WAIT-FOR/SET stmt, stream IO dots, ON frame widget ([fd11b4d](https://github.com/oxabl-project/oxabl/commit/fd11b4d770aea071cecf911a2314cd19e69aa0a2))
* **parser:** reach 100% on api/ — DYNAMIC-NEW, compound assignments, property EXTENT, method body with period ([6115db5](https://github.com/oxabl-project/oxabl/commit/6115db52d9ea4a25a63c3048fd0c70702e8b9633))
* **parser:** UI widgets, DEFINE FRAME, numeric labels, CLEAR/HIDE, SIZE keyword ([ab5eb21](https://github.com/oxabl-project/oxabl/commit/ab5eb213bd115a1ab07e018a35809d027e51723f))


### Performance Improvements

* **lexer,parser:** pre-allocate token Vec, SmallVec for AssignPair, ternary fast-path ([230fff9](https://github.com/oxabl-project/oxabl/commit/230fff913a04cdd4c5114769575a1e334a09e1ae))
* **lexer,parser:** pre-allocate token Vec, SmallVec for AssignPair, ternary fast-path ([e1f1e37](https://github.com/oxabl-project/oxabl/commit/e1f1e375077644e4ad12bb64156244628f0637d7))
* **lexer:** eliminate heap allocations in keyword matching ([2fa1b87](https://github.com/oxabl-project/oxabl/commit/2fa1b87f9c7bb37ea3fa699a9e4073a34d8c5226))
* **lexer:** eliminate heap allocations in keyword matching ([c7553f7](https://github.com/oxabl-project/oxabl/commit/c7553f7d46f0867c43794e7afdd65eb31795953b))
* **lexer:** use length-dispatched match in match_keyword() ([3a24353](https://github.com/oxabl-project/oxabl/commit/3a243534295d9b3737f056fb107816449ba06c45))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 0.3.0 to 0.4.0

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lexer-v0.3.0...oxabl_lexer-v0.3.1) (2026-04-03)


### Bug Fixes

* add versions to deps ([0d1b3d9](https://github.com/oxabl-project/oxabl/commit/0d1b3d951b3ce8b2f617c9b328644fad2ce1a551))

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lexer-v0.2.0...oxabl_lexer-v0.3.0) (2026-04-03)


### Features

* **workspace:** Add publish job to release workflow and add required fields to crates ([e819219](https://github.com/oxabl-project/oxabl/commit/e81921955ff078af912aa3e590673ffe3ed576fa))
* **workspace:** Add publish job to release workflow and add required… ([439b2f5](https://github.com/oxabl-project/oxabl/commit/439b2f5397bc899948fd38227fe98f7b667c18db))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lexer-v0.1.0...oxabl_lexer-v0.2.0) (2026-04-03)


### Features

* **ast,parser:** add initial AST literal types and token conversion ([f4ce601](https://github.com/oxabl-project/oxabl/commit/f4ce601f564e5d89e15f8d983cb459e3ff0e968a))
* **codegen, lexer:** New callable function list ([b7fba6f](https://github.com/oxabl-project/oxabl/commit/b7fba6f04d922b02f24d28dc85637d5117a69698))
* **codegen:** Massive code gen overhaul ([72a60b5](https://github.com/oxabl-project/oxabl/commit/72a60b52c58aa63cac07899e68155049fbefd5d6))
* **codegen:** Refactor ([4697aab](https://github.com/oxabl-project/oxabl/commit/4697aabd67a73abf0a04a009e6c42a74aaf47618))
* **lexer:** Added Benchmarks ([82e2ea2](https://github.com/oxabl-project/oxabl/commit/82e2ea26fe556c1501297d7a7391747d64606adc))
* **lexer:** Decimal ([376ddb1](https://github.com/oxabl-project/oxabl/commit/376ddb1c9fc19f6ce5a92ef07ddb2ee626a2a8e9))
* **lexer:** More keywords and symbols in build ([a6f7b4a](https://github.com/oxabl-project/oxabl/commit/a6f7b4a122ffc28d8da792b44f6a20e5d5a6fea2))
* **lexer:** More operators, keywords, finish foundations ([90d31d0](https://github.com/oxabl-project/oxabl/commit/90d31d0837c289d50428b79222aad99efa3e74b9))
* **lexer:** MVP finished ([4445086](https://github.com/oxabl-project/oxabl/commit/4445086d1086e4bacfc209fc81000ee83cc544de))
* **lexer:** Using codegen ([860df05](https://github.com/oxabl-project/oxabl/commit/860df0554712535ea90ba8509fb8a8f89022b618))
* **lib:** Read strings, numbers ([1583cb7](https://github.com/oxabl-project/oxabl/commit/1583cb7a8b62edbb5942517e2f7d25e876ed8121))
* **oxabl:** Initial commit, includes oxabl, oxabl_lexer, oxable_parser ([a21760b](https://github.com/oxabl-project/oxabl/commit/a21760b5b749d3ff1f810c42ec941433d02fe675))
* **parser, ast:** Began procedure parsing support ([6d6af23](https://github.com/oxabl-project/oxabl/commit/6d6af2373d13d71b0c9f5e708713891e87968d48))
* **parser,ast:** Find statement support ([00db56e](https://github.com/oxabl-project/oxabl/commit/00db56ed11836cd04bd1717d3e8bd1914d3aa0cc))
* **parser:** Finished token to literal, added tests for each literal type ([236b230](https://github.com/oxabl-project/oxabl/commit/236b230b7cce84f329719a6bbaded04160898cce))
* **parser:** Finished token to literal, added tests for each literal type ([#2](https://github.com/oxabl-project/oxabl/issues/2)) ([9846c9d](https://github.com/oxabl-project/oxabl/commit/9846c9d1d5ac246e000d98d976af4245b31d0258))
* **paser, lexer, ast:** WIP, adding for each and lock type support ([81e12b2](https://github.com/oxabl-project/oxabl/commit/81e12b2cdb5890a97c588de12c958d17c01780db))


### Bug Fixes

* **lexer:** Added tokenize_full to group to get throughput in bench ([8eb68a4](https://github.com/oxabl-project/oxabl/commit/8eb68a49e79ea593e87e58207dcb248c62da853a))
* **lexer:** Number Literals ([6c41218](https://github.com/oxabl-project/oxabl/commit/6c412185736f01add009436f8f303c7e0211fe12))
* **lexer:** Parse lock types correctyl ([fa28158](https://github.com/oxabl-project/oxabl/commit/fa2815874c946b63f5198f4e3fbe6550ca0f1fce))
* **parser,ast,lexer:** Mod, Comparison Operators ([e33134d](https://github.com/oxabl-project/oxabl/commit/e33134d0ec3a725a3e976b8a374d164aac8eb1ac))
* **tokenizer:** Fix example test ([c8871f8](https://github.com/oxabl-project/oxabl/commit/c8871f8bbb769bf78889d1a8925120b9765acb5e))
* **workspace:** release please and circular dependency ([3118dad](https://github.com/oxabl-project/oxabl/commit/3118dada817296527564fdea053a8c036b8c0ed3))
