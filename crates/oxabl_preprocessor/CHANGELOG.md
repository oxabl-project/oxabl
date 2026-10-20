# Changelog

## [0.7.2](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.7.1...oxabl_preprocessor-v0.7.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 3.1.0 to 3.2.0

## [0.7.1](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.7.0...oxabl_preprocessor-v0.7.1) (2026-10-20)


### Bug Fixes

* **preprocessor:** read DEFINED as the AVM does and treat an empty &IF as false ([2185b3f](https://github.com/oxabl-project/oxabl/commit/2185b3ffd019c2562ff4424f4a020193e50f21cf))
* **preprocessor:** read DEFINED as the AVM does and treat an empty &IF as false ([bcc3de8](https://github.com/oxabl-project/oxabl/commit/bcc3de81e6d5473f682b91148e91c27f107f8dae))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 3.0.0 to 3.1.0

## [0.7.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.6.0...oxabl_preprocessor-v0.7.0) (2026-10-16)


### ⚠ BREAKING CHANGES

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

## [0.6.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.5.2...oxabl_preprocessor-v0.6.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **index:** oxabl_index::DegradedReason, oxabl_index::Completeness and oxabl_preprocessor::TextLossKind are #[non_exhaustive]; an exhaustive match on any of them outside its crate no longer compiles.
* **preprocessor:** PreprocessedFile gains text_losses, UnresolvedInclude gains unterminated, and ExpandedFile gains text_losses.
* **preprocessor:** PipelineConfig gains the public preprocessor field.
* **preprocessor:** CollectedDiagnostics gains the public parse_recovered field.

### Features

* **index:** mark the degradation and text-loss enums non-exhaustive ([6999df2](https://github.com/oxabl-project/oxabl/commit/6999df2dc5410ed79b6d92da6240c0d07e08f449))
* **preprocessor:** answer the AVM's predefined names ([d996c49](https://github.com/oxabl-project/oxabl/commit/d996c49d821b7e086a4e0dfb691962e3f4c6bd8f)), closes [#216](https://github.com/oxabl-project/oxabl/issues/216)
* **preprocessor:** keep unreadable and nested unresolved includes as data ([b56cf8b](https://github.com/oxabl-project/oxabl/commit/b56cf8bb426ff6c6eb1fb983bdc0c8d6a0006085))
* **preprocessor:** record lost text, rescan define values and match braces by quote ([57c7152](https://github.com/oxabl-project/oxabl/commit/57c71523e62e3d89fb796d18f61d6c196d8de3f8))


### Bug Fixes

* **preprocessor:** apply only the globals an include defined or undefined ([aa38222](https://github.com/oxabl-project/oxabl/commit/aa3822225904c89edadfd798f7d10bb0f874ae0c))
* **preprocessor:** close the last silent branch choices in &IF evaluation ([5d9fc53](https://github.com/oxabl-project/oxabl/commit/5d9fc53be1ad38b960e3dbf06a070a4843cc154e))
* **preprocessor:** compare two strings as text and report a mixed comparison ([bf1d80e](https://github.com/oxabl-project/oxabl/commit/bf1d80e62fb5822138a16a4356207b1783192a2e))
* **preprocessor:** drop the comments of a define value when it is defined ([83b5d54](https://github.com/oxabl-project/oxabl/commit/83b5d54159efa8972484231afd65102d7ce2a4d2))
* **preprocessor:** evaluate DBTYPE, CONNECTED and LDBNAME from the listed databases ([223560c](https://github.com/oxabl-project/oxabl/commit/223560c095e31cf45d2a44db8012a74e9fda1dfa))
* **preprocessor:** evaluate the functions an &IF names, or say it guessed ([026e29e](https://github.com/oxabl-project/oxabl/commit/026e29e4fc9fff19a17ace4b872f2210d15129da))
* **preprocessor:** expand include references with whitespace inside the braces ([82f05de](https://github.com/oxabl-project/oxabl/commit/82f05dee1506581e02016358d2a747b7a6563f90)), closes [#209](https://github.com/oxabl-project/oxabl/issues/209)
* **preprocessor:** hold a global behind a scoped define made after it ([44bd2b6](https://github.com/oxabl-project/oxabl/commit/44bd2b658994505da2ba09fda44f41c0f4fa6d18))
* **preprocessor:** ignore the gaps of an operand a settled AND or OR never needed ([7cb4713](https://github.com/oxabl-project/oxabl/commit/7cb471302c15516e8ba061c5003dd7a0519a0f6a))
* **preprocessor:** keep a global define behind a scoped name or include argument instead of over it ([ba409a6](https://github.com/oxabl-project/oxabl/commit/ba409a6e861ac84dbbc1feb53d93a3a10c7879a8))
* **preprocessor:** model the unknown value of an &IF condition ([9637e30](https://github.com/oxabl-project/oxabl/commit/9637e30bbc2e35a17f677a192e06d546ffaf6054))
* **preprocessor:** read a define comment that spans lines to its close ([fb52456](https://github.com/oxabl-project/oxabl/commit/fb52456764aae62ee347732efc588e04f1a71653))
* **preprocessor:** restore a scoped origin when the global behind it is undefined ([8ff0593](https://github.com/oxabl-project/oxabl/commit/8ff059320c7c18533586fcee632528daa338c6a6))
* **preprocessor:** restore the copy-everything behaviour of PreprocVarTable::merge_globals and deprecate it ([5a4be34](https://github.com/oxabl-project/oxabl/commit/5a4be344211e64367f94b57a1ad98e0db608fa44))
* **preprocessor:** return the AVM's value of DEFINED for a global and a scoped define ([d36a14f](https://github.com/oxabl-project/oxabl/commit/d36a14ff8cc4c73f68e3f63b45084bc2cbe480ac))
* **preprocessor:** settle an AND or OR chain by its result, not by operand order ([a8e20d0](https://github.com/oxabl-project/oxabl/commit/a8e20d0b66a94fade1c08c913d4727fff6093f34))
* **preprocessor:** treat a database list inferred from schema files as partial ([c26de87](https://github.com/oxabl-project/oxabl/commit/c26de8751a8a8af649c800503e79f9c8e8eb7da1))
* **preprocessor:** treat LDBNAME and DBTYPE of DICTDB as a gap under an inferred database list ([4f9e563](https://github.com/oxabl-project/oxabl/commit/4f9e5635008d93acbf1433aa68e6e906f3fbf05c))


### Performance Improvements

* **preprocessor:** evaluate a condition without copying its text or cloning its tokens ([9f62a34](https://github.com/oxabl-project/oxabl/commit/9f62a34781f8b1cc1263e86561d295ad72c14def))
* **preprocessor:** find directive keywords without upper-casing the text ([2e0c989](https://github.com/oxabl-project/oxabl/commit/2e0c98994fb07cda6f2830e39800b9530a78c17d))
* **preprocessor:** hand a substituted value to the source list without cloning it ([597e3f2](https://github.com/oxabl-project/oxabl/commit/597e3f2e9bde85d6fd4f4b99e648a4f79d0a6775))
* **preprocessor:** keep whether the scan is emitting instead of recomputing it per byte ([f4684ce](https://github.com/oxabl-project/oxabl/commit/f4684ceb6b71d3ac17165257e8640d407d6aa927))
* **preprocessor:** move a define's value instead of copying it, and scan for comments by byte ([a4e047b](https://github.com/oxabl-project/oxabl/commit/a4e047bba311a0594ce464a9e96a3d28b9156c62))
* **preprocessor:** share the predefined names between runs ([1b4dd9b](https://github.com/oxabl-project/oxabl/commit/1b4dd9be5e59c3354e4318eac7da7f995a6c73dc))
* **preprocessor:** undo an include's variable changes instead of cloning the table ([ba533e1](https://github.com/oxabl-project/oxabl/commit/ba533e117b09b8fd2610fe48957a013d3fa39516))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0

## [0.5.2](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.5.1...oxabl_preprocessor-v0.5.2) (2026-09-21)


### Bug Fixes

* **preprocessor:** report a nested unresolvable include instead of dropping it ([#177](https://github.com/oxabl-project/oxabl/issues/177)) ([676dafe](https://github.com/oxabl-project/oxabl/commit/676dafe4f3b5b3dc1d98ed43342033f567a7f778))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 1.0.2 to 1.0.3

## [0.5.1](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.5.0...oxabl_preprocessor-v0.5.1) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 1.0.1 to 1.0.2

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.4.0...oxabl_preprocessor-v0.5.0) (2026-08-12)


### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_workspace bumped from 1.0.0 to 1.0.1

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.3.1...oxabl_preprocessor-v0.4.0) (2026-08-04)


### Features

* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([6447178](https://github.com/oxabl-project/oxabl/commit/64471788cd3436c8f62fe64b8f9b5d6684c03550))
* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([bf346d8](https://github.com/oxabl-project/oxabl/commit/bf346d80b5e9d9b390a7fa3ffa4e38c6dbd7023f))
* **bench:** add preprocessor benchmark suite ([1e14fa7](https://github.com/oxabl-project/oxabl/commit/1e14fa77621a747518290e8ec9137bbaf154ccd8))
* **cli:** wire preprocessor into `oxabl check --preprocess` ([9870f98](https://github.com/oxabl-project/oxabl/commit/9870f98b21d29e4e07760457180e209376caf57a))
* **preprocessor:** add include file argument support (positional and named) ([2591eca](https://github.com/oxabl-project/oxabl/commit/2591eca22af18057ea2e0861c194666422917eb2))
* **preprocessor:** add MATCHES, BEGINS, and keyword operators to condition evaluator ([0bd1d19](https://github.com/oxabl-project/oxabl/commit/0bd1d194dcec92f3b21e25538734ac12ccbea6a5))
* **preprocessor:** add oxabl_preprocessor crate with include expansion, &IF evaluation, and variable tracking ([379389b](https://github.com/oxabl-project/oxabl/commit/379389b83afff049654ea586778b7b8974bde640))
* **preprocessor:** resolve dynamic include names like `{{&frame}.f …}` ([69deb29](https://github.com/oxabl-project/oxabl/commit/69deb2940b147b896dbe9f656028935bb24c8867))


### Bug Fixes

* ADM2 mid-line &IF ([#65](https://github.com/oxabl-project/oxabl/issues/65)) and xp-property BUFFER-FIELD path ([#66](https://github.com/oxabl-project/oxabl/issues/66)) ([#67](https://github.com/oxabl-project/oxabl/issues/67)) ([872c741](https://github.com/oxabl-project/oxabl/commit/872c7411bdd0d9d8a91a695993fbc390f159ebe6))
* allow '.' in preprocessor macro names for include-once guards ([#74](https://github.com/oxabl-project/oxabl/issues/74)) ([#75](https://github.com/oxabl-project/oxabl/issues/75)) ([9f9fd51](https://github.com/oxabl-project/oxabl/commit/9f9fd5193e30ea7d2ddded15305d38475d58476b))
* **check:** silence &MESSAGE noise and only surface error-level preprocessor diagnostics ([e04476f](https://github.com/oxabl-project/oxabl/commit/e04476f291d287e520e353c54e8a226c5b8252d9))
* eliminate unused-variable false positives from block-body parse errors ([#79](https://github.com/oxabl-project/oxabl/issues/79)) ([#80](https://github.com/oxabl-project/oxabl/issues/80)) ([e653c36](https://github.com/oxabl-project/oxabl/commit/e653c365373827ab7ec782776e987331732d8694))
* expand undefined {&macro} references to empty string ([3397101](https://github.com/oxabl-project/oxabl/commit/3397101f82cbf5cb405dd9c2a954c25896a4d403))
* **preprocessor:** downgrade missing-include from Error to Warning ([dc115dd](https://github.com/oxabl-project/oxabl/commit/dc115dddb121cb57b0f416405dbb5c96e681316e))
* **preprocessor:** handle multi-line include arguments and spaces around = ([f7dda13](https://github.com/oxabl-project/oxabl/commit/f7dda132c72dd770bf403a08ddd437df24f67993))
* **preprocessor:** handle whitespace after include name, surface diagnostics ([45ba132](https://github.com/oxabl-project/oxabl/commit/45ba132a5903c7cbbc511974f18d3bfc42329489))
* **preprocessor:** resolve named {&…} refs in define values at define time ([#121](https://github.com/oxabl-project/oxabl/issues/121)) ([84912cc](https://github.com/oxabl-project/oxabl/commit/84912cc004e6b7b95ff06043ce31570fdc6393a3))
* **preprocessor:** skip .i files in check command and preserve undefined refs ([1617ebe](https://github.com/oxabl-project/oxabl/commit/1617ebef94f54b1518187a993b2c2ea7b1fe552a))
* **preprocessor:** skip comment bodies when scanning for directives and includes ([b754eaf](https://github.com/oxabl-project/oxabl/commit/b754eaf13b4a060fb469ff0ed358444402407dd4))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
