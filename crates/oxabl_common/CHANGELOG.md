# Changelog

## [1.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.3.1...oxabl_common-v1.4.0) (2026-11-24)


### Features

* **lsp:** folding and selection ranges from the syntax tree ([39ffd97](https://github.com/oxabl-project/oxabl/commit/39ffd97cfc343ee634472870c8d676b42f5200de))
* **lsp:** hover with types, signatures, built-in docs and schema fields ([ca50283](https://github.com/oxabl-project/oxabl/commit/ca502838f6343f5f28a225df5e8b64020b304263))
* **lsp:** inlay hints for locks, index, buffer scope and transactions ([5358467](https://github.com/oxabl-project/oxabl/commit/535846717f49af4de07e31280201cd7f76de8cbd))
* **lsp:** related locations on unknown-value and handle-leak diagnostics ([2bd42ec](https://github.com/oxabl-project/oxabl/commit/2bd42ec174d6051e58327e78277516accbdb172b))
* **workspace:** detect XCODE-encrypted source and skip it with one message ([a07fa67](https://github.com/oxabl-project/oxabl/commit/a07fa6756fa4dafdd4e408ba36cce8815b90c854))


### Bug Fixes

* **analysis:** look up substitution sites in a set, not a list ([70c2fbe](https://github.com/oxabl-project/oxabl/commit/70c2fbe8fdbbe07a0290368a87865c68ebe3d3bc))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 5.0.0 to 6.0.0

## [1.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.3.0...oxabl_common-v1.3.1) (2026-11-23)


### Bug Fixes

* **preprocessor:** long lines and runs of unclosed braces cost memory and time in line with the input ([322fc7b](https://github.com/oxabl-project/oxabl/commit/322fc7b9e4edfe2601d03060c3508b3b0e0eb81c))

## [1.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.2.2...oxabl_common-v1.3.0) (2026-11-20)


### Features

* ABL document outline, and no-setup defaults for the editor ([375b8c4](https://github.com/oxabl-project/oxabl/commit/375b8c4704d4de20e8104c188fb0b9b7d6f2f855))
* **pipeline:** document outline contract ([8083056](https://github.com/oxabl-project/oxabl/commit/808305661312c226313a005332e67a57159e89a8))


### Bug Fixes

* **parser:** stop at a nesting and chain limit, and parse on a stack of known size ([7b741da](https://github.com/oxabl-project/oxabl/commit/7b741dae5637ba90408c8ac0a857464a5c44ee13))
* **parser:** stop at a nesting and chain limit, and parse on a stack of known size ([e39f9c0](https://github.com/oxabl-project/oxabl/commit/e39f9c0922816300faf35657e8294cc713410354))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 4.0.0 to 5.0.0

## [1.2.2](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.2.1...oxabl_common-v1.2.2) (2026-11-06)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 3.0.0 to 4.0.0

## [1.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.2.0...oxabl_common-v1.2.1) (2026-10-16)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0

## [1.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.1.0...oxabl_common-v1.2.0) (2026-10-14)


### Features

* **common:** resolve lint settings by rule name, group and file scope ([b004465](https://github.com/oxabl-project/oxabl/commit/b004465cad6d0fd26b112b3bc29d4b7467066615))


### Bug Fixes

* **lint:** rank group toggles below rule settings and warn on an override with no files ([e31d5f8](https://github.com/oxabl-project/oxabl/commit/e31d5f8295aebeb94fbaff09a1106f185b38ed73))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v1.0.0...oxabl_common-v1.1.0) (2026-08-12)


### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v0.5.0...oxabl_common-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* **api:** `oxabl::parse`, `oxabl::analyze`, `oxabl::analyze_with_fs`, and `oxabl_formatter::format_source` are deprecated in favor of their `try_*` siblings. They still compile and behave identically; callers denying deprecation warnings will need to migrate or allow them.

### Features

* **api:** diagnostic renderer, Display, and serde on the diagnostic family ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#114](https://github.com/oxabl-project/oxabl/issues/114)) ([0681257](https://github.com/oxabl-project/oxabl/commit/068125785a379b1ba5d0a5ec59b71138841d5312))
* **api:** panic-safe parse, analyze, and format entry points, and browser crash recovery ([#135](https://github.com/oxabl-project/oxabl/issues/135)) ([2e87258](https://github.com/oxabl-project/oxabl/commit/2e8725842c6ed1266607b36023a6b58f4c289ad0))
* **ast:** comment side-table on Program + blank-line detection ([#92](https://github.com/oxabl-project/oxabl/issues/92)) ([2d6b92e](https://github.com/oxabl-project/oxabl/commit/2d6b92e9f4e4770cbb5bdef70778345760d8d459))
* **lint:** add block-var-used-outside advisory (LINT0005) ([#123](https://github.com/oxabl-project/oxabl/issues/123)) ([e870d5b](https://github.com/oxabl-project/oxabl/commit/e870d5bb228cf442af933d9c257ab8b569bba726))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **oxabl_semantic:** add crate skeleton and declare pass (Phase 3) ([c3db693](https://github.com/oxabl-project/oxabl/commit/c3db69311129aa3a79143112f1975a7f828da99d))
* **oxabl_semantic:** crate skeleton and declare pass (Phase 3) ([f155645](https://github.com/oxabl-project/oxabl/commit/f155645048552a34860a0074527b1777a3c12550))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v0.4.0...oxabl_common-v0.5.0) (2026-04-16)


### Features

* **common:** add FileId, FileSet, FileSpan, and Diagnostic types ([4df4d45](https://github.com/oxabl-project/oxabl/commit/4df4d45443a18b890eced304cb2e65b11151fe63))


### Bug Fixes

* **cargo:** bump versions ([1ac918e](https://github.com/oxabl-project/oxabl/commit/1ac918eec08c73ddc04ada8e1e16a6956cd3c102))

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v0.3.0...oxabl_common-v0.4.0) (2026-04-13)


### Features

* **bench:** expand benchmark suite to cover parser, lexer scenarios, and source map ([18c6681](https://github.com/oxabl-project/oxabl/commit/18c6681ab28e2097fdff15c647841d95ff49fb51))
* **bench:** expand benchmark suite to parser, lexer scenarios, and source map ([36abc32](https://github.com/oxabl-project/oxabl/commit/36abc32aebd88514f1fcd203b9da0c9a4eb3d88a))

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v0.2.0...oxabl_common-v0.3.0) (2026-04-03)


### Features

* **workspace:** Add publish job to release workflow and add required fields to crates ([e819219](https://github.com/oxabl-project/oxabl/commit/e81921955ff078af912aa3e590673ffe3ed576fa))
* **workspace:** Add publish job to release workflow and add required… ([439b2f5](https://github.com/oxabl-project/oxabl/commit/439b2f5397bc899948fd38227fe98f7b667c18db))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_common-v0.1.0...oxabl_common-v0.2.0) (2026-04-03)


### Features

* **common:** Added common lib with Source Map ([b1ae472](https://github.com/oxabl-project/oxabl/commit/b1ae472e081cc8b01f86d3e14f255e96ad63fe9f))


### Bug Fixes

* **common:** Make doc test runnable ([e8d2102](https://github.com/oxabl-project/oxabl/commit/e8d210256fae6418c40fb4e1a38abc863b796f09))
* **workspace:** release please and circular dependency ([3118dad](https://github.com/oxabl-project/oxabl/commit/3118dada817296527564fdea053a8c036b8c0ed3))
