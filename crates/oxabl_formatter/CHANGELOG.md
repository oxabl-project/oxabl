# Changelog

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_formatter-v1.0.1...oxabl_formatter-v1.0.2) (2026-10-07)


### Bug Fixes

* **formatter:** anchor continuation indent on the statement's first line ([326d147](https://github.com/oxabl-project/oxabl/commit/326d1471f898c51be37451a8bdb41b5e314d8949)), closes [#199](https://github.com/oxabl-project/oxabl/issues/199)
* **formatter:** keep the column of a line holding a TAB in a string literal ([a54eebd](https://github.com/oxabl-project/oxabl/commit/a54eebdaa191e2eac41e4766e6b4d8d89c3c007b)), closes [#200](https://github.com/oxabl-project/oxabl/issues/200)
* **formatter:** keep the first-line comment of a file at column zero ([8e65683](https://github.com/oxabl-project/oxabl/commit/8e65683c1fb0fb050fe10362c226e83b1756dfb3)), closes [#204](https://github.com/oxabl-project/oxabl/issues/204)
* **formatter:** nest a statement after WHEN ... THEN or OTHERWISE one level ([6213a62](https://github.com/oxabl-project/oxabl/commit/6213a62c5b172bc62c8c1071a237174a4443db6b)), closes [#203](https://github.com/oxabl-project/oxabl/issues/203)
* **formatter:** restore idempotency and value preservation, and fix four indent bugs ([1ba5c2f](https://github.com/oxabl-project/oxabl/commit/1ba5c2f20790e4aa90ec92fc8b500b9cd880616b))
* **parser:** give CATCH and FINALLY blocks their real source span ([5c9013e](https://github.com/oxabl-project/oxabl/commit/5c9013e70e389863a5ecd4d6d08bc36fb7226109)), closes [#201](https://github.com/oxabl-project/oxabl/issues/201)
* **parser:** skip the FIELDS and EXCEPT phrase of a FOR EACH record ([db20011](https://github.com/oxabl-project/oxabl/commit/db200111d5160a37fca21acc4f2f8a2db526451e)), closes [#202](https://github.com/oxabl-project/oxabl/issues/202)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.3 to 1.0.4

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_formatter-v1.0.0...oxabl_formatter-v1.0.1) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.2 to 1.0.3

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_formatter-v0.1.2...oxabl_formatter-v1.0.0) (2026-08-14)


### ⚠ BREAKING CHANGES

* **api:** `oxabl::parse`, `oxabl::analyze`, `oxabl::analyze_with_fs`, and `oxabl_formatter::format_source` are deprecated in favor of their `try_*` siblings. They still compile and behave identically; callers denying deprecation warnings will need to migrate or allow them.

### Features

* **api:** format_source() convenience and a streaming lexer iterator ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#116](https://github.com/oxabl-project/oxabl/issues/116)) ([f285ca0](https://github.com/oxabl-project/oxabl/commit/f285ca096b6575df560b3216d3f2f4a855101c61))
* **api:** panic-safe parse, analyze, and format entry points, and browser crash recovery ([#135](https://github.com/oxabl-project/oxabl/issues/135)) ([2e87258](https://github.com/oxabl-project/oxabl/commit/2e8725842c6ed1266607b36023a6b58f4c289ad0))
* **cli:** oxabl format CLI + oxabl.toml [workspace.style] discovery (Track B slice 4) ([#94](https://github.com/oxabl-project/oxabl/issues/94)) ([a290fed](https://github.com/oxabl-project/oxabl/commit/a290fedee88b31e21e668b992c35a494428df39a))
* **formatter:** oxabl_formatter layout engine (Track B slice 3) ([#93](https://github.com/oxabl-project/oxabl/issues/93)) ([11a9659](https://github.com/oxabl-project/oxabl/commit/11a9659afd5e62912a34fba93b8945b92a033219))


### Bug Fixes

* credit table reads in buffer, empty-table, and query forms ([#130](https://github.com/oxabl-project/oxabl/issues/130)) ([#138](https://github.com/oxabl-project/oxabl/issues/138)) ([6e39138](https://github.com/oxabl-project/oxabl/commit/6e3913885f513af727b1aee14973ff7936518132))
* **formatter:** keep wrapped multi-line branch at continuation indent ([#98](https://github.com/oxabl-project/oxabl/issues/98)) ([#99](https://github.com/oxabl-project/oxabl/issues/99)) ([2cd193c](https://github.com/oxabl-project/oxabl/commit/2cd193ca5be0840b03559c945947a9c15839af74))
* **formatter:** leave lines that begin inside a multi-line token verbatim ([#96](https://github.com/oxabl-project/oxabl/issues/96)) ([b0de2d3](https://github.com/oxabl-project/oxabl/commit/b0de2d306a592fa44f773dea0dee3dbb78ca3fd7))
* **parser:** stamp full-extent spans on IF/ELSE block branches ([#97](https://github.com/oxabl-project/oxabl/issues/97)) ([6ed9268](https://github.com/oxabl-project/oxabl/commit/6ed926825b5cb44cd7aa2cbed2ef4d622d413186))
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_style bumped from 0.1.0 to 0.2.0
