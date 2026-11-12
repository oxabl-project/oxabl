# Changelog

## [0.3.3](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.3.2...oxabl_effects-v0.3.3) (2026-11-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_flow bumped from 0.3.4 to 0.3.5
    * oxabl_index bumped from 5.0.1 to 5.0.2
    * oxabl_scope bumped from 0.2.4 to 0.2.5
  * dev-dependencies
    * oxabl_parser bumped from 4.0.1 to 4.0.2

## [0.3.2](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.3.1...oxabl_effects-v0.3.2) (2026-11-11)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_flow bumped from 0.3.3 to 0.3.4
    * oxabl_index bumped from 5.0.0 to 5.0.1
    * oxabl_schema bumped from 0.4.3 to 0.4.4
    * oxabl_scope bumped from 0.2.3 to 0.2.4
  * dev-dependencies
    * oxabl_parser bumped from 4.0.0 to 4.0.1
    * oxabl_schema bumped from 0.4.3 to 0.4.4

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.3.0...oxabl_effects-v0.3.1) (2026-11-10)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_index bumped from 4.0.0 to 5.0.0

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.2.2...oxabl_effects-v0.3.0) (2026-11-06)


### Features

* **build:** verify compile results and cache them in a local cache ([f87a7b0](https://github.com/oxabl-project/oxabl/commit/f87a7b04a2784c79dc74f621e993b3ec49b10f06))
* **build:** verify compile results and cache them in a local cache ([61ba98e](https://github.com/oxabl-project/oxabl/commit/61ba98e6b856bdc08bb5814bf09c090a9ca15010))


### Bug Fixes

* **build:** a &gt; inside an XREF-XML attribute value does not end the element ([3874bf3](https://github.com/oxabl-project/oxabl/commit/3874bf3db908cfc82ee636f45e189dfd03fae130))
* **build:** a &gt; inside an XREF-XML attribute value does not end the element ([ae5fbe6](https://github.com/oxabl-project/oxabl/commit/ae5fbe65ca2a05e99e4fc441d11a8a8dbd6385e5))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 3.0.0 to 4.0.0
    * oxabl_flow bumped from 0.3.2 to 0.3.3
    * oxabl_index bumped from 3.1.0 to 4.0.0
    * oxabl_lexer bumped from 2.0.0 to 2.0.1
    * oxabl_schema bumped from 0.4.2 to 0.4.3
    * oxabl_scope bumped from 0.2.2 to 0.2.3
  * dev-dependencies
    * oxabl_parser bumped from 3.0.2 to 4.0.0
    * oxabl_schema bumped from 0.4.2 to 0.4.3

## [0.2.2](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.2.1...oxabl_effects-v0.2.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_flow bumped from 0.3.1 to 0.3.2
    * oxabl_index bumped from 3.0.1 to 3.1.0
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_scope bumped from 0.2.1 to 0.2.2
  * dev-dependencies
    * oxabl_parser bumped from 3.0.1 to 3.0.2
    * oxabl_schema bumped from 0.4.1 to 0.4.2

## [0.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.2.0...oxabl_effects-v0.2.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_flow bumped from 0.3.0 to 0.3.1
    * oxabl_index bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_scope bumped from 0.2.0 to 0.2.1
  * dev-dependencies
    * oxabl_parser bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.1.0...oxabl_effects-v0.2.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** add oxabl_effects, the database effects and likely index per unit ([0d11099](https://github.com/oxabl-project/oxabl/commit/0d11099ad81f0c980040595c0c169547c1def77a))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
