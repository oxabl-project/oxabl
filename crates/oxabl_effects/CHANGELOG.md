# Changelog

## [0.2.3](https://github.com/oxabl-project/oxabl/compare/oxabl_effects-v0.2.2...oxabl_effects-v0.2.3) (2026-10-30)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_flow bumped from 0.3.2 to 0.3.3
    * oxabl_scope bumped from 0.2.2 to 0.2.3

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
