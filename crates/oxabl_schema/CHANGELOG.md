# Changelog

## [0.4.1](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.4.0...oxabl_schema-v0.4.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 3.0.0 to 3.1.0

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.3.1...oxabl_schema-v0.4.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **schema:** `ParseOutcome` has a new public `sequences` field.

### Features

* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **schema:** parse ADD SEQUENCE and expose sequences on Schema ([b0555f9](https://github.com/oxabl-project/oxabl/commit/b0555f9c8a44b15865a2d5e0a3729ec1725c87be)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)
* **schema:** tell an unknown INITIAL from a quoted question mark ([b30c0e6](https://github.com/oxabl-project/oxabl/commit/b30c0e6815521936c2810f080720cccb82f87d0a))
* **semantic:** resolve abbreviated schema table names ([4a6a265](https://github.com/oxabl-project/oxabl/commit/4a6a26530ca683521f958d4270f0fe8eddd9e6eb))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.3.0...oxabl_schema-v0.3.1) (2026-10-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.3 to 2.0.0

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.2.3...oxabl_schema-v0.3.0) (2026-09-21)


### Features

* **schema:** ship the OpenEdge metaschema so `_file` and friends resolve ([#189](https://github.com/oxabl-project/oxabl/issues/189)) ([0367b60](https://github.com/oxabl-project/oxabl/commit/0367b608273df8e20bb6c89495e27f23d8e50da2)), closes [#151](https://github.com/oxabl-project/oxabl/issues/151)

## [0.2.3](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.2.2...oxabl_schema-v0.2.3) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 1.0.2 to 1.0.3

## [0.2.2](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.2.1...oxabl_schema-v0.2.2) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_workspace bumped from 1.0.1 to 1.0.2

## [0.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.2.0...oxabl_schema-v0.2.1) (2026-08-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_workspace bumped from 1.0.0 to 1.0.1

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_schema-v0.1.0...oxabl_schema-v0.2.0) (2026-08-04)


### Features

* **api:** analyze() convenience, AnalyzeOptions, and schema conveniences ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#115](https://github.com/oxabl-project/oxabl/issues/115)) ([3b4dd6e](https://github.com/oxabl-project/oxabl/commit/3b4dd6ec3e72a246019f2099c383b6a11350a15f))
* **oxabl_schema:** add .df parser and Schema loader (Phase 2) ([52ef860](https://github.com/oxabl-project/oxabl/commit/52ef860c853a60e544cafbf057574efe340cff74))
* **oxabl_schema:** add .df parser and Schema loader (Phase 2) ([3e22f9e](https://github.com/oxabl-project/oxabl/commit/3e22f9e08a071648b1d4f4dd2e17b0196882e7c7))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))


### Bug Fixes

* resolve PR [#140](https://github.com/oxabl-project/oxabl/issues/140) review findings ([#141](https://github.com/oxabl-project/oxabl/issues/141)) ([dd0849e](https://github.com/oxabl-project/oxabl/commit/dd0849ea72d38ee68c404af1d2414b093ef64459))
* **semantic:** resolve unambiguous abbreviated field references ([#82](https://github.com/oxabl-project/oxabl/issues/82)) ([#84](https://github.com/oxabl-project/oxabl/issues/84)) ([f3a340c](https://github.com/oxabl-project/oxabl/commit/f3a340c44bcb3941bfbf02f86ab4459509a43bf1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
