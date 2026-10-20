# Changelog

## [0.3.2](https://github.com/oxabl-project/oxabl/compare/oxabl_flow-v0.3.1...oxabl_flow-v0.3.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dev-dependencies
    * oxabl_parser bumped from 3.0.1 to 3.0.2

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_flow-v0.3.0...oxabl_flow-v0.3.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dev-dependencies
    * oxabl_parser bumped from 3.0.0 to 3.0.1

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_flow-v0.2.0...oxabl_flow-v0.3.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* **flow:** end the dataflow solve at loop heads and bound its work ([cecbfb1](https://github.com/oxabl-project/oxabl/commit/cecbfb1a2aeddc68dbec1ce2a72ac652ed141b3c))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))


### Performance Improvements

* **flow:** charge dataflow visits by the work of their fact ([65fdbe4](https://github.com/oxabl-project/oxabl/commit/65fdbe4537a8bfc7ef08e9e5796371a3bb64779d))
* **flow:** read block headers from the parser's tokens ([a1a0f78](https://github.com/oxabl-project/oxabl/commit/a1a0f78a8215fd984637c916ea2b939c2f87822d))
* **nulls:** bound the work of the null analyses ([edbd5cb](https://github.com/oxabl-project/oxabl/commit/edbd5cb128509b93fd3f51dee3a939430055b3b2))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_flow-v0.1.0...oxabl_flow-v0.2.0) (2026-10-14)


### Features

* **flow:** add oxabl_flow, control-flow graphs with ABL block semantics ([99977ee](https://github.com/oxabl-project/oxabl/commit/99977ee8eda92cbfbe3332c150f5981809d46889))
* **flow:** record the buffers a FOR or DO FOR block scopes ([9f04702](https://github.com/oxabl-project/oxabl/commit/9f047029d4d208c3b4e88e3e668760ef5bd3dd10))


### Bug Fixes

* **flow:** keep both outcomes of an action-less UNDO, raise for UNDO values ([7b9193c](https://github.com/oxabl-project/oxabl/commit/7b9193cf464434f6a434570cc77b4f1b10290c5b))
* **flow:** model STOP from STOP-AFTER, calls and lock waits ([9afae86](https://github.com/oxabl-project/oxabl/commit/9afae862ffb1cf3e3794a6ca6189fc87aa316279))
* **flow:** run FOR FIRST and FOR LAST once ([7103b04](https://github.com/oxabl-project/oxabl/commit/7103b04def094fbf0de8fd9c110ea6eb23ea26dc))
* **flow:** send every UNDO statement through the undo path ([c7deaf5](https://github.com/oxabl-project/oxabl/commit/c7deaf5de47ea44024a12932df89ce86cc1038e0))
* **parser:** parse UNDO, RETURN NO-APPLY ([5e03314](https://github.com/oxabl-project/oxabl/commit/5e033140b2d22fc496eee5dc4fd04e6396e137d3))


### Performance Improvements

* **lint:** build one unit and solve once per block for LINT0005 ([4d61b17](https://github.com/oxabl-project/oxabl/commit/4d61b17c80287480aea40a417bc7e4bd93c60c84))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_parser bumped from 1.0.4 to 2.0.0
