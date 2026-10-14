# Changelog

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
