# Changelog

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v0.1.0...oxabl_parser-v0.2.0) (2026-04-03)


### Features

* **ast,parser:** add initial AST literal types and token conversion ([f4ce601](https://github.com/oxabl-project/oxabl/commit/f4ce601f564e5d89e15f8d983cb459e3ff0e968a))
* **oxabl:** Initial commit, includes oxabl, oxabl_lexer, oxable_parser ([a21760b](https://github.com/oxabl-project/oxabl/commit/a21760b5b749d3ff1f810c42ec941433d02fe675))
* **parser, ast:** Added IF statements ([27f374b](https://github.com/oxabl-project/oxabl/commit/27f374b221c953f1f534a1776c0116c2d1fb6fb5))
* **parser, ast:** Added loop control ([2c88f00](https://github.com/oxabl-project/oxabl/commit/2c88f00c1edf86144ba996fa305a0898c42de9e7))
* **parser, ast:** Added postfix expressions ([35a1336](https://github.com/oxabl-project/oxabl/commit/35a1336ec9068408d6ceaccb5128b685f68533d4))
* **parser, ast:** Added support for CASE statements ([26f56e8](https://github.com/oxabl-project/oxabl/commit/26f56e81b8e35dbc7d78f3abc8c0466778786e0a))
* **parser, ast:** Began procedure parsing support ([6d6af23](https://github.com/oxabl-project/oxabl/commit/6d6af2373d13d71b0c9f5e708713891e87968d48))
* **parser, ast:** Do block and loop parsing ([f572747](https://github.com/oxabl-project/oxabl/commit/f5727477de3ecd3ab5ec2388e1b92739d146c5b7))
* **parser, ast:** Function Calls ([13c3460](https://github.com/oxabl-project/oxabl/commit/13c34602d03c20fc1adc493ec63fdc5bec7e4ce7))
* **parser, ast:** Simple statement support ([23ff400](https://github.com/oxabl-project/oxabl/commit/23ff4005a2f65d67990aec25a1bad40cc14894ad))
* **parser,ast:** Find statement support ([00db56e](https://github.com/oxabl-project/oxabl/commit/00db56ed11836cd04bd1717d3e8bd1914d3aa0cc))
* **parser,expressions:** Parentheses, Unary, literals ([513aabf](https://github.com/oxabl-project/oxabl/commit/513aabf2873432815b4b9f950cbecfab3e3fdb59))
* **parser:** Add DISPLAY and MESSAGE statement support ([76cdf7b](https://github.com/oxabl-project/oxabl/commit/76cdf7b6287d2cfbc1a387acef428a08cd77c312))
* **parser:** Add parse additive and multiplicative, more tests ([5989a3b](https://github.com/oxabl-project/oxabl/commit/5989a3bfab9757203b908591d458043c20938cad))
* **parser:** Add PERSISTENT, ASYNCHRONOUS, INPUT-OUTPUT, and RUN IN SUPER support ([facd229](https://github.com/oxabl-project/oxabl/commit/facd229d7ad6bea05d86cb310b3c757e37129a3d))
* **parser:** Added variable definition statements ([53b7b1d](https://github.com/oxabl-project/oxabl/commit/53b7b1ddbca915b473f405b39988461f5d9c882c))
* **parser:** Began expressions ([1aeaf88](https://github.com/oxabl-project/oxabl/commit/1aeaf88d157e50b026e18ab728453a0a3e00363e))
* **parser:** Can parse longer expressions ([a275c87](https://github.com/oxabl-project/oxabl/commit/a275c87339c1c16c94b42dd6fa5ea4e9e6555eec))
* **parser:** Complete RUN statement parsing with tests ([3fe1347](https://github.com/oxabl-project/oxabl/commit/3fe134791ffe72f65971ef248647070d70a988df))
* **parser:** Complete RUN statement support ([1cc7fa5](https://github.com/oxabl-project/oxabl/commit/1cc7fa5eef4222c1fba4346e5287e07f1f3aab23))
* **parser:** Created parser.rs with struct and AST navigation methods ([e323e8c](https://github.com/oxabl-project/oxabl/commit/e323e8c342cd5c111453ac79e1ff63acc98ec1fc))
* **parser:** Finished procedure and input/output parameter support ([138a24a](https://github.com/oxabl-project/oxabl/commit/138a24a748e7565a94e5c75565fcb07be704122a))
* **parser:** Finished token to literal, added tests for each literal type ([236b230](https://github.com/oxabl-project/oxabl/commit/236b230b7cce84f329719a6bbaded04160898cce))
* **parser:** Finished token to literal, added tests for each literal type ([#2](https://github.com/oxabl-project/oxabl/issues/2)) ([9846c9d](https://github.com/oxabl-project/oxabl/commit/9846c9d1d5ac246e000d98d976af4245b31d0258))
* **parser:** Modulo, Identifiers, Comparison, Logical ([b59575b](https://github.com/oxabl-project/oxabl/commit/b59575bc0b78337327938c942120680e3ff8d1f0))
* **parser:** Parse ternary, %, more tests ([2e88e02](https://github.com/oxabl-project/oxabl/commit/2e88e02952d794f8bc2b4496073b874a6c5b9c50))
* **parser:** reduce repetative code with expect_kind ([2fcdd4b](https://github.com/oxabl-project/oxabl/commit/2fcdd4bb1ac5e9706034512152cee29245525f0d))
* **parser:** Started on RUN statement support ([31b5c9a](https://github.com/oxabl-project/oxabl/commit/31b5c9ae847408f3eb14fe67df74dbb5063c9431))
* **paser, lexer, ast:** WIP, adding for each and lock type support ([81e12b2](https://github.com/oxabl-project/oxabl/commit/81e12b2cdb5890a97c588de12c958d17c01780db))


### Bug Fixes

* clippy warnings ([9f684c6](https://github.com/oxabl-project/oxabl/commit/9f684c6249468661cb30f177f796a3e6331e2e97))
* **parser,ast,lexer:** Mod, Comparison Operators ([e33134d](https://github.com/oxabl-project/oxabl/commit/e33134d0ec3a725a3e976b8a374d164aac8eb1ac))
* **parser,ast:** Restore green build — rename typo, remove debug prints, wire RUN dispatch ([f0f6db0](https://github.com/oxabl-project/oxabl/commit/f0f6db0cdc9b92d47264f78abc06ada6fadc964f))
* **parser:** Fix test with missing import ([221596f](https://github.com/oxabl-project/oxabl/commit/221596f2896a3d212c7ad563c2bea6c8a3539eeb))
* **parser:** Handle COLUMN-LABEL in DISPLAY items, remove unused import ([66d7a26](https://github.com/oxabl-project/oxabl/commit/66d7a264a93d0d40ae62f586cbfb591771ce5e7c))
