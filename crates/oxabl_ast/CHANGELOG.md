# Changelog

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_ast-v0.1.0...oxabl_ast-v0.2.0) (2026-04-03)


### Features

* **ast,parser:** add initial AST literal types and token conversion ([f4ce601](https://github.com/oxabl-project/oxabl/commit/f4ce601f564e5d89e15f8d983cb459e3ff0e968a))
* **ast:** Add minus, mulitply, divide ([666d0d5](https://github.com/oxabl-project/oxabl/commit/666d0d50959d95457ed4520e310ab0443960b7aa))
* **ast:** Added RUN statement to AST ([d56872b](https://github.com/oxabl-project/oxabl/commit/d56872b42a69961b2104622034316ef9591e8cb5))
* **ast:** Added variable declaration and data types ([a1e5978](https://github.com/oxabl-project/oxabl/commit/a1e5978919b0bcf946555976a4a7a7bceaf36b5a))
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
* **parser:** Add PERSISTENT, ASYNCHRONOUS, INPUT-OUTPUT, and RUN IN SUPER support ([facd229](https://github.com/oxabl-project/oxabl/commit/facd229d7ad6bea05d86cb310b3c757e37129a3d))
* **parser:** Began expressions ([1aeaf88](https://github.com/oxabl-project/oxabl/commit/1aeaf88d157e50b026e18ab728453a0a3e00363e))
* **parser:** Complete RUN statement parsing with tests ([3fe1347](https://github.com/oxabl-project/oxabl/commit/3fe134791ffe72f65971ef248647070d70a988df))
* **parser:** Complete RUN statement support ([1cc7fa5](https://github.com/oxabl-project/oxabl/commit/1cc7fa5eef4222c1fba4346e5287e07f1f3aab23))
* **parser:** Finished token to literal, added tests for each literal type ([236b230](https://github.com/oxabl-project/oxabl/commit/236b230b7cce84f329719a6bbaded04160898cce))
* **parser:** Finished token to literal, added tests for each literal type ([#2](https://github.com/oxabl-project/oxabl/issues/2)) ([9846c9d](https://github.com/oxabl-project/oxabl/commit/9846c9d1d5ac246e000d98d976af4245b31d0258))
* **parser:** Modulo, Identifiers, Comparison, Logical ([b59575b](https://github.com/oxabl-project/oxabl/commit/b59575bc0b78337327938c942120680e3ff8d1f0))
* **parser:** Parse ternary, %, more tests ([2e88e02](https://github.com/oxabl-project/oxabl/commit/2e88e02952d794f8bc2b4496073b874a6c5b9c50))
* **paser, lexer, ast:** WIP, adding for each and lock type support ([81e12b2](https://github.com/oxabl-project/oxabl/commit/81e12b2cdb5890a97c588de12c958d17c01780db))


### Bug Fixes

* **parser,ast:** Restore green build — rename typo, remove debug prints, wire RUN dispatch ([f0f6db0](https://github.com/oxabl-project/oxabl/commit/f0f6db0cdc9b92d47264f78abc06ada6fadc964f))
