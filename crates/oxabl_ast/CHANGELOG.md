# Changelog

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_ast-v0.4.0...oxabl_ast-v0.5.0) (2026-04-13)


### Features

* **ast:** add dataset/data-source AST types and lexer keywords ([96d0416](https://github.com/oxabl-project/oxabl/commit/96d041669d0b2c4b20d14fe40503b11079e4b345))
* **parser:** add database manipulation statements ([8428b93](https://github.com/oxabl-project/oxabl/commit/8428b93b8848864fc52ec6f95310184130f7c3a8))
* **parser:** add database manipulation statements ([404e85c](https://github.com/oxabl-project/oxabl/commit/404e85c91a57f5f7244cb1680f05d4f5ed0bb1e7))
* **parser:** add dataset and data-source parsing support ([ae26e42](https://github.com/oxabl-project/oxabl/commit/ae26e428db7022c864bde37b877e1f7021ca1b0c))
* **parser:** add dataset benchmark fixture and apply formatting ([e2510d5](https://github.com/oxabl-project/oxabl/commit/e2510d582b67d7906fde12ba94a91ba341c424c4))
* **parser:** Add include file reference parsing ([28cba6b](https://github.com/oxabl-project/oxabl/commit/28cba6b250247e2936c3a19e711465145189e7fe))
* **parser:** add ON trigger and TRIGGER PROCEDURE statement parsing ([0ec08d3](https://github.com/oxabl-project/oxabl/commit/0ec08d30a6f927ff11e54a0cfc2c53c68e8d0af6))
* **parser:** add ON trigger and TRIGGER PROCEDURE statement parsing ([6dfffdc](https://github.com/oxabl-project/oxabl/commit/6dfffdcac671142290109f13294499ba14e8fac7))
* **parser:** add OO-ABL support ([5c3b2fb](https://github.com/oxabl-project/oxabl/commit/5c3b2fb9269db4dfeaca7d9d041dd8f9424f8bdf))
* **parser:** add OO-ABL support (CLASS, METHOD, PROPERTY, INTERFACE) ([f2ec7a5](https://github.com/oxabl-project/oxabl/commit/f2ec7a5587330fd7c7a0909c752c0478e78fae02))
* **parser:** add preprocessor statement parsing ([a2f82fd](https://github.com/oxabl-project/oxabl/commit/a2f82fd7032dda5fc7c27c3dccead1cf652eec17))
* **parser:** add preprocessor statement parsing ([eea5606](https://github.com/oxabl-project/oxabl/commit/eea560646d49c24614f9bce123432523781defe1))
* **parser:** add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT parsing ([4eb3af4](https://github.com/oxabl-project/oxabl/commit/4eb3af451f99bf02512e5799b9c25bfd1d0336e7))
* **parser:** add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT parsing ([233af8c](https://github.com/oxabl-project/oxabl/commit/233af8ca8df241e00606241a38b8aaed5e86871c))
* **parser:** add stream and frame parsing ([e959ffd](https://github.com/oxabl-project/oxabl/commit/e959ffd74e5a2459800eb076a4e19547d3b3ae70))
* **parser:** add stream and frame parsing ([8182d4e](https://github.com/oxabl-project/oxabl/commit/8182d4e1b952d7de365348469b9d69ba2a9e599c))
* **parser:** handle real-world ABL patterns from oe100fi.p ([15b4405](https://github.com/oxabl-project/oxabl/commit/15b4405e906e6d4dd1c9853590885a212db12a47))
* **parser:** Support LIKE syntax in DEFINE VARIABLE, DEFINE PARAMETER, and VAR ([edf3b84](https://github.com/oxabl-project/oxabl/commit/edf3b8463dc6b32afb564352546b4312877fdcf8))
* **parser:** Support LIKE syntax in DEFINE VARIABLE, DEFINE PARAMETER, and VAR ([dbb1fdf](https://github.com/oxabl-project/oxabl/commit/dbb1fdf325c9dc77ecd89437d74ac2cf03986021))


### Bug Fixes

* **ast, lexer:** resolved some code breakage from the last merge ([f7b44d3](https://github.com/oxabl-project/oxabl/commit/f7b44d37474515ff00c0865d3a6a462af7966ff8))
* **parser:** EXTERNAL procedure, RETURN param, BREAK BY, identifier keywords ([fa2c220](https://github.com/oxabl-project/oxabl/commit/fa2c220d44a17c0cf293d1dcabf0923431d1298e))
* **parser:** handle VALUE(), class types, CREATE BUFFER/QUERY, BUFFER-COPY ASSIGN ([2a9a869](https://github.com/oxabl-project/oxabl/commit/2a9a8693471ebbac6c7dde248c5a4308608a725b))


### Performance Improvements

* **lexer,parser:** pre-allocate token Vec, SmallVec for AssignPair, ternary fast-path ([230fff9](https://github.com/oxabl-project/oxabl/commit/230fff913a04cdd4c5114769575a1e334a09e1ae))
* **lexer,parser:** pre-allocate token Vec, SmallVec for AssignPair, ternary fast-path ([e1f1e37](https://github.com/oxabl-project/oxabl/commit/e1f1e375077644e4ad12bb64156244628f0637d7))

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_ast-v0.3.0...oxabl_ast-v0.4.0) (2026-04-04)


### Features

* **parser:** Add ASSIGN statement and FUNCTION definition support ([5f85a32](https://github.com/oxabl-project/oxabl/commit/5f85a323a572a36b3c2206bbc5eaddb37bcc57f0))
* **parser:** Add ASSIGN statement and FUNCTION definition support ([e1308a1](https://github.com/oxabl-project/oxabl/commit/e1308a1af9f5f4cb90bfb1be4626028d4bfc0970))
* **parser:** Add CATCH, FINALLY, and THROW support ([193a410](https://github.com/oxabl-project/oxabl/commit/193a410fc78869bff108e1316ee58e243809a863))
* **parser:** Add CATCH, FINALLY, and THROW support ([c97bd34](https://github.com/oxabl-project/oxabl/commit/c97bd347d8a629675767aeb93abde8e8ec585f8a))

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_ast-v0.2.0...oxabl_ast-v0.3.0) (2026-04-03)


### Features

* **workspace:** Add publish job to release workflow and add required fields to crates ([e819219](https://github.com/oxabl-project/oxabl/commit/e81921955ff078af912aa3e590673ffe3ed576fa))
* **workspace:** Add publish job to release workflow and add required… ([439b2f5](https://github.com/oxabl-project/oxabl/commit/439b2f5397bc899948fd38227fe98f7b667c18db))

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
