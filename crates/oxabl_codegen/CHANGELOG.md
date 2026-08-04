# Changelog

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_codegen-v0.4.0...oxabl_codegen-v0.5.0) (2026-08-04)


### Features

* **ast:** full-fidelity spans on AST wrapper nodes ([#91](https://github.com/oxabl-project/oxabl/issues/91)) ([2e774b1](https://github.com/oxabl-project/oxabl/commit/2e774b1338bd9c9ff0db014e2cdc238db10801a7))
* close residual LINT0001 language-coverage gaps ([#58](https://github.com/oxabl-project/oxabl/issues/58)) ([b3124fd](https://github.com/oxabl-project/oxabl/commit/b3124fdf3fe6c60472e94e5d6fa4f63b3e84d1a1))
* **formatter:** oxabl_formatter layout engine (Track B slice 3) ([#93](https://github.com/oxabl-project/oxabl/issues/93)) ([11a9659](https://github.com/oxabl-project/oxabl/commit/11a9659afd5e62912a34fba93b8945b92a033219))
* resolve abbreviated built-in function calls in undefined-symbol lint ([3f45edd](https://github.com/oxabl-project/oxabl/commit/3f45edd11e2f33f6c924a14d1d43c772ae8bcd47))
* resolve abbreviated built-in function calls in undefined-symbol lint ([7f763a3](https://github.com/oxabl-project/oxabl/commit/7f763a39c99d66e8f1e45d5e598ab9d18f197f66))


### Bug Fixes

* resolve built-in ABL functions in undefined-symbol lint ([c47b55a](https://github.com/oxabl-project/oxabl/commit/c47b55a694ec62297359d96b3bd36881965abc43)), closes [#58](https://github.com/oxabl-project/oxabl/issues/58)
* silence LINT0001 for MAX/MIN/ABS/VALUE, NO-APPLY, lock method args ([1e997d2](https://github.com/oxabl-project/oxabl/commit/1e997d201e9d42a0240ca9958523c9c98085e4f4))

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_codegen-v0.3.0...oxabl_codegen-v0.4.0) (2026-04-13)


### Features

* **parser:** add preprocessor statement parsing ([a2f82fd](https://github.com/oxabl-project/oxabl/commit/a2f82fd7032dda5fc7c27c3dccead1cf652eec17))
* **parser:** add preprocessor statement parsing ([eea5606](https://github.com/oxabl-project/oxabl/commit/eea560646d49c24614f9bce123432523781defe1))
* **parser:** add stream and frame parsing ([e959ffd](https://github.com/oxabl-project/oxabl/commit/e959ffd74e5a2459800eb076a4e19547d3b3ae70))


### Bug Fixes

* **parser:** handle many real-world ABL patterns ([386fbce](https://github.com/oxabl-project/oxabl/commit/386fbce593b5972586760d3f3f5872d76bf69d9a))


### Performance Improvements

* **lexer:** eliminate heap allocations in keyword matching ([2fa1b87](https://github.com/oxabl-project/oxabl/commit/2fa1b87f9c7bb37ea3fa699a9e4073a34d8c5226))
* **lexer:** eliminate heap allocations in keyword matching ([c7553f7](https://github.com/oxabl-project/oxabl/commit/c7553f7d46f0867c43794e7afdd65eb31795953b))
* **lexer:** use length-dispatched match in match_keyword() ([3a24353](https://github.com/oxabl-project/oxabl/commit/3a243534295d9b3737f056fb107816449ba06c45))

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_codegen-v0.2.0...oxabl_codegen-v0.3.0) (2026-04-03)


### Features

* **workspace:** Add publish job to release workflow and add required fields to crates ([e819219](https://github.com/oxabl-project/oxabl/commit/e81921955ff078af912aa3e590673ffe3ed576fa))
* **workspace:** Add publish job to release workflow and add required… ([439b2f5](https://github.com/oxabl-project/oxabl/commit/439b2f5397bc899948fd38227fe98f7b667c18db))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_codegen-v0.1.0...oxabl_codegen-v0.2.0) (2026-04-03)


### Features

* **codegen, lexer:** New callable function list ([b7fba6f](https://github.com/oxabl-project/oxabl/commit/b7fba6f04d922b02f24d28dc85637d5117a69698))
* **codegen:** Added codegen, resources, and overrides ([c02b4b3](https://github.com/oxabl-project/oxabl/commit/c02b4b3e678ef2a49bbf77632d7f3a54a3a70fa9))
* **codegen:** Massive code gen overhaul ([72a60b5](https://github.com/oxabl-project/oxabl/commit/72a60b52c58aa63cac07899e68155049fbefd5d6))
* **codegen:** Refactor ([4697aab](https://github.com/oxabl-project/oxabl/commit/4697aabd67a73abf0a04a009e6c42a74aaf47618))


### Bug Fixes

* **lexer:** Number Literals ([6c41218](https://github.com/oxabl-project/oxabl/commit/6c412185736f01add009436f8f303c7e0211fe12))
