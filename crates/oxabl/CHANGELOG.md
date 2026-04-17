# Changelog

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.4.0...oxabl-v0.5.0) (2026-04-16)


### Features

* **cli:** wire preprocessor into `oxabl check --preprocess` ([4ba5def](https://github.com/oxabl-project/oxabl/commit/4ba5defc9a1fad54022a05a9af6d493d898038a1))
* **workspace:** add oxabl_workspace crate with config, file system, and file discovery ([02b4e67](https://github.com/oxabl-project/oxabl/commit/02b4e672ab34afecc7e0277bfdd90dbc18a529ed))


### Bug Fixes

* **check:** silence &MESSAGE noise and only surface error-level preprocessor diagnostics ([5e2b811](https://github.com/oxabl-project/oxabl/commit/5e2b811c009f62eb96718b226a5ed1c0ec2a203c))
* **parser:** handle implicit output, widget attrs, bare field access, and assign gaps ([ed66678](https://github.com/oxabl-project/oxabl/commit/ed66678bab364f9deb77e7c96f725404fa3e789a))
* **preprocessor:** handle whitespace after include name, surface diagnostics ([f1510fe](https://github.com/oxabl-project/oxabl/commit/f1510fe346bb7ba10a575e62f6e3e12b75732f6d))
* **preprocessor:** skip .i files in check command and preserve undefined refs ([84ff0a2](https://github.com/oxabl-project/oxabl/commit/84ff0a2850d9344758be60ba61cb8f9f96161183))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 0.5.0 to 0.6.0
    * oxabl_lexer bumped from 0.4.0 to 0.4.1
    * oxabl_common bumped from 0.4.0 to 0.5.0
    * oxabl_workspace bumped from 0.3.0 to 0.4.0
    * oxabl_preprocessor bumped from 0.3.0 to 0.3.1

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.3.2...oxabl-v0.4.0) (2026-04-13)


### Features

* **cli:** Add codebase validation CLI with `oxabl check` ([c31438c](https://github.com/oxabl-project/oxabl/commit/c31438c730c2aaaa87d6da1e41ad3a28a2c66a57))
* **parser:** Support LIKE syntax in DEFINE VARIABLE, DEFINE PARAMETER, and VAR ([edf3b84](https://github.com/oxabl-project/oxabl/commit/edf3b8463dc6b32afb564352546b4312877fdcf8))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 0.4.0 to 0.5.0
    * oxabl_lexer bumped from 0.3.1 to 0.4.0
    * oxabl_common bumped from 0.3.0 to 0.4.0
    * oxabl_ast bumped from 0.4.0 to 0.5.0

## [0.3.2](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.3.1...oxabl-v0.3.2) (2026-04-04)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 0.3.1 to 0.4.0
    * oxabl_ast bumped from 0.3.0 to 0.4.0

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.3.0...oxabl-v0.3.1) (2026-04-03)


### Bug Fixes

* add versions to deps ([0d1b3d9](https://github.com/oxabl-project/oxabl/commit/0d1b3d951b3ce8b2f617c9b328644fad2ce1a551))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 0.3.0 to 0.3.1
    * oxabl_lexer bumped from 0.3.0 to 0.3.1

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.2.0...oxabl-v0.3.0) (2026-04-03)


### Features

* **workspace:** Add publish job to release workflow and add required fields to crates ([e819219](https://github.com/oxabl-project/oxabl/commit/e81921955ff078af912aa3e590673ffe3ed576fa))
* **workspace:** Add publish job to release workflow and add required… ([439b2f5](https://github.com/oxabl-project/oxabl/commit/439b2f5397bc899948fd38227fe98f7b667c18db))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.1.0...oxabl-v0.2.0) (2026-04-03)


### Features

* **ast,parser:** add initial AST literal types and token conversion ([f4ce601](https://github.com/oxabl-project/oxabl/commit/f4ce601f564e5d89e15f8d983cb459e3ff0e968a))
* **common:** Added common lib with Source Map ([b1ae472](https://github.com/oxabl-project/oxabl/commit/b1ae472e081cc8b01f86d3e14f255e96ad63fe9f))
* **oxabl:** Initial commit, includes oxabl, oxabl_lexer, oxable_parser ([a21760b](https://github.com/oxabl-project/oxabl/commit/a21760b5b749d3ff1f810c42ec941433d02fe675))
