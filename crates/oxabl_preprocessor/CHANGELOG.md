# Changelog

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.4.0...oxabl_preprocessor-v0.5.0) (2026-08-12)


### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_workspace bumped from 1.0.0 to 1.0.1

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_preprocessor-v0.3.1...oxabl_preprocessor-v0.4.0) (2026-08-04)


### Features

* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([6447178](https://github.com/oxabl-project/oxabl/commit/64471788cd3436c8f62fe64b8f9b5d6684c03550))
* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([bf346d8](https://github.com/oxabl-project/oxabl/commit/bf346d80b5e9d9b390a7fa3ffa4e38c6dbd7023f))
* **bench:** add preprocessor benchmark suite ([1e14fa7](https://github.com/oxabl-project/oxabl/commit/1e14fa77621a747518290e8ec9137bbaf154ccd8))
* **cli:** wire preprocessor into `oxabl check --preprocess` ([9870f98](https://github.com/oxabl-project/oxabl/commit/9870f98b21d29e4e07760457180e209376caf57a))
* **preprocessor:** add include file argument support (positional and named) ([2591eca](https://github.com/oxabl-project/oxabl/commit/2591eca22af18057ea2e0861c194666422917eb2))
* **preprocessor:** add MATCHES, BEGINS, and keyword operators to condition evaluator ([0bd1d19](https://github.com/oxabl-project/oxabl/commit/0bd1d194dcec92f3b21e25538734ac12ccbea6a5))
* **preprocessor:** add oxabl_preprocessor crate with include expansion, &IF evaluation, and variable tracking ([379389b](https://github.com/oxabl-project/oxabl/commit/379389b83afff049654ea586778b7b8974bde640))
* **preprocessor:** resolve dynamic include names like `{{&frame}.f …}` ([69deb29](https://github.com/oxabl-project/oxabl/commit/69deb2940b147b896dbe9f656028935bb24c8867))


### Bug Fixes

* ADM2 mid-line &IF ([#65](https://github.com/oxabl-project/oxabl/issues/65)) and xp-property BUFFER-FIELD path ([#66](https://github.com/oxabl-project/oxabl/issues/66)) ([#67](https://github.com/oxabl-project/oxabl/issues/67)) ([872c741](https://github.com/oxabl-project/oxabl/commit/872c7411bdd0d9d8a91a695993fbc390f159ebe6))
* allow '.' in preprocessor macro names for include-once guards ([#74](https://github.com/oxabl-project/oxabl/issues/74)) ([#75](https://github.com/oxabl-project/oxabl/issues/75)) ([9f9fd51](https://github.com/oxabl-project/oxabl/commit/9f9fd5193e30ea7d2ddded15305d38475d58476b))
* **check:** silence &MESSAGE noise and only surface error-level preprocessor diagnostics ([e04476f](https://github.com/oxabl-project/oxabl/commit/e04476f291d287e520e353c54e8a226c5b8252d9))
* eliminate unused-variable false positives from block-body parse errors ([#79](https://github.com/oxabl-project/oxabl/issues/79)) ([#80](https://github.com/oxabl-project/oxabl/issues/80)) ([e653c36](https://github.com/oxabl-project/oxabl/commit/e653c365373827ab7ec782776e987331732d8694))
* expand undefined {&macro} references to empty string ([3397101](https://github.com/oxabl-project/oxabl/commit/3397101f82cbf5cb405dd9c2a954c25896a4d403))
* **preprocessor:** downgrade missing-include from Error to Warning ([dc115dd](https://github.com/oxabl-project/oxabl/commit/dc115dddb121cb57b0f416405dbb5c96e681316e))
* **preprocessor:** handle multi-line include arguments and spaces around = ([f7dda13](https://github.com/oxabl-project/oxabl/commit/f7dda132c72dd770bf403a08ddd437df24f67993))
* **preprocessor:** handle whitespace after include name, surface diagnostics ([45ba132](https://github.com/oxabl-project/oxabl/commit/45ba132a5903c7cbbc511974f18d3bfc42329489))
* **preprocessor:** resolve named {&…} refs in define values at define time ([#121](https://github.com/oxabl-project/oxabl/issues/121)) ([84912cc](https://github.com/oxabl-project/oxabl/commit/84912cc004e6b7b95ff06043ce31570fdc6393a3))
* **preprocessor:** skip .i files in check command and preserve undefined refs ([1617ebe](https://github.com/oxabl-project/oxabl/commit/1617ebef94f54b1518187a993b2c2ea7b1fe552a))
* **preprocessor:** skip comment bodies when scanning for directives and includes ([b754eaf](https://github.com/oxabl-project/oxabl/commit/b754eaf13b4a060fb469ff0ed358444402407dd4))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
