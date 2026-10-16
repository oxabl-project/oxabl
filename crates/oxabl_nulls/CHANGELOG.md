# Changelog

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_nulls-v0.1.0...oxabl_nulls-v0.2.0) (2026-10-16)


### Features

* **nulls:** add oxabl_nulls, the records a failed FIND ... NO-ERROR may leave missing ([a6400ca](https://github.com/oxabl-project/oxabl/commit/a6400ca553a01d5adb1370702a6c7e1b44ea5283))
* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **nulls:** track local unknown values through unit control flow ([3d39621](https://github.com/oxabl-project/oxabl/commit/3d396219ab936312b1e4ca6743847f27a5f0312d))
* **nulls:** treat database fields the schema starts at ? as sources of unknown values ([3b39aba](https://github.com/oxabl-project/oxabl/commit/3b39abaecb3b1477f1a2e26444511e27ca7c6ced))


### Bug Fixes

* **nulls:** a counted loop with constant bounds that run its body exits after an iteration ([356d862](https://github.com/oxabl-project/oxabl/commit/356d862d475d017bb0a4c23536d73c6534d8b68a))
* **nulls:** a literal zero divisor is Unknown, not a source of ? ([f9e4bbf](https://github.com/oxabl-project/oxabl/commit/f9e4bbf36dbdf4807ba9b900a2fece1b98940085))
* **nulls:** forget locals that triggers or a REPEAT loop may write ([7d8a4c3](https://github.com/oxabl-project/oxabl/commit/7d8a4c34f7393b4061f8036a280d3a6f50db7bfa))
* **nulls:** frame fields, per-site dependent finds and unreadable ERROR-STATUS tests ([16b5ce2](https://github.com/oxabl-project/oxabl/commit/16b5ce2abb02fbe2d39ff828822e7949f0422f35))
* **nulls:** make the value transfers monotone and bound both solves ([2268fc1](https://github.com/oxabl-project/oxabl/commit/2268fc18490b07735d6a5004eedf0d5ff4ade8da))
* **nulls:** the OF list of an ON header names widgets, not record reads ([1966e24](https://github.com/oxabl-project/oxabl/commit/1966e242ec18d901da8793495a46bc4a344ab304))
* **scope:** mark a reference in an unknown statement form as opaque for temp-tables too ([d911d47](https://github.com/oxabl-project/oxabl/commit/d911d473d4d41007f4e292cdf597e44c49b1fe59))


### Performance Improvements

* **nulls:** bound the work of the null analyses ([edbd5cb](https://github.com/oxabl-project/oxabl/commit/edbd5cb128509b93fd3f51dee3a939430055b3b2))
* **nulls:** bound the work of the null analyses and report units that fall back ([5cebc88](https://github.com/oxabl-project/oxabl/commit/5cebc88ee58a024e00972a8990f1df7483dd53cc))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
