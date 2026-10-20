# Changelog

## [3.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.0.1...oxabl_lsp-v3.0.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 3.0.1 to 3.0.2
    * oxabl_daemon bumped from 0.5.1 to 0.5.2
    * oxabl_pipeline bumped from 3.1.0 to 3.2.0
    * oxabl_workspace bumped from 3.1.0 to 3.2.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.1 to 0.5.2
    * oxabl_analyze bumped from 3.0.1 to 3.0.2
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_workspace bumped from 3.1.0 to 3.2.0
    * oxabl_pipeline bumped from 3.1.0 to 3.2.0

## [3.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.0.0...oxabl_lsp-v3.0.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 3.0.0 to 3.0.1
    * oxabl_daemon bumped from 0.5.0 to 0.5.1
    * oxabl_pipeline bumped from 3.0.0 to 3.1.0
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.0 to 0.5.1
    * oxabl_analyze bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
    * oxabl_pipeline bumped from 3.0.0 to 3.1.0

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v2.0.0...oxabl_lsp-v3.0.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **analyze:** `oxabl_analyze::IncludeFinding` and `oxabl_analyze::CollectedDiagnostics` are `#[non_exhaustive]`. Code outside the crate can no longer build them with a struct expression or match them without `..`. Use `CollectedDiagnostics::new`; `IncludeFinding` is produced by the crate only.

### Features

* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lsp:** code lens for database effect summaries ([530aa7b](https://github.com/oxabl-project/oxabl/commit/530aa7b1cd76e46d4b25e3e4db6082f299bd99ec))
* **lsp:** show cached database effect summaries as code lenses ([2924b89](https://github.com/oxabl-project/oxabl/commit/2924b89d1fffc950307c8b3453fce42c8ee7c80c))


### Bug Fixes

* **analyze:** mark IncludeFinding and CollectedDiagnostics non-exhaustive ([40a0bae](https://github.com/oxabl-project/oxabl/commit/40a0bae77f1a1c6a6cd4a0886522d719b0076b7a))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_daemon bumped from 0.4.0 to 0.5.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.4.0 to 0.5.0
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v1.0.3...oxabl_lsp-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **preprocessor:** CollectedDiagnostics gains the public parse_recovered field.

### Features

* **preprocessor:** keep unreadable and nested unresolved includes as data ([b56cf8b](https://github.com/oxabl-project/oxabl/commit/b56cf8bb426ff6c6eb1fb983bdc0c8d6a0006085))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.4 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_daemon bumped from 0.3.2 to 0.4.0
    * oxabl_pipeline bumped from 1.2.1 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.3.2 to 0.4.0
    * oxabl_analyze bumped from 1.1.4 to 2.0.0
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
    * oxabl_pipeline bumped from 1.2.1 to 2.0.0

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v1.0.2...oxabl_lsp-v1.0.3) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.3 to 1.1.4
    * oxabl_daemon bumped from 0.3.1 to 0.3.2
    * oxabl_pipeline bumped from 1.2.0 to 1.2.1
  * dev-dependencies
    * oxabl_daemon bumped from 0.3.1 to 0.3.2
    * oxabl_analyze bumped from 1.1.3 to 1.1.4
    * oxabl_pipeline bumped from 1.2.0 to 1.2.1

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v1.0.1...oxabl_lsp-v1.0.2) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.2 to 1.1.3
    * oxabl_daemon bumped from 0.3.0 to 0.3.1
    * oxabl_pipeline bumped from 1.1.2 to 1.2.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.3.0 to 0.3.1
    * oxabl_analyze bumped from 1.1.2 to 1.1.3
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_pipeline bumped from 1.1.2 to 1.2.0

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v1.0.0...oxabl_lsp-v1.0.1) (2026-09-21)


### Bug Fixes

* **daemon:** close the registry's symlink hole and stop the probe taking the lock it tests ([#173](https://github.com/oxabl-project/oxabl/issues/173)) ([3fa0bc2](https://github.com/oxabl-project/oxabl/commit/3fa0bc2c402cab88218372fed7d9c3400148bc05))
* **daemon:** move the socket and lock to the runtime directory, and key discovery on a canonical root ([#186](https://github.com/oxabl-project/oxabl/issues/186)) ([68c6a6f](https://github.com/oxabl-project/oxabl/commit/68c6a6f571013e47d694e7685de1ace222e3b7ce))
* **daemon:** shared session correctness, and the contract bump that covers the series ([#174](https://github.com/oxabl-project/oxabl/issues/174)) ([39bba72](https://github.com/oxabl-project/oxabl/commit/39bba72396c8c4a8b241056b91d44b776d068a31))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_daemon bumped from 0.2.0 to 0.3.0
    * oxabl_daemon_protocol bumped from 0.2.0 to 0.3.0
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_daemon bumped from 0.2.0 to 0.3.0
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v0.1.2...oxabl_lsp-v1.0.0) (2026-08-14)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.
* **pipeline:** `oxabl check` no longer reports parse conformance. It lints and reports formatting drift, and its --json shape has changed accordingly. The previous behaviour, report text, --json shape, and exit codes are available unchanged as `oxabl conformance`.
* **api:** `oxabl::parse`, `oxabl::analyze`, `oxabl::analyze_with_fs`, and `oxabl_formatter::format_source` are deprecated in favor of their `try_*` siblings. They still compile and behave identically; callers denying deprecation warnings will need to migrate or allow them.

### Features

* **api:** format_source() convenience and a streaming lexer iterator ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#116](https://github.com/oxabl-project/oxabl/issues/116)) ([f285ca0](https://github.com/oxabl-project/oxabl/commit/f285ca096b6575df560b3216d3f2f4a855101c61))
* **api:** panic-safe parse, analyze, and format entry points, and browser crash recovery ([#135](https://github.com/oxabl-project/oxabl/issues/135)) ([2e87258](https://github.com/oxabl-project/oxabl/commit/2e8725842c6ed1266607b36023a6b58f4c289ad0))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **lsp:** wire textDocument/formatting to oxabl_formatter ([#100](https://github.com/oxabl-project/oxabl/issues/100)) ([9e536bd](https://github.com/oxabl-project/oxabl/commit/9e536bd5ea3141221323447b9d3ef0a4a8c62dbc))
* **pipeline:** one shared lint and format run behind every client ([#140](https://github.com/oxabl-project/oxabl/issues/140)) ([58d961e](https://github.com/oxabl-project/oxabl/commit/58d961e7d9b77e8887c78ee69580e60cc07f279f))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))
* **vscode:** LSP client extension for format-on-save and diagnostics ([#104](https://github.com/oxabl-project/oxabl/issues/104)) ([cff70d7](https://github.com/oxabl-project/oxabl/commit/cff70d736da22dc8811293dad5539e7899a9275e))


### Bug Fixes

* resolve PR [#140](https://github.com/oxabl-project/oxabl/issues/140) review findings ([#141](https://github.com/oxabl-project/oxabl/issues/141)) ([dd0849e](https://github.com/oxabl-project/oxabl/commit/dd0849ea72d38ee68c404af1d2414b093ef64459))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_daemon bumped from 0.1.1 to 0.2.0
    * oxabl_daemon_protocol bumped from 0.1.0 to 0.2.0
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
    * oxabl_style bumped from 0.1.0 to 0.2.0
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_daemon bumped from 0.1.1 to 0.2.0
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
