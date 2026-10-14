# Changelog

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v1.0.3...oxabl_workspace-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **workspace:** PreprocessorConfig has two new public fields, databases and database_types, so a struct literal that names every field no longer compiles. Build it with `..PreprocessorConfig::default()`.
* **workspace:** LintConfig is no longer Copy and its six fields are replaced by settings (enable, disable, rules) and overrides. set_by_name is replaced by set_rule. LintConfig::default() configures nothing; default severities come from the rule registry.
* **workspace:** PipelineConfig gains the public run_extensions field.
* **workspace:** WorkspaceSection gains the public preprocessor field.

### Features

* **cli:** check context-free strict rules in the include files of a walked tree ([7b931f0](https://github.com/oxabl-project/oxabl/commit/7b931f0770f0149ff22c6f0c35658085682797c5))
* **workspace:** add the [workspace.preprocessor] table ([e6ec64c](https://github.com/oxabl-project/oxabl/commit/e6ec64ce71b20259773cd61f2224f70f6839d19f)), closes [#216](https://github.com/oxabl-project/oxabl/issues/216)
* **workspace:** configure lint groups and per-directory overrides ([df7caab](https://github.com/oxabl-project/oxabl/commit/df7caab473d4d5749ff37282371f406c33f5ee9d))
* **workspace:** configure the extension order for an extensionless RUN ([77c1e5f](https://github.com/oxabl-project/oxabl/commit/77c1e5fdf2dfee4b9264122c8f838310a2af0bc1))
* **workspace:** list the connected databases in [workspace.preprocessor] ([5a6f721](https://github.com/oxabl-project/oxabl/commit/5a6f721de461f6bb48f811707f65a4dff3992ee8))


### Bug Fixes

* **index:** probe every spelling of a run extension and normalize hand-built lists ([94ec29a](https://github.com/oxabl-project/oxabl/commit/94ec29a841e850330176b05130b55b5e14c3d045))
* **lint:** rank group toggles below rule settings and warn on an override with no files ([e31d5f8](https://github.com/oxabl-project/oxabl/commit/e31d5f8295aebeb94fbaff09a1106f185b38ed73))
* **preprocessor:** close the last silent branch choices in &IF evaluation ([5d9fc53](https://github.com/oxabl-project/oxabl/commit/5d9fc53be1ad38b960e3dbf06a070a4843cc154e))
* **preprocessor:** evaluate the functions an &IF names, or say it guessed ([026e29e](https://github.com/oxabl-project/oxabl/commit/026e29e4fc9fff19a17ace4b872f2210d15129da))
* **preprocessor:** treat a database list inferred from schema files as partial ([c26de87](https://github.com/oxabl-project/oxabl/commit/c26de8751a8a8af649c800503e79f9c8e8eb7da1))
* **preprocessor:** treat LDBNAME and DBTYPE of DICTDB as a gap under an inferred database list ([4f9e563](https://github.com/oxabl-project/oxabl/commit/4f9e5635008d93acbf1433aa68e6e906f3fbf05c))
* **workspace:** validate run_extensions at config load ([df81dd9](https://github.com/oxabl-project/oxabl/commit/df81dd992aeb3a7bad07fadabdfb5ec46b5106e1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_ast bumped from 1.0.0 to 2.0.0

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v1.0.2...oxabl_workspace-v1.0.3) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v1.0.1...oxabl_workspace-v1.0.2) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_style bumped from 0.1.0 to 0.2.0

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v1.0.0...oxabl_workspace-v1.0.1) (2026-08-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v0.4.0...oxabl_workspace-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* **pipeline:** `oxabl check` no longer reports parse conformance. It lints and reports formatting drift, and its --json shape has changed accordingly. The previous behaviour, report text, --json shape, and exit codes are available unchanged as `oxabl conformance`.

### Features

* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([6447178](https://github.com/oxabl-project/oxabl/commit/64471788cd3436c8f62fe64b8f9b5d6684c03550))
* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([bf346d8](https://github.com/oxabl-project/oxabl/commit/bf346d80b5e9d9b390a7fa3ffa4e38c6dbd7023f))
* **cli:** oxabl format CLI + oxabl.toml [workspace.style] discovery (Track B slice 4) ([#94](https://github.com/oxabl-project/oxabl/issues/94)) ([a290fed](https://github.com/oxabl-project/oxabl/commit/a290fedee88b31e21e668b992c35a494428df39a))
* **lint:** add block-var-used-outside advisory (LINT0005) ([#123](https://github.com/oxabl-project/oxabl/issues/123)) ([e870d5b](https://github.com/oxabl-project/oxabl/commit/e870d5bb228cf442af933d9c257ab8b569bba726))
* **lint:** credit table parameters, split dead stores into LINT0006 ([#129](https://github.com/oxabl-project/oxabl/issues/129)) ([d539c31](https://github.com/oxabl-project/oxabl/commit/d539c31d7b90cfac43b54a9370adbe5097130268))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **pipeline:** one shared lint and format run behind every client ([#140](https://github.com/oxabl-project/oxabl/issues/140)) ([58d961e](https://github.com/oxabl-project/oxabl/commit/58d961e7d9b77e8887c78ee69580e60cc07f279f))
* **vscode:** LSP client extension for format-on-save and diagnostics ([#104](https://github.com/oxabl-project/oxabl/issues/104)) ([cff70d7](https://github.com/oxabl-project/oxabl/commit/cff70d736da22dc8811293dad5539e7899a9275e))


### Bug Fixes

* resolve PR [#140](https://github.com/oxabl-project/oxabl/issues/140) review findings ([#141](https://github.com/oxabl-project/oxabl/issues/141)) ([dd0849e](https://github.com/oxabl-project/oxabl/commit/dd0849ea72d38ee68c404af1d2414b093ef64459))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_ast bumped from 0.5.0 to 1.0.0

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_workspace-v0.3.0...oxabl_workspace-v0.4.0) (2026-04-16)


### Features

* **workspace:** add oxabl_workspace crate with config, file system, and file discovery ([02b4e67](https://github.com/oxabl-project/oxabl/commit/02b4e672ab34afecc7e0277bfdd90dbc18a529ed))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 0.4.0 to 0.5.0
