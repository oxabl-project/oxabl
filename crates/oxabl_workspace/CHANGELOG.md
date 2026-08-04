# Changelog

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
