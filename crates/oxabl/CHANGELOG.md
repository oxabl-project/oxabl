# Changelog

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl-v1.1.0...oxabl-v1.1.1) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
    * oxabl_preprocessor bumped from 0.5.0 to 0.5.1
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
    * oxabl_lint bumped from 1.0.1 to 1.0.2
    * oxabl_lsp bumped from 0.1.2 to 1.0.0
    * oxabl_daemon bumped from 0.1.1 to 0.2.0
    * oxabl_formatter bumped from 0.1.2 to 1.0.0
    * oxabl_style bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_daemon_protocol bumped from 0.1.0 to 0.2.0
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v1.0.0...oxabl-v1.1.0) (2026-08-12)


### Features

* answer who depends on this, and what must rebuild ([#164](https://github.com/oxabl-project/oxabl/issues/164)) ([9284e18](https://github.com/oxabl-project/oxabl/commit/9284e1887efb00788532209495108d750b1a97b8))
* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_workspace bumped from 1.0.0 to 1.0.1
    * oxabl_preprocessor bumped from 0.4.0 to 0.5.0
    * oxabl_semantic bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.0 to 0.2.1
    * oxabl_analyze bumped from 1.0.0 to 1.1.0
    * oxabl_pipeline bumped from 1.0.0 to 1.1.0
    * oxabl_lint bumped from 1.0.0 to 1.0.1
    * oxabl_lsp bumped from 0.1.1 to 0.1.2
    * oxabl_daemon bumped from 0.1.0 to 0.1.1
    * oxabl_formatter bumped from 0.1.1 to 0.1.2
  * dev-dependencies
    * oxabl_analyze bumped from 1.0.0 to 1.1.0
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_schema bumped from 0.2.0 to 0.2.1
    * oxabl_workspace bumped from 1.0.0 to 1.0.1
    * oxabl_pipeline bumped from 1.0.0 to 1.1.0

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v0.5.0...oxabl-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.
* **semantic:** UnresolvedReason gains NotFoundInWorkspace and Unknowable. An exhaustive match on it will no longer compile. Both are skip-listed by every lint rule, so no diagnostic changes.
* **pipeline:** `oxabl check` no longer reports parse conformance. It lints and reports formatting drift, and its --json shape has changed accordingly. The previous behaviour, report text, --json shape, and exit codes are available unchanged as `oxabl conformance`.
* **api:** `oxabl::parse`, `oxabl::analyze`, `oxabl::analyze_with_fs`, and `oxabl_formatter::format_source` are deprecated in favor of their `try_*` siblings. They still compile and behave identically; callers denying deprecation warnings will need to migrate or allow them.
* **api:** the `oxabl` umbrella no longer glob-re-exports its sub-crates at the crate root; consumers use the named modules (e.g. `oxabl::ast::Statement`). `oxabl_parser` no longer exposes its `skip_to_*` recovery helpers or its `expressions`/`statements` modules.

### Features

* add find_statements example to extract AST statements as CSV ([#76](https://github.com/oxabl-project/oxabl/issues/76)) ([ddeffa2](https://github.com/oxabl-project/oxabl/commit/ddeffa21fd7fe6a683b63a59cc46eadb218781c5))
* **api:** analyze() convenience, AnalyzeOptions, and schema conveniences ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#115](https://github.com/oxabl-project/oxabl/issues/115)) ([3b4dd6e](https://github.com/oxabl-project/oxabl/commit/3b4dd6ec3e72a246019f2099c383b6a11350a15f))
* **api:** curated public facade + parse() convenience ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#113](https://github.com/oxabl-project/oxabl/issues/113)) ([dd496e0](https://github.com/oxabl-project/oxabl/commit/dd496e080212462b2d7463dd172a59806764b589))
* **api:** diagnostic renderer, Display, and serde on the diagnostic family ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#114](https://github.com/oxabl-project/oxabl/issues/114)) ([0681257](https://github.com/oxabl-project/oxabl/commit/068125785a379b1ba5d0a5ec59b71138841d5312))
* **api:** format_source() convenience and a streaming lexer iterator ([#55](https://github.com/oxabl-project/oxabl/issues/55)) ([#116](https://github.com/oxabl-project/oxabl/issues/116)) ([f285ca0](https://github.com/oxabl-project/oxabl/commit/f285ca096b6575df560b3216d3f2f4a855101c61))
* **api:** panic-safe parse, analyze, and format entry points, and browser crash recovery ([#135](https://github.com/oxabl-project/oxabl/issues/135)) ([2e87258](https://github.com/oxabl-project/oxabl/commit/2e8725842c6ed1266607b36023a6b58f4c289ad0))
* **ast:** full-fidelity spans on AST wrapper nodes ([#91](https://github.com/oxabl-project/oxabl/issues/91)) ([2e774b1](https://github.com/oxabl-project/oxabl/commit/2e774b1338bd9c9ff0db014e2cdc238db10801a7))
* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([6447178](https://github.com/oxabl-project/oxabl/commit/64471788cd3436c8f62fe64b8f9b5d6684c03550))
* auto-load oxabl.toml include paths + loud unresolvable-include diagnostic ([bf346d8](https://github.com/oxabl-project/oxabl/commit/bf346d80b5e9d9b390a7fa3ffa4e38c6dbd7023f))
* **cli:** oxabl format CLI + oxabl.toml [workspace.style] discovery (Track B slice 4) ([#94](https://github.com/oxabl-project/oxabl/issues/94)) ([a290fed](https://github.com/oxabl-project/oxabl/commit/a290fedee88b31e21e668b992c35a494428df39a))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **oxabl_analyze:** add dump crate + oxabl analyze subcommand (Phase 6) ([47153ea](https://github.com/oxabl-project/oxabl/commit/47153eae53f9b1306dfa5bc50b919c736ca3a2ed))
* **pipeline:** one shared lint and format run behind every client ([#140](https://github.com/oxabl-project/oxabl/issues/140)) ([58d961e](https://github.com/oxabl-project/oxabl/commit/58d961e7d9b77e8887c78ee69580e60cc07f279f))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))
* **semantic:** the cross-file resolution seam, with the rules held still ([#146](https://github.com/oxabl-project/oxabl/issues/146)) ([3cee2c0](https://github.com/oxabl-project/oxabl/commit/3cee2c0a032c3e9d8176ca2740ed9c065e84824e))
* **vscode:** LSP client extension for format-on-save and diagnostics ([#104](https://github.com/oxabl-project/oxabl/issues/104)) ([cff70d7](https://github.com/oxabl-project/oxabl/commit/cff70d736da22dc8811293dad5539e7899a9275e))


### Bug Fixes

* ADM2 mid-line &IF ([#65](https://github.com/oxabl-project/oxabl/issues/65)) and xp-property BUFFER-FIELD path ([#66](https://github.com/oxabl-project/oxabl/issues/66)) ([#67](https://github.com/oxabl-project/oxabl/issues/67)) ([872c741](https://github.com/oxabl-project/oxabl/commit/872c7411bdd0d9d8a91a695993fbc390f159ebe6))
* correct AST span-seeding placement (supersedes merged version) ([#88](https://github.com/oxabl-project/oxabl/issues/88)) ([7172c66](https://github.com/oxabl-project/oxabl/commit/7172c66c89b413b513833a97699fb85404ff05a9))
* expand undefined {&macro} references to empty string ([3397101](https://github.com/oxabl-project/oxabl/commit/3397101f82cbf5cb405dd9c2a954c25896a4d403))
* resolve PR [#140](https://github.com/oxabl-project/oxabl/issues/140) review findings ([#141](https://github.com/oxabl-project/oxabl/issues/141)) ([dd0849e](https://github.com/oxabl-project/oxabl/commit/dd0849ea72d38ee68c404af1d2414b093ef64459))
* **semantic:** accumulate read/write counts on schema field symbols ([#60](https://github.com/oxabl-project/oxabl/issues/60)) ([#112](https://github.com/oxabl-project/oxabl/issues/112)) ([c70c768](https://github.com/oxabl-project/oxabl/commit/c70c7681323801dcfc2189e66590a7aa1f8403f3))
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 0.6.0 to 1.0.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
    * oxabl_preprocessor bumped from 0.3.1 to 0.4.0
    * oxabl_semantic bumped from 0.1.0 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
    * oxabl_analyze bumped from 0.1.0 to 1.0.0
    * oxabl_pipeline bumped from 0.1.0 to 1.0.0
    * oxabl_lint bumped from 0.1.0 to 1.0.0
    * oxabl_lsp bumped from 0.1.0 to 0.1.1
    * oxabl_formatter bumped from 0.1.0 to 0.1.1
  * dev-dependencies
    * oxabl_analyze bumped from 0.1.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
    * oxabl_pipeline bumped from 0.1.0 to 1.0.0

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
