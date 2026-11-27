# Changelog

## [4.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.6.0...oxabl_lsp-v4.0.0) (2026-11-27)


### ⚠ BREAKING CHANGES

* **ast:** Identifier.name is a NameId, expression fields are ExprId or ExprList, and the analysis entry points take the program's AstPools.

### Features

* **lsp:** complete the members of a class after obj: ([#620](https://github.com/oxabl-project/oxabl/issues/620)) ([c5f0f8d](https://github.com/oxabl-project/oxabl/commit/c5f0f8d6b232b74b1b83e1242b1cac310f39b779))


### Performance Improvements

* **ast:** keep names and expressions in per-program pools ([#632](https://github.com/oxabl-project/oxabl/issues/632)) ([e6fbebd](https://github.com/oxabl-project/oxabl/commit/e6fbebd1ec47740059c1557fd5e3118a4818df6a))
* **daemon:** keep the subtype index and build it in the background after the first request ([#622](https://github.com/oxabl-project/oxabl/issues/622)) ([57c62aa](https://github.com/oxabl-project/oxabl/commit/57c62aa199dc7781021a9d279fb60bd9102c182d))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 5.0.0 to 6.0.0
    * oxabl_common bumped from 1.6.0 to 2.0.0
    * oxabl_daemon bumped from 0.10.0 to 0.11.0
    * oxabl_daemon_protocol bumped from 0.4.1 to 0.5.0
    * oxabl_pipeline bumped from 5.0.0 to 6.0.0
    * oxabl_style bumped from 0.2.1 to 0.3.0
    * oxabl_workspace bumped from 3.8.0 to 4.0.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.10.0 to 0.11.0
    * oxabl_analyze bumped from 5.0.0 to 6.0.0
    * oxabl_ast bumped from 7.0.0 to 8.0.0
    * oxabl_schema bumped from 0.6.2 to 0.7.0
    * oxabl_common bumped from 1.6.0 to 2.0.0
    * oxabl_workspace bumped from 3.8.0 to 4.0.0
    * oxabl_pipeline bumped from 5.0.0 to 6.0.0

## [3.6.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.5.0...oxabl_lsp-v3.6.0) (2026-11-27)


### Features

* **lsp:** code completion for keywords, snippets and names in scope ([#534](https://github.com/oxabl-project/oxabl/issues/534)) ([e600394](https://github.com/oxabl-project/oxabl/commit/e6003942dd5ee81663503acc3ed0422689f80348))
* **lsp:** complete records, fields, RUN targets, include files and preprocessor names ([#546](https://github.com/oxabl-project/oxabl/issues/546)) ([6d30754](https://github.com/oxabl-project/oxabl/commit/6d30754e74636091f7c90fc981dd98b5e5b2823c))
* **lsp:** declarations from include files in the outline ([#543](https://github.com/oxabl-project/oxabl/issues/543)) ([6cb14cf](https://github.com/oxabl-project/oxabl/commit/6cb14cfa78751724c799ff61bed2a01579e0e285))
* **lsp:** format a selection ([#540](https://github.com/oxabl-project/oxabl/issues/540)) ([b1cedcb](https://github.com/oxabl-project/oxabl/commit/b1cedcbdcae6a809d11f9918358599e39ea60361))
* **lsp:** go to implementation, super method and type hierarchy ([#531](https://github.com/oxabl-project/oxabl/issues/531)) ([578188c](https://github.com/oxabl-project/oxabl/commit/578188ca8f9ce6c59561500e67bddf0964838b13))
* **lsp:** reference-count code lenses ([#532](https://github.com/oxabl-project/oxabl/issues/532)) ([1af858b](https://github.com/oxabl-project/oxabl/commit/1af858b6fdaa262d205abe24d5e34a24b1491ed9))
* **lsp:** signature help and parameter-name inlay hints ([#533](https://github.com/oxabl-project/oxabl/issues/533)) ([eaeab6b](https://github.com/oxabl-project/oxabl/commit/eaeab6be41c16d152b1b3042b34d4244580d8046))


### Bug Fixes

* **check:** report a parse error inside an include at the include site ([#528](https://github.com/oxabl-project/oxabl/issues/528)) ([a5339bb](https://github.com/oxabl-project/oxabl/commit/a5339bb33cdd305f0e8ca2bf6c1676fb91246487))
* **lsp:** an edit no longer waits for a running diagnostics analysis ([#609](https://github.com/oxabl-project/oxabl/issues/609)) ([5a5e83f](https://github.com/oxabl-project/oxabl/commit/5a5e83f26c7f9051de5a62b1e69956fcbad39b24))
* **lsp:** the proxy and the daemon never wait on each other ([#578](https://github.com/oxabl-project/oxabl/issues/578)) ([0563621](https://github.com/oxabl-project/oxabl/commit/056362150cd9572882909b436ece4758ab78d775))
* **lsp:** wait for a slow daemon start ([#545](https://github.com/oxabl-project/oxabl/issues/545)) ([8445706](https://github.com/oxabl-project/oxabl/commit/8445706fc1fa4ecd00a4d70335fef03cfffd8bc7))
* **preprocessor:** fill {N} in a nested include reference inside an include ([6e7e30d](https://github.com/oxabl-project/oxabl/commit/6e7e30d841be0e5853b6626405a70ac6d437e89c))


### Performance Improvements

* **check:** spell and resolve each include path once ([#608](https://github.com/oxabl-project/oxabl/issues/608)) ([793a0d9](https://github.com/oxabl-project/oxabl/commit/793a0d94b98ba7c498b7f941ff9b02e8957cfe0a))
* **lsp:** resolve the reference lenses of a file in one search ([#592](https://github.com/oxabl-project/oxabl/issues/592)) ([91d9306](https://github.com/oxabl-project/oxabl/commit/91d930645dbc1d54ae5e9ec4a3c0eddc194bfc9b))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.5.0 to 5.0.0
    * oxabl_common bumped from 1.5.0 to 1.6.0
    * oxabl_daemon bumped from 0.9.0 to 0.10.0
    * oxabl_pipeline bumped from 4.1.0 to 5.0.0
    * oxabl_workspace bumped from 3.7.1 to 3.8.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.9.0 to 0.10.0
    * oxabl_analyze bumped from 4.5.0 to 5.0.0
    * oxabl_ast bumped from 6.0.0 to 7.0.0
    * oxabl_schema bumped from 0.6.1 to 0.6.2
    * oxabl_common bumped from 1.5.0 to 1.6.0
    * oxabl_workspace bumped from 3.7.1 to 3.8.0
    * oxabl_pipeline bumped from 4.1.0 to 5.0.0

## [3.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.4.0...oxabl_lsp-v3.5.0) (2026-11-25)


### Features

* **lsp:** semantic tokens for the names oxabl resolves ([0e04b4c](https://github.com/oxabl-project/oxabl/commit/0e04b4c154c2811773002c1c2dffe56c86a3151a))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.4.0 to 4.5.0
    * oxabl_common bumped from 1.4.0 to 1.5.0
    * oxabl_daemon bumped from 0.8.0 to 0.9.0
    * oxabl_pipeline bumped from 4.0.0 to 4.1.0
    * oxabl_workspace bumped from 3.7.0 to 3.7.1
  * dev-dependencies
    * oxabl_daemon bumped from 0.8.0 to 0.9.0
    * oxabl_analyze bumped from 4.4.0 to 4.5.0
    * oxabl_schema bumped from 0.6.0 to 0.6.1
    * oxabl_common bumped from 1.4.0 to 1.5.0
    * oxabl_workspace bumped from 3.7.0 to 3.7.1
    * oxabl_pipeline bumped from 4.0.0 to 4.1.0

## [3.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.3.0...oxabl_lsp-v3.4.0) (2026-11-24)


### Features

* **lsp:** dim inactive preprocessor regions ([cc57d5e](https://github.com/oxabl-project/oxabl/commit/cc57d5efd24bbb6b6880abc6276ec0f7460133f6))
* **lsp:** find references and the call hierarchy ([4fbab73](https://github.com/oxabl-project/oxabl/commit/4fbab733b91d8594a30071b5515bdd5b1cf73703))
* **lsp:** folding and selection ranges from the syntax tree ([39ffd97](https://github.com/oxabl-project/oxabl/commit/39ffd97cfc343ee634472870c8d676b42f5200de))
* **lsp:** highlight the other uses of the name under the cursor ([47df7a5](https://github.com/oxabl-project/oxabl/commit/47df7a5a62c4b76358ef6a7c45a4336014561736))
* **lsp:** hover with types, signatures, built-in docs and schema fields ([ca50283](https://github.com/oxabl-project/oxabl/commit/ca502838f6343f5f28a225df5e8b64020b304263))
* **lsp:** inlay hints for locks, index, buffer scope and transactions ([5358467](https://github.com/oxabl-project/oxabl/commit/535846717f49af4de07e31280201cd7f76de8cbd))
* **lsp:** related locations on unknown-value and handle-leak diagnostics ([2bd42ec](https://github.com/oxabl-project/oxabl/commit/2bd42ec174d6051e58327e78277516accbdb172b))
* **lsp:** schema browser for the configured .df files ([09e1661](https://github.com/oxabl-project/oxabl/commit/09e166179a76347d56d8e9b173d9e62f8e4865fb))
* **lsp:** show a routine's database effects on hover ([772b5c6](https://github.com/oxabl-project/oxabl/commit/772b5c6943c456e18f080cb1ab7bc6dc89550e62))
* **lsp:** show the editor a daemon of another version ([4870036](https://github.com/oxabl-project/oxabl/commit/48700369252158265bc3fe75f50741103b869fa5))


### Bug Fixes

* **daemon:** exit after an idle timeout and remove the registration ([94a4214](https://github.com/oxabl-project/oxabl/commit/94a4214272c16c653ce4ed3ae60302c6aa89a22b))
* **lsp:** replace a daemon of another build of the same version ([0285b5d](https://github.com/oxabl-project/oxabl/commit/0285b5d3d9ccff5e64d77956467e15691b3fcc6b))
* **preprocessor:** end an include reference at the first } as the compiler does ([2072df5](https://github.com/oxabl-project/oxabl/commit/2072df58afae02cb78b1861afeeeda065d50ed7a))
* **scope:** the effects of a statement on an inherited buffer or temp-table are attributed ([c27d9e2](https://github.com/oxabl-project/oxabl/commit/c27d9e2c3663a7226f33bbc5ee92398a90f4a3dc))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.3.0 to 4.4.0
    * oxabl_common bumped from 1.3.1 to 1.4.0
    * oxabl_daemon bumped from 0.7.0 to 0.8.0
    * oxabl_daemon_protocol bumped from 0.4.0 to 0.4.1
    * oxabl_pipeline bumped from 3.8.0 to 4.0.0
    * oxabl_workspace bumped from 3.6.2 to 3.7.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.7.0 to 0.8.0
    * oxabl_analyze bumped from 4.3.0 to 4.4.0
    * oxabl_ast bumped from 5.0.0 to 6.0.0
    * oxabl_schema bumped from 0.5.0 to 0.6.0
    * oxabl_common bumped from 1.3.1 to 1.4.0
    * oxabl_workspace bumped from 3.6.2 to 3.7.0
    * oxabl_pipeline bumped from 3.8.0 to 4.0.0

## [3.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.2.0...oxabl_lsp-v3.3.0) (2026-11-23)


### Features

* **lsp:** answer workspace/symbol from the workspace index ([c7f8f90](https://github.com/oxabl-project/oxabl/commit/c7f8f90b7c6dd18b66fc29a0cd311dfe6181aae5))
* **lsp:** answer workspace/symbol from the workspace index ([c6680be](https://github.com/oxabl-project/oxabl/commit/c6680bed6ececab2739fbe4e673660fe5953078c)), closes [#406](https://github.com/oxabl-project/oxabl/issues/406)
* **lsp:** go to definition and document links ([9a89c3d](https://github.com/oxabl-project/oxabl/commit/9a89c3d344cee9ac558d7a9218565496066da4d0))
* **lsp:** go to definition and document links ([1df981d](https://github.com/oxabl-project/oxabl/commit/1df981dfba85c1ffadd26219b36e402a881ef6ca)), closes [#407](https://github.com/oxabl-project/oxabl/issues/407) [#408](https://github.com/oxabl-project/oxabl/issues/408)
* **lsp:** inspect the expanded compilation unit of a file ([faa7bf8](https://github.com/oxabl-project/oxabl/commit/faa7bf81431e7bf794510fa1e95c10bfc6d8c985))
* **lsp:** inspect the expanded compilation unit of a file ([57e65b6](https://github.com/oxabl-project/oxabl/commit/57e65b61801d9a25f57f30c516adf3a0e02f766c)), closes [#169](https://github.com/oxabl-project/oxabl/issues/169)
* **lsp:** select an includer context for an open include ([18a621e](https://github.com/oxabl-project/oxabl/commit/18a621eca5d4de5eb875f3214ae0f319f06f33cc))
* **lsp:** select an includer context for an open include ([99a3291](https://github.com/oxabl-project/oxabl/commit/99a3291874d6628bfe9ae96d234fe7f82543dc1f)), closes [#168](https://github.com/oxabl-project/oxabl/issues/168)


### Bug Fixes

* **daemon:** a reopened buffer takes the path of its new open ([f8bc8b6](https://github.com/oxabl-project/oxabl/commit/f8bc8b62945f6d4144869f2d89652dd11533deed))
* **daemon:** free the analysis of closed buffers ([180f330](https://github.com/oxabl-project/oxabl/commit/180f3301f3e1d556d751aa62390a56f1da5b3400))
* **lsp:** definition of a LIKE field, and null answers through the proxy ([85351b7](https://github.com/oxabl-project/oxabl/commit/85351b77041175e8e00255116b1a12741d8d6f63))
* **lsp:** definition of a LIKE field, and null answers through the proxy ([1bfda41](https://github.com/oxabl-project/oxabl/commit/1bfda4148c8a121292be2d763523ac007ea19adb))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.2.2 to 4.3.0
    * oxabl_common bumped from 1.3.0 to 1.3.1
    * oxabl_daemon bumped from 0.6.1 to 0.7.0
    * oxabl_pipeline bumped from 3.7.0 to 3.8.0
    * oxabl_workspace bumped from 3.6.1 to 3.6.2
  * dev-dependencies
    * oxabl_daemon bumped from 0.6.1 to 0.7.0
    * oxabl_analyze bumped from 4.2.2 to 4.3.0
    * oxabl_schema bumped from 0.4.7 to 0.5.0
    * oxabl_common bumped from 1.3.0 to 1.3.1
    * oxabl_workspace bumped from 3.6.1 to 3.6.2
    * oxabl_pipeline bumped from 3.7.0 to 3.8.0

## [3.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.6...oxabl_lsp-v3.2.0) (2026-11-20)


### Features

* ABL document outline, and no-setup defaults for the editor ([375b8c4](https://github.com/oxabl-project/oxabl/commit/375b8c4704d4de20e8104c188fb0b9b7d6f2f855))
* **lsp:** without an oxabl.toml, no diagnostics and no formatting ([c120ed7](https://github.com/oxabl-project/oxabl/commit/c120ed7697a8b5df89fc9d106315380ae7a0c5f7))


### Bug Fixes

* **parser:** stop at a nesting and chain limit, and parse on a stack of known size ([7b741da](https://github.com/oxabl-project/oxabl/commit/7b741dae5637ba90408c8ac0a857464a5c44ee13))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.2.1 to 4.2.2
    * oxabl_common bumped from 1.2.2 to 1.3.0
    * oxabl_daemon bumped from 0.6.0 to 0.6.1
    * oxabl_pipeline bumped from 3.6.1 to 3.7.0
    * oxabl_workspace bumped from 3.6.0 to 3.6.1
  * dev-dependencies
    * oxabl_daemon bumped from 0.6.0 to 0.6.1
    * oxabl_analyze bumped from 4.2.1 to 4.2.2
    * oxabl_ast bumped from 4.0.0 to 5.0.0
    * oxabl_schema bumped from 0.4.6 to 0.4.7
    * oxabl_common bumped from 1.2.2 to 1.3.0
    * oxabl_workspace bumped from 3.6.0 to 3.6.1
    * oxabl_pipeline bumped from 3.6.1 to 3.7.0

## [3.1.6](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.5...oxabl_lsp-v3.1.6) (2026-11-18)


### Bug Fixes

* **daemon:** replace a daemon that outlived an upgrade ([d556c51](https://github.com/oxabl-project/oxabl/commit/d556c516cb2146066d3e557c89ac34cf1e19b1aa))
* **daemon:** the editor replaces a daemon that outlived an upgrade ([a87fc8e](https://github.com/oxabl-project/oxabl/commit/a87fc8e764a37f7db9fe09b6b8fdae1ba7eaaced))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_daemon bumped from 0.5.10 to 0.6.0
    * oxabl_daemon_protocol bumped from 0.3.0 to 0.4.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.10 to 0.6.0

## [3.1.5](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.4...oxabl_lsp-v3.1.5) (2026-11-18)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.2.0 to 4.2.1
    * oxabl_daemon bumped from 0.5.9 to 0.5.10
    * oxabl_pipeline bumped from 3.6.0 to 3.6.1
    * oxabl_workspace bumped from 3.5.0 to 3.6.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.9 to 0.5.10
    * oxabl_analyze bumped from 4.2.0 to 4.2.1
    * oxabl_schema bumped from 0.4.5 to 0.4.6
    * oxabl_workspace bumped from 3.5.0 to 3.6.0
    * oxabl_pipeline bumped from 3.6.0 to 3.6.1

## [3.1.4](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.3...oxabl_lsp-v3.1.4) (2026-11-17)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.1.0 to 4.2.0
    * oxabl_daemon bumped from 0.5.8 to 0.5.9
    * oxabl_pipeline bumped from 3.5.0 to 3.6.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.8 to 0.5.9
    * oxabl_analyze bumped from 4.1.0 to 4.2.0
    * oxabl_pipeline bumped from 3.5.0 to 3.6.0

## [3.1.3](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.2...oxabl_lsp-v3.1.3) (2026-11-16)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.4 to 4.1.0
    * oxabl_daemon bumped from 0.5.7 to 0.5.8
    * oxabl_pipeline bumped from 3.4.2 to 3.5.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.7 to 0.5.8
    * oxabl_analyze bumped from 4.0.4 to 4.1.0
    * oxabl_pipeline bumped from 3.4.2 to 3.5.0

## [3.1.2](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.1...oxabl_lsp-v3.1.2) (2026-11-13)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.3 to 4.0.4
    * oxabl_daemon bumped from 0.5.6 to 0.5.7
    * oxabl_pipeline bumped from 3.4.1 to 3.4.2
    * oxabl_workspace bumped from 3.4.0 to 3.5.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.6 to 0.5.7
    * oxabl_analyze bumped from 4.0.3 to 4.0.4
    * oxabl_schema bumped from 0.4.4 to 0.4.5
    * oxabl_workspace bumped from 3.4.0 to 3.5.0
    * oxabl_pipeline bumped from 3.4.1 to 3.4.2

## [3.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.1.0...oxabl_lsp-v3.1.1) (2026-11-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.2 to 4.0.3
    * oxabl_daemon bumped from 0.5.5 to 0.5.6
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.5 to 0.5.6
    * oxabl_analyze bumped from 4.0.2 to 4.0.3
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1

## [3.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.0.4...oxabl_lsp-v3.1.0) (2026-11-11)


### Features

* **config:** turn diagnostics and formatting on or off in [workspace.features] ([5f9d715](https://github.com/oxabl-project/oxabl/commit/5f9d715a394471319298f6895077ba9179d408c1))
* **config:** turn diagnostics and formatting on or off in [workspace.features] ([55e57ec](https://github.com/oxabl-project/oxabl/commit/55e57ecef9141909908b7416593d21b983f70932))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.1 to 4.0.2
    * oxabl_daemon bumped from 0.5.4 to 0.5.5
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0
    * oxabl_workspace bumped from 3.3.0 to 3.4.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.4 to 0.5.5
    * oxabl_analyze bumped from 4.0.1 to 4.0.2
    * oxabl_schema bumped from 0.4.3 to 0.4.4
    * oxabl_workspace bumped from 3.3.0 to 3.4.0
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0

## [3.0.4](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.0.3...oxabl_lsp-v3.0.4) (2026-11-10)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.0 to 4.0.1
    * oxabl_daemon bumped from 0.5.3 to 0.5.4
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.3 to 0.5.4
    * oxabl_analyze bumped from 4.0.0 to 4.0.1
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1

## [3.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_lsp-v3.0.2...oxabl_lsp-v3.0.3) (2026-11-06)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 3.0.2 to 4.0.0
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_daemon bumped from 0.5.2 to 0.5.3
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0
    * oxabl_style bumped from 0.2.0 to 0.2.1
    * oxabl_workspace bumped from 3.2.0 to 3.3.0
  * dev-dependencies
    * oxabl_daemon bumped from 0.5.2 to 0.5.3
    * oxabl_analyze bumped from 3.0.2 to 4.0.0
    * oxabl_ast bumped from 3.0.0 to 4.0.0
    * oxabl_schema bumped from 0.4.2 to 0.4.3
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_workspace bumped from 3.2.0 to 3.3.0
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0

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
