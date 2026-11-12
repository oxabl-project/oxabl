# Changelog

## [5.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v5.1.0...oxabl-v5.2.0) (2026-11-12)


### Features

* **build:** add --yield so a newer build can stop an editor's build ([863d5a7](https://github.com/oxabl-project/oxabl/commit/863d5a71bf8342647b3220ced38a4d987ca3bc99))
* **cli:** add oxabl self-update behind the self-update feature ([2e8ec18](https://github.com/oxabl-project/oxabl/commit/2e8ec180c759ca26d3cd3440af341a63316259e5))
* **cli:** add scripts/install.sh for the static Linux binary ([26d9732](https://github.com/oxabl-project/oxabl/commit/26d973264168e2c77e0444b9c8170f212144cc66))
* **cli:** install script, self-update and build --yield ([d3dbc2e](https://github.com/oxabl-project/oxabl/commit/d3dbc2e667c41f09020c606a90253f618afdb07a))


### Bug Fixes

* **cli:** treat a next-check stamp more than a day ahead as due ([cce7346](https://github.com/oxabl-project/oxabl/commit/cce734606476e1c7107557b8203d3aa4f2324bf1))
* **preprocessor:** stop expanding at a limit of 32 MiB per file ([958fe45](https://github.com/oxabl-project/oxabl/commit/958fe45edf0555ea774c125e949af5c34b30263a))
* **preprocessor:** stop expanding at a limit of 32 MiB per file ([fbbe2ec](https://github.com/oxabl-project/oxabl/commit/fbbe2ec08e8e7b1e89c145052acea0edc25f186f)), closes [#337](https://github.com/oxabl-project/oxabl/issues/337)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 4.0.1 to 4.0.2
    * oxabl_preprocessor bumped from 0.7.4 to 0.7.5
    * oxabl_semantic bumped from 5.0.1 to 5.0.2
    * oxabl_analyze bumped from 4.0.2 to 4.0.3
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1
    * oxabl_lint bumped from 5.0.1 to 5.0.2
    * oxabl_lsp bumped from 3.1.0 to 3.1.1
    * oxabl_daemon bumped from 0.5.5 to 0.5.6
    * oxabl_build bumped from 0.2.2 to 0.3.0
    * oxabl_build_remote bumped from 0.2.2 to 0.2.3
    * oxabl_formatter bumped from 2.0.4 to 2.0.5
  * dev-dependencies
    * oxabl_analyze bumped from 4.0.2 to 4.0.3
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1
    * oxabl_flow bumped from 0.3.4 to 0.3.5
    * oxabl_handles bumped from 0.2.4 to 0.2.5
    * oxabl_scope bumped from 0.2.4 to 0.2.5
    * oxabl_effects bumped from 0.3.2 to 0.3.3
    * oxabl_nulls bumped from 0.2.4 to 0.2.5
    * oxabl_index bumped from 5.0.1 to 5.0.2

## [5.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v5.0.0...oxabl-v5.1.0) (2026-11-11)


### Features

* **config:** turn diagnostics and formatting on or off in [workspace.features] ([5f9d715](https://github.com/oxabl-project/oxabl/commit/5f9d715a394471319298f6895077ba9179d408c1))
* **config:** turn diagnostics and formatting on or off in [workspace.features] ([55e57ec](https://github.com/oxabl-project/oxabl/commit/55e57ecef9141909908b7416593d21b983f70932))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 4.0.0 to 4.0.1
    * oxabl_workspace bumped from 3.3.0 to 3.4.0
    * oxabl_preprocessor bumped from 0.7.3 to 0.7.4
    * oxabl_semantic bumped from 5.0.0 to 5.0.1
    * oxabl_schema bumped from 0.4.3 to 0.4.4
    * oxabl_analyze bumped from 4.0.1 to 4.0.2
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0
    * oxabl_lint bumped from 5.0.0 to 5.0.1
    * oxabl_lsp bumped from 3.0.4 to 3.1.0
    * oxabl_daemon bumped from 0.5.4 to 0.5.5
    * oxabl_build bumped from 0.2.1 to 0.2.2
    * oxabl_build_remote bumped from 0.2.1 to 0.2.2
    * oxabl_formatter bumped from 2.0.3 to 2.0.4
  * dev-dependencies
    * oxabl_analyze bumped from 4.0.1 to 4.0.2
    * oxabl_schema bumped from 0.4.3 to 0.4.4
    * oxabl_workspace bumped from 3.3.0 to 3.4.0
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0
    * oxabl_flow bumped from 0.3.3 to 0.3.4
    * oxabl_handles bumped from 0.2.3 to 0.2.4
    * oxabl_scope bumped from 0.2.3 to 0.2.4
    * oxabl_effects bumped from 0.3.1 to 0.3.2
    * oxabl_nulls bumped from 0.2.3 to 0.2.4
    * oxabl_index bumped from 5.0.0 to 5.0.1

## [5.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v4.0.0...oxabl-v5.0.0) (2026-11-10)


### ⚠ BREAKING CHANGES

* **semantic:** an inherited buffer reads its table and statements resolve inherited records

### Bug Fixes

* **build:** show the skips that need no environment in a plan without --probe ([dcb376c](https://github.com/oxabl-project/oxabl/commit/dcb376c09dc449529303c8d9a28defd4839c8ba8))
* **semantic:** an inherited buffer reads its table and statements resolve inherited records ([aeaa593](https://github.com/oxabl-project/oxabl/commit/aeaa59328fa762e0246b3df03787e961f0f74749))


### Performance Improvements

* **build:** keep store writes and cache copies off the dispatch thread ([dd1a048](https://github.com/oxabl-project/oxabl/commit/dd1a0484d181ffcd0b696b92f684b67e00626edf))
* **build:** share the $DLC hash phase across the workers and time the setup phases ([8863457](https://github.com/oxabl-project/oxabl/commit/88634579a50f54dd0256ff876f47b998c86bd678))
* **build:** share the $DLC hash phase across the workers; the plan without --probe shows path-only skips ([020b852](https://github.com/oxabl-project/oxabl/commit/020b8522af5b166c116bb4b5a330c67f93db2d6a))
* **build:** store writes and cache uploads leave the coordinator thread ([b2717a2](https://github.com/oxabl-project/oxabl/commit/b2717a2f68d5a45276a178bdc01a1a57526cdee9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 4.0.0 to 5.0.0
    * oxabl_analyze bumped from 4.0.0 to 4.0.1
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1
    * oxabl_lint bumped from 4.0.0 to 5.0.0
    * oxabl_lsp bumped from 3.0.3 to 3.0.4
    * oxabl_daemon bumped from 0.5.3 to 0.5.4
    * oxabl_build bumped from 0.2.0 to 0.2.1
    * oxabl_build_remote bumped from 0.2.0 to 0.2.1
  * dev-dependencies
    * oxabl_analyze bumped from 4.0.0 to 4.0.1
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1
    * oxabl_effects bumped from 0.3.0 to 0.3.1
    * oxabl_index bumped from 4.0.0 to 5.0.0

## [4.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v3.0.2...oxabl-v4.0.0) (2026-11-06)


### ⚠ BREAKING CHANGES

* **index:** inherited data members are in scope in a subclass

### Features

* **build-remote:** add the remote cache of oxabl build behind the remote-cache feature ([08d47d4](https://github.com/oxabl-project/oxabl/commit/08d47d4da400855284038dc04e4cc4e0b0a478ae))
* **build:** add compile keys, keyability and keys in oxabl build --plan --probe ([09c3c11](https://github.com/oxabl-project/oxabl/commit/09c3c114819cce5e45082aaf29722c4e7eeeeea0))
* **build:** add oxabl build --plan --since, --json and the plan parity script ([2aaff6b](https://github.com/oxabl-project/oxabl/commit/2aaff6b51858513cd597a2440ad707afa8eb488f))
* **build:** an observed closure holds every workspace class and compiles after the class units ([2f77a74](https://github.com/oxabl-project/oxabl/commit/2f77a7416babe4e17716a1980678d1529fb31725))
* **build:** AVM worker pool, worker protocol and oxabl build --probe ([4b7ad93](https://github.com/oxabl-project/oxabl/commit/4b7ad93710b546e55c4b7c47c81ca15940cc7c05))
* **build:** compile keys, keyability and keys in oxabl build --plan --probe ([00ccdee](https://github.com/oxabl-project/oxabl/commit/00ccdee0a3985b9cb83dd486bdf1f3095fad08b6))
* **build:** compile units in dependency order with oxabl build ([5e41320](https://github.com/oxabl-project/oxabl/commit/5e41320e59d6914b6f16c96276f59f323698464f))
* **build:** compile units in dependency order with oxabl build ([7252935](https://github.com/oxabl-project/oxabl/commit/7252935c559d34df6403d260e6c3d32f1726791f))
* **build:** config overlay, effective config, and the wrapper guide ([df21e74](https://github.com/oxabl-project/oxabl/commit/df21e74656c08b87ee18828621f65e49e435b5f1))
* **build:** fetch from and push to an oxabl-cache server ([9bcd642](https://github.com/oxabl-project/oxabl/commit/9bcd6425c519d6e9e3ce94075b9ad5fcc4f24494))
* **build:** graph store, incremental refresh and oxabl build --plan ([50d3004](https://github.com/oxabl-project/oxabl/commit/50d3004216da610003c27a3a1430bdb97fdfcbe3))
* **build:** key a unit that oxabl cannot read by the closure its compiler read ([680c8f9](https://github.com/oxabl-project/oxabl/commit/680c8f908ed3c731139698dc277e58ee0f553835))
* **build:** key a unit that oxabl cannot read by the closure its compiler read ([43e7178](https://github.com/oxabl-project/oxabl/commit/43e71787e57665d850a43173fc3274c9ad724104))
* **build:** merge an overlay over oxabl.toml and print the effective configuration ([5d26d87](https://github.com/oxabl-project/oxabl/commit/5d26d877d70be0423690310fa05442d8405367f3))
* **build:** oxabl build --plan --since, --json and a parity script ([80e7008](https://github.com/oxabl-project/oxabl/commit/80e7008f4cc3199fe36ffc68b469b4543a170664))
* **build:** verify compile results and cache them in a local cache ([f87a7b0](https://github.com/oxabl-project/oxabl/commit/f87a7b04a2784c79dc74f621e993b3ec49b10f06))
* **build:** verify compile results and cache them in a local cache ([61ba98e](https://github.com/oxabl-project/oxabl/commit/61ba98e6b856bdc08bb5814bf09c090a9ca15010))
* **cli:** add oxabl build --plan [--files] ([68bb264](https://github.com/oxabl-project/oxabl/commit/68bb264cfbafb25780e96dc6d56c81ef424b117c))
* **cli:** add oxabl build --probe ([bb6fcf5](https://github.com/oxabl-project/oxabl/commit/bb6fcf5382c21a6f290702a0bfdd248aa251fc49))
* **cli:** list oxabl build in --help ([fbd38d4](https://github.com/oxabl-project/oxabl/commit/fbd38d488d7a863bbc3b9e15adcc8c8fda05a262))


### Bug Fixes

* **build:** a &gt; inside an XREF-XML attribute value does not end the element ([3874bf3](https://github.com/oxabl-project/oxabl/commit/3874bf3db908cfc82ee636f45e189dfd03fae130))
* **build:** a &gt; inside an XREF-XML attribute value does not end the element ([ae5fbe6](https://github.com/oxabl-project/oxabl/commit/ae5fbe65ca2a05e99e4fc441d11a8a8dbd6385e5))
* **build:** a slow install never loses the answers of a remote fetch ([09e3c58](https://github.com/oxabl-project/oxabl/commit/09e3c58a0c86756f34a4bb4a6d2161be64d3b4c2))
* **build:** a slow install never loses the answers of a remote fetch ([5513406](https://github.com/oxabl-project/oxabl/commit/5513406f15845f71830b324d0da7ba97dd1fa31c))
* **build:** compile every unit that has no result when a build follows a plan ([ad1c3c4](https://github.com/oxabl-project/oxabl/commit/ad1c3c42e2c73fd071b86f0753b519938c344d3d))
* **build:** derive the class name of a unit from its compile name ([e8326a3](https://github.com/oxabl-project/oxabl/commit/e8326a39bef776ea6c669ed7beedddea17d162dd))
* **build:** encrypted source never gets an observed closure ([896562f](https://github.com/oxabl-project/oxabl/commit/896562fc9ddb1b0b92fe64c13e40694515af4c2f))
* **build:** encrypted source never gets an observed closure ([7dc9c5d](https://github.com/oxabl-project/oxabl/commit/7dc9c5ddddc1e0cce7c22f4b8b372b3cd1ba11e8))
* **build:** find a class statement after a preprocessor directive ([5d177f5](https://github.com/oxabl-project/oxabl/commit/5d177f57f3bd1973fa1b1b0121daf831310adeb4))
* **build:** find a class statement after a preprocessor directive ([0eb583b](https://github.com/oxabl-project/oxabl/commit/0eb583b0083653fc0495acc7b8e068272926a8a8)), closes [#286](https://github.com/oxabl-project/oxabl/issues/286)
* **build:** hand out no job while a completion is unhandled, and queue jobs that become ready together at once ([60a95ae](https://github.com/oxabl-project/oxabl/commit/60a95ae8188a1f7427d1f36634060b451a9b31b1))
* **build:** hash the running executable into the analysis identity ([805e34f](https://github.com/oxabl-project/oxabl/commit/805e34f0e5bdc8f5aa7931b08e2c0eb0876237d4))
* **build:** hash the running executable into the analysis identity ([5e5ad60](https://github.com/oxabl-project/oxabl/commit/5e5ad60ffff860ff10ce08eb46f74fc67fa8dfbf))
* **build:** judge an include in its includer's context for K1 ([4e38a81](https://github.com/oxabl-project/oxabl/commit/4e38a81644610c34171701ce27aa9517c7435cc9))
* **build:** keep the dot of the stage path for XREF files and give every failed unit a reason ([92d1ec6](https://github.com/oxabl-project/oxabl/commit/92d1ec63a52f48eaec1a76ec5147ec96ca2488d9))
* **build:** queue the ready jobs before any worker can take one ([59b349b](https://github.com/oxabl-project/oxabl/commit/59b349b08197fd39c644f9a9e1d956dd60136475))
* **build:** report keys the remote client left without an answer ([8341645](https://github.com/oxabl-project/oxabl/commit/834164579efed04cf38f92df78d4f37dce5b1426))
* **build:** take a class name from its compile name when the CLASS statement does not spell it ([1e709e6](https://github.com/oxabl-project/oxabl/commit/1e709e6ab0afaac39032021ec77817c972556c13))
* **cli:** check --include-findings prints include paths like every other path ([a8cd082](https://github.com/oxabl-project/oxabl/commit/a8cd082cd86e94db51ce97240d09ae243a107047))
* **cli:** drop internal requirement tags from help and schema text ([f1d1dfc](https://github.com/oxabl-project/oxabl/commit/f1d1dfc6d93c4543f8da31cf3d5c7278bba7515a))
* **cli:** format refuses an oxabl.toml it cannot use ([fb1a2c5](https://github.com/oxabl-project/oxabl/commit/fb1a2c571edb5d4a1d144c7437a4dfdcb0951d8b))
* **cli:** oxabl --version prints the crate version ([cfe6c14](https://github.com/oxabl-project/oxabl/commit/cfe6c147463a9185e7f4bdb2f0f7b4c015563aa4))
* **cli:** rank tied conformance error patterns by text ([fb695c7](https://github.com/oxabl-project/oxabl/commit/fb695c7e914373034c3e700e082124e4818e2af8))
* **cli:** the schema lists every encoding the binary accepts ([791ada6](https://github.com/oxabl-project/oxabl/commit/791ada6192e3db1e11269985e08402efba2b808b))
* **cli:** user-facing gaps found while writing the user docs ([d34d5e7](https://github.com/oxabl-project/oxabl/commit/d34d5e790391fddecf68ced90ba79edd8c20e432))
* **cli:** words the formatter expands or inserts keep the case of the surrounding keywords under Preserve ([049abfe](https://github.com/oxabl-project/oxabl/commit/049abfe176da0ff00cc20c70d4d2c97e879d937e))
* **index:** inherited data members are in scope in a subclass ([fde4113](https://github.com/oxabl-project/oxabl/commit/fde41134d4d4cfd7f2d6a8e70f00bbe759146d70))
* **lint:** unknown-table-or-field describes what it reports ([c8198ef](https://github.com/oxabl-project/oxabl/commit/c8198ef126b7c50e86e50e1f8ec0452be1c1ed47))
* **parser:** accept WHILE before the counting loop, and {&name } ([f593e2d](https://github.com/oxabl-project/oxabl/commit/f593e2db26f11b35dc908188246fecc5f29d8e41))
* **parser:** accept WHILE before the counting loop, and {&name } ([d7b7e82](https://github.com/oxabl-project/oxabl/commit/d7b7e82ebc129d1e4129ef0b18d90a64fedce21f)), closes [#260](https://github.com/oxabl-project/oxabl/issues/260)
* **preprocessor:** split include arguments into words as the compiler does ([6911c59](https://github.com/oxabl-project/oxabl/commit/6911c5965084a74cbdafa59d9ffe7bcda686a6a8))
* **preprocessor:** split include arguments into words as the compiler does ([bdff1d7](https://github.com/oxabl-project/oxabl/commit/bdff1d79c22cf396b39ed07b76f3d6ce670e6544)), closes [#285](https://github.com/oxabl-project/oxabl/issues/285)


### Performance Improvements

* **build:** promote fetched sets in batches and fill the local cache off the fetch thread ([1c772f3](https://github.com/oxabl-project/oxabl/commit/1c772f3ccf8690eaa34e3c1ae61746da09be1b1f))
* **build:** promote fetched sets in batches and fill the local cache off the fetch thread ([0d544a3](https://github.com/oxabl-project/oxabl/commit/0d544a3fe084e82addfd9c8ec2fc9a1b304438b9))
* **cli:** run format and conformance on all cores ([37d4a55](https://github.com/oxabl-project/oxabl/commit/37d4a55361fe400c569e9d7573b13273e4a1605f))
* **cli:** run format and conformance on all cores ([d195735](https://github.com/oxabl-project/oxabl/commit/d195735f6d30e52be7ebd8f806697f52d228f24c))
* **cli:** use jemalloc as the oxabl binary's allocator ([b7f598e](https://github.com/oxabl-project/oxabl/commit/b7f598ef6264d3ec157f24e2201a209e6fddf02b))
* **cli:** use jemalloc as the oxabl binary's allocator ([f7e2cf8](https://github.com/oxabl-project/oxabl/commit/f7e2cf8885f4460ad066c7331d53060ad649db54))
* **pipeline:** build TX004's workspace summaries on all cores ([75d9f8e](https://github.com/oxabl-project/oxabl/commit/75d9f8e1bc29a50364db54900c7161e53680dcb6))
* **pipeline:** build TX004's workspace summaries on all cores ([f289387](https://github.com/oxabl-project/oxabl/commit/f289387b593c36806a16f7c546e96b2237e974aa))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 3.0.2 to 4.0.0
    * oxabl_lexer bumped from 2.0.0 to 2.0.1
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_ast bumped from 3.0.0 to 4.0.0
    * oxabl_workspace bumped from 3.2.0 to 3.3.0
    * oxabl_preprocessor bumped from 0.7.2 to 0.7.3
    * oxabl_semantic bumped from 3.0.2 to 4.0.0
    * oxabl_schema bumped from 0.4.2 to 0.4.3
    * oxabl_analyze bumped from 3.0.2 to 4.0.0
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0
    * oxabl_lint bumped from 3.0.2 to 4.0.0
    * oxabl_lsp bumped from 3.0.2 to 3.0.3
    * oxabl_daemon bumped from 0.5.2 to 0.5.3
    * oxabl_build bumped from 0.1.0 to 0.2.0
    * oxabl_build_remote bumped from 0.1.0 to 0.2.0
    * oxabl_formatter bumped from 2.0.2 to 2.0.3
    * oxabl_style bumped from 0.2.0 to 0.2.1
  * dev-dependencies
    * oxabl_analyze bumped from 3.0.2 to 4.0.0
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_schema bumped from 0.4.2 to 0.4.3
    * oxabl_workspace bumped from 3.2.0 to 3.3.0
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0
    * oxabl_flow bumped from 0.3.2 to 0.3.3
    * oxabl_handles bumped from 0.2.2 to 0.2.3
    * oxabl_scope bumped from 0.2.2 to 0.2.3
    * oxabl_effects bumped from 0.2.2 to 0.3.0
    * oxabl_nulls bumped from 0.2.2 to 0.2.3
    * oxabl_index bumped from 3.1.0 to 4.0.0

## [3.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl-v3.0.1...oxabl-v3.0.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 3.0.1 to 3.0.2
    * oxabl_workspace bumped from 3.1.0 to 3.2.0
    * oxabl_preprocessor bumped from 0.7.1 to 0.7.2
    * oxabl_semantic bumped from 3.0.1 to 3.0.2
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_analyze bumped from 3.0.1 to 3.0.2
    * oxabl_pipeline bumped from 3.1.0 to 3.2.0
    * oxabl_lint bumped from 3.0.1 to 3.0.2
    * oxabl_lsp bumped from 3.0.1 to 3.0.2
    * oxabl_daemon bumped from 0.5.1 to 0.5.2
    * oxabl_formatter bumped from 2.0.1 to 2.0.2
  * dev-dependencies
    * oxabl_analyze bumped from 3.0.1 to 3.0.2
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_workspace bumped from 3.1.0 to 3.2.0
    * oxabl_pipeline bumped from 3.1.0 to 3.2.0
    * oxabl_flow bumped from 0.3.1 to 0.3.2
    * oxabl_handles bumped from 0.2.1 to 0.2.2
    * oxabl_scope bumped from 0.2.1 to 0.2.2
    * oxabl_effects bumped from 0.2.1 to 0.2.2
    * oxabl_nulls bumped from 0.2.1 to 0.2.2
    * oxabl_index bumped from 3.0.1 to 3.1.0

## [3.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl-v3.0.0...oxabl-v3.0.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 3.0.0 to 3.0.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
    * oxabl_preprocessor bumped from 0.7.0 to 0.7.1
    * oxabl_semantic bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_analyze bumped from 3.0.0 to 3.0.1
    * oxabl_pipeline bumped from 3.0.0 to 3.1.0
    * oxabl_lint bumped from 3.0.0 to 3.0.1
    * oxabl_lsp bumped from 3.0.0 to 3.0.1
    * oxabl_daemon bumped from 0.5.0 to 0.5.1
    * oxabl_formatter bumped from 2.0.0 to 2.0.1
  * dev-dependencies
    * oxabl_analyze bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
    * oxabl_pipeline bumped from 3.0.0 to 3.1.0
    * oxabl_flow bumped from 0.3.0 to 0.3.1
    * oxabl_handles bumped from 0.2.0 to 0.2.1
    * oxabl_scope bumped from 0.2.0 to 0.2.1
    * oxabl_effects bumped from 0.2.0 to 0.2.1
    * oxabl_nulls bumped from 0.2.0 to 0.2.1
    * oxabl_index bumped from 3.0.0 to 3.0.1

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v2.0.0...oxabl-v3.0.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **pipeline:** `ConditionalIncludeClosure` has a new `references` field and is now `#[non_exhaustive]`.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **pipeline:** CollectedDiagnostics has a new public field, include_origin, so code that builds it with a struct expression must set it.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **pipeline:** the edge_set_with callback receives a LintResult without lint findings or type-check diagnostics; edge_set runs no lint rules.
* **lint:** `oxabl_analyze::CollectedDiagnostics` has a new public field, `include_findings`. Code that builds it with a struct literal must set it or use `..Default::default()`.
* **preprocessor:** `PreprocessedFile` has a new pub field, `skipped_includes`.
* **semantic:** `SymbolKind` has a new `Sequence` variant.
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **cli:** add check --watch ([e01b176](https://github.com/oxabl-project/oxabl/commit/e01b176619848dd87a7a5bc1faa5b9f70334513f)), closes [#144](https://github.com/oxabl-project/oxabl/issues/144)
* **cli:** analyse files in parallel with `check --jobs` ([a3a2978](https://github.com/oxabl-project/oxabl/commit/a3a2978132d6c291c1948e428dfa2ff187a5153b))
* **effects:** check index choices against COMPILE ... XREF output ([ed7321b](https://github.com/oxabl-project/oxabl/commit/ed7321bf6a26f5391c215c9de832927bc942d95d))
* **effects:** measure call-graph coverage, summaries and TX004 over a tree ([b3a369c](https://github.com/oxabl-project/oxabl/commit/b3a369ccb65c23defc81ba95ef03f23957d550bf))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **examples:** count conditional references, include closures and degraded roots in the edge census ([04ef3cd](https://github.com/oxabl-project/oxabl/commit/04ef3cd6e86c814e1366fb70f53d773905ab005c))
* **handles:** add oxabl_handles, lifetimes of dynamic objects per unit ([005ae67](https://github.com/oxabl-project/oxabl/commit/005ae67771f889bd80c71d268554ae43b16b3b3d))
* **handles:** read compound guards, failed NO-ERROR creates and class widget pools ([881727e](https://github.com/oxabl-project/oxabl/commit/881727ee87815748f33a87281d9d1ed56c9f4fac))
* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lint:** add NL001 and NL002 unknown-value warnings ([17e0c6c](https://github.com/oxabl-project/oxabl/commit/17e0c6c0c6f94ba01729a4b3bf50b3380722cd28))
* **lint:** add NL004 nullable-db-field ([0ea9cd2](https://github.com/oxabl-project/oxabl/commit/0ea9cd24ad501d88c919c0ec4694a793b5443127))
* **lint:** add the strict.access rule DB001 find-in-loop ([df71f57](https://github.com/oxabl-project/oxabl/commit/df71f57b41299e34f0fd02a86733120a378de266))
* **lint:** add the strict.handles rules HD001-HD005 ([0dd3530](https://github.com/oxabl-project/oxabl/commit/0dd3530cbf90c1f073a5243dc501eb6f1a60b6b1))
* **lint:** add the strict.locks rules LK001-LK003 ([318f55f](https://github.com/oxabl-project/oxabl/commit/318f55fd9a7367bf78a6475fb75056bc494abc2f))
* **lint:** add the strict.transactions rules TX001-TX005 ([1253cc8](https://github.com/oxabl-project/oxabl/commit/1253cc83c520d4f321abafe9c7704b76641905bd))
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **nulls:** count NL003 findings per directory over a tree ([1254ce4](https://github.com/oxabl-project/oxabl/commit/1254ce4793e5b47861d467c2a1d46c6954fc2e1f))
* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **nulls:** treat database fields the schema starts at ? as sources of unknown values ([3b39aba](https://github.com/oxabl-project/oxabl/commit/3b39abaecb3b1477f1a2e26444511e27ca7c6ced))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **pipeline:** list the class and program names a conditional include's target writes ([81462eb](https://github.com/oxabl-project/oxabl/commit/81462eb81a826051c2e285bf6f0d855f5dbd316c))
* **pipeline:** retain include-origin findings with their compilation context ([950e08b](https://github.com/oxabl-project/oxabl/commit/950e08bc61f631828eb57261cc26f00a9b2f166f))
* **preprocessor:** carry include references in not-taken &IF branches ([b1057ac](https://github.com/oxabl-project/oxabl/commit/b1057ac88120afad1ce4d59acb2deb808a3fcc9e))
* **scope:** add oxabl_scope, the compiler's buffer scopes, transaction blocks and locks per unit ([cebe727](https://github.com/oxabl-project/oxabl/commit/cebe727b779e8d0806aa41e60d2968f56ed0cca6))
* **scope:** check the scope engine against COMPILE ... LISTING output ([19f0ae2](https://github.com/oxabl-project/oxabl/commit/19f0ae2d57391ee48d79c747f1a7b81aa9cafe33))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* **cli:** share callee summaries across check --watch handles ([4552d86](https://github.com/oxabl-project/oxabl/commit/4552d863a6b61fba5c70f44954987735293a954a))
* **handles:** model calls, :HANDLE identity, unmapped frees, COPY-LOB, shadowed frees and a work budget ([05f23ef](https://github.com/oxabl-project/oxabl/commit/05f23ef729196a3175317ac9d3b89815c3042c03))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lexer:** stop printing to stdout on an overflowing number literal ([14d970e](https://github.com/oxabl-project/oxabl/commit/14d970efeacf9ac7b8112772348fbb00ff32f1ed))
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** report handle findings anchored in an include at the use site ([b414a76](https://github.com/oxabl-project/oxabl/commit/b414a76e4179e87c6b78a06d8fa732b883a631af))
* **lint:** update example for is_prototype ([601246a](https://github.com/oxabl-project/oxabl/commit/601246ab207e5aa2ace7bb83d551087c8886415b))
* **parser:** read database-qualified names in record statements ([6d8cbf1](https://github.com/oxabl-project/oxabl/commit/6d8cbf10c5e64de5963dde999b0727d24b41e697))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** pair LISTING blocks by the end of the file path ([8965d2d](https://github.com/oxabl-project/oxabl/commit/8965d2d115bf1e3673a1e33c777184d3492ed78a))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **check:** scan the SHARED producer map on every core ([fa951a2](https://github.com/oxabl-project/oxabl/commit/fa951a247a9754d992970c543437ba5d0d159b7d))
* **cli:** build the watch cycle's reverse graph on the check's jobs ([7a20d8e](https://github.com/oxabl-project/oxabl/commit/7a20d8e52629b9026b9573dc04fbb03712d8f6b8))
* **examples:** scan the edge_pass producer map on the run's threads ([4a5bde8](https://github.com/oxabl-project/oxabl/commit/4a5bde8455c6e385080fa93d02bd9d645744ebfb))
* **nulls:** bound the work of the null analyses ([edbd5cb](https://github.com/oxabl-project/oxabl/commit/edbd5cb128509b93fd3f51dee3a939430055b3b2))
* **nulls:** bound the work of the null analyses and report units that fall back ([5cebc88](https://github.com/oxabl-project/oxabl/commit/5cebc88ee58a024e00972a8990f1df7483dd53cc))
* **pipeline:** build edge sets without lint rules or type checks ([bcf44d3](https://github.com/oxabl-project/oxabl/commit/bcf44d36ba12efa7fed1558bac1bb1bf4ddc3733)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_lint bumped from 2.0.0 to 3.0.0
    * oxabl_lsp bumped from 2.0.0 to 3.0.0
    * oxabl_daemon bumped from 0.4.0 to 0.5.0
    * oxabl_formatter bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_handles bumped from 0.1.0 to 0.2.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
    * oxabl_effects bumped from 0.1.0 to 0.2.0
    * oxabl_nulls bumped from 0.1.0 to 0.2.0
    * oxabl_index bumped from 2.0.0 to 3.0.0

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl-v1.1.4...oxabl-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.
* **preprocessor:** PipelineConfig gains the public preprocessor field.

### Features

* **cli:** add check --baseline and --update-baseline ([73f052a](https://github.com/oxabl-project/oxabl/commit/73f052a841c3a6548207dab4dcafbdcdb5d17a2e))
* **cli:** check context-free strict rules in the include files of a walked tree ([7b931f0](https://github.com/oxabl-project/oxabl/commit/7b931f0770f0149ff22c6f0c35658085682797c5))
* **lint:** register CL001, CL002, CL005 and TX006 in their strict groups ([714834e](https://github.com/oxabl-project/oxabl/commit/714834e13ebb313b70c94ca7dc1438215304e3c4))
* **oxabl:** re-export the lint rule registry and lint config types ([4596422](https://github.com/oxabl-project/oxabl/commit/45964228e01e18c33747cf19f5438700c6263101))
* **pipeline:** apply lint overrides per file and warn on unknown names ([e65a26e](https://github.com/oxabl-project/oxabl/commit/e65a26e0e9e68a64895dfe032639c77cbb40f281))
* **preprocessor:** answer the AVM's predefined names ([d996c49](https://github.com/oxabl-project/oxabl/commit/d996c49d821b7e086a4e0dfb691962e3f4c6bd8f)), closes [#216](https://github.com/oxabl-project/oxabl/issues/216)


### Bug Fixes

* **cli:** check a directory that holds only include files ([2090fd4](https://github.com/oxabl-project/oxabl/commit/2090fd449f29617a866339da6b75eb8a97792039))
* **cli:** key baseline paths through the file system ([8a67412](https://github.com/oxabl-project/oxabl/commit/8a67412855655128183d5194df2607ebcac6300f))
* **cli:** make baseline keys survive keyword case and path spelling ([c7ce879](https://github.com/oxabl-project/oxabl/commit/c7ce879b8d27b6dff26a76d683ff2568a3469975))
* **lint:** lower only the findings an includer can change ([4e29dbc](https://github.com/oxabl-project/oxabl/commit/4e29dbc0cf283ae44d61891eb1b44173b0ca53de))
* **lint:** lower the confidence of includer-dependent fragment findings ([4fcec91](https://github.com/oxabl-project/oxabl/commit/4fcec91f4528a01993297ec2456a7555bd969046))
* **preprocessor:** evaluate the functions an &IF names, or say it guessed ([026e29e](https://github.com/oxabl-project/oxabl/commit/026e29e4fc9fff19a17ace4b872f2210d15129da))
* **preprocessor:** expand include references with whitespace inside the braces ([82f05de](https://github.com/oxabl-project/oxabl/commit/82f05dee1506581e02016358d2a747b7a6563f90)), closes [#209](https://github.com/oxabl-project/oxabl/issues/209)
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
    * oxabl_preprocessor bumped from 0.5.2 to 0.6.0
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_analyze bumped from 1.1.4 to 2.0.0
    * oxabl_pipeline bumped from 1.2.1 to 2.0.0
    * oxabl_lint bumped from 1.1.1 to 2.0.0
    * oxabl_lsp bumped from 1.0.3 to 2.0.0
    * oxabl_daemon bumped from 0.3.2 to 0.4.0
    * oxabl_formatter bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_analyze bumped from 1.1.4 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
    * oxabl_pipeline bumped from 1.2.1 to 2.0.0
    * oxabl_flow bumped from 0.1.0 to 0.2.0

## [1.1.4](https://github.com/oxabl-project/oxabl/compare/oxabl-v1.1.3...oxabl-v1.1.4) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.3 to 1.0.4
    * oxabl_semantic bumped from 1.1.0 to 1.1.1
    * oxabl_analyze bumped from 1.1.3 to 1.1.4
    * oxabl_pipeline bumped from 1.2.0 to 1.2.1
    * oxabl_lint bumped from 1.1.0 to 1.1.1
    * oxabl_lsp bumped from 1.0.2 to 1.0.3
    * oxabl_daemon bumped from 0.3.1 to 0.3.2
    * oxabl_formatter bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_analyze bumped from 1.1.3 to 1.1.4
    * oxabl_pipeline bumped from 1.2.0 to 1.2.1

## [1.1.3](https://github.com/oxabl-project/oxabl/compare/oxabl-v1.1.2...oxabl-v1.1.3) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_analyze bumped from 1.1.2 to 1.1.3
    * oxabl_pipeline bumped from 1.1.2 to 1.2.0
    * oxabl_lint bumped from 1.0.3 to 1.1.0
    * oxabl_lsp bumped from 1.0.1 to 1.0.2
    * oxabl_daemon bumped from 0.3.0 to 0.3.1
  * dev-dependencies
    * oxabl_analyze bumped from 1.1.2 to 1.1.3
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_pipeline bumped from 1.1.2 to 1.2.0

## [1.1.2](https://github.com/oxabl-project/oxabl/compare/oxabl-v1.1.1...oxabl-v1.1.2) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))
* **daemon:** close the registry's symlink hole and stop the probe taking the lock it tests ([#173](https://github.com/oxabl-project/oxabl/issues/173)) ([3fa0bc2](https://github.com/oxabl-project/oxabl/commit/3fa0bc2c402cab88218372fed7d9c3400148bc05))
* **daemon:** move the socket and lock to the runtime directory, and key discovery on a canonical root ([#186](https://github.com/oxabl-project/oxabl/issues/186)) ([68c6a6f](https://github.com/oxabl-project/oxabl/commit/68c6a6f571013e47d694e7685de1ace222e3b7ce))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
    * oxabl_preprocessor bumped from 0.5.1 to 0.5.2
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2
    * oxabl_lint bumped from 1.0.2 to 1.0.3
    * oxabl_lsp bumped from 1.0.0 to 1.0.1
    * oxabl_daemon bumped from 0.2.0 to 0.3.0
    * oxabl_formatter bumped from 1.0.0 to 1.0.1
  * dev-dependencies
    * oxabl_daemon_protocol bumped from 0.2.0 to 0.3.0
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2

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
