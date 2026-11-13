# Changelog

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_build-v0.3.0...oxabl_build-v0.4.0) (2026-11-13)


### Features

* **build:** bind verification to the cache mode and pace schema checks by a time budget ([642c8ce](https://github.com/oxabl-project/oxabl/commit/642c8ce19cd43a2aa336bf61e88ef1d20244e268))
* **build:** bind verification to the cache mode and pace V2 by a time budget ([1ef6165](https://github.com/oxabl-project/oxabl/commit/1ef616561087ca27bdd69de0408313cd154745de))
* **build:** read the private XREF in its text form ([6e9c6a8](https://github.com/oxabl-project/oxabl/commit/6e9c6a8e623dae08ac266e640f2abe47f264d163))
* **build:** read the private XREF in its text form ([988d3cb](https://github.com/oxabl-project/oxabl/commit/988d3cb50b600a6f97b236d576a6065efa10a0ef))
* **build:** skip schema imaging when a raw digest shows no change ([85e8b1b](https://github.com/oxabl-project/oxabl/commit/85e8b1b66071a6a2698f86f3e4aa5e899c3f8cc1))
* **build:** skip schema imaging when a raw digest shows no change ([8180ffb](https://github.com/oxabl-project/oxabl/commit/8180ffb428152d2ecf2e655e356edb0c57e96b0a))


### Bug Fixes

* **build:** cover the system tables a unit can read in the schema identity ([05bc807](https://github.com/oxabl-project/oxabl/commit/05bc8071714edbbfe59ca232c80914802a59d694))
* **build:** cover the system tables a unit can read in the schema identity ([58dafa2](https://github.com/oxabl-project/oxabl/commit/58dafa22ecc066498a39e34a33d4c41ef58bd57d))
* **build:** release the workspace lock on drop, not only when its descriptor closes ([5e84ddb](https://github.com/oxabl-project/oxabl/commit/5e84ddbc2655b11a4bd53817beeb70daccd926b2))
* **build:** unlock the workspace lock before closing it ([7303f3e](https://github.com/oxabl-project/oxabl/commit/7303f3e6deeccc618f9769cf738e372a1723a7d7))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.3.3 to 0.3.4
    * oxabl_index bumped from 5.0.2 to 5.0.3
    * oxabl_pipeline bumped from 3.4.1 to 3.4.2
    * oxabl_semantic bumped from 5.0.2 to 5.0.3
    * oxabl_workspace bumped from 3.4.0 to 3.5.0

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_build-v0.2.2...oxabl_build-v0.3.0) (2026-11-12)


### Features

* **build:** add --yield so a newer build can stop an editor's build ([863d5a7](https://github.com/oxabl-project/oxabl/commit/863d5a71bf8342647b3220ced38a4d987ca3bc99))
* **cli:** install script, self-update and build --yield ([d3dbc2e](https://github.com/oxabl-project/oxabl/commit/d3dbc2e667c41f09020c606a90253f618afdb07a))


### Bug Fixes

* **build:** iterate the jobs instead of indexing them (clippy needless_range_loop) ([1a68b5d](https://github.com/oxabl-project/oxabl/commit/1a68b5d3b7d0d19982bf8c30e93e3ab2e56f9f1a))
* **build:** satisfy clippy needless_range_loop on the newest stable ([af34987](https://github.com/oxabl-project/oxabl/commit/af3498764a948d5f159eb31f181b858ada3dbae9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.3.2 to 0.3.3
    * oxabl_index bumped from 5.0.1 to 5.0.2
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1
    * oxabl_semantic bumped from 5.0.1 to 5.0.2

## [0.2.2](https://github.com/oxabl-project/oxabl/compare/oxabl_build-v0.2.1...oxabl_build-v0.2.2) (2026-11-11)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.3.1 to 0.3.2
    * oxabl_index bumped from 5.0.0 to 5.0.1
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0
    * oxabl_semantic bumped from 5.0.0 to 5.0.1
    * oxabl_workspace bumped from 3.3.0 to 3.4.0

## [0.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_build-v0.2.0...oxabl_build-v0.2.1) (2026-11-10)


### Bug Fixes

* **build:** show the skips that need no environment in a plan without --probe ([dcb376c](https://github.com/oxabl-project/oxabl/commit/dcb376c09dc449529303c8d9a28defd4839c8ba8))


### Performance Improvements

* **build:** keep store writes and cache copies off the dispatch thread ([dd1a048](https://github.com/oxabl-project/oxabl/commit/dd1a0484d181ffcd0b696b92f684b67e00626edf))
* **build:** share the $DLC hash phase across the workers and time the setup phases ([8863457](https://github.com/oxabl-project/oxabl/commit/88634579a50f54dd0256ff876f47b998c86bd678))
* **build:** share the $DLC hash phase across the workers; the plan without --probe shows path-only skips ([020b852](https://github.com/oxabl-project/oxabl/commit/020b8522af5b166c116bb4b5a330c67f93db2d6a))
* **build:** store writes and cache uploads leave the coordinator thread ([b2717a2](https://github.com/oxabl-project/oxabl/commit/b2717a2f68d5a45276a178bdc01a1a57526cdee9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.3.0 to 0.3.1
    * oxabl_index bumped from 4.0.0 to 5.0.0
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1
    * oxabl_semantic bumped from 4.0.0 to 5.0.0

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_build-v0.1.0...oxabl_build-v0.2.0) (2026-11-06)


### Features

* **build-remote:** add the remote cache of oxabl build behind the remote-cache feature ([08d47d4](https://github.com/oxabl-project/oxabl/commit/08d47d4da400855284038dc04e4cc4e0b0a478ae))
* **build:** add compile keys, keyability and keys in oxabl build --plan --probe ([09c3c11](https://github.com/oxabl-project/oxabl/commit/09c3c114819cce5e45082aaf29722c4e7eeeeea0))
* **build:** add oxabl build --plan --since, --json and the plan parity script ([2aaff6b](https://github.com/oxabl-project/oxabl/commit/2aaff6b51858513cd597a2440ad707afa8eb488f))
* **build:** add oxabl_build, the graph store, the incremental refresh and the planner ([bbc0b54](https://github.com/oxabl-project/oxabl/commit/bbc0b544a72596cd8bc856be3956a4bf823054f3))
* **build:** add the AVM worker pool, the OXW1 worker protocol and the compile environment ([4177873](https://github.com/oxabl-project/oxabl/commit/4177873e8d17ef69eaafd288e9720602934075aa))
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


### Bug Fixes

* **build:** a &gt; inside an XREF-XML attribute value does not end the element ([3874bf3](https://github.com/oxabl-project/oxabl/commit/3874bf3db908cfc82ee636f45e189dfd03fae130))
* **build:** a &gt; inside an XREF-XML attribute value does not end the element ([ae5fbe6](https://github.com/oxabl-project/oxabl/commit/ae5fbe65ca2a05e99e4fc441d11a8a8dbd6385e5))
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
* **build:** say first that an encrypted unit looks encrypted ([fa0c937](https://github.com/oxabl-project/oxabl/commit/fa0c9379f36a359fbaf010f6e72b4ee3836903ea))
* **build:** say first that an encrypted unit looks encrypted ([9756700](https://github.com/oxabl-project/oxabl/commit/9756700d8bd72d1202e22777df305e3e920c3768))
* **build:** take a class name from its compile name when the CLASS statement does not spell it ([1e709e6](https://github.com/oxabl-project/oxabl/commit/1e709e6ab0afaac39032021ec77817c972556c13))


### Performance Improvements

* **build:** promote fetched sets in batches and fill the local cache off the fetch thread ([1c772f3](https://github.com/oxabl-project/oxabl/commit/1c772f3ccf8690eaa34e3c1ae61746da09be1b1f))
* **build:** promote fetched sets in batches and fill the local cache off the fetch thread ([0d544a3](https://github.com/oxabl-project/oxabl/commit/0d544a3fe084e82addfd9c8ec2fc9a1b304438b9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_effects bumped from 0.2.2 to 0.3.0
    * oxabl_index bumped from 3.1.0 to 4.0.0
    * oxabl_lexer bumped from 2.0.0 to 2.0.1
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0
    * oxabl_semantic bumped from 3.0.2 to 4.0.0
    * oxabl_workspace bumped from 3.2.0 to 3.3.0
