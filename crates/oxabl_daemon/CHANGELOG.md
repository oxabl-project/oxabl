# Changelog

## [0.11.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.10.0...oxabl_daemon-v0.11.0) (2026-11-27)


### ⚠ BREAKING CHANGES

* **ast:** Identifier.name is a NameId, expression fields are ExprId or ExprList, and the analysis entry points take the program's AstPools.

### Features

* **lsp:** complete the members of a class after obj: ([#620](https://github.com/oxabl-project/oxabl/issues/620)) ([c5f0f8d](https://github.com/oxabl-project/oxabl/commit/c5f0f8d6b232b74b1b83e1242b1cac310f39b779))


### Performance Improvements

* **ast:** keep names and expressions in per-program pools ([#632](https://github.com/oxabl-project/oxabl/issues/632)) ([e6fbebd](https://github.com/oxabl-project/oxabl/commit/e6fbebd1ec47740059c1557fd5e3118a4818df6a))
* **bench:** measure the first subtypes request after the workspace pass ([#617](https://github.com/oxabl-project/oxabl/issues/617)) ([efc7a6f](https://github.com/oxabl-project/oxabl/commit/efc7a6f400d0321f43b521602ced610a1198a263))
* **daemon:** keep the subtype index and build it in the background after the first request ([#622](https://github.com/oxabl-project/oxabl/issues/622)) ([57c62aa](https://github.com/oxabl-project/oxabl/commit/57c62aa199dc7781021a9d279fb60bd9102c182d))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 5.0.0 to 6.0.0
    * oxabl_ast bumped from 7.0.0 to 8.0.0
    * oxabl_common bumped from 1.6.0 to 2.0.0
    * oxabl_daemon_protocol bumped from 0.4.1 to 0.5.0
    * oxabl_index bumped from 7.0.0 to 8.0.0
    * oxabl_pipeline bumped from 5.0.0 to 6.0.0
    * oxabl_semantic bumped from 7.0.0 to 8.0.0
    * oxabl_workspace bumped from 3.8.0 to 4.0.0
  * dev-dependencies
    * oxabl_schema bumped from 0.6.2 to 0.7.0
    * oxabl_common bumped from 1.6.0 to 2.0.0
    * oxabl_pipeline bumped from 5.0.0 to 6.0.0

## [0.10.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.9.0...oxabl_daemon-v0.10.0) (2026-11-27)


### ⚠ BREAKING CHANGES

* **semantic:** Scope::kind, parent and owner_node are methods, not fields.

### Features

* **lsp:** code completion for keywords, snippets and names in scope ([#534](https://github.com/oxabl-project/oxabl/issues/534)) ([e600394](https://github.com/oxabl-project/oxabl/commit/e6003942dd5ee81663503acc3ed0422689f80348))
* **lsp:** complete records, fields, RUN targets, include files and preprocessor names ([#546](https://github.com/oxabl-project/oxabl/issues/546)) ([6d30754](https://github.com/oxabl-project/oxabl/commit/6d30754e74636091f7c90fc981dd98b5e5b2823c))
* **lsp:** go to implementation, super method and type hierarchy ([#531](https://github.com/oxabl-project/oxabl/issues/531)) ([578188c](https://github.com/oxabl-project/oxabl/commit/578188ca8f9ce6c59561500e67bddf0964838b13))
* **lsp:** reference-count code lenses ([#532](https://github.com/oxabl-project/oxabl/issues/532)) ([1af858b](https://github.com/oxabl-project/oxabl/commit/1af858b6fdaa262d205abe24d5e34a24b1491ed9))
* **lsp:** signature help and parameter-name inlay hints ([#533](https://github.com/oxabl-project/oxabl/issues/533)) ([eaeab6b](https://github.com/oxabl-project/oxabl/commit/eaeab6be41c16d152b1b3042b34d4244580d8046))


### Bug Fixes

* **daemon:** exit when the workspace root is gone or no client is left ([#588](https://github.com/oxabl-project/oxabl/issues/588)) ([f5bc646](https://github.com/oxabl-project/oxabl/commit/f5bc6466dc6384975ee46c2576e80e5609ae985f))
* **lsp:** an edit no longer waits for a running diagnostics analysis ([#609](https://github.com/oxabl-project/oxabl/issues/609)) ([5a5e83f](https://github.com/oxabl-project/oxabl/commit/5a5e83f26c7f9051de5a62b1e69956fcbad39b24))
* **lsp:** the proxy and the daemon never wait on each other ([#578](https://github.com/oxabl-project/oxabl/issues/578)) ([0563621](https://github.com/oxabl-project/oxabl/commit/056362150cd9572882909b436ece4758ab78d775))
* **lsp:** wait for a slow daemon start ([#545](https://github.com/oxabl-project/oxabl/issues/545)) ([8445706](https://github.com/oxabl-project/oxabl/commit/8445706fc1fa4ecd00a4d70335fef03cfffd8bc7))
* **navigation:** compare parameter lists after preprocessing ([#568](https://github.com/oxabl-project/oxabl/issues/568)) ([a8183ec](https://github.com/oxabl-project/oxabl/commit/a8183ecfd583132f2a0f4ab2f73f97a5b79657c8))


### Performance Improvements

* **bench:** end-to-end and memory benchmarks, and a generated ERP-sized workspace ([#569](https://github.com/oxabl-project/oxabl/issues/569)) ([e819edc](https://github.com/oxabl-project/oxabl/commit/e819edcfd261e43755178e6e20c44cc650b757e4))
* **bench:** measure the whole edit pass on CodSpeed ([#605](https://github.com/oxabl-project/oxabl/issues/605)) ([d93f472](https://github.com/oxabl-project/oxabl/commit/d93f472fabe57fe83e6daf9f0076237085f539d8))
* **bench:** measure the workspace pass after an edit ([#604](https://github.com/oxabl-project/oxabl/issues/604)) ([e754399](https://github.com/oxabl-project/oxabl/commit/e754399e79157a2a5c8cb8ef27202af7dd315f91))
* **check:** find SHARED producers with one expansion and a token scan ([#582](https://github.com/oxabl-project/oxabl/issues/582)) ([c0da43d](https://github.com/oxabl-project/oxabl/commit/c0da43da8f68f284a421fa5e7c9cbb235cf76a2d))
* **check:** spell and resolve each include path once ([#608](https://github.com/oxabl-project/oxabl/issues/608)) ([793a0d9](https://github.com/oxabl-project/oxabl/commit/793a0d94b98ba7c498b7f941ff9b02e8957cfe0a))
* **completion:** lex only what a request changes in its base's text ([#610](https://github.com/oxabl-project/oxabl/issues/610)) ([b619d63](https://github.com/oxabl-project/oxabl/commit/b619d638c5ac2c33a552251c3a188cb9d00a4cc3))
* **daemon:** analyse only the changed files in the workspace pass after an edit ([#594](https://github.com/oxabl-project/oxabl/issues/594)) ([cc5ca85](https://github.com/oxabl-project/oxabl/commit/cc5ca85f471f25bc426a56ab5e645b99fe75c5d6))
* **daemon:** cut the fixed cost of a workspace pass ([#611](https://github.com/oxabl-project/oxabl/issues/611)) ([2a3c781](https://github.com/oxabl-project/oxabl/commit/2a3c781abbc06dfad83963a134ae136b53d6e960))
* **lsp:** resolve the reference lenses of a file in one search ([#592](https://github.com/oxabl-project/oxabl/issues/592)) ([91d9306](https://github.com/oxabl-project/oxabl/commit/91d930645dbc1d54ae5e9ec4a3c0eddc194bfc9b))
* **semantic:** find the scope of a statement in a table, and shrink a scope to 48 bytes ([#584](https://github.com/oxabl-project/oxabl/issues/584)) ([258f393](https://github.com/oxabl-project/oxabl/commit/258f393c9f582c77eeb3a4c38defe87a963b2339))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.5.0 to 5.0.0
    * oxabl_ast bumped from 6.0.0 to 7.0.0
    * oxabl_common bumped from 1.5.0 to 1.6.0
    * oxabl_index bumped from 6.0.1 to 7.0.0
    * oxabl_pipeline bumped from 4.1.0 to 5.0.0
    * oxabl_semantic bumped from 6.0.1 to 7.0.0
    * oxabl_workspace bumped from 3.7.1 to 3.8.0
  * dev-dependencies
    * oxabl_schema bumped from 0.6.1 to 0.6.2
    * oxabl_common bumped from 1.5.0 to 1.6.0
    * oxabl_pipeline bumped from 4.1.0 to 5.0.0

## [0.9.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.8.0...oxabl_daemon-v0.9.0) (2026-11-25)


### Features

* **lsp:** semantic tokens for the names oxabl resolves ([0e04b4c](https://github.com/oxabl-project/oxabl/commit/0e04b4c154c2811773002c1c2dffe56c86a3151a))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.4.0 to 4.5.0
    * oxabl_common bumped from 1.4.0 to 1.5.0
    * oxabl_index bumped from 6.0.0 to 6.0.1
    * oxabl_pipeline bumped from 4.0.0 to 4.1.0
    * oxabl_semantic bumped from 6.0.0 to 6.0.1
    * oxabl_workspace bumped from 3.7.0 to 3.7.1
  * dev-dependencies
    * oxabl_schema bumped from 0.6.0 to 0.6.1
    * oxabl_common bumped from 1.4.0 to 1.5.0
    * oxabl_pipeline bumped from 4.0.0 to 4.1.0

## [0.8.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.7.0...oxabl_daemon-v0.8.0) (2026-11-24)


### Features

* **lsp:** dim inactive preprocessor regions ([cc57d5e](https://github.com/oxabl-project/oxabl/commit/cc57d5efd24bbb6b6880abc6276ec0f7460133f6))
* **lsp:** find references and the call hierarchy ([4fbab73](https://github.com/oxabl-project/oxabl/commit/4fbab733b91d8594a30071b5515bdd5b1cf73703))
* **lsp:** highlight the other uses of the name under the cursor ([47df7a5](https://github.com/oxabl-project/oxabl/commit/47df7a5a62c4b76358ef6a7c45a4336014561736))
* **lsp:** hover with types, signatures, built-in docs and schema fields ([ca50283](https://github.com/oxabl-project/oxabl/commit/ca502838f6343f5f28a225df5e8b64020b304263))
* **lsp:** inlay hints for locks, index, buffer scope and transactions ([5358467](https://github.com/oxabl-project/oxabl/commit/535846717f49af4de07e31280201cd7f76de8cbd))
* **lsp:** related locations on unknown-value and handle-leak diagnostics ([2bd42ec](https://github.com/oxabl-project/oxabl/commit/2bd42ec174d6051e58327e78277516accbdb172b))
* **lsp:** show a routine's database effects on hover ([772b5c6](https://github.com/oxabl-project/oxabl/commit/772b5c6943c456e18f080cb1ab7bc6dc89550e62))


### Bug Fixes

* **daemon:** exit after an idle timeout and remove the registration ([94a4214](https://github.com/oxabl-project/oxabl/commit/94a4214272c16c653ce4ed3ae60302c6aa89a22b))
* **lsp:** replace a daemon of another build of the same version ([0285b5d](https://github.com/oxabl-project/oxabl/commit/0285b5d3d9ccff5e64d77956467e15691b3fcc6b))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.3.0 to 4.4.0
    * oxabl_ast bumped from 5.0.0 to 6.0.0
    * oxabl_common bumped from 1.3.1 to 1.4.0
    * oxabl_daemon_protocol bumped from 0.4.0 to 0.4.1
    * oxabl_index bumped from 5.3.0 to 6.0.0
    * oxabl_pipeline bumped from 3.8.0 to 4.0.0
    * oxabl_semantic bumped from 5.2.0 to 6.0.0
    * oxabl_workspace bumped from 3.6.2 to 3.7.0
  * dev-dependencies
    * oxabl_schema bumped from 0.5.0 to 0.6.0
    * oxabl_common bumped from 1.3.1 to 1.4.0
    * oxabl_pipeline bumped from 3.8.0 to 4.0.0

## [0.7.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.6.1...oxabl_daemon-v0.7.0) (2026-11-23)


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


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.2.2 to 4.3.0
    * oxabl_common bumped from 1.3.0 to 1.3.1
    * oxabl_index bumped from 5.2.1 to 5.3.0
    * oxabl_pipeline bumped from 3.7.0 to 3.8.0
    * oxabl_semantic bumped from 5.1.0 to 5.2.0
    * oxabl_workspace bumped from 3.6.1 to 3.6.2
  * dev-dependencies
    * oxabl_schema bumped from 0.4.7 to 0.5.0
    * oxabl_common bumped from 1.3.0 to 1.3.1
    * oxabl_pipeline bumped from 3.7.0 to 3.8.0

## [0.6.1](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.6.0...oxabl_daemon-v0.6.1) (2026-11-20)


### Bug Fixes

* **parser:** stop at a nesting and chain limit, and parse on a stack of known size ([7b741da](https://github.com/oxabl-project/oxabl/commit/7b741dae5637ba90408c8ac0a857464a5c44ee13))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.2.1 to 4.2.2
    * oxabl_ast bumped from 4.0.0 to 5.0.0
    * oxabl_common bumped from 1.2.2 to 1.3.0
    * oxabl_index bumped from 5.2.0 to 5.2.1
    * oxabl_pipeline bumped from 3.6.1 to 3.7.0
    * oxabl_semantic bumped from 5.0.5 to 5.1.0
    * oxabl_workspace bumped from 3.6.0 to 3.6.1
  * dev-dependencies
    * oxabl_schema bumped from 0.4.6 to 0.4.7
    * oxabl_common bumped from 1.2.2 to 1.3.0
    * oxabl_pipeline bumped from 3.6.1 to 3.7.0

## [0.6.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.10...oxabl_daemon-v0.6.0) (2026-11-18)


### ⚠ BREAKING CHANGES

* **daemon:** CONTRACT_VERSION is 5, because Registration has a new field. A contract 4 client and a contract 5 daemon refuse each other.

### Bug Fixes

* **daemon:** a registration names the daemon's build ([475b323](https://github.com/oxabl-project/oxabl/commit/475b3237d05090bdcd440724d35afd6c063c48a8))
* **daemon:** replace a daemon that outlived an upgrade ([d556c51](https://github.com/oxabl-project/oxabl/commit/d556c516cb2146066d3e557c89ac34cf1e19b1aa))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_daemon_protocol bumped from 0.3.0 to 0.4.0

## [0.5.10](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.9...oxabl_daemon-v0.5.10) (2026-11-18)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.2.0 to 4.2.1
    * oxabl_index bumped from 5.1.0 to 5.2.0
    * oxabl_pipeline bumped from 3.6.0 to 3.6.1
    * oxabl_semantic bumped from 5.0.4 to 5.0.5
    * oxabl_workspace bumped from 3.5.0 to 3.6.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.5 to 0.4.6
    * oxabl_pipeline bumped from 3.6.0 to 3.6.1

## [0.5.9](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.8...oxabl_daemon-v0.5.9) (2026-11-17)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.1.0 to 4.2.0
    * oxabl_pipeline bumped from 3.5.0 to 3.6.0
  * dev-dependencies
    * oxabl_pipeline bumped from 3.5.0 to 3.6.0

## [0.5.8](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.7...oxabl_daemon-v0.5.8) (2026-11-16)


### Bug Fixes

* **build:** resolve includes of a class from its package root, as the compiler does ([954ce9c](https://github.com/oxabl-project/oxabl/commit/954ce9c1fb6d893eae9859fa15eb5f4663d9667d))
* **build:** resolve includes of a class from its package root, as the compiler does ([c95549f](https://github.com/oxabl-project/oxabl/commit/c95549f2a5e5418bc69585fcae56c02b07a3ec8d))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.4 to 4.1.0
    * oxabl_index bumped from 5.0.3 to 5.1.0
    * oxabl_pipeline bumped from 3.4.2 to 3.5.0
    * oxabl_semantic bumped from 5.0.3 to 5.0.4
  * dev-dependencies
    * oxabl_pipeline bumped from 3.4.2 to 3.5.0

## [0.5.7](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.6...oxabl_daemon-v0.5.7) (2026-11-13)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.3 to 4.0.4
    * oxabl_index bumped from 5.0.2 to 5.0.3
    * oxabl_pipeline bumped from 3.4.1 to 3.4.2
    * oxabl_semantic bumped from 5.0.2 to 5.0.3
    * oxabl_workspace bumped from 3.4.0 to 3.5.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.4 to 0.4.5
    * oxabl_pipeline bumped from 3.4.1 to 3.4.2

## [0.5.6](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.5...oxabl_daemon-v0.5.6) (2026-11-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.2 to 4.0.3
    * oxabl_index bumped from 5.0.1 to 5.0.2
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1
    * oxabl_semantic bumped from 5.0.1 to 5.0.2
  * dev-dependencies
    * oxabl_pipeline bumped from 3.4.0 to 3.4.1

## [0.5.5](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.4...oxabl_daemon-v0.5.5) (2026-11-11)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.1 to 4.0.2
    * oxabl_index bumped from 5.0.0 to 5.0.1
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0
    * oxabl_semantic bumped from 5.0.0 to 5.0.1
    * oxabl_workspace bumped from 3.3.0 to 3.4.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.3 to 0.4.4
    * oxabl_pipeline bumped from 3.3.1 to 3.4.0

## [0.5.4](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.3...oxabl_daemon-v0.5.4) (2026-11-10)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 4.0.0 to 4.0.1
    * oxabl_index bumped from 4.0.0 to 5.0.0
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1
    * oxabl_semantic bumped from 4.0.0 to 5.0.0
  * dev-dependencies
    * oxabl_pipeline bumped from 3.3.0 to 3.3.1

## [0.5.3](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.2...oxabl_daemon-v0.5.3) (2026-11-06)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 3.0.2 to 4.0.0
    * oxabl_ast bumped from 3.0.0 to 4.0.0
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_index bumped from 3.1.0 to 4.0.0
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0
    * oxabl_semantic bumped from 3.0.2 to 4.0.0
    * oxabl_workspace bumped from 3.2.0 to 3.3.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.2 to 0.4.3
    * oxabl_common bumped from 1.2.1 to 1.2.2
    * oxabl_pipeline bumped from 3.2.0 to 3.3.0

## [0.5.2](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.1...oxabl_daemon-v0.5.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 3.0.1 to 3.0.2
    * oxabl_index bumped from 3.0.1 to 3.1.0
    * oxabl_pipeline bumped from 3.1.0 to 3.2.0
    * oxabl_semantic bumped from 3.0.1 to 3.0.2
    * oxabl_workspace bumped from 3.1.0 to 3.2.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_pipeline bumped from 3.1.0 to 3.2.0

## [0.5.1](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.5.0...oxabl_daemon-v0.5.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 3.0.0 to 3.0.1
    * oxabl_index bumped from 3.0.0 to 3.0.1
    * oxabl_pipeline bumped from 3.0.0 to 3.1.0
    * oxabl_semantic bumped from 3.0.0 to 3.0.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0
  * dev-dependencies
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_pipeline bumped from 3.0.0 to 3.1.0

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.4.0...oxabl_daemon-v0.5.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **daemon:** `ReverseGraph::build_with` takes the file list, a job count, a factory for per-worker run handles and an `Fn` inspect that returns a value, and returns the graph together with the inspected values. `ReverseGraph` now derives `Debug` and `PartialEq`.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** index_file takes an IncludeContext instead of a PreprocessorConfig. SharedProducers::build and definitions_in take an IncludeContext instead of a filesystem and a PreprocessorConfig. IndexHandle::set_preprocessor is replaced by set_environment, which takes the PipelineConfig, and IndexEnvironment gains an include_paths field.
* **index:** SourcesConfig and PipelineConfig gain a pub case_insensitive_lookup field.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **index:** add opt-in case-insensitive class and program lookup ([6b14dc8](https://github.com/oxabl-project/oxabl/commit/6b14dc808ed1118c3634eee3a59cd47f6f44afe9)), closes [#214](https://github.com/oxabl-project/oxabl/issues/214)
* **index:** answer parameter lists and workspace procedure names ([b76cbec](https://github.com/oxabl-project/oxabl/commit/b76cbec66a9e4d2e41c275bfd03d442184e32f5b))
* **index:** index classes built from includes through the preprocessor ([1d80f13](https://github.com/oxabl-project/oxabl/commit/1d80f13fed347e40f75eb88940f54db79bcbec1d)), closes [#225](https://github.com/oxabl-project/oxabl/issues/225)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **lsp:** code lens for database effect summaries ([530aa7b](https://github.com/oxabl-project/oxabl/commit/530aa7b1cd76e46d4b25e3e4db6082f299bd99ec))
* **lsp:** show cached database effect summaries as code lenses ([2924b89](https://github.com/oxabl-project/oxabl/commit/2924b89d1fffc950307c8b3453fce42c8ee7c80c))
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **check:** scan the SHARED producer map on every core ([fa951a2](https://github.com/oxabl-project/oxabl/commit/fa951a247a9754d992970c543437ba5d0d159b7d))
* **daemon:** build the cold workspace pass on every core ([4efe428](https://github.com/oxabl-project/oxabl/commit/4efe428e7774965b106413dc90cf48a6ebc0dd9c)), closes [#170](https://github.com/oxabl-project/oxabl/issues/170)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 2.0.0 to 3.0.0
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_pipeline bumped from 2.0.0 to 3.0.0

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.3.2...oxabl_daemon-v0.4.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **daemon:** oxabl_daemon::db::IndexedFile no longer has a preprocessor field; IndexedFile::new takes three arguments. The value lives on the new IndexEnvironment input.
* **index:** oxabl_index::index_file takes a third argument, &PreprocessorConfig.
* **workspace:** PipelineConfig gains the public run_extensions field.

### Features

* **index:** record unlinked RUN targets and the files behind unusable answers ([432a323](https://github.com/oxabl-project/oxabl/commit/432a3236ad2bc10887ff945c0b5d00ac26e3b8df)), closes [#212](https://github.com/oxabl-project/oxabl/issues/212) [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **workspace:** configure the extension order for an extensionless RUN ([77c1e5f](https://github.com/oxabl-project/oxabl/commit/77c1e5fdf2dfee4b9264122c8f838310a2af0bc1))


### Bug Fixes

* **daemon:** expand a buffer under its own file name ([671a9a0](https://github.com/oxabl-project/oxabl/commit/671a9a0f16a323f8d3c452f4cc751955d2d75b5a))
* **daemon:** read the preprocessor environment from one shared input ([8572ac9](https://github.com/oxabl-project/oxabl/commit/8572ac9a5630ea4697b61b03e26be013a0052894))
* **index:** apply the configured predefined names to a file's own directives ([c925a40](https://github.com/oxabl-project/oxabl/commit/c925a40fb7ce99b4b309dd31ac69e13568704c0b))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.4 to 2.0.0
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_index bumped from 1.0.3 to 2.0.0
    * oxabl_pipeline bumped from 1.2.1 to 2.0.0
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0
  * dev-dependencies
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_pipeline bumped from 1.2.1 to 2.0.0

## [0.3.2](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.3.1...oxabl_daemon-v0.3.2) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.3 to 1.1.4
    * oxabl_index bumped from 1.0.2 to 1.0.3
    * oxabl_pipeline bumped from 1.2.0 to 1.2.1
    * oxabl_semantic bumped from 1.1.0 to 1.1.1
  * dev-dependencies
    * oxabl_pipeline bumped from 1.2.0 to 1.2.1

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.3.0...oxabl_daemon-v0.3.1) (2026-09-21)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.2 to 1.1.3
    * oxabl_index bumped from 1.0.1 to 1.0.2
    * oxabl_pipeline bumped from 1.1.2 to 1.2.0
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
  * dev-dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0
    * oxabl_pipeline bumped from 1.1.2 to 1.2.0

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.2.0...oxabl_daemon-v0.3.0) (2026-09-21)


### ⚠ BREAKING CHANGES

* **daemon:** the daemon's socket, lock and registration live under $XDG_RUNTIME_DIR/oxabl/daemon instead of the cache directory, and discovery is keyed on the canonical workspace root. A client built before this change will not discover a daemon built after it, and vice versa; a daemon left running at the old location is not adopted and exits on its own idle timeout. When XDG_RUNTIME_DIR is unset or empty the chain falls back to XDG_CACHE_HOME, then $HOME/.cache, then the system temp directory, each warning about the guarantee it gives up. A value that is set but is not an absolute path is refused outright, naming the variable and the value, rather than falling through.

### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))
* **daemon:** close the registry's symlink hole and stop the probe taking the lock it tests ([#173](https://github.com/oxabl-project/oxabl/issues/173)) ([3fa0bc2](https://github.com/oxabl-project/oxabl/commit/3fa0bc2c402cab88218372fed7d9c3400148bc05))
* **daemon:** detect added files in freshness, and correct span extents and lookup cost ([#187](https://github.com/oxabl-project/oxabl/issues/187)) ([d50be6a](https://github.com/oxabl-project/oxabl/commit/d50be6a897742e622b0c4f281de4a6d1d9c2c355))
* **daemon:** move the socket and lock to the runtime directory, and key discovery on a canonical root ([#186](https://github.com/oxabl-project/oxabl/issues/186)) ([68c6a6f](https://github.com/oxabl-project/oxabl/commit/68c6a6f571013e47d694e7685de1ace222e3b7ce))
* **daemon:** shared session correctness, and the contract bump that covers the series ([#174](https://github.com/oxabl-project/oxabl/issues/174)) ([39bba72](https://github.com/oxabl-project/oxabl/commit/39bba72396c8c4a8b241056b91d44b776d068a31))
* **daemon:** the socket moved, so an older client no longer finds a daemon ([77ab645](https://github.com/oxabl-project/oxabl/commit/77ab645f5d1813e98d4c5d21ceb14473f68da920))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.1 to 1.1.2
    * oxabl_daemon_protocol bumped from 0.2.0 to 0.3.0
    * oxabl_index bumped from 1.0.0 to 1.0.1
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
    * oxabl_workspace bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_pipeline bumped from 1.1.1 to 1.1.2

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_daemon-v0.1.1...oxabl_daemon-v0.2.0) (2026-08-14)


### Features

* the daemon session core, and the wire a client can afford ([#165](https://github.com/oxabl-project/oxabl/issues/165)) ([5184c04](https://github.com/oxabl-project/oxabl/commit/5184c0470ebe55c83241ef8c72a5976d4e6de235))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_analyze bumped from 1.1.0 to 1.1.1
    * oxabl_daemon_protocol bumped from 0.1.0 to 0.2.0
    * oxabl_index bumped from 0.1.2 to 1.0.0
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
    * oxabl_workspace bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_pipeline bumped from 1.1.0 to 1.1.1
