# Changelog

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
