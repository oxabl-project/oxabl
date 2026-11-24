# Changelog

## [0.12.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.11.0...oxabl_vscode-v0.12.0) (2026-11-24)


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


### Bug Fixes

* **lint:** read date constants as DATE, accept converted INITIAL constants, and place LINT0004 findings ([1baf226](https://github.com/oxabl-project/oxabl/commit/1baf226acce5ae3a89372a1b45835e3f28b9776b))
* **preprocessor:** end an include reference at the first } as the compiler does ([2072df5](https://github.com/oxabl-project/oxabl/commit/2072df58afae02cb78b1861afeeeda065d50ed7a))
* **vscode:** bound the nesting of comments and include references in the grammar ([66cb2f5](https://github.com/oxabl-project/oxabl/commit/66cb2f524d1f0f29e1084c25d00bbdb22b969751))
* **vscode:** lead the nested define match with a word boundary, not a look-behind ([96697e0](https://github.com/oxabl-project/oxabl/commit/96697e0983b5401c0f1a94129b5b2cf393532918))


### Performance Improvements

* **vscode:** lead keyword rules with a word boundary, not a look-behind ([27de1bb](https://github.com/oxabl-project/oxabl/commit/27de1bbf184cf053cea2e358b64a577c62ca4f66))

## [0.11.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.10.0...oxabl_vscode-v0.11.0) (2026-11-23)


### Features

* **lsp:** answer workspace/symbol from the workspace index ([c7f8f90](https://github.com/oxabl-project/oxabl/commit/c7f8f90b7c6dd18b66fc29a0cd311dfe6181aae5))
* **lsp:** answer workspace/symbol from the workspace index ([c6680be](https://github.com/oxabl-project/oxabl/commit/c6680bed6ececab2739fbe4e673660fe5953078c)), closes [#406](https://github.com/oxabl-project/oxabl/issues/406)
* **lsp:** go to definition and document links ([9a89c3d](https://github.com/oxabl-project/oxabl/commit/9a89c3d344cee9ac558d7a9218565496066da4d0))
* **lsp:** go to definition and document links ([1df981d](https://github.com/oxabl-project/oxabl/commit/1df981dfba85c1ffadd26219b36e402a881ef6ca)), closes [#407](https://github.com/oxabl-project/oxabl/issues/407) [#408](https://github.com/oxabl-project/oxabl/issues/408)
* **lsp:** inspect the expanded compilation unit of a file ([faa7bf8](https://github.com/oxabl-project/oxabl/commit/faa7bf81431e7bf794510fa1e95c10bfc6d8c985))
* **lsp:** inspect the expanded compilation unit of a file ([57e65b6](https://github.com/oxabl-project/oxabl/commit/57e65b61801d9a25f57f30c516adf3a0e02f766c)), closes [#169](https://github.com/oxabl-project/oxabl/issues/169)
* **lsp:** select an includer context for an open include ([18a621e](https://github.com/oxabl-project/oxabl/commit/18a621eca5d4de5eb875f3214ae0f319f06f33cc))
* **lsp:** select an includer context for an open include ([99a3291](https://github.com/oxabl-project/oxabl/commit/99a3291874d6628bfe9ae96d234fe7f82543dc1f)), closes [#168](https://github.com/oxabl-project/oxabl/issues/168)

## [0.10.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.9.0...oxabl_vscode-v0.10.0) (2026-11-20)


### Features

* ABL document outline, and no-setup defaults for the editor ([375b8c4](https://github.com/oxabl-project/oxabl/commit/375b8c4704d4de20e8104c188fb0b9b7d6f2f855))
* **vscode:** offer the oxabl CLI without an oxabl.toml ([4451768](https://github.com/oxabl-project/oxabl/commit/4451768b7cab5a28f654d1adbc0f6dfc538cbdb0))

## [0.9.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.8.0...oxabl_vscode-v0.9.0) (2026-11-18)


### Features

* **build:** r-code beside the source with [build].rcode = "beside-source" ([c4c88a7](https://github.com/oxabl-project/oxabl/commit/c4c88a7de0162dcf0a9fd792f699405125a2aa9d))
* **build:** r-code beside the source with [build].rcode = "beside-source" ([e317d75](https://github.com/oxabl-project/oxabl/commit/e317d758247fceab45b13d82891f9025b6d7bb04))
* **vscode:** build a root with r-code beside the source ([4c3473a](https://github.com/oxabl-project/oxabl/commit/4c3473abe43a6cd618c4c11da27e373217dafe50))

## [0.8.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.7.1...oxabl_vscode-v0.8.0) (2026-11-17)


### Features

* **build:** write a .gitignore into the directory that holds oxabl's output ([35150ca](https://github.com/oxabl-project/oxabl/commit/35150ca43b1a13855b2d2dc80825b3fa0a5d313a))
* onboarding polish: a .gitignore of oxabl's own, a first-build tooltip, notices once per workspace ([d6f7c24](https://github.com/oxabl-project/oxabl/commit/d6f7c242d7bd7c843fbe555883f060f9b3206eed))
* **vscode:** say in the tooltip that a first build takes several minutes ([a2bfd92](https://github.com/oxabl-project/oxabl/commit/a2bfd926768afe18ae1c10570c9f072bd0378a21))


### Bug Fixes

* **vscode:** show a warning about units without a key once per workspace ([cb2dad4](https://github.com/oxabl-project/oxabl/commit/cb2dad4dc42998670f31217d49d102d3cbbee620))

## [0.7.1](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.7.0...oxabl_vscode-v0.7.1) (2026-11-16)


### Bug Fixes

* **build:** report compiler warnings that have no position ([fbdbddc](https://github.com/oxabl-project/oxabl/commit/fbdbddc1f322da9bb0369644bd809c727a231689))
* **build:** report compiler warnings that have no position ([07daeae](https://github.com/oxabl-project/oxabl/commit/07daeaeec05176b50681a4f8c57e77d8b32ac944))

## [0.7.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.6.1...oxabl_vscode-v0.7.0) (2026-11-16)


### Features

* **vscode:** update a managed oxabl CLI with the extension's update check ([312afa2](https://github.com/oxabl-project/oxabl/commit/312afa2c9f4b19a162b8304e93c8846c11012894))
* **vscode:** update a managed oxabl CLI with the extension's update check ([52b4db4](https://github.com/oxabl-project/oxabl/commit/52b4db4f132401e0ea2e5859e3e1409279ab5566))

## [0.6.1](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.6.0...oxabl_vscode-v0.6.1) (2026-11-16)


### Bug Fixes

* **vscode:** keep the building tooltip still while units finish ([3538784](https://github.com/oxabl-project/oxabl/commit/353878461521743acf65b4364b52ce64a16f6465))
* **vscode:** keep the building tooltip still while units finish ([e8cb367](https://github.com/oxabl-project/oxabl/commit/e8cb36724dc4c013ed7f97a645ddbbdc50270bd5))

## [0.6.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.5.0...oxabl_vscode-v0.6.0) (2026-11-13)


### Features

* **build:** bind verification to the cache mode and pace schema checks by a time budget ([642c8ce](https://github.com/oxabl-project/oxabl/commit/642c8ce19cd43a2aa336bf61e88ef1d20244e268))
* **build:** bind verification to the cache mode and pace V2 by a time budget ([1ef6165](https://github.com/oxabl-project/oxabl/commit/1ef616561087ca27bdd69de0408313cd154745de))


### Bug Fixes

* **vscode:** show the workspace share that needs no compile in the status bar ([958a086](https://github.com/oxabl-project/oxabl/commit/958a086f9e70671c6d08abc7bcec93f7b9744746))
* **vscode:** show the workspace share that needs no compile in the status bar ([7f879e9](https://github.com/oxabl-project/oxabl/commit/7f879e9b7920a69187a5995fe889b295d49617fc))

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.4.1...oxabl_vscode-v0.5.0) (2026-11-13)


### Features

* **vscode:** warn when units have no compile key, and show the up-to-date units ([6ae98a6](https://github.com/oxabl-project/oxabl/commit/6ae98a60e236085e8d8165d285b155445295a8a5))
* **vscode:** warn when units have no compile key, and show the up-to-date units ([913c7f4](https://github.com/oxabl-project/oxabl/commit/913c7f4764442d0b988c0146a107970e1827139a))

## [0.4.1](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.4.0...oxabl_vscode-v0.4.1) (2026-11-13)


### Bug Fixes

* **vscode:** keep the build spinner turning while units finish ([95d6b5d](https://github.com/oxabl-project/oxabl/commit/95d6b5da9f8d8c883e380a4622d358d8ba93f5dc))
* **vscode:** keep the build spinner turning while units finish ([75d4a4c](https://github.com/oxabl-project/oxabl/commit/75d4a4cab979bb7a9a8c799f49531e416f22b753))

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.3.2...oxabl_vscode-v0.4.0) (2026-11-12)


### Features

* **vscode:** document the build loop and test it against a fake oxabl ([c36e010](https://github.com/oxabl-project/oxabl/commit/c36e01056a28b45a436e96ccd5e2b3d5b0f2505a))
* **vscode:** install the CLI and build on open and on save ([4064da5](https://github.com/oxabl-project/oxabl/commit/4064da5d45f75c72ba39ac9b5d55b197bc9fdec3))
* **vscode:** install the CLI and build on open and on save ([26c998a](https://github.com/oxabl-project/oxabl/commit/26c998a0cc1e4b67f803c29f9a3a9aab165cade3))


### Bug Fixes

* **vscode:** install the newest release over a managed binary that has no self-update ([122f0cb](https://github.com/oxabl-project/oxabl/commit/122f0cb900914dcc253626235e2e0ce606b679c5))

## [0.3.2](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.3.1...oxabl_vscode-v0.3.2) (2026-11-11)


### Bug Fixes

* **vscode:** a table named like an unreserved keyword is a name ([7b18ad9](https://github.com/oxabl-project/oxabl/commit/7b18ad931c08e3261c615a590c27e61a93328609))
* **vscode:** a table named like an unreserved keyword is a name ([b6e211a](https://github.com/oxabl-project/oxabl/commit/b6e211a649e50d7ed7a6fb431e5ca09146ca3888))

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.3.0...oxabl_vscode-v0.3.1) (2026-11-11)


### Bug Fixes

* **vscode:** show no message when the oxabl program is missing ([f092338](https://github.com/oxabl-project/oxabl/commit/f092338e93674f888f8e9aa4a0c3f27139cc5b46))
* **vscode:** show no message when the oxabl program is missing ([a429991](https://github.com/oxabl-project/oxabl/commit/a429991be14994eee105586ffa68773e251c080d))

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.2.0...oxabl_vscode-v0.3.0) (2026-11-11)


### Features

* **config:** turn diagnostics and formatting on or off in [workspace.features] ([5f9d715](https://github.com/oxabl-project/oxabl/commit/5f9d715a394471319298f6895077ba9179d408c1))
* **config:** turn diagnostics and formatting on or off in [workspace.features] ([55e57ec](https://github.com/oxabl-project/oxabl/commit/55e57ecef9141909908b7416593d21b983f70932))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_vscode-v0.1.0...oxabl_vscode-v0.2.0) (2026-11-10)


### ⚠ BREAKING CHANGES

* **workspace:** `SourcesConfig` gains the public fields `encoding` and `fallback_encoding`, and `PipelineConfig` gains the public field `decoding`. Code that builds either with a struct literal must set them.
* **workspace:** `SourcesConfig` has a new pub field, `root_relative_include_paths`; `PipelineConfig` has a new pub field, `root_relative_include_paths`; `IncludeContext` has a new pub field, `root_relative_paths`.
* **workspace:** SourcesConfig and PipelineConfig gain a pub root_extensions field.
* **index:** SourcesConfig and PipelineConfig gain a pub case_insensitive_lookup field.

### Features

* **build:** config overlay, effective config, and the wrapper guide ([df21e74](https://github.com/oxabl-project/oxabl/commit/df21e74656c08b87ee18828621f65e49e435b5f1))
* **build:** graph store, incremental refresh and oxabl build --plan ([50d3004](https://github.com/oxabl-project/oxabl/commit/50d3004216da610003c27a3a1430bdb97fdfcbe3))
* **build:** merge an overlay over oxabl.toml and print the effective configuration ([5d26d87](https://github.com/oxabl-project/oxabl/commit/5d26d877d70be0423690310fa05442d8405367f3))
* **build:** verify compile results and cache them in a local cache ([f87a7b0](https://github.com/oxabl-project/oxabl/commit/f87a7b04a2784c79dc74f621e993b3ec49b10f06))
* **build:** verify compile results and cache them in a local cache ([61ba98e](https://github.com/oxabl-project/oxabl/commit/61ba98e6b856bdc08bb5814bf09c090a9ca15010))
* **index:** add opt-in case-insensitive class and program lookup ([6b14dc8](https://github.com/oxabl-project/oxabl/commit/6b14dc808ed1118c3634eee3a59cd47f6f44afe9)), closes [#214](https://github.com/oxabl-project/oxabl/issues/214)
* **vscode:** ABL syntax highlighting, extension releases and self-update ([8ee30f8](https://github.com/oxabl-project/oxabl/commit/8ee30f8be3d7075224c756447efbfd932059caf9))
* **vscode:** ship a TextMate grammar and release the extension on its own ([de9d56b](https://github.com/oxabl-project/oxabl/commit/de9d56b6d35a9b343c5766b91de7cd5a554efa67))
* **vscode:** update the extension from GitHub Releases ([0db1ee8](https://github.com/oxabl-project/oxabl/commit/0db1ee84440da9531a247ae37e3130e98567c6c4))
* **workspace:** add the [build] tables, [workspace.sources].dlc and $DLC include path entries ([4dae3e6](https://github.com/oxabl-project/oxabl/commit/4dae3e69e8308fb3a2ccaf2982cc2f7339a5b62e))
* **workspace:** make the root extension set configurable ([8994bb4](https://github.com/oxabl-project/oxabl/commit/8994bb4b21dc869b3c05be77310dd8324049b7e8)), closes [#143](https://github.com/oxabl-project/oxabl/issues/143)
* **workspace:** read source files in a configured encoding ([68164e3](https://github.com/oxabl-project/oxabl/commit/68164e3e46912235b76836838879080fc17f0869))
* **workspace:** search each root's own directory first with root_relative_include_paths ([1793dc7](https://github.com/oxabl-project/oxabl/commit/1793dc7d65eaba696cf115172a04422b2f5b061a))


### Bug Fixes

* **cli:** drop internal requirement tags from help and schema text ([f1d1dfc](https://github.com/oxabl-project/oxabl/commit/f1d1dfc6d93c4543f8da31cf3d5c7278bba7515a))
* **cli:** the schema lists every encoding the binary accepts ([791ada6](https://github.com/oxabl-project/oxabl/commit/791ada6192e3db1e11269985e08402efba2b808b))
* **cli:** user-facing gaps found while writing the user docs ([d34d5e7](https://github.com/oxabl-project/oxabl/commit/d34d5e790391fddecf68ced90ba79edd8c20e432))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **vscode:** a pending reload needs this profile's record and the folder on disk ([baf0dca](https://github.com/oxabl-project/oxabl/commit/baf0dca0e407a929fd6b07d136ddbb0cc4a7b1dd))
* **vscode:** an indented DO: or FINALLY: is a keyword, not a label ([a26559e](https://github.com/oxabl-project/oxabl/commit/a26559e19ae54dfedd8e4280069bd9c9c1aca2dc))
* **vscode:** decide a pending reload from this host's extensions folder ([ad12f50](https://github.com/oxabl-project/oxabl/commit/ad12f501150e4b7a435217bfd02740e0a378e6a5))
* **vscode:** harden the self-update after review ([ba97fbc](https://github.com/oxabl-project/oxabl/commit/ba97fbc95776aeec593332a7ad73cc831fa5fb2e))


### Performance Improvements

* **vscode:** bound the label rule's look-behind ([d54617b](https://github.com/oxabl-project/oxabl/commit/d54617b1a46ed040d0e2cdedc52b580874adc9e8))
* **vscode:** match every keyword category with one regex ([0dbf579](https://github.com/oxabl-project/oxabl/commit/0dbf579057c948ceeb66ef97100edc99bedbf8c7))
