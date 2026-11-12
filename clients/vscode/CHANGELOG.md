# Changelog

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
