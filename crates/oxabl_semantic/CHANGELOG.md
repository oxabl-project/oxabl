# Changelog

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v2.0.0...oxabl_semantic-v3.0.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind::Update and StatementKind::Set gain a fields field.
* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **parser:** oxabl_lexer::Kind adds the Stop variant. Exhaustive matches must handle it.
* **parser:** StatementKind::Form and StatementKind::DefineFrame have a new `fields: Vec<FrameField>` field.
* **parser:** StatementKind has a new WaitFor variant; WAIT-FOR no longer parses to Skipped.
* **parser:** StatementKind has a new Apply variant; APPLY no longer parses to Skipped.
* **parser:** StatementKind has new Enable, Disable, Hide and View variants; those statements no longer parse to Skipped.
* **parser:** StatementKind has new Update and Set variants; UPDATE and SET no longer parse to Skipped.
* **parser:** StatementKind has new Form and DefineWidget variants and SymbolKind has a new Widget variant; FORM and static widget definitions no longer parse to Skipped.
* **parser:** StatementKind has new Pause, Down and Up variants; PAUSE, DOWN and UP no longer parse to Skipped.
* **parser:** StatementKind has new Export and Import variants; EXPORT and IMPORT no longer parse to Skipped.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **semantic:** `AnalysisContext` has two new public fields, `tokens` and `elided`. Code that builds it with a struct literal must set them;
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** Statement has a new public field, skipped_tail.
* **parser:** AccessModifier gains a PackageProtected variant.
* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **semantic:** `SymbolKind` has a new `Sequence` variant.
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **lint:** report LINT0006 dead stores at every write form ([248b988](https://github.com/oxabl-project/oxabl/commit/248b988aedfa848e9de487d9e34fc826ca15e8e2)), closes [#131](https://github.com/oxabl-project/oxabl/issues/131)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **parser:** accept USING-imported keyword class names and OE12 VAR lists ([498faa2](https://github.com/oxabl-project/oxabl/commit/498faa2a248db29222384e7735c1aa6ccbe70f57))
* **parser:** credit reads inside the skipped tails of modelled statements ([c93aa74](https://github.com/oxabl-project/oxabl/commit/c93aa742c3f49dc3b1621cb641d003860dfe0c83)), closes [#134](https://github.com/oxabl-project/oxabl/issues/134)
* **parser:** declare frame fields of FORM and DEFINE FRAME ([de9817d](https://github.com/oxabl-project/oxabl/commit/de9817d06e20866f0eff98c311ae92c24bba6d9e))
* **parser:** declare frame fields of UPDATE and SET ([ffb8263](https://github.com/oxabl-project/oxabl/commit/ffb82636fd6767d0b1864f48e09ee400396ea734))
* **parser:** declare the AS/LIKE field of a MESSAGE SET/UPDATE phrase ([72b77a3](https://github.com/oxabl-project/oxabl/commit/72b77a306d99c7b46e3445d515a3cd5ce2d11789))
* **parser:** head-parse APPLY ([ef487a1](https://github.com/oxabl-project/oxabl/commit/ef487a1f35ab24a6f90973a304e242101c8e9957)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse ENABLE, DISABLE, HIDE and VIEW ([d983d80](https://github.com/oxabl-project/oxabl/commit/d983d80ca20ccc87d952289215f20e47bd059980)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse EXPORT and IMPORT ([59f6c2c](https://github.com/oxabl-project/oxabl/commit/59f6c2c074d9d2a0f2f2038e60d171e48532f1e3)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse FORM and declare static widgets ([27d2c24](https://github.com/oxabl-project/oxabl/commit/27d2c2487060f605c4f7b042fb12e5a27f62b7a8)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PAUSE, DOWN and UP ([b16c0eb](https://github.com/oxabl-project/oxabl/commit/b16c0ebab1fb7a91a4db041b9f7ae6e5198c46e2)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse PUT and trim the harvest of OS-* file statements ([ba9faee](https://github.com/oxabl-project/oxabl/commit/ba9faeef75aa39f3b549c9c13b7ae2b2580f2801)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse UPDATE and SET ([c2882f4](https://github.com/oxabl-project/oxabl/commit/c2882f45a574bdb676c110a4be0fe8372856a77c)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** head-parse WAIT-FOR ([7179cea](https://github.com/oxabl-project/oxabl/commit/7179cea2f0c45f2afa8dd852a4671cc020d5cf07)), closes [#136](https://github.com/oxabl-project/oxabl/issues/136)
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **parser:** model the handle-lifetime statements ([86c7938](https://github.com/oxabl-project/oxabl/commit/86c793879ff33e210d4253563aaa8af1dee90b9d))
* **parser:** record USE-WIDGET-POOL on a class ([2b7fbbf](https://github.com/oxabl-project/oxabl/commit/2b7fbbf00dc2cda3a729f7d542c71a7528969490))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))
* **semantic,analyze:** analyze a file without the type-check pass ([96c2aa4](https://github.com/oxabl-project/oxabl/commit/96c2aa48e3852106a09c0b73102b1f6a6d339de8)), closes [#218](https://github.com/oxabl-project/oxabl/issues/218)
* **semantic:** carry the tokens and the elided offsets in AnalysisContext ([a1b1c2c](https://github.com/oxabl-project/oxabl/commit/a1b1c2c83a5321136657740bfb23c965ca394f8b))
* **semantic:** read the parameter lists of procedures ([7d0296c](https://github.com/oxabl-project/oxabl/commit/7d0296cbabdfb5d4e65b75e4d913f8cbe3f3c455))
* **semantic:** recognize documented OpenEdge built-in types ([a528bbc](https://github.com/oxabl-project/oxabl/commit/a528bbc0f7953dcc355a88c91e2a5438099f1d37)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* **semantic:** resolve abbreviated schema table names ([4a6a265](https://github.com/oxabl-project/oxabl/commit/4a6a26530ca683521f958d4270f0fe8eddd9e6eb))
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)


### Bug Fixes

* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **parser:** keep the table a signature buffer parameter names ([0843777](https://github.com/oxabl-project/oxabl/commit/0843777db35aa2595dd20d5faeaef40448425c8a)), closes [#139](https://github.com/oxabl-project/oxabl/issues/139)
* **parser:** parse the PACKAGE-PROTECTED access modifier ([95d7b23](https://github.com/oxabl-project/oxabl/commit/95d7b23d30f45c079e1801b5e178100fd13a5e02))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* **semantic:** bind FOR EACH to the buffer already in scope ([cf17969](https://github.com/oxabl-project/oxabl/commit/cf17969bf7c59e7a3cf995e62a4e88ba72c18640)), closes [#179](https://github.com/oxabl-project/oxabl/issues/179)
* **semantic:** credit an assignment through a pseudo-function to its operand ([c778506](https://github.com/oxabl-project/oxabl/commit/c7785061c695c5fece3dbeaa9cc916b06f0ce3e6)), closes [#156](https://github.com/oxabl-project/oxabl/issues/156)
* **semantic:** credit reads in arguments of a method call inside a member-access chain ([73ecd48](https://github.com/oxabl-project/oxabl/commit/73ecd48e9262f1832f9af2b23b126ce817f02903)), closes [#197](https://github.com/oxabl-project/oxabl/issues/197) [#157](https://github.com/oxabl-project/oxabl/issues/157)
* **semantic:** declare the NEW and OLD buffers of a TRIGGER PROCEDURE ([11c7d3a](https://github.com/oxabl-project/oxabl/commit/11c7d3aabbdb72f2a5769e8f6c37088c81e57507))
* **semantic:** declare TRIGGER PROCEDURE buffers only for a table that resolves ([0fb98cf](https://github.com/oxabl-project/oxabl/commit/0fb98cfa7b68ec041e16f2b174515bb276ffae3e))
* **semantic:** do not flag a declaration repeated in sibling &IF arms as a duplicate ([55a80cd](https://github.com/oxabl-project/oxabl/commit/55a80cd6944b0ad805cb264886aabe5b683b1e8a)), closes [#180](https://github.com/oxabl-project/oxabl/issues/180)
* **semantic:** do not report a `..` RUN target as absent ([5c3074f](https://github.com/oxabl-project/oxabl/commit/5c3074f68c83213c99bc9c01e17d69e96d32687e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* **semantic:** give record-only schema table references an edge ([3f8e090](https://github.com/oxabl-project/oxabl/commit/3f8e090ec4bd58fd724f4e2fb334a693207f401f)), closes [#223](https://github.com/oxabl-project/oxabl/issues/223)
* **semantic:** look up the class-shaped runs of a modelled statement's skipped tail ([0da4553](https://github.com/oxabl-project/oxabl/commit/0da45535c733292776ca1baf659831c4af962cd0))
* **semantic:** read ON ... OF targets and register missing built-ins ([a5899ba](https://github.com/oxabl-project/oxabl/commit/a5899ba049ea4d16c57feaf21f8f709996c8b050))
* **semantic:** resolve stream operands and value-before-parenthesis items ([38bc1f1](https://github.com/oxabl-project/oxabl/commit/38bc1f18006d8ae27ff01e720f60dad033277a70))
* **semantic:** resolve the type operand of CAST, TYPE-OF and GET-CLASS as a type ([0e837c6](https://github.com/oxabl-project/oxabl/commit/0e837c6b614ad9a4efbdeaeac40551d9bc7bfb7f)), closes [#155](https://github.com/oxabl-project/oxabl/issues/155)
* **semantic:** resolve unqualified fields against temp-tables in scope ([337f1ac](https://github.com/oxabl-project/oxabl/commit/337f1acc079d6dadda328a28eac683a3094e714b)), closes [#160](https://github.com/oxabl-project/oxabl/issues/160)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **index:** reduce name interning and unrelated span mapping ([ce8616c](https://github.com/oxabl-project/oxabl/commit/ce8616c93eef5aa92c621ee18a848d79963251d9))
* **semantic:** fold short names inline and skip lexing empty record tails ([f734e57](https://github.com/oxabl-project/oxabl/commit/f734e57cb67a46e668e8758ccbf0905501d54977))
* **semantic:** size the side tables from the program's node count ([7f24230](https://github.com/oxabl-project/oxabl/commit/7f24230a70bddc01484931066effbded3bce7684))
* **semantic:** size the side tables from the program's node count ([4c53517](https://github.com/oxabl-project/oxabl/commit/4c53517be3000ab5a569fbda2e4004dd6487bf71))
* win back parser, resolve and preprocessor time ([07a6324](https://github.com/oxabl-project/oxabl/commit/07a63246bca88025c833fba6c96e3d003465a819))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.1.1...oxabl_semantic-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **parser:** DataType has a new variant, Generic, and ExpressionKind::New has a new field, type_arguments. Code that matches DataType exhaustively, or builds or matches New without `..`, must handle them.
* **parser:** StatementKind::Function has a new field, prototype_parameters. Code that builds the variant or matches it without `..` must name it.
* **parser:** `StatementKind` gains an `Annotation` variant, so exhaustive matches over it need a new arm.
* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.
* **index:** EdgeInputs gains preprocessor_loss, UnresolvedIncludeInput gains unterminated, UnreadableIncludeInput gains include_site, and nested_unresolved_includes takes NestedUnresolvedIncludeInput. DegradedReason gains two variants.
* **index:** ClassKind gains the Enum variant.

### Features

* **index:** degrade the edge set on preprocessor loss and unmodelled statements ([5fd8261](https://github.com/oxabl-project/oxabl/commit/5fd8261475d3fa49af2d356ee3cbc69ce18ceec6))
* **index:** record unlinked RUN targets and the files behind unusable answers ([432a323](https://github.com/oxabl-project/oxabl/commit/432a3236ad2bc10887ff945c0b5d00ac26e3b8df)), closes [#212](https://github.com/oxabl-project/oxabl/issues/212) [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **parser:** keep the parameters of a function prototype ([038c3ff](https://github.com/oxabl-project/oxabl/commit/038c3ff21b19654b47dcb17f5e153d3a42a58e80))
* **parser:** keep the type arguments of a generic class name ([6f5e3cb](https://github.com/oxabl-project/oxabl/commit/6f5e3cb11f3e20f4fa2c89e191c6e18c247b4ab9))
* **parser:** parse ABL annotations as a statement ([0de3bb6](https://github.com/oxabl-project/oxabl/commit/0de3bb6408e74e1bdbc729288713880d2d23bc90))


### Bug Fixes

* **index:** index ENUM declarations and in-file directive-built classes ([feb5ad0](https://github.com/oxabl-project/oxabl/commit/feb5ad055d32985787649b56a8eed0fd1142fd97))
* **semantic:** do not record RUN through a handle as a program reference ([3abc7f0](https://github.com/oxabl-project/oxabl/commit/3abc7f0769981d293042d4babc0c81e7785ee9fa)), closes [#215](https://github.com/oxabl-project/oxabl/issues/215)
* **semantic:** keep missing-class rows under shipped parents and in widget statements ([e00a6d0](https://github.com/oxabl-project/oxabl/commit/e00a6d0ca6cb067fb6daa91509c6ef576ecbce5d))
* **semantic:** leave no row for a bare RUN name that no file supplies ([4aa0ab0](https://github.com/oxabl-project/oxabl/commit/4aa0ab005e7652db0bf15362d51a86cb9fcd52a1))
* **semantic:** let an explicit USING decide whether a missed class is shipped ([7241c73](https://github.com/oxabl-project/oxabl/commit/7241c7301e60c5f927722a7a6daff192206a90fd))
* **semantic:** let an unseen import outrank a shipped wildcard in a parent's header ([2996a8d](https://github.com/oxabl-project/oxabl/commit/2996a8d66c264aacd2bf36c2570cda0d8f5d1433))
* **semantic:** merge the lookups of two spellings of one class ([b949dab](https://github.com/oxabl-project/oxabl/commit/b949dabf0665b8c8d6dfbc1d5084713d0cd0c60f))
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))
* **semantic:** record a class lookup at every site that names a class ([7221537](https://github.com/oxabl-project/oxabl/commit/722153782e3960231a92fc939b9c17e6a35f0d43)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **semantic:** record the parameter types of a function prototype as class sites ([fc78d4c](https://github.com/oxabl-project/oxabl/commit/fc78d4cfe7b1d3b71e0a523c093412b9069b7e3f))
* **semantic:** record the type arguments of a generic class as class sites ([1b854fc](https://github.com/oxabl-project/oxabl/commit/1b854fccc8aaf532ef379f61b6fb3a41975153d6))
* **semantic:** report an unusable class at a naming site as an edge and a row ([658f2b2](https://github.com/oxabl-project/oxabl/commit/658f2b23429d1ab6f5bb07cbf828e4e18a8582e0)), closes [#208](https://github.com/oxabl-project/oxabl/issues/208)
* **semantic:** stop reporting classes that are not workspace classes ([d5a78e3](https://github.com/oxabl-project/oxabl/commit/d5a78e3b6f9c293699f7b8039107673cd1b417c9))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.3.0 to 0.3.1
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.1.0...oxabl_semantic-v1.1.1) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dev-dependencies
    * oxabl_parser bumped from 1.0.3 to 1.0.4

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.3...oxabl_semantic-v1.1.0) (2026-09-21)


### Features

* **schema:** ship the OpenEdge metaschema so `_file` and friends resolve ([#189](https://github.com/oxabl-project/oxabl/issues/189)) ([0367b60](https://github.com/oxabl-project/oxabl/commit/0367b608273df8e20bb6c89495e27f23d8e50da2)), closes [#151](https://github.com/oxabl-project/oxabl/issues/151)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0
  * dev-dependencies
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.2...oxabl_semantic-v1.0.3) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.2 to 0.2.3
  * dev-dependencies
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_schema bumped from 0.2.2 to 0.2.3

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.1...oxabl_semantic-v1.0.2) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_schema bumped from 0.2.1 to 0.2.2
  * dev-dependencies
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.1 to 0.2.2

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v1.0.0...oxabl_semantic-v1.0.1) (2026-08-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.0 to 0.2.1
  * dev-dependencies
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.0 to 0.2.1

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_semantic-v0.1.0...oxabl_semantic-v1.0.0) (2026-08-04)


### ⚠ BREAKING CHANGES

* `SymbolTable::inherited_member_type` and `record_inherited_member_type` are removed; read `Symbol::data_type`.
* **semantic:** UnresolvedReason gains NotFoundInWorkspace and Unknowable. An exhaustive match on it will no longer compile. Both are skip-listed by every lint rule, so no diagnostic changes.

### Features

* **ast:** full-fidelity spans on AST wrapper nodes ([#91](https://github.com/oxabl-project/oxabl/issues/91)) ([2e774b1](https://github.com/oxabl-project/oxabl/commit/2e774b1338bd9c9ff0db014e2cdc238db10801a7))
* capture SHARED/NEW SHARED flags on variable, temp-table, and buffer defines ([04abd6c](https://github.com/oxabl-project/oxabl/commit/04abd6c4860d6b3c0049a08c258b65c044b6e3b5))
* capture SHARED/NEW SHARED flags on variable, temp-table, and buffer defines ([2be6087](https://github.com/oxabl-project/oxabl/commit/2be60876efbe135278129e0f282e99794f3de1b9))
* close residual LINT0001 language-coverage gaps ([#58](https://github.com/oxabl-project/oxabl/issues/58)) ([b3124fd](https://github.com/oxabl-project/oxabl/commit/b3124fdf3fe6c60472e94e5d6fa4f63b3e84d1a1))
* judge the cross-file population, and drain the top of the unmodelled-statement suppression ([#153](https://github.com/oxabl-project/oxabl/issues/153)) ([a1e03e6](https://github.com/oxabl-project/oxabl/commit/a1e03e6e1484ebd1bf3a7e6b194c74df8affbc4b))
* **lint:** add block-var-used-outside advisory (LINT0005) ([#123](https://github.com/oxabl-project/oxabl/issues/123)) ([e870d5b](https://github.com/oxabl-project/oxabl/commit/e870d5bb228cf442af933d9c257ab8b569bba726))
* **lint:** credit table parameters, split dead stores into LINT0006 ([#129](https://github.com/oxabl-project/oxabl/issues/129)) ([d539c31](https://github.com/oxabl-project/oxabl/commit/d539c31d7b90cfac43b54a9370adbe5097130268))
* **lsp:** ship `oxabl lsp` diagnostics-to-editor skeleton ([#90](https://github.com/oxabl-project/oxabl/issues/90)) ([d025e3d](https://github.com/oxabl-project/oxabl/commit/d025e3da8f97337e66d4ead1685ae38f88573adf))
* **oxabl_lint:** add lint crate with 4 v1 rules (Phase 5) ([ed159e6](https://github.com/oxabl-project/oxabl/commit/ed159e61419770b4b8341d9d227d4f7cbe607556))
* **oxabl_semantic:** add crate skeleton and declare pass (Phase 3) ([c3db693](https://github.com/oxabl-project/oxabl/commit/c3db69311129aa3a79143112f1975a7f828da99d))
* **oxabl_semantic:** add per-pass semantic benches + release config (Phase 8) ([56cc85b](https://github.com/oxabl-project/oxabl/commit/56cc85bc6083f41de704dfd5df1d47e506cd96c4))
* **oxabl_semantic:** add type-check pass (Phase 4b) ([79435cd](https://github.com/oxabl-project/oxabl/commit/79435cd48c5365b7bcb293accd110cfde9450ffe))
* **oxabl_semantic:** crate skeleton and declare pass (Phase 3) ([f155645](https://github.com/oxabl-project/oxabl/commit/f155645048552a34860a0074527b1777a3c12550))
* **pipeline:** wire cross-file resolution into every client, and prove they agree ([#148](https://github.com/oxabl-project/oxabl/issues/148)) ([967331b](https://github.com/oxabl-project/oxabl/commit/967331bc7b4a0ae288dc5398a9f1c01d4be9278a))
* resolve abbreviated built-in function calls in undefined-symbol lint ([3f45edd](https://github.com/oxabl-project/oxabl/commit/3f45edd11e2f33f6c924a14d1d43c772ae8bcd47))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))
* seed full ABL system-handle set for LINT0001 ([b461dd7](https://github.com/oxabl-project/oxabl/commit/b461dd7336ff86cdedc874131ef7587933e1d121))
* **semantic:** resolve cross-file names, with the rules still held still ([#147](https://github.com/oxabl-project/oxabl/issues/147)) ([8873de9](https://github.com/oxabl-project/oxabl/commit/8873de9a13f5ef5c30d584f13e63e21cea6300e0))
* **semantic:** the cross-file resolution seam, with the rules held still ([#146](https://github.com/oxabl-project/oxabl/issues/146)) ([3cee2c0](https://github.com/oxabl-project/oxabl/commit/3cee2c0a032c3e9d8176ca2740ed9c065e84824e))


### Bug Fixes

* complete shared-flag fields on schema-PR test fixtures after [#63](https://github.com/oxabl-project/oxabl/issues/63) merge ([d78cd4c](https://github.com/oxabl-project/oxabl/commit/d78cd4c777ac887c03b637ae7f5d7dc011a8badf))
* credit table reads in buffer, empty-table, and query forms ([#130](https://github.com/oxabl-project/oxabl/issues/130)) ([#138](https://github.com/oxabl-project/oxabl/issues/138)) ([6e39138](https://github.com/oxabl-project/oxabl/commit/6e3913885f513af727b1aee14973ff7936518132))
* **lint:** count OUTPUT-argument passing as a use in unused-variable (LINT0002) ([#127](https://github.com/oxabl-project/oxabl/issues/127)) ([a6eabb7](https://github.com/oxabl-project/oxabl/commit/a6eabb7346ba10f31a5d1a83993eebddd3287dae))
* reconcile FUNCTION FORWARD/IN SUPER prototype + definition ([#69](https://github.com/oxabl-project/oxabl/issues/69)) ([#73](https://github.com/oxabl-project/oxabl/issues/73)) ([beda23c](https://github.com/oxabl-project/oxabl/commit/beda23c81b953d8dbb080dfb2af4d7b6b0d1bd04))
* resolve built-in ABL functions in undefined-symbol lint ([c47b55a](https://github.com/oxabl-project/oxabl/commit/c47b55a694ec62297359d96b3bd36881965abc43)), closes [#58](https://github.com/oxabl-project/oxabl/issues/58)
* **semantic:** accumulate read/write counts on schema field symbols ([#60](https://github.com/oxabl-project/oxabl/issues/60)) ([#112](https://github.com/oxabl-project/oxabl/issues/112)) ([c70c768](https://github.com/oxabl-project/oxabl/commit/c70c7681323801dcfc2189e66590a7aa1f8403f3))
* **semantic:** resolve unambiguous abbreviated field references ([#82](https://github.com/oxabl-project/oxabl/issues/82)) ([#84](https://github.com/oxabl-project/oxabl/issues/84)) ([f3a340c](https://github.com/oxabl-project/oxabl/commit/f3a340c44bcb3941bfbf02f86ab4459509a43bf1))
* **semantic:** resolve unqualified FIRST-OF/LAST-OF field against block buffers ([#111](https://github.com/oxabl-project/oxabl/issues/111)) ([6dcf5db](https://github.com/oxabl-project/oxabl/commit/6dcf5db754f9efc65b18c16989d89331e3d3035e)), closes [#107](https://github.com/oxabl-project/oxabl/issues/107)
* **semantic:** scope DEFINE VARIABLE to the routine, not the block ([#122](https://github.com/oxabl-project/oxabl/issues/122)) ([edc98f8](https://github.com/oxabl-project/oxabl/commit/edc98f8de8eb9fd48ef3b5f40b3931707680ec6f))
* **semantic:** scope temp-table fields to their temp-table, not program scope ([#110](https://github.com/oxabl-project/oxabl/issues/110)) ([3d19165](https://github.com/oxabl-project/oxabl/commit/3d19165207ff07e62ae7874b7b43a9faa7b9e900)), closes [#106](https://github.com/oxabl-project/oxabl/issues/106)
* **semantic:** treat DO loop counter as a use of an existing variable ([#83](https://github.com/oxabl-project/oxabl/issues/83)) ([#85](https://github.com/oxabl-project/oxabl/issues/85)) ([9d77fcf](https://github.com/oxabl-project/oxabl/commit/9d77fcf5ae542e8aa39438f186abe046de5c65ea))
* silence LINT0001 for MAX/MIN/ABS/VALUE, NO-APPLY, lock method args ([1e997d2](https://github.com/oxabl-project/oxabl/commit/1e997d201e9d42a0240ca9958523c9c98085e4f4))
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Performance Improvements

* seed ABL system handles once via LazyLock ([#72](https://github.com/oxabl-project/oxabl/issues/72)) ([5fabd77](https://github.com/oxabl-project/oxabl/commit/5fabd77fbc5ba5c093c3e3395ecbb18e62c3c0cd))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
  * dev-dependencies
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_parser bumped from 0.6.0 to 1.0.0
    * oxabl_schema bumped from 0.1.0 to 0.2.0
