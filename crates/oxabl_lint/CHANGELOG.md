# Changelog

## [3.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v3.0.1...oxabl_lint-v3.0.2) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.2.1 to 0.2.2
    * oxabl_flow bumped from 0.3.1 to 0.3.2
    * oxabl_handles bumped from 0.2.1 to 0.2.2
    * oxabl_index bumped from 3.0.1 to 3.1.0
    * oxabl_nulls bumped from 0.2.1 to 0.2.2
    * oxabl_scope bumped from 0.2.1 to 0.2.2
    * oxabl_semantic bumped from 3.0.1 to 3.0.2
  * dev-dependencies
    * oxabl_index bumped from 3.0.1 to 3.1.0
    * oxabl_schema bumped from 0.4.1 to 0.4.2
    * oxabl_parser bumped from 3.0.1 to 3.0.2
    * oxabl_preprocessor bumped from 0.7.1 to 0.7.2
    * oxabl_workspace bumped from 3.1.0 to 3.2.0

## [3.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v3.0.0...oxabl_lint-v3.0.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_effects bumped from 0.2.0 to 0.2.1
    * oxabl_flow bumped from 0.3.0 to 0.3.1
    * oxabl_handles bumped from 0.2.0 to 0.2.1
    * oxabl_index bumped from 3.0.0 to 3.0.1
    * oxabl_nulls bumped from 0.2.0 to 0.2.1
    * oxabl_scope bumped from 0.2.0 to 0.2.1
    * oxabl_semantic bumped from 3.0.0 to 3.0.1
  * dev-dependencies
    * oxabl_index bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
    * oxabl_parser bumped from 3.0.0 to 3.0.1
    * oxabl_preprocessor bumped from 0.7.0 to 0.7.1
    * oxabl_workspace bumped from 3.0.0 to 3.1.0

## [3.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v2.0.0...oxabl_lint-v3.0.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **parser:** StatementKind::Message gains a fields field.
* **parser:** StatementKind::Update and StatementKind::Set gain a fields field.
* **parser:** StatementKind::Update and StatementKind::Set have a new `options` field.
* **analysis:** oxabl_lint::RunShared now has a lifetime parameter so it can borrow a callee summary provider. Types that store RunShared must supply that lifetime.
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
* **lexer:** `share`, `share-`, `share-l`, `share-lo` and `share-loc` now lex as `Kind::ShareLock`, not `Kind::Identifier`.
* **parser:** StatementKind has a new Put variant; PUT no longer parses to Skipped.
* **parser:** Statement has a new public field, skipped_tail.
* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **lexer:** `OnAction::PersistentRun` gains `in_handle`.
* **lint:** `oxabl_analyze::CollectedDiagnostics` has a new public field, `include_findings`. Code that builds it with a struct literal must set it or use `..Default::default()`.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;
* **parser:** `StatementKind::Class` has a new field, `use_widget_pool`. Code that builds the variant or matches it without `..` must name it.
* **parser:** new `StatementKind::ReleaseObject` and
* **parser:** `StatementKind::Do`, `Repeat` and `ForEach` have a new
* **semantic:** `SymbolKind` has a new `Sequence` variant.
* **parser:** StatementKind has a new VariableDeclarationList variant, so exhaustive matches on StatementKind must handle it.
* **lint:** StatementKind::Procedure gains an is_prototype field.

### Features

* **analysis:** add callee summaries and database explanations ([ac6fd5d](https://github.com/oxabl-project/oxabl/commit/ac6fd5d57965e713c8df46086e5c9d5419427682))
* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **lexer:** lex OBJECT, WIDGET and BUFFER-NAME as keywords ([cb99b14](https://github.com/oxabl-project/oxabl/commit/cb99b14ba33e78f13b12b00606b66f3726118e2d))
* **lint:** add CL003 signature-mismatch and CL004 unresolved-call ([d43dd19](https://github.com/oxabl-project/oxabl/commit/d43dd1908b435043accbfda80847578f005d26e9))
* **lint:** add NL001 and NL002 unknown-value warnings ([17e0c6c](https://github.com/oxabl-project/oxabl/commit/17e0c6c0c6f94ba01729a4b3bf50b3380722cd28))
* **lint:** add NL004 nullable-db-field ([0ea9cd2](https://github.com/oxabl-project/oxabl/commit/0ea9cd24ad501d88c919c0ec4694a793b5443127))
* **lint:** add the strict.access rule DB001 find-in-loop ([df71f57](https://github.com/oxabl-project/oxabl/commit/df71f57b41299e34f0fd02a86733120a378de266))
* **lint:** add the strict.handles rules HD001-HD005 ([0dd3530](https://github.com/oxabl-project/oxabl/commit/0dd3530cbf90c1f073a5243dc501eb6f1a60b6b1))
* **lint:** add the strict.locks rules LK001-LK003 ([318f55f](https://github.com/oxabl-project/oxabl/commit/318f55fd9a7367bf78a6475fb75056bc494abc2f))
* **lint:** add the strict.nulls rule NL003 field-after-failed-find ([66c4579](https://github.com/oxabl-project/oxabl/commit/66c457953ffdb95eabc445fc5847debce0fa9cb8))
* **lint:** add the strict.transactions rules TX001-TX005 ([1253cc8](https://github.com/oxabl-project/oxabl/commit/1253cc83c520d4f321abafe9c7704b76641905bd))
* **lint:** report LINT0006 dead stores at every write form ([248b988](https://github.com/oxabl-project/oxabl/commit/248b988aedfa848e9de487d9e34fc826ca15e8e2)), closes [#131](https://github.com/oxabl-project/oxabl/issues/131)
* **lint:** strict ABL M6 slice 1: CL003 signature-mismatch and CL004 unresolved-call ([16f4256](https://github.com/oxabl-project/oxabl/commit/16f425673616cb68c743c902b1d4abf256239e3f))
* **nulls:** NL004 nullable-db-field ([7515ae3](https://github.com/oxabl-project/oxabl/commit/7515ae326c0326fd984cc5b61e8a33e49d38e772))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **nulls:** treat database fields the schema starts at ? as sources of unknown values ([3b39aba](https://github.com/oxabl-project/oxabl/commit/3b39abaecb3b1477f1a2e26444511e27ca7c6ced))
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
* **semantic:** recognize documented OpenEdge built-in types ([a528bbc](https://github.com/oxabl-project/oxabl/commit/a528bbc0f7953dcc355a88c91e2a5438099f1d37)), closes [#154](https://github.com/oxabl-project/oxabl/issues/154)
* **semantic:** resolve abbreviated schema table names ([4a6a265](https://github.com/oxabl-project/oxabl/commit/4a6a26530ca683521f958d4270f0fe8eddd9e6eb))
* **semantic:** resolve sequence operands against the loaded schema ([95bbbc2](https://github.com/oxabl-project/oxabl/commit/95bbbc25011cdb0bf5341825c8ff072fd67839fb)), closes [#185](https://github.com/oxabl-project/oxabl/issues/185)


### Bug Fixes

* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* **handles:** model calls, :HANDLE identity, unmapped frees, COPY-LOB, shadowed frees and a work budget ([05f23ef](https://github.com/oxabl-project/oxabl/commit/05f23ef729196a3175317ac9d3b89815c3042c03))
* **handles:** read only the callee bodies that run, and bound the read ([613fad3](https://github.com/oxabl-project/oxabl/commit/613fad34c004b5b5853a7d42470f6790a0026d1d))
* **handles:** read what a callee does with a handle, resolve methods by receiver ([9227fe2](https://github.com/oxabl-project/oxabl/commit/9227fe2c7ea3907a02116b1e8c4b8fe6004fa3d5))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **lexer:** lex SHARE as the abbreviation of SHARE-LOCK ([232929e](https://github.com/oxabl-project/oxabl/commit/232929e0e7d9a87e44f59500c317e8dcd89f9f64))
* **lint:** give LK001 a CAN-FIND message and help of its own ([be1ebfd](https://github.com/oxabl-project/oxabl/commit/be1ebfdc70fb8d77c8d68ef0d7f807c46811925c))
* **lint:** judge a scalar assigned to an EXTENT variable against the element type ([7358d88](https://github.com/oxabl-project/oxabl/commit/7358d889f6323c13fc40df9b37861c40e6e62938)), closes [#182](https://github.com/oxabl-project/oxabl/issues/182)
* **lint:** leave a recovered parse unjudged by the strict.handles rules ([4396af1](https://github.com/oxabl-project/oxabl/commit/4396af1a570ed4b05bb594d733fdd44d86705c37))
* **lint:** name TX005 in the LK002 help only when TX005 reports the raise ([024030a](https://github.com/oxabl-project/oxabl/commit/024030a0ea4f8406f105bb9c8016af8e4b513612))
* **lint:** read RUN ... ON SESSION as the local session in TX004 ([300b444](https://github.com/oxabl-project/oxabl/commit/300b4445f7886e067b9eda11d27bac06e5727b57))
* **lint:** report handle findings anchored in an include at the use site ([b414a76](https://github.com/oxabl-project/oxabl/commit/b414a76e4179e87c6b78a06d8fa732b883a631af))
* **lint:** report LINT0006 at a write inside CREATE ... TRIGGERS ([b8c3b9b](https://github.com/oxabl-project/oxabl/commit/b8c3b9b9ef84854624005f4ad0206e19f628a962))
* **lint:** say "(or 1 other)" in NL003 and use a field the test schema has ([303da06](https://github.com/oxabl-project/oxabl/commit/303da06f5a318379bf0d5b48b37da5cc54bae4a1))
* **lint:** say that NO-LOCK in a CAN-FIND may see an uncommitted change ([d0f846c](https://github.com/oxabl-project/oxabl/commit/d0f846c7840754a60ef67bc6e4aa48ac95df5d19))
* **lint:** skip CATCH variables and EXTERNAL/IN procedure parameters in unused-variable ([0ac00a2](https://github.com/oxabl-project/oxabl/commit/0ac00a2b5f2ec41d27011a300430569310a68a5f)), closes [#195](https://github.com/oxabl-project/oxabl/issues/195) [#196](https://github.com/oxabl-project/oxabl/issues/196)
* **lint:** skip LINT0004 on an assignment that an empty reference joins ([8ef08c6](https://github.com/oxabl-project/oxabl/commit/8ef08c6a2b52e8a246f5b161fca2cc128c5d101b))
* **nulls:** forget locals that triggers or a REPEAT loop may write ([7d8a4c3](https://github.com/oxabl-project/oxabl/commit/7d8a4c34f7393b4061f8036a280d3a6f50db7bfa))
* **parser:** accept OVERRIDE in DEFINE PROPERTY ([91ca632](https://github.com/oxabl-project/oxabl/commit/91ca6324fcdb2d6ade0f63666fdaed82bbaf67cd))
* **parser:** handle STOP, comments between statement keywords, and bare metaschema fields ([a2b093b](https://github.com/oxabl-project/oxabl/commit/a2b093b74998b49260470fac9a4a25d23d3aef39))
* **parser:** keep the database qualifier of a buffer or LIKE table ([2302914](https://github.com/oxabl-project/oxabl/commit/23029149faaa223c062e3ed411fb4bf084ff5d29))
* **parser:** keep the expression of a block header ON phrase action ([c930ea0](https://github.com/oxabl-project/oxabl/commit/c930ea016dcb12e644b3df2a34ed3baf7154f6f0)), closes [#226](https://github.com/oxabl-project/oxabl/issues/226)
* **parser:** keep the expressions of UPDATE, SET, FORM and ENABLE format options ([d3436fb](https://github.com/oxabl-project/oxabl/commit/d3436fb0428577d7c363068017323fd60582791c))
* **parser:** model DEFINE WORKFILE/WORK-TABLE and the ACCUM aggregate phrase ([ad51cbc](https://github.com/oxabl-project/oxabl/commit/ad51cbc97efa8f68a6b6e0103f37e3a8977ffe7b)), closes [#159](https://github.com/oxabl-project/oxabl/issues/159) [#184](https://github.com/oxabl-project/oxabl/issues/184)
* **parser:** parse the CHOOSE statement as a skipped statement ([91bdc0b](https://github.com/oxabl-project/oxabl/commit/91bdc0b0d7f840597b5a1e56ba3c3488f4622524))
* **parser:** parse the SEEK statement and register USER as the USERID abbreviation ([73fb2f9](https://github.com/oxabl-project/oxabl/commit/73fb2f99979f98bcf25d7362b82f72683fd400e7))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** read database-qualified names in record statements ([6d8cbf1](https://github.com/oxabl-project/oxabl/commit/6d8cbf10c5e64de5963dde999b0727d24b41e697))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **scope:** count the screen form of ASSIGN and read written in execution order ([bf95fb7](https://github.com/oxabl-project/oxabl/commit/bf95fb795baf4e35355ee3b336d489ca6b5ca2bb))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** read a lock that unexpanded text may hide as Unknown ([ca9caae](https://github.com/oxabl-project/oxabl/commit/ca9caae9d8ed232f1a64271222b2fac34c58813e))
* **semantic:** an unqualified name under an unread superclass is unknowable ([07b4825](https://github.com/oxabl-project/oxabl/commit/07b4825df5c4b3e037d2912aade44fc8df5ed582)), closes [#178](https://github.com/oxabl-project/oxabl/issues/178)
* **semantic:** bind FOR EACH to the buffer already in scope ([cf17969](https://github.com/oxabl-project/oxabl/commit/cf17969bf7c59e7a3cf995e62a4e88ba72c18640)), closes [#179](https://github.com/oxabl-project/oxabl/issues/179)
* **semantic:** credit an assignment through a pseudo-function to its operand ([c778506](https://github.com/oxabl-project/oxabl/commit/c7785061c695c5fece3dbeaa9cc916b06f0ce3e6)), closes [#156](https://github.com/oxabl-project/oxabl/issues/156)
* **semantic:** credit reads in arguments of a method call inside a member-access chain ([73ecd48](https://github.com/oxabl-project/oxabl/commit/73ecd48e9262f1832f9af2b23b126ce817f02903)), closes [#197](https://github.com/oxabl-project/oxabl/issues/197) [#157](https://github.com/oxabl-project/oxabl/issues/157)
* **semantic:** declare the NEW and OLD buffers of a TRIGGER PROCEDURE ([11c7d3a](https://github.com/oxabl-project/oxabl/commit/11c7d3aabbdb72f2a5769e8f6c37088c81e57507))
* **semantic:** declare TRIGGER PROCEDURE buffers only for a table that resolves ([0fb98cf](https://github.com/oxabl-project/oxabl/commit/0fb98cfa7b68ec041e16f2b174515bb276ffae3e))
* **semantic:** do not report a `..` RUN target as absent ([5c3074f](https://github.com/oxabl-project/oxabl/commit/5c3074f68c83213c99bc9c01e17d69e96d32687e)), closes [#227](https://github.com/oxabl-project/oxabl/issues/227)
* **semantic:** read ON ... OF targets and register missing built-ins ([a5899ba](https://github.com/oxabl-project/oxabl/commit/a5899ba049ea4d16c57feaf21f8f709996c8b050))
* **semantic:** resolve stream operands and value-before-parenthesis items ([38bc1f1](https://github.com/oxabl-project/oxabl/commit/38bc1f18006d8ae27ff01e720f60dad033277a70))
* **semantic:** resolve the type operand of CAST, TYPE-OF and GET-CLASS as a type ([0e837c6](https://github.com/oxabl-project/oxabl/commit/0e837c6b614ad9a4efbdeaeac40551d9bc7bfb7f)), closes [#155](https://github.com/oxabl-project/oxabl/issues/155)
* **semantic:** resolve unqualified fields against temp-tables in scope ([337f1ac](https://github.com/oxabl-project/oxabl/commit/337f1acc079d6dadda328a28eac683a3094e714b)), closes [#160](https://github.com/oxabl-project/oxabl/issues/160)
* sweep of 25 open parser, semantic, lint and schema issues ([f01fcce](https://github.com/oxabl-project/oxabl/commit/f01fcce408f13c4c20050f712a683435b57a4a46))
* UPDATE/SET frame fields, shared summaries in check --watch, rustdoc links ([bd0df69](https://github.com/oxabl-project/oxabl/commit/bd0df6971eb692a65dcbc18ac89c160319952a01))


### Performance Improvements

* **flow:** read block headers from the parser's tokens ([a1a0f78](https://github.com/oxabl-project/oxabl/commit/a1a0f78a8215fd984637c916ea2b939c2f87822d))
* **lint:** assert bench fixtures parse and trigger their target rules ([2edbb48](https://github.com/oxabl-project/oxabl/commit/2edbb48766ff118b6ae779cabfca170acc33807b)), closes [#132](https://github.com/oxabl-project/oxabl/issues/132)
* **lint:** benchmark lint_file and the registry on realistic fixtures ([ae504b1](https://github.com/oxabl-project/oxabl/commit/ae504b1ef57a955992cd513ea2f70638d85f6b37)), closes [#132](https://github.com/oxabl-project/oxabl/issues/132)
* **lint:** build the CFGs once for the scope and handle analyses ([b72e92a](https://github.com/oxabl-project/oxabl/commit/b72e92a3a3bab5b0f7fea6a9a1fcf2eeba79a1c7))
* **lint:** decide on sharing the CFGs at the first handles rule ([eb834d5](https://github.com/oxabl-project/oxabl/commit/eb834d5cae673e2dfc5ae0b162df818bae810098))
* **lint:** find the skipped statements once per file in LK002 ([ada4a20](https://github.com/oxabl-project/oxabl/commit/ada4a205d218e08572e610f3f2adef5202a903a3))
* **lint:** give the realistic bench the parsed tokens, as check does ([ff1b2ff](https://github.com/oxabl-project/oxabl/commit/ff1b2ff3641194928406d7ea7526db2032ae2eb0))
* **lint:** lex a TX003 or TX004 statement only inside a transaction ([bde529f](https://github.com/oxabl-project/oxabl/commit/bde529f86d8f95518e72e7636355fd1489340b29))
* **lint:** pass the parsed tokens to the scope analysis ([901b4a0](https://github.com/oxabl-project/oxabl/commit/901b4a04bcecd4563db55ce99fcaee9ad2d57c02))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_common bumped from 1.2.0 to 1.2.1
    * oxabl_effects bumped from 0.1.0 to 0.2.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_handles bumped from 0.1.0 to 0.2.0
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_nulls bumped from 0.1.0 to 0.2.0
    * oxabl_scope bumped from 0.1.0 to 0.2.0
    * oxabl_semantic bumped from 2.0.0 to 3.0.0
  * dev-dependencies
    * oxabl_index bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_preprocessor bumped from 0.6.0 to 0.7.0
    * oxabl_workspace bumped from 2.0.0 to 3.0.0

## [2.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.1.1...oxabl_lint-v2.0.0) (2026-10-14)


### ⚠ BREAKING CHANGES

* **parser:** DataType has a new variant, Generic, and ExpressionKind::New has a new field, type_arguments. Code that matches DataType exhaustively, or builds or matches New without `..`, must handle them.
* **parser:** StatementKind::Function has a new field, prototype_parameters. Code that builds the variant or matches it without `..` must name it.
* **lint:** a diagnostic is now reported at its rule's default severity when nothing configures the rule. With an empty LintSeverityMap, unknown-table-or-field and type-mismatch-assignment now report as warnings, as [workspace.lint] already documented.
* **parser:** `StatementKind` gains an `Annotation` variant, so exhaustive matches over it need a new arm.
* **semantic:** oxabl_semantic::ClassDescriptor (re-exported by oxabl) has a new public field, imports, so struct literals need it. A new public type, ImportContext, is exported from both crates.

### Features

* **cli:** check context-free strict rules in the include files of a walked tree ([7b931f0](https://github.com/oxabl-project/oxabl/commit/7b931f0770f0149ff22c6f0c35658085682797c5))
* **lint:** add a public rule registry with rule groups ([352f942](https://github.com/oxabl-project/oxabl/commit/352f942df9a5db018e04f3a158a95c6c1fc6f99b))
* **lint:** add the syntactic strict rules CL001, CL002, CL005 and TX006 ([1467921](https://github.com/oxabl-project/oxabl/commit/1467921f019e6e4dd5fef38ca163d662aaaeb6b2))
* **lint:** register CL001, CL002, CL005 and TX006 in their strict groups ([714834e](https://github.com/oxabl-project/oxabl/commit/714834e13ebb313b70c94ca7dc1438215304e3c4))
* **parser:** keep the parameters of a function prototype ([038c3ff](https://github.com/oxabl-project/oxabl/commit/038c3ff21b19654b47dcb17f5e153d3a42a58e80))
* **parser:** keep the type arguments of a generic class name ([6f5e3cb](https://github.com/oxabl-project/oxabl/commit/6f5e3cb11f3e20f4fa2c89e191c6e18c247b4ab9))
* **parser:** parse ABL annotations as a statement ([0de3bb6](https://github.com/oxabl-project/oxabl/commit/0de3bb6408e74e1bdbc729288713880d2d23bc90))


### Bug Fixes

* **lint:** ask for NO-UNDO on both sides of a shared definition ([9281841](https://github.com/oxabl-project/oxabl/commit/9281841487ca3f0edb0f67b81c1dc345a6f70228))
* **lint:** lower only the findings an includer can change ([4e29dbc](https://github.com/oxabl-project/oxabl/commit/4e29dbc0cf283ae44d61891eb1b44173b0ca53de))
* **lint:** lower the confidence of includer-dependent fragment findings ([4fcec91](https://github.com/oxabl-project/oxabl/commit/4fcec91f4528a01993297ec2456a7555bd969046))
* **lint:** make block-var-used-outside path-aware ([4bc6785](https://github.com/oxabl-project/oxabl/commit/4bc67857b18afb558a3e19d5fe9b09c0f774076a)), closes [#124](https://github.com/oxabl-project/oxabl/issues/124) [#126](https://github.com/oxabl-project/oxabl/issues/126)
* **lint:** read include fragments without comments, undefines and local defines ([aaf66ed](https://github.com/oxabl-project/oxabl/commit/aaf66ede95ce599dbdc4d66ca20abb3f57d6056a))
* **lint:** reset block-var-used-outside at UNDO statements ([6ec7165](https://github.com/oxabl-project/oxabl/commit/6ec7165d81ce073bc6b340bc0f578b3c2e174fad))
* **semantic:** read a parent's supertypes through the declaring file's imports ([fcfc554](https://github.com/oxabl-project/oxabl/commit/fcfc55497b368819c74d5d2d8454010f083d9892))


### Performance Improvements

* **lint:** add a benchmark for block-hoisted variables ([d9ff7b6](https://github.com/oxabl-project/oxabl/commit/d9ff7b696d785cc94b5411033d2b440b7012196d))
* **lint:** build one unit and solve once per block for LINT0005 ([4d61b17](https://github.com/oxabl-project/oxabl/commit/4d61b17c80287480aea40a417bc7e4bd93c60c84))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 1.0.0 to 2.0.0
    * oxabl_common bumped from 1.1.0 to 1.2.0
    * oxabl_flow bumped from 0.1.0 to 0.2.0
    * oxabl_lexer bumped from 1.0.1 to 1.0.2
    * oxabl_semantic bumped from 1.1.1 to 2.0.0
  * dev-dependencies
    * oxabl_index bumped from 1.0.3 to 2.0.0
    * oxabl_schema bumped from 0.3.0 to 0.3.1
    * oxabl_parser bumped from 1.0.4 to 2.0.0
    * oxabl_preprocessor bumped from 0.5.2 to 0.6.0
    * oxabl_workspace bumped from 1.0.3 to 2.0.0

## [1.1.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.1.0...oxabl_lint-v1.1.1) (2026-10-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.1.0 to 1.1.1
  * dev-dependencies
    * oxabl_index bumped from 1.0.2 to 1.0.3
    * oxabl_parser bumped from 1.0.3 to 1.0.4

## [1.1.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.3...oxabl_lint-v1.1.0) (2026-09-21)


### Features

* **schema:** ship the OpenEdge metaschema so `_file` and friends resolve ([#189](https://github.com/oxabl-project/oxabl/issues/189)) ([0367b60](https://github.com/oxabl-project/oxabl/commit/0367b608273df8e20bb6c89495e27f23d8e50da2)), closes [#151](https://github.com/oxabl-project/oxabl/issues/151)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.3 to 1.1.0
  * dev-dependencies
    * oxabl_index bumped from 1.0.1 to 1.0.2
    * oxabl_schema bumped from 0.2.3 to 0.3.0

## [1.0.3](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.2...oxabl_lint-v1.0.3) (2026-09-21)


### Bug Fixes

* analyze include roots as fragments ([#166](https://github.com/oxabl-project/oxabl/issues/166)) ([dba8a3c](https://github.com/oxabl-project/oxabl/commit/dba8a3cb0cdea9f753ae6e1161bd36aeb3014409))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.2 to 1.0.3
  * dev-dependencies
    * oxabl_index bumped from 1.0.0 to 1.0.1
    * oxabl_schema bumped from 0.2.2 to 0.2.3
    * oxabl_parser bumped from 1.0.2 to 1.0.3
    * oxabl_preprocessor bumped from 0.5.1 to 0.5.2
    * oxabl_workspace bumped from 1.0.2 to 1.0.3

## [1.0.2](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.1...oxabl_lint-v1.0.2) (2026-08-14)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_semantic bumped from 1.0.1 to 1.0.2
  * dev-dependencies
    * oxabl_index bumped from 0.1.2 to 1.0.0
    * oxabl_schema bumped from 0.2.1 to 0.2.2
    * oxabl_parser bumped from 1.0.1 to 1.0.2
    * oxabl_preprocessor bumped from 0.5.0 to 0.5.1
    * oxabl_workspace bumped from 1.0.1 to 1.0.2

## [1.0.1](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v1.0.0...oxabl_lint-v1.0.1) (2026-08-12)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_common bumped from 1.0.0 to 1.1.0
    * oxabl_semantic bumped from 1.0.0 to 1.0.1
  * dev-dependencies
    * oxabl_index bumped from 0.1.1 to 0.1.2
    * oxabl_schema bumped from 0.2.0 to 0.2.1
    * oxabl_lexer bumped from 1.0.0 to 1.0.1
    * oxabl_parser bumped from 1.0.0 to 1.0.1
    * oxabl_preprocessor bumped from 0.4.0 to 0.5.0
    * oxabl_workspace bumped from 1.0.0 to 1.0.1

## [1.0.0](https://github.com/oxabl-project/oxabl/compare/oxabl_lint-v0.1.0...oxabl_lint-v1.0.0) (2026-08-04)


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
* resolve abbreviated built-in function calls in undefined-symbol lint ([3f45edd](https://github.com/oxabl-project/oxabl/commit/3f45edd11e2f33f6c924a14d1d43c772ae8bcd47))
* resolve abbreviated built-in function calls in undefined-symbol lint ([7f763a3](https://github.com/oxabl-project/oxabl/commit/7f763a39c99d66e8f1e45d5e598ab9d18f197f66))
* schema-backed symbol resolution for single-file field validation ([91d6097](https://github.com/oxabl-project/oxabl/commit/91d6097ed888c10d9f4c268d93bc5225bd72c055))
* seed full ABL system-handle set for LINT0001 ([b461dd7](https://github.com/oxabl-project/oxabl/commit/b461dd7336ff86cdedc874131ef7587933e1d121))
* **semantic:** resolve cross-file names, with the rules still held still ([#147](https://github.com/oxabl-project/oxabl/issues/147)) ([8873de9](https://github.com/oxabl-project/oxabl/commit/8873de9a13f5ef5c30d584f13e63e21cea6300e0))
* **semantic:** the cross-file resolution seam, with the rules held still ([#146](https://github.com/oxabl-project/oxabl/issues/146)) ([3cee2c0](https://github.com/oxabl-project/oxabl/commit/3cee2c0a032c3e9d8176ca2740ed9c065e84824e))


### Bug Fixes

* bind FUNCTION signature params into function scope ([#68](https://github.com/oxabl-project/oxabl/issues/68)) ([#70](https://github.com/oxabl-project/oxabl/issues/70)) ([8699d86](https://github.com/oxabl-project/oxabl/commit/8699d86109b9e0035693a1db51c8423e3bf74ea9))
* complete shared-flag fields on schema-PR test fixtures after [#63](https://github.com/oxabl-project/oxabl/issues/63) merge ([d78cd4c](https://github.com/oxabl-project/oxabl/commit/d78cd4c777ac887c03b637ae7f5d7dc011a8badf))
* credit table reads in buffer, empty-table, and query forms ([#130](https://github.com/oxabl-project/oxabl/issues/130)) ([#138](https://github.com/oxabl-project/oxabl/issues/138)) ([6e39138](https://github.com/oxabl-project/oxabl/commit/6e3913885f513af727b1aee14973ff7936518132))
* eliminate unused-variable false positives from block-body parse errors ([#79](https://github.com/oxabl-project/oxabl/issues/79)) ([#80](https://github.com/oxabl-project/oxabl/issues/80)) ([e653c36](https://github.com/oxabl-project/oxabl/commit/e653c365373827ab7ec782776e987331732d8694))
* **lint:** count OUTPUT-argument passing as a use in unused-variable (LINT0002) ([#127](https://github.com/oxabl-project/oxabl/issues/127)) ([a6eabb7](https://github.com/oxabl-project/oxabl/commit/a6eabb7346ba10f31a5d1a83993eebddd3287dae))
* resolve built-in ABL functions in undefined-symbol lint ([c47b55a](https://github.com/oxabl-project/oxabl/commit/c47b55a694ec62297359d96b3bd36881965abc43)), closes [#58](https://github.com/oxabl-project/oxabl/issues/58)
* **semantic:** treat DO loop counter as a use of an existing variable ([#83](https://github.com/oxabl-project/oxabl/issues/83)) ([#85](https://github.com/oxabl-project/oxabl/issues/85)) ([9d77fcf](https://github.com/oxabl-project/oxabl/commit/9d77fcf5ae542e8aa39438f186abe046de5c65ea))
* silence LINT0001 for MAX/MIN/ABS/VALUE, NO-APPLY, lock method args ([1e997d2](https://github.com/oxabl-project/oxabl/commit/1e997d201e9d42a0240ca9958523c9c98085e4f4))
* stop three lint rules reporting variables that unmodelled statements touch ([#137](https://github.com/oxabl-project/oxabl/issues/137)) ([28c0a1e](https://github.com/oxabl-project/oxabl/commit/28c0a1e92950596d815acbb654c871036c4ed6b1))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.5.0 to 1.0.0
    * oxabl_common bumped from 0.5.0 to 1.0.0
    * oxabl_semantic bumped from 0.1.0 to 1.0.0
  * dev-dependencies
    * oxabl_index bumped from 0.1.0 to 0.1.1
    * oxabl_schema bumped from 0.1.0 to 0.2.0
    * oxabl_lexer bumped from 0.4.1 to 1.0.0
    * oxabl_parser bumped from 0.6.0 to 1.0.0
    * oxabl_preprocessor bumped from 0.3.1 to 0.4.0
    * oxabl_workspace bumped from 0.4.0 to 1.0.0
