# Changelog

## [0.5.0](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v0.4.0...oxabl_parser-v0.5.0) (2026-04-13)


### Features

* **ast:** add dataset/data-source AST types and lexer keywords ([96d0416](https://github.com/oxabl-project/oxabl/commit/96d041669d0b2c4b20d14fe40503b11079e4b345))
* **bench:** expand benchmark suite to cover parser, lexer scenarios, and source map ([18c6681](https://github.com/oxabl-project/oxabl/commit/18c6681ab28e2097fdff15c647841d95ff49fb51))
* **bench:** expand benchmark suite to parser, lexer scenarios, and source map ([36abc32](https://github.com/oxabl-project/oxabl/commit/36abc32aebd88514f1fcd203b9da0c9a4eb3d88a))
* **lexer:** add OO-ABL keyword Kind variants ([2380ac8](https://github.com/oxabl-project/oxabl/commit/2380ac8b856175931097d9d2703f0b60ba9b7f11))
* **lexer:** add OO-ABL keyword Kind variants ([c3977f4](https://github.com/oxabl-project/oxabl/commit/c3977f46987e5255766b2c384dc1499cac592364))
* **parser:** add database manipulation statements ([8428b93](https://github.com/oxabl-project/oxabl/commit/8428b93b8848864fc52ec6f95310184130f7c3a8))
* **parser:** add database manipulation statements ([404e85c](https://github.com/oxabl-project/oxabl/commit/404e85c91a57f5f7244cb1680f05d4f5ed0bb1e7))
* **parser:** add dataset and data-source parsing support ([ae26e42](https://github.com/oxabl-project/oxabl/commit/ae26e428db7022c864bde37b877e1f7021ca1b0c))
* **parser:** add dataset benchmark fixture and apply formatting ([e2510d5](https://github.com/oxabl-project/oxabl/commit/e2510d582b67d7906fde12ba94a91ba341c424c4))
* **parser:** add dataset, data-source, and XML/serialize parsing ([356d658](https://github.com/oxabl-project/oxabl/commit/356d6587d6961d1fbc7f2fdbc49cfdc6564b3268))
* **parser:** Add include file reference parsing ([28cba6b](https://github.com/oxabl-project/oxabl/commit/28cba6b250247e2936c3a19e711465145189e7fe))
* **parser:** add ON trigger and TRIGGER PROCEDURE statement parsing ([0ec08d3](https://github.com/oxabl-project/oxabl/commit/0ec08d30a6f927ff11e54a0cfc2c53c68e8d0af6))
* **parser:** add ON trigger and TRIGGER PROCEDURE statement parsing ([6dfffdc](https://github.com/oxabl-project/oxabl/commit/6dfffdcac671142290109f13294499ba14e8fac7))
* **parser:** add OO-ABL support ([5c3b2fb](https://github.com/oxabl-project/oxabl/commit/5c3b2fb9269db4dfeaca7d9d041dd8f9424f8bdf))
* **parser:** add OO-ABL support (CLASS, METHOD, PROPERTY, INTERFACE) ([f2ec7a5](https://github.com/oxabl-project/oxabl/commit/f2ec7a5587330fd7c7a0909c752c0478e78fae02))
* **parser:** add preprocessor statement parsing ([a2f82fd](https://github.com/oxabl-project/oxabl/commit/a2f82fd7032dda5fc7c27c3dccead1cf652eec17))
* **parser:** add preprocessor statement parsing ([eea5606](https://github.com/oxabl-project/oxabl/commit/eea560646d49c24614f9bce123432523781defe1))
* **parser:** add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT parsing ([4eb3af4](https://github.com/oxabl-project/oxabl/commit/4eb3af451f99bf02512e5799b9c25bfd1d0336e7))
* **parser:** add PUBLISH/SUBSCRIBE/UNSUBSCRIBE and DEFINE EVENT parsing ([233af8c](https://github.com/oxabl-project/oxabl/commit/233af8ca8df241e00606241a38b8aaed5e86871c))
* **parser:** add stream and frame parsing ([e959ffd](https://github.com/oxabl-project/oxabl/commit/e959ffd74e5a2459800eb076a4e19547d3b3ae70))
* **parser:** add stream and frame parsing ([8182d4e](https://github.com/oxabl-project/oxabl/commit/8182d4e1b952d7de365348469b9d69ba2a9e599c))
* **parser:** handle real-world ABL patterns from oe100fi.p ([15b4405](https://github.com/oxabl-project/oxabl/commit/15b4405e906e6d4dd1c9853590885a212db12a47))
* **parser:** Support LIKE syntax in DEFINE VARIABLE, DEFINE PARAMETER, and VAR ([edf3b84](https://github.com/oxabl-project/oxabl/commit/edf3b8463dc6b32afb564352546b4312877fdcf8))
* **parser:** Support LIKE syntax in DEFINE VARIABLE, DEFINE PARAMETER, and VAR ([dbb1fdf](https://github.com/oxabl-project/oxabl/commit/dbb1fdf325c9dc77ecd89437d74ac2cf03986021))


### Bug Fixes

* **ast, lexer:** resolved some code breakage from the last merge ([f7b44d3](https://github.com/oxabl-project/oxabl/commit/f7b44d37474515ff00c0865d3a6a462af7966ff8))
* **lexer,parser:** &ANALYZE-SUSPEND, quoted procs, CAN-FIND OF, ACCUM label, string :n ([259ea59](https://github.com/oxabl-project/oxabl/commit/259ea594b8d21d5f4fb1cf39aef2ba098584ef73))
* **lexer,parser:** allow '$' in identifiers; add Compile to can_be_identifier; EXTENT before LIKE in temp-table field ([85bf3e4](https://github.com/oxabl-project/oxabl/commit/85bf3e4d083aa51af4706bbcda5377ed4578f22f))
* **lexer,parser:** backslash line-continuation, Add/Write/Save as method names ([92ff17a](https://github.com/oxabl-project/oxabl/commit/92ff17aaeac22cffe4a0ea65372c25e8f2677720))
* **lexer,parser:** backslash string escapes, ASSIGN FRAME clause ([4a9874b](https://github.com/oxabl-project/oxabl/commit/4a9874b6816dd9a5c7c8870260adf4114b3b8454))
* **lexer,parser:** nested include braces, GET/CREATE WIDGET-POOL, VIEW-AS TEXT ([794b1ca](https://github.com/oxabl-project/oxabl/commit/794b1cac9cebb25e12c85006c5ac0fc91d5dde6f))
* **lexer,parser:** tilde line-continuation in skip_whitespace, Progress.* class type ([f6914bf](https://github.com/oxabl-project/oxabl/commit/f6914bf725b0c2a3cfccdadef7a552abcb81de88))
* **lexer,parser:** trailing-minus numbers, MESSAGE AUTO-RETURN modifier ([0829044](https://github.com/oxabl-project/oxabl/commit/0829044eb0967d24ccb948ccc7715c764b54dc5a))
* **parser,lexer:** handle :U1 format suffix, ACCUM statement, and REPOSITION-TO-ROW method calls ([067ac61](https://github.com/oxabl-project/oxabl/commit/067ac61d349a4abc3b3055496cd065a92c7036d5))
* **parser,lexer:** handle dotnetrm/ patterns — 100% success rate ([36c94a9](https://github.com/oxabl-project/oxabl/commit/36c94a9bfa104666c0b760803da98d56f2af89ab))
* **parser:** accept preprocessor references as identifiers ([61544d7](https://github.com/oxabl-project/oxabl/commit/61544d7106c7bd3db0c1f520779fb2e52143b1af))
* **parser:** ACCUM expression with qualified fields and BY clause ([9941171](https://github.com/oxabl-project/oxabl/commit/9941171f994da08fc8f733c2f94f60fa3a03a283))
* **parser:** add GET-KEY-VALUE skip and LOAD/UNLOAD/USE registry statement handlers ([1b0fcec](https://github.com/oxabl-project/oxabl/commit/1b0fcec7b0bf5e4e2efbc158c8c9c0fbdaf3c4b7))
* **parser:** add Kind::Returns to can_be_identifier ([1334743](https://github.com/oxabl-project/oxabl/commit/13347434186a0f4f28d1cb39bf266c7c4230f63c))
* **parser:** add KwEnum and Database to can_be_identifier ([5963464](https://github.com/oxabl-project/oxabl/commit/5963464de5d77a3379b9c6a73a3c8ae9495351d1))
* **parser:** add new keywords to can_be_identifier ([71a8406](https://github.com/oxabl-project/oxabl/commit/71a84063ac5bc2ce7db38c502e7b7b759ca87ba6))
* **parser:** add THROW statement and ON phrase support ([668fd45](https://github.com/oxabl-project/oxabl/commit/668fd4543479358e0be21318b95f6c955894f13d))
* **parser:** add Xref/XrefXml to can_be_identifier for COMPILE member access ([6b3d050](https://github.com/oxabl-project/oxabl/commit/6b3d0504e4cf4d278952ee1e38c42ce8e2791482))
* **parser:** allow DATA-SOURCE as argument qualifier in method/function calls ([4bcd811](https://github.com/oxabl-project/oxabl/commit/4bcd8117d59061055ed8d7e941d5bbdbcae7b8d0))
* **parser:** allow ROWID/RECID as callable identifiers in expression position ([4bcf324](https://github.com/oxabl-project/oxabl/commit/4bcf324f466d2a70466a4d8b26401e2d5b20af47))
* **parser:** AMBIGUOUS predicate, DISABLE/ACCUMULATE/DOWN/OPEN statements, &THEN in IF ([d198ab0](https://github.com/oxabl-project/oxabl/commit/d198ab09f8ece12a0f9f55dddc9b1cdab11d0ac1))
* **parser:** APPLY/UPDATE/STATUS statements, UNDO THROW, DEFINE BUFFER preproc name ([6688f96](https://github.com/oxabl-project/oxabl/commit/6688f965d444705e39f3001f94de08d753f2eeef))
* **parser:** BigIntLiteral, Stream expression, DISPLAY WHEN/AT/VIEW-AS options ([185e475](https://github.com/oxabl-project/oxabl/commit/185e4754f2fd21222acf907de65fbe347c3d8935))
* **parser:** BROWSE name:attr expressions, IN FRAME/BROWSE in RUN args ([6af55d6](https://github.com/oxabl-project/oxabl/commit/6af55d6b58c2dbec105db3d14d54eb0dcc4851a6))
* **parser:** BUFFER-COPY EXCEPT order, LOCKED predicate, FOR EACH USE-INDEX lock re-check, ASSIGN WHEN, FRAME compound expr ([1dc9e7c](https://github.com/oxabl-project/oxabl/commit/1dc9e7c943662776837c4ca9fe70779a3327114e))
* **parser:** BY-REFERENCE modifier, inline IF, BELL, QUERY expr, END CASE, RELATION-FIELD ([5ad0fb7](https://github.com/oxabl-project/oxabl/commit/5ad0fb7cbde7c9e6a0479319d474fedaaffc185c))
* **parser:** CAN-FIND NO-WAIT, BUFFER-COMPARE USING/BINARY clauses ([944af81](https://github.com/oxabl-project/oxabl/commit/944af812a011868fc4771a5015c29cf3c28a466e))
* **parser:** CASE period separator, compound preprop identifiers, qualified relation-fields ([cb09025](https://github.com/oxabl-project/oxabl/commit/cb090252fc6464469c322b7c0142ebe9e57d7b4d))
* **parser:** COLOR/READKEY/UP statements, lock-type Preprop, interface signatures ([2bb8f40](https://github.com/oxabl-project/oxabl/commit/2bb8f40e8fd2975a2f5af97a5617abaa3e7174cd))
* **parser:** compound preprop names in identifier and expression positions ([03a139f](https://github.com/oxabl-project/oxabl/commit/03a139fd1802cb96e007652c52f1e532d07b7cf2))
* **parser:** consume optional NO-ERROR after ASSIGN statement ([759dd0c](https://github.com/oxabl-project/oxabl/commit/759dd0cf9dec11673ac1ea21146ef4439b637100))
* **parser:** db-qualified names, interface method signatures, AVAILABLE/FOR EACH dotted tables ([c86fe49](https://github.com/oxabl-project/oxabl/commit/c86fe49a6a39c7dfcacbc17eea89772f2e47220b))
* **parser:** DELETE/CREATE type prefixes, IMPORT/OS-DELETE stmts, .01 decimal literals ([0503d90](https://github.com/oxabl-project/oxabl/commit/0503d900476dc988528ca0d38eda3a8221bae06a))
* **parser:** dispatch CATCH/FINALLY before block-label heuristic ([1684cb6](https://github.com/oxabl-project/oxabl/commit/1684cb6101a0f62b5a7fe9e8d2b96f4aa312edc6))
* **parser:** DISPLAY @, FOR EACH WHILE, BUFFER-COPY qualified EXCEPT fields ([5d5d165](https://github.com/oxabl-project/oxabl/commit/5d5d16539915d0c7b377d05464d51c983dd0a1ba))
* **parser:** DISPLAY WHEN+WITH, implicit concat scope, EQ word operator in statements ([731ca66](https://github.com/oxabl-project/oxabl/commit/731ca66766cde7b672fd57f4ca2f5ed96e855613))
* **parser:** DO counting-loop TRANSACTION, CLOSE QUERY, FORMAT statement ([29e8402](https://github.com/oxabl-project/oxabl/commit/29e8402132dc4d75b99b00f59761fac9460031bb))
* **parser:** DO FOR/STOP-AFTER, GET() accessor, OS-COMMAND statement ([021ca6c](https://github.com/oxabl-project/oxabl/commit/021ca6cedfc7f29f8c6e739d7208145250325c3c))
* **parser:** DO preprop loop var, CASE WHEN OR logic, Kind::GetByte callable ([e8ec572](https://github.com/oxabl-project/oxabl/commit/e8ec572e1a2a9909149fb439149e479bd271ae8f))
* **parser:** DO WITH FRAME, OPEN/skip_to_statement_end, EQ in statement context ([ac0bffd](https://github.com/oxabl-project/oxabl/commit/ac0bffda1210494e55187d2a9ed730adbcd9b7d6))
* **parser:** DYNAMIC-FUNCTION expression and BUFFER-COMPARE EXCEPT clause ([5cccb56](https://github.com/oxabl-project/oxabl/commit/5cccb567efaf2a5dcdbccc1cd291e33cd1d3c3fa))
* **parser:** exclude Kind::Comment from is_word_kind to prevent class name over-consumption ([af095b1](https://github.com/oxabl-project/oxabl/commit/af095b1e77518e78bd1920e07e59f2af882e96eb))
* **parser:** extend parse_identifier to handle identifier{&suffix} compounds ([37408a2](https://github.com/oxabl-project/oxabl/commit/37408a269324144710cead1dc779f765b6ea728c))
* **parser:** EXTERNAL procedure, RETURN param, BREAK BY, identifier keywords ([fa2c220](https://github.com/oxabl-project/oxabl/commit/fa2c220d44a17c0cf293d1dcabf0923431d1298e))
* **parser:** field access on keyword-named fields (e.g. table.METHOD) ([ecf92fc](https://github.com/oxabl-project/oxabl/commit/ecf92fccb2b0042c8e4a634c0423461985bc21a3))
* **parser:** FINALLY/CATCH at program level, REPOSITION statement, DATASET compound expr, COPY-LOB/FORM field access, preprop macro statements ([4e28668](https://github.com/oxabl-project/oxabl/commit/4e28668afe66b628dcafbcd584b544d2bf3c44f4))
* **parser:** FIND USE-INDEX before lock, CREATE BUFFER string table, INITIAL array, OS-COPY ([f7b96af](https://github.com/oxabl-project/oxabl/commit/f7b96afff2708f9d7507550dd593b1dabb052f45))
* **parser:** FIND/DELETE preproc compound names, unary plus ([ce53167](https://github.com/oxabl-project/oxabl/commit/ce53167af047336f82887251db20b14cbd58e24d))
* **parser:** FOR EACH Preprop table, empty WHERE, EXTENT param, TABLE FOR, CONSTRUCTOR period ([9533db8](https://github.com/oxabl-project/oxabl/commit/9533db8b9a00fc11f7f62d0abbd82269fd4c6b5b))
* **parser:** FOR EACH TRANSACTION ordering, PRESELECT REPEAT, preproc compound names ([8091260](https://github.com/oxabl-project/oxabl/commit/8091260e864729541e3256fb56c02595d000acbc))
* **parser:** FUNCTION MAP TO, DISPLAY WHEN/TO, tilde line continuation in &DEFINE ([10c6494](https://github.com/oxabl-project/oxabl/commit/10c6494193e49da770fdbbdfb48433e77ffcec4c))
* **parser:** handle :: operator, query-off-end/available as identifiers, TABLE/include params ([7156f65](https://github.com/oxabl-project/oxabl/commit/7156f65d8a1f3c67cbf006b88ff9c95563bf6096))
* **parser:** handle access modifiers on property GET/SET accessors and method names as identifiers ([3d2e157](https://github.com/oxabl-project/oxabl/commit/3d2e157f0df8375cf18ee45b0d81195fc29c9121))
* **parser:** handle BUFFER params, method modifiers/forward-decls, dynamic/system keywords ([1c7ad75](https://github.com/oxabl-project/oxabl/commit/1c7ad758650dc28e0a2828fa9369a31ba535a02a))
* **parser:** handle compound preprop widget names in IN FRAME/BROWSE qualifiers ([014aff9](https://github.com/oxabl-project/oxabl/commit/014aff93089c8a31f5ea505a9814bc7cd40d2725))
* **parser:** handle CONSTRUCTOR STATIC and FRAME-NAME system variable ([bac6720](https://github.com/oxabl-project/oxabl/commit/bac67208acb0046d9b908db16fecd790638602d6))
* **parser:** handle cv/ directory ABL patterns (97.0% → 99.4%) ([7747efe](https://github.com/oxabl-project/oxabl/commit/7747efe7f9c46642fbd7cf1351570a5b4691582c))
* **parser:** handle Dataset.Fill dotted names, DATA-RELATION flag order, EXTENT {&...} ([f780b2f](https://github.com/oxabl-project/oxabl/commit/f780b2f9e9753f829ea33b37b29b1dcfa9b96ccc))
* **parser:** Handle DECIMALS clause in DEFINE VARIABLE, DEFINE PARAMETER, and TEMP-TABLE FIELD ([1fa56cd](https://github.com/oxabl-project/oxabl/commit/1fa56cda2ac0d3d9a957c8b80eeb2474fadb49e7))
* **parser:** Handle DECIMALS clause in DEFINE VARIABLE, DEFINE PARAMETER, and TEMP-TABLE FIELD ([28e207c](https://github.com/oxabl-project/oxabl/commit/28e207c2e7b98e9bb2e0f5820c9518320c2f5598))
* **parser:** handle DO counting loop IN FRAME clause and CREATE BUFFER dotted handle ([c149170](https://github.com/oxabl-project/oxabl/commit/c149170d0edc9faf27af7ce594277b2a10a148d2))
* **parser:** handle DOS/UNIX statements and numeric FORMAT values ([2c4c5a9](https://github.com/oxabl-project/oxabl/commit/2c4c5a9284d3b8f82ed8a47b71527de94f4cf5be))
* **parser:** handle dotnet/ ABL patterns (93.8% → 97.3%) ([dad04f8](https://github.com/oxabl-project/oxabl/commit/dad04f87c1bac2aafb9d6112a243b8e750a02e31))
* **parser:** handle edi/ ABL patterns (96.3% → 97.9%) ([fa7321e](https://github.com/oxabl-project/oxabl/commit/fa7321e16a5c1934bb9b480a3e51f2e72f112410))
* **parser:** handle EXPORT statement ([ad87813](https://github.com/oxabl-project/oxabl/commit/ad878136769959eeef625f352d0ed0ad8c6cafb3))
* **parser:** handle EXTENT qualifier on method and function return types ([38d2127](https://github.com/oxabl-project/oxabl/commit/38d2127494572ccbb2283e73b7f0b500586b600e))
* **parser:** Handle FORMAT/LABEL clause in DEFINE VARIABLE and DEFINE PARAMETER ([5c38a87](https://github.com/oxabl-project/oxabl/commit/5c38a871927734d32a3c62116e2f5ddef35e12d9))
* **parser:** Handle FORMAT/LABEL clause in DEFINE VARIABLE and DEFINE PARAMETER ([7aa437a](https://github.com/oxabl-project/oxabl/commit/7aa437a4af6bfd25c452704077b24f6f387266dd))
* **parser:** handle forms/ ABL patterns — 100% success rate ([c632143](https://github.com/oxabl-project/oxabl/commit/c63214379e98932156278475e80cea03b21e3b8c))
* **parser:** handle gl/ ABL patterns (96.0% → 97.7%) ([9f10b2a](https://github.com/oxabl-project/oxabl/commit/9f10b2a1504c3a86ff837b909d01161c99eb13c2))
* **parser:** handle ic/ ABL patterns (99.5% → 99.8%) ([cf86ebc](https://github.com/oxabl-project/oxabl/commit/cf86ebc1a2f445eee35c18e1ba39038ea438f63b))
* **parser:** handle inc/ ABL patterns (88.5% → 89.9%) ([7d4e82a](https://github.com/oxabl-project/oxabl/commit/7d4e82a12c9b205bd630204fddc20b7a220429a7))
* **parser:** handle LOCKED(), UNDO statement, VIEW/HIDE, FIND OF, NEW with direction args ([39e4723](https://github.com/oxabl-project/oxabl/commit/39e47234863592a835e23375dbdc60dfafc42e91))
* **parser:** handle many real-world ABL patterns from pcna-erp ([386fbce](https://github.com/oxabl-project/oxabl/commit/386fbce593b5972586760d3f3f5872d76bf69d9a))
* **parser:** handle mf/ patterns — ACCUM form 2, NO-UNDO before LIKE, PRESELECT clauses, GROUP BY, OF after lock ([ba82762](https://github.com/oxabl-project/oxabl/commit/ba82762b893e43438cccbfdd8bd4af3ba8233153))
* **parser:** handle ms/ patterns — PROCESS EVENTS, SYSTEM-DIALOG, EXTERNAL proc, ON RETURN, stream preprop, terminal/input expr ([bd6b5cc](https://github.com/oxabl-project/oxabl/commit/bd6b5cc2ed439c604ee29a700002305a3f9cfa74))
* **parser:** handle NO-UNDO before LIKE, BUFFER-COMPARE SAVE, and FORM as FORMAT ([5d53c2d](https://github.com/oxabl-project/oxabl/commit/5d53c2daaa5436d2279ae3eca4ea6648fc32044a))
* **parser:** handle property INITIAL values, Kind::Set as method name, flexible no-undo/initial order ([861cb62](https://github.com/oxabl-project/oxabl/commit/861cb62edd296ccad0346dab8128aabf0a1e1bcc))
* **parser:** handle REPEAT TRANSACTION and comma-joined PRESELECT tables ([d423241](https://github.com/oxabl-project/oxabl/commit/d423241fea846a6f2cd4d8c47afb6d22880f0eb4))
* **parser:** handle reserved-keyword namespace prefixes in OO-ABL class names ([54c3bd0](https://github.com/oxabl-project/oxabl/commit/54c3bd051a73349a7a7aa8141fdd42cd8d50d9ee))
* **parser:** handle TABLE BIND params, DO STOP-AFTER expr, FUNCTION IN qualifier, OO method names ([f2bca7e](https://github.com/oxabl-project/oxabl/commit/f2bca7ea71c56cfb6bb2391721388a84aa1548a9))
* **parser:** handle UNIX field-access periods, CREATE ALIAS, and Preprop implicit args ([5ce8c80](https://github.com/oxabl-project/oxabl/commit/5ce8c80874056e583e030c98cf77b88f49825d3b))
* **parser:** handle VALIDATE clause in DELETE statement ([539acc3](https://github.com/oxabl-project/oxabl/commit/539acc34b81bde47429c13a5c7905fc17e794669))
* **parser:** handle VALUE(), class types, CREATE BUFFER/QUERY, BUFFER-COPY ASSIGN ([2a9a869](https://github.com/oxabl-project/oxabl/commit/2a9a8693471ebbac6c7dde248c5a4308608a725b))
* **parser:** Help/CurrentLanguage as identifiers, empty ELSE, interface method signatures ([794978f](https://github.com/oxabl-project/oxabl/commit/794978f5d425d710c47baab03025c9ea00f05a71))
* **parser:** IF...&THEN inside &ELSEIF condition returns condition early ([eb96767](https://github.com/oxabl-project/oxabl/commit/eb9676765224976b201e17cb0093125d294a66c0))
* **parser:** implicit comma after include-ref in RUN argument lists ([34e9f5a](https://github.com/oxabl-project/oxabl/commit/34e9f5a512aafba61c4010c476ac3bb6f85efb2c))
* **parser:** implicit string concat, RETURN as function return keyword, TRANSACTION in FOR EACH, INPUT/OUTPUT in function calls, SESSION:PARAM/NO-LOCK as identifiers ([e2cb004](https://github.com/oxabl-project/oxabl/commit/e2cb00402834967da5699674dd00fe68d13b6e0c))
* **parser:** implicit WHERE in FOR EACH when table name is a preprop macro ([e415ff9](https://github.com/oxabl-project/oxabl/commit/e415ff973a0ea8ac46385aab83e4a80e1017c57c))
* **parser:** include-ref params after direction keyword; CASE-SENSITIVE parameter option ([12e9cbf](https://github.com/oxabl-project/oxabl/commit/12e9cbf8525b4efe12e24d49bbb0020ca99cedc5))
* **parser:** INIT before type, FORMAT preprop, END PROCEDURE no-period, CAN-FIND USE-INDEX ([f48530f](https://github.com/oxabl-project/oxabl/commit/f48530f12691dc2dc258564f4c493d663c3c4221))
* **parser:** INPUT THROUGH with raw OS commands containing invalid tokens ([a1f303e](https://github.com/oxabl-project/oxabl/commit/a1f303ef6bb3fb7b0869d0335811e9ee51f5d3e1))
* **parser:** label detection with comments, WORKFILE field access, reposition-forward ([19fb5b7](https://github.com/oxabl-project/oxabl/commit/19fb5b72f99ce9b9e8d9e11c56f6b4b6addedf38))
* **parser:** LAST-EVENT/CONNECT as identifiers, WAIT-FOR/SET stmt, stream IO dots, ON frame widget ([fd11b4d](https://github.com/oxabl-project/oxabl/commit/fd11b4d770aea071cecf911a2314cd19e69aa0a2))
* **parser:** make trailing period optional after END CLASS ([b1bd360](https://github.com/oxabl-project/oxabl/commit/b1bd360a99b4c2bd3524589e9837014dee0e59f7))
* **parser:** ms/ round 2 — system vars, RELEASE/DELETE OBJECT, array extent, event names, CREATE VALUE ([a9a5b52](https://github.com/oxabl-project/oxabl/commit/a9a5b521885bfb3d0ad8f2973c5d4ef00fcf3497))
* **parser:** multiline function calls and ACCUM expression ([fd58549](https://github.com/oxabl-project/oxabl/commit/fd585495c16bc8c6f2cc9808ebe3caf85f9a5fa2))
* **parser:** multiple parser fixes for real-world ABL patterns ([f49bc4b](https://github.com/oxabl-project/oxabl/commit/f49bc4bdd50651435da69b47bce7bbe9296a0145))
* **parser:** NO-WAIT/SHARE-LOCK/EXCLUSIVE-LOCK as identifiers, NO-ERROR on expression stmts ([152c670](https://github.com/oxabl-project/oxabl/commit/152c67057d9a0e5b6315c2db8b3b14b4c3228d6c))
* **parser:** object method names, NO-ERROR in assignments, FINALLY in procedures ([f0d7c5f](https://github.com/oxabl-project/oxabl/commit/f0d7c5ffeb3248c414c5b8efc920ca72d4c0e42d))
* **parser:** PAGE/OS-CREATE-DIR statements and DISPLAY COLON specifier ([400fcf3](https://github.com/oxabl-project/oxabl/commit/400fcf320d0f3c657327598b19540ca5ab12c05f))
* **parser:** PAUSE statement, WITH FRAME in FOR EACH, TABLE/Parameter as identifiers ([f13b347](https://github.com/oxabl-project/oxabl/commit/f13b347dc6311f4eecbab7911fc748f22ee3b770))
* **parser:** preproc refs in DEFINE/expressions, BUFFER dynamic, DISPLAY AT/LABEL/SKIP ([c4cc0ef](https://github.com/oxabl-project/oxabl/commit/c4cc0ef857c553163100e3e68a900037de66b748))
* **parser:** QUIT statement, stream I/O trailing clauses, USING dotted name, OS-DIR expression ([45122dc](https://github.com/oxabl-project/oxabl/commit/45122dc46b9aa08799701a3460726af0e35b86c1))
* **parser:** reach 100% on api/ — DYNAMIC-NEW, compound assignments, property EXTENT, method body with period ([6115db5](https://github.com/oxabl-project/oxabl/commit/6115db52d9ea4a25a63c3048fd0c70702e8b9633))
* **parser:** require adjacency in is_field_access_ahead() ([4274343](https://github.com/oxabl-project/oxabl/commit/4274343b0d5f2be13dbd8e1c81bafb10e3e7de71))
* **parser:** require direct adjacency for compound preprop suffix ([c44de43](https://github.com/oxabl-project/oxabl/commit/c44de435b2ceea0a85818b622849dabc468b95b0))
* **parser:** restrict dotted proc-name suffix to safe identifiers; add Fill to can_be_identifier ([6453b4b](https://github.com/oxabl-project/oxabl/commit/6453b4b9a215fffea2065dbd7236aa3b51eb71d3))
* **parser:** RUN ON SERVER, TABLE-HANDLE in run args, function arg TABLE skip ([c1fbf1f](https://github.com/oxabl-project/oxabl/commit/c1fbf1fea7987b347eb22f7b22382218aa4d2730))
* **parser:** RUN path with digit-leading components, BY-REFERENCE in args, preprop macro invocations ([4e2c57e](https://github.com/oxabl-project/oxabl/commit/4e2c57e108c0a17179579122e03cdb9b88b4d0d1))
* **parser:** RUN statement clause ordering and trailing expression ([c716a3e](https://github.com/oxabl-project/oxabl/commit/c716a3ebdd4ad55c942b7d05584cdfd1fd4bcf5b))
* **parser:** same-line guard in parse_dotted_name; replication triggers support OLD BUFFER clause ([906547c](https://github.com/oxabl-project/oxabl/commit/906547c225c8cd43cc35e3db0a2108baef6fb64e))
* **parser:** silently consume orphaned END at top-level parse position ([9c0b857](https://github.com/oxabl-project/oxabl/commit/9c0b857475595b0eac325661e106186af0e55bdb))
* **parser:** Skip comment tokens during parsing ([34de788](https://github.com/oxabl-project/oxabl/commit/34de7882128dc846b433012d1a30fb4dec8c794a))
* **parser:** skip IncludeArgReference after DEFINE ([9b56134](https://github.com/oxabl-project/oxabl/commit/9b56134a2f783712671822e06a16bf7561db5208))
* **parser:** standalone preprop macros before block labels and statement boundaries ([e243c90](https://github.com/oxabl-project/oxabl/commit/e243c907f291380124925a14fc25601ebbab5cea))
* **parser:** treat standalone preprop macros as terminated when followed by statement-starting keyword ([e9c038b](https://github.com/oxabl-project/oxabl/commit/e9c038b1798fe9532eead2cab0ab7b8de0035fce))
* **parser:** TRIGGERS lexes as Kind::Triggers not Kind::Identifier; VMS statement support ([677948c](https://github.com/oxabl-project/oxabl/commit/677948ca9e76c6bd6e3d87e1f860826c2dffdcba))
* **parser:** UI widgets, DEFINE FRAME, numeric labels, CLEAR/HIDE, SIZE keyword ([ab5eb21](https://github.com/oxabl-project/oxabl/commit/ab5eb213bd115a1ab07e018a35809d027e51723f))
* **parser:** UNDO RETURN expr, ACCUM parenthesized field, OS-RENAME skip ([b313c49](https://github.com/oxabl-project/oxabl/commit/b313c49bb6e0a71bbb832122fc013c2ed4dfef33))
* **parser:** UPDATE as identifier, 'in' data type abbrev, IN FRAME assign clause ([c18939b](https://github.com/oxabl-project/oxabl/commit/c18939b0c20308e4887039258992637a51f09325))
* **parser:** USING FROM clause, ThisObject, class name dots, ABSTRACT after name ([81b8e1a](https://github.com/oxabl-project/oxabl/commit/81b8e1ae1ba76f8594448dcca0a5ad5b096a07fb))
* **parser:** VAR type-name syntax supports class type identifiers ([0d1309f](https://github.com/oxabl-project/oxabl/commit/0d1309f769601c3452aeb6bb8689044eb3daf1af))
* **parser:** widget attributes, IN FRAME/BROWSE qualifiers, ENABLE, empty args ([e8ffef3](https://github.com/oxabl-project/oxabl/commit/e8ffef327795b529e4533f24f1ed0a0359377807))
* **parser:** widget keyword attribute access and include-arg-ref member assignment ([52899d3](https://github.com/oxabl-project/oxabl/commit/52899d35a1886e365eddbadbce633ce7ebba5e27))


### Performance Improvements

* **lexer,parser:** pre-allocate token Vec, SmallVec for AssignPair, ternary fast-path ([230fff9](https://github.com/oxabl-project/oxabl/commit/230fff913a04cdd4c5114769575a1e334a09e1ae))
* **lexer,parser:** pre-allocate token Vec, SmallVec for AssignPair, ternary fast-path ([e1f1e37](https://github.com/oxabl-project/oxabl/commit/e1f1e375077644e4ad12bb64156244628f0637d7))
* **lexer:** eliminate heap allocations in keyword matching ([2fa1b87](https://github.com/oxabl-project/oxabl/commit/2fa1b87f9c7bb37ea3fa699a9e4073a34d8c5226))
* **lexer:** eliminate heap allocations in keyword matching ([c7553f7](https://github.com/oxabl-project/oxabl/commit/c7553f7d46f0867c43794e7afdd65eb31795953b))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.4.0 to 0.5.0
    * oxabl_lexer bumped from 0.3.1 to 0.4.0

## [0.4.0](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v0.3.1...oxabl_parser-v0.4.0) (2026-04-04)


### Features

* **parser:** Add ASSIGN statement and FUNCTION definition support ([5f85a32](https://github.com/oxabl-project/oxabl/commit/5f85a323a572a36b3c2206bbc5eaddb37bcc57f0))
* **parser:** Add ASSIGN statement and FUNCTION definition support ([e1308a1](https://github.com/oxabl-project/oxabl/commit/e1308a1af9f5f4cb90bfb1be4626028d4bfc0970))
* **parser:** Add CATCH, FINALLY, and THROW support ([193a410](https://github.com/oxabl-project/oxabl/commit/193a410fc78869bff108e1316ee58e243809a863))
* **parser:** Add CATCH, FINALLY, and THROW support ([c97bd34](https://github.com/oxabl-project/oxabl/commit/c97bd347d8a629675767aeb93abde8e8ec585f8a))
* **parser:** Add error recovery with Program type and synchronize() ([7286eda](https://github.com/oxabl-project/oxabl/commit/7286eda62877fc172ea77d4b9c96fd9e87cf2725))
* **parser:** Add error recovery with Program type and synchronize() ([1e59952](https://github.com/oxabl-project/oxabl/commit/1e59952a90a8380ce745d033403e16bef5ac0887))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 0.3.0 to 0.4.0

## [0.3.1](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v0.3.0...oxabl_parser-v0.3.1) (2026-04-03)


### Bug Fixes

* add versions to deps ([0d1b3d9](https://github.com/oxabl-project/oxabl/commit/0d1b3d951b3ce8b2f617c9b328644fad2ce1a551))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_lexer bumped from 0.3.0 to 0.3.1

## [0.3.0](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v0.2.0...oxabl_parser-v0.3.0) (2026-04-03)


### Features

* **workspace:** Add publish job to release workflow and add required fields to crates ([e819219](https://github.com/oxabl-project/oxabl/commit/e81921955ff078af912aa3e590673ffe3ed576fa))
* **workspace:** Add publish job to release workflow and add required… ([439b2f5](https://github.com/oxabl-project/oxabl/commit/439b2f5397bc899948fd38227fe98f7b667c18db))

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_parser-v0.1.0...oxabl_parser-v0.2.0) (2026-04-03)


### Features

* **ast,parser:** add initial AST literal types and token conversion ([f4ce601](https://github.com/oxabl-project/oxabl/commit/f4ce601f564e5d89e15f8d983cb459e3ff0e968a))
* **oxabl:** Initial commit, includes oxabl, oxabl_lexer, oxable_parser ([a21760b](https://github.com/oxabl-project/oxabl/commit/a21760b5b749d3ff1f810c42ec941433d02fe675))
* **parser, ast:** Added IF statements ([27f374b](https://github.com/oxabl-project/oxabl/commit/27f374b221c953f1f534a1776c0116c2d1fb6fb5))
* **parser, ast:** Added loop control ([2c88f00](https://github.com/oxabl-project/oxabl/commit/2c88f00c1edf86144ba996fa305a0898c42de9e7))
* **parser, ast:** Added postfix expressions ([35a1336](https://github.com/oxabl-project/oxabl/commit/35a1336ec9068408d6ceaccb5128b685f68533d4))
* **parser, ast:** Added support for CASE statements ([26f56e8](https://github.com/oxabl-project/oxabl/commit/26f56e81b8e35dbc7d78f3abc8c0466778786e0a))
* **parser, ast:** Began procedure parsing support ([6d6af23](https://github.com/oxabl-project/oxabl/commit/6d6af2373d13d71b0c9f5e708713891e87968d48))
* **parser, ast:** Do block and loop parsing ([f572747](https://github.com/oxabl-project/oxabl/commit/f5727477de3ecd3ab5ec2388e1b92739d146c5b7))
* **parser, ast:** Function Calls ([13c3460](https://github.com/oxabl-project/oxabl/commit/13c34602d03c20fc1adc493ec63fdc5bec7e4ce7))
* **parser, ast:** Simple statement support ([23ff400](https://github.com/oxabl-project/oxabl/commit/23ff4005a2f65d67990aec25a1bad40cc14894ad))
* **parser,ast:** Find statement support ([00db56e](https://github.com/oxabl-project/oxabl/commit/00db56ed11836cd04bd1717d3e8bd1914d3aa0cc))
* **parser,expressions:** Parentheses, Unary, literals ([513aabf](https://github.com/oxabl-project/oxabl/commit/513aabf2873432815b4b9f950cbecfab3e3fdb59))
* **parser:** Add DISPLAY and MESSAGE statement support ([76cdf7b](https://github.com/oxabl-project/oxabl/commit/76cdf7b6287d2cfbc1a387acef428a08cd77c312))
* **parser:** Add parse additive and multiplicative, more tests ([5989a3b](https://github.com/oxabl-project/oxabl/commit/5989a3bfab9757203b908591d458043c20938cad))
* **parser:** Add PERSISTENT, ASYNCHRONOUS, INPUT-OUTPUT, and RUN IN SUPER support ([facd229](https://github.com/oxabl-project/oxabl/commit/facd229d7ad6bea05d86cb310b3c757e37129a3d))
* **parser:** Added variable definition statements ([53b7b1d](https://github.com/oxabl-project/oxabl/commit/53b7b1ddbca915b473f405b39988461f5d9c882c))
* **parser:** Began expressions ([1aeaf88](https://github.com/oxabl-project/oxabl/commit/1aeaf88d157e50b026e18ab728453a0a3e00363e))
* **parser:** Can parse longer expressions ([a275c87](https://github.com/oxabl-project/oxabl/commit/a275c87339c1c16c94b42dd6fa5ea4e9e6555eec))
* **parser:** Complete RUN statement parsing with tests ([3fe1347](https://github.com/oxabl-project/oxabl/commit/3fe134791ffe72f65971ef248647070d70a988df))
* **parser:** Complete RUN statement support ([1cc7fa5](https://github.com/oxabl-project/oxabl/commit/1cc7fa5eef4222c1fba4346e5287e07f1f3aab23))
* **parser:** Created parser.rs with struct and AST navigation methods ([e323e8c](https://github.com/oxabl-project/oxabl/commit/e323e8c342cd5c111453ac79e1ff63acc98ec1fc))
* **parser:** Finished procedure and input/output parameter support ([138a24a](https://github.com/oxabl-project/oxabl/commit/138a24a748e7565a94e5c75565fcb07be704122a))
* **parser:** Finished token to literal, added tests for each literal type ([236b230](https://github.com/oxabl-project/oxabl/commit/236b230b7cce84f329719a6bbaded04160898cce))
* **parser:** Finished token to literal, added tests for each literal type ([#2](https://github.com/oxabl-project/oxabl/issues/2)) ([9846c9d](https://github.com/oxabl-project/oxabl/commit/9846c9d1d5ac246e000d98d976af4245b31d0258))
* **parser:** Modulo, Identifiers, Comparison, Logical ([b59575b](https://github.com/oxabl-project/oxabl/commit/b59575bc0b78337327938c942120680e3ff8d1f0))
* **parser:** Parse ternary, %, more tests ([2e88e02](https://github.com/oxabl-project/oxabl/commit/2e88e02952d794f8bc2b4496073b874a6c5b9c50))
* **parser:** reduce repetative code with expect_kind ([2fcdd4b](https://github.com/oxabl-project/oxabl/commit/2fcdd4bb1ac5e9706034512152cee29245525f0d))
* **parser:** Started on RUN statement support ([31b5c9a](https://github.com/oxabl-project/oxabl/commit/31b5c9ae847408f3eb14fe67df74dbb5063c9431))
* **paser, lexer, ast:** WIP, adding for each and lock type support ([81e12b2](https://github.com/oxabl-project/oxabl/commit/81e12b2cdb5890a97c588de12c958d17c01780db))


### Bug Fixes

* clippy warnings ([9f684c6](https://github.com/oxabl-project/oxabl/commit/9f684c6249468661cb30f177f796a3e6331e2e97))
* **parser,ast,lexer:** Mod, Comparison Operators ([e33134d](https://github.com/oxabl-project/oxabl/commit/e33134d0ec3a725a3e976b8a374d164aac8eb1ac))
* **parser,ast:** Restore green build — rename typo, remove debug prints, wire RUN dispatch ([f0f6db0](https://github.com/oxabl-project/oxabl/commit/f0f6db0cdc9b92d47264f78abc06ada6fadc964f))
* **parser:** Fix test with missing import ([221596f](https://github.com/oxabl-project/oxabl/commit/221596f2896a3d212c7ad563c2bea6c8a3539eeb))
* **parser:** Handle COLUMN-LABEL in DISPLAY items, remove unused import ([66d7a26](https://github.com/oxabl-project/oxabl/commit/66d7a264a93d0d40ae62f586cbfb591771ce5e7c))
