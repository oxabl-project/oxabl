# Changelog

## [0.2.1](https://github.com/oxabl-project/oxabl/compare/oxabl_scope-v0.2.0...oxabl_scope-v0.2.1) (2026-10-20)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_flow bumped from 0.3.0 to 0.3.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1
  * dev-dependencies
    * oxabl_parser bumped from 3.0.0 to 3.0.1
    * oxabl_schema bumped from 0.4.0 to 0.4.1

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_scope-v0.1.0...oxabl_scope-v0.2.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **parser:** `StreamOperation::From` and `StreamOperation::To` now carry a `StreamTarget` instead of an `Expression`; the new `StreamTarget::FileName` holds the bare file name.
* **scope:** `oxabl_semantic::AnalysisContext` has a new public field, `elided_includes`, so a struct literal of it no longer compiles. Build it with `AnalysisContext::new` and `with_elided_includes`.
* **lexer:** `oxabl_lexer::Kind` gains File, Total, Count, Average, Maximum, Minimum, SubTotal, SubCount, SubAverage, SubMaximum and SubMinimum; these words no longer lex as `Kind::Identifier`.

### Features

* **effects:** strict ABL M4: call graph, database effects, callee summaries, DB001 and explain ([f68126d](https://github.com/oxabl-project/oxabl/commit/f68126d6b8d4f7718558a542abfdc90a43b7cf7e))
* **lexer:** lex FILE and the aggregate words as keywords ([8d3cde1](https://github.com/oxabl-project/oxabl/commit/8d3cde15d110bcc094910cdbc93c9e6e03e6e338))
* **nulls:** strict ABL M5: oxabl_nulls, NL001, NL002 and NL003 ([906d636](https://github.com/oxabl-project/oxabl/commit/906d636b6a35509bce15cb92cda5aefda72dd527))
* **scope:** add oxabl_scope, the compiler's buffer scopes, transaction blocks and locks per unit ([cebe727](https://github.com/oxabl-project/oxabl/commit/cebe727b779e8d0806aa41e60d2968f56ed0cca6))
* **scope:** check the scope engine against COMPILE ... LISTING output ([19f0ae2](https://github.com/oxabl-project/oxabl/commit/19f0ae2d57391ee48d79c747f1a7b81aa9cafe33))
* **scope:** expose database updates and analyse over a prebuilt flow graph ([773f731](https://github.com/oxabl-project/oxabl/commit/773f731782d3f0970557c4cab6a4c0d5af779a07))
* **scope:** expose the resolved buffer references, with whether each names a field ([d725929](https://github.com/oxabl-project/oxabl/commit/d72592926ffb170a1903abba7074563d199ea1f1))
* **scope:** record the table of each record read ([3339571](https://github.com/oxabl-project/oxabl/commit/333957195343ea7c7f693bc4fc5cdda97749a3d8))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))
* **parser:** read an unquoted OUTPUT/INPUT file name as a name ([130d384](https://github.com/oxabl-project/oxabl/commit/130d384c83d7e61c4840cecb2f155b5d0c70b680))
* **parser:** unquoted stream file names and MESSAGE UPDATE fields ([06c823c](https://github.com/oxabl-project/oxabl/commit/06c823cc47c388bbbcf63639cb32a3e8889e9a45))
* **scope:** count the screen form of ASSIGN and read written in execution order ([bf95fb7](https://github.com/oxabl-project/oxabl/commit/bf95fb795baf4e35355ee3b336d489ca6b5ca2bb))
* **scope:** label an opaque or passing reference in another routine by what it is ([ef972fc](https://github.com/oxabl-project/oxabl/commit/ef972fcb3ebdab5c4bf7c53085d77d301770d8df))
* **scope:** let an include that did not arrive between statements hide a write or a release ([5afe7fd](https://github.com/oxabl-project/oxabl/commit/5afe7fd02ac03eb367e9b5cf08318325605f2575))
* **scope:** mark a reference in an unknown statement form as opaque for temp-tables too ([d911d47](https://github.com/oxabl-project/oxabl/commit/d911d473d4d41007f4e292cdf597e44c49b1fe59))
* **scope:** pair a listing path that starts with .. with the resolved path ([eb9cdcb](https://github.com/oxabl-project/oxabl/commit/eb9cdcbfaa31fe98adb812d2a4427e13b97ee9f7))
* **scope:** pair LISTING blocks by the end of the file path ([8965d2d](https://github.com/oxabl-project/oxabl/commit/8965d2d115bf1e3673a1e33c777184d3492ed78a))
* **scope:** read `+` as an operator in the screen form of ASSIGN ([8e04b5d](https://github.com/oxabl-project/oxabl/commit/8e04b5d0fd6881626cd063af8e8adb8e8d977cbb))
* **scope:** read a file name as no record reference ([0d36d78](https://github.com/oxabl-project/oxabl/commit/0d36d7881db1e0d6a05551e9a071fe2ea647b4fa))
* **scope:** read a lock that unexpanded text may hide as Unknown ([ca9caae](https://github.com/oxabl-project/oxabl/commit/ca9caae9d8ed232f1a64271222b2fac34c58813e))


### Performance Improvements

* **flow:** read block headers from the parser's tokens ([a1a0f78](https://github.com/oxabl-project/oxabl/commit/a1a0f78a8215fd984637c916ea2b939c2f87822d))
* **lint:** build the CFGs once for the scope and handle analyses ([b72e92a](https://github.com/oxabl-project/oxabl/commit/b72e92a3a3bab5b0f7fea6a9a1fcf2eeba79a1c7))
* **scope:** fold a reference's words only when it is kept ([45538a6](https://github.com/oxabl-project/oxabl/commit/45538a68193892eab2b917c7a0bc291f8e61e0d9))
* **scope:** index field names for the exact searches of a bare field ([a2f25e4](https://github.com/oxabl-project/oxabl/commit/a2f25e40d04f30eb39289b966aa1af79cd2d8aaa))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
    * oxabl_schema bumped from 0.3.1 to 0.4.0
