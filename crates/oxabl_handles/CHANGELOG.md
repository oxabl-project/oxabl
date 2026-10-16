# Changelog

## [0.2.0](https://github.com/oxabl-project/oxabl/compare/oxabl_handles-v0.1.0...oxabl_handles-v0.2.0) (2026-10-16)


### ⚠ BREAKING CHANGES

* **parser:** StatementKind::Procedure has a new `is_external` field. Code that builds or destructures it without `..` must add the field.
* **handles:** `oxabl_semantic::AnalysisContext` has a new public field, `include_regions`. Code that builds it with a struct literal must set it;

### Features

* **handles:** add oxabl_handles, lifetimes of dynamic objects per unit ([005ae67](https://github.com/oxabl-project/oxabl/commit/005ae67771f889bd80c71d268554ae43b16b3b3d))
* **handles:** read compound guards, failed NO-ERROR creates and class widget pools ([881727e](https://github.com/oxabl-project/oxabl/commit/881727ee87815748f33a87281d9d1ed56c9f4fac))
* **handles:** read the new create, delete, pool, release and apply forms ([a6b51d2](https://github.com/oxabl-project/oxabl/commit/a6b51d22b9c464baa0e3088a84e2ad5c623730ee))
* **parser:** mark EXTERNAL procedure declarations ([aee65aa](https://github.com/oxabl-project/oxabl/commit/aee65aa962334a06fb829d839a254ee6ba35c1d0))
* **scope:** strict-analysis M3: oxabl_scope, the LISTING harness, LK001-LK003 and TX001-TX005 ([d4779c9](https://github.com/oxabl-project/oxabl/commit/d4779c9f0ff420c55403a67b93c773dd24a4c6dd))


### Bug Fixes

* **handles:** close false-positive causes found in the HD001-HD005 triage ([d4e211c](https://github.com/oxabl-project/oxabl/commit/d4e211ce75927af28a0370fbd4966e73970b78ad))
* **handles:** do not bind a call to a body whose parameter cannot take the argument ([e58e019](https://github.com/oxabl-project/oxabl/commit/e58e01916ecf0109884b25403d78c83374db01bc))
* **handles:** model calls, :HANDLE identity, unmapped frees, COPY-LOB, shadowed frees and a work budget ([05f23ef](https://github.com/oxabl-project/oxabl/commit/05f23ef729196a3175317ac9d3b89815c3042c03))
* **handles:** read only the callee bodies that run, and bound the read ([613fad3](https://github.com/oxabl-project/oxabl/commit/613fad34c004b5b5853a7d42470f6790a0026d1d))
* **handles:** read what a callee does with a handle, resolve methods by receiver ([9227fe2](https://github.com/oxabl-project/oxabl/commit/9227fe2c7ea3907a02116b1e8c4b8fe6004fa3d5))
* issue sweep 2 (downstream edges, parser members) ([430fc3b](https://github.com/oxabl-project/oxabl/commit/430fc3b3302699c55984b2bfc2cc78c265584406))


### Performance Improvements

* **handles:** find a create's unit in the shared CFGs ([c1e5e0a](https://github.com/oxabl-project/oxabl/commit/c1e5e0a5decf8056308a6e58acda786db65982b9))
* **handles:** keep statement facts only for units that create something ([5634d47](https://github.com/oxabl-project/oxabl/commit/5634d47e5eed16f1d442eec5e3204a380237603c))
* **lint:** build the CFGs once for the scope and handle analyses ([b72e92a](https://github.com/oxabl-project/oxabl/commit/b72e92a3a3bab5b0f7fea6a9a1fcf2eeba79a1c7))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * oxabl_ast bumped from 2.0.0 to 3.0.0
    * oxabl_flow bumped from 0.2.0 to 0.3.0
    * oxabl_lexer bumped from 1.0.2 to 2.0.0
  * dev-dependencies
    * oxabl_parser bumped from 2.0.0 to 3.0.0
