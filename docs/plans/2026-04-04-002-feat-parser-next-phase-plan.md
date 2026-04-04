---
title: "Parser Next Phase: OO-ABL, Stream I/O, Cleanup"
type: feat
status: active
date: 2026-04-04
---

# Parser Next Phase: OO-ABL, Stream I/O, Cleanup

## Current State

**Version:** parser 0.4.0, ast 0.4.0, 230 parser tests, 278 total  
**Base commit:** `75be8bd` (master as of 2026-04-04)

The parser now handles: DEFINE VARIABLE/VAR/PARAMETER/TEMP-TABLE/BUFFER, DO/REPEAT/FOR EACH, IF/THEN/ELSE, CASE, FIND, PROCEDURE, FUNCTION, RUN, DISPLAY, MESSAGE, ASSIGN, CATCH/FINALLY/THROW, and error recovery via `parse_program()`.

## Goal

Expand the parser to cover the next tier of ABL constructs — prioritized by how often they appear in production ABL codebases and how much they block linting/formatting. All branches are created off the same commit (`75be8bd`) and merged sequentially.

## Branch Architecture

**Branch 0 (`feat/lexer-keywords`) merges first.** It adds all new Kind variants needed by branches A, B, and C via `keyword_overrides.toml` + `cargo run -p oxabl_codegen`. This is the only branch that touches generated files (`kind.rs`, `build.rs`) and `keyword_overrides.toml`.

**Branches A, B, C all fork from master after Branch 0 merges.** They only touch parser and AST files. Conflicts are minimized by:

- **Each branch adds new `Statement` variants at the END of the enum** (before `Leave`/`Next`/`Return`/`Empty`)
- **Each branch adds new parser methods at the END of the `impl Parser` block** (before `parse_block_body`)
- **Each branch adds a dispatch line to `parse_statement()`** — this is the only shared edit point, and each is a single `if self.check(Kind::Foo)` block, trivially rebased
- **Each branch adds to `can_start_statement()`** — again a single `Kind::Foo` addition, trivially rebased
- **Each branch appends its test section at the END of `tests.rs`** with a clear section header

**Merge order:** 0 → A → B → C → D (after 0, each subsequent branch needs only a trivial rebase before merge)

---

## Branch 0: `feat/lexer-keywords` — Add all Kind variants for upcoming parser work

**This merges first.** All subsequent parser branches fork from the commit after this merges.

### Keywords to add (`resources/keyword_overrides.toml`)

**OO-ABL (for Branch A):**

| Keyword | keyword_type | min_abbreviation | Notes |
|---------|-------------|-----------------|-------|
| CLASS | Statement | | |
| INTERFACE | Statement | | |
| INHERITS | Option | | |
| IMPLEMENTS | Option | | |
| METHOD | Statement | | |
| CONSTRUCTOR | Statement | CONSTRUCT | Check ABL docs for abbreviation |
| DESTRUCTOR | Statement | DESTRUCT | Check ABL docs for abbreviation |
| ABSTRACT | Option | | |
| FINAL | Option | | |
| OVERRIDE | Option | OVERRI | Check ABL docs |
| STATIC | Option | | |
| PUBLIC | Option | | |
| PRIVATE | Option | | |
| PROTECTED | Option | | |
| PACKAGE-PRIVATE | Option | | |
| VOID | Type | | |
| USING | Statement | | |

**Stream I/O (for Branch B):**

| Keyword | keyword_type | min_abbreviation | Notes |
|---------|-------------|-----------------|-------|
| THROUGH | Option | THRU | ABL abbreviation |
| APPEND | Option | | |
| CLOSE | Statement | | Standalone CLOSE (not QUERY-CLOSE) |

**Database ops (for Branch C):**

| Keyword | keyword_type | min_abbreviation | Notes |
|---------|-------------|-----------------|-------|
| GET | Statement | | Standalone GET (not GET-BYTE etc.) |

**Migrating existing `is_identifier_text()` usages:**

These keywords are currently matched by string comparison in the parser. Add Kind variants so they can use proper dispatch:

| Keyword | keyword_type | Notes |
|---------|-------------|-------|
| CATCH | Statement | Currently `is_identifier_text("catch")` |
| FINALLY | Statement | Currently `is_identifier_text("finally")` |
| FUNCTION | Statement | Currently `is_identifier_text("function")` |
| VAR | Statement | Currently `is_identifier_text("var")` |
| THROW | Statement | Currently `is_identifier_text("throw")` — used via UNDO, THROW |

### Steps

1. Add all entries above to `keyword_overrides.toml`
2. Run `cargo run -p oxabl_codegen` to regenerate `kind.rs` and `build.rs`
3. Run `cargo check -p oxabl_lexer` to verify compilation
4. Run `cargo test -p oxabl_lexer` to verify existing tests still pass
5. **Do NOT migrate parser dispatch in this branch** — that happens in branches A/B/C/D. This branch only touches lexer files + `keyword_overrides.toml`.

### Validation

- `cargo test` — all 278 tests pass (new Kind variants are additive, no parser changes)
- `cargo check --workspace` — no warnings

---

## Branch A: `feat/oo-abl` — CLASS, METHOD, PROPERTY, INTERFACE

**Why first:** OO-ABL is the most structurally complex addition and touches the most files. Merging it first means subsequent branches rebase onto its changes rather than the other way around.

### AST additions (`crates/oxabl_ast/src/statement.rs`)

Add these variants to `Statement`, **before the `Leave` variant**:

```rust
/// CLASS definition.
///
/// ```abl
/// CLASS MyApp.CustomerService INHERITS BaseService IMPLEMENTS IService:
///   /* body */
/// END CLASS.
/// ```
Class {
    /// Fully qualified class name (e.g., "MyApp.CustomerService").
    name: Identifier,
    /// Parent class name, if any.
    inherits: Option<String>,
    /// Implemented interfaces.
    implements: Vec<String>,
    /// Whether ABSTRACT was specified.
    is_abstract: bool,
    /// Whether FINAL was specified.
    is_final: bool,
    /// Class body statements (methods, properties, variables, constructors).
    body: Vec<Statement>,
},

/// METHOD definition inside a CLASS.
///
/// `METHOD PUBLIC VOID DoSomething(INPUT x AS INTEGER): body END METHOD.`
Method {
    /// Access modifier.
    access: AccessModifier,
    /// Whether STATIC was specified.
    is_static: bool,
    /// Whether ABSTRACT was specified (no body).
    is_abstract: bool,
    /// Whether OVERRIDE was specified.
    is_override: bool,
    /// Return type (Void is not in DataType — use Option<DataType>, None = VOID).
    return_type: Option<DataType>,
    /// Method name.
    name: Identifier,
    /// Method body (empty if abstract).
    body: Vec<Statement>,
},

/// PROPERTY definition inside a CLASS.
///
/// `DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.`
Property {
    /// Access modifier.
    access: AccessModifier,
    /// Whether STATIC was specified.
    is_static: bool,
    /// Property name.
    name: Identifier,
    /// Property data type.
    data_type: DataType,
    /// Whether NO-UNDO was specified.
    no_undo: bool,
    /// Whether GET is defined (true for `GET.` or `GET: body END.`).
    has_get: bool,
    /// Whether SET is defined (true for `SET.` or `SET: body END.`).
    has_set: bool,
},

/// CONSTRUCTOR definition inside a CLASS.
///
/// `CONSTRUCTOR PUBLIC MyClass(): body END CONSTRUCTOR.`
Constructor {
    access: AccessModifier,
    body: Vec<Statement>,
},

/// DESTRUCTOR definition inside a CLASS.
///
/// `DESTRUCTOR PUBLIC MyClass(): body END DESTRUCTOR.`
Destructor {
    body: Vec<Statement>,
},

/// INTERFACE definition.
///
/// `INTERFACE IService: METHOD PUBLIC VOID Run(). END INTERFACE.`
Interface {
    name: Identifier,
    inherits: Vec<String>,
    body: Vec<Statement>,
},

/// USING statement for class imports.
///
/// `USING MyApp.Services.*.`
/// `USING Progress.Lang.Object.`
Using {
    type_name: String,
},
```

Add supporting types:

```rust
/// Access modifier for OO-ABL members.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessModifier {
    Public,
    Private,
    Protected,
    PackagePrivate,
}
```

### Parser additions (`crates/oxabl_parser/src/parser/statements.rs`)

1. Add `Kind::Class` and `Kind::Interface` to `can_start_statement()`
2. Add dispatch branches in `parse_statement()` for `Kind::Class` and `Kind::Interface`
3. Add `Kind::Using` dispatch (check via `is_identifier_text("using")` since USING may lex as identifier)
4. Implement:
   - `parse_class()` — consume CLASS, parse name, optional INHERITS/IMPLEMENTS, colon, body until END CLASS
   - `parse_interface()` — same structure, different keywords
   - `parse_method()` — consume METHOD, parse access modifiers/return type/name, optional params, colon, body until END METHOD
   - `parse_property()` — parse DEFINE [access] PROPERTY ..., GET./SET. or GET: body END./SET: body END.
   - `parse_constructor()` — consume CONSTRUCTOR, access, params, colon, body until END CONSTRUCTOR
   - `parse_destructor()` — consume DESTRUCTOR, body until END DESTRUCTOR
   - `parse_using()` — consume USING, parse dotted name (may end in `.*`), period
   - `parse_access_modifier()` — helper returning `Option<AccessModifier>`

**Important:** DEFINE PROPERTY must be detected inside `parse_define_statement()` — after consuming DEFINE, check for access modifier + PROPERTY before falling through to VARIABLE/TEMP-TABLE/BUFFER. Consider also handling `DEFINE [access] VARIABLE` and `DEFINE [access] TEMP-TABLE` at this point for OO-ABL scope modifiers (`PUBLIC`, `PRIVATE`, etc.).

### Lexer keywords needed

**None of these exist as Kind variants.** All OO-ABL keywords currently lex as `Kind::Identifier`. Two approaches:

**Option 1 (recommended): Use `is_identifier_text()` throughout.** This is the pattern already used for CATCH, FINALLY, THROW, FUNCTION, and VAR. It avoids touching the lexer/codegen pipeline and keeps the branch self-contained. OO-ABL keywords like CLASS, METHOD, PROPERTY are context-dependent anyway (e.g., `CLASS` is also valid as a data type modifier `AS CLASS ClassName`).

**Option 2: Add Kind variants via codegen.** Add to `keyword_overrides.toml` and run `cargo run -p oxabl_codegen`. This gives cleaner dispatch but creates conflicts with any branch that also touches `kind.rs` or `keyword_overrides.toml`.

Keywords to match (all via `is_identifier_text()`):
- `class`, `interface`, `inherits`, `implements`, `method`, `property`, `constructor`, `destructor`
- `abstract`, `final`, `override`, `static`
- `public`, `private`, `protected`, `package-private`
- `using`, `void`, `get`, `set`

### Tests (~20)

Append section `// ===================== CLASS/OO-ABL tests =====================` at end of tests.rs:

1. Simple class with empty body
2. Class with INHERITS
3. Class with IMPLEMENTS (single)
4. Class with INHERITS + IMPLEMENTS (multiple)
5. ABSTRACT class
6. FINAL class
7. Method with PUBLIC VOID
8. Method with PRIVATE, returns INTEGER
9. STATIC method
10. ABSTRACT method (no body)
11. OVERRIDE method
12. Property with inline GET. SET.
13. Property with body GET: / SET: blocks
14. Constructor
15. Destructor
16. Interface with method signatures
17. Interface with INHERITS
18. USING with qualified name
19. USING with wildcard (`USING MyApp.Services.*`)
20. Class with mixed members (method + property + variable)

---

## Branch B: `feat/stream-io` — INPUT/OUTPUT/CLOSE, PUT, EXPORT, IMPORT

**Why:** Stream I/O is heavily used in batch processing ABL. These are simple keyword-initiated statements with straightforward parsing.

### AST additions (`crates/oxabl_ast/src/statement.rs`)

```rust
/// INPUT/OUTPUT/INPUT-OUTPUT THROUGH/FROM/TO statement.
///
/// `INPUT FROM "data.txt".`
/// `OUTPUT TO "report.txt" APPEND.`
/// `INPUT CLOSE.`
StreamIO {
    /// Direction of the stream operation.
    direction: StreamDirection,
    /// Stream name if named (`INPUT STREAM sIn FROM ...`).
    stream_name: Option<Identifier>,
    /// The operation — open source/target, or close.
    operation: StreamOperation,
},

/// PUT statement — writes to a stream.
///
/// `PUT UNFORMATTED "hello" SKIP.`
/// `PUT STREAM sOut UNFORMATTED x SKIP(2).`
Put {
    stream_name: Option<Identifier>,
    unformatted: bool,
    items: Vec<Expression>,
},

/// EXPORT statement — writes delimited data.
///
/// `EXPORT Customer.Name Customer.Balance.`
/// `EXPORT DELIMITER "," Customer.`
Export {
    stream_name: Option<Identifier>,
    delimiter: Option<String>,
    items: Vec<Expression>,
},

/// IMPORT statement — reads delimited data.
///
/// `IMPORT cName cBalance.`
/// `IMPORT DELIMITER "," Customer.`
Import {
    stream_name: Option<Identifier>,
    delimiter: Option<String>,
    targets: Vec<Identifier>,
    unformatted: bool,
},
```

Supporting types:

```rust
/// Direction of a stream I/O operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StreamDirection {
    Input,
    Output,
    InputOutput,
}

/// What a stream I/O statement does.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StreamOperation {
    /// `INPUT FROM "file.txt"` or `OUTPUT TO "file.txt" [APPEND]`
    Open { target: Expression, append: bool },
    /// `INPUT THROUGH "command"` (pipe through OS command)
    Through { command: Expression, append: bool },
    /// `INPUT CLOSE` / `OUTPUT CLOSE`
    Close,
}
```

### Parser additions

1. Add dispatch in `parse_statement()`: `Kind::Input`, `Kind::Output`, `Kind::InputOutput` → `parse_stream_io()`; `Kind::Put` → `parse_put()`; `Kind::Export` → `parse_export()`; `Kind::Import` → `parse_import()`
2. Add `Kind::Put`, `Kind::Export`, `Kind::Import` to `can_start_statement()`
3. Note: `Kind::Input`/`Kind::Output`/`Kind::InputOutput` are already in the lexer for parameter directions, but as statement-starters they mean stream I/O. Disambiguate: if token is `Kind::Input` and next is NOT `Kind::Identifier` with text matching a parameter name context, treat as stream I/O. In practice: INPUT at statement level (not inside a DEFINE PARAMETER or RUN argument list) is always stream I/O. Since `parse_statement()` runs at statement level, this is already disambiguated.
4. Implement `parse_stream_io()`, `parse_put()`, `parse_export()`, `parse_import()`

### Lexer keywords needed

**Already exist as Kind variants:** `PUT`, `EXPORT`, `IMPORT`, `STREAM`, `FROM`, `UNFORMATTED`, `DELIMITER`

**Missing — use `is_identifier_text()` for:** `THROUGH`, `APPEND`, `CLOSE` (note: `CLOSE` exists only as `QUERY-CLOSE`, not standalone)

Alternatively, add these three to `keyword_overrides.toml` if cleaner dispatch is preferred. Since they're unambiguous statement-level keywords, either approach works.

### Tests (~15)

Section header: `// ===================== Stream I/O tests =====================`

1. `INPUT FROM "data.txt".`
2. `INPUT FROM "data.txt" NO-ECHO.` (skip NO-ECHO)
3. `OUTPUT TO "report.txt".`
4. `OUTPUT TO "report.txt" APPEND.`
5. `INPUT CLOSE.`
6. `OUTPUT CLOSE.`
7. `INPUT STREAM sIn FROM "data.txt".`
8. `OUTPUT STREAM sOut TO "report.txt".`
9. `INPUT-OUTPUT THROUGH "sort".`
10. `PUT UNFORMATTED "hello" SKIP.`
11. `PUT STREAM sOut UNFORMATTED x.`
12. `EXPORT Customer.Name Customer.Balance.`
13. `EXPORT DELIMITER "," x y z.`
14. `IMPORT cName cBalance.`
15. `IMPORT DELIMITER "," x y z.`

---

## Branch C: `feat/create-delete-release` — CREATE, DELETE, RELEASE, VALIDATE, OPEN QUERY, GET

**Why:** Database manipulation statements are core ABL. These are short, keyword-driven statements with simple structure.

### AST additions

```rust
/// CREATE record — creates a new database or temp-table record.
///
/// `CREATE Customer.`
/// `CREATE Customer NO-ERROR.`
Create {
    buffer: Identifier,
    no_error: bool,
},

/// DELETE record — deletes a database or temp-table record.
///
/// `DELETE Customer.`
/// `DELETE Customer VALIDATE(CustNum > 0, "Cannot delete").`
Delete {
    buffer: Identifier,
    validate: Option<(Expression, Expression)>,
    no_error: bool,
},

/// RELEASE record — releases a record lock.
///
/// `RELEASE Customer.`
/// `RELEASE Customer NO-ERROR.`
Release {
    buffer: Identifier,
    no_error: bool,
},

/// VALIDATE statement — validates a record.
///
/// `VALIDATE Customer.`
Validate {
    buffer: Identifier,
    no_error: bool,
},

/// OPEN QUERY — opens a query for iteration.
///
/// `OPEN QUERY q1 FOR EACH Customer NO-LOCK.`
OpenQuery {
    name: Identifier,
    /// The FOR EACH / PRESELECT clause (parse as raw expressions for now).
    for_each: Identifier,
    where_clause: Option<Expression>,
    lock_type: LockType,
},

/// GET FIRST/NEXT/PREV/LAST on a query.
///
/// `GET FIRST q1.`
/// `GET NEXT q1 NO-ERROR.`
GetRecord {
    direction: FindType,
    query: Identifier,
    no_error: bool,
},

/// CLOSE QUERY statement.
///
/// `CLOSE QUERY q1.`
CloseQuery {
    name: Identifier,
},

/// DEFINE QUERY statement.
///
/// `DEFINE QUERY q1 FOR Customer.`
DefineQuery {
    name: Identifier,
    buffers: Vec<Identifier>,
},
```

### Parser additions

1. Add dispatch for `Kind::Create`, `Kind::Delete`, `Kind::Release`, `Kind::Validate`, `Kind::Open`, `Kind::Get`, `Kind::Close`
2. `parse_create()`, `parse_delete()`, `parse_release_statement()`, `parse_validate_statement()`
3. `parse_open_query()` — consume OPEN QUERY name, then FOR EACH buffer, optional WHERE, lock type
4. `parse_get_record()` — consume GET FIRST/NEXT/PREV/LAST query-name
5. `parse_close_query()` — consume CLOSE QUERY name
6. Add QUERY handling to `parse_define_statement()` after BUFFER
7. Add all new kinds to `can_start_statement()`

### Lexer keywords needed

**Already exist as Kind variants:** `CREATE` (`Kind::Create`), `DELETE` (`Kind::Delete`), `RELEASE` (`Kind::Release`), `OPEN` (`Kind::Open`), `QUERY` (`Kind::Query`), `VALIDATE` (`Kind::Validate`)

**Missing:** `GET` and `CLOSE` as standalone keywords. `GET` likely exists as part of property syntax but needs verification. Use `is_identifier_text()` for both, or add via codegen.

### Tests (~15)

Section header: `// ===================== Database operations tests =====================`

1. `CREATE Customer.`
2. `CREATE Customer NO-ERROR.`
3. `DELETE Customer.`
4. `DELETE Customer NO-ERROR.`
5. `RELEASE Customer.`
6. `RELEASE Customer NO-ERROR.`
7. `VALIDATE Customer.`
8. `DEFINE QUERY q1 FOR Customer.`
9. `OPEN QUERY q1 FOR EACH Customer NO-LOCK.`
10. `OPEN QUERY q1 FOR EACH Customer WHERE CustNum > 10 NO-LOCK.`
11. `GET FIRST q1.`
12. `GET NEXT q1.`
13. `GET NEXT q1 NO-ERROR.`
14. `CLOSE QUERY q1.`
15. `DELETE Customer VALIDATE(CustNum > 0, "err").` (if supported)

---

## Branch D: `feat/doc-and-cleanup` — CLAUDE.md, plan statuses, README, minor fixes

**Why last:** Pure documentation, no code conflicts. Reflects the state after A+B+C have merged.

### Tasks

1. **Update CLAUDE.md** — The parser feature list is stale. Update the "Not yet implemented" section to reflect what A/B/C added, and what remains (frames, dataset, dynamic object creation, preprocessor statements, etc.)

2. **Update plan statuses:**
   - `2026-03-31-feat-parser-error-recovery-plan.md` → status: `completed`
   - `2026-03-31-feat-parser-completion-plan.md` → status: `completed` (all phases done)
   - `2026-03-31-parser-next-steps.md` → status: `completed` (superseded by this plan)
   - This plan → update status to `completed`

3. **Update README.md:**
   - Test count (will be ~278 + ~50 = ~328)
   - Feature list reflecting all statement types
   - Accurate parser status

4. **Fix duplicate RUN dispatch** in `statements.rs` — lines 98-101 and 108-111 both dispatch `Kind::Run`. Remove the duplicate.

5. **Add `Kind::Assign` and `Kind::Throw` to `can_start_statement()`** if not already there.

---

## Conflict Cheat Sheet

When rebasing branch N+1 onto new master after merging branch N:

| File | Expected conflict | Resolution |
|------|-------------------|------------|
| `statement.rs` (AST) | `Statement` enum — two branches add variants near the same spot | Add the new variants. Both go before `Leave`. |
| `statements.rs:parse_statement()` | Two branches add `if self.check(Kind::Foo)` blocks | Keep both blocks — they're independent dispatch arms. |
| `statements.rs:can_start_statement()` | Two branches add `Kind::Foo` to the match | Add both kinds. |
| `tests.rs` | Both branches append test sections at EOF | Keep both — they're separate test sections. |
| `keyword_overrides.toml` | Both branches may add keywords | Keep all additions. |

All conflicts are additive (both sides add non-overlapping content in the same region). No semantic conflicts expected.

## Post-merge

After all four branches merge:
- Run `cargo test` — expect ~330 tests passing
- Run `cargo clippy -D warnings` — no warnings
- Run `cargo doc --no-deps --workspace` — docs build
- The release-please PR will auto-bump versions

## Future (not in scope)

- Frames and frame phrases (WITH FRAME ... complex options)
- DATASET and DATA-SOURCE definitions
- Preprocessor statements (`&IF`, `&GLOBAL-DEFINE`, etc. — lexer handles these but parser doesn't)
- PUBLISH/SUBSCRIBE events
- Dynamic object creation (`CREATE widget-type`)
- ON triggers (`ON CHOOSE OF btn DO:`)
- APPLY statement
- Expression refactor: collapse 14 binary ops into `Binary(BinaryOp, Box, Box)`
- Span on Statement nodes
- Arena allocation
