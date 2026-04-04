---
title: "Parser Next Phase: OO-ABL Support"
type: feat
status: active
date: 2026-04-04
---

# Parser Next Phase: OO-ABL Support

## Current State

**Version:** parser 0.4.0, ast 0.4.0, 239 parser tests, 287 total
**Base commit:** current master

The parser handles: DEFINE VARIABLE/VAR/PARAMETER/TEMP-TABLE/BUFFER, DO/REPEAT/FOR EACH, IF/THEN/ELSE, CASE, FIND, PROCEDURE, FUNCTION, RUN, DISPLAY, MESSAGE, ASSIGN, CATCH/FINALLY/THROW, and error recovery via `parse_program()`.

## Goal

Add OO-ABL support to the parser — CLASS, INTERFACE, METHOD, PROPERTY, CONSTRUCTOR, DESTRUCTOR, and USING. OO-ABL is the most structurally complex ABL feature set not yet supported and is required for any modern ABL codebase.

## Branch Architecture

Two sequential branches:

1. **Branch 0: `feat/oo-abl-keywords`** — Add Kind variants for all OO-ABL keywords via `keyword_overrides.toml` + codegen. No parser changes.
2. **Branch A: `feat/oo-abl`** — Parser and AST changes, forked from master after Branch 0 merges.

This follows the established pattern: lexer keywords land first so parser branches dispatch on Kind variants (O(1) integer comparison), per CLAUDE.md.

---

## Branch 0: `feat/oo-abl-keywords`

Add all OO-ABL Kind variants. This is the only branch that touches generated files and `keyword_overrides.toml`.

### Keywords to add (`resources/keyword_overrides.toml`)

| Keyword | keyword_type | min_abbreviation | Notes |
|---------|-------------|-----------------|-------|
| CLASS | Statement | | |
| INTERFACE | Statement | | |
| INHERITS | Option | | |
| IMPLEMENTS | Option | | |
| METHOD | Statement | | |
| CONSTRUCTOR | Statement | | Verify abbreviation in ABL docs |
| DESTRUCTOR | Statement | | Verify abbreviation in ABL docs |
| ABSTRACT | Option | | |
| FINAL | Option | | |
| OVERRIDE | Option | | Verify abbreviation in ABL docs |
| STATIC | Option | | |
| PUBLIC | Option | | |
| PRIVATE | Option | | |
| PROTECTED | Option | | |
| PACKAGE-PRIVATE | Option | | |
| VOID | Type | | |
| PROPERTY | Option | | Used in DEFINE PROPERTY |
| GET | Option | | For property GET accessor |
| SET | Option | | For property SET accessor |

**Already exist (no action needed):** `Kind::Using`, `Kind::Finally`

### Steps

1. Add entries to `keyword_overrides.toml`
2. `cargo run -p oxabl_codegen` to regenerate `kind.rs` and `build.rs`
3. `cargo check -p oxabl_lexer` — verify compilation
4. `cargo test` — all 287 tests pass (additive Kind variants, no parser changes)

### Validation

- `cargo test` — all existing tests pass
- `cargo check --workspace` — no warnings

---

## Branch A: `feat/oo-abl`

Forks from master after Branch 0 merges.

### AST additions (`crates/oxabl_ast/src/statement.rs`)

Add these variants to `Statement`, **before `Leave`**:

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
    /// Return type (None = VOID).
    return_type: Option<DataType>,
    /// Method name.
    name: Identifier,
    /// Parameters (parsed as DefineParameter statements).
    parameters: Vec<Statement>,
    /// Method body (empty vec if abstract).
    body: Vec<Statement>,
},

/// DEFINE PROPERTY inside a CLASS.
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
    /// Whether GET is defined (true for `GET.` or `GET: body END GET.`).
    has_get: bool,
    /// Whether SET is defined (true for `SET.` or `SET: body END SET.`).
    has_set: bool,
},

/// CONSTRUCTOR definition inside a CLASS.
///
/// `CONSTRUCTOR PUBLIC MyClass(INPUT x AS INTEGER): body END CONSTRUCTOR.`
Constructor {
    access: AccessModifier,
    /// Parameters (parsed as DefineParameter statements).
    parameters: Vec<Statement>,
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

Add supporting type:

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

#### Dispatch changes

1. Add to `can_start_statement()`: `Kind::Class`, `Kind::Interface`, `Kind::Using`, `Kind::Method`, `Kind::Constructor`, `Kind::Destructor`
2. Add dispatch in `parse_statement()`:
   - `Kind::Class` → `parse_class()`
   - `Kind::Interface` → `parse_interface()`
   - `Kind::Using` → `parse_using()`
   - `Kind::Method` → `parse_method()`
   - `Kind::Constructor` → `parse_constructor()`
   - `Kind::Destructor` → `parse_destructor()`
3. Extend `parse_define_statement()` to handle DEFINE PROPERTY:
   - After consuming DEFINE, check for access modifier (`Kind::Public`/`Kind::Private`/`Kind::Protected`/`Kind::PackagePrivate`)
   - If access modifier found, check for `Kind::Property` → `parse_define_property()`
   - Also allow `DEFINE [access] VARIABLE` and `DEFINE [access] TEMP-TABLE` for OO-ABL class bodies

#### New parser methods

- **`parse_class()`** — consume CLASS, parse dotted name, optional INHERITS name, optional IMPLEMENTS name1, name2, ..., colon, body until END CLASS, period
- **`parse_interface()`** — same structure as class but INTERFACE keyword, body contains method signatures only
- **`parse_method()`** — consume METHOD, parse access modifier, optional STATIC/ABSTRACT/OVERRIDE, return type (VOID or DataType), name, parenthesized parameter list, colon, body until END METHOD, period. Abstract methods have no body — just period after params.
- **`parse_define_property()`** — after DEFINE [access] PROPERTY, parse name AS type [NO-UNDO], then GET accessor and SET accessor. Accessors are either inline (`GET.`) or have bodies (`GET: body END GET.`)
- **`parse_constructor()`** — consume CONSTRUCTOR, access modifier, name (class name, ignored), parenthesized params, colon, body until END CONSTRUCTOR, period
- **`parse_destructor()`** — consume DESTRUCTOR, PUBLIC (ignored), name (class name, ignored), colon, body until END DESTRUCTOR, period
- **`parse_using()`** — consume USING, parse dotted name (segments joined by `.`, may end in `.*`), period
- **`parse_access_modifier()`** — helper returning `Option<AccessModifier>`, consuming `Kind::Public`/`Kind::Private`/`Kind::Protected`/`Kind::PackagePrivate`

#### Dotted name parsing

CLASS names, USING paths, INHERITS/IMPLEMENTS references are all dot-separated identifiers (e.g., `MyApp.Services.CustomerService`). Add a helper `parse_dotted_name() -> String` that joins identifier tokens separated by `Kind::Dot`.

#### Parameter list reuse

METHOD and CONSTRUCTOR parameter lists use the same syntax as PROCEDURE parameters: `(INPUT x AS INTEGER, OUTPUT y AS CHARACTER)`. Reuse existing `parse_define_parameter()` logic. Parse the param list by consuming `(`, then looping: parse direction + name + AS + type until `)`.

### Disambiguation notes

- **CLASS as data type vs statement:** `AS CLASS ClassName` already works via `parse_data_type()` which checks for the `CLASS` keyword. At statement level, `Kind::Class` at the start of a statement is always a CLASS definition. No conflict — `parse_data_type()` is only called mid-statement.
- **GET/SET as property accessors vs other uses:** GET and SET only appear as property accessors inside DEFINE PROPERTY. They're parsed explicitly after the property type, not dispatched at statement level.
- **METHOD/CONSTRUCTOR/DESTRUCTOR at statement level:** These only appear inside CLASS bodies. If encountered at top level, they'll parse and produce an AST node — semantic validation (class-only context) is a future concern, not a parser concern.

### Tests (~20)

Append section `// ===================== CLASS/OO-ABL tests =====================` at end of tests.rs:

1. Simple class with empty body
2. Class with INHERITS
3. Class with IMPLEMENTS (single interface)
4. Class with INHERITS + IMPLEMENTS (multiple interfaces)
5. ABSTRACT class
6. FINAL class
7. Method with PUBLIC VOID, no params
8. Method with PRIVATE, returns INTEGER, with params
9. STATIC method
10. ABSTRACT method (no body, just period)
11. OVERRIDE method
12. DEFINE PUBLIC PROPERTY with inline GET. SET.
13. DEFINE PROPERTY with body GET: / SET: blocks
14. Constructor with params
15. Destructor
16. Interface with method signatures
17. Interface with INHERITS
18. USING with qualified name
19. USING with wildcard (`USING MyApp.Services.*`)
20. Full class with mixed members (method + property + variable + constructor)

---

## Post-merge

After both branches merge:
- `cargo test` — expect ~307 tests passing (287 + ~20)
- `cargo clippy -D warnings` — no warnings
- Update CLAUDE.md parser feature list to include OO-ABL
- Update test count in CLAUDE.md

## Future (not in scope)

- Stream I/O (INPUT/OUTPUT/CLOSE, PUT, EXPORT, IMPORT)
- Database manipulation (CREATE, DELETE, RELEASE, OPEN/CLOSE/GET QUERY)
- Frames and frame phrases
- DATASET and DATA-SOURCE definitions
- Preprocessor statements (`&IF`, `&GLOBAL-DEFINE`)
- PUBLISH/SUBSCRIBE events
- Dynamic object creation (`CREATE widget-type`)
- ON triggers (`ON CHOOSE OF btn DO:`)
- APPLY statement
- DEFINE [access] VARIABLE/TEMP-TABLE inside class bodies (access modifiers on non-property members)
- Expression refactor: collapse binary ops into `Binary(BinaryOp, Box, Box)`
- Span on Statement nodes
- Arena allocation
