---
title: "Parser Next Phase: OO-ABL Support"
type: feat
status: completed
date: 2026-04-04
revised: 2026-04-04
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

This follows the established pattern: lexer keywords land first so the parser dispatches on Kind variants (O(1) integer comparison), per CLAUDE.md.

---

## Branch 0: `feat/oo-abl-keywords`

Add all OO-ABL Kind variants. This is the only branch that touches generated files and `keyword_overrides.toml`.

### Keyword status

All OO-ABL keywords exist in `abl_keyword_index.html` but almost none generate Kind variants because they are **non-reserved** (the codegen only processes reserved keywords from the HTML index). Only `Kind::Set` and `Kind::Using` already exist.

None of these keywords have abbreviations — they must always be spelled in full.

### Keywords to add (`resources/keyword_overrides.toml`)

| Keyword | keyword_type | Already exists? | Notes |
|---------|-------------|----------------|-------|
| CLASS | Statement | No | Non-reserved in ABL |
| INTERFACE | Statement | No | Non-reserved in ABL |
| INHERITS | Option | No | |
| IMPLEMENTS | Option | No | |
| METHOD | Statement | No | Non-reserved in ABL |
| CONSTRUCTOR | Statement | No | Non-reserved in ABL |
| DESTRUCTOR | Statement | No | Non-reserved in ABL |
| ABSTRACT | Option | No | Non-reserved in ABL |
| FINAL | Option | No | Non-reserved in ABL |
| OVERRIDE | Option | No | Non-reserved in ABL |
| STATIC | Option | No | Non-reserved in ABL |
| PUBLIC | Option | No | Non-reserved in ABL |
| PRIVATE | Option | No | Non-reserved in ABL |
| PROTECTED | Option | No | Non-reserved in ABL |
| PACKAGE-PRIVATE | Option | No | Non-reserved, hyphenated |
| VOID | Type | No | Non-reserved in ABL |
| PROPERTY | Option | No | Used in DEFINE PROPERTY |
| GET | Option | No | Non-reserved; bare `GET` has no Kind variant (only compound forms like `GET-BYTE` exist) |

**Already exist (no action needed):** `Kind::Set` (reserved), `Kind::Using`

### Steps

1. Add entries to `keyword_overrides.toml` (18 additions)
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

Add supporting type first:

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
    /// Parent class name, if any. Uses Identifier to preserve span.
    inherits: Option<Identifier>,
    /// Implemented interfaces. Uses Identifier to preserve span.
    implements: Vec<Identifier>,
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
    /// Parameters (reuses DefineParameter Statement variant).
    parameters: Vec<Statement>,
    /// Method body (empty vec if abstract).
    body: Vec<Statement>,
},

/// DEFINE PROPERTY inside a CLASS.
///
/// ```abl
/// DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
///     GET.
///     SET.
/// ```
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
    /// GET accessor: None = no getter, Some(vec![]) = auto-getter (`GET.`),
    /// Some(body) = computed getter (`GET: body END GET.`).
    get_body: Option<Vec<Statement>>,
    /// SET accessor: None = no setter, Some(vec![]) = auto-setter (`SET.`),
    /// Some(body) = computed setter (`SET: body END SET.`).
    set_body: Option<Vec<Statement>>,
},

/// CONSTRUCTOR definition inside a CLASS.
///
/// `CONSTRUCTOR PUBLIC MyClass(INPUT x AS INTEGER): body END CONSTRUCTOR.`
Constructor {
    access: AccessModifier,
    /// Parameters (reuses DefineParameter Statement variant).
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
    /// Interfaces this interface inherits from. Uses Identifier to preserve span.
    inherits: Vec<Identifier>,
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

### Design decisions

**`inherits`/`implements` use `Identifier`, not `String`.** The original plan used `String` which discards source spans. Since this is new code, we use `Identifier` from the start. This matches the pattern issue already noted for `DataType::Class(String)` — no need to create the same tech debt again.

**Property GET/SET uses `Option<Vec<Statement>>` instead of `bool`.** This cleanly distinguishes three states:
- `None` — accessor not defined (read-only or write-only property)
- `Some(vec![])` — auto-accessor (`GET.` or `SET.`)
- `Some(body)` — computed accessor with body (`GET: ... END GET.`)

The parser detects auto vs computed by checking for colon vs period after GET/SET.

**`Using.type_name` stays as `String`.** USING paths like `MyApp.Services.*` with wildcards don't map cleanly to `Identifier` semantics. A plain string is sufficient.

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
   - If access modifier found, store it, then check next keyword:
     - `Kind::Property` → `parse_define_property(access)`
     - `Kind::Variable` → pass through to existing variable parsing (access modifier ignored for now — tracked in Future section)
     - `Kind::TempTable` → pass through (access modifier ignored for now)
     - Other → error

#### New parser methods

**Helper: `parse_access_modifier()`**

```rust
fn parse_access_modifier(&mut self) -> Option<AccessModifier> {
    match self.peek().kind {
        Kind::Public => { self.advance(); Some(AccessModifier::Public) }
        Kind::Private => { self.advance(); Some(AccessModifier::Private) }
        Kind::Protected => { self.advance(); Some(AccessModifier::Protected) }
        Kind::PackagePrivate => { self.advance(); Some(AccessModifier::PackagePrivate) }
        _ => None,
    }
}
```

**Helper: `parse_dotted_name()`**

CLASS names, USING paths, INHERITS/IMPLEMENTS references are all dot-separated identifiers (e.g., `MyApp.Services.CustomerService`). Returns a single `Identifier` whose `name` field is the joined dotted string and whose `span` covers start-to-end.

```rust
fn parse_dotted_name(&mut self) -> ParseResult<Identifier> {
    // consume first identifier
    let first = self.parse_identifier()?;
    let start = first.span.start;
    let mut name = first.name;

    while self.check(Kind::Period) {
        // Peek ahead: if next after period is an identifier, it's part of the dotted name
        if !Self::can_be_identifier(self.peek_at(1).kind)
            && !(self.peek_at(1).kind == Kind::Star) // for USING wildcards
        {
            break; // period is the statement terminator
        }
        self.advance(); // consume period
        name.push('.');
        let token = self.advance().clone();
        name.push_str(&self.source[token.start..token.end]);
    }

    let end = self.previous_span().end;
    Ok(Identifier {
        span: Span { start, end },
        name,
    })
}
```

**Helper: `parse_parenthesized_params()`**

METHOD and CONSTRUCTOR parameter lists use the same syntax as PROCEDURE parameters: `(INPUT x AS INTEGER, OUTPUT y AS CHARACTER)`. This helper extracts the shared logic. PROCEDURE parameters are separate `DEFINE PARAMETER` statements in the body, but METHOD/CONSTRUCTOR params use a parenthesized comma-separated list. The helper:

1. Consume `(`
2. Loop: parse direction keyword (`INPUT`/`OUTPUT`/`INPUT-OUTPUT`, default INPUT), name, `AS`, type, optional `NO-UNDO`
3. Each param becomes a `Statement::DefineParameter` (reusing the existing AST variant)
4. Consume `,` between params
5. Consume `)`

```rust
fn parse_parenthesized_params(&mut self) -> ParseResult<Vec<Statement>> {
    self.expect_kind(Kind::LeftParen, "Expected '(' for parameter list")?;
    let mut params = Vec::new();

    if !self.check(Kind::RightParen) {
        loop {
            let direction = match self.peek().kind {
                Kind::Output => { self.advance(); ParameterDirection::Output }
                Kind::InputOutput => { self.advance(); ParameterDirection::InputOutput }
                Kind::Input => { self.advance(); ParameterDirection::Input }
                _ => ParameterDirection::Input,
            };

            let name = self.parse_identifier()?;
            self.expect_kind(Kind::KwAs, "Expected AS after parameter name")?;
            let data_type = self.parse_data_type()?;
            let no_undo = if self.check(Kind::NoUndo) { self.advance(); true } else { false };

            params.push(Statement::DefineParameter {
                direction,
                name,
                data_type,
                no_undo,
            });

            if !self.check(Kind::Comma) {
                break;
            }
            self.advance(); // consume comma
        }
    }

    self.expect_kind(Kind::RightParen, "Expected ')'")?;
    Ok(params)
}
```

**`parse_class()`**

```
CLASS [ABSTRACT] [FINAL] dotted-name [INHERITS dotted-name] [IMPLEMENTS dotted-name [, dotted-name ...]]:
    body
END CLASS.
```

Parse flow:
1. Consume CLASS
2. Check for ABSTRACT, FINAL flags (before name, in any order)
3. Parse dotted name
4. Optional INHERITS → parse dotted name
5. Optional IMPLEMENTS → parse comma-separated dotted names
6. Expect colon
7. Parse body until END CLASS
8. Expect period

**`parse_interface()`**

Same structure as class but simpler — INTERFACE keyword, optional INHERITS (can inherit multiple interfaces), body contains method signatures only (no implementations).

**`parse_method()`**

```
METHOD [access] [STATIC] [ABSTRACT] [OVERRIDE] (VOID | return-type) name (params):
    body
END METHOD.
```

Parse flow:
1. Consume METHOD
2. Parse access modifier (defaults to PUBLIC if absent)
3. Check for STATIC, ABSTRACT, OVERRIDE flags (in any order)
4. Parse return type: VOID → `None`, else `Some(parse_data_type())`
5. Parse method name
6. Parse parenthesized params via `parse_parenthesized_params()`
7. If abstract: expect period, body = empty vec
8. Else: expect colon, parse body until END METHOD, expect period

**`parse_define_property(access)`**

```
DEFINE [access] [STATIC] PROPERTY name AS type [NO-UNDO]
    GET.                    -- auto-getter
    GET: body END GET.      -- computed getter
    SET.                    -- auto-setter
    SET: body END SET.      -- computed setter
```

Parse flow:
1. (DEFINE and access already consumed by `parse_define_statement()`)
2. Check for STATIC flag
3. Consume PROPERTY
4. Parse name, AS, data type
5. Optional NO-UNDO
6. Parse GET accessor:
   - If `Kind::Get`: advance, check next token
     - If `Kind::Period`: auto-getter → `Some(vec![])`
     - If `Kind::Colon`: computed getter → parse body until `END GET`, expect period → `Some(body)`
   - If no GET: `None`
7. Parse SET accessor (same logic as GET)
8. Expect period (statement terminator)

**Key disambiguation:** After consuming `GET` or `SET`, check for **colon** (block body) vs **period** (auto-accessor). This is the critical parsing decision.

**`parse_constructor()`**

```
CONSTRUCTOR [access] class-name (params):
    body
END CONSTRUCTOR.
```

Parse flow:
1. Consume CONSTRUCTOR
2. Parse access modifier (defaults to PUBLIC)
3. Skip class name (advance past identifier — constructor name must match class, validated semantically not syntactically)
4. Parse parenthesized params
5. Expect colon, parse body until END CONSTRUCTOR, expect period

**`parse_destructor()`**

```
DESTRUCTOR [PUBLIC] class-name ():
    body
END DESTRUCTOR.
```

Parse flow:
1. Consume DESTRUCTOR
2. Skip optional PUBLIC
3. Skip class name
4. Expect `(` `)` (no params allowed)
5. Expect colon, parse body until END DESTRUCTOR, expect period

**`parse_using()`**

```
USING dotted-name[.*].
```

Parse flow:
1. Consume USING
2. Build type_name string by consuming identifier + `.` tokens, including `.*` wildcard
3. Expect period

### Disambiguation notes

- **CLASS as data type vs statement:** `AS CLASS ClassName` already works via `parse_data_type()` which checks for the `CLASS` keyword mid-statement. At statement level, `Kind::Class` at the start of a statement is always a CLASS definition. No conflict.
- **GET/SET as property accessors vs other uses:** GET and SET only appear as property accessors inside DEFINE PROPERTY. They're parsed explicitly after the property type, not dispatched at statement level.
- **METHOD/CONSTRUCTOR/DESTRUCTOR at statement level:** These only appear inside CLASS bodies. If encountered at top level, they'll parse and produce an AST node — semantic validation (class-only context) is a future concern, not a parser concern.
- **DEFINE with access modifiers:** Currently `parse_define_statement()` expects VARIABLE/TEMP-TABLE/BUFFER/param-direction immediately after DEFINE. In OO-ABL class bodies, `DEFINE PUBLIC PROPERTY ...` has an access modifier between DEFINE and the member type. The parser checks for access modifier keywords after DEFINE and routes accordingly.

### Tests (~22)

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
12. DEFINE PUBLIC PROPERTY with inline GET. SET. (auto-accessors)
13. DEFINE PROPERTY with computed GET: / SET: blocks
14. DEFINE read-only PROPERTY (GET only, no SET)
15. Constructor with params
16. Destructor
17. Interface with method signatures
18. Interface with INHERITS
19. USING with qualified name
20. USING with wildcard (`USING MyApp.Services.*`)
21. Full class with mixed members (method + property + variable + constructor)
22. DEFINE PUBLIC VARIABLE inside class body (access modifier passthrough)

---

## Post-merge

After both branches merge:
- `cargo test` — expect ~309 tests passing (287 + ~22)
- `cargo clippy -D warnings` — no warnings
- Update CLAUDE.md parser feature list to include OO-ABL
- Update test count in CLAUDE.md

## Future (not in scope)

These are explicitly deferred:

- **SERIALIZABLE classes** — `CLASS MyClass SERIALIZABLE:` — uncommon but used for session serialization
- **ENUM classes** — `ENUM MyEnum: DEFINE ENUM val1 val2. END ENUM.` — modern ABL feature
- **DEFINE [access] VARIABLE inside class bodies** — access modifiers on non-property class members need `parse_define_statement()` to produce a variant that carries the access modifier. Deferred until the AST has a `ClassMember` wrapper or `VariableDeclaration` gains an `access` field.
- **DEFINE [access] TEMP-TABLE inside class bodies** — same issue as above
- **Property accessor parameters** — SET accessor can take a parameter: `SET(INPUT val AS CHARACTER): ... END SET.`
- **Stream I/O** (INPUT/OUTPUT/CLOSE, PUT, EXPORT, IMPORT)
- **Database manipulation** (CREATE, DELETE, RELEASE, OPEN/CLOSE/GET QUERY)
- **Frames and frame phrases**
- **DATASET and DATA-SOURCE definitions**
- **Preprocessor statements** (`&IF`, `&GLOBAL-DEFINE`)
- **PUBLISH/SUBSCRIBE events**
- **Dynamic object creation** (`CREATE widget-type`)
- **ON triggers** (`ON CHOOSE OF btn DO:`)
- **APPLY statement**
- **Expression refactor:** collapse binary ops into `Binary(BinaryOp, Box, Box)`
- **Span on Statement nodes**
- **Arena allocation**
- **`DataType::Class(String)` → `DataType::Class(Identifier)`** — preserve span (noted in parser-completion-plan)
