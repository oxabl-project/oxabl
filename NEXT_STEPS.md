# Oxabl Parser - Next Steps

## Current Progress Summary

The parser has a solid foundation covering:

### Expressions (Complete)
- **Operator precedence** properly implemented via recursive descent:
  - Ternary (`IF...THEN...ELSE` expressions)
  - Logical (`OR`, `AND`, `NOT`)
  - Comparison (`=`, `<>`, `<`, `<=`, `>`, `>=`, `EQ`, `NE`, `LT`, `LE`, `GT`, `GE`, `BEGINS`, `MATCHES`, `CONTAINS`)
  - Arithmetic (`+`, `-`, `*`, `/`, `%`)
  - Unary (`-`, `NOT`)
  - Postfix (member `:`, method call `:xxx()`, array `[]`, field `.`)
  - Primary (literals, identifiers, function calls, parentheses)

### Statements (Good Foundation)
- Variable declarations (`DEFINE VARIABLE`, `VAR`)
- Assignments
- Expression statements (function/method calls as statements)
- `DO` blocks (simple, counting loops with `TO`/`BY`, `WHILE`)
- `IF`/`THEN`/`ELSE`
- `REPEAT`
- `LEAVE`, `NEXT`, `RETURN`
- Empty statements

### Tests
- 80+ tests covering expression parsing and statement parsing

---

## Immediate Fixes Needed

### 1. Remove Debug Print Statements

The parser has `println!` calls that should be removed for production:

```rust
// parser.rs:103, 126, 129, 134, 136, 365, 366, 449, 508, 513, 762, 765, 779, 784, 792, 798
```

Consider using the `log` crate with `debug!` macro instead, which can be disabled at compile time.

### 2. Complete `make_comparison` Function

The `make_comparison` function at line 728 is incomplete - it only handles two operators:

```rust
fn make_comparison(&self, left: Expression, op: Kind, right: Expression) -> Expression {
    match op {
        Kind::NotEqual | Kind::Ne => Expression::NotEqual(Box::new(left), Box::new(right)),
        Kind::LessThan | Kind::Lt => Expression::LessThan(Box::new(left), Box::new(right)),
        // Missing: LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Begins, Matches, Contains
        _ => unreachable!(),
    }
}
```

Add the missing operators to match `is_non_equals_comparison_operator()`.

---

## Next Features by Priority

### Priority 1: FOR EACH Statement (Database Queries)

This is the most important ABL feature for real-world code. ABL's `FOR EACH` is how you query and iterate over database records.

**Syntax:**
```abl
FOR EACH Customer WHERE Customer.Balance > 1000 NO-LOCK:
    DISPLAY Customer.Name Customer.Balance.
END.

FOR EACH Order OF Customer NO-LOCK,
    EACH OrderLine OF Order NO-LOCK:
    total = total + OrderLine.Price.
END.
```

**Concept:** `FOR EACH` combines a database query with iteration. It's similar to SQL's `SELECT` wrapped in a `foreach` loop.

**AST Addition:**
```rust
// In statement.rs
ForEach {
    /// The record buffer being queried
    buffer: Identifier,
    /// Optional join to parent record (OF clause)
    of_relation: Option<Identifier>,
    /// WHERE clause filter
    where_clause: Option<Expression>,
    /// Lock type: NO-LOCK, SHARE-LOCK, EXCLUSIVE-LOCK
    lock_type: LockType,
    /// Body statements
    body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LockType {
    NoLock,
    ShareLock,
    ExclusiveLock,
}
```

**Parser Implementation:**
```rust
fn parse_for_each(&mut self) -> ParseResult<Statement> {
    self.advance(); // consume FOR
    self.expect_kind(Kind::Each, "Expected EACH after FOR")?;

    // Parse buffer name
    let buffer = self.parse_identifier()?;

    // Optional OF clause
    let of_relation = if self.check(Kind::Of) {
        self.advance();
        Some(self.parse_identifier()?)
    } else {
        None
    };

    // Optional WHERE clause
    let where_clause = if self.check(Kind::Where) {
        self.advance();
        Some(self.parse_expression()?)
    } else {
        None
    };

    // Lock type (default NO-LOCK for queries)
    let lock_type = self.parse_lock_type()?;

    self.expect_kind(Kind::Colon, "Expected ':' after FOR EACH")?;
    let body = self.parse_block_body()?;

    Ok(Statement::ForEach { buffer, of_relation, where_clause, lock_type, body })
}
```

**Tests to Write:**
```rust
#[test]
fn parse_simple_for_each() {
    let source = "FOR EACH Customer NO-LOCK: x = Customer.Name. END.";
    // ...
}

#[test]
fn parse_for_each_with_where() {
    let source = "FOR EACH Customer WHERE Customer.Balance > 1000 NO-LOCK: END.";
    // ...
}

#[test]
fn parse_for_each_with_of() {
    let source = "FOR EACH Order OF Customer NO-LOCK: END.";
    // ...
}
```

---

### Priority 2: FIND Statement (Single Record Lookup)

**Syntax:**
```abl
FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.
FIND Customer 1 NO-LOCK.
```

**Concept:** `FIND` retrieves a single record into a buffer. Unlike `FOR EACH`, it doesn't iterate - it either finds one record or fails.

**AST Addition:**
```rust
Find {
    find_type: FindType,  // FIRST, LAST, NEXT, PREV, or none (unique)
    buffer: Identifier,
    where_clause: Option<Expression>,
    lock_type: LockType,
    no_error: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FindType {
    First,
    Last,
    Next,
    Prev,
    Unique,  // No qualifier = expects exactly one match
}
```

---

### Priority 3: CASE Statement

**Syntax:**
```abl
CASE CustomerType:
    WHEN "retail" THEN
        discount = 0.1.
    WHEN "wholesale" THEN
        discount = 0.2.
    OTHERWISE
        discount = 0.
END CASE.
```

**Concept:** ABL's `CASE` is similar to `switch` in other languages. The `WHEN` clauses test equality against the case expression.

**AST Addition:**
```rust
Case {
    expression: Expression,
    when_branches: Vec<(Expression, Vec<Statement>)>,
    otherwise: Option<Vec<Statement>>,
}
```

**Parser Implementation:**
```rust
fn parse_case_statement(&mut self) -> ParseResult<Statement> {
    self.advance(); // consume CASE
    let expression = self.parse_expression()?;
    self.expect_kind(Kind::Colon, "Expected ':' after CASE expression")?;

    let mut when_branches = Vec::new();

    while self.check(Kind::When) {
        self.advance();
        let value = self.parse_expression()?;
        self.expect_kind(Kind::Then, "Expected THEN after WHEN value")?;

        // Parse statements until next WHEN, OTHERWISE, or END
        let mut body = Vec::new();
        while !self.check(Kind::When)
            && !self.check(Kind::Otherwise)
            && !self.check(Kind::End)
        {
            body.push(self.parse_statement()?);
        }
        when_branches.push((value, body));
    }

    let otherwise = if self.check(Kind::Otherwise) {
        self.advance();
        let mut body = Vec::new();
        while !self.check(Kind::End) {
            body.push(self.parse_statement()?);
        }
        Some(body)
    } else {
        None
    };

    self.expect_kind(Kind::End, "Expected END")?;
    self.expect_kind(Kind::Case, "Expected CASE after END")?;
    self.expect_kind(Kind::Period, "Expected '.' after END CASE")?;

    Ok(Statement::Case { expression, when_branches, otherwise })
}
```

---

### Priority 4: Procedure Definitions

**Syntax:**
```abl
PROCEDURE calculate-total:
    DEFINE INPUT PARAMETER pPrice AS DECIMAL.
    DEFINE INPUT PARAMETER pQty AS INTEGER.
    DEFINE OUTPUT PARAMETER pTotal AS DECIMAL.

    pTotal = pPrice * pQty.
END PROCEDURE.
```

**Concept:** Procedures are ABL's basic unit of reusable code. They can have INPUT, OUTPUT, and INPUT-OUTPUT parameters.

**AST Addition:**
```rust
Procedure {
    name: Identifier,
    parameters: Vec<Parameter>,
    body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub direction: ParameterDirection,
    pub name: Identifier,
    pub data_type: DataType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParameterDirection {
    Input,
    Output,
    InputOutput,
}
```

---

### Priority 5: DISPLAY / MESSAGE Statements

**Syntax:**
```abl
DISPLAY Customer.Name Customer.Balance WITH FRAME f1.
MESSAGE "Hello, " + userName VIEW-AS ALERT-BOX.
```

**Concept:** These are ABL's output statements. `DISPLAY` shows data in a frame (UI grid), while `MESSAGE` shows alert dialogs or console output.

---

### Priority 6: RUN Statement (Calling Procedures)

**Syntax:**
```abl
RUN calculate-total (INPUT 100, INPUT 5, OUTPUT result).
RUN external-prog.p (INPUT "data").
```

**Concept:** `RUN` executes an internal procedure or external `.p` file, passing parameters.

---

## Medium-Term Features

### Object-Oriented ABL (OO-ABL)

```abl
CLASS MyClass:
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
        GET.
        SET.

    METHOD PUBLIC VOID DoSomething():
        /* ... */
    END METHOD.
END CLASS.
```

This requires:
- `CLASS` definition parsing
- `METHOD` definition parsing
- `PROPERTY` definitions with `GET`/`SET`
- Access modifiers (`PUBLIC`, `PRIVATE`, `PROTECTED`)
- `CONSTRUCTOR` and `DESTRUCTOR`
- `INTERFACE` definitions
- `INHERITS` and `IMPLEMENTS` clauses

### Error Handling

```abl
DO ON ERROR UNDO, THROW:
    /* risky code */
    CATCH e AS Progress.Lang.Error:
        MESSAGE e:GetMessage(1).
    END CATCH.
    FINALLY:
        cleanup().
    END FINALLY.
END.
```

### Temp-Table and Buffer Definitions

```abl
DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustNum AS INTEGER
    FIELD Name AS CHARACTER
    INDEX idx1 CustNum.

DEFINE BUFFER bCust FOR Customer.
```

---

## Architecture Recommendations

### 1. Add Helper Method for Identifier Parsing

Currently identifier parsing is scattered. Create a reusable helper:

```rust
fn parse_identifier(&mut self) -> ParseResult<Identifier> {
    if !is_callable_kind(self.peek().kind) {
        return Err(ParseError {
            message: "Expected identifier".to_string(),
            span: self.current_span(),
        });
    }
    let token = self.advance().clone();
    Ok(Identifier {
        span: Span {
            start: token.start as u32,
            end: token.end as u32,
        },
        name: self.source[token.start..token.end].to_string(),
    })
}

fn current_span(&self) -> Span {
    Span {
        start: self.peek().start as u32,
        end: self.peek().end as u32,
    }
}
```

### 2. Consider a Statement Dispatcher

As statements grow, consider a cleaner dispatcher:

```rust
pub fn parse_statement(&mut self) -> ParseResult<Statement> {
    match self.peek().kind {
        Kind::Period => { self.advance(); Ok(Statement::Empty) }
        Kind::Do => self.parse_do_statement(),
        Kind::KwIf => self.parse_if_statement(),
        Kind::Repeat => self.parse_repeat_statement(),
        Kind::For => self.parse_for_each(),
        Kind::Find => self.parse_find_statement(),
        Kind::Case => self.parse_case_statement(),
        Kind::Procedure => self.parse_procedure(),
        Kind::Leave => { self.advance(); self.expect_period()?; Ok(Statement::Leave) }
        Kind::Next => { self.advance(); self.expect_period()?; Ok(Statement::Next) }
        Kind::KwReturn => self.parse_return_statement(),
        Kind::Define => self.parse_define_statement(),
        _ => self.parse_assignment_or_expression(),
    }
}
```

### 3. Error Recovery

For a linter/formatter, you'll want error recovery to continue parsing after errors:

```rust
fn synchronize(&mut self) {
    // Skip tokens until we find a statement boundary
    while !self.at_end() {
        if self.peek().kind == Kind::Period {
            self.advance();
            return;
        }
        // Also sync on statement-starting keywords
        if matches!(self.peek().kind,
            Kind::Do | Kind::KwIf | Kind::For | Kind::Define | Kind::End
        ) {
            return;
        }
        self.advance();
    }
}
```

---

## Test Coverage Gaps

Add tests for:

1. **Edge cases in loops:**
   - `DO i = 10 TO 1 BY -1` (counting down)
   - `DO WHILE TRUE` without counting variable

2. **Complex nested structures:**
   - `IF` inside `FOR EACH` inside `PROCEDURE`

3. **Error cases:**
   - Missing `END.`
   - Missing `:` after block headers
   - Unmatched parentheses

4. **ABL-specific quirks:**
   - Keyword abbreviations in expressions
   - Case-insensitive comparisons

---

## Summary Checklist

- [ ] Remove debug `println!` statements
- [ ] Complete `make_comparison()` function
- [ ] Add `parse_identifier()` helper
- [ ] Implement `FOR EACH` statement
- [ ] Implement `FIND` statement
- [ ] Implement `CASE` statement
- [ ] Implement `PROCEDURE` definition
- [ ] Add tests for each new feature
- [ ] Consider error recovery strategy
