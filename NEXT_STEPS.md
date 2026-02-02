# Oxabl Parser - Next Steps

## Priority 4: RUN Statement (Calling Procedures)

**Status:** AST not defined, parser not implemented

**Syntax:**
```abl
RUN calculate-total (INPUT 100, INPUT 5, OUTPUT result).
RUN calculate-total.
RUN external-prog.p (INPUT "data").
RUN VALUE(procName).
```

**Concept:** `RUN` executes an internal procedure or external `.p` file, passing parameters. The procedure name can be a literal, identifier, or `VALUE(expression)` for dynamic dispatch.

**AST Addition:**
```rust
Run {
    target: RunTarget,
    arguments: Vec<RunArgument>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RunTarget {
    Literal(String),           // RUN my-proc or RUN "file.p"
    Dynamic(Expression),       // RUN VALUE(expr)
}

#[derive(Debug, Clone, PartialEq)]
pub struct RunArgument {
    pub direction: ParameterDirection,
    pub expression: Expression,
}
```

**Parser Implementation:**
```rust
fn parse_run_statement(&mut self) -> ParseResult<Statement> {
    self.advance(); // consume RUN

    // Parse target: VALUE(expr) or procedure-name
    let target = if self.check(Kind::Value) {
        self.advance();
        self.expect_kind(Kind::LeftParen, "Expected '(' after VALUE")?;
        let expr = self.parse_expression()?;
        self.expect_kind(Kind::RightParen, "Expected ')'")?;
        RunTarget::Dynamic(expr)
    } else {
        // Procedure name (may contain hyphens, dots for .p files)
        let name = self.parse_procedure_name()?;
        RunTarget::Literal(name)
    };

    // Parse optional arguments
    let arguments = if self.check(Kind::LeftParen) {
        self.advance();
        let mut args = Vec::new();

        if !self.check(Kind::RightParen) {
            loop {
                let direction = match self.peek().kind {
                    Kind::Input => { self.advance(); ParameterDirection::Input }
                    Kind::Output => { self.advance(); ParameterDirection::Output }
                    Kind::InputOutput => { self.advance(); ParameterDirection::InputOutput }
                    _ => ParameterDirection::Input, // Default to INPUT
                };

                let expression = self.parse_expression()?;
                args.push(RunArgument { direction, expression });

                if !self.check(Kind::Comma) {
                    break;
                }
                self.advance(); // consume comma
            }
        }

        self.expect_kind(Kind::RightParen, "Expected ')'")?;
        args
    } else {
        Vec::new()
    };

    self.expect_kind(Kind::Period, "Expected '.' after RUN statement")?;

    Ok(Statement::Run { target, arguments })
}

// Helper to parse procedure names that may include dots (e.g., "file.p")
fn parse_procedure_name(&mut self) -> ParseResult<String> {
    let start = self.peek().start;
    self.advance(); // consume first identifier

    // Handle dotted names like "myproc.p"
    while self.check(Kind::Period) {
        // Peek ahead - if next is an identifier, it's part of the name
        let saved = self.position;
        self.advance(); // consume period
        if self.check_identifier() {
            self.advance(); // consume extension
        } else {
            // It's the statement terminator, rollback
            self.position = saved;
            break;
        }
    }

    let end = self.previous().end;
    Ok(self.source[start..end].to_string())
}
```

---

## Priority 5: DISPLAY Statement

**Status:** AST not defined, parser not implemented

**Syntax:**
```abl
DISPLAY Customer.Name Customer.Balance.
DISPLAY "Total:" total WITH FRAME f1.
DISPLAY x y z WITH FRAME results 2 COLUMNS.
DISPLAY Customer EXCEPT CustNum WITH FRAME cust-frame.
```

**Concept:** `DISPLAY` outputs field/variable values to the screen or a frame. It's one of ABL's primary output statements for UI applications.

**AST Addition:**
```rust
Display {
    items: Vec<DisplayItem>,
    frame: Option<FrameClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DisplayItem {
    pub expression: Expression,
    pub label: Option<String>,       // Optional "label" format
    pub format: Option<String>,      // Optional FORMAT "xxx"
}

#[derive(Debug, Clone, PartialEq)]
pub struct FrameClause {
    pub name: Identifier,
    pub columns: Option<u32>,        // N COLUMNS
    pub down: Option<u32>,           // N DOWN
    pub except: Vec<Identifier>,     // EXCEPT field-list
}
```

**Parser Implementation:**
```rust
fn parse_display_statement(&mut self) -> ParseResult<Statement> {
    self.advance(); // consume DISPLAY

    let mut items = Vec::new();

    // Parse display items until WITH or period
    while !self.check(Kind::With) && !self.check(Kind::Period) {
        let expression = self.parse_expression()?;

        // Optional label
        let label = if self.check(Kind::StringLiteral) {
            let token = self.advance().clone();
            Some(self.source[token.start+1..token.end-1].to_string())
        } else {
            None
        };

        // Optional FORMAT
        let format = if self.check(Kind::Format) {
            self.advance();
            let token = self.expect_kind(Kind::StringLiteral, "Expected format string")?;
            Some(self.source[token.start+1..token.end-1].to_string())
        } else {
            None
        };

        items.push(DisplayItem { expression, label, format });
    }

    // Parse optional WITH FRAME clause
    let frame = if self.check(Kind::With) {
        self.advance();
        Some(self.parse_frame_clause()?)
    } else {
        None
    };

    self.expect_kind(Kind::Period, "Expected '.' after DISPLAY")?;

    Ok(Statement::Display { items, frame })
}

fn parse_frame_clause(&mut self) -> ParseResult<FrameClause> {
    self.expect_kind(Kind::Frame, "Expected FRAME")?;
    let name = self.parse_identifier()?;

    let mut columns = None;
    let mut down = None;
    let mut except = Vec::new();

    // Parse frame options
    loop {
        if self.check(Kind::IntegerLiteral) {
            let num = self.parse_integer()?;
            if self.check(Kind::Columns) {
                self.advance();
                columns = Some(num);
            } else if self.check(Kind::Down) {
                self.advance();
                down = Some(num);
            }
        } else if self.check(Kind::Except) {
            self.advance();
            while self.check_identifier() && !self.check(Kind::Period) && !self.check(Kind::With) {
                except.push(self.parse_identifier()?);
            }
        } else {
            break;
        }
    }

    Ok(FrameClause { name, columns, down, except })
}
```

---

## Priority 6: MESSAGE Statement

**Status:** AST not defined, parser not implemented

**Syntax:**
```abl
MESSAGE "Hello, World!".
MESSAGE "Error:" errMsg VIEW-AS ALERT-BOX ERROR.
MESSAGE "Confirm delete?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice.
MESSAGE Customer.Name SKIP Customer.Balance.
```

**Concept:** `MESSAGE` displays messages to the user. It can show simple console output or UI alert boxes with buttons.

**AST Addition:**
```rust
Message {
    items: Vec<MessageItem>,
    view_as: Option<AlertBoxOptions>,
    update: Option<Identifier>,  // Variable to store button response
}

#[derive(Debug, Clone, PartialEq)]
pub enum MessageItem {
    Expression(Expression),
    Skip,                        // SKIP keyword for newline
    SkipCount(u32),              // SKIP(n)
}

#[derive(Debug, Clone, PartialEq)]
pub struct AlertBoxOptions {
    pub alert_type: Option<AlertType>,  // ERROR, WARNING, INFO, QUESTION
    pub buttons: Option<ButtonType>,     // YES-NO, YES-NO-CANCEL, OK, OK-CANCEL, RETRY-CANCEL
    pub title: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AlertType {
    Error,
    Warning,
    Info,
    Question,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ButtonType {
    YesNo,
    YesNoCancel,
    Ok,
    OkCancel,
    RetryCancel,
}
```

**Parser Implementation:**
```rust
fn parse_message_statement(&mut self) -> ParseResult<Statement> {
    self.advance(); // consume MESSAGE

    let mut items = Vec::new();

    // Parse message items until VIEW-AS, UPDATE, or period
    while !self.check(Kind::ViewAs) && !self.check(Kind::Update) && !self.check(Kind::Period) {
        if self.check(Kind::Skip) {
            self.advance();
            if self.check(Kind::LeftParen) {
                self.advance();
                let count = self.parse_integer()?;
                self.expect_kind(Kind::RightParen, "Expected ')'")?;
                items.push(MessageItem::SkipCount(count));
            } else {
                items.push(MessageItem::Skip);
            }
        } else {
            items.push(MessageItem::Expression(self.parse_expression()?));
        }
    }

    // Parse optional VIEW-AS ALERT-BOX
    let view_as = if self.check(Kind::ViewAs) {
        self.advance();
        self.expect_kind(Kind::AlertBox, "Expected ALERT-BOX after VIEW-AS")?;
        Some(self.parse_alert_box_options()?)
    } else {
        None
    };

    // Parse optional UPDATE variable
    let update = if self.check(Kind::Update) {
        self.advance();
        Some(self.parse_identifier()?)
    } else {
        None
    };

    self.expect_kind(Kind::Period, "Expected '.' after MESSAGE")?;

    Ok(Statement::Message { items, view_as, update })
}

fn parse_alert_box_options(&mut self) -> ParseResult<AlertBoxOptions> {
    let mut alert_type = None;
    let mut buttons = None;
    let mut title = None;

    loop {
        match self.peek().kind {
            Kind::Error => { self.advance(); alert_type = Some(AlertType::Error); }
            Kind::Warning => { self.advance(); alert_type = Some(AlertType::Warning); }
            Kind::Information => { self.advance(); alert_type = Some(AlertType::Info); }
            Kind::Question => { self.advance(); alert_type = Some(AlertType::Question); }
            Kind::Buttons => {
                self.advance();
                buttons = Some(match self.peek().kind {
                    Kind::YesNo => { self.advance(); ButtonType::YesNo }
                    Kind::YesNoCancel => { self.advance(); ButtonType::YesNoCancel }
                    Kind::Ok => { self.advance(); ButtonType::Ok }
                    Kind::OkCancel => { self.advance(); ButtonType::OkCancel }
                    Kind::RetryCancel => { self.advance(); ButtonType::RetryCancel }
                    _ => return Err(ParseError { message: "Expected button type".into(), .. }),
                });
            }
            Kind::Title => {
                self.advance();
                let token = self.expect_kind(Kind::StringLiteral, "Expected title string")?;
                title = Some(self.source[token.start+1..token.end-1].to_string());
            }
            _ => break,
        }
    }

    Ok(AlertBoxOptions { alert_type, buttons, title })
}
```

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
        Kind::KwFor => self.parse_for_each(),
        Kind::Find => self.parse_find_statement(),
        Kind::Case => self.parse_case_statement(),
        Kind::Procedure => self.parse_procedure(),
        Kind::Run => self.parse_run_statement(),
        Kind::Display => self.parse_display_statement(),
        Kind::Message => self.parse_message_statement(),
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
            Kind::Do | Kind::KwIf | Kind::KwFor | Kind::Define | Kind::End
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

### Completed
- [x] `FOR EACH` statement with lock types
- [x] Lock type lexer support (hyphenated and space-separated forms)

### In Progress / Next Up
- [ ] `FIND` statement (AST exists, parser needed)
- [ ] `CASE` statement (AST and parser needed)
- [ ] `PROCEDURE` definition (AST and parser needed)
- [ ] `RUN` statement (AST and parser needed)
- [ ] `DISPLAY` statement (AST and parser needed)
- [ ] `MESSAGE` statement (AST and parser needed)

### Cleanup
- [ ] Remove debug `println!` statements
- [ ] Add `parse_identifier()` helper
- [ ] Add tests for each new feature
- [ ] Consider error recovery strategy
