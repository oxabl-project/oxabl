# Parser Statement Development: Implementation Guide

This document continues from `NEXT_5_PHASES.md` (expression parsing) and covers statement parsing. Statements are the executable units of ABL programs - they *do* things rather than *evaluate* to values.

---

## Table of Contents

1. [Phase 6: Statement Infrastructure + Assignment](#phase-6-statement-infrastructure--assignment)
2. [Phase 7: Variable Declarations](#phase-7-variable-declarations)
3. [Phase 8: DO Blocks](#phase-8-do-blocks)
4. [Phase 9: IF Statements](#phase-9-if-statements)
5. [Phase 10: REPEAT and Loop Control](#phase-10-repeat-and-loop-control)

---

## Phase 6: Statement Infrastructure + Assignment

**Goal**: Create the Statement AST infrastructure and parse basic assignment statements.

### Lesson: Statements vs Expressions

In ABL (and most languages), there's a fundamental distinction:

- **Expressions** evaluate to a value: `1 + 2`, `NOW()`, `Customer.Name`
- **Statements** perform actions: `x = 5.`, `DISPLAY name.`, `DELETE Customer.`

Key differences in ABL:
1. Statements end with a period (`.`)
2. Statements can contain expressions, but not vice versa
3. Some things can be both (e.g., function calls can be expressions OR statements)

```
┌─────────────────────────────────────────────────┐
│  Statement                                      │
│  ┌───────────────────────────────────────────┐  │
│  │  myVar = 1 + calculateTotal(order)        │  │
│  │          └──────────┬─────────────┘       │  │
│  │              Expression                    │  │
│  └───────────────────────────────────────────┘  │
│                                            .    │
│                                    (terminator) │
└─────────────────────────────────────────────────┘
```

### Step 6.1: Add Statement AST

Create a new file `crates/oxabl_ast/src/statement.rs`:

```rust
use crate::{Expression, Identifier};

/// A statement in ABL - an executable unit that performs an action.
/// All statements are terminated by a period (`.`) in source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Assignment: `target = value.`
    /// Target can be identifier, array access, field access, member access
    Assignment {
        target: Expression,
        value: Expression,
    },

    /// An expression used as a statement (e.g., function/method calls)
    /// Example: `RUN myProc.` or `buffer:FIND-FIRST().`
    ExpressionStatement(Expression),

    /// A block of statements (used by IF, DO, REPEAT, etc.)
    Block(Vec<Statement>),

    /// Empty statement (just a period)
    Empty,
}
```

### Step 6.2: Update AST lib.rs

Edit `crates/oxabl_ast/src/lib.rs` to include the new module:

```rust
mod expression;
mod literal;
mod span;
mod statement;

pub use expression::*;
pub use literal::*;
pub use span::*;
pub use statement::*;
```

### Step 6.3: Add Statement Parsing to Parser

Add these methods to `impl Parser` in `crates/oxabl_parser/src/parser.rs`:

```rust
    /// Parse a single statement
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        // Skip empty statements (just periods)
        if self.check(Kind::Period) {
            self.advance();
            return Ok(Statement::Empty);
        }

        // For now, try to parse as expression, then determine if it's
        // an assignment or expression statement
        let expr = self.parse_expression()?;

        // Check for assignment: expr = value
        if self.check(Kind::Equals) {
            self.advance(); // consume '='
            let value = self.parse_expression()?;
            self.expect_period()?;
            return Ok(Statement::Assignment {
                target: expr,
                value,
            });
        }

        // Otherwise it's an expression statement
        self.expect_period()?;
        Ok(Statement::ExpressionStatement(expr))
    }

    /// Expect and consume a period, or return an error
    fn expect_period(&mut self) -> ParseResult<()> {
        if !self.check(Kind::Period) {
            return Err(ParseError {
                message: format!(
                    "Expected '.' to end statement, found {:?}",
                    self.peek().kind
                ),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance();
        Ok(())
    }

    /// Parse multiple statements until we hit a terminator
    pub fn parse_statements(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.at_end() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }
```

### Step 6.4: Add Tests

Add to the test module in `parser.rs`:

```rust
    // =========================================================================
    // Statement Tests
    // =========================================================================

    #[test]
    fn parse_simple_assignment() {
        let source = "x = 5.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::Assignment {
                target: Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "x".to_string()
                }),
                value: Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 5
                }))
            }
        );
    }

    #[test]
    fn parse_assignment_with_expression() {
        let source = "total = price * quantity.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::Assignment {
                target: Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 5 },
                    name: "total".to_string()
                }),
                value: Expression::Multiply(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 8, end: 13 },
                        name: "price".to_string()
                    })),
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 16, end: 24 },
                        name: "quantity".to_string()
                    }))
                )
            }
        );
    }

    #[test]
    fn parse_array_assignment() {
        let source = "arr[1] = 10.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::Assignment {
                target: Expression::ArrayAccess {
                    array: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 3 },
                        name: "arr".to_string()
                    })),
                    index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 1
                    })))
                },
                value: Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 9, end: 11 },
                    value: 10
                }))
            }
        );
    }

    #[test]
    fn parse_field_assignment() {
        let source = "Customer.Name = \"John\".";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::Assignment {
                target: Expression::FieldAccess {
                    qualifier: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 8 },
                        name: "Customer".to_string()
                    })),
                    field: Identifier {
                        span: Span { start: 9, end: 13 },
                        name: "Name".to_string()
                    }
                },
                value: Expression::Literal(Literal::String(StringLiteral {
                    span: Span { start: 16, end: 22 },
                    value: "John".to_string()
                }))
            }
        );
    }

    #[test]
    fn parse_expression_statement() {
        let source = "calculateTotals().";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::ExpressionStatement(Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 15 },
                    name: "calculateTotals".to_string()
                },
                arguments: vec![]
            })
        );
    }

    #[test]
    fn parse_method_call_statement() {
        let source = "buffer:SAVE().";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::ExpressionStatement(Expression::MethodCall {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "buffer".to_string()
                })),
                method: Identifier {
                    span: Span { start: 7, end: 11 },
                    name: "SAVE".to_string()
                },
                arguments: vec![]
            })
        );
    }

    #[test]
    fn parse_empty_statement() {
        let source = ".";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(stmt, Statement::Empty);
    }

    #[test]
    fn parse_multiple_statements() {
        let source = "x = 1. y = 2. z = x + y.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmts = parser.parse_statements().expect("Expected statements");
        assert_eq!(stmts.len(), 3);
    }
```

### Verification

```bash
cargo test -p oxabl_parser
```

---

## Phase 7: Variable Declarations

**Goal**: Parse `DEFINE VARIABLE` and `VAR` statements.

### Lesson: ABL Variable Declaration Syntax

ABL has two syntaxes for variable declaration:

**Classic syntax:**
```abl
DEFINE VARIABLE myVar AS INTEGER NO-UNDO.
DEFINE VARIABLE name AS CHARACTER INITIAL "unknown".
DEFINE VARIABLE total AS DECIMAL INITIAL 0.0.
```

**Modern syntax (OpenEdge 11.6+):**
```abl
VAR INTEGER myVar.
VAR CHARACTER name = "unknown".
VAR DECIMAL total = 0.0.
```

Key components:
- **Data type**: INTEGER, CHARACTER, DECIMAL, LOGICAL, DATE, DATETIME, HANDLE, etc.
- **NO-UNDO**: Variable won't be rolled back on transaction undo (common optimization)
- **INITIAL**: Default value (classic syntax)
- **= value**: Initial value (modern syntax)

### Step 7.1: Add DataType Enum

Add to `crates/oxabl_ast/src/statement.rs`:

```rust
/// ABL data types for variable declarations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Integer,
    Int64,
    Decimal,
    Character,
    Logical,
    Date,
    DateTime,
    DateTimeTz,
    Handle,
    Rowid,
    Recid,
    Raw,
    Memptr,
    Longchar,
    Clob,
    Blob,
    Com,
    /// Class type with fully qualified name
    Class(String),
}
```

### Step 7.2: Add VariableDeclaration to Statement

Update the `Statement` enum in `statement.rs`:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// Variable declaration
    /// Classic: `DEFINE VARIABLE name AS type [NO-UNDO] [INITIAL value].`
    /// Modern: `VAR type name [= value].`
    VariableDeclaration {
        name: Identifier,
        data_type: DataType,
        initial_value: Option<Expression>,
        no_undo: bool,
        /// Extent (array size), None for scalar, Some(0) for dynamic extent
        extent: Option<u32>,
    },

    // ... existing variants ...
}
```

### Step 7.3: Add Type Parsing Helper

Add to parser.rs:

```rust
    /// Parse a data type keyword
    fn parse_data_type(&mut self) -> ParseResult<DataType> {
        let token = self.peek();
        let type_str = self.source[token.start..token.end].to_uppercase();

        let data_type = match type_str.as_str() {
            "INTEGER" | "INT" => DataType::Integer,
            "INT64" => DataType::Int64,
            "DECIMAL" | "DEC" => DataType::Decimal,
            "CHARACTER" | "CHAR" => DataType::Character,
            "LOGICAL" | "LOG" => DataType::Logical,
            "DATE" => DataType::Date,
            "DATETIME" => DataType::DateTime,
            "DATETIME-TZ" => DataType::DateTimeTz,
            "HANDLE" => DataType::Handle,
            "ROWID" => DataType::Rowid,
            "RECID" => DataType::Recid,
            "RAW" => DataType::Raw,
            "MEMPTR" => DataType::Memptr,
            "LONGCHAR" => DataType::Longchar,
            "CLOB" => DataType::Clob,
            "BLOB" => DataType::Blob,
            "COM-HANDLE" => DataType::Com,
            _ => {
                return Err(ParseError {
                    message: format!("Unknown data type: {}", type_str),
                    span: Span {
                        start: token.start as u32,
                        end: token.end as u32,
                    },
                });
            }
        };

        self.advance();
        Ok(data_type)
    }
```

### Step 7.4: Add Variable Declaration Parsing

Add to parser.rs. Update `parse_statement()` to check for declaration keywords:

```rust
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        // Skip empty statements
        if self.check(Kind::Period) {
            self.advance();
            return Ok(Statement::Empty);
        }

        // DEFINE VARIABLE ...
        if self.check(Kind::Define) {
            return self.parse_define_statement();
        }

        // VAR type name ...
        // Note: "var" is not a reserved keyword, so we check the source text
        if self.check(Kind::Identifier) {
            let token = self.peek();
            let text = &self.source[token.start..token.end];
            if text.eq_ignore_ascii_case("var") {
                return self.parse_var_statement();
            }
        }

        // Expression or assignment
        let expr = self.parse_expression()?;

        if self.check(Kind::Equals) {
            self.advance();
            let value = self.parse_expression()?;
            self.expect_period()?;
            return Ok(Statement::Assignment {
                target: expr,
                value,
            });
        }

        self.expect_period()?;
        Ok(Statement::ExpressionStatement(expr))
    }

    /// Parse: DEFINE VARIABLE name AS type [NO-UNDO] [INITIAL value].
    fn parse_define_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DEFINE

        // For now, only handle VARIABLE. Later: BUFFER, TEMP-TABLE, etc.
        if !self.check(Kind::Identifier) {
            return Err(ParseError {
                message: "Expected VARIABLE after DEFINE".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let define_what = self.peek();
        let define_text = &self.source[define_what.start..define_what.end];

        if !define_text.eq_ignore_ascii_case("variable") {
            return Err(ParseError {
                message: format!("Expected VARIABLE after DEFINE, found {}", define_text),
                span: Span {
                    start: define_what.start as u32,
                    end: define_what.end as u32,
                },
            });
        }
        self.advance(); // consume VARIABLE

        // Parse variable name
        if !is_callable_kind(self.peek().kind) {
            return Err(ParseError {
                message: "Expected variable name".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        let name_token = self.advance();
        let name = Identifier {
            span: Span {
                start: name_token.start as u32,
                end: name_token.end as u32,
            },
            name: self.source[name_token.start..name_token.end].to_string(),
        };

        // Expect AS
        if !self.check(Kind::KwAs) {
            return Err(ParseError {
                message: "Expected AS after variable name".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance(); // consume AS

        // Parse data type
        let data_type = self.parse_data_type()?;

        // Parse optional modifiers
        let mut no_undo = false;
        let mut initial_value = None;
        let mut extent = None;

        loop {
            if self.check(Kind::NoUndo) {
                self.advance();
                no_undo = true;
            } else if self.check(Kind::Identifier) {
                let token = self.peek();
                let text = &self.source[token.start..token.end];

                if text.eq_ignore_ascii_case("initial") {
                    self.advance(); // consume INITIAL
                    initial_value = Some(self.parse_expression()?);
                } else if text.eq_ignore_ascii_case("extent") {
                    self.advance(); // consume EXTENT
                    // EXTENT can be followed by a number or nothing (dynamic)
                    if self.check(Kind::IntegerLiteral) {
                        let ext_token = self.advance();
                        if let Ok(n) = self.source[ext_token.start..ext_token.end].parse::<u32>() {
                            extent = Some(n);
                        }
                    } else {
                        extent = Some(0); // dynamic extent
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect_period()?;

        Ok(Statement::VariableDeclaration {
            name,
            data_type,
            initial_value,
            no_undo,
            extent,
        })
    }

    /// Parse: VAR type name [= value].
    fn parse_var_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume VAR

        // Parse data type
        let data_type = self.parse_data_type()?;

        // Parse variable name
        if !is_callable_kind(self.peek().kind) {
            return Err(ParseError {
                message: "Expected variable name".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        let name_token = self.advance();
        let name = Identifier {
            span: Span {
                start: name_token.start as u32,
                end: name_token.end as u32,
            },
            name: self.source[name_token.start..name_token.end].to_string(),
        };

        // Optional initial value
        let initial_value = if self.check(Kind::Equals) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_period()?;

        Ok(Statement::VariableDeclaration {
            name,
            data_type,
            initial_value,
            no_undo: true, // VAR implies NO-UNDO
            extent: None,
        })
    }
```

### Step 7.5: Add Tests

```rust
    #[test]
    fn parse_define_variable_simple() {
        let source = "DEFINE VARIABLE myVar AS INTEGER NO-UNDO.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::VariableDeclaration {
                name: Identifier {
                    span: Span { start: 16, end: 21 },
                    name: "myVar".to_string()
                },
                data_type: DataType::Integer,
                initial_value: None,
                no_undo: true,
                extent: None,
            }
        );
    }

    #[test]
    fn parse_define_variable_with_initial() {
        let source = "DEFINE VARIABLE counter AS INTEGER INITIAL 0.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::VariableDeclaration {
                name: Identifier {
                    span: Span { start: 16, end: 23 },
                    name: "counter".to_string()
                },
                data_type: DataType::Integer,
                initial_value: Some(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 43, end: 44 },
                    value: 0
                }))),
                no_undo: false,
                extent: None,
            }
        );
    }

    #[test]
    fn parse_define_variable_character() {
        let source = "DEFINE VARIABLE name AS CHARACTER NO-UNDO INITIAL \"unknown\".";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::VariableDeclaration {
                data_type,
                no_undo,
                initial_value,
                ..
            } => {
                assert_eq!(data_type, DataType::Character);
                assert!(no_undo);
                assert!(initial_value.is_some());
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn parse_var_statement_simple() {
        let source = "VAR INTEGER myCount.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(
            stmt,
            Statement::VariableDeclaration {
                name: Identifier {
                    span: Span { start: 12, end: 19 },
                    name: "myCount".to_string()
                },
                data_type: DataType::Integer,
                initial_value: None,
                no_undo: true,
                extent: None,
            }
        );
    }

    #[test]
    fn parse_var_statement_with_initial() {
        let source = "VAR DECIMAL total = 100.50.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::VariableDeclaration {
                name,
                data_type,
                initial_value,
                no_undo,
                ..
            } => {
                assert_eq!(name.name, "total");
                assert_eq!(data_type, DataType::Decimal);
                assert!(no_undo);
                assert!(initial_value.is_some());
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }

    #[test]
    fn parse_var_logical() {
        let source = "VAR LOGICAL isActive = TRUE.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::VariableDeclaration {
                data_type,
                initial_value,
                ..
            } => {
                assert_eq!(data_type, DataType::Logical);
                assert!(matches!(
                    initial_value,
                    Some(Expression::Literal(Literal::Boolean(_)))
                ));
            }
            _ => panic!("Expected VariableDeclaration"),
        }
    }
```

### Verification

```bash
cargo test -p oxabl_parser
```

---

## Phase 8: DO Blocks

**Goal**: Parse DO blocks with various forms.

### Lesson: ABL Block Structure

ABL uses keyword pairs to delimit blocks:

```abl
DO:
    /* statements */
END.

DO i = 1 TO 10:
    /* statements */
END.

DO WHILE condition:
    /* statements */
END.
```

Key insight: The colon (`:`) begins the block body, and `END.` terminates it.

### Step 8.1: Add DO Statement to AST

Add to `Statement` enum in `statement.rs`:

```rust
    /// DO block with optional loop variable
    /// `DO [var = start TO end [BY step]]:` or `DO WHILE condition:` or just `DO:`
    Do {
        /// Loop variable assignment (for counting loops)
        loop_var: Option<Identifier>,
        /// Start value (for counting loops)
        from: Option<Expression>,
        /// End value (for counting loops)
        to: Option<Expression>,
        /// Step value (for counting loops)
        by: Option<Expression>,
        /// WHILE condition (for conditional loops)
        while_condition: Option<Expression>,
        /// Block body
        body: Vec<Statement>,
    },
```

### Step 8.2: Add Block Parsing

Add to parser.rs:

```rust
    /// Parse statements until END keyword
    fn parse_block_body(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.check(Kind::End) && !self.at_end() {
            statements.push(self.parse_statement()?);
        }

        // Consume END
        if !self.check(Kind::End) {
            return Err(ParseError {
                message: "Expected END to close block".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance();
        self.expect_period()?;

        Ok(statements)
    }
```

### Step 8.3: Add DO Parsing

Update `parse_statement()` to handle DO:

```rust
        // DO block
        if self.check(Kind::Do) {
            return self.parse_do_statement();
        }
```

Add the DO parsing method:

```rust
    /// Parse DO block: `DO [var = start TO end [BY step]] [WHILE cond]:`
    fn parse_do_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DO

        let mut loop_var = None;
        let mut from = None;
        let mut to = None;
        let mut by = None;
        let mut while_condition = None;

        // Check for loop variable: DO i = 1 TO 10:
        if is_callable_kind(self.peek().kind) {
            // Peek ahead to see if this is `var = start TO end`
            let saved_pos = self.current;
            let potential_var = self.advance();

            if self.check(Kind::Equals) {
                // It's a counting loop
                let var_name = Identifier {
                    span: Span {
                        start: potential_var.start as u32,
                        end: potential_var.end as u32,
                    },
                    name: self.source[potential_var.start..potential_var.end].to_string(),
                };
                loop_var = Some(var_name);

                self.advance(); // consume '='
                from = Some(self.parse_expression()?);

                // Expect TO
                if !self.check(Kind::To) {
                    return Err(ParseError {
                        message: "Expected TO in DO loop".to_string(),
                        span: Span {
                            start: self.peek().start as u32,
                            end: self.peek().end as u32,
                        },
                    });
                }
                self.advance(); // consume TO
                to = Some(self.parse_expression()?);

                // Optional BY
                if self.check(Kind::By) {
                    self.advance();
                    by = Some(self.parse_expression()?);
                }
            } else {
                // Not a counting loop, restore position
                self.current = saved_pos;
            }
        }

        // Check for WHILE
        if self.check(Kind::KwWhile) {
            self.advance();
            while_condition = Some(self.parse_expression()?);
        }

        // Expect colon to start block
        if !self.check(Kind::Colon) {
            return Err(ParseError {
                message: "Expected ':' after DO".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance();

        let body = self.parse_block_body()?;

        Ok(Statement::Do {
            loop_var,
            from,
            to,
            by,
            while_condition,
            body,
        })
    }
```

### Step 8.4: Add Tests

```rust
    #[test]
    fn parse_simple_do_block() {
        let source = "DO: x = 1. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Do { body, loop_var, .. } => {
                assert!(loop_var.is_none());
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected Do statement"),
        }
    }

    #[test]
    fn parse_do_counting_loop() {
        let source = "DO i = 1 TO 10: x = i. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Do {
                loop_var,
                from,
                to,
                body,
                ..
            } => {
                assert!(loop_var.is_some());
                assert_eq!(loop_var.unwrap().name, "i");
                assert!(from.is_some());
                assert!(to.is_some());
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected Do statement"),
        }
    }

    #[test]
    fn parse_do_with_by() {
        let source = "DO i = 0 TO 100 BY 10: total = total + i. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Do { by, .. } => {
                assert!(by.is_some());
            }
            _ => panic!("Expected Do statement"),
        }
    }

    #[test]
    fn parse_do_while() {
        let source = "DO WHILE x < 10: x = x + 1. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Do {
                while_condition, ..
            } => {
                assert!(while_condition.is_some());
            }
            _ => panic!("Expected Do statement"),
        }
    }

    #[test]
    fn parse_nested_do_blocks() {
        let source = "DO i = 1 TO 3: DO j = 1 TO 3: x = i * j. END. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Do { body, .. } => {
                assert_eq!(body.len(), 1);
                assert!(matches!(body[0], Statement::Do { .. }));
            }
            _ => panic!("Expected Do statement"),
        }
    }
```

### Verification

```bash
cargo test -p oxabl_parser
```

---

## Phase 9: IF Statements

**Goal**: Parse IF/THEN/ELSE as statements (not expressions).

### Lesson: IF Expression vs IF Statement

ABL has both:

**IF Expression** (already implemented - evaluates to a value):
```abl
x = IF condition THEN value1 ELSE value2.
```

**IF Statement** (executes code blocks):
```abl
IF condition THEN DO:
    /* statements */
END.
ELSE DO:
    /* statements */
END.

/* Or single statement form: */
IF condition THEN
    x = 1.
ELSE
    x = 2.
```

The key difference: IF statements don't require ELSE (it's optional), while IF expressions require both THEN and ELSE branches.

### Step 9.1: Add IF Statement to AST

Add to `Statement` enum:

```rust
    /// IF statement (not expression)
    /// `IF condition THEN statement [ELSE statement]`
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
```

### Step 9.2: Add IF Statement Parsing

Update `parse_statement()`:

```rust
        // IF statement
        if self.check(Kind::KwIf) {
            return self.parse_if_statement();
        }
```

Add the parsing method:

```rust
    /// Parse IF statement
    fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume IF

        let condition = self.parse_expression()?;

        // Expect THEN
        if !self.check(Kind::Then) {
            return Err(ParseError {
                message: "Expected THEN after IF condition".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance();

        // Parse then branch - could be DO block or single statement
        let then_branch = if self.check(Kind::Do) {
            self.parse_do_statement()?
        } else {
            self.parse_statement()?
        };

        // Optional ELSE
        let else_branch = if self.check(Kind::KwElse) {
            self.advance();
            let else_stmt = if self.check(Kind::Do) {
                self.parse_do_statement()?
            } else if self.check(Kind::KwIf) {
                // ELSE IF chain
                self.parse_if_statement()?
            } else {
                self.parse_statement()?
            };
            Some(Box::new(else_stmt))
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }
```

### Step 9.3: Add Tests

```rust
    #[test]
    fn parse_if_then_simple() {
        let source = "IF x > 0 THEN y = 1.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert!(matches!(condition, Expression::GreaterThan(_, _)));
                assert!(matches!(*then_branch, Statement::Assignment { .. }));
                assert!(else_branch.is_none());
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    fn parse_if_then_else() {
        let source = "IF x > 0 THEN y = 1. ELSE y = 0.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::If { else_branch, .. } => {
                assert!(else_branch.is_some());
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    fn parse_if_then_do_block() {
        let source = "IF x > 0 THEN DO: y = 1. z = 2. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::If { then_branch, .. } => {
                assert!(matches!(*then_branch, Statement::Do { .. }));
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    fn parse_if_else_if_chain() {
        let source = "IF x > 10 THEN y = 3. ELSE IF x > 5 THEN y = 2. ELSE y = 1.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::If { else_branch, .. } => {
                let else_stmt = else_branch.expect("Should have else");
                assert!(matches!(*else_stmt, Statement::If { .. }));
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    fn parse_nested_if() {
        let source = "IF a THEN IF b THEN x = 1.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::If { then_branch, .. } => {
                assert!(matches!(*then_branch, Statement::If { .. }));
            }
            _ => panic!("Expected If statement"),
        }
    }
```

### Verification

```bash
cargo test -p oxabl_parser
```

---

## Phase 10: REPEAT and Loop Control

**Goal**: Parse REPEAT blocks and LEAVE/NEXT statements.

### Lesson: REPEAT vs DO

REPEAT is ABL's infinite loop construct:

```abl
REPEAT:
    /* Process records */
    IF done THEN LEAVE.
END.
```

Key differences from DO:
- REPEAT loops forever unless broken with LEAVE
- REPEAT can have WHILE but no counting variable
- REPEAT is often used with database queries

Control statements:
- `LEAVE.` - Exit the innermost loop (like `break`)
- `NEXT.` - Skip to next iteration (like `continue`)

### Step 10.1: Add REPEAT and Control Statements to AST

Add to `Statement` enum:

```rust
    /// REPEAT block
    /// `REPEAT [WHILE condition]:`
    Repeat {
        while_condition: Option<Expression>,
        body: Vec<Statement>,
    },

    /// LEAVE statement - exit innermost loop
    Leave,

    /// NEXT statement - skip to next iteration
    Next,

    /// RETURN statement
    /// `RETURN [expression].`
    Return(Option<Expression>),
```

### Step 10.2: Add Parsing

Update `parse_statement()`:

```rust
        // REPEAT block
        if self.check(Kind::Repeat) {
            return self.parse_repeat_statement();
        }

        // LEAVE
        if self.check(Kind::Leave) {
            self.advance();
            self.expect_period()?;
            return Ok(Statement::Leave);
        }

        // NEXT
        if self.check(Kind::Next) {
            self.advance();
            self.expect_period()?;
            return Ok(Statement::Next);
        }

        // RETURN
        if self.check(Kind::KwReturn) {
            return self.parse_return_statement();
        }
```

Add the methods:

```rust
    /// Parse REPEAT block
    fn parse_repeat_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume REPEAT

        // Optional WHILE
        let while_condition = if self.check(Kind::KwWhile) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Expect colon
        if !self.check(Kind::Colon) {
            return Err(ParseError {
                message: "Expected ':' after REPEAT".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance();

        let body = self.parse_block_body()?;

        Ok(Statement::Repeat {
            while_condition,
            body,
        })
    }

    /// Parse RETURN statement
    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume RETURN

        // Check if there's a return value (not just a period)
        let value = if !self.check(Kind::Period) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_period()?;
        Ok(Statement::Return(value))
    }
```

### Step 10.3: Add Tests

```rust
    #[test]
    fn parse_simple_repeat() {
        let source = "REPEAT: x = x + 1. IF x > 10 THEN LEAVE. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Repeat { body, while_condition } => {
                assert!(while_condition.is_none());
                assert_eq!(body.len(), 2);
            }
            _ => panic!("Expected Repeat statement"),
        }
    }

    #[test]
    fn parse_repeat_while() {
        let source = "REPEAT WHILE x < 100: x = x * 2. END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Repeat { while_condition, .. } => {
                assert!(while_condition.is_some());
            }
            _ => panic!("Expected Repeat statement"),
        }
    }

    #[test]
    fn parse_leave_statement() {
        let source = "LEAVE.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(stmt, Statement::Leave);
    }

    #[test]
    fn parse_next_statement() {
        let source = "NEXT.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(stmt, Statement::Next);
    }

    #[test]
    fn parse_return_no_value() {
        let source = "RETURN.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert_eq!(stmt, Statement::Return(None));
    }

    #[test]
    fn parse_return_with_value() {
        let source = "RETURN x + 1.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Return(Some(expr)) => {
                assert!(matches!(expr, Expression::Add(_, _)));
            }
            _ => panic!("Expected Return with value"),
        }
    }

    #[test]
    fn parse_loop_with_leave_and_next() {
        let source = "DO i = 1 TO 100: IF i MOD 2 = 0 THEN NEXT. IF i > 50 THEN LEAVE. process(i). END.";
        // Note: MOD is not implemented yet, so this test is conceptual
        // For now, test a simpler version:
        let source = "DO i = 1 TO 100: IF done THEN LEAVE. IF skip THEN NEXT. process(i). END.";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt {
            Statement::Do { body, .. } => {
                assert_eq!(body.len(), 3);
            }
            _ => panic!("Expected Do statement"),
        }
    }
```

### Verification

```bash
cargo test -p oxabl_parser
```

---

## Complete Implementation Checklist

### Phase 6: Statement Infrastructure
- [ ] Create `statement.rs` with `Statement` enum
- [ ] Add `Statement::Assignment`
- [ ] Add `Statement::ExpressionStatement`
- [ ] Add `Statement::Block`
- [ ] Add `Statement::Empty`
- [ ] Update AST lib.rs exports
- [ ] Add `parse_statement()` method
- [ ] Add `expect_period()` helper
- [ ] Add `parse_statements()` method
- [ ] Add 8 statement tests
- [ ] Run `cargo test -p oxabl_parser`

### Phase 7: Variable Declarations
- [ ] Add `DataType` enum
- [ ] Add `Statement::VariableDeclaration`
- [ ] Add `parse_data_type()` helper
- [ ] Add `parse_define_statement()`
- [ ] Add `parse_var_statement()`
- [ ] Add 6 variable declaration tests
- [ ] Run `cargo test -p oxabl_parser`

### Phase 8: DO Blocks
- [ ] Add `Statement::Do` variant
- [ ] Add `parse_block_body()` helper
- [ ] Add `parse_do_statement()`
- [ ] Add 5 DO block tests
- [ ] Run `cargo test -p oxabl_parser`

### Phase 9: IF Statements
- [ ] Add `Statement::If` variant
- [ ] Add `parse_if_statement()`
- [ ] Add 5 IF statement tests
- [ ] Run `cargo test -p oxabl_parser`

### Phase 10: REPEAT and Loop Control
- [ ] Add `Statement::Repeat` variant
- [ ] Add `Statement::Leave`
- [ ] Add `Statement::Next`
- [ ] Add `Statement::Return`
- [ ] Add `parse_repeat_statement()`
- [ ] Add `parse_return_statement()`
- [ ] Add 7 loop control tests
- [ ] Run `cargo test -p oxabl_parser`

---

## Summary of New Tests

**Phase 6 (8 tests)**:
- `parse_simple_assignment`
- `parse_assignment_with_expression`
- `parse_array_assignment`
- `parse_field_assignment`
- `parse_expression_statement`
- `parse_method_call_statement`
- `parse_empty_statement`
- `parse_multiple_statements`

**Phase 7 (6 tests)**:
- `parse_define_variable_simple`
- `parse_define_variable_with_initial`
- `parse_define_variable_character`
- `parse_var_statement_simple`
- `parse_var_statement_with_initial`
- `parse_var_logical`

**Phase 8 (5 tests)**:
- `parse_simple_do_block`
- `parse_do_counting_loop`
- `parse_do_with_by`
- `parse_do_while`
- `parse_nested_do_blocks`

**Phase 9 (5 tests)**:
- `parse_if_then_simple`
- `parse_if_then_else`
- `parse_if_then_do_block`
- `parse_if_else_if_chain`
- `parse_nested_if`

**Phase 10 (7 tests)**:
- `parse_simple_repeat`
- `parse_repeat_while`
- `parse_leave_statement`
- `parse_next_statement`
- `parse_return_no_value`
- `parse_return_with_value`
- `parse_loop_with_leave_and_next`

**Total: 31 new tests**
