//! Statement parsing for the Oxabl parser.
//!
//! Handles DEFINE VARIABLE, VAR, assignments, DO blocks (with counting loops),
//! IF/THEN/ELSE, REPEAT, FOR EACH, FIND, CASE, PROCEDURE, RUN, DISPLAY,
//! MESSAGE, LEAVE, NEXT, and RETURN statements.

use oxabl_ast::{
    BufferTarget, DisplayItem, Expression, FieldTypeSource, FindType, Identifier, IndexField,
    LockType, ParameterDirection, RunArgument, RunTarget, SortDirection, Span, Statement,
    TempTableField, TempTableIndex, UseIndex, WhenBranch,
};
use oxabl_lexer::Kind;

use super::{ParseError, ParseResult, Parser};

impl Parser<'_> {
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        // Skip empty statements
        if self.check(Kind::Period) {
            self.advance();
            return Ok(Statement::Empty);
        }

        // DO blocks
        if self.check(Kind::Do) {
            return self.parse_do_statement();
        }

        // IF statement
        if self.check(Kind::KwIf) {
            return self.parse_if_statement();
        }

        // Repeat block
        if self.check(Kind::Repeat) {
            return self.parse_repeat_statement();
        }

        // LEAVE
        if self.check(Kind::Leave) {
            self.advance();
            self.expect_kind(Kind::Period, "Expected '.' to come after LEAVE")?;
            return Ok(Statement::Leave);
        }

        // Next
        if self.check(Kind::Next) {
            self.advance();
            self.expect_kind(Kind::Period, "Expected '.' to come after NEXT")?;
            return Ok(Statement::Next);
        }

        // Return
        if self.check(Kind::KwReturn) {
            return self.parse_return_statement();
        }

        // FOR EACH
        if self.check(Kind::KwFor) {
            return self.parse_for_each();
        }

        // FIND statement
        if self.check(Kind::Find) {
            return self.parse_find_statement();
        }

        // CASE statement
        if self.check(Kind::Case) {
            return self.parse_case_statement();
        }

        // RUN statement
        if self.check(Kind::Run) {
            return self.parse_run_statement();
        }

        // PROCEDURE statement
        if self.check(Kind::Procedure) {
            return self.parse_procedure();
        }

        // RUN statement
        if self.check(Kind::Run) {
            return self.parse_run_statement();
        }

        // DISPLAY statement
        if self.check(Kind::Display) {
            return self.parse_display_statement();
        }

        // MESSAGE statement
        if self.check(Kind::Message) {
            return self.parse_message_statement();
        }

        // Check for traditional define statement
        // def var name as type [no-undo] [initial value] [extent n].
        if self.check(Kind::Define) {
            return self.parse_define_statement();
        }

        // Check for new var statement
        // var char name [=] [5].
        if self.check(Kind::Identifier) {
            let token = self.peek();
            let text = &self.source[token.start..token.end];
            if text.eq_ignore_ascii_case("var") {
                return self.parse_var_statement();
            }
        }

        // Parse left-hand assignment, stop before comparison operators
        let left = self.parse_additive()?;

        if self.check(Kind::Equals) {
            self.advance(); // consume the "="
            let value = self.parse_expression()?;
            self.expect_kind(Kind::Period, "Expected '.' to end statement")?;
            return Ok(Statement::Assignment {
                target: left,
                value,
            });
        }

        // not an assignment, continue parsing as full expression
        let expr = self.finish_expression(left)?;
        self.expect_kind(Kind::Period, "Expected '.' to end statement")?;
        Ok(Statement::ExpressionStatement(expr))
    }

    // parse define variable as type [no-undo] [initial]
    fn parse_define_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DEFINE

        // parse INPUT/OUTPUT parameters
        if self.check(Kind::Input) || self.check(Kind::Output) || self.check(Kind::InputOutput) {
            return self.parse_define_parameter();
        }

        // DEFINE TEMP-TABLE
        if self.check(Kind::TempTable) {
            return self.parse_define_temp_table();
        }

        // DEFINE BUFFER
        if self.check(Kind::Buffer) {
            return self.parse_define_buffer();
        }

        // DEFINE VARIABLE / DEFINE VAR
        if Self::can_be_identifier(self.peek().kind) {
            let token = self.peek();
            let text = &self.source[token.start..token.end];
            if text.eq_ignore_ascii_case("variable") || text.eq_ignore_ascii_case("var") {
                self.advance(); // consume VARIABLE or VAR
            } else {
                return Err(ParseError {
                    message: "Expected VARIABLE, VAR, TEMP-TABLE, or BUFFER after DEFINE"
                        .to_string(),
                    span: Span {
                        start: token.start as u32,
                        end: token.end as u32,
                    },
                });
            }
        } else {
            return Err(ParseError {
                message: "Expected keyword after DEFINE (VARIABLE, TEMP-TABLE, BUFFER, etc.)"
                    .to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        // Name
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected variable name after DEFINE VARIABLE".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let name_token = self.advance().clone();
        let name = Identifier {
            span: Span {
                start: name_token.start as u32,
                end: name_token.end as u32,
            },
            name: self.source[name_token.start..name_token.end].to_string(),
        };

        // expect As
        self.expect_kind(Kind::KwAs, "Expected AS after variable name")?;

        // parse data type
        let data_type = self.parse_data_type()?;

        // parse optional no-undo, initial, and extent
        let mut no_undo = false;
        let mut initial_value = None;
        let mut extent = None;

        loop {
            match self.peek().kind {
                Kind::NoUndo => {
                    self.advance();
                    no_undo = true;
                }
                Kind::Initial => {
                    self.advance();
                    initial_value = Some(self.parse_expression()?);
                }
                Kind::Extent => {
                    self.advance();
                    // Extent can be followed by number or nothing (dynamic)
                    if self.check(Kind::IntegerLiteral) {
                        let ext_token = self.advance().clone();
                        if let Ok(n) = self.source[ext_token.start..ext_token.end].parse::<u32>() {
                            extent = Some(n);
                        } else {
                            extent = Some(0); // dynamic
                        }
                    }
                }
                _ => break,
            }
        }

        self.expect_kind(Kind::Period, "Expected '.' to end statement")?;

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
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected variable name".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        let name_token = self.advance().clone();
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

        self.expect_kind(Kind::Period, "Expected '.' to end statement")?;

        Ok(Statement::VariableDeclaration {
            name,
            data_type,
            initial_value,
            no_undo: true, // VAR implies NO-UNDO
            extent: None,
        })
    }

    fn parse_define_parameter(&mut self) -> ParseResult<Statement> {
        // Parse direction (we already know it's INPUT, OUTPUT, or INPUT-OUTPUT)
        let direction = match self.peek().kind {
            Kind::Input => {
                self.advance();
                ParameterDirection::Input
            }
            Kind::Output => {
                self.advance();
                ParameterDirection::Output
            }
            Kind::InputOutput => {
                self.advance();
                ParameterDirection::InputOutput
            }
            _ => unreachable!("parse_define_parameter called without INPUT/OUTPUT token"),
        };

        // Expect PARAMETER keyword
        self.expect_kind(Kind::Parameter, "Expected PARAMETER after INPUT/OUTPUT")?;

        // Parse parameter name
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected parameter name".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        let name_token = self.advance().clone();
        let name = Identifier {
            span: Span {
                start: name_token.start as u32,
                end: name_token.end as u32,
            },
            name: self.source[name_token.start..name_token.end].to_string(),
        };

        // Expect AS
        self.expect_kind(Kind::KwAs, "Expected AS after parameter name")?;

        // Parse data type
        let data_type = self.parse_data_type()?;

        // Optional NO-UNDO
        let no_undo = if self.check(Kind::NoUndo) {
            self.advance();
            true
        } else {
            false
        };

        self.expect_kind(Kind::Period, "Expected '.' after parameter definition")?;

        Ok(Statement::DefineParameter {
            direction,
            name,
            data_type,
            no_undo,
        })
    }

    // Parse DEFINE TEMP-TABLE
    fn parse_define_temp_table(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume TEMP-TABLE

        let name = self.parse_identifier()?;

        // Optional NO-UNDO
        let no_undo = if self.check(Kind::NoUndo) {
            self.advance();
            true
        } else {
            false
        };

        // Optional LIKE / LIKE-SEQUENTIAL clause
        let mut like_table = None;
        let mut validate = false;
        let mut use_indexes = Vec::new();

        if self.check(Kind::Like) || self.check(Kind::LikeSequential) {
            self.advance(); // consume LIKE or LIKE-SEQUENTIAL
            like_table = Some(self.parse_identifier()?);

            // Optional VALIDATE
            if self.check(Kind::Validate) {
                self.advance();
                validate = true;
            }

            // Optional USE-INDEX clauses
            while self.check(Kind::UseIndex) {
                self.advance(); // consume USE-INDEX
                let idx_name = self.parse_identifier()?;
                let as_primary = if self.check(Kind::KwAs) {
                    self.advance();
                    self.expect_kind(Kind::Primary, "Expected PRIMARY after AS in USE-INDEX")?;
                    true
                } else {
                    false
                };
                use_indexes.push(UseIndex {
                    name: idx_name,
                    as_primary,
                });
            }
        }

        let mut fields = Vec::new();
        let mut indexes = Vec::new();

        // Parse FIELD and INDEX definitions until period
        while !self.check(Kind::Period) && !self.at_end() {
            if self.check(Kind::Field) {
                self.advance(); // consume FIELD
                let field_name = self.parse_identifier()?;

                // Parse type source: AS type or LIKE field
                let type_source = if self.check(Kind::Like) {
                    self.advance();
                    let source = self.parse_qualified_identifier()?;
                    let field_validate = if self.check(Kind::Validate) {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    FieldTypeSource::Like {
                        source,
                        validate: field_validate,
                    }
                } else {
                    self.expect_kind(Kind::KwAs, "Expected AS or LIKE after field name")?;
                    FieldTypeSource::Explicit(self.parse_data_type()?)
                };

                // Parse optional field options
                let mut initial_value = None;
                let mut extent = None;

                loop {
                    match self.peek().kind {
                        Kind::Initial => {
                            self.advance();
                            // Handle array initial syntax: INITIAL [val1, val2, ...]
                            if self.check(Kind::LeftBracket) {
                                self.advance(); // consume [
                                let mut values = Vec::new();
                                if !self.check(Kind::RightBracket) {
                                    values.push(self.parse_expression()?);
                                    while self.check(Kind::Comma) {
                                        self.advance();
                                        values.push(self.parse_expression()?);
                                    }
                                }
                                self.expect_kind(
                                    Kind::RightBracket,
                                    "Expected ']' after initial values",
                                )?;
                                initial_value = Some(values);
                            } else {
                                // Scalar initial value
                                initial_value = Some(vec![self.parse_expression()?]);
                            }
                        }
                        Kind::Extent => {
                            self.advance();
                            if self.check(Kind::IntegerLiteral) {
                                let ext_token = self.advance().clone();
                                if let Ok(n) =
                                    self.source[ext_token.start..ext_token.end].parse::<u32>()
                                {
                                    extent = Some(n);
                                } else {
                                    extent = Some(0); // dynamic
                                }
                            }
                        }
                        // Skip known field options we don't store in the AST
                        Kind::Format | Kind::Label | Kind::ColumnLabel => {
                            self.advance(); // consume keyword
                            // Skip the value (usually a string literal)
                            if self.check(Kind::StringLiteral) {
                                self.advance();
                            }
                        }
                        // Break on field/index/period boundaries
                        Kind::Field | Kind::Index | Kind::Period => break,
                        _ => break,
                    }
                }

                fields.push(TempTableField {
                    name: field_name,
                    type_source,
                    initial_value,
                    extent,
                });
            } else if self.check(Kind::Index) {
                self.advance(); // consume INDEX
                let index_name = self.parse_identifier()?;

                let mut is_primary = false;
                let mut is_unique = false;
                let mut is_word_index = false;

                // Optional IS or AS prefix (both valid, or neither)
                if self.check(Kind::Is) || self.check(Kind::KwAs) {
                    self.advance();
                }

                // Parse flags in any order: PRIMARY, UNIQUE, WORD-INDEX
                loop {
                    match self.peek().kind {
                        Kind::Primary => {
                            self.advance();
                            is_primary = true;
                        }
                        Kind::Unique => {
                            self.advance();
                            is_unique = true;
                        }
                        Kind::WordIndex => {
                            self.advance();
                            is_word_index = true;
                        }
                        _ => break,
                    }
                }

                // Parse index fields with optional ASC/DESC direction
                let mut index_fields = Vec::new();
                while Self::can_be_identifier(self.peek().kind)
                    && !self.check(Kind::Field)
                    && !self.check(Kind::Index)
                    && !self.check(Kind::Period)
                {
                    let field_name = self.parse_identifier()?;
                    let direction = match self.peek().kind {
                        Kind::Ascending => {
                            self.advance();
                            Some(SortDirection::Ascending)
                        }
                        Kind::Descending => {
                            self.advance();
                            Some(SortDirection::Descending)
                        }
                        _ => None,
                    };
                    index_fields.push(IndexField {
                        name: field_name,
                        direction,
                    });
                }

                indexes.push(TempTableIndex {
                    name: index_name,
                    is_primary,
                    is_unique,
                    is_word_index,
                    fields: index_fields,
                });
            } else {
                // Skip unknown tokens in temp-table definition
                self.advance();
            }
        }

        self.expect_kind(Kind::Period, "Expected '.' after DEFINE TEMP-TABLE")?;

        Ok(Statement::DefineTempTable {
            name,
            no_undo,
            like_table,
            validate,
            use_indexes,
            fields,
            indexes,
        })
    }

    // Parse DEFINE BUFFER name FOR [TEMP-TABLE] table [PRESELECT] [LABEL "str"].
    fn parse_define_buffer(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume BUFFER

        let name = self.parse_identifier()?;
        self.expect_kind(Kind::KwFor, "Expected FOR after buffer name")?;

        // Check for FOR TEMP-TABLE vs FOR table
        let target = if self.check(Kind::TempTable) {
            self.advance();
            BufferTarget::TempTable(self.parse_identifier()?)
        } else {
            BufferTarget::Table(self.parse_identifier()?)
        };

        // Parse optional modifiers
        let mut preselect = false;
        let mut label = None;

        while !self.check(Kind::Period) && !self.at_end() {
            match self.peek().kind {
                Kind::Preselect => {
                    self.advance();
                    preselect = true;
                }
                Kind::Label => {
                    self.advance();
                    if self.check(Kind::StringLiteral) {
                        let token = self.advance().clone();
                        // Strip quotes from string literal
                        let raw = &self.source[token.start..token.end];
                        label = Some(raw[1..raw.len() - 1].to_string());
                    }
                }
                _ => {
                    // Skip unknown tokens (NAMESPACE-URI, SERIALIZE-NAME, etc.)
                    self.advance();
                    // Skip their string value if present
                    if self.check(Kind::StringLiteral) {
                        self.advance();
                    }
                }
            }
        }

        self.expect_kind(Kind::Period, "Expected '.' after DEFINE BUFFER")?;

        Ok(Statement::DefineBuffer {
            name,
            target,
            preselect,
            label,
        })
    }

    /// Continue parsing an expression after additive level has been parsed
    fn finish_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        // Handle comparison operators (except = which we already checked)
        let expr = if self.is_non_equals_comparison_operator() {
            let op_kind = self.advance().kind;
            let right = self.parse_additive()?;
            self.make_comparison(left, op_kind, right)
        } else {
            left
        };

        // Handle AND
        let mut expr = expr;
        while self.check(Kind::And) {
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expression::And(Box::new(expr), Box::new(right));
        }

        // Handle OR
        while self.check(Kind::Or) {
            self.advance();
            let right = self.parse_and()?;
            expr = Expression::Or(Box::new(expr), Box::new(right));
        }

        Ok(expr)
    }

    fn is_non_equals_comparison_operator(&self) -> bool {
        matches!(
            self.peek().kind,
            Kind::NotEqual
                | Kind::LessThan
                | Kind::LessThanOrEqual
                | Kind::GreaterThan
                | Kind::GreaterThanOrEqual
                | Kind::Ne
                | Kind::Lt
                | Kind::Le
                | Kind::Gt
                | Kind::Ge
                | Kind::Begins
                | Kind::Matches
                | Kind::Contains
        )
    }

    fn make_comparison(&self, left: Expression, op: Kind, right: Expression) -> Expression {
        match op {
            Kind::NotEqual | Kind::Ne => Expression::NotEqual(Box::new(left), Box::new(right)),
            Kind::LessThan | Kind::Lt => Expression::LessThan(Box::new(left), Box::new(right)),
            Kind::LessThanOrEqual | Kind::Le => {
                Expression::LessThanOrEqual(Box::new(left), Box::new(right))
            }
            Kind::GreaterThan | Kind::Gt => {
                Expression::GreaterThan(Box::new(left), Box::new(right))
            }
            Kind::GreaterThanOrEqual | Kind::Ge => {
                Expression::GreaterThanOrEqual(Box::new(left), Box::new(right))
            }
            Kind::Begins => Expression::Begins(Box::new(left), Box::new(right)),
            Kind::Matches => Expression::Matches(Box::new(left), Box::new(right)),
            Kind::Contains => Expression::Contains(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        }
    }

    /// Parse multiple statements until we hit a terminator
    pub fn parse_statements(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.at_end() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    fn parse_do_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consume DO

        let mut loop_var = None;
        let mut from = None;
        let mut to = None;
        let mut by = None;
        let mut while_condition = None;

        // check for loop
        if Self::can_be_identifier(self.peek().kind) {
            // peek ahead to see if this is 'var = start to end'
            let saved_pos = self.current;
            let potential_var = self.advance().clone();

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

                self.advance(); // consume =
                from = Some(self.parse_expression()?);

                // Expect TO, because we have a var and consumed =
                self.expect_kind(Kind::To, "Expected TO in DO loop")?;
                to = Some(self.parse_expression()?);

                // Optional BY
                if self.check(Kind::By) {
                    self.advance();
                    by = Some(self.parse_expression()?);
                }
            } else {
                // not a counting loop
                self.current = saved_pos;
            }
        }

        // check for WHILE
        if self.check(Kind::KwWhile) {
            self.advance();
            while_condition = Some(self.parse_expression()?);
        }

        self.expect_kind(Kind::Colon, "Expected ':' after DO")?;

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

    fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consumes IF

        // If "condition" THEN
        let condition = self.parse_expression()?;

        // Expect THEN
        self.expect_kind(Kind::Then, "Expected THEN after IF condition")?;

        // parse then branch, may be a DO block or single statement
        let then_branch = if self.check(Kind::Do) {
            self.parse_do_statement()?
        } else {
            self.parse_statement()?
        };

        // optional ELSE
        let else_branch = if self.check(Kind::KwElse) {
            self.advance();
            let else_stmt = if self.check(Kind::Do) {
                self.parse_do_statement()?
            } else if self.check(Kind::KwIf) {
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
        self.expect_kind(Kind::Colon, "Expected ':' after REPEAT")?;

        let body = self.parse_block_body()?;

        Ok(Statement::Repeat {
            while_condition,
            body,
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume RETURN

        // Check if there's a return value (not just a period)
        let value = if !self.check(Kind::Period) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_kind(Kind::Period, "Expected a '.' after RETURN")?;
        Ok(Statement::Return(value))
    }

    fn parse_for_each(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consume FOR
        self.expect_kind(Kind::Each, "Expected EACH after FOR")?;

        // Parse buffer name
        let buffer = self.parse_identifier()?;

        // optional OF clause
        let of_relation = if self.check(Kind::Of) {
            self.advance();
            Some(self.parse_identifier()?)
        } else {
            None
        };

        // optional WHERE clause
        let where_clause = if self.check(Kind::KwWhere) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Lock type (default is SHARE-LOCK if not explicit)
        let lock_type = self.parse_lock_type();

        self.expect_kind(Kind::Colon, "Expected ':' after FOR EACH")?;
        let body = self.parse_block_body()?;

        Ok(Statement::ForEach {
            buffer,
            of_relation,
            where_clause,
            lock_type,
            body,
        })
    }

    // parse find statements
    fn parse_find_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consume FIND

        // parse optional find type
        let find_type = match self.peek().kind {
            Kind::First => {
                self.advance();
                FindType::First
            }
            Kind::Last => {
                self.advance();
                FindType::Last
            }
            Kind::Next => {
                self.advance();
                FindType::Next
            }
            Kind::Prev => {
                self.advance();
                FindType::Prev
            }
            _ => FindType::Unique,
        };

        // parse buffer/table name
        let buffer = self.parse_identifier()?;

        // parse optional key-value (FIND customer <key> syntax,
        // equivalent to find customer where customer.primary-index field eq 1)
        // Key value is present if next token is NOT a clause keyword, lock type, or terminator.
        let key_value = if !self.is_find_clause_start() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // parse optional where clause
        let where_clause = if self.check(Kind::KwWhere) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // parse lock type, defaults to share lock if none is present
        let lock_type = self.parse_lock_type();

        // parse optional no error
        let no_error = if self.check(Kind::NoError) {
            self.advance();
            true
        } else {
            false
        };

        self.expect_kind(Kind::Period, "Expected '.' after FIND statement")?;

        Ok(Statement::Find {
            find_type,
            buffer,
            key_value,
            where_clause,
            lock_type,
            no_error,
        })
    }

    // Parse Case statement and when clauses
    fn parse_case_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume CASE
        let expression = self.parse_expression()?;
        self.expect_kind(Kind::Colon, "Expected a ':' after CASE expression")?;

        let mut when_branches = Vec::new();

        while self.check(Kind::When) {
            self.advance();
            // Use parse_and() instead of parse_expression() to avoid consuming OR
            // This allows WHEN "a" OR WHEN "b" syntax to work correctly
            let mut values = vec![self.parse_and()?];

            // handle WHEN "a" OR WHEN "b" syntax
            while self.check(Kind::Or) {
                self.advance();
                self.expect_kind(Kind::When, "Expected WHEN after OR")?;
                values.push(self.parse_and()?);
            }

            self.expect_kind(Kind::Then, "Expected THEN after WHEN value")?;

            // parse statements until next WHEN, OTHERWISE, or END
            let mut body = Vec::new();
            while !self.check(Kind::When) && !self.check(Kind::Otherwise) && !self.check(Kind::End)
            {
                body.push(self.parse_statement()?);
            }

            when_branches.push(WhenBranch { values, body });
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

        Ok(Statement::Case {
            expression,
            when_branches,
            otherwise,
        })
    }

    fn parse_procedure(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume PROCEDURE

        let name = self.parse_identifier()?;
        self.expect_kind(Kind::Colon, "Expected ':' after procedure name")?;

        // parse body until END
        let mut body = Vec::new();
        while !self.check(Kind::End) {
            body.push(self.parse_statement()?);
        }

        self.expect_kind(Kind::End, "Expected END at end of PROCEDURE body")?;

        // END PROCEDURE or just END. both are valid.
        if self.check(Kind::Procedure) {
            self.advance();
        }

        self.expect_kind(Kind::Period, "Expected '.' after END PROCEDURE")?;

        Ok(Statement::Procedure { name, body })
    }

    // parse RUN statements
    fn parse_run_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume RUN

        // Parse target: VALUE(expr), string literal, or procedure name
        let target = if self.check(Kind::Value) {
            self.advance();
            self.expect_kind(Kind::LeftParen, "Expected '(' after VALUE")?;
            let expr = self.parse_expression()?;
            self.expect_kind(Kind::RightParen, "Expected ')' after VALUE expression")?;
            RunTarget::Dynamic(expr)
        } else if self.check(Kind::StringLiteral) {
            // String literal target: RUN "my-proc.p".
            let token = self.advance().clone();
            let name = self.source[token.start + 1..token.end - 1].to_string();
            RunTarget::Literal(name)
        } else {
            // Procedure name (may contain hyphens, dots for .p/.w/.r/.i/.cls files)
            let name = self.parse_procedure_name()?;
            RunTarget::Literal(name)
        };

        // parse optional arguments
        let arguments = if self.check(Kind::LeftParen) {
            self.advance();
            let mut args = Vec::new();

            if !self.check(Kind::RightParen) {
                loop {
                    let direction = match self.peek().kind {
                        Kind::Input => {
                            self.advance();
                            ParameterDirection::Input
                        }
                        Kind::Output => {
                            self.advance();
                            ParameterDirection::Output
                        }
                        Kind::InputOutput => {
                            self.advance();
                            ParameterDirection::InputOutput
                        }
                        _ => ParameterDirection::Input, // Default to INPUT
                    };

                    let expression = self.parse_expression()?;
                    args.push(RunArgument {
                        direction,
                        expression,
                    });

                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance(); // consume comma
                }
            }

            self.expect_kind(Kind::RightParen, "Expected ')' after RUN arguments")?;
            args
        } else {
            Vec::new()
        };

        // parse optional IN handle
        let in_handle = if self.check(Kind::KwIn) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // parse optional PERSISTENT [SET handle]
        let (persistent, persistent_handle) = if self.check(Kind::Persistent) {
            self.advance();
            let h = if self.check(Kind::Set) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };
            (true, h)
        } else {
            (false, None)
        };

        // parse optional ASYNCHRONOUS [SET handle] [EVENT-PROCEDURE expr]
        let (asynchronous, async_handle, event_procedure) = if self.check(Kind::Asynchronous) {
            self.advance();
            let h = if self.check(Kind::Set) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };
            let ep = if self.check(Kind::EventProcedure) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };
            (true, h, ep)
        } else {
            (false, None, None)
        };

        // parse optional NO-ERROR
        let no_error = if self.check(Kind::NoError) {
            self.advance();
            true
        } else {
            false
        };

        self.expect_kind(Kind::Period, "Expected '.' after RUN statement")?;

        Ok(Statement::Run {
            target,
            arguments,
            in_handle,
            persistent,
            persistent_handle,
            asynchronous,
            async_handle,
            event_procedure,
            no_error,
        })
    }

    // Parse DISPLAY statement
    fn parse_display_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DISPLAY

        let mut items = Vec::new();
        let mut except = Vec::new();
        let mut frame = None;

        // Parse display items until WITH, EXCEPT, or period
        while !self.check(Kind::With)
            && !self.check(Kind::Except)
            && !self.check(Kind::Period)
            && !self.at_end()
        {
            let expression = self.parse_expression()?;

            // Optional per-item WHEN condition
            let when_condition = if self.check(Kind::When) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };

            // Skip FORMAT "string" and COLUMN-LABEL "string" if present (no variable refs)
            while self.check(Kind::Format) || self.check(Kind::ColumnLabel) {
                self.advance();
                if self.check(Kind::StringLiteral) {
                    self.advance();
                }
            }

            items.push(DisplayItem {
                expression,
                when_condition,
            });
        }

        // Parse optional EXCEPT clause
        if self.check(Kind::Except) {
            self.advance();
            while !self.check(Kind::With) && !self.check(Kind::Period) && !self.at_end() {
                except.push(self.parse_identifier()?);
            }
        }

        // Parse optional WITH FRAME clause
        if self.check(Kind::With) {
            self.advance();
            if self.check(Kind::Frame) {
                self.advance();
                frame = Some(self.parse_identifier()?);

                // Skip remaining frame options until period
                while !self.check(Kind::Period) && !self.at_end() {
                    self.advance();
                }
            } else {
                // WITH without FRAME — skip to period
                while !self.check(Kind::Period) && !self.at_end() {
                    self.advance();
                }
            }
        }

        self.expect_kind(Kind::Period, "Expected '.' after DISPLAY statement")?;

        Ok(Statement::Display {
            items,
            except,
            frame,
        })
    }

    // Parse MESSAGE statement
    fn parse_message_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume MESSAGE

        let mut items = Vec::new();
        let mut set_targets = Vec::new();

        // Parse message items until VIEW-AS, SET, UPDATE, or period
        while !self.check(Kind::ViewAs)
            && !self.check(Kind::Set)
            && !self.check(Kind::Update)
            && !self.check(Kind::Period)
            && !self.at_end()
        {
            // Recognize SKIP / SKIP(n) as formatting directives — don't treat as identifiers
            if self.check(Kind::Skip) {
                self.advance();
                // SKIP(n) — consume the parenthesized integer
                if self.check(Kind::LeftParen) {
                    self.advance();
                    if self.check(Kind::IntegerLiteral) {
                        self.advance();
                    }
                    if self.check(Kind::RightParen) {
                        self.advance();
                    }
                }
                continue; // SKIP has no variable refs, skip it
            }

            items.push(self.parse_expression()?);
        }

        // Parse optional VIEW-AS ALERT-BOX clause — skip over without failing
        if self.check(Kind::ViewAs) {
            self.advance(); // consume VIEW-AS
            // Skip tokens until we hit SET, UPDATE, or period
            while !self.check(Kind::Set)
                && !self.check(Kind::Update)
                && !self.check(Kind::Period)
                && !self.at_end()
            {
                self.advance();
            }
        }

        // Parse optional SET or UPDATE variable list
        if self.check(Kind::Set) || self.check(Kind::Update) {
            self.advance(); // consume SET or UPDATE
            // Parse variable names until period or another clause
            while !self.check(Kind::Period) && !self.at_end() {
                // Skip FORMAT "string" if present after a variable
                if self.check(Kind::Format) {
                    self.advance();
                    if self.check(Kind::StringLiteral) {
                        self.advance();
                    }
                    continue;
                }
                if Self::can_be_identifier(self.peek().kind) {
                    set_targets.push(self.parse_identifier()?);
                } else {
                    break;
                }
            }
        }

        self.expect_kind(Kind::Period, "Expected '.' after MESSAGE statement")?;

        Ok(Statement::Message { items, set_targets })
    }

    // Parse the block body for code blocks like DO, consume till END.
    fn parse_block_body(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.check(Kind::End) && !self.at_end() {
            statements.push(self.parse_statement()?);
        }

        // Consume the END
        self.expect_kind(Kind::End, "Expected END to close block")?;
        self.expect_kind(Kind::Period, "Expected '.' to end statement")?;

        Ok(statements)
    }

    /// Parses an optional lock type (NO-LOCK, SHARE-LOCK, EXCLUSIVE-LOCK)
    /// Returns ShareLock if no lock type is specified (ABL default)
    fn parse_lock_type(&mut self) -> LockType {
        match self.peek().kind {
            Kind::NoLock => {
                self.advance();
                LockType::NoLock
            }
            Kind::ShareLock => {
                self.advance();
                LockType::ShareLock
            }
            Kind::ExclusiveLock => {
                self.advance();
                LockType::ExclusiveLock
            }
            _ => LockType::ShareLock, // Default in ABL
        }
    }

    /// Parse a procedure name for RUN statements.
    ///
    /// ABL procedure names can contain hyphens (e.g., `calculate-total`) and may have
    /// file extensions (e.g., `my-proc.p`). Known ABL extensions are `.p`, `.w`, `.r`,
    /// `.i`, and `.cls`. A period followed by a non-extension token is treated as the
    /// statement terminator, not part of the name.
    fn parse_procedure_name(&mut self) -> ParseResult<String> {
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected procedure name after RUN".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let start = self.peek().start;
        self.advance(); // consume the first identifier token

        // Check for dotted extension (e.g., my-proc.p)
        // Only consume the dot + extension if it's a known ABL file extension
        if self.check(Kind::Period)
            && let Some(next) = self.tokens.get(self.current + 1)
            && next.kind == Kind::Identifier
        {
            let ext = &self.source[next.start..next.end];
            if ext.eq_ignore_ascii_case("p")
                || ext.eq_ignore_ascii_case("w")
                || ext.eq_ignore_ascii_case("r")
                || ext.eq_ignore_ascii_case("i")
                || ext.eq_ignore_ascii_case("cls")
            {
                self.advance(); // consume the period
                self.advance(); // consume the extension
            }
        }

        let end = self.tokens[self.current - 1].end;
        Ok(self.source[start..end].to_string())
    }

    /// Check if the current token is the start of a find clause (WHERE, lock, no-error, terminator)
    fn is_find_clause_start(&self) -> bool {
        matches!(
            self.peek().kind,
            Kind::KwWhere
                | Kind::NoLock
                | Kind::ShareLock
                | Kind::ExclusiveLock
                | Kind::NoError
                | Kind::Period
        )
    }
}
