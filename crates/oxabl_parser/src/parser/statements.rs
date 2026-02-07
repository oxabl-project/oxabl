//! Statement parsing for the Oxabl parser

use oxabl_ast::{
    Expression, FindType, Identifier, LockType, ParameterDirection, RunArgument, RunTarget, Span,
    Statement, WhenBranch,
};
use oxabl_lexer::{Kind, is_callable_kind};

use super::{ParseError, ParseResult, Parser};

impl Parser<'_> {
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        println!("current token: {:?}", self.tokens[self.current]);
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
        println!("left: {:?}", left);

        if self.check(Kind::Equals) {
            self.advance(); // consume the "="
            let value = self.parse_expression()?;
            println!("value: {:?}", value);
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

        // Variable
        if !self.check(Kind::Identifier) {
            return Err(ParseError {
                message: "Expected variable name after DEFINE".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let define_what = self.peek();
        let define_text = &self.source[define_what.start..define_what.end];

        if !define_text.eq_ignore_ascii_case("variable") && !define_text.eq_ignore_ascii_case("var")
        {
            return Err(ParseError {
                message: "Expected VARIABLE or VAR after DEFINE".to_string(),
                span: Span {
                    start: define_what.start as u32,
                    end: define_what.end as u32,
                },
            });
        }
        self.advance(); // consume VARIABLE or VAR

        // Name
        if !is_callable_kind(self.peek().kind) {
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
            if self.check(Kind::NoUndo) {
                self.advance();
                no_undo = true;
            } else if self.check(Kind::Identifier) {
                let token = self.peek();
                let text = &self.source[token.start..token.end];

                if text.eq_ignore_ascii_case("initial") || text.eq_ignore_ascii_case("init") {
                    self.advance(); // Consume init
                    initial_value = Some(self.parse_expression()?);
                } else if text.eq_ignore_ascii_case("extent") {
                    self.advance(); // Consume extent

                    // Extent can be followed by number or nothing (dynamic)
                    if self.check(Kind::IntegerLiteral) {
                        let ext_token = self.advance().clone();
                        if let Ok(n) = self.source[ext_token.start..ext_token.end].parse::<u32>() {
                            extent = Some(n);
                        } else {
                            extent = Some(0); // dynamic
                        }
                    } // extent check if it's set or dynamic
                } else {
                    // check for initial or extent
                    break; // not intial or extent, exit loop
                }
            } else {
                break; // not an identifier, exit loop
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
        if !is_callable_kind(self.peek().kind) {
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
        if !is_callable_kind(self.peek().kind) {
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

        Ok(Statement::DefineParamter {
            direction,
            name,
            data_type,
            no_undo,
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
        if is_callable_kind(self.peek().kind) {
            // peek ahead to see if this is 'var = start to end'
            let saved_pos = self.current;
            let potential_var = self.advance().clone();
            println!("Potential var: {:?}", potential_var);

            if self.check(Kind::Equals) {
                println!("Equals found");
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
                println!("From parsed: {:?}", from);

                // Expect TO, because we have a var and consume  =
                self.expect_kind(Kind::To, "Expected TO in DO loop")?;
                to = Some(self.parse_expression()?);
                println!("To parsed: {:?}", to);

                // Optional BY
                if self.check(Kind::By) {
                    self.advance();
                    by = Some(self.parse_expression()?);
                }
            } else {
                println!("Not a counting loop");
                // not a counting loop
                self.current = saved_pos;
            }
        }

        println!("Current token: {:?}", self.tokens[self.current]);

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

        // Expect collor
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
        println!("current token before lock: {:?}", self.peek());
        let lock_type = self.parse_lock_type();
        println!("lock type: {:?}", lock_type);

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

        let target = if self.check(Kind::Value) {
            self.advance();
            self.expect_kind(Kind::LeftParen, "Expected '(' after VALUE")?;
            let expr = self.parse_expression()?;
            self.expect_kind(Kind::RightParen, "Expected ')' after VALUE expression")?;
            RunTarget::Dynamic(expr)
        } else {
            // Procedure name (may contain hyphens, dots for .p files)
            let name = self.parse_procedure_name()?;
            RunTarget::Literal(name)
        };

        // parse optional arguments
        let arguments = if self.check(Kind::LeftParen) {
            self.advance();
            let mut args = Vec::new();

            if !self.check(Kind::RightParen) {
                loop {
                    // TODO - finish, add tests, make not broken.
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

            self.expect_kind(Kind::RightParen, "Expected ')' after argument statement")?;
            args
        } else {
            Vec::new()
        };

        self.expect_kind(Kind::Period, "Expected ',' after RUN statement")?;

        Ok(Statement::Run { target, arguments })
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

    // Helper to parse procedure names that may include dots (e.g., "file.p")
    fn parse_procedure_name(&mut self) -> ParseResult<String> {
        let start = self.peek().start;
        self.advance(); // consume first identifier

        // Handle dotted names like "myproc.p"
        while self.check(Kind::Period) {
            // Peek ahead - if next is an identifier, it's part of the name
            let saved = self.current;
            self.advance(); // consume period
            if self.check(Kind::Identifier) {
                self.advance(); // consume extension
            } else {
                // It's the statement terminator, rollback
                self.current = saved;
                break;
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
