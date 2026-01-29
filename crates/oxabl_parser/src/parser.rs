//! Oxabl parser for navigating through our AST
//!
//! Will panic if peek or advance are called when current cursor position
//! is out of bounds. Contract is to check is_end before using them.

use oxabl_ast::{DataType, Expression, Identifier, Span, Statement};
use oxabl_lexer::{Kind, Token, is_callable_kind};

use crate::literal::token_to_literal;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    source: &'a str,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], source: &'a str) -> Self {
        debug_assert!(!tokens.is_empty(), "Token slice must contain at least EOF");
        Parser {
            tokens,
            source,
            current: 0,
        }
    }
    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.current];
        self.current += 1;
        token
    }

    pub fn check(&self, kind: Kind) -> bool {
        self.tokens
            .get(self.current)
            .is_some_and(|t| t.kind == kind)
    }

    pub fn at_end(&self) -> bool {
        self.check(Kind::Eof)
    }

    pub fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_ternary()
    }

    pub fn parse_ternary(&mut self) -> ParseResult<Expression> {
        if !self.check(Kind::KwIf) {
            return self.parse_or();
        }

        self.advance(); // consume IF
        let condition = self.parse_or()?; // condition can use OR/AND/comparison

        if !self.check(Kind::Then) {
            return Err(ParseError {
                message: "Expected 'THEN' after IF condition".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance(); // consume THEN

        let then_expr = self.parse_ternary()?; // recursive for nested ternary in then branch

        if !self.check(Kind::KwElse) {
            return Err(ParseError {
                message: "Expected 'ELSE' in IF expression".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance(); // consume ELSE

        let else_expr = self.parse_ternary()?; // recursive for nested ternary in else branch

        Ok(Expression::IfThenElse(
            Box::new(condition),
            Box::new(then_expr),
            Box::new(else_expr),
        ))
    }

    pub fn parse_or(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_and()?;
        while self.check(Kind::Or) {
            self.advance();
            let right = self.parse_and()?;
            expr = Expression::Or(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    pub fn parse_and(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_comparison()?;
        while self.check(Kind::And) {
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expression::And(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn is_comparison_operator(&self) -> bool {
        println!("self.peek().kind: {:?}", self.peek().kind);
        matches!(
            self.peek().kind,
            Kind::Equals
                | Kind::NotEqual
                | Kind::LessThan
                | Kind::LessThanOrEqual
                | Kind::GreaterThan
                | Kind::GreaterThanOrEqual
                | Kind::Eq
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

    pub fn parse_comparison(&mut self) -> ParseResult<Expression> {
        let left = self.parse_additive()?;
        println!("left: {:?}", left);

        if !self.is_comparison_operator() {
            println!("not a comparison operator");
            return Ok(left);
        }

        let op_kind = self.advance().kind;
        println!("op_kind: {:?}", op_kind);
        let right = self.parse_additive()?;
        println!("right: {:?}", right);

        let expr = match op_kind {
            Kind::Equals | Kind::Eq => Expression::Equal(Box::new(left), Box::new(right)),
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
        };

        Ok(expr)
    }

    pub fn parse_additive(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_multiplicative()?;
        while self.check(Kind::Add) || self.check(Kind::Minus) {
            let operator = self.advance();
            match operator.kind {
                Kind::Add => {
                    let right_exp = self.parse_multiplicative()?;
                    expr = Expression::Add(Box::new(expr), Box::new(right_exp));
                }
                Kind::Minus => {
                    let right_exp = self.parse_multiplicative()?;
                    expr = Expression::Minus(Box::new(expr), Box::new(right_exp));
                }
                _ => unreachable!(),
            }
        }
        Ok(expr)
    }

    pub fn parse_multiplicative(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_unary()?;
        while self.check(Kind::Star) || self.check(Kind::Slash) || self.check(Kind::Percent) {
            let operator = self.advance();
            match operator.kind {
                Kind::Star => {
                    let right_exp = self.parse_unary()?;
                    expr = Expression::Multiply(Box::new(expr), Box::new(right_exp));
                }
                Kind::Slash => {
                    let right_exp = self.parse_unary()?;
                    expr = Expression::Divide(Box::new(expr), Box::new(right_exp));
                }
                Kind::Percent => {
                    let right_exp = self.parse_unary()?;
                    expr = Expression::Modulo(Box::new(expr), Box::new(right_exp));
                }
                _ => unreachable!(),
            }
        }
        Ok(expr)
    }

    pub fn parse_unary(&mut self) -> ParseResult<Expression> {
        if self.check(Kind::Minus) {
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expression::Negate(Box::new(expr)));
        }
        if self.check(Kind::Not) {
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expression::Not(Box::new(expr)));
        }
        self.parse_postfix()
    }

    pub fn parse_postfix(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.check(Kind::Colon) {
                expr = self.parse_member_or_method(expr)?;
            } else if self.check(Kind::LeftBracket) {
                expr = self.parse_array_access(expr)?;
            } else if self.check(Kind::Period)
                && self.is_field_access_ahead()
                && Self::can_have_field_access(&expr)
            {
                expr = self.parse_field_access(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub fn parse_member_or_method(&mut self, object: Expression) -> ParseResult<Expression> {
        self.advance(); // consumes ':'

        // Expect identifier after ':'
        if !is_callable_kind(self.peek().kind) {
            return Err(ParseError {
                message: format!(
                    "Expected identifier after ':', found {:?}",
                    self.peek().kind
                ),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let token = self.advance().clone();
        let member = Identifier {
            span: Span {
                start: token.start as u32,
                end: token.end as u32,
            },
            name: self.source[token.start..token.end].to_string(),
        };

        // Check for method call: member followed by (
        if self.check(Kind::LeftParen) {
            self.advance(); // Consume '('

            // consume and store all arguments
            let mut arguments = Vec::new();
            if !self.check(Kind::RightParen) {
                arguments.push(self.parse_expression()?);

                while self.check(Kind::Comma) {
                    self.advance(); // Consume ','
                    arguments.push(self.parse_expression()?);
                }
            }

            // if after parsing all arguments we don't find the
            // closing ), throw error
            if !self.check(Kind::RightParen) {
                return Err(ParseError {
                    message: "Expected ')' after method arguments".to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            }
            self.advance(); // consume ')'

            return Ok(Expression::MethodCall {
                object: Box::new(object),
                method: member,
                arguments,
            });
        }

        Ok(Expression::MemberAccess {
            object: Box::new(object),
            member,
        })
    }

    pub fn parse_array_access(&mut self, array: Expression) -> ParseResult<Expression> {
        self.advance(); // consume the '['

        let index = self.parse_expression()?;

        if !self.check(Kind::RightBracket) {
            return Err(ParseError {
                message: "Expected ']' after array index".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance(); // consume ']'

        Ok(Expression::ArrayAccess {
            array: Box::new(array),
            index: Box::new(index),
        })
    }

    /// Check if we're looking at field access, rather that a statement terminator
    pub fn is_field_access_ahead(&mut self) -> bool {
        // skip if it's not a period
        if !self.check(Kind::Period) {
            return false;
        }

        // return true if there is an identifer after the period
        self.tokens
            .get(self.current + 1)
            .is_some_and(|t| t.kind == Kind::Identifier)
    }

    /// Check if an expression can be the base of field access (Table.Field)
    fn can_have_field_access(expr: &Expression) -> bool {
        matches!(
            expr,
            Expression::Identifier(_) | Expression::FieldAccess { .. }
        )
    }

    pub fn parse_field_access(&mut self, qualifier: Expression) -> ParseResult<Expression> {
        self.advance(); // consume '.'

        // Expect identifier after '.'
        if !self.check(Kind::Identifier) {
            return Err(ParseError {
                message: "Expected field name after '.'".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let token = self.advance().clone();
        let field = Identifier {
            span: Span {
                start: token.start as u32,
                end: token.end as u32,
            },
            name: self.source[token.start..token.end].to_string(),
        };

        Ok(Expression::FieldAccess {
            qualifier: Box::new(qualifier),
            field,
        })
    }

    pub fn parse_primary(&mut self) -> ParseResult<Expression> {
        // Parenthesized expression
        if self.check(Kind::LeftParen) {
            self.advance();
            let expr = self.parse_expression()?;
            if !self.check(Kind::RightParen) {
                return Err(ParseError {
                    message: "Expected ')' after expression".to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            }
            self.advance();
            return Ok(expr);
        }

        // Literals
        if self.check(Kind::IntegerLiteral)
            || self.check(Kind::DecimalLiteral)
            || self.check(Kind::StringLiteral)
            || self.check(Kind::KwTrue)
            || self.check(Kind::KwFalse)
            || self.check(Kind::Question)
        {
            let token = self.advance();
            let literal = token_to_literal(token).ok_or_else(|| ParseError {
                message: "Failed to convert token to literal".to_string(),
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            })?;
            return Ok(Expression::Literal(literal));
        }

        // Identifiers and callable keywords (built-in functions like NOW, TRIM, etc.)
        if is_callable_kind(self.peek().kind) {
            let token = self.advance();
            let start = token.start;
            let end = token.end;
            let name = self.source[start..end].to_string();
            let identifier = Identifier {
                span: Span {
                    start: start as u32,
                    end: end as u32,
                },
                name,
            };

            // Check for function call: identifier/callable followed by (
            if self.check(Kind::LeftParen) {
                return self.parse_function_call(identifier);
            }

            return Ok(Expression::Identifier(identifier));
        }

        Err(ParseError {
            message: format!("Unexpected token {:?}", self.peek().kind),
            span: Span {
                start: self.peek().start as u32,
                end: self.peek().end as u32,
            },
        })
    }

    pub fn parse_function_call(&mut self, name: Identifier) -> ParseResult<Expression> {
        self.advance(); // consume the left parenthesis

        let mut arguments = Vec::new();

        // Empty argument list
        if !self.check(Kind::RightParen) {
            // parse first argument
            arguments.push(self.parse_expression()?);

            // parse remaining
            while self.check(Kind::Comma) {
                self.advance(); // consume ','
                arguments.push(self.parse_expression()?);
            }
        }

        if !self.check(Kind::RightParen) {
            return Err(ParseError {
                message: "Expected ')' after function arguments".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        self.advance(); // consume the right parenthesis

        Ok(Expression::FunctionCall { name, arguments })
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        println!("current token: {:?}", self.tokens[self.current]);
        // Skip empty statements
        if self.check(Kind::Period) {
            self.advance();
            return Ok(Statement::Empty);
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
            self.expect_period()?;
            return Ok(Statement::Assignment {
                target: left,
                value,
            });
        }

        // not an assignment, continue parsing as full expression
        let expr = self.finish_expression(left)?;
        self.expect_period()?;
        Ok(Statement::ExpressionStatement(expr))
    }

    // parse define variable as type [no-undo] [initial]
    fn parse_define_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DEFINE

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
        // buffer
        // todo
        // temp-table
        // todo

        let define_what = self.peek();
        let define_text = &self.source[define_what.start..define_what.end];

        if !define_text.eq_ignore_ascii_case("variable") &&
        !define_text.eq_ignore_ascii_case("var") {
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

                if text.eq_ignore_ascii_case("initial") ||
                text.eq_ignore_ascii_case("init") {
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
                } else { // check for initial or extent
                    break; // not intial or extent, exit loop
                }
            } else {
                break; // not an identifier, exit loop
            }
        }

        self.expect_period()?;

        Ok(Statement::VariableDeclaration { name, data_type, initial_value, no_undo, extent })

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

        self.expect_period()?;

        Ok(Statement::VariableDeclaration {
            name,
            data_type,
            initial_value,
            no_undo: true, // VAR implies NO-UNDO
            extent: None,
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
            // ... etc for other operators
            _ => unreachable!(),
        }
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
                })
            }
        };

        self.advance();
        Ok(data_type)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use oxabl_ast::{
        BooleanLiteral, DecimalLiteral, IntegerLiteral, Literal, Span, StringLiteral,
        UnknownLiteral,
    };
    use oxabl_lexer::tokenize;
    use rust_decimal::Decimal;
    use std::str::FromStr;

    #[test]
    fn parse_simple_add_expression() {
        let source = "1 + 2";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )
        );
    }

    #[test]
    fn parse_double_add_expression() {
        let source = "1 + 2 + 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::Add(
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 0, end: 1 },
                        value: 1
                    }))),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 2
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                })))
            )
        );
    }

    #[test]
    fn parse_simple_minus_expression() {
        let source = "1 - 2";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Minus(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )
        );
    }

    #[test]
    fn parse_double_minus_expression() {
        let source = "1 - 2 - 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Minus(
                Box::new(Expression::Minus(
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 0, end: 1 },
                        value: 1
                    }))),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 2
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                })))
            )
        );
    }

    #[test]
    fn parse_add_minus_expression() {
        let source = "1 + 2 - 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Minus(
                Box::new(Expression::Add(
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 0, end: 1 },
                        value: 1
                    }))),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 2
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                })))
            )
        );
    }

    #[test]
    fn parse_simple_multiplication_expression() {
        let source = "1 * 2";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Multiply(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )
        );
    }

    #[test]
    fn parse_add_multiplication_expression() {
        let source = "1 + 2 * 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        println!("{:?}", expression);
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 2
                    }))),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 8, end: 9 },
                        value: 3
                    })))
                )),
            )
        );
    }

    #[test]
    fn parse_simple_division_expression() {
        let source = "6 / 2";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Divide(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 6
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )
        );
    }

    #[test]
    fn parse_parenthesized_expression() {
        let source = "(1 + 2)";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 1, end: 2 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 5, end: 6 },
                    value: 2
                })))
            )
        );
    }

    #[test]
    fn parse_parentheses_override_precedence() {
        let source = "(1 + 2) * 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Multiply(
                Box::new(Expression::Add(
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 1, end: 2 },
                        value: 1
                    }))),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 5, end: 6 },
                        value: 2
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 10, end: 11 },
                    value: 3
                })))
            )
        );
    }

    #[test]
    fn parse_unary_negate() {
        let source = "-5";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Negate(Box::new(Expression::Literal(Literal::Integer(
                IntegerLiteral {
                    span: Span { start: 1, end: 2 },
                    value: 5
                }
            ))))
        );
    }

    #[test]
    fn parse_double_negate() {
        let source = "--5";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Negate(Box::new(Expression::Negate(Box::new(Expression::Literal(
                Literal::Integer(IntegerLiteral {
                    span: Span { start: 2, end: 3 },
                    value: 5
                })
            )))))
        );
    }

    #[test]
    fn parse_unary_not() {
        let source = "not true";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Not(Box::new(Expression::Literal(Literal::Boolean(
                BooleanLiteral {
                    span: Span { start: 4, end: 8 },
                    value: true
                }
            ))))
        );
    }

    #[test]
    fn parse_decimal_literal() {
        let source = "3.14";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Literal(Literal::Decimal(DecimalLiteral {
                span: Span { start: 0, end: 4 },
                value: Decimal::from_str("3.14").unwrap()
            }))
        );
    }

    #[test]
    fn parse_string_literal() {
        let source = "\"hello\"";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Literal(Literal::String(StringLiteral {
                span: Span { start: 0, end: 7 },
                value: "hello".to_string()
            }))
        );
    }

    #[test]
    fn parse_boolean_literals() {
        let source = "true";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Literal(Literal::Boolean(BooleanLiteral {
                span: Span { start: 0, end: 4 },
                value: true
            }))
        );
    }

    #[test]
    fn parse_unknown_literal() {
        let source = "?";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Literal(Literal::Unknown(UnknownLiteral {
                span: Span { start: 0, end: 1 }
            }))
        );
    }

    #[test]
    fn parse_modulo_expression() {
        // ABL uses % for modulo, not 'mod'
        let source = "10 % 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Modulo(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 2 },
                    value: 10
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 5, end: 6 },
                    value: 3
                })))
            )
        );
    }

    #[test]
    fn parse_identifier() {
        let source = "myVar";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Identifier(Identifier {
                span: Span { start: 0, end: 5 },
                name: "myVar".to_string()
            })
        );
    }

    #[test]
    fn parse_hyphenated_identifier() {
        let source = "my-hyphenated-var";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Identifier(Identifier {
                span: Span { start: 0, end: 17 },
                name: "my-hyphenated-var".to_string()
            })
        );
    }

    #[test]
    fn parse_equal_expression() {
        let source = "a = b";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Equal(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "b".to_string()
                }))
            )
        );
    }

    #[test]
    fn parse_not_equal_expression() {
        let source = "a <> b";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::NotEqual(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                }))
            )
        );
    }

    #[test]
    fn parse_less_than_keyword() {
        let source = "a lt b";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::LessThan(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                }))
            )
        );
    }

    #[test]
    fn parse_begins_expression() {
        let source = "userName begins \"Jo\"";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Begins(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 8 },
                    name: "userName".to_string()
                })),
                Box::new(Expression::Literal(Literal::String(StringLiteral {
                    span: Span { start: 16, end: 20 },
                    value: "Jo".to_string()
                })))
            )
        );
    }

    #[test]
    fn parse_contains_expression() {
        let source = "content contains \"abc\"";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Contains(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 7 },
                    name: "content".to_string()
                })),
                Box::new(Expression::Literal(Literal::String(StringLiteral {
                    span: Span { start: 17, end: 22 },
                    value: "abc".to_string()
                })))
            )
        );
    }

    #[test]
    fn parse_and_expression() {
        let source = "a and b";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::And(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 6, end: 7 },
                    name: "b".to_string()
                }))
            )
        );
    }

    #[test]
    fn parse_or_expression() {
        let source = "a or b";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Or(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                }))
            )
        );
    }

    #[test]
    fn parse_or_and_precedence() {
        // a or b and c should parse as a or (b and c)
        let source = "a or b and c";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Or(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })),
                Box::new(Expression::And(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 5, end: 6 },
                        name: "b".to_string()
                    })),
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 11, end: 12 },
                        name: "c".to_string()
                    }))
                ))
            )
        );
    }

    #[test]
    fn parse_simple_ternary() {
        // IF xx > 5 THEN 10 ELSE 20
        let source = "if xx > 5 then 10 else 20";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::IfThenElse(
                Box::new(Expression::GreaterThan(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 3, end: 5 },
                        name: "xx".to_string()
                    })),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 8, end: 9 },
                        value: 5
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 15, end: 17 },
                    value: 10
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 23, end: 25 },
                    value: 20
                })))
            )
        );
    }

    #[test]
    fn parse_nested_ternary_in_else() {
        // IF xx > 10 THEN 1 ELSE IF xx > 5 THEN 2 ELSE 3
        let source = "if xx > 10 then 1 else if xx > 5 then 2 else 3";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::IfThenElse(
                Box::new(Expression::GreaterThan(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 3, end: 5 },
                        name: "xx".to_string()
                    })),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 8, end: 10 },
                        value: 10
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 16, end: 17 },
                    value: 1
                }))),
                Box::new(Expression::IfThenElse(
                    Box::new(Expression::GreaterThan(
                        Box::new(Expression::Identifier(Identifier {
                            span: Span { start: 26, end: 28 },
                            name: "xx".to_string()
                        })),
                        Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 31, end: 32 },
                            value: 5
                        })))
                    )),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 38, end: 39 },
                        value: 2
                    }))),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 45, end: 46 },
                        value: 3
                    })))
                ))
            )
        );
    }

    #[test]
    fn parse_ternary_with_complex_condition() {
        // IF a = b AND c > d THEN 1 ELSE 0
        let source = "if a = b and c > d then 1 else 0";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::IfThenElse(
                Box::new(Expression::And(
                    Box::new(Expression::Equal(
                        Box::new(Expression::Identifier(Identifier {
                            span: Span { start: 3, end: 4 },
                            name: "a".to_string()
                        })),
                        Box::new(Expression::Identifier(Identifier {
                            span: Span { start: 7, end: 8 },
                            name: "b".to_string()
                        }))
                    )),
                    Box::new(Expression::GreaterThan(
                        Box::new(Expression::Identifier(Identifier {
                            span: Span { start: 13, end: 14 },
                            name: "c".to_string()
                        })),
                        Box::new(Expression::Identifier(Identifier {
                            span: Span { start: 17, end: 18 },
                            name: "d".to_string()
                        }))
                    ))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 24, end: 25 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 31, end: 32 },
                    value: 0
                })))
            )
        );
    }

    #[test]
    fn parse_ternary_with_expressions_in_branches() {
        // IF cond THEN xx + yy ELSE xx - yy
        let source = "if cond then xx + yy else xx - yy";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::IfThenElse(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 3, end: 7 },
                    name: "cond".to_string()
                })),
                Box::new(Expression::Add(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 13, end: 15 },
                        name: "xx".to_string()
                    })),
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 18, end: 20 },
                        name: "yy".to_string()
                    }))
                )),
                Box::new(Expression::Minus(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 26, end: 28 },
                        name: "xx".to_string()
                    })),
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 31, end: 33 },
                        name: "yy".to_string()
                    }))
                ))
            )
        );
    }

    #[test]
    fn parse_function_call_no_args() {
        let source = "NOW()";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "NOW".to_string()
                },
                arguments: vec![]
            }
        );
    }

    #[test]
    fn parse_function_call_1_arg() {
        let source = "TRIM(name)";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 4 },
                    name: "TRIM".to_string()
                },
                arguments: vec![Expression::Identifier(Identifier {
                    span: Span { start: 5, end: 9 },
                    name: "name".to_string()
                })]
            }
        );
    }

    #[test]
    fn parse_function_call_multiple_args() {
        let source = "SUBSTRING(str, 1, 5)";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 9 },
                    name: "SUBSTRING".to_string()
                },
                arguments: vec![
                    Expression::Identifier(Identifier {
                        span: Span { start: 10, end: 13 },
                        name: "str".to_string()
                    }),
                    Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 15, end: 16 },
                        value: 1
                    })),
                    Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 18, end: 19 },
                        value: 5
                    }))
                ]
            }
        );
    }

    #[test]
    fn parse_function_call_with_expression_arg() {
        let source = "ABS(x - 5)";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "ABS".to_string()
                },
                arguments: vec![Expression::Minus(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 4, end: 5 },
                        name: "x".to_string()
                    })),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 8, end: 9 },
                        value: 5
                    })))
                )]
            }
        );
    }

    #[test]
    fn parse_nested_function_calls() {
        let source = "TRIM(SUBSTRING(name, 1, 5))";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 4 },
                    name: "TRIM".to_string()
                },
                arguments: vec![Expression::FunctionCall {
                    name: Identifier {
                        span: Span { start: 5, end: 14 },
                        name: "SUBSTRING".to_string()
                    },
                    arguments: vec![
                        Expression::Identifier(Identifier {
                            span: Span { start: 15, end: 19 },
                            name: "name".to_string()
                        }),
                        Expression::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 21, end: 22 },
                            value: 1
                        })),
                        Expression::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 24, end: 25 },
                            value: 5
                        }))
                    ]
                }]
            }
        );
    }

    #[test]
    fn parse_function_in_expression() {
        let source = "LENGTH(str) + 1";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::FunctionCall {
                    name: Identifier {
                        span: Span { start: 0, end: 6 },
                        name: "LENGTH".to_string()
                    },
                    arguments: vec![Expression::Identifier(Identifier {
                        span: Span { start: 7, end: 10 },
                        name: "str".to_string()
                    })]
                }),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 1
                })))
            )
        );
    }

    #[test]
    fn parse_simple_array_access() {
        let source = "arr[1]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "arr".to_string()
                })),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 1
                })))
            }
        )
    }

    #[test]
    fn parse_array_access_with_expression_index() {
        let source = "arr[i + 1]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "arr".to_string()
                })),
                index: Box::new(Expression::Add(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 4, end: 5 },
                        name: "i".to_string()
                    })),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 8, end: 9 },
                        value: 1
                    })))
                ))
            }
        );
    }

    #[test]
    fn parse_hyphenated_array_access() {
        let source = "month-quota[3]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 11 },
                    name: "month-quota".to_string()
                })),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 12, end: 13 },
                    value: 3
                })))
            }
        );
    }

    #[test]
    fn parse_multidimensional_array() {
        let source = "matrix[row][col]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::ArrayAccess {
                    array: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 6 },
                        name: "matrix".to_string()
                    })),
                    index: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 7, end: 10 },
                        name: "row".to_string()
                    }))
                }),
                index: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 12, end: 15 },
                    name: "col".to_string()
                }))
            }
        );
    }

    #[test]
    fn parse_array_in_arithmetic() {
        let source = "arr[1] + arr[2]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::ArrayAccess {
                    array: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 3 },
                        name: "arr".to_string()
                    })),
                    index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 1
                    })))
                }),
                Box::new(Expression::ArrayAccess {
                    array: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 9, end: 12 },
                        name: "arr".to_string()
                    })),
                    index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 13, end: 14 },
                        value: 2
                    })))
                })
            )
        );
    }

    #[test]
    fn parse_simple_member_access() {
        let source = "handle:PRIVATE-DATA";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::MemberAccess {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "handle".to_string()
                })),
                member: Identifier {
                    span: Span { start: 7, end: 19 },
                    name: "PRIVATE-DATA".to_string()
                }
            }
        );
    }

    #[test]
    fn parse_simple_method_call() {
        let source = "buffer:FIND-FIRST()";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::MethodCall {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "buffer".to_string()
                })),
                method: Identifier {
                    span: Span { start: 7, end: 17 },
                    name: "FIND-FIRST".to_string()
                },
                arguments: vec![]
            }
        );
    }

    #[test]
    fn parse_method_call_with_args() {
        let source = "widget:MOVE(10, 20)";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::MethodCall {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "widget".to_string()
                })),
                method: Identifier {
                    span: Span { start: 7, end: 11 },
                    name: "MOVE".to_string()
                },
                arguments: vec![
                    Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 12, end: 14 },
                        value: 10
                    })),
                    Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 16, end: 18 },
                        value: 20
                    }))
                ]
            }
        );
    }

    #[test]
    fn parse_chained_member_access() {
        let source = "obj:property:nested";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::MemberAccess {
                object: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 3 },
                        name: "obj".to_string()
                    })),
                    member: Identifier {
                        span: Span { start: 4, end: 12 },
                        name: "property".to_string()
                    }
                }),
                member: Identifier {
                    span: Span { start: 13, end: 19 },
                    name: "nested".to_string()
                }
            }
        );
    }

    #[test]
    fn parse_member_then_method() {
        let source = "obj:prop:method()";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::MethodCall {
                object: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 3 },
                        name: "obj".to_string()
                    })),
                    member: Identifier {
                        span: Span { start: 4, end: 8 },
                        name: "prop".to_string()
                    }
                }),
                method: Identifier {
                    span: Span { start: 9, end: 15 },
                    name: "method".to_string()
                },
                arguments: vec![]
            }
        );
    }

    #[test]
    fn parse_member_access_with_array() {
        let source = "myBuf:myField[1]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 5 },
                        name: "myBuf".to_string()
                    })),
                    member: Identifier {
                        span: Span { start: 6, end: 13 },
                        name: "myField".to_string()
                    }
                }),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 1
                })))
            }
        );
    }

    #[test]
    fn parse_member_in_expression() {
        let source = "myObj:myVal + 5";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 5 },
                        name: "myObj".to_string()
                    })),
                    member: Identifier {
                        span: Span { start: 6, end: 11 },
                        name: "myVal".to_string()
                    }
                }),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 5
                })))
            )
        );
    }

    #[test]
    fn parse_simple_field_access() {
        let source = "Customer.CustNum";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FieldAccess {
                qualifier: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 8 },
                    name: "Customer".to_string()
                })),
                field: Identifier {
                    span: Span { start: 9, end: 16 },
                    name: "CustNum".to_string()
                }
            }
        );
    }

    #[test]
    fn parse_qualified_field_access() {
        let source = "Sports2020.Customer.Name";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::FieldAccess {
                qualifier: Box::new(Expression::FieldAccess {
                    qualifier: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 10 },
                        name: "Sports2020".to_string()
                    })),
                    field: Identifier {
                        span: Span { start: 11, end: 19 },
                        name: "Customer".to_string()
                    }
                }),
                field: Identifier {
                    span: Span { start: 20, end: 24 },
                    name: "Name".to_string()
                }
            }
        );
    }

    #[test]
    fn parse_field_access_with_array() {
        let source = "myTable.myField[extent]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::FieldAccess {
                    qualifier: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 7 },
                        name: "myTable".to_string()
                    })),
                    field: Identifier {
                        span: Span { start: 8, end: 15 },
                        name: "myField".to_string()
                    }
                }),
                index: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 16, end: 22 },
                    name: "extent".to_string()
                }))
            }
        );
    }

    #[test]
    fn parse_field_in_comparison() {
        let source = "Customer.Balance > 1000";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::GreaterThan(
                Box::new(Expression::FieldAccess {
                    qualifier: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 8 },
                        name: "Customer".to_string()
                    })),
                    field: Identifier {
                        span: Span { start: 9, end: 16 },
                        name: "Balance".to_string()
                    }
                }),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 19, end: 23 },
                    value: 1000
                })))
            )
        );
    }

    #[test]
    fn parse_mixed_access_operators() {
        // Combines field access, member access, and array indexing
        let source = "myDb.myTable:buffer[1]";
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let expression = parser.parse_expression().expect("Expected an expression");
        assert_eq!(
            expression,
            Expression::ArrayAccess {
                array: Box::new(Expression::MemberAccess {
                    object: Box::new(Expression::FieldAccess {
                        qualifier: Box::new(Expression::Identifier(Identifier {
                            span: Span { start: 0, end: 4 },
                            name: "myDb".to_string()
                        })),
                        field: Identifier {
                            span: Span { start: 5, end: 12 },
                            name: "myTable".to_string()
                        }
                    }),
                    member: Identifier {
                        span: Span { start: 13, end: 19 },
                        name: "buffer".to_string()
                    }
                }),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 20, end: 21 },
                    value: 1
                })))
            }
        );
    }

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
        let source = "buffer:save-row-changes().";
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
                    span: Span { start: 7, end: 23 },
                    name: "save-row-changes".to_string()
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
}
