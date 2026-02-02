//! Expression parsing for the Oxabl parser

use oxabl_ast::{Expression, Identifier, Span};
use oxabl_lexer::{Kind, is_callable_kind};

use super::{ParseError, ParseResult, Parser};
use crate::literal::token_to_literal;

impl Parser<'_> {
    pub fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_ternary()
    }

    pub fn parse_ternary(&mut self) -> ParseResult<Expression> {
        if !self.check(Kind::KwIf) {
            return self.parse_or();
        }

        self.advance(); // consume IF
        let condition = self.parse_or()?; // condition can use OR/AND/comparison

        self.expect_kind(Kind::Then, "Expected 'THEN' after IF condition")?;

        let then_expr = self.parse_ternary()?; // recursive for nested ternary in then branch

        self.expect_kind(Kind::KwElse, "Expected 'ELSE' in IF expression")?;

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

    pub(super) fn is_comparison_operator(&self) -> bool {
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
        while self.check(Kind::Star) || self.check(Kind::Slash) || self.check(Kind::Modulo) {
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
                Kind::Modulo => {
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

        // Literals can't have postfix operations (member access, method calls, etc.)
        // Return early to avoid incorrectly parsing following tokens like ':' in "do i = 1 to 10:"
        if matches!(expr, Expression::Literal(_)) {
            return Ok(expr);
        }

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
            self.expect_kind(Kind::RightParen, "Expected ')' after method arguments")?;

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

        self.expect_kind(Kind::RightBracket, "Expected ']' after array index")?;

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
        println!("parsing primary");
        println!("token: {:?}", self.tokens[self.current]);

        // Parenthesized expression
        if self.check(Kind::LeftParen) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect_kind(Kind::RightParen, "Expected ')' after expression")?;
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

        self.expect_kind(Kind::RightParen, "Expected ')' after function arguments")?;

        Ok(Expression::FunctionCall { name, arguments })
    }
}
