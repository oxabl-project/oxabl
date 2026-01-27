//! Oxabl parser for navigating through our AST
//!
//! Will panic if peek or advance are called when current cursor position
//! is out of bounds. Contract is to check is_end before using them.

use oxabl_ast::{Expression, Identifier, Span};
use oxabl_lexer::{Kind, Token};

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
        Parser { tokens, source, current: 0 }
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
        self.parse_or()
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

        if !self.is_comparison_operator() {
            return Ok(left);
        }

        let op_kind = self.advance().kind;
        let right = self.parse_additive()?;

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
        self.parse_primary()
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

        // Identifiers
        if self.check(Kind::Identifier) {
            let token = self.advance();
            let start = token.start;
            let end = token.end;
            let name = self.source[start..end].to_string();
            return Ok(Expression::Identifier(Identifier {
                span: Span {
                    start: start as u32,
                    end: end as u32,
                },
                name,
            }));
        }

        Err(ParseError {
            message: format!("Unexpected token {:?}", self.peek().kind),
            span: Span {
                start: self.peek().start as u32,
                end: self.peek().end as u32,
            },
        })
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
        let source = "10 mod 3";
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
                    span: Span { start: 7, end: 8 },
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
}
