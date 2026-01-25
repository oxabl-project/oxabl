//! Oxabl parser for navigating through our AST
//!
//! Will panic if peek or advance are called when current cursor position
//! is out of bounds. Contract is to check is_end before using them.

use oxabl_ast::{Expression, Span};
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
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        debug_assert!(!tokens.is_empty(), "Token slice must contain at least EOF");
        Parser { tokens, current: 0 }
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
        // parse 1 + 1
        // TODO - operator precedence
        let mut expr = self.parse_primary()?;
        // check for "+" operand
        while self.check(Kind::Add) {
            let _operator = self.advance();
            let right_exp = self.parse_primary()?;
            expr = Expression::Add(Box::new(expr), Box::new(right_exp));
        }
        Ok(expr)
    }

    pub fn parse_primary(&mut self) -> ParseResult<Expression> {
        if self.check(Kind::IntegerLiteral) {
            let token = self.advance();
            let literal = token_to_literal(token).ok_or_else(|| ParseError {
                message: format!("Failed to convert token to literal"),
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            })?;
            let expression = Expression::Literal(literal);
            return Ok(expression);
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
    use oxabl_ast::{IntegerLiteral, Literal, Span};
    use oxabl_lexer::tokenize;

    #[test]
    fn parse_simple_add_expression() {
        let tokens = tokenize("1 + 2");
        let mut parser = Parser::new(&tokens);
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
        let tokens = tokenize("1 + 2 + 3");
        let mut parser = Parser::new(&tokens);
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
}
