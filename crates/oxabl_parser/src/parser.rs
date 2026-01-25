//! Oxabl parser for navigating through our AST
//!
//! Will panic if peek or advance are called when current cursor position
//! is out of bounds. Contract is to check is_end before using them.
use oxabl_ast::Expression;
use oxabl_lexer::{Kind, Token};

use crate::literal::token_to_literal;

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

    pub fn parse_expression(&mut self) -> Expression {
        // parse 1 + 1
        // TODO - operator precedence
        let left_exp = self.parse_primary().expect("Expected an expression");
        // check for "+" operand
        if self.check(Kind::Add) { // TODO - change to a while to keep consuming operators
            let _operator = self.advance();
            let right_exp = self.parse_primary().expect("Expected an expression");
            return Expression::Add(Box::new(left_exp), Box::new(right_exp));
        } else {
            left_exp
        }
    }

    pub fn parse_primary(&mut self) -> Option<Expression> {
        if self.check(Kind::IntegerLiteral) {
            let token = self.advance();
            let literal = token_to_literal(token).expect("Thought I'd get a literal bro");
            let expression = Expression::Literal(literal);
            return Some(expression);
        } else {
            None
        }
    }

}

#[cfg(test)]
mod test {
    use oxabl_ast::{IntegerLiteral, Span};
    use oxabl_lexer::tokenize;
    use super::*;

    #[test]
    fn parse_add_expression(){
        let tokens = tokenize("1 + 1");
        let mut parser = Parser::new(&tokens);
        let expression = parser.parse_expression();
        assert_eq!(
            expression,
            Expression::Add(
                Box::new(Expression::Literal(
                    Literal::Integer(IntegerLiteral {
                        span: Span { start: 0, end: 1 },
                        value: 1
                    })
                )),
                Box::new(Expression::Literal(
                    Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 1
                    })
                ))
            )
        );
    }
}