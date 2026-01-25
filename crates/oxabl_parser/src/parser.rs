//! Oxabl parser for navigating through our AST
//!
//! Will panic if peek or advance are called when current cursor position
//! is out of bounds. Contract is to check is_end before using them.
use oxabl_lexer::{Kind, Token};

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
}
