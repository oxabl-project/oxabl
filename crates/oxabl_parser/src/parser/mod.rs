//! Oxabl parser for navigating through our AST
//!
//! Will panic if peek or advance are called when current cursor position
//! is out of bounds. Contract is to check is_end before using them.

pub mod expressions;
pub mod statements;
#[cfg(test)]
mod tests;

use oxabl_ast::{DataType, Identifier, Span};
use oxabl_lexer::{Kind, Token, is_callable_kind};

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

    /// Checks for a specific kind
    /// If not found, throws a ParseError with a message and span
    /// If available, advances the cursor
    fn expect_kind(&mut self, kind: Kind, msg: &str) -> ParseResult<()> {
        if !self.check(kind) {
            return Err(ParseError {
                message: msg.to_string(),
                span: self.current_span(),
            });
        }
        self.advance();
        Ok(())
    }

    /// Parses an Identifier
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
}
