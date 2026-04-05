//! Recursive-descent parser for ABL source code.
//!
//! The parser walks a token slice using a cursor. Expression parsing uses
//! precedence climbing (see [`expressions`]). Statement dispatch uses
//! keyword-based if/else chains (see [`statements`]).
//!
//! Will panic if `peek` or `advance` are called when the cursor is past the
//! end of the token slice. Callers must check [`Parser::at_end`] first.

pub mod expressions;
pub mod statements;
#[cfg(test)]
mod tests;

use oxabl_ast::{DataType, Identifier, Span, Statement};
use oxabl_lexer::{Kind, Token, is_callable_kind};

/// An error encountered during parsing, with a human-readable message and source [`Span`].
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

/// Alias for parser results.
pub type ParseResult<T> = Result<T, ParseError>;

/// The result of parsing an ABL source file.
///
/// Contains all successfully parsed statements and any errors encountered.
/// When error recovery is active, the parser continues past errors, so a
/// `Program` may contain both statements and errors.
#[derive(Debug)]
pub struct Program {
    /// Successfully parsed statements.
    pub statements: Vec<Statement>,
    /// Errors encountered during parsing.
    pub errors: Vec<ParseError>,
}

impl Program {
    /// Returns true if parsing completed without errors.
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
}

/// A recursive-descent parser for ABL source code.
///
/// Holds a borrowed token slice and the original source string, advancing a
/// cursor as it recognizes language constructs.
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

    /// Parse the entire token stream into a [`Program`] with error recovery.
    ///
    /// Unlike [`parse_statements`], this method does not bail on the first error.
    /// Instead, it records the error, skips to the next statement boundary via
    /// [`synchronize`], and continues parsing.
    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                }
            }
        }

        Program { statements, errors }
    }

    /// Skip tokens until we reach a statement boundary.
    ///
    /// A statement boundary is either:
    /// - A `.` (period) — ABL's statement terminator. Consumed.
    /// - A statement-starting keyword — not consumed, left for the next
    ///   `parse_statement` call.
    fn synchronize(&mut self) {
        while !self.at_end() {
            if self.check(Kind::Period) {
                self.advance(); // consume the period
                return;
            }
            if statements::can_start_statement(self.peek().kind) {
                return; // don't consume — it starts the next statement
            }
            self.advance();
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

    /// Check if the token at `current + offset` has the given kind.
    /// Safe because the token slice always ends with Kind::Eof.
    fn check_at(&self, offset: usize, kind: Kind) -> bool {
        self.tokens
            .get(self.current + offset)
            .is_some_and(|t| t.kind == kind)
    }

    /// Peek at the token `offset` positions ahead of current.
    /// Safe because the token slice always ends with Kind::Eof.
    fn peek_at(&self, offset: usize) -> &Token {
        &self.tokens[self.current + offset]
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

    /// Returns true if the given Kind can appear as an identifier.
    ///
    /// ABL is very permissive about using keywords as identifiers. This includes
    /// all callable kinds (functions) plus many statement/option keywords like
    /// BUFFER, TEMP-TABLE, PRIMARY, INITIAL, EXTENT, etc.
    fn can_be_identifier(kind: Kind) -> bool {
        is_callable_kind(kind)
            || matches!(
                kind,
                Kind::Buffer
                    | Kind::TempTable
                    | Kind::Initial
                    | Kind::Extent
                    | Kind::Primary
                    | Kind::Validate
                    | Kind::BeforeTable
                    | Kind::WordIndex
                    | Kind::Preselect
                    | Kind::Format
                    | Kind::Label
                    | Kind::ColumnLabel
                    | Kind::Ascending
                    | Kind::Descending
                    | Kind::Shared
                    | Kind::Global
                    // Statement keywords (unreserved)
                    | Kind::Variable
                    | Kind::Function
                    | Kind::Catch
                    | Kind::Finally
                    // Data type keywords (unreserved)
                    | Kind::Integer
                    | Kind::Int64
                    | Kind::Decimal
                    | Kind::Character
                    | Kind::Logical
                    | Kind::Date
                    | Kind::Datetime
                    | Kind::DatetimeTz
                    | Kind::Handle
                    | Kind::Raw
                    | Kind::Memptr
                    | Kind::Longchar
                    | Kind::Clob
                    | Kind::Blob
                    | Kind::ComHandle
            )
    }

    /// Returns true if the given Kind is a data type keyword.
    fn is_data_type_kind(kind: Kind) -> bool {
        matches!(
            kind,
            Kind::Integer
                | Kind::Int64
                | Kind::Decimal
                | Kind::Character
                | Kind::Logical
                | Kind::Date
                | Kind::Datetime
                | Kind::DatetimeTz
                | Kind::Handle
                | Kind::Rowid
                | Kind::Recid
                | Kind::Raw
                | Kind::Memptr
                | Kind::Longchar
                | Kind::Clob
                | Kind::Blob
                | Kind::ComHandle
        )
    }

    /// Parses a potentially dot-qualified identifier like `Customer.CustNum` or `db.table.field`.
    ///
    /// Used for LIKE references where the source is a qualified field name.
    /// Returns a single Identifier whose name contains dots (e.g., "Customer.CustNum").
    fn parse_qualified_identifier(&mut self) -> ParseResult<Identifier> {
        let first = self.parse_identifier()?;
        let mut name = first.name;
        let start = first.span.start;
        let mut end = first.span.end;

        // Consume .qualifier parts
        while self.check(Kind::Period) {
            // Peek past the period to see if there's an identifier following
            let saved = self.current;
            self.advance(); // consume .
            if Self::can_be_identifier(self.peek().kind) {
                let next = self.advance().clone();
                name.push('.');
                name.push_str(&self.source[next.start..next.end]);
                end = next.end as u32;
            } else {
                // Not a qualified name — put the period back
                self.current = saved;
                break;
            }
        }

        Ok(Identifier {
            span: Span { start, end },
            name,
        })
    }

    /// Parses an Identifier
    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        if !Self::can_be_identifier(self.peek().kind) {
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
        let data_type = match token.kind {
            Kind::Integer => DataType::Integer,
            Kind::Int64 => DataType::Int64,
            Kind::Decimal => DataType::Decimal,
            Kind::Character => DataType::Character,
            Kind::Logical => DataType::Logical,
            Kind::Date => DataType::Date,
            Kind::Datetime => DataType::DateTime,
            Kind::DatetimeTz => DataType::DateTimeTz,
            Kind::Handle => DataType::Handle,
            Kind::Rowid => DataType::Rowid,
            Kind::Recid => DataType::Recid,
            Kind::Raw => DataType::Raw,
            Kind::Memptr => DataType::Memptr,
            Kind::Longchar => DataType::Longchar,
            Kind::Clob => DataType::Clob,
            Kind::Blob => DataType::Blob,
            Kind::ComHandle => DataType::Com,
            _ => {
                return Err(ParseError {
                    message: format!(
                        "Unknown data type: {}",
                        &self.source[token.start..token.end]
                    ),
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
