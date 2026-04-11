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

use oxabl_ast::{
    AccessModifier, DataType, HandleParamKind, HandlePassingOptions, Identifier,
    ParameterDirection, ParameterType, Span, Statement, TypeSource,
};
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
    /// True iff the remaining token stream (after the initial skip) contains at
    /// least one Comment token.  When false, `advance()` skips the
    /// `skip_comments()` call entirely, eliminating the per-advance overhead in
    /// files that only have leading comments (e.g. the expression benchmark).
    has_comments: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], source: &'a str) -> Self {
        debug_assert!(!tokens.is_empty(), "Token slice must contain at least EOF");
        let mut current = 0;
        // Fast-path: skip any leading comments without going through advance().
        while current < tokens.len() && tokens[current].kind == Kind::Comment {
            current += 1;
        }
        // Check whether any comments remain after the leading block so we know
        // whether advance() needs to call skip_comments() at all.
        let has_comments = tokens[current..].iter().any(|t| t.kind == Kind::Comment);
        Parser {
            tokens,
            source,
            current,
            has_comments,
        }
    }

    /// Parse the entire token stream into a [`Program`] with error recovery.
    ///
    /// Unlike [`parse_statements`], this method does not bail on the first error.
    /// Instead, it records the error, skips to the next statement boundary via
    /// [`synchronize`], and continues parsing.
    /// Maximum number of errors before the parser bails out.
    /// Prevents infinite loops when error recovery cannot make progress.
    const MAX_ERRORS: usize = 50;

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.at_end() {
            let pos_before = self.current;
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    errors.push(err);
                    if errors.len() >= Self::MAX_ERRORS {
                        break;
                    }
                    self.synchronize();
                    // If neither parse_statement nor synchronize advanced the
                    // cursor, we are stuck on a token that can_start_statement
                    // recognises but parse_statement cannot handle (e.g. END).
                    // Force progress to avoid an infinite loop.
                    if self.current == pos_before {
                        self.advance();
                    }
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

    /// Skip tokens unconditionally until a period is consumed.
    /// Unlike `synchronize`, this does NOT stop at statement-starting keywords —
    /// use this when skipping the body of a known statement that may contain
    /// keyword tokens like FOR, DO, etc. as part of its own syntax.
    pub fn skip_to_period(&mut self) {
        while !self.at_end() {
            if self.check(Kind::Period) {
                self.advance(); // consume the period
                return;
            }
            self.advance();
        }
    }

    /// Like skip_to_period but treats `.identifier` on the same line as field access,
    /// only stopping at a period that terminates a statement (not followed by an identifier).
    pub fn skip_to_statement_end(&mut self) {
        while !self.at_end() {
            if self.check(Kind::Period) {
                let period_end = self.tokens[self.current].end;
                let is_field_access = self.tokens.get(self.current + 1).is_some_and(|t| {
                    Self::can_be_identifier(t.kind)
                        && !self.source[period_end..t.start].contains('\n')
                });
                if !is_field_access {
                    self.advance(); // consume the terminating period
                    return;
                }
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
        if self.has_comments {
            self.skip_comments();
        }
        token
    }

    fn skip_comments(&mut self) {
        // Direct bounds check + indexing is faster than `.get().is_some_and()`.
        // The EOF sentinel at the end of the stream is never Comment, so we
        // always stop before going out of bounds.
        let len = self.tokens.len();
        while self.current < len && self.tokens[self.current].kind == Kind::Comment {
            self.current += 1;
        }
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

    /// Consumes a string literal that may carry an ABL translation suffix (`:U`, `:T`, etc.).
    /// After parsing `FORMAT "x(125)":U`, the `"x(125)"` is a `StringLiteral` followed by
    /// `Colon` + `Identifier`. Call this after consuming the FORMAT/LABEL keyword.
    fn skip_format_value(&mut self) {
        if self.check(Kind::StringLiteral) {
            self.advance();
            // Consume optional :U / :T / :6 etc. translation suffix
            if self.check(Kind::Colon) && Self::can_be_identifier(self.peek_at(1).kind) {
                self.advance(); // colon
                self.advance(); // suffix identifier
            }
        }
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
                    // Statement keywords (unreserved, may appear as identifiers)
                    | Kind::Variable
                    | Kind::Function
                    | Kind::Catch
                    | Kind::Finally
                    | Kind::Run
                    | Kind::Display
                    | Kind::Message
                    | Kind::Assign
                    | Kind::Find
                    | Kind::Procedure
                    // OO-ABL keywords (all unreserved except SET which is already handled)
                    | Kind::Class
                    | Kind::Interface
                    | Kind::Inherits
                    | Kind::Implements
                    | Kind::Method
                    | Kind::Constructor
                    | Kind::Destructor
                    | Kind::Property
                    | Kind::Public
                    | Kind::Private
                    | Kind::Protected
                    | Kind::PackagePrivate
                    | Kind::Abstract
                    | Kind::Final
                    | Kind::Override
                    | Kind::KwStatic
                    | Kind::Void
                    | Kind::Get
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
                    | Kind::Rowid
                    | Kind::Recid
                    | Kind::Raw
                    | Kind::Memptr
                    | Kind::Longchar
                    | Kind::Clob
                    | Kind::Blob
                    | Kind::ComHandle
                    // Dataset / data-source keywords (unreserved)
                    | Kind::Dataset
                    | Kind::DatasetHandle
                    | Kind::DataRelation
                    | Kind::DataSource
                    | Kind::NamespaceUri
                    | Kind::NamespacePrefix
                    | Kind::XmlNodeName
                    | Kind::XmlNodeType
                    | Kind::SerializeName
                    | Kind::SerializeHidden
                    | Kind::Serializable
                    | Kind::NonSerializable
                    | Kind::ReferenceOnly
                    | Kind::RelationFields
                    | Kind::Nested
                    | Kind::ForeignKeyHidden
                    | Kind::NotActive
                    | Kind::Recursive
                    | Kind::ParentIdRelation
                    | Kind::ParentIdField
                    | Kind::ParentFieldsBefore
                    | Kind::ParentFieldsAfter
                    | Kind::WidgetPool
                    | Kind::TableHandle
                    | Kind::Bind
                    | Kind::ByValue
                    | Kind::Query
                    | Kind::Reposition
                    // Event system keywords (unreserved)
                    | Kind::Publish
                    | Kind::Subscribe
                    | Kind::Unsubscribe
                    | Kind::Anywhere
                    | Kind::Event
                    | Kind::Signature
                    | Kind::RunProcedure
                    // ON trigger keywords (unreserved)
                    | Kind::Trigger
                    | Kind::Triggers
                    | Kind::Persistent
                    | Kind::Revert
                    | Kind::Choose
                    | Kind::Endkey
                    | Kind::Browse
                    // BREAK BY group functions (callable, take a field argument)
                    | Kind::FirstOf
                    | Kind::LastOf
                    // System handles and built-in functions used in expression position
                    | Kind::ThisProcedure
                    | Kind::KwSelf
                    | Kind::FileInfo
                    | Kind::CanDo
                    | Kind::Entry
                    | Kind::NumEntries
                    | Kind::Index
                    | Kind::Yes
                    | Kind::Session
                    | Kind::ErrorStatus
                    | Kind::Value
                    | Kind::Locked
                    | Kind::NoLock
                    | Kind::Parameter
                    | Kind::Table
                    // Statement keywords that may also appear as identifiers/names
                    | Kind::Empty
                    | Kind::Form
                    | Kind::Put
                    | Kind::CopyLob
                    | Kind::OsDir
                    // Preprocessor references used as identifiers (e.g. {&table-name}, {&buffer})
                    | Kind::Preprop
                    | Kind::IncludeReference
                    | Kind::IncludeArgReference
                    // Handle attribute/method names (used after ':' in postfix access)
                    | Kind::Available
                    | Kind::QueryOffEnd
                    // Lock/query option keywords passed as method arguments (e.g. GET-FIRST(NO-LOCK,NO-WAIT))
                    | Kind::NoWait
                    | Kind::ShareLock
                    | Kind::ExclusiveLock
                    // Object method names that are also reserved keywords
                    // (e.g. dataset:clear(), query:query-close(), table:buffer-copy())
                    | Kind::Clear
                    | Kind::QueryClose
                    | Kind::RepositionToRowid
                    | Kind::BufferCopy
                    | Kind::BufferCompare
                    | Kind::GetBufferHandle
                    | Kind::Disconnect
                    // Frame/widget attribute names (e.g. frame hdr:page-top = false.)
                    | Kind::PageTop
            )
    }

    /// Returns true if the given Kind is "word-like" — an identifier, keyword, or
    /// similar token that can appear as a component in a procedure file path.
    /// This is deliberately broader than `can_be_identifier`: it includes reserved
    /// keywords like DO, IF, FOR etc., which are valid directory or file name parts.
    fn is_word_kind(kind: Kind) -> bool {
        !matches!(
            kind,
            Kind::Slash
                | Kind::Star
                | Kind::Add
                | Kind::Minus
                | Kind::Equals
                | Kind::Period
                | Kind::Comma
                | Kind::LeftParen
                | Kind::RightParen
                | Kind::LeftBracket
                | Kind::RightBracket
                | Kind::Colon
                | Kind::IntegerLiteral
                | Kind::DecimalLiteral
                | Kind::StringLiteral
                | Kind::KwTrue
                | Kind::KwFalse
                | Kind::Question
                | Kind::Eof
                | Kind::Invalid
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

        // Consume .qualifier parts (same-line only — the period must not be a statement terminator)
        while self.check(Kind::Period) {
            // Peek past the period to see if there's an identifier on the same line
            let period_end = self.tokens[self.current].end;
            let next_idx = self.current + 1;
            if let Some(next_tok) = self.tokens.get(next_idx) {
                if Self::can_be_identifier(next_tok.kind)
                    && !self.source[period_end..next_tok.start].contains('\n')
                {
                    self.advance(); // consume '.'
                    let next = self.advance().clone();
                    name.push('.');
                    name.push_str(&self.source[next.start..next.end]);
                    end = next.end as u32;
                } else {
                    break;
                }
            } else {
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

    /// Parse an optional access modifier (PUBLIC, PRIVATE, PROTECTED, PACKAGE-PRIVATE).
    /// Returns None if the current token is not an access modifier.
    fn parse_access_modifier(&mut self) -> Option<AccessModifier> {
        match self.peek().kind {
            Kind::Public => {
                self.advance();
                Some(AccessModifier::Public)
            }
            Kind::Private => {
                self.advance();
                Some(AccessModifier::Private)
            }
            Kind::Protected => {
                self.advance();
                Some(AccessModifier::Protected)
            }
            Kind::PackagePrivate => {
                self.advance();
                Some(AccessModifier::PackagePrivate)
            }
            _ => None,
        }
    }

    /// Parse a parenthesized parameter list for METHOD/CONSTRUCTOR.
    ///
    /// `(INPUT x AS INTEGER, OUTPUT y AS CHARACTER)`
    ///
    /// Each parameter becomes a `Statement::DefineParameter`.
    fn parse_parenthesized_params(&mut self) -> ParseResult<Vec<Statement>> {
        self.expect_kind(Kind::LeftParen, "Expected '(' for parameter list")?;
        let mut params = Vec::new();

        if !self.check(Kind::RightParen) {
            loop {
                // Include file references (e.g. {gl/global-input-func.i}) expand to
                // a set of parameters at preprocessing time — skip them as a unit.
                if self.check(Kind::IncludeReference) {
                    self.advance();
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }

                let direction = match self.peek().kind {
                    Kind::Output => {
                        self.advance();
                        ParameterDirection::Output
                    }
                    Kind::InputOutput => {
                        self.advance();
                        ParameterDirection::InputOutput
                    }
                    Kind::Input => {
                        self.advance();
                        ParameterDirection::Input
                    }
                    _ => ParameterDirection::Input,
                };

                // TABLE <name> — temp-table pass-through parameter (no AS/LIKE)
                if self.check(Kind::Table) {
                    self.advance(); // consume TABLE
                    let name = self.parse_identifier()?;
                    params.push(Statement::DefineParameter {
                        direction,
                        param_type: ParameterType::Handle {
                            kind: HandleParamKind::Table,
                            name,
                            passing: HandlePassingOptions::default(),
                        },
                    });
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }

                let name = self.parse_identifier()?;
                let type_source = self.parse_type_source()?;
                let no_undo = if self.check(Kind::NoUndo) {
                    self.advance();
                    true
                } else {
                    false
                };

                params.push(Statement::DefineParameter {
                    direction,
                    param_type: ParameterType::Variable {
                        name,
                        type_source,
                        no_undo,
                    },
                });

                if !self.check(Kind::Comma) {
                    break;
                }
                self.advance(); // consume comma
            }
        }

        self.expect_kind(Kind::RightParen, "Expected ')'")?;
        Ok(params)
    }

    /// Parse `AS type | LIKE field` for DEFINE VARIABLE and DEFINE PARAMETER contexts.
    /// Consumes the `AS` or `LIKE` keyword and the following type/identifier.
    fn parse_type_source(&mut self) -> ParseResult<TypeSource> {
        if self.check(Kind::Like) {
            self.advance(); // consume LIKE
            let source = self.parse_qualified_identifier()?;
            // Consume optional array subscript: LIKE table.field[n]
            if self.check(Kind::LeftBracket) {
                self.advance(); // consume '['
                self.parse_expression().ok();
                if self.check(Kind::RightBracket) {
                    self.advance();
                }
            }
            Ok(TypeSource::Like { source })
        } else {
            self.expect_kind(Kind::KwAs, "Expected AS or LIKE")?;
            let data_type = self.parse_data_type()?;
            Ok(TypeSource::Explicit(data_type))
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
            Kind::Class => {
                self.advance(); // consume CLASS
                let class_name = self.parse_qualified_identifier()?;
                return Ok(DataType::Class(class_name.name));
            }
            // ABL allows `AS ClassName` (without CLASS keyword) for class types.
            // Dotted names like `forms.deco_proof_form` are class references.
            Kind::Identifier => {
                let class_name = self.parse_qualified_identifier()?;
                return Ok(DataType::Class(class_name.name));
            }
            Kind::PreprocIf => {
                self.advance(); // consume &IF
                let preproc = self.parse_preproc_if(1, &Self::parse_data_type)?;
                if preproc.else_branch.is_none() {
                    return Err(ParseError {
                        message: "Data type-level &IF requires &ELSE branch".to_string(),
                        span: self.current_span(),
                    });
                }
                return Ok(DataType::PreprocIf(Box::new(preproc)));
            }
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
