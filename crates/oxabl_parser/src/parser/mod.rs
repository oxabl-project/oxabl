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
    AccessModifier, DataType, Expression, ExpressionKind, HandleParamKind, HandlePassingOptions,
    Identifier, NodeIdAllocator, ParameterDirection, ParameterType, Span, Statement, StatementKind,
    TypeSource,
};
use oxabl_lexer::{Kind, Token, is_callable_kind};

/// An error encountered during parsing, with a human-readable message and source [`Span`].
#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    /// Convert this error into a [`Diagnostic`](oxabl_common::Diagnostic) for
    /// a given file. The caller supplies the [`FileId`](oxabl_common::FileId)
    /// because `ParseError` is file-agnostic.
    pub fn into_diagnostic(self, file: oxabl_common::FileId) -> oxabl_common::Diagnostic {
        oxabl_common::Diagnostic::error(
            "PARSE001",
            self.message,
            oxabl_common::FileSpan {
                file,
                span: self.span,
            },
        )
    }
}

/// Fallback conversion using [`FileId::UNKNOWN`](oxabl_common::FileId::UNKNOWN).
/// Prefer [`ParseError::into_diagnostic`] when the file id is known.
impl From<ParseError> for oxabl_common::Diagnostic {
    fn from(err: ParseError) -> Self {
        err.into_diagnostic(oxabl_common::FileId::UNKNOWN)
    }
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
#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    source: &'a str,
    current: usize,
    /// True iff the remaining token stream (after the initial skip) contains at
    /// least one Comment token.  When false, `advance()` skips the
    /// `skip_comments()` call entirely, eliminating the per-advance overhead in
    /// files that only have leading comments (e.g. the expression benchmark).
    has_comments: bool,
    /// Monotonic allocator for node ids assigned to parser-produced
    /// [`Statement`]s. See `docs/design/ast-invariants.md` §NodeId invariants.
    node_ids: NodeIdAllocator,
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
            node_ids: NodeIdAllocator::new(),
        }
    }

    /// Allocate a fresh [`NodeId`](oxabl_ast::NodeId) and wrap a
    /// [`StatementKind`] in a [`Statement`]. Every parser-produced statement
    /// goes through this helper.
    #[inline]
    pub(crate) fn stmt(&mut self, kind: StatementKind) -> Statement {
        Statement::with_id(self.node_ids.alloc(), kind)
    }

    /// Allocate a fresh [`NodeId`](oxabl_ast::NodeId) and wrap an
    /// [`ExpressionKind`] in an [`Expression`]. Every parser-produced expression
    /// goes through this helper.
    #[inline]
    pub(crate) fn expr(&mut self, kind: ExpressionKind) -> Expression {
        Expression::with_id(self.node_ids.alloc(), kind)
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

    /// Like skip_to_statement_end, but also handles EDITING: ... END. sub-blocks
    /// that appear inside UPDATE/SET/PROMPT-FOR statements.  When EDITING: is
    /// encountered, the body is parsed as a full block (statements until END.)
    /// so that periods inside the editing block are not mistaken for the
    /// statement terminator.
    pub fn skip_to_statement_end_editing_aware(&mut self) {
        while !self.at_end() {
            // Detect EDITING: — parse the editing block body.
            if self.check(Kind::Editing) && self.check_at(1, Kind::Colon) {
                self.advance(); // consume EDITING
                self.advance(); // consume ':'
                let _ = self.parse_block_body();
                return;
            }
            // Normal statement-end detection.
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

    /// Like skip_to_statement_end, but also skips over TRIGGERS: ... END TRIGGERS. blocks
    /// that appear as part of a CREATE widget ASSIGN ... construct.  Without this, the
    /// first statement-terminating period *inside* the trigger sub-block would be mistaken
    /// for the end of the CREATE statement, leaving block nesting misaligned.
    pub fn skip_to_statement_end_triggers_aware(&mut self) {
        while !self.at_end() {
            // Detect TRIGGERS: — enter trigger-block skip mode.
            // TRIGGERS lexes as Kind::Triggers (dedicated keyword kind, NOT Kind::Identifier).
            if self.check(Kind::Triggers) {
                self.advance(); // consume TRIGGERS
                if self.check(Kind::Colon) {
                    self.advance(); // consume ':'
                }
                // Skip the entire TRIGGERS body until END TRIGGERS.
                while !self.at_end() {
                    if self.check(Kind::End) {
                        // Check for END TRIGGERS. — use raw index arithmetic to look ahead
                        // without consuming, skipping comment tokens in the way.
                        let next_is_triggers = {
                            let mut i = self.current + 1;
                            loop {
                                match self.tokens.get(i) {
                                    Some(t) if t.kind == Kind::Comment => i += 1,
                                    Some(t) => break t.kind == Kind::Triggers,
                                    None => break false,
                                }
                            }
                        };
                        if next_is_triggers {
                            self.advance(); // consume END
                            self.advance(); // consume TRIGGERS
                            if self.check(Kind::Period) {
                                self.advance(); // consume '.'
                            }
                            return;
                        }
                    }
                    self.advance();
                }
                return;
            }
            // Normal statement-end detection.
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
    /// Consume a widget/frame/browse name token.
    ///
    /// A name may be a compound preprop+identifier token pair written adjacent
    /// in source (e.g. `{&tablename}f-builder`).  The original token stream has
    /// these as separate tokens (Preprop then Identifier) with adjacent byte
    /// ranges.  This helper consumes the first part unconditionally, then
    /// continues consuming adjacent parts of the same kind.
    pub(crate) fn consume_widget_name(&mut self) {
        if self.at_end() {
            return;
        }
        let mut name_end = self.advance().end;
        loop {
            if self.at_end() {
                break;
            }
            let next = &self.tokens[self.current];
            if next.start != name_end {
                break;
            }
            if Self::can_be_identifier(next.kind)
                || matches!(next.kind, Kind::Preprop | Kind::IncludeArgReference)
            {
                name_end = self.advance().end;
            } else {
                break;
            }
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

    /// Peek at the nth non-comment token ahead of current (1-based: n=1 means the next token).
    /// Used when the token stream may contain comment tokens between meaningful tokens.
    fn peek_nth_non_comment(&self, n: usize) -> &Token {
        let mut count = 0;
        let mut i = self.current;
        loop {
            let t = &self.tokens[i];
            if t.kind != Kind::Comment {
                count += 1;
                if count == n {
                    return t;
                }
            }
            if t.kind == Kind::Eof {
                return t; // guard against running off the end
            }
            i += 1;
        }
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

    /// Consume a period statement terminator, or silently accept EoF.
    /// ABL allows the final statement in a file to omit the trailing `.`.
    fn expect_period(&mut self, msg: &str) -> ParseResult<()> {
        if self.check(Kind::Period) {
            self.advance();
            return Ok(());
        }
        if self.at_end() {
            return Ok(());
        }
        Err(ParseError {
            message: msg.to_string(),
            span: self.current_span(),
        })
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
        } else if self.check(Kind::IntegerLiteral) || self.check(Kind::DecimalLiteral) {
            // FORMAT 9999, FORMAT 99.99, or FORMAT 99/99/9999 — numeric format masks
            self.advance();
            // Consume any trailing /integer or /decimal pairs (date format: 99/99/9999)
            while self.check(Kind::Slash) {
                if matches!(
                    self.peek_at(1).kind,
                    Kind::IntegerLiteral | Kind::DecimalLiteral
                ) {
                    self.advance(); // consume '/'
                    self.advance(); // consume the number
                } else {
                    break;
                }
            }
        } else if self.check(Kind::Preprop) || self.check(Kind::IncludeArgReference) {
            // FORMAT {&variable} — preprocessor reference used as format string
            self.advance();
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
                    | Kind::Returns // unreserved; used as field/variable name (e.g. field returns as log)
                    // DB-operation keywords used as OO method names (e.g. model:create(), obj:delete())
                    | Kind::Create
                    | Kind::Delete
                    | Kind::Release
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
                    | Kind::RepositionToRow
                    | Kind::RepositionBackward
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
                    | Kind::FrameName // system variable returning the current frame's name
                    | Kind::ThisProcedure
                    | Kind::KwSelf
                    | Kind::ThisObject
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
                    | Kind::Update
                    // Preprocessor references used as identifiers (e.g. {&table-name}, {&buffer})
                    | Kind::Preprop
                    | Kind::IncludeReference
                    | Kind::IncludeArgReference
                    // Handle attribute/method names (used after ':' in postfix access)
                    | Kind::Available
                    | Kind::QueryOffEnd
                    // Iterator method names (e.g. iter:Next(), list:ListIterator())
                    | Kind::Next
                    // Lock/query option keywords passed as method arguments (e.g. GET-FIRST(NO-LOCK,NO-WAIT))
                    | Kind::NoWait
                    | Kind::ShareLock
                    | Kind::ExclusiveLock
                    // Object method names that are also reserved keywords
                    // (e.g. dataset:clear(), query:query-close(), table:buffer-copy())
                    | Kind::Clear
                    | Kind::QueryClose
                    | Kind::RepositionToRowid
                    | Kind::RepositionForward
                    | Kind::BufferCopy
                    | Kind::BufferCompare
                    | Kind::GetBufferHandle
                    | Kind::Disconnect
                    // JSON/socket/IO method names used as object methods
                    | Kind::Add
                    | Kind::Write
                    | Kind::Save
                    | Kind::Open
                    | Kind::Close
                    | Kind::Set
                    // ABL dynamic invocation built-ins (usable in expression position)
                    | Kind::DynamicEnum
                    | Kind::DynamicInvoke
                    // System handle keywords (property access via ':')
                    | Kind::SecurityPolicy
                    | Kind::Propath
                    // Memory manipulation functions (usable in assignment LHS / expression position)
                    | Kind::PutByte
                    | Kind::GetByte
                    // Handle attribute names (e.g. builder:Library)
                    | Kind::Library
                    // Frame/widget attribute names (e.g. frame hdr:page-top = false.)
                    | Kind::PageTop
                    | Kind::Blank
                    // Unreserved keywords commonly used as variable/field names
                    | Kind::Transaction
                    | Kind::External
                    // Frame handle attributes used as identifier names (e.g. frame-file, frame-field)
                    | Kind::FrameFile
                    | Kind::FrameField
                    | Kind::FrameIndex
                    // Widget/data keywords commonly used as class/type name components
                    | Kind::Text
                    | Kind::Rectangle
                    | Kind::Size
                    // UI/display keywords used as procedure/method names or frame attribute names
                    | Kind::Header
                    // Frame and Browse can appear as attribute names after ':' (e.g. frame1:frame)
                    // and are also used as qualifier keywords in expression position
                    | Kind::Frame
                    // LAST-EVENT is a system handle used as an object (e.g. last-event:label)
                    | Kind::LastEvent
                    // CONNECT is a statement keyword but also used as an OO method name (e.g. hwsdl:CONNECT(...))
                    | Kind::Connect
                    // Handle attribute names used as member names after ':' (e.g. htable:BUFFER-FIELD(j):HELP)
                    | Kind::Help
                    // CURRENT-LANGUAGE is a system variable used in expression context
                    | Kind::CurrentLanguage
                    // CURRENT-WINDOW is a system handle used as RHS in assignments (e.g. {&WINDOW-NAME} = CURRENT-WINDOW.)
                    | Kind::CurrentWindow
                    // KEYS is a buffer/table attribute used as method name (e.g. b-table:keys)
                    | Kind::Keys
                    // FRAME-VALUE is an ABL built-in used in expression context (e.g. frame-value = "")
                    | Kind::FrameValue
                    // DBNAME is an ABL built-in function (returns the connected database name)
                    | Kind::Dbname
                    // AMBIGUOUS is a buffer attribute (e.g. hdbSource:Ambiguous)
                    | Kind::Ambiguous
                    // SEEK is an ABL built-in function (returns current stream position)
                    | Kind::Seek
                    // OVERLAY is an ABL built-in that can appear as lvalue in ASSIGN (e.g. OVERLAY(s,1,3) = "abc")
                    | Kind::Overlay
                    // TITLE is a widget attribute name (e.g. p-widgets:title)
                    | Kind::Title
                    // ACTIVE-WINDOW is a system handle used as widget name in ON triggers (e.g. ON HELP OF ACTIVE-WINDOW)
                    | Kind::ActiveWindow
                    // FOCUS is a system handle for the currently focused widget (used in expression context)
                    | Kind::Focus
                    // TERMINAL is a system handle/variable (e.g. OUTPUT TO TERMINAL, v = terminal)
                    | Kind::Terminal
                    // GO-PENDING is a system variable (logical, true if GO key triggered)
                    | Kind::GoPending
                    // READ-EXACT-NUM is a socket read mode constant
                    | Kind::ReadExactNum
                    // BIG-ENDIAN / LITTLE-ENDIAN are byte-order constants
                    | Kind::BigEndian
                    // WINDOW is a system object attribute name (e.g. lframes:window)
                    | Kind::Window
                    // SCREEN-LINES is an ABL system variable (number of screen rows)
                    | Kind::ScreenLines
                    // LOG-MANAGER is a system handle for logging (e.g. LOG-MANAGER:LOGGING-LEVEL)
                    | Kind::LogManager
                    // XREF / XREF-XML are COMPILE statement options used as member names (e.g. compile.xref)
                    | Kind::Xref
                    | Kind::XrefXml
                    // COMPILE appears as an ordinary identifier in variable names (e.g. "comp" is a
                    // valid variable name even though "comp" is the min abbreviation of COMPILE)
                    | Kind::Compile
                    // ENUM is a reserved keyword but appears as a field name in DEFINE TEMP-TABLE (field enum as int)
                    | Kind::KwEnum
                    // DATABASE appears in qualified class names like Progress.Database.TempTableInfo
                    | Kind::Database
                    // FILL is a Dataset method name (e.g. Dataset.Fill, Run Dataset.Fill)
                    | Kind::Fill
                    // Widget attribute names that are also ABL keywords (e.g. widget:font, widget:skip)
                    | Kind::Font
                    | Kind::Skip
                    | Kind::Field
                    // SQL keywords that can appear as identifier names (e.g. variable select)
                    | Kind::Select
                    | Kind::Insert
            )
    }

    /// Returns true if the given Kind is valid as the member/method name
    /// following a postfix `:` (e.g. `obj:foo()`, `list:Current`,
    /// `list:Contains(x)`).
    ///
    /// This extends `can_be_identifier` with reserved keywords that ABL class
    /// libraries legitimately use as method/property names. The list is
    /// deliberately targeted (not every keyword) — block-delimiter contexts
    /// like `CASE x: WHEN ...` must still break out of the postfix loop, so
    /// we cannot accept `WHEN`, `END`, `OTHERWISE`, etc. here.
    fn can_be_member_name(kind: Kind) -> bool {
        Self::can_be_identifier(kind)
            || matches!(
                kind,
                Kind::Begins | Kind::Contains | Kind::Matches | Kind::Current
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
                // Comments are never "word-like" — parse_class_qualified_name() uses
                // raw next_idx arithmetic (bypassing skip_comments) and must not
                // treat a trailing comment as a continuation of the class name.
                | Kind::Comment
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

    /// Parses a dot-qualified class name where namespace components may be reserved keywords.
    ///
    /// ABL class files live in directories whose names often match reserved keywords
    /// (e.g. the `do/` module produces class names like `do.wsdo800obj`).
    /// Unlike `parse_qualified_identifier()`, this accepts any word-like token as the
    /// first component (via `is_word_kind()`), not just `can_be_identifier()` tokens.
    fn parse_class_qualified_name(&mut self) -> ParseResult<Identifier> {
        let token = self.peek();
        if !Self::is_word_kind(token.kind) {
            return Err(ParseError {
                message: "Expected class name".to_string(),
                span: self.current_span(),
            });
        }
        let start_tok = self.advance().clone();
        let start = start_tok.start as u32;
        let mut end = start_tok.end as u32;
        let mut name = self.source[start_tok.start..start_tok.end].to_string();

        // Consume .qualifier parts (same-line only)
        while self.check(Kind::Period) {
            let period_end = self.tokens[self.current].end;
            let next_idx = self.current + 1;
            if let Some(next_tok) = self.tokens.get(next_idx) {
                if Self::is_word_kind(next_tok.kind)
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

    /// Parses an Identifier, handling compound preprop names like:
    /// - `{&prefix}suffix`  (preprop directly followed by identifier)
    /// - `prefix{&suffix}`  (identifier directly followed by preprop)
    /// - `{&a}b{&c}`        (multi-part chains via direct adjacency)
    ///
    /// Requires no whitespace between parts to avoid consuming operators.
    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected identifier".to_string(),
                span: self.current_span(),
            });
        }
        let token = self.advance().clone();
        let start = token.start;
        let mut end = token.end;

        // Extend with directly-adjacent Preprop or identifier parts.
        // Also handle adjacent Minus tokens so that compound names like
        // `b-{&preproc}-suffix` (where the lexer splits on `{`) are
        // consumed as a single identifier.
        loop {
            let next = &self.tokens[self.current];
            if next.start != end {
                break;
            }
            if next.kind == Kind::Preprop
                || next.kind == Kind::IncludeArgReference
                || Self::can_be_identifier(next.kind)
            {
                end = self.advance().end;
            } else if next.kind == Kind::Minus {
                // Adjacent hyphen: part of a compound name like b-{&preproc}-suffix.
                // Only consume the '-' if a word-like token follows directly, so that
                // arithmetic like `{&x} - value` (spaced) is unaffected.
                // Use is_word_kind() for the suffix so keyword-named parts (e.g.
                // `-control`, `-table`) are accepted as name components.
                let minus_end = next.end;
                let after_idx = self.current + 1;
                let after_ok = self
                    .tokens
                    .get(after_idx)
                    .is_some_and(|a| a.start == minus_end && Self::is_word_kind(a.kind));
                if after_ok {
                    self.advance(); // consume '-'
                    end = self.advance().end; // consume word
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(Identifier {
            span: Span {
                start: start as u32,
                end: end as u32,
            },
            name: self.source[start..end].to_string(),
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
    /// Each parameter becomes a `self.stmt(StatementKind::DefineParameter)`.
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

                // Include reference after direction keyword (e.g. INPUT {gl/global-input-func.i})
                // expands to one or more parameters at preprocessing time — skip as a unit.
                if self.check(Kind::IncludeReference) {
                    self.advance();
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }

                // TABLE [FOR] <name> [APPEND] [BIND] [BY-VALUE] [BY-REFERENCE]
                if self.check(Kind::Table) {
                    self.advance(); // consume TABLE
                    // Optional FOR keyword: TABLE FOR ttablename passes by reference
                    if self.check(Kind::KwFor) {
                        self.advance();
                    }
                    let name = self.parse_identifier()?;
                    // Consume optional passing modifiers
                    while matches!(
                        self.peek().kind,
                        Kind::Append | Kind::Bind | Kind::ByValue | Kind::ByReference
                    ) {
                        self.advance();
                    }
                    params.push(self.stmt(StatementKind::DefineParameter {
                        direction,
                        param_type: ParameterType::Handle {
                            kind: HandleParamKind::Table,
                            name,
                            passing: HandlePassingOptions::default(),
                        },
                    }));
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }

                // BUFFER <buf-name> FOR <table-name> — buffer parameter (no AS/LIKE)
                if self.check(Kind::Buffer) {
                    self.advance(); // consume BUFFER
                    let name = self.parse_identifier()?; // buffer name
                    if self.check(Kind::KwFor) {
                        self.advance(); // consume FOR
                        self.parse_identifier().ok(); // table name
                    }
                    params.push(self.stmt(StatementKind::DefineParameter {
                        direction,
                        param_type: ParameterType::Buffer {
                            name: name.clone(),
                            target: name,
                        },
                    }));
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }

                // DATASET <name> [APPEND] [BIND] [BY-VALUE] — dataset parameter (no AS/LIKE)
                if self.check(Kind::Dataset) {
                    self.advance(); // consume DATASET
                    let name = self.parse_identifier()?;
                    while matches!(
                        self.peek().kind,
                        Kind::Append | Kind::Bind | Kind::ByValue | Kind::ByReference
                    ) {
                        self.advance();
                    }
                    params.push(self.stmt(StatementKind::DefineParameter {
                        direction,
                        param_type: ParameterType::Handle {
                            kind: HandleParamKind::Dataset,
                            name,
                            passing: HandlePassingOptions::default(),
                        },
                    }));
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                    continue;
                }

                // DATASET-HANDLE <name> [APPEND] [BIND] [BY-VALUE] — dataset handle parameter (no AS/LIKE)
                if self.check(Kind::DatasetHandle) {
                    self.advance(); // consume DATASET-HANDLE
                    let name = self.parse_identifier()?;
                    while matches!(
                        self.peek().kind,
                        Kind::Append | Kind::Bind | Kind::ByValue | Kind::ByReference
                    ) {
                        self.advance();
                    }
                    params.push(self.stmt(StatementKind::DefineParameter {
                        direction,
                        param_type: ParameterType::Handle {
                            kind: HandleParamKind::DatasetHandle,
                            name,
                            passing: HandlePassingOptions::default(),
                        },
                    }));
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

                params.push(self.stmt(StatementKind::DefineParameter {
                    direction,
                    param_type: ParameterType::Variable {
                        name,
                        type_source,
                        no_undo,
                    },
                }));

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
            // Consume optional EXTENT [n] clause (e.g. AS CHAR EXTENT 2 or AS HANDLE EXTENT {&N})
            if self.check(Kind::Extent) {
                self.advance();
                if self.check(Kind::IntegerLiteral)
                    || self.check(Kind::Preprop)
                    || self.check(Kind::IncludeArgReference)
                {
                    self.advance();
                }
            }
            Ok(TypeSource::Explicit(data_type))
        }
    }

    /// If the current token is `<`, consume tokens through the matching `>`,
    /// balancing nested `<...>` pairs. Used after class-name parsing in data
    /// type position so generic class types like `List<String>` or
    /// `Map<String, List<Integer>>` parse. Generic type arguments aren't yet
    /// modeled in the AST — this is a syntactic skip.
    fn consume_generic_type_args(&mut self) {
        if !self.check(Kind::LessThan) {
            return;
        }
        self.advance(); // consume `<`
        let mut depth: i32 = 1;
        while !self.at_end() && depth > 0 {
            match self.peek().kind {
                Kind::LessThan => depth += 1,
                Kind::GreaterThan => depth -= 1,
                _ => {}
            }
            self.advance();
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
                let class_name = self.parse_class_qualified_name()?;
                self.consume_generic_type_args();
                return Ok(DataType::Class(class_name.name));
            }
            // ABL allows `AS ClassName` (without CLASS keyword) for class types.
            // Dotted names like `forms.deco_proof_form` are class references.
            Kind::Identifier => {
                let class_name = self.parse_class_qualified_name()?;
                self.consume_generic_type_args();
                return Ok(DataType::Class(class_name.name));
            }
            // ABL allows "in" as abbreviation for "integer" (e.g. "def var x as in no-undo")
            Kind::KwIn => DataType::Integer,
            // Progress.* is a namespace prefix for built-in ABL classes
            // (e.g. "Progress.Json.ObjectModel.JsonObject")
            Kind::Progress => {
                let class_name = self.parse_class_qualified_name()?;
                self.consume_generic_type_args();
                return Ok(DataType::Class(class_name.name));
            }
            // Reserved keywords used as namespace prefixes in OO-ABL class names.
            // ABL class files live in directories whose names may be reserved keywords
            // (e.g. `do.wsdo800obj` where `do` is Kind::Do). These must be followed
            // by a dot to be valid class references.
            _ if Self::is_word_kind(token.kind)
                && self
                    .tokens
                    .get(self.current + 1)
                    .is_some_and(|t| t.kind == Kind::Period) =>
            {
                let class_name = self.parse_class_qualified_name()?;
                self.consume_generic_type_args();
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
