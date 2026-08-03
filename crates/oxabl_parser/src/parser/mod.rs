//! Recursive-descent parser for ABL source code.
//!
//! The parser walks a token slice using a cursor. Expression parsing uses
//! precedence climbing (see [`expressions`]). Statement dispatch uses
//! keyword-based if/else chains (see [`statements`]).
//!
//! Will panic if `peek` or `advance` are called when the cursor is past the
//! end of the token slice. Callers must check [`Parser::at_end`] first.

pub(crate) mod expressions;
pub(crate) mod statements;
#[cfg(test)]
mod tests;

use oxabl_ast::{
    AccessModifier, Comment, CommentKind, DataType, Expression, ExpressionKind, HandleParamKind,
    HandlePassingOptions, Identifier, NodeId, NodeIdAllocator, ParameterDirection, ParameterType,
    Span, Statement, StatementKind, TypeSource,
};
use oxabl_lexer::{Kind, Token, is_callable_kind};

/// An error encountered during parsing, with a human-readable message and source [`Span`].
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ParseError {}

/// Fallback conversion using [`FileId::UNKNOWN`](oxabl_common::FileId::UNKNOWN).
/// Prefer [`ParseError::into_diagnostic`] when the file id is known.
impl From<ParseError> for oxabl_common::Diagnostic {
    fn from(err: ParseError) -> Self {
        err.into_diagnostic(oxabl_common::FileId::UNKNOWN)
    }
}

/// Alias for parser results.
pub type ParseResult<T> = Result<T, ParseError>;

/// Debug-only invariant check (R1.3, ast-invariants.md §1): sibling statement
/// spans must be in source order and non-overlapping.
///
/// Zero-width synthetic/recovery nodes (`start == end`, R1.4) are permitted to
/// abut a neighbour — the `<=` comparison already allows it. Compiled out in
/// release builds; the whole workspace test suite runs in debug, exercising it.
#[inline]
#[cfg_attr(not(debug_assertions), allow(unused_variables))]
pub(crate) fn debug_assert_stmt_sibling_order(stmts: &[Statement]) {
    #[cfg(debug_assertions)]
    for pair in stmts.windows(2) {
        debug_assert!(
            pair[0].span.end <= pair[1].span.start,
            "sibling statement spans out of source order: {:?} then {:?}",
            pair[0].span,
            pair[1].span,
        );
    }
}

/// Debug-only invariant check (R1.3): sibling expression spans (e.g. argument
/// lists) must be in source order and non-overlapping. See
/// [`debug_assert_stmt_sibling_order`] for the tolerance/gating contract.
#[inline]
#[cfg_attr(not(debug_assertions), allow(unused_variables))]
pub(crate) fn debug_assert_expr_sibling_order(exprs: &[Expression]) {
    #[cfg(debug_assertions)]
    for pair in exprs.windows(2) {
        debug_assert!(
            pair[0].span.end <= pair[1].span.start,
            "sibling expression spans out of source order: {:?} then {:?}",
            pair[0].span,
            pair[1].span,
        );
    }
}

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
    /// Every comment recognized by the lexer, in source order, hung off the
    /// root as advisory fidelity data for the future formatter. Empty for files
    /// with no comments. Never read by semantic/lint/analyze passes. See
    /// `docs/design/ast-invariants.md` §13.
    pub comments: Vec<Comment>,
}

impl Program {
    /// Returns true if parsing completed without errors.
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }

    /// Returns the first [`ParseError`] recovered during parsing, or `None` if
    /// the parse was clean. Consumers that only care whether parsing succeeded
    /// (the fail-fast shape of [`Parser::parse_statements`]) can read this
    /// instead of iterating `errors`; error recovery still preserves the first
    /// error at `errors[0]`.
    pub fn first_error(&self) -> Option<&ParseError> {
        self.errors.first()
    }

    /// Convert this program into a `Result`, yielding `Ok(self)` when the parse
    /// was clean and `Err(errors)` otherwise. Sugar for consumers that prefer
    /// `Result`-style flow over inspecting the embedded `errors` vector.
    pub fn into_result(self) -> Result<Program, Vec<ParseError>> {
        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(self.errors)
        }
    }

    /// Map every recovered [`ParseError`] to a [`Diagnostic`](oxabl_common::Diagnostic)
    /// for the given file, using the existing [`ParseError::into_diagnostic`]
    /// conversion. Consumers no longer need to map `errors` by hand.
    pub fn into_diagnostics(self, file: oxabl_common::FileId) -> Vec<oxabl_common::Diagnostic> {
        self.errors
            .into_iter()
            .map(|e| e.into_diagnostic(file))
            .collect()
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
    /// Accumulated parse errors recovered from anywhere in the parse — both the
    /// top-level [`parse_program`](Self::parse_program) loop and the per-statement
    /// recovery inside block bodies (see
    /// [`parse_block_statement_recovering`](Self::parse_block_statement_recovering)).
    /// Drained by `parse_program` into [`Program::errors`]. Capped at
    /// [`MAX_ERRORS`](Self::MAX_ERRORS) to bound memory on pathological input.
    errors: Vec<ParseError>,
    /// Token index of the first token of the statement currently being parsed,
    /// maintained by the [`parse_statement`](Self::parse_statement) funnel
    /// (saved and restored around nested statements).
    ///
    /// Exists so [`skipped_stmt`](Self::skipped_stmt) can take its harvest range
    /// from the statement's *first* token no matter where the skip started.
    /// Dispatch sites disagree about whether they `advance()` past their own
    /// keyword before skipping — `PUT` and `UPDATE` do not, `EXPORT` and
    /// `COMPILE` do — so anchoring on the statement start is what lets one
    /// uniform "drop index 0" rule strip the dispatch keyword in both shapes.
    stmt_start_token: usize,
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
            errors: Vec::new(),
            stmt_start_token: current,
        }
    }

    /// Allocate a fresh [`NodeId`](oxabl_ast::NodeId) and wrap a
    /// [`StatementKind`] in a [`Statement`]. Every parser-produced statement
    /// goes through this helper.
    #[inline]
    pub(crate) fn stmt(&mut self, kind: StatementKind) -> Statement {
        // The span defaults to DUMMY here; the `parse_statement` funnel
        // overwrites it with the real full-extent span (KTD2).
        Statement::with_id(self.node_ids.alloc(), Span::DUMMY, kind)
    }

    /// Allocate a fresh [`NodeId`](oxabl_ast::NodeId) for a node that is not a
    /// [`Statement`] or [`Expression`] wrapper.
    ///
    /// The only such nodes are the two cross-file targets named as bare strings
    /// — `StatementKind::Using` and `RunTarget::Literal` — which need identity
    /// because workspace resolution records them in the `NodeId`-keyed
    /// `references` side table (`docs/design/ast-invariants.md` §2).
    #[inline]
    pub(crate) fn node_id(&mut self) -> NodeId {
        self.node_ids.alloc()
    }

    /// Allocate a fresh [`NodeId`](oxabl_ast::NodeId) and wrap an
    /// [`ExpressionKind`] in an [`Expression`]. Every parser-produced expression
    /// goes through this helper.
    #[inline]
    pub(crate) fn expr(&mut self, kind: ExpressionKind) -> Expression {
        // The span defaults to DUMMY here; the precedence-level funnels in
        // `expressions.rs` overwrite it with the real full-extent span (KTD2).
        Expression::with_id(self.node_ids.alloc(), Span::DUMMY, kind)
    }

    /// Allocate a fresh [`NodeId`](oxabl_ast::NodeId) and wrap an
    /// [`ExpressionKind`] with an explicit full-extent span `lo..hi` (KTD2/U3).
    ///
    /// `hi` is clamped to at least `lo` so a node can never carry an inverted
    /// span; a genuinely token-less node collapses to zero-width at `lo`.
    #[inline]
    pub(crate) fn spanned_expr(&mut self, lo: u32, hi: u32, kind: ExpressionKind) -> Expression {
        Expression::with_id(
            self.node_ids.alloc(),
            Span {
                start: lo,
                end: hi.max(lo),
            },
            kind,
        )
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

        while !self.at_end() {
            if self.errors.len() >= Self::MAX_ERRORS {
                break;
            }
            let pos_before = self.current;
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.record_error(err);
                    if self.errors.len() >= Self::MAX_ERRORS {
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

        // Drain the accumulator: it holds both top-level errors recorded above
        // and any per-statement errors recovered inside block bodies during the
        // parse_statement calls. This preserves the aggregate error-reporting
        // contract even though block bodies now recover instead of bubbling.
        debug_assert_stmt_sibling_order(&statements);
        let comments = self.collect_comments();
        Program {
            statements,
            errors: std::mem::take(&mut self.errors),
            comments,
        }
    }

    /// Collect every `Kind::Comment` token into a sorted, classified comment
    /// table (KTD1). This is one independent linear pass over the full token
    /// slice — decoupled from the cursor and the `skip_comments` fast-path, so
    /// no skip site can leak a dropped comment (origin R3.4). It does **not**
    /// gate on `has_comments` (KTD2): that flag is `false` for a file whose only
    /// comments are leading, so gating on it would lose them. The token stream
    /// is already source-ordered, so the result is sorted by `span.start` with
    /// no explicit sort, and the `Vec` allocates only when comments exist.
    fn collect_comments(&self) -> Vec<Comment> {
        let mut comments = Vec::new();
        for token in self.tokens {
            if token.kind != Kind::Comment {
                continue;
            }
            if let Some(kind) = self.classify_comment(token.start) {
                comments.push(Comment {
                    span: Span {
                        start: token.start as u32,
                        end: token.end as u32,
                    },
                    kind,
                });
            }
        }
        comments
    }

    /// Classify a `Kind::Comment` token by its leading source byte (KTD3):
    /// `//` → [`CommentKind::Line`], `/*` → [`CommentKind::Block`], and an
    /// AppBuilder `&`-directive line → [`CommentKind::Line`]. Any other leading
    /// byte is excluded and `debug_assert!`-tripped: no current lexer route
    /// produces one, so hitting it means a lexer change added a `Comment` shape
    /// the classifier must be taught rather than silently dropping a comment.
    /// The `{` arm is defensive only — include/preprocessor references never lex
    /// to `Kind::Comment` today (KTD3) — so it is excluded without tripping the
    /// assert.
    fn classify_comment(&self, start: usize) -> Option<CommentKind> {
        let bytes = self.source.as_bytes();
        match bytes.get(start) {
            Some(b'/') => match bytes.get(start + 1) {
                Some(b'*') => Some(CommentKind::Block),
                // `//` line comment; also the defensive fall-through for any
                // lone `/`-led comment, which no lexer route emits.
                _ => Some(CommentKind::Line),
            },
            Some(b'&') => Some(CommentKind::Line),
            // Defensive: no current lexer route emits a `{`-led `Kind::Comment`
            // (include/preprocessor refs are AST nodes). Excluded, no assert.
            Some(b'{') => None,
            _ => {
                debug_assert!(
                    false,
                    "Kind::Comment token at byte {start} has unexpected leading byte {:?}; \
                     a lexer change added a comment shape the classifier must be taught",
                    bytes.get(start).map(|b| *b as char),
                );
                None
            }
        }
    }

    /// Record a recovered parse error in the accumulator, capping the total at
    /// [`MAX_ERRORS`](Self::MAX_ERRORS) so pathological input cannot grow the
    /// vector without bound. Errors past the cap are dropped (the parse still
    /// terminates because recovery always makes forward progress).
    fn record_error(&mut self, err: ParseError) {
        if self.errors.len() < Self::MAX_ERRORS {
            self.errors.push(err);
        }
    }

    /// Parse one statement inside a block body with per-statement error
    /// recovery, mirroring the top-level [`parse_program`](Self::parse_program)
    /// loop. On success the statement is pushed into `out`. On error the error
    /// is recorded and the cursor is synchronized to the next statement
    /// boundary.
    ///
    /// Recovery is bounded to the enclosing block: [`synchronize`](Self::synchronize)
    /// stops at any statement-starting keyword, and `END` is one of them
    /// (see [`can_start_statement`](statements::can_start_statement)), so it
    /// never consumes the block's own `END`/`END CASE`/… terminator nor
    /// overruns into a parent block — nesting stays intact.
    ///
    /// Always makes forward progress unless already at EOF, so every caller
    /// must also bound its loop with `!self.at_end()` to terminate on an
    /// unterminated block.
    fn parse_block_statement_recovering(&mut self, out: &mut Vec<Statement>) {
        let pos_before = self.current;
        match self.parse_statement() {
            Ok(stmt) => {
                out.push(stmt);
                // Incremental R1.3 check: the just-pushed statement must not
                // start before the previous sibling ends. O(1) per push covers
                // every block body assembled through this funnel.
                #[cfg(debug_assertions)]
                if let [.., prev, last] = out.as_slice() {
                    debug_assert!(
                        prev.span.end <= last.span.start,
                        "sibling statement spans out of source order: {:?} then {:?}",
                        prev.span,
                        last.span,
                    );
                }
            }
            Err(err) => {
                self.record_error(err);
                self.synchronize();
                // Force progress if neither parse_statement nor synchronize
                // advanced (e.g. stuck on a keyword synchronize stops at but
                // parse_statement rejects), but never step past EOF.
                if self.current == pos_before && !self.at_end() {
                    self.advance();
                }
            }
        }
    }

    /// Close a block body that terminates with `END`. If `END` is present it is
    /// consumed and the terminating period is expected (a missing period is
    /// recorded, not returned). If `END` is absent — an unterminated block at
    /// EOF — the missing-`END` error is recorded rather than returned, so the
    /// enclosing block statement survives with the statements parsed so far.
    ///
    /// `trailing` names the optional keyword ABL allows between `END` and the
    /// period for this block kind (`END CASE`, `END FUNCTION`, …); pass `None`
    /// for a bare `END .` (DO/FOR/REPEAT). Blocks that accept more than one
    /// trailing keyword handle their terminator inline instead.
    fn recover_block_end(&mut self, trailing: Option<Kind>, end_msg: &str, period_msg: &str) {
        if self.check(Kind::End) {
            self.advance();
            if let Some(kw) = trailing
                && self.check(kw)
            {
                self.advance();
            }
            if let Err(err) = self.expect_period(period_msg) {
                self.record_error(err);
            }
        } else {
            self.record_error(ParseError {
                message: end_msg.to_string(),
                span: self.current_span(),
            });
        }
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
    ///
    /// Returns the half-open token range `[start, end)` it consumed. See
    /// [`skipped_stmt`](Self::skipped_stmt) for why the range is `#[must_use]`.
    #[must_use = "a dispatch site that skips a statement must harvest its identifiers — \
                  see Parser::skipped_stmt; a caller that keeps a real statement node \
                  discards the range with `let _ =`"]
    pub(crate) fn skip_to_period(&mut self) -> (usize, usize) {
        let start = self.current;
        while !self.at_end() {
            if self.check(Kind::Period) {
                self.advance(); // consume the period
                return self.skip_range(start);
            }
            self.advance();
        }
        self.skip_range(start)
    }

    /// Like skip_to_period but treats `.identifier` on the same line as field access,
    /// only stopping at a period that terminates a statement (not followed by an identifier).
    ///
    /// Returns the half-open token range `[start, end)` it consumed. See
    /// [`skipped_stmt`](Self::skipped_stmt) for why the range is `#[must_use]`.
    #[must_use = "a dispatch site that skips a statement must harvest its identifiers — \
                  see Parser::skipped_stmt; a caller that keeps a real statement node \
                  discards the range with `let _ =`"]
    pub(crate) fn skip_to_statement_end(&mut self) -> (usize, usize) {
        let start = self.current;
        while !self.at_end() {
            if self.check(Kind::Period) {
                let period_end = self.tokens[self.current].end;
                let is_field_access = self.tokens.get(self.current + 1).is_some_and(|t| {
                    Self::can_be_identifier(t.kind)
                        && !self.source[period_end..t.start].contains('\n')
                });
                if !is_field_access {
                    self.advance(); // consume the terminating period
                    return self.skip_range(start);
                }
            }
            self.advance();
        }
        self.skip_range(start)
    }

    /// Like skip_to_statement_end, but also handles EDITING: ... END. sub-blocks
    /// that appear inside UPDATE/SET/PROMPT-FOR statements.  When EDITING: is
    /// encountered, the body is parsed as a full block (statements until END.)
    /// so that periods inside the editing block are not mistaken for the
    /// statement terminator.
    ///
    /// Returns the half-open token range `[start, end)` it consumed. See
    /// [`skipped_stmt`](Self::skipped_stmt) for why the range is `#[must_use]`.
    /// The `EDITING:` body is fully parsed and then thrown away, but
    /// `parse_block_body` advances the cursor past it, so the returned range
    /// still covers those tokens and the lexical harvest reaches them.
    #[must_use = "a dispatch site that skips a statement must harvest its identifiers — \
                  see Parser::skipped_stmt; a caller that keeps a real statement node \
                  discards the range with `let _ =`"]
    pub(crate) fn skip_to_statement_end_editing_aware(&mut self) -> (usize, usize) {
        let start = self.current;
        while !self.at_end() {
            // Detect EDITING: — parse the editing block body.
            if self.check(Kind::Editing) && self.check_at(1, Kind::Colon) {
                self.advance(); // consume EDITING
                self.advance(); // consume ':'
                // The editing body recurses through `parse_statement`, which
                // overwrites the enclosing statement's harvest anchor. This is
                // the only path between a `parse_statement` anchor store and the
                // matching `skipped_stmt` read that recurses, so the save and
                // restore live here — on a cold path — rather than in the
                // funnel, which every statement pays for.
                let outer_anchor = self.stmt_start_token;
                let _ = self.parse_block_body();
                self.stmt_start_token = outer_anchor;
                return self.skip_range(start);
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
                    return self.skip_range(start);
                }
            }
            self.advance();
        }
        self.skip_range(start)
    }

    /// Like skip_to_statement_end, but also skips over TRIGGERS: ... END TRIGGERS. blocks
    /// that appear as part of a CREATE widget ASSIGN ... construct.  Without this, the
    /// first statement-terminating period *inside* the trigger sub-block would be mistaken
    /// for the end of the CREATE statement, leaving block nesting misaligned.
    ///
    /// Returns the half-open token range `[start, end)` it consumed. See
    /// [`skipped_stmt`](Self::skipped_stmt) for why the range is `#[must_use]`.
    #[must_use = "a dispatch site that skips a statement must harvest its identifiers — \
                  see Parser::skipped_stmt; a caller that keeps a real statement node \
                  discards the range with `let _ =`"]
    pub(crate) fn skip_to_statement_end_triggers_aware(&mut self) -> (usize, usize) {
        let start = self.current;
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
                            return self.skip_range(start);
                        }
                    }
                    self.advance();
                }
                return self.skip_range(start);
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
                    return self.skip_range(start);
                }
            }
            self.advance();
        }
        self.skip_range(start)
    }

    /// The half-open token range a skip helper consumed, `[start, self.current)`.
    ///
    /// The `debug_assert!` is the skip-loop hazard guard from
    /// `docs/solutions/logic-errors/recursive-descent-skip-to-sync-infinite-loop.md`:
    /// a helper that returned without advancing would produce an empty range
    /// here, and the same non-advancing path is the one that hangs the parser.
    #[inline]
    fn skip_range(&self, start: usize) -> (usize, usize) {
        debug_assert!(
            self.current >= start,
            "a skip helper must never move the cursor backwards"
        );
        (start, self.current)
    }

    /// Build a [`StatementKind::Skipped`] node for a recognized-but-unmodelled
    /// statement form, harvesting candidate identifiers out of the tokens the
    /// skip helper passed over.
    ///
    /// `hi` is the exclusive end of the range the helper returned; the range
    /// *start* is deliberately not the helper's — it is
    /// [`stmt_start_token`](Self::stmt_start_token), the statement's own first
    /// token, so that dropping index 0 strips the dispatch keyword whether or not
    /// the dispatch site consumed it before skipping.
    ///
    /// The skip helpers return their range as `#[must_use]` so that adding a new
    /// unmodelled form the old way — skip, then `StatementKind::Empty` — is a
    /// clippy failure under CI's `-D warnings` rather than a silently
    /// reintroduced false-positive class.
    ///
    /// `#[inline(never)]`: there are ~29 call sites inside `parse_statement_inner`,
    /// already the parser's largest function. Inlining a `Vec`-building loop at
    /// every one of them bloats the dispatch chain that *every* statement walks,
    /// which measured as a broad regression on fixtures containing no unmodelled
    /// forms at all. Out-of-lining costs one call on a cold path.
    #[inline(never)]
    pub(crate) fn skipped_stmt(&mut self, hi: usize) -> Statement {
        let names = self.harvest_skipped_names(self.stmt_start_token, hi);
        self.stmt(StatementKind::Skipped {
            names,
            may_reference_tables: false,
        })
    }

    /// [`skipped_stmt`](Self::skipped_stmt) for a form whose operands are **not
    /// symbols**: the node carries an empty name list.
    ///
    /// `COMPILE some/path.p SAVE.` is the case. Its operand is a *file path* and
    /// its trailing words are grammar keywords, so no identifier in the statement
    /// is a reference to anything — the harvest credited nothing true, while
    /// actively suppressing real variables whose names collided with a path
    /// segment or with `SAVE`. Measured over a large real-world codebase, that
    /// made `COMPILE` one of the two forms that dominate the unmodelled-statement
    /// suppression, and the worst of them: it is the one whose suppression is
    /// *entirely* spurious.
    ///
    /// A separate entry point rather than an `Option`-ised name list on
    /// [`skipped_stmt`](Self::skipped_stmt), mirroring how
    /// [`skipped_table_stmt`](Self::skipped_table_stmt) was added: the shape is
    /// the exception, and a defaulted constructor keeps every existing call site
    /// correct by construction. The statement still emits `Skipped`, not an
    /// error-recovery `Empty` — the form *was* recognized, which is a different
    /// fact from a parse failure and one the span still records.
    #[must_use]
    pub(crate) fn skipped_stmt_no_names(&mut self) -> Statement {
        self.stmt(StatementKind::Skipped {
            names: Vec::new(),
            may_reference_tables: false,
        })
    }

    /// [`skipped_stmt`](Self::skipped_stmt) for a form whose grammar names a
    /// table or temp-table: `DEFINE QUERY` and `OPEN QUERY` (#130).
    ///
    /// Identical harvest, `may_reference_tables: true`. The marker is what earns
    /// the harvest a second, buffer/table-namespace lookup in the semantic pass;
    /// without it a temp-table used only by a query looks untouched. These forms
    /// are unmodelled, so every harvested token is a table candidate, including
    /// the query's own name — the resolve-side lookup is silent on a miss, so an
    /// over-inclusive candidate costs nothing.
    ///
    /// Deliberately a separate entry point rather than a boolean parameter on
    /// `skipped_stmt`: the marked forms number three against roughly thirty
    /// unmarked ones, and a defaulted-`false` constructor keeps every existing
    /// call site correct by construction.
    #[inline(never)]
    pub(crate) fn skipped_table_stmt(&mut self, hi: usize) -> Statement {
        let names = self.harvest_skipped_names(self.stmt_start_token, hi);
        self.stmt(StatementKind::Skipped {
            names,
            may_reference_tables: true,
        })
    }

    /// A marked [`StatementKind::Skipped`] node carrying names the parser
    /// identified *exactly*, rather than by lexical harvest.
    ///
    /// `EMPTY TEMP-TABLE tt` is the one #130 form whose grammar the parser walks
    /// token by token, so it knows precisely which identifier is the table and
    /// must not sweep in `NO-ERROR` or the `TEMP-TABLE` keyword alongside it.
    pub(crate) fn skipped_table_stmt_with(&mut self, names: Vec<Identifier>) -> Statement {
        self.stmt(StatementKind::Skipped {
            names,
            may_reference_tables: true,
        })
    }

    /// Filter the tokens in `[lo, hi)` down to the names worth offering the
    /// semantic pass (KTD5).
    ///
    /// Three rules, in order:
    ///
    /// 1. **Drop index `lo`** — always the keyword that selected the dispatch
    ///    arm. Without this, `PUT v-total.` would harvest `put`.
    /// 2. **Keep only `can_be_identifier` kinds** — deliberately the same broad
    ///    classifier the skip helpers use for their period-vs-field-access test.
    ///    It admits unreserved option keywords (`VALUE`, `FORMAT`, `LABEL`,
    ///    `FRAME`, …) as candidate names. That is accepted, not accidental: ABL
    ///    lexes a user's variable named `value` as `Kind::Value` everywhere, so
    ///    inside a statement whose grammar we do not model the two are genuinely
    ///    indistinguishable. The resolve pass's non-diagnostic lookup is the real
    ///    filter — a harvested keyword suppresses nothing unless a same-named
    ///    variable is actually in scope.
    /// 3. **Drop tokens byte-adjacent to a preceding `.`, `:` or `/`** —
    ///    adjacency, not mere precedence, mirroring `consume_widget_name`'s
    ///    `next.start != name_end` test. That reduces `table.field` to `table`,
    ///    `obj:attr` to `obj`, and `/usr/tmp/log.txt` to nothing, while keeping
    ///    spaced division operands (`PUT v-total / v-count.`) and the
    ///    statement-leading names inside an `EDITING:` block, which follow a
    ///    terminating period and a space.
    ///
    /// Allocates nothing when the filter keeps nothing, which is the common case
    /// for `PAUSE 1.`-shaped forms.
    #[inline(never)]
    fn harvest_skipped_names(&self, lo: usize, hi: usize) -> Vec<Identifier> {
        let mut names: Vec<Identifier> = Vec::new();
        let hi = hi.min(self.tokens.len());
        for i in (lo + 1)..hi {
            let tok = &self.tokens[i];
            if !Self::can_be_identifier(tok.kind) {
                continue;
            }
            let prev = &self.tokens[i - 1];
            if matches!(prev.kind, Kind::Period | Kind::Colon | Kind::Slash)
                && prev.end == tok.start
            {
                continue;
            }
            names.push(Identifier {
                name: self.source[tok.start..tok.end].to_string(),
                span: Span {
                    start: tok.start as u32,
                    end: tok.end as u32,
                },
            });
        }
        names
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
                    // System handles usable in expression position (#58 seed set)
                    | Kind::AuditControl
                    | Kind::AuditPolicy
                    | Kind::DefaultWindow
                    | Kind::Clipboard
                    | Kind::Compiler
                    | Kind::Debugger
                    | Kind::Profiler
                    | Kind::LogManager
                    | Kind::RcodeInformation
                    | Kind::CodebaseLocator
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
                    name.push_str(&self.identifier_source(next.start, next.end));
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
        let mut name = self.identifier_source(start_tok.start, start_tok.end);

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
                    name.push_str(&self.identifier_source(next.start, next.end));
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
            name: self.identifier_source(start, end),
        })
    }

    /// ABL on UNIX accepts backslash as an alternative escape character. The
    /// token span stays on the authored bytes, while semantic identity uses the
    /// spelling the compiler sees after escape removal.
    fn identifier_source(&self, start: usize, end: usize) -> String {
        let authored = &self.source[start..end];
        if authored.contains('\\') {
            authored.replace('\\', "")
        } else {
            authored.to_string()
        }
    }

    fn current_span(&self) -> Span {
        Span {
            start: self.peek().start as u32,
            end: self.peek().end as u32,
        }
    }

    /// Byte end offset of the most recently consumed non-comment token (KTD3).
    ///
    /// Used to compute span `hi`. `advance()` skips trailing comments, so
    /// `self.current - 1` may point at a comment token between statements;
    /// this scans backward past any such comments so a statement/expression
    /// span never absorbs a trailing comment. Returns `0` if nothing has been
    /// consumed (only reachable before any dispatch token).
    fn prev_end(&self) -> u32 {
        let mut i = self.current;
        while i > 0 {
            i -= 1;
            if self.tokens[i].kind != Kind::Comment {
                return self.tokens[i].end as u32;
            }
        }
        0
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

    /// Parse a parenthesized parameter list for METHOD/CONSTRUCTOR/FUNCTION.
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

                // TABLE-HANDLE <name> [APPEND] [BIND] [BY-VALUE] — table handle parameter (no AS/LIKE)
                if self.check(Kind::TableHandle) {
                    self.advance(); // consume TABLE-HANDLE
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
                            kind: HandleParamKind::TableHandle,
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

                // Type-only (unnamed) parameter — legal in FUNCTION prototypes:
                //   (INPUT CHARACTER) FORWARD.
                //   (CHARACTER, INTEGER) IN SUPER.
                //   (INPUT CLASS Progress.Lang.Object) FORWARD.
                // Named form keeps requiring AS/LIKE: (INPUT x AS CHARACTER).
                if self.looks_like_type_only_param() {
                    let type_start = self.current_span().start;
                    let data_type = self.parse_data_type()?;
                    if self.check(Kind::Extent) {
                        self.advance();
                        if self.check(Kind::IntegerLiteral)
                            || self.check(Kind::Preprop)
                            || self.check(Kind::IncludeArgReference)
                        {
                            self.advance();
                        }
                    }
                    let no_undo = if self.check(Kind::NoUndo) {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    // Synthetic name keeps the AST well-formed; prototypes discard
                    // these statements, and named definitions never use this arm.
                    let name = Identifier {
                        name: format!("${}", params.len()),
                        span: Span {
                            start: type_start,
                            end: self.current_span().start,
                        },
                    };
                    params.push(self.stmt(StatementKind::DefineParameter {
                        direction,
                        param_type: ParameterType::Variable {
                            name,
                            type_source: TypeSource::Explicit(data_type),
                            no_undo,
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

    /// True when the next tokens are a type-only (unnamed) parameter rather than
    /// `name AS/LIKE type`. Used for FUNCTION prototype signatures where ABL
    /// permits omitting the parameter name (#68 PARSE001 follow-up).
    fn looks_like_type_only_param(&self) -> bool {
        let k = self.peek().kind;
        // CLASS Foo / Progress.Lang… always introduce a type, never a param name.
        if matches!(k, Kind::Class | Kind::Progress) {
            return true;
        }
        // Primitive type keywords and bare identifiers: type-only unless the
        // next token is AS/LIKE (then the current token is the parameter name).
        // A following `.` is a class path (`forms.deco`) — also type-only.
        if Self::is_primitive_data_type_kind(k) || Self::can_be_identifier(k) {
            let next = self.peek_nth_non_comment(2);
            if next.kind == Kind::Period {
                return true;
            }
            if next.kind == Kind::KwAs || next.kind == Kind::Like {
                return false;
            }
            return true;
        }
        false
    }

    /// Built-in data type keyword kinds recognized by [`Self::parse_data_type`].
    fn is_primitive_data_type_kind(kind: Kind) -> bool {
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
                | Kind::KwIn // "in" abbreviates integer
        )
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

#[cfg(all(test, feature = "serde"))]
mod serde_tests {
    use crate::Parser;
    use oxabl_lexer::tokenize;

    #[test]
    fn parse_error_serializes_under_feature() {
        let src = "DEFINE VARIABLE .";
        let tokens = tokenize(src);
        let program = Parser::new(&tokens, src).parse_program();
        let err = program.first_error().expect("expected a parse error");
        let v = serde_json::to_value(err).unwrap();
        assert!(v["message"].is_string());
        assert!(v["span"]["start"].is_number());
        assert!(v["span"]["end"].is_number());
    }
}
