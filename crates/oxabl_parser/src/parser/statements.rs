//! Statement parsing for the Oxabl parser.
//!
//! Handles DEFINE VARIABLE, VAR, ASSIGN, assignments, DO blocks (with counting loops),
//! IF/THEN/ELSE, REPEAT, FOR EACH, FIND, CASE, PROCEDURE, FUNCTION, RUN, DISPLAY,
//! MESSAGE, CLASS, INTERFACE, METHOD, PROPERTY, CONSTRUCTOR, DESTRUCTOR, USING,
//! PUBLISH, SUBSCRIBE, UNSUBSCRIBE, DEFINE EVENT,
//! LEAVE, NEXT, and RETURN statements.

use oxabl_ast::{
    AccessModifier, AssignPair, BufferTarget, CreateTarget, CreateTargetKind, DataRelation,
    DataSourceBuffer, DataSourceKeys, DbTriggerEvent, DisplayItem, Expression, ExpressionKind,
    FindType, HandleParamKind, HandlePassingOptions, Identifier, IndexField, Literal, LockType,
    OnAction, OnEventClause, OnKind, ParameterDirection, ParameterType, ParentIdRelation,
    PreprocIf, RunArgument, RunTarget, SortDirection, Span, Statement, StatementKind,
    StreamDirection, StreamOperation, StringLiteral, SubscribeTarget, TempTableField,
    TempTableIndex, TriggerAssignParam, TriggerReferencing, TypeSource, UnknownLiteral, UseIndex,
    WhenBranch, WidgetQualifier, WidgetRef, XmlSerializeOptions,
};
use oxabl_lexer::Kind;
use oxabl_lexer::TokenValue;
use smallvec::SmallVec;

use super::{ParseError, ParseResult, Parser};

/// Returns true if the given Kind can start a new top-level statement.
///
/// Used for error recovery: if we encounter a statement-starting keyword while
/// parsing inside a DEFINE TEMP-TABLE body, it likely means a period was missed.
pub(crate) fn can_start_statement(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Define
            | Kind::Do
            | Kind::KwIf
            | Kind::Repeat
            | Kind::KwFor
            | Kind::Find
            | Kind::Case
            | Kind::Procedure
            | Kind::Run
            | Kind::Display
            | Kind::Message
            | Kind::KwReturn
            | Kind::Leave
            | Kind::Next
            | Kind::End
            | Kind::Variable
            | Kind::Function
            | Kind::Class
            | Kind::Interface
            | Kind::Using
            | Kind::Method
            | Kind::Constructor
            | Kind::Destructor
            | Kind::Create
            | Kind::Delete
            | Kind::Release
            | Kind::Validate
            | Kind::BufferCopy
            | Kind::BufferCompare
            | Kind::PreprocIf
            | Kind::PreprocScopedDefine
            | Kind::PreprocGlobalDefine
            | Kind::PreprocUndefine
            | Kind::PreprocMessage
            | Kind::Input
            | Kind::Output
            | Kind::InputOutput
            | Kind::Publish
            | Kind::Subscribe
            | Kind::Unsubscribe
            | Kind::On
            | Kind::Trigger
            | Kind::Export
            | Kind::Dos
            | Kind::Unix
            | Kind::BlockLevel
            | Kind::RoutineLevel
            | Kind::Throw
            | Kind::Select
            | Kind::Insert
    )
}

impl Parser<'_> {
    /// Parse one statement and stamp its full-extent span (`lo..prev_end`,
    /// including the trailing `.`/`:`) onto the returned wrapper.
    ///
    /// This is a thin funnel over [`parse_statement_inner`](Self::parse_statement_inner)
    /// (KTD2): `lo` is captured from the first token before dispatch, and `hi`
    /// is the end of the last non-comment token consumed. Because statement
    /// bodies are parsed by recursive `parse_statement` calls, every statement —
    /// nested or top-level — is stamped at its own dispatch. Genuinely
    /// token-less synthetic nodes (nothing consumed) collapse to a zero-width
    /// span at `lo` (R1.4).
    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        let lo = self.peek().start as u32;
        // Anchor the unmodelled-form harvest range on this statement's first
        // token. A bare store, deliberately: this is the parser's hottest
        // funnel, and save/restore around the call measured ~1.5% on the
        // existing fixtures. Correctness does not need it — between this store
        // and the matching `skipped_stmt` read, the only path that recurses into
        // `parse_statement` is the `EDITING:` body, which saves and restores the
        // anchor itself (`skip_to_statement_end_editing_aware`).
        self.stmt_start_token = self.current;
        let mut stmt = self.parse_statement_inner()?;
        let hi = self.prev_end().max(lo);
        stmt.span = Span { start: lo, end: hi };
        Ok(stmt)
    }

    fn parse_statement_inner(&mut self) -> ParseResult<Statement> {
        // Skip empty statements
        if self.check(Kind::Period) {
            self.advance();
            return Ok(self.stmt(StatementKind::Empty));
        }

        // CATCH / FINALLY blocks — must be checked before the block-label heuristic because
        // FINALLY and CATCH are in can_be_identifier(), and `FINALLY: FOR EACH ...` would
        // otherwise be mis-parsed as a labeled FOR EACH block.
        if self.check(Kind::Catch) {
            return self.parse_catch_block();
        }
        if self.check(Kind::Finally) {
            return self.parse_finally_block();
        }

        // Implicit widget method call: :method-name(args) [IN FRAME frame-name].
        // A bare colon at position 0 indicates a method call on the currently focused
        // widget (set by a prior CHOOSE or selection-list interaction).
        if self.check(Kind::Colon) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // Block label: `LABEL: DO: ...` or `LABEL: REPEAT: ...`
        // An identifier (or identifier-like keyword) followed by a colon where
        // the token after the colon is a block-starting keyword.
        // Check the colon first: it fails for the vast majority of statements
        // (e.g. `ASSIGN x = ...` where position +1 is the variable name, not
        // a colon), avoiding the more expensive `can_be_identifier` call.
        // Use peek_nth_non_comment(3) to skip over inline comments between ':' and the block
        // keyword (e.g. `LABEL: /* comment */\n DO:` has a Comment at peek_at(2)).
        if self.check_at(1, Kind::Colon)
            && Self::can_be_identifier(self.peek().kind)
            && matches!(
                self.peek_nth_non_comment(3).kind,
                Kind::Do | Kind::Repeat | Kind::KwFor
            )
        {
            let token = self.advance().clone(); // consume label name
            let name = self.source[token.start..token.end].to_string();
            self.advance(); // consume ':'
            let body = self.parse_statement()?;
            return Ok(self.stmt(StatementKind::Label {
                name,
                body: Box::new(body),
            }));
        }

        // Numeric-prefixed block label: `1TO99-MILLIONS: DO:` — ABL allows labels that
        // start with a digit.  The lexer splits this into IntegerLiteral + Identifier + Colon.
        // Detect and discard the label so the inner block statement parses normally.
        if self.check(Kind::IntegerLiteral)
            && Self::can_be_identifier(self.peek_at(1).kind)
            && self.check_at(2, Kind::Colon)
            && matches!(
                self.peek_nth_non_comment(4).kind,
                Kind::Do | Kind::Repeat | Kind::KwFor
            )
        {
            let start = self.tokens[self.current].start;
            self.advance(); // consume integer part
            let end = self.tokens[self.current - 1].end;
            let name_start = start;
            let ident_tok = self.advance().clone(); // consume identifier part
            let name = format!(
                "{}{}",
                &self.source[name_start..end],
                &self.source[ident_tok.start..ident_tok.end]
            );
            self.advance(); // consume ':'
            let body = self.parse_statement()?;
            return Ok(self.stmt(StatementKind::Label {
                name,
                body: Box::new(body),
            }));
        }

        // DO blocks
        if self.check(Kind::Do) {
            return self.parse_do_statement();
        }

        // IF statement
        if self.check(Kind::KwIf) {
            return self.parse_if_statement();
        }

        // Repeat block
        if self.check(Kind::Repeat) {
            return self.parse_repeat_statement();
        }

        // LEAVE [label].
        if self.check(Kind::Leave) {
            self.advance();
            let label = if Self::can_be_identifier(self.peek().kind) {
                let token = self.advance().clone();
                Some(self.source[token.start..token.end].to_string())
            } else {
                None
            };
            self.expect_period("Expected '.' to come after LEAVE")?;
            return Ok(self.stmt(StatementKind::Leave(label)));
        }

        // NEXT [label].
        if self.check(Kind::Next) {
            self.advance();
            let label = if Self::can_be_identifier(self.peek().kind) {
                let token = self.advance().clone();
                Some(self.source[token.start..token.end].to_string())
            } else {
                None
            };
            self.expect_period("Expected '.' to come after NEXT")?;
            return Ok(self.stmt(StatementKind::Next(label)));
        }

        // QUIT — exits the entire program; no label.
        if self.check(Kind::Quit) {
            self.advance();
            self.expect_period("Expected '.' after QUIT")?;
            return Ok(self.stmt(StatementKind::Empty));
        }

        // Bare END [qualifier]. — an orphaned block terminator appearing at the top-level
        // parse loop. This most commonly occurs when a preprocessor-defined method/procedure
        // body (e.g. `{&methodType} name {&returnType} (...):`) is incorrectly skipped as a
        // preprop expression statement, leaving the closing END visible at the top level.
        // Silently consume END [PROCEDURE|FUNCTION|METHOD|CLASS|INTERFACE|CONSTRUCTOR|DESTRUCTOR]
        // followed by an optional period.
        if self.check(Kind::End) {
            self.advance(); // consume END
            if matches!(
                self.peek().kind,
                Kind::Procedure
                    | Kind::Function
                    | Kind::Method
                    | Kind::Class
                    | Kind::Interface
                    | Kind::Constructor
                    | Kind::Destructor
                    | Kind::Trigger
            ) {
                self.advance(); // consume qualifier
            }
            if self.check(Kind::Period) {
                self.advance(); // consume '.'
            }
            return Ok(self.stmt(StatementKind::Empty));
        }

        // UNDO [label] [, LEAVE/RETRY/NEXT/RETURN [label]].
        if self.check(Kind::Undo) {
            self.advance(); // consume UNDO
            // consume optional block label
            if Self::can_be_identifier(self.peek().kind)
                && !self.check(Kind::Comma)
                && !self.check(Kind::Period)
            {
                self.advance();
            }
            // consume optional action
            if self.check(Kind::Comma) {
                self.advance();
                if self.check(Kind::KwReturn) {
                    self.advance();
                    // RETURN may have a value expression (e.g. UNDO, RETURN false.)
                    if !self.check(Kind::Period) && !self.at_end() {
                        self.parse_expression().ok();
                    }
                } else if matches!(self.peek().kind, Kind::Leave | Kind::Retry | Kind::Next) {
                    self.advance();
                    // consume optional label
                    if Self::can_be_identifier(self.peek().kind) && !self.check(Kind::Period) {
                        self.advance();
                    }
                } else if self.check(Kind::Throw) {
                    self.advance(); // consume THROW
                    // parse the thrown expression (e.g. NEW Progress.Lang.AppError(...))
                    if !self.check(Kind::Period) && !self.at_end() {
                        self.parse_expression().ok();
                    }
                }
            }
            self.expect_period("Expected '.' after UNDO statement")?;
            return Ok(self.stmt(StatementKind::Empty));
        }

        // Return
        if self.check(Kind::KwReturn) {
            return self.parse_return_statement();
        }

        // FOR EACH
        if self.check(Kind::KwFor) {
            return self.parse_for_each();
        }

        // FIND statement
        if self.check(Kind::Find) {
            return self.parse_find_statement();
        }

        // CASE statement
        if self.check(Kind::Case) {
            return self.parse_case_statement();
        }

        // RUN statement
        if self.check(Kind::Run) {
            return self.parse_run_statement();
        }

        // Event system statements
        if self.check(Kind::Publish) {
            return self.parse_publish_statement();
        }
        if self.check(Kind::Subscribe) {
            return self.parse_subscribe_statement();
        }
        if self.check(Kind::Unsubscribe) {
            return self.parse_unsubscribe_statement();
        }

        // ON triggers
        // NOTE: ON in block headers (DO ON ERROR UNDO) is consumed inside
        // parse_do_statement/parse_for_each/parse_repeat_statement.
        // Any ON reaching here is always a trigger statement.
        if self.check(Kind::On) {
            return self.parse_on_statement();
        }

        // TRIGGER PROCEDURE
        if self.check(Kind::Trigger) {
            return self.parse_trigger_procedure();
        }

        // PROCEDURE statement
        if self.check(Kind::Procedure) {
            return self.parse_procedure();
        }

        // DISPLAY statement
        if self.check(Kind::Display) {
            return self.parse_display_statement();
        }

        // MESSAGE statement
        if self.check(Kind::Message) {
            return self.parse_message_statement();
        }

        // EXPORT statement: EXPORT [STREAM stream-name] [DELIMITER char] [SKIP [n]] expr... .
        if self.check(Kind::Export) {
            self.advance(); // consume EXPORT
            // Use skip_to_statement_end so field-access dots (e.g. vendor.vend-number) are not
            // mistaken for the statement-terminating period.
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // DOS / UNIX / VMS statements: launch the OS command interpreter.
        // Use skip_to_statement_end() so that field-access periods inside VALUE(db.field)
        // are not mistaken for the statement-terminating period.
        // VMS is not a reserved keyword so it lexes as Kind::Identifier.
        if self.check(Kind::Dos) || self.check(Kind::Unix) {
            self.advance();
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }
        if self.check(Kind::Identifier) {
            let tok = self.peek();
            if self.source[tok.start..tok.end].eq_ignore_ascii_case("vms") {
                self.advance();
                let (_, hi) = self.skip_to_statement_end();
                return Ok(self.skipped_stmt(hi));
            }
        }

        // COMPILE VALUE(path) [OPTIONS ...] [SAVE] [NO-ERROR]. — compile an ABL program.
        if self.check(Kind::Compile) {
            self.advance(); // consume COMPILE
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // PROCESS EVENTS. — flush the ABL event queue.
        if self.check(Kind::Process) {
            self.advance(); // consume PROCESS
            let (_, hi) = self.skip_to_period();
            return Ok(self.skipped_stmt(hi));
        }

        // SYSTEM-DIALOG PRINTER-SETUP [UPDATE var]. — OS print-setup dialog.
        if self.check(Kind::SystemDialog) {
            self.advance(); // consume SYSTEM-DIALOG
            let (_, hi) = self.skip_to_period();
            return Ok(self.skipped_stmt(hi));
        }

        // BLOCK-LEVEL / ROUTINE-LEVEL ON ERROR UNDO, THROW. — error propagation directives.
        if self.check(Kind::BlockLevel) || self.check(Kind::RoutineLevel) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // THROW expr. — raise an exception.
        if self.check(Kind::Throw) {
            self.advance(); // consume THROW
            if !self.check(Kind::Period) && !self.at_end() {
                self.parse_expression().ok();
            }
            self.expect_period("Expected '.' after THROW")?;
            return Ok(self.stmt(StatementKind::Empty));
        }

        // ASSIGN statement
        if self.check(Kind::Assign) {
            return self.parse_assign_statement();
        }

        // Database manipulation statements
        if self.check(Kind::Create) {
            return self.parse_create_statement();
        }
        if self.check(Kind::Delete) {
            return self.parse_delete_statement();
        }
        if self.check(Kind::Release) {
            return self.parse_release_statement();
        }
        if self.check(Kind::Validate) {
            return self.parse_validate_statement();
        }
        if self.check(Kind::BufferCopy) {
            return self.parse_buffer_copy();
        }
        if self.check(Kind::BufferCompare) {
            return self.parse_buffer_compare();
        }

        // Preprocessor statements
        if self.check(Kind::PreprocIf) {
            return self.parse_preproc_if_statement();
        }
        if self.check(Kind::PreprocScopedDefine) || self.check(Kind::PreprocGlobalDefine) {
            return self.parse_preproc_define();
        }
        if self.check(Kind::PreprocUndefine) {
            return self.parse_preproc_undefine();
        }
        if self.check(Kind::PreprocMessage) {
            return self.parse_preproc_message();
        }
        // Stray preprocessor block-boundary tokens left after preprocessing
        // (e.g. &ENDIF at the end of a file wrapped in &IF DEFINED(...) &THEN).
        // Consume silently — they have no semantic meaning to the parser.
        if matches!(
            self.peek().kind,
            Kind::PreprocEndif | Kind::PreprocElse | Kind::PreprocElseif
        ) {
            self.advance();
            return Ok(self.stmt(StatementKind::Empty));
        }

        // Stream I/O statements: INPUT/OUTPUT/INPUT-OUTPUT
        // Disambiguate from parameter direction or function call via lookahead
        if self.check(Kind::Input) {
            let next = self.peek_at(1).kind;
            if matches!(
                next,
                Kind::From | Kind::Through | Kind::Thru | Kind::Close | Kind::Stream
            ) {
                return self.parse_stream_io(StreamDirection::Input);
            }
        }
        if self.check(Kind::Output) {
            let next = self.peek_at(1).kind;
            if matches!(
                next,
                Kind::To
                    | Kind::Through
                    | Kind::Thru
                    | Kind::Close
                    | Kind::Stream
                    // Preprocessor reference used as stream name: OUTPUT {&stream} TO ...
                    | Kind::Preprop
            ) {
                return self.parse_stream_io(StreamDirection::Output);
            }
        }
        if self.check(Kind::InputOutput) {
            let next = self.peek_at(1).kind;
            if matches!(
                next,
                Kind::Through | Kind::Thru | Kind::Close | Kind::Stream
            ) {
                return self.parse_stream_io(StreamDirection::InputOutput);
            }
        }
        // OO-ABL statements
        if self.check(Kind::Class) {
            return self.parse_class();
        }
        if self.check(Kind::Interface) {
            return self.parse_interface();
        }
        if self.check(Kind::Using) {
            return self.parse_using();
        }
        if self.check(Kind::Method) {
            return self.parse_method();
        }
        if self.check(Kind::Constructor) {
            return self.parse_constructor();
        }
        if self.check(Kind::Destructor) {
            return self.parse_destructor();
        }

        // EMPTY TEMP-TABLE tt-name [NO-ERROR].
        //
        // Still unmodelled, but the table name is preserved as the statement's
        // single table candidate (#130). This is the one #130 form whose grammar
        // the parser walks token by token, so it names the table exactly instead
        // of harvesting the statement lexically — `NO-ERROR` and the `TEMP-TABLE`
        // keyword are roles it already knows, not candidates.
        if self.check(Kind::Empty) {
            self.advance(); // consume EMPTY
            // Optionally consume TEMP-TABLE or the table name directly
            if self.check(Kind::TempTable) {
                self.advance();
            }
            // Consume the table name
            let mut names = Vec::new();
            if Self::can_be_identifier(self.peek().kind) {
                let (start, end) = {
                    let tok = self.advance();
                    (tok.start, tok.end)
                };
                names.push(Identifier {
                    name: self.source[start..end].to_string(),
                    span: Span {
                        start: start as u32,
                        end: end as u32,
                    },
                });
            }
            // Optional NO-ERROR
            if self.check(Kind::NoError) {
                self.advance();
            }
            self.expect_period("Expected '.' after EMPTY TEMP-TABLE")?;
            return Ok(self.skipped_table_stmt_with(names));
        }

        // COPY-LOB: complex LOB manipulation statement — skip to next statement period.
        // Uses skip_to_statement_end() (not skip_to_period()) to avoid stopping at
        // '.' field-access separators inside expressions like clob_data.datawad.
        if self.check(Kind::CopyLob) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // PUT [STREAM s] UNFORMATTED expr. — stream output statement, skip to statement end.
        if self.check(Kind::Put) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // FORM ... — legacy UI form definition, skip to statement end.
        // Uses skip_to_statement_end() to skip over .field-access in form items.
        if self.check(Kind::Form) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // VIEW [STREAM s] FRAME f — UI frame display statement, skip to statement end.
        if self.check(Kind::View) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // HIDE [STREAM s] FRAME f [NO-PAUSE] — UI hide statement, skip to statement end.
        if self.check(Kind::Hide) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // UPDATE / SET / PROMPT-FOR — may contain an EDITING: ... END. sub-block.
        // Skip tokens until a statement-ending period, but if we encounter EDITING:
        // along the way, parse the editing block body before consuming the outer END.
        if self.check(Kind::Update) || self.check(Kind::Set) || self.check(Kind::PromptFor) {
            let (_, hi) = self.skip_to_statement_end_editing_aware();
            return Ok(self.skipped_stmt(hi));
        }

        // PAUSE / BELL / IMPORT / OS-DELETE / OS-DIR / OS-CREATE-DIR / OS-COMMAND / OS-COPY /
        // OPEN QUERY q FOR EACH tt ... — split out of the bulk arm below so that
        // only the query shape is marked as a table candidate (#130). The bulk arm
        // matches a bare OPEN, and marking every shape routed through it would
        // widen table lookup past the three forms this issue owns.
        if self.check(Kind::Open) && self.peek_at(1).kind == Kind::Query {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_table_stmt(hi));
        }

        // PAGE / DISABLE / ENABLE / ACCUMULATE / DOWN / OPEN / APPLY / STATUS /
        // CLEAR / HIDE — skip to end.
        // Uses skip_to_statement_end() (not skip_to_period()) so that field-access dots
        // like 'order.company' in WHERE clauses are not mistaken for statement terminators.
        if self.check(Kind::Pause)
            || self.check(Kind::Bell)
            || self.check(Kind::Import)
            || self.check(Kind::OsDelete)
            || self.check(Kind::OsDir)
            || self.check(Kind::OsCreateDir)
            || self.check(Kind::OsCommand)
            || self.check(Kind::OsCopy)
            || self.check(Kind::OsRename)
            || self.check(Kind::OsAppend)
            || self.check(Kind::Page)
            || self.check(Kind::Disable)
            || self.check(Kind::Enable)
            || self.check(Kind::Accumulate)
            || self.check(Kind::Accum) // abbreviation of ACCUMULATE
            || self.check(Kind::Down)
            || self.check(Kind::Open)
            || self.check(Kind::Apply)
            || self.check(Kind::Status)
            || self.check(Kind::Get)
            || self.check(Kind::Clear)
            || self.check(Kind::Hide)
            // FORMAT/FORM expr. — standalone field display-format spec (e.g. after DEFINE PARAMETER).
            || self.check(Kind::Format)
            || self.check(Kind::Form)
            // CLOSE QUERY/DATASET/BROWSING name. — close a query, dataset, or browse widget.
            || self.check(Kind::Close)
            // CONNECT/DISCONNECT — database connection management statements.
            || self.check(Kind::Connect)
            || self.check(Kind::Disconnect)
            // WAIT-FOR event OF widget. — ABL UI event loop.
            || self.check(Kind::WaitFor)
            // COLOR statement — terminal color settings (e.g. COLOR DISPLAY NORMAL ... WITH FRAME name.)
            || self.check(Kind::Color)
            // READKEY [PAUSE n]. — reads next keypress from keyboard.
            || self.check(Kind::Readkey)
            // UP n [WITH FRAME name]. — scroll up n lines in a frame.
            || self.check(Kind::Up)
            // GET-KEY-VALUE section "s" key "k" value var. — Windows registry read.
            || self.check(Kind::GetKeyValue)
            // NEXT-PROMPT field [WITH FRAME name]. — legacy UI cursor hint.
            || self.check(Kind::NextPrompt)
        {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // Embedded SQL: SELECT ... FROM ... / INSERT INTO ... — embedded SQL in ABL.
        if self.check(Kind::Select) || self.check(Kind::Insert) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // LOAD "key" [BASE-KEY "root"]. / UNLOAD "key". / USE "key". — Windows registry access.
        // These are not dedicated lexer kinds; detect by identifier name.
        if self.check(Kind::Identifier) {
            let tok = self.peek().clone();
            let text = self.source[tok.start..tok.end].to_ascii_lowercase();
            if matches!(text.as_str(), "load" | "unload" | "use") {
                let (_, hi) = self.skip_to_period();
                return Ok(self.skipped_stmt(hi));
            }
        }

        // REPOSITION query-name TO ... — query cursor repositioning, skip to statement end.
        // Uses skip_to_statement_end() to skip over ROWID(...) and NO-ERROR properly.
        if self.check(Kind::Reposition) && Self::can_be_identifier(self.peek_at(1).kind) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // ENUM class-name: DEFINE ENUM ... END ENUM. — ABL enumeration type
        if self.check(Kind::KwEnum) {
            self.synchronize(); // skip to end of header line
            // Skip until END ENUM.
            while !self.at_end() {
                if self.check(Kind::EndEnum) {
                    self.advance();
                    self.expect_period("Expected '.' after END ENUM")?;
                    break;
                }
                self.advance();
            }
            return Ok(self.stmt(StatementKind::Empty));
        }

        // Check for traditional define statement
        // def var name as type [no-undo] [initial value] [extent n].
        if self.check(Kind::Define) {
            return self.parse_define_statement();
        }

        // VAR statement: positive lookahead for data type keyword, LIKE, or class name (Identifier)
        if self.check(Kind::Variable)
            && (Self::is_data_type_kind(self.peek_at(1).kind)
                || self.check_at(1, Kind::Like)
                || self.peek_at(1).kind == Kind::Identifier)
        {
            return self.parse_var_statement();
        }

        // FUNCTION definition: negative lookahead (not an assignment)
        if self.check(Kind::Function) && !self.check_at(1, Kind::Equals) {
            return self.parse_function();
        }

        // Include file references: {file.i}, {file.i args}
        if self.check(Kind::IncludeReference) {
            return self.parse_include_reference_statement();
        }

        // Include positional argument references: {0}, {1}, {2}
        // But if followed by ':' (member access) or '=' (assignment), fall through to
        // expression/assignment parsing so that e.g. `{5}:font = 8.` is handled correctly.
        if self.check(Kind::IncludeArgReference)
            && !matches!(self.peek_at(1).kind, Kind::Colon | Kind::Equals)
        {
            return self.parse_include_arg_reference_statement();
        }

        // Parse left-hand assignment, stop before comparison operators
        let left = self.parse_additive()?;

        // Consume optional "IN FRAME/BROWSE framename" qualifier between target and '='
        // e.g. widget:attribute IN FRAME ftab1 = value NO-ERROR.
        // e.g. field:screen-value IN BROWSE bname = value.
        if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse) {
            self.advance(); // consume IN
            self.advance(); // consume FRAME or BROWSE
            self.advance(); // consume framename/browsename
        }

        // Compound assignment operators: +=, -=, *=, /=
        if matches!(
            self.peek().kind,
            Kind::PlusEquals | Kind::MinusEquals | Kind::StarEquals | Kind::SlashEquals
        ) {
            self.advance(); // consume the compound operator
            let value = self.parse_expression()?;
            if self.check(Kind::NoError) {
                self.advance();
            }
            self.expect_period("Expected '.' to end statement")?;
            return Ok(self.stmt(StatementKind::Assignment {
                target: left,
                value,
            }));
        }

        if self.check(Kind::Equals) {
            self.advance(); // consume the "="
            let value = self.parse_assignment_value()?;
            // Consume optional IN FRAME/BROWSE qualifier on the value side
            // e.g. handle = widget:attr IN FRAME fname.
            if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
            {
                self.advance(); // consume IN
                self.advance(); // consume FRAME or BROWSE
                self.advance(); // consume name
            }

            // ADM2 xp-property path: consecutive accessor assignments without a
            // leading ASSIGN keyword (gated off when xp-assign is defined).
            //   ghProp:BUFFER-FIELD('a':U):BUFFER-VALUE = 'x':U
            //   ghProp:BUFFER-FIELD('b':U):BUFFER-VALUE = 'y':U.
            // Collect multi-pair into Assign; single pair stays Assignment.
            if self.looks_like_bare_assign_pair_ahead() {
                let mut assignments = SmallVec::new();
                assignments.push(AssignPair {
                    target: left,
                    value,
                });
                while self.looks_like_bare_assign_pair_ahead() {
                    let target = self.parse_additive()?;
                    if self.check(Kind::KwIn)
                        && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                    {
                        self.advance(); // IN
                        self.advance(); // FRAME/BROWSE
                        self.advance(); // name
                    }
                    if !self.check(Kind::Equals) {
                        // Lookahead said `=` was ahead; if tokens moved under us,
                        // stop collecting rather than erroring mid-statement.
                        break;
                    }
                    self.advance(); // '='
                    let pair_value = self.parse_assignment_value()?;
                    if self.check(Kind::KwIn)
                        && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                    {
                        self.advance();
                        self.advance();
                        self.advance();
                    }
                    if self.check(Kind::When) {
                        self.advance();
                        self.parse_expression().ok();
                    }
                    assignments.push(AssignPair {
                        target,
                        value: pair_value,
                    });
                }
                if self.check(Kind::NoError) {
                    self.advance();
                }
                self.expect_period("Expected '.' to end statement")?;
                return Ok(self.stmt(StatementKind::Assign { assignments }));
            }

            // Consume optional NO-ERROR trailing clause (e.g. x = func() NO-ERROR.)
            if self.check(Kind::NoError) {
                self.advance();
            }
            self.expect_period("Expected '.' to end statement")?;
            return Ok(self.stmt(StatementKind::Assignment {
                target: left,
                value,
            }));
        }

        // not an assignment, continue parsing as full expression
        let expr = self.finish_expression(left)?;
        // Consume optional NO-ERROR trailing clause (e.g. handle:method() NO-ERROR.)
        if self.check(Kind::NoError) {
            self.advance();
        }
        // Fast path: statement ends with period (vast majority of cases).
        if self.check(Kind::Period) {
            self.advance();
            return Ok(self.stmt(StatementKind::ExpressionStatement(expr)));
        }

        // Unified continuation logic for implicit output / preprop macro invocations.
        //
        // ABL allows implicit output via juxtaposed expressions — multiple string
        // literals, field references, function calls, and format keywords (skip,
        // space, format, etc.) placed side-by-side with no commas.  This is the
        // dominant pattern in web-facing ABL files (WebSpeed's {&OUT} macro, bare
        // string output, function calls like get-copy() followed by display items).
        //
        // Also handles preprop macros with trailing arguments:
        //   {&out} "<h1>" smessage "</h1>".
        //
        // When a standalone preprop macro like {&leave-logic} expands to a complete
        // statement at compile time (including its own period), the next token will
        // be a statement boundary — we return without consuming.
        //
        // Safety: relies on the negative guard (can_start_statement) to prevent
        // absorbing new statements as continuation tokens.
        if !self.at_end() {
            let next = self.peek().kind;
            // Negative guard: if the next token starts a new statement, don't absorb it.
            let next_is_new_statement = can_start_statement(next)
                || matches!(
                    next,
                    Kind::PreprocIf
                        | Kind::PreprocElse
                        | Kind::PreprocElseif
                        | Kind::PreprocEndif
                        | Kind::PreprocScopedDefine
                        | Kind::PreprocGlobalDefine
                        | Kind::PreprocUndefine
                        | Kind::KwReturn
                        | Kind::End
                        | Kind::KwElse
                        | Kind::Leave
                        | Kind::Next
                );
            let next_is_block_label = Self::can_be_identifier(next)
                && self.check_at(1, Kind::Colon)
                && matches!(self.peek_at(2).kind, Kind::Do | Kind::Repeat | Kind::KwFor);

            if !next_is_new_statement && !next_is_block_label {
                let _ = self.skip_to_statement_end();
                return Ok(self.stmt(StatementKind::ExpressionStatement(expr)));
            }
            // Next token is a new statement — treat current expression as
            // already-terminated (preprop macro expanded its own period).
            return Ok(self.stmt(StatementKind::ExpressionStatement(expr)));
        }

        self.expect_period("Expected '.' to end statement")?;
        Ok(self.stmt(StatementKind::ExpressionStatement(expr)))
    }

    // parse define variable as type [no-undo] [initial]
    fn parse_define_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DEFINE

        // Skip optional preprocessor/include-arg references that may appear as modifiers
        // e.g., `DEF {&ns} VAR` or `DEF {1} SHARED TEMP-TABLE`
        while self.check(Kind::Preprop) || self.check(Kind::IncludeArgReference) {
            self.advance();
        }

        // parse INPUT/OUTPUT/RETURN parameters
        if self.check(Kind::Input)
            || self.check(Kind::Output)
            || self.check(Kind::InputOutput)
            || self.check(Kind::KwReturn)
        {
            return self.parse_define_parameter();
        }

        // DEFINE PARAMETER BUFFER b FOR [TEMP-TABLE] tt.
        //
        // Buffer parameters carry no direction in ABL — the buffer binds to the
        // caller's, so INPUT/OUTPUT would be meaningless — which is why this
        // spelling never reaches the direction-led dispatch above. Recorded as
        // `Input`, matching what the `DEFINE INPUT PARAMETER BUFFER` path
        // already produces; nothing downstream distinguishes them.
        if self.check(Kind::Parameter) && self.peek_at(1).kind == Kind::Buffer {
            self.advance(); // consume PARAMETER
            self.advance(); // consume BUFFER
            let name = self.parse_identifier()?;
            self.expect_kind(Kind::KwFor, "Expected FOR after buffer parameter name")?;
            // FOR TEMP-TABLE tt is the explicit spelling of FOR tt.
            if self.check(Kind::TempTable) {
                self.advance();
            }
            let target = self.parse_identifier()?;
            self.expect_period("Expected '.' after parameter definition")?;
            return Ok(self.stmt(StatementKind::DefineParameter {
                direction: ParameterDirection::Input,
                param_type: ParameterType::Buffer { name, target },
            }));
        }

        // Parse optional NEW [GLOBAL] SHARED / SHARED. These three modes are
        // mutually exclusive by construction (see ast-invariants.md §12): the
        // GLOBAL form sets `is_new_global_shared`, plain NEW sets
        // `is_new_shared`, and the SHARED-only consumer form sets `is_shared`.
        let mut is_new_shared = false;
        let mut is_new_global_shared = false;
        if self.check(Kind::New) && matches!(self.peek_at(1).kind, Kind::Shared | Kind::Global) {
            self.advance(); // consume NEW
            if self.check(Kind::Global) {
                self.advance(); // consume GLOBAL (NEW GLOBAL [SHARED] form)
                is_new_global_shared = true;
            } else {
                is_new_shared = true;
            }
            if self.check(Kind::Shared) {
                self.advance(); // consume the optional trailing SHARED
            }
        }
        let is_shared = if !is_new_shared && !is_new_global_shared && self.check(Kind::Shared) {
            self.advance();
            true
        } else {
            false
        };

        // OO-ABL: DEFINE [access] [STATIC] PROPERTY/DATASET/DATA-SOURCE ...
        // Check for access modifier before PROPERTY/VARIABLE/TEMP-TABLE
        let access = self.parse_access_modifier();

        // Check for STATIC after access modifier (or before it — both orders valid)
        let is_static = if self.check(Kind::KwStatic) {
            self.advance();
            true
        } else {
            false
        };

        // If STATIC came before access modifier, check for access modifier again
        let access = if access.is_none() {
            self.parse_access_modifier().or(access)
        } else {
            access
        };

        // Check for ABSTRACT (used by DEFINE EVENT)
        let is_abstract = if self.check(Kind::Abstract) {
            self.advance();
            true
        } else {
            false
        };

        // Skip preprocessor/include-arg references that expand to modifiers at
        // compile time (e.g. `DEF SHARED {2} {3} TEMP-TABLE`).  The first skip
        // loop above handles refs before SHARED; this one handles refs between
        // SHARED/access/STATIC and the keyword (VARIABLE, TEMP-TABLE, etc.).
        while self.check(Kind::Preprop) || self.check(Kind::IncludeArgReference) {
            self.advance();
        }

        if self.check(Kind::Property) {
            return self.parse_define_property(access.unwrap_or(AccessModifier::Public), is_static);
        }

        // DEFINE EVENT
        if self.check(Kind::Event) {
            return self.parse_define_event(
                access.unwrap_or(AccessModifier::Public),
                is_static,
                is_abstract,
            );
        }

        // Parse SERIALIZABLE / NON-SERIALIZABLE (dataset-specific, before DATASET keyword)
        let serializable = self.check(Kind::Serializable) && {
            self.advance();
            true
        };
        let non_serializable = !serializable && self.check(Kind::NonSerializable) && {
            self.advance();
            true
        };

        // DEFINE DATASET
        if self.check(Kind::Dataset) {
            return self.parse_define_dataset(
                access,
                is_static,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                serializable,
                non_serializable,
            );
        }

        // DEFINE DATA-SOURCE
        if self.check(Kind::DataSource) {
            return self.parse_define_data_source(access, is_static);
        }

        // If we consumed an access modifier or STATIC but it's not PROPERTY/DATASET/DATA-SOURCE,
        // fall through to normal DEFINE handling (access modifier is ignored
        // for VARIABLE/TEMP-TABLE for now — tracked in Future)

        // DEFINE TEMP-TABLE
        if self.check(Kind::TempTable) {
            return self.parse_define_temp_table(is_new_shared, is_shared, is_new_global_shared);
        }

        // DEFINE BUFFER
        if self.check(Kind::Buffer) {
            return self.parse_define_buffer(is_new_shared, is_shared, is_new_global_shared);
        }

        // DEFINE STREAM
        if self.check(Kind::Stream) {
            return self.parse_define_stream();
        }

        // DEFINE FRAME
        if self.check(Kind::Frame) {
            return self.parse_define_frame();
        }

        // DEFINE QUERY — skip to statement end (may contain table.field references).
        // Marked as a table candidate (#130): the FOR clause names the temp-tables
        // or tables the query is defined over, and a table used only here would
        // otherwise look unread.
        if self.check(Kind::Query) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_table_stmt(hi));
        }

        // DEFINE WORKFILE — legacy synonym for DEFINE TEMP-TABLE, skip to statement end.
        // Uses skip_to_statement_end() to skip over .field access in LIKE clauses.
        if self.check(Kind::Workfile) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // UI widget definitions — DEFINE BROWSE, RECTANGLE, BUTTON, IMAGE, MENU, SUB-MENU
        // etc. that are not separately handled. Skip the whole DEFINE statement.
        if self.check(Kind::Browse) || self.check(Kind::Rectangle) || self.check(Kind::Identifier) {
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }

        // DEFINE VARIABLE / DEFINE VAR
        if self.check(Kind::Variable) {
            self.advance(); // consume VARIABLE or VAR
        } else {
            return Err(ParseError {
                message:
                    "Expected VARIABLE, VAR, TEMP-TABLE, BUFFER, STREAM, FRAME, DATASET, DATA-SOURCE, or EVENT after DEFINE"
                        .to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        // Name (use parse_identifier so compound preprop names like {&pre}inc_whse-time work)
        let name = self.parse_identifier().map_err(|_| ParseError {
            message: "Expected variable name after DEFINE VARIABLE".to_string(),
            span: self.current_span(),
        })?;

        // Consume optional INIT/INITIAL value appearing before AS/LIKE (some legacy code style)
        if self.check(Kind::Initial) {
            self.advance();
            if !matches!(
                self.peek().kind,
                Kind::KwAs | Kind::Like | Kind::NoUndo | Kind::Period
            ) {
                self.parse_expression().ok();
            }
        }

        // parse optional no-undo, initial, and extent
        let mut no_undo = false;
        let mut initial_value = None;
        let mut extent = None;

        // Some coding styles write NO-UNDO before AS/LIKE; consume it here so
        // parse_type_source() sees the type-source keyword next.
        if self.check(Kind::NoUndo) {
            self.advance();
            no_undo = true;
        }

        // parse type source (AS type | LIKE field)
        let type_source = self.parse_type_source()?;

        loop {
            match self.peek().kind {
                Kind::NoUndo => {
                    self.advance();
                    no_undo = true;
                }
                Kind::Initial => {
                    self.advance();
                    // Array initial syntax: INITIAL [val1, val2, ...]
                    if self.check(Kind::LeftBracket) {
                        self.advance(); // consume [
                        let mut depth = 1;
                        while depth > 0 && !self.at_end() {
                            if self.check(Kind::LeftBracket) {
                                depth += 1;
                            } else if self.check(Kind::RightBracket) {
                                depth -= 1;
                            }
                            if depth > 0 {
                                self.advance();
                            }
                        }
                        if self.check(Kind::RightBracket) {
                            self.advance(); // consume ]
                        }
                    } else {
                        initial_value = Some(self.parse_expression()?);
                    }
                }
                Kind::Extent => {
                    self.advance();
                    // Extent can be followed by number, preprocessor reference, or nothing (dynamic)
                    if self.check(Kind::IntegerLiteral) {
                        let ext_token = self.advance().clone();
                        if let Ok(n) = self.source[ext_token.start..ext_token.end].parse::<u32>() {
                            extent = Some(n);
                        } else {
                            extent = Some(0); // dynamic
                        }
                    } else if self.check(Kind::Preprop) || self.check(Kind::IncludeArgReference) {
                        // e.g. EXTENT {&DEPTH_MAX} — treat as dynamic extent
                        self.advance();
                        extent = Some(0);
                    }
                }
                Kind::Decimals => {
                    self.advance();
                    // Consume optional integer argument
                    if self.check(Kind::IntegerLiteral) {
                        self.advance();
                    }
                }
                Kind::CaseSensitive => {
                    self.advance();
                }
                Kind::Format | Kind::Form | Kind::Label | Kind::ColumnLabel | Kind::Help => {
                    self.advance();
                    self.skip_format_value();
                }
                // UI display options (VIEW-AS, SIZE) — when we hit VIEW-AS or SIZE, the
                // remaining variable options are entirely UI-related.  Skip to the statement
                // end using the field-access-aware helper so that dots inside numeric
                // size specs (e.g. 24.4 or .62) are not mistaken for the terminator.
                Kind::ViewAs | Kind::Size => {
                    let _ = self.skip_to_statement_end();
                    return Ok(self.stmt(StatementKind::VariableDeclaration {
                        name,
                        type_source,
                        initial_value,
                        no_undo,
                        extent,
                        is_new_shared,
                        is_shared,
                        is_new_global_shared,
                    }));
                }
                _ => break,
            }
        }

        self.expect_period("Expected '.' to end statement")?;

        Ok(self.stmt(StatementKind::VariableDeclaration {
            name,
            type_source,
            initial_value,
            no_undo,
            extent,
            is_new_shared,
            is_shared,
            is_new_global_shared,
        }))
    }

    /// Parse: VAR type name [= value].
    fn parse_var_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume VAR

        // Parse type source (AS type | LIKE field).
        // VAR has no AS keyword — the type comes positionally after VAR.
        let type_source = if self.check(Kind::Like) {
            self.advance(); // consume LIKE
            TypeSource::Like {
                source: self.parse_qualified_identifier()?,
            }
        } else {
            TypeSource::Explicit(self.parse_data_type()?)
        };

        // Parse variable name
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected variable name".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }
        let name_token = self.advance().clone();
        let name = Identifier {
            span: Span {
                start: name_token.start as u32,
                end: name_token.end as u32,
            },
            name: self.source[name_token.start..name_token.end].to_string(),
        };

        // Optional initial value
        let initial_value = if self.check(Kind::Equals) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_period("Expected '.' to end statement")?;

        Ok(self.stmt(StatementKind::VariableDeclaration {
            name,
            type_source,
            initial_value,
            no_undo: true, // VAR implies NO-UNDO
            extent: None,
            // VAR has no SHARED syntax.
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        }))
    }

    fn parse_define_parameter(&mut self) -> ParseResult<Statement> {
        // Parse direction (INPUT, OUTPUT, INPUT-OUTPUT, or RETURN)
        let direction = match self.peek().kind {
            Kind::Input => {
                self.advance();
                ParameterDirection::Input
            }
            Kind::Output => {
                self.advance();
                ParameterDirection::Output
            }
            Kind::InputOutput => {
                self.advance();
                ParameterDirection::InputOutput
            }
            Kind::KwReturn => {
                self.advance();
                ParameterDirection::Return
            }
            _ => unreachable!("parse_define_parameter called without INPUT/OUTPUT/RETURN token"),
        };

        // Expect PARAMETER keyword
        self.expect_kind(Kind::Parameter, "Expected PARAMETER after INPUT/OUTPUT")?;

        // Dispatch on parameter type
        let param_type = match self.peek().kind {
            // TABLE FOR tt-name [APPEND] [BIND] [BY-VALUE]
            Kind::Table => {
                self.advance();
                self.expect_kind(Kind::KwFor, "Expected FOR after TABLE")?;
                let name = self.parse_identifier()?;
                let passing = self.parse_handle_passing_options();
                ParameterType::Handle {
                    kind: HandleParamKind::Table,
                    name,
                    passing,
                }
            }
            // TABLE-HANDLE handle [APPEND] [BIND] [BY-VALUE]
            Kind::TableHandle => {
                self.advance();
                let name = self.parse_identifier()?;
                let passing = self.parse_handle_passing_options();
                ParameterType::Handle {
                    kind: HandleParamKind::TableHandle,
                    name,
                    passing,
                }
            }
            // DATASET FOR ds-name [APPEND] [BIND] [BY-VALUE]
            Kind::Dataset => {
                self.advance();
                self.expect_kind(Kind::KwFor, "Expected FOR after DATASET")?;
                let name = self.parse_identifier()?;
                let passing = self.parse_handle_passing_options();
                ParameterType::Handle {
                    kind: HandleParamKind::Dataset,
                    name,
                    passing,
                }
            }
            // DATASET-HANDLE handle [APPEND] [BIND] [BY-VALUE]
            Kind::DatasetHandle => {
                self.advance();
                let name = self.parse_identifier()?;
                let passing = self.parse_handle_passing_options();
                ParameterType::Handle {
                    kind: HandleParamKind::DatasetHandle,
                    name,
                    passing,
                }
            }
            // BUFFER buf FOR table
            Kind::Buffer => {
                self.advance();
                let name = self.parse_identifier()?;
                self.expect_kind(Kind::KwFor, "Expected FOR after buffer name")?;
                let target = self.parse_identifier()?;
                ParameterType::Buffer { name, target }
            }
            // Standard: name [INIT value] [NO-UNDO] AS type [NO-UNDO] or LIKE field [NO-UNDO]
            _ => {
                let name = self.parse_identifier()?;
                // Consume optional INIT value before AS/LIKE (some code specifies INIT first)
                if self.check(Kind::Initial) {
                    self.advance();
                    self.parse_expression().ok();
                }
                // NO-UNDO may appear before AS/LIKE (e.g. DEF INPUT PARAM p NO-UNDO LIKE t.f)
                let mut no_undo = self.check(Kind::NoUndo);
                if no_undo {
                    self.advance();
                }
                let type_source = self.parse_type_source()?;
                loop {
                    match self.peek().kind {
                        Kind::NoUndo => {
                            self.advance();
                            no_undo = true;
                        }
                        Kind::Decimals => {
                            self.advance();
                            // Consume optional integer argument
                            if self.check(Kind::IntegerLiteral) {
                                self.advance();
                            }
                        }
                        Kind::Format
                        | Kind::Form
                        | Kind::Label
                        | Kind::ColumnLabel
                        | Kind::Help => {
                            self.advance();
                            self.skip_format_value();
                        }
                        Kind::Initial => {
                            self.advance(); // consume INIT/INITIAL
                            // Consume the initial value expression
                            self.parse_expression().ok();
                        }
                        Kind::Extent => {
                            self.advance(); // consume EXTENT
                            // Consume optional array size integer (e.g. EXTENT 10)
                            if self.check(Kind::IntegerLiteral) {
                                self.advance();
                            }
                        }
                        Kind::CaseSensitive | Kind::FindCaseSensitive => {
                            self.advance(); // consume CASE-SENSITIVE
                        }
                        _ => break,
                    }
                }
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                }
            }
        };

        self.expect_period("Expected '.' after parameter definition")?;

        Ok(self.stmt(StatementKind::DefineParameter {
            direction,
            param_type,
        }))
    }

    fn parse_handle_passing_options(&mut self) -> HandlePassingOptions {
        let mut opts = HandlePassingOptions::default();
        loop {
            match self.peek().kind {
                Kind::Append => {
                    self.advance();
                    opts.append = true;
                }
                Kind::Bind => {
                    self.advance();
                    opts.bind = true;
                }
                Kind::ByValue => {
                    self.advance();
                    opts.by_value = true;
                }
                _ => break,
            }
        }
        opts
    }

    /// Parse XML and serialization options shared by TEMP-TABLE, BUFFER, and DATASET.
    /// Consumes NAMESPACE-URI, NAMESPACE-PREFIX, XML-NODE-NAME, XML-NODE-TYPE,
    /// SERIALIZE-NAME (all take a string literal or identifier value),
    /// and SERIALIZE-HIDDEN (flag, no value).
    fn parse_xml_serialize_options(&mut self) -> XmlSerializeOptions {
        let mut opts = XmlSerializeOptions::default();
        loop {
            match self.peek().kind {
                Kind::NamespaceUri => {
                    self.advance();
                    if let Ok(id) = self.parse_string_as_identifier() {
                        opts.namespace_uri = Some(id);
                    }
                }
                Kind::NamespacePrefix => {
                    self.advance();
                    if let Ok(id) = self.parse_string_as_identifier() {
                        opts.namespace_prefix = Some(id);
                    }
                }
                Kind::XmlNodeName => {
                    self.advance();
                    if let Ok(id) = self.parse_string_as_identifier() {
                        opts.xml_node_name = Some(id);
                    }
                }
                Kind::XmlNodeType => {
                    self.advance();
                    if let Ok(id) = self.parse_string_as_identifier() {
                        opts.xml_node_type = Some(id);
                    }
                }
                Kind::SerializeName => {
                    self.advance();
                    if let Ok(id) = self.parse_string_as_identifier() {
                        opts.serialize_name = Some(id);
                    }
                }
                Kind::SerializeHidden => {
                    self.advance();
                    opts.serialize_hidden = true;
                }
                _ => break,
            }
        }
        opts
    }

    /// Parse a string literal as an Identifier (for XML/serialize option values).
    fn parse_string_as_identifier(&mut self) -> ParseResult<Identifier> {
        if self.check(Kind::StringLiteral) {
            let token = self.advance().clone();
            let raw = &self.source[token.start..token.end];
            // Strip quotes from string literal
            let name = if raw.len() >= 2 {
                raw[1..raw.len() - 1].to_string()
            } else {
                raw.to_string()
            };
            Ok(Identifier {
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
                name,
            })
        } else {
            // Try as regular identifier
            self.parse_identifier()
        }
    }

    // Parse DEFINE TEMP-TABLE
    fn parse_define_temp_table(
        &mut self,
        is_new_shared: bool,
        is_shared: bool,
        is_new_global_shared: bool,
    ) -> ParseResult<Statement> {
        self.advance(); // consume TEMP-TABLE

        let name = self.parse_identifier()?;

        // Optional NO-UNDO
        let no_undo = if self.check(Kind::NoUndo) {
            self.advance();
            true
        } else {
            false
        };

        // Parse XML/serialize options (NAMESPACE-URI, SERIALIZE-NAME, etc.)
        let xml_options = self.parse_xml_serialize_options();

        // Optional LIKE / LIKE-SEQUENTIAL clause
        let mut like_table = None;
        let mut validate = false;
        let mut use_indexes = Vec::new();

        if self.check(Kind::Like) || self.check(Kind::LikeSequential) {
            self.advance(); // consume LIKE or LIKE-SEQUENTIAL
            like_table = Some(self.parse_identifier()?);

            // Optional VALIDATE
            if self.check(Kind::Validate) {
                self.advance();
                validate = true;
            }

            // Optional USE-INDEX clauses
            while self.check(Kind::UseIndex) {
                self.advance(); // consume USE-INDEX
                let idx_name = self.parse_identifier()?;
                let as_primary = if self.check(Kind::KwAs) {
                    self.advance();
                    self.expect_kind(Kind::Primary, "Expected PRIMARY after AS in USE-INDEX")?;
                    true
                } else {
                    false
                };
                use_indexes.push(UseIndex {
                    name: idx_name,
                    as_primary,
                });
            }
        }

        let mut fields = Vec::new();
        let mut indexes = Vec::new();

        // Parse FIELD and INDEX definitions until period
        while !self.check(Kind::Period) && !self.at_end() {
            if self.check(Kind::Field) {
                self.advance(); // consume FIELD
                let field_name = self.parse_identifier()?;

                // Optional EXTENT before AS/LIKE (ABL allows: field name EXTENT n LIKE ...)
                let mut pre_extent: Option<u32> = None;
                if self.check(Kind::Extent) {
                    self.advance();
                    if self.check(Kind::IntegerLiteral) {
                        let ext_token = self.advance().clone();
                        pre_extent = self.source[ext_token.start..ext_token.end]
                            .parse::<u32>()
                            .ok()
                            .or(Some(0));
                    }
                }

                // Parse type source: AS type or LIKE field [VALIDATE]
                let (type_source, validate) = if self.check(Kind::Like) {
                    self.advance();
                    let source = self.parse_qualified_identifier()?;
                    let field_validate = if self.check(Kind::Validate) {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    (TypeSource::Like { source }, field_validate)
                } else {
                    self.expect_kind(Kind::KwAs, "Expected AS or LIKE after field name")?;
                    (TypeSource::Explicit(self.parse_data_type()?), false)
                };

                // Parse optional field options
                let mut initial_value = None;
                let mut extent = pre_extent; // may already be set from EXTENT before AS/LIKE

                loop {
                    match self.peek().kind {
                        Kind::Initial => {
                            self.advance();
                            // Handle array initial syntax: INITIAL [val1, val2, ...]
                            if self.check(Kind::LeftBracket) {
                                self.advance(); // consume [
                                let mut values = Vec::new();
                                if !self.check(Kind::RightBracket) {
                                    values.push(self.parse_expression()?);
                                    while self.check(Kind::Comma) {
                                        self.advance();
                                        values.push(self.parse_expression()?);
                                    }
                                }
                                self.expect_kind(
                                    Kind::RightBracket,
                                    "Expected ']' after initial values",
                                )?;
                                crate::parser::debug_assert_expr_sibling_order(&values);
                                initial_value = Some(values);
                            } else {
                                // Scalar initial value
                                initial_value = Some(vec![self.parse_expression()?]);
                            }
                        }
                        Kind::Extent => {
                            self.advance();
                            if self.check(Kind::IntegerLiteral) {
                                let ext_token = self.advance().clone();
                                if let Ok(n) =
                                    self.source[ext_token.start..ext_token.end].parse::<u32>()
                                {
                                    extent = Some(n);
                                } else {
                                    extent = Some(0); // dynamic
                                }
                            }
                        }
                        Kind::Decimals => {
                            self.advance();
                            // Consume optional integer argument
                            if self.check(Kind::IntegerLiteral) {
                                self.advance();
                            }
                        }
                        // Skip known field options we don't store in the AST
                        Kind::Format | Kind::Form | Kind::Label | Kind::ColumnLabel => {
                            self.advance(); // consume keyword
                            // Skip the value (usually a string literal)
                            if self.check(Kind::StringLiteral) {
                                self.advance();
                            }
                        }
                        // Break on field/index/period boundaries
                        Kind::Field | Kind::Index | Kind::Period => break,
                        _ => break,
                    }
                }

                fields.push(TempTableField {
                    name: field_name,
                    type_source,
                    validate,
                    initial_value,
                    extent,
                });
            } else if self.check(Kind::Index) {
                self.advance(); // consume INDEX
                let index_name = self.parse_identifier()?;

                let mut is_primary = false;
                let mut is_unique = false;
                let mut is_word_index = false;

                // Optional IS or AS prefix (both valid, or neither)
                if self.check(Kind::Is) || self.check(Kind::KwAs) {
                    self.advance();
                }

                // Parse flags in any order: PRIMARY, UNIQUE, WORD-INDEX.
                // IS and AS may appear between flags (e.g. "AS UNIQUE IS PRIMARY").
                loop {
                    match self.peek().kind {
                        Kind::Primary => {
                            self.advance();
                            is_primary = true;
                        }
                        Kind::Unique => {
                            self.advance();
                            is_unique = true;
                        }
                        Kind::WordIndex => {
                            self.advance();
                            is_word_index = true;
                        }
                        // IS/AS can precede each flag: "AS UNIQUE IS PRIMARY"
                        Kind::Is | Kind::KwAs => {
                            self.advance();
                        }
                        _ => break,
                    }
                }

                // Parse index fields with optional ASC/DESC direction
                let mut index_fields = Vec::new();
                while Self::can_be_identifier(self.peek().kind)
                    && !self.check(Kind::Field)
                    && !self.check(Kind::Index)
                    && !self.check(Kind::Period)
                {
                    let field_name = self.parse_identifier()?;
                    let direction = match self.peek().kind {
                        Kind::Ascending => {
                            self.advance();
                            Some(SortDirection::Ascending)
                        }
                        Kind::Descending => {
                            self.advance();
                            Some(SortDirection::Descending)
                        }
                        _ => None,
                    };
                    index_fields.push(IndexField {
                        name: field_name,
                        direction,
                    });
                }

                indexes.push(TempTableIndex {
                    name: index_name,
                    is_primary,
                    is_unique,
                    is_word_index,
                    fields: index_fields,
                });
            } else if can_start_statement(self.peek().kind) {
                // A statement-starting keyword means we likely missed a period
                return Err(ParseError {
                    message: "Expected '.' to end DEFINE TEMP-TABLE (found statement keyword)"
                        .to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            } else {
                // Skip unknown tokens in temp-table definition (forward-compatibility)
                self.advance();
            }
        }

        self.expect_period("Expected '.' after DEFINE TEMP-TABLE")?;

        Ok(self.stmt(StatementKind::DefineTempTable {
            name,
            no_undo,
            like_table,
            validate,
            use_indexes,
            fields,
            indexes,
            xml_options,
            is_new_shared,
            is_shared,
            is_new_global_shared,
        }))
    }

    // Parse DEFINE BUFFER name FOR [TEMP-TABLE] table [PRESELECT] [LABEL "str"].
    fn parse_define_buffer(
        &mut self,
        is_new_shared: bool,
        is_shared: bool,
        is_new_global_shared: bool,
    ) -> ParseResult<Statement> {
        self.advance(); // consume BUFFER

        let mut name = self.parse_identifier()?;
        // Handle compound names like b-{&preproc} where identifier prefix ends with hyphen
        // and a preprocessor reference follows adjacently (e.g. b-{&line-buffer}).
        if self.check(Kind::Preprop) && self.peek().start == name.span.end as usize {
            let pp = self.advance().clone();
            name.span.end = pp.end as u32;
            name.name.push_str(&self.source[pp.start..pp.end]);
        }
        self.expect_kind(Kind::KwFor, "Expected FOR after buffer name")?;

        // Check for FOR TEMP-TABLE vs FOR [preproc] table
        let target = if self.check(Kind::TempTable) {
            self.advance();
            self.parse_identifier().map(BufferTarget::TempTable)?
        } else if self.check(Kind::Preprop) {
            let pp = self.advance().clone();
            BufferTarget::Table(Identifier {
                span: Span {
                    start: pp.start as u32,
                    end: pp.end as u32,
                },
                name: self.source[pp.start..pp.end].to_string(),
            })
        } else {
            self.parse_identifier().map(BufferTarget::Table)?
        };

        // Parse XML/serialize options (NAMESPACE-URI, SERIALIZE-NAME, etc.)
        let xml_options = self.parse_xml_serialize_options();

        // Parse optional modifiers
        let mut preselect = false;
        let mut label = None;

        while !self.check(Kind::Period) && !self.at_end() {
            match self.peek().kind {
                Kind::Preselect => {
                    self.advance();
                    preselect = true;
                }
                Kind::Label => {
                    self.advance();
                    if self.check(Kind::StringLiteral) {
                        let token = self.advance().clone();
                        // Strip quotes from string literal
                        let raw = &self.source[token.start..token.end];
                        label = Some(raw[1..raw.len() - 1].to_string());
                    }
                }
                _ => {
                    // Skip remaining unknown tokens for forward-compatibility
                    self.advance();
                    if self.check(Kind::StringLiteral) {
                        self.advance();
                    }
                }
            }
        }

        self.expect_period("Expected '.' after DEFINE BUFFER")?;

        Ok(self.stmt(StatementKind::DefineBuffer {
            name,
            target,
            preselect,
            label,
            xml_options,
            is_new_shared,
            is_shared,
            is_new_global_shared,
        }))
    }

    // Parse DEFINE DATASET statement.
    #[allow(clippy::too_many_arguments)]
    fn parse_define_dataset(
        &mut self,
        access: Option<AccessModifier>,
        is_static: bool,
        is_new_shared: bool,
        is_shared: bool,
        is_new_global_shared: bool,
        serializable: bool,
        non_serializable: bool,
    ) -> ParseResult<Statement> {
        self.advance(); // consume DATASET

        let name = self.parse_identifier()?;

        // Parse XML/serialize options
        let xml_options = self.parse_xml_serialize_options();

        // Optional REFERENCE-ONLY
        let reference_only = if self.check(Kind::ReferenceOnly) {
            self.advance();
            true
        } else {
            false
        };

        // Expect FOR followed by comma-separated buffer names
        self.expect_kind(Kind::KwFor, "Expected FOR after dataset name")?;
        let mut buffers = vec![self.parse_identifier()?];
        while self.check(Kind::Comma) {
            self.advance();
            buffers.push(self.parse_identifier()?);
        }

        // Parse DATA-RELATION and PARENT-ID-RELATION clauses
        let mut data_relations = Vec::new();
        let mut parent_id_relations = Vec::new();

        while !self.check(Kind::Period) && !self.at_end() {
            if self.check(Kind::DataRelation) {
                data_relations.push(self.parse_data_relation()?);
            } else if self.check(Kind::ParentIdRelation) {
                parent_id_relations.push(self.parse_parent_id_relation()?);
            } else if can_start_statement(self.peek().kind) {
                return Err(ParseError {
                    message: "Expected '.' to end DEFINE DATASET (found statement keyword)"
                        .to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            } else {
                // Skip unknown tokens for forward-compatibility
                self.advance();
            }
        }

        self.expect_period("Expected '.' after DEFINE DATASET")?;

        Ok(self.stmt(StatementKind::DefineDataset {
            name,
            access,
            is_static,
            is_new_shared,
            is_shared,
            is_new_global_shared,
            serializable,
            non_serializable,
            xml_options,
            reference_only,
            buffers,
            data_relations,
            parent_id_relations,
        }))
    }

    // Parse a DATA-RELATION clause.
    fn parse_data_relation(&mut self) -> ParseResult<DataRelation> {
        self.advance(); // consume DATA-RELATION

        // Optional relation name (if next token is not FOR)
        let name = if !self.check(Kind::KwFor) && Self::can_be_identifier(self.peek().kind) {
            Some(self.parse_identifier()?)
        } else {
            None
        };

        // FOR parent, child
        self.expect_kind(Kind::KwFor, "Expected FOR in DATA-RELATION")?;
        let parent_buffer = self.parse_identifier()?;
        self.expect_kind(Kind::Comma, "Expected ',' between parent and child buffers")?;
        let child_buffer = self.parse_identifier()?;

        // Flags (REPOSITION, NESTED, etc.) and RELATION-FIELDS may appear in any order.
        // Parse them all in a unified loop.
        let mut reposition = false;
        let mut nested = false;
        let mut foreign_key_hidden = false;
        let mut not_active = false;
        let mut recursive = false;
        let mut relation_fields: Vec<(Identifier, Identifier)> = Vec::new();

        loop {
            let tok = self.peek();
            let is_relation_field = self.check(Kind::RelationFields)
                || (self.check(Kind::Identifier)
                    && self.source[tok.start..tok.end].eq_ignore_ascii_case("relation-field"));
            if is_relation_field {
                self.advance();
                self.expect_kind(Kind::LeftParen, "Expected '(' after RELATION-FIELDS")?;
                loop {
                    let parent_field = self.parse_identifier()?;
                    // Optional table.field qualifier (e.g. ttMethod.id)
                    if self.check(Kind::Period) && self.is_field_access_ahead() {
                        self.advance(); // consume .
                        self.advance(); // consume field name
                    }
                    self.expect_kind(Kind::Comma, "Expected ',' between field pair")?;
                    let child_field = self.parse_identifier()?;
                    // Optional table.field qualifier
                    if self.check(Kind::Period) && self.is_field_access_ahead() {
                        self.advance(); // consume .
                        self.advance(); // consume field name
                    }
                    relation_fields.push((parent_field, child_field));
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance(); // consume comma before next pair
                }
                self.expect_kind(Kind::RightParen, "Expected ')' after RELATION-FIELDS")?;
            } else {
                match self.peek().kind {
                    Kind::Reposition => {
                        self.advance();
                        reposition = true;
                    }
                    Kind::Nested => {
                        self.advance();
                        nested = true;
                        // FOREIGN-KEY-HIDDEN can only follow NESTED
                        if self.check(Kind::ForeignKeyHidden) {
                            self.advance();
                            foreign_key_hidden = true;
                        }
                    }
                    Kind::NotActive => {
                        self.advance();
                        not_active = true;
                    }
                    Kind::Recursive => {
                        self.advance();
                        recursive = true;
                    }
                    _ => break,
                }
            }
        }

        Ok(DataRelation {
            name,
            parent_buffer,
            child_buffer,
            relation_fields,
            reposition,
            nested,
            foreign_key_hidden,
            not_active,
            recursive,
        })
    }

    // Parse a PARENT-ID-RELATION clause.
    fn parse_parent_id_relation(&mut self) -> ParseResult<ParentIdRelation> {
        self.advance(); // consume PARENT-ID-RELATION

        // Optional relation name
        let name = if !self.check(Kind::KwFor) && Self::can_be_identifier(self.peek().kind) {
            Some(self.parse_identifier()?)
        } else {
            None
        };

        // FOR parent, child
        self.expect_kind(Kind::KwFor, "Expected FOR in PARENT-ID-RELATION")?;
        let parent_buffer = self.parse_identifier()?;
        self.expect_kind(Kind::Comma, "Expected ',' between parent and child buffers")?;
        let child_buffer = self.parse_identifier()?;

        // PARENT-ID-FIELD id-field
        self.expect_kind(
            Kind::ParentIdField,
            "Expected PARENT-ID-FIELD in PARENT-ID-RELATION",
        )?;
        let id_field = self.parse_identifier()?;

        // Optional PARENT-FIELDS-BEFORE
        let parent_fields_before = if self.check(Kind::ParentFieldsBefore) {
            self.advance();
            self.parse_paren_identifier_list()?
        } else {
            Vec::new()
        };

        // Optional PARENT-FIELDS-AFTER
        let parent_fields_after = if self.check(Kind::ParentFieldsAfter) {
            self.advance();
            self.parse_paren_identifier_list()?
        } else {
            Vec::new()
        };

        Ok(ParentIdRelation {
            name,
            parent_buffer,
            child_buffer,
            id_field,
            parent_fields_before,
            parent_fields_after,
        })
    }

    /// Parse a parenthesized comma-separated list of identifiers: (id1, id2, ...)
    fn parse_paren_identifier_list(&mut self) -> ParseResult<Vec<Identifier>> {
        self.expect_kind(Kind::LeftParen, "Expected '('")?;
        let mut ids = vec![self.parse_identifier()?];
        while self.check(Kind::Comma) {
            self.advance();
            ids.push(self.parse_identifier()?);
        }
        self.expect_kind(Kind::RightParen, "Expected ')'")?;
        Ok(ids)
    }

    // Parse DEFINE DATA-SOURCE statement.
    fn parse_define_data_source(
        &mut self,
        access: Option<AccessModifier>,
        is_static: bool,
    ) -> ParseResult<Statement> {
        self.advance(); // consume DATA-SOURCE

        let name = self.parse_identifier()?;

        // Expect FOR
        self.expect_kind(Kind::KwFor, "Expected FOR after data-source name")?;

        // Optional QUERY query-name
        let query = if self.check(Kind::Query) {
            self.advance();
            Some(self.parse_identifier()?)
        } else {
            None
        };

        // Parse comma-separated source buffer phrases (optional — may be absent when
        // only a QUERY is specified: DEFINE DATA-SOURCE src FOR QUERY qry.)
        let mut source_buffers = Vec::new();
        while !self.check(Kind::Period)
            && !self.at_end()
            && Self::can_be_identifier(self.peek().kind)
        {
            let buf_name = self.parse_identifier()?;

            // Optional KEYS clause
            let keys = if self.check(Kind::Keys) {
                self.advance();
                self.expect_kind(Kind::LeftParen, "Expected '(' after KEYS")?;

                if self.check(Kind::Rowid) {
                    self.advance();
                    self.expect_kind(Kind::RightParen, "Expected ')' after ROWID")?;
                    Some(DataSourceKeys::Rowid)
                } else {
                    let mut fields = vec![self.parse_identifier()?];
                    while self.check(Kind::Comma) {
                        self.advance();
                        fields.push(self.parse_identifier()?);
                    }
                    self.expect_kind(Kind::RightParen, "Expected ')' after KEYS fields")?;
                    Some(DataSourceKeys::Fields(fields))
                }
            } else {
                None
            };

            source_buffers.push(DataSourceBuffer {
                name: buf_name,
                keys,
            });

            if !self.check(Kind::Comma) {
                break;
            }
            self.advance(); // consume comma
        }

        self.expect_period("Expected '.' after DEFINE DATA-SOURCE")?;

        Ok(self.stmt(StatementKind::DefineDataSource {
            name,
            access,
            is_static,
            query,
            source_buffers,
        }))
    }

    /// Continue parsing an expression after additive level has been parsed
    fn finish_expression(&mut self, left: Expression) -> ParseResult<Expression> {
        // Handle comparison operators (except = which we already checked)
        let expr = if self.is_non_equals_comparison_operator() {
            let op_kind = self.advance().kind;
            let right = self.parse_additive()?;
            self.make_comparison(left, op_kind, right)
        } else {
            left
        };

        // Handle AND
        let mut expr = expr;
        while self.check(Kind::And) {
            let lo = expr.span.start;
            self.advance();
            let right = self.parse_comparison()?;
            let hi = self.prev_end();
            expr = self.spanned_expr(lo, hi, ExpressionKind::And(Box::new(expr), Box::new(right)));
        }

        // Handle OR
        while self.check(Kind::Or) {
            let lo = expr.span.start;
            self.advance();
            let right = self.parse_and()?;
            let hi = self.prev_end();
            expr = self.spanned_expr(lo, hi, ExpressionKind::Or(Box::new(expr), Box::new(right)));
        }

        Ok(expr)
    }

    fn is_non_equals_comparison_operator(&self) -> bool {
        matches!(
            self.peek().kind,
            Kind::Eq
                | Kind::NotEqual
                | Kind::LessThan
                | Kind::LessThanOrEqual
                | Kind::GreaterThan
                | Kind::GreaterThanOrEqual
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

    fn make_comparison(&mut self, left: Expression, op: Kind, right: Expression) -> Expression {
        let lo = left.span.start;
        let kind = match op {
            Kind::Eq => ExpressionKind::Equal(Box::new(left), Box::new(right)),
            Kind::NotEqual | Kind::Ne => ExpressionKind::NotEqual(Box::new(left), Box::new(right)),
            Kind::LessThan | Kind::Lt => ExpressionKind::LessThan(Box::new(left), Box::new(right)),
            Kind::LessThanOrEqual | Kind::Le => {
                ExpressionKind::LessThanOrEqual(Box::new(left), Box::new(right))
            }
            Kind::GreaterThan | Kind::Gt => {
                ExpressionKind::GreaterThan(Box::new(left), Box::new(right))
            }
            Kind::GreaterThanOrEqual | Kind::Ge => {
                ExpressionKind::GreaterThanOrEqual(Box::new(left), Box::new(right))
            }
            Kind::Begins => ExpressionKind::Begins(Box::new(left), Box::new(right)),
            Kind::Matches => ExpressionKind::Matches(Box::new(left), Box::new(right)),
            Kind::Contains => ExpressionKind::Contains(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };
        let hi = self.prev_end();
        self.spanned_expr(lo, hi, kind)
    }

    /// Parse the whole token stream, returning the statements on a clean parse
    /// or the first error otherwise (fail-fast shape).
    ///
    /// Prefer the top-level `oxabl::parse` convenience for new code: it returns
    /// a [`Program`](crate::Program) with full error recovery, and
    /// [`Program::first_error`](crate::Program::first_error) recovers this
    /// method's fail-fast view without discarding the rest of the parse. This
    /// method is retained for the rare caller that genuinely wants a
    /// `Result`-shaped first-error-only entry point.
    ///
    /// This is the non-`Program` entry point used by the `analyze` CLI path. It
    /// now shares [`parse_program`](Self::parse_program)'s per-statement error
    /// recovery: block bodies no longer bubble the first in-body error out of
    /// the whole file. To preserve this method's original observable contract
    /// (report the first parse error, treat the file as failed), it surfaces the
    /// first accumulated error as `Err`; nothing that previously reported an
    /// error becomes silent. On a clean parse it returns the statements as
    /// before.
    pub fn parse_statements(&mut self) -> ParseResult<Vec<Statement>> {
        let program = self.parse_program();
        match program.errors.into_iter().next() {
            Some(err) => Err(err),
            None => Ok(program.statements),
        }
    }

    fn parse_do_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consume DO

        let mut loop_var = None;
        let mut from = None;
        let mut to = None;
        let mut by = None;
        let mut while_condition = None;

        // Optional TRANSACTION keyword: DO TRANSACTION:
        let transaction = if self.check(Kind::Transaction) {
            self.advance();
            true
        } else {
            false
        };

        // DO FOR table: — buffer lock scope; consume FOR and table name
        if self.check(Kind::KwFor) {
            self.advance(); // consume FOR
            if Self::can_be_identifier(self.peek().kind) {
                self.advance(); // consume table name
            }
        }

        // DO STOP-AFTER expr: — consume STOP-AFTER and its duration argument
        // STOP-AFTER is an identifier token (not a reserved keyword)
        if self.check(Kind::Identifier) {
            let tok = self.peek().clone();
            let text = &self.source[tok.start..tok.end];
            if text.eq_ignore_ascii_case("stop-after") {
                self.advance(); // consume STOP-AFTER
                // Consume the duration expression (may be integer literal or identifier)
                if !self.check(Kind::Colon) && !self.at_end() {
                    self.parse_expression().ok();
                }
            }
        }

        // check for loop — var may start with a preprop: DO {&tablename}i = 1 TO n
        if Self::can_be_identifier(self.peek().kind)
            || matches!(self.peek().kind, Kind::Preprop | Kind::IncludeArgReference)
        {
            // peek ahead to see if this is 'var = start to end'
            let saved_pos = self.current;
            let potential_var = self.advance().clone();
            let var_start = potential_var.start;
            let mut var_end = potential_var.end;

            // Consume any directly-adjacent preprop/identifier parts (e.g. i-j-{&sale-type})
            loop {
                let next = &self.tokens[self.current];
                if next.start != var_end {
                    break;
                }
                if matches!(next.kind, Kind::Preprop | Kind::IncludeArgReference)
                    || Self::can_be_identifier(next.kind)
                {
                    var_end = self.advance().end;
                } else {
                    break;
                }
            }

            if self.check(Kind::Equals) {
                // It's a counting loop
                let var_name = Identifier {
                    span: Span {
                        start: var_start as u32,
                        end: var_end as u32,
                    },
                    name: self.source[var_start..var_end].to_string(),
                };

                loop_var = Some(var_name);

                self.advance(); // consume =
                from = Some(self.parse_expression()?);

                // Expect TO, because we have a var and consumed =
                self.expect_kind(Kind::To, "Expected TO in DO loop")?;
                to = Some(self.parse_expression()?);

                // Optional BY
                if self.check(Kind::By) {
                    self.advance();
                    by = Some(self.parse_expression()?);
                }

                // Optional IN FRAME <name> after counting loop: DO i = 1 TO n IN FRAME f:
                if self.check(Kind::KwIn) && self.check_at(1, Kind::Frame) {
                    self.advance(); // consume IN
                    self.advance(); // consume FRAME
                    // frame name may be a compound preprop+identifier: {&tablename}f-builder
                    if Self::can_be_identifier(self.peek().kind)
                        || matches!(self.peek().kind, Kind::Preprop | Kind::IncludeArgReference)
                    {
                        let mut name_end = self.advance().end;
                        // Consume directly-adjacent parts of compound names.
                        loop {
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
                }

                // Optional TRANSACTION after counting loop: DO i = 1 TO 2 TRANSACTION:
                if self.check(Kind::Transaction) {
                    self.advance();
                }
            } else {
                // not a counting loop
                self.current = saved_pos;
            }
        }

        // check for WHILE
        if self.check(Kind::KwWhile) {
            self.advance();
            while_condition = Some(self.parse_expression()?);
            // Optional TRANSACTION after WHILE condition: DO WHILE cond TRANSACTION: ...
            if self.check(Kind::Transaction) {
                self.advance();
            }
        }

        // Skip DO/REPEAT block-header ON phrases: ON ERROR UNDO, RETRY / LEAVE / etc.
        // These are complex error-handling annotations on block headers; we consume them
        // without building AST nodes for now.
        while self.check(Kind::On) {
            self.advance(); // consume ON
            // consume the condition (e.g., ERROR, ENDKEY, STOP, QUIT — keyword or identifier)
            if !self.check(Kind::Colon) && !self.check(Kind::Comma) {
                self.advance(); // consume condition keyword/identifier
            }
            // consume UNDO and optional label: UNDO [label]
            if self.check(Kind::Undo) {
                self.advance();
                // consume optional block label after UNDO (e.g. UNDO TRANS-BLOCK, ...)
                if Self::can_be_identifier(self.peek().kind)
                    && !self.check(Kind::Colon)
                    && !self.check(Kind::Comma)
                {
                    self.advance();
                }
            }
            // consume optional action after comma: , LEAVE / RETRY / NEXT / RETURN [label]
            if self.check(Kind::Comma) {
                self.advance(); // consume ','
                // consume action keyword (LEAVE, RETRY, NEXT, RETURN, THROW)
                if matches!(
                    self.peek().kind,
                    Kind::Leave | Kind::Retry | Kind::Next | Kind::KwReturn | Kind::Throw
                ) {
                    self.advance();
                    // consume optional label or throw expression
                    if Self::can_be_identifier(self.peek().kind) && !self.check(Kind::Colon) {
                        self.advance();
                    }
                }
            }
        }

        // Optional WITH clause before block opener (e.g. WITH FRAME name, WITH WIDTH 255 NO-BOX).
        // The WITH clause can contain many frame options; consume everything until the
        // ':' or '.' that opens the block body.
        if self.check(Kind::With) {
            self.advance(); // consume WITH
            while !self.at_end() && !self.check(Kind::Colon) && !self.check(Kind::Period) {
                self.advance();
            }
        }

        // Accept either ':' or '.' to open the DO body (legacy ABL uses '.')
        if self.check(Kind::Period) {
            self.advance(); // consume '.' as body-start
        } else {
            self.expect_kind(Kind::Colon, "Expected ':' after DO")?;
        }

        let body = self.parse_block_body()?;

        Ok(self.stmt(StatementKind::Do {
            loop_var,
            from,
            to,
            by,
            while_condition,
            transaction,
            body,
        }))
    }

    fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consumes IF

        // If "condition" THEN
        let condition = self.parse_expression()?;

        // Expect THEN (or &THEN when IF appears inside a preprocessor &ELSEIF condition)
        if self.check(Kind::PreprocThen) {
            self.advance();
        } else {
            self.expect_kind(Kind::Then, "Expected THEN after IF condition")?;
        }

        // parse then branch, may be a DO block or single statement.
        //
        // A DO-block branch is parsed by calling `parse_do_statement` directly,
        // which bypasses the `parse_statement` funnel that normally stamps a
        // statement's real full-extent span — so without the explicit stamp
        // below it would keep `Span::DUMMY` (0..0). A zero span makes every
        // span-consuming pass (the formatter's line map, semantic ranges) treat
        // the branch as living at byte 0 / line 0, which mis-indents unrelated
        // leading content. Stamp `lo..prev_end` here, mirroring the funnel. The
        // `parse_statement` arm already carries a correct span; re-stamping it
        // with the identical `lo`/`hi` is a harmless no-op.
        let then_lo = self.peek().start as u32;
        let mut then_branch = if self.check(Kind::Do) {
            self.parse_do_statement()?
        } else {
            self.parse_statement()?
        };
        then_branch.span = Span {
            start: then_lo,
            end: self.prev_end().max(then_lo),
        };

        // Skip any stray bare periods between THEN-branch and ELSE (e.g. double-period `..`
        // from field-access syntax like `hbuffer::table_name..`).
        while self.check(Kind::Period) {
            self.advance();
        }

        // Legacy ABL sometimes embeds an *undefined* preprocessor placeholder
        // (e.g. `{&misc-keys}`) between `End.` and `Else`, used as an
        // extensibility hook for additional `Else If` branches. In
        // unpreprocessed source such a reference lexes as a `Preprop`
        // token, which would otherwise be parsed as a standalone statement
        // and leave the trailing `Else` orphaned. (Under `--preprocess` the
        // preprocessor expands undefined refs to empty, so this never
        // arises there.) If the next meaningful
        // token after one or more `Preprop` references is `Else`, consume
        // those placeholders so the ELSE binds correctly.
        if self.check(Kind::Preprop) {
            let saved = self.current;
            while self.check(Kind::Preprop) {
                self.advance();
                while self.check(Kind::Period) {
                    self.advance();
                }
            }
            if !self.check(Kind::KwElse) {
                // Not an IF placeholder — restore position and let the outer
                // parser handle the Preprop as its own statement.
                self.current = saved;
            }
        }

        // optional ELSE
        let else_branch = if self.check(Kind::KwElse) {
            self.advance();
            // Same span-stamping rationale as the THEN branch above: the DO and
            // ELSE-IF arms bypass the `parse_statement` funnel, so stamp their
            // real span here to avoid a `Span::DUMMY` (0..0) branch.
            let else_lo = self.peek().start as u32;
            let mut else_stmt = if self.check(Kind::Do) {
                self.parse_do_statement()?
            } else if self.check(Kind::KwIf) {
                self.parse_if_statement()?
            } else if self.check(Kind::End) {
                // Empty ELSE branch: END belongs to the enclosing DO/FOR/REPEAT block.
                // Don't consume it — the outer block parser handles it.
                self.stmt(StatementKind::Empty)
            } else {
                self.parse_statement()?
            };
            else_stmt.span = Span {
                start: else_lo,
                end: self.prev_end().max(else_lo),
            };
            Some(Box::new(else_stmt))
        } else {
            None
        };

        Ok(self.stmt(StatementKind::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        }))
    }

    fn parse_repeat_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume REPEAT

        // Optional block label before the colon (LABEL: REPEAT)
        // Already handled at statement dispatch level; nothing extra needed here.

        // Optional counting loop: REPEAT var = expr TO expr [BY expr]
        // var may be a compound preprop+identifier token pair (e.g. {&tablename}i)
        if Self::can_be_identifier(self.peek().kind)
            || matches!(self.peek().kind, Kind::Preprop | Kind::IncludeArgReference)
        {
            let saved_pos = self.current;
            let mut var_end = self.advance().end;
            // Consume any directly-adjacent compound name parts.
            loop {
                let next = &self.tokens[self.current];
                if next.start != var_end {
                    break;
                }
                if matches!(next.kind, Kind::Preprop | Kind::IncludeArgReference)
                    || Self::can_be_identifier(next.kind)
                {
                    var_end = self.advance().end;
                } else {
                    break;
                }
            }
            if self.check(Kind::Equals) {
                self.advance(); // consume =
                self.parse_expression().ok(); // from value
                if self.check(Kind::To) {
                    self.advance(); // consume TO
                    self.parse_expression().ok(); // to value
                    if self.check(Kind::By) {
                        self.advance(); // consume BY
                        self.parse_expression().ok(); // by value
                    }
                } else {
                    self.current = saved_pos; // not a counting loop; backtrack
                }
            } else {
                self.current = saved_pos; // not a counting loop; backtrack
            }
        }

        // Optional WHILE
        let while_condition = if self.check(Kind::KwWhile) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Consume ON phrase(s) same as DO (ON ERROR UNDO [label] [, action [label]])
        while self.check(Kind::On) {
            self.advance();
            if !self.check(Kind::Colon) && !self.check(Kind::Comma) {
                self.advance();
            }
            if self.check(Kind::Undo) {
                self.advance();
                if Self::can_be_identifier(self.peek().kind)
                    && !self.check(Kind::Colon)
                    && !self.check(Kind::Comma)
                {
                    self.advance();
                }
            }
            if self.check(Kind::Comma) {
                self.advance();
                if matches!(
                    self.peek().kind,
                    Kind::Leave | Kind::Retry | Kind::Next | Kind::KwReturn | Kind::Throw
                ) {
                    self.advance();
                    if Self::can_be_identifier(self.peek().kind) && !self.check(Kind::Colon) {
                        self.advance();
                    }
                }
            }
        }

        // Optional TRANSACTION keyword (e.g. REPEAT TRANSACTION:)
        if self.check(Kind::Transaction) {
            self.advance();
        }

        // Optional PRESELECT EACH/FIRST/LAST table [lock] [WHERE cond] [lock]
        // Multiple tables may be comma-joined: PRESELECT EACH t1 NO-LOCK, EACH t2 NO-LOCK
        while self.check(Kind::Preselect) {
            self.advance(); // consume PRESELECT
            // Inner loop: handle comma-joined table entries
            loop {
                // optional qualifier (EACH/FIRST/LAST)
                if matches!(self.peek().kind, Kind::Each | Kind::First | Kind::Last) {
                    self.advance();
                }
                // table/buffer name
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
                // optional OF parent-table (buffer join: EACH child OF parent)
                if self.check(Kind::Of) {
                    self.advance();
                    if Self::can_be_identifier(self.peek().kind) {
                        self.advance();
                    }
                }
                // lock type may appear before WHERE
                self.parse_lock_type();
                // optional WHERE clause
                if self.check(Kind::KwWhere) {
                    self.advance();
                    self.parse_expression().ok();
                }
                // lock type may also appear after WHERE
                self.parse_lock_type();
                // optional USE-INDEX clause
                while self.check(Kind::UseIndex) {
                    self.advance(); // consume USE-INDEX
                    if Self::can_be_identifier(self.peek().kind) {
                        self.advance(); // consume index name
                    }
                }
                // Comma-joined next table entry (EACH/FIRST/LAST follows the comma)?
                if self.check(Kind::Comma)
                    && matches!(self.peek_at(1).kind, Kind::Each | Kind::First | Kind::Last)
                {
                    self.advance(); // consume comma
                    continue; // process next table
                }
                break;
            }
            // optional BREAK BY / BY field [BY field ...] — applies to the whole PRESELECT
            if self.check(Kind::KwBreak) {
                self.advance(); // consume BREAK
            }
            while self.check(Kind::By) {
                self.advance(); // consume BY
                if Self::can_be_identifier(self.peek().kind) {
                    self.parse_postfix().ok(); // consume break field
                }
            }
        }

        // Expect colon (or period for legacy code)
        if self.check(Kind::Period) {
            self.advance();
        } else {
            self.expect_kind(Kind::Colon, "Expected ':' after REPEAT")?;
        }

        let body = self.parse_block_body()?;

        Ok(self.stmt(StatementKind::Repeat {
            while_condition,
            body,
        }))
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume RETURN

        // RETURN ERROR expr. — ABL error-throw syntax; ERROR is an identifier modifier.
        // Skip ERROR keyword and parse the rest as a return value expression, or skip to period.
        if Self::can_be_identifier(self.peek().kind)
            && self.source[self.peek().start..self.peek().end].eq_ignore_ascii_case("error")
        {
            let _ = self.skip_to_statement_end();
            return Ok(self.stmt(StatementKind::Return(None)));
        }

        // RETURN NO-APPLY. — standard UI-trigger return keyword. Consumed as a
        // keyword return with no value expression so it is not walked as an
        // undefined identifier (#58 second-pass residual).
        if self.check(Kind::NoApply) {
            self.advance();
            if self.check(Kind::NoError) {
                self.advance();
            }
            self.expect_period("Expected a '.' after RETURN NO-APPLY")?;
            return Ok(self.stmt(StatementKind::Return(None)));
        }

        // Check if there's a return value (not just a period)
        let value = if !self.check(Kind::Period) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Consume optional NO-ERROR (e.g. RETURN value NO-ERROR.)
        if self.check(Kind::NoError) {
            self.advance();
        }
        self.expect_period("Expected a '.' after RETURN")?;
        Ok(self.stmt(StatementKind::Return(value)))
    }

    fn parse_for_each(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consume FOR
        // FOR EACH / FOR FIRST / FOR LAST / FOR NEXT / FOR PREV are all valid.
        // Qualifier may also be omitted: FOR table WHERE ... is valid ABL.
        if self.check(Kind::Each)
            || self.check(Kind::First)
            || self.check(Kind::Last)
            || self.check(Kind::Next)
            || self.check(Kind::Prev)
        {
            self.advance(); // consume the qualifier
        }

        // Parse buffer name — may be a preprocessor reference {&find-orders}, a positional
        // include argument {1}, a compound name like {&web}order-line (preprop + adjacent
        // identifier), or a database-qualified name like dictdb._field (db.table dotted form).
        // parse_qualified_identifier → parse_identifier handles all of these: bare identifiers,
        // preprop refs, adjacent preprop+identifier compounds, and hyphenated names.
        let buffer = self.parse_qualified_identifier()?;

        // optional OF clause
        let of_relation = if self.check(Kind::Of) {
            self.advance();
            Some(self.parse_identifier()?)
        } else {
            None
        };

        // Lock type may appear before or after WHERE (ABL is flexible)
        let lock_type_pre = self.parse_lock_type();

        // optional WHERE clause — skip if WHERE is immediately followed by ':' / '.' / sort clause
        let where_clause = if self.check(Kind::KwWhere) {
            self.advance();
            if self.check(Kind::Colon)
                || self.check(Kind::Period)
                || self.check(Kind::KwBreak)
                || self.check(Kind::By)
            {
                None // empty WHERE clause; sort/block-open token follows
            } else {
                Some(self.parse_expression()?)
            }
        } else if !self.check(Kind::Colon)
            && !self.check(Kind::Period)
            && !self.check(Kind::Comma)
            && !self.check(Kind::NoLock)
            && !self.check(Kind::ShareLock)
            && !self.check(Kind::ExclusiveLock)
            && !self.check(Kind::UseIndex)
            && !self.check(Kind::KwBreak)
            && !self.check(Kind::By)
            && !self.check(Kind::KwWhile)
            && !self.check(Kind::NoWait)
            && !self.check(Kind::Transaction)
            && !self.check(Kind::KwBreak)
            && !self.at_end()
            && Self::can_be_identifier(self.peek().kind)
        {
            // Implicit WHERE: table name came from a preprocessor macro that included WHERE.
            // The expression parser stops naturally at lock types and clause keywords.
            self.parse_expression().ok()
        } else {
            None
        };

        // Trailing lock type (canonical position); prefer the explicit one
        let lock_type_post = self.parse_lock_type();
        let lock_type = if lock_type_pre != LockType::ShareLock {
            lock_type_pre
        } else {
            lock_type_post
        };

        // Optional USE-INDEX clause on first table
        while self.check(Kind::UseIndex) {
            self.advance();
            if Self::can_be_identifier(self.peek().kind) {
                self.advance();
            }
        }

        // Re-check lock type after USE-INDEX (ABL allows lock after use-index)
        self.parse_lock_type();

        // Optional NO-WAIT on first table
        if self.check(Kind::NoWait) {
            self.advance();
        }

        // ABL JOIN: FOR EACH t1 ..., FIRST/EACH/LAST t2 WHERE ... [, ...]
        // Consume additional joined table phrases separated by commas.
        while self.check(Kind::Comma) {
            self.advance(); // consume ','
            // Optional qualifier (EACH, FIRST, LAST, NEXT, PREV) for joined table
            if self.check(Kind::Each)
                || self.check(Kind::First)
                || self.check(Kind::Last)
                || self.check(Kind::Next)
                || self.check(Kind::Prev)
            {
                self.advance();
            }
            // table/buffer name
            if Self::can_be_identifier(self.peek().kind) {
                self.advance();
            }
            // optional OF clause (may appear before or after lock type)
            if self.check(Kind::Of) {
                self.advance();
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
            }
            // optional lock before WHERE
            self.parse_lock_type();
            // optional OF clause after lock type (ABL allows: table NO-LOCK OF related)
            if self.check(Kind::Of) {
                self.advance();
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
            }
            // optional WHERE clause
            if self.check(Kind::KwWhere) {
                self.advance();
                self.parse_expression().ok();
            }
            // trailing lock, USE-INDEX, NO-WAIT
            self.parse_lock_type();
            while self.check(Kind::UseIndex) {
                self.advance();
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
            }
            if self.check(Kind::NoWait) {
                self.advance();
            }
        }

        // Handle optional BREAK, GROUP, BY, TRANSACTION, and WHILE clauses in any order.
        // ABL allows these interleaved, e.g. TRANSACTION BY field: or BY field TRANSACTION:
        // GROUP BY is a less common sort grouping clause that behaves like BREAK BY.
        loop {
            if self.check(Kind::KwBreak) || self.check(Kind::Group) {
                self.advance(); // consume BREAK or GROUP (both followed by BY)
            } else if self.check(Kind::By) {
                self.advance(); // consume BY
                // consume the sort field expression (stops at ':' or '.')
                if !self.check(Kind::Colon) && !self.check(Kind::Period) {
                    self.parse_expression().ok();
                }
                // Optional DESCENDING / ASCENDING
                if self.check(Kind::Descending) || self.check(Kind::Ascending) {
                    self.advance();
                }
            } else if self.check(Kind::KwWhile) {
                self.advance(); // consume WHILE
                self.parse_expression().ok(); // parse the condition
            } else if self.check(Kind::Transaction) {
                self.advance();
            } else {
                break;
            }
        }

        // Optional WITH clause before block opener (e.g. WITH FRAME name, WITH WIDTH 255 NO-BOX).
        // The WITH clause can contain many frame options; consume everything until the
        // ':' or '.' that opens the block body.
        if self.check(Kind::With) {
            self.advance(); // consume WITH
            while !self.at_end() && !self.check(Kind::Colon) && !self.check(Kind::Period) {
                self.advance();
            }
        }

        // ABL accepts either ':' or '.' to start the FOR EACH body.
        // Some code uses 'FOR EACH table WHERE cond NO-LOCK.' with a period.
        // Also handle cases where a preprocessor reference expands to include the WHERE clause —
        // in unexpanded source, unrecognized tokens may appear before ':'; skip them.
        if self.check(Kind::Period) {
            self.advance(); // consume '.' as body-start
        } else if self.check(Kind::Colon) {
            self.advance(); // consume ':'
        } else {
            // Skip unrecognized tokens (e.g. condition from {&preproc} expansion) until ':' or '.'
            while !self.at_end() && !self.check(Kind::Colon) && !self.check(Kind::Period) {
                self.advance();
            }
            if self.check(Kind::Period) {
                self.advance();
            } else {
                self.expect_kind(Kind::Colon, "Expected ':' after FOR EACH")?;
            }
        }
        let body = self.parse_block_body()?;

        Ok(self.stmt(StatementKind::ForEach {
            buffer,
            of_relation,
            where_clause,
            lock_type,
            body,
        }))
    }

    // parse find statements
    fn parse_find_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // Consume FIND

        // parse optional find type
        let find_type = match self.peek().kind {
            Kind::First => {
                self.advance();
                FindType::First
            }
            Kind::Last => {
                self.advance();
                FindType::Last
            }
            Kind::Next => {
                self.advance();
                FindType::Next
            }
            Kind::Prev => {
                self.advance();
                FindType::Prev
            }
            Kind::Current => {
                self.advance();
                FindType::Current
            }
            _ => FindType::Unique,
        };

        // parse buffer/table name — may be database-qualified (dictdb._file) or a compound
        // like b-{&preproc} (identifier + adjacent Preprop).
        // Use parse_qualified_identifier to handle db.table dotted forms on the same line.
        let mut buffer = self.parse_qualified_identifier()?;
        if self.check(Kind::Preprop) && self.peek().start == buffer.span.end as usize {
            let pp = self.advance().clone();
            buffer.span.end = pp.end as u32;
            buffer.name.push_str(&self.source[pp.start..pp.end]);
        }

        // Optional OF clause: FIND customer OF order — shorthand for related-record find
        if self.check(Kind::Of) {
            self.advance(); // consume OF
            if Self::can_be_identifier(self.peek().kind) {
                self.advance(); // consume related-table name
            }
        }

        // parse optional key-value (FIND customer <key> syntax,
        // equivalent to find customer where customer.primary-index field eq 1)
        // Key value is present if next token is NOT a clause keyword, lock type, or terminator.
        let key_value = if !self.is_find_clause_start() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // In ABL, lock type may appear before or after the WHERE clause.
        // Parse an optional leading lock type, then WHERE, then trailing lock type.
        let lock_type_before = self.parse_lock_type();

        // parse optional where clause
        let where_clause = if self.check(Kind::KwWhere) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // parse trailing lock type (the canonical position); use the first one found
        let lock_type_after = self.parse_lock_type();
        let lock_type = if lock_type_before != LockType::ShareLock {
            lock_type_before
        } else {
            lock_type_after
        };

        // parse optional USE-INDEX clause
        while self.check(Kind::UseIndex) {
            self.advance(); // consume USE-INDEX
            // Use parse_identifier() so compound names like {&preproc}-key-name are handled
            self.parse_identifier().ok();
        }

        // lock type may also appear after USE-INDEX
        let lock_type_post = self.parse_lock_type();
        let lock_type = if lock_type != LockType::ShareLock {
            lock_type
        } else {
            lock_type_post
        };

        // parse optional NO-WAIT (can appear before or after NO-ERROR)
        if self.check(Kind::NoWait) {
            self.advance();
        }

        // parse optional no error
        let no_error = if self.check(Kind::NoError) {
            self.advance();
            true
        } else {
            false
        };

        // parse optional NO-WAIT after NO-ERROR
        if self.check(Kind::NoWait) {
            self.advance();
        }

        self.expect_period("Expected '.' after FIND statement")?;

        Ok(self.stmt(StatementKind::Find {
            find_type,
            buffer,
            key_value,
            where_clause,
            lock_type,
            no_error,
        }))
    }

    // Parse Case statement and when clauses
    fn parse_case_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume CASE
        let expression = self.parse_expression()?;
        // ABL requires ':' as block opener; some code uses '.' — accept either
        if self.check(Kind::Period) {
            self.advance();
        } else {
            self.expect_kind(Kind::Colon, "Expected a ':' after CASE expression")?;
        }

        let mut when_branches = Vec::new();

        while self.check(Kind::When) {
            self.advance();
            // Parse the WHEN condition expression.
            // Use parse_expression() to allow logical OR within the condition
            // (e.g. WHEN expr1 OR expr2 THEN).
            // When OR is followed immediately by WHEN (WHEN "a" OR WHEN "b"),
            // treat it as a multi-value pattern instead.
            let mut values = vec![self.parse_and()?];

            // handle WHEN "a" OR WHEN "b" syntax:
            // only treat OR as a WHEN-separator when the next non-comment token after OR is WHEN
            while self.check(Kind::Or) && self.peek_nth_non_comment(2).kind == Kind::When {
                self.advance(); // consume OR
                // skip any inline comments between OR and WHEN
                while self.check(Kind::Comment) {
                    self.advance();
                }
                self.advance(); // consume WHEN
                values.push(self.parse_and()?);
            }

            // If OR remains (logical OR within the WHEN expression), consume the rest
            // of the condition to avoid leaving it unconsumed before THEN
            while self.check(Kind::Or) {
                self.advance(); // consume OR
                let rhs = self.parse_and()?;
                // Combine into a logical OR expression using the AST Or variant
                if let Some(last) = values.last_mut() {
                    let lo = last.span.start;
                    let hi = self.prev_end();
                    *last = self.spanned_expr(
                        lo,
                        hi,
                        ExpressionKind::Or(Box::new(last.clone()), Box::new(rhs)),
                    );
                }
            }

            self.expect_kind(Kind::Then, "Expected THEN after WHEN value")?;

            // parse statements until next WHEN, OTHERWISE, or END
            let mut body = Vec::new();
            while !self.check(Kind::When)
                && !self.check(Kind::Otherwise)
                && !self.check(Kind::End)
                && !self.at_end()
            {
                self.parse_block_statement_recovering(&mut body);
            }

            crate::parser::debug_assert_expr_sibling_order(&values);
            when_branches.push(WhenBranch { values, body });
        }

        let otherwise = if self.check(Kind::Otherwise) {
            self.advance();
            let mut body = Vec::new();
            while !self.check(Kind::End) && !self.at_end() {
                self.parse_block_statement_recovering(&mut body);
            }
            Some(body)
        } else {
            None
        };

        self.recover_block_end(
            Some(Kind::Case),
            "Expected END",
            "Expected '.' after END CASE",
        );

        Ok(self.stmt(StatementKind::Case {
            expression,
            when_branches,
            otherwise,
        }))
    }

    fn parse_procedure(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume PROCEDURE

        // Procedure names can be quoted strings: PROCEDURE "checkout":
        let mut name = if self.check(Kind::StringLiteral) {
            let tok = self.advance().clone();
            Identifier {
                span: Span {
                    start: tok.start as u32,
                    end: tok.end as u32,
                },
                name: self.source[tok.start..tok.end]
                    .trim_matches('"')
                    .to_string(),
            }
        } else {
            self.parse_identifier()?
        };

        // Dotted version-number procedure names: e.g. setup_18.4.2_spdata lexes as
        // Identifier("setup_18") + DecimalLiteral(".4") + DecimalLiteral(".2") + Identifier("_spdata").
        // Consume the extra tokens to advance past the full name.
        while self.check(Kind::DecimalLiteral) {
            self.advance(); // consume ".4", ".2", etc.
            // An underscore/alpha-led segment directly following (e.g. "_spdata") is a separate Identifier.
            if self.check(Kind::Identifier) {
                self.advance();
            }
        }

        // Dotted method-name suffix: e.g. "Procedure Dataset.Fill:" where Dataset is a keyword.
        // Only consume if the next token is a plain identifier or can_be_identifier, and it's on
        // the same line. Block keywords like END/WHEN/THEN must not be consumed here.
        if self.check(Kind::Period) {
            let period_end = self.tokens[self.current].end;
            let next_idx = self.current + 1;
            if let Some(next_tok) = self.tokens.get(next_idx) {
                let nk = next_tok.kind;
                if (nk == Kind::Identifier || Self::can_be_identifier(nk))
                    && !self.source[period_end..next_tok.start].contains('\n')
                {
                    self.advance(); // consume '.'
                    let seg = self.advance().clone();
                    let suffix = self.source[seg.start..seg.end].to_string();
                    name.name = format!("{}.{}", name.name, suffix);
                    name.span.end = seg.end as u32;
                }
            }
        }

        // Skip optional EXTERNAL "dll-name" [calling-convention] [PERSISTENT] clause
        // e.g. PROCEDURE foo EXTERNAL "mylib.dll" cdecl:
        // The DLL name may also be a preprocessor reference: EXTERNAL {&DLL_NAME}
        if self.check(Kind::External) {
            self.advance(); // consume EXTERNAL
            // DLL name: string literal or preprocessor/include reference
            if self.check(Kind::StringLiteral)
                || self.check(Kind::Preprop)
                || self.check(Kind::IncludeReference)
            {
                self.advance(); // consume the DLL path
            }
            // Optional calling convention identifier (cdecl, pascal, ordinal, etc.)
            if Self::can_be_identifier(self.peek().kind) {
                self.advance();
            }
            if self.check(Kind::Persistent) {
                self.advance();
            }
        }

        // Skip optional access modifier: PROCEDURE name PRIVATE: (or PUBLIC/PROTECTED)
        if matches!(
            self.peek().kind,
            Kind::Private | Kind::Public | Kind::Protected
        ) {
            self.advance();
        }

        // Optional super-procedure / external-procedure reference:
        //   PROCEDURE name IN SUPER:
        //   PROCEDURE name IN THIS-PROCEDURE:
        //   PROCEDURE name IN hProc.
        //   PROCEDURE name IN {&HANDLE}:
        // Target is elided from the AST (same as EXTERNAL) — name + body remain.
        // ADM2 prototype includes (smrtprto.i, qryprto.i, …) rely on this form (#65).
        if self.check(Kind::KwIn) {
            self.advance(); // consume IN
            if self.check(Kind::ThisProcedure)
                || self.check(Kind::StringLiteral)
                || self.check(Kind::Preprop)
                || self.check(Kind::IncludeReference)
                || self.check(Kind::Identifier)
                || Self::can_be_identifier(self.peek().kind)
            {
                self.advance(); // SUPER | THIS-PROCEDURE | handle | {&…}
            } else {
                return Err(ParseError {
                    message: "Expected SUPER, THIS-PROCEDURE, or handle after IN".to_string(),
                    span: self.current_span(),
                });
            }

            // Period-only form is a header/prototype with no body — do not treat
            // '.' as a legacy body opener (that would swallow following statements
            // until END). Colon form continues to body-until-END below.
            if self.check(Kind::Period) {
                self.advance();
                return Ok(self.stmt(StatementKind::Procedure {
                    name,
                    body: Vec::new(),
                }));
            }
        }

        // Accept both ':' and '.' as the body opener (legacy ABL uses '.')
        if self.check(Kind::Colon) || self.check(Kind::Period) {
            self.advance();
        } else {
            return Err(ParseError {
                message: "Expected ':' after procedure name".to_string(),
                span: self.current_span(),
            });
        }

        // parse body until END
        let mut body = Vec::new();
        while !self.check(Kind::End) && !self.at_end() {
            // Handle CATCH and FINALLY blocks that may appear at the end of a PROCEDURE body
            if self.check(Kind::Catch) {
                body.push(self.parse_catch_block()?);
                continue;
            }
            if self.check(Kind::Finally) {
                body.push(self.parse_finally_block()?);
                continue;
            }
            self.parse_block_statement_recovering(&mut body);
        }

        // Recover a missing END rather than discarding the whole PROCEDURE.
        if self.check(Kind::End) {
            self.advance();
            // END PROCEDURE or just END. both are valid.
            if self.check(Kind::Procedure) {
                self.advance();
            }
            // Period is required but may be missing in legacy code — optional.
            if self.check(Kind::Period) {
                self.advance();
            }
        } else {
            self.record_error(ParseError {
                message: "Expected END at end of PROCEDURE body".to_string(),
                span: self.current_span(),
            });
        }

        Ok(self.stmt(StatementKind::Procedure { name, body }))
    }

    // parse RUN statements
    fn parse_run_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume RUN

        // Parse target: VALUE(expr), string literal, or procedure name
        let target = if self.check(Kind::Value) {
            self.advance();
            self.expect_kind(Kind::LeftParen, "Expected '(' after VALUE")?;
            let expr = self.parse_expression()?;
            self.expect_kind(Kind::RightParen, "Expected ')' after VALUE expression")?;
            RunTarget::Dynamic(expr)
        } else if self.check(Kind::StringLiteral) {
            // String literal target: RUN "my-proc.p".
            let token = self.advance().clone();
            let name = self.source[token.start + 1..token.end - 1].to_string();
            RunTarget::Literal(name)
        } else {
            // Procedure name (may contain hyphens, dots for .p/.w/.r/.i/.cls files)
            let name = self.parse_procedure_name()?;
            RunTarget::Literal(name)
        };

        // parse optional arguments
        let arguments = if self.check(Kind::LeftParen) {
            self.advance();
            let mut args = Vec::new();

            if !self.check(Kind::RightParen) {
                loop {
                    let direction = match self.peek().kind {
                        Kind::Input => {
                            self.advance();
                            ParameterDirection::Input
                        }
                        Kind::Output => {
                            self.advance();
                            ParameterDirection::Output
                        }
                        Kind::InputOutput => {
                            self.advance();
                            ParameterDirection::InputOutput
                        }
                        _ => ParameterDirection::Input, // Default to INPUT
                    };
                    // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE keyword
                    if matches!(
                        self.peek().kind,
                        Kind::Table | Kind::TableHandle | Kind::Dataset | Kind::DatasetHandle
                    ) {
                        self.advance();
                    }

                    let expression = self.parse_expression()?;
                    // Consume optional IN FRAME/BROWSE qualifier (e.g. field:handle IN FRAME f)
                    if self.check(Kind::KwIn)
                        && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                    {
                        self.advance(); // consume IN
                        self.advance(); // consume FRAME or BROWSE
                        self.advance(); // consume name
                    }
                    let last_was_include =
                        matches!(expression.kind, ExpressionKind::IncludeReference { .. });
                    args.push(RunArgument {
                        direction,
                        expression,
                    });
                    // Consume optional passing modifiers (BIND, BY-VALUE, BY-REFERENCE, APPEND)
                    while matches!(
                        self.peek().kind,
                        Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                    ) {
                        self.advance();
                    }

                    if self.check(Kind::Comma) {
                        self.advance(); // consume comma
                    } else if last_was_include && !self.check(Kind::RightParen) {
                        // An include reference (e.g. {ms/global-out.i &COMMA}) expands to an
                        // argument plus a trailing comma — allow any next token to follow.
                    } else if matches!(
                        self.peek().kind,
                        Kind::Input | Kind::Output | Kind::InputOutput | Kind::IncludeReference
                    ) {
                        // A bare direction keyword or include ref may follow without a comma.
                    } else {
                        break;
                    }
                }
            }

            self.expect_kind(Kind::RightParen, "Expected ')' after RUN arguments")?;
            args
        } else {
            Vec::new()
        };

        // Parse optional modifiers in a flexible loop — ABL allows these in varying orders:
        // IN handle, ON [SERVER] name, PERSISTENT [SET handle], ASYNCHRONOUS [...], SET handle
        let mut in_handle: Option<Expression> = None;
        let mut persistent = false;
        let mut persistent_handle: Option<Expression> = None;
        let mut asynchronous = false;
        let mut async_handle: Option<Expression> = None;
        let mut event_procedure: Option<Expression> = None;

        loop {
            if self.check(Kind::KwIn) {
                self.advance();
                in_handle = Some(self.parse_run_in_handle()?);
            } else if self.check(Kind::On) && Self::can_be_identifier(self.peek_at(1).kind) {
                // ON SERVER name — or ON handle (AppServer targeting)
                self.advance(); // consume ON
                let next_tok = self.peek();
                let next_is_server =
                    self.source[next_tok.start..next_tok.end].eq_ignore_ascii_case("server");
                self.advance(); // consume SERVER or handle name
                if next_is_server && Self::can_be_identifier(self.peek().kind) {
                    self.advance(); // consume server name after SERVER keyword
                }
            } else if self.check(Kind::Persistent) {
                self.advance();
                persistent = true;
                if self.check(Kind::Set) {
                    self.advance();
                    let ident = self.parse_identifier()?;
                    let sp = ident.span;
                    persistent_handle = Some(self.spanned_expr(
                        sp.start,
                        sp.end,
                        ExpressionKind::Identifier(ident),
                    ));
                }
            } else if self.check(Kind::Asynchronous) {
                self.advance();
                asynchronous = true;
                if self.check(Kind::Set) {
                    self.advance();
                    async_handle = Some(self.parse_expression()?);
                }
                if self.check(Kind::EventProcedure) {
                    self.advance();
                    event_procedure = Some(self.parse_expression()?);
                }
            } else if self.check(Kind::Set) {
                // Standalone SET handle (e.g. RUN VALUE(...) SET hHandle ...)
                self.advance();
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
            } else {
                break;
            }
        }

        // parse optional argument list that appears after modifiers
        // (ABL allows: RUN proc IN handle (args). as well as RUN proc (args) IN handle.)
        let arguments = if arguments.is_empty() && self.check(Kind::LeftParen) {
            self.advance();
            let mut args = Vec::new();
            if !self.check(Kind::RightParen) {
                loop {
                    let direction = match self.peek().kind {
                        Kind::Input => {
                            self.advance();
                            ParameterDirection::Input
                        }
                        Kind::Output => {
                            self.advance();
                            ParameterDirection::Output
                        }
                        Kind::InputOutput => {
                            self.advance();
                            ParameterDirection::InputOutput
                        }
                        _ => ParameterDirection::Input,
                    };
                    // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE keyword
                    if matches!(
                        self.peek().kind,
                        Kind::Table | Kind::TableHandle | Kind::Dataset | Kind::DatasetHandle
                    ) {
                        self.advance();
                    }
                    let expression = self.parse_expression()?;
                    let last_was_include =
                        matches!(expression.kind, ExpressionKind::IncludeReference { .. });
                    args.push(RunArgument {
                        direction,
                        expression,
                    });
                    // Consume optional passing modifiers (BIND, BY-VALUE, BY-REFERENCE, APPEND)
                    while matches!(
                        self.peek().kind,
                        Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                    ) {
                        self.advance();
                    }
                    if self.check(Kind::Comma) {
                        self.advance();
                    } else if last_was_include && !self.check(Kind::RightParen) {
                        // Include ref (e.g. {ms/global-out.i &COMMA}) expands to include a
                        // trailing comma — continue without consuming a literal comma.
                    } else if matches!(
                        self.peek().kind,
                        Kind::Input | Kind::Output | Kind::InputOutput | Kind::IncludeReference
                    ) {
                        // Bare direction keyword or include ref follows without a comma.
                    } else {
                        break;
                    }
                }
            }
            self.expect_kind(Kind::RightParen, "Expected ')' after RUN arguments")?;
            args
        } else {
            arguments
        };

        // Handle any remaining modifiers after the arg list
        loop {
            if self.check(Kind::On) && Self::can_be_identifier(self.peek_at(1).kind) {
                self.advance();
                let next_tok = self.peek();
                let next_is_server =
                    self.source[next_tok.start..next_tok.end].eq_ignore_ascii_case("server");
                self.advance();
                if next_is_server && Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
            } else if self.check(Kind::Set) {
                self.advance();
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance();
                }
            } else if self.check(Kind::Persistent) {
                self.advance();
                persistent = true;
                if self.check(Kind::Set) {
                    self.advance();
                    if Self::can_be_identifier(self.peek().kind) {
                        self.advance();
                    }
                }
            } else {
                break;
            }
        }

        // parse optional NO-ERROR
        let no_error = if self.check(Kind::NoError) {
            self.advance();
            true
        } else {
            false
        };

        // Skip any trailing content before the period (e.g. hold-code.hold-logic or extra ')')
        // and consume the terminating period.
        if !self.check(Kind::Period) && !self.at_end() {
            let _ = self.skip_to_statement_end();
        } else {
            self.expect_period("Expected '.' after RUN statement")?;
        }

        Ok(self.stmt(StatementKind::Run {
            target,
            arguments,
            in_handle,
            persistent,
            persistent_handle,
            asynchronous,
            async_handle,
            event_procedure,
            no_error,
        }))
    }

    /// Parse a handle expression for `RUN ... IN <handle>`.
    /// Restricted: parses identifier + optional colon-member/array postfix,
    /// but NEVER treats a following `(` as a function call.
    /// The `(` belongs to the RUN argument list.
    fn parse_run_in_handle(&mut self) -> ParseResult<Expression> {
        let identifier = self.parse_identifier()?;
        let lo = identifier.span.start;
        let mut expr = self.spanned_expr(
            lo,
            identifier.span.end,
            ExpressionKind::Identifier(identifier),
        );

        loop {
            if self.check(Kind::Colon) {
                let colon_end = self.tokens[self.current].end;
                let next_is_member = self.tokens.get(self.current + 1).is_some_and(|t| {
                    Self::can_be_identifier(t.kind)
                        && !self.source[colon_end..t.start].contains('\n')
                });
                if next_is_member {
                    self.advance(); // consume ':'
                    let member = self.parse_identifier()?;
                    let hi = self.prev_end();
                    expr = self.spanned_expr(
                        lo,
                        hi,
                        ExpressionKind::MemberAccess {
                            object: Box::new(expr),
                            member,
                        },
                    );
                } else {
                    break;
                }
            } else if self.check(Kind::LeftBracket) {
                self.advance(); // consume '['
                let index = self.parse_expression()?;
                self.expect_kind(Kind::RightBracket, "Expected ']' after array index")?;
                let hi = self.prev_end();
                expr = self.spanned_expr(
                    lo,
                    hi,
                    ExpressionKind::ArrayAccess {
                        array: Box::new(expr),
                        index: Box::new(index),
                    },
                );
            } else {
                break;
            }
        }

        Ok(expr)
    }

    // Parse DISPLAY statement
    fn parse_display_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DISPLAY

        // Optional STREAM clause: DISPLAY STREAM stream-name ...
        let stream_name = if self.check(Kind::Stream) {
            self.advance();
            Some(self.parse_identifier()?)
        } else {
            None
        };

        let mut items = Vec::new();
        let mut except = Vec::new();
        let mut frame = None;

        // Parse display items until WITH, EXCEPT, FRAME, or period
        while !self.check(Kind::With)
            && !self.check(Kind::Except)
            && !self.check(Kind::Frame)
            && !self.check(Kind::Period)
            && !self.at_end()
        {
            // Standalone SKIP [n] or SPACE [n] directives between display items
            if self.check(Kind::Skip) || self.check(Kind::Space) {
                self.advance();
                if self.check(Kind::IntegerLiteral) {
                    self.advance();
                }
                continue;
            }

            // NO-ATTR-SPACE — display-level option that may appear anywhere in the item list
            if self.check(Kind::NoAttrSpace) {
                self.advance();
                continue;
            }

            // WHEN condition item — condition preceding the display item (pre-WHEN form)
            // Also handles WHEN cond WITH/FRAME/. where there is no following display item.
            if self.check(Kind::When) {
                self.advance(); // consume WHEN
                self.parse_expression().ok(); // parse the condition
                // If we're now at a statement terminator, the WHEN was trailing; restart loop
                // so the outer while condition can exit cleanly.
                if self.check(Kind::With)
                    || self.check(Kind::Except)
                    || self.check(Kind::Frame)
                    || self.check(Kind::Period)
                    || self.at_end()
                {
                    continue;
                }
            }

            let expression = self.parse_expression()?;

            // Skip per-item display options (FORMAT, WHEN, AT, VIEW-AS, etc.)
            // These can appear in any order after the item expression.
            let mut when_condition = None;
            loop {
                if self.check(Kind::Format)
                    || self.check(Kind::Form)
                    || self.check(Kind::ColumnLabel)
                    || self.check(Kind::Label)
                {
                    self.advance();
                    self.skip_format_value();
                } else if self.check(Kind::When) {
                    // Post-item WHEN condition (can appear after FORMAT or AT options)
                    self.advance();
                    when_condition = Some(self.parse_expression()?);
                } else if self.check(Kind::At) {
                    // AT column-number or @ column-reference
                    self.advance();
                    if !self.check(Kind::Period) && !self.check(Kind::With) {
                        self.parse_expression().ok();
                    }
                } else if self.check(Kind::ViewAs) {
                    // VIEW-AS widget-type [SIZE[CHARS] n BY m] — consume widget descriptor
                    self.advance(); // consume VIEW-AS
                    // Consume widget type (text, editor, fill-in, etc.)
                    // TEXT is a keyword but valid as widget type here.
                    if Self::can_be_identifier(self.peek().kind) || self.check(Kind::Text) {
                        self.advance();
                    }
                    // Consume optional SIZE [SIZE-CHARS / CHARS] n BY m
                    while matches!(
                        self.peek().kind,
                        Kind::IntegerLiteral | Kind::DecimalLiteral | Kind::By
                    ) || (Self::can_be_identifier(self.peek().kind)
                        && !self.check(Kind::When)
                        && !self.check(Kind::At)
                        && !self.check(Kind::Period)
                        && !self.check(Kind::With))
                    {
                        self.advance();
                    }
                } else if self.check(Kind::NoLabels) || self.check(Kind::NoAttrSpace) {
                    self.advance();
                } else if self.check(Kind::Skip) || self.check(Kind::Space) {
                    // SKIP [n] or SPACE [n]
                    self.advance();
                    if self.check(Kind::IntegerLiteral) {
                        self.advance();
                    }
                } else if self.check(Kind::Colon) {
                    // COLON n — column position specifier
                    self.advance();
                    if self.check(Kind::IntegerLiteral) || self.check(Kind::DecimalLiteral) {
                        self.advance();
                    }
                } else if self.check(Kind::To) {
                    // TO n — ending column specifier
                    self.advance();
                    if self.check(Kind::IntegerLiteral) || self.check(Kind::DecimalLiteral) {
                        self.advance();
                    }
                } else {
                    break;
                }
            }

            items.push(DisplayItem {
                expression,
                when_condition,
            });
        }

        // Parse optional EXCEPT clause
        if self.check(Kind::Except) {
            self.advance();
            while !self.check(Kind::With) && !self.check(Kind::Period) && !self.at_end() {
                except.push(self.parse_identifier()?);
            }
        }

        // Parse optional FRAME clause (may appear without WITH)
        if self.check(Kind::Frame) {
            self.advance();
            frame = Some(self.parse_identifier()?);
            // Use skip_to_statement_end() to avoid stopping at field-access dots
            // inside expressions (e.g. title "..." + table.field + "...").
            let _ = self.skip_to_statement_end();
            return Ok(self.stmt(StatementKind::Display {
                stream_name,
                items,
                except,
                frame,
            }));
        }

        // Parse optional WITH FRAME clause
        if self.check(Kind::With) {
            self.advance();
            if self.check(Kind::Frame) {
                self.advance();
                frame = Some(self.parse_identifier()?);
            }
            // Skip all WITH options to statement end (handles field-access dots in
            // expressions like `title "prefix" + table.field + "suffix"`).
            let _ = self.skip_to_statement_end();
            return Ok(self.stmt(StatementKind::Display {
                stream_name,
                items,
                except,
                frame,
            }));
        }

        self.expect_period("Expected '.' after DISPLAY statement")?;

        Ok(self.stmt(StatementKind::Display {
            stream_name,
            items,
            except,
            frame,
        }))
    }

    // Parse MESSAGE statement
    fn parse_message_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume MESSAGE

        let mut items = Vec::new();
        let mut set_targets = Vec::new();

        // Parse message items until VIEW-AS, SET, UPDATE, or period
        while !self.check(Kind::ViewAs)
            && !self.check(Kind::Set)
            && !self.check(Kind::Update)
            && !self.check(Kind::Period)
            && !self.at_end()
        {
            // Recognize SKIP / SKIP(n) as formatting directives — don't treat as identifiers
            if self.check(Kind::Skip) {
                self.advance();
                // SKIP(n) — consume the parenthesized integer
                if self.check(Kind::LeftParen) {
                    self.advance();
                    if self.check(Kind::IntegerLiteral) {
                        self.advance();
                    }
                    if self.check(Kind::RightParen) {
                        self.advance();
                    }
                }
                continue; // SKIP has no variable refs, skip it
            }

            items.push(self.parse_expression()?);
        }

        // Parse optional VIEW-AS ALERT-BOX clause — skip over without failing
        if self.check(Kind::ViewAs) {
            self.advance(); // consume VIEW-AS
            // Skip tokens until we hit SET, UPDATE, or period
            while !self.check(Kind::Set)
                && !self.check(Kind::Update)
                && !self.check(Kind::Period)
                && !self.at_end()
            {
                self.advance();
            }
        }

        // Parse optional SET or UPDATE variable list
        if self.check(Kind::Set) || self.check(Kind::Update) {
            self.advance(); // consume SET or UPDATE
            // Parse variable names until period or another clause
            while !self.check(Kind::Period) && !self.at_end() {
                // Skip FORMAT "string" if present after a variable
                if self.check(Kind::Format) || self.check(Kind::Form) {
                    self.advance();
                    if self.check(Kind::StringLiteral) {
                        self.advance();
                    }
                    continue;
                }
                // Skip field modifiers: AUTO-RETURN, NO-ECHO, etc.
                if self.check(Kind::AutoReturn) {
                    self.advance();
                    continue;
                }
                // Skip AS DataType annotation (e.g. UPDATE v-fix AS LOGICAL)
                if self.check(Kind::KwAs) {
                    self.advance(); // consume AS
                    let _ = self.parse_data_type(); // consume the data type
                    continue;
                }
                if Self::can_be_identifier(self.peek().kind) {
                    set_targets.push(self.parse_identifier()?);
                } else {
                    break;
                }
            }
        }

        self.expect_period("Expected '.' after MESSAGE statement")?;

        crate::parser::debug_assert_expr_sibling_order(&items);
        Ok(self.stmt(StatementKind::Message { items, set_targets }))
    }

    // Parse ASSIGN statement: ASSIGN target = value [target = value ...].
    fn parse_assign_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume ASSIGN
        // ASSIGN FRAME framename [field...] -- frame-field assignment form (no = pairs)
        // skip_to_period() already consumes the terminating period
        if self.check(Kind::Frame) {
            let _ = self.skip_to_period();
            return Ok(self.stmt(StatementKind::Assign {
                assignments: SmallVec::new(),
            }));
        }
        let assignments = self.parse_assign_pairs()?;
        // Optional NO-ERROR after ASSIGN
        if self.check(Kind::NoError) {
            self.advance();
        }
        // parse_assign_pairs() may have consumed the period via skip_to_statement_end()
        // when '=' was missing; only consume it here if it's still present.
        if self.check(Kind::Period) {
            self.advance();
        }
        Ok(self.stmt(StatementKind::Assign { assignments }))
    }

    /// Parse one or more `target = value` pairs for ASSIGN and BUFFER-COPY ASSIGN clauses.
    /// Stops at period, NO-ERROR, or end of input.
    fn parse_assign_pairs(&mut self) -> ParseResult<SmallVec<[AssignPair; 4]>> {
        let mut assignments = SmallVec::new();
        while !self.check(Kind::Period) && !self.check(Kind::NoError) && !self.at_end() {
            let target = self.parse_additive()?;
            // Optional IN FRAME/BROWSE before '=': widget:attr IN FRAME f = value
            if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
            {
                self.advance(); // consume IN
                self.advance(); // consume FRAME or BROWSE
                self.advance(); // consume name
            }
            // If '=' is missing, the target expression consumed more than just the LHS
            // (e.g. MENU widget-name:HANDLE where MENU is an identifier prefix).
            // Skip to statement end and return accumulated pairs rather than erroring.
            if !self.check(Kind::Equals) {
                let _ = self.skip_to_statement_end();
                return Ok(assignments);
            }
            self.advance(); // consume '='
            // Handle "ASSIGN x = ." — assignment with no value, period terminates.
            // This pattern appears in old character-UI code where the value is set
            // by a prior CHOOSE/UPDATE widget interaction.
            if self.check(Kind::Period) {
                let span = Span {
                    start: self.peek().start as u32,
                    end: self.peek().start as u32,
                };
                assignments.push(AssignPair {
                    target,
                    value: self.spanned_expr(
                        span.start,
                        span.end,
                        ExpressionKind::Literal(oxabl_ast::Literal::Unknown(
                            oxabl_ast::UnknownLiteral { span },
                        )),
                    ),
                });
                continue;
            }
            let value = self.parse_assignment_value()?;
            // Optional IN FRAME framename clause (specifies frame context for widget assignment)
            if self.check(Kind::KwIn) && self.peek_at(1).kind == Kind::Frame {
                self.advance(); // consume IN
                self.advance(); // consume FRAME
                self.advance(); // consume framename
            }
            // Optional WHEN condition: field = expr WHEN condition
            if self.check(Kind::When) {
                self.advance(); // consume WHEN
                self.parse_expression().ok(); // consume condition
            }
            assignments.push(AssignPair { target, value });
        }
        Ok(assignments)
    }

    /// Parse the RHS of `target = value` for ASSIGN and bare assignment.
    ///
    /// Fast path: non-IF values go through `parse_or` (same as historical ASSIGN).
    ///
    /// ADM2 xp-property / include-arg character lists expand to unquoted
    /// comma-separated identifiers after Progress-like quote stripping:
    /// `BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending`.
    /// When the value starts with `ident ',' ident`, fold the list into one
    /// string literal covering the full source span. This only fires on token
    /// sequences that are parse errors without the fold.
    fn parse_assignment_value(&mut self) -> ParseResult<Expression> {
        if self.check(Kind::KwIf) {
            return self.parse_expression();
        }
        if self.looks_like_unquoted_comma_ident_list() {
            return Ok(self.parse_unquoted_comma_ident_list());
        }
        self.parse_or()
    }

    /// `ident ',' ident …` at the current cursor (no `=` after the post-comma ident).
    fn looks_like_unquoted_comma_ident_list(&self) -> bool {
        if !Self::can_be_identifier(self.peek().kind) {
            return false;
        }
        if !self.check_at(1, Kind::Comma) {
            return false;
        }
        self.can_continue_comma_list_at(2)
    }

    /// Whether the token at `ident_offset` can extend an unquoted comma-ident list.
    ///
    /// Excludes `NO-ERROR` / `WHEN` / `IN`, and refuses `ident =` (next ASSIGN pair).
    fn can_continue_comma_list_at(&self, ident_offset: usize) -> bool {
        let ident_kind = self.peek_at(ident_offset).kind;
        if matches!(
            ident_kind,
            Kind::NoError
                | Kind::When
                | Kind::KwIn
                | Kind::Period
                | Kind::Eof
                | Kind::Comma
                | Kind::Equals
        ) {
            return false;
        }
        if !Self::can_be_identifier(ident_kind) {
            return false;
        }
        // Do not swallow `, name =` as a list continuation.
        if self.peek_at(ident_offset + 1).kind == Kind::Equals {
            return false;
        }
        true
    }

    /// Consume `ident (',' ident)+` and emit a synthetic character string literal.
    fn parse_unquoted_comma_ident_list(&mut self) -> Expression {
        let start = self.peek().start as u32;
        self.advance(); // first identifier
        while self.check(Kind::Comma) && self.can_continue_comma_list_at(1) {
            self.advance(); // ','
            self.advance(); // identifier
        }
        let end = self.tokens[self.current - 1].end as u32;
        let text = self.source[start as usize..end as usize].to_string();
        self.spanned_expr(
            start,
            end,
            ExpressionKind::Literal(Literal::String(StringLiteral {
                span: Span { start, end },
                value: text,
            })),
        )
    }

    /// Non-allocating lookahead: does the upcoming token stream look like another
    /// `target = value` pair for bare multi-assign (no leading ASSIGN keyword)?
    ///
    /// Must not parse or allocate NodeIds (ast-invariants.md §2 — dense contiguous
    /// allocator; no speculative parse + rollback).
    fn looks_like_bare_assign_pair_ahead(&self) -> bool {
        if self.at_end() || self.check(Kind::Period) || self.check(Kind::NoError) {
            return false;
        }
        let next = self.peek().kind;
        // Negative guard: next token starts a new statement / block closer.
        if can_start_statement(next)
            || matches!(
                next,
                Kind::PreprocIf
                    | Kind::PreprocElse
                    | Kind::PreprocElseif
                    | Kind::PreprocEndif
                    | Kind::PreprocScopedDefine
                    | Kind::PreprocGlobalDefine
                    | Kind::PreprocUndefine
                    | Kind::KwReturn
                    | Kind::End
                    | Kind::KwElse
                    | Kind::Leave
                    | Kind::Next
            )
        {
            return false;
        }
        // Assignment targets start with an identifier-like token (variable, handle,
        // or keyword-as-ident). Reject punctuation / literals as pair starts.
        if !Self::can_be_identifier(next) {
            return false;
        }

        // Scan forward for `=` at paren/bracket depth 0 before period / EOF.
        // Bound the scan so a runaway stream cannot quadratic-scan forever;
        // chained `h:BUFFER-FIELD(...):BUFFER-VALUE` is well under 64 tokens.
        let mut depth: i32 = 0;
        for i in 0..64 {
            if self.current + i >= self.tokens.len() {
                break;
            }
            let k = self.peek_at(i).kind;
            match k {
                Kind::LeftParen | Kind::LeftBracket => depth += 1,
                Kind::RightParen | Kind::RightBracket => {
                    depth = depth.saturating_sub(1);
                }
                Kind::Equals if depth == 0 => {
                    // Need at least one target token before `=`.
                    return i > 0;
                }
                Kind::Period | Kind::Eof if depth == 0 => return false,
                _ => {}
            }
        }
        false
    }

    // Parse FUNCTION definition
    // FUNCTION name RETURNS type [(params)]:
    //   body
    // END [FUNCTION].
    fn parse_function(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume FUNCTION (identifier)

        // Parse function name
        let name = self.parse_identifier()?;

        // RETURNS / RETURN is optional in ABL function declarations
        if self.check(Kind::Returns) || self.check(Kind::KwReturn) {
            self.advance();
        }

        // Parse return type
        let return_type = self.parse_data_type()?;

        // Consume optional EXTENT [n] on the return type (e.g. "CHARACTER EXTENT 5")
        if self.check(Kind::Extent) {
            self.advance();
            if self.check(Kind::IntegerLiteral) {
                self.advance();
            }
        }

        // Optional access modifier between return type and parameter list
        // (e.g., `function name char private (input p as char):`)
        if matches!(
            self.peek().kind,
            Kind::Public | Kind::Private | Kind::Protected | Kind::PackagePrivate | Kind::KwStatic
        ) {
            self.advance();
        }

        // Optional parameter list in parentheses. Signature params become
        // DefineParameter statements prepended onto the function body so the
        // semantic declare pass binds them into ScopeKind::Function (#68).
        // Prototypes (FORWARD / IN … / MAP TO …) parse params then discard them
        // so prototype-only symbols do not collide with a later definition.
        let signature_params = if self.check(Kind::LeftParen) {
            self.parse_parenthesized_params()?
        } else {
            Vec::new()
        };

        // FORWARD declaration: FUNCTION name [RETURNS] type [(params)] FORWARD.
        // FORWARD is an identifier token (not a reserved keyword).
        let is_forward = self.check(Kind::Identifier) && {
            let tok = self.peek();
            self.source[tok.start..tok.end].eq_ignore_ascii_case("forward")
        };
        if is_forward {
            let _ = signature_params; // discard — prototype has empty body
            self.advance(); // consume FORWARD
            // FORWARD declarations may end with '.' or ':' (legacy ABL uses ':')
            if self.check(Kind::Period) || self.check(Kind::Colon) {
                self.advance();
            } else {
                return Err(ParseError {
                    message: "Expected '.' after FUNCTION FORWARD".to_string(),
                    span: self.current_span(),
                });
            }
            return Ok(self.stmt(StatementKind::Function {
                name,
                return_type,
                body: Vec::new(),
            }));
        }

        // IN super|this-procedure|handle — external function reference (forward declaration)
        // e.g. "function name returns type (params) in super."
        if self.check(Kind::KwIn) {
            let _ = signature_params; // discard — prototype has empty body
            let _ = self.skip_to_statement_end();
            return Ok(self.stmt(StatementKind::Function {
                name,
                return_type,
                body: Vec::new(),
            }));
        }

        // MAP TO name IN handle — external function mapping (class context)
        if self.check(Kind::Map) {
            let _ = signature_params; // discard — prototype has empty body
            while !self.check(Kind::Period) && !self.at_end() {
                self.advance();
            }
            self.expect_period("Expected '.' after MAP TO")?;
            return Ok(self.stmt(StatementKind::Function {
                name,
                return_type,
                body: Vec::new(),
            }));
        }

        // Accept either ':' or '.' to open the function body (legacy ABL uses '.')
        if self.check(Kind::Colon) || self.check(Kind::Period) {
            self.advance();
        } else {
            return Err(ParseError {
                message: "Expected ':' after FUNCTION header".to_string(),
                span: self.current_span(),
            });
        }

        // Parse body until END; prepend signature params so they bind in-scope.
        let mut body = signature_params;
        while !self.check(Kind::End) && !self.at_end() {
            self.parse_block_statement_recovering(&mut body);
        }

        self.recover_block_end(
            Some(Kind::Function),
            "Expected END at end of FUNCTION body",
            "Expected '.' after END FUNCTION",
        );

        Ok(self.stmt(StatementKind::Function {
            name,
            return_type,
            body,
        }))
    }

    // Parse the block body for code blocks like DO, consume till END.
    // Also handles CATCH and FINALLY blocks that appear before END.
    pub(crate) fn parse_block_body(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = Vec::new();

        loop {
            if self.at_end() {
                break;
            }
            // Check for CATCH block
            if self.check(Kind::Catch) {
                statements.push(self.parse_catch_block()?);
                continue;
            }
            // Check for FINALLY block
            if self.check(Kind::Finally) {
                statements.push(self.parse_finally_block()?);
                continue;
            }
            // END is the block terminator — but END TRIGGERS. is a nested trigger
            // block terminator (from CREATE widget ASSIGN ... TRIGGERS:...END TRIGGERS.)
            // that can appear *inside* a DO/FOR/REPEAT body. Consume it and continue
            // so that the outer block's own END. is handled correctly.
            // TRIGGERS lexes as Kind::Triggers (dedicated keyword, not Kind::Identifier).
            if self.check(Kind::End) {
                // Look ahead past any comment tokens to find the real next token.
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
                    continue;
                }
                break; // This END closes the current block.
            }
            self.parse_block_statement_recovering(&mut statements);
        }

        // Consume the END (or record its absence at EOF) — either way the block
        // survives with the statements parsed so far. See recover_block_end.
        self.recover_block_end(
            None,
            "Expected END to close block",
            "Expected '.' to end statement",
        );

        Ok(statements)
    }

    // Parse CATCH e AS ClassName:
    //   body
    // END CATCH.
    fn parse_catch_block(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume CATCH

        let error_var = self.parse_identifier()?;
        self.expect_kind(Kind::KwAs, "Expected AS after CATCH variable")?;

        // Skip optional CLASS keyword: CATCH e AS CLASS SysError:
        if self.check(Kind::Class) {
            self.advance();
        }

        // Parse error class type (e.g., Progress.Lang.Error)
        // This may be a dotted name
        let start = self.peek().start;
        self.advance(); // consume first part
        while self.check(Kind::Period) {
            // Check if next is an identifier (part of class name) vs statement terminator
            if Self::can_be_identifier(self.peek_at(1).kind) {
                self.advance(); // consume dot
                self.advance(); // consume next part
            } else {
                break;
            }
        }
        let end = self.tokens[self.current - 1].end;
        let error_type = self.source[start..end].to_string();

        self.expect_kind(Kind::Colon, "Expected ':' after CATCH type")?;

        // Parse body until END
        let mut body = Vec::new();
        while !self.check(Kind::End) && !self.at_end() {
            self.parse_block_statement_recovering(&mut body);
        }

        self.recover_block_end(
            Some(Kind::Catch),
            "Expected END to close CATCH",
            "Expected '.' after END CATCH",
        );

        Ok(self.stmt(StatementKind::Catch {
            error_var,
            error_type,
            body,
        }))
    }

    // Parse FINALLY:
    //   body
    // END FINALLY.
    fn parse_finally_block(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume FINALLY
        self.expect_kind(Kind::Colon, "Expected ':' after FINALLY")?;

        let mut body = Vec::new();
        while !self.check(Kind::End) && !self.at_end() {
            self.parse_block_statement_recovering(&mut body);
        }

        self.recover_block_end(
            Some(Kind::Finally),
            "Expected END to close FINALLY",
            "Expected '.' after END FINALLY",
        );

        Ok(self.stmt(StatementKind::Finally { body }))
    }

    /// Parses an optional lock type (NO-LOCK, SHARE-LOCK, EXCLUSIVE-LOCK)
    /// Returns ShareLock if no lock type is specified (ABL default)
    pub(crate) fn parse_lock_type(&mut self) -> LockType {
        match self.peek().kind {
            Kind::NoLock => {
                self.advance();
                LockType::NoLock
            }
            Kind::ShareLock => {
                self.advance();
                LockType::ShareLock
            }
            Kind::ExclusiveLock => {
                self.advance();
                LockType::ExclusiveLock
            }
            // Preprocessor variable used as lock type placeholder: {&lock-type}
            // Consume it and return the default; actual lock type is resolved at preproc time.
            Kind::Preprop => {
                self.advance();
                LockType::ShareLock
            }
            _ => LockType::ShareLock, // Default in ABL
        }
    }

    /// Parse a procedure name for RUN statements.
    ///
    /// ABL procedure names can contain hyphens (e.g., `calculate-total`) and may have
    /// file extensions (e.g., `my-proc.p`). Known ABL extensions are `.p`, `.w`, `.r`,
    /// `.i`, and `.cls`. A period followed by a non-extension token is treated as the
    /// statement terminator, not part of the name.
    fn parse_procedure_name(&mut self) -> ParseResult<String> {
        // Procedure names are file paths and can start with any word-like token,
        // including reserved keywords (e.g., `do/doclimit.p`, `for/something.p`).
        // Absolute UNIX paths starting with `/` are also valid (e.g., `RUN /u/live/sofwork.p`).
        if !Self::is_word_kind(self.peek().kind) && !self.check(Kind::Slash) {
            return Err(ParseError {
                message: "Expected procedure name after RUN".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let start = self.peek().start;
        // For absolute paths, consume the leading `/` then the first path segment
        if self.check(Kind::Slash) {
            self.advance(); // consume leading '/'
            if Self::is_word_kind(self.peek().kind) {
                self.advance(); // consume first path segment
            }
        } else {
            self.advance(); // consume the first identifier token
        }

        // Consume additional path components separated by `/` (e.g., `oe/oe_calc_order_total.p`)
        // Path components may start with digits (e.g., `zp/170oe150svd.p`), so also allow
        // IntegerLiteral and DecimalLiteral as path segment starts.
        while self.check(Kind::Slash)
            && (Self::is_word_kind(self.peek_at(1).kind)
                || matches!(
                    self.peek_at(1).kind,
                    Kind::IntegerLiteral | Kind::DecimalLiteral
                ))
        {
            self.advance(); // consume '/'
            self.advance(); // consume next path component
            // A digit-leading component like "170oe150svd" is lexed as two tokens:
            // IntegerLiteral("170") + Identifier("oe150svd"). Consume the rest.
            while Self::is_word_kind(self.peek().kind)
                && !self.check(Kind::Slash)
                && !self.check(Kind::Period)
            {
                self.advance();
            }
        }

        // Check for dotted extension (e.g., my-proc.p) or dotted method name (e.g., Dataset.Fill).
        // Only consume if the next token is a plain identifier or a keyword safe to use as an
        // identifier (via can_be_identifier). Block-terminating keywords like END, WHEN, THEN
        // must NOT be consumed here, even if on the same line.
        // Use check_at (safe) before peek_at (panics on OOB).
        if self.check(Kind::Period)
            && (self.check_at(1, Kind::Identifier) || Self::can_be_identifier(self.peek_at(1).kind))
        {
            let next = self.peek_at(1);
            let ext = &self.source[next.start..next.end];
            let ext_bytes = ext.as_bytes();
            let is_file_ext = match ext_bytes.len() {
                1 => matches!(ext_bytes[0] | 0x20, b'p' | b'w' | b'r' | b'i'),
                2 => ext.eq_ignore_ascii_case("pp"), // preprocessed procedure files (.pp)
                3 => ext.eq_ignore_ascii_case("cls"),
                _ => false,
            };
            // Also accept dotted method names (e.g. Dataset.Fill) on the same line.
            let period_end = self.tokens[self.current].end;
            let same_line = !self.source[period_end..next.start].contains('\n');
            if is_file_ext || same_line {
                self.advance(); // consume the period
                self.advance(); // consume the extension or method name
            }
        }

        let end = self.tokens[self.current - 1].end;
        Ok(self.source[start..end].to_string())
    }

    /// Check if the current token is the start of a find clause (WHERE, lock, no-error, terminator)
    fn is_find_clause_start(&self) -> bool {
        matches!(
            self.peek().kind,
            Kind::KwWhere
                | Kind::NoLock
                | Kind::ShareLock
                | Kind::ExclusiveLock
                | Kind::NoError
                | Kind::NoWait
                | Kind::UseIndex
                | Kind::Period
        )
    }

    /// Parse an include file reference as a statement: {file.i} or {file.i args}
    fn parse_include_reference_statement(&mut self) -> ParseResult<Statement> {
        let token = self.advance().clone();
        let path_and_args = match &token.value {
            TokenValue::String(s) => s.to_string(),
            _ => self.source[token.start + 1..token.end - 1]
                .trim()
                .to_string(),
        };
        let span = Span {
            start: token.start as u32,
            end: token.end as u32,
        };

        // Include references don't require a period terminator since they are
        // preprocessor constructs that expand to arbitrary code. But consume
        // a period if present (common in ABL: `{file.i}.`)
        if self.check(Kind::Period) {
            self.advance();
        }

        Ok(self.stmt(StatementKind::IncludeReference {
            path_and_args,
            span,
        }))
    }

    /// Parse an include positional argument reference as a statement: {0}, {1}, {2}
    fn parse_include_arg_reference_statement(&mut self) -> ParseResult<Statement> {
        let token = self.advance().clone();
        let index = match &token.value {
            TokenValue::Integer(i) => *i as i64,
            _ => {
                return Err(ParseError {
                    message: "Expected integer index in include argument reference".to_string(),
                    span: Span {
                        start: token.start as u32,
                        end: token.end as u32,
                    },
                });
            }
        };
        let span = Span {
            start: token.start as u32,
            end: token.end as u32,
        };

        if self.check(Kind::Period) {
            self.advance();
        }

        Ok(self.stmt(StatementKind::IncludeArgReference { index, span }))
    }

    // ===================== OO-ABL parsing =====================

    /// Parse a CLASS definition.
    ///
    /// ```text
    /// CLASS [ABSTRACT] [FINAL] dotted-name [INHERITS name] [IMPLEMENTS name, ...]:
    ///     body
    /// END CLASS.
    /// ```
    fn parse_class(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume CLASS

        // Parse optional ABSTRACT/FINAL flags (before name, in any order)
        let mut is_abstract = false;
        let mut is_final = false;
        loop {
            if self.check(Kind::Abstract) {
                self.advance();
                is_abstract = true;
            } else if self.check(Kind::Final) {
                self.advance();
                is_final = true;
            } else {
                break;
            }
        }

        // Parse class name (dotted). Use parse_class_qualified_name() so that reserved
        // keywords used as namespace prefixes (e.g. `do.wsdo800obj`) are accepted.
        let name = self.parse_class_qualified_name()?;

        // Parse optional ABSTRACT/FINAL after name (ABL allows either position)
        loop {
            if self.check(Kind::Abstract) {
                self.advance();
                is_abstract = true;
            } else if self.check(Kind::Final) {
                self.advance();
                is_final = true;
            } else {
                break;
            }
        }

        // Parse optional INHERITS
        let inherits = if self.check(Kind::Inherits) {
            self.advance();
            Some(self.parse_class_qualified_name()?)
        } else {
            None
        };

        // Parse optional IMPLEMENTS (comma-separated)
        let mut implements = Vec::new();
        if self.check(Kind::Implements) {
            self.advance();
            implements.push(self.parse_class_qualified_name()?);
            while self.check(Kind::Comma) {
                self.advance();
                implements.push(self.parse_class_qualified_name()?);
            }
        }

        // Skip optional USE-WIDGET-POOL clause (AppBuilder / legacy ABL)
        if self.check(Kind::Identifier) {
            let tok = self.peek().clone();
            let text = &self.source[tok.start..tok.end];
            if text.eq_ignore_ascii_case("use-widget-pool") {
                self.advance();
            }
        }

        // Expect colon to start body
        self.expect_kind(Kind::Colon, "Expected ':' after CLASS header")?;

        // Parse body until END CLASS
        let mut body = Vec::new();
        while !self.at_end() {
            if self.check(Kind::End) {
                self.advance(); // consume END
                // Optional CLASS keyword after END
                if self.check(Kind::Class) {
                    self.advance();
                }
                // Consume the trailing period if present; some files omit it (e.g. "END CLASS /* comment */")
                if self.check(Kind::Period) {
                    self.advance();
                }
                break;
            }
            self.parse_block_statement_recovering(&mut body);
        }

        Ok(self.stmt(StatementKind::Class {
            name,
            inherits,
            implements,
            is_abstract,
            is_final,
            body,
        }))
    }

    /// Parse an INTERFACE definition.
    ///
    /// ```text
    /// INTERFACE name [INHERITS name, ...]:
    ///     body (method signatures, property signatures)
    /// END INTERFACE.
    /// ```
    fn parse_interface(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume INTERFACE

        let name = self.parse_class_qualified_name()?;

        // Parse optional INHERITS (can inherit multiple interfaces)
        let mut inherits = Vec::new();
        if self.check(Kind::Inherits) {
            self.advance();
            inherits.push(self.parse_class_qualified_name()?);
            while self.check(Kind::Comma) {
                self.advance();
                inherits.push(self.parse_class_qualified_name()?);
            }
        }

        self.expect_kind(Kind::Colon, "Expected ':' after INTERFACE header")?;

        // Parse body until END INTERFACE
        let mut body = Vec::new();
        while !self.at_end() {
            if self.check(Kind::End) {
                self.advance(); // consume END
                if self.check(Kind::Interface) {
                    self.advance();
                }
                if let Err(err) = self.expect_period("Expected '.' after END INTERFACE") {
                    self.record_error(err);
                }
                break;
            }
            self.parse_block_statement_recovering(&mut body);
        }

        Ok(self.stmt(StatementKind::Interface {
            name,
            inherits,
            body,
        }))
    }

    /// Parse a METHOD definition.
    ///
    /// ```text
    /// METHOD [access] [STATIC] [ABSTRACT] [OVERRIDE] (VOID | return-type) name (params):
    ///     body
    /// END METHOD.
    /// ```
    fn parse_method(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume METHOD

        let access = self
            .parse_access_modifier()
            .unwrap_or(AccessModifier::Public);

        // Parse optional STATIC, ABSTRACT, OVERRIDE flags and access modifiers (in any order).
        // ABL allows: METHOD OVERRIDE PROTECTED INTEGER name(...) as well as
        //             METHOD PUBLIC STATIC VOID name(...)
        let mut is_static = false;
        let mut is_abstract = false;
        let mut is_override = false;
        loop {
            if self.check(Kind::KwStatic) {
                self.advance();
                is_static = true;
            } else if self.check(Kind::Abstract) {
                self.advance();
                is_abstract = true;
            } else if self.check(Kind::Override) {
                self.advance();
                is_override = true;
            } else if matches!(
                self.peek().kind,
                Kind::Public | Kind::Private | Kind::Protected | Kind::PackagePrivate
            ) {
                // Re-parse access modifier that appears after qualifier flags
                // (e.g. "METHOD OVERRIDE PROTECTED")
                self.advance();
            } else if self.check(Kind::Preprop) || self.check(Kind::IncludeArgReference) {
                // Preprocessor reference standing in for an access modifier —
                // e.g. `METHOD {&method-access-type} JsonObject foo(...):`
                // where `{&method-access-type}` resolves to PUBLIC or PRIVATE.
                // We don't know the value at parse time; `access` stays at its
                // default (Public).
                self.advance();
            } else {
                break;
            }
        }

        // Parse return type: VOID or a data type
        let return_type = if self.check(Kind::Void) {
            self.advance();
            None
        } else {
            let dt = self.parse_data_type()?;
            // Consume optional EXTENT [n] on the return type (e.g. "CHARACTER EXTENT 5")
            if self.check(Kind::Extent) {
                self.advance();
                if self.check(Kind::IntegerLiteral) {
                    self.advance();
                }
            }
            Some(dt)
        };

        // Parse method name
        let name = self.parse_identifier()?;

        // Parse parameter list
        let parameters = self.parse_parenthesized_params()?;

        // Abstract methods have no body — just a period
        if is_abstract {
            self.expect_period("Expected '.' after abstract method signature")?;
            return Ok(self.stmt(StatementKind::Method {
                access,
                is_static,
                is_abstract,
                is_override,
                return_type,
                name,
                parameters,
                body: Vec::new(),
            }));
        }

        // FORWARD declaration: METHOD … FORWARD. — declares the method with
        // no body (class-header forward declaration). FORWARD is an identifier
        // token, not a reserved keyword.
        let is_forward = self.check(Kind::Identifier) && {
            let tok = self.peek();
            self.source[tok.start..tok.end].eq_ignore_ascii_case("forward")
        };
        if is_forward {
            self.advance(); // consume FORWARD
            self.expect_period("Expected '.' after METHOD FORWARD")?;
            return Ok(self.stmt(StatementKind::Method {
                access,
                is_static,
                is_abstract,
                is_override,
                return_type,
                name,
                parameters,
                body: Vec::new(),
            }));
        }

        // Non-abstract: expect ':' or '.' to open the body (legacy ABL uses '.')
        // Track whether we opened with '.' — in interface declarations, '.' is the
        // statement terminator (no body) rather than a body opener. We detect this
        // below by checking if the first meaningful token is END followed by a
        // non-METHOD keyword (e.g. END INTERFACE, END CLASS).
        let opened_with_period = self.check(Kind::Period);
        if self.check(Kind::Colon) || self.check(Kind::Period) {
            self.advance();
        } else {
            return Err(ParseError {
                message: "Expected ':' after METHOD header".to_string(),
                span: self.current_span(),
            });
        }

        // Interface method signatures end with '.' and have no body. Detect this by
        // looking for END (not METHOD) immediately after the '.' opener. In that case,
        // return an empty body so the enclosing INTERFACE parser handles END INTERFACE.
        if opened_with_period && self.check(Kind::End) && !self.check_at(1, Kind::Method) {
            return Ok(self.stmt(StatementKind::Method {
                access,
                is_static,
                is_abstract,
                is_override,
                return_type,
                name,
                parameters,
                body: Vec::new(),
            }));
        }

        let mut body = Vec::new();
        while !self.at_end() {
            if self.check(Kind::End) {
                self.advance(); // consume END
                if self.check(Kind::Method) {
                    self.advance(); // consume METHOD
                }
                if let Err(err) = self.expect_period("Expected '.' after END METHOD") {
                    self.record_error(err);
                }
                break;
            }
            // Handle CATCH and FINALLY blocks that may appear at the end of a METHOD body
            if self.check(Kind::Catch) {
                body.push(self.parse_catch_block()?);
                continue;
            }
            if self.check(Kind::Finally) {
                body.push(self.parse_finally_block()?);
                continue;
            }
            self.parse_block_statement_recovering(&mut body);
        }

        Ok(self.stmt(StatementKind::Method {
            access,
            is_static,
            is_abstract,
            is_override,
            return_type,
            name,
            parameters,
            body,
        }))
    }

    /// Parse a DEFINE PROPERTY statement.
    ///
    /// Called after DEFINE [access] [STATIC] PROPERTY has been partially consumed.
    /// The access modifier and static flag are passed in.
    ///
    /// ```text
    /// DEFINE [access] [STATIC] PROPERTY name AS type [NO-UNDO]
    ///     GET.                    -- auto-getter
    ///     GET: body END GET.      -- computed getter
    ///     SET.                    -- auto-setter
    ///     SET: body END SET.      -- computed setter
    /// ```
    fn parse_define_property(
        &mut self,
        access: AccessModifier,
        is_static: bool,
    ) -> ParseResult<Statement> {
        self.advance(); // consume PROPERTY

        let name = self.parse_identifier()?;
        self.expect_kind(Kind::KwAs, "Expected AS after property name")?;
        let data_type = self.parse_data_type()?;

        // Consume optional EXTENT [n] on the property type (e.g. "AS LONGCHAR EXTENT 2")
        if self.check(Kind::Extent) {
            self.advance();
            if self.check(Kind::IntegerLiteral) {
                self.advance();
            }
        }

        // Consume NO-UNDO and INITIAL [n] in any order (properties can have both)
        let mut no_undo = false;
        loop {
            if self.check(Kind::NoUndo) {
                self.advance();
                no_undo = true;
            } else if self.check(Kind::Initial) {
                self.advance();
                // INITIAL can be followed by a value expression or nothing
                if !matches!(
                    self.peek().kind,
                    Kind::NoUndo
                        | Kind::Get
                        | Kind::Set
                        | Kind::Period
                        | Kind::Public
                        | Kind::Private
                        | Kind::Protected
                        | Kind::PackagePrivate
                ) {
                    self.parse_expression().ok();
                }
            } else {
                break;
            }
        }

        // Parse GET accessor (with optional leading access modifier, e.g. "protected get.")
        if matches!(
            self.peek().kind,
            Kind::Public | Kind::Private | Kind::Protected | Kind::PackagePrivate
        ) && self.check_at(1, Kind::Get)
        {
            self.advance(); // consume access modifier
        }
        let get_body = if self.check(Kind::Get) {
            self.advance(); // consume GET
            // Optional parameter list: GET() or GET(INPUT p AS TYPE)
            if self.check(Kind::LeftParen) {
                let mut depth = 1;
                self.advance(); // consume '('
                while depth > 0 && !self.at_end() {
                    if self.check(Kind::LeftParen) {
                        depth += 1;
                    } else if self.check(Kind::RightParen) {
                        depth -= 1;
                    }
                    if depth > 0 {
                        self.advance();
                    }
                }
                self.advance(); // consume final ')'
            }
            if self.check(Kind::Period) {
                self.advance(); // auto-getter: GET.
                Some(Vec::new())
            } else if self.check(Kind::Colon) {
                self.advance(); // computed getter: GET:
                let mut body = Vec::new();
                while !self.at_end() {
                    if self.check(Kind::End) {
                        self.advance(); // consume END
                        if self.check(Kind::Get) {
                            self.advance();
                        }
                        if let Err(err) = self.expect_period("Expected '.' after END GET") {
                            self.record_error(err);
                        }
                        break;
                    }
                    self.parse_block_statement_recovering(&mut body);
                }
                Some(body)
            } else {
                return Err(ParseError {
                    message: "Expected '.' or ':' after GET".to_string(),
                    span: self.current_span(),
                });
            }
        } else {
            None
        };

        // Parse SET accessor (with optional leading access modifier, e.g. "private set.")
        if matches!(
            self.peek().kind,
            Kind::Public | Kind::Private | Kind::Protected | Kind::PackagePrivate
        ) && self.check_at(1, Kind::Set)
        {
            self.advance(); // consume access modifier
        }
        let mut set_parameters = Vec::new();
        let set_body = if self.check(Kind::Set) {
            self.advance(); // consume SET
            // Optional parameter list: SET (INPUT p AS TYPE)
            if self.check(Kind::LeftParen) {
                set_parameters = self.parse_parenthesized_params()?;
            }
            if self.check(Kind::Period) {
                self.advance(); // auto-setter: SET.
                Some(Vec::new())
            } else if self.check(Kind::Colon) {
                self.advance(); // computed setter: SET:
                let mut body = Vec::new();
                while !self.at_end() {
                    if self.check(Kind::End) {
                        self.advance(); // consume END
                        if self.check(Kind::Set) {
                            self.advance();
                        }
                        if let Err(err) = self.expect_period("Expected '.' after END SET") {
                            self.record_error(err);
                        }
                        break;
                    }
                    self.parse_block_statement_recovering(&mut body);
                }
                Some(body)
            } else {
                return Err(ParseError {
                    message: "Expected '.' or ':' after SET".to_string(),
                    span: self.current_span(),
                });
            }
        } else {
            None
        };

        Ok(self.stmt(StatementKind::Property {
            access,
            is_static,
            name,
            data_type,
            no_undo,
            get_body,
            set_body,
            set_parameters,
        }))
    }

    /// Parse a CONSTRUCTOR definition.
    ///
    /// ```text
    /// CONSTRUCTOR [access] class-name (params):
    ///     body
    /// END CONSTRUCTOR.
    /// ```
    fn parse_constructor(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume CONSTRUCTOR

        let access = self
            .parse_access_modifier()
            .unwrap_or(AccessModifier::Public);

        // Skip optional STATIC keyword (e.g. CONSTRUCTOR STATIC classname():)
        if self.check(Kind::KwStatic) {
            self.advance();
        }

        // Skip the class name (constructor name must match class — semantic concern)
        if Self::can_be_identifier(self.peek().kind) {
            self.advance();
        }

        let parameters = self.parse_parenthesized_params()?;

        // Some include-file-heavy constructors use a period instead of colon to open the body
        // (the include expands to include the actual ':' at preprocessing time).
        if self.check(Kind::Period) {
            self.advance();
        } else {
            self.expect_kind(Kind::Colon, "Expected ':' after CONSTRUCTOR header")?;
        }

        let mut body = Vec::new();
        while !self.at_end() {
            if self.check(Kind::End) {
                self.advance(); // consume END
                // Accept END CONSTRUCTOR, END METHOD, or just END.
                if self.check(Kind::Constructor) || self.check(Kind::Method) {
                    self.advance();
                }
                if let Err(err) = self.expect_period("Expected '.' after END CONSTRUCTOR") {
                    self.record_error(err);
                }
                break;
            }
            self.parse_block_statement_recovering(&mut body);
        }

        Ok(self.stmt(StatementKind::Constructor {
            access,
            parameters,
            body,
        }))
    }

    /// Parse a DESTRUCTOR definition.
    ///
    /// ```text
    /// DESTRUCTOR [PUBLIC] class-name ():
    ///     body
    /// END DESTRUCTOR.
    /// ```
    fn parse_destructor(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DESTRUCTOR

        // Optional PUBLIC
        if self.check(Kind::Public) {
            self.advance();
        }

        // Skip class name
        if Self::can_be_identifier(self.peek().kind) {
            self.advance();
        }

        // Expect empty param list
        self.expect_kind(Kind::LeftParen, "Expected '(' after DESTRUCTOR name")?;
        self.expect_kind(
            Kind::RightParen,
            "Expected ')' — destructors take no parameters",
        )?;

        self.expect_kind(Kind::Colon, "Expected ':' after DESTRUCTOR header")?;

        let mut body = Vec::new();
        while !self.at_end() {
            if self.check(Kind::End) {
                self.advance(); // consume END
                // Accept END DESTRUCTOR, END METHOD, or just END.
                if self.check(Kind::Destructor) || self.check(Kind::Method) {
                    self.advance();
                }
                if let Err(err) = self.expect_period("Expected '.' after END DESTRUCTOR") {
                    self.record_error(err);
                }
                break;
            }
            self.parse_block_statement_recovering(&mut body);
        }

        Ok(self.stmt(StatementKind::Destructor { body }))
    }

    /// Parse a USING statement.
    ///
    /// ```text
    /// USING dotted-name[.*].
    /// ```
    fn parse_using(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume USING

        // Build the type name from dotted identifiers
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected type name after USING".to_string(),
                span: self.current_span(),
            });
        }

        let first_token = self.advance().clone();
        let mut type_name = self.source[first_token.start..first_token.end].to_string();

        // Consume .segment parts, including .* wildcard
        while self.check(Kind::Period) {
            // Peek past period: identifier or * continues the name, anything else is terminator.
            // Exclude preprocessor tokens — they are never part of a dotted type name.
            let next_kind = self.peek_at(1).kind;
            // A period is a name separator only if the next token is on the same line
            // (no newline in the source between the period end and the next token start).
            // This distinguishes "Progress.Lang.Class." (Class on same line = name segment)
            // from "HarmonizationCode.\nclass" (class on next line = statement terminator).
            let period_end = self.tokens[self.current].end;
            let next_token_start = self.peek_at(1).start;
            let same_line = !self.source[period_end..next_token_start].contains('\n');
            let is_name_segment = same_line
                && Self::can_be_identifier(next_kind)
                && !matches!(
                    next_kind,
                    Kind::IncludeReference | Kind::IncludeArgReference | Kind::Preprop
                );
            if is_name_segment {
                self.advance(); // consume .
                let seg = self.advance().clone();
                type_name.push('.');
                type_name.push_str(&self.source[seg.start..seg.end]);
            } else if next_kind == Kind::Star {
                self.advance(); // consume .
                self.advance(); // consume *
                type_name.push_str(".*");
                break; // wildcard is always last
            } else {
                break; // period is statement terminator
            }
        }

        // Consume optional "FROM PROPATH" / "FROM ASSEMBLY" source clause
        if self.check(Kind::From) {
            self.advance(); // consume FROM
            // Consume the source keyword (PROPATH, ASSEMBLY, or an identifier)
            if Self::can_be_identifier(self.peek().kind) || self.check(Kind::Propath) {
                self.advance();
            }
        }

        self.expect_period("Expected '.' after USING statement")?;

        Ok(self.stmt(StatementKind::Using { type_name }))
    }

    // =========================================================================
    // Database manipulation statements
    // =========================================================================

    // CREATE buffer-name [NO-ERROR].
    // CREATE DATASET/DATA-SOURCE/TEMP-TABLE handle [IN WIDGET-POOL pool] [NO-ERROR].
    fn parse_create_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume CREATE

        let target = if let Some(kind) = self.match_create_target_kind() {
            self.advance(); // consume the type keyword
            let mut handle = self.parse_identifier()?;
            // Handle member-access references: CREATE DATASET THIS-OBJECT:myDataset
            // — consume ':member' suffix and extend the handle name.
            // Also consume '.field' suffix: CREATE BUFFER t-buf.bh FOR TABLE(...).
            while (self.check(Kind::Colon) || self.check(Kind::Period))
                && Self::can_be_identifier(self.peek_at(1).kind)
            {
                let sep_end = self.tokens[self.current].end;
                let next_start = self.tokens[self.current + 1].start;
                if self.source[sep_end..next_start].contains('\n') {
                    break;
                }
                let sep = if self.check(Kind::Colon) { ':' } else { '.' };
                self.advance(); // consume ':' or '.'
                let seg = self.advance().clone();
                handle.name.push(sep);
                handle.name.push_str(&self.source[seg.start..seg.end]);
                handle.span.end = seg.end as u32;
            }
            // Consume optional array subscript on the handle: CREATE TEMP-TABLE x[depth].
            if self.check(Kind::LeftBracket) {
                self.advance(); // consume '['
                self.parse_expression().ok();
                if self.check(Kind::RightBracket) {
                    self.advance(); // consume ']'
                }
            }
            // CREATE BUFFER handle FOR TABLE(expr) or FOR TABLE tablename
            if self.check(Kind::KwFor) {
                self.advance(); // consume FOR
                // TABLE keyword (static or dynamic)
                if self.check(Kind::Table) {
                    self.advance();
                    // Dynamic: TABLE(expr), TABLE tablename, or TABLE "string"
                    if self.check(Kind::LeftParen) {
                        self.advance();
                        self.parse_expression().ok();
                        if self.check(Kind::RightParen) {
                            self.advance();
                        }
                    } else {
                        self.parse_expression().ok();
                    }
                }
            }
            let widget_pool = self.parse_optional_widget_pool()?;
            CreateTarget::Handle {
                kind,
                handle,
                widget_pool,
            }
        } else if self.check(Kind::WidgetPool) {
            // CREATE WIDGET-POOL "pool-name". — named pool creation
            self.advance(); // consume WIDGET-POOL
            if self.check(Kind::StringLiteral) {
                self.advance(); // consume pool name string
            }
            CreateTarget::Name(Identifier {
                span: Span { start: 0, end: 0 },
                name: String::new(),
            })
        } else if !Self::can_be_identifier(self.peek().kind) {
            // CREATE WINDOW / CREATE SERVER / CREATE X-DOCUMENT etc. — complex UI/handle
            // creation forms with ASSIGN clauses we don't fully model.  Skip to end.
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        } else if self.check(Kind::Value) && self.check_at(1, Kind::LeftParen) {
            // CREATE VALUE(class-expr) handle NO-ERROR. — dynamic COM/OO object creation.
            // Skip the VALUE(expr) part and consume the handle name.
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        } else {
            let name = self.parse_identifier()?;
            // If a second identifier follows (e.g. CREATE SERVER hService or CREATE X-document hXML),
            // the first token was a type prefix — discard it and use the second as the handle name.
            if Self::can_be_identifier(self.peek().kind)
                && !self.check(Kind::NoError)
                && !self.check(Kind::Period)
            {
                let _ = self.parse_identifier()?;
            }
            // CREATE handle ASSIGN prop = val ... — UI widget creation with property list;
            // skip the ASSIGN block entirely. Use the TRIGGERS-aware variant so that
            // CREATE widget ASSIGN ... TRIGGERS: ... END TRIGGERS. is skipped atomically
            // (otherwise the first period inside the trigger sub-block stops the scan
            // prematurely, leaving block nesting misaligned).
            // CREATE ALIAS name FOR DATABASE value(expr). — database alias creation;
            // skip the FOR DATABASE ... clause entirely.
            if self.check(Kind::Assign) || self.check(Kind::KwFor) {
                let (_, hi) = self.skip_to_statement_end_triggers_aware();
                return Ok(self.skipped_stmt(hi));
            }
            CreateTarget::Name(name)
        };

        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after CREATE statement")?;
        Ok(self.stmt(StatementKind::Create { target, no_error }))
    }

    fn match_create_target_kind(&self) -> Option<CreateTargetKind> {
        match self.peek().kind {
            Kind::Dataset => Some(CreateTargetKind::Dataset),
            Kind::DataSource => Some(CreateTargetKind::DataSource),
            Kind::TempTable => Some(CreateTargetKind::TempTable),
            Kind::Buffer => Some(CreateTargetKind::Buffer),
            Kind::Query => Some(CreateTargetKind::Query),
            _ => None,
        }
    }

    fn parse_optional_widget_pool(&mut self) -> ParseResult<Option<Expression>> {
        if self.check(Kind::KwIn) {
            self.advance(); // consume IN
            self.expect_kind(Kind::WidgetPool, "Expected WIDGET-POOL after IN")?;
            Ok(Some(self.parse_expression()?))
        } else {
            Ok(None)
        }
    }

    // DELETE buffer-name [NO-ERROR].
    fn parse_delete_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume DELETE
        // DELETE WIDGET-POOL "name" — pool deletion
        if self.check(Kind::WidgetPool) {
            self.advance(); // consume WIDGET-POOL
            if self.check(Kind::StringLiteral) {
                self.advance(); // consume pool name
            }
            let no_error = self.parse_no_error();
            self.expect_period("Expected '.' after DELETE statement")?;
            return Ok(self.stmt(StatementKind::Delete {
                buffer: Identifier {
                    span: Span { start: 0, end: 0 },
                    name: String::new(),
                },
                no_error,
            }));
        }
        // Skip optional type prefixes: OBJECT, PROCEDURE, WIDGET, SERVER, ALIAS, etc.
        // DELETE OBJECT handle [NO-ERROR]. — delete a COM/OO object handle
        // The handle may be a complex expression; skip to period.
        // e.g., DELETE PROCEDURE hproc., DELETE SERVER hService., DELETE ALIAS dictdb.
        if self.check(Kind::Alias) {
            // DELETE ALIAS name. — delete a database alias; skip to period.
            let (_, hi) = self.skip_to_statement_end();
            return Ok(self.skipped_stmt(hi));
        }
        if Self::can_be_identifier(self.peek().kind) {
            let token = &self.tokens[self.current];
            let text = self.source[token.start..token.end].to_ascii_lowercase();
            if matches!(text.as_str(), "object") {
                self.advance(); // consume OBJECT
                let (_, hi) = self.skip_to_statement_end();
                return Ok(self.skipped_stmt(hi));
            }
            if matches!(text.as_str(), "procedure" | "widget" | "server")
                && Self::can_be_identifier(self.peek_at(1).kind)
            {
                self.advance(); // skip the type prefix
            }
        }
        let buffer = self.parse_identifier()?;
        // Skip optional VALIDATE(...) clause: DELETE table VALIDATE(true, "msg").
        if self.check(Kind::Validate) {
            self.advance(); // consume VALIDATE
            if self.check(Kind::LeftParen) {
                self.advance();
                while !self.check(Kind::RightParen) && !self.check(Kind::Period) && !self.at_end() {
                    self.advance();
                }
                if self.check(Kind::RightParen) {
                    self.advance();
                }
            }
        }
        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after DELETE statement")?;
        Ok(self.stmt(StatementKind::Delete { buffer, no_error }))
    }

    // RELEASE buffer-name [NO-ERROR].
    fn parse_release_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume RELEASE

        // RELEASE OBJECT handle [NO-ERROR]. — release a COM/OO object
        // RELEASE EXTERNAL PROCEDURE "name". — release a loaded external procedure
        if Self::can_be_identifier(self.peek().kind) {
            let tok = &self.tokens[self.current];
            let text = self.source[tok.start..tok.end].to_ascii_lowercase();
            if text == "object" {
                self.advance(); // consume OBJECT
                let (_, hi) = self.skip_to_statement_end();
                return Ok(self.skipped_stmt(hi));
            }
        }
        if self.check(Kind::External) {
            self.advance(); // consume EXTERNAL
            let (_, hi) = self.skip_to_period();
            return Ok(self.skipped_stmt(hi));
        }

        let buffer = self.parse_identifier()?;
        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after RELEASE statement")?;
        Ok(self.stmt(StatementKind::Release { buffer, no_error }))
    }

    // VALIDATE buffer-name [NO-ERROR].
    fn parse_validate_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume VALIDATE
        let buffer = self.parse_identifier()?;
        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after VALIDATE statement")?;
        Ok(self.stmt(StatementKind::Validate { buffer, no_error }))
    }

    // BUFFER-COPY source [EXCEPT field...] TO target [ASSIGN field = expr ...] [NO-ERROR].
    fn parse_buffer_copy(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume BUFFER-COPY
        let source = self.parse_identifier()?;

        // Source may be a db.table or buf.field reference: consume the dotted suffix.
        if self.check(Kind::Period) && self.is_field_access_ahead() {
            self.advance(); // consume '.'
            if Self::can_be_identifier(self.peek().kind) {
                self.advance(); // consume table/field name
            }
        }

        // Optional USING or EXCEPT clause (field list to include/exclude)
        // Appears BEFORE the TO keyword: BUFFER-COPY src [USING|EXCEPT] f1 f2 TO target
        // Field names may be qualified: USING table.field or buf.field
        if self.check(Kind::Using) || self.check(Kind::Except) {
            self.advance(); // consume USING/EXCEPT
            while Self::can_be_identifier(self.peek().kind) {
                self.advance(); // consume field name (or table/buffer qualifier)
                // Consume optional .field qualifier on same line
                if self.check(Kind::Period) && self.is_field_access_ahead() {
                    self.advance(); // consume '.'
                    if Self::can_be_identifier(self.peek().kind) {
                        self.advance(); // consume field name
                    }
                }
            }
        }

        self.expect_kind(Kind::To, "Expected TO after source buffer in BUFFER-COPY")?;
        let target = self.parse_identifier()?;

        // Optional ASSIGN clause
        let assignments = if self.check(Kind::Assign) {
            self.advance(); // consume ASSIGN
            self.parse_assign_pairs()?
        } else {
            SmallVec::new()
        };

        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after BUFFER-COPY statement")?;

        Ok(self.stmt(StatementKind::BufferCopy {
            source,
            target,
            assignments,
            no_error,
        }))
    }

    // BUFFER-COMPARE source TO target [SAVE RESULT IN lvar] [NO-ERROR].
    fn parse_buffer_compare(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume BUFFER-COMPARE
        let source = self.parse_identifier()?;

        // Optional USING or EXCEPT field-list clause (field1 field2 ...) before TO
        if self.check(Kind::Using) || self.check(Kind::Except) {
            self.advance(); // consume USING/EXCEPT
            while Self::can_be_identifier(self.peek().kind) {
                self.advance(); // consume each field name
            }
        }

        self.expect_kind(
            Kind::To,
            "Expected TO after source buffer in BUFFER-COMPARE",
        )?;
        let target = self.parse_identifier()?;

        // Optional BINARY option (compare byte-by-byte rather than field-by-field)
        if self.is_identifier_text_at(0, "BINARY") {
            self.advance();
        }

        // Optional SAVE [RESULT IN] clause
        // ABL supports both: SAVE lvar  and  SAVE RESULT IN lvar
        let result_var = if self.check(Kind::Save) {
            self.advance(); // consume SAVE
            // Optional RESULT IN prefix
            if self.is_identifier_text_at(0, "RESULT") && self.check_at(1, Kind::KwIn) {
                self.advance(); // consume RESULT
                self.advance(); // consume IN
            }
            Some(self.parse_identifier()?)
        } else {
            None
        };

        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after BUFFER-COMPARE statement")?;

        Ok(self.stmt(StatementKind::BufferCompare {
            source,
            target,
            result_var,
            no_error,
        }))
    }

    // ── Event system parsing ────────────────────────────────────────

    /// Parse event name: string literal, VALUE(expr), or bare identifier.
    ///
    /// Uses a hand-rolled approach matching `parse_run_statement` to avoid
    /// `parse_primary()` or `parse_expression()` consuming parenthesized
    /// arguments as a function call.
    fn parse_event_name(&mut self) -> ParseResult<Expression> {
        if self.check(Kind::Value) {
            // VALUE(expr)
            self.advance();
            self.expect_kind(Kind::LeftParen, "Expected '(' after VALUE")?;
            let expr = self.parse_expression()?;
            self.expect_kind(Kind::RightParen, "Expected ')' after VALUE expression")?;
            Ok(expr)
        } else if self.check(Kind::StringLiteral) {
            // String literal event name — safe to use parse_primary (no function-call promotion)
            self.parse_primary()
        } else if Self::can_be_identifier(self.peek().kind) || self.check(Kind::Identifier) {
            // Bare identifier — do NOT use parse_primary() which promotes identifier( to function call.
            // Instead, parse just the identifier and return it as an Expression::Identifier.
            let ident = self.parse_identifier()?;
            let sp = ident.span;
            Ok(self.spanned_expr(sp.start, sp.end, ExpressionKind::Identifier(ident)))
        } else {
            Err(ParseError {
                message: "Expected event name (string literal, identifier, or VALUE expression)"
                    .to_string(),
                span: self.current_span(),
            })
        }
    }

    /// Parse RUN-style parenthesized arguments: `(INPUT x, OUTPUT y, ...)`.
    fn parse_run_arguments(&mut self) -> ParseResult<Vec<RunArgument>> {
        if !self.check(Kind::LeftParen) {
            return Ok(Vec::new());
        }
        self.advance(); // consume (

        let mut args = Vec::new();
        if !self.check(Kind::RightParen) {
            loop {
                let direction = match self.peek().kind {
                    Kind::Input => {
                        self.advance();
                        ParameterDirection::Input
                    }
                    Kind::Output => {
                        self.advance();
                        ParameterDirection::Output
                    }
                    Kind::InputOutput => {
                        self.advance();
                        ParameterDirection::InputOutput
                    }
                    _ => ParameterDirection::Input,
                };
                // Skip optional TABLE keyword (for temp-table pass-through args)
                if self.check(Kind::Table) {
                    self.advance();
                }

                let expression = self.parse_expression()?;
                // Consume optional IN FRAME/BROWSE qualifier (e.g. field:handle IN FRAME f)
                if self.check(Kind::KwIn)
                    && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                {
                    self.advance(); // consume IN
                    self.advance(); // consume FRAME or BROWSE
                    self.advance(); // consume name
                }
                args.push(RunArgument {
                    direction,
                    expression,
                });

                if self.check(Kind::Comma) {
                    self.advance(); // consume comma
                } else if matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput | Kind::IncludeReference
                ) {
                    // Implicit comma: include reference or bare direction keyword follows
                } else {
                    break;
                }
            }
        }

        self.expect_kind(Kind::RightParen, "Expected ')' after arguments")?;
        Ok(args)
    }

    /// PUBLISH event-name [FROM publisher-handle] [(args...)].
    fn parse_publish_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume PUBLISH

        let event_name = self.parse_event_name()?;

        let from_handle = if self.check(Kind::From) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        let arguments = self.parse_run_arguments()?;

        self.expect_period("Expected '.' after PUBLISH statement")?;

        Ok(self.stmt(StatementKind::Publish {
            event_name,
            from_handle,
            arguments,
        }))
    }

    /// SUBSCRIBE [PROCEDURE subscriber-handle] [TO] event-name {IN handle | ANYWHERE}
    ///   [RUN-PROCEDURE handler-name] [NO-ERROR].
    fn parse_subscribe_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume SUBSCRIBE

        // Optional PROCEDURE subscriber-handle
        let subscriber = if self.check(Kind::Procedure) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Optional TO noise word
        if self.check(Kind::To) {
            self.advance();
        }

        let event_name = self.parse_event_name()?;

        // Required: IN handle or ANYWHERE
        let target = if self.check(Kind::KwIn) {
            self.advance();
            SubscribeTarget::InHandle(self.parse_expression()?)
        } else if self.check(Kind::Anywhere) {
            self.advance();
            SubscribeTarget::Anywhere
        } else {
            return Err(ParseError {
                message: "Expected IN or ANYWHERE after event name in SUBSCRIBE".to_string(),
                span: self.current_span(),
            });
        };

        // Optional RUN-PROCEDURE handler-name
        let run_procedure = if self.check(Kind::RunProcedure) {
            self.advance();
            Some(self.parse_identifier()?)
        } else {
            None
        };

        let no_error = self.parse_no_error();
        self.expect_period("Expected '.' after SUBSCRIBE statement")?;

        Ok(self.stmt(StatementKind::Subscribe {
            subscriber,
            event_name,
            target,
            run_procedure,
            no_error,
        }))
    }

    /// UNSUBSCRIBE [PROCEDURE subscriber-handle] [TO] {event-name | ALL} [IN publisher-handle].
    fn parse_unsubscribe_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume UNSUBSCRIBE

        // Optional PROCEDURE subscriber-handle
        let subscriber = if self.check(Kind::Procedure) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Optional TO noise word
        if self.check(Kind::To) {
            self.advance();
        }

        // event-name or ALL
        let event_name = if self.check(Kind::All) {
            self.advance();
            None
        } else {
            Some(self.parse_event_name()?)
        };

        // Optional IN publisher-handle
        let in_handle = if self.check(Kind::KwIn) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect_period("Expected '.' after UNSUBSCRIBE statement")?;

        Ok(self.stmt(StatementKind::Unsubscribe {
            subscriber,
            event_name,
            in_handle,
        }))
    }

    /// DEFINE [access] [STATIC] [ABSTRACT] EVENT event-name SIGNATURE VOID (params...).
    fn parse_define_event(
        &mut self,
        access: AccessModifier,
        is_static: bool,
        is_abstract: bool,
    ) -> ParseResult<Statement> {
        self.advance(); // consume EVENT

        let name = self.parse_identifier()?;

        self.expect_kind(Kind::Signature, "Expected SIGNATURE after event name")?;
        self.expect_kind(Kind::Void, "Expected VOID after SIGNATURE")?;

        // Parse parameter list using existing helper
        let parameters = self.parse_parenthesized_params()?;

        self.expect_period("Expected '.' after DEFINE EVENT statement")?;

        Ok(self.stmt(StatementKind::DefineEvent {
            access,
            is_static,
            is_abstract,
            name,
            parameters,
        }))
    }

    /// Helper: consume NO-ERROR if present.
    fn parse_no_error(&mut self) -> bool {
        if self.check(Kind::NoError) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Helper: check if token at offset is an identifier with specific text (case-insensitive).
    fn is_identifier_text_at(&self, offset: usize, text: &str) -> bool {
        let token = self.peek_at(offset);
        (token.kind == Kind::Identifier || Self::can_be_identifier(token.kind))
            && self.source[token.start..token.end].eq_ignore_ascii_case(text)
    }

    // ── Stream / Frame parsing ───────────────────────────────────────

    /// Parse DEFINE STREAM stream-name.
    fn parse_define_stream(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume STREAM
        let name = self.parse_identifier()?;
        // Skip any options or include files before the terminating period
        let _ = self.skip_to_statement_end();
        Ok(self.stmt(StatementKind::DefineStream { name }))
    }

    /// Parse DEFINE FRAME frame-name ... .
    /// Simplified: skips all tokens between name and period, storing the raw span.
    fn parse_define_frame(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume FRAME
        let name = self.parse_identifier()?;

        // Record start of unparsed content
        let raw_start = self.peek().start as u32;

        // Skip frame phrase content.  Use skip_to_statement_end() so that field-access dots
        // inside VALIDATE expressions (e.g. t-pc-center.company) are not mistaken for
        // the terminating period.
        // The harvest range is discarded here: this site keeps a real
        // `DefineFrame` node, so there is no `Skipped` payload to hang the names
        // on. The frame-phrase reads it drops are the deferred modelled-tail
        // class tracked by #134.
        let _ = self.skip_to_statement_end();
        let raw_end = self.tokens[self.current - 1].start as u32;
        let raw_span = Span {
            start: raw_start,
            end: raw_end,
        };

        Ok(self.stmt(StatementKind::DefineFrame { name, raw_span }))
    }

    /// Parse INPUT/OUTPUT/INPUT-OUTPUT stream I/O statement.
    ///
    /// All three directions share this function:
    /// 1. Advance past direction keyword
    /// 2. Optional STREAM stream-name
    /// 3. Dispatch on operation: FROM / TO / THROUGH / CLOSE
    fn parse_stream_io(&mut self, direction: StreamDirection) -> ParseResult<Statement> {
        self.advance(); // consume INPUT / OUTPUT / INPUT-OUTPUT

        // Optional STREAM stream-name (or a preprocessor reference expanding to a stream name)
        // e.g. OUTPUT STREAM s-log TO ...  or  OUTPUT {&stream} TO ...
        let stream_name = if self.check(Kind::Stream) {
            self.advance();
            Some(self.parse_identifier()?)
        } else if self.check(Kind::Preprop) || self.check(Kind::IncludeReference) {
            // {&stream} used directly as stream name — consume and treat like a named stream
            let tok = self.advance().clone();
            Some(Identifier {
                span: Span {
                    start: tok.start as u32,
                    end: tok.end as u32,
                },
                name: self.source[tok.start..tok.end].to_string(),
            })
        } else {
            None
        };

        // Dispatch on operation
        let operation = if self.check(Kind::From) {
            if direction == StreamDirection::Output {
                return Err(ParseError {
                    message: "OUTPUT does not support FROM".to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            }
            if direction == StreamDirection::InputOutput {
                return Err(ParseError {
                    message: "INPUT-OUTPUT does not support FROM".to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            }
            self.advance(); // consume FROM
            let target = self.parse_expression()?;
            StreamOperation::From(target)
        } else if self.check(Kind::To) {
            if direction == StreamDirection::Input {
                return Err(ParseError {
                    message: "INPUT does not support TO".to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            }
            if direction == StreamDirection::InputOutput {
                return Err(ParseError {
                    message: "INPUT-OUTPUT does not support TO".to_string(),
                    span: Span {
                        start: self.peek().start as u32,
                        end: self.peek().end as u32,
                    },
                });
            }
            self.advance(); // consume TO
            // Unix/absolute paths start with '/' (e.g. /usr/tmp/log.txt) — not parseable
            // as an expression. Consume greedily until period and return an empty IO statement.
            if self.check(Kind::Slash) {
                let (_, hi) = self.skip_to_statement_end();
                return Ok(self.skipped_stmt(hi));
            }
            let target = self.parse_expression()?;
            let append = if self.check(Kind::Append) {
                self.advance();
                true
            } else {
                false
            };
            StreamOperation::To { target, append }
        } else if self.check(Kind::Through) || self.check(Kind::Thru) {
            self.advance(); // consume THROUGH/THRU
            // The THROUGH target may be a raw OS command (e.g. "date +%Z") that
            // contains tokens invalid in ABL expressions (%, etc.). Try to parse
            // as an expression; if it fails, skip to the statement end and use an
            // Unknown placeholder so the rest of the file can still be parsed.
            let target = match self.parse_expression() {
                Ok(expr) => expr,
                Err(_) => {
                    let _ = self.skip_to_statement_end();
                    let span = self.current_span();
                    let dummy_expr = self.spanned_expr(
                        span.start,
                        span.end,
                        ExpressionKind::Literal(Literal::Unknown(UnknownLiteral { span })),
                    );
                    return Ok(self.stmt(StatementKind::StreamIo {
                        direction,
                        stream_name,
                        operation: StreamOperation::Through(dummy_expr),
                    }));
                }
            };
            StreamOperation::Through(target)
        } else if self.check(Kind::Close) {
            self.advance(); // consume CLOSE
            StreamOperation::Close
        } else {
            return Err(ParseError {
                message: "Expected FROM, TO, THROUGH, or CLOSE after stream direction".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        };

        // Skip optional trailing clauses: NO-ECHO, CONVERT TARGET "...", PAGE-SIZE n, etc.
        // Use skip_to_statement_end to avoid stopping at field-access dots
        // inside VALUE() arguments (e.g. VALUE(warehouse.remote-host-name)).
        let _ = self.skip_to_statement_end();

        Ok(self.stmt(StatementKind::StreamIo {
            direction,
            stream_name,
            operation,
        }))
    }

    // ── Preprocessor parsing ─────────────────────────────────────────

    /// Maximum preprocessor nesting depth to prevent stack overflow.
    const MAX_PREPROC_DEPTH: u32 = 64;

    /// Generic preprocessor &IF parser. The `parse_branch` closure controls
    /// what content type is parsed inside each branch.
    pub(super) fn parse_preproc_if<T>(
        &mut self,
        depth: u32,
        parse_branch: &dyn Fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<PreprocIf<T>> {
        if depth > Self::MAX_PREPROC_DEPTH {
            return Err(ParseError {
                message: "Preprocessor nesting too deep".to_string(),
                span: self.current_span(),
            });
        }

        // Already consumed &IF
        let condition = self.parse_expression()?;
        self.expect_kind(Kind::PreprocThen, "Expected '&THEN' after &IF condition")?;

        let then_branch = parse_branch(self)?;

        let mut elseif_branches = Vec::new();
        while self.check(Kind::PreprocElseif) {
            self.advance(); // consume &ELSEIF
            let elseif_cond = self.parse_expression()?;
            self.expect_kind(
                Kind::PreprocThen,
                "Expected '&THEN' after &ELSEIF condition",
            )?;
            let elseif_body = parse_branch(self)?;
            elseif_branches.push((elseif_cond, elseif_body));
        }

        let else_branch = if self.check(Kind::PreprocElse) {
            self.advance(); // consume &ELSE
            Some(parse_branch(self)?)
        } else {
            None
        };

        self.expect_kind(Kind::PreprocEndif, "Expected '&ENDIF'")?;

        Ok(PreprocIf {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        })
    }

    /// Parse statements until a preprocessor boundary (&ELSEIF, &ELSE, &ENDIF).
    fn parse_block_until_preproc_boundary(&mut self) -> ParseResult<Vec<Statement>> {
        let mut stmts = Vec::new();
        while !self.at_end()
            && !self.check(Kind::PreprocElseif)
            && !self.check(Kind::PreprocElse)
            && !self.check(Kind::PreprocEndif)
        {
            stmts.push(self.parse_statement()?);
        }
        crate::parser::debug_assert_stmt_sibling_order(&stmts);
        Ok(stmts)
    }

    /// &IF ... &THEN stmts [&ELSEIF ... &THEN stmts]... [&ELSE stmts] &ENDIF
    fn parse_preproc_if_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume &IF
        let preproc = self.parse_preproc_if(1, &Self::parse_block_until_preproc_boundary)?;
        Ok(self.stmt(StatementKind::PreprocIf(preproc)))
    }

    /// &SCOPED-DEFINE name [value tokens...] PreprocEnd
    /// &GLOBAL-DEFINE name [value tokens...] PreprocEnd
    fn parse_preproc_define(&mut self) -> ParseResult<Statement> {
        let is_global = self.check(Kind::PreprocGlobalDefine);
        self.advance(); // consume &SCOPED-DEFINE or &GLOBAL-DEFINE

        // The macro variable name can be any token (including reserved keywords like TRANS,
        // TRANSACTION, etc.) — preprocessor names are not restricted to identifiers.
        if self.at_end() || self.check(Kind::PreprocEnd) {
            return Err(ParseError {
                message: "Expected name after &SCOPED-DEFINE / &GLOBAL-DEFINE".to_string(),
                span: self.current_span(),
            });
        }
        let name_tok = self.advance().clone();
        let name = Identifier {
            span: Span {
                start: name_tok.start as u32,
                end: name_tok.end as u32,
            },
            name: self.source[name_tok.start..name_tok.end].to_string(),
        };

        // Collect value span: everything from current position until PreprocEnd or Eof.
        let value_span = if self.check(Kind::PreprocEnd) || self.at_end() {
            None
        } else {
            let start = self.peek().start as u32;
            let mut end = start;
            while !self.check(Kind::PreprocEnd) && !self.at_end() {
                let tok = self.advance();
                end = tok.end as u32;
            }
            Some(Span { start, end })
        };

        // Consume the PreprocEnd if present
        if self.check(Kind::PreprocEnd) {
            self.advance();
        }

        Ok(self.stmt(StatementKind::PreprocDefine {
            name,
            value_span,
            is_global,
        }))
    }

    /// &UNDEFINE name
    fn parse_preproc_undefine(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume &UNDEFINE
        let name = self.parse_identifier()?;
        Ok(self.stmt(StatementKind::PreprocUndefine { name }))
    }

    /// &MESSAGE expression
    fn parse_preproc_message(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume &MESSAGE
        let expression = self.parse_expression()?;
        Ok(self.stmt(StatementKind::PreprocMessage { expression }))
    }

    // =========================================================================
    // ON triggers and TRIGGER PROCEDURE
    // =========================================================================

    /// Parse an ON statement, disambiguating between the 3 forms:
    /// 1. UI/developer event trigger: ON event-list OF widget-list ...
    /// 2. Database event trigger: ON CREATE/DELETE/FIND/WRITE/ASSIGN OF table ...
    /// 3. Key remapping: ON key-label key-function.
    fn parse_on_statement(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume ON

        // Check for string literal event name (e.g., ON "WEB-NOTIFY" ANYWHERE ...)
        // Parsed as UiEvent with the string as the sole event name.
        if self.check(Kind::StringLiteral) {
            return self.parse_on_ui_event();
        }

        // Check for DB events: CREATE/DELETE/FIND/WRITE/ASSIGN followed by OF (not Comma).
        // If followed by Comma, it's a UI event with multiple event names.
        if self.is_db_event_kind(self.peek().kind) && self.check_at(1, Kind::Of) {
            return self.parse_on_db_event();
        }

        // Check for key remapping: ON <ident> <ident> .
        // Two tokens followed by a period, with no OF or comma.
        // UI events always have OF or comma after the event list.
        // Key labels/functions can be any keyword (HELP, ENDKEY, GO, etc.).
        let next_kind = self.peek_at(1).kind;
        if !matches!(next_kind, Kind::Of | Kind::Comma | Kind::Eof | Kind::Period)
            && self.check_at(2, Kind::Period)
        {
            return self.parse_on_key_remap();
        }

        // Default: UI/developer event trigger
        self.parse_on_ui_event()
    }

    /// Parse a UI/developer event trigger:
    /// ON event-list [OF widget-list] [OR event-list OF widget-list]... [ANYWHERE]
    ///   { trigger-block | REVERT | PERSISTENT RUN proc [(args)] }
    fn parse_on_ui_event(&mut self) -> ParseResult<Statement> {
        let mut clauses = Vec::new();
        let mut anywhere = false;

        loop {
            let events = self.parse_trigger_event_list()?;

            // ANYWHERE without OF — standalone form
            if self.check(Kind::Anywhere) && !self.check_at(1, Kind::Of) {
                anywhere = true;
                self.advance();
                break;
            }

            // OF widget-list
            if self.check(Kind::Of) {
                self.advance();
                let widgets = self.parse_widget_ref_list()?;
                clauses.push(OnEventClause { events, widgets });
            } else if clauses.is_empty() && events.is_empty() {
                return Err(ParseError {
                    message: "Expected event name or ANYWHERE after ON".to_string(),
                    span: self.current_span(),
                });
            }

            // Check for OR to chain another event/widget clause
            if self.check(Kind::Or) {
                self.advance();
            } else {
                break;
            }
        }

        // Check for trailing ANYWHERE (after widget list)
        if self.check(Kind::Anywhere) {
            anywhere = true;
            self.advance();
        }

        let action = self.parse_trigger_action()?;
        Ok(self.stmt(StatementKind::On {
            kind: OnKind::UiEvent {
                clauses,
                anywhere,
                action,
            },
        }))
    }

    /// Parse a database event trigger:
    /// ON CREATE|DELETE|FIND|WRITE|ASSIGN OF table [referencing] [OVERRIDE]
    ///   { trigger-block | REVERT }
    fn parse_on_db_event(&mut self) -> ParseResult<Statement> {
        let event = self.parse_db_event_kind()?;
        self.expect_kind(Kind::Of, "Expected OF after database event")?;
        let target = self.parse_dotted_name()?;

        let mut referencing = TriggerReferencing::default();

        // Parse optional referencing phrases for WRITE
        if event == DbTriggerEvent::Write {
            if self.check(Kind::New) {
                self.advance();
                // Optional BUFFER keyword
                if self.check(Kind::Buffer) {
                    self.advance();
                }
                referencing.new_buffer = Some(self.parse_identifier()?);
            }
            if self.check(Kind::Old) {
                self.advance();
                if self.check(Kind::Buffer) {
                    self.advance();
                }
                referencing.old_buffer = Some(self.parse_identifier()?);
            }
        }

        // Parse optional OLD VALUE for ASSIGN
        if event == DbTriggerEvent::Assign && self.check(Kind::Old) {
            self.advance();
            if self.check(Kind::Value) {
                self.advance();
            }
            referencing.old_value = Some(self.parse_identifier()?);
        }

        let is_override = if self.check(Kind::Override) {
            self.advance();
            true
        } else {
            false
        };

        let action = self.parse_trigger_action()?;
        Ok(self.stmt(StatementKind::On {
            kind: OnKind::DbEvent {
                event,
                target,
                referencing,
                is_override,
                action,
            },
        }))
    }

    /// Parse key remapping: ON key-label key-function.
    /// Key labels and functions can be any identifier, including reserved keywords
    /// like HELP, ENDKEY, GO, RETURN, STOP, ERROR, END, TAB, HOME, CLEAR, etc.
    fn parse_on_key_remap(&mut self) -> ParseResult<Statement> {
        let key_label = self.parse_any_keyword_as_identifier()?;
        let key_function = self.parse_any_keyword_as_identifier()?;
        self.expect_period("Expected '.' after key remapping")?;
        Ok(self.stmt(StatementKind::On {
            kind: OnKind::KeyRemap {
                key_label,
                key_function,
            },
        }))
    }

    /// Parse a trigger action: REVERT, PERSISTENT RUN, DO block, or single statement.
    fn parse_trigger_action(&mut self) -> ParseResult<OnAction> {
        // REVERT
        if self.check(Kind::Revert) {
            self.advance();
            self.expect_period("Expected '.' after REVERT")?;
            return Ok(OnAction::Revert);
        }

        // PERSISTENT RUN procedure [(args)]
        if self.check(Kind::Persistent) {
            self.advance();
            self.expect_kind(Kind::Run, "Expected RUN after PERSISTENT")?;
            let procedure = self.parse_identifier()?;
            let arguments = if self.check(Kind::LeftParen) {
                self.parse_persistent_run_args()?
            } else {
                Vec::new()
            };
            self.expect_period("Expected '.' after PERSISTENT RUN")?;
            return Ok(OnAction::PersistentRun {
                procedure,
                arguments,
            });
        }

        // DO...END block
        if self.check(Kind::Do) {
            let block = self.parse_do_statement()?;
            return Ok(OnAction::Block(Box::new(block)));
        }

        // Single statement (terminates with its own period)
        let stmt = self.parse_statement()?;
        Ok(OnAction::Block(Box::new(stmt)))
    }

    /// Parse PERSISTENT RUN arguments: (INPUT expr, ...).
    /// Simplified: just parses comma-separated expressions inside parens.
    fn parse_persistent_run_args(&mut self) -> ParseResult<Vec<Expression>> {
        self.expect_kind(Kind::LeftParen, "Expected '(' for arguments")?;
        let mut args = Vec::new();
        while !self.check(Kind::RightParen) && !self.at_end() {
            // Skip optional INPUT keyword (only INPUT is valid for PERSISTENT RUN)
            if self.check(Kind::Input) {
                self.advance();
            }
            args.push(self.parse_expression()?);
            if !self.check(Kind::RightParen) {
                self.expect_kind(Kind::Comma, "Expected ',' between arguments")?;
            }
        }
        self.expect_kind(Kind::RightParen, "Expected ')' after arguments")?;
        Ok(args)
    }

    /// Parse a comma-separated list of event names (identifiers).
    /// Accepts keywords that double as event names (LEAVE, ENTRY, CREATE, DELETE, etc.).
    fn parse_trigger_event_list(&mut self) -> ParseResult<Vec<Identifier>> {
        let mut events = Vec::new();
        loop {
            if self.can_be_event_name() {
                events.push(self.parse_event_name_identifier()?);
            } else {
                break;
            }
            if !self.check(Kind::Comma) {
                break;
            }
            self.advance(); // consume comma
        }
        Ok(events)
    }

    /// Check if the current token can be an event name in an ON trigger.
    /// Event names include regular identifiers, `can_be_identifier()` keywords,
    /// and reserved keywords that double as event names (LEAVE, ENTRY, HELP, etc.).
    fn can_be_event_name(&self) -> bool {
        let kind = self.peek().kind;
        kind == Kind::Identifier
            || kind == Kind::StringLiteral
            || Self::can_be_identifier(kind)
            || matches!(
                kind,
                Kind::Leave
                    | Kind::Entry
                    | Kind::Create
                    | Kind::Delete
                    | Kind::Close
                    | Kind::Write
                    | Kind::Help
                    | Kind::GoOn
                    | Kind::ErrorStatus
                    | Kind::ValueChanged
                    // RETURN is a statement keyword but also a keyboard event name in ON triggers
                    | Kind::KwReturn
            )
    }

    /// Parse a single event name identifier, including reserved keywords
    /// that serve as event names in ON trigger context.
    fn parse_event_name_identifier(&mut self) -> ParseResult<Identifier> {
        let token = self.peek();
        let kind = token.kind;

        if kind == Kind::StringLiteral {
            // String literal event name (e.g., "WEB-NOTIFY")
            let name = self.source[token.start..token.end].to_string();
            let ident = Identifier {
                name,
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            };
            self.advance();
            Ok(ident)
        } else if kind == Kind::Identifier || Self::can_be_identifier(kind) {
            self.parse_identifier()
        } else if matches!(
            kind,
            Kind::Leave
                | Kind::Entry
                | Kind::Create
                | Kind::Delete
                | Kind::Close
                | Kind::Write
                | Kind::Help
                | Kind::GoOn
                | Kind::ErrorStatus
                | Kind::ValueChanged
                | Kind::KwReturn
        ) {
            // Reserved keywords that are also valid event names
            let name = self.source[token.start..token.end].to_string();
            let ident = Identifier {
                name,
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            };
            self.advance();
            Ok(ident)
        } else {
            Err(ParseError {
                message: "Expected event name".to_string(),
                span: self.current_span(),
            })
        }
    }

    /// Parse a comma-separated list of widget references, each with optional IN FRAME/BROWSE.
    fn parse_widget_ref_list(&mut self) -> ParseResult<Vec<WidgetRef>> {
        let mut refs = Vec::new();
        loop {
            // Handle bare "FRAME name" or "BROWSE name" form: the widget IS the frame/browse.
            // e.g. `of frame fmain anywhere` — consume FRAME and parse the name that follows.
            let name = if self.check(Kind::Frame) || self.check(Kind::Browse) {
                self.advance(); // consume FRAME/BROWSE keyword
                self.parse_identifier()?
            } else {
                let first = self.parse_identifier()?;
                // Handle widget-type + name form: e.g. `MENU-ITEM m_Close` where MENU-ITEM is
                // the widget type prefix and m_Close is the actual widget name.  Only applies
                // when the following token is a plain Identifier (not a reserved keyword used as
                // a trigger action or qualifier — those are Kind::Revert, Kind::Persistent, etc.).
                if self.peek().kind == Kind::Identifier {
                    // First token was the widget type prefix — consume it and use the next as name.
                    self.parse_identifier()?
                } else {
                    first
                }
            };
            let qualifier = if self.check(Kind::KwIn) {
                self.advance();
                if self.check(Kind::Frame) {
                    self.advance();
                    Some(WidgetQualifier::InFrame(self.parse_identifier()?))
                } else if self.check(Kind::Browse) {
                    self.advance();
                    Some(WidgetQualifier::InBrowse(self.parse_identifier()?))
                } else {
                    None
                }
            } else {
                None
            };
            refs.push(WidgetRef { name, qualifier });
            if !self.check(Kind::Comma) {
                break;
            }
            self.advance(); // consume comma
        }
        Ok(refs)
    }

    /// Parse any token as an identifier, even reserved keywords.
    /// Used for key labels and key functions where any keyword is valid.
    fn parse_any_keyword_as_identifier(&mut self) -> ParseResult<Identifier> {
        let token = self.peek();
        if token.kind == Kind::Eof {
            return Err(ParseError {
                message: "Expected identifier".to_string(),
                span: self.current_span(),
            });
        }
        let name = self.source[token.start..token.end].to_string();
        let ident = Identifier {
            name,
            span: Span {
                start: token.start as u32,
                end: token.end as u32,
            },
        };
        self.advance();
        Ok(ident)
    }

    /// Check if a Kind is a database trigger event keyword.
    fn is_db_event_kind(&self, kind: Kind) -> bool {
        matches!(
            kind,
            Kind::Create | Kind::Delete | Kind::Find | Kind::Write | Kind::Assign
        )
    }

    /// Parse a database event keyword and return the corresponding DbTriggerEvent.
    fn parse_db_event_kind(&mut self) -> ParseResult<DbTriggerEvent> {
        let kind = self.peek().kind;
        let event = match kind {
            Kind::Create => DbTriggerEvent::Create,
            Kind::Delete => DbTriggerEvent::Delete,
            Kind::Find => DbTriggerEvent::Find,
            Kind::Write => DbTriggerEvent::Write,
            Kind::Assign => DbTriggerEvent::Assign,
            _ => {
                return Err(ParseError {
                    message: "Expected database event (CREATE, DELETE, FIND, WRITE, or ASSIGN)"
                        .to_string(),
                    span: self.current_span(),
                });
            }
        };
        self.advance();
        Ok(event)
    }

    /// Parse a dotted name like `table.field` for ASSIGN OF targets.
    /// Returns an Identifier whose name contains the full dotted form.
    fn parse_dotted_name(&mut self) -> ParseResult<Identifier> {
        let first = self.parse_identifier()?;
        // Check if a period follows and the next token is an identifier-like token
        // on the SAME LINE (not a statement-starting keyword, end of input, or a
        // token on the next line which would be a statement terminator).
        // This distinguishes `Customer.Name` from `Customer.` (statement terminator).
        if self.check(Kind::Period) {
            let period_end = self.tokens[self.current].end;
            let next_tok = self.peek_at(1);
            let next_kind = next_tok.kind;
            let same_line = !self.source[period_end..next_tok.start].contains('\n');
            if same_line
                && next_kind != Kind::Comment
                && (next_kind == Kind::Identifier || Self::can_be_identifier(next_kind))
            {
                self.advance(); // consume period
                let second = self.parse_identifier()?;
                let name = format!("{}.{}", first.name, second.name);
                let span = Span {
                    start: first.span.start,
                    end: second.span.end,
                };
                return Ok(Identifier { name, span });
            }
        }
        Ok(first)
    }

    /// Parse a TRIGGER PROCEDURE statement:
    /// TRIGGER PROCEDURE FOR event OF table [NEW/OLD clauses].
    fn parse_trigger_procedure(&mut self) -> ParseResult<Statement> {
        self.advance(); // consume TRIGGER
        self.expect_kind(Kind::Procedure, "Expected PROCEDURE after TRIGGER")?;
        self.expect_kind(Kind::KwFor, "Expected FOR after TRIGGER PROCEDURE")?;

        // Parse event kind — also check for REPLICATION-* events
        let event = if self.peek().kind == Kind::Identifier {
            let token = self.peek();
            let text = &self.source[token.start..token.end];
            if text.eq_ignore_ascii_case("replication-create") {
                self.advance();
                DbTriggerEvent::ReplicationCreate
            } else if text.eq_ignore_ascii_case("replication-delete") {
                self.advance();
                DbTriggerEvent::ReplicationDelete
            } else if text.eq_ignore_ascii_case("replication-write") {
                self.advance();
                DbTriggerEvent::ReplicationWrite
            } else {
                self.parse_db_event_kind()?
            }
        } else {
            self.parse_db_event_kind()?
        };

        // ASSIGN has two mutually exclusive forms
        if event == DbTriggerEvent::Assign {
            if self.check(Kind::Of) {
                // OF table.field form
                self.advance();
                let target = self.parse_dotted_name()?;
                self.expect_period("Expected '.' after TRIGGER PROCEDURE")?;
                return Ok(self.stmt(StatementKind::TriggerProcedure {
                    event,
                    target,
                    referencing: TriggerReferencing::default(),
                    new_value: None,
                    old_value_param: None,
                }));
            } else {
                // NEW VALUE form
                self.expect_kind(Kind::New, "Expected NEW or OF after ASSIGN")?;
                if self.check(Kind::Value) {
                    self.advance();
                }
                let new_value = self.parse_trigger_assign_param()?;
                let old_value_param = if self.check(Kind::Old) {
                    self.advance();
                    if self.check(Kind::Value) {
                        self.advance();
                    }
                    Some(self.parse_trigger_assign_param()?)
                } else {
                    None
                };
                self.expect_period("Expected '.' after TRIGGER PROCEDURE")?;
                // Use a placeholder target for the NEW VALUE form
                let target = Identifier {
                    name: String::new(),
                    span: self.current_span(),
                };
                return Ok(self.stmt(StatementKind::TriggerProcedure {
                    event,
                    target,
                    referencing: TriggerReferencing::default(),
                    new_value: Some(new_value),
                    old_value_param,
                }));
            }
        }

        self.expect_kind(Kind::Of, "Expected OF after event")?;
        let target = self.parse_dotted_name()?;

        let mut referencing = TriggerReferencing::default();
        // Write and replication-write triggers support NEW/OLD BUFFER clauses.
        if matches!(
            event,
            DbTriggerEvent::Write
                | DbTriggerEvent::ReplicationWrite
                | DbTriggerEvent::ReplicationCreate
                | DbTriggerEvent::ReplicationDelete
        ) {
            if self.check(Kind::New) {
                self.advance();
                if self.check(Kind::Buffer) {
                    self.advance();
                }
                referencing.new_buffer = Some(self.parse_identifier()?);
            }
            if self.check(Kind::Old) {
                self.advance();
                if self.check(Kind::Buffer) {
                    self.advance();
                }
                referencing.old_buffer = Some(self.parse_identifier()?);
            }
        }

        self.expect_period("Expected '.' after TRIGGER PROCEDURE")?;
        Ok(self.stmt(StatementKind::TriggerProcedure {
            event,
            target,
            referencing,
            new_value: None,
            old_value_param: None,
        }))
    }

    /// Parse a TRIGGER PROCEDURE ASSIGN parameter: name AS type.
    fn parse_trigger_assign_param(&mut self) -> ParseResult<TriggerAssignParam> {
        let name = self.parse_identifier()?;
        self.expect_kind(Kind::KwAs, "Expected AS after parameter name")?;
        let data_type = self.parse_data_type()?;
        Ok(TriggerAssignParam { name, data_type })
    }
}
