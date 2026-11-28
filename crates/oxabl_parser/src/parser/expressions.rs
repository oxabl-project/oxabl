//! Expression parsing for the Oxabl parser.
//!
//! Precedence levels (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative
//! > unary > postfix (member access, method calls, array/field access) > primary.

use oxabl_ast::{
    ExprId, ExprList, Expression, ExpressionKind, FindType, Identifier, Literal, Span,
    UnknownLiteral,
};
use oxabl_lexer::Kind;
use smallvec::SmallVec;

use super::{ParseError, ParseResult, Parser, tok_span};
use crate::literal::token_to_literal;

impl Parser<'_> {
    /// Every expression the grammar nests goes through here or through one of
    /// the few `nested` calls that bypass it, so each is one nesting level.
    pub fn parse_expression(&mut self) -> ParseResult<ExprId> {
        self.nested(Self::parse_ternary)
    }

    pub fn parse_ternary(&mut self) -> ParseResult<ExprId> {
        if !self.check(Kind::KwIf) {
            return self.parse_or();
        }

        // Prefix form: capture `lo` from the IF token before advancing so the
        // span covers the leading IF (R1.1 full-extent rule).
        let lo = self.peek().start;
        self.advance(); // consume IF
        let condition = self.parse_or()?; // condition can use OR/AND/comparison

        // Inside a &ELSEIF condition, IF...&THEN is not a ternary — &THEN terminates the
        // &ELSEIF condition block. Return the condition and leave &THEN for the caller.
        if self.check(Kind::PreprocThen) {
            return Ok(condition);
        }

        self.expect_kind(Kind::Then, "Expected 'THEN' after IF condition")?;

        let then_expr = self.nested(Self::parse_ternary)?;

        self.expect_kind(Kind::KwElse, "Expected 'ELSE' in IF expression")?;

        let else_expr = self.nested(Self::parse_ternary)?;

        let hi = self.prev_end();
        Ok(self.spanned_expr(
            lo,
            hi,
            ExpressionKind::IfThenElse(condition, then_expr, else_expr),
        ))
    }

    pub fn parse_or(&mut self) -> ParseResult<ExprId> {
        let mut links = 0;
        let mut expr = self.parse_and()?;
        while self.check(Kind::Or) {
            self.link(&mut links)?;
            let lo = self.exprs.get(expr).span.start;
            self.advance();
            let right = self.parse_and()?;
            let hi = self.prev_end();
            expr = self.spanned_expr(lo, hi, ExpressionKind::Or(expr, right));
        }
        self.unlink(links);
        Ok(expr)
    }

    pub fn parse_and(&mut self) -> ParseResult<ExprId> {
        let mut links = 0;
        let mut expr = self.parse_comparison()?;
        while self.check(Kind::And) {
            self.link(&mut links)?;
            let lo = self.exprs.get(expr).span.start;
            self.advance();
            let right = self.parse_comparison()?;
            let hi = self.prev_end();
            expr = self.spanned_expr(lo, hi, ExpressionKind::And(expr, right));
        }
        self.unlink(links);
        Ok(expr)
    }

    pub(super) fn is_comparison_operator(&self) -> bool {
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

    pub fn parse_comparison(&mut self) -> ParseResult<ExprId> {
        let left = self.parse_additive()?;

        // Consume optional "IN FRAME/BROWSE name" widget qualifier that scopes a widget attribute
        // before the comparison operator (e.g. widget:attr IN FRAME fname EQ value).
        if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse) {
            self.advance(); // consume IN
            self.advance(); // consume FRAME or BROWSE
            self.consume_widget_name();
        }

        if !self.is_comparison_operator() {
            return Ok(left);
        }

        let lo = self.exprs.get(left).span.start;
        let op_kind = self.advance().kind;
        let right = self.parse_additive()?;

        // Consume optional "IN FRAME/BROWSE name" widget qualifier after the right operand
        // (e.g. field-a eq field-b:screen-value IN BROWSE bname).
        if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse) {
            self.advance(); // consume IN
            self.advance(); // consume FRAME or BROWSE
            self.consume_widget_name();
        }

        let kind = match op_kind {
            Kind::Equals | Kind::Eq => ExpressionKind::Equal(left, right),
            Kind::NotEqual | Kind::Ne => ExpressionKind::NotEqual(left, right),
            Kind::LessThan | Kind::Lt => ExpressionKind::LessThan(left, right),
            Kind::LessThanOrEqual | Kind::Le => ExpressionKind::LessThanOrEqual(left, right),
            Kind::GreaterThan | Kind::Gt => ExpressionKind::GreaterThan(left, right),
            Kind::GreaterThanOrEqual | Kind::Ge => ExpressionKind::GreaterThanOrEqual(left, right),
            Kind::Begins => ExpressionKind::Begins(left, right),
            Kind::Matches => ExpressionKind::Matches(left, right),
            Kind::Contains => ExpressionKind::Contains(left, right),
            _ => unreachable!(),
        };
        let hi = self.prev_end();
        Ok(self.spanned_expr(lo, hi, kind))
    }

    pub fn parse_additive(&mut self) -> ParseResult<ExprId> {
        let mut links = 0;
        let mut expr = self.parse_multiplicative()?;
        loop {
            if self.check(Kind::Add) || self.check(Kind::Minus) {
                self.link(&mut links)?;
                let lo = self.exprs.get(expr).span.start;
                let op_kind = self.peek().kind;
                self.advance();
                let right_exp = self.parse_multiplicative()?;
                let hi = self.prev_end();
                let kind = match op_kind {
                    Kind::Add => ExpressionKind::Add(expr, right_exp),
                    Kind::Minus => ExpressionKind::Minus(expr, right_exp),
                    _ => unreachable!(),
                };
                expr = self.spanned_expr(lo, hi, kind);
            } else if self.check(Kind::StringLiteral) && Self::is_string_like(self.exprs.get(expr))
            {
                // Implicit string concatenation: adjacent tokens without explicit +.
                // Only fires when the LHS is already string-like (literal, preprop ref, or
                // a prior implicit concat). This prevents consuming display items or other
                // adjacent tokens as accidental concatenation.
                self.link(&mut links)?;
                let lo = self.exprs.get(expr).span.start;
                let right_exp = self.parse_multiplicative()?;
                let hi = self.prev_end();
                expr = self.spanned_expr(lo, hi, ExpressionKind::Add(expr, right_exp));
            } else {
                break;
            }
        }
        self.unlink(links);
        Ok(expr)
    }

    pub fn parse_multiplicative(&mut self) -> ParseResult<ExprId> {
        let mut links = 0;
        let mut expr = self.parse_unary()?;
        while self.check(Kind::Star) || self.check(Kind::Slash) || self.check(Kind::Modulo) {
            self.link(&mut links)?;
            let lo = self.exprs.get(expr).span.start;
            let op_kind = self.peek().kind;
            self.advance();
            let right_exp = self.parse_unary()?;
            let hi = self.prev_end();
            let kind = match op_kind {
                Kind::Star => ExpressionKind::Multiply(expr, right_exp),
                Kind::Slash => ExpressionKind::Divide(expr, right_exp),
                Kind::Modulo => ExpressionKind::Modulo(expr, right_exp),
                _ => unreachable!(),
            };
            expr = self.spanned_expr(lo, hi, kind);
        }
        self.unlink(links);
        Ok(expr)
    }

    pub fn parse_unary(&mut self) -> ParseResult<ExprId> {
        if self.check(Kind::Minus) {
            // Prefix form: `lo` is the `-` token, captured before advancing.
            let lo = self.peek().start;
            self.advance();
            let expr = self.nested(Self::parse_unary)?;
            let hi = self.prev_end();
            return Ok(self.spanned_expr(lo, hi, ExpressionKind::Negate(expr)));
        }
        if self.check(Kind::Add) {
            // Unary plus — identity operation (e.g. "- + value" means subtract unary+value)
            self.advance();
            return self.nested(Self::parse_unary);
        }
        if self.check(Kind::Not) {
            // Prefix form: `lo` is the `NOT` token, captured before advancing.
            let lo = self.peek().start;
            self.advance();
            let expr = self.nested(Self::parse_unary)?;
            let hi = self.prev_end();
            return Ok(self.spanned_expr(lo, hi, ExpressionKind::Not(expr)));
        }
        self.parse_postfix()
    }

    pub fn parse_postfix(&mut self) -> ParseResult<ExprId> {
        let mut links = 0;
        let mut expr = self.parse_primary()?;

        // Literals can't have postfix operations (member access, method calls, etc.)
        // Return early to avoid incorrectly parsing following tokens like ':' in "do i = 1 to 10:"
        // Exception: string literals may have `:U` or `:T` character-type qualifiers — consume them.
        if matches!(self.exprs.get(expr).kind, ExpressionKind::Literal(_)) {
            if self.check(Kind::Colon) {
                let colon_end = self.token_at(self.current).end;
                if let Some(next) = self.tokens.get(self.current + 1)
                    && next.start == colon_end
                    && matches!(next.kind, Kind::Identifier)
                    && (next.slice(self.source).eq_ignore_ascii_case("u")
                        || next.slice(self.source).eq_ignore_ascii_case("t"))
                {
                    // consume ':'
                    self.advance();
                    // consume 'U' or 'T'
                    self.advance();
                }
            }
            return Ok(expr);
        }

        loop {
            if self.check(Kind::Colon) || self.check(Kind::DoubleColon) {
                // Only parse as member/method access if the next token is a valid member name
                // AND is on the same line as the colon.
                // This avoids consuming ':' in block delimiters like "CASE x:" or "DO:"
                // and block-headers like "FOR EACH ... cond:".
                // DoubleColon (::) is ABL's dynamic buffer field access operator.
                let colon_end = self.token_at(self.current).end;
                let next_is_member = self.tokens.get(self.current + 1).is_some_and(|t| {
                    !self.slice(colon_end, t.start).contains('\n')
                        && self.is_member_name(self.current + 1, colon_end)
                });
                if !next_is_member {
                    break;
                }
                self.link(&mut links)?;
                expr = self.parse_member_or_method(expr)?;
            } else if self.check(Kind::LeftBracket) {
                self.link(&mut links)?;
                expr = self.parse_array_access(expr)?;
            } else if self.check(Kind::Period)
                && Self::can_have_field_access(self.exprs.get(expr))
                && self.is_field_access_ahead()
            {
                self.link(&mut links)?;
                expr = self.parse_field_access(expr)?;
            } else {
                break;
            }
        }

        self.unlink(links);
        Ok(expr)
    }

    pub fn parse_member_or_method(&mut self, object: ExprId) -> ParseResult<ExprId> {
        // Postfix left-nesting: the node spans from the object's start through
        // the closing `)`/member token (ast-invariants §5).
        let lo = self.exprs.get(object).span.start;
        let colon_end = self.advance().end; // consumes ':'

        // Expect identifier after ':'
        if !self.is_member_name(self.current, colon_end) {
            return Err(ParseError {
                message: format!(
                    "Expected identifier after ':', found {:?}",
                    self.peek().kind
                ),
                span: tok_span(self.peek()),
            });
        }

        let token = *self.advance();
        let member = Identifier {
            span: tok_span(&token),
            name: self.intern_ident(token.start, token.end),
        };

        // Check for method call: member followed by (
        if self.check(Kind::LeftParen) {
            let arguments =
                self.parse_call_arguments(false, "Expected ')' after method arguments")?;
            let hi = self.prev_end();
            return Ok(self.spanned_expr(
                lo,
                hi,
                ExpressionKind::MethodCall {
                    object,
                    method: member,
                    arguments,
                },
            ));
        }

        let hi = self.prev_end();
        Ok(self.spanned_expr(lo, hi, ExpressionKind::MemberAccess { object, member }))
    }

    /// Parse a parenthesized argument list for a method or function call.
    fn parse_call_arguments(
        &mut self,
        allow_bare_direction_sep: bool,
        close_error: &str,
    ) -> ParseResult<ExprList> {
        self.advance(); // consume '('
        let mut arguments: SmallVec<[ExprId; 8]> = SmallVec::new();
        // Empty argument list
        if !self.check(Kind::RightParen) {
            // parse first argument
            // consume and store all arguments
            arguments.push(self.parse_call_argument()?);
            // parse remaining — separated by ',' or, when a Preprop reference like {&args}
            // expands to comma-separated arguments at compile time, by a bare direction
            // qualifier (INPUT/OUTPUT/INPUT-OUTPUT) with no preceding comma.
            while self.check(Kind::Comma)
                || (allow_bare_direction_sep
                    && matches!(
                        self.peek().kind,
                        Kind::Input | Kind::Output | Kind::InputOutput
                    ))
            {
                if self.check(Kind::Comma) {
                    // consume ',' only when present
                    self.advance();
                }
                arguments.push(self.parse_optional_call_argument()?);
            }
        }
        // if after parsing all arguments we don't find the
        // closing ), throw error
        self.expect_kind(Kind::RightParen, close_error)?;
        Ok(self.push_expr_list(&arguments))
    }

    fn parse_optional_call_argument(&mut self) -> ParseResult<ExprId> {
        // Skip optional direction qualifier per argument
        self.skip_call_arg_prefixes();
        // Empty argument (successive commas like func(1,, "x")) — use Unknown literal
        // Empty argument (successive commas like obj:method(1,, "x")) — Unknown
        if self.check(Kind::Comma) || self.check(Kind::RightParen) {
            let pos = self.peek().start;
            return Ok(self.spanned_expr(
                pos,
                pos,
                ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
                    span: Span {
                        start: pos,
                        end: pos,
                    },
                })),
            ));
        }
        let argument = self.parse_expression()?;
        self.skip_call_arg_suffixes();
        Ok(argument)
    }

    fn skip_call_arg_prefixes(&mut self) {
        // Skip optional direction qualifier (INPUT / OUTPUT / INPUT-OUTPUT)
        if matches!(
            self.peek().kind,
            Kind::Input | Kind::Output | Kind::InputOutput
        ) {
            self.advance();
        }
        // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE/DATA-SOURCE for handle args
        if matches!(
            self.peek().kind,
            Kind::Table
                | Kind::TableHandle
                | Kind::Dataset
                | Kind::DatasetHandle
                | Kind::DataSource
        ) {
            self.advance();
        }
    }

    fn skip_call_arg_suffixes(&mut self) {
        // Consume optional IN FRAME/BROWSE qualifier (field:handle IN FRAME f)
        if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse) {
            // consume IN
            self.advance();
            // consume FRAME or BROWSE
            self.advance();
            self.consume_widget_name();
        }
        // Consume optional passing modifiers (BIND, BY-VALUE, BY-REFERENCE, APPEND)
        while matches!(
            self.peek().kind,
            Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
        ) {
            self.advance();
        }
    }

    pub fn parse_array_access(&mut self, array: ExprId) -> ParseResult<ExprId> {
        let lo = self.exprs.get(array).span.start;
        self.advance(); // consume '['

        let index = self.parse_expression()?;

        // Optional ABL extent range syntax: [start FOR count]
        if self.check(Kind::KwFor) {
            self.advance();
            self.parse_expression_dropped();
        }

        self.expect_kind(Kind::RightBracket, "Expected ']' after array index")?;

        let hi = self.prev_end();
        Ok(self.spanned_expr(lo, hi, ExpressionKind::ArrayAccess { array, index }))
    }

    /// Check if we're looking at field access, rather than a statement terminator.
    pub fn is_field_access_ahead(&mut self) -> bool {
        // skip if it's not a period
        if !self.check(Kind::Period) {
            return false;
        }

        // Only treat as field access if an identifier (or keyword used as identifier) immediately
        // follows the period with no whitespace. A period followed by whitespace (space, newline)
        // before an identifier is a statement terminator, not a field access separator.
        // This prevents "p. var" (space after period) from being mis-parsed as "p.var".
        // Any word-like token qualifies, so a keyword segment such as `pkg.Color` is a name.
        let period_end = self.token_at(self.current).end;
        self.tokens.get(self.current + 1).is_some_and(|t| {
            (Self::can_be_identifier(t.kind) || Self::is_word_kind(t.kind)) && t.start == period_end
        })
    }

    /// Check if an expression is string-like for implicit concatenation purposes.
    /// Implicit concat (adjacent tokens without +) only fires when the LHS is
    /// already a string value, preventing accidental consumption of display items.
    fn is_string_like(expr: &Expression) -> bool {
        matches!(
            &expr.kind,
            ExpressionKind::Literal(oxabl_ast::Literal::String(_))
                | ExpressionKind::PreprocReference(_)
                | ExpressionKind::Add(..)
        )
    }

    /// Check if an expression can be the base of field access (Table.Field)
    fn can_have_field_access(expr: &Expression) -> bool {
        matches!(
            &expr.kind,
            ExpressionKind::Identifier(_)
                | ExpressionKind::FieldAccess { .. }
                | ExpressionKind::PreprocReference(_)
                | ExpressionKind::IncludeArgReference { .. }
        )
    }

    pub fn parse_field_access(&mut self, qualifier: ExprId) -> ParseResult<ExprId> {
        let lo = self.exprs.get(qualifier).span.start;
        self.advance(); // consume '.'

        // Expect identifier (or keyword used as field name) after '.'
        if !Self::can_be_identifier(self.peek().kind) && !Self::is_word_kind(self.peek().kind) {
            return Err(ParseError {
                message: "Expected field name after '.'".to_string(),
                span: tok_span(self.peek()),
            });
        }

        let token = *self.advance();
        let field = Identifier {
            span: tok_span(&token),
            name: self.intern_ident(token.start, token.end),
        };

        let hi = self.prev_end();
        Ok(self.spanned_expr(lo, hi, ExpressionKind::FieldAccess { qualifier, field }))
    }

    /// Parse a primary expression and stamp its full-extent span.
    ///
    /// Thin funnel over `parse_primary_inner`:
    /// `lo` is the first token examined, `hi` is the end of the last non-comment
    /// token consumed. Parenthesized groups therefore include the enclosing
    /// parens (U3), and every leaf/primary form is stamped uniformly regardless
    /// of which branch produced it.
    pub fn parse_primary(&mut self) -> ParseResult<ExprId> {
        let lo = self.peek().start;
        let expr = self.parse_primary_inner()?;
        let hi = self.prev_end().max(lo);
        self.exprs.get_mut(expr).span = Span { start: lo, end: hi };
        Ok(expr)
    }

    fn parse_primary_inner(&mut self) -> ParseResult<ExprId> {
        match self.peek().kind {
            // Inline ternary: IF expr THEN expr ELSE expr — valid in any expression position
            Kind::KwIf => self.nested(Self::parse_ternary),
            // Implicit widget attribute/method: :attribute or :method(args)
            // In ABL, `:name` without a preceding expression accesses an attribute or
            // method on the currently focused widget (set by CHOOSE/UPDATE/SET).
            // Parse as an identifier and let postfix parsing handle method calls.
            Kind::Colon => Ok(self.parse_implicit_widget_member()),
            // Bare field access: .fieldname — field of the default/implicit buffer.
            // In ABL, `.fieldname` without a preceding buffer name accesses a field on the
            // default buffer in scope (e.g., inside a FOR EACH block).
            Kind::Period if Self::can_be_identifier(self.peek_at(1).kind) => {
                Ok(self.parse_implicit_buffer_field())
            }
            // Preprocessor reference: {&variable} or compound {&prefix}suffix{&more}...
            // e.g. {&pre}order{&ext} — multiple adjacent parts form a single identifier.
            // Requires direct adjacency (no whitespace) between each part.
            Kind::Preprop => self.parse_preprop_primary(),
            // Mid-expression preprocessor conditional: &IF cond &THEN expr &ELSE expr &ENDIF
            Kind::PreprocIf => self.parse_preproc_if_expr(),
            // Parenthesized expression
            Kind::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect_kind(Kind::RightParen, "Expected ')' after expression")?;
                Ok(expr)
            }
            // Literals
            Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::DateLiteral
            | Kind::StringLiteral
            | Kind::KwTrue
            | Kind::KwFalse
            | Kind::Yes
            | Kind::No
            | Kind::Question => self.parse_literal_primary(),
            // Include file reference in expression position: {file.i}
            Kind::IncludeReference => Ok(self.parse_include_ref_primary()),
            // Include positional argument reference in expression position: {1}
            Kind::IncludeArgReference => Ok(self.parse_include_arg_primary()),
            // NEW ClassName(args) — object instantiation.
            // Class names may be dotted: NEW app.Widget(company, user-ref).
            Kind::New => self.parse_new_primary(),
            // DYNAMIC-NEW expr([args]) — dynamic object instantiation where the class name
            // is a runtime expression (variable or string), not a static identifier.
            // Modelled like DYNAMIC-FUNCTION: the class expression comes first, then the
            // constructor arguments.
            Kind::DynamicNew => self.parse_dynamic_new_primary(),
            // DYNAMIC-FUNCTION("func-name" [IN handle] [, arg1, arg2, ...])
            Kind::DynamicFunction => self.parse_dynamic_function_primary(),
            // CAN-FIND([FIRST|LAST] table [WHERE expr] [lock] [NO-ERROR])
            // Boolean expression that checks if a record matching the phrase exists.
            Kind::CanFind => self.parse_can_find_primary(),
            // LOCKED [table] / LOCKED(table) — checks if a record is exclusively locked.
            // ABL allows both parenthesized and bare forms.
            Kind::Locked => self.parse_locked_primary(),
            // AVAILABLE [table] / AVAILABLE(table) — built-in record-availability predicate.
            // ABL allows both parenthesized and bare forms, e.g.:
            //   IF AVAILABLE order THEN ...
            //   IF AVAILABLE(order) THEN ...
            // CURRENT-CHANGED takes its record the same way.
            Kind::Available | Kind::Ambiguous | Kind::CurrentChanged => {
                self.parse_record_predicate_primary()
            }
            // TEMP-TABLE name[:attr] / BUFFER name[:attr] / FRAME name[:attr] / BROWSE name[:attr]
            // — handle/object reference expressions.
            // When these keywords appear in expression context followed by an identifier,
            // consume both tokens as a compound identifier (e.g. BROWSE b-area-p:first-column).
            // Also handles BUFFER {&preproc} for dynamic buffer references.
            Kind::TempTable
            | Kind::Buffer
            | Kind::Frame
            | Kind::Query
            | Kind::Dataset
            | Kind::Stream
            | Kind::Browse
                if Self::can_be_identifier(self.peek_at(1).kind)
                    || self.check_at(1, Kind::Preprop) =>
            {
                Ok(self.parse_handle_prefix_primary())
            }
            // ACCUM has two forms:
            //   Form 1: ACCUM aggregate-type [BY break-field] field  (e.g. ACCUM TOTAL amt-total)
            //   Form 2: ACCUM field (aggregate-type [BY break-field]) (e.g. ACCUM qty (TOTAL BY item))
            // When not followed by '(', parse the first operand via parse_postfix so that qualified
            // fields like table.field are consumed whole.  A trailing '(' signals form 2.
            Kind::Accum => self.parse_accum_primary(),
            // INPUT / OUTPUT as transparent parameter-direction qualifier in expression context.
            // e.g. WHERE field EQ INPUT param-name — consume INPUT/OUTPUT/INPUT-OUTPUT and
            // then parse the actual operand.
            Kind::Input | Kind::Output | Kind::InputOutput => {
                // consume the direction qualifier
                self.advance();
                self.nested(Self::parse_primary)
            }
            // Identifiers and callable keywords (built-in functions like NOW, TRIM, etc.)
            kind if Self::can_be_identifier(kind) => self.parse_identifier_or_call_primary(),
            kind => Err(ParseError {
                message: format!("Unexpected token {kind:?}"),
                span: tok_span(self.peek()),
            }),
        }
    }

    fn parse_implicit_widget_member(&mut self) -> ExprId {
        // consume ':'
        let colon_token = *self.advance();
        if Self::can_be_identifier(self.peek().kind) {
            let attr_token = *self.advance();
            let prefixed = format!(":{}", attr_token.slice(self.source));
            let ident = Identifier {
                span: Span {
                    start: colon_token.start,
                    end: attr_token.end,
                },
                name: self.intern(&prefixed),
            };
            return self.expr(ExpressionKind::Identifier(ident));
        }
        // Bare colon with no identifier — return as unknown
        self.expr(ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
            span: tok_span(&colon_token),
        })))
    }

    fn parse_implicit_buffer_field(&mut self) -> ExprId {
        // consume '.'
        let period_token = *self.advance();
        let field_token = *self.advance();
        let prefixed = format!(".{}", field_token.slice(self.source));
        let ident = Identifier {
            span: Span {
                start: period_token.start,
                end: field_token.end,
            },
            name: self.intern(&prefixed),
        };
        self.expr(ExpressionKind::Identifier(ident))
    }

    /// Extend a compound identifier end offset across directly-adjacent
    /// preprocessor/identifier/hyphen-word segments.
    fn extend_adjacent_compound_end(&mut self, mut end: u32, allow_include_arg: bool) -> u32 {
        // Extend with directly-adjacent parts (identifiers, preprops, or
        // hyphen-separated word suffixes like `{&preproc}-control`).
        loop {
            let next = self.token_at(self.current);
            if next.start != end {
                break;
            }
            if next.kind == Kind::Preprop
                || Self::can_be_identifier(next.kind)
                || (allow_include_arg && next.kind == Kind::IncludeArgReference)
            {
                end = self.advance().end;
            } else if next.kind == Kind::Minus {
                // Adjacent hyphen: consume only if a word-like token follows directly.
                let minus_end = next.end;
                let after_ok = self
                    .tokens
                    .get(self.current + 1)
                    .is_some_and(|a| a.start == minus_end && Self::is_word_kind(a.kind));
                if after_ok {
                    // consume '-'
                    self.advance();
                    // consume word
                    end = self.advance().end;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        end
    }

    fn parse_preprop_primary(&mut self) -> ParseResult<ExprId> {
        let first = *self.advance();
        let compound_end = self.extend_adjacent_compound_end(first.end, false);
        // Bare {&name} with no adjacent parts → PreprocReference
        if compound_end == first.end {
            let raw = first.slice(self.source);
            let stripped = raw
                .strip_prefix("{&")
                .and_then(|s| s.strip_suffix('}'))
                .unwrap_or(raw);
            let name = self.intern(stripped);
            return Ok(self.expr(ExpressionKind::PreprocReference(name)));
        }
        // Compound → Identifier using raw source text
        let identifier = Identifier {
            span: Span {
                start: first.start,
                end: compound_end,
            },
            name: self.intern(self.slice(first.start, compound_end)),
        };
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(identifier);
        }
        Ok(self.expr(ExpressionKind::Identifier(identifier)))
    }

    fn parse_preproc_if_expr(&mut self) -> ParseResult<ExprId> {
        self.advance(); // consume &IF
        let preproc = self.parse_preproc_if(1, &Self::parse_expression)?;
        if preproc.else_branch.is_none() {
            return Err(ParseError {
                message: "Expression-level &IF requires &ELSE branch".to_string(),
                span: self.current_span(),
            });
        }
        let id = self.exprs.push_preproc_if(preproc);
        Ok(self.expr(ExpressionKind::PreprocIf(id)))
    }

    fn parse_literal_primary(&mut self) -> ParseResult<ExprId> {
        let source = self.source;
        let token = *self.advance();
        let literal =
            token_to_literal(&token, source, &mut self.names).ok_or_else(|| ParseError {
                message: "Failed to convert token to literal".to_string(),
                span: tok_span(&token),
            })?;
        Ok(self.expr(ExpressionKind::Literal(literal)))
    }

    fn parse_include_ref_primary(&mut self) -> ExprId {
        let token = *self.advance();
        let text = match token.text(self.source) {
            Some(text) => text,
            None => self.slice(token.start + 1, token.end - 1).trim(),
        };
        let path_and_args = self.intern(text);
        self.expr(ExpressionKind::IncludeReference {
            path_and_args,
            span: tok_span(&token),
        })
    }

    fn parse_include_arg_primary(&mut self) -> ExprId {
        let token = *self.advance();
        let index = i64::from(token.include_arg_index(self.source).unwrap_or(0));
        self.expr(ExpressionKind::IncludeArgReference {
            index,
            span: tok_span(&token),
        })
    }

    fn parse_new_primary(&mut self) -> ParseResult<ExprId> {
        // consume NEW
        let new_token = *self.advance();
        // `NEW(buffer)` is the record-buffer test; a class name never starts with '('.
        if self.check(Kind::LeftParen) {
            let name = Identifier {
                span: tok_span(&new_token),
                name: self.intern(new_token.slice(self.source)),
            };
            return self.parse_function_call(name);
        }
        // Parse dotted class name (e.g., "app.Widget", "api.text", "Progress.Lang.Error").
        // Use is_word_kind() so keyword-named namespaces/types (e.g. api.text) are accepted.
        if !Self::is_word_kind(self.peek().kind) {
            return Err(ParseError {
                message: "Expected class name after NEW".to_string(),
                span: self.current_span(),
            });
        }
        let start = self.peek().start;
        // consume first name component
        self.advance();
        // A qualified name has no space around its periods: in
        // `l = NEW customer.` the period ends the statement.
        while self.check(Kind::Period)
            && self.token_at(self.current - 1).end == self.peek().start
            && self
                .tokens
                .get(self.current + 1)
                .is_some_and(|t| Self::is_word_kind(t.kind) && t.start == self.peek().end)
        {
            // consume '.'
            self.advance();
            // consume next component
            self.advance();
        }
        let end = self.token_at(self.current - 1).end;
        let class_name = self.intern(self.slice(start, end));

        // Optional generic type args — `NEW List<String>(...)`.
        let type_arguments = self.parse_generic_type_args();
        let type_arguments =
            (!type_arguments.0.is_empty()).then(|| self.exprs.push_type_args(type_arguments));

        // If no '(' follows, this is the logical NEW record-name test (boolean expression),
        // not an OO object constructor. Model it as `NEW(record)`, the shape
        // `AVAILABLE record` has.
        if !self.check(Kind::LeftParen) {
            let (lo, hi) = (start, end);
            let record = self.spanned_expr(
                lo,
                hi,
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: lo, end: hi },
                    name: class_name,
                }),
            );
            let name = Identifier {
                span: tok_span(&new_token),
                name: self.intern(new_token.slice(self.source)),
            };
            let arguments = self.push_expr_list(&[record]);
            return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
        }

        // Parse argument list (supports INPUT/OUTPUT direction qualifiers and TABLE/BUFFER args)
        let arguments = self.parse_new_arguments()?;
        Ok(self.expr(ExpressionKind::New {
            class_name,
            type_arguments,
            arguments,
        }))
    }

    fn parse_new_arguments(&mut self) -> ParseResult<ExprList> {
        self.expect_kind(Kind::LeftParen, "Expected '(' after class name in NEW")?;
        let mut arguments: SmallVec<[ExprId; 8]> = SmallVec::new();
        if !self.check(Kind::RightParen) {
            loop {
                // Skip optional direction qualifier
                if matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput
                ) {
                    self.advance();
                }
                // Handle TABLE/BUFFER/DATASET handle-type arguments (no expression value)
                if matches!(
                    self.peek().kind,
                    Kind::Table
                        | Kind::Buffer
                        | Kind::Dataset
                        | Kind::TableHandle
                        | Kind::DatasetHandle
                ) {
                    self.skip_new_handle_arg();
                } else {
                    arguments.push(self.parse_expression()?);
                }
                if !self.check(Kind::Comma) {
                    break;
                }
                // consume comma
                self.advance();
            }
        }
        self.expect_kind(Kind::RightParen, "Expected ')' after NEW arguments")?;
        Ok(self.push_expr_list(&arguments))
    }

    fn skip_new_handle_arg(&mut self) {
        // consume TABLE/BUFFER/DATASET etc.
        self.advance();
        // Optional FOR keyword
        if self.check(Kind::KwFor) {
            self.advance();
        }
        // Consume the name
        if Self::can_be_identifier(self.peek().kind) {
            self.advance();
        }
        // Consume optional BIND/APPEND/BY-VALUE
        while matches!(
            self.peek().kind,
            Kind::Bind | Kind::Append | Kind::ByValue | Kind::ByReference
        ) {
            self.advance();
        }
    }

    fn parse_dynamic_new_primary(&mut self) -> ParseResult<ExprId> {
        let token = *self.advance();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(token.slice(self.source)),
        };
        // `DYNAMIC-NEW cls (args)`: a name before `(` is the class operand,
        // not a function call, so it is taken as a plain identifier.
        let class =
            if Self::can_be_identifier(self.peek().kind) && self.check_at(1, Kind::LeftParen) {
                let id = self.parse_identifier()?;
                self.expr(ExpressionKind::Identifier(id))
            } else {
                self.nested(Self::parse_primary)?
            };
        let mut arguments: SmallVec<[ExprId; 8]> = SmallVec::new();
        arguments.push(class);
        // Parse optional argument list
        if self.check(Kind::LeftParen) {
            self.advance();
            if !self.check(Kind::RightParen) {
                loop {
                    arguments.push(self.parse_call_argument()?);
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance();
                }
            }
            self.expect_kind(Kind::RightParen, "Expected ')' after DYNAMIC-NEW arguments")?;
        }
        let arguments = self.push_expr_list(&arguments);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }

    fn parse_dynamic_function_primary(&mut self) -> ParseResult<ExprId> {
        let token = *self.advance();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(token.slice(self.source)),
        };
        self.expect_kind(Kind::LeftParen, "Expected '(' after DYNAMIC-FUNCTION")?;
        let mut arguments: SmallVec<[ExprId; 8]> = SmallVec::new();
        if !self.check(Kind::RightParen) {
            // First arg is the function name string
            arguments.push(self.parse_expression()?);
            // Optional IN handle clause
            if self.check(Kind::KwIn) {
                self.advance();
                arguments.push(self.parse_expression()?);
            }
            // Additional comma-separated arguments, with parameter modes
            while self.check(Kind::Comma) {
                self.advance();
                arguments.push(self.parse_call_argument()?);
            }
        }
        self.expect_kind(Kind::RightParen, "Expected ')' after DYNAMIC-FUNCTION")?;
        let arguments = self.push_expr_list(&arguments);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }

    fn parse_can_find_primary(&mut self) -> ParseResult<ExprId> {
        self.advance(); // consume CAN-FIND
        self.expect_kind(Kind::LeftParen, "Expected '(' after CAN-FIND")?;

        // Optional FIRST/LAST/CURRENT qualifier
        let find_type = match self.peek().kind {
            Kind::First => {
                self.advance();
                FindType::First
            }
            Kind::Last => {
                self.advance();
                FindType::Last
            }
            Kind::Current => {
                self.advance();
                FindType::Current
            }
            _ => FindType::Unique,
        };

        // Table/buffer name
        let buffer = self.parse_identifier()?;

        // Optional OF table — related-record syntax: CAN-FIND(child OF parent)
        if self.check(Kind::Of) {
            // consume OF
            self.advance();
            let lo = self.current;
            // consume parent table name
            self.parse_identifier().ok();
            self.note_dropped(lo);
        }
        let (lock_type, where_clause, no_error) = self.parse_can_find_options()?;
        self.expect_kind(
            Kind::RightParen,
            "Expected ')' after CAN-FIND record phrase",
        )?;
        Ok(self.expr(ExpressionKind::CanFind {
            find_type,
            buffer,
            where_clause,
            lock_type,
            no_error,
        }))
    }

    fn parse_can_find_options(
        &mut self,
    ) -> ParseResult<(oxabl_ast::LockType, Option<ExprId>, bool)> {
        // The record-phrase options may come in any order; the first explicit
        // lock type wins and WHERE may appear once.
        let mut lock_type = None;
        let mut where_clause = None;
        let mut no_error = false;
        loop {
            match self.peek().kind {
                Kind::NoLock | Kind::ShareLock | Kind::ExclusiveLock | Kind::Preprop => {
                    let parsed = self.parse_lock_type();
                    lock_type.get_or_insert(parsed);
                }
                Kind::KwWhere if where_clause.is_none() => {
                    self.advance();
                    where_clause = Some(self.parse_expression()?);
                }
                Kind::UseIndex => {
                    self.advance();
                    if Self::can_be_identifier(self.peek().kind) {
                        // consume index name
                        self.advance();
                    }
                }
                // USING field [AND field]...
                Kind::Using => {
                    self.advance();
                    self.nested(Self::parse_postfix)?;
                    while self.check(Kind::And) {
                        self.advance();
                        self.nested(Self::parse_postfix)?;
                    }
                }
                Kind::NoWait => {
                    self.advance();
                }
                Kind::NoError => {
                    self.advance();
                    no_error = true;
                }
                _ => break,
            }
        }
        Ok((
            lock_type.unwrap_or(oxabl_ast::LockType::ShareLock),
            where_clause,
            no_error,
        ))
    }

    fn parse_locked_primary(&mut self) -> ParseResult<ExprId> {
        let token = *self.advance();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(token.slice(self.source)),
        };
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(name);
        }
        // Bare form: LOCKED table — parse the table name as the sole argument
        let arg_token = *self.advance();
        let arg_span = tok_span(&arg_token);
        let arg_name = Identifier {
            span: arg_span,
            name: self.intern(arg_token.slice(self.source)),
        };
        let arg_expr = self.spanned_expr(
            arg_span.start,
            arg_span.end,
            ExpressionKind::Identifier(arg_name),
        );
        let arguments = self.push_expr_list(&[arg_expr]);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }

    fn parse_record_predicate_primary(&mut self) -> ParseResult<ExprId> {
        let token = *self.advance();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(token.slice(self.source)),
        };
        // Parenthesized form: AVAILABLE(table) / AMBIGUOUS(table)
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(name);
        }
        // Bare form: AVAILABLE table / AMBIGUOUS table
        // Use parse_postfix to handle database-qualified names like dictdb._file
        // Guard: if the next token cannot be a buffer/table name, treat as bare
        // AVAILABLE with no argument (bad practice but valid ABL — evaluates the
        // default buffer in scope).
        if !Self::can_be_identifier(self.peek().kind) && !self.check(Kind::LeftParen) {
            return Ok(self.expr(ExpressionKind::FunctionCall {
                name,
                arguments: ExprList::EMPTY,
            }));
        }
        let arg = self.nested(Self::parse_postfix)?;
        let arguments = self.push_expr_list(&[arg]);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }

    fn parse_handle_prefix_primary(&mut self) -> ExprId {
        // consume TEMP-TABLE or BUFFER
        let _kw_token = self.advance();
        // consume the table/buffer name
        let name_token = *self.advance();

        // Extend with directly-adjacent Preprop or identifier parts so that
        // compound names like `dataset ds{&mainTable}` or `buffer b-{&tbl}`
        // are consumed whole. Without this, the adjacent preprop is left as
        // the next token and the postfix `:member` access is never parsed.
        let end = self.extend_adjacent_compound_end(name_token.end, true);

        // Use only the handle *name* (qh), not the keyword prefix (QUERY).
        // `QUERY qh:ATTR` is handle-qualified access; resolving "QUERY qh"
        // as a single atom always fails LINT0001 (#58 item C).
        let identifier = Identifier {
            span: Span {
                start: name_token.start,
                end,
            },
            name: self.intern(self.slice(name_token.start, end)),
        };
        self.expr(ExpressionKind::Identifier(identifier))
    }

    fn parse_accum_primary(&mut self) -> ParseResult<ExprId> {
        let token = *self.advance();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(token.slice(self.source)),
        };
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(name);
        }
        let mut arguments: SmallVec<[ExprId; 4]> = SmallVec::new();
        // Form 1 aggregate-type word (TOTAL, COUNT, ...): a lone word followed by BY or
        // by the field. It is a phrase keyword, not an expression, so it is consumed
        // without becoming an argument.
        if Self::can_be_identifier(self.peek().kind)
            && (self.peek_at(1).kind == Kind::By || Self::can_be_identifier(self.peek_at(1).kind))
        {
            self.advance();
        } else if Self::can_be_identifier(self.peek().kind)
            && let Ok(first_expr) = self.nested(Self::parse_postfix)
        {
            arguments.push(first_expr);
        }
        // Form 2: the first operand was the field; the parenthesised spec follows.
        // Consume '(' ... ')' and return.
        if self.check(Kind::LeftParen) {
            self.skip_balanced_parens();
            let arguments = self.push_expr_list(&arguments);
            return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
        }
        // Form 1: first operand was the aggregate-type.
        // optional BY break-field [label] (may be qualified: table.field)
        if self.check(Kind::By) {
            self.advance();
            if Self::can_be_identifier(self.peek().kind) {
                let lo = self.current;
                self.nested(Self::parse_postfix).ok();
                self.note_dropped(lo);
            }
            // Optional label (integer or string) identifying the break level
            if matches!(self.peek().kind, Kind::IntegerLiteral | Kind::StringLiteral) {
                self.advance();
            }
        }
        // field name — may be an identifier, qualified field, indexed access, or
        // parenthesized expression like (expr1 - expr2)
        if (Self::can_be_identifier(self.peek().kind) || self.check(Kind::LeftParen))
            && let Ok(field_expr) = self.nested(Self::parse_postfix)
        {
            arguments.push(field_expr);
        }
        let arguments = self.push_expr_list(&arguments);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }

    fn skip_balanced_parens(&mut self) {
        // consume '('
        self.advance();
        let mut depth = 1usize;
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
        if self.check(Kind::RightParen) {
            self.advance();
        }
    }

    fn parse_identifier_or_call_primary(&mut self) -> ParseResult<ExprId> {
        let token = *self.advance();

        // identifier{&suffix} compound: e.g. b-{&line-buffer} (direct adjacency only).
        // Also handles adjacent hyphen-word: e.g. b-{&preproc}-suffix.
        let end = self.extend_adjacent_compound_end(token.end, false);
        let identifier = Identifier {
            span: Span {
                start: token.start,
                end,
            },
            name: self.intern_ident(token.start, end),
        };

        // Check for function call: identifier/callable followed by '('
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(identifier);
        }

        Ok(self.expr(ExpressionKind::Identifier(identifier)))
    }

    /// One call argument: `[INPUT|OUTPUT|INPUT-OUTPUT] [TABLE|TABLE-HANDLE|DATASET|
    /// DATASET-HANDLE|DATA-SOURCE] expr [IN FRAME|BROWSE name] [BIND|BY-VALUE|
    /// BY-REFERENCE|APPEND]...`. Only the operand is kept, like the other call forms.
    fn parse_call_argument(&mut self) -> ParseResult<ExprId> {
        self.skip_call_arg_prefixes();
        let argument = self.parse_expression()?;
        self.skip_call_arg_suffixes();
        Ok(argument)
    }

    pub fn parse_function_call(&mut self, name: Identifier) -> ParseResult<ExprId> {
        let arguments = self.parse_call_arguments(true, "Expected ')' after function arguments")?;
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }
}
