//! Expression parsing for the Oxabl parser.
//!
//! Precedence levels (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative
//! > unary > postfix (member access, method calls, array/field access) > primary.

use oxabl_ast::{
    ExprId, ExprList, Expression, ExpressionKind, FindType, Identifier, Literal, Span,
    UnknownLiteral,
};
use oxabl_lexer::{Kind, TokenValue};
use smallvec::SmallVec;

use super::{ParseError, ParseResult, Parser, span_u32, tok_span};
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
        let lo = span_u32(self.peek().start);
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
            let lo = span_u32(self.peek().start);
            self.advance();
            let expr = self.nested(Self::parse_unary)?;
            let hi = self.prev_end();
            return Ok(self.spanned_expr(lo, hi, ExpressionKind::Negate(expr)));
        }
        if self.check(Kind::Add) {
            self.advance();
            return self.nested(Self::parse_unary);
        }
        if self.check(Kind::Not) {
            let lo = span_u32(self.peek().start);
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

        if matches!(self.exprs.get(expr).kind, ExpressionKind::Literal(_)) {
            if self.check(Kind::Colon) {
                let colon_end = self.token_at(self.current).end;
                if let Some(next) = self.tokens.get(self.current + 1)
                    && next.start == colon_end
                    && matches!(next.kind, Kind::Identifier)
                    && (self.source[next.start..next.end].eq_ignore_ascii_case("u")
                        || self.source[next.start..next.end].eq_ignore_ascii_case("t"))
                {
                    self.advance();
                    self.advance();
                }
            }
            return Ok(expr);
        }

        loop {
            if self.check(Kind::Colon) || self.check(Kind::DoubleColon) {
                let colon_end = self.token_at(self.current).end;
                let next_is_member = self.tokens.get(self.current + 1).is_some_and(|t| {
                    !self.source[colon_end..t.start].contains('\n')
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
        let lo = self.exprs.get(object).span.start;
        let colon_end = self.advance().end; // consumes ':'

        if !self.is_member_name(self.current, colon_end) {
            return Err(ParseError {
                message: format!(
                    "Expected identifier after ':', found {:?}",
                    self.peek().kind
                ),
                span: tok_span(self.peek()),
            });
        }

        let token = self.advance().clone();
        let member = Identifier {
            span: tok_span(&token),
            name: self.intern_ident(token.start, token.end),
        };

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
        if !self.check(Kind::RightParen) {
            arguments.push(self.parse_call_argument()?);
            while self.check(Kind::Comma)
                || (allow_bare_direction_sep
                    && matches!(
                        self.peek().kind,
                        Kind::Input | Kind::Output | Kind::InputOutput
                    ))
            {
                if self.check(Kind::Comma) {
                    self.advance();
                }
                arguments.push(self.parse_optional_call_argument()?);
            }
        }
        self.expect_kind(Kind::RightParen, close_error)?;
        Ok(self.push_expr_list(&arguments))
    }

    fn parse_optional_call_argument(&mut self) -> ParseResult<ExprId> {
        self.skip_call_arg_prefixes();
        if self.check(Kind::Comma) || self.check(Kind::RightParen) {
            let pos = span_u32(self.peek().start);
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
        if matches!(
            self.peek().kind,
            Kind::Input | Kind::Output | Kind::InputOutput
        ) {
            self.advance();
        }
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
        if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse) {
            self.advance();
            self.advance();
            self.consume_widget_name();
        }
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
        if !self.check(Kind::Period) {
            return false;
        }
        let period_end = self.token_at(self.current).end;
        self.tokens.get(self.current + 1).is_some_and(|t| {
            (Self::can_be_identifier(t.kind) || Self::is_word_kind(t.kind)) && t.start == period_end
        })
    }

    fn is_string_like(expr: &Expression) -> bool {
        matches!(
            &expr.kind,
            ExpressionKind::Literal(oxabl_ast::Literal::String(_))
                | ExpressionKind::PreprocReference(_)
                | ExpressionKind::Add(..)
        )
    }

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

        if !Self::can_be_identifier(self.peek().kind) && !Self::is_word_kind(self.peek().kind) {
            return Err(ParseError {
                message: "Expected field name after '.'".to_string(),
                span: tok_span(self.peek()),
            });
        }

        let token = self.advance().clone();
        let field = Identifier {
            span: tok_span(&token),
            name: self.intern_ident(token.start, token.end),
        };

        let hi = self.prev_end();
        Ok(self.spanned_expr(lo, hi, ExpressionKind::FieldAccess { qualifier, field }))
    }

    /// Parse a primary expression and stamp its full-extent span.
    pub fn parse_primary(&mut self) -> ParseResult<ExprId> {
        let lo = span_u32(self.peek().start);
        let expr = self.parse_primary_inner()?;
        let hi = self.prev_end().max(lo);
        self.exprs.get_mut(expr).span = Span { start: lo, end: hi };
        Ok(expr)
    }

    fn parse_primary_inner(&mut self) -> ParseResult<ExprId> {
        match self.peek().kind {
            Kind::KwIf => self.nested(Self::parse_ternary),
            Kind::Colon => Ok(self.parse_implicit_widget_member()),
            Kind::Period if Self::can_be_identifier(self.peek_at(1).kind) => {
                Ok(self.parse_implicit_buffer_field())
            }
            Kind::Preprop => self.parse_preprop_primary(),
            Kind::PreprocIf => self.parse_preproc_if_expr(),
            Kind::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect_kind(Kind::RightParen, "Expected ')' after expression")?;
                Ok(expr)
            }
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
            Kind::IncludeReference => Ok(self.parse_include_ref_primary()),
            Kind::IncludeArgReference => Ok(self.parse_include_arg_primary()),
            Kind::New => self.parse_new_primary(),
            Kind::DynamicNew => self.parse_dynamic_new_primary(),
            Kind::DynamicFunction => self.parse_dynamic_function_primary(),
            Kind::CanFind => self.parse_can_find_primary(),
            Kind::Locked => self.parse_locked_primary(),
            Kind::Available | Kind::Ambiguous | Kind::CurrentChanged => {
                self.parse_record_predicate_primary()
            }
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
            Kind::Accum => self.parse_accum_primary(),
            Kind::Input | Kind::Output | Kind::InputOutput => {
                self.advance();
                self.nested(Self::parse_primary)
            }
            kind if Self::can_be_identifier(kind) => self.parse_identifier_or_call_primary(),
            kind => Err(ParseError {
                message: format!("Unexpected token {kind:?}"),
                span: tok_span(self.peek()),
            }),
        }
    }

    fn parse_implicit_widget_member(&mut self) -> ExprId {
        let colon_token = self.advance().clone();
        if Self::can_be_identifier(self.peek().kind) {
            let attr_token = self.advance().clone();
            let prefixed = format!(":{}", &self.source[attr_token.start..attr_token.end]);
            let ident = Identifier {
                span: Span {
                    start: span_u32(colon_token.start),
                    end: span_u32(attr_token.end),
                },
                name: self.intern(&prefixed),
            };
            return self.expr(ExpressionKind::Identifier(ident));
        }
        self.expr(ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
            span: tok_span(&colon_token),
        })))
    }

    fn parse_implicit_buffer_field(&mut self) -> ExprId {
        let period_token = self.advance().clone();
        let field_token = self.advance().clone();
        let prefixed = format!(".{}", &self.source[field_token.start..field_token.end]);
        let ident = Identifier {
            span: Span {
                start: span_u32(period_token.start),
                end: span_u32(field_token.end),
            },
            name: self.intern(&prefixed),
        };
        self.expr(ExpressionKind::Identifier(ident))
    }

    /// Extend a compound identifier end offset across directly-adjacent
    /// preprocessor/identifier/hyphen-word segments.
    fn extend_adjacent_compound_end(&mut self, mut end: usize, allow_include_arg: bool) -> usize {
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
                let minus_end = next.end;
                let after_ok = self
                    .tokens
                    .get(self.current + 1)
                    .is_some_and(|a| a.start == minus_end && Self::is_word_kind(a.kind));
                if after_ok {
                    self.advance();
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
        let first = self.advance().clone();
        let compound_end = self.extend_adjacent_compound_end(first.end, false);
        if compound_end == first.end {
            let raw = &self.source[first.start..first.end];
            let stripped = raw
                .strip_prefix("{&")
                .and_then(|s| s.strip_suffix('}'))
                .unwrap_or(raw);
            let name = self.intern(stripped);
            return Ok(self.expr(ExpressionKind::PreprocReference(name)));
        }
        let identifier = Identifier {
            span: Span {
                start: span_u32(first.start),
                end: span_u32(compound_end),
            },
            name: self.intern(&self.source[first.start..compound_end]),
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
        Ok(self.expr(ExpressionKind::PreprocIf(Box::new(preproc))))
    }

    fn parse_literal_primary(&mut self) -> ParseResult<ExprId> {
        let source = self.source;
        let token = self.advance().clone();
        let literal =
            token_to_literal(&token, source, &mut self.names).ok_or_else(|| ParseError {
                message: "Failed to convert token to literal".to_string(),
                span: tok_span(&token),
            })?;
        Ok(self.expr(ExpressionKind::Literal(literal)))
    }

    fn parse_include_ref_primary(&mut self) -> ExprId {
        let token = self.advance().clone();
        let text = match token.text(self.source) {
            Some(text) => text,
            None => self.source[token.start + 1..token.end - 1].trim(),
        };
        let path_and_args = self.intern(text);
        self.expr(ExpressionKind::IncludeReference {
            path_and_args,
            span: tok_span(&token),
        })
    }

    fn parse_include_arg_primary(&mut self) -> ExprId {
        let token = self.advance().clone();
        let index = match &token.value {
            TokenValue::Integer(i) => i64::from(*i),
            _ => 0,
        };
        self.expr(ExpressionKind::IncludeArgReference {
            index,
            span: tok_span(&token),
        })
    }

    fn parse_new_primary(&mut self) -> ParseResult<ExprId> {
        let new_token = self.advance().clone();
        if self.check(Kind::LeftParen) {
            let name = Identifier {
                span: tok_span(&new_token),
                name: self.intern(&self.source[new_token.start..new_token.end]),
            };
            return self.parse_function_call(name);
        }
        if !Self::is_word_kind(self.peek().kind) {
            return Err(ParseError {
                message: "Expected class name after NEW".to_string(),
                span: self.current_span(),
            });
        }
        let start = self.peek().start;
        self.advance();
        while self.check(Kind::Period)
            && self.token_at(self.current - 1).end == self.peek().start
            && self
                .tokens
                .get(self.current + 1)
                .is_some_and(|t| Self::is_word_kind(t.kind) && t.start == self.peek().end)
        {
            self.advance();
            self.advance();
        }
        let end = self.token_at(self.current - 1).end;
        let class_name = self.intern(&self.source[start..end]);
        let type_arguments = self.parse_generic_type_args();
        let type_arguments = (!type_arguments.0.is_empty()).then(|| Box::new(type_arguments));

        if !self.check(Kind::LeftParen) {
            let (lo, hi) = (span_u32(start), span_u32(end));
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
                name: self.intern(&self.source[new_token.start..new_token.end]),
            };
            let arguments = self.push_expr_list(&[record]);
            return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
        }

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
                if matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput
                ) {
                    self.advance();
                }
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
                self.advance();
            }
        }
        self.expect_kind(Kind::RightParen, "Expected ')' after NEW arguments")?;
        Ok(self.push_expr_list(&arguments))
    }

    fn skip_new_handle_arg(&mut self) {
        self.advance();
        if self.check(Kind::KwFor) {
            self.advance();
        }
        if Self::can_be_identifier(self.peek().kind) {
            self.advance();
        }
        while matches!(
            self.peek().kind,
            Kind::Bind | Kind::Append | Kind::ByValue | Kind::ByReference
        ) {
            self.advance();
        }
    }

    fn parse_dynamic_new_primary(&mut self) -> ParseResult<ExprId> {
        let token = self.advance().clone();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(&self.source[token.start..token.end]),
        };
        let class =
            if Self::can_be_identifier(self.peek().kind) && self.check_at(1, Kind::LeftParen) {
                let id = self.parse_identifier()?;
                self.expr(ExpressionKind::Identifier(id))
            } else {
                self.nested(Self::parse_primary)?
            };
        let mut arguments: SmallVec<[ExprId; 8]> = SmallVec::new();
        arguments.push(class);
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
        let token = self.advance().clone();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(&self.source[token.start..token.end]),
        };
        self.expect_kind(Kind::LeftParen, "Expected '(' after DYNAMIC-FUNCTION")?;
        let mut arguments: SmallVec<[ExprId; 8]> = SmallVec::new();
        if !self.check(Kind::RightParen) {
            arguments.push(self.parse_expression()?);
            if self.check(Kind::KwIn) {
                self.advance();
                arguments.push(self.parse_expression()?);
            }
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
        let buffer = self.parse_identifier()?;
        if self.check(Kind::Of) {
            self.advance();
            let lo = self.current;
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
                        self.advance();
                    }
                }
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
        let token = self.advance().clone();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(&self.source[token.start..token.end]),
        };
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(name);
        }
        let arg_token = self.advance().clone();
        let arg_span = tok_span(&arg_token);
        let arg_name = Identifier {
            span: arg_span,
            name: self.intern(&self.source[arg_token.start..arg_token.end]),
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
        let token = self.advance().clone();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(&self.source[token.start..token.end]),
        };
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(name);
        }
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
        let _kw_token = self.advance();
        let name_token = self.advance().clone();
        let end = self.extend_adjacent_compound_end(name_token.end, true);
        let identifier = Identifier {
            span: Span {
                start: span_u32(name_token.start),
                end: span_u32(end),
            },
            name: self.intern(&self.source[name_token.start..end]),
        };
        self.expr(ExpressionKind::Identifier(identifier))
    }

    fn parse_accum_primary(&mut self) -> ParseResult<ExprId> {
        let token = self.advance().clone();
        let name = Identifier {
            span: tok_span(&token),
            name: self.intern(&self.source[token.start..token.end]),
        };
        if self.check(Kind::LeftParen) {
            return self.parse_function_call(name);
        }
        let mut arguments: SmallVec<[ExprId; 4]> = SmallVec::new();
        if Self::can_be_identifier(self.peek().kind)
            && (self.peek_at(1).kind == Kind::By || Self::can_be_identifier(self.peek_at(1).kind))
        {
            self.advance();
        } else if Self::can_be_identifier(self.peek().kind)
            && let Ok(first_expr) = self.nested(Self::parse_postfix)
        {
            arguments.push(first_expr);
        }
        if self.check(Kind::LeftParen) {
            self.skip_balanced_parens();
            let arguments = self.push_expr_list(&arguments);
            return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
        }
        if self.check(Kind::By) {
            self.advance();
            if Self::can_be_identifier(self.peek().kind) {
                let lo = self.current;
                self.nested(Self::parse_postfix).ok();
                self.note_dropped(lo);
            }
            if matches!(self.peek().kind, Kind::IntegerLiteral | Kind::StringLiteral) {
                self.advance();
            }
        }
        if (Self::can_be_identifier(self.peek().kind) || self.check(Kind::LeftParen))
            && let Ok(field_expr) = self.nested(Self::parse_postfix)
        {
            arguments.push(field_expr);
        }
        let arguments = self.push_expr_list(&arguments);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }

    fn skip_balanced_parens(&mut self) {
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
        let token = self.advance().clone();
        let end = self.extend_adjacent_compound_end(token.end, false);
        let identifier = Identifier {
            span: Span {
                start: span_u32(token.start),
                end: span_u32(end),
            },
            name: self.intern_ident(token.start, end),
        };
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
