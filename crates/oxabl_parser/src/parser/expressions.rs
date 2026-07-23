//! Expression parsing for the Oxabl parser.
//!
//! Precedence levels (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative
//! > unary > postfix (member access, method calls, array/field access) > primary.

use oxabl_ast::{Expression, ExpressionKind, FindType, Identifier, Literal, Span, UnknownLiteral};
use oxabl_lexer::{Kind, TokenValue};

use super::{ParseError, ParseResult, Parser, debug_assert_expr_sibling_order};
use crate::literal::token_to_literal;

impl Parser<'_> {
    pub fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_ternary()
    }

    pub fn parse_ternary(&mut self) -> ParseResult<Expression> {
        if !self.check(Kind::KwIf) {
            return self.parse_or();
        }

        // Prefix form: capture `lo` from the IF token before advancing so the
        // span covers the leading IF (R1.1 full-extent rule).
        let lo = self.peek().start as u32;
        self.advance(); // consume IF
        let condition = self.parse_or()?; // condition can use OR/AND/comparison

        // Inside a &ELSEIF condition, IF...&THEN is not a ternary — &THEN terminates the
        // &ELSEIF condition block. Return the condition and leave &THEN for the caller.
        if self.check(Kind::PreprocThen) {
            return Ok(condition);
        }

        self.expect_kind(Kind::Then, "Expected 'THEN' after IF condition")?;

        let then_expr = self.parse_ternary()?; // recursive for nested ternary in then branch

        self.expect_kind(Kind::KwElse, "Expected 'ELSE' in IF expression")?;

        let else_expr = self.parse_ternary()?; // recursive for nested ternary in else branch

        let hi = self.prev_end();
        Ok(self.spanned_expr(
            lo,
            hi,
            ExpressionKind::IfThenElse(
                Box::new(condition),
                Box::new(then_expr),
                Box::new(else_expr),
            ),
        ))
    }

    pub fn parse_or(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_and()?;
        while self.check(Kind::Or) {
            let lo = expr.span.start;
            self.advance();
            let right = self.parse_and()?;
            let hi = self.prev_end();
            expr = self.spanned_expr(lo, hi, ExpressionKind::Or(Box::new(expr), Box::new(right)));
        }
        Ok(expr)
    }

    pub fn parse_and(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_comparison()?;
        while self.check(Kind::And) {
            let lo = expr.span.start;
            self.advance();
            let right = self.parse_comparison()?;
            let hi = self.prev_end();
            expr = self.spanned_expr(lo, hi, ExpressionKind::And(Box::new(expr), Box::new(right)));
        }
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

    pub fn parse_comparison(&mut self) -> ParseResult<Expression> {
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

        let lo = left.span.start;
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
            Kind::Equals | Kind::Eq => ExpressionKind::Equal(Box::new(left), Box::new(right)),
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
        let expr = self.spanned_expr(lo, hi, kind);

        Ok(expr)
    }

    pub fn parse_additive(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            if self.check(Kind::Add) || self.check(Kind::Minus) {
                let lo = expr.span.start;
                let op_kind = self.peek().kind;
                self.advance();
                let right_exp = self.parse_multiplicative()?;
                let hi = self.prev_end();
                match op_kind {
                    Kind::Add => {
                        expr = self.spanned_expr(
                            lo,
                            hi,
                            ExpressionKind::Add(Box::new(expr), Box::new(right_exp)),
                        );
                    }
                    Kind::Minus => {
                        expr = self.spanned_expr(
                            lo,
                            hi,
                            ExpressionKind::Minus(Box::new(expr), Box::new(right_exp)),
                        );
                    }
                    _ => unreachable!(),
                }
            } else if self.check(Kind::StringLiteral) && Self::is_string_like(&expr) {
                // Implicit string concatenation: adjacent tokens without explicit +.
                // Only fires when the LHS is already string-like (literal, preprop ref, or
                // a prior implicit concat). This prevents consuming display items or other
                // adjacent tokens as accidental concatenation.
                let lo = expr.span.start;
                let right_exp = self.parse_multiplicative()?;
                let hi = self.prev_end();
                expr = self.spanned_expr(
                    lo,
                    hi,
                    ExpressionKind::Add(Box::new(expr), Box::new(right_exp)),
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    pub fn parse_multiplicative(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_unary()?;
        while self.check(Kind::Star) || self.check(Kind::Slash) || self.check(Kind::Modulo) {
            let lo = expr.span.start;
            let op_kind = self.peek().kind;
            self.advance();
            match op_kind {
                Kind::Star => {
                    let right_exp = self.parse_unary()?;
                    let hi = self.prev_end();
                    expr = self.spanned_expr(
                        lo,
                        hi,
                        ExpressionKind::Multiply(Box::new(expr), Box::new(right_exp)),
                    );
                }
                Kind::Slash => {
                    let right_exp = self.parse_unary()?;
                    let hi = self.prev_end();
                    expr = self.spanned_expr(
                        lo,
                        hi,
                        ExpressionKind::Divide(Box::new(expr), Box::new(right_exp)),
                    );
                }
                Kind::Modulo => {
                    let right_exp = self.parse_unary()?;
                    let hi = self.prev_end();
                    expr = self.spanned_expr(
                        lo,
                        hi,
                        ExpressionKind::Modulo(Box::new(expr), Box::new(right_exp)),
                    );
                }
                _ => unreachable!(),
            }
        }
        Ok(expr)
    }

    pub fn parse_unary(&mut self) -> ParseResult<Expression> {
        if self.check(Kind::Minus) {
            // Prefix form: `lo` is the `-` token, captured before advancing.
            let lo = self.peek().start as u32;
            self.advance();
            let expr = self.parse_unary()?;
            let hi = self.prev_end();
            return Ok(self.spanned_expr(lo, hi, ExpressionKind::Negate(Box::new(expr))));
        }
        if self.check(Kind::Add) {
            // Unary plus — identity operation (e.g. "- + value" means subtract unary+value)
            self.advance();
            return self.parse_unary();
        }
        if self.check(Kind::Not) {
            // Prefix form: `lo` is the `NOT` token, captured before advancing.
            let lo = self.peek().start as u32;
            self.advance();
            let expr = self.parse_unary()?;
            let hi = self.prev_end();
            return Ok(self.spanned_expr(lo, hi, ExpressionKind::Not(Box::new(expr))));
        }
        self.parse_postfix()
    }

    pub fn parse_postfix(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_primary()?;

        // Literals can't have postfix operations (member access, method calls, etc.)
        // Return early to avoid incorrectly parsing following tokens like ':' in "do i = 1 to 10:"
        // Exception: string literals may have `:U` or `:T` character-type qualifiers — consume them.
        if matches!(expr.kind, ExpressionKind::Literal(_)) {
            if self.check(Kind::Colon) {
                let colon_end = self.tokens[self.current].end;
                if let Some(next) = self.tokens.get(self.current + 1)
                    && next.start == colon_end
                    && matches!(next.kind, Kind::Identifier)
                    && matches!(
                        self.source[next.start..next.end]
                            .to_ascii_lowercase()
                            .as_str(),
                        "u" | "t"
                    )
                {
                    self.advance(); // consume ':'
                    self.advance(); // consume 'U' or 'T'
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
                let colon_end = self.tokens[self.current].end;
                let next_is_member = self.tokens.get(self.current + 1).is_some_and(|t| {
                    Self::can_be_member_name(t.kind)
                        && !self.source[colon_end..t.start].contains('\n')
                });
                if !next_is_member {
                    break;
                }
                expr = self.parse_member_or_method(expr)?;
            } else if self.check(Kind::LeftBracket) {
                expr = self.parse_array_access(expr)?;
            } else if self.check(Kind::Period)
                && Self::can_have_field_access(&expr)
                && self.is_field_access_ahead()
            {
                expr = self.parse_field_access(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub fn parse_member_or_method(&mut self, object: Expression) -> ParseResult<Expression> {
        // Postfix left-nesting: the node spans from the object's start through
        // the closing `)`/member token (ast-invariants §5).
        let lo = object.span.start;
        self.advance(); // consumes ':'

        // Expect identifier after ':'
        if !Self::can_be_member_name(self.peek().kind) {
            return Err(ParseError {
                message: format!(
                    "Expected identifier after ':', found {:?}",
                    self.peek().kind
                ),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let token = self.advance().clone();
        let member = Identifier {
            span: Span {
                start: token.start as u32,
                end: token.end as u32,
            },
            name: self.source[token.start..token.end].to_string(),
        };

        // Check for method call: member followed by (
        if self.check(Kind::LeftParen) {
            self.advance(); // Consume '('

            // consume and store all arguments
            let mut arguments = Vec::new();
            if !self.check(Kind::RightParen) {
                // Skip optional INPUT/OUTPUT/INPUT-OUTPUT direction qualifier
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
                arguments.push(self.parse_expression()?);
                // Consume optional IN FRAME/BROWSE qualifier (field:handle IN FRAME f)
                if self.check(Kind::KwIn)
                    && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                {
                    self.advance(); // consume IN
                    self.advance(); // consume FRAME or BROWSE
                    self.consume_widget_name();
                }
                // Consume optional passing modifiers (BIND, BY-VALUE, BY-REFERENCE, APPEND)
                while matches!(
                    self.peek().kind,
                    Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                ) {
                    self.advance();
                }

                while self.check(Kind::Comma) {
                    self.advance(); // Consume ','
                    // Skip optional direction qualifier
                    if matches!(
                        self.peek().kind,
                        Kind::Input | Kind::Output | Kind::InputOutput
                    ) {
                        self.advance();
                    }
                    // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE/DATA-SOURCE
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
                    // Empty argument (successive commas like obj:method(1,, "x")) — Unknown
                    if self.check(Kind::Comma) || self.check(Kind::RightParen) {
                        let pos = self.peek().start as u32;
                        arguments.push(self.spanned_expr(
                            pos,
                            pos,
                            ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
                                span: Span {
                                    start: pos,
                                    end: pos,
                                },
                            })),
                        ));
                    } else {
                        arguments.push(self.parse_expression()?);
                        // Consume optional IN FRAME/BROWSE qualifier
                        if self.check(Kind::KwIn)
                            && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                        {
                            self.advance(); // consume IN
                            self.advance(); // consume FRAME or BROWSE
                            self.consume_widget_name();
                        }
                        // Consume optional passing modifiers
                        while matches!(
                            self.peek().kind,
                            Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                        ) {
                            self.advance();
                        }
                    }
                }
            }

            // if after parsing all arguments we don't find the
            // closing ), throw error
            self.expect_kind(Kind::RightParen, "Expected ')' after method arguments")?;

            debug_assert_expr_sibling_order(&arguments);
            let hi = self.prev_end();
            return Ok(self.spanned_expr(
                lo,
                hi,
                ExpressionKind::MethodCall {
                    object: Box::new(object),
                    method: member,
                    arguments,
                },
            ));
        }

        let hi = self.prev_end();
        Ok(self.spanned_expr(
            lo,
            hi,
            ExpressionKind::MemberAccess {
                object: Box::new(object),
                member,
            },
        ))
    }

    pub fn parse_array_access(&mut self, array: Expression) -> ParseResult<Expression> {
        let lo = array.span.start;
        self.advance(); // consume the '['

        let index = self.parse_expression()?;

        // Optional ABL extent range syntax: [start FOR count]
        if self.check(Kind::KwFor) {
            self.advance(); // consume FOR
            self.parse_expression().ok(); // consume the count
        }

        self.expect_kind(Kind::RightBracket, "Expected ']' after array index")?;

        let hi = self.prev_end();
        Ok(self.spanned_expr(
            lo,
            hi,
            ExpressionKind::ArrayAccess {
                array: Box::new(array),
                index: Box::new(index),
            },
        ))
    }

    /// Check if we're looking at field access, rather that a statement terminator
    pub fn is_field_access_ahead(&mut self) -> bool {
        // skip if it's not a period
        if !self.check(Kind::Period) {
            return false;
        }

        // Only treat as field access if an identifier (or keyword used as identifier) immediately
        // follows the period with no whitespace. A period followed by whitespace (space, newline)
        // before an identifier is a statement terminator, not a field access separator.
        // This prevents "p. var" (space after period) from being mis-parsed as "p.var".
        let period_end = self.tokens[self.current].end;
        self.tokens
            .get(self.current + 1)
            .is_some_and(|t| Self::can_be_identifier(t.kind) && t.start == period_end)
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

    pub fn parse_field_access(&mut self, qualifier: Expression) -> ParseResult<Expression> {
        let lo = qualifier.span.start;
        self.advance(); // consume '.'

        // Expect identifier (or keyword used as field name) after '.'
        if !Self::can_be_identifier(self.peek().kind) {
            return Err(ParseError {
                message: "Expected field name after '.'".to_string(),
                span: Span {
                    start: self.peek().start as u32,
                    end: self.peek().end as u32,
                },
            });
        }

        let token = self.advance().clone();
        let field = Identifier {
            span: Span {
                start: token.start as u32,
                end: token.end as u32,
            },
            name: self.source[token.start..token.end].to_string(),
        };

        let hi = self.prev_end();
        Ok(self.spanned_expr(
            lo,
            hi,
            ExpressionKind::FieldAccess {
                qualifier: Box::new(qualifier),
                field,
            },
        ))
    }

    /// Parse a primary expression and stamp its full-extent span.
    ///
    /// Thin funnel over [`parse_primary_inner`](Self::parse_primary_inner):
    /// `lo` is the first token examined, `hi` is the end of the last non-comment
    /// token consumed. Parenthesized groups therefore include the enclosing
    /// parens (U3), and every leaf/primary form is stamped uniformly regardless
    /// of which branch produced it.
    pub fn parse_primary(&mut self) -> ParseResult<Expression> {
        let lo = self.peek().start as u32;
        let mut expr = self.parse_primary_inner()?;
        expr.span = Span {
            start: lo,
            end: self.prev_end().max(lo),
        };
        Ok(expr)
    }

    fn parse_primary_inner(&mut self) -> ParseResult<Expression> {
        // Inline ternary: IF expr THEN expr ELSE expr — valid in any expression position
        if self.check(Kind::KwIf) {
            return self.parse_ternary();
        }

        // Implicit widget attribute/method: :attribute or :method(args)
        // In ABL, `:name` without a preceding expression accesses an attribute or
        // method on the currently focused widget (set by CHOOSE/UPDATE/SET).
        // Parse as an identifier and let postfix parsing handle method calls.
        if self.check(Kind::Colon) {
            let colon_token = self.advance().clone(); // consume ':'
            if Self::can_be_identifier(self.peek().kind) {
                let attr_token = self.advance().clone();
                let ident = Identifier {
                    span: Span {
                        start: colon_token.start as u32,
                        end: attr_token.end as u32,
                    },
                    name: format!(":{}", &self.source[attr_token.start..attr_token.end]),
                };
                return Ok(self.expr(ExpressionKind::Identifier(ident)));
            }
            // Bare colon with no identifier — return as unknown
            return Ok(
                self.expr(ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
                    span: Span {
                        start: colon_token.start as u32,
                        end: colon_token.end as u32,
                    },
                }))),
            );
        }

        // Bare field access: .fieldname — field of the default/implicit buffer.
        // In ABL, `.fieldname` without a preceding buffer name accesses a field on the
        // default buffer in scope (e.g., inside a FOR EACH block).
        if self.check(Kind::Period) && Self::can_be_identifier(self.peek_at(1).kind) {
            let period_token = self.advance().clone(); // consume '.'
            let field_token = self.advance().clone();
            let ident = Identifier {
                span: Span {
                    start: period_token.start as u32,
                    end: field_token.end as u32,
                },
                name: format!(".{}", &self.source[field_token.start..field_token.end]),
            };
            return Ok(self.expr(ExpressionKind::Identifier(ident)));
        }

        // Preprocessor reference: {&variable} or compound {&prefix}suffix{&more}...
        // e.g. {&pre}order{&ext} — multiple adjacent parts form a single identifier.
        // Requires direct adjacency (no whitespace) between each part.
        if self.check(Kind::Preprop) {
            let first = self.advance().clone();
            let compound_start = first.start;
            let mut compound_end = first.end;

            // Extend with directly-adjacent parts (identifiers, preprops, or
            // hyphen-separated word suffixes like `{&preproc}-control`).
            loop {
                let next = &self.tokens[self.current];
                if next.start != compound_end {
                    break;
                }
                if Self::can_be_identifier(next.kind) || next.kind == Kind::Preprop {
                    compound_end = self.advance().end;
                } else if next.kind == Kind::Minus {
                    // Adjacent hyphen: consume only if a word-like token follows directly.
                    let minus_end = next.end;
                    let after_idx = self.current + 1;
                    let after_ok = self
                        .tokens
                        .get(after_idx)
                        .is_some_and(|a| a.start == minus_end && Self::is_word_kind(a.kind));
                    if after_ok {
                        self.advance(); // consume '-'
                        compound_end = self.advance().end; // consume word
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Bare {&name} with no adjacent parts → PreprocReference
            if compound_end == first.end {
                let raw = &self.source[first.start..first.end];
                let name = raw
                    .strip_prefix("{&")
                    .and_then(|s| s.strip_suffix('}'))
                    .unwrap_or(raw)
                    .to_string();
                return Ok(self.expr(ExpressionKind::PreprocReference(name)));
            }
            // Compound → Identifier using raw source text
            let identifier = Identifier {
                span: Span {
                    start: compound_start as u32,
                    end: compound_end as u32,
                },
                name: self.source[compound_start..compound_end].to_string(),
            };
            if self.check(Kind::LeftParen) {
                return self.parse_function_call(identifier);
            }
            return Ok(self.expr(ExpressionKind::Identifier(identifier)));
        }

        // Mid-expression preprocessor conditional: &IF cond &THEN expr &ELSE expr &ENDIF
        if self.check(Kind::PreprocIf) {
            self.advance(); // consume &IF
            let preproc = self.parse_preproc_if(1, &Self::parse_expression)?;
            if preproc.else_branch.is_none() {
                return Err(ParseError {
                    message: "Expression-level &IF requires &ELSE branch".to_string(),
                    span: self.current_span(),
                });
            }
            return Ok(self.expr(ExpressionKind::PreprocIf(Box::new(preproc))));
        }

        // Parenthesized expression
        if self.check(Kind::LeftParen) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect_kind(Kind::RightParen, "Expected ')' after expression")?;
            return Ok(expr);
        }

        // Literals
        if self.check(Kind::IntegerLiteral)
            || self.check(Kind::BigIntLiteral)
            || self.check(Kind::DecimalLiteral)
            || self.check(Kind::StringLiteral)
            || self.check(Kind::KwTrue)
            || self.check(Kind::KwFalse)
            || self.check(Kind::Yes)
            || self.check(Kind::No)
            || self.check(Kind::Question)
        {
            let token = self.advance();
            let literal = token_to_literal(token).ok_or_else(|| ParseError {
                message: "Failed to convert token to literal".to_string(),
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            })?;
            return Ok(self.expr(ExpressionKind::Literal(literal)));
        }

        // Include file reference in expression position: {file.i}
        if self.check(Kind::IncludeReference) {
            let token = self.advance().clone();
            let path_and_args = match &token.value {
                TokenValue::String(s) => s.to_string(),
                _ => self.source[token.start + 1..token.end - 1]
                    .trim()
                    .to_string(),
            };
            return Ok(self.expr(ExpressionKind::IncludeReference {
                path_and_args,
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            }));
        }

        // Include positional argument reference in expression position: {1}
        if self.check(Kind::IncludeArgReference) {
            let token = self.advance().clone();
            let index = match &token.value {
                TokenValue::Integer(i) => *i as i64,
                _ => 0,
            };
            return Ok(self.expr(ExpressionKind::IncludeArgReference {
                index,
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            }));
        }

        // NEW ClassName(args) — object instantiation.
        // Class names may be dotted: NEW oe.wsdeco(company, gs-userid).
        if self.check(Kind::New) {
            self.advance(); // consume NEW

            // Parse dotted class name (e.g., "oe.wsdeco", "api.text", "Progress.Lang.Error").
            // Use is_word_kind() so keyword-named namespaces/types (e.g. api.text) are accepted.
            let start = self.peek().start;
            self.advance(); // consume first name component
            while self.check(Kind::Period)
                && self
                    .tokens
                    .get(self.current + 1)
                    .is_some_and(|t| Self::is_word_kind(t.kind))
            {
                self.advance(); // consume '.'
                self.advance(); // consume next component
            }
            let end = self.tokens[self.current - 1].end;
            let class_name = self.source[start..end].to_string();

            // Consume optional generic type args — `NEW List<String>(...)`.
            self.consume_generic_type_args();

            // If no '(' follows, this is the logical NEW record-name test (boolean expression),
            // not an OO object constructor.  Return a simple identifier so parsing continues.
            if !self.check(Kind::LeftParen) {
                return Ok(self.expr(ExpressionKind::Identifier(Identifier {
                    span: Span {
                        start: start as u32,
                        end: end as u32,
                    },
                    name: class_name,
                })));
            }

            // Parse argument list (supports INPUT/OUTPUT direction qualifiers and TABLE/BUFFER args)
            self.expect_kind(Kind::LeftParen, "Expected '(' after class name in NEW")?;
            let mut arguments = Vec::new();
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
                        self.advance(); // consume TABLE/BUFFER/DATASET etc.
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
                    } else {
                        arguments.push(self.parse_expression()?);
                    }
                    if !self.check(Kind::Comma) {
                        break;
                    }
                    self.advance(); // consume comma
                }
            }
            self.expect_kind(Kind::RightParen, "Expected ')' after NEW arguments")?;

            debug_assert_expr_sibling_order(&arguments);
            return Ok(self.expr(ExpressionKind::New {
                class_name,
                arguments,
            }));
        }

        // DYNAMIC-NEW expr([args]) — dynamic object instantiation where the class name
        // is a runtime expression (variable or string), not a static identifier.
        if self.check(Kind::DynamicNew) {
            self.advance(); // consume DYNAMIC-NEW
            // Parse the class name expression (typically an identifier or string)
            let _class_expr = self.parse_primary()?;
            // Parse optional argument list
            if self.check(Kind::LeftParen) {
                self.advance();
                if !self.check(Kind::RightParen) {
                    loop {
                        if matches!(
                            self.peek().kind,
                            Kind::Input | Kind::Output | Kind::InputOutput
                        ) {
                            self.advance();
                        }
                        self.parse_expression().ok();
                        if !self.check(Kind::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.expect_kind(Kind::RightParen, "Expected ')' after DYNAMIC-NEW arguments")?;
            }
            // Return as an empty expression (we don't have a DynamicNew AST node yet)
            return Ok(self.expr(ExpressionKind::Identifier(Identifier {
                span: self.current_span(),
                name: "dynamic-new".to_string(),
            })));
        }

        // DYNAMIC-FUNCTION("func-name" [IN handle] [, arg1, arg2, ...])
        if self.check(Kind::DynamicFunction) {
            let token = self.advance();
            let (ts, te) = (token.start, token.end);
            let name = Identifier {
                span: Span {
                    start: ts as u32,
                    end: te as u32,
                },
                name: self.source[ts..te].to_string(),
            };
            self.expect_kind(Kind::LeftParen, "Expected '(' after DYNAMIC-FUNCTION")?;
            let mut arguments = Vec::new();
            if !self.check(Kind::RightParen) {
                // First arg is the function name string
                arguments.push(self.parse_expression()?);
                // Optional IN handle clause
                if self.check(Kind::KwIn) {
                    self.advance();
                    arguments.push(self.parse_expression()?);
                }
                // Additional comma-separated arguments
                while self.check(Kind::Comma) {
                    self.advance();
                    arguments.push(self.parse_expression()?);
                }
            }
            self.expect_kind(Kind::RightParen, "Expected ')' after DYNAMIC-FUNCTION")?;
            debug_assert_expr_sibling_order(&arguments);
            return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
        }

        // CAN-FIND([FIRST|LAST] table [WHERE expr] [lock] [NO-ERROR])
        // Boolean expression that checks if a record matching the phrase exists.
        if self.check(Kind::CanFind) {
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
                self.advance(); // consume OF
                self.parse_identifier().ok(); // consume parent table name
            }

            // Lock type may appear before or after WHERE (ABL is flexible).
            let lock_type_before = self.parse_lock_type();

            // Optional WHERE clause
            let where_clause = if self.check(Kind::KwWhere) {
                self.advance();
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            // Trailing lock type (canonical position); prefer the first one found
            let lock_type_after = self.parse_lock_type();
            let lock_type = if lock_type_before != oxabl_ast::LockType::ShareLock {
                lock_type_before
            } else {
                lock_type_after
            };

            // Optional USE-INDEX hint (e.g. CAN-FIND(t WHERE cond USE-INDEX idx))
            if self.check(Kind::UseIndex) {
                self.advance(); // consume USE-INDEX
                if Self::can_be_identifier(self.peek().kind) {
                    self.advance(); // consume index name
                }
            }

            // Optional NO-WAIT — don't block if record is locked by another user
            if self.check(Kind::NoWait) {
                self.advance();
            }

            // Optional NO-ERROR
            let no_error = if self.check(Kind::NoError) {
                self.advance();
                true
            } else {
                false
            };

            self.expect_kind(
                Kind::RightParen,
                "Expected ')' after CAN-FIND record phrase",
            )?;

            return Ok(self.expr(ExpressionKind::CanFind {
                find_type,
                buffer,
                where_clause,
                lock_type,
                no_error,
            }));
        }

        // LOCKED [table] / LOCKED(table) — checks if a record is exclusively locked.
        // ABL allows both parenthesized and bare forms.
        if self.check(Kind::Locked) {
            let token = self.advance().clone();
            let name = Identifier {
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
                name: self.source[token.start..token.end].to_string(),
            };
            if self.check(Kind::LeftParen) {
                return self.parse_function_call(name);
            }
            // Bare form: LOCKED table — parse the table name as the sole argument
            let arg_token = self.advance().clone();
            let (arg_start, arg_end) = (arg_token.start as u32, arg_token.end as u32);
            let arg_name = Identifier {
                span: Span {
                    start: arg_start,
                    end: arg_end,
                },
                name: self.source[arg_token.start..arg_token.end].to_string(),
            };
            let arg_expr =
                self.spanned_expr(arg_start, arg_end, ExpressionKind::Identifier(arg_name));
            return Ok(self.expr(ExpressionKind::FunctionCall {
                name,
                arguments: vec![arg_expr],
            }));
        }

        // AVAILABLE [table] / AVAILABLE(table) — built-in record-availability predicate.
        // ABL allows both parenthesized and bare forms, e.g.:
        //   IF AVAILABLE order THEN ...
        //   IF AVAILABLE(order) THEN ...
        if self.check(Kind::Available) || self.check(Kind::Ambiguous) {
            let token = self.advance().clone();
            let name = Identifier {
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
                name: self.source[token.start..token.end].to_string(),
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
                    arguments: vec![],
                }));
            }
            let arg = self.parse_postfix()?;
            return Ok(self.expr(ExpressionKind::FunctionCall {
                name,
                arguments: vec![arg],
            }));
        }

        // TEMP-TABLE name[:attr] / BUFFER name[:attr] / FRAME name[:attr] / BROWSE name[:attr]
        // — handle/object reference expressions.
        // When these keywords appear in expression context followed by an identifier,
        // consume both tokens as a compound identifier (e.g. BROWSE b-whse-p:first-column).
        // Also handles BUFFER {&preproc} for dynamic buffer references.
        if (self.check(Kind::TempTable)
            || self.check(Kind::Buffer)
            || self.check(Kind::Frame)
            || self.check(Kind::Query)
            || self.check(Kind::Dataset)
            || self.check(Kind::Stream)
            || self.check(Kind::Browse))
            && (Self::can_be_identifier(self.peek_at(1).kind) || self.check_at(1, Kind::Preprop))
        {
            let kw_token = self.advance(); // consume TEMP-TABLE or BUFFER
            let start = kw_token.start;
            let name_token = self.advance(); // consume the table/buffer name
            let name_start = name_token.start;
            let mut end = name_token.end;

            // Extend with directly-adjacent Preprop or identifier parts so that
            // compound names like `dataset ds{&mainTable}` or `buffer b-{&tbl}`
            // are consumed whole. Without this, the adjacent preprop is left as
            // the next token and the postfix `:member` access is never parsed.
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

            // Use only the handle *name* (qh), not the keyword prefix (QUERY).
            // `QUERY qh:ATTR` is handle-qualified access; resolving "QUERY qh"
            // as a single atom always fails LINT0001 (#58 item C).
            let _ = start; // keyword start kept for potential span diagnostics
            let handle_name = self.source[name_start..end].to_string();
            let identifier = Identifier {
                span: Span {
                    start: name_start as u32,
                    end: end as u32,
                },
                name: handle_name,
            };
            return Ok(self.expr(ExpressionKind::Identifier(identifier)));
        }

        // ACCUM has two forms:
        //   Form 1: ACCUM aggregate-type [BY break-field] field  (e.g. ACCUM TOTAL amt-sale)
        //   Form 2: ACCUM field (aggregate-type [BY break-field]) (e.g. ACCUM qty (TOTAL BY item))
        // When not followed by '(', parse the first operand via parse_postfix so that qualified
        // fields like table.field are consumed whole.  A trailing '(' signals form 2.
        if self.check(Kind::Accum) {
            let token = self.advance();
            let (ts, te) = (token.start, token.end);
            let name = Identifier {
                span: Span {
                    start: ts as u32,
                    end: te as u32,
                },
                name: self.source[ts..te].to_string(),
            };
            if self.check(Kind::LeftParen) {
                return self.parse_function_call(name);
            }
            let mut arguments = Vec::new();
            // Parse the first operand (aggregate-type word OR qualified field reference).
            // Use parse_postfix so that table.field forms are consumed whole.
            if Self::can_be_identifier(self.peek().kind)
                && let Ok(first_expr) = self.parse_postfix()
            {
                arguments.push(first_expr);
            }
            // Form 2: the first operand was the field; the parenthesised spec follows.
            // Consume '(' ... ')' and return.
            if self.check(Kind::LeftParen) {
                self.advance(); // consume '('
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
                    self.advance(); // consume ')'
                }
                return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
            }
            // Form 1: first operand was the aggregate-type.
            // optional BY break-field [label] (may be qualified: table.field)
            if self.check(Kind::By) {
                self.advance();
                if Self::can_be_identifier(self.peek().kind) {
                    self.parse_postfix().ok();
                }
                // Optional label (integer or string) identifying the break level
                if matches!(self.peek().kind, Kind::IntegerLiteral | Kind::StringLiteral) {
                    self.advance();
                }
            }
            // field name — may be an identifier, qualified field, indexed access, or
            // parenthesized expression like (expr1 - expr2)
            if (Self::can_be_identifier(self.peek().kind) || self.check(Kind::LeftParen))
                && let Ok(field_expr) = self.parse_postfix()
            {
                arguments.push(field_expr);
            }
            return Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }));
        }

        // INPUT / OUTPUT as transparent parameter-direction qualifier in expression context.
        // e.g. WHERE field EQ INPUT param-name — consume INPUT/OUTPUT/INPUT-OUTPUT and
        // then parse the actual operand.
        if matches!(
            self.peek().kind,
            Kind::Input | Kind::Output | Kind::InputOutput
        ) {
            self.advance(); // consume the direction qualifier
            return self.parse_primary();
        }

        // Identifiers and callable keywords (built-in functions like NOW, TRIM, etc.)
        if Self::can_be_identifier(self.peek().kind) {
            let token = self.advance();
            let start = token.start;
            let mut end = token.end;

            // identifier{&suffix} compound: e.g. b-{&line-buffer} (direct adjacency only).
            // Also handles adjacent hyphen-word: e.g. b-{&preproc}-suffix.
            loop {
                let next = &self.tokens[self.current];
                if next.start != end {
                    break;
                }
                if next.kind == Kind::Preprop || Self::can_be_identifier(next.kind) {
                    end = self.advance().end;
                } else if next.kind == Kind::Minus {
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

            let name = self.source[start..end].to_string();
            let identifier = Identifier {
                span: Span {
                    start: start as u32,
                    end: end as u32,
                },
                name,
            };

            // Check for function call: identifier/callable followed by '('
            if self.check(Kind::LeftParen) {
                return self.parse_function_call(identifier);
            }

            return Ok(self.expr(ExpressionKind::Identifier(identifier)));
        }

        Err(ParseError {
            message: format!("Unexpected token {:?}", self.peek().kind),
            span: Span {
                start: self.peek().start as u32,
                end: self.peek().end as u32,
            },
        })
    }

    pub fn parse_function_call(&mut self, name: Identifier) -> ParseResult<Expression> {
        self.advance(); // consume the left parenthesis

        let mut arguments = Vec::new();

        // Empty argument list
        if !self.check(Kind::RightParen) {
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
            // parse first argument
            arguments.push(self.parse_expression()?);
            // Consume optional IN FRAME/BROWSE qualifier (field:handle IN FRAME f)
            if self.check(Kind::KwIn) && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
            {
                self.advance(); // consume IN
                self.advance(); // consume FRAME or BROWSE
                self.advance(); // consume name
            }
            // Consume optional passing modifiers
            while matches!(
                self.peek().kind,
                Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
            ) {
                self.advance();
            }

            // parse remaining — separated by ',' or, when a Preprop reference like {&args}
            // expands to comma-separated arguments at compile time, by a bare direction
            // qualifier (INPUT/OUTPUT/INPUT-OUTPUT) with no preceding comma.
            while self.check(Kind::Comma)
                || matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput
                )
            {
                if self.check(Kind::Comma) {
                    self.advance(); // consume ',' only when present
                }
                // Skip optional direction qualifier per argument
                if matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput
                ) {
                    self.advance();
                }
                // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE/DATA-SOURCE
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
                // Empty argument (successive commas like func(1,, "x")) — use Unknown literal
                if self.check(Kind::Comma) || self.check(Kind::RightParen) {
                    let pos = self.peek().start as u32;
                    arguments.push(self.spanned_expr(
                        pos,
                        pos,
                        ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
                            span: Span {
                                start: pos,
                                end: pos,
                            },
                        })),
                    ));
                } else {
                    arguments.push(self.parse_expression()?);
                    // Consume optional IN FRAME/BROWSE qualifier
                    if self.check(Kind::KwIn)
                        && matches!(self.peek_at(1).kind, Kind::Frame | Kind::Browse)
                    {
                        self.advance(); // consume IN
                        self.advance(); // consume FRAME or BROWSE
                        self.consume_widget_name();
                    }
                    // Consume optional passing modifiers
                    while matches!(
                        self.peek().kind,
                        Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                    ) {
                        self.advance();
                    }
                }
            }
        }

        self.expect_kind(Kind::RightParen, "Expected ')' after function arguments")?;

        debug_assert_expr_sibling_order(&arguments);
        Ok(self.expr(ExpressionKind::FunctionCall { name, arguments }))
    }
}
