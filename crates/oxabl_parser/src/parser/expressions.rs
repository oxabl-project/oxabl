//! Expression parsing for the Oxabl parser.
//!
//! Precedence levels (lowest to highest):
//! ternary (IF/THEN/ELSE) > OR > AND > comparison > additive > multiplicative
//! > unary > postfix (member access, method calls, array/field access) > primary.

use oxabl_ast::{Expression, FindType, Identifier, Span};
use oxabl_lexer::{Kind, TokenValue};

use super::{ParseError, ParseResult, Parser};
use crate::literal::token_to_literal;

impl Parser<'_> {
    pub fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_ternary()
    }

    pub fn parse_ternary(&mut self) -> ParseResult<Expression> {
        if !self.check(Kind::KwIf) {
            return self.parse_or();
        }

        self.advance(); // consume IF
        let condition = self.parse_or()?; // condition can use OR/AND/comparison

        self.expect_kind(Kind::Then, "Expected 'THEN' after IF condition")?;

        let then_expr = self.parse_ternary()?; // recursive for nested ternary in then branch

        self.expect_kind(Kind::KwElse, "Expected 'ELSE' in IF expression")?;

        let else_expr = self.parse_ternary()?; // recursive for nested ternary in else branch

        Ok(Expression::IfThenElse(
            Box::new(condition),
            Box::new(then_expr),
            Box::new(else_expr),
        ))
    }

    pub fn parse_or(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_and()?;
        while self.check(Kind::Or) {
            self.advance();
            let right = self.parse_and()?;
            expr = Expression::Or(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    pub fn parse_and(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_comparison()?;
        while self.check(Kind::And) {
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expression::And(Box::new(expr), Box::new(right));
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

        if !self.is_comparison_operator() {
            return Ok(left);
        }

        let op_kind = self.advance().kind;
        let right = self.parse_additive()?;

        let expr = match op_kind {
            Kind::Equals | Kind::Eq => Expression::Equal(Box::new(left), Box::new(right)),
            Kind::NotEqual | Kind::Ne => Expression::NotEqual(Box::new(left), Box::new(right)),
            Kind::LessThan | Kind::Lt => Expression::LessThan(Box::new(left), Box::new(right)),
            Kind::LessThanOrEqual | Kind::Le => {
                Expression::LessThanOrEqual(Box::new(left), Box::new(right))
            }
            Kind::GreaterThan | Kind::Gt => {
                Expression::GreaterThan(Box::new(left), Box::new(right))
            }
            Kind::GreaterThanOrEqual | Kind::Ge => {
                Expression::GreaterThanOrEqual(Box::new(left), Box::new(right))
            }
            Kind::Begins => Expression::Begins(Box::new(left), Box::new(right)),
            Kind::Matches => Expression::Matches(Box::new(left), Box::new(right)),
            Kind::Contains => Expression::Contains(Box::new(left), Box::new(right)),
            _ => unreachable!(),
        };

        Ok(expr)
    }

    pub fn parse_additive(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_multiplicative()?;
        loop {
            if self.check(Kind::Add) || self.check(Kind::Minus) {
                let op_kind = self.peek().kind;
                self.advance();
                let right_exp = self.parse_multiplicative()?;
                match op_kind {
                    Kind::Add => {
                        expr = Expression::Add(Box::new(expr), Box::new(right_exp));
                    }
                    Kind::Minus => {
                        expr = Expression::Minus(Box::new(expr), Box::new(right_exp));
                    }
                    _ => unreachable!(),
                }
            } else if self.check(Kind::StringLiteral) && Self::is_string_like(&expr) {
                // Implicit string concatenation: adjacent tokens without explicit +.
                // Only fires when the LHS is already string-like (literal, preprop ref, or
                // a prior implicit concat). This prevents consuming display items or other
                // adjacent tokens as accidental concatenation.
                let right_exp = self.parse_multiplicative()?;
                expr = Expression::Add(Box::new(expr), Box::new(right_exp));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    pub fn parse_multiplicative(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_unary()?;
        while self.check(Kind::Star) || self.check(Kind::Slash) || self.check(Kind::Modulo) {
            let operator = self.advance();
            match operator.kind {
                Kind::Star => {
                    let right_exp = self.parse_unary()?;
                    expr = Expression::Multiply(Box::new(expr), Box::new(right_exp));
                }
                Kind::Slash => {
                    let right_exp = self.parse_unary()?;
                    expr = Expression::Divide(Box::new(expr), Box::new(right_exp));
                }
                Kind::Modulo => {
                    let right_exp = self.parse_unary()?;
                    expr = Expression::Modulo(Box::new(expr), Box::new(right_exp));
                }
                _ => unreachable!(),
            }
        }
        Ok(expr)
    }

    pub fn parse_unary(&mut self) -> ParseResult<Expression> {
        if self.check(Kind::Minus) {
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expression::Negate(Box::new(expr)));
        }
        if self.check(Kind::Add) {
            // Unary plus — identity operation (e.g. "- + value" means subtract unary+value)
            self.advance();
            return self.parse_unary();
        }
        if self.check(Kind::Not) {
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expression::Not(Box::new(expr)));
        }
        self.parse_postfix()
    }

    pub fn parse_postfix(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_primary()?;

        // Literals can't have postfix operations (member access, method calls, etc.)
        // Return early to avoid incorrectly parsing following tokens like ':' in "do i = 1 to 10:"
        if matches!(expr, Expression::Literal(_)) {
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
                    Self::can_be_identifier(t.kind)
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
        self.advance(); // consumes ':'

        // Expect identifier after ':'
        if !Self::can_be_identifier(self.peek().kind) {
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
                // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE for handle args
                if matches!(
                    self.peek().kind,
                    Kind::Table | Kind::TableHandle | Kind::Dataset | Kind::DatasetHandle
                ) {
                    self.advance();
                }
                arguments.push(self.parse_expression()?);
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
                    // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE
                    if matches!(
                        self.peek().kind,
                        Kind::Table | Kind::TableHandle | Kind::Dataset | Kind::DatasetHandle
                    ) {
                        self.advance();
                    }
                    arguments.push(self.parse_expression()?);
                    // Consume optional passing modifiers
                    while matches!(
                        self.peek().kind,
                        Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                    ) {
                        self.advance();
                    }
                }
            }

            // if after parsing all arguments we don't find the
            // closing ), throw error
            self.expect_kind(Kind::RightParen, "Expected ')' after method arguments")?;

            return Ok(Expression::MethodCall {
                object: Box::new(object),
                method: member,
                arguments,
            });
        }

        Ok(Expression::MemberAccess {
            object: Box::new(object),
            member,
        })
    }

    pub fn parse_array_access(&mut self, array: Expression) -> ParseResult<Expression> {
        self.advance(); // consume the '['

        let index = self.parse_expression()?;

        self.expect_kind(Kind::RightBracket, "Expected ']' after array index")?;

        Ok(Expression::ArrayAccess {
            array: Box::new(array),
            index: Box::new(index),
        })
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
            expr,
            Expression::Literal(oxabl_ast::Literal::String(_))
                | Expression::PreprocReference(_)
                | Expression::Add(..)
        )
    }

    /// Check if an expression can be the base of field access (Table.Field)
    fn can_have_field_access(expr: &Expression) -> bool {
        matches!(
            expr,
            Expression::Identifier(_)
                | Expression::FieldAccess { .. }
                | Expression::PreprocReference(_)
        )
    }

    pub fn parse_field_access(&mut self, qualifier: Expression) -> ParseResult<Expression> {
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

        Ok(Expression::FieldAccess {
            qualifier: Box::new(qualifier),
            field,
        })
    }

    pub fn parse_primary(&mut self) -> ParseResult<Expression> {
        // Inline ternary: IF expr THEN expr ELSE expr — valid in any expression position
        if self.check(Kind::KwIf) {
            return self.parse_ternary();
        }

        // Preprocessor reference: {&variable} or compound {&prefix}suffix
        if self.check(Kind::Preprop) {
            let token = self.advance().clone();
            // Strip {& and } to get the variable name
            let raw = &self.source[token.start..token.end];
            let name = raw
                .strip_prefix("{&")
                .and_then(|s| s.strip_suffix('}'))
                .unwrap_or(raw)
                .to_string();
            // {&prefix}suffix — preprocessor prefix followed by identifier on same line
            // e.g. {&web}order expands at runtime to a single identifier
            if Self::can_be_identifier(self.peek().kind) {
                let next_tok = &self.tokens[self.current];
                if !self.source[token.end..next_tok.start].contains('\n') {
                    let suffix_tok = self.advance().clone();
                    let suffix = &self.source[suffix_tok.start..suffix_tok.end];
                    let compound = format!("{{{}&}}{}", name, suffix);
                    return Ok(Expression::Identifier(Identifier {
                        span: Span {
                            start: token.start as u32,
                            end: suffix_tok.end as u32,
                        },
                        name: compound,
                    }));
                }
            }
            return Ok(Expression::PreprocReference(name));
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
            return Ok(Expression::PreprocIf(Box::new(preproc)));
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
            return Ok(Expression::Literal(literal));
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
            return Ok(Expression::IncludeReference {
                path_and_args,
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            });
        }

        // Include positional argument reference in expression position: {1}
        if self.check(Kind::IncludeArgReference) {
            let token = self.advance().clone();
            let index = match &token.value {
                TokenValue::Integer(i) => *i as i64,
                _ => 0,
            };
            return Ok(Expression::IncludeArgReference {
                index,
                span: Span {
                    start: token.start as u32,
                    end: token.end as u32,
                },
            });
        }

        // NEW ClassName(args) — object instantiation.
        // Class names may be dotted: NEW oe.wsdeco(company, gs-userid).
        if self.check(Kind::New) {
            self.advance(); // consume NEW

            // Parse dotted class name (e.g., "oe.wsdeco" or "Progress.Lang.Error")
            let start = self.peek().start;
            self.advance(); // consume first name component
            while self.check(Kind::Period) && self.check_at(1, Kind::Identifier) {
                self.advance(); // consume '.'
                self.advance(); // consume next component
            }
            let end = self.tokens[self.current - 1].end;
            let class_name = self.source[start..end].to_string();

            // Parse argument list (supports INPUT/OUTPUT direction qualifiers)
            self.expect_kind(Kind::LeftParen, "Expected '(' after class name in NEW")?;
            let mut arguments = Vec::new();
            if !self.check(Kind::RightParen) {
                // Skip optional direction qualifier
                if matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput
                ) {
                    self.advance();
                }
                arguments.push(self.parse_expression()?);
                while self.check(Kind::Comma) {
                    self.advance();
                    // Skip optional direction qualifier
                    if matches!(
                        self.peek().kind,
                        Kind::Input | Kind::Output | Kind::InputOutput
                    ) {
                        self.advance();
                    }
                    arguments.push(self.parse_expression()?);
                }
            }
            self.expect_kind(Kind::RightParen, "Expected ')' after NEW arguments")?;

            return Ok(Expression::New {
                class_name,
                arguments,
            });
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
            return Ok(Expression::FunctionCall { name, arguments });
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

            return Ok(Expression::CanFind {
                find_type,
                buffer,
                where_clause,
                lock_type,
                no_error,
            });
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
            let arg_name = Identifier {
                span: Span {
                    start: arg_token.start as u32,
                    end: arg_token.end as u32,
                },
                name: self.source[arg_token.start..arg_token.end].to_string(),
            };
            return Ok(Expression::FunctionCall {
                name,
                arguments: vec![Expression::Identifier(arg_name)],
            });
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
            let arg = self.parse_primary()?;
            return Ok(Expression::FunctionCall {
                name,
                arguments: vec![arg],
            });
        }

        // TEMP-TABLE name[:attr] / BUFFER name[:attr] / FRAME name[:attr] — handle expressions.
        // When TEMP-TABLE, BUFFER, or FRAME appear in expression context followed by an identifier,
        // consume both tokens as a compound identifier.
        // Also handles BUFFER {&preproc} for dynamic buffer references.
        if (self.check(Kind::TempTable)
            || self.check(Kind::Buffer)
            || self.check(Kind::Frame)
            || self.check(Kind::Query)
            || self.check(Kind::Dataset)
            || self.check(Kind::Stream))
            && (Self::can_be_identifier(self.peek_at(1).kind) || self.check_at(1, Kind::Preprop))
        {
            let kw_token = self.advance(); // consume TEMP-TABLE or BUFFER
            let start = kw_token.start;
            let name_token = self.advance(); // consume the table/buffer name
            let end = name_token.end;
            let compound_name = self.source[start..end].to_string();
            let identifier = Identifier {
                span: Span {
                    start: start as u32,
                    end: end as u32,
                },
                name: compound_name,
            };
            return Ok(Expression::Identifier(identifier));
        }

        // ACCUM aggregate-type field  (e.g. ACCUM TOTAL amt-sale)
        // When not followed by '(', consume the aggregate-type identifier and the field name.
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
            // aggregate-type (TOTAL, AVERAGE, COUNT, MINIMUM, MAXIMUM, etc.)
            if Self::can_be_identifier(self.peek().kind) {
                let agg = self.advance();
                let (as_, ae) = (agg.start, agg.end);
                arguments.push(Expression::Identifier(Identifier {
                    span: Span {
                        start: as_ as u32,
                        end: ae as u32,
                    },
                    name: self.source[as_..ae].to_string(),
                }));
                // optional BY break-field (may be qualified: table.field)
                if self.check(Kind::By) {
                    self.advance();
                    if Self::can_be_identifier(self.peek().kind) {
                        self.parse_postfix().ok();
                    }
                }
            }
            // field name — may be an identifier, qualified field, indexed access, or
            // parenthesized expression like (expr1 - expr2)
            let is_field_start =
                Self::can_be_identifier(self.peek().kind) || self.check(Kind::LeftParen);
            if is_field_start && let Ok(field_expr) = self.parse_postfix() {
                arguments.push(field_expr);
            }
            return Ok(Expression::FunctionCall { name, arguments });
        }

        // Identifiers and callable keywords (built-in functions like NOW, TRIM, etc.)
        if Self::can_be_identifier(self.peek().kind) {
            let token = self.advance();
            let start = token.start;
            let end = token.end;
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

            return Ok(Expression::Identifier(identifier));
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
            // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE for handle args
            if matches!(
                self.peek().kind,
                Kind::Table | Kind::TableHandle | Kind::Dataset | Kind::DatasetHandle
            ) {
                self.advance();
            }
            // parse first argument
            arguments.push(self.parse_expression()?);
            // Consume optional passing modifiers
            while matches!(
                self.peek().kind,
                Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
            ) {
                self.advance();
            }

            // parse remaining
            while self.check(Kind::Comma) {
                self.advance(); // consume ','
                // Skip optional direction qualifier per argument
                if matches!(
                    self.peek().kind,
                    Kind::Input | Kind::Output | Kind::InputOutput
                ) {
                    self.advance();
                }
                // Skip optional TABLE/TABLE-HANDLE/DATASET/DATASET-HANDLE
                if matches!(
                    self.peek().kind,
                    Kind::Table | Kind::TableHandle | Kind::Dataset | Kind::DatasetHandle
                ) {
                    self.advance();
                }
                arguments.push(self.parse_expression()?);
                // Consume optional passing modifiers
                while matches!(
                    self.peek().kind,
                    Kind::Bind | Kind::ByValue | Kind::ByReference | Kind::Append
                ) {
                    self.advance();
                }
            }
        }

        self.expect_kind(Kind::RightParen, "Expected ')' after function arguments")?;

        Ok(Expression::FunctionCall { name, arguments })
    }
}
