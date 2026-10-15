use super::*;
use oxabl_ast::{
    AccessModifier, BooleanLiteral, BufferTarget, CommentKind, CreateTarget, CreateTargetKind,
    DataSourceKeys, DataType, DbTriggerEvent, DecimalLiteral, DeleteObjectKind, ExpressionKind,
    FindType, FrameField, HandleParamKind, Identifier, IntegerLiteral, Literal, LockType, NodeId,
    OnAction, OnKind, ParameterDirection, ParameterType, RunTarget, SortDirection, Span,
    StatementKind, StreamDirection, StreamOperation, StringLiteral, SubscribeTarget, TypeSource,
    UnknownLiteral, UsingSource, WhenBranch, WidgetQualifier,
};
use oxabl_lexer::tokenize;
use rust_decimal::Decimal;
use std::str::FromStr;

/// The harvested names on a [`StatementKind::Skipped`] node, as plain strings.
///
/// `Identifier` derives `PartialEq` *including* `span`, so asserting on whole
/// statements would mean hand-building exact byte offsets for every name. Tests
/// assert on the contents instead.
#[track_caller]
fn skipped_names(stmt: &Statement) -> Vec<&str> {
    match &stmt.kind {
        StatementKind::Skipped { names, .. } => names.iter().map(|n| n.name.as_str()).collect(),
        other => panic!("expected StatementKind::Skipped, got {other:?}"),
    }
}

/// Parse `source` as one statement and return its harvested names.
#[track_caller]
fn harvest(source: &str) -> Vec<String> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("statement should parse");
    skipped_names(&stmt)
        .into_iter()
        .map(str::to_string)
        .collect()
}

/// Parse `source` as one statement and return its table-candidate marker (#130).
#[track_caller]
fn marks_tables(source: &str) -> bool {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("statement should parse");
    match &stmt.kind {
        StatementKind::Skipped {
            may_reference_tables,
            ..
        } => *may_reference_tables,
        other => panic!("expected StatementKind::Skipped, got {other:?}"),
    }
}

/// Assert `span` covers exactly the first occurrence of `needle` in `source`.
///
/// Offsets are located rather than hardcoded so the assertion survives an edit
/// to the fixture text.
#[track_caller]
fn assert_span_covers(source: &str, span: Span, needle: &str) {
    let start = source
        .find(needle)
        .unwrap_or_else(|| panic!("{needle:?} does not appear in {source:?}"));
    assert_eq!(span.start as usize, start, "span start");
    assert_eq!(span.end as usize, start + needle.len(), "span end");
}

#[test]
fn parse_simple_add_expression() {
    let source = "1 + 2";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_double_add_expression() {
    let source = "1 + 2 + 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::Add(
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 0, end: 1 },
                            value: 1
                        }))
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 4, end: 5 },
                            value: 2
                        }))
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_simple_minus_expression() {
    let source = "1 - 2";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Minus(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_double_minus_expression() {
    let source = "1 - 2 - 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Minus(
            Box::new(
                ExpressionKind::Minus(
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 0, end: 1 },
                            value: 1
                        }))
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 4, end: 5 },
                            value: 2
                        }))
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_add_minus_expression() {
    let source = "1 + 2 - 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Minus(
            Box::new(
                ExpressionKind::Add(
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 0, end: 1 },
                            value: 1
                        }))
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 4, end: 5 },
                            value: 2
                        }))
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_simple_multiplication_expression() {
    let source = "1 * 2";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Multiply(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_add_multiplication_expression() {
    let source = "1 + 2 * 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Multiply(
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 4, end: 5 },
                            value: 2
                        }))
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 8, end: 9 },
                            value: 3
                        }))
                        .into()
                    )
                )
                .into()
            ),
        )
    );
}

#[test]
fn parse_simple_division_expression() {
    let source = "6 / 2";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Divide(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 6
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_parenthesized_expression() {
    let source = "(1 + 2)";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 1, end: 2 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 5, end: 6 },
                    value: 2
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_parentheses_override_precedence() {
    let source = "(1 + 2) * 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Multiply(
            Box::new(
                ExpressionKind::Add(
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 1, end: 2 },
                            value: 1
                        }))
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 5, end: 6 },
                            value: 2
                        }))
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 10, end: 11 },
                    value: 3
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_unary_negate() {
    let source = "-5";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Negate(Box::new(
            ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 1, end: 2 },
                value: 5
            }))
            .into()
        ))
    );
}

#[test]
fn parse_double_negate() {
    let source = "--5";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Negate(Box::new(
            ExpressionKind::Negate(Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 2, end: 3 },
                    value: 5
                }))
                .into()
            ))
            .into()
        ))
    );
}

#[test]
fn parse_unary_not() {
    let source = "not true";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Not(Box::new(
            ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
                span: Span { start: 4, end: 8 },
                value: true
            }))
            .into()
        ))
    );
}

#[test]
fn parse_decimal_literal() {
    let source = "3.14";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Literal(Literal::Decimal(DecimalLiteral {
            span: Span { start: 0, end: 4 },
            value: Decimal::from_str("3.14").unwrap()
        }))
    );
}

#[test]
fn parse_string_literal() {
    let source = "\"hello\"";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Literal(Literal::String(StringLiteral {
            span: Span { start: 0, end: 7 },
            value: "hello".to_string()
        }))
    );
}

#[test]
fn parse_boolean_literals() {
    let source = "true";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
            span: Span { start: 0, end: 4 },
            value: true
        }))
    );
}

#[test]
fn parse_unknown_literal() {
    let source = "?";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
            span: Span { start: 0, end: 1 }
        }))
    );
}

#[test]
fn parse_modulo_expression() {
    // ABL uses mod/modulo keyword for modulo
    let source = "10 mod 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Modulo(
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 2 },
                    value: 10
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 7, end: 8 },
                    value: 3
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_identifier() {
    let source = "myVar";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Identifier(Identifier {
            span: Span { start: 0, end: 5 },
            name: "myVar".to_string()
        })
    );
}

#[test]
fn parse_hyphenated_identifier() {
    let source = "my-hyphenated-var";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Identifier(Identifier {
            span: Span { start: 0, end: 17 },
            name: "my-hyphenated-var".to_string()
        })
    );
}

#[test]
fn parse_equal_expression() {
    let source = "a = b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Equal(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_not_equal_expression() {
    let source = "a <> b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::NotEqual(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_less_than_keyword() {
    let source = "a lt b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::LessThan(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_begins_expression() {
    let source = "userName begins \"Jo\"";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Begins(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 8 },
                    name: "userName".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::String(StringLiteral {
                    span: Span { start: 16, end: 20 },
                    value: "Jo".to_string()
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_contains_expression() {
    let source = "content contains \"abc\"";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Contains(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 7 },
                    name: "content".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::String(StringLiteral {
                    span: Span { start: 17, end: 22 },
                    value: "abc".to_string()
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_and_expression() {
    let source = "a and b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::And(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 6, end: 7 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_or_expression() {
    let source = "a or b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Or(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_or_and_precedence() {
    // a or b and c should parse as a or (b and c)
    let source = "a or b and c";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Or(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::And(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 5, end: 6 },
                            name: "b".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 11, end: 12 },
                            name: "c".to_string()
                        })
                        .into()
                    )
                )
                .into()
            )
        )
    );
}

#[test]
fn parse_less_than_symbol() {
    let source = "a < b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::LessThan(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_less_than_or_equal_symbol() {
    let source = "a <= b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::LessThanOrEqual(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_less_than_or_equal_keyword() {
    let source = "a le b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::LessThanOrEqual(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_greater_than_symbol() {
    let source = "a > b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::GreaterThan(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_greater_than_keyword() {
    let source = "a gt b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::GreaterThan(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_greater_than_or_equal_symbol() {
    let source = "a >= b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::GreaterThanOrEqual(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_greater_than_or_equal_keyword() {
    let source = "a ge b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::GreaterThanOrEqual(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_not_equal_keyword() {
    let source = "a ne b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::NotEqual(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_equal_keyword() {
    let source = "a eq b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Equal(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 1 },
                    name: "a".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_not_expression() {
    let source = "not a";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Not(Box::new(
            ExpressionKind::Identifier(Identifier {
                span: Span { start: 4, end: 5 },
                name: "a".to_string()
            })
            .into()
        ))
    );
}

#[test]
fn parse_not_with_comparison() {
    // In ABL, NOT has higher precedence than comparison operators
    // so "not a = b" is parsed as "(not a) = b"
    let source = "not a = b";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Equal(
            Box::new(
                ExpressionKind::Not(Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 4, end: 5 },
                        name: "a".to_string()
                    })
                    .into()
                ))
                .into()
            ),
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 8, end: 9 },
                    name: "b".to_string()
                })
                .into()
            )
        )
    );
}

#[test]
fn parse_not_with_parenthesized_comparison() {
    // To negate an entire comparison, use parentheses
    let source = "not (a = b)";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Not(Box::new(
            ExpressionKind::Equal(
                Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 5, end: 6 },
                        name: "a".to_string()
                    })
                    .into()
                ),
                Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 9, end: 10 },
                        name: "b".to_string()
                    })
                    .into()
                )
            )
            .into()
        ))
    );
}

#[test]
fn parse_matches_expression() {
    let source = "name matches \"*son\"";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Matches(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 4 },
                    name: "name".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::String(StringLiteral {
                    span: Span { start: 13, end: 19 },
                    value: "*son".to_string()
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_simple_ternary() {
    // IF xx > 5 THEN 10 ELSE 20
    let source = "if xx > 5 then 10 else 20";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::IfThenElse(
            Box::new(
                ExpressionKind::GreaterThan(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 3, end: 5 },
                            name: "xx".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 8, end: 9 },
                            value: 5
                        }))
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 15, end: 17 },
                    value: 10
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 23, end: 25 },
                    value: 20
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_nested_ternary_in_else() {
    // IF xx > 10 THEN 1 ELSE IF xx > 5 THEN 2 ELSE 3
    let source = "if xx > 10 then 1 else if xx > 5 then 2 else 3";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::IfThenElse(
            Box::new(
                ExpressionKind::GreaterThan(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 3, end: 5 },
                            name: "xx".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 8, end: 10 },
                            value: 10
                        }))
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 16, end: 17 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::IfThenElse(
                    Box::new(
                        ExpressionKind::GreaterThan(
                            Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 26, end: 28 },
                                    name: "xx".to_string()
                                })
                                .into()
                            ),
                            Box::new(
                                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                                    span: Span { start: 31, end: 32 },
                                    value: 5
                                }))
                                .into()
                            )
                        )
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 38, end: 39 },
                            value: 2
                        }))
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 45, end: 46 },
                            value: 3
                        }))
                        .into()
                    )
                )
                .into()
            )
        )
    );
}

#[test]
fn parse_ternary_with_complex_condition() {
    // IF a = b AND c > d THEN 1 ELSE 0
    let source = "if a = b and c > d then 1 else 0";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::IfThenElse(
            Box::new(
                ExpressionKind::And(
                    Box::new(
                        ExpressionKind::Equal(
                            Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 3, end: 4 },
                                    name: "a".to_string()
                                })
                                .into()
                            ),
                            Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 7, end: 8 },
                                    name: "b".to_string()
                                })
                                .into()
                            )
                        )
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::GreaterThan(
                            Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 13, end: 14 },
                                    name: "c".to_string()
                                })
                                .into()
                            ),
                            Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 17, end: 18 },
                                    name: "d".to_string()
                                })
                                .into()
                            )
                        )
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 24, end: 25 },
                    value: 1
                }))
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 31, end: 32 },
                    value: 0
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_ternary_with_expressions_in_branches() {
    // IF cond THEN xx + yy ELSE xx - yy
    let source = "if cond then xx + yy else xx - yy";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::IfThenElse(
            Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 3, end: 7 },
                    name: "cond".to_string()
                })
                .into()
            ),
            Box::new(
                ExpressionKind::Add(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 13, end: 15 },
                            name: "xx".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 18, end: 20 },
                            name: "yy".to_string()
                        })
                        .into()
                    )
                )
                .into()
            ),
            Box::new(
                ExpressionKind::Minus(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 26, end: 28 },
                            name: "xx".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 31, end: 33 },
                            name: "yy".to_string()
                        })
                        .into()
                    )
                )
                .into()
            )
        )
    );
}

#[test]
fn parse_function_call_no_args() {
    let source = "NOW()";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 3 },
                name: "NOW".to_string()
            },
            arguments: vec![]
        }
    );
}

#[test]
fn parse_function_call_1_arg() {
    let source = "TRIM(name)";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 4 },
                name: "TRIM".to_string()
            },
            arguments: vec![
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 5, end: 9 },
                    name: "name".to_string()
                })
                .into()
            ]
        }
    );
}

#[test]
fn parse_function_call_multiple_args() {
    let source = "SUBSTRING(str, 1, 5)";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 9 },
                name: "SUBSTRING".to_string()
            },
            arguments: vec![
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 10, end: 13 },
                    name: "str".to_string()
                })
                .into(),
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 15, end: 16 },
                    value: 1
                }))
                .into(),
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 18, end: 19 },
                    value: 5
                }))
                .into()
            ]
        }
    );
}

#[test]
fn parse_function_call_with_expression_arg() {
    let source = "ABS(x - 5)";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 3 },
                name: "ABS".to_string()
            },
            arguments: vec![
                ExpressionKind::Minus(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 4, end: 5 },
                            name: "x".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 8, end: 9 },
                            value: 5
                        }))
                        .into()
                    )
                )
                .into()
            ]
        }
    );
}

#[test]
fn parse_dynamic_func_abbreviation_with_in_handle() {
    // ADE tty/set emits DYNAMIC-FUNC (abbrev of DYNAMIC-FUNCTION). #65 round 4.
    let source = r#"DYNAMIC-FUNC("setDataSourceEvents":U IN TARGET-PROCEDURE, evtList)"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser
        .parse_expression()
        .expect("DYNAMIC-FUNC … IN handle must parse");
    match expression.kind {
        ExpressionKind::FunctionCall { name, arguments } => {
            assert!(
                name.name.eq_ignore_ascii_case("dynamic-func")
                    || name.name.eq_ignore_ascii_case("dynamic-function"),
                "name={}",
                name.name
            );
            assert!(
                arguments.len() >= 2,
                "name + handle (+ args), got {}",
                arguments.len()
            );
        }
        other => panic!("Expected FunctionCall, got {other:?}"),
    }
}

#[test]
fn parse_nested_function_calls() {
    let source = "TRIM(SUBSTRING(name, 1, 5))";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 4 },
                name: "TRIM".to_string()
            },
            arguments: vec![
                ExpressionKind::FunctionCall {
                    name: Identifier {
                        span: Span { start: 5, end: 14 },
                        name: "SUBSTRING".to_string()
                    },
                    arguments: vec![
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 15, end: 19 },
                            name: "name".to_string()
                        })
                        .into(),
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 21, end: 22 },
                            value: 1
                        }))
                        .into(),
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 24, end: 25 },
                            value: 5
                        }))
                        .into()
                    ]
                }
                .into()
            ]
        }
    );
}

#[test]
fn parse_function_in_expression() {
    let source = "LENGTH(str) + 1";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::FunctionCall {
                    name: Identifier {
                        span: Span { start: 0, end: 6 },
                        name: "LENGTH".to_string()
                    },
                    arguments: vec![
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 7, end: 10 },
                            name: "str".to_string()
                        })
                        .into()
                    ]
                }
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 1
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_simple_array_access() {
    let source = "arr[1]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "arr".to_string()
                })
                .into()
            ),
            index: Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 1
                }))
                .into()
            )
        }
    )
}

#[test]
fn parse_array_access_with_expression_index() {
    let source = "arr[i + 1]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "arr".to_string()
                })
                .into()
            ),
            index: Box::new(
                ExpressionKind::Add(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 4, end: 5 },
                            name: "i".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 8, end: 9 },
                            value: 1
                        }))
                        .into()
                    )
                )
                .into()
            )
        }
    );
}

#[test]
fn parse_hyphenated_array_access() {
    let source = "month-quota[3]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 11 },
                    name: "month-quota".to_string()
                })
                .into()
            ),
            index: Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 12, end: 13 },
                    value: 3
                }))
                .into()
            )
        }
    );
}

#[test]
fn parse_multidimensional_array() {
    let source = "matrix[row][col]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::ArrayAccess {
                    array: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 6 },
                            name: "matrix".to_string()
                        })
                        .into()
                    ),
                    index: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 7, end: 10 },
                            name: "row".to_string()
                        })
                        .into()
                    )
                }
                .into()
            ),
            index: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 12, end: 15 },
                    name: "col".to_string()
                })
                .into()
            )
        }
    );
}

#[test]
fn parse_array_in_arithmetic() {
    let source = "arr[1] + arr[2]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::ArrayAccess {
                    array: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 3 },
                            name: "arr".to_string()
                        })
                        .into()
                    ),
                    index: Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 4, end: 5 },
                            value: 1
                        }))
                        .into()
                    )
                }
                .into()
            ),
            Box::new(
                ExpressionKind::ArrayAccess {
                    array: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 9, end: 12 },
                            name: "arr".to_string()
                        })
                        .into()
                    ),
                    index: Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 13, end: 14 },
                            value: 2
                        }))
                        .into()
                    )
                }
                .into()
            )
        )
    );
}

#[test]
fn parse_simple_member_access() {
    let source = "handle:PRIVATE-DATA";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::MemberAccess {
            object: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "handle".to_string()
                })
                .into()
            ),
            member: Identifier {
                span: Span { start: 7, end: 19 },
                name: "PRIVATE-DATA".to_string()
            }
        }
    );
}

#[test]
fn parse_simple_method_call() {
    let source = "buffer:FIND-FIRST()";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::MethodCall {
            object: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "buffer".to_string()
                })
                .into()
            ),
            method: Identifier {
                span: Span { start: 7, end: 17 },
                name: "FIND-FIRST".to_string()
            },
            arguments: vec![]
        }
    );
}

#[test]
fn parse_method_call_with_args() {
    let source = "widget:MOVE(10, 20)";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::MethodCall {
            object: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "widget".to_string()
                })
                .into()
            ),
            method: Identifier {
                span: Span { start: 7, end: 11 },
                name: "MOVE".to_string()
            },
            arguments: vec![
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 12, end: 14 },
                    value: 10
                }))
                .into(),
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 16, end: 18 },
                    value: 20
                }))
                .into()
            ]
        }
    );
}

#[test]
fn parse_chained_member_access() {
    let source = "obj:property:nested";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::MemberAccess {
            object: Box::new(
                ExpressionKind::MemberAccess {
                    object: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 3 },
                            name: "obj".to_string()
                        })
                        .into()
                    ),
                    member: Identifier {
                        span: Span { start: 4, end: 12 },
                        name: "property".to_string()
                    }
                }
                .into()
            ),
            member: Identifier {
                span: Span { start: 13, end: 19 },
                name: "nested".to_string()
            }
        }
    );
}

#[test]
fn parse_member_then_method() {
    let source = "obj:prop:method()";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::MethodCall {
            object: Box::new(
                ExpressionKind::MemberAccess {
                    object: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 3 },
                            name: "obj".to_string()
                        })
                        .into()
                    ),
                    member: Identifier {
                        span: Span { start: 4, end: 8 },
                        name: "prop".to_string()
                    }
                }
                .into()
            ),
            method: Identifier {
                span: Span { start: 9, end: 15 },
                name: "method".to_string()
            },
            arguments: vec![]
        }
    );
}

#[test]
fn parse_member_access_with_array() {
    let source = "myBuf:myField[1]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::MemberAccess {
                    object: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 5 },
                            name: "myBuf".to_string()
                        })
                        .into()
                    ),
                    member: Identifier {
                        span: Span { start: 6, end: 13 },
                        name: "myField".to_string()
                    }
                }
                .into()
            ),
            index: Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 1
                }))
                .into()
            )
        }
    );
}

#[test]
fn parse_member_in_expression() {
    let source = "myObj:myVal + 5";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::Add(
            Box::new(
                ExpressionKind::MemberAccess {
                    object: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 5 },
                            name: "myObj".to_string()
                        })
                        .into()
                    ),
                    member: Identifier {
                        span: Span { start: 6, end: 11 },
                        name: "myVal".to_string()
                    }
                }
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 5
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_simple_field_access() {
    let source = "Customer.CustNum";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FieldAccess {
            qualifier: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 0, end: 8 },
                    name: "Customer".to_string()
                })
                .into()
            ),
            field: Identifier {
                span: Span { start: 9, end: 16 },
                name: "CustNum".to_string()
            }
        }
    );
}

#[test]
fn parse_qualified_field_access() {
    let source = "Sports2020.Customer.Name";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::FieldAccess {
            qualifier: Box::new(
                ExpressionKind::FieldAccess {
                    qualifier: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 10 },
                            name: "Sports2020".to_string()
                        })
                        .into()
                    ),
                    field: Identifier {
                        span: Span { start: 11, end: 19 },
                        name: "Customer".to_string()
                    }
                }
                .into()
            ),
            field: Identifier {
                span: Span { start: 20, end: 24 },
                name: "Name".to_string()
            }
        }
    );
}

#[test]
fn parse_field_access_with_array() {
    let source = "myTable.myField[extent]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::FieldAccess {
                    qualifier: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 7 },
                            name: "myTable".to_string()
                        })
                        .into()
                    ),
                    field: Identifier {
                        span: Span { start: 8, end: 15 },
                        name: "myField".to_string()
                    }
                }
                .into()
            ),
            index: Box::new(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 16, end: 22 },
                    name: "extent".to_string()
                })
                .into()
            )
        }
    );
}

#[test]
fn parse_field_in_comparison() {
    let source = "Customer.Balance > 1000";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::GreaterThan(
            Box::new(
                ExpressionKind::FieldAccess {
                    qualifier: Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 0, end: 8 },
                            name: "Customer".to_string()
                        })
                        .into()
                    ),
                    field: Identifier {
                        span: Span { start: 9, end: 16 },
                        name: "Balance".to_string()
                    }
                }
                .into()
            ),
            Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 19, end: 23 },
                    value: 1000
                }))
                .into()
            )
        )
    );
}

#[test]
fn parse_mixed_access_operators() {
    // Combines field access, member access, and array indexing
    let source = "myDb.myTable:buffer[1]";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        ExpressionKind::ArrayAccess {
            array: Box::new(
                ExpressionKind::MemberAccess {
                    object: Box::new(
                        ExpressionKind::FieldAccess {
                            qualifier: Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 0, end: 4 },
                                    name: "myDb".to_string()
                                })
                                .into()
                            ),
                            field: Identifier {
                                span: Span { start: 5, end: 12 },
                                name: "myTable".to_string()
                            }
                        }
                        .into()
                    ),
                    member: Identifier {
                        span: Span { start: 13, end: 19 },
                        name: "buffer".to_string()
                    }
                }
                .into()
            ),
            index: Box::new(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 20, end: 21 },
                    value: 1
                }))
                .into()
            )
        }
    );
}

#[test]
fn parse_simple_assignment() {
    let source = "x = 5.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Assignment {
            target: ExpressionKind::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "x".to_string()
            })
            .into(),
            value: ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 5
            }))
            .into()
        }
    );
}

#[test]
fn parse_assignment_with_expression() {
    let source = "total = price * quantity.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Assignment {
            target: ExpressionKind::Identifier(Identifier {
                span: Span { start: 0, end: 5 },
                name: "total".to_string()
            })
            .into(),
            value: ExpressionKind::Multiply(
                Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 8, end: 13 },
                        name: "price".to_string()
                    })
                    .into()
                ),
                Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 16, end: 24 },
                        name: "quantity".to_string()
                    })
                    .into()
                )
            )
            .into()
        }
    );
}

#[test]
fn parse_array_assignment() {
    let source = "arr[1] = 10.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Assignment {
            target: ExpressionKind::ArrayAccess {
                array: Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 0, end: 3 },
                        name: "arr".to_string()
                    })
                    .into()
                ),
                index: Box::new(
                    ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 4, end: 5 },
                        value: 1
                    }))
                    .into()
                )
            }
            .into(),
            value: ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 9, end: 11 },
                value: 10
            }))
            .into()
        }
    );
}

#[test]
fn parse_field_assignment() {
    let source = "Customer.Name = \"John\".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Assignment {
            target: ExpressionKind::FieldAccess {
                qualifier: Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 0, end: 8 },
                        name: "Customer".to_string()
                    })
                    .into()
                ),
                field: Identifier {
                    span: Span { start: 9, end: 13 },
                    name: "Name".to_string()
                }
            }
            .into(),
            value: ExpressionKind::Literal(Literal::String(StringLiteral {
                span: Span { start: 16, end: 22 },
                value: "John".to_string()
            }))
            .into()
        }
    );
}

#[test]
fn parse_expression_statement() {
    let source = "calculateTotals().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::ExpressionStatement(
            ExpressionKind::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 15 },
                    name: "calculateTotals".to_string()
                },
                arguments: vec![]
            }
            .into()
        )
    );
}

#[test]
fn parse_method_call_statement() {
    let source = "buffer:save-row-changes().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::ExpressionStatement(
            ExpressionKind::MethodCall {
                object: Box::new(
                    ExpressionKind::Identifier(Identifier {
                        span: Span { start: 0, end: 6 },
                        name: "buffer".to_string()
                    })
                    .into()
                ),
                method: Identifier {
                    span: Span { start: 7, end: 23 },
                    name: "save-row-changes".to_string()
                },
                arguments: vec![]
            }
            .into()
        )
    );
}

#[test]
fn parse_empty_statement() {
    let source = ".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, StatementKind::Empty);
}

#[test]
fn parse_multiple_statements() {
    let source = "x = 1. y = 2. z = x + y.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmts = parser.parse_statements().expect("Expected statements");
    assert_eq!(stmts.len(), 3);
}

#[test]
fn parse_define_variable_simple() {
    let source = "DEFINE VARIABLE myVar AS INTEGER NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::VariableDeclaration {
            name: Identifier {
                span: Span { start: 16, end: 21 },
                name: "myVar".to_string()
            },
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: true,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        }
    );
}

#[test]
fn parse_define_variable_with_initial() {
    let source = "DEFINE VARIABLE counter AS INTEGER INITIAL 0.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::VariableDeclaration {
            name: Identifier {
                span: Span { start: 16, end: 23 },
                name: "counter".to_string()
            },
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: Some(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 43, end: 44 },
                    value: 0
                }))
                .into()
            ),
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        }
    );
}

#[test]
fn parse_define_variable_character() {
    let source = "DEFINE VARIABLE name AS CHARACTER NO-UNDO INITIAL \"unknown\".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            type_source,
            no_undo,
            initial_value,
            ..
        } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Character));
            assert!(no_undo);
            assert!(initial_value.is_some());
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_var_statement_simple() {
    let source = "VAR INTEGER myCount.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::VariableDeclaration {
            name: Identifier {
                span: Span { start: 12, end: 19 },
                name: "myCount".to_string()
            },
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: true,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        }
    );
}

#[test]
fn parse_var_statement_with_initial() {
    let source = "VAR DECIMAL total = 100.50.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name,
            type_source,
            initial_value,
            no_undo,
            ..
        } => {
            assert_eq!(name.name, "total");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Decimal));
            assert!(no_undo);
            assert!(initial_value.is_some());
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_var_logical() {
    let source = "VAR LOGICAL isActive = TRUE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            type_source,
            initial_value,
            ..
        } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Logical));
            assert!(matches!(
                initial_value.as_ref().map(|e| &e.kind),
                Some(ExpressionKind::Literal(Literal::Boolean(_)))
            ));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

// ===================== LIKE syntax tests =====================

#[test]
fn parse_define_variable_like() {
    let source = "DEFINE VARIABLE v LIKE Customer.CustName NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name,
            type_source,
            no_undo,
            ..
        } => {
            assert_eq!(name.name, "v");
            assert_eq!(
                type_source,
                TypeSource::Like {
                    source: Identifier {
                        name: "Customer.CustName".to_string(),
                        span: Span { start: 23, end: 40 }
                    }
                }
            );
            assert!(no_undo);
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_define_variable_like_simple_name() {
    let source = "DEFINE VARIABLE v LIKE iVar NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration { type_source, .. } => match type_source {
            TypeSource::Like { source } => assert_eq!(source.name, "iVar"),
            _ => panic!("Expected Like"),
        },
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_define_variable_like_with_initial() {
    // LIKE + INITIAL override: verify LIKE and INITIAL can coexist
    let source = "DEFINE VARIABLE v LIKE Customer.Balance INITIAL 0 NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            type_source,
            initial_value,
            no_undo,
            ..
        } => {
            assert!(matches!(type_source, TypeSource::Like { .. }));
            assert!(initial_value.is_some());
            assert!(no_undo);
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_define_variable_decimal_decimals_extent() {
    let source = "Def var rate-adj as decimal decimals 2 extent 5.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name,
            type_source,
            extent,
            ..
        } => {
            assert_eq!(name.name, "rate-adj");
            assert!(matches!(
                type_source,
                TypeSource::Explicit(DataType::Decimal)
            ));
            assert_eq!(extent, Some(5));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_var_like() {
    let source = "VAR LIKE Customer.CustName vCust.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            type_source,
            name,
            no_undo,
            ..
        } => {
            assert_eq!(name.name, "vCust");
            assert!(matches!(type_source, TypeSource::Like { .. }));
            assert!(no_undo); // VAR implies NO-UNDO
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_define_input_parameter_like() {
    let source = "DEFINE INPUT PARAMETER p LIKE Customer.CustName NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                },
        } => {
            assert_eq!(direction, ParameterDirection::Input);
            assert_eq!(name.name, "p");
            assert!(matches!(type_source, TypeSource::Like { .. }));
            assert!(no_undo);
        }
        _ => panic!("Expected DefineParameter"),
    }
}

#[test]
fn parse_define_output_parameter_like() {
    let source = "DEFINE OUTPUT PARAMETER p LIKE Order.Amount NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type: ParameterType::Variable { type_source, .. },
        } => {
            assert_eq!(direction, ParameterDirection::Output);
            assert!(matches!(type_source, TypeSource::Like { .. }));
        }
        _ => panic!("Expected DefineParameter"),
    }
}

#[test]
fn parse_define_input_output_parameter_like() {
    let source = "DEFINE INPUT-OUTPUT PARAMETER p LIKE Invoice.InvNum NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type: ParameterType::Variable { type_source, .. },
        } => {
            assert_eq!(direction, ParameterDirection::InputOutput);
            assert!(matches!(type_source, TypeSource::Like { .. }));
        }
        _ => panic!("Expected DefineParameter"),
    }
}

#[test]
fn parse_temp_table_field_like_validate_regression() {
    // After moving validate out of TypeSource::Like onto TempTableField,
    // verify that VALIDATE is still correctly parsed and stored.
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD f LIKE Customer.CustName VALIDATE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields.len(), 1);
            assert!(
                matches!(&fields[0].type_source, TypeSource::Like { .. }),
                "Expected Like type source"
            );
            assert!(fields[0].validate, "Expected validate = true");
        }
        _ => panic!("Expected DefineTempTable"),
    }
}

#[test]
fn parse_simple_do_block() {
    let source = "DO: x = 1. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, loop_var, .. } => {
            assert!(loop_var.is_none());
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_do_counting_loop() {
    let source = "DO i = 1 TO 10: x = i. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do {
            loop_var,
            from,
            to,
            body,
            ..
        } => {
            assert!(loop_var.is_some());
            assert_eq!(loop_var.unwrap().name, "i");
            assert!(from.is_some());
            assert!(to.is_some());
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_do_with_by() {
    let source = "DO i = 0 TO 100 BY 10: total = total + i. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { by, .. } => {
            assert!(by.is_some());
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_do_while() {
    let source = "DO WHILE x < 10: x = x + 1. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do {
            while_condition, ..
        } => {
            assert!(while_condition.is_some());
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_nested_do_blocks() {
    let source = "DO i = 1 TO 3: DO j = 1 TO 3: x = i * j. END. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::Do { .. }));
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_if_then_simple() {
    let source = "IF x > 0 THEN y = 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert!(matches!(condition.kind, ExpressionKind::GreaterThan(_, _)));
            assert!(matches!(then_branch.kind, StatementKind::Assignment { .. }));
            assert!(else_branch.is_none());
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_if_then_else() {
    let source = "IF x > 0 THEN y = 1. ELSE y = 0.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If { else_branch, .. } => {
            assert!(else_branch.is_some());
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_if_then_do_block() {
    let source = "IF x > 0 THEN DO: y = 1. z = 2. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If { then_branch, .. } => {
            assert!(matches!(then_branch.kind, StatementKind::Do { .. }));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_if_then_do_end_else_do_end() {
    // Legacy ABL idiom: the `.` ends the inner DO's END statement, but the
    // surrounding IF scope stays open and binds the following ELSE.
    let source = "IF x > 0 THEN DO: y = 1. END. ELSE DO: y = 0. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            assert!(matches!(then_branch.kind, StatementKind::Do { .. }));
            let else_stmt = else_branch.expect("Should have else");
            assert!(matches!(else_stmt.kind, StatementKind::Do { .. }));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn if_do_branch_spans_are_full_extent_not_dummy() {
    // Regression: a DO-block IF branch is parsed via `parse_do_statement`
    // directly, bypassing the `parse_statement` span funnel. Before the fix it
    // kept `Span::DUMMY` (0..0), which made every span-consuming pass treat the
    // branch as living at byte 0 (the formatter then mis-indented unrelated
    // leading content). Both branch spans must cover their real `DO:…END` text.
    let source = "IF x THEN DO: y = 1. END. ELSE DO: y = 0. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    let StatementKind::If {
        then_branch,
        else_branch,
        ..
    } = stmt.kind
    else {
        panic!("Expected If statement");
    };

    // THEN branch: `DO: y = 1. END.`
    assert!(matches!(then_branch.kind, StatementKind::Do { .. }));
    assert_ne!(
        (then_branch.span.start, then_branch.span.end),
        (0, 0),
        "THEN branch span must not be the dummy 0..0"
    );
    assert_eq!(
        &source[then_branch.span.start as usize..then_branch.span.end as usize],
        "DO: y = 1. END.",
        "THEN branch span must cover its full DO…END extent"
    );

    // ELSE branch: `DO: y = 0. END.`
    let else_stmt = else_branch.expect("Should have else");
    assert!(matches!(else_stmt.kind, StatementKind::Do { .. }));
    assert_ne!(
        (else_stmt.span.start, else_stmt.span.end),
        (0, 0),
        "ELSE branch span must not be the dummy 0..0"
    );
    assert_eq!(
        &source[else_stmt.span.start as usize..else_stmt.span.end as usize],
        "DO: y = 0. END.",
        "ELSE branch span must cover its full DO…END extent"
    );
}

#[test]
fn parse_next_prompt_statement() {
    let source = "NEXT-PROMPT ord-proc.hold-flag WITH FRAME shared/order-entry.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("NEXT-PROMPT should parse as a skipped legacy UI statement");
    // Recognized-but-unmodelled, so `Skipped` rather than a recovery `Empty`.
    // Both halves of the adjacency filter are exercised here: the field half of
    // `ord-proc.hold-flag` and the tail of the `shared/order-entry` path drop
    // out, their qualifiers survive.
    assert_eq!(skipped_names(&stmt), vec!["ord-proc", "FRAME", "shared"]);
}

#[test]
fn parse_if_then_do_else_with_preproc_placeholder() {
    // An undefined preprocessor variable placeholder appears between `End.`
    // and `Else`. The placeholder
    // would normally expand to additional `Else If` branches; in raw
    // (unpreprocessed) source it lexes as `{&name}`. The parser must look past
    // it to bind ELSE.
    let source = "\
        IF a THEN DO: x = 1. END.\n\
        {&extra-else-branches}\n\
        ELSE DO: x = 2. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If { else_branch, .. } => {
            assert!(
                else_branch.is_some(),
                "ELSE must bind across undefined preprocessor placeholder"
            );
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_if_then_do_end_else_do_end_nested_in_procedure() {
    // Same pattern but inside a PROCEDURE body.
    let source = "\
        PROCEDURE p:\n\
            IF x > 0 THEN DO:\n\
                y = 1.\n\
            END.\n\
            ELSE DO:\n\
                y = 0.\n\
            END.\n\
        END PROCEDURE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Procedure { body, .. } => {
            assert_eq!(body.len(), 1, "procedure body: {:?}", body);
            match &body[0].kind {
                StatementKind::If { else_branch, .. } => {
                    assert!(else_branch.is_some(), "else arm must bind to IF");
                }
                other => panic!("Expected If, got {:?}", other),
            }
        }
        _ => panic!("Expected Procedure statement"),
    }
}

#[test]
fn parse_if_else_if_chain() {
    let source = "IF x > 10 THEN y = 3. ELSE IF x > 5 THEN y = 2. ELSE y = 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If { else_branch, .. } => {
            let else_stmt = else_branch.expect("Should have else");
            assert!(matches!(else_stmt.kind, StatementKind::If { .. }));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_nested_if() {
    let source = "IF a THEN IF b THEN x = 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::If { then_branch, .. } => {
            assert!(matches!(then_branch.kind, StatementKind::If { .. }));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_simple_repeat() {
    let source = "REPEAT: x = x + 1. IF x > 10 THEN LEAVE. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Repeat {
            body,
            while_condition,
            ..
        } => {
            assert!(while_condition.is_none());
            assert_eq!(body.len(), 2);
        }
        _ => panic!("Expected Repeat statement"),
    }
}

#[test]
fn parse_repeat_while() {
    let source = "REPEAT WHILE x < 100: x = x * 2. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Repeat {
            while_condition, ..
        } => {
            assert!(while_condition.is_some());
        }
        _ => panic!("Expected Repeat statement"),
    }
}

#[test]
fn parse_leave_statement() {
    let source = "LEAVE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, StatementKind::Leave(None));
}

#[test]
fn parse_next_statement() {
    let source = "NEXT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, StatementKind::Next(None));
}

#[test]
fn parse_return_no_value() {
    let source = "RETURN.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, StatementKind::Return(None));
}

#[test]
fn parse_return_with_value() {
    let source = "RETURN x + 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Return(Some(expr)) => {
            assert!(matches!(expr.kind, ExpressionKind::Add(_, _)));
        }
        _ => panic!("Expected Return with value"),
    }
}

#[test]
fn parse_return_no_apply() {
    // UI-trigger return keyword — must not leave NO-APPLY as an expression.
    let source = "RETURN NO-APPLY.";
    let tokens = tokenize(source);
    assert!(
        tokens.iter().any(|t| t.kind == oxabl_lexer::Kind::NoApply),
        "NO-APPLY must lex as Kind::NoApply, got {:?}",
        tokens.iter().map(|t| t.kind).collect::<Vec<_>>()
    );
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt.kind, StatementKind::Return(None));
}

#[test]
fn parse_undo_return_no_apply() {
    for source in ["UNDO, RETURN NO-APPLY.", "UNDO lbl, RETURN NO-APPLY."] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let program = parser.parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
    }
}

#[test]
fn parse_throw_forms_keep_the_thrown_expression() {
    for source in [
        "THROW NEW Err().",
        "UNDO, THROW NEW Err().",
        "RETURN ERROR NEW Err().",
        "UNDO, RETURN ERROR NEW Err().",
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        assert!(
            matches!(&stmt.kind, StatementKind::Throw(e) if matches!(e.kind, ExpressionKind::New { .. })),
            "{source}: {:?}",
            stmt.kind
        );
    }
}

#[test]
fn parse_return_error_without_value_is_a_bare_return() {
    let source = "RETURN ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt.kind, StatementKind::Return(None));
}

#[test]
fn run_target_stops_at_whitespace_and_comments() {
    for (source, expected) in [
        ("RUN util/export PERSISTENT SET h (1).", "util/export"),
        ("RUN util/export\n  PERSISTENT SET h.", "util/export"),
        ("RUN sub2 /* c */.", "sub2"),
        ("RUN sub2 // c\n.", "sub2"),
        ("RUN sub2.p /* c */.", "sub2.p"),
        ("RUN sub2.\ni = 1.", "sub2"),
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt.kind {
            StatementKind::Run {
                target: RunTarget::Literal { name, .. },
                ..
            } => assert_eq!(name, expected, "{source}"),
            other => panic!("{source}: {other:?}"),
        }
    }
}

#[test]
fn a_keyword_segment_is_a_field_access() {
    let source = "x = pkg.Color:Red.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    let StatementKind::Assignment { value, .. } = stmt.kind else {
        panic!("{:?}", stmt.kind);
    };
    let ExpressionKind::MemberAccess { object, .. } = value.kind else {
        panic!("{:?}", value.kind);
    };
    assert!(matches!(object.kind, ExpressionKind::FieldAccess { .. }));
}

#[test]
fn parse_loop_with_leave_and_next() {
    let source = "DO i = 1 TO 100: IF l_done THEN LEAVE. IF l_skip THEN NEXT. l_process(i). END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 3);
        }
        _ => panic!("Expected Do statement"),
    }
}

// ==================== FOR EACH Tests ====================

#[test]
fn parse_simple_for_each() {
    let source = "FOR EACH Customer NO-LOCK: x = Customer.Name. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach {
            buffer,
            of_relation,
            where_clause,
            lock_type,
            body,
            ..
        } => {
            assert_eq!(buffer.name, "Customer");
            assert!(of_relation.is_none());
            assert!(where_clause.is_none());
            assert_eq!(lock_type, LockType::NoLock);
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_where() {
    let source = "FOR EACH Customer WHERE Customer.Balance > 1000 NO-LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach {
            buffer,
            where_clause,
            lock_type,
            ..
        } => {
            assert_eq!(buffer.name, "Customer");
            assert!(where_clause.is_some());
            assert!(matches!(
                where_clause.unwrap().kind,
                ExpressionKind::GreaterThan(_, _)
            ));
            assert_eq!(lock_type, LockType::NoLock);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_of() {
    let source = "FOR EACH Order OF Customer NO-LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach {
            buffer,
            of_relation,
            lock_type,
            ..
        } => {
            assert_eq!(buffer.name, "Order");
            assert!(of_relation.is_some());
            assert_eq!(of_relation.unwrap().name, "Customer");
            assert_eq!(lock_type, LockType::NoLock);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_share_lock() {
    let source = "FOR EACH Customer SHARE-LOCK: x = 1. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { lock_type, .. } => {
            assert_eq!(lock_type, LockType::ShareLock);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_exclusive_lock() {
    let source = "FOR EACH Customer EXCLUSIVE-LOCK: Customer.Balance = 0. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach {
            lock_type, body, ..
        } => {
            assert_eq!(lock_type, LockType::ExclusiveLock);
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_default_lock() {
    // When no lock type is specified, default is SHARE-LOCK
    let source = "FOR EACH Customer: x = 1. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { lock_type, .. } => {
            assert_eq!(lock_type, LockType::ShareLock);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_of_and_where() {
    let source = "FOR EACH OrderLine OF Order WHERE OrderLine.Qty > 0 NO-LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach {
            buffer,
            of_relation,
            where_clause,
            lock_type,
            ..
        } => {
            assert_eq!(buffer.name, "OrderLine");
            assert!(of_relation.is_some());
            assert_eq!(of_relation.unwrap().name, "Order");
            assert!(where_clause.is_some());
            assert_eq!(lock_type, LockType::NoLock);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_complex_where() {
    let source =
        "FOR EACH Customer WHERE Customer.Balance > 1000 AND Customer.Active NO-LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { where_clause, .. } => {
            assert!(where_clause.is_some());
            assert!(matches!(
                where_clause.unwrap().kind,
                ExpressionKind::And(_, _)
            ));
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_multiple_statements() {
    // Use simple assignments to avoid the period ambiguity (field access vs statement terminator)
    let source = "FOR EACH Customer NO-LOCK: x = 1. y = 2. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { body, .. } => {
            assert_eq!(body.len(), 2);
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_nested_for_each() {
    let source = "FOR EACH Customer NO-LOCK: FOR EACH Order OF Customer NO-LOCK: END. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::ForEach { .. }));
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_leave() {
    let source = "FOR EACH Customer NO-LOCK: IF Customer.Balance < 0 THEN LEAVE. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::If { .. }));
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_for_each_with_field_access_in_expression() {
    // Field access works when not immediately before statement-terminating period
    let source = "FOR EACH Customer NO-LOCK: total = total + Customer.Balance. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::ForEach { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0].kind {
                StatementKind::Assignment { value, .. } => {
                    assert!(matches!(value.kind, ExpressionKind::Add(_, _)));
                }
                _ => panic!("Expected Assignment statement"),
            }
        }
        _ => panic!("Expected ForEach statement"),
    }
}

#[test]
fn parse_simple_for_each_no_lock_space() {
    // Full verification of parsed structure
    let source = "FOR EACH Customer NO LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::ForEach {
            buffer: Identifier {
                span: Span { start: 9, end: 17 },
                name: "Customer".to_string()
            },
            of_relation: None,
            where_clause: None,
            lock_type: LockType::NoLock,
            on_actions: Vec::new(),
            body: vec![]
        }
    );
}

#[test]
fn parse_simple_for_each_share_lock_space() {
    // Full verification of parsed structure
    let source = "FOR EACH Customer SHARE LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::ForEach {
            buffer: Identifier {
                span: Span { start: 9, end: 17 },
                name: "Customer".to_string()
            },
            of_relation: None,
            where_clause: None,
            lock_type: LockType::ShareLock,
            on_actions: Vec::new(),
            body: vec![]
        }
    );
}

#[test]
fn parse_simple_for_each_exclusive_lock_space() {
    // Full verification of parsed structure
    let source = "FOR EACH Customer EXCLUSIVE LOCK: END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::ForEach {
            buffer: Identifier {
                span: Span { start: 9, end: 17 },
                name: "Customer".to_string()
            },
            of_relation: None,
            where_clause: None,
            lock_type: LockType::ExclusiveLock,
            on_actions: Vec::new(),
            body: vec![]
        }
    );
}

#[test]
fn parse_find_first_with_where() {
    let source = "find first Customer where Customer.CustNum = 1 no-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Find {
            find_type: FindType::First,
            buffer: Identifier {
                span: Span { start: 11, end: 19 },
                name: "Customer".to_string()
            },
            key_value: None,
            where_clause: Some(
                ExpressionKind::Equal(
                    Box::new(
                        ExpressionKind::FieldAccess {
                            qualifier: Box::new(
                                ExpressionKind::Identifier(Identifier {
                                    span: Span { start: 26, end: 34 },
                                    name: "Customer".to_string()
                                })
                                .into()
                            ),
                            field: Identifier {
                                span: Span { start: 35, end: 42 },
                                name: "CustNum".to_string()
                            }
                        }
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 45, end: 46 },
                            value: 1
                        }))
                        .into()
                    )
                )
                .into()
            ),
            lock_type: LockType::NoLock,
            no_error: false,
        }
    );
}

#[test]
fn parse_find_by_integer_key() {
    let source = "find Customer 1 no-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Find {
            find_type: FindType::Unique,
            buffer: Identifier {
                span: Span { start: 5, end: 13 },
                name: "Customer".to_string()
            },
            key_value: Some(
                ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 14, end: 15 },
                    value: 1
                }))
                .into()
            ),
            where_clause: None,
            lock_type: LockType::NoLock,
            no_error: false,
        }
    );
}

#[test]
fn parse_find_by_string_key() {
    let source = r#"find Customer "ACME" no-lock."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Find {
            find_type: FindType::Unique,
            buffer: Identifier {
                span: Span { start: 5, end: 13 },
                name: "Customer".to_string()
            },
            key_value: Some(
                ExpressionKind::Literal(Literal::String(StringLiteral {
                    span: Span { start: 14, end: 20 },
                    value: "ACME".to_string()
                }))
                .into()
            ),
            where_clause: None,
            lock_type: LockType::NoLock,
            no_error: false,
        }
    );
}

#[test]
fn parse_find_by_variable_key() {
    let source = "find Customer custId no-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Find {
            find_type: FindType::Unique,
            buffer: Identifier {
                span: Span { start: 5, end: 13 },
                name: "Customer".to_string()
            },
            key_value: Some(
                ExpressionKind::Identifier(Identifier {
                    span: Span { start: 14, end: 20 },
                    name: "custId".to_string()
                })
                .into()
            ),
            where_clause: None,
            lock_type: LockType::NoLock,
            no_error: false,
        }
    );
}

#[test]
fn parse_find_with_no_error() {
    let source = "find first Customer where active = true no-lock no-error.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Find {
            find_type: FindType::First,
            buffer: Identifier {
                span: Span { start: 11, end: 19 },
                name: "Customer".to_string()
            },
            key_value: None,
            where_clause: Some(
                ExpressionKind::Equal(
                    Box::new(
                        ExpressionKind::Identifier(Identifier {
                            span: Span { start: 26, end: 32 },
                            name: "active".to_string()
                        })
                        .into()
                    ),
                    Box::new(
                        ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
                            span: Span { start: 35, end: 39 },
                            value: true
                        }))
                        .into()
                    )
                )
                .into()
            ),
            lock_type: LockType::NoLock,
            no_error: true,
        }
    );
}

#[test]
fn parse_find_last() {
    let source = "find last Order no-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find {
            find_type, buffer, ..
        } => {
            assert_eq!(find_type, FindType::Last);
            assert_eq!(buffer.name, "Order");
        }
        _ => panic!("Expected Find statement"),
    }
}

#[test]
fn parse_find_next() {
    let source = "find next Customer no-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find {
            find_type, buffer, ..
        } => {
            assert_eq!(find_type, FindType::Next);
            assert_eq!(buffer.name, "Customer");
        }
        _ => panic!("Expected Find statement"),
    }
}

#[test]
fn parse_find_prev() {
    let source = "find prev Customer no-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find {
            find_type, buffer, ..
        } => {
            assert_eq!(find_type, FindType::Prev);
            assert_eq!(buffer.name, "Customer");
        }
        _ => panic!("Expected Find statement"),
    }
}

#[test]
fn parse_find_exclusive_lock() {
    let source = "find first Customer exclusive-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find { lock_type, .. } => {
            assert_eq!(lock_type, LockType::ExclusiveLock);
        }
        _ => panic!("Expected Find statement"),
    }
}

#[test]
fn parse_find_share_lock() {
    let source = "find first Customer share-lock.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find { lock_type, .. } => {
            assert_eq!(lock_type, LockType::ShareLock);
        }
        _ => panic!("Expected Find statement"),
    }
}

#[test]
fn parse_find_default_lock() {
    // When no lock type is specified, defaults to ShareLock
    let source = "find first Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find { lock_type, .. } => {
            assert_eq!(lock_type, LockType::ShareLock);
        }
        _ => panic!("Expected Find statement"),
    }
}

#[test]
fn parse_find_where_rowid() {
    // ROWID() is a built-in function and must parse in expression position
    let source = "find order where rowid(order) eq v-rowid no-lock no-error.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Find { where_clause, .. } => {
            assert!(where_clause.is_some(), "Expected where clause");
        }
        _ => panic!("Expected Find statement"),
    }
}

// ==================== CASE Statement Tests ====================

#[test]
fn parse_simple_case_statement() {
    let source = "CASE x: WHEN 1 THEN y = 1. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case {
            expression,
            when_branches,
            otherwise,
        } => {
            assert!(matches!(expression.kind, ExpressionKind::Identifier(_)));
            assert_eq!(when_branches.len(), 1);
            assert_eq!(when_branches[0].values.len(), 1);
            assert_eq!(when_branches[0].body.len(), 1);
            assert!(otherwise.is_none());
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_multiple_when_branches() {
    let source =
        "CASE myStatus: WHEN 1 THEN x = 1. WHEN 2 THEN x = 2. WHEN 3 THEN x = 3. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => {
            assert_eq!(when_branches.len(), 3);
            assert!(otherwise.is_none());
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_otherwise() {
    let source = "CASE x: WHEN 1 THEN y = 1. OTHERWISE y = 0. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case {
            when_branches,
            otherwise,
            ..
        } => {
            assert_eq!(when_branches.len(), 1);
            assert!(otherwise.is_some());
            assert_eq!(otherwise.unwrap().len(), 1);
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_or_when() {
    // WHEN "a" OR WHEN "b" syntax for multiple matching values
    let source = r#"CASE letter: WHEN "a" OR WHEN "b" THEN x = 1. END CASE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 1);
            // The single WHEN branch has two values
            assert_eq!(when_branches[0].values.len(), 2);
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_string_values() {
    let source = r#"CASE name: WHEN "John" THEN x = 1. WHEN "Jane" THEN x = 2. END CASE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 2);
            assert!(matches!(
                when_branches[0].values[0].kind,
                ExpressionKind::Literal(Literal::String(_))
            ));
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_multiple_statements_in_when() {
    let source = "CASE x: WHEN 1 THEN a = 1. b = 2. c = 3. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 1);
            assert_eq!(when_branches[0].body.len(), 3);
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_multiple_statements_in_otherwise() {
    let source = "CASE x: WHEN 1 THEN y = 1. OTHERWISE a = 0. b = 0. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { otherwise, .. } => {
            assert!(otherwise.is_some());
            assert_eq!(otherwise.unwrap().len(), 2);
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_expression_condition() {
    let source = "CASE x + 1: WHEN 2 THEN y = 1. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { expression, .. } => {
            assert!(matches!(expression.kind, ExpressionKind::Add(_, _)));
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_full_structure() {
    let source = "CASE myVal: WHEN 1 THEN x = 1. WHEN 2 THEN x = 2. OTHERWISE x = 0. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        StatementKind::Case {
            expression: ExpressionKind::Identifier(Identifier {
                span: Span { start: 5, end: 10 },
                name: "myVal".to_string()
            })
            .into(),
            when_branches: vec![
                WhenBranch {
                    values: vec![
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 17, end: 18 },
                            value: 1
                        }))
                        .into()
                    ],
                    body: vec![
                        StatementKind::Assignment {
                            target: ExpressionKind::Identifier(Identifier {
                                span: Span { start: 24, end: 25 },
                                name: "x".to_string()
                            })
                            .into(),
                            value: ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                                span: Span { start: 28, end: 29 },
                                value: 1
                            }))
                            .into()
                        }
                        .into()
                    ]
                },
                WhenBranch {
                    values: vec![
                        ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 36, end: 37 },
                            value: 2
                        }))
                        .into()
                    ],
                    body: vec![
                        StatementKind::Assignment {
                            target: ExpressionKind::Identifier(Identifier {
                                span: Span { start: 43, end: 44 },
                                name: "x".to_string()
                            })
                            .into(),
                            value: ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                                span: Span { start: 47, end: 48 },
                                value: 2
                            }))
                            .into()
                        }
                        .into()
                    ]
                }
            ],
            otherwise: Some(vec![
                StatementKind::Assignment {
                    target: ExpressionKind::Identifier(Identifier {
                        span: Span { start: 60, end: 61 },
                        name: "x".to_string()
                    })
                    .into(),
                    value: ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 64, end: 65 },
                        value: 0
                    }))
                    .into()
                }
                .into()
            ])
        }
    );
}

#[test]
fn parse_case_with_nested_if() {
    let source = "CASE x: WHEN 1 THEN IF y > 0 THEN z = 1. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 1);
            assert!(matches!(
                when_branches[0].body[0].kind,
                StatementKind::If { .. }
            ));
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_boolean_values() {
    let source = "CASE isActive: WHEN TRUE THEN x = 1. WHEN FALSE THEN x = 0. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 2);
            assert!(matches!(
                when_branches[0].values[0].kind,
                ExpressionKind::Literal(Literal::Boolean(BooleanLiteral { value: true, .. }))
            ));
            assert!(matches!(
                when_branches[1].values[0].kind,
                ExpressionKind::Literal(Literal::Boolean(BooleanLiteral { value: false, .. }))
            ));
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_case_with_triple_or_when() {
    // Test WHEN with multiple OR WHEN clauses
    let source = r#"CASE grade: WHEN "A" OR WHEN "B" OR WHEN "C" THEN passed = TRUE. END CASE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 1);
            assert_eq!(when_branches[0].values.len(), 3);
        }
        _ => panic!("Expected Case statement"),
    }
}

#[test]
fn parse_define_input_parameter() {
    let source = "DEFINE INPUT PARAMETER name AS CHARACTER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                },
        } => {
            assert_eq!(direction, ParameterDirection::Input);
            assert_eq!(name.name, "name");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Character));
            assert!(!no_undo);
        }
        _ => panic!("Expected DefineParameter statement"),
    }
}

#[test]
fn parse_define_output_parameter() {
    let source = "DEFINE OUTPUT PARAMETER result AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                },
        } => {
            assert_eq!(direction, ParameterDirection::Output);
            assert_eq!(name.name, "result");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
            assert!(!no_undo);
        }
        _ => panic!("Expected DefineParameter statement"),
    }
}

#[test]
fn parse_define_output_parameter_with_format() {
    let source = r#"def output parameter v-message as character format "x(125)":U no-undo."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                },
        } => {
            assert_eq!(direction, ParameterDirection::Output);
            assert_eq!(name.name, "v-message");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Character));
            assert!(no_undo);
        }
        _ => panic!("Expected DefineParameter statement"),
    }
}

#[test]
fn parse_define_input_output_parameter() {
    let source = "DEFINE INPUT-OUTPUT PARAMETER data AS LOGICAL.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                },
        } => {
            assert_eq!(direction, ParameterDirection::InputOutput);
            assert_eq!(name.name, "data");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Logical));
            assert!(!no_undo);
        }
        _ => panic!("Expected DefineParameter statement"),
    }
}

#[test]
fn parse_define_parameter_with_no_undo() {
    let source = "DEFINE INPUT PARAMETER name AS CHARACTER NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name,
                    type_source,
                    no_undo,
                },
        } => {
            assert_eq!(direction, ParameterDirection::Input);
            assert_eq!(name.name, "name");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Character));
            assert!(no_undo);
        }
        _ => panic!("Expected DefineParameter statement"),
    }
}

#[test]
fn parse_procedure_with_parameters() {
    let source = r#"
PROCEDURE my-proc:
    DEFINE INPUT PARAMETER name AS CHARACTER.
    DEFINE OUTPUT PARAMETER result AS INTEGER.
    result = 42.
END PROCEDURE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "my-proc");
            assert_eq!(body.len(), 3);
            // First statement should be input parameter
            match &body[0].kind {
                StatementKind::DefineParameter {
                    direction,
                    param_type:
                        ParameterType::Variable {
                            name, type_source, ..
                        },
                } => {
                    assert_eq!(*direction, ParameterDirection::Input);
                    assert_eq!(name.name, "name");
                    assert_eq!(*type_source, TypeSource::Explicit(DataType::Character));
                }
                _ => panic!("Expected DefineParameter"),
            }
            // Second statement should be output parameter
            match &body[1].kind {
                StatementKind::DefineParameter {
                    direction,
                    param_type:
                        ParameterType::Variable {
                            name, type_source, ..
                        },
                } => {
                    assert_eq!(*direction, ParameterDirection::Output);
                    assert_eq!(name.name, "result");
                    assert_eq!(*type_source, TypeSource::Explicit(DataType::Integer));
                }
                _ => panic!("Expected DefineParameter"),
            }
        }
        _ => panic!("Expected Procedure statement"),
    }
}

// ===================== PROCEDURE IN SUPER / IN handle (#65 follow-up) =====================

#[test]
fn parse_procedure_in_super_with_body() {
    // ADM2 prototype include shape (smrtprto.i / qryprto.i).
    let source = r#"
PROCEDURE assignDBRow IN SUPER:
  DEFINE INPUT PARAMETER phRowObjUpd AS HANDLE.
END PROCEDURE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("PROCEDURE … IN SUPER: must parse");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "assignDBRow");
            assert_eq!(body.len(), 1);
            match &body[0].kind {
                StatementKind::DefineParameter { direction, .. } => {
                    assert_eq!(*direction, ParameterDirection::Input);
                }
                _ => panic!("Expected DEFINE INPUT PARAMETER in body"),
            }
        }
        _ => panic!("Expected Procedure statement"),
    }
}

#[test]
fn procedure_external_is_marked_external() {
    for (source, prototype, external) in [
        (
            "PROCEDURE Sleep EXTERNAL \"kernel32.dll\" STDCALL:\n  DEFINE INPUT PARAMETER n AS LONG.\nEND PROCEDURE.",
            true,
            true,
        ),
        (
            "PROCEDURE work IN SUPER:\n  DEFINE INPUT PARAMETER h AS HANDLE.\nEND PROCEDURE.",
            true,
            false,
        ),
        ("PROCEDURE work IN SUPER.", true, false),
        ("PROCEDURE work:\nEND PROCEDURE.", false, false),
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect(source);
        match stmt.kind {
            StatementKind::Procedure {
                is_prototype,
                is_external,
                ..
            } => {
                assert_eq!(is_prototype, prototype, "{source}");
                assert_eq!(is_external, external, "{source}");
            }
            _ => panic!("Expected Procedure statement: {source}"),
        }
    }
}

#[test]
fn parse_procedure_in_super_period_only_empty_body() {
    let source = "PROCEDURE assignDBRow IN SUPER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("PROCEDURE … IN SUPER. must parse");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "assignDBRow");
            assert!(body.is_empty(), "period form has empty body");
        }
        _ => panic!("Expected Procedure statement"),
    }
}

#[test]
fn parse_procedure_in_this_procedure() {
    let source = r#"
PROCEDURE local-hook IN THIS-PROCEDURE:
END PROCEDURE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("PROCEDURE … IN THIS-PROCEDURE: must parse");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "local-hook");
            assert!(body.is_empty());
        }
        _ => panic!("Expected Procedure statement"),
    }
}

#[test]
fn parse_procedure_in_handle_identifier() {
    let source = r#"
PROCEDURE doWork IN hSuper:
END PROCEDURE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("PROCEDURE … IN hSuper: must parse");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "doWork");
            assert!(body.is_empty());
        }
        _ => panic!("Expected Procedure statement"),
    }
}

#[test]
fn parse_procedure_in_super_period_then_next_procedure() {
    // Guard: period form must not swallow the following prototype as body.
    let source = r#"
PROCEDURE a IN SUPER.
PROCEDURE b IN SUPER:
  DEFINE INPUT PARAMETER x AS INTEGER.
END PROCEDURE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "expected no parse errors, got: {:?}",
        program.errors
    );
    assert_eq!(program.statements.len(), 2);
    match &program.statements[0].kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "a");
            assert!(body.is_empty());
        }
        _ => panic!("Expected first Procedure"),
    }
    match &program.statements[1].kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "b");
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected second Procedure"),
    }
}

/// The prototype's parameter types, and whether its body is empty.
fn prototype_parameter_types(source: &str) -> (Vec<DataType>, bool) {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("prototype must parse");
    let StatementKind::Function {
        body,
        prototype_parameters,
        ..
    } = stmt.kind
    else {
        panic!("Expected Function statement");
    };
    let types = prototype_parameters
        .iter()
        .map(|param| match &param.kind {
            StatementKind::DefineParameter {
                param_type:
                    ParameterType::Variable {
                        type_source: TypeSource::Explicit(data_type),
                        ..
                    },
                ..
            } => data_type.clone(),
            other => panic!("Expected a typed parameter, got {other:?}"),
        })
        .collect();
    (types, body.is_empty())
}

#[test]
fn a_function_prototype_keeps_its_parameters_apart_from_its_body() {
    for tail in ["FORWARD", "IN SUPER", "IN h", "MAP TO g2 IN h"] {
        let source = format!(
            "FUNCTION g RETURNS LOGICAL (INPUT p AS pkg.Thing, INPUT n AS INTEGER) {tail}."
        );
        let (types, body_is_empty) = prototype_parameter_types(&source);
        assert_eq!(
            types,
            [DataType::Class("pkg.Thing".into()), DataType::Integer],
            "{tail}"
        );
        assert!(body_is_empty, "{tail}");
    }
}

#[test]
fn a_function_definition_has_no_prototype_parameters() {
    let source =
        "FUNCTION g RETURNS LOGICAL (INPUT p AS pkg.Thing):\n  RETURN TRUE.\nEND FUNCTION.";
    let (types, body_is_empty) = prototype_parameter_types(source);
    assert!(types.is_empty());
    assert!(!body_is_empty);
}

#[test]
fn parse_function_in_super_still_works() {
    let source = "FUNCTION getX RETURNS CHARACTER IN SUPER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("FUNCTION … IN SUPER. must parse");
    match stmt.kind {
        StatementKind::Function {
            name,
            return_type,
            body,
            ..
        } => {
            assert_eq!(name.name, "getX");
            assert_eq!(return_type, DataType::Character);
            assert!(body.is_empty());
        }
        _ => panic!("Expected Function statement"),
    }
}

// ===================== DISPLAY statement tests =====================

#[test]
fn parse_display_simple_field_access() {
    let source = "DISPLAY Customer.Name Customer.Balance.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display {
            items,
            except,
            frame,
            ..
        } => {
            assert_eq!(items.len(), 2);
            assert!(items[0].when_condition.is_none());
            assert!(items[1].when_condition.is_none());
            assert!(except.is_empty());
            assert!(frame.is_none());
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_with_frame() {
    let source = r#"DISPLAY "Total:" total WITH FRAME f1."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, frame, .. } => {
            assert_eq!(items.len(), 2);
            assert!(frame.is_some());
            assert_eq!(frame.unwrap().name, "f1");
        }
        _ => panic!("Expected Display statement"),
    }
}

/// A period followed by whitespace ends the statement, so a statement on the
/// same line after `WITH FRAME x.` is its own statement and not swallowed into
/// the frame phrase.
#[test]
fn a_same_line_statement_after_a_frame_phrase_is_its_own_statement() {
    for source in [
        "DISPLAY x WITH FRAME a. y = 1.",
        "DISPLAY x FRAME a. y = 1.",
        "UPDATE x WITH FRAME a. y = 1.",
        "DISPLAY x WITH FRAME a.\ny = 1.",
    ] {
        let tokens = tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        assert!(
            program.errors.is_empty(),
            "{source:?}: {:?}",
            program.errors
        );
        assert_eq!(program.statements.len(), 2, "for {source:?}");
        assert!(
            matches!(program.statements[1].kind, StatementKind::Assignment { .. }),
            "for {source:?}: {:?}",
            program.statements[1].kind
        );
    }
}

/// A byte-adjacent period still joins a field access inside the frame phrase.
#[test]
fn a_qualified_name_inside_a_frame_phrase_does_not_end_the_statement() {
    let source = "DISPLAY x WITH FRAME a TITLE \"t\" + tbl.fld + \"u\". y = 1.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert_eq!(program.statements.len(), 2);
}

#[test]
fn parse_display_with_frame_columns() {
    let source = "DISPLAY x y z WITH FRAME results 2 COLUMNS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, frame, .. } => {
            assert_eq!(items.len(), 3);
            assert!(frame.is_some());
            assert_eq!(frame.unwrap().name, "results");
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_except_with_frame() {
    let source = "DISPLAY Customer EXCEPT CustNum WITH FRAME cust-frame.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display {
            items,
            except,
            frame,
            ..
        } => {
            assert_eq!(items.len(), 1);
            assert_eq!(except.len(), 1);
            assert_eq!(except[0].name, "CustNum");
            assert!(frame.is_some());
            assert_eq!(frame.unwrap().name, "cust-frame");
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_expression() {
    let source = "DISPLAY 1 + 2.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, .. } => {
            assert_eq!(items.len(), 1);
            assert!(matches!(
                items[0].expression.kind,
                ExpressionKind::Add(_, _)
            ));
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_with_when() {
    let source = "DISPLAY x WHEN showIt.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, .. } => {
            assert_eq!(items.len(), 1);
            assert!(items[0].when_condition.is_some());
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_with_format() {
    let source = r#"DISPLAY x FORMAT "x(20)" y FORMAT "9(5)"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, .. } => {
            assert_eq!(items.len(), 2);
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_with_column_label() {
    let source = r#"DISPLAY x COLUMN-LABEL "Name" y COLUMN-LABEL "Balance"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, .. } => {
            assert_eq!(items.len(), 2);
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_with_format_and_column_label() {
    let source = r#"DISPLAY x FORMAT "x(20)" COLUMN-LABEL "Name"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Display { items, .. } => {
            assert_eq!(items.len(), 1);
        }
        _ => panic!("Expected Display statement"),
    }
}

// ===================== MESSAGE statement tests =====================

#[test]
fn parse_message_simple() {
    let source = r#"MESSAGE "Hello, World!"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Message { items, set_targets } => {
            assert_eq!(items.len(), 1);
            assert!(set_targets.is_empty());
        }
        _ => panic!("Expected Message statement"),
    }
}

#[test]
fn parse_message_view_as_alert_box() {
    let source = r#"MESSAGE "Error:" errMsg VIEW-AS ALERT-BOX ERROR."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Message { items, set_targets } => {
            assert_eq!(items.len(), 2); // "Error:" and errMsg
            assert!(set_targets.is_empty());
        }
        _ => panic!("Expected Message statement"),
    }
}

#[test]
fn parse_message_with_update() {
    let source = r#"MESSAGE "Confirm?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Message { items, set_targets } => {
            assert_eq!(items.len(), 1); // "Confirm?"
            assert_eq!(set_targets.len(), 1);
            assert_eq!(set_targets[0].name, "lChoice");
        }
        _ => panic!("Expected Message statement"),
    }
}

#[test]
fn parse_message_with_skip() {
    let source = "MESSAGE Customer.Name SKIP Customer.Balance.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Message { items, .. } => {
            assert_eq!(items.len(), 2); // Customer.Name and Customer.Balance (SKIP is skipped)
        }
        _ => panic!("Expected Message statement"),
    }
}

#[test]
fn parse_message_with_skip_count() {
    let source = r#"MESSAGE "Line 1" SKIP(2) "Line 4"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Message { items, .. } => {
            assert_eq!(items.len(), 2); // "Line 1" and "Line 4" (SKIP(2) is skipped)
        }
        _ => panic!("Expected Message statement"),
    }
}

#[test]
fn parse_message_update_without_view_as() {
    let source = r#"MESSAGE "Enter name:" UPDATE cName."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Message { items, set_targets } => {
            assert_eq!(items.len(), 1);
            assert_eq!(set_targets.len(), 1);
            assert_eq!(set_targets[0].name, "cName");
        }
        _ => panic!("Expected Message statement"),
    }
}

// ===================== RUN statement tests =====================

#[test]
fn parse_run_simple_procedure() {
    let source = "RUN simple-proc.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target,
            arguments,
            in_handle,
            no_error,
            ..
        } => {
            assert_eq!(target.literal_name(), Some("simple-proc"));
            assert!(arguments.is_empty());
            assert!(in_handle.is_none());
            assert!(!no_error);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_mixed_direction_args() {
    let source = "RUN calculate-total (INPUT 100, INPUT 5, OUTPUT result).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target, arguments, ..
        } => {
            assert_eq!(target.literal_name(), Some("calculate-total"));
            assert_eq!(arguments.len(), 3);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
            assert_eq!(arguments[1].direction, ParameterDirection::Input);
            assert_eq!(arguments[2].direction, ParameterDirection::Output);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_dynamic_value() {
    let source = "RUN VALUE(procName).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { target, .. } => {
            assert!(matches!(target, RunTarget::Dynamic(_)));
            // A dynamic target names nothing at parse time, so there is no
            // literal target to give an identity to.
            assert_eq!(target.literal_name(), None);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_literal_target_records_node_id_and_name_span() {
    let source = "RUN my-proc.p.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { target, .. } => match target {
            RunTarget::Literal {
                id,
                name,
                name_span,
            } => {
                assert_eq!(name, "my-proc.p");
                assert_ne!(id, NodeId::DUMMY);
                assert_ne!(id, stmt.id);
                assert_span_covers(source, name_span, "my-proc.p");
            }
            RunTarget::Dynamic(_) => panic!("Expected a literal target"),
        },
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_target_stops_at_the_period_before_another_statement() {
    // `RUN a. RUN b.` on one line. The target-name scan accepts a dotted method
    // name on the same line, and `RUN` is usable as an identifier, so it used to
    // consume the period and the next statement's keyword into the name — yielding
    // the target `write-header. RUN` and, once cross-file resolution began
    // reporting absent targets, a diagnostic about a name nobody wrote.
    let source = "RUN write-header. RUN build-list.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let names: Vec<String> = program
        .statements
        .iter()
        .filter_map(|s| match &s.kind {
            StatementKind::Run {
                target: RunTarget::Literal { name, .. },
                ..
            } => Some(name.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(names, vec!["write-header", "build-list"]);
}

#[test]
fn parse_run_dotted_method_name_on_one_line_still_parses() {
    // The shape the same-line allowance exists for: a plain identifier after the
    // period is still taken as part of the name.
    let source = "RUN Dataset.Fill.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target: RunTarget::Literal { name, .. },
            ..
        } => assert_eq!(name, "Dataset.Fill"),
        _ => panic!("Expected a literal Run target"),
    }
}

#[test]
fn parse_run_quoted_target_name_span_includes_the_quotes() {
    let source = r#"RUN "sub/dir/thing.p"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { target, .. } => match target {
            RunTarget::Literal {
                id,
                name,
                name_span,
            } => {
                // `name` drops the quotes; the span keeps them so a diagnostic
                // underlines the literal exactly as written.
                assert_eq!(name, "sub/dir/thing.p");
                assert_ne!(id, NodeId::DUMMY);
                assert_span_covers(source, name_span, r#""sub/dir/thing.p""#);
            }
            RunTarget::Dynamic(_) => panic!("Expected a literal target"),
        },
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_quoted_target_with_translation_suffix_keeps_the_name_clean() {
    // A `:U` suffix's bytes sit inside the string token's extent, so building
    // the name by slicing between the first and last byte would yield
    // `my-proc.p":`. The name comes from the token's parsed payload instead;
    // the span still underlines the literal as written, suffix included.
    let source = r#"RUN "my-proc.p":U."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { target, .. } => {
            assert_eq!(target.literal_name(), Some("my-proc.p"));
            match target {
                RunTarget::Literal { name_span, .. } => {
                    assert_span_covers(source, name_span, r#""my-proc.p":U"#);
                }
                RunTarget::Dynamic(_) => panic!("Expected a literal target"),
            }
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn using_and_run_target_ids_are_unique_across_a_program() {
    let source = "\
USING MyApp.Services.Foo.
USING MyApp.Data.*.
RUN first-proc.p.
RUN \"second-proc.p\".
RUN VALUE(cName).
";
    let stmts = parse_program_stmts(source);
    let mut ids = Vec::new();
    for stmt in &stmts {
        ids.push(stmt.id);
        match &stmt.kind {
            StatementKind::Using { id, .. } => ids.push(*id),
            StatementKind::Run { target, .. } => {
                if let RunTarget::Literal { id, .. } = target {
                    ids.push(*id);
                }
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }
    // 5 statements + 2 USING targets + 2 literal RUN targets; the dynamic RUN
    // contributes no target id.
    assert_eq!(ids.len(), 9);
    let mut sorted: Vec<u32> = ids.iter().map(|id| id.as_u32()).collect();
    sorted.sort_unstable();
    let before = sorted.len();
    sorted.dedup();
    assert_eq!(sorted.len(), before, "node ids must be unique");
}

#[test]
fn parse_run_dotted_filename_with_args() {
    let source = r#"RUN external-prog.p (INPUT "data")."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target, arguments, ..
        } => {
            assert_eq!(target.literal_name(), Some("external-prog.p"));
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_hyphenated_name() {
    let source = "RUN my-proc.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { target, .. } => {
            assert_eq!(target.literal_name(), Some("my-proc"));
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_expression_args() {
    let source = "RUN some-proc (INPUT 1 + 2, OUTPUT x).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target, arguments, ..
        } => {
            assert_eq!(target.literal_name(), Some("some-proc"));
            assert_eq!(arguments.len(), 2);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
            // First arg should be an Add expression
            assert!(matches!(
                arguments[0].expression.kind,
                ExpressionKind::Add(_, _)
            ));
            assert_eq!(arguments[1].direction, ParameterDirection::Output);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_string_literal_target() {
    let source = r#"RUN "my-proc.p"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { target, .. } => {
            assert_eq!(target.literal_name(), Some("my-proc.p"));
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_in_handle() {
    let source = "RUN myProc IN hServer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target,
            in_handle,
            no_error,
            ..
        } => {
            assert_eq!(target.literal_name(), Some("myProc"));
            assert!(in_handle.is_some());
            assert!(!no_error);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_no_error() {
    let source = "RUN myProc NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target, no_error, ..
        } => {
            assert_eq!(target.literal_name(), Some("myProc"));
            assert!(no_error);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_args_and_no_error() {
    let source = "RUN myProc (OUTPUT result) NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target,
            arguments,
            no_error,
            ..
        } => {
            assert_eq!(target.literal_name(), Some("myProc"));
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Output);
            assert!(no_error);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_at_eof_no_period() {
    // ABL accepts a missing trailing period when EoF is reached — valid in the last statement.
    let source = "RUN myProc";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_ok(), "Expected Ok, got: {:?}", result.err());
}

#[test]
fn parse_run_input_output_arg() {
    let source = "RUN some-proc (INPUT-OUTPUT x).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run { arguments, .. } => {
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::InputOutput);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_in_super() {
    let source = "RUN myMethod IN SUPER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target, in_handle, ..
        } => {
            assert_eq!(target.literal_name(), Some("myMethod"));
            assert!(in_handle.is_some());
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_persistent_no_handle() {
    let source = "RUN proc.p PERSISTENT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target,
            persistent,
            persistent_handle,
            ..
        } => {
            assert_eq!(target.literal_name(), Some("proc.p"));
            assert!(persistent);
            assert!(persistent_handle.is_none());
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_persistent_set_handle() {
    let source = "RUN proc.p PERSISTENT SET hServer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target,
            persistent,
            persistent_handle,
            ..
        } => {
            assert_eq!(target.literal_name(), Some("proc.p"));
            assert!(persistent);
            assert!(persistent_handle.is_some());
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_asynchronous_no_handle() {
    let source = "RUN proc ASYNCHRONOUS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            target,
            asynchronous,
            async_handle,
            event_procedure,
            ..
        } => {
            assert_eq!(target.literal_name(), Some("proc"));
            assert!(asynchronous);
            assert!(async_handle.is_none());
            assert!(event_procedure.is_none());
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_asynchronous_set_handle() {
    let source = "RUN proc ASYNCHRONOUS SET hAsync.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            asynchronous,
            async_handle,
            event_procedure,
            ..
        } => {
            assert!(asynchronous);
            assert!(async_handle.is_some());
            assert!(event_procedure.is_none());
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_asynchronous_with_event_procedure() {
    let source = r#"RUN proc ASYNCHRONOUS SET hAsync EVENT-PROCEDURE "my-ep.p"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Run {
            asynchronous,
            async_handle,
            event_procedure,
            ..
        } => {
            assert!(asynchronous);
            assert!(async_handle.is_some());
            assert!(event_procedure.is_some());
        }
        _ => panic!("Expected Run statement"),
    }
}

// ===================== TEMP-TABLE tests =====================

#[test]
fn parse_define_temp_table_simple() {
    let source = r#"
DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD CustNum AS INTEGER
    FIELD Name AS CHARACTER
    FIELD Balance AS DECIMAL.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            name,
            no_undo,
            fields,
            indexes,
            ..
        } => {
            assert_eq!(name.name, "ttCustomer");
            assert!(no_undo);
            assert_eq!(fields.len(), 3);
            assert_eq!(fields[0].name.name, "CustNum");
            assert_eq!(
                fields[0].type_source,
                TypeSource::Explicit(DataType::Integer)
            );
            assert_eq!(fields[1].name.name, "Name");
            assert_eq!(
                fields[1].type_source,
                TypeSource::Explicit(DataType::Character)
            );
            assert_eq!(fields[2].name.name, "Balance");
            assert_eq!(
                fields[2].type_source,
                TypeSource::Explicit(DataType::Decimal)
            );
            assert!(indexes.is_empty());
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_with_index() {
    let source = r#"
DEFINE TEMP-TABLE ttOrder NO-UNDO
    FIELD OrderNum AS INTEGER
    FIELD CustNum AS INTEGER
    INDEX idx1 IS PRIMARY UNIQUE OrderNum
    INDEX idx2 CustNum.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            fields, indexes, ..
        } => {
            assert_eq!(fields.len(), 2);
            assert_eq!(indexes.len(), 2);
            assert_eq!(indexes[0].name.name, "idx1");
            assert!(indexes[0].is_primary);
            assert!(indexes[0].is_unique);
            assert_eq!(indexes[0].fields.len(), 1);
            assert_eq!(indexes[0].fields[0].name.name, "OrderNum");
            assert_eq!(indexes[1].name.name, "idx2");
            assert!(!indexes[1].is_primary);
            assert!(!indexes[1].is_unique);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_no_undo_false() {
    let source = r#"
DEFINE TEMP-TABLE ttSimple
    FIELD x AS INTEGER.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { name, no_undo, .. } => {
            assert_eq!(name.name, "ttSimple");
            assert!(!no_undo);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

// ===================== BUFFER tests =====================

#[test]
fn parse_define_buffer() {
    let source = "DEFINE BUFFER bCust FOR Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineBuffer { name, target, .. } => {
            assert_eq!(name.name, "bCust");
            match &target {
                BufferTarget::Table(t) => assert_eq!(t.name, "Customer"),
                _ => panic!("Expected Table target"),
            }
        }
        _ => panic!("Expected DefineBuffer statement"),
    }
}

#[test]
fn parse_define_buffer_for_temp_table() {
    let source = "DEFINE BUFFER bTT FOR TEMP-TABLE ttCustomer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineBuffer { name, target, .. } => {
            assert_eq!(name.name, "bTT");
            match &target {
                BufferTarget::TempTable(t) => assert_eq!(t.name, "ttCustomer"),
                _ => panic!("Expected TempTable target"),
            }
        }
        _ => panic!("Expected DefineBuffer statement"),
    }
}

#[test]
fn parse_define_buffer_preselect() {
    let source = "DEFINE BUFFER bCust FOR Customer PRESELECT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineBuffer { preselect, .. } => {
            assert!(preselect);
        }
        _ => panic!("Expected DefineBuffer statement"),
    }
}

#[test]
fn parse_define_buffer_label() {
    let source = r#"DEFINE BUFFER bCust FOR Customer LABEL "Customer Buffer"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineBuffer { label, .. } => {
            assert_eq!(label, Some("Customer Buffer".to_string()));
        }
        _ => panic!("Expected DefineBuffer statement"),
    }
}

#[test]
fn parse_define_buffer_xml_attrs_skipped() {
    let source =
        r#"DEFINE BUFFER bCust FOR Customer NAMESPACE-URI "urn:foo" SERIALIZE-NAME "cust"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineBuffer { name, .. } => {
            assert_eq!(name.name, "bCust");
        }
        _ => panic!("Expected DefineBuffer statement"),
    }
}

// ===================== TEMP-TABLE LIKE and field enhancements =====================

#[test]
fn parse_define_temp_table_empty() {
    let source = "DEFINE TEMP-TABLE ttEmpty NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            name,
            no_undo,
            fields,
            indexes,
            ..
        } => {
            assert_eq!(name.name, "ttEmpty");
            assert!(no_undo);
            assert!(fields.is_empty());
            assert!(indexes.is_empty());
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_like() {
    let source = "DEFINE TEMP-TABLE tt LIKE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            like_table,
            validate,
            ..
        } => {
            assert!(like_table.is_some());
            assert_eq!(like_table.unwrap().name, "Customer");
            assert!(!validate);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_like_validate() {
    let source = "DEFINE TEMP-TABLE tt LIKE Customer VALIDATE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            like_table,
            validate,
            ..
        } => {
            assert!(like_table.is_some());
            assert!(validate);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_like_use_index() {
    let source =
        "DEFINE TEMP-TABLE tt LIKE Customer USE-INDEX CustNum USE-INDEX CountryPost AS PRIMARY.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { use_indexes, .. } => {
            assert_eq!(use_indexes.len(), 2);
            assert_eq!(use_indexes[0].name.name, "CustNum");
            assert!(!use_indexes[0].as_primary);
            assert_eq!(use_indexes[1].name.name, "CountryPost");
            assert!(use_indexes[1].as_primary);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_like_with_extra_field() {
    let source = r#"
DEFINE TEMP-TABLE tt LIKE Customer
    FIELD extraField AS CHARACTER.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            like_table, fields, ..
        } => {
            assert!(like_table.is_some());
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name.name, "extraField");
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_field_like() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD cust-num LIKE Customer.CustNum.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name.name, "cust-num");
            match &fields[0].type_source {
                TypeSource::Like { source } => {
                    // The dot-separated name is parsed as a single identifier by the lexer
                    // (ABL identifiers can contain dots in field references)
                    assert!(!source.name.is_empty());
                    assert!(!fields[0].validate);
                }
                _ => panic!("Expected Like type source"),
            }
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_field_like_validate() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x LIKE Customer.CustNum VALIDATE.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => match &fields[0].type_source {
            TypeSource::Like { .. } => {
                assert!(fields[0].validate);
            }
            _ => panic!("Expected Like type source"),
        },
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_field_initial() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS INTEGER INITIAL 0
    FIELD y AS CHARACTER INITIAL "".
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields.len(), 2);
            assert!(fields[0].initial_value.is_some());
            assert_eq!(fields[0].initial_value.as_ref().unwrap().len(), 1);
            assert!(fields[1].initial_value.is_some());
            assert_eq!(fields[1].initial_value.as_ref().unwrap().len(), 1);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_field_extent() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS INTEGER EXTENT 5.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields[0].extent, Some(5));
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_field_format_skipped() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS CHARACTER FORMAT "x(20)".
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name.name, "x");
            // FORMAT is skipped, not stored
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_define_temp_table_field_label_skipped() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS CHARACTER LABEL "Name" COLUMN-LABEL "Nm".
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields.len(), 1);
            // Labels are skipped, not stored
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

// ===================== Index enhancement tests =====================

#[test]
fn parse_index_with_ascending_descending() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD a AS INTEGER
    FIELD b AS INTEGER
    INDEX idx1 a ASCENDING b DESCENDING.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { indexes, .. } => {
            assert_eq!(indexes.len(), 1);
            assert_eq!(indexes[0].fields.len(), 2);
            assert_eq!(
                indexes[0].fields[0].direction,
                Some(SortDirection::Ascending)
            );
            assert_eq!(
                indexes[0].fields[1].direction,
                Some(SortDirection::Descending)
            );
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_index_desc_propagation_stores_none() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD a AS CHARACTER
    FIELD b AS CHARACTER
    FIELD c AS CHARACTER
    INDEX idx1 a DESC b c.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { indexes, .. } => {
            assert_eq!(indexes[0].fields.len(), 3);
            assert_eq!(
                indexes[0].fields[0].direction,
                Some(SortDirection::Descending)
            );
            assert_eq!(indexes[0].fields[1].direction, None);
            assert_eq!(indexes[0].fields[2].direction, None);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_index_word_index() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD Name AS CHARACTER
    INDEX idx1 IS WORD-INDEX Name.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { indexes, .. } => {
            assert!(indexes[0].is_word_index);
            assert_eq!(indexes[0].fields.len(), 1);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_index_as_unique_primary() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS INTEGER
    INDEX idx1 AS UNIQUE PRIMARY x.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { indexes, .. } => {
            assert!(indexes[0].is_unique);
            assert!(indexes[0].is_primary);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_index_flags_without_is_prefix() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS INTEGER
    INDEX idx1 PRIMARY UNIQUE x.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { indexes, .. } => {
            assert!(indexes[0].is_primary);
            assert!(indexes[0].is_unique);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_index_flags_reverse_order() {
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD x AS INTEGER
    INDEX idx1 UNIQUE PRIMARY x.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable { indexes, .. } => {
            assert!(indexes[0].is_unique);
            assert!(indexes[0].is_primary);
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

// ===================== Error recovery tests =====================

#[test]
fn parse_temp_table_missing_period_detected() {
    // A DEFINE keyword in the body should trigger a missing-period error
    let source = "DEFINE TEMP-TABLE tt FIELD x AS INTEGER DEFINE VARIABLE y AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("Expected '.'"));
}

#[test]
fn parse_temp_table_skips_xml_attrs() {
    // Unknown tokens like NAMESPACE-URI should be silently skipped
    let source = r#"
DEFINE TEMP-TABLE tt NO-UNDO
    NAMESPACE-URI "urn:foo"
    FIELD x AS INTEGER.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("Should parse despite XML attrs");
    match stmt.kind {
        StatementKind::DefineTempTable { fields, .. } => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name.name, "x");
        }
        _ => panic!("Expected DefineTempTable statement"),
    }
}

#[test]
fn parse_temp_table_missing_name_error() {
    let source = "DEFINE TEMP-TABLE .";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
}

#[test]
fn parse_temp_table_missing_field_name_error() {
    let source = "DEFINE TEMP-TABLE tt FIELD .";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
}

#[test]
fn parse_temp_table_missing_data_type_error() {
    let source = "DEFINE TEMP-TABLE tt FIELD x AS .";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
}

#[test]
fn parse_buffer_missing_name_error() {
    let source = "DEFINE BUFFER FOR Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
}

#[test]
fn parse_buffer_missing_for_error() {
    let source = "DEFINE BUFFER bCust Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
}

// ===================== Error recovery tests =====================

#[test]
fn parse_program_valid_file() {
    let source = r#"
DEFINE VARIABLE x AS INTEGER.
DEFINE VARIABLE y AS CHARACTER.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    assert_eq!(program.statements.len(), 2);
    assert!(program.errors.is_empty());
}

#[test]
fn parse_program_recovers_after_error() {
    // "BLARG BLURG BLORP." is now valid — parsed as an implicit output
    // statement (identifier followed by continuation tokens).
    // Use a construct the parser truly cannot handle: an unterminated block.
    let source = r#"
BLARG BLURG BLORP.
DEFINE VARIABLE x AS INTEGER.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    // Both statements now parse successfully with the continuation logic
    assert!(program.is_ok());
    assert_eq!(program.statements.len(), 2);
}

#[test]
fn parse_program_multiple_errors() {
    // "BLARG BLURG." and "NOPE NOPE NOPE." are now parsed as implicit output
    // statements (identifier followed by continuation tokens, terminated by period).
    let source = r#"
DEFINE VARIABLE x AS INTEGER.
BLARG BLURG.
DEFINE VARIABLE y AS CHARACTER.
NOPE NOPE NOPE.
DEFINE VARIABLE z AS LOGICAL.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    // All five statements now parse successfully
    assert_eq!(program.statements.len(), 5);
    assert_eq!(program.errors.len(), 0);
}

#[test]
fn parse_program_recovers_at_statement_keyword() {
    // Missing period after first statement, parser should sync at IF
    let source = r#"
DEFINE VARIABLE x AS INTEGER
IF TRUE THEN
    x = 1.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    // Should recover and parse the IF statement
    assert!(!program.errors.is_empty());
    assert!(!program.statements.is_empty());
}

#[test]
fn parse_program_empty_input() {
    let source = "";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    assert!(program.statements.is_empty());
    assert!(program.errors.is_empty());
}

#[test]
fn parse_program_all_errors() {
    // Use constructs that actually fail to parse (incomplete DEFINE statements)
    let source = "DEFINE. DEFINE. DEFINE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.statements.is_empty());
    assert_eq!(program.errors.len(), 3);
}

// ===================== CATCH/FINALLY tests =====================

#[test]
fn parse_do_with_catch() {
    let source = r#"
DO:
    x = 1.
    CATCH e AS Progress.Lang.Error:
        y = 2.
    END CATCH.
END.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 2); // x = 1. and CATCH block
            match &body[1].kind {
                StatementKind::Catch {
                    error_var,
                    error_type,
                    body,
                } => {
                    assert_eq!(error_var.name, "e");
                    assert_eq!(error_type, "Progress.Lang.Error");
                    assert_eq!(body.len(), 1);
                }
                _ => panic!("Expected Catch statement"),
            }
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_do_with_finally() {
    let source = r#"
DO:
    x = 1.
    FINALLY:
        y = 2.
    END FINALLY.
END.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 2);
            match &body[1].kind {
                StatementKind::Finally { body } => {
                    assert_eq!(body.len(), 1);
                }
                _ => panic!("Expected Finally statement"),
            }
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_do_with_catch_and_finally() {
    let source = r#"
DO:
    x = 1.
    CATCH e AS Progress.Lang.AppError:
        y = 2.
    END CATCH.
    FINALLY:
        z = 3.
    END FINALLY.
END.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 3); // assignment, CATCH, FINALLY
            assert!(matches!(body[1].kind, StatementKind::Catch { .. }));
            assert!(matches!(body[2].kind, StatementKind::Finally { .. }));
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_catch_end_without_keyword() {
    let source = r#"
DO:
    x = 1.
    CATCH e AS Progress.Lang.Error:
        y = 2.
    END.
END.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    // This should fail because END without CATCH closes the DO block prematurely
    // Actually in ABL, END without qualifier is ambiguous here.
    // Our parser expects END CATCH. Let's test that END. without CATCH works
    // by treating the first END. as closing CATCH (the parser consumes END then
    // checks for optional "catch" keyword).
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 2);
            assert!(matches!(body[1].kind, StatementKind::Catch { .. }));
        }
        _ => panic!("Expected Do statement"),
    }
}

// ===================== ASSIGN statement tests =====================

#[test]
fn parse_assign_single() {
    let source = "ASSIGN x = 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 1);
        }
        _ => panic!("Expected Assign statement"),
    }
}

#[test]
fn parse_assign_multiple() {
    let source = r#"ASSIGN x = 1 y = 2 z = "hello"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 3);
        }
        _ => panic!("Expected Assign statement"),
    }
}

#[test]
fn parse_assign_field_access() {
    let source = "ASSIGN Customer.Name = cName Customer.Balance = 100.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 2);
            assert!(matches!(
                assignments[0].target.kind,
                ExpressionKind::FieldAccess { .. }
            ));
        }
        _ => panic!("Expected Assign statement"),
    }
}

#[test]
fn parse_assign_with_expression() {
    let source = "ASSIGN total = price * quantity.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 1);
            assert!(matches!(
                assignments[0].value.kind,
                ExpressionKind::Multiply(_, _)
            ));
        }
        _ => panic!("Expected Assign statement"),
    }
}

// ===================== FUNCTION definition tests =====================

#[test]
fn parse_function_simple() {
    let source = r#"
FUNCTION get-greeting RETURNS CHARACTER:
    RETURN "Hello".
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function {
            name,
            return_type,
            body,
            ..
        } => {
            assert_eq!(name.name, "get-greeting");
            assert_eq!(return_type, DataType::Character);
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_with_params() {
    let source = r#"
FUNCTION calc-total RETURNS INTEGER (INPUT x AS INTEGER, INPUT y AS INTEGER):
    RETURN x + y.
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function {
            name,
            return_type,
            body,
            ..
        } => {
            assert_eq!(name.name, "calc-total");
            assert_eq!(return_type, DataType::Integer);
            // Signature params prepended as DefineParameter, then RETURN.
            assert_eq!(body.len(), 3, "expected 2 params + RETURN, got {body:?}");
            match &body[0].kind {
                StatementKind::DefineParameter {
                    direction,
                    param_type: ParameterType::Variable { name, .. },
                } => {
                    assert_eq!(*direction, ParameterDirection::Input);
                    assert_eq!(name.name, "x");
                }
                other => panic!("expected first param, got {other:?}"),
            }
            match &body[1].kind {
                StatementKind::DefineParameter {
                    direction,
                    param_type: ParameterType::Variable { name, .. },
                } => {
                    assert_eq!(*direction, ParameterDirection::Input);
                    assert_eq!(name.name, "y");
                }
                other => panic!("expected second param, got {other:?}"),
            }
            assert!(matches!(body[2].kind, StatementKind::Return(_)));
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_empty_param_list() {
    let source = r#"
FUNCTION f RETURNS INTEGER ():
    RETURN 1.
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::Return(_)));
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_forward_with_params_has_empty_body() {
    let source = "FUNCTION getVal RETURNS CHARACTER (INPUT sValue AS CHARACTER) FORWARD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { name, body, .. } => {
            assert_eq!(name.name, "getVal");
            assert!(body.is_empty(), "FORWARD prototype must discard params");
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_in_super_with_params_has_empty_body() {
    let source = "FUNCTION getVal RETURNS CHARACTER (INPUT s AS CHARACTER) IN SUPER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { name, body, .. } => {
            assert_eq!(name.name, "getVal");
            assert!(body.is_empty(), "IN SUPER prototype must discard params");
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_table_handle_param() {
    let source = r#"
FUNCTION f RETURNS LOGICAL (INPUT TABLE-HANDLE hTt):
    RETURN TRUE.
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { body, .. } => {
            assert_eq!(body.len(), 2);
            match &body[0].kind {
                StatementKind::DefineParameter {
                    param_type: ParameterType::Handle { kind, name, .. },
                    ..
                } => {
                    assert_eq!(*kind, HandleParamKind::TableHandle);
                    assert_eq!(name.name, "hTt");
                }
                other => panic!("expected TABLE-HANDLE param, got {other:?}"),
            }
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_type_only_params_forward() {
    // ABL prototypes may omit parameter names: mode + type only.
    for source in [
        "FUNCTION f RETURNS CHARACTER (INPUT CHARACTER) FORWARD.",
        "FUNCTION f RETURNS CHARACTER (CHARACTER) FORWARD.",
        "FUNCTION f RETURNS CHARACTER (INPUT CHARACTER, INPUT INTEGER) FORWARD.",
        "FUNCTION f RETURNS CHARACTER (INPUT CHARACTER) IN SUPER.",
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser
            .parse_statement()
            .unwrap_or_else(|e| panic!("parse failed for {source:?}: {e:?}"));
        match stmt.kind {
            StatementKind::Function { body, .. } => {
                assert!(
                    body.is_empty(),
                    "prototype body must stay empty: {source:?}"
                );
            }
            other => panic!("Expected Function for {source:?}, got {other:?}"),
        }
    }
}

#[test]
fn parse_function_named_param_with_comment_before_as() {
    // A comment between the param name and AS must not trip the
    // type-only-param heuristic into misreading the name as a type.
    let source = r#"
FUNCTION f RETURNS INTEGER (INPUT pc /* customer name */ AS CHARACTER):
    RETURN 1.
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { body, .. } => match &body[0].kind {
            StatementKind::DefineParameter {
                param_type: ParameterType::Variable { name, .. },
                ..
            } => assert_eq!(name.name, "pc"),
            other => panic!("expected named param, got {other:?}"),
        },
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_named_param_still_requires_as() {
    // Type keyword used as a *name* still works with AS/LIKE.
    let source = r#"
FUNCTION f RETURNS INTEGER (INPUT character AS INTEGER):
    RETURN character.
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { body, .. } => match &body[0].kind {
            StatementKind::DefineParameter {
                param_type: ParameterType::Variable { name, .. },
                ..
            } => assert_eq!(name.name, "character"),
            other => panic!("expected named param, got {other:?}"),
        },
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_end_without_keyword() {
    let source = r#"
FUNCTION get-value RETURNS LOGICAL:
    RETURN TRUE.
END.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function {
            name, return_type, ..
        } => {
            assert_eq!(name.name, "get-value");
            assert_eq!(return_type, DataType::Logical);
        }
        _ => panic!("Expected Function statement"),
    }
}

#[test]
fn parse_function_with_body() {
    let source = r#"
FUNCTION calc RETURNS INTEGER:
    DEFINE VARIABLE result AS INTEGER.
    result = 42.
    RETURN result.
END FUNCTION.
"#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { body, .. } => {
            assert_eq!(body.len(), 3); // DEFINE, assignment, RETURN
        }
        _ => panic!("Expected Function statement"),
    }
}

// ============================================================================
// Keyword migration tests: verify unreserved keywords work as both
// statement keywords and identifiers
// ============================================================================

#[test]
fn parse_var_keyword_as_variable_name() {
    // "var" used as a variable name in DEFINE VARIABLE
    let source = "DEFINE VARIABLE var AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name, type_source, ..
        } => {
            assert_eq!(name.name, "var");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_function_keyword_as_variable_name() {
    let source = "DEFINE VARIABLE function AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name, type_source, ..
        } => {
            assert_eq!(name.name, "function");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_catch_keyword_as_variable_name() {
    let source = "DEFINE VARIABLE catch AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name, type_source, ..
        } => {
            assert_eq!(name.name, "catch");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_data_type_keyword_as_variable_name() {
    let source = "DEFINE VARIABLE integer AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name, type_source, ..
        } => {
            assert_eq!(name.name, "integer");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_date_keyword_as_variable_name() {
    let source = "DEFINE VARIABLE date AS DATE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name, type_source, ..
        } => {
            assert_eq!(name.name, "date");
            assert_eq!(type_source, TypeSource::Explicit(DataType::Date));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_variable_keyword_assignment_disambiguation() {
    // "variable = 5." should parse as assignment, not VAR statement
    let source = "variable = 5.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { target, .. } => match target.kind {
            ExpressionKind::Identifier(id) => assert_eq!(id.name, "variable"),
            _ => panic!("Expected identifier target"),
        },
        _ => panic!("Expected Assignment, got {:?}", stmt),
    }
}

#[test]
fn parse_function_keyword_assignment_disambiguation() {
    // "function = 5." should parse as assignment, not FUNCTION definition
    let source = "function = 5.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { target, .. } => match target.kind {
            ExpressionKind::Identifier(id) => assert_eq!(id.name, "function"),
            _ => panic!("Expected identifier target"),
        },
        _ => panic!("Expected Assignment, got {:?}", stmt),
    }
}

#[test]
fn parse_var_with_abbreviated_data_types() {
    // INT is abbreviation for INTEGER
    let source = "VAR INT x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
        }
        _ => panic!("Expected VariableDeclaration"),
    }

    // DEC is abbreviation for DECIMAL
    let source = "VAR DEC x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Decimal));
        }
        _ => panic!("Expected VariableDeclaration"),
    }

    // CHAR is abbreviation for CHARACTER
    let source = "VAR CHAR x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Character));
        }
        _ => panic!("Expected VariableDeclaration"),
    }

    // LOG is abbreviation for LOGICAL
    let source = "VAR LOG x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Logical));
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_var_with_all_data_types() {
    let cases = vec![
        ("VAR INTEGER x.", DataType::Integer),
        ("VAR INT64 x.", DataType::Int64),
        ("VAR DECIMAL x.", DataType::Decimal),
        ("VAR CHARACTER x.", DataType::Character),
        ("VAR LOGICAL x.", DataType::Logical),
        ("VAR DATE x.", DataType::Date),
        ("VAR DATETIME x.", DataType::DateTime),
        ("VAR DATETIME-TZ x.", DataType::DateTimeTz),
        ("VAR HANDLE x.", DataType::Handle),
        ("VAR ROWID x.", DataType::Rowid),
        ("VAR RECID x.", DataType::Recid),
        ("VAR RAW x.", DataType::Raw),
        ("VAR MEMPTR x.", DataType::Memptr),
        ("VAR LONGCHAR x.", DataType::Longchar),
        ("VAR CLOB x.", DataType::Clob),
        ("VAR BLOB x.", DataType::Blob),
        ("VAR COM-HANDLE x.", DataType::Com),
    ];

    for (source, expected_type) in cases {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser
            .parse_statement()
            .unwrap_or_else(|e| panic!("Failed to parse '{}': {}", source, e.message));
        match stmt.kind {
            StatementKind::VariableDeclaration { type_source, .. } => {
                assert_eq!(
                    type_source,
                    TypeSource::Explicit(expected_type),
                    "Wrong data type for '{}'",
                    source
                );
            }
            _ => panic!("Expected VariableDeclaration for '{}'", source),
        }
    }
}

// ===================== CLASS/OO-ABL tests =====================

#[test]
fn parse_simple_class() {
    let source = "CLASS MyClass: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class {
            name,
            inherits,
            implements,
            is_abstract,
            is_final,
            use_widget_pool,
            body,
        } => {
            assert_eq!(name.name, "MyClass");
            assert!(!use_widget_pool);
            assert!(inherits.is_none());
            assert!(implements.is_empty());
            assert!(!is_abstract);
            assert!(!is_final);
            assert!(body.is_empty());
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_class_use_widget_pool() {
    let source = "CLASS MyClass INHERITS Base USE-WIDGET-POOL: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class {
            inherits,
            use_widget_pool,
            ..
        } => {
            assert_eq!(inherits.unwrap().name, "Base");
            assert!(use_widget_pool);
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_class_with_inherits() {
    let source = "CLASS MyApp.CustomerService INHERITS BaseService: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class { name, inherits, .. } => {
            assert_eq!(name.name, "MyApp.CustomerService");
            assert_eq!(inherits.unwrap().name, "BaseService");
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_class_with_implements() {
    let source = "CLASS MyService IMPLEMENTS IService: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class { implements, .. } => {
            assert_eq!(implements.len(), 1);
            assert_eq!(implements[0].name, "IService");
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_class_inherits_and_implements_multiple() {
    let source =
        "CLASS MyService INHERITS BaseService IMPLEMENTS IService, IDisposable: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class {
            inherits,
            implements,
            ..
        } => {
            assert_eq!(inherits.unwrap().name, "BaseService");
            assert_eq!(implements.len(), 2);
            assert_eq!(implements[0].name, "IService");
            assert_eq!(implements[1].name, "IDisposable");
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_abstract_class() {
    let source = "CLASS ABSTRACT MyClass: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class {
            is_abstract,
            is_final,
            ..
        } => {
            assert!(is_abstract);
            assert!(!is_final);
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_final_class() {
    let source = "CLASS FINAL MyClass: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class { is_final, .. } => {
            assert!(is_final);
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_method_public_void_no_params() {
    let source = "METHOD PUBLIC VOID DoSomething(): END METHOD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            access,
            is_static,
            is_abstract,
            is_override,
            return_type,
            name,
            parameters,
            body,
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert!(!is_static);
            assert!(!is_abstract);
            assert!(!is_override);
            assert!(return_type.is_none()); // VOID
            assert_eq!(name.name, "DoSomething");
            assert!(parameters.is_empty());
            assert!(body.is_empty());
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_method_with_preproc_access_modifier() {
    // Real-world ABL code uses a preprocessor
    // variable where an access modifier would normally appear:
    //     method {&method-access-type} JsonObject foo (input x as char):
    // `{&method-access-type}` resolves to PUBLIC or PRIVATE at preprocessing
    // time. The parser cannot know which, so it treats the reference as a
    // placeholder and defaults the access to Public.
    let source =
        "METHOD {&method-access-type} INTEGER Foo(INPUT x AS CHARACTER): RETURN 0. END METHOD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            access,
            return_type,
            name,
            parameters,
            ..
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert_eq!(return_type, Some(DataType::Integer));
            assert_eq!(name.name, "Foo");
            assert_eq!(parameters.len(), 1);
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_generic_class_data_type() {
    // `List<String>`, `IIterator<String>` — OO-ABL generic class references.
    let source = "DEF PUBLIC PROPERTY mode_list AS List<String> NO-UNDO GET. PRIVATE SET.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_statement().expect("expected property");
}

#[test]
fn parse_new_with_generic_class() {
    // `NEW List<String>()` — object instantiation with generic class.
    let source = "x = NEW List<String>().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_statement().expect("expected assignment");
}

/// The type arguments written back out, nested ones in their own brackets.
fn render_args(arguments: &TypeArguments) -> String {
    arguments
        .0
        .iter()
        .map(|arg| {
            if arg.arguments.0.is_empty() {
                arg.name.name.clone()
            } else {
                format!("{}<{}>", arg.name.name, render_args(&arg.arguments))
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// The class and type arguments of the variable `DEFINE VARIABLE v AS <ty>.`
fn declared_generic(ty: &str) -> (String, String) {
    let source = format!("DEFINE VARIABLE v AS {ty} NO-UNDO.");
    let tokens = tokenize(&source);
    let mut parser = Parser::new(&tokens, &source);
    let stmt = parser.parse_statement().expect("expected a declaration");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            type_source: TypeSource::Explicit(DataType::Generic(generic)),
            ..
        } => (generic.class.clone(), render_args(&generic.arguments)),
        other => panic!("Expected a generic declaration, got {other:?}"),
    }
}

#[test]
fn a_generic_type_keeps_the_class_names_between_its_brackets() {
    let generic = |class: &str, args: &str| (class.to_string(), args.to_string());
    assert_eq!(
        declared_generic("List<pkg.Thing>"),
        generic("List", "pkg.Thing")
    );
    assert_eq!(
        declared_generic("Progress.Collections.HashMap<Thing, pkg.Gone>"),
        generic("Progress.Collections.HashMap", "Thing, pkg.Gone")
    );
}

#[test]
fn nested_type_arguments_stay_nested() {
    assert_eq!(
        declared_generic("HashMap<A, List<pkg.B>>"),
        ("HashMap".to_string(), "A, List<pkg.B>".to_string())
    );
}

#[test]
fn a_built_in_type_argument_is_dropped() {
    assert_eq!(
        declared_generic("HashMap<INTEGER, pkg.Thing>"),
        ("HashMap".to_string(), "pkg.Thing".to_string())
    );
    let source = "DEFINE VARIABLE v AS HashMap<INT64, CHARACTER> NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("expected a declaration");
    assert!(matches!(
        stmt.kind,
        StatementKind::VariableDeclaration {
            type_source: TypeSource::Explicit(DataType::Class(_)),
            ..
        }
    ));
}

#[test]
fn a_type_argument_carries_the_span_of_its_name() {
    let source = "DEFINE VARIABLE v AS List<pkg.Thing> NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("expected a declaration");
    let StatementKind::VariableDeclaration {
        type_source: TypeSource::Explicit(DataType::Generic(generic)),
        ..
    } = stmt.kind
    else {
        panic!("Expected a generic declaration");
    };
    let span = generic.arguments.0[0].name.span;
    assert_eq!(&source[span.start as usize..span.end as usize], "pkg.Thing");
}

#[test]
fn a_class_with_no_type_arguments_is_not_generic() {
    let source = "DEFINE VARIABLE v AS pkg.Thing NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("expected a declaration");
    assert!(matches!(
        stmt.kind,
        StatementKind::VariableDeclaration {
            type_source: TypeSource::Explicit(DataType::Class(_)),
            ..
        }
    ));
}

#[test]
fn a_new_expression_keeps_its_type_arguments() {
    let new_of = |source: &str| {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("expected an assignment");
        let StatementKind::Assignment { value, .. } = stmt.kind else {
            panic!("Expected Assignment");
        };
        match value.kind {
            ExpressionKind::New {
                class_name,
                type_arguments,
                ..
            } => (class_name, type_arguments.map(|args| render_args(&args))),
            other => panic!("Expected New, got {other:?}"),
        }
    };
    assert_eq!(
        new_of("x = NEW List<pkg.Thing>()."),
        ("List".to_string(), Some("pkg.Thing".to_string()))
    );
    assert_eq!(
        new_of("x = NEW pkg.Thing()."),
        ("pkg.Thing".to_string(), None)
    );
}

#[test]
fn parse_method_call_on_keyword_member_names() {
    // OO-ABL libraries use `Current`, `Contains`, `Begins`, `Matches` as
    // method/property names even though they're ABL keywords.
    let source =
        "x = iter:Current. y = list:Contains(z). b = s:Begins(\"a\"). c = s:Matches(\"*\").";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    for _ in 0..4 {
        parser.parse_statement().expect("expected assignment");
    }
}

#[test]
fn parse_chained_colon_postfix() {
    // `a:b:c()` — chained postfix member/method access.
    let source = "valid = mode_iter:Current:ToString().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_statement().expect("expected assignment");
}

#[test]
fn parse_method_forward_declaration() {
    // Class headers frequently list method forward declarations — the signature
    // with `forward.` and no body. `FORWARD` is an identifier, not a reserved
    // keyword, matching how FUNCTION FORWARD declarations are parsed.
    let source = "METHOD PRIVATE LOGICAL DoThing(INPUT x AS CHARACTER) FORWARD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            access,
            return_type,
            name,
            parameters,
            body,
            ..
        } => {
            assert_eq!(access, AccessModifier::Private);
            assert_eq!(return_type, Some(DataType::Logical));
            assert_eq!(name.name, "DoThing");
            assert_eq!(parameters.len(), 1);
            assert!(body.is_empty());
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_method_forward_with_preproc_access() {
    // Combined real-world case: preprocessor access modifier plus
    // forward declaration.
    let source = "METHOD {&method-access-type} LOGICAL GetErrors(OUTPUT hError AS HANDLE) FORWARD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            access, name, body, ..
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert_eq!(name.name, "GetErrors");
            assert!(body.is_empty());
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_method_private_with_return_and_params() {
    let source = "METHOD PRIVATE INTEGER Calculate(INPUT x AS INTEGER, INPUT y AS INTEGER): RETURN x + y. END METHOD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            access,
            return_type,
            name,
            parameters,
            body,
            ..
        } => {
            assert_eq!(access, AccessModifier::Private);
            assert_eq!(return_type, Some(DataType::Integer));
            assert_eq!(name.name, "Calculate");
            assert_eq!(parameters.len(), 2);
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_static_method() {
    let source = "METHOD PUBLIC STATIC VOID Initialize(): END METHOD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            is_static, name, ..
        } => {
            assert!(is_static);
            assert_eq!(name.name, "Initialize");
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_abstract_method() {
    let source = "METHOD PUBLIC ABSTRACT VOID Run().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method {
            is_abstract,
            body,
            name,
            ..
        } => {
            assert!(is_abstract);
            assert_eq!(name.name, "Run");
            assert!(body.is_empty());
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_override_method() {
    let source = "METHOD PUBLIC OVERRIDE VOID ToString(): RETURN. END METHOD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method { is_override, .. } => {
            assert!(is_override);
        }
        _ => panic!("Expected Method statement"),
    }
}

#[test]
fn parse_property_auto_get_set() {
    let source = "DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Property {
            access,
            is_static,
            name,
            data_type,
            no_undo,
            get_body,
            set_body,
            set_parameters,
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert!(!is_static);
            assert_eq!(name.name, "Name");
            assert_eq!(data_type, DataType::Character);
            assert!(no_undo);
            assert_eq!(get_body, Some(Vec::new())); // auto-getter
            assert_eq!(set_body, Some(Vec::new())); // auto-setter
            assert!(set_parameters.is_empty());
        }
        _ => panic!("Expected Property statement"),
    }
}

#[test]
fn parse_property_computed_get_set() {
    let source = r#"DEFINE PUBLIC PROPERTY FullName AS CHARACTER NO-UNDO
        GET:
            RETURN "hello".
        END GET.
        SET:
            DEFINE VARIABLE x AS CHARACTER.
        END SET."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Property {
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            let get = get_body.unwrap();
            assert_eq!(get.len(), 1); // RETURN "hello".
            let set = set_body.unwrap();
            assert_eq!(set.len(), 1); // DEFINE VARIABLE x AS CHARACTER.
            assert!(set_parameters.is_empty());
        }
        _ => panic!("Expected Property statement"),
    }
}

#[test]
fn parse_property_read_only() {
    let source = "DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO GET.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Property {
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            assert_eq!(get_body, Some(Vec::new())); // has getter
            assert!(set_body.is_none()); // no setter — read-only
            assert!(set_parameters.is_empty());
        }
        _ => panic!("Expected Property statement"),
    }
}

#[test]
fn parse_constructor_with_params() {
    let source = "CONSTRUCTOR PUBLIC MyClass(INPUT name AS CHARACTER): DEFINE VARIABLE x AS INTEGER. END CONSTRUCTOR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Constructor {
            access,
            parameters,
            body,
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert_eq!(parameters.len(), 1);
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected Constructor statement"),
    }
}

#[test]
fn parse_destructor() {
    let source = "DESTRUCTOR PUBLIC MyClass(): END DESTRUCTOR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Destructor { body } => {
            assert!(body.is_empty());
        }
        _ => panic!("Expected Destructor statement"),
    }
}

#[test]
fn parse_interface_with_method_signatures() {
    let source = "INTERFACE IService: METHOD PUBLIC ABSTRACT VOID Run(). END INTERFACE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Interface {
            name,
            inherits,
            body,
        } => {
            assert_eq!(name.name, "IService");
            assert!(inherits.is_empty());
            assert_eq!(body.len(), 1);
            match &body[0].kind {
                StatementKind::Method { is_abstract, .. } => assert!(is_abstract),
                _ => panic!("Expected Method in interface body"),
            }
        }
        _ => panic!("Expected Interface statement"),
    }
}

#[test]
fn parse_interface_with_inherits() {
    let source = "INTERFACE ISpecialService INHERITS IService: END INTERFACE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Interface { inherits, .. } => {
            assert_eq!(inherits.len(), 1);
            assert_eq!(inherits[0].name, "IService");
        }
        _ => panic!("Expected Interface statement"),
    }
}

#[test]
fn parse_using_qualified_name() {
    let source = "USING Progress.Lang.Object.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Using { type_name, .. } => {
            assert_eq!(type_name, "Progress.Lang.Object");
        }
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_using_records_node_id_and_name_span() {
    let source = "USING MyApp.Services.Foo.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Using {
            id,
            type_name,
            name_span,
            ..
        } => {
            assert_eq!(type_name, "MyApp.Services.Foo");
            // The import gets its own identity, distinct from the statement's.
            assert_ne!(id, NodeId::DUMMY);
            assert_ne!(id, stmt.id);
            // The span stops at the name, short of the statement terminator.
            assert_span_covers(source, name_span, "MyApp.Services.Foo");
        }
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_using_wildcard_name_span_includes_the_star() {
    let source = "USING MyApp.Services.*.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Using {
            id,
            type_name,
            name_span,
            ..
        } => {
            assert_eq!(type_name, "MyApp.Services.*");
            assert_ne!(id, NodeId::DUMMY);
            assert_span_covers(source, name_span, "MyApp.Services.*");
        }
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_using_from_propath_name_span_excludes_the_source_clause() {
    let source = "USING MyApp.Services.Foo FROM PROPATH.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Using {
            name_span,
            source: using_source,
            ..
        } => {
            assert_span_covers(source, name_span, "MyApp.Services.Foo");
            assert_eq!(using_source, UsingSource::Propath);
        }
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_using_from_assembly_preserves_the_source_clause() {
    let source = "USING Acme.Widget FROM ASSEMBLY.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Using { source, .. } => assert_eq!(source, UsingSource::Assembly),
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_unix_backslash_escapes_before_whitespace_and_inside_names() {
    let source = "DO:\n\\ END.\nDEFINE VARIABLE ab\\cd AS INTEGER NO-UNDO.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "unexpected errors: {:?}",
        program.errors
    );
    match &program.statements[1].kind {
        StatementKind::VariableDeclaration { name, .. } => assert_eq!(name.name, "abcd"),
        other => panic!("expected variable declaration, got {other:?}"),
    }
}

#[test]
fn parse_using_wildcard() {
    let source = "USING MyApp.Services.*.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Using { type_name, .. } => {
            assert_eq!(type_name, "MyApp.Services.*");
        }
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_full_class_with_mixed_members() {
    let source = r#"CLASS MyApp.CustomerService INHERITS BaseService:
        DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.

        CONSTRUCTOR PUBLIC CustomerService():
        END CONSTRUCTOR.

        METHOD PUBLIC VOID DoWork():
            DEFINE VARIABLE x AS INTEGER.
        END METHOD.

        METHOD PUBLIC CHARACTER GetName():
            RETURN "hello".
        END METHOD.
    END CLASS."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Class { name, body, .. } => {
            assert_eq!(name.name, "MyApp.CustomerService");
            // Property + Constructor + 2 Methods = 4 members
            assert_eq!(body.len(), 4);
            assert!(matches!(body[0].kind, StatementKind::Property { .. }));
            assert!(matches!(body[1].kind, StatementKind::Constructor { .. }));
            assert!(matches!(body[2].kind, StatementKind::Method { .. }));
            assert!(matches!(body[3].kind, StatementKind::Method { .. }));
        }
        _ => panic!("Expected Class statement"),
    }
}

#[test]
fn parse_define_public_variable_in_class() {
    // Access modifier before VARIABLE is accepted (modifier is ignored for now)
    let source = "DEFINE PUBLIC VARIABLE x AS INTEGER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::VariableDeclaration { name, .. } => {
            assert_eq!(name.name, "x");
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn parse_method_with_class_return_type() {
    let source = "METHOD PUBLIC CLASS Progress.Lang.Object GetObj(): RETURN. END METHOD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt.kind {
        StatementKind::Method { return_type, .. } => {
            assert_eq!(
                return_type,
                Some(DataType::Class("Progress.Lang.Object".to_string()))
            );
        }
        _ => panic!("Expected Method statement"),
    }
}

// =============================================================================
// Database manipulation statements
// =============================================================================

/// The name of a plain identifier expression.
fn expr_name(expr: &Expression) -> &str {
    match &expr.kind {
        ExpressionKind::Identifier(id) => &id.name,
        other => panic!("expected an identifier, got {other:?}"),
    }
}

#[test]
fn parse_create_basic() {
    let source = "CREATE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Name(name),
            no_error,
        } => {
            assert_eq!(name.name, "Customer");
            assert!(!no_error);
        }
        _ => panic!("Expected Create statement"),
    }
}

#[test]
fn parse_create_no_error() {
    let source = "CREATE Customer NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Name(name),
            no_error,
        } => {
            assert_eq!(name.name, "Customer");
            assert!(no_error);
        }
        _ => panic!("Expected Create statement"),
    }
}

#[test]
fn compile_is_a_skipped_node_with_no_harvested_names() {
    // Still `Skipped`, not an error-recovery `Empty` — the form was recognized,
    // which is a different fact from a parse failure — and the span still covers
    // the whole statement. What changes is the name list: nothing in a file path
    // is a symbol.
    let source = "COMPILE some/path.p SAVE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match &stmt.kind {
        StatementKind::Skipped {
            names,
            may_reference_tables,
        } => {
            assert!(names.is_empty(), "harvested {names:?}");
            assert!(!*may_reference_tables);
        }
        other => panic!("Expected Skipped, got {other:?}"),
    }
    assert_eq!(stmt.span.start, 0);
    assert_eq!(stmt.span.end as usize, source.len());
}

#[test]
fn parse_delete_object_holds_the_operand_as_an_expression() {
    let source = "DELETE OBJECT h NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match &stmt.kind {
        StatementKind::DeleteObject {
            target, no_error, ..
        } => {
            assert!(*no_error);
            assert!(matches!(&target.kind, ExpressionKind::Identifier(id) if id.name == "h"));
        }
        other => panic!("Expected DeleteObject, got {other:?}"),
    }
    // The span invariant: a statement's span covers the whole statement.
    assert_eq!(stmt.span.start, 0);
    assert_eq!(stmt.span.end as usize, source.len());
}

#[test]
fn parse_delete_object_accepts_a_complex_operand() {
    // The case that forced the old skip: an attribute access, not a name. A
    // `StatementKind::Delete { buffer: Identifier }` could not hold it, which is
    // why the whole statement used to be skipped and its identifiers harvested.
    for source in [
        "DELETE OBJECT ttbl:HANDLE.",
        "DELETE OBJECT hArray[i].",
        "DELETE OBJECT THIS-OBJECT:someHandle NO-ERROR.",
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser
            .parse_statement()
            .unwrap_or_else(|e| panic!("{source}: {e:?}"));
        match &stmt.kind {
            StatementKind::DeleteObject { target, .. } => {
                assert!(
                    !matches!(&target.kind, ExpressionKind::Identifier(_)),
                    "{source}: the operand should be a compound expression"
                );
            }
            other => panic!("{source}: expected DeleteObject, got {other:?}"),
        }
    }
}

#[test]
fn parse_delete_buffer_form_is_unchanged() {
    let source = "DELETE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    match &parser.parse_statement().expect("statement").kind {
        StatementKind::Delete { buffer, .. } => assert_eq!(buffer.name, "Customer"),
        other => panic!("expected Delete, got {other:?}"),
    }
}

#[test]
fn parse_delete_basic() {
    let source = "DELETE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Delete { buffer, no_error } => {
            assert_eq!(buffer.name, "Customer");
            assert!(!no_error);
        }
        _ => panic!("Expected Delete statement"),
    }
}

#[test]
fn parse_delete_no_error() {
    let source = "DELETE Customer NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Delete { buffer, no_error } => {
            assert_eq!(buffer.name, "Customer");
            assert!(no_error);
        }
        _ => panic!("Expected Delete statement"),
    }
}

#[test]
fn parse_release_basic() {
    let source = "RELEASE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Release { buffer, no_error } => {
            assert_eq!(buffer.name, "Customer");
            assert!(!no_error);
        }
        _ => panic!("Expected Release statement"),
    }
}

#[test]
fn parse_release_no_error() {
    let source = "RELEASE Customer NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Release { buffer, no_error } => {
            assert_eq!(buffer.name, "Customer");
            assert!(no_error);
        }
        _ => panic!("Expected Release statement"),
    }
}

#[test]
fn parse_validate_basic() {
    let source = "VALIDATE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Validate { buffer, no_error } => {
            assert_eq!(buffer.name, "Customer");
            assert!(!no_error);
        }
        _ => panic!("Expected Validate statement"),
    }
}

#[test]
fn parse_validate_no_error() {
    let source = "VALIDATE Customer NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Validate { buffer, no_error } => {
            assert_eq!(buffer.name, "Customer");
            assert!(no_error);
        }
        _ => panic!("Expected Validate statement"),
    }
}

#[test]
fn parse_buffer_copy_basic() {
    let source = "BUFFER-COPY bSource TO bTarget.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCopy {
            source,
            target,
            assignments,
            no_error,
        } => {
            assert_eq!(source.name, "bSource");
            assert_eq!(target.name, "bTarget");
            assert!(assignments.is_empty());
            assert!(!no_error);
        }
        _ => panic!("Expected BufferCopy statement"),
    }
}

#[test]
fn parse_buffer_copy_with_assign() {
    let source = "BUFFER-COPY bSource TO bTarget ASSIGN field1 = 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCopy {
            source,
            target,
            assignments,
            no_error,
        } => {
            assert_eq!(source.name, "bSource");
            assert_eq!(target.name, "bTarget");
            assert_eq!(assignments.len(), 1);
            match &assignments[0].target.kind {
                ExpressionKind::Identifier(ident) => assert_eq!(ident.name, "field1"),
                _ => panic!("Expected identifier target"),
            }
            assert!(!no_error);
        }
        _ => panic!("Expected BufferCopy statement"),
    }
}

#[test]
fn parse_buffer_copy_with_multiple_assigns() {
    let source = "BUFFER-COPY bSource TO bTarget ASSIGN field1 = 1 field2 = 2.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCopy {
            assignments,
            no_error,
            ..
        } => {
            assert_eq!(assignments.len(), 2);
            match &assignments[0].target.kind {
                ExpressionKind::Identifier(ident) => assert_eq!(ident.name, "field1"),
                _ => panic!("Expected identifier target"),
            }
            match &assignments[1].target.kind {
                ExpressionKind::Identifier(ident) => assert_eq!(ident.name, "field2"),
                _ => panic!("Expected identifier target"),
            }
            assert!(!no_error);
        }
        _ => panic!("Expected BufferCopy statement"),
    }
}

#[test]
fn parse_buffer_copy_with_assign_and_no_error() {
    let source = "BUFFER-COPY bSource TO bTarget ASSIGN field1 = 1 NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCopy {
            assignments,
            no_error,
            ..
        } => {
            assert_eq!(assignments.len(), 1);
            assert!(no_error);
        }
        _ => panic!("Expected BufferCopy statement"),
    }
}

#[test]
fn parse_buffer_copy_no_error() {
    let source = "BUFFER-COPY bSource TO bTarget NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCopy {
            assignments,
            no_error,
            ..
        } => {
            assert!(assignments.is_empty());
            assert!(no_error);
        }
        _ => panic!("Expected BufferCopy statement"),
    }
}

#[test]
fn parse_buffer_compare_basic() {
    let source = "BUFFER-COMPARE bSource TO bTarget.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCompare {
            source,
            target,
            result_var,
            no_error,
        } => {
            assert_eq!(source.name, "bSource");
            assert_eq!(target.name, "bTarget");
            assert!(result_var.is_none());
            assert!(!no_error);
        }
        _ => panic!("Expected BufferCompare statement"),
    }
}

#[test]
fn parse_buffer_compare_save_result_in() {
    let source = "BUFFER-COMPARE bSource TO bTarget SAVE RESULT IN lResult.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCompare {
            result_var,
            no_error,
            ..
        } => {
            assert_eq!(result_var.unwrap().name, "lResult");
            assert!(!no_error);
        }
        _ => panic!("Expected BufferCompare statement"),
    }
}

#[test]
fn parse_buffer_compare_save_result_in_no_error() {
    let source = "BUFFER-COMPARE bSource TO bTarget SAVE RESULT IN lResult NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCompare {
            result_var,
            no_error,
            ..
        } => {
            assert!(result_var.is_some());
            assert!(no_error);
        }
        _ => panic!("Expected BufferCompare statement"),
    }
}

#[test]
fn parse_buffer_compare_no_error() {
    let source = "BUFFER-COMPARE bSource TO bTarget NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::BufferCompare {
            result_var,
            no_error,
            ..
        } => {
            assert!(result_var.is_none());
            assert!(no_error);
        }
        _ => panic!("Expected BufferCompare statement"),
    }
}

#[test]
fn parse_create_case_insensitive() {
    let source = "create customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Name(name),
            ..
        } => {
            assert_eq!(name.name, "customer");
        }
        _ => panic!("Expected Create statement"),
    }
}

#[test]
fn parse_db_ops_in_program() {
    let source = r#"CREATE Customer.
DELETE Customer NO-ERROR.
RELEASE Customer.
VALIDATE Customer NO-ERROR."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    assert_eq!(program.statements.len(), 4);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::Create { .. }
    ));
    assert!(matches!(
        program.statements[1].kind,
        StatementKind::Delete { .. }
    ));
    assert!(matches!(
        program.statements[2].kind,
        StatementKind::Release { .. }
    ));
    assert!(matches!(
        program.statements[3].kind,
        StatementKind::Validate { .. }
    ));
}

// ── Preprocessor statement tests ─────────────────────────────────

#[test]
fn preproc_if_then_endif() {
    let source = "&IF TRUE &THEN\nDISPLAY \"hello\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(preproc) => {
            assert!(matches!(preproc.condition.kind, ExpressionKind::Literal(_)));
            assert_eq!(preproc.then_branch.len(), 1);
            assert!(preproc.elseif_branches.is_empty());
            assert!(preproc.else_branch.is_none());
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_if_then_else_endif() {
    let source = "&IF TRUE &THEN\nDISPLAY \"a\".\n&ELSE\nDISPLAY \"b\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(preproc) => {
            assert_eq!(preproc.then_branch.len(), 1);
            assert!(preproc.else_branch.is_some());
            assert_eq!(preproc.else_branch.unwrap().len(), 1);
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_if_elseif_endif() {
    let source = "&IF 1 = 1 &THEN\nDISPLAY \"a\".\n&ELSEIF 2 = 2 &THEN\nDISPLAY \"b\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(preproc) => {
            assert_eq!(preproc.then_branch.len(), 1);
            assert_eq!(preproc.elseif_branches.len(), 1);
            assert!(preproc.else_branch.is_none());
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_if_elseif_else_endif() {
    let source = "&IF 1 = 1 &THEN\nDISPLAY \"a\".\n&ELSEIF 2 = 2 &THEN\nDISPLAY \"b\".\n&ELSE\nDISPLAY \"c\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(preproc) => {
            assert_eq!(preproc.then_branch.len(), 1);
            assert_eq!(preproc.elseif_branches.len(), 1);
            assert!(preproc.else_branch.is_some());
            assert_eq!(preproc.else_branch.unwrap().len(), 1);
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_nested_if() {
    let source = "&IF TRUE &THEN\n&IF FALSE &THEN\nDISPLAY \"inner\".\n&ENDIF\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(outer) => {
            assert_eq!(outer.then_branch.len(), 1);
            assert!(matches!(
                outer.then_branch[0].kind,
                StatementKind::PreprocIf(_)
            ));
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_scoped_define_with_value() {
    let source = "&scoped-define FOO 42\nDISPLAY \"x\".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    assert_eq!(program.statements.len(), 2);
    match &program.statements[0].kind {
        StatementKind::PreprocDefine {
            name,
            value_span,
            is_global,
        } => {
            assert_eq!(name.name, "FOO");
            assert!(!is_global);
            assert!(value_span.is_some());
            let span = value_span.unwrap();
            assert_eq!(&source[span.start as usize..span.end as usize], "42");
        }
        _ => panic!("Expected PreprocDefine"),
    }
}

#[test]
fn preproc_scoped_define_no_value() {
    let source = "&scoped-define EMPTY\nDISPLAY.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    assert_eq!(program.statements.len(), 2);
    match &program.statements[0].kind {
        StatementKind::PreprocDefine {
            name,
            value_span,
            is_global,
        } => {
            assert_eq!(name.name, "EMPTY");
            assert!(!is_global);
            assert!(value_span.is_none());
        }
        _ => panic!("Expected PreprocDefine"),
    }
}

#[test]
fn preproc_global_define() {
    let source = "&global-define BAR yes\nDISPLAY.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    match &program.statements[0].kind {
        StatementKind::PreprocDefine {
            name,
            value_span,
            is_global,
        } => {
            assert_eq!(name.name, "BAR");
            assert!(*is_global);
            assert!(value_span.is_some());
        }
        _ => panic!("Expected PreprocDefine"),
    }
}

#[test]
fn preproc_define_complex_value() {
    let source = "&scoped-define EXPR 1 + 2\nDISPLAY.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok());
    match &program.statements[0].kind {
        StatementKind::PreprocDefine {
            name, value_span, ..
        } => {
            assert_eq!(name.name, "EXPR");
            let span = value_span.unwrap();
            assert_eq!(&source[span.start as usize..span.end as usize], "1 + 2");
        }
        _ => panic!("Expected PreprocDefine"),
    }
}

#[test]
fn preproc_undefine() {
    let source = "&UNDEFINE FOO";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocUndefine { name } => {
            assert_eq!(name.name, "FOO");
        }
        _ => panic!("Expected PreprocUndefine"),
    }
}

#[test]
fn preproc_message() {
    let source = "&MESSAGE \"compile-time warning\"";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocMessage { expression } => {
            assert!(matches!(
                expression.kind,
                ExpressionKind::Literal(Literal::String(_))
            ));
        }
        _ => panic!("Expected PreprocMessage"),
    }
}

// ── Expression-level preprocessor tests ──────────────────────────

#[test]
fn preproc_reference_expression() {
    let source = "{&myvar}";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expr = parser.parse_expression().expect("Expected expression");
    match expr.kind {
        ExpressionKind::PreprocReference(name) => {
            assert_eq!(name, "myvar");
        }
        _ => panic!("Expected PreprocReference, got {:?}", expr),
    }
}

#[test]
fn preproc_if_expression() {
    let source = "&IF TRUE &THEN 1 &ELSE 2 &ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expr = parser.parse_expression().expect("Expected expression");
    match expr.kind {
        ExpressionKind::PreprocIf(preproc) => {
            assert!(matches!(preproc.condition.kind, ExpressionKind::Literal(_)));
            assert!(matches!(
                preproc.then_branch.kind,
                ExpressionKind::Literal(_)
            ));
            assert!(preproc.else_branch.is_some());
            assert!(matches!(
                preproc.else_branch.as_ref().unwrap().kind,
                ExpressionKind::Literal(_)
            ));
        }
        _ => panic!("Expected PreprocIf expression"),
    }
}

#[test]
fn preproc_if_expression_missing_else_is_error() {
    let source = "&IF TRUE &THEN 1 &ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_expression();
    assert!(result.is_err());
}

#[test]
fn preproc_if_expression_with_function_call() {
    // DEFINED(x) parses as a function call in the expression parser
    let source = "&IF DEFINED(use-int) &THEN 1 &ELSE 2 &ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expr = parser.parse_expression().expect("Expected expression");
    match expr.kind {
        ExpressionKind::PreprocIf(preproc) => {
            assert!(matches!(
                preproc.condition.kind,
                ExpressionKind::FunctionCall { .. }
            ));
        }
        _ => panic!("Expected PreprocIf expression"),
    }
}

// ── DataType-level preprocessor tests ────────────────────────────

#[test]
fn preproc_if_data_type() {
    let source = "DEFINE VARIABLE x AS &IF TRUE &THEN INTEGER &ELSE CHARACTER &ENDIF NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::VariableDeclaration {
            name, type_source, ..
        } => {
            assert_eq!(name.name, "x");
            match type_source {
                TypeSource::Explicit(DataType::PreprocIf(preproc)) => {
                    assert!(matches!(preproc.then_branch, DataType::Integer));
                    assert!(matches!(
                        preproc.else_branch.as_ref().unwrap(),
                        DataType::Character
                    ));
                }
                _ => panic!("Expected PreprocIf DataType"),
            }
        }
        _ => panic!("Expected VariableDeclaration"),
    }
}

#[test]
fn preproc_if_data_type_missing_else_is_error() {
    let source = "DEFINE VARIABLE x AS &IF TRUE &THEN INTEGER &ENDIF NO-UNDO.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    assert!(result.is_err());
}

// ── PreprocIf<T> structural consistency tests ────────────────────

#[test]
fn preproc_if_statement_with_multiple_stmts_per_branch() {
    let source = "&IF TRUE &THEN\nDISPLAY \"a\".\nDISPLAY \"b\".\n&ELSE\nDISPLAY \"c\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(preproc) => {
            assert_eq!(preproc.then_branch.len(), 2);
            assert_eq!(preproc.else_branch.unwrap().len(), 1);
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_if_with_defined_condition() {
    let source = "&IF DEFINED(use-widget) &THEN\nDISPLAY \"yes\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::PreprocIf(preproc) => {
            assert!(matches!(
                preproc.condition.kind,
                ExpressionKind::FunctionCall { .. }
            ));
        }
        _ => panic!("Expected PreprocIf statement"),
    }
}

#[test]
fn preproc_reference_in_assignment() {
    // {&var} used as expression in a statement
    let source = "x = {&default-value}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { value, .. } => {
            assert!(matches!(value.kind, ExpressionKind::PreprocReference(_)));
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn preproc_case_insensitive() {
    let source = "&if true &then\ndisplay \"x\".\n&endif";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert!(matches!(stmt.kind, StatementKind::PreprocIf(_)));
}

#[test]
fn preproc_program_with_mixed_statements() {
    let source =
        "&SCOPED-DEFINE VER 1\n&IF TRUE &THEN\nDISPLAY \"hello\".\n&ENDIF\nDISPLAY \"done\".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok(), "Errors: {:?}", program.errors);
    assert_eq!(program.statements.len(), 3);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::PreprocDefine { .. }
    ));
    assert!(matches!(
        program.statements[1].kind,
        StatementKind::PreprocIf(_)
    ));
    assert!(matches!(
        program.statements[2].kind,
        StatementKind::Display { .. }
    ));
}

#[test]
fn preproc_if_expression_with_elseif() {
    let source = "&IF 1 = 1 &THEN 10 &ELSEIF 2 = 2 &THEN 20 &ELSE 30 &ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expr = parser.parse_expression().expect("Expected expression");
    match expr.kind {
        ExpressionKind::PreprocIf(preproc) => {
            assert_eq!(preproc.elseif_branches.len(), 1);
            assert!(preproc.else_branch.is_some());
        }
        _ => panic!("Expected PreprocIf expression"),
    }
}

#[test]
fn preproc_define_at_eof() {
    // Define at end of file with no trailing newline
    let source = "&scoped-define NAME hello";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(program.is_ok(), "Errors: {:?}", program.errors);
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0].kind {
        StatementKind::PreprocDefine {
            name, value_span, ..
        } => {
            assert_eq!(name.name, "NAME");
            let span = value_span.unwrap();
            assert_eq!(&source[span.start as usize..span.end as usize], "hello");
        }
        _ => panic!("Expected PreprocDefine"),
    }
}

// ── Stream & Frame Tests ────────────────────────────────────────────

#[test]
fn define_stream_basic() {
    let source = "DEFINE STREAM s1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::DefineStream { name } => {
            assert_eq!(name.name, "s1");
        }
        _ => panic!("Expected DefineStream"),
    }
}

#[test]
fn define_stream_case_insensitive() {
    let source = "define stream MyStream.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::DefineStream { name } => {
            assert_eq!(name.name, "MyStream");
        }
        _ => panic!("Expected DefineStream"),
    }
}

#[test]
fn define_frame_basic() {
    let source = "DEFINE FRAME f1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::DefineFrame { name, .. } => {
            assert_eq!(name.name, "f1");
        }
        _ => panic!("Expected DefineFrame"),
    }
}

#[test]
fn define_frame_with_content() {
    let source = "DEFINE FRAME cust-frame x y WITH 2 COLUMNS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::DefineFrame { name, raw_span, .. } => {
            assert_eq!(name.name, "cust-frame");
            // raw_span should cover content between name and period
            let raw_text = &source[raw_span.start as usize..raw_span.end as usize];
            assert!(raw_text.contains("COLUMNS"));
        }
        _ => panic!("Expected DefineFrame"),
    }
}

#[test]
fn input_from_string_literal() {
    let source = r#"INPUT FROM "data.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            stream_name,
            operation,
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert!(stream_name.is_none());
            assert!(matches!(operation, StreamOperation::From(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_from_identifier() {
    let source = "INPUT FROM cFile.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert!(matches!(operation, StreamOperation::From(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_through() {
    let source = r#"INPUT THROUGH "sort -r"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert!(matches!(operation, StreamOperation::Through(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_thru_abbreviation() {
    let source = r#"INPUT THRU "sort -r"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert!(matches!(operation, StreamOperation::Through(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_close() {
    let source = "INPUT CLOSE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            stream_name,
            operation,
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert!(stream_name.is_none());
            assert_eq!(operation, StreamOperation::Close);
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_stream_named_from() {
    let source = r#"INPUT STREAM s1 FROM "data.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            stream_name,
            operation,
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert_eq!(stream_name.unwrap().name, "s1");
            assert!(matches!(operation, StreamOperation::From(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn output_to_basic() {
    let source = r#"OUTPUT TO "report.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Output);
            match operation {
                StreamOperation::To { append, .. } => assert!(!append),
                _ => panic!("Expected To operation"),
            }
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn output_to_with_append() {
    let source = r#"OUTPUT TO "report.txt" APPEND."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Output);
            match operation {
                StreamOperation::To { append, .. } => assert!(append),
                _ => panic!("Expected To operation"),
            }
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn output_through() {
    let source = r#"OUTPUT THROUGH "lpr"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Output);
            assert!(matches!(operation, StreamOperation::Through(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn output_close() {
    let source = "OUTPUT CLOSE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Output);
            assert_eq!(operation, StreamOperation::Close);
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn output_stream_named_to() {
    let source = r#"OUTPUT STREAM s-report TO "report.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            stream_name,
            operation,
        } => {
            assert_eq!(direction, StreamDirection::Output);
            assert_eq!(stream_name.unwrap().name, "s-report");
            match operation {
                StreamOperation::To { append, .. } => assert!(!append),
                _ => panic!("Expected To operation"),
            }
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_output_through() {
    let source = r#"INPUT-OUTPUT THROUGH "terminal"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::InputOutput);
            assert!(matches!(operation, StreamOperation::Through(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_output_close() {
    let source = "INPUT-OUTPUT CLOSE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::InputOutput);
            assert_eq!(operation, StreamOperation::Close);
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_output_stream_named() {
    let source = r#"INPUT-OUTPUT STREAM sIO THROUGH "pipe"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            stream_name,
            operation,
        } => {
            assert_eq!(direction, StreamDirection::InputOutput);
            assert_eq!(stream_name.unwrap().name, "sIO");
            assert!(matches!(operation, StreamOperation::Through(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn display_with_stream() {
    let source = "DISPLAY STREAM s1 x y.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::Display {
            stream_name, items, ..
        } => {
            assert_eq!(stream_name.unwrap().name, "s1");
            assert_eq!(items.len(), 2);
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn display_without_stream() {
    let source = "DISPLAY x y.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::Display { stream_name, .. } => {
            assert!(stream_name.is_none());
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn stream_io_case_insensitive() {
    let source = r#"input from "data.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("should parse");
    match stmt.kind {
        StatementKind::StreamIo {
            direction,
            operation,
            ..
        } => {
            assert_eq!(direction, StreamDirection::Input);
            assert!(matches!(operation, StreamOperation::From(_)));
        }
        _ => panic!("Expected StreamIo"),
    }
}

#[test]
fn input_does_not_support_to() {
    let source = r#"INPUT TO "file.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    // INPUT TO should not be parsed as stream I/O (TO isn't in the lookahead)
    let result = parser.parse_statement();
    if matches!(&result, Ok(s) if matches!(s.kind, StatementKind::StreamIo { .. })) {
        panic!("INPUT should not support TO");
    }
}

#[test]
fn output_does_not_support_from() {
    let source = r#"OUTPUT FROM "file.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    // OUTPUT FROM should not be parsed as stream I/O (FROM isn't in the lookahead)
    let result = parser.parse_statement();
    if matches!(&result, Ok(s) if matches!(s.kind, StatementKind::StreamIo { .. })) {
        panic!("OUTPUT should not support FROM");
    }
}

#[test]
fn input_output_does_not_support_from() {
    let source = r#"INPUT-OUTPUT FROM "file.txt"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    // INPUT-OUTPUT FROM should not be parsed as stream I/O
    let result = parser.parse_statement();
    if matches!(&result, Ok(s) if matches!(s.kind, StatementKind::StreamIo { .. })) {
        panic!("INPUT-OUTPUT should not support FROM");
    }
}

#[test]
fn input_as_expression_not_stream() {
    // INPUT iParam should not parse as stream I/O — falls through to expression
    let source = "INPUT iParam.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_statement();
    if matches!(&result, Ok(s) if matches!(s.kind, StatementKind::StreamIo { .. })) {
        panic!("INPUT(identifier) should not be StreamIo");
    }
}

// ── DEFINE DATASET tests ────────────────────────────────────────

#[test]
fn parse_define_dataset_basic() {
    let source = "DEFINE DATASET ds FOR ttA.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset {
            name,
            buffers,
            data_relations,
            parent_id_relations,
            ..
        } => {
            assert_eq!(name.name, "ds");
            assert_eq!(buffers.len(), 1);
            assert_eq!(buffers[0].name, "ttA");
            assert!(data_relations.is_empty());
            assert!(parent_id_relations.is_empty());
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_multiple_buffers() {
    let source = "DEFINE DATASET ds FOR ttA, ttB, ttC.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset { buffers, .. } => {
            assert_eq!(buffers.len(), 3);
            assert_eq!(buffers[0].name, "ttA");
            assert_eq!(buffers[1].name, "ttB");
            assert_eq!(buffers[2].name, "ttC");
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_data_relation() {
    let source = r#"DEFINE DATASET dsPerson FOR ttPerson, ttAddress
        DATA-RELATION drPersonAddr FOR ttPerson, ttAddress
        RELATION-FIELDS (personId, personId)."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset { data_relations, .. } => {
            assert_eq!(data_relations.len(), 1);
            let rel = &data_relations[0];
            assert_eq!(rel.name.as_ref().unwrap().name, "drPersonAddr");
            assert_eq!(rel.parent_buffer.name, "ttPerson");
            assert_eq!(rel.child_buffer.name, "ttAddress");
            assert_eq!(rel.relation_fields.len(), 1);
            assert_eq!(rel.relation_fields[0].0.name, "personId");
            assert_eq!(rel.relation_fields[0].1.name, "personId");
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_multiple_relations() {
    let source = r#"DEFINE DATASET ds FOR ttOrder, ttLine, ttItem
        DATA-RELATION r1 FOR ttOrder, ttLine
        RELATION-FIELDS (ordNum, ordNum)
        DATA-RELATION r2 FOR ttLine, ttItem
        RELATION-FIELDS (itemId, itemId)."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset { data_relations, .. } => {
            assert_eq!(data_relations.len(), 2);
            assert_eq!(data_relations[0].name.as_ref().unwrap().name, "r1");
            assert_eq!(data_relations[1].name.as_ref().unwrap().name, "r2");
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_relation_flags() {
    let source = r#"DEFINE DATASET ds FOR ttA, ttB
        DATA-RELATION FOR ttA, ttB
        RELATION-FIELDS (id, id)
        REPOSITION NESTED FOREIGN-KEY-HIDDEN NOT-ACTIVE RECURSIVE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset { data_relations, .. } => {
            let rel = &data_relations[0];
            assert!(rel.name.is_none()); // unnamed relation
            assert!(rel.reposition);
            assert!(rel.nested);
            assert!(rel.foreign_key_hidden);
            assert!(rel.not_active);
            assert!(rel.recursive);
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_parent_id_relation() {
    let source = r#"DEFINE DATASET ds FOR ttParent, ttChild
        PARENT-ID-RELATION pidRel FOR ttParent, ttChild
        PARENT-ID-FIELD idField
        PARENT-FIELDS-BEFORE (field1, field2)
        PARENT-FIELDS-AFTER (field3)."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset {
            parent_id_relations,
            ..
        } => {
            assert_eq!(parent_id_relations.len(), 1);
            let rel = &parent_id_relations[0];
            assert_eq!(rel.name.as_ref().unwrap().name, "pidRel");
            assert_eq!(rel.parent_buffer.name, "ttParent");
            assert_eq!(rel.child_buffer.name, "ttChild");
            assert_eq!(rel.id_field.name, "idField");
            assert_eq!(rel.parent_fields_before.len(), 2);
            assert_eq!(rel.parent_fields_before[0].name, "field1");
            assert_eq!(rel.parent_fields_after.len(), 1);
            assert_eq!(rel.parent_fields_after[0].name, "field3");
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_mixed_relations() {
    let source = r#"DEFINE DATASET ds FOR ttA, ttB, ttC
        DATA-RELATION FOR ttA, ttB
        RELATION-FIELDS (id, id)
        PARENT-ID-RELATION FOR ttA, ttC
        PARENT-ID-FIELD recid-field."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset {
            data_relations,
            parent_id_relations,
            ..
        } => {
            assert_eq!(data_relations.len(), 1);
            assert_eq!(parent_id_relations.len(), 1);
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_xml_serialize_options() {
    let source = r#"DEFINE DATASET ds
        NAMESPACE-URI "urn:example"
        NAMESPACE-PREFIX "ex"
        XML-NODE-NAME "myDs"
        SERIALIZE-NAME "dataset1"
        SERIALIZE-HIDDEN
        FOR ttA."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset { xml_options, .. } => {
            assert_eq!(
                xml_options.namespace_uri.as_ref().unwrap().name,
                "urn:example"
            );
            assert_eq!(xml_options.namespace_prefix.as_ref().unwrap().name, "ex");
            assert_eq!(xml_options.xml_node_name.as_ref().unwrap().name, "myDs");
            assert_eq!(
                xml_options.serialize_name.as_ref().unwrap().name,
                "dataset1"
            );
            assert!(xml_options.serialize_hidden);
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_reference_only() {
    let source = "DEFINE DATASET ds REFERENCE-ONLY FOR ttA.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset { reference_only, .. } => {
            assert!(reference_only);
        }
        _ => panic!("Expected DefineDataset"),
    }
}

#[test]
fn parse_define_dataset_modifiers() {
    // Test NEW SHARED
    let source = "DEFINE NEW SHARED DATASET ds1 FOR ttA.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset {
            is_new_shared,
            is_shared,
            ..
        } => {
            assert!(is_new_shared);
            assert!(!is_shared);
        }
        _ => panic!("Expected DefineDataset"),
    }

    // Test PRIVATE STATIC SERIALIZABLE
    let source = "DEFINE PRIVATE STATIC SERIALIZABLE DATASET ds2 FOR ttA.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataset {
            access,
            is_static,
            serializable,
            non_serializable,
            ..
        } => {
            assert_eq!(access, Some(AccessModifier::Private));
            assert!(is_static);
            assert!(serializable);
            assert!(!non_serializable);
        }
        _ => panic!("Expected DefineDataset"),
    }
}

// ── SHARED / NEW [GLOBAL] SHARED flag capture (ast-invariants §12) ──

fn parse_one(source: &str) -> StatementKind {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_statement().expect("Expected a statement").kind
}

#[test]
fn parse_shared_variable_sets_is_shared() {
    match parse_one("DEFINE SHARED VARIABLE x AS INTEGER.") {
        StatementKind::VariableDeclaration {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(is_shared);
            assert!(!is_new_shared);
            assert!(!is_new_global_shared);
        }
        other => panic!("Expected VariableDeclaration, got {other:?}"),
    }
}

#[test]
fn parse_new_shared_variable_sets_is_new_shared() {
    match parse_one("DEFINE NEW SHARED VARIABLE y AS CHARACTER.") {
        StatementKind::VariableDeclaration {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(!is_shared);
            assert!(is_new_shared);
            assert!(!is_new_global_shared);
        }
        other => panic!("Expected VariableDeclaration, got {other:?}"),
    }
}

#[test]
fn parse_new_global_shared_variable_sets_global() {
    match parse_one("DEFINE NEW GLOBAL SHARED VARIABLE z AS DECIMAL.") {
        StatementKind::VariableDeclaration {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(!is_shared);
            assert!(!is_new_shared);
            assert!(is_new_global_shared);
        }
        other => panic!("Expected VariableDeclaration, got {other:?}"),
    }
}

#[test]
fn parse_shared_temp_table_and_buffer() {
    match parse_one("DEFINE SHARED TEMP-TABLE tt FIELD f AS INTEGER.") {
        StatementKind::DefineTempTable {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(is_shared);
            assert!(!is_new_shared);
            assert!(!is_new_global_shared);
        }
        other => panic!("Expected DefineTempTable, got {other:?}"),
    }

    match parse_one("DEFINE NEW SHARED BUFFER b FOR customer.") {
        StatementKind::DefineBuffer {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(!is_shared);
            assert!(is_new_shared);
            assert!(!is_new_global_shared);
        }
        other => panic!("Expected DefineBuffer, got {other:?}"),
    }
}

#[test]
fn parse_new_global_shared_dataset_sets_global() {
    match parse_one("DEFINE NEW GLOBAL SHARED DATASET ds FOR ttA.") {
        StatementKind::DefineDataset {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(!is_shared);
            assert!(!is_new_shared);
            assert!(is_new_global_shared);
        }
        other => panic!("Expected DefineDataset, got {other:?}"),
    }
}

#[test]
fn parse_plain_variable_has_no_shared_flags() {
    match parse_one("DEFINE VARIABLE n AS INTEGER.") {
        StatementKind::VariableDeclaration {
            is_shared,
            is_new_shared,
            is_new_global_shared,
            ..
        } => {
            assert!(!is_shared);
            assert!(!is_new_shared);
            assert!(!is_new_global_shared);
        }
        other => panic!("Expected VariableDeclaration, got {other:?}"),
    }
}

// ── DEFINE DATA-SOURCE tests ────────────────────────────────────

#[test]
fn parse_define_data_source_basic() {
    let source = "DEFINE DATA-SOURCE dsSrc FOR Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataSource {
            name,
            source_buffers,
            query,
            ..
        } => {
            assert_eq!(name.name, "dsSrc");
            assert!(query.is_none());
            assert_eq!(source_buffers.len(), 1);
            assert_eq!(source_buffers[0].name.name, "Customer");
            assert!(source_buffers[0].keys.is_none());
        }
        _ => panic!("Expected DefineDataSource"),
    }
}

#[test]
fn parse_define_data_source_with_query() {
    let source = "DEFINE DATA-SOURCE dsSrc FOR QUERY qCust Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataSource {
            query,
            source_buffers,
            ..
        } => {
            assert_eq!(query.as_ref().unwrap().name, "qCust");
            assert_eq!(source_buffers.len(), 1);
            assert_eq!(source_buffers[0].name.name, "Customer");
        }
        _ => panic!("Expected DefineDataSource"),
    }
}

#[test]
fn parse_define_data_source_multiple_buffers() {
    let source = "DEFINE DATA-SOURCE dsSrc FOR Customer, Order.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataSource { source_buffers, .. } => {
            assert_eq!(source_buffers.len(), 2);
            assert_eq!(source_buffers[0].name.name, "Customer");
            assert_eq!(source_buffers[1].name.name, "Order");
        }
        _ => panic!("Expected DefineDataSource"),
    }
}

#[test]
fn parse_define_data_source_with_keys() {
    let source = "DEFINE DATA-SOURCE dsSrc FOR Customer KEYS (CustNum, Region).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataSource { source_buffers, .. } => match &source_buffers[0].keys {
            Some(DataSourceKeys::Fields(fields)) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name, "CustNum");
                assert_eq!(fields[1].name, "Region");
            }
            _ => panic!("Expected Fields keys"),
        },
        _ => panic!("Expected DefineDataSource"),
    }
}

#[test]
fn parse_define_data_source_with_rowid_key() {
    let source = "DEFINE DATA-SOURCE dsSrc FOR Customer KEYS (ROWID).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataSource { source_buffers, .. } => {
            assert!(matches!(
                source_buffers[0].keys,
                Some(DataSourceKeys::Rowid)
            ));
        }
        _ => panic!("Expected DefineDataSource"),
    }
}

#[test]
fn parse_define_data_source_access_static() {
    let source = "DEFINE PRIVATE STATIC DATA-SOURCE dsSrc FOR Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineDataSource {
            access, is_static, ..
        } => {
            assert_eq!(access, Some(AccessModifier::Private));
            assert!(is_static);
        }
        _ => panic!("Expected DefineDataSource"),
    }
}

// ── CREATE typed tests ──────────────────────────────────────────

#[test]
fn parse_create_dataset() {
    let source = "CREATE DATASET hDs.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target:
                CreateTarget::Handle {
                    kind,
                    handle,
                    widget_pool,
                    ..
                },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::Dataset);
            assert_eq!(expr_name(&handle), "hDs");
            assert!(widget_pool.is_none());
        }
        _ => panic!("Expected Create Dataset"),
    }

    // With WIDGET-POOL
    let source = r#"CREATE DATASET hDs IN WIDGET-POOL "myPool"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Handle {
                kind, widget_pool, ..
            },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::Dataset);
            assert!(widget_pool.is_some());
        }
        _ => panic!("Expected Create Dataset with widget pool"),
    }
}

#[test]
fn parse_create_data_source() {
    let source = "CREATE DATA-SOURCE hDs.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Handle { kind, handle, .. },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::DataSource);
            assert_eq!(expr_name(&handle), "hDs");
        }
        _ => panic!("Expected Create DataSource"),
    }
}

#[test]
fn parse_create_temp_table() {
    let source = "CREATE TEMP-TABLE hTt.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Handle { kind, handle, .. },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::TempTable);
            assert_eq!(expr_name(&handle), "hTt");
        }
        _ => panic!("Expected Create TempTable"),
    }
}

#[test]
fn parse_create_name() {
    // Existing behavior preserved
    let source = "CREATE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Name(name),
            no_error,
        } => {
            assert_eq!(name.name, "Customer");
            assert!(!no_error);
        }
        _ => panic!("Expected Create Name"),
    }
}

// ── DEFINE PARAMETER typed tests ────────────────────────────────

#[test]
fn parse_define_parameter_dataset() {
    let source = "DEFINE INPUT PARAMETER DATASET FOR dsName.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Handle {
                    kind,
                    name,
                    passing,
                },
        } => {
            assert_eq!(direction, ParameterDirection::Input);
            assert_eq!(kind, HandleParamKind::Dataset);
            assert_eq!(name.name, "dsName");
            assert!(!passing.append);
            assert!(!passing.bind);
        }
        _ => panic!("Expected DefineParameter Handle Dataset"),
    }
}

#[test]
fn parse_define_parameter_dataset_handle() {
    let source = "DEFINE OUTPUT PARAMETER DATASET-HANDLE hDs.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            direction,
            param_type: ParameterType::Handle { kind, name, .. },
        } => {
            assert_eq!(direction, ParameterDirection::Output);
            assert_eq!(kind, HandleParamKind::DatasetHandle);
            assert_eq!(name.name, "hDs");
        }
        _ => panic!("Expected DefineParameter Handle DatasetHandle"),
    }
}

#[test]
fn parse_define_parameter_table_for() {
    let source = "DEFINE INPUT PARAMETER TABLE FOR ttName.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            param_type: ParameterType::Handle { kind, name, .. },
            ..
        } => {
            assert_eq!(kind, HandleParamKind::Table);
            assert_eq!(name.name, "ttName");
        }
        _ => panic!("Expected DefineParameter Handle Table"),
    }
}

#[test]
fn parse_define_parameter_table_handle() {
    let source = "DEFINE OUTPUT PARAMETER TABLE-HANDLE hTt.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            param_type: ParameterType::Handle { kind, name, .. },
            ..
        } => {
            assert_eq!(kind, HandleParamKind::TableHandle);
            assert_eq!(name.name, "hTt");
        }
        _ => panic!("Expected DefineParameter Handle TableHandle"),
    }
}

#[test]
fn parse_define_parameter_buffer() {
    let source = "DEFINE INPUT PARAMETER BUFFER bCust FOR Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            param_type: ParameterType::Buffer { name, target },
            ..
        } => {
            assert_eq!(name.name, "bCust");
            assert_eq!(target.name, "Customer");
        }
        _ => panic!("Expected DefineParameter Buffer"),
    }
}

/// The directionless spelling is the one real ABL uses: a buffer parameter binds
/// to the caller's buffer, so INPUT/OUTPUT would be meaningless and the language
/// does not accept them. Until #130 only the direction-led form parsed, so the
/// valid statement was a parse error and the invalid one was not.
#[test]
fn parse_define_parameter_buffer_without_direction() {
    for (source, expected_target) in [
        ("DEFINE PARAMETER BUFFER bCust FOR Customer.", "Customer"),
        (
            "DEFINE PARAMETER BUFFER bItem FOR TEMP-TABLE ttItem.",
            "ttItem",
        ),
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect("Expected a statement");
        match stmt.kind {
            StatementKind::DefineParameter {
                param_type: ParameterType::Buffer { target, .. },
                ..
            } => assert_eq!(target.name, expected_target),
            other => panic!("Expected DefineParameter Buffer, got {other:?}"),
        }
    }
}

#[test]
fn parse_define_parameter_bind_append() {
    let source = "DEFINE INPUT PARAMETER DATASET FOR dsName BIND APPEND.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineParameter {
            param_type: ParameterType::Handle { passing, .. },
            ..
        } => {
            assert!(passing.bind);
            assert!(passing.append);
            assert!(!passing.by_value);
        }
        _ => panic!("Expected DefineParameter Handle with passing options"),
    }
}

// ── XML/serialize retrofit tests ────────────────────────────────

#[test]
fn parse_define_temp_table_xml_options() {
    let source = r#"DEFINE TEMP-TABLE tt NO-UNDO
        NAMESPACE-URI "urn:test"
        SERIALIZE-NAME "myTable"
        FIELD x AS INTEGER."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineTempTable {
            xml_options,
            fields,
            ..
        } => {
            assert_eq!(xml_options.namespace_uri.as_ref().unwrap().name, "urn:test");
            assert_eq!(xml_options.serialize_name.as_ref().unwrap().name, "myTable");
            assert_eq!(fields.len(), 1);
        }
        _ => panic!("Expected DefineTempTable"),
    }
}

#[test]
fn parse_define_buffer_xml_options() {
    let source =
        r#"DEFINE BUFFER bCust FOR Customer NAMESPACE-URI "urn:foo" SERIALIZE-NAME "cust"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineBuffer { xml_options, .. } => {
            assert_eq!(xml_options.namespace_uri.as_ref().unwrap().name, "urn:foo");
            assert_eq!(xml_options.serialize_name.as_ref().unwrap().name, "cust");
        }
        _ => panic!("Expected DefineBuffer"),
    }
}

// ── PUBLISH tests ───────────────────────────────────────────────────

#[test]
fn parse_publish_string_literal() {
    let source = r#"PUBLISH "NewCustomer"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Publish {
            event_name,
            from_handle,
            arguments,
        } => {
            assert!(matches!(
                event_name.kind,
                ExpressionKind::Literal(Literal::String(_))
            ));
            assert!(from_handle.is_none());
            assert!(arguments.is_empty());
        }
        _ => panic!("Expected Publish statement"),
    }
}

#[test]
fn parse_publish_with_from() {
    let source = r#"PUBLISH "NewCustomer" FROM hProc."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Publish {
            from_handle,
            arguments,
            ..
        } => {
            assert!(from_handle.is_some());
            assert!(arguments.is_empty());
        }
        _ => panic!("Expected Publish statement"),
    }
}

#[test]
fn parse_publish_with_params() {
    let source = r#"PUBLISH "NewCustomer" (INPUT cName)."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Publish {
            arguments,
            from_handle,
            ..
        } => {
            assert!(from_handle.is_none());
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
        }
        _ => panic!("Expected Publish statement"),
    }
}

#[test]
fn parse_publish_expression_event() {
    let source = "PUBLISH cEventName.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Publish {
            event_name,
            arguments,
            ..
        } => {
            match event_name.kind {
                ExpressionKind::Identifier(id) => assert_eq!(id.name, "cEventName"),
                _ => panic!("Expected identifier event name"),
            }
            assert!(arguments.is_empty());
        }
        _ => panic!("Expected Publish statement"),
    }
}

// ── SUBSCRIBE tests ─────────────────────────────────────────────────

#[test]
fn parse_subscribe_anywhere() {
    let source = r#"SUBSCRIBE TO "NewCustomer" ANYWHERE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Subscribe {
            subscriber,
            target,
            run_procedure,
            no_error,
            ..
        } => {
            assert!(subscriber.is_none());
            assert_eq!(target, SubscribeTarget::Anywhere);
            assert!(run_procedure.is_none());
            assert!(!no_error);
        }
        _ => panic!("Expected Subscribe statement"),
    }
}

#[test]
fn parse_subscribe_in_handle() {
    let source = r#"SUBSCRIBE TO "NewCustomer" IN hPub."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Subscribe { target, .. } => {
            assert!(matches!(target, SubscribeTarget::InHandle(_)));
        }
        _ => panic!("Expected Subscribe statement"),
    }
}

#[test]
fn parse_subscribe_with_procedure() {
    let source = r#"SUBSCRIBE PROCEDURE hSub TO "NewCustomer" IN hPub."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Subscribe {
            subscriber, target, ..
        } => {
            assert!(subscriber.is_some());
            assert!(matches!(target, SubscribeTarget::InHandle(_)));
        }
        _ => panic!("Expected Subscribe statement"),
    }
}

#[test]
fn parse_subscribe_with_run_procedure() {
    let source = r#"SUBSCRIBE TO "NewCustomer" IN hPub RUN-PROCEDURE MyHandler."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Subscribe { run_procedure, .. } => {
            assert_eq!(run_procedure.as_ref().unwrap().name, "MyHandler");
        }
        _ => panic!("Expected Subscribe statement"),
    }
}

#[test]
fn parse_subscribe_no_to() {
    let source = r#"SUBSCRIBE "NewCustomer" ANYWHERE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Subscribe { target, .. } => {
            assert_eq!(target, SubscribeTarget::Anywhere);
        }
        _ => panic!("Expected Subscribe statement"),
    }
}

// ── UNSUBSCRIBE tests ───────────────────────────────────────────────

#[test]
fn parse_unsubscribe_event() {
    let source = r#"UNSUBSCRIBE TO "NewCustomer"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Unsubscribe {
            subscriber,
            event_name,
            in_handle,
        } => {
            assert!(subscriber.is_none());
            assert!(event_name.is_some());
            assert!(in_handle.is_none());
        }
        _ => panic!("Expected Unsubscribe statement"),
    }
}

#[test]
fn parse_unsubscribe_all() {
    let source = "UNSUBSCRIBE TO ALL.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Unsubscribe {
            event_name,
            in_handle,
            ..
        } => {
            assert!(event_name.is_none());
            assert!(in_handle.is_none());
        }
        _ => panic!("Expected Unsubscribe statement"),
    }
}

#[test]
fn parse_unsubscribe_with_procedure() {
    let source = r#"UNSUBSCRIBE PROCEDURE hSub TO "NewCustomer"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Unsubscribe { subscriber, .. } => {
            assert!(subscriber.is_some());
        }
        _ => panic!("Expected Unsubscribe statement"),
    }
}

#[test]
fn parse_unsubscribe_with_in_handle() {
    let source = r#"UNSUBSCRIBE TO "NewCustomer" IN hPub."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Unsubscribe { in_handle, .. } => {
            assert!(in_handle.is_some());
        }
        _ => panic!("Expected Unsubscribe statement"),
    }
}

// ── DEFINE EVENT tests ──────────────────────────────────────────────

#[test]
fn parse_define_event_minimal() {
    let source = "DEFINE EVENT MyEvent SIGNATURE VOID ().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineEvent {
            access,
            is_static,
            is_abstract,
            name,
            parameters,
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert!(!is_static);
            assert!(!is_abstract);
            assert_eq!(name.name, "MyEvent");
            assert!(parameters.is_empty());
        }
        _ => panic!("Expected DefineEvent statement"),
    }
}

#[test]
fn parse_define_event_abstract() {
    let source = "DEFINE PROTECTED ABSTRACT EVENT MyEvent SIGNATURE VOID ().";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineEvent {
            access,
            is_abstract,
            name,
            ..
        } => {
            assert_eq!(access, AccessModifier::Protected);
            assert!(is_abstract);
            assert_eq!(name.name, "MyEvent");
        }
        _ => panic!("Expected DefineEvent statement"),
    }
}

#[test]
fn parse_define_event_multiple_params() {
    let source = "DEFINE PUBLIC EVENT MyEvent SIGNATURE VOID (INPUT p1 AS INTEGER, INPUT p2 AS CHARACTER, OUTPUT p3 AS LOGICAL).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::DefineEvent {
            parameters, name, ..
        } => {
            assert_eq!(name.name, "MyEvent");
            assert_eq!(parameters.len(), 3);
            // Verify first param
            match &parameters[0].kind {
                StatementKind::DefineParameter {
                    direction,
                    param_type,
                } => {
                    assert_eq!(*direction, ParameterDirection::Input);
                    match param_type {
                        ParameterType::Variable {
                            name, type_source, ..
                        } => {
                            assert_eq!(name.name, "p1");
                            assert_eq!(*type_source, TypeSource::Explicit(DataType::Integer));
                        }
                        _ => panic!("Expected Variable param type"),
                    }
                }
                _ => panic!("Expected DefineParameter"),
            }
            // Verify third param is OUTPUT
            match &parameters[2].kind {
                StatementKind::DefineParameter { direction, .. } => {
                    assert_eq!(*direction, ParameterDirection::Output);
                }
                _ => panic!("Expected DefineParameter"),
            }
        }
        _ => panic!("Expected DefineEvent statement"),
    }
}

// ── Integration tests ───────────────────────────────────────────────

#[test]
fn parse_publish_in_do_block() {
    let source = r#"DO:
    PUBLISH "NewCustomer" (INPUT cName).
END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::Publish { .. }));
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_define_event_in_interface() {
    let source = "INTERFACE IObservable:
    DEFINE PUBLIC EVENT OnChange SIGNATURE VOID (INPUT pValue AS CHARACTER).
END INTERFACE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Interface { body, name, .. } => {
            assert_eq!(name.name, "IObservable");
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::DefineEvent { .. }));
        }
        _ => panic!("Expected Interface statement"),
    }
}

// ── Negative / disambiguation test ──────────────────────────────────

#[test]
fn parse_publish_event_name_not_function_call() {
    // Verify that `myEvent (INPUT x)` is parsed as event name `myEvent`
    // with argument list `(INPUT x)`, NOT as function call `myEvent(INPUT x)`.
    let source = "PUBLISH myEvent (INPUT x).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Publish {
            event_name,
            arguments,
            ..
        } => {
            // Event name should be a plain identifier, not a function call
            match event_name.kind {
                ExpressionKind::Identifier(id) => assert_eq!(id.name, "myEvent"),
                _ => panic!("Expected identifier event name, got {:?}", event_name),
            }
            // Arguments should be parsed as PUBLISH arguments
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
        }
        _ => panic!("Expected Publish statement"),
    }
}

// ── ON trigger tests ────────────────────────────────────────────────

#[test]
fn parse_on_choose_of_button() {
    let source = r#"ON CHOOSE OF btnOk DO: MESSAGE "clicked". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind:
                OnKind::UiEvent {
                    clauses,
                    anywhere,
                    action,
                },
        } => {
            assert!(!anywhere);
            assert_eq!(clauses.len(), 1);
            assert_eq!(clauses[0].events.len(), 1);
            assert_eq!(clauses[0].events[0].name, "CHOOSE");
            assert_eq!(clauses[0].widgets.len(), 1);
            assert_eq!(clauses[0].widgets[0].name.name, "btnOk");
            assert!(clauses[0].widgets[0].qualifier.is_none());
            assert!(matches!(action, OnAction::Block(_)));
        }
        _ => panic!("Expected On UiEvent statement, got {:?}", stmt),
    }
}

#[test]
fn parse_on_multiple_events() {
    let source = r#"ON CHOOSE, ENTRY OF btnOk DO: MESSAGE "hi". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { clauses, .. },
        } => {
            assert_eq!(clauses[0].events.len(), 2);
            assert_eq!(clauses[0].events[0].name, "CHOOSE");
            assert_eq!(clauses[0].events[1].name, "ENTRY");
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_multiple_widgets() {
    let source = r#"ON CHOOSE OF btn1, btn2 DO: MESSAGE "hi". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { clauses, .. },
        } => {
            assert_eq!(clauses[0].widgets.len(), 2);
            assert_eq!(clauses[0].widgets[0].name.name, "btn1");
            assert_eq!(clauses[0].widgets[1].name.name, "btn2");
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_widget_in_frame() {
    let source = r#"ON CHOOSE OF btnOk IN FRAME main-frame DO: MESSAGE "hi". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { clauses, .. },
        } => {
            assert_eq!(clauses[0].widgets.len(), 1);
            assert_eq!(clauses[0].widgets[0].name.name, "btnOk");
            match &clauses[0].widgets[0].qualifier {
                Some(WidgetQualifier::InFrame(frame)) => assert_eq!(frame.name, "main-frame"),
                other => panic!("Expected InFrame qualifier, got {:?}", other),
            }
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_or_clause() {
    let source = r#"ON CHOOSE OF btn1 OR ENTRY OF fill1 DO: MESSAGE "hi". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { clauses, .. },
        } => {
            assert_eq!(clauses.len(), 2);
            assert_eq!(clauses[0].events[0].name, "CHOOSE");
            assert_eq!(clauses[0].widgets[0].name.name, "btn1");
            assert_eq!(clauses[1].events[0].name, "ENTRY");
            assert_eq!(clauses[1].widgets[0].name.name, "fill1");
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_anywhere() {
    let source = r#"ON CHOOSE ANYWHERE DO: MESSAGE "hi". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent {
                clauses, anywhere, ..
            },
        } => {
            assert!(anywhere);
            assert!(clauses.is_empty());
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_anywhere_with_widgets() {
    let source = r#"ON CHOOSE OF btn1 ANYWHERE DO: MESSAGE "hi". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent {
                clauses, anywhere, ..
            },
        } => {
            assert!(anywhere);
            assert_eq!(clauses.len(), 1);
            assert_eq!(clauses[0].widgets[0].name.name, "btn1");
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_single_statement() {
    let source = r#"ON CHOOSE OF btnOk MESSAGE "clicked"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { action, .. },
        } => {
            assert!(matches!(action, OnAction::Block(_)));
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_revert() {
    let source = "ON CHOOSE OF btnOk REVERT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { action, .. },
        } => {
            assert!(matches!(action, OnAction::Revert));
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_persistent_run() {
    let source = "ON CHOOSE OF btnOk PERSISTENT RUN myProc.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { action, .. },
        } => match action {
            OnAction::PersistentRun {
                procedure,
                arguments,
                ..
            } => {
                assert_eq!(procedure.name, "myProc");
                assert!(arguments.is_empty());
            }
            other => panic!("Expected PersistentRun, got {:?}", other),
        },
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_persistent_run_with_args() {
    let source = "ON CHOOSE OF btnOk PERSISTENT RUN myProc (INPUT x).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { action, .. },
        } => match action {
            OnAction::PersistentRun { arguments, .. } => {
                assert_eq!(arguments.len(), 1);
            }
            other => panic!("Expected PersistentRun, got {:?}", other),
        },
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_leave_event_name() {
    let source = r#"ON LEAVE OF fill1 DO: MESSAGE "left". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { clauses, .. },
        } => {
            assert_eq!(clauses[0].events[0].name, "LEAVE");
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

#[test]
fn parse_on_web_notify() {
    let source = r#"ON "WEB-NOTIFY" ANYWHERE DO: MESSAGE "notify". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::UiEvent { anywhere, .. },
        } => {
            assert!(anywhere);
        }
        _ => panic!("Expected On UiEvent statement"),
    }
}

// ── ON database event tests ─────────────────────────────────────────

#[test]
fn parse_on_create_of_table() {
    let source = r#"ON CREATE OF Customer DO: MESSAGE "created". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind:
                OnKind::DbEvent {
                    event,
                    target,
                    is_override,
                    ..
                },
        } => {
            assert_eq!(event, DbTriggerEvent::Create);
            assert_eq!(target.name, "Customer");
            assert!(!is_override);
        }
        _ => panic!("Expected On DbEvent statement"),
    }
}

#[test]
fn parse_on_write_with_buffers() {
    let source =
        r#"ON WRITE OF Customer NEW BUFFER bNew OLD BUFFER bOld DO: MESSAGE "wrote". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::DbEvent {
                event, referencing, ..
            },
        } => {
            assert_eq!(event, DbTriggerEvent::Write);
            assert_eq!(referencing.new_buffer.as_ref().unwrap().name, "bNew");
            assert_eq!(referencing.old_buffer.as_ref().unwrap().name, "bOld");
        }
        _ => panic!("Expected On DbEvent statement"),
    }
}

#[test]
fn parse_on_assign_of_field() {
    let source = r#"ON ASSIGN OF Customer.Name DO: MESSAGE "assigned". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::DbEvent { event, target, .. },
        } => {
            assert_eq!(event, DbTriggerEvent::Assign);
            assert_eq!(target.name, "Customer.Name");
        }
        _ => panic!("Expected On DbEvent statement"),
    }
}

#[test]
fn parse_on_assign_old_value() {
    let source = r#"ON ASSIGN OF Customer.Name OLD VALUE oldName DO: MESSAGE "changed". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::DbEvent { referencing, .. },
        } => {
            assert_eq!(referencing.old_value.as_ref().unwrap().name, "oldName");
        }
        _ => panic!("Expected On DbEvent statement"),
    }
}

#[test]
fn parse_on_write_override() {
    let source = r#"ON WRITE OF Customer OVERRIDE DO: MESSAGE "overridden". END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::DbEvent { is_override, .. },
        } => {
            assert!(is_override);
        }
        _ => panic!("Expected On DbEvent statement"),
    }
}

#[test]
fn parse_on_db_revert() {
    let source = "ON WRITE OF Customer REVERT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind: OnKind::DbEvent { action, .. },
        } => {
            assert!(matches!(action, OnAction::Revert));
        }
        _ => panic!("Expected On DbEvent statement"),
    }
}

// ── ON key remap tests ──────────────────────────────────────────────

#[test]
fn parse_on_key_remap() {
    let source = "ON F1 HELP.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::On {
            kind:
                OnKind::KeyRemap {
                    key_label,
                    key_function,
                },
        } => {
            assert_eq!(key_label.name, "F1");
            assert_eq!(key_function.name, "HELP");
        }
        _ => panic!("Expected On KeyRemap statement"),
    }
}

// ── TRIGGER PROCEDURE tests ─────────────────────────────────────────

#[test]
fn parse_trigger_procedure_create() {
    let source = "TRIGGER PROCEDURE FOR CREATE OF Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::TriggerProcedure { event, target, .. } => {
            assert_eq!(event, DbTriggerEvent::Create);
            assert_eq!(target.name, "Customer");
        }
        _ => panic!("Expected TriggerProcedure statement"),
    }
}

#[test]
fn parse_trigger_procedure_write() {
    let source = "TRIGGER PROCEDURE FOR WRITE OF Customer NEW BUFFER bNew OLD BUFFER bOld.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::TriggerProcedure {
            event, referencing, ..
        } => {
            assert_eq!(event, DbTriggerEvent::Write);
            assert_eq!(referencing.new_buffer.as_ref().unwrap().name, "bNew");
            assert_eq!(referencing.old_buffer.as_ref().unwrap().name, "bOld");
        }
        _ => panic!("Expected TriggerProcedure statement"),
    }
}

#[test]
fn parse_trigger_procedure_write_no_buffer_keyword() {
    let source = "TRIGGER PROCEDURE FOR WRITE OF Customer NEW bNew OLD bOld.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::TriggerProcedure { referencing, .. } => {
            assert_eq!(referencing.new_buffer.as_ref().unwrap().name, "bNew");
            assert_eq!(referencing.old_buffer.as_ref().unwrap().name, "bOld");
        }
        _ => panic!("Expected TriggerProcedure statement"),
    }
}

#[test]
fn parse_trigger_procedure_assign_of() {
    let source = "TRIGGER PROCEDURE FOR ASSIGN OF Customer.Name.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::TriggerProcedure {
            event,
            target,
            new_value,
            ..
        } => {
            assert_eq!(event, DbTriggerEvent::Assign);
            assert_eq!(target.name, "Customer.Name");
            assert!(new_value.is_none());
        }
        _ => panic!("Expected TriggerProcedure statement"),
    }
}

#[test]
fn parse_trigger_procedure_assign_new_value() {
    let source = "TRIGGER PROCEDURE FOR ASSIGN NEW VALUE newVal AS CHARACTER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::TriggerProcedure {
            event,
            new_value,
            old_value_param,
            ..
        } => {
            assert_eq!(event, DbTriggerEvent::Assign);
            let nv = new_value.unwrap();
            assert_eq!(nv.name.name, "newVal");
            assert_eq!(nv.data_type, DataType::Character);
            assert!(old_value_param.is_none());
        }
        _ => panic!("Expected TriggerProcedure statement"),
    }
}

#[test]
fn parse_trigger_procedure_assign_new_old_value() {
    let source =
        "TRIGGER PROCEDURE FOR ASSIGN NEW VALUE newVal AS CHARACTER OLD VALUE oldVal AS CHARACTER.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::TriggerProcedure {
            new_value,
            old_value_param,
            ..
        } => {
            let nv = new_value.unwrap();
            assert_eq!(nv.name.name, "newVal");
            assert_eq!(nv.data_type, DataType::Character);
            let ov = old_value_param.unwrap();
            assert_eq!(ov.name.name, "oldVal");
            assert_eq!(ov.data_type, DataType::Character);
        }
        _ => panic!("Expected TriggerProcedure statement"),
    }
}

// ── ON trigger integration tests ────────────────────────────────────

#[test]
fn parse_on_trigger_in_procedure() {
    let source = r#"PROCEDURE myProc:
    ON CHOOSE OF btnOk DO:
        MESSAGE "clicked".
    END.
END PROCEDURE."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "myProc");
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::On { .. }));
        }
        _ => panic!("Expected Procedure statement"),
    }
}

#[test]
fn parse_on_trigger_inside_do_block() {
    let source = r#"DO:
    ON CHOOSE OF btnOk DO:
        MESSAGE "clicked".
    END.
END."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::On { .. }));
        }
        _ => panic!("Expected Do statement"),
    }
}

#[test]
fn parse_on_trigger_in_class_body() {
    let source = r#"CLASS MyApp.MyClass:
    ON CHOOSE OF btnOk DO:
        MESSAGE "clicked".
    END.
END CLASS."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Class { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StatementKind::On { .. }));
        }
        _ => panic!("Expected Class statement"),
    }
}

// ==================== Include File Reference Tests ====================

#[test]
fn parse_include_reference_statement() {
    let source = "{file.i}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::IncludeReference {
            path_and_args,
            span,
        } => {
            assert_eq!(path_and_args, "file.i");
            assert_eq!(span.start, 0);
            assert_eq!(span.end, 8);
        }
        _ => panic!("Expected IncludeReference statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_reference_with_path() {
    let source = "{globals/globals.i}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::IncludeReference { path_and_args, .. } => {
            assert_eq!(path_and_args, "globals/globals.i");
        }
        _ => panic!("Expected IncludeReference statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_reference_with_args() {
    let source = "{file.i NEW shared}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::IncludeReference { path_and_args, .. } => {
            assert_eq!(path_and_args, "file.i NEW shared");
        }
        _ => panic!("Expected IncludeReference statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_reference_no_period() {
    // Include references without trailing period should also parse
    // (since they expand to arbitrary code that may include its own period)
    let source = "{file.i}";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert!(matches!(stmt.kind, StatementKind::IncludeReference { .. }));
}

#[test]
fn parse_include_arg_reference_statement() {
    let source = "{1}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::IncludeArgReference { index, span } => {
            assert_eq!(index, 1);
            assert_eq!(span.start, 0);
            assert_eq!(span.end, 3);
        }
        _ => panic!("Expected IncludeArgReference statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_reference_as_expression() {
    // Include in expression position: x = {file.i}.
    let source = "x = {file.i}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { target, value } => {
            assert!(matches!(target.kind, ExpressionKind::Identifier(_)));
            match value.kind {
                ExpressionKind::IncludeReference { path_and_args, .. } => {
                    assert_eq!(path_and_args, "file.i");
                }
                _ => panic!("Expected IncludeReference expression, got {:?}", value),
            }
        }
        _ => panic!("Expected Assignment statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_arg_reference_as_expression() {
    // Include arg in expression position: x = {1}.
    let source = "x = {1}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { target, value } => {
            assert!(matches!(target.kind, ExpressionKind::Identifier(_)));
            match value.kind {
                ExpressionKind::IncludeArgReference { index, .. } => {
                    assert_eq!(index, 1);
                }
                _ => panic!("Expected IncludeArgReference expression, got {:?}", value),
            }
        }
        _ => panic!("Expected Assignment statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_inside_do_block() {
    let source = "DO: {body.i} END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(
                body[0].kind,
                StatementKind::IncludeReference { .. }
            ));
        }
        _ => panic!("Expected Do statement, got {:?}", stmt),
    }
}

#[test]
fn parse_include_inside_procedure() {
    let source = "PROCEDURE my-proc: {proc-body.i} END PROCEDURE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Procedure { name, body, .. } => {
            assert_eq!(name.name, "my-proc");
            assert_eq!(body.len(), 1);
            assert!(matches!(
                body[0].kind,
                StatementKind::IncludeReference { .. }
            ));
        }
        _ => panic!("Expected Procedure statement, got {:?}", stmt),
    }
}

#[test]
fn parse_multiple_includes() {
    let source = "{a.i} {b.i} {c.i}";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);

    let stmt1 = parser.parse_statement().expect("Expected first statement");
    assert!(matches!(stmt1.kind, StatementKind::IncludeReference { .. }));

    let stmt2 = parser.parse_statement().expect("Expected second statement");
    assert!(matches!(stmt2.kind, StatementKind::IncludeReference { .. }));

    let stmt3 = parser.parse_statement().expect("Expected third statement");
    assert!(matches!(stmt3.kind, StatementKind::IncludeReference { .. }));
}

#[test]
fn parse_include_with_named_args() {
    let source = r#"{inc/report.i &event="start" &stream-name="s-device"}."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::IncludeReference { path_and_args, .. } => {
            assert_eq!(
                path_and_args,
                r#"inc/report.i &event="start" &stream-name="s-device""#
            );
        }
        _ => panic!("Expected IncludeReference statement, got {:?}", stmt),
    }
}

#[test]
fn parse_case_when_or_when() {
    // ABL allows WHEN val1 OR WHEN val2 OR WHEN val3 THEN as a multi-value WHEN branch
    let source = "CASE sAction:\n    WHEN \"init\" OR WHEN ? OR WHEN \"\" THEN DO: RUN BuildInterface. END.\n    WHEN \"other\" THEN DO: END.\nEND CASE.\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

#[test]
fn parse_run_dataset_fill() {
    let source = "Run Dataset.Fill in hproc(input-output hhandle).\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

#[test]
fn parse_define_variable_extent_preprop() {
    let source = "def var x as handle extent {&DEPTH_MAX}.\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

#[test]
fn parse_do_counting_loop_in_frame_compound_name() {
    // Frame name is a compound preprop+identifier: {&tablename}f-builder
    let source = "do x = 1 to 10 in frame {&tablename}f-builder:\n   assign y = 1.\nend.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

#[test]
fn parse_create_widget_assign_triggers_block() {
    // CREATE widget ASSIGN ... TRIGGERS: ... END TRIGGERS. inside a DO block.
    // TRIGGERS lexes as Kind::Triggers, not Kind::Identifier — the block-end
    // detection must use Kind::Triggers, not string comparison.
    let source = "if x eq 0 then\ndo:\n   create sub-menu head-ptr\n      assign parent = app-menu\n             label  = x-menu.\nend.\nelse\ndo:\n   create menu-item head-ptr\n   assign parent = app-menu\n          label  = x-menu\n          sensitive = true\n   triggers:\n    on choose\n        persistent run value (\"lib/data-util.w\") (y).\n    end triggers.\nend.\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

#[test]
fn parse_repeat_counting_loop_compound_preprop_var() {
    // Loop variable is a compound preprop+identifier: {&tablename}i
    let source =
        "repeat {&tablename}i = 1 to {&tablename}btemp:num-fields:\n   assign x = 1.\nend.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

#[test]
fn parse_end_statement_without_trailing_period_at_eof() {
    // ABL allows the last statement in a file to omit the trailing period.
    // A comment after the last statement (before EoF) is also valid.
    for source in &[
        "end",
        "end /* end for each item-doc */",
        "assign x = 1",
        "assign x = 1 /* comment */",
        "do:\n   assign x = 1.\nend",
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let result = parser.parse_program();
        assert!(
            result.errors.is_empty(),
            "source {:?} produced errors: {:?}",
            source,
            result.errors
        );
    }
}

#[test]
fn dataset_preprop_compound_name_member_access() {
    // DATASET/BUFFER keyword followed by a name that includes an adjacent preprop
    // reference must consume the whole compound name so that postfix `:member`
    // access can be parsed. E.g. `dataset ds{&mainTable}:handle` or
    // `hdsDest:copy-dataset(hds{&mainTable},true,false,true)`.
    for source in &[
        "hdsDest = dataset ds{&mainTable}:handle.",
        "hdsDest:copy-dataset(hds{&mainTable},true,false,true).",
        "hdsDest:copy-dataset(dataset ds{&mainTable}:handle,true).",
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let result = parser.parse_program();
        assert!(
            result.errors.is_empty(),
            "source {:?} produced errors: {:?}",
            source,
            result.errors
        );
    }
}

#[test]
fn for_each_preprop_compound_buffer_name() {
    // FOR EACH with a {&preproc}table-name compound buffer name must consume
    // the full compound name so the WHERE clause and ELSE branch parse correctly.
    let source = r#"If "x" ne "y" then
Do:
   For each {&web}order-line where
       {&web}order-line.company eq company
   no-lock
   break by {&web}order-line.warehouse:
      if first-of( {&web}order-line.warehouse ) Then
      Do:
         assign dTax = 1.
      End.
   End.
End.
Else
Do:
   assign x = 2.
End."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    assert!(result.errors.is_empty(), "Errors: {:?}", result.errors);
}

// ===================== #66 xp-property BUFFER-FIELD fast path =====================

#[test]
fn buffer_value_unquoted_comma_list_standalone() {
    // ADM2 set expansion: unquoted comma-separated event list as BUFFER-VALUE.
    let source = "ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { value, .. } => match value.kind {
            ExpressionKind::Literal(Literal::String(s)) => {
                assert_eq!(
                    s.value,
                    "dataAvailable,confirmContinue,isUpdatePending,buildDataRequest"
                );
            }
            other => panic!("expected string literal value, got {other:?}"),
        },
        other => panic!("expected Assignment, got {other:?}"),
    }
}

#[test]
fn buffer_value_unquoted_comma_list_under_assign() {
    let source = "ASSIGN ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue,isUpdatePending,buildDataRequest.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 1);
            match &assignments[0].value.kind {
                ExpressionKind::Literal(Literal::String(s)) => {
                    assert_eq!(
                        s.value,
                        "dataAvailable,confirmContinue,isUpdatePending,buildDataRequest"
                    );
                }
                other => panic!("expected string literal value, got {other:?}"),
            }
        }
        other => panic!("expected Assign, got {other:?}"),
    }
}

#[test]
fn buffer_field_chained_lvalue_quoted_still_works() {
    let source = "ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = \"dataAvailable,confirmContinue\".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assignment { value, .. } => match value.kind {
            ExpressionKind::Literal(Literal::String(s)) => {
                assert_eq!(s.value, "dataAvailable,confirmContinue");
            }
            other => panic!("expected string literal, got {other:?}"),
        },
        other => panic!("expected Assignment, got {other:?}"),
    }
}

#[test]
fn bare_consecutive_buffer_field_assigns() {
    // xp-assign gated off: no ASSIGN keyword, two accessor pairs, one trailing period.
    let source = "\
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U
ghProp:BUFFER-FIELD('ContainerType':U):BUFFER-VALUE = '':U.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 2, "expected two bare assign pairs");
        }
        other => panic!("expected Assign with 2 pairs, got {other:?}"),
    }
}

#[test]
fn comma_list_then_no_error_under_assign() {
    let source = "ASSIGN ghProp:BUFFER-FIELD('x':U):BUFFER-VALUE = a,b,c NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 1);
            match &assignments[0].value.kind {
                ExpressionKind::Literal(Literal::String(s)) => assert_eq!(s.value, "a,b,c"),
                other => panic!("expected string list, got {other:?}"),
            }
        }
        other => panic!("expected Assign, got {other:?}"),
    }
}

#[test]
fn comma_list_then_second_assign_pair() {
    // Fold must stop before the next ASSIGN pair (space-separated, no comma between pairs).
    let source = "ASSIGN t1 = a,b t2 = c.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 2);
            match &assignments[0].value.kind {
                ExpressionKind::Literal(Literal::String(s)) => assert_eq!(s.value, "a,b"),
                other => panic!("expected folded list for t1, got {other:?}"),
            }
            match &assignments[1].value.kind {
                ExpressionKind::Identifier(id) => assert_eq!(id.name, "c"),
                other => panic!("expected identifier c for t2, got {other:?}"),
            }
        }
        other => panic!("expected Assign, got {other:?}"),
    }
}

#[test]
fn bare_multi_with_comma_list_value() {
    // Combined A+B: bare multi-pair where first value is a comma-list.
    let source = "\
ghProp:BUFFER-FIELD('DataSourceEvents':U):BUFFER-VALUE = dataAvailable,confirmContinue
ghProp:BUFFER-FIELD('ObjectType':U):BUFFER-VALUE = 'Procedure':U.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Assign { assignments } => {
            assert_eq!(assignments.len(), 2);
            match &assignments[0].value.kind {
                ExpressionKind::Literal(Literal::String(s)) => {
                    assert_eq!(s.value, "dataAvailable,confirmContinue");
                }
                other => panic!("expected folded comma list, got {other:?}"),
            }
        }
        other => panic!("expected Assign, got {other:?}"),
    }
}

#[test]
fn bare_assign_does_not_swallow_run() {
    // Negative: unterminated assignment followed by a real statement must not
    // treat RUN as a second bare assign pair.
    let source = "x = 1 RUN foo.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let result = parser.parse_program();
    // Either an error on the first statement, or recovery — but must not parse
    // the whole line as a single successful multi-assign with a RUN target.
    let has_run_as_assign = result.statements.iter().any(|s| {
        matches!(
            &s.kind,
            StatementKind::Assign { assignments } if assignments.len() > 1
        )
    });
    assert!(
        !has_run_as_assign,
        "RUN must not be absorbed as a bare assign pair: {:?}",
        result.statements
    );
    // Primary expectation: parse error about missing period (or recovery Empty).
    assert!(
        !result.errors.is_empty()
            || result
                .statements
                .iter()
                .any(|s| matches!(s.kind, StatementKind::Empty)),
        "expected error or recovery for `x = 1 RUN foo.`, got stmts={:?} errs={:?}",
        result.statements,
        result.errors
    );
}

// ---- oxabl#79: per-statement block-body error recovery ----

#[test]
fn block_survives_in_body_error_keeping_header_and_statements() {
    // A stray `else` inside the DO body is an in-body parse error. The DO must
    // survive with its while_condition header and the well-formed statements
    // parsed before and after the error; the error is still reported.
    let source = "do while not done:\n\
                    x = 1.\n\
                    else foo.\n\
                    y = 2.\n\
                  end.\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();

    assert!(
        !program.errors.is_empty(),
        "the in-body error must still be reported"
    );
    assert_eq!(program.statements.len(), 1, "the DO block must survive");
    match &program.statements[0].kind {
        StatementKind::Do {
            while_condition,
            body,
            ..
        } => {
            assert!(
                while_condition.is_some(),
                "DO WHILE header condition must be preserved"
            );
            // Both well-formed assignments (`x = 1`, `y = 2`) survive recovery.
            let assigns = body
                .iter()
                .filter(|s| matches!(s.kind, StatementKind::Assignment { .. }))
                .count();
            assert_eq!(
                assigns, 2,
                "both surrounding assignments must survive: {body:?}"
            );
        }
        other => panic!("expected surviving Do block, got {other:?}"),
    }
}

#[test]
fn unterminated_block_recovers_and_survives() {
    // Missing END at EOF is recovered: the DO survives with its header rather
    // than being discarded, and the missing-END error is reported.
    let source = "do while not lcheck:\n  x = 1.\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();

    assert!(
        !program.errors.is_empty(),
        "an unterminated block must still report an error"
    );
    assert_eq!(program.statements.len(), 1, "the DO block must survive");
    assert!(
        matches!(
            &program.statements[0].kind,
            StatementKind::Do {
                while_condition: Some(_),
                ..
            }
        ),
        "unterminated DO WHILE must survive with its header: {:?}",
        program.statements[0].kind
    );
}

#[test]
fn nested_block_error_does_not_orphan_outer_block() {
    // An error deep in an inner block must be contained there; the outer DO and
    // the inner DO both survive with intact nesting.
    let source = "do while a:\n\
                    do while b:\n\
                      else oops.\n\
                    end.\n\
                    z = 1.\n\
                  end.\n";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();

    assert!(!program.errors.is_empty(), "inner error must be reported");
    assert_eq!(program.statements.len(), 1, "outer DO must survive");
    match &program.statements[0].kind {
        StatementKind::Do { body, .. } => {
            // Inner DO survived as a nested statement, and `z = 1` after it too.
            assert!(
                body.iter()
                    .any(|s| matches!(s.kind, StatementKind::Do { .. })),
                "inner DO must survive nested: {body:?}"
            );
            assert!(
                body.iter()
                    .any(|s| matches!(s.kind, StatementKind::Assignment { .. })),
                "statement after inner block must survive: {body:?}"
            );
        }
        other => panic!("expected outer Do, got {other:?}"),
    }
}

// ---------------------------------------------------------------------------
// Wrapper-span seeding (feat: full-fidelity spans on Statement/Expression).
// Covers U2 (statement spans incl. trailing `.`/`:`), U3 (expression spans
// across precedence/postfix/parens/prefix), and U4 (sibling source-order
// debug_assert). See docs/plans/2026-07-22-001-feat-ast-wrapper-spans-plan.md.
// ---------------------------------------------------------------------------

fn parse_program_stmts(source: &str) -> Vec<Statement> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_program().statements
}

fn parse_one_expr(source: &str) -> Expression {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_expression().expect("expected an expression")
}

#[test]
fn stmt_span_covers_leading_keyword_through_trailing_period() {
    let source = "DEFINE VARIABLE x AS INTEGER.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 1);
    // Full extent: first token through and including the trailing period.
    assert_eq!(stmts[0].span.start, 0);
    assert_eq!(stmts[0].span.end as usize, source.len());
}

#[test]
fn stmt_span_do_block_covers_end_period_and_nests() {
    let source = "DO:\n  DISPLAY x.\nEND.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 1);
    // Outer DO spans the whole block including the final `END.` period.
    assert_eq!(stmts[0].span.start, 0);
    assert_eq!(stmts[0].span.end as usize, source.len());
    // The nested body statement carries its own correct sub-span.
    if let StatementKind::Do { body, .. } = &stmts[0].kind {
        assert_eq!(body.len(), 1);
        let disp = &body[0];
        let d_start = source.find("DISPLAY").unwrap();
        // "DISPLAY x." ends at the period after x.
        let d_end = source.find("x.").unwrap() + "x.".len();
        assert_eq!(disp.span.start as usize, d_start);
        assert_eq!(disp.span.end as usize, d_end);
    } else {
        panic!("expected Do, got {:?}", stmts[0].kind);
    }
}

#[test]
fn stmt_span_multiline_assign_ends_at_final_period() {
    let source = "ASSIGN x = 1\n       y = 2.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 1);
    assert_eq!(stmts[0].span.start, 0);
    assert_eq!(stmts[0].span.end as usize, source.len());
}

#[test]
fn empty_stmt_from_stray_period_is_period_width_and_non_overlapping() {
    // Leading stray period → Empty (period-width), then a real statement.
    let source = ". DISPLAY x.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 2);
    // First Empty covers just the leading period.
    assert!(matches!(stmts[0].kind, StatementKind::Empty));
    assert_eq!(stmts[0].span.start, 0);
    assert_eq!(stmts[0].span.end, 1);
    // Non-overlapping with the following statement.
    assert!(stmts[0].span.end <= stmts[1].span.start);
    assert_eq!(stmts[1].span.end as usize, source.len());
}

#[test]
fn expr_span_precedence_tree_shape() {
    // a + b * c : outer Add spans a..c; inner Multiply spans b..c; leaves single.
    let source = "a + b * c";
    let expr = parse_one_expr(source);
    assert_eq!(expr.span, Span { start: 0, end: 9 });
    if let ExpressionKind::Add(lhs, rhs) = &expr.kind {
        assert_eq!(lhs.span, Span { start: 0, end: 1 }); // a
        assert_eq!(rhs.span, Span { start: 4, end: 9 }); // b * c
        if let ExpressionKind::Multiply(m_lhs, m_rhs) = &rhs.kind {
            assert_eq!(m_lhs.span, Span { start: 4, end: 5 }); // b
            assert_eq!(m_rhs.span, Span { start: 8, end: 9 }); // c
        } else {
            panic!("expected Multiply, got {:?}", rhs.kind);
        }
    } else {
        panic!("expected Add, got {:?}", expr.kind);
    }
}

#[test]
fn expr_span_parenthesized_includes_parens() {
    let source = "(a + b)";
    let expr = parse_one_expr(source);
    // Node span includes the enclosing parens (round-trip fidelity).
    assert_eq!(expr.span, Span { start: 0, end: 7 });
    assert!(matches!(expr.kind, ExpressionKind::Add(..)));
}

#[test]
fn expr_span_prefix_negate_starts_at_operator() {
    let source = "-x";
    let expr = parse_one_expr(source);
    assert_eq!(expr.span, Span { start: 0, end: 2 });
    if let ExpressionKind::Negate(inner) = &expr.kind {
        assert_eq!(inner.span, Span { start: 1, end: 2 });
    } else {
        panic!("expected Negate, got {:?}", expr.kind);
    }
}

#[test]
fn expr_span_prefix_not_starts_at_operator() {
    let source = "NOT x";
    let expr = parse_one_expr(source);
    assert_eq!(expr.span, Span { start: 0, end: 5 });
    assert!(matches!(expr.kind, ExpressionKind::Not(_)));
}

#[test]
fn expr_span_ternary_starts_at_if() {
    let source = "IF c THEN a ELSE b";
    let expr = parse_one_expr(source);
    assert_eq!(expr.span.start, 0);
    assert_eq!(expr.span.end as usize, source.len());
    assert!(matches!(expr.kind, ExpressionKind::IfThenElse(..)));
}

#[test]
fn expr_span_function_call_covers_through_close_paren() {
    let source = "foo(a, b)";
    let expr = parse_one_expr(source);
    assert_eq!(expr.span, Span { start: 0, end: 9 });
    if let ExpressionKind::FunctionCall { arguments, .. } = &expr.kind {
        assert_eq!(arguments.len(), 2);
        assert_eq!(arguments[0].span, Span { start: 4, end: 5 }); // a
        assert_eq!(arguments[1].span, Span { start: 7, end: 8 }); // b
    } else {
        panic!("expected FunctionCall, got {:?}", expr.kind);
    }
}

#[test]
fn expr_span_postfix_method_chain() {
    // obj:method(arg) — MethodCall spans obj..close-paren; object span is just obj.
    let source = "obj:method(arg)";
    let expr = parse_one_expr(source);
    assert_eq!(expr.span.start, 0);
    assert_eq!(expr.span.end as usize, source.len());
    if let ExpressionKind::MethodCall { object, .. } = &expr.kind {
        assert_eq!(object.span, Span { start: 0, end: 3 }); // obj
    } else {
        panic!("expected MethodCall, got {:?}", expr.kind);
    }
}

#[test]
#[cfg(debug_assertions)]
#[should_panic(expected = "out of source order")]
fn u4_out_of_order_sibling_statements_trip_assert() {
    let mut a = Statement::new(StatementKind::Empty);
    a.span = Span { start: 10, end: 20 };
    let mut b = Statement::new(StatementKind::Empty);
    b.span = Span { start: 5, end: 8 }; // starts before `a` ends → out of order
    debug_assert_stmt_sibling_order(&[a, b]);
}

#[test]
fn u4_zero_width_empty_between_reals_is_tolerated() {
    let mut a = Statement::new(StatementKind::Empty);
    a.span = Span { start: 0, end: 5 };
    let mut mid = Statement::new(StatementKind::Empty);
    mid.span = Span { start: 5, end: 5 }; // zero-width synthetic (R1.4)
    let mut b = Statement::new(StatementKind::Empty);
    b.span = Span { start: 5, end: 10 };
    // Must not panic — `<=` tolerates zero-width nodes abutting neighbours.
    debug_assert_stmt_sibling_order(&[a, mid, b]);
}

// ---------------------------------------------------------------------------
// U2: comment side-table collection (`Program.comments`)
// ---------------------------------------------------------------------------

/// Parse `source` and return its collected comment table.
fn collect_program_comments(source: &str) -> Vec<oxabl_ast::Comment> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_program().comments
}

#[test]
fn comments_single_line_includes_trailing_newline() {
    // Mirrors lexer test `comments_line`: the `//` span includes the `\n`.
    let source = "def // this is a comment\nvar x as int.";
    let comments = collect_program_comments(source);
    assert_eq!(comments.len(), 1);
    assert_eq!(comments[0].kind, CommentKind::Line);
    // `//` starts at byte 4; the span runs through and includes the `\n` at 24.
    assert_eq!(comments[0].span, Span { start: 4, end: 25 });
    assert_eq!(&source[..source.len()][24..25], "\n");
}

#[test]
fn comments_single_block_excludes_following_newline() {
    let source = "def /* block */ var\n";
    let comments = collect_program_comments(source);
    assert_eq!(comments.len(), 1);
    assert_eq!(comments[0].kind, CommentKind::Block);
    // Full extent through `*/`, excluding the following space/newline.
    assert_eq!(comments[0].span, Span { start: 4, end: 15 });
    assert_eq!(&source[4..15], "/* block */");
}

#[test]
fn comments_nested_block_is_one_entry() {
    let source = "a = /* a /* b */ c */ b.";
    let comments = collect_program_comments(source);
    assert_eq!(comments.len(), 1);
    assert_eq!(comments[0].kind, CommentKind::Block);
    let s = comments[0].span;
    assert_eq!(
        &source[s.start as usize..s.end as usize],
        "/* a /* b */ c */"
    );
}

#[test]
fn comments_are_sorted_by_start_without_manual_sort() {
    let source = "/* first */ a = 1. // second\nb = 2. /* third */";
    let comments = collect_program_comments(source);
    assert_eq!(comments.len(), 3);
    assert_eq!(comments[0].kind, CommentKind::Block);
    assert_eq!(comments[1].kind, CommentKind::Line);
    assert_eq!(comments[2].kind, CommentKind::Block);
    // Ascending by span.start, strictly increasing (non-overlapping).
    assert!(comments[0].span.start < comments[1].span.start);
    assert!(comments[1].span.start < comments[2].span.start);
}

#[test]
fn comments_between_and_trailing_statements_do_not_overlap_nodes() {
    let source = "do:\nend. /* done */";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert_eq!(program.comments.len(), 1);
    let c = program.comments[0].span;
    assert_eq!(&source[c.start as usize..c.end as usize], "/* done */");
    // The trailing comment must start after the last statement's span ends.
    let last = program.statements.last().expect("a statement");
    assert!(last.span.end <= c.start, "comment overlaps statement span");
}

#[test]
fn comments_leading_only_file_still_captured() {
    // A file whose only comments are leading: `has_comments` is `false`, yet
    // the collector must still capture them (KTD2 regression guard).
    let source = "// leading one\n/* leading two */\nvar x as int.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    // Sanity: the leading skip in `Parser::new` cleared the fast-path flag.
    assert!(!parser.has_comments, "expected leading-only fast-path");
    let program = parser.parse_program();
    assert_eq!(program.comments.len(), 2);
    assert_eq!(program.comments[0].kind, CommentKind::Line);
    assert_eq!(program.comments[1].kind, CommentKind::Block);
}

#[test]
fn comments_appbuilder_directive_is_line_excluding_newline() {
    let source = "&ANALYZE-SUSPEND _VERSION-NUMBER\nvar x as int.";
    let comments = collect_program_comments(source);
    assert_eq!(comments.len(), 1);
    assert_eq!(comments[0].kind, CommentKind::Line);
    let s = comments[0].span;
    // Span excludes the trailing `\n` (KTD3 asymmetry vs `//`).
    assert_eq!(
        &source[s.start as usize..s.end as usize],
        "&ANALYZE-SUSPEND _VERSION-NUMBER"
    );
    assert_eq!(&source[s.end as usize..s.end as usize + 1], "\n");
}

#[test]
fn comments_include_and_macro_refs_are_not_comments() {
    let source = "{lib/util.i}\n{&MY-MACRO}\nvar x as int.";
    let comments = collect_program_comments(source);
    assert!(
        comments.is_empty(),
        "include/preprocessor refs must not enter the comment table: {comments:?}"
    );
}

#[test]
fn comments_unterminated_block_is_not_captured() {
    // Unterminated block comment lexes to `Kind::Invalid`, not `Kind::Comment`.
    let source = "var x as int. /* never closed";
    let comments = collect_program_comments(source);
    assert!(
        comments.is_empty(),
        "unterminated block must be excluded: {comments:?}"
    );
}

#[test]
fn comments_no_comment_file_is_empty() {
    let source = "var x as int.\ndisplay x.";
    let comments = collect_program_comments(source);
    assert!(comments.is_empty());
}

#[test]
fn classify_comment_defensive_brace_branch_is_excluded() {
    // The `{`-led branch of the leading-byte gate is defensive: no real lexer
    // output produces it, so it is exercised as a direct function-level test
    // (KTD3). A synthetic `{`-led span must classify to `None` (excluded)
    // without tripping the debug_assert on the fallthrough arm.
    let source = "{not a comment}";
    let tokens = tokenize(source);
    let parser = Parser::new(&tokens, source);
    assert_eq!(parser.classify_comment(0), None);
    // And the two live shapes classify as expected via the same entry point.
    let line_src = "// hi";
    let lt = tokenize(line_src);
    let lp = Parser::new(&lt, line_src);
    assert_eq!(lp.classify_comment(0), Some(CommentKind::Line));
    let block_src = "/* hi */";
    let bt = tokenize(block_src);
    let bp = Parser::new(&bt, block_src);
    assert_eq!(bp.classify_comment(0), Some(CommentKind::Block));
    let amp_src = "&ANALYZE-SUSPEND x";
    let at = tokenize(amp_src);
    let ap = Parser::new(&at, amp_src);
    assert_eq!(ap.classify_comment(0), Some(CommentKind::Line));
}

// ---------------------------------------------------------------------------
// Recognized-but-unmodelled statement forms → StatementKind::Skipped (#128)
// ---------------------------------------------------------------------------

/// The dispatch keyword is the range's first token and is dropped
/// unconditionally, so `UNDERLINE` does not harvest itself.
#[test]
fn skipped_underline_harvests_the_variable_not_the_keyword() {
    assert_eq!(harvest("UNDERLINE v-total."), vec!["v-total"]);
}

/// `UNIX` consumes its own keyword before skipping, where `UNDERLINE` does not.
/// Anchoring the range on the statement's first token normalizes both shapes,
/// so the harvest looks the same from the outside.
#[test]
fn skipped_unix_drops_the_field_half_of_a_qualified_name() {
    assert_eq!(harvest("UNIX supplier.supp-id."), vec!["supplier"]);
}

/// The broad `can_be_identifier` classifier admits unreserved option keywords
/// as candidate names. Accepted, not accidental (KTD5): ABL lexes a user's
/// variable named `format` as `Kind::Format`, so inside a statement whose
/// grammar we do not model the two cannot be told apart. The cost is a possible
/// false negative on a variable named after an option keyword; the alternative
/// costs a false positive on every such variable.
#[test]
fn skipped_harvest_keeps_option_keywords_as_candidate_names() {
    assert_eq!(
        harvest(r#"UNDERLINE v-total FORMAT ">>9"."#),
        vec!["v-total", "FORMAT"]
    );
}

/// Adjacency, not mere precedence: a spaced `/` is a division operator, and its
/// right operand is a real read.
#[test]
fn skipped_harvest_keeps_spaced_division_operands() {
    assert_eq!(
        harvest("UNDERLINE v-total / v-count."),
        vec!["v-total", "v-count"]
    );
}

/// String literals are not identifier-shaped, so the string drops out while
/// the widget and frame names survive.
#[test]
fn skipped_color_harvests_widget_and_frame_not_the_string() {
    let names = harvest(r#"COLOR DISPLAY "CHOOSE" btn-ok WITH FRAME f-main."#);
    assert!(names.contains(&"btn-ok".to_string()), "got {names:?}");
    assert!(names.contains(&"f-main".to_string()), "got {names:?}");
    assert!(!names.iter().any(|n| n.contains("CHOOSE")), "got {names:?}");
}

/// Every component of an absolute path is byte-adjacent to a preceding `/` or
/// `.`, so a redirect to a file harvests nothing at all — and allocates nothing.
#[test]
fn skipped_output_to_path_harvests_nothing() {
    assert!(harvest("OUTPUT TO /usr/tmp/log.txt.").is_empty());
}

/// Forms with no plausible identifier content are converted anyway: a uniform
/// rule cannot drift, and the filter yields little or nothing there regardless.
#[test]
fn skipped_forms_without_identifier_content_harvest_almost_nothing() {
    assert!(harvest("PAGE.").is_empty());
    // `ERROR` is identifier-capable and survives; `ON`, `UNDO` and `THROW` are
    // not. The residue is the accepted keyword-collision case from KTD5 in its
    // most harmless form: it can only exempt a variable literally named
    // `error`, and only from a rule that would otherwise call it unused.
    assert_eq!(harvest("BLOCK-LEVEL ON ERROR UNDO, THROW."), vec!["ERROR"]);
}

/// `skip_to_statement_end_editing_aware` parses the `EDITING:` body and throws
/// it away, but the cursor still advances past it, so the harvest reaches those
/// tokens. A statement-leading name inside the block follows a terminating
/// period *and a space*, so the adjacency filter does not eat it.
#[test]
fn skipped_editing_block_names_survive_the_harvest() {
    let names = harvest("PROMPT-FOR v-name WITH FRAME f-main EDITING: v-x = 1. v-y = 2. END.");
    assert!(names.contains(&"v-name".to_string()), "got {names:?}");
    assert!(names.contains(&"v-x".to_string()), "got {names:?}");
    assert!(names.contains(&"v-y".to_string()), "got {names:?}");
}

/// The nested-statement save/restore in the `parse_statement` funnel: an
/// `EDITING:` body recurses through `parse_statement`, and without restoring the
/// anchor the outer harvest range would start at the last inner statement.
#[test]
fn skipped_editing_block_does_not_clobber_the_outer_harvest_anchor() {
    let names = harvest("PROMPT-FOR v-first WITH FRAME f-main EDITING: v-x = 1. END.");
    assert_eq!(
        names.first().map(String::as_str),
        Some("v-first"),
        "outer anchor lost — got {names:?}"
    );
}

/// The thirteen non-dispatch skip-helper callers keep their real statement node
/// and discard the range. `DISPLAY ... WITH FRAME` is one of them: it must not
/// have been swept up in the conversion.
#[test]
fn display_with_frame_is_still_a_display_not_a_skipped() {
    let source = "DISPLAY v-name WITH FRAME f-main.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("statement should parse");
    assert!(
        matches!(stmt.kind, StatementKind::Display { .. }),
        "got {:?}",
        stmt.kind
    );
}

/// A `Skipped` node's span still covers the statement's full extent, so no
/// companion `raw_span` field is needed (ast-invariants §8).
#[test]
fn skipped_span_covers_the_whole_statement() {
    let source = "PUT UNFORMATTED v-total SKIP.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("statement should parse");
    assert_eq!(stmt.span.start, 0);
    assert_eq!(stmt.span.end as usize, source.len());
}

// ---------------------------------------------------------------------------
// Table-candidate marking on the three #130 forms
// ---------------------------------------------------------------------------

/// The marker is opt-in. An ordinary unmodelled form pays no table lookup, so
/// the roughly thirty #128 dispatch sites are unchanged in behavior and cost.
#[test]
fn skipped_ordinary_form_is_not_a_table_candidate() {
    assert!(!marks_tables("UNDERLINE v-total."));
    assert!(!marks_tables("UNDERLINE supplier.supp-id."));
}

/// `DEFINE QUERY q FOR tt.` names its backing table, so its harvest is offered
/// to the table namespaces. `q` rides along as a candidate — the grammar is not
/// modelled until #136, so the harvest cannot tell the query's own name from the
/// tables it is defined over. A candidate that resolves to nothing is silent.
#[test]
fn skipped_define_query_is_a_table_candidate() {
    assert!(marks_tables("DEFINE QUERY q FOR ttItem."));
    let names = harvest("DEFINE QUERY q FOR ttItem.");
    assert!(names.iter().any(|n| n == "q"), "got {names:?}");
    assert!(names.iter().any(|n| n == "ttItem"), "got {names:?}");
}

#[test]
fn skipped_open_query_is_a_table_candidate() {
    assert!(marks_tables("OPEN QUERY q FOR EACH ttItem."));
    let names = harvest("OPEN QUERY q FOR EACH ttItem.");
    assert!(names.iter().any(|n| n == "ttItem"), "got {names:?}");
}

/// Only `OPEN QUERY` marks. The bulk dispatch arm matches a bare `Kind::Open`,
/// and the other shapes routed through it name no table — marking them would
/// widen table lookup past the three forms #130 owns.
#[test]
fn skipped_non_query_open_stays_unmarked() {
    assert!(!marks_tables("OPEN foo."));
}

/// A comment between OPEN and QUERY is legal ABL and the rest of the parser
/// tolerates one at every keyword boundary. A raw one-token lookahead here would
/// see the comment, skip the marker, and hand the table back its false positive.
#[test]
fn skipped_open_query_is_marked_across_an_interleaved_comment() {
    assert!(marks_tables("OPEN /* which */ QUERY q FOR EACH ttItem."));
}

/// `EMPTY TEMP-TABLE` is the one #130 form whose table name the parser knows
/// exactly, so it contributes precisely that one identifier rather than a
/// lexical sweep: no `NO-ERROR`, no `TEMP-TABLE` keyword.
#[test]
fn skipped_empty_temp_table_harvests_exactly_the_table() {
    assert!(marks_tables("EMPTY TEMP-TABLE ttItem NO-ERROR."));
    assert_eq!(harvest("EMPTY TEMP-TABLE ttItem NO-ERROR."), vec!["ttItem"]);
    // The `TEMP-TABLE` keyword is optional in the same statement.
    assert_eq!(harvest("EMPTY ttItem."), vec!["ttItem"]);
}

/// A form with no table name must not panic and must harvest nothing — there is
/// no candidate to offer, and inventing one would credit an unrelated symbol.
#[test]
fn skipped_empty_temp_table_without_a_name_harvests_nothing() {
    assert!(harvest("EMPTY TEMP-TABLE.").is_empty());
}

/// The span rule holds for the newly-`Skipped` form too (ast-invariants §8):
/// `EMPTY TEMP-TABLE` used to return a recovery `Empty`, and its replacement
/// must still cover the statement's full extent including the trailing period.
#[test]
fn skipped_empty_temp_table_span_covers_the_whole_statement() {
    let source = "EMPTY TEMP-TABLE ttItem NO-ERROR.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("statement should parse");
    assert_eq!(stmt.span.start, 0);
    assert_eq!(stmt.span.end as usize, source.len());
}

#[test]
fn for_each_fields_and_except_phrases_keep_where_and_body() {
    for source in [
        "FOR EACH customer FIELDS (name) NO-LOCK WHERE customer.name EQ \"a\":\n  MESSAGE 1.\nEND.",
        "FOR EACH customer EXCEPT (name city) NO-LOCK WHERE customer.name EQ \"a\":\n  MESSAGE 1.\nEND.",
        "FOR FIRST customer FIELDS (name) WHERE customer.name EQ \"a\" NO-LOCK:\n  MESSAGE 1.\nEND.",
    ] {
        let stmts = parse_program_stmts(source);
        assert_eq!(stmts.len(), 1, "{source}");
        match &stmts[0].kind {
            StatementKind::ForEach {
                buffer,
                where_clause,
                lock_type,
                body,
                ..
            } => {
                assert_eq!(buffer.name, "customer");
                assert!(where_clause.is_some(), "{source}");
                assert_eq!(*lock_type, LockType::NoLock);
                assert_eq!(body.len(), 1, "{source}");
                assert!(matches!(body[0].kind, StatementKind::Message { .. }));
            }
            other => panic!("expected ForEach, got {other:?}"),
        }
    }
}

#[test]
fn for_each_fields_phrase_on_joined_table_is_skipped() {
    let source =
        "FOR EACH a NO-LOCK, EACH b FIELDS (x y) NO-LOCK WHERE b.x EQ a.x:\n  MESSAGE 1.\nEND.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 1);
    match &stmts[0].kind {
        StatementKind::ForEach { body, .. } => assert_eq!(body.len(), 1),
        other => panic!("expected ForEach, got {other:?}"),
    }
}

#[test]
fn catch_and_finally_in_any_block_body_carry_real_spans() {
    let sources = [
        "PROCEDURE p:\n  MESSAGE 1.\n  FINALLY:\n    MESSAGE 2.\n  END FINALLY.\nEND PROCEDURE.",
        "CLASS C:\n  METHOD PUBLIC VOID M():\n    MESSAGE 1.\n    CATCH e AS Progress.Lang.Error:\n      MESSAGE 2.\n    END CATCH.\n  END METHOD.\nEND CLASS.",
        "DO:\n  MESSAGE 1.\n  CATCH e AS Progress.Lang.Error:\n    MESSAGE 2.\n  END CATCH.\nEND.",
    ];
    for source in sources {
        let stmts = parse_program_stmts(source);
        fn find<'a>(stmts: &'a [Statement], out: &mut Vec<&'a Statement>) {
            for s in stmts {
                match &s.kind {
                    StatementKind::Catch { .. } | StatementKind::Finally { .. } => out.push(s),
                    StatementKind::Procedure { body, .. }
                    | StatementKind::Class { body, .. }
                    | StatementKind::Method { body, .. }
                    | StatementKind::Do { body, .. } => find(body, out),
                    _ => {}
                }
            }
        }
        let mut found = Vec::new();
        find(&stmts, &mut found);
        assert_eq!(found.len(), 1, "{source}");
        let kw = if matches!(found[0].kind, StatementKind::Catch { .. }) {
            "CATCH e"
        } else {
            "FINALLY"
        };
        assert_eq!(found[0].span.start as usize, source.find(kw).unwrap());
        assert!(source[found[0].span.start as usize..found[0].span.end as usize].ends_with('.'));
    }
}

#[test]
fn on_trigger_do_block_carries_its_real_span() {
    let source = "ON CHOOSE OF b IN FRAME f DO:\n  MESSAGE 1.\nEND.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 1);
    match &stmts[0].kind {
        StatementKind::On {
            kind: OnKind::UiEvent { action, .. },
        } => match action {
            OnAction::Block(block) => {
                assert_eq!(block.span.start as usize, source.find("DO:").unwrap());
                assert_eq!(block.span.end as usize, source.len());
            }
            other => panic!("expected a block action, got {other:?}"),
        },
        other => panic!("expected ON, got {other:?}"),
    }
}

// ---- annotations -----------------------------------------------------------

fn parse_with_errors(source: &str) -> Program {
    let tokens = tokenize(source);
    Parser::new(&tokens, source).parse_program()
}

#[track_caller]
fn only_annotation(source: &str) -> oxabl_ast::Annotation {
    let program = parse_with_errors(source);
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert_eq!(program.statements.len(), 1, "{source}");
    match &program.statements[0].kind {
        StatementKind::Annotation(a) => a.clone(),
        other => panic!("expected an annotation, got {other:?}"),
    }
}

#[test]
fn bare_annotation_parses() {
    let a = only_annotation("@Test.");
    assert_eq!(a.name.name, "Test");
    assert!(a.qualifier.is_none());
    assert!(a.attributes.is_empty());
}

#[test]
fn annotation_span_covers_at_through_period() {
    let source = "MESSAGE 1.\n  @Test.\nMESSAGE 2.";
    let stmts = parse_program_stmts(source);
    assert_eq!(stmts.len(), 3);
    assert_span_covers(source, stmts[1].span, "@Test.");
}

#[test]
fn annotation_name_span_excludes_the_at_sign() {
    let source = "@strict.effects(reads=\"Customer\").";
    let a = only_annotation(source);
    assert_span_covers(source, a.name.span, "strict.effects");
}

#[test]
fn annotation_with_attributes() {
    let a = only_annotation("@Test (expected=\"Progress.Lang.AppError\", ignore=\"x\").");
    assert_eq!(a.attributes.len(), 2);
    assert_eq!(a.attributes[0].name.name, "expected");
    assert_eq!(
        a.attributes[0].value,
        oxabl_ast::AnnotationValue::String("Progress.Lang.AppError".into())
    );
    assert_eq!(a.attribute("IGNORE").unwrap().value.as_str(), "x");
}

#[test]
fn dotted_annotation_name_with_attributes() {
    let a = only_annotation("@strict.effects(reads=\"Customer\", writes=\"Order\").");
    assert_eq!(a.name.name, "strict.effects");
    assert_eq!(a.attribute("reads").unwrap().value.as_str(), "Customer");
    assert_eq!(a.attribute("writes").unwrap().value.as_str(), "Order");
}

#[test]
fn annotation_name_may_contain_reserved_words() {
    let a = only_annotation("@openapi.openedge.export(type=\"REST\").");
    assert_eq!(a.name.name, "openapi.openedge.export");
    assert_eq!(a.attribute("type").unwrap().value.as_str(), "REST");
}

#[test]
fn annotation_with_qualifier() {
    let a =
        only_annotation("@openapi.openedge.export FILE(type=\"REST\", useReturnValue=\"false\").");
    assert_eq!(a.name.name, "openapi.openedge.export");
    assert_eq!(a.qualifier.as_ref().unwrap().name, "FILE");
    assert_eq!(a.attributes.len(), 2);
}

#[test]
fn annotation_empty_parens_and_single_quotes() {
    assert!(only_annotation("@Test().").attributes.is_empty());
    let a = only_annotation("@Test(a='x').");
    assert_eq!(a.attribute("a").unwrap().value.as_str(), "x");
}

#[test]
fn annotation_unquoted_value_is_kept_bare() {
    let a = only_annotation("@Test(count=3, flag=yes).");
    assert_eq!(
        a.attribute("count").unwrap().value,
        oxabl_ast::AnnotationValue::Bare("3".into())
    );
    assert_eq!(
        a.attribute("flag").unwrap().value,
        oxabl_ast::AnnotationValue::Bare("yes".into())
    );
}

#[test]
fn trailing_period_is_not_part_of_the_name() {
    let source = "@Test.\nPROCEDURE p:\nEND PROCEDURE.";
    let program = parse_with_errors(source);
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert_eq!(program.statements.len(), 2);
    assert!(matches!(
        &program.statements[0].kind,
        StatementKind::Annotation(a) if a.name.name == "Test"
    ));
    assert!(matches!(
        program.statements[1].kind,
        StatementKind::Procedure { .. }
    ));
}

#[test]
fn annotations_inside_a_class_body() {
    let source = "CLASS Sample:\n  @Test.\n  METHOD PUBLIC VOID Works():\n  END METHOD.\n\n  \
                  @Test (expected=\"X\").\n  METHOD PUBLIC VOID Fails():\n  END METHOD.\nEND CLASS.";
    let program = parse_with_errors(source);
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let StatementKind::Class { body, .. } = &program.statements[0].kind else {
        panic!("expected a class");
    };
    let pairs = oxabl_ast::associate_annotations(body);
    assert_eq!(pairs.len(), 2);
    assert!(
        pairs
            .iter()
            .all(|p| matches!(p.target.unwrap().kind, StatementKind::Method { .. }))
    );
}

#[test]
fn annotation_without_final_period_at_eof() {
    assert_eq!(only_annotation("@Test").name.name, "Test");
}

#[test]
fn malformed_annotation_is_an_error_and_recovers() {
    for source in [
        "@.\nMESSAGE 1.",
        "@Test(expected).\nMESSAGE 1.",
        "@Test(expected=).\nMESSAGE 1.",
        "@Test(a=\"x\" b=\"y\").\nMESSAGE 1.",
        "@Test(a=\"x\".\nMESSAGE 1.",
    ] {
        let program = parse_with_errors(source);
        assert!(!program.errors.is_empty(), "{source}");
    }
}

#[test]
fn field_format_at_sign_is_unaffected() {
    let program = parse_with_errors("DISPLAY x @ y.");
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert!(
        !program
            .statements
            .iter()
            .any(|s| matches!(s.kind, StatementKind::Annotation(_)))
    );
}

#[test]
fn lone_at_signs_are_reported_at_the_first_one() {
    let source = "@ @ @\nMESSAGE 1.";
    let program = parse_with_errors(source);
    assert_eq!(program.errors.len(), 1, "{:?}", program.errors);
    assert_eq!(program.errors[0].span.start, 0);
}

#[test]
fn annotation_named_at_is_a_name() {
    assert_eq!(only_annotation("@at.").name.name, "at");
}

#[test]
fn create_call_is_a_handle_create() {
    let stmts = parse_program_stmts("CREATE CALL hCall IN WIDGET-POOL \"p\" NO-ERROR.");
    assert_eq!(stmts.len(), 1);
    match &stmts[0].kind {
        StatementKind::Create {
            target:
                CreateTarget::Handle {
                    kind,
                    handle,
                    widget_pool,
                    ..
                },
            no_error,
        } => {
            assert_eq!(*kind, CreateTargetKind::Call);
            assert_eq!(expr_name(handle), "hCall");
            assert!(widget_pool.is_some());
            assert!(*no_error);
        }
        other => panic!("expected CREATE CALL, got {other:?}"),
    }
}

#[test]
fn create_on_a_table_named_call_stays_a_record_create() {
    let stmts = parse_program_stmts("CREATE call.");
    assert!(matches!(
        &stmts[0].kind,
        StatementKind::Create {
            target: CreateTarget::Name(_),
            ..
        } | StatementKind::Skipped { .. }
    ));
}

#[test]
fn no_undo_after_view_as_is_recorded() {
    let stmts = parse_program_stmts(
        "DEFINE VARIABLE fi AS CHARACTER FORMAT \"x(8)\" VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.",
    );
    assert!(matches!(
        &stmts[0].kind,
        StatementKind::VariableDeclaration { no_undo: true, .. }
    ));
    let stmts =
        parse_program_stmts("DEFINE VARIABLE fi AS CHARACTER VIEW-AS FILL-IN SIZE 10 BY 1.");
    assert!(matches!(
        &stmts[0].kind,
        StatementKind::VariableDeclaration { no_undo: false, .. }
    ));
}

#[test]
fn dynamic_new_is_a_function_call_over_the_class_and_arguments() {
    let stmts = parse_program_stmts("h = DYNAMIC-NEW cls (1, x).");
    let StatementKind::Assignment { value, .. } = &stmts[0].kind else {
        panic!("expected assignment, got {:?}", stmts[0].kind);
    };
    let ExpressionKind::FunctionCall { name, arguments } = &value.kind else {
        panic!("expected a call, got {:?}", value.kind);
    };
    assert_eq!(name.name, "DYNAMIC-NEW");
    assert!(!arguments.is_empty());
}

#[test]
fn define_workfile_declares_a_temp_table() {
    for source in [
        "DEFINE WORK-TABLE wt FIELD a AS INTEGER FIELD b LIKE t.f.",
        "DEFINE SHARED WORKFILE wt FIELD a AS INTEGER FIELD b LIKE t.f.",
    ] {
        let program = Parser::new(&tokenize(source), source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        let StatementKind::DefineTempTable { name, fields, .. } = &program.statements[0].kind
        else {
            panic!(
                "expected DefineTempTable, got {:?}",
                program.statements[0].kind
            );
        };
        assert_eq!(name.name, "wt");
        assert_eq!(fields.len(), 2);
    }
}

#[test]
fn a_block_header_on_phrase_keeps_its_action_expression() {
    for source in [
        "DO ON ERROR UNDO, RETURN ERROR NEW pkg.Err(): END.",
        "DO ON ERROR UNDO, THROW NEW pkg.Err(): END.",
        "DO ON ENDKEY UNDO, LEAVE ON ERROR UNDO, RETURN ERROR NEW pkg.Err(): END.",
        "REPEAT ON ERROR UNDO, RETURN ERROR NEW pkg.Err(): END.",
        "FOR EACH Customer NO-LOCK ON ERROR UNDO, RETURN ERROR NEW pkg.Err(): END.",
        "FOR EACH Customer NO-LOCK BY Customer.Name ON ERROR UNDO, RETURN ERROR NEW pkg.Err(): END.",
        "DO ON ERROR UNDO, RETURN ERROR \"bad\" + STRING(1): END.",
    ] {
        let program = Parser::new(&tokenize(source), source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        let on_actions = match &program.statements[0].kind {
            StatementKind::Do { on_actions, .. }
            | StatementKind::Repeat { on_actions, .. }
            | StatementKind::ForEach { on_actions, .. } => on_actions,
            other => panic!("{source}: expected a block, got {other:?}"),
        };
        assert_eq!(on_actions.len(), 1, "{source}");
    }
}

#[test]
fn a_block_header_on_phrase_without_a_value_keeps_no_action_expression() {
    for source in [
        "DO ON ERROR UNDO, RETURN ERROR: END.",
        "DO ON ERROR UNDO, THROW: END.",
        "DO ON ERROR UNDO, RETURN NO-APPLY: END.",
        "REPEAT ON STOP UNDO, RETURN ERROR ON QUIT UNDO, NEXT: END.",
        "FOR EACH Customer NO-LOCK ON ERROR UNDO, LEAVE: END.",
    ] {
        let program = Parser::new(&tokenize(source), source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        let on_actions = match &program.statements[0].kind {
            StatementKind::Do { on_actions, .. }
            | StatementKind::Repeat { on_actions, .. }
            | StatementKind::ForEach { on_actions, .. } => on_actions,
            other => panic!("{source}: expected a block, got {other:?}"),
        };
        assert!(on_actions.is_empty(), "{source}");
    }
}

#[test]
fn accum_aggregate_word_is_not_an_argument() {
    let source = "DISPLAY ACCUM TOTAL BY t.cust t.amount.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let StatementKind::Display { items, .. } = &program.statements[0].kind else {
        panic!("expected DISPLAY, got {:?}", program.statements[0].kind);
    };
    let ExpressionKind::FunctionCall { arguments, .. } = &items[0].expression.kind else {
        panic!("expected ACCUM call, got {:?}", items[0].expression.kind);
    };
    assert_eq!(arguments.len(), 1);
}

#[test]
fn get_codepage_and_get_codepages_parse_as_expressions() {
    let source = "cPage = GET-CODEPAGE(lcText).\ncPages = GET-CODEPAGES.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert_eq!(program.statements.len(), 2);
}

#[test]
fn member_reference_named_null_parses() {
    let source = "IF oObj:GetType(\"v\") = JsonDataType:NULL THEN MESSAGE \"null\".";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
}

#[test]
fn class_header_accepts_serializable() {
    for source in [
        "CLASS Failure INHERITS Progress.Lang.AppError SERIALIZABLE:\nEND CLASS.",
        "CLASS Failure FINAL SERIALIZABLE:\nEND CLASS.",
        "CLASS Failure USE-WIDGET-POOL SERIALIZABLE:\nEND CLASS.",
    ] {
        let program = Parser::new(&tokenize(source), source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
    }
}

#[test]
fn run_target_takes_every_path_shape_the_avm_accepts() {
    for (source, expected) in [
        ("RUN a/b.c.p.", "a/b.c.p"),
        ("RUN a.b/c.p.", "a.b/c.p"),
        ("RUN a.b.c.", "a.b.c"),
        ("RUN util\\export.p.", "util\\export.p"),
        ("RUN x\\y\\z.p (1).", "x\\y\\z.p"),
        ("RUN 3rd/x.p.", "3rd/x.p"),
        ("RUN 3rd.p.", "3rd.p"),
        ("RUN ./sub2.p.", "./sub2.p"),
        ("RUN ../x/y.p.", "../x/y.p"),
        ("RUN ./../x/y.p.", "./../x/y.p"),
        ("RUN ../../x.p.", "../../x.p"),
        ("RUN ..\\x\\y.p.", "..\\x\\y.p"),
        ("RUN .\\x.p.", ".\\x.p"),
        ("RUN a/b.c.p PERSISTENT SET h.", "a/b.c.p"),
        ("RUN a/b.c.p(1).", "a/b.c.p"),
        ("RUN a/b.c.p. RUN d.p.", "a/b.c.p"),
    ] {
        let tokens = tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        let StatementKind::Run {
            target: RunTarget::Literal {
                name, name_span, ..
            },
            ..
        } = &program.statements[0].kind
        else {
            panic!("{source}: {:?}", program.statements[0].kind);
        };
        assert_eq!(name, expected, "{source}");
        assert_eq!(
            &source[name_span.start as usize..name_span.end as usize],
            expected,
            "{source}"
        );
    }
}

#[test]
fn a_bare_number_is_not_a_run_target() {
    let source = "RUN 12.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(!program.errors.is_empty());
}

/// Parse `source` and require exactly the statements in `source` with no error.
#[track_caller]
fn parse_clean(source: &str) -> Vec<Statement> {
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
    program.statements
}

#[test]
fn using_import_makes_keyword_named_class_a_data_type() {
    let stmts = parse_clean(
        "USING Progress.Reflect.Method.\n\
         USING Progress.Reflect.Property.\n\
         DEFINE VARIABLE oMethod AS Method NO-UNDO.\n\
         DEFINE VARIABLE aProps AS Property EXTENT NO-UNDO.\n\
         DEFINE VARIABLE oLower AS method NO-UNDO.",
    );
    let types: Vec<_> = stmts
        .iter()
        .filter_map(|s| match &s.kind {
            StatementKind::VariableDeclaration {
                type_source: TypeSource::Explicit(t),
                ..
            } => Some(t.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        types,
        [
            DataType::Class("Method".into()),
            DataType::Class("Property".into()),
            DataType::Class("method".into()),
        ]
    );
}

#[test]
fn keyword_named_class_without_using_is_still_rejected() {
    let source = "DEFINE VARIABLE oMethod AS Method NO-UNDO.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(!program.errors.is_empty());
}

#[test]
fn using_import_keyword_class_in_var_statement() {
    let stmts = parse_clean("USING Progress.Reflect.Method.\nVAR Method oMethod.");
    assert!(matches!(
        &stmts[1].kind,
        StatementKind::VariableDeclaration {
            type_source: TypeSource::Explicit(DataType::Class(c)),
            ..
        } if c == "Method"
    ));
}

#[test]
fn var_statement_comma_separated_names() {
    let source = "VAR LOGICAL a, b = TRUE, c.";
    let stmts = parse_clean(source);
    assert_eq!(stmts.len(), 1);
    let StatementKind::VariableDeclarationList(decls) = &stmts[0].kind else {
        panic!("expected a list, got {:?}", stmts[0].kind);
    };
    let names: Vec<_> = decls
        .iter()
        .map(|d| match &d.kind {
            StatementKind::VariableDeclaration {
                name,
                type_source: TypeSource::Explicit(DataType::Logical),
                initial_value,
                no_undo: true,
                ..
            } => (name.name.as_str(), initial_value.is_some()),
            other => panic!("expected a logical declaration, got {other:?}"),
        })
        .collect();
    assert_eq!(names, [("a", false), ("b", true), ("c", false)]);
    assert_eq!(
        &source[decls[1].span.start as usize..decls[1].span.end as usize],
        "b = TRUE"
    );
}

#[test]
fn var_statement_class_type() {
    let stmts = parse_clean("VAR CLASS Foo.Bar baz.\nVAR CLASS Foo.Bar x, y.");
    assert!(matches!(
        &stmts[0].kind,
        StatementKind::VariableDeclaration {
            type_source: TypeSource::Explicit(DataType::Class(c)),
            name,
            ..
        } if c == "Foo.Bar" && name.name == "baz"
    ));
    assert!(matches!(
        &stmts[1].kind,
        StatementKind::VariableDeclarationList(d) if d.len() == 2
    ));
}

fn first_param_type(source: &str) -> ParameterType {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt.kind {
        StatementKind::Function { body, .. } => match &body[0].kind {
            StatementKind::DefineParameter { param_type, .. } => param_type.clone(),
            other => panic!("Expected DefineParameter, got {other:?}"),
        },
        other => panic!("Expected Function, got {other:?}"),
    }
}

#[test]
fn procedure_signature_buffer_param_keeps_its_table() {
    for source in [
        "FUNCTION f RETURNS INTEGER (BUFFER bItem FOR ttItem):\nRETURN 1.\nEND FUNCTION.",
        "FUNCTION f RETURNS INTEGER (BUFFER bItem FOR TEMP-TABLE ttItem):\nRETURN 1.\nEND FUNCTION.",
    ] {
        match first_param_type(source) {
            ParameterType::Buffer { name, target } => {
                assert_eq!(name.name, "bItem");
                assert_eq!(target.name, "ttItem");
            }
            other => panic!("Expected Buffer, got {other:?}"),
        }
    }
}

#[test]
fn unresolved_include_in_call_argument_position_does_not_break_the_list() {
    use oxabl_common::FileId;
    use oxabl_preprocessor::Preprocessor;
    let source = "RUN foo.p({inc/args.i},\n          INPUT RECID(item)).\n";
    let fs = oxabl_workspace::InMemoryFileSystem::new();
    let include_paths = Vec::new();
    let result = Preprocessor::new(&fs, &include_paths)
        .process(FileId::new(1), source)
        .unwrap();
    assert_eq!(result.unresolved_includes.len(), 1);
    let text = result.to_text();
    let tokens = tokenize(&text);
    let mut parser = Parser::new(&tokens, &text);
    let program = parser.parse_program();
    assert!(program.is_ok(), "got {:?}", program.first_error());
}

/// Statement and option keywords sampled as member names.
const KEYWORD_MEMBER_NAMES: &[&str] = &[
    "Fetch", "Delete", "Find", "Create", "Release", "Run", "Display", "Message", "Return", "Do",
    "If", "For", "Repeat", "Case", "End", "When", "Else", "Then", "Null", "Open", "Close", "Get",
    "Set", "Update", "Insert", "Input", "Output", "Define", "Assign", "Leave", "Next", "Quit",
    "Apply", "Enable", "Disable", "Export", "Import", "Put", "Read", "Trigger", "Using", "Where",
    "Wait-For", "Disp",
];

fn assert_parses_clean(source: &str) -> Vec<Statement> {
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
    program.statements
}

#[test]
fn a_keyword_is_a_method_name_after_a_colon() {
    for kw in KEYWORD_MEMBER_NAMES {
        let source = format!("r = o:{kw}(a).");
        let stmts = assert_parses_clean(&source);
        let StatementKind::Assignment { value, .. } = &stmts[0].kind else {
            panic!("{source}: expected assignment, got {:?}", stmts[0].kind);
        };
        let ExpressionKind::MethodCall { method, .. } = &value.kind else {
            panic!("{source}: expected a method call, got {:?}", value.kind);
        };
        assert_eq!(method.name, *kw, "{source}");
    }
}

#[test]
fn a_keyword_is_a_property_name_after_a_colon() {
    for kw in KEYWORD_MEMBER_NAMES {
        let source = format!("r = o:{kw}.");
        let stmts = assert_parses_clean(&source);
        let StatementKind::Assignment { value, .. } = &stmts[0].kind else {
            panic!("{source}: expected assignment, got {:?}", stmts[0].kind);
        };
        let ExpressionKind::MemberAccess { member, .. } = &value.kind else {
            panic!("{source}: expected member access, got {:?}", value.kind);
        };
        assert_eq!(member.name, *kw, "{source}");
    }
}

#[test]
fn a_keyword_member_parses_in_statement_condition_and_chain_positions() {
    for kw in KEYWORD_MEMBER_NAMES {
        for source in [
            format!("o:{kw}(a)."),
            format!("o:{kw} = 1."),
            format!("MESSAGE o:{kw}(a)."),
            format!("IF o:{kw} THEN MESSAGE \"x\"."),
            format!("r = o:First():{kw}(a):Last()."),
            format!("r = NEW Foo():{kw}(a):{kw}."),
            format!("r = Foo:{kw}(a)."),
        ] {
            assert_parses_clean(&source);
        }
    }
}

#[test]
fn a_colon_followed_by_a_space_and_a_keyword_still_ends_the_block_header() {
    let source = "\
FOR EACH t WHERE t.a = x: DISPLAY t.a. END.
CASE x: WHEN 1 THEN MESSAGE \"one\". OTHERWISE MESSAGE \"other\". END CASE.
";
    assert_parses_clean(source);
}

#[test]
fn a_block_keyword_member_at_statement_start_is_not_a_label() {
    for source in ["o:Do(a).", "o:Do.", "o:For = 1.", "o:Repeat(1, 2)."] {
        let stmts = assert_parses_clean(source);
        assert!(
            !matches!(stmts[0].kind, StatementKind::Label { .. }),
            "{source}: {:?}",
            stmts[0].kind
        );
    }
}

#[test]
fn a_label_directly_against_its_block_keyword_is_still_a_label() {
    for source in [
        "lbl:DO: MESSAGE \"x\". END.",
        "lbl: REPEAT: LEAVE lbl. END.",
        "lbl:FOR EACH t: DISPLAY t.a. END.",
        "lbl:\nDO.\n  MESSAGE \"x\".\nEND.",
        "lbl :\nREPEAT .\n  LEAVE lbl.\nEND.",
    ] {
        let stmts = assert_parses_clean(source);
        assert!(
            matches!(stmts[0].kind, StatementKind::Label { .. }),
            "{source}: {:?}",
            stmts[0].kind
        );
    }
}

#[test]
fn an_override_property_with_a_computed_getter_is_a_property_statement() {
    for source in [
        "DEFINE PROTECTED OVERRIDE PROPERTY P AS CHARACTER NO-UNDO\n  GET():\n    RETURN \"x\".\n  END GET.",
        "DEFINE OVERRIDE PROTECTED PROPERTY P AS CHARACTER NO-UNDO GET.",
        "DEFINE PUBLIC OVERRIDE PROPERTY P AS INTEGER NO-UNDO GET. SET.",
    ] {
        let program = Parser::new(&tokenize(source), source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
        assert!(
            matches!(program.statements[0].kind, StatementKind::Property { .. }),
            "{source}: {:?}",
            program.statements[0].kind
        );
    }
}

#[test]
fn new_as_the_last_token_does_not_panic() {
    for source in [
        "DO ON ERROR UNDO, THROW NEW",
        "x = new",
        "IF NEW",
        "y = NEW pkg.",
    ] {
        let _ = Parser::new(&tokenize(source), source).parse_program();
    }
}

#[test]
fn a_do_block_accepts_transaction_after_its_on_phrases() {
    for colon in [":", "\n  :"] {
        let source = format!(
            "PROCEDURE p:\n  DO ON ERROR UNDO main, LEAVE main ON STOP UNDO main, LEAVE main TRANSACTION{colon}\n    MESSAGE \"x\".\n  END.\nEND PROCEDURE.\nMESSAGE \"after\"."
        );
        let stmts = assert_parses_clean(&source);
        assert_eq!(stmts.len(), 2, "{source}: {stmts:?}");
        let StatementKind::Procedure { body, .. } = &stmts[0].kind else {
            panic!("expected a procedure, got {:?}", stmts[0].kind);
        };
        let StatementKind::Do { transaction, .. } = &body[0].kind else {
            panic!("expected DO, got {:?}", body[0].kind);
        };
        assert!(*transaction, "{source}");
    }
}

#[test]
fn a_method_accepts_the_package_protected_access_modifier() {
    for source in [
        "METHOD PACKAGE-PROTECTED VOID m():\nEND METHOD.",
        "METHOD PACKAGE-PRIVATE VOID m():\nEND METHOD.",
        "METHOD STATIC PACKAGE-PROTECTED INTEGER m():\n  RETURN 1.\nEND METHOD.",
    ] {
        let stmts = assert_parses_clean(source);
        assert!(
            matches!(stmts[0].kind, StatementKind::Method { .. }),
            "{source}: {:?}",
            stmts[0].kind
        );
    }
    let stmts = assert_parses_clean("METHOD PACKAGE-PROTECTED VOID m():\nEND METHOD.");
    let StatementKind::Method { access, .. } = &stmts[0].kind else {
        unreachable!()
    };
    assert_eq!(*access, AccessModifier::PackageProtected);
}

#[test]
fn an_external_procedure_accepts_its_options_in_any_order() {
    for options in [
        "ORDINAL 5",
        "CDECL",
        "PASCAL",
        "STDCALL",
        "PERSISTENT",
        "THREAD-SAFE",
        "CDECL ORDINAL 5",
        "ORDINAL 5 CDECL",
        "ORDINAL 5 STDCALL PERSISTENT",
        "PERSISTENT THREAD-SAFE PASCAL ORDINAL 12",
        "THREAD-SAFE ORDINAL {&ord} CDECL",
    ] {
        let source = format!(
            "PROCEDURE p EXTERNAL \"lib.so\" {options}:\n  DEFINE INPUT PARAMETER x AS LONG.\nEND PROCEDURE.\nMESSAGE \"after\"."
        );
        let stmts = assert_parses_clean(&source);
        assert_eq!(stmts.len(), 2, "{source}: {stmts:?}");
        let StatementKind::Procedure {
            body, is_prototype, ..
        } = &stmts[0].kind
        else {
            panic!("{source}: expected a procedure, got {:?}", stmts[0].kind);
        };
        assert!(*is_prototype, "{source}");
        assert_eq!(body.len(), 1, "{source}");
    }
}

#[test]
fn an_option_keyword_of_an_external_procedure_is_still_a_name() {
    let source = "DEFINE VARIABLE ordinal AS INTEGER NO-UNDO.\nDEFINE VARIABLE cdecl AS INTEGER NO-UNDO.\nDEFINE VARIABLE pascal AS INTEGER NO-UNDO.\nDEFINE VARIABLE stdcall AS INTEGER NO-UNDO.\nordinal = cdecl + pascal * stdcall.\nMESSAGE ordinal cdecl pascal stdcall.";
    assert_parses_clean(source);
}

fn statements_in_first_method(stmts: &[Statement]) -> &[Statement] {
    let StatementKind::Class { body, .. } = &stmts[0].kind else {
        panic!("expected a class, got {:?}", stmts[0].kind);
    };
    body.iter()
        .find_map(|s| match &s.kind {
            StatementKind::Method { name, body, .. } if name.name == "Run1" => {
                Some(body.as_slice())
            }
            _ => None,
        })
        .expect("method Run1")
}

#[test]
fn a_receiverless_call_to_a_method_named_like_a_statement_is_a_call() {
    for name in ["Release", "Delete", "Create", "Display"] {
        let source = format!(
            "CLASS C:\n  METHOD PUBLIC VOID {name}(x AS INTEGER):\n  END METHOD.\n  METHOD PUBLIC VOID Run1():\n    {name}(1).\n    {name}(1) NO-ERROR.\n  END METHOD.\nEND CLASS."
        );
        let stmts = assert_parses_clean(&source);
        let body = statements_in_first_method(&stmts);
        assert_eq!(body.len(), 2, "{source}: {body:?}");
        for stmt in body {
            let StatementKind::ExpressionStatement(expr) = &stmt.kind else {
                panic!("{name}: expected a call statement, got {:?}", stmt.kind);
            };
            assert!(
                matches!(
                    expr.kind,
                    ExpressionKind::FunctionCall { .. } | ExpressionKind::MethodCall { .. }
                ),
                "{name}: {:?}",
                expr.kind
            );
        }
    }
}

#[test]
fn a_method_declared_after_its_call_still_makes_the_word_a_call() {
    let source = "CLASS C:\n  METHOD PUBLIC VOID Run1():\n    Release(1).\n  END METHOD.\n  METHOD PRIVATE VOID Release(x AS INTEGER):\n  END METHOD.\nEND CLASS.";
    let stmts = assert_parses_clean(source);
    let body = statements_in_first_method(&stmts);
    assert!(
        matches!(body[0].kind, StatementKind::ExpressionStatement(_)),
        "{:?}",
        body[0].kind
    );
}

#[test]
fn a_statement_keyword_without_a_declared_method_keeps_its_statement_reading() {
    let source = "CLASS C:\n  METHOD PUBLIC VOID Run1():\n    DISPLAY(x) WITH FRAME f.\n  END METHOD.\n  METHOD PUBLIC VOID Other():\n  END METHOD.\nEND CLASS.";
    let stmts = assert_parses_clean(source);
    let body = statements_in_first_method(&stmts);
    assert!(
        !matches!(body[0].kind, StatementKind::ExpressionStatement(_)),
        "{:?}",
        body[0].kind
    );
}

#[test]
fn a_statement_keyword_with_a_space_before_the_paren_keeps_its_statement_reading() {
    let source = "CLASS C:\n  METHOD PUBLIC VOID Display(x AS INTEGER):\n  END METHOD.\n  METHOD PUBLIC VOID Run1():\n    DISPLAY (1) WITH FRAME f.\n    DISPLAY(1) WITH FRAME f.\n  END METHOD.\nEND CLASS.";
    let stmts = assert_parses_clean(source);
    let body = statements_in_first_method(&stmts);
    for stmt in body {
        assert!(
            !matches!(stmt.kind, StatementKind::ExpressionStatement(_)),
            "{:?}",
            stmt.kind
        );
    }
}

fn tail_names(stmt: &Statement) -> Vec<&str> {
    stmt.skipped_tail
        .as_ref()
        .map(|t| t.names.iter().map(|n| n.name.as_str()).collect())
        .unwrap_or_default()
}

#[test]
fn a_skipped_tail_keeps_the_names_it_passed_over() {
    let stmts = assert_parses_clean("DISPLAY a WITH FRAME f TITLE \"x\" + v-title + tbl.fld.");
    assert!(matches!(stmts[0].kind, StatementKind::Display { .. }));
    // `fld` is byte-adjacent to a `.`, so only the qualifier survives.
    let names = tail_names(&stmts[0]);
    assert!(
        names.contains(&"v-title") && names.contains(&"tbl"),
        "{names:?}"
    );
    assert!(!names.contains(&"fld"), "{names:?}");
}

#[test]
fn a_statement_with_no_skipped_identifiers_carries_no_tail() {
    let stmts = assert_parses_clean("DISPLAY a.\nDEFINE STREAM s.\n");
    assert!(stmts.iter().all(|s| s.skipped_tail.is_none()));
}

#[test]
fn a_skipped_tail_does_not_include_modelled_tokens() {
    let stmts = assert_parses_clean("RUN p.p (INPUT a) NO-ERROR v-extra ).");
    assert!(matches!(stmts[0].kind, StatementKind::Run { .. }));
    assert_eq!(tail_names(&stmts[0]), ["v-extra"]);
}

#[test]
fn a_function_forward_declaration_in_super_keeps_its_tail() {
    let stmts = assert_parses_clean("FUNCTION f RETURNS INTEGER (INPUT a AS INTEGER) IN h-super.");
    assert!(matches!(stmts[0].kind, StatementKind::Function { .. }));
    assert_eq!(tail_names(&stmts[0]), ["h-super"]);
}

#[test]
fn a_dropped_assign_target_is_part_of_the_skipped_tail() {
    let stmts = assert_parses_clean("ASSIGN a = 1 v-target.");
    assert!(matches!(stmts[0].kind, StatementKind::Assign { .. }));
    assert_eq!(tail_names(&stmts[0]), ["v-target"]);
}

#[test]
fn harvest_drops_a_dynamic_buffer_field() {
    assert_eq!(harvest("UNDERLINE hbuf::company."), vec!["hbuf"]);
}

#[test]
fn put_is_head_parsed_with_stream_and_items() {
    let source = "PUT STREAM s1 UNFORMATTED a \"x\" SKIP b + 1 FORMAT \"x(5)\" AT 10 tail-var.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let stmt = &program.statements[0];
    let StatementKind::Put { stream_name, items } = &stmt.kind else {
        panic!("expected Put, got {:?}", stmt.kind);
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "s1");
    assert_eq!(items.len(), 3);
    let tail = stmt.skipped_tail.as_ref().expect("options stay a tail");
    assert!(tail.names.iter().any(|n| n.name == "tail-var"));
}

#[test]
fn os_file_statements_harvest_only_value_operands() {
    let source = "OS-DELETE VALUE(path-var) SAVE silent.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    let StatementKind::Skipped { names, .. } = &program.statements[0].kind else {
        panic!("expected Skipped");
    };
    let got: Vec<_> = names.iter().map(|n| n.name.as_str()).collect();
    assert_eq!(got, ["path-var"]);

    let source = "OS-CREATE-DIR some-dir.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    let StatementKind::Skipped { names, .. } = &program.statements[0].kind else {
        panic!("expected Skipped");
    };
    assert!(names.is_empty());
}

#[test]
fn put_dynamic_function_item_is_an_expression() {
    let source = "PUT UNFORMATTED DYNAMIC-FUNCTION(\"f\") SKIP.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    let StatementKind::Put { items, .. } = &program.statements[0].kind else {
        panic!("expected Put, got {:?}", program.statements[0].kind);
    };
    assert_eq!(items.len(), 1, "{:?}", program.statements[0]);
}

#[test]
fn put_with_a_non_identifier_stream_name_still_parses() {
    let source = "PUT STREAM 2dstream UNFORMATTED apage.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let stmt = &program.statements[0];
    assert!(matches!(stmt.kind, StatementKind::Put { .. }));
    let tail = stmt.skipped_tail.as_ref().expect("names kept as a tail");
    assert!(tail.names.iter().any(|n| n.name == "apage"));
}

#[test]
fn seek_statement_parses_with_end_operand() {
    let source = "SEEK OUTPUT TO END.\nSEEK STREAM s TO 0.";
    let program = Parser::new(&tokenize(source), source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert_eq!(program.statements.len(), 2);
    assert!(matches!(
        program.statements[0].kind,
        StatementKind::Skipped { .. }
    ));
}

#[test]
fn a_statement_cut_short_at_the_end_of_the_file_does_not_panic() {
    for source in [
        "DO TRANSACTION ON ",
        "IF LOCKED",
        "IF LOCKED ",
        "CATCH e AS",
        "CATCH e AS ",
        "x = NEW",
        "FIND FIRST t NO-LOCK NO-ERROR. IF AVAILABLE",
        "DO ON ERROR UNDO, LEAVE ON",
    ] {
        let tokens = tokenize(source);
        let _ = Parser::new(&tokens, source).parse_program();
    }
}

/// Every prefix of each file parses without a panic: a buffer being typed is
/// cut at any byte.
#[test]
fn no_prefix_of_a_representative_file_panics() {
    let resources = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../resources");
    for name in [
        "bench_parser_control_flow.abl",
        "bench_parser_datasets.abl",
        "bench_parser_declarations.abl",
        "bench_parser_expressions.abl",
        "bench_parser_oo_abl.abl",
        "bench_parser_procs_funcs.abl",
        "bench_parser_skipped_forms.abl",
        "bench_parser_temp_tables.abl",
        "bench_lint_class.abl",
        "bench_lint_procedural.abl",
    ] {
        let text = std::fs::read_to_string(resources.join(name)).expect("fixture");
        for end in (0..=text.len()).filter(|&end| text.is_char_boundary(end)) {
            let source = &text[..end];
            let parsed = std::panic::catch_unwind(|| {
                let tokens = tokenize(source);
                let _ = Parser::new(&tokens, source).parse_program();
            });
            assert!(
                parsed.is_ok(),
                "{name} cut at byte {end} panics; the prefix ends with {:?}",
                &source[source.len().saturating_sub(40)..]
            );
        }
    }
}

#[test]
fn a_keyword_against_a_block_header_colon_still_ends_the_header() {
    for source in [
        "CASE x:WHEN 1 THEN MESSAGE \"one\". OTHERWISE MESSAGE \"other\". END CASE.",
        "CASE x:WHEN \"a\" OR WHEN \"b\" THEN MESSAGE \"ab\". END CASE.",
        "CASE x: WHEN 1 THEN MESSAGE \"one\". END CASE.",
        "CASE o:Value:WHEN 1 THEN MESSAGE \"one\". END CASE.",
        "CASE x:OTHERWISE MESSAGE \"other\". END CASE.",
        "CASE x:OTHERWISE DO: MESSAGE \"other\". END. END CASE.",
        "FOR EACH t WHERE t.a = x:DISPLAY t.a. END.",
        "FOR EACH t WHERE t.a = x:DELETE t. END.",
        "FOR EACH t BY t.a DESCENDING:DISPLAY t.a. END.",
        "DO WHILE x:IF x THEN LEAVE. END.",
        "DO WHILE x:LEAVE. END.",
        "REPEAT WHILE x < 3:ASSIGN x = x + 1. END.",
        "DO i = 1 TO n:IF i > 1 THEN LEAVE. END.",
        "IF x THEN DO:MESSAGE \"x\". END.",
    ] {
        assert_parses_clean(source);
    }
}

#[test]
fn a_keyword_member_is_still_read_before_what_can_follow_a_member() {
    for source in [
        "r = o:When.",
        "r = o:Otherwise(1).",
        "r = o:Display + 1.",
        "IF o:Display AND o:When THEN MESSAGE \"x\".",
        "DISPLAY o:Display FORMAT \"x(8)\" WITH FRAME f.",
        "r = MAXIMUM(o:Display, o:When).",
        "r = o:When[1].",
        "o:Display = 2.",
        "r = IF a THEN o:When ELSE 0.",
        "CASE o:When:WHEN 1 THEN MESSAGE \"one\". END CASE.",
    ] {
        assert_parses_clean(source);
    }
}

#[test]
fn a_do_block_takes_while_transaction_and_on_phrases_in_any_order() {
    for source in [
        "DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE TRANSACTION WHILE i < 3:\n  i = i + 1.\nEND.",
        "DO TRANSACTION WHILE i < 3 ON ERROR UNDO, LEAVE:\n  i = i + 1.\nEND.",
        "DO WHILE i < 3 TRANSACTION ON ERROR UNDO, LEAVE:\n  i = i + 1.\nEND.",
        "DO WHILE i < 3 ON ERROR UNDO, LEAVE TRANSACTION:\n  i = i + 1.\nEND.",
        "DO ON ERROR UNDO, RETRY TRANSACTION WHILE i < 3:\n  i = i + 1.\nEND.",
        "DO i = 1 TO 3 TRANSACTION ON ERROR UNDO, THROW:\n  MESSAGE i.\nEND.",
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::Do {
            transaction,
            while_condition,
            to,
            ..
        } = &stmts[0].kind
        else {
            panic!("{source}: expected DO, got {:?}", stmts[0].kind);
        };
        assert!(*transaction, "{source}");
        assert!(while_condition.is_some() || to.is_some(), "{source}");
    }
}

#[test]
fn override_and_abstract_on_a_property_come_in_either_order() {
    for source in [
        "DEFINE PUBLIC ABSTRACT OVERRIDE PROPERTY P AS CHARACTER NO-UNDO GET. SET.",
        "DEFINE PUBLIC OVERRIDE ABSTRACT PROPERTY P AS CHARACTER NO-UNDO GET. SET.",
        "DEFINE OVERRIDE PUBLIC ABSTRACT PROPERTY P AS CHARACTER NO-UNDO GET. SET.",
    ] {
        let stmts = assert_parses_clean(source);
        assert_eq!(stmts.len(), 1, "{source}");
        assert!(
            matches!(stmts[0].kind, StatementKind::Property { .. }),
            "{source}: {:?}",
            stmts[0].kind
        );
    }
}

#[test]
fn raw_transfer_and_underline_are_unmodelled_statements_and_raw_transfer_is_still_a_name() {
    for source in [
        "RAW-TRANSFER BUFFER Customer TO BUFFER tt NO-ERROR.",
        "RAW-TRANSFER Customer TO tt.",
        "UNDERLINE Customer.Name WITH FRAME f.",
    ] {
        let stmts = assert_parses_clean(source);
        assert!(
            matches!(stmts[0].kind, StatementKind::Skipped { .. }),
            "{source}: {:?}",
            stmts[0].kind
        );
    }
    assert_parses_clean("DEFINE VARIABLE raw-transfer AS INTEGER NO-UNDO.\nraw-transfer = 1.");
}

#[test]
fn new_with_a_parenthesised_buffer_is_a_function_call() {
    let stmts = assert_parses_clean("l = NEW(customer).");
    let StatementKind::Assignment { value, .. } = &stmts[0].kind else {
        panic!("expected assignment, got {:?}", stmts[0].kind);
    };
    let ExpressionKind::FunctionCall { name, arguments } = &value.kind else {
        panic!("expected a call, got {:?}", value.kind);
    };
    assert_eq!(name.name, "NEW");
    assert_eq!(arguments.len(), 1);
}

#[test]
fn new_with_a_class_name_is_still_an_instantiation() {
    let stmts = assert_parses_clean("o = NEW Foo(1).");
    let StatementKind::Assignment { value, .. } = &stmts[0].kind else {
        panic!("expected assignment, got {:?}", stmts[0].kind);
    };
    assert!(matches!(value.kind, ExpressionKind::New { .. }));
}

#[test]
fn byte_order_constants_are_expressions() {
    for source in [
        "SET-BYTE-ORDER(m) = BIG-ENDIAN.",
        "SET-BYTE-ORDER(m) = LITTLE-ENDIAN.",
        "SET-BYTE-ORDER(m) = HOST-BYTE-ORDER.",
        "i = HOST-BYTE-ORDER.",
    ] {
        assert_parses_clean(source);
    }
}

#[test]
fn overlay_is_a_target_and_a_value() {
    for source in [
        "OVERLAY(s, 1, 3) = \"abc\".",
        "OVERLAY(s, 1, 3, \"CHARACTER\") = \"abc\".",
        "s = OVERLAY(s, 1, 3).",
    ] {
        assert_parses_clean(source);
    }
}

#[test]
fn input_from_os_dir_takes_a_path_expression() {
    for source in [
        "INPUT FROM OS-DIR(\".\").",
        "INPUT STREAM s FROM OS-DIR(cDir + \"/x\") NO-ECHO.",
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::StreamIo {
            operation: StreamOperation::From(source_expr),
            ..
        } = &stmts[0].kind
        else {
            panic!("{source}: expected INPUT FROM, got {:?}", stmts[0].kind);
        };
        assert!(
            matches!(&source_expr.kind, ExpressionKind::FunctionCall { name, .. } if name.name.eq_ignore_ascii_case("os-dir")),
            "{source}: {:?}",
            source_expr.kind
        );
    }
}

#[test]
fn export_is_head_parsed_with_stream_delimiter_and_items() {
    let stmts = assert_parses_clean(
        "EXPORT STREAM s1 DELIMITER \",\" tt.a v-b + 1 tt EXCEPT tt.c NO-LOBS.",
    );
    let StatementKind::Export {
        stream_name,
        delimiter,
        items,
    } = &stmts[0].kind
    else {
        panic!("expected Export, got {:?}", stmts[0].kind);
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "s1");
    assert!(delimiter.is_some());
    assert_eq!(items.len(), 3);
    assert_eq!(tail_names(&stmts[0]), ["tt"]);
}

#[test]
fn import_is_head_parsed_with_targets_and_drops_a_caret() {
    let stmts = assert_parses_clean("IMPORT STREAM s1 ^ v-a ^ tt.b v-c[2] NO-ERROR.");
    let StatementKind::Import {
        stream_name,
        targets,
        ..
    } = &stmts[0].kind
    else {
        panic!("expected Import, got {:?}", stmts[0].kind);
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "s1");
    assert_eq!(targets.len(), 3);

    let stmts = assert_parses_clean("IMPORT UNFORMATTED v-line.");
    let StatementKind::Import { targets, .. } = &stmts[0].kind else {
        panic!("expected Import, got {:?}", stmts[0].kind);
    };
    assert_eq!(targets.len(), 1);
}

#[test]
fn export_and_import_with_a_stream_handle_keep_the_rest_as_a_tail() {
    for source in ["EXPORT STREAM-HANDLE h v-a.", "IMPORT STREAM-HANDLE h v-a."] {
        let stmts = assert_parses_clean(source);
        assert!(
            matches!(
                stmts[0].kind,
                StatementKind::Export { ref items, .. } | StatementKind::Import { targets: ref items, .. }
                    if items.is_empty()
            ),
            "{source}: {:?}",
            stmts[0].kind
        );
        assert_eq!(tail_names(&stmts[0]), ["h", "v-a"], "{source}");
    }
}

#[test]
fn import_stops_at_a_target_that_is_not_writable() {
    let stmts = assert_parses_clean("IMPORT v-a \"x\" v-b.");
    let StatementKind::Import { targets, .. } = &stmts[0].kind else {
        panic!("expected Import, got {:?}", stmts[0].kind);
    };
    assert_eq!(targets.len(), 1);
    assert_eq!(tail_names(&stmts[0]), ["v-b"]);
}

#[test]
fn pause_is_head_parsed_with_duration_and_message() {
    let stmts =
        assert_parses_clean("PAUSE v-secs * 60 BEFORE-HIDE MESSAGE \"Wait \" + v-who IN WINDOW h.");
    let StatementKind::Pause { duration, message } = &stmts[0].kind else {
        panic!("expected Pause, got {:?}", stmts[0].kind);
    };
    assert!(duration.is_some());
    assert!(message.is_some());
    assert!(tail_names(&stmts[0]).contains(&"h"));

    for source in [
        "PAUSE.",
        "PAUSE 0 BEFORE-HIDE.",
        "PAUSE NO-MESSAGE.",
        "PAUSE BEFORE-H.",
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::Pause { message, .. } = &stmts[0].kind else {
            panic!("{source}: expected Pause, got {:?}", stmts[0].kind);
        };
        assert!(message.is_none(), "{source}");
        assert!(stmts[0].skipped_tail.is_none(), "{source}");
    }
}

#[test]
fn down_and_up_are_head_parsed_with_stream_and_lines() {
    let stmts = assert_parses_clean("DOWN STREAM s1 v-n WITH FRAME f.");
    let StatementKind::Down { stream_name, lines } = &stmts[0].kind else {
        panic!("expected Down, got {:?}", stmts[0].kind);
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "s1");
    assert!(lines.is_some());
    assert!(tail_names(&stmts[0]).contains(&"f"));

    let stmts = assert_parses_clean("DOWN 1 STREAM s1.");
    let StatementKind::Down { stream_name, lines } = &stmts[0].kind else {
        panic!("expected Down, got {:?}", stmts[0].kind);
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "s1");
    assert!(lines.is_some());

    let stmts = assert_parses_clean("UP WITH FRAME f.");
    let StatementKind::Up { stream_name, lines } = &stmts[0].kind else {
        panic!("expected Up, got {:?}", stmts[0].kind);
    };
    assert!(stream_name.is_none() && lines.is_none());
}

#[test]
fn form_is_head_parsed_with_items_and_format_options() {
    let source = "FORM HEADER\n  \"Page:\" AT 64 PAGE-NUMBER(s1) FORMAT \">>9\":U\n  v-user AT 1 + v-col\n  /* note */ tt.name LABEL \"Name\" COLON 20 SKIP(1)\n  STRING(TIME, \"HH:MM\") NO-LABEL TO 30\n  WITH FRAME f-hdr PAGE-TOP v-width.";
    let stmts = assert_parses_clean(source);
    let StatementKind::Form { items, .. } = &stmts[0].kind else {
        panic!("expected Form, got {:?}", stmts[0].kind);
    };
    // "Page:", PAGE-NUMBER(s1), v-user, 1 + v-col, tt.name, STRING(...).
    assert_eq!(items.len(), 6, "{items:?}");
    assert!(tail_names(&stmts[0]).contains(&"v-width"));
    assert!(!tail_names(&stmts[0]).contains(&"v-user"));
}

#[test]
fn form_leaves_unmodelled_item_options_to_the_tail() {
    for (source, modelled, tail) in [
        // A name followed by a number is an option the lexer does not classify.
        ("FORM v-a BGCOLOR 3 v-b.", 1, "v-b"),
        ("FORM v-a AT ROW 2 COL 5 v-b.", 1, "v-b"),
        ("FORM v-a AT v-col v-b.", 1, "v-b"),
        ("FORM v-a VIEW-AS FILL-IN v-b WITH FRAME f.", 1, "v-b"),
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::Form { items, .. } = &stmts[0].kind else {
            panic!("{source}: expected Form, got {:?}", stmts[0].kind);
        };
        assert_eq!(items.len(), modelled, "{source}: {items:?}");
        assert!(tail_names(&stmts[0]).contains(&tail), "{source}");
    }
}

#[test]
fn a_widget_definition_declares_its_name() {
    for (source, name) in [
        ("DEFINE BUTTON b-ok LABEL \"OK\" AUTO-GO.", "b-ok"),
        (
            "DEF BROWSE br-cust QUERY q DISPLAY Customer.Name WITH 5 DOWN.",
            "br-cust",
        ),
        (
            "DEFINE RECTANGLE RECT-1 EDGE-PIXELS 2 SIZE 10 BY 2.",
            "RECT-1",
        ),
        ("DEFINE NEW SHARED MENU m-main MENUBAR.", "m-main"),
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::DefineWidget { name: got } = &stmts[0].kind else {
            panic!("{source}: expected DefineWidget, got {:?}", stmts[0].kind);
        };
        assert_eq!(got.name, name, "{source}");
    }
}

#[test]
fn update_and_set_are_head_parsed_with_targets() {
    let source = "UPDATE UNLESS-HIDDEN v-a AUTO-RETURN LABEL \"A\" HELP \"h\" ^ \"note\" AT 5 tt.b v-c[2] GO-ON(F1) WITH FRAME f EDITING: v-x = 1. END.";
    let stmts = assert_parses_clean(source);
    assert_eq!(stmts.len(), 1, "{stmts:?}");
    let StatementKind::Update { targets, .. } = &stmts[0].kind else {
        panic!("expected Update, got {:?}", stmts[0].kind);
    };
    assert_eq!(targets.len(), 3, "{targets:?}");
    let tail = tail_names(&stmts[0]);
    assert!(tail.contains(&"f") && tail.contains(&"v-x"), "{tail:?}");

    let stmts = assert_parses_clean("SET v-a = v-b + 1.");
    let StatementKind::Set { targets, .. } = &stmts[0].kind else {
        panic!("expected Set, got {:?}", stmts[0].kind);
    };
    assert!(targets.is_empty(), "an assignment form stays in the tail");
    assert!(tail_names(&stmts[0]).contains(&"v-b"));
}

#[test]
fn widget_list_statements_are_head_parsed() {
    for (source, count, tail) in [
        (
            "ENABLE UNLESS-HIDDEN b-ok v-name LABEL \"Name\" WITH FRAME f.",
            2,
            Some("f"),
        ),
        ("ENABLE ALL EXCEPT b-ok WITH FRAME f.", 0, Some("b-ok")),
        ("DISABLE b-ok tt.name WITH FRAME f.", 2, Some("f")),
        ("HIDE FRAME f NO-PAUSE.", 0, Some("f")),
        ("HIDE STREAM s1 ALL.", 0, None),
        // The expression parser takes `IN FRAME f` as part of the widget reference.
        ("HIDE v-name IN FRAME f.", 1, None),
        ("VIEW C-Win.", 1, None),
        ("VIEW FRAME f IN WINDOW C-Win.", 0, Some("C-Win")),
    ] {
        let stmts = assert_parses_clean(source);
        let items = match &stmts[0].kind {
            StatementKind::Enable { items }
            | StatementKind::Disable { items }
            | StatementKind::Hide { items, .. }
            | StatementKind::View { items, .. } => items,
            other => panic!("{source}: got {other:?}"),
        };
        assert_eq!(items.len(), count, "{source}: {items:?}");
        if let Some(name) = tail {
            assert!(
                tail_names(&stmts[0]).contains(&name),
                "{source}: {:?}",
                stmts[0]
            );
        }
    }
    let stmts = assert_parses_clean("DISABLE TRIGGERS FOR LOAD OF Customer.");
    assert!(matches!(stmts[0].kind, StatementKind::Skipped { .. }));
}

#[test]
fn apply_is_head_parsed_with_event_and_target() {
    for (source, has_target, tail) in [
        ("APPLY \"entry\" TO v-name IN FRAME f.", true, None),
        ("APPLY \"close\" TO THIS-PROCEDURE.", true, None),
        ("APPLY v-event TO h-proc.", true, None),
        ("APPLY (LASTKEY).", false, None),
        (
            "APPLY \"choose\" TO BROWSE br-cust.",
            false,
            Some("br-cust"),
        ),
        ("APPLY \"go\" TO FRAME f.", false, Some("f")),
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::Apply { event, target } = &stmts[0].kind else {
            panic!("{source}: expected Apply, got {:?}", stmts[0].kind);
        };
        assert!(event.is_some(), "{source}");
        assert_eq!(target.is_some(), has_target, "{source}");
        if let Some(name) = tail {
            assert!(tail_names(&stmts[0]).contains(&name), "{source}");
        }
    }
}

#[test]
fn wait_for_is_head_parsed_with_the_widgets() {
    for (source, count, tail) in [
        ("WAIT-FOR CLOSE OF THIS-PROCEDURE.", 1, None),
        (
            "WAIT-FOR \"close\":U OF THIS-PROCEDURE, CURRENT-WINDOW.",
            2,
            None,
        ),
        (
            "WAIT-FOR ENTER, TAB OF v-name IN FRAME f OR CHOOSE OF b-ok FOCUS v-name.",
            2,
            Some("v-name"),
        ),
        ("WAIT-FOR GO OF FRAME f.", 0, Some("f")),
        ("WAIT-FOR frm:ShowDialog().", 0, Some("frm")),
    ] {
        let stmts = assert_parses_clean(source);
        let StatementKind::WaitFor { widgets } = &stmts[0].kind else {
            panic!("{source}: expected WaitFor, got {:?}", stmts[0].kind);
        };
        assert_eq!(widgets.len(), count, "{source}: {widgets:?}");
        if let Some(name) = tail {
            assert!(tail_names(&stmts[0]).contains(&name), "{source}");
        }
    }
}

#[test]
fn define_buffer_for_a_database_qualified_table_keeps_the_whole_name() {
    for source in [
        "DEFINE BUFFER b FOR sports.Customer.",
        "DEFINE BUFFER b FOR sports.Customer .",
        "DEFINE BUFFER b FOR sports.Customer PRESELECT.",
    ] {
        let stmts = assert_parses_clean(source);
        assert_eq!(stmts.len(), 1, "{source}: {stmts:?}");
        let StatementKind::DefineBuffer {
            target: BufferTarget::Table(t),
            ..
        } = &stmts[0].kind
        else {
            panic!("{source}: expected DefineBuffer, got {:?}", stmts[0].kind);
        };
        assert_eq!(t.name, "sports.Customer", "{source}");
    }
}

#[test]
fn buffer_parameters_and_like_take_a_database_qualified_table() {
    let buffer_target = |stmt: &Statement| match &stmt.kind {
        StatementKind::DefineParameter {
            param_type: ParameterType::Buffer { target, .. },
            ..
        } => target.name.clone(),
        other => panic!("expected a buffer parameter, got {other:?}"),
    };
    for source in [
        "DEFINE PARAMETER BUFFER b FOR sports.Customer.",
        "DEFINE INPUT PARAMETER BUFFER b FOR sports.Customer.",
    ] {
        let stmts = assert_parses_clean(source);
        assert_eq!(stmts.len(), 1, "{source}: {stmts:?}");
        assert_eq!(buffer_target(&stmts[0]), "sports.Customer", "{source}");
    }

    let source = "FUNCTION f RETURNS LOGICAL (BUFFER b FOR sports.Customer):\nEND FUNCTION.";
    let stmts = assert_parses_clean(source);
    let StatementKind::Function { body, .. } = &stmts[0].kind else {
        panic!("expected Function, got {:?}", stmts[0].kind);
    };
    assert_eq!(buffer_target(&body[0]), "sports.Customer");

    let source = "DEFINE TEMP-TABLE tt NO-UNDO LIKE sports.Customer.";
    let stmts = assert_parses_clean(source);
    let StatementKind::DefineTempTable { like_table, .. } = &stmts[0].kind else {
        panic!("expected DefineTempTable, got {:?}", stmts[0].kind);
    };
    assert_eq!(
        like_table.as_ref().map(|t| t.name.as_str()),
        Some("sports.Customer")
    );
}

fn frame_field_names(fields: &[FrameField]) -> Vec<(&str, String)> {
    fields
        .iter()
        .map(|f| {
            let ty = match &f.type_source {
                TypeSource::Explicit(dt) => format!("{dt:?}"),
                TypeSource::Like { source } => format!("like {}", source.name),
            };
            (f.name.name.as_str(), ty)
        })
        .collect()
}

#[test]
fn form_declares_frame_fields_and_keeps_parsing_items() {
    let source = "FORM v-a v-new AS CHARACTER FORMAT \"x(8)\" v-b\n  v-qty LIKE sports.Item.Qty LABEL \"Qty\" v-c\n  WITH FRAME f.";
    let stmts = assert_parses_clean(source);
    let StatementKind::Form { items, fields } = &stmts[0].kind else {
        panic!("expected Form, got {:?}", stmts[0].kind);
    };
    assert_eq!(items.len(), 3, "v-a, v-b, v-c: {items:?}");
    assert_eq!(
        frame_field_names(fields),
        vec![
            ("v-new", "Character".to_string()),
            ("v-qty", "like sports.Item.Qty".to_string()),
        ]
    );
    let tail = tail_names(&stmts[0]);
    assert!(
        !tail.contains(&"v-new") && !tail.contains(&"v-c"),
        "{tail:?}"
    );
}

#[test]
fn update_and_set_declare_frame_fields_and_keep_parsing_items() {
    for source in [
        "UPDATE v-a v-new AS CHARACTER FORMAT \"x(8)\" v-b\n  v-qty LIKE item.qty LABEL \"Qty\" v-c\n  WITH FRAME f.",
        "SET v-a v-new AS CHARACTER FORMAT \"x(8)\" v-b\n  v-qty LIKE item.qty LABEL \"Qty\" v-c\n  WITH FRAME f.",
    ] {
        let stmts = assert_parses_clean(source);
        let (StatementKind::Update {
            targets, fields, ..
        }
        | StatementKind::Set {
            targets, fields, ..
        }) = &stmts[0].kind
        else {
            panic!("{source}: got {:?}", stmts[0].kind);
        };
        assert_eq!(targets.len(), 3, "{source}: v-a, v-b, v-c: {targets:?}");
        assert_eq!(
            frame_field_names(fields),
            vec![
                ("v-new", "Character".to_string()),
                ("v-qty", "like item.qty".to_string()),
            ],
            "{source}"
        );
        let tail = tail_names(&stmts[0]);
        assert!(
            !tail.contains(&"v-new") && !tail.contains(&"v-c"),
            "{source}: {tail:?}"
        );
    }
}

#[test]
fn enable_does_not_declare_frame_fields() {
    let source = "ENABLE v-a v-new AS CHARACTER v-b.";
    let stmts = assert_parses_clean(source);
    assert!(
        tail_names(&stmts[0]).contains(&"v-new"),
        "{source}: {:?}",
        stmts[0]
    );
}

#[test]
fn define_frame_declares_its_frame_fields() {
    let source = "DEFINE FRAME f-main\n  \"Order:\" VIEW-AS TEXT AT ROW 1 COL 1\n  v-order AS CHARACTER FORMAT \"x(20)\" VIEW-AS FILL-IN SIZE 20 BY 1 AT ROW 1 COL 8\n  v-count AS INTEGER EXTENT 3\n  v-desc LIKE Item.Desc VIEW-AS EDITOR SIZE 30 BY 2\n  Customer.Name AT ROW 3 COL 1 v-known\n  WITH NO-LABELS SIZE 40 BY 6.";
    let stmts = assert_parses_clean(source);
    assert_eq!(stmts.len(), 1, "{stmts:?}");
    let StatementKind::DefineFrame { name, fields, .. } = &stmts[0].kind else {
        panic!("expected DefineFrame, got {:?}", stmts[0].kind);
    };
    assert_eq!(name.name, "f-main");
    assert_eq!(
        frame_field_names(fields),
        vec![
            ("v-order", "Character".to_string()),
            ("v-count", "Integer".to_string()),
            ("v-desc", "like Item.Desc".to_string()),
        ]
    );
}

#[test]
fn stop_statement_and_variable() {
    let source = "STOP. DEFINE VARIABLE stop AS INTEGER NO-UNDO. stop = 1. MESSAGE stop.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert!(matches!(program.statements[0].kind, StatementKind::Empty));
    assert_eq!(program.statements.len(), 4);
    assert!(matches!(
        program.statements[2].kind,
        StatementKind::Assignment { .. }
    ));
}

#[test]
fn comments_between_statement_keywords() {
    for source in [
        "OUTPUT /* c */ CLOSE.",
        "INPUT /* c */ CLOSE.",
        "INPUT-OUTPUT /* c */ CLOSE.",
        "DEFINE /* c */ VARIABLE n AS INTEGER NO-UNDO.",
        "DEFINE /* c */ NEW /* c */ SHARED VARIABLE n AS INTEGER NO-UNDO.",
        "DEFINE /* c */ PARAMETER /* c */ BUFFER b FOR Customer.",
        "lbl /* c */ : /* c */ DO: END.",
        "VAR /* c */ INTEGER /* c */ n.",
        "VAR /* c */ CLASS /* c */ Example /* c */ n.",
    ] {
        let tokens = tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
        assert!(
            !matches!(
                program.statements[0].kind,
                StatementKind::ExpressionStatement(_)
            ),
            "{source}"
        );
    }
}

#[test]
fn comment_lookahead_preserves_run_target_boundaries() {
    for (source, target) in [
        ("RUN task/* c */.p.", "task"),
        ("RUN task./* c */p.", "task"),
        ("RUN ./task.p /* c */ NO-ERROR.", "./task.p"),
        ("RUN task/* c */(1).", "task"),
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let stmt = parser.parse_statement().expect(source);
        let StatementKind::Run {
            target: RunTarget::Literal { name, .. },
            ..
        } = stmt.kind
        else {
            panic!("{source}: {:?}", stmt.kind);
        };
        assert_eq!(name, target, "{source}");
    }
}

#[test]
fn comment_lookahead_keeps_method_calls_and_adjacency() {
    let source = "CLASS C: METHOD PUBLIC VOID Display(x AS INTEGER): END METHOD. METHOD PUBLIC VOID Run1(): Display(1) /* c */ . Display(1) /* c */ NO-ERROR /* c */ . DISPLAY/* c */(1) WITH FRAME f. END METHOD. END CLASS.";
    let stmts = assert_parses_clean(source);
    let body = statements_in_first_method(&stmts);
    assert_eq!(body.len(), 3);
    assert!(matches!(
        body[0].kind,
        StatementKind::ExpressionStatement(_)
    ));
    assert!(matches!(
        body[1].kind,
        StatementKind::ExpressionStatement(_)
    ));
    assert!(matches!(body[2].kind, StatementKind::Display { .. }));
}

#[test]
fn lookahead_counts_non_comment_tokens_and_preserves_positions() {
    let source = "OUTPUT/* one *//* two */CLOSE/* three */.";
    let tokens = tokenize(source);
    let parser = Parser::new(&tokens, source);
    assert_eq!(parser.peek_at(0).kind, Kind::Output);
    assert_eq!(parser.peek_at(1).kind, Kind::Close);
    assert_eq!(parser.peek_at(2).kind, Kind::Period);
    assert_eq!(parser.peek_at(3).kind, Kind::Eof);
    assert_eq!(parser.peek_at(30).kind, Kind::Eof);
    assert_ne!(parser.peek_at(0).end, parser.peek_at(1).start);
    assert_eq!(parser.peek_at(1).start, source.find("CLOSE").unwrap());
}

#[test]
fn comment_lookahead_preserves_statement_newlines() {
    for source in [
        "CREATE TEMP-TABLE h./* c */\nMESSAGE 1.",
        "USING Example.Type./* c */\nMESSAGE 1.",
        "@Example./* c */\nMESSAGE 1.",
    ] {
        let stmts = assert_parses_clean(source);
        assert_eq!(stmts.len(), 2, "{source}: {stmts:?}");
        assert!(matches!(stmts[1].kind, StatementKind::Message { .. }));
    }
}

#[test]
fn update_and_set_keep_the_expressions_of_their_format_options() {
    for source in [
        "UPDATE v-a VALIDATE(CAN-FIND(Customer WHERE Customer.Num = v-a), \"bad\") WITH FRAME f.",
        "UPDATE v-a WHEN CAN-FIND(Customer WHERE Customer.Num = v-a) WITH FRAME f.",
        "SET v-a VALIDATE(CAN-FIND(Customer WHERE Customer.Num = v-a), \"bad\") WITH FRAME f.",
        "SET v-a WHEN CAN-FIND(Customer WHERE Customer.Num = v-a) WITH FRAME f.",
    ] {
        let stmts = assert_parses_clean(source);
        let (StatementKind::Update {
            targets, options, ..
        }
        | StatementKind::Set {
            targets, options, ..
        }) = &stmts[0].kind
        else {
            panic!("{source}: got {:?}", stmts[0].kind);
        };
        assert_eq!(targets.len(), 1, "{source}: {targets:?}");
        assert_eq!(options.len(), 1, "{source}: {options:?}");
        assert!(tail_names(&stmts[0]).contains(&"f"), "{source}");
    }

    let stmts =
        assert_parses_clean("UPDATE v-a VALIDATE(v-a > 0, \"bad\") v-b WHEN v-a = 1 WITH FRAME f.");
    let StatementKind::Update {
        targets,
        options,
        fields,
    } = &stmts[0].kind
    else {
        panic!("expected Update, got {:?}", stmts[0].kind);
    };
    assert_eq!((targets.len(), options.len()), (2, 2), "{stmts:?}");
    assert!(fields.is_empty(), "no AS/LIKE item here");
}

#[test]
fn form_and_enable_keep_the_expressions_of_their_format_options() {
    for source in [
        "FORM v-a VALIDATE(CAN-FIND(Customer WHERE Customer.Num = v-a), \"bad\") WITH FRAME f.",
        "FORM v-a WHEN CAN-FIND(Customer WHERE Customer.Num = v-a) WITH FRAME f.",
        "ENABLE v-a VALIDATE(CAN-FIND(Customer WHERE Customer.Num = v-a), \"bad\") WITH FRAME f.",
        "ENABLE v-a WHEN CAN-FIND(Customer WHERE Customer.Num = v-a) WITH FRAME f.",
    ] {
        let stmts = assert_parses_clean(source);
        let (StatementKind::Form { items, .. } | StatementKind::Enable { items }) = &stmts[0].kind
        else {
            panic!("{source}: got {:?}", stmts[0].kind);
        };
        assert_eq!(items.len(), 2, "{source}: {items:?}");
        assert!(tail_names(&stmts[0]).contains(&"f"), "{source}");
    }
}

// =============================================================================
// Handle-lifetime statements: dynamic-object CREATE, pools, DELETE, RELEASE, APPLY
// =============================================================================

/// The statement after the one under test, which a swallowing parser would lose.
const NEXT_STMT: &str = "\nx = 1.";

#[track_caller]
fn assert_next_stmt_survives(stmts: &[Statement], source: &str) {
    assert_eq!(stmts.len(), 2, "{source}: following statement was lost");
    assert!(
        matches!(stmts[1].kind, StatementKind::Assignment { .. }),
        "{source}: got {:?}",
        stmts[1].kind
    );
}

#[track_caller]
fn handle_create(stmt: &Statement) -> (&CreateTargetKind, &Expression, bool) {
    match &stmt.kind {
        StatementKind::Create {
            target: CreateTarget::Handle { kind, handle, .. },
            no_error,
        } => (kind, handle, *no_error),
        other => panic!("expected a handle create, got {other:?}"),
    }
}

#[test]
fn create_handle_types_keep_their_kind_and_handle() {
    for (keyword, expected) in [
        ("X-DOCUMENT", CreateTargetKind::XDocument),
        ("X-NODEREF", CreateTargetKind::XNodeRef),
        ("SAX-READER", CreateTargetKind::SaxReader),
        ("SAX-WRITER", CreateTargetKind::SaxWriter),
        ("SAX-ATTRIBUTES", CreateTargetKind::SaxAttributes),
        ("SOCKET", CreateTargetKind::Socket),
        ("SERVER-SOCKET", CreateTargetKind::ServerSocket),
        ("SERVER", CreateTargetKind::Server),
        ("SOAP-HEADER", CreateTargetKind::SoapHeader),
        ("SOAP-HEADER-ENTRYREF", CreateTargetKind::SoapHeaderEntryRef),
        ("CLIENT-PRINCIPAL", CreateTargetKind::ClientPrincipal),
        ("DATASET", CreateTargetKind::Dataset),
        ("DATA-SOURCE", CreateTargetKind::DataSource),
        ("TEMP-TABLE", CreateTargetKind::TempTable),
        ("BUFFER", CreateTargetKind::Buffer),
        ("QUERY", CreateTargetKind::Query),
        ("CALL", CreateTargetKind::Call),
    ] {
        for tail in [
            "",
            " NO-ERROR",
            " IN WIDGET-POOL \"p\"",
            " IN WIDGET-POOL \"p\" NO-ERROR",
        ] {
            let source = format!("CREATE {keyword} h{tail}.{NEXT_STMT}");
            let stmts = parse_clean(&source);
            assert_next_stmt_survives(&stmts, &source);
            let (kind, handle, no_error) = handle_create(&stmts[0]);
            assert_eq!(*kind, expected, "{source}");
            assert_eq!(expr_name(handle), "h", "{source}");
            assert_eq!(no_error, tail.ends_with("NO-ERROR"), "{source}");
            match &stmts[0].kind {
                StatementKind::Create {
                    target: CreateTarget::Handle { widget_pool, .. },
                    ..
                } => assert_eq!(
                    widget_pool.is_some(),
                    tail.contains("WIDGET-POOL"),
                    "{source}"
                ),
                _ => unreachable!(),
            }
        }
    }
}

#[test]
fn create_widget_types_are_widget_creates() {
    for keyword in [
        "BROWSE",
        "BUTTON",
        "COMBO-BOX",
        "CONTROL-FRAME",
        "DIALOG-BOX",
        "EDITOR",
        "FILL-IN",
        "FRAME",
        "IMAGE",
        "MENU",
        "MENU-ITEM",
        "RADIO-SET",
        "RECTANGLE",
        "SELECTION-LIST",
        "SLIDER",
        "SUB-MENU",
        "TEXT",
        "TOGGLE-BOX",
        "WINDOW",
    ] {
        for tail in ["", " NO-ERROR", " IN WIDGET-POOL \"p\" NO-ERROR"] {
            let source = format!("CREATE {keyword} h{tail}.{NEXT_STMT}");
            let stmts = parse_clean(&source);
            assert_next_stmt_survives(&stmts, &source);
            let (kind, handle, _) = handle_create(&stmts[0]);
            assert_eq!(*kind, CreateTargetKind::Widget, "{source}");
            assert_eq!(expr_name(handle), "h", "{source}");
        }
    }
}

#[test]
fn create_widget_keeps_its_assign_expressions() {
    let source =
        format!("CREATE BUTTON h ASSIGN LABEL = \"x\" ROW = iRow + 1 SENSITIVE = TRUE.{NEXT_STMT}");
    let stmts = parse_clean(&source);
    assert_next_stmt_survives(&stmts, &source);
    match &stmts[0].kind {
        StatementKind::Create {
            target: CreateTarget::Handle { assignments, .. },
            ..
        } => {
            let names: Vec<_> = assignments
                .iter()
                .map(|a| a.attribute.name.as_str())
                .collect();
            assert_eq!(names, ["LABEL", "ROW", "SENSITIVE"]);
            assert!(matches!(assignments[1].value.kind, ExpressionKind::Add(..)));
        }
        other => panic!("got {other:?}"),
    }
}

#[test]
fn create_widget_keeps_its_triggers_phrase_as_statements() {
    let source = format!(
        "CREATE BROWSE h IN WIDGET-POOL \"p\"\n  ASSIGN WIDTH = 10\n  TRIGGERS:\n    ON CHOOSE DO:\n      y = 2.\n    END.\n    ON ENTRY PERSISTENT RUN lib.\n  END TRIGGERS\n  NO-ERROR.{NEXT_STMT}"
    );
    let stmts = parse_clean(&source);
    assert_next_stmt_survives(&stmts, &source);
    match &stmts[0].kind {
        StatementKind::Create {
            target:
                CreateTarget::Handle {
                    widget_pool,
                    assignments,
                    triggers,
                    ..
                },
            no_error,
        } => {
            assert!(widget_pool.is_some());
            assert!(no_error);
            assert_eq!(assignments.len(), 1);
            assert_eq!(triggers.len(), 2);
            assert_eq!(triggers[0].events[0].name, "CHOOSE");
            match &triggers[0].action {
                OnAction::Block(body) => match &body.kind {
                    StatementKind::Do { body, .. } => assert_eq!(body.len(), 1),
                    other => panic!("got {other:?}"),
                },
                other => panic!("got {other:?}"),
            }
            assert!(matches!(triggers[1].action, OnAction::PersistentRun { .. }));
        }
        other => panic!("got {other:?}"),
    }
}

#[test]
fn create_handle_keeps_subscripts_and_member_access() {
    for (handle_src, expected) in [
        ("hArr[1]", "ArrayAccess"),
        ("tt.hq", "FieldAccess"),
        ("THIS-OBJECT:hq", "MemberAccess"),
    ] {
        let source = format!("CREATE QUERY {handle_src}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        let (kind, handle, _) = handle_create(&stmts[0]);
        assert_eq!(*kind, CreateTargetKind::Query);
        let debug = format!("{:?}", handle.kind);
        assert!(debug.starts_with(expected), "{source}: {debug}");
    }
}

#[test]
fn create_value_is_a_dynamic_create_with_its_class_expression() {
    for tail in ["", " NO-ERROR"] {
        let source = format!("CREATE VALUE(cClass) h{tail}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        let (kind, handle, no_error) = handle_create(&stmts[0]);
        assert_eq!(*kind, CreateTargetKind::Dynamic);
        assert_eq!(expr_name(handle), "h");
        assert_eq!(no_error, !tail.is_empty());
        match &stmts[0].kind {
            StatementKind::Create {
                target:
                    CreateTarget::Handle {
                        class: Some(class), ..
                    },
                ..
            } => assert_eq!(expr_name(class), "cClass"),
            other => panic!("got {other:?}"),
        }
    }
}

#[test]
fn create_com_class_literal_is_an_automation_create() {
    for (tail, connect, file) in [
        ("", false, false),
        (" NO-ERROR", false, false),
        (" CONNECT", true, false),
        (" CONNECT TO cFile NO-ERROR", true, true),
        (" IN WIDGET-POOL \"p\"", false, false),
    ] {
        let source = format!("CREATE \"Excel.Application\" chExcel{tail}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        let (kind, handle, _) = handle_create(&stmts[0]);
        assert_eq!(*kind, CreateTargetKind::Automation, "{source}");
        assert_eq!(expr_name(handle), "chExcel", "{source}");
        match &stmts[0].kind {
            StatementKind::Create {
                target:
                    CreateTarget::Handle {
                        class: Some(class),
                        connect: conn,
                        ..
                    },
                ..
            } => {
                assert!(matches!(
                    class.kind,
                    ExpressionKind::Literal(Literal::String(_))
                ));
                assert_eq!(conn.is_some(), connect, "{source}");
                assert_eq!(
                    conn.as_ref().is_some_and(|c| c.file.is_some()),
                    file,
                    "{source}"
                );
            }
            other => panic!("got {other:?}"),
        }
    }
}

#[test]
fn create_on_a_table_named_like_a_handle_type_stays_a_record_create() {
    for source in [
        "CREATE image.",
        "CREATE menu NO-ERROR.",
        "CREATE server.",
        "CREATE button.",
    ] {
        let stmts = parse_clean(&format!("{source}{NEXT_STMT}"));
        assert_next_stmt_survives(&stmts, source);
        assert!(
            matches!(
                stmts[0].kind,
                StatementKind::Create {
                    target: CreateTarget::Name(_),
                    ..
                }
            ),
            "{source}: {:?}",
            stmts[0].kind
        );
    }
}

#[test]
fn create_widget_pool_forms() {
    for (tail, has_name, persistent, no_error) in [
        ("", false, false, false),
        (" PERSISTENT", false, true, false),
        (" NO-ERROR", false, false, true),
        (" \"p\"", true, false, false),
        (" \"p\" PERSISTENT", true, true, false),
        (" cPool PERSISTENT NO-ERROR", true, true, true),
    ] {
        let source = format!("CREATE WIDGET-POOL{tail}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        match &stmts[0].kind {
            StatementKind::CreateWidgetPool {
                name,
                persistent: p,
                no_error: n,
            } => {
                assert_eq!(name.is_some(), has_name, "{source}");
                assert_eq!(*p, persistent, "{source}");
                assert_eq!(*n, no_error, "{source}");
            }
            other => panic!("{source}: got {other:?}"),
        }
    }
}

#[test]
fn delete_widget_pool_forms() {
    for (tail, has_name, no_error) in [
        ("", false, false),
        (" NO-ERROR", false, true),
        (" \"p\"", true, false),
        (" cPool NO-ERROR", true, true),
    ] {
        let source = format!("DELETE WIDGET-POOL{tail}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        match &stmts[0].kind {
            StatementKind::DeleteWidgetPool { name, no_error: n } => {
                assert_eq!(name.is_some(), has_name, "{source}");
                assert_eq!(*n, no_error, "{source}");
            }
            other => panic!("{source}: got {other:?}"),
        }
    }
}

#[test]
fn delete_handle_forms_are_not_record_deletes() {
    for (source, expected, extra) in [
        ("DELETE OBJECT h.", DeleteObjectKind::Object, 0),
        ("DELETE OBJECT h NO-ERROR.", DeleteObjectKind::Object, 0),
        ("DELETE PROCEDURE h.", DeleteObjectKind::Procedure, 0),
        (
            "DELETE PROCEDURE h NO-ERROR.",
            DeleteObjectKind::Procedure,
            0,
        ),
        ("DELETE WIDGET h.", DeleteObjectKind::Widget, 0),
        ("DELETE WIDGET h1 h2.", DeleteObjectKind::Widget, 1),
        (
            "DELETE WIDGET h1 h2 h3 NO-ERROR.",
            DeleteObjectKind::Widget,
            2,
        ),
        ("DELETE SERVER h.", DeleteObjectKind::Server, 0),
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
        match &stmts[0].kind {
            StatementKind::DeleteObject {
                kind,
                target,
                extra_targets,
                no_error,
            } => {
                assert_eq!(*kind, expected, "{source}");
                assert!(expr_name(target).starts_with('h'), "{source}");
                assert_eq!(extra_targets.len(), extra, "{source}");
                assert_eq!(*no_error, source.contains("NO-ERROR"), "{source}");
            }
            other => panic!("{source}: got {other:?}"),
        }
    }
}

#[test]
fn delete_of_a_table_named_like_a_handle_keyword_stays_a_record_delete() {
    for source in [
        "DELETE widget.",
        "DELETE procedure.",
        "DELETE object NO-ERROR.",
    ] {
        let stmts = parse_clean(&format!("{source}{NEXT_STMT}"));
        assert_next_stmt_survives(&stmts, source);
        assert!(
            matches!(stmts[0].kind, StatementKind::Delete { .. }),
            "{source}"
        );
    }
}

#[test]
fn release_object_is_a_com_release_and_release_buffer_is_not() {
    for (source, no_error) in [
        ("RELEASE OBJECT chExcel.", false),
        ("RELEASE OBJECT chExcel NO-ERROR.", true),
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
        match &stmts[0].kind {
            StatementKind::ReleaseObject {
                target,
                no_error: n,
            } => {
                assert_eq!(expr_name(target), "chExcel");
                assert_eq!(*n, no_error);
            }
            other => panic!("{source}: got {other:?}"),
        }
    }
    let stmts = parse_clean("RELEASE Customer.");
    assert!(matches!(stmts[0].kind, StatementKind::Release { .. }));
}

#[test]
fn apply_keeps_event_and_target() {
    for (source, has_target) in [
        ("APPLY \"CLOSE\" TO hProc.", true),
        ("APPLY \"CHOOSE\" TO btn-ok IN FRAME f-main.", true),
        ("APPLY \"ENTRY\" TO SELF.", true),
        ("APPLY cEvent.", false),
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
        match &stmts[0].kind {
            StatementKind::Apply { target, .. } => {
                assert_eq!(target.is_some(), has_target, "{source}")
            }
            other => panic!("{source}: got {other:?}"),
        }
    }
}

#[test]
fn unparsable_create_tail_is_skipped_without_swallowing_the_next_statement() {
    for source in [
        "CREATE BUTTON h ASSIGN LABEL ! = \"x\".",
        "CREATE X-DOCUMENT h BOGUS-CLAUSE.",
        "CREATE WIDGET-POOL \"p\" BOGUS.",
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
        assert!(
            matches!(stmts[0].kind, StatementKind::Skipped { .. }),
            "{source}"
        );
    }
}

#[test]
fn create_buffer_keeps_its_for_table_operand() {
    for tail in [
        "FOR TABLE \"Customer\"",
        "FOR TABLE Customer",
        "FOR TABLE(cName)",
        "FOR TABLE hTt:DEFAULT-BUFFER-HANDLE IN WIDGET-POOL \"p\" NO-ERROR",
    ] {
        let source = format!("CREATE BUFFER h {tail}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        match &stmts[0].kind {
            StatementKind::Create {
                target:
                    CreateTarget::Handle {
                        kind: CreateTargetKind::Buffer,
                        for_table,
                        ..
                    },
                ..
            } => assert!(for_table.is_some(), "{source}"),
            other => panic!("{source}: got {other:?}"),
        }
    }
}

#[test]
fn object_and_widget_stay_usable_as_names() {
    for source in [
        "DEFINE VARIABLE object AS CHARACTER NO-UNDO.",
        "DEFINE VARIABLE widget AS CHARACTER NO-UNDO.",
        "DEFINE TEMP-TABLE tt NO-UNDO FIELD widget AS CHARACTER FIELD object AS INTEGER.",
        "object = widget.",
        "MESSAGE object widget.",
        "x = tt.widget + h:object.",
        "DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.",
        "PROCEDURE object:\n  RETURN.\nEND PROCEDURE.",
        "RUN widget.",
        "DELETE object.",
        "DELETE OBJECT object.",
        "DELETE WIDGET widget.",
        "DELETE OBJECT h:object.",
        "RELEASE OBJECT object.",
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
    }
}

#[test]
fn choose_is_a_skipped_statement_and_still_a_name() {
    let stmts = parse_clean("CHOOSE FIELD c AUTO-RETURN WITH FRAME f.\nCHOOSE ROW b.\n");
    assert_eq!(stmts.len(), 2);
    for s in &stmts {
        assert!(
            matches!(s.kind, StatementKind::Skipped { .. }),
            "{:?}",
            s.kind
        );
    }
    for source in [
        "DEFINE VARIABLE choose AS INTEGER NO-UNDO.",
        "choose = 1.",
        "ON CHOOSE OF b DO:\nEND.",
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
    }
}

#[test]
fn aggregate_words_and_file_stay_usable_as_names() {
    for source in [
        "DEFINE VARIABLE total AS DECIMAL NO-UNDO.",
        "DEFINE VARIABLE file AS CHARACTER NO-UNDO.",
        "DEFINE TEMP-TABLE tt NO-UNDO FIELD count AS INTEGER FIELD average AS DECIMAL.",
        "total = count + average.",
        "x = tt.total + tt.sub-total.",
        "x = MAXIMUM(1, 2) + MINIMUM(3, 4).",
        "MESSAGE file maximum minimum.",
        "ASSIGN sub-count = 1 sub-average = 2 sub-maximum = 3 sub-minimum = 4.",
        "DISPLAY qty (TOTAL) price (SUB-AVERAGE).",
        "COPY-LOB FROM x TO FILE \"y.txt\".",
        "PROCEDURE total:\n  RETURN.\nEND PROCEDURE.",
        "RUN count.",
    ] {
        let full = format!("{source}{NEXT_STMT}");
        let stmts = parse_clean(&full);
        assert_next_stmt_survives(&stmts, &full);
    }
}

#[test]
fn delete_and_release_object_forms_keep_their_kind() {
    let stmts = parse_clean("DELETE OBJECT object.\nDELETE WIDGET widget.\nRELEASE OBJECT object.");
    assert!(matches!(
        stmts[0].kind,
        StatementKind::DeleteObject {
            kind: DeleteObjectKind::Object,
            ..
        }
    ));
    assert!(matches!(
        stmts[1].kind,
        StatementKind::DeleteObject {
            kind: DeleteObjectKind::Widget,
            ..
        }
    ));
    assert!(matches!(stmts[2].kind, StatementKind::ReleaseObject { .. }));
}

#[test]
fn create_buffer_accepts_buffer_name() {
    for tail in [
        "FOR TABLE \"x\" BUFFER-NAME \"y\"",
        "FOR TABLE cTable BUFFER-NAME cName NO-ERROR",
        "FOR TABLE hTt:DEFAULT-BUFFER-HANDLE BUFFER-NAME cName IN WIDGET-POOL \"p\"",
    ] {
        let source = format!("CREATE BUFFER h {tail}.{NEXT_STMT}");
        let stmts = parse_clean(&source);
        assert_next_stmt_survives(&stmts, &source);
        match &stmts[0].kind {
            StatementKind::Create {
                target:
                    CreateTarget::Handle {
                        kind: CreateTargetKind::Buffer,
                        for_table,
                        assignments,
                        ..
                    },
                ..
            } => {
                assert!(for_table.is_some(), "{source}");
                assert_eq!(assignments.len(), 1, "{source}");
                assert!(
                    assignments[0]
                        .attribute
                        .name
                        .eq_ignore_ascii_case("BUFFER-NAME"),
                    "{source}"
                );
            }
            other => panic!("{source}: got {other:?}"),
        }
    }
}

#[test]
fn buffer_name_stays_usable_as_a_member_name() {
    let source = format!("cName = hBuf:BUFFER-NAME.{NEXT_STMT}");
    let stmts = parse_clean(&source);
    assert_next_stmt_survives(&stmts, &source);
}

#[test]
fn on_persistent_run_takes_in_handle_and_path_names() {
    for (tail, name, has_in, args) in [
        ("PERSISTENT RUN p IN THIS-PROCEDURE.", "p", true, 0),
        ("PERSISTENT RUN p IN hLib (INPUT x).", "p", true, 1),
        ("PERSISTENT RUN dir/file.p.", "dir/file.p", false, 0),
        (
            "PERSISTENT RUN dir/file.p IN hLib (x, y).",
            "dir/file.p",
            true,
            2,
        ),
    ] {
        let on = format!("ON CHOOSE OF btn {tail}{NEXT_STMT}");
        let stmts = parse_clean(&on);
        assert_next_stmt_survives(&stmts, &on);
        match &stmts[0].kind {
            StatementKind::On {
                kind:
                    OnKind::UiEvent {
                        action:
                            OnAction::PersistentRun {
                                procedure,
                                in_handle,
                                arguments,
                            },
                        ..
                    },
            } => {
                assert_eq!(procedure.name, name, "{on}");
                assert_eq!(in_handle.is_some(), has_in, "{on}");
                assert_eq!(arguments.len(), args, "{on}");
            }
            other => panic!("{on}: got {other:?}"),
        }

        let trig = format!(
            "CREATE BUTTON h\n  TRIGGERS:\n    ON CHOOSE {tail}\n  END TRIGGERS.{NEXT_STMT}"
        );
        let stmts = parse_clean(&trig);
        assert_next_stmt_survives(&stmts, &trig);
        match &stmts[0].kind {
            StatementKind::Create {
                target: CreateTarget::Handle { triggers, .. },
                ..
            } => assert_eq!(triggers.len(), 1, "{trig}"),
            other => panic!("{trig}: got {other:?}"),
        }
    }
}

#[test]
fn unreserved_keywords_are_valid_class_type_names() {
    let class = |body: &str| format!("CLASS c:\n{body}\nEND CLASS.{NEXT_STMT}");
    for source in [
        "DEFINE VARIABLE o AS Object NO-UNDO.".to_string(),
        "DEFINE VARIABLE o AS OBJECT EXTENT 3 NO-UNDO.".to_string(),
        "DEFINE VARIABLE s AS Socket NO-UNDO.".to_string(),
        "DEFINE VARIABLE s AS Server NO-UNDO.".to_string(),
        "DEFINE VARIABLE i AS Image NO-UNDO.".to_string(),
        "DEFINE VARIABLE w AS Widget NO-UNDO.".to_string(),
        "DEFINE VARIABLE o AS CLASS Object NO-UNDO.".to_string(),
        "DEFINE VARIABLE o AS Menu.Item NO-UNDO.".to_string(),
        class("DEFINE PRIVATE PROPERTY body AS Object GET. SET."),
        class("METHOD PUBLIC Object Get():\n  RETURN ?.\nEND METHOD."),
        class("METHOD PUBLIC VOID Put(INPUT pData AS OBJECT):\nEND METHOD."),
        class("METHOD PUBLIC VOID Put(INPUT pData AS Socket):\nEND METHOD."),
    ] {
        let full = if source.starts_with("CLASS") {
            source
        } else {
            format!("{source}{NEXT_STMT}")
        };
        let stmts = parse_clean(&full);
        assert!(stmts.len() >= 2, "{full}");
    }
}

#[test]
fn strong_scope_headers_keep_their_bodies() {
    for (source, transaction) in [
        ("DO FOR b TRANSACTION:\n  FIND FIRST b.\nEND.", true),
        (
            "DO TRANSACTION FOR b, sports.c:\n  FIND FIRST b.\nEND.",
            true,
        ),
        ("DO FOR b, c:\n  FIND FIRST b.\nEND.", false),
        (
            "DO FOR b ON ERROR UNDO, LEAVE TRANSACTION:\n  FIND FIRST b.\nEND.",
            true,
        ),
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let program = parser.parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
        match &program.statements[0].kind {
            StatementKind::Do {
                body,
                transaction: t,
                ..
            } => {
                assert_eq!(body.len(), 1, "{source}");
                assert_eq!(*t, transaction, "{source}");
            }
            other => panic!("{source}: expected DO, got {other:?}"),
        }
    }
    for source in [
        "REPEAT FOR b:\n  FIND NEXT b.\nEND.",
        "REPEAT FOR b, sports.c TRANSACTION:\n  FIND NEXT b.\nEND.",
        "REPEAT TRANSACTION FOR b ON ERROR UNDO, LEAVE:\n  FIND NEXT b.\nEND.",
    ] {
        let tokens = tokenize(source);
        let mut parser = Parser::new(&tokens, source);
        let program = parser.parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
        match &program.statements[0].kind {
            StatementKind::Repeat { body, .. } => assert_eq!(body.len(), 1, "{source}"),
            other => panic!("{source}: expected REPEAT, got {other:?}"),
        }
    }
}

#[test]
fn record_statements_take_database_qualified_names() {
    for (source, expected) in [
        ("CREATE sports.customer.", "sports.customer"),
        ("DELETE sports.customer NO-ERROR.", "sports.customer"),
        ("RELEASE sports.customer.", "sports.customer"),
        ("VALIDATE sports.customer.", "sports.customer"),
        ("RELEASE customer.", "customer"),
    ] {
        let tokens = tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
        let name = match &program.statements[0].kind {
            StatementKind::Create {
                target: CreateTarget::Name(n),
                ..
            } => &n.name,
            StatementKind::Delete { buffer, .. }
            | StatementKind::Release { buffer, .. }
            | StatementKind::Validate { buffer, .. } => &buffer.name,
            other => panic!("{source}: {other:?}"),
        };
        assert_eq!(name, expected, "{source}");
    }
    let source = "BUFFER-COPY sports.customer TO sports.cust2.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    match &program.statements[..] {
        [s] => match &s.kind {
            StatementKind::BufferCopy { source, target, .. } => {
                assert_eq!(source.name, "sports.customer");
                assert_eq!(target.name, "sports.cust2");
            }
            other => panic!("{other:?}"),
        },
        other => panic!("{other:?}"),
    }
}

#[test]
fn buffer_definitions_take_database_qualified_tables() {
    for source in [
        "DEFINE BUFFER b FOR sports.customer.",
        "DEFINE BUFFER b FOR sports.customer .",
        "DEFINE PARAMETER BUFFER b FOR sports.customer.",
        "DEFINE INPUT PARAMETER BUFFER b FOR sports.customer.",
    ] {
        let tokens = tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        assert!(program.errors.is_empty(), "{source}: {:?}", program.errors);
        assert_eq!(program.statements.len(), 1, "{source}");
        let target = match &program.statements[0].kind {
            StatementKind::DefineBuffer {
                target: BufferTarget::Table(t),
                ..
            } => &t.name,
            StatementKind::DefineParameter {
                param_type: ParameterType::Buffer { target, .. },
                ..
            } => &target.name,
            other => panic!("{source}: {other:?}"),
        };
        assert_eq!(target, "sports.customer", "{source}");
    }
}

#[test]
fn temp_table_like_takes_a_database_qualified_table() {
    let source = "DEFINE TEMP-TABLE tt NO-UNDO LIKE sports.customer FIELD x AS INTEGER.";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0].kind {
        StatementKind::DefineTempTable {
            like_table: Some(like),
            fields,
            ..
        } => {
            assert_eq!(like.name, "sports.customer");
            assert_eq!(fields.len(), 1);
        }
        other => panic!("{other:?}"),
    }
}

#[test]
fn block_headers_cut_at_the_end_of_the_file_do_not_panic() {
    for source in [
        "DO ON",
        "DO ON ERROR",
        "DO ON ERROR UNDO",
        "DO ON ERROR UNDO,",
        "REPEAT TRANSACTION ON",
        "DO FOR customer TRANSACTION ON",
        "DO TRANSACTION",
        "REPEAT FOR",
        "DO FOR sports.",
        "FOR EACH customer ON",
    ] {
        let tokens = tokenize(source);
        let program = Parser::new(&tokens, source).parse_program();
        assert!(!program.errors.is_empty(), "{source}");
    }
}
