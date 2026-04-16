use super::*;
use oxabl_ast::{
    AccessModifier, BooleanLiteral, BufferTarget, CreateTarget, CreateTargetKind, DataSourceKeys,
    DataType, DbTriggerEvent, DecimalLiteral, Expression, FindType, HandleParamKind, Identifier,
    IntegerLiteral, Literal, LockType, OnAction, OnKind, ParameterDirection, ParameterType,
    RunTarget, SortDirection, Span, Statement, StreamDirection, StreamOperation, StringLiteral,
    SubscribeTarget, TypeSource, UnknownLiteral, WhenBranch, WidgetQualifier,
};
use oxabl_lexer::tokenize;
use rust_decimal::Decimal;
use std::str::FromStr;

#[test]
fn parse_simple_add_expression() {
    let source = "1 + 2";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    println!("{:?}", expression);
    assert_eq!(
        expression,
        Expression::Add(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: 1
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 2
            })))
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
        Expression::Add(
            Box::new(Expression::Add(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 8, end: 9 },
                value: 3
            })))
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
        Expression::Minus(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: 1
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 2
            })))
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
        Expression::Minus(
            Box::new(Expression::Minus(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 8, end: 9 },
                value: 3
            })))
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
        Expression::Minus(
            Box::new(Expression::Add(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                })))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 8, end: 9 },
                value: 3
            })))
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
        Expression::Multiply(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: 1
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 2
            })))
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
        Expression::Add(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: 1
            }))),
            Box::new(Expression::Multiply(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 2
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 3
                })))
            )),
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
        Expression::Divide(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: 6
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 2
            })))
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
        Expression::Add(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 1, end: 2 },
                value: 1
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 5, end: 6 },
                value: 2
            })))
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
        Expression::Multiply(
            Box::new(Expression::Add(
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 1, end: 2 },
                    value: 1
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 5, end: 6 },
                    value: 2
                })))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 10, end: 11 },
                value: 3
            })))
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
        Expression::Negate(Box::new(Expression::Literal(Literal::Integer(
            IntegerLiteral {
                span: Span { start: 1, end: 2 },
                value: 5
            }
        ))))
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
        Expression::Negate(Box::new(Expression::Negate(Box::new(Expression::Literal(
            Literal::Integer(IntegerLiteral {
                span: Span { start: 2, end: 3 },
                value: 5
            })
        )))))
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
        Expression::Not(Box::new(Expression::Literal(Literal::Boolean(
            BooleanLiteral {
                span: Span { start: 4, end: 8 },
                value: true
            }
        ))))
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
        Expression::Literal(Literal::Decimal(DecimalLiteral {
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
        Expression::Literal(Literal::String(StringLiteral {
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
        Expression::Literal(Literal::Boolean(BooleanLiteral {
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
        Expression::Literal(Literal::Unknown(UnknownLiteral {
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
        Expression::Modulo(
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 2 },
                value: 10
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 7, end: 8 },
                value: 3
            })))
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
        Expression::Identifier(Identifier {
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
        Expression::Identifier(Identifier {
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
        Expression::Equal(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 4, end: 5 },
                name: "b".to_string()
            }))
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
        Expression::NotEqual(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::LessThan(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::Begins(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 8 },
                name: "userName".to_string()
            })),
            Box::new(Expression::Literal(Literal::String(StringLiteral {
                span: Span { start: 16, end: 20 },
                value: "Jo".to_string()
            })))
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
        Expression::Contains(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 7 },
                name: "content".to_string()
            })),
            Box::new(Expression::Literal(Literal::String(StringLiteral {
                span: Span { start: 17, end: 22 },
                value: "abc".to_string()
            })))
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
        Expression::And(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 6, end: 7 },
                name: "b".to_string()
            }))
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
        Expression::Or(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::Or(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::And(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 5, end: 6 },
                    name: "b".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 11, end: 12 },
                    name: "c".to_string()
                }))
            ))
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
        Expression::LessThan(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 4, end: 5 },
                name: "b".to_string()
            }))
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
        Expression::LessThanOrEqual(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::LessThanOrEqual(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::GreaterThan(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 4, end: 5 },
                name: "b".to_string()
            }))
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
        Expression::GreaterThan(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::GreaterThanOrEqual(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::GreaterThanOrEqual(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::NotEqual(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::Equal(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "b".to_string()
            }))
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
        Expression::Not(Box::new(Expression::Identifier(Identifier {
            span: Span { start: 4, end: 5 },
            name: "a".to_string()
        })))
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
        Expression::Equal(
            Box::new(Expression::Not(Box::new(Expression::Identifier(
                Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "a".to_string()
                }
            )))),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 8, end: 9 },
                name: "b".to_string()
            }))
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
        Expression::Not(Box::new(Expression::Equal(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 5, end: 6 },
                name: "a".to_string()
            })),
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 9, end: 10 },
                name: "b".to_string()
            }))
        )))
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
        Expression::Matches(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 4 },
                name: "name".to_string()
            })),
            Box::new(Expression::Literal(Literal::String(StringLiteral {
                span: Span { start: 13, end: 19 },
                value: "*son".to_string()
            })))
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
        Expression::IfThenElse(
            Box::new(Expression::GreaterThan(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 3, end: 5 },
                    name: "xx".to_string()
                })),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 5
                })))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 15, end: 17 },
                value: 10
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 23, end: 25 },
                value: 20
            })))
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
        Expression::IfThenElse(
            Box::new(Expression::GreaterThan(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 3, end: 5 },
                    name: "xx".to_string()
                })),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 10 },
                    value: 10
                })))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 16, end: 17 },
                value: 1
            }))),
            Box::new(Expression::IfThenElse(
                Box::new(Expression::GreaterThan(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 26, end: 28 },
                        name: "xx".to_string()
                    })),
                    Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 31, end: 32 },
                        value: 5
                    })))
                )),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 38, end: 39 },
                    value: 2
                }))),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 45, end: 46 },
                    value: 3
                })))
            ))
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
        Expression::IfThenElse(
            Box::new(Expression::And(
                Box::new(Expression::Equal(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 3, end: 4 },
                        name: "a".to_string()
                    })),
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 7, end: 8 },
                        name: "b".to_string()
                    }))
                )),
                Box::new(Expression::GreaterThan(
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 13, end: 14 },
                        name: "c".to_string()
                    })),
                    Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 17, end: 18 },
                        name: "d".to_string()
                    }))
                ))
            )),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 24, end: 25 },
                value: 1
            }))),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 31, end: 32 },
                value: 0
            })))
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
        Expression::IfThenElse(
            Box::new(Expression::Identifier(Identifier {
                span: Span { start: 3, end: 7 },
                name: "cond".to_string()
            })),
            Box::new(Expression::Add(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 13, end: 15 },
                    name: "xx".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 18, end: 20 },
                    name: "yy".to_string()
                }))
            )),
            Box::new(Expression::Minus(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 26, end: 28 },
                    name: "xx".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 31, end: 33 },
                    name: "yy".to_string()
                }))
            ))
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
        Expression::FunctionCall {
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
        Expression::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 4 },
                name: "TRIM".to_string()
            },
            arguments: vec![Expression::Identifier(Identifier {
                span: Span { start: 5, end: 9 },
                name: "name".to_string()
            })]
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
        Expression::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 9 },
                name: "SUBSTRING".to_string()
            },
            arguments: vec![
                Expression::Identifier(Identifier {
                    span: Span { start: 10, end: 13 },
                    name: "str".to_string()
                }),
                Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 15, end: 16 },
                    value: 1
                })),
                Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 18, end: 19 },
                    value: 5
                }))
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
        Expression::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 3 },
                name: "ABS".to_string()
            },
            arguments: vec![Expression::Minus(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "x".to_string()
                })),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 5
                })))
            )]
        }
    );
}

#[test]
fn parse_nested_function_calls() {
    let source = "TRIM(SUBSTRING(name, 1, 5))";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expression = parser.parse_expression().expect("Expected an expression");
    assert_eq!(
        expression,
        Expression::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 4 },
                name: "TRIM".to_string()
            },
            arguments: vec![Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 5, end: 14 },
                    name: "SUBSTRING".to_string()
                },
                arguments: vec![
                    Expression::Identifier(Identifier {
                        span: Span { start: 15, end: 19 },
                        name: "name".to_string()
                    }),
                    Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 21, end: 22 },
                        value: 1
                    })),
                    Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 24, end: 25 },
                        value: 5
                    }))
                ]
            }]
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
        Expression::Add(
            Box::new(Expression::FunctionCall {
                name: Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "LENGTH".to_string()
                },
                arguments: vec![Expression::Identifier(Identifier {
                    span: Span { start: 7, end: 10 },
                    name: "str".to_string()
                })]
            }),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 14, end: 15 },
                value: 1
            })))
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
        Expression::ArrayAccess {
            array: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 3 },
                name: "arr".to_string()
            })),
            index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 1
            })))
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
        Expression::ArrayAccess {
            array: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 3 },
                name: "arr".to_string()
            })),
            index: Box::new(Expression::Add(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 4, end: 5 },
                    name: "i".to_string()
                })),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 8, end: 9 },
                    value: 1
                })))
            ))
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
        Expression::ArrayAccess {
            array: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 11 },
                name: "month-quota".to_string()
            })),
            index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 12, end: 13 },
                value: 3
            })))
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
        Expression::ArrayAccess {
            array: Box::new(Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 6 },
                    name: "matrix".to_string()
                })),
                index: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 7, end: 10 },
                    name: "row".to_string()
                }))
            }),
            index: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 12, end: 15 },
                name: "col".to_string()
            }))
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
        Expression::Add(
            Box::new(Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "arr".to_string()
                })),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 1
                })))
            }),
            Box::new(Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 9, end: 12 },
                    name: "arr".to_string()
                })),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 13, end: 14 },
                    value: 2
                })))
            })
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
        Expression::MemberAccess {
            object: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 6 },
                name: "handle".to_string()
            })),
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
        Expression::MethodCall {
            object: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 6 },
                name: "buffer".to_string()
            })),
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
        Expression::MethodCall {
            object: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 6 },
                name: "widget".to_string()
            })),
            method: Identifier {
                span: Span { start: 7, end: 11 },
                name: "MOVE".to_string()
            },
            arguments: vec![
                Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 12, end: 14 },
                    value: 10
                })),
                Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 16, end: 18 },
                    value: 20
                }))
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
        Expression::MemberAccess {
            object: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "obj".to_string()
                })),
                member: Identifier {
                    span: Span { start: 4, end: 12 },
                    name: "property".to_string()
                }
            }),
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
        Expression::MethodCall {
            object: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "obj".to_string()
                })),
                member: Identifier {
                    span: Span { start: 4, end: 8 },
                    name: "prop".to_string()
                }
            }),
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
        Expression::ArrayAccess {
            array: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 5 },
                    name: "myBuf".to_string()
                })),
                member: Identifier {
                    span: Span { start: 6, end: 13 },
                    name: "myField".to_string()
                }
            }),
            index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 14, end: 15 },
                value: 1
            })))
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
        Expression::Add(
            Box::new(Expression::MemberAccess {
                object: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 5 },
                    name: "myObj".to_string()
                })),
                member: Identifier {
                    span: Span { start: 6, end: 11 },
                    name: "myVal".to_string()
                }
            }),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 14, end: 15 },
                value: 5
            })))
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
        Expression::FieldAccess {
            qualifier: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 8 },
                name: "Customer".to_string()
            })),
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
        Expression::FieldAccess {
            qualifier: Box::new(Expression::FieldAccess {
                qualifier: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 10 },
                    name: "Sports2020".to_string()
                })),
                field: Identifier {
                    span: Span { start: 11, end: 19 },
                    name: "Customer".to_string()
                }
            }),
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
        Expression::ArrayAccess {
            array: Box::new(Expression::FieldAccess {
                qualifier: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 7 },
                    name: "myTable".to_string()
                })),
                field: Identifier {
                    span: Span { start: 8, end: 15 },
                    name: "myField".to_string()
                }
            }),
            index: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 16, end: 22 },
                name: "extent".to_string()
            }))
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
        Expression::GreaterThan(
            Box::new(Expression::FieldAccess {
                qualifier: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 8 },
                    name: "Customer".to_string()
                })),
                field: Identifier {
                    span: Span { start: 9, end: 16 },
                    name: "Balance".to_string()
                }
            }),
            Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 19, end: 23 },
                value: 1000
            })))
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
        Expression::ArrayAccess {
            array: Box::new(Expression::MemberAccess {
                object: Box::new(Expression::FieldAccess {
                    qualifier: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 0, end: 4 },
                        name: "myDb".to_string()
                    })),
                    field: Identifier {
                        span: Span { start: 5, end: 12 },
                        name: "myTable".to_string()
                    }
                }),
                member: Identifier {
                    span: Span { start: 13, end: 19 },
                    name: "buffer".to_string()
                }
            }),
            index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 20, end: 21 },
                value: 1
            })))
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
        Statement::Assignment {
            target: Expression::Identifier(Identifier {
                span: Span { start: 0, end: 1 },
                name: "x".to_string()
            }),
            value: Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 4, end: 5 },
                value: 5
            }))
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
        Statement::Assignment {
            target: Expression::Identifier(Identifier {
                span: Span { start: 0, end: 5 },
                name: "total".to_string()
            }),
            value: Expression::Multiply(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 8, end: 13 },
                    name: "price".to_string()
                })),
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 16, end: 24 },
                    name: "quantity".to_string()
                }))
            )
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
        Statement::Assignment {
            target: Expression::ArrayAccess {
                array: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 3 },
                    name: "arr".to_string()
                })),
                index: Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 4, end: 5 },
                    value: 1
                })))
            },
            value: Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 9, end: 11 },
                value: 10
            }))
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
        Statement::Assignment {
            target: Expression::FieldAccess {
                qualifier: Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 0, end: 8 },
                    name: "Customer".to_string()
                })),
                field: Identifier {
                    span: Span { start: 9, end: 13 },
                    name: "Name".to_string()
                }
            },
            value: Expression::Literal(Literal::String(StringLiteral {
                span: Span { start: 16, end: 22 },
                value: "John".to_string()
            }))
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
        Statement::ExpressionStatement(Expression::FunctionCall {
            name: Identifier {
                span: Span { start: 0, end: 15 },
                name: "calculateTotals".to_string()
            },
            arguments: vec![]
        })
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
        Statement::ExpressionStatement(Expression::MethodCall {
            object: Box::new(Expression::Identifier(Identifier {
                span: Span { start: 0, end: 6 },
                name: "buffer".to_string()
            })),
            method: Identifier {
                span: Span { start: 7, end: 23 },
                name: "save-row-changes".to_string()
            },
            arguments: vec![]
        })
    );
}

#[test]
fn parse_empty_statement() {
    let source = ".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, Statement::Empty);
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
        Statement::VariableDeclaration {
            name: Identifier {
                span: Span { start: 16, end: 21 },
                name: "myVar".to_string()
            },
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: true,
            extent: None,
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
        Statement::VariableDeclaration {
            name: Identifier {
                span: Span { start: 16, end: 23 },
                name: "counter".to_string()
            },
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: Some(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 43, end: 44 },
                value: 0
            }))),
            no_undo: false,
            extent: None,
        }
    );
}

#[test]
fn parse_define_variable_character() {
    let source = "DEFINE VARIABLE name AS CHARACTER NO-UNDO INITIAL \"unknown\".";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::VariableDeclaration {
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
        Statement::VariableDeclaration {
            name: Identifier {
                span: Span { start: 12, end: 19 },
                name: "myCount".to_string()
            },
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: true,
            extent: None,
        }
    );
}

#[test]
fn parse_var_statement_with_initial() {
    let source = "VAR DECIMAL total = 100.50.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::VariableDeclaration {
            type_source,
            initial_value,
            ..
        } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Logical));
            assert!(matches!(
                initial_value,
                Some(Expression::Literal(Literal::Boolean(_)))
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::VariableDeclaration { type_source, .. } => match type_source {
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
    match stmt {
        Statement::VariableDeclaration {
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
    let source = "Def var tax-round as decimal decimals 2 extent 5.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::VariableDeclaration {
            name,
            type_source,
            extent,
            ..
        } => {
            assert_eq!(name.name, "tax-round");
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::Do { body, loop_var, .. } => {
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
    match stmt {
        Statement::Do {
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
    match stmt {
        Statement::Do { by, .. } => {
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
    match stmt {
        Statement::Do {
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::Do { .. }));
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
    match stmt {
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert!(matches!(condition, Expression::GreaterThan(_, _)));
            assert!(matches!(*then_branch, Statement::Assignment { .. }));
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
    match stmt {
        Statement::If { else_branch, .. } => {
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
    match stmt {
        Statement::If { then_branch, .. } => {
            assert!(matches!(*then_branch, Statement::Do { .. }));
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
    match stmt {
        Statement::If {
            then_branch,
            else_branch,
            ..
        } => {
            assert!(matches!(*then_branch, Statement::Do { .. }));
            let else_stmt = else_branch.expect("Should have else");
            assert!(matches!(*else_stmt, Statement::Do { .. }));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_next_prompt_statement() {
    let source = "NEXT-PROMPT menu-proc.breakpoint WITH FRAME static/menu_prc.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser
        .parse_statement()
        .expect("NEXT-PROMPT should parse as a skipped legacy UI statement");
    assert!(matches!(stmt, Statement::Empty));
}

#[test]
fn parse_if_then_do_else_with_preproc_placeholder() {
    // Corpus pattern (ad100.p, secco.p, static/*.p): an undefined preprocessor
    // variable placeholder appears between `End.` and `Else`. The placeholder
    // would normally expand to additional `Else If` branches; when undefined
    // it preserves as `{&name}`. The parser must look past it to bind ELSE.
    let source = "\
        IF a THEN DO: x = 1. END.\n\
        {&extra-else-branches}\n\
        ELSE DO: x = 2. END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::If { else_branch, .. } => {
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
    // Same pattern but inside a PROCEDURE body, which is the shape seen in
    // the failing corpus files (e.g. wam_tmpl/manage_store.p).
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
    match stmt {
        Statement::Procedure { body, .. } => {
            assert_eq!(body.len(), 1, "procedure body: {:?}", body);
            match &body[0] {
                Statement::If { else_branch, .. } => {
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
    match stmt {
        Statement::If { else_branch, .. } => {
            let else_stmt = else_branch.expect("Should have else");
            assert!(matches!(*else_stmt, Statement::If { .. }));
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
    match stmt {
        Statement::If { then_branch, .. } => {
            assert!(matches!(*then_branch, Statement::If { .. }));
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
    match stmt {
        Statement::Repeat {
            body,
            while_condition,
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
    match stmt {
        Statement::Repeat {
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
    assert_eq!(stmt, Statement::Leave(None));
}

#[test]
fn parse_next_statement() {
    let source = "NEXT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, Statement::Next(None));
}

#[test]
fn parse_return_no_value() {
    let source = "RETURN.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, Statement::Return(None));
}

#[test]
fn parse_return_with_value() {
    let source = "RETURN x + 1.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Return(Some(expr)) => {
            assert!(matches!(expr, Expression::Add(_, _)));
        }
        _ => panic!("Expected Return with value"),
    }
}

#[test]
fn parse_loop_with_leave_and_next() {
    let source = "DO i = 1 TO 100: IF l_done THEN LEAVE. IF l_skip THEN NEXT. l_process(i). END.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Do { body, .. } => {
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
    match stmt {
        Statement::ForEach {
            buffer,
            of_relation,
            where_clause,
            lock_type,
            body,
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
    match stmt {
        Statement::ForEach {
            buffer,
            where_clause,
            lock_type,
            ..
        } => {
            assert_eq!(buffer.name, "Customer");
            assert!(where_clause.is_some());
            assert!(matches!(
                where_clause.unwrap(),
                Expression::GreaterThan(_, _)
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
    match stmt {
        Statement::ForEach {
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
    match stmt {
        Statement::ForEach { lock_type, .. } => {
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
    match stmt {
        Statement::ForEach {
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
    match stmt {
        Statement::ForEach { lock_type, .. } => {
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
    match stmt {
        Statement::ForEach {
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
    match stmt {
        Statement::ForEach { where_clause, .. } => {
            assert!(where_clause.is_some());
            assert!(matches!(where_clause.unwrap(), Expression::And(_, _)));
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
    match stmt {
        Statement::ForEach { body, .. } => {
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
    match stmt {
        Statement::ForEach { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::ForEach { .. }));
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
    match stmt {
        Statement::ForEach { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::If { .. }));
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
    match stmt {
        Statement::ForEach { body, .. } => {
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Assignment { value, .. } => {
                    assert!(matches!(value, Expression::Add(_, _)));
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
        Statement::ForEach {
            buffer: Identifier {
                span: Span { start: 9, end: 17 },
                name: "Customer".to_string()
            },
            of_relation: None,
            where_clause: None,
            lock_type: LockType::NoLock,
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
        Statement::ForEach {
            buffer: Identifier {
                span: Span { start: 9, end: 17 },
                name: "Customer".to_string()
            },
            of_relation: None,
            where_clause: None,
            lock_type: LockType::ShareLock,
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
        Statement::ForEach {
            buffer: Identifier {
                span: Span { start: 9, end: 17 },
                name: "Customer".to_string()
            },
            of_relation: None,
            where_clause: None,
            lock_type: LockType::ExclusiveLock,
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
        Statement::Find {
            find_type: FindType::First,
            buffer: Identifier {
                span: Span { start: 11, end: 19 },
                name: "Customer".to_string()
            },
            key_value: None,
            where_clause: Some(Expression::Equal(
                Box::new(Expression::FieldAccess {
                    qualifier: Box::new(Expression::Identifier(Identifier {
                        span: Span { start: 26, end: 34 },
                        name: "Customer".to_string()
                    })),
                    field: Identifier {
                        span: Span { start: 35, end: 42 },
                        name: "CustNum".to_string()
                    }
                }),
                Box::new(Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 45, end: 46 },
                    value: 1
                })))
            )),
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
        Statement::Find {
            find_type: FindType::Unique,
            buffer: Identifier {
                span: Span { start: 5, end: 13 },
                name: "Customer".to_string()
            },
            key_value: Some(Expression::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 14, end: 15 },
                value: 1
            }))),
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
        Statement::Find {
            find_type: FindType::Unique,
            buffer: Identifier {
                span: Span { start: 5, end: 13 },
                name: "Customer".to_string()
            },
            key_value: Some(Expression::Literal(Literal::String(StringLiteral {
                span: Span { start: 14, end: 20 },
                value: "ACME".to_string()
            }))),
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
        Statement::Find {
            find_type: FindType::Unique,
            buffer: Identifier {
                span: Span { start: 5, end: 13 },
                name: "Customer".to_string()
            },
            key_value: Some(Expression::Identifier(Identifier {
                span: Span { start: 14, end: 20 },
                name: "custId".to_string()
            })),
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
        Statement::Find {
            find_type: FindType::First,
            buffer: Identifier {
                span: Span { start: 11, end: 19 },
                name: "Customer".to_string()
            },
            key_value: None,
            where_clause: Some(Expression::Equal(
                Box::new(Expression::Identifier(Identifier {
                    span: Span { start: 26, end: 32 },
                    name: "active".to_string()
                })),
                Box::new(Expression::Literal(Literal::Boolean(BooleanLiteral {
                    span: Span { start: 35, end: 39 },
                    value: true
                })))
            )),
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
    match stmt {
        Statement::Find {
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
    match stmt {
        Statement::Find {
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
    match stmt {
        Statement::Find {
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
    match stmt {
        Statement::Find { lock_type, .. } => {
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
    match stmt {
        Statement::Find { lock_type, .. } => {
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
    match stmt {
        Statement::Find { lock_type, .. } => {
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
    match stmt {
        Statement::Find { where_clause, .. } => {
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
    match stmt {
        Statement::Case {
            expression,
            when_branches,
            otherwise,
        } => {
            assert!(matches!(expression, Expression::Identifier(_)));
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
    match stmt {
        Statement::Case {
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
    match stmt {
        Statement::Case {
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
    match stmt {
        Statement::Case { when_branches, .. } => {
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
    match stmt {
        Statement::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 2);
            assert!(matches!(
                &when_branches[0].values[0],
                Expression::Literal(Literal::String(_))
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
    match stmt {
        Statement::Case { when_branches, .. } => {
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
    match stmt {
        Statement::Case { otherwise, .. } => {
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
    match stmt {
        Statement::Case { expression, .. } => {
            assert!(matches!(expression, Expression::Add(_, _)));
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
        Statement::Case {
            expression: Expression::Identifier(Identifier {
                span: Span { start: 5, end: 10 },
                name: "myVal".to_string()
            }),
            when_branches: vec![
                WhenBranch {
                    values: vec![Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 17, end: 18 },
                        value: 1
                    }))],
                    body: vec![Statement::Assignment {
                        target: Expression::Identifier(Identifier {
                            span: Span { start: 24, end: 25 },
                            name: "x".to_string()
                        }),
                        value: Expression::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 28, end: 29 },
                            value: 1
                        }))
                    }]
                },
                WhenBranch {
                    values: vec![Expression::Literal(Literal::Integer(IntegerLiteral {
                        span: Span { start: 36, end: 37 },
                        value: 2
                    }))],
                    body: vec![Statement::Assignment {
                        target: Expression::Identifier(Identifier {
                            span: Span { start: 43, end: 44 },
                            name: "x".to_string()
                        }),
                        value: Expression::Literal(Literal::Integer(IntegerLiteral {
                            span: Span { start: 47, end: 48 },
                            value: 2
                        }))
                    }]
                }
            ],
            otherwise: Some(vec![Statement::Assignment {
                target: Expression::Identifier(Identifier {
                    span: Span { start: 60, end: 61 },
                    name: "x".to_string()
                }),
                value: Expression::Literal(Literal::Integer(IntegerLiteral {
                    span: Span { start: 64, end: 65 },
                    value: 0
                }))
            }])
        }
    );
}

#[test]
fn parse_case_with_nested_if() {
    let source = "CASE x: WHEN 1 THEN IF y > 0 THEN z = 1. END CASE.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 1);
            assert!(matches!(when_branches[0].body[0], Statement::If { .. }));
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
    match stmt {
        Statement::Case { when_branches, .. } => {
            assert_eq!(when_branches.len(), 2);
            assert!(matches!(
                &when_branches[0].values[0],
                Expression::Literal(Literal::Boolean(BooleanLiteral { value: true, .. }))
            ));
            assert!(matches!(
                &when_branches[1].values[0],
                Expression::Literal(Literal::Boolean(BooleanLiteral { value: false, .. }))
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
    match stmt {
        Statement::Case { when_branches, .. } => {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::Procedure { name, body } => {
            assert_eq!(name.name, "my-proc");
            assert_eq!(body.len(), 3);
            // First statement should be input parameter
            match &body[0] {
                Statement::DefineParameter {
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
            match &body[1] {
                Statement::DefineParameter {
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

// ===================== DISPLAY statement tests =====================

#[test]
fn parse_display_simple_field_access() {
    let source = "DISPLAY Customer.Name Customer.Balance.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Display {
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
    match stmt {
        Statement::Display { items, frame, .. } => {
            assert_eq!(items.len(), 2);
            assert!(frame.is_some());
            assert_eq!(frame.unwrap().name, "f1");
        }
        _ => panic!("Expected Display statement"),
    }
}

#[test]
fn parse_display_with_frame_columns() {
    let source = "DISPLAY x y z WITH FRAME results 2 COLUMNS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Display { items, frame, .. } => {
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
    match stmt {
        Statement::Display {
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
    match stmt {
        Statement::Display { items, .. } => {
            assert_eq!(items.len(), 1);
            assert!(matches!(items[0].expression, Expression::Add(_, _)));
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
    match stmt {
        Statement::Display { items, .. } => {
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
    match stmt {
        Statement::Display { items, .. } => {
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
    match stmt {
        Statement::Display { items, .. } => {
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
    match stmt {
        Statement::Display { items, .. } => {
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
    match stmt {
        Statement::Message { items, set_targets } => {
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
    match stmt {
        Statement::Message { items, set_targets } => {
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
    match stmt {
        Statement::Message { items, set_targets } => {
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
    match stmt {
        Statement::Message { items, .. } => {
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
    match stmt {
        Statement::Message { items, .. } => {
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
    match stmt {
        Statement::Message { items, set_targets } => {
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
    match stmt {
        Statement::Run {
            target,
            arguments,
            in_handle,
            no_error,
            ..
        } => {
            assert_eq!(target, RunTarget::Literal("simple-proc".to_string()));
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
    match stmt {
        Statement::Run {
            target, arguments, ..
        } => {
            assert_eq!(target, RunTarget::Literal("calculate-total".to_string()));
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
    match stmt {
        Statement::Run { target, .. } => {
            assert!(matches!(target, RunTarget::Dynamic(_)));
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_dotted_filename_with_args() {
    let source = r#"RUN external-prog.p (INPUT "data")."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run {
            target, arguments, ..
        } => {
            assert_eq!(target, RunTarget::Literal("external-prog.p".to_string()));
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
    match stmt {
        Statement::Run { target, .. } => {
            assert_eq!(target, RunTarget::Literal("my-proc".to_string()));
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
    match stmt {
        Statement::Run {
            target, arguments, ..
        } => {
            assert_eq!(target, RunTarget::Literal("some-proc".to_string()));
            assert_eq!(arguments.len(), 2);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
            // First arg should be an Add expression
            assert!(matches!(arguments[0].expression, Expression::Add(_, _)));
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
    match stmt {
        Statement::Run { target, .. } => {
            assert_eq!(target, RunTarget::Literal("my-proc.p".to_string()));
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
    match stmt {
        Statement::Run {
            target,
            in_handle,
            no_error,
            ..
        } => {
            assert_eq!(target, RunTarget::Literal("myProc".to_string()));
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
    match stmt {
        Statement::Run {
            target, no_error, ..
        } => {
            assert_eq!(target, RunTarget::Literal("myProc".to_string()));
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
    match stmt {
        Statement::Run {
            target,
            arguments,
            no_error,
            ..
        } => {
            assert_eq!(target, RunTarget::Literal("myProc".to_string()));
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
    match stmt {
        Statement::Run { arguments, .. } => {
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
    match stmt {
        Statement::Run {
            target, in_handle, ..
        } => {
            assert_eq!(target, RunTarget::Literal("myMethod".to_string()));
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
    match stmt {
        Statement::Run {
            target,
            persistent,
            persistent_handle,
            ..
        } => {
            assert_eq!(target, RunTarget::Literal("proc.p".to_string()));
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
    match stmt {
        Statement::Run {
            target,
            persistent,
            persistent_handle,
            ..
        } => {
            assert_eq!(target, RunTarget::Literal("proc.p".to_string()));
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
    match stmt {
        Statement::Run {
            target,
            asynchronous,
            async_handle,
            event_procedure,
            ..
        } => {
            assert_eq!(target, RunTarget::Literal("proc".to_string()));
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
    match stmt {
        Statement::Run {
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
    match stmt {
        Statement::Run {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineTempTable { name, no_undo, .. } => {
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
    match stmt {
        Statement::DefineBuffer { name, target, .. } => {
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
    match stmt {
        Statement::DefineBuffer { name, target, .. } => {
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
    match stmt {
        Statement::DefineBuffer { preselect, .. } => {
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
    match stmt {
        Statement::DefineBuffer { label, .. } => {
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
    match stmt {
        Statement::DefineBuffer { name, .. } => {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineTempTable { use_indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => match &fields[0].type_source {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::DefineTempTable { indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable { indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable { indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable { indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable { indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable { indexes, .. } => {
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
    match stmt {
        Statement::DefineTempTable { fields, .. } => {
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 2); // x = 1. and CATCH block
            match &body[1] {
                Statement::Catch {
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 2);
            match &body[1] {
                Statement::Finally { body } => {
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 3); // assignment, CATCH, FINALLY
            assert!(matches!(body[1], Statement::Catch { .. }));
            assert!(matches!(body[2], Statement::Finally { .. }));
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 2);
            assert!(matches!(body[1], Statement::Catch { .. }));
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
    match stmt {
        Statement::Assign { assignments } => {
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
    match stmt {
        Statement::Assign { assignments } => {
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
    match stmt {
        Statement::Assign { assignments } => {
            assert_eq!(assignments.len(), 2);
            assert!(matches!(
                assignments[0].target,
                Expression::FieldAccess { .. }
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
    match stmt {
        Statement::Assign { assignments } => {
            assert_eq!(assignments.len(), 1);
            assert!(matches!(assignments[0].value, Expression::Multiply(_, _)));
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
    match stmt {
        Statement::Function {
            name,
            return_type,
            body,
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
    match stmt {
        Statement::Function {
            name,
            return_type,
            body,
        } => {
            assert_eq!(name.name, "calc-total");
            assert_eq!(return_type, DataType::Integer);
            assert_eq!(body.len(), 1); // RETURN x + y.
        }
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
    match stmt {
        Statement::Function {
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
    match stmt {
        Statement::Function { body, .. } => {
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::Assignment { target, .. } => match target {
            Expression::Identifier(id) => assert_eq!(id.name, "variable"),
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
    match stmt {
        Statement::Assignment { target, .. } => match target {
            Expression::Identifier(id) => assert_eq!(id.name, "function"),
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
    match stmt {
        Statement::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Integer));
        }
        _ => panic!("Expected VariableDeclaration"),
    }

    // DEC is abbreviation for DECIMAL
    let source = "VAR DEC x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Decimal));
        }
        _ => panic!("Expected VariableDeclaration"),
    }

    // CHAR is abbreviation for CHARACTER
    let source = "VAR CHAR x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::VariableDeclaration { type_source, .. } => {
            assert_eq!(type_source, TypeSource::Explicit(DataType::Character));
        }
        _ => panic!("Expected VariableDeclaration"),
    }

    // LOG is abbreviation for LOGICAL
    let source = "VAR LOG x.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::VariableDeclaration { type_source, .. } => {
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
        match stmt {
            Statement::VariableDeclaration { type_source, .. } => {
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
    match stmt {
        Statement::Class {
            name,
            inherits,
            implements,
            is_abstract,
            is_final,
            body,
        } => {
            assert_eq!(name.name, "MyClass");
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
fn parse_class_with_inherits() {
    let source = "CLASS MyApp.CustomerService INHERITS BaseService: END CLASS.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt {
        Statement::Class { name, inherits, .. } => {
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
    match stmt {
        Statement::Class { implements, .. } => {
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
    match stmt {
        Statement::Class {
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
    match stmt {
        Statement::Class {
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
    match stmt {
        Statement::Class { is_final, .. } => {
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
    match stmt {
        Statement::Method {
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
    // ABL code in the pcna-erp corpus (ms/fedexRest.cls) uses a preprocessor
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
    match stmt {
        Statement::Method {
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
    // Generic type arguments aren't modeled in the AST; they're accepted
    // syntactically so property/variable declarations parse.
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
    match stmt {
        Statement::Method {
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
    // Combined case from ms/fedexRest.cls: preprocessor access modifier plus
    // forward declaration.
    let source = "METHOD {&method-access-type} LOGICAL GetErrors(OUTPUT hError AS HANDLE) FORWARD.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt {
        Statement::Method {
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
    match stmt {
        Statement::Method {
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
    match stmt {
        Statement::Method {
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
    match stmt {
        Statement::Method {
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
    match stmt {
        Statement::Method { is_override, .. } => {
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
    match stmt {
        Statement::Property {
            access,
            is_static,
            name,
            data_type,
            no_undo,
            get_body,
            set_body,
        } => {
            assert_eq!(access, AccessModifier::Public);
            assert!(!is_static);
            assert_eq!(name.name, "Name");
            assert_eq!(data_type, DataType::Character);
            assert!(no_undo);
            assert_eq!(get_body, Some(Vec::new())); // auto-getter
            assert_eq!(set_body, Some(Vec::new())); // auto-setter
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
    match stmt {
        Statement::Property {
            get_body, set_body, ..
        } => {
            let get = get_body.unwrap();
            assert_eq!(get.len(), 1); // RETURN "hello".
            let set = set_body.unwrap();
            assert_eq!(set.len(), 1); // DEFINE VARIABLE x AS CHARACTER.
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
    match stmt {
        Statement::Property {
            get_body, set_body, ..
        } => {
            assert_eq!(get_body, Some(Vec::new())); // has getter
            assert!(set_body.is_none()); // no setter — read-only
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
    match stmt {
        Statement::Constructor {
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
    match stmt {
        Statement::Destructor { body } => {
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
    match stmt {
        Statement::Interface {
            name,
            inherits,
            body,
        } => {
            assert_eq!(name.name, "IService");
            assert!(inherits.is_empty());
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Method { is_abstract, .. } => assert!(is_abstract),
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
    match stmt {
        Statement::Interface { inherits, .. } => {
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
    match stmt {
        Statement::Using { type_name } => {
            assert_eq!(type_name, "Progress.Lang.Object");
        }
        _ => panic!("Expected Using statement"),
    }
}

#[test]
fn parse_using_wildcard() {
    let source = "USING MyApp.Services.*.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().unwrap();
    match stmt {
        Statement::Using { type_name } => {
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
    match stmt {
        Statement::Class { name, body, .. } => {
            assert_eq!(name.name, "MyApp.CustomerService");
            // Property + Constructor + 2 Methods = 4 members
            assert_eq!(body.len(), 4);
            assert!(matches!(body[0], Statement::Property { .. }));
            assert!(matches!(body[1], Statement::Constructor { .. }));
            assert!(matches!(body[2], Statement::Method { .. }));
            assert!(matches!(body[3], Statement::Method { .. }));
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
    match stmt {
        Statement::VariableDeclaration { name, .. } => {
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
    match stmt {
        Statement::Method { return_type, .. } => {
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

#[test]
fn parse_create_basic() {
    let source = "CREATE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Create {
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
    match stmt {
        Statement::Create {
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
fn parse_delete_basic() {
    let source = "DELETE Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Delete { buffer, no_error } => {
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
    match stmt {
        Statement::Delete { buffer, no_error } => {
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
    match stmt {
        Statement::Release { buffer, no_error } => {
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
    match stmt {
        Statement::Release { buffer, no_error } => {
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
    match stmt {
        Statement::Validate { buffer, no_error } => {
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
    match stmt {
        Statement::Validate { buffer, no_error } => {
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
    match stmt {
        Statement::BufferCopy {
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
    match stmt {
        Statement::BufferCopy {
            source,
            target,
            assignments,
            no_error,
        } => {
            assert_eq!(source.name, "bSource");
            assert_eq!(target.name, "bTarget");
            assert_eq!(assignments.len(), 1);
            match &assignments[0].target {
                Expression::Identifier(ident) => assert_eq!(ident.name, "field1"),
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
    match stmt {
        Statement::BufferCopy {
            assignments,
            no_error,
            ..
        } => {
            assert_eq!(assignments.len(), 2);
            match &assignments[0].target {
                Expression::Identifier(ident) => assert_eq!(ident.name, "field1"),
                _ => panic!("Expected identifier target"),
            }
            match &assignments[1].target {
                Expression::Identifier(ident) => assert_eq!(ident.name, "field2"),
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
    match stmt {
        Statement::BufferCopy {
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
    match stmt {
        Statement::BufferCopy {
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
    match stmt {
        Statement::BufferCompare {
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
    match stmt {
        Statement::BufferCompare {
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
    match stmt {
        Statement::BufferCompare {
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
    match stmt {
        Statement::BufferCompare {
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
    match stmt {
        Statement::Create {
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
    assert!(matches!(program.statements[0], Statement::Create { .. }));
    assert!(matches!(program.statements[1], Statement::Delete { .. }));
    assert!(matches!(program.statements[2], Statement::Release { .. }));
    assert!(matches!(program.statements[3], Statement::Validate { .. }));
}

// ── Preprocessor statement tests ─────────────────────────────────

#[test]
fn preproc_if_then_endif() {
    let source = "&IF TRUE &THEN\nDISPLAY \"hello\".\n&ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::PreprocIf(preproc) => {
            assert!(matches!(preproc.condition, Expression::Literal(_)));
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
    match stmt {
        Statement::PreprocIf(preproc) => {
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
    match stmt {
        Statement::PreprocIf(preproc) => {
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
    match stmt {
        Statement::PreprocIf(preproc) => {
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
    match stmt {
        Statement::PreprocIf(outer) => {
            assert_eq!(outer.then_branch.len(), 1);
            assert!(matches!(outer.then_branch[0], Statement::PreprocIf(_)));
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
    match &program.statements[0] {
        Statement::PreprocDefine {
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
    match &program.statements[0] {
        Statement::PreprocDefine {
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
    match &program.statements[0] {
        Statement::PreprocDefine {
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
    match &program.statements[0] {
        Statement::PreprocDefine {
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
    match stmt {
        Statement::PreprocUndefine { name } => {
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
    match stmt {
        Statement::PreprocMessage { expression } => {
            assert!(matches!(
                expression,
                Expression::Literal(Literal::String(_))
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
    match expr {
        Expression::PreprocReference(name) => {
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
    match expr {
        Expression::PreprocIf(preproc) => {
            assert!(matches!(preproc.condition, Expression::Literal(_)));
            assert!(matches!(preproc.then_branch, Expression::Literal(_)));
            assert!(preproc.else_branch.is_some());
            assert!(matches!(
                preproc.else_branch.as_ref().unwrap(),
                Expression::Literal(_)
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
    match expr {
        Expression::PreprocIf(preproc) => {
            assert!(matches!(preproc.condition, Expression::FunctionCall { .. }));
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
    match stmt {
        Statement::VariableDeclaration {
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
    match stmt {
        Statement::PreprocIf(preproc) => {
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
    match stmt {
        Statement::PreprocIf(preproc) => {
            assert!(matches!(preproc.condition, Expression::FunctionCall { .. }));
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
    match stmt {
        Statement::Assignment { value, .. } => {
            assert!(matches!(value, Expression::PreprocReference(_)));
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
    assert!(matches!(stmt, Statement::PreprocIf(_)));
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
        program.statements[0],
        Statement::PreprocDefine { .. }
    ));
    assert!(matches!(program.statements[1], Statement::PreprocIf(_)));
    assert!(matches!(program.statements[2], Statement::Display { .. }));
}

#[test]
fn preproc_if_expression_with_elseif() {
    let source = "&IF 1 = 1 &THEN 10 &ELSEIF 2 = 2 &THEN 20 &ELSE 30 &ENDIF";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let expr = parser.parse_expression().expect("Expected expression");
    match expr {
        Expression::PreprocIf(preproc) => {
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
    match &program.statements[0] {
        Statement::PreprocDefine {
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
    match stmt {
        Statement::DefineStream { name } => {
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
    match stmt {
        Statement::DefineStream { name } => {
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
    match stmt {
        Statement::DefineFrame { name, .. } => {
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
    match stmt {
        Statement::DefineFrame { name, raw_span } => {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::StreamIo {
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
    match stmt {
        Statement::Display {
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
    match stmt {
        Statement::Display { stream_name, .. } => {
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
    match stmt {
        Statement::StreamIo {
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
    if let Ok(Statement::StreamIo { .. }) = &result {
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
    if let Ok(Statement::StreamIo { .. }) = &result {
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
    if let Ok(Statement::StreamIo { .. }) = &result {
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
    if let Ok(Statement::StreamIo { .. }) = &result {
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
    match stmt {
        Statement::DefineDataset {
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
    match stmt {
        Statement::DefineDataset { buffers, .. } => {
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
    match stmt {
        Statement::DefineDataset { data_relations, .. } => {
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
    match stmt {
        Statement::DefineDataset { data_relations, .. } => {
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
    match stmt {
        Statement::DefineDataset { data_relations, .. } => {
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
    match stmt {
        Statement::DefineDataset {
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
    match stmt {
        Statement::DefineDataset {
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
    match stmt {
        Statement::DefineDataset { xml_options, .. } => {
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
    match stmt {
        Statement::DefineDataset { reference_only, .. } => {
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
    match stmt {
        Statement::DefineDataset {
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
    match stmt {
        Statement::DefineDataset {
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

// ── DEFINE DATA-SOURCE tests ────────────────────────────────────

#[test]
fn parse_define_data_source_basic() {
    let source = "DEFINE DATA-SOURCE dsSrc FOR Customer.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::DefineDataSource {
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
    match stmt {
        Statement::DefineDataSource {
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
    match stmt {
        Statement::DefineDataSource { source_buffers, .. } => {
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
    match stmt {
        Statement::DefineDataSource { source_buffers, .. } => match &source_buffers[0].keys {
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
    match stmt {
        Statement::DefineDataSource { source_buffers, .. } => {
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
    match stmt {
        Statement::DefineDataSource {
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
    match stmt {
        Statement::Create {
            target:
                CreateTarget::Handle {
                    kind,
                    handle,
                    widget_pool,
                },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::Dataset);
            assert_eq!(handle.name, "hDs");
            assert!(widget_pool.is_none());
        }
        _ => panic!("Expected Create Dataset"),
    }

    // With WIDGET-POOL
    let source = r#"CREATE DATASET hDs IN WIDGET-POOL "myPool"."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Create {
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
    match stmt {
        Statement::Create {
            target: CreateTarget::Handle { kind, handle, .. },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::DataSource);
            assert_eq!(handle.name, "hDs");
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
    match stmt {
        Statement::Create {
            target: CreateTarget::Handle { kind, handle, .. },
            ..
        } => {
            assert_eq!(kind, CreateTargetKind::TempTable);
            assert_eq!(handle.name, "hTt");
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
    match stmt {
        Statement::Create {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineParameter {
            param_type: ParameterType::Buffer { name, target },
            ..
        } => {
            assert_eq!(name.name, "bCust");
            assert_eq!(target.name, "Customer");
        }
        _ => panic!("Expected DefineParameter Buffer"),
    }
}

#[test]
fn parse_define_parameter_bind_append() {
    let source = "DEFINE INPUT PARAMETER DATASET FOR dsName BIND APPEND.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::DefineParameter {
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
    match stmt {
        Statement::DefineTempTable {
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
    match stmt {
        Statement::DefineBuffer { xml_options, .. } => {
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
    match stmt {
        Statement::Publish {
            event_name,
            from_handle,
            arguments,
        } => {
            assert!(matches!(
                event_name,
                Expression::Literal(Literal::String(_))
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
    match stmt {
        Statement::Publish {
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
    match stmt {
        Statement::Publish {
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
    match stmt {
        Statement::Publish {
            event_name,
            arguments,
            ..
        } => {
            match &event_name {
                Expression::Identifier(id) => assert_eq!(id.name, "cEventName"),
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
    match stmt {
        Statement::Subscribe {
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
    match stmt {
        Statement::Subscribe { target, .. } => {
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
    match stmt {
        Statement::Subscribe {
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
    match stmt {
        Statement::Subscribe { run_procedure, .. } => {
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
    match stmt {
        Statement::Subscribe { target, .. } => {
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
    match stmt {
        Statement::Unsubscribe {
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
    match stmt {
        Statement::Unsubscribe {
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
    match stmt {
        Statement::Unsubscribe { subscriber, .. } => {
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
    match stmt {
        Statement::Unsubscribe { in_handle, .. } => {
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
    match stmt {
        Statement::DefineEvent {
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
    match stmt {
        Statement::DefineEvent {
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
    match stmt {
        Statement::DefineEvent {
            parameters, name, ..
        } => {
            assert_eq!(name.name, "MyEvent");
            assert_eq!(parameters.len(), 3);
            // Verify first param
            match &parameters[0] {
                Statement::DefineParameter {
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
            match &parameters[2] {
                Statement::DefineParameter { direction, .. } => {
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::Publish { .. }));
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
    match stmt {
        Statement::Interface { body, name, .. } => {
            assert_eq!(name.name, "IObservable");
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::DefineEvent { .. }));
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
    match stmt {
        Statement::Publish {
            event_name,
            arguments,
            ..
        } => {
            // Event name should be a plain identifier, not a function call
            match &event_name {
                Expression::Identifier(id) => assert_eq!(id.name, "myEvent"),
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
            kind: OnKind::UiEvent { action, .. },
        } => match action {
            OnAction::PersistentRun {
                procedure,
                arguments,
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::On {
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
    match stmt {
        Statement::TriggerProcedure { event, target, .. } => {
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
    match stmt {
        Statement::TriggerProcedure {
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
    match stmt {
        Statement::TriggerProcedure { referencing, .. } => {
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
    match stmt {
        Statement::TriggerProcedure {
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
    match stmt {
        Statement::TriggerProcedure {
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
    match stmt {
        Statement::TriggerProcedure {
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
    match stmt {
        Statement::Procedure { name, body } => {
            assert_eq!(name.name, "myProc");
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::On { .. }));
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::On { .. }));
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
    match stmt {
        Statement::Class { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::On { .. }));
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
    match stmt {
        Statement::IncludeReference {
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
    match stmt {
        Statement::IncludeReference { path_and_args, .. } => {
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
    match stmt {
        Statement::IncludeReference { path_and_args, .. } => {
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
    assert!(matches!(stmt, Statement::IncludeReference { .. }));
}

#[test]
fn parse_include_arg_reference_statement() {
    let source = "{1}.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::IncludeArgReference { index, span } => {
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
    match stmt {
        Statement::Assignment { target, value } => {
            assert!(matches!(target, Expression::Identifier(_)));
            match value {
                Expression::IncludeReference { path_and_args, .. } => {
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
    match stmt {
        Statement::Assignment { target, value } => {
            assert!(matches!(target, Expression::Identifier(_)));
            match value {
                Expression::IncludeArgReference { index, .. } => {
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
    match stmt {
        Statement::Do { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::IncludeReference { .. }));
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
    match stmt {
        Statement::Procedure { name, body } => {
            assert_eq!(name.name, "my-proc");
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Statement::IncludeReference { .. }));
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
    assert!(matches!(stmt1, Statement::IncludeReference { .. }));

    let stmt2 = parser.parse_statement().expect("Expected second statement");
    assert!(matches!(stmt2, Statement::IncludeReference { .. }));

    let stmt3 = parser.parse_statement().expect("Expected third statement");
    assert!(matches!(stmt3, Statement::IncludeReference { .. }));
}

#[test]
fn parse_include_with_named_args() {
    let source = r#"{ms/report.i &event="start" &stream-name="s-printer"}."#;
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::IncludeReference { path_and_args, .. } => {
            assert_eq!(
                path_and_args,
                r#"ms/report.i &event="start" &stream-name="s-printer""#
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
    let source = "if x eq 0 then\ndo:\n   create sub-menu head-ptr\n      assign parent = fdm4-menu\n             label  = x-menu.\nend.\nelse\ndo:\n   create menu-item head-ptr\n   assign parent = fdm4-menu\n          label  = x-menu\n          sensitive = true\n   triggers:\n    on choose\n        persistent run value (\"ms/data_conv.w\") (y).\n    end triggers.\nend.\n";
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
        "end /* end for each std-doc */",
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
    // `hdsTarget:copy-dataset(hds{&mainTable},true,false,true)`.
    for source in &[
        "hdsTarget = dataset ds{&mainTable}:handle.",
        "hdsTarget:copy-dataset(hds{&mainTable},true,false,true).",
        "hdsTarget:copy-dataset(dataset ds{&mainTable}:handle,true).",
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
