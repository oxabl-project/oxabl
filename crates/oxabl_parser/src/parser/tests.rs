use super::*;
use oxabl_ast::{
    BooleanLiteral, DataType, DecimalLiteral, Expression, FindType, Identifier, IntegerLiteral,
    Literal, LockType, ParameterDirection, RunTarget, Span, Statement, StringLiteral,
    UnknownLiteral, WhenBranch,
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
            data_type: DataType::Integer,
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
            data_type: DataType::Integer,
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
            data_type,
            no_undo,
            initial_value,
            ..
        } => {
            assert_eq!(data_type, DataType::Character);
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
            data_type: DataType::Integer,
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
            data_type,
            initial_value,
            no_undo,
            ..
        } => {
            assert_eq!(name.name, "total");
            assert_eq!(data_type, DataType::Decimal);
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
            data_type,
            initial_value,
            ..
        } => {
            assert_eq!(data_type, DataType::Logical);
            assert!(matches!(
                initial_value,
                Some(Expression::Literal(Literal::Boolean(_)))
            ));
        }
        _ => panic!("Expected VariableDeclaration"),
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
    assert_eq!(stmt, Statement::Leave);
}

#[test]
fn parse_next_statement() {
    let source = "NEXT.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(stmt, Statement::Next);
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
        Statement::DefineParamter {
            direction,
            name,
            data_type,
            no_undo,
        } => {
            assert_eq!(direction, ParameterDirection::Input);
            assert_eq!(name.name, "name");
            assert_eq!(data_type, DataType::Character);
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
        Statement::DefineParamter {
            direction,
            name,
            data_type,
            no_undo,
        } => {
            assert_eq!(direction, ParameterDirection::Output);
            assert_eq!(name.name, "result");
            assert_eq!(data_type, DataType::Integer);
            assert!(!no_undo);
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
        Statement::DefineParamter {
            direction,
            name,
            data_type,
            no_undo,
        } => {
            assert_eq!(direction, ParameterDirection::InputOutput);
            assert_eq!(name.name, "data");
            assert_eq!(data_type, DataType::Logical);
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
        Statement::DefineParamter {
            direction,
            name,
            data_type,
            no_undo,
        } => {
            assert_eq!(direction, ParameterDirection::Input);
            assert_eq!(name.name, "name");
            assert_eq!(data_type, DataType::Character);
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
                Statement::DefineParamter {
                    direction,
                    name,
                    data_type,
                    ..
                } => {
                    assert_eq!(*direction, ParameterDirection::Input);
                    assert_eq!(name.name, "name");
                    assert_eq!(*data_type, DataType::Character);
                }
                _ => panic!("Expected DefineParameter"),
            }
            // Second statement should be output parameter
            match &body[1] {
                Statement::DefineParamter {
                    direction,
                    name,
                    data_type,
                    ..
                } => {
                    assert_eq!(*direction, ParameterDirection::Output);
                    assert_eq!(name.name, "result");
                    assert_eq!(*data_type, DataType::Integer);
                }
                _ => panic!("Expected DefineParameter"),
            }
        }
        _ => panic!("Expected Procedure statement"),
    }
}

// ==================== RUN Statement Tests ====================

#[test]
fn parse_run_simple_procedure() {
    let source = "RUN my-proc.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        Statement::Run {
            target: RunTarget::Literal("my-proc".to_string()),
            arguments: vec![],
        }
    );
}

#[test]
fn parse_run_dotted_procedure_name() {
    let source = "RUN my-proc.p.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        Statement::Run {
            target: RunTarget::Literal("my-proc.p".to_string()),
            arguments: vec![],
        }
    );
}

#[test]
fn parse_run_dynamic_value() {
    let source = "RUN VALUE(procName).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { target, arguments } => {
            assert!(matches!(target, RunTarget::Dynamic(_)));
            assert!(arguments.is_empty());
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_input_argument() {
    let source = "RUN my-proc (INPUT myVal).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { target, arguments } => {
            assert_eq!(target, RunTarget::Literal("my-proc".to_string()));
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_output_argument() {
    let source = "RUN my-proc (OUTPUT result).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { target, arguments } => {
            assert_eq!(target, RunTarget::Literal("my-proc".to_string()));
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Output);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_multiple_arguments() {
    let source = "RUN my-proc (INPUT x, OUTPUT y).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { arguments, .. } => {
            assert_eq!(arguments.len(), 2);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
            assert_eq!(arguments[1].direction, ParameterDirection::Output);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_input_output_argument() {
    let source = "RUN my-proc (INPUT-OUTPUT data).";
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
fn parse_run_with_default_input_direction() {
    // When no direction keyword, defaults to INPUT
    let source = "RUN my-proc (x).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { arguments, .. } => {
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_with_expression_argument() {
    let source = "RUN my-proc (INPUT x + 1).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { arguments, .. } => {
            assert_eq!(arguments.len(), 1);
            assert!(matches!(arguments[0].expression, Expression::Add(_, _)));
        }
        _ => panic!("Expected Run statement"),
    }
}

#[test]
fn parse_run_no_arguments() {
    let source = "RUN cleanup.";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    assert_eq!(
        stmt,
        Statement::Run {
            target: RunTarget::Literal("cleanup".to_string()),
            arguments: vec![],
        }
    );
}

#[test]
fn parse_run_dynamic_with_arguments() {
    let source = "RUN VALUE(procName) (INPUT 42).";
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let stmt = parser.parse_statement().expect("Expected a statement");
    match stmt {
        Statement::Run { target, arguments } => {
            assert!(matches!(target, RunTarget::Dynamic(_)));
            assert_eq!(arguments.len(), 1);
            assert_eq!(arguments[0].direction, ParameterDirection::Input);
        }
        _ => panic!("Expected Run statement"),
    }
}
