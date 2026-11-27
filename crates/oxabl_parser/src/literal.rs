use crate::parser::tok_span;
use oxabl_ast::{
    BooleanLiteral, DateLiteral, DecimalLiteral, IntegerLiteral, Literal, NameInterner,
    StringLiteral, UnknownLiteral,
};
use oxabl_lexer::{Kind, Token, TokenValue};

/// The literal of `token`, read from `source`, interning string/date text into `names`.
pub fn token_to_literal(token: &Token, source: &str, names: &mut NameInterner) -> Option<Literal> {
    let span = tok_span(token);
    match (&token.kind, &token.value) {
        (Kind::IntegerLiteral, TokenValue::Integer(v)) => Some(Literal::Integer(IntegerLiteral {
            span,
            value: i64::from(*v),
        })),
        (Kind::BigIntLiteral, TokenValue::BigInt(v)) => {
            Some(Literal::Integer(IntegerLiteral { span, value: *v }))
        }
        (Kind::DecimalLiteral, TokenValue::Decimal(v)) => {
            Some(Literal::Decimal(DecimalLiteral { span, value: *v }))
        }
        (Kind::StringLiteral, TokenValue::Text { .. }) => {
            let value = names.intern(token.text(source)?);
            Some(Literal::String(StringLiteral { span, value }))
        }
        (Kind::DateLiteral, TokenValue::Text { .. }) => {
            let text = names.intern(token.text(source)?);
            Some(Literal::Date(DateLiteral { span, text }))
        }
        (Kind::Question, TokenValue::None) => Some(Literal::Unknown(UnknownLiteral { span })),
        (Kind::KwTrue | Kind::KwFalse, TokenValue::Boolean(v)) => {
            Some(Literal::Boolean(BooleanLiteral { span, value: *v }))
        }
        // YES is an alias for TRUE in ABL
        (Kind::Yes, _) => Some(Literal::Boolean(BooleanLiteral { span, value: true })),
        // NO is an alias for FALSE in ABL. Bare `no` only reaches the parser
        // when the lexer's space-separated lock lookahead did not claim a
        // following LOCK, so this never shadows `NO LOCK`.
        (Kind::No, _) => Some(Literal::Boolean(BooleanLiteral { span, value: false })),
        _ => None,
    }
}

// TODO add a round-trip test to ensure tokens are converted to literals correctly
#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::Span;
    use oxabl_lexer::tokenize;
    use rust_decimal::Decimal;
    use std::str::FromStr;

    fn assert_integer(
        literal: Literal,
        expected_value: i64,
        expected_start: u32,
        expected_end: u32,
    ) {
        assert_eq!(
            literal,
            Literal::Integer(IntegerLiteral {
                span: Span {
                    start: expected_start,
                    end: expected_end
                },
                value: expected_value
            })
        );
    }

    fn assert_decimal(
        literal: Literal,
        expected_value: Decimal,
        expected_start: u32,
        expected_end: u32,
    ) {
        assert_eq!(
            literal,
            Literal::Decimal(DecimalLiteral {
                span: Span {
                    start: expected_start,
                    end: expected_end
                },
                value: expected_value
            })
        );
    }

    fn assert_boolean(
        literal: Literal,
        expected_value: bool,
        expected_start: u32,
        expected_end: u32,
    ) {
        assert_eq!(
            literal,
            Literal::Boolean(BooleanLiteral {
                span: Span {
                    start: expected_start,
                    end: expected_end
                },
                value: expected_value
            })
        );
    }

    fn assert_unknown(literal: Literal, expected_start: u32, expected_end: u32) {
        assert_eq!(
            literal,
            Literal::Unknown(UnknownLiteral {
                span: Span {
                    start: expected_start,
                    end: expected_end
                },
            })
        );
    }

    #[test]
    fn integer_literal_from_token() {
        let source = "def var myInt as int no-undo init 4.";
        let tokens = tokenize(source);
        // get the integer literal token
        let token = tokens
            .iter()
            .find(|t| t.kind == Kind::IntegerLiteral)
            .expect("expected an integer literal token");
        // convert to integer literal
        let mut names = NameInterner::new();
        let lit =
            token_to_literal(token, source, &mut names).expect("expected an integer literal span");
        assert_integer(lit, 4, 34, 35);
    }

    #[test]
    fn decimal_literal_from_token() {
        let source = "def var myDec as dec no-undo init 3.14.";
        let tokens = tokenize(source);
        // get the decimal literal token
        let token = tokens
            .iter()
            .find(|t| t.kind == Kind::DecimalLiteral)
            .expect("expected a decimal literal token");
        // convert to decimal literal
        let mut names = NameInterner::new();
        let lit =
            token_to_literal(token, source, &mut names).expect("expected a decimal literal span");
        assert_decimal(lit, Decimal::from_str("3.14").unwrap(), 34, 38);
    }

    #[test]
    fn string_literal_from_token() {
        let source = "def var myStr as char no-undo init 'hello'.";
        let tokens = tokenize(source);
        // get the string literal token
        let token = tokens
            .iter()
            .find(|t| t.kind == Kind::StringLiteral)
            .expect("expected a string literal token");
        // convert to string literal
        let mut names = NameInterner::new();
        let lit =
            token_to_literal(token, source, &mut names).expect("expected a string literal span");
        let Literal::String(s) = lit else {
            panic!("expected String literal, got {lit:?}");
        };
        assert_eq!(s.span, Span { start: 35, end: 42 });
        assert_eq!(names.get(s.value), "hello");
    }

    #[test]
    fn boolean_literal_from_token() {
        let source = "def var myBool as log no-undo init true.";
        let tokens = tokenize(source);
        // get the boolean literal token
        let token = tokens
            .iter()
            .find(|t| t.kind == Kind::KwTrue)
            .expect("expected a boolean literal token");
        // convert to boolean literal
        let mut names = NameInterner::new();
        let lit =
            token_to_literal(token, source, &mut names).expect("expected a boolean literal span");
        assert_boolean(lit, true, 35, 39);
    }

    #[test]
    fn unknown_literal_from_token() {
        let source = "def var myUnk as int no-undo init ?.";
        let tokens = tokenize(source);
        // get the unknown literal token
        let token = tokens
            .iter()
            .find(|t| t.kind == Kind::Question)
            .expect("expected an unknown literal token");
        // convert to unknown literal
        let mut names = NameInterner::new();
        let lit =
            token_to_literal(token, source, &mut names).expect("expected an unknown literal span");
        assert_unknown(lit, 34, 35);
    }
}
