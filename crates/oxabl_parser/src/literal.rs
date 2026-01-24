use oxabl_ast::{DecimalLiteral, IntegerLiteral, Literal, Span, StringLiteral, UnknownLiteral};
use oxabl_lexer::{Kind, Token, TokenValue};
use rust_decimal::Decimal;

// pub fn parse_literal(tokens: &mut TokenStream) -> Literal {
    
// }

pub fn token_to_literal<'a>(token: &'a Token, source: &str) -> Option<Literal<'a>> {
    match (&token.kind, &token.value) {
        (Kind::IntegerLiteral, TokenValue::Integer(v)) => Some(Literal::Integer(IntegerLiteral {
            span: Span { start: token.start as u32, end: token.end as u32 },
            value: *v as i64,
        })),
        (Kind::BigIntLiteral, TokenValue::BigInt(v)) => Some(Literal::Integer(IntegerLiteral { 
            span: Span { start: token.start as u32, end: token.end as u32}, 
            value: *v as i64,
        })),
        (Kind::DecimalLiteral, TokenValue::Decimal(v)) => Some(Literal::Decimal(DecimalLiteral {
            span: Span { start: token.start as u32, end: token.end as u32},
            value: *v as Decimal,
        })),
        (Kind::StringLiteral, TokenValue::String(v)) => Some(Literal::String(StringLiteral {
            span: Span { start: token.start as u32, end: token.end as u32},
            value: v as &'a str,
        })),
        (Kind::Question, TokenValue::None) => Some(Literal::Unknown(UnknownLiteral {
            span: Span { start: token.start as u32, end: token.end as u32},
        })),
        // TODO Booleans
        _ => None,
    }
}

// TODO add a round-trip test to ensure tokens are converted to literals correctly