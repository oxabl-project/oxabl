use crate::Span;
use rust_decimal::Decimal;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'a> {
    Integer(IntegerLiteral),

    Decimal(DecimalLiteral),

    String(StringLiteral<'a>),

    Boolean(BooleanLiteral),

    Unknown(UnknownLiteral), // The ? literal in ABL
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub span: Span,

    pub value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecimalLiteral {
    pub span: Span,

    pub value: Decimal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral<'a> {
    pub span: Span,

    pub value: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub span: Span,

    pub value: bool, // True/False/Yes/No
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownLiteral {
    pub span: Span, // Just ?
}
