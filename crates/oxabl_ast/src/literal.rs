use crate::Span;
use rust_decimal::Decimal;

pub enum Literal<'a> {
    Integer(IntegerLiteral),

    Decimal(DecimalLiteral),

    String(StringLiteral<'a>),

    Boolean(BooleanLiteral),

    Unknown(UnknownLiteral), // The ? literal in ABL
}

pub struct IntegerLiteral {
    pub span: Span,

    pub value: i64,
}

pub struct DecimalLiteral {
    pub span: Span,

    pub value: Decimal,
}

pub struct StringLiteral<'a> {
    pub span: Span,

    pub value: &'a str,
}

pub struct BooleanLiteral {
    pub span: Span,

    pub value: bool, // True/False/Yes/No
}

pub struct UnknownLiteral {
    pub span: Span, // Just ?
}
