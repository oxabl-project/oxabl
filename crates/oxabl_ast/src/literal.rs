use crate::Span;
use rust_decimal::Decimal;

/// An ABL literal value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(IntegerLiteral),

    Decimal(DecimalLiteral),

    String(StringLiteral),

    Boolean(BooleanLiteral),

    /// The ABL unknown value literal (`?`).
    Unknown(UnknownLiteral),
}

/// An integer literal value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub span: Span,

    pub value: i64,
}

/// A decimal literal value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecimalLiteral {
    pub span: Span,

    pub value: Decimal,
}

/// A string literal value (single or double quoted).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    pub span: Span,

    pub value: String,
}

/// A boolean literal (`TRUE`/`FALSE`/`YES`/`NO`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub span: Span,

    pub value: bool, // True/False/Yes/No
}

/// The ABL unknown value literal (`?`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownLiteral {
    pub span: Span, // Just ?
}
