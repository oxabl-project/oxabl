use crate::{NameId, Span};
use rust_decimal::Decimal;

/// An ABL literal value.
///
/// `Copy` because string and date literal text is interned in the AST's
/// [`NamePool`](crate::NamePool) and referenced via 4-byte [`NameId`] handles.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    Integer(IntegerLiteral),

    Decimal(DecimalLiteral),

    String(StringLiteral),

    Boolean(BooleanLiteral),

    /// A date constant (`02/01/2026`).
    Date(DateLiteral),

    /// The ABL unknown value literal (`?`).
    Unknown(UnknownLiteral),
}

// Lock the size of a literal deliberately. Gated to 64-bit; bump the bound
// deliberately.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Literal>() <= 32);

/// An integer literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub span: Span,

    pub value: i64,
}

/// A decimal literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DecimalLiteral {
    pub span: Span,

    pub value: Decimal,
}

/// A string literal value (single or double quoted), interned in [`NamePool`](crate::NamePool).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringLiteral {
    pub span: Span,

    pub value: NameId,
}

/// A boolean literal (`TRUE`/`FALSE`/`YES`/`NO`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub span: Span,

    pub value: bool, // True/False/Yes/No
}

/// A date constant: digit runs joined by `/` with no whitespace (`mm/dd/yy`,
/// `mm/dd/yyyy`, `mm/dd`). The order of the parts follows the `-d` startup
/// parameter, so the literal keeps its spelling interned in [`NamePool`](crate::NamePool).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DateLiteral {
    pub span: Span,

    pub text: NameId,
}

/// The ABL unknown value literal (`?`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnknownLiteral {
    pub span: Span, // Just ?
}
