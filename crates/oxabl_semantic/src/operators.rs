//! Typing tables for ABL binary / unary operators.
//!
//! Baked into concrete functions so the check pass matches on the
//! [`ExpressionKind`] variant directly — no string lookups, no allocation.
//!
//! ABL quirks preserved (see plan §"Quirks baked into operators.rs"):
//! - `/` always returns `Decimal` regardless of operand types.
//! - `DATE + INTEGER = DATE` (days); `DATETIME + INTEGER = DATETIME` (ms).
//! - `Unknown` arithmetic propagates (not poison).

use oxabl_ast::ExpressionKind;

use crate::coercion::widen_primitive;
use crate::types::{PrimitiveTy, ResolvedType};

/// Result type of a binary operator applied to `(lhs, rhs)`.
///
/// Precedence for the three special cases:
/// 1. Either operand `Unknown` ⇒ result `Unknown` (lattice propagation).
/// 2. Either operand `Error` ⇒ result `Error` (suppress cascade).
/// 3. Otherwise, consult the operator-specific rule below.
pub fn binary_op_result(
    kind: &ExpressionKind,
    lhs: &ResolvedType,
    rhs: &ResolvedType,
) -> ResolvedType {
    // `?` propagates as Unknown, not Error.
    if matches!(lhs, ResolvedType::Unknown) || matches!(rhs, ResolvedType::Unknown) {
        return ResolvedType::Unknown;
    }
    if matches!(lhs, ResolvedType::Error) || matches!(rhs, ResolvedType::Error) {
        return ResolvedType::Error;
    }

    match kind {
        ExpressionKind::Add(..) => add_result(lhs, rhs),
        ExpressionKind::Minus(..) => subtract_result(lhs, rhs),
        ExpressionKind::Multiply(..) => numeric_widen(lhs, rhs),
        // ABL: `/` always returns DECIMAL.
        ExpressionKind::Divide(..) => {
            if is_numeric(lhs) && is_numeric(rhs) {
                ResolvedType::Primitive(PrimitiveTy::Decimal)
            } else {
                ResolvedType::Error
            }
        }
        ExpressionKind::Modulo(..) => modulo_result(lhs, rhs),
        ExpressionKind::Equal(..)
        | ExpressionKind::NotEqual(..)
        | ExpressionKind::LessThan(..)
        | ExpressionKind::LessThanOrEqual(..)
        | ExpressionKind::GreaterThan(..)
        | ExpressionKind::GreaterThanOrEqual(..) => {
            // Comparisons: if operands can be widened to a common type,
            // result is Logical; else Error.
            if comparable(lhs, rhs) {
                ResolvedType::Primitive(PrimitiveTy::Logical)
            } else {
                ResolvedType::Error
            }
        }
        ExpressionKind::And(..) | ExpressionKind::Or(..) => {
            if is_logical(lhs) && is_logical(rhs) {
                ResolvedType::Primitive(PrimitiveTy::Logical)
            } else {
                ResolvedType::Error
            }
        }
        ExpressionKind::Begins(..) | ExpressionKind::Matches(..) | ExpressionKind::Contains(..) => {
            if is_char(lhs) && is_char(rhs) {
                ResolvedType::Primitive(PrimitiveTy::Logical)
            } else {
                ResolvedType::Error
            }
        }
        // Not a binary operator — caller should never hit this branch.
        _ => ResolvedType::Error,
    }
}

/// Unary minus: numeric → same numeric. Other operands → Error.
pub fn unary_negate_result(operand: &ResolvedType) -> ResolvedType {
    match operand {
        ResolvedType::Unknown => ResolvedType::Unknown,
        ResolvedType::Error => ResolvedType::Error,
        ResolvedType::Primitive(
            PrimitiveTy::Integer | PrimitiveTy::Int64 | PrimitiveTy::Decimal,
        ) => operand.clone(),
        _ => ResolvedType::Error,
    }
}

/// Logical NOT: Logical → Logical. Other operands → Error.
pub fn unary_not_result(operand: &ResolvedType) -> ResolvedType {
    match operand {
        ResolvedType::Unknown => ResolvedType::Unknown,
        ResolvedType::Error => ResolvedType::Error,
        ResolvedType::Primitive(PrimitiveTy::Logical) => operand.clone(),
        _ => ResolvedType::Error,
    }
}

fn add_result(lhs: &ResolvedType, rhs: &ResolvedType) -> ResolvedType {
    use PrimitiveTy::*;
    if let (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) = (lhs, rhs) {
        match (a, b) {
            // Date / Datetime + integer types preserve the date-like type.
            (Date, Integer) | (Integer, Date) | (Date, Int64) | (Int64, Date) => {
                return ResolvedType::Primitive(Date);
            }
            (Datetime, Integer) | (Integer, Datetime) | (Datetime, Int64) | (Int64, Datetime) => {
                return ResolvedType::Primitive(Datetime);
            }
            (DatetimeTz, Integer)
            | (Integer, DatetimeTz)
            | (DatetimeTz, Int64)
            | (Int64, DatetimeTz) => {
                return ResolvedType::Primitive(DatetimeTz);
            }
            // Character concatenation (`+` is string concat in ABL).
            (Character, Character) => return ResolvedType::Primitive(Character),
            (Longchar, Longchar) | (Character, Longchar) | (Longchar, Character) => {
                return ResolvedType::Primitive(Longchar);
            }
            _ => {}
        }
    }
    numeric_widen(lhs, rhs)
}

fn subtract_result(lhs: &ResolvedType, rhs: &ResolvedType) -> ResolvedType {
    use PrimitiveTy::*;
    if let (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) = (lhs, rhs) {
        match (a, b) {
            // Date - Date = days (Integer).
            (Date, Date) => return ResolvedType::Primitive(Integer),
            // Datetime - Datetime = milliseconds (Int64).
            (Datetime, Datetime)
            | (DatetimeTz, DatetimeTz)
            | (Datetime, DatetimeTz)
            | (DatetimeTz, Datetime) => {
                return ResolvedType::Primitive(Int64);
            }
            (Date, Integer) | (Date, Int64) => return ResolvedType::Primitive(Date),
            (Datetime, Integer) | (Datetime, Int64) => {
                return ResolvedType::Primitive(Datetime);
            }
            (DatetimeTz, Integer) | (DatetimeTz, Int64) => {
                return ResolvedType::Primitive(DatetimeTz);
            }
            _ => {}
        }
    }
    numeric_widen(lhs, rhs)
}

fn modulo_result(lhs: &ResolvedType, rhs: &ResolvedType) -> ResolvedType {
    use PrimitiveTy::*;
    if let (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) = (lhs, rhs) {
        match (a, b) {
            (Integer, Integer) => return ResolvedType::Primitive(Integer),
            (Int64, Int64) | (Integer, Int64) | (Int64, Integer) => {
                return ResolvedType::Primitive(Int64);
            }
            _ => {}
        }
    }
    ResolvedType::Error
}

fn numeric_widen(lhs: &ResolvedType, rhs: &ResolvedType) -> ResolvedType {
    if let (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) = (lhs, rhs)
        && let Some(widened) = widen_primitive(*a, *b)
        && matches!(
            widened,
            PrimitiveTy::Integer | PrimitiveTy::Int64 | PrimitiveTy::Decimal
        )
    {
        return ResolvedType::Primitive(widened);
    }
    ResolvedType::Error
}

fn is_numeric(t: &ResolvedType) -> bool {
    matches!(
        t,
        ResolvedType::Primitive(PrimitiveTy::Integer)
            | ResolvedType::Primitive(PrimitiveTy::Int64)
            | ResolvedType::Primitive(PrimitiveTy::Decimal)
    )
}

fn is_logical(t: &ResolvedType) -> bool {
    matches!(t, ResolvedType::Primitive(PrimitiveTy::Logical))
}

fn is_char(t: &ResolvedType) -> bool {
    matches!(
        t,
        ResolvedType::Primitive(PrimitiveTy::Character)
            | ResolvedType::Primitive(PrimitiveTy::Longchar)
    )
}

fn comparable(lhs: &ResolvedType, rhs: &ResolvedType) -> bool {
    if let (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) = (lhs, rhs) {
        a == b || widen_primitive(*a, *b).is_some()
    } else {
        lhs == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{Expression, ExpressionKind, Literal};

    fn p(t: PrimitiveTy) -> ResolvedType {
        ResolvedType::Primitive(t)
    }

    // Dummy sub-expressions carry no semantic weight; only the operator
    // variant is inspected. Use a cheap literal placeholder.
    fn dummy_expr() -> Box<Expression> {
        use oxabl_ast::{Span, UnknownLiteral};
        Box::new(Expression::new(ExpressionKind::Literal(Literal::Unknown(
            UnknownLiteral {
                span: Span { start: 0, end: 0 },
            },
        ))))
    }

    fn add_kind() -> ExpressionKind {
        ExpressionKind::Add(dummy_expr(), dummy_expr())
    }
    fn sub_kind() -> ExpressionKind {
        ExpressionKind::Minus(dummy_expr(), dummy_expr())
    }
    fn div_kind() -> ExpressionKind {
        ExpressionKind::Divide(dummy_expr(), dummy_expr())
    }
    fn mul_kind() -> ExpressionKind {
        ExpressionKind::Multiply(dummy_expr(), dummy_expr())
    }
    fn mod_kind() -> ExpressionKind {
        ExpressionKind::Modulo(dummy_expr(), dummy_expr())
    }
    fn eq_kind() -> ExpressionKind {
        ExpressionKind::Equal(dummy_expr(), dummy_expr())
    }
    fn and_kind() -> ExpressionKind {
        ExpressionKind::And(dummy_expr(), dummy_expr())
    }
    fn begins_kind() -> ExpressionKind {
        ExpressionKind::Begins(dummy_expr(), dummy_expr())
    }

    #[test]
    fn integer_plus_integer_is_integer() {
        assert_eq!(
            binary_op_result(
                &add_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Integer)
            ),
            p(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn integer_plus_decimal_is_decimal() {
        assert_eq!(
            binary_op_result(
                &add_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Decimal)
            ),
            p(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn integer_div_integer_is_decimal_abl_quirk() {
        // ABL: integer / integer always returns DECIMAL.
        assert_eq!(
            binary_op_result(
                &div_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Integer)
            ),
            p(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn int64_div_int64_is_decimal() {
        assert_eq!(
            binary_op_result(&div_kind(), &p(PrimitiveTy::Int64), &p(PrimitiveTy::Int64)),
            p(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn date_plus_integer_is_date_not_int() {
        assert_eq!(
            binary_op_result(&add_kind(), &p(PrimitiveTy::Date), &p(PrimitiveTy::Integer)),
            p(PrimitiveTy::Date)
        );
    }

    #[test]
    fn datetime_plus_integer_is_datetime() {
        assert_eq!(
            binary_op_result(
                &add_kind(),
                &p(PrimitiveTy::Datetime),
                &p(PrimitiveTy::Integer)
            ),
            p(PrimitiveTy::Datetime)
        );
    }

    #[test]
    fn date_minus_date_is_integer_days() {
        assert_eq!(
            binary_op_result(&sub_kind(), &p(PrimitiveTy::Date), &p(PrimitiveTy::Date)),
            p(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn datetime_minus_datetime_is_int64_ms() {
        assert_eq!(
            binary_op_result(
                &sub_kind(),
                &p(PrimitiveTy::Datetime),
                &p(PrimitiveTy::Datetime)
            ),
            p(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn character_plus_character_is_character() {
        assert_eq!(
            binary_op_result(
                &add_kind(),
                &p(PrimitiveTy::Character),
                &p(PrimitiveTy::Character)
            ),
            p(PrimitiveTy::Character)
        );
    }

    #[test]
    fn unknown_propagates_not_error() {
        assert_eq!(
            binary_op_result(
                &add_kind(),
                &ResolvedType::Unknown,
                &p(PrimitiveTy::Integer)
            ),
            ResolvedType::Unknown
        );
    }

    #[test]
    fn error_poisons_without_cascade() {
        assert_eq!(
            binary_op_result(&add_kind(), &ResolvedType::Error, &p(PrimitiveTy::Integer)),
            ResolvedType::Error
        );
    }

    #[test]
    fn modulo_integer_integer_is_integer() {
        assert_eq!(
            binary_op_result(
                &mod_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Integer)
            ),
            p(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn multiply_integer_int64_is_int64() {
        assert_eq!(
            binary_op_result(
                &mul_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Int64)
            ),
            p(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn comparison_returns_logical() {
        assert_eq!(
            binary_op_result(
                &eq_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Integer)
            ),
            p(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn comparison_incompatible_is_error() {
        assert_eq!(
            binary_op_result(
                &eq_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Logical)
            ),
            ResolvedType::Error
        );
    }

    #[test]
    fn and_logical_logical_is_logical() {
        assert_eq!(
            binary_op_result(
                &and_kind(),
                &p(PrimitiveTy::Logical),
                &p(PrimitiveTy::Logical)
            ),
            p(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn and_non_logical_is_error() {
        assert_eq!(
            binary_op_result(
                &and_kind(),
                &p(PrimitiveTy::Integer),
                &p(PrimitiveTy::Integer)
            ),
            ResolvedType::Error
        );
    }

    #[test]
    fn begins_char_char_is_logical() {
        assert_eq!(
            binary_op_result(
                &begins_kind(),
                &p(PrimitiveTy::Character),
                &p(PrimitiveTy::Character)
            ),
            p(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn unary_negate_integer_is_integer() {
        assert_eq!(
            unary_negate_result(&p(PrimitiveTy::Integer)),
            p(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn unary_negate_logical_is_error() {
        assert_eq!(
            unary_negate_result(&p(PrimitiveTy::Logical)),
            ResolvedType::Error
        );
    }

    #[test]
    fn unary_not_logical_is_logical() {
        assert_eq!(
            unary_not_result(&p(PrimitiveTy::Logical)),
            p(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn unary_not_integer_is_error() {
        assert_eq!(
            unary_not_result(&p(PrimitiveTy::Integer)),
            ResolvedType::Error
        );
    }
}
