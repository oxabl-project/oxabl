//! Coercion predicates for assignment and parameter-passing sites.
//!
//! v1 exports two predicates:
//! - [`assignable`] — lenient; used at assignment sites
//!   (`x = expr`, `ASSIGN x = expr`, variable initial values). Permits the
//!   widening ladder, silent numeric narrowing, and Unknown as universal
//!   bottom.
//! - [`assignable_strict`] — parameter sites (`OUTPUT` / `INPUT-OUTPUT`).
//!   Exact type match only. `Unknown` still acts as universal bottom so
//!   ABL's `?` can pass through any parameter site.
//!
//! See plan §Coercion catalog for the authoritative rule set.

use crate::types::{PrimitiveTy, ResolvedType};

/// Whether a value of type `from` may be assigned to a location of type
/// `to`. See module docs for details.
pub fn assignable(from: &ResolvedType, to: &ResolvedType) -> bool {
    use ResolvedType::*;
    // Unknown/Error are universal; they never produce a LINT0004 mismatch.
    if matches!(from, Unknown | Error) || matches!(to, Unknown | Error) {
        return true;
    }
    if from == to {
        return true;
    }
    match (from, to) {
        (Primitive(f), Primitive(t)) => primitive_assignable(*f, *t),
        (
            Array {
                element: fe,
                extent: fx,
            },
            Array {
                element: te,
                extent: tx,
            },
        ) => (fx == tx || fx.is_none() || tx.is_none()) && assignable(fe, te),
        // Class SymbolId identity — the walker upgrades INHERITS / IMPLEMENTS
        // chains via a pre-pass; cross-file parents stay Unknown.
        (Class(a), Class(b)) => a == b,
        (Buffer(a), Buffer(b)) => a == b,
        (Table(ra, ta), Table(rb, tb)) => ra == rb && ta == tb,
        _ => false,
    }
}

/// Strict assignment — parameter `OUTPUT` / `INPUT-OUTPUT` sites. Requires
/// exact primitive match (no widening, no narrowing). `Unknown` remains
/// universal.
pub fn assignable_strict(from: &ResolvedType, to: &ResolvedType) -> bool {
    use ResolvedType::*;
    if matches!(from, Unknown | Error) || matches!(to, Unknown | Error) {
        return true;
    }
    from == to
}

/// Whether the conversion `from` → `to` is a narrowing one that `LINT0004`
/// should emit as `Warning`. Integer-from-Decimal/Int64 truncation is
/// **silent** (idiomatic ABL; callers should not treat it as a warning).
/// Longchar→Character, Datetime→Date, and DatetimeTz→Date are warnings
/// because the narrowing discards user-visible information (bytes past
/// 32K or the time component).
pub fn is_narrowing_warning(from: &ResolvedType, to: &ResolvedType) -> bool {
    use PrimitiveTy::*;
    if let (ResolvedType::Primitive(f), ResolvedType::Primitive(t)) = (from, to) {
        matches!(
            (f, t),
            (Longchar, Character) | (Datetime, Date) | (DatetimeTz, Date)
        )
    } else {
        false
    }
}

/// Widen two primitives to a common type for arithmetic/comparison. Returns
/// `None` if the two primitives don't share a widening ladder.
pub fn widen_primitive(a: PrimitiveTy, b: PrimitiveTy) -> Option<PrimitiveTy> {
    use PrimitiveTy::*;
    if a == b {
        return Some(a);
    }
    match (a, b) {
        // Numeric ladder: Integer ⊂ Int64 ⊂ Decimal.
        (Integer, Int64) | (Int64, Integer) => Some(Int64),
        (Integer, Decimal) | (Decimal, Integer) => Some(Decimal),
        (Int64, Decimal) | (Decimal, Int64) => Some(Decimal),
        // Date ladder.
        (Date, Datetime) | (Datetime, Date) => Some(Datetime),
        (Date, DatetimeTz) | (DatetimeTz, Date) => Some(DatetimeTz),
        (Datetime, DatetimeTz) | (DatetimeTz, Datetime) => Some(DatetimeTz),
        // Character ladder.
        (Character, Longchar) | (Longchar, Character) => Some(Longchar),
        _ => None,
    }
}

fn primitive_assignable(from: PrimitiveTy, to: PrimitiveTy) -> bool {
    use PrimitiveTy::*;
    if from == to {
        return true;
    }
    match (from, to) {
        // Widening (silent).
        (Integer, Int64) | (Integer, Decimal) => true,
        (Int64, Decimal) => true,
        (Date, Datetime) | (Date, DatetimeTz) => true,
        (Datetime, DatetimeTz) => true,
        (Character, Longchar) => true,
        // Narrowing — assignable in v1, with LINT0004 severity decided by
        // `is_narrowing_warning` above.
        (Decimal, Integer) | (Decimal, Int64) => true,
        (Int64, Integer) => true,
        (Longchar, Character) => true,
        (Datetime, Date) | (DatetimeTz, Date) => true,
        // Strip explicit tz: DatetimeTz → Datetime assignable.
        (DatetimeTz, Datetime) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(t: PrimitiveTy) -> ResolvedType {
        ResolvedType::Primitive(t)
    }

    #[test]
    fn unknown_is_universal_bottom() {
        assert!(assignable(&ResolvedType::Unknown, &p(PrimitiveTy::Integer)));
        assert!(assignable(&p(PrimitiveTy::Integer), &ResolvedType::Unknown));
    }

    #[test]
    fn error_poisons_without_cascade() {
        assert!(assignable(&ResolvedType::Error, &p(PrimitiveTy::Integer)));
    }

    #[test]
    fn integer_widens_to_int64() {
        assert!(assignable(&p(PrimitiveTy::Integer), &p(PrimitiveTy::Int64)));
    }

    #[test]
    fn integer_widens_to_decimal() {
        assert!(assignable(
            &p(PrimitiveTy::Integer),
            &p(PrimitiveTy::Decimal)
        ));
    }

    #[test]
    fn decimal_narrows_to_integer_silently() {
        assert!(assignable(
            &p(PrimitiveTy::Decimal),
            &p(PrimitiveTy::Integer)
        ));
        assert!(!is_narrowing_warning(
            &p(PrimitiveTy::Decimal),
            &p(PrimitiveTy::Integer)
        ));
    }

    #[test]
    fn longchar_narrows_to_character_with_warning() {
        assert!(assignable(
            &p(PrimitiveTy::Longchar),
            &p(PrimitiveTy::Character)
        ));
        assert!(is_narrowing_warning(
            &p(PrimitiveTy::Longchar),
            &p(PrimitiveTy::Character)
        ));
    }

    #[test]
    fn datetime_narrows_to_date_with_warning() {
        assert!(assignable(&p(PrimitiveTy::Datetime), &p(PrimitiveTy::Date)));
        assert!(is_narrowing_warning(
            &p(PrimitiveTy::Datetime),
            &p(PrimitiveTy::Date)
        ));
    }

    #[test]
    fn logical_to_integer_is_not_assignable() {
        assert!(!assignable(
            &p(PrimitiveTy::Logical),
            &p(PrimitiveTy::Integer)
        ));
        assert!(!assignable(
            &p(PrimitiveTy::Integer),
            &p(PrimitiveTy::Logical)
        ));
    }

    #[test]
    fn strict_rejects_widening() {
        assert!(!assignable_strict(
            &p(PrimitiveTy::Integer),
            &p(PrimitiveTy::Int64)
        ));
    }

    #[test]
    fn strict_accepts_unknown() {
        assert!(assignable_strict(
            &ResolvedType::Unknown,
            &p(PrimitiveTy::Integer)
        ));
    }

    #[test]
    fn widen_same_type_is_identity() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Integer, PrimitiveTy::Integer),
            Some(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn widen_integer_int64_is_int64() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Integer, PrimitiveTy::Int64),
            Some(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn widen_integer_decimal_is_decimal() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Integer, PrimitiveTy::Decimal),
            Some(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn widen_incompatible_is_none() {
        assert_eq!(
            widen_primitive(PrimitiveTy::Logical, PrimitiveTy::Integer),
            None
        );
    }

    #[test]
    fn array_identity_is_assignable() {
        let arr = ResolvedType::Array {
            element: Box::new(p(PrimitiveTy::Integer)),
            extent: Some(5),
        };
        assert!(assignable(&arr, &arr));
    }

    #[test]
    fn array_dynamic_extent_is_assignable_to_fixed() {
        let fixed = ResolvedType::Array {
            element: Box::new(p(PrimitiveTy::Integer)),
            extent: Some(5),
        };
        let dynamic = ResolvedType::Array {
            element: Box::new(p(PrimitiveTy::Integer)),
            extent: None,
        };
        assert!(assignable(&dynamic, &fixed));
        assert!(assignable(&fixed, &dynamic));
    }
}
