use oxabl_lexer::oxabl_atom::OxablAtom;

/// Maximum length that case-folds on the stack. Matches the lexer's keyword
/// buffer (`match_keyword` in `oxabl_lexer/src/kind.rs`) — `.df` identifiers
/// and keywords never exceed this, and fold_atom falls back to a heap
/// allocation only for the rare pathological case.
const INLINE_BUF: usize = 64;

/// Case-fold `s` (ASCII) and intern it as an `OxablAtom`.
///
/// Uses a stack buffer for the common case so the hot path (every table and
/// field name in a `.df` load) does no heap allocation beyond whatever
/// `string_cache` does internally for a previously-unseen atom.
#[inline]
pub fn fold_atom(s: &str) -> OxablAtom {
    let bytes = s.as_bytes();
    if bytes.len() <= INLINE_BUF {
        let mut buf = [0u8; INLINE_BUF];
        for (i, &b) in bytes.iter().enumerate() {
            buf[i] = b.to_ascii_lowercase();
        }
        // SAFETY: ASCII case folding preserves UTF-8 validity, and `s` was
        // already valid UTF-8.
        let lower = unsafe { std::str::from_utf8_unchecked(&buf[..bytes.len()]) };
        OxablAtom::from(lower)
    } else {
        OxablAtom::from(s.to_ascii_lowercase())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn different_casings_fold_to_same_atom() {
        assert_eq!(fold_atom("Customer"), fold_atom("customer"));
        assert_eq!(fold_atom("CUSTOMER"), fold_atom("cUsToMeR"));
    }

    #[test]
    fn long_identifier_falls_back_to_heap() {
        let long = "A".repeat(200);
        let a = fold_atom(&long);
        let b = fold_atom(&long.to_lowercase());
        assert_eq!(a, b);
    }

    #[test]
    fn distinct_names_do_not_collide() {
        assert_ne!(fold_atom("Customer"), fold_atom("CustNum"));
    }
}
