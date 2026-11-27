//! Semantic-preservation guard (U7 / R6.3 / KTD4).
//!
//! Re-lex the candidate output and compare its **non-trivia** token stream
//! (comments and — since the lexer never emits them — whitespace excluded)
//! against the input's. The comparison is **kind-aware** so that every legal v1
//! transform is invisible while genuine drift trips:
//!
//! - **keywords / operators / punctuation** → compare `Kind` only. Recasing
//!   (`define`→`DEFINE`) and abbreviation expansion (`def`→`DEFINE`) keep the
//!   same `Kind` and carry no value, so they never trip.
//! - **literals** → compare `(Kind, TokenValue)`, with the text of a string or
//!   date literal read from each source, so a changed literal value trips.
//! - **identifiers and preprocessor/include refs** (no value to compare) →
//!   compare **source text byte-for-byte**. Without this, identifier corruption
//!   (`cust-num`→`custnum`, or one identifier swapped for another) would be
//!   invisible because both sides are `(Identifier, None)` (Fable finding 1).
//! - **`END` ⇔ `END <type>`** → a type keyword inserted immediately after
//!   `Kind::End` (the legal `end_with_type` transform) is not a mismatch (Fable
//!   finding 2), so a bare-`END` file under a strict preset does not bail.
//!
//! - **TAB columns in string literals** → the compiler expands a TAB inside a
//!   quoted literal to the next 8-column stop measured from the source line
//!   start, so the compiled value depends on the TAB's column. Each such TAB's
//!   column modulo 8 must be unchanged, or the guard trips.
//!
//! **Known blind spot (documented):** comment loss/corruption is invisible here
//! because comments are trivia. U2's no-loss/no-duplication invariant carries
//! that guarantee instead.

use oxabl_lexer::{Kind, Token, tokenize};

/// Compare two text payloads read from their sources; unreadable `None` equals
/// nothing so the guard fails closed.
fn texts_equal(a: Option<&str>, b: Option<&str>) -> bool {
    matches!((a, b), (Some(a), Some(b)) if a == b)
}

fn is_literal(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::DateLiteral
            | Kind::StringLiteral
    )
}

/// Value-less kinds whose *spelling* is semantic and therefore compared by
/// source text: identifiers and preprocessor/include references.
fn is_text_compared(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Identifier
            | Kind::IncludeReference
            | Kind::IncludeArgReference
            | Kind::PreprocElse
            | Kind::PreprocElseif
            | Kind::PreprocEndif
            | Kind::PreprocGlobalDefine
            | Kind::PreprocIf
            | Kind::PreprocMessage
            | Kind::PreprocScopedDefine
            | Kind::PreprocThen
            | Kind::PreprocUndefine
            | Kind::PreprocWebstream
            | Kind::Preprocess
    )
}

/// The block-type keywords `end_with_type` may insert after `END`.
fn is_end_type_keyword(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Procedure
            | Kind::Function
            | Kind::Class
            | Kind::Method
            | Kind::Constructor
            | Kind::Destructor
            | Kind::Interface
            | Kind::Case
    )
}

/// Column of every TAB byte inside a string literal, modulo 8 (the compiler's
/// tab stop), in source order. Two layouts expand every such literal to the same
/// value iff these sequences are equal.
fn literal_tab_columns(src: &str, toks: &[Token]) -> Vec<u8> {
    let bytes = src.as_bytes();
    let mut out = Vec::new();
    for t in toks.iter().filter(|t| t.kind == Kind::StringLiteral) {
        let start = t.start as usize;
        for (i, _) in bytes[t.range()]
            .iter()
            .enumerate()
            .filter(|(_, b)| **b == b'\t')
        {
            let at = start + i;
            let line_start = bytes[..at]
                .iter()
                .rposition(|b| *b == b'\n')
                .map_or(0, |p| p + 1);
            out.push(((at - line_start) % 8) as u8);
        }
    }
    out
}

/// Filter trivia from a pre-computed token slice.
fn stream_from(toks: &[Token]) -> Vec<Token> {
    toks.iter()
        .copied()
        .filter(|t| t.kind != Kind::Comment && t.kind != Kind::Eof)
        .collect()
}

fn tokens_equal(src_a: &str, a: Token, src_b: &str, b: Token) -> bool {
    if a.kind != b.kind {
        return false;
    }
    if is_literal(a.kind) {
        match a.kind {
            Kind::StringLiteral | Kind::DateLiteral => texts_equal(a.text(src_a), b.text(src_b)),
            Kind::IntegerLiteral | Kind::BigIntLiteral => {
                a.slice(src_a) == b.slice(src_b) || a.integer_value(src_a) == b.integer_value(src_b)
            }
            Kind::DecimalLiteral => {
                a.slice(src_a) == b.slice(src_b) || a.decimal_value(src_a) == b.decimal_value(src_b)
            }
            _ => true,
        }
    } else if is_text_compared(a.kind) {
        a.slice(src_a) == b.slice(src_b)
    } else {
        // Keyword / operator / punctuation: same kind is enough.
        true
    }
}

/// Return `true` iff `candidate`'s non-trivia token stream is semantically
/// identical to `input`'s under the KTD4 rules, lexing both sides.
///
/// `format()` uses [`preserves_with_input_tokens`] instead (it already holds the
/// input tokens); this two-sided variant backs the guard's own unit tests.
#[cfg(test)]
pub(crate) fn preserves(input: &str, candidate: &str) -> bool {
    preserves_with_input_tokens(input, &tokenize(input), candidate)
}

/// Like [`preserves`], but the input side reuses a token slice already computed
/// by the caller (the printer tokenizes `input` for its protected-line scan and
/// keyword transform, so re-lexing it here would be redundant). Only the
/// candidate is lexed fresh.
pub(crate) fn preserves_with_input_tokens(
    input: &str,
    input_tokens: &[Token],
    candidate: &str,
) -> bool {
    // The same text lexes to the same tokens, so it preserves itself: a file
    // that is already formatted is not lexed again.
    if candidate == input {
        return true;
    }
    let candidate_tokens = tokenize(candidate);
    compare(
        input,
        &stream_from(input_tokens),
        candidate,
        &stream_from(&candidate_tokens),
    ) && literal_tab_columns(input, input_tokens)
        == literal_tab_columns(candidate, &candidate_tokens)
}

/// Kind-aware comparison of two non-trivia token streams under the KTD4 rules.
fn compare(src_a: &str, a: &[Token], src_b: &str, b: &[Token]) -> bool {
    let (mut i, mut j) = (0, 0);
    let mut prev_end = false;
    while i < a.len() && j < b.len() {
        if tokens_equal(src_a, a[i], src_b, b[j]) {
            prev_end = a[i].kind == Kind::End;
            i += 1;
            j += 1;
            continue;
        }
        // `END` ⇔ `END <type>`: skip a type keyword the other side lacks, but
        // only immediately after a matched `END`.
        if prev_end && is_end_type_keyword(b[j].kind) {
            j += 1;
            continue;
        }
        if prev_end && is_end_type_keyword(a[i].kind) {
            i += 1;
            continue;
        }
        return false;
    }
    // Allow a trailing inserted/removed type keyword right after a final END.
    while j < b.len() && prev_end && is_end_type_keyword(b[j].kind) {
        j += 1;
    }
    while i < a.len() && prev_end && is_end_type_keyword(a[i].kind) {
        i += 1;
    }
    i == a.len() && j == b.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_unreadable_text_equals_nothing() {
        assert!(!texts_equal(None, None));
        assert!(texts_equal(Some("a"), Some("a")));
        assert!(!texts_equal(Some("a"), Some("b")));
        assert!(!texts_equal(Some("a"), None));
    }

    #[test]
    fn identical_streams_pass() {
        let s = "DEFINE VARIABLE x AS INTEGER.";
        assert!(preserves(s, s));
    }

    #[test]
    fn whitespace_and_comments_ignored() {
        let a = "DEFINE VARIABLE x AS INTEGER.";
        let b = "DEFINE   VARIABLE x AS INTEGER. /* added */";
        assert!(preserves(a, b), "whitespace + comments are trivia");
    }

    #[test]
    fn keyword_recasing_passes() {
        assert!(preserves(
            "define variable x as integer.",
            "DEFINE VARIABLE x AS INTEGER."
        ));
    }

    #[test]
    fn abbreviation_expansion_passes() {
        assert!(preserves(
            "def var x as int.",
            "DEFINE VARIABLE x AS INTEGER."
        ));
    }

    #[test]
    fn identifier_corruption_trips() {
        // Same kinds/values on both sides, only the identifier text differs.
        assert!(!preserves("MESSAGE cust-num.", "MESSAGE custnum."));
    }

    #[test]
    fn dropped_token_trips() {
        assert!(!preserves("x = 1 + 2.", "x = 1."));
    }

    #[test]
    fn literal_value_change_trips() {
        assert!(!preserves("x = 1.", "x = 2."));
    }

    #[test]
    fn string_literal_change_trips() {
        assert!(!preserves("MESSAGE \"abc\".", "MESSAGE \"abd\"."));
        assert!(!preserves("d = 1/2/2026.", "d = 1/3/2026."));
    }

    #[test]
    fn a_moved_string_literal_passes() {
        assert!(preserves("MESSAGE \"abc\".", "MESSAGE   \"abc\"."));
    }

    #[test]
    fn end_with_type_insertion_passes() {
        assert!(preserves(
            "PROCEDURE foo:\n  MESSAGE \"x\".\nEND.",
            "PROCEDURE foo:\n  MESSAGE \"x\".\nEND PROCEDURE."
        ));
    }

    #[test]
    fn shifting_a_tab_literal_off_its_tab_stop_trips() {
        assert!(!preserves("c = \"a\tb\".", "   c = \"a\tb\"."));
        assert!(!preserves("c = 'a\tb'.", "   c = 'a\tb'."));
    }

    #[test]
    fn shifting_a_tab_literal_by_a_tab_stop_passes() {
        assert!(preserves("c = \"a\tb\".", "        c = \"a\tb\"."));
    }

    #[test]
    fn spurious_token_insertion_trips() {
        assert!(!preserves("MESSAGE x.", "MESSAGE x y."));
    }
}
