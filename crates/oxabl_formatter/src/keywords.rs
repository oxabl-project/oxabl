//! Keyword recasing + abbreviation transform (U5 / R4.2 / R6.4).
//!
//! Applied to keyword tokens **only**, and **only** when the [`StyleGuide`] opts
//! in. Under `default_base()` (`Preserve` + `KeepAbbreviations`) this is a
//! no-op, so the author's keyword spelling survives a safe-default format
//! (R6.4). Identifiers, literals, comments, and preprocessor/include refs are
//! never touched — the last of these matters because the semantic guard (KTD4)
//! compares them by source text, so recasing one would trip the guard.
//!
//! The transform is driven by a **whole-source** tokenization (in the printer),
//! not per-line, so multi-line block comments stay a single comment token and
//! their interior text is never mistaken for keywords.

use oxabl_lexer::Kind;
use oxabl_style::{KeywordAbbreviation, KeywordCase, StyleGuide};

use crate::keyword_spelling::canonical_keyword;

/// Whether the resolved style asks for any keyword transform at all. When
/// false, the printer skips the whole-source tokenization.
pub(crate) fn wants_transform(style: &StyleGuide) -> bool {
    style.keyword_case != KeywordCase::Preserve
        || style.keyword_abbreviation == KeywordAbbreviation::AbbreviateNothing
}

/// Whether a token of this kind is a recasable/expandable keyword.
///
/// Identifiers, comments, literals, the EOF/invalid/`&DEFINE`-end sentinels,
/// include refs, and every preprocessor directive are excluded: their spelling
/// is either author-owned (identifiers) or guard-compared by source text
/// (preprocessor/include refs), so the formatter must leave them byte-exact.
pub(crate) fn is_transformable(kind: Kind) -> bool {
    !matches!(
        kind,
        Kind::Identifier
            | Kind::Comment
            | Kind::Eof
            | Kind::Invalid
            | Kind::PreprocEnd
            | Kind::IntegerLiteral
            | Kind::BigIntLiteral
            | Kind::DecimalLiteral
            | Kind::StringLiteral
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

/// Transform one keyword token's source text per the style.
///
/// Abbreviation expansion (`AbbreviateNothing`) replaces the token with its
/// canonical single-word full form when one exists; recasing then applies. Any
/// symbolic operator (no letters) is unaffected. `AbbreviateEverything` is not
/// shortened in v1 (treated as `KeepAbbreviations`).
pub(crate) fn transform_token(raw: &str, kind: Kind, style: &StyleGuide) -> String {
    let mut s = raw.to_string();
    if style.keyword_abbreviation == KeywordAbbreviation::AbbreviateNothing
        && let Some(full) = canonical_keyword(kind)
    {
        s = full.to_string();
    }
    match style.keyword_case {
        KeywordCase::Uppercase => s.to_ascii_uppercase(),
        KeywordCase::Lowercase => s.to_ascii_lowercase(),
        KeywordCase::Preserve => s,
    }
}
