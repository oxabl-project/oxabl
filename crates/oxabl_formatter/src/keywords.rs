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

use oxabl_lexer::{Kind, Token};
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

/// Keywords after which a class name or a path follows (`CLASS`, `NEW`,
/// `INHERITS`, `IMPLEMENTS`, `USING`, `RUN`, `AS`).
fn introduces_path(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Class
            | Kind::Interface
            | Kind::KwEnum
            | Kind::New
            | Kind::Inherits
            | Kind::Implements
            | Kind::Using
            | Kind::Run
            | Kind::KwAs
    )
}

/// Keywords after which a bare class name follows. A name in a `RUN` or `AS`
/// position stays a keyword (`RUN fill`, `AS PRIMARY`); only a dotted path is
/// a name there.
fn introduces_class(kind: Kind) -> bool {
    introduces_path(kind) && !matches!(kind, Kind::Run | Kind::KwAs)
}

/// Keywords that can follow a name-introducing keyword and are still keywords:
/// built-in types, modifiers and the table/buffer/dataset forms.
fn is_structural(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Buffer
            | Kind::TempTable
            | Kind::Table
            | Kind::TableHandle
            | Kind::Dataset
            | Kind::DatasetHandle
            | Kind::Class
            | Kind::Handle
            | Kind::Character
            | Kind::Integer
            | Kind::Int64
            | Kind::Decimal
            | Kind::Logical
            | Kind::Date
            | Kind::Datetime
            | Kind::DatetimeTz
            | Kind::Recid
            | Kind::Rowid
            | Kind::Raw
            | Kind::Memptr
            | Kind::Longchar
            | Kind::Clob
            | Kind::Blob
            | Kind::ComHandle
            | Kind::Void
            | Kind::Shared
            | Kind::Global
            | Kind::Value
            | Kind::NoUndo
            | Kind::Public
            | Kind::Private
            | Kind::Protected
            | Kind::KwStatic
            | Kind::KwAs
            | Kind::KwIn
            | Kind::KwFor
            | Kind::With
            | Kind::Like
            | Kind::KwWhere
            | Kind::Of
            | Kind::Abstract
            | Kind::Override
            | Kind::Final
    )
}

/// Words that lexed as identifiers before they became unreserved keywords. Each
/// is a legal class, table or variable name, so it is recased only where it is a
/// real keyword (see [`is_keyword_position`]).
fn was_identifier(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Object
            | Kind::Widget
            | Kind::XDocument
            | Kind::XNoderef
            | Kind::SaxReader
            | Kind::SaxWriter
            | Kind::SaxAttributes
            | Kind::Socket
            | Kind::ServerSocket
            | Kind::Server
            | Kind::SoapHeader
            | Kind::SoapHeaderEntryref
            | Kind::ClientPrincipal
            | Kind::Button
            | Kind::ComboBox
            | Kind::ControlFrame
            | Kind::DialogBox
            | Kind::Editor
            | Kind::FillIn
            | Kind::Image
            | Kind::Menu
            | Kind::MenuItem
            | Kind::RadioSet
            | Kind::SelectionList
            | Kind::Slider
            | Kind::SubMenu
            | Kind::ToggleBox
            | Kind::File
            | Kind::Total
            | Kind::Count
            | Kind::Average
            | Kind::Maximum
            | Kind::Minimum
            | Kind::SubTotal
            | Kind::SubCount
            | Kind::SubAverage
            | Kind::SubMaximum
            | Kind::SubMinimum
            | Kind::Printer
            | Kind::Row
            | Kind::Column
            | Kind::Bgcolor
            | Kind::Fgcolor
            | Kind::NoTabStop
            | Kind::SizeChars
            | Kind::SizePixels
    )
}

/// Operators spelled as words, which can follow a variable that is named like a
/// keyword.
fn is_operator_word(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Eq
            | Kind::Ne
            | Kind::Lt
            | Kind::Le
            | Kind::Gt
            | Kind::Ge
            | Kind::And
            | Kind::Or
            | Kind::Not
            | Kind::Matches
            | Kind::Begins
            | Kind::Contains
            | Kind::Modulo
    )
}

/// Whether the [`was_identifier`] token at `sig[p]` is a keyword: the operand of
/// `CREATE`, `DELETE`, `RELEASE` or `VIEW-AS`, or the widget type of a
/// `DEFINE [NEW] [SHARED] ...`. After `CREATE`/`DELETE`/`RELEASE` a word that ends
/// the statement is a table name.
fn is_keyword_position(toks: &[Token], sig: &[usize], p: usize) -> bool {
    let mut q = p;
    while q > 0 {
        q -= 1;
        match toks[sig[q]].kind {
            Kind::New | Kind::Shared | Kind::Global => continue,
            Kind::Define => return true,
            Kind::ViewAs => return q + 1 == p,
            Kind::Create | Kind::Delete | Kind::Release => {
                return q + 1 == p
                    && sig
                        .get(p + 1)
                        .is_some_and(|&n| toks[n].kind != Kind::Period);
            }
            _ => return false,
        }
    }
    false
}

/// Flags (by index into `toks`) the keyword tokens that are names, not keywords.
///
/// Every keyword is recased, except where the case of a word matters or the word
/// is a legal name:
/// - a word that lexed as an identifier before it became an unreserved keyword
///   (`image`, `server`, ...) stays as written unless it is a keyword use, see
///   [`is_keyword_position`];
/// - a class name after `CLASS`, `NEW`, `INHERITS`, `IMPLEMENTS` (every entry) or
///   `USING`, and a dotted path after one of these, `RUN` or `AS`, names a file.
pub(crate) fn name_uses(source: &str, toks: &[Token]) -> Vec<bool> {
    let sig: Vec<usize> = (0..toks.len())
        .filter(|&i| !matches!(toks[i].kind, Kind::Comment | Kind::Eof))
        .collect();
    let word_like = |i: usize| {
        source[toks[i].start..toks[i].end]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    };
    let touches = |a: usize, b: usize| toks[a].end == toks[b].start;
    let in_list = type_list_entries(toks, &sig, &word_like, &touches);

    let mut is_name = vec![false; toks.len()];
    for (p, &i) in sig.iter().enumerate() {
        if !word_like(i) {
            continue;
        }
        if was_identifier(toks[i].kind) {
            is_name[i] = !is_keyword_position(toks, &sig, p);
            continue;
        }
        let in_chain = p >= 2
            && toks[sig[p - 1]].kind == Kind::Period
            && touches(sig[p - 2], sig[p - 1])
            && touches(sig[p - 1], i);
        if in_chain {
            is_name[i] = is_name[sig[p - 2]];
            continue;
        }
        let Some(q) = p.checked_sub(1).map(|q| sig[q]) else {
            continue;
        };
        // The first word of a dotted chain decides for the whole chain.
        let dotted = matches!((sig.get(p + 1), sig.get(p + 2)), (Some(&n), Some(&a))
            if toks[n].kind == Kind::Period
                && touches(i, n)
                && touches(n, a)
                && word_like(a));
        let in_list = toks[q].kind == Kind::Comma && in_list[p];
        // `tt.CLASS EQ "x"`: a field named like the introducer introduces nothing.
        let member = p >= 2
            && matches!(toks[q].kind, Kind::Class | Kind::Interface)
            && matches!(toks[sig[p - 2]].kind, Kind::Period | Kind::Colon)
            && touches(sig[p - 2], q);
        is_name[i] = if dotted {
            introduces_path(toks[q].kind) || in_list
        } else {
            (introduces_class(toks[q].kind) || in_list)
                && !member
                && !is_structural(toks[i].kind)
                && !is_operator_word(toks[i].kind)
        };
    }
    is_name
}

/// By index into `sig`: whether the token is inside an `INHERITS` or
/// `IMPLEMENTS` list, that is, only words, commas and the periods of dotted
/// names come between the two. One pass, so a long statement stays linear.
fn type_list_entries(
    toks: &[Token],
    sig: &[usize],
    word_like: &dyn Fn(usize) -> bool,
    touches: &dyn Fn(usize, usize) -> bool,
) -> Vec<bool> {
    let mut entries = vec![false; sig.len()];
    let mut open = false;
    for (p, &j) in sig.iter().enumerate() {
        entries[p] = open;
        open = match toks[j].kind {
            Kind::Inherits | Kind::Implements => true,
            Kind::Comma => open,
            Kind::Period => {
                open && p > 0
                    && sig
                        .get(p + 1)
                        .is_some_and(|&n| touches(sig[p - 1], j) && touches(j, n))
            }
            _ => open && word_like(j),
        };
    }
    entries
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
