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

use std::collections::HashSet;

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

/// Keywords that open a declaration of a name; the name that follows is protected
/// and, for these, every other spelling of it in the file too.
fn declares_name(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::Variable
            | Kind::Parameter
            | Kind::Buffer
            | Kind::TempTable
            | Kind::Field
            | Kind::Property
            | Kind::Procedure
            | Kind::Function
            | Kind::Event
    )
}

/// Keywords after which a name or type name follows without declaring a
/// variable-like name.
fn introduces_name(kind: Kind) -> bool {
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
            | Kind::Returns
    )
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

/// Keywords that head a `:` chain as a system handle or block opener, never as a
/// user name (`THIS-OBJECT:Menu`, `DO:MESSAGE`).
fn is_receiver_keyword(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::ThisObject
            | Kind::ThisProcedure
            | Kind::KwSelf
            | Kind::Session
            | Kind::ErrorStatus
            | Kind::Focus
            | Kind::CurrentWindow
            | Kind::ActiveWindow
            | Kind::DefaultWindow
            | Kind::Compiler
            | Kind::Clipboard
            | Kind::LastEvent
            | Kind::Debugger
            | Kind::FileInfo
            | Kind::Buffer
            | Kind::Frame
            | Kind::Browse
            | Kind::Dataset
            | Kind::Do
            | Kind::Repeat
            | Kind::KwElse
            | Kind::Finally
            | Kind::Triggers
    )
}

/// Flags (by index into `toks`) the keyword tokens that are names, not keywords.
///
/// An unreserved keyword such as `image` or `server` is a legal name, and the
/// token stream alone cannot tell the two uses apart. A token is a name when it
/// is part of a dotted or colon-qualified name, sits in a name position (after
/// `CLASS`, `NEW`, `AS`, `DEFINE ... VARIABLE`, `PROCEDURE`, `RUN`, ...), is a
/// method name or its return type, or is spelled like a name the file declares.
/// A name that comes from an include file stays undetected.
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
    let qualifier =
        |i: usize| matches!(toks[i].kind, Kind::Period | Kind::Colon | Kind::DoubleColon);

    let mut is_name = vec![false; toks.len()];
    let mut declared: HashSet<String> = HashSet::new();
    let mut declare = |i: usize, is_name: &mut Vec<bool>, remember: bool| {
        is_name[i] = true;
        if remember && toks[i].kind != Kind::Identifier {
            declared.insert(source[toks[i].start..toks[i].end].to_ascii_lowercase());
        }
    };

    for (p, &i) in sig.iter().enumerate() {
        if !word_like(i) {
            continue;
        }
        let prev = p.checked_sub(1).map(|q| sig[q]);
        let next = sig.get(p + 1).copied();

        // `a.b`, `a:b`, `a::b`: every word of the chain is a name.
        if prev.is_some_and(|q| qualifier(q) && touches(q, i)) {
            is_name[i] = true;
        }
        if let (Some(n), Some(&after)) = (next, sig.get(p + 2))
            && qualifier(n)
            && !(toks[n].kind != Kind::Period && is_receiver_keyword(toks[i].kind))
            && touches(i, n)
            && touches(n, after)
            && word_like(after)
        {
            is_name[i] = true;
        }

        if let Some(q) = prev
            && !is_structural(toks[i].kind)
            && !is_name[q]
        {
            // `DELETE PROCEDURE h` and `EXTERNAL PROCEDURE "x"` name no new thing.
            let not_a_declaration = toks[q].kind == Kind::Procedure
                && p >= 2
                && matches!(toks[sig[p - 2]].kind, Kind::Delete | Kind::External);
            if not_a_declaration {
                // The operand is an expression; its keywords stay keywords.
            } else if declares_name(toks[q].kind) {
                declare(i, &mut is_name, true);
            } else if introduces_name(toks[q].kind) {
                declare(i, &mut is_name, false);
            }
        }

        // `name AS type`
        if next.is_some_and(|n| toks[n].kind == Kind::KwAs) && !is_structural(toks[i].kind) {
            declare(i, &mut is_name, true);
        }

        // `METHOD [modifiers] type name(`: the name and the type before it.
        if toks[i].kind == Kind::Method
            && prev.is_none_or(|q| toks[q].kind != Kind::End)
            && let Some(open) = sig[p + 1..]
                .iter()
                .take(8)
                .position(|&j| toks[j].kind == Kind::LeftParen)
                .map(|k| p + 1 + k)
            && open >= p + 3
        {
            let name = sig[open - 1];
            let ty = sig[open - 2];
            if word_like(name) {
                declare(name, &mut is_name, true);
            }
            if word_like(ty) && !is_structural(toks[ty].kind) {
                is_name[ty] = true;
            }
        }
    }

    if !declared.is_empty() {
        for &i in &sig {
            if !is_name[i]
                && toks[i].kind != Kind::Identifier
                && word_like(i)
                && declared.contains(&source[toks[i].start..toks[i].end].to_ascii_lowercase())
            {
                is_name[i] = true;
            }
        }
    }
    is_name
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
