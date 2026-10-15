//! Queries over the scope analysis shared by the `strict.locks` rules
//! (LK001-LK003). The rules select findings from `oxabl_scope`; the facts
//! themselves are not analysed here.

use oxabl_ast::Span;
use oxabl_flow::{BlockId, BlockKind, UnitId, UnitKind};
use oxabl_lexer::{Kind, Lexer};
use oxabl_scope::{FileScopes, ReadKind, UnitScopes};

use super::handle_support::quote;

/// Whether the file is left unjudged: something the analysis cannot see (a
/// recovered parse, an unexpanded include, the work budget) may change every
/// answer, and an Unknown answer never makes a finding.
pub(super) fn not_judged(scopes: &FileScopes) -> bool {
    !scopes.unknown().is_empty()
}

/// The statement form of a read, for a sentence.
pub(super) fn read_form(kind: ReadKind) -> &'static str {
    match kind {
        ReadKind::Find => "`FIND`",
        ReadKind::For => "the `FOR` phrase",
        ReadKind::Preselect => "the `PRESELECT` phrase",
        ReadKind::CanFind => "`CAN-FIND`",
        ReadKind::OpenQuery => "the `OPEN QUERY` phrase",
        ReadKind::Get => "`GET`",
    }
}

/// A block, for a sentence: its header quoted (`` `DO TRANSACTION` ``), or
/// the routine's name. Line numbers are not used: the spans are in the
/// expanded text, whose lines are not the file's.
pub(super) fn describe_block(source: &str, unit: &UnitScopes, block: BlockId) -> String {
    let b = unit.block(block);
    if let BlockKind::Routine(kind) = b.kind {
        return match (&unit.name, unit.parent) {
            (Some(name), _) => match kind {
                UnitKind::Procedure => format!("procedure `{name}`"),
                UnitKind::Function => format!("function `{name}`"),
                _ => format!("`{name}`"),
            },
            (None, None) => "the main block".to_string(),
            (None, Some(_)) => "the trigger block".to_string(),
        };
    }
    format!("`{}`", quote(source, header(source, b.span)))
}

/// The header of the block statement at `span`: the text before the colon
/// that opens its body.
fn header(source: &str, span: Span) -> Span {
    let Some(text) = source.get(span.start as usize..span.end as usize) else {
        return span;
    };
    let colon = Lexer::new(text)
        .take_while(|t| t.kind != Kind::Eof)
        .find(|t| {
            t.kind == Kind::Colon && text[t.end..].chars().next().is_none_or(char::is_whitespace)
        });
    match colon {
        Some(t) => Span {
            start: span.start,
            end: span.start + t.start as u32,
        },
        None => span,
    }
}

/// Whether `outer` is `inner` or one of its enclosing blocks.
pub(super) fn block_within(unit: &UnitScopes, outer: BlockId, inner: BlockId) -> bool {
    std::iter::successors(Some(inner), |b| unit.block(*b).parent).any(|b| b == outer)
}

/// Whether `outer` is `inner` or one of the units that encloses it.
pub(super) fn unit_within(scopes: &FileScopes, outer: UnitId, inner: UnitId) -> bool {
    std::iter::successors(Some(inner), |u| scopes.unit(*u).parent).any(|u| u == outer)
}

/// Whether two buffer names, as written, name the same buffer: the last
/// dotted segment, compared case-insensitively (`db.Customer` and `customer`).
pub(super) fn same_buffer_name(a: &str, b: &str) -> bool {
    fn last(s: &str) -> &str {
        s.rsplit('.').next().unwrap_or(s)
    }
    last(a).eq_ignore_ascii_case(last(b))
}
