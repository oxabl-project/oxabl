//! Queries over the scope analysis shared by the `strict.locks` rules
//! (LK001-LK003) and the `strict.transactions` rules (TX001-TX005). The rules
//! select findings from `oxabl_scope`; the facts themselves are not analysed
//! here.

use oxabl_ast::Span;
use oxabl_flow::{BlockId, BlockKind, UnitId, UnitKind};
use oxabl_lexer::{Kind, Lexer};
use oxabl_scope::{FileScopes, ReadKind, Table, TxKind, UnitScopes, Update, UpdateKind};

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
pub(super) fn header(source: &str, span: Span) -> Span {
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

/// A class file: the scope rules for classes (B3) are inferred.
pub(super) fn is_class(scopes: &FileScopes) -> bool {
    scopes.units().iter().any(|u| {
        matches!(
            u.kind,
            UnitKind::Method
                | UnitKind::Constructor
                | UnitKind::Destructor
                | UnitKind::PropertyGet
                | UnitKind::PropertySet
        )
    })
}

/// Whether the transaction block rests on a documented rule (T1, T2).
pub(super) fn transaction_documented(unit: &UnitScopes, tx: BlockId) -> bool {
    let b = unit.block(tx);
    match (b.transaction.kind, b.kind) {
        (TxKind::Explicit, _) => true,
        (TxKind::Implicit(_), BlockKind::Routine(k)) => {
            matches!(
                k,
                UnitKind::Main | UnitKind::Procedure | UnitKind::DbTrigger
            )
        }
        (TxKind::Implicit(_), _) => true,
        _ => false,
    }
}

/// Whether `block` is a transaction block the transaction rules report on:
/// explicit, or implicit by a documented rule with an update of a table of
/// the loaded schema. The main block of an include fragment is none: the
/// includer decides which block the fragment's statements are in.
pub(super) fn known_transaction(
    scopes: &FileScopes,
    unit: &UnitScopes,
    block: BlockId,
    fragment: bool,
) -> bool {
    if !transaction_documented(unit, block) {
        return false;
    }
    match unit.block(block).transaction.kind {
        TxKind::Implicit(update) => {
            !(fragment && unit.parent.is_none() && block == BlockId::ROUTINE)
                && update
                    .buffer
                    .is_some_and(|b| matches!(scopes.buffer(b).table, Table::Database(_)))
        }
        kind => kind == TxKind::Explicit,
    }
}

/// The outermost [`known_transaction`] block around `block`, itself included.
pub(super) fn enclosing_transaction(
    scopes: &FileScopes,
    unit: &UnitScopes,
    block: BlockId,
    fragment: bool,
) -> Option<BlockId> {
    std::iter::successors(Some(block), |b| unit.block(*b).parent)
        .filter(|&b| known_transaction(scopes, unit, b, fragment))
        .last()
}

/// The innermost unit and block whose text holds the offset `at`.
pub(super) fn locate(scopes: &FileScopes, at: u32) -> Option<(&UnitScopes, BlockId)> {
    let inside = |span: Span| span.start <= at && at < span.end;
    let unit = scopes
        .units()
        .iter()
        .filter(|u| inside(u.span))
        .max_by_key(|u| u.span.start)?;
    let block = unit
        .blocks
        .iter()
        .filter(|b| inside(b.span))
        .max_by_key(|b| b.span.start)
        .map_or(BlockId::ROUTINE, |b| b.id);
    Some((unit, block))
}

/// What an update does, for a sentence: `` it assigns `Customer.Name`, a
/// field of `Customer` ``.
pub(super) fn update_phrase(scopes: &FileScopes, source: &str, update: &Update) -> String {
    let table = update
        .buffer
        .map_or("a database table", |b| scopes.buffer(b).table.name());
    match update.kind {
        UpdateKind::Create => format!("it creates a record of `{table}`"),
        UpdateKind::Delete => format!("it deletes a record of `{table}`"),
        UpdateKind::Assign => format!(
            "it assigns `{}`, a field of `{table}`",
            quote(source, update.at)
        ),
        UpdateKind::BufferCopy => format!("it copies into a record of `{table}`"),
        UpdateKind::Input => format!("it reads user input into `{}`", quote(source, update.at)),
        UpdateKind::ExclusiveRead => format!("it reads `{table}` with EXCLUSIVE-LOCK"),
    }
}
