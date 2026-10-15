//! `lock-outlives-transaction` lint (LK002), group `strict.locks`.
//!
//! Fires on a database record that a transaction reads with a lock (or
//! creates) when the buffer's scope is wider than the transaction: the record
//! stays in the buffer, `SHARE-LOCK`ed, after the transaction ends.
//!
//! # Why
//!
//! A lock taken in a transaction is held until the transaction ends. If the
//! buffer's scope is still open then, an `EXCLUSIVE-LOCK` becomes a
//! `SHARE-LOCK`, held until the scope ends, the buffer reads another record, or
//! `RELEASE`. Other sessions cannot change the record meanwhile. It happens
//! when a `DO TRANSACTION`, which does not scope records, reads into a buffer
//! scoped to the procedure, or when an internal procedure uses the file's
//! buffer, which is scoped to the main block.
//!
//! ```abl
//! DO TRANSACTION:
//!     FIND FIRST Customer WHERE Customer.CustNum = 1 EXCLUSIVE-LOCK.
//!     Customer.Balance = 0.
//! END.
//! MESSAGE "done".
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DO FOR Customer TRANSACTION:
//!     FIND FIRST Customer WHERE Customer.CustNum = 1 EXCLUSIVE-LOCK.
//!     Customer.Balance = 0.
//! END.
//! MESSAGE "done".
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per buffer and outermost transaction block, on the first
//! read in the transaction that takes a lock. The rules it rests on are in
//! `docs/design/strict-analysis-open-questions.md`, section 7 (L4, S1-S5, B2,
//! T1-T2). A finding is made only from the rules marked Documented or
//! Recalled with medium confidence there.
//!
//! Not reported:
//!
//! - a transaction whose last read into the buffer is `NO-LOCK`, or that
//!   releases or deletes the record after its last read;
//! - a transaction whose last read into the buffer is the record phrase of a
//!   `FOR EACH` or a `PRESELECT`: whether the buffer still holds a record after
//!   the last iteration is not decided;
//! - a scope that ends with the transaction: nothing runs in the scope after
//!   the transaction block;
//! - a temp-table, a table the loaded schema does not have, or any table when
//!   no schema is loaded;
//! - a buffer whose scope is Unknown, or belongs to the caller (a parameter, a
//!   shared buffer, a trigger buffer);
//! - a transaction block inside a block whose transaction status is Unknown;
//! - an implicit transaction of a function, method, property accessor or
//!   `ON ... DO` trigger, and any class file: the model's rules for them are
//!   inferred, not documented;
//! - a scope that reaches the read only because an outer free reference
//!   absorbed the scope of free references in a deeper block (S5's general
//!   merge, which is inferred). A weak scope raised by a free reference, and a
//!   file's buffer used in an internal procedure (B2), are reported;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! A record that `CREATE` makes in the transaction is held the same way, but
//! only reads are reported.
//!
//! When a reference outside a `FOR` block raises the buffer's scope,
//! `scope-widened` (TX005) reports that reference, and the help of this
//! finding points at it. The two are kept: this one shows the lock, that one
//! the cause.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::{Span, Statement};
use oxabl_common::{Diagnostic, Severity};
use oxabl_flow::{BlockId, BlockKind, ForQualifier};
use oxabl_lexer::{Kind, Lexer};
use oxabl_scope::{
    Buffer, BufferId, FileScopes, Lock, LockMode, ReadKind, RecordRead, Scope, ScopeKind, Table,
    TxKind, UnitScopes,
};
use oxabl_semantic::AnalysisContext;
use rustc_hash::FxHashMap;

use super::LK002;
use super::handle_support::warning;
use super::lock_support::{
    block_within, describe_block, is_class, not_judged, same_buffer_name, transaction_documented,
    unit_within,
};

/// Kebab-case rule name.
pub const NAME: &str = "lock-outlives-transaction";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.locks";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A record still SHARE-LOCKed after its transaction, because the buffer scope is wider";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    check(&crate::shared::analyze_scopes(program, ctx), program, ctx)
}

/// The findings over an analysed file.
pub fn check(scopes: &FileScopes, program: &[Statement], ctx: &AnalysisContext) -> Vec<Diagnostic> {
    if not_judged(scopes) || is_class(scopes) {
        return Vec::new();
    }
    // Reads into one buffer inside one outermost transaction block.
    let mut groups: FxHashMap<(BufferId, oxabl_flow::UnitId, BlockId), Vec<&RecordRead>> =
        FxHashMap::default();
    let mut order = Vec::new();
    for read in scopes.reads() {
        let Some(b) = read.buffer else {
            continue;
        };
        let unit = scopes.unit(read.unit);
        let Some(tx) = outermost_transaction(unit, read.block) else {
            continue;
        };
        let key = (b, read.unit, tx);
        let group = groups.entry(key).or_default();
        if group.is_empty() {
            order.push(key);
        }
        group.push(read);
    }
    let skipped = super::scope_widened::skipped_statements(program);
    let mut diags = Vec::new();
    for key in order {
        let (b, unit_id, tx) = key;
        let reads = &groups[&key];
        let buffer = scopes.buffer(b);
        let unit = scopes.unit(unit_id);
        if let Some(d) = finding(scopes, &skipped, ctx, buffer, unit, tx, reads) {
            diags.push(d);
        }
    }
    diags
}

fn finding(
    scopes: &FileScopes,
    skipped: &[Span],
    ctx: &AnalysisContext,
    buffer: &Buffer,
    unit: &UnitScopes,
    tx: BlockId,
    reads: &[&RecordRead],
) -> Option<Diagnostic> {
    if !matches!(buffer.table, Table::Database(_)) || !transaction_documented(unit, tx) {
        return None;
    }
    let buffer_scopes = buffer.scopes()?;
    if reads.iter().any(|r| matches!(r.lock, Lock::Unknown(_))) {
        return None;
    }
    let last = reads.iter().max_by_key(|r| r.at.start)?;
    if !takes_lock(last) || ends_a_loop(unit, last) {
        return None;
    }
    let first = reads.iter().find(|r| takes_lock(r))?;
    let scope = scope_of(scopes, buffer_scopes, first)?;
    if reads
        .iter()
        .any(|r| !scope_documented(scopes, buffer.id, scope, r))
    {
        return None;
    }
    let tx_span = unit.block(tx).span;
    let cross_unit = scope.unit != unit.unit;
    let scope_unit = scopes.unit(scope.unit);
    if cross_unit {
        if scope.block != BlockId::ROUTINE || !unit_within(scopes, scope.unit, unit.unit) {
            return None;
        }
    } else if scope.block == tx || !block_within(unit, scope.block, tx) {
        return None;
    }
    if released_after(ctx.source, tx_span, last.at.start, &buffer.name) {
        return None;
    }
    if !cross_unit
        && !runs_after(
            ctx.source,
            tx_span.end,
            scope_unit.block(scope.block).span.end,
        )
    {
        return None;
    }
    let held = match first.lock {
        Lock::Explicit(LockMode::ExclusiveLock) => "its EXCLUSIVE-LOCK becomes a SHARE-LOCK",
        _ => "its SHARE-LOCK is kept",
    };
    let mut help = if cross_unit {
        format!(
            "The procedure uses the file's buffer `{name}`, which is scoped to the main block. \
             Define a buffer for `{table}` in the procedure, or `RELEASE {name}` before the \
             transaction ends.",
            name = buffer.name,
            table = buffer.table.name(),
        )
    } else {
        format!(
            "Scope the buffer to the transaction (`DO FOR {name} TRANSACTION:`), or \
             `RELEASE {name}` before the transaction ends.",
            name = buffer.name,
        )
    };
    if super::scope_widened::reports(scopes, skipped, ctx, buffer, scope) {
        help.push_str(
            " A reference outside the block raises the buffer's scope; scope-widened (TX005) \
             reports such references.",
        );
    }
    Some(warning(
        ctx,
        LK002,
        format!(
            "`{}` still holds its record locked after the transaction of {} ends: {held}, held \
             until the scope of the buffer, {}, ends",
            buffer.name,
            describe_block(ctx.source, unit, tx),
            describe_block(ctx.source, scope_unit, scope.block),
        ),
        help,
        first.at,
    ))
}

fn takes_lock(read: &RecordRead) -> bool {
    read.kind != ReadKind::CanFind
        && matches!(
            read.lock.effective(),
            Some(LockMode::ShareLock | LockMode::ExclusiveLock)
        )
}

/// Whether the read is the record phrase of a loop: after the last iteration
/// the buffer may hold no record, which the model does not decide.
fn ends_a_loop(unit: &UnitScopes, read: &RecordRead) -> bool {
    match read.kind {
        ReadKind::For => !matches!(
            unit.block(read.block).kind,
            BlockKind::For(ForQualifier::First | ForQualifier::Last)
        ),
        ReadKind::Preselect => true,
        _ => false,
    }
}

/// The outermost transaction block around `block`, itself included. `None`
/// when there is none, or a block on the way is Unknown.
fn outermost_transaction(unit: &UnitScopes, block: BlockId) -> Option<BlockId> {
    let mut outer = None;
    for b in std::iter::successors(Some(block), |b| unit.block(*b).parent) {
        match unit.block(b).transaction.kind {
            TxKind::Unknown(_) => return None,
            k if k.is_transaction() => outer = Some(b),
            _ => {}
        }
    }
    outer
}

/// The scope of the buffer that holds `read`.
fn scope_of<'s>(scopes: &FileScopes, seen: &'s [Scope], read: &RecordRead) -> Option<&'s Scope> {
    let unit = scopes.unit(read.unit);
    seen.iter().find(|s| {
        if s.unit == read.unit {
            block_within(unit, s.block, read.block)
        } else {
            s.block == BlockId::ROUTINE && unit_within(scopes, s.unit, read.unit)
        }
    })
}

/// Whether the scope reaches `read` by documented or medium-confidence rules:
/// the read's own scope (S1, S2, S4), a weak scope raised by a free reference
/// (S5), or a file's buffer used in an internal procedure (B2).
fn scope_documented(scopes: &FileScopes, b: BufferId, scope: &Scope, read: &RecordRead) -> bool {
    let unit = scopes.unit(read.unit);
    let natural = natural_block(unit, scope, read);
    if scope.unit == read.unit && scope.block == natural {
        return true;
    }
    let weak_raised = scope.kind == ScopeKind::Raised
        && scopes.reads().iter().any(|r| {
            r.unit == read.unit
                && r.block == natural
                && r.buffer == Some(b)
                && matches!(r.kind, ReadKind::For | ReadKind::Preselect)
        });
    weak_raised || (scope.unit != read.unit && natural == BlockId::ROUTINE)
}

/// The block whose scope the read belongs to before any raise: its `FOR` or
/// `PRESELECT` block, else the nearest block that scopes records (S4).
fn natural_block(unit: &UnitScopes, scope: &Scope, read: &RecordRead) -> BlockId {
    if matches!(read.kind, ReadKind::For | ReadKind::Preselect) {
        return read.block;
    }
    std::iter::successors(Some(read.block), |b| unit.block(*b).parent)
        .find(|&b| match unit.block(b).kind {
            BlockKind::Routine(_) | BlockKind::Repeat | BlockKind::For(_) => true,
            BlockKind::Do => {
                scope.unit == read.unit && scope.block == b && scope.kind == ScopeKind::Strong
            }
            BlockKind::Catch | BlockKind::Finally => false,
        })
        .unwrap_or(BlockId::ROUTINE)
}

/// Whether the transaction releases or deletes the buffer's record after
/// `after`: `RELEASE b` or `DELETE b`.
fn released_after(source: &str, tx: Span, after: u32, name: &str) -> bool {
    let start = after.max(tx.start) as usize;
    let Some(text) = source.get(start..tx.end as usize) else {
        return false;
    };
    let mut tokens = Lexer::new(text).filter(|t| t.kind != Kind::Comment);
    while let Some(t) = tokens.next() {
        if t.kind == Kind::Eof {
            break;
        }
        if matches!(t.kind, Kind::Release | Kind::Delete)
            && tokens
                .next()
                .is_some_and(|n| same_buffer_name(&text[n.start..n.end], name))
        {
            return true;
        }
    }
    false
}

/// Whether a statement runs between `from` and `to` other than the `END` of
/// the enclosing block.
fn runs_after(source: &str, from: u32, to: u32) -> bool {
    let Some(text) = source.get(from as usize..to as usize) else {
        return false;
    };
    let mut in_end = false;
    for t in Lexer::new(text) {
        match t.kind {
            Kind::Eof => break,
            Kind::Comment => {}
            Kind::Period => in_end = false,
            Kind::End => in_end = true,
            _ if in_end => {}
            _ => return true,
        }
    }
    false
}
