//! `scope-widened` lint (TX005), group `strict.transactions`.
//!
//! Fires on a reference to a database buffer, outside any block that scopes
//! it, that raises the scope of a `FOR` block of the same buffer to the whole
//! procedure.
//!
//! # Why
//!
//! A `FOR` block scopes its buffer to itself: the record leaves the buffer,
//! and its lock is released, when the block ends. A reference to the same
//! buffer outside any block that scopes records (a `FIND`, a field in a
//! `DISPLAY`, at procedure level or in a plain `DO`) scopes the buffer to the
//! procedure instead, and the `FOR` block's scope is raised to it. The last
//! record the block read then stays in the buffer, `SHARE-LOCK`ed if it was
//! locked, until the procedure ends.
//!
//! ```abl
//! FOR EACH Customer WHERE Customer.Balance > 1000 NO-LOCK:
//!     DISPLAY Customer.Name.
//! END.
//! FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK.
//! DISPLAY Customer.Balance.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE BUFFER bCustomer FOR Customer.
//! FOR EACH Customer WHERE Customer.Balance > 1000 NO-LOCK:
//!     DISPLAY Customer.Name.
//! END.
//! FIND FIRST bCustomer WHERE bCustomer.CustNum = 1 NO-LOCK.
//! DISPLAY bCustomer.Balance.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per raised scope, on the reference that raises it, naming
//! the first `FOR` block whose scope it raises. The rules it rests on are in
//! `docs/design/strict-analysis-open-questions.md`, section 7 (S2, S4, S5,
//! B2). Reported:
//!
//! - a reference and a `FOR` block in the same routine (S5);
//! - a reference in the main block and a `FOR` block in an internal procedure
//!   or function, or both in the same internal procedure, for a buffer of the
//!   file (B2).
//!
//! Not reported:
//!
//! - a reference in one internal procedure and a `FOR` block in another: B2
//!   says so, but it is the case least sure to match the compiler;
//! - a scope raised only over the free references of a deeper block, a raise
//!   held by a reference in a block header (`FOR EACH o OF c`), and one held
//!   by a statement the parser skips (`DISABLE TRIGGERS FOR LOAD OF t`):
//!   those rules are inferred;
//! - a scope raised to a block other than a routine block;
//! - a buffer parameter, a shared buffer, a trigger buffer, a temp-table, a
//!   table the loaded schema does not have, or any table when no schema is
//!   loaded;
//! - a class file: the scope rules for classes are inferred;
//! - a buffer whose scope is Unknown, and a file the scope analysis cannot
//!   judge.
//!
//! A raised scope that keeps a record locked after a transaction is also
//! reported by `lock-outlives-transaction` (LK002), at the read. The two
//! findings point at each other: this one at the cause, that one at the lock.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_flow::{BlockId, UnitId};
use oxabl_scope::{
    Buffer, BufferKind, FileScopes, ReadKind, RecordRead, Scope, ScopeKind, Table, UnitScopes,
};
use oxabl_semantic::AnalysisContext;

use super::TX005;
use super::handle_support::{quote, warning};
use super::lock_support::{describe_block, header, is_class, locate, not_judged, unit_within};
use super::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "scope-widened";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A reference outside a FOR block that raises the block's buffer scope to the procedure";

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
    let skipped = skipped_statements(program);
    let mut diags = Vec::new();
    for buffer in scopes.buffers() {
        let Some(seen) = buffer.scopes().filter(|_| judged_buffer(buffer)) else {
            continue;
        };
        for scope in seen {
            if in_skipped(&skipped, scope) {
                continue;
            }
            if let Some(d) = finding(scopes, ctx, buffer, scope) {
                diags.push(d);
            }
        }
    }
    diags.sort_by_key(|d| d.span.span.start);
    diags
}

/// Whether [`check`] reports `scope` of `buffer`, in a file it judges.
pub(crate) fn reports(
    scopes: &FileScopes,
    skipped: &[Span],
    ctx: &AnalysisContext,
    buffer: &Buffer,
    scope: &Scope,
) -> bool {
    scope.kind == ScopeKind::Raised
        && judged_buffer(buffer)
        && !in_skipped(skipped, scope)
        && finding(scopes, ctx, buffer, scope).is_some()
}

fn judged_buffer(buffer: &Buffer) -> bool {
    matches!(buffer.table, Table::Database(_))
        && matches!(buffer.kind, BufferKind::Default | BufferKind::Defined)
}

fn in_skipped(skipped: &[Span], scope: &Scope) -> bool {
    skipped
        .iter()
        .any(|s| s.start <= scope.at.start && scope.at.end <= s.end)
}

fn finding(
    scopes: &FileScopes,
    ctx: &AnalysisContext,
    buffer: &Buffer,
    scope: &Scope,
) -> Option<Diagnostic> {
    if scope.kind != ScopeKind::Raised || scope.block != BlockId::ROUTINE {
        return None;
    }
    let (raiser, raiser_block) = locate(scopes, scope.at.start)?;
    if in_header(ctx.source, raiser, raiser_block, scope.at) {
        return None;
    }
    let widened: Vec<&RecordRead> = scope
        .absorbed
        .iter()
        .filter_map(|a| weak_read(scopes, buffer, *a))
        .filter(|r| documented(scopes, scope.unit, raiser.unit, r))
        .collect();
    let first = widened.first()?;
    let scope_unit = scopes.unit(scope.unit);
    let for_unit = scopes.unit(first.unit);
    let more = match widened.len() {
        1 => String::new(),
        2 => " and of one other block".to_string(),
        n => format!(" and of {} other blocks", n - 1),
    };
    let used_in = if raiser.unit == scope.unit {
        String::new()
    } else {
        format!(
            " (a buffer of the file, used in {})",
            describe_block(ctx.source, raiser, BlockId::ROUTINE)
        )
    };
    let block_in = if first.unit == raiser.unit {
        String::new()
    } else {
        format!(
            " in {}",
            describe_block(ctx.source, for_unit, BlockId::ROUTINE)
        )
    };
    Some(warning(
        ctx,
        TX005,
        format!(
            "`{}` scopes `{}` to {}{used_in}, outside {}{block_in}: the scope of that block{more} \
             is raised to it",
            quote(ctx.source, scope.at),
            buffer.name,
            describe_block(ctx.source, scope_unit, scope.block),
            describe_block(ctx.source, for_unit, first.block),
        ),
        format!(
            "Give the reference a buffer of its own (`DEFINE BUFFER b{name} FOR {table}.`), or \
             scope it to a block (`DO FOR {name}:`). Until {end} ends, the buffer keeps the last \
             record the block read, and any lock on it (lock-outlives-transaction, LK002, \
             reports the locks that outlive a transaction).",
            name = buffer.name,
            table = buffer.table.name(),
            end = describe_block(ctx.source, scope_unit, scope.block),
        ),
        scope.at,
    ))
}

/// The `FOR` or `PRESELECT` phrase of `buffer` at `at`: an absorbed weak scope.
fn weak_read<'s>(scopes: &'s FileScopes, buffer: &Buffer, at: Span) -> Option<&'s RecordRead> {
    scopes.reads().iter().find(|r| {
        r.at.start == at.start
            && r.buffer == Some(buffer.id)
            && matches!(r.kind, ReadKind::For | ReadKind::Preselect)
    })
}

/// Whether a raise of the weak scope of `read` to the routine block of
/// `scope_unit`, held by a reference in `raiser`, rests on S5 or B2.
fn documented(scopes: &FileScopes, scope_unit: UnitId, raiser: UnitId, read: &RecordRead) -> bool {
    read.unit == raiser || (raiser == scope_unit && unit_within(scopes, scope_unit, read.unit))
}

/// Whether `at` is in the header of `block` (`WHERE`, `OF`, `BY`, `WHILE`):
/// such a reference belongs to the block around it by an inferred rule (S8).
fn in_header(source: &str, unit: &UnitScopes, block: BlockId, at: Span) -> bool {
    block != BlockId::ROUTINE && {
        let h = header(source, unit.block(block).span);
        h.start <= at.start && at.end <= h.end
    }
}

/// The statements the parser skips. A reference in one is a free reference by
/// an inferred rule (S3): `DISABLE TRIGGERS FOR LOAD OF t` names a table.
pub(crate) fn skipped_statements(program: &[Statement]) -> Vec<Span> {
    struct Skipped(Vec<Span>);
    impl Visitor for Skipped {
        fn statement(&mut self, stmt: &Statement) -> bool {
            if matches!(stmt.kind, StatementKind::Skipped { .. }) {
                self.0.push(stmt.span);
            }
            true
        }
    }
    let mut v = Skipped(Vec::new());
    walk_block(&mut v, program);
    v.0
}
