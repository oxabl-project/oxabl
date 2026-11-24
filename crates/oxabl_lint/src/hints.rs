//! Database facts for editor inlay hints.
//!
//! Each kind is selected by the function the strict rule itself uses, over
//! the same [`RunShared`] facts, so a hint and a finding never disagree:
//!
//! - a lock: the reads `default-lock` (LK001) reports;
//! - a buffer scope: the raised scopes `scope-widened` (TX005) reports;
//! - a transaction: the blocks `implicit-transaction` (TX001) and
//!   `procedure-wide-transaction` (TX002) report;
//! - an index: the index the effects analysis names for a search, the facts
//!   `find-in-loop` (DB001) reads.
//!
//! A file the scope analysis cannot judge gives no hint, and a fact that is
//! Unknown gives no hint. An index hint is also left out for a read that
//! follows an include that did not arrive. Spans are in the analysed
//! (expanded) text.

use oxabl_ast::{Span, Statement};
use oxabl_effects::{Access, IndexReason};
use oxabl_flow::BlockId;
use oxabl_lexer::{Kind, Token};
use oxabl_scope::{LockMode, Table};
use oxabl_semantic::AnalysisContext;

use crate::RunShared;
use crate::rules::lock_support::{describe_block, not_judged};
use crate::rules::{default_lock, implicit_transaction, procedure_wide_transaction, scope_widened};

/// The kinds of hint to select. A kind that is off costs nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HintKinds {
    pub locks: bool,
    pub index: bool,
    pub scope: bool,
    pub transactions: bool,
}

impl HintKinds {
    /// Every kind.
    pub const ALL: HintKinds = HintKinds {
        locks: true,
        index: true,
        scope: true,
        transactions: true,
    };

    /// Whether no kind is on.
    pub fn none(&self) -> bool {
        !(self.locks || self.index || self.scope || self.transactions)
    }
}

/// One fact to show in the editor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DatabaseHint {
    /// A read with no lock phrase, at the buffer name: the lock it takes.
    Lock { at: Span, mode: LockMode },
    /// A search, at the buffer name: the index the model expects the
    /// compiler to use.
    Index {
        at: Span,
        name: String,
        reason: IndexReason,
        /// The search reads the whole index.
        whole: bool,
    },
    /// A buffer scope raised to a routine block: the block, the reference
    /// that raises it, and the first `FOR` block whose scope it raises, each
    /// block described as the rule describes it.
    Scope {
        buffer: String,
        block: Span,
        described: String,
        raised_by: Span,
        widened: String,
    },
    /// A block that is a transaction block only because of `update`.
    Transaction {
        block: Span,
        /// The block, described as the rule describes it.
        described: String,
        update: Span,
        /// The main block of a procedure (TX002), not a nested block (TX001).
        procedure: bool,
    },
}

/// The hints of `kinds` over an analysed file.
pub fn database_hints(
    program: &[Statement],
    ctx: &AnalysisContext,
    shared: &RunShared,
    kinds: HintKinds,
) -> Vec<DatabaseHint> {
    if kinds.none() {
        return Vec::new();
    }
    let scopes = shared.scopes(program, ctx);
    if not_judged(scopes) {
        return Vec::new();
    }
    let tokens = shared.tokens(ctx);
    let mut hints = Vec::new();
    if kinds.locks {
        for read in default_lock::reported_reads(scopes) {
            if let Some(mode) = read.lock.effective() {
                hints.push(DatabaseHint::Lock { at: read.at, mode });
            }
        }
    }
    if kinds.index {
        for e in shared.effects(program, ctx).effects() {
            // An include that did not arrive may hold a temp-table or buffer
            // of the same name, so the table of a later read is a guess.
            if let Some(Access::Index(choice)) = &e.access
                && matches!(e.table, Table::Database(_) | Table::TempTable { .. })
                && ctx.elided_includes.first().is_none_or(|&o| o >= e.at.start)
            {
                hints.push(DatabaseHint::Index {
                    at: e.at,
                    name: choice.name.clone(),
                    reason: choice.reason,
                    whole: choice.whole,
                });
            }
        }
    }
    if kinds.scope {
        for w in scope_widened::reported_scopes(scopes, program, ctx) {
            let unit = scopes.unit(w.scope.unit);
            let first = w.widened[0];
            hints.push(DatabaseHint::Scope {
                buffer: w.buffer.name.clone(),
                block: trim(tokens, unit.block(w.scope.block).span),
                described: describe_block(ctx.source, unit, w.scope.block),
                raised_by: w.scope.at,
                widened: describe_block(ctx.source, scopes.unit(first.unit), first.block),
            });
        }
    }
    if kinds.transactions {
        let fragment = ctx.source_context.is_include_fragment();
        if let Some((main, update)) = procedure_wide_transaction::reported_main(scopes, fragment) {
            hints.push(DatabaseHint::Transaction {
                block: trim(tokens, main.block(BlockId::ROUTINE).span),
                described: describe_block(ctx.source, main, BlockId::ROUTINE),
                update: update.at,
                procedure: true,
            });
        }
        for (unit, block, update) in implicit_transaction::reported_blocks(scopes, fragment) {
            hints.push(DatabaseHint::Transaction {
                block: trim(tokens, unit.block(block).span),
                described: describe_block(ctx.source, unit, block),
                update: update.at,
                procedure: false,
            });
        }
    }
    hints
}

/// `span` from its first to its last token that is not a comment: the main
/// block's span holds the file's leading comments and trailing lines.
fn trim(tokens: &[Token], span: Span) -> Span {
    let first = tokens.partition_point(|t| (t.start as u32) < span.start);
    let last = tokens.partition_point(|t| (t.end as u32) <= span.end);
    let mut inside = tokens[first..last.max(first)]
        .iter()
        .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof));
    match (inside.next(), inside.next_back()) {
        (Some(a), Some(b)) => Span {
            start: a.start as u32,
            end: b.end as u32,
        },
        (Some(a), None) => Span {
            start: a.start as u32,
            end: a.end as u32,
        },
        _ => span,
    }
}
