//! `implicit-transaction` lint (TX001), group `strict.transactions`.
//!
//! Fires on a block that becomes a transaction block only because it updates
//! the database directly: a `FOR`, a `REPEAT`, a `DO ON ERROR` or an internal
//! procedure with no `TRANSACTION` keyword.
//!
//! # Why
//!
//! The compiler makes a block a transaction block when it updates the
//! database directly or reads with `EXCLUSIVE-LOCK`. Nothing in the code then
//! says where the transaction starts and ends, and a change elsewhere in the
//! block (one more update, a statement moved into a nested block) moves it.
//! Writing `TRANSACTION` says which block the author means.
//!
//! ```abl
//! REPEAT:
//!     CREATE Customer.
//!     Customer.CustNum = NEXT-VALUE(NextCustNum).
//!     LEAVE.
//! END.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! REPEAT TRANSACTION:
//!     CREATE Customer.
//!     Customer.CustNum = NEXT-VALUE(NextCustNum).
//!     LEAVE.
//! END.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per block, on its first update (the statement that makes it
//! a transaction block). The rules it rests on are in
//! `docs/design/strict-analysis-open-questions.md`, section 7 (T2, T3).
//!
//! Not reported:
//!
//! - the main block: `procedure-wide-transaction` (TX002) reports it;
//! - a block inside another transaction block of the same routine, which is a
//!   sub-transaction, or inside a block whose transaction status is Unknown;
//! - the routine block of a function, method, property accessor, `ON ... DO`
//!   trigger or `TRIGGER PROCEDURE`: the model's rule for the first four is
//!   inferred, and a database trigger always runs in the transaction of the
//!   write that fires it;
//! - an update of a temp-table, of a table the loaded schema does not have,
//!   or of any table when no schema is loaded;
//! - in an include checked alone, a block of its main block: the includer may
//!   put it inside a transaction;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_flow::{BlockId, BlockKind, UnitKind};
use oxabl_scope::{FileScopes, TxKind, UnitScopes};
use oxabl_semantic::AnalysisContext;

use super::TX001;
use super::handle_support::warning;
use super::lock_support::{describe_block, known_transaction, not_judged, update_phrase};

/// Kebab-case rule name.
pub const NAME: &str = "implicit-transaction";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A block that becomes a transaction only because it updates the database";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    check(&crate::shared::analyze_scopes(program, ctx), program, ctx)
}

/// The findings over an analysed file.
pub fn check(
    scopes: &FileScopes,
    _program: &[Statement],
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    if not_judged(scopes) {
        return Vec::new();
    }
    let fragment = ctx.source_context.is_include_fragment();
    let mut diags = Vec::new();
    for unit in scopes.units() {
        if fragment && unit.parent.is_none() {
            continue;
        }
        for b in &unit.blocks {
            let TxKind::Implicit(update) = b.transaction.kind else {
                continue;
            };
            if !reported(scopes, unit, b.id, fragment) {
                continue;
            }
            diags.push(warning(
                ctx,
                TX001,
                format!(
                    "{} is a transaction block only because {}",
                    describe_block(ctx.source, unit, b.id),
                    update_phrase(scopes, ctx.source, &update),
                ),
                help(ctx.source, unit, b.id),
                update.at,
            ));
        }
    }
    diags
}

fn reported(scopes: &FileScopes, unit: &UnitScopes, block: BlockId, fragment: bool) -> bool {
    let b = unit.block(block);
    if let BlockKind::Routine(kind) = b.kind
        && kind != UnitKind::Procedure
    {
        return false;
    }
    b.transaction.within.is_none()
        && known_transaction(scopes, unit, block, fragment)
        && std::iter::successors(b.parent, |p| unit.block(*p).parent)
            .all(|p| !matches!(unit.block(p).transaction.kind, TxKind::Unknown(_)))
}

fn help(source: &str, unit: &UnitScopes, block: BlockId) -> String {
    match unit.block(block).kind {
        BlockKind::Routine(_) => format!(
            "Put the updates in a `DO TRANSACTION:` block, so that the transaction is written \
             down and does not span all of {}.",
            describe_block(source, unit, block)
        ),
        _ => "Write the transaction down: add `TRANSACTION` to the block header, or put the \
              updates in a `DO TRANSACTION:` block."
            .to_string(),
    }
}
