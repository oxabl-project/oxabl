//! `procedure-wide-transaction` lint (TX002), group `strict.transactions`.
//!
//! Fires once per procedure file whose main block is a transaction block: it
//! updates the database directly, outside any block that scopes a
//! transaction.
//!
//! # Why
//!
//! A main block that updates the database directly becomes the transaction.
//! The transaction then starts with the procedure and ends with it: every
//! record it locks stays locked, and every change waits to be committed,
//! until the procedure returns. An error anywhere in it undoes all of it.
//!
//! ```abl
//! FIND FIRST Customer EXCLUSIVE-LOCK.
//! Customer.Balance = 0.
//! RUN report.p.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DO FOR Customer TRANSACTION:
//!     FIND FIRST Customer EXCLUSIVE-LOCK.
//!     Customer.Balance = 0.
//! END.
//! RUN report.p.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per file, on the first update the main block holds
//! directly. The rules it rests on are in
//! `docs/design/strict-analysis-open-questions.md`, section 7 (T2, T3).
//! An internal procedure that is a transaction block is reported by
//! `implicit-transaction` (TX001).
//!
//! Not reported:
//!
//! - a `TRIGGER PROCEDURE` file: a database trigger always runs in the
//!   transaction of the write that fires it;
//! - an include checked alone: its main block is not a procedure;
//! - an update of a temp-table, of a table the loaded schema does not have,
//!   or of any table when no schema is loaded;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_flow::{BlockId, UnitKind};
use oxabl_scope::{FileScopes, TxKind};
use oxabl_semantic::AnalysisContext;

use super::TX002;
use super::handle_support::warning;
use super::lock_support::{known_transaction, not_judged, update_phrase};

/// Kebab-case rule name.
pub const NAME: &str = "procedure-wide-transaction";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A main block that is a transaction block, so the transaction spans the procedure";

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
    let Some(main) = scopes.units().first() else {
        return Vec::new();
    };
    let TxKind::Implicit(update) = main.block(BlockId::ROUTINE).transaction.kind else {
        return Vec::new();
    };
    if main.kind != UnitKind::Main || !known_transaction(scopes, main, BlockId::ROUTINE, fragment) {
        return Vec::new();
    }
    vec![warning(
        ctx,
        TX002,
        format!(
            "The main block is a transaction block, because {} directly in it: the \
             transaction spans the whole procedure",
            update_phrase(scopes, ctx.source, &update)
        ),
        "Put the updates in `DO TRANSACTION:` blocks (`DO FOR <buffer> TRANSACTION:` also ends \
         the buffer's scope), so that each transaction ends with its updates and its locks are \
         not held until the procedure returns."
            .to_string(),
        update.at,
    )]
}
