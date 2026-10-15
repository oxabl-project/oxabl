//! `find-in-loop` lint (DB001), group `strict.access`.
//!
//! Fires on a `FIND` or `CAN-FIND` of a database table that runs once per row
//! of a record loop of its routine: a `FOR EACH` block, or a `REPEAT` or
//! `DO WHILE` block that `GET NEXT` or `FIND NEXT` drives.
//!
//! # Why
//!
//! Each `FIND` is a request to the database: one per row of the loop. A loop
//! over a thousand orders that finds each order's customer makes a thousand
//! and one requests (N+1). A join in the loop's own query lets the
//! database find both records in one pass, and a client-server client fetch
//! them in fewer messages.
//!
//! ```abl
//! FOR EACH Order NO-LOCK:
//!     FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.
//!     DISPLAY Order.OrderNum Customer.Name.
//! END.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! FOR EACH Order NO-LOCK,
//!     FIRST Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK:
//!     DISPLAY Order.OrderNum Customer.Name.
//! END.
//! ```
//!
//! A join drops the rows that have no match; `OUTER-JOIN` keeps them. When the
//! record does not depend on the row, read it once before the loop.
//!
//! # What is and is not reported
//!
//! One diagnostic per `FIND` or `CAN-FIND`, naming the innermost record loop
//! around it. The model is the access shape of `oxabl_effects`
//! (`docs/design/strict-analysis-open-questions.md`, section 8.3).
//!
//! Not reported:
//!
//! - a read of a temp-table, or of a table the loaded schema does not have
//!   (with no schema, nothing is reported);
//! - `FIND NEXT` or `FIND PREV` that drives its own loop, `FIND CURRENT`,
//!   which reads the record already in the buffer again, and a `FIND` by
//!   `ROWID` or `RECID`, which fetches one known record, mostly to lock it
//!   again (`lock-upgrade-in-loop`, LK003);
//! - a `CAN-FIND` in the header of the loop (its `WHERE`), which the scope
//!   model places in the block around the loop;
//! - a read in another routine that the loop calls (`RUN`, a function): the
//!   transitive summaries that would show it are a later slice;
//! - a loop that only counts (`DO i = 1 TO n`, `REPEAT` with no `GET NEXT` or
//!   `FIND NEXT`), and `FOR FIRST` / `FOR LAST`, which read one record;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_effects::{Access, FileEffects, Op};
use oxabl_scope::{FileScopes, ReadKind, Table};
use oxabl_semantic::AnalysisContext;

use super::DB001;
use super::handle_support::{quote, warning};
use super::lock_support::{describe_block, not_judged};

/// Kebab-case rule name.
pub const NAME: &str = "find-in-loop";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.access";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A FIND or CAN-FIND that runs once per row of a FOR EACH or query loop (N+1)";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let shared = crate::RunShared::default();
    check(
        shared.effects(program, ctx),
        shared.scopes(program, ctx),
        program,
        ctx,
    )
}

/// The findings over an analysed file.
pub fn check(
    effects: &FileEffects,
    scopes: &FileScopes,
    _program: &[Statement],
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    if not_judged(scopes) {
        return Vec::new();
    }
    let mut diags = Vec::new();
    for e in effects.effects() {
        let (Op::Read, Some(kind @ (ReadKind::Find | ReadKind::CanFind)), Some(lp)) =
            (e.op, e.read, e.per_row_of)
        else {
            continue;
        };
        // FIND CURRENT does not search; a find by ROWID fetches one known
        // record, mostly to lock it again (LK003).
        if matches!(e.access, None | Some(Access::Rowid)) || !matches!(e.table, Table::Database(_))
        {
            continue;
        }
        let unit = scopes.unit(e.unit);
        let form = match kind {
            ReadKind::CanFind => "`CAN-FIND`",
            _ => "`FIND`",
        };
        let what = match kind {
            ReadKind::CanFind => "an existence check",
            _ => "a database request",
        };
        diags.push(warning(
            ctx,
            DB001,
            format!(
                "{form} of `{}` runs once per row of {}: {what} for every row (N+1)",
                quote(ctx.source, e.at),
                describe_block(ctx.source, unit, lp),
            ),
            "Join the table into the loop's query (`FOR EACH a NO-LOCK, FIRST b WHERE ... \
             NO-LOCK:`; `OUTER-JOIN` keeps rows with no match), or read it once before the \
             loop when it does not depend on the row."
                .to_string(),
            e.at,
        ));
    }
    diags
}
