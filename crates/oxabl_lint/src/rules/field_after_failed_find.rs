//! `field-after-failed-find` lint (NL003), group `strict.nulls`.
//!
//! Fires on a field of a buffer read or assigned where the buffer may have no
//! record, because a `FIND ... NO-ERROR` of the buffer may have found nothing
//! and no `AVAILABLE` test or refill lies between them on some path.
//!
//! # Why
//!
//! `NO-ERROR` turns a failed `FIND` into an empty buffer instead of an error.
//! The field reference after it then raises the error the `FIND` did not:
//! `** No Customer record is available`. It does not read the unknown value,
//! so the code after it never runs, and the message names the field, not the
//! `FIND` that failed.
//!
//! ```abl
//! DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
//! FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.
//! cName = Customer.Name.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
//! FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.
//! IF AVAILABLE Customer THEN
//!     cName = Customer.Name.
//! ```
//!
//! When the record must exist, drop `NO-ERROR` and let the `FIND` raise the
//! error where it happens.
//!
//! # What is and is not reported
//!
//! One diagnostic per statement and buffer, at its first field reference.
//! The model is `oxabl_nulls` (`docs/design/strict-analysis-open-questions.md`,
//! section 9). A guard is `AVAILABLE b` in an `IF` or `WHILE` condition (with
//! `NOT`, `AND`, `OR`), an arm that leaves (`RETURN`, `LEAVE`, `NEXT`, `UNDO`),
//! `ERROR-STATUS:ERROR` right after the `FIND`, or `AVAILABLE b` anywhere in
//! the same statement. A refill is a `FIND` without `NO-ERROR`, `CREATE`, or a
//! field reference that completed. A guard or use of `c` also proves a buffer
//! whose fields the criteria of `FIND c ... NO-ERROR` read: that `FIND` would
//! have failed without its record.
//!
//! Not reported:
//!
//! - a reference in a statement with `NO-ERROR`, and the criteria of a `FIND`
//!   or `FOR` of the buffer itself;
//! - `FIND CURRENT ... NO-ERROR`, which re-reads the record the buffer holds;
//! - a path on which a statement raised an error (a `CATCH`, an `ON ERROR`
//!   arm): the error is the first problem there;
//! - a guard the model cannot see, such as a flag set in the not-available
//!   arm and tested later;
//! - a branch of an `&IF` whose test the preprocessor could not decide;
//! - a buffer the model cannot follow: passed to a call (`BUFFER b`), changed
//!   by a dynamic method on a handle, shared, or named by another routine of
//!   the file when the statement calls code;
//! - a reference in an internal procedure or function to a `FIND` of its
//!   caller: each routine starts with nothing known;
//! - a table the loaded schema does not have (with no schema, only
//!   temp-tables are judged);
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget, and a statement with an elided
//!   `{&name}`.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_nulls::FileNulls;
use oxabl_scope::FileScopes;
use oxabl_semantic::AnalysisContext;

use super::NL003;
use super::handle_support::{quote, warning};

/// Kebab-case rule name.
pub const NAME: &str = "field-after-failed-find";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.nulls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A buffer field used after a FIND ... NO-ERROR that may have failed, with no AVAILABLE test";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let shared = crate::RunShared::default();
    check(
        shared.nulls(program, ctx),
        shared.scopes(program, ctx),
        program,
        ctx,
    )
}

/// The findings over an analysed file.
pub fn check(
    nulls: &FileNulls,
    scopes: &FileScopes,
    _program: &[Statement],
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for m in nulls.missing() {
        let buffer = &scopes.buffer(m.buffer).name;
        let Some(&first) = m.finds.first() else {
            continue;
        };
        let find = quote(ctx.source, nulls.find(first).at);
        let more = match m.finds.len() {
            1 => String::new(),
            n => format!(" (or one of {} others)", n - 1),
        };
        let verb = if m.write { "assigned" } else { "read" };
        diags.push(warning(
            ctx,
            NL003,
            format!(
                "`{}` is {verb} where `{buffer}` may have no record: `{find}`{more} may have \
                 found nothing",
                quote(ctx.source, m.at),
            ),
            format!(
                "Test `IF AVAILABLE {buffer} THEN` before the reference (or leave when it is \
                 not), or drop `NO-ERROR` when the record must exist."
            ),
            m.at,
        ));
    }
    diags
}
