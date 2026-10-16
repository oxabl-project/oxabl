//! `nullable-db-field` (NL004), group `strict.nulls`.
//!
//! A field the schema starts at `?` and does not make `MANDATORY` holds `?` in
//! every record created without a value for it. Used in a comparison or in
//! arithmetic, it can skip code or make the whole result unknown. Test it for
//! `?` first.
//!
//! ```abl
//! FIND FIRST Parcel NO-LOCK NO-ERROR.
//! IF AVAILABLE Parcel AND Parcel.ShipDate < TODAY THEN MESSAGE "late".
//! ```
//!
//! Fixed:
//!
//! ```abl
//! FIND FIRST Parcel NO-LOCK NO-ERROR.
//! IF AVAILABLE Parcel AND Parcel.ShipDate <> ? AND Parcel.ShipDate < TODAY THEN MESSAGE "late".
//! ```
//!
//! The example assumes a schema in which `Parcel.ShipDate` is a `DATE` with
//! `INITIAL ?` and no `MANDATORY`. The rule judges a field of the loaded
//! schema only, so it is silent with no schema. A field that is `MANDATORY`,
//! whose `INITIAL` is not `?`, or that has an extent is never a source. The
//! same guards and sinks apply as for a local variable (NL001, NL002). A
//! record that is read or created refills its fields, so a test before a
//! `FIND` or a loop pass does not cover the record after it; a write to the
//! field, a write to the whole record, a buffer passed to a call, and a call
//! that may change a shared buffer leave the field Unknown. The `WHERE` of
//! the `FIND` or `FOR EACH` that read the record holds of its fields. A
//! field is reported once per unit and kind, at its first use.
//!
//! The comparison finding relies on two recalled rules that are not yet
//! checked on an OpenEdge install: an ordering comparison with `?` gives `?`,
//! and a `?` condition skips the `THEN` branch or ends the loop. The rule stays
//! in preview.

use super::handle_support::warning;
use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_nulls::FileNulls;
use oxabl_scope::FileScopes;
use oxabl_semantic::AnalysisContext;

/// Rule name.
pub const NAME: &str = "nullable-db-field";
/// Opt-in rule group.
pub const GROUP: &str = "strict.nulls";
/// Default severity.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Rule description.
pub const DESCRIPTION: &str =
    "A database field that starts at `?` and is not mandatory, used without a test for `?`";

/// Run the rule over a parsed file.
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let shared = crate::RunShared::default();
    check(
        shared.null_fields(program, ctx),
        shared.scopes(program, ctx),
        program,
        ctx,
    )
}

/// Read findings from the shared field analysis.
pub fn check(
    nulls: &FileNulls,
    _scopes: &FileScopes,
    _program: &[Statement],
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    nulls
        .first_field_values()
        .map(|u| {
            let (effect, caveat) = if u.propagation {
                ("the arithmetic result becomes unknown", "")
            } else {
                (
                    "the comparison or condition can skip code",
                    " This assumes an ordering comparison with `?` gives `?` and a `?` \
                     condition acts as false, which is not yet checked on OpenEdge.",
                )
            };
            warning(
                ctx,
                super::NL004,
                format!("`{}` may hold `?`: {effect}", u.name),
                format!(
                    "The schema starts this field at `?` and does not make it MANDATORY. Test \
                     `{} <> ?` before this use, or assign a known value first.{caveat}",
                    u.name
                ),
                u.at,
            )
        })
        .collect()
}
