//! `unknown-in-comparison` (NL001), group `strict.nulls`.
//!
//! An unknown condition can select the ELSE branch or stop a loop. Test for `?` before the comparison.
//!
//! ```abl
//! DEFINE VARIABLE d AS DATE NO-UNDO.
//! IF d < TODAY THEN MESSAGE "old".
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE d AS DATE NO-UNDO.
//! IF d <> ? AND d < TODAY THEN MESSAGE "old".
//! ```
//!
//! The rule requires a known possible source of `?`. Unseen calls and input
//! parameters are Unknown and produce no finding. Fields are outside this rule.
//! Ordering and logical results remain Unknown. A comparison requires a separate
//! known possible source of `?` in its operand. A variable is reported once per
//! unit, at its first use.
//!
//! The finding relies on two recalled rules that are not yet checked on an
//! OpenEdge install: an ordering comparison with `?` gives `?`, and a `?`
//! condition skips the `THEN` branch or ends the loop. The rule stays in
//! preview until they are.

use super::handle_support::warning;
use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_nulls::FileNulls;
use oxabl_scope::FileScopes;
use oxabl_semantic::AnalysisContext;

/// Rule name.
pub const NAME: &str = "unknown-in-comparison";
/// Opt-in rule group.
pub const GROUP: &str = "strict.nulls";
/// Default severity.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Rule description.
pub const DESCRIPTION: &str =
    "A possibly unknown variable used in an ordering comparison or condition";

/// Run the rule over a parsed file.
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    let shared = crate::RunShared::default();
    check(
        shared.null_values(program, ctx),
        shared.scopes(program, ctx),
        program,
        ctx,
    )
}

/// Read findings from the shared null analysis.
pub fn check(
    nulls: &FileNulls,
    _scopes: &FileScopes,
    _program: &[Statement],
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    nulls
        .first_values()
        .filter(|u| !u.propagation)
        .map(|u| {
            warning(
                ctx,
                super::NL001,
                format!(
                    "`{}` may hold `?`: the comparison or condition can skip code",
                    u.name
                ),
                format!(
                    "Test `{} <> ?` before this use, or assign a known value first. This assumes \
                     an ordering comparison with `?` gives `?` and a `?` condition acts as false, \
                     which is not yet checked on OpenEdge.",
                    u.name
                ),
                u.at,
            )
        })
        .collect()
}
