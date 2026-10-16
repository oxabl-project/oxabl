//! `unknown-propagation` (NL002), group `strict.nulls`.
//!
//! Arithmetic and string concatenation propagate `?`. The whole result becomes unknown. Test the variable before use.
//!
//! ```abl
//! DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.
//! MESSAGE n + 1.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.
//! IF n <> ? THEN MESSAGE n + 1.
//! ```
//!
//! The rule requires a known possible source of `?`. Unseen calls and input
//! parameters are Unknown and produce no finding. Fields are outside this rule.
//! A variable is reported once per unit, at its first use.

use super::handle_support::warning;
use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_nulls::FileNulls;
use oxabl_scope::FileScopes;
use oxabl_semantic::AnalysisContext;

/// Rule name.
pub const NAME: &str = "unknown-propagation";
/// Opt-in rule group.
pub const GROUP: &str = "strict.nulls";
/// Default severity.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Rule description.
pub const DESCRIPTION: &str = "A possibly unknown variable in arithmetic stored or displayed";

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
        .filter(|u| u.propagation)
        .map(|u| {
            warning(
                ctx,
                super::NL002,
                format!(
                    "`{}` may hold `?`: the arithmetic result becomes unknown",
                    u.name
                ),
                format!(
                    "Test `{} <> ?` before this use, or assign a known value first.",
                    u.name
                ),
                u.at,
            )
        })
        .collect()
}
