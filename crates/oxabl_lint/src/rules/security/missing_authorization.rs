//! `missing-authorization` (ABLSEC011), group `security`.
//!
//! Fires where an entry point reaches a sensitive operation on a path that
//! passes no authorisation check. A caller who can reach the entry runs the
//! operation, whoever they are.
//!
//! ```abl
//! /* web/orders.p, the REST entry DELETE /orders/{id} */
//! FIND Order EXCLUSIVE-LOCK WHERE Order.OrderNum = INTEGER(get-value("id")).
//! DELETE Order.
//! ```
//!
//! Fixed — check before the operation, with a routine named in
//! `auth-routines`:
//!
//! ```abl
//! RUN auth/require-role.p (INPUT "order-admin").
//! FIND Order EXCLUSIVE-LOCK WHERE Order.OrderNum = INTEGER(get-value("id")).
//! DELETE Order.
//! ```
//!
//! # What it needs
//!
//! The entry points (`[workspace.lint.security.reach] entry-points`) and the
//! routines that authorise (`auth-routines`): a procedure, function, method or
//! program whose call, or an include file whose inclusion, counts as the check.
//! An entry marked `"auth": true` is authorised by its framework. Without
//! entry points the rule is silent; without `auth-routines` every reached
//! sensitive operation is reported.
//!
//! # Sensitive operations
//!
//! A database write the effect analysis finds (`CREATE`, `DELETE`, an
//! assignment to a field; temp-tables are not), and a call of a
//! `sensitive-calls` routine.
//!
//! # When a check counts
//!
//! A check counts for the rest of a routine when its statement comes before
//! the operation on every path through the routine (it dominates it), and it
//! counts for a called routine when it comes before the call. One path from an
//! entry that passes no check is a finding. A check is trusted by name: one
//! that returns a result the caller ignores still counts.
//!
//! One finding per routine, at its first sensitive operation that no check
//! covers. A routine that no entry is known to reach is not reported.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::{ABLSEC011, finding};
use crate::registry::{RuleMeta, Stability};
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "missing-authorization";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "An entry point reaches a database write or a sensitive call with no authorisation check";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Check that the caller may do this before the operation, on every path \
     from the entry point, with a routine named in `auth-routines`; or mark the entry \
     `\"auth\": true` when its framework checks it.";

/// Registry metadata. Not context-free: the answer depends on the files that
/// reach this one.
pub const META: RuleMeta = RuleMeta::new(ABLSEC011, NAME, "security", DESCRIPTION)
    .with_default_severity(DEFAULT_SEVERITY)
    .with_stability(Stability::Preview)
    .with_help(HELP)
    .with_security_severity(SECURITY_SEVERITY);

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, ctx, &RunShared::default())
}

/// [`run`] with the facts of a registry run.
pub fn check(program: &[Statement], ctx: &AnalysisContext, shared: &RunShared) -> Vec<Diagnostic> {
    let Some(reach) = shared.reach() else {
        return Vec::new();
    };
    if !ctx.lint_severities.security().reach.configured {
        return Vec::new();
    }
    let facts = shared.reach_facts(program, ctx);
    let mut out = Vec::new();
    for unit in &facts.units {
        let Some(op) = unit.sensitive.iter().find(|s| !s.authorised) else {
            continue;
        };
        let Some(path) = reach.unauthorised(unit.span.start) else {
            continue;
        };
        let here = unit.name.as_deref().unwrap_or("main block");
        out.push(finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC011,
            op.at,
            format!(
                "{} runs with no authorisation check on the path {}",
                op.what,
                path.describe(&format!("`{here}`"))
            ),
            HELP,
        ));
    }
    out
}
