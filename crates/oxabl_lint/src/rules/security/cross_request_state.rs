//! `cross-request-state` (ABLSEC012), group `security`.
//!
//! Fires where code that an entry point reaches reads session state before
//! the request sets it. An application server agent serves many requests,
//! and session state lives as long as the agent: a `NEW GLOBAL SHARED`
//! variable, a `STATIC` member of a class. A request that reads it first sees
//! what an earlier request, perhaps of another user, left there.
//!
//! ```abl
//! DEFINE NEW GLOBAL SHARED VARIABLE gcCompany AS CHARACTER NO-UNDO.
//! FOR EACH Order NO-LOCK WHERE Order.Company = gcCompany: /* the last caller's company */
//! ```
//!
//! Fixed — set the state at the start of each request, from the request's
//! own session, or keep it in a variable of the request:
//!
//! ```abl
//! gcCompany = session-company().
//! FOR EACH Order NO-LOCK WHERE Order.Company = gcCompany:
//! ```
//!
//! # What counts
//!
//! A piece of session state is cross-request when code reached from the
//! entry points writes it and code reached from them reads it. A read is
//! reported when an entry reaches its routine on a path that writes the state
//! nowhere before (a write counts when it comes before the call, or before
//! the read in the same routine, on every path). A `SHARED` variable is the
//! `NEW GLOBAL SHARED` one of the same name; a static member is named by its
//! class's last name segment and its own name (`Cache:Last` and
//! `app.Cache:Last` are one).
//!
//! One finding per piece of state and routine. Without entry points the rule
//! is silent.
//!
//! # What is not seen
//!
//! State held inside an object that a static property holds (a singleton's
//! own variables), persistent procedures that keep state in their own
//! variables, and `SESSION` attributes.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::{ABLSEC012, finding};
use crate::registry::{RuleMeta, Stability};
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "cross-request-state";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "5.5";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A request reads session state that an earlier request may have written";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Set the state at the start of every request that reads it, from that \
     request's own session, or keep it in a variable that lives only for the request.";

/// Registry metadata. Not context-free: the answer depends on the files that
/// reach this one.
pub const META: RuleMeta = RuleMeta::new(ABLSEC012, NAME, "security", DESCRIPTION)
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
        let mut seen: Vec<&str> = Vec::new();
        for read in unit.reads.iter().filter(|r| !r.reset) {
            if seen.contains(&read.key.as_str()) {
                continue;
            }
            let Some(path) = reach.unreset(unit.span.start, &read.key) else {
                continue;
            };
            seen.push(&read.key);
            let here = reach
                .routine(unit.span.start)
                .or(unit.name.as_deref())
                .unwrap_or("main block");
            out.push(finding(
                ctx,
                DEFAULT_SEVERITY,
                ABLSEC012,
                read.at,
                format!(
                    "`{}` is session state that the request reads before it sets it, so it can \
                     hold what an earlier request left; path {}",
                    read.name,
                    path.describe(&format!("`{here}`"))
                ),
                HELP,
            ));
        }
    }
    out
}
