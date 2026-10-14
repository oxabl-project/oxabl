//! `run-value` lint (CL001), group `strict.calls`.
//!
//! Fires on `RUN VALUE(expr)`: a `RUN` whose target is computed at run time.
//! That covers every spelling of the statement — `RUN VALUE(x) IN h`,
//! `PERSISTENT`, `ASYNCHRONOUS`, `ON SERVER`, `NO-ERROR` and arguments change
//! nothing about the target being dynamic.
//!
//! # Why
//!
//! A static `RUN prog.p` names its callee. A tool can resolve it over PROPATH,
//! check its parameters, and follow it when it asks what a procedure reads or
//! writes. `RUN VALUE(cProg)` names nothing: the callee is whatever the
//! variable holds when the line runs, so every analysis that crosses the call
//! has to give up. Code that is read by searching for a procedure name also
//! stops showing up in the results.
//!
//! ```abl
//! DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.
//! cProg = "post-" + cKind + ".p".
//! RUN VALUE(cProg) (INPUT iOrder).
//! ```
//!
//! Fixed — the target is visible, and an unknown kind is an explicit case:
//!
//! ```abl
//! CASE cKind:
//!     WHEN "invoice" THEN RUN post-invoice.p (INPUT iOrder).
//!     WHEN "credit"  THEN RUN post-credit.p (INPUT iOrder).
//!     OTHERWISE UNDO, THROW NEW Progress.Lang.AppError("unknown kind", 1).
//! END CASE.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per `RUN VALUE` statement, anchored on the target
//! expression. A `RUN` with a literal target — quoted or not, with or without
//! `IN handle` — is never reported, even when the handle is dynamic: the
//! *procedure* is still named. The same goes for `RUN VALUE("prog.p")`, whose
//! argument is a bare string literal. Any other argument, including a
//! concatenation of literals, is reported. Dispatch through `DYNAMIC-FUNCTION` and the
//! other dynamic calls belongs to `dynamic-invoke` (CL002).
//!
//! `ON ... PERSISTENT RUN proc` only accepts a procedure name, so it cannot
//! be dynamic. `RUN VALUE` inside a statement the parser does not model
//! (`StatementKind::Skipped`) cannot be seen; `RUN` itself is always modelled,
//! so in practice nothing is lost.

use oxabl_ast::{ExpressionKind, Literal, RunTarget, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::CL001;
use super::syntactic::{Visitor, file_span, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "run-value";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.calls";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str = "RUN VALUE(...) calls a procedure chosen at run time";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = RunValues {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct RunValues<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl Visitor for RunValues<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        if let StatementKind::Run {
            target: RunTarget::Dynamic(target),
            ..
        } = &stmt.kind
        {
            // `RUN VALUE("prog.p")` still names its callee.
            if matches!(&target.kind, ExpressionKind::Literal(Literal::String(_))) {
                return true;
            }
            let span = if target.span.start < target.span.end {
                target.span
            } else {
                stmt.span
            };
            self.diags.push(
                Diagnostic::warning(
                    CL001,
                    "`RUN VALUE(...)` calls a procedure chosen at run time".to_string(),
                    file_span(self.ctx, span),
                )
                .with_help(
                    "Name the procedure in a static `RUN`, or choose between known procedures \
                     with a `CASE`, so analysis can follow the call."
                        .to_string(),
                ),
            );
        }
        true
    }
}
