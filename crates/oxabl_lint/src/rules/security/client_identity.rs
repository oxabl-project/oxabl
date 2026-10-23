//! `client-identity` (ABLSEC010), group `security`.
//!
//! Fires where a decision reads a user, company or role that the client sent
//! with the request. The client can send any value it likes, so a check of
//! that value decides nothing: the caller names itself an administrator, or
//! asks for another company's records.
//!
//! ```abl
//! IF get-value("role") = "admin" THEN RUN purge-orders.
//! FOR EACH Order NO-LOCK WHERE Order.Company = get-field("company"):
//! ```
//!
//! Fixed — take the identity from the session that authenticated the caller:
//!
//! ```abl
//! hPrincipal = SESSION:CURRENT-REQUEST-INFO:GetClientPrincipal().
//! IF CAN-DO(hPrincipal:ROLES, "admin") THEN RUN purge-orders.
//! ```
//!
//! # Identity
//!
//! A request value is an identity when the key it is read by has a word of
//! `identity-keys` (`[workspace.lint.security]`; default `user`, `username`,
//! `userid`, `login`, `company`, `tenant`, `role`, `roles`, `owner`,
//! `admin`): `get-value("user")`, `get-field("company-id")`,
//! `poRequest:GetPathParameter("tenant")`, and `o:GetCharacter("userId")` on
//! an object that holds request input. `identity-sources` names project
//! functions whose result is the client's claim. The value is followed with
//! the taint analysis (in every mode), through variables, fields and the
//! routines of the file. A comparison, a number conversion or a guard does
//! not clean it: the checked value is still the client's. A validator that
//! `protects = ["identity"]` does.
//!
//! # Decisions
//!
//! The condition of `IF`, `CASE`, `DO WHILE` and `REPEAT WHILE`, the `WHERE`
//! of `FOR EACH`, `FIND` and `CAN-FIND`, and an argument of an
//! `auth-routines` routine (`[workspace.lint.security.reach]`). A value that
//! is only displayed or stored is not reported.
//!
//! # What is not reported
//!
//! The identity of the session (`USERID`, the client principal), a decision
//! the analysis cannot judge (an unexpanded reference, a unit past its work
//! budget), and any decision in a file whose parse recovered.

use oxabl_ast::{Expression, ExpressionKind, RunTarget, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::reach::names_routine;
use super::taint::{Gate, SinkKind};
use super::{ABLSEC010, finding, meta};
use crate::registry::RuleMeta;
use crate::rules::syntactic::{Visitor, walk_block};
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "client-identity";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A decision reads a user, company or role that the client sent with the request";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Take the user, company and role from the session that authenticated the \
     caller (the client principal), never from a request parameter, header or body.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC010,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, ctx, &RunShared::default())
}

/// [`run`] with the facts of a registry run.
pub fn check(program: &[Statement], ctx: &AnalysisContext, shared: &RunShared) -> Vec<Diagnostic> {
    let mut v = Decisions {
        ctx,
        gate: Gate::new(ABLSEC010, NAME, program, ctx, shared),
        auth: &ctx.lint_severities.security().reach.auth_routines,
        reported: Vec::new(),
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct Decisions<'a> {
    ctx: &'a AnalysisContext<'a>,
    gate: Gate<'a>,
    auth: &'a [String],
    /// The operands of the decisions reported: a `CAN-FIND` inside one is not
    /// reported again.
    reported: Vec<Span>,
    diags: Vec<Diagnostic>,
}

impl Decisions<'_> {
    /// Report the decision whose keyword is `word` at `at` when an identity
    /// reaches one of `operands`.
    fn decide(&mut self, at: u32, word: &str, operands: &[Span]) {
        let inside = |s: &Span| s.start <= at && at < s.end;
        if self.reported.iter().any(inside) {
            return;
        }
        let Some(j) = self.gate.judge_decision(operands, SinkKind::Identity) else {
            return;
        };
        self.reported.extend_from_slice(operands);
        let span = Span {
            start: at,
            end: at + word.len() as u32,
        };
        let d = finding(
            self.ctx,
            DEFAULT_SEVERITY,
            ABLSEC010,
            span,
            format!(
                "`{word}` decides on an identity the client supplied: {}",
                j.chain
            ),
            HELP,
        );
        self.diags.push(j.attach(d));
    }

    fn is_auth(&self, name: &str) -> bool {
        self.auth.iter().any(|p| names_routine(p, &[name]))
    }
}

impl Visitor for Decisions<'_> {
    fn statement(&mut self, s: &Statement) -> bool {
        let at = s.span.start;
        match &s.kind {
            StatementKind::If { condition, .. } => self.decide(at, "IF", &[condition.span]),
            StatementKind::Case { expression, .. } => self.decide(at, "CASE", &[expression.span]),
            StatementKind::Do {
                while_condition: Some(w),
                ..
            } => self.decide(at, "DO", &[w.span]),
            StatementKind::Repeat {
                while_condition: Some(w),
                ..
            } => self.decide(at, "REPEAT", &[w.span]),
            StatementKind::ForEach {
                where_clause: Some(w),
                ..
            } => self.decide(at, "FOR", &[w.span]),
            StatementKind::Find {
                where_clause: Some(w),
                ..
            } => self.decide(at, "FIND", &[w.span]),
            StatementKind::Run {
                target: RunTarget::Literal { name, .. },
                arguments,
                ..
            } if self.is_auth(name) => {
                let spans: Vec<Span> = arguments.iter().map(|a| a.expression.span).collect();
                self.decide(at, "RUN", &spans);
            }
            _ => {}
        }
        true
    }

    fn expression(&mut self, e: &Expression) {
        match &e.kind {
            ExpressionKind::CanFind {
                where_clause: Some(w),
                ..
            } => self.decide(e.span.start, "CAN-FIND", &[w.span]),
            ExpressionKind::FunctionCall { name, arguments } if self.is_auth(&name.name) => {
                let spans: Vec<Span> = arguments.iter().map(|a| a.span).collect();
                let word = name.name.clone();
                self.decide(e.span.start, &word, &spans);
            }
            _ => {}
        }
    }
}
