//! `header-injection` (ABLSEC007), group `security`.
//!
//! Fires where a response header, a cookie or a redirect target is set from a
//! value that is not a literal. A value with a line break in it ends the
//! header and starts another, or starts the body (response splitting). A
//! redirect target that the caller chose sends the browser to any site (open
//! redirect).
//!
//! # The set
//!
//! | Form | Value read |
//! |---|---|
//! | `obj:SetHeader(...)`, `AddHeader`, `SetCookie`, `AddCookie`, `SetLocation` | every argument |
//! | `NEW ...HttpHeader(...)`, `NEW ...Cookie(...)` | every argument |
//! | `HttpHeaderBuilder:Build(name)`, `...:Value(value)` | the name, the value |
//! | `obj:Location = expr` | the assigned value |
//! | `RUN output-http-header (...)`, `RUN set-cookie (...)` | every argument |
//!
//! ```abl
//! oResponse:SetHeader("Location", pcNext).
//! ```
//!
//! With `pcNext` set to `/home~r~nSet-Cookie: sid=1`, the response carries a
//! cookie the caller chose.
//!
//! Fixed — strip CR and LF (or reject the value), and redirect only to a
//! literal or to a target chosen from a fixed list:
//!
//! ```abl
//! CASE pcNext:
//!     WHEN "home"    THEN oResponse:SetHeader("Location", "/home").
//!     WHEN "account" THEN oResponse:SetHeader("Location", "/account").
//! END CASE.
//! ```
//!
//! # What is not reported
//!
//! A header object that is built in the call, `SetHeader(NEW HttpHeader("X",
//! cValue))`, is reported once, at the `NEW`. An object that is built
//! elsewhere and passed in (`SetHeader(oHeader)`) is not a literal and is
//! reported at the call.
//!
//! # Where the rule looks
//!
//! The method names are matched by spelling, since the parser has no types: a
//! user method named `SetHeader` on another object is checked too. The rule
//! reads modelled statements and expressions, and by lexing, the calls inside
//! statements the parser does not model. A header written through
//! `{&OUT}` or `PUT` before the body is not seen, and neither is a redirect
//! that a `RUN` argument carries to a procedure the rule does not name.

use oxabl_ast::{Expression, ExpressionKind, RunTarget, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Arg, Call, CallKind, Site, arg_of, scan};
use super::literal::cover;
use super::taint::{Gate, SinkKind};
use super::{ABLSEC007, finding, meta};
use crate::registry::RuleMeta;
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "header-injection";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "5.5";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "a response header, cookie or redirect target is set from a value that is not a literal";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Strip CR and LF from the value, or reject it, before it reaches a header or cookie. \
     Redirect only to a literal or to a target that a `CASE` or an allow-list picks from an input \
     key, never to text a caller supplied.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC007,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// Methods that set a header, cookie or redirect from their arguments.
const METHODS: [&str; 5] = [
    "SetHeader",
    "AddHeader",
    "SetCookie",
    "AddCookie",
    "SetLocation",
];

/// Classes whose constructor arguments become a header or cookie.
const OBJECTS: [&str; 3] = ["HttpHeader", "Cookie", "HttpCookie"];

/// WebSpeed procedures that write a header.
const PROCEDURES: [&str; 2] = ["output-http-header", "set-cookie"];

fn is_any(name: &str, list: &[&str]) -> bool {
    list.iter().any(|n| n.eq_ignore_ascii_case(name))
}

/// Whether `arg` is not constant and is not just a header object that is
/// built in place (and reported there).
fn dynamic(arg: &Arg) -> bool {
    !arg.constant && !arg.calls.iter().any(|c| is_any(c, &OBJECTS))
}

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, ctx, &RunShared::default())
}

/// [`run`] with the facts of a registry run.
pub fn check(program: &[Statement], ctx: &AnalysisContext, shared: &RunShared) -> Vec<Diagnostic> {
    let gate = Gate::new(ABLSEC007, NAME, program, ctx, shared);
    let mut diags = Vec::new();
    let mut report = |span: Span, what: &str, operands: &[Span]| {
        let Some(j) = gate.judge(operands, SinkKind::Header) else {
            return;
        };
        let d = finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC007,
            span,
            format!("`{what}` sets a header or redirect from {}", j.subject),
            HELP,
        );
        diags.push(j.attach(d));
    };
    scan(
        ctx,
        program,
        &["Header", "Cookie", "Location", "Build", "Value"],
        &mut |site| match site {
            Site::Statement(stmt) => statement(stmt, &mut report),
            Site::Call(call) => {
                if let Some(what) = sink(&call) {
                    let operands: Vec<Span> = call
                        .args
                        .iter()
                        .take(if what == "Build" { 1 } else { usize::MAX })
                        .filter(|a| dynamic(a))
                        .map(|a| a.span)
                        .collect();
                    report(call.span, what, &operands);
                }
            }
        },
    );
    diags
}

/// The name to show when `call` sets a header from a non-literal.
fn sink(call: &Call) -> Option<&'static str> {
    match call.kind {
        CallKind::Method | CallKind::Function => {
            if let Some(name) = METHODS.iter().find(|m| call.is(m)) {
                return call.args.iter().any(dynamic).then_some(*name);
            }
            if call.is("Build") && call.receiver_is("HttpHeaderBuilder") {
                return call.args.first().is_some_and(dynamic).then_some("Build");
            }
            if call.is("Value") && call.receiver_is("Build") {
                return call.args.iter().any(dynamic).then_some("Value");
            }
            None
        }
        CallKind::New => {
            let name = OBJECTS.iter().find(|o| call.is(o))?;
            call.args.iter().any(dynamic).then_some(*name)
        }
    }
}

fn statement(stmt: &Statement, report: &mut impl FnMut(Span, &str, &[Span])) {
    match &stmt.kind {
        StatementKind::Assignment { target, value } => location(stmt, target, value, report),
        StatementKind::Assign { assignments } => {
            for pair in assignments {
                location(stmt, &pair.target, &pair.value, report);
            }
        }
        StatementKind::Run {
            target: RunTarget::Literal { name, .. },
            arguments,
            ..
        } if is_any(name, &PROCEDURES)
            && arguments.iter().any(|a| !arg_of(&a.expression).constant) =>
        {
            let operands: Vec<Span> = arguments
                .iter()
                .filter(|a| !arg_of(&a.expression).constant)
                .map(|a| a.expression.span)
                .collect();
            report(stmt.span, name, &operands);
        }
        _ => {}
    }
}

fn location(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    report: &mut impl FnMut(Span, &str, &[Span]),
) {
    if let ExpressionKind::MemberAccess { member, .. } = &target.kind
        && member.name.eq_ignore_ascii_case("Location")
        && !arg_of(value).constant
    {
        report(
            cover(target.span, value.span, stmt.span),
            "Location",
            &[value.span],
        );
    }
}
