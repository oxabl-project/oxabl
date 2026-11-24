//! `information-leak` (ABLSEC013), group `security`.
//!
//! Fires where the text of an error leaves the server, and where a secret
//! goes to a message or a log. Error text holds table and field names, file
//! paths, SQL fragments and call stacks, which help an attacker plan the next
//! request. A password in a log is readable by everyone with access to the log.
//!
//! # The set
//!
//! Error detail is `ERROR-STATUS:GET-MESSAGE(n)`, `err:GetMessage(n)` and
//! `err:CallStack`, where `err` is a `CATCH` variable, a variable or parameter
//! typed as one of the `...Error` classes. It is reported in two places, and
//! only in the same statement (stage 1 does not follow a variable):
//!
//! | Sink | Form |
//! |---|---|
//! | an output parameter | `pcError = <detail>`, `ASSIGN pcError = <detail>`, where `pcError` is an `OUTPUT` or `INPUT-OUTPUT` parameter of a procedure, function, method or constructor in the file |
//! | a web response | `{&OUT} ... <detail>`, `PUT STREAM WebStream ... <detail>`, `writer:Write(<detail>)`, `response:Entity = <detail>` |
//!
//! A secret is a password-like name (see [`names`](super::names)) in:
//!
//! | Sink | Form |
//! |---|---|
//! | the message box or log | `MESSAGE ... name ...` |
//! | the server log | `LOG-MANAGER:WRITE-MESSAGE(... name ...)` |
//!
//! ```abl
//! CATCH e AS Progress.Lang.Error:
//!     pcError = e:GetMessage(1).
//! END CATCH.
//! ```
//!
//! Fixed — a generic message to the client, and the detail in the server log:
//!
//! ```abl
//! CATCH e AS Progress.Lang.Error:
//!     LOG-MANAGER:WRITE-MESSAGE(e:GetMessage(1)).
//!     pcError = "The request failed".
//! END CATCH.
//! ```
//!
//! # What is not reported
//!
//! Error detail kept in a local variable (`cMsg = e:GetMessage(1)`) or sent to
//! `MESSAGE` or the log is not reported: the detail has to reach a client in
//! the same statement. `MESSAGE` and `LOG-MANAGER` are sinks for secrets only.
//!
//! # Where the rule looks
//!
//! The set of `OUTPUT` parameter names and of error variables is read once for
//! the whole file, so a procedure that reuses the name of another procedure's
//! output parameter for a local variable is reported (a false positive). The
//! web sinks are those of [`xss-output`](super::xss_output), picked by the
//! name of the object. A call that is made through a helper
//! (`sendError(e:GetMessage(1))`) is not seen.
//!
//! Code that embeds the linter can pass its own word lists to [`run_with`].

use std::collections::HashSet;

use oxabl_ast::{
    DataType, Expression, ExpressionKind, ParameterDirection, ParameterType, Span, Statement,
    StatementKind, TypeSource,
};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Site, any_expr, arg_of, last_name, scan};
use super::literal::cover;
use super::names::{Heuristics, defaults};
use super::web::{StreamWrite, is_response, stream_write};
use super::{ABLSEC013, finding, meta};
use crate::registry::RuleMeta;
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "information-leak";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "5.5";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "error text reaches a client, or a password-like value goes to MESSAGE or the log";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Return a generic error to the client and write the detail to the server log. \
     Never pass a password, key or token to `MESSAGE` or `LOG-MANAGER:WRITE-MESSAGE`; log that \
     an event happened, not the secret.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC013,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    run_with(program, ctx, defaults())
}

/// Entry point with the word lists of `heuristics` for password-like names.
pub fn run_with(
    program: &[Statement],
    ctx: &AnalysisContext,
    heuristics: &Heuristics,
) -> Vec<Diagnostic> {
    let mut vars = Variables::default();
    walk_block(&mut vars, program);

    let mut diags = Vec::new();
    let mut report = |span: Span, message: String| {
        diags.push(finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC013,
            span,
            message,
            HELP,
        ));
    };
    scan(ctx, program, &["WRITE-MESSAGE"], &mut |site| match site {
        Site::Statement(stmt) => statement(ctx, stmt, &vars, heuristics, &mut report),
        Site::Call(call) => {
            if call.is("WRITE-MESSAGE")
                && call.receiver_is("LOG-MANAGER")
                && let Some(name) = call
                    .args
                    .iter()
                    .flat_map(|a| a.names.iter())
                    .find(|n| heuristics.is_secret_value(n))
            {
                report(
                    call.span,
                    format!("`{name}` is a password-like value written to the log"),
                );
            }
        }
    });
    diags
}

/// What the file declares: the parameters that return values, and the
/// variables that hold an error.
#[derive(Default)]
struct Variables {
    outputs: HashSet<String>,
    errors: HashSet<String>,
}

impl Variables {
    fn parameter(&mut self, stmt: &Statement) {
        if let StatementKind::DefineParameter {
            direction,
            param_type:
                ParameterType::Variable {
                    name, type_source, ..
                },
        } = &stmt.kind
        {
            if matches!(
                direction,
                ParameterDirection::Output | ParameterDirection::InputOutput
            ) {
                self.outputs.insert(name.name.to_ascii_lowercase());
            }
            if is_error_type(type_source) {
                self.errors.insert(name.name.to_ascii_lowercase());
            }
        }
    }

    fn is_error(&self, name: &str) -> bool {
        self.errors.contains(&name.to_ascii_lowercase())
    }

    fn is_output(&self, name: &str) -> bool {
        self.outputs.contains(&name.to_ascii_lowercase())
    }
}

impl Visitor for Variables {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::DefineParameter { .. } => self.parameter(stmt),
            StatementKind::Method { parameters, .. }
            | StatementKind::Constructor { parameters, .. } => {
                parameters.iter().for_each(|p| self.parameter(p));
            }
            StatementKind::Function {
                prototype_parameters,
                ..
            } => prototype_parameters.iter().for_each(|p| self.parameter(p)),
            StatementKind::Catch { error_var, .. } => {
                self.errors.insert(error_var.name.to_ascii_lowercase());
            }
            StatementKind::VariableDeclaration {
                name, type_source, ..
            } if is_error_type(type_source) => {
                self.errors.insert(name.name.to_ascii_lowercase());
            }
            _ => {}
        }
        true
    }
}

/// Whether the type is a `...Error` class: `Progress.Lang.Error`, `AppError`,
/// `SysError`, `ProError`, `StopError`.
fn is_error_type(source: &TypeSource) -> bool {
    matches!(source, TypeSource::Explicit(DataType::Class(name))
        if name.rsplit('.').next().is_some_and(|c| c.to_ascii_lowercase().ends_with("error")))
}

/// Whether `expr` holds error text: `ERROR-STATUS:GET-MESSAGE(n)`,
/// `err:GetMessage(n)` or `err:CallStack`.
fn has_detail(expr: &Expression, vars: &Variables) -> bool {
    any_expr(expr, &mut |e| match &e.kind {
        ExpressionKind::MethodCall { object, method, .. } => {
            let receiver = last_name(object).unwrap_or("");
            (method.name.eq_ignore_ascii_case("GET-MESSAGE")
                && receiver.eq_ignore_ascii_case("ERROR-STATUS"))
                || (method.name.eq_ignore_ascii_case("GetMessage") && vars.is_error(receiver))
        }
        ExpressionKind::MemberAccess { member, .. } => {
            member.name.eq_ignore_ascii_case("CallStack")
        }
        _ => false,
    })
}

/// Whether the tokens that a stream write puts out hold error text.
fn tokens_have_detail(write: &StreamWrite, vars: &Variables) -> bool {
    let st = &write.text;
    write.args.clone().any(|i| {
        let after_colon = i > 0 && st.is(i - 1, Kind::Colon);
        (st.is_word(i, "ERROR-STATUS")
            && st.is(i + 1, Kind::Colon)
            && st.is_word(i + 2, "GET-MESSAGE"))
            || (st.is_word(i, "CallStack") && after_colon)
            || (st.is_word(i, "GetMessage")
                && after_colon
                && i >= 2
                && vars.is_error(st.word(i - 2)))
    })
}

fn statement(
    ctx: &AnalysisContext,
    stmt: &Statement,
    vars: &Variables,
    heuristics: &Heuristics,
    report: &mut impl FnMut(Span, String),
) {
    if let Some(write) = stream_write(ctx.source, stmt) {
        if tokens_have_detail(&write, vars) {
            report(
                write.text.span(0..write.text.len()),
                "error text is written to the web response".to_string(),
            );
        }
        return;
    }
    match &stmt.kind {
        StatementKind::Assignment { target, value } => {
            assignment(stmt, target, value, vars, report);
        }
        StatementKind::Assign { assignments } => {
            for pair in assignments {
                assignment(stmt, &pair.target, &pair.value, vars, report);
            }
        }
        StatementKind::ExpressionStatement(expr) => {
            if let ExpressionKind::MethodCall {
                object,
                method,
                arguments,
            } = &expr.kind
                && method.name.eq_ignore_ascii_case("Write")
                && last_name(object).is_some_and(is_response)
                && arguments.iter().any(|a| has_detail(a, vars))
            {
                report(
                    cover(method.span, expr.span, expr.span),
                    "error text is written to the web response".to_string(),
                );
            }
        }
        StatementKind::Message { items, .. } => {
            let secret = items
                .iter()
                .flat_map(|item| arg_of(item).names)
                .find(|n| heuristics.is_secret_value(n));
            if let Some(name) = secret {
                report(
                    stmt.span,
                    format!("`{name}` is a password-like value passed to `MESSAGE`"),
                );
            }
        }
        _ => {}
    }
}

fn assignment(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    vars: &Variables,
    report: &mut impl FnMut(Span, String),
) {
    let Some(name) = last_name(target) else {
        return;
    };
    if !has_detail(value, vars) {
        return;
    }
    let span = cover(target.span, value.span, stmt.span);
    if vars.is_output(name) {
        report(
            span,
            format!("`{name}` is an output parameter and receives error text"),
        );
    } else if let ExpressionKind::MemberAccess { object, member } = &target.kind
        && member.name.eq_ignore_ascii_case("Entity")
        && last_name(object).is_some_and(is_response)
    {
        report(
            span,
            "error text is written to the web response".to_string(),
        );
    }
}
