//! `xss-output` (ABLSEC006), group `security`.
//!
//! Fires where a value that is not a literal is written into the body of a web
//! response. Text that reaches the body unencoded can carry markup or script
//! that runs in the browser of the next reader.
//!
//! # The set
//!
//! | Form | Value read |
//! |---|---|
//! | `{&OUT} ...` | every item written |
//! | `PUT STREAM WebStream [UNFORMATTED] ...` | every item written |
//! | `writer:Write(expr)` | the first argument |
//! | `response:Entity = expr` | the assigned value |
//!
//! ```abl
//! {&OUT} "<p>Hello, " pcName "</p>".
//! ```
//!
//! With `pcName` set to `<script>...</script>`, the page runs the caller's
//! script.
//!
//! Fixed — HTML-encode on output, and build JSON with the object model:
//!
//! ```abl
//! {&OUT} "<p>Hello, " html-encode(pcName) "</p>".
//! oResponse:Entity = oJsonObject.
//! ```
//!
//! # Sanitisers
//!
//! Stage 1 does not know a sanitiser. `html-encode(pcName)` is a function
//! call, not a literal, so the line above is still reported. Baseline it, or
//! suppress it with a comment, once the encoding is the only way the value
//! reaches the page.
//!
//! # What counts as a web response
//!
//! The parser has no types, so the `OpenEdge.Web` forms are picked by the name
//! of the object: it holds the word `resp`, `response` or `writer` (`oResp`,
//! `poResponse`, `oWriter`, `THIS-OBJECT:oResponse`). A `Write` on a stream
//! with another name is not reported. `{&OUT}` is the WebSpeed output name:
//! when the web include defines it, the text holds
//! `PUT STREAM WebStream UNFORMATTED`, and the rule reads that. When the
//! program has no definition, the name stays in place and the rule reads the
//! items that follow it.
//!
//! An `Entity` that is a `NEW` of a `Json...` class is not reported: the
//! response serialises a JSON object model safely. A variable that holds a JSON
//! object is reported, because its type is not known.
//!
//! # Where the rule looks
//!
//! Modelled statements and expressions, and by lexing, the calls inside
//! statements the parser does not model. `EXPORT STREAM WebStream`,
//! `DISPLAY STREAM WebStream` and the `{&OUT-LONG}` and `{&OUT-FMT}` names are
//! not read. A response written through a method other than `Write` is not
//! seen.

use oxabl_ast::{Expression, ExpressionKind, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Site, constant, last_name, scan};
use super::literal::cover;
use super::web::{is_body_write, is_response, stream_write};
use super::{ABLSEC006, finding, meta};
use crate::registry::RuleMeta;

/// Kebab-case rule name.
pub const NAME: &str = "xss-output";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str = "a web response body is written from a value that is not a literal";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "HTML-encode every value on output, and build JSON with a `JsonObject` instead of \
     joining strings. This check does not know an encoder: a value that goes through one is still \
     reported until a later stage accepts sanitisers.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC006,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut report = |span: Span, what: &str| {
        diags.push(finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC006,
            span,
            format!("`{what}` writes a non-literal value to the response body"),
            HELP,
        ));
    };
    scan(ctx, program, &["Write"], &mut |site| match site {
        Site::Statement(stmt) => {
            if let Some(write) = stream_write(ctx.source, stmt) {
                if !write.is_constant() {
                    let what = if write.text.is_word(0, "PUT") {
                        "PUT STREAM WebStream"
                    } else {
                        "{&OUT}"
                    };
                    report(write.text.span(0..write.text.len()), what);
                }
            } else {
                match &stmt.kind {
                    StatementKind::Assignment { target, value } => {
                        entity(stmt, target, value, &mut report);
                    }
                    StatementKind::Assign { assignments } => {
                        for pair in assignments {
                            entity(stmt, &pair.target, &pair.value, &mut report);
                        }
                    }
                    _ => {}
                }
            }
        }
        Site::Call(call) => {
            if is_body_write(&call) && call.args.first().is_some_and(|a| !a.constant) {
                report(call.span, "Write");
            }
        }
    });
    diags
}

/// Report an assignment to the `Entity` of a response when the value can carry
/// text from outside.
fn entity(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    report: &mut impl FnMut(Span, &str),
) {
    if let ExpressionKind::MemberAccess { object, member } = &target.kind
        && member.name.eq_ignore_ascii_case("Entity")
        && last_name(object).is_some_and(is_response)
        && !constant(value)
        && !is_json_model(value)
    {
        report(cover(target.span, value.span, stmt.span), "Entity");
    }
}

/// `NEW ...JsonObject(...)`, `NEW ...JsonArray(...)`.
fn is_json_model(expr: &Expression) -> bool {
    matches!(&expr.kind, ExpressionKind::New { class_name, .. }
        if class_name.rsplit('.').next().is_some_and(|c| c.len() >= 4 && c[..4].eq_ignore_ascii_case("Json")))
}
