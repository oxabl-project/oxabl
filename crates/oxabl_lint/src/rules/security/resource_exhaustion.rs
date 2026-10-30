//! `resource-exhaustion` (ABLSEC016), group `security`.
//!
//! Fires where a size or a row count that nobody caps decides how much memory
//! the session takes. A request that asks for a very large buffer, sends a
//! very large length prefix, or reads a very large table can make the agent
//! page out or fail, for every user of the agent. The finding is `info`: many of
//! these sites are fine, and the rule cannot see the caller's limit.
//!
//! # The set
//!
//! | Form | Reported when |
//! |---|---|
//! | `SET-SIZE(m) = expr` | `expr` is not made only of number literals |
//! | `socket:READ(m, offset, bytes, mode)` | `bytes` is not made only of number literals, and does not call `MINIMUM` or `MIN` |
//! | `handle:FILL()` | no assignment to `BATCH-SIZE` with a value other than `0` appears in the file |
//!
//! ```abl
//! SET-SIZE(mBody) = iContentLength.
//! ```
//!
//! With a `Content-Length` of two gigabytes, the session asks for two
//! gigabytes.
//!
//! Fixed — cap the size and reject what is over the cap:
//!
//! ```abl
//! IF iContentLength > 1048576 THEN UNDO, THROW NEW Progress.Lang.AppError("too large", 1).
//! SET-SIZE(mBody) = iContentLength.
//!
//! hBuffer:BATCH-SIZE = 100.
//! hDataSet:FILL().
//! ```
//!
//! # What counts as a cap
//!
//! Stage 1 knows two. A `BATCH-SIZE` assignment anywhere in the file covers
//! every `FILL` in the file, because the buffer or data source that it was set
//! on is not followed. A size that goes through `MINIMUM` (or `MIN`) is
//! capped for `READ`. A guard before `SET-SIZE` (the `IF` above) is not seen, so
//! the line after it is still reported; baseline it or suppress it. The
//! existing transaction rules of this tree do not model batch size, so this rule
//! carries its own.
//!
//! # Where the rule looks
//!
//! `SET-SIZE` assignments in `=` and `ASSIGN` statements, and `FILL` calls in
//! modelled expressions and, by lexing, in unmodelled statements. The parser
//! rejects the mode keyword of `READ` (`READ-AVAILABLE`, `READ-EXACT-NUM`) as an
//! argument, which drops the statement from the tree, so `READ` calls are found
//! by lexing the whole file for `:READ(`. The method names are matched by
//! spelling, since the parser has no types: a user method named `READ` or `FILL`
//! on another object is also checked (a `FILL` with arguments is not).

use oxabl_ast::{Expression, ExpressionKind, Literal, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{CallKind, Site, scan};
use super::literal::{StatementText, cover};
use super::taint::{Gate, SinkKind};
use super::{ABLSEC016, finding, meta};
use crate::registry::RuleMeta;
use crate::rules::syntactic::{Visitor, walk_block};
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "resource-exhaustion";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Info;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "5.5";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "a memory size or a data-set fill has no cap: SET-SIZE, a socket READ, FILL without BATCH-SIZE";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Put a cap on every size that comes from outside, with `MINIMUM(size, limit)` or a \
     check that rejects a size over the limit, before `SET-SIZE` or a socket `READ`. Set \
     `BATCH-SIZE` on the buffer or data source before `FILL`. This check cannot see a limit that \
     the caller applies.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC016,
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
    let mut batch = BatchSize::default();
    walk_block(&mut batch, program);

    let gate = Gate::new(ABLSEC016, NAME, program, ctx, shared);
    let mut diags = Vec::new();
    let mut report = |span: Span, message: &str, operands: &[Span]| {
        let Some(j) = gate.judge(operands, SinkKind::Size) else {
            return;
        };
        let message = if j.tainted {
            format!("{message}: it comes from {}", j.subject)
        } else {
            message.to_string()
        };
        let d = finding(ctx, DEFAULT_SEVERITY, ABLSEC016, span, message, HELP);
        diags.push(j.attach(d));
    };
    socket_reads(ctx.source, &mut report);
    scan(ctx, program, &["FILL"], &mut |site| match site {
        Site::Statement(stmt) => match &stmt.kind {
            StatementKind::Assignment { target, value } => {
                set_size(stmt, target, value, &mut report);
            }
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    set_size(stmt, &pair.target, &pair.value, &mut report);
                }
            }
            _ => {}
        },
        Site::Call(call) => {
            if call.kind == CallKind::Method
                && call.is("FILL")
                && call.args.is_empty()
                && !batch.capped
            {
                report(
                    call.span,
                    "`FILL` reads every row: no `BATCH-SIZE` is set in this file",
                    &[],
                );
            }
        }
    });
    diags
}

/// Whether `expr` is a number literal, or arithmetic on number literals.
fn size_constant(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Literal(Literal::Integer(_) | Literal::Decimal(_)) => true,
        ExpressionKind::Add(l, r)
        | ExpressionKind::Minus(l, r)
        | ExpressionKind::Multiply(l, r)
        | ExpressionKind::Divide(l, r)
        | ExpressionKind::Modulo(l, r) => size_constant(l) && size_constant(r),
        ExpressionKind::Negate(e) => size_constant(e),
        _ => false,
    }
}

fn set_size(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    report: &mut impl FnMut(Span, &str, &[Span]),
) {
    if let ExpressionKind::FunctionCall { name, .. } = &target.kind
        && name.name.eq_ignore_ascii_case("SET-SIZE")
        && !size_constant(value)
    {
        report(
            cover(target.span, value.span, stmt.span),
            "`SET-SIZE` allocates a buffer whose size is not a literal",
            &[value.span],
        );
    }
}

/// Whether the file sets `BATCH-SIZE` to something that is not `0`.
#[derive(Default)]
struct BatchSize {
    capped: bool,
}

impl BatchSize {
    fn assignment(&mut self, target: &Expression, value: &Expression) {
        if let ExpressionKind::MemberAccess { member, .. } = &target.kind
            && member.name.eq_ignore_ascii_case("BATCH-SIZE")
            && !matches!(&value.kind, ExpressionKind::Literal(Literal::Integer(i)) if i.value == 0)
        {
            self.capped = true;
        }
    }
}

impl Visitor for BatchSize {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::Assignment { target, value } => self.assignment(target, value),
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    self.assignment(&pair.target, &pair.value);
                }
            }
            _ => {}
        }
        true
    }
}

/// Report each `:READ(buffer, offset, bytes, ...)` whose `bytes` has no cap.
fn socket_reads(source: &str, report: &mut impl FnMut(Span, &str, &[Span])) {
    if !source
        .as_bytes()
        .windows(5)
        .any(|w| w.eq_ignore_ascii_case(b":read"))
    {
        return;
    }
    let whole = Span {
        start: 0,
        end: source.len() as u32,
    };
    let Some(st) = StatementText::over(source, whole) else {
        return;
    };
    for i in 2..st.len().saturating_sub(1) {
        if !(st.is_word(i, "READ") && st.is(i - 1, Kind::Colon) && st.is(i + 1, Kind::LeftParen)) {
            continue;
        }
        let args = st.call_args(i + 1);
        let (Some(bytes), Some(close)) = (args.get(2), st.close_of(i + 1)) else {
            continue;
        };
        let literal = bytes.clone().all(|j| {
            matches!(
                st.token(j).kind,
                Kind::IntegerLiteral
                    | Kind::BigIntLiteral
                    | Kind::Plus
                    | Kind::Minus
                    | Kind::Star
                    | Kind::LeftParen
                    | Kind::RightParen
            ) || (st.token(j).kind == Kind::Add && st.word(j) == "+")
        });
        let capped = bytes
            .clone()
            .any(|j| st.is_word(j, "MINIMUM") || st.is_word(j, "MIN"));
        if !literal && !capped {
            report(
                st.span(i..close + 1),
                "`READ` takes a byte count that is not a literal and has no cap",
                &[st.span(bytes.clone())],
            );
        }
    }
}
