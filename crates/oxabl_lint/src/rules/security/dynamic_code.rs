//! `dynamic-code` (ABLSEC004), group `security`.
//!
//! Fires where the procedure, function, method or class to run is chosen by a
//! string that is not a literal. A value that reaches the name decides which
//! code runs.
//!
//! # The set
//!
//! | Form | Argument read |
//! |---|---|
//! | `RUN VALUE(expr)` | the procedure name |
//! | `DYNAMIC-FUNCTION(name, ...)` | `name` |
//! | `DYNAMIC-INVOKE(target, name, ...)` | `name`, the second argument |
//! | `DYNAMIC-NEW class (...)` | `class` |
//! | `Progress.Lang.Class:GetClass(expr)` | the type name |
//! | `COMPILE VALUE(expr) ...` | the source file |
//! | `handle:CALL-NAME = expr` | the assigned name |
//!
//! ```abl
//! RUN VALUE("post-" + pcKind + ".p") (INPUT iOrder).
//! ```
//!
//! With `pcKind` set to `../../tmp/payload`, the program runs a file the
//! caller chose.
//!
//! Fixed — map an input key to a literal name:
//!
//! ```abl
//! CASE pcKind:
//!     WHEN "invoice" THEN RUN post-invoice.p (INPUT iOrder).
//!     WHEN "credit"  THEN RUN post-credit.p (INPUT iOrder).
//!     OTHERWISE UNDO, THROW NEW Progress.Lang.AppError("unknown kind", 1).
//! END CASE.
//! ```
//!
//! # Relation to `run-value` and `dynamic-invoke`
//!
//! The strict `run-value` (CL001) and `dynamic-invoke` (CL002) rules are about
//! analysability: a call whose callee is not in the text hides from every
//! analysis that follows calls, including `RUN VALUE("a" + ".p")`. This rule is
//! about injection: it reuses their detection (`run_value::dynamic_target` and
//! `dynamic_invoke::dynamic_name`) and reports only where the name is not a
//! literal, because only then can a caller steer it. Where both groups are on, a
//! call such as `RUN VALUE(c)` gets two findings, each saying why; fixing the
//! target clears both. The help of every finding says so.
//!
//! `DYNAMIC-PROPERTY` and `CREATE CALL` belong to `dynamic-invoke` only: a
//! property name does not choose code to run, and `CREATE CALL` has no name
//! until `CALL-NAME` is set, which this rule reads.
//!
//! # Where the rule looks
//!
//! Modelled statements and expressions, and, by lexing the statement text,
//! `COMPILE` and calls inside statements the parser recognizes but does not
//! model (`StatementKind::Skipped`: `PUT`, `EXPORT`, ...). The first argument
//! of `DYNAMIC-INVOKE` is a handle or a class name and is not read: when it is a
//! character variable holding a class name, the call is missed. Reflection
//! through other `Progress.Lang.Class` or `Progress.Reflect` methods is not
//! seen, and `COMPILE ... SAVE INTO VALUE(dir)` is a path, not code, and is not
//! reported.

use oxabl_ast::{Expression, ExpressionKind, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::literal::{StatementText, cover, is_constant, is_text_read, mentions};
use super::{ABLSEC004, finding};
use crate::rules::dynamic_invoke::dynamic_name;
use crate::rules::run_value::dynamic_target;
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "dynamic-code";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Error;
/// One-line description for listings.
pub const DESCRIPTION: &str = "code to run or a class to load is chosen by a run-time string";

const HELP: &str = "Map an input key to a literal name (a `CASE` with a static call for each choice) \
     instead of passing text as a procedure, function, method or class name. `run-value` (CL001) \
     and `dynamic-invoke` (CL002) report the same calls because analysis cannot follow them; this \
     check reports only a name that is not a literal, because that is what a caller can steer. \
     Fixing the name clears both.";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = DynamicCode {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

/// The index of the argument that names what a dynamic call runs, or `None`
/// for a call that does not choose code (`DYNAMIC-PROPERTY`).
fn name_argument(canonical: &str) -> Option<usize> {
    match canonical {
        "DYNAMIC-FUNCTION" | "DYNAMIC-NEW" => Some(0),
        "DYNAMIC-INVOKE" => Some(1),
        _ => None,
    }
}

/// The dotted name an expression spells, or `None` when it is not a name.
fn dotted_name(expr: &Expression) -> Option<String> {
    match &expr.kind {
        ExpressionKind::Identifier(id) => Some(id.name.clone()),
        ExpressionKind::FieldAccess { qualifier, field } => {
            Some(format!("{}.{}", dotted_name(qualifier)?, field.name))
        }
        _ => None,
    }
}

/// Whether `expr` names `Progress.Lang.Class`, in full or as `Class`.
fn is_class_type(expr: &Expression) -> bool {
    dotted_name(expr).is_some_and(|n| {
        n.eq_ignore_ascii_case("Progress.Lang.Class") || n.eq_ignore_ascii_case("Class")
    })
}

struct DynamicCode<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl DynamicCode<'_> {
    fn report(&mut self, span: Span, what: &str) {
        self.diags.push(finding(
            self.ctx,
            DEFAULT_SEVERITY,
            ABLSEC004,
            span,
            format!("`{what}` runs code chosen by a non-literal value"),
            HELP,
        ));
    }

    fn assignment(&mut self, stmt: &Statement, target: &Expression, value: &Expression) {
        if let ExpressionKind::MemberAccess { member, .. } = &target.kind
            && member.name.eq_ignore_ascii_case("CALL-NAME")
            && !is_constant(value)
        {
            let span = cover(target.span, value.span, stmt.span);
            self.report(span, "CALL-NAME");
        }
    }

    /// Report the argument at `index` of the call whose `(` is at `open`.
    fn token_call(&mut self, st: &StatementText, open: usize, index: usize, what: &str) {
        if let Some(arg) = st.call_args(open).into_iter().nth(index)
            && !st.is_constant(arg)
            && let Some(close) = st.close_of(open)
        {
            // From the keyword before `(` to the closing parenthesis.
            self.report(st.span(open - 1..close + 1), what);
        }
    }

    /// Lex a skipped statement: `COMPILE VALUE(...)` and dynamic calls.
    fn scan_skipped(&mut self, stmt: &Statement) {
        if !mentions(self.ctx.source, stmt, &["COMP", "DYNAMIC-", "GetClass"]) {
            return;
        }
        let Some(st) = StatementText::of(self.ctx.source, stmt) else {
            return;
        };
        if st.is(0, Kind::Compile)
            && st.is(1, Kind::Value)
            && let Some(operand) = st.value_operands(1..st.len()).into_iter().next()
            && !st.is_constant(operand.clone())
        {
            self.report(st.span(operand.start - 2..operand.end + 1), "COMPILE VALUE");
        }
        for i in 0..st.len() {
            if let Some(canonical) = dynamic_name(st.word(i))
                && let Some(index) = name_argument(canonical)
            {
                self.token_call_at(&st, i, canonical, index);
            } else if st.is_word(i, "GetClass")
                && i > 1
                && st.is(i - 1, Kind::Colon)
                && st.is_word(i - 2, "Class")
                && st.is(i + 1, Kind::LeftParen)
            {
                self.token_call(&st, i + 1, 0, "Class:GetClass");
            }
        }
    }

    /// The call keyword at token `i`: `NAME(args)`, or `DYNAMIC-NEW class (args)`.
    fn token_call_at(&mut self, st: &StatementText, i: usize, canonical: &str, index: usize) {
        if canonical != "DYNAMIC-NEW" {
            if st.is(i + 1, Kind::LeftParen) {
                self.token_call(st, i + 1, index, canonical);
            }
            return;
        }
        let operand = if st.is(i + 1, Kind::LeftParen) {
            st.close_of(i + 1).map(|close| i + 2..close)
        } else {
            (i + 2..st.len())
                .find(|&j| st.is(j, Kind::LeftParen))
                .map(|open| i + 1..open)
        };
        if let Some(operand) = operand
            && !st.is_constant(operand.clone())
        {
            self.report(st.span(i..operand.end), canonical);
        }
    }
}

impl Visitor for DynamicCode<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        if let Some(target) = dynamic_target(stmt) {
            if !is_constant(target) {
                // From `RUN` to the end of the target.
                let span = Span {
                    start: stmt.span.start,
                    end: target.span.end.max(stmt.span.start),
                };
                self.report(span, "RUN VALUE");
            }
            return true;
        }
        match &stmt.kind {
            StatementKind::Assignment { target, value } => self.assignment(stmt, target, value),
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    self.assignment(stmt, &pair.target, &pair.value);
                }
            }
            _ if is_text_read(stmt) => {
                self.scan_skipped(stmt);
                return false;
            }
            _ => {}
        }
        true
    }

    fn expression(&mut self, expr: &Expression) {
        match &expr.kind {
            ExpressionKind::FunctionCall { name, arguments } => {
                if let Some(canonical) = dynamic_name(&name.name)
                    && let Some(index) = name_argument(canonical)
                    && let Some(arg) = arguments.get(index)
                    && !is_constant(arg)
                {
                    self.report(expr.span, canonical);
                }
            }
            ExpressionKind::MethodCall {
                object,
                method,
                arguments,
            } if method.name.eq_ignore_ascii_case("GetClass") && is_class_type(object) => {
                if let Some(arg) = arguments.first()
                    && !is_constant(arg)
                {
                    self.report(cover(method.span, expr.span, expr.span), "Class:GetClass");
                }
            }
            _ => {}
        }
    }
}
