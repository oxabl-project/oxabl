//! `path-traversal` (ABLSEC005), group `security`.
//!
//! Fires where a file or directory path is built from text that is not a
//! literal. A value that reaches the path can climb out of the intended
//! directory with `../`, or name any file the process can reach.
//!
//! # The set
//!
//! | Form | Operand read |
//! |---|---|
//! | `INPUT FROM VALUE(expr)`, `OUTPUT TO VALUE(expr)` (with or without `STREAM`) | the path |
//! | `INPUT FROM OS-DIR(expr)` | the directory |
//! | `OS-DELETE`, `OS-CREATE-DIR`, `OS-COPY`, `OS-RENAME` with `VALUE(expr)` | every path |
//! | `COPY-LOB ... FILE expr` | the path, taken as an expression |
//! | `FILE-INFO:FILE-NAME = expr` (`FILE-INFORMATION` too) | the assigned path |
//!
//! ```abl
//! OUTPUT TO VALUE("/data/reports/" + pcName).
//! ```
//!
//! With `pcName` set to `../../etc/cron.d/job`, the output leaves the report
//! directory.
//!
//! Fixed — resolve the full path and confirm it is under an approved base
//! before use:
//!
//! ```abl
//! FILE-INFO:FILE-NAME = "/data/reports/" + pcName.
//! IF NOT FILE-INFO:FULL-PATHNAME BEGINS "/data/reports/" THEN
//!     UNDO, THROW NEW Progress.Lang.AppError("path outside report directory", 1).
//! ```
//!
//! The check itself assigns `FILE-INFO:FILE-NAME` from a non-literal, so it is
//! reported too. Baseline that line, or suppress it with a comment, once it
//! sits in the single helper that does the check.
//!
//! # What is not reported
//!
//! A path written without `VALUE(...)` is text: `OS-DELETE old-file` deletes
//! `old-file` whether or not a variable has that name. A path that is a
//! literal, or literals joined by `+`, is never reported.
//!
//! # Where the rule looks
//!
//! `INPUT FROM` and `OUTPUT TO` are modelled statements; the rule reads the
//! `VALUE(...)` or `OS-DIR(...)` operand of the stream. The `OS-*` statements
//! and `COPY-LOB` are `StatementKind::Skipped`, so the rule lexes their text:
//! it reads each `VALUE( ... )` group, and for `COPY-LOB` the tokens after a
//! `FILE` that follows `COPY-LOB`, `FROM` or `TO`, up to the next clause
//! keyword. Paths set by `RUN` arguments, `LOAD`, `SEARCH` and the `.NET`
//! `System.IO` classes are not seen.

use oxabl_ast::{
    Expression, ExpressionKind, Span, Statement, StatementKind, StreamOperation, StreamTarget,
};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::literal::{StatementText, cover, is_constant, leading_word};
use super::{ABLSEC005, finding};
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "path-traversal";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "a file path for a stream, OS-* statement, COPY-LOB or FILE-INFO is built at run time";

const HELP: &str = "Resolve the full path (`FILE-INFO:FULL-PATHNAME`) and confirm that it is under an \
     approved base directory before use, or take the file name from a literal allow-list.";

/// Statements whose `VALUE(...)` operands are paths.
const OS_STATEMENTS: [&str; 4] = ["OS-DELETE", "OS-CREATE-DIR", "OS-COPY", "OS-RENAME"];

/// Words that end a `COPY-LOB` file operand.
const COPY_LOB_CLAUSES: [&str; 9] = [
    "TO",
    "FROM",
    "STARTING",
    "FOR",
    "OVERLAY",
    "APPEND",
    "CONVERT",
    "NO-CONVERT",
    "NO-ERROR",
];

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = Paths {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

/// The path operand of a stream target: the argument of `VALUE(...)` or
/// `OS-DIR(...)`. Any other form (`"x.txt"`, `data.txt`, `TERMINAL`) is text.
fn stream_operand(expr: &Expression) -> Option<&Expression> {
    let ExpressionKind::FunctionCall { name, arguments } = &expr.kind else {
        return None;
    };
    (name.name.eq_ignore_ascii_case("VALUE") || name.name.eq_ignore_ascii_case("OS-DIR"))
        .then(|| arguments.first())
        .flatten()
}

/// Whether `expr` is the `FILE-INFO` (or `FILE-INFORMATION`) system handle.
fn is_file_info(expr: &Expression) -> bool {
    const FULL: &str = "FILE-INFORMATION";
    matches!(
        &expr.kind,
        ExpressionKind::Identifier(id)
            if id.name.len() >= "FILE-INFO".len()
                && id.name.len() <= FULL.len()
                && FULL[..id.name.len()].eq_ignore_ascii_case(&id.name)
    )
}

struct Paths<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl Paths<'_> {
    fn report(&mut self, span: Span, what: &str) {
        self.diags.push(finding(
            self.ctx,
            DEFAULT_SEVERITY,
            ABLSEC005,
            span,
            format!("`{what}` takes a file path built from a non-literal value"),
            HELP,
        ));
    }

    fn assignment(&mut self, stmt: &Statement, target: &Expression, value: &Expression) {
        if let ExpressionKind::MemberAccess { object, member } = &target.kind
            && member.name.eq_ignore_ascii_case("FILE-NAME")
            && is_file_info(object)
            && !is_constant(value)
        {
            let span = cover(target.span, value.span, stmt.span);
            self.report(span, "FILE-INFO:FILE-NAME");
        }
    }

    fn stream(&mut self, stmt: &Statement, target: &StreamTarget, what: &str) {
        let Some(operand) = target.expression() else {
            return;
        };
        if let Some(path) = stream_operand(operand)
            && !is_constant(path)
        {
            let span = if operand.span.start < operand.span.end {
                operand.span
            } else {
                stmt.span
            };
            self.report(span, what);
        }
    }

    fn copy_lob(&mut self, st: &StatementText) {
        for i in 1..st.len() {
            if !st.is_word(i, "FILE")
                || !(i == 1 || st.is_word(i - 1, "FROM") || st.is_word(i - 1, "TO"))
            {
                continue;
            }
            let mut depth = 0usize;
            let end = (i + 1..st.len())
                .find(|&j| {
                    match st.token(j).kind {
                        Kind::LeftParen => depth += 1,
                        Kind::RightParen => depth = depth.saturating_sub(1),
                        _ => {}
                    }
                    depth == 0 && COPY_LOB_CLAUSES.iter().any(|w| st.is_word(j, w))
                })
                .unwrap_or(st.len());
            if i + 1 < end && !st.is_constant(i + 1..end) {
                self.report(st.span(i..end), "COPY-LOB FILE");
            }
        }
    }

    fn scan_skipped(&mut self, stmt: &Statement) {
        let Some(word) = leading_word(self.ctx.source, stmt) else {
            return;
        };
        if !OS_STATEMENTS.contains(&word.as_str()) && word != "COPY-LOB" {
            return;
        }
        let Some(st) = StatementText::of(self.ctx.source, stmt) else {
            return;
        };
        if OS_STATEMENTS.contains(&word.as_str()) {
            for operand in st.value_operands(1..st.len()) {
                if !st.is_constant(operand.clone()) {
                    // From `VALUE` to its closing parenthesis.
                    self.report(st.span(operand.start - 2..operand.end + 1), &word);
                }
            }
        } else if word == "COPY-LOB" {
            self.copy_lob(&st);
        }
    }
}

impl Visitor for Paths<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::StreamIo { operation, .. } => match operation {
                StreamOperation::From(target) => self.stream(stmt, target, "INPUT FROM"),
                StreamOperation::To { target, .. } => self.stream(stmt, target, "OUTPUT TO"),
                StreamOperation::Through(_) | StreamOperation::Close => {}
            },
            StatementKind::Assignment { target, value } => self.assignment(stmt, target, value),
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    self.assignment(stmt, &pair.target, &pair.value);
                }
            }
            StatementKind::Skipped { .. } => self.scan_skipped(stmt),
            _ => {}
        }
        true
    }
}
