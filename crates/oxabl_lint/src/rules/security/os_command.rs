//! `os-command` (ABLSEC003), group `security`.
//!
//! Fires where an operating-system command is built from text that is not a
//! literal. A value that reaches the command line can add commands or options
//! of its own.
//!
//! # The set
//!
//! | Form | Operand read |
//! |---|---|
//! | `OS-COMMAND [SILENT] [NO-WAIT] ... VALUE(expr) ...` | every `VALUE(...)` |
//! | `UNIX [SILENT] VALUE(expr)` and `DOS [SILENT] VALUE(expr)` | every `VALUE(...)` |
//! | `INPUT`, `OUTPUT` and `INPUT-OUTPUT [STREAM s] THROUGH ... VALUE(expr) ...` | every `VALUE(...)` before a `CONVERT` clause |
//!
//! ```abl
//! OS-COMMAND SILENT VALUE("tar -xf " + pcArchive).
//! ```
//!
//! With `pcArchive` set to `a.tar; rm -rf /data`, the shell runs both
//! commands.
//!
//! Fixed — a fixed command, with arguments chosen from an allow-list:
//!
//! ```abl
//! CASE pcAction:
//!     WHEN "daily"  THEN OS-COMMAND SILENT VALUE("backup-daily.sh").
//!     WHEN "weekly" THEN OS-COMMAND SILENT VALUE("backup-weekly.sh").
//! END CASE.
//! ```
//!
//! # What is not reported
//!
//! Words written without `VALUE(...)` are text, not expressions:
//! `OS-COMMAND SILENT ls -l` and `OS-COMMAND SILENT cmd-name` are fixed
//! commands even where `cmd-name` is also a variable name. The program name
//! of a `THROUGH` statement is such a word. The clause after `CONVERT` names a
//! code page, not a command, so it is not read.
//!
//! # Where the rule looks
//!
//! The parser does not model `OS-COMMAND`, `UNIX` or `DOS`
//! (`StatementKind::Skipped`), and keeps only the program name of a `THROUGH`
//! statement, so the rule lexes the statement text and reads each
//! `VALUE( ... )` group. A command started another way (`System.Diagnostics`
//! through `NEW`, a shell script chosen by `RUN`) is not seen.

use oxabl_ast::{Span, Statement, StatementKind, StreamOperation};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::literal::{StatementText, leading_word};
use super::{ABLSEC003, finding};
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "os-command";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Error;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "OS-COMMAND, UNIX, DOS and THROUGH streams run a command built at run time";

const HELP: &str = "Run a fixed command. If an argument must vary, choose it from a literal allow-list \
     with a `CASE`, and never pass user text through a shell.";

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = OsCommands {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct OsCommands<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl OsCommands<'_> {
    fn report(&mut self, span: Span, what: &str) {
        self.diags.push(finding(
            self.ctx,
            DEFAULT_SEVERITY,
            ABLSEC003,
            span,
            format!("`{what}` runs a command built from a non-literal value"),
            HELP,
        ));
    }

    /// Report each non-literal `VALUE(...)` operand after the first token, up
    /// to a `CONVERT` clause.
    fn scan(&mut self, st: &StatementText, what: &str) {
        let end = (1..st.len())
            .find(|&i| st.is_word(i, "CONVERT") || st.is_word(i, "NO-CONVERT"))
            .unwrap_or(st.len());
        for operand in st.value_operands(1..end) {
            if !st.is_constant(operand.clone()) {
                // From `VALUE` to its closing parenthesis.
                self.report(st.span(operand.start - 2..operand.end + 1), what);
            }
        }
    }
}

impl Visitor for OsCommands<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        let through = match &stmt.kind {
            StatementKind::Skipped { .. } => false,
            StatementKind::StreamIo {
                operation: StreamOperation::Through(_),
                ..
            } => true,
            _ => return true,
        };
        if !through
            && !leading_word(self.ctx.source, stmt)
                .is_some_and(|w| ["OS-COMMAND", "UNIX", "DOS"].contains(&w.as_str()))
        {
            return true;
        }
        let Some(st) = StatementText::of(self.ctx.source, stmt).filter(|st| !st.is_empty()) else {
            return true;
        };
        let word = st.word(0).to_ascii_uppercase();
        if through {
            self.scan(&st, &format!("{word} THROUGH"));
        } else if ["OS-COMMAND", "UNIX", "DOS"].contains(&word.as_str()) {
            self.scan(&st, &word);
        }
        true
    }
}
