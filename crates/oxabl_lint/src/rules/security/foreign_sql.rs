//! `foreign-sql` (ABLSEC002), group `security`.
//!
//! Fires where SQL text for a database that is not the OpenEdge one is built
//! from text that is not a literal. A value that reaches the SQL text can
//! change what the statement does.
//!
//! # The set
//!
//! | Form | Argument read |
//! |---|---|
//! | `RUN STORED-PROC send-sql-statement ... (sql)`, also `dbname.send-sql-statement` | every parameter in the parentheses |
//! | `NEW SqlCommand(sql, ...)`, `OdbcCommand`, `OleDbCommand`, `OracleCommand`, `MySqlCommand`, `NpgsqlCommand` | the first argument |
//! | `NEW SqlDataAdapter(sql, ...)` and the matching `...DataAdapter` classes | the first argument |
//! | `obj:CommandText = sql` | the assigned text |
//!
//! ```abl
//! RUN STORED-PROC send-sql-statement hProc = PROC-HANDLE NO-ERROR
//!     ("SELECT * FROM customers WHERE name = '" + pcName + "'").
//! ```
//!
//! Fixed — keep the SQL a literal and pass values as parameters:
//!
//! ```abl
//! oCmd = NEW System.Data.SqlClient.SqlCommand("SELECT * FROM customers WHERE name = @name", oConn).
//! oCmd:Parameters:AddWithValue("@name", pcName).
//! ```
//!
//! A stored procedure with named parameters (`RUN STORED-PROC get-orders ...
//! (INPUT pcName)`) is parameterised already and is not reported.
//!
//! # Where the rule looks
//!
//! `RUN STORED-PROC` is a `RUN` whose parameters the parser does not keep, so
//! the rule lexes the statement text and reads the first parenthesised list
//! after the procedure name; a leading `INPUT`, `PARAM` and `name =` are
//! stripped from each parameter. `.NET` classes are matched by the last segment
//! of the class name, so `NEW SqlCommand(...)` is checked with or without a
//! `USING`. The `CommandText` property is matched by spelling on any object.
//! SQL passed to a type the rule does not list, to `EXEC SQL` embedded SQL, or
//! built in one place and executed in another, is not seen.

use oxabl_ast::{Expression, ExpressionKind, RunTarget, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::literal::{StatementText, cover, is_constant};
use super::{ABLSEC002, finding, meta};
use crate::registry::RuleMeta;
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "foreign-sql";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Error;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "9.5";
/// One-line description for listings.
pub const DESCRIPTION: &str = "SQL text for a foreign database is built at run time";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Keep the SQL text a literal and pass values as parameters (`@name` with \
     `Parameters:AddWithValue`, or a stored procedure with named parameters).";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC002,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// `.NET` classes whose first constructor argument is SQL text.
const SQL_CLASSES: [&str; 12] = [
    "SqlCommand",
    "OdbcCommand",
    "OleDbCommand",
    "OracleCommand",
    "MySqlCommand",
    "NpgsqlCommand",
    "SqlDataAdapter",
    "OdbcDataAdapter",
    "OleDbDataAdapter",
    "OracleDataAdapter",
    "MySqlDataAdapter",
    "NpgsqlDataAdapter",
];

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = ForeignSql {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct ForeignSql<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl ForeignSql<'_> {
    fn report(&mut self, span: Span, what: &str) {
        self.diags.push(finding(
            self.ctx,
            DEFAULT_SEVERITY,
            ABLSEC002,
            span,
            format!("`{what}` takes SQL text built from a non-literal value"),
            HELP,
        ));
    }

    fn scan_send_sql(&mut self, stmt: &Statement) {
        let Some(st) = StatementText::of(self.ctx.source, stmt) else {
            return;
        };
        let Some(name) = (0..st.len()).find(|&i| st.is_word(i, "send-sql-statement")) else {
            return;
        };
        let Some(open) = (name + 1..st.len()).find(|&i| st.is(i, Kind::LeftParen)) else {
            return;
        };
        for arg in st.call_args(open) {
            // From the `(` or `,` before the parameter.
            let lead = arg.start - 1;
            let mut value = arg;
            if st.is_word(value.start, "INPUT") {
                value.start += 1;
            }
            if st.is_word(value.start, "PARAM") || st.is_word(value.start, "PARAMETER") {
                value.start += 1;
            }
            if value.start + 1 < value.end && st.is(value.start + 1, Kind::Equals) {
                value.start += 2;
            }
            if !st.is_constant(value.clone()) {
                self.report(st.span(lead..value.end), "send-sql-statement");
            }
        }
    }

    fn command_text(&mut self, stmt: &Statement, target: &Expression, value: &Expression) {
        if let ExpressionKind::MemberAccess { member, .. } = &target.kind
            && member.name.eq_ignore_ascii_case("CommandText")
            && !is_constant(value)
        {
            let span = cover(target.span, value.span, stmt.span);
            self.report(span, "CommandText");
        }
    }
}

impl Visitor for ForeignSql<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
        match &stmt.kind {
            StatementKind::Run {
                target: RunTarget::Literal { name, .. },
                ..
            } if name.eq_ignore_ascii_case("STORED-PROC") => self.scan_send_sql(stmt),
            StatementKind::Assignment { target, value } => self.command_text(stmt, target, value),
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    self.command_text(stmt, &pair.target, &pair.value);
                }
            }
            _ => {}
        }
        true
    }

    fn expression(&mut self, expr: &Expression) {
        if let ExpressionKind::New {
            class_name,
            arguments,
            ..
        } = &expr.kind
            && let Some(class) = SQL_CLASSES
                .into_iter()
                .find(|c| c.eq_ignore_ascii_case(class_name.rsplit('.').next().unwrap_or("")))
            && let Some(first) = arguments.first()
            && !is_constant(first)
        {
            self.report(expr.span, class);
        }
    }
}
