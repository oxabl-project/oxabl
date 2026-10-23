//! `dynamic-query` (ABLSEC001), group `security`.
//!
//! Fires where a query is built from text that is not a literal and then
//! compiled at run time. That is SQL injection's ABL form: a value that
//! reaches the query string can change which records the query selects.
//!
//! # The set
//!
//! | Form | Argument read |
//! |---|---|
//! | `handle:QUERY-PREPARE(expr)` | the query string |
//! | `handle:FIND-FIRST(where, ...)`, `FIND-LAST`, `FIND-UNIQUE` | the where string |
//! | `handle:FILL-WHERE-STRING = expr` (also in `ASSIGN`) | the assigned string |
//!
//! There is no `OPEN QUERY ... PREPARE` form: a static `OPEN QUERY`, `FOR
//! EACH` or `FIND` binds its variables as values, so no value can change the
//! query. Those forms are never reported.
//!
//! ```abl
//! hQuery:QUERY-PREPARE("FOR EACH Customer WHERE Customer.Name = '" + pcName + "'").
//! ```
//!
//! With `pcName` set to `' OR Customer.Name <> '`, the query selects every
//! customer.
//!
//! Fixed — static code binds the value:
//!
//! ```abl
//! FOR EACH Customer NO-LOCK WHERE Customer.Name = pcName:
//! ```
//!
//! If the query has to be dynamic, `QUOTER()` every value and take field and
//! table names from a literal allow-list.
//!
//! # `QUOTER` and stage 1
//!
//! `QUOTER(x)` is the right fix, and stage 1 still reports a string that holds
//! it. A concatenation is constant only when every part is a literal
//! ([`literal`](super::literal)), and the rule does not yet know a sanitiser.
//! The help of each finding says so. Use a baseline for code that is checked.
//!
//! # Where the rule looks
//!
//! Method calls in every modelled statement and expression, `FILL-WHERE-STRING`
//! assignments in `=` and `ASSIGN` statements, and, by lexing the statement
//! text, method calls inside statements the parser recognizes but does not
//! model (`StatementKind::Skipped`). A `FILL-WHERE-STRING` assignment inside a
//! skipped statement is not seen, and neither is a query string set through a
//! method the rule does not name (for example a `DATA-SOURCE` `QUERY` built
//! elsewhere).
//!
//! The method names are matched by spelling, since the parser has no types: a
//! user method named `FIND-FIRST` on another object is also checked.

use oxabl_ast::{Expression, ExpressionKind, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::Kind;
use oxabl_semantic::{AnalysisContext, Semantic};

use super::literal::{StatementText, cover, is_constant, is_text_read, mentions};
use super::{ABLSEC001, finding};
use crate::rules::syntactic::{Visitor, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "dynamic-query";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Error;
/// One-line description for listings.
pub const DESCRIPTION: &str = "QUERY-PREPARE, FIND-FIRST, FIND-LAST, FIND-UNIQUE and FILL-WHERE-STRING take a query built at run time";

const HELP: &str = "Use a static `FOR EACH`, `FIND` or `OPEN QUERY` with variables, which bind values \
     instead of splicing them. If the query must be dynamic, wrap every value in `QUOTER()` and \
     take field and table names from a literal allow-list. `QUOTER()` does not make a \
     concatenated string a literal: this check still reports it until a later stage accepts \
     sanitisers.";

/// Methods whose first argument is a query or where string.
const METHODS: [&str; 4] = ["QUERY-PREPARE", "FIND-FIRST", "FIND-LAST", "FIND-UNIQUE"];

fn sink_method(name: &str) -> Option<&'static str> {
    METHODS.into_iter().find(|m| m.eq_ignore_ascii_case(name))
}

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = DynamicQueries {
        ctx,
        diags: Vec::new(),
    };
    walk_block(&mut v, program);
    v.diags
}

struct DynamicQueries<'a> {
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl DynamicQueries<'_> {
    fn report(&mut self, span: Span, what: &str) {
        self.diags.push(finding(
            self.ctx,
            DEFAULT_SEVERITY,
            ABLSEC001,
            span,
            format!("`{what}` takes a query built from a non-literal value"),
            HELP,
        ));
    }

    fn assignment(&mut self, stmt: &Statement, target: &Expression, value: &Expression) {
        if let ExpressionKind::MemberAccess { member, .. } = &target.kind
            && member.name.eq_ignore_ascii_case("FILL-WHERE-STRING")
            && !is_constant(value)
        {
            let span = cover(target.span, value.span, stmt.span);
            self.report(span, "FILL-WHERE-STRING");
        }
    }

    /// Lex a skipped statement and report each sink method call in it.
    fn scan_skipped(&mut self, stmt: &Statement) {
        if !mentions(self.ctx.source, stmt, &METHODS) {
            return;
        }
        let Some(st) = StatementText::of(self.ctx.source, stmt) else {
            return;
        };
        for i in 0..st.len() {
            let Some(method) = (st.is(i, Kind::Colon) && i + 2 < st.len())
                .then(|| sink_method(st.word(i + 1)))
                .flatten()
            else {
                continue;
            };
            if !st.is(i + 2, Kind::LeftParen) {
                continue;
            }
            if let Some(first) = st.call_args(i + 2).into_iter().next()
                && !st.is_constant(first.clone())
                && let Some(close) = st.close_of(i + 2)
            {
                self.report(st.span(i + 1..close + 1), method);
            }
        }
    }
}

impl Visitor for DynamicQueries<'_> {
    fn statement(&mut self, stmt: &Statement) -> bool {
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
        if let ExpressionKind::MethodCall {
            method, arguments, ..
        } = &expr.kind
            && let Some(name) = sink_method(&method.name)
            && let Some(first) = arguments.first()
            && !is_constant(first)
        {
            self.report(cover(method.span, expr.span, expr.span), name);
        }
    }
}
