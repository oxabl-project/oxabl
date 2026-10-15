//! `transaction-spans-call` lint (TX004), group `strict.transactions`.
//!
//! Fires on a `RUN` inside a transaction block whose callee is not in the
//! file: an external procedure, a procedure run in another procedure's handle
//! (`RUN p IN h`), or a target named at run time (`RUN VALUE(...)`).
//!
//! # Why
//!
//! A called procedure runs inside the caller's transaction: its updates join
//! it and are undone with it, its own transaction blocks are only
//! sub-transactions, and the transaction holds its locks until the call
//! returns. When the callee is not in the file, nothing here shows whether it
//! updates the database, waits for the user or takes a long time.
//!
//! ```abl
//! DO TRANSACTION:
//!     FIND FIRST Customer EXCLUSIVE-LOCK.
//!     Customer.Balance = 0.
//!     RUN notify.p (Customer.CustNum).
//! END.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! DEFINE VARIABLE iCust AS INTEGER NO-UNDO.
//! DO FOR Customer TRANSACTION:
//!     FIND FIRST Customer EXCLUSIVE-LOCK.
//!     Customer.Balance = 0.
//!     iCust = Customer.CustNum.
//! END.
//! RUN notify.p (iCust).
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per `RUN`, naming the outermost transaction block around it
//! in its routine (the transaction blocks `transaction-spans-ui`, TX003,
//! counts).
//!
//! "Not in the file" is decided from the file alone. The workspace index can
//! say which file a literal target names, but it holds no facts about what
//! that file does, so a callee in another file counts as unknown whether the
//! index finds it or not.
//!
//! Not reported:
//!
//! - a `RUN` of an internal procedure the file defines with a body (also with
//!   `IN THIS-PROCEDURE`): its statements are in the file, and the other
//!   transaction rules read them;
//! - a `RUN` of a procedure the file declares `EXTERNAL` (a shared library),
//!   which cannot update the database;
//! - in an include checked alone, a `RUN` of a name with no path or
//!   extension, which may be an internal procedure of the includer;
//! - `RUN ... ASYNCHRONOUS` and `RUN ... ON SERVER`: the callee runs in
//!   another session, outside the transaction;
//! - a block that is a transaction block only by an inferred rule, only
//!   because of a temp-table update, or only when no schema is loaded;
//! - in an include checked alone, its main block;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::{RunTarget, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::{Kind, Lexer};
use oxabl_scope::FileScopes;
use oxabl_semantic::AnalysisContext;

use super::TX004;
use super::handle_support::{quote, warning};
use super::lock_support::{describe_block, enclosing_transaction, locate, not_judged};
use super::syntactic::{Visitor, leading_tokens, source_of, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "transaction-spans-call";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.transactions";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A RUN inside a transaction whose callee is not in the file, so its effects are unknown";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    check(&crate::shared::analyze_scopes(program, ctx), program, ctx)
}

/// What a `RUN` calls, when its statements are not in the file.
enum Callee {
    /// `RUN VALUE(...)`.
    Dynamic,
    /// `RUN p IN h`.
    InHandle(String),
    /// A literal target the file does not define.
    Outside(String),
}

/// The findings over an analysed file.
pub fn check(scopes: &FileScopes, program: &[Statement], ctx: &AnalysisContext) -> Vec<Diagnostic> {
    if not_judged(scopes) {
        return Vec::new();
    }
    let fragment = ctx.source_context.is_include_fragment();
    let mut diags = Vec::new();
    for (span, callee) in opaque_runs(program, ctx.source) {
        // In an include checked alone, a bare name may be an internal
        // procedure of the includer.
        if fragment && matches!(&callee, Callee::Outside(name) if !name.contains(['.', '/', '\\']))
        {
            continue;
        }
        let Some((unit, block)) = locate(scopes, span.start) else {
            continue;
        };
        let Some(tx) = enclosing_transaction(scopes, unit, block, fragment) else {
            continue;
        };
        let what = match callee {
            Callee::Dynamic => "a procedure named at run time".to_string(),
            Callee::InHandle(name) => {
                format!("`{name}` in another running procedure, which this file does not show")
            }
            Callee::Outside(name) => format!("`{name}`, which is not in this file"),
        };
        diags.push(warning(
            ctx,
            TX004,
            format!(
                "`{}` runs {what} inside the transaction of {}: what it updates, and how long it \
                 takes, is not known here",
                quote(ctx.source, span),
                describe_block(ctx.source, unit, tx),
            ),
            "Run it before or after the transaction, or check that it neither waits for the user \
             nor takes long: its updates join the transaction, and the transaction's locks are \
             held until it returns."
                .to_string(),
            span,
        ));
    }
    diags
}

/// Every `RUN` whose callee is not in the file and runs in this session, in
/// source order.
fn opaque_runs(program: &[Statement], source: &str) -> Vec<(Span, Callee)> {
    struct Runs<'a> {
        source: &'a str,
        runs: Vec<(Span, Callee)>,
    }
    impl Visitor for Runs<'_> {
        fn statement(&mut self, stmt: &Statement) -> bool {
            let StatementKind::Run {
                target,
                in_handle,
                asynchronous,
                ..
            } = &stmt.kind
            else {
                return true;
            };
            if *asynchronous || on_server(self.source, stmt.span) {
                return true;
            }
            let this_procedure = in_handle.as_ref().is_some_and(|h| {
                leading_tokens(self.source, h.span, 2)
                    .iter()
                    .map(|(t, _)| t.kind)
                    .eq([Kind::ThisProcedure])
            });
            let callee = match (target, in_handle) {
                (RunTarget::Dynamic(_), _) => Callee::Dynamic,
                (RunTarget::Literal { name, .. }, Some(_)) if !this_procedure => {
                    Callee::InHandle(name.clone())
                }
                (RunTarget::Literal { name, .. }, _) => Callee::Outside(name.clone()),
            };
            self.runs.push((stmt.span, callee));
            true
        }
    }
    let mut v = Runs {
        source,
        runs: Vec::new(),
    };
    walk_block(&mut v, program);
    let internal = internal_procedures(program);
    let mut runs: Vec<(Span, Callee)> = v
        .runs
        .into_iter()
        .filter(|(_, c)| match c {
            Callee::Outside(name) => !internal.iter().any(|p| p.eq_ignore_ascii_case(name)),
            _ => true,
        })
        .collect();
    runs.sort_by_key(|(s, _)| s.start);
    runs
}

/// The internal procedures the file defines with a body, or as an entry
/// point of a shared library.
fn internal_procedures(program: &[Statement]) -> Vec<String> {
    struct Procs(Vec<String>);
    impl Visitor for Procs {
        fn statement(&mut self, stmt: &Statement) -> bool {
            if let StatementKind::Procedure {
                name,
                is_prototype,
                is_external,
                ..
            } = &stmt.kind
                && (!is_prototype || *is_external)
            {
                self.0.push(name.name.to_string());
            }
            true
        }
    }
    let mut v = Procs(Vec::new());
    walk_block(&mut v, program);
    v.0
}

/// Whether the `RUN` has an `ON [SERVER] h` phrase: the callee runs in
/// another session.
fn on_server(source: &str, span: Span) -> bool {
    let text = source_of(source, span).unwrap_or("");
    let mut depth = 0u32;
    for t in Lexer::new(text).take_while(|t| t.kind != Kind::Eof) {
        match t.kind {
            Kind::LeftParen => depth += 1,
            Kind::RightParen => depth = depth.saturating_sub(1),
            Kind::On if depth == 0 => return true,
            _ => {}
        }
    }
    false
}
