//! The `security` rule group: source → sink → sanitiser checks for injection
//! and unsafe-resource bugs, off until `enable = ["security"]`.
//!
//! # Stage 1: sink rules
//!
//! A sink rule reports every call that takes a value it cannot prove constant.
//! "Constant" is decided by [`literal`]: a string or number literal, a
//! concatenation of those, and anything that expanded from a preprocessor name
//! to one (the rules run on preprocessed text, so `{&NAME}` is already the text
//! of its definition). A variable, a field, a function call and `VALUE(x)` of a
//! non-constant are all findings.
//!
//! The rules are complete and noisy on purpose. They do not follow a value
//! back to where it came from, and they do not know a sanitiser: `QUOTER(x)`
//! around a value still leaves the string non-constant. A baseline holds the
//! findings of existing code; later stages add taint tracking inside a
//! procedure and reachability from entry points.
//!
//! All rules are in preview, context-free ([`crate::RuleMeta::context_free`])
//! and report in the file where the sink text is written, so a sink inside an
//! include file or a `&GLOBAL-DEFINE` body is reported once, at its origin.
//!
//! # Severity
//!
//! oxabl has `error`, `warning`, `info` and `hint`. A catalogue entry of
//! critical or high impact reports as `error`, except where the pattern is
//! often benign (path traversal), which reports as `warning`. The impact word
//! is not carried into the diagnostic.
//!
//! # Where the parser does not model a statement
//!
//! `OS-COMMAND`, `UNIX`, `DOS`, `OS-DELETE`, `OS-COPY`, `OS-RENAME`,
//! `OS-CREATE-DIR`, `COPY-LOB` and `COMPILE` are `StatementKind::Skipped`. The
//! rules lex the statement text and read its `VALUE(...)` operands, so a sink
//! in a clause they do not name is not seen. The same lexical read covers
//! dynamic calls inside other skipped statements.
//!
//! # Adding a rule
//!
//! Put the rule in its own file, name it in the block for its stage below, and
//! register it in `registry.rs`. Reuse [`literal`] for the "is this a
//! literal" decision, so every rule agrees on it.

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::AnalysisContext;

use super::syntactic::file_span;

pub mod dynamic_code;
pub mod dynamic_query;
pub mod foreign_sql;
pub mod literal;
pub mod os_command;
pub mod path_traversal;

// --- Stage 1, sink rules: ABLSEC001-ABLSEC005 -------------------------------

/// Diagnostic code for `dynamic-query` (ABLSEC001).
pub const ABLSEC001: &str = "ABLSEC001";
/// Diagnostic code for `foreign-sql` (ABLSEC002).
pub const ABLSEC002: &str = "ABLSEC002";
/// Diagnostic code for `os-command` (ABLSEC003).
pub const ABLSEC003: &str = "ABLSEC003";
/// Diagnostic code for `dynamic-code` (ABLSEC004).
pub const ABLSEC004: &str = "ABLSEC004";
/// Diagnostic code for `path-traversal` (ABLSEC005).
pub const ABLSEC005: &str = "ABLSEC005";

// --- Stage 1, ABLSEC006+: modules and codes are added below this line ------

/// A finding at `span` with its suggestion. The registry replaces `severity`
/// with the configured one, so a rule passes its own default.
pub(super) fn finding(
    ctx: &AnalysisContext,
    severity: Severity,
    code: &'static str,
    span: Span,
    message: String,
    help: &str,
) -> Diagnostic {
    let span = file_span(ctx, span);
    let diagnostic = match severity {
        Severity::Error => Diagnostic::error(code, message, span),
        _ => Diagnostic::warning(code, message, span),
    };
    diagnostic.with_help(help.to_string())
}
