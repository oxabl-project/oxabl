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
//! findings of existing code.
//!
//! # Stage 2: taint mode
//!
//! A rule whose finding depends on where a value comes from also has a taint
//! mode, chosen by `[workspace.lint.security]` (`mode`, `modes`). At each
//! stage 1 finding the rule asks a [`taint::Gate`] with the span of the
//! argument and its [`taint::SinkKind`]. The gate keeps the finding when
//! untrusted input reaches the argument without a sanitiser for that kind,
//! drops it when nothing does, and keeps it as stage 1 when the analysis
//! cannot judge it. So stage 2 only removes findings. A kept finding names its
//! path in the message and carries each step as a label, source first; the
//! labels of a security finding are that path and nothing else, which is how
//! SARIF output reads them. ABLSEC008, ABLSEC009, ABLSEC013 and ABLSEC017 do
//! not ask where a value comes from and have no taint mode. See [`taint`] for
//! the analysis.
//!
//! # Stage 3: entry points
//!
//! With an entry-point file (`[workspace.lint.security.reach]`), a client that
//! reads the workspace builds the call graph from the entries and answers,
//! for the file under analysis, how each routine is reached ([`reach`]). The
//! registry labels every finding of this group with it: the status
//! (reachable, unknown, unreachable), the entries, one path, and a note on the
//! message; the severity moves only when configured. Three rules need more
//! than one file or the identity of a value:
//!
//! - `client-identity` (ABLSEC010): a decision on a user, company or role the
//!   client sent, once per routine and source, in taint mode only;
//! - `missing-authorization` (ABLSEC011): a sensitive operation an entry
//!   reaches past no authorisation check;
//! - `cross-request-state` (ABLSEC012): session state a request reads before
//!   it sets it.
//!
//! ABLSEC011 and ABLSEC012 are not context-free: their answer depends on the
//! files that reach this one.
//!
//! All rules are in preview, all but ABLSEC011 and ABLSEC012 are context-free
//! ([`crate::RuleMeta::context_free`]), and they report in the file where the
//! sink text is written, so a sink inside an
//! include file or a `&GLOBAL-DEFINE` body is reported once, at its origin.
//!
//! # Severity
//!
//! oxabl has `error`, `warning`, `info` and `hint`. A catalogue entry of
//! critical impact reports as `error`. So do ABLSEC001 and ABLSEC008, which are
//! high impact and rarely benign. The other high and medium impact entries
//! (ABLSEC010 and ABLSEC011 high, ABLSEC012 medium among them) report as
//! `warning`: the pattern is often benign (path traversal), or the rule reads
//! names and guesses (weak crypto, information leak). An entry that needs a
//! review (untrusted XML) or that depends on a limit the rule cannot see
//! (resource exhaustion) reports as `info`. The impact word is not carried into
//! the diagnostic; SARIF output carries it as `security-severity`: `9.5`
//! critical, `8.0` high, `5.5` medium, `3.0` needs review.
//!
//! # Finding spans
//!
//! A finding starts at the sink keyword or name (the method, the function, the
//! statement keyword, the assigned target) and not at its argument. When an
//! argument is the expansion of a preprocessor name, its own text does not
//! belong to the file, and a finding that starts there is not reported.
//!
//! # Rules that read a literal, a name, or a statement
//!
//! Most rules ask "is the argument a literal?". Three differ:
//!
//! - `hardcoded-secret` (ABLSEC008) reports a *literal*, and never prints it.
//! - `weak-crypto` (ABLSEC009), `information-leak` (ABLSEC013) and
//!   `hardcoded-secret` guess from the *name* of a value. The word lists are in
//!   [`names`], and a program that embeds the linter can replace them.
//! - `information-leak` follows error text through one statement, to an output
//!   parameter or a web response ([`web`]).
//!
//! The rules that match a call by name share [`calls`], which gives one view of
//! a call in a modelled expression and in a skipped statement.
//!
//! A class name from data is owned by `dynamic-code` (ABLSEC004).
//! `unsafe-deserialization` (ABLSEC014) reports `Deserialize` only, so no call
//! has a finding from two rules of this group.
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
//! literal" decision, so every rule agrees on it, and [`calls`] to find calls.
//! A rule about where a value comes from takes a [`taint::Gate`] and registers
//! with its `check` entry point. Add a corpus under `tests/security_corpus`
//! (and `tests/security_corpus/taint` for a taint mode) and a line to its
//! precision note.

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::AnalysisContext;

use super::syntactic::file_span;
use crate::registry::{RuleMeta, Stability};

pub mod calls;
pub mod client_identity;
pub mod cross_request_state;
pub mod dynamic_code;
pub mod dynamic_query;
pub mod foreign_sql;
pub mod hardcoded_secret;
pub mod header_injection;
pub mod information_leak;
pub mod integrity_bypass;
pub mod literal;
pub mod missing_authorization;
pub mod names;
pub mod os_command;
pub mod path_traversal;
pub mod reach;
pub mod resource_exhaustion;
pub mod taint;
pub mod unsafe_deserialization;
pub mod untrusted_xml;
pub mod weak_crypto;
pub mod web;
pub mod xss_output;

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

// --- Stage 1, ABLSEC006-ABLSEC009 and ABLSEC013-ABLSEC017 ------------------

/// Diagnostic code for `xss-output` (ABLSEC006).
pub const ABLSEC006: &str = "ABLSEC006";
/// Diagnostic code for `header-injection` (ABLSEC007).
pub const ABLSEC007: &str = "ABLSEC007";
/// Diagnostic code for `hardcoded-secret` (ABLSEC008).
pub const ABLSEC008: &str = "ABLSEC008";
/// Diagnostic code for `weak-crypto` (ABLSEC009).
pub const ABLSEC009: &str = "ABLSEC009";
/// Diagnostic code for `information-leak` (ABLSEC013).
pub const ABLSEC013: &str = "ABLSEC013";
/// Diagnostic code for `unsafe-deserialization` (ABLSEC014).
pub const ABLSEC014: &str = "ABLSEC014";
/// Diagnostic code for `untrusted-xml` (ABLSEC015).
pub const ABLSEC015: &str = "ABLSEC015";
/// Diagnostic code for `resource-exhaustion` (ABLSEC016).
pub const ABLSEC016: &str = "ABLSEC016";
/// Diagnostic code for `integrity-bypass` (ABLSEC017).
pub const ABLSEC017: &str = "ABLSEC017";

// --- Stage 3, entry points: ABLSEC010-ABLSEC012 ------------------------------

/// Diagnostic code for `client-identity` (ABLSEC010).
pub const ABLSEC010: &str = "ABLSEC010";
/// Diagnostic code for `missing-authorization` (ABLSEC011).
pub const ABLSEC011: &str = "ABLSEC011";
/// Diagnostic code for `cross-request-state` (ABLSEC012).
pub const ABLSEC012: &str = "ABLSEC012";

/// Metadata shared by the security rules: preview, context-free, and carrying
/// the safe pattern and the security severity for output formats that show
/// them. `NAME` and `GROUP` stay constants of each rule module.
pub(super) const fn meta(
    code: &'static str,
    name: &'static str,
    description: &'static str,
    severity: Severity,
    security_severity: &'static str,
    help: &'static str,
) -> RuleMeta {
    RuleMeta::new(code, name, "security", description)
        .with_default_severity(severity)
        .with_stability(Stability::Preview)
        .with_help(help)
        .with_security_severity(security_severity)
        .context_free()
}

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
        Severity::Info => Diagnostic::info(code, message, span),
        _ => Diagnostic::warning(code, message, span),
    };
    diagnostic.with_help(help.to_string())
}
