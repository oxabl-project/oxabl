//! `untrusted-xml` (ABLSEC015), group `security`.
//!
//! Fires where the program parses XML. This is a review prompt, not a defect:
//! an XML parser that reads a document from outside can be made to read local
//! files or call other servers through external entities (XXE), or to run out
//! of memory on a document with nested entities (the "billion laughs" case).
//! Whether the parser in use is safe depends on its configuration, which this
//! rule cannot see, so findings are `info`.
//!
//! # The set
//!
//! | Form | Reported when |
//! |---|---|
//! | `handle:LOAD("file" or "memptr" or "longchar", source, ...)` (`X-DOCUMENT`) | `source` is not a literal |
//! | `handle:SAX-PARSE()`, `handle:SAX-PARSE-FIRST()` (`SAX-READER`) | always: the input is set by an earlier call, which the rule does not follow |
//!
//! ```abl
//! hDoc:LOAD("longchar", lcRequest, FALSE).
//! ```
//!
//! Review — reject documents that have a DTD or a `DOCTYPE`, and do not turn on
//! entity or schema resolution for input from outside. Check the size of the
//! document before the parse.
//!
//! # Where the rule looks
//!
//! A `LOAD` call with at least two arguments, where the first is the literal
//! `file`, `memptr` or `longchar`. The name is matched by spelling, since the
//! parser has no types. `SAX-PARSE-NEXT` continues a parse and is not reported.
//! `READ-XML` on a dataset or temp-table is not reported.

use oxabl_ast::{Span, Statement};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{Call, CallKind, Site, scan};
use super::taint::{Gate, SinkKind};
use super::{ABLSEC015, finding, meta};
use crate::registry::RuleMeta;
use crate::shared::RunShared;

/// Kebab-case rule name.
pub const NAME: &str = "untrusted-xml";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Info;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "3.0";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "XML is parsed with X-DOCUMENT:LOAD or SAX-READER; review entity handling";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Reject documents with a DTD or `DOCTYPE`, do not resolve external entities or \
     schemas for input from outside, and check the size of the document before the parse. \
     This finding is a prompt for review: the check cannot see the reader's configuration.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC015,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// The source kinds that `X-DOCUMENT:LOAD` takes as its first argument.
const SOURCES: [&str; 3] = ["file", "memptr", "longchar"];

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    check(program, ctx, &RunShared::default())
}

/// [`run`] with the facts of a registry run.
pub fn check(program: &[Statement], ctx: &AnalysisContext, shared: &RunShared) -> Vec<Diagnostic> {
    let gate = Gate::new(ABLSEC015, NAME, program, ctx, shared);
    let mut diags = Vec::new();
    scan(ctx, program, &["LOAD", "SAX-PARSE"], &mut |site| {
        let Site::Call(call) = site else {
            return;
        };
        let Some(what) = sink(&call) else {
            return;
        };
        // `SAX-PARSE` reads a source set elsewhere: it stays a review.
        let operands: Vec<Span> = (what == "LOAD")
            .then(|| call.args.get(1).map(|a| a.span))
            .flatten()
            .into_iter()
            .collect();
        let Some(j) = gate.judge(&operands, SinkKind::Xml) else {
            return;
        };
        let message = if j.tainted {
            format!(
                "`{what}` parses XML from {}; review entity handling",
                j.subject
            )
        } else {
            format!("`{what}` parses XML that may come from outside; review entity handling")
        };
        let d = finding(ctx, DEFAULT_SEVERITY, ABLSEC015, call.span, message, HELP);
        diags.push(j.attach(d));
    });
    diags
}

fn sink(call: &Call) -> Option<&'static str> {
    if call.kind != CallKind::Method {
        return None;
    }
    if call.is("SAX-PARSE") {
        return Some("SAX-PARSE");
    }
    if call.is("SAX-PARSE-FIRST") {
        return Some("SAX-PARSE-FIRST");
    }
    let literal_kind = call
        .args
        .first()
        .and_then(|a| a.literal.as_deref())
        .is_some_and(|k| SOURCES.iter().any(|s| s.eq_ignore_ascii_case(k)));
    (call.is("LOAD") && literal_kind && call.args.get(1).is_some_and(|a| !a.constant))
        .then_some("LOAD")
}
