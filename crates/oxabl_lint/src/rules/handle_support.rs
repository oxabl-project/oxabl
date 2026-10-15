//! Wording and anchoring shared by the `strict.handles` rules (HD001-HD005).
//! The rules select findings from `oxabl_handles`; nothing is analysed here.

use oxabl_ast::Span;
use oxabl_common::Diagnostic;
use oxabl_flow::UnitKind;
use oxabl_handles::{Leak, LeakKind, Object, ObjectKind, UnitHandles};
use oxabl_semantic::AnalysisContext;

use super::syntactic::{file_span, source_of};
use crate::fragment::FRAGMENT_NOTE;

/// Longest statement text quoted in a message.
const QUOTE_MAX: usize = 70;

/// The kind of an object, for a sentence.
pub(super) fn noun(kind: ObjectKind) -> &'static str {
    match kind {
        ObjectKind::Dynamic => "dynamic object",
        ObjectKind::Automation => "COM object",
        ObjectKind::PersistentProcedure => "persistent procedure",
        other => other.as_str(),
    }
}

/// The object as a message names it: `` the query in `hQuery` ``.
pub(super) fn describe(o: &Object) -> String {
    match &o.holder {
        Some(h) => format!("the {} in `{h}`", noun(o.kind)),
        None => format!("the {}", noun(o.kind)),
    }
}

/// The unit as a message names it.
pub(super) fn scope(unit: &UnitHandles) -> String {
    match (&unit.name, unit.kind) {
        (Some(name), UnitKind::Procedure) => format!("procedure `{name}`"),
        (Some(name), UnitKind::Function) => format!("function `{name}`"),
        (Some(name), UnitKind::Method) => format!("method `{name}`"),
        (Some(name), _) => format!("`{name}`"),
        (None, UnitKind::Main) => "the main block".to_string(),
        (None, _) => "this block".to_string(),
    }
}

/// The statement at `span` on one line, without block comments, cut at
/// [`QUOTE_MAX`] characters.
pub(super) fn quote(source: &str, span: Span) -> String {
    let text = source_of(source, span).unwrap_or("");
    let mut flat = String::new();
    let mut rest = text;
    while let Some(open) = rest.find("/*") {
        flat.push_str(&rest[..open]);
        flat.push(' ');
        rest = match rest[open + 2..].find("*/") {
            Some(close) => &rest[open + 2 + close + 2..],
            None => "",
        };
    }
    flat.push_str(rest);
    let mut out = String::new();
    for word in flat.split_whitespace() {
        if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(word);
    }
    let end = out.trim_end_matches('.').len();
    out.truncate(end);
    if out.chars().count() > QUOTE_MAX {
        let cut = out
            .char_indices()
            .nth(QUOTE_MAX)
            .map_or(out.len(), |(i, _)| i);
        out.truncate(cut);
        out.push_str("...");
    }
    out
}

/// Whether the file is left unjudged. Parse recovery can close a unit early
/// and move a free out of it. An include fragment is exempt: it is partial by
/// nature, its parse often recovers, and its findings are reported once, in
/// the include.
pub(super) fn not_judged(ctx: &AnalysisContext) -> bool {
    ctx.parse_recovered && !ctx.source_context.is_include_fragment()
}

/// The first leak that happens without a raised condition.
pub(super) fn first_normal_leak(o: &Object) -> Option<&Leak> {
    o.leaks.iter().find(|l| l.normal)
}

/// The leak that makes a normal-path finding: the first exit with the object
/// live, else the first overwrite.
pub(super) fn reported_leak(o: &Object) -> Option<&Leak> {
    o.leaks
        .iter()
        .find(|l| l.normal && matches!(l.kind, LeakKind::Exit(_)))
        .or_else(|| first_normal_leak(o))
}

/// A leak that an including file could undo: the object is live at the end of
/// the main block of an include, and the including file may free it later.
pub(super) fn includer_may_free(ctx: &AnalysisContext, unit: &UnitHandles, leak: &Leak) -> bool {
    ctx.source_context.is_include_fragment()
        && unit.kind == UnitKind::Main
        && matches!(leak.kind, LeakKind::Exit(_))
}

/// Add the includer note to the help of `d` when `lowered`.
pub(super) fn with_fragment_note(mut d: Diagnostic, lowered: bool) -> Diagnostic {
    if lowered {
        d.help = Some(match d.help.take() {
            Some(help) => format!("{help} {FRAGMENT_NOTE}"),
            None => FRAGMENT_NOTE.to_string(),
        });
    }
    d
}

/// A warning at `span` of the file under analysis.
pub(super) fn warning(
    ctx: &AnalysisContext,
    code: &'static str,
    message: String,
    help: String,
    span: Span,
) -> Diagnostic {
    Diagnostic::warning(code, message, file_span(ctx, span)).with_help(help)
}
