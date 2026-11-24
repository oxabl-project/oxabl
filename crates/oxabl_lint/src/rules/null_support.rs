//! The related locations of the `strict.nulls` rules: where the `?` of a use
//! may come from. The rules select findings from `oxabl_nulls`; nothing is
//! analysed here.

use oxabl_ast::Span;
use oxabl_common::Diagnostic;
use oxabl_nulls::{ValueSourceKind, ValueUse};
use oxabl_semantic::AnalysisContext;

use super::syntactic::file_span;

/// The most sources shown for one finding. The last one says how many more
/// there are.
const MAX_SOURCES: usize = 8;

/// `d` with a label at each place the `?` of `u` may come from.
pub(super) fn with_source_labels(
    mut d: Diagnostic,
    ctx: &AnalysisContext,
    u: &ValueUse,
) -> Diagnostic {
    let Some(sources) = &u.sources else {
        return d;
    };
    let name = &u.name;
    let placed: Option<Vec<(Span, String)>> = sources
        .iter()
        .map(|source| {
            Some(match source.kind {
                ValueSourceKind::Declaration => (source.at, format!("`{name}` starts as `?` here")),
                ValueSourceKind::Assignment => {
                    (source.at, format!("this may assign `?` to `{name}`"))
                }
                ValueSourceKind::Test => {
                    (source.at, format!("`{name}` may be `?` after this test"))
                }
                ValueSourceKind::UnitStart => (
                    first_line(ctx.source, source.at),
                    format!("`{name}` may already be `?` when this unit starts"),
                ),
                ValueSourceKind::RecordRead => (
                    first_line(ctx.source, source.at),
                    format!("`{name}` may be `?` in the record read here"),
                ),
                // A kind this rule cannot word: the list would be partial.
                _ => return None,
            })
        })
        .collect();
    let Some(placed) = placed else {
        return d;
    };
    let total = placed.len();
    for (k, (at, mut message)) in placed.into_iter().take(MAX_SOURCES).enumerate() {
        if k + 1 == MAX_SOURCES && total > MAX_SOURCES {
            message.push_str(&format!(" (and {} more)", total - MAX_SOURCES));
        }
        d = d.with_label(file_span(ctx, at), message);
    }
    d
}

/// `span` up to the end of its first line, so a block header does not take
/// in the whole block.
pub(super) fn first_line(source: &str, span: Span) -> Span {
    let Some(text) = source.get(span.start as usize..span.end as usize) else {
        return span;
    };
    let line = text.split('\n').next().unwrap_or(text).trim_end();
    Span {
        start: span.start,
        end: span.start + line.len() as u32,
    }
}
