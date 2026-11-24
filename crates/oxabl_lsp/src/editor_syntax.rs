//! Folding and selection responses over the shared per-document syntax cache.

use std::collections::HashMap;

use lsp_types::{FoldingRange, FoldingRangeKind, PositionEncodingKind, Range, SelectionRange, Uri};
use oxabl_common::InternalPanic;
use oxabl_pipeline::{DocumentSyntax, FoldKind};
use ropey::Rope;

use crate::position::byte_to_position;

/// The parsed editor facts of each open document, keyed by buffer version.
#[derive(Default)]
pub(crate) struct SyntaxCache {
    entries: HashMap<Uri, (i32, DocumentSyntax)>,
}

impl SyntaxCache {
    pub(crate) fn get_or_compute(
        &mut self,
        uri: &Uri,
        version: i32,
        compute: impl FnOnce() -> Result<DocumentSyntax, InternalPanic>,
    ) -> Result<&DocumentSyntax, InternalPanic> {
        if self
            .entries
            .get(uri)
            .is_none_or(|(cached, _)| *cached != version)
        {
            let syntax = compute()?;
            self.entries.insert(uri.clone(), (version, syntax));
        }
        Ok(&self.entries[uri].1)
    }

    pub(crate) fn invalidate(&mut self, uri: &Uri) {
        self.entries.remove(uri);
    }
}

fn range(rope: &Rope, encoding: &PositionEncodingKind, start: u32, end: u32) -> Range {
    Range {
        start: byte_to_position(rope, start as usize, encoding),
        end: byte_to_position(rope, end as usize, encoding),
    }
}

/// The folds of `syntax`, at most `limit` of them (the client's `rangeLimit`).
pub(crate) fn folding_ranges(
    syntax: &DocumentSyntax,
    limit: Option<usize>,
    rope: &Rope,
    encoding: &PositionEncodingKind,
) -> Vec<FoldingRange> {
    let folds = match limit {
        Some(limit) => syntax.folds_within(limit),
        None => syntax.folds().to_vec(),
    };
    folds
        .iter()
        .map(|fold| {
            let range = range(rope, encoding, fold.span.start, fold.span.end);
            FoldingRange {
                start_line: range.start.line,
                start_character: Some(range.start.character),
                end_line: range.end.line,
                end_character: Some(range.end.character),
                kind: fold.kind.map(|kind| match kind {
                    FoldKind::Comment => FoldingRangeKind::Comment,
                    FoldKind::Region => FoldingRangeKind::Region,
                }),
                collapsed_text: None,
            }
        })
        .collect()
}

pub(crate) fn selection_ranges(
    syntax: &DocumentSyntax,
    offsets: &[u32],
    rope: &Rope,
    encoding: &PositionEncodingKind,
) -> Vec<SelectionRange> {
    offsets
        .iter()
        .map(|offset| {
            let chain = syntax.selection(*offset);
            let mut ranges = chain.iter().rev();
            let first = ranges.next().copied().expect("the file ends every chain");
            let mut response = SelectionRange {
                range: range(rope, encoding, first.start, first.end),
                parent: None,
            };
            for span in ranges {
                response = SelectionRange {
                    range: range(rope, encoding, span.start, span.end),
                    parent: Some(Box::new(response)),
                };
            }
            response
        })
        .collect()
}
