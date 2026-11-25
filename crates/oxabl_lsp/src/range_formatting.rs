//! `textDocument/rangeFormatting` and `textDocument/rangesFormatting`: format
//! the statements a selection touches. See `docs/design/range-formatting.md`.
//!
//! The shared [`FormatPipeline`] decides the edits, as it does for the whole
//! document. This module converts the selections to byte ranges of the open
//! buffer and the edits back to positions. The client's `FormattingOptions` stay
//! ignored, as they are for the whole document.
//!
//! A selection that cannot be formatted gives no edits and a
//! `window/showMessage` warning that says why: the file has a parse error, the
//! selection holds no statement, or the formatter had an internal failure.
//! Format on type is not offered; the design note says why.

use lsp_server::{ErrorCode, Request, Response};
use lsp_types::{PositionEncodingKind, Range, TextDocumentIdentifier, TextEdit};
use oxabl_pipeline::FormatPipeline;
use serde::Deserialize;

use crate::Server;
use crate::document::Document;
use crate::position::{byte_to_position, position_to_byte};

/// The LSP 3.18 request for several ranges; `lsp_types` has no type for it.
pub const RANGES_METHOD: &str = "textDocument/rangesFormatting";

/// The `documentRangeFormattingProvider` value: `lsp_types` has no field for
/// `rangesSupport`.
pub fn capability() -> serde_json::Value {
    serde_json::json!({ "rangesSupport": true })
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct RangesParams {
    text_document: TextDocumentIdentifier,
    ranges: Vec<Range>,
}

/// The edits that format `ranges` of an open [`Document`], or the reason the
/// pipeline did not format them.
pub fn compute_range_formatting_edits(
    doc: &Document,
    pipeline: &FormatPipeline,
    encoding: &PositionEncodingKind,
    ranges: &[Range],
) -> Result<Vec<TextEdit>, String> {
    let bytes: Vec<std::ops::Range<usize>> = ranges
        .iter()
        .map(|range| {
            let start = position_to_byte(&doc.rope, range.start, encoding);
            let end = position_to_byte(&doc.rope, range.end, encoding);
            start.min(end)..start.max(end)
        })
        .collect();
    let edits = pipeline
        .format_ranges(&doc.rope.to_string(), &bytes)
        .map_err(|failure| failure.to_string())?;
    Ok(edits
        .into_iter()
        .map(|edit| TextEdit {
            range: Range {
                start: byte_to_position(&doc.rope, edit.start, encoding),
                end: byte_to_position(&doc.rope, edit.end, encoding),
            },
            new_text: edit.new_text,
        })
        .collect())
}

impl Server<'_> {
    /// `textDocument/rangeFormatting` (one range) and `textDocument/rangesFormatting`
    /// (several). With `format` off in `[workspace.features]`, or for a buffer
    /// that is not open, the answer is `[]`, as for the whole document.
    pub(crate) fn handle_range_formatting(
        &self,
        req: Request,
        several: bool,
    ) -> anyhow::Result<()> {
        let parsed: Result<(TextDocumentIdentifier, Vec<Range>), String> = if several {
            serde_json::from_value::<RangesParams>(req.params)
                .map(|p| (p.text_document, p.ranges))
                .map_err(|e| e.to_string())
        } else {
            serde_json::from_value::<lsp_types::DocumentRangeFormattingParams>(req.params)
                .map(|p| (p.text_document, vec![p.range]))
                .map_err(|e| e.to_string())
        };
        let (document, ranges) = match parsed {
            Ok(parsed) => parsed,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    format!("oxabl-lsp: invalid range formatting params: {error}"),
                ));
            }
        };
        let edits = match self
            .documents
            .get(&document.uri)
            .filter(|_| self.features.format)
        {
            Some(doc) => {
                compute_range_formatting_edits(doc, &self.formatter, &self.encoding, &ranges)
            }
            None => Ok(Vec::new()),
        };
        match edits {
            Ok(edits) => self.respond(Response::new_ok(req.id, edits)),
            Err(reason) => {
                let message = format!("oxabl: the selection was not formatted: {reason}");
                self.show_warning(&message);
                self.log_warning(&message);
                self.respond(Response::new_ok(req.id, Vec::<TextEdit>::new()))
            }
        }
    }
}
