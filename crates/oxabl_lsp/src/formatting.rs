//! `textDocument/formatting` support: render the shared format pipeline's one
//! outcome as LSP edits (R1–R7).
//!
//! [`compute_formatting_edits`] is the whole module — a pure, server-loop-free
//! function so it is unit-testable without a running connection. It hands the
//! open buffer's raw bytes to a caller-supplied [`FormatPipeline`] and maps the
//! resulting [`FormatOutcome`]: [`Reformatted`](FormatOutcome::Reformatted) into
//! exactly one whole-document [`TextEdit`], and **both** leave-it-alone arms
//! (`Unchanged`, `DidNotFormat`) into no edits at all, so the editor never sees a
//! partial rewrite.
//!
//! # What this module no longer does
//!
//! It used to resolve the style itself, per request, by walking up from the
//! document's path to the nearest `oxabl.toml` — a second config path that read
//! the same file the diagnostics side had already read, and reported a malformed
//! one to nobody. The style now arrives inside the pipeline handle the server
//! built when it resolved configuration once (KTD3), which is also why this
//! function no longer needs the document's URI.
//!
//! Two properties survive that move and are load-bearing:
//!
//! * The formatter sees **raw** source. It is not a discipline here but a
//!   property of the type: [`FormatPipeline`] has no filesystem, no include
//!   path, and no preprocess switch, so there is no way for it to see expanded
//!   macro text whose byte offsets would rewrite the wrong bytes (KTD4).
//! * The client's `FormattingOptions` (tab size, insert-spaces) stay ignored.
//!   The resolved style owns indentation; honoring both would give one buffer two
//!   answers depending on which surface asked.

use lsp_types::{Position, PositionEncodingKind, Range, TextEdit};
use oxabl_pipeline::{FormatOutcome, FormatPipeline};

use crate::document::Document;
use crate::position::byte_to_position;

/// Compute the `textDocument/formatting` edits for an already-fetched open
/// [`Document`], formatting through `pipeline` (R2, R4, R5).
///
/// On [`FormatOutcome::Reformatted`] it returns exactly one whole-document
/// [`TextEdit`] replacing `(0,0)..end` with the formatted string; the end
/// position is derived from the **rope** under the negotiated `encoding`, which
/// is the only oracle that gets a UTF-16 column right on a line holding a
/// multi-byte character (KTD5).
///
/// On [`Unchanged`](FormatOutcome::Unchanged) and on
/// [`DidNotFormat`](FormatOutcome::DidNotFormat) — a bail, a parse-dirty buffer,
/// or a contained internal panic — it returns an **empty** edit list, so the
/// editor leaves the buffer exactly as the user typed it. A silent no-op is both
/// the least-surprising editor behavior and the "never mangle" safety property;
/// the two are deliberately not distinguished here, because there is no correct
/// edit to send in either case.
pub fn compute_formatting_edits(
    doc: &Document,
    pipeline: &FormatPipeline,
    encoding: &PositionEncodingKind,
) -> Vec<TextEdit> {
    match pipeline.format(&doc.rope.to_string()) {
        FormatOutcome::Reformatted(formatted) => {
            let end = byte_to_position(&doc.rope, doc.rope.len_bytes(), encoding);
            vec![TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end,
                },
                new_text: formatted,
            }]
        }
        FormatOutcome::Unchanged | FormatOutcome::DidNotFormat(_) => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use oxabl_style::{IndentStyle, StyleGuide};
    use ropey::Rope;

    fn doc(text: &str) -> Document {
        Document {
            rope: Rope::from_str(text),
            version: 1,
        }
    }

    fn base() -> FormatPipeline {
        FormatPipeline::new(StyleGuide::default_base())
    }

    fn utf8() -> PositionEncodingKind {
        PositionEncodingKind::UTF8
    }

    #[test]
    fn reformattable_buffer_yields_single_whole_document_edit() {
        // Mis-indented block that the formatter will re-indent.
        let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
        let d = doc(src);
        let pipeline = base();
        let edits = compute_formatting_edits(&d, &pipeline, &utf8());

        let expected = pipeline
            .format(src)
            .output()
            .expect("fixture must actually reformat")
            .to_string();
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, expected);
        // Range covers the whole buffer: (0,0)..byte_to_position(len_bytes).
        assert_eq!(
            edits[0].range.start,
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            edits[0].range.end,
            byte_to_position(&d.rope, d.rope.len_bytes(), &utf8())
        );
    }

    #[test]
    fn already_formatted_buffer_yields_no_edits() {
        let pipeline = base();
        let formatted = pipeline
            .format("IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n")
            .output()
            .expect("fixture must reformat")
            .to_string();
        // Formatting the already-formatted output is idempotent → no edits.
        let edits = compute_formatting_edits(&doc(&formatted), &pipeline, &utf8());
        assert!(edits.is_empty(), "idempotent buffer must yield no edits");
    }

    #[test]
    fn parse_error_buffer_yields_no_edits() {
        // Unterminated DO block → a bail → no edits, bytes untouched.
        let src = "DO:\n  MESSAGE \"hi\".\n";
        let pipeline = base();
        assert!(
            pipeline.format(src).not_formatted().is_some(),
            "fixture must make the formatter decline"
        );
        assert!(compute_formatting_edits(&doc(src), &pipeline, &utf8()).is_empty());
    }

    #[test]
    fn garbage_buffer_yields_no_edits_no_panic() {
        // Parse-dirty garbage the parser recovers into an errored program.
        let src = "@#$ %^& THEN DO END .. .\n((((\n";
        assert!(compute_formatting_edits(&doc(src), &base(), &utf8()).is_empty());
    }

    #[test]
    fn multibyte_end_position_matches_encoding() {
        // A reformattable buffer containing a multibyte char in a comment, so
        // the whole-document end position differs by encoding.
        let src = "/* héllo 😀 */\nIF TRUE THEN DO:\nMESSAGE \"x\".\nEND.\n";
        let d = doc(src);
        let pipeline = base();
        assert!(pipeline.format(src).changed());

        for enc in [PositionEncodingKind::UTF8, PositionEncodingKind::UTF16] {
            let edits = compute_formatting_edits(&d, &pipeline, &enc);
            assert_eq!(edits.len(), 1, "enc {enc:?}");
            assert_eq!(
                edits[0].range.end,
                byte_to_position(&d.rope, d.rope.len_bytes(), &enc),
                "enc {enc:?}"
            );
        }
    }

    /// The style comes from the handle the server built when it resolved
    /// configuration — not from the document's path, and not from the client's
    /// `FormattingOptions`. Two pipelines over the same buffer must therefore
    /// disagree.
    #[test]
    fn the_handles_style_drives_the_edit() {
        let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
        let d = doc(src);

        let four = compute_formatting_edits(&d, &base(), &utf8());
        let mut two = StyleGuide::default_base();
        two.indent_size = 2;
        two.indent_style = IndentStyle::Spaces;
        let two = compute_formatting_edits(&d, &FormatPipeline::new(two), &utf8());

        assert_eq!(four.len(), 1);
        assert_eq!(two.len(), 1);
        assert!(four[0].new_text.contains("    MESSAGE"));
        assert!(two[0].new_text.contains("\n  MESSAGE"));
        assert_ne!(four[0].new_text, two[0].new_text);
    }
}
