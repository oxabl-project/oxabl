//! `textDocument/formatting` support (R1–R7).
//!
//! Two pure, server-loop-free pieces so both are unit-testable without a
//! running connection:
//!
//! - [`style_for_uri`] maps a document URI to the [`StyleGuide`] the CLI would
//!   apply for that file's path — the *same* `oxabl.toml [workspace.style]`
//!   surface (`resolved_style`), with a safe `default_base()` fallback (R3,
//!   KTD2). No new LSP config surface is introduced.
//! - [`compute_formatting_edits`] parses the open buffer **raw** (preprocessor
//!   OFF — KTD1) exactly like the CLI's `format_one`, runs the formatter under
//!   a panic guard (KTD4), and returns either a single whole-document
//!   [`TextEdit`] (KTD5) or **no edits** on any non-success (bail / unchanged /
//!   parse-dirty / panic — KTD3, R4/R5).
//!
//! The handler deliberately does **not** touch the salsa `expanded_text` /
//! `collect_from_expanded` diagnostics query: that path is preprocessor-*on*
//! and would reformat macro output, not the user's buffer (KTD1, R-risk).

use lsp_types::{Position, PositionEncodingKind, Range, TextEdit, Uri};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_style::StyleGuide;
use oxabl_workspace::resolved_style;

use crate::document::Document;
use crate::position::byte_to_position;
use crate::uri_to_path;

/// Resolve the [`StyleGuide`] to format `uri` with (R3, KTD2).
///
/// A `file://` URI is converted to a filesystem path (reusing the crate's
/// [`uri_to_path`] convention — no second decoder) and handed to
/// [`resolved_style`], which walks up to the nearest `oxabl.toml
/// [workspace.style]`. A non-`file` URI or a path that fails to convert falls
/// back to [`StyleGuide::default_base`]. A config-discovery error from
/// `resolved_style` is non-fatal: it already returns `default_base()` in that
/// case, so we simply use the returned guide (KTD2).
pub fn style_for_uri(uri: &Uri) -> StyleGuide {
    match uri_to_path(uri) {
        Some(path) => {
            let (style, _cfg_err) = resolved_style(&path, None);
            style
        }
        None => StyleGuide::default_base(),
    }
}

/// Compute the `textDocument/formatting` edits for an already-fetched open
/// [`Document`] (R2, R4, R5).
///
/// Mirrors the CLI's `format_one`: tokenize → raw `parse_program()` →
/// [`oxabl_formatter::format`], all inside `catch_unwind` (KTD1, KTD4). On
/// success *with changed output* it returns exactly one whole-document
/// [`TextEdit`] replacing `(0,0)..end` with the formatted string (KTD5). On a
/// [`FormatBail`](oxabl_formatter::FormatBail), unchanged output, parse-dirty
/// input the formatter bails on, or a panic, it returns an **empty** edit list
/// so the editor leaves the buffer untouched (KTD3) — never a partial rewrite.
///
/// `params.options` (editor tab-size / insert-spaces) is intentionally ignored:
/// the resolved [`StyleGuide`] owns indentation (see plan Scope Boundaries).
pub fn compute_formatting_edits(
    doc: &Document,
    uri: &Uri,
    encoding: &PositionEncodingKind,
) -> Vec<TextEdit> {
    let text = doc.rope.to_string();
    let style = style_for_uri(uri);

    // Parse RAW (preprocessing off) — the formatter must see the user's actual
    // buffer, not preprocessor-expanded text (KTD1). The whole pipeline is
    // panic-guarded so a formatter/lexer panic degrades to "no edits" rather
    // than killing the server's main loop (KTD4).
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let tokens = tokenize(&text);
        let program = Parser::new(&tokens, &text).parse_program();
        oxabl_formatter::format(&text, &program, &style)
    }));

    match result {
        // Changed output → one whole-document replace (KTD5). The returned text
        // is exactly what the formatter produced, whose re-lex guard already
        // proved the token stream is preserved (R5).
        Ok(Ok(formatted)) if formatted != text => {
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
        // Unchanged (idempotent no-op), any bail, or a panic → no edits (KTD3,
        // R4). A silent no-op is the least-surprising editor behavior and the
        // "never mangle" safety property.
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use oxabl_style::StyleGuide;
    use ropey::Rope;
    use std::str::FromStr;
    use tempfile::TempDir;

    fn file_uri(path: &std::path::Path) -> Uri {
        Uri::from_str(&format!("file://{}", path.display())).unwrap()
    }

    fn doc(text: &str) -> Document {
        Document {
            rope: Rope::from_str(text),
            version: 1,
        }
    }

    fn utf8() -> PositionEncodingKind {
        PositionEncodingKind::UTF8
    }

    // --- U2: style_for_uri -------------------------------------------------

    #[test]
    fn style_for_uri_discovers_workspace_style() {
        let tmp = TempDir::new().unwrap();
        // A distinguishing style: 2-space indentation (base default is 4).
        std::fs::write(
            tmp.path().join("oxabl.toml"),
            "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 2\n",
        )
        .unwrap();
        let file = tmp.path().join("main.p");
        std::fs::write(&file, "").unwrap();

        let style = style_for_uri(&file_uri(&file));
        // Compare via the to_toml() projection (StyleGuide has no PartialEq),
        // matching the oxabl_style / oxabl_workspace test convention.
        let expected = {
            let (s, _err) = resolved_style(&file, None);
            s
        };
        assert_eq!(style.to_toml().unwrap(), expected.to_toml().unwrap());
        // And it is *not* the base default (the table took effect).
        assert_ne!(
            style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    #[test]
    fn style_for_uri_defaults_without_config() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("main.p");
        std::fs::write(&file, "").unwrap();
        let style = style_for_uri(&file_uri(&file));
        assert_eq!(
            style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    #[test]
    fn style_for_uri_non_file_scheme_defaults() {
        let uri = Uri::from_str("untitled:Untitled-1").unwrap();
        let style = style_for_uri(&uri);
        assert_eq!(
            style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    #[test]
    fn style_for_uri_malformed_path_defaults() {
        // A percent-encoded path defeats the bare `file://` strip → None →
        // default_base(), no panic.
        let uri = Uri::from_str("file:///nonexistent/does-not-exist-xyz.p").unwrap();
        let style = style_for_uri(&uri);
        // No oxabl.toml on any ancestor of a nonexistent absolute path.
        assert_eq!(
            style.to_toml().unwrap(),
            StyleGuide::default_base().to_toml().unwrap()
        );
    }

    // --- U3: compute_formatting_edits -------------------------------------

    fn direct_format(
        text: &str,
        style: &StyleGuide,
    ) -> Result<String, oxabl_formatter::FormatBail> {
        let tokens = tokenize(text);
        let program = Parser::new(&tokens, text).parse_program();
        oxabl_formatter::format(text, &program, style)
    }

    #[test]
    fn reformattable_buffer_yields_single_whole_document_edit() {
        // Mis-indented block that the formatter will re-indent.
        let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
        let uri = Uri::from_str("file:///buf.p").unwrap();
        let d = doc(src);
        let edits = compute_formatting_edits(&d, &uri, &utf8());

        let expected = direct_format(src, &StyleGuide::default_base()).unwrap();
        assert_ne!(expected, src, "fixture must actually reformat");
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
        let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
        let formatted = direct_format(src, &StyleGuide::default_base()).unwrap();
        // Formatting the already-formatted output is idempotent → no edits.
        let uri = Uri::from_str("file:///buf.p").unwrap();
        let edits = compute_formatting_edits(&doc(&formatted), &uri, &utf8());
        assert!(edits.is_empty(), "idempotent buffer must yield no edits");
    }

    #[test]
    fn parse_error_buffer_yields_no_edits() {
        // Unterminated DO block → ParseErrors bail → no edits, bytes untouched.
        let src = "DO:\n  MESSAGE \"hi\".\n";
        assert!(direct_format(src, &StyleGuide::default_base()).is_err());
        let uri = Uri::from_str("file:///buf.p").unwrap();
        let edits = compute_formatting_edits(&doc(src), &uri, &utf8());
        assert!(edits.is_empty());
    }

    #[test]
    fn garbage_buffer_yields_no_edits_no_panic() {
        // Parse-dirty garbage the parser recovers into an errored program.
        let src = "@#$ %^& THEN DO END .. .\n((((\n";
        let uri = Uri::from_str("file:///buf.p").unwrap();
        let edits = compute_formatting_edits(&doc(src), &uri, &utf8());
        assert!(edits.is_empty());
    }

    #[test]
    fn multibyte_end_position_matches_encoding() {
        // A reformattable buffer containing a multibyte char in a comment, so
        // the whole-document end position differs by encoding.
        let src = "/* héllo 😀 */\nIF TRUE THEN DO:\nMESSAGE \"x\".\nEND.\n";
        let uri = Uri::from_str("file:///buf.p").unwrap();
        let d = doc(src);
        let expected = direct_format(src, &StyleGuide::default_base()).unwrap();
        assert_ne!(expected, src);

        for enc in [PositionEncodingKind::UTF8, PositionEncodingKind::UTF16] {
            let edits = compute_formatting_edits(&d, &uri, &enc);
            assert_eq!(edits.len(), 1, "enc {enc:?}");
            assert_eq!(
                edits[0].range.end,
                byte_to_position(&d.rope, d.rope.len_bytes(), &enc),
                "enc {enc:?}"
            );
        }
    }
}
