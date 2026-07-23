//! Map oxabl diagnostics to LSP diagnostics (U5).
//!
//! The shared collector resolves every diagnostic to a **root-buffer byte
//! span** (dropping include-origin ones, R8). This module only converts those
//! byte spans to LSP [`Range`]s under the negotiated position encoding and maps
//! severity/code — the analysis is already done. `oxabl_common::Diagnostic`
//! is documented as mapping 1:1 to the LSP shape.

use lsp_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity, NumberOrString, Range};
use oxabl_analyze::CollectedDiagnostics;
use oxabl_common::{Diagnostic, Severity};
use ropey::Rope;

use crate::position::byte_to_position;

/// The `source` field stamped on every published diagnostic.
const SOURCE: &str = "oxabl";

/// Convert a collected diagnostic set to LSP diagnostics, positioning each span
/// against `rope` in the negotiated `encoding`.
pub fn to_lsp_diagnostics(
    collected: &CollectedDiagnostics,
    rope: &Rope,
    encoding: &lsp_types::PositionEncodingKind,
) -> Vec<LspDiagnostic> {
    collected
        .all()
        .map(|c| to_lsp(&c.diagnostic, rope, encoding))
        .collect()
}

fn to_lsp(
    d: &Diagnostic,
    rope: &Rope,
    encoding: &lsp_types::PositionEncodingKind,
) -> LspDiagnostic {
    let start = byte_to_position(rope, d.span.span.start as usize, encoding);
    let end = byte_to_position(rope, d.span.span.end as usize, encoding);
    LspDiagnostic {
        range: Range { start, end },
        severity: Some(map_severity(d.severity)),
        code: Some(NumberOrString::String(d.code.0.to_string())),
        source: Some(SOURCE.to_string()),
        message: d.message.clone(),
        ..Default::default()
    }
}

/// `Severity` maps 1:1 to `DiagnosticSeverity`.
fn map_severity(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Info => DiagnosticSeverity::INFORMATION,
        Severity::Hint => DiagnosticSeverity::HINT,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Position, PositionEncodingKind};
    use oxabl_analyze::{CollectedDiagnostic, DiagnosticSource};
    use oxabl_ast::Span;
    use oxabl_common::{DiagnosticCode, FileId, FileSpan};

    fn diag(code: &'static str, severity: Severity, start: u32, end: u32) -> CollectedDiagnostic {
        CollectedDiagnostic {
            diagnostic: Diagnostic {
                severity,
                code: DiagnosticCode(code),
                message: format!("{code} here"),
                span: FileSpan {
                    file: FileId::new(1),
                    span: Span { start, end },
                },
                labels: Vec::new(),
                help: None,
            },
            source: DiagnosticSource::Parse,
        }
    }

    fn set(items: Vec<CollectedDiagnostic>) -> CollectedDiagnostics {
        CollectedDiagnostics { diagnostics: items }
    }

    #[test]
    fn maps_range_under_both_encodings_on_multibyte_line() {
        // Line 1 has a 4-byte / 2-cu emoji before the target token 'y'.
        let rope = Rope::from_str("line one\nx😀y = 1.\n");
        // Byte offset of 'y': line 2 starts at byte 9; "x😀" is 1+4=5 bytes → y at 14.
        let y = 9 + 1 + 4;
        let d = set(vec![diag("PARSE001", Severity::Error, y, y + 1)]);

        let utf8 = to_lsp_diagnostics(&d, &rope, &PositionEncodingKind::UTF8);
        assert_eq!(
            utf8[0].range.start,
            Position {
                line: 1,
                character: 5
            } // byte column
        );

        let utf16 = to_lsp_diagnostics(&d, &rope, &PositionEncodingKind::UTF16);
        assert_eq!(
            utf16[0].range.start,
            Position {
                line: 1,
                character: 3
            } // code-unit column
        );
    }

    #[test]
    fn severity_and_code_preserved() {
        let rope = Rope::from_str("abc");
        for (sev, expected) in [
            (Severity::Error, DiagnosticSeverity::ERROR),
            (Severity::Warning, DiagnosticSeverity::WARNING),
            (Severity::Info, DiagnosticSeverity::INFORMATION),
            (Severity::Hint, DiagnosticSeverity::HINT),
        ] {
            let d = set(vec![diag("LINT0002", sev, 0, 1)]);
            let mapped = to_lsp_diagnostics(&d, &rope, &PositionEncodingKind::UTF8);
            assert_eq!(mapped[0].severity, Some(expected));
            assert_eq!(
                mapped[0].code,
                Some(NumberOrString::String("LINT0002".to_string()))
            );
            assert_eq!(mapped[0].source.as_deref(), Some("oxabl"));
        }
    }
}
