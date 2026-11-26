//! Map oxabl diagnostics to LSP diagnostics (U5).
//!
//! The shared collector resolves every diagnostic to a **root-buffer byte
//! span** (dropping include-origin ones, R8). This module only converts those
//! byte spans to LSP [`Range`]s under the negotiated position encoding and maps
//! severity/code — the analysis is already done. `oxabl_common::Diagnostic`
//! is documented as mapping 1:1 to the LSP shape.
//!
//! The labels of a diagnostic become `relatedInformation` when the client
//! advertises `textDocument.publishDiagnostics.relatedInformation`. A label in
//! an include points at the physical include file, positioned against its text
//! as the analysis read it. A label whose file or text is not known is left out.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use lsp_types::{
    Diagnostic as LspDiagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location,
    NumberOrString, Range, Uri,
};
use oxabl_analyze::{CollectedDiagnostics, LabelFile};
use oxabl_common::{Diagnostic, FileSpan, Severity};
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::ContextDiagnostics;
use ropey::Rope;

use crate::path_to_uri;
use crate::position::byte_to_position;

/// The `source` field stamped on every published diagnostic.
const SOURCE: &str = "oxabl";

/// Where the labels of a diagnostic set go, for a client that takes
/// `relatedInformation`. A label in the file of its diagnostic is in the
/// document; any other is in a file of `files`.
struct Related<'a> {
    /// The document the diagnostics are published for.
    uri: &'a Uri,
    files: &'a [LabelFile],
}

/// Places labels, building the rope and resolving the path of each other file
/// once.
struct Placer<'a> {
    related: &'a Related<'a>,
    document: Option<PathBuf>,
    ropes: HashMap<PathBuf, Option<Rope>>,
    /// The canonical path of each file a label pointed into: a file system
    /// call, so one for each file and not one for each label.
    canonical: HashMap<PathBuf, PathBuf>,
}

impl<'a> Placer<'a> {
    fn new(related: &'a Related<'a>) -> Self {
        Placer {
            related,
            document: crate::uri_to_path(related.uri).map(|path| canonical_path(&path)),
            ropes: HashMap::new(),
            canonical: HashMap::new(),
        }
    }

    /// The location of `label`, for a diagnostic in file `own`. `None` when
    /// its file or that file's text is not known: no position is invented.
    fn locate(
        &mut self,
        own: oxabl_common::FileId,
        label: &FileSpan,
        rope: &Rope,
        encoding: &lsp_types::PositionEncodingKind,
    ) -> Option<Location> {
        let in_document = |range| Location {
            uri: self.related.uri.clone(),
            range,
        };
        if label.file == own {
            return Some(in_document(range(rope, label, encoding)));
        }
        let known = self.related.files.iter().find(|f| f.file == label.file)?;
        let path = known.path.as_path();
        if !self.canonical.contains_key(path) {
            self.canonical
                .insert(path.to_path_buf(), canonical_path(path));
        }
        let canonical = &self.canonical[path];
        if self.document.as_ref() == Some(canonical) {
            return Some(in_document(range(rope, label, encoding)));
        }
        let other = self
            .ropes
            .entry(path.to_path_buf())
            .or_insert_with(|| known.text.as_deref().map(Rope::from_str))
            .as_ref()?;
        Some(Location {
            uri: path_to_uri(canonical)?,
            range: range(other, label, encoding),
        })
    }
}

fn range(rope: &Rope, at: &FileSpan, encoding: &lsp_types::PositionEncodingKind) -> Range {
    Range {
        start: byte_to_position(rope, at.span.start as usize, encoding),
        end: byte_to_position(rope, at.span.end as usize, encoding),
    }
}

/// Convert a collected diagnostic set to LSP diagnostics, positioning each span
/// against `rope` in the negotiated `encoding`. Labels become related
/// information in the document at `uri` and the label files of `collected`,
/// when `uri` is given.
pub fn to_lsp_diagnostics(
    collected: &CollectedDiagnostics,
    rope: &Rope,
    encoding: &lsp_types::PositionEncodingKind,
    uri: Option<&Uri>,
) -> Vec<LspDiagnostic> {
    let related = uri.map(|uri| Related {
        uri,
        files: &collected.label_files,
    });
    let mut placer = related.as_ref().map(Placer::new);
    collected
        .all()
        .map(|c| to_lsp(&c.diagnostic, rope, encoding, placer.as_mut()))
        .collect()
}

/// Convert the answer of an include's selected root, positioning each span
/// against the include's `rope`. A root that gives no answer gives one warning
/// at the start of the file that says why. Labels become related information
/// in the document at `uri` and the files of the answer, when `uri` is given.
pub fn context_to_lsp_diagnostics(
    context: &ContextDiagnostics,
    root: &Path,
    rope: &Rope,
    encoding: &lsp_types::PositionEncodingKind,
    uri: Option<&Uri>,
) -> Vec<LspDiagnostic> {
    if let Some(reason) = &context.failure {
        return vec![LspDiagnostic {
            range: Range::default(),
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some(SOURCE.to_string()),
            message: format!(
                "the selected compilation context {} gives no diagnostics for this file: \
                 {reason}; select another context or fragment mode",
                root.display()
            ),
            ..Default::default()
        }];
    }
    let related = uri.map(|uri| Related {
        uri,
        files: &context.label_files,
    });
    let mut placer = related.as_ref().map(Placer::new);
    context
        .findings
        .iter()
        .map(|finding| to_lsp(&finding.diagnostic, rope, encoding, placer.as_mut()))
        .collect()
}

fn to_lsp(
    d: &Diagnostic,
    rope: &Rope,
    encoding: &lsp_types::PositionEncodingKind,
    placer: Option<&mut Placer<'_>>,
) -> LspDiagnostic {
    let related_information = placer.and_then(|placer| {
        let related: Vec<_> = d
            .labels
            .iter()
            .filter_map(|label| {
                Some(DiagnosticRelatedInformation {
                    location: placer.locate(d.span.file, &label.span, rope, encoding)?,
                    message: label.message.clone(),
                })
            })
            .collect();
        (!related.is_empty()).then_some(related)
    });
    LspDiagnostic {
        range: range(rope, &d.span, encoding),
        severity: Some(map_severity(d.severity)),
        code: Some(NumberOrString::String(d.code.0.to_string())),
        source: Some(SOURCE.to_string()),
        message: d.message.clone(),
        related_information,
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
        CollectedDiagnostics::new(items)
    }

    #[test]
    fn maps_range_under_both_encodings_on_multibyte_line() {
        // Line 1 has a 4-byte / 2-cu emoji before the target token 'y'.
        let rope = Rope::from_str("line one\nx😀y = 1.\n");
        // Byte offset of 'y': line 2 starts at byte 9; "x😀" is 1+4=5 bytes → y at 14.
        let y = 9 + 1 + 4;
        let d = set(vec![diag("PARSE001", Severity::Error, y, y + 1)]);

        let utf8 = to_lsp_diagnostics(&d, &rope, &PositionEncodingKind::UTF8, None);
        assert_eq!(
            utf8[0].range.start,
            Position {
                line: 1,
                character: 5
            } // byte column
        );

        let utf16 = to_lsp_diagnostics(&d, &rope, &PositionEncodingKind::UTF16, None);
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
            let mapped = to_lsp_diagnostics(&d, &rope, &PositionEncodingKind::UTF8, None);
            assert_eq!(mapped[0].severity, Some(expected));
            assert_eq!(
                mapped[0].code,
                Some(NumberOrString::String("LINT0002".to_string()))
            );
            assert_eq!(mapped[0].source.as_deref(), Some("oxabl"));
        }
    }
}
