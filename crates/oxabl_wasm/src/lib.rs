//! Browser bindings for Oxabl's shared public pipelines.
//!
//! This crate deliberately contains no ABL behavior. It translates the
//! diagnostics and formatter result returned by the `oxabl` umbrella crate
//! into a small JSON wire shape suitable for a browser. The CLI, LSP, VS Code
//! extension, and browser therefore share the same lexer, parser, semantic
//! analysis, lint rules, formatter, and safe default style.

use oxabl::analyze::{CollectedDiagnostic, DiagnosticSource};
use oxabl::common::SourceMap;
use oxabl::style::StyleGuide;
use oxabl::workspace::InMemoryFileSystem;
use oxabl::{AnalyzeOptions, analyze_with_fs};
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
struct AnalyzeResponse {
    diagnostics: Vec<WireDiagnostic>,
}

#[derive(Serialize)]
struct WireDiagnostic {
    source: &'static str,
    severity: &'static str,
    code: &'static str,
    message: String,
    start: WirePosition,
    end: WirePosition,
    help: Option<String>,
}

#[derive(Serialize)]
struct WirePosition {
    byte: u32,
    line: usize,
    column: usize,
}

#[derive(Serialize)]
struct FormatResponse {
    source: String,
    changed: bool,
    error: Option<String>,
}

fn diagnostic_to_wire(item: CollectedDiagnostic, source_map: &SourceMap) -> WireDiagnostic {
    let diagnostic = item.diagnostic;
    let start = diagnostic.span.span.start;
    let end = diagnostic.span.span.end;
    let (start_line, start_column) = source_map.lookup(start as usize);
    let (end_line, end_column) = source_map.lookup(end as usize);

    WireDiagnostic {
        source: diagnostic_source(item.source),
        severity: diagnostic.severity.as_str(),
        code: diagnostic.code.0,
        message: diagnostic.message,
        start: WirePosition {
            byte: start,
            line: start_line,
            column: start_column,
        },
        end: WirePosition {
            byte: end,
            line: end_line,
            column: end_column,
        },
        help: diagnostic.help,
    }
}

fn diagnostic_source(source: DiagnosticSource) -> &'static str {
    source.as_str()
}

/// Analyze one in-memory ABL file through the same parse → semantic → lint
/// collector used by the CLI and LSP.
///
/// The browser MVP has no project filesystem, include path, or schema upload,
/// so preprocessing and schema-backed rules are disabled rather than emulated.
#[wasm_bindgen]
pub fn analyze_source(source: &str) -> String {
    let options = AnalyzeOptions::default();
    let fs = InMemoryFileSystem::new();
    let (_, collected) = analyze_with_fs(source, &fs, &options);
    let source_map = SourceMap::new(source);
    let diagnostics = collected
        .diagnostics
        .into_iter()
        .map(|diagnostic| diagnostic_to_wire(diagnostic, &source_map))
        .collect();

    serde_json::to_string(&AnalyzeResponse { diagnostics })
        .expect("the browser diagnostic wire shape is always serializable")
}

/// Format one ABL file through Oxabl's shared layout-only formatter using the
/// same safe default style as the LSP when no `oxabl.toml` is present.
#[wasm_bindgen]
pub fn format_source(source: &str) -> String {
    let result = match oxabl::format_source(source, &StyleGuide::default_base()) {
        Ok(formatted) => FormatResponse {
            changed: formatted != source,
            source: formatted,
            error: None,
        },
        Err(error) => FormatResponse {
            source: source.to_string(),
            changed: false,
            error: Some(error.to_string()),
        },
    };

    serde_json::to_string(&result).expect("the browser format wire shape is always serializable")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analysis_uses_the_shared_lint_pipeline() {
        let response: serde_json::Value = serde_json::from_str(&analyze_source(
            "DEFINE VARIABLE unused AS INTEGER NO-UNDO.",
        ))
        .unwrap();
        let diagnostics = response["diagnostics"].as_array().unwrap();

        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic["code"] == "LINT0002"
                && diagnostic["source"] == "lint"
                && diagnostic["start"]["line"] == 1
        }));
    }

    #[test]
    fn formatting_uses_the_safe_shared_default() {
        let source = "IF TRUE THEN\nMESSAGE \"hello\".";
        let response: serde_json::Value = serde_json::from_str(&format_source(source)).unwrap();

        assert_eq!(response["error"], serde_json::Value::Null);
        assert_eq!(response["changed"], true);
        assert_eq!(response["source"], "IF TRUE THEN\n    MESSAGE \"hello\".\n");
    }

    #[test]
    fn formatting_bail_keeps_the_original_source() {
        let source = "IF THEN.";
        let response: serde_json::Value = serde_json::from_str(&format_source(source)).unwrap();

        assert_eq!(response["source"], source);
        assert_eq!(response["changed"], false);
        assert!(response["error"].is_string());
    }
}
