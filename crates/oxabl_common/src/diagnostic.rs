use std::fmt;

use crate::{FileId, FileSpan, SourceMap};

/// Severity level for a diagnostic.
///
/// Serializes (under the `serde` feature) to a lowercase string —
/// `"error"`/`"warning"`/`"info"`/`"hint"` — matching the diagnostic wire
/// shape shared across clients.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl Severity {
    /// The lowercase wire spelling — `"error"`, `"warning"`, `"info"`, `"hint"`.
    /// This is the non-serde way to get the same string the `serde` derive
    /// produces, so text rendering doesn't need `format!("{:?}").to_lowercase()`.
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
            Severity::Hint => "hint",
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Machine-readable diagnostic code (e.g. `"ABL0001"`, `"PARSE001"`).
///
/// Serializes as its inner string (newtype), so a `DiagnosticCode` becomes
/// `"PARSE001"` rather than a wrapper object.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticCode(pub &'static str);

/// A secondary annotation pointing to a related source location.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub span: FileSpan,
    pub message: String,
}

/// A structured diagnostic: typed error codes, severity, multi-span labels.
///
/// Designed to map 1:1 to LSP `Diagnostic` and Rust's `ariadne`/`miette`
/// rendering. This is the project-wide error vocabulary — all components
/// (parser, preprocessor, semantic layer, linter) emit these.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: DiagnosticCode,
    pub message: String,
    pub span: FileSpan,
    pub labels: Vec<Label>,
    pub help: Option<String>,
}

impl Diagnostic {
    /// Create an error diagnostic with no secondary labels or help.
    pub fn error(code: &'static str, message: String, span: FileSpan) -> Self {
        Diagnostic {
            severity: Severity::Error,
            code: DiagnosticCode(code),
            message,
            span,
            labels: Vec::new(),
            help: None,
        }
    }

    /// Create a warning diagnostic with no secondary labels or help.
    pub fn warning(code: &'static str, message: String, span: FileSpan) -> Self {
        Diagnostic {
            severity: Severity::Warning,
            code: DiagnosticCode(code),
            message,
            span,
            labels: Vec::new(),
            help: None,
        }
    }

    /// Create an info diagnostic with no secondary labels or help.
    pub fn info(code: &'static str, message: String, span: FileSpan) -> Self {
        Diagnostic {
            severity: Severity::Info,
            code: DiagnosticCode(code),
            message,
            span,
            labels: Vec::new(),
            help: None,
        }
    }

    /// Add a secondary label to this diagnostic.
    pub fn with_label(mut self, span: FileSpan, message: String) -> Self {
        self.labels.push(Label { span, message });
        self
    }

    /// Add a help/suggestion string to this diagnostic.
    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }
}

/// One-line form: `severity[code]: message`, e.g. `error[PARSE001]: unexpected token`.
///
/// This is position-free — a bare [`Diagnostic`] carries a byte-offset [`FileSpan`]
/// but no [`SourceMap`], so it cannot resolve line/col on its own. Use
/// [`render_diagnostics`] with a [`SourceResolver`] for the
/// `path:line:col:`-prefixed, snippet-bearing form.
impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]: {}", self.severity, self.code.0, self.message)
    }
}

/// Resolves diagnostics against a single **root** file's source: its
/// [`FileId`], display path, and [`SourceMap`].
///
/// v1 is deliberately single-file (scope-guardian review): a diagnostic whose
/// `span.file` matches [`file`](Self::file) is rendered with a `path:line:col`
/// prefix and a source snippet; a diagnostic from any other file (e.g. an
/// expanded include) is rendered without a root-relative position, matching the
/// CLI's existing include-origin behavior. A generic multi-file resolver is
/// deferred until a consumer needs it.
pub struct SourceResolver<'a> {
    file: FileId,
    path: String,
    source: &'a str,
    map: SourceMap,
}

impl<'a> SourceResolver<'a> {
    /// Build a resolver for `source`, identified by `file` and displayed as `path`.
    pub fn new(file: FileId, path: impl Into<String>, source: &'a str) -> Self {
        SourceResolver {
            file,
            path: path.into(),
            source,
            map: SourceMap::new(source),
        }
    }

    /// The root file this resolver renders positions for.
    pub fn file(&self) -> FileId {
        self.file
    }

    /// The trimmed text of a 1-indexed line, for snippet display.
    fn line_text(&self, line: usize) -> Option<&str> {
        let start = self.map.line_start(line)?;
        let end = self.map.line_start(line + 1).unwrap_or(self.source.len());
        Some(self.source[start..end].trim_end_matches(['\n', '\r']))
    }
}

/// Render diagnostics to the familiar `path:line:col: severity[code]: message`
/// form with a source snippet, using `resolver` for positions.
///
/// Root-file diagnostics get a resolved position and a snippet line; diagnostics
/// from other files render without a root-relative position (see
/// [`SourceResolver`]). Diagnostics are rendered in the given order; a `help`
/// string, when present, is appended on its own line.
pub fn render_diagnostics(diagnostics: &[Diagnostic], resolver: &SourceResolver) -> String {
    let mut out = String::new();
    for d in diagnostics {
        if d.span.file == resolver.file {
            let (line, col) = resolver.map.lookup(d.span.span.start as usize);
            out.push_str(&format!("{}:{}:{}: {}\n", resolver.path, line, col, d));
            if let Some(text) = resolver.line_text(line) {
                out.push_str(&format!("{line:>5} | {text}\n"));
            }
        } else {
            out.push_str(&format!("{}: {} (in included file)\n", resolver.path, d));
        }
        if let Some(help) = &d.help {
            out.push_str(&format!("  help: {help}\n"));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use oxabl_ast::Span;

    use super::*;
    use crate::FileId;

    #[test]
    fn error_constructor() {
        let span = FileSpan {
            file: FileId::new(1),
            span: Span { start: 0, end: 10 },
        };
        let diag = Diagnostic::error("PARSE001", "unexpected token".to_string(), span);

        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.code.0, "PARSE001");
        assert_eq!(diag.message, "unexpected token");
        assert!(diag.labels.is_empty());
        assert!(diag.help.is_none());
    }

    #[test]
    fn warning_with_label_and_help() {
        let primary = FileSpan {
            file: FileId::new(1),
            span: Span { start: 0, end: 10 },
        };
        let secondary = FileSpan {
            file: FileId::new(1),
            span: Span { start: 20, end: 30 },
        };
        let diag = Diagnostic::warning("ABL0005", "unused variable".to_string(), primary)
            .with_label(secondary, "defined here".to_string())
            .with_help("remove or prefix with _".to_string());

        assert_eq!(diag.severity, Severity::Warning);
        assert_eq!(diag.labels.len(), 1);
        assert_eq!(diag.labels[0].message, "defined here");
        assert_eq!(diag.help.as_deref(), Some("remove or prefix with _"));
    }

    #[test]
    fn diagnostic_display_is_one_line_form() {
        let span = FileSpan {
            file: FileId::new(1),
            span: Span { start: 0, end: 4 },
        };
        let diag = Diagnostic::error("PARSE001", "unexpected token".to_string(), span);
        assert_eq!(format!("{diag}"), "error[PARSE001]: unexpected token");
    }

    #[test]
    fn render_root_diagnostic_has_position_and_snippet() {
        let source = "MESSAGE \"a\".\nDEFINE VARIABLE .";
        let root = FileId::new(1);
        // Error on line 2 (the empty variable name after the second period-less define).
        let span = FileSpan {
            file: root,
            span: Span { start: 29, end: 30 },
        };
        let diag = Diagnostic::error("PARSE001", "Expected variable name".to_string(), span);
        let resolver = SourceResolver::new(root, "foo.p", source);
        let out = render_diagnostics(std::slice::from_ref(&diag), &resolver);

        assert!(
            out.starts_with("foo.p:2:"),
            "expected root-relative position, got: {out}"
        );
        assert!(out.contains("error[PARSE001]: Expected variable name"));
        // Snippet shows the offending source line.
        assert!(
            out.contains("DEFINE VARIABLE ."),
            "expected snippet, got: {out}"
        );
    }

    #[test]
    fn render_include_origin_diagnostic_has_no_root_position() {
        let source = "MESSAGE \"a\".";
        let root = FileId::new(1);
        let other = FileId::new(2);
        let span = FileSpan {
            file: other,
            span: Span { start: 3, end: 4 },
        };
        let diag = Diagnostic::error("PARSE001", "bad token".to_string(), span);
        let resolver = SourceResolver::new(root, "foo.p", source);
        let out = render_diagnostics(std::slice::from_ref(&diag), &resolver);

        assert!(out.contains("(in included file)"), "got: {out}");
        assert!(
            !out.contains("foo.p:1:"),
            "should not have root line/col: {out}"
        );
    }

    #[test]
    fn render_multiple_diagnostics_in_order() {
        let source = "AAA\nBBB\nCCC";
        let root = FileId::new(1);
        let mk = |start: u32, msg: &str| {
            Diagnostic::error(
                "X",
                msg.to_string(),
                FileSpan {
                    file: root,
                    span: Span {
                        start,
                        end: start + 1,
                    },
                },
            )
        };
        let diags = [mk(0, "first"), mk(8, "second")];
        let resolver = SourceResolver::new(root, "f.p", source);
        let out = render_diagnostics(&diags, &resolver);
        let first = out.find("first").unwrap();
        let second = out.find("second").unwrap();
        assert!(first < second, "diagnostics should render in order: {out}");
    }

    #[cfg(feature = "serde")]
    #[test]
    fn diagnostic_serializes_with_lowercase_severity() {
        let span = FileSpan {
            file: FileId::new(1),
            span: Span { start: 0, end: 10 },
        };
        let diag = Diagnostic::warning("PREPROC007", "unresolvable include".to_string(), span);
        let v = serde_json::to_value(&diag).unwrap();

        assert_eq!(v["code"], "PREPROC007");
        assert_eq!(v["message"], "unresolvable include");
        // Severity serializes to the lowercase form the CLI previously produced
        // via `format!("{:?}", severity).to_lowercase()`.
        assert_eq!(v["severity"], "warning");
        // The span carries file + byte offsets (not pre-resolved line/col).
        assert_eq!(v["span"]["file"], 1);
        assert_eq!(v["span"]["span"]["start"], 0);
        assert_eq!(v["span"]["span"]["end"], 10);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn severity_variants_serialize_lowercase() {
        for (sev, want) in [
            (Severity::Error, "error"),
            (Severity::Warning, "warning"),
            (Severity::Info, "info"),
            (Severity::Hint, "hint"),
        ] {
            assert_eq!(serde_json::to_value(sev).unwrap(), want);
        }
    }

    #[cfg(feature = "serde")]
    #[test]
    fn file_span_round_trips_through_value() {
        let fs = FileSpan {
            file: FileId::new(3),
            span: Span { start: 4, end: 9 },
        };
        let v = serde_json::to_value(fs).unwrap();
        assert_eq!(v["file"], 3);
        assert_eq!(v["span"]["start"], 4);
        assert_eq!(v["span"]["end"], 9);
    }
}
