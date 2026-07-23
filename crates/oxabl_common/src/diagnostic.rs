use crate::FileSpan;

/// Severity level for a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// Machine-readable diagnostic code (e.g. `"ABL0001"`, `"PARSE001"`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticCode(pub &'static str);

/// A secondary annotation pointing to a related source location.
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
}
