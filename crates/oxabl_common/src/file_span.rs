use oxabl_ast::Span;

use crate::FileId;

/// A source span qualified by its owning file.
///
/// Combines a [`FileId`] with a byte-offset [`Span`] so that diagnostics and
/// the semantic layer can reference locations across multiple files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileSpan {
    pub file: FileId,
    pub span: Span,
}

impl From<(FileId, Span)> for FileSpan {
    #[inline]
    fn from((file, span): (FileId, Span)) -> Self {
        FileSpan { file, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_tuple() {
        let file = FileId::new(1);
        let span = Span { start: 10, end: 20 };
        let fs: FileSpan = (file, span).into();

        assert_eq!(fs.file, file);
        assert_eq!(fs.span, span);
    }

    #[test]
    fn with_unknown_file() {
        let span = Span { start: 0, end: 5 };
        let fs = FileSpan {
            file: FileId::UNKNOWN,
            span,
        };

        assert_eq!(fs.file, FileId::UNKNOWN);
    }
}
