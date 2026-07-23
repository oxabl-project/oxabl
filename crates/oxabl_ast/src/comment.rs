use crate::Span;

/// The lexical shape of a captured comment.
///
/// Derived from the comment span's leading source bytes at collection time
/// (the lexer emits a single `Kind::Comment` without a line-vs-block flag).
///
/// The two `Line`-classified sources differ at the tail (see the note on
/// [`Comment`]): a `//` comment's span *includes* its trailing `\n`, while an
/// AppBuilder `&`-directive line's span *excludes* it. Both are `Line`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    /// A `//`-to-end-of-line comment, or an AppBuilder `&`-directive line
    /// (`&ANALYZE-SUSPEND`/`&ANALYZE-RESUME`) treated as to-EOL trivia.
    Line,
    /// A `/* ... */` block comment (nested-aware — one entry spans the whole
    /// outer comment regardless of nesting depth).
    Block,
}

/// A single comment captured off the parser's discard path, hung off the
/// `Program` root as advisory fidelity data.
///
/// The comment *text* is intentionally not stored: it is derived verbatim from
/// the original source by [`span`](Comment::span) at format time. This keeps
/// the type `Copy` and the side-table cheap to build.
///
/// **Span-end convention.** The bytes a comment span owns depend on the source
/// shape:
/// - a `//` line comment span *includes* the terminating `\n` (the lexer's
///   `skip_line_comment` consumes it);
/// - a `/* */` block comment span covers the full extent through `*/` and
///   excludes any following newline;
/// - an AppBuilder `&`-directive line span *excludes* the terminating `\n`
///   (the lexer stops before it).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comment {
    /// Byte range of the comment in the original source.
    pub span: Span,
    /// Whether the comment is a line or block comment.
    pub kind: CommentKind,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comment_kind_equality() {
        assert_eq!(CommentKind::Line, CommentKind::Line);
        assert_eq!(CommentKind::Block, CommentKind::Block);
        assert_ne!(CommentKind::Line, CommentKind::Block);
    }

    #[test]
    fn comment_construction_and_copy() {
        let c = Comment {
            span: Span { start: 3, end: 8 },
            kind: CommentKind::Block,
        };
        // `Comment` is `Copy`: this moves a bitwise copy, leaving `c` usable.
        let copied = c;
        assert_eq!(copied, c);
        assert_eq!(c.span, Span { start: 3, end: 8 });
        assert_eq!(c.kind, CommentKind::Block);
    }
}
