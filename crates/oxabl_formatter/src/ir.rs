//! Line-buffer IR (U3 / R5.4): the deliberately-minimal emit substrate.
//!
//! The printer emits into a [`LineBuf`] — a `Vec` of [`Line`], each carrying an
//! absolute leading-indent width (in columns) and its content with the leading
//! whitespace stripped. Flushing renders the indent per `indent_style` /
//! `indent_size` and joins with the source's dominant line ending.
//!
//! This is enough to reindent and normalize blank runs (KTD5), and its shape is
//! structurally replaceable by a v2 doc-IR — but it builds **none** of Wadler's
//! `group`/`break`/`nest` algebra. There is no width measurement here.

use oxabl_style::IndentStyle;

/// One emitted line: an absolute indent width in columns plus its content.
///
/// `content` holds the line's text with **no** leading indentation; a blank
/// line has empty `content` (and its `indent` is irrelevant — flushing never
/// emits trailing whitespace on a blank line).
///
/// `protected` marks a line the printer emitted **verbatim** because it begins
/// inside a multi-line token (a string literal or `{include}`/preprocessor
/// reference whose interior bytes are significant). Its `content` already
/// carries the original leading whitespace and `indent` is 0, so flushing
/// reproduces the source bytes exactly. The blank-normalization pass (U6) must
/// treat a protected line as never-blank so a blank physical line living inside
/// such a token is never dropped or clamped (that blank is a significant byte of
/// the token's value).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Line {
    pub indent: usize,
    pub content: String,
    pub protected: bool,
}

impl Line {
    pub(crate) fn is_blank(&self) -> bool {
        self.content.is_empty()
    }
}

/// A line-oriented output buffer, flushable to a `String`.
#[derive(Debug, Default)]
pub(crate) struct LineBuf {
    lines: Vec<Line>,
}

impl LineBuf {
    pub(crate) fn new() -> Self {
        Self { lines: Vec::new() }
    }

    /// Push a content line at an absolute indent width (columns).
    pub(crate) fn push(&mut self, indent: usize, content: String) {
        self.lines.push(Line {
            indent,
            content,
            protected: false,
        });
    }

    /// Push a **verbatim** line whose `content` already includes its original
    /// leading whitespace (indent 0). Used for physical lines that begin inside
    /// a multi-line token, which must not be reindented or normalized.
    pub(crate) fn push_protected(&mut self, content: String) {
        self.lines.push(Line {
            indent: 0,
            content,
            protected: true,
        });
    }

    /// Push a blank line.
    pub(crate) fn push_blank(&mut self) {
        self.lines.push(Line {
            indent: 0,
            content: String::new(),
            protected: false,
        });
    }

    pub(crate) fn lines(&self) -> &[Line] {
        &self.lines
    }

    /// Replace the line list (used by the blank-normalization pass, U6).
    pub(crate) fn set_lines(&mut self, lines: Vec<Line>) {
        self.lines = lines;
    }

    /// Flush to a single `String`: render each line's indent per `style`/`size`,
    /// join with `line_ending`, and terminate with exactly one `line_ending`.
    ///
    /// An empty buffer flushes to an empty string (an empty file stays empty).
    /// Blank lines emit no trailing whitespace.
    pub(crate) fn flush(&self, style: IndentStyle, size: usize, line_ending: &str) -> String {
        if self.lines.is_empty() {
            return String::new();
        }
        let mut out = String::new();
        for line in &self.lines {
            if !line.is_blank() {
                render_indent_into(&mut out, line.indent, style, size);
                out.push_str(&line.content);
            }
            out.push_str(line_ending);
        }
        out
    }
}

/// Render `cols` columns of indentation per `style` into `out`.
///
/// `Spaces` → `cols` spaces. `Tabs` → `cols / size` tabs followed by
/// `cols % size` spaces (so a non-multiple indent degrades gracefully to a
/// tab-then-spaces mix rather than losing alignment).
fn render_indent_into(out: &mut String, cols: usize, style: IndentStyle, size: usize) {
    match style {
        IndentStyle::Spaces => {
            for _ in 0..cols {
                out.push(' ');
            }
        }
        IndentStyle::Tabs => {
            let (tabs, spaces) = match cols.checked_div(size) {
                Some(tabs) => (tabs, cols % size),
                None => (0, cols),
            };
            for _ in 0..tabs {
                out.push('\t');
            }
            for _ in 0..spaces {
                out.push(' ');
            }
        }
    }
}

/// Render `cols` columns of indentation to a fresh `String`.
#[cfg(test)]
pub(crate) fn render_indent(cols: usize, style: IndentStyle, size: usize) -> String {
    let mut s = String::new();
    render_indent_into(&mut s, cols, style, size);
    s
}

/// Detect the source's dominant line ending (KTD7 / Fable finding 7).
///
/// Returns `"\r\n"` if the file has any CRLF endings and CRLF is at least as
/// common as bare LF; otherwise `"\n"`. This keeps a CRLF codebase from being
/// silently rewritten to LF (a surprise byte-level churn), while defaulting to
/// LF for LF-only or newline-free files.
pub(crate) fn dominant_line_ending(source: &str) -> &'static str {
    let crlf = source.matches("\r\n").count();
    let bytes = source.as_bytes();
    let total_lf = bytes.iter().filter(|&&b| b == b'\n').count();
    let lf_only = total_lf - crlf;
    if crlf > 0 && crlf >= lf_only {
        "\r\n"
    } else {
        "\n"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indent_rendering_spaces_and_tabs() {
        // depth 2 at indent_size 4 → 8 columns.
        assert_eq!(render_indent(8, IndentStyle::Spaces, 4), "        "); // 8 spaces
        assert_eq!(render_indent(8, IndentStyle::Tabs, 4), "\t\t"); // 2 tabs
    }

    #[test]
    fn nested_open_close_returns_to_outer_depth() {
        // Simulate descending and ascending block depths at indent_size 4.
        let size = 4;
        let depths = [0usize, 1, 2, 1, 0];
        let mut buf = LineBuf::new();
        for (i, d) in depths.iter().enumerate() {
            buf.push(d * size, format!("line{i}"));
        }
        let out = buf.flush(IndentStyle::Spaces, size, "\n");
        let widths: Vec<usize> = out
            .lines()
            .map(|l| l.len() - l.trim_start().len())
            .collect();
        assert_eq!(widths, vec![0, 4, 8, 4, 0]);
    }

    #[test]
    fn flush_no_trailing_whitespace_on_blank_lines() {
        let mut buf = LineBuf::new();
        buf.push(4, "a".into());
        buf.push_blank();
        buf.push(4, "b".into());
        let out = buf.flush(IndentStyle::Spaces, 4, "\n");
        assert_eq!(out, "    a\n\n    b\n");
        // The blank line is exactly empty — no stray spaces.
        assert_eq!(out.lines().nth(1), Some(""));
    }

    #[test]
    fn empty_buffer_flushes_empty() {
        let buf = LineBuf::new();
        assert_eq!(buf.flush(IndentStyle::Spaces, 4, "\n"), "");
    }

    #[test]
    fn detects_crlf_and_lf() {
        assert_eq!(dominant_line_ending("a\r\nb\r\n"), "\r\n");
        assert_eq!(dominant_line_ending("a\nb\n"), "\n");
        assert_eq!(dominant_line_ending("no newline"), "\n");
        // Mixed but CRLF-dominant.
        assert_eq!(dominant_line_ending("a\r\nb\r\nc\n"), "\r\n");
    }
}
