//! Blank-line detection over raw source bytes.
//!
//! The future `oxabl_formatter` normalizes blank runs by observing where they
//! occurred in the original source. Blank lines are never tokenized — they are
//! inferred from the gap between two node spans. This module provides the pure
//! counting primitive that operationalizes that inference.

/// Count the number of fully-blank lines in the source gap between two byte
/// offsets.
///
/// The gap is the half-open range `source[start..end]`, expected to be the
/// span between two adjacent nodes (e.g. the end of one statement's span and
/// the start of the next). A "blank line" is a line in that gap containing no
/// content — the count is the number of `\n` in the gap minus one, clamped at
/// zero. Whitespace-only lines count as blank, and `\r\n` endings are handled
/// by keying on `\n` alone.
///
/// Examples (n = newline count in the gap):
/// - no newline (`start == end`, or same-line gap) → `0`
/// - one newline (spans on consecutive lines, nothing between) → `0`
/// - two newlines (one empty line between) → `1`
/// - four newlines → `3`
///
/// Defensive: an inverted or empty range (`end <= start`) returns `0`, and
/// out-of-bounds offsets are clamped to the source length so this never panics.
pub fn blank_lines_between(source: &str, start: usize, end: usize) -> usize {
    let len = source.len();
    let start = start.min(len);
    let end = end.min(len);
    if end <= start {
        return 0;
    }
    let newlines = source.as_bytes()[start..end]
        .iter()
        .filter(|&&b| b == b'\n')
        .count();
    newlines.saturating_sub(1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adjacent_spans_are_zero() {
        let src = "a b";
        assert_eq!(blank_lines_between(src, 1, 1), 0);
    }

    #[test]
    fn one_newline_no_blank_line() {
        // Two spans on consecutive lines with nothing blank between them.
        let src = "a\nb";
        assert_eq!(blank_lines_between(src, 1, 2), 0);
    }

    #[test]
    fn two_newlines_one_blank_line() {
        let src = "a\n\nb";
        assert_eq!(blank_lines_between(src, 1, 3), 1);
    }

    #[test]
    fn run_of_blank_lines() {
        // Four newlines in the gap → three blank lines.
        let src = "a\n\n\n\nb";
        assert_eq!(blank_lines_between(src, 1, 5), 3);
    }

    #[test]
    fn whitespace_only_line_counts_as_blank() {
        // The middle line contains only spaces/tabs; it still counts.
        let src = "a\n   \t\nb";
        assert_eq!(blank_lines_between(src, 1, src.len() - 1), 1);
    }

    #[test]
    fn crlf_line_endings_match_lf() {
        let src = "a\r\n\r\nb";
        // Same as the LF `a\n\nb` case: one blank line.
        assert_eq!(blank_lines_between(src, 1, src.len() - 1), 1);
    }

    #[test]
    fn inverted_range_is_zero() {
        let src = "a\n\n\nb";
        assert_eq!(blank_lines_between(src, 4, 1), 0);
        assert_eq!(blank_lines_between(src, 2, 2), 0);
    }

    #[test]
    fn out_of_bounds_offsets_are_clamped() {
        let src = "a\n\nb";
        // end past the end is clamped to len; must not panic.
        assert_eq!(blank_lines_between(src, 1, 999), 1);
        assert_eq!(blank_lines_between(src, 999, 1000), 0);
    }
}
