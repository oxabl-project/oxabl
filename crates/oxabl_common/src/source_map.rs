/// Maps byte offsets to line/column positions
pub struct SourceMap {
    /// Byte offset where each line starts
    line_starts: Vec<usize>,
}

impl SourceMap {
    /// Build the source map in a single pass over source
    pub fn new(source: &str) -> Self {
        // pre-allocate assuming 40 chars per line average
        let estimated_lines = source.len() / 40 + 1;
        let mut line_starts = Vec::with_capacity(estimated_lines);

        // Line 1 starts on byte 0, always
        line_starts.push(0);

        // find all newlines
        for (byte_offset, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push(byte_offset + 1);
            }
        }

        Self { line_starts }
    }

    /// Lookup line and column for a byte offset
    /// Returns (line, column) as 1-indexed values
    /// Inline to hopefully reduce overhead
    ///
    /// Example
    ///
    /// ```rust
    /// use oxabl_lexer::tokenize;
    /// use oxabl_common::SourceMap;
    /// let source = "def var myInt as int no-undo init 1.";
    /// let source_map = SourceMap::new(&source);
    /// let tokens = tokenize(source);
    /// let (line_number, column_number) = source_map.lookup(tokens[0].start);
    /// ```
    #[inline]
    pub fn lookup(&self, offset: usize) -> (usize, usize) {
        // Binary search to find line, partition_point returns the first index
        // where the predicate is false, the first line_start that is > offset.
        // Subtract 1 to get the line.
        let line_index = self.line_starts.partition_point(|&start| start <= offset) - 1;
        // Column is the distance from the start of the line
        let line_start = self.line_starts[line_index];
        let column_index = offset - line_start;
        // convert to 1-indexed based, human readable
        (line_index + 1, column_index + 1)
    }

    /// Return total number of lines in source
    #[inline]
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Return byte offset where a line starts.
    /// Use to extract full lines for error context.
    #[inline]
    pub fn line_start(&self, line: usize) -> Option<usize> {
        self.line_starts.get(line - 1).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_line() {
        let source = "hello world";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 1);
        assert_eq!(map.lookup(0), (1, 1)); // 'h'
        assert_eq!(map.lookup(5), (1, 6)); // ' '
        assert_eq!(map.lookup(10), (1, 11)); // 'd'
    }

    #[test]
    fn multiple_lines() {
        let source = "line one\nline two\nline three";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 3);

        // Line 1: "line one" (bytes 0-7, newline at 8)
        assert_eq!(map.lookup(0), (1, 1)); // 'l'
        assert_eq!(map.lookup(4), (1, 5)); // ' '
        assert_eq!(map.lookup(7), (1, 8)); // 'e'

        // Line 2: "line two" (bytes 9-16, newline at 17)
        assert_eq!(map.lookup(9), (2, 1)); // 'l'
        assert_eq!(map.lookup(13), (2, 5)); // ' '
        assert_eq!(map.lookup(16), (2, 8)); // 'o'

        // Line 3: "line three" (bytes 18-27)
        assert_eq!(map.lookup(18), (3, 1)); // 'l'
        assert_eq!(map.lookup(27), (3, 10)); // 'e'
    }

    #[test]
    fn empty_lines() {
        let source = "first\n\nthird";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 3);
        assert_eq!(map.lookup(0), (1, 1)); // 'f'
        assert_eq!(map.lookup(5), (1, 6)); // newline char is end of line 1
        assert_eq!(map.lookup(6), (2, 1)); // empty line 2 starts here (newline char)
        assert_eq!(map.lookup(7), (3, 1)); // 't' - start of line 3
    }

    #[test]
    fn line_start_offsets() {
        let source = "abc\ndef\nghi";
        let map = SourceMap::new(source);

        assert_eq!(map.line_start(1), Some(0)); // Line 1 starts at byte 0
        assert_eq!(map.line_start(2), Some(4)); // Line 2 starts at byte 4
        assert_eq!(map.line_start(3), Some(8)); // Line 3 starts at byte 8
        assert_eq!(map.line_start(4), None); // No line 4
    }

    #[test]
    fn trailing_newline() {
        let source = "line one\nline two\n";
        let map = SourceMap::new(source);

        // Trailing newline creates an empty line 3
        assert_eq!(map.line_count(), 3);
        assert_eq!(map.line_start(3), Some(18));
    }

    #[test]
    fn windows_style_crlf() {
        // Windows uses \r\n - we only track \n, so \r is just a character
        let source = "line one\r\nline two";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 2);
        assert_eq!(map.lookup(0), (1, 1)); // 'l'
        assert_eq!(map.lookup(8), (1, 9)); // '\r' is column 9
        assert_eq!(map.lookup(10), (2, 1)); // 'l' of line two
    }

    #[test]
    fn unicode_characters() {
        // Unicode: "héllo" has 6 bytes (é is 2 bytes in UTF-8)
        let source = "héllo\nworld";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 2);
        assert_eq!(map.lookup(0), (1, 1)); // 'h'
        // 'é' is bytes 1-2, 'l' is byte 3
        assert_eq!(map.lookup(3), (1, 4)); // first 'l'
        assert_eq!(map.lookup(6), (1, 7)); // newline
        assert_eq!(map.lookup(7), (2, 1)); // 'w'
    }

    #[test]
    fn empty_source() {
        let source = "";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 1); // Even empty source has line 1
        assert_eq!(map.line_start(1), Some(0));
    }

    #[test]
    fn abl_code_example() {
        let source = "def var myInt as int no-undo.\nassign myInt = 42.";
        let map = SourceMap::new(source);

        assert_eq!(map.line_count(), 2);

        // "def" is at start
        assert_eq!(map.lookup(0), (1, 1));

        // "myInt" starts at column 9 (after "def var ")
        assert_eq!(map.lookup(8), (1, 9));

        // Line 2: "assign" starts at byte 30
        assert_eq!(map.lookup(30), (2, 1));

        // "42" is at bytes 45-46
        assert_eq!(map.lookup(45), (2, 16));
    }

    #[test]
    fn last_byte_of_line() {
        let source = "abc\ndef";
        let map = SourceMap::new(source);

        // Newline character at byte 3 is still on line 1
        assert_eq!(map.lookup(3), (1, 4));
        // First char of line 2
        assert_eq!(map.lookup(4), (2, 1));
    }
}
