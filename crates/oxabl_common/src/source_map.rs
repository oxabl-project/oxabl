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
