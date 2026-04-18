//! Virtual spans: byte offsets into post-preprocessor expanded source.
//!
//! The semantic layer operates on post-preprocessor source, so every span it
//! produces is a *virtual* offset, not a real source coordinate. A virtual
//! span is translated to a [`FileSpan`] exactly once — at diagnostic emission
//! time — via `PreprocessedFile::resolve`. Modelling that as a distinct
//! newtype prevents accidental mixing at API boundaries.

/// A span of byte offsets into preprocessor-expanded source.
///
/// Virtual spans are the semantic layer's span currency. They are not
/// directly comparable to [`FileSpan`](crate::FileSpan) — the preprocessor
/// must resolve them first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtualSpan {
    pub start: u32,
    pub end: u32,
}

impl VirtualSpan {
    /// Construct a virtual span from raw byte offsets.
    #[inline]
    pub const fn new(start: u32, end: u32) -> Self {
        VirtualSpan { start, end }
    }

    /// Length in bytes.
    #[inline]
    pub const fn len(&self) -> u32 {
        self.end - self.start
    }

    /// Whether the span is empty (zero-length).
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_constructs_span() {
        let s = VirtualSpan::new(3, 7);
        assert_eq!(s.start, 3);
        assert_eq!(s.end, 7);
        assert_eq!(s.len(), 4);
        assert!(!s.is_empty());
    }

    #[test]
    fn empty_span() {
        let s = VirtualSpan::new(5, 5);
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
    }
}
