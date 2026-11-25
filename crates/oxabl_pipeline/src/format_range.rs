//! Format a selection through the shared [`FormatPipeline`].

use std::ops::Range;

use oxabl_formatter::try_format_ranges;
pub use oxabl_formatter::{RangeBail, RangeEdit, RangeFailure};

use crate::FormatPipeline;

impl FormatPipeline {
    /// The edits that give the statements `ranges` touch (byte ranges of
    /// `source`, which is **raw**, unexpanded text) the layout they get in the
    /// whole file, and no other edits.
    ///
    /// An empty list means they already have it. A refusal is a
    /// [`RangeFailure`]: the file does not format, the ranges touch no statement,
    /// or a contained panic. On a refusal the source stands. See
    /// [`oxabl_formatter::format_ranges`] for which statements a range touches.
    pub fn format_ranges(
        &self,
        source: &str,
        ranges: &[Range<usize>],
    ) -> Result<Vec<RangeEdit>, RangeFailure> {
        try_format_ranges(source, self.style(), ranges)
    }
}
