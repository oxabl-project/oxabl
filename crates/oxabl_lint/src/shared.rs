//! Facts that one registry run computes once for several rules.

use std::cell::OnceCell;

use oxabl_ast::Statement;
use oxabl_handles::FileHandles;

/// What the rules of one [`Registry::run_where`](crate::Registry::run_where)
/// share. Each fact is computed on first use, so a run that enables no rule
/// that needs it pays nothing.
#[derive(Default)]
pub struct RunShared {
    handles: OnceCell<FileHandles>,
}

impl RunShared {
    /// The dynamic objects of the file, analysed once. `source` is the text
    /// `program` was parsed from.
    pub fn handles(&self, program: &[Statement], source: &str) -> &FileHandles {
        self.handles
            .get_or_init(|| oxabl_handles::analyze_file(program, source))
    }
}
