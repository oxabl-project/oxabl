//! Whether the outputs of a result are still what oxabl installed.

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;

use super::promote::describe;
use crate::store::OutputRow;

/// The state of one recorded output.
#[derive(Debug, PartialEq, Eq)]
pub enum OutputState {
    Current,
    /// The stat differs but the bytes are the same: the new stat to record.
    Restat(OutputRow),
    Missing,
    Changed,
}

/// Compares the file at the recorded path with its row: equal size, mtime and
/// inode is current; another stat is hashed, and equal bytes are current.
pub fn check(root: &Path, row: &OutputRow) -> OutputState {
    let path = root.join(&row.path);
    let Ok(meta) = fs::symlink_metadata(&path) else {
        return OutputState::Missing;
    };
    if !meta.is_file() {
        return OutputState::Changed;
    }
    let mtime_ns = meta.mtime() * 1_000_000_000 + meta.mtime_nsec();
    if meta.len() == row.size && mtime_ns == row.mtime_ns && meta.ino() == row.ino {
        return OutputState::Current;
    }
    match describe(&path, &row.path) {
        Ok(now) if now.hash == row.hash => OutputState::Restat(now),
        Ok(_) => OutputState::Changed,
        Err(_) => OutputState::Missing,
    }
}
