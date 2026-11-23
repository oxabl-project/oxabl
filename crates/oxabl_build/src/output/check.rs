//! Whether the outputs of a result are still what oxabl installed.

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;

use super::promote::describe;
use crate::refresh::RACY_NS;
use crate::store::OutputRow;

/// The state of one recorded output.
#[derive(Debug, PartialEq, Eq)]
pub enum OutputState {
    Current,
    /// The stat differs, or was racy, but the bytes are the same: the new row
    /// to record.
    Restat(OutputRow),
    Missing,
    Changed,
}

/// Compares the file at the recorded path with its row. Equal size, mtime,
/// ctime and inode is current, unless the row was taken so soon after the
/// last change that a write in the same clock tick leaves the stat equal.
/// Any other stat is hashed, and equal bytes are current.
pub fn check(root: &Path, row: &OutputRow) -> OutputState {
    let path = root.join(&row.path);
    let Ok(meta) = fs::symlink_metadata(&path) else {
        return OutputState::Missing;
    };
    if !meta.is_file() {
        return OutputState::Changed;
    }
    let mtime_ns = meta.mtime() * 1_000_000_000 + meta.mtime_nsec();
    let ctime_ns = meta.ctime() * 1_000_000_000 + meta.ctime_nsec();
    let racy = row.observed_ns - row.mtime_ns.max(row.ctime_ns) < RACY_NS;
    if !racy
        && meta.len() == row.size
        && mtime_ns == row.mtime_ns
        && ctime_ns == row.ctime_ns
        && meta.ino() == row.ino
    {
        return OutputState::Current;
    }
    match describe(&path, &row.path) {
        Ok(now) if now.hash == row.hash => OutputState::Restat(now),
        Ok(_) => OutputState::Changed,
        Err(_) => OutputState::Missing,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A write in the clock tick of the last hash leaves every stat field
    /// equal; only the racy rule hashes the file again.
    #[test]
    fn a_racy_row_is_hashed_again_when_the_stat_is_equal() {
        let root = tempfile::TempDir::new().unwrap();
        fs::write(root.path().join("x.r"), "one").unwrap();
        let mut row = describe(&root.path().join("x.r"), "x.r").unwrap();
        fs::write(root.path().join("x.r"), "two").unwrap();
        let now = describe(&root.path().join("x.r"), "x.r").unwrap();
        row.mtime_ns = now.mtime_ns;
        row.ctime_ns = now.ctime_ns;
        row.ino = now.ino;
        row.observed_ns = now.mtime_ns.max(now.ctime_ns) + RACY_NS - 1;
        assert_eq!(check(root.path(), &row), OutputState::Changed);

        row.observed_ns = now.mtime_ns.max(now.ctime_ns) + RACY_NS;
        assert_eq!(check(root.path(), &row), OutputState::Current);
    }
}
