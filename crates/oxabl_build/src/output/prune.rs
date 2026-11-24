//! Removing outputs that no result owns.

use std::collections::HashSet;
use std::path::Path;

use super::ownership::{LOCK, MARKER};

/// Deletes every file below the output `directories` (root-relative) that is
/// not in `owned` (root-relative paths), except oxabl's own files, and the
/// directories that this leaves empty. Returns the number of files deleted.
pub fn prune(root: &Path, directories: &[&str], owned: &HashSet<String>) -> usize {
    let mut deleted = 0;
    for directory in directories {
        deleted += walk(&root.join(directory), directory, owned);
    }
    deleted
}

fn walk(path: &Path, shown: &str, owned: &HashSet<String>) -> usize {
    let Ok(entries) = std::fs::read_dir(path) else {
        return 0;
    };
    let mut deleted = 0;
    for entry in entries.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        let relative = format!("{shown}/{name}");
        let Ok(kind) = entry.file_type() else {
            continue;
        };
        if kind.is_dir() {
            deleted += walk(&entry.path(), &relative, owned);
            let _ = std::fs::remove_dir(entry.path());
        } else if !owned.contains(&relative)
            && name != MARKER
            && name != LOCK
            && std::fs::remove_file(entry.path()).is_ok()
        {
            deleted += 1;
        }
    }
    deleted
}
