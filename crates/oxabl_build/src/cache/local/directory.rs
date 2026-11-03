//! Creating the cache directory and checking that only this user can write it.

use std::fs::{self, DirBuilder};
use std::os::unix::fs::{DirBuilderExt, MetadataExt};
use std::path::Path;

use super::layout::Layout;

/// Creates the directories of the cache with mode `0700` and checks the
/// cache directory: another user's directory, or one the group or others can
/// write, is unusable.
pub fn prepare(directory: &Path, layout: &Layout) -> Result<(), String> {
    let mut builder = DirBuilder::new();
    builder.recursive(true).mode(0o700);
    builder
        .create(directory)
        .map_err(|e| format!("cannot create {}: {e}", directory.display()))?;
    let meta =
        fs::metadata(directory).map_err(|e| format!("cannot read {}: {e}", directory.display()))?;
    if !meta.is_dir() {
        return Err(format!("{} is not a directory", directory.display()));
    }
    if meta.uid() != rustix::process::getuid().as_raw() {
        return Err(format!("{} belongs to another user", directory.display()));
    }
    if meta.mode() & 0o022 != 0 {
        return Err(format!(
            "{} can be written by its group or by others",
            directory.display()
        ));
    }
    for sub in [
        layout.base().to_path_buf(),
        layout.tmp(),
        layout.quarantine(),
        layout.entries(),
    ] {
        builder
            .create(&sub)
            .map_err(|e| format!("cannot create {}: {e}", sub.display()))?;
        let meta = fs::metadata(&sub).map_err(|e| format!("cannot read {}: {e}", sub.display()))?;
        if meta.uid() != rustix::process::getuid().as_raw() || meta.mode() & 0o022 != 0 {
            return Err(format!("{} is not private to this user", sub.display()));
        }
    }
    Ok(())
}
