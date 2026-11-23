//! R-code beside the source: what a build deletes before it compiles, and the
//! directories it must be able to write.

use std::collections::{BTreeSet, HashSet};
use std::io;
use std::os::unix::fs::MetadataExt;
use std::path::Path;

/// The path of the r-code beside the unit `unit`: its path with the
/// extension `.r`.
pub fn rcode_path(unit: &str) -> String {
    let stem = match unit.rsplit_once('.') {
        Some((stem, extension)) if !extension.contains('/') => stem,
        _ => unit,
    };
    format!("{stem}.r")
}

/// What a build in place clears before its first compile or fetch.
pub struct Clear<'a> {
    /// The units of the workspace that do not compile.
    pub skipped: &'a [String],
    /// The r-code paths of the units that compile: never deleted for a skipped
    /// unit, also when a link or the case of a name makes them one file.
    pub compiling: &'a [String],
    /// The r-code paths of the class units that compile or fetch in this
    /// build: a user must never bind r-code that is older than its class.
    pub classes: &'a [String],
}

/// Deletes the r-code beside each skipped unit and each class unit of the
/// build. The number of files deleted, or every file that stays.
pub fn clear(root: &Path, clear: &Clear<'_>) -> Result<usize, Vec<String>> {
    let identity = |path: &str| {
        std::fs::symlink_metadata(root.join(path))
            .ok()
            .map(|meta| (meta.dev(), meta.ino()))
    };
    let kept: HashSet<(u64, u64)> = clear
        .compiling
        .iter()
        .filter_map(|path| identity(path))
        .collect();
    let skipped = clear
        .skipped
        .iter()
        .filter(|unit| !unit.starts_with(['$', '/']))
        .map(|unit| rcode_path(unit))
        .filter(|path| identity(path).is_some_and(|id| !kept.contains(&id)));
    let mut deleted = 0;
    let mut failed = Vec::new();
    let paths: BTreeSet<String> = skipped.chain(clear.classes.iter().cloned()).collect();
    for path in paths {
        match std::fs::remove_file(root.join(&path)) {
            Ok(()) => deleted += 1,
            Err(error) if error.kind() == io::ErrorKind::NotFound => {}
            Err(error) => failed.push(format!("{path}: {error}")),
        }
    }
    match failed.is_empty() {
        true => Ok(deleted),
        false => Err(failed),
    }
}

/// The directories of `paths` (root-relative r-code paths) that a build
/// cannot write, each with the reason. oxabl creates no directory beside the
/// sources.
pub fn unwritable(root: &Path, paths: &[String]) -> Vec<String> {
    let directories: BTreeSet<&str> = paths
        .iter()
        .map(|path| {
            path.rsplit_once('/')
                .map_or(".", |(directory, _)| directory)
        })
        .collect();
    directories
        .into_iter()
        .filter_map(|directory| {
            let real = root.join(directory);
            match rustix::fs::access(&real, rustix::fs::Access::WRITE_OK) {
                Ok(()) => None,
                Err(error) => Some(format!("{directory}: {}", io::Error::from(error))),
            }
        })
        .collect()
}
