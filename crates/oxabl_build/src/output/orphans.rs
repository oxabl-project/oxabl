//! Removing r-code that oxabl installed and no unit expects any more: a unit
//! whose source is gone or that is skipped now, and r-code at the place of the
//! other `[build].rcode` mode after a switch.

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};

use super::check::{OutputState, check};
use super::ignore;
use super::manifest::{Installed, Manifest};
use super::ownership::{LOCK, MARKER};
use super::promote::TEMP_INFIX;
use crate::store::OutputRow;

/// What a sweep did.
#[derive(Debug, Default)]
pub struct Swept {
    pub deleted: usize,
    /// Files that stay because their bytes are not the ones oxabl installed.
    pub kept: Vec<String>,
    /// Files that are oxabl's and that could not be deleted, with the reason.
    pub failed: Vec<String>,
}

impl Swept {
    fn delete(&mut self, root: &Path, path: &str, emptied: &mut BTreeSet<PathBuf>) -> bool {
        let file = root.join(path);
        match fs::remove_file(&file) {
            Ok(()) => {
                self.deleted += 1;
                emptied.extend(file.parent().map(Path::to_path_buf));
                true
            }
            Err(error) if error.kind() == ErrorKind::NotFound => true,
            Err(error) => {
                self.failed.push(format!("{path}: {error}"));
                false
            }
        }
    }
}

/// Whether `name` is a temporary file of an install: `<name>.r` with the
/// temporary infix and a process id.
fn is_temporary(name: &str) -> bool {
    name.split_once(TEMP_INFIX).is_some_and(|(target, pid)| {
        target.ends_with(".r") && !pid.is_empty() && pid.bytes().all(|b| b.is_ascii_digit())
    })
}

/// Deletes the temporary files that an install across file systems leaves
/// when it is stopped, in the directories of the root-relative `paths`. The
/// caller holds the workspace lock, so no install is running.
pub fn remove_temporaries<'a>(root: &Path, paths: impl IntoIterator<Item = &'a str>) -> Swept {
    let directories: BTreeSet<&str> = paths
        .into_iter()
        .map(|path| path.rsplit_once('/').map_or("", |(directory, _)| directory))
        .collect();
    let mut swept = Swept::default();
    for directory in directories {
        let Ok(entries) = fs::read_dir(root.join(directory)) else {
            continue;
        };
        for entry in entries.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if !is_temporary(&name) {
                continue;
            }
            let shown = match directory {
                "" => name,
                _ => format!("{directory}/{name}"),
            };
            match fs::remove_file(entry.path()) {
                Ok(()) => swept.deleted += 1,
                Err(error) if error.kind() == ErrorKind::NotFound => {}
                Err(error) => swept.failed.push(format!("{shown}: {error}")),
            }
        }
    }
    swept
}

/// Whether the file at `path` holds what oxabl installed: an intent, or the
/// recorded hash and size.
fn installed_by_oxabl(path: &Path, installed: &Installed) -> bool {
    let Some((hash, size)) = installed else {
        return true;
    };
    fs::metadata(path).is_ok_and(|meta| meta.len() == *size)
        && fs::read(path).is_ok_and(|bytes| blake3::hash(&bytes).as_bytes() == hash)
}

/// Deletes the r-code at each path of `manifest` that is not in `expected`,
/// the r-code paths beside the units that compile now. A path whose bytes are
/// not the ones oxabl installed stays, with a warning. Returns what happened
/// and the lines to keep: those of `expected` and the ones whose delete
/// failed, so that the next build tries again.
pub fn sweep_manifest(
    root: &Path,
    manifest: &Manifest,
    expected: &HashSet<String>,
    keep: &HashSet<PathBuf>,
) -> (Swept, BTreeMap<String, Installed>) {
    let mut swept = Swept::default();
    let mut lines = BTreeMap::new();
    let mut emptied = BTreeSet::new();
    for (path, installed) in &manifest.lines {
        if expected.contains(path) {
            lines.insert(path.clone(), *installed);
            continue;
        }
        let file = root.join(path);
        match fs::symlink_metadata(&file) {
            Err(_) => {}
            Ok(meta) if !meta.is_file() => {
                swept.kept.push(format!("{path}: it is not a regular file"))
            }
            Ok(_) if !installed_by_oxabl(&file, installed) => swept.kept.push(format!(
                "{path}: its bytes are not the ones oxabl installed"
            )),
            Ok(_) => {
                if !swept.delete(root, path, &mut emptied) {
                    lines.insert(path.clone(), *installed);
                }
            }
        }
    }
    remove_emptied(root, emptied, keep);
    (swept, lines)
}

/// Deletes the r-code file of each row of `rows`, which no unit expects now,
/// while it holds the bytes the row recorded; then the directories this
/// empties, with oxabl's marker, lock and `.gitignore` in them.
pub fn sweep_rows<'a>(
    root: &Path,
    rows: impl IntoIterator<Item = &'a OutputRow>,
    keep: &HashSet<PathBuf>,
) -> Swept {
    let mut swept = Swept::default();
    let mut emptied = BTreeSet::new();
    for row in rows {
        match check(root, row) {
            OutputState::Current | OutputState::Restat(_) => {
                swept.delete(root, &row.path, &mut emptied);
            }
            OutputState::Changed => swept.kept.push(format!(
                "{}: its bytes are not the ones oxabl installed",
                row.path
            )),
            OutputState::Missing => {}
        }
    }
    remove_emptied(root, emptied, keep);
    swept
}

/// Removes each directory of `directories`, and then its parents, while it
/// holds nothing but oxabl's own files. Stops at the root and at a directory
/// of `keep`. A directory that holds anything else stays.
fn remove_emptied(root: &Path, directories: BTreeSet<PathBuf>, keep: &HashSet<PathBuf>) {
    for mut directory in directories.into_iter().rev() {
        while directory.starts_with(root) && directory != root && !keep.contains(&directory) {
            let Ok(entries) = fs::read_dir(&directory) else {
                break;
            };
            let mut ours = Vec::new();
            let mut other = false;
            for entry in entries {
                let Ok(entry) = entry else {
                    other = true;
                    break;
                };
                let name = entry.file_name();
                if name == MARKER || name == LOCK || ignore::is_ours(&entry.path()) {
                    ours.push(entry.path());
                } else {
                    other = true;
                    break;
                }
            }
            if other || ours.iter().any(|file| fs::remove_file(file).is_err()) {
                break;
            }
            if fs::remove_dir(&directory).is_err() {
                break;
            }
            directory.pop();
        }
    }
}
