//! Reading entries: every byte is checked before a set is installed.

use std::collections::BTreeSet;
use std::fs::{self, OpenOptions};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;
use std::thread;
use std::time::SystemTime;

use super::hashing::{CopyError, copy_and_hash};
use super::layout::{FILES, LAST_USED, Layout, MANIFEST};
use super::manifest::Manifest;
use super::place::open_no_follow;
use crate::cache::seam::{ArtifactPath, FetchOutcome, FetchRequest, MissReason, Origin};
use crate::worker::random;

/// The most entries read at once.
const READERS: usize = 8;

/// Why an entry could not be installed, and whether the entry is at fault.
enum Failure {
    /// The entry is bad: set it aside.
    Entry,
    /// The stage could not be written: the entry may be fine.
    Stage,
}

/// Looks up every request, on up to [`READERS`] threads, and reports each
/// outcome on the calling thread as it completes.
pub fn fetch(
    layout: &Layout,
    requests: Vec<FetchRequest>,
    stage: &Path,
    on_outcome: &mut dyn FnMut(FetchOutcome),
) {
    let next = AtomicUsize::new(0);
    let (tx, rx) = mpsc::channel();
    thread::scope(|scope| {
        for _ in 0..READERS.min(requests.len()) {
            let (tx, next, requests) = (tx.clone(), &next, &requests);
            scope.spawn(move || {
                loop {
                    let at = next.fetch_add(1, Ordering::SeqCst);
                    let Some(request) = requests.get(at) else {
                        break;
                    };
                    if tx.send(read(layout, request, stage)).is_err() {
                        break;
                    }
                }
            });
        }
        drop(tx);
        for outcome in rx {
            on_outcome(outcome);
        }
    });
}

fn read(layout: &Layout, request: &FetchRequest, stage: &Path) -> FetchOutcome {
    let key = request.key;
    let miss = |reason| FetchOutcome::Miss { key, reason };
    let entry = layout.entry(&key);
    let text = match fs::read_to_string(entry.join(MANIFEST)) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return miss(MissReason::NotFound);
        }
        Err(_) => {
            quarantine(layout, &entry);
            return miss(MissReason::Corrupt);
        }
    };
    let Some(manifest) = Manifest::parse(&text).filter(|manifest| manifest.key == key) else {
        quarantine(layout, &entry);
        return miss(MissReason::Corrupt);
    };
    let held: BTreeSet<&ArtifactPath> = manifest.files.iter().map(|file| &file.path).collect();
    let wanted: BTreeSet<&ArtifactPath> = request.expect.iter().collect();
    if held != wanted || held.len() != manifest.files.len() {
        return miss(MissReason::PathMismatch);
    }
    if manifest.files.iter().any(|file| !file.path.is_valid()) {
        quarantine(layout, &entry);
        return miss(MissReason::Corrupt);
    }
    let mut installed: Vec<PathBuf> = Vec::new();
    for file in &manifest.files {
        let target = stage.join(&file.path.0);
        match install(
            &entry.join(FILES).join(&file.path.0),
            &target,
            file.size,
            file.blake3,
        ) {
            Ok(()) => installed.push(target),
            Err(failure) => {
                for path in &installed {
                    let _ = fs::remove_file(path);
                }
                return match failure {
                    Failure::Entry => {
                        quarantine(layout, &entry);
                        miss(MissReason::Corrupt)
                    }
                    Failure::Stage => miss(MissReason::Unreadable),
                };
            }
        }
    }
    touch(&entry);
    FetchOutcome::Installed {
        key,
        files: manifest.files.into_iter().map(|file| file.path).collect(),
        origin: Origin::Local {
            recorded: manifest.remote,
        },
    }
}

/// Copies one file of an entry to the stage while hashing it. A source that is
/// not a regular file of the recorded size and hash makes the entry bad.
fn install(source: &Path, target: &Path, size: u64, blake3: [u8; 32]) -> Result<(), Failure> {
    let mut from = open_no_follow(source).map_err(|_| Failure::Entry)?;
    if !from.metadata().map_err(|_| Failure::Entry)?.is_file() {
        return Err(Failure::Entry);
    }
    if let Some(parent) = target.parent() {
        fs::create_dir_all(parent).map_err(|_| Failure::Stage)?;
    }
    let mut to = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(target)
        .map_err(|_| Failure::Stage)?;
    let result = copy_and_hash(&mut from, Some(&mut to));
    drop(to);
    match result {
        Ok(digest) if digest.size == size && digest.blake3 == blake3 => Ok(()),
        Ok(_) => {
            let _ = fs::remove_file(target);
            Err(Failure::Entry)
        }
        Err(error) => {
            let _ = fs::remove_file(target);
            Err(match error {
                CopyError::Read => Failure::Entry,
                CopyError::Write => Failure::Stage,
            })
        }
    }
}

/// Moves a bad entry out of `entries/`, for the next eviction to delete.
fn quarantine(layout: &Layout, entry: &Path) {
    let name = entry
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_default();
    let target = layout
        .quarantine()
        .join(format!("{name}-{}", random::token()));
    let _ = fs::create_dir_all(layout.quarantine());
    let _ = fs::rename(entry, target);
}

/// Sets the mtime of `last-used` to now, best effort.
fn touch(entry: &Path) {
    if let Ok(file) = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .open(entry.join(LAST_USED))
    {
        let _ = file.set_modified(SystemTime::now());
    }
}
