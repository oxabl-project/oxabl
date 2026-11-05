//! Writing entries: each is built in `tmp/` and renamed into place whole.

use std::fs::{self, OpenOptions};
use std::path::{Path, PathBuf};

use super::accounting;
use super::hashing::{CopyError, copy_and_hash};
use super::layout::{FILES, LAST_USED, Layout, MANIFEST};
use super::manifest::{Manifest, ManifestFile};
use super::place::{open_no_follow, place};
use crate::cache::seam::{
    ArtifactSetRef, ArtifactUploader, CompileKey, NotStoredReason, RemoteOrigin, StoreOutcome,
};
use crate::worker::random;

/// Entries built and not yet renamed are installed in batches of this size.
const BATCH: usize = 500;

/// An entry that is complete in `tmp/`.
struct Built {
    key: CompileKey,
    directory: PathBuf,
    bytes: u64,
}

/// Builds the entry of `set` in a new directory of `tmp/`, from the files at
/// their output paths. Every file must still have the size and hash that the
/// build verified.
fn build(
    layout: &Layout,
    set: &ArtifactSetRef,
    remote: Option<&RemoteOrigin>,
) -> Result<Built, NotStoredReason> {
    let directory = layout.tmp().join(format!("entry-{}", random::token()));
    let result = fill(&directory, set, remote);
    if result.is_err() {
        let _ = fs::remove_dir_all(&directory);
    }
    result.map(|bytes| Built {
        key: set.key,
        directory,
        bytes,
    })
}

fn fill(
    directory: &Path,
    set: &ArtifactSetRef,
    remote: Option<&RemoteOrigin>,
) -> Result<u64, NotStoredReason> {
    let files_dir = directory.join(FILES);
    fs::create_dir_all(&files_dir).map_err(|_| NotStoredReason::Unavailable)?;
    let mut recorded = Vec::with_capacity(set.files.len());
    for file in &set.files {
        if !file.path.is_valid() {
            return Err(NotStoredReason::Rejected);
        }
        let target = files_dir.join(&file.path.0);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent).map_err(|_| NotStoredReason::Unavailable)?;
        }
        place(&file.disk_path, &target).map_err(|_| NotStoredReason::Unreadable)?;
        let mut placed = open_no_follow(&target).map_err(|_| NotStoredReason::Unreadable)?;
        let digest = match copy_and_hash(&mut placed, None) {
            Ok(digest) => digest,
            Err(CopyError::Read | CopyError::Write) => {
                return Err(NotStoredReason::Unreadable);
            }
        };
        if digest.size != file.size || digest.blake3 != file.blake3 {
            return Err(NotStoredReason::ChangedOnDisk);
        }
        recorded.push(ManifestFile {
            size: digest.size,
            blake3: digest.blake3,
            path: file.path.clone(),
        });
    }
    let manifest = Manifest {
        key: set.key,
        remote: remote.cloned(),
        files: recorded,
    };
    fs::write(directory.join(MANIFEST), manifest.render())
        .map_err(|_| NotStoredReason::Unavailable)?;
    OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(directory.join(LAST_USED))
        .map_err(|_| NotStoredReason::Unavailable)?;
    Ok(manifest.bytes())
}

/// Renames the built entries into `entries/` after one `syncfs`. An entry that
/// another process installed first is dropped.
fn install(layout: &Layout, built: Vec<Built>) -> Vec<(CompileKey, StoreOutcome, u64)> {
    if let Ok(base) = fs::File::open(layout.base()) {
        let _ = rustix::fs::syncfs(&base);
    }
    built
        .into_iter()
        .map(|entry| {
            let target = layout.entry(&entry.key);
            let outcome = match target.parent().map(fs::create_dir_all) {
                Some(Err(_)) | None => StoreOutcome::NotStored(NotStoredReason::Unavailable),
                Some(Ok(())) => match fs::rename(&entry.directory, &target) {
                    Ok(()) => StoreOutcome::Stored,
                    Err(_) if target.join(MANIFEST).is_file() => StoreOutcome::AlreadyPresent,
                    Err(_) => StoreOutcome::NotStored(NotStoredReason::Unavailable),
                },
            };
            if outcome != StoreOutcome::Stored {
                let _ = fs::remove_dir_all(&entry.directory);
            }
            (entry.key, outcome, entry.bytes)
        })
        .collect()
}

/// Takes the sets of one build and installs them in batches.
pub struct Uploader {
    layout: Layout,
    limit: u64,
    pending: Vec<Built>,
    done: Vec<(CompileKey, StoreOutcome)>,
}

impl Uploader {
    pub fn new(layout: Layout, limit: u64) -> Uploader {
        Uploader {
            layout,
            limit,
            pending: Vec::new(),
            done: Vec::new(),
        }
    }

    fn flush(&mut self) {
        let built = std::mem::take(&mut self.pending);
        let mut added = 0;
        for (key, outcome, bytes) in install(&self.layout, built) {
            if outcome == StoreOutcome::Stored {
                added += bytes;
            }
            self.done.push((key, outcome));
        }
        if added > 0 {
            accounting::add(&self.layout, added);
        }
    }
}

impl ArtifactUploader for Uploader {
    fn push(&mut self, set: ArtifactSetRef) {
        if self.layout.entry(&set.key).join(MANIFEST).is_file() {
            self.done.push((set.key, StoreOutcome::AlreadyPresent));
            return;
        }
        match build(&self.layout, &set, None) {
            Ok(built) => self.pending.push(built),
            Err(reason) => self.done.push((set.key, StoreOutcome::NotStored(reason))),
        }
        if self.pending.len() >= BATCH {
            self.flush();
        }
    }

    fn finish(mut self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        self.flush();
        accounting::evict(&self.layout, self.limit);
        self.done
    }
}

/// Stores one set at once, with a remote origin.
pub fn store_now(layout: &Layout, set: ArtifactSetRef, remote: &RemoteOrigin, limit: u64) {
    if layout.entry(&set.key).join(MANIFEST).is_file() {
        return;
    }
    if let Ok(built) = build(layout, &set, Some(remote)) {
        let mut added = 0;
        for (_, outcome, bytes) in install(layout, vec![built]) {
            if outcome == StoreOutcome::Stored {
                added += bytes;
            }
        }
        if added > 0 && accounting::add(layout, added) > limit {
            accounting::evict(layout, limit);
        }
    }
}
