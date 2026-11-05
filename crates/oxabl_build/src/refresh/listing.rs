use std::os::unix::ffi::OsStrExt;
use std::path::Path;

use rustc_hash::FxHashMap;

use super::parallel;
use super::probe::{Probe, Stat, is_racy, probe};
use crate::store::ListingRow;

/// The digest of a directory that does not exist.
fn absent_digest() -> [u8; 32] {
    *blake3::hash(b"\0absent directory").as_bytes()
}

/// The BLAKE3 hash of the sorted entry names of `path`, each followed by a NUL
/// byte; the digest of a missing directory when it cannot be listed.
fn digest_directory(path: &Path) -> [u8; 32] {
    let Ok(entries) = std::fs::read_dir(path) else {
        return absent_digest();
    };
    let mut names: Vec<Vec<u8>> = entries
        .filter_map(Result::ok)
        .map(|entry| entry.file_name().as_bytes().to_vec())
        .collect();
    names.sort_unstable();
    let mut hasher = blake3::Hasher::new();
    for name in names {
        hasher.update(&name);
        hasher.update(&[0]);
    }
    *hasher.finalize().as_bytes()
}

/// The current row of the listing of `path`, and whether its entries differ
/// from the stored row `old`. The stored digest stands when the directory's
/// stat is equal and was not racy.
fn current(
    path: &Path,
    old: Option<&ListingRow>,
    observed_ns: i64,
    generation: u64,
) -> (ListingRow, bool) {
    let (stat, present) = match probe(path) {
        Probe::Present { stat, .. } => (stat, true),
        Probe::Absent => (
            Stat {
                size: 0,
                mtime_ns: 0,
                ctime_ns: 0,
                ino: 0,
                dev: 0,
            },
            false,
        ),
    };
    let holds = old.is_some_and(|row| {
        present
            && row.mtime_ns == stat.mtime_ns
            && row.ctime_ns == stat.ctime_ns
            && row.ino == stat.ino
            && !is_racy(row.observed_ns, row.mtime_ns)
    });
    if let (true, Some(row)) = (holds, old) {
        return (row.clone(), false);
    }
    let digest = if present {
        digest_directory(path)
    } else {
        absent_digest()
    };
    let changed = old.is_none_or(|row| row.digest != digest);
    let changed_gen = match old {
        Some(row) if !changed => row.changed_gen,
        _ => generation,
    };
    (
        ListingRow {
            mtime_ns: stat.mtime_ns,
            ctime_ns: stat.ctime_ns,
            ino: stat.ino,
            observed_ns,
            digest,
            changed_gen,
        },
        changed,
    )
}

/// The listings of one refresh.
pub struct Listings {
    pub rows: FxHashMap<u32, ListingRow>,
    stored: FxHashMap<u32, ListingRow>,
    /// The listings whose entries changed since the store saw them.
    pub changed: Vec<u32>,
    observed_ns: i64,
    generation: u64,
}

impl Listings {
    /// Check each stored listing against its directory.
    pub fn check(
        stored: Vec<(u32, ListingRow)>,
        path_of: impl Fn(u32) -> std::path::PathBuf + Sync,
        observed_ns: i64,
        generation: u64,
    ) -> Self {
        let results = parallel::map(&stored, |(id, old)| {
            current(&path_of(*id), Some(old), observed_ns, generation)
        });
        let mut rows = FxHashMap::default();
        let mut changed = Vec::new();
        for ((id, _), (row, differs)) in stored.iter().zip(results) {
            if differs {
                changed.push(*id);
            }
            rows.insert(*id, row);
        }
        Listings {
            rows,
            stored: stored.into_iter().collect(),
            changed,
            observed_ns,
            generation,
        }
    }

    /// Start tracking a directory an analysis listed that the store did not.
    pub fn discover(&mut self, id: u32, path: &Path) {
        if !self.rows.contains_key(&id) {
            let (row, _) = current(path, None, self.observed_ns, self.generation);
            self.rows.insert(id, row);
        }
    }

    /// The rows to write for the listings in `keep`, and the ids to delete.
    pub fn rows_for(
        &self,
        keep: &rustc_hash::FxHashSet<u32>,
    ) -> (Vec<(u32, ListingRow)>, Vec<u32>) {
        let mut upsert = Vec::new();
        for (id, row) in &self.rows {
            if !keep.contains(id) {
                continue;
            }
            if self.stored.get(id) != Some(row) {
                upsert.push((*id, row.clone()));
            }
        }
        let deleted = self
            .stored
            .keys()
            .filter(|id| !keep.contains(id))
            .copied()
            .collect();
        (upsert, deleted)
    }
}
