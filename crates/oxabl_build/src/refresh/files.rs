use std::path::Path;

use rustc_hash::{FxHashMap, FxHashSet};

use super::parallel;
use super::probe::{Probe, Stat, hash_file, hash_holds, is_racy, probe};
use super::snapshot::{Known, SnapshotFs};
use crate::store::FileRow;

/// What a tracked path is now.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Current {
    Absent,
    Present { stat: Stat, hash: [u8; 32] },
}

/// One tracked path: its state now, and the row the store held for it.
pub struct Tracked {
    pub current: Current,
    pub old: Option<FileRow>,
}

/// A path to probe: its id, its host path and, for a unit the walk already
/// stat'ed, the answer.
pub struct Job<'a> {
    pub id: u32,
    pub path: &'a Path,
    pub stat: Option<Probe>,
}

/// The tracked files of one refresh.
pub struct Files {
    map: FxHashMap<u32, Tracked>,
    /// The paths whose content or presence differs from the store.
    pub changed: FxHashSet<u32>,
    observed_ns: i64,
    generation: u64,
}

fn current_of(path: &Path, probed: Probe, old: Option<&FileRow>) -> Current {
    match probed {
        Probe::Absent => Current::Absent,
        Probe::Present { stat, is_dir } => {
            let hash = match old {
                Some(row) if hash_holds(row, &stat) => row.hash.unwrap_or_default(),
                _ => hash_file(path, is_dir),
            };
            Current::Present { stat, hash }
        }
    }
}

fn differs(old: Option<&FileRow>, current: &Current) -> bool {
    match (old, current) {
        (None, _) => true,
        (Some(row), Current::Absent) => row.present,
        (Some(row), Current::Present { hash, .. }) => {
            !row.present || row.hash.as_ref() != Some(hash)
        }
    }
}

impl Files {
    pub fn new(observed_ns: i64, generation: u64) -> Self {
        Files {
            map: FxHashMap::default(),
            changed: FxHashSet::default(),
            observed_ns,
            generation,
        }
    }

    /// Probe every job on all cores, reuse the stored hash of a file whose
    /// stat did not change, and teach `snapshot` what each present file holds.
    pub fn probe_all(
        &mut self,
        jobs: Vec<Job<'_>>,
        old: &mut FxHashMap<u32, FileRow>,
        snapshot: &SnapshotFs,
    ) {
        let probed = parallel::map(&jobs, |job| {
            let stat = job.stat.unwrap_or_else(|| probe(job.path));
            current_of(job.path, stat, old.get(&job.id))
        });
        for (job, current) in jobs.iter().zip(probed) {
            self.insert(job.id, job.path, current, old.remove(&job.id), snapshot);
        }
    }

    fn insert(
        &mut self,
        id: u32,
        path: &Path,
        current: Current,
        old: Option<FileRow>,
        snapshot: &SnapshotFs,
    ) {
        if let Current::Present { hash, .. } = current {
            snapshot.learn(path.to_path_buf(), Known { hash });
        }
        if differs(old.as_ref(), &current) {
            self.changed.insert(id);
        }
        self.map.insert(id, Tracked { current, old });
    }

    pub fn get(&self, id: u32) -> Option<&Tracked> {
        self.map.get(&id)
    }

    pub fn contains(&self, id: u32) -> bool {
        self.map.contains_key(&id)
    }

    pub fn ids(&self) -> impl Iterator<Item = u32> + '_ {
        self.map.keys().copied()
    }

    /// Start tracking a path an analysis observed that the store did not
    /// track. It is new to the store, not changed.
    pub fn discover(&mut self, id: u32, path: &Path, snapshot: &SnapshotFs) {
        let current = match probe(path) {
            Probe::Absent => Current::Absent,
            Probe::Present { stat, is_dir } => {
                let hash = snapshot
                    .known(path)
                    .map_or_else(|| hash_file(path, is_dir), |k| k.hash);
                Current::Present { stat, hash }
            }
        };
        if let Current::Present { hash, .. } = current {
            snapshot.learn(path.to_path_buf(), Known { hash });
        }
        self.map.insert(id, Tracked { current, old: None });
    }

    /// Stat and hash a path again after it changed during the refresh.
    pub fn reprobe(&mut self, id: u32, path: &Path, snapshot: &SnapshotFs) {
        let current = match probe(path) {
            Probe::Absent => Current::Absent,
            Probe::Present { stat, is_dir } => Current::Present {
                stat,
                hash: hash_file(path, is_dir),
            },
        };
        if let Current::Present { hash, .. } = current {
            snapshot.learn(path.to_path_buf(), Known { hash });
        }
        if let Some(tracked) = self.map.get_mut(&id) {
            if differs(tracked.old.as_ref(), &current) {
                self.changed.insert(id);
            }
            tracked.current = current;
        }
    }

    /// The row to store for `id` with `bits` of `tracked_by`, and whether it
    /// differs from the stored one.
    pub fn row(&self, id: u32, bits: u8) -> Option<(FileRow, bool)> {
        let tracked = self.map.get(&id)?;
        let changed = self.changed.contains(&id);
        let changed_gen = match (&tracked.old, changed) {
            (Some(old), false) => old.changed_gen,
            _ => self.generation,
        };
        let (present, stat, hash) = match tracked.current {
            Current::Absent => (false, None, None),
            Current::Present { stat, hash } => (true, Some(stat), Some(hash)),
        };
        let stat = stat.unwrap_or(Stat {
            size: 0,
            mtime_ns: 0,
            ctime_ns: 0,
            ino: 0,
            dev: 0,
        });
        let row = FileRow {
            present,
            size: stat.size,
            mtime_ns: stat.mtime_ns,
            ctime_ns: stat.ctime_ns,
            ino: stat.ino,
            dev: stat.dev,
            hash,
            observed_ns: self.observed_ns,
            changed_gen,
            tracked_by: bits,
        };
        let dirty = match &tracked.old {
            None => true,
            Some(old) => {
                changed
                    || old.tracked_by != bits
                    || old.present != row.present
                    || (present && Stat::of_row(old) != stat)
                    || (is_racy(old.observed_ns, old.mtime_ns)
                        && !is_racy(row.observed_ns, row.mtime_ns))
            }
        };
        Some((row, dirty))
    }
}
