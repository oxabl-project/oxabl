//! V1: the unit and its closure still hold the bytes the key used.

use std::path::Path;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use rustc_hash::FxHashMap;

use crate::key::FileDigest;
use crate::key::digest::sha256_bytes;
use crate::refresh::{Content, Outcome};
use crate::worker::{Worker, WorkerError};

/// What was seen at a path, and when.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Seen {
    Blake3([u8; 32]),
    Sha256([u8; 32]),
    Unreadable,
}

/// Hashes files again, and shares what it saw between units: a file hashed
/// after a unit finished stands for that unit too.
pub struct Rehasher<'a> {
    root: &'a Path,
    outcome: &'a Outcome,
    /// The SHA-256 of each `$DLC/...` file as the key used it.
    dlc: &'a FxHashMap<String, [u8; 32]>,
    seen: Mutex<FxHashMap<u32, (Instant, Seen)>>,
    timeout: Duration,
}

impl<'a> Rehasher<'a> {
    pub fn new(
        root: &'a Path,
        outcome: &'a Outcome,
        dlc: &'a FxHashMap<String, [u8; 32]>,
        timeout: Duration,
    ) -> Rehasher<'a> {
        Rehasher {
            root,
            outcome,
            dlc,
            seen: Mutex::new(FxHashMap::default()),
            timeout,
        }
    }

    fn observe(&self, node: u32, worker: &mut Worker) -> Result<Seen, WorkerError> {
        let name = self.outcome.name(node);
        if name.starts_with("$DLC/") {
            return Ok(match worker.hash_file(name, self.timeout)? {
                FileDigest::File { sha256, .. } => {
                    sha256_bytes(&sha256).map_or(Seen::Unreadable, Seen::Sha256)
                }
                FileDigest::Dir | FileDigest::Absent => Seen::Unreadable,
            });
        }
        Ok(match std::fs::read(self.root.join(name)) {
            Ok(bytes) => Seen::Blake3(*blake3::hash(&bytes).as_bytes()),
            Err(_) => Seen::Unreadable,
        })
    }

    fn expected(&self, node: u32) -> Option<Seen> {
        let name = self.outcome.name(node);
        if let Some(sha) = self.dlc.get(name) {
            return Some(Seen::Sha256(*sha));
        }
        match self.outcome.content_of(node) {
            Content::Hash(hash) => Some(Seen::Blake3(hash)),
            _ => None,
        }
    }

    /// `Ok` when the unit and every file of `closure` has the hash the key
    /// used, measured after `done_at`. `Err` names the first that differs.
    /// A `$DLC` file is hashed by `worker`, the copy the compile read.
    pub fn check(
        &self,
        unit: u32,
        closure: &[u32],
        done_at: Instant,
        worker: &mut Worker,
    ) -> Result<(), String> {
        for node in std::iter::once(unit).chain(closure.iter().copied()) {
            let name = self.outcome.name(node);
            let Some(expected) = self.expected(node) else {
                return Err(name.to_string());
            };
            let reusable = self
                .seen
                .lock()
                .unwrap()
                .get(&node)
                .filter(|(at, _)| *at >= done_at)
                .map(|(_, seen)| *seen);
            let seen = match reusable {
                Some(seen) => seen,
                None => {
                    let at = Instant::now();
                    let seen = self
                        .observe(node, worker)
                        .map_err(|error| format!("{name}: {error}"))?;
                    self.seen.lock().unwrap().insert(node, (at, seen));
                    seen
                }
            };
            if seen != expected {
                return Err(name.to_string());
            }
        }
        Ok(())
    }
}
