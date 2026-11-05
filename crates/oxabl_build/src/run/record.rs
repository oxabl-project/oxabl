//! Writing results to the store in batches.

use std::path::Path;
use std::time::{Duration, Instant};

use crate::error::BuildError;
use crate::store::{OutputRow, ResultBatch, ResultRow, Store};

/// A batch is written when it holds this many results.
const BATCH_UNITS: usize = 500;
/// A batch is written when its first result is this old.
const BATCH_AGE: Duration = Duration::from_secs(2);

/// The store of a build, written one batch at a time.
pub struct Recorder<'a> {
    store: Store,
    root: &'a Path,
    batch: ResultBatch,
    since: Option<Instant>,
}

fn failure(error: impl std::fmt::Display) -> BuildError {
    BuildError::Infrastructure(error.to_string())
}

impl<'a> Recorder<'a> {
    pub fn new(store: Store, root: &'a Path) -> Recorder<'a> {
        Recorder {
            store,
            root,
            batch: ResultBatch::default(),
            since: None,
        }
    }

    /// The batch being filled, for what is not a result.
    pub fn batch(&mut self) -> &mut ResultBatch {
        self.since.get_or_insert_with(Instant::now);
        &mut self.batch
    }

    pub fn push(&mut self, unit: u32, result: ResultRow, outputs: Vec<OutputRow>) {
        self.batch().upserts.push((unit, result, outputs));
    }

    /// Writes the batch when it is full or old.
    pub fn flush_due(&mut self) -> Result<(), BuildError> {
        let due = self.batch.upserts.len() >= BATCH_UNITS
            || self.since.is_some_and(|at| at.elapsed() >= BATCH_AGE);
        if due { self.flush() } else { Ok(()) }
    }

    /// Makes the outputs durable, then writes the batch in one transaction:
    /// a result never names an output whose bytes can still be lost.
    pub fn flush(&mut self) -> Result<(), BuildError> {
        if self.batch.is_empty() {
            return Ok(());
        }
        if let Ok(root) = std::fs::File::open(self.root) {
            let _ = rustix::fs::syncfs(&root);
        }
        self.store.commit_results(&self.batch).map_err(failure)?;
        self.batch = ResultBatch::default();
        self.since = None;
        Ok(())
    }

    /// Writes what is left and closes the store.
    pub fn close(mut self) -> Result<(), BuildError> {
        self.flush()?;
        self.store.close().map_err(failure)
    }
}
