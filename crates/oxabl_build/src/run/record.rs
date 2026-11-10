//! Writing results to the store in batches, on a thread of its own: a `syncfs`
//! and a commit never hold up the thread that hands out jobs.

use std::path::{Path, PathBuf};
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::error::BuildError;
use crate::store::{OutputRow, ResultBatch, ResultRow, Store};

/// A batch is written when it holds this many results.
const BATCH_UNITS: usize = 500;
/// A batch is written when its first result is this old.
const BATCH_AGE: Duration = Duration::from_secs(2);

/// What the writer thread is asked to do.
enum Write {
    Batch(ResultBatch),
    /// Answers once every batch before it is written, or has failed.
    Wait(Sender<()>),
}

/// The store of a build, written one batch at a time.
pub struct Recorder {
    batch: ResultBatch,
    since: Option<Instant>,
    writes: Option<Sender<Write>>,
    /// Gives back the store, or the first error; after an error no batch is
    /// written and the store is not marked clean.
    writer: Option<JoinHandle<Result<Store, String>>>,
    /// The first error of the writer that this side has seen.
    failed: Option<String>,
    errors: Receiver<String>,
}

fn failure(error: impl std::fmt::Display) -> BuildError {
    BuildError::Infrastructure(error.to_string())
}

/// Makes the outputs durable, then writes each batch in one transaction: a
/// result never names an output whose bytes can still be lost.
fn write(
    mut store: Store,
    root: PathBuf,
    writes: Receiver<Write>,
    errors: Sender<String>,
) -> Result<Store, String> {
    let mut failed = None;
    for request in writes {
        match request {
            Write::Batch(batch) if failed.is_none() => {
                if let Ok(root) = std::fs::File::open(&root) {
                    let _ = rustix::fs::syncfs(&root);
                }
                if let Err(error) = store.commit_results(&batch) {
                    let _ = errors.send(error.to_string());
                    failed = Some(error.to_string());
                }
            }
            Write::Batch(_) => {}
            Write::Wait(done) => {
                let _ = done.send(());
            }
        }
    }
    match failed {
        None => Ok(store),
        Some(error) => Err(error),
    }
}

impl Recorder {
    pub fn new(store: Store, root: &Path) -> Recorder {
        let (writes, requests) = mpsc::channel();
        let (failures, errors) = mpsc::channel();
        let root = root.to_path_buf();
        let writer = thread::Builder::new()
            .name("oxabl-store".to_string())
            .spawn(move || write(store, root, requests, failures))
            .expect("the store writer thread starts");
        Recorder {
            batch: ResultBatch::default(),
            since: None,
            writes: Some(writes),
            writer: Some(writer),
            failed: None,
            errors,
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

    /// The first error the writer met, if any.
    fn error(&mut self) -> Result<(), BuildError> {
        if self.failed.is_none() {
            self.failed = self.errors.try_recv().ok();
        }
        match &self.failed {
            Some(error) => Err(failure(error)),
            None => Ok(()),
        }
    }

    /// Hands `write` to the writer; a writer that is gone is an error.
    fn ask(&mut self, write: Write) {
        let sent = self
            .writes
            .as_ref()
            .is_some_and(|writes| writes.send(write).is_ok());
        if !sent && self.failed.is_none() {
            self.failed = Some("the store writer stopped".to_string());
        }
    }

    fn send(&mut self) {
        if self.batch.is_empty() {
            return;
        }
        let batch = std::mem::take(&mut self.batch);
        self.since = None;
        self.ask(Write::Batch(batch));
    }

    /// Hands the batch to the writer when it is full or old. Fails once a
    /// batch could not be written.
    pub fn flush_due(&mut self) -> Result<(), BuildError> {
        let due = self.batch.upserts.len() >= BATCH_UNITS
            || self.since.is_some_and(|at| at.elapsed() >= BATCH_AGE);
        if due {
            self.send();
        }
        self.error()
    }

    /// Writes the batch and waits until every batch is in the store.
    pub fn flush(&mut self) -> Result<(), BuildError> {
        self.send();
        let (done, wait) = mpsc::channel();
        self.ask(Write::Wait(done));
        if wait.recv().is_err() && self.failed.is_none() {
            self.failed = self.errors.try_recv().ok();
            self.failed
                .get_or_insert_with(|| "the store writer stopped".to_string());
        }
        self.error()
    }

    /// Writes what is left and closes the store.
    pub fn close(mut self) -> Result<(), BuildError> {
        self.send();
        self.writes = None;
        let writer = self.writer.take().expect("the writer runs until close");
        match writer.join() {
            Ok(Ok(store)) => store.close().map_err(failure),
            Ok(Err(error)) => Err(failure(error)),
            Err(_) => Err(failure("the store writer panicked")),
        }
    }
}
