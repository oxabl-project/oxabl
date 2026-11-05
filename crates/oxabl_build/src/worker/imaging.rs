//! Imaging the databases: every worker images a share of every database.

use std::thread;
use std::time::{Duration, Instant};

use super::error::WorkerError;
use super::schema_fetch;
use super::session::Session;
use crate::key::{SchemaImage, SchemaPart};

/// Images each database across all workers at once. Worker `k` of `N` images
/// the tables whose place in the sorted table list is `k` modulo `N`; worker 0
/// also images the database record and the sequences. Each share has
/// `timeout`. Returns the images in the order of `databases`, or an error when
/// a worker fails or the shares do not add up to the whole database.
pub fn image_databases(
    sessions: &mut [&mut Session],
    databases: &[String],
    timeout: Duration,
) -> Result<Vec<SchemaImage>, WorkerError> {
    let parts = sessions.len();
    let shares: Vec<Result<Vec<SchemaPart>, WorkerError>> = thread::scope(|scope| {
        let handles: Vec<_> = sessions
            .iter_mut()
            .enumerate()
            .map(|(part, session)| {
                scope.spawn(move || {
                    databases
                        .iter()
                        .map(|ldb| {
                            let deadline = Instant::now() + timeout;
                            schema_fetch::fetch(session, ldb, part, parts, deadline)
                        })
                        .collect::<Result<Vec<_>, _>>()
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|handle| handle.join().expect("an imaging thread panicked"))
            .collect()
    });
    let mut per_worker = Vec::with_capacity(parts);
    for share in shares {
        per_worker.push(share?.into_iter());
    }
    databases
        .iter()
        .map(|ldb| {
            let parts: Vec<SchemaPart> = per_worker
                .iter_mut()
                .map(|shares| shares.next().expect("one share per database"))
                .collect();
            SchemaImage::combine(ldb.clone(), parts).map_err(WorkerError::Protocol)
        })
        .collect()
}
