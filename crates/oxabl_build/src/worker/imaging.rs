//! Imaging the databases: every worker images a share of every database; and
//! the raw digest pass, which tells whether a stored image still holds.

use std::thread;
use std::time::{Duration, Instant};

use super::error::WorkerError;
use super::session::Session;
use super::{raw_fetch, schema_fetch};
use crate::key::raw_digest;
use crate::key::{RawDigest, RawPart, SchemaImage, SchemaPart};

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

/// The raw digests of a pass, or why there are none, and each session that
/// failed the pass, by its place, with its error.
pub type RawPass = (Result<Vec<RawDigest>, String>, Vec<(usize, WorkerError)>);

/// Makes the raw digest of each database across at most
/// [`raw_digest::MAX_WORKERS`] of the sessions: worker `k` of `N` makes the
/// pieces whose number is `k` modulo `N`, and worker 0 also the head. Returns
/// the digests in the order of `databases`, or why there are none, and each
/// session that failed with its error. A session that failed is out of step
/// with its worker and must not be used again; pieces that do not add up
/// leave the sessions usable.
pub fn raw_digests(
    sessions: &mut [&mut Session],
    databases: &[String],
    timeout: Duration,
) -> RawPass {
    let parts = sessions.len().min(raw_digest::MAX_WORKERS);
    let shares: Vec<Result<Vec<RawPart>, WorkerError>> = thread::scope(|scope| {
        let handles: Vec<_> = sessions[..parts]
            .iter_mut()
            .enumerate()
            .map(|(part, session)| {
                scope.spawn(move || {
                    databases
                        .iter()
                        .map(|ldb| {
                            let deadline = Instant::now() + timeout;
                            raw_fetch::fetch(
                                session,
                                ldb,
                                part,
                                parts,
                                raw_digest::BUCKETS,
                                deadline,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|handle| handle.join().expect("a raw digest thread panicked"))
            .collect()
    });
    let mut per_worker = Vec::with_capacity(parts);
    let mut failed = Vec::new();
    for (at, share) in shares.into_iter().enumerate() {
        match share {
            Ok(share) => per_worker.push(share.into_iter()),
            Err(error) => failed.push((at, error)),
        }
    }
    if let Some((at, error)) = failed.first() {
        return (
            Err(format!("worker {at} of the raw digest pass: {error}")),
            failed,
        );
    }
    if parts == 0 {
        return (Err("no worker made the raw digest".to_string()), failed);
    }
    let digests = databases
        .iter()
        .map(|ldb| {
            let parts: Vec<RawPart> = per_worker
                .iter_mut()
                .map(|shares| shares.next().expect("one share per database"))
                .collect();
            RawDigest::combine(ldb.clone(), parts, raw_digest::BUCKETS)
        })
        .collect();
    (digests, failed)
}
