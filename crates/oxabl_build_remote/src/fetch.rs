//! `fetch`: the client's stream of outcomes, delivered one by one.

use std::collections::BTreeMap;
use std::path::Path;

use oxabl_build::cache::{FetchOutcome, FetchRequest, MissReason};
use oxabl_cache_client as client;

use crate::RemoteCache;
use crate::convert::{client_key, client_path, miss_reason, origin, seam_key, seam_path};

/// A request the client can take, or the miss that the build records for it.
fn request(request: &FetchRequest) -> Result<client::FetchRequest, FetchOutcome> {
    let expect: Option<Vec<_>> = request.expect.iter().map(client_path).collect();
    match expect {
        Some(expect) => Ok(client::FetchRequest {
            key: client_key(request.key),
            expect: Some(expect),
        }),
        None => Err(FetchOutcome::Miss {
            key: request.key,
            reason: MissReason::PathMismatch,
        }),
    }
}

fn outcome(outcome: client::FetchOutcome) -> FetchOutcome {
    match outcome {
        client::FetchOutcome::Installed {
            key,
            files,
            provenance,
        } => FetchOutcome::Installed {
            key: seam_key(key),
            files: files.iter().map(|file| seam_path(&file.path)).collect(),
            origin: origin(&provenance),
        },
        client::FetchOutcome::Miss { key, reason } => FetchOutcome::Miss {
            key: seam_key(key),
            reason: miss_reason(reason),
        },
    }
}

/// One line about the misses that are not plain absence.
fn trouble(misses: &BTreeMap<&'static str, usize>, asked: usize) -> Option<String> {
    let total: usize = misses.values().sum();
    if total == 0 {
        return None;
    }
    let parts: Vec<String> = misses
        .iter()
        .map(|(reason, count)| format!("{count} {reason}"))
        .collect();
    Some(format!(
        "the remote cache could not give {total} of {asked} keys ({}); those units compile",
        parts.join(", ")
    ))
}

fn name(reason: MissReason) -> Option<&'static str> {
    match reason {
        MissReason::NotFound | MissReason::PathMismatch => None,
        MissReason::Corrupt => Some("corrupt"),
        MissReason::Untrusted => Some("untrusted"),
        MissReason::InstallFailed => Some("install failed"),
        MissReason::Unreadable => Some("unreadable"),
        MissReason::Unavailable => Some("unavailable"),
        MissReason::Timeout => Some("timed out"),
    }
}

pub fn run(
    remote: &RemoteCache,
    requests: Vec<FetchRequest>,
    stage: &Path,
    on_outcome: &mut dyn FnMut(FetchOutcome),
) {
    let asked = requests.len();
    let mut taken = Vec::with_capacity(asked);
    for one in &requests {
        match request(one) {
            Ok(request) => taken.push(request),
            Err(miss) => on_outcome(miss),
        }
    }
    let mut options = client::FetchOptions::new(stage.to_path_buf());
    options.durability = client::Durability::Caller;
    options.idle_timeout = remote.idle_timeout;
    let mut misses: BTreeMap<&'static str, usize> = BTreeMap::new();
    remote.runtime.block_on(async {
        let mut fetch = remote.cache.fetch(taken, options);
        // The client counts the time its outcome buffer is full as no progress, and
        // ends the fetch after the idle timeout. Installing an outcome here is slower
        // than a download, so the stream is drained at once and installed from the
        // queue.
        let (queue, mut queued) = tokio::sync::mpsc::unbounded_channel();
        let drain = tokio::spawn(async move {
            while let Some(next) = fetch.next().await {
                if queue.send(next).is_err() {
                    break;
                }
            }
        });
        loop {
            let next = tokio::select! {
                next = queued.recv() => next,
                () = remote.cancel.notified() => None,
            };
            let Some(next) = next else { break };
            let next = outcome(next);
            if let FetchOutcome::Miss { reason, .. } = &next
                && let Some(reason) = name(*reason)
            {
                *misses.entry(reason).or_default() += 1;
            }
            on_outcome(next);
        }
        drain.abort();
    });
    if let Some(note) = trouble(&misses, asked) {
        remote.notes.push(note);
    }
}
