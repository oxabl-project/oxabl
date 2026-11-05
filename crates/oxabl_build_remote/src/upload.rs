//! The uploaders: the client's, and the two that stand in when it cannot be.

use std::sync::Arc;

use oxabl_build::cache::{
    ArtifactSetRef, ArtifactUploader, CompileKey, NotStoredReason, StoreOutcome,
};
use oxabl_cache_client as client;
use tokio::runtime::Runtime;

use crate::convert::{client_file, client_key, seam_key, store_outcome};

/// The uploader of a mode that does not write: `push` does nothing.
pub struct Idle;

impl ArtifactUploader for Idle {
    fn push(&mut self, _set: ArtifactSetRef) {}

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        Vec::new()
    }
}

/// The uploader of a writing mode that could not start: every set it is given
/// is reported as not stored.
#[derive(Default)]
pub struct Failed {
    keys: Vec<CompileKey>,
}

impl ArtifactUploader for Failed {
    fn push(&mut self, set: ArtifactSetRef) {
        self.keys.push(set.key);
    }

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        self.keys
            .into_iter()
            .map(|key| (key, StoreOutcome::NotStored(NotStoredReason::Unavailable)))
            .collect()
    }
}

/// The client's uploader. A set with a path the client cannot take is
/// rejected here and never offered.
pub struct Remote {
    runtime: Arc<Runtime>,
    uploader: client::Uploader,
    rejected: Vec<CompileKey>,
}

impl Remote {
    pub fn new(runtime: Arc<Runtime>, uploader: client::Uploader) -> Remote {
        Remote {
            runtime,
            uploader,
            rejected: Vec::new(),
        }
    }
}

impl ArtifactUploader for Remote {
    fn push(&mut self, set: ArtifactSetRef) {
        let files: Option<Vec<_>> = set.files.iter().map(client_file).collect();
        match files {
            Some(files) => self
                .runtime
                .block_on(self.uploader.put(client_key(set.key), files)),
            None => self.rejected.push(set.key),
        }
    }

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        let Remote {
            runtime,
            uploader,
            rejected,
        } = *self;
        let report = runtime.block_on(uploader.finish());
        let mut outcomes: Vec<_> = report
            .outcomes
            .into_iter()
            .map(|(key, outcome)| (seam_key(key), store_outcome(outcome)))
            .collect();
        outcomes.extend(
            rejected
                .into_iter()
                .map(|key| (key, StoreOutcome::NotStored(NotStoredReason::Rejected))),
        );
        outcomes
    }
}
