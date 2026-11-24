//! The local cache: a content-addressed directory of artifact sets that every
//! process of the user shares.

mod accounting;
mod directory;
mod fetch;
mod hashing;
mod layout;
mod manifest;
mod place;
mod store;

use std::path::Path;

use self::layout::Layout;
use super::seam::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, FetchOutcome, FetchRequest, RemoteOrigin,
};

/// A local cache in one directory.
pub struct LocalCache {
    layout: Layout,
    limit: u64,
}

impl LocalCache {
    /// Opens the cache in `directory`, creating it with mode `0700`. A
    /// directory that another user owns or that others can write is an error.
    /// `limit` is the size the cache may hold, in bytes.
    pub fn open(directory: &Path, limit: u64) -> Result<LocalCache, String> {
        let layout = Layout::new(directory);
        directory::prepare(directory, &layout)?;
        Ok(LocalCache { layout, limit })
    }
}

impl ArtifactCache for LocalCache {
    fn label(&self) -> &str {
        "local"
    }

    fn fetch(
        &self,
        requests: Vec<FetchRequest>,
        stage: &Path,
        on_outcome: &mut dyn FnMut(FetchOutcome),
    ) {
        fetch::fetch(&self.layout, requests, stage, on_outcome);
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        Box::new(store::Uploader::new(self.layout.clone(), self.limit))
    }

    fn fill(&self, sets: Vec<(ArtifactSetRef, RemoteOrigin)>) {
        store::store_remote(&self.layout, sets, self.limit);
    }
}
