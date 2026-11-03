//! The cache that stores nothing.

use std::path::Path;

use super::seam::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, CompileKey, FetchOutcome, FetchRequest,
    StoreOutcome,
};

/// No cache: every lookup is a miss and every set offered is dropped.
pub struct NoCache;

struct Discard;

impl ArtifactUploader for Discard {
    fn push(&mut self, _set: ArtifactSetRef) {}

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        Vec::new()
    }
}

impl ArtifactCache for NoCache {
    fn label(&self) -> &str {
        "none"
    }

    fn writes(&self) -> bool {
        false
    }

    fn fetch(&self, _: Vec<FetchRequest>, _: &Path, _: &mut dyn FnMut(FetchOutcome)) {}

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        Box::new(Discard)
    }
}
