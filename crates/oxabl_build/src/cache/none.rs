//! The cache that stores nothing.

use super::seam::{ArtifactCache, ArtifactSetRef, ArtifactUploader, CompileKey, StoreOutcome};

/// No cache: every set offered is dropped.
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

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        Box::new(Discard)
    }
}
