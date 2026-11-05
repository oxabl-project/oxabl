//! The cache seam: what a build offers a cache and what a cache answers.

mod none;
pub mod seam;

pub use none::NoCache;
pub use seam::{
    ArtifactCache, ArtifactFileRef, ArtifactPath, ArtifactSetRef, ArtifactUploader, CompileKey,
    NotStoredReason, StoreOutcome,
};
