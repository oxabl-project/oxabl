//! Caches of compile outputs: the seam, the local cache and how they chain.

pub mod job_dir;
pub mod layered;
pub mod local;
pub mod mode;
mod none;
pub mod seam;
pub mod select;

pub use layered::Layered;
pub use local::LocalCache;
pub use mode::Mode;
pub use none::NoCache;
pub use seam::{
    ArtifactCache, ArtifactFileRef, ArtifactPath, ArtifactSetRef, ArtifactUploader, CompileKey,
    FetchOutcome, FetchRequest, MissReason, NotStoredReason, Origin, RemoteOrigin, StoreOutcome,
};
pub use select::{Environment, Flags, Selection, select};
