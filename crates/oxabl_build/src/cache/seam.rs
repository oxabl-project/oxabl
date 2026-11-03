//! The seam between the build and a cache of compile outputs.

use std::path::PathBuf;

/// The 32-byte key of one compile.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CompileKey(pub [u8; 32]);

/// A file's place in the artifact tree: relative, `/`-separated, and below one
/// of `r`, `xref`, `dbg` or `lst`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArtifactPath(pub String);

/// One output file of a compiled unit.
#[derive(Clone, Debug)]
pub struct ArtifactFileRef {
    pub path: ArtifactPath,
    /// Where the file is now, in its configured output directory.
    pub disk_path: PathBuf,
    pub size: u64,
    pub blake3: [u8; 32],
}

/// The outputs of one compiled unit.
#[derive(Clone, Debug)]
pub struct ArtifactSetRef {
    pub key: CompileKey,
    pub files: Vec<ArtifactFileRef>,
}

/// What a cache did with one set.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StoreOutcome {
    Stored,
    AlreadyPresent,
    NotStored(NotStoredReason),
}

/// Why a set was not stored.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotStoredReason {
    ChangedOnDisk,
    Unreadable,
    Rejected,
    TooLarge,
    QuotaExceeded,
    Unavailable,
}

/// Takes the outputs of units that compiled and passed verification, one set
/// at a time while the build runs.
pub trait ArtifactUploader: Send {
    fn push(&mut self, set: ArtifactSetRef);

    /// Flushes and reports what happened to each key.
    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)>;
}

/// A cache of compile outputs.
pub trait ArtifactCache: Send + Sync {
    fn label(&self) -> &str;

    fn uploader(&self) -> Box<dyn ArtifactUploader>;
}
