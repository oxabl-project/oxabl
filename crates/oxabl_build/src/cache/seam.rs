//! The seam between the build and a cache of compile outputs.

use std::path::{Path, PathBuf};

/// The 32-byte key of one compile.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CompileKey(pub [u8; 32]);

impl CompileKey {
    /// The key as 64 lower-case hex digits.
    pub fn hex(&self) -> String {
        blake3::Hash::from_bytes(self.0).to_hex().to_string()
    }
}

/// A file's place in the artifact tree: relative, `/`-separated, and below one
/// of `r`, `xref`, `dbg` or `lst`.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ArtifactPath(pub String);

impl ArtifactPath {
    /// Whether the path is relative, has no `.` or `..` component and starts
    /// below one of the four artifact kinds.
    pub fn is_valid(&self) -> bool {
        let mut parts = self.0.split('/');
        let kind_ok = matches!(parts.next(), Some("r" | "xref" | "dbg" | "lst"));
        let mut any = false;
        let rest_ok = parts.all(|part| {
            any = true;
            !part.is_empty() && part != "." && part != ".." && !part.contains('\0')
        });
        kind_ok && any && rest_ok
    }
}

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

/// One key to look up and the artifact paths the build requires of its set.
#[derive(Clone, Debug)]
pub struct FetchRequest {
    pub key: CompileKey,
    pub expect: Vec<ArtifactPath>,
}

/// The tier and the signer a remote cache names for an entry.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RemoteOrigin {
    pub tier: String,
    pub signer: String,
}

/// Where an installed set came from.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Origin {
    /// The local cache. `recorded` is what the entry's manifest says about a
    /// remote origin: nothing authenticates it.
    Local {
        recorded: Option<RemoteOrigin>,
    },
    Remote(RemoteOrigin),
}

/// Why a key is a miss.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MissReason {
    NotFound,
    /// The entry failed a check and was set aside.
    Corrupt,
    /// The signer of the entry is not accepted.
    Untrusted,
    /// The set could not be installed in the stage.
    InstallFailed,
    /// The entry holds other artifact paths than the build requires.
    PathMismatch,
    Unreadable,
    Unavailable,
    Timeout,
}

/// The answer for one key.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FetchOutcome {
    /// The set is complete and verified at `stage/<artifact path>`.
    Installed {
        key: CompileKey,
        files: Vec<ArtifactPath>,
        origin: Origin,
    },
    Miss {
        key: CompileKey,
        reason: MissReason,
    },
}

/// Takes the outputs of units that compiled and passed verification, one set
/// at a time while the build runs.
pub trait ArtifactUploader: Send {
    fn push(&mut self, set: ArtifactSetRef);

    /// Flushes and reports what happened to each key.
    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)>;
}

/// A cache of compile outputs. A failure is a miss or a failed store, never
/// an error.
pub trait ArtifactCache: Send + Sync {
    fn label(&self) -> &str;

    /// Whether `uploader` stores anything. A build that no cache writes to
    /// skips verification.
    fn writes(&self) -> bool {
        true
    }

    /// Looks up all `requests` at once and calls `on_outcome` once per key as
    /// each completes. Every key is installed below `stage`, at its artifact
    /// paths; an installed set stays installed whatever happens to later keys.
    /// A key that is never reported is a miss.
    fn fetch(
        &self,
        requests: Vec<FetchRequest>,
        stage: &Path,
        on_outcome: &mut dyn FnMut(FetchOutcome),
    );

    fn uploader(&self) -> Box<dyn ArtifactUploader>;

    /// Ends a `fetch` that runs on another thread, at once: the keys it has
    /// not reported are misses.
    fn cancel_fetch(&self) {}

    /// Warnings the cache collected since the last call, each one a line for
    /// the user. Taking them clears them.
    fn notes(&self) -> Vec<String> {
        Vec::new()
    }

    /// Stores a set that a remote tier gave, from its final paths, where it is
    /// nearer the machine. Best effort.
    fn fill(&self, _set: ArtifactSetRef, _origin: &RemoteOrigin) {}
}
