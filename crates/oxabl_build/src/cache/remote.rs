//! The seam for a remote cache that another crate supplies, so that this crate
//! needs no network stack.

use std::path::Path;
use std::time::Duration;

use super::seam::ArtifactCache;

/// Why a remote cache is not open.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RemoteError {
    /// The build must stop: an attempt to replace the root of trust.
    Refused(String),
    /// The build runs without the remote cache and says why.
    Unavailable(String),
}

/// Opens the remote cache of a workspace.
pub trait RemoteOpener {
    /// `idle_timeout` ends a fetch that makes no progress for that long.
    fn open(
        &self,
        root: &Path,
        idle_timeout: Duration,
    ) -> Result<Box<dyn ArtifactCache>, RemoteError>;
}
