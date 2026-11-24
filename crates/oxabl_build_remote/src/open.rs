//! How `oxabl build` opens the remote cache.

use std::path::Path;
use std::time::Duration;

use oxabl_build::cache::{ArtifactCache, RemoteError, RemoteOpener};

use crate::RemoteCache;

/// Opens the [`RemoteCache`] of a workspace.
pub struct RemoteCacheOpener;

impl RemoteOpener for RemoteCacheOpener {
    fn open(
        &self,
        root: &Path,
        idle_timeout: Duration,
    ) -> Result<Box<dyn ArtifactCache>, RemoteError> {
        RemoteCache::open(root, idle_timeout).map(|cache| Box::new(cache) as _)
    }
}
