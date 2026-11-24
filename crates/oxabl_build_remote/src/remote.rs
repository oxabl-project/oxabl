//! The remote cache: a client handle and the runtime that drives it.

use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use oxabl_build::cache::{
    ArtifactCache, ArtifactUploader, FetchOutcome, FetchRequest, RemoteError,
};
use oxabl_cache_client::{Cache, Config, ConfigError, OpenError};
use tokio::runtime::{Builder, Runtime};
use tokio::sync::Notify;

use crate::fetch;
use crate::notes::Notes;
use crate::upload::{Failed, Idle, Remote};

/// The remote cache of a workspace, open and verified against the roster that
/// `.oxabl/cache.toml` pins.
pub struct RemoteCache {
    pub(crate) runtime: Arc<Runtime>,
    pub(crate) cache: Cache,
    pub(crate) idle_timeout: Duration,
    pub(crate) notes: Notes,
    pub(crate) cancel: Notify,
}

fn refusal(variable: &str) -> RemoteError {
    RemoteError::Refused(format!(
        "{variable} cannot override a trust field of .oxabl/cache.toml; unset it"
    ))
}

impl RemoteCache {
    /// Opens the cache that `<root>/.oxabl/cache.toml` names. An attempt to
    /// override the root of trust is refused; any other failure is
    /// unavailable.
    pub fn open(root: &Path, idle_timeout: Duration) -> Result<RemoteCache, RemoteError> {
        let config = Config::from_repo(root).map_err(|error| match error {
            ConfigError::TrustOverride(variable) => refusal(variable),
            other => RemoteError::Unavailable(other.to_string()),
        })?;
        let runtime = Builder::new_multi_thread()
            .enable_all()
            .build()
            .map_err(|error| RemoteError::Unavailable(format!("no runtime: {error}")))?;
        let cache = runtime
            .block_on(Cache::open(config))
            .map_err(|error| match error {
                OpenError::Config(ConfigError::TrustOverride(variable)) => refusal(variable),
                other => RemoteError::Unavailable(other.to_string()),
            })?;
        Ok(RemoteCache {
            runtime: Arc::new(runtime),
            cache,
            idle_timeout,
            notes: Notes::default(),
            cancel: Notify::new(),
        })
    }
}

impl ArtifactCache for RemoteCache {
    fn label(&self) -> &str {
        "remote"
    }

    fn writes(&self) -> bool {
        self.cache.mode().writes()
    }

    fn fetch(
        &self,
        requests: Vec<FetchRequest>,
        stage: &Path,
        on_outcome: &mut dyn FnMut(FetchOutcome),
    ) {
        fetch::run(self, requests, stage, on_outcome);
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        if !self.writes() {
            return Box::new(Idle);
        }
        let _enter = self.runtime.enter();
        match self.cache.uploader() {
            Ok(uploader) => Box::new(Remote::new(self.runtime.clone(), uploader)),
            Err(error) => {
                self.notes
                    .push(format!("the remote cache takes no uploads: {error}"));
                Box::new(Failed::default())
            }
        }
    }

    fn cancel_fetch(&self) {
        self.cancel.notify_one();
    }

    fn notes(&self) -> Vec<String> {
        self.notes.take()
    }
}
