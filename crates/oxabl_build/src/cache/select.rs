//! Which caches a build uses, from the configuration, the flags and the
//! environment.

use std::ffi::OsString;
use std::path::{Path, PathBuf};

use oxabl_workspace::BuildCache;

use super::job_dir;
use super::layered::Layered;
use super::local::LocalCache;
use super::mode::Mode;
use super::remote::{RemoteError, RemoteOpener};
use super::seam::ArtifactCache;
use crate::cache::NoCache;
use crate::config::BuildWorkspace;
use crate::error::BuildError;

/// What the command line says about caches.
#[derive(Clone, Copy, Debug, Default)]
pub struct Flags {
    pub no_cache: bool,
    pub no_remote_cache: bool,
}

/// The cache of a build and what to tell the user about it.
pub struct Selection {
    pub cache: Box<dyn ArtifactCache>,
    pub mode: Mode,
    pub notes: Vec<String>,
}

/// The environment that decides the mode and the directories.
#[derive(Clone, Debug, Default)]
pub struct Environment {
    pub cache_mode: Option<OsString>,
    pub ci: Option<OsString>,
    pub cache_dir: Option<OsString>,
    pub xdg_cache_home: Option<OsString>,
    pub home: Option<OsString>,
}

impl Environment {
    /// The variables of this process.
    pub fn from_process() -> Environment {
        Environment {
            cache_mode: std::env::var_os("OXABL_CACHE_MODE"),
            ci: std::env::var_os("CI"),
            cache_dir: std::env::var_os("OXABL_BUILD_CACHE_DIR"),
            xdg_cache_home: std::env::var_os("XDG_CACHE_HOME"),
            home: std::env::var_os("HOME"),
        }
    }
}

fn non_empty(value: &Option<OsString>) -> Option<&Path> {
    value
        .as_deref()
        .filter(|value| !value.is_empty())
        .map(Path::new)
}

/// The shared local cache directory: `OXABL_BUILD_CACHE_DIR`, then
/// `[build.cache].local`, then the user cache directory.
fn shared_directory(config: &BuildCache, environment: &Environment) -> Option<PathBuf> {
    if let Some(directory) = non_empty(&environment.cache_dir) {
        return Some(directory.to_path_buf());
    }
    if !config.local.as_os_str().is_empty() {
        return Some(config.local.clone());
    }
    if cfg!(target_os = "macos") {
        return non_empty(&environment.home)
            .map(|home| home.join("Library/Caches/oxabl/artifacts"));
    }
    non_empty(&environment.xdg_cache_home)
        .map(|cache| cache.join("oxabl/artifacts"))
        .or_else(|| non_empty(&environment.home).map(|home| home.join(".cache/oxabl/artifacts")))
}

/// Whether the workspace turns the remote cache on: `[build.cache].remote`,
/// else whether `.oxabl/cache.toml` exists.
pub fn remote_wanted(config: &BuildCache, root: &Path) -> bool {
    config
        .remote
        .unwrap_or_else(|| root.join(".oxabl/cache.toml").is_file())
}

/// Opens the remote cache when the workspace wants it and the flags allow it.
fn open_remote(
    config: &BuildCache,
    root: &Path,
    flags: Flags,
    opener: Option<&dyn RemoteOpener>,
    notes: &mut Vec<String>,
) -> Result<Option<Box<dyn ArtifactCache>>, BuildError> {
    if !config.enabled || flags.no_cache || flags.no_remote_cache || !remote_wanted(config, root) {
        return Ok(None);
    }
    if !root.join(".oxabl/cache.toml").is_file() {
        return Err(BuildError::Usage(
            "[build.cache].remote is true, but .oxabl/cache.toml does not exist".to_string(),
        ));
    }
    let Some(opener) = opener else {
        return Err(BuildError::Usage(
            "the remote cache is on, but this oxabl was built without remote-cache; rebuild with --features remote-cache or pass --no-remote-cache"
                .to_string(),
        ));
    };
    match opener.open(root, config.idle_timeout.0) {
        Ok(remote) => Ok(Some(remote)),
        Err(RemoteError::Refused(message)) => Err(BuildError::Usage(message)),
        Err(RemoteError::Unavailable(message)) => {
            notes.push(format!("the remote cache is not used: {message}"));
            Ok(None)
        }
    }
}

/// Chooses the caches of a build. `remote` opens the remote cache; a binary
/// without one passes `None`.
pub fn select(
    workspace: &BuildWorkspace,
    flags: Flags,
    environment: &Environment,
    remote: Option<&dyn RemoteOpener>,
) -> Result<Selection, BuildError> {
    let config = &workspace.config.build.cache;
    let root = &workspace.settings.root;
    let mode = Mode::resolve(environment.cache_mode.as_deref(), environment.ci.as_deref())?;
    let mut notes = Vec::new();
    let remote = open_remote(config, root, flags, remote, &mut notes)?;
    if !config.enabled || flags.no_cache {
        return Ok(Selection {
            cache: Box::new(NoCache),
            mode,
            notes,
        });
    }
    let limit = config.local_max_size.0;
    let directory = match mode {
        Mode::Developer => shared_directory(config, environment),
        Mode::Ci | Mode::ReadOnly => match config.local_job_dir.as_os_str().is_empty() {
            true => None,
            false => {
                let given = if config.local_job_dir.is_absolute() {
                    config.local_job_dir.clone()
                } else {
                    root.join(&config.local_job_dir)
                };
                Some(job_dir::prepare(
                    &given,
                    root,
                    non_empty(&environment.home),
                )?)
            }
        },
    };
    let local: Option<Box<dyn ArtifactCache>> = match directory {
        None => {
            if mode == Mode::Developer {
                notes.push(
                    "the local cache is not used: no directory is known; set OXABL_BUILD_CACHE_DIR, XDG_CACHE_HOME or HOME"
                        .to_string(),
                );
            }
            None
        }
        Some(directory) => match LocalCache::open(&directory, limit) {
            Ok(local) => Some(Box::new(local)),
            Err(message) => {
                notes.push(format!("the local cache is not used: {message}"));
                None
            }
        },
    };
    let cache: Box<dyn ArtifactCache> = match (local, remote) {
        (Some(local), Some(remote)) => Box::new(Layered::new(local, remote)),
        (Some(cache), None) | (None, Some(cache)) => cache,
        (None, None) => Box::new(NoCache),
    };
    Ok(Selection { cache, mode, notes })
}
