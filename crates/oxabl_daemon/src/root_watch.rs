//! Whether the workspace root of a daemon is still the directory it started for.
//!
//! A daemon outlives the client that started it. When its root is deleted, or replaced
//! by another directory, nobody can use it any more, and it must not wait for a client
//! that never comes. The accept loop calls [`RootWatch::check`] every
//! [`CHECK_INTERVAL`]. A check is one `stat` call.
//!
//! Only a proof ends the daemon: the root is missing, is no directory, or has another
//! device and inode number. Any other error (a permission, a slow mount) is
//! [`RootState::Unknown`], and the daemon keeps serving.

use std::io;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// How often the accept loop looks at the root.
pub const CHECK_INTERVAL: Duration = Duration::from_secs(1);

/// What a check of the root found.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootState {
    /// The same directory as at the start.
    Present,
    /// The root is not the directory the daemon started for.
    Gone(String),
    /// The check failed for a reason that does not prove anything.
    Unknown(String),
}

/// The identity of a workspace root at the start of its daemon.
#[derive(Debug)]
pub struct RootWatch {
    path: PathBuf,
    device: u64,
    inode: u64,
}

impl RootWatch {
    /// Record the identity of the directory `path`.
    pub fn new(path: &Path) -> io::Result<RootWatch> {
        let metadata = std::fs::metadata(path)?;
        if !metadata.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("{} is not a directory", path.display()),
            ));
        }
        Ok(RootWatch {
            path: path.to_path_buf(),
            device: metadata.dev(),
            inode: metadata.ino(),
        })
    }

    /// Whether the root is still the directory that [`RootWatch::new`] recorded.
    pub fn check(&self) -> RootState {
        match std::fs::metadata(&self.path) {
            Ok(metadata) if !metadata.is_dir() => RootState::Gone(format!(
                "{} is not a directory any more",
                self.path.display()
            )),
            Ok(metadata) if metadata.dev() != self.device || metadata.ino() != self.inode => {
                RootState::Gone(format!("{} was replaced", self.path.display()))
            }
            Ok(_) => RootState::Present,
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
                ) =>
            {
                RootState::Gone(format!("{} was removed", self.path.display()))
            }
            Err(error) => RootState::Unknown(format!(
                "{} could not be checked: {error}",
                self.path.display()
            )),
        }
    }
}
