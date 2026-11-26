//! Whether the workspace root of a daemon is still the directory it started for.
//!
//! A daemon outlives the client that started it. When its root is deleted, nobody can
//! use it any more, and it must not wait for a client that never comes. The root watch
//! opens the root at the start and keeps the descriptor. A [`RootWatch::check`] is one
//! `fstat` call and one `stat` call.
//!
//! Only a proof ends a daemon that has a client: the open directory has no link any
//! more, or the path is missing or is no directory. A path that now names another
//! device or inode number is [`RootState::Replaced`]. Some file systems (SMB without
//! server inode numbers, an overlay without `xino`, an NFS automount) do not keep these
//! numbers, so the daemon ends on this alone only when it has no client. Any other
//! error (a permission, a slow mount) is [`RootState::Unknown`], and the daemon keeps
//! serving.

use std::io;
use std::os::fd::OwnedFd;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::time::Duration;

use rustix::fs::{Mode, OFlags};

/// How often the root watch thread looks at the root.
pub const CHECK_INTERVAL: Duration = Duration::from_secs(1);

/// What a check of the root found.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RootState {
    /// The same directory as at the start.
    Present,
    /// The root is deleted, or its path is missing or names no directory.
    Gone(String),
    /// The path names a directory with another device or inode number.
    Replaced(String),
    /// The check failed for a reason that does not prove anything.
    Unknown(String),
}

/// The workspace root of a daemon, held open since its start.
#[derive(Debug)]
pub struct RootWatch {
    path: PathBuf,
    root: OwnedFd,
    device: u64,
    inode: u64,
}

impl RootWatch {
    /// Open the directory `path` and record its identity.
    pub fn new(path: &Path) -> io::Result<RootWatch> {
        let root = rustix::fs::open(
            path,
            OFlags::RDONLY | OFlags::DIRECTORY | OFlags::CLOEXEC,
            Mode::empty(),
        )?;
        let stat = rustix::fs::fstat(&root)?;
        Ok(RootWatch {
            path: path.to_path_buf(),
            root,
            device: stat.st_dev as u64,
            inode: stat.st_ino as u64,
        })
    }

    /// Whether the root is still the directory that [`RootWatch::new`] opened.
    pub fn check(&self) -> RootState {
        match rustix::fs::fstat(&self.root) {
            Ok(stat) if stat.st_nlink == 0 => {
                return RootState::Gone(format!("{} was removed", self.path.display()));
            }
            Ok(_) => {}
            Err(error) => {
                return RootState::Unknown(format!(
                    "{} could not be checked: {error}",
                    self.path.display()
                ));
            }
        }
        match std::fs::metadata(&self.path) {
            Ok(metadata) if !metadata.is_dir() => RootState::Gone(format!(
                "{} is not a directory any more",
                self.path.display()
            )),
            Ok(metadata) if metadata.dev() != self.device || metadata.ino() != self.inode => {
                RootState::Replaced(format!("{} was replaced", self.path.display()))
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
