use std::fs::{File, OpenOptions, TryLockError};
use std::io::Write;
use std::path::Path;

const FILE_NAME: &str = "build.lock";

/// The exclusive lock on a workspace's state directory. The kernel releases it
/// when the holder exits, so a stale lock cannot exist.
pub struct WorkspaceLock {
    _file: File,
}

/// The outcome of asking for the workspace lock without waiting.
pub enum LockOutcome {
    Held(WorkspaceLock),
    /// Another process holds it, with its process id when the file names one.
    Busy(Option<u32>),
    /// The lock file could not be opened, so nothing can be locked.
    Unavailable(String),
}

/// Try to take the lock in `state_dir`, creating the directory when needed.
pub fn try_acquire(state_dir: &Path) -> LockOutcome {
    try_acquire_file(state_dir, FILE_NAME)
}

/// Try to take the lock file `name` in `directory`, creating the directory
/// when needed.
pub fn try_acquire_file(directory: &Path, name: &str) -> LockOutcome {
    if let Err(e) = std::fs::create_dir_all(directory) {
        return LockOutcome::Unavailable(e.to_string());
    }
    let path = directory.join(name);
    let mut file = match OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&path)
    {
        Ok(file) => file,
        Err(e) => return LockOutcome::Unavailable(e.to_string()),
    };
    match file.try_lock() {
        Ok(()) => {
            let _ = file.set_len(0);
            let _ = write!(file, "{}", std::process::id());
            LockOutcome::Held(WorkspaceLock { _file: file })
        }
        Err(TryLockError::WouldBlock) => LockOutcome::Busy(
            std::fs::read_to_string(&path)
                .ok()
                .and_then(|s| s.trim().parse().ok()),
        ),
        Err(TryLockError::Error(e)) => LockOutcome::Unavailable(e.to_string()),
    }
}
