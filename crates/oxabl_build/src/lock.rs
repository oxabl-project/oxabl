use std::fs::{File, OpenOptions, TryLockError};
use std::io::Write;
use std::path::Path;

const FILE_NAME: &str = "build.lock";

/// The exclusive lock on a workspace's state directory. The kernel releases it
/// when the holder exits, so a stale lock cannot exist.
pub struct WorkspaceLock {
    file: File,
}

impl Drop for WorkspaceLock {
    /// Unlocks before the descriptor closes. A child that another thread of
    /// the process forked while the lock was held shares the lock until it
    /// execs, so closing the descriptor alone leaves the lock taken for that
    /// long, and a build that starts at once finds it busy.
    fn drop(&mut self) {
        let _ = self.file.unlock();
    }
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

/// Like [`try_acquire`], and a build that `yield`s marks the lock file so a
/// newer build may stop it.
pub fn try_acquire_marked(state_dir: &Path, yielding: bool) -> LockOutcome {
    try_acquire_as(state_dir, FILE_NAME, yielding)
}

/// The holder of the workspace lock in `state_dir`, as its file names it.
pub fn holder(state_dir: &Path) -> Option<Holder> {
    parse_holder(&std::fs::read_to_string(state_dir.join(FILE_NAME)).ok()?)
}

/// The process the lock file names, and whether it marked itself as yielding.
#[derive(Debug, PartialEq, Eq)]
pub struct Holder {
    pub pid: u32,
    pub yielding: bool,
}

/// The first token is the pid; a second token `yield` marks a yielding build.
fn parse_holder(text: &str) -> Option<Holder> {
    let mut tokens = text.split_whitespace();
    let pid = tokens.next()?.parse().ok()?;
    Some(Holder {
        pid,
        yielding: tokens.next() == Some("yield"),
    })
}

/// Try to take the lock file `name` in `directory`, creating the directory
/// when needed.
pub fn try_acquire_file(directory: &Path, name: &str) -> LockOutcome {
    try_acquire_as(directory, name, false)
}

fn try_acquire_as(directory: &Path, name: &str, yielding: bool) -> LockOutcome {
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
            let _ = if yielding {
                write!(file, "{} yield", std::process::id())
            } else {
                write!(file, "{}", std::process::id())
            };
            LockOutcome::Held(WorkspaceLock { file })
        }
        Err(TryLockError::WouldBlock) => LockOutcome::Busy(
            std::fs::read_to_string(&path)
                .ok()
                .and_then(|s| parse_holder(&s))
                .map(|holder| holder.pid),
        ),
        Err(TryLockError::Error(e)) => LockOutcome::Unavailable(e.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_first_token_is_the_pid_and_a_second_marks_a_yield() {
        assert_eq!(
            parse_holder("42"),
            Some(Holder {
                pid: 42,
                yielding: false
            })
        );
        assert_eq!(
            parse_holder("42 yield\n"),
            Some(Holder {
                pid: 42,
                yielding: true
            })
        );
        assert_eq!(parse_holder("42 other").map(|h| h.yielding), Some(false));
        assert_eq!(parse_holder(""), None);
        assert_eq!(parse_holder("yield"), None);
    }
}
