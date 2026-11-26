//! The daemon's stderr, made safe to write to after its first client has gone.
//!
//! A daemon outlives the client that started it, and its stderr is that client's
//! (often a pipe to an editor). When the reader of that pipe closes, a write fails
//! with `EPIPE`, and `eprintln!` panics on a failed write: a log line in a shared
//! path would then end the session of every other client. [`StderrGuard::install`]
//! points the process's stderr at a pipe that the daemon reads itself, and a thread
//! copies what it reads to the original stderr. When a copy fails, the thread drops
//! the rest and goes on reading, so no write to stderr in the process can fail.

use std::fs::File;
use std::io::{self, Read, Write};
use std::os::fd::{AsFd, OwnedFd};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::time::Duration;

/// While this value lives, the process's stderr is a pipe that the daemon reads.
/// Dropping it gives the process its original stderr back (or `/dev/null` when the
/// original has failed) and waits a short time for the text still in the pipe.
pub struct StderrGuard {
    original: OwnedFd,
    broken: Arc<AtomicBool>,
    done: mpsc::Receiver<()>,
}

/// How long [`StderrGuard`]'s drop waits for the copy of the last lines.
const DRAIN: Duration = Duration::from_secs(1);

impl StderrGuard {
    /// Point stderr at the daemon's own pipe. On an error, stderr is not changed.
    pub fn install() -> io::Result<StderrGuard> {
        let original = io::stderr().as_fd().try_clone_to_owned()?;
        let target = File::from(original.try_clone()?);
        let (read, write) = rustix::pipe::pipe_with(rustix::pipe::PipeFlags::CLOEXEC)?;
        let broken = Arc::new(AtomicBool::new(false));
        let (done_tx, done) = mpsc::channel();
        let copy_broken = Arc::clone(&broken);
        std::thread::Builder::new()
            .name("oxabl-stderr".to_string())
            .spawn(move || {
                copy(File::from(read), target, &copy_broken);
                let _ = done_tx.send(());
            })?;
        // Only now: a failed spawn leaves stderr as it was.
        rustix::stdio::dup2_stderr(&write)?;
        drop(write);
        Ok(StderrGuard {
            original,
            broken,
            done,
        })
    }
}

/// Copy `from` to `to` until `from` ends. After the first failed write, only read.
fn copy(mut from: File, mut to: File, broken: &AtomicBool) {
    let mut chunk = [0u8; 4096];
    loop {
        let read = match from.read(&mut chunk) {
            Ok(0) => return,
            Ok(read) => read,
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(_) => return,
        };
        if !broken.load(Ordering::Relaxed) && to.write_all(&chunk[..read]).is_err() {
            broken.store(true, Ordering::Relaxed);
        }
    }
}

impl Drop for StderrGuard {
    fn drop(&mut self) {
        // Replacing descriptor 2 closes the pipe's last write end, so the copy ends.
        let restored = if self.broken.load(Ordering::Relaxed) {
            File::open("/dev/null")
                .and_then(|null| rustix::stdio::dup2_stderr(&null).map_err(io::Error::from))
        } else {
            rustix::stdio::dup2_stderr(&self.original).map_err(io::Error::from)
        };
        if restored.is_ok() {
            let _ = self.done.recv_timeout(DRAIN);
        }
    }
}
