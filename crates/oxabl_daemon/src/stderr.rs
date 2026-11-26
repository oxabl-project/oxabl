//! The daemon's stderr after the reader of it has gone.
//!
//! A daemon outlives the client that started it, and its stderr is that client's
//! (often a pipe to an editor). When the reader of that pipe closes, a write fails
//! with `EPIPE`, and `eprintln!` panics on a failed write: a log line in a shared
//! path would then end the session of every other client. [`watch`] starts a thread
//! that waits until the reader has gone (`POLLERR` or `POLLHUP` on stderr) and then
//! points stderr at `/dev/null`, so later writes cannot fail.
//!
//! Until then, stderr stays the original descriptor and every write is synchronous,
//! so the last message of a daemon that aborts (a stack overflow, a failed
//! allocation) reaches the reader. Two limits remain:
//!
//! * A write in the short time between the reader's close and the watcher's switch
//!   can still fail. The daemon's own log lines ignore a failed write for that reason.
//! * A reader that is alive and does not read blocks a write when the pipe is full,
//!   as it does for any process.

use std::io;
use std::os::fd::{AsFd, OwnedFd};

/// Start the watcher thread of stderr. An error means stderr is not watched.
pub fn watch() -> io::Result<()> {
    let original = io::stderr().as_fd().try_clone_to_owned()?;
    std::thread::Builder::new()
        .name("oxabl-stderr".to_string())
        .spawn(move || {
            if reader_gone(&original) {
                // `/dev/null` is opened only now, so the watcher holds no other
                // descriptor while it waits.
                if let Ok(null) = std::fs::File::open("/dev/null") {
                    let _ = rustix::stdio::dup2_stderr(&null);
                }
            }
        })?;
    Ok(())
}

/// Wait until `fd` reports that its reader has gone. `false` when the wait fails or
/// the descriptor is not valid.
fn reader_gone(fd: &OwnedFd) -> bool {
    use rustix::event::{PollFd, PollFlags, poll};
    loop {
        // No events are asked for: `POLLERR`, `POLLHUP` and `POLLNVAL` are always
        // reported, and a regular file or `/dev/null` reports none of them.
        let mut fds = [PollFd::new(fd, PollFlags::empty())];
        match poll(&mut fds, None) {
            Ok(_) => {
                let revents = fds[0].revents();
                if revents.contains(PollFlags::NVAL) {
                    return false;
                }
                if revents.intersects(PollFlags::ERR | PollFlags::HUP) {
                    return true;
                }
            }
            Err(rustix::io::Errno::INTR) => {}
            Err(_) => return false,
        }
    }
}
