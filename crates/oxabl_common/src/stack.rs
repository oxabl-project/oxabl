//! The stack of every thread that parses or analyses ABL.
//!
//! The parser and the passes over its tree recurse once per nesting level, and
//! the parser stops at a fixed depth (`oxabl_parser::Parser::MAX_NESTING`). That
//! bounds the stack a file can need, but only on a stack of a known size: a
//! spawned thread gets 2 MiB by default and the main thread whatever `ulimit -s`
//! says, while the deepest tree the parser accepts needs about 3 MiB in a release
//! build and about 30 MiB in a debug build. A stack overflow aborts the process;
//! `catch_panic` cannot stop it.
//!
//! Spawn such threads with [`builder`] (or run on one with [`run`]). Only the
//! pages a thread touches take memory, so the size is address space, not RSS.

use std::thread;

/// Stack size of an analysis thread.
pub const SIZE: usize = 64 << 20;

/// A thread builder with an analysis stack of [`SIZE`].
pub fn builder() -> thread::Builder {
    thread::Builder::new().stack_size(SIZE)
}

/// Run `work` on a thread with an analysis stack and return its result. A panic
/// in `work` resumes on the caller.
///
/// # Panics
///
/// When the thread cannot be created, as `std::thread::spawn` does.
pub fn run<T: Send>(work: impl FnOnce() -> T + Send) -> T {
    thread::scope(|scope| {
        builder()
            .spawn_scoped(scope, work)
            .expect("failed to spawn an analysis thread")
            .join()
            .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
    })
}
