//! Cancellation shared by the CLI's signal handler and the pool.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Instant;

use super::kill;

#[derive(Default)]
struct Inner {
    cancelled: AtomicBool,
    at: OnceLock<Instant>,
    groups: Mutex<Vec<u32>>,
}

impl std::fmt::Debug for Cancel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Cancel({})", self.is_cancelled())
    }
}

/// A cancel flag plus the process groups of every live worker.
#[derive(Clone, Default)]
pub struct Cancel(Arc<Inner>);

impl Cancel {
    pub fn new() -> Cancel {
        Cancel::default()
    }

    /// Marks the build cancelled. Pool code stops waiting and stops workers.
    pub fn cancel(&self) {
        let _ = self.0.at.set(Instant::now());
        self.0.cancelled.store(true, Ordering::SeqCst);
    }

    /// When the first cancel arrived.
    pub fn cancelled_at(&self) -> Option<Instant> {
        self.0.at.get().copied()
    }

    pub fn is_cancelled(&self) -> bool {
        self.0.cancelled.load(Ordering::SeqCst)
    }

    pub(crate) fn register(&self, pgid: u32) {
        self.0.groups.lock().unwrap().push(pgid);
    }

    pub(crate) fn unregister(&self, pgid: u32) {
        self.0.groups.lock().unwrap().retain(|group| *group != pgid);
    }

    /// Kills every live worker at once, for a second signal.
    pub fn kill_all_now(&self) {
        self.cancel();
        for group in self.0.groups.lock().unwrap().iter() {
            kill::signal_group(*group, kill::Signal::Kill);
        }
    }
}
