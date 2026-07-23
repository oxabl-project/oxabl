//! Per-URI debounce timers (R13).
//!
//! A burst of `didChange` notifications collapses to a single diagnostics
//! computation ~150–300ms after the last edit. Each edit (re)schedules the
//! buffer's deadline; the main loop asks for the nearest deadline to size its
//! `select!` timeout and, when a deadline elapses, drains the due URIs.

use std::collections::HashMap;
use std::time::{Duration, Instant};

use lsp_types::Uri;

/// Default debounce window. Within the 150–300ms band the origin specifies;
/// a burst of edits inside this window yields exactly one computation.
pub const DEFAULT_WINDOW: Duration = Duration::from_millis(200);

/// Tracks the pending recompute deadline for each edited buffer.
pub struct Debouncer {
    window: Duration,
    deadlines: HashMap<Uri, Instant>,
}

impl Debouncer {
    pub fn new(window: Duration) -> Self {
        Debouncer {
            window,
            deadlines: HashMap::new(),
        }
    }

    /// (Re)schedule `uri`'s recompute to `now + window`, collapsing any earlier
    /// pending deadline for the same buffer.
    pub fn schedule(&mut self, uri: Uri, now: Instant) {
        self.deadlines.insert(uri, now + self.window);
    }

    /// Cancel any pending recompute for `uri` (e.g. on close).
    pub fn cancel(&mut self, uri: &Uri) {
        self.deadlines.remove(uri);
    }

    /// The earliest pending deadline, if any — used to size the loop timeout.
    pub fn next_deadline(&self) -> Option<Instant> {
        self.deadlines.values().min().copied()
    }

    /// Remove and return every URI whose deadline is at or before `now`.
    pub fn take_due(&mut self, now: Instant) -> Vec<Uri> {
        let due: Vec<Uri> = self
            .deadlines
            .iter()
            .filter(|(_, deadline)| **deadline <= now)
            .map(|(uri, _)| uri.clone())
            .collect();
        for uri in &due {
            self.deadlines.remove(uri);
        }
        due
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    fn uri(s: &str) -> Uri {
        Uri::from_str(s).unwrap()
    }

    #[test]
    fn reschedule_collapses_to_latest_deadline() {
        let mut d = Debouncer::new(Duration::from_millis(100));
        let t0 = Instant::now();
        let u = uri("file:///a.p");
        d.schedule(u.clone(), t0);
        // A later edit pushes the deadline out; only the latest survives.
        d.schedule(u.clone(), t0 + Duration::from_millis(50));
        assert_eq!(d.next_deadline(), Some(t0 + Duration::from_millis(150)));
        // Nothing is due before the (rescheduled) deadline.
        assert!(d.take_due(t0 + Duration::from_millis(120)).is_empty());
        // Due after it.
        let due = d.take_due(t0 + Duration::from_millis(200));
        assert_eq!(due, vec![u]);
        assert!(d.next_deadline().is_none());
    }

    #[test]
    fn cancel_removes_pending() {
        let mut d = Debouncer::new(Duration::from_millis(100));
        let u = uri("file:///a.p");
        d.schedule(u.clone(), Instant::now());
        d.cancel(&u);
        assert!(d.next_deadline().is_none());
    }
}
