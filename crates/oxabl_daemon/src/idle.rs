//! When a daemon may exit: no client is connected, no background work runs, and that
//! has lasted for the idle timeout.
//!
//! # The decision and the accept are one lock
//!
//! A client that connects while the daemon decides to exit must be served or refused,
//! never half served. So the accept loop admits a client ([`Activity::admit`]) under the
//! same lock that [`Activity::wait_until_idle`] takes to decide. Once the daemon has
//! decided, every later admission fails, and the accept loop drops the stream unread:
//! the client sees the connection close before any answer, which tells it to look for a
//! daemon again (and start one).

use std::sync::{Arc, Condvar, Mutex, MutexGuard};
use std::time::{Duration, Instant};

/// How long a daemon waits with no client before it exits, when nothing says otherwise.
pub const DEFAULT_IDLE_TIMEOUT: Duration = Duration::from_secs(10 * 60);

/// The environment variable that sets the idle timeout of a daemon.
pub const IDLE_TIMEOUT_VARIABLE: &str = "OXABL_DAEMON_IDLE_TIMEOUT";

/// Read an idle timeout: `never`, or a whole number with an optional unit `ms`, `s`,
/// `m` or `h` (seconds without one). `None` is `never`.
pub fn parse_idle_timeout(value: &str) -> Result<Option<Duration>, String> {
    let value = value.trim();
    if value.eq_ignore_ascii_case("never") {
        return Ok(None);
    }
    let split = value
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(value.len());
    let (number, unit) = value.split_at(split);
    let invalid = || {
        format!(
            "`{value}` is not an idle timeout: give `never` or a whole number with an \
             optional unit ms, s, m or h, for example `10m`"
        )
    };
    let number: u64 = number.parse().map_err(|_| invalid())?;
    let duration = match unit {
        "ms" => Duration::from_millis(number),
        "" | "s" => Duration::from_secs(number),
        "m" => Duration::from_secs(number.checked_mul(60).ok_or_else(invalid)?),
        "h" => Duration::from_secs(number.checked_mul(3600).ok_or_else(invalid)?),
        _ => return Err(invalid()),
    };
    if duration.is_zero() {
        return Err(format!(
            "`{value}` is not an idle timeout: it must be more than zero (`never` keeps \
             the daemon running)"
        ));
    }
    Ok(Some(duration))
}

/// The clients and the background work of one daemon.
#[derive(Debug)]
pub struct Activity {
    state: Mutex<State>,
    changed: Condvar,
}

#[derive(Debug)]
struct State {
    clients: usize,
    work: usize,
    /// When the last client or work ended, or when the daemon started.
    idle_since: Instant,
    /// The daemon decided to exit: no client is admitted after this.
    closing: bool,
    /// The accept loop ended for another reason, so nobody waits for idle any more.
    ended: bool,
}

impl Default for Activity {
    fn default() -> Self {
        Activity {
            state: Mutex::new(State {
                clients: 0,
                work: 0,
                idle_since: Instant::now(),
                closing: false,
                ended: false,
            }),
            changed: Condvar::new(),
        }
    }
}

/// Holds a daemon open while it lives: a connected client or a running piece of
/// background work.
#[derive(Debug)]
#[must_use = "the daemon counts as busy only while this guard lives"]
pub struct Busy {
    activity: Arc<Activity>,
    kind: Kind,
}

#[derive(Debug, Clone, Copy)]
enum Kind {
    Client,
    Work,
}

impl Activity {
    fn lock(&self) -> MutexGuard<'_, State> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Count a client the accept loop took, or `None` when the daemon has decided to
    /// exit and the client must be refused.
    pub fn admit(self: &Arc<Self>) -> Option<Busy> {
        let mut state = self.lock();
        if state.closing {
            return None;
        }
        state.clients += 1;
        Some(Busy {
            activity: Arc::clone(self),
            kind: Kind::Client,
        })
    }

    /// Count background work that must finish before the daemon may exit, such as a
    /// workspace pass that outlives the request that started it.
    pub fn work(self: &Arc<Self>) -> Busy {
        self.lock().work += 1;
        Busy {
            activity: Arc::clone(self),
            kind: Kind::Work,
        }
    }

    /// Block until nothing has been connected or running for `timeout`, then refuse
    /// every later client and return `true`. Returns `false` when [`Activity::end`]
    /// was called first.
    pub fn wait_until_idle(&self, timeout: Duration) -> bool {
        let mut state = self.lock();
        loop {
            if state.ended {
                return false;
            }
            let wait = if state.clients == 0 && state.work == 0 {
                let idle = state.idle_since.elapsed();
                if idle >= timeout {
                    state.closing = true;
                    return true;
                }
                timeout - idle
            } else {
                // Woken by the guard that brings the count to zero.
                timeout
            };
            state = self
                .changed
                .wait_timeout(state, wait)
                .unwrap_or_else(|poisoned| poisoned.into_inner())
                .0;
        }
    }

    /// The accept loop has ended: release a waiter.
    pub fn end(&self) {
        self.lock().ended = true;
        self.changed.notify_all();
    }
}

impl Drop for Busy {
    fn drop(&mut self) {
        let mut state = self.activity.lock();
        match self.kind {
            Kind::Client => state.clients -= 1,
            Kind::Work => state.work -= 1,
        }
        if state.clients == 0 && state.work == 0 {
            state.idle_since = Instant::now();
            drop(state);
            self.activity.changed.notify_all();
        }
    }
}
