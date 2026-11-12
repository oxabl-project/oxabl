//! When the automatic check runs: at most one success a day, and after a
//! failure a back-off that starts at 15 minutes and doubles up to a day.

use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

const FILE_NAME: &str = "update-state.json";
const DAY: u64 = 24 * 60 * 60;
const FIRST_BACKOFF: u64 = 15 * 60;

#[derive(Serialize, Deserialize, Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct State {
    /// Failures in a row since the last success.
    pub failures: u32,
    /// Unix seconds from which a check is due; zero means now.
    pub next_due: u64,
}

pub fn now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_secs())
}

impl State {
    pub fn due(&self, now: u64) -> bool {
        now >= self.next_due
    }

    pub fn succeeded(now: u64) -> State {
        State {
            failures: 0,
            next_due: now + DAY,
        }
    }

    pub fn failed(self, now: u64) -> State {
        let failures = self.failures.saturating_add(1);
        let delay = FIRST_BACKOFF
            .saturating_mul(1u64 << (failures - 1).min(7))
            .min(DAY);
        State {
            failures,
            next_due: now + delay,
        }
    }
}

fn file(directory: &Path) -> PathBuf {
    directory.join(FILE_NAME)
}

/// The state; an absent or unreadable file means a check is due now.
pub fn read(directory: &Path) -> State {
    std::fs::read(file(directory))
        .ok()
        .and_then(|bytes| serde_json::from_slice(&bytes).ok())
        .unwrap_or_default()
}

pub fn write(directory: &Path, state: State) {
    let _ = std::fs::create_dir_all(directory);
    let temporary = directory.join(format!(".{FILE_NAME}.{}", std::process::id()));
    let text = serde_json::to_string(&state).expect("a state is JSON");
    if std::fs::write(&temporary, text).is_ok() {
        let _ = std::fs::rename(&temporary, file(directory));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_failure_backs_off_from_fifteen_minutes_and_doubles_to_a_day() {
        let mut state = State::default();
        let mut delays = Vec::new();
        for _ in 0..9 {
            state = state.failed(1_000);
            delays.push(state.next_due - 1_000);
        }
        assert_eq!(delays[..3], [900, 1_800, 3_600]);
        assert_eq!(delays[7..], [DAY, DAY]);
        assert!(delays.windows(2).all(|pair| pair[0] <= pair[1]));
    }

    #[test]
    fn a_success_waits_a_day_and_clears_the_failures() {
        let state = State::succeeded(1_000);
        assert_eq!(state.failures, 0);
        assert!(!state.due(1_000 + DAY - 1));
        assert!(state.due(1_000 + DAY));
        assert!(State::default().due(0));
    }
}
