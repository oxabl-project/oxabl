//! Faults a test switches on through environment variables.

use std::path::PathBuf;
use std::time::Duration;

fn flag(name: &str) -> bool {
    std::env::var_os(name).is_some_and(|value| value == "1")
}

pub struct Faults {
    pub noise: bool,
    pub start_delay: Option<Duration>,
    pub bad_token: bool,
    pub exit_in_handshake: bool,
    pub exit_in_handshake_once: Option<PathBuf>,
    pub schema_fault: Option<String>,
    /// The share whose tables the worker leaves out (`FAKE_AVM_SKIP_SHARE`).
    pub skip_share: Option<usize>,
    /// The share that reports one table too many (`FAKE_AVM_COUNT_SKEW`).
    pub count_skew: Option<usize>,
    pub log: Option<PathBuf>,
    /// A directory a test gives for markers that outlive one process.
    pub state: Option<PathBuf>,
    pub dlc: Option<PathBuf>,
}

impl Faults {
    pub fn from_env() -> Faults {
        let state = std::env::var_os("FAKE_AVM_STATE").map(PathBuf::from);
        Faults {
            state: state.clone(),
            noise: flag("FAKE_AVM_NOISE"),
            start_delay: std::env::var("FAKE_AVM_START_DELAY")
                .ok()
                .and_then(|ms| ms.parse().ok())
                .map(Duration::from_millis),
            bad_token: flag("FAKE_AVM_BAD_TOKEN"),
            exit_in_handshake: flag("FAKE_AVM_EXIT_IN_HANDSHAKE"),
            exit_in_handshake_once: flag("FAKE_AVM_EXIT_IN_HANDSHAKE_ONCE")
                .then(|| state.unwrap_or_default().join("exit-in-handshake-once")),
            schema_fault: std::env::var("FAKE_AVM_SCHEMA_FAULT").ok(),
            count_skew: std::env::var("FAKE_AVM_COUNT_SKEW")
                .ok()
                .and_then(|n| n.parse().ok()),
            skip_share: std::env::var("FAKE_AVM_SKIP_SHARE")
                .ok()
                .and_then(|n| n.parse().ok()),
            log: std::env::var_os("FAKE_AVM_LOG").map(PathBuf::from),
            dlc: std::env::var_os("FAKE_AVM_DLC").map(PathBuf::from),
        }
    }
}
