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
    /// Compiles after which the schema image differs (`FAKE_AVM_SCHEMA_CHANGE_AFTER`).
    pub schema_change_after: Option<usize>,
    /// Compiles after which a schema answer has a wrong count
    /// (`FAKE_AVM_SCHEMA_ERROR_AFTER`).
    pub schema_error_after: Option<usize>,
    /// The private `XREF-XML` is not written (`FAKE_AVM_NO_PRIVATE_XREF`).
    pub no_private_xref: bool,
    /// While this file exists the schema image differs (`FAKE_AVM_SCHEMA_CHANGE_FILE`).
    pub schema_change_file: Option<PathBuf>,
    /// Time a worker waits after it read the schema and before it answers
    /// (`FAKE_AVM_SCHEMA_DELAY_MS`).
    pub schema_delay: Option<Duration>,
    /// Time a worker waits before it handles each request, as a slow
    /// transport adds to every round trip (`FAKE_AVM_REQUEST_DELAY_MS`).
    pub request_delay: Option<Duration>,
    /// Receives one line with the time for each request (`FAKE_AVM_REQUEST_LOG`).
    pub request_log: Option<PathBuf>,
    /// Paths whose presence each `COMPILE` request logs (`FAKE_AVM_WATCH`, `:`-separated).
    pub watch: Vec<PathBuf>,
    /// Receives `<unit> <XREF mode>` for each compiled unit (`FAKE_AVM_XREF_LOG`).
    pub xref_log: Option<PathBuf>,
    /// Time each compile takes (`FAKE_AVM_COMPILE_MS`).
    pub compile_time: Option<Duration>,
    /// Text that changes the raw digest and not the image (`FAKE_AVM_RAW_SALT`).
    pub raw_salt: Option<String>,
    /// While this file exists the raw digest differs (`FAKE_AVM_RAW_CHANGE_FILE`).
    pub raw_change_file: Option<PathBuf>,
    /// From this raw request of the process on, the raw digest differs
    /// (`FAKE_AVM_RAW_CHANGE_FROM`).
    pub raw_change_from: Option<usize>,
    /// A broken raw digest answer (`FAKE_AVM_RAW_FAULT`).
    pub raw_fault: Option<String>,
    /// The share that `raw_fault` breaks (`FAKE_AVM_RAW_FAULT_SHARE`).
    pub raw_fault_share: Option<usize>,
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
            schema_change_after: std::env::var("FAKE_AVM_SCHEMA_CHANGE_AFTER")
                .ok()
                .and_then(|n| n.parse().ok()),
            schema_error_after: std::env::var("FAKE_AVM_SCHEMA_ERROR_AFTER")
                .ok()
                .and_then(|n| n.parse().ok()),
            no_private_xref: flag("FAKE_AVM_NO_PRIVATE_XREF"),
            schema_change_file: std::env::var_os("FAKE_AVM_SCHEMA_CHANGE_FILE").map(PathBuf::from),
            schema_delay: std::env::var("FAKE_AVM_SCHEMA_DELAY_MS")
                .ok()
                .and_then(|ms| ms.parse().ok())
                .map(Duration::from_millis),
            request_delay: std::env::var("FAKE_AVM_REQUEST_DELAY_MS")
                .ok()
                .and_then(|ms| ms.parse().ok())
                .map(Duration::from_millis),
            request_log: std::env::var_os("FAKE_AVM_REQUEST_LOG").map(PathBuf::from),
            watch: std::env::var_os("FAKE_AVM_WATCH")
                .map(|paths| std::env::split_paths(&paths).collect())
                .unwrap_or_default(),
            xref_log: std::env::var_os("FAKE_AVM_XREF_LOG").map(PathBuf::from),
            compile_time: std::env::var("FAKE_AVM_COMPILE_MS")
                .ok()
                .and_then(|ms| ms.parse().ok())
                .map(Duration::from_millis),
            raw_salt: std::env::var("FAKE_AVM_RAW_SALT").ok(),
            raw_change_file: std::env::var_os("FAKE_AVM_RAW_CHANGE_FILE").map(PathBuf::from),
            raw_change_from: std::env::var("FAKE_AVM_RAW_CHANGE_FROM")
                .ok()
                .and_then(|n| n.parse().ok()),
            raw_fault: std::env::var("FAKE_AVM_RAW_FAULT").ok(),
            raw_fault_share: std::env::var("FAKE_AVM_RAW_FAULT_SHARE")
                .ok()
                .and_then(|n| n.parse().ok()),
        }
    }
}
