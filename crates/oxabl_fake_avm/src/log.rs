//! The append-only log a test reads to see which workers ran.

use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;

/// Appends `<pid> <what>` to the log; a missing or unwritable log is ignored.
pub fn append(log: Option<&Path>, what: &str) {
    let Some(path) = log else { return };
    if let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) {
        // One write call, so lines of parallel workers never interleave.
        let _ = file.write_all(format!("{} {what}\n", std::process::id()).as_bytes());
    }
}
