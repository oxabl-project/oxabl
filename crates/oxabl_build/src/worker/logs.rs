//! Worker logs: `<state>/logs/<build id>/worker-<n>.log`.

use std::fs::{self, File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};

/// Builds whose logs stay on disk.
const KEPT_BUILDS: usize = 5;

/// The log directory of one build.
#[derive(Clone, Debug)]
pub struct Logs {
    directory: PathBuf,
}

impl Logs {
    /// Creates the directory and removes the logs of all but the newest
    /// builds. Build ids sort by time.
    pub fn create(state_dir: &Path, build_id: &str) -> io::Result<Logs> {
        let parent = state_dir.join("logs");
        let directory = parent.join(build_id);
        fs::create_dir_all(&directory)?;
        let mut builds: Vec<String> = fs::read_dir(&parent)?
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().is_dir())
            .filter_map(|entry| entry.file_name().into_string().ok())
            .collect();
        builds.sort();
        let excess = builds.len().saturating_sub(KEPT_BUILDS);
        for old in builds.iter().take(excess).filter(|name| *name != build_id) {
            let _ = fs::remove_dir_all(parent.join(old));
        }
        Ok(Logs { directory })
    }

    pub fn path(&self, worker: usize) -> PathBuf {
        self.directory.join(format!("worker-{worker}.log"))
    }

    /// Opens the log of `worker` for appending.
    pub fn open(&self, worker: usize) -> io::Result<File> {
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(self.path(worker))
    }
}

/// The last `lines` lines of a log; empty when it cannot be read.
pub fn tail(path: &Path, lines: usize) -> String {
    let text = fs::read(path)
        .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
        .unwrap_or_default();
    let all: Vec<&str> = text.lines().collect();
    all[all.len().saturating_sub(lines)..].join("\n")
}
