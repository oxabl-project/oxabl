//! The stage directory of one worker process.

use std::path::{Path, PathBuf};

/// The stage as a worker names it. A relative path starts with `./`: the
/// compiler removes a leading `.` from a relative XREF file name, so a state
/// directory such as `.oxabl/build` would lose it.
fn worker_path(relative: &Path, build: &str, name: &str) -> String {
    let prefix = if relative.is_absolute() { "" } else { "./" };
    format!(
        "{prefix}{}/stage/{build}/{name}",
        relative.to_string_lossy()
    )
}

/// The directories of a stage that a request writes into.
const KINDS: [&str; 4] = ["r", "xref", "dbg", "lst"];

/// The stage of one worker process: `<state>/stage/<build>/w<n>-<s>`.
#[derive(Clone, Debug)]
pub struct Stage {
    host: PathBuf,
    /// The same directory as the worker sees it, relative to its root.
    worker: String,
}

impl Stage {
    /// `state` is the state directory relative to the root, or absolute.
    pub fn new(state_dir: &Path, root: &Path, build: &str, worker: usize, start: usize) -> Stage {
        let relative = state_dir.strip_prefix(root).unwrap_or(state_dir);
        let name = format!("w{worker}-{start}");
        Stage {
            host: state_dir.join("stage").join(build).join(&name),
            worker: worker_path(relative, build, &name),
        }
    }

    /// The directory on the host.
    pub fn host(&self) -> &Path {
        &self.host
    }

    /// The directory as the worker sees it.
    pub fn worker_path(&self) -> &str {
        &self.worker
    }

    /// Removes what a request left, so the next one starts empty.
    pub fn clear(&self) {
        for kind in KINDS {
            let _ = std::fs::remove_dir_all(self.host.join(kind));
        }
    }
}

/// Removes every stage of every build below `<state>/stage`: the caller holds
/// the workspace lock, so none of them is in use.
pub fn remove_stale(state_dir: &Path) {
    let _ = std::fs::remove_dir_all(state_dir.join("stage"));
}
