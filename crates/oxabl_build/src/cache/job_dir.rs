//! The per-job local cache directory of the `ci` and `read-only` modes.

use std::path::{Component, Path, PathBuf};

use crate::error::BuildError;

/// The marker of a directory oxabl uses as a job cache.
pub const MARKER: &str = ".oxabl-cache-job";

fn usage(directory: &Path, why: &str) -> BuildError {
    BuildError::Usage(format!(
        "[build.cache].local_job_dir = \"{}\" {why}",
        directory.display()
    ))
}

/// `path` made absolute and lexically normalised, with the real path of its
/// longest existing prefix, so that a link cannot hide the workspace root.
fn resolved(path: &Path) -> PathBuf {
    let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    let mut clean = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::ParentDir => {
                clean.pop();
            }
            Component::CurDir => {}
            other => clean.push(other.as_os_str()),
        }
    }
    let mut existing = clean.clone();
    let mut tail = Vec::new();
    while !existing.exists() {
        match existing.file_name().map(|name| name.to_os_string()) {
            Some(name) => tail.push(name),
            None => break,
        }
        if !existing.pop() {
            break;
        }
    }
    let mut real = std::fs::canonicalize(&existing).unwrap_or(existing);
    real.extend(tail.iter().rev());
    real
}

/// Checks `directory` and makes it ready: it is not the workspace root, an
/// ancestor of it, `$HOME` or `/`; it is absent or empty, or it holds the
/// marker. Deletes `v1/` beneath it, nothing else. Returns its absolute path.
pub fn prepare(directory: &Path, root: &Path, home: Option<&Path>) -> Result<PathBuf, BuildError> {
    let job = resolved(directory);
    let root = resolved(root);
    if job.parent().is_none() {
        return Err(usage(directory, "is the file system root"));
    }
    if root.starts_with(&job) {
        return Err(usage(
            directory,
            "is the workspace root or one of its ancestors",
        ));
    }
    if home.is_some_and(|home| resolved(home) == job) {
        return Err(usage(directory, "is the home directory"));
    }
    match std::fs::read_dir(&job) {
        Ok(mut entries) => {
            if !job.join(MARKER).is_file() && entries.next().is_some() {
                return Err(usage(
                    directory,
                    "holds files and is not a job cache of oxabl; empty it or choose another directory",
                ));
            }
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => {
            return Err(BuildError::Infrastructure(format!(
                "cannot read {}: {error}",
                job.display()
            )));
        }
    }
    std::fs::create_dir_all(&job).map_err(|error| {
        BuildError::Infrastructure(format!("cannot create {}: {error}", job.display()))
    })?;
    std::fs::write(job.join(MARKER), b"").map_err(|error| {
        BuildError::Infrastructure(format!(
            "cannot write the marker in {}: {error}",
            job.display()
        ))
    })?;
    let _ = std::fs::remove_dir_all(job.join("v1"));
    Ok(job)
}
