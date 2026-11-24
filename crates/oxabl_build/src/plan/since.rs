use std::path::{Path, PathBuf};
use std::process::Command;

use crate::error::BuildError;

fn git(root: &Path, arguments: &[&str]) -> Result<Vec<u8>, BuildError> {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(arguments)
        .output()
        .map_err(|e| BuildError::Usage(format!("--since needs the git binary: {e}")))?;
    if output.status.success() {
        return Ok(output.stdout);
    }
    let message = String::from_utf8_lossy(&output.stderr);
    Err(BuildError::Usage(format!(
        "git {} failed: {}",
        arguments.first().copied().unwrap_or_default(),
        message.trim()
    )))
}

fn paths(root: &Path, listing: &[u8]) -> Vec<PathBuf> {
    listing
        .split(|byte| *byte == 0)
        .filter(|name| !name.is_empty())
        .map(|name| root.join(String::from_utf8_lossy(name).as_ref()))
        .collect()
}

/// The files that differ between `rev` and the working tree under `root`
/// (committed, staged and unstaged edits, deletions, both sides of a rename),
/// and the untracked files that git does not ignore, as absolute paths.
pub fn changed_since(root: &Path, rev: &str) -> Result<Vec<PathBuf>, BuildError> {
    if rev.is_empty() || rev.starts_with('-') {
        return Err(BuildError::Usage(format!("{rev:?} is not a git revision")));
    }
    let inside = git(root, &["rev-parse", "--is-inside-work-tree"]).map_err(|_| {
        BuildError::Usage(format!(
            "--since needs a git work tree; {} is not in one",
            root.display()
        ))
    })?;
    if inside.trim_ascii() != b"true" {
        return Err(BuildError::Usage(format!(
            "--since needs a git work tree; {} is not in one",
            root.display()
        )));
    }
    let commit = format!("{rev}^{{commit}}");
    git(root, &["rev-parse", "--verify", "--quiet", &commit, "--"])
        .map_err(|_| BuildError::Usage(format!("{rev:?} is not a git revision")))?;
    let mut files = paths(
        root,
        &git(
            root,
            &[
                "diff",
                "--name-only",
                "--no-renames",
                "--relative",
                "-z",
                rev,
                "--",
            ],
        )?,
    );
    files.extend(paths(
        root,
        &git(root, &["ls-files", "--others", "--exclude-standard", "-z"])?,
    ));
    files.sort();
    files.dedup();
    Ok(files)
}
