use std::path::{Path, PathBuf};

use oxabl_index::search::normalize_lexically;

const DLC_PREFIX: &str = "$DLC";

/// Converts between host paths and canonical paths: UTF-8, `/`-separated and
/// without `.` or `..`, relative to the workspace root, or `$DLC/` plus a path
/// under the DLC source directory.
///
/// A path under neither has no portable form. It keeps its absolute spelling,
/// which starts with `/` and so cannot equal a canonical path, so a change to
/// it is still seen.
#[derive(Debug, Clone)]
pub struct PathMap {
    root: PathBuf,
    dlc: Option<PathBuf>,
}

impl PathMap {
    /// `root` and `dlc` must be absolute.
    pub fn new(root: &Path, dlc: Option<&Path>) -> Self {
        PathMap {
            root: normalize_lexically(root),
            dlc: dlc.map(normalize_lexically),
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn dlc(&self) -> Option<&Path> {
        self.dlc.as_deref()
    }

    /// The canonical form of the absolute host path `path`.
    pub fn canonical(&self, path: &Path) -> String {
        let path = normalize_lexically(path);
        if let Ok(below) = path.strip_prefix(&self.root) {
            return join_components(None, below);
        }
        if let Some(below) = self.dlc.as_deref().and_then(|d| path.strip_prefix(d).ok()) {
            return join_components(Some(DLC_PREFIX), below);
        }
        path.to_string_lossy().into_owned()
    }

    /// The host path of the canonical path `canonical`.
    pub fn absolute(&self, canonical: &str) -> PathBuf {
        if canonical.starts_with('/') {
            return PathBuf::from(canonical);
        }
        match canonical.strip_prefix(DLC_PREFIX) {
            Some(rest) => match &self.dlc {
                Some(dlc) => dlc.join(rest.trim_start_matches('/')),
                None => PathBuf::from(canonical),
            },
            None => self.root.join(canonical),
        }
    }
}

fn join_components(prefix: Option<&str>, below: &Path) -> String {
    let mut out = String::from(prefix.unwrap_or(""));
    for part in below.components() {
        if !out.is_empty() {
            out.push('/');
        }
        out.push_str(&part.as_os_str().to_string_lossy());
    }
    if out.is_empty() {
        out.push('.');
    }
    out
}
