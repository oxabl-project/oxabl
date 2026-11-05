//! [`Observations`]: what an analysis read, found absent and listed.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::file_system::FileSystem;

/// The file system inputs of one analysis.
///
/// A path is named as the analysis spelled it; the paths are not normalized.
/// Each set is sorted.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Observations {
    read: BTreeSet<PathBuf>,
    absent: BTreeSet<PathBuf>,
    listed: BTreeSet<PathBuf>,
}

impl Observations {
    /// The files whose bytes, or whose existence as a file, the analysis used.
    pub fn read(&self) -> &BTreeSet<PathBuf> {
        &self.read
    }

    /// The paths the analysis looked for and did not find.
    pub fn absent(&self) -> &BTreeSet<PathBuf> {
        &self.absent
    }

    /// The directories a case-insensitive lookup listed.
    pub fn listed(&self) -> &BTreeSet<PathBuf> {
        &self.listed
    }

    pub fn is_empty(&self) -> bool {
        self.read.is_empty() && self.absent.is_empty() && self.listed.is_empty()
    }

    /// Add everything `other` holds.
    pub fn merge(&mut self, other: &Observations) {
        self.read.extend(other.read.iter().cloned());
        self.absent.extend(other.absent.iter().cloned());
        self.listed.extend(other.listed.iter().cloned());
    }

    pub(crate) fn add_read(&mut self, path: &Path) {
        insert(&mut self.read, path);
    }

    pub(crate) fn add_absent(&mut self, path: &Path) {
        insert(&mut self.absent, path);
    }

    pub(crate) fn add_listed(&mut self, path: &Path) {
        insert(&mut self.listed, path);
    }
}

fn insert(set: &mut BTreeSet<PathBuf>, path: &Path) {
    if !set.contains(path) {
        set.insert(path.to_path_buf());
    }
}

/// A value computed once and shared, with the observations made while it was
/// computed.
///
/// A memo keeps one for each entry. Every use of the entry, the first
/// included, calls [`attributed`](Self::attributed), so each analysis that
/// uses the entry observes what the entry observed, though the entry read
/// its files once.
///
/// Over a filesystem that does not observe it holds no observations and
/// costs nothing but its size.
#[derive(Debug, Clone)]
pub struct Observed<T> {
    value: T,
    observed: Option<Arc<Observations>>,
}

impl<T> Observed<T> {
    /// Run `compute` and keep what it observed through `fs`.
    pub fn capture(fs: &dyn FileSystem, compute: impl FnOnce() -> T) -> Self {
        let mut compute = Some(compute);
        let mut value = None;
        let observed = fs.observe(&mut || value = compute.take().map(|compute| compute()));
        Observed {
            value: value.expect("observe runs the computation exactly once"),
            observed,
        }
    }

    /// The value, with what it observed added to the analysis that runs on
    /// this thread through `fs`.
    pub fn attributed(&self, fs: &dyn FileSystem) -> &T {
        if let Some(observed) = &self.observed {
            fs.attribute(observed);
        }
        &self.value
    }

    /// The value, without what it observed.
    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn into_value(self) -> T {
        self.value
    }

    /// The value and what it observed.
    pub fn into_parts(self) -> (T, Option<Arc<Observations>>) {
        (self.value, self.observed)
    }
}

/// Run `compute` and drop what it observed through `fs`, for a computation no
/// single analysis owns, such as a scan of the whole workspace.
pub fn unobserved<T>(fs: &dyn FileSystem, compute: impl FnOnce() -> T) -> T {
    Observed::capture(fs, compute).into_value()
}
