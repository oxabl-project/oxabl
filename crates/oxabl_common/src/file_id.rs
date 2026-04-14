use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Lightweight, copyable identifier for a source file.
///
/// Backed by a `u32` index into a [`FileSet`]. Use [`FileId::UNKNOWN`] as a
/// sentinel for synthetic or in-memory files that have no disk path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl FileId {
    /// Sentinel value for synthetic / in-memory files with no real path.
    pub const UNKNOWN: FileId = FileId(0);

    /// Create a `FileId` from a raw index. Intended for use by [`FileSet`].
    #[inline]
    pub const fn new(id: u32) -> Self {
        FileId(id)
    }

    /// Return the raw `u32` backing this id.
    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// Bidirectional mapping between [`FileId`] values and filesystem paths.
///
/// Built by the workspace layer and threaded through the system so that every
/// component can resolve a `FileId` back to a human-readable path (for
/// diagnostics, LSP URIs, etc.).
#[derive(Debug, Default)]
pub struct FileSet {
    id_to_path: Vec<PathBuf>,
    path_to_id: HashMap<PathBuf, FileId>,
}

impl FileSet {
    /// Create an empty `FileSet`. The first slot (index 0) is reserved for
    /// [`FileId::UNKNOWN`] and maps to an empty path.
    pub fn new() -> Self {
        let mut set = FileSet {
            id_to_path: Vec::new(),
            path_to_id: HashMap::new(),
        };
        // Reserve index 0 for UNKNOWN
        set.id_to_path.push(PathBuf::new());
        set
    }

    /// Insert a path and return its [`FileId`].
    ///
    /// If the path was already inserted, the existing `FileId` is returned.
    pub fn insert(&mut self, path: PathBuf) -> FileId {
        if let Some(&id) = self.path_to_id.get(&path) {
            return id;
        }
        let id = FileId::new(self.id_to_path.len() as u32);
        self.id_to_path.push(path.clone());
        self.path_to_id.insert(path, id);
        id
    }

    /// Look up the path for a given [`FileId`].
    pub fn path(&self, id: FileId) -> Option<&Path> {
        self.id_to_path.get(id.0 as usize).map(|p| p.as_path())
    }

    /// Look up the [`FileId`] for a given path.
    pub fn id(&self, path: &Path) -> Option<FileId> {
        self.path_to_id.get(path).copied()
    }

    /// Return the number of files (excluding the UNKNOWN sentinel).
    pub fn len(&self) -> usize {
        self.id_to_path.len() - 1
    }

    /// Return true if no files have been inserted.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unknown_sentinel() {
        let set = FileSet::new();
        assert_eq!(FileId::UNKNOWN.raw(), 0);
        assert_eq!(set.path(FileId::UNKNOWN), Some(Path::new("")));
    }

    #[test]
    fn insert_and_lookup() {
        let mut set = FileSet::new();
        let id = set.insert(PathBuf::from("src/main.p"));

        assert_eq!(id.raw(), 1);
        assert_eq!(set.path(id), Some(Path::new("src/main.p")));
        assert_eq!(set.id(Path::new("src/main.p")), Some(id));
    }

    #[test]
    fn duplicate_insert_returns_same_id() {
        let mut set = FileSet::new();
        let id1 = set.insert(PathBuf::from("src/main.p"));
        let id2 = set.insert(PathBuf::from("src/main.p"));

        assert_eq!(id1, id2);
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn multiple_files() {
        let mut set = FileSet::new();
        let a = set.insert(PathBuf::from("a.p"));
        let b = set.insert(PathBuf::from("b.p"));
        let c = set.insert(PathBuf::from("c.p"));

        assert_ne!(a, b);
        assert_ne!(b, c);
        assert_eq!(set.len(), 3);
        assert_eq!(set.path(a), Some(Path::new("a.p")));
        assert_eq!(set.path(b), Some(Path::new("b.p")));
        assert_eq!(set.path(c), Some(Path::new("c.p")));
    }

    #[test]
    fn unknown_id_lookup() {
        let set = FileSet::new();
        // Looking up a non-existent FileId returns None
        assert_eq!(set.path(FileId::new(999)), None);
    }

    #[test]
    fn is_empty() {
        let mut set = FileSet::new();
        assert!(set.is_empty());

        set.insert(PathBuf::from("a.p"));
        assert!(!set.is_empty());
    }
}
