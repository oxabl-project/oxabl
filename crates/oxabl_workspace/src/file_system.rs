use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Abstraction over file I/O so that every component that reads files
/// (preprocessor, workspace scanner, LSP document sync) can be tested
/// hermetically with [`InMemoryFileSystem`].
pub trait FileSystem: Send + Sync {
    /// Read the entire contents of a file.
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error>;

    /// Check whether a path exists.
    fn exists(&self, path: &Path) -> bool;

    /// Search `include_paths` in order for a file named `name`, returning the
    /// first match or `None` if not found.
    ///
    /// PROPATH resolution rules (matching the AVM):
    ///
    /// 1. **First-match-wins, in order.** Earlier `include_paths` entries shadow
    ///    later ones — the list is a PROPATH, not a set.
    /// 2. **Absolute vs relative.** `dir.join(name)` follows Rust's
    ///    [`Path::join`] semantics: an absolute `name` replaces `dir` entirely
    ///    (so `{/abs/x.i}` is an absolute reference); a relative `name` (the
    ///    common case) is appended to `dir`. Callers normalize every search
    ///    `dir` to an absolute path before calling, so relative-vs-absolute
    ///    ambiguity is resolved at the config boundary and this function stays
    ///    semantics-free.
    /// 3. **No implicit current directory.** Unlike some AVM configurations we
    ///    do not auto-prepend `"."`; users add it to `include_paths` explicitly
    ///    if they want cwd searched.
    fn resolve_include(&self, include_paths: &[PathBuf], name: &str) -> Option<PathBuf> {
        for dir in include_paths {
            let candidate = dir.join(name);
            if self.exists(&candidate) {
                return Some(candidate);
            }
        }
        None
    }
}

/// [`FileSystem`] implementation that reads from the real filesystem.
#[derive(Debug, Default)]
pub struct RealFileSystem;

impl FileSystem for RealFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        let content = std::fs::read_to_string(path)?;
        Ok(Arc::from(content))
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
}

/// [`FileSystem`] implementation backed by an in-memory `HashMap`.
///
/// Used for hermetic tests and for the LSP server (where file contents
/// come from document sync, not disk).
#[derive(Debug, Default)]
pub struct InMemoryFileSystem {
    files: HashMap<PathBuf, Arc<str>>,
}

impl InMemoryFileSystem {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a file. Overwrites any previous content at the same path.
    pub fn insert(&mut self, path: PathBuf, content: impl Into<Arc<str>>) {
        self.files.insert(path, content.into());
    }

    /// Iterate over all paths in the file system.
    pub fn paths(&self) -> impl Iterator<Item = &Path> {
        self.files.keys().map(|p| p.as_path())
    }
}

impl FileSystem for InMemoryFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.files
            .get(path)
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("{}", path.display())))
    }

    fn exists(&self, path: &Path) -> bool {
        self.files.contains_key(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn in_memory_read_existing() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/src/main.p"), "MESSAGE 'hello'.".to_string());

        let content = fs.read(Path::new("/src/main.p")).unwrap();
        assert_eq!(&*content, "MESSAGE 'hello'.");
    }

    #[test]
    fn in_memory_read_missing() {
        let fs = InMemoryFileSystem::new();
        let err = fs.read(Path::new("/nope.p")).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::NotFound);
    }

    #[test]
    fn in_memory_exists() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/a.p"), "");
        assert!(fs.exists(Path::new("/a.p")));
        assert!(!fs.exists(Path::new("/b.p")));
    }

    #[test]
    fn resolve_include_finds_first_match() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/inc2/shared.i"), "/* shared */");

        let paths = vec![PathBuf::from("/inc1"), PathBuf::from("/inc2")];
        let result = fs.resolve_include(&paths, "shared.i");
        assert_eq!(result, Some(PathBuf::from("/inc2/shared.i")));
    }

    #[test]
    fn resolve_include_prefers_earlier_path() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/inc1/shared.i"), "/* first */");
        fs.insert(PathBuf::from("/inc2/shared.i"), "/* second */");

        let paths = vec![PathBuf::from("/inc1"), PathBuf::from("/inc2")];
        let result = fs.resolve_include(&paths, "shared.i");
        assert_eq!(result, Some(PathBuf::from("/inc1/shared.i")));
    }

    #[test]
    fn resolve_include_returns_none_when_not_found() {
        let fs = InMemoryFileSystem::new();
        let paths = vec![PathBuf::from("/inc1")];
        assert_eq!(fs.resolve_include(&paths, "missing.i"), None);
    }

    #[test]
    fn in_memory_overwrite() {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/a.p"), "old");
        fs.insert(PathBuf::from("/a.p"), "new");

        let content = fs.read(Path::new("/a.p")).unwrap();
        assert_eq!(&*content, "new");
    }
}
