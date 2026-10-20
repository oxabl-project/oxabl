use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::encoding::SourceDecoding;

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

    /// Every existing file under `dir` whose path below `dir` equals `relative`
    /// when each component is compared ignoring ASCII case. The returned paths
    /// carry the on-disk spelling.
    ///
    /// The default finds nothing, which is right for a filesystem that is
    /// already case-insensitive or one that cannot list directories. Callers
    /// reach for it only after an exact lookup missed, and only when the
    /// workspace opted in.
    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        let _ = (dir, relative);
        Vec::new()
    }

    /// This filesystem reading with `decoding`, or `None` when its source of
    /// text has no encoding to configure (the default).
    ///
    /// A long-lived client that holds a filesystem and re-resolves
    /// `oxabl.toml` calls it to carry a changed `encoding` over.
    fn with_decoding(&self, decoding: SourceDecoding) -> Option<Arc<dyn FileSystem>> {
        let _ = decoding;
        None
    }
}

/// Forwards every question, so a wrapper such as
/// [`CachedFileSystem`](crate::CachedFileSystem) can borrow or box the
/// filesystem it wraps.
macro_rules! forward_file_system {
    ($ty:ty) => {
        impl<F: FileSystem + ?Sized> FileSystem for $ty {
            fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
                (**self).read(path)
            }

            fn exists(&self, path: &Path) -> bool {
                (**self).exists(path)
            }

            fn resolve_include(&self, include_paths: &[PathBuf], name: &str) -> Option<PathBuf> {
                (**self).resolve_include(include_paths, name)
            }

            fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
                (**self).find_ignoring_case(dir, relative)
            }

            fn with_decoding(&self, decoding: SourceDecoding) -> Option<Arc<dyn FileSystem>> {
                (**self).with_decoding(decoding)
            }
        }
    };
}

forward_file_system!(&F);
forward_file_system!(Box<F>);
forward_file_system!(Arc<F>);

/// Components of `relative` as text; `None` when one is not a plain name.
fn plain_components(relative: &Path) -> Option<Vec<&std::ffi::OsStr>> {
    relative
        .components()
        .map(|c| match c {
            std::path::Component::Normal(part) => Some(part),
            _ => None,
        })
        .collect()
}

/// [`FileSystem`] implementation that reads UTF-8 from the real filesystem. A
/// leading byte order mark is removed. For another encoding use
/// [`EncodedFileSystem`].
#[derive(Debug, Default)]
pub struct RealFileSystem;

impl FileSystem for RealFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        EncodedFileSystem::default().read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        find_on_disk_ignoring_case(dir, relative)
    }

    fn with_decoding(&self, decoding: SourceDecoding) -> Option<Arc<dyn FileSystem>> {
        Some(Arc::new(EncodedFileSystem::new(decoding)))
    }
}

/// [`FileSystem`] implementation that reads from the real filesystem and decodes
/// each file as the configured [`SourceDecoding`]
/// (`[workspace.sources].encoding`).
#[derive(Debug, Default, Clone, Copy)]
pub struct EncodedFileSystem {
    decoding: SourceDecoding,
}

impl EncodedFileSystem {
    pub fn new(decoding: SourceDecoding) -> Self {
        EncodedFileSystem { decoding }
    }
}

impl FileSystem for EncodedFileSystem {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        let source = self.decoding.read_file(path)?;
        Ok(Arc::from(source.text))
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        find_on_disk_ignoring_case(dir, relative)
    }

    fn with_decoding(&self, decoding: SourceDecoding) -> Option<Arc<dyn FileSystem>> {
        Some(Arc::new(EncodedFileSystem::new(decoding)))
    }
}

fn find_on_disk_ignoring_case(dir: &Path, relative: &Path) -> Vec<PathBuf> {
    let Some(parts) = plain_components(relative) else {
        return Vec::new();
    };
    let mut level = vec![dir.to_path_buf()];
    for part in parts {
        let mut next = Vec::new();
        for parent in &level {
            let Ok(entries) = std::fs::read_dir(parent) else {
                continue;
            };
            for entry in entries.flatten() {
                if entry.file_name().eq_ignore_ascii_case(part) {
                    next.push(entry.path());
                }
            }
        }
        if next.is_empty() {
            return next;
        }
        level = next;
    }
    level.sort();
    level
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

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        let Some(parts) = plain_components(relative) else {
            return Vec::new();
        };
        let mut found: Vec<PathBuf> = self
            .files
            .keys()
            .filter(|path| {
                let Ok(below) = path.strip_prefix(dir) else {
                    return false;
                };
                let mut actual = below.components();
                parts.iter().all(|part| {
                    matches!(actual.next(), Some(std::path::Component::Normal(a)) if a.eq_ignore_ascii_case(part))
                }) && actual.next().is_none()
            })
            .cloned()
            .collect();
        found.sort();
        found
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn real_file_system_is_strict_utf8_and_drops_a_bom() {
        let dir = tempfile::tempdir().unwrap();
        let bom = dir.path().join("bom.p");
        std::fs::write(&bom, b"\xef\xbb\xbfMESSAGE 'caf\xc3\xa9'.").unwrap();
        assert_eq!(&*RealFileSystem.read(&bom).unwrap(), "MESSAGE 'caf\u{e9}'.");

        let latin = dir.path().join("latin.p");
        std::fs::write(&latin, b"MESSAGE 'caf\xe9'.").unwrap();
        let err = RealFileSystem.read(&latin).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn encoded_file_system_decodes_a_single_byte_file() {
        use crate::{SourceDecoding, SourceEncoding};
        let dir = tempfile::tempdir().unwrap();
        let latin = dir.path().join("latin.p");
        std::fs::write(&latin, b"MESSAGE '\x93caf\xe9\x94'.").unwrap();
        let fs = EncodedFileSystem::new(SourceDecoding::new(SourceEncoding::Windows1252, None));
        assert_eq!(
            &*fs.read(&latin).unwrap(),
            "MESSAGE '\u{201c}caf\u{e9}\u{201d}'."
        );
    }

    #[test]
    fn with_decoding_upgrades_the_disk_filesystems_only() {
        use crate::{SourceDecoding, SourceEncoding};
        let decoding = SourceDecoding::new(SourceEncoding::Windows1252, None);
        assert!(RealFileSystem.with_decoding(decoding).is_some());
        assert!(InMemoryFileSystem::new().with_decoding(decoding).is_none());
    }

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

    #[test]
    fn find_ignoring_case_matches_every_component_on_a_real_tree() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("sub")).unwrap();
        std::fs::write(dir.path().join("sub/prog.p"), "").unwrap();
        let fs = RealFileSystem;
        assert_eq!(
            fs.find_ignoring_case(dir.path(), Path::new("Sub/Prog.p")),
            [dir.path().join("sub/prog.p")]
        );
        assert!(
            fs.find_ignoring_case(dir.path(), Path::new("Sub/Other.p"))
                .is_empty()
        );
        assert!(
            fs.find_ignoring_case(dir.path(), Path::new("../x.p"))
                .is_empty()
        );
    }

    #[test]
    fn find_ignoring_case_default_finds_nothing() {
        struct Plain;
        impl FileSystem for Plain {
            fn read(&self, _: &Path) -> Result<Arc<str>, io::Error> {
                Err(io::ErrorKind::NotFound.into())
            }
            fn exists(&self, _: &Path) -> bool {
                true
            }
        }
        assert!(
            Plain
                .find_ignoring_case(Path::new("/"), Path::new("a.p"))
                .is_empty()
        );
    }
}
