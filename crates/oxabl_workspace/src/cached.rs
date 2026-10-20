//! [`CachedFileSystem`]: a [`FileSystem`] that remembers what another one
//! answered, for a run over files that do not change while it lasts.

use std::hash::{BuildHasher, Hash};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::encoding::SourceDecoding;
use crate::file_system::FileSystem;

const SHARDS: usize = 32;

/// A [`FileSystem`] that answers each question once and remembers the answer.
///
/// A walk that analyses every compile root of a tree reads each shared include
/// once per root that includes it, and resolves its name on the search path
/// again each time. When names resolve ignoring case, a miss in one directory
/// lists that directory. Wrapped in this, the walk reads, decodes and resolves
/// each include once.
///
/// A file's text is kept from its third read on, so a file a walk reads once
/// or twice, such as a compile root, costs no memory. Name resolutions,
/// existence checks and failed reads are kept at once.
///
/// Every answer is kept for the life of the value, so build one per run and
/// drop it when the files may have changed; it is not for a client that lives
/// across edits. It is `Sync`, so the threads of a run share one.
///
/// It owns what it wraps. `F` may be unsized, so a boxed
/// `CachedFileSystem<SomeFs>` coerces to `Box<CachedFileSystem<dyn FileSystem>>`.
pub struct CachedFileSystem<F: ?Sized> {
    shards: Box<[RwLock<Shard>]>,
    inner: F,
}

#[derive(Default)]
struct Shard {
    reads: FxHashMap<PathBuf, Read>,
    exists: FxHashMap<PathBuf, bool>,
    /// Keyed by a hash of the search path and the name; the entries compare
    /// both, so a hash collision costs a search and never a wrong answer.
    resolved: FxHashMap<u64, Vec<Resolved>>,
    ignoring_case: FxHashMap<(PathBuf, PathBuf), Vec<PathBuf>>,
}

/// Reads before a file's text is kept.
const READS_BEFORE_KEPT: u8 = 2;

enum Read {
    Seen(u8),
    Text(Arc<str>),
    Failed(io::ErrorKind, String),
}

struct Resolved {
    include_paths: Box<[PathBuf]>,
    name: Box<str>,
    found: Option<PathBuf>,
}

impl<F: FileSystem> CachedFileSystem<F> {
    /// Remember what `inner` answers.
    pub fn new(inner: F) -> Self {
        CachedFileSystem {
            shards: (0..SHARDS).map(|_| RwLock::default()).collect(),
            inner,
        }
    }
}

impl<F: FileSystem + ?Sized> CachedFileSystem<F> {
    /// The wrapped filesystem.
    pub fn inner(&self) -> &F {
        &self.inner
    }

    fn shard(&self, hash: u64) -> &RwLock<Shard> {
        &self.shards[(hash as usize) % SHARDS]
    }

    fn shard_of(&self, key: &(impl Hash + ?Sized)) -> &RwLock<Shard> {
        self.shard(FxBuildHasher.hash_one(key))
    }
}

fn read_lock(shard: &RwLock<Shard>) -> std::sync::RwLockReadGuard<'_, Shard> {
    shard.read().unwrap_or_else(|e| e.into_inner())
}

fn write_lock(shard: &RwLock<Shard>) -> std::sync::RwLockWriteGuard<'_, Shard> {
    shard.write().unwrap_or_else(|e| e.into_inner())
}

impl<F: FileSystem + ?Sized> FileSystem for CachedFileSystem<F> {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        let shard = self.shard_of(path);
        let seen = match read_lock(shard).reads.get(path) {
            Some(Read::Text(text)) => return Ok(Arc::clone(text)),
            Some(Read::Failed(kind, message)) => {
                return Err(io::Error::new(*kind, message.clone()));
            }
            Some(Read::Seen(count)) => *count,
            None => 0,
        };
        let read = self.inner.read(path);
        let entry = match &read {
            Ok(text) if seen >= READS_BEFORE_KEPT => Read::Text(Arc::clone(text)),
            Ok(_) => Read::Seen(seen + 1),
            Err(e) => Read::Failed(e.kind(), e.to_string()),
        };
        write_lock(shard).reads.insert(path.to_path_buf(), entry);
        read
    }

    fn exists(&self, path: &Path) -> bool {
        let shard = self.shard_of(path);
        if let Some(known) = read_lock(shard).exists.get(path) {
            return *known;
        }
        let exists = self.inner.exists(path);
        write_lock(shard).exists.insert(path.to_path_buf(), exists);
        exists
    }

    fn resolve_include(&self, include_paths: &[PathBuf], name: &str) -> Option<PathBuf> {
        let hash = FxBuildHasher.hash_one((include_paths, name));
        let shard = self.shard(hash);
        let same = |r: &Resolved| *r.include_paths == *include_paths && *r.name == *name;
        if let Some(known) = read_lock(shard)
            .resolved
            .get(&hash)
            .and_then(|entries| entries.iter().find(|r| same(r)))
        {
            return known.found.clone();
        }
        let found = self.inner.resolve_include(include_paths, name);
        let mut shard = write_lock(shard);
        let entries = shard.resolved.entry(hash).or_default();
        if !entries.iter().any(same) {
            entries.push(Resolved {
                include_paths: include_paths.into(),
                name: name.into(),
                found: found.clone(),
            });
        }
        found
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        let key = (dir.to_path_buf(), relative.to_path_buf());
        let shard = self.shard_of(&key);
        if let Some(known) = read_lock(shard).ignoring_case.get(&key) {
            return known.clone();
        }
        let found = self.inner.find_ignoring_case(dir, relative);
        write_lock(shard).ignoring_case.insert(key, found.clone());
        found
    }

    /// `inner` with `decoding`, uncached, as a filesystem with another
    /// decoding reads other text.
    fn with_decoding(&self, decoding: SourceDecoding) -> Option<Arc<dyn FileSystem>> {
        self.inner.with_decoding(decoding)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::InMemoryFileSystem;
    use std::sync::Mutex;

    /// Counts the questions that reach the wrapped filesystem.
    #[derive(Default)]
    struct Counting {
        inner: InMemoryFileSystem,
        calls: Mutex<Vec<String>>,
    }

    impl Counting {
        fn calls(&self, prefix: &str) -> usize {
            self.calls
                .lock()
                .unwrap()
                .iter()
                .filter(|c| c.starts_with(prefix))
                .count()
        }
    }

    impl FileSystem for Counting {
        fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
            self.calls
                .lock()
                .unwrap()
                .push(format!("read {}", path.display()));
            self.inner.read(path)
        }

        fn exists(&self, path: &Path) -> bool {
            self.calls
                .lock()
                .unwrap()
                .push(format!("exists {}", path.display()));
            self.inner.exists(path)
        }

        fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
            self.calls.lock().unwrap().push("ignoring_case".to_string());
            self.inner.find_ignoring_case(dir, relative)
        }
    }

    fn fs() -> CachedFileSystem<Counting> {
        let mut inner = InMemoryFileSystem::new();
        inner.insert(PathBuf::from("/a/x.i"), "/* a */");
        inner.insert(PathBuf::from("/b/x.i"), "/* b */");
        inner.insert(PathBuf::from("/b/Y.i"), "/* y */");
        CachedFileSystem::new(Counting {
            inner,
            calls: Mutex::default(),
        })
    }

    #[test]
    fn a_text_is_kept_from_its_third_read() {
        let fs = fs();
        for _ in 0..5 {
            assert_eq!(&*fs.read(Path::new("/a/x.i")).unwrap(), "/* a */");
        }
        assert_eq!(fs.inner().calls("read"), 3);
    }

    #[test]
    fn a_failed_read_is_kept_with_its_kind() {
        let fs = fs();
        for _ in 0..3 {
            let err = fs.read(Path::new("/nope.i")).unwrap_err();
            assert_eq!(err.kind(), io::ErrorKind::NotFound);
        }
        assert_eq!(fs.inner().calls("read"), 1);
    }

    #[test]
    fn a_resolution_is_kept_per_search_path_and_name() {
        let fs = fs();
        let ab = [PathBuf::from("/a"), PathBuf::from("/b")];
        let ba = [PathBuf::from("/b"), PathBuf::from("/a")];
        for _ in 0..3 {
            assert_eq!(
                fs.resolve_include(&ab, "x.i"),
                Some(PathBuf::from("/a/x.i"))
            );
            assert_eq!(
                fs.resolve_include(&ba, "x.i"),
                Some(PathBuf::from("/b/x.i"))
            );
            assert_eq!(fs.resolve_include(&ab, "z.i"), None);
        }
        // The default resolution asks `exists` once per directory it tries.
        assert_eq!(fs.inner().calls("exists"), 1 + 1 + 2);
    }

    #[test]
    fn existence_and_case_insensitive_matches_are_kept() {
        let fs = fs();
        for _ in 0..3 {
            assert!(fs.exists(Path::new("/b/Y.i")));
            assert_eq!(
                fs.find_ignoring_case(Path::new("/b"), Path::new("y.I")),
                [PathBuf::from("/b/Y.i")]
            );
        }
        assert_eq!(fs.inner().calls("exists"), 1);
        assert_eq!(fs.inner().calls("ignoring_case"), 1);
    }

    #[test]
    fn a_boxed_cache_coerces_to_one_over_any_filesystem() {
        let boxed: Box<CachedFileSystem<dyn FileSystem>> = Box::new(fs());
        assert_eq!(&*boxed.read(Path::new("/a/x.i")).unwrap(), "/* a */");
        let erased: Box<dyn FileSystem> = Box::new(fs());
        assert!(erased.exists(Path::new("/b/Y.i")));
    }

    #[test]
    fn it_is_sync() {
        fn sync<T: Sync + Send>() {}
        sync::<CachedFileSystem<crate::RealFileSystem>>();
    }
}
