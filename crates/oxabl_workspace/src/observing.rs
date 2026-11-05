//! [`ObservingFileSystem`]: a [`FileSystem`] that records what is asked of it.

use std::cell::RefCell;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::file_system::FileSystem;
use crate::observations::Observations;

static NEXT_OWNER: AtomicU64 = AtomicU64::new(1);

struct Scope {
    owner: u64,
    observations: Observations,
}

thread_local! {
    static SCOPES: RefCell<Vec<Scope>> = const { RefCell::new(Vec::new()) };
}

/// A scope open on this thread. Dropped before [`close`](Self::close), as
/// when the observed run panics, it leaves the thread's stack as it found it.
struct Open {
    depth: usize,
    closed: bool,
}

impl Open {
    fn new(owner: u64) -> Self {
        let depth = SCOPES.with(|scopes| {
            let mut scopes = scopes.borrow_mut();
            scopes.push(Scope {
                owner,
                observations: Observations::default(),
            });
            scopes.len() - 1
        });
        Open {
            depth,
            closed: false,
        }
    }

    fn close(mut self) -> Observations {
        self.closed = true;
        SCOPES
            .with(|scopes| scopes.borrow_mut().pop())
            .map(|scope| scope.observations)
            .unwrap_or_default()
    }
}

impl Drop for Open {
    fn drop(&mut self) {
        if !self.closed {
            SCOPES.with(|scopes| scopes.borrow_mut().truncate(self.depth));
        }
    }
}

/// A [`FileSystem`] that records the files it reads, the paths it finds
/// absent and the directories it lists, for the scope that asked.
///
/// [`observe`](FileSystem::observe) opens a scope on the calling thread: the
/// questions the thread asks until the run ends are the scope's observations.
/// A question outside any scope is answered and not recorded. Scopes nest and
/// do not leak into each other: an inner scope's observations reach the outer
/// one only through [`attribute`](FileSystem::attribute).
///
/// - A read is a `read` observation, unless the file is missing: that is an
///   `absent` one. A read that fails for another reason is a `read` too, as
///   the file's bytes decide the failure.
/// - An existence check is `read` when the path exists and `absent` when it
///   does not. So each candidate an include search tries is observed, the
///   misses before the hit as `absent`.
/// - A case-insensitive lookup is a `listed` observation for each directory
///   it listed.
///
/// Wrap the outermost filesystem: one under a [`CachedFileSystem`] never
/// sees the questions the cache answers. It owns what it wraps; `F` may be
/// unsized. [`with_decoding`](FileSystem::with_decoding) answers `None`:
/// build the observer over a filesystem that already has its decoding.
///
/// [`CachedFileSystem`]: crate::CachedFileSystem
pub struct ObservingFileSystem<F: ?Sized> {
    owner: u64,
    inner: F,
}

impl<F: FileSystem> ObservingFileSystem<F> {
    pub fn new(inner: F) -> Self {
        ObservingFileSystem {
            owner: NEXT_OWNER.fetch_add(1, Ordering::Relaxed),
            inner,
        }
    }
}

impl<F: FileSystem + ?Sized> ObservingFileSystem<F> {
    /// The wrapped filesystem.
    pub fn inner(&self) -> &F {
        &self.inner
    }

    fn record(&self, record: impl FnOnce(&mut Observations)) {
        SCOPES.with(|scopes| {
            let mut scopes = scopes.borrow_mut();
            if let Some(scope) = scopes.iter_mut().rev().find(|s| s.owner == self.owner) {
                record(&mut scope.observations);
            }
        });
    }
}

impl<F: FileSystem + ?Sized> FileSystem for ObservingFileSystem<F> {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        let read = self.inner.read(path);
        match &read {
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::NotFound | io::ErrorKind::NotADirectory
                ) =>
            {
                self.record(|o| o.add_absent(path))
            }
            _ => self.record(|o| o.add_read(path)),
        }
        read
    }

    fn exists(&self, path: &Path) -> bool {
        let exists = self.inner.exists(path);
        if exists {
            self.record(|o| o.add_read(path));
        } else {
            self.record(|o| o.add_absent(path));
        }
        exists
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        self.find_ignoring_case_listing(dir, relative, &mut Vec::new())
    }

    fn find_ignoring_case_listing(
        &self,
        dir: &Path,
        relative: &Path,
        listed: &mut Vec<PathBuf>,
    ) -> Vec<PathBuf> {
        let start = listed.len();
        let found = self.inner.find_ignoring_case_listing(dir, relative, listed);
        self.record(|o| listed[start..].iter().for_each(|dir| o.add_listed(dir)));
        found
    }

    fn observes(&self) -> bool {
        true
    }

    fn observe(&self, run: &mut dyn FnMut()) -> Option<Arc<Observations>> {
        let open = Open::new(self.owner);
        run();
        Some(Arc::new(open.close()))
    }

    fn attribute(&self, observations: &Observations) {
        self.record(|o| o.merge(observations));
    }
}
