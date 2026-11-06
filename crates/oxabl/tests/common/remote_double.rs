//! A test-only `ArtifactCache` that stands in for a remote cache service: it
//! keeps sets in memory, serves them back, and records what it sees. It never
//! stands in for oxabl.

#![allow(dead_code)]

use std::collections::HashMap;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use oxabl::build::cache::{
    ArtifactCache, ArtifactPath, ArtifactSetRef, ArtifactUploader, CompileKey, FetchOutcome,
    FetchRequest, NotStoredReason, Origin, RemoteOrigin, StoreOutcome,
};

type StoredFiles = Vec<(ArtifactPath, Vec<u8>)>;
type PushHook = Box<dyn Fn(&ArtifactSetRef) + Send>;

/// One push as the service saw it.
#[derive(Clone, Debug)]
pub struct Push {
    pub key: CompileKey,
    pub paths: Vec<String>,
    /// Where the files were when the set was pushed.
    pub disk_paths: Vec<std::path::PathBuf>,
    pub at_nanos: u128,
}

#[derive(Default)]
pub struct Inner {
    sets: Mutex<HashMap<CompileKey, StoredFiles>>,
    pushes: Mutex<Vec<Push>>,
    outcomes: Mutex<Vec<(CompileKey, StoreOutcome)>>,
    finished: AtomicUsize,
    fetched_keys: Mutex<Vec<CompileKey>>,
    fills: Mutex<Vec<CompileKey>>,
    /// Report only this many keys of a fetch, then stop.
    stop_after: Mutex<Option<usize>>,
    panic_in_fetch: std::sync::atomic::AtomicBool,
    panic_in_push: std::sync::atomic::AtomicBool,
    /// Rewrites a file between verification and the read of a push.
    tamper: Mutex<Option<PushHook>>,
    pub writes: bool,
}

/// The service and the handle a build gets.
#[derive(Clone)]
pub struct Remote(pub Arc<Inner>);

fn nanos() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| elapsed.as_nanos())
}

impl Remote {
    pub fn new() -> Remote {
        Remote(Arc::new(Inner {
            writes: true,
            ..Inner::default()
        }))
    }

    /// A service that takes no sets.
    pub fn read_only() -> Remote {
        Remote(Arc::new(Inner::default()))
    }

    pub fn cache(&self) -> Box<dyn ArtifactCache> {
        Box::new(self.clone())
    }

    pub fn stop_fetch_after(&self, keys: usize) {
        *self.0.stop_after.lock().unwrap() = Some(keys);
    }

    /// The next `fetch` call panics.
    pub fn panic_in_fetch(&self) {
        self.0.panic_in_fetch.store(true, Ordering::SeqCst);
    }

    /// Every `push` panics.
    pub fn panic_in_push(&self) {
        self.0.panic_in_push.store(true, Ordering::SeqCst);
    }

    pub fn on_push_read(&self, hook: impl Fn(&ArtifactSetRef) + Send + 'static) {
        *self.0.tamper.lock().unwrap() = Some(Box::new(hook));
    }

    pub fn stored(&self) -> usize {
        self.0.sets.lock().unwrap().len()
    }

    pub fn pushes(&self) -> Vec<Push> {
        self.0.pushes.lock().unwrap().clone()
    }

    /// The artifact paths of every push, sorted.
    pub fn pushed_paths(&self) -> Vec<String> {
        let mut paths: Vec<String> = self.pushes().into_iter().flat_map(|p| p.paths).collect();
        paths.sort();
        paths
    }

    pub fn outcomes(&self) -> Vec<(CompileKey, StoreOutcome)> {
        self.0.outcomes.lock().unwrap().clone()
    }

    pub fn finished(&self) -> usize {
        self.0.finished.load(Ordering::SeqCst)
    }

    pub fn fetched_keys(&self) -> usize {
        self.0.fetched_keys.lock().unwrap().len()
    }

    pub fn fills(&self) -> usize {
        self.0.fills.lock().unwrap().len()
    }

    /// Drops the sets with the given artifact path, as a service that lost them.
    pub fn forget(&self, path: &str) {
        self.0
            .sets
            .lock()
            .unwrap()
            .retain(|_, files| !files.iter().any(|(p, _)| p.0 == path));
    }
}

struct Uploader(Arc<Inner>, Vec<(CompileKey, StoreOutcome)>);

impl ArtifactUploader for Uploader {
    fn push(&mut self, set: ArtifactSetRef) {
        if self.0.panic_in_push.load(Ordering::SeqCst) {
            panic!("the double's uploader panics");
        }
        self.0.pushes.lock().unwrap().push(Push {
            key: set.key,
            paths: set.files.iter().map(|file| file.path.0.clone()).collect(),
            disk_paths: set
                .files
                .iter()
                .map(|file| file.disk_path.clone())
                .collect(),
            at_nanos: nanos(),
        });
        if let Some(hook) = self.0.tamper.lock().unwrap().as_ref() {
            hook(&set);
        }
        let mut files = Vec::new();
        for file in &set.files {
            let bytes = match std::fs::read(&file.disk_path) {
                Ok(bytes) => bytes,
                Err(_) => {
                    self.1.push((
                        set.key,
                        StoreOutcome::NotStored(NotStoredReason::Unreadable),
                    ));
                    return;
                }
            };
            if bytes.len() as u64 != file.size || *blake3::hash(&bytes).as_bytes() != file.blake3 {
                self.1.push((
                    set.key,
                    StoreOutcome::NotStored(NotStoredReason::ChangedOnDisk),
                ));
                return;
            }
            files.push((file.path.clone(), bytes));
        }
        let outcome = match self.0.sets.lock().unwrap().insert(set.key, files) {
            Some(_) => StoreOutcome::AlreadyPresent,
            None => StoreOutcome::Stored,
        };
        self.1.push((set.key, outcome));
    }

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        self.0.finished.fetch_add(1, Ordering::SeqCst);
        *self.0.outcomes.lock().unwrap() = self.1.clone();
        self.1
    }
}

struct Discard(Arc<Inner>);

impl ArtifactUploader for Discard {
    fn push(&mut self, _: ArtifactSetRef) {}

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        self.0.finished.fetch_add(1, Ordering::SeqCst);
        Vec::new()
    }
}

impl ArtifactCache for Remote {
    fn label(&self) -> &str {
        "remote-double"
    }

    fn writes(&self) -> bool {
        self.0.writes
    }

    fn fetch(
        &self,
        requests: Vec<FetchRequest>,
        stage: &Path,
        on_outcome: &mut dyn FnMut(FetchOutcome),
    ) {
        if self.0.panic_in_fetch.load(Ordering::SeqCst) {
            panic!("the double's fetch panics");
        }
        let limit = self.0.stop_after.lock().unwrap().unwrap_or(usize::MAX);
        for (reported, request) in requests.into_iter().enumerate() {
            if reported >= limit {
                return;
            }
            self.0.fetched_keys.lock().unwrap().push(request.key);
            let found = self.0.sets.lock().unwrap().get(&request.key).cloned();
            let Some(files) = found else {
                on_outcome(FetchOutcome::Miss {
                    key: request.key,
                    reason: oxabl::build::cache::MissReason::NotFound,
                });
                continue;
            };
            let mut have: Vec<&ArtifactPath> = files.iter().map(|(path, _)| path).collect();
            let mut want: Vec<&ArtifactPath> = request.expect.iter().collect();
            have.sort();
            want.sort();
            if have != want {
                on_outcome(FetchOutcome::Miss {
                    key: request.key,
                    reason: oxabl::build::cache::MissReason::PathMismatch,
                });
                continue;
            }
            for (path, bytes) in &files {
                let target = stage.join(&path.0);
                std::fs::create_dir_all(target.parent().unwrap()).unwrap();
                std::fs::write(target, bytes).unwrap();
            }
            on_outcome(FetchOutcome::Installed {
                key: request.key,
                files: files.iter().map(|(path, _)| path.clone()).collect(),
                origin: Origin::Remote(RemoteOrigin {
                    tier: "shared".to_string(),
                    signer: "double".to_string(),
                }),
            });
        }
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        match self.0.writes {
            true => Box::new(Uploader(self.0.clone(), Vec::new())),
            false => Box::new(Discard(self.0.clone())),
        }
    }

    fn fill(&self, sets: Vec<(ArtifactSetRef, RemoteOrigin)>) {
        let mut fills = self.0.fills.lock().unwrap();
        fills.extend(sets.iter().map(|(set, _)| set.key));
    }
}
