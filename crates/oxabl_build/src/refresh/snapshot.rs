use std::collections::{BTreeSet, HashMap};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

use oxabl_workspace::{EncodedFileSystem, FileSystem, SourceDecoding};

use super::probe::unreadable_hash;

/// What the refresh knows about the bytes of one file.
#[derive(Debug, Clone, Copy)]
pub struct Known {
    pub hash: [u8; 32],
}

/// The files of one refresh as it hashed them. A read hashes the bytes it
/// returns and compares the hash with what the refresh knows: the stored hash
/// of a file whose stat did not change, or the hash of the first read. A file
/// whose bytes differ from that changed during the refresh; it is recorded in
/// [`drift`](Self::take_drift) and the analyses that read it run again.
pub struct SnapshotFs {
    inner: EncodedFileSystem,
    decoding: SourceDecoding,
    known: RwLock<HashMap<PathBuf, Known>>,
    drift: Mutex<BTreeSet<PathBuf>>,
}

impl SnapshotFs {
    pub fn new(decoding: SourceDecoding) -> Self {
        SnapshotFs {
            inner: EncodedFileSystem::new(decoding),
            decoding,
            known: RwLock::default(),
            drift: Mutex::default(),
        }
    }

    /// Record what the refresh knows of `path`.
    pub fn learn(&self, path: PathBuf, known: Known) {
        self.known
            .write()
            .unwrap_or_else(|e| e.into_inner())
            .insert(path, known);
    }

    /// What the refresh knows of `path`.
    pub fn known(&self, path: &Path) -> Option<Known> {
        self.known
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get(path)
            .copied()
    }

    /// The paths read with bytes other than the ones the refresh knew, since
    /// the last call.
    pub fn take_drift(&self) -> BTreeSet<PathBuf> {
        std::mem::take(&mut *self.drift.lock().unwrap_or_else(|e| e.into_inner()))
    }

    /// The raw bytes of `path` and their hash, compared with what is known.
    fn read_bytes(&self, path: &Path) -> io::Result<(Vec<u8>, [u8; 32])> {
        let mut file = std::fs::File::open(path)?;
        let mut bytes = Vec::with_capacity(file.metadata()?.len() as usize);
        file.read_to_end(&mut bytes)?;
        let hash = *blake3::hash(&bytes).as_bytes();
        let agrees = self
            .known
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get(path)
            .map(|known| known.hash == hash);
        match agrees {
            Some(true) => {}
            Some(false) => {
                self.drift
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .insert(path.to_path_buf());
            }
            None => {
                self.known
                    .write()
                    .unwrap_or_else(|e| e.into_inner())
                    .entry(path.to_path_buf())
                    .or_insert(Known { hash });
            }
        }
        Ok((bytes, hash))
    }

    /// The text of `path` as the analysis of that file reads it, and the hash
    /// of its bytes. The hash is that of an unreadable file when the read
    /// fails.
    pub fn read_hashed(&self, path: &Path) -> (io::Result<Arc<str>>, [u8; 32]) {
        match self.read_bytes(path) {
            Ok((bytes, hash)) => {
                let text = self.decoding.decode(bytes).map(|t| Arc::from(t.text));
                (text, hash)
            }
            Err(e) => (Err(e), unreadable_hash()),
        }
    }
}

impl FileSystem for SnapshotFs {
    fn read(&self, path: &Path) -> io::Result<Arc<str>> {
        let (bytes, _) = self.read_bytes(path)?;
        Ok(Arc::from(self.decoding.decode(bytes)?.text))
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn find_ignoring_case(&self, dir: &Path, relative: &Path) -> Vec<PathBuf> {
        self.inner.find_ignoring_case(dir, relative)
    }

    fn find_ignoring_case_listing(
        &self,
        dir: &Path,
        relative: &Path,
        listed: &mut Vec<PathBuf>,
    ) -> Vec<PathBuf> {
        self.inner.find_ignoring_case_listing(dir, relative, listed)
    }
}
