//! The schema cache: where `oxabl` keeps the loaded form of the `.df` files so
//! that the next process does not parse them again.
//!
//! An entry is one file below the user cache directory, named by a hash of the
//! build of this program and of every path and byte of the `.df` files it was
//! made from (see `docs/overview.md`, "The schema cache"). It starts with a hash
//! of its key and of the bytes after it, so an entry that changed on disk is
//! not read. Any problem with the cache is a miss: the files are parsed and the
//! run goes on.

use std::ffi::OsStr;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, SystemTime};

use oxabl_build::cache::Mode;
use oxabl_schema::{SchemaCache, SchemaSource};

/// Entries kept in the directory. The oldest go first.
const MAX_ENTRIES: usize = 16;
/// Bytes of the hash at the start of an entry.
const HASH_LEN: usize = 32;
/// An entry larger than this is not read.
const MAX_ENTRY_BYTES: u64 = 256 << 20;
/// A temporary file older than this is a write that did not finish.
const STALE_TEMPORARY: Duration = Duration::from_secs(3600);

/// Route the schema loads of this process through the cache, unless the
/// environment turns it off.
pub fn install() {
    if let Some(cache) = DiskCache::from_environment() {
        oxabl_pipeline::set_schema_cache(Box::new(cache));
    }
}

struct DiskCache {
    directory: PathBuf,
    /// Hash of the executable (`oxabl_daemon::build_identity`), so a new build
    /// never reads an entry of an old one. Made at the first key: a run that
    /// loads no schema does not read the executable.
    build: std::sync::OnceLock<Option<[u8; 32]>>,
}

impl DiskCache {
    /// `None` when the cache is off: `OXABL_SCHEMA_CACHE=off`, a cache mode
    /// other than `developer` (the cache trusts every process of the user, as
    /// the local build cache does), or no known directory. With no known build (the
    /// executable cannot be read) every key is empty and no entry is read or written. A
    /// test build uses only a directory that `OXABL_SCHEMA_CACHE_DIR` names, so
    /// a test never reads or writes the cache of the user.
    fn from_environment() -> Option<Self> {
        let var = |name| std::env::var_os(name).filter(|value| !value.is_empty());
        if var("OXABL_SCHEMA_CACHE").is_some_and(|value| value == "off") {
            return None;
        }
        let mode = Mode::resolve(
            std::env::var_os("OXABL_CACHE_MODE").as_deref(),
            std::env::var_os("CI").as_deref(),
        );
        if mode.ok() != Some(Mode::Developer) {
            return None;
        }
        let directory = match var("OXABL_SCHEMA_CACHE_DIR") {
            Some(directory) => PathBuf::from(directory),
            None if oxabl_common::schema_files_parsed().is_some() => return None,
            None => default_directory(
                var("XDG_CACHE_HOME").map(PathBuf::from),
                var("HOME").map(PathBuf::from),
            )?,
        };
        Some(DiskCache {
            directory,
            build: std::sync::OnceLock::new(),
        })
    }

    fn entry(&self, key: &str) -> PathBuf {
        self.directory.join(format!("{key}.bin"))
    }
}

impl SchemaCache for DiskCache {
    fn key(&self, files: &[SchemaSource<'_>]) -> String {
        let Some(build) = self.build.get_or_init(build_identity) else {
            return String::new();
        };
        let mut hasher = blake3::Hasher::new();
        hasher.update(build);
        hasher.update(&(files.len() as u64).to_le_bytes());
        for file in files {
            let path = file.path.as_os_str().as_encoded_bytes();
            hasher.update(&(path.len() as u64).to_le_bytes());
            hasher.update(path);
            hasher.update(&(file.text.len() as u64).to_le_bytes());
            hasher.update(file.text.as_bytes());
        }
        hasher.finalize().to_hex().to_string()
    }

    fn fetch(&self, key: &str) -> Option<Vec<u8>> {
        if key.is_empty() {
            return None;
        }
        private_directory(&self.directory).ok()?;
        let mut file = open_regular(&self.entry(key)).ok()?;
        let len = file.metadata().ok().filter(|meta| meta.is_file())?.len();
        if !(HASH_LEN as u64..=MAX_ENTRY_BYTES).contains(&len) {
            return None;
        }
        let mut bytes = Vec::with_capacity(len as usize);
        file.by_ref()
            .take(MAX_ENTRY_BYTES + 1)
            .read_to_end(&mut bytes)
            .ok()?;
        if bytes.len() as u64 != len || bytes[..HASH_LEN] != entry_hash(key, &bytes[HASH_LEN..]) {
            return None;
        }
        // A use keeps the entry from being the first to go. Best effort.
        let _ = file.set_modified(SystemTime::now());
        bytes.drain(..HASH_LEN);
        Some(bytes)
    }

    fn store(&self, key: &str, entry: &[u8]) {
        if !key.is_empty() {
            let _ = self.write(key, entry);
        }
    }
}

impl DiskCache {
    fn write(&self, key: &str, entry: &[u8]) -> std::io::Result<()> {
        create_private_directory(&self.directory)?;
        private_directory(&self.directory)?;
        let target = self.entry(key);
        // Unique in the process and among processes, so two writers of one key
        // never share a file.
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let temporary = target.with_extension(format!(
            "{}-{}.tmp",
            std::process::id(),
            NEXT.fetch_add(1, Ordering::Relaxed)
        ));
        let written =
            write_private(&temporary, key, entry).and_then(|()| fs::rename(&temporary, &target));
        if written.is_err() {
            let _ = fs::remove_file(&temporary);
        }
        written?;
        prune(&self.directory);
        Ok(())
    }
}

/// `$XDG_CACHE_HOME/oxabl/schema`, or `~/.cache/oxabl/schema`
/// (`~/Library/Caches/oxabl/schema` on macOS).
fn default_directory(xdg_cache_home: Option<PathBuf>, home: Option<PathBuf>) -> Option<PathBuf> {
    if cfg!(target_os = "macos") {
        return home.map(|home| home.join("Library/Caches/oxabl/schema"));
    }
    xdg_cache_home
        .map(|cache| cache.join("oxabl/schema"))
        .or_else(|| home.map(|home| home.join(".cache/oxabl/schema")))
}

/// What identifies the executable that runs: its version, and the size, the
/// times, the inode and the device of its file. The change time and the inode
/// cannot be set by `touch`, `cp -p`, `rsync -a` or an archive, so another
/// build with the size and the modification time of this one has another
/// identity. A hash of the bytes would cost about 15 ms in each run.
/// `None` when the file cannot be read, so the cache is off.
fn build_identity() -> Option<[u8; 32]> {
    let meta = fs::metadata(std::env::current_exe().ok()?).ok()?;
    let mut hasher = blake3::Hasher::new();
    hasher.update(env!("CARGO_PKG_VERSION").as_bytes());
    hasher.update(&meta.len().to_le_bytes());
    let modified = meta.modified().ok()?;
    hasher.update(
        &modified
            .duration_since(SystemTime::UNIX_EPOCH)
            .ok()?
            .as_nanos()
            .to_le_bytes(),
    );
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        for part in [
            meta.ctime(),
            meta.ctime_nsec(),
            meta.ino() as i64,
            meta.dev() as i64,
        ] {
            hasher.update(&part.to_le_bytes());
        }
    }
    #[cfg(not(unix))]
    {
        let id = &oxabl_daemon::build_identity().id;
        if id.is_empty() {
            return None;
        }
        hasher.update(id.as_bytes());
    }
    Some(*hasher.finalize().as_bytes())
}

/// The hash that starts an entry: it covers the key and the payload.
fn entry_hash(key: &str, payload: &[u8]) -> [u8; HASH_LEN] {
    let mut hasher = blake3::Hasher::new();
    hasher.update(&(key.len() as u64).to_le_bytes());
    hasher.update(key.as_bytes());
    hasher.update(payload);
    *hasher.finalize().as_bytes()
}

/// Opens `path` for reading without blocking and without following a link, so
/// a FIFO or a device at the path cannot stop the run.
#[cfg(unix)]
fn open_regular(path: &Path) -> std::io::Result<fs::File> {
    use std::os::unix::fs::OpenOptionsExt;
    let flags = rustix::fs::OFlags::NONBLOCK | rustix::fs::OFlags::NOFOLLOW;
    fs::OpenOptions::new()
        .read(true)
        .custom_flags(flags.bits() as i32)
        .open(path)
}

#[cfg(not(unix))]
fn open_regular(path: &Path) -> std::io::Result<fs::File> {
    if !fs::symlink_metadata(path)?.is_file() {
        return Err(std::io::ErrorKind::InvalidInput.into());
    }
    fs::File::open(path)
}

#[cfg(unix)]
fn create_private_directory(directory: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::DirBuilderExt;
    fs::DirBuilder::new()
        .recursive(true)
        .mode(0o700)
        .create(directory)
}

#[cfg(not(unix))]
fn create_private_directory(directory: &Path) -> std::io::Result<()> {
    fs::create_dir_all(directory)
}

/// Checks that only this user can write the directory.
#[cfg(unix)]
fn private_directory(directory: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::MetadataExt;
    let meta = fs::metadata(directory)?;
    if meta.is_dir() && meta.uid() == rustix::process::getuid().as_raw() && meta.mode() & 0o022 == 0
    {
        Ok(())
    } else {
        Err(std::io::ErrorKind::PermissionDenied.into())
    }
}

#[cfg(not(unix))]
fn private_directory(directory: &Path) -> std::io::Result<()> {
    if fs::metadata(directory)?.is_dir() {
        Ok(())
    } else {
        Err(std::io::ErrorKind::NotADirectory.into())
    }
}

fn write_private(path: &Path, key: &str, payload: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    let mut options = fs::OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    std::os::unix::fs::OpenOptionsExt::mode(&mut options, 0o600);
    let mut file = options.open(path)?;
    file.write_all(&entry_hash(key, payload))?;
    file.write_all(payload)
}

/// Whether `name` is an entry (`<64 hex digits>.bin`) or a temporary file of
/// ours (`<64 hex digits>.<pid>-<number>.tmp`): nothing else in the directory is touched.
fn kind_of(name: &OsStr) -> Option<bool> {
    let name = name.to_str()?;
    let (stem, rest) = name.split_once('.')?;
    if stem.len() != 64 || !stem.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    if rest == "bin" {
        return Some(true);
    }
    let (pid, number) = rest.strip_suffix(".tmp")?.split_once('-')?;
    let digits = |part: &str| !part.is_empty() && part.bytes().all(|b| b.is_ascii_digit());
    (digits(pid) && digits(number)).then_some(false)
}

/// Removes the entries beyond `MAX_ENTRIES`, the longest unused first (a read
/// sets the time of an entry), and the temporary
/// files of writes that did not finish.
fn prune(directory: &Path) {
    let Ok(listing) = fs::read_dir(directory) else {
        return;
    };
    let now = SystemTime::now();
    let mut entries = Vec::new();
    for item in listing.flatten() {
        let Some(is_entry) = kind_of(&item.file_name()) else {
            continue;
        };
        let modified = item
            .metadata()
            .and_then(|meta| meta.modified())
            .unwrap_or(SystemTime::UNIX_EPOCH);
        if is_entry {
            entries.push((modified, item.path()));
        } else if now.duration_since(modified).unwrap_or_default() > STALE_TEMPORARY {
            let _ = fs::remove_file(item.path());
        }
    }
    entries.sort();
    let excess = entries.len().saturating_sub(MAX_ENTRIES);
    for (_, path) in entries.into_iter().take(excess) {
        let _ = fs::remove_file(path);
    }
}
