//! The schema cache: where `oxabl` keeps the loaded form of the `.df` files so
//! that the next process does not parse them again.
//!
//! An entry is one file below the user cache directory, named by a hash of the
//! build of this program and of every path and byte of the `.df` files it was
//! made from (see `docs/overview.md`, "The schema cache"). Any problem with the
//! cache is a miss: the files are parsed and the run goes on.

use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

use oxabl_build::cache::Mode;
use oxabl_schema::{SchemaCache, SchemaSource};

/// Entries kept in the directory. The oldest go first.
const MAX_ENTRIES: usize = 16;
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
    /// Hash of what decides how the program reads a `.df`: its version and the
    /// file it runs from, so a new build never reads an entry of an old one.
    build: [u8; 32],
}

impl DiskCache {
    /// `None` when the cache is off: `OXABL_SCHEMA_CACHE=off`, a cache mode
    /// other than `developer` (the cache trusts every process of the user, as
    /// the local build cache does), no known directory, or no known build.
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
            None => default_directory(
                var("XDG_CACHE_HOME").map(PathBuf::from),
                var("HOME").map(PathBuf::from),
            )?,
        };
        Some(DiskCache {
            directory,
            build: build_identity()?,
        })
    }

    fn entry(&self, key: &str) -> PathBuf {
        self.directory.join(format!("{key}.bin"))
    }
}

impl SchemaCache for DiskCache {
    fn key(&self, files: &[SchemaSource<'_>]) -> String {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&self.build);
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
        private_directory(&self.directory).ok()?;
        fs::read(self.entry(key)).ok()
    }

    fn store(&self, key: &str, entry: &[u8]) {
        let _ = self.write(key, entry);
    }
}

impl DiskCache {
    fn write(&self, key: &str, entry: &[u8]) -> std::io::Result<()> {
        create_private_directory(&self.directory)?;
        private_directory(&self.directory)?;
        let target = self.entry(key);
        let temporary = target.with_extension(format!("{}.tmp", std::process::id()));
        let written =
            write_private(&temporary, entry).and_then(|()| fs::rename(&temporary, &target));
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

/// The version of this program and the size and time of the file it runs from.
fn build_identity() -> Option<[u8; 32]> {
    let meta = fs::metadata(std::env::current_exe().ok()?).ok()?;
    let modified = meta
        .modified()
        .ok()?
        .duration_since(SystemTime::UNIX_EPOCH)
        .ok()?;
    let mut hasher = blake3::Hasher::new();
    hasher.update(env!("CARGO_PKG_VERSION").as_bytes());
    hasher.update(&meta.len().to_le_bytes());
    hasher.update(&modified.as_nanos().to_le_bytes());
    Some(*hasher.finalize().as_bytes())
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

fn write_private(path: &Path, bytes: &[u8]) -> std::io::Result<()> {
    use std::io::Write;
    let mut options = fs::OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    std::os::unix::fs::OpenOptionsExt::mode(&mut options, 0o600);
    options.open(path)?.write_all(bytes)
}

/// Whether `name` is an entry (`<64 hex digits>.bin`) or a temporary file of
/// ours (`<64 hex digits>.<pid>.tmp`): nothing else in the directory is touched.
fn kind_of(name: &OsStr) -> Option<bool> {
    let name = name.to_str()?;
    let (stem, rest) = name.split_once('.')?;
    if stem.len() != 64 || !stem.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    if rest == "bin" {
        return Some(true);
    }
    let pid = rest.strip_suffix(".tmp")?;
    (!pid.is_empty() && pid.bytes().all(|b| b.is_ascii_digit())).then_some(false)
}

/// Removes the entries beyond `MAX_ENTRIES`, oldest first, and the temporary
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
