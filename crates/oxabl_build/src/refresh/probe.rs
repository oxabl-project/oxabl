use std::fs::Metadata;
use std::io::Read;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::store::FileRow;

/// A stat younger than this when taken is racy: the file can change again
/// without a new mtime, so its hash is not trusted at the next refresh.
pub const RACY_NS: i64 = 2_000_000_000;

/// What `stat` says about a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Stat {
    pub size: u64,
    pub mtime_ns: i64,
    pub ctime_ns: i64,
    pub ino: u64,
    pub dev: u64,
}

impl Stat {
    pub fn of(metadata: &Metadata) -> Self {
        Stat {
            size: metadata.size(),
            mtime_ns: metadata.mtime() * 1_000_000_000 + metadata.mtime_nsec(),
            ctime_ns: metadata.ctime() * 1_000_000_000 + metadata.ctime_nsec(),
            ino: metadata.ino(),
            dev: metadata.dev(),
        }
    }

    pub fn of_row(row: &FileRow) -> Self {
        Stat {
            size: row.size,
            mtime_ns: row.mtime_ns,
            ctime_ns: row.ctime_ns,
            ino: row.ino,
            dev: row.dev,
        }
    }
}

/// The wall clock, in nanoseconds since the Unix epoch.
pub fn now_ns() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_nanos() as i64)
}

/// The BLAKE3 hash that stands for a file whose bytes cannot be read, so that
/// it differs from the hash of any readable file.
pub fn unreadable_hash() -> [u8; 32] {
    *blake3::hash(b"\0unreadable").as_bytes()
}

/// The BLAKE3 hash that stands for a directory found where a file was looked
/// for.
pub fn directory_hash() -> [u8; 32] {
    *blake3::hash(b"\0directory").as_bytes()
}

/// The hash of the raw bytes at `path`. A directory and an unreadable file
/// have their own marker hashes.
pub fn hash_file(path: &Path, is_dir: bool) -> [u8; 32] {
    if is_dir {
        return directory_hash();
    }
    let mut bytes = Vec::new();
    match std::fs::File::open(path).and_then(|mut f| f.read_to_end(&mut bytes)) {
        Ok(_) => *blake3::hash(&bytes).as_bytes(),
        Err(_) => unreadable_hash(),
    }
}

/// What the file system says about `path` now.
#[derive(Debug, Clone, Copy)]
pub enum Probe {
    Absent,
    Present { stat: Stat, is_dir: bool },
}

/// Stat `path`, following links. Anything that stops the stat counts as absent.
pub fn probe(path: &Path) -> Probe {
    match std::fs::metadata(path) {
        Ok(metadata) => Probe::Present {
            stat: Stat::of(&metadata),
            is_dir: metadata.is_dir(),
        },
        Err(_) => Probe::Absent,
    }
}

/// Whether the stored hash of `old` still holds for a file that stats as
/// `stat`: the stat is equal and was not racy when it was taken.
pub fn hash_holds(old: &FileRow, stat: &Stat) -> bool {
    old.present
        && old.hash.is_some()
        && Stat::of_row(old) == *stat
        && old.observed_ns - old.mtime_ns >= RACY_NS
}

/// Whether a row taken at `observed_ns` for a file last modified at `mtime_ns`
/// is racy.
pub fn is_racy(observed_ns: i64, mtime_ns: i64) -> bool {
    observed_ns - mtime_ns < RACY_NS
}
