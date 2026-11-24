use std::collections::hash_map::RandomState;
use std::fs::File;
use std::hash::{BuildHasher, Hasher};

use super::probe::{RACY_NS, Stat};
use crate::store::BinaryRow;

/// The running executable, opened. On Linux this is the file that runs, even
/// when its path was replaced or removed since.
fn open_executable() -> std::io::Result<File> {
    if cfg!(target_os = "linux") {
        File::open("/proc/self/exe")
    } else {
        File::open(std::env::current_exe()?)
    }
}

/// A value that no stored hash equals, for an executable that cannot be read.
fn unique_hash() -> [u8; 32] {
    let mut hasher = blake3::Hasher::new_derive_key("oxabl 2026-11 unreadable executable");
    for _ in 0..2 {
        hasher.update(&RandomState::new().build_hasher().finish().to_le_bytes());
    }
    hasher.update(&std::process::id().to_le_bytes());
    hasher.update(&crate::refresh::probe::now_ns().to_le_bytes());
    *hasher.finalize().as_bytes()
}

fn stat_of(row: &BinaryRow) -> Stat {
    Stat {
        size: row.size,
        mtime_ns: row.mtime_ns,
        ctime_ns: row.ctime_ns,
        ino: row.ino,
        dev: row.dev,
    }
}

fn hash_of(file: &mut File) -> std::io::Result<[u8; 32]> {
    let mut hasher = blake3::Hasher::new();
    hasher.update_reader(file)?;
    Ok(*hasher.finalize().as_bytes())
}

/// The BLAKE3 hash of the running executable: the code that makes an analysis.
/// `stored` is the row of the last refresh; its hash is reused when the stat
/// of the executable is equal and was not racy. The second value is the row to
/// store, when the hash was taken again.
///
/// An executable that cannot be read gives a different hash on every call, so
/// that no stored analysis is trusted.
pub fn executable_hash(
    stored: Option<&BinaryRow>,
    observed_ns: i64,
) -> ([u8; 32], Option<BinaryRow>) {
    let Ok(mut file) = open_executable() else {
        return (unique_hash(), None);
    };
    let Ok(metadata) = file.metadata() else {
        return (unique_hash(), None);
    };
    let stat = Stat::of(&metadata);
    if let Some(row) = stored
        && stat_of(row) == stat
        && row.observed_ns - row.mtime_ns >= RACY_NS
    {
        return (row.hash, None);
    }
    let Ok(hash) = hash_of(&mut file) else {
        return (unique_hash(), None);
    };
    let path = std::env::current_exe()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();
    let row = BinaryRow {
        path,
        size: stat.size,
        mtime_ns: stat.mtime_ns,
        ctime_ns: stat.ctime_ns,
        ino: stat.ino,
        dev: stat.dev,
        observed_ns,
        hash,
    };
    (hash, Some(row))
}
