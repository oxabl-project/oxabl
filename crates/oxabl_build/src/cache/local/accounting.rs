//! The size of the cache and eviction, under the cache lock.

use std::fs::{self, File, OpenOptions};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use super::layout::{LAST_USED, Layout, MANIFEST};
use super::manifest::Manifest;
use crate::worker::random;

/// Eviction stops at this share of the limit.
const TARGET_PERCENT: u64 = 90;

/// The lock file of the cache, locked for the length of one value.
pub struct Guard {
    file: File,
}

impl Guard {
    /// Waits for the exclusive lock.
    pub fn exclusive(layout: &Layout) -> Option<Guard> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(layout.lock())
            .ok()?;
        file.lock().ok()?;
        Some(Guard { file })
    }
}

impl Drop for Guard {
    fn drop(&mut self) {
        let _ = self.file.unlock();
    }
}

fn read_size(layout: &Layout) -> Option<u64> {
    fs::read_to_string(layout.size()).ok()?.trim().parse().ok()
}

fn write_size(layout: &Layout, size: u64) {
    let temp = layout.tmp().join(format!("size-{}", random::token()));
    if fs::write(&temp, size.to_string()).is_ok() && fs::rename(&temp, layout.size()).is_err() {
        let _ = fs::remove_file(&temp);
    }
}

/// Adds `bytes` to the recorded size. The size is a hint: eviction measures
/// the entries again.
pub fn add(layout: &Layout, bytes: u64) -> u64 {
    let Some(_guard) = Guard::exclusive(layout) else {
        return 0;
    };
    let total = read_size(layout).unwrap_or(0).saturating_add(bytes);
    write_size(layout, total);
    total
}

struct Held {
    path: PathBuf,
    bytes: u64,
    used: SystemTime,
}

fn entries(layout: &Layout) -> Vec<Held> {
    let mut held = Vec::new();
    let Ok(groups) = fs::read_dir(layout.entries()) else {
        return held;
    };
    for group in groups.flatten() {
        let Ok(inside) = fs::read_dir(group.path()) else {
            continue;
        };
        for entry in inside.flatten() {
            let path = entry.path();
            let bytes = fs::read_to_string(path.join(MANIFEST))
                .ok()
                .and_then(|text| Manifest::parse(&text))
                .map_or(0, |manifest| manifest.bytes());
            let used = fs::metadata(path.join(LAST_USED))
                .or_else(|_| fs::metadata(&path))
                .and_then(|meta| meta.modified())
                .unwrap_or(SystemTime::UNIX_EPOCH);
            held.push(Held { path, bytes, used });
        }
    }
    held
}

fn remove(layout: &Layout, path: &Path) {
    let away = layout.tmp().join(format!("gone-{}", random::token()));
    if fs::rename(path, &away).is_ok() {
        let _ = fs::remove_dir_all(&away);
    }
}

/// Deletes the quarantine and, when the recorded size is over `limit`, the
/// entries with the oldest `last-used` until the total is under 90 % of it.
/// Returns the number of entries removed.
pub fn evict(layout: &Layout, limit: u64) -> usize {
    let Some(_guard) = Guard::exclusive(layout) else {
        return 0;
    };
    if read_size(layout).is_some_and(|size| size <= limit) {
        return 0;
    }
    if let Ok(held) = fs::read_dir(layout.quarantine()) {
        for item in held.flatten() {
            remove(layout, &item.path());
        }
    }
    let mut held = entries(layout);
    let mut total: u64 = held.iter().map(|entry| entry.bytes).sum();
    let mut removed = 0;
    if total > limit {
        held.sort_by(|a, b| a.used.cmp(&b.used).then_with(|| a.path.cmp(&b.path)));
        let target = limit / 100 * TARGET_PERCENT;
        for entry in held {
            if total <= target {
                break;
            }
            remove(layout, &entry.path);
            total = total.saturating_sub(entry.bytes);
            removed += 1;
        }
    }
    write_size(layout, total);
    removed
}
