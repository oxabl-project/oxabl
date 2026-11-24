//! Looking at a local cache directory from outside: entries, quarantine and
//! manifests, as the format document describes them.

#![allow(dead_code)]

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Every file below `dir` with its bytes, by path relative to `dir`.
pub fn tree(dir: &Path) -> BTreeMap<String, Vec<u8>> {
    fn walk(base: &Path, dir: &Path, out: &mut BTreeMap<String, Vec<u8>>) {
        let Ok(entries) = fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(base, &path, out);
            } else {
                let relative = path.strip_prefix(base).unwrap();
                out.insert(
                    relative.to_string_lossy().into_owned(),
                    fs::read(&path).unwrap(),
                );
            }
        }
    }
    let mut out = BTreeMap::new();
    walk(dir, dir, &mut out);
    out
}

/// The entry directories `v1/entries/<hh>/<64 hex>` of a cache.
pub fn entries(cache: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let Ok(groups) = fs::read_dir(cache.join("v1/entries")) else {
        return found;
    };
    for group in groups.flatten() {
        for entry in fs::read_dir(group.path()).unwrap().flatten() {
            found.push(entry.path());
        }
    }
    found.sort();
    found
}

/// The files of an entry: artifact path and disk path.
pub fn entry_files(entry: &Path) -> Vec<(String, PathBuf)> {
    let base = entry.join("files");
    tree(&base)
        .into_keys()
        .map(|relative| (relative.clone(), base.join(relative)))
        .collect()
}

/// The entry that holds the artifact `path`, for example `r/main.r`.
pub fn entry_with(cache: &Path, path: &str) -> PathBuf {
    entries(cache)
        .into_iter()
        .find(|entry| entry.join("files").join(path).is_file())
        .unwrap_or_else(|| panic!("no entry holds {path} in {}", cache.display()))
}

/// How many entries wait in `v1/quarantine`.
pub fn quarantined(cache: &Path) -> usize {
    fs::read_dir(cache.join("v1/quarantine")).map_or(0, |entries| entries.count())
}

/// Rewrites the `origin` line of a manifest and its closing `end` line, which
/// is the BLAKE3 hash of all the lines above it.
pub fn set_origin(entry: &Path, origin: &str) {
    let path = entry.join("manifest");
    let text = fs::read_to_string(&path).unwrap();
    let body: String = text
        .lines()
        .take_while(|line| !line.starts_with("end "))
        .map(|line| match line.starts_with("origin ") {
            true => format!("origin {origin}\n"),
            false => format!("{line}\n"),
        })
        .collect();
    let end = blake3::hash(body.as_bytes()).to_hex();
    fs::write(path, format!("{body}end {end}\n")).unwrap();
}

/// Flips one bit of the first byte of a file, as a new file: the entry may
/// share its inode with a build output, which must stay as it is.
pub fn flip_a_byte(path: &Path) {
    let mut bytes = fs::read(path).unwrap();
    bytes[0] ^= 1;
    fs::remove_file(path).unwrap();
    fs::write(path, bytes).unwrap();
}
