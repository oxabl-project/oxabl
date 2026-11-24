//! Answers to `HASH` and `HASHSET`.

use std::path::{Path, PathBuf};

use sha2::{Digest, Sha256};

/// What the worker finds at one path.
pub enum Hashed {
    File { size: u64, sha256: String },
    Dir,
    None,
}

impl Hashed {
    /// The digest word of a `HASHSET` list line.
    fn digest_word(&self) -> String {
        match self {
            Hashed::File { sha256, .. } => sha256.clone(),
            Hashed::Dir => "dir".to_string(),
            Hashed::None => "none".to_string(),
        }
    }
}

fn resolve(path: &str, dlc: &Path) -> PathBuf {
    if path == "$DLC" {
        dlc.to_path_buf()
    } else if let Some(rest) = path.strip_prefix("$DLC/") {
        dlc.join(rest)
    } else {
        PathBuf::from(path)
    }
}

pub fn hash(path: &str, dlc: &Path) -> Hashed {
    let resolved = resolve(path, dlc);
    match std::fs::metadata(&resolved) {
        Ok(meta) if meta.is_dir() => Hashed::Dir,
        Ok(_) => match std::fs::read(&resolved) {
            Ok(bytes) => Hashed::File {
                size: bytes.len() as u64,
                sha256: hex(&Sha256::digest(&bytes)),
            },
            Err(_) => Hashed::None,
        },
        Err(_) => Hashed::None,
    }
}

/// The SHA-256 of the lines `<path> TAB <digest> LF` of a sorted path list.
pub fn hash_set(paths: &[String], dlc: &Path) -> String {
    let mut text = String::new();
    for path in paths {
        text.push_str(path);
        text.push('\t');
        text.push_str(&hash(path, dlc).digest_word());
        text.push('\n');
    }
    hex(&Sha256::digest(text.as_bytes()))
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}
