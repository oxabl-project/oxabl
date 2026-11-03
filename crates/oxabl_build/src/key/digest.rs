//! What a worker finds at a path it was asked to hash.

use sha2::{Digest, Sha256};

/// The answer to one `HASH`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FileDigest {
    File { size: u64, sha256: String },
    Dir,
    Absent,
}

impl FileDigest {
    /// The word a `HASHSET` list line holds: the digest, `dir` or `none`.
    pub fn word(&self) -> &str {
        match self {
            FileDigest::File { sha256, .. } => sha256,
            FileDigest::Dir => "dir",
            FileDigest::Absent => "none",
        }
    }
}

/// The digest a worker answers to a `HASHSET` of `entries`, which must be
/// sorted by path bytes: SHA-256 of the lines `<path> TAB <word> LF`.
pub fn hash_set_digest(entries: &[(String, FileDigest)]) -> String {
    let mut text = String::new();
    for (path, digest) in entries {
        text.push_str(path);
        text.push('\t');
        text.push_str(digest.word());
        text.push('\n');
    }
    Sha256::digest(text.as_bytes())
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}
