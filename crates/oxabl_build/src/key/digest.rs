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
    sha256_hex(text.as_bytes())
}

/// The 32 bytes of a lower-case hex SHA-256.
pub fn sha256_bytes(hex: &str) -> Option<[u8; 32]> {
    if hex.len() != 64
        || !hex
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
    {
        return None;
    }
    let mut out = [0u8; 32];
    for (at, byte) in out.iter_mut().enumerate() {
        *byte = u8::from_str_radix(&hex[at * 2..at * 2 + 2], 16).ok()?;
    }
    Some(out)
}

/// The lower-case hex SHA-256 of `bytes`.
pub fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}
