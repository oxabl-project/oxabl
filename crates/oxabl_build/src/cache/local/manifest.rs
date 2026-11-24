//! The manifest of one cache entry: a text file that names every file with
//! its size and hash and ends with a hash of itself.

use std::fmt::Write;

use super::super::seam::{ArtifactPath, CompileKey, RemoteOrigin};

const MAGIC: &str = "oxabl-artifacts 1";

/// One file of an entry.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ManifestFile {
    pub size: u64,
    pub blake3: [u8; 32],
    pub path: ArtifactPath,
}

/// What an entry holds and where it came from.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Manifest {
    pub key: CompileKey,
    /// `None` for a set this machine compiled.
    pub remote: Option<RemoteOrigin>,
    pub files: Vec<ManifestFile>,
}

fn encode(path: &str) -> String {
    let mut out = String::with_capacity(path.len());
    for byte in path.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'/' | b'~' => {
                out.push(byte as char)
            }
            other => {
                let _ = write!(out, "%{other:02X}");
            }
        }
    }
    out
}

fn decode(text: &str) -> Option<String> {
    let bytes = text.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut at = 0;
    while at < bytes.len() {
        if bytes[at] == b'%' {
            let hex = text.get(at + 1..at + 3)?;
            out.push(u8::from_str_radix(hex, 16).ok()?);
            at += 3;
        } else {
            out.push(bytes[at]);
            at += 1;
        }
    }
    String::from_utf8(out).ok()
}

fn hex(bytes: &[u8; 32]) -> String {
    blake3::Hash::from_bytes(*bytes).to_hex().to_string()
}

fn unhex(text: &str) -> Option<[u8; 32]> {
    blake3::Hash::from_hex(text)
        .ok()
        .map(|hash| *hash.as_bytes())
}

/// A token of an origin line: no white space.
fn token(text: &str) -> String {
    encode(text)
}

impl Manifest {
    /// The text of the manifest, with its closing `end` line.
    pub fn render(&self) -> String {
        let mut text = format!("{MAGIC}\nkey {}\n", self.key.hex());
        match &self.remote {
            None => text.push_str("origin compiled\n"),
            Some(remote) => {
                let _ = writeln!(
                    text,
                    "origin remote {} {}",
                    token(&remote.tier),
                    token(&remote.signer)
                );
            }
        }
        for file in &self.files {
            let _ = writeln!(
                text,
                "file {} {} {}",
                file.size,
                hex(&file.blake3),
                encode(&file.path.0)
            );
        }
        let end = blake3::hash(text.as_bytes());
        let _ = writeln!(text, "end {}", end.to_hex());
        text
    }

    /// Reads a manifest and checks its `end` line. Any deviation is `None`.
    pub fn parse(text: &str) -> Option<Manifest> {
        let body_end = text.rfind("end ")?;
        if body_end != 0 && text.as_bytes()[body_end - 1] != b'\n' {
            return None;
        }
        let (body, last) = text.split_at(body_end);
        let last = last.strip_suffix('\n')?;
        let claimed = unhex(last.strip_prefix("end ")?)?;
        if *blake3::hash(body.as_bytes()).as_bytes() != claimed {
            return None;
        }
        let mut lines = body.lines();
        if lines.next()? != MAGIC {
            return None;
        }
        let key = CompileKey(unhex(lines.next()?.strip_prefix("key ")?)?);
        let origin = lines.next()?.strip_prefix("origin ")?;
        let remote = match origin.strip_prefix("remote ") {
            Some(rest) => {
                let (tier, signer) = rest.split_once(' ')?;
                Some(RemoteOrigin {
                    tier: decode(tier)?,
                    signer: decode(signer)?,
                })
            }
            None if origin == "compiled" => None,
            None => return None,
        };
        let mut files = Vec::new();
        for line in lines {
            let mut parts = line.strip_prefix("file ")?.splitn(3, ' ');
            let size = parts.next()?.parse().ok()?;
            let blake3 = unhex(parts.next()?)?;
            let path = ArtifactPath(decode(parts.next()?)?);
            files.push(ManifestFile { size, blake3, path });
        }
        Some(Manifest { key, remote, files })
    }

    /// The bytes the entry's files take.
    pub fn bytes(&self) -> u64 {
        self.files.iter().map(|file| file.size).sum()
    }
}
