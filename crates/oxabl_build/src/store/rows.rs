//! The rows of the store, as the refresh and the planner see them.

use crate::kinds::{EdgeKind, ObservationKind};

/// One edge of an analysed node. `name` is the class name that reached the
/// target, for class edges.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Edge {
    pub kind: EdgeKind,
    pub target: u32,
    pub name: Option<String>,
}

/// One reference that resolved to nothing usable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Unresolved {
    pub kind: EdgeKind,
    pub name: String,
    pub reason: u8,
}

/// One thing an analysis observed: a path it read, found absent or listed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Observation {
    pub kind: ObservationKind,
    pub path: u32,
}

/// The state of one tracked path when the store last saw it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileRow {
    pub present: bool,
    pub size: u64,
    pub mtime_ns: i64,
    pub ctime_ns: i64,
    pub ino: u64,
    pub dev: u64,
    pub hash: Option<[u8; 32]>,
    pub observed_ns: i64,
    pub changed_gen: u64,
    pub tracked_by: u8,
}

/// The hash of the running executable, with the stat it was taken at, so that
/// an unchanged executable is not read again.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryRow {
    pub path: String,
    pub size: u64,
    pub mtime_ns: i64,
    pub ctime_ns: i64,
    pub ino: u64,
    pub dev: u64,
    pub observed_ns: i64,
    pub hash: [u8; 32],
}

impl BinaryRow {
    /// Six 8-byte fields, the hash, then the path.
    pub fn encode(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(80 + self.path.len());
        out.extend_from_slice(&self.size.to_le_bytes());
        out.extend_from_slice(&self.mtime_ns.to_le_bytes());
        out.extend_from_slice(&self.ctime_ns.to_le_bytes());
        out.extend_from_slice(&self.ino.to_le_bytes());
        out.extend_from_slice(&self.dev.to_le_bytes());
        out.extend_from_slice(&self.observed_ns.to_le_bytes());
        out.extend_from_slice(&self.hash);
        out.extend_from_slice(self.path.as_bytes());
        out
    }

    /// `None` for bytes that are not an encoded row.
    pub fn decode(bytes: &[u8]) -> Option<Self> {
        if bytes.len() < 80 {
            return None;
        }
        let word = |at: usize| <[u8; 8]>::try_from(&bytes[at..at + 8]).ok();
        Some(BinaryRow {
            size: u64::from_le_bytes(word(0)?),
            mtime_ns: i64::from_le_bytes(word(8)?),
            ctime_ns: i64::from_le_bytes(word(16)?),
            ino: u64::from_le_bytes(word(24)?),
            dev: u64::from_le_bytes(word(32)?),
            observed_ns: i64::from_le_bytes(word(40)?),
            hash: <[u8; 32]>::try_from(&bytes[48..80]).ok()?,
            path: String::from_utf8(bytes[80..].to_vec()).ok()?,
        })
    }
}

/// `tracked_by` bits.
pub mod tracked {
    /// A unit found by the walk.
    pub const UNIT: u8 = 1;
    /// A file an analysis read.
    pub const READ: u8 = 2;
    /// A path an analysis found absent.
    pub const ABSENT: u8 = 4;
    /// A file or an absent path of an observed closure.
    pub const OBSERVED: u8 = 8;
}

/// The files a compile of a unit read, as its cross-reference named them, the
/// paths that must stay absent so that each include resolves to the same
/// file, and the class sources the compile could read. Canonical paths,
/// sorted, without the unit itself.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ObservedClosure {
    pub files: Vec<String>,
    pub absent: Vec<String>,
    pub classes: Vec<String>,
}

/// A directory that a case-insensitive lookup listed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListingRow {
    pub mtime_ns: i64,
    pub ctime_ns: i64,
    pub ino: u64,
    pub observed_ns: i64,
    pub digest: [u8; 32],
    pub changed_gen: u64,
}

/// What one analysis found, and what it depended on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Analysis {
    pub analysed_gen: u64,
    pub content_hash: [u8; 32],
    pub completeness: Vec<u8>,
    pub edges: Vec<Edge>,
    pub unresolved: Vec<Unresolved>,
    pub observations: Vec<Observation>,
}

/// An analysis as stored: the blobs, not yet decoded.
#[derive(Debug, Clone)]
pub struct StoredAnalysis {
    pub analysed_gen: u64,
    pub content_hash: [u8; 32],
    pub completeness: Vec<u8>,
    pub edges: Vec<u8>,
    pub unresolved: Vec<u8>,
    pub observations: Vec<u8>,
}
