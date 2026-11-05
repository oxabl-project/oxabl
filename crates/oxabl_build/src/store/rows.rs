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

/// `tracked_by` bits.
pub mod tracked {
    /// A unit found by the walk.
    pub const UNIT: u8 = 1;
    /// A file an analysis read.
    pub const READ: u8 = 2;
    /// A path an analysis found absent.
    pub const ABSENT: u8 = 4;
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
