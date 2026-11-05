use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use super::settle::Analyses;
use crate::graph::Graph;
use crate::store::Changes;

/// What the working tree holds at a path that a refresh tracks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Content {
    /// The refresh does not track the path.
    Untracked,
    /// Nothing is there.
    Absent,
    /// A directory, or a file that cannot be read.
    Unreadable,
    /// The BLAKE3 hash of the file's raw bytes.
    Hash([u8; 32]),
}

/// What a refresh found different from the store.
#[derive(Debug, Default)]
pub struct Delta {
    /// The paths whose content or presence differs from the store, new units
    /// included.
    pub changed: FxHashSet<u32>,
    /// The units the store had not seen as units.
    pub new_units: FxHashSet<u32>,
    /// The units of the store that are no longer in the working tree.
    pub removed_units: Vec<u32>,
    /// The nodes analysed again.
    pub reanalysed: FxHashSet<u32>,
}

/// The observed closure of a unit, by path id.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Observed {
    /// The files the last compile read, without the unit.
    pub files: Vec<u32>,
    /// The paths that must stay absent.
    pub absent: Vec<u32>,
}

/// The result of one refresh: the working tree as the graph, and what changed.
pub struct Outcome {
    /// The generation of the store after the refresh commits.
    pub generation: u64,
    /// The generation the store holds now.
    pub stored_generation: u64,
    /// The canonical path of each path id.
    pub names: Vec<String>,
    /// The ids of the compile units, sorted by path.
    pub units: Vec<u32>,
    pub delta: Delta,
    /// The graph before the refresh and after it. Empty when nothing changed
    /// and the caller did not ask for the graph.
    pub before: Arc<Graph>,
    pub after: Arc<Graph>,
    /// The analysis of each node that holds. Empty when the refresh skipped
    /// loading them.
    pub analyses: Analyses,
    /// The content of each path by id. Empty unless the request asked for
    /// keys.
    pub content: Vec<Content>,
    /// The generation in which each tracked path last changed, by id. Empty
    /// unless the request asked for keys.
    pub changed_gen: Vec<u64>,
    /// The observed closure of each unit that has one.
    pub observed: FxHashMap<u32, Observed>,
    /// Problems a client shows and goes on from.
    pub warnings: Vec<String>,
    /// What to write to the store.
    pub changes: Changes,
}

impl Outcome {
    /// The canonical path of path id `id`.
    pub fn name(&self, id: u32) -> &str {
        self.names.get(id as usize).map_or("", String::as_str)
    }

    /// The content of path id `id`.
    pub fn content_of(&self, id: u32) -> Content {
        self.content
            .get(id as usize)
            .copied()
            .unwrap_or(Content::Untracked)
    }

    /// The generation in which path id `id` last changed; 0 when untracked.
    pub fn changed_gen_of(&self, id: u32) -> u64 {
        self.changed_gen.get(id as usize).copied().unwrap_or(0)
    }

    /// The id of the canonical path `name`, when the working tree has one.
    pub fn id_of(&self, name: &str) -> Option<u32> {
        self.names
            .iter()
            .position(|n| n == name)
            .map(|id| id as u32)
    }
}
