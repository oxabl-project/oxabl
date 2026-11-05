use std::sync::Arc;

use rustc_hash::FxHashSet;

use crate::graph::Graph;
use crate::store::Changes;

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

    /// The id of the canonical path `name`, when the working tree has one.
    pub fn id_of(&self, name: &str) -> Option<u32> {
        self.names
            .iter()
            .position(|n| n == name)
            .map(|id| id as u32)
    }
}
