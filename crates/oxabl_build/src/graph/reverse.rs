use std::collections::{HashMap, VecDeque};

use super::Graph;

/// A [`Graph`] turned around: the nodes whose compile reads a node.
#[derive(Debug, Default)]
pub struct Reverse {
    offsets: Vec<u32>,
    sources: Vec<u32>,
}

impl Reverse {
    pub fn of(graph: &Graph) -> Self {
        let nodes = graph.nodes();
        let mut offsets = vec![0u32; nodes + 1];
        for &target in graph.targets() {
            offsets[target as usize + 1] += 1;
        }
        for i in 0..nodes {
            offsets[i + 1] += offsets[i];
        }
        let mut sources = vec![0u32; graph.targets().len()];
        let mut fill = offsets.clone();
        for node in 0..nodes {
            let (from, to) = (graph.offsets()[node], graph.offsets()[node + 1]);
            for &target in &graph.targets()[from as usize..to as usize] {
                let at = &mut fill[target as usize];
                sources[*at as usize] = node as u32;
                *at += 1;
            }
        }
        Reverse { offsets, sources }
    }

    /// The nodes whose compile reads `node` directly.
    pub fn readers(&self, node: u32) -> &[u32] {
        match self.offsets.get(node as usize..node as usize + 2) {
            Some(&[from, to]) => &self.sources[from as usize..to as usize],
            _ => &[],
        }
    }

    /// Every node that reaches one of `starts`, the starts included, each with
    /// the next node on its way to a start (a start maps to itself).
    pub fn reaching(&self, starts: impl IntoIterator<Item = u32>) -> HashMap<u32, u32> {
        let mut via: HashMap<u32, u32> = HashMap::new();
        let mut queue = VecDeque::new();
        for start in starts {
            if via.insert(start, start).is_none() {
                queue.push_back(start);
            }
        }
        while let Some(node) = queue.pop_front() {
            for &reader in self.readers(node) {
                if let std::collections::hash_map::Entry::Vacant(slot) = via.entry(reader) {
                    slot.insert(node);
                    queue.push_back(reader);
                }
            }
        }
        via
    }
}
