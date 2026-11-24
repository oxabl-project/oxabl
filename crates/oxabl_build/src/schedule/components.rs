//! Strongly connected components of the ordering graph.

use rustc_hash::FxHashMap;

use crate::graph::Graph;

/// The ordering graph: the compile-time graph and further edges, from a unit
/// to units that must compile before it.
pub struct Order<'a> {
    pub graph: &'a Graph,
    pub extra: &'a FxHashMap<u32, Vec<u32>>,
}

impl Order<'_> {
    /// The edge of `node` at position `at`, the extra edges after the graph's.
    fn edge(&self, node: u32, at: usize) -> Option<u32> {
        let edges = self.graph.edges(node);
        match edges.get(at) {
            Some(target) => Some(*target),
            None => self
                .extra
                .get(&node)
                .and_then(|extra| extra.get(at - edges.len()))
                .copied(),
        }
    }

    /// Every edge of `node`.
    pub fn edges(&self, node: u32) -> impl Iterator<Item = u32> + '_ {
        let extra = self.extra.get(&node).map_or(&[][..], Vec::as_slice);
        self.graph.edges(node).iter().chain(extra).copied()
    }
}

/// The components reachable from some roots, numbered so that a component
/// comes after every component it points at.
pub struct Components {
    /// The component of each node; `NONE` for a node no root reaches.
    pub of: Vec<u32>,
    /// The members of each component.
    pub members: Vec<Vec<u32>>,
}

pub const NONE: u32 = u32::MAX;

/// Tarjan's algorithm, iterative, over the nodes `roots` reach.
pub fn components(graph: &Order<'_>, nodes: usize, roots: &[u32]) -> Components {
    const UNSEEN: u32 = u32::MAX;
    let mut index = vec![UNSEEN; nodes];
    let mut low = vec![0u32; nodes];
    let mut on_stack = vec![false; nodes];
    let mut of = vec![NONE; nodes];
    let mut members: Vec<Vec<u32>> = Vec::new();
    let mut stack: Vec<u32> = Vec::new();
    let mut next = 0u32;
    // Each frame is a node and the position of its next edge.
    let mut frames: Vec<(u32, usize)> = Vec::new();
    for &root in roots {
        if index[root as usize] != UNSEEN {
            continue;
        }
        frames.push((root, 0));
        while let Some(&mut (node, ref mut at)) = frames.last_mut() {
            let n = node as usize;
            if *at == 0 && index[n] == UNSEEN {
                index[n] = next;
                low[n] = next;
                next += 1;
                stack.push(node);
                on_stack[n] = true;
            }
            if let Some(target) = graph.edge(node, *at) {
                *at += 1;
                let t = target as usize;
                if index[t] == UNSEEN {
                    frames.push((target, 0));
                } else if on_stack[t] {
                    low[n] = low[n].min(index[t]);
                }
                continue;
            }
            frames.pop();
            if let Some(&(parent, _)) = frames.last() {
                let p = parent as usize;
                low[p] = low[p].min(low[n]);
            }
            if low[n] == index[n] {
                let id = members.len() as u32;
                let mut group = Vec::new();
                while let Some(member) = stack.pop() {
                    on_stack[member as usize] = false;
                    of[member as usize] = id;
                    group.push(member);
                    if member == node {
                        break;
                    }
                }
                members.push(group);
            }
        }
    }
    Components { of, members }
}
