//! Strongly connected components of the ordering graph.

use crate::graph::Graph;

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
pub fn components(graph: &Graph, nodes: usize, roots: &[u32]) -> Components {
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
            let edges = graph.edges(node);
            if let Some(&target) = edges.get(*at) {
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
