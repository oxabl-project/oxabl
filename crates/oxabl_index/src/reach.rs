//! Reachability from entry points over the routines of many files.
//!
//! A [`ReachGraph`] holds the call sites of each routine (a node) and what
//! each one calls: another node, or Unknown with the reason
//! ([`Callee`]). Two searches start from the entry points ([`Seed`]):
//!
//! - [`ReachGraph::reach`]: which nodes an entry reaches, which entries reach
//!   each node (up to a cap), and the Unknown reasons of the reached calls.
//!   A node no entry reaches may still be reached through one of those.
//! - [`ReachGraph::gated`]: the same walk over states `(node, gated)`, where
//!   a call keeps the state gated once a gate is passed before it (an
//!   authorisation check, a reset of some state). A node reached in the
//!   ungated state has a path from an entry that passes no gate, and the
//!   search keeps one such path, shortest first.
//!
//! What a gate is, and whether a site comes after one, is the caller's
//! question: the graph knows only nodes and sites.

use std::collections::VecDeque;

use crate::calls::{Callee, UnknownCall};

/// Where a search starts: the node an entry point runs, and whether the
/// entry is authorised before it runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Seed {
    pub node: u32,
    /// The caller's id for the entry point.
    pub entry: u32,
    pub gated: bool,
}

/// The call sites of many routines.
#[derive(Debug, Clone, Default)]
pub struct ReachGraph {
    edges: Vec<Vec<(u32, Callee)>>,
}

impl ReachGraph {
    /// A graph of `nodes` routines and no calls.
    pub fn new(nodes: usize) -> Self {
        ReachGraph {
            edges: vec![Vec::new(); nodes],
        }
    }

    /// The number of routines.
    pub fn len(&self) -> usize {
        self.edges.len()
    }

    /// Whether the graph has no routine.
    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }

    /// Add the call at `site` (the caller's id for it) from `from`.
    pub fn add(&mut self, from: u32, site: u32, to: Callee) {
        self.edges[from as usize].push((site, to));
    }

    /// The calls of one routine, in the order added.
    pub fn calls(&self, node: u32) -> &[(u32, Callee)] {
        &self.edges[node as usize]
    }

    /// Which nodes the seeds reach, and which entries reach each one, at most
    /// `cap` per node.
    pub fn reach(&self, seeds: &[Seed], cap: usize) -> Reach {
        let n = self.edges.len();
        let mut entries = vec![EntrySet::default(); n];
        let mut queued = vec![false; n];
        let mut queue = VecDeque::new();
        for s in seeds {
            entries[s.node as usize].insert(s.entry, cap);
            entries[s.node as usize].reached = true;
            if !queued[s.node as usize] {
                queued[s.node as usize] = true;
                queue.push_back(s.node);
            }
        }
        let mut unknown = Vec::new();
        while let Some(m) = queue.pop_front() {
            queued[m as usize] = false;
            let from = entries[m as usize].clone();
            for &(_, callee) in &self.edges[m as usize] {
                match callee {
                    Callee::Unknown(r) => {
                        if !unknown.contains(&r) {
                            unknown.push(r);
                        }
                    }
                    Callee::Node(t) => {
                        if entries[t as usize].absorb(&from, cap) && !queued[t as usize] {
                            queued[t as usize] = true;
                            queue.push_back(t);
                        }
                    }
                }
            }
        }
        unknown.sort_unstable();
        Reach { entries, unknown }
    }

    /// The walk over `(node, gated)` states. `gate(node, site)` says whether
    /// `node` passes a gate before its call at `site`.
    pub fn gated(&self, seeds: &[Seed], gate: impl Fn(u32, u32) -> bool) -> Gated {
        let n = self.edges.len();
        let mut out = Gated {
            ungated: vec![None; n],
            gated: vec![None; n],
        };
        let mut queue = VecDeque::new();
        for s in seeds {
            let slot = out.slot(s.node, s.gated);
            if slot.is_none() {
                *slot = Some(Parent::Entry(s.entry));
                queue.push_back((s.node, s.gated));
            }
        }
        while let Some((m, g)) = queue.pop_front() {
            for &(site, callee) in &self.edges[m as usize] {
                let Callee::Node(t) = callee else {
                    continue;
                };
                let next = g || gate(m, site);
                let slot = out.slot(t, next);
                if slot.is_none() {
                    *slot = Some(Parent::Call {
                        from: m,
                        site,
                        gated: g,
                    });
                    queue.push_back((t, next));
                }
            }
        }
        out
    }
}

/// The entries that reach one node.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EntrySet {
    /// Sorted.
    ids: Vec<u32>,
    /// More entries reach the node than the cap.
    more: bool,
    reached: bool,
}

impl EntrySet {
    /// The entries, lowest first, at most the cap.
    pub fn ids(&self) -> &[u32] {
        &self.ids
    }

    /// Whether more entries reach the node than [`ids`](Self::ids) lists.
    pub fn more(&self) -> bool {
        self.more
    }

    fn insert(&mut self, id: u32, cap: usize) -> bool {
        match self.ids.binary_search(&id) {
            Ok(_) => false,
            Err(_) if self.ids.len() >= cap => {
                let changed = !self.more;
                self.more = true;
                changed
            }
            Err(i) => {
                self.ids.insert(i, id);
                true
            }
        }
    }

    fn absorb(&mut self, other: &EntrySet, cap: usize) -> bool {
        let mut changed = !self.reached;
        self.reached = true;
        for &id in &other.ids {
            changed |= self.insert(id, cap);
        }
        if other.more && !self.more {
            self.more = true;
            changed = true;
        }
        changed
    }
}

/// What [`ReachGraph::reach`] finds.
#[derive(Debug, Clone, Default)]
pub struct Reach {
    entries: Vec<EntrySet>,
    unknown: Vec<UnknownCall>,
}

impl Reach {
    /// The entries that reach `node`.
    pub fn entries(&self, node: u32) -> &EntrySet {
        &self.entries[node as usize]
    }

    /// Whether an entry reaches `node`.
    pub fn is_reached(&self, node: u32) -> bool {
        self.entries[node as usize].reached
    }

    /// The reasons of the Unknown calls of the reached nodes, sorted. When
    /// empty, a node that is not reached is unreachable.
    pub fn unknown(&self) -> &[UnknownCall] {
        &self.unknown
    }
}

/// How a search state was first reached.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Parent {
    /// The node is the target of this entry.
    Entry(u32),
    /// A call at `site` of `from`, in the state `gated`.
    Call { from: u32, site: u32, gated: bool },
}

/// One path from an entry to a node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub entry: u32,
    /// The calls on the way, from the entry: `(caller, site)`.
    pub calls: Vec<(u32, u32)>,
}

/// What [`ReachGraph::gated`] finds.
#[derive(Debug, Clone)]
pub struct Gated {
    ungated: Vec<Option<Parent>>,
    gated: Vec<Option<Parent>>,
}

impl Gated {
    fn slot(&mut self, node: u32, gated: bool) -> &mut Option<Parent> {
        if gated {
            &mut self.gated[node as usize]
        } else {
            &mut self.ungated[node as usize]
        }
    }

    /// How `node` is first reached with no gate passed, if it is.
    pub fn ungated(&self, node: u32) -> Option<Parent> {
        self.ungated[node as usize]
    }

    /// How `node` is first reached after a gate, if it is.
    pub fn gated(&self, node: u32) -> Option<Parent> {
        self.gated[node as usize]
    }

    /// The path that first reached `node` in the state `gated`.
    pub fn path(&self, node: u32, gated: bool) -> Option<Path> {
        let mut calls = Vec::new();
        let (mut at, mut state) = (node, gated);
        // A parent is always discovered before its child, so the walk ends.
        for _ in 0..=self.ungated.len() * 2 {
            let parent = if state {
                self.gated[at as usize]
            } else {
                self.ungated[at as usize]
            }?;
            match parent {
                Parent::Entry(entry) => {
                    calls.reverse();
                    return Some(Path { entry, calls });
                }
                Parent::Call { from, site, gated } => {
                    calls.push((from, site));
                    at = from;
                    state = gated;
                }
            }
        }
        None
    }
}
