//! A worklist dataflow solver over a [`Cfg`].
//!
//! Implement [`Analysis`] and call [`solve`]. The fact type is any lattice
//! value; [`BitSet`] covers the usual "set of variables" or "set of handles".
//!
//! * A **may** analysis ("some path") joins with union and starts every node at
//!   the empty set.
//! * A **must** analysis ("every path", e.g. definite assignment) joins with
//!   intersection and starts every node at the full set, so a node's fact only
//!   shrinks as paths reach it. A node no path reaches keeps the full set.
//!
//! Raise edges leave a statement that did not complete. Override
//! [`Analysis::transfer_edge`] to treat them differently from the normal edges
//! (for example, a variable assigned by `x = f().` is not assigned on its
//! [`EdgeKind::Raise`](crate::EdgeKind::Raise) edge).

use std::collections::VecDeque;

use crate::model::{Cfg, Edge, NodeIx, NodeKind};

/// Which way facts flow.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    /// From the entry along edges: facts about the past (assignment, values).
    Forward,
    /// From the exits against edges: facts about the future (liveness).
    Backward,
}

/// A dataflow problem.
pub trait Analysis {
    type Fact: Clone + PartialEq;

    fn direction(&self) -> Direction;

    /// The start value of every node: the identity of [`join`](Self::join).
    fn initial(&self, cfg: &Cfg) -> Self::Fact;

    /// The fact at the entry (forward) or at every exit (backward).
    fn boundary(&self, cfg: &Cfg) -> Self::Fact;

    /// Merge `other` into `acc`: union for a may analysis, intersection for a
    /// must analysis.
    fn join(&self, acc: &mut Self::Fact, other: &Self::Fact);

    /// The effect of executing `node`.
    fn transfer(&self, cfg: &Cfg, node: NodeIx, fact: &mut Self::Fact);

    /// The effect of executing `node` when control leaves it by `edge`.
    /// Defaults to [`transfer`](Self::transfer).
    fn transfer_edge(&self, cfg: &Cfg, node: NodeIx, edge: &Edge, fact: &mut Self::Fact) {
        let _ = edge;
        self.transfer(cfg, node, fact);
    }
}

/// The solved facts of every node.
#[derive(Debug, Clone)]
pub struct Solution<F> {
    before: Vec<F>,
    after: Vec<F>,
}

impl<F> Solution<F> {
    /// The fact at the point just before `n` executes.
    pub fn before(&self, n: NodeIx) -> &F {
        &self.before[n.index()]
    }

    /// The fact at the point just after `n` executes. For a forward analysis
    /// this is [`Analysis::transfer`] of [`before`](Self::before); for a
    /// backward one it is the join over the successors.
    pub fn after(&self, n: NodeIx) -> &F {
        &self.after[n.index()]
    }
}

/// Solve `analysis` over `cfg` to a fixed point.
pub fn solve<A: Analysis>(cfg: &Cfg, analysis: &A) -> Solution<A::Fact> {
    match analysis.direction() {
        Direction::Forward => solve_forward(cfg, analysis),
        Direction::Backward => solve_backward(cfg, analysis),
    }
}

/// Nodes in reverse post-order from the entry, then the unreachable rest.
fn order(cfg: &Cfg) -> Vec<NodeIx> {
    let n = cfg.node_count();
    let mut seen = BitSet::new_empty(n);
    let mut post = Vec::with_capacity(n);
    let mut stack: Vec<(NodeIx, usize)> = vec![(cfg.entry(), 0)];
    seen.insert(cfg.entry().index());
    while let Some((node, i)) = stack.last_mut() {
        let node = *node;
        let succ = cfg.successors(node).nth(*i).map(|e| e.to);
        *i += 1;
        match succ {
            Some(s) if seen.insert(s.index()) => stack.push((s, 0)),
            Some(_) => {}
            None => {
                post.push(node);
                stack.pop();
            }
        }
    }
    post.reverse();
    post.extend(
        (0..n)
            .filter(|&i| !seen.contains(i))
            .map(|i| NodeIx(i as u32)),
    );
    post
}

fn solve_forward<A: Analysis>(cfg: &Cfg, a: &A) -> Solution<A::Fact> {
    let n = cfg.node_count();
    let initial = a.initial(cfg);
    let boundary = a.boundary(cfg);
    let start = |node: NodeIx| {
        if node == cfg.entry() {
            boundary.clone()
        } else {
            initial.clone()
        }
    };
    let mut before: Vec<A::Fact> = (0..n).map(|i| start(NodeIx(i as u32))).collect();
    let mut queued = BitSet::new_full(n);
    let mut work: VecDeque<NodeIx> = order(cfg).into();
    while let Some(node) = work.pop_front() {
        queued.remove(node.index());
        let mut fact = start(node);
        for e in cfg.predecessors(node) {
            let mut incoming = before[e.from.index()].clone();
            a.transfer_edge(cfg, e.from, e, &mut incoming);
            a.join(&mut fact, &incoming);
        }
        if fact != before[node.index()] {
            before[node.index()] = fact;
            for e in cfg.successors(node) {
                if queued.insert(e.to.index()) {
                    work.push_back(e.to);
                }
            }
        }
    }
    let after = (0..n)
        .map(|i| {
            let mut f = before[i].clone();
            a.transfer(cfg, NodeIx(i as u32), &mut f);
            f
        })
        .collect();
    Solution { before, after }
}

fn solve_backward<A: Analysis>(cfg: &Cfg, a: &A) -> Solution<A::Fact> {
    let n = cfg.node_count();
    let initial = a.initial(cfg);
    let boundary = a.boundary(cfg);
    let is_exit = |node: NodeIx| matches!(cfg.node(node).kind, NodeKind::Exit(_));
    let start = |node: NodeIx| {
        if is_exit(node) {
            boundary.clone()
        } else {
            initial.clone()
        }
    };
    let mut before: Vec<A::Fact> = (0..n).map(|i| start(NodeIx(i as u32))).collect();
    let mut queued = BitSet::new_full(n);
    let mut work: VecDeque<NodeIx> = order(cfg).into_iter().rev().collect();
    while let Some(node) = work.pop_front() {
        queued.remove(node.index());
        let mut fact = start(node);
        for e in cfg.successors(node) {
            let mut outgoing = before[e.to.index()].clone();
            a.transfer_edge(cfg, node, e, &mut outgoing);
            a.join(&mut fact, &outgoing);
        }
        if fact != before[node.index()] {
            before[node.index()] = fact;
            for e in cfg.predecessors(node) {
                if queued.insert(e.from.index()) {
                    work.push_back(e.from);
                }
            }
        }
    }
    let after = (0..n)
        .map(|i| {
            let node = NodeIx(i as u32);
            let mut f = start(node);
            for e in cfg.successors(node) {
                a.join(&mut f, &before[e.to.index()]);
            }
            f
        })
        .collect();
    Solution { before, after }
}

/// A fixed-size set of small integers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitSet {
    words: Vec<u64>,
    len: usize,
}

impl BitSet {
    /// An empty set over `0..len`.
    pub fn new_empty(len: usize) -> Self {
        BitSet {
            words: vec![0; len.div_ceil(64)],
            len,
        }
    }

    /// The full set `0..len`.
    pub fn new_full(len: usize) -> Self {
        let mut s = BitSet {
            words: vec![u64::MAX; len.div_ceil(64)],
            len,
        };
        if !len.is_multiple_of(64)
            && let Some(last) = s.words.last_mut()
        {
            *last = (1u64 << (len % 64)) - 1;
        }
        s
    }

    /// The size of the universe.
    pub fn universe(&self) -> usize {
        self.len
    }

    pub fn contains(&self, i: usize) -> bool {
        i < self.len && self.words[i / 64] & (1 << (i % 64)) != 0
    }

    /// Add `i`; `true` if it was not there.
    pub fn insert(&mut self, i: usize) -> bool {
        let (w, bit) = (i / 64, 1u64 << (i % 64));
        let was = self.words[w] & bit != 0;
        self.words[w] |= bit;
        !was
    }

    /// Remove `i`; `true` if it was there.
    pub fn remove(&mut self, i: usize) -> bool {
        let (w, bit) = (i / 64, 1u64 << (i % 64));
        let was = self.words[w] & bit != 0;
        self.words[w] &= !bit;
        was
    }

    /// Union in place; `true` if `self` changed.
    pub fn union_with(&mut self, other: &BitSet) -> bool {
        let mut changed = false;
        for (a, b) in self.words.iter_mut().zip(&other.words) {
            let new = *a | b;
            changed |= new != *a;
            *a = new;
        }
        changed
    }

    /// Intersection in place; `true` if `self` changed.
    pub fn intersect_with(&mut self, other: &BitSet) -> bool {
        let mut changed = false;
        for (a, b) in self.words.iter_mut().zip(&other.words) {
            let new = *a & b;
            changed |= new != *a;
            *a = new;
        }
        changed
    }

    pub fn is_empty(&self) -> bool {
        self.words.iter().all(|&w| w == 0)
    }

    pub fn count(&self) -> usize {
        self.words.iter().map(|w| w.count_ones() as usize).sum()
    }

    /// Members in increasing order.
    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.len).filter(|&i| self.contains(i))
    }
}
