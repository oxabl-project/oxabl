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
//!
//! # Termination
//!
//! At the target of every edge that goes back in the visiting order (a loop
//! head) the solver joins the node's previous fact into the new one. With
//! monotone transfers this changes nothing; with a transfer that is not, the
//! facts at the loop heads still only grow, so a lattice of finite height
//! reaches a fixed point. [`solve_within`] also bounds the work and reports
//! when the bound is hit.

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

    /// The work of one visit of a node whose joined fact is `fact`, charged
    /// against [`Limits::work`]. A visit clones, joins and compares facts, so a
    /// fact that grows with the unit should return its size here. Defaults to
    /// 1.
    fn fact_cost(&self, fact: &Self::Fact) -> usize {
        let _ = fact;
        1
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
    match solve_within(cfg, analysis, usize::MAX) {
        Ok(solution) => solution,
        Err(Exhausted) => unreachable!("an unbounded budget is never spent"),
    }
}

/// The work [`solve_within`] did not finish within its budget.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Exhausted;

/// A budget for [`solve_within`]: 64 visits per node.
pub fn default_budget(cfg: &Cfg) -> usize {
    cfg.node_count().saturating_mul(64)
}

/// The bounds of [`solve_bounded`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Limits {
    /// The most node visits.
    pub visits: usize,
    /// The most work: the sum of [`Analysis::fact_cost`] over the visits.
    pub work: usize,
}

/// Solve `analysis` over `cfg` to a fixed point, visiting at most `budget`
/// nodes. A caller that gets [`Exhausted`] has no answer for the unit.
pub fn solve_within<A: Analysis>(
    cfg: &Cfg,
    analysis: &A,
    budget: usize,
) -> Result<Solution<A::Fact>, Exhausted> {
    solve_bounded(
        cfg,
        analysis,
        Limits {
            visits: budget,
            work: usize::MAX,
        },
    )
}

/// Solve `analysis` over `cfg` to a fixed point within `limits`. A caller
/// that gets [`Exhausted`] has no answer for the unit.
pub fn solve_bounded<A: Analysis>(
    cfg: &Cfg,
    analysis: &A,
    limits: Limits,
) -> Result<Solution<A::Fact>, Exhausted> {
    match analysis.direction() {
        Direction::Forward => solve_forward(cfg, analysis, limits),
        Direction::Backward => solve_backward(cfg, analysis, limits),
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

/// The targets of the edges that go back in `order`: every cycle has one.
fn heads(cfg: &Cfg, order: &[NodeIx], forward: bool) -> BitSet {
    let mut position = vec![0; cfg.node_count()];
    for (i, n) in order.iter().enumerate() {
        position[n.index()] = i;
    }
    let mut out = BitSet::new_empty(cfg.node_count());
    for &n in order {
        for e in cfg.successors(n) {
            let (from, to) = if forward {
                (e.from, e.to)
            } else {
                (e.to, e.from)
            };
            if position[from.index()] >= position[to.index()] {
                out.insert(to.index());
            }
        }
    }
    out
}

fn solve_forward<A: Analysis>(
    cfg: &Cfg,
    a: &A,
    limits: Limits,
) -> Result<Solution<A::Fact>, Exhausted> {
    let Limits {
        visits: mut budget,
        work: mut work_left,
    } = limits;
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
    let order = order(cfg);
    let heads = heads(cfg, &order, true);
    let mut work: VecDeque<NodeIx> = order.into();
    while let Some(node) = work.pop_front() {
        budget = budget.checked_sub(1).ok_or(Exhausted)?;
        queued.remove(node.index());
        let mut fact = start(node);
        for e in cfg.predecessors(node) {
            let mut incoming = before[e.from.index()].clone();
            a.transfer_edge(cfg, e.from, e, &mut incoming);
            a.join(&mut fact, &incoming);
        }
        if heads.contains(node.index()) {
            a.join(&mut fact, &before[node.index()]);
        }
        work_left = work_left.checked_sub(a.fact_cost(&fact)).ok_or(Exhausted)?;
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
    Ok(Solution { before, after })
}

fn solve_backward<A: Analysis>(
    cfg: &Cfg,
    a: &A,
    limits: Limits,
) -> Result<Solution<A::Fact>, Exhausted> {
    let Limits {
        visits: mut budget,
        work: mut work_left,
    } = limits;
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
    let order: Vec<NodeIx> = order(cfg).into_iter().rev().collect();
    let heads = heads(cfg, &order, false);
    let mut work: VecDeque<NodeIx> = order.into();
    while let Some(node) = work.pop_front() {
        budget = budget.checked_sub(1).ok_or(Exhausted)?;
        queued.remove(node.index());
        let mut fact = start(node);
        for e in cfg.successors(node) {
            let mut outgoing = before[e.to.index()].clone();
            a.transfer_edge(cfg, node, e, &mut outgoing);
            a.join(&mut fact, &outgoing);
        }
        if heads.contains(node.index()) {
            a.join(&mut fact, &before[node.index()]);
        }
        work_left = work_left.checked_sub(a.fact_cost(&fact)).ok_or(Exhausted)?;
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
    Ok(Solution { before, after })
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
        self.words.iter().enumerate().flat_map(|(w, &word)| {
            let mut bits = word;
            std::iter::from_fn(move || {
                (bits != 0).then(|| {
                    let b = bits.trailing_zeros() as usize;
                    bits &= bits - 1;
                    w * 64 + b
                })
            })
        })
    }
}
