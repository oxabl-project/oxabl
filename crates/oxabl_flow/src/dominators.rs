//! Dominator and post-dominator trees (Cooper, Harvey and Kennedy's
//! iterative algorithm).
//!
//! `a` dominates `b` when every path from the entry to `b` passes `a`; `a`
//! post-dominates `b` when every path from `b` to a unit exit passes `a`.
//! Exits of every kind count, so a node skipped on an error path does not
//! post-dominate. Nodes that reach no exit (an endless loop) have no
//! post-dominator.

use crate::model::{Cfg, NodeIx};

/// A dominator (or post-dominator) tree.
#[derive(Debug, Clone)]
pub struct Dominators {
    idom: Vec<Option<NodeIx>>,
    reached: Vec<bool>,
}

impl Dominators {
    /// The immediate dominator of `n`; `None` for a root or an unreached node.
    pub fn immediate(&self, n: NodeIx) -> Option<NodeIx> {
        self.idom[n.index()]
    }

    /// Whether the analysis reached `n` (from the entry, or back from an
    /// exit).
    pub fn is_reached(&self, n: NodeIx) -> bool {
        self.reached[n.index()]
    }

    /// Whether `a` dominates `b`. Every reached node dominates itself.
    pub fn dominates(&self, a: NodeIx, b: NodeIx) -> bool {
        if !self.is_reached(b) {
            return false;
        }
        let mut cur = Some(b);
        while let Some(c) = cur {
            if c == a {
                return true;
            }
            cur = self.idom[c.index()];
        }
        false
    }
}

impl Cfg {
    /// The dominator tree from the entry.
    pub fn dominators(&self) -> Dominators {
        let roots = [self.entry()];
        compute(
            self.node_count(),
            &roots,
            |n| self.successors(n).map(|e| e.to).collect(),
            |n| self.predecessors(n).map(|e| e.from).collect(),
        )
    }

    /// The post-dominator tree from every exit.
    pub fn post_dominators(&self) -> Dominators {
        let roots: Vec<NodeIx> = self.exits().map(|(_, n)| n).collect();
        compute(
            self.node_count(),
            &roots,
            |n| self.predecessors(n).map(|e| e.from).collect(),
            |n| self.successors(n).map(|e| e.to).collect(),
        )
    }
}

/// Dominators over the graph `succ`/`pred` from `roots`, joined under one
/// virtual root.
fn compute(
    count: usize,
    roots: &[NodeIx],
    succ: impl Fn(NodeIx) -> Vec<NodeIx>,
    pred: impl Fn(NodeIx) -> Vec<NodeIx>,
) -> Dominators {
    const UNSET: usize = usize::MAX;
    let virt = count;
    let succ_of = |v: usize| -> Vec<usize> {
        if v == virt {
            roots.iter().map(|r| r.index()).collect()
        } else {
            succ(NodeIx(v as u32)).iter().map(|n| n.index()).collect()
        }
    };

    // Post-order from the virtual root.
    let mut rpo_num = vec![UNSET; count + 1];
    let mut post: Vec<usize> = Vec::with_capacity(count + 1);
    let mut visited = vec![false; count + 1];
    let mut stack: Vec<(usize, Vec<usize>, usize)> = vec![(virt, succ_of(virt), 0)];
    visited[virt] = true;
    while let Some((v, succs, i)) = stack.last_mut() {
        if let Some(&s) = succs.get(*i) {
            *i += 1;
            if !visited[s] {
                visited[s] = true;
                let next = succ_of(s);
                stack.push((s, next, 0));
            }
        } else {
            post.push(*v);
            stack.pop();
        }
    }
    let rpo: Vec<usize> = post.iter().rev().copied().collect();
    for (i, &v) in rpo.iter().enumerate() {
        rpo_num[v] = i;
    }

    let mut idom = vec![UNSET; count + 1];
    idom[virt] = virt;
    let preds_of = |v: usize| -> Vec<usize> {
        let mut p: Vec<usize> = pred(NodeIx(v as u32)).iter().map(|n| n.index()).collect();
        if roots.iter().any(|r| r.index() == v) {
            p.push(virt);
        }
        p
    };
    let intersect = |idom: &[usize], mut a: usize, mut b: usize| {
        while a != b {
            while rpo_num[a] > rpo_num[b] {
                a = idom[a];
            }
            while rpo_num[b] > rpo_num[a] {
                b = idom[b];
            }
        }
        a
    };
    let mut changed = true;
    while changed {
        changed = false;
        for &v in rpo.iter().skip(1) {
            let mut new = UNSET;
            for p in preds_of(v) {
                if rpo_num[p] == UNSET || idom[p] == UNSET {
                    continue;
                }
                new = if new == UNSET {
                    p
                } else {
                    intersect(&idom, p, new)
                };
            }
            if new != UNSET && idom[v] != new {
                idom[v] = new;
                changed = true;
            }
        }
    }

    Dominators {
        idom: (0..count)
            .map(|v| match idom[v] {
                UNSET => None,
                d if d == virt => None,
                d => Some(NodeIx(d as u32)),
            })
            .collect(),
        reached: (0..count).map(|v| visited[v]).collect(),
    }
}
