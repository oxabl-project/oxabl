//! Graph access and path queries on a [`Cfg`].

use std::collections::VecDeque;

use oxabl_ast::NodeId;

use crate::dataflow::BitSet;
use crate::model::*;

impl Cfg {
    /// This unit's id in its [`FileFlow`](crate::FileFlow).
    pub fn id(&self) -> UnitId {
        self.id
    }

    /// What this CFG covers.
    pub fn unit(&self) -> &Unit {
        &self.unit
    }

    pub fn entry(&self) -> NodeIx {
        self.entry
    }

    /// The exit node of a kind, if any path can leave that way.
    pub fn exit(&self, kind: ExitKind) -> Option<NodeIx> {
        self.exits[kind as usize]
    }

    /// Every exit node with its kind.
    pub fn exits(&self) -> impl Iterator<Item = (ExitKind, NodeIx)> + '_ {
        [
            ExitKind::Return,
            ExitKind::Error,
            ExitKind::Stop,
            ExitKind::Quit,
        ]
        .into_iter()
        .filter_map(|k| self.exit(k).map(|n| (k, n)))
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn node(&self, n: NodeIx) -> &Node {
        &self.nodes[n.index()]
    }

    pub fn nodes(&self) -> impl Iterator<Item = (NodeIx, &Node)> {
        self.nodes
            .iter()
            .enumerate()
            .map(|(i, n)| (NodeIx(i as u32), n))
    }

    pub fn block_count(&self) -> usize {
        self.blocks.len()
    }

    pub fn block(&self, b: BlockId) -> &Block {
        &self.blocks[b.index()]
    }

    pub fn blocks(&self) -> impl Iterator<Item = (BlockId, &Block)> {
        self.blocks
            .iter()
            .enumerate()
            .map(|(i, b)| (BlockId(i as u32), b))
    }

    pub fn edges(&self) -> &[Edge] {
        &self.edges
    }

    /// Outgoing edges of `n`, in creation order.
    pub fn successors(&self, n: NodeIx) -> impl Iterator<Item = &Edge> {
        self.succ[n.index()]
            .iter()
            .map(|&e| &self.edges[e as usize])
    }

    /// Incoming edges of `n`.
    pub fn predecessors(&self, n: NodeIx) -> impl Iterator<Item = &Edge> {
        self.pred[n.index()]
            .iter()
            .map(|&e| &self.edges[e as usize])
    }

    /// The nodes of a statement: one per copy for a statement in a `FINALLY`,
    /// the [`NodeKind::Head`] nodes for a block statement, none for a statement
    /// of another unit or a definition (`PROCEDURE`, `METHOD`, ...).
    pub fn nodes_of(&self, stmt: NodeId) -> &[NodeIx] {
        self.stmt_nodes.get(&stmt).map_or(&[], |v| v.as_slice())
    }

    /// The block a block statement (or the unit statement) opens.
    pub fn block_of(&self, stmt: NodeId) -> Option<BlockId> {
        self.stmt_block.get(&stmt).copied()
    }

    /// Opaque nodes with their reasons.
    pub fn opaque_nodes(&self) -> impl Iterator<Item = (NodeIx, OpaqueReason)> + '_ {
        self.nodes()
            .filter_map(|(i, n)| n.opaque.map(|reason| (i, reason)))
    }

    /// Whether any node is opaque.
    pub fn is_opaque(&self) -> bool {
        self.nodes.iter().any(|n| n.opaque.is_some())
    }

    /// Whether `inner` is `outer` or nested in it.
    pub fn block_contains(&self, outer: BlockId, inner: BlockId) -> bool {
        let mut b = Some(inner);
        while let Some(cur) = b {
            if cur == outer {
                return true;
            }
            b = self.blocks[cur.index()].parent;
        }
        false
    }

    /// Whether node `n` is inside block `b` (an [`NodeKind::After`] node is in
    /// the enclosing block).
    pub fn node_in_block(&self, n: NodeIx, b: BlockId) -> bool {
        self.block_contains(b, self.nodes[n.index()].block)
    }

    /// Nodes reachable from the entry.
    pub fn reachable(&self) -> BitSet {
        self.reachable_from(&[self.entry], |_| false)
    }

    /// Nodes reachable from `starts` without entering an `avoid` node.
    pub fn reachable_from(&self, starts: &[NodeIx], avoid: impl Fn(NodeIx) -> bool) -> BitSet {
        let mut seen = BitSet::new_empty(self.nodes.len());
        let mut stack: Vec<NodeIx> = starts.to_vec();
        for s in starts {
            seen.insert(s.index());
        }
        while let Some(n) = stack.pop() {
            for e in self.successors(n) {
                if !avoid(e.to) && seen.insert(e.to.index()) {
                    stack.push(e.to);
                }
            }
        }
        seen
    }

    /// Statement nodes no path from the entry reaches: dead code.
    pub fn unreachable_stmts(&self) -> Vec<NodeIx> {
        let live = self.reachable();
        self.nodes()
            .filter(|(i, n)| n.kind == NodeKind::Stmt && !live.contains(i.index()))
            .map(|(i, _)| i)
            .collect()
    }

    /// A shortest path from `from` to a node satisfying `goal` that enters no
    /// `avoid` node, or `None` when every path to a goal passes an `avoid`
    /// node. The path includes both ends.
    ///
    /// "Is every path from a create to a unit exit through a delete?" is
    /// `find_path(create, is_exit, is_delete).is_none()`, and the path is the
    /// witness when it is not.
    pub fn find_path(
        &self,
        from: NodeIx,
        goal: impl Fn(NodeIx) -> bool,
        avoid: impl Fn(NodeIx) -> bool,
    ) -> Option<Vec<NodeIx>> {
        let mut parent: Vec<Option<NodeIx>> = vec![None; self.nodes.len()];
        let mut seen = BitSet::new_empty(self.nodes.len());
        let mut queue = VecDeque::from([from]);
        seen.insert(from.index());
        while let Some(n) = queue.pop_front() {
            if n != from && goal(n) {
                let mut path = vec![n];
                let mut cur = n;
                while let Some(p) = parent[cur.index()] {
                    path.push(p);
                    cur = p;
                }
                path.reverse();
                return Some(path);
            }
            for e in self.successors(n) {
                if !avoid(e.to) && seen.insert(e.to.index()) {
                    parent[e.to.index()] = Some(n);
                    queue.push_back(e.to);
                }
            }
        }
        None
    }

    /// Edges that leave block `b`: from a node inside it to one outside. Each
    /// edge's kind tells how (normal end, `LEAVE`, `RETURN`, a raised error,
    /// ...). Exits through a `FINALLY` leave from that `FINALLY`'s end.
    pub fn block_exits(&self, b: BlockId) -> Vec<&Edge> {
        self.edges
            .iter()
            .filter(|e| self.node_in_block(e.from, b) && !self.node_in_block(e.to, b))
            .collect()
    }

    /// Whether every path that enters block `b` and leaves it, by any exit,
    /// passes one of `through`. Paths that never leave (an endless loop) do not
    /// count. "Does the `DELETE OBJECT` run on every exit, errors included?"
    /// is `runs_on_every_exit(b, cfg.nodes_of(delete_stmt))`.
    pub fn runs_on_every_exit(&self, b: BlockId, through: &[NodeIx]) -> bool {
        let starts: Vec<NodeIx> = if b == BlockId::ROUTINE {
            vec![self.entry]
        } else {
            self.nodes()
                .filter(|(_, n)| n.kind == NodeKind::Head && n.block == b)
                .map(|(i, _)| i)
                .collect()
        };
        let left = |n: NodeIx| {
            matches!(self.nodes[n.index()].kind, NodeKind::Exit(_)) || !self.node_in_block(n, b)
        };
        starts.iter().all(|&s| {
            through.contains(&s) || self.find_path(s, left, |n| through.contains(&n)).is_none()
        })
    }
}
