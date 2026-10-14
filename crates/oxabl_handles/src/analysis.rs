//! The per-unit lifetime walk.
//!
//! Each tracked object gets one breadth-first walk of its unit's CFG, from its
//! create node. The walk state is the node, the set of the unit's variables
//! that hold the object on this path, whether the object escaped on this path,
//! and whether the path raised a condition. Holding the variable set per path
//! makes aliases and overwrites exact along each path, which a merged dataflow
//! fact would not.

use std::collections::VecDeque;

use oxabl_ast::{NodeId, StatementKind as SK};
use oxabl_flow::{
    BlockKind, Cfg, EdgeKind, ExitKind, NodeIx, NodeKind, OpaqueReason as FlowOpaque,
};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::events::{self, Effect, Place, PoolRef};
use crate::index::FileIndex;
use crate::model::*;

/// More walk states than this for one object marks it opaque.
const STATE_LIMIT: usize = 100_000;

#[derive(Debug, Clone, Copy)]
enum Into {
    Var(u32),
    Other(EscapeKind),
    Nothing,
}

#[derive(Debug, Clone)]
enum Fx {
    Create {
        into: Into,
    },
    Free {
        var: u32,
        kind: FreeKind,
    },
    Assign {
        to: Into,
        from: Option<u32>,
    },
    Escape {
        var: u32,
        kind: EscapeKind,
    },
    Opaque {
        var: u32,
        reason: OpaqueReason,
    },
    DeletePool {
        name: Option<String>,
    },
    /// A delete of a procedure found by walking a procedure list.
    Sweep,
}

struct Var {
    escape: Option<EscapeKind>,
    freed_elsewhere: bool,
}

struct Obj {
    kind: ObjectKind,
    site: Site,
    holder: Option<String>,
    into: Into,
    pool: PoolRef,
    nodes: SmallVec<[NodeIx; 1]>,
}

pub(crate) fn analyze_unit(cfg: &Cfg, index: &FileIndex) -> UnitHandles {
    let mut u = UnitWalk::new(cfg, index);
    let mut objects: Vec<Object> = (0..u.objs.len()).map(|o| u.object(o as u32)).collect();
    objects.sort_by_key(|o| o.create.span.start);
    UnitHandles {
        unit: cfg.id(),
        kind: cfg.unit().kind,
        name: cfg.unit().name.clone(),
        span: cfg.unit().span,
        objects,
        pools: u.pools,
    }
}

struct UnitWalk<'c, 'i, 'a> {
    cfg: &'c Cfg,
    index: &'i FileIndex<'a>,
    unit_stmt: NodeId,
    vars: Vec<Var>,
    fx: Vec<SmallVec<[Fx; 2]>>,
    guards: Vec<Option<(u32, bool)>>,
    /// The statement cannot raise, though the CFG gives it a raise edge.
    safe: Vec<bool>,
    objs: Vec<Obj>,
    pools: Vec<PoolSite>,
    /// Nodes of `CREATE WIDGET-POOL` with no name.
    unnamed_pool_nodes: Vec<NodeIx>,
    dominators: Option<oxabl_flow::dominators::Dominators>,
}

impl<'c, 'i, 'a> UnitWalk<'c, 'i, 'a> {
    fn new(cfg: &'c Cfg, index: &'i FileIndex<'a>) -> Self {
        let unit_stmt = cfg.unit().stmt;
        let mut w = UnitWalk {
            cfg,
            index,
            unit_stmt,
            vars: Vec::new(),
            fx: Vec::with_capacity(cfg.node_count()),
            guards: vec![None; cfg.node_count()],
            safe: vec![false; cfg.node_count()],
            objs: Vec::new(),
            pools: Vec::new(),
            unnamed_pool_nodes: Vec::new(),
            dominators: None,
        };
        let mut var_ix: FxHashMap<String, u32> = FxHashMap::default();
        let mut obj_ix: FxHashMap<(NodeId, u32), u32> = FxHashMap::default();
        let mut pool_seen: FxHashMap<NodeId, ()> = FxHashMap::default();
        // Variables this unit creates a dataset into, and variables that walk
        // a procedure list.
        let mut datasets: Vec<String> = Vec::new();
        let mut walkers: Vec<String> = Vec::new();
        for (_, node) in cfg.nodes() {
            if let (NodeKind::Stmt, Some(stmt)) = (node.kind, node.stmt) {
                for e in index.effects(stmt) {
                    if let Effect::Create {
                        kind: ObjectKind::Dataset,
                        into: Some(Place::Var(v)),
                        ..
                    } = e
                    {
                        datasets.push(v.to_ascii_lowercase());
                    }
                    if let Effect::ProcedureWalk { var } = e {
                        walkers.push(var.to_ascii_lowercase());
                    }
                }
            }
        }
        for (n, node) in cfg.nodes() {
            let mut out: SmallVec<[Fx; 2]> = SmallVec::new();
            if let (NodeKind::Stmt, Some(stmt)) = (node.kind, node.stmt) {
                let mut ordinal = 0u32;
                for e in index.effects(stmt) {
                    let mut var = |name: &str| w.var(&mut var_ix, name);
                    match e {
                        Effect::Create { kind, into, pool } => {
                            let into_r = match into {
                                Some(Place::Var(v)) => Into::Var(var(v)),
                                Some(Place::Other(k)) => Into::Other(*k),
                                None => Into::Nothing,
                            };
                            let key = (stmt, ordinal);
                            ordinal += 1;
                            let obj = *obj_ix.entry(key).or_insert_with(|| {
                                w.objs.push(Obj {
                                    kind: *kind,
                                    site: Site {
                                        stmt,
                                        span: node.span,
                                    },
                                    holder: match into {
                                        Some(Place::Var(v)) => Some((*v).to_string()),
                                        _ => None,
                                    },
                                    into: into_r,
                                    pool: pool.clone(),
                                    nodes: SmallVec::new(),
                                });
                                w.objs.len() as u32 - 1
                            });
                            w.objs[obj as usize].nodes.push(n);
                            out.push(Fx::Create { into: into_r });
                        }
                        Effect::Collected => {
                            let key = (stmt, ordinal);
                            ordinal += 1;
                            obj_ix.entry(key).or_insert_with(|| {
                                w.objs.push(Obj {
                                    kind: ObjectKind::ClassInstance,
                                    site: Site {
                                        stmt,
                                        span: node.span,
                                    },
                                    holder: None,
                                    into: Into::Nothing,
                                    pool: PoolRef::Default,
                                    nodes: SmallVec::new(),
                                });
                                w.objs.len() as u32 - 1
                            });
                        }
                        Effect::Free { var: v, .. }
                            if walkers.contains(&v.to_ascii_lowercase()) =>
                        {
                            out.push(Fx::Sweep)
                        }
                        Effect::Free { var: v, kind } => out.push(Fx::Free {
                            var: var(v),
                            kind: *kind,
                        }),
                        Effect::ProcedureWalk { .. } => {}
                        Effect::Assign { to, from } => {
                            let to = match to {
                                Place::Var(v) => Into::Var(var(v)),
                                Place::Other(k) => Into::Other(*k),
                            };
                            out.push(Fx::Assign {
                                to,
                                from: from.map(&mut var),
                            });
                        }
                        Effect::Escape { var: v, kind } => out.push(Fx::Escape {
                            var: var(v),
                            kind: *kind,
                        }),
                        Effect::Opaque { var: v, reason } => out.push(Fx::Opaque {
                            var: var(v),
                            reason: *reason,
                        }),
                        Effect::CreatePool { name, persistent } => {
                            if name.is_none() {
                                w.unnamed_pool_nodes.push(n);
                            }
                            if pool_seen.insert(stmt, ()).is_none() {
                                w.pools.push(PoolSite {
                                    site: Site {
                                        stmt,
                                        span: node.span,
                                    },
                                    name: name.clone(),
                                    persistent: *persistent,
                                });
                            }
                        }
                        Effect::DeletePool { name } => {
                            out.push(Fx::DeletePool { name: name.clone() })
                        }
                        Effect::Attach { recv, var: v } => {
                            if datasets.contains(&recv.to_ascii_lowercase()) {
                                out.push(Fx::Escape {
                                    var: var(v),
                                    kind: EscapeKind::DatasetMember,
                                });
                            }
                        }
                    }
                }
                if let Some(s) = index.stmt(stmt) {
                    w.safe[n.index()] = events::cannot_raise(s);
                }
                if let Some(s) = index.stmt(stmt)
                    && let SK::If { condition, .. } = &s.kind
                    && let Some((v, when)) = events::invalid_test(condition)
                {
                    w.guards[n.index()] = Some((w.var(&mut var_ix, v), when));
                }
            }
            w.fx.push(out);
        }
        w
    }

    fn var(&mut self, ix: &mut FxHashMap<String, u32>, name: &str) -> u32 {
        let key = name.to_ascii_lowercase();
        if let Some(&i) = ix.get(&key) {
            return i;
        }
        let i = self.vars.len() as u32;
        self.vars.push(Var {
            escape: self.index.escape_of(self.unit_stmt, name),
            freed_elsewhere: self.index.freed_elsewhere(self.unit_stmt, name),
        });
        ix.insert(key, i);
        i
    }

    fn placement(&mut self, o: u32) -> Placement {
        let obj = &self.objs[o as usize];
        if !obj.kind.uses_widget_pool() {
            return Placement::NotPooled;
        }
        match &obj.pool {
            PoolRef::Named(name) => match self.index.named_pools.get(name) {
                Some(&persistent) => Placement::NamedPool {
                    name: name.clone(),
                    persistent,
                },
                None => Placement::ForeignPool { name: name.clone() },
            },
            PoolRef::Default => {
                if !self.unnamed_pool_nodes.is_empty() {
                    let dom = self.dominators.get_or_insert_with(|| self.cfg.dominators());
                    let covered = obj.nodes.iter().all(|&c| {
                        self.unnamed_pool_nodes
                            .iter()
                            .any(|&p| p != c && dom.dominates(p, c))
                    });
                    if covered {
                        return Placement::UnitPool;
                    }
                }
                if self.unit_stmt != NodeId::PROGRAM && self.index.main_pool {
                    Placement::FilePool
                } else {
                    Placement::Unscoped
                }
            }
        }
    }

    fn object(&mut self, o: u32) -> Object {
        let placement = self.placement(o);
        let obj = &self.objs[o as usize];
        let mut out = Object {
            kind: obj.kind,
            create: obj.site,
            holder: obj.holder.clone(),
            placement,
            frees: Vec::new(),
            escapes: Vec::new(),
            leaks: Vec::new(),
            opaque: Vec::new(),
            verdict: Verdict::Collected,
        };
        if obj.kind.is_collected() {
            return out;
        }
        if matches!(obj.into, Into::Nothing) {
            out.verdict = Verdict::Uncaptured;
            return out;
        }
        Walker::new(self, o, out).run()
    }

    fn in_finally(&self, n: NodeIx) -> bool {
        let mut b = Some(self.cfg.node(n).block);
        while let Some(cur) = b {
            let block = self.cfg.block(cur);
            if block.kind == BlockKind::Finally {
                return true;
            }
            b = block.parent;
        }
        false
    }

    fn site(&self, n: NodeIx) -> Site {
        let node = self.cfg.node(n);
        Site {
            stmt: node.stmt.unwrap_or(NodeId::DUMMY),
            span: node.span,
        }
    }
}

const ABNORMAL: u8 = 1;
const ESCAPED: u8 = 2;
/// The create node itself, before any effect; told apart from a later
/// arrival at the same node (a loop back to the create).
const ROOT: u8 = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    node: u32,
    flags: u8,
    holders: u64,
}

enum End {
    Freed,
    /// The last holder was overwritten.
    Lost,
}

struct Walker<'w, 'c, 'i, 'a> {
    u: &'w UnitWalk<'c, 'i, 'a>,
    o: u32,
    out: Object,
    states: Vec<(State, Option<(u32, EdgeKind)>)>,
    seen: FxHashMap<State, u32>,
    queue: VecDeque<u32>,
    normal_leaks: FxHashMap<(LeakKind, u32), Leak>,
    abnormal_leaks: FxHashMap<(LeakKind, u32), Leak>,
    escaped: bool,
    pool_freed: bool,
    freed: bool,
}

fn bit(v: u32) -> u64 {
    if v < 64 { 1u64 << v } else { 0 }
}

fn is_abnormal(kind: EdgeKind) -> bool {
    matches!(
        kind,
        EdgeKind::Raise(_) | EdgeKind::Throw | EdgeKind::ReturnError | EdgeKind::Catch
    )
}

impl<'w, 'c, 'i, 'a> Walker<'w, 'c, 'i, 'a> {
    fn new(u: &'w UnitWalk<'c, 'i, 'a>, o: u32, out: Object) -> Self {
        Walker {
            u,
            o,
            out,
            states: Vec::new(),
            seen: FxHashMap::default(),
            queue: VecDeque::new(),
            normal_leaks: FxHashMap::default(),
            abnormal_leaks: FxHashMap::default(),
            escaped: false,
            pool_freed: false,
            freed: false,
        }
    }

    fn run(mut self) -> Object {
        let u = self.u;
        let obj = &u.objs[self.o as usize];
        let (holders, mut flags) = match obj.into {
            Into::Var(v) => {
                let var = &u.vars[v as usize];
                let mut flags = ROOT;
                if let Some(kind) = var.escape {
                    flags |= ESCAPED;
                    for &c in &obj.nodes {
                        self.escape(kind, c, var.freed_elsewhere);
                    }
                }
                (bit(v), flags)
            }
            Into::Other(kind) => {
                for &c in &obj.nodes {
                    self.escape(kind, c, false);
                }
                self.escaped = true;
                return self.finish();
            }
            Into::Nothing => unreachable!("uncaptured objects are not walked"),
        };
        if holders == 0 {
            // More variables than a walk state holds: nothing to follow.
            flags |= ESCAPED;
        }
        for &c in &obj.nodes {
            self.push(
                State {
                    node: c.index() as u32,
                    flags,
                    holders,
                },
                None,
            );
        }
        while let Some(i) = self.queue.pop_front() {
            if self.states.len() > STATE_LIMIT {
                let site = self.out.create;
                self.out.opaque.push(Opaque {
                    reason: OpaqueReason::TooComplex,
                    site,
                });
                break;
            }
            self.expand(i);
        }
        self.finish()
    }

    fn push(&mut self, s: State, parent: Option<(u32, EdgeKind)>) {
        if self.seen.contains_key(&s) {
            return;
        }
        let i = self.states.len() as u32;
        self.states.push((s, parent));
        self.seen.insert(s, i);
        self.queue.push_back(i);
    }

    fn expand(&mut self, i: u32) {
        let st = self.states[i as usize].0;
        let n = NodeIx::from_index(st.node as usize);
        let cfg = self.u.cfg;
        if let NodeKind::Exit(kind) = cfg.node(n).kind {
            self.at_exit(i, st, kind);
            return;
        }
        let root = st.flags & ROOT != 0;
        let (normal, raised) = if root {
            (Ok(st), Err(None))
        } else {
            (self.apply(i, n, st, false), self.apply(i, n, st, true))
        };
        let succ: SmallVec<[(NodeIx, EdgeKind); 4]> =
            cfg.successors(n).map(|e| (e.to, e.kind)).collect();
        for (to, kind) in succ {
            if self.u.safe[n.index()] && matches!(kind, EdgeKind::Raise(_)) {
                continue;
            }
            let next = if matches!(kind, EdgeKind::Raise(_)) {
                &raised
            } else {
                &normal
            };
            let Ok(s) = next else { continue };
            if self.pruned(n, kind, s.holders) {
                continue;
            }
            let mut flags = s.flags & !ROOT;
            if is_abnormal(kind) {
                flags |= ABNORMAL;
            }
            self.push(
                State {
                    node: to.index() as u32,
                    flags,
                    holders: s.holders,
                },
                Some((i, kind)),
            );
        }
    }

    /// The edge leaves an `IF` that tests a holder for an invalid handle, on
    /// the branch where it is invalid: no path with the object live goes there.
    fn pruned(&self, n: NodeIx, kind: EdgeKind, holders: u64) -> bool {
        let Some((v, when)) = self.u.guards[n.index()] else {
            return false;
        };
        holders & bit(v) != 0
            && match kind {
                EdgeKind::True => when,
                EdgeKind::False => !when,
                _ => false,
            }
    }

    /// Run node `n`'s effects on the object. `Err(None)` stops the path with
    /// nothing to record; `Err(Some(end))` stops it with that end recorded.
    fn apply(&mut self, i: u32, n: NodeIx, st: State, raised: bool) -> Result<State, Option<End>> {
        let u = self.u;
        if let Some(reason) = u.cfg.node(n).opaque {
            let reason = match reason {
                FlowOpaque::SkippedStatement => None,
                FlowOpaque::UnexpandedInclude => Some(OpaqueReason::UnexpandedInclude),
                FlowOpaque::UnrecognizedStatement => Some(OpaqueReason::UnrecognizedStatement),
                FlowOpaque::StrayHandlerBlock => Some(OpaqueReason::StrayHandlerBlock),
                FlowOpaque::UnresolvedLabel => None,
            };
            if let Some(reason) = reason {
                self.opaque(reason, n);
            }
        }
        let mut h = st.holders;
        let mut esc = st.flags & ESCAPED != 0;
        let lose = |this: &mut Self, esc: bool| -> Result<State, Option<End>> {
            if esc {
                this.escaped = true;
                Err(None)
            } else {
                this.end(i, st, n, End::Lost);
                Err(Some(End::Lost))
            }
        };
        for fx in &u.fx[n.index()] {
            match fx {
                Fx::Create { into, .. } if !raised => {
                    if let Into::Var(v) = into
                        && h & bit(*v) != 0
                    {
                        if self.u.objs[self.o as usize].kind == ObjectKind::Memptr {
                            // SET-SIZE on an allocated MEMPTR fails rather than
                            // dropping the memory.
                            continue;
                        }
                        h &= !bit(*v);
                        if h == 0 {
                            return lose(self, esc);
                        }
                    }
                }
                Fx::Free { var, kind } => {
                    if h & bit(*var) != 0 {
                        self.free(*kind, n);
                        self.end(i, st, n, End::Freed);
                        return Err(Some(End::Freed));
                    }
                }
                Fx::DeletePool { name } => {
                    let hit = match (name, &self.out.placement) {
                        (None, Placement::UnitPool | Placement::FilePool) => true,
                        (Some(n), Placement::NamedPool { name, .. })
                        | (Some(n), Placement::ForeignPool { name }) => n == name,
                        _ => false,
                    };
                    if hit {
                        self.free(FreeKind::DeletePool, n);
                        self.end(i, st, n, End::Freed);
                        return Err(Some(End::Freed));
                    }
                }
                Fx::Assign { to, from } if !raised => {
                    let copy = from.is_some_and(|f| h & bit(f) != 0);
                    match *to {
                        Into::Var(w) => {
                            if copy {
                                h |= bit(w);
                                let var = &u.vars[w as usize];
                                if let Some(kind) = var.escape {
                                    esc = true;
                                    self.escape(kind, n, var.freed_elsewhere);
                                }
                            } else if h & bit(w) != 0 {
                                h &= !bit(w);
                                if h == 0 {
                                    return lose(self, esc);
                                }
                            }
                        }
                        Into::Other(kind) => {
                            if copy {
                                esc = true;
                                self.escape(kind, n, false);
                            }
                        }
                        Into::Nothing => {}
                    }
                }
                Fx::Escape { var, kind } if !raised && h & bit(*var) != 0 => {
                    esc = true;
                    let freed = u.vars[*var as usize].freed_elsewhere;
                    self.escape(*kind, n, freed && *kind != EscapeKind::Return);
                }
                Fx::Sweep if u.objs[self.o as usize].kind == ObjectKind::PersistentProcedure => {
                    self.opaque(OpaqueReason::ProcedureSweep, n);
                }
                Fx::Opaque { var, reason } if h & bit(*var) != 0 => {
                    self.opaque(*reason, n);
                }
                _ => {}
            }
        }
        let mut flags = st.flags;
        if esc {
            flags |= ESCAPED;
        }
        Ok(State {
            node: st.node,
            flags,
            holders: h,
        })
    }

    fn at_exit(&mut self, i: u32, st: State, kind: ExitKind) {
        if st.flags & ESCAPED != 0 {
            self.escaped = true;
            return;
        }
        let u = self.u;
        let placement = &self.out.placement;
        if placement.freed_with_procedure() {
            self.pool_freed = true;
            return;
        }
        let escape = match placement {
            Placement::ForeignPool { .. } => Some(EscapeKind::ForeignPool),
            Placement::NamedPool {
                name,
                persistent: true,
            } if u.index.deleted_pools.contains(name) => Some(EscapeKind::PersistentPool),
            // A main-block variable another unit frees: the internal procedure
            // or trigger that frees it owns the object.
            _ if u.unit_stmt == NodeId::PROGRAM
                && (0..u.vars.len().min(64))
                    .any(|v| st.holders & (1 << v) != 0 && u.vars[v].freed_elsewhere) =>
            {
                Some(EscapeKind::FreedByOtherUnit)
            }
            _ => None,
        };
        if let Some(k) = escape {
            self.escaped = true;
            let site = self.out.create;
            self.push_escape(k, site, k == EscapeKind::FreedByOtherUnit);
            return;
        }
        let leak = self.leak(i, LeakKind::Exit(kind));
        self.record_leak((LeakKind::Exit(kind), 0), st, leak);
    }

    fn end(&mut self, i: u32, st: State, n: NodeIx, end: End) {
        match end {
            End::Freed => self.freed = true,
            End::Lost => {
                if self.out.placement.freed_with_procedure() {
                    self.pool_freed = true;
                    return;
                }
                let leak = self.leak(i, LeakKind::Overwrite);
                self.record_leak((LeakKind::Overwrite, n.index() as u32), st, leak);
            }
        }
    }

    fn record_leak(&mut self, key: (LeakKind, u32), st: State, leak: Leak) {
        let map = if st.flags & ABNORMAL != 0 {
            &mut self.abnormal_leaks
        } else {
            &mut self.normal_leaks
        };
        map.entry(key).or_insert(leak);
    }

    /// The path from the create to state `i`, where the object is lost.
    fn leak(&self, i: u32, kind: LeakKind) -> Leak {
        let cfg = self.u.cfg;
        let mut chain: Vec<(NodeIx, Option<EdgeKind>)> = Vec::new();
        let mut cur = i;
        let mut leaving = None;
        loop {
            let (st, parent) = self.states[cur as usize];
            chain.push((NodeIx::from_index(st.node as usize), leaving));
            match parent {
                Some((p, k)) => {
                    leaving = Some(k);
                    cur = p;
                }
                None => break,
            }
        }
        chain.reverse();
        let mut path = Vec::new();
        let mut via = None;
        let mut at = self.out.create.span;
        for &(n, edge) in &chain {
            let node = cfg.node(n);
            let step = Step {
                span: node.span,
                edge: edge.unwrap_or(EdgeKind::Normal),
            };
            if edge.is_some_and(is_abnormal) {
                via.get_or_insert(step);
            }
            if node.kind == NodeKind::Stmt {
                at = node.span;
            }
            if matches!(node.kind, NodeKind::Stmt | NodeKind::Head) {
                path.push(step);
            }
        }
        Leak {
            kind,
            at,
            normal: self.states[i as usize].0.flags & ABNORMAL == 0,
            via,
            path,
        }
    }

    fn free(&mut self, kind: FreeKind, n: NodeIx) {
        let site = self.u.site(n);
        if !self
            .out
            .frees
            .iter()
            .any(|f| f.site.stmt == site.stmt && f.kind == kind)
        {
            let in_finally = self.u.in_finally(n);
            self.out.frees.push(Free {
                kind,
                site,
                in_finally,
            });
        }
    }

    fn escape(&mut self, kind: EscapeKind, n: NodeIx, freed_elsewhere: bool) {
        let site = self.u.site(n);
        self.push_escape(kind, site, freed_elsewhere);
    }

    fn push_escape(&mut self, kind: EscapeKind, site: Site, freed_elsewhere: bool) {
        if !self
            .out
            .escapes
            .iter()
            .any(|e| e.site.stmt == site.stmt && e.kind == kind)
        {
            self.out.escapes.push(Escape {
                kind,
                site,
                freed_elsewhere,
            });
        }
    }

    fn opaque(&mut self, reason: OpaqueReason, n: NodeIx) {
        let site = self.u.site(n);
        if !self
            .out
            .opaque
            .iter()
            .any(|e| e.site.stmt == site.stmt && e.reason == reason)
        {
            self.out.opaque.push(Opaque { reason, site });
        }
    }

    fn finish(mut self) -> Object {
        let mut leaks: Vec<(LeakKind, u32, Leak)> = Vec::new();
        for ((kind, at), leak) in self.normal_leaks.drain() {
            leaks.push((kind, at, leak));
        }
        for ((kind, at), leak) in self.abnormal_leaks.drain() {
            if !leaks.iter().any(|(k, a, _)| *k == kind && *a == at) {
                leaks.push((kind, at, leak));
            }
        }
        leaks.sort_by_key(|(kind, at, l)| (l.at.start, *kind, *at));
        self.out.leaks = leaks.into_iter().map(|(_, _, l)| l).collect();
        self.out.frees.sort_by_key(|f| (f.site.span.start, f.kind));
        self.out
            .escapes
            .sort_by_key(|e| (e.site.span.start, e.kind));
        self.out
            .opaque
            .sort_by_key(|e| (e.site.span.start, e.reason));
        self.out.verdict = if !self.out.leaks.is_empty() {
            if self.out.opaque.is_empty() {
                Verdict::Leaks
            } else {
                Verdict::Unknown
            }
        } else if self
            .out
            .opaque
            .iter()
            .any(|o| o.reason == OpaqueReason::TooComplex)
        {
            Verdict::Unknown
        } else if self.escaped {
            Verdict::Escapes
        } else if self.pool_freed {
            Verdict::PoolFreed
        } else if self.freed {
            Verdict::Freed
        } else {
            let site = self.out.create;
            self.out.opaque.push(Opaque {
                reason: OpaqueReason::NoExitPath,
                site,
            });
            Verdict::Unknown
        };
        self.out
    }
}
