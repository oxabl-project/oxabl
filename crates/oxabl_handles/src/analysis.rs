//! The per-unit lifetime walk.
//!
//! Each tracked object gets one breadth-first walk of its unit's CFG, from its
//! create node. The walk state is the node, the set of the unit's variables
//! that hold the object on this path, whether the object escaped on this path,
//! and whether the path raised a condition. Holding the variable set per path
//! makes aliases and overwrites exact along each path, which a merged dataflow
//! fact would not.

use std::cell::Cell;
use std::collections::VecDeque;

use oxabl_ast::{Identifier, NodeId};
use oxabl_flow::{
    BlockKind, Cfg, EdgeKind, ExitKind, NodeIx, NodeKind, OpaqueReason as FlowOpaque,
};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::events::{self, Effect, Place, PoolRef};
use crate::index::{FileIndex, Name, Passed};
use crate::model::*;

/// More walk states than this for one object marks it opaque.
const STATE_LIMIT: usize = 100_000;

/// More walk states than this over all the objects of a file marks the rest
/// opaque, so a generated file with many objects stays fast.
const FILE_STATE_BUDGET: usize = 600_000;

/// The walk states a file can still spend.
pub(crate) struct Budget(Cell<usize>);

impl Budget {
    pub(crate) fn new() -> Self {
        Budget(Cell::new(FILE_STATE_BUDGET))
    }

    fn left(&self) -> usize {
        self.0.get()
    }

    fn spend(&self, states: usize) {
        self.0.set(self.0.get().saturating_sub(states));
    }
}

#[derive(Debug, Clone, Copy)]
enum Into {
    Var(u32),
    Other(EscapeKind),
    Nothing,
}

#[derive(Debug, Clone)]
enum Fx {
    Create {
        obj: u32,
        into: Into,
    },
    Free {
        var: u32,
        kind: FreeKind,
    },
    /// `APPLY "CLOSE"`, which frees only a persistent procedure.
    Close {
        var: u32,
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
    Output {
        var: u32,
    },
    DerivedAssign {
        to: Into,
        from: u32,
    },
    DeletePool {
        name: Option<String>,
    },
    /// A delete of a procedure found by walking a procedure list.
    Sweep,
}

/// An `IF` condition's value for a live holder; see `events::Guard`.
#[derive(Debug, Clone, Copy)]
struct GuardFx {
    var: Option<u32>,
    live: Option<bool>,
    created: Option<bool>,
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
    /// `CREATE ... NO-ERROR`: the object may not exist after it.
    no_error: bool,
    /// A MEMPTR a `COPY-LOB` fills.
    weak: bool,
    /// A weak object the allocation of an earlier create already covers.
    duplicate: bool,
    nodes: SmallVec<[NodeIx; 1]>,
}

pub(crate) fn analyze_unit(
    cfg: &Cfg,
    index: &FileIndex,
    src: &str,
    budget: &Budget,
) -> UnitHandles {
    let mut u = UnitWalk::new(cfg, index, src, budget);
    let live: Vec<u32> = (0..u.objs.len() as u32)
        .filter(|&o| !u.objs[o as usize].duplicate)
        .collect();
    let mut objects: Vec<Object> = live.into_iter().map(|o| u.object(o)).collect();
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
    src: &'i str,
    budget: &'i Budget,
    /// Per node: 0 unknown, 1 plain, 2 runs or calls something. Read when a
    /// raise edge is taken, so most nodes are never classified.
    calls: Vec<Cell<u8>>,
    unit_stmt: NodeId,
    vars: Vec<Var>,
    fx: Vec<SmallVec<[Fx; 2]>>,
    guards: Vec<SmallVec<[GuardFx; 1]>>,
    /// The statement cannot raise, though the CFG gives it a raise edge.
    safe: Vec<bool>,
    objs: Vec<Obj>,
    pools: Vec<PoolSite>,
    /// Nodes of `CREATE WIDGET-POOL` with no name.
    unnamed_pool_nodes: Vec<NodeIx>,
    dominators: Option<oxabl_flow::dominators::Dominators>,
}

impl<'c, 'i, 'a> UnitWalk<'c, 'i, 'a> {
    fn new(cfg: &'c Cfg, index: &'i FileIndex<'a>, src: &'i str, budget: &'i Budget) -> Self {
        let unit_stmt = cfg.unit().stmt;
        let mut w = UnitWalk {
            cfg,
            index,
            src,
            budget,
            calls: vec![Cell::new(0); cfg.node_count()],
            unit_stmt,
            vars: Vec::new(),
            fx: Vec::with_capacity(cfg.node_count()),
            guards: vec![SmallVec::new(); cfg.node_count()],
            safe: vec![false; cfg.node_count()],
            objs: Vec::new(),
            pools: Vec::new(),
            unnamed_pool_nodes: Vec::new(),
            dominators: None,
        };
        let empty = FxHashMap::default();
        let facts = index.unit_facts(unit_stmt).unwrap_or(&empty);
        // Variables this unit creates a dataset or a query into, and variables
        // that walk a procedure list.
        let mut datasets: FxHashSet<Name> = FxHashSet::default();
        let mut queries: FxHashSet<Name> = FxHashSet::default();
        let mut walkers: FxHashSet<Name> = FxHashSet::default();
        for e in facts.values().flat_map(|f| &f.effects) {
            match e {
                Effect::Create {
                    kind: ObjectKind::Dataset,
                    into: Some(Place::Var(v)),
                    ..
                } => {
                    datasets.insert(Name(v));
                }
                Effect::Create {
                    kind: ObjectKind::Query,
                    into: Some(Place::Var(v)),
                    ..
                } => {
                    queries.insert(Name(v));
                }
                Effect::ProcedureWalk { var } => {
                    walkers.insert(Name(var));
                }
                _ => {}
            }
        }
        let mut var_ix: FxHashMap<Name, u32> = FxHashMap::default();
        let mut obj_ix: FxHashMap<(NodeId, u32), u32> = FxHashMap::default();
        let mut pool_seen: FxHashMap<NodeId, ()> = FxHashMap::default();
        let mut skipped: Vec<(NodeIx, &'a [Identifier])> = Vec::new();
        for (n, node) in cfg.nodes() {
            let mut out: SmallVec<[Fx; 2]> = SmallVec::new();
            if let (NodeKind::Stmt | NodeKind::Head, Some(stmt)) = (node.kind, node.stmt)
                && let Some(f) = facts.get(&stmt)
            {
                let mut ordinal = 0u32;
                for e in &f.effects {
                    let mut var = |name: &'a str| w.var(&mut var_ix, name);
                    match e {
                        Effect::Create {
                            kind,
                            into,
                            pool,
                            no_error,
                            weak,
                        } => {
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
                                    no_error: *no_error,
                                    weak: *weak,
                                    duplicate: false,
                                    nodes: SmallVec::new(),
                                });
                                w.objs.len() as u32 - 1
                            });
                            w.objs[obj as usize].nodes.push(n);
                            out.push(Fx::Create { obj, into: into_r });
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
                                    no_error: false,
                                    weak: false,
                                    duplicate: false,
                                    nodes: SmallVec::new(),
                                });
                                w.objs.len() as u32 - 1
                            });
                        }
                        Effect::Free { var: v, .. } if walkers.contains(&Name(v)) => {
                            out.push(Fx::Sweep)
                        }
                        Effect::Free {
                            var: v,
                            kind: FreeKind::ApplyClose,
                        } => out.push(Fx::Close { var: var(v) }),
                        Effect::Free { var: v, kind } => out.push(Fx::Free {
                            var: var(v),
                            kind: *kind,
                        }),
                        Effect::Pass {
                            var: v,
                            callee,
                            pos,
                        } => match index.passed(unit_stmt, *callee, *pos) {
                            Passed::Borrow => {}
                            Passed::Frees => out.push(Fx::Escape {
                                var: var(v),
                                kind: EscapeKind::FreedByCallee,
                            }),
                            Passed::Keeps => out.push(Fx::Escape {
                                var: var(v),
                                kind: EscapeKind::KeptByCallee,
                            }),
                            Passed::Unknown => out.push(Fx::Opaque {
                                var: var(v),
                                reason: OpaqueReason::Callee,
                            }),
                        },
                        Effect::CopyLob { .. } => {}
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
                        Effect::OutputArgument { var: v } => out.push(Fx::Output { var: var(v) }),
                        Effect::DerivedAssign { to, from } => {
                            let to = match to {
                                Place::Var(v) => Into::Var(var(v)),
                                Place::Other(k) => Into::Other(*k),
                            };
                            out.push(Fx::DerivedAssign {
                                to,
                                from: var(from),
                            });
                        }
                        Effect::Skipped(names) => skipped.push((n, names)),
                        Effect::Attach { recv, var: v } => match recv.map(Name) {
                            Some(r) if datasets.contains(&r) => out.push(Fx::Escape {
                                var: var(v),
                                kind: EscapeKind::DatasetMember,
                            }),
                            Some(r) if queries.contains(&r) => {}
                            _ => out.push(Fx::Opaque {
                                var: var(v),
                                reason: OpaqueReason::AddBuffer,
                            }),
                        },
                        Effect::ProcedureWalk { .. } => {}
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
                    }
                }
                w.safe[n.index()] = f.safe;
                for g in &f.guards {
                    let var = g.var.map(|v| w.var(&mut var_ix, v));
                    w.guards[n.index()].push(GuardFx {
                        var,
                        live: g.live,
                        created: g.created,
                    });
                }
            }
            w.fx.push(out);
        }
        w.drop_duplicate_creates();
        // A skipped statement matters only where it names a variable that may
        // hold an object.
        for (n, names) in skipped {
            for name in names {
                if let Some(&v) = var_ix.get(&Name(&name.name)) {
                    w.fx[n.index()].push(Fx::Opaque {
                        var: v,
                        reason: OpaqueReason::SkippedStatement,
                    });
                }
            }
        }
        w
    }

    /// A MEMPTR a `COPY-LOB` fills is not a second object where an earlier
    /// create of the same variable (`SET-SIZE`, or another copy) always runs
    /// first: that one already owns the block.
    fn drop_duplicate_creates(&mut self) {
        for o in 0..self.objs.len() {
            let obj = &self.objs[o];
            let (true, Into::Var(v)) = (obj.weak, obj.into) else {
                continue;
            };
            let dom = self.dominators.get_or_insert_with(|| self.cfg.dominators());
            let covered = obj.nodes.iter().all(|&c| {
                self.objs.iter().enumerate().any(|(j, other)| {
                    j != o
                        && !other.duplicate
                        && other.kind == ObjectKind::Memptr
                        && matches!(other.into, Into::Var(w) if w == v)
                        && other.nodes.iter().any(|&p| p != c && dom.dominates(p, c))
                })
            });
            self.objs[o].duplicate = covered;
        }
    }

    fn var(&mut self, ix: &mut FxHashMap<Name<'a>, u32>, name: &'a str) -> u32 {
        let key = Name(name);
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
                if self.index.class_pool {
                    Placement::ClassPool
                } else if self.unit_stmt != NodeId::PROGRAM && self.index.main_pool {
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
        if self.budget.left() == 0 {
            out.verdict = Verdict::Unknown;
            out.opaque.push(Opaque {
                reason: OpaqueReason::TooComplex,
                site: out.create,
            });
            return out;
        }
        Walker::new(self, o, out).run()
    }

    fn calls(&self, n: NodeIx) -> bool {
        let cell = &self.calls[n.index()];
        if cell.get() == 0 {
            let found = events::calls_something(self.src, self.cfg.node(n).span);
            cell.set(if found { 2 } else { 1 });
        }
        cell.get() == 2
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
/// Just after a `NO-ERROR` create, before the next statement: the object
/// exists on this path, so `ERROR-STATUS:ERROR` is false.
const CREATED: u8 = 8;
/// The path took a raise edge out of a node that runs or calls something.
const CALLING: u8 = 16;

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
    /// The bit of each unit variable in a walk state's holder set, assigned
    /// when the variable first holds the object; `u8::MAX` for none.
    local: Vec<u8>,
    next_bit: u8,
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
            local: vec![u8::MAX; u.vars.len()],
            next_bit: 0,
        }
    }

    /// The holder bit of `v`, or 0 when `v` never held the object.
    fn bit(&self, v: u32) -> u64 {
        match self.local[v as usize] {
            u8::MAX => 0,
            b => 1 << b,
        }
    }

    /// The holder bit of `v`, assigned if needed; 0 past 64 holders.
    fn hold(&mut self, v: u32) -> u64 {
        if self.local[v as usize] == u8::MAX {
            if self.next_bit == 64 {
                self.too_complex();
                return 0;
            }
            self.local[v as usize] = self.next_bit;
            self.next_bit += 1;
        }
        self.bit(v)
    }

    fn too_complex(&mut self) {
        if !self
            .out
            .opaque
            .iter()
            .any(|o| o.reason == OpaqueReason::TooComplex)
        {
            let site = self.out.create;
            self.out.opaque.push(Opaque {
                reason: OpaqueReason::TooComplex,
                site,
            });
        }
    }

    fn run(mut self) -> Object {
        let u = self.u;
        let obj = &u.objs[self.o as usize];
        let (holders, flags) = match obj.into {
            Into::Var(v) => {
                let var = &u.vars[v as usize];
                let mut flags = ROOT;
                if let Some(kind) = var.escape {
                    flags |= ESCAPED;
                    for &c in &obj.nodes {
                        self.escape(kind, c, var.freed_elsewhere);
                    }
                }
                (self.hold(v), flags)
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
        let limit = STATE_LIMIT.min(u.budget.left());
        while let Some(i) = self.queue.pop_front() {
            if self.states.len() > limit {
                self.too_complex();
                break;
            }
            self.expand(i);
        }
        u.budget.spend(self.states.len());
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
            // The rest of the create statement runs on the new object.
            let after = self.u.fx[n.index()]
                .iter()
                .position(|f| matches!(f, Fx::Create { obj, .. } if *obj == self.o))
                .map_or(0, |p| p + 1);
            (self.apply(i, n, st, false, after), Err(None))
        } else {
            (
                self.apply(i, n, st, false, 0),
                self.apply(i, n, st, true, 0),
            )
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
            if self.pruned(n, kind, st.flags, s.holders) {
                continue;
            }
            let mut flags = s.flags & !ROOT;
            // Only a later statement can change the error status.
            if cfg.node(n).kind == NodeKind::Stmt {
                flags &= !CREATED;
                if root && self.u.objs[self.o as usize].no_error {
                    flags |= CREATED;
                }
            }
            if is_abnormal(kind) {
                flags |= ABNORMAL;
                if self.u.calls(n) {
                    flags |= CALLING;
                }
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

    /// The edge leaves an `IF` whose condition has the other value while the
    /// object lives: no path with the object live goes there.
    fn pruned(&self, n: NodeIx, kind: EdgeKind, flags: u8, holders: u64) -> bool {
        let edge = match kind {
            EdgeKind::True => true,
            EdgeKind::False => false,
            _ => return false,
        };
        self.u.guards[n.index()].iter().any(|g| {
            let value = if flags & CREATED != 0 {
                g.created.or(g.live)
            } else {
                g.live
            };
            let held = g.var.is_none_or(|v| holders & self.bit(v) != 0);
            held && value.is_some_and(|v| v != edge)
        })
    }

    /// Run node `n`'s effects from the `from`-th on the object. `Err(None)`
    /// stops the path with nothing to record; `Err(Some(end))` stops it with
    /// that end recorded.
    fn apply(
        &mut self,
        i: u32,
        n: NodeIx,
        st: State,
        raised: bool,
        from: usize,
    ) -> Result<State, Option<End>> {
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
        for fx in &u.fx[n.index()][from..] {
            match fx {
                Fx::Create { into, .. } if !raised => {
                    if let Into::Var(v) = into
                        && h & self.bit(*v) != 0
                    {
                        if self.u.objs[self.o as usize].kind == ObjectKind::Memptr {
                            // SET-SIZE on an allocated MEMPTR fails rather than
                            // dropping the memory.
                            continue;
                        }
                        h &= !self.bit(*v);
                        if h == 0 {
                            return lose(self, esc);
                        }
                    }
                }
                Fx::Free { var, kind } => {
                    if h & self.bit(*var) != 0 {
                        self.free(*kind, n);
                        self.end(i, st, n, End::Freed);
                        return Err(Some(End::Freed));
                    }
                }
                Fx::Close { var }
                    if h & self.bit(*var) != 0
                        && u.objs[self.o as usize].kind == ObjectKind::PersistentProcedure =>
                {
                    self.free(FreeKind::ApplyClose, n);
                    self.end(i, st, n, End::Freed);
                    return Err(Some(End::Freed));
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
                    let copy = from.is_some_and(|f| h & self.bit(f) != 0);
                    match *to {
                        Into::Var(w) => {
                            if copy {
                                h |= self.hold(w);
                                let var = &u.vars[w as usize];
                                if let Some(kind) = var.escape {
                                    esc = true;
                                    self.escape(kind, n, var.freed_elsewhere);
                                }
                            } else if h & self.bit(w) != 0 {
                                h &= !self.bit(w);
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
                Fx::Output { var } => {
                    if h & self.bit(*var) != 0 && !raised {
                        if self.u.objs[self.o as usize].kind == ObjectKind::Memptr {
                            self.opaque(OpaqueReason::OutputArgument, n);
                        } else {
                            h &= !self.bit(*var);
                            if h == 0 {
                                return lose(self, esc);
                            }
                        }
                    }
                }
                Fx::DerivedAssign { to, from } => {
                    if h & self.bit(*from) != 0 {
                        match *to {
                            Into::Var(w) => {
                                if u.vars[w as usize].escape.is_some() {
                                    esc = true;
                                    let freed = u.vars[w as usize].freed_elsewhere;
                                    self.escape(EscapeKind::DerivedHandle, n, freed);
                                }
                            }
                            Into::Other(_) => {
                                esc = true;
                                self.escape(EscapeKind::DerivedHandle, n, false);
                            }
                            Into::Nothing => {}
                        }
                    }
                    // The target takes a value that is not the object.
                    if !raised
                        && let Into::Var(w) = *to
                        && h & self.bit(w) != 0
                    {
                        h &= !self.bit(w);
                        if h == 0 {
                            return lose(self, esc);
                        }
                    }
                }
                Fx::Escape { var, kind }
                    if (!raised
                        || matches!(
                            kind,
                            EscapeKind::DerivedHandle
                                | EscapeKind::FreedByCallee
                                | EscapeKind::KeptByCallee
                        ))
                        && h & self.bit(*var) != 0 =>
                {
                    esc = true;
                    let freed = u.vars[*var as usize].freed_elsewhere;
                    self.escape(*kind, n, freed && *kind != EscapeKind::Return);
                }
                Fx::Sweep if u.objs[self.o as usize].kind == ObjectKind::PersistentProcedure => {
                    self.opaque(OpaqueReason::ProcedureSweep, n);
                }
                Fx::Opaque { var, reason } if h & self.bit(*var) != 0 => {
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
                && (0..u.vars.len())
                    .any(|v| st.holders & self.bit(v as u32) != 0 && u.vars[v].freed_elsewhere) =>
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
        match map.entry(key) {
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(leak);
            }
            std::collections::hash_map::Entry::Occupied(mut e) => {
                let have = e.get_mut();
                if have.via_call.is_none() {
                    have.via_call = leak.via_call;
                }
            }
        }
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
        let mut via_call = None;
        let mut at = self.out.create.span;
        for &(n, edge) in &chain {
            let node = cfg.node(n);
            let step = Step {
                span: node.span,
                edge: edge.unwrap_or(EdgeKind::Normal),
            };
            if edge.is_some_and(is_abnormal) {
                via.get_or_insert(step);
                if via_call.is_none() && self.u.calls(n) {
                    via_call = Some(step);
                }
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
            via_call,
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
