//! Transitive effect summaries over the call graph (section 8.6).
//!
//! Each routine is a node of an [`oxabl_index::calls::CallGraph`]. Its
//! summary is its own database effects plus those of every routine it calls,
//! with the call's record-loop depth added to the callee's shapes. The
//! components of the graph are solved callees first; a component with a cycle
//! is iterated to a fixed point, which the depth bound ([`MAX_DEPTH`]) makes
//! finite. A summary is **partial** when an effect may be missing: a call
//! whose target is not known, a unit whose own effects are not all known, or
//! a bound reached. It names every reason.

use oxabl_index::calls::{CallGraph, Callee, UnknownCall};
use oxabl_scope::{Lock, LockMode, Table};

use crate::FileEffects;
use crate::model::{EffectUnknown, Op, UnitEffects};

/// Record-loop depths past this are counted as this.
pub const MAX_DEPTH: u8 = 4;
/// Effects past this many in one summary are dropped, and the summary is
/// partial ([`Partial::Bound`]).
pub const MAX_EFFECTS: usize = 512;
/// Rounds of the fixed point over one component before it stops, partial.
pub const MAX_ROUNDS: usize = 64;

/// The lock of a read in a summary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SummaryLock {
    /// A write.
    None,
    Mode(LockMode),
    /// The default `SHARE-LOCK`.
    Default,
    Unknown,
}

/// One database effect in a summary.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SummaryEffect {
    /// The table name, as defined; a table the schema does not have (or any
    /// table with no schema loaded) starts with `?`.
    pub table: String,
    pub op: Op,
    pub lock: SummaryLock,
    /// The record-loop depth, up to [`MAX_DEPTH`]: 0 once, 1 per row, 2 or
    /// more nested.
    pub depth: u8,
    /// `None` for a direct effect; else the routine called first on the way.
    pub via: Option<u32>,
}

/// Why a summary may miss effects.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Partial {
    /// The unit's own effects are not all known.
    Unit(EffectUnknown),
    /// A call's target is not known.
    Call(UnknownCall),
    /// A bound was reached (effects, rounds, or graph edges).
    Bound,
}

impl Partial {
    /// Kebab-case name used in reports.
    pub fn as_str(self) -> String {
        match self {
            Partial::Unit(u) => format!("unit:{}", u.as_str()),
            Partial::Call(c) => format!("call:{}", c.as_str()),
            Partial::Bound => "bound".to_string(),
        }
    }
}

/// The input for one node: its direct effects and calls.
#[derive(Debug, Clone, Default)]
pub struct Node {
    pub effects: Vec<SummaryEffect>,
    /// Each call with the record-loop depth of its site.
    pub calls: Vec<(Callee, u8)>,
    pub unknown: Vec<EffectUnknown>,
}

impl Node {
    /// The direct facts of one unit. `resolve` maps each call to a node or an
    /// Unknown; a call it maps to `None` is left out (a call that runs in
    /// another session, for one).
    pub fn of_unit(
        file: &FileEffects,
        unit: &UnitEffects,
        mut resolve: impl FnMut(&crate::UnitCall) -> Option<Callee>,
    ) -> Node {
        let mut effects: Vec<SummaryEffect> = file
            .unit_effects(unit.unit)
            .filter_map(|e| {
                let table = match &e.table {
                    Table::Database(n) => n.clone(),
                    Table::Unknown(n) => format!("?{n}"),
                    Table::TempTable { .. } => return None,
                };
                let lock = match e.lock {
                    None => SummaryLock::None,
                    Some(Lock::Explicit(m)) => SummaryLock::Mode(m),
                    Some(Lock::Default) => SummaryLock::Default,
                    Some(Lock::TempTable) => return None,
                    Some(Lock::Unknown(_)) => SummaryLock::Unknown,
                };
                Some(SummaryEffect {
                    table,
                    op: e.op,
                    lock,
                    depth: e.shape.depth().min(MAX_DEPTH),
                    via: None,
                })
            })
            .collect();
        effects.sort();
        effects.dedup();
        let calls = unit
            .calls
            .iter()
            .filter_map(|c| resolve(c).map(|t| (t, c.shape.depth().min(MAX_DEPTH))))
            .collect();
        Node {
            effects,
            calls,
            unknown: unit.unknown.iter().map(|(r, _)| *r).collect(),
        }
    }
}

/// The transitive summary of one node.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Summary {
    /// Sorted, one per table, operation, lock and depth (the first `via`
    /// kept).
    pub effects: Vec<SummaryEffect>,
    /// Sorted reasons the summary may miss effects; empty when complete.
    pub partial: Vec<Partial>,
    /// The node is on a cycle of calls.
    pub recursive: bool,
}

impl Summary {
    /// Whether the summary is known to hold no database write: complete, and
    /// no create, update or delete.
    pub fn known_no_writes(&self) -> bool {
        self.partial.is_empty() && !self.effects.iter().any(|e| e.op.writes())
    }

    /// Whether any effect writes the database.
    pub fn writes(&self) -> bool {
        self.effects.iter().any(|e| e.op.writes())
    }
}

/// Solve every node's summary. `nodes` and `graph` are indexed alike; the
/// graph's edges are the nodes' calls (see [`graph_of`]).
pub fn summarize(nodes: &[Node], graph: &CallGraph) -> Vec<Summary> {
    let mut out: Vec<Summary> = vec![Summary::default(); nodes.len()];
    for comp in graph.components() {
        let recursive = comp.len() > 1
            || nodes[comp[0] as usize]
                .calls
                .iter()
                .any(|(c, _)| *c == Callee::Node(comp[0]));
        for &n in &comp {
            out[n as usize] = base(&nodes[n as usize]);
            out[n as usize].recursive = recursive;
        }
        let mut rounds = 0;
        loop {
            let mut changed = false;
            for &n in &comp {
                let next = step(n, nodes, &out, recursive);
                if next != out[n as usize] {
                    out[n as usize] = next;
                    changed = true;
                }
            }
            rounds += 1;
            if !changed || !recursive {
                break;
            }
            if rounds >= MAX_ROUNDS {
                for &n in &comp {
                    add_partial(&mut out[n as usize], Partial::Bound);
                }
                break;
            }
        }
    }
    if graph.truncated() {
        for s in &mut out {
            add_partial(s, Partial::Bound);
        }
    }
    out
}

/// The graph of the nodes' calls.
pub fn graph_of(nodes: &[Node]) -> CallGraph {
    let mut g = CallGraph::new(nodes.len());
    for (i, n) in nodes.iter().enumerate() {
        for (c, _) in &n.calls {
            g.add(i as u32, *c);
        }
    }
    g
}

fn add_partial(s: &mut Summary, p: Partial) {
    if let Err(i) = s.partial.binary_search(&p) {
        s.partial.insert(i, p);
    }
}

fn base(node: &Node) -> Summary {
    let mut s = Summary {
        effects: node.effects.clone(),
        ..Summary::default()
    };
    for u in &node.unknown {
        add_partial(&mut s, Partial::Unit(*u));
    }
    s
}

fn step(n: u32, nodes: &[Node], solved: &[Summary], recursive: bool) -> Summary {
    let node = &nodes[n as usize];
    let mut s = base(node);
    s.recursive = recursive;
    for &(callee, depth) in &node.calls {
        match callee {
            Callee::Unknown(r) => add_partial(&mut s, Partial::Call(r)),
            Callee::Node(m) => {
                let c = &solved[m as usize];
                for p in &c.partial {
                    add_partial(&mut s, *p);
                }
                for e in &c.effects {
                    s.effects.push(SummaryEffect {
                        depth: (e.depth + depth).min(MAX_DEPTH),
                        via: Some(m),
                        ..e.clone()
                    });
                }
            }
        }
    }
    // One per table, operation, lock and depth: a direct effect before an
    // inherited one, then the lowest via.
    s.effects.sort_by(|a, b| {
        (&a.table, a.op, a.lock, a.depth, a.via.is_some(), a.via).cmp(&(
            &b.table,
            b.op,
            b.lock,
            b.depth,
            b.via.is_some(),
            b.via,
        ))
    });
    s.effects.dedup_by(|b, a| {
        a.table == b.table && a.op == b.op && a.lock == b.lock && a.depth == b.depth
    });
    if s.effects.len() > MAX_EFFECTS {
        s.effects.truncate(MAX_EFFECTS);
        add_partial(&mut s, Partial::Bound);
    }
    s
}
