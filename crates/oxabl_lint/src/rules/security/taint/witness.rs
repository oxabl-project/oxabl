//! The path a finding shows: from the sink back to its source.
//!
//! The walk takes the slot of the operand that holds the origin, finds the
//! nearest earlier write of that slot that carries the origin, and goes on
//! with the slot that write read, until the origin itself. A slot with no
//! write in the unit is file-level: the step points at a write elsewhere in
//! the file and the walk ends at the source.

use std::collections::VecDeque;

use oxabl_flow::dataflow::{BitSet, Solution};
use oxabl_flow::provenance::Provenance;
use oxabl_flow::{Cfg, NodeIx};

use super::FileTaint;
use super::unit::{UnitIr, apply};
use super::value::{Env, Origins, Value};

/// The most steps of a path, the source excluded.
const MAX_STEPS: usize = 8;

/// One step of a path.
#[derive(Debug, Clone)]
pub(crate) struct Step {
    /// Where it happens, when the file holds it.
    pub span: Option<oxabl_ast::Span>,
    /// The label text.
    pub label: String,
    /// The step in the message: the source, or a slot name.
    pub chain: String,
}

/// The path from `origin` to the operand `value` read at `at` in `starts`,
/// where the facts are `fact`. Source first.
#[allow(clippy::too_many_arguments)]
pub(super) fn path(
    ft: &FileTaint,
    cfg: &Cfg,
    starts: &[NodeIx],
    at: u32,
    value: &Value,
    fact: &Provenance,
    origin: u32,
    local: &Origins,
) -> Vec<Step> {
    let u = cfg.id().index();
    let (ir, Some(sol)) = (&ft.units[u], &ft.solutions[u]) else {
        return Vec::new();
    };
    let source = ft.origins.get(origin).or_else(|| local.get(origin));
    let mut steps = Vec::new();
    let global = |name: &str, key: &str| Step {
        span: ft.summaries.sites.get(key).copied(),
        label: format!("`{name}` is set from untrusted input in another routine"),
        chain: format!("`{name}`"),
    };
    let mut slot = None;
    if !value.holds_origin(origin) {
        slot = slot_with(value, fact, origin);
        if slot.is_none()
            && let Some((name, key)) = value.file_with(&ft.summaries, origin)
        {
            steps.push(global(name, key));
        }
    }
    let mut starts = starts.to_vec();
    let mut limit = at;
    while let Some(s) = slot.take() {
        if steps.len() == MAX_STEPS {
            break;
        }
        let info = &ir.slots.info[s as usize];
        let Some((node, k)) = find_def(ft, cfg, ir, sol, &starts, limit, s, origin) else {
            if info.global {
                steps.push(global(&info.name, &info.key));
            }
            break;
        };
        let op = &ir.ops[node.index()][k];
        if source.is_none_or(|o| o.span != op.span) {
            steps.push(Step {
                span: Some(op.span),
                label: format!("`{}` takes the untrusted value", info.name),
                chain: format!("`{}`", info.name),
            });
        }
        if op.value.holds_origin(origin) {
            break;
        }
        if let Some((name, key)) = op.value.file_with(&ft.summaries, origin) {
            steps.push(global(name, key));
            break;
        }
        slot = slot_with(&op.value, &fact_before(ft, sol, ir, node, k), origin);
        starts = vec![node];
        limit = op.span.start;
    }
    if let Some(o) = source {
        steps.push(Step {
            span: Some(o.span),
            label: format!("untrusted source: {}", o.what),
            chain: o.what.clone(),
        });
    }
    steps.reverse();
    steps
}

/// A slot that `value` reads and that holds `origin` in `fact`.
fn slot_with(value: &Value, fact: &Provenance, origin: u32) -> Option<u32> {
    let mut slots = Vec::new();
    value.slots(&mut slots);
    slots
        .into_iter()
        .find(|&s| fact.get(s as usize).iter().any(|(o, _)| *o == origin))
}

/// The fact before the `k`-th write of `node`.
fn fact_before(
    ft: &FileTaint,
    sol: &Solution<Provenance>,
    ir: &UnitIr,
    node: NodeIx,
    k: usize,
) -> Provenance {
    let mut f = sol.before(node).clone();
    for op in &ir.ops[node.index()][..k] {
        apply(op, &mut f, Env::new(&ft.summaries));
    }
    f
}

/// Whether the `k`-th write of `node` puts `origin` into its slot.
fn writes(
    ft: &FileTaint,
    sol: &Solution<Provenance>,
    ir: &UnitIr,
    node: NodeIx,
    k: usize,
    origin: u32,
) -> bool {
    let (labels, _) = ir.ops[node.index()][k]
        .value
        .eval(&fact_before(ft, sol, ir, node, k), Env::new(&ft.summaries));
    labels.iter().any(|(o, _)| *o == origin)
}

/// The nearest write of `slot` before `starts` (before `limit` within them)
/// that carries `origin`.
#[allow(clippy::too_many_arguments)]
fn find_def(
    ft: &FileTaint,
    cfg: &Cfg,
    ir: &UnitIr,
    sol: &Solution<Provenance>,
    starts: &[NodeIx],
    limit: u32,
    slot: u32,
    origin: u32,
) -> Option<(NodeIx, usize)> {
    let hit = |n: NodeIx, within: bool| {
        ir.ops[n.index()]
            .iter()
            .enumerate()
            .rev()
            .find(|(k, op)| {
                op.slot == slot
                    && (!within || op.span.end <= limit)
                    && writes(ft, sol, ir, n, *k, origin)
            })
            .map(|(k, _)| (n, k))
    };
    for &n in starts {
        if let Some(found) = hit(n, true) {
            return Some(found);
        }
    }
    let mut seen = BitSet::new_empty(cfg.node_count());
    let mut queue: VecDeque<NodeIx> = VecDeque::new();
    for &n in starts {
        for e in cfg.predecessors(n) {
            if seen.insert(e.from.index()) {
                queue.push_back(e.from);
            }
        }
    }
    while let Some(m) = queue.pop_front() {
        if let Some(found) = hit(m, false) {
            return Some(found);
        }
        // A replacing write without the origin ends this path.
        if ir.ops[m.index()]
            .iter()
            .any(|op| op.slot == slot && op.strong)
        {
            continue;
        }
        for e in cfg.predecessors(m) {
            if seen.insert(e.from.index()) {
                queue.push_back(e.from);
            }
        }
    }
    None
}
