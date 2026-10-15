//! Buffer scopes from the resolved references (section 7.2).
//!
//! Each strong and weak block is a candidate scope. A free reference that no
//! strong or weak block of its buffer encloses is a candidate at the nearest
//! block with record scoping, or at the owner's routine block when it comes
//! from another unit. Candidates are then merged outermost first: a free
//! candidate absorbs every candidate its block encloses (raising it), a strong
//! one absorbs the weak ones inside it, and anything else nested is a conflict.

use oxabl_ast::Span;
use oxabl_flow::{BlockId, BlockKind, FileFlow, UnitId};
use rustc_hash::FxHashMap;

use crate::collect::Role;
use crate::model::{BufferKind, Scope, ScopeKind, Unknown, UnknownReason};
use crate::resolve::Resolved;

/// Candidates past this many for one buffer make its scopes Unknown.
const MAX_CANDIDATES: usize = 2048;

struct Candidate {
    unit: UnitId,
    block: BlockId,
    role: Role,
    at: Span,
    /// From a unit other than the owner.
    cross: bool,
    refs: Vec<usize>,
    /// The candidate that absorbed this one.
    into: Option<usize>,
}

/// Compute every buffer's scopes. Returns, for each reference, the index of
/// its scope in its buffer's `seen` list.
pub(crate) fn assign(res: &mut Resolved, flow: &FileFlow) -> Vec<Option<usize>> {
    let mut by_buffer: Vec<Vec<usize>> = vec![Vec::new(); res.buffers.len()];
    for (i, r) in res.refs.iter().enumerate() {
        by_buffer[r.buffer.index()].push(i);
    }
    let mut scope_of = vec![None; res.refs.len()];
    for (b, refs) in by_buffer.iter().enumerate() {
        res.buffers[b].references = refs.len() as u32;
        if res.buffers[b].is_external() {
            continue;
        }
        buffer_scopes(res, flow, b, refs, &mut scope_of);
    }
    scope_of
}

fn unit_depth(flow: &FileFlow, unit: UnitId) -> usize {
    std::iter::successors(Some(unit), |u| flow.unit(*u).unit().parent).count()
}

fn block_depth(flow: &FileFlow, unit: UnitId, block: BlockId) -> usize {
    let cfg = flow.unit(unit);
    std::iter::successors(Some(block), |b| cfg.block(*b).parent).count()
}

/// Whether `outer` (a unit) encloses `inner` and is not it.
fn unit_encloses(flow: &FileFlow, outer: UnitId, inner: UnitId) -> bool {
    std::iter::successors(flow.unit(inner).unit().parent, |u| {
        flow.unit(*u).unit().parent
    })
    .any(|u| u == outer)
}

/// Whether candidate block `a` encloses block `b` (and is not it).
fn encloses(flow: &FileFlow, a: (UnitId, BlockId), b: (UnitId, BlockId)) -> bool {
    if a == b {
        return false;
    }
    if a.0 == b.0 {
        return flow.unit(a.0).block_contains(a.1, b.1);
    }
    a.1 == BlockId::ROUTINE && unit_encloses(flow, a.0, b.0)
}

/// The nearest block with record scoping, from `block` up.
fn scoping_block(flow: &FileFlow, unit: UnitId, block: BlockId) -> BlockId {
    let cfg = flow.unit(unit);
    let mut b = block;
    loop {
        let bl = cfg.block(b);
        match bl.kind {
            BlockKind::Routine(_) | BlockKind::Repeat | BlockKind::For(_) => return b,
            BlockKind::Do | BlockKind::Catch | BlockKind::Finally => {}
        }
        match bl.parent {
            Some(p) => b = p,
            None => return b,
        }
    }
}

fn buffer_scopes(
    res: &mut Resolved,
    flow: &FileFlow,
    b: usize,
    refs: &[usize],
    scope_of: &mut [Option<usize>],
) {
    let owner = res.buffers[b].owner;
    let mut cands: Vec<Candidate> = Vec::new();
    let mut index: FxHashMap<(UnitId, BlockId, bool), usize> = FxHashMap::default();
    // Strong and weak blocks first, so free references can find them.
    for &i in refs {
        let r = &res.refs[i];
        if r.role == Role::Free {
            continue;
        }
        let key = (r.unit, r.block, true);
        match index.get(&key) {
            Some(&c) => cands[c].refs.push(i),
            None => {
                index.insert(key, cands.len());
                cands.push(Candidate {
                    unit: r.unit,
                    block: r.block,
                    role: r.role,
                    at: r.at,
                    cross: false,
                    refs: vec![i],
                    into: None,
                });
            }
        }
    }
    if res.buffers[b].kind == BufferKind::NewShared {
        let at = res.buffers[b].defined.unwrap_or(Span::DUMMY);
        index.insert((owner, BlockId::ROUTINE, false), cands.len());
        cands.push(Candidate {
            unit: owner,
            block: BlockId::ROUTINE,
            role: Role::Free,
            at,
            cross: false,
            refs: Vec::new(),
            into: None,
        });
    }
    let held: Vec<usize> = (0..cands.len()).collect();
    for &i in refs {
        let r = &res.refs[i];
        if r.role != Role::Free {
            continue;
        }
        // The innermost strong or weak block of the buffer around it.
        let cover = held
            .iter()
            .copied()
            .filter(|&c| cands[c].role != Role::Free)
            .filter(|&c| {
                cands[c].unit == r.unit && flow.unit(r.unit).block_contains(cands[c].block, r.block)
            })
            .max_by_key(|&c| block_depth(flow, r.unit, cands[c].block));
        if let Some(c) = cover {
            cands[c].refs.push(i);
            continue;
        }
        let (unit, block, cross) = if r.unit == owner {
            (r.unit, scoping_block(flow, r.unit, r.block), false)
        } else {
            (owner, BlockId::ROUTINE, true)
        };
        let key = (unit, block, false);
        match index.get(&key) {
            Some(&c) => {
                cands[c].refs.push(i);
                cands[c].cross |= cross;
                if cands[c].refs.len() == 1 || r.at.start < cands[c].at.start {
                    cands[c].at = r.at;
                }
            }
            None => {
                index.insert(key, cands.len());
                cands.push(Candidate {
                    unit,
                    block,
                    role: Role::Free,
                    at: r.at,
                    cross,
                    refs: vec![i],
                    into: None,
                });
            }
        }
    }
    if cands.len() > MAX_CANDIDATES {
        res.buffers[b].unknown.push(Unknown {
            reason: UnknownReason::TooComplex,
            at: cands[0].at,
        });
        return;
    }
    // Merge, outermost first.
    let mut order: Vec<usize> = (0..cands.len()).collect();
    order.sort_by_key(|&c| {
        (
            unit_depth(flow, cands[c].unit),
            block_depth(flow, cands[c].unit, cands[c].block),
            cands[c].at.start,
        )
    });
    let mut done: Vec<usize> = Vec::with_capacity(order.len());
    for &c in &order {
        let here = (cands[c].unit, cands[c].block);
        let mut absorber = None;
        for &e in &done {
            if !encloses(flow, (cands[e].unit, cands[e].block), here) {
                continue;
            }
            let ok = matches!(
                (cands[e].role, cands[c].role),
                (Role::Free, Role::Weak | Role::Free) | (Role::Strong, Role::Weak)
            );
            if !ok {
                res.buffers[b].unknown.push(Unknown {
                    reason: UnknownReason::Conflict,
                    at: cands[c].at,
                });
            }
            if absorber.is_none() {
                absorber = Some(cands[e].into.unwrap_or(e));
            }
        }
        cands[c].into = absorber;
        done.push(c);
    }
    // The final scopes, in source order.
    let mut finals: Vec<usize> = (0..cands.len())
        .filter(|&c| cands[c].into.is_none())
        .collect();
    finals.sort_by_key(|&c| cands[c].at.start);
    let mut seen = Vec::with_capacity(finals.len());
    let mut slot = vec![0usize; cands.len()];
    for (k, &c) in finals.iter().enumerate() {
        slot[c] = k;
        seen.push(Scope {
            unit: cands[c].unit,
            block: cands[c].block,
            kind: match cands[c].role {
                Role::Strong => ScopeKind::Strong,
                Role::Weak => ScopeKind::Weak,
                Role::Free => ScopeKind::Free,
            },
            at: cands[c].at,
            absorbed: Vec::new(),
            references: 0,
        });
    }
    for c in 0..cands.len() {
        let k = slot[cands[c].into.unwrap_or(c)];
        let scope = &mut seen[k];
        scope.references += cands[c].refs.len() as u32;
        if cands[c].into.is_some() {
            scope.absorbed.push(cands[c].at);
            if scope.kind == ScopeKind::Free {
                scope.kind = ScopeKind::Raised;
            }
        } else if cands[c].cross && scope.kind == ScopeKind::Free {
            scope.kind = ScopeKind::Raised;
        }
        for &i in &cands[c].refs {
            scope_of[i] = Some(k);
        }
    }
    for s in &mut seen {
        s.absorbed.sort_by_key(|a| a.start);
    }
    res.buffers[b].seen = seen;
}
