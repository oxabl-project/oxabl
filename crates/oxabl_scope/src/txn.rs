//! Transaction blocks (section 7.3).

use oxabl_flow::{Block, BlockId, BlockKind, Condition, FileFlow, Origin};

use crate::model::{Transaction, TxKind, Unknown, Update};
use crate::resolve::Resolved;

/// Whether a block may be a transaction block.
pub(crate) fn capable(b: &Block) -> bool {
    match b.kind {
        BlockKind::Routine(_) | BlockKind::Repeat | BlockKind::For(_) => true,
        BlockKind::Do => {
            b.transaction
                || [Condition::Error, Condition::Endkey]
                    .iter()
                    .any(|&c| b.handling(c).is_some_and(|h| h.origin == Origin::Phrase))
        }
        BlockKind::Catch | BlockKind::Finally => false,
    }
}

/// The transaction facts of every block, by unit and block. `extra` is an
/// Unknown that applies to every capable block that is not a transaction
/// block (a recovered parse, a spent budget).
pub(crate) fn transactions(
    flow: &FileFlow,
    res: &Resolved,
    extra: Option<Unknown>,
) -> Vec<Vec<Transaction>> {
    let mut first: Vec<Vec<Option<Update>>> = flow
        .units()
        .iter()
        .map(|c| vec![None; c.block_count()])
        .collect();
    let mut unknown: Vec<Vec<Option<Unknown>>> = first
        .iter()
        .map(|blocks| vec![None; blocks.len()])
        .collect();
    let target = |unit: oxabl_flow::UnitId, block: BlockId| -> BlockId {
        let cfg = flow.unit(unit);
        let mut b = block;
        while !capable(cfg.block(b)) {
            match cfg.block(b).parent {
                Some(p) => b = p,
                None => break,
            }
        }
        b
    };
    for (unit, block, update) in &res.updates {
        let t = target(*unit, *block);
        let slot = &mut first[unit.index()][t.index()];
        if slot.is_none_or(|u| update.at.start < u.at.start) {
            *slot = Some(*update);
        }
    }
    for (unit, block, u) in &res.tx_unknowns {
        let t = target(*unit, *block);
        let slot = &mut unknown[unit.index()][t.index()];
        if slot.is_none_or(|o| u.at.start < o.at.start) {
            *slot = Some(*u);
        }
    }
    flow.units()
        .iter()
        .enumerate()
        .map(|(ui, cfg)| {
            let kinds: Vec<TxKind> = cfg
                .blocks()
                .map(|(id, b)| {
                    if b.transaction {
                        TxKind::Explicit
                    } else if let Some(u) = first[ui][id.index()] {
                        TxKind::Implicit(u)
                    } else if let Some(u) = unknown[ui][id.index()] {
                        TxKind::Unknown(u)
                    } else if let Some(u) = extra.filter(|_| capable(b)) {
                        TxKind::Unknown(u)
                    } else {
                        TxKind::None
                    }
                })
                .collect();
            cfg.blocks()
                .map(|(id, b)| {
                    let within = std::iter::successors(b.parent, |p| cfg.block(*p).parent)
                        .find(|p| kinds[p.index()].is_transaction());
                    Transaction {
                        kind: kinds[id.index()],
                        within,
                    }
                })
                .collect()
        })
        .collect()
}
