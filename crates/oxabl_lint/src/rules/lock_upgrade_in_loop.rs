//! `lock-upgrade-in-loop` lint (LK003), group `strict.locks`.
//!
//! Fires on a `FIND CURRENT b EXCLUSIVE-LOCK`, or a re-find of the same record
//! (`FIND b2 WHERE ROWID(b2) = ROWID(b) EXCLUSIVE-LOCK`), that runs on every
//! iteration of a loop that read `b` with `NO-LOCK`.
//!
//! # Why
//!
//! The loop reads each record without a lock, then reads it again to lock it.
//! Every iteration costs a second read and a lock request, and the record can
//! change between the two. When every record is changed, read it with
//! `EXCLUSIVE-LOCK` in the loop. When only some are, upgrade only those.
//!
//! ```abl
//! FOR EACH Customer NO-LOCK:
//!     FIND CURRENT Customer EXCLUSIVE-LOCK.
//!     Customer.Balance = 0.
//! END.
//! ```
//!
//! Fixed:
//!
//! ```abl
//! FOR EACH Customer EXCLUSIVE-LOCK TRANSACTION:
//!     Customer.Balance = 0.
//! END.
//! ```
//!
//! # What is and is not reported
//!
//! One diagnostic per upgrade, on the `FIND` statement. The loop is the
//! innermost `FOR EACH`, `REPEAT` or `DO WHILE`/`TO` block around the upgrade
//! (a `FOR FIRST` or `FOR LAST` runs once and is not a loop). The `NO-LOCK`
//! read is the loop's own record phrase, or a `FIND` or `GET` with `NO-LOCK`
//! before the upgrade in the same loop.
//!
//! Not reported:
//!
//! - an upgrade that some iteration skips: under an `IF` or `CASE`, or after
//!   a `NEXT` that a condition takes;
//! - an upgrade with `NO-WAIT`: the loop skips the records another session
//!   holds, which a loop that reads with `EXCLUSIVE-LOCK` cannot do;
//! - an upgrade from `SHARE-LOCK` (`default-lock`, LK001, covers a read with
//!   no lock phrase);
//! - a re-find by anything other than `ROWID` or `RECID` equality;
//! - `GET CURRENT ... EXCLUSIVE-LOCK`;
//! - a file the scope analysis cannot judge: a recovered parse, an unexpanded
//!   include, or a file past its work budget.
//!
//! # Include files
//!
//! A finding in an include is reported in the include when the include alone
//! shows it, else at the `{...}` site in the including file.

use oxabl_ast::{
    Expression, ExpressionKind, FindType, LockType, NodeId, Span, Statement, StatementKind,
};
use oxabl_common::{Diagnostic, Severity};
use oxabl_flow::{
    BlockId, BlockKind, Cfg, EdgeKind, FlowOptions, ForQualifier, NodeIx, NodeKind, UnitBuilder,
    UnitId, dataflow::BitSet,
};
use oxabl_lexer::{Kind, Lexer};
use oxabl_scope::{FileScopes, Lock, LockMode, ReadKind, RecordRead};
use oxabl_semantic::AnalysisContext;
use rustc_hash::FxHashMap;

use super::LK003;
use super::handle_support::{quote, warning};
use super::lock_support::{describe_block, not_judged, read_form, same_buffer_name};
use super::syntactic::{Visitor, leading_tokens, walk_block};

/// Kebab-case rule name.
pub const NAME: &str = "lock-upgrade-in-loop";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "strict.locks";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Warning;
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "A NO-LOCK read upgraded to EXCLUSIVE-LOCK on every iteration of a loop";

/// Entry point: analyse the file, then [`check`].
pub fn run(
    program: &[Statement],
    _sem: &oxabl_semantic::Semantic,
    ctx: &AnalysisContext,
) -> Vec<Diagnostic> {
    check(&crate::shared::analyze_scopes(program, ctx), program, ctx)
}

/// How a `FIND ... EXCLUSIVE-LOCK` names the record it locks again.
enum Upgrade {
    /// `FIND CURRENT b`: the record already in `b`.
    Current,
    /// `ROWID(found) = ROWID(other)` (or `RECID`): the record in `other`.
    SameRow(String),
}

/// The findings over an analysed file.
pub fn check(scopes: &FileScopes, program: &[Statement], ctx: &AnalysisContext) -> Vec<Diagnostic> {
    if not_judged(scopes) {
        return Vec::new();
    }
    let upgrades = upgrades(program, ctx.source);
    if upgrades.is_empty() {
        return Vec::new();
    }
    let mut builder: Option<UnitBuilder> = None;
    let mut cfgs: FxHashMap<UnitId, Cfg> = FxHashMap::default();
    let mut diags = Vec::new();
    for x in scopes.reads() {
        if x.kind != ReadKind::Find
            || x.lock != Lock::Explicit(LockMode::ExclusiveLock)
            || x.buffer.is_none()
        {
            continue;
        }
        let Some(upgrade) = upgrades.get(&x.stmt) else {
            continue;
        };
        let cfg = cfgs.entry(x.unit).or_insert_with(|| {
            builder
                .get_or_insert_with(|| {
                    UnitBuilder::with_tokens(
                        program,
                        ctx.source,
                        ctx.tokens,
                        &FlowOptions::default(),
                    )
                })
                .build(x.unit)
        });
        let Some(lp) = innermost_loop(cfg, x.block) else {
            continue;
        };
        let Some(read) = scopes.reads().iter().find(|n| {
            n.unit == x.unit
                && n.lock == Lock::Explicit(LockMode::NoLock)
                && same_record(n, x, upgrade)
                && match n.kind {
                    ReadKind::For | ReadKind::Preselect => n.block == lp,
                    ReadKind::Find | ReadKind::Get => {
                        n.at.start < x.at.start && innermost_loop(cfg, n.block) == Some(lp)
                    }
                    _ => false,
                }
        }) else {
            continue;
        };
        if !every_iteration(cfg, lp, x.stmt) {
            continue;
        }
        let Some(&node) = cfg.nodes_of(x.stmt).first() else {
            continue;
        };
        let span = cfg.node(node).span;
        if no_wait(ctx.source, span) {
            continue;
        }
        let unit = scopes.unit(x.unit);
        diags.push(warning(
            ctx,
            LK003,
            format!(
                "`{}` locks again, on every iteration of {}, the record of `{}` that {} read \
                 with NO-LOCK",
                quote(ctx.source, span),
                describe_block(ctx.source, unit, lp),
                read.name,
                read_form(read.kind),
            ),
            "Read the record with `EXCLUSIVE-LOCK` in the loop when every record is \
             changed, or lock again only the records that are (under the condition that \
             picks them)."
                .to_string(),
            span,
        ));
    }
    diags
}

/// Whether the `NO-LOCK` read `n` reads the record that `x` locks again.
fn same_record(n: &RecordRead, x: &RecordRead, upgrade: &Upgrade) -> bool {
    match upgrade {
        Upgrade::Current => n.buffer.is_some() && n.buffer == x.buffer,
        Upgrade::SameRow(other) => n.buffer.is_some() && same_buffer_name(&n.name, other),
    }
}

/// The innermost block around `block`, itself included, that runs its body
/// more than once.
fn innermost_loop(cfg: &Cfg, block: BlockId) -> Option<BlockId> {
    std::iter::successors(Some(block), |b| cfg.block(*b).parent).find(|b| {
        let blk = cfg.block(*b);
        blk.iterating
            && !matches!(
                blk.kind,
                BlockKind::For(ForQualifier::First | ForQualifier::Last)
            )
    })
}

/// Whether every iteration of `lp` that completes runs `stmt`: the end of the
/// body is reachable from its start, but not around `stmt`. Raised conditions
/// are not followed.
fn every_iteration(cfg: &Cfg, lp: BlockId, stmt: NodeId) -> bool {
    let block_stmt = cfg.block(lp).stmt;
    let of_loop = |n: NodeIx, kind: fn(&NodeKind) -> bool| {
        let node = cfg.node(n);
        kind(&node.kind) && node.stmt == Some(block_stmt)
    };
    let upgrade = cfg.nodes_of(stmt);
    let reaches_end = |avoid: bool| {
        let mut seen = BitSet::new_empty(cfg.node_count());
        let mut stack: Vec<NodeIx> = cfg
            .nodes()
            .filter(|(n, _)| of_loop(*n, |k| matches!(k, NodeKind::Head)))
            .flat_map(|(n, _)| cfg.successors(n))
            .filter(|e| e.kind == EdgeKind::LoopBody)
            .map(|e| e.to)
            .collect();
        while let Some(n) = stack.pop() {
            if !seen.insert(n.index()) {
                continue;
            }
            if of_loop(n, |k| matches!(k, NodeKind::End)) {
                return true;
            }
            if of_loop(n, |k| matches!(k, NodeKind::Head))
                || (avoid && upgrade.contains(&n))
                || !cfg.block_contains(lp, cfg.node(n).block)
            {
                continue;
            }
            stack.extend(
                cfg.successors(n)
                    .filter(|e| !matches!(e.kind, EdgeKind::Raise(_)))
                    .map(|e| e.to),
            );
        }
        false
    };
    reaches_end(false) && !reaches_end(true)
}

/// Whether the statement at `span` has `NO-WAIT`: the loop then skips the
/// records another session holds, which a locking loop cannot do.
fn no_wait(source: &str, span: Span) -> bool {
    source
        .get(span.start as usize..span.end as usize)
        .is_some_and(|text| {
            Lexer::new(text)
                .take_while(|t| t.kind != Kind::Eof)
                .any(|t| t.kind == Kind::NoWait)
        })
}

/// Every `FIND ... EXCLUSIVE-LOCK` that locks a record again, by statement.
fn upgrades(program: &[Statement], source: &str) -> FxHashMap<NodeId, Upgrade> {
    struct Finds<'a> {
        source: &'a str,
        out: FxHashMap<NodeId, Upgrade>,
    }
    impl Visitor for Finds<'_> {
        fn statement(&mut self, stmt: &Statement) -> bool {
            if let StatementKind::Find {
                find_type,
                buffer,
                where_clause,
                lock_type: LockType::ExclusiveLock,
                ..
            } = &stmt.kind
            {
                let upgrade = if *find_type == FindType::Current {
                    Some(Upgrade::Current)
                } else {
                    where_clause
                        .as_ref()
                        .and_then(|w| same_row(self.source, w, &buffer.name))
                        .map(Upgrade::SameRow)
                };
                if let Some(u) = upgrade {
                    self.out.insert(stmt.id, u);
                }
            }
            true
        }
    }
    let mut v = Finds {
        source,
        out: FxHashMap::default(),
    };
    walk_block(&mut v, program);
    v.out
}

/// In a `WHERE` of `AND`ed terms, the other buffer of a term
/// `ROWID(found) = ROWID(other)` (or `RECID`).
fn same_row(source: &str, expr: &Expression, found: &str) -> Option<String> {
    match &expr.kind {
        ExpressionKind::And(a, b) => {
            same_row(source, a, found).or_else(|| same_row(source, b, found))
        }
        ExpressionKind::Equal(a, b) => {
            let (ka, na) = row_of(source, a)?;
            let (kb, nb) = row_of(source, b)?;
            if ka != kb {
                return None;
            }
            if same_buffer_name(na, found) && !same_buffer_name(nb, found) {
                Some(nb.to_string())
            } else if same_buffer_name(nb, found) && !same_buffer_name(na, found) {
                Some(na.to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// `ROWID(b)` or `RECID(b)`: the function and the buffer name.
fn row_of<'e>(source: &str, expr: &'e Expression) -> Option<(Kind, &'e str)> {
    let ExpressionKind::FunctionCall { name, arguments } = &expr.kind else {
        return None;
    };
    let kind = leading_tokens(source, name.span, 1).first()?.0.kind;
    if !matches!(kind, Kind::Rowid | Kind::Recid) {
        return None;
    }
    match arguments.as_slice() {
        [
            Expression {
                kind: ExpressionKind::Identifier(id),
                ..
            },
        ] => Some((kind, id.name.as_str())),
        _ => None,
    }
}
