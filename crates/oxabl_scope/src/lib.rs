//! Buffer scopes, transaction blocks and lock modes in Progress ABL.
//!
//! [`analyze_file`] replays three decisions the OpenEdge compiler makes for a
//! file, over the units and blocks of its flow graphs (`oxabl_flow`):
//!
//! * **Buffer scopes.** Every buffer (default buffers of database tables and
//!   temp-tables, `DEFINE BUFFER`, parameters, shared and trigger buffers) and
//!   the blocks it is scoped to ([`Buffer::scopes`]): a strong scope (`DO FOR`,
//!   `REPEAT FOR`), a weak scope (a `FOR` record phrase), or the scope of free
//!   references at the nearest block with record scoping. A free reference
//!   whose block encloses another scope of the buffer raises that scope to
//!   its own block ([`ScopeKind::Raised`]), and the scope names the reference
//!   that raised it.
//! * **Transaction blocks.** Each block's [`Transaction`]: explicit
//!   (`TRANSACTION`), implicit with the first database update or
//!   `EXCLUSIVE-LOCK` read the block holds directly, or none; and the
//!   enclosing transaction block when it is a sub-transaction.
//! * **Locks.** Each `FIND`, `FOR` phrase, `CAN-FIND`, `OPEN QUERY` and `GET`
//!   with the lock it takes ([`RecordRead`]): explicit, the `SHARE-LOCK`
//!   default, or none for a temp-table; and whether a write to the buffer
//!   may run after it in the same scope.
//!
//! The ABL rules this follows, with confidence marks, are in
//! `docs/design/strict-analysis-open-questions.md`, section 7. The oracle is
//! the compiler's `COMPILE ... LISTING` output: [`listing`] parses its block
//! table and compares it with the model, and the `scope_listing` example of
//! the `oxabl` crate runs that comparison over a tree.
//!
//! # Unknown
//!
//! When the file does not show enough to decide, the answer is Unknown with a
//! reason ([`UnknownReason`]), never a guess: an unexpanded include, a
//! statement the parser skips whose form the model does not know, a dynamic
//! buffer method that may update a record, an unqualified field that two
//! buffers have, a table the schema does not have (or no schema), two scopes
//! that conflict, a recovered parse, or a file past the work budget.
//!
//! # Names
//!
//! Names are resolved here, against the file's definitions and the schema,
//! not through `oxabl_semantic`: the compiler picks the buffer of an
//! unqualified field by the record phrases and record blocks around it.
//! A name in a buffer position resolves to a buffer the unit defines, then
//! one of an enclosing unit, then a temp-table's default buffer, then a
//! schema table's default buffer, which belongs to the main block. An
//! unqualified field resolves against the statement's own record phrases,
//! then the enclosing `FOR` / `DO FOR` / `REPEAT FOR` blocks, then every
//! buffer the file names. A declared variable wins over a field.
//!
//! # Budget
//!
//! The work is linear in the references of the file, except the merge of one
//! buffer's candidate scopes, which is quadratic in their number and capped.
//! A file with more than [`MAX_REFERENCES`] references gets Unknown
//! (`too-complex`) everywhere.
//!
//! Spans are in the text the statements were parsed from (the expanded text
//! when includes were expanded).

mod collect;
mod dump;
pub mod listing;
mod model;
mod resolve;
mod scopes;
mod txn;

pub use model::{
    BlockScopes, Buffer, BufferId, BufferKind, Lock, LockMode, ReadKind, RecordRead, Scope,
    ScopeKind, Table, Transaction, TxKind, UnitScopes, Unknown, UnknownReason, Update, UpdateKind,
    Written,
};

use oxabl_ast::{Span, Statement};
use oxabl_flow::{FlowOptions, UnitId};
use oxabl_schema::Schema;
use rustc_hash::FxHashMap;

/// References past this many in one file make every answer Unknown.
pub const MAX_REFERENCES: usize = 400_000;

/// Options for [`analyze_file_with`].
#[derive(Debug, Clone, Copy, Default)]
pub struct ScopeOptions<'a> {
    /// The flow options (the default error directive).
    pub flow: FlowOptions,
    /// The parser recovered from an error: statements may be missing, so
    /// every answer is Unknown (`recovered-parse`).
    pub parse_recovered: bool,
    /// Offsets in the source where the preprocessor replaced a reference with
    /// no text, sorted. One after the record phrase of a read without a
    /// visible lock phrase makes its lock Unknown (`unexpanded-include`).
    pub elided: &'a [u32],
    /// The offsets of `elided` where an include did not arrive, sorted. A
    /// statement the include held may write a record: one after a read, in
    /// its scope, makes `written` Unknown (`unexpanded-include`).
    pub elided_includes: &'a [u32],
    /// The source is an include file checked alone: an elided reference
    /// anywhere in a read makes its lock Unknown, because the includer's
    /// definitions are missing.
    pub fragment: bool,
}

/// The scopes, transaction blocks and record reads of one file.
#[derive(Debug, Clone, Default)]
pub struct FileScopes {
    units: Vec<UnitScopes>,
    buffers: Vec<Buffer>,
    reads: Vec<RecordRead>,
    unknown: Vec<Unknown>,
}

impl FileScopes {
    /// Every unit, in source pre-order; the main block is first.
    pub fn units(&self) -> &[UnitScopes] {
        &self.units
    }

    /// One unit.
    pub fn unit(&self, id: UnitId) -> &UnitScopes {
        &self.units[id.index()]
    }

    /// Every buffer the file defines or names, in order of first sight.
    pub fn buffers(&self) -> &[Buffer] {
        &self.buffers
    }

    /// One buffer.
    pub fn buffer(&self, id: BufferId) -> &Buffer {
        &self.buffers[id.index()]
    }

    /// Every record read, in source order.
    pub fn reads(&self) -> &[RecordRead] {
        &self.reads
    }

    /// Reasons that hold for the whole file (an unexpanded include may name
    /// a table no statement of the file names).
    pub fn unknown(&self) -> &[Unknown] {
        &self.unknown
    }

    /// A stable text rendering, for snapshot tests and debugging. `source` is
    /// the text that was analysed; sites show as `@line`.
    pub fn dump(&self, source: &str) -> String {
        dump::dump(self, source)
    }
}

/// Whether a statement of the read's unit at `at`, in `block`, may run after
/// the read while the scope that ends at `bound` is open: it comes later in
/// the text, or an iterating block of the scope holds both. The scope block
/// counts when it iterates, though the record may be released at the end of
/// each iteration: the answer errs toward a write.
fn runs_after(
    flow: &oxabl_flow::FileFlow,
    read: &RecordRead,
    at: u32,
    block: oxabl_flow::BlockId,
    bound: Option<(UnitId, oxabl_flow::BlockId)>,
) -> bool {
    if at > read.at.start {
        return true;
    }
    let cfg = flow.unit(read.unit);
    let bound = bound.filter(|(u, _)| *u == read.unit).map(|(_, b)| b);
    let mut b = Some(read.block);
    while let Some(id) = b {
        let blk = cfg.block(id);
        if blk.iterating && cfg.block_contains(id, block) {
            return true;
        }
        if Some(id) == bound {
            return false;
        }
        b = blk.parent;
    }
    false
}

/// Whether an include that did not arrive (`includes`, sorted offsets) lies
/// in the scope of the read, where text it held may run after the read: it
/// comes later in the text, or an iterating block holds it and the read.
fn include_may_run_after(
    flow: &oxabl_flow::FileFlow,
    read: &RecordRead,
    bound: Option<(UnitId, oxabl_flow::BlockId)>,
    includes: &[u32],
) -> bool {
    if includes.is_empty() {
        return false;
    }
    let cfg = flow.unit(read.unit);
    let scope = bound
        .filter(|(u, _)| *u == read.unit)
        .map_or(oxabl_flow::BlockId::ROUTINE, |(_, b)| b);
    let span = cfg.block(scope).span;
    let from = includes.partition_point(|&o| o < span.start);
    includes[from..]
        .iter()
        .take_while(|&&o| o < span.end)
        .filter(|&&o| o < read.at.start || o >= read.at.end)
        .any(|&o| {
            let innermost = cfg
                .blocks()
                .filter(|(_, b)| b.span.start <= o && o < b.span.end)
                .min_by_key(|(_, b)| b.span.end - b.span.start);
            innermost.is_some_and(|(id, _)| runs_after(flow, read, o, id, bound))
        })
}

/// Analyse a parsed file. `source` must be the text the statements were
/// parsed from; `schema` is `None` when no schema is loaded.
pub fn analyze_file(program: &[Statement], source: &str, schema: Option<&Schema>) -> FileScopes {
    analyze_file_with(program, source, schema, &ScopeOptions::default())
}

/// [`analyze_file`] with options.
pub fn analyze_file_with(
    program: &[Statement],
    source: &str,
    schema: Option<&Schema>,
    options: &ScopeOptions<'_>,
) -> FileScopes {
    let tokens = oxabl_lexer::tokenize(source);
    analyze_tokens(program, source, &tokens, schema, options)
}

/// [`analyze_file_with`] with the tokens the statements were parsed from
/// (`oxabl_lexer::tokenize(source)`), so the text is not lexed again.
pub fn analyze_tokens(
    program: &[Statement],
    source: &str,
    tokens: &[oxabl_lexer::Token],
    schema: Option<&Schema>,
    options: &ScopeOptions<'_>,
) -> FileScopes {
    let flow = oxabl_flow::build_file_tokens(program, source, tokens, &options.flow);
    analyze_flow(program, source, tokens, &flow, schema, options)
}

/// [`analyze_tokens`] over the file's CFGs, already built from the same
/// statements with `options.flow` (`oxabl_flow::build_file_tokens`).
pub fn analyze_flow(
    program: &[Statement],
    source: &str,
    tokens: &[oxabl_lexer::Token],
    flow: &oxabl_flow::FileFlow,
    schema: Option<&Schema>,
    options: &ScopeOptions<'_>,
) -> FileScopes {
    let collected = collect::collect(program, source, tokens, flow, options);
    let mut file_unknown: Vec<Unknown> = Vec::new();
    if options.parse_recovered {
        file_unknown.push(Unknown {
            reason: UnknownReason::RecoveredParse,
            at: Span::DUMMY,
        });
    }
    if collected.refs.len() > MAX_REFERENCES {
        file_unknown.push(Unknown {
            reason: UnknownReason::TooComplex,
            at: Span::DUMMY,
        });
    }
    let mut res = resolve::resolve(&collected, flow, schema);
    let scope_of = scopes::assign(&mut res, flow);

    // Unknowns: per buffer, per unit (every buffer the unit can see), file.
    for (b, u) in std::mem::take(&mut res.buffer_unknowns) {
        res.buffers[b.index()].unknown.push(u);
    }
    for (unit, u) in &res.unit_unknowns {
        for b in &mut res.buffers {
            let visible = std::iter::successors(Some(*unit), |x| flow.unit(*x).unit().parent)
                .any(|x| x == b.owner);
            let affected = match u.reason {
                UnknownReason::NoSchema => !b.table.is_temp_table(),
                _ => true,
            };
            if visible && affected && !b.is_external() {
                b.unknown.push(*u);
            }
        }
        if u.reason == UnknownReason::UnexpandedInclude
            && !file_unknown.iter().any(|f| f.reason == u.reason)
        {
            file_unknown.push(*u);
        }
    }
    for b in &mut res.buffers {
        if !b.is_external() {
            b.unknown.extend(file_unknown.iter().copied());
        }
        b.unknown.sort_by_key(|u| (u.reason, u.at.start));
        b.unknown.dedup_by_key(|u| u.reason);
    }

    // Writes that may run after each read, in the same scope; else a call or
    // a statement that may write.
    let mut ref_at: FxHashMap<(BufferId, u32), usize> = FxHashMap::default();
    let mut later: FxHashMap<BufferId, Vec<usize>> = FxHashMap::default();
    for (i, r) in res.refs.iter().enumerate() {
        ref_at.entry((r.buffer, r.at.start)).or_insert(i);
        if r.write.is_some() || r.passed || r.opaque {
            later.entry(r.buffer).or_default().push(i);
        }
    }
    for read in &mut res.reads {
        let Some(b) = read.buffer else {
            continue;
        };
        let scope = ref_at.get(&(b, read.at.start)).and_then(|&i| scope_of[i]);
        let bound = scope.map(|s| {
            let s = &res.buffers[b.index()].seen[s];
            (s.unit, s.block)
        });
        let refs = later.get(&b).map_or(&[][..], Vec::as_slice);
        let mut written = Written::No;
        for &i in refs {
            let r = &res.refs[i];
            let same = match scope {
                Some(s) => scope_of[i] == Some(s),
                None => r.unit == read.unit,
            };
            if !same {
                continue;
            }
            let same_unit = r.unit == read.unit;
            if same_unit && !runs_after(flow, read, r.at.start, r.block, bound) {
                continue;
            }
            let reason = if r.write.is_some() {
                if same_unit {
                    written = Written::Yes;
                    break;
                }
                // Another routine of the scope: it runs when it is called.
                UnknownReason::UnitOrder
            } else if r.passed {
                UnknownReason::Callee
            } else {
                UnknownReason::SkippedStatement
            };
            if written == Written::No {
                written = Written::Unknown(reason);
            }
        }
        let shared = matches!(
            res.buffers[b.index()].kind,
            BufferKind::Shared | BufferKind::NewShared
        );
        if written == Written::No
            && shared
            && res
                .runs
                .iter()
                .any(|&(u, block, at)| u == read.unit && runs_after(flow, read, at, block, bound))
        {
            written = Written::Unknown(UnknownReason::Callee);
        }
        if written == Written::No
            && include_may_run_after(flow, read, bound, options.elided_includes)
        {
            written = Written::Unknown(UnknownReason::UnexpandedInclude);
        }
        read.written = written;
    }
    res.reads.sort_by_key(|r| r.at.start);

    let extra = file_unknown.first().copied();
    let tx = txn::transactions(flow, &res, extra);
    let mut units: Vec<UnitScopes> = flow
        .units()
        .iter()
        .zip(tx)
        .map(|(cfg, tx)| {
            let u = cfg.unit();
            UnitScopes {
                unit: cfg.id(),
                kind: u.kind,
                name: u.name.clone(),
                span: u.span,
                parent: u.parent,
                blocks: cfg
                    .blocks()
                    .zip(tx)
                    .map(|((id, b), t)| BlockScopes {
                        id,
                        kind: b.kind,
                        stmt: b.stmt,
                        span: b.span,
                        parent: b.parent,
                        label: b.label.clone(),
                        transaction_capable: txn::capable(b),
                        transaction: t,
                        buffers: Vec::new(),
                    })
                    .collect(),
            }
        })
        .collect();
    for b in &res.buffers {
        if let Some(scopes) = b.scopes() {
            for s in scopes {
                units[s.unit.index()].blocks[s.block.index()]
                    .buffers
                    .push(b.id);
            }
        }
    }
    FileScopes {
        units,
        buffers: res.buffers,
        reads: res.reads,
        unknown: file_unknown,
    }
}
