//! The failed-`FIND` analysis: a forward dataflow per unit over the `FIND ...
//! NO-ERROR` statements that may leave their buffer empty (section 9).
//!
//! Each `Stmt` and `Head` node of the flow graph owns the tokens and buffer
//! references that no node nested in it holds. What a node does to the
//! buffers is read from those, then the dataflow carries, per program point,
//! the finds that may have failed with nothing in between to guard or refill
//! their buffer.

use oxabl_ast::{NodeId, Span};
use oxabl_flow::dataflow::{Analysis, BitSet, Direction, solve};
use oxabl_flow::{Cfg, EdgeKind, FileFlow, NodeIx, NodeKind, OpaqueReason, UnitId};
use oxabl_lexer::{Kind, Token};
use oxabl_scope::{BufferId, BufferKind, FileScopes, ReadKind, Reference, Table};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::cond::{self, Cond};
use crate::model::{FailedFind, MissingRecord};
use crate::{FileNulls, NullOptions};

/// A token without its value.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Tok {
    pub kind: Kind,
    pub start: u32,
    pub end: u32,
}

/// A span of the text that one or more nodes cover: copies of a statement in
/// `FINALLY` blocks share it.
struct Interval {
    start: u32,
    end: u32,
    nodes: SmallVec<[(UnitId, NodeIx); 1]>,
}

/// What the nodes of one interval do to the buffers.
#[derive(Default)]
struct Facts {
    no_error: bool,
    /// `FIND CURRENT`: with `NO-ERROR` it re-reads the record the buffer
    /// holds, and keeps the buffer as it was (N6).
    find_current: bool,
    /// Buffers named at record level (`RECID(b)`), not written.
    mentions: SmallVec<[BufferId; 2]>,
    /// Text the preprocessor elided, or a dynamic method on a handle: every
    /// buffer is Unknown after it (K5, section 9.5).
    forget_all: bool,
    /// The statement may run other code (K6).
    calls: bool,
    /// Buffers refilled or Unknown after the statement (K1-K5).
    kills: SmallVec<[BufferId; 2]>,
    /// The first field reference of each buffer, with whether it writes.
    sinks: SmallVec<[(BufferId, Span, bool); 2]>,
    /// Buffers a completed field reference proves (K3).
    proven: SmallVec<[BufferId; 2]>,
    /// Buffers whose fields a statement with `NO-ERROR` reads: when it is a
    /// `FIND` of another buffer that a guard then proves, they had a record
    /// (G9).
    suppressed: SmallVec<[BufferId; 2]>,
    /// The statement is in a branch of a statement-level `&IF` whose test
    /// the preprocessor left: it may never be compiled (section 9.5).
    in_preproc: bool,
    /// Buffers the statement tests with `AVAILABLE` (G8).
    tested: SmallVec<[BufferId; 2]>,
    /// The condition of an `IF` or of a `WHILE` header, and whether its false
    /// edge leaves the loop only when the condition is false (no `TO`).
    cond: Option<(Cond, bool)>,
}

/// Analyse the file. See [`crate::analyze`].
pub(crate) fn analyze(
    source: &str,
    tokens: &[Token],
    flow: &FileFlow,
    scopes: &FileScopes,
    options: &NullOptions<'_>,
) -> FileNulls {
    if !scopes.unknown().is_empty() {
        return FileNulls {
            unknown: scopes.unknown().to_vec(),
            ..FileNulls::default()
        };
    }
    let tracked: Vec<bool> = scopes
        .buffers()
        .iter()
        .map(|b| !matches!(b.table, Table::Unknown(_)))
        .collect();
    let is_tracked = |b: BufferId| tracked[b.index()];
    let finds: Vec<usize> = scopes
        .reads()
        .iter()
        .enumerate()
        .filter(|(_, r)| r.kind == ReadKind::Find && r.buffer.is_some_and(is_tracked))
        .map(|(i, _)| i)
        .collect();
    if finds.is_empty() || !tokens.iter().any(|t| t.kind == Kind::NoError) {
        return FileNulls::default();
    }

    let toks: Vec<Tok> = tokens
        .iter()
        .filter(|t| !matches!(t.kind, Kind::Comment | Kind::Eof))
        .map(|t| Tok {
            kind: t.kind,
            start: t.start as u32,
            end: t.end as u32,
        })
        .collect();
    let refs = scopes.references();
    let intervals = intervals(flow);
    let tok_owner = owners(&intervals, toks.iter().map(|t| t.start));
    let ref_owner = owners(&intervals, refs.iter().map(|r| r.at.start));
    let elided_owner = owners(&intervals, options.elided.iter().copied());

    let mut ref_at: FxHashMap<u32, usize> = FxHashMap::default();
    for (i, r) in refs.iter().enumerate() {
        ref_at.entry(r.at.start).or_insert(i);
    }
    // The operand of `AVAILABLE` is always a buffer, also where a variable
    // of the same name hides it from the scope analysis.
    let mut operands: FxHashMap<u32, (BufferId, u32)> = FxHashMap::default();
    for (i, t) in toks.iter().enumerate() {
        if t.kind != Kind::Available {
            continue;
        }
        let at = if toks.get(i + 1).map(|t| t.kind) == Some(Kind::LeftParen) {
            i + 2
        } else {
            i + 1
        };
        let (Some(op), Some(iv)) = (toks.get(at), tok_owner[i]) else {
            continue;
        };
        if ref_at.contains_key(&op.start) {
            continue;
        }
        let Some(&(unit, _)) = intervals[iv as usize].nodes.first() else {
            continue;
        };
        let name = source.get(op.start as usize..op.end as usize).unwrap_or("");
        if let Some(b) = buffer_named(scopes, flow, name, unit) {
            operands.insert(op.start, (b, op.end));
        }
    }
    let lookup = |at: u32| {
        ref_at
            .get(&at)
            .map(|&i| (refs[i].buffer, refs[i].field, refs[i].at.end))
            .or_else(|| operands.get(&at).map(|&(b, end)| (b, false, end)))
    };

    let mut facts: Vec<Facts> = (0..intervals.len()).map(|_| Facts::default()).collect();
    read_tokens(source, &toks, &tok_owner, &lookup, &mut facts);
    for (o, _) in elided_owner.iter().zip(options.elided) {
        if let Some(iv) = o {
            facts[*iv as usize].forget_all = true;
        }
    }
    read_references(refs, &ref_owner, &is_tracked, &mut facts);

    // Reads per statement: a FIND, FOR or query of a buffer refills it, and
    // its own criteria are no use of an empty record.
    let mut reads_of: FxHashMap<NodeId, SmallVec<[usize; 1]>> = FxHashMap::default();
    for (i, r) in scopes.reads().iter().enumerate() {
        reads_of.entry(r.stmt).or_default().push(i);
    }
    for (iv, f) in facts.iter_mut().enumerate() {
        let Some(&(unit, node)) = intervals[iv].nodes.first() else {
            continue;
        };
        let node = flow.unit(unit).node(node);
        let stmt = node.stmt;
        let first = toks.partition_point(|t| t.start < intervals[iv].start);
        f.find_current = toks.get(first).is_some_and(|t| t.kind == Kind::Find)
            && toks.get(first + 1).is_some_and(|t| t.kind == Kind::Current);
        let own: SmallVec<[BufferId; 2]> = stmt
            .and_then(|s| reads_of.get(&s))
            .into_iter()
            .flatten()
            .filter_map(|&i| {
                let r = &scopes.reads()[i];
                (r.kind != ReadKind::CanFind).then_some(r.buffer).flatten()
            })
            .collect();
        f.sinks.retain(|(b, _, _)| !own.contains(b));
        if !(f.find_current && f.no_error) {
            for &b in &own {
                if !f.kills.contains(&b) {
                    f.kills.push(b);
                }
            }
        }
        // A loop whose header names a buffer at record level (`WHERE
        // x.parent = RECID(b)`) may run only when it has a record.
        if node.kind == NodeKind::Head {
            let mentions = std::mem::take(&mut f.mentions);
            for b in mentions {
                if !own.contains(&b) && !f.tested.contains(&b) && !f.kills.contains(&b) {
                    f.kills.push(b);
                }
            }
        }
        if f.no_error && !f.forget_all {
            f.suppressed = f.sinks.iter().map(|(b, _, _)| *b).collect();
        }
        if f.no_error || f.forget_all {
            f.sinks.clear();
        }
        let tested = std::mem::take(&mut f.tested);
        let mut proven: SmallVec<[BufferId; 2]> = SmallVec::new();
        f.sinks.retain(|(b, _, _)| {
            if tested.contains(b) {
                return false;
            }
            proven.push(*b);
            true
        });
        f.proven = proven;
        f.tested = tested;
        f.cond = condition(source, &toks, &tok_owner, &intervals[iv], iv, flow, &lookup);
    }

    // The branches of a statement-level `&IF` the preprocessor left.
    for cfg in flow.units() {
        for (n, node) in cfg.nodes() {
            if !cfg.successors(n).any(|e| e.kind == EdgeKind::Preproc) {
                continue;
            }
            let from = intervals.partition_point(|iv| iv.start < node.span.start);
            for (i, iv) in intervals.iter().enumerate().skip(from) {
                if iv.start >= node.span.end {
                    break;
                }
                if iv.end <= node.span.end && (iv.start, iv.end) != (node.span.start, node.span.end)
                {
                    facts[i].in_preproc = true;
                }
            }
        }
    }

    // Buffers that other code may change when the statement calls it (K6).
    let mut first_unit: Vec<Option<UnitId>> = vec![None; scopes.buffers().len()];
    let mut shared: Vec<bool> = scopes
        .buffers()
        .iter()
        .map(|b| {
            matches!(
                b.kind,
                BufferKind::Shared | BufferKind::NewShared | BufferKind::Parameter
            )
        })
        .collect();
    for r in refs {
        let i = r.buffer.index();
        match first_unit[i] {
            None => first_unit[i] = Some(r.unit),
            Some(u) if u != r.unit => shared[i] = true,
            Some(_) => {}
        }
    }
    let shared: Vec<BufferId> = scopes
        .buffers()
        .iter()
        .filter(|b| shared[b.id.index()])
        .map(|b| b.id)
        .collect();

    // The node -> interval map of each unit.
    let mut iv_of: Vec<Vec<u32>> = flow
        .units()
        .iter()
        .map(|c| vec![u32::MAX; c.node_count()])
        .collect();
    for (i, iv) in intervals.iter().enumerate() {
        for &(u, n) in &iv.nodes {
            iv_of[u.index()][n.index()] = i as u32;
        }
    }

    let mut out_finds: Vec<FailedFind> = Vec::new();
    let mut missing: FxHashMap<(u32, u32), MissingRecord> = FxHashMap::default();
    let mut by_unit: FxHashMap<UnitId, SmallVec<[usize; 4]>> = FxHashMap::default();
    for &i in &finds {
        by_unit.entry(scopes.reads()[i].unit).or_default().push(i);
    }
    let mut units: Vec<UnitId> = by_unit.keys().copied().collect();
    units.sort();
    for unit in units {
        let cfg = flow.unit(unit);
        let ivs = &iv_of[unit.index()];
        let mut sites = Sites::default();
        for &i in &by_unit[&unit] {
            let read = &scopes.reads()[i];
            let buffer = read.buffer.expect("filtered");
            let Some(&first) = cfg.nodes_of(read.stmt).first() else {
                continue;
            };
            let iv = ivs[first.index()];
            if iv == u32::MAX {
                continue;
            }
            let f = &facts[iv as usize];
            if !f.no_error || f.forget_all || sites.of_iv.contains_key(&iv) {
                continue;
            }
            if f.find_current {
                continue;
            }
            let interval = &intervals[iv as usize];
            let index = out_finds.len() as u32;
            out_finds.push(FailedFind {
                unit,
                stmt: read.stmt,
                buffer,
                at: Span {
                    start: interval.start,
                    end: interval.end,
                },
            });
            let s = sites.buffer.len() as u32;
            sites.of_iv.insert(iv, s);
            sites.buffer.push(buffer);
            sites.index.push(index);
            sites.of_buffer.entry(buffer).or_default().push(s);
            let deps = sites.deps.entry(buffer).or_default();
            for &d in &f.suppressed {
                if d != buffer && !deps.contains(&d) {
                    deps.push(d);
                }
            }
        }
        if sites.buffer.is_empty() {
            continue;
        }
        let problem = Problem {
            ivs,
            facts: &facts,
            sites: &sites,
            shared: &shared,
        };
        let solution = solve(cfg, &problem);
        for (n, node) in cfg.nodes() {
            let iv = ivs[n.index()];
            if iv == u32::MAX {
                continue;
            }
            let before = &solution.before(n).maybe;
            if before.is_empty() || !matches!(node.kind, NodeKind::Stmt | NodeKind::Head) {
                continue;
            }
            if facts[iv as usize].in_preproc {
                continue;
            }
            for &(b, at, write) in &facts[iv as usize].sinks {
                let Some(ss) = sites.of_buffer.get(&b) else {
                    continue;
                };
                let reaching: SmallVec<[u32; 2]> = ss
                    .iter()
                    .filter(|&&s| before.contains(s as usize))
                    .map(|&s| sites.index[s as usize])
                    .collect();
                if reaching.is_empty() {
                    continue;
                }
                let m = missing
                    .entry((at.start, at.end))
                    .or_insert_with(|| MissingRecord {
                        unit,
                        buffer: b,
                        at,
                        write,
                        finds: SmallVec::new(),
                    });
                for s in reaching {
                    if !m.finds.contains(&s) {
                        m.finds.push(s);
                    }
                }
            }
        }
    }

    // Sort the finds by position and renumber the references to them.
    let mut order: Vec<u32> = (0..out_finds.len() as u32).collect();
    order.sort_by_key(|&i| out_finds[i as usize].at.start);
    let mut renumber = vec![0u32; out_finds.len()];
    for (new, &old) in order.iter().enumerate() {
        renumber[old as usize] = new as u32;
    }
    let finds_sorted: Vec<FailedFind> = order
        .iter()
        .map(|&i| out_finds[i as usize].clone())
        .collect();
    let mut missing: Vec<MissingRecord> = missing.into_values().collect();
    for m in &mut missing {
        for f in &mut m.finds {
            *f = renumber[*f as usize];
        }
        m.finds.sort_unstable();
    }
    missing.sort_by_key(|m| (m.at.start, m.at.end));
    FileNulls {
        finds: finds_sorted,
        missing,
        unknown: Vec::new(),
    }
}

/// The buffer `name` gives in `unit`: one the unit or the closest unit around
/// it owns, a defined buffer before a default one.
fn buffer_named(
    scopes: &FileScopes,
    flow: &FileFlow,
    name: &str,
    unit: UnitId,
) -> Option<BufferId> {
    let name = name.rsplit('.').next().unwrap_or(name);
    let chain: SmallVec<[UnitId; 4]> =
        std::iter::successors(Some(unit), |u| flow.unit(*u).unit().parent).collect();
    scopes
        .buffers()
        .iter()
        .filter(|b| b.name.eq_ignore_ascii_case(name))
        .filter_map(|b| {
            let depth = chain.iter().position(|u| *u == b.owner)?;
            Some((depth, b.kind == BufferKind::Default, b.id))
        })
        .min()
        .map(|(_, _, id)| id)
}

/// The `Stmt` and `Head` nodes of every unit, grouped by span and sorted by
/// start, outer before inner.
fn intervals(flow: &FileFlow) -> Vec<Interval> {
    let mut by_span: FxHashMap<(u32, u32), usize> = FxHashMap::default();
    let mut out: Vec<Interval> = Vec::new();
    for cfg in flow.units() {
        for (n, node) in cfg.nodes() {
            if !matches!(node.kind, NodeKind::Stmt | NodeKind::Head)
                || node.span.start >= node.span.end
            {
                continue;
            }
            let key = (node.span.start, node.span.end);
            let i = *by_span.entry(key).or_insert_with(|| {
                out.push(Interval {
                    start: key.0,
                    end: key.1,
                    nodes: SmallVec::new(),
                });
                out.len() - 1
            });
            out[i].nodes.push((cfg.id(), n));
        }
    }
    out.sort_by_key(|iv| (iv.start, std::cmp::Reverse(iv.end)));
    out
}

/// The innermost interval holding each position (positions ascending).
fn owners(intervals: &[Interval], positions: impl Iterator<Item = u32>) -> Vec<Option<u32>> {
    let mut stack: Vec<usize> = Vec::new();
    let mut next = 0;
    let mut out = Vec::new();
    for p in positions {
        while next < intervals.len() && intervals[next].start <= p {
            while stack
                .last()
                .is_some_and(|&t| intervals[t].end <= intervals[next].start)
            {
                stack.pop();
            }
            stack.push(next);
            next += 1;
        }
        while stack.last().is_some_and(|&t| intervals[t].end <= p) {
            stack.pop();
        }
        out.push(stack.last().map(|&t| t as u32));
    }
    out
}

/// `NO-ERROR`, calls, dynamic methods and `AVAILABLE` tests, per interval.
fn read_tokens(
    source: &str,
    toks: &[Tok],
    owner: &[Option<u32>],
    lookup: &dyn Fn(u32) -> Option<(BufferId, bool, u32)>,
    facts: &mut [Facts],
) {
    let text = |t: &Tok| source.get(t.start as usize..t.end as usize).unwrap_or("");
    for (i, t) in toks.iter().enumerate() {
        let Some(iv) = owner[i] else {
            continue;
        };
        let f = &mut facts[iv as usize];
        let next = toks.get(i + 1).map(|t| t.kind);
        match t.kind {
            Kind::NoError => f.no_error = true,
            Kind::Available => {
                let at = if next == Some(Kind::LeftParen) {
                    toks.get(i + 2)
                } else {
                    toks.get(i + 1)
                };
                if let Some((b, false, _)) = at.and_then(|a| lookup(a.start))
                    && !f.tested.contains(&b)
                {
                    f.tested.push(b);
                }
            }
            Kind::Run
            | Kind::Publish
            | Kind::Apply
            | Kind::WaitFor
            | Kind::Update
            | Kind::Set
            | Kind::PromptFor
            | Kind::Insert
            | Kind::Choose
            | Kind::Readkey
            | Kind::DynamicFunction
            | Kind::DynamicInvoke => f.calls = true,
            Kind::Identifier if next == Some(Kind::LeftParen) => f.calls = true,
            Kind::Colon | Kind::DoubleColon => {
                if let Some(name) = toks.get(i + 1) {
                    if toks.get(i + 2).map(|t| t.kind) == Some(Kind::LeftParen) {
                        f.calls = true;
                    }
                    if dynamic_method(text(name)) {
                        f.forget_all = true;
                    }
                }
            }
            _ => {}
        }
    }
}

/// A method that may change the record of a buffer whose handle it is called
/// on (K5).
fn dynamic_method(name: &str) -> bool {
    let is = |m: &str| name.eq_ignore_ascii_case(m);
    let starts = |p: &str| {
        name.len() > p.len()
            && name.is_char_boundary(p.len())
            && name[..p.len()].eq_ignore_ascii_case(p)
    };
    starts("find-")
        || starts("reposition-")
        || [
            "get-first",
            "get-last",
            "get-next",
            "get-prev",
            "get-current",
            "buffer-create",
            "buffer-copy",
            "buffer-delete",
            "buffer-release",
            "fill",
            "query-open",
            "raw-transfer",
            "synchronize",
            "empty-temp-table",
            "copy-temp-table",
        ]
        .iter()
        .any(|m| is(m))
}

/// Field uses and record-level writes, per interval.
fn read_references(
    refs: &[Reference],
    owner: &[Option<u32>],
    tracked: &dyn Fn(BufferId) -> bool,
    facts: &mut [Facts],
) {
    for (r, o) in refs.iter().zip(owner) {
        let Some(iv) = o else {
            continue;
        };
        let f = &mut facts[*iv as usize];
        let b = r.buffer;
        if r.passed || r.opaque || (!r.field && r.write.is_some()) {
            if !f.kills.contains(&b) {
                f.kills.push(b);
            }
            continue;
        }
        if !r.field {
            if !f.mentions.contains(&b) {
                f.mentions.push(b);
            }
        } else if tracked(b) && !f.sinks.iter().any(|(x, _, _)| *x == b) {
            f.sinks.push((b, r.at, r.write.is_some()));
        }
    }
}

/// The condition of an `IF` statement or of a `WHILE` block header.
fn condition(
    source: &str,
    toks: &[Tok],
    owner: &[Option<u32>],
    interval: &Interval,
    iv: usize,
    flow: &FileFlow,
    lookup: &dyn Fn(u32) -> Option<(BufferId, bool, u32)>,
) -> Option<(Cond, bool)> {
    let &(unit, node) = interval.nodes.first()?;
    let kind = flow.unit(unit).node(node).kind;
    let first = toks.partition_point(|t| t.start < interval.start);
    // The leading run of the interval's own tokens: the header.
    let run_end = (first..toks.len())
        .find(|&i| owner[i] != Some(iv as u32))
        .unwrap_or(toks.len());
    let run = &toks[first..run_end];
    match kind {
        NodeKind::Stmt if run.first().map(|t| t.kind) == Some(Kind::KwIf) => {
            let mut depth = 0i32;
            let mut nested = 0u32;
            let mut end = None;
            for (i, t) in run.iter().enumerate().skip(1) {
                match t.kind {
                    Kind::LeftParen | Kind::LeftBracket => depth += 1,
                    Kind::RightParen | Kind::RightBracket => depth -= 1,
                    Kind::KwIf if depth == 0 => nested += 1,
                    Kind::Then if depth == 0 && nested == 0 => {
                        end = Some(i);
                        break;
                    }
                    Kind::Then if depth == 0 => nested -= 1,
                    _ => {}
                }
            }
            let end = end?;
            Some((cond::read(&run[1..end], source, lookup), true))
        }
        NodeKind::Head => {
            let mut depth = 0i32;
            let mut to = false;
            let mut start = None;
            for (i, t) in run.iter().enumerate() {
                match t.kind {
                    Kind::LeftParen | Kind::LeftBracket => depth += 1,
                    Kind::RightParen | Kind::RightBracket => depth -= 1,
                    Kind::To if depth == 0 => to = true,
                    Kind::KwWhile if depth == 0 => {
                        start = Some(i + 1);
                        break;
                    }
                    Kind::Colon if depth == 0 => return None,
                    _ => {}
                }
            }
            let start = start?;
            let mut depth = 0i32;
            let end = run[start..]
                .iter()
                .position(|t| {
                    match t.kind {
                        Kind::LeftParen | Kind::LeftBracket => depth += 1,
                        Kind::RightParen | Kind::RightBracket => depth -= 1,
                        _ => {}
                    }
                    depth == 0
                        && matches!(
                            t.kind,
                            Kind::Colon | Kind::Period | Kind::On | Kind::Transaction | Kind::With
                        )
                })
                .map_or(run.len(), |p| start + p);
            Some((cond::read(&run[start..end], source, lookup), !to))
        }
        _ => None,
    }
}

/// The finds of one unit that may fail.
#[derive(Default)]
struct Sites {
    /// The site of the interval of a `FIND ... NO-ERROR`.
    of_iv: FxHashMap<u32, u32>,
    /// The buffer of each site.
    buffer: Vec<BufferId>,
    /// The index of each site in the file's finds.
    index: Vec<u32>,
    of_buffer: FxHashMap<BufferId, SmallVec<[u32; 2]>>,
    /// Per buffer, the buffers whose fields its finds read in their criteria.
    deps: FxHashMap<BufferId, SmallVec<[BufferId; 2]>>,
}

/// The finds that may have failed, and the finds that `ERROR-STATUS:ERROR`
/// being false proves (G5): on every path the find set `ERROR-STATUS` last,
/// or its buffer was refilled. A find not in `maybe` is always in
/// `error_status`.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Fact {
    maybe: BitSet,
    error_status: BitSet,
}

struct Problem<'a> {
    ivs: &'a [u32],
    facts: &'a [Facts],
    sites: &'a Sites,
    shared: &'a [BufferId],
}

impl Problem<'_> {
    fn kill(&self, fact: &mut Fact, b: BufferId) {
        if let Some(ss) = self.sites.of_buffer.get(&b) {
            for &s in ss {
                fact.maybe.remove(s as usize);
                fact.error_status.insert(s as usize);
            }
        }
    }

    /// A guard or a completed use proves `b` has a record (`kill` it too when
    /// asked), so did the buffers its finds read in their criteria, else they
    /// would have failed (G9).
    fn guarded(&self, fact: &mut Fact, b: BufferId, kill: bool) {
        if kill {
            self.kill(fact, b);
        }
        if let Some(deps) = self.sites.deps.get(&b) {
            for &d in deps {
                self.kill(fact, d);
            }
        }
    }

    fn step(&self, cfg: &Cfg, node: NodeIx, edge: Option<EdgeKind>, fact: &mut Fact) {
        // Error paths are not followed: where a statement raised, the error
        // is the first problem (K8).
        if matches!(edge, Some(EdgeKind::Raise(_))) {
            *fact = self.initial(cfg);
            return;
        }
        let iv = self.ivs[node.index()];
        if iv == u32::MAX {
            return;
        }
        let f = &self.facts[iv as usize];
        let opaque = matches!(
            cfg.node(node).opaque,
            Some(
                OpaqueReason::UnexpandedInclude
                    | OpaqueReason::UnrecognizedStatement
                    | OpaqueReason::StrayHandlerBlock
            )
        );
        if f.forget_all || opaque {
            let n = self.sites.buffer.len();
            fact.maybe = BitSet::new_empty(n);
            fact.error_status = BitSet::new_full(n);
            return;
        }
        if let Some((c, leaves_on_false)) = &f.cond {
            let proves = match edge {
                Some(EdgeKind::True | EdgeKind::LoopBody) => Some(&c.yes),
                Some(EdgeKind::False) => Some(&c.no),
                Some(EdgeKind::LoopDone) if *leaves_on_false => Some(&c.no),
                _ => None,
            };
            if let Some(p) = proves {
                if p.no_error {
                    let proven: SmallVec<[usize; 2]> = fact
                        .error_status
                        .iter()
                        .filter(|&s| fact.maybe.contains(s))
                        .collect();
                    for s in proven {
                        fact.maybe.remove(s);
                        self.guarded(fact, self.sites.buffer[s], false);
                    }
                }
                for &b in &p.buffers {
                    self.guarded(fact, b, true);
                }
            }
        }
        for &b in &f.kills {
            self.kill(fact, b);
        }
        for &b in &f.proven {
            self.guarded(fact, b, true);
        }
        if f.calls {
            for &b in self.shared {
                self.kill(fact, b);
            }
        }
        let site = self.sites.of_iv.get(&iv).copied();
        if let Some(s) = site {
            self.kill(fact, self.sites.buffer[s as usize]);
            fact.maybe.insert(s as usize);
        }
        if f.no_error {
            fact.error_status = BitSet::new_full(self.sites.buffer.len());
            for s in fact.maybe.iter() {
                if Some(s as u32) != site {
                    fact.error_status.remove(s);
                }
            }
        }
    }
}

impl Analysis for Problem<'_> {
    type Fact = Fact;

    fn direction(&self) -> Direction {
        Direction::Forward
    }

    fn initial(&self, _cfg: &Cfg) -> Fact {
        let n = self.sites.buffer.len();
        Fact {
            maybe: BitSet::new_empty(n),
            error_status: BitSet::new_full(n),
        }
    }

    fn boundary(&self, _cfg: &Cfg) -> Fact {
        let n = self.sites.buffer.len();
        Fact {
            maybe: BitSet::new_empty(n),
            error_status: BitSet::new_full(n),
        }
    }

    fn join(&self, acc: &mut Fact, other: &Fact) {
        acc.maybe.union_with(&other.maybe);
        acc.error_status.intersect_with(&other.error_status);
    }

    fn transfer(&self, cfg: &Cfg, node: NodeIx, fact: &mut Fact) {
        self.step(cfg, node, None, fact);
    }

    fn transfer_edge(&self, cfg: &Cfg, node: NodeIx, edge: &oxabl_flow::Edge, fact: &mut Fact) {
        self.step(cfg, node, Some(edge.kind), fact);
    }
}
