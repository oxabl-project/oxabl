//! Database effects in Progress ABL.
//!
//! [`analyze_file`] lists, for every unit of a file (the main block, internal
//! procedures, functions, methods, triggers), what it does to the database:
//! each record read with its lock (from `oxabl_scope`), each create, update
//! and delete, the access shape of each (once, or per row of the record loops
//! around it), and for each search the index the compiler is expected to use
//! with the reason, `WHOLE-INDEX` when no criterion brackets it, or Unknown
//! with a reason. It also places the file's calls (`oxabl_index::calls`) in
//! their units and shapes.
//!
//! [`summary::summarize`] combines the direct effects of many units over the
//! call graph into transitive summaries, with a fixed point over cycles.
//! [`xref`] parses the compiler's `COMPILE ... XREF` output and compares its
//! `SEARCH` rows with the model's index choices: the oracle for the index
//! rules.
//!
//! The ABL rules this follows, with confidence marks, are in
//! `docs/design/strict-analysis-open-questions.md`, section 8.
//!
//! # Unknown
//!
//! Nothing is guessed. A search whose index the model cannot tell is
//! [`Access::Unknown`] with a reason; a unit whose effects the file does not
//! show in full (a dynamic query, an unexpanded include, a statement the
//! parser skips) lists the reasons in [`UnitEffects::unknown`]; a call whose
//! target is not known is a [`CallTarget::Unknown`](oxabl_index::calls::CallTarget).

mod dump;
mod model;
mod phrase;
mod select;
pub mod summary;
pub mod xref;

pub use model::{
    Access, Effect, EffectUnknown, IndexChoice, IndexReason, IndexUnknown, Op, Shape, TieBreak,
    UnitCall, UnitEffects,
};

use oxabl_ast::{Span, Statement, StatementKind};
use oxabl_flow::{BlockId, BlockKind, FileFlow, UnitId};
use oxabl_index::calls::FileCalls;
use oxabl_lexer::{Kind, Token};
use oxabl_schema::Schema;
use oxabl_scope::{FileScopes, ReadKind, Table, UnknownReason, UpdateKind};
use rustc_hash::{FxHashMap, FxHashSet};

use phrase::Reader;
use select::{Context, Idx, Search, TableInfo};

/// Options for [`analyze_file`].
#[derive(Debug, Clone, Copy, Default)]
pub struct EffectOptions<'a> {
    /// Offsets in the source where the preprocessor replaced a reference with
    /// no text, sorted. One inside a record phrase makes its index Unknown.
    pub elided: &'a [u32],
}

/// The effects of one file.
#[derive(Debug, Clone, Default)]
pub struct FileEffects {
    units: Vec<UnitEffects>,
    effects: Vec<Effect>,
}

impl FileEffects {
    /// Every unit, in source pre-order; the main block is first.
    pub fn units(&self) -> &[UnitEffects] {
        &self.units
    }

    /// One unit.
    pub fn unit(&self, id: UnitId) -> &UnitEffects {
        &self.units[id.index()]
    }

    /// Every effect, in source order.
    pub fn effects(&self) -> &[Effect] {
        &self.effects
    }

    /// The effects of one unit, in source order.
    pub fn unit_effects(&self, id: UnitId) -> impl Iterator<Item = &Effect> {
        self.units[id.index()]
            .effects
            .iter()
            .map(|&i| &self.effects[i as usize])
    }

    /// A stable text rendering, for snapshot tests and debugging. `source` is
    /// the text that was analysed; sites show as `@line`.
    pub fn dump(&self, source: &str) -> String {
        dump::dump(self, source)
    }
}

/// Analyse a file. `tokens` are the tokens `program` was parsed from, `flow`
/// its flow graphs and `scopes` its scope analysis over them (see
/// `oxabl_scope::analyze_flow`), `calls` its calls; `schema` is `None` when
/// no schema is loaded.
#[allow(clippy::too_many_arguments)]
pub fn analyze_file(
    program: &[Statement],
    source: &str,
    tokens: &[Token],
    flow: &FileFlow,
    scopes: &FileScopes,
    calls: &FileCalls,
    schema: Option<&Schema>,
    options: &EffectOptions<'_>,
) -> FileEffects {
    let toks = phrase::significant(tokens);
    let reader = Reader {
        source,
        toks: &toks,
    };
    let decls = Decls::collect(program, &reader, schema);
    let loops = Loops::find(flow, scopes, &reader);
    let mut tables: FxHashMap<String, Option<TableInfo>> = FxHashMap::default();

    let mut units: Vec<UnitEffects> = flow
        .units()
        .iter()
        .map(|cfg| {
            let u = cfg.unit();
            UnitEffects {
                unit: cfg.id(),
                kind: u.kind,
                name: u.name.clone(),
                span: u.span,
                parent: u.parent,
                effects: Vec::new(),
                calls: Vec::new(),
                unknown: Vec::new(),
            }
        })
        .collect();
    let mut effects: Vec<Effect> = Vec::new();

    // Reads: one effect per read, with the index of each search.
    // Per statement: the phrases seen so far, and how many were EACH.
    let mut stmt_phrases: FxHashMap<oxabl_ast::NodeId, (u8, u8)> = FxHashMap::default();
    for read in scopes.reads() {
        let name_tok = reader.at(read.at.start);
        let statement = statement_span(flow, read.unit, read.stmt);
        let limit = statement.map_or(u32::MAX, |s| s.end);
        let phrase = name_tok.map(|i| reader.phrase(i, limit, read.kind == ReadKind::Find));
        let joined = matches!(
            read.kind,
            ReadKind::For | ReadKind::Preselect | ReadKind::OpenQuery
        );
        let (seen, earlier_each) = if joined {
            let slot = stmt_phrases.entry(read.stmt).or_insert((0, 0));
            let before = *slot;
            slot.0 += 1;
            if phrase.as_ref().is_some_and(|p| p.each) {
                slot.1 += 1;
            }
            before
        } else {
            (0, 0)
        };
        let first_phrase = seen == 0;
        let driver = is_driver(&reader, read, name_tok);
        let mut depth = match read.kind {
            ReadKind::For | ReadKind::Preselect => {
                loops.depth_outside(flow, read.unit, read.block) + earlier_each
            }
            ReadKind::OpenQuery => loops.depth(flow, read.unit, read.block) + earlier_each,
            _ => loops.depth(flow, read.unit, read.block),
        };
        let mut per_row_of = match read.kind {
            _ if earlier_each > 0 && read.kind != ReadKind::OpenQuery => Some(read.block),
            ReadKind::For | ReadKind::Preselect => {
                loops.innermost_outside(flow, read.unit, read.block)
            }
            _ => loops.innermost(flow, read.unit, read.block),
        };
        if driver && per_row_of.is_some_and(|b| loops.query_loops.contains(&(read.unit, b))) {
            depth = depth.saturating_sub(1);
            per_row_of = per_row_of.and_then(|b| {
                flow.unit(read.unit)
                    .block(b)
                    .parent
                    .and_then(|p| loops.innermost(flow, read.unit, p))
            });
        }
        let searches =
            !matches!(read.kind, ReadKind::Get) && !is_find_current(&reader, read, name_tok);
        let access = searches.then(|| match &phrase {
            None => Access::Unknown(IndexUnknown::Unreadable),
            Some(p) => {
                let hidden = matches!(read.lock, oxabl_scope::Lock::Unknown(_))
                    || elided_in(options.elided, read.at.start, p.end);
                if hidden {
                    return Access::Unknown(IndexUnknown::Unexpanded);
                }
                let by = if first_phrase
                    && read.kind != ReadKind::Find
                    && read.kind != ReadKind::CanFind
                {
                    name_tok.map_or_else(Vec::new, |i| reader.by_fields(i, limit))
                } else {
                    Vec::new()
                };
                access_of(read, p, by, &reader, scopes, schema, &decls, &mut tables)
            }
        });
        let shape = Shape::from_depth(depth);
        units[read.unit.index()].effects.push(effects.len() as u32);
        effects.push(Effect {
            unit: read.unit,
            block: read.block,
            at: read.at,
            statement: statement.unwrap_or(read.at),
            table: read.table.clone(),
            op: Op::Read,
            read: Some(read.kind),
            lock: Some(read.lock),
            shape,
            per_row_of: per_row_of.filter(|_| depth > 0),
            access,
        });
    }

    // Writes.
    for u in scopes.updates() {
        let op = match u.update.kind {
            UpdateKind::Create => Op::Create,
            UpdateKind::Delete => Op::Delete,
            UpdateKind::Assign | UpdateKind::BufferCopy | UpdateKind::Input => Op::Update,
            UpdateKind::ExclusiveRead => continue,
        };
        let Some(b) = u.update.buffer else {
            continue;
        };
        let depth = loops.depth(flow, u.unit, u.block);
        units[u.unit.index()].effects.push(effects.len() as u32);
        effects.push(Effect {
            unit: u.unit,
            block: u.block,
            at: u.update.at,
            statement: u.update.at,
            table: scopes.buffer(b).table.clone(),
            op,
            read: None,
            lock: None,
            shape: Shape::from_depth(depth),
            per_row_of: loops.innermost(flow, u.unit, u.block).filter(|_| depth > 0),
            access: None,
        });
    }
    for u in &mut units {
        u.effects.sort_by_key(|&i| effects[i as usize].at.start);
    }

    // Why a unit's effects may be incomplete.
    let mut unknown = |unit: UnitId, reason: EffectUnknown, at: Span| {
        let list = &mut units[unit.index()].unknown;
        match list.iter_mut().find(|(r, _)| *r == reason) {
            Some(slot) if at.start < slot.1.start => slot.1 = at,
            Some(_) => {}
            None => list.push((reason, at)),
        }
    };
    for w in scopes.write_unknowns() {
        if let Some(r) = effect_unknown(w.unknown.reason) {
            unknown(w.unit, r, w.unknown.at);
        }
    }
    for (at, unit) in dynamic_access(&reader)
        .into_iter()
        .filter_map(|at| locate(flow, at.start).map(|(u, _)| (at, u)))
    {
        unknown(unit, EffectUnknown::Dynamic, at);
    }
    for f in scopes.unknown() {
        if let Some(r) = effect_unknown(f.reason) {
            for cfg in flow.units() {
                unknown(cfg.id(), r, f.at);
            }
        }
    }
    for u in &mut units {
        u.unknown.sort_by_key(|(r, at)| (*r, at.start));
    }

    // Calls.
    for site in &calls.sites {
        let Some((unit, block)) = locate(flow, site.at.start) else {
            continue;
        };
        let depth = loops.depth(flow, unit, block);
        units[unit.index()].calls.push(UnitCall {
            at: site.at,
            block,
            kind: site.kind,
            target: site.target.clone(),
            shape: Shape::from_depth(depth),
        });
    }

    effects_sorted(units, effects)
}

/// Renumber effects into source order.
fn effects_sorted(mut units: Vec<UnitEffects>, effects: Vec<Effect>) -> FileEffects {
    let mut order: Vec<u32> = (0..effects.len() as u32).collect();
    order.sort_by_key(|&i| (effects[i as usize].at.start, i));
    let mut new_of = vec![0u32; effects.len()];
    for (new, &old) in order.iter().enumerate() {
        new_of[old as usize] = new as u32;
    }
    for u in &mut units {
        for e in &mut u.effects {
            *e = new_of[*e as usize];
        }
        u.effects.sort_unstable();
    }
    let mut slots: Vec<Option<Effect>> = effects.into_iter().map(Some).collect();
    let effects = order
        .iter()
        .map(|&i| slots[i as usize].take().expect("each effect once"))
        .collect();
    FileEffects { units, effects }
}

fn effect_unknown(reason: UnknownReason) -> Option<EffectUnknown> {
    Some(match reason {
        UnknownReason::DynamicBuffer => EffectUnknown::Dynamic,
        UnknownReason::SkippedStatement => EffectUnknown::SkippedStatement,
        UnknownReason::UnknownTable => EffectUnknown::UnknownTable,
        UnknownReason::NoSchema => EffectUnknown::NoSchema,
        UnknownReason::UnexpandedInclude => EffectUnknown::UnexpandedInclude,
        UnknownReason::RecoveredParse => EffectUnknown::RecoveredParse,
        UnknownReason::TooComplex => EffectUnknown::TooComplex,
        _ => return None,
    })
}

fn elided_in(elided: &[u32], from: u32, to: u32) -> bool {
    let i = elided.partition_point(|&o| o < from);
    elided.get(i).is_some_and(|&o| o < to)
}

/// The span of a statement, from its flow nodes.
fn statement_span(flow: &FileFlow, unit: UnitId, stmt: oxabl_ast::NodeId) -> Option<Span> {
    let cfg = flow.unit(unit);
    let nodes = cfg.nodes_of(stmt);
    let start = nodes.iter().map(|&n| cfg.node(n).span.start).min()?;
    let end = nodes.iter().map(|&n| cfg.node(n).span.end).max()?;
    Some(Span { start, end })
}

/// `FIND CURRENT`: re-reads the record in the buffer, no search.
fn is_find_current(
    reader: &Reader<'_>,
    read: &oxabl_scope::RecordRead,
    name: Option<usize>,
) -> bool {
    read.kind == ReadKind::Find
        && name.is_some_and(|i| i > 0 && reader.toks[i - 1].kind == Kind::Current)
}

/// `FIND NEXT`/`PREV` and `GET NEXT`/`PREV`: the read that drives a query
/// loop.
fn is_driver(reader: &Reader<'_>, read: &oxabl_scope::RecordRead, name: Option<usize>) -> bool {
    match read.kind {
        ReadKind::Find => name
            .is_some_and(|i| i > 0 && matches!(reader.toks[i - 1].kind, Kind::Next | Kind::Prev)),
        ReadKind::Get => get_moves(reader, read.at.start),
        _ => false,
    }
}

/// Whether the `GET` statement around offset `at` is `GET NEXT` or `GET PREV`.
fn get_moves(reader: &Reader<'_>, at: u32) -> bool {
    let i = reader.toks.partition_point(|t| t.start < at);
    reader.toks[..i.min(reader.toks.len())]
        .iter()
        .rev()
        .take(64)
        .position(|t| t.kind == Kind::Get)
        .is_some_and(|back| {
            let g = i - 1 - back;
            reader
                .toks
                .get(g + 1)
                .is_some_and(|t| matches!(t.kind, Kind::Next | Kind::Prev))
        })
}

/// The innermost unit and block whose text holds the offset.
fn locate(flow: &FileFlow, at: u32) -> Option<(UnitId, BlockId)> {
    let inside = |span: Span| span.start <= at && at < span.end;
    let cfg = flow
        .units()
        .iter()
        .filter(|c| inside(c.unit().span) || c.unit().parent.is_none())
        .max_by_key(|c| (inside(c.unit().span), c.unit().span.start))?;
    let block = cfg
        .blocks()
        .filter(|(_, b)| inside(b.span))
        .max_by_key(|(_, b)| b.span.start)
        .map_or(BlockId::ROUTINE, |(id, _)| id);
    Some((cfg.id(), block))
}

/// Dynamic database access the compiler does not see: `CREATE QUERY`,
/// `CREATE BUFFER`, and the methods of dynamic queries, buffers and datasets
/// that read or write records. Most work on temp-tables, which the model
/// cannot tell apart without following the handle.
fn dynamic_access(reader: &Reader<'_>) -> Vec<Span> {
    const METHODS: [&str; 15] = [
        "query-prepare",
        "query-open",
        "get-first",
        "get-next",
        "get-prev",
        "get-last",
        "find-first",
        "find-last",
        "find-unique",
        "find-by-rowid",
        "find-current",
        "buffer-create",
        "buffer-delete",
        "fill",
        "save-row-changes",
    ];
    let toks = reader.toks;
    let mut out = Vec::new();
    for i in 0..toks.len() {
        let t = toks[i];
        let span = Span {
            start: t.start,
            end: t.end,
        };
        let create = t.kind == Kind::Create
            && toks
                .get(i + 1)
                .is_some_and(|n| matches!(n.kind, Kind::Query | Kind::Buffer));
        let method = t.kind == Kind::Colon
            && toks.get(i + 1).is_some_and(|n| n.start == t.end)
            && toks.get(i + 2).is_some_and(|n| n.kind == Kind::LeftParen)
            && METHODS
                .iter()
                .any(|m| reader.text(i + 1).eq_ignore_ascii_case(m));
        if create || method {
            out.push(span);
        }
    }
    out
}

/// The record loops of each unit (section 8.3): `FOR` blocks with an `EACH`
/// phrase, and iterating blocks that `GET NEXT`/`PREV` or `FIND NEXT`/`PREV`
/// drive (query loops).
struct Loops {
    loops: FxHashSet<(UnitId, BlockId)>,
    query_loops: FxHashSet<(UnitId, BlockId)>,
}

impl Loops {
    fn find(flow: &FileFlow, scopes: &FileScopes, reader: &Reader<'_>) -> Loops {
        let mut loops = FxHashSet::default();
        let mut query_loops = FxHashSet::default();
        for cfg in flow.units() {
            for (id, b) in cfg.blocks() {
                if matches!(b.kind, BlockKind::For(_)) && header_has_each(reader, b.span) {
                    loops.insert((cfg.id(), id));
                }
            }
        }
        let mut mark = |unit: UnitId, block: BlockId| {
            let cfg = flow.unit(unit);
            let mut b = Some(block);
            while let Some(id) = b {
                let blk = cfg.block(id);
                if blk.iterating {
                    if !matches!(blk.kind, BlockKind::For(_)) {
                        loops.insert((unit, id));
                        query_loops.insert((unit, id));
                    }
                    return;
                }
                b = blk.parent;
            }
        };
        for read in scopes.reads() {
            let name = reader.at(read.at.start);
            if is_driver(reader, read, name) {
                mark(read.unit, read.block);
            }
        }
        let toks = reader.toks;
        for i in 0..toks.len() {
            if toks[i].kind == Kind::Colon
                && toks.get(i + 1).is_some_and(|n| n.start == toks[i].end)
                && ["get-next", "get-prev"]
                    .iter()
                    .any(|m| reader.text(i + 1).eq_ignore_ascii_case(m))
                && let Some((unit, block)) = locate(flow, toks[i].start)
            {
                mark(unit, block);
            }
        }
        Loops { loops, query_loops }
    }

    /// Record loops among `block` and the blocks around it in the unit.
    fn depth(&self, flow: &FileFlow, unit: UnitId, block: BlockId) -> u8 {
        let cfg = flow.unit(unit);
        std::iter::successors(Some(block), |b| cfg.block(*b).parent)
            .filter(|b| self.loops.contains(&(unit, *b)))
            .count()
            .min(u8::MAX as usize) as u8
    }

    /// Record loops around `block`, not counting it.
    fn depth_outside(&self, flow: &FileFlow, unit: UnitId, block: BlockId) -> u8 {
        match flow.unit(unit).block(block).parent {
            Some(p) => self.depth(flow, unit, p),
            None => 0,
        }
    }

    fn innermost(&self, flow: &FileFlow, unit: UnitId, block: BlockId) -> Option<BlockId> {
        let cfg = flow.unit(unit);
        std::iter::successors(Some(block), |b| cfg.block(*b).parent)
            .find(|b| self.loops.contains(&(unit, *b)))
    }

    fn innermost_outside(&self, flow: &FileFlow, unit: UnitId, block: BlockId) -> Option<BlockId> {
        flow.unit(unit)
            .block(block)
            .parent
            .and_then(|p| self.innermost(flow, unit, p))
    }
}

/// Whether a `FOR` header has an `EACH` phrase (at depth 0, before the colon
/// that opens the body).
fn header_has_each(reader: &Reader<'_>, span: Span) -> bool {
    let from = reader.toks.partition_point(|t| t.start < span.start);
    let mut depth = 0i32;
    for t in &reader.toks[from..] {
        if t.start >= span.end {
            break;
        }
        match t.kind {
            Kind::LeftParen | Kind::LeftBracket => depth += 1,
            Kind::RightParen | Kind::RightBracket => depth -= 1,
            Kind::Each if depth == 0 => return true,
            Kind::Colon | Kind::Period if depth == 0 => {
                let next = reader.source[t.end as usize..].chars().next();
                if next.is_none_or(char::is_whitespace) {
                    return false;
                }
            }
            _ => {}
        }
    }
    false
}

/// What the file declares that index selection needs: variable names, and the
/// temp-tables with their indexes.
struct Decls {
    variables: FxHashSet<String>,
    temp_tables: FxHashMap<String, TableInfo>,
}

impl Decls {
    fn collect(program: &[Statement], reader: &Reader<'_>, schema: Option<&Schema>) -> Decls {
        let mut d = Decls {
            variables: FxHashSet::default(),
            temp_tables: FxHashMap::default(),
        };
        walk(program, &mut |s| match &s.kind {
            StatementKind::VariableDeclaration { name, .. } => {
                d.variables.insert(name.name.to_ascii_lowercase());
            }
            StatementKind::DefineTempTable {
                name,
                like_table,
                use_indexes,
                fields,
                indexes,
                ..
            } => {
                let mut info = TableInfo::default();
                if let Some(like) = like_table {
                    let like_info = d
                        .temp_tables
                        .get(&like.name.to_ascii_lowercase())
                        .cloned()
                        .or_else(|| {
                            schema
                                .and_then(|s| s.table(last(&like.name)))
                                .map(TableInfo::from_schema)
                        });
                    if let Some(l) = like_info {
                        info.fields = l.fields;
                        if indexes.is_empty() && use_indexes.is_empty() {
                            info.indexes = l.indexes;
                        } else {
                            for u in use_indexes {
                                if let Some(i) = l
                                    .indexes
                                    .iter()
                                    .find(|i| i.name.eq_ignore_ascii_case(&u.name.name))
                                {
                                    let mut i = i.clone();
                                    i.primary = u.as_primary;
                                    info.indexes.push(i);
                                }
                            }
                        }
                    } else {
                        // The source table is not known: neither are the fields.
                        return;
                    }
                }
                info.fields
                    .extend(fields.iter().map(|f| f.name.name.to_ascii_lowercase()));
                for i in indexes {
                    info.indexes.push(Idx {
                        name: i.name.name.clone(),
                        unique: i.is_unique,
                        primary: i.is_primary,
                        word: i.is_word_index,
                        inactive: false,
                        components: i
                            .fields
                            .iter()
                            .map(|f| f.name.name.to_ascii_lowercase())
                            .collect(),
                    });
                }
                if info.indexes.is_empty() {
                    info.indexes.push(Idx {
                        name: "default".to_string(),
                        unique: false,
                        primary: true,
                        word: false,
                        inactive: false,
                        components: Vec::new(),
                    });
                } else if !info.indexes.iter().any(|i| i.primary) {
                    info.indexes[0].primary = true;
                }
                d.temp_tables.insert(name.name.to_ascii_lowercase(), info);
            }
            _ => {}
        });
        // Parameters: the word after PARAMETER.
        for i in 0..reader.toks.len() {
            if reader.toks[i].kind == Kind::Parameter
                && let Some(n) = reader.toks.get(i + 1)
                && !matches!(
                    n.kind,
                    Kind::Buffer | Kind::Table | Kind::TableHandle | Kind::Dataset
                )
            {
                d.variables.insert(reader.text(i + 1).to_ascii_lowercase());
            }
        }
        d
    }
}

fn last(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// The table information for a read's table.
fn table_info<'t>(
    table: &Table,
    schema: Option<&Schema>,
    decls: &Decls,
    cache: &'t mut FxHashMap<String, Option<TableInfo>>,
) -> Result<&'t TableInfo, IndexUnknown> {
    let key = match table {
        Table::TempTable { name, .. } => format!("tt:{}", name.to_ascii_lowercase()),
        Table::Database(name) => format!("db:{}", name.to_ascii_lowercase()),
        Table::Unknown(_) => {
            return Err(if schema.is_none() {
                IndexUnknown::NoSchema
            } else {
                IndexUnknown::UnknownTable
            });
        }
    };
    let entry = cache.entry(key).or_insert_with(|| match table {
        Table::TempTable { name, .. } => decls.temp_tables.get(&name.to_ascii_lowercase()).cloned(),
        Table::Database(name) => schema
            .and_then(|s| s.table(last(name)))
            .map(TableInfo::from_schema),
        Table::Unknown(_) => None,
    });
    entry.as_ref().ok_or(IndexUnknown::UnknownTable)
}

#[allow(clippy::too_many_arguments)]
fn access_of(
    read: &oxabl_scope::RecordRead,
    phrase: &phrase::Phrase,
    by: Vec<String>,
    reader: &Reader<'_>,
    scopes: &FileScopes,
    schema: Option<&Schema>,
    decls: &Decls,
    cache: &mut FxHashMap<String, Option<TableInfo>>,
) -> Access {
    let of_fields = match &phrase.of {
        None => Vec::new(),
        Some(of) => {
            let other = scopes
                .buffers()
                .iter()
                .find(|b| b.name.eq_ignore_ascii_case(last(of)))
                .map(|b| b.table.clone());
            let Some(other) = other else {
                return Access::Unknown(IndexUnknown::OfJoin);
            };
            let other = match table_info(&other, schema, decls, cache) {
                Ok(t) => t.clone(),
                Err(_) => return Access::Unknown(IndexUnknown::OfJoin),
            };
            let own = match table_info(&read.table, schema, decls, cache) {
                Ok(t) => t,
                Err(u) => return Access::Unknown(u),
            };
            match of_join(own, &other) {
                Some(f) => f,
                None => return Access::Unknown(IndexUnknown::OfJoin),
            }
        }
    };
    let info = match table_info(&read.table, schema, decls, cache) {
        Ok(t) => t,
        Err(u) => return Access::Unknown(u),
    };
    let ctx = Context {
        reader,
        buffer: &read.name,
        table: info,
        variables: &decls.variables,
    };
    // BY fields of the table, in order, up to the first that is not one.
    let mut by_fields = Vec::new();
    for b in &by {
        let field = match b.rsplit_once('.') {
            Some((q, f)) if last(q).eq_ignore_ascii_case(last(&read.name)) => info.field(f),
            Some(_) => None,
            None if !decls.variables.contains(&b.to_ascii_lowercase()) => info.field(b),
            None => None,
        };
        match field {
            Some(f) => by_fields.push(f.to_string()),
            None => break,
        }
    }
    let search = Search {
        phrase,
        multi: matches!(
            read.kind,
            ReadKind::For | ReadKind::Preselect | ReadKind::OpenQuery
        ),
        by: by_fields,
        of_fields,
    };
    select::choose(&ctx, &search)
}

/// The fields an `OF` join matches by equality: the components of a unique
/// index of the other table that the own table has, else of a unique index of
/// the own table that the other has.
fn of_join(own: &TableInfo, other: &TableInfo) -> Option<Vec<String>> {
    let covered = |idx: &Idx, by: &TableInfo| {
        idx.unique
            && !idx.components.is_empty()
            && idx.components.iter().all(|c| by.fields.contains(c))
    };
    other
        .indexes
        .iter()
        .find(|i| covered(i, own))
        .or_else(|| own.indexes.iter().find(|i| covered(i, other)))
        .map(|i| i.components.clone())
}

/// Visit every statement, nested bodies included.
fn walk(stmts: &[Statement], f: &mut impl FnMut(&Statement)) {
    for s in stmts {
        f(s);
        match &s.kind {
            StatementKind::Procedure { body, .. }
            | StatementKind::Function { body, .. }
            | StatementKind::Class { body, .. }
            | StatementKind::Destructor { body }
            | StatementKind::Finally { body }
            | StatementKind::Catch { body, .. }
            | StatementKind::Block(body)
            | StatementKind::VariableDeclarationList(body)
            | StatementKind::Method { body, .. }
            | StatementKind::Constructor { body, .. }
            | StatementKind::Do { body, .. }
            | StatementKind::Repeat { body, .. }
            | StatementKind::ForEach { body, .. } => walk(body, f),
            StatementKind::Property {
                get_body, set_body, ..
            } => {
                for body in [get_body, set_body].into_iter().flatten() {
                    walk(body, f);
                }
            }
            StatementKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                walk(std::slice::from_ref(then_branch), f);
                if let Some(e) = else_branch {
                    walk(std::slice::from_ref(e), f);
                }
            }
            StatementKind::Case {
                when_branches,
                otherwise,
                ..
            } => {
                for wb in when_branches {
                    walk(&wb.body, f);
                }
                if let Some(o) = otherwise {
                    walk(o, f);
                }
            }
            StatementKind::Label { body, .. } => walk(std::slice::from_ref(body), f),
            StatementKind::On {
                kind:
                    oxabl_ast::OnKind::UiEvent {
                        action: oxabl_ast::OnAction::Block(body),
                        ..
                    }
                    | oxabl_ast::OnKind::DbEvent {
                        action: oxabl_ast::OnAction::Block(body),
                        ..
                    },
            } => walk(std::slice::from_ref(body), f),
            StatementKind::PreprocIf(pif) => {
                walk(&pif.then_branch, f);
                for (_, b) in &pif.elseif_branches {
                    walk(b, f);
                }
                if let Some(b) = &pif.else_branch {
                    walk(b, f);
                }
            }
            _ => {}
        }
    }
}
