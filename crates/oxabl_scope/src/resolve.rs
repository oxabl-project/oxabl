//! Name resolution: which buffer each collected reference names.
//!
//! A name in a buffer position (`FIND b`, `FOR EACH b`) is looked up as a
//! buffer of the unit, then of its enclosing units, then as a temp-table, then
//! as a table of the schema. Such a name always gives a buffer: one the file
//! does not know is a default buffer of an unknown table. A name in an
//! expression gives a buffer only when one is known by then. A bare word is a
//! variable first, then a buffer, then a field (section 7.2, S11).

use oxabl_ast::Span;
use oxabl_flow::{BlockId, FileFlow, UnitId};
use oxabl_schema::{FieldResolution, Schema};
use std::cell::RefCell;

use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::collect::{
    Collected, DefBuffer, Form, Phrase, RawRef, ReadTarget, Role, What, fold, last_segment,
};
use crate::model::{
    Buffer, BufferId, BufferKind, Lock, LockMode, ReadKind, RecordRead, Table, Unknown,
    UnknownReason, Update, UpdateKind, Written,
};

/// A resolved reference.
#[derive(Debug, Clone)]
pub(crate) struct Ref {
    pub buffer: BufferId,
    pub unit: UnitId,
    pub block: BlockId,
    pub role: Role,
    pub at: Span,
    pub write: Option<UpdateKind>,
    /// Passed to a call as `BUFFER b`.
    pub passed: bool,
}

#[derive(Debug, Default)]
pub(crate) struct Resolved {
    pub buffers: Vec<Buffer>,
    pub refs: Vec<Ref>,
    pub reads: Vec<RecordRead>,
    /// Database updates with the block whose statement holds them.
    pub updates: Vec<(UnitId, BlockId, Update)>,
    /// Things that may make a block a transaction block.
    pub tx_unknowns: Vec<(UnitId, BlockId, Unknown)>,
    /// Things that make the scopes of one buffer Unknown.
    pub buffer_unknowns: Vec<(BufferId, Unknown)>,
    /// Every `RUN`: its unit and where it starts.
    pub runs: Vec<(UnitId, u32)>,
    /// Things that make the scopes of every buffer visible in a unit Unknown,
    /// with the tables that may be affected: all (`None`), or database ones.
    pub unit_unknowns: Vec<(UnitId, Unknown)>,
}

/// What a lookup of a field in one buffer's table gives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Has {
    Exact,
    Prefix,
    /// The table is not known: it may have the field.
    Maybe,
    No,
}

/// The answer for a bare field at one level of the search.
enum Level {
    Hit(BufferId),
    Private,
    Ambiguous(SmallVec<[BufferId; 2]>),
    Maybe(SmallVec<[BufferId; 2]>),
    Miss,
}

/// Values by folded name, each for the unit that defines it.
struct ByName<T>(FxHashMap<String, SmallVec<[(UnitId, T); 1]>>);

impl<T> Default for ByName<T> {
    fn default() -> Self {
        ByName(FxHashMap::default())
    }
}

impl<T: Copy> ByName<T> {
    /// Add `value` for `name` in `unit`, unless the unit has one already.
    fn insert_first(&mut self, name: String, unit: UnitId, value: T) {
        let entries = self.0.entry(name).or_default();
        if !entries.iter().any(|(u, _)| *u == unit) {
            entries.push((unit, value));
        }
    }

    /// The value for `name` in exactly `unit`.
    fn get(&self, name: &str, unit: UnitId) -> Option<T> {
        self.0
            .get(name)?
            .iter()
            .find(|(u, _)| *u == unit)
            .map(|(_, v)| *v)
    }
}

pub(crate) fn resolve(c: &Collected, flow: &FileFlow, schema: Option<&Schema>) -> Resolved {
    let mut r = Resolver::new(c, flow, schema);
    r.run();
    r.out
}

struct Resolver<'a> {
    c: &'a Collected,
    schema: Option<&'a Schema>,
    parent: Vec<Option<UnitId>>,
    defined: ByName<usize>,
    temp_tables: ByName<usize>,
    variables: ByName<()>,
    /// Schema tables by folded name, and their fields by folded word.
    tables: RefCell<FxHashMap<String, Option<&'a oxabl_schema::Table>>>,
    fields: RefCell<FxHashMap<String, FxHashMap<String, Has>>>,
    /// The buffer made for each `DefBuffer`.
    of_def: Vec<Option<BufferId>>,
    /// Default buffers by folded table name and owner.
    defaults: ByName<BufferId>,
    /// The temp-table of each temp-table buffer (index into `c.temp_tables`).
    temp_table_of: FxHashMap<BufferId, usize>,
    /// The resolved buffer of each raw reference (`None`: not a buffer).
    raw: Vec<Option<BufferId>>,
    /// The buffer a bare field falls back to, per word and unit.
    memo: ByName<Option<BufferId>>,
    named: Vec<BufferId>,
    out: Resolved,
}

impl<'a> Resolver<'a> {
    fn new(c: &'a Collected, flow: &'a FileFlow, schema: Option<&'a Schema>) -> Self {
        let parent = flow.units().iter().map(|u| u.unit().parent).collect();
        let mut defined = ByName::default();
        for (i, d) in c.buffers.iter().enumerate() {
            defined.insert_first(fold(&d.name), d.unit, i);
        }
        let mut temp_tables = ByName::default();
        for (i, t) in c.temp_tables.iter().enumerate() {
            temp_tables.insert_first(fold(&t.name), t.unit, i);
        }
        let mut variables = ByName::default();
        for (u, v) in &c.variables {
            variables.insert_first(v.clone(), *u, ());
        }
        Resolver {
            c,
            schema,
            parent,
            defined,
            temp_tables,
            variables,
            tables: RefCell::default(),
            fields: RefCell::default(),
            of_def: vec![None; c.buffers.len()],
            defaults: ByName::default(),
            temp_table_of: FxHashMap::default(),
            raw: vec![None; c.refs.len()],
            memo: ByName::default(),
            named: Vec::new(),
            out: Resolved::default(),
        }
    }

    fn chain(&self, unit: UnitId) -> impl Iterator<Item = UnitId> + '_ {
        std::iter::successors(Some(unit), |u| self.parent[u.index()])
    }

    /// The entry of `map` for `name` in `unit` or the closest unit around it.
    fn find<T: Copy>(&self, map: &ByName<T>, name: &str, unit: UnitId) -> Option<T> {
        let entries = map.0.get(name)?;
        self.chain(unit)
            .find_map(|u| entries.iter().find(|(o, _)| *o == u).map(|(_, v)| *v))
    }

    fn schema_table(&self, key: &str) -> Option<&'a oxabl_schema::Table> {
        let schema = self.schema?;
        if let Some(t) = self.tables.borrow().get(key) {
            return *t;
        }
        let t = schema.table(key);
        self.tables.borrow_mut().insert(key.to_string(), t);
        t
    }

    /// Whether `owner` is `unit` or encloses it.
    fn visible(&self, owner: UnitId, unit: UnitId) -> bool {
        self.chain(unit).any(|u| u == owner)
    }

    fn run(&mut self) {
        let c = self.c;
        // Definitions make their buffers whether or not they are used, so a
        // buffer parameter or a shared buffer shows in the model.
        for i in 0..c.buffers.len() {
            self.def_buffer(i);
        }
        // Buffer positions first: they make the default buffers that the
        // expressions may then name.
        for (i, r) in c.refs.iter().enumerate() {
            if let What::Buffer(name) = &r.what {
                self.raw[i] = Some(self.buffer_named(name, r.unit, true));
            }
        }
        for (i, r) in c.refs.iter().enumerate() {
            if let What::Dotted(words) = &r.what {
                self.raw[i] = self.dotted(words, r);
            }
        }
        let mut named: Vec<BufferId> = self.raw.iter().flatten().copied().collect();
        named.sort_unstable();
        named.dedup();
        self.named = named;
        for (i, r) in c.refs.iter().enumerate() {
            if let What::Bare { word, keyword } = &r.what {
                self.raw[i] = self.bare(word, *keyword, r);
            }
        }
        self.finish_refs();
        self.reads();
        self.out.runs = c.runs.clone();
        for u in &c.unknowns {
            let unknown = Unknown {
                reason: u.reason,
                at: u.at,
            };
            self.out.tx_unknowns.push((u.unit, u.block, unknown));
            if u.scopes {
                self.out.unit_unknowns.push((u.unit, unknown));
            }
        }
    }

    // ----- buffers -----

    fn new_buffer(
        &mut self,
        name: &str,
        kind: BufferKind,
        table: Table,
        owner: UnitId,
        defined: Option<Span>,
    ) -> BufferId {
        let id = BufferId(self.out.buffers.len() as u32);
        self.out.buffers.push(Buffer {
            id,
            name: name.to_string(),
            kind,
            table,
            owner,
            defined,
            seen: Vec::new(),
            unknown: Vec::new(),
            references: 0,
        });
        id
    }

    fn def_buffer(&mut self, i: usize) -> BufferId {
        if let Some(id) = self.of_def[i] {
            return id;
        }
        let d: &DefBuffer = &self.c.buffers[i];
        let key = fold(last_segment(&d.table));
        let tt = self.temp_table_in(&key, d.unit);
        let table = match tt {
            Some(t) => {
                let t = &self.c.temp_tables[t];
                Table::TempTable {
                    name: t.name.clone(),
                    no_undo: t.no_undo,
                }
            }
            None if d.temp_table => Table::TempTable {
                name: d.table.clone(),
                no_undo: false,
            },
            None => self.db_table(&key, last_segment(&d.table)),
        };
        let id = self.new_buffer(&d.name, d.kind, table, d.unit, Some(d.at));
        if let Some(t) = tt {
            self.temp_table_of.insert(id, t);
        }
        self.of_def[i] = Some(id);
        id
    }

    fn temp_table_in(&self, key: &str, unit: UnitId) -> Option<usize> {
        self.find(&self.temp_tables, key, unit)
    }

    fn db_table(&self, key: &str, written: &str) -> Table {
        match self.schema_table(key) {
            Some(t) => Table::Database(t.display_name.clone()),
            None => Table::Unknown(written.to_string()),
        }
    }

    /// The buffer a name gives from `unit`. With `create`, a name the file
    /// does not know becomes the default buffer of an unknown table.
    fn lookup(&mut self, name: &str, unit: UnitId, create: bool) -> Option<BufferId> {
        let written = last_segment(name);
        let folded;
        let key = if written.bytes().any(|b| b.is_ascii_uppercase()) {
            folded = fold(written);
            folded.as_str()
        } else {
            written
        };
        if let Some(i) = self.find(&self.defined, key, unit) {
            return Some(self.def_buffer(i));
        }
        if let Some(t) = self.temp_table_in(key, unit) {
            let owner = self.c.temp_tables[t].unit;
            if let Some(id) = self.defaults.get(key, owner) {
                return Some(id);
            }
            let tt = &self.c.temp_tables[t];
            let table = Table::TempTable {
                name: tt.name.clone(),
                no_undo: tt.no_undo,
            };
            let id = self.new_buffer(&tt.name.clone(), BufferKind::Default, table, owner, None);
            self.temp_table_of.insert(id, t);
            self.defaults.insert_first(key.to_string(), owner, id);
            return Some(id);
        }
        let main = UnitId::from_index(0);
        if let Some(id) = self.defaults.get(key, main) {
            return Some(id);
        }
        let known = self.schema_table(key).is_some();
        if !known && !create {
            return None;
        }
        let table = self.db_table(key, written);
        let id = self.new_buffer(written, BufferKind::Default, table, main, None);
        self.defaults.insert_first(key.to_string(), main, id);
        Some(id)
    }

    fn buffer_named(&mut self, name: &str, unit: UnitId, create: bool) -> BufferId {
        self.lookup(name, unit, create)
            .expect("a buffer position always gives a buffer")
    }

    // ----- expressions -----

    fn is_variable(&self, word: &str, unit: UnitId) -> bool {
        self.find(&self.variables, word, unit).is_some()
    }

    /// The private `CAN-FIND` buffers of a reference's context.
    fn privates(&self, r: &RawRef) -> SmallVec<[&'a str; 1]> {
        self.c.contexts[r.context as usize]
            .phrase
            .iter()
            .filter_map(|p| match p {
                Phrase::Private(i) => Some(self.c.privates[*i as usize].1.as_str()),
                Phrase::Ref(_) => None,
            })
            .collect()
    }

    fn dotted(&mut self, words: &[String], r: &RawRef) -> Option<BufferId> {
        let qualifier = match words.len() {
            2 => &words[0],
            3 => &words[1],
            _ => return None,
        };
        if self.privates(r).contains(&qualifier.as_str()) {
            return None;
        }
        if let Some(b) = self.lookup(qualifier, r.unit, false) {
            return Some(b);
        }
        if words.len() == 2 {
            // `db.table` in an expression.
            if let Some(b) = self.lookup(&words[1], r.unit, false) {
                return Some(b);
            }
            if self.schema.is_none() {
                self.no_schema(r);
            }
        }
        None
    }

    fn no_schema(&mut self, r: &RawRef) {
        let unknown = Unknown {
            reason: UnknownReason::NoSchema,
            at: r.at,
        };
        self.out.unit_unknowns.push((r.unit, unknown));
        if r.write.is_some() {
            self.out.tx_unknowns.push((r.unit, r.block, unknown));
        }
    }

    fn bare(&mut self, word: &str, keyword: bool, r: &RawRef) -> Option<BufferId> {
        if self.is_variable(word, r.unit) {
            return None;
        }
        if self.privates(r).contains(&word) {
            return None;
        }
        if let Some(b) = self.lookup(word, r.unit, false) {
            return Some(b);
        }
        // The statement's own record phrases, then the enclosing record
        // blocks, innermost first.
        let context = &self.c.contexts[r.context as usize];
        let mut levels: SmallVec<[SmallVec<[Option<BufferId>; 2]>; 4]> = SmallVec::new();
        levels.push(
            context
                .phrase
                .iter()
                .map(|p| match p {
                    Phrase::Ref(i) => self.raw[*i as usize],
                    Phrase::Private(_) => None,
                })
                .collect(),
        );
        for b in &context.enclosing {
            let phrase = self
                .c
                .block_phrases
                .get(&(r.unit, *b))
                .map(|refs| refs.iter().map(|&i| self.raw[i as usize]).collect())
                .unwrap_or_default();
            levels.push(phrase);
        }
        for (depth, level) in levels.iter().enumerate() {
            let privates: SmallVec<[usize; 1]> = if depth == 0 {
                context
                    .phrase
                    .iter()
                    .filter_map(|p| match p {
                        Phrase::Private(i) => Some(*i as usize),
                        Phrase::Ref(_) => None,
                    })
                    .collect()
            } else {
                SmallVec::new()
            };
            let candidates: SmallVec<[BufferId; 4]> = level.iter().flatten().copied().collect();
            match self.level(word, keyword, &candidates, &privates, r.unit) {
                Level::Hit(b) => return Some(b),
                Level::Private => return None,
                Level::Ambiguous(bs) => return self.ambiguous(&bs, r),
                Level::Maybe(bs) => return self.maybe(&bs, r),
                Level::Miss => {}
            }
        }
        if let Some(hit) = self.memo.get(word, r.unit) {
            return hit;
        }
        let named: SmallVec<[BufferId; 8]> = self
            .named
            .iter()
            .copied()
            .filter(|&b| self.visible(self.out.buffers[b.index()].owner, r.unit))
            .collect();
        // An abbreviation counts only against a record phrase or an
        // enclosing record block, never against every buffer of the file.
        let (hit, keep) = match self.level(word, true, &named, &[], r.unit) {
            Level::Hit(b) => (Some(b), true),
            Level::Ambiguous(bs) => (self.ambiguous(&bs, r), false),
            Level::Maybe(bs) => (self.maybe(&bs, r), false),
            Level::Private | Level::Miss => {
                if self.schema.is_none() {
                    self.no_schema(r);
                    (None, false)
                } else {
                    (None, true)
                }
            }
        };
        if keep {
            self.memo.insert_first(word.to_string(), r.unit, hit);
        }
        hit
    }

    fn ambiguous(&mut self, bs: &[BufferId], r: &RawRef) -> Option<BufferId> {
        for &b in bs {
            self.out.buffer_unknowns.push((
                b,
                Unknown {
                    reason: UnknownReason::AmbiguousField,
                    at: r.at,
                },
            ));
        }
        None
    }

    fn maybe(&mut self, bs: &[BufferId], r: &RawRef) -> Option<BufferId> {
        let reason = if self.schema.is_none() {
            UnknownReason::NoSchema
        } else {
            UnknownReason::UnknownTable
        };
        let unknown = Unknown { reason, at: r.at };
        for &b in bs {
            self.out.buffer_unknowns.push((b, unknown));
        }
        if r.write.is_some() {
            self.out.tx_unknowns.push((r.unit, r.block, unknown));
        }
        None
    }

    /// Search one level of candidates for a field.
    fn level(
        &self,
        word: &str,
        exact_only: bool,
        candidates: &[BufferId],
        privates: &[usize],
        unit: UnitId,
    ) -> Level {
        let mut exact: SmallVec<[BufferId; 2]> = SmallVec::new();
        let mut prefix: SmallVec<[BufferId; 2]> = SmallVec::new();
        let mut maybe: SmallVec<[BufferId; 2]> = SmallVec::new();
        for &b in candidates {
            let list = match self.has_field(b, word) {
                Has::Exact => &mut exact,
                Has::Prefix if exact_only => continue,
                Has::Prefix => &mut prefix,
                Has::Maybe => &mut maybe,
                Has::No => continue,
            };
            if !list.contains(&b) {
                list.push(b);
            }
        }
        // A CAN-FIND's own table, read by name.
        let private_hit = privates.iter().any(|&p| {
            let name = &self.c.privates[p].1;
            match self.table_has_field(&self.private_table(name, unit), None, word) {
                Has::Exact | Has::Maybe => true,
                Has::Prefix => !exact_only,
                Has::No => false,
            }
        });
        if private_hit {
            return Level::Private;
        }
        match exact.len() {
            1 => return Level::Hit(exact[0]),
            0 => {}
            _ => return Level::Ambiguous(exact),
        }
        if !maybe.is_empty() {
            return Level::Maybe(maybe);
        }
        match prefix.len() {
            0 => Level::Miss,
            1 => Level::Hit(prefix[0]),
            _ => Level::Ambiguous(prefix),
        }
    }

    fn private_table(&self, name: &str, unit: UnitId) -> Table {
        if let Some(t) = self.temp_table_in(name, unit) {
            return Table::TempTable {
                name: self.c.temp_tables[t].name.clone(),
                no_undo: true,
            };
        }
        let def = self.find(&self.defined, name, unit);
        let key = match def {
            Some(i) => fold(last_segment(&self.c.buffers[i].table)),
            None => name.to_string(),
        };
        if let Some(t) = self.temp_table_in(&key, unit) {
            return Table::TempTable {
                name: self.c.temp_tables[t].name.clone(),
                no_undo: true,
            };
        }
        self.db_table(&key, &key)
    }

    fn has_field(&self, b: BufferId, word: &str) -> Has {
        let buffer = &self.out.buffers[b.index()];
        self.table_has_field(&buffer.table, self.temp_table_of.get(&b).copied(), word)
    }

    fn table_has_field(&self, table: &Table, tt: Option<usize>, word: &str) -> Has {
        match table {
            Table::Database(name) => self.schema_has_field(name, word),
            Table::Unknown(_) => Has::Maybe,
            Table::TempTable { name, .. } => {
                let tt = tt.or_else(|| {
                    self.c
                        .temp_tables
                        .iter()
                        .position(|t| t.name.eq_ignore_ascii_case(name))
                });
                match tt {
                    Some(t) => self.temp_table_has_field(t, word, 0),
                    None => Has::Maybe,
                }
            }
        }
    }

    fn schema_has_field(&self, table: &str, word: &str) -> Has {
        if let Some(h) = self.fields.borrow().get(table).and_then(|m| m.get(word)) {
            return *h;
        }
        let has = match self.schema_table(&fold(table)) {
            None => Has::Maybe,
            Some(t) => match t.resolve_field_by_name(word) {
                FieldResolution::Unique(f) if f.name.as_ref() == word => Has::Exact,
                FieldResolution::Unique(_) => Has::Prefix,
                FieldResolution::Ambiguous | FieldResolution::NotFound => Has::No,
            },
        };
        self.fields
            .borrow_mut()
            .entry(table.to_string())
            .or_default()
            .insert(word.to_string(), has);
        has
    }

    fn temp_table_has_field(&self, t: usize, word: &str, depth: u8) -> Has {
        let tt = &self.c.temp_tables[t];
        if tt.fields.iter().any(|f| f == word) {
            return Has::Exact;
        }
        let Some(like) = &tt.like else {
            return Has::No;
        };
        if depth > 8 {
            return Has::No;
        }
        if let Some(other) = self.temp_table_in(like, tt.unit) {
            return self.temp_table_has_field(other, word, depth + 1);
        }
        self.schema_has_field(like, word)
    }

    // ----- results -----

    fn finish_refs(&mut self) {
        let c = self.c;
        for (i, r) in c.refs.iter().enumerate() {
            let Some(buffer) = self.raw[i] else {
                continue;
            };
            let mut block = r.block;
            if let Some(h) = r.header_of {
                let own = c
                    .block_phrases
                    .get(&(r.unit, h))
                    .is_some_and(|refs| refs.iter().any(|&j| self.raw[j as usize] == Some(buffer)));
                if own {
                    block = h;
                }
            }
            let table = &self.out.buffers[buffer.index()].table;
            let database = !table.is_temp_table();
            if r.form == Form::UnknownSkipped && database {
                self.out.tx_unknowns.push((
                    r.unit,
                    block,
                    Unknown {
                        reason: UnknownReason::SkippedStatement,
                        at: Span {
                            start: r.origin.min(r.at.start),
                            end: r.at.end,
                        },
                    },
                ));
            }
            if let Some(kind) = r.write {
                self.write(r.unit, block, kind, r.at, buffer);
            }
            self.out.refs.push(Ref {
                buffer,
                unit: r.unit,
                block,
                role: r.role,
                at: r.at,
                write: r.write,
                passed: r.passed,
            });
        }
    }

    /// A write to a buffer: a database update, or Unknown when the table is.
    fn write(&mut self, unit: UnitId, block: BlockId, kind: UpdateKind, at: Span, b: BufferId) {
        match &self.out.buffers[b.index()].table {
            Table::TempTable { .. } => {}
            Table::Unknown(_) if self.schema.is_some() => self.out.tx_unknowns.push((
                unit,
                block,
                Unknown {
                    reason: UnknownReason::UnknownTable,
                    at,
                },
            )),
            _ => self.out.updates.push((
                unit,
                block,
                Update {
                    kind,
                    at,
                    buffer: Some(b),
                },
            )),
        }
    }

    fn reads(&mut self) {
        let c = self.c;
        for read in &c.reads {
            let lock_for = |table: &Table| match (table, read.lock) {
                (Table::TempTable { .. }, _) => Lock::TempTable,
                (_, Some(m)) => Lock::Explicit(m),
                (_, None) => Lock::Default,
            };
            let targets: SmallVec<[(BufferId, String); 2]> = match read.target {
                ReadTarget::Ref(i) => match self.raw[i as usize] {
                    Some(b) => smallvec::smallvec![(b, read.name.clone())],
                    None => SmallVec::new(),
                },
                // A CAN-FIND reads with a buffer of its own: only its table
                // counts.
                ReadTarget::Private => {
                    let table = self.private_table(&fold(last_segment(&read.name)), read.unit);
                    self.out.reads.push(RecordRead {
                        unit: read.unit,
                        block: read.block,
                        stmt: read.stmt,
                        kind: read.kind,
                        name: read.name.clone(),
                        buffer: None,
                        at: read.at,
                        lock: lock_for(&table),
                        table,
                        written: Written::No,
                    });
                    continue;
                }
                ReadTarget::Query(q) => {
                    let (unit, name) = &c.query_uses[q as usize];
                    let names: SmallVec<[String; 2]> = self
                        .query(name, *unit)
                        .map(|d| d.buffers.clone())
                        .unwrap_or_default();
                    names
                        .iter()
                        .map(|n| (self.buffer_named(n, read.unit, true), n.clone()))
                        .collect()
                }
            };
            for (b, name) in targets {
                let table = self.out.buffers[b.index()].table.clone();
                let mut lock = lock_for(&table);
                if read.kind == ReadKind::Get && lock == Lock::Default {
                    lock = self.opened_with(read, &name);
                }
                if let ReadTarget::Query(_) = read.target {
                    self.out.refs.push(Ref {
                        buffer: b,
                        unit: read.unit,
                        block: read.block,
                        role: Role::Free,
                        at: read.at,
                        write: None,
                        passed: false,
                    });
                }
                if read.lock == Some(LockMode::ExclusiveLock) {
                    self.write(read.unit, read.block, UpdateKind::ExclusiveRead, read.at, b);
                }
                self.out.reads.push(RecordRead {
                    unit: read.unit,
                    block: read.block,
                    stmt: read.stmt,
                    kind: read.kind,
                    name,
                    buffer: Some(b),
                    table,
                    at: read.at,
                    lock,
                    written: Written::No,
                });
            }
        }
    }

    /// The lock of the last `OPEN QUERY` phrase of the same query and buffer
    /// before a `GET` without a lock phrase.
    fn opened_with(&self, get: &crate::collect::RawRead, buffer: &str) -> Lock {
        let buffer = fold(last_segment(buffer));
        let open = self
            .c
            .reads
            .iter()
            .filter(|r| {
                r.kind == ReadKind::OpenQuery
                    && r.query == get.query
                    && r.at.start < get.at.start
                    && fold(last_segment(&r.name)) == buffer
                    && self.visible(r.unit, get.unit)
            })
            .max_by_key(|r| r.at.start);
        match open {
            Some(r) => r.lock.map_or(Lock::Default, Lock::Explicit),
            None => Lock::Unknown(UnknownReason::QueryLock),
        }
    }

    fn query(&self, name: &str, unit: UnitId) -> Option<&'a crate::collect::Query> {
        let c = self.c;
        self.chain(unit).find_map(|u| {
            c.queries
                .iter()
                .rev()
                .find(|q| q.unit == u && q.name.eq_ignore_ascii_case(name))
        })
    }
}
