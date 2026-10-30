//! The value IR, and the two front ends that build it: one from the AST, one
//! from the tokens of a span.
//!
//! A [`Value`] is a union of terms: a slot read, an origin, fixed labels, a
//! sub-value with some sink kinds cleared, or Unknown. The solver evaluates
//! it against the facts before a node.

use std::ops::Range;

use oxabl_ast::{Expression, ExpressionKind as E, Span};
use oxabl_flow::provenance::{self, Labels, Provenance};
use oxabl_lexer::Kind;
use rustc_hash::{FxHashMap, FxHashSet};

use super::super::literal::{StatementText, is_constant};
use super::{ALL, Options, SinkKind};

/// Every kind but `size`: what a number can no longer carry.
pub(super) const NUMERIC_SAFE: u32 = ALL & !SinkKind::Size.bit();

/// The origin of a value the analysis cannot judge. It is never a real
/// source: a sink it reaches is reported as stage 1 reports it.
pub(super) const UNKNOWN: u32 = u32::MAX;

/// The slot of `RETURN-VALUE` in every unit that reads or sets it, and in
/// every procedure, in a file that reads it: what the last `RUN` left, or
/// what a procedure gives back with `RETURN` or `RETURN ERROR`. A file that
/// does not read it has no such slot, so its summaries do not follow it.
pub(super) const RETURN_VALUE: &str = "$return-value";

/// The slot of what a procedure may give back in `RETURN-VALUE` through the
/// `RETURN` action of a block's `ON` phrase, where the CFG has no edge for
/// it: the condition may come at any statement of the block. Only added to.
pub(super) const ON_RETURN: &str = "$on-return";

/// The labels of a value the analysis cannot judge.
const UNKNOWN_LABELS: [(u32, u32); 1] = [(UNKNOWN, ALL)];

/// The origin of a value the analysis did not finish because a unit ran past
/// its work budget: what that unit leaves. Like [`UNKNOWN`] it is never a real
/// source; a decision it reaches is reported as not judged.
pub(super) const BUDGET: u32 = u32::MAX - 1;

/// The labels of a value the analysis did not finish: every kind, the
/// identity of [`SinkKind::Identity`] too, since it may hold one.
pub(super) const BUDGET_LABELS: [(u32, u32); 1] = [(BUDGET, ALL | SinkKind::Identity.bit())];

/// The origin of a value the analysis did not finish because the rounds ended
/// before the summaries of the file stopped changing: every file-level read,
/// parameter and call. Read as [`BUDGET`] is, with its own reason.
pub(super) const UNSETTLED: u32 = u32::MAX - 2;

/// Not a kind: on the origin of a parameter, the label passes on, through a
/// call, the identity that the argument holds. A logical made from the
/// parameter keeps it, so `RETURN p = "admin"` gives back the identity of the
/// argument; the parameter is no identity of its own. A validator that
/// protects the identity clears it too.
pub(super) const PASSED: u32 = 1 << 11;

/// The labels of [`UNSETTLED`]: every kind, the identity too.
const UNSETTLED_LABELS: [(u32, u32); 1] = [(UNSETTLED, ALL | SinkKind::Identity.bit())];

/// An untrusted source site.
#[derive(Debug, Clone)]
pub(super) struct Origin {
    pub span: Span,
    /// A noun phrase: "input parameter `pc`".
    pub what: String,
    /// Stands for what callers pass to a parameter: not a source of its own.
    pub placeholder: bool,
}

/// The origins of a file, or of one operand on top of them.
#[derive(Debug, Clone, Default)]
pub(super) struct Origins {
    /// The id of the first origin of `list`.
    pub offset: u32,
    pub list: Vec<Origin>,
    by_key: FxHashMap<String, u32>,
}

impl Origins {
    pub fn on_top_of(base: &Origins) -> Self {
        Origins {
            offset: base.offset + base.list.len() as u32,
            ..Origins::default()
        }
    }

    pub fn add(&mut self, what: String, span: Span) -> u32 {
        self.push(what, span, false)
    }

    /// An origin that stands for the argument of a parameter.
    pub fn placeholder(&mut self, what: String, span: Span) -> u32 {
        self.push(what, span, true)
    }

    fn push(&mut self, what: String, span: Span, placeholder: bool) -> u32 {
        self.list.push(Origin {
            span,
            what,
            placeholder,
        });
        self.offset + self.list.len() as u32 - 1
    }

    /// One origin per `key`: a database field is one source however often it
    /// is read.
    pub fn keyed(&mut self, key: &str, what: impl FnOnce() -> String, span: Span) -> u32 {
        if let Some(&id) = self.by_key.get(key) {
            return id;
        }
        let id = self.add(what(), span);
        self.by_key.insert(key.to_string(), id);
        id
    }

    pub fn get(&self, id: u32) -> Option<&Origin> {
        self.list.get(id.checked_sub(self.offset)? as usize)
    }
}

#[derive(Debug, Clone, Default)]
pub(super) struct Value(pub Vec<Term>);

#[derive(Debug, Clone)]
pub(super) enum Term {
    Slot(u32),
    Origin(u32),
    /// A file-level variable or field, at its summary.
    File {
        name: String,
        key: String,
    },
    /// The sub-value with these kinds removed.
    Clear(u32, Value),
    /// The sub-value with these kinds added to each of its labels.
    Tag(u32, Value),
    /// A logical made from the sub-value: only the identity of its labels.
    /// Text in it that the analysis cannot read is an Unknown identity, not an
    /// Unknown value, since no injection kind is left to reach a sink.
    Logical(Value),
    /// What a routine of the file returns (or leaves in an output parameter)
    /// for these arguments, by its summary.
    Call {
        callees: Vec<u32>,
        out: Out,
        args: Vec<Value>,
    },
    /// What the callers of a unit pass to its parameter at this position.
    Param {
        unit: u32,
        pos: u32,
    },
    /// Every field of a table, at its summary.
    Table(String),
    /// Text the analysis cannot read, such as an unexpanded reference.
    Unknown,
}

/// The result of a call that a [`Term::Call`] reads.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Out {
    Return,
    /// The parameter at this position.
    Param(u32),
}

/// What [`Value::eval`] reads besides the fact.
#[derive(Debug, Clone, Copy)]
pub(super) struct Env<'a> {
    pub files: &'a Summaries,
    /// Whether parameters take what callers pass. A routine's summary is
    /// solved without, so one caller's input does not reach another caller.
    pub seeds: bool,
    /// Code outside the file may run before the unit: see `UnitIr::outside`.
    pub outside: bool,
}

impl<'a> Env<'a> {
    pub fn new(files: &'a Summaries) -> Self {
        Env {
            files,
            seeds: true,
            outside: false,
        }
    }

    /// The environment of a unit that code outside the file may run before.
    pub fn outside(self, outside: bool) -> Self {
        Env { outside, ..self }
    }

    /// What every read of a file-level slot or a parameter holds besides its
    /// summary: [`UNSETTLED`] when the summaries did not converge, Unknown
    /// when code outside the file may run first.
    pub fn unsure(&self, out: &mut Labels) {
        if self.files.unconverged {
            provenance::merge(out, &UNSETTLED_LABELS);
        }
        if self.outside {
            provenance::merge(out, &UNKNOWN_LABELS);
        }
    }
}

impl Value {
    pub fn origin(id: u32) -> Self {
        Value(vec![Term::Origin(id)])
    }

    pub fn union(values: impl IntoIterator<Item = Value>) -> Self {
        Value(values.into_iter().flat_map(|v| v.0).collect())
    }

    pub fn cleared(self, mask: u32) -> Self {
        if self.0.is_empty() {
            self
        } else {
            Value(vec![Term::Clear(mask, self)])
        }
    }

    /// A logical made from the value: a comparison, a logical operator or
    /// function, `CAN-FIND`. It decides on the value, so it still holds the
    /// value's identity, and no other kind.
    pub fn logical(self) -> Self {
        if self.0.is_empty() {
            self
        } else {
            Value(vec![Term::Logical(self)])
        }
    }

    /// The value with the identity kind added to its labels.
    pub fn identity(self) -> Self {
        self.tagged(SinkKind::Identity.bit())
    }

    /// The value with the kinds of `mask` added to its labels.
    pub fn tagged(self, mask: u32) -> Self {
        if self.0.is_empty() {
            self
        } else {
            Value(vec![Term::Tag(mask, self)])
        }
    }

    /// The labels of the value against `fact` and the file-level summaries,
    /// and whether it holds Unknown.
    pub fn eval(&self, fact: &Provenance, env: Env) -> (Labels, bool) {
        let mut out = Labels::new();
        let unknown = self.eval_into(fact, env, &mut out);
        (out, unknown)
    }

    fn eval_into(&self, fact: &Provenance, env: Env, out: &mut Labels) -> bool {
        let files = env.files;
        let mut unknown = false;
        for t in &self.0 {
            match t {
                Term::Slot(s) => {
                    provenance::merge(out, fact.get(*s as usize));
                }
                Term::Origin(o) => {
                    provenance::merge(out, &[(*o, ALL)]);
                }
                Term::File { key, .. } => {
                    if let Some(l) = files.labels.get(key) {
                        provenance::merge(out, l);
                    }
                    env.unsure(out);
                }
                Term::Table(table) => {
                    for (key, l) in &files.labels {
                        if key
                            .strip_prefix(table.as_str())
                            .is_some_and(|f| f.starts_with('.'))
                        {
                            provenance::merge(out, l);
                        }
                    }
                    env.unsure(out);
                }
                Term::Clear(mask, v) => {
                    let mut inner = Labels::new();
                    unknown |= v.eval_into(fact, env, &mut inner);
                    provenance::clear(&mut inner, *mask);
                    provenance::merge(out, &inner);
                }
                Term::Tag(mask, v) => {
                    let mut inner = Labels::new();
                    unknown |= v.eval_into(fact, env, &mut inner);
                    inner.iter_mut().for_each(|l| l.1 |= mask);
                    provenance::merge(out, &inner);
                }
                Term::Logical(v) => {
                    let mut inner = Labels::new();
                    if v.eval_into(fact, env, &mut inner) {
                        provenance::merge(out, &[(UNKNOWN, SinkKind::Identity.bit())]);
                    }
                    provenance::clear(&mut inner, ALL);
                    provenance::merge(out, &inner);
                }
                Term::Call {
                    callees,
                    out: o,
                    args,
                } => {
                    unknown |= call_into(callees, *o, args, fact, env, out);
                }
                Term::Param { unit, pos } => {
                    if !env.seeds {
                        continue;
                    }
                    if let Some(l) = files.seeds.get(&(*unit, *pos)) {
                        provenance::merge(out, l);
                    }
                    env.unsure(out);
                }
                Term::Unknown => {
                    unknown = true;
                    provenance::merge(out, &UNKNOWN_LABELS);
                }
            }
        }
        unknown
    }

    /// Whether the value names `origin` itself, not through a slot.
    pub fn holds_origin(&self, origin: u32) -> bool {
        self.0.iter().any(|t| match t {
            Term::Origin(o) => *o == origin,
            Term::Clear(_, v) | Term::Tag(_, v) | Term::Logical(v) => v.holds_origin(origin),
            _ => false,
        })
    }

    /// The file-level term that carries `origin`: its name and key.
    pub fn file_with(&self, files: &Summaries, origin: u32) -> Option<(&str, &str)> {
        self.0.iter().find_map(|t| match t {
            Term::File { name, key }
                if files
                    .labels
                    .get(key)
                    .is_some_and(|l| l.iter().any(|(o, _)| *o == origin)) =>
            {
                Some((name.as_str(), key.as_str()))
            }
            Term::Clear(_, v) | Term::Tag(_, v) | Term::Logical(v) => v.file_with(files, origin),
            _ => None,
        })
    }

    /// Add to each [`Term::Table`] the unit's slots of that table, which
    /// hold what the unit wrote to them.
    pub fn expand_tables(&mut self, slots: &Slots) {
        let mut add = Vec::new();
        for t in &mut self.0 {
            match t {
                Term::Table(table) => add.extend(
                    slots
                        .info
                        .iter()
                        .enumerate()
                        .filter(|(_, s)| {
                            s.key
                                .strip_prefix(table.as_str())
                                .is_some_and(|f| f.starts_with('.'))
                        })
                        .map(|(i, _)| Term::Slot(i as u32)),
                ),
                Term::Clear(_, v) | Term::Tag(_, v) | Term::Logical(v) => v.expand_tables(slots),
                Term::Call { args, .. } => args.iter_mut().for_each(|a| a.expand_tables(slots)),
                _ => {}
            }
        }
        self.0.extend(add);
    }

    /// The slots the value reads, outermost first.
    pub fn slots(&self, out: &mut Vec<u32>) {
        for t in &self.0 {
            match t {
                Term::Slot(s) => out.push(*s),
                Term::Clear(_, v) | Term::Tag(_, v) | Term::Logical(v) => v.slots(out),
                Term::Call { args, .. } => args.iter().for_each(|a| a.slots(out)),
                _ => {}
            }
        }
    }
}

/// The labels of a call: each callee's summary with the arguments put in
/// for its parameters. A callee past its work budget returns its arguments
/// and [`BUDGET`], and every call holds [`UNSETTLED`] when the summaries did
/// not converge.
fn call_into(
    callees: &[u32],
    o: Out,
    args: &[Value],
    fact: &Provenance,
    env: Env,
    out: &mut Labels,
) -> bool {
    let mut unknown = false;
    let mut evaluated: Vec<Option<Labels>> = vec![None; args.len()];
    let mut arg = |i: usize, unknown: &mut bool| -> Labels {
        evaluated[i]
            .get_or_insert_with(|| {
                let (l, u) = args[i].eval(fact, env);
                *unknown |= u;
                l
            })
            .clone()
    };
    if env.files.unconverged {
        provenance::merge(out, &UNSETTLED_LABELS);
    }
    for &c in callees {
        let Some(s) = env.files.units.get(&c) else {
            continue;
        };
        if s.unknown {
            for i in 0..args.len() {
                provenance::merge(out, &arg(i, &mut unknown));
            }
            provenance::merge(out, &BUDGET_LABELS);
            continue;
        }
        let labels = match o {
            Out::Return => &s.ret,
            Out::Param(p) => match s.outs.get(p as usize) {
                Some(l) => l,
                None => continue,
            },
        };
        for &(origin, mask) in labels {
            match s.params.iter().position(|p| *p == Some(origin)) {
                Some(i) if i < args.len() => {
                    let mut l = arg(i, &mut unknown);
                    let keep = if mask & PASSED != 0 {
                        mask | SinkKind::Identity.bit()
                    } else {
                        mask
                    };
                    l.iter_mut().for_each(|x| x.1 &= keep);
                    l.retain(|x| x.1 != 0);
                    provenance::merge(out, &l);
                }
                Some(_) => {}
                None => {
                    provenance::merge(out, &[(origin, mask)]);
                }
            }
        }
    }
    unknown
}

/// A variable, a field or a whole table that a value can sit in.
#[derive(Debug, Clone)]
pub(super) struct SlotInfo {
    /// As first written: `cWhere`, `ttFilter.cWhere`.
    pub name: String,
    pub key: String,
    /// Not declared in the unit: its value at the entry comes from the file.
    pub global: bool,
    /// A variable no unit of the file declares, or a field of a table the file
    /// does not define, in a file whose names may come from outside it, or a
    /// variable or temp-table field that the file binds as `SHARED` or `NEW
    /// GLOBAL SHARED`: its value at the entry is Unknown.
    pub outside: bool,
}

#[derive(Debug, Clone, Default)]
pub(super) struct Slots {
    pub info: Vec<SlotInfo>,
    index: FxHashMap<String, u32>,
}

impl Slots {
    pub fn get(&self, key: &str) -> Option<u32> {
        self.index.get(key).copied()
    }

    pub fn len(&self) -> usize {
        self.info.len()
    }
}

/// Facts about the file that every unit reads.
#[derive(Debug, Default)]
pub(super) struct Tables {
    /// Lower-cased temp-table names.
    pub temp_tables: FxHashSet<String>,
    /// Lower-cased buffer name to the table it is for.
    pub buffers: FxHashMap<String, String>,
    /// Lower-cased variable, parameter and property names to their class
    /// type, lower-cased; empty for a primitive type.
    pub types: FxHashMap<String, String>,
    /// Lower-cased dataset names to the tables of their buffers.
    pub datasets: FxHashMap<String, Vec<String>>,
    /// Every lower-cased name the file defines: variables and parameters of
    /// its main block or class, properties, tables and their fields, buffers,
    /// datasets, streams, ...
    pub names: FxHashSet<String>,
    /// Lower-cased qualifiers of `q.f` reads that the schema knows as tables.
    pub database: FxHashSet<String>,
    /// Lower-cased receivers of `name:member` that the file does not define
    /// and the schema (with the metaschema) knows as tables: buffers, not
    /// classes.
    pub receivers: FxHashSet<String>,
    /// Lower-cased handle variables to the names of the tables, datasets or
    /// buffers whose `:HANDLE` the file assigns to them.
    pub handles: FxHashMap<String, Vec<String>>,
    /// Names the file reads and does not define may come from outside it: the
    /// file is an include checked on its own, an include did not arrive, or a
    /// class inherits members.
    pub open: bool,
    /// Lower-cased names that code the file does not hold may write while
    /// the file runs: variables and temp-tables the file defines as shared,
    /// in any form, and class members that code outside the class may set
    /// (`PUBLIC` or `PROTECTED` variables, properties with a `SET` that is
    /// not `PRIVATE`). After a call of such code they are Unknown.
    pub shared: FxHashSet<String>,
    /// Those defined `SHARED` or `NEW GLOBAL SHARED`: code outside the file may
    /// set them before the file runs.
    pub shared_in: FxHashSet<String>,
    /// The file has a routine or trigger, which code it does not hold may run
    /// back while the file runs (a callback through `THIS-PROCEDURE` or
    /// `SOURCE-PROCEDURE`, a trigger in `WAIT-FOR`, a method of an object
    /// that was passed on).
    pub callbacks: bool,
    /// The source names `RETURN-VALUE`: only then does the analysis follow
    /// it.
    pub return_value: bool,
    /// Lower-cased `SESSION` and `THIS-PROCEDURE` when the file assigns their
    /// `PRIVATE-DATA`; see [`SHARED_HANDLES`].
    pub private_data: FxHashSet<String>,
}

impl Tables {
    /// Whether the `PRIVATE-DATA` of the system handle `name` is set by code
    /// the file does not hold, as far as the file shows: `name` is on
    /// [`SHARED_HANDLES`] and the file does not assign it.
    fn shared_private_data(&self, name: &str) -> bool {
        let name = name.to_ascii_lowercase();
        SHARED_HANDLES.contains(&name.as_str())
            && !self.types.contains_key(&name)
            && !self.private_data.contains(&name)
    }

    /// Whether code the file does not hold may write what the file sees: it
    /// has [`Self::shared`] names or [`Self::callbacks`]. Only such a file
    /// looks for the calls of that code.
    pub fn exposed(&self) -> bool {
        self.callbacks || !self.shared.is_empty()
    }

    /// Whether the file looks for the calls of code it does not hold: it is
    /// [`Self::exposed`], or it reads `RETURN-VALUE`, which such code may set.
    pub fn watches(&self) -> bool {
        self.exposed() || self.return_value
    }

    /// Whether `name` may hold a JSON object or array: its declared class is
    /// one, or the file does not declare it.
    pub fn may_be_json(&self, name: &str) -> bool {
        match self.types.get(&name.to_ascii_lowercase()) {
            None => true,
            Some(t) => is_json_class(t),
        }
    }

    /// The table a buffer or table name stands for, lower-cased.
    pub fn table_of(&self, name: &str) -> String {
        let lower = name.to_ascii_lowercase();
        self.buffers.get(&lower).cloned().unwrap_or(lower)
    }

    /// The tables `name` stands for when it names a dataset, a temp-table or
    /// a buffer of one, lower-cased.
    pub fn tables_of(&self, name: &str) -> Vec<String> {
        let lower = name.to_ascii_lowercase();
        if let Some(tables) = self.datasets.get(&lower) {
            return tables.iter().map(|t| self.table_of(t)).collect();
        }
        let table = self.table_of(&lower);
        if self.temp_tables.contains(&table) {
            vec![table]
        } else {
            Vec::new()
        }
    }

    /// Whether the variable `key` may come from outside the file: the file is
    /// open, defines no such name, and the name is one word and no system
    /// handle, reserved keyword or built-in function that is called without
    /// parentheses.
    pub fn outside(&self, key: &str) -> bool {
        if !self.open
            || self.names.contains(key)
            || oxabl_semantic::SYSTEM_HANDLES.contains(&key)
            || BARE_FUNCTIONS.contains(&key)
            || oxabl_lexer::is_reserved_keyword(key)
        {
            return false;
        }
        matches!(
            oxabl_lexer::tokenize(key).as_slice(),
            [_, end] if end.kind == Kind::Eof
        )
    }

    /// Whether the table or buffer `qualifier` of a `q.f` read may come from
    /// outside the file: the file binds it as a shared temp-table, or it is
    /// open, defines no such name, and the schema knows no such table.
    pub fn outside_table(&self, qualifier: &str) -> bool {
        let q = qualifier.to_ascii_lowercase();
        self.shared_in.contains(&self.table_of(&q))
            || self.open && !self.names.contains(&q) && !self.database.contains(&q)
    }
}

/// Built-in functions that a program can call without parentheses. Any other
/// built-in name, alone, is a variable (`extent`, `length`).
const BARE_FUNCTIONS: [&str; 49] = [
    "audit-enabled",
    "current-language",
    "dataservers",
    "dbname",
    "etime",
    "frame-db",
    "frame-field",
    "frame-file",
    "frame-index",
    "frame-name",
    "frame-value",
    "gateways",
    "generate-pbe-salt",
    "generate-random-key",
    "generate-uuid",
    "get-codepages",
    "guid",
    "is-attr-space",
    "lastkey",
    "line-counter",
    "message-lines",
    "mtime",
    "now",
    "num-aliases",
    "num-dbs",
    "opsys",
    "os-drives",
    "os-error",
    "page-number",
    "page-size",
    "proc-handle",
    "proc-status",
    "process-architecture",
    "promsgs",
    "propath",
    "proversion",
    "retry",
    "return-value",
    "screen-lines",
    "tenant-id",
    "tenant-name",
    "term",
    "terminal",
    "time",
    "timezone",
    "today",
    "transaction",
    "user",
    "userid",
];

/// File-level summaries: the labels every unit writes to a variable or field
/// that other units see, and one place each was written; what each routine
/// returns; and what callers pass to each parameter.
#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct Summaries {
    pub labels: FxHashMap<String, Labels>,
    pub sites: FxHashMap<String, Span>,
    /// By unit index. A unit not here returns nothing yet.
    pub units: FxHashMap<u32, UnitSummary>,
    /// By unit index and parameter position.
    pub seeds: FxHashMap<(u32, u32), Labels>,
    /// The rounds ended before the summaries stopped changing: every read of
    /// a file-level slot, a routine result or a parameter also holds Unknown.
    pub unconverged: bool,
}

/// What a routine returns and leaves in its output parameters, in terms of
/// real origins and of the origins of its own parameters.
#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct UnitSummary {
    pub ret: Labels,
    /// By parameter position; empty for an input parameter.
    pub outs: Vec<Labels>,
    /// The origin of each parameter position, which a call replaces with its
    /// argument.
    pub params: Vec<Option<u32>>,
    /// The unit ran out of its work budget.
    pub unknown: bool,
}

/// The units of a file that a call can name, by lower-cased name.
#[derive(Debug, Default)]
pub(super) struct Routines {
    pub procedures: FxHashMap<String, Vec<u32>>,
    pub functions: FxHashMap<String, Vec<u32>>,
    pub methods: FxHashMap<String, Vec<u32>>,
}

impl Routines {
    /// Every routine unit.
    pub fn units(&self) -> impl Iterator<Item = u32> + '_ {
        self.procedures
            .values()
            .chain(self.functions.values())
            .chain(self.methods.values())
            .flatten()
            .copied()
    }

    /// Whether an expression can call a routine of the file.
    pub fn any_callable(&self) -> bool {
        !self.functions.is_empty() || !self.methods.is_empty()
    }

    /// A function, or a method called without a receiver.
    pub fn callable(&self, name: &str) -> Option<&[u32]> {
        let key = name.to_ascii_lowercase();
        self.functions
            .get(&key)
            .or_else(|| self.methods.get(&key))
            .map(Vec::as_slice)
    }

    pub fn method(&self, name: &str) -> Option<&[u32]> {
        self.methods
            .get(&name.to_ascii_lowercase())
            .map(Vec::as_slice)
    }

    pub fn procedure(&self, name: &str) -> Option<&[u32]> {
        self.procedures
            .get(&name.to_ascii_lowercase())
            .map(Vec::as_slice)
    }
}

/// A call of a routine of the file: what each position receives.
#[derive(Debug, Clone)]
pub(super) struct CallIr {
    pub callees: Vec<u32>,
    pub args: Vec<Value>,
    /// The call.
    pub span: Span,
    /// A `RUN` of a procedure, which sets `RETURN-VALUE` by its summary; a
    /// function or method may set it by a `RUN` of its own.
    pub run: bool,
}

/// Builds values for one unit.
pub(super) struct Compiler<'c> {
    pub options: &'c Options,
    pub tables: &'c Tables,
    pub slots: &'c mut Slots,
    pub origins: &'c mut Origins,
    pub declared: &'c FxHashSet<String>,
    /// Reading an operand after the solve: a slot the unit never used is not
    /// added, and a file-level one reads its summary.
    pub frozen: bool,
    pub routines: &'c Routines,
    /// Calls of routines of the file met while building values.
    pub calls: Vec<CallIr>,
    /// Calls met while building values that may run code the file does not
    /// hold, which may write its shared names and run its routines. See
    /// [`Self::runs_elsewhere`].
    pub foreign: Vec<Span>,
    /// Reading a decision: a logical function passes on its arguments, so the
    /// value is what the decision reads.
    pub decision: bool,
}

/// Whether `word` is an ABL keyword, as a built-in attribute or method is.
fn is_keyword(word: &str) -> bool {
    oxabl_lexer::tokenize(word)
        .first()
        .is_some_and(|t| t.kind != Kind::Identifier)
}

/// Whether the lower-cased class name `class` is a JSON class.
fn is_json_class(class: &str) -> bool {
    class
        .rsplit('.')
        .next()
        .is_some_and(|s| s.starts_with("json"))
}

/// How a function name changes the value of its arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Builtin {
    Source,
    Quoter,
    Numeric,
    Logical,
    Encode,
    /// A built-in function whose result is made of its arguments, or holds
    /// no input: see [`ARGS`].
    Args,
    /// `RETURN-VALUE`: what the last `RUN` left.
    ReturnValue,
    /// A built-in function whose result stage 2 does not model: Unknown.
    Unknown,
    /// Not a built-in function: an unknown call.
    Other,
}

/// Built-in functions whose result is made of their arguments (`ENTRY`,
/// `SUBSTRING`, `STRING`), or holds no input (`TODAY`, `GUID`, `DBNAME`,
/// `PROGRAM-NAME`): the union of the arguments is their value. With the
/// lists of [`builtin`] and [`UNKNOWN_FUNCTIONS`], every built-in function
/// has a decision; one on none of them is Unknown.
const ARGS: [&str; 192] = [
    "accum",
    "add-interval",
    "alias",
    "audit-enabled",
    "base64-decode",
    "base64-encode",
    "big-endian",
    "box",
    "buffer-group-id",
    "buffer-group-name",
    "buffer-partition-id",
    "buffer-tenant-id",
    "buffer-tenant-name",
    // The parser makes `CAN-FIND` and `IF ... THEN ... ELSE` expression
    // kinds; the token front end reads `CAN-FIND(...)` as its arguments.
    "can-find",
    "caps",
    "cast",
    "chr",
    "codepage-convert",
    "compare",
    "connected",
    "count-of",
    "current-changed",
    "current-language",
    "current-result-row",
    "current-value",
    "data-source-modified",
    "dataservers",
    "db-remote-host",
    "dbcodepage",
    "dbcollation",
    "dbname",
    "dbparam",
    "dbrestrictions",
    "dbtaskid",
    "dbtype",
    "dbversion",
    "decrypt",
    "dynamic-cast",
    "dynamic-current-value",
    "dynamic-enum",
    "dynamic-new",
    "dynamic-next-value",
    "encode",
    "encrypt",
    "entered",
    "entry",
    "error",
    "extent",
    "fill",
    "first-of",
    "fix-codepage",
    "frame-col",
    "frame-db",
    "frame-down",
    "frame-field",
    "frame-file",
    "frame-index",
    "frame-line",
    "frame-name",
    "frame-row",
    "gateways",
    "generate-pbe-key",
    "generate-pbe-salt",
    "generate-random-key",
    "generate-uuid",
    "get-bits",
    "get-byte-order",
    "get-bytes",
    "get-class",
    "get-codepages",
    "get-collations",
    "get-db-client",
    "get-double",
    "get-effective-tenant-id",
    "get-effective-tenant-name",
    "get-float",
    "get-int64",
    "get-long",
    "get-pointer-value",
    "get-short",
    "get-string",
    "get-unsigned-long",
    "get-unsigned-short",
    "go-pending",
    "guid",
    "handle",
    "hash-code",
    "hex-decode",
    "hex-encode",
    "host-byte-order",
    "if...then...else",
    "is-attr-space",
    "is-codepage-fixed",
    "is-column-codepage",
    "is-db-multi-tenant",
    "is-lead-byte",
    "iso-date",
    "kblabel",
    "keycode",
    "keyfunction",
    "keylabel",
    "keyword",
    "keyword-all",
    "last-of",
    "lastkey",
    "lc",
    "ldbname",
    "left-trim",
    "library",
    "line-counter",
    "list-events",
    "list-query-attrs",
    "list-set-attrs",
    "list-widgets",
    "little-endian",
    "lower",
    "md5-digest",
    "member",
    "message-digest",
    "message-lines",
    "new",
    "next-value",
    "normalize",
    "now",
    "num-aliases",
    "num-dbs",
    "num-results",
    "opsys",
    "os-drives",
    "os-error",
    "overlay",
    "page-number",
    "page-size",
    "pdbname",
    "proc-handle",
    "proc-status",
    "process-architecture",
    "program-name",
    "progress",
    "promsgs",
    "propath",
    "proversion",
    "put-bits",
    "put-bytes",
    "put-double",
    "put-float",
    "put-int64",
    "put-long",
    "put-short",
    "put-string",
    "put-unsigned-long",
    "put-unsigned-short",
    "query-off-end",
    "raw",
    "recid",
    "record-length",
    "rejected",
    "replace",
    "retry",
    "rgb-value",
    "right-trim",
    "row-state",
    "rowid",
    "screen-lines",
    "sdbname",
    "search",
    "set-byte-order",
    "set-db-client",
    "set-effective-tenant",
    "set-pointer-value",
    "set-size",
    "setuserid",
    "sha1-digest",
    "string",
    "substitute",
    "substring",
    "tenant-id",
    "tenant-name-to-id",
    "terminal",
    "timezone",
    "to-rowid",
    "today",
    "transaction",
    "trim",
    "type-of",
    "unbox",
    "upper",
    "user",
    "userid",
    "valid-event",
    "value",
    "widget-handle",
];

/// Built-in functions whose result comes from outside the program and its
/// arguments: what the user typed on the screen (`FRAME-VALUE`, `INPUT`), a
/// directory listing (`OS-DIR`), the name of a TLS peer
/// (`SSL-SERVER-NAME`). Their value is Unknown.
const UNKNOWN_FUNCTIONS: [&str; 8] = [
    // Calls of code elsewhere, which may return anything. `DYNAMIC-FUNCTION`
    // of a function of the file is that function (see `Compiler::call`).
    "dynamic-function",
    "dynamic-invoke",
    "dynamic-property",
    "frame-value",
    "input",
    "os-dir",
    "ssl-server-name",
    "super",
];

/// Whether the lower-cased `name` is an entry of `list`, or a registered
/// abbreviation of one.
fn listed(list: &[&str], name: &str) -> bool {
    list.contains(&name)
        || oxabl_lexer::is_builtin_function(name) && list.iter().any(|f| f.starts_with(name))
}

/// Whether `name` is `full` or an abbreviation of it at least `min` long.
fn abbrev(name: &str, full: &str, min: usize) -> bool {
    name.len() >= min && name.len() <= full.len() && full[..name.len()].eq_ignore_ascii_case(name)
}

fn builtin(name: &str) -> Builtin {
    decision(name).unwrap_or_else(|| {
        if oxabl_lexer::is_builtin_function(&name.to_ascii_lowercase()) {
            Builtin::Unknown
        } else {
            Builtin::Other
        }
    })
}

/// The decision a list makes for the function `name`; `None` for a name on
/// no list, which [`builtin`] reads as Unknown when it is a built-in
/// function.
fn decision(name: &str) -> Option<Builtin> {
    const SOURCES: [&str; 7] = [
        "GET-VALUE",
        "GET-FIELD",
        "GET-CGI",
        "GET-USER-FIELD",
        "GET-COOKIE",
        "GET-LONG-VALUE",
        "OS-GETENV",
    ];
    const NUMERIC: [(&str, usize); 31] = [
        ("INTEGER", 3),
        ("INT64", 5),
        ("DECIMAL", 3),
        ("DATE", 4),
        ("DATETIME", 8),
        ("DATETIME-TZ", 11),
        ("LENGTH", 6),
        ("NUM-ENTRIES", 11),
        ("LOOKUP", 6),
        ("INDEX", 5),
        ("R-INDEX", 7),
        ("ASC", 3),
        ("ABSOLUTE", 3),
        ("ROUND", 5),
        ("TRUNCATE", 5),
        ("MAXIMUM", 3),
        ("MINIMUM", 3),
        ("EXP", 3),
        ("LOG", 3),
        ("SQRT", 4),
        ("RANDOM", 6),
        ("DAY", 3),
        ("MONTH", 5),
        ("YEAR", 4),
        ("WEEKDAY", 7),
        ("TIME", 4),
        ("ETIME", 5),
        ("MTIME", 5),
        ("GET-SIZE", 8),
        ("INTERVAL", 8),
        ("SEEK", 4),
    ];
    const LOGICAL: [(&str, usize); 9] = [
        ("LOGICAL", 4),
        ("AVAILABLE", 5),
        ("VALID-HANDLE", 12),
        ("VALID-OBJECT", 12),
        ("CAN-DO", 6),
        ("CAN-QUERY", 9),
        ("CAN-SET", 7),
        ("LOCKED", 6),
        ("AMBIGUOUS", 5),
    ];
    let lower = name.to_ascii_lowercase();
    Some(if SOURCES.iter().any(|s| s.eq_ignore_ascii_case(name)) {
        Builtin::Source
    } else if name.eq_ignore_ascii_case("QUOTER") {
        Builtin::Quoter
    } else if name.eq_ignore_ascii_case("HTML-ENCODE") {
        Builtin::Encode
    } else if NUMERIC.iter().any(|(f, m)| abbrev(name, f, *m)) {
        Builtin::Numeric
    } else if LOGICAL.iter().any(|(f, m)| abbrev(name, f, *m)) {
        Builtin::Logical
    } else if abbrev(name, "RETURN-VALUE", 10) {
        Builtin::ReturnValue
    } else if listed(&UNKNOWN_FUNCTIONS, &lower) {
        Builtin::Unknown
    } else if listed(&ARGS, &lower) {
        Builtin::Args
    } else {
        return None;
    })
}

/// Whether `name` looks like a web request: it has the word `request` or
/// `req`.
fn is_request(name: &str) -> bool {
    super::super::names::segments(name)
        .iter()
        .any(|w| w == "request" || w == "req")
}

impl Compiler<'_> {
    /// The slot of `key`, added when the unit is compiled.
    fn slot(&mut self, written: &str, key: String, global: bool, outside: bool) -> Option<u32> {
        if let Some(s) = self.slots.get(&key) {
            return Some(s);
        }
        if self.frozen {
            return None;
        }
        let id = self.slots.info.len() as u32;
        self.slots.info.push(SlotInfo {
            name: written.to_string(),
            key: key.clone(),
            global,
            outside,
        });
        self.slots.index.insert(key, id);
        Some(id)
    }

    /// The slot of a variable named `name`.
    pub fn var_slot(&mut self, name: &str) -> Option<u32> {
        let key = name.to_ascii_lowercase();
        let global = !self.declared.contains(&key);
        let outside = self.tables.shared_in.contains(&key) || global && self.tables.outside(&key);
        self.slot(name, key, global, outside)
    }

    /// The slot of field `field` of the table or buffer `qualifier`.
    pub fn field_slot(&mut self, qualifier: &str, field: &str) -> Option<u32> {
        let outside = self.tables.outside_table(qualifier);
        self.field_slot_in(qualifier, field, outside)
    }

    fn field_slot_in(&mut self, qualifier: &str, field: &str, outside: bool) -> Option<u32> {
        let key = format!(
            "{}.{}",
            self.tables.table_of(qualifier),
            field.to_ascii_lowercase()
        );
        self.slot(&format!("{qualifier}.{field}"), key, true, outside)
    }

    /// The slot that holds taint for every field of a table.
    pub fn table_slot(&mut self, table: &str) -> Option<u32> {
        let outside = self.tables.outside_table(table);
        self.table_slot_in(table, outside)
    }

    fn table_slot_in(&mut self, table: &str, outside: bool) -> Option<u32> {
        let key = format!("{}.*", self.tables.table_of(table));
        self.slot(table, key, true, outside)
    }

    /// A read of the variable `name`.
    fn var(&mut self, name: &str) -> Value {
        let key = name.to_ascii_lowercase();
        if !self.declared.contains(&key)
            && !self.tables.names.contains(&key)
            && let Some(v) = self.bare(&key)
        {
            return v;
        }
        if let Some(s) = self.var_slot(name) {
            return Value(vec![Term::Slot(s)]);
        }
        let outside = self.tables.shared_in.contains(&key) || self.tables.outside(&key);
        self.summary(name, &key, outside)
    }

    /// After the solve, a name the unit never used: its file-level summary,
    /// and Unknown for a name from `outside` the file.
    fn summary(&self, name: &str, key: &str, outside: bool) -> Value {
        if !self.frozen || self.declared.contains(key) {
            return Value::default();
        }
        let mut v = Value(vec![Term::File {
            name: name.to_string(),
            key: key.to_string(),
        }]);
        if outside {
            v.0.push(Term::Origin(UNKNOWN));
        }
        v
    }

    /// A read of `qualifier.field`. A database field is a source when the
    /// options say so; `receiver` reads the name of a class, not data.
    fn field(&mut self, qualifier: &str, field: &str, span: Span, receiver: bool) -> Value {
        let table = self.tables.table_of(qualifier);
        let outside = !receiver && self.tables.outside_table(qualifier);
        let mut v = Value::default();
        if let Some(s) = self.field_slot_in(qualifier, field, outside) {
            v.0.push(Term::Slot(s));
        } else {
            let key = format!("{table}.{}", field.to_ascii_lowercase());
            v = Value::union([
                v,
                self.summary(&format!("{qualifier}.{field}"), &key, outside),
            ]);
        }
        if let Some(s) = self.table_slot_in(qualifier, outside) {
            v.0.push(Term::Slot(s));
        } else {
            v = Value::union([v, self.summary(qualifier, &format!("{table}.*"), outside)]);
        }
        if self.options.database_fields && !receiver && !self.tables.temp_tables.contains(&table) {
            let key = format!("{table}.{}", field.to_ascii_lowercase());
            let id = self.origins.keyed(
                &key,
                || format!("database field `{qualifier}.{field}`"),
                span,
            );
            v.0.push(Term::Origin(id));
        }
        v
    }

    fn origin(&mut self, what: String, span: Span) -> Value {
        Value::origin(self.origins.add(what, span))
    }

    /// A call of a routine of the file: its result, by its summary.
    pub fn routine_call(
        &mut self,
        callees: &[u32],
        out: Out,
        args: Vec<Value>,
        span: Span,
    ) -> Value {
        if out == Out::Return {
            self.calls.push(CallIr {
                callees: callees.to_vec(),
                args: args.clone(),
                span,
                run: false,
            });
        }
        Value(vec![Term::Call {
            callees: callees.to_vec(),
            out,
            args,
        }])
    }

    /// Whether a call of the function `name`, which is not a routine of the
    /// file, may run code the file does not hold: a function of another
    /// program (`FUNCTION f ... IN h` or `IN SUPER`, or one the file does not
    /// declare), `DYNAMIC-FUNCTION`, `DYNAMIC-INVOKE`, `DYNAMIC-NEW`,
    /// `DYNAMIC-PROPERTY` and `SUPER`. Only a file that watches
    /// calls asks ([`Tables::watches`]).
    fn runs_elsewhere(&self, name: &str) -> bool {
        if !self.tables.watches() {
            return false;
        }
        let name = name.to_ascii_lowercase();
        matches!(
            name.as_str(),
            "dynamic-function" | "dynamic-invoke" | "dynamic-new" | "dynamic-property" | "super"
        ) || !oxabl_lexer::is_builtin_function(&name) && !oxabl_lexer::is_reserved_keyword(&name)
    }

    /// Whether `object:member` (a call when `call`) may run code the file does
    /// not hold: a method or property of an object of a class other than a
    /// JSON class, a static member of a class, and `INVOKE` of a handle. A
    /// member of a member is judged by the first object of the chain. Only a
    /// file that watches calls asks ([`Tables::watches`]).
    fn member_runs_elsewhere(&self, object: &Expression, member: &str, call: bool) -> bool {
        if !self.tables.watches() {
            return false;
        }
        match &object.kind {
            // `BUFFER b:...`, `TEMP-TABLE tt:...`, `DATASET ds:...`.
            E::Identifier(id) if object.span.start < id.span.start => false,
            E::Identifier(id) => self.name_runs_elsewhere(&id.name, member, call),
            E::ArrayAccess { array, .. } => self.member_runs_elsewhere(array, member, call),
            // A class (`pkg.Cls:Member`), or a field of a table.
            E::FieldAccess { .. } => match field_parts(object) {
                Some((q, _)) if self.is_table(q) => !is_keyword(member),
                _ => true,
            },
            E::New { .. } => true,
            // An object a function returns.
            E::FunctionCall { name, .. } => {
                let name = name.name.to_ascii_lowercase();
                !oxabl_lexer::is_builtin_function(&name)
                    || abbrev(&name, "cast", 4)
                    || abbrev(&name, "dynamic-cast", 12)
            }
            _ => false,
        }
    }

    /// [`Self::member_runs_elsewhere`] for an object the source names `o`. A
    /// name the file does not declare as a variable is a system handle, a
    /// table, a buffer or a dataset; a table the schema knows, whose member is
    /// a keyword (a buffer attribute or method); or else a class, or a name
    /// from outside the file, either of which may run code: `Other:Run()`.
    fn name_runs_elsewhere(&self, o: &str, member: &str, call: bool) -> bool {
        let o = o.to_ascii_lowercase();
        match self.tables.types.get(&o) {
            Some(class) if class.is_empty() => call && member.eq_ignore_ascii_case("INVOKE"),
            Some(class) => !is_json_class(class),
            None if oxabl_semantic::SYSTEM_HANDLES.contains(&o.as_str()) || self.is_table(&o) => {
                false
            }
            None if self.tables.receivers.contains(&o) => !is_keyword(member),
            None => true,
        }
    }

    /// Whether the file defines `name` as anything but a variable (a table, a
    /// buffer, a dataset, ...), or the schema knows it as a table.
    fn is_table(&self, name: &str) -> bool {
        let name = name.to_ascii_lowercase();
        self.tables.names.contains(&name) || self.tables.database.contains(&name)
    }

    /// What the built-in method `object:method(arguments)` of a handle writes
    /// into its arguments that are a variable or a field (see
    /// [`argument_write`]), as weak writes. A method of an object of a class
    /// writes only its `OUTPUT` arguments, and `SAVE("FILE", name)` and the
    /// like write a file, not `name`.
    pub fn method_writes<'e>(
        &mut self,
        object: &Expression,
        method: &str,
        arguments: &'e [Expression],
    ) -> Vec<(&'e Expression, Value)> {
        let write = argument_write(method);
        if write == ArgWrite::Input || self.owner(object, method) == Owner::Class {
            return Vec::new();
        }
        if arguments.first().is_some_and(|a| {
            matches!(&a.kind, E::Literal(oxabl_ast::Literal::String(s))
                if s.value.eq_ignore_ascii_case("FILE"))
        }) {
            return Vec::new();
        }
        let written = |a: &Expression| match &a.kind {
            E::Identifier(id) => a.span.start >= id.span.start,
            E::FieldAccess { .. } | E::ArrayAccess { .. } => true,
            _ => false,
        };
        if !arguments.iter().any(written) {
            return Vec::new();
        }
        let receiver = self.receiver(object);
        let mut out = Vec::new();
        for (i, a) in arguments.iter().enumerate() {
            if !written(a) {
                continue;
            }
            let value = match write {
                ArgWrite::Carry => Value::union(
                    std::iter::once(receiver.clone())
                        .chain(
                            arguments
                                .iter()
                                .enumerate()
                                .filter(|(j, _)| *j != i)
                                .map(|(_, o)| self.expr(o)),
                        )
                        .collect::<Vec<_>>(),
                ),
                _ => Value::union([receiver.clone(), Value::origin(UNKNOWN)]),
            };
            out.push((a, value));
        }
        out
    }

    /// Whether a read of the name `o` as a value, not as a receiver, copies a
    /// handle that code the file does not hold fills: a foreign system handle
    /// (`h = SOURCE-PROCEDURE`, `h = SELF`), or one of [`SHARED_HANDLES`]
    /// whose `PRIVATE-DATA` the file does not set. What the copy then reads
    /// holds Unknown.
    fn copies_foreign(&self, o: &str) -> bool {
        let key = o.to_ascii_lowercase();
        !self.declared.contains(&key)
            && !self.tables.names.contains(&key)
            && (self.name_owner(&key, "") == Owner::Foreign
                || self.tables.shared_private_data(&key))
    }

    /// Whose object `object:member` reads. A member of a member belongs to
    /// the owner of the first object of the chain.
    fn owner(&self, object: &Expression, member: &str) -> Owner {
        match &object.kind {
            // `BUFFER b:...`, `TEMP-TABLE tt:...`, `DATASET ds:...`.
            E::Identifier(id) if object.span.start < id.span.start => Owner::Program,
            E::Identifier(id) => self.name_owner(&id.name, member),
            E::ArrayAccess { array, .. } => self.owner(array, member),
            E::MemberAccess { object, member }
            | E::MethodCall {
                object,
                method: member,
                ..
            } => self.owner(object, &member.name),
            // A built-in function that returns a handle (`WIDGET-HANDLE(c)`).
            E::FunctionCall { name, .. }
                if oxabl_lexer::is_builtin_function(&name.name.to_ascii_lowercase())
                    && !abbrev(&name.name, "cast", 4)
                    && !abbrev(&name.name, "dynamic-cast", 12) =>
            {
                Owner::Program
            }
            // A user function or a field: a handle when the member is a
            // keyword.
            E::FunctionCall { .. } | E::FieldAccess { .. } if is_keyword(member) => Owner::Program,
            _ => Owner::Class,
        }
    }

    /// [`Self::owner`] of an object the source names `o`: a variable of a
    /// class, a handle variable, a system handle, a table or buffer, or a
    /// name the file does not declare (a class, or a handle when the member
    /// is a keyword).
    fn name_owner(&self, o: &str, member: &str) -> Owner {
        let o = o.to_ascii_lowercase();
        match self.tables.types.get(&o) {
            Some(class) if class.is_empty() => Owner::Program,
            Some(_) => Owner::Class,
            None if o == "this-object" || o == "super" => Owner::Class,
            None if PROGRAM_HANDLES.contains(&o.as_str()) => Owner::Program,
            // `ERROR-STATUS`, `SELF`, ..., and a system handle on no list.
            None if FOREIGN_HANDLES.contains(&o.as_str())
                || oxabl_semantic::SYSTEM_HANDLES.contains(&o.as_str()) =>
            {
                Owner::Foreign
            }
            None if self.is_table(&o) || self.tables.receivers.contains(&o) => Owner::Program,
            None if is_keyword(member) => Owner::Program,
            None => Owner::Class,
        }
    }

    /// A call of the function `name`; `key` is its first argument when that
    /// is a string literal.
    fn call(&mut self, name: &str, args: Vec<Value>, span: Span, key: Option<&str>) -> Value {
        if let Some(mask) = self.options.validator(name) {
            return Value::union(args).cleared(mask);
        }
        if self.options.is_identity_source(name) {
            return self
                .origin(format!("client identity `{name}`"), span)
                .identity();
        }
        if self.options.is_source(name) {
            return self.origin(format!("project source `{name}`"), span);
        }
        if let Some(callees) = self.routines.callable(name) {
            let callees = callees.to_vec();
            return self.routine_call(&callees, Out::Return, args, span);
        }
        if self.runs_elsewhere(name) {
            self.foreign.push(span);
        }
        // `DYNAMIC-FUNCTION("f")`, with no `IN` handle (nothing after the
        // name): the function `f` of the file.
        if listed(&["dynamic-function"], &name.to_ascii_lowercase())
            && args.len() == 1
            && let Some(callees) = key.and_then(|k| self.routines.callable(k))
        {
            let callees = callees.to_vec();
            return self.routine_call(&callees, Out::Return, Vec::new(), span);
        }
        match builtin(name) {
            Builtin::Source if name.eq_ignore_ascii_case("OS-GETENV") => {
                self.origin(format!("environment `{name}`"), span)
            }
            Builtin::Source => match key.filter(|k| self.options.is_identity_key(k)) {
                Some(k) => self
                    .origin(format!("web input `{name}` for `{k}`"), span)
                    .identity(),
                None => self.origin(format!("web input `{name}`"), span),
            },
            Builtin::Quoter => args
                .into_iter()
                .next()
                .unwrap_or_default()
                .cleared(SinkKind::Query.bit()),
            Builtin::Numeric => Value::union(args).cleared(NUMERIC_SAFE),
            Builtin::Logical if self.decision => Value::union(args),
            Builtin::Logical => Value::union(args).logical(),
            Builtin::Encode => Value::union(args).cleared(SinkKind::Html.bit()),
            Builtin::ReturnValue => self.return_value(),
            Builtin::Unknown => Value::union(args.into_iter().chain([Value::origin(UNKNOWN)])),
            Builtin::Args | Builtin::Other => Value::union(args),
        }
    }

    /// `RETURN-VALUE`: what the last `RUN` of the unit left, or what it held
    /// when the unit started, which is Unknown.
    fn return_value(&mut self) -> Value {
        match self.return_value_slot() {
            Some(s) => Value(vec![Term::Slot(s)]),
            None => Value::origin(UNKNOWN),
        }
    }

    /// The slot of `RETURN-VALUE`, added when the unit is compiled.
    pub fn return_value_slot(&mut self) -> Option<u32> {
        self.slot("RETURN-VALUE", RETURN_VALUE.to_string(), false, false)
    }

    /// The slot of [`ON_RETURN`].
    pub fn on_return_slot(&mut self) -> Option<u32> {
        self.slot("RETURN-VALUE", ON_RETURN.to_string(), false, false)
    }

    /// A built-in function that the source calls without parentheses, as
    /// `name` (lower-cased) is when the file defines no such name.
    fn bare(&mut self, key: &str) -> Option<Value> {
        match builtin(key) {
            Builtin::ReturnValue => Some(self.return_value()),
            _ if !BARE_FUNCTIONS.contains(&key) => None,
            // An unreserved name (`TODAY`) may be a variable of the includer.
            _ if self.tables.open && !oxabl_lexer::is_reserved_keyword(key) => None,
            Builtin::Unknown => Some(Value::origin(UNKNOWN)),
            _ => Some(Value::default()),
        }
    }

    /// `object:member`, or `object:member(args)` when `args` is given.
    fn access(
        &mut self,
        receiver: Receiver,
        object: Value,
        member: &str,
        args: Option<Vec<Value>>,
        span: Span,
        key: Option<&str>,
    ) -> Value {
        let identity = args.is_some()
            && member.len() > 3
            && member[..3].eq_ignore_ascii_case("get")
            && key.is_some_and(|k| self.options.is_identity_key(k));
        let value = self.access_value(receiver, object, member, args, span);
        if identity { value.identity() } else { value }
    }

    fn access_value(
        &mut self,
        receiver: Receiver,
        object: Value,
        member: &str,
        args: Option<Vec<Value>>,
        span: Span,
    ) -> Value {
        if let Some(o) = receiver.name {
            if o.eq_ignore_ascii_case("WEB-CONTEXT") {
                return self.origin(format!("`WEB-CONTEXT:{member}`"), span);
            }
            // `SESSION:PARAM` abbreviates `PARAMETER`.
            if o.eq_ignore_ascii_case("SESSION")
                && (abbrev(member, "PARAMETER", 5)
                    || ["STARTUP-PARAMETERS", "ICFPARAMETER"]
                        .iter()
                        .any(|m| m.eq_ignore_ascii_case(member)))
            {
                return self.origin(format!("`SESSION:{member}`"), span);
            }
            if o.eq_ignore_ascii_case("THIS-OBJECT") {
                match (&args, self.routines.method(member)) {
                    (None, _) => return self.var(member),
                    (Some(_), Some(callees)) => {
                        let callees = callees.to_vec();
                        return self.routine_call(
                            &callees,
                            Out::Return,
                            args.unwrap_or_default(),
                            span,
                        );
                    }
                    _ => {}
                }
            }
            if is_request(o) {
                return self.origin(format!("web request `{o}:{member}`"), span);
            }
        }
        // Any program of the session, or the caller of this one, may set it.
        let object = match receiver.name {
            Some(o)
                if member.eq_ignore_ascii_case("PRIVATE-DATA")
                    && self.tables.shared_private_data(o) =>
            {
                Value::union([object, Value::origin(UNKNOWN)])
            }
            _ => object,
        };
        let Some(args) = args else {
            return receiver.owner.read(member, object);
        };
        if let Some(mask) = self.options.validator(member) {
            return Value::union(args).cleared(mask);
        }
        if self.options.is_source(member) {
            return self.origin(format!("project source `{member}`"), span);
        }
        receiver
            .owner
            .read(member, Value::union(std::iter::once(object).chain(args)))
    }

    /// The value of an expression. Every operand (see [`operands`]) is read
    /// first, so the calls in it run whatever the kind makes of its value: a
    /// comparison, a logical operator, `CAN-FIND` and the condition of an `IF`
    /// expression pass on only its identity ([`Value::logical`]), and a
    /// subscript runs its calls and passes on no value (see
    /// [`Self::effects`]). A kind the compiler does not model is Unknown.
    pub fn expr(&mut self, e: &Expression) -> Value {
        let receiver = match &e.kind {
            E::MemberAccess { object, .. } | E::MethodCall { object, .. } => Some(&**object),
            _ => None,
        };
        let values: Vec<Value> = operands(e)
            .into_iter()
            .enumerate()
            .map(|(i, o)| match receiver {
                Some(r) if std::ptr::eq(r, o) => self.receiver(o),
                _ => match passes_on(e, i) {
                    Pass::Value => self.expr(o),
                    Pass::Identity => self.expr(o).logical(),
                    Pass::Nothing => self.effects(o),
                },
            })
            .collect();
        match &e.kind {
            E::Literal(_) => Value::default(),
            // A copy of a foreign handle, or of a handle whose `PRIVATE-DATA`
            // other code sets, may hold anything: Unknown.
            E::Identifier(id) if self.copies_foreign(&id.name) => {
                Value::union([self.var(&id.name), Value::origin(UNKNOWN)])
            }
            E::Identifier(id) => self.var(&id.name),
            E::FieldAccess { .. } => match field_parts(e) {
                Some((q, f)) => self.field(q, f, e.span, false),
                None => Value(vec![Term::Unknown]),
            },
            E::Add(..) => Value::union(values),
            E::Minus(..) | E::Multiply(..) | E::Divide(..) | E::Modulo(..) | E::Negate(..) => {
                Value::union(values).cleared(NUMERIC_SAFE)
            }
            E::Equal(..)
            | E::NotEqual(..)
            | E::LessThan(..)
            | E::LessThanOrEqual(..)
            | E::GreaterThan(..)
            | E::GreaterThanOrEqual(..)
            | E::Begins(..)
            | E::Matches(..)
            | E::Contains(..)
            | E::And(..)
            | E::Or(..)
            | E::Not(..)
            | E::CanFind { .. }
            | E::IfThenElse(..) => Value::union(values),
            E::FunctionCall { name, arguments } => {
                self.call(&name.name, values, e.span, literal_key(arguments))
            }
            E::MemberAccess { object, member } => {
                if self.member_runs_elsewhere(object, &member.name, false) {
                    self.foreign.push(e.span);
                }
                let receiver = Receiver {
                    name: receiver_name(object),
                    owner: self.owner(object, &member.name),
                };
                let object = values.into_iter().next().unwrap_or_default();
                self.access(receiver, object, &member.name, None, e.span, None)
            }
            E::MethodCall {
                object,
                method,
                arguments,
            } => {
                if self.member_runs_elsewhere(object, &method.name, true) {
                    self.foreign.push(e.span);
                }
                let receiver = Receiver {
                    name: receiver_name(object),
                    owner: self.owner(object, &method.name),
                };
                let mut values = values.into_iter();
                let object = values.next().unwrap_or_default();
                let key = literal_key(arguments);
                self.access(
                    receiver,
                    object,
                    &method.name,
                    Some(values.collect()),
                    e.span,
                    key,
                )
            }
            E::ArrayAccess { .. } => values.into_iter().next().unwrap_or_default(),
            E::New { .. } => {
                if self.tables.watches() {
                    self.foreign.push(e.span);
                }
                Value::union(values)
            }
            E::IncludeReference { .. }
            | E::IncludeArgReference { .. }
            | E::PreprocReference(_)
            | E::PreprocIf(_) => Value(vec![Term::Unknown]),
        }
    }

    /// Run the calls in an operand whose value is not passed on. An operand
    /// that holds no call, member access or `NEW` runs nothing, and is not
    /// read, so it adds no slot to the unit.
    fn effects(&mut self, e: &Expression) -> Value {
        if may_run(e) {
            self.expr(e);
        }
        Value::default()
    }

    /// The value of the receiver of a member access.
    fn receiver(&mut self, object: &Expression) -> Value {
        match &object.kind {
            // `BUFFER b:...`, `TEMP-TABLE tt:...`, `DATASET ds:...`.
            E::Identifier(id) if object.span.start < id.span.start => {
                let tables = self.tables.tables_of(&id.name);
                Value(tables.into_iter().map(Term::Table).collect())
            }
            // A system handle read through its own name: its owner decides.
            E::Identifier(id) => self.var(&id.name),
            // A dotted name before `:` is a class (`Progress.Lang.Class`) or a
            // buffer field holding an object.
            E::FieldAccess { .. } => match field_parts(object) {
                Some((q, f)) => self.field(q, f, object.span, true),
                None => self.expr(object),
            },
            _ => self.expr(object),
        }
    }

    /// The slot an assignment to `target` writes, and whether it is a strong
    /// update. The calls in its operands that are not names, such as a
    /// subscript, run.
    pub fn target(&mut self, target: &Expression) -> Vec<(u32, bool)> {
        for o in operands(target) {
            if !matches!(o.kind, E::Identifier(_) | E::FieldAccess { .. }) {
                self.expr(o);
            }
        }
        match &target.kind {
            E::Identifier(id) => self
                .var_slot(&id.name)
                .map(|s| (s, true))
                .into_iter()
                .collect(),
            E::ArrayAccess { array, .. } => self
                .target(array)
                .into_iter()
                .map(|(s, _)| (s, false))
                .collect(),
            E::FieldAccess { .. } => field_parts(target)
                .and_then(|(q, f)| self.field_slot(q, f))
                .map(|s| (s, false))
                .into_iter()
                .collect(),
            E::MemberAccess { object, member } => match &object.kind {
                E::Identifier(o) if o.name.eq_ignore_ascii_case("THIS-OBJECT") => self
                    .var_slot(&member.name)
                    .map(|s| (s, true))
                    .into_iter()
                    .collect(),
                // The object now holds the value.
                E::Identifier(o) => {
                    if self.member_runs_elsewhere(object, &member.name, false) {
                        self.foreign.push(target.span);
                    }
                    self.var_slot(&o.name)
                        .map(|s| (s, false))
                        .into_iter()
                        .collect()
                }
                _ => Vec::new(),
            },
            // `SUBSTRING(c, 1, 2) = x`, `ENTRY(1, c) = x`: part of `c` changes.
            E::FunctionCall { arguments, .. } => arguments
                .iter()
                .filter_map(|a| match &a.kind {
                    E::Identifier(id) => self.var_slot(&id.name).map(|s| (s, false)),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// The slot a guard narrows: a variable or a field.
    pub fn guarded(&mut self, e: &Expression) -> Option<u32> {
        match &e.kind {
            E::Identifier(id) => self.var_slot(&id.name),
            E::FieldAccess { .. } => field_parts(e).and_then(|(q, f)| self.field_slot(q, f)),
            _ => None,
        }
    }

    /// The slots that `cond` being `positive` cleans, with the kinds it
    /// cleans each for.
    pub fn guard(&mut self, cond: &Expression, positive: bool) -> Vec<(u32, u32)> {
        match &cond.kind {
            E::Not(c) => self.guard(c, !positive),
            E::And(a, b) if positive => {
                let mut out = self.guard(a, true);
                out.extend(self.guard(b, true));
                out
            }
            E::Or(a, b) if !positive => {
                let mut out = self.guard(a, false);
                out.extend(self.guard(b, false));
                out
            }
            E::Or(a, b) => {
                let left = self.guard(a, true);
                let right = self.guard(b, true);
                left.into_iter()
                    .filter_map(|(s, m)| {
                        let other = right
                            .iter()
                            .filter(|(r, _)| *r == s)
                            .fold(0, |acc, (_, m)| acc | m);
                        (m & other != 0).then_some((s, m & other))
                    })
                    .collect()
            }
            E::Equal(a, b) | E::NotEqual(a, b) => {
                // The parser reads `NOT a = b` as `(NOT a) = b`.
                if let E::Not(inner) = &a.kind {
                    let mut flipped = cond.clone();
                    match &mut flipped.kind {
                        E::Equal(l, _) | E::NotEqual(l, _) => *l = inner.clone(),
                        _ => {}
                    }
                    return self.guard(&flipped, !positive);
                }
                let equal = matches!(cond.kind, E::Equal(..));
                if let Some(slot) = self.lookup_member(a)
                    && is_zero(b)
                {
                    // `LOOKUP(x, list) = 0` holds when `x` is not a member.
                    return if equal != positive {
                        vec![(slot, ALL)]
                    } else {
                        Vec::new()
                    };
                }
                if equal != positive {
                    return Vec::new();
                }
                match (is_constant(a), is_constant(b)) {
                    (false, true) => self.guarded(a).map(|s| (s, ALL)).into_iter().collect(),
                    (true, false) => self.guarded(b).map(|s| (s, ALL)).into_iter().collect(),
                    _ => Vec::new(),
                }
            }
            E::GreaterThan(a, b) | E::GreaterThanOrEqual(a, b) if positive => {
                let floor = matches!(cond.kind, E::GreaterThan(..)) && is_zero(b)
                    || matches!(cond.kind, E::GreaterThanOrEqual(..)) && is_one(b);
                match self.lookup_member(a) {
                    Some(slot) if floor => vec![(slot, ALL)],
                    _ => Vec::new(),
                }
            }
            E::FunctionCall { name, arguments } if positive => {
                if let Some(mask) = self.options.validator(&name.name) {
                    return arguments
                        .iter()
                        .filter_map(|a| self.guarded(a).map(|s| (s, mask)))
                        .collect();
                }
                if name.name.eq_ignore_ascii_case("CAN-DO")
                    && let [list, value] = arguments.as_slice()
                    && is_plain_list(list)
                {
                    return self.guarded(value).map(|s| (s, ALL)).into_iter().collect();
                }
                Vec::new()
            }
            E::MethodCall {
                method, arguments, ..
            } if positive => match self.options.validator(&method.name) {
                Some(mask) => arguments
                    .iter()
                    .filter_map(|a| self.guarded(a).map(|s| (s, mask)))
                    .collect(),
                None => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    /// The slot `x` of `LOOKUP(x, list)` with a literal list.
    fn lookup_member(&mut self, e: &Expression) -> Option<u32> {
        let E::FunctionCall { name, arguments } = &e.kind else {
            return None;
        };
        if !abbrev(&name.name, "LOOKUP", 6) {
            return None;
        }
        match arguments.as_slice() {
            [value, list, ..] if is_constant(list) => self.guarded(value),
            _ => None,
        }
    }

    /// The value of the tokens in `range`.
    pub fn tokens(&mut self, st: &StatementText, range: Range<usize>) -> Value {
        let mut v = Value::default();
        let end = range.end.min(st.len());
        let mut i = range.start;
        while i < end {
            let word = st.word(i);
            if word.starts_with('{') {
                v.0.push(Term::Unknown);
                i += 1;
                continue;
            }
            if !is_word(st, i) || is_operator_word(word) {
                i += 1;
                continue;
            }
            // `NEW c(...)` runs the constructor of a class; `NEW(b)` is a function.
            if st.is_word(i, "NEW") && !st.is(i + 1, Kind::LeftParen) && self.tables.watches() {
                self.foreign.push(st.span(i..i + 1));
            }
            // The condition of `IF c THEN a ELSE b` picks a value; it is not
            // one, but its calls run.
            if st.is_word(i, "IF")
                && let Some(then) = (i + 1..end).find(|&j| st.is_word(j, "THEN"))
            {
                self.tokens(st, i + 1..then);
                i = then + 1;
                continue;
            }
            if st.is(i + 1, Kind::LeftParen)
                && let Some(close) = st.close_of(i + 1).filter(|&c| c < end)
            {
                let ranges = st.call_args(i + 1);
                let key = ranges.first().and_then(|r| token_key(st, r.clone()));
                let args = ranges.into_iter().map(|a| self.tokens(st, a)).collect();
                v = Value::union([v, self.call(word, args, st.span(i..close + 1), key)]);
                i = close + 1;
                continue;
            }
            // `BUFFER b:...`, `TEMP-TABLE tt:...`, `DATASET ds:...`.
            if ["BUFFER", "TEMP-TABLE", "DATASET"]
                .iter()
                .any(|w| w.eq_ignore_ascii_case(word))
                && is_word(st, i + 1)
                && st.is(i + 2, Kind::Colon)
                && is_word(st, i + 3)
            {
                let (value, next) = self.token_chain(st, i + 1, end, true);
                v = Value::union([v, value]);
                i = next;
                continue;
            }
            if st.is(i + 1, Kind::Colon) && is_word(st, i + 2) {
                let (value, next) = self.token_chain(st, i, end, false);
                v = Value::union([v, value]);
                i = next;
                continue;
            }
            if dotted(st, i) {
                // `t.f`, or `db.t.f`.
                let (mut q, mut j) = (i, i + 2);
                while dotted(st, j) {
                    q = j;
                    j += 2;
                }
                // A dotted name before `:` is a class.
                let receiver = st.is(j + 1, Kind::Colon);
                if receiver
                    && self.tables.watches()
                    && (!self.is_table(st.word(q)) || !is_keyword(st.word(j + 2)))
                {
                    self.foreign.push(st.span(i..j + 1));
                }
                let field = self.field(st.word(q), st.word(j), st.span(i..j + 1), receiver);
                v = Value::union([v, field]);
                i = j + 1;
                continue;
            }
            v = Value::union([v, self.var(word)]);
            i += 1;
        }
        v
    }

    /// `a:b:c(args)...` starting at `i`; the value and the index after it.
    /// `prefixed`: `a` follows `BUFFER`, `TEMP-TABLE` or `DATASET`, and the
    /// chain reads its tables.
    fn token_chain(
        &mut self,
        st: &StatementText,
        i: usize,
        end: usize,
        prefixed: bool,
    ) -> (Value, usize) {
        let first = st.word(i);
        let mut j = i + 1;
        let (mut name, mut value, owner) = if prefixed {
            let tables = self.tables.tables_of(first);
            (
                None,
                Value(tables.into_iter().map(Term::Table).collect()),
                Owner::Program,
            )
        } else {
            (
                Some(first.to_string()),
                self.var(first),
                self.name_owner(first, st.word(j + 1)),
            )
        };
        if !prefixed
            && self.tables.watches()
            && self.name_runs_elsewhere(first, st.word(j + 1), st.is(j + 2, Kind::LeftParen))
        {
            self.foreign.push(st.span(i..j + 2));
        }
        while j + 1 < end && st.is(j, Kind::Colon) && is_word(st, j + 1) {
            let member = st.word(j + 1);
            let mut args = None;
            let mut key = None;
            let mut next = j + 2;
            if st.is(j + 2, Kind::LeftParen)
                && let Some(close) = st.close_of(j + 2).filter(|&c| c < end)
            {
                let ranges = st.call_args(j + 2);
                key = ranges.first().and_then(|r| token_key(st, r.clone()));
                args = Some(ranges.into_iter().map(|a| self.tokens(st, a)).collect());
                next = close + 1;
            }
            let span = st.span(i..next);
            let receiver = Receiver {
                name: name.as_deref(),
                owner,
            };
            value = self.access(receiver, value, member, args, span, key);
            name = None;
            j = next;
        }
        (value, j)
    }
}

/// The receiver of a member read.
#[derive(Debug, Clone, Copy)]
struct Receiver<'n> {
    /// The receiver when it is a plain name.
    name: Option<&'n str>,
    owner: Owner,
}

/// Whose object a member read reads, which decides what the read gives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Owner {
    /// An object of a class: the read gives what the object and the
    /// arguments hold, as an unknown call does.
    Class,
    /// A handle of the program (a buffer, a query, a widget, `SESSION`,
    /// `FILE-INFO`, ...): the handle holds what the program set on it, so a
    /// member of [`MODELLED_MEMBERS`] gives what the handle and the arguments
    /// hold. Any other member gives data the runtime fills (a field of the
    /// record, the text on the screen, a node of an XML document): Unknown.
    Program,
    /// A handle of an object that the runtime or another program fills
    /// ([`FOREIGN_HANDLES`]): a member of [`NO_INPUT_MEMBERS`] holds no input;
    /// any other member is Unknown.
    Foreign,
}

impl Owner {
    /// The value of a read of `member`, given what the object and the
    /// arguments hold.
    fn read(self, member: &str, value: Value) -> Value {
        let listed = |list: &[&str]| list.iter().any(|m| m.eq_ignore_ascii_case(member));
        // `BUFFER b:BUFFER-FIELD("f"):BUFFER-VALUE` reads what the tables of
        // `b` hold, which its receiver holds.
        let fields = member.eq_ignore_ascii_case("BUFFER-VALUE")
            && value.0.iter().any(|t| matches!(t, Term::Table(_)));
        let modelled = match self {
            Owner::Class => true,
            Owner::Program => listed(&MODELLED_MEMBERS) || listed(&NO_INPUT_MEMBERS) || fields,
            Owner::Foreign => listed(&NO_INPUT_MEMBERS),
        };
        if modelled {
            value
        } else {
            Value::union([value, Value::origin(UNKNOWN)])
        }
    }
}

/// System handles of objects of the program or the session, which hold what
/// the program set on them (`FILE-INFO:FILE-NAME`) or the session's settings.
/// `THIS-OBJECT` and `SUPER` are objects of a class; `WEB-CONTEXT` and the
/// parameters of `SESSION` are sources.
const PROGRAM_HANDLES: [&str; 33] = [
    "audit-control",
    "audit-policy",
    "codebase-locator",
    "codepage-table",
    "color-table",
    "compiler",
    "current-window",
    "active-window",
    "debugger",
    "default-window",
    "dslog-manager",
    "file-info",
    "file-information",
    "font-table",
    "log-manager",
    "profiler",
    "rcode-info",
    "rcode-infor",
    "rcode-inform",
    "rcode-informa",
    "rcode-informat",
    "rcode-informati",
    "rcode-informatio",
    "rcode-information",
    "security-policy",
    "session",
    "term",
    "termi",
    "termin",
    "termina",
    "terminal",
    "this-procedure",
    "web-context",
];

/// System handles of the program whose `PRIVATE-DATA` code the file does not
/// hold may set: any program of the session, or the caller on the handle of a
/// persistent procedure. Unknown unless the file sets it.
const SHARED_HANDLES: [&str; 2] = ["session", "this-procedure"];

/// System handles of objects that the runtime or another program fills: the
/// error of the last statement, the widget of the event, the clipboard, the
/// procedure that ran this one. A system handle on neither list is one too.
const FOREIGN_HANDLES: [&str; 14] = [
    "active-form",
    "clipboard",
    "com-self",
    "error-stat",
    "error-status",
    "error-statu",
    "first-object",
    "focus",
    "last-even",
    "last-event",
    "last-object",
    "self",
    "source-procedure",
    "target-procedure",
];

/// Built-in attributes and methods whose value holds no input, whoever owns
/// the handle: numbers, logicals, kinds and names of objects that the
/// program or the schema defines.
const NO_INPUT_MEMBERS: [&str; 71] = [
    "active",
    "available",
    "bgcolor",
    "blank",
    "checked",
    "code",
    "column",
    "column-label",
    "connected",
    "cursor-offset",
    "data-type",
    "dbname",
    "description",
    "display-type",
    "error",
    "event-type",
    "extent",
    "fgcolor",
    "file-mod-date",
    "file-mod-time",
    "file-name",
    "file-size",
    "file-type",
    "first-child",
    "format",
    "frame",
    "function",
    "get-number",
    "handle",
    "height-chars",
    "height-pixels",
    "help",
    "hidden",
    "index",
    "is-json",
    "is-xml",
    "label",
    "last-child",
    "length",
    "line",
    "locked",
    "modified",
    "name",
    "new",
    "next-sibling",
    "num-buffers",
    "num-children",
    "num-fields",
    "num-messages",
    "num-results",
    "num-selected-rows",
    "parent",
    "persistent",
    "position",
    "prev-sibling",
    "read-only",
    "recid",
    "remote",
    "row",
    "rowid",
    "sensitive",
    "subtype",
    "table",
    "type",
    "unique-id",
    "visible",
    "width-chars",
    "width-pixels",
    "window",
    "x",
    "y",
];

/// Built-in attributes and methods whose value, on a handle of the program,
/// is what the program set on the handle (`PRIVATE-DATA`, `FULL-PATHNAME`
/// after `FILE-NAME`), a handle it reaches (`BUFFER-FIELD`), a logical of an
/// action (`QUERY-PREPARE`), or a setting of the session
/// (`TEMP-DIRECTORY`): what the handle and the arguments hold gives it. Any
/// member on neither list, such as `BUFFER-VALUE`, `SCREEN-VALUE`,
/// `NODE-VALUE` or `GET-MESSAGE`, gives data the runtime fills, and is
/// Unknown.
const MODELLED_MEMBERS: [&str; 74] = [
    "add-buffer",
    "add-fields-from",
    "add-index-field",
    "add-like-field",
    "add-new-field",
    "add-new-index",
    "add-relation",
    "append-child",
    "buffer-copy",
    "buffer-create",
    "buffer-delete",
    "buffer-field",
    "buffer-release",
    "copy-dataset",
    "copy-temp-table",
    "create-like",
    "create-node",
    "date-format",
    "default-buffer-handle",
    "default-value",
    "empty-dataset",
    "empty-temp-table",
    "end-element",
    "fetch-selected-row",
    "find-by-rowid",
    "find-first",
    "find-last",
    "find-unique",
    "first-column",
    "full-pathname",
    "get-buffer-handle",
    "get-document-element",
    "get-first",
    "get-last",
    "get-next",
    "get-prev",
    "get-relation",
    "get-signature",
    "image",
    "index-information",
    "insert-attribute",
    "list-item-pairs",
    "list-items",
    "move-to-top",
    "next-column",
    "numeric-format",
    "page-top",
    "parent-buffer",
    "pathname",
    "private-data",
    "query-close",
    "query-off-end",
    "query-open",
    "query-prepare",
    "read-json",
    "read-xml",
    "reposition-to-rowid",
    "save",
    "serialize-hidden",
    "serialize-name",
    "set-attribute",
    "set-buffers",
    "set-socket-option",
    "set-wait-state",
    "start-element",
    "table-handle",
    "temp-directory",
    "temp-table-prepare",
    "title",
    "validate",
    "write-data-element",
    "write-json",
    "write-xml",
    "x-document",
];

/// What a built-in method of a handle writes into an argument that is a
/// variable or a field. ABL writes such an argument without `OUTPUT`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ArgWrite {
    /// It takes what the receiver and the other arguments hold: a node of the
    /// receiver's document, the document saved to a `LONGCHAR`
    /// ([`CARRYING_METHODS`]).
    Carry,
    /// It takes the receiver and Unknown: data the runtime fills
    /// ([`UNKNOWN_WRITE_METHODS`]), and any method on no list.
    Unknown,
    /// It is only read ([`INPUT_METHODS`]).
    Input,
}

/// What `method` writes into its arguments.
pub(super) fn argument_write(method: &str) -> ArgWrite {
    let listed = |list: &[&str]| list.iter().any(|m| m.eq_ignore_ascii_case(method));
    if listed(&CARRYING_METHODS) {
        ArgWrite::Carry
    } else if listed(&INPUT_METHODS) {
        ArgWrite::Input
    } else {
        ArgWrite::Unknown
    }
}

/// Built-in methods that write into an argument what the receiver holds:
/// `GET-DOCUMENT-ELEMENT(hRoot)`, `GET-CHILD(hKid, i)`,
/// `SAVE("LONGCHAR", lc)`, `WRITE-JSON("LONGCHAR", lc)`.
const CARRYING_METHODS: [&str; 18] = [
    "add-header-entry",
    "clone-node",
    "create-node",
    "create-node-namespace",
    "get-attribute-node",
    "get-child",
    "get-document-element",
    "get-header-entry",
    "get-node",
    "get-parent",
    "import-node",
    "merge-changes",
    "merge-row-changes",
    "save",
    "serialize-row",
    "write-json",
    "write-xml",
    "write-xmlschema",
];

/// Built-in methods that write into an argument data the runtime fills or the
/// lists do not model: `NODE-VALUE-TO-LONGCHAR(lc)`, the socket `READ`, the
/// target of `SET-OUTPUT-DESTINATION` and an `OUTPUT` parameter of
/// `SET-PARAMETER`. A method on no list is read the same way.
#[cfg(test)]
const UNKNOWN_WRITE_METHODS: [&str; 11] = [
    "buffer-export",
    "buffer-export-fields",
    "export",
    "node-value-to-longchar",
    "node-value-to-memptr",
    "raw-transfer",
    "read",
    "set-output-destination",
    "set-parameter",
    "string-value",
    "user-data",
];

/// Built-in methods that only read their arguments.
const INPUT_METHODS: [&str; 304] = [
    "accept-changes",
    "accept-row-changes",
    "add-buffer",
    "add-calc-column",
    "add-columns-from",
    "add-events-procedure",
    "add-fields-from",
    "add-first",
    "add-index-field",
    "add-last",
    "add-like-column",
    "add-like-field",
    "add-like-index",
    "add-new-field",
    "add-new-index",
    "add-parent-id-relation",
    "add-relation",
    "add-schema-location",
    "add-source-buffer",
    "add-super-procedure",
    "append-child",
    "apply-callback",
    "attach-data-source",
    "authentication-failed",
    "begin-event-group",
    "buffer-compare",
    "buffer-copy",
    "buffer-create",
    "buffer-delete",
    "buffer-field",
    "buffer-import",
    "buffer-import-fields",
    "buffer-release",
    "buffer-validate",
    "buffer-value",
    "cancel-break",
    "cancel-requests",
    "cancel-requests-after",
    "clear",
    "clear-appl-context",
    "clear-log",
    "clear-selection",
    "clear-sort-arrows",
    "close-log",
    "connect",
    "connected",
    "convert-to-offset",
    "copy-dataset",
    "copy-sax-attributes",
    "copy-temp-table",
    "create-like",
    "create-like-sequential",
    "create-result-list-entry",
    "current-query",
    "debug",
    "declare-namespace",
    "delete",
    "delete-char",
    "delete-current-row",
    "delete-header-entry",
    "delete-line",
    "delete-node",
    "delete-result-list-entry",
    "delete-selected-row",
    "delete-selected-rows",
    "deselect-focused-row",
    "deselect-rows",
    "deselect-selected-row",
    "detach-data-source",
    "disable",
    "disable-connections",
    "disable-dump-triggers",
    "disable-load-triggers",
    "disconnect",
    "display-message",
    "dump-logging-now",
    "edit-clear",
    "edit-copy",
    "edit-cut",
    "edit-paste",
    "edit-undo",
    "empty-dataset",
    "empty-temp-table",
    "enable",
    "enable-connections",
    "enable-events",
    "encode-domain-access-code",
    "encrypt-audit-mac-key",
    "end-document",
    "end-element",
    "end-event-group",
    "end-file-drop",
    "entry",
    "export-principal",
    "fetch-selected-row",
    "fill",
    "find-by-rowid",
    "find-current",
    "find-first",
    "find-last",
    "find-unique",
    "first-of",
    "get-attribute",
    "get-binary-data",
    "get-blue-value",
    "get-browse-column",
    "get-buffer-handle",
    "get-bytes-available",
    "get-callback-proc-context",
    "get-callback-proc-name",
    "get-cgi-list",
    "get-cgi-long-value",
    "get-cgi-value",
    "get-changes",
    "get-child-relation",
    "get-client",
    "get-column",
    "get-config-value",
    "get-current",
    "get-dataset-buffer",
    "get-dropped-file",
    "get-dynamic",
    "get-error-column",
    "get-error-row",
    "get-file-name",
    "get-file-offset",
    "get-first",
    "get-green-value",
    "get-index-by-namespace-name",
    "get-index-by-qname",
    "get-iteration",
    "get-last",
    "get-localname-by-index",
    "get-message",
    "get-message-type",
    "get-next",
    "get-number",
    "get-prev",
    "get-printers",
    "get-property",
    "get-qname-by-index",
    "get-red-value",
    "get-relation",
    "get-repositioned-row",
    "get-rgb-value",
    "get-row",
    "get-safe-user",
    "get-selected-widget",
    "get-serialized",
    "get-signature",
    "get-socket-option",
    "get-source-buffer",
    "get-tab-item",
    "get-text-height-chars",
    "get-text-height-pixels",
    "get-text-width-chars",
    "get-text-width-pixels",
    "get-top-buffer",
    "get-type-by-index",
    "get-type-by-namespace-name",
    "get-type-by-qname",
    "get-uri-by-index",
    "get-value-by-index",
    "get-value-by-namespace-name",
    "get-value-by-qname",
    "get-wait-state",
    "import-principal",
    "increment-exclusive-id",
    "index-information",
    "initialize",
    "initialize-document-type",
    "initiate",
    "insert",
    "insert-attribute",
    "insert-backtab",
    "insert-before",
    "insert-file",
    "insert-row",
    "insert-string",
    "insert-tab",
    "invoke",
    "is-row-selected",
    "is-selected",
    "last-of",
    "list-property-names",
    "load",
    "load-domains",
    "load-icon",
    "load-image",
    "load-image-down",
    "load-image-insensitive",
    "load-image-up",
    "load-mouse-pointer",
    "load-small-icon",
    "lock-registration",
    "log-audit-event",
    "logout",
    "longchar-to-node-value",
    "lookup",
    "mark-new",
    "mark-row-state",
    "memptr-to-node-value",
    "move-after-tab-item",
    "move-before-tab-item",
    "move-column",
    "move-to-bottom",
    "move-to-eof",
    "move-to-top",
    "normalize",
    "query-close",
    "query-open",
    "query-prepare",
    "read-file",
    "read-json",
    "read-xml",
    "read-xmlschema",
    "refresh",
    "refresh-audit-policy",
    "register-domain",
    "reject-changes",
    "reject-row-changes",
    "remove-attribute",
    "remove-child",
    "remove-events-procedure",
    "remove-super-procedure",
    "replace",
    "replace-child",
    "replace-selection-text",
    "reposition-backward",
    "reposition-forward",
    "reposition-to-row",
    "reposition-to-rowid",
    "reset",
    "save-file",
    "save-row-changes",
    "sax-parse",
    "sax-parse-first",
    "sax-parse-next",
    "scroll-to-current-row",
    "scroll-to-item",
    "scroll-to-selected-row",
    "seal",
    "search",
    "select-all",
    "select-focused-row",
    "select-next-row",
    "select-prev-row",
    "select-row",
    "set-actor",
    "set-appl-context",
    "set-attribute",
    "set-attribute-node",
    "set-blue-value",
    "set-break",
    "set-buffers",
    "set-callback",
    "set-callback-procedure",
    "set-client",
    "set-commit",
    "set-connect-procedure",
    "set-dynamic",
    "set-green-value",
    "set-input-source",
    "set-must-understand",
    "set-node",
    "set-numeric-format",
    "set-property",
    "set-read-response-procedure",
    "set-red-value",
    "set-repositioned-row",
    "set-rgb-value",
    "set-role",
    "set-rollback",
    "set-safe-user",
    "set-selection",
    "set-serialized",
    "set-socket-option",
    "set-sort-arrow",
    "set-wait-state",
    "start-document",
    "start-element",
    "stop-parsing",
    "synchronize",
    "temp-table-prepare",
    "tenant-id",
    "tenant-name",
    "update-attribute",
    "url-decode",
    "url-encode",
    "validate",
    "validate-domain-access-code",
    "validate-seal",
    "write",
    "write-cdata",
    "write-characters",
    "write-comment",
    "write-data",
    "write-data-element",
    "write-empty-element",
    "write-entity-ref",
    "write-external-dtd",
    "write-fragment",
    "write-message",
    "write-processing-instruction",
];

/// The name of a receiver that is a plain name, not `BUFFER b`.
fn receiver_name(object: &Expression) -> Option<&str> {
    match &object.kind {
        E::Identifier(id) if object.span.start >= id.span.start => Some(&id.name),
        _ => None,
    }
}

/// Every operand of `e`: each child expression whose value or calls it may
/// read. The parts of a name (`t.f`, `db.t.f`) are not operands. Each kind is
/// listed, so a new kind does not build until its operands are.
pub(super) fn operands(e: &Expression) -> Vec<&Expression> {
    match &e.kind {
        E::Literal(_)
        | E::Identifier(_)
        | E::IncludeReference { .. }
        | E::IncludeArgReference { .. }
        | E::PreprocReference(_) => Vec::new(),
        E::FieldAccess { qualifier, .. } => match field_parts(e) {
            Some(_) => Vec::new(),
            None => vec![&**qualifier],
        },
        E::Add(a, b)
        | E::Minus(a, b)
        | E::Multiply(a, b)
        | E::Divide(a, b)
        | E::Modulo(a, b)
        | E::Equal(a, b)
        | E::NotEqual(a, b)
        | E::LessThan(a, b)
        | E::LessThanOrEqual(a, b)
        | E::GreaterThan(a, b)
        | E::GreaterThanOrEqual(a, b)
        | E::Begins(a, b)
        | E::Matches(a, b)
        | E::Contains(a, b)
        | E::And(a, b)
        | E::Or(a, b)
        | E::ArrayAccess { array: a, index: b } => vec![&**a, &**b],
        E::Negate(a) | E::Not(a) | E::MemberAccess { object: a, .. } => vec![&**a],
        E::IfThenElse(c, a, b) => vec![&**c, &**a, &**b],
        E::FunctionCall { arguments, .. } | E::New { arguments, .. } => arguments.iter().collect(),
        E::MethodCall {
            object, arguments, ..
        } => std::iter::once(&**object).chain(arguments).collect(),
        E::CanFind { where_clause, .. } => where_clause.iter().map(|w| &**w).collect(),
        // The conditions are the preprocessor's; the branches are code.
        E::PreprocIf(p) => std::iter::once(&p.then_branch)
            .chain(p.elseif_branches.iter().map(|(_, b)| b))
            .chain(&p.else_branch)
            .collect(),
    }
}

/// What an expression passes on of the value of one operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pass {
    Value,
    /// Only its identity: the operand decides a logical.
    Identity,
    /// Nothing; its calls run.
    Nothing,
}

/// What `e` passes on of its operand at `i`: a comparison, a logical operator,
/// `CAN-FIND` and the condition of an `IF` expression only the identity, and a
/// subscript not its index.
fn passes_on(e: &Expression, i: usize) -> Pass {
    match &e.kind {
        E::Equal(..)
        | E::NotEqual(..)
        | E::LessThan(..)
        | E::LessThanOrEqual(..)
        | E::GreaterThan(..)
        | E::GreaterThanOrEqual(..)
        | E::Begins(..)
        | E::Matches(..)
        | E::Contains(..)
        | E::And(..)
        | E::Or(..)
        | E::Not(..)
        | E::CanFind { .. } => Pass::Identity,
        E::IfThenElse(..) if i == 0 => Pass::Identity,
        E::ArrayAccess { .. } if i > 0 => Pass::Nothing,
        _ => Pass::Value,
    }
}

/// Whether evaluating `e` may run code: it or an operand at any depth is a
/// call, a member access or a `NEW`.
fn may_run(e: &Expression) -> bool {
    matches!(
        e.kind,
        E::FunctionCall { .. } | E::MethodCall { .. } | E::MemberAccess { .. } | E::New { .. }
    ) || operands(e).into_iter().any(may_run)
}

/// The text of a first argument that is a string literal.
fn literal_key(arguments: &[Expression]) -> Option<&str> {
    match &arguments.first()?.kind {
        E::Literal(oxabl_ast::Literal::String(s)) => Some(s.value.as_str()),
        _ => None,
    }
}

/// The text of the tokens `range` when they are one string literal.
fn token_key<'s>(st: &'s StatementText, range: Range<usize>) -> Option<&'s str> {
    if range.len() != 1 || st.token(range.start).kind != Kind::StringLiteral {
        return None;
    }
    let w = st.word(range.start);
    let body = match w.rfind(['"', '\'']) {
        Some(i) => &w[..=i],
        None => w,
    };
    Some(body.trim_matches(|c| c == '"' || c == '\''))
}

/// The table and field of `t.f` or `db.t.f`.
fn field_parts(e: &Expression) -> Option<(&str, &str)> {
    let E::FieldAccess { qualifier, field } = &e.kind else {
        return None;
    };
    match &qualifier.kind {
        E::Identifier(q) => Some((&q.name, &field.name)),
        E::FieldAccess { field: table, .. } => Some((&table.name, &field.name)),
        _ => None,
    }
}

fn is_word(st: &StatementText, i: usize) -> bool {
    i < st.len()
        && st.token(i).kind != Kind::StringLiteral
        && st
            .word(i)
            .bytes()
            .next()
            .is_some_and(|b| b.is_ascii_alphabetic() || b == b'_')
}

fn is_operator_word(word: &str) -> bool {
    [
        "AND",
        "OR",
        "NOT",
        "EQ",
        "NE",
        "LT",
        "GT",
        "LE",
        "GE",
        "BEGINS",
        "MATCHES",
        "CONTAINS",
        "THEN",
        "ELSE",
        "MODULO",
        "SKIP",
        "SPACE",
        "UNFORMATTED",
        "FORMATTED",
    ]
    .iter()
    .any(|w| w.eq_ignore_ascii_case(word))
}

/// Whether the word at `i` is followed by `.` and a word, with no space.
fn dotted(st: &StatementText, i: usize) -> bool {
    is_word(st, i)
        && st.is(i + 1, Kind::Period)
        && adjacent(st, i)
        && adjacent(st, i + 1)
        && is_word(st, i + 2)
}

/// Whether the token at `i` ends where the next one starts.
fn adjacent(st: &StatementText, i: usize) -> bool {
    i + 1 < st.len() && st.token(i).end == st.token(i + 1).start
}

fn is_zero(e: &Expression) -> bool {
    matches!(&e.kind, E::Literal(oxabl_ast::Literal::Integer(i)) if i.value == 0)
}

fn is_one(e: &Expression) -> bool {
    matches!(&e.kind, E::Literal(oxabl_ast::Literal::Integer(i)) if i.value == 1)
}

/// A literal `CAN-DO` list with no pattern characters.
fn is_plain_list(e: &Expression) -> bool {
    matches!(&e.kind, E::Literal(oxabl_ast::Literal::String(s))
        if !s.value.contains(['*', '!', '.']))
}

#[cfg(test)]
mod tests {
    use oxabl_ast::{
        ExpressionKind, FindType, Identifier, IntegerLiteral, Literal, LockType, PreprocIf,
    };
    use oxabl_common::SecurityOptions;

    use super::*;

    /// How many kinds [`ordinal`] numbers.
    const KINDS: usize = 32;

    /// A number for each kind. The match lists every kind, so a new kind does
    /// not build until it has a number, and the test below fails until it
    /// has a sample.
    fn ordinal(k: &ExpressionKind) -> usize {
        match k {
            E::Literal(_) => 0,
            E::Identifier(_) => 1,
            E::Add(..) => 2,
            E::Minus(..) => 3,
            E::Multiply(..) => 4,
            E::Divide(..) => 5,
            E::Modulo(..) => 6,
            E::Equal(..) => 7,
            E::NotEqual(..) => 8,
            E::LessThan(..) => 9,
            E::LessThanOrEqual(..) => 10,
            E::GreaterThan(..) => 11,
            E::GreaterThanOrEqual(..) => 12,
            E::Begins(..) => 13,
            E::Matches(..) => 14,
            E::Contains(..) => 15,
            E::And(..) => 16,
            E::Or(..) => 17,
            E::Negate(_) => 18,
            E::Not(_) => 19,
            E::IfThenElse(..) => 20,
            E::FunctionCall { .. } => 21,
            E::MemberAccess { .. } => 22,
            E::MethodCall { .. } => 23,
            E::ArrayAccess { .. } => 24,
            E::FieldAccess { .. } => 25,
            E::IncludeReference { .. } => 26,
            E::IncludeArgReference { .. } => 27,
            E::PreprocReference(_) => 28,
            E::PreprocIf(_) => 29,
            E::New { .. } => 30,
            E::CanFind { .. } => 31,
        }
    }

    /// Whether the compiler models the kind; any other kind is Unknown.
    fn modelled(k: &ExpressionKind) -> bool {
        !matches!(
            k,
            E::IncludeReference { .. }
                | E::IncludeArgReference { .. }
                | E::PreprocReference(_)
                | E::PreprocIf(_)
        )
    }

    fn name(n: &str) -> Identifier {
        Identifier {
            span: Span::DUMMY,
            name: n.to_string(),
        }
    }

    /// A call of `f`, a function of the file.
    fn call() -> Expression {
        Expression::new(E::FunctionCall {
            name: name("f"),
            arguments: Vec::new(),
        })
    }

    type Binary = fn(Box<Expression>, Box<Expression>) -> ExpressionKind;

    fn b(e: Expression) -> Box<Expression> {
        Box::new(e)
    }

    /// One expression of each kind, with a call of `f` in every operand, and
    /// how many of those calls run.
    fn samples() -> Vec<(ExpressionKind, usize)> {
        let c = call;
        let binary: [Binary; 16] = [
            E::Add,
            E::Minus,
            E::Multiply,
            E::Divide,
            E::Modulo,
            E::Equal,
            E::NotEqual,
            E::LessThan,
            E::LessThanOrEqual,
            E::GreaterThan,
            E::GreaterThanOrEqual,
            E::Begins,
            E::Matches,
            E::Contains,
            E::And,
            E::Or,
        ];
        let mut out: Vec<(ExpressionKind, usize)> =
            binary.iter().map(|k| (k(b(c()), b(c())), 2)).collect();
        out.extend([
            (
                E::Literal(Literal::Integer(IntegerLiteral {
                    span: Span::DUMMY,
                    value: 1,
                })),
                0,
            ),
            (E::Identifier(name("x")), 0),
            (E::Negate(b(c())), 1),
            (E::Not(b(c())), 1),
            (E::IfThenElse(b(c()), b(c()), b(c())), 3),
            (
                E::FunctionCall {
                    name: name("g"),
                    arguments: vec![c(), c()],
                },
                2,
            ),
            (
                E::MemberAccess {
                    object: b(c()),
                    member: name("m"),
                },
                1,
            ),
            (
                E::MethodCall {
                    object: b(c()),
                    method: name("m"),
                    arguments: vec![c(), c()],
                },
                3,
            ),
            (
                E::ArrayAccess {
                    array: b(c()),
                    index: b(c()),
                },
                2,
            ),
            (
                E::FieldAccess {
                    qualifier: b(Expression::new(E::Identifier(name("t")))),
                    field: name("k"),
                },
                0,
            ),
            (
                E::IncludeReference {
                    path_and_args: "x.i".to_string(),
                    span: Span::DUMMY,
                },
                0,
            ),
            (
                E::IncludeArgReference {
                    index: 1,
                    span: Span::DUMMY,
                },
                0,
            ),
            (E::PreprocReference("x".to_string()), 0),
            // The conditions are the preprocessor's: only the branches run.
            (
                E::PreprocIf(Box::new(PreprocIf {
                    condition: c(),
                    then_branch: c(),
                    elseif_branches: vec![(c(), c())],
                    else_branch: Some(c()),
                })),
                3,
            ),
            (
                E::New {
                    class_name: "C".to_string(),
                    type_arguments: None,
                    arguments: vec![c(), c()],
                },
                2,
            ),
            (
                E::CanFind {
                    find_type: FindType::First,
                    buffer: name("t"),
                    where_clause: Some(b(c())),
                    lock_type: LockType::NoLock,
                    no_error: false,
                },
                1,
            ),
        ]);
        out
    }

    fn has_unknown(v: &Value) -> bool {
        v.0.iter().any(|t| match t {
            Term::Unknown => true,
            Term::Clear(_, v) | Term::Tag(_, v) => has_unknown(v),
            _ => false,
        })
    }

    #[test]
    fn every_kind_runs_the_calls_in_its_operands_and_is_modelled_or_unknown() {
        let options = super::super::Options::new(&SecurityOptions::default());
        let tables = Tables::default();
        let mut routines = Routines::default();
        routines.functions.insert("f".to_string(), vec![0]);
        let declared = FxHashSet::default();
        let samples = samples();
        let mut seen = [false; KINDS];
        for (kind, runs) in samples {
            seen[ordinal(&kind)] = true;
            let mut slots = Slots::default();
            let mut origins = Origins::default();
            let mut c = Compiler {
                options: &options,
                tables: &tables,
                slots: &mut slots,
                origins: &mut origins,
                declared: &declared,
                frozen: false,
                routines: &routines,
                calls: Vec::new(),
                foreign: Vec::new(),
                decision: false,
            };
            let modelled = modelled(&kind);
            let value = c.expr(&Expression::new(kind.clone()));
            assert_eq!(c.calls.len(), runs, "calls that run in {kind:?}");
            assert_eq!(!has_unknown(&value), modelled, "value of {kind:?}");
        }
        let missing: Vec<usize> = (0..seen.len()).filter(|&i| !seen[i]).collect();
        assert!(missing.is_empty(), "kinds with no sample: {missing:?}");
    }

    fn holds_unknown(v: &Value) -> bool {
        v.0.iter().any(|t| matches!(t, Term::Origin(UNKNOWN)))
    }

    /// Every built-in function, and every one a program calls without
    /// parentheses, is on a list of [`decision`]: a new one in the registry
    /// fails here until it has a decision. Until then it is Unknown.
    #[test]
    fn every_builtin_function_has_a_decision() {
        let undecided: Vec<&str> = oxabl_lexer::BUILTIN_FUNCTIONS
            .iter()
            .chain(&BARE_FUNCTIONS)
            .copied()
            .filter(|f| decision(f).is_none())
            .collect();
        assert!(
            undecided.is_empty(),
            "built-in functions with no decision: {undecided:?}"
        );
        assert_eq!(builtin("no-such-builtin"), Builtin::Other);
        for f in UNKNOWN_FUNCTIONS {
            assert_eq!(builtin(f), Builtin::Unknown, "{f}");
        }
    }

    /// Every system handle is a handle of the program, of a foreign object, or
    /// an object of a class: a new one fails here until it is on a list, and
    /// is foreign until then.
    #[test]
    fn every_system_handle_has_an_owner() {
        let options = super::super::Options::new(&SecurityOptions::default());
        let tables = Tables::default();
        let routines = Routines::default();
        let declared = FxHashSet::default();
        let mut slots = Slots::default();
        let mut origins = Origins::default();
        let c = Compiler {
            options: &options,
            tables: &tables,
            slots: &mut slots,
            origins: &mut origins,
            declared: &declared,
            frozen: false,
            routines: &routines,
            calls: Vec::new(),
            foreign: Vec::new(),
            decision: false,
        };
        let classes = ["this-object", "super"];
        for h in oxabl_semantic::SYSTEM_HANDLES {
            let lists = [
                PROGRAM_HANDLES.contains(h),
                FOREIGN_HANDLES.contains(h),
                classes.contains(h),
            ];
            assert_eq!(lists.iter().filter(|l| **l).count(), 1, "{h}: {lists:?}");
            let expected = match lists {
                [true, ..] => Owner::Program,
                [_, true, _] => Owner::Foreign,
                _ => Owner::Class,
            };
            assert_eq!(c.name_owner(h, "NAME"), expected, "{h}");
        }
        for h in PROGRAM_HANDLES.iter().chain(&FOREIGN_HANDLES) {
            assert!(
                oxabl_semantic::SYSTEM_HANDLES.contains(h),
                "{h} is no system handle"
            );
        }
    }

    /// The attributes and methods of the ABL reference, by name.
    fn members() -> Vec<String> {
        let index = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../resources/abl_keyword_index.json"),
        )
        .unwrap();
        let mut out: Vec<String> = index
            .lines()
            .filter_map(|l| l.trim().strip_prefix("\"title\": \""))
            .filter_map(|t| {
                let t = t.trim_end_matches(['"', ',']);
                t.strip_suffix(" attribute")
                    .or_else(|| t.strip_suffix(" method"))
            })
            .map(|t| t.split('(').next().unwrap_or(t).trim().to_ascii_lowercase())
            .collect();
        out.sort();
        out.dedup();
        out
    }

    /// Every built-in attribute and method read on a handle is modelled when
    /// a list names it, and Unknown otherwise; on a handle of a foreign object
    /// only one that holds no input is modelled. A name on a list that is no
    /// attribute or method fails.
    #[test]
    fn every_builtin_member_read_is_modelled_or_unknown() {
        let members = members();
        assert!(members.len() > 1000, "{}", members.len());
        let read =
            |owner: Owner, m: &str| !holds_unknown(&owner.read(m, Value(vec![Term::Slot(0)])));
        for m in &members {
            let no_input = NO_INPUT_MEMBERS.contains(&m.as_str());
            let modelled = no_input || MODELLED_MEMBERS.contains(&m.as_str());
            assert_eq!(
                read(Owner::Program, m),
                modelled,
                "{m} on a handle of the program"
            );
            assert_eq!(read(Owner::Foreign, m), no_input, "{m} on a foreign handle");
            assert!(read(Owner::Class, m), "{m} of an object of a class");
        }
        for m in NO_INPUT_MEMBERS.iter().chain(&MODELLED_MEMBERS) {
            assert!(
                members.iter().any(|x| x == m),
                "{m} is no attribute or method"
            );
        }
        for m in [
            "buffer-value",
            "screen-value",
            "node-value",
            "get-message",
            "input-value",
        ] {
            assert!(!read(Owner::Program, m), "{m}");
        }
    }

    /// The built-in methods of a handle in the ABL reference. Their names are
    /// upper case; the methods of a class (`GetCharacter`) are not.
    fn handle_methods() -> Vec<String> {
        let index = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../resources/abl_keyword_index.json"),
        )
        .unwrap();
        let mut out: Vec<String> = index
            .lines()
            .filter_map(|l| l.trim().strip_prefix("\"title\": \""))
            .filter_map(|t| {
                let t = t.trim_end_matches(['"', ',']);
                // `CLEAR( ) method (Call Object Handle)`.
                t.split_once(" method (")
                    .map(|(m, _)| m)
                    .or_else(|| t.strip_suffix(" method"))
            })
            .map(|t| t.split('(').next().unwrap_or(t).trim().to_string())
            .filter(|t| {
                !t.is_empty() && !t.contains(' ') && !t.chars().any(|c| c.is_ascii_lowercase())
            })
            .map(|t| t.to_ascii_lowercase())
            .collect();
        out.sort();
        out.dedup();
        out
    }

    /// Every built-in method of a handle has one decision for what it writes
    /// into an argument that is a variable: what the receiver holds, Unknown,
    /// or nothing. A new one fails here until it is on a list, and writes
    /// Unknown until then. A name on a list that is no such method fails.
    #[test]
    fn every_builtin_method_has_an_argument_decision() {
        let methods = handle_methods();
        assert!(methods.len() > 300, "{}", methods.len());
        let lists: [&[&str]; 3] = [&CARRYING_METHODS, &UNKNOWN_WRITE_METHODS, &INPUT_METHODS];
        let undecided: Vec<&String> = methods
            .iter()
            .filter(|m| lists.iter().filter(|l| l.contains(&m.as_str())).count() != 1)
            .collect();
        assert!(
            undecided.is_empty(),
            "built-in methods with no single argument decision: {undecided:?}"
        );
        for m in lists.iter().flat_map(|l| l.iter()) {
            assert!(
                methods.iter().any(|x| x == m),
                "{m} is no method of a handle"
            );
        }

        let options = super::super::Options::new(&SecurityOptions::default());
        let mut tables = Tables::default();
        tables.types.insert("h".to_string(), String::new());
        let routines = Routines::default();
        let declared: FxHashSet<String> = ["h", "x"].iter().map(|n| n.to_string()).collect();
        let mut slots = Slots::default();
        let mut origins = Origins::default();
        let mut c = Compiler {
            options: &options,
            tables: &tables,
            slots: &mut slots,
            origins: &mut origins,
            declared: &declared,
            frozen: false,
            routines: &routines,
            calls: Vec::new(),
            foreign: Vec::new(),
            decision: false,
        };
        let receiver = Expression::new(E::Identifier(name("h")));
        let argument = [Expression::new(E::Identifier(name("x")))];
        for m in methods.iter().map(String::as_str).chain(["no-such-method"]) {
            let writes = c.method_writes(&receiver, m, &argument);
            match argument_write(m) {
                ArgWrite::Input => assert!(writes.is_empty(), "{m} writes"),
                ArgWrite::Carry => {
                    assert_eq!(writes.len(), 1, "{m}");
                    assert!(!holds_unknown(&writes[0].1), "{m} writes Unknown");
                }
                ArgWrite::Unknown => {
                    assert_eq!(writes.len(), 1, "{m}");
                    assert!(holds_unknown(&writes[0].1), "{m} writes no Unknown");
                }
            }
        }
        assert_eq!(argument_write("no-such-method"), ArgWrite::Unknown);
        for m in UNKNOWN_WRITE_METHODS {
            assert_eq!(argument_write(m), ArgWrite::Unknown, "{m}");
        }
    }
}
