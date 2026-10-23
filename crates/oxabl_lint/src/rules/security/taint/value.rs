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

/// An untrusted source site.
#[derive(Debug, Clone)]
pub(super) struct Origin {
    pub span: Span,
    /// A noun phrase: "input parameter `pc`".
    pub what: String,
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
        self.list.push(Origin { span, what });
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
    /// Text the analysis cannot read, such as an unexpanded reference.
    Unknown,
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

    /// The labels of the value against `fact` and the file-level `files`,
    /// and whether it holds Unknown.
    pub fn eval(&self, fact: &Provenance, files: &Summaries) -> (Labels, bool) {
        let mut out = Labels::new();
        let unknown = self.eval_into(fact, files, &mut out);
        (out, unknown)
    }

    fn eval_into(&self, fact: &Provenance, files: &Summaries, out: &mut Labels) -> bool {
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
                }
                Term::Clear(mask, v) => {
                    let mut inner = Labels::new();
                    unknown |= v.eval_into(fact, files, &mut inner);
                    provenance::clear(&mut inner, *mask);
                    provenance::merge(out, &inner);
                }
                Term::Unknown => unknown = true,
            }
        }
        unknown
    }

    /// Whether the value names `origin` itself, not through a slot.
    pub fn holds_origin(&self, origin: u32) -> bool {
        self.0.iter().any(|t| match t {
            Term::Origin(o) => *o == origin,
            Term::Clear(_, v) => v.holds_origin(origin),
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
            Term::Clear(_, v) => v.file_with(files, origin),
            _ => None,
        })
    }

    /// The slots the value reads, outermost first.
    pub fn slots(&self, out: &mut Vec<u32>) {
        for t in &self.0 {
            match t {
                Term::Slot(s) => out.push(*s),
                Term::Clear(_, v) => v.slots(out),
                _ => {}
            }
        }
    }
}

/// A variable, a field or a whole table that a value can sit in.
#[derive(Debug, Clone)]
pub(super) struct SlotInfo {
    /// As first written: `cWhere`, `ttFilter.cWhere`.
    pub name: String,
    pub key: String,
    /// Not declared in the unit: its value at the entry comes from the file.
    pub global: bool,
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
}

impl Tables {
    /// The table a buffer or table name stands for, lower-cased.
    pub fn table_of(&self, name: &str) -> String {
        let lower = name.to_ascii_lowercase();
        self.buffers.get(&lower).cloned().unwrap_or(lower)
    }
}

/// File-level summaries: the labels every unit writes to a variable or field
/// that other units see, and one place each was written.
#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct Summaries {
    pub labels: FxHashMap<String, Labels>,
    pub sites: FxHashMap<String, Span>,
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
}

/// How a function name changes the value of its arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Builtin {
    Source,
    Quoter,
    Numeric,
    Logical,
    Encode,
    Other,
}

/// Whether `name` is `full` or an abbreviation of it at least `min` long.
fn abbrev(name: &str, full: &str, min: usize) -> bool {
    name.len() >= min && name.len() <= full.len() && full[..name.len()].eq_ignore_ascii_case(name)
}

fn builtin(name: &str) -> Builtin {
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
        ("LOGICAL", 7),
        ("AVAILABLE", 5),
        ("VALID-HANDLE", 12),
        ("VALID-OBJECT", 12),
        ("CAN-DO", 6),
        ("CAN-QUERY", 9),
        ("CAN-SET", 7),
        ("LOCKED", 6),
        ("AMBIGUOUS", 5),
    ];
    if SOURCES.iter().any(|s| s.eq_ignore_ascii_case(name)) {
        Builtin::Source
    } else if name.eq_ignore_ascii_case("QUOTER") {
        Builtin::Quoter
    } else if name.eq_ignore_ascii_case("HTML-ENCODE") || name.eq_ignore_ascii_case("URL-ENCODE") {
        Builtin::Encode
    } else if NUMERIC.iter().any(|(f, m)| abbrev(name, f, *m)) {
        Builtin::Numeric
    } else if LOGICAL.iter().any(|(f, m)| abbrev(name, f, *m)) {
        Builtin::Logical
    } else {
        Builtin::Other
    }
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
    fn slot(&mut self, written: &str, key: String, global: bool) -> Option<u32> {
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
        });
        self.slots.index.insert(key, id);
        Some(id)
    }

    /// The slot of a variable named `name`.
    pub fn var_slot(&mut self, name: &str) -> Option<u32> {
        let key = name.to_ascii_lowercase();
        let global = !self.declared.contains(&key);
        self.slot(name, key, global)
    }

    /// The slot of field `field` of the table or buffer `qualifier`.
    pub fn field_slot(&mut self, qualifier: &str, field: &str) -> Option<u32> {
        let key = format!(
            "{}.{}",
            self.tables.table_of(qualifier),
            field.to_ascii_lowercase()
        );
        self.slot(&format!("{qualifier}.{field}"), key, true)
    }

    /// The slot that holds taint for every field of a table.
    pub fn table_slot(&mut self, table: &str) -> Option<u32> {
        let key = format!("{}.*", self.tables.table_of(table));
        self.slot(table, key, true)
    }

    /// A read of the variable `name`.
    fn var(&mut self, name: &str) -> Value {
        if let Some(s) = self.var_slot(name) {
            return Value(vec![Term::Slot(s)]);
        }
        self.summary(name, &name.to_ascii_lowercase())
    }

    /// After the solve, a name the unit never used: its file-level summary.
    fn summary(&self, name: &str, key: &str) -> Value {
        if self.frozen && !self.declared.contains(key) {
            Value(vec![Term::File {
                name: name.to_string(),
                key: key.to_string(),
            }])
        } else {
            Value::default()
        }
    }

    /// A read of `qualifier.field`. A database field is a source when the
    /// options say so; `receiver` reads the name of a class, not data.
    fn field(&mut self, qualifier: &str, field: &str, span: Span, receiver: bool) -> Value {
        let table = self.tables.table_of(qualifier);
        let mut v = Value::default();
        if let Some(s) = self.field_slot(qualifier, field) {
            v.0.push(Term::Slot(s));
        } else {
            let key = format!("{table}.{}", field.to_ascii_lowercase());
            v = Value::union([v, self.summary(&format!("{qualifier}.{field}"), &key)]);
        }
        if let Some(s) = self.table_slot(qualifier) {
            v.0.push(Term::Slot(s));
        } else {
            v = Value::union([v, self.summary(qualifier, &format!("{table}.*"))]);
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

    /// A call of the function `name`.
    fn call(&mut self, name: &str, args: Vec<Value>, span: Span) -> Value {
        if let Some(mask) = self.options.validator(name) {
            return Value::union(args).cleared(mask);
        }
        if self.options.is_source(name) {
            return self.origin(format!("project source `{name}`"), span);
        }
        match builtin(name) {
            Builtin::Source if name.eq_ignore_ascii_case("OS-GETENV") => {
                self.origin(format!("environment `{name}`"), span)
            }
            Builtin::Source => self.origin(format!("web input `{name}`"), span),
            Builtin::Quoter => args
                .into_iter()
                .next()
                .unwrap_or_default()
                .cleared(SinkKind::Query.bit()),
            Builtin::Numeric => Value::union(args).cleared(NUMERIC_SAFE),
            Builtin::Logical => Value::default(),
            Builtin::Encode => {
                Value::union(args).cleared(SinkKind::Html.bit() | SinkKind::Header.bit())
            }
            Builtin::Other => Value::union(args),
        }
    }

    /// `object:member`, or `object:member(args)` when `args` is given.
    /// `object_name` is the receiver when it is a plain name.
    fn access(
        &mut self,
        object_name: Option<&str>,
        object: Value,
        member: &str,
        args: Option<Vec<Value>>,
        span: Span,
    ) -> Value {
        if let Some(o) = object_name {
            if o.eq_ignore_ascii_case("WEB-CONTEXT") {
                return self.origin(format!("`WEB-CONTEXT:{member}`"), span);
            }
            if o.eq_ignore_ascii_case("SESSION")
                && ["PARAMETER", "STARTUP-PARAMETERS", "ICFPARAMETER"]
                    .iter()
                    .any(|m| m.eq_ignore_ascii_case(member))
            {
                return self.origin(format!("`SESSION:{member}`"), span);
            }
            if o.eq_ignore_ascii_case("THIS-OBJECT") && args.is_none() {
                return self.var(member);
            }
            if is_request(o) {
                return self.origin(format!("web request `{o}:{member}`"), span);
            }
        }
        let Some(args) = args else {
            return object;
        };
        if let Some(mask) = self.options.validator(member) {
            return Value::union(args).cleared(mask);
        }
        if self.options.is_source(member) {
            return self.origin(format!("project source `{member}`"), span);
        }
        Value::union(std::iter::once(object).chain(args))
    }

    /// The value of an expression.
    pub fn expr(&mut self, e: &Expression) -> Value {
        match &e.kind {
            E::Literal(_) => Value::default(),
            E::Identifier(id) => self.var(&id.name),
            E::FieldAccess { qualifier, field } => match &qualifier.kind {
                E::Identifier(q) => self.field(&q.name, &field.name, e.span, false),
                _ => Value::default(),
            },
            E::Add(a, b) => Value::union([self.expr(a), self.expr(b)]),
            E::Minus(a, b) | E::Multiply(a, b) | E::Divide(a, b) | E::Modulo(a, b) => {
                Value::union([self.expr(a), self.expr(b)]).cleared(NUMERIC_SAFE)
            }
            E::Negate(a) => self.expr(a).cleared(NUMERIC_SAFE),
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
            | E::CanFind { .. } => Value::default(),
            E::IfThenElse(_, a, b) => Value::union([self.expr(a), self.expr(b)]),
            E::FunctionCall { name, arguments } => {
                let args = arguments.iter().map(|a| self.expr(a)).collect();
                self.call(&name.name, args, e.span)
            }
            E::MemberAccess { object, member } => {
                let (name, value) = self.receiver(object);
                self.access(name.as_deref(), value, &member.name, None, e.span)
            }
            E::MethodCall {
                object,
                method,
                arguments,
            } => {
                let (name, value) = self.receiver(object);
                let args = arguments.iter().map(|a| self.expr(a)).collect();
                self.access(name.as_deref(), value, &method.name, Some(args), e.span)
            }
            E::ArrayAccess { array, .. } => self.expr(array),
            E::New { arguments, .. } => {
                Value::union(arguments.iter().map(|a| self.expr(a)).collect::<Vec<_>>())
            }
            E::IncludeReference { .. }
            | E::IncludeArgReference { .. }
            | E::PreprocReference(_)
            | E::PreprocIf(_) => Value(vec![Term::Unknown]),
        }
    }

    /// The name and value of the receiver of a member access.
    fn receiver(&mut self, object: &Expression) -> (Option<String>, Value) {
        match &object.kind {
            E::Identifier(id) => (Some(id.name.clone()), self.var(&id.name)),
            // A dotted name before `:` is a class (`Progress.Lang.Class`) or a
            // buffer field holding an object.
            E::FieldAccess { qualifier, field } => match &qualifier.kind {
                E::Identifier(q) => (None, self.field(&q.name, &field.name, object.span, true)),
                _ => (None, Value::default()),
            },
            _ => (None, self.expr(object)),
        }
    }

    /// The slot an assignment to `target` writes, and whether it is a strong
    /// update.
    pub fn target(&mut self, target: &Expression) -> Vec<(u32, bool)> {
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
            E::FieldAccess { qualifier, field } => match &qualifier.kind {
                E::Identifier(q) => self
                    .field_slot(&q.name, &field.name)
                    .map(|s| (s, false))
                    .into_iter()
                    .collect(),
                _ => Vec::new(),
            },
            E::MemberAccess { object, member } => match &object.kind {
                E::Identifier(o) if o.name.eq_ignore_ascii_case("THIS-OBJECT") => self
                    .var_slot(&member.name)
                    .map(|s| (s, true))
                    .into_iter()
                    .collect(),
                // The object now holds the value.
                E::Identifier(o) => self
                    .var_slot(&o.name)
                    .map(|s| (s, false))
                    .into_iter()
                    .collect(),
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
            E::FieldAccess { qualifier, field } => match &qualifier.kind {
                E::Identifier(q) => self.field_slot(&q.name, &field.name),
                _ => None,
            },
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
            // The condition of `IF c THEN a ELSE b` picks a value; it is not one.
            if st.is_word(i, "IF")
                && let Some(then) = (i + 1..end).find(|&j| st.is_word(j, "THEN"))
            {
                i = then + 1;
                continue;
            }
            let span = st.span(i..i + 1);
            if st.is(i + 1, Kind::LeftParen)
                && let Some(close) = st.close_of(i + 1).filter(|&c| c < end)
            {
                let args = st
                    .call_args(i + 1)
                    .into_iter()
                    .map(|a| self.tokens(st, a))
                    .collect();
                v = Value::union([v, self.call(word, args, st.span(i..close + 1))]);
                i = close + 1;
                continue;
            }
            if st.is(i + 1, Kind::Colon) && is_word(st, i + 2) {
                let (value, next) = self.token_chain(st, i, end);
                v = Value::union([v, value]);
                i = next;
                continue;
            }
            if st.is(i + 1, Kind::Period)
                && adjacent(st, i)
                && adjacent(st, i + 1)
                && is_word(st, i + 2)
            {
                let field = self.field(word, st.word(i + 2), st.span(i..i + 3), false);
                v = Value::union([v, field]);
                i += 3;
                continue;
            }
            let _ = span;
            v = Value::union([v, self.var(word)]);
            i += 1;
        }
        v
    }

    /// `a:b:c(args)...` starting at `i`; the value and the index after it.
    fn token_chain(&mut self, st: &StatementText, i: usize, end: usize) -> (Value, usize) {
        let first = st.word(i);
        let mut name = Some(first.to_string());
        let mut value = self.var(first);
        let mut j = i + 1;
        while j + 1 < end && st.is(j, Kind::Colon) && is_word(st, j + 1) {
            let member = st.word(j + 1);
            let mut args = None;
            let mut next = j + 2;
            if st.is(j + 2, Kind::LeftParen)
                && let Some(close) = st.close_of(j + 2).filter(|&c| c < end)
            {
                args = Some(
                    st.call_args(j + 2)
                        .into_iter()
                        .map(|a| self.tokens(st, a))
                        .collect(),
                );
                next = close + 1;
            }
            let span = st.span(i..next);
            value = self.access(name.as_deref(), value, member, args, span);
            name = None;
            j = next;
        }
        (value, j)
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
