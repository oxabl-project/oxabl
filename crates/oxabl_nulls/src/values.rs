//! Local unknown-value facts. Low-confidence operations produce Unknown.
use oxabl_ast::{
    DataType, Expression, ExpressionKind as E, Literal, NodeId, OnAction, OnKind,
    ParameterDirection, ParameterType, Span, Statement, StatementKind as S, TypeSource,
};
use oxabl_flow::dataflow::{Analysis, Direction, Limits, Solution, default_budget, solve_bounded};
use oxabl_flow::{Cfg, Edge, EdgeKind, FileFlow, NodeIx, NodeKind, UnitId};
use oxabl_scope::{BufferId, ReadKind, UpdateKind};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::fields::{self, Effects, FieldSources};

#[path = "value_fact.rs"]
mod fact;
use fact::{Fact, Required};

const KNOWN: u8 = 1;
const QUESTION: u8 = 2;
// Keep source evidence separate from paths whose value is not known.
const OPAQUE: u8 = 4;
const UNKNOWN: u8 = OPAQUE;
/// A database field that may hold `?` or a value.
const FIELD: u8 = KNOWN | QUESTION;

/// A variable use with a known possible source of `?`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueUse {
    pub unit: UnitId,
    pub at: Span,
    pub name: String,
    /// An arithmetic result reaches a store or display; otherwise a condition.
    pub propagation: bool,
}

/// The value of a local variable before a flow node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum ValueState {
    /// No feasible path reaches this point.
    Unreachable,
    /// Every modelled value differs from `?`.
    Known,
    /// A modelled source of `?` can reach this point.
    PossiblyUnknown,
    /// The analysis cannot decide. Rules produce no finding.
    Unknown,
}

#[derive(Debug, Clone)]
pub(crate) struct UnitValues {
    pub unit: UnitId,
    names: FxHashMap<String, usize>,
    before: Vec<Vec<u8>>,
    /// The solve ran out of its work budget: every local is Unknown.
    pub exhausted: bool,
}
impl UnitValues {
    pub fn before(&self, node: NodeIx, name: &str) -> Option<ValueState> {
        let index = lookup(&self.names, name)?;
        if self.exhausted {
            return Some(ValueState::Unknown);
        }
        let value = *self.before.get(node.index())?.get(index)?;
        Some(if value == 0 {
            ValueState::Unreachable
        } else if value & QUESTION != 0 {
            ValueState::PossiblyUnknown
        } else if value & OPAQUE != 0 {
            ValueState::Unknown
        } else {
            ValueState::Known
        })
    }
}

#[derive(Clone)]
struct Variable {
    name: String,
    /// Lowercase name of a local; unique key of a field.
    key: String,
    initial: u8,
    constant: Option<i64>,
}
struct Problem<'a> {
    statements: FxHashMap<NodeId, &'a Statement>,
    variables: Vec<Variable>,
    names: FxHashMap<String, usize>,
    elided: &'a [u32],
    /// The loop variable of each `REPEAT var = ... TO ...`, which the AST does
    /// not keep.
    repeat_vars: &'a FxHashMap<NodeId, String>,
    call_writes: Vec<usize>,
    /// Buffers named by `AVAILABLE` in the unit, for correlated tests.
    buffers: FxHashMap<String, usize>,
    /// The header of each counted loop whose constant bounds make its body
    /// run, with the index of its [`Fact::fresh`] atom.
    counted: FxHashMap<NodeIx, usize>,
    /// Comparisons whose left operand the parser read as `NOT a`.
    loosened: &'a FxHashMap<NodeId, Expression>,
    /// The leading variables that some test reads (section 9.9).
    tracked: usize,
    /// The variables are database fields, found by the span of their
    /// reference, not locals found by name (section 9.11).
    fields_only: bool,
    field_spans: FxHashMap<(u32, u32), usize>,
    /// What each statement does to the fields, by statement.
    effects: FxHashMap<NodeId, Effects>,
    /// The values of `ASSIGN` pairs with a `WHEN`, which the AST does not keep.
    when_values: &'a FxHashSet<NodeId>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Builtin {
    Propagates,
    Absorbs,
    Unknown,
}

// The AST gives a function name as text, and most of these names have no
// `Kind`, so they are matched by name, folded on the stack (as in
// `oxabl_handles`).
fn builtin(name: &str) -> Builtin {
    let mut folded = [0u8; 64];
    if name.len() > folded.len() {
        return Builtin::Unknown;
    }
    for (out, byte) in folded.iter_mut().zip(name.bytes()) {
        *out = byte.to_ascii_uppercase();
    }
    let Ok(name) = std::str::from_utf8(&folded[..name.len()]) else {
        return Builtin::Unknown;
    };
    match name {
        "STRING" | "VALID-HANDLE" | "VALID-OBJECT" | "AVAIL" | "AVAILA" | "AVAILAB"
        | "AVAILABL" | "AVAILABLE" | "CAN-FIND" | "LOCKED" => Builtin::Absorbs,
        "ENTRY" | "NUM-ENTRIES" | "LOOKUP" | "SUBSTR" | "SUBSTRI" | "SUBSTRIN" | "SUBSTRING"
        | "LENGTH" | "TRIM" | "CAPS" | "INDEX" | "REPLACE" | "MAX" | "MAXI" | "MAXIM"
        | "MAXIMU" | "MAXIMUM" | "MIN" | "MINI" | "MINIM" | "MINIMU" | "MINIMUM" | "ABS"
        | "ABSO" | "ABSOL" | "ABSOLU" | "ABSOLUT" | "ABSOLUTE" | "ROUND" | "TRUNC" | "TRUNCA"
        | "TRUNCAT" | "TRUNCATE" | "INT" | "INTE" | "INTEG" | "INTEGE" | "INTEGER" | "DEC"
        | "DECI" | "DECIM" | "DECIMA" | "DECIMAL" | "DATE" | "DAY" | "MONTH" | "YEAR" => {
            Builtin::Propagates
        }
        // SUBSTITUTE's absorption remains low confidence.
        _ => Builtin::Unknown,
    }
}

/// Look `name` up in a map of lowercase names without allocating.
fn lookup<V: Copy>(map: &FxHashMap<String, V>, name: &str) -> Option<V> {
    let mut folded = [0u8; 64];
    if name.len() > folded.len() {
        return map.get(&name.to_ascii_lowercase()).copied();
    }
    for (out, byte) in folded.iter_mut().zip(name.bytes()) {
        *out = byte.to_ascii_lowercase();
    }
    map.get(std::str::from_utf8(&folded[..name.len()]).ok()?)
        .copied()
}

fn propagates(name: &str) -> bool {
    builtin(name) == Builtin::Propagates
}

fn is_available(name: &str) -> bool {
    (5..=9).contains(&name.len()) && "AVAILABLE"[..name.len()].eq_ignore_ascii_case(name)
}

/// The buffer `AVAILABLE b` or `AVAILABLE(db.b)` tests.
fn available_buffer(e: &Expression) -> Option<&str> {
    let E::FunctionCall { name, arguments } = &e.kind else {
        return None;
    };
    if !is_available(&name.name) || arguments.len() != 1 {
        return None;
    }
    match &arguments[0].kind {
        E::Identifier(i) => Some(&i.name),
        E::FieldAccess { field, .. } => Some(&field.name),
        _ => None,
    }
}

/// Visit `e` and every subexpression.
fn walk<'a>(e: &'a Expression, visit: &mut impl FnMut(&'a Expression)) {
    visit(e);
    match &e.kind {
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
        | E::ArrayAccess { array: a, index: b } => {
            walk(a, visit);
            walk(b, visit);
        }
        E::Negate(a)
        | E::Not(a)
        | E::MemberAccess { object: a, .. }
        | E::FieldAccess { qualifier: a, .. } => walk(a, visit),
        E::IfThenElse(c, a, b) => {
            walk(c, visit);
            walk(a, visit);
            walk(b, visit);
        }
        E::FunctionCall { arguments, .. } | E::New { arguments, .. } => {
            for a in arguments {
                walk(a, visit);
            }
        }
        E::MethodCall {
            object, arguments, ..
        } => {
            walk(object, visit);
            for a in arguments {
                walk(a, visit);
            }
        }
        _ => {}
    }
}

/// Whether evaluating `e` may run code the model does not see.
fn calls(e: &Expression) -> bool {
    let mut found = false;
    walk(e, &mut |e| {
        found |= match &e.kind {
            E::FunctionCall { name, .. } => builtin(&name.name) == Builtin::Unknown,
            E::MethodCall { .. } | E::New { .. } => true,
            _ => false,
        }
    });
    found
}

/// The expressions of a statement the model reads.
fn expressions(s: &Statement) -> Vec<&Expression> {
    match &s.kind {
        S::Assignment { target, value } => vec![target, value],
        S::Assign { assignments } => assignments
            .iter()
            .flat_map(|a| [&a.target, &a.value])
            .collect(),
        S::If { condition, .. } => vec![condition],
        S::Do {
            from,
            to,
            by,
            while_condition,
            ..
        } => [from, to, by, while_condition]
            .into_iter()
            .flatten()
            .collect(),
        S::Repeat {
            while_condition, ..
        } => while_condition.iter().collect(),
        S::Display { items, .. } => items.iter().map(|i| &i.expression).collect(),
        S::Message { items, .. } | S::Put { items, .. } | S::Export { items, .. } => {
            items.iter().collect()
        }
        S::Case { expression, .. } => vec![expression],
        _ => Vec::new(),
    }
}

/// `NOT a = b` is `NOT (a = b)` in ABL: `NOT` binds looser than a comparison,
/// while the parser reads `(NOT a) = b`.
fn loosen_not(e: &Expression) -> Option<Expression> {
    let (a, b) = match &e.kind {
        E::Equal(a, b)
        | E::NotEqual(a, b)
        | E::LessThan(a, b)
        | E::LessThanOrEqual(a, b)
        | E::GreaterThan(a, b)
        | E::GreaterThanOrEqual(a, b)
        | E::Begins(a, b)
        | E::Matches(a, b)
        | E::Contains(a, b) => (a, b),
        _ => return None,
    };
    let E::Not(a) = &a.kind else {
        return None;
    };
    let (a, b) = (a.clone(), b.clone());
    let kind = match &e.kind {
        E::Equal(..) => E::Equal(a, b),
        E::NotEqual(..) => E::NotEqual(a, b),
        E::LessThan(..) => E::LessThan(a, b),
        E::LessThanOrEqual(..) => E::LessThanOrEqual(a, b),
        E::GreaterThan(..) => E::GreaterThan(a, b),
        E::GreaterThanOrEqual(..) => E::GreaterThanOrEqual(a, b),
        E::Begins(..) => E::Begins(a, b),
        E::Matches(..) => E::Matches(a, b),
        _ => E::Contains(a, b),
    };
    let mut inner = Expression::new(kind);
    inner.span = e.span;
    let mut outer = Expression::new(E::Not(Box::new(inner)));
    outer.span = e.span;
    Some(outer)
}

// Bound the relations between locals without limiting the value facts.
const TRACKED_LIMIT: usize = 256;
/// The most work the solve of one unit may do (see [`Fact::cost`]). Past it
/// the unit is Unknown.
const WORK_LIMIT: usize = 1_000_000;

/// The keys of the locals, or of the fields given by `spans`, that a
/// condition, comparison or loop limit reads.
fn tested_names<'a>(
    statements: impl Iterator<Item = &'a Statement>,
    spans: Option<&FxHashMap<(u32, u32), String>>,
) -> FxHashSet<String> {
    let mut out = FxHashSet::default();
    let mut collect = |e: &Expression| {
        walk(e, &mut |e| match (spans, &e.kind) {
            (Some(spans), E::Identifier(_) | E::FieldAccess { .. }) => {
                if let Some(key) = spans.get(&(e.span.start, e.span.end)) {
                    out.insert(key.clone());
                }
            }
            (None, E::Identifier(i)) => {
                out.insert(i.name.to_ascii_lowercase());
            }
            _ => {}
        })
    };
    for s in statements {
        let whole: Vec<&Expression> = match &s.kind {
            S::If { condition, .. } => vec![condition],
            S::Do {
                from,
                to,
                while_condition,
                ..
            } => [from, to, while_condition].into_iter().flatten().collect(),
            S::Repeat {
                while_condition, ..
            } => while_condition.iter().collect(),
            _ => Vec::new(),
        };
        for e in whole {
            collect(e);
        }
        for e in expressions(s) {
            walk(e, &mut |e| match &e.kind {
                E::Equal(a, b)
                | E::NotEqual(a, b)
                | E::LessThan(a, b)
                | E::LessThanOrEqual(a, b)
                | E::GreaterThan(a, b)
                | E::GreaterThanOrEqual(a, b)
                | E::Begins(a, b)
                | E::Matches(a, b)
                | E::Contains(a, b)
                | E::And(a, b)
                | E::Or(a, b) => {
                    collect(a);
                    collect(b);
                }
                E::Not(c) | E::IfThenElse(c, ..) => collect(c),
                _ => {}
            });
        }
    }
    out
}

/// A statement that cannot change which record a buffer holds.
fn keeps_buffers(s: &Statement) -> bool {
    matches!(
        s.kind,
        S::Assignment { .. }
            | S::Assign { .. }
            | S::If { .. }
            | S::Do { .. }
            | S::Repeat { .. }
            | S::Display { .. }
            | S::Message { .. }
            | S::Put { .. }
            | S::Export { .. }
            | S::Case { .. }
            | S::Leave(_)
            | S::Next(_)
            | S::VariableDeclaration { .. }
    ) && !expressions(s).into_iter().any(calls)
}

fn default_value(t: &TypeSource) -> u8 {
    match t {
        TypeSource::Explicit(
            DataType::Date
            | DataType::DateTime
            | DataType::DateTimeTz
            | DataType::Handle
            | DataType::Rowid
            | DataType::Recid
            | DataType::Class(_)
            | DataType::Generic(_),
        ) => QUESTION,
        TypeSource::Explicit(_) => KNOWN,
        _ => UNKNOWN,
    }
}
fn children<'a>(s: &'a Statement, out: &mut Vec<&'a Statement>) {
    out.push(s);
    match &s.kind {
        S::If {
            then_branch,
            else_branch,
            ..
        } => {
            children(then_branch, out);
            if let Some(s) = else_branch {
                children(s, out);
            }
        }
        S::Block(body)
        | S::VariableDeclarationList(body)
        | S::Do { body, .. }
        | S::Repeat { body, .. }
        | S::ForEach { body, .. }
        | S::Procedure { body, .. }
        | S::Function { body, .. }
        | S::Class { body, .. }
        | S::Interface { body, .. }
        | S::Destructor { body, .. }
        | S::Catch { body, .. }
        | S::Finally { body } => {
            for s in body {
                children(s, out);
            }
        }
        S::Method {
            parameters, body, ..
        }
        | S::Constructor {
            parameters, body, ..
        } => {
            for s in parameters.iter().chain(body) {
                children(s, out);
            }
        }
        S::Case {
            when_branches,
            otherwise,
            ..
        } => {
            for b in when_branches {
                for s in &b.body {
                    children(s, out);
                }
            }
            if let Some(body) = otherwise {
                for s in body {
                    children(s, out);
                }
            }
        }
        S::Label { body, .. } => children(body, out),
        S::On {
            kind:
                OnKind::UiEvent {
                    action: OnAction::Block(body),
                    ..
                }
                | OnKind::DbEvent {
                    action: OnAction::Block(body),
                    ..
                },
        } => children(body, out),
        S::Property {
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            for s in get_body
                .iter()
                .flatten()
                .chain(set_body.iter().flatten())
                .chain(set_parameters)
            {
                children(s, out);
            }
        }
        _ => {}
    }
}
/// What each statement of the unit does to the fields: reads and `CREATE`
/// refill the buffer, other writes to the record make its fields Unknown.
fn field_effects(
    p: &Problem<'_>,
    fields: &fields::UnitFields,
    indexed: &FxHashMap<String, usize>,
    scopes: Option<&oxabl_scope::FileScopes>,
) -> FxHashMap<NodeId, Effects> {
    let Some(scopes) = scopes else {
        return FxHashMap::default();
    };
    let mut of_buffer: FxHashMap<BufferId, Vec<usize>> = FxHashMap::default();
    for v in &fields.vars {
        of_buffer.entry(v.buffer).or_default().push(indexed[&v.key]);
    }
    let mut out: FxHashMap<NodeId, Effects> = FxHashMap::default();
    for r in fields.reads.iter().map(|&i| &scopes.reads()[i]) {
        let Some(b) = r.buffer else {
            continue;
        };
        let Some(vars) = of_buffer.get(&b) else {
            continue;
        };
        let fx = out.entry(r.stmt).or_default();
        match r.kind {
            ReadKind::Find | ReadKind::Get => fx.refill.extend(vars),
            ReadKind::For => {
                fx.body_refill.extend(vars);
                fx.done_forget.extend(vars);
            }
            _ => {}
        }
    }
    let mut spans: Vec<(u32, u32, NodeId)> = p
        .statements
        .values()
        .map(|s| (s.span.start, s.span.end, s.id))
        .collect();
    spans.sort_by_key(|&(start, end, _)| (start, std::cmp::Reverse(end)));
    let refs: Vec<_> = fields
        .references
        .iter()
        .map(|&i| &scopes.references()[i])
        .collect();
    let positions: Vec<u32> = refs.iter().map(|r| r.at.start).collect();
    let owners = fields::owners(&spans, &positions);
    for (r, owner) in refs.iter().zip(owners) {
        let Some(id) = owner else {
            continue;
        };
        let fx = out.entry(id).or_default();
        if r.field {
            // `ASSIGN` and `=` are read by the statement transfer.
            let handled = p
                .statements
                .get(&id)
                .is_some_and(|s| matches!(s.kind, S::Assignment { .. } | S::Assign { .. }));
            if r.write.is_some()
                && !handled
                && let Some(&i) = p.field_spans.get(&(r.at.start, r.at.end))
            {
                fx.forget.push(i);
            }
            continue;
        }
        let Some(vars) = of_buffer.get(&r.buffer) else {
            continue;
        };
        match r.write {
            Some(UpdateKind::Create) => fx.refill.extend(vars),
            Some(UpdateKind::ExclusiveRead) if !r.passed && !r.opaque => {}
            Some(_) => fx.forget.extend(vars),
            None if r.passed || r.opaque => fx.forget.extend(vars),
            None => {}
        }
    }
    out
}

impl Problem<'_> {
    fn view<'s>(&'s self, e: &'s Expression) -> &'s Expression {
        self.loosened.get(&e.id).unwrap_or(e)
    }
    /// The condition `e` tests, as the atom that says it is true, and whether
    /// it may also be `?` (a logical local).
    fn atom(&self, e: &Expression) -> Option<(usize, bool)> {
        if let Some(i) = self.variable(e) {
            return Fact::flag(self.tracked, i).map(|a| (a, true));
        }
        let k = lookup(&self.buffers, available_buffer(e)?)?;
        Some((Fact::buffer(self.tracked, self.counted.len(), k), false))
    }
    fn forget_buffers(&self, f: &mut Fact) {
        f.drop_atoms_from(Fact::buffer(self.tracked, self.counted.len(), 0));
    }
    fn variable(&self, e: &Expression) -> Option<usize> {
        if self.fields_only {
            if matches!(e.kind, E::Identifier(_) | E::FieldAccess { .. }) {
                return self.field_spans.get(&(e.span.start, e.span.end)).copied();
            }
            return None;
        }
        if let E::Identifier(i) = &e.kind {
            lookup(&self.names, &i.name)
        } else {
            None
        }
    }
    fn eval(&self, e: &Expression, f: &Fact) -> u8 {
        let e = self.view(e);
        if f.dead() {
            return 0;
        }
        match &e.kind {
            E::Literal(Literal::Unknown(_)) => QUESTION,
            E::Literal(_) | E::New { .. } | E::CanFind { .. } => KNOWN,
            E::Identifier(_) | E::FieldAccess { .. } => self
                .variable(e)
                .map_or(UNKNOWN, |i| f.get(i).copied().unwrap_or(UNKNOWN)),
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                // A zero divisor (V3, medium) is no source.
                if matches!(e.kind, E::Divide(..)) && self.number(b) == Some(0) {
                    return UNKNOWN;
                }
                let (a, b) = (self.eval(a, f), self.eval(b, f));
                if a == QUESTION || b == QUESTION {
                    return QUESTION;
                }
                ((a | b) & (QUESTION | OPAQUE)) | ((a & b) & KNOWN)
            }
            E::Negate(a) => self.eval(a, f),
            E::Equal(..) | E::NotEqual(..) => KNOWN,
            E::IfThenElse(c, a, b) => {
                if let E::Literal(Literal::Boolean(v)) = &c.kind {
                    return self.eval(if v.value { a } else { b }, f);
                }
                if self.eval(c, f) == QUESTION {
                    return self.eval(b, f);
                }
                let mut yes = f.clone();
                let mut no = f.clone();
                self.narrow(c, true, &mut yes);
                self.narrow(c, false, &mut no);
                self.eval(a, &yes) | self.eval(b, &no)
            }
            E::FunctionCall { name, arguments } => match builtin(&name.name) {
                Builtin::Absorbs => KNOWN,
                Builtin::Propagates => {
                    let (maybe, known) = arguments.iter().fold((0, KNOWN), |(maybe, known), a| {
                        let value = self.eval(a, f);
                        (maybe | (value & (QUESTION | OPAQUE)), known & value)
                    });
                    maybe | (known & KNOWN)
                }
                Builtin::Unknown => UNKNOWN,
            },
            _ => UNKNOWN,
        }
    }
    fn assign(&self, target: &Expression, value: &Expression, f: &mut Fact) {
        self.forget_calls(value, f);
        if self.when_values.contains(&value.id) {
            // The pair may not run: the target keeps the old value or gets the new.
            if let Some(i) = self.variable(target) {
                f.forget(i, UNKNOWN);
            }
            return;
        }
        if let Some(i) = self.variable(target) {
            let result = self.eval(value, f);
            let required = self.required(value, f).unwrap_or_default();
            let preserves = self.propagates_variable(value, i);
            f.write(i, result, required, preserves);
            if let E::Literal(Literal::Boolean(v)) = &value.kind
                && let Some(a) = Fact::flag(self.tracked, i)
            {
                f.assume(a + usize::from(!v.value));
            }
        }
    }
    fn propagates_variable(&self, e: &Expression, i: usize) -> bool {
        if self.variable(e) == Some(i) {
            return true;
        }
        match &e.kind {
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => self.propagates_variable(a, i) || self.propagates_variable(b, i),
            E::Negate(a) => self.propagates_variable(a, i),
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                arguments.iter().any(|a| self.propagates_variable(a, i))
            }
            _ => false,
        }
    }
    fn required(&self, e: &Expression, f: &Fact) -> Option<Required> {
        let e = self.view(e);
        if self.eval(e, f) & QUESTION == 0 {
            return None;
        }
        if let Some(i) = self.variable(e) {
            return f.requires.get(&i).cloned();
        }
        let mut merged: Option<Required> = None;
        let mut add = |r: Option<Required>| {
            if let Some(r) = r {
                if let Some(m) = &mut merged {
                    m.intersect(&r);
                } else {
                    merged = Some(r);
                }
            }
        };
        match &e.kind {
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                add(self.required(a, f));
                add(self.required(b, f));
            }
            E::Negate(a) => add(self.required(a, f)),
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                for a in arguments {
                    add(self.required(a, f));
                }
            }
            E::IfThenElse(c, a, b) => {
                let mut yes = f.clone();
                let mut no = f.clone();
                self.narrow(c, true, &mut yes);
                self.narrow(c, false, &mut no);
                add(self.required(a, &yes));
                add(self.required(b, &no));
            }
            _ => {}
        }
        Some(merged.unwrap_or_else(|| f.certain()))
    }
    fn number(&self, e: &Expression) -> Option<i64> {
        match &e.kind {
            E::Literal(Literal::Integer(v)) => Some(v.value),
            E::Literal(Literal::Decimal(v)) if v.value.is_zero() => Some(0),
            E::Identifier(_) => self.variable(e).and_then(|i| self.variables[i].constant),
            E::Negate(a) => self.number(a)?.checked_neg(),
            _ => None,
        }
    }
    fn constant_condition(&self, e: &Expression) -> Option<bool> {
        let (a, b) = match &e.kind {
            E::LessThan(a, b)
            | E::LessThanOrEqual(a, b)
            | E::GreaterThan(a, b)
            | E::GreaterThanOrEqual(a, b)
            | E::Equal(a, b)
            | E::NotEqual(a, b) => (self.number(a)?, self.number(b)?),
            _ => return None,
        };
        Some(match e.kind {
            E::LessThan(..) => a < b,
            E::LessThanOrEqual(..) => a <= b,
            E::GreaterThan(..) => a > b,
            E::GreaterThanOrEqual(..) => a >= b,
            E::Equal(..) => a == b,
            _ => a != b,
        })
    }
    fn prove_known(&self, e: &Expression, strong: bool, f: &mut Fact) {
        if f.dead() {
            return;
        }
        if self.eval(e, f) == QUESTION {
            f.fill(0);
            return;
        }
        if let Some(i) = self.variable(e) {
            f.known(i, strong);
            return;
        }
        match &e.kind {
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                self.prove_known(a, strong, f);
                self.prove_known(b, strong, f);
            }
            E::Negate(a) => self.prove_known(a, strong, f),
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                for a in arguments {
                    self.prove_known(a, strong, f);
                }
            }
            _ => {}
        }
    }
    fn nullable_variable(&self, e: &Expression) -> Option<usize> {
        if let Some(i) = self.variable(e) {
            return Some(i);
        }
        if let E::FunctionCall { name, arguments } = &e.kind
            && arguments.len() == 1
            && (name.name.eq_ignore_ascii_case("NUM-ENTRIES")
                || name.name.eq_ignore_ascii_case("LENGTH"))
        {
            return self.nullable_variable(&arguments[0]);
        }
        None
    }
    fn required_question(&self, e: &Expression, f: &Fact, r: &Required) -> bool {
        let e = self.view(e);
        if let Some(i) = self.variable(e) {
            return f[i] == QUESTION || r.contains(i);
        }
        match &e.kind {
            E::Literal(Literal::Unknown(_)) => true,
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => self.required_question(a, f, r) || self.required_question(b, f, r),
            E::Negate(a) => self.required_question(a, f, r),
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                arguments.iter().any(|a| self.required_question(a, f, r))
            }
            _ => self.eval(e, f) == QUESTION,
        }
    }
    // Possible logical values: yes=1, no=2, ?=4. These only remove sources.
    fn truth(&self, e: &Expression, f: &Fact, r: &Required) -> u8 {
        let e = self.view(e);
        if let Some(value) = self.constant_condition(e) {
            return if value { 1 } else { 2 };
        }
        if let Some((a, flag)) = self.atom(e) {
            let held = |x: usize| r.contains(x) || f.held.contains(x);
            let known = if held(a) {
                1
            } else if held(a + 1) {
                6
            } else {
                7
            };
            if !flag {
                return known & 3;
            }
            let base = if self.required_question(e, f, r) {
                4
            } else {
                7
            };
            return known & base;
        }
        match &e.kind {
            E::Literal(Literal::Boolean(v)) => {
                if v.value {
                    1
                } else {
                    2
                }
            }
            E::Literal(Literal::Unknown(_)) => 4,
            E::Equal(a, b) | E::NotEqual(a, b) => {
                let (aq, bq) = (
                    self.required_question(a, f, r),
                    self.required_question(b, f, r),
                );
                let result = if aq && bq {
                    Some(true)
                } else if (aq && self.eval(b, f) == KNOWN) || (bq && self.eval(a, f) == KNOWN) {
                    Some(false)
                } else {
                    None
                };
                result.map_or(3, |v| {
                    if v == matches!(e.kind, E::Equal(..)) {
                        1
                    } else {
                        2
                    }
                })
            }
            E::LessThan(a, b)
            | E::LessThanOrEqual(a, b)
            | E::GreaterThan(a, b)
            | E::GreaterThanOrEqual(a, b)
            | E::Begins(a, b)
            | E::Matches(a, b) => {
                if self.required_question(a, f, r) || self.required_question(b, f, r) {
                    4
                } else {
                    7
                }
            }
            E::Not(a) => {
                let a = self.truth(a, f, r);
                ((a & 1) << 1) | ((a & 2) >> 1) | (a & 4)
            }
            E::And(a, b) | E::Or(a, b) => {
                let (a, b) = (self.truth(a, f, r), self.truth(b, f, r));
                let mut out = 0;
                for x in [1, 2, 4] {
                    for y in [1, 2, 4] {
                        if a & x == 0 || b & y == 0 {
                            continue;
                        }
                        out |= if matches!(e.kind, E::And(..)) {
                            if x == 2 || y == 2 {
                                2
                            } else if x == 1 && y == 1 {
                                1
                            } else {
                                4
                            }
                        } else if x == 1 || y == 1 {
                            1
                        } else if x == 2 && y == 2 {
                            2
                        } else {
                            4
                        };
                    }
                }
                out
            }
            _ => {
                if self.required_question(e, f, r) {
                    4
                } else {
                    7
                }
            }
        }
    }
    fn filter_sources(&self, c: &Expression, yes: bool, f: &mut Fact) {
        let wanted = if yes { 1 } else { 2 | 4 };
        if self.truth(c, f, &Required::new()) & wanted == 0 {
            f.fill(0);
            return;
        }
        let cut: Vec<usize> = f
            .requires
            .iter()
            .filter(|(_, r)| self.truth(c, f, r) & wanted == 0)
            .map(|(&i, _)| i)
            .collect();
        for i in cut {
            f.drop_source(i);
            if f.dead() {
                return;
            }
        }
    }
    fn narrow(&self, e: &Expression, yes: bool, f: &mut Fact) {
        self.narrow_with(e, yes, true, f);
    }
    /// Narrow `f` by `e` being `yes`. A weak narrowing only removes values:
    /// it never turns an Unknown local into a known one or into a source.
    /// Narrowing that depends on the fact must be weak to stay monotone.
    fn narrow_with(&self, e: &Expression, yes: bool, strong: bool, f: &mut Fact) {
        let e = self.view(e);
        if f.dead() {
            return;
        }
        if self.constant_condition(e).is_some_and(|value| value != yes) {
            f.fill(0);
            return;
        }
        self.filter_sources(e, yes, f);
        if f.dead() {
            return;
        }
        if let Some((a, _)) = self.atom(e) {
            f.assume(if yes { a } else { a + 1 });
        }
        match &e.kind {
            E::Literal(Literal::Boolean(v)) if v.value != yes => f.fill(0),
            E::Literal(Literal::Unknown(_)) if yes => f.fill(0),
            E::LessThan(a, b)
            | E::LessThanOrEqual(a, b)
            | E::GreaterThan(a, b)
            | E::GreaterThanOrEqual(a, b)
            | E::Begins(a, b)
            | E::Matches(a, b)
                if yes =>
            {
                self.prove_known(a, strong, f);
                self.prove_known(b, strong, f);
            }
            E::Equal(a, b) | E::NotEqual(a, b) => {
                let equal = matches!(e.kind, E::Equal(..)) == yes;
                let var = if matches!(a.kind, E::Literal(Literal::Unknown(_))) {
                    self.nullable_variable(b)
                } else if matches!(b.kind, E::Literal(Literal::Unknown(_))) {
                    self.nullable_variable(a)
                } else {
                    None
                };
                // A field equal to a known value is not `?`.
                if self.fields_only && var.is_none() && equal {
                    for (x, y) in [(a, b), (b, a)] {
                        if let Some(i) = self.nullable_variable(x)
                            && self.eval(y, f) == KNOWN
                        {
                            f.known(i, false);
                        }
                    }
                }
                if var.is_none()
                    && let (Some(a), Some(b)) =
                        (self.nullable_variable(a), self.nullable_variable(b))
                    && equal
                {
                    f.link(a, b);
                    if f[a] == KNOWN {
                        f.known(b, false);
                    } else if f[a] == QUESTION {
                        f.question(b, false);
                    }
                    if !f.dead() {
                        if f[b] == KNOWN {
                            f.known(a, false);
                        } else if f[b] == QUESTION {
                            f.question(a, false);
                        }
                    }
                }
                if let Some(i) = var {
                    if equal {
                        if f[i] & (QUESTION | OPAQUE) != 0 {
                            f.question(i, strong);
                        } else {
                            f.fill(0);
                        }
                    } else {
                        f.known(i, strong);
                    }
                }
            }
            E::Identifier(_) | E::FieldAccess { .. } if yes => {
                if let Some(i) = self.variable(e) {
                    if f[i] == QUESTION {
                        f.fill(0);
                    } else {
                        f.known(i, strong);
                    }
                }
            }
            E::And(a, b) if yes => {
                self.narrow_with(a, true, strong, f);
                self.narrow_with(b, true, strong, f);
            }
            E::Or(a, b) if !yes => {
                self.narrow_with(a, false, strong, f);
                self.narrow_with(b, false, strong, f);
            }
            // A false AND with one side true: the other is false (OR dually).
            E::And(a, b) | E::Or(a, b) => {
                let side = if yes { 2 } else { 1 };
                let a_decided = self.truth(a, f, &Required::new()) == side;
                let b_decided = self.truth(b, f, &Required::new()) == side;
                if a_decided {
                    self.narrow_with(b, yes, false, f);
                }
                if b_decided {
                    self.narrow_with(a, yes, false, f);
                }
            }
            E::Not(a) => self.narrow_with(a, !yes, strong, f),
            _ => {}
        }
        if f.dead() {
            f.fill(0);
        }
    }
    fn condition<'a>(&self, s: &'a Statement) -> Option<&'a Expression> {
        match &s.kind {
            S::If { condition, .. } => Some(condition),
            S::Do {
                while_condition, ..
            }
            | S::Repeat {
                while_condition, ..
            } => while_condition.as_ref(),
            _ => None,
        }
    }
    fn forget_calls(&self, e: &Expression, f: &mut Fact) {
        if f.dead() {
            return;
        }
        match &e.kind {
            E::FunctionCall { name, arguments } => {
                let pure = builtin(&name.name) != Builtin::Unknown;
                if !pure {
                    for &i in &self.call_writes {
                        f.forget(i, UNKNOWN);
                    }
                }
                for a in arguments {
                    self.forget_calls(a, f);
                    if !pure && let Some(i) = self.variable(a) {
                        f.forget(i, UNKNOWN);
                    }
                }
            }
            E::MethodCall {
                object, arguments, ..
            } => {
                for &i in &self.call_writes {
                    f.forget(i, UNKNOWN);
                }
                self.forget_calls(object, f);
                for a in arguments {
                    self.forget_calls(a, f);
                    if let Some(i) = self.variable(a) {
                        f.forget(i, UNKNOWN);
                    }
                }
            }
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
            | E::Or(a, b) => {
                self.forget_calls(a, f);
                self.forget_calls(b, f);
            }
            E::Negate(a)
            | E::Not(a)
            | E::MemberAccess { object: a, .. }
            | E::FieldAccess { qualifier: a, .. } => self.forget_calls(a, f),
            E::IfThenElse(c, a, b) => {
                self.forget_calls(c, f);
                self.forget_calls(a, f);
                self.forget_calls(b, f);
            }
            _ => {}
        }
    }
    fn forget_call_writes(&self, f: &mut Fact) {
        for &i in &self.call_writes {
            f.forget(i, UNKNOWN);
        }
    }
    fn step(&self, s: &Statement, f: &mut Fact) {
        if f.dead() {
            return;
        }
        if !keeps_buffers(s) {
            self.forget_buffers(f);
        }
        if let Some(c) = self.condition(s) {
            self.forget_calls(c, f);
        }
        match &s.kind {
            S::Assignment { target, value } => self.assign(target, value, f),
            S::Assign { assignments } => {
                for a in assignments {
                    self.assign(&a.target, &a.value, f);
                }
            }
            S::Do {
                loop_var: Some(name),
                ..
            } => {
                if let Some(i) = lookup(&self.names, &name.name) {
                    f.forget(i, UNKNOWN);
                }
            }
            S::Repeat { .. } => {
                if let Some(i) = self
                    .repeat_vars
                    .get(&s.id)
                    .and_then(|n| lookup(&self.names, n))
                {
                    f.forget(i, UNKNOWN);
                }
            }
            S::Import { targets, .. } | S::Update { targets, .. } | S::Set { targets, .. } => {
                // Input runs the triggers of the frame.
                if !matches!(s.kind, S::Import { .. }) {
                    self.forget_call_writes(f);
                }
                for target in targets {
                    if let Some(i) = self.variable(target) {
                        f.forget(i, UNKNOWN);
                    }
                }
            }
            // These run triggers, which may write the locals other units write.
            S::WaitFor { .. } | S::Apply { .. } | S::Enable { .. } => self.forget_call_writes(f),
            S::Publish { arguments, .. } => {
                self.forget_call_writes(f);
                for a in arguments {
                    self.forget_calls(&a.expression, f);
                    if a.direction != ParameterDirection::Input
                        && let Some(i) = self.variable(&a.expression)
                    {
                        f.forget(i, UNKNOWN);
                    }
                }
            }
            S::Message {
                items, set_targets, ..
            } => {
                for e in items {
                    self.forget_calls(e, f);
                }
                for target in set_targets {
                    if let Some(i) = lookup(&self.names, &target.name) {
                        f.forget(i, UNKNOWN);
                    }
                }
            }
            // An unseen callee may change variables passed by reference.
            S::Run { arguments, .. } => {
                self.forget_call_writes(f);
                for a in arguments {
                    self.forget_calls(&a.expression, f);
                    if a.direction != ParameterDirection::Input
                        && let Some(i) = self.variable(&a.expression)
                    {
                        f.forget(i, UNKNOWN);
                    }
                }
            }
            S::ExpressionStatement(e) => self.forget_calls(e, f),
            S::Display { items, .. } => {
                for i in items {
                    self.forget_calls(&i.expression, f);
                }
            }
            S::Put { items, .. } | S::Export { items, .. } => {
                for e in items {
                    self.forget_calls(e, f);
                }
            }
            _ => {}
        }
        self.field_effects(s, f);
    }
    /// What the statement does to the fields of the buffers it names: a
    /// refilled buffer may hold `?` again, and the criteria of a `FIND` hold
    /// of the record it found.
    fn field_effects(&self, s: &Statement, f: &mut Fact) {
        if !self.fields_only || f.dead() {
            return;
        }
        if let Some(fx) = self.effects.get(&s.id) {
            for &i in &fx.forget {
                f.forget(i, UNKNOWN);
            }
            for &i in &fx.refill {
                f.write(i, FIELD, Required::new(), false);
            }
        }
        if let S::Find {
            where_clause: Some(w),
            ..
        } = &s.kind
        {
            self.narrow(w, true, f);
        }
    }
    fn use_var(
        &self,
        e: &Expression,
        f: &Fact,
        unit: UnitId,
        propagation: bool,
        out: &mut Vec<ValueUse>,
    ) {
        if f.dead() {
            return;
        }
        if let Some(i) = self.variable(e)
            && f[i] & QUESTION != 0
        {
            out.push(ValueUse {
                unit,
                at: e.span,
                name: self.variables[i].name.clone(),
                propagation,
            });
        }
    }
    fn sinks(&self, e: &Expression, f: &Fact, unit: UnitId, stored: bool, out: &mut Vec<ValueUse>) {
        self.sinks_under(e, f, unit, stored, &[], out);
    }
    /// Narrow `f` by `c`, then again by the enclosing conditions: an outer
    /// `A AND B` known false decides `B` once an inner test proves `A`.
    fn narrow_under(&self, c: &Expression, yes: bool, outer: &[(&Expression, bool)], f: &mut Fact) {
        self.narrow(c, yes, f);
        for &(c, yes) in outer {
            self.narrow(c, yes, f);
        }
    }
    fn sinks_under(
        &self,
        e: &Expression,
        f: &Fact,
        unit: UnitId,
        stored: bool,
        outer: &[(&Expression, bool)],
        out: &mut Vec<ValueUse>,
    ) {
        let e = self.view(e);
        match &e.kind {
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                if stored {
                    self.operands(a, f, unit, true, out);
                    self.operands(b, f, unit, true, out);
                }
                self.sinks_under(a, f, unit, stored, outer, out);
                self.sinks_under(b, f, unit, stored, outer, out);
            }
            E::Negate(a) => {
                if stored {
                    self.operands(a, f, unit, true, out);
                }
                self.sinks_under(a, f, unit, stored, outer, out);
            }
            E::LessThan(a, b)
            | E::LessThanOrEqual(a, b)
            | E::GreaterThan(a, b)
            | E::GreaterThanOrEqual(a, b)
            | E::Begins(a, b)
            | E::Matches(a, b) => {
                self.operands(a, f, unit, false, out);
                self.operands(b, f, unit, false, out);
            }
            E::And(a, b) | E::Or(a, b) => {
                let and = matches!(e.kind, E::And(..));
                let mut inner = outer.to_vec();
                let mut left = f.clone();
                self.narrow_under(b, and, outer, &mut left);
                inner.push((b, and));
                self.sinks_under(a, &left, unit, false, &inner, out);
                inner.pop();
                let mut narrowed = f.clone();
                self.narrow_under(a, and, outer, &mut narrowed);
                inner.push((a, and));
                self.sinks_under(b, &narrowed, unit, false, &inner, out);
            }
            E::Not(a) => self.sinks_under(a, f, unit, false, outer, out),
            E::IfThenElse(c, a, b) => {
                self.sinks_under(c, f, unit, false, outer, out);
                let mut inner = outer.to_vec();
                let mut yes = f.clone();
                let mut no = f.clone();
                self.narrow_under(c, true, outer, &mut yes);
                self.narrow_under(c, false, outer, &mut no);
                inner.push((c, true));
                self.sinks_under(a, &yes, unit, stored, &inner, out);
                inner.pop();
                inner.push((c, false));
                self.sinks_under(b, &no, unit, stored, &inner, out);
            }
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                for a in arguments {
                    self.sinks_under(a, f, unit, stored, outer, out);
                }
            }
            _ => {}
        }
    }
    fn operands(&self, e: &Expression, f: &Fact, unit: UnitId, p: bool, out: &mut Vec<ValueUse>) {
        self.use_var(e, f, unit, p, out);
        match &e.kind {
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                self.operands(a, f, unit, p, out);
                self.operands(b, f, unit, p, out);
            }
            E::Negate(a) => self.operands(a, f, unit, p, out),
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                for a in arguments {
                    self.operands(a, f, unit, p, out);
                }
            }
            _ => {}
        }
    }
}
impl Analysis for Problem<'_> {
    type Fact = Fact;
    fn direction(&self) -> Direction {
        Direction::Forward
    }
    fn initial(&self, _: &Cfg) -> Fact {
        Fact::new(vec![0; self.variables.len()], self.tracked)
    }
    fn boundary(&self, _: &Cfg) -> Fact {
        Fact::new(
            self.variables.iter().map(|v| v.initial).collect(),
            self.tracked,
        )
    }
    fn join(&self, a: &mut Fact, b: &Fact) {
        a.join(b);
    }
    fn fact_cost(&self, f: &Fact) -> usize {
        f.cost()
    }
    fn transfer(&self, c: &Cfg, n: NodeIx, f: &mut Fact) {
        if !matches!(c.node(n).kind, NodeKind::Stmt | NodeKind::Head) {
            return;
        }
        let span = c.node(n).span;
        let hidden = self.elided.partition_point(|&at| at < span.start);
        if c.node(n).opaque.is_some()
            || c.node(n).undoes.is_some()
            || self.elided.get(hidden).is_some_and(|&at| at < span.end)
        {
            if !f.dead() {
                f.fill(UNKNOWN);
            }
            return;
        }
        match c.node(n).stmt.and_then(|id| self.statements.get(&id)) {
            Some(s) => self.step(s, f),
            None => self.forget_buffers(f),
        }
    }
    fn transfer_edge(&self, c: &Cfg, n: NodeIx, e: &Edge, f: &mut Fact) {
        self.edge_step(c, n, e, f);
        if f.dead() {
            return;
        }
        // A counted loop whose body runs leaves only after an iteration: the
        // sources that only entering paths carry are gone at its exit.
        if let Some(&j) = self.counted.get(&n)
            && e.kind == EdgeKind::LoopDone
        {
            f.drop_marked(Fact::fresh(self.tracked, j));
        }
        if let Some(&j) = self.counted.get(&e.to) {
            let a = Fact::fresh(self.tracked, j);
            if matches!(
                e.kind,
                EdgeKind::LoopBack | EdgeKind::Next | EdgeKind::Retry
            ) {
                f.unmark_sources(a);
            } else {
                f.mark_sources(a);
            }
        }
    }
}

impl Problem<'_> {
    /// A `FOR` header fills its buffers on the way into its body, and the
    /// `WHERE` of a `FOR EACH` holds of the record it selects. Leaving the
    /// loop leaves no record.
    fn field_edge(&self, s: &Statement, kind: EdgeKind, f: &mut Fact) {
        let fx = self.effects.get(&s.id);
        match kind {
            EdgeKind::LoopBody | EdgeKind::Normal => {
                for &i in fx.iter().flat_map(|fx| &fx.body_refill) {
                    f.write(i, FIELD, Required::new(), false);
                }
                if let S::ForEach {
                    where_clause: Some(w),
                    ..
                } = &s.kind
                {
                    self.narrow(w, true, f);
                }
            }
            EdgeKind::LoopDone => {
                for &i in fx.iter().flat_map(|fx| &fx.done_forget) {
                    f.forget(i, UNKNOWN);
                }
            }
            _ => {}
        }
    }
    /// `DO v = a TO b [BY k]` with constant bounds that run the body, and no
    /// `WHILE`.
    fn runs_body(&self, s: &Statement) -> bool {
        let S::Do {
            loop_var: Some(_),
            from: Some(from),
            to: Some(to),
            by,
            while_condition: None,
            ..
        } = &s.kind
        else {
            return false;
        };
        let (Some(from), Some(to)) = (self.number(from), self.number(to)) else {
            return false;
        };
        match by.as_ref().map(|b| self.number(b)) {
            None => from <= to,
            Some(Some(k)) if k > 0 => from <= to,
            Some(Some(k)) if k < 0 => from >= to,
            _ => false,
        }
    }
    fn edge_step(&self, c: &Cfg, n: NodeIx, e: &Edge, f: &mut Fact) {
        if matches!(e.kind, EdgeKind::Raise(_)) {
            if !f.dead() {
                f.fill(UNKNOWN);
            }
            return;
        }
        self.transfer(c, n, f);
        if !matches!(c.node(n).kind, NodeKind::Stmt | NodeKind::Head) {
            return;
        }
        let span = c.node(n).span;
        let hidden = self.elided.partition_point(|&at| at < span.start);
        if c.node(n).opaque.is_some()
            || c.node(n).undoes.is_some()
            || self.elided.get(hidden).is_some_and(|&at| at < span.end)
        {
            if !f.dead() {
                f.fill(UNKNOWN);
            }
            return;
        }
        let Some(s) = c.node(n).stmt.and_then(|id| self.statements.get(&id)) else {
            return;
        };
        if self.fields_only && c.node(n).kind == NodeKind::Head {
            self.field_edge(s, e.kind, f);
            if f.dead() {
                return;
            }
        }
        // A `?` limit ends a counted loop at once (V6, V8); this only removes sources.
        if let (
            EdgeKind::LoopBody,
            S::Do {
                from, to: Some(to), ..
            },
        ) = (e.kind, &s.kind)
        {
            if let Some(from) = from {
                self.prove_known(from, true, f);
            }
            self.prove_known(to, true, f);
        }
        if let Some(cond) = self.condition(s) {
            match e.kind {
                EdgeKind::True | EdgeKind::LoopBody => self.narrow(cond, true, f),
                EdgeKind::False => self.narrow(cond, false, f),
                EdgeKind::LoopDone if !matches!(s.kind, S::Do { to: Some(_), .. }) => {
                    self.narrow(cond, false, f)
                }
                _ => {}
            }
        }
    }
}

/// The values of the `ASSIGN` pairs of `all` that a `WHEN` follows. The parser
/// drops the condition, so it reads the tokens.
fn when_values(all: &[&Statement], tokens: &[oxabl_lexer::Token]) -> FxHashSet<NodeId> {
    let mut out = FxHashSet::default();
    for s in all {
        let S::Assign { assignments } = &s.kind else {
            continue;
        };
        let first = tokens.partition_point(|t| t.start < s.span.start as usize);
        for t in tokens[first..]
            .iter()
            .take_while(|t| t.start < s.span.end as usize)
            .filter(|t| t.kind == oxabl_lexer::Kind::When)
        {
            let pair = assignments
                .iter()
                .filter(|a| a.value.span.end as usize <= t.start)
                .max_by_key(|a| a.value.span.end);
            if let Some(a) = pair {
                out.insert(a.value.id);
            }
        }
    }
    out
}

/// The loop variable of each `REPEAT var = a TO b` among `all`, lowercase.
fn repeat_vars(
    all: &[&Statement],
    source: &str,
    tokens: &[oxabl_lexer::Token],
) -> FxHashMap<NodeId, String> {
    use oxabl_lexer::Kind;
    let mut out = FxHashMap::default();
    for s in all {
        if !matches!(s.kind, S::Repeat { .. }) {
            continue;
        }
        let first = tokens.partition_point(|t| t.start < s.span.start as usize);
        if tokens.get(first).map(|t| t.kind) != Some(Kind::Repeat) {
            continue;
        }
        let header: Vec<_> = tokens[first + 1..]
            .iter()
            .filter(|t| t.kind != Kind::Comment)
            .take_while(|t| !matches!(t.kind, Kind::Colon | Kind::KwWhile | Kind::KwWhere))
            .collect();
        let mut depth = 0i32;
        for (i, t) in header.iter().enumerate() {
            match t.kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => depth -= 1,
                Kind::Equals if depth == 0 && i > 0 => {
                    if header[i..].iter().any(|t| t.kind == Kind::To) {
                        let name = &source[header[i - 1].start..header[i - 1].end];
                        out.insert(s.id, name.to_ascii_lowercase());
                    }
                    break;
                }
                _ => {}
            }
        }
    }
    out
}

pub(crate) fn analyze(
    source: &str,
    tokens: &[oxabl_lexer::Token],
    program: &[Statement],
    flow: &FileFlow,
    elided: &[u32],
) -> (Vec<ValueUse>, Vec<UnitValues>) {
    analyze_inspect(
        source,
        tokens,
        program,
        flow,
        elided,
        None,
        &mut |_, _, _| {},
    )
}

/// The uses of database fields that may hold `?`, and the units whose solve
/// ran out of its budget.
pub(crate) fn analyze_fields(
    source: &str,
    tokens: &[oxabl_lexer::Token],
    program: &[Statement],
    flow: &FileFlow,
    elided: &[u32],
    input: &FieldSources<'_>,
) -> (Vec<ValueUse>, Vec<UnitId>) {
    let (uses, facts) = analyze_inspect(
        source,
        tokens,
        program,
        flow,
        elided,
        Some(input),
        &mut |_, _, _| {},
    );
    let exhausted = facts
        .iter()
        .filter(|f| f.exhausted)
        .map(|f| f.unit)
        .collect();
    (uses, exhausted)
}

/// [`analyze`], showing each solved unit to `inspect`.
fn analyze_inspect(
    source: &str,
    tokens: &[oxabl_lexer::Token],
    program: &[Statement],
    flow: &FileFlow,
    elided: &[u32],
    fields: Option<&FieldSources<'_>>,
    inspect: &mut dyn FnMut(&Cfg, &Problem<'_>, &Solution<Fact>),
) -> (Vec<ValueUse>, Vec<UnitValues>) {
    let unit_fields = fields.map(|input| fields::collect(source, input));
    let mut all = Vec::new();
    for s in program {
        children(s, &mut all);
    }
    let repeat_vars = repeat_vars(&all, source, tokens);
    let when_values = if fields.is_some() {
        when_values(&all, tokens)
    } else {
        FxHashSet::default()
    };
    // The locals are not read in a field run.
    let local_all: &[&Statement] = if fields.is_some() { &[] } else { &all };
    let mut written = FxHashSet::default();
    let mut write = |e: &Expression| {
        if let E::Identifier(i) = &e.kind {
            written.insert(i.name.to_ascii_lowercase());
        }
    };
    for s in local_all {
        match &s.kind {
            S::Assignment { target, .. } => write(target),
            S::Assign { assignments } => {
                for a in assignments {
                    write(&a.target);
                }
            }
            S::Import { targets, .. } | S::Update { targets, .. } | S::Set { targets, .. } => {
                for t in targets {
                    write(t);
                }
            }
            S::Run { arguments, .. } | S::Publish { arguments, .. } => {
                for a in arguments {
                    if a.direction != ParameterDirection::Input {
                        write(&a.expression);
                    }
                }
            }
            _ => {}
        }
    }
    written.extend(repeat_vars.values().cloned());
    for s in local_all {
        match &s.kind {
            S::Do {
                loop_var: Some(name),
                ..
            } => {
                written.insert(name.name.to_ascii_lowercase());
            }
            S::Message { set_targets, .. } => {
                for name in set_targets {
                    written.insert(name.name.to_ascii_lowercase());
                }
            }
            _ => {}
        }
    }
    let mut owners: FxHashMap<_, _> = if fields.is_some() {
        FxHashMap::default()
    } else {
        flow.units()
            .iter()
            .flat_map(|cfg| {
                cfg.nodes()
                    .filter_map(move |(_, n)| n.stmt.map(|id| (id, cfg.id())))
            })
            .collect()
    };
    for s in local_all {
        let parameters = match &s.kind {
            S::Method { parameters, .. } | S::Constructor { parameters, .. } => {
                parameters.as_slice()
            }
            S::Property { set_parameters, .. } => set_parameters.as_slice(),
            _ => &[],
        };
        for cfg in flow.units_defined_by(s.id) {
            for param in parameters {
                owners.insert(param.id, cfg.id());
            }
        }
    }
    let declared: FxHashSet<_> = all
        .iter()
        .filter_map(|s| {
            let name = match &s.kind {
                S::VariableDeclaration { name, .. }
                | S::DefineParameter {
                    param_type: ParameterType::Variable { name, .. },
                    ..
                } => name,
                _ => return None,
            };
            Some((*owners.get(&s.id)?, name.name.to_ascii_lowercase()))
        })
        .collect();
    let mut nonlocal = FxHashSet::default();
    for s in local_all {
        let Some(&unit) = owners.get(&s.id) else {
            continue;
        };
        let mut record = |e: &Expression| {
            if let E::Identifier(i) = &e.kind {
                let name = i.name.to_ascii_lowercase();
                if !declared.contains(&(unit, name.clone())) {
                    nonlocal.insert(name);
                }
            }
        };
        match &s.kind {
            S::Assignment { target, .. } => record(target),
            S::Assign { assignments } => {
                for a in assignments {
                    record(&a.target);
                }
            }
            S::Import { targets, .. } | S::Update { targets, .. } | S::Set { targets, .. } => {
                for t in targets {
                    record(t);
                }
            }
            S::Run { arguments, .. } | S::Publish { arguments, .. } => {
                for a in arguments {
                    if a.direction != ParameterDirection::Input {
                        record(&a.expression);
                    }
                }
            }
            _ => {}
        }
    }
    let mut loosened = FxHashMap::default();
    for s in &all {
        for e in expressions(s) {
            walk(e, &mut |e| {
                if let Some(r) = loosen_not(e) {
                    loosened.insert(e.id, r);
                }
            });
        }
    }
    let mut out = Vec::new();
    let mut facts = Vec::new();
    let by_id: FxHashMap<NodeId, &Statement> = all.iter().map(|s| (s.id, *s)).collect();
    let mut by_unit: FxHashMap<UnitId, Vec<&Statement>> = FxHashMap::default();
    for s in local_all {
        if let Some(&unit) = owners.get(&s.id) {
            by_unit.entry(unit).or_default().push(*s);
        }
    }
    for cfg in flow.units() {
        let own_fields = unit_fields.as_ref().map(|u| u.get(&cfg.id()));
        if own_fields.is_some_and(|u| u.is_none()) {
            continue;
        }
        let statements: FxHashMap<_, _> = cfg
            .nodes()
            .filter_map(|(_, n)| n.stmt)
            .filter_map(|id| Some((id, *by_id.get(&id)?)))
            .collect();
        let mut p = Problem {
            statements,
            variables: Vec::new(),
            names: FxHashMap::default(),
            elided,
            repeat_vars: &repeat_vars,
            call_writes: Vec::new(),
            buffers: FxHashMap::default(),
            counted: FxHashMap::default(),
            loosened: &loosened,
            tracked: 0,
            fields_only: fields.is_some(),
            field_spans: FxHashMap::default(),
            effects: FxHashMap::default(),
            when_values: &when_values,
        };
        let locals = if fields.is_some() {
            &[][..]
        } else {
            by_unit.get(&cfg.id()).map_or(&[][..], Vec::as_slice)
        };
        for v in own_fields.flatten().iter().flat_map(|u| &u.vars) {
            p.variables.push(Variable {
                name: v.name.clone(),
                key: v.key.clone(),
                initial: FIELD,
                constant: None,
            });
        }
        for s in locals {
            let declaration = match &s.kind {
                S::VariableDeclaration {
                    name,
                    type_source,
                    initial_value,
                    is_shared,
                    is_new_shared,
                    is_new_global_shared,
                    extent,
                    ..
                } => {
                    let initial = if *is_shared
                        || *is_new_shared
                        || *is_new_global_shared
                        || extent.is_some()
                    {
                        UNKNOWN
                    } else {
                        initial_value.as_ref().map_or_else(
                            || default_value(type_source),
                            |e| p.eval(e, &Fact::new(Vec::new(), 0)),
                        )
                    };
                    Some((name, initial))
                }
                S::DefineParameter {
                    param_type:
                        ParameterType::Variable {
                            name, type_source, ..
                        },
                    direction,
                } => Some((
                    name,
                    if *direction == ParameterDirection::Output {
                        default_value(type_source)
                    } else {
                        UNKNOWN
                    },
                )),
                _ => None,
            };
            if let Some((name, initial)) = declaration {
                p.names
                    .insert(name.name.to_ascii_lowercase(), p.variables.len());
                p.variables.push(Variable {
                    name: name.name.clone(),
                    key: name.name.to_ascii_lowercase(),
                    initial,
                    constant: if written.contains(&name.name.to_ascii_lowercase()) {
                        None
                    } else {
                        match &s.kind {
                            S::VariableDeclaration {
                                type_source:
                                    TypeSource::Explicit(DataType::Integer | DataType::Int64),
                                initial_value,
                                ..
                            } if initial == KNOWN => match initial_value {
                                Some(e) => p.number(e),
                                None => Some(0),
                            },
                            _ => None,
                        }
                    },
                });
            }
        }
        if p.variables.is_empty() {
            continue;
        }
        // Tested locals first: a source records which of them must be `?`.
        let spans = own_fields.flatten().map(|u| &u.spans);
        let tested = tested_names(p.statements.values().copied(), spans);
        let (mut first, rest): (Vec<_>, Vec<_>) = std::mem::take(&mut p.variables)
            .into_iter()
            .partition(|v| tested.contains(&v.key));
        p.tracked = first.len().min(TRACKED_LIMIT);
        first.extend(rest);
        let indexed: FxHashMap<String, usize> = first
            .iter()
            .enumerate()
            .map(|(i, v)| (v.key.clone(), i))
            .collect();
        p.variables = first;
        if let Some(uf) = own_fields.flatten() {
            p.field_spans = uf
                .spans
                .iter()
                .map(|(&span, key)| (span, indexed[key]))
                .collect();
            p.effects = field_effects(&p, uf, &indexed, fields.map(|i| i.scopes));
            p.call_writes = uf
                .vars
                .iter()
                .filter(|v| v.shared)
                .map(|v| indexed[&v.key])
                .collect();
        } else {
            p.names = indexed;
        }
        let mut buffers = FxHashMap::default();
        for s in p.statements.values() {
            for e in expressions(s) {
                walk(e, &mut |e| {
                    if let Some(b) = available_buffer(e) {
                        let next = buffers.len();
                        buffers.entry(b.to_ascii_lowercase()).or_insert(next);
                    }
                });
            }
        }
        p.buffers = buffers;
        for (n, node) in cfg.nodes() {
            if node.kind == NodeKind::Head
                && let Some(s) = node.stmt.and_then(|id| p.statements.get(&id))
                && p.runs_body(s)
            {
                let next = p.counted.len();
                p.counted.insert(n, next);
            }
        }
        if fields.is_none() {
            p.call_writes = p
                .names
                .iter()
                .filter(|(name, _)| nonlocal.contains(*name))
                .map(|(_, &i)| i)
                .collect();
        }
        let Ok(solution) = solve_bounded(
            cfg,
            &p,
            Limits {
                visits: default_budget(cfg),
                work: WORK_LIMIT,
            },
        ) else {
            facts.push(UnitValues {
                unit: cfg.id(),
                names: p.names,
                before: Vec::new(),
                exhausted: true,
            });
            continue;
        };
        inspect(cfg, &p, &solution);
        let reachable = cfg.reachable();
        for (n, node) in cfg.nodes() {
            if !reachable.contains(n.index())
                || !matches!(node.kind, NodeKind::Stmt | NodeKind::Head)
            {
                continue;
            }
            let Some(s) = node.stmt.and_then(|id| p.statements.get(&id)) else {
                continue;
            };
            let f = solution.before(n);
            if f.dead() {
                continue;
            }
            if let Some(c) = p.condition(s) {
                p.use_var(c, f, cfg.id(), false, &mut out);
                p.sinks(c, f, cfg.id(), false, &mut out);
            }
            match &s.kind {
                S::Assignment { value, .. } => p.sinks(value, f, cfg.id(), true, &mut out),
                S::Assign { assignments } => {
                    let mut f = f.clone();
                    for a in assignments {
                        if !p.when_values.contains(&a.value.id) {
                            p.sinks(&a.value, &f, cfg.id(), true, &mut out);
                        }
                        p.assign(&a.target, &a.value, &mut f);
                    }
                }
                S::Display { items, .. } => {
                    for i in items {
                        match &i.when_condition {
                            Some(w) if p.fields_only => {
                                let mut shown = f.clone();
                                p.narrow(w, true, &mut shown);
                                p.sinks(&i.expression, &shown, cfg.id(), true, &mut out);
                            }
                            _ => p.sinks(&i.expression, f, cfg.id(), true, &mut out),
                        }
                    }
                }
                S::Message { items, .. } | S::Put { items, .. } | S::Export { items, .. } => {
                    for e in items {
                        p.sinks(e, f, cfg.id(), true, &mut out);
                    }
                }
                _ => {}
            }
        }
        if fields.is_some() {
            continue;
        }
        facts.push(UnitValues {
            unit: cfg.id(),
            names: p.names,
            before: cfg
                .nodes()
                .map(|(n, _)| solution.before(n).values.clone())
                .collect(),
            exhausted: false,
        });
    }
    out.sort_by_key(|u| (u.at.start, u.at.end, u.propagation));
    out.dedup_by_key(|u| (u.at.start, u.at.end, u.propagation));
    (out, facts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_transfer_is_monotone() {
        let p = Problem {
            statements: FxHashMap::default(),
            variables: vec![
                Variable {
                    name: "a".into(),
                    key: "a".into(),
                    initial: UNKNOWN,
                    constant: None,
                },
                Variable {
                    name: "b".into(),
                    key: "b".into(),
                    initial: UNKNOWN,
                    constant: None,
                },
            ],
            names: [("a".into(), 0), ("b".into(), 1)].into_iter().collect(),
            elided: &[],
            repeat_vars: &FxHashMap::default(),
            call_writes: Vec::new(),
            buffers: FxHashMap::default(),
            counted: FxHashMap::default(),
            loosened: &FxHashMap::default(),
            tracked: 2,
            fields_only: false,
            field_spans: FxHashMap::default(),
            effects: FxHashMap::default(),
            when_values: &FxHashSet::default(),
        };
        let states = [
            0,
            KNOWN,
            QUESTION,
            KNOWN | QUESTION,
            UNKNOWN,
            UNKNOWN | KNOWN,
            UNKNOWN | QUESTION,
            UNKNOWN | KNOWN | QUESTION,
        ];
        for source in [
            "a + b",
            "a - b",
            "a * b",
            "a / b",
            "a MODULO b",
            "-a",
            "LENGTH(a)",
            "INTEGER(a)",
            "STRING(a)",
            "IF a <> ? THEN a ELSE b",
            "IF a THEN b ELSE a",
            "a > b",
            "a AND b",
            "a = b",
        ] {
            let tokens = oxabl_lexer::tokenize(source);
            let expression = oxabl_parser::Parser::new(&tokens, source)
                .parse_expression()
                .expect("expression");
            for a in states {
                for b in states {
                    for c in states {
                        for d in states {
                            if a | c != c || b | d != d {
                                continue;
                            }
                            // A fact is unreachable in every local or in none.
                            if (a == 0) != (b == 0) || (c == 0) != (d == 0) {
                                continue;
                            }
                            let left = p.eval(&expression, &Fact::new(vec![a, b], 2));
                            let right = p.eval(&expression, &Fact::new(vec![c, d], 2));
                            assert_eq!(left | right, right, "{source}: [{a},{b}] <= [{c},{d}]");
                        }
                    }
                }
            }
        }
    }

    fn parse(source: &str) -> (Vec<oxabl_lexer::Token>, oxabl_parser::Program, FileFlow) {
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        let flow = oxabl_flow::build_file_tokens(
            &program.statements,
            source,
            &tokens,
            &Default::default(),
        );
        (tokens, program, flow)
    }

    #[test]
    fn random_programs_reach_a_fixed_point() {
        let mut programs = crate::fuzz::Programs::new(0x5eed);
        for k in 0..300 {
            let source = programs.program();
            let (tokens, program, flow) = parse(&source);
            let (_, facts) = analyze(&source, &tokens, &program.statements, &flow, &[]);
            assert!(
                facts.iter().all(|f| !f.exhausted),
                "program {k} spent its budget:\n{source}"
            );
        }
    }

    #[test]
    fn statement_and_edge_transfers_are_monotone() {
        let mut programs = crate::fuzz::Programs::new(0x6d6f);
        for k in 0..100 {
            let source = programs.program();
            let (tokens, program, flow) = parse(&source);
            let (statements, elided) = (&program.statements, &[]);
            analyze_inspect(
                &source,
                &tokens,
                statements,
                &flow,
                elided,
                None,
                &mut |cfg, p, solution| {
                    let mut pool: Vec<&Fact> = Vec::new();
                    for (n, _) in cfg.nodes() {
                        let f = solution.before(n);
                        if !f.dead() && !pool.contains(&f) && pool.len() < 6 {
                            pool.push(f);
                        }
                    }
                    for (n, node) in cfg.nodes() {
                        if !matches!(node.kind, NodeKind::Stmt | NodeKind::Head) {
                            continue;
                        }
                        for e in cfg.successors(n) {
                            for small in &pool {
                                for other in &pool {
                                    let mut large = (*small).clone();
                                    large.join(other);
                                    let (mut a, mut b) = ((*small).clone(), large.clone());
                                    p.transfer_edge(cfg, n, e, &mut a);
                                    p.transfer_edge(cfg, n, e, &mut b);
                                    assert!(
                                        a.leq(&b),
                                        "program {k}, node {} {:?} by {:?}:\n{small:?}\n<= {large:?}\ngives {a:?}\nnot <= {b:?}\n{source}",
                                        n.index(),
                                        cfg.node(n).kind,
                                        e.kind,
                                    );
                                }
                            }
                        }
                    }
                },
            );
        }
    }
}
