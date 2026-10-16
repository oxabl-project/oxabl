//! Local unknown-value facts. Low-confidence operations produce Unknown.
use oxabl_ast::{
    DataType, Expression, ExpressionKind as E, Literal, NodeId, OnAction, OnKind,
    ParameterDirection, ParameterType, Span, Statement, StatementKind as S, TypeSource,
};
use oxabl_flow::dataflow::{Analysis, Direction, solve};
use oxabl_flow::{Cfg, Edge, EdgeKind, FileFlow, NodeIx, NodeKind, UnitId};
use rustc_hash::{FxHashMap, FxHashSet};

#[path = "value_fact.rs"]
mod fact;
use fact::{Fact, Required};

const KNOWN: u8 = 1;
const QUESTION: u8 = 2;
// Keep source evidence separate from paths whose value is not known.
const OPAQUE: u8 = 4;
const UNKNOWN: u8 = OPAQUE;

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
}
impl UnitValues {
    pub fn before(&self, node: NodeIx, name: &str) -> Option<ValueState> {
        let index = lookup(&self.names, name)?;
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
    initial: u8,
    constant: Option<i64>,
}
struct Problem<'a> {
    statements: FxHashMap<NodeId, &'a Statement>,
    variables: Vec<Variable>,
    names: FxHashMap<String, usize>,
    elided: &'a [u32],
    call_writes: Vec<usize>,
    /// Buffers named by `AVAILABLE` in the unit, for correlated tests.
    buffers: FxHashMap<String, usize>,
    /// Comparisons whose left operand the parser read as `NOT a`.
    loosened: &'a FxHashMap<NodeId, Expression>,
    /// The leading variables that some test reads (section 9.9).
    tracked: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Builtin {
    Propagates,
    Absorbs,
    Unknown,
}

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

/// Locals that a condition, comparison or loop limit reads.
fn tested_names<'a>(statements: impl Iterator<Item = &'a Statement>) -> FxHashSet<String> {
    let mut out = FxHashSet::default();
    let mut collect = |e: &Expression| {
        walk(e, &mut |e| {
            if let E::Identifier(i) = &e.kind {
                out.insert(i.name.to_ascii_lowercase());
            }
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
        Some((Fact::buffer(self.tracked, k), false))
    }
    fn forget_buffers(&self, f: &mut Fact) {
        f.drop_atoms_from(Fact::buffer(self.tracked, 0));
    }
    fn variable(&self, e: &Expression) -> Option<usize> {
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
            E::Identifier(_) => self
                .variable(e)
                .map_or(UNKNOWN, |i| f.get(i).copied().unwrap_or(UNKNOWN)),
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                if matches!(e.kind, E::Divide(..)) && self.number(b) == Some(0) {
                    return QUESTION;
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
            | E::Modulo(a, b)
                if !matches!(e.kind, E::Divide(..)) || self.number(b) != Some(0) =>
            {
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
    fn prove_known(&self, e: &Expression, f: &mut Fact) {
        if f.dead() {
            return;
        }
        if self.eval(e, f) == QUESTION {
            f.fill(0);
            return;
        }
        if let Some(i) = self.variable(e) {
            f.known(i);
            return;
        }
        match &e.kind {
            E::Add(a, b)
            | E::Minus(a, b)
            | E::Multiply(a, b)
            | E::Divide(a, b)
            | E::Modulo(a, b) => {
                self.prove_known(a, f);
                self.prove_known(b, f);
            }
            E::Negate(a) => self.prove_known(a, f),
            E::FunctionCall { name, arguments } if propagates(&name.name) => {
                for a in arguments {
                    self.prove_known(a, f);
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
                self.prove_known(a, f);
                self.prove_known(b, f);
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
                if var.is_none()
                    && let (Some(a), Some(b)) =
                        (self.nullable_variable(a), self.nullable_variable(b))
                    && equal
                {
                    f.link(a, b);
                    if f[a] == KNOWN {
                        f.known(b);
                    } else if f[a] == QUESTION {
                        f.question(b);
                    }
                    if !f.dead() {
                        if f[b] == KNOWN {
                            f.known(a);
                        } else if f[b] == QUESTION {
                            f.question(a);
                        }
                    }
                }
                if let Some(i) = var {
                    if equal {
                        if f[i] & (QUESTION | OPAQUE) != 0 {
                            f.question(i);
                        } else {
                            f.fill(0);
                        }
                    } else {
                        f.known(i);
                    }
                }
            }
            E::Identifier(_) if yes => {
                if let Some(i) = self.variable(e) {
                    if f[i] == QUESTION {
                        f.fill(0);
                    } else {
                        f.known(i);
                    }
                }
            }
            E::And(a, b) if yes => {
                self.narrow(a, true, f);
                self.narrow(b, true, f);
            }
            E::Or(a, b) if !yes => {
                self.narrow(a, false, f);
                self.narrow(b, false, f);
            }
            // A false AND with one side true: the other is false (OR dually).
            E::And(a, b) | E::Or(a, b) => {
                let side = if yes { 2 } else { 1 };
                if self.truth(a, f, &Required::new()) == side {
                    self.narrow(b, yes, f);
                } else if self.truth(b, f, &Required::new()) == side {
                    self.narrow(a, yes, f);
                }
            }
            E::Not(a) => self.narrow(a, !yes, f),
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
            // An unseen callee may change variables passed by reference.
            S::Do {
                loop_var: Some(name),
                ..
            } => {
                if let Some(i) = lookup(&self.names, &name.name) {
                    f.forget(i, UNKNOWN);
                }
            }
            S::Import { targets, .. } | S::Update { targets, .. } | S::Set { targets, .. } => {
                for target in targets {
                    if let Some(i) = self.variable(target) {
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
            S::Run { arguments, .. } => {
                for &i in &self.call_writes {
                    f.forget(i, UNKNOWN);
                }
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
        // A `?` limit ends a counted loop at once (V6, V8); this only removes sources.
        if let (
            EdgeKind::LoopBody,
            S::Do {
                from, to: Some(to), ..
            },
        ) = (e.kind, &s.kind)
        {
            if let Some(from) = from {
                self.prove_known(from, f);
            }
            self.prove_known(to, f);
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

pub(crate) fn analyze(
    program: &[Statement],
    flow: &FileFlow,
    elided: &[u32],
) -> (Vec<ValueUse>, Vec<UnitValues>) {
    let mut all = Vec::new();
    for s in program {
        children(s, &mut all);
    }
    let mut written = FxHashSet::default();
    let mut write = |e: &Expression| {
        if let E::Identifier(i) = &e.kind {
            written.insert(i.name.to_ascii_lowercase());
        }
    };
    for s in &all {
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
            S::Run { arguments, .. } => {
                for a in arguments {
                    if a.direction != ParameterDirection::Input {
                        write(&a.expression);
                    }
                }
            }
            _ => {}
        }
    }
    for s in &all {
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
    let mut owners: FxHashMap<_, _> = flow
        .units()
        .iter()
        .flat_map(|cfg| {
            cfg.nodes()
                .filter_map(move |(_, n)| n.stmt.map(|id| (id, cfg.id())))
        })
        .collect();
    for s in &all {
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
    for s in &all {
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
            S::Run { arguments, .. } => {
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
    for s in &all {
        if let Some(&unit) = owners.get(&s.id) {
            by_unit.entry(unit).or_default().push(*s);
        }
    }
    for cfg in flow.units() {
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
            call_writes: Vec::new(),
            buffers: FxHashMap::default(),
            loosened: &loosened,
            tracked: 0,
        };
        for s in by_unit.get(&cfg.id()).map_or(&[][..], Vec::as_slice) {
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
        let tested = tested_names(p.statements.values().copied());
        let (mut first, rest): (Vec<_>, Vec<_>) = std::mem::take(&mut p.variables)
            .into_iter()
            .partition(|v| tested.contains(&v.name.to_ascii_lowercase()));
        p.tracked = first.len().min(TRACKED_LIMIT);
        first.extend(rest);
        p.names = first
            .iter()
            .enumerate()
            .map(|(i, v)| (v.name.to_ascii_lowercase(), i))
            .collect();
        p.variables = first;
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
        p.call_writes = p
            .names
            .iter()
            .filter(|(name, _)| nonlocal.contains(*name))
            .map(|(_, &i)| i)
            .collect();
        let solution = solve(cfg, &p);
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
                        p.sinks(&a.value, &f, cfg.id(), true, &mut out);
                        p.assign(&a.target, &a.value, &mut f);
                    }
                }
                S::Display { items, .. } => {
                    for i in items {
                        p.sinks(&i.expression, f, cfg.id(), true, &mut out);
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
        facts.push(UnitValues {
            unit: cfg.id(),
            names: p.names,
            before: cfg
                .nodes()
                .map(|(n, _)| solution.before(n).values.clone())
                .collect(),
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
                    initial: UNKNOWN,
                    constant: None,
                },
                Variable {
                    name: "b".into(),
                    initial: UNKNOWN,
                    constant: None,
                },
            ],
            names: [("a".into(), 0), ("b".into(), 1)].into_iter().collect(),
            elided: &[],
            call_writes: Vec::new(),
            buffers: FxHashMap::default(),
            loosened: &FxHashMap::default(),
            tracked: 2,
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
}
