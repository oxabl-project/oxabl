//! One unit: its slots, what each node writes, the guards on its edges, and
//! the solve.

use oxabl_ast::{
    AccessModifier, Expression, ExpressionKind as E, HandleParamKind, NodeId, ParameterDirection,
    ParameterType, RunTarget, Span, Statement, StatementKind as S,
};
use oxabl_common::ParameterSources;
use oxabl_flow::dataflow::{Analysis, Direction, Limits, Solution, default_budget, solve_bounded};
use oxabl_flow::provenance::Provenance;
use oxabl_flow::{Cfg, Edge, EdgeKind, NodeIx, NodeKind, Unit, UnitKind};
use oxabl_lexer::Kind;
use rustc_hash::{FxHashMap, FxHashSet};

use super::super::literal::{StatementText, is_constant};
use super::value::{Compiler, Origins, Slots, Summaries, Tables, Value};
use super::{ALL, Options};

/// The most work the solve of one unit may do. Past it the unit is Unknown.
const WORK_LIMIT: usize = 2_000_000;

/// One write: `slot` takes `value`.
#[derive(Debug, Clone)]
pub(super) struct Op {
    pub slot: u32,
    pub value: Value,
    /// A strong update replaces the labels; a weak one adds to them.
    pub strong: bool,
    /// The statement, or the `ASSIGN` pair, that writes.
    pub span: Span,
}

/// What the edges out of an `IF` or `CASE` node clean.
#[derive(Debug, Clone, Default)]
pub(super) struct Guard {
    pub on_true: Vec<(u32, u32)>,
    pub on_false: Vec<(u32, u32)>,
    /// Per `WHEN` branch.
    pub when: Vec<Vec<(u32, u32)>>,
}

/// A unit compiled for the solve.
#[derive(Debug, Default)]
pub(super) struct UnitIr {
    pub slots: Slots,
    pub declared: FxHashSet<String>,
    /// Per node.
    pub ops: Vec<Vec<Op>>,
    pub guards: FxHashMap<u32, Guard>,
    /// A main block: the variables it declares are file-level for the units
    /// inside it.
    pub main: bool,
}

/// The statements of a file by id.
pub(super) fn statements(program: &[Statement]) -> FxHashMap<NodeId, &Statement> {
    fn walk<'a>(list: &'a [Statement], out: &mut FxHashMap<NodeId, &'a Statement>) {
        for s in list {
            out.insert(s.id, s);
            match &s.kind {
                S::Procedure { body, .. }
                | S::Function { body, .. }
                | S::Class { body, .. }
                | S::Interface { body, .. }
                | S::Destructor { body }
                | S::Finally { body }
                | S::Catch { body, .. }
                | S::Block(body)
                | S::VariableDeclarationList(body)
                | S::Do { body, .. }
                | S::Repeat { body, .. }
                | S::ForEach { body, .. } => walk(body, out),
                S::Method {
                    parameters, body, ..
                }
                | S::Constructor {
                    parameters, body, ..
                } => {
                    walk(parameters, out);
                    walk(body, out);
                }
                S::Property {
                    get_body,
                    set_body,
                    set_parameters,
                    ..
                } => {
                    walk(set_parameters, out);
                    for b in [get_body, set_body].into_iter().flatten() {
                        walk(b, out);
                    }
                }
                S::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    walk(std::slice::from_ref(then_branch), out);
                    if let Some(e) = else_branch {
                        walk(std::slice::from_ref(e), out);
                    }
                }
                S::Case {
                    when_branches,
                    otherwise,
                    ..
                } => {
                    for w in when_branches {
                        walk(&w.body, out);
                    }
                    if let Some(o) = otherwise {
                        walk(o, out);
                    }
                }
                S::PreprocIf(p) => {
                    walk(&p.then_branch, out);
                    for (_, b) in &p.elseif_branches {
                        walk(b, out);
                    }
                    if let Some(e) = &p.else_branch {
                        walk(e, out);
                    }
                }
                S::Label { body, .. } => walk(std::slice::from_ref(body), out),
                S::On {
                    kind:
                        oxabl_ast::OnKind::UiEvent { action, .. }
                        | oxabl_ast::OnKind::DbEvent { action, .. },
                } => {
                    if let oxabl_ast::OnAction::Block(b) = action {
                        walk(std::slice::from_ref(b), out);
                    }
                }
                _ => {}
            }
        }
    }
    let mut out = FxHashMap::default();
    walk(program, &mut out);
    out
}

/// The temp-tables of a file and what each buffer is for.
pub(super) fn tables(stmts: &FxHashMap<NodeId, &Statement>) -> Tables {
    let mut t = Tables::default();
    for s in stmts.values() {
        match &s.kind {
            S::DefineTempTable { name, .. } => {
                t.temp_tables.insert(name.name.to_ascii_lowercase());
            }
            S::DefineBuffer { name, target, .. } => {
                let table = match target {
                    oxabl_ast::BufferTarget::Table(i) | oxabl_ast::BufferTarget::TempTable(i) => i,
                };
                t.buffers.insert(
                    name.name.to_ascii_lowercase(),
                    table.name.to_ascii_lowercase(),
                );
            }
            S::DefineParameter {
                param_type: ParameterType::Buffer { name, target },
                ..
            } => {
                t.buffers.insert(
                    name.name.to_ascii_lowercase(),
                    target.name.to_ascii_lowercase(),
                );
            }
            _ => {}
        }
    }
    t
}

/// The parameters a unit's statement declares outside its body.
fn entry_parameters<'a>(unit: &Unit, stmt: Option<&'a Statement>) -> &'a [Statement] {
    match (unit.kind, stmt.map(|s| &s.kind)) {
        (UnitKind::Method, Some(S::Method { parameters, .. }))
        | (UnitKind::Constructor, Some(S::Constructor { parameters, .. }))
        | (
            UnitKind::PropertySet,
            Some(S::Property {
                set_parameters: parameters,
                ..
            }),
        ) => parameters,
        _ => &[],
    }
}

/// Whether code outside the file can call the unit with its own arguments.
fn is_external(unit: &Unit, stmt: Option<&Statement>, source: &str, options: &Options) -> bool {
    match options.parameters {
        ParameterSources::All => return true,
        ParameterSources::None => return false,
        ParameterSources::External => {}
    }
    let not_private = |a: &AccessModifier| *a != AccessModifier::Private;
    match (unit.kind, stmt.map(|s| &s.kind)) {
        (UnitKind::Main, _) => true,
        (UnitKind::Procedure | UnitKind::Function, Some(_)) => {
            !header_is_private(source, unit.span)
        }
        (UnitKind::Method, Some(S::Method { access, .. }))
        | (UnitKind::Constructor, Some(S::Constructor { access, .. }))
        | (UnitKind::PropertySet, Some(S::Property { access, .. })) => not_private(access),
        _ => false,
    }
}

/// Whether the header of a `PROCEDURE` or `FUNCTION` (up to its `(`, `:` or
/// `.`) says `PRIVATE`.
fn header_is_private(source: &str, span: Span) -> bool {
    let Some(st) = StatementText::over(source, span) else {
        return false;
    };
    (0..st.len())
        .take_while(|&i| {
            !st.is(i, Kind::LeftParen) && !st.is(i, Kind::Colon) && !st.is(i, Kind::Period)
        })
        .any(|i| st.is_word(i, "PRIVATE"))
}

/// The direction keyword written just before the argument at `start`.
fn written_direction(source: &str, start: u32) -> Option<ParameterDirection> {
    let before = source.get(..start as usize)?.trim_end();
    let word_start = before
        .rfind(|c: char| !(c.is_ascii_alphanumeric() || c == '-' || c == '_'))
        .map_or(0, |i| i + 1);
    let word = &before[word_start..];
    if word.eq_ignore_ascii_case("OUTPUT") {
        Some(ParameterDirection::Output)
    } else if word.eq_ignore_ascii_case("INPUT-OUTPUT") {
        Some(ParameterDirection::InputOutput)
    } else {
        None
    }
}

/// The names a unit declares: variables, parameters, `CATCH` variables.
fn declared(
    cfg: &Cfg,
    stmts: &FxHashMap<NodeId, &Statement>,
    entry: &[Statement],
) -> FxHashSet<String> {
    let mut out = FxHashSet::default();
    let mut add = |s: &Statement| match &s.kind {
        S::VariableDeclaration { name, .. } => {
            out.insert(name.name.to_ascii_lowercase());
        }
        S::DefineParameter {
            param_type: ParameterType::Variable { name, .. } | ParameterType::Handle { name, .. },
            ..
        } => {
            out.insert(name.name.to_ascii_lowercase());
        }
        S::Catch { error_var, .. } => {
            out.insert(error_var.name.to_ascii_lowercase());
        }
        _ => {}
    };
    entry.iter().for_each(&mut add);
    for (_, n) in cfg.nodes() {
        if let Some(s) = n.stmt.and_then(|id| stmts.get(&id)) {
            add(s);
        }
    }
    out
}

/// Builds one unit.
struct Builder<'c, 'a> {
    c: Compiler<'c>,
    source: &'a str,
    external: bool,
}

impl Builder<'_, '_> {
    fn set(&mut self, out: &mut Vec<Op>, target: &Expression, value: Value, span: Span) {
        for (slot, strong) in self.c.target(target) {
            out.push(Op {
                slot,
                value: value.clone(),
                strong,
                span,
            });
        }
    }

    fn parameter(&mut self, out: &mut Vec<Op>, stmt: &Statement) {
        let S::DefineParameter {
            direction,
            param_type,
        } = &stmt.kind
        else {
            return;
        };
        let input = matches!(
            direction,
            ParameterDirection::Input | ParameterDirection::InputOutput
        ) && self.external;
        match param_type {
            ParameterType::Variable { name, .. }
            | ParameterType::Handle {
                kind: HandleParamKind::TableHandle | HandleParamKind::DatasetHandle,
                name,
                ..
            } => {
                let Some(slot) = self.c.var_slot(&name.name) else {
                    return;
                };
                let value = if input {
                    Value::origin(
                        self.c
                            .origins
                            .add(format!("input parameter `{}`", name.name), stmt.span),
                    )
                } else {
                    Value::default()
                };
                out.push(Op {
                    slot,
                    value,
                    strong: true,
                    span: stmt.span,
                });
            }
            ParameterType::Handle {
                kind: HandleParamKind::Table,
                name,
                ..
            } if input => {
                if let Some(slot) = self.c.table_slot(&name.name) {
                    let id = self
                        .c
                        .origins
                        .add(format!("input table `{}`", name.name), stmt.span);
                    out.push(Op {
                        slot,
                        value: Value::origin(id),
                        strong: false,
                        span: stmt.span,
                    });
                }
            }
            _ => {}
        }
    }

    /// `OUTPUT` arguments of calls in `e`: each takes the receiver and the
    /// other arguments.
    fn output_arguments(&mut self, out: &mut Vec<Op>, e: &Expression, span: Span) {
        let (object, arguments) = match &e.kind {
            E::FunctionCall { arguments, .. } => (None, arguments),
            E::MethodCall {
                object, arguments, ..
            } => (Some(object), arguments),
            _ => {
                for_each_child(e, &mut |c| self.output_arguments(out, c, span));
                return;
            }
        };
        for a in arguments {
            self.output_arguments(out, a, span);
        }
        let outputs: Vec<&Expression> = arguments
            .iter()
            .filter(|a| written_direction(self.source, a.span.start).is_some())
            .collect();
        if outputs.is_empty() {
            return;
        }
        let mut inputs = Vec::new();
        if let Some(o) = object {
            inputs.push(self.c.expr(o));
        }
        for a in arguments {
            if written_direction(self.source, a.span.start) != Some(ParameterDirection::Output) {
                inputs.push(self.c.expr(a));
            }
        }
        let value = Value::union(inputs);
        for o in outputs {
            self.set(out, o, value.clone(), span);
        }
    }

    /// A socket `READ(memptr, ...)` fills its first argument.
    fn socket_read(&mut self, out: &mut Vec<Op>, e: &Expression, span: Span) {
        if let E::MethodCall {
            method, arguments, ..
        } = &e.kind
            && method.name.eq_ignore_ascii_case("READ")
            && let Some(first) = arguments.first()
        {
            let id = self.c.origins.add("socket read `READ`".to_string(), e.span);
            self.set(out, first, Value::origin(id), span);
        }
    }

    fn statement(&mut self, node: &oxabl_flow::Node, s: &Statement) -> (Vec<Op>, Option<Guard>) {
        let mut out = Vec::new();
        let mut guard = None;
        match (&s.kind, node.kind) {
            (S::VariableDeclaration { initial_value, .. }, _) => {
                if let S::VariableDeclaration { name, .. } = &s.kind
                    && let Some(slot) = self.c.var_slot(&name.name)
                {
                    let value = initial_value
                        .as_ref()
                        .map(|v| self.c.expr(v))
                        .unwrap_or_default();
                    out.push(Op {
                        slot,
                        value,
                        strong: true,
                        span: s.span,
                    });
                }
            }
            (S::DefineParameter { .. }, _) => self.parameter(&mut out, s),
            (S::Assignment { target, value }, _) => {
                self.output_arguments(&mut out, value, s.span);
                let v = self.c.expr(value);
                self.set(&mut out, target, v, s.span);
            }
            (S::Assign { assignments }, _) => {
                for pair in assignments {
                    let span = Span {
                        start: pair.target.span.start.min(pair.value.span.start),
                        end: pair.value.span.end.max(pair.target.span.end),
                    };
                    let span = if span.start < span.end { span } else { s.span };
                    self.output_arguments(&mut out, &pair.value, span);
                    let v = self.c.expr(&pair.value);
                    self.set(&mut out, &pair.target, v, span);
                }
            }
            (S::ExpressionStatement(e), _) => {
                self.output_arguments(&mut out, e, s.span);
                self.socket_read(&mut out, e, s.span);
            }
            (S::Return(Some(e)), _) => self.output_arguments(&mut out, e, s.span),
            (
                S::Run {
                    target,
                    arguments,
                    in_handle,
                    ..
                },
                _,
            ) => {
                let mut inputs = Vec::new();
                if let RunTarget::Dynamic(t) = target {
                    inputs.push(self.c.expr(t));
                }
                if let Some(h) = in_handle {
                    inputs.push(self.c.expr(h));
                }
                for a in arguments {
                    if a.direction != ParameterDirection::Output {
                        inputs.push(self.c.expr(&a.expression));
                    }
                }
                let value = Value::union(inputs);
                for a in arguments {
                    if matches!(
                        a.direction,
                        ParameterDirection::Output | ParameterDirection::InputOutput
                    ) {
                        self.set(&mut out, &a.expression, value.clone(), s.span);
                    }
                }
            }
            (S::Import { targets, .. }, _) => {
                let id = self.c.origins.add("file read `IMPORT`".to_string(), s.span);
                for t in targets {
                    self.read_into(&mut out, t, id, s.span);
                }
            }
            (S::Set { targets, .. } | S::Update { targets, .. }, _) => {
                let word = if matches!(s.kind, S::Set { .. }) {
                    "SET"
                } else {
                    "UPDATE"
                };
                let id = self.c.origins.add(format!("user input `{word}`"), s.span);
                for t in targets {
                    self.read_into(&mut out, t, id, s.span);
                }
            }
            (S::Message { set_targets, .. }, _) if !set_targets.is_empty() => {
                let id = self
                    .c
                    .origins
                    .add("user input `MESSAGE ... UPDATE`".to_string(), s.span);
                for t in set_targets {
                    if let Some(slot) = self.c.var_slot(&t.name) {
                        out.push(Op {
                            slot,
                            value: Value::origin(id),
                            strong: true,
                            span: s.span,
                        });
                    }
                }
            }
            (S::Skipped { .. }, _) => self.copy_lob(&mut out, s),
            (
                S::Do {
                    loop_var: Some(v),
                    from: Some(from),
                    ..
                },
                NodeKind::Head,
            ) => {
                if let Some(slot) = self.c.var_slot(&v.name) {
                    let value = self.c.expr(from).cleared(super::value::NUMERIC_SAFE);
                    out.push(Op {
                        slot,
                        value,
                        strong: true,
                        span: node.span,
                    });
                }
            }
            (S::Catch { error_var, .. }, NodeKind::Head) => {
                if let Some(slot) = self.c.var_slot(&error_var.name) {
                    out.push(Op {
                        slot,
                        value: Value::default(),
                        strong: true,
                        span: node.span,
                    });
                }
            }
            (S::If { condition, .. }, NodeKind::Stmt) => {
                self.output_arguments(&mut out, condition, s.span);
                guard = Some(Guard {
                    on_true: self.c.guard(condition, true),
                    on_false: self.c.guard(condition, false),
                    when: Vec::new(),
                });
            }
            (
                S::Case {
                    expression,
                    when_branches,
                    ..
                },
                NodeKind::Stmt,
            ) => {
                let when = when_branches
                    .iter()
                    .map(|w| {
                        if w.values.iter().all(is_constant) {
                            self.c
                                .guarded(expression)
                                .map(|slot| (slot, ALL))
                                .into_iter()
                                .collect()
                        } else {
                            Vec::new()
                        }
                    })
                    .collect();
                guard = Some(Guard {
                    when,
                    ..Guard::default()
                });
            }
            _ => {}
        }
        (out, guard)
    }

    /// `IMPORT` or `SET` into `t`: a field, a variable, or a whole record.
    fn read_into(&mut self, out: &mut Vec<Op>, t: &Expression, id: u32, span: Span) {
        if let E::Identifier(i) = &t.kind
            && self
                .c
                .tables
                .temp_tables
                .contains(&self.c.tables.table_of(&i.name))
            && let Some(slot) = self.c.table_slot(&i.name)
        {
            out.push(Op {
                slot,
                value: Value::origin(id),
                strong: false,
                span,
            });
            return;
        }
        self.set(out, t, Value::origin(id), span);
    }

    /// `COPY-LOB [FROM] [FILE] src TO [OBJECT] target`.
    fn copy_lob(&mut self, out: &mut Vec<Op>, s: &Statement) {
        let Some(st) = StatementText::of(self.source, s) else {
            return;
        };
        if !st.is_word(0, "COPY-LOB") {
            return;
        }
        let mut depth = 0usize;
        let Some(to) = (1..st.len()).find(|&i| {
            match st.token(i).kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => depth = depth.saturating_sub(1),
                _ => {}
            }
            depth == 0 && st.is_word(i, "TO")
        }) else {
            return;
        };
        let mut from = 1;
        if st.is_word(from, "FROM") {
            from += 1;
        }
        let value = if st.is_word(from, "FILE") {
            Value::origin(
                self.c
                    .origins
                    .add("file read `COPY-LOB FROM FILE`".to_string(), s.span),
            )
        } else {
            if st.is_word(from, "OBJECT") {
                from += 1;
            }
            self.c.tokens(&st, from..to)
        };
        let mut t = to + 1;
        if st.is_word(t, "OBJECT") {
            t += 1;
        }
        if t >= st.len() || st.is_word(t, "FILE") {
            return;
        }
        let slot = if st.is(t + 1, Kind::Period) && t + 2 < st.len() {
            self.c.field_slot(st.word(t), st.word(t + 2))
        } else {
            self.c.var_slot(st.word(t))
        };
        if let Some(slot) = slot {
            out.push(Op {
                slot,
                value,
                strong: false,
                span: s.span,
            });
        }
    }
}

fn for_each_child<'e>(e: &'e Expression, f: &mut impl FnMut(&'e Expression)) {
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
            f(a);
            f(b);
        }
        E::Negate(a) | E::Not(a) | E::MemberAccess { object: a, .. } => f(a),
        E::FieldAccess { qualifier, .. } => f(qualifier),
        E::IfThenElse(c, a, b) => {
            f(c);
            f(a);
            f(b);
        }
        E::New { arguments, .. } => arguments.iter().for_each(f),
        _ => {}
    }
}

/// Compile one unit. Origins are added to `origins`.
pub(super) fn compile(
    cfg: &Cfg,
    stmts: &FxHashMap<NodeId, &Statement>,
    tables: &Tables,
    source: &str,
    options: &Options,
    origins: &mut Origins,
) -> UnitIr {
    let unit = cfg.unit();
    let unit_stmt = stmts.get(&unit.stmt).copied();
    let entry = entry_parameters(unit, unit_stmt);
    let declared = declared(cfg, stmts, entry);
    let mut slots = Slots::default();
    let mut ops = vec![Vec::new(); cfg.node_count()];
    let mut guards = FxHashMap::default();
    {
        let mut b = Builder {
            c: Compiler {
                options,
                tables,
                slots: &mut slots,
                origins,
                declared: &declared,
                frozen: None,
            },
            source,
            external: is_external(unit, unit_stmt, source, options),
        };
        for p in entry {
            b.parameter(&mut ops[cfg.entry().index()], p);
        }
        for (i, n) in cfg.nodes() {
            if !matches!(n.kind, NodeKind::Stmt | NodeKind::Head) {
                continue;
            }
            let Some(s) = n.stmt.and_then(|id| stmts.get(&id)) else {
                continue;
            };
            let (o, g) = b.statement(n, s);
            ops[i.index()].extend(o);
            if let Some(g) = g {
                guards.insert(i.index() as u32, g);
            }
        }
    }
    UnitIr {
        slots,
        declared,
        ops,
        guards,
        main: unit.kind == UnitKind::Main,
    }
}

/// The forward problem of one unit.
struct Problem<'a> {
    ir: &'a UnitIr,
    seeds: Provenance,
}

impl Problem<'_> {
    fn apply(&self, node: NodeIx, fact: &mut Provenance) {
        for op in &self.ir.ops[node.index()] {
            apply(op, fact);
        }
    }
}

/// Apply one write to `fact`.
pub(super) fn apply(op: &Op, fact: &mut Provenance) {
    let (labels, _) = op.value.eval(fact);
    if op.strong {
        fact.set(op.slot as usize, labels);
    } else {
        fact.add(op.slot as usize, &labels);
    }
}

impl Analysis for Problem<'_> {
    type Fact = Provenance;

    fn direction(&self) -> Direction {
        Direction::Forward
    }

    fn initial(&self, _: &Cfg) -> Provenance {
        Provenance::new(self.ir.slots.len())
    }

    fn boundary(&self, _: &Cfg) -> Provenance {
        self.seeds.clone()
    }

    fn join(&self, acc: &mut Provenance, other: &Provenance) {
        acc.join(other);
    }

    fn transfer(&self, _: &Cfg, node: NodeIx, fact: &mut Provenance) {
        self.apply(node, fact);
    }

    fn transfer_edge(&self, _: &Cfg, node: NodeIx, edge: &Edge, fact: &mut Provenance) {
        self.apply(node, fact);
        let Some(g) = self.ir.guards.get(&(node.index() as u32)) else {
            return;
        };
        let clears = match edge.kind {
            EdgeKind::True => &g.on_true,
            EdgeKind::False => &g.on_false,
            EdgeKind::When(n) => match g.when.get(n as usize) {
                Some(c) => c,
                None => return,
            },
            _ => return,
        };
        for &(slot, mask) in clears {
            fact.clear(slot as usize, mask);
        }
    }

    fn fact_cost(&self, fact: &Provenance) -> usize {
        fact.cost()
    }
}

/// Solve one unit with the file-level variables and fields at their
/// summaries. `None` when the unit costs more than its budget.
pub(super) fn solve(cfg: &Cfg, ir: &UnitIr, summaries: &Summaries) -> Option<Solution<Provenance>> {
    let mut seeds = Provenance::new(ir.slots.len());
    for (i, slot) in ir.slots.info.iter().enumerate() {
        if slot.global
            && let Some(l) = summaries.labels.get(&slot.key)
        {
            seeds.set(i, l.clone());
        }
    }
    let problem = Problem { ir, seeds };
    let limits = Limits {
        visits: default_budget(cfg),
        work: WORK_LIMIT,
    };
    solve_bounded(cfg, &problem, limits).ok()
}

/// Add what `ir` writes to file-level slots to `into`.
pub(super) fn contribute(ir: &UnitIr, solution: &Solution<Provenance>, into: &mut Summaries) {
    for (n, ops) in ir.ops.iter().enumerate() {
        for op in ops {
            let slot = &ir.slots.info[op.slot as usize];
            if !(slot.global || ir.main) {
                continue;
            }
            let labels = solution.after(NodeIx::from_index(n)).get(op.slot as usize);
            if labels.is_empty() {
                continue;
            }
            let entry = into.labels.entry(slot.key.clone()).or_default();
            oxabl_flow::provenance::merge(entry, labels);
            into.sites.entry(slot.key.clone()).or_insert(op.span);
        }
    }
}
