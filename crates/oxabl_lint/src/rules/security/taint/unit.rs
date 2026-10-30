//! One unit: its slots, what each node writes, the guards on its edges, and
//! the solve.

use oxabl_ast::{
    AccessModifier, Expression, ExpressionKind as E, HandleParamKind, NodeId, ParameterDirection,
    ParameterType, RunTarget, Span, Statement, StatementKind as S,
};
use oxabl_common::ParameterSources;
use oxabl_flow::dataflow::{Analysis, Direction, Limits, Solution, default_budget, solve_bounded};
use oxabl_flow::provenance::{Labels, Provenance};
use oxabl_flow::{
    Action, BlockId, Cfg, Condition, Edge, EdgeKind, ExitKind, NodeIx, NodeKind, ReturnKind, Unit,
    UnitKind,
};
use oxabl_lexer::Kind;
use rustc_hash::{FxHashMap, FxHashSet};

use super::super::literal::{StatementText, ends_with_word, is_constant};
use super::value::{
    BUDGET_LABELS, CallIr, Compiler, Env, ON_RETURN, Origins, Out, PASSED, RECEIVING_METHODS,
    RETURN_VALUE, Routines, Slots, Summaries, Tables, Term, UNKNOWN, UnitSummary, Value,
};
use super::{ALL, Options};

/// The most work the solve of one unit may do, unless the options set
/// another. Past it the unit is not judged: see `value::BUDGET`. Measured on
/// a large web tree: 2M leaves 0.05% of its units unjudged; 8M leaves a third
/// of those for 1.4x the run time; 16M still leaves a fifth, for 1.8x.
pub(super) const WORK_LIMIT: usize = 8_000_000;

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

/// One parameter position of a unit.
#[derive(Debug, Clone, Default)]
pub(super) struct ParamInfo {
    /// The origin a call replaces with its argument.
    pub origin: Option<u32>,
    pub slot: Option<u32>,
    /// `OUTPUT` or `INPUT-OUTPUT`: its value at the exit goes back.
    pub output: bool,
}

/// The slot of a function or method result.
const RETURN_SLOT: &str = "$return";

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
    /// A procedure: what it leaves in `RETURN-VALUE` is its result.
    pub procedure: bool,
    /// By position.
    pub params: Vec<ParamInfo>,
    /// The calls of routines of the file, with the node that makes each.
    pub calls: Vec<(NodeIx, CallIr)>,
    /// The calls that may run code the file does not hold, with the node that
    /// makes each: see `foreign_statement` and `Compiler::runs_elsewhere`.
    pub foreign: Vec<(NodeIx, Span)>,
    /// Code outside the file may run before the unit and write what it reads:
    /// an include that did not arrive, or the includer of an include checked
    /// on its own, which may run its routines. Every read of a file-level slot
    /// or a parameter also holds Unknown.
    pub outside: bool,
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

/// The temp-tables of a file, what each buffer, dataset and handle variable
/// is for, and the names it defines.
pub(super) fn tables(stmts: &FxHashMap<NodeId, &Statement>, source: &str) -> Tables {
    let mut t = Tables {
        return_value: source
            .as_bytes()
            .windows(10)
            .any(|w| w.eq_ignore_ascii_case(b"return-val")),
        ..Tables::default()
    };
    let locals = routine_statements(stmts);
    let mut undoable = FxHashSet::default();
    let mut handle_vars = FxHashSet::default();
    let mut copies = Vec::new();
    for s in stmts.values() {
        defined_names(s, source, locals.contains(&s.id), &mut t.names);
        if let S::VariableDeclaration { name, no_undo, .. }
        | S::Property { name, no_undo, .. }
        | S::DefineParameter {
            param_type: ParameterType::Variable { name, no_undo, .. },
            ..
        } = &s.kind
        {
            let set = if *no_undo {
                &mut t.no_undo
            } else {
                &mut undoable
            };
            set.insert(name.name.to_ascii_lowercase());
        }
        if let S::VariableDeclaration {
            name,
            is_new_shared,
            is_shared,
            is_new_global_shared,
            ..
        }
        | S::DefineTempTable {
            name,
            is_new_shared,
            is_shared,
            is_new_global_shared,
            ..
        } = &s.kind
        {
            let name = name.name.to_ascii_lowercase();
            if *is_shared || *is_new_global_shared {
                t.shared_in.insert(name.clone());
            }
            if *is_new_shared || *is_shared || *is_new_global_shared {
                t.shared.insert(name);
            }
        }
        // A member that code outside the class may set.
        match &s.kind {
            S::VariableDeclaration { name, .. } if member_is_visible(source, s.span) => {
                t.shared.insert(name.name.to_ascii_lowercase());
            }
            S::Property {
                name,
                access,
                set_body: Some(_),
                ..
            } if *access != AccessModifier::Private && !private_set(source, s.span) => {
                t.shared.insert(name.name.to_ascii_lowercase());
            }
            _ => {}
        }
        match &s.kind {
            S::DefineTempTable { name, .. } => {
                t.temp_tables.insert(name.name.to_ascii_lowercase());
            }
            S::DefineDataset { name, buffers, .. } => {
                t.datasets.insert(
                    name.name.to_ascii_lowercase(),
                    buffers
                        .iter()
                        .map(|b| b.name.to_ascii_lowercase())
                        .collect(),
                );
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
            S::VariableDeclaration {
                name, type_source, ..
            }
            | S::DefineParameter {
                param_type:
                    ParameterType::Variable {
                        name, type_source, ..
                    },
                ..
            } => {
                let ty = match type_source {
                    oxabl_ast::TypeSource::Explicit(d) => d.class_name().unwrap_or(""),
                    oxabl_ast::TypeSource::Like { .. } => "",
                };
                // A handle, or a type the file does not show.
                if matches!(
                    type_source,
                    oxabl_ast::TypeSource::Like { .. }
                        | oxabl_ast::TypeSource::Explicit(
                            oxabl_ast::DataType::Handle
                                | oxabl_ast::DataType::Com
                                | oxabl_ast::DataType::PreprocIf(_)
                        )
                ) {
                    handle_vars.insert(name.name.to_ascii_lowercase());
                }
                t.types
                    .insert(name.name.to_ascii_lowercase(), ty.to_ascii_lowercase());
            }
            S::Property {
                name, data_type, ..
            } => {
                t.types.insert(
                    name.name.to_ascii_lowercase(),
                    data_type.class_name().unwrap_or("").to_ascii_lowercase(),
                );
            }
            // An error object: its members are a class's.
            S::Catch {
                error_var,
                error_type,
                ..
            } => {
                t.types.insert(
                    error_var.name.to_ascii_lowercase(),
                    error_type.to_ascii_lowercase(),
                );
            }
            S::Assignment { target, value } => {
                handle_of(target, value, &mut t.handles);
                private_data(target, &mut t.private_data);
                copies.extend(copy_of(target, value));
            }
            S::Assign { assignments } => assignments.iter().for_each(|p| {
                handle_of(&p.target, &p.value, &mut t.handles);
                private_data(&p.target, &mut t.private_data);
                copies.extend(copy_of(&p.target, &p.value));
            }),
            _ => {}
        }
    }
    t.no_undo.retain(|n| !undoable.contains(n));
    for (a, b) in copies {
        if handle_vars.contains(&a) || handle_vars.contains(&b) {
            t.aliased.insert(a);
            t.aliased.insert(b);
        }
    }
    t
}

/// `a = b` with two plain names, lower-cased: `b` is no system handle.
fn copy_of(target: &Expression, value: &Expression) -> Option<(String, String)> {
    let (E::Identifier(a), E::Identifier(b)) = (&target.kind, &value.kind) else {
        return None;
    };
    let b_name = b.name.to_ascii_lowercase();
    (value.span.start >= b.span.start && !oxabl_semantic::SYSTEM_HANDLES.contains(&b_name.as_str()))
        .then(|| (a.name.to_ascii_lowercase(), b_name))
}

/// The statements inside a routine: what they declare is not visible to the
/// rest of the file.
fn routine_statements(stmts: &FxHashMap<NodeId, &Statement>) -> FxHashSet<NodeId> {
    let mut out = FxHashSet::default();
    for s in stmts.values() {
        let bodies: Vec<&[Statement]> = match &s.kind {
            S::Procedure { body, .. } | S::Function { body, .. } | S::Destructor { body } => {
                vec![body]
            }
            S::Method {
                parameters, body, ..
            }
            | S::Constructor {
                parameters, body, ..
            } => vec![parameters, body],
            S::Property {
                get_body,
                set_body,
                set_parameters,
                ..
            } => std::iter::once(set_parameters.as_slice())
                .chain(
                    [get_body, set_body]
                        .into_iter()
                        .flatten()
                        .map(Vec::as_slice),
                )
                .collect(),
            _ => continue,
        };
        for body in bodies {
            out.extend(statements(body).into_keys());
        }
    }
    out
}

/// `h = TEMP-TABLE tt:HANDLE`, `DATASET ds:HANDLE` or `BUFFER b:HANDLE`: `h`
/// stands for those tables.
fn handle_of(target: &Expression, value: &Expression, out: &mut FxHashMap<String, Vec<String>>) {
    let (E::Identifier(h), E::MemberAccess { object, member }) = (&target.kind, &value.kind) else {
        return;
    };
    let E::Identifier(table) = &object.kind else {
        return;
    };
    if object.span.start < table.span.start
        && ["HANDLE", "DEFAULT-BUFFER-HANDLE"]
            .iter()
            .any(|m| m.eq_ignore_ascii_case(&member.name))
    {
        out.entry(h.name.to_ascii_lowercase())
            .or_default()
            .push(table.name.clone());
    }
}

/// `h:PRIVATE-DATA = x`: add `h`.
fn private_data(target: &Expression, out: &mut FxHashSet<String>) {
    if let E::MemberAccess { object, member } = &target.kind
        && let E::Identifier(h) = &object.kind
        && member.name.eq_ignore_ascii_case("PRIVATE-DATA")
    {
        out.insert(h.name.to_ascii_lowercase());
    }
}

/// Add the names a statement defines to `out`, lower-cased. A variable or
/// parameter `local` to a routine is not added.
fn defined_names(s: &Statement, source: &str, local: bool, out: &mut FxHashSet<String>) {
    let mut add = |i: &oxabl_ast::Identifier| {
        out.insert(i.name.to_ascii_lowercase());
    };
    match &s.kind {
        S::VariableDeclaration { .. }
        | S::DefineParameter {
            param_type: ParameterType::Variable { .. } | ParameterType::Handle { .. },
            ..
        }
        | S::Catch { .. }
            if local => {}
        S::VariableDeclaration { name, .. }
        | S::Property { name, .. }
        | S::DefineBuffer { name, .. }
        | S::DefineDataset { name, .. }
        | S::DefineDataSource { name, .. }
        | S::DefineStream { name }
        | S::DefineFrame { name, .. }
        | S::DefineWidget { name }
        | S::DefineEvent { name, .. } => add(name),
        S::DefineTempTable { name, fields, .. } => {
            add(name);
            fields.iter().for_each(|f| add(&f.name));
        }
        S::DefineParameter { param_type, .. } => match param_type {
            ParameterType::Variable { name, .. }
            | ParameterType::Handle { name, .. }
            | ParameterType::Buffer { name, .. } => add(name),
        },
        S::Catch { error_var, .. } => add(error_var),
        // `DEFINE QUERY q ...` and other definitions the parser skips.
        S::Skipped { names, .. }
            if crate::rules::syntactic::source_of(source, s.span).is_some_and(|t| {
                t.trim_start()
                    .get(..6)
                    .is_some_and(|w| w.eq_ignore_ascii_case("DEFINE"))
            }) =>
        {
            names.iter().for_each(add);
        }
        _ => {}
    }
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
    unit: u32,
    /// The unit is a procedure: `RETURN` and `RETURN ERROR` set its
    /// `RETURN-VALUE`.
    procedure: bool,
    /// Parameter position by the id of its definition.
    positions: FxHashMap<NodeId, u32>,
    params: Vec<ParamInfo>,
}

impl Builder<'_, '_> {
    /// `RETURN-VALUE` takes `value`, in a file that reads it.
    fn set_return_value(&mut self, out: &mut Vec<Op>, value: Value, span: Span) {
        if !self.c.tables.return_value {
            return;
        }
        if let Some(slot) = self.c.return_value_slot() {
            out.push(Op {
                slot,
                value,
                strong: true,
                span,
            });
        }
    }

    /// What the `RETURN [ERROR] x` action of the `ON condition` phrase of a
    /// block gives back in `RETURN-VALUE`: `x`, or empty. `None` when the
    /// header does not show it (a `RETURN NO-APPLY`, a phrase the header does
    /// not hold), which the caller reads as Unknown.
    fn on_return(
        &mut self,
        header: Span,
        block: &Statement,
        condition: Condition,
        kind: ReturnKind,
    ) -> Option<Value> {
        let (S::Do { on_actions, .. }
        | S::Repeat { on_actions, .. }
        | S::ForEach { on_actions, .. }) = &block.kind
        else {
            return None;
        };
        if kind == ReturnKind::NoApply {
            return None;
        }
        let st = StatementText::over(self.source, header)?;
        let words: &[&str] = match condition {
            Condition::Error => &["ERROR"],
            Condition::Endkey => &["ENDKEY", "END-KEY"],
            Condition::Stop => &["STOP"],
            Condition::Quit => &["QUIT"],
        };
        // The `ON` phrases of the header, up to its `:`.
        let mut depth = 0usize;
        let mut phrases = Vec::new();
        for i in 0..st.len() {
            match st.token(i).kind {
                Kind::LeftParen => depth += 1,
                Kind::RightParen => depth = depth.saturating_sub(1),
                Kind::Colon if depth == 0 => break,
                Kind::On if depth == 0 => phrases.push(i),
                _ => {}
            }
        }
        let k = phrases
            .iter()
            .position(|&i| words.iter().any(|w| st.is_word(i + 1, w)))?;
        let start = st.span(phrases[k]..phrases[k] + 1).start;
        let end = phrases
            .get(k + 1)
            .map_or(header.end, |&i| st.span(i..i + 1).start);
        Some(
            match on_actions
                .iter()
                .find(|e| e.span.start >= start && e.span.start < end)
            {
                Some(e) => self.c.expr(e),
                None => Value::default(),
            },
        )
    }

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
        let inbound = matches!(
            direction,
            ParameterDirection::Input | ParameterDirection::InputOutput
        );
        let input = inbound && self.external;
        let pos = self.positions.get(&stmt.id).copied();
        let output = matches!(
            direction,
            ParameterDirection::Output | ParameterDirection::InputOutput
        );
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
                let mut origin = None;
                // `PASSED` too, so a call puts back what its argument holds of
                // an identity.
                let value = if input {
                    let id = self
                        .c
                        .origins
                        .add(format!("input parameter `{}`", name.name), stmt.span);
                    origin = Some(id);
                    Value::origin(id).tagged(PASSED)
                } else if inbound && pos.is_some() {
                    let id = self
                        .c
                        .origins
                        .placeholder(format!("parameter `{}`", name.name), stmt.span);
                    origin = Some(id);
                    Value::origin(id).tagged(PASSED)
                } else {
                    Value::default()
                };
                let value = match pos {
                    Some(p) if inbound => Value::union([
                        value,
                        Value(vec![Term::Param {
                            unit: self.unit,
                            pos: p,
                        }]),
                    ]),
                    _ => value,
                };
                if let Some(p) = pos {
                    self.params[p as usize] = ParamInfo {
                        origin,
                        slot: Some(slot),
                        output,
                    };
                }
                out.push(Op {
                    slot,
                    value,
                    strong: true,
                    span: stmt.span,
                });
            }
            ParameterType::Handle {
                kind: kind @ (HandleParamKind::Table | HandleParamKind::Dataset),
                name,
                ..
            } if input => {
                let (what, tables) = match kind {
                    HandleParamKind::Table => ("table", vec![name.name.clone()]),
                    _ => ("dataset", self.c.tables.tables_of(&name.name)),
                };
                let id = self
                    .c
                    .origins
                    .add(format!("input {what} `{}`", name.name), stmt.span);
                for table in tables {
                    if let Some(slot) = self.c.table_slot(&table) {
                        out.push(Op {
                            slot,
                            value: Value::origin(id),
                            strong: false,
                            span: stmt.span,
                        });
                    }
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
        if let E::MethodCall { object, method, .. } = &e.kind {
            self.receive(out, object, &method.name, arguments, span);
            for (a, v) in self.c.method_writes(object, &method.name, arguments) {
                if written_direction(self.source, a.span.start).is_none() {
                    let at = out.len();
                    self.set(out, a, v, span);
                    out[at..].iter_mut().for_each(|op| op.strong = false);
                }
            }
        }
        let outputs: Vec<&Expression> = arguments
            .iter()
            .filter(|a| written_direction(self.source, a.span.start).is_some())
            .collect();
        if outputs.is_empty() {
            return;
        }
        let callees = match &e.kind {
            E::FunctionCall { name, .. } => self.c.routines.callable(&name.name),
            E::MethodCall { object, method, .. } if is_this_object(object) => {
                self.c.routines.method(&method.name)
            }
            _ => None,
        }
        .map(<[u32]>::to_vec);
        if let Some(callees) = callees {
            let args: Vec<Value> = arguments
                .iter()
                .map(|a| match written_direction(self.source, a.span.start) {
                    Some(ParameterDirection::Output) => Value::default(),
                    _ => self.c.expr(a),
                })
                .collect();
            for (i, a) in arguments.iter().enumerate() {
                if written_direction(self.source, a.span.start).is_some() {
                    let v =
                        self.c
                            .routine_call(&callees, Out::Param(i as u32), args.clone(), e.span);
                    self.set(out, a, v, span);
                }
            }
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

    /// A method of [`RECEIVING_METHODS`] on a handle variable adds its
    /// arguments to the variable: `hRoot:APPEND-CHILD(hText)`,
    /// `hNode:SET-ATTRIBUTE("a", x)`.
    fn receive(
        &mut self,
        out: &mut Vec<Op>,
        object: &Expression,
        method: &str,
        arguments: &[Expression],
        span: Span,
    ) {
        let E::Identifier(id) = &object.kind else {
            return;
        };
        let var = self
            .c
            .tables
            .types
            .get(&id.name.to_ascii_lowercase())
            .is_some_and(String::is_empty);
        if object.span.start < id.span.start
            || !var
            || !RECEIVING_METHODS
                .iter()
                .any(|m| m.eq_ignore_ascii_case(method))
        {
            return;
        }
        let value = Value::union(arguments.iter().map(|a| self.c.expr(a)).collect::<Vec<_>>());
        if let Some(slot) = self.c.var_slot(&id.name) {
            out.push(Op {
                slot,
                value,
                strong: false,
                span,
            });
        }
    }

    /// `READ-JSON`, `READ-XML`, `LOAD`, `COPY-TEMP-TABLE`, `COPY-DATASET`
    /// and `BUFFER-COPY` methods fill their receiver with their arguments:
    /// the tables of `TEMP-TABLE tt:`, `DATASET ds:` or `BUFFER b:`, or the
    /// handle variable a chain starts at and the tables the file set it to.
    /// `READ-JSON`, `READ-XML` and `LOAD` from a file are a file read.
    fn fill(&mut self, out: &mut Vec<Op>, e: &Expression, span: Span) {
        const FILLS: [&str; 6] = [
            "READ-JSON",
            "READ-XML",
            "LOAD",
            "COPY-TEMP-TABLE",
            "COPY-DATASET",
            "BUFFER-COPY",
        ];
        for_each_child(e, &mut |c| self.fill(out, c, span));
        let E::MethodCall {
            object,
            method,
            arguments,
        } = &e.kind
        else {
            return;
        };
        arguments.iter().for_each(|a| self.fill(out, a, span));
        if !FILLS.iter().any(|f| f.eq_ignore_ascii_case(&method.name)) {
            return;
        }
        let mut value = Value::union(arguments.iter().map(|a| self.c.expr(a)).collect::<Vec<_>>());
        // `READ-JSON("FILE", ...)`, or a source type that is not a literal.
        let read = ["READ-JSON", "READ-XML", "LOAD"]
            .iter()
            .any(|m| m.eq_ignore_ascii_case(&method.name));
        if read
            && arguments.first().is_some_and(|a| {
                !matches!(&a.kind, E::Literal(oxabl_ast::Literal::String(s))
                    if !s.value.eq_ignore_ascii_case("FILE"))
            })
        {
            let what = format!("file read `{}`", method.name.to_ascii_uppercase());
            let id = self.c.origins.add(what, e.span);
            value.0.push(Term::Origin(id));
        }
        let mut base = &**object;
        while let E::MemberAccess { object, .. } | E::MethodCall { object, .. } = &base.kind {
            base = object;
        }
        let E::Identifier(id) = &base.kind else {
            return;
        };
        let slots: Vec<u32> = if base.span.start < id.span.start {
            self.c
                .tables
                .tables_of(&id.name)
                .iter()
                .filter_map(|t| self.c.table_slot(t))
                .collect()
        } else {
            // A handle variable, and the tables the file set it to.
            let tables = self.c.tables;
            let named = tables.handles.get(&id.name.to_ascii_lowercase());
            self.c
                .var_slot(&id.name)
                .into_iter()
                .chain(
                    named
                        .into_iter()
                        .flatten()
                        .flat_map(|n| tables.tables_of(n))
                        .filter_map(|t| self.c.table_slot(&t))
                        .collect::<Vec<_>>(),
                )
                .collect()
        };
        for slot in slots {
            out.push(Op {
                slot,
                value: value.clone(),
                strong: false,
                span,
            });
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
        // The statement is read from its tokens, whole.
        let mut whole = false;
        if self.c.tables.watches() && foreign_statement(s, self.source, self.c.tables) {
            self.c.foreign.push(s.span);
        }
        match (&s.kind, node.kind) {
            // A `SHARED` or `NEW GLOBAL SHARED` definition binds a value that
            // may already exist.
            (
                S::VariableDeclaration {
                    is_shared: false,
                    is_new_global_shared: false,
                    initial_value,
                    ..
                },
                _,
            ) => {
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
                self.fill(&mut out, value, s.span);
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
                    self.fill(&mut out, &pair.value, span);
                    let v = self.c.expr(&pair.value);
                    self.set(&mut out, &pair.target, v, span);
                }
            }
            (
                S::BufferCopy {
                    source,
                    target,
                    assignments,
                    ..
                },
                _,
            ) => {
                if let Some(slot) = self.c.table_slot(&target.name) {
                    let table = self.c.tables.table_of(&source.name);
                    out.push(Op {
                        slot,
                        value: Value(vec![Term::Table(table)]),
                        strong: false,
                        span: s.span,
                    });
                }
                for pair in assignments {
                    let v = self.c.expr(&pair.value);
                    self.set(&mut out, &pair.target, v, s.span);
                }
            }
            (S::ExpressionStatement(e), _) => {
                self.output_arguments(&mut out, e, s.span);
                self.socket_read(&mut out, e, s.span);
                self.json(&mut out, e, s.span);
                self.fill(&mut out, e, s.span);
                self.c.expr(e);
            }
            (S::Return(Some(e)), _) => {
                self.output_arguments(&mut out, e, s.span);
                let v = self.c.expr(e);
                if self.procedure {
                    self.set_return_value(&mut out, v.clone(), s.span);
                }
                if self.c.declared.contains(RETURN_SLOT)
                    && let Some(slot) = self.c.var_slot(RETURN_SLOT)
                {
                    out.push(Op {
                        slot,
                        value: v,
                        strong: true,
                        span: s.span,
                    });
                }
            }
            // `RETURN.`: an empty `RETURN-VALUE`. `RETURN ERROR` with a value
            // the parser does not hold reads it from the tail.
            (S::Return(None), _) if self.procedure => {
                self.set_return_value(&mut out, Value::default(), s.span);
            }
            // `RETURN ERROR x` and `UNDO, THROW x`: a procedure gives `x` back
            // in `RETURN-VALUE`, also when it is an error object.
            (S::Throw(e), _) if self.procedure => {
                self.output_arguments(&mut out, e, s.span);
                let v = self.c.expr(e);
                self.set_return_value(&mut out, v, s.span);
            }
            (
                S::Run {
                    target: RunTarget::Literal { name, .. },
                    arguments,
                    in_handle,
                    ..
                },
                _,
            ) if in_handle.as_ref().is_none_or(|h| self.is_this_procedure(h))
                && self.c.routines.procedure(name).is_some() =>
            {
                let callees = self.c.routines.procedure(name).unwrap_or_default().to_vec();
                let args: Vec<Value> = arguments
                    .iter()
                    .map(|a| match a.direction {
                        ParameterDirection::Output => Value::default(),
                        _ => self.c.expr(&a.expression),
                    })
                    .collect();
                self.c.calls.push(CallIr {
                    callees: callees.clone(),
                    args: args.clone(),
                    span: s.span,
                    run: true,
                });
                let result = Value(vec![Term::Call {
                    callees: callees.clone(),
                    out: Out::Return,
                    args: args.clone(),
                }]);
                self.set_return_value(&mut out, result, s.span);
                for (i, a) in arguments.iter().enumerate() {
                    if matches!(
                        a.direction,
                        ParameterDirection::Output | ParameterDirection::InputOutput
                    ) {
                        let v = Value(vec![Term::Call {
                            callees: callees.clone(),
                            out: Out::Param(i as u32),
                            args: args.clone(),
                        }]);
                        self.set(&mut out, &a.expression, v, s.span);
                    }
                }
            }
            (
                S::Run {
                    target,
                    arguments,
                    in_handle,
                    ..
                },
                _,
            ) => {
                if self.c.tables.watches() {
                    self.c.foreign.push(s.span);
                }
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
                // What the program it runs returns: an unknown call's result,
                // and Unknown.
                let result = Value::union([value, Value::origin(UNKNOWN)]);
                self.set_return_value(&mut out, result, s.span);
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
            (S::Skipped { .. }, _) => {
                whole = true;
                self.copy_lob(&mut out, s);
                if let Some(st) = StatementText::of(self.source, s) {
                    self.c.tokens(&st, 0..st.len());
                }
            }
            (
                S::Do {
                    loop_var: Some(v),
                    from: Some(from),
                    ..
                },
                NodeKind::Head,
            ) => {
                let value = self.c.expr(from).cleared(super::value::NUMERIC_SAFE);
                self.header_calls(s);
                if let Some(slot) = self.c.var_slot(&v.name) {
                    out.push(Op {
                        slot,
                        value,
                        strong: true,
                        span: node.span,
                    });
                }
            }
            // The error object holds what was thrown, which may be text of any
            // origin: Unknown.
            (S::Catch { error_var, .. }, NodeKind::Head) => {
                if let Some(slot) = self.c.var_slot(&error_var.name) {
                    out.push(Op {
                        slot,
                        value: Value::origin(UNKNOWN),
                        strong: true,
                        span: node.span,
                    });
                }
            }
            (S::If { condition, .. }, NodeKind::Stmt) => {
                self.output_arguments(&mut out, condition, s.span);
                self.fill(&mut out, condition, s.span);
                self.c.expr(condition);
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
                self.c.expr(expression);
                for v in when_branches.iter().flat_map(|w| &w.values) {
                    self.c.expr(v);
                }
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
            (S::Do { .. } | S::Repeat { .. } | S::ForEach { .. }, NodeKind::Head) => {
                self.header_calls(s);
            }
            (kind, _) if !has_body(kind) => {
                whole = true;
                self.scan_calls(s);
            }
            _ => {}
        }
        if !whole {
            self.unmodelled(&mut out, s);
        }
        (out, guard)
    }

    /// The ranges of a modelled statement that its node does not hold
    /// (`SkippedTail::unmodelled`). Each runs its calls and makes the writes
    /// of the statement weak, since it may decide whether they happen; one
    /// that is not a condition or a sort key (`WHEN`, `WHERE`, `WHILE` or `BY`
    /// before it) also adds its value to them. In an `ASSIGN`, a range touches
    /// the pair it follows.
    fn unmodelled(&mut self, out: &mut [Op], s: &Statement) {
        let Some(tail) = &s.skipped_tail else {
            return;
        };
        for &span in &tail.unmodelled {
            let Some(st) = StatementText::over(self.source, span) else {
                continue;
            };
            let value = self.c.tokens(&st, 0..st.len());
            let before = self.source.get(..span.start as usize).unwrap_or_default();
            let condition = ["WHEN", "WHERE", "WHILE", "BY"]
                .iter()
                .any(|w| ends_with_word(before, w));
            let pair = match &s.kind {
                S::Assign { .. } => out
                    .iter()
                    .filter(|op| op.span.end <= span.start)
                    .map(|op| op.span)
                    .max_by_key(|p| p.end),
                _ => None,
            };
            for op in out
                .iter_mut()
                .filter(|op| pair.is_none_or(|p| op.span == p))
            {
                op.strong = false;
                if !condition {
                    op.value = Value::union([std::mem::take(&mut op.value), value.clone()]);
                }
            }
        }
    }

    /// The calls in the header expressions of a block: its `ON` actions and
    /// all but the `FROM` of a loop variable, which the caller reads.
    fn header_calls(&mut self, s: &Statement) {
        let exprs: Vec<&Expression> = match &s.kind {
            S::Do {
                loop_var,
                from,
                to,
                by,
                while_condition,
                on_actions,
                ..
            } => [
                from.as_ref().filter(|_| loop_var.is_none()),
                to.as_ref(),
                by.as_ref(),
                while_condition.as_ref(),
            ]
            .into_iter()
            .flatten()
            .chain(on_actions)
            .collect(),
            S::Repeat {
                while_condition,
                on_actions,
                ..
            } => while_condition.iter().chain(on_actions).collect(),
            S::ForEach {
                where_clause,
                on_actions,
                ..
            } => where_clause.iter().chain(on_actions).collect(),
            _ => Vec::new(),
        };
        for e in exprs {
            self.c.expr(e);
        }
    }

    /// The calls of routines in a statement that is not modelled here, read
    /// from its tokens; with shared variables, also the members it reads,
    /// which may run code elsewhere.
    fn scan_calls(&mut self, s: &Statement) {
        let exposed = self.c.tables.watches();
        let Some(text) = crate::rules::syntactic::source_of(self.source, s.span) else {
            return;
        };
        let calls = (self.c.routines.any_callable() || exposed) && text.contains('(');
        if !calls && !(exposed && text.contains(':')) {
            return;
        }
        if let Some(st) = StatementText::of(self.source, s) {
            self.c.tokens(&st, 0..st.len());
        }
    }

    fn is_this_procedure(&self, h: &Expression) -> bool {
        self.source
            .get(h.span.start as usize..h.span.end as usize)
            .is_some_and(|t| t.trim().eq_ignore_ascii_case("THIS-PROCEDURE"))
    }

    /// `o:Add(k, v)`, `o:Set(k, v)` and `o:Insert(i, v)` put the value in
    /// the JSON object `o`; `o:Write(target, ...)` puts `o` into `target`.
    fn json(&mut self, out: &mut Vec<Op>, e: &Expression, span: Span) {
        let E::MethodCall {
            object,
            method,
            arguments,
        } = &e.kind
        else {
            return;
        };
        let E::Identifier(o) = &object.kind else {
            return;
        };
        if !self.c.tables.may_be_json(&o.name) {
            return;
        }
        let m = method.name.as_str();
        if ["Add", "Set", "Insert"]
            .iter()
            .any(|w| w.eq_ignore_ascii_case(m))
        {
            let value = Value::union(arguments.iter().map(|a| self.c.expr(a)).collect::<Vec<_>>());
            if let Some(slot) = self.c.var_slot(&o.name) {
                out.push(Op {
                    slot,
                    value,
                    strong: false,
                    span,
                });
            }
        } else if m.eq_ignore_ascii_case("Write")
            && let Some(first) = arguments.first()
            && written_direction(self.source, first.span.start).is_none()
        {
            let value = self.c.expr(object);
            self.set(out, first, value, span);
        }
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

/// Whether a statement holds other statements, which have nodes of their own.
fn has_body(kind: &S) -> bool {
    matches!(
        kind,
        S::Do { .. }
            | S::Repeat { .. }
            | S::ForEach { .. }
            | S::If { .. }
            | S::Case { .. }
            | S::Procedure { .. }
            | S::Function { .. }
            | S::Class { .. }
            | S::Interface { .. }
            | S::Method { .. }
            | S::Constructor { .. }
            | S::Destructor { .. }
            | S::Property { .. }
            | S::Block(_)
            | S::Catch { .. }
            | S::Finally { .. }
            | S::On { .. }
            | S::Label { .. }
            | S::PreprocIf(_)
            | S::VariableDeclarationList(_)
    )
}

fn is_this_object(e: &Expression) -> bool {
    matches!(&e.kind, E::Identifier(i) if i.name.eq_ignore_ascii_case("THIS-OBJECT"))
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

/// The parameter positions of a unit, by the id of each definition. A main
/// block has none: no routine of the file calls it.
fn positions(unit: &Unit, stmt: Option<&Statement>, entry: &[Statement]) -> FxHashMap<NodeId, u32> {
    let list: &[Statement] = match (unit.kind, stmt.map(|s| &s.kind)) {
        (UnitKind::Main, _) => &[],
        (UnitKind::Procedure, Some(S::Procedure { body, .. }))
        | (UnitKind::Function, Some(S::Function { body, .. })) => body,
        _ => entry,
    };
    list.iter()
        .filter(|s| matches!(s.kind, S::DefineParameter { .. }))
        .enumerate()
        .map(|(i, s)| (s.id, i as u32))
        .collect()
}

/// Compile one unit. Origins are added to `origins`.
#[allow(clippy::too_many_arguments)]
pub(super) fn compile(
    cfg: &Cfg,
    stmts: &FxHashMap<NodeId, &Statement>,
    tables: &Tables,
    source: &str,
    options: &Options,
    routines: &Routines,
    origins: &mut Origins,
) -> UnitIr {
    let unit = cfg.unit();
    let unit_stmt = stmts.get(&unit.stmt).copied();
    let entry = entry_parameters(unit, unit_stmt);
    let mut declared = declared(cfg, stmts, entry);
    if matches!(
        unit.kind,
        UnitKind::Function | UnitKind::Method | UnitKind::PropertyGet
    ) {
        declared.insert(RETURN_SLOT.to_string());
    }
    let positions = positions(unit, unit_stmt, entry);
    let procedure = unit.kind == UnitKind::Procedure;
    let mut slots = Slots::default();
    let mut ops = vec![Vec::new(); cfg.node_count()];
    let mut guards = FxHashMap::default();
    let mut calls = Vec::new();
    let mut foreign = Vec::new();
    let params;
    {
        let mut b = Builder {
            c: Compiler {
                options,
                tables,
                slots: &mut slots,
                origins,
                declared: &declared,
                frozen: false,
                routines,
                calls: Vec::new(),
                foreign: Vec::new(),
                decision: false,
            },
            source,
            external: is_external(unit, unit_stmt, source, options),
            unit: cfg.id().index() as u32,
            procedure,
            params: vec![ParamInfo::default(); positions.len()],
            positions,
        };
        for p in entry {
            b.parameter(&mut ops[cfg.entry().index()], p);
        }
        for (i, n) in cfg.nodes() {
            if let NodeKind::Handler(condition) = n.kind
                && procedure
                && tables.return_value
            {
                let o = handler_return(&mut b, cfg, stmts, i, condition);
                ops[i.index()].extend(o);
                calls.extend(b.c.calls.drain(..).map(|c| (i, c)));
                continue;
            }
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
            calls.extend(b.c.calls.drain(..).map(|c| (i, c)));
            foreign.extend(b.c.foreign.drain(..).map(|span| (i, span)));
        }
        if procedure && tables.return_value {
            b.c.return_value_slot();
            on_return_without_edge(&mut b, cfg, stmts, &mut ops, &mut calls);
        }
        undo_restores(&mut b.c, cfg, &mut ops);
        params = b.params;
    }
    if let Some(slot) = slots.get(RETURN_VALUE) {
        return_value_after_calls(slot, &mut ops, &calls, &foreign, cfg.entry());
    }
    for op in ops.iter_mut().flatten() {
        op.value.expand_tables(&slots);
    }
    UnitIr {
        slots,
        declared,
        ops,
        guards,
        main: unit.kind == UnitKind::Main,
        procedure,
        params,
        calls,
        foreign,
        outside: false,
    }
}

/// The `RETURN` action of the `ON` phrase a `Handler` node carries out: it
/// sets `RETURN-VALUE`. On a node that may also go to a `CATCH`, the write is
/// weak.
fn handler_return(
    b: &mut Builder,
    cfg: &Cfg,
    stmts: &FxHashMap<NodeId, &Statement>,
    i: NodeIx,
    condition: Condition,
) -> Vec<Op> {
    let n = cfg.node(i);
    let returns = |e: &&Edge| matches!(e.kind, EdgeKind::Return | EdgeKind::ReturnError);
    let Some(Action::Return(kind)) = cfg.block(n.block).handling(condition).map(|h| &h.action)
    else {
        return Vec::new();
    };
    if !cfg.successors(i).any(|e| returns(&e)) {
        return Vec::new();
    }
    let strong = cfg.successors(i).all(|e| returns(&e));
    let value = n
        .stmt
        .and_then(|id| stmts.get(&id))
        .and_then(|s| b.on_return(n.span, s, condition, *kind));
    let mut out = Vec::new();
    match value {
        Some(v) => {
            b.set_return_value(&mut out, v, n.span);
            out.iter_mut().for_each(|op| op.strong = strong);
        }
        None => {
            b.set_return_value(&mut out, Value::origin(UNKNOWN), n.span);
            out.iter_mut().for_each(|op| op.strong = false);
        }
    }
    out
}

/// A block whose `ON` phrase returns on a condition that nothing in the CFG
/// raises in it (an `ENDKEY` with no input statement, a `STOP`) may still
/// return there: each node of the block adds the action's value to
/// [`ON_RETURN`], which the summary gives back.
fn on_return_without_edge(
    b: &mut Builder,
    cfg: &Cfg,
    stmts: &FxHashMap<NodeId, &Statement>,
    ops: &mut [Vec<Op>],
    calls: &mut Vec<(NodeIx, CallIr)>,
) {
    let inside = |mut at: Option<BlockId>, root: BlockId| {
        while let Some(b) = at {
            if b == root {
                return true;
            }
            at = cfg.block(b).parent;
        }
        false
    };
    for (id, block) in cfg.blocks() {
        for condition in Condition::ALL {
            let Some(Action::Return(kind)) = block.handling(condition).map(|h| &h.action) else {
                continue;
            };
            if cfg
                .nodes()
                .any(|(_, n)| n.kind == NodeKind::Handler(condition) && n.block == id)
            {
                continue;
            }
            let Some((head, h)) = cfg
                .nodes()
                .find(|(_, n)| n.kind == NodeKind::Head && n.block == id)
            else {
                continue;
            };
            let value = stmts
                .get(&block.stmt)
                .and_then(|s| b.on_return(h.span, s, condition, *kind))
                .unwrap_or_else(|| Value::origin(UNKNOWN));
            calls.extend(b.c.calls.drain(..).map(|c| (head, c)));
            let Some(slot) = b.c.on_return_slot() else {
                continue;
            };
            for (i, n) in cfg.nodes() {
                if matches!(n.kind, NodeKind::Head | NodeKind::Stmt | NodeKind::End)
                    && inside(Some(n.block), id)
                {
                    ops[i.index()].push(Op {
                        slot,
                        value: value.clone(),
                        strong: false,
                        span: h.span,
                    });
                }
            }
        }
    }
}

/// The block that `n` undoes, if any: a handler undoes the block its `ON`
/// phrase names, else its own; an `UNDO` statement the block it targets.
fn undone(cfg: &Cfg, n: NodeIx) -> Option<BlockId> {
    let node = cfg.node(n);
    let NodeKind::Handler(condition) = node.kind else {
        return cfg.undone_block(n);
    };
    let label = cfg
        .block(node.block)
        .handling(condition)
        .and_then(|h| h.undo.as_deref());
    let Some(label) = label else {
        return Some(node.block);
    };
    let mut at = Some(node.block);
    while let Some(b) = at {
        let block = cfg.block(b);
        if block
            .label
            .as_deref()
            .is_some_and(|l| l.eq_ignore_ascii_case(label))
        {
            return Some(b);
        }
        at = block.parent;
    }
    Some(node.block)
}

/// An `UNDO` gives each undoable variable that the undone block writes back
/// the value it had at the head of the block (at the unit entry for the
/// routine block). Each such variable has a slot that the head copies it to;
/// each node that undoes the block (a handler, before its action; an `UNDO`
/// statement, after the value of its `RETURN` or `THROW`) adds that slot back
/// to the variable. Only a strong write can lose the old value, so only
/// variables the block writes strongly are followed. A field is written
/// weakly, so it keeps its old value already. Of a routine, only the
/// file-level variables live on after the undo.
fn undo_restores(c: &mut Compiler, cfg: &Cfg, ops: &mut [Vec<Op>]) {
    let mut undoes: Vec<(NodeIx, BlockId)> = cfg
        .nodes()
        .filter_map(|(i, _)| undone(cfg, i).map(|b| (i, b)))
        .collect();
    undoes.sort_by_key(|(i, b)| (b.index(), i.index()));
    let mut blocks: Vec<BlockId> = undoes.iter().map(|(_, b)| *b).collect();
    blocks.dedup();
    for block in blocks {
        let routine = block == BlockId::ROUTINE;
        let mut written: Vec<u32> = cfg
            .nodes()
            .filter(|(_, n)| cfg.block_contains(block, n.block))
            .flat_map(|(i, _)| ops[i.index()].iter())
            .filter(|op| op.strong)
            .map(|op| op.slot)
            .filter(|&s| {
                let info = &c.slots.info[s as usize];
                !info.key.starts_with('$')
                    && !info.key.contains('.')
                    && !c.tables.no_undo.contains(&info.key)
                    && (!routine || info.global)
            })
            .collect();
        written.sort_unstable();
        written.dedup();
        if written.is_empty() {
            continue;
        }
        let head = if routine {
            Some(cfg.entry())
        } else {
            cfg.nodes()
                .find(|(_, n)| n.kind == NodeKind::Head && n.block == block)
                .map(|(i, _)| i)
        };
        let Some(head) = head else {
            continue;
        };
        for var in written {
            let Some(saved) = c.undo_slot(block.index(), var) else {
                continue;
            };
            let span = cfg.node(head).span;
            let copy = Op {
                slot: saved,
                value: Value(vec![Term::Slot(var)]),
                strong: true,
                span,
            };
            if routine {
                ops[head.index()].push(copy);
            } else {
                ops[head.index()].insert(0, copy);
            }
            for &(n, _) in undoes.iter().filter(|(_, b)| *b == block) {
                let restore = Op {
                    slot: var,
                    value: Value(vec![Term::Slot(saved)]),
                    strong: false,
                    span: cfg.node(n).span,
                };
                if matches!(cfg.node(n).kind, NodeKind::Handler(_)) {
                    ops[n.index()].insert(0, restore);
                } else {
                    ops[n.index()].push(restore);
                }
            }
        }
    }
}

/// `RETURN-VALUE` holds, at the unit entry, what a caller's last `RUN` left:
/// Unknown. After a call that may run code the file does not hold, and after
/// a call of a function or method of the file, which may `RUN` something, it
/// may hold Unknown too. A `RUN` of a procedure sets it as it compiles.
fn return_value_after_calls(
    slot: u32,
    ops: &mut [Vec<Op>],
    calls: &[(NodeIx, CallIr)],
    foreign: &[(NodeIx, Span)],
    entry: NodeIx,
) {
    let unknown = |span, strong| Op {
        slot,
        value: Value::origin(UNKNOWN),
        strong,
        span,
    };
    let sites = foreign.iter().copied().chain(
        calls
            .iter()
            .filter(|(_, c)| !c.run)
            .map(|(n, c)| (*n, c.span)),
    );
    for (n, span) in sites {
        ops[n.index()].push(unknown(span, false));
    }
    ops[entry.index()].insert(0, unknown(Span::DUMMY, true));
}

/// The forward problem of one unit.
struct Problem<'a> {
    ir: &'a UnitIr,
    env: Env<'a>,
    seeds: Provenance,
}

impl Problem<'_> {
    fn apply(&self, node: NodeIx, fact: &mut Provenance) {
        for op in &self.ir.ops[node.index()] {
            apply(op, fact, self.env);
        }
    }
}

/// Apply one write to `fact`.
pub(super) fn apply(op: &Op, fact: &mut Provenance, env: Env) {
    let (labels, _) = op.value.eval(fact, env);
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
/// summaries. `None` when the unit costs more than `work`.
pub(super) fn solve(cfg: &Cfg, ir: &UnitIr, env: Env, work: usize) -> Option<Solution<Provenance>> {
    let env = env.outside(ir.outside);
    let mut seeds = Provenance::new(ir.slots.len());
    for (i, slot) in ir.slots.info.iter().enumerate() {
        if slot.global
            && let Some(l) = env.files.labels.get(&slot.key)
        {
            seeds.set(i, l.clone());
        }
        if slot.outside {
            seeds.add(i, &[(UNKNOWN, ALL)]);
        }
        if slot.global {
            let mut unsure = Labels::new();
            env.unsure(&mut unsure);
            seeds.add(i, &unsure);
        }
    }
    let problem = Problem { ir, env, seeds };
    let limits = Limits {
        visits: default_budget(cfg),
        work,
    };
    solve_bounded(cfg, &problem, limits).ok()
}

/// Whether running `s` may run code the file does not hold, which may write
/// its shared variables: `PUBLISH`, which runs the subscribers; `APPLY`,
/// `WAIT-FOR`, `PROCESS EVENTS` and the statements that wait for input
/// (`UPDATE`, `SET`, `PROMPT-FOR`, `CHOOSE`, `INSERT`), which run triggers;
/// and `DELETE OBJECT` of an object of a class (not a handle), which runs its
/// destructor. The `RUN` of anything but an internal procedure of the file in
/// this procedure and the calls in expressions are found where they are
/// compiled (`Compiler::runs_elsewhere`, `Compiler::member_runs_elsewhere`).
fn foreign_statement(s: &Statement, source: &str, tables: &Tables) -> bool {
    match &s.kind {
        S::Publish { .. }
        | S::Apply { .. }
        | S::WaitFor { .. }
        | S::Update { .. }
        | S::Set { .. } => true,
        S::DeleteObject { target, .. } => match &target.kind {
            E::Identifier(id) => match tables.types.get(&id.name.to_ascii_lowercase()) {
                Some(class) => !class.is_empty() && !tables.may_be_json(&id.name),
                None => !tables.names.contains(&id.name.to_ascii_lowercase()),
            },
            _ => false,
        },
        _ => crate::rules::syntactic::source_of(source, s.span).is_some_and(|t| {
            let mut words = t
                .split(|c: char| c.is_whitespace() || c == '.')
                .filter(|w| !w.is_empty());
            match words.next() {
                Some(w) if w.eq_ignore_ascii_case("PROCESS") => words
                    .next()
                    .is_some_and(|w| w.eq_ignore_ascii_case("EVENTS")),
                Some(w) => ["PROMPT-FOR", "CHOOSE", "INSERT"]
                    .iter()
                    .any(|k| k.eq_ignore_ascii_case(w)),
                None => false,
            }
        }),
    }
}

/// The routine units of the file that may run code the file does not hold,
/// themselves or through the routines they call.
pub(super) fn reaching_elsewhere(units: &[UnitIr]) -> FxHashSet<u32> {
    let mut out: FxHashSet<u32> = units
        .iter()
        .enumerate()
        .filter(|(_, ir)| !ir.foreign.is_empty())
        .map(|(u, _)| u as u32)
        .collect();
    loop {
        let before = out.len();
        for (u, ir) in units.iter().enumerate() {
            if ir
                .calls
                .iter()
                .any(|(_, c)| c.callees.iter().any(|callee| out.contains(callee)))
            {
                out.insert(u as u32);
            }
        }
        if out.len() == before {
            return out;
        }
    }
}

/// After each call of a routine of the file (a `RUN` of an internal
/// procedure, a function or a method), the file-level variables of `ir` that
/// a routine writes take what the routines wrote; in a main block, every
/// variable it declares is file-level. After a call that may run code the
/// file does not hold (see [`foreign_statement`]), or of a routine in
/// `elsewhere` that may, the [`Tables::shared`] names are Unknown: that code
/// may write them. That code may also run back any routine or trigger of the
/// file, so the file-level variables that a routine writes are Unknown by the
/// same rule.
///
/// Each write takes the span of its call, so that a sink in the same
/// statement, after the call, sees it.
pub(super) fn refresh_after_calls(
    ir: &mut UnitIr,
    elsewhere: &FxHashSet<u32>,
    written: &FxHashSet<String>,
    tables: &Tables,
) {
    let keys: Vec<(u32, String, String)> = ir
        .slots
        .info
        .iter()
        .enumerate()
        .filter(|(_, s)| (ir.main || s.global) && written.contains(&s.key))
        .map(|(i, s)| (i as u32, s.name.clone(), s.key.clone()))
        .collect();
    let shared: Vec<u32> = ir
        .slots
        .info
        .iter()
        .enumerate()
        .filter(|(_, s)| {
            let name = s.key.split_once('.').map_or(s.key.as_str(), |(t, _)| t);
            tables.shared.contains(name)
        })
        .map(|(i, _)| i as u32)
        .collect();
    if keys.is_empty() && shared.is_empty() {
        return;
    }
    let mut foreign: Vec<(NodeIx, Span)> = ir.foreign.clone();
    foreign.extend(
        ir.calls
            .iter()
            .filter(|(_, c)| c.callees.iter().any(|callee| elsewhere.contains(callee)))
            .map(|(n, c)| (*n, c.span)),
    );
    foreign.sort_by_key(|(n, span)| (n.index(), span.start, span.end));
    foreign.dedup();
    let mut refreshed: Vec<u32> = shared
        .into_iter()
        .chain(keys.iter().map(|(slot, ..)| *slot))
        .collect();
    refreshed.sort_unstable();
    refreshed.dedup();
    for (n, span) in foreign {
        for &slot in &refreshed {
            ir.ops[n.index()].push(Op {
                slot,
                value: Value::origin(UNKNOWN),
                strong: false,
                span,
            });
        }
    }
    let mut calls: Vec<(NodeIx, Span)> = ir.calls.iter().map(|(n, c)| (*n, c.span)).collect();
    calls.sort_by_key(|(n, span)| (n.index(), span.start, span.end));
    calls.dedup();
    for (n, span) in calls {
        for (slot, name, key) in &keys {
            ir.ops[n.index()].push(Op {
                slot: *slot,
                value: Value(vec![Term::File {
                    name: name.clone(),
                    key: key.clone(),
                }]),
                strong: false,
                span,
            });
        }
    }
}

/// What a routine returns and leaves in its output parameters, from a solve
/// in which its parameters hold only their own origins.
pub(super) fn summarize(
    cfg: &Cfg,
    ir: &UnitIr,
    solution: Option<&Solution<Provenance>>,
) -> UnitSummary {
    let params = ir.params.iter().map(|p| p.origin).collect();
    let Some(sol) = solution else {
        return UnitSummary {
            params,
            unknown: true,
            ..UnitSummary::default()
        };
    };
    // A procedure gives back its `RETURN-VALUE`, also when it raises an error.
    let ret = if ir.procedure {
        let mut ret = Labels::new();
        if let Some(s) = ir.slots.get(RETURN_VALUE) {
            for exit in [ExitKind::Return, ExitKind::Error]
                .into_iter()
                .filter_map(|k| cfg.exit(k))
            {
                oxabl_flow::provenance::merge(&mut ret, sol.before(exit).get(s as usize));
            }
        }
        if let Some(s) = ir.slots.get(ON_RETURN) {
            for (n, _) in cfg.nodes() {
                oxabl_flow::provenance::merge(&mut ret, sol.after(n).get(s as usize));
            }
        }
        ret
    } else {
        Labels::new()
    };
    let Some(exit) = cfg.exit(ExitKind::Return) else {
        return UnitSummary {
            ret,
            params,
            ..UnitSummary::default()
        };
    };
    let fact = sol.before(exit);
    let ret = match ir.slots.get(RETURN_SLOT) {
        Some(s) if !ir.procedure => fact.get(s as usize).iter().copied().collect(),
        _ => ret,
    };
    let outs = ir
        .params
        .iter()
        .map(|p| match (p.output, p.slot) {
            (true, Some(s)) => fact.get(s as usize).iter().copied().collect(),
            _ => Default::default(),
        })
        .collect();
    UnitSummary {
        ret,
        outs,
        params,
        unknown: false,
    }
}

/// Add what the calls of `ir` pass to each parameter of their callees.
pub(super) fn pass_arguments(
    ir: &UnitIr,
    solution: &Solution<Provenance>,
    env: Env,
    into: &mut Summaries,
) {
    let env = env.outside(ir.outside);
    for (node, call) in &ir.calls {
        let fact = solution.before(*node);
        for (pos, arg) in call.args.iter().enumerate() {
            let (labels, _) = arg.eval(fact, env);
            if labels.is_empty() {
                continue;
            }
            for &c in &call.callees {
                let entry = into.seeds.entry((c, pos as u32)).or_default();
                oxabl_flow::provenance::merge(entry, &labels);
            }
        }
    }
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

/// What a unit past its work budget leaves: `value::BUDGET` in every file-level
/// slot it writes and in every parameter it passes to a routine of the file.
pub(super) fn contribute_unknown(ir: &UnitIr, into: &mut Summaries) {
    let unknown = BUDGET_LABELS;
    for op in ir.ops.iter().flatten() {
        let slot = &ir.slots.info[op.slot as usize];
        if slot.global || ir.main {
            let entry = into.labels.entry(slot.key.clone()).or_default();
            oxabl_flow::provenance::merge(entry, &unknown);
        }
    }
    for (_, call) in &ir.calls {
        for pos in 0..call.args.len() {
            for &c in &call.callees {
                let entry = into.seeds.entry((c, pos as u32)).or_default();
                oxabl_flow::provenance::merge(entry, &unknown);
            }
        }
    }
}

/// A class member that code outside the file can set is a source: a
/// `PUBLIC` or `PROTECTED` variable, and a property of that access whose
/// `SET` is automatic and not `PRIVATE`. A computed `SET` is a unit whose
/// parameter is a source already.
pub(super) fn member_sources(
    program: &[Statement],
    source: &str,
    options: &Options,
    origins: &mut Origins,
    into: &mut Summaries,
) {
    if options.parameters == ParameterSources::None {
        return;
    }
    let all = options.parameters == ParameterSources::All;
    for s in program {
        let S::Class { body, .. } = &s.kind else {
            continue;
        };
        for m in body {
            let name = match &m.kind {
                S::Property {
                    name,
                    access,
                    set_body: Some(set),
                    ..
                } if set.is_empty()
                    && (all
                        || *access != AccessModifier::Private && !private_set(source, m.span)) =>
                {
                    name
                }
                S::VariableDeclaration { name, .. } if all || member_is_visible(source, m.span) => {
                    name
                }
                _ => continue,
            };
            let id = origins.add(format!("class member `{}`", name.name), m.span);
            let key = name.name.to_ascii_lowercase();
            oxabl_flow::provenance::merge(
                into.labels.entry(key.clone()).or_default(),
                &[(id, ALL)],
            );
            into.sites.entry(key).or_insert(m.span);
        }
    }
}

/// Whether a property says `PRIVATE SET`.
fn private_set(source: &str, span: Span) -> bool {
    let Some(st) = StatementText::over(source, span) else {
        return false;
    };
    (1..st.len()).any(|i| st.is_word(i, "SET") && st.is_word(i - 1, "PRIVATE"))
}

/// Whether a `DEFINE ... VARIABLE` says `PUBLIC` or `PROTECTED` before
/// `VARIABLE`.
fn member_is_visible(source: &str, span: Span) -> bool {
    let Some(st) = StatementText::over(source, span) else {
        return false;
    };
    (0..st.len())
        .take_while(|&i| !st.is_word(i, "VARIABLE") && !st.is_word(i, "VAR"))
        .any(|i| st.is_word(i, "PUBLIC") || st.is_word(i, "PROTECTED"))
}
