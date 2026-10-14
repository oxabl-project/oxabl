//! One walk over a file's statements: the effects of every statement, the
//! variables each unit defines, and the file-wide facts the units share
//! (which units free a name, which widget pools the file creates).

use oxabl_ast::{
    NodeId, OnAction, OnKind, ParameterDirection, ParameterType, Statement, StatementKind as SK,
};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::events::{self, Effect, Effects};
use crate::model::EscapeKind;

/// How a variable name resolves from one unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VarClass {
    /// Defined in the unit, not shared: an `INPUT` parameter counts.
    Local,
    /// Defined with an `EXTENT`.
    Array,
    OutputParameter,
    Shared,
    /// A data member or property of the class.
    Member,
}

#[derive(Default)]
struct Scope {
    parent: Option<NodeId>,
    defs: FxHashMap<String, VarClass>,
}

pub(crate) struct FileIndex<'a> {
    effects: FxHashMap<NodeId, Effects<'a>>,
    stmts: FxHashMap<NodeId, &'a Statement>,
    scopes: FxHashMap<NodeId, Scope>,
    /// Statements that create a tracked object, in source order.
    pub(crate) creating: Vec<NodeId>,
    /// Lowercased variable name to the units that free it.
    frees: FxHashMap<String, SmallVec<[NodeId; 2]>>,
    /// The main block creates an unnamed widget pool.
    pub(crate) main_pool: bool,
    /// Named pools the file creates, lowercased, with `PERSISTENT`.
    pub(crate) named_pools: FxHashMap<String, bool>,
    /// Named pools the file deletes, lowercased.
    pub(crate) deleted_pools: FxHashSet<String>,
}

impl<'a> FileIndex<'a> {
    pub(crate) fn build(program: &'a [Statement], src: &str) -> Self {
        let mut ix = FileIndex {
            effects: FxHashMap::default(),
            stmts: FxHashMap::default(),
            scopes: FxHashMap::default(),
            creating: Vec::new(),
            frees: FxHashMap::default(),
            main_pool: false,
            named_pools: FxHashMap::default(),
            deleted_pools: FxHashSet::default(),
        };
        ix.scopes.insert(NodeId::PROGRAM, Scope::default());
        ix.walk(program, NodeId::PROGRAM, false, src);
        ix
    }

    pub(crate) fn effects(&self, stmt: NodeId) -> &[Effect<'a>] {
        self.effects.get(&stmt).map_or(&[], |e| e.as_slice())
    }

    pub(crate) fn stmt(&self, stmt: NodeId) -> Option<&'a Statement> {
        self.stmts.get(&stmt).copied()
    }

    /// How `name` resolves from the unit defined by `unit`: `None` for a
    /// variable the unit owns, else the escape that storing a handle in it is.
    pub(crate) fn escape_of(&self, unit: NodeId, name: &str) -> Option<EscapeKind> {
        let key = name.to_ascii_lowercase();
        let mut cur = Some(unit);
        let mut own = true;
        while let Some(u) = cur {
            let Some(scope) = self.scopes.get(&u) else {
                break;
            };
            if let Some(&class) = scope.defs.get(&key) {
                return match (class, own) {
                    (VarClass::Local, true) => None,
                    (VarClass::OutputParameter, true) => Some(EscapeKind::OutputParameter),
                    (VarClass::Local | VarClass::OutputParameter, false) => {
                        Some(EscapeKind::OuterVariable)
                    }
                    (VarClass::Array, _) => Some(EscapeKind::ArrayElement),
                    (VarClass::Shared, _) => Some(EscapeKind::Shared),
                    (VarClass::Member, _) => Some(EscapeKind::Member),
                };
            }
            own = false;
            cur = scope.parent;
        }
        Some(EscapeKind::Unresolved)
    }

    /// Whether a unit other than `unit` frees a variable called `name`.
    pub(crate) fn freed_elsewhere(&self, unit: NodeId, name: &str) -> bool {
        self.frees
            .get(&name.to_ascii_lowercase())
            .is_some_and(|units| units.iter().any(|&u| u != unit))
    }

    fn define(&mut self, unit: NodeId, name: &str, class: VarClass) {
        self.scopes
            .entry(unit)
            .or_default()
            .defs
            .insert(name.to_ascii_lowercase(), class);
    }

    fn open(&mut self, unit: NodeId, parent: NodeId) {
        self.scopes.entry(unit).or_default().parent = Some(parent);
    }

    fn walk(&mut self, stmts: &'a [Statement], unit: NodeId, member: bool, src: &str) {
        for s in stmts {
            self.visit(s, unit, member, src);
        }
    }

    fn visit(&mut self, s: &'a Statement, unit: NodeId, member: bool, src: &str) {
        self.stmts.insert(s.id, s);
        let effects = events::effects(s, src);
        if !effects.is_empty() {
            self.note(s.id, unit, &effects);
            self.effects.insert(s.id, effects);
        }
        match &s.kind {
            SK::VariableDeclaration {
                name,
                extent,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                let class = if *is_new_shared || *is_shared || *is_new_global_shared {
                    VarClass::Shared
                } else if member {
                    VarClass::Member
                } else if extent.is_some() {
                    VarClass::Array
                } else {
                    VarClass::Local
                };
                self.define(unit, &name.name, class);
            }
            SK::DefineParameter {
                direction,
                param_type,
            } => {
                let name = match param_type {
                    ParameterType::Variable { name, .. }
                    | ParameterType::Handle { name, .. }
                    | ParameterType::Buffer { name, .. } => name,
                };
                let class = match direction {
                    ParameterDirection::Input => VarClass::Local,
                    _ => VarClass::OutputParameter,
                };
                self.define(unit, &name.name, class);
            }
            SK::Procedure { body, .. } | SK::Destructor { body } => {
                self.open(s.id, unit);
                self.walk(body, s.id, false, src);
            }
            SK::Function {
                body,
                prototype_parameters,
                ..
            } => {
                self.open(s.id, unit);
                self.walk(prototype_parameters, s.id, false, src);
                self.walk(body, s.id, false, src);
            }
            SK::Method {
                parameters, body, ..
            }
            | SK::Constructor {
                parameters, body, ..
            } => {
                self.open(s.id, unit);
                self.walk(parameters, s.id, false, src);
                self.walk(body, s.id, false, src);
            }
            SK::Property {
                name,
                get_body,
                set_body,
                set_parameters,
                ..
            } => {
                if member {
                    self.define(unit, &name.name, VarClass::Member);
                }
                self.open(s.id, unit);
                self.walk(set_parameters, s.id, false, src);
                for body in [get_body, set_body].into_iter().flatten() {
                    self.walk(body, s.id, false, src);
                }
            }
            SK::Class { body, .. } => self.walk(body, unit, true, src),
            SK::Interface { .. } => {}
            SK::On { kind } => {
                if let Some(action) = trigger_block(kind) {
                    self.open(s.id, unit);
                    match &action.kind {
                        SK::Do { body, .. } => {
                            self.stmts.insert(action.id, action);
                            self.walk(body, s.id, false, src);
                        }
                        _ => self.visit(action, s.id, false, src),
                    }
                }
            }
            SK::Do { body, .. }
            | SK::Repeat { body, .. }
            | SK::ForEach { body, .. }
            | SK::Catch { body, .. }
            | SK::Finally { body }
            | SK::Block(body) => self.walk(body, unit, member, src),
            SK::Label { body, .. } => self.visit(body, unit, member, src),
            SK::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.visit(then_branch, unit, member, src);
                if let Some(e) = else_branch {
                    self.visit(e, unit, member, src);
                }
            }
            SK::Case {
                when_branches,
                otherwise,
                ..
            } => {
                for w in when_branches {
                    self.walk(&w.body, unit, member, src);
                }
                if let Some(o) = otherwise {
                    self.walk(o, unit, member, src);
                }
            }
            SK::PreprocIf(p) => {
                self.walk(&p.then_branch, unit, member, src);
                for (_, b) in &p.elseif_branches {
                    self.walk(b, unit, member, src);
                }
                if let Some(e) = &p.else_branch {
                    self.walk(e, unit, member, src);
                }
            }
            _ => {}
        }
    }

    fn note(&mut self, stmt: NodeId, unit: NodeId, effects: &Effects<'a>) {
        if events::creates(effects) {
            self.creating.push(stmt);
        }
        for e in effects {
            match e {
                Effect::Free { var, .. } => {
                    let units = self.frees.entry(var.to_ascii_lowercase()).or_default();
                    if !units.contains(&unit) {
                        units.push(unit);
                    }
                }
                Effect::CreatePool {
                    name: None,
                    persistent: _,
                } if unit == NodeId::PROGRAM => self.main_pool = true,
                Effect::CreatePool {
                    name: Some(n),
                    persistent,
                } => {
                    let p = self.named_pools.entry(n.clone()).or_default();
                    *p |= *persistent;
                }
                Effect::DeletePool { name: Some(n) } => {
                    self.deleted_pools.insert(n.clone());
                }
                _ => {}
            }
        }
    }
}

fn trigger_block(kind: &OnKind) -> Option<&Statement> {
    match kind {
        OnKind::UiEvent {
            action: OnAction::Block(b),
            ..
        }
        | OnKind::DbEvent {
            action: OnAction::Block(b),
            ..
        } => Some(b),
        _ => None,
    }
}
