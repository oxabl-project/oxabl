//! One walk over a file's statements: the effects of every statement, the
//! variables each unit defines, and the file-wide facts the units share
//! (which units free a name, which widget pools the file creates). The
//! per-statement facts are kept only for units that create something.

use std::hash::{Hash, Hasher};

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

/// A name compared without ASCII case, as ABL compares names.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Name<'a>(pub(crate) &'a str);

impl PartialEq for Name<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(other.0)
    }
}

impl Eq for Name<'_> {}

impl Hash for Name<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for b in self.0.bytes() {
            state.write_u8(b.to_ascii_lowercase());
        }
    }
}

#[derive(Default)]
struct Scope<'a> {
    parent: Option<NodeId>,
    defs: FxHashMap<Name<'a>, VarClass>,
}

/// What the analysis reads about one statement of a unit that creates
/// something.
pub(crate) struct StmtFacts<'a> {
    pub(crate) effects: Effects<'a>,
    /// The statement cannot raise, though the CFG gives it a raise edge.
    pub(crate) safe: bool,
    /// The variable an `IF` tests for an invalid handle, and the value of the
    /// condition when it is invalid.
    pub(crate) guard: Option<(&'a str, bool)>,
}

/// The facts of the unit being walked.
#[derive(Default)]
struct UnitFacts<'a> {
    stmts: Vec<(NodeId, StmtFacts<'a>)>,
    creates: bool,
}

pub(crate) struct FileIndex<'a> {
    /// The statement facts of each unit that creates something, by the
    /// unit's defining statement.
    units: FxHashMap<NodeId, FxHashMap<NodeId, StmtFacts<'a>>>,
    cur: UnitFacts<'a>,
    scopes: FxHashMap<NodeId, Scope<'a>>,
    /// Statements that create a tracked object, in source order.
    pub(crate) creating: Vec<NodeId>,
    /// Variable name to the units that free it.
    frees: FxHashMap<Name<'a>, SmallVec<[NodeId; 2]>>,
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
            units: FxHashMap::default(),
            cur: UnitFacts::default(),
            scopes: FxHashMap::default(),
            creating: Vec::new(),
            frees: FxHashMap::default(),
            main_pool: false,
            named_pools: FxHashMap::default(),
            deleted_pools: FxHashSet::default(),
        };
        ix.scopes.insert(NodeId::PROGRAM, Scope::default());
        ix.walk(program, NodeId::PROGRAM, false, src);
        let main = std::mem::take(&mut ix.cur);
        if main.creates {
            ix.units
                .insert(NodeId::PROGRAM, main.stmts.into_iter().collect());
        }
        ix
    }

    /// The facts of the statements of a unit that creates something, by
    /// statement.
    pub(crate) fn unit_facts(&self, unit: NodeId) -> Option<&FxHashMap<NodeId, StmtFacts<'a>>> {
        self.units.get(&unit)
    }

    /// How `name` resolves from the unit defined by `unit`: `None` for a
    /// variable the unit owns, else the escape that storing a handle in it is.
    pub(crate) fn escape_of(&self, unit: NodeId, name: &str) -> Option<EscapeKind> {
        let key = Name(name);
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
            .get(&Name(name))
            .is_some_and(|units| units.iter().any(|&u| u != unit))
    }

    fn define(&mut self, unit: NodeId, name: &'a str, class: VarClass) {
        self.scopes
            .entry(unit)
            .or_default()
            .defs
            .insert(Name(name), class);
    }

    /// Walk the statements of the unit `unit` defines, nested in `parent`.
    fn unit(&mut self, unit: NodeId, parent: NodeId, walk: impl FnOnce(&mut Self)) {
        self.scopes.entry(unit).or_default().parent = Some(parent);
        let outer = std::mem::take(&mut self.cur);
        walk(self);
        let own = std::mem::replace(&mut self.cur, outer);
        if own.creates {
            // Property accessors share their property's statement.
            self.units.entry(unit).or_default().extend(own.stmts);
        }
    }

    fn walk(&mut self, stmts: &'a [Statement], unit: NodeId, member: bool, src: &str) {
        for s in stmts {
            self.visit(s, unit, member, src);
        }
    }

    fn visit(&mut self, s: &'a Statement, unit: NodeId, member: bool, src: &str) {
        let effects = events::effects(s, src);
        if !effects.is_empty() {
            self.note(s.id, unit, &effects);
        }
        let safe = events::cannot_raise(s);
        let guard = match &s.kind {
            SK::If { condition, .. } => events::invalid_test(condition),
            _ => None,
        };
        if !effects.is_empty() || safe || guard.is_some() {
            self.cur.stmts.push((
                s.id,
                StmtFacts {
                    effects,
                    safe,
                    guard,
                },
            ));
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
                self.unit(s.id, unit, |ix| ix.walk(body, s.id, false, src));
            }
            SK::Function {
                body,
                prototype_parameters,
                ..
            } => {
                self.unit(s.id, unit, |ix| {
                    ix.walk(prototype_parameters, s.id, false, src);
                    ix.walk(body, s.id, false, src);
                });
            }
            SK::Method {
                parameters, body, ..
            }
            | SK::Constructor {
                parameters, body, ..
            } => {
                self.unit(s.id, unit, |ix| {
                    ix.walk(parameters, s.id, false, src);
                    ix.walk(body, s.id, false, src);
                });
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
                self.unit(s.id, unit, |ix| {
                    ix.walk(set_parameters, s.id, false, src);
                    for body in [get_body, set_body].into_iter().flatten() {
                        ix.walk(body, s.id, false, src);
                    }
                });
            }
            SK::Class { body, .. } => self.walk(body, unit, true, src),
            SK::Interface { .. } => {}
            SK::On { kind } => {
                if let Some(action) = trigger_block(kind) {
                    self.unit(s.id, unit, |ix| match &action.kind {
                        SK::Do { body, .. } => ix.walk(body, s.id, false, src),
                        _ => ix.visit(action, s.id, false, src),
                    });
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
            self.cur.creates = true;
        }
        for e in effects {
            match e {
                Effect::Free { var, .. } | Effect::DeleteNamed { var, .. } => {
                    let units = self.frees.entry(Name(var)).or_default();
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
