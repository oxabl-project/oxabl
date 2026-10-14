//! One walk over a file's statements: the effects of every statement, the
//! variables each unit defines, and the file-wide facts the units share
//! (which units free a name, which widget pools the file creates). The
//! per-statement facts are kept only for units that create something.

use std::cell::{Cell, RefCell};
use std::hash::{Hash, Hasher};

use oxabl_ast::{
    AccessModifier, DataType, Identifier, NodeId, OnAction, OnKind, ParameterDirection,
    ParameterType, Statement, StatementKind as SK, TypeSource,
};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::events::{self, Callee, Effect, Effects, Guards, Place, PoolRef, Receiver};
use crate::model::{EscapeKind, ObjectKind};

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

/// The declared type of a variable, as far as the analysis reads it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VarType<'a> {
    /// `HANDLE`, `WIDGET-HANDLE`, or a buffer, table or dataset parameter: its
    /// methods are built in.
    Handle,
    Memptr,
    Class(&'a str),
    Other,
}

#[derive(Debug, Clone, Copy)]
struct Def<'a> {
    class: VarClass,
    ty: VarType<'a>,
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

/// What a call by name can run in this file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallableKind {
    Procedure,
    Function,
    Method,
}

/// What a call of a callable runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Runs {
    /// The body the file defines.
    Body,
    /// Code another file may define: a method a subclass can override, or an
    /// `ABSTRACT` one.
    Elsewhere,
    /// A shared-library entry point (`PROCEDURE ... EXTERNAL`). It cannot free
    /// an ABL MEMPTR or take a handle as a handle, so it only borrows.
    Library,
}

#[derive(Debug, Clone, Copy)]
struct Callable {
    kind: CallableKind,
    unit: NodeId,
    runs: Runs,
}

#[derive(Default)]
struct Scope<'a> {
    parent: Option<NodeId>,
    defs: FxHashMap<Name<'a>, Def<'a>>,
    /// Static buffers, temp-tables, datasets, data-sources and frames.
    statics: FxHashSet<Name<'a>>,
}

/// What a callee does with a handle passed to it, weakest first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Passed {
    /// The callee only uses the handle.
    Borrow,
    /// The file does not show what the callee does.
    Unknown,
    /// The callee stores it outside itself, returns it or hands it on.
    Keeps,
    /// The callee frees it.
    Frees,
}

/// How many calls deep a handle handed on from callee to callee is followed.
const CALL_DEPTH: u8 = 4;

/// How many callee bodies one fate may read before it is taken as unknown.
const FATE_BUDGET: u32 = 1000;

/// What a unit does with a variable, for the units a call can run.
#[derive(Debug, Clone, Copy)]
enum Use<'a> {
    Free(&'a str),
    /// `to = from`, `from` a plain variable.
    Copy {
        to: Place<'a>,
        from: &'a str,
    },
    /// `to = from:ATTR`, a handle derived from `from`.
    Derived {
        to: Place<'a>,
        from: &'a str,
    },
    Escape(&'a str),
    Opaque(&'a str),
    Skipped(&'a [Identifier]),
    /// `recv:ADD-BUFFER(var)`.
    Attach {
        recv: Option<&'a str>,
        var: &'a str,
    },
    /// `CREATE DATASET var` or `CREATE QUERY var`.
    Created {
        kind: ObjectKind,
        var: &'a str,
    },
    Pass {
        var: &'a str,
        callee: Callee<'a>,
        pos: usize,
    },
}

/// What the analysis reads about one statement of a unit that creates
/// something.
pub(crate) struct StmtFacts<'a> {
    pub(crate) effects: Effects<'a>,
    /// The statement cannot raise, though the CFG gives it a raise edge.
    pub(crate) safe: bool,
    /// What an `IF` condition says about holders of a live object.
    pub(crate) guards: Guards<'a>,
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
    /// Procedures, functions and methods the file defines, by name.
    callables: FxHashMap<Name<'a>, SmallVec<[Callable; 1]>>,
    /// The parameter names of each unit, in order.
    params: FxHashMap<NodeId, Vec<&'a str>>,
    /// What each unit other than the main block does with variables.
    uses: FxHashMap<NodeId, Vec<Use<'a>>>,
    /// What a call's callee does with an argument, by callee unit and
    /// position.
    fates: RefCell<FxHashMap<(NodeId, usize), Passed>>,
    /// The parameters whose fate is being read, innermost last.
    reading: RefCell<SmallVec<[(NodeId, usize); 4]>>,
    /// The callee bodies the fate being read may still read.
    budget: Cell<u32>,
    /// The name of the file's class.
    class_name: Option<&'a str>,
    /// The file's class is `FINAL`.
    class_final: bool,
    /// The main block creates an unnamed widget pool.
    pub(crate) main_pool: bool,
    /// The file's class has `USE-WIDGET-POOL`.
    pub(crate) class_pool: bool,
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
            callables: FxHashMap::default(),
            params: FxHashMap::default(),
            uses: FxHashMap::default(),
            fates: RefCell::default(),
            reading: RefCell::default(),
            budget: Cell::new(0),
            class_name: None,
            class_final: false,
            main_pool: false,
            class_pool: false,
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
            if let Some(def) = scope.defs.get(&key) {
                return match (def.class, own) {
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

    /// The unit whose definitions the name `name` resolves to from `unit`;
    /// `None` when no unit of the file defines it.
    fn defining_unit(&self, unit: NodeId, name: &str) -> Option<NodeId> {
        let key = Name(name);
        let mut cur = Some(unit);
        while let Some(u) = cur {
            let scope = self.scopes.get(&u)?;
            if scope.defs.contains_key(&key) {
                return Some(u);
            }
            cur = scope.parent;
        }
        None
    }

    /// The definition `name` resolves to from `unit`.
    fn def_of(&self, unit: NodeId, name: &str) -> Option<Def<'a>> {
        let u = self.defining_unit(unit, name)?;
        self.scopes.get(&u)?.defs.get(&Name(name)).copied()
    }

    /// Whether `name` is a static object visible from `unit`.
    fn is_static(&self, unit: NodeId, name: &str) -> bool {
        let key = Name(name);
        let mut cur = Some(unit);
        while let Some(u) = cur {
            let Some(scope) = self.scopes.get(&u) else {
                return false;
            };
            if scope.statics.contains(&key) {
                return true;
            }
            cur = scope.parent;
        }
        false
    }

    /// Whether a unit other than `unit` frees the variable `name` resolves to
    /// from `unit`. A same-named variable of another unit is a different
    /// variable.
    pub(crate) fn freed_elsewhere(&self, unit: NodeId, name: &str) -> bool {
        let here = self.defining_unit(unit, name);
        self.frees.get(&Name(name)).is_some_and(|units| {
            units
                .iter()
                .any(|&u| u != unit && self.defining_unit(u, name) == here)
        })
    }

    /// What the callee of a call from `unit` does with a handle passed as its
    /// `pos`-th `INPUT` argument.
    pub(crate) fn passed(&self, unit: NodeId, callee: Callee, pos: usize) -> Passed {
        self.resolve(unit, callee, pos, 0)
    }

    fn resolve(&self, unit: NodeId, callee: Callee, pos: usize, depth: u8) -> Passed {
        let defined = match callee {
            Callee::External => None,
            Callee::Procedure(name) => self.defined(name, CallableKind::Procedure, pos, depth),
            // A call with no receiver in a class may name one of its methods.
            Callee::Function(name) => self.defined(name, CallableKind::Function, pos, depth),
            Callee::Method { name, recv } => return self.method(unit, name, recv, pos, depth),
        };
        defined.unwrap_or(Passed::Unknown)
    }

    /// A method call: a built-in method of a handle, a method of this class,
    /// or code the file does not show.
    fn method(&self, unit: NodeId, name: &str, recv: Receiver, pos: usize, depth: u8) -> Passed {
        let builtin = || {
            if events::is_builtin_method(name) {
                Passed::Borrow
            } else {
                Passed::Unknown
            }
        };
        let (root, chained) = match recv {
            Receiver::Name(n) => (n, false),
            Receiver::Chain(n) => (n, true),
            Receiver::Handle => return builtin(),
            Receiver::Other => return Passed::Unknown,
        };
        match self.def_of(unit, root).map(|d| d.ty) {
            Some(VarType::Handle) => builtin(),
            Some(VarType::Class(ty)) if !chained && self.is_this_class(ty) => self
                .defined(name, CallableKind::Method, pos, depth)
                .unwrap_or(Passed::Unknown),
            Some(_) => Passed::Unknown,
            None if root.eq_ignore_ascii_case("THIS-OBJECT") => {
                if chained {
                    Passed::Unknown
                } else {
                    self.defined(name, CallableKind::Method, pos, depth)
                        .unwrap_or(Passed::Unknown)
                }
            }
            None if self.is_static(unit, root) || events::is_system_handle(root) => builtin(),
            None => Passed::Unknown,
        }
    }

    fn is_this_class(&self, ty: &str) -> bool {
        let Some(class) = self.class_name else {
            return false;
        };
        fn last(n: &str) -> &str {
            n.rsplit('.').next().unwrap_or(n)
        }
        class.eq_ignore_ascii_case(ty)
            || ((!class.contains('.') || !ty.contains('.'))
                && last(class).eq_ignore_ascii_case(last(ty)))
    }

    /// The strongest fate over the callables of that name the file defines;
    /// `None` when it defines none.
    fn defined(&self, name: &str, kind: CallableKind, pos: usize, depth: u8) -> Option<Passed> {
        let named = |c: &&Callable| {
            c.kind == kind || (kind == CallableKind::Function && c.kind == CallableKind::Method)
        };
        self.callables
            .get(&Name(name))?
            .iter()
            .filter(named)
            .map(|c| match c.runs {
                Runs::Body => self.fate(c.unit, pos, depth),
                Runs::Elsewhere => Passed::Unknown,
                Runs::Library => Passed::Borrow,
            })
            .max()
    }

    /// What the callable `unit` does with its `pos`-th parameter: one walk of
    /// its uses, without regard to order, following plain copies to its own
    /// variables. A call back into a parameter being read adds nothing.
    fn fate(&self, unit: NodeId, pos: usize, depth: u8) -> Passed {
        if depth == 0 {
            if let Some(&f) = self.fates.borrow().get(&(unit, pos)) {
                return f;
            }
            self.budget.set(FATE_BUDGET);
        }
        if self.reading.borrow().contains(&(unit, pos)) {
            return Passed::Borrow;
        }
        let Some(left) = self.budget.get().checked_sub(1) else {
            return Passed::Unknown;
        };
        self.budget.set(left);
        self.reading.borrow_mut().push((unit, pos));
        let fate = self.read_fate(unit, pos, depth);
        self.reading.borrow_mut().pop();
        if depth == 0 {
            self.fates.borrow_mut().insert((unit, pos), fate);
        }
        fate
    }

    fn read_fate(&self, unit: NodeId, pos: usize, depth: u8) -> Passed {
        let Some(&param) = self.params.get(&unit).and_then(|p| p.get(pos)) else {
            return Passed::Unknown;
        };
        let uses = self.uses.get(&unit).map_or(&[][..], Vec::as_slice);
        let mut holders: SmallVec<[Name; 4]> = SmallVec::new();
        holders.push(Name(param));
        let holds = |h: &SmallVec<[Name; 4]>, v: &str| h.contains(&Name(v));
        let local = |v: &str| self.escape_of(unit, v).is_none();
        loop {
            let before = holders.len();
            for u in uses {
                if let Use::Copy {
                    to: Place::Var(w),
                    from,
                } = *u
                    && holds(&holders, from)
                    && !holds(&holders, w)
                    && local(w)
                {
                    holders.push(Name(w));
                }
            }
            if holders.len() == before {
                break;
            }
        }
        let mut fate = Passed::Borrow;
        for u in uses {
            let f = match *u {
                Use::Free(v) if holds(&holders, v) => Passed::Frees,
                Use::Copy { to, from } | Use::Derived { to, from } if holds(&holders, from) => {
                    match to {
                        Place::Var(w) if local(w) => Passed::Borrow,
                        _ => Passed::Keeps,
                    }
                }
                Use::Escape(v) if holds(&holders, v) => Passed::Keeps,
                // As in the caller: a dataset the unit creates owns it, a query
                // it creates does not, any other receiver may be either.
                Use::Attach { recv, var } if holds(&holders, var) => {
                    let created = recv.and_then(|r| {
                        uses.iter().find_map(|u| match *u {
                            Use::Created { kind, var } if Name(var) == Name(r) => Some(kind),
                            _ => None,
                        })
                    });
                    match created {
                        Some(ObjectKind::Dataset) => Passed::Keeps,
                        Some(_) => Passed::Borrow,
                        None => Passed::Unknown,
                    }
                }
                Use::Opaque(v) if holds(&holders, v) => Passed::Unknown,
                Use::Skipped(names) if names.iter().any(|n| holds(&holders, &n.name)) => {
                    Passed::Unknown
                }
                Use::Pass { var, callee, pos } if holds(&holders, var) => {
                    if depth + 1 >= CALL_DEPTH {
                        Passed::Unknown
                    } else {
                        self.resolve(unit, callee, pos, depth + 1)
                    }
                }
                _ => Passed::Borrow,
            };
            fate = fate.max(f);
            if fate == Passed::Frees {
                break;
            }
        }
        fate
    }

    /// A `COPY-LOB ... TO m` of a MEMPTR allocates it; any other target
    /// changes nothing this crate tracks. `m` is the definition it resolves to
    /// from `unit`.
    fn resolve_copy_lob(&self, unit: NodeId, effects: &mut Effects<'a>) {
        for e in effects.iter_mut() {
            if let Effect::CopyLob { to, no_error } = *e
                && let Some(var) = to.filter(|t| {
                    self.def_of(unit, t)
                        .is_some_and(|d| d.ty == VarType::Memptr)
                })
            {
                *e = Effect::Create {
                    kind: ObjectKind::Memptr,
                    into: Some(Place::Var(var)),
                    pool: PoolRef::Default,
                    no_error,
                    weak: true,
                };
            }
        }
        effects.retain(|e| !matches!(e, Effect::CopyLob { .. }));
    }

    fn callable(&mut self, name: &'a Identifier, kind: CallableKind, unit: NodeId, runs: Runs) {
        self.callables
            .entry(Name(&name.name))
            .or_default()
            .push(Callable { kind, unit, runs });
    }

    fn define(&mut self, unit: NodeId, name: &'a str, class: VarClass, ty: VarType<'a>) {
        self.scopes
            .entry(unit)
            .or_default()
            .defs
            .insert(Name(name), Def { class, ty });
    }

    fn define_static(&mut self, unit: NodeId, name: &'a Identifier) {
        self.scopes
            .entry(unit)
            .or_default()
            .statics
            .insert(Name(&name.name));
    }

    /// Keep what a unit that a call can run does with variables.
    fn record_uses(&mut self, unit: NodeId, effects: &Effects<'a>) {
        let uses = effects.iter().filter_map(|e| {
            Some(match *e {
                Effect::Free { var, .. } => Use::Free(var),
                Effect::Assign {
                    to,
                    from: Some(from),
                } => Use::Copy { to, from },
                Effect::DerivedAssign { to, from } => Use::Derived { to, from },
                Effect::Escape { var, .. } => Use::Escape(var),
                Effect::Opaque { var, .. } => Use::Opaque(var),
                Effect::Attach { recv, var } => Use::Attach { recv, var },
                Effect::Create {
                    kind: kind @ (ObjectKind::Dataset | ObjectKind::Query),
                    into: Some(Place::Var(var)),
                    ..
                } => Use::Created { kind, var },
                Effect::Skipped(names) => Use::Skipped(names),
                Effect::Pass { var, callee, pos } => Use::Pass { var, callee, pos },
                _ => return None,
            })
        });
        let mut uses = uses.peekable();
        if uses.peek().is_some() {
            self.uses.entry(unit).or_default().extend(uses);
        }
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
        let mut effects = events::effects(s, src);
        self.resolve_copy_lob(unit, &mut effects);
        if !effects.is_empty() {
            self.note(s.id, unit, &effects);
            if unit != NodeId::PROGRAM {
                self.record_uses(unit, &effects);
            }
        }
        let safe = events::cannot_raise(s);
        let guards = match &s.kind {
            SK::If { condition, .. } => events::guards(condition),
            _ => Guards::new(),
        };
        if !effects.is_empty() || safe || !guards.is_empty() {
            self.cur.stmts.push((
                s.id,
                StmtFacts {
                    effects,
                    safe,
                    guards,
                },
            ));
        }
        match &s.kind {
            SK::VariableDeclaration {
                name,
                type_source,
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
                self.define(unit, &name.name, class, var_type(type_source));
            }
            SK::DefineParameter {
                direction,
                param_type,
            } => {
                let (name, ty) = match param_type {
                    ParameterType::Variable {
                        name, type_source, ..
                    } => (name, var_type(type_source)),
                    // A table or dataset parameter names a static object.
                    ParameterType::Handle { name, .. } | ParameterType::Buffer { name, .. } => {
                        (name, VarType::Handle)
                    }
                };
                self.params.entry(unit).or_default().push(&name.name);
                let class = match direction {
                    ParameterDirection::Input => VarClass::Local,
                    _ => VarClass::OutputParameter,
                };
                self.define(unit, &name.name, class, ty);
            }
            SK::DefineBuffer { name, .. }
            | SK::DefineTempTable { name, .. }
            | SK::DefineDataset { name, .. }
            | SK::DefineDataSource { name, .. }
            | SK::DefineFrame { name, .. } => self.define_static(unit, name),
            SK::Procedure {
                name,
                body,
                is_prototype,
                is_external,
            } => {
                // `IN SUPER` and `IN h` run code of another file.
                if *is_external {
                    self.callable(name, CallableKind::Procedure, s.id, Runs::Library);
                } else if !is_prototype {
                    self.callable(name, CallableKind::Procedure, s.id, Runs::Body);
                }
                self.unit(s.id, unit, |ix| ix.walk(body, s.id, false, src));
            }
            SK::Destructor { body } => {
                self.unit(s.id, unit, |ix| ix.walk(body, s.id, false, src));
            }
            SK::Function {
                name,
                body,
                prototype_parameters,
                ..
            } => {
                // A `FORWARD`, `IN` or `MAP TO` prototype has no body. Only a
                // `FORWARD` one is defined in the file, by its own definition.
                if !body.is_empty() {
                    self.callable(name, CallableKind::Function, s.id, Runs::Body);
                }
                self.unit(s.id, unit, |ix| {
                    ix.walk(prototype_parameters, s.id, false, src);
                    ix.walk(body, s.id, false, src);
                });
            }
            SK::Method {
                access,
                is_static,
                is_abstract,
                name,
                parameters,
                body,
                ..
            } => {
                let runs = if !is_abstract
                    && (*is_static || *access == AccessModifier::Private || self.class_final)
                {
                    Runs::Body
                } else {
                    Runs::Elsewhere
                };
                self.callable(name, CallableKind::Method, s.id, runs);
                self.unit(s.id, unit, |ix| {
                    ix.walk(parameters, s.id, false, src);
                    ix.walk(body, s.id, false, src);
                });
            }
            SK::Constructor {
                parameters, body, ..
            } => {
                self.unit(s.id, unit, |ix| {
                    ix.walk(parameters, s.id, false, src);
                    ix.walk(body, s.id, false, src);
                });
            }
            SK::Property {
                name,
                data_type,
                get_body,
                set_body,
                set_parameters,
                ..
            } => {
                if member {
                    self.define(unit, &name.name, VarClass::Member, data_var_type(data_type));
                }
                self.unit(s.id, unit, |ix| {
                    ix.walk(set_parameters, s.id, false, src);
                    for body in [get_body, set_body].into_iter().flatten() {
                        ix.walk(body, s.id, false, src);
                    }
                });
            }
            SK::Class {
                name,
                body,
                use_widget_pool,
                is_final,
                ..
            } => {
                self.class_name = Some(&name.name);
                self.class_final = *is_final;
                self.class_pool |= *use_widget_pool;
                self.walk(body, unit, true, src)
            }
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
            | SK::Block(body)
            | SK::VariableDeclarationList(body) => self.walk(body, unit, member, src),
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
                Effect::Free { var, .. } => {
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

fn var_type(t: &TypeSource) -> VarType<'_> {
    match t {
        TypeSource::Explicit(d) => data_var_type(d),
        TypeSource::Like { .. } => VarType::Other,
    }
}

fn data_var_type(d: &DataType) -> VarType<'_> {
    match d {
        DataType::Handle => VarType::Handle,
        DataType::Memptr => VarType::Memptr,
        // The parser reads `WIDGET-HANDLE` as a class name.
        DataType::Class(n) if n.eq_ignore_ascii_case("WIDGET-HANDLE") => VarType::Handle,
        _ => d.class_name().map_or(VarType::Other, VarType::Class),
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
