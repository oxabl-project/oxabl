//! Scope tree.
//!
//! The scope tree is an arena of [`Scope`] records with parent-pointer
//! linking. Every AST construct that introduces a lexical scope — file
//! root, procedure, function, class, method, property accessor,
//! constructor, destructor, `DO` / `REPEAT` / `FOR` block, CATCH, trigger —
//! gets one [`Scope`]. Lookups walk parents; each scope carries one
//! [`BindingMap`] per namespace (indexed by
//! [`NamespaceId`](crate::NamespaceId)).

use oxabl_ast::NodeId;
use oxabl_lexer::oxabl_atom::OxablAtom;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{NUM_NAMESPACES, NamespaceId, SymbolId};

/// Dense arena index for a scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl ScopeId {
    /// Reserved id for the implicit file-level scope.
    pub const ROOT: ScopeId = ScopeId(0);

    #[inline]
    pub const fn new(n: u32) -> Self {
        ScopeId(n)
    }

    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// What kind of lexical scope this is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Implicit top-level scope of a file. Always id `ScopeId::ROOT`.
    File,
    Procedure,
    Function,
    Class,
    Interface,
    Method,
    PropertyGet,
    PropertySet,
    Constructor,
    Destructor,
    /// `DO`/`REPEAT`/`FOR` block scope. The block kind is recorded on the
    /// owning AST node, not the scope record.
    Block,
    /// `CATCH` block scope; the exception variable binds here.
    Catch,
    /// `FINALLY` block scope.
    Finally,
    /// `ON ... DO: ... END.` trigger block.
    Trigger,
    /// Implicit scope for a `DEFINE FRAME`.
    Frame,
    /// Implicit scope for a `DEFINE TEMP-TABLE`; its `FIELD`s bind here so
    /// identically-named fields in different temp-tables never collide.
    TempTable,
    /// Implicit scope for a `TRIGGER PROCEDURE` file.
    TriggerProcedure,
}

impl ScopeKind {
    /// Whether this scope is *transparent* to `DEFINE VARIABLE` scoping.
    ///
    /// ABL scopes a `DEFINE VARIABLE` to its enclosing routine (the main
    /// procedure body, an internal procedure, a user-defined function, a
    /// method/constructor/destructor/property accessor), never to the
    /// `DO`/`FOR`/`REPEAT`/`CATCH`/`FINALLY` block it textually sits in.
    /// Placing a `DEFINE VARIABLE` inside such a block is purely stylistic —
    /// the name is visible throughout the routine (after its textual
    /// definition). These block kinds are therefore "seen through" when
    /// choosing where a variable binds.
    ///
    /// Note this governs only where a `DEFINE VARIABLE` *binds*; block-local
    /// bindings introduced by the block itself (a `DO` loop counter, a
    /// `CATCH` error variable) are declared directly against the block scope
    /// and are unaffected.
    pub fn is_var_transparent(self) -> bool {
        matches!(
            self,
            ScopeKind::Block | ScopeKind::Catch | ScopeKind::Finally
        )
    }
}

/// Per-namespace bindings for one [`Scope`].
///
/// For ≤ `BINDING_MAP_SMALL_CAP` bindings — the overwhelming majority of ABL
/// scopes per prototype measurement — a `SmallVec` with linear scan beats a
/// `FxHashMap` for atom equality comparisons (no hash, no indirection).
/// Spills to the hashmap variant past the threshold.
#[derive(Debug, Clone)]
pub enum BindingMap {
    Small(SmallVec<[(OxablAtom, SymbolId); BINDING_MAP_SMALL_CAP]>),
    Large(FxHashMap<OxablAtom, SymbolId>),
}

/// Inline capacity at which [`BindingMap::Small`] spills into
/// [`BindingMap::Large`]. Chosen to cover the vast majority of ABL method
/// bodies, blocks, and procedures without heap allocation.
pub const BINDING_MAP_SMALL_CAP: usize = 8;

impl Default for BindingMap {
    fn default() -> Self {
        BindingMap::Small(SmallVec::new())
    }
}

impl BindingMap {
    /// Case-insensitive name lookup. Atoms are already case-folded at
    /// intern time, so this is a raw atom equality scan.
    pub fn get(&self, name: &OxablAtom) -> Option<SymbolId> {
        match self {
            BindingMap::Small(vec) => vec.iter().find(|(n, _)| n == name).map(|(_, id)| *id),
            BindingMap::Large(map) => map.get(name).copied(),
        }
    }

    /// Insert `(name, sym)`. If a binding for `name` already exists, the
    /// prior id is returned so the declare pass can raise `SEM0001`.
    pub fn insert(&mut self, name: OxablAtom, sym: SymbolId) -> Option<SymbolId> {
        // Spill small → large once we grow past the inline cap.
        if let BindingMap::Small(vec) = self
            && vec.len() >= BINDING_MAP_SMALL_CAP
            && vec.iter().all(|(n, _)| *n != name)
        {
            let mut map: FxHashMap<OxablAtom, SymbolId> =
                FxHashMap::with_capacity_and_hasher(vec.len() + 1, Default::default());
            for (n, s) in vec.drain(..) {
                map.insert(n, s);
            }
            *self = BindingMap::Large(map);
        }

        match self {
            BindingMap::Small(vec) => {
                if let Some(entry) = vec.iter_mut().find(|(n, _)| *n == name) {
                    let prior = entry.1;
                    entry.1 = sym;
                    Some(prior)
                } else {
                    vec.push((name, sym));
                    None
                }
            }
            BindingMap::Large(map) => map.insert(name, sym),
        }
    }

    /// Iterate over every `(name, SymbolId)` pair. Order is unspecified.
    pub fn iter(&self) -> Box<dyn Iterator<Item = (&OxablAtom, SymbolId)> + '_> {
        match self {
            BindingMap::Small(vec) => Box::new(vec.iter().map(|(n, s)| (n, *s))),
            BindingMap::Large(map) => Box::new(map.iter().map(|(n, s)| (n, *s))),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            BindingMap::Small(vec) => vec.len(),
            BindingMap::Large(map) => map.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// One scope in the tree.
#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    /// The AST node that introduced this scope. For `ScopeKind::File`,
    /// this is [`NodeId::PROGRAM`]; for everything else it's the node id
    /// of the statement that opened the scope.
    pub owner_node: NodeId,
    /// Per-namespace bindings. Indexed by
    /// [`NamespaceId::index`](crate::NamespaceId::index).
    pub bindings: [BindingMap; NUM_NAMESPACES],
}

impl Scope {
    fn new(kind: ScopeKind, parent: Option<ScopeId>, owner_node: NodeId) -> Self {
        Scope {
            kind,
            parent,
            owner_node,
            bindings: std::array::from_fn(|_| BindingMap::default()),
        }
    }

    /// Look up `name` in the requested namespace of this scope only — does
    /// not walk parents.
    #[inline]
    pub fn get_in(&self, ns: NamespaceId, name: &OxablAtom) -> Option<SymbolId> {
        self.bindings[ns.index()].get(name)
    }
}

/// Arena of scopes with a fixed root. Scopes are never removed; the tree
/// grows monotonically during the declare pass.
#[derive(Debug, Clone)]
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl ScopeTree {
    /// Construct a tree with the implicit [`ScopeId::ROOT`] file scope
    /// already created.
    pub fn new() -> Self {
        ScopeTree {
            scopes: vec![Scope::new(ScopeKind::File, None, NodeId::PROGRAM)],
        }
    }

    /// Push a new scope under `parent` and return its id.
    pub fn push(&mut self, kind: ScopeKind, parent: ScopeId, owner_node: NodeId) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope::new(kind, Some(parent), owner_node));
        id
    }

    pub fn get(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.raw() as usize]
    }

    pub fn get_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id.raw() as usize]
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes
            .iter()
            .enumerate()
            .map(|(i, s)| (ScopeId(i as u32), s))
    }

    /// The scope a `DEFINE VARIABLE` at `scope` actually binds into.
    ///
    /// Walks up from `scope` skipping [var-transparent](ScopeKind::is_var_transparent)
    /// block scopes and returns the first enclosing routine (or class/file)
    /// scope. When `scope` is not itself a block, it is returned unchanged.
    /// The root file scope is var-transparent-free, so this always terminates.
    pub fn var_binding_scope(&self, scope: ScopeId) -> ScopeId {
        self.ancestors(scope)
            .find(|id| !self.scopes[id.raw() as usize].kind.is_var_transparent())
            .unwrap_or(ScopeId::ROOT)
    }

    /// Walk from `scope` up through parents, yielding each id in order.
    pub fn ancestors(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(Some(scope), move |cur| {
            self.scopes[cur.raw() as usize].parent
        })
    }

    /// Resolve `name` in namespace `ns`, walking parents. Returns the first
    /// binding encountered (shadowing preserves ABL semantics).
    pub fn resolve(&self, scope: ScopeId, ns: NamespaceId, name: &OxablAtom) -> Option<SymbolId> {
        for id in self.ancestors(scope) {
            if let Some(sym) = self.scopes[id.raw() as usize].get_in(ns, name) {
                return Some(sym);
            }
        }
        None
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_schema::Schema;

    fn atom(s: &str) -> OxablAtom {
        OxablAtom::from(s)
    }

    #[test]
    fn root_scope_exists() {
        let tree = ScopeTree::new();
        assert_eq!(tree.len(), 1);
        let root = tree.get(ScopeId::ROOT);
        assert_eq!(root.kind, ScopeKind::File);
        assert_eq!(root.parent, None);
    }

    #[test]
    fn push_produces_dense_ids() {
        let mut tree = ScopeTree::new();
        let a = tree.push(ScopeKind::Procedure, ScopeId::ROOT, NodeId::from_u32(10));
        let b = tree.push(ScopeKind::Block, a, NodeId::from_u32(11));
        assert_eq!(a.raw(), 1);
        assert_eq!(b.raw(), 2);
        assert_eq!(tree.get(b).parent, Some(a));
    }

    #[test]
    fn ancestors_walks_parents() {
        let mut tree = ScopeTree::new();
        let a = tree.push(ScopeKind::Procedure, ScopeId::ROOT, NodeId::from_u32(1));
        let b = tree.push(ScopeKind::Block, a, NodeId::from_u32(2));
        let chain: Vec<_> = tree.ancestors(b).map(|s| s.raw()).collect();
        assert_eq!(chain, vec![2, 1, 0]);
    }

    #[test]
    fn binding_map_small_then_large() {
        let mut map = BindingMap::default();
        for i in 0..(BINDING_MAP_SMALL_CAP as u32) {
            let prior = map.insert(atom(&format!("n{i}")), SymbolId::new(i));
            assert!(prior.is_none());
        }
        assert!(matches!(map, BindingMap::Small(_)));
        // Exceeding cap triggers spill.
        let prior = map.insert(atom("over"), SymbolId::new(99));
        assert!(prior.is_none());
        assert!(matches!(map, BindingMap::Large(_)));
        assert_eq!(map.get(&atom("n0")), Some(SymbolId::new(0)));
        assert_eq!(map.get(&atom("over")), Some(SymbolId::new(99)));
    }

    #[test]
    fn binding_map_duplicate_returns_prior() {
        let mut map = BindingMap::default();
        assert!(map.insert(atom("x"), SymbolId::new(1)).is_none());
        assert_eq!(
            map.insert(atom("x"), SymbolId::new(2)),
            Some(SymbolId::new(1))
        );
        assert_eq!(map.get(&atom("x")), Some(SymbolId::new(2)));
    }

    #[test]
    fn resolve_walks_parents_shadow_aware() {
        let mut tree = ScopeTree::new();
        let a = tree.push(ScopeKind::Procedure, ScopeId::ROOT, NodeId::from_u32(1));
        let b = tree.push(ScopeKind::Block, a, NodeId::from_u32(2));
        tree.get_mut(ScopeId::ROOT).bindings[NamespaceId::Values.index()]
            .insert(atom("outer"), SymbolId::new(1));
        tree.get_mut(a).bindings[NamespaceId::Values.index()]
            .insert(atom("inner"), SymbolId::new(2));
        tree.get_mut(b).bindings[NamespaceId::Values.index()]
            .insert(atom("outer"), SymbolId::new(3));

        assert_eq!(
            tree.resolve(b, NamespaceId::Values, &atom("outer")),
            Some(SymbolId::new(3))
        );
        assert_eq!(
            tree.resolve(b, NamespaceId::Values, &atom("inner")),
            Some(SymbolId::new(2))
        );
        assert_eq!(
            tree.resolve(ScopeId::ROOT, NamespaceId::Values, &atom("inner")),
            None
        );
    }

    #[test]
    fn var_binding_scope_hoists_through_blocks() {
        let mut tree = ScopeTree::new();
        let proc = tree.push(ScopeKind::Procedure, ScopeId::ROOT, NodeId::from_u32(1));
        let outer = tree.push(ScopeKind::Block, proc, NodeId::from_u32(2));
        let inner = tree.push(ScopeKind::Block, outer, NodeId::from_u32(3));
        let catch = tree.push(ScopeKind::Catch, inner, NodeId::from_u32(4));
        // From any depth of blocks/catch, a variable binds at the routine.
        assert_eq!(tree.var_binding_scope(inner), proc);
        assert_eq!(tree.var_binding_scope(catch), proc);
        assert_eq!(tree.var_binding_scope(outer), proc);
        // A non-block scope is returned unchanged.
        assert_eq!(tree.var_binding_scope(proc), proc);
        assert_eq!(tree.var_binding_scope(ScopeId::ROOT), ScopeId::ROOT);
    }

    #[test]
    fn var_binding_scope_stops_at_trigger_boundary() {
        // A trigger block is a genuine boundary — variables do not hoist out.
        let mut tree = ScopeTree::new();
        let proc = tree.push(ScopeKind::Procedure, ScopeId::ROOT, NodeId::from_u32(1));
        let trig = tree.push(ScopeKind::Trigger, proc, NodeId::from_u32(2));
        let blk = tree.push(ScopeKind::Block, trig, NodeId::from_u32(3));
        assert_eq!(tree.var_binding_scope(blk), trig);
    }

    // Keep a smoke-test that the Schema dep compiles through the crate tree.
    #[test]
    fn schema_dep_reachable() {
        let _ = Schema::empty();
    }
}
