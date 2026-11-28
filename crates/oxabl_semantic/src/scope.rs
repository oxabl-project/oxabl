//! Scope tree.
//!
//! The scope tree is an arena of [`Scope`] records with parent-pointer
//! linking. Every AST construct that introduces a lexical scope — file
//! root, procedure, function, class, method, property accessor,
//! constructor, destructor, `DO` / `REPEAT` / `FOR` block, CATCH, trigger —
//! gets one [`Scope`]. Lookups walk parents; each scope carries one
//! [`BindingMap`] for each namespace it binds a name in
//! ([`ScopeBindings`]).

use oxabl_ast::{FoldedNameId, FoldedNames, NodeId};
use oxabl_common::VirtualSpan;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use crate::{NamespaceId, SymbolId};

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    /// The same rule holds for every other `DEFINE` statement (`BUFFER`,
    /// `TEMP-TABLE`, `PARAMETER`, `STREAM`, `FRAME`, `DATASET`, ...) and for a
    /// `FUNCTION` or `PROCEDURE` written inside a block.
    ///
    /// Note this governs only where a definition *binds*; block-local
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
/// A name is a [`FoldedNameId`] of the tree's [`FoldedNames`], so a lookup
/// compares `u32`s and never folds or hashes text.
///
/// For ≤ `BINDING_MAP_SMALL_CAP` bindings — the overwhelming majority of ABL
/// scopes per prototype measurement — a `SmallVec` with linear scan beats a
/// `FxHashMap` for id equality comparisons (no hash, no indirection).
/// Spills to the hashmap variant past the threshold.
#[derive(Debug, Clone)]
pub enum BindingMap {
    Small(SmallVec<[(FoldedNameId, SymbolId); BINDING_MAP_SMALL_CAP]>),
    Large(FxHashMap<FoldedNameId, SymbolId>),
}

// Lock the size of a binding map deliberately: a scope holds one for each
// namespace it binds a name in. Gated to 64-bit; bump the bound deliberately.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<BindingMap>() <= 80);

/// Inline capacity at which [`BindingMap::Small`] spills into
/// [`BindingMap::Large`]. A map exists only for a namespace that binds a
/// name, and nearly all such maps hold 8 names or fewer.
pub const BINDING_MAP_SMALL_CAP: usize = 8;

impl Default for BindingMap {
    fn default() -> Self {
        BindingMap::Small(SmallVec::new())
    }
}

impl BindingMap {
    /// Case-insensitive name lookup. Names are already case-folded in the
    /// table that issued the id, so this is a raw id equality scan.
    pub fn get(&self, name: FoldedNameId) -> Option<SymbolId> {
        match self {
            BindingMap::Small(vec) => vec.iter().find(|(n, _)| *n == name).map(|(_, id)| *id),
            BindingMap::Large(map) => map.get(&name).copied(),
        }
    }

    /// Insert `(name, sym)`. If a binding for `name` already exists, the
    /// prior id is returned so the declare pass can raise `SEM0001`.
    pub fn insert(&mut self, name: FoldedNameId, sym: SymbolId) -> Option<SymbolId> {
        // Spill small → large once we grow past the inline cap.
        if let BindingMap::Small(vec) = self
            && vec.len() >= BINDING_MAP_SMALL_CAP
            && vec.iter().all(|(n, _)| *n != name)
        {
            let mut map: FxHashMap<FoldedNameId, SymbolId> =
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
    pub fn iter(&self) -> BindingIter<'_> {
        match self {
            BindingMap::Small(vec) => BindingIter(BindingIterInner::Small(vec.iter())),
            BindingMap::Large(map) => BindingIter(BindingIterInner::Large(map.iter())),
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

/// The iterator of [`BindingMap::iter`].
#[derive(Clone)]
pub struct BindingIter<'a>(BindingIterInner<'a>);

#[derive(Clone)]
enum BindingIterInner<'a> {
    Small(std::slice::Iter<'a, (FoldedNameId, SymbolId)>),
    Large(std::collections::hash_map::Iter<'a, FoldedNameId, SymbolId>),
}

impl<'a> Iterator for BindingIter<'a> {
    type Item = (FoldedNameId, SymbolId);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            BindingIterInner::Small(it) => it.next().map(|(n, s)| (*n, *s)),
            BindingIterInner::Large(it) => it.next().map(|(n, s)| (*n, *s)),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            BindingIterInner::Small(it) => it.size_hint(),
            BindingIterInner::Large(it) => it.size_hint(),
        }
    }
}

impl ExactSizeIterator for BindingIter<'_> {}

impl std::iter::FusedIterator for BindingIter<'_> {}

/// The binding maps of one [`Scope`]: one for each namespace that binds a
/// name, in namespace order. Most scopes bind no name, so this holds no map
/// and allocates nothing.
#[derive(Debug, Clone, Default)]
pub struct ScopeBindings(Vec<(NamespaceId, BindingMap)>);

impl ScopeBindings {
    /// The map of `ns`, or `None` when the scope binds no name in it.
    #[inline]
    pub fn get(&self, ns: NamespaceId) -> Option<&BindingMap> {
        self.0.iter().find(|(n, _)| *n == ns).map(|(_, map)| map)
    }

    /// The map of `ns`, made empty when the scope has none yet.
    pub fn get_or_insert(&mut self, ns: NamespaceId) -> &mut BindingMap {
        let at = match self.0.binary_search_by_key(&ns.index(), |(n, _)| n.index()) {
            Ok(at) => at,
            Err(at) => {
                // One namespace is the common case: no spare slots.
                self.0.reserve_exact(1);
                self.0.insert(at, (ns, BindingMap::default()));
                at
            }
        };
        &mut self.0[at].1
    }

    /// Every map with its namespace, in namespace order.
    pub fn iter(&self) -> impl Iterator<Item = (NamespaceId, &BindingMap)> {
        self.0.iter().map(|(ns, map)| (*ns, map))
    }
}

/// One scope in the tree.
#[derive(Debug, Clone)]
pub struct Scope {
    // Read-only: `ScopeTree::children` is keyed by these three.
    kind: ScopeKind,
    parent: Option<ScopeId>,
    /// The AST node that introduced this scope. For `ScopeKind::File`,
    /// this is [`NodeId::PROGRAM`]; for everything else it's the node id
    /// of the statement that opened the scope.
    owner_node: NodeId,
    /// The bindings of the namespaces this scope binds a name in.
    pub bindings: ScopeBindings,
    /// The text the scope covers: the span of its owner statement, which holds
    /// the body and the `END` of a block. The file scope covers all text, and
    /// a block that the end of the text closed runs to `u32::MAX`.
    pub span: VirtualSpan,
}

// Lock the size of a scope deliberately: a file has one for each block and
// routine. Gated to 64-bit; bump the bound deliberately.
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Scope>() <= 48);

impl Scope {
    fn new(
        kind: ScopeKind,
        parent: Option<ScopeId>,
        owner_node: NodeId,
        span: VirtualSpan,
    ) -> Self {
        Scope {
            kind,
            parent,
            owner_node,
            bindings: ScopeBindings::default(),
            span,
        }
    }

    #[inline]
    pub fn kind(&self) -> ScopeKind {
        self.kind
    }

    /// The enclosing scope, or `None` for the file scope.
    #[inline]
    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    /// The AST node that opened this scope.
    #[inline]
    pub fn owner_node(&self) -> NodeId {
        self.owner_node
    }

    /// Look up `name` in the requested namespace of this scope only — does
    /// not walk parents.
    #[inline]
    pub fn get_in(&self, ns: NamespaceId, name: FoldedNameId) -> Option<SymbolId> {
        self.bindings.get(ns).and_then(|map| map.get(name))
    }

    /// Bind `name` to `sym` in namespace `ns` of this scope. Returns the prior
    /// binding of `name`, if any.
    #[inline]
    pub fn bind(&mut self, ns: NamespaceId, name: FoldedNameId, sym: SymbolId) -> Option<SymbolId> {
        self.bindings.get_or_insert(ns).insert(name, sym)
    }
}

/// The storage of a [`ScopeTree`]. Every read of a record goes through it and
/// its vector is private, so the test-only count sees a scan wherever it sits.
mod records {
    use super::{Scope, ScopeId};

    /// How many scope records [`super::ScopeTree`] read in this process,
    /// counted only with the test-only `test-counts` feature in a build with
    /// debug assertions. A bench build (`cargo build --benches` turns the
    /// feature on for the whole workspace) has none, so the count never runs
    /// in measured code.
    #[cfg(all(feature = "test-counts", debug_assertions))]
    static READ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

    #[inline]
    fn count(_n: usize) {
        #[cfg(all(feature = "test-counts", debug_assertions))]
        READ.fetch_add(_n, std::sync::atomic::Ordering::Relaxed);
    }

    pub(super) fn read() -> Option<usize> {
        #[cfg(all(feature = "test-counts", debug_assertions))]
        return Some(READ.load(std::sync::atomic::Ordering::Relaxed));
        #[cfg(not(all(feature = "test-counts", debug_assertions)))]
        None
    }

    #[derive(Debug, Clone)]
    pub(super) struct Records(Vec<Scope>);

    impl Records {
        pub(super) fn new(root: Scope) -> Self {
            Records(vec![root])
        }

        pub(super) fn push(&mut self, scope: Scope) -> ScopeId {
            let id = ScopeId(self.0.len() as u32);
            self.0.push(scope);
            id
        }

        pub(super) fn len(&self) -> usize {
            self.0.len()
        }

        #[inline]
        pub(super) fn get(&self, id: ScopeId) -> &Scope {
            count(1);
            &self.0[id.raw() as usize]
        }

        #[inline]
        pub(super) fn get_mut(&mut self, id: ScopeId) -> &mut Scope {
            count(1);
            &mut self.0[id.raw() as usize]
        }

        #[inline]
        pub(super) fn iter(&self) -> impl DoubleEndedIterator<Item = (ScopeId, &Scope)> {
            self.0.iter().enumerate().map(|(i, s)| {
                count(1);
                (ScopeId(i as u32), s)
            })
        }
    }
}

use records::Records;

/// Arena of scopes with a fixed root. Scopes are never removed; the tree
/// grows monotonically during the declare pass.
#[derive(Debug, Clone)]
pub struct ScopeTree {
    scopes: Records,
    /// The first scope pushed for each parent, owner node and kind, so a pass
    /// finds the scope of a statement without a scan.
    children: FxHashMap<(ScopeId, NodeId, ScopeKind), ScopeId>,
    /// The folded names the bindings are keyed by. The declare pass fills it
    /// before it binds a name, and the passes after it only read it.
    names: FoldedNames,
}

impl ScopeTree {
    /// Construct a tree with the implicit [`ScopeId::ROOT`] file scope
    /// already created.
    pub fn new() -> Self {
        ScopeTree {
            scopes: Records::new(Scope::new(
                ScopeKind::File,
                None,
                NodeId::PROGRAM,
                VirtualSpan::new(0, u32::MAX),
            )),
            children: FxHashMap::default(),
            names: FoldedNames::new(),
        }
    }

    /// The folded names that the bindings of this tree are keyed by.
    #[inline]
    pub fn names(&self) -> &FoldedNames {
        &self.names
    }

    /// Install the table that the bindings are keyed by. Call it before the
    /// first binding: an id means something only for the table that issued it.
    pub(crate) fn set_names(&mut self, names: FoldedNames) {
        debug_assert!(
            self.scopes.len() == 1,
            "a name table replaced the keys of live bindings"
        );
        self.names = names;
    }

    /// Push a new scope under `parent` that covers `span`, and return its id.
    pub fn push(
        &mut self,
        kind: ScopeKind,
        parent: ScopeId,
        owner_node: NodeId,
        span: VirtualSpan,
    ) -> ScopeId {
        let id = self
            .scopes
            .push(Scope::new(kind, Some(parent), owner_node, span));
        self.children
            .entry((parent, owner_node, kind))
            .or_insert(id);
        id
    }

    /// The first scope of `kind` pushed under `parent` for `owner_node`.
    pub fn child(&self, parent: ScopeId, owner_node: NodeId, kind: ScopeKind) -> Option<ScopeId> {
        let id = self.children.get(&(parent, owner_node, kind)).copied()?;
        debug_assert!(
            {
                let s = self.scopes.get(id);
                s.parent == Some(parent) && s.owner_node == owner_node && s.kind == kind
            },
            "a scope changed its parent, owner or kind after push"
        );
        Some(id)
    }

    /// How many scope records every tree read in this process, or `None`
    /// unless the test-only `test-counts` feature is on in a build with debug
    /// assertions. A test sees that this count grows with the number of
    /// scopes and not with its square.
    #[doc(hidden)]
    pub fn scopes_read() -> Option<usize> {
        records::read()
    }

    /// The innermost scope whose span holds `offset`. A scope is pushed after
    /// its parent and siblings do not overlap, so the holder with the highest
    /// id is the innermost one.
    pub fn innermost_at(&self, offset: u32) -> ScopeId {
        self.scopes
            .iter()
            .rev()
            .find(|(_, scope)| scope.span.start <= offset && offset < scope.span.end)
            .map_or(ScopeId::ROOT, |(id, _)| id)
    }

    pub fn get(&self, id: ScopeId) -> &Scope {
        self.scopes.get(id)
    }

    /// The scope's bindings and span. Its parent, owner and kind stay as
    /// `push` set them.
    pub fn get_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes.get_mut(id)
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.scopes.iter()
    }

    /// The scope a `DEFINE` statement, `FUNCTION` or `PROCEDURE` at `scope`
    /// actually binds into.
    ///
    /// Walks up from `scope` skipping [var-transparent](ScopeKind::is_var_transparent)
    /// block scopes and returns the first enclosing routine (or class/file)
    /// scope. When `scope` is not itself a block, it is returned unchanged.
    /// The root file scope is var-transparent-free, so this always terminates.
    pub fn var_binding_scope(&self, scope: ScopeId) -> ScopeId {
        self.ancestors(scope)
            .find(|id| !self.scopes.get(*id).kind.is_var_transparent())
            .unwrap_or(ScopeId::ROOT)
    }

    /// Walk from `scope` up through parents, yielding each id in order.
    pub fn ancestors(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(Some(scope), move |cur| self.scopes.get(*cur).parent)
    }

    /// Resolve `name` in namespace `ns`, walking parents. Returns the first
    /// binding encountered (shadowing preserves ABL semantics).
    pub fn resolve(&self, scope: ScopeId, ns: NamespaceId, name: FoldedNameId) -> Option<SymbolId> {
        for id in self.ancestors(scope) {
            if let Some(sym) = self.scopes.get(id).get_in(ns, name) {
                return Some(sym);
            }
        }
        None
    }

    /// The binding of `name` in `scope` alone, for a source spelling. It does
    /// not walk the parents; see [`resolve_name`](Self::resolve_name) for that.
    pub fn resolve_in_scope(
        &self,
        scope: ScopeId,
        ns: NamespaceId,
        name: &str,
    ) -> Option<SymbolId> {
        self.scopes.get(scope).get_in(ns, self.names.lookup(name)?)
    }

    /// [`resolve`](Self::resolve) for a source spelling: reads `name` in ASCII
    /// lower case, as the table folds declared names. For a statement-position
    /// identifier, which carries no `NodeId` and so no `references` entry.
    /// A spelling that no declared name folds to binds nothing.
    pub fn resolve_name(&self, scope: ScopeId, ns: NamespaceId, name: &str) -> Option<SymbolId> {
        self.resolve(scope, ns, self.names.lookup(name)?)
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

    fn atom(s: &str) -> FoldedNameId {
        thread_local! {
            static NAMES: std::cell::RefCell<FoldedNames> =
                const { std::cell::RefCell::new(FoldedNames::new()) };
        }
        NAMES.with(|names| names.borrow_mut().intern(s))
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
        let a = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(10),
            VirtualSpan::new(0, 0),
        );
        let b = tree.push(
            ScopeKind::Block,
            a,
            NodeId::from_u32(11),
            VirtualSpan::new(0, 0),
        );
        assert_eq!(a.raw(), 1);
        assert_eq!(b.raw(), 2);
        assert_eq!(tree.get(b).parent, Some(a));
    }

    #[test]
    fn ancestors_walks_parents() {
        let mut tree = ScopeTree::new();
        let a = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(1),
            VirtualSpan::new(0, 0),
        );
        let b = tree.push(
            ScopeKind::Block,
            a,
            NodeId::from_u32(2),
            VirtualSpan::new(0, 0),
        );
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
        assert_eq!(map.get(atom("n0")), Some(SymbolId::new(0)));
        assert_eq!(map.get(atom("over")), Some(SymbolId::new(99)));
    }

    #[test]
    fn binding_map_duplicate_returns_prior() {
        let mut map = BindingMap::default();
        assert!(map.insert(atom("x"), SymbolId::new(1)).is_none());
        assert_eq!(
            map.insert(atom("x"), SymbolId::new(2)),
            Some(SymbolId::new(1))
        );
        assert_eq!(map.get(atom("x")), Some(SymbolId::new(2)));
    }

    #[test]
    fn resolve_walks_parents_shadow_aware() {
        let mut tree = ScopeTree::new();
        let a = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(1),
            VirtualSpan::new(0, 0),
        );
        let b = tree.push(
            ScopeKind::Block,
            a,
            NodeId::from_u32(2),
            VirtualSpan::new(0, 0),
        );
        tree.get_mut(ScopeId::ROOT)
            .bind(NamespaceId::Values, atom("outer"), SymbolId::new(1));
        tree.get_mut(a)
            .bind(NamespaceId::Values, atom("inner"), SymbolId::new(2));
        tree.get_mut(b)
            .bind(NamespaceId::Values, atom("outer"), SymbolId::new(3));

        assert_eq!(
            tree.resolve(b, NamespaceId::Values, atom("outer")),
            Some(SymbolId::new(3))
        );
        assert_eq!(
            tree.resolve(b, NamespaceId::Values, atom("inner")),
            Some(SymbolId::new(2))
        );
        assert_eq!(
            tree.resolve(ScopeId::ROOT, NamespaceId::Values, atom("inner")),
            None
        );
    }

    #[test]
    fn var_binding_scope_hoists_through_blocks() {
        let mut tree = ScopeTree::new();
        let proc = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(1),
            VirtualSpan::new(0, 0),
        );
        let outer = tree.push(
            ScopeKind::Block,
            proc,
            NodeId::from_u32(2),
            VirtualSpan::new(0, 0),
        );
        let inner = tree.push(
            ScopeKind::Block,
            outer,
            NodeId::from_u32(3),
            VirtualSpan::new(0, 0),
        );
        let catch = tree.push(
            ScopeKind::Catch,
            inner,
            NodeId::from_u32(4),
            VirtualSpan::new(0, 0),
        );
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
        let proc = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(1),
            VirtualSpan::new(0, 0),
        );
        let trig = tree.push(
            ScopeKind::Trigger,
            proc,
            NodeId::from_u32(2),
            VirtualSpan::new(0, 0),
        );
        let blk = tree.push(
            ScopeKind::Block,
            trig,
            NodeId::from_u32(3),
            VirtualSpan::new(0, 0),
        );
        assert_eq!(tree.var_binding_scope(blk), trig);
    }

    #[test]
    fn innermost_at_finds_the_deepest_holder() {
        let mut tree = ScopeTree::new();
        let span = VirtualSpan::new;
        let proc = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(1),
            span(10, 100),
        );
        let outer = tree.push(ScopeKind::Block, proc, NodeId::from_u32(2), span(20, 80));
        let inner = tree.push(ScopeKind::Block, outer, NodeId::from_u32(3), span(30, 40));
        let after = tree.push(ScopeKind::Block, outer, NodeId::from_u32(4), span(50, 60));
        let open = tree.push(
            ScopeKind::Procedure,
            ScopeId::ROOT,
            NodeId::from_u32(5),
            span(120, u32::MAX),
        );
        assert_eq!(tree.innermost_at(5), ScopeId::ROOT);
        assert_eq!(tree.innermost_at(10), proc);
        assert_eq!(tree.innermost_at(25), outer);
        assert_eq!(tree.innermost_at(35), inner);
        assert_eq!(tree.innermost_at(40), outer, "the end is outside the span");
        assert_eq!(tree.innermost_at(55), after);
        assert_eq!(tree.innermost_at(90), proc);
        assert_eq!(tree.innermost_at(110), ScopeId::ROOT);
        assert_eq!(tree.innermost_at(5000), open);
    }

    // Keep a smoke-test that the Schema dep compiles through the crate tree.
    #[test]
    fn schema_dep_reachable() {
        let _ = Schema::empty();
    }
}
