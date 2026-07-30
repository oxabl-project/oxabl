//! The workspace index seam: what the semantic layer may ask about a name
//! that does not live in the file being analyzed.
//!
//! The index is defined here, *below* every implementation, for the same
//! reason [`Schema`](oxabl_schema::Schema) is defined below its loader: the
//! answers are spelled in semantic vocabulary ([`SymbolKind`],
//! [`ResolvedType`]), so a trait defined beneath this crate could not name its
//! own return types, and a trait defined above it could not be consumed by the
//! resolve pass without a dependency cycle. Fact extraction, the batch cache,
//! and the language server's salsa-backed cache all sit above and implement
//! [`WorkspaceIndex`].
//!
//! Three properties are load-bearing and every implementation owes them:
//!
//! 1. **Every query is total.** There is no error case — an answer is
//!    [`IndexAnswer::Found`], [`IndexAnswer::NotFound`], or
//!    [`IndexAnswer::Unknowable`]. A file that could not be read or parsed is
//!    *not found* (a broken file is knowably unusable); a name whose target
//!    can only be decided at run time is *unknowable*. That is why a caching
//!    implementation never has to model failure.
//!
//!    Totality is about *answers*, not about unwinding: it is **not** licence
//!    to catch panics. In this workspace salsa's `Cancelled` travels as a panic
//!    payload, so an implementation that wrapped its lookups in a guard would
//!    convert a cancelled recompute into `NotFound` and freeze a buffer on
//!    stale results. Cancellation must propagate. This is the same reason
//!    `LintPipeline::expand`/`collect` are deliberately unguarded — see
//!    `oxabl_pipeline/src/lint.rs`.
//! 2. **Each query is keyed by one name.** A class's answer does not depend on
//!    any other class's file, so an incremental implementation gets early
//!    cutoff per key: editing one file's method body cannot invalidate an
//!    unrelated class's member list.
//! 3. **Keys are cheap.** [`IndexName`] wraps two interned atoms — the
//!    case-folded name, which is its whole identity for `Eq`/`Hash`, and the
//!    spelling the source used, which is what a file path must be derived from.
//!    Both are hashable and `Clone`-by-refcount-bump, and construction touches
//!    neither the AST the name came from nor the heap: it is a stack-buffer
//!    fold plus two interning hash lookups. An index lookup lands on the
//!    language server's per-keystroke path, so a key that allocated per call
//!    would be paid for on every character typed.
//!
//! Absence is a separate question from a miss. [`AnalysisContext::index`]
//! always holds a handle — [`NullIndex`] when no index was supplied — so
//! resolution code calls the index unconditionally and consults
//! [`AnalysisContext::index_loaded`] to decide what a miss *means*: with no
//! index attached a cross-file name stays
//! [`UnresolvedReason::External`](crate::UnresolvedReason::External) ("we did
//! not look"), and only a loaded index turns a miss into
//! [`NotFoundInWorkspace`](crate::UnresolvedReason::NotFoundInWorkspace).
//!
//! [`AnalysisContext::index`]: crate::AnalysisContext::index
//! [`AnalysisContext::index_loaded`]: crate::AnalysisContext::index_loaded

use std::sync::Arc;

use oxabl_ast::AccessModifier;
use oxabl_lexer::oxabl_atom::OxablAtom;

use crate::resolve::fold_atom;
use crate::{ResolvedType, SymbolKind};

// ---------------------------------------------------------------------------
// Keys
// ---------------------------------------------------------------------------

/// A name handed to the index: a qualified class name (`myapp.services.cache`),
/// a `RUN` target as written (`sub/thing.p`), a `SHARED` definition's name, or
/// a class member's name.
///
/// Case-folded on construction, because ABL is case-insensitive and every
/// other name in the symbol table is folded the same way. Dots, hyphens, and
/// path separators are preserved verbatim — the index, not this type, decides
/// how a qualified name maps onto a file.
///
/// **Both spellings are kept.** The folded one is the identity: `PartialEq`,
/// `Eq`, and `Hash` consider *only* it, so this stays a case-insensitive key
/// and two spellings of one ABL name are the same map entry. Equality
/// deliberately ignores [`as_written`](Self::as_written) — do not reach for
/// this type to tell two spellings apart.
///
/// The raw spelling is kept because a folded name cannot derive a file path on
/// a case-sensitive filesystem: `MyApp.Cache` lives in `MyApp/Cache.cls`, and
/// the folded `myapp/cache.cls` does not exist on Linux. Path derivation is
/// therefore [`as_written`](Self::as_written)'s job, with the folded spelling
/// only a fallback — see `oxabl_index::search`.
#[derive(Debug, Clone)]
pub struct IndexName {
    folded: OxablAtom,
    /// The spelling the source used. Interned rather than a `String` so `Clone`
    /// stays a refcount bump and this type keeps costing nothing to pass
    /// around on the language server's per-keystroke path.
    written: OxablAtom,
}

// Hand-written rather than derived: a derive would make the raw spelling part
// of the identity, and `MyApp.Cache` must remain the same key as `myapp.cache`
// everywhere this type is used as one. Written together so the `Hash`/`Eq`
// agreement is visible in one place.
impl PartialEq for IndexName {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.folded == other.folded
    }
}

impl Eq for IndexName {}

impl std::hash::Hash for IndexName {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.folded.hash(state);
    }
}

impl IndexName {
    /// Fold `raw`, and intern both spellings. Cheap enough for a per-reference
    /// call: the fold is a stack-buffer byte pass and interning a repeated name
    /// is a hash lookup, so carrying the raw spelling costs one extra lookup
    /// and no allocation.
    pub fn new(raw: &str) -> Self {
        IndexName {
            folded: fold_atom(raw),
            written: OxablAtom::from(raw),
        }
    }

    /// The **folded** name as a string — the identity, and what to install
    /// wherever a symbol-table name is expected.
    ///
    /// Must **not** be used to derive a file path: it has lost the source
    /// casing, so the path it yields misses on a case-sensitive filesystem.
    /// Use [`as_written`](Self::as_written) for that.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.folded
    }

    /// The folded name as an atom, ready to install as
    /// [`Symbol::name`](crate::Symbol::name) on a synthesized symbol.
    #[inline]
    pub fn as_atom(&self) -> &OxablAtom {
        &self.folded
    }

    /// The name **as the source spelled it**, casing intact. This is what a
    /// file path must be derived from; [`as_str`](Self::as_str) is the folded
    /// identity and is not interchangeable with it.
    #[inline]
    pub fn as_written(&self) -> &str {
        &self.written
    }

    /// The as-written spelling as an atom, for use as a *case-sensitive* map
    /// key. An `IndexName` cannot serve as one — its `Hash`/`Eq` ignore casing
    /// by design.
    #[inline]
    pub fn as_written_atom(&self) -> &OxablAtom {
        &self.written
    }
}

/// Identity of a file the index answered from.
///
/// Deliberately **not** [`FileId`](oxabl_common::FileId): that id space is
/// owned by the pipeline and anchors rendered diagnostics, and anchoring a
/// diagnostic at an indexed file would point it at a file the client never
/// loaded. This is an opaque handle in the *index's* id space — the
/// implementation that minted it is the one that can map it back to a path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IndexedFileId(u32);

impl IndexedFileId {
    /// Mint an id. Only an index implementation should call this; the values
    /// mean nothing outside the index that assigned them.
    #[inline]
    pub const fn new(value: u32) -> Self {
        IndexedFileId(value)
    }

    /// Stable integer for serialization and display. Do not fabricate.
    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// Generation counter for the whole index, mirroring
/// [`SchemaRevision`](oxabl_schema::SchemaRevision).
///
/// A [`Semantic`](crate::Semantic) is stamped with the revision it was
/// computed under, so a consumer holding results across an index rebuild can
/// tell they are stale. [`ABSENT`](Self::ABSENT) is reserved for
/// [`NullIndex`], which is what makes "analyzed with no index" distinguishable
/// from "analyzed against an index" in the output itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IndexRevision(u32);

impl IndexRevision {
    /// The revision of [`NullIndex`] — no index was present.
    pub const ABSENT: IndexRevision = IndexRevision(0);

    /// Mint a revision. Only an index implementation should call this, and only
    /// with a monotonically increasing value.
    ///
    /// # Panics
    ///
    /// If `value` is `0`. That value is [`ABSENT`](Self::ABSENT), and this
    /// constructor is `pub` because out-of-crate implementors need it, so the
    /// reservation has to be enforced rather than merely documented: a real
    /// index that claimed `ABSENT` would read as *no index*, and
    /// [`AnalysisContext::index_loaded`](crate::AnalysisContext::index_loaded)
    /// is derived from exactly this value. Being a `const fn`, a literal `0` in
    /// a `const` initializer fails to compile rather than at run time.
    #[inline]
    pub const fn new(value: u32) -> Self {
        assert!(
            value != 0,
            "IndexRevision::new(0) is reserved for IndexRevision::ABSENT"
        );
        IndexRevision(value)
    }

    /// Stable integer for serialization and display. Do not fabricate.
    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

// ---------------------------------------------------------------------------
// Answers
// ---------------------------------------------------------------------------

/// The three states every index query can be in.
///
/// Total by construction: an implementation that cannot read a file, cannot
/// parse it, or cannot decide between two candidate files still returns one of
/// these. The split between the two negative states is the whole point —
/// `NotFound` is a fact about the workspace that a rule may report, while
/// `Unknowable` is a statically undecidable name that no amount of indexing
/// would resolve and that no rule may report.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IndexAnswer<T> {
    /// The index located the thing and this is it.
    Found(T),
    /// The index looked on the configured paths and it is genuinely absent —
    /// or was located but is unusable (unreadable, unparseable).
    NotFound,
    /// The answer cannot be determined statically: a run-time-computed target,
    /// or two candidate files where exactly one match is required and picking
    /// the wrong link would be worse than declining.
    Unknowable,
}

/// Whether an indexed type was declared as a class or an interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassKind {
    Class,
    Interface,
}

/// What the index knows about one class or interface, *before* any chain
/// walking.
///
/// `inherits` and `implements` are the names **as the declaring file wrote
/// them**, unresolved. Resolving them is the consumer's job on purpose: the
/// consumer is the one that must record an unresolved parent against the
/// child's symbol, and keeping the walk out of the index is what preserves
/// per-class early cutoff — an answer that had already followed the chain
/// would have to be invalidated whenever any ancestor changed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDescriptor {
    /// The class as indexed — the folded qualified name.
    pub name: IndexName,
    /// The file that declares it.
    pub file: IndexedFileId,
    pub kind: ClassKind,
    /// The `INHERITS` name as written, if any. `None` distinguishes "declares
    /// no parent" from a parent that failed to resolve.
    pub inherits: Option<IndexName>,
    /// The `IMPLEMENTS` names as written, in declaration order.
    pub implements: Vec<IndexName>,
}

/// A [`ResolvedType`] proven safe to carry across a file boundary.
///
/// A bare [`ResolvedType`] is not: its `Class`, `Buffer`, and `Table` variants
/// carry a [`SymbolId`](crate::SymbolId) or a
/// [`TableId`](oxabl_schema::TableId) valid only in the tables of the file that
/// minted it, so handing one across the seam would produce a dangling
/// reference in the consumer's symbol table.
///
/// The field is private — as with [`IndexName`] — so [`PortableType::new`] is
/// the *only* way an out-of-crate index implementation can produce one. A
/// public payload would let an implementor write the file-scoped type straight
/// into the variant and smuggle the dangling id across the seam the check
/// exists to guard.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PortableType(ResolvedType);

impl PortableType {
    /// Check `ty` for file-scoped ids and wrap it if it has none.
    ///
    /// Returns `None` for `Class`, `Buffer`, and `Table` (and arrays over
    /// them) — use [`MemberType::Named`] for those. Extraction code should
    /// treat a `None` here as "spell this one as a name", never as a reason to
    /// drop the member.
    pub fn new(ty: ResolvedType) -> Option<Self> {
        fn is_portable(ty: &ResolvedType) -> bool {
            match ty {
                ResolvedType::Primitive(_) | ResolvedType::Unknown | ResolvedType::Error => true,
                ResolvedType::Array { element, .. } => is_portable(element),
                ResolvedType::Class(_) | ResolvedType::Buffer(_) | ResolvedType::Table(..) => false,
            }
        }
        is_portable(&ty).then_some(PortableType(ty))
    }

    /// The wrapped type, ready to install on a synthesized symbol.
    #[inline]
    pub fn as_resolved(&self) -> &ResolvedType {
        &self.0
    }
}

/// Declared type of an indexed member, in a form that survives crossing a file
/// boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberType {
    /// A type whose meaning does not depend on any one file's side tables — a
    /// primitive, an array over one, `Unknown`, or `Error`. Install directly
    /// on a synthesized symbol.
    Portable(PortableType),
    /// A class- or interface-typed declaration, carried as the name written in
    /// the declaring file. The consumer resolves it in *its own* symbol space,
    /// through [`WorkspaceIndex::class`] if need be.
    Named(IndexName),
    /// The declaration carries no type: a `VOID` method, or a form that names
    /// none.
    Untyped,
}

impl MemberType {
    /// Wrap a [`ResolvedType`] that is safe to carry across files, deferring to
    /// [`PortableType::new`] for the check — the one construction path.
    pub fn portable(ty: ResolvedType) -> Option<Self> {
        PortableType::new(ty).map(MemberType::Portable)
    }
}

/// One member the index reports for a class: enough to synthesize a symbol for
/// it in the consuming file without reading the declaring file again.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberDescriptor {
    /// Folded member name.
    pub name: IndexName,
    /// The symbol kind to synthesize. Methods are
    /// [`SymbolKind::Function`] — the same kind the declare pass gives a
    /// method declared locally — and properties are [`SymbolKind::Property`].
    pub kind: SymbolKind,
    /// Declared return type for a method, value type for a property.
    pub ty: MemberType,
    /// Declared accessibility, as the AST spells it. An undecorated member is
    /// `Public`, matching ABL's default.
    pub access: AccessModifier,
    /// `STATIC` members are reached through the type name rather than an
    /// instance, so the consumer needs to tell them apart from instance
    /// members.
    pub is_static: bool,
}

impl MemberDescriptor {
    /// Whether a subclass in another file inherits this member.
    ///
    /// `PackagePrivate` answers `false`: ABL scopes it to the declaring
    /// package, and the index does not model packages, so the conservative
    /// answer avoids synthesizing a member a later access-check rule would
    /// have to report as a violation on an already-resolved reference.
    #[inline]
    pub fn inherited_by_subclass(&self) -> bool {
        matches!(
            self.access,
            AccessModifier::Public | AccessModifier::Protected
        )
    }
}

// ---------------------------------------------------------------------------
// The trait
// ---------------------------------------------------------------------------

/// The four questions the semantic layer may ask about another file.
///
/// Object-safe and `Send + Sync`, mirroring `oxabl_workspace::FileSystem`:
/// the language server holds an
/// `Arc<dyn WorkspaceIndex>` (so cloning its config on every debounced
/// recompute stays a pointer bump) and borrows it into
/// [`AnalysisContext::index`](crate::AnalysisContext::index), while every
/// other client hands over a borrow of a value it owns for the run.
///
/// There are exactly four queries and no client carve-outs. A client that
/// cannot answer one of them answers [`IndexAnswer::NotFound`] for it — it
/// does not get a narrower trait.
///
/// Every query is total in the sense of property 1 above: three answers, no
/// error variant. That is not a mandate to swallow unwinding — an
/// implementation must never turn a salsa `Cancelled` (which travels as a panic
/// payload) into `NotFound`; cancellation propagates, as it does through the
/// unguarded `LintPipeline::expand`/`collect` in `oxabl_pipeline/src/lint.rs`.
pub trait WorkspaceIndex: Send + Sync {
    /// Look up a class or interface by qualified name.
    ///
    /// The returned descriptor is `Arc`-wrapped so a memoizing implementation
    /// hands back a refcount bump rather than a deep clone on every reference
    /// to the same type.
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>>;

    /// List the members `class` **declares itself**, in declaration order.
    ///
    /// Inherited members are deliberately not included: the consumer walks the
    /// chain with [`ClassDescriptor::inherits`] and calls this once per level.
    /// Folding ancestors in here would make every answer depend on every
    /// ancestor's file and destroy the per-class early cutoff this query set
    /// exists to provide.
    ///
    /// `NotFound` for a name that is not a class at all — the same answer
    /// [`Self::class`] gives, so a consumer need not special-case the pair.
    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>>;

    /// Look up the program a literal `RUN` target names, returning the file
    /// that supplies it.
    ///
    /// `Unknowable` when more than one file on the search paths matches:
    /// linking to the wrong program is worse than declining to link.
    fn program(&self, target: &IndexName) -> IndexAnswer<IndexedFileId>;

    /// Look up the file whose `DEFINE NEW [GLOBAL] SHARED` definition produces
    /// `name`, for a consumer's `DEFINE SHARED` to link against.
    fn shared_producer(&self, name: &IndexName) -> IndexAnswer<IndexedFileId>;

    /// The generation this index is on. Stamped onto every
    /// [`Semantic`](crate::Semantic) so stale results are detectable.
    /// Implementations must not return [`IndexRevision::ABSENT`] — that value
    /// means [`NullIndex`].
    fn revision(&self) -> IndexRevision;
}

/// The index that knows nothing.
///
/// Answers `NotFound` to every query so resolution code can call the index
/// without a branch; the branch that matters is
/// [`AnalysisContext::index_loaded`](crate::AnalysisContext::index_loaded),
/// which is `false` whenever this is the handle and keeps every cross-file
/// miss on today's `External` path.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct NullIndex;

impl WorkspaceIndex for NullIndex {
    fn class(&self, _name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        IndexAnswer::NotFound
    }

    fn class_members(&self, _class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        IndexAnswer::NotFound
    }

    fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn revision(&self) -> IndexRevision {
        IndexRevision::ABSENT
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PrimitiveTy;

    #[test]
    fn index_name_folds_case_and_keeps_qualification() {
        let a = IndexName::new("MyApp.Services.Cache");
        let b = IndexName::new("myapp.services.CACHE");
        assert_eq!(a, b);
        assert_eq!(a.as_str(), "myapp.services.cache");
        assert_eq!(a.as_atom(), b.as_atom());
    }

    #[test]
    fn index_name_equality_ignores_case_while_the_raw_spelling_survives() {
        use std::hash::{BuildHasher, RandomState};

        let written = IndexName::new("MyApp.Cache");
        let folded = IndexName::new("myapp.cache");

        // Identity is the folded spelling: same key, same hash. Every map keyed
        // by `IndexName` depends on this.
        assert_eq!(written, folded);
        let hasher = RandomState::new();
        assert_eq!(hasher.hash_one(&written), hasher.hash_one(&folded));
        assert_eq!(written.as_str(), folded.as_str());

        // The raw spelling is still there for path derivation, and it is *not*
        // part of the identity — the two lines above are the reason a
        // case-sensitive cache key has to be `as_written_atom`, not the name.
        assert_eq!(written.as_written(), "MyApp.Cache");
        assert_eq!(folded.as_written(), "myapp.cache");
        assert_ne!(written.as_written(), folded.as_written());
        assert_ne!(written.as_written_atom(), folded.as_written_atom());
    }

    #[test]
    fn index_name_folds_names_longer_than_the_stack_buffer() {
        let long = "A".repeat(200);
        assert_eq!(IndexName::new(&long).as_str(), "a".repeat(200));
    }

    #[test]
    fn null_index_answers_not_found_for_every_query() {
        let idx = NullIndex;
        let name = IndexName::new("myapp.foo");
        assert_eq!(idx.class(&name), IndexAnswer::NotFound);
        assert_eq!(idx.class_members(&name), IndexAnswer::NotFound);
        assert_eq!(
            idx.program(&IndexName::new("thing.p")),
            IndexAnswer::NotFound
        );
        assert_eq!(idx.shared_producer(&name), IndexAnswer::NotFound);
    }

    #[test]
    fn null_index_revision_is_absent() {
        assert_eq!(NullIndex.revision(), IndexRevision::ABSENT);
        assert_eq!(IndexRevision::ABSENT.raw(), 0);
    }

    #[test]
    fn null_index_is_usable_through_a_shared_handle() {
        // KTD8: the language server holds `Arc<dyn WorkspaceIndex>` and
        // borrows it into the `&dyn` slot. Assert that coercion compiles.
        let shared: Arc<dyn WorkspaceIndex> = Arc::new(NullIndex);
        let borrowed: &dyn WorkspaceIndex = &*shared;
        assert_eq!(borrowed.revision(), IndexRevision::ABSENT);
    }

    #[test]
    #[should_panic(expected = "reserved for IndexRevision::ABSENT")]
    fn minting_the_absent_revision_is_rejected() {
        // `new` is `pub` for out-of-crate implementors, so the reservation of 0
        // has to be enforced rather than documented. `black_box` keeps the
        // argument out of a const context, where this is a compile error
        // instead of a run-time panic.
        let _ = IndexRevision::new(std::hint::black_box(0));
    }

    #[test]
    fn portable_accepts_file_independent_types() {
        assert_eq!(
            MemberType::portable(ResolvedType::Primitive(PrimitiveTy::Integer)),
            Some(MemberType::Portable(
                PortableType::new(ResolvedType::Primitive(PrimitiveTy::Integer)).unwrap()
            ))
        );
        assert!(MemberType::portable(ResolvedType::Unknown).is_some());
        let array = ResolvedType::Array {
            element: Box::new(ResolvedType::Primitive(PrimitiveTy::Character)),
            extent: Some(4),
        };
        let wrapped = PortableType::new(array.clone()).expect("array over a primitive is portable");
        // The wrapper is transparent: the consumer gets back exactly the type
        // the index extracted, ready to install on a synthesized symbol.
        assert_eq!(wrapped.as_resolved(), &array);
        assert!(MemberType::portable(array).is_some());
    }

    #[test]
    fn portable_rejects_types_carrying_single_file_ids() {
        use crate::SymbolId;
        // The guard lives on `PortableType::new`, which is the only way to
        // construct the variant's payload, so rejecting here rejects everywhere.
        assert!(PortableType::new(ResolvedType::Class(SymbolId::new(0))).is_none());
        assert!(MemberType::portable(ResolvedType::Class(SymbolId::new(0))).is_none());
        assert!(MemberType::portable(ResolvedType::Buffer(SymbolId::new(0))).is_none());
        assert!(
            MemberType::portable(ResolvedType::Array {
                element: Box::new(ResolvedType::Class(SymbolId::new(0))),
                extent: None,
            })
            .is_none()
        );
    }

    #[test]
    fn private_and_package_private_members_are_not_inherited() {
        let member = |access| MemberDescriptor {
            name: IndexName::new("calculate-total"),
            kind: SymbolKind::Function,
            ty: MemberType::Untyped,
            access,
            is_static: false,
        };
        assert!(member(AccessModifier::Public).inherited_by_subclass());
        assert!(member(AccessModifier::Protected).inherited_by_subclass());
        assert!(!member(AccessModifier::Private).inherited_by_subclass());
        assert!(!member(AccessModifier::PackagePrivate).inherited_by_subclass());
    }
}
