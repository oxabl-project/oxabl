//! Symbol table.
//!
//! A [`Symbol`] is a declaration's identity. Symbols live in an arena keyed
//! by [`SymbolId`] and are referenced from the scope tree (by scope) and the
//! reference side table (by lookup site). Display casing is reconstructed
//! from the declaration span at diagnostic time rather than stored on the
//! symbol — saves 24 bytes per entry and eliminates one atom-to-string
//! conversion on the hot path.

use bitflags::bitflags;
use oxabl_ast::NodeId;
use oxabl_common::VirtualSpan;
use oxabl_lexer::oxabl_atom::OxablAtom;
use rustc_hash::FxHashMap;

use crate::{IndexName, NamespaceId, ResolvedType, ScopeId};

/// Dense arena index for a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(u32);

impl SymbolId {
    #[inline]
    pub const fn new(n: u32) -> Self {
        SymbolId(n)
    }

    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

/// Kind of symbol. Mirrors the parser's declaration variants at the
/// granularity the semantic layer cares about.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Parameter,
    Property,
    /// Field of a temp-table defined in this file, or a schema-table field
    /// synthesized by the resolve pass on first reference. Synthesized
    /// schema-field symbols carry `declaration: NodeId::DUMMY`, a
    /// schema-derived `data_type`, and are never inserted into the scope
    /// tree — they are reachable only through `references` entries.
    Field,
    TempTable,
    Buffer,
    Stream,
    Frame,
    Event,
    Procedure,
    Function,
    Class,
    Interface,
    /// System handles and pseudo-variables (e.g. `SESSION`, `THIS-OBJECT`).
    /// Seeded by [`crate::builtins`].
    BuiltIn,
    /// Dataset, data-source — tracked enough for duplicate detection and
    /// dump output, but with no declared type.
    Dataset,
    DataSource,
}

bitflags! {
    /// Bitset of modifier flags attached to a [`Symbol`]. Kept in a single
    /// `u32` — cheap to clone, to compare, and to serialize.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct SymbolFlags: u32 {
        const NO_UNDO          = 1 << 0;
        const STATIC           = 1 << 1;
        const ABSTRACT         = 1 << 2;
        const FINAL            = 1 << 3;
        const OVERRIDE         = 1 << 4;
        const PARAM_INPUT      = 1 << 5;
        const PARAM_OUTPUT     = 1 << 6;
        const PARAM_INPUT_OUT  = 1 << 7;
        const PARAM_RETURN     = 1 << 8;
        const SHARED           = 1 << 9;
        const NEW_SHARED       = 1 << 10;
        const NEW_GLOBAL_SHARED = 1 << 11;
        const PRIVATE          = 1 << 12;
        const PROTECTED        = 1 << 13;
        const PUBLIC           = 1 << 14;
        const PACKAGE_PRIVATE  = 1 << 15;
        /// Incomplete FUNCTION declaration (FORWARD / IN handle / MAP TO).
        /// Cleared when a later full definition in the same scope merges into
        /// this symbol (#69).
        const PROTOTYPE        = 1 << 16;
        /// Resolve-computed usage facts (not declaration modifiers): set on a
        /// block-hoisted variable (one recorded in `SymbolTable::block_defined`)
        /// when it is referenced from *outside* its defining block. Consumed by
        /// the `block-var-used-outside` lint (LINT0005) to distinguish the
        /// "may still hold its default value" hazard (read outside, never
        /// written outside) from a deliberate cross-block assignment.
        const READ_OUTSIDE_BLOCK  = 1 << 17;
        const WRITE_OUTSIDE_BLOCK = 1 << 18;
        /// Resolve-computed usage fact (not a declaration modifier): set when
        /// the symbol is passed as a *write-back* argument (`OUTPUT`,
        /// `INPUT-OUTPUT`, `RETURN`) to a `RUN` — i.e. a callee writes into
        /// this variable. Consumed by `unused-variable` (LINT0002) to skip the
        /// false positive where a call site must supply an out-param it never
        /// reads, and available to the planned "written via OUTPUT but never
        /// read" dead-store advisory (#125). `RETURN` is accepted defensively —
        /// the AST models the direction, but `parse_run_arguments` does not
        /// currently produce it for a `RUN` argument, so only `OUTPUT` and
        /// `INPUT-OUTPUT` are reachable from real source today.
        ///
        /// Intentional stopgap: once CFG def-use records land (#126) this
        /// becomes a def-site attribute and the standalone flag can be folded
        /// in and removed.
        const PASSED_AS_OUTPUT_ARG = 1 << 19;
        /// Declaration shape fact: set on a parameter declared as
        /// `TABLE FOR <tt>` or `DATASET FOR <ds>`. Such a parameter *names* a
        /// temp-table or dataset rather than holding a handle value, so every
        /// reference to the name resolves through `NamespaceId::Buffers` (or to
        /// the `DEFINE DATASET` symbol) and lands on that declaration — never
        /// on this symbol. This symbol's own `read_count` is therefore
        /// meaningless by construction: it is permanently zero no matter how
        /// heavily the table is used.
        ///
        /// Not set for `TABLE-HANDLE` / `DATASET-HANDLE`, whose names really
        /// are handle values that collect their own reads, nor for a plain
        /// `AS HANDLE` parameter. Consumers must not treat "typed HANDLE" as a
        /// proxy for this flag, or they lose genuine unused-handle-parameter
        /// findings. Consumed by `unused-variable` (LINT0002), which redirects
        /// the read-count question to the backing table symbol.
        ///
        /// Intentional stopgap, like [`Self::PASSED_AS_OUTPUT_ARG`]: once CFG
        /// def-use records land (#126) the redirect becomes a def-use query and
        /// this flag can be folded in and removed.
        const PARAM_TABLE_LIKE = 1 << 20;
        /// Resolve-computed usage fact: the symbol's name appeared inside a
        /// statement form the parser recognizes but does not model
        /// (`StatementKind::Skipped` — `PUT`, `UPDATE`, `ENABLE`, `EXPORT`,
        /// embedded SQL, …). Those statements credit no reads and no writes, so
        /// this symbol's `read_count` / `write_count` describe only part of what
        /// the code does.
        ///
        /// This is a *distrust* marker, not an access record: the counts are
        /// left exact on purpose, so consumers can tell "provably unused" from
        /// "not fully judged". Consumed by `unused-variable` (LINT0002),
        /// `assigned-but-never-read` (LINT0006) and `block-var-used-outside`
        /// (LINT0005), all three through the one shared `is_skipped` predicate,
        /// as a reason not to fire.
        ///
        /// Coarse by construction: the mark is per-symbol and file-wide, so a
        /// genuine dead store early in a file becomes unjudgeable because of an
        /// `ENABLE` mention nine hundred lines later. That is the real cost of
        /// the lexical fallback, and it is why the retirement path matters.
        ///
        /// Unlike [`Self::PASSED_AS_OUTPUT_ARG`] and [`Self::PARAM_TABLE_LIKE`],
        /// this bit is **not** waiting on #126's CFG def-use records. It drains
        /// incrementally: head-parsing an unmodelled form makes its dispatch site
        /// stop emitting `Skipped`, and this flag's population shrinks with no
        /// change to the resolve pass or to any rule. That work is tracked by
        /// the head-parsing issue (#136); the flag retires when only `FORM`,
        /// embedded SQL, `EDITING:` bodies and option tails still reach it.
        const TOUCHED_BY_UNMODELLED_STATEMENT = 1 << 21;
    }
}

/// One supertype named in a `CLASS` or `INTERFACE` header, recorded by the
/// declare pass **as written and where written**.
///
/// The span is the load-bearing half: a later diagnostic about a parent no file
/// declares has to point at the name in the header, and by the time the resolve
/// pass discovers the miss the AST node is long behind it. The name is an
/// [`IndexName`] rather than a bare atom because that is what the workspace
/// index is keyed by, and because it keeps the source casing the index needs to
/// derive a file path from — folding here and re-spelling later is not possible.
#[derive(Debug, Clone)]
pub struct SupertypeRef {
    /// The supertype as the header spelled it, folded for identity.
    pub name: IndexName,
    /// Span of the name inside the header.
    pub name_span: VirtualSpan,
}

/// The supertypes a `CLASS` or `INTERFACE` header declared.
///
/// Recorded verbatim and **unresolved**: whether any of these names corresponds
/// to a class the workspace declares is the resolve pass's question, and
/// answering it at declare time would make the declare pass depend on the index
/// (which the indexer itself runs, so it would recurse).
///
/// An `INTERFACE` may extend several interfaces, which does not fit
/// `inherits: Option<_>`; its supertype list is recorded in
/// [`implements`](Self::implements) instead. That mirrors
/// `oxabl_index`'s own projection of an interface header, so the two spellings
/// of "the other supertypes, as a set" agree and a consumer's walk needs no
/// special case.
#[derive(Debug, Clone, Default)]
pub struct Supertypes {
    /// The `INHERITS` name, if any. `None` means the header named no parent —
    /// deliberately distinguishable from a parent that failed to resolve, which
    /// is recorded here and simply never links to anything.
    pub inherits: Option<SupertypeRef>,
    /// The `IMPLEMENTS` names, in declaration order (or, for an interface, the
    /// interfaces it extends).
    pub implements: Vec<SupertypeRef>,
}

/// A declared name. Identity is the `SymbolId` issued by [`SymbolTable`];
/// two declarations of the same name in overlapping scopes are distinct
/// symbols.
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Case-folded atom for namespace lookup.
    pub name: OxablAtom,
    pub namespace: NamespaceId,
    pub kind: SymbolKind,
    /// The scope this symbol was declared in. A symbol reachable in
    /// descendant scopes through normal chain-walking never gets cloned;
    /// `SHARED` re-introductions live in `SymbolTable::rebinding_scopes`.
    pub declared_in: ScopeId,
    /// The AST node that introduced this symbol.
    pub declaration: NodeId,
    /// Source span of the declarer's identifier (for diagnostics that
    /// point at the `name`, not the full statement).
    pub name_span: VirtualSpan,
    /// Resolved declared type. Populated by the declare pass for constructs
    /// that carry a type (variables, parameters, properties, fields,
    /// function return types); left `None` for symbols that don't
    /// (procedures, events without a data type, etc.).
    pub data_type: Option<ResolvedType>,
    /// Incremented by the resolve pass on every resolving read reference.
    pub read_count: u32,
    /// Incremented by the resolve pass on every resolving write reference.
    pub write_count: u32,
    pub flags: SymbolFlags,
    /// Link to the backing schema table for `Buffer` / `TempTable` symbols.
    /// Populated at declare time for `DEFINE BUFFER ... FOR <table>` and
    /// `FOR EACH <table>` (and synthesized default-buffer symbols at resolve
    /// time). Valid only under the `Schema` whose `revision()` equals the
    /// owning `Semantic`'s `schema_revision` — never resolve this id against
    /// a `Schema` from a different revision.
    pub table_id: Option<oxabl_schema::TableId>,
}

/// Arena of symbols plus the SHARED rebinding side map.
#[derive(Debug, Default)]
pub struct SymbolTable {
    arena: Vec<Symbol>,
    /// A single `SHARED` variable may be reintroduced in additional scopes
    /// through `NEW SHARED` / `NEW GLOBAL SHARED`. Rather than duplicating
    /// the symbol, we track the rebinding scopes here — mirrors Ruff's
    /// `rebinding_scopes` side map for Python `global`/`nonlocal`.
    pub rebinding_scopes: FxHashMap<SymbolId, Vec<ScopeId>>,
    /// The original block scope of each `DEFINE VARIABLE` the declare pass
    /// hoisted out of a `DO`/`FOR`/`REPEAT`/`CATCH`/`FINALLY` block to its
    /// routine scope (see variable hoisting). Kept as a side map rather than a
    /// `Symbol` field so the common case (no hoisting) neither grows every
    /// symbol nor allocates. The `block-var-used-outside` analysis (LINT0005)
    /// uses it to classify a reference as inside/outside the defining block.
    block_defined: FxHashMap<SymbolId, ScopeId>,
    /// The supertypes each `Class` / `Interface` symbol's header named, recorded
    /// by the declare pass and consumed by the resolve pass's chain walk.
    ///
    /// A **typed record** rather than a [`SymbolFlags`] bit — there is no yes/no
    /// fact here to flag, the payload is names and spans — but a side map rather
    /// than a `Symbol` field, for the reason `block_defined` above gives and
    /// because that reason turned out to be measurable: even boxed to one
    /// pointer, the field grew `Symbol` from 64 to 72 bytes and cost the declare
    /// pass 17–25% across every benchmark fixture. Classes are a small minority
    /// of symbols and nothing on the hot path reads this, so the map is entirely
    /// absent from the common file.
    supertypes: FxHashMap<SymbolId, Supertypes>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable::default()
    }

    /// Number of symbols in the arena.
    pub fn len(&self) -> usize {
        self.arena.len()
    }

    pub fn is_empty(&self) -> bool {
        self.arena.is_empty()
    }

    /// Insert a symbol; the returned id is stable within this table.
    pub fn insert(&mut self, symbol: Symbol) -> SymbolId {
        let id = SymbolId::new(self.arena.len() as u32);
        self.arena.push(symbol);
        id
    }

    pub fn get(&self, id: SymbolId) -> &Symbol {
        &self.arena[id.raw() as usize]
    }

    pub fn get_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.arena[id.raw() as usize]
    }

    pub fn iter(&self) -> impl Iterator<Item = (SymbolId, &Symbol)> {
        self.arena
            .iter()
            .enumerate()
            .map(|(i, s)| (SymbolId::new(i as u32), s))
    }

    /// Record that `sym` has been reintroduced in `scope` via
    /// `SHARED`/`NEW SHARED`/`NEW GLOBAL SHARED`.
    pub fn record_rebinding(&mut self, sym: SymbolId, scope: ScopeId) {
        self.rebinding_scopes.entry(sym).or_default().push(scope);
    }

    /// Record the supertypes `sym`'s class or interface header named.
    ///
    /// Only called for a header that named at least one, so a class with no
    /// parent has no entry — which is what makes "declares no parent"
    /// distinguishable from "declares one that resolved to nothing".
    pub fn record_supertypes(&mut self, sym: SymbolId, supertypes: Supertypes) {
        self.supertypes.insert(sym, supertypes);
    }

    /// The supertypes `sym`'s header named, or `None` when it named none (or
    /// `sym` is not a class or interface at all).
    pub fn supertypes(&self, sym: SymbolId) -> Option<&Supertypes> {
        self.supertypes.get(&sym)
    }

    /// Record that variable `sym` was hoisted out of block scope `block`.
    pub fn record_block_defined(&mut self, sym: SymbolId, block: ScopeId) {
        self.block_defined.insert(sym, block);
    }

    /// The block scope `sym` was hoisted out of, if it is a block-hoisted
    /// variable; `None` otherwise.
    pub fn block_defined_scope(&self, sym: SymbolId) -> Option<ScopeId> {
        self.block_defined.get(&sym).copied()
    }

    /// Whether any declared variable was hoisted out of a block. When `false`,
    /// the block-var-used-outside analysis (LINT0005) has nothing to track.
    pub fn has_block_scoped_var(&self) -> bool {
        !self.block_defined.is_empty()
    }
}
