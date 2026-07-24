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

use crate::{NamespaceId, ResolvedType, ScopeId};

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
        /// block-hoisted variable (`defined_in_block.is_some()`) when it is
        /// referenced from *outside* its defining block. Consumed by the
        /// `block-var-used-outside` lint (LINT0005) to distinguish the
        /// "may still hold its default value" hazard (read outside, never
        /// written outside) from a deliberate cross-block assignment.
        const READ_OUTSIDE_BLOCK  = 1 << 17;
        const WRITE_OUTSIDE_BLOCK = 1 << 18;
    }
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
    /// For a `DEFINE VARIABLE` that ABL hoisted out of a `DO`/`FOR`/`REPEAT`/
    /// `CATCH`/`FINALLY` block to its routine scope: the original block scope
    /// the definition textually sat in. `None` for variables defined directly
    /// at a routine scope and for every non-variable symbol. The
    /// `block-var-used-outside` lint (LINT0005) uses it to decide whether a
    /// reference is "inside" or "outside" the defining block.
    pub defined_in_block: Option<ScopeId>,
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
}
