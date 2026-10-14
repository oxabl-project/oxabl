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
use smallvec::SmallVec;

use crate::{IndexName, IndexedFileId, NamespaceId, ResolvedType, ScopeId, UnresolvedReason};

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
    /// A database sequence from the loaded schema, synthesized on first
    /// reference like a schema field.
    Sequence,
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
        /// The variable a `CATCH` header names. ABL requires the name even when
        /// the handler only cares about the error's type, so never reading it
        /// is not a defect.
        const CATCH_VARIABLE = 1 << 22;
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

/// What the index answered when the resolve pass asked about one class name.
///
/// Recorded per name actually asked about — every class this file names (a
/// supertype, a `NEW` or `CAST` operand, a declared type, a static receiver) and
/// every ancestor the chain walk reaches through them — so the *outcome* of a
/// cross-file class lookup survives the pass that made it. A `USING` import
/// alone records nothing: it names no use.
/// Nothing else in the model carries it: a supertype that resolved mints no
/// symbol (the members it contributes do), so without this record a parent no
/// file declares is indistinguishable from one that resolved and contributed
/// nothing.
///
/// A name in a namespace the AVM ships (`Progress.*`, `OpenEdge.*`, ...) that
/// the index does not supply gets no record: no workspace file could supply it,
/// so "absent" would be a claim about the workspace nobody can make. A bare name
/// the implicit `USING Progress.Lang.*` supplies is the same.
///
/// Spellings that differ only in case are one class and share one entry; see
/// [`SymbolTable::record_class_lookup`] for how their answers merge.
///
/// Bookkeeping only. No pass branches on it and no rule may read it — it exists
/// so the analyze envelope can report which files a run consulted and which
/// lookups came back empty, which is the evidence a dependency-extraction
/// consumer needs and the only way the absent-parent case is observable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassLookup {
    /// The index found it, in this file.
    Linked(IndexedFileId),
    /// Searched on the configured paths and genuinely absent.
    Absent,
    /// A file that would declare it was located, and could not be read or
    /// parsed. Separate from [`Self::Absent`] because the envelope's dependency
    /// section is where a user goes to find out *why* a member did not resolve,
    /// and "the file is right there and oxabl could not read it" is a different
    /// answer from "no file declares it" — the first is a gap in oxabl, the
    /// second a fact about the workspace.
    Unusable,
    /// The index declined to choose between candidates.
    Unknowable,
}

/// What the index answered for the spellings of one folded class name, merged.
///
/// The index probes a case-sensitive path per spelling, so `pkg.Base` and
/// `pkg.base` can get different answers, and the record is keyed by the folded
/// name. Merging keeps the result independent of statement order: every file a
/// spelling linked to stays, and a miss never displaces a link. Among misses the
/// one that says most wins: [`ClassLookup::Unusable`], then
/// [`ClassLookup::Unknowable`], then [`ClassLookup::Absent`].
///
/// A miss next to a link is dropped on purpose. The class resolved, so the
/// dependency is the link; the other spelling is a case mismatch the file system
/// is strict about, not a second unresolved class.
#[derive(Debug, Clone, Default)]
struct ClassAnswers {
    links: SmallVec<[IndexedFileId; 1]>,
    miss: Option<ClassLookup>,
}

impl ClassAnswers {
    fn record(&mut self, lookup: ClassLookup) {
        match lookup {
            ClassLookup::Linked(file) => {
                self.miss = None;
                if !self.links.contains(&file) {
                    self.links.push(file);
                }
            }
            // A miss next to a link is dropped; among misses the most
            // informative stays.
            miss => {
                let outranks = self
                    .miss
                    .is_none_or(|kept| miss_rank(miss) > miss_rank(kept));
                if self.links.is_empty() && outranks {
                    self.miss = Some(miss);
                }
            }
        }
    }

    fn iter(&self) -> impl Iterator<Item = ClassLookup> + '_ {
        self.links
            .iter()
            .map(|file| ClassLookup::Linked(*file))
            .chain(self.miss)
    }

    /// One verdict for the name: the link with the lowest file id, so the pick
    /// does not depend on the order the spellings were met in.
    fn first(&self) -> Option<ClassLookup> {
        self.links
            .iter()
            .min_by_key(|file| file.raw())
            .map(|file| ClassLookup::Linked(*file))
            .or(self.miss)
    }
}

fn miss_rank(lookup: ClassLookup) -> u8 {
    match lookup {
        ClassLookup::Unusable => 2,
        ClassLookup::Unknowable => 1,
        ClassLookup::Absent | ClassLookup::Linked(_) => 0,
    }
}

/// A literal or computed `RUN` target the index did not link to a usable file.
///
/// A linked target mints a program symbol (see [`SymbolTable::program_file`]);
/// every other outcome mints nothing, so without this record a `RUN` that missed,
/// was ambiguous, or could not be named at all looks exactly like no `RUN`.
///
/// Bookkeeping only, like [`ClassLookup`]: it exists so a dependency consumer can
/// tell silence from a gap.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnlinkedRun {
    /// The folded target, or the empty atom for `RUN VALUE(...)`, which has none.
    pub name: OxablAtom,
    /// Why the target did not link.
    pub reason: UnresolvedReason,
    /// The file the target maps onto when the index located one it could not use.
    /// A program oxabl cannot parse is still a file the `RUN` depends on.
    pub located: Option<IndexedFileId>,
    /// Where the target is written, in expanded coordinates.
    pub span: VirtualSpan,
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

impl Symbol {
    /// A symbol the resolve pass synthesizes for something **this file does not
    /// declare** — a schema table's field or default buffer, a workspace class,
    /// an external program, an inherited member.
    ///
    /// The constant fields are the convention, and having one constructor is
    /// what makes it structural rather than restated at each site:
    ///
    /// - `declaration: NodeId::DUMMY` — there is no declaring node in this file,
    ///   so nothing may look one up.
    /// - `name_span` is the **use site**, not a declaration, so a diagnostic can
    ///   still point somewhere real in the file under analysis.
    /// - `declared_in: ScopeId::ROOT` and, load-bearing, the symbol table
    ///   **only** — a synthesized symbol is never inserted into the scope tree.
    ///   That is what stops ordinary name resolution from stumbling onto it; it
    ///   is reachable only through the `references` entry the caller records.
    /// - `read_count` / `write_count` start at zero and are credited by the
    ///   resolve pass exactly as a declared symbol's are.
    ///
    /// `data_type`, `flags`, and `table_id` default to empty/absent; a caller
    /// that carries one fills it in with struct-update syntax.
    pub fn synthesized(
        name: OxablAtom,
        namespace: NamespaceId,
        kind: SymbolKind,
        name_span: VirtualSpan,
    ) -> Self {
        Symbol {
            name,
            namespace,
            kind,
            declared_in: ScopeId::ROOT,
            declaration: NodeId::DUMMY,
            name_span,
            data_type: None,
            read_count: 0,
            write_count: 0,
            flags: SymbolFlags::empty(),
            table_id: None,
        }
    }
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
    /// The four **class-and-cross-file** side maps, behind one `Option<Box<…>>`
    /// so a table that needs none of them costs one word rather than four empty
    /// hash maps.
    ///
    /// Same lesson as the maps themselves, one level up. Inline, they grew
    /// `SymbolTable` from 88 to 216 bytes; a `SymbolTable` is built once per
    /// analysis, and on a small file per-run setup *is* the measurement (the
    /// sibling `ResolveWalker::cross_file` note records a ~10% `tiny/resolve`
    /// regression from exactly this shape). The overwhelmingly common file
    /// declares no class and is analyzed with no index attached, so it should
    /// carry none of this.
    ///
    /// Created on the first write. Every reader treats `None` as "no entry",
    /// which is the same answer an empty map gives, so no consumer can tell the
    /// difference.
    side: Option<Box<SymbolSideMaps>>,
    /// How temp-tables, and buffers for them, reach their fields. Boxed and
    /// created on first write, like [`Self::side`], so a file with no
    /// temp-table pays one word.
    records: Option<Box<RecordLinks>>,
}

/// The record a buffer reads from: a schema table, or a temp-table this file
/// declares (identified by its `TempTable` symbol).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum RecordSource {
    Schema(oxabl_schema::TableId),
    TempTable(SymbolId),
}

/// Declare-pass facts about temp-table records. `Symbol::table_id` covers a
/// schema-backed buffer; these cover everything a temp-table needs.
#[derive(Debug, Default)]
struct RecordLinks {
    /// A `Buffer` symbol → the `TempTable` symbol it is a buffer for.
    buffer_for: FxHashMap<SymbolId, SymbolId>,
    /// A `TempTable` symbol → the scope holding its `FIELD` symbols.
    fields: FxHashMap<SymbolId, ScopeId>,
    /// A `TempTable` symbol → the record its `LIKE` clause copies.
    like: FxHashMap<SymbolId, RecordSource>,
}

/// The `SymbolTable` side maps only a class-bearing file or an index-backed run
/// populates. Reached exclusively through `SymbolTable`'s accessors, which is
/// what lets the box be created lazily without any caller knowing.
#[derive(Debug, Default)]
struct SymbolSideMaps {
    /// Canonical parameter signatures for locally declared methods.
    method_signatures: FxHashMap<SymbolId, Vec<String>>,
    /// All declarations sharing an overloaded method name, keyed by the
    /// symbol retained in the scope binding map.
    method_overloads: FxHashMap<SymbolId, Vec<SymbolId>>,
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
    /// Supertype identities resolved by the resolve pass. The authored names
    /// above remain for diagnostics; assignability consumes these edges.
    resolved_supertypes: FxHashMap<SymbolId, Vec<SymbolId>>,
    /// The indexed file that supplies each synthesized external-program symbol —
    /// the file a literal `RUN` target resolved to.
    ///
    /// A side map for the reason [`Self::supertypes`] documents: eight more bytes
    /// on `Symbol` cost the declare pass 17–25%, and resolved `RUN` targets are a
    /// handful per file at most. Absent entirely
    /// from a run with no index attached, which is what keeps a single-file
    /// analysis at exactly its current cost.
    program_files: FxHashMap<SymbolId, IndexedFileId>,
    /// For each `DEFINE SHARED` consumer symbol, the indexed file whose
    /// `DEFINE NEW [GLOBAL] SHARED` definition it corresponds to.
    ///
    /// **This is the whole link.** It records *which file*, and nothing else: the
    /// consumer's own declaration remains the type of record, and no diagnostic
    /// compares the two declarations. Retyping from the producer, or reporting a
    /// disagreement between them, is deferred follow-up work — doing either here
    /// would produce new findings from a pass whose contract is that it produces
    /// none.
    ///
    /// Deliberately separate from [`Self::program_files`] even though both map a
    /// symbol to a file. They are different facts: a program symbol *is* defined
    /// in the file it names, while a `SHARED` consumer is defined right here and
    /// merely has a counterpart elsewhere. One map would force a consumer to
    /// re-derive which of the two it was looking at.
    shared_producers: FxHashMap<SymbolId, IndexedFileId>,
    /// Every class name the resolve pass asked the index about, and what came
    /// back. Keyed by the **folded** class name, since that is the index's own
    /// identity for a name: two spellings of one class share one entry, and their
    /// answers merge (see [`ClassAnswers`]) rather than the later one winning.
    ///
    /// Keyed by name rather than by [`SymbolId`] because most of these names have
    /// no symbol in this table: a resolved supertype contributes members, not a
    /// symbol of its own, and an absent one contributes nothing at all. That is
    /// exactly why the map is needed — see [`ClassLookup`].
    ///
    /// A side map for the reason [`Self::supertypes`] documents, and absent
    /// entirely from a run with no index attached or one whose classes nobody
    /// inherits from.
    class_lookups: FxHashMap<OxablAtom, ClassAnswers>,
    /// The file a class lookup located but could not use, keyed like
    /// [`Self::class_lookups`]. Separate from [`ClassLookup::Unusable`] so that
    /// enum stays a plain verdict.
    unusable_class_files: FxHashMap<OxablAtom, IndexedFileId>,
    /// Every `RUN` target that did not link, in walk order.
    unlinked_runs: Vec<UnlinkedRun>,
    /// Every place this file names a class whose lookup is in
    /// [`Self::class_lookups`], keyed the same way, in walk order.
    ///
    /// One row per mention rather than one per name. The semantic layer works in
    /// expanded coordinates and cannot tell a mention in the file's own bytes from
    /// one spliced in from an include, so it keeps them all and lets the edge
    /// builder, which holds the expansion's mapping, pick a site of each kind.
    class_sites: Vec<(OxablAtom, VirtualSpan)>,
    /// Where an unmodelled statement (`PUT`, `EXPORT`, ...) names something that
    /// may be a class — `NEW x(...)` or a `name:` member access — and the walk
    /// could neither attribute it to a class nor rule it out.
    unattributed_unmodelled_uses: Vec<VirtualSpan>,
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

    /// The side maps, created on first write. Private: the box is an allocation
    /// detail, and every reader goes through an accessor that treats an absent
    /// box as an absent entry.
    fn side_mut(&mut self) -> &mut SymbolSideMaps {
        self.side.get_or_insert_with(Default::default)
    }

    /// Record the supertypes `sym`'s class or interface header named.
    ///
    /// Only called for a header that named at least one, so a class with no
    /// parent has no entry — which is what makes "declares no parent"
    /// distinguishable from "declares one that resolved to nothing".
    pub fn record_supertypes(&mut self, sym: SymbolId, supertypes: Supertypes) {
        self.side_mut().supertypes.insert(sym, supertypes);
    }

    /// The supertypes `sym`'s header named, or `None` when it named none (or
    /// `sym` is not a class or interface at all).
    pub fn supertypes(&self, sym: SymbolId) -> Option<&Supertypes> {
        self.side.as_deref()?.supertypes.get(&sym)
    }

    pub fn record_resolved_supertypes(&mut self, sym: SymbolId, parents: Vec<SymbolId>) {
        if !parents.is_empty() {
            self.side_mut().resolved_supertypes.insert(sym, parents);
        }
    }

    pub fn resolved_supertypes(&self, sym: SymbolId) -> &[SymbolId] {
        self.side
            .as_deref()
            .and_then(|side| side.resolved_supertypes.get(&sym))
            .map_or(&[], Vec::as_slice)
    }

    pub(crate) fn record_method_signature(&mut self, sym: SymbolId, signature: Vec<String>) {
        self.side_mut().method_signatures.insert(sym, signature);
    }

    pub(crate) fn method_signature(&self, sym: SymbolId) -> Option<&[String]> {
        self.side
            .as_deref()?
            .method_signatures
            .get(&sym)
            .map(Vec::as_slice)
    }

    pub(crate) fn record_method_overload(&mut self, anchor: SymbolId, member: SymbolId) {
        let members = self
            .side_mut()
            .method_overloads
            .entry(anchor)
            .or_insert_with(|| vec![anchor]);
        members.push(member);
    }

    pub fn method_overloads(&self, anchor: SymbolId) -> Option<&[SymbolId]> {
        self.side
            .as_deref()?
            .method_overloads
            .get(&anchor)
            .map(Vec::as_slice)
    }

    pub fn is_overloaded_method(&self, anchor: SymbolId) -> bool {
        self.method_overloads(anchor)
            .is_some_and(|members| members.len() > 1)
    }

    /// The return type an overload set can be judged by, or `None` when `anchor`
    /// names no overload set at all and the caller should read the symbol's own
    /// type as usual.
    ///
    /// Overload *selection* is not modelled — oxabl matches no argument list
    /// against a signature — so a set whose members return **different** types
    /// yields [`ResolvedType::Unknown`], the lattice bottom, leaving the
    /// expression unjudged rather than judged by an arbitrary candidate. A set
    /// whose members all return the **same** type needs no selection: whichever
    /// candidate the AVM picks, the value has that type, so the ordinary
    /// widening ladder still applies. That keeps the common ABL idiom of
    /// overloading on parameters alone judgeable, which an unconditional
    /// `Unknown` silently gave up.
    pub fn overload_set_data_type(&self, anchor: SymbolId) -> Option<ResolvedType> {
        let members = self.method_overloads(anchor)?;
        if members.len() <= 1 {
            return None;
        }
        let mut folded: Option<&ResolvedType> = None;
        for member in members {
            // A member with no declared type is `VOID` or names no type; it
            // cannot agree with a typed sibling, so the set is unknowable.
            let Some(ty) = self.get(*member).data_type.as_ref() else {
                return Some(ResolvedType::Unknown);
            };
            match folded {
                None => folded = Some(ty),
                Some(seen) if seen == ty => {}
                Some(_) => return Some(ResolvedType::Unknown),
            }
        }
        Some(folded.cloned().unwrap_or(ResolvedType::Unknown))
    }

    /// Record that synthesized program symbol `sym` is supplied by indexed file
    /// `file`. Only the resolve pass calls this, and only for a symbol it
    /// synthesized from a resolved literal `RUN` target.
    pub fn record_program_file(&mut self, sym: SymbolId, file: IndexedFileId) {
        self.side_mut().program_files.insert(sym, file);
    }

    /// The indexed file supplying `sym`, or `None` when `sym` is not a
    /// synthesized external-program symbol — which includes every locally
    /// declared `PROCEDURE`, since a local declaration is supplied by this file.
    pub fn program_file(&self, sym: SymbolId) -> Option<IndexedFileId> {
        self.side.as_deref()?.program_files.get(&sym).copied()
    }

    /// Record that `DEFINE SHARED` consumer `sym` corresponds to the
    /// `DEFINE NEW [GLOBAL] SHARED` definition in `file`.
    pub fn record_shared_producer(&mut self, sym: SymbolId, file: IndexedFileId) {
        self.side_mut().shared_producers.insert(sym, file);
    }

    /// The file producing the `SHARED` name `sym` consumes, or `None` when no
    /// producer was located.
    ///
    /// `None` covers three genuinely different situations on purpose — no index
    /// was attached, no indexed file defines the name, or two do and the index
    /// declined to choose. None of them is a link, and a consumer of this map
    /// wants the link or nothing; the reason a link is absent is the index's
    /// business, not the symbol table's.
    pub fn shared_producer(&self, sym: SymbolId) -> Option<IndexedFileId> {
        self.side.as_deref()?.shared_producers.get(&sym).copied()
    }

    /// Record what the index answered for class name `name`. The resolve pass
    /// calls this from the chain walk and from every site that names a class.
    ///
    /// Answers for one folded name merge instead of overwriting: a link beats a
    /// miss, and several links all stay. The result is the same whatever order the
    /// calls come in.
    pub fn record_class_lookup(&mut self, name: &OxablAtom, lookup: ClassLookup) {
        self.side_mut()
            .class_lookups
            .entry(name.clone())
            .or_default()
            .record(lookup);
    }

    /// What the index answered for folded class name `name`, or `None` when the
    /// pass never asked about it — which is the common case: with no index
    /// attached nothing is asked, and a file that names no class asks nothing
    /// either. A name that linked to several files answers with the one of lowest
    /// id; [`Self::class_lookups`] has all of them.
    pub fn class_lookup(&self, name: &OxablAtom) -> Option<ClassLookup> {
        self.side.as_deref()?.class_lookups.get(name)?.first()
    }

    /// Every class lookup this run made, in **unspecified order** — a hash map's
    /// order, so a consumer that renders these must sort them itself. A name
    /// appears once per distinct answer, which is more than once only when its
    /// spellings linked to different files.
    pub fn class_lookups(&self) -> impl Iterator<Item = (&OxablAtom, ClassLookup)> {
        self.side.as_deref().into_iter().flat_map(|side| {
            side.class_lookups
                .iter()
                .flat_map(|(name, answers)| answers.iter().map(move |lookup| (name, lookup)))
        })
    }

    /// Record the file a class lookup located but could not use. Where two
    /// spellings located different files, the one of lowest id is kept.
    pub fn record_unusable_class_file(&mut self, name: &OxablAtom, file: IndexedFileId) {
        self.side_mut()
            .unusable_class_files
            .entry(name.clone())
            .and_modify(|kept| {
                if file.raw() < kept.raw() {
                    *kept = file;
                }
            })
            .or_insert(file);
    }

    /// The file behind a class lookup that answered [`ClassLookup::Unusable`], or
    /// `None` when the index could not say which file it was.
    pub fn unusable_class_file(&self, name: &OxablAtom) -> Option<IndexedFileId> {
        self.side
            .as_deref()?
            .unusable_class_files
            .get(name)
            .copied()
    }

    /// Record a `RUN` target that did not link.
    pub fn record_unlinked_run(&mut self, run: UnlinkedRun) {
        self.side_mut().unlinked_runs.push(run);
    }

    /// Every `RUN` target that did not link, in walk order.
    pub fn unlinked_runs(&self) -> &[UnlinkedRun] {
        self.side.as_deref().map_or(&[], |side| &side.unlinked_runs)
    }

    /// Record that this file names class `name` at `site`. The name is the key
    /// its lookup was recorded under by [`Self::record_class_lookup`].
    pub fn record_class_site(&mut self, name: &OxablAtom, site: VirtualSpan) {
        self.side_mut().class_sites.push((name.clone(), site));
    }

    /// Every place this file names a class, in walk order, in expanded
    /// coordinates. A name may appear many times; a class reached only through
    /// another class's header (an ancestor found by the chain walk) has no site
    /// here, because this file never writes it.
    pub fn class_sites(&self) -> &[(OxablAtom, VirtualSpan)] {
        self.side
            .as_deref()
            .map_or(&[], |side| side.class_sites.as_slice())
    }

    /// Record a place in an unmodelled statement that may name a class and was
    /// neither attributed to one nor ruled out.
    pub fn record_unattributed_unmodelled_use(&mut self, site: VirtualSpan) {
        self.side_mut().unattributed_unmodelled_uses.push(site);
    }

    /// Whether any unmodelled statement may name a class this table has no record
    /// of. A reference there could have produced a dependency that is absent.
    pub fn has_unattributed_unmodelled_use(&self) -> bool {
        self.side
            .as_deref()
            .is_some_and(|side| !side.unattributed_unmodelled_uses.is_empty())
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

    fn records_mut(&mut self) -> &mut RecordLinks {
        self.records.get_or_insert_with(Default::default)
    }

    /// Record that buffer `buffer` is a buffer for temp-table `temp_table`.
    pub(crate) fn record_buffer_for(&mut self, buffer: SymbolId, temp_table: SymbolId) {
        self.records_mut().buffer_for.insert(buffer, temp_table);
    }

    /// Record the field scope, and the `LIKE` source if any, of `temp_table`.
    pub(crate) fn record_temp_table(
        &mut self,
        temp_table: SymbolId,
        fields: ScopeId,
        like: Option<RecordSource>,
    ) {
        let records = self.records_mut();
        records.fields.insert(temp_table, fields);
        if let Some(like) = like {
            records.like.insert(temp_table, like);
        }
    }

    /// The record buffer-like symbol `sym` reads from: its schema table, the
    /// temp-table itself, or the temp-table a buffer was defined for. `None`
    /// for a buffer whose table is unknown.
    pub(crate) fn record_source(&self, sym: SymbolId) -> Option<RecordSource> {
        let symbol = self.get(sym);
        if let Some(tid) = symbol.table_id {
            return Some(RecordSource::Schema(tid));
        }
        if symbol.kind == SymbolKind::TempTable {
            return Some(RecordSource::TempTable(sym));
        }
        let tt = self.records.as_deref()?.buffer_for.get(&sym)?;
        Some(RecordSource::TempTable(*tt))
    }

    /// The scope holding `temp_table`'s own `FIELD` symbols.
    pub(crate) fn temp_table_fields(&self, temp_table: SymbolId) -> Option<ScopeId> {
        self.records.as_deref()?.fields.get(&temp_table).copied()
    }

    /// The record `temp_table`'s `LIKE` clause copies, when it names one.
    pub(crate) fn temp_table_like(&self, temp_table: SymbolId) -> Option<RecordSource> {
        self.records.as_deref()?.like.get(&temp_table).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn file(n: u32) -> IndexedFileId {
        IndexedFileId::new(n)
    }

    /// Every ordering of the same answers must leave the same record.
    fn merged(answers: &[ClassLookup]) -> Vec<(String, ClassLookup)> {
        fn permutations(items: &[ClassLookup]) -> Vec<Vec<ClassLookup>> {
            if items.len() <= 1 {
                return vec![items.to_vec()];
            }
            let mut out = Vec::new();
            for i in 0..items.len() {
                let mut rest = items.to_vec();
                let head = rest.remove(i);
                for mut tail in permutations(&rest) {
                    tail.insert(0, head);
                    out.push(tail);
                }
            }
            out
        }
        let name = OxablAtom::from("pkg.base");
        let mut results: Vec<Vec<(String, ClassLookup)>> = Vec::new();
        for order in permutations(answers) {
            let mut table = SymbolTable::new();
            for lookup in order {
                table.record_class_lookup(&name, lookup);
            }
            let mut rows: Vec<(String, ClassLookup)> = table
                .class_lookups()
                .map(|(n, l)| (n.as_ref().to_string(), l))
                .collect();
            rows.sort_by_key(|(_, l)| format!("{l:?}"));
            results.push(rows);
        }
        let first = results[0].clone();
        assert!(
            results.iter().all(|r| *r == first),
            "the record depends on the order of the answers: {results:?}"
        );
        first
    }

    #[test]
    fn a_link_beats_every_miss_in_either_order() {
        let rows = merged(&[
            ClassLookup::Absent,
            ClassLookup::Unknowable,
            ClassLookup::Unusable,
            ClassLookup::Linked(file(3)),
        ]);
        assert_eq!(
            rows,
            vec![("pkg.base".to_string(), ClassLookup::Linked(file(3)))]
        );
    }

    #[test]
    fn spellings_that_linked_to_different_files_keep_both() {
        let rows = merged(&[ClassLookup::Linked(file(2)), ClassLookup::Linked(file(1))]);
        assert_eq!(rows.len(), 2);
        let mut table = SymbolTable::new();
        let name = OxablAtom::from("pkg.base");
        table.record_class_lookup(&name, ClassLookup::Linked(file(2)));
        table.record_class_lookup(&name, ClassLookup::Linked(file(1)));
        assert_eq!(
            table.class_lookup(&name),
            Some(ClassLookup::Linked(file(1))),
            "the single verdict is the lowest file id, whatever order came first"
        );
    }

    #[test]
    fn among_misses_the_most_informative_one_stays() {
        let rows = merged(&[ClassLookup::Absent, ClassLookup::Unknowable]);
        assert_eq!(
            rows,
            vec![("pkg.base".to_string(), ClassLookup::Unknowable)]
        );
        let rows = merged(&[
            ClassLookup::Unknowable,
            ClassLookup::Unusable,
            ClassLookup::Absent,
        ]);
        assert_eq!(rows, vec![("pkg.base".to_string(), ClassLookup::Unusable)]);
    }

    #[test]
    fn the_unusable_file_kept_is_the_lowest_id() {
        let name = OxablAtom::from("pkg.base");
        let mut table = SymbolTable::new();
        table.record_unusable_class_file(&name, file(5));
        table.record_unusable_class_file(&name, file(4));
        table.record_unusable_class_file(&name, file(9));
        assert_eq!(table.unusable_class_file(&name), Some(file(4)));
    }
}
