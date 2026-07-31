//! Semantic analysis for Progress ABL source.
//!
//! The semantic layer consumes a [`Program`](oxabl_ast::Statement) AST and an
//! [`AnalysisContext`] and produces a [`Semantic`] value carrying a scope
//! tree, symbol table, resolution map, type map, and diagnostics. v1 runs
//! three passes — declare, resolve, type-check — implemented as separate
//! entry points that share walker infra (`analyze_file` composes them).
//!
//! Side tables are stored as `IndexVec<NodeId, Option<T>>` keyed by the
//! parser's monotonic [`NodeId`](oxabl_ast::NodeId). Side-table-over-mutation
//! preserved the upgrade path to cross-file analysis and Salsa-style
//! incrementality without an IR rewrite — and that bet paid: cross-file
//! resolution shipped as the [`WorkspaceIndex`] seam, consulted *during*
//! the resolve pass, with no per-file table reshaped. A cross-file hit is an
//! ordinary [`Resolution::Resolved`] against an index-synthesized symbol, so a
//! consumer reading `references` needs no second lookup path.

mod builtins;
mod check;
mod coercion;
mod diagnostics;
mod index;
mod index_vec;
mod namespace;
mod operators;
mod resolve;
mod scope;
mod symbol;
mod types;

pub use builtins::SYSTEM_HANDLES;
pub use check::check_pass;
pub use coercion::{
    ClassLattice, assignable, assignable_strict, is_narrowing_warning, widen_primitive,
};
pub use diagnostics::{SEM0001, SEM0002, SEM0003};
pub use index::{
    ClassDescriptor, ClassKind, IndexAnswer, IndexName, IndexRevision, IndexedFileId,
    MemberDescriptor, MemberType, NullIndex, PortableType, WorkspaceIndex,
};
pub use index_vec::NodeIndexVec;
pub use namespace::{NUM_NAMESPACES, NamespaceId};
pub use operators::{binary_op_result, unary_negate_result, unary_not_result};
pub use resolve::{Resolution, UnresolvedReason, declare_pass, resolve_pass};
pub use scope::{BindingMap, Scope, ScopeId, ScopeKind, ScopeTree};
pub use symbol::{
    ClassLookup, SupertypeRef, Supertypes, Symbol, SymbolFlags, SymbolId, SymbolKind, SymbolTable,
};
pub use types::{PrimitiveTy, ResolvedType};

use oxabl_common::{Diagnostic, FileId, LintSeverityMap, VirtualSpan};
use oxabl_schema::{Schema, SchemaRevision};

/// Input to [`analyze_file`] and the per-pass entry points.
///
/// The context never takes ownership of any data — the caller holds the AST,
/// the preprocessor output, the schema, and the workspace index.
/// `schema_loaded` is an explicit flag so schema-dependent diagnostics can
/// suppress silently when a `.df` was not loaded; it is independent of
/// `Schema::is_empty()` because an intentionally empty `.df` should still read
/// as "loaded". `index_loaded` plays the same role for the workspace index.
pub struct AnalysisContext<'a> {
    pub file_id: FileId,
    pub source: &'a str,
    pub schema: &'a Schema,
    pub schema_loaded: bool,
    /// Answers to the four cross-file questions. Always a live handle —
    /// [`NullIndex`] when the caller supplied none — so resolution code can
    /// query unconditionally.
    pub index: &'a dyn WorkspaceIndex,
    /// Whether [`index`](Self::index) is a real index rather than
    /// [`NullIndex`]. This is what decides the *meaning* of a miss: with no
    /// index attached a cross-file name stays
    /// [`UnresolvedReason::External`] ("we did not look"), and only a loaded
    /// index turns a miss into [`UnresolvedReason::NotFoundInWorkspace`].
    pub index_loaded: bool,
    /// Resolved per-rule lint severity overrides. Empty (the default) means
    /// every lint rule keeps its built-in severity. `oxabl_lint::lint_file`
    /// consults this to skip *off* rules and remap emitted severities (KTD6);
    /// the semantic passes themselves ignore it.
    pub lint_severities: LintSeverityMap,
}

impl<'a> AnalysisContext<'a> {
    /// Construct a context with a default-empty schema and no workspace index.
    /// Used by tests and by the analyze subcommand when `--schema` is absent.
    ///
    /// The signature deliberately stays three-argument: a cross-file index is
    /// an addition, and every caller that has nothing to say about one keeps
    /// today's single-file behavior by saying nothing.
    pub fn new(file_id: FileId, source: &'a str, schema: &'a Schema) -> Self {
        AnalysisContext {
            file_id,
            source,
            schema,
            schema_loaded: !schema.is_empty(),
            index: &NullIndex,
            index_loaded: false,
            lint_severities: LintSeverityMap::new(),
        }
    }

    /// Attach resolved lint severity overrides to this context (builder-style).
    pub fn with_lint_severities(mut self, severities: LintSeverityMap) -> Self {
        self.lint_severities = severities;
        self
    }

    /// Attach a workspace index to this context (builder-style). Accepts
    /// anything that borrows as `&dyn WorkspaceIndex`, so a language server
    /// holding an `Arc<dyn WorkspaceIndex>` passes `&*arc`.
    ///
    /// [`index_loaded`](Self::index_loaded) is **derived from the handle**, not
    /// asserted: only [`NullIndex`] may report [`IndexRevision::ABSENT`], so a
    /// handle that knows nothing cannot be talked into claiming it was
    /// consulted. Setting the flag unconditionally would let
    /// `with_index(&NullIndex)` turn a miss into
    /// [`UnresolvedReason::NotFoundInWorkspace`] — a fact about the workspace —
    /// when nothing was looked at.
    pub fn with_index(mut self, index: &'a dyn WorkspaceIndex) -> Self {
        self.index = index;
        self.index_loaded = index.revision() != IndexRevision::ABSENT;
        self
    }
}

/// Output of `analyze_file`. Phases 3 and 4a populate `scope_tree`,
/// `symbols`, `references`, `types` (declaration entries), and
/// `diagnostics`; Phase 4b extends `types` with expression-body entries.
pub struct Semantic {
    pub scope_tree: ScopeTree,
    pub symbols: SymbolTable,
    pub references: NodeIndexVec<Resolution>,
    pub types: NodeIndexVec<ResolvedType>,
    pub schema_revision: SchemaRevision,
    /// Generation of the workspace index this result was computed under.
    /// [`IndexRevision::ABSENT`] when no index was attached, so "analyzed
    /// single-file" is distinguishable from "analyzed against an index" in the
    /// output itself.
    pub index_revision: IndexRevision,
    pub diagnostics: Vec<Diagnostic>,
}

/// Run every semantic pass over `program` and return a [`Semantic`]. v1
/// runs all three passes: declare (Phase 3), resolve (Phase 4a), and
/// type-check (Phase 4b). The resulting `Semantic` carries fully populated
/// scope tree, symbol table, reference map, and type map.
pub fn analyze_file(program: &[oxabl_ast::Statement], ctx: &AnalysisContext) -> Semantic {
    let (scope_tree, mut symbols, mut diagnostics, declare_revision) = declare_pass(program, ctx);
    let (references, mut types, resolve_diags) =
        resolve_pass(program, ctx, &scope_tree, &mut symbols, declare_revision);
    diagnostics.extend(resolve_diags);
    let check_diags = check_pass(program, ctx, &scope_tree, &symbols, &references, &mut types);
    diagnostics.extend(check_diags);
    Semantic {
        scope_tree,
        symbols,
        references,
        types,
        schema_revision: ctx.schema.revision(),
        index_revision: ctx.index.revision(),
        diagnostics,
    }
}

/// Stamp a [`VirtualSpan`] with this context's [`FileId`] to produce a
/// [`FileSpan`](oxabl_common::FileSpan). The offsets pass through unchanged:
/// this crate analyzes one already-expanded buffer and has no expansion table
/// to consult.
///
/// **Preprocessor translation does not happen here.** Turning an expanded-text
/// offset back into a real `(file, offset)` pair is `oxabl_analyze`'s job —
/// `ExpandedFile::resolve_span` in `crates/oxabl_analyze/src/collect.rs` owns
/// the flattened virtual-to-real table and rewrites every diagnostic's span
/// (and each of its labels) after the semantic passes have run. So the
/// `file_id` a semantic diagnostic carries is the *root buffer's*, and it is
/// only correct because `oxabl_analyze` re-anchors it downstream. Do not add
/// an expansion lookup to this function: the semantic crate does not depend on
/// `oxabl_preprocessor`, and duplicating the table here would give the
/// workspace two of them.
pub fn resolve_span(ctx: &AnalysisContext, vs: VirtualSpan) -> oxabl_common::FileSpan {
    oxabl_common::FileSpan {
        file: ctx.file_id,
        span: oxabl_ast::Span {
            start: vs.start,
            end: vs.end,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    /// Minimal index that answers `Found` for one class and carries a
    /// non-absent revision, so a test can tell it apart from [`NullIndex`].
    struct StubIndex;

    const STUB_REVISION: IndexRevision = IndexRevision::new(7);

    impl WorkspaceIndex for StubIndex {
        fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
            if name == &IndexName::new("app.thing") {
                IndexAnswer::Found(Arc::new(ClassDescriptor {
                    name: name.clone(),
                    file: IndexedFileId::new(3),
                    kind: ClassKind::Class,
                    inherits: None,
                    implements: Vec::new(),
                }))
            } else {
                IndexAnswer::NotFound
            }
        }

        fn class_members(&self, _class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
            IndexAnswer::NotFound
        }

        fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
            IndexAnswer::Unknowable
        }

        fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
            IndexAnswer::NotFound
        }

        fn revision(&self) -> IndexRevision {
            STUB_REVISION
        }
    }

    #[test]
    fn new_context_has_no_index_loaded() {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        assert!(!ctx.index_loaded);
        assert_eq!(ctx.index.revision(), IndexRevision::ABSENT);
    }

    #[test]
    fn with_index_marks_loaded_and_installs_the_handle() {
        let schema = Schema::empty();
        let stub = StubIndex;
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema).with_index(&stub);
        assert!(ctx.index_loaded);
        // The handle the passes see is the one supplied, not the null index.
        assert_eq!(ctx.index.revision(), STUB_REVISION);
        assert!(matches!(
            ctx.index.class(&IndexName::new("App.Thing")),
            IndexAnswer::Found(_)
        ));
        assert_eq!(
            ctx.index.program(&IndexName::new("thing.p")),
            IndexAnswer::Unknowable
        );
    }

    #[test]
    fn with_index_of_the_null_index_leaves_the_flag_clear() {
        // The flag is derived from the handle, so routing `NullIndex` through
        // the builder cannot claim an index was consulted — a miss has to stay
        // `External` ("we did not look") rather than becoming a claim about
        // what the workspace contains.
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema).with_index(&NullIndex);
        assert!(!ctx.index_loaded);
        assert_eq!(ctx.index.revision(), IndexRevision::ABSENT);
    }

    #[test]
    fn with_index_accepts_a_shared_handle() {
        // KTD8: the language server clones an `Arc<dyn WorkspaceIndex>` into
        // its config and borrows it into the `&dyn` slot.
        let schema = Schema::empty();
        let shared: Arc<dyn WorkspaceIndex> = Arc::new(StubIndex);
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema).with_index(&*shared);
        assert!(ctx.index_loaded);
        assert_eq!(ctx.index.revision(), STUB_REVISION);
    }

    #[test]
    fn semantic_records_whether_an_index_was_attached() {
        let schema = Schema::empty();
        let source = "DEFINE VARIABLE i AS INTEGER NO-UNDO.";
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();

        let without = analyze_file(
            &program.statements,
            &AnalysisContext::new(FileId::UNKNOWN, source, &schema),
        );
        let stub = StubIndex;
        let with = analyze_file(
            &program.statements,
            &AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&stub),
        );

        assert_eq!(without.index_revision, IndexRevision::ABSENT);
        assert_eq!(with.index_revision, STUB_REVISION);
        assert_ne!(without.index_revision, with.index_revision);
        // Attaching an index changes nothing else in this unit.
        assert_eq!(without.symbols.len(), with.symbols.len());
        assert_eq!(without.diagnostics.len(), with.diagnostics.len());
    }
}
