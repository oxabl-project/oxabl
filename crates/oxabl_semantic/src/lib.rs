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
//! preserves the upgrade path to cross-file analysis and Salsa-style
//! incrementality without an IR rewrite.

mod builtins;
mod diagnostics;
mod index_vec;
mod namespace;
mod resolve;
mod scope;
mod symbol;
mod types;

pub use diagnostics::{SEM0001, SEM0002, SEM0003};
pub use index_vec::NodeIndexVec;
pub use namespace::{NUM_NAMESPACES, NamespaceId};
pub use resolve::declare_pass;
pub use scope::{BindingMap, Scope, ScopeId, ScopeKind, ScopeTree};
pub use symbol::{Symbol, SymbolFlags, SymbolId, SymbolKind, SymbolTable};
pub use types::{PrimitiveTy, ResolvedType};

use oxabl_common::{Diagnostic, FileId, VirtualSpan};
use oxabl_schema::{Schema, SchemaRevision};

/// Input to [`analyze_file`] and the per-pass entry points.
///
/// The context never takes ownership of any data — the caller holds the AST,
/// the preprocessor output, and the schema. `schema_loaded` is an explicit
/// flag so schema-dependent diagnostics can suppress silently when a `.df`
/// was not loaded; it is independent of `Schema::is_empty()` because an
/// intentionally empty `.df` should still read as "loaded".
pub struct AnalysisContext<'a> {
    pub file_id: FileId,
    pub source: &'a str,
    pub schema: &'a Schema,
    pub schema_loaded: bool,
}

impl<'a> AnalysisContext<'a> {
    /// Construct a context with a default-empty schema. Used by tests and by
    /// the analyze subcommand when `--schema` is absent.
    pub fn new(file_id: FileId, source: &'a str, schema: &'a Schema) -> Self {
        AnalysisContext {
            file_id,
            source,
            schema,
            schema_loaded: !schema.is_empty(),
        }
    }
}

/// Output of `analyze_file`. In v1 only `scope_tree`, `symbols`, and
/// `diagnostics` are populated (declare pass). `references` and `types` are
/// reserved side tables for Phases 4a/4b.
pub struct Semantic {
    pub scope_tree: ScopeTree,
    pub symbols: SymbolTable,
    pub references: NodeIndexVec<resolve::Resolution>,
    pub types: NodeIndexVec<ResolvedType>,
    pub schema_revision: SchemaRevision,
    pub diagnostics: Vec<Diagnostic>,
}

/// Run every semantic pass over `program` and return a [`Semantic`]. v1 ships
/// only the declare pass; resolve and check populate their side tables but
/// leave references/types empty for now.
pub fn analyze_file(program: &[oxabl_ast::Statement], ctx: &AnalysisContext) -> Semantic {
    let (scope_tree, symbols, diagnostics) = declare_pass(program, ctx);
    Semantic {
        scope_tree,
        symbols,
        references: NodeIndexVec::new(),
        types: NodeIndexVec::new(),
        schema_revision: ctx.schema.revision(),
        diagnostics,
    }
}

/// Translate a [`VirtualSpan`] in analyzer output to a concrete
/// [`FileSpan`](oxabl_common::FileSpan) for diagnostic rendering. v1 treats
/// virtual and file offsets as identical (no preprocessor expansion between
/// them); Phase 4a hooks `PreprocessedFile::resolve` into this boundary.
pub fn resolve_span(ctx: &AnalysisContext, vs: VirtualSpan) -> oxabl_common::FileSpan {
    oxabl_common::FileSpan {
        file: ctx.file_id,
        span: oxabl_ast::Span {
            start: vs.start,
            end: vs.end,
        },
    }
}
