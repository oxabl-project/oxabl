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
mod check;
mod coercion;
mod diagnostics;
mod index_vec;
mod namespace;
mod operators;
mod resolve;
mod scope;
mod symbol;
mod types;

pub use builtins::SYSTEM_HANDLES;
pub use check::check_pass;
pub use coercion::{assignable, assignable_strict, is_narrowing_warning, widen_primitive};
pub use diagnostics::{SEM0001, SEM0002, SEM0003};
pub use index_vec::NodeIndexVec;
pub use namespace::{NUM_NAMESPACES, NamespaceId};
pub use operators::{binary_op_result, unary_negate_result, unary_not_result};
pub use resolve::{Resolution, UnresolvedReason, declare_pass, resolve_pass};
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

/// Output of `analyze_file`. Phases 3 and 4a populate `scope_tree`,
/// `symbols`, `references`, `types` (declaration entries), and
/// `diagnostics`; Phase 4b extends `types` with expression-body entries.
pub struct Semantic {
    pub scope_tree: ScopeTree,
    pub symbols: SymbolTable,
    pub references: NodeIndexVec<Resolution>,
    pub types: NodeIndexVec<ResolvedType>,
    pub schema_revision: SchemaRevision,
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
