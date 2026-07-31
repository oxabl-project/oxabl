//! Declare (and, eventually, resolve) pass.
//!
//! v1 ships the declare half: walk every statement the parser emits, open
//! and close scopes, and insert [`Symbol`] records into the scope they
//! belong to. The resolve half (Phase 4a) walks the same tree again to
//! populate `references`; the two halves share the walker infrastructure
//! which is why they live in one module.
//!
//! Declaration forms handled in v1:
//! - DEFINE VARIABLE / VAR (emitted as [`StatementKind::VariableDeclaration`])
//! - DEFINE PARAMETER
//! - DEFINE TEMP-TABLE (plus its fields)
//! - DEFINE BUFFER
//! - DEFINE STREAM / FRAME / EVENT / PROPERTY / DATASET / DATA-SOURCE
//! - PROCEDURE / FUNCTION
//! - CLASS / INTERFACE, METHOD, CONSTRUCTOR, DESTRUCTOR
//! - CATCH (its error variable)
//! - DO counter variable
//! - FOR EACH implicit buffer
//!
//! Diagnostics emitted:
//! - `SEM0001` — duplicate declaration in the same scope & namespace
//! - `SEM0002` — SHARED / NEW SHARED boundary mismatch (declare pass)
//! - `SEM0003` — BLOB/CLOB used as a local variable type

use std::collections::VecDeque;

use oxabl_ast::{
    AccessModifier, BufferTarget, CreateTarget, DataType, Expression, ExpressionKind,
    HandleParamKind, Identifier, NodeId, OnAction, OnKind, ParameterDirection, ParameterType,
    RunTarget, Span, Statement, StatementKind, StreamOperation, SubscribeTarget, TypeSource,
};
use oxabl_common::{Diagnostic, VirtualSpan};
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_schema::{FieldResolution, SchemaRevision, TableId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    AnalysisContext, ClassKind, ClassLookup, IndexAnswer, IndexName, IndexedFileId,
    MemberDescriptor, MemberType, NamespaceId, NodeIndexVec, ResolvedType, ScopeId, ScopeKind,
    ScopeTree, SupertypeRef, Supertypes, Symbol, SymbolFlags, SymbolId, SymbolKind, SymbolTable,
    builtins, diagnostics, resolve_span,
};

/// Resolution of a single reference site. Populated by Phase 4a; the type
/// is defined here so the declare pass can ship without `references` being
/// `()`-typed.
///
/// `Unresolved` carries the case-folded atom of the identifier so lint
/// diagnostics (`LINT0001`) don't need to reslice the source span at emit
/// time. See plan §C5 for the trade-off.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolution {
    Resolved(crate::SymbolId),
    Unresolved {
        name: OxablAtom,
        reason: UnresolvedReason,
    },
}

/// Reason a reference did not resolve. See plan §Resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnresolvedReason {
    NotInScope,
    /// USING import, `RUN "x"`, `DYNAMIC-FUNCTION`, dynamic buffer op — any
    /// reference outside the single-file unit that no workspace index was
    /// present to answer. Every lint rule skip-lists this reason, so it is
    /// the *suppression* state: "we did not look", not "we looked and
    /// failed".
    External,
    /// Field / table reference that needs a schema we don't have loaded.
    NoSchema,
    /// A cross-file name searched for on the configured paths and genuinely
    /// absent — the index was present and answered "no such file / no such
    /// member". Distinct from [`Self::External`] because the answer is a
    /// fact about the workspace rather than a missing capability. A parent
    /// that *was* located but could not be parsed folds in here too: a
    /// broken file is knowably not usable, and the only distinction any
    /// consumer branches on is knowable-versus-unknowable.
    NotFoundInWorkspace,
    /// A cross-file name that cannot be known statically — a runtime-computed
    /// target, so no amount of indexing would resolve it. Separated from
    /// [`Self::NotFoundInWorkspace`] so a future rule can report the absent
    /// name without ever reporting the unknowable one.
    Unknowable,
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

/// Run the declare pass over `program` and return the populated scope tree,
/// symbol table, any declaration-level diagnostics, and the `SchemaRevision`
/// observed at declare time. The revision is handed to [`resolve_pass`] as a
/// staleness tripwire: `Symbol::table_id` values minted during declare are
/// only valid under the same schema revision.
pub fn declare_pass(
    program: &[Statement],
    ctx: &AnalysisContext,
) -> (ScopeTree, SymbolTable, Vec<Diagnostic>, SchemaRevision) {
    let mut walker = Walker::new(ctx);
    walker.walk_block(program, ScopeId::ROOT);
    let declare_revision = ctx.schema.revision();
    (
        walker.tree,
        walker.symbols,
        walker.diagnostics,
        declare_revision,
    )
}

// ---------------------------------------------------------------------------
// Walker state
// ---------------------------------------------------------------------------

struct Walker<'a> {
    ctx: &'a AnalysisContext<'a>,
    tree: ScopeTree,
    symbols: SymbolTable,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Walker<'a> {
    fn new(ctx: &'a AnalysisContext<'a>) -> Self {
        let mut tree = ScopeTree::new();
        let mut symbols = SymbolTable::new();
        builtins::seed(&mut tree, &mut symbols);
        Walker {
            ctx,
            tree,
            symbols,
            diagnostics: Vec::new(),
        }
    }

    fn walk_block(&mut self, stmts: &[Statement], scope: ScopeId) {
        for stmt in stmts {
            self.walk_statement(stmt, scope);
        }
    }

    fn walk_statement(&mut self, stmt: &Statement, scope: ScopeId) {
        match &stmt.kind {
            // ---- Variables ------------------------------------------------
            StatementKind::VariableDeclaration {
                name,
                type_source,
                extent,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                let flags = shared_flags(*is_shared, *is_new_shared, *is_new_global_shared);
                self.declare_variable(stmt, scope, name, type_source, *extent, flags);
            }

            // ---- Parameters (DEFINE [IN|OUT|IN-OUT] PARAMETER ...) --------
            StatementKind::DefineParameter {
                direction,
                param_type,
            } => self.declare_parameter(stmt, scope, direction.clone(), param_type),

            // ---- Streams / Frames / Events --------------------------------
            StatementKind::DefineStream { name } => {
                self.declare_simple(stmt, scope, name, NamespaceId::Streams, SymbolKind::Stream);
            }
            StatementKind::DefineFrame { name, .. } => {
                self.declare_simple(stmt, scope, name, NamespaceId::Frames, SymbolKind::Frame);
            }
            StatementKind::DefineEvent {
                access,
                is_static,
                is_abstract,
                name,
                parameters,
            } => {
                let flags = access_flag(*access)
                    | flag_if(*is_static, SymbolFlags::STATIC)
                    | flag_if(*is_abstract, SymbolFlags::ABSTRACT);
                let sym = self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Events,
                    SymbolKind::Event,
                    None,
                    flags,
                    None,
                );
                let event_scope = self.tree.push(ScopeKind::Method, scope, stmt.id);
                let _ = sym;
                self.walk_block(parameters, event_scope);
            }

            // ---- Property -------------------------------------------------
            StatementKind::Property {
                access,
                is_static,
                name,
                data_type,
                no_undo,
                get_body,
                set_body,
                set_parameters,
            } => {
                let flags = access_flag(*access)
                    | flag_if(*is_static, SymbolFlags::STATIC)
                    | flag_if(*no_undo, SymbolFlags::NO_UNDO);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::Property,
                    Some(ResolvedType::from_data_type(data_type)),
                    flags,
                    None,
                );
                if let Some(body) = get_body {
                    let getter = self.tree.push(ScopeKind::PropertyGet, scope, stmt.id);
                    self.walk_block(body, getter);
                }
                if let Some(body) = set_body {
                    let setter = self.tree.push(ScopeKind::PropertySet, scope, stmt.id);
                    // SET (INPUT pv AS TYPE) parameters bind in the setter scope
                    // so the body can reference them (#58 item B).
                    self.walk_block(set_parameters, setter);
                    self.walk_block(body, setter);
                } else if !set_parameters.is_empty() {
                    // Defensive: params without a body still get a scope.
                    let setter = self.tree.push(ScopeKind::PropertySet, scope, stmt.id);
                    self.walk_block(set_parameters, setter);
                }
            }

            // ---- Temp-table -----------------------------------------------
            StatementKind::DefineTempTable {
                name,
                fields,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                let flags = shared_flags(*is_shared, *is_new_shared, *is_new_global_shared);
                self.declare_temp_table(stmt, scope, name, fields, flags);
            }

            // ---- Buffer ---------------------------------------------------
            // Inlined rather than via `declare_simple` so the shared-mode flags
            // reach the symbol; `declare_simple` stays flag-less for its
            // Stream/Frame callers.
            StatementKind::DefineBuffer {
                name,
                target,
                is_new_shared,
                is_shared,
                is_new_global_shared,
                ..
            } => {
                // Link the buffer to its backing schema table so the resolve
                // pass can validate and type field accesses. Temp-table
                // targets have no schema table.
                let table_id = match target {
                    BufferTarget::Table(table) => self.schema_table_id(table),
                    BufferTarget::TempTable(_) => None,
                };
                let flags = shared_flags(*is_shared, *is_new_shared, *is_new_global_shared);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Buffers,
                    SymbolKind::Buffer,
                    None,
                    flags,
                    table_id,
                );
            }

            // ---- Dataset / Data-source ------------------------------------
            StatementKind::DefineDataset {
                name,
                is_shared,
                is_new_shared,
                is_new_global_shared,
                ..
            } => {
                let flags = shared_flags(*is_shared, *is_new_shared, *is_new_global_shared);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::Dataset,
                    None,
                    flags,
                    None,
                );
            }
            StatementKind::DefineDataSource { name, .. } => {
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::DataSource,
                    None,
                    SymbolFlags::empty(),
                    None,
                );
            }

            // ---- Procedure / Function -------------------------------------
            StatementKind::Procedure { name, body } => {
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Procedures,
                    SymbolKind::Procedure,
                    None,
                    SymbolFlags::empty(),
                    None,
                );
                let proc_scope = self.tree.push(ScopeKind::Procedure, scope, stmt.id);
                self.walk_block(body, proc_scope);
            }
            StatementKind::Function {
                name,
                return_type,
                body,
            } => {
                // Empty body marks FORWARD / IN … / MAP TO prototypes (#69).
                // Full definitions (incl. signature params from #68) have
                // non-empty bodies and clear PROTOTYPE on merge.
                let flags = if body.is_empty() {
                    SymbolFlags::PROTOTYPE
                } else {
                    SymbolFlags::empty()
                };
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Functions,
                    SymbolKind::Function,
                    Some(ResolvedType::from_data_type(return_type)),
                    flags,
                    None,
                );
                let fn_scope = self.tree.push(ScopeKind::Function, scope, stmt.id);
                self.walk_block(body, fn_scope);
            }

            // ---- Class / Method / Constructor / Destructor / Interface ----
            StatementKind::Class {
                name,
                inherits,
                implements,
                is_abstract,
                is_final,
                body,
            } => {
                let flags = flag_if(*is_abstract, SymbolFlags::ABSTRACT)
                    | flag_if(*is_final, SymbolFlags::FINAL);
                let sym = self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Types,
                    SymbolKind::Class,
                    None,
                    flags,
                    None,
                );
                // The header's supertype names, kept rather than dropped: the
                // resolve pass needs them to walk the chain, and their spans are
                // the only thing a later diagnostic about an unresolvable parent
                // could point at.
                self.record_supertypes(
                    sym,
                    inherits.as_ref().map(supertype_ref),
                    implements.iter().map(supertype_ref).collect(),
                );
                let class_scope = self.tree.push(ScopeKind::Class, scope, stmt.id);
                self.walk_block(body, class_scope);
            }
            StatementKind::Interface {
                name,
                inherits,
                body,
            } => {
                let sym = self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Types,
                    SymbolKind::Interface,
                    None,
                    SymbolFlags::empty(),
                    None,
                );
                // An interface extends a *list* of interfaces, so its supertypes
                // go to `implements` — see [`Supertypes`] for why that loses
                // nothing.
                self.record_supertypes(sym, None, inherits.iter().map(supertype_ref).collect());
                let iface_scope = self.tree.push(ScopeKind::Interface, scope, stmt.id);
                self.walk_block(body, iface_scope);
            }
            StatementKind::Method {
                access,
                is_static,
                is_abstract,
                is_override,
                return_type,
                name,
                parameters,
                body,
            } => {
                let flags = access_flag(*access)
                    | flag_if(*is_static, SymbolFlags::STATIC)
                    | flag_if(*is_abstract, SymbolFlags::ABSTRACT)
                    | flag_if(*is_override, SymbolFlags::OVERRIDE);
                let ret = return_type.as_ref().map(ResolvedType::from_data_type);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Functions,
                    SymbolKind::Function,
                    ret,
                    flags,
                    None,
                );
                let method_scope = self.tree.push(ScopeKind::Method, scope, stmt.id);
                self.walk_block(parameters, method_scope);
                self.walk_block(body, method_scope);
            }
            StatementKind::Constructor {
                access,
                parameters,
                body,
            } => {
                let cs = self.tree.push(ScopeKind::Constructor, scope, stmt.id);
                let _ = access;
                self.walk_block(parameters, cs);
                self.walk_block(body, cs);
            }
            StatementKind::Destructor { body } => {
                let ds = self.tree.push(ScopeKind::Destructor, scope, stmt.id);
                self.walk_block(body, ds);
            }

            // ---- CATCH / FINALLY -----------------------------------------
            StatementKind::Catch {
                error_var,
                error_type: _,
                body,
            } => {
                let cs = self.tree.push(ScopeKind::Catch, scope, stmt.id);
                // v1: record the error variable as a local. Type carries
                // its declared class name as an opaque string; Phase 4a
                // resolves it into the types namespace.
                self.declare(
                    stmt,
                    cs,
                    error_var,
                    NamespaceId::Values,
                    SymbolKind::Variable,
                    Some(ResolvedType::Unknown),
                    SymbolFlags::empty(),
                    None,
                );
                self.walk_block(body, cs);
            }
            StatementKind::Finally { body } => {
                let fs = self.tree.push(ScopeKind::Finally, scope, stmt.id);
                self.walk_block(body, fs);
            }

            // ---- Block-introducing statements ----------------------------
            StatementKind::Do { loop_var, body, .. } => {
                let bs = self.tree.push(ScopeKind::Block, scope, stmt.id);
                if let Some(id) = loop_var {
                    // A DO counter references an existing variable — ABL never
                    // implicitly declares it. Only mint a block-scoped counter
                    // when nothing by that name already resolves in an
                    // enclosing scope; otherwise the reference pass binds the
                    // counter to that outer symbol (bumping its use), so
                    // declaring a shadow here would leave the real variable
                    // looking unused (false LINT0002).
                    let atom = fold_atom(&id.name);
                    if self
                        .tree
                        .resolve(scope, NamespaceId::Values, &atom)
                        .is_none()
                    {
                        self.declare(
                            stmt,
                            bs,
                            id,
                            NamespaceId::Values,
                            SymbolKind::Variable,
                            Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer)),
                            SymbolFlags::empty(),
                            None,
                        );
                    }
                }
                self.walk_block(body, bs);
            }
            StatementKind::Repeat { body, .. } => {
                let bs = self.tree.push(ScopeKind::Block, scope, stmt.id);
                self.walk_block(body, bs);
            }
            StatementKind::ForEach { buffer, body, .. } => {
                let bs = self.tree.push(ScopeKind::Block, scope, stmt.id);
                // FOR EACH introduces an implicit buffer at block scope. Its
                // name is the table name, so link it to the schema table.
                let table_id = self.schema_table_id(buffer);
                self.declare(
                    stmt,
                    bs,
                    buffer,
                    NamespaceId::Buffers,
                    SymbolKind::Buffer,
                    None,
                    SymbolFlags::empty(),
                    table_id,
                );
                self.walk_block(body, bs);
            }
            StatementKind::Block(body) => {
                self.walk_block(body, scope);
            }
            StatementKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_statement(then_branch, scope);
                if let Some(eb) = else_branch {
                    self.walk_statement(eb, scope);
                }
            }
            StatementKind::Case {
                when_branches,
                otherwise,
                ..
            } => {
                for branch in when_branches {
                    self.walk_block(&branch.body, scope);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o, scope);
                }
            }
            StatementKind::Label { body, .. } => {
                self.walk_statement(body, scope);
            }
            StatementKind::On { kind } => {
                use oxabl_ast::{OnAction, OnKind};
                match kind {
                    OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => {
                        if let OnAction::Block(body) = action {
                            let ts = self.tree.push(ScopeKind::Trigger, scope, stmt.id);
                            self.walk_statement(body, ts);
                        }
                    }
                    OnKind::KeyRemap { .. } => {}
                }
            }
            StatementKind::TriggerProcedure { .. } => {
                // Opens an implicit trigger-procedure scope for the file
                // body that follows. Currently just records the scope
                // without lifting the subsequent statements into it —
                // parse-level ordering already nests them.
                let _ = self.tree.push(ScopeKind::TriggerProcedure, scope, stmt.id);
            }

            // ---- PreprocIf: walk both branches so declarations in either
            // branch are visible to the resolve pass. Ties into the plan's
            // PreprocIf invariant.
            StatementKind::PreprocIf(pif) => {
                self.walk_block(&pif.then_branch, scope);
                for (_, branch) in &pif.elseif_branches {
                    self.walk_block(branch, scope);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb, scope);
                }
            }

            // ---- Everything else: no declarations; nothing to walk. ------
            _ => {}
        }
    }

    // ------------------------------------------------------------------
    // Declaration helpers
    // ------------------------------------------------------------------

    fn declare_variable(
        &mut self,
        stmt: &Statement,
        scope: ScopeId,
        name: &Identifier,
        type_source: &TypeSource,
        extent: Option<u32>,
        flags: SymbolFlags,
    ) {
        // BLOB / CLOB rejection per plan §Type system.
        if let TypeSource::Explicit(dt) = type_source
            && matches!(dt, DataType::Blob | DataType::Clob)
        {
            let span = identifier_span(name);
            let label = match dt {
                DataType::Blob => "BLOB",
                DataType::Clob => "CLOB",
                _ => unreachable!(),
            };
            self.diagnostics.push(Diagnostic::error(
                diagnostics::SEM0003,
                format!(
                    "{label} requires a TEMP-TABLE or database field; not permitted on a local variable"
                ),
                resolve_span(self.ctx, span),
            ));
            // Declare anyway so downstream passes don't re-flag it.
        }

        let data_type = match type_source {
            TypeSource::Explicit(dt) => Some(wrap_extent(ResolvedType::from_data_type(dt), extent)),
            TypeSource::Like { .. } => None,
        };
        // ABL scopes a `DEFINE VARIABLE` to its enclosing routine, not to the
        // `DO`/`FOR`/`REPEAT`/`CATCH`/`FINALLY` block it happens to sit in.
        // Bind (and duplicate-check) against that routine scope so a variable
        // defined inside a block is visible throughout the routine — matching
        // the language and avoiding a false `undefined-symbol` when it is used
        // after the block closes.
        let block_scope = scope;
        let scope = self.tree.var_binding_scope(scope);
        let sym = self.declare(
            stmt,
            scope,
            name,
            NamespaceId::Values,
            SymbolKind::Variable,
            data_type,
            flags,
            None,
        );
        // When the definition was hoisted out of a block, remember the block
        // it sat in so the `block-var-used-outside` lint (LINT0005) can flag
        // reads from outside that block — where the variable may still hold
        // its default value because the block never executed.
        if block_scope != scope
            && let Some(id) = sym
        {
            self.symbols.record_block_defined(id, block_scope);
        }
    }

    fn declare_parameter(
        &mut self,
        stmt: &Statement,
        scope: ScopeId,
        direction: ParameterDirection,
        param_type: &ParameterType,
    ) {
        let dir_flag = match direction {
            ParameterDirection::Input => SymbolFlags::PARAM_INPUT,
            ParameterDirection::Output => SymbolFlags::PARAM_OUTPUT,
            ParameterDirection::InputOutput => SymbolFlags::PARAM_INPUT_OUT,
            ParameterDirection::Return => SymbolFlags::PARAM_RETURN,
        };
        match param_type {
            ParameterType::Variable {
                name,
                type_source,
                no_undo,
            } => {
                let data_type = match type_source {
                    TypeSource::Explicit(dt) => Some(ResolvedType::from_data_type(dt)),
                    TypeSource::Like { .. } => None,
                };
                let flags = dir_flag | flag_if(*no_undo, SymbolFlags::NO_UNDO);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::Parameter,
                    data_type,
                    flags,
                    None,
                );
            }
            ParameterType::Buffer { name, target } => {
                // `DEFINE PARAMETER BUFFER b FOR <table>` — link the backing
                // schema table like any other buffer-introducing site.
                let table_id = self.schema_table_id(target);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Buffers,
                    SymbolKind::Buffer,
                    None,
                    dir_flag,
                    table_id,
                );
            }
            ParameterType::Handle { kind, name, .. } => {
                // All four handle-ish forms declare the same way — a `HANDLE`
                // parameter in `Values`. But the two `FOR` forms *name* a
                // temp-table / dataset, so their references resolve to that
                // declaration and never to this symbol; flag them so consumers
                // can tell them from the `*-HANDLE` forms, whose names really
                // are handle values. See `SymbolFlags::PARAM_TABLE_LIKE`.
                let table_like = matches!(kind, HandleParamKind::Table | HandleParamKind::Dataset);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::Parameter,
                    Some(ResolvedType::Primitive(crate::PrimitiveTy::Handle)),
                    dir_flag | flag_if(table_like, SymbolFlags::PARAM_TABLE_LIKE),
                    None,
                );
            }
        }
    }

    fn declare_temp_table(
        &mut self,
        stmt: &Statement,
        scope: ScopeId,
        name: &Identifier,
        fields: &[oxabl_ast::TempTableField],
        flags: SymbolFlags,
    ) {
        let tt_sym = self.declare(
            stmt,
            scope,
            name,
            NamespaceId::Buffers,
            SymbolKind::TempTable,
            None,
            flags,
            // Temp-tables are file-local; no backing schema table.
            None,
        );
        // Fields bind in a scope of their own, nested under the enclosing
        // scope and owned by this temp-table's statement. This keeps each
        // temp-table's field namespace private: identically-named fields in
        // different temp-tables (extremely common — many tables share a
        // `code`/`qty`/`name` field) never collide, while a field declared
        // twice in the *same* temp-table still raises SEM0001 within this
        // scope. Namespace `Values` is a simplification for v1 — Phase 4a
        // will likely route field lookups through the temp-table symbol.
        if tt_sym.is_none() {
            return;
        }
        let field_scope = self.tree.push(ScopeKind::TempTable, scope, stmt.id);
        for field in fields {
            let data_type = match &field.type_source {
                TypeSource::Explicit(dt) => {
                    Some(wrap_extent(ResolvedType::from_data_type(dt), field.extent))
                }
                TypeSource::Like { .. } => None,
            };
            self.declare(
                stmt,
                field_scope,
                &field.name,
                NamespaceId::Values,
                SymbolKind::Field,
                data_type,
                SymbolFlags::empty(),
                None,
            );
        }
    }

    fn declare_simple(
        &mut self,
        stmt: &Statement,
        scope: ScopeId,
        name: &Identifier,
        ns: NamespaceId,
        kind: SymbolKind,
    ) {
        self.declare(
            stmt,
            scope,
            name,
            ns,
            kind,
            None,
            SymbolFlags::empty(),
            None,
        );
    }

    /// Case-insensitive schema lookup of a table-name identifier. Returns
    /// `None` when the table is absent from the schema (or no schema is
    /// loaded — an empty schema yields `None` here by construction).
    fn schema_table_id(&self, table: &Identifier) -> Option<TableId> {
        self.ctx.schema.table_id(&fold_atom(&table.name))
    }

    /// Core insertion routine. Returns the new `SymbolId` on success;
    /// returns `None` if a duplicate was suppressed (and emits `SEM0001`).
    /// `table_id` links `Buffer` / `TempTable` symbols to their backing
    /// schema table; every other kind passes `None`.
    #[allow(clippy::too_many_arguments)]
    fn declare(
        &mut self,
        stmt: &Statement,
        scope: ScopeId,
        name: &Identifier,
        ns: NamespaceId,
        kind: SymbolKind,
        data_type: Option<ResolvedType>,
        flags: SymbolFlags,
        table_id: Option<TableId>,
    ) -> Option<crate::SymbolId> {
        let atom = fold_atom(&name.name);
        let name_span = VirtualSpan::new(name.span.start, name.span.end);

        // Duplicate check — same scope + same namespace only. Shadowing
        // across scope boundaries is legal. We check `Scope::get_in`
        // (this scope only) rather than `resolve` (walks parents).
        if let Some(prior) = self.tree.get(scope).get_in(ns, &atom) {
            // Skip duplicate detection for built-ins seeded at the root —
            // re-declaring `session` or `error-status` is permitted in the
            // plan's v1 cut; Phase 4a may tighten this.
            let prior_kind = self.symbols.get(prior).kind;
            if prior_kind != SymbolKind::BuiltIn {
                // FUNCTION prototype + definition reconciliation (#69).
                // Methods share SymbolKind::Function but never set PROTOTYPE,
                // so method duplicates still fall through to SEM0001.
                if kind == SymbolKind::Function
                    && prior_kind == SymbolKind::Function
                    && matches!(stmt.kind, StatementKind::Function { .. })
                    && let Some(id) =
                        self.try_merge_function_prototype(prior, stmt, name_span, data_type, flags)
                {
                    return Some(id);
                }
                let prior_span = self.symbols.get(prior).name_span;
                self.diagnostics.push(
                    Diagnostic::error(
                        diagnostics::SEM0001,
                        format!("`{}` is already declared in this scope", name.name),
                        resolve_span(self.ctx, name_span),
                    )
                    .with_label(
                        resolve_span(self.ctx, prior_span),
                        "prior declaration here".into(),
                    ),
                );
                return None;
            }
        }

        let symbol = Symbol {
            name: atom.clone(),
            namespace: ns,
            kind,
            declared_in: scope,
            declaration: stmt.id,
            name_span,
            data_type,
            read_count: 0,
            write_count: 0,
            flags,
            table_id,
        };
        let id = self.symbols.insert(symbol);
        self.tree.get_mut(scope).bindings[ns.index()].insert(atom, id);
        Some(id)
    }

    /// Attach a class or interface header's supertypes to its symbol.
    ///
    /// A no-op when the header named none, so the symbol gets no entry — which is
    /// what makes "declares no parent" distinguishable from "declares one that
    /// resolved to nothing" — and a no-op when [`Self::declare`] refused the
    /// declaration (a duplicate), because there is no symbol of *this* header's
    /// to hang them on and writing them onto the prior declaration's symbol
    /// would attribute one class's parent to another.
    fn record_supertypes(
        &mut self,
        sym: Option<SymbolId>,
        inherits: Option<crate::SupertypeRef>,
        implements: Vec<crate::SupertypeRef>,
    ) {
        let Some(sym) = sym else { return };
        if inherits.is_none() && implements.is_empty() {
            return;
        }
        self.symbols.record_supertypes(
            sym,
            crate::Supertypes {
                inherits,
                implements,
            },
        );
    }

    /// Reconcile FUNCTION prototype(s) with a later (or earlier) definition.
    ///
    /// Returns `Some(prior)` when the collision is a legal prototype/prototype
    /// or prototype/definition pairing; `None` when the caller should emit
    /// SEM0001 (true duplicate definitions).
    fn try_merge_function_prototype(
        &mut self,
        prior: crate::SymbolId,
        stmt: &Statement,
        name_span: VirtualSpan,
        data_type: Option<ResolvedType>,
        incoming_flags: SymbolFlags,
    ) -> Option<crate::SymbolId> {
        let prior_is_proto = self
            .symbols
            .get(prior)
            .flags
            .contains(SymbolFlags::PROTOTYPE);
        let incoming_is_proto = incoming_flags.contains(SymbolFlags::PROTOTYPE);

        match (prior_is_proto, incoming_is_proto) {
            // Prototype then definition — complete the symbol.
            (true, false) => {
                let sym = self.symbols.get_mut(prior);
                sym.declaration = stmt.id;
                sym.name_span = name_span;
                if data_type.is_some() {
                    sym.data_type = data_type;
                }
                sym.flags.remove(SymbolFlags::PROTOTYPE);
                Some(prior)
            }
            // Prototype then prototype — merge (idempotent). Repeated
            // prototypes are idiomatic in ADM2/WebSpeed preprocessed output
            // (e.g. FORWARD + IN SUPER before the full definition).
            (true, true) => Some(prior),
            // Definition then prototype — ignore the redundant prototype.
            (false, true) => Some(prior),
            // Two full definitions — true duplicate.
            (false, false) => None,
        }
    }
}

// ===========================================================================
// Resolve pass
// ===========================================================================

/// Run the resolve pass over `program` given a populated scope tree and
/// symbol table from the declare pass. Walks every expression position,
/// consults the scope chain with namespace narrowing, and populates the
/// `references` side table. Also emits the declared-type entries into the
/// `types` side table and upgrades `DataType::Class(_)` symbol types from
/// `Unknown` to `Class(SymbolId)` where the class is declared in this file.
///
/// Schema-backed resolution: when `ctx.schema` carries tables, buffer
/// symbols' `table_id` links drive field lookup (`Schema::get_by_id` →
/// `Table::get_field`), synthesizing one `Field` symbol per distinct
/// referenced field and one default-`Buffer` symbol per bare table name.
///
/// `declare_revision` is the `SchemaRevision` captured by [`declare_pass`];
/// the tripwire below fires if resolve runs against a schema whose revision
/// differs from the one declare saw, because `TableId`s are dense indices
/// valid only within a single revision — `Schema::get_by_id` is a bare `Vec`
/// index, so a stale id would *silently* read the wrong table.
///
/// Idempotent: per-symbol read/write counts are collected into a local
/// accumulator and written back at end-of-pass, so re-running the pass is a
/// no-op. This is the Salsa-ready invariant called out in plan §C7.
pub fn resolve_pass(
    program: &[Statement],
    ctx: &AnalysisContext,
    tree: &ScopeTree,
    symbols: &mut SymbolTable,
    declare_revision: SchemaRevision,
) -> (
    NodeIndexVec<Resolution>,
    NodeIndexVec<ResolvedType>,
    Vec<Diagnostic>,
) {
    debug_assert_eq!(
        declare_revision,
        ctx.schema.revision(),
        "resolve_pass must run against the same schema revision declare_pass saw"
    );
    let mut walker = ResolveWalker::new(ctx, tree, symbols);
    // Only the block-var-used-outside analysis (LINT0005) reads per-reference
    // scope, and only for hoisted variables. Skip that work wholesale when the
    // declare pass hoisted none — the common case — in O(1).
    walker.track_block_vars = walker.symbols.has_block_scoped_var();
    // Imports first: the class-type pre-pass immediately below resolves
    // `AS CLASS Foo` through them, so they have to be known before it runs.
    walker.collect_using_imports(program);
    walker.upgrade_class_types(program);
    walker.walk_block(program, ScopeId::ROOT);
    // After the walk, never during it: the only files the index can answer a
    // `SHARED` producer question from are the ones the walk's `RUN` lookups
    // pulled in. See `link_shared_consumers`.
    walker.link_shared_consumers();

    // Flush symbol-level read/write counts exactly once at pass end.
    for (sym, (reads, writes)) in &walker.counts {
        let s = walker.symbols.get_mut(*sym);
        s.read_count = *reads;
        s.write_count = *writes;
    }

    // Flush the outside-defining-block usage facts for block-hoisted variables.
    for (sym, (read_outside, write_outside)) in &walker.block_var_outside {
        let s = walker.symbols.get_mut(*sym);
        if *read_outside {
            s.flags |= crate::SymbolFlags::READ_OUTSIDE_BLOCK;
        }
        if *write_outside {
            s.flags |= crate::SymbolFlags::WRITE_OUTSIDE_BLOCK;
        }
    }

    // Flush the write-back-argument usage fact (LINT0002's OUTPUT-argument
    // exemption).
    for sym in &walker.output_args {
        walker.symbols.get_mut(*sym).flags |= crate::SymbolFlags::PASSED_AS_OUTPUT_ARG;
    }

    // Flush the unmodelled-statement usage fact (the three count-gated rules'
    // "these counts are not trustworthy" marker).
    for sym in &walker.unmodelled_touch {
        walker.symbols.get_mut(*sym).flags |= crate::SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT;
    }

    // Mirror every symbol's declared type into the `types` side table keyed
    // by the declaration's NodeId. Skips builtins and synthesized
    // schema-field/buffer symbols (declaration = DUMMY).
    for (_, sym) in walker.symbols.iter() {
        if sym.declaration == NodeId::DUMMY {
            continue;
        }
        if let Some(ty) = sym.data_type.as_ref() {
            walker.types.insert(sym.declaration, ty.clone());
        }
    }

    (walker.references, walker.types, walker.diagnostics)
}

/// Access mode for a resolving reference — drives which counter bumps on
/// [`Symbol`] (`read_count` or `write_count`). `ReadWrite` covers
/// `INPUT-OUTPUT` parameter sites, where the callee both observes and
/// mutates the target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AccessMode {
    Read,
    Write,
    ReadWrite,
}

struct ResolveWalker<'a> {
    ctx: &'a AnalysisContext<'a>,
    tree: &'a ScopeTree,
    /// The declare-pass symbol table, mutated in place: synthesized
    /// schema-field / default-buffer symbols are inserted mid-walk and
    /// read/write counts flush here at end-of-pass.
    symbols: &'a mut SymbolTable,
    references: NodeIndexVec<Resolution>,
    types: NodeIndexVec<ResolvedType>,
    diagnostics: Vec<Diagnostic>,
    /// Per-symbol `(reads, writes)` accumulator. Written to `Symbol` exactly
    /// once at end-of-pass so the pass stays idempotent under Salsa re-run.
    counts: FxHashMap<SymbolId, (u32, u32)>,
    /// Per-symbol `(read_outside, write_outside)` accumulator for block-hoisted
    /// variables: whether the variable is read / written from *outside* the
    /// block it was hoisted out of. Flushed to [`SymbolFlags::READ_OUTSIDE_BLOCK`]
    /// / [`SymbolFlags::WRITE_OUTSIDE_BLOCK`] at end-of-pass (same idempotency
    /// contract as `counts`).
    block_var_outside: FxHashMap<SymbolId, (bool, bool)>,
    /// Symbols passed as a write-back (`OUTPUT` / `INPUT-OUTPUT` / `RETURN`)
    /// argument to a `RUN`. Flushed to [`SymbolFlags::PASSED_AS_OUTPUT_ARG`]
    /// at end-of-pass (same idempotency contract as `counts`). Needs no
    /// tracking gate: it only fires on write-back RUN arguments, which are
    /// rare, and costs one O(1) set insert each.
    output_args: FxHashSet<SymbolId>,
    /// Symbols named by a [`StatementKind::Skipped`] node — a statement form
    /// the parser recognized but did not model. Flushed to
    /// [`SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT`] at end-of-pass (same
    /// idempotency contract as `counts`). Deliberately *not* fed into `counts`:
    /// the harvest is lexical, so crediting it as a read or a write would make
    /// the counts lie about what the code does.
    unmodelled_touch: FxHashSet<SymbolId>,
    /// True only when the declare pass hoisted at least one `DEFINE VARIABLE`
    /// out of a block (`SymbolTable::has_block_scoped_var`). The vast majority
    /// of files have none, so callers skip [`Self::note_block_var_use`]
    /// entirely on the resolve hot path.
    track_block_vars: bool,
    /// Dedup cache for synthesized schema-field symbols: one symbol per
    /// distinct `(table, field)` referenced, no matter how many times the
    /// field is accessed.
    synth_fields: FxHashMap<(TableId, OxablAtom), SymbolId>,
    /// Dedup cache for synthesized default-buffer symbols: one symbol per
    /// schema table referenced by bare name.
    synth_buffers: FxHashMap<TableId, SymbolId>,
    /// The class whose body the walk is currently inside, if any. An inherited
    /// member is only reachable from within its subclass, so this is the gate on
    /// consulting [`CrossFileState::inherited`] at all.
    ///
    /// Stays on the walker rather than moving into [`CrossFileState`]: it is one
    /// word, and it is assigned on the ordinary class-walk path rather than from
    /// behind an index gate, so boxing it would buy nothing and cost a branch.
    current_class: Option<SymbolId>,
    /// Everything cross-file resolution needs, and **nothing a single-file run
    /// touches** — behind one `Option<Box<…>>` so a run with no index attached
    /// pays one word for the whole apparatus.
    ///
    /// This is a measured shape, not a stylistic one. Inline, the nine
    /// containers inside grew `ResolveWalker` from 296 to 608 bytes, and
    /// constructing and dropping nine empty hash containers per pass cost the
    /// `tiny/resolve` benchmark ~10% of its instruction count — a fixture small
    /// enough that per-run setup *is* the measurement. It is the same lesson
    /// `SymbolTable`'s side maps record: this crate's hot structs are sensitive
    /// to bytes, and machinery a run never uses must not be sitting in them.
    ///
    /// `None` is indistinguishable from empty at every read site (see
    /// [`Self::cross_file`]), so the resolution answers are identical either way.
    cross_file: Option<Box<CrossFileState>>,
}

/// The resolve pass's cross-file state: memos, synthesized-symbol caches, and the
/// inherited-member surface. Reached only from behind an `index_loaded` gate.
///
/// Created lazily on the first write, so even a run *with* an index attached that
/// never resolves anything across a file boundary allocates nothing.
#[derive(Default)]
struct CrossFileState {
    /// Classes whose supertype chain has already been walked. Holds the *class*,
    /// not the member: the walk is the expensive half (one index lookup per
    /// ancestor), so it is memoized once per class no matter how many members of
    /// it are named, and a chain of depth five under twenty references costs five
    /// lookups rather than a hundred.
    inherited_walked: FxHashSet<SymbolId>,
    /// What each ancestor contributes to a class in this file, keyed by
    /// `(subclass, folded member name)`. Populated by the chain walk; the symbol
    /// inside is minted only when a reference actually names the member.
    inherited: FxHashMap<(SymbolId, OxablAtom), InheritedMember>,
    /// Names an ancestor declares but does **not** pass on to a subclass, keyed
    /// the same way *plus the namespace the member lives in*. Kept because "an
    /// ancestor declares this privately" and "nothing anywhere declares this" are
    /// different facts, and only the second one is an undefined symbol.
    ///
    /// **The namespace is part of the key for the same reason it gates
    /// [`Self::inherited`].** Softening a reference to "an inaccessible member" is
    /// what suppresses its `undefined-symbol` finding, so a namespace-blind key
    /// suppresses across namespaces: a private ancestor *method* named
    /// `calc-secret` would silence the value-namespace reference in
    /// `v-n = calc-secret.`, which names no method at all and is a genuine
    /// undefined symbol. That is a lost true positive, not a conservative
    /// answer — the accessible path has always been namespace-gated, and this is
    /// the same gate on the other branch.
    inherited_inaccessible: FxHashSet<(SymbolId, NamespaceId, OxablAtom)>,
    /// Explicit `USING <package>.<Name>` imports, in the order the file wrote
    /// them: the simple name folded, paired with the qualified name **as
    /// written** (which is what a file path has to be derived from).
    ///
    /// Collected once, before the walk, and only when an index is attached — a
    /// single-file run pays nothing for it.
    using_types: Vec<(OxablAtom, IndexName)>,
    /// Wildcard `USING <package>.*` prefixes, in the order the file wrote them.
    /// Each keeps its trailing dot, so a candidate is `prefix + name` with no
    /// re-splicing at lookup time.
    using_prefixes: Vec<String>,
    /// Memo for the *workspace* half of a type-name lookup, keyed by the
    /// **as-written** spelling — the exact bytes the source used, unfolded.
    ///
    /// Case-sensitive on purpose, and for the same reason [`BatchIndex`]'s own
    /// `classes` / `programs` memos are: the search underneath derives a file path
    /// from the name, path lookup is case-sensitive, and so two spellings of one
    /// name are two genuinely different questions. Folding the key merges them,
    /// and the merge is not symmetric — a *miss* memoized under `myapp.cache`
    /// would be served to `MyApp.Cache`, which would have found the file. Keying
    /// on the written spelling costs one memo entry per distinct spelling (a
    /// file that writes a class name two ways is already unusual) and cannot
    /// poison.
    ///
    /// The scope-tree half is deliberately not memoized here: it depends on the
    /// reference's scope, and caching a scope-dependent answer under a scope-free
    /// key is how a shadowed name starts resolving to the wrong symbol.
    ///
    /// This is what keeps twenty `NEW Foo()` sites at one index lookup.
    type_lookups: FxHashMap<OxablAtom, TypeLookup>,
    /// One synthesized `Types` symbol per indexed class, so `USING MyApp.Foo`
    /// and `NEW MyApp.Foo()` point at the *same* symbol — which is what makes
    /// `ResolvedType::Class` identity comparison work at all.
    synth_classes: FxHashMap<IndexName, SymbolId>,
    /// Reverse of [`Self::synth_classes`]. Two jobs: it is how a member lookup
    /// re-derives the index key from a class symbol (the symbol's own `name` is
    /// the *folded* spelling, which cannot derive a path on a case-sensitive
    /// filesystem), and it is how [`ResolveWalker::walk_member_surface`] tells an
    /// indexed class
    /// symbol apart from a locally declared one.
    indexed_class_of: FxHashMap<SymbolId, IndexName>,
    /// One synthesized `Procedures` symbol per external program a literal `RUN`
    /// resolved to, so two `RUN post-order.p` sites in one file point at the
    /// *same* symbol rather than minting one each.
    synth_programs: FxHashMap<IndexName, SymbolId>,
}

/// The atom carried by the resolution recorded for a run-time-computed `RUN`
/// target.
///
/// [`Resolution::Unresolved`] carries a name so a diagnostic need not reslice the
/// source, and a computed target *has* no name — the identifier inside
/// `VALUE(...)` names the variable holding the target, not the target, and it
/// already owns its own resolution one node down. So the field is empty rather
/// than misleading: the only reason it exists here is that the variant requires
/// it, and [`UnresolvedReason::Unknowable`] is skip-listed by every rule, so no
/// renderer ever reaches it.
const COMPUTED_TARGET: &str = "";

/// `ResolveWalker` is constructed once per resolve pass, and on a small file that
/// construction *is* the measurement: the `tiny/resolve` benchmark regressed ~10%
/// on instruction count when nine cross-file containers were added inline, taking
/// the struct from 296 to 608 bytes. Moving them behind
/// [`ResolveWalker::cross_file`] brought it back.
///
/// So the size is pinned. A new field that pushes past the bound is not
/// necessarily wrong — but it must be a deliberate decision made against a
/// benchmark, not one discovered by CodSpeed after the fact. Prefer
/// [`CrossFileState`] (or a `SymbolTable` side map) for anything the common
/// single-file run does not touch; raise the bound only with a measurement.
const _: () = assert!(
    std::mem::size_of::<ResolveWalker<'_>>() <= 320,
    "ResolveWalker grew: see the note above this assertion before raising the bound"
);

/// Where a type name was found, and therefore what a miss on it means.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeLookup {
    /// Declared in *this* file — the scope tree answered, and it always wins.
    Local(SymbolId),
    /// Resolved through the workspace index; the symbol is synthesized.
    Indexed(SymbolId),
    /// An index is attached and no candidate spelling matched a class.
    NotFound,
    /// The index declined to choose between candidates.
    Unknowable,
    /// No index is attached, so nothing was looked at. Callers keep today's
    /// answer — [`UnresolvedReason::External`] — on this one.
    NotLooked,
}

/// Which of a class's members are in play, and how visible they must be.
///
/// One walk serves both, because the *set* of members is the same question in
/// both roles and the memo that answers it should not be duplicated. Only the
/// seed and the accessibility test differ.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemberRole {
    /// The class whose body the walk is inside. Its own members come from the
    /// scope tree (they are declared here), so only its *ancestors* contribute,
    /// and `PROTECTED` is reachable.
    Subclass,
    /// A class reached from outside — through a variable's declared type, a
    /// `NEW`, or a type name used as a receiver. Its own members are in play,
    /// and only `PUBLIC` ones are reachable.
    Outside,
}

/// One class-typed declaration found by the pre-pass, and what to do about it.
enum ClassTypedDecl {
    /// The named class is declared in this file: upgrade the declaration's
    /// `data_type` to `Class(sym)`, exactly as before cross-file resolution
    /// existed.
    Local(SymbolId, ResolvedType),
    /// The named class is not declared in this file, and an index is attached
    /// that might know it. Carries the declared symbol, the class name as
    /// written, and a span to hang a synthesized symbol on.
    Foreign(SymbolId, IndexName, VirtualSpan),
}

/// What the enclosing class's inherited surface says about a name.
enum InheritedLookup {
    /// A member the class inherits, with the symbol synthesized for it.
    Member(SymbolId),
    /// A name an ancestor declares and does not pass on — `PRIVATE`, or
    /// package-private, which the index treats conservatively because it does not
    /// model packages. Not a resolution, but a fact about the workspace rather
    /// than a missing declaration.
    Inaccessible,
    /// Nothing in the chain contributes the name.
    Absent,
}

/// One member an ancestor contributes to a class in this file: enough to mint a
/// symbol for it without asking the index again, plus that symbol once minted.
///
/// Split from the synthesis itself so an inherited member nobody names never
/// reaches the symbol table at all — the alternative, materializing a whole
/// ancestor's surface on first contact, would grow every subclass's symbol table
/// by its parent's full member list and give each entry a `name_span` pointing at
/// whichever unrelated reference happened to trigger the walk.
struct InheritedMember {
    kind: SymbolKind,
    /// Namespace the member resolves in — `Functions` for a method,
    /// `Values` for a property, matching where the declare pass binds a locally
    /// declared one. Checked against the reference site's candidate namespaces so
    /// a call never resolves to an inherited property, nor a bare read to an
    /// inherited method.
    namespace: NamespaceId,
    data_type: Option<ResolvedType>,
    flags: SymbolFlags,
    /// `None` until the first reference names this member.
    symbol: Option<SymbolId>,
}

impl<'a> ResolveWalker<'a> {
    fn new(
        ctx: &'a AnalysisContext<'a>,
        tree: &'a ScopeTree,
        symbols: &'a mut SymbolTable,
    ) -> Self {
        ResolveWalker {
            ctx,
            tree,
            symbols,
            references: NodeIndexVec::new(),
            types: NodeIndexVec::new(),
            diagnostics: Vec::new(),
            counts: FxHashMap::default(),
            block_var_outside: FxHashMap::default(),
            output_args: FxHashSet::default(),
            unmodelled_touch: FxHashSet::default(),
            track_block_vars: false,
            synth_fields: FxHashMap::default(),
            synth_buffers: FxHashMap::default(),
            current_class: None,
            // Deliberately not created here, not even when an index *is*
            // attached: the first cross-file write creates it, so the cost is
            // paid by the run that has something to put in it.
            cross_file: None,
        }
    }

    /// Read side of the cross-file state.
    ///
    /// `None` means "nothing has been recorded", which every caller must treat
    /// exactly as it treats an empty map — that equivalence is what makes the
    /// boxing invisible to resolution. A caller reaching this without an index
    /// attached therefore still gets today's answer rather than a wrong one.
    fn cross_file(&self) -> Option<&CrossFileState> {
        self.cross_file.as_deref()
    }

    /// Write side of the cross-file state, created on first use.
    ///
    /// Every call site sits behind an `ctx.index_loaded` gate, so this allocates
    /// only on a run that has an index to resolve against. Returning `&mut`
    /// rather than an `Option` is what keeps the call sites readable; the price
    /// is that it borrows all of `self`, so a site that also needs
    /// `self.symbols` must scope the borrow (see [`Self::lookup_inherited`]).
    fn cross_file_mut(&mut self) -> &mut CrossFileState {
        self.cross_file.get_or_insert_with(Default::default)
    }

    /// The file's explicit `USING <package>.<Name>` imports — empty when no
    /// index is attached, because nothing collected them.
    ///
    /// A slice accessor rather than `cross_file()?.using_types` because the
    /// search below indexes it in a loop whose body needs `&mut self`, and
    /// threading an `Option` through that is how an off-by-one gets in.
    fn using_types(&self) -> &[(OxablAtom, IndexName)] {
        self.cross_file().map_or(&[], |cf| &cf.using_types)
    }

    /// The file's wildcard `USING <package>.*` prefixes — empty for the reason
    /// [`Self::using_types`] gives.
    fn using_prefixes(&self) -> &[String] {
        self.cross_file().map_or(&[], |cf| &cf.using_prefixes)
    }

    /// Record the file's `USING` imports before anything tries to resolve a
    /// type name through them.
    ///
    /// Only top-level statements are scanned: ABL puts `USING` at file scope,
    /// ahead of the class or procedure body, and its effect is the whole
    /// compilation unit. Scanning bodies too would admit an import that is not
    /// legal ABL.
    ///
    /// A no-op without an index, which is what keeps a single-file run at
    /// exactly its current cost — there is nothing an import could be resolved
    /// *against*.
    fn collect_using_imports(&mut self, program: &[Statement]) {
        if !self.ctx.index_loaded {
            return;
        }
        for stmt in program {
            let StatementKind::Using { type_name, .. } = &stmt.kind else {
                continue;
            };
            match type_name.strip_suffix('*') {
                // `USING MyApp.Services.*` — names no single class, so it
                // resolves to nothing and instead widens the set of prefixes an
                // unqualified name is tried against.
                Some(prefix) => self
                    .cross_file_mut()
                    .using_prefixes
                    .push(prefix.to_string()),
                // `USING MyApp.Services.Cache` — makes `Cache` mean this one
                // qualified name. Stored under the simple name, since that is
                // the spelling a later reference will use.
                None => {
                    let simple = type_name.rsplit('.').next().unwrap_or(type_name);
                    self.cross_file_mut()
                        .using_types
                        .push((fold_atom(simple), IndexName::new(type_name)));
                }
            }
        }
    }

    /// Resolve a type name written at `scope`, in the one order every caller
    /// shares. Stated here because the reference entries it produces are what
    /// downstream units and the analyze envelope read out:
    ///
    /// 1. **The local scope tree's `Types` namespace**, from `scope` outward.
    ///    Always first, always winning: a class declared in this file must
    ///    shadow a workspace class of the same name, and the scope tree is the
    ///    only thing that knows about the former.
    /// 2. The name **as a fully-qualified index key**, when it contains a dot.
    ///    A dotted name is most often already qualified, so that is tried
    ///    before any import is applied to it.
    /// 3. Each **explicit `USING <package>.<Name>`** whose simple name matches,
    ///    in the order the file wrote its `USING` statements.
    /// 4. Each **wildcard `USING <package>.*` prefix**, in the order the file
    ///    wrote them, as `prefix + name`.
    /// 5. The name as a fully-qualified index key when it contains **no** dot —
    ///    a class in the unnamed package, tried last because an undotted name is
    ///    far more often an import than a root-level class.
    /// 6. The miss.
    ///
    /// First match wins at every step. An [`IndexAnswer::Unknowable`] at any
    /// step short-circuits rather than falling through to the next candidate:
    /// the index only says that when it found an ambiguity worth declining over,
    /// and trying a different spelling would silently paper over it.
    fn resolve_type_name(
        &mut self,
        raw: &str,
        scope: ScopeId,
        use_span: VirtualSpan,
    ) -> TypeLookup {
        let atom = fold_atom(raw);
        if let Some(sym) = self.tree.resolve(scope, NamespaceId::Types, &atom) {
            return TypeLookup::Local(sym);
        }
        self.resolve_type_in_workspace(raw, use_span)
    }

    /// Steps 2-6 of [`Self::resolve_type_name`], memoized by the folded name.
    ///
    /// Split out because two callers have already done step 1 themselves — the
    /// receiver path resolves the identifier normally before softening it, and
    /// the class-typed-declaration pre-pass looks the class up in the scope tree
    /// to decide whether it is foreign at all.
    fn resolve_type_in_workspace(&mut self, raw: &str, use_span: VirtualSpan) -> TypeLookup {
        // The one gate: with no index attached nothing was looked at, so every
        // caller keeps the answer it has today.
        if !self.ctx.index_loaded {
            return TypeLookup::NotLooked;
        }
        // The as-written spelling, *not* `fold_atom(raw)`. Every candidate this
        // memo caches an answer for is derived from `raw` verbatim and looked up
        // on a case-sensitive path, so folding the key would serve one spelling's
        // answer to another that would have resolved differently. Matches
        // `BatchIndex`'s `classes` memo one layer down, which keys on
        // `IndexName::as_written_atom` for exactly this reason — a memo above a
        // case-sensitive memo has to be at least as case-sensitive, or it
        // reintroduces the poisoning the lower one avoided.
        let key = OxablAtom::from(raw);
        // `copied()` ends the borrow on the memo before the miss path takes
        // `&mut self` below.
        if let Some(hit) = self
            .cross_file()
            .and_then(|cf| cf.type_lookups.get(&key))
            .copied()
        {
            return hit;
        }
        let answer = self.search_type_in_workspace(raw, use_span);
        self.cross_file_mut().type_lookups.insert(key, answer);
        answer
    }

    /// The uncached search. Never call directly — [`Self::resolve_type_in_workspace`]
    /// is the memoized door, and an index lookup per reference is exactly the
    /// cost this unit is benchmarked against.
    fn search_type_in_workspace(&mut self, raw: &str, use_span: VirtualSpan) -> TypeLookup {
        let dotted = raw.contains('.');
        if dotted && let Some(answer) = self.try_indexed_class(&IndexName::new(raw), use_span) {
            return answer;
        }
        let simple = fold_atom(raw);
        // Indexed rather than iterated: the loop body needs `&mut self` to mint
        // a symbol, and the candidate is cloned out before that borrow starts.
        for i in 0..self.using_types().len() {
            if self.using_types()[i].0 != simple {
                continue;
            }
            let candidate = self.using_types()[i].1.clone();
            if let Some(answer) = self.try_indexed_class(&candidate, use_span) {
                return answer;
            }
        }
        for i in 0..self.using_prefixes().len() {
            // One allocation per (name, prefix) pair actually tried, on a path
            // that runs once per distinct name in the file and only with an
            // index attached — `type_lookups` absorbs every repeat.
            let mut qualified = String::with_capacity(self.using_prefixes()[i].len() + raw.len());
            qualified.push_str(&self.using_prefixes()[i]);
            qualified.push_str(raw);
            let candidate = IndexName::new(&qualified);
            if let Some(answer) = self.try_indexed_class(&candidate, use_span) {
                return answer;
            }
        }
        if !dotted && let Some(answer) = self.try_indexed_class(&IndexName::new(raw), use_span) {
            return answer;
        }
        TypeLookup::NotFound
    }

    /// Ask the index about one candidate spelling. `None` means "keep looking";
    /// `Some` is an answer the search must stop on.
    fn try_indexed_class(&mut self, name: &IndexName, use_span: VirtualSpan) -> Option<TypeLookup> {
        // Copied out so no borrow of `self` is held across the synthesis below.
        let index = self.ctx.index;
        match index.class(name) {
            IndexAnswer::Found(descriptor) => Some(TypeLookup::Indexed(self.indexed_class_symbol(
                name,
                descriptor.kind,
                use_span,
            ))),
            // No file on the configured paths spells the name this way; another
            // candidate spelling still might.
            IndexAnswer::NotFound => None,
            IndexAnswer::Unknowable => Some(TypeLookup::Unknowable),
        }
    }

    /// The synthesized `Types` symbol for an indexed class, minted on first
    /// reference and shared by every later one.
    ///
    /// [`Symbol::synthesized`] carries the conventions — notably that the symbol
    /// goes in the symbol table only, so ordinary name resolution cannot stumble
    /// onto a workspace class that no `USING` imported.
    fn indexed_class_symbol(
        &mut self,
        name: &IndexName,
        kind: ClassKind,
        use_span: VirtualSpan,
    ) -> SymbolId {
        if let Some(sym) = self
            .cross_file()
            .and_then(|cf| cf.synth_classes.get(name))
            .copied()
        {
            return sym;
        }
        let sym = self.symbols.insert(Symbol::synthesized(
            name.as_atom().clone(),
            NamespaceId::Types,
            match kind {
                ClassKind::Class => SymbolKind::Class,
                ClassKind::Interface => SymbolKind::Interface,
            },
            use_span,
        ));
        let cf = self.cross_file_mut();
        cf.synth_classes.insert(name.clone(), sym);
        cf.indexed_class_of.insert(sym, name.clone());
        // The class's own header, recorded on the symbol the same way a local
        // header's is. Needed the moment a cross-file class became a real
        // `ResolvedType::Class`: `coercion::ClassLattice` climbs from a value's
        // class through `SymbolTable::supertypes`, so a synthesized class with no
        // entry looks like a class that inherits nothing — and a subclass assigned
        // into its parent-typed variable reads as a mismatch. That would be a
        // false positive minted by attaching an index, on the one shape
        // inheritance widening exists to allow.
        //
        // Every supertype span points at the *use site* in this file, since the
        // header is in another one and no span of it is available here. That is
        // what the spans are for anyway — a diagnostic about an unresolvable
        // parent must underline something the reader can see.
        if let IndexAnswer::Found(descriptor) = self.ctx.index.class(name) {
            let refs = |names: &[IndexName]| -> Vec<SupertypeRef> {
                names
                    .iter()
                    .map(|n| SupertypeRef {
                        name: n.clone(),
                        name_span: use_span,
                    })
                    .collect()
            };
            let inherits = descriptor.inherits.as_ref().map(|n| SupertypeRef {
                name: n.clone(),
                name_span: use_span,
            });
            let implements = refs(&descriptor.implements);
            // Nothing recorded when the header named nothing, which is what keeps
            // "declares no parent" distinguishable from "declares one that
            // resolved to nothing".
            if inherits.is_some() || !implements.is_empty() {
                self.symbols.record_supertypes(
                    sym,
                    Supertypes {
                        inherits,
                        implements,
                    },
                );
            }
        }
        sym
    }

    /// Walk the program once to find declarations with `DataType::Class(name)`
    /// and upgrade their symbol's `data_type` from `ResolvedType::Unknown` to
    /// `ResolvedType::Class(SymbolId)` when `name` resolves in the local
    /// `Types` namespace. Class names that don't resolve locally are
    /// `External` (cross-file or USING-imported) and stay `Unknown` — the
    /// type-check pass treats `Unknown` as the lattice bottom.
    fn upgrade_class_types(&mut self, program: &[Statement]) {
        let mut decls: Vec<ClassTypedDecl> = Vec::new();
        self.collect_class_upgrades(program, ScopeId::ROOT, &mut decls);
        for decl in decls {
            match decl {
                ClassTypedDecl::Local(sid, rt) => self.symbols.get_mut(sid).data_type = Some(rt),
                // A class only the workspace knows. Written into `data_type`
                // exactly as the local branch above does: the declaration's type
                // *is* that class, and holding the link in a side map instead was
                // the valve that kept the type lattice from learning it.
                ClassTypedDecl::Foreign(sid, name, span) => {
                    match self.resolve_type_in_workspace(name.as_written(), span) {
                        TypeLookup::Indexed(class) => {
                            self.symbols.get_mut(sid).data_type = Some(ResolvedType::Class(class));
                        }
                        // Nothing to link. The declaration keeps `Unknown`,
                        // which is what it has today, and a member access
                        // through it stays unresolved.
                        TypeLookup::Local(_)
                        | TypeLookup::NotFound
                        | TypeLookup::Unknowable
                        | TypeLookup::NotLooked => {}
                    }
                }
            }
        }
    }

    fn collect_class_upgrades(
        &self,
        stmts: &[Statement],
        scope: ScopeId,
        out: &mut Vec<ClassTypedDecl>,
    ) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::VariableDeclaration {
                    name,
                    type_source: TypeSource::Explicit(DataType::Class(class_name)),
                    ..
                } => {
                    if let Some(up) = self.class_upgrade(class_name, scope, name) {
                        out.push(up);
                    }
                }
                StatementKind::DefineParameter {
                    param_type:
                        ParameterType::Variable {
                            name,
                            type_source: TypeSource::Explicit(DataType::Class(class_name)),
                            ..
                        },
                    ..
                } => {
                    if let Some(up) = self.class_upgrade(class_name, scope, name) {
                        out.push(up);
                    }
                }
                StatementKind::Property {
                    name,
                    data_type: DataType::Class(class_name),
                    ..
                } => {
                    if let Some(up) = self.class_upgrade(class_name, scope, name) {
                        out.push(up);
                    }
                }
                StatementKind::Function {
                    name,
                    return_type: DataType::Class(class_name),
                    ..
                } => {
                    if let Some(up) =
                        self.class_upgrade_in_ns(class_name, scope, NamespaceId::Functions, name)
                    {
                        out.push(up);
                    }
                }
                _ => {}
            }
            // Recurse into body-bearing scopes so nested declarations also
            // get upgraded. The correct scope for body resolution is the
            // child scope declare created.
            match &stmt.kind {
                StatementKind::Procedure { body, .. } => {
                    if let Some(ps) = self.find_child_scope(scope, stmt.id, ScopeKind::Procedure) {
                        self.collect_class_upgrades(body, ps, out);
                    }
                }
                StatementKind::Function { body, .. } => {
                    if let Some(fs) = self.find_child_scope(scope, stmt.id, ScopeKind::Function) {
                        self.collect_class_upgrades(body, fs, out);
                    }
                }
                StatementKind::Class { body, .. } => {
                    if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Class) {
                        self.collect_class_upgrades(body, cs, out);
                    }
                }
                StatementKind::Interface { body, .. } => {
                    if let Some(is_) = self.find_child_scope(scope, stmt.id, ScopeKind::Interface) {
                        self.collect_class_upgrades(body, is_, out);
                    }
                }
                StatementKind::Method {
                    parameters, body, ..
                } => {
                    if let Some(ms) = self.find_child_scope(scope, stmt.id, ScopeKind::Method) {
                        self.collect_class_upgrades(parameters, ms, out);
                        self.collect_class_upgrades(body, ms, out);
                    }
                }
                StatementKind::Constructor {
                    parameters, body, ..
                } => {
                    if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Constructor)
                    {
                        self.collect_class_upgrades(parameters, cs, out);
                        self.collect_class_upgrades(body, cs, out);
                    }
                }
                StatementKind::Destructor { body } => {
                    if let Some(ds) = self.find_child_scope(scope, stmt.id, ScopeKind::Destructor) {
                        self.collect_class_upgrades(body, ds, out);
                    }
                }
                StatementKind::Do { body, .. } | StatementKind::Repeat { body, .. } => {
                    if let Some(bs) = self.find_child_scope(scope, stmt.id, ScopeKind::Block) {
                        self.collect_class_upgrades(body, bs, out);
                    }
                }
                StatementKind::ForEach { body, .. } => {
                    if let Some(bs) = self.find_child_scope(scope, stmt.id, ScopeKind::Block) {
                        self.collect_class_upgrades(body, bs, out);
                    }
                }
                StatementKind::Catch { body, .. } => {
                    if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Catch) {
                        self.collect_class_upgrades(body, cs, out);
                    }
                }
                StatementKind::Finally { body } => {
                    if let Some(fs) = self.find_child_scope(scope, stmt.id, ScopeKind::Finally) {
                        self.collect_class_upgrades(body, fs, out);
                    }
                }
                StatementKind::Block(body) => {
                    self.collect_class_upgrades(body, scope, out);
                }
                StatementKind::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    self.collect_class_upgrades(std::slice::from_ref(&**then_branch), scope, out);
                    if let Some(eb) = else_branch {
                        self.collect_class_upgrades(std::slice::from_ref(&**eb), scope, out);
                    }
                }
                StatementKind::Case {
                    when_branches,
                    otherwise,
                    ..
                } => {
                    for wb in when_branches {
                        self.collect_class_upgrades(&wb.body, scope, out);
                    }
                    if let Some(o) = otherwise {
                        self.collect_class_upgrades(o, scope, out);
                    }
                }
                StatementKind::Label { body, .. } => {
                    self.collect_class_upgrades(std::slice::from_ref(&**body), scope, out);
                }
                StatementKind::PreprocIf(pif) => {
                    self.collect_class_upgrades(&pif.then_branch, scope, out);
                    for (_, br) in &pif.elseif_branches {
                        self.collect_class_upgrades(br, scope, out);
                    }
                    if let Some(eb) = &pif.else_branch {
                        self.collect_class_upgrades(eb, scope, out);
                    }
                }
                _ => {}
            }
        }
    }

    fn class_upgrade(
        &self,
        class_name: &str,
        scope: ScopeId,
        decl_name: &Identifier,
    ) -> Option<ClassTypedDecl> {
        self.class_upgrade_in_ns(class_name, scope, NamespaceId::Values, decl_name)
    }

    fn class_upgrade_in_ns(
        &self,
        class_name: &str,
        scope: ScopeId,
        decl_ns: NamespaceId,
        decl_name: &Identifier,
    ) -> Option<ClassTypedDecl> {
        let decl_atom = fold_atom(&decl_name.name);
        let decl_sym = self.tree.get(scope).get_in(decl_ns, &decl_atom)?;
        // Only in play if still Unknown.
        if !matches!(
            self.symbols.get(decl_sym).data_type.as_ref(),
            Some(ResolvedType::Unknown)
        ) {
            return None;
        }
        let class_atom = fold_atom(class_name);
        if let Some(class_sym) = self.tree.resolve(scope, NamespaceId::Types, &class_atom) {
            return Some(ClassTypedDecl::Local(
                decl_sym,
                ResolvedType::Class(class_sym),
            ));
        }
        // No such class in this file. With an index attached the workspace may
        // still have it, and that is worth recording *because* a member access
        // through this declaration needs to know its class. Without one there is
        // nothing to ask, so the declaration keeps today's `Unknown`.
        self.ctx.index_loaded.then(|| {
            ClassTypedDecl::Foreign(
                decl_sym,
                IndexName::new(class_name),
                identifier_span(decl_name),
            )
        })
    }

    fn walk_block(&mut self, stmts: &[Statement], scope: ScopeId) {
        for stmt in stmts {
            self.walk_statement(stmt, scope);
        }
    }

    fn walk_statement(&mut self, stmt: &Statement, scope: ScopeId) {
        match &stmt.kind {
            // ---- Declarations with initializers --------------------------
            StatementKind::VariableDeclaration { initial_value, .. } => {
                if let Some(e) = initial_value {
                    self.walk_expression(e, scope, AccessMode::Read);
                }
            }
            StatementKind::DefineParameter { param_type, .. } => {
                // Only the buffer shape names a table; every other parameter
                // shape declares and reads nothing here.
                if let ParameterType::Buffer { name, target } = param_type {
                    self.credit_buffer_target(name, target, scope);
                }
            }
            StatementKind::DefineTempTable { fields, .. } => {
                for f in fields {
                    if let Some(init) = &f.initial_value {
                        for e in init {
                            self.walk_expression(e, scope, AccessMode::Read);
                        }
                    }
                }
            }
            StatementKind::DefineBuffer { name, target, .. } => {
                let (BufferTarget::Table(t) | BufferTarget::TempTable(t)) = target;
                self.credit_buffer_target(name, t, scope);
            }
            StatementKind::DefineDataset { .. } => {}
            StatementKind::DefineDataSource { .. } => {}
            StatementKind::DefineStream { .. } => {}
            StatementKind::DefineFrame { .. } => {}
            StatementKind::DefineEvent { parameters, .. } => {
                if let Some(ev) = self.find_child_scope(scope, stmt.id, ScopeKind::Method) {
                    self.walk_block(parameters, ev);
                } else {
                    self.walk_block(parameters, scope);
                }
            }
            StatementKind::Property {
                get_body,
                set_body,
                set_parameters,
                ..
            } => {
                if let Some(body) = get_body
                    && let Some(gs) = self.find_child_scope(scope, stmt.id, ScopeKind::PropertyGet)
                {
                    self.walk_block(body, gs);
                }
                if let Some(ss) = self.find_child_scope(scope, stmt.id, ScopeKind::PropertySet) {
                    self.walk_block(set_parameters, ss);
                    if let Some(body) = set_body {
                        self.walk_block(body, ss);
                    }
                }
            }

            // ---- Scope-opening declarations ------------------------------
            StatementKind::Procedure { body, .. } => {
                if let Some(ps) = self.find_child_scope(scope, stmt.id, ScopeKind::Procedure) {
                    self.walk_block(body, ps);
                }
            }
            StatementKind::Function { body, .. } => {
                if let Some(fs) = self.find_child_scope(scope, stmt.id, ScopeKind::Function) {
                    self.walk_block(body, fs);
                }
            }
            StatementKind::Class { name, body, .. } => {
                if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Class) {
                    // Remember which class we are inside so a name the scope
                    // chain cannot answer can be tried against what this class
                    // inherits. Saved and restored rather than assigned, so a
                    // nested declaration cannot leak its class outward.
                    let outer = self.current_class;
                    // Gated, because the only reader is `lookup_inherited`, which
                    // returns `Absent` without an index. Left `None` on a
                    // single-file run this costs nothing; ungated it would intern
                    // an atom and walk the scope chain once per `CLASS` for an
                    // answer nothing would ever ask for.
                    self.current_class = if self.ctx.index_loaded {
                        self.tree
                            .resolve(scope, NamespaceId::Types, &fold_atom(&name.name))
                    } else {
                        None
                    };
                    self.walk_block(body, cs);
                    self.current_class = outer;
                }
            }
            StatementKind::Interface { body, .. } => {
                if let Some(is_) = self.find_child_scope(scope, stmt.id, ScopeKind::Interface) {
                    self.walk_block(body, is_);
                }
            }
            StatementKind::Method {
                parameters, body, ..
            } => {
                if let Some(ms) = self.find_child_scope(scope, stmt.id, ScopeKind::Method) {
                    self.walk_block(parameters, ms);
                    self.walk_block(body, ms);
                }
            }
            StatementKind::Constructor {
                parameters, body, ..
            } => {
                if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Constructor) {
                    self.walk_block(parameters, cs);
                    self.walk_block(body, cs);
                }
            }
            StatementKind::Destructor { body } => {
                if let Some(ds) = self.find_child_scope(scope, stmt.id, ScopeKind::Destructor) {
                    self.walk_block(body, ds);
                }
            }
            StatementKind::Catch { body, .. } => {
                if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Catch) {
                    self.walk_block(body, cs);
                }
            }
            StatementKind::Finally { body } => {
                if let Some(fs) = self.find_child_scope(scope, stmt.id, ScopeKind::Finally) {
                    self.walk_block(body, fs);
                }
            }

            // ---- Control-flow block statements ---------------------------
            StatementKind::Do {
                loop_var,
                from,
                to,
                by,
                while_condition,
                body,
                ..
            } => {
                let bs = self
                    .find_child_scope(scope, stmt.id, ScopeKind::Block)
                    .unwrap_or(scope);
                if let Some(counter) = loop_var {
                    // The loop assigns the counter its initial value and
                    // reads/compares it each iteration — resolve it as a use
                    // (ReadWrite) so the counter variable isn't flagged unused.
                    // Binds to the enclosing definition, or to the fallback
                    // block-scoped counter minted by the declare pass.
                    self.resolve_statement_ident(
                        counter,
                        bs,
                        &[NamespaceId::Values],
                        AccessMode::ReadWrite,
                    );
                }
                if let Some(e) = from {
                    self.walk_expression(e, bs, AccessMode::Read);
                }
                if let Some(e) = to {
                    self.walk_expression(e, bs, AccessMode::Read);
                }
                if let Some(e) = by {
                    self.walk_expression(e, bs, AccessMode::Read);
                }
                if let Some(e) = while_condition {
                    self.walk_expression(e, bs, AccessMode::Read);
                }
                self.walk_block(body, bs);
            }
            StatementKind::Repeat {
                while_condition,
                body,
            } => {
                let bs = self
                    .find_child_scope(scope, stmt.id, ScopeKind::Block)
                    .unwrap_or(scope);
                if let Some(e) = while_condition {
                    self.walk_expression(e, bs, AccessMode::Read);
                }
                self.walk_block(body, bs);
            }
            StatementKind::ForEach {
                buffer,
                of_relation,
                where_clause,
                body,
                ..
            } => {
                let bs = self
                    .find_child_scope(scope, stmt.id, ScopeKind::Block)
                    .unwrap_or(scope);
                // The implicit FOR EACH buffer was declared by the declare
                // pass in this block scope; resolve it so `read_count` bumps.
                self.resolve_statement_ident(
                    buffer,
                    bs,
                    &[NamespaceId::Buffers, NamespaceId::Tables],
                    AccessMode::Read,
                );
                if let Some(of) = of_relation {
                    self.resolve_statement_ident(
                        of,
                        bs,
                        &[NamespaceId::Buffers, NamespaceId::Tables],
                        AccessMode::Read,
                    );
                }
                if let Some(w) = where_clause {
                    self.walk_expression(w, bs, AccessMode::Read);
                }
                self.walk_block(body, bs);
            }
            StatementKind::Find {
                buffer,
                key_value,
                where_clause,
                ..
            } => {
                self.resolve_statement_ident(
                    buffer,
                    scope,
                    &[NamespaceId::Buffers, NamespaceId::Tables],
                    AccessMode::Read,
                );
                if let Some(k) = key_value {
                    self.walk_expression(k, scope, AccessMode::Read);
                }
                if let Some(w) = where_clause {
                    self.walk_expression(w, scope, AccessMode::Read);
                }
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expression(condition, scope, AccessMode::Read);
                self.walk_statement(then_branch, scope);
                if let Some(eb) = else_branch {
                    self.walk_statement(eb, scope);
                }
            }
            StatementKind::Case {
                expression,
                when_branches,
                otherwise,
            } => {
                self.walk_expression(expression, scope, AccessMode::Read);
                for wb in when_branches {
                    for v in &wb.values {
                        self.walk_expression(v, scope, AccessMode::Read);
                    }
                    self.walk_block(&wb.body, scope);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o, scope);
                }
            }
            StatementKind::Block(body) => {
                self.walk_block(body, scope);
            }
            StatementKind::Label { body, .. } => {
                self.walk_statement(body, scope);
            }

            // ---- Trigger / preprocessor ---------------------------------
            StatementKind::On { kind } => match kind {
                OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => match action {
                    OnAction::Block(body) => {
                        let ts = self
                            .find_child_scope(scope, stmt.id, ScopeKind::Trigger)
                            .unwrap_or(scope);
                        self.walk_statement(body, ts);
                    }
                    OnAction::PersistentRun { arguments, .. } => {
                        for arg in arguments {
                            self.walk_expression(arg, scope, AccessMode::Read);
                        }
                    }
                    OnAction::Revert => {}
                },
                OnKind::KeyRemap { .. } => {}
            },
            StatementKind::TriggerProcedure { .. } => {}
            StatementKind::PreprocIf(pif) => {
                self.walk_expression(&pif.condition, scope, AccessMode::Read);
                self.walk_block(&pif.then_branch, scope);
                for (c, br) in &pif.elseif_branches {
                    self.walk_expression(c, scope, AccessMode::Read);
                    self.walk_block(br, scope);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb, scope);
                }
            }
            StatementKind::PreprocDefine { .. } | StatementKind::PreprocUndefine { .. } => {}
            StatementKind::PreprocMessage { expression } => {
                self.walk_expression(expression, scope, AccessMode::Read);
            }

            // ---- Assignment forms ----------------------------------------
            StatementKind::Assignment { target, value } => {
                self.walk_expression(target, scope, AccessMode::Write);
                self.walk_expression(value, scope, AccessMode::Read);
            }
            StatementKind::Assign { assignments } => {
                for pair in assignments {
                    self.walk_expression(&pair.target, scope, AccessMode::Write);
                    self.walk_expression(&pair.value, scope, AccessMode::Read);
                }
            }
            StatementKind::ExpressionStatement(expr) => {
                self.walk_expression(expr, scope, AccessMode::Read);
            }
            StatementKind::Return(opt) => {
                if let Some(e) = opt {
                    self.walk_expression(e, scope, AccessMode::Read);
                }
            }
            StatementKind::Throw(expr) => {
                self.walk_expression(expr, scope, AccessMode::Read);
            }

            // ---- Output / display ----------------------------------------
            StatementKind::Display {
                stream_name,
                items,
                except: _,
                frame,
            } => {
                if let Some(s) = stream_name {
                    self.resolve_statement_ident(
                        s,
                        scope,
                        &[NamespaceId::Streams],
                        AccessMode::Read,
                    );
                }
                for item in items {
                    self.walk_expression(&item.expression, scope, AccessMode::Read);
                    if let Some(w) = &item.when_condition {
                        self.walk_expression(w, scope, AccessMode::Read);
                    }
                }
                if let Some(fr) = frame {
                    self.resolve_statement_ident(
                        fr,
                        scope,
                        &[NamespaceId::Frames],
                        AccessMode::Read,
                    );
                }
            }
            StatementKind::Message { items, set_targets } => {
                for e in items {
                    self.walk_expression(e, scope, AccessMode::Read);
                }
                for t in set_targets {
                    self.resolve_statement_ident(
                        t,
                        scope,
                        &[NamespaceId::Values],
                        AccessMode::Write,
                    );
                }
            }

            // ---- RUN / buffer ops ---------------------------------------
            StatementKind::Run {
                target,
                arguments,
                in_handle,
                persistent_handle,
                async_handle,
                event_procedure,
                ..
            } => {
                match target {
                    RunTarget::Literal {
                        id,
                        name,
                        name_span,
                    } => self.resolve_run_target(*id, name, *name_span, scope),
                    RunTarget::Dynamic(e) => {
                        // The expression is walked first and its resolution is
                        // left strictly alone: `RUN VALUE(c-name)` reads
                        // `c-name` exactly as it always has, and the unknowable
                        // fact is about the *target*, which is a different thing
                        // recorded at a different node.
                        self.walk_expression(e, scope, AccessMode::Read);
                        self.record_computed_run_target(stmt.id);
                    }
                }
                for arg in arguments {
                    let mode = match arg.direction {
                        ParameterDirection::Input => AccessMode::Read,
                        ParameterDirection::Output => AccessMode::Write,
                        ParameterDirection::InputOutput | ParameterDirection::Return => {
                            AccessMode::ReadWrite
                        }
                    };
                    self.walk_expression(&arg.expression, scope, mode);
                    // A write-back argument means the callee assigns into the
                    // caller's variable — a genuine use of the binding even
                    // when the call site never reads it back. Reuse the
                    // resolution `walk_expression` just recorded rather than
                    // resolving again; non-identifier and unresolved arguments
                    // simply miss and are a no-op.
                    if mode != AccessMode::Read
                        && matches!(arg.expression.kind, ExpressionKind::Identifier(_))
                        && let Some(Resolution::Resolved(sym)) =
                            self.references.get(arg.expression.id)
                    {
                        let sym = *sym;
                        self.output_args.insert(sym);
                    }
                }
                if let Some(h) = in_handle {
                    self.walk_expression(h, scope, AccessMode::Read);
                }
                if let Some(h) = persistent_handle {
                    self.walk_expression(h, scope, AccessMode::Write);
                }
                if let Some(h) = async_handle {
                    self.walk_expression(h, scope, AccessMode::Write);
                }
                if let Some(e) = event_procedure {
                    self.walk_expression(e, scope, AccessMode::Read);
                }
            }
            StatementKind::Delete { buffer, .. }
            | StatementKind::Release { buffer, .. }
            | StatementKind::Validate { buffer, .. } => {
                self.resolve_statement_ident(
                    buffer,
                    scope,
                    &[NamespaceId::Buffers, NamespaceId::Tables],
                    AccessMode::Read,
                );
            }
            StatementKind::BufferCopy {
                source,
                target,
                assignments,
                ..
            } => {
                self.resolve_statement_ident(
                    source,
                    scope,
                    &[NamespaceId::Buffers],
                    AccessMode::Read,
                );
                self.resolve_statement_ident(
                    target,
                    scope,
                    &[NamespaceId::Buffers],
                    AccessMode::Write,
                );
                for pair in assignments {
                    self.walk_expression(&pair.target, scope, AccessMode::Write);
                    self.walk_expression(&pair.value, scope, AccessMode::Read);
                }
            }
            StatementKind::BufferCompare {
                source,
                target,
                result_var,
                ..
            } => {
                self.resolve_statement_ident(
                    source,
                    scope,
                    &[NamespaceId::Buffers],
                    AccessMode::Read,
                );
                self.resolve_statement_ident(
                    target,
                    scope,
                    &[NamespaceId::Buffers],
                    AccessMode::Read,
                );
                if let Some(rv) = result_var {
                    self.resolve_statement_ident(
                        rv,
                        scope,
                        &[NamespaceId::Values],
                        AccessMode::Write,
                    );
                }
            }
            StatementKind::Create { target, .. } => match target {
                CreateTarget::Name(n) => {
                    self.resolve_statement_ident(
                        n,
                        scope,
                        &[NamespaceId::Buffers, NamespaceId::Tables],
                        AccessMode::Write,
                    );
                }
                CreateTarget::Handle {
                    handle,
                    widget_pool,
                    ..
                } => {
                    self.resolve_statement_ident(
                        handle,
                        scope,
                        &[NamespaceId::Values],
                        AccessMode::Write,
                    );
                    if let Some(wp) = widget_pool {
                        self.walk_expression(wp, scope, AccessMode::Read);
                    }
                }
            },

            // ---- Event pub/sub ------------------------------------------
            StatementKind::Publish {
                event_name,
                from_handle,
                arguments,
            } => {
                self.walk_expression(event_name, scope, AccessMode::Read);
                if let Some(fh) = from_handle {
                    self.walk_expression(fh, scope, AccessMode::Read);
                }
                for arg in arguments {
                    self.walk_expression(&arg.expression, scope, AccessMode::Read);
                }
            }
            StatementKind::Subscribe {
                subscriber,
                event_name,
                target,
                ..
            } => {
                if let Some(s) = subscriber {
                    self.walk_expression(s, scope, AccessMode::Read);
                }
                self.walk_expression(event_name, scope, AccessMode::Read);
                if let SubscribeTarget::InHandle(h) = target {
                    self.walk_expression(h, scope, AccessMode::Read);
                }
            }
            StatementKind::Unsubscribe {
                subscriber,
                event_name,
                in_handle,
            } => {
                if let Some(s) = subscriber {
                    self.walk_expression(s, scope, AccessMode::Read);
                }
                if let Some(e) = event_name {
                    self.walk_expression(e, scope, AccessMode::Read);
                }
                if let Some(h) = in_handle {
                    self.walk_expression(h, scope, AccessMode::Read);
                }
            }

            // ---- Stream I/O ---------------------------------------------
            StatementKind::StreamIo {
                stream_name,
                operation,
                ..
            } => {
                if let Some(s) = stream_name {
                    self.resolve_statement_ident(
                        s,
                        scope,
                        &[NamespaceId::Streams],
                        AccessMode::Read,
                    );
                }
                match operation {
                    StreamOperation::From(e) | StreamOperation::Through(e) => {
                        self.walk_expression(e, scope, AccessMode::Read);
                    }
                    StreamOperation::To { target, .. } => {
                        self.walk_expression(target, scope, AccessMode::Read);
                    }
                    StreamOperation::Close => {}
                }
            }

            // ---- External / leaf forms ----------------------------------
            StatementKind::Using {
                id,
                type_name,
                name_span,
            } => self.resolve_using(*id, type_name, *name_span),
            StatementKind::Leave(_) | StatementKind::Next(_) => {}
            StatementKind::IncludeReference { .. } | StatementKind::IncludeArgReference { .. } => {}
            StatementKind::Empty => {}
            StatementKind::Skipped {
                names,
                may_reference_tables,
            } => {
                self.credit_unmodelled_names(names, scope);
                if *may_reference_tables {
                    self.credit_unmodelled_table_names(names, scope);
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // Expression walking
    // ------------------------------------------------------------------

    fn walk_expression(&mut self, expr: &Expression, scope: ScopeId, mode: AccessMode) {
        match &expr.kind {
            ExpressionKind::Literal(_) => {}

            ExpressionKind::Identifier(id) => {
                // Bare identifier — try Values, then Buffers (ABL's default
                // buffer / implicit-buffer fallthrough).
                self.resolve_expr_ident(
                    id,
                    expr.id,
                    scope,
                    &[NamespaceId::Values, NamespaceId::Buffers],
                    mode,
                );
            }

            ExpressionKind::Add(l, r)
            | ExpressionKind::Minus(l, r)
            | ExpressionKind::Multiply(l, r)
            | ExpressionKind::Divide(l, r)
            | ExpressionKind::Modulo(l, r)
            | ExpressionKind::Equal(l, r)
            | ExpressionKind::NotEqual(l, r)
            | ExpressionKind::LessThan(l, r)
            | ExpressionKind::LessThanOrEqual(l, r)
            | ExpressionKind::GreaterThan(l, r)
            | ExpressionKind::GreaterThanOrEqual(l, r)
            | ExpressionKind::And(l, r)
            | ExpressionKind::Or(l, r)
            | ExpressionKind::Begins(l, r)
            | ExpressionKind::Matches(l, r)
            | ExpressionKind::Contains(l, r) => {
                self.walk_expression(l, scope, AccessMode::Read);
                self.walk_expression(r, scope, AccessMode::Read);
            }
            ExpressionKind::Negate(e) | ExpressionKind::Not(e) => {
                self.walk_expression(e, scope, AccessMode::Read);
            }
            ExpressionKind::IfThenElse(c, t, e) => {
                self.walk_expression(c, scope, AccessMode::Read);
                self.walk_expression(t, scope, AccessMode::Read);
                self.walk_expression(e, scope, AccessMode::Read);
            }

            ExpressionKind::FunctionCall { name, arguments } => {
                // Functions and Procedures share a call surface in expression
                // position (internal procedure calls surface as function
                // calls syntactically).
                self.resolve_expr_ident(
                    name,
                    expr.id,
                    scope,
                    &[NamespaceId::Functions, NamespaceId::Procedures],
                    AccessMode::Read,
                );
                for a in arguments {
                    self.walk_expression(a, scope, AccessMode::Read);
                }
            }
            ExpressionKind::MethodCall {
                object,
                method,
                arguments,
            } => {
                // The receiver may be a local handle, a package-qualified type
                // (`acme.security.Auth:Check`), or a static class name
                // (`MyStatics:CurrentCompany`). Unresolved receivers are
                // softened to External so LINT0001 does not fire on type
                // references the single-file model cannot see (#58 D/C).
                self.walk_receiver(object, scope);
                // A method is a cross-class member: with no index it stays
                // unrecorded, as it always has been; with one, it resolves
                // against the receiver's class. `Functions` only, so `obj:foo()`
                // cannot land on a property named `foo`.
                self.resolve_member_reference(
                    object,
                    method,
                    expr.id,
                    &[NamespaceId::Functions],
                    AccessMode::Read,
                );
                for a in arguments {
                    self.walk_expression(a, scope, AccessMode::Read);
                }
            }
            ExpressionKind::MemberAccess { object, member } => {
                self.walk_receiver(object, scope);
                // `Values` only, mirroring where the declare pass binds a
                // locally declared property: `obj:foo` cannot land on a method.
                // The access mode is the enclosing one, so `obj:prop = 1` credits
                // a write.
                self.resolve_member_reference(
                    object,
                    member,
                    expr.id,
                    &[NamespaceId::Values],
                    mode,
                );
            }
            ExpressionKind::ArrayAccess { array, index } => {
                self.walk_expression(array, scope, mode);
                self.walk_expression(index, scope, AccessMode::Read);
            }

            ExpressionKind::FieldAccess { qualifier, field } => {
                self.resolve_field_access(qualifier, field, expr.id, scope, mode);
            }

            ExpressionKind::New {
                class_name,
                arguments,
            } => {
                let atom = fold_atom(class_name);
                let span = VirtualSpan::new(expr.span.start, expr.span.end);
                // A `NEW` operand is unambiguously a type name, so unlike a
                // receiver it gets the full workspace search and a miss is a
                // fact about the workspace rather than a shrug.
                match self.resolve_type_name(class_name, scope, span) {
                    TypeLookup::Local(sym) | TypeLookup::Indexed(sym) => {
                        self.references.insert(expr.id, Resolution::Resolved(sym));
                        self.bump_count(sym, AccessMode::Read);
                    }
                    TypeLookup::NotFound => {
                        self.references.insert(
                            expr.id,
                            Resolution::Unresolved {
                                name: atom,
                                reason: UnresolvedReason::NotFoundInWorkspace,
                            },
                        );
                    }
                    TypeLookup::Unknowable => {
                        self.references.insert(
                            expr.id,
                            Resolution::Unresolved {
                                name: atom,
                                reason: UnresolvedReason::Unknowable,
                            },
                        );
                    }
                    // No index attached: today's answer, unchanged. `NEW` of a
                    // `USING`-imported class is the archetypal `External`.
                    TypeLookup::NotLooked => {
                        self.references.insert(
                            expr.id,
                            Resolution::Unresolved {
                                name: atom,
                                reason: UnresolvedReason::External,
                            },
                        );
                    }
                }
                for a in arguments {
                    self.walk_expression(a, scope, AccessMode::Read);
                }
            }

            ExpressionKind::CanFind {
                buffer,
                where_clause,
                ..
            } => {
                self.resolve_expr_ident(
                    buffer,
                    expr.id,
                    scope,
                    &[NamespaceId::Buffers, NamespaceId::Tables],
                    AccessMode::Read,
                );
                if let Some(w) = where_clause {
                    self.walk_expression(w, scope, AccessMode::Read);
                }
            }

            ExpressionKind::IncludeReference { .. }
            | ExpressionKind::IncludeArgReference { .. }
            | ExpressionKind::PreprocReference(_) => {}
            ExpressionKind::PreprocIf(pif) => {
                self.walk_expression(&pif.condition, scope, AccessMode::Read);
                self.walk_expression(&pif.then_branch, scope, mode);
                for (c, br) in &pif.elseif_branches {
                    self.walk_expression(c, scope, AccessMode::Read);
                    self.walk_expression(br, scope, mode);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_expression(eb, scope, mode);
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // Resolution helpers
    // ------------------------------------------------------------------

    /// Resolve an `Identifier` in an **expression** position: records a
    /// `Resolution` at the wrapping [`Expression`]'s NodeId.
    fn resolve_expr_ident(
        &mut self,
        id: &Identifier,
        expr_id: NodeId,
        scope: ScopeId,
        namespaces: &[NamespaceId],
        mode: AccessMode,
    ) {
        let atom = fold_atom(&id.name);
        for &ns in namespaces {
            if let Some(sym) = self.tree.resolve(scope, ns, &atom) {
                self.references.insert(expr_id, Resolution::Resolved(sym));
                self.bump_count(sym, mode);
                if self.track_block_vars {
                    self.note_block_var_use(sym, scope, mode);
                }
                return;
            }
        }
        // Inherited members, from a superclass or an implemented interface the
        // workspace index could answer for. Tried *after* the scope chain and
        // never before it: a member the class declares itself shadows the one it
        // inherits, and the local scope tree is the only thing that knows about
        // the former. Nothing happens here without a loaded index, so the
        // single-file answer is unchanged by construction.
        if let Some(class) = self.current_class {
            match self.lookup_inherited(class, &atom, namespaces, id) {
                InheritedLookup::Member(sym) => {
                    self.references.insert(expr_id, Resolution::Resolved(sym));
                    self.bump_count(sym, mode);
                    return;
                }
                InheritedLookup::Inaccessible => {
                    // `NotInScope` would claim there is no such symbol, which is
                    // false and is what LINT0001 renders. `NotFoundInWorkspace`
                    // says what actually happened — we looked, and the name is
                    // not available here — and every rule already skips it, so an
                    // inaccessible member stays silent until the access-check
                    // rule that should own it exists.
                    self.references.insert(
                        expr_id,
                        Resolution::Unresolved {
                            name: atom,
                            reason: UnresolvedReason::NotFoundInWorkspace,
                        },
                    );
                    return;
                }
                // Nothing inherited by this name: fall through to the fallbacks
                // below, so the answer is exactly today's.
                InheritedLookup::Absent => {}
            }
        }

        // Reserved lock / wait option keywords used as bare identifiers —
        // most commonly as arguments to dynamic query methods
        // (`hq:GET-FIRST(NO-LOCK, NO-WAIT)`). The parser admits them via
        // `can_be_identifier`; they are reserved words so they cannot be
        // user variables. Soft-resolve as External so LINT0001 stays silent
        // (#58 second-pass residual).
        if matches!(
            atom.as_ref(),
            "no-lock" | "share-lock" | "exclusive-lock" | "no-wait"
        ) {
            self.references.insert(
                expr_id,
                Resolution::Unresolved {
                    name: atom,
                    reason: UnresolvedReason::External,
                },
            );
            return;
        }

        // Fall back to the built-in ABL function registry. A locally declared
        // symbol always wins (checked above), so user shadowing is preserved;
        // only names that match no local declaration reach here. Built-ins are
        // defined by the runtime rather than this file, so they are recorded as
        // `External` — the same bucket used for USING imports and dynamic calls,
        // which every lint rule already skips. Without this, every call to a
        // built-in (`LENGTH`, `ENTRY`, `SUBSTRING`, ...) was reported as an
        // `undefined-symbol` false positive.
        //
        // This helper backs bare identifiers (Values/Buffers), function calls
        // (Functions/Procedures) and CAN-FIND (Buffers/Tables), so the fallback
        // applies to all three — intentionally: many built-ins are used without
        // parentheses (`TODAY`, `NOW`, `TIME`) and so parse as bare identifiers.
        // The cost is that a genuinely undefined name colliding with a built-in
        // function name is not flagged; that narrow false negative is an
        // accepted trade against the false-positive volume this eliminates.
        if oxabl_lexer::is_builtin_function(&atom) {
            self.references.insert(
                expr_id,
                Resolution::Unresolved {
                    name: atom,
                    reason: UnresolvedReason::External,
                },
            );
            return;
        }
        // Schema fallback: a bare table name in a buffer-capable position
        // (bare identifier / CAN-FIND — anything whose namespace list
        // includes `Buffers`) resolves to a synthesized default-buffer
        // symbol when the schema is loaded. Local declarations and built-ins
        // both win over the schema table, matching ABL shadowing rules.
        // Function calls (Functions/Procedures) never reach this fallback —
        // a call named like a table stays `NotInScope`.
        if self.ctx.schema_loaded
            && namespaces.contains(&NamespaceId::Buffers)
            && let Some(tid) = self.ctx.schema.table_id(&atom)
        {
            let bsym = self.synth_table_buffer_symbol(tid, &atom, id);
            self.references.insert(expr_id, Resolution::Resolved(bsym));
            self.bump_count(bsym, mode);
            return;
        }
        // Unqualified field fallback: ABL lets a field of a buffer in scope
        // be referenced by its bare name, without the `buffer.` qualifier —
        // the buffer is inferred from the enclosing block(s). This is
        // idiomatic inside a `FOR EACH ... BREAK BY buffer.field` block, where
        // the break field is named bare inside `FIRST-OF(field)` /
        // `LAST-OF(field)` (whose argument parses as a plain identifier
        // expression and lands here). Gated on `Buffers` being a candidate
        // namespace so call names (Functions/Procedures) never resolve as
        // fields, and tried only after local values, buffers, built-ins and
        // schema table names, so a real declaration always wins.
        if namespaces.contains(&NamespaceId::Buffers)
            && let Some(fsym) = self.resolve_bare_block_field(&atom, id, scope)
        {
            self.references.insert(expr_id, Resolution::Resolved(fsym));
            self.bump_count(fsym, mode);
            return;
        }
        self.references.insert(
            expr_id,
            Resolution::Unresolved {
                name: atom,
                reason: UnresolvedReason::NotInScope,
            },
        );
    }

    /// Resolve a `USING` import and record the answer as a reference entry on
    /// the statement's own node id.
    ///
    /// That entry is the observable fact: a `USING` is a reference to a thing
    /// named at a use site, so it belongs in the `references` side table like
    /// any other, and the statement carries a `NodeId` and a `name_span`
    /// precisely so it can. The span over the qualified name is what a later
    /// diagnostic about an import nothing declares has to point at.
    ///
    /// Two deliberate silences:
    ///
    /// * **A wildcard import gets no entry at all.** `USING MyApp.*` names no
    ///   single class, so there is nothing for a reference to point *to*; its
    ///   effect is to widen [`Self::using_prefixes`], which
    ///   [`Self::collect_using_imports`] already did.
    /// * **With no index attached there is no entry either.** Today a `USING`
    ///   produces nothing here, and recording an `External` placeholder would
    ///   add a `references` row to every single-file analysis of every file that
    ///   imports anything — visible in the analyze envelope, on a run where
    ///   nothing was looked at.
    ///
    /// No count is bumped on a resolved import. An import is not a *use* of the
    /// class, and a future unused-import rule needs to be able to tell the
    /// difference.
    fn resolve_using(&mut self, id: NodeId, type_name: &str, name_span: Span) {
        if !self.ctx.index_loaded || type_name.ends_with('*') {
            return;
        }
        let span = VirtualSpan::new(name_span.start, name_span.end);
        // A `USING` name is always fully qualified as written — it is what
        // *defines* an import, so it is not resolved through other imports.
        let name = IndexName::new(type_name);
        let resolution = match self.try_indexed_class(&name, span) {
            Some(TypeLookup::Indexed(sym)) => Resolution::Resolved(sym),
            Some(TypeLookup::Unknowable) => Resolution::Unresolved {
                name: name.as_atom().clone(),
                reason: UnresolvedReason::Unknowable,
            },
            // `None` is the index's `NotFound`: an index was attached, it looked
            // on the configured paths, and no file declares this class. That is
            // a fact about the workspace, not a missing capability.
            None => Resolution::Unresolved {
                name: name.as_atom().clone(),
                reason: UnresolvedReason::NotFoundInWorkspace,
            },
            // `try_indexed_class` answers with these two states only; the arms
            // exist so a third would be a compile error rather than a silent
            // fall-through.
            Some(TypeLookup::Local(_))
            | Some(TypeLookup::NotFound)
            | Some(TypeLookup::NotLooked) => {
                return;
            }
        };
        self.references.insert(id, resolution);
    }

    /// Resolve a literal `RUN` target — `RUN post-order.p`,
    /// `RUN "orders/recalc-total.p"`, `RUN some-internal-procedure` — and record
    /// the answer on the target's own node id.
    ///
    /// The order is not negotiable:
    ///
    /// 1. **The local `Procedures` namespace, and if it answers the index is not
    ///    asked at all.** An internal `PROCEDURE` declared in this file is what
    ///    `RUN its-name` means, full stop; consulting the workspace first — or
    ///    even as a tiebreak — would let a same-named program on the search path
    ///    hijack an internal call, and a wrong `RUN` link mis-attributes symbols
    ///    across the whole program graph.
    /// 2. The index, under the exactly-one-match rule it already implements. Two
    ///    files matching the name is [`IndexAnswer::Unknowable`], and this code
    ///    consumes that rather than re-deriving it: the tie-breaking the AVM does
    ///    (`.r`-versus-source precedence, working-directory-relative paths) is
    ///    exactly what the rule declines to imitate.
    ///
    /// Nothing is recorded without an index attached, which is what makes this a
    /// no-behavior-change addition: today a literal target produces no reference
    /// entry, and a run that asked no cross-file question must not grow one.
    /// That gate also covers step 1 deliberately — resolving the local procedure
    /// only when an index is present looks arbitrary, but the alternative is a
    /// `references` row (and, if a count were credited, a `read_count`) that
    /// appears and disappears with an unrelated flag. An index-dependent answer
    /// to a purely local question is the worse of the two.
    ///
    /// No count is bumped on a resolved target, for that same reason: a `RUN` of
    /// an internal procedure genuinely is a use of it, but crediting it here
    /// would make the same file report different `read_count`s with and without
    /// an index. Whether a procedure was ever run is a question for the unit that
    /// can answer it identically either way.
    fn resolve_run_target(&mut self, id: NodeId, raw: &str, name_span: Span, scope: ScopeId) {
        if !self.ctx.index_loaded {
            return;
        }
        let atom = fold_atom(raw);
        if let Some(sym) = self.tree.resolve(scope, NamespaceId::Procedures, &atom) {
            self.references.insert(id, Resolution::Resolved(sym));
            return;
        }
        // Copied out so no borrow of `self` is held across the synthesis below.
        let index = self.ctx.index;
        let name = IndexName::new(raw);
        let span = VirtualSpan::new(name_span.start, name_span.end);
        let resolution = match index.program(&name) {
            IndexAnswer::Found(file) => {
                Resolution::Resolved(self.indexed_program_symbol(&name, span, file))
            }
            // An index was attached and it searched every path entry: this is a
            // fact about the workspace, not a missing capability.
            IndexAnswer::NotFound => Resolution::Unresolved {
                name: name.as_atom().clone(),
                reason: UnresolvedReason::NotFoundInWorkspace,
            },
            IndexAnswer::Unknowable => Resolution::Unresolved {
                name: name.as_atom().clone(),
                reason: UnresolvedReason::Unknowable,
            },
        };
        self.references.insert(id, resolution);
    }

    /// The synthesized `Procedures` symbol for an external program, minted on the
    /// first `RUN` that reaches it and shared by every later one.
    ///
    /// [`Symbol::synthesized`] carries the conventions, with the `RUN` site as
    /// the span. Staying out of the scope tree is especially load-bearing here: a
    /// program name like `orders/recalc-total.p` is not an ABL identifier at all,
    /// and it must be unreachable by ordinary name resolution rather than merely
    /// unlikely to be written.
    fn indexed_program_symbol(
        &mut self,
        name: &IndexName,
        use_span: VirtualSpan,
        file: IndexedFileId,
    ) -> SymbolId {
        if let Some(sym) = self
            .cross_file()
            .and_then(|cf| cf.synth_programs.get(name))
            .copied()
        {
            return sym;
        }
        let sym = self.symbols.insert(Symbol::synthesized(
            name.as_atom().clone(),
            NamespaceId::Procedures,
            SymbolKind::Procedure,
            use_span,
        ));
        self.symbols.record_program_file(sym, file);
        self.cross_file_mut()
            .synth_programs
            .insert(name.clone(), sym);
        sym
    }

    /// Record that a `RUN VALUE(<expr>)` target cannot be known statically.
    ///
    /// This is the canonical instance of the unknowable signal: no amount of
    /// indexing resolves a name the program computes at run time, which is
    /// precisely why it must be distinguishable from "searched for and absent" —
    /// a consumer may report the second and must never report the first.
    ///
    /// Anchored on the **statement's** node id, because a dynamic target has none
    /// of its own and the expression's id is already taken by the expression's
    /// own resolution. Gated on an attached index for the same reason as
    /// [`Self::resolve_run_target`]: unknowability does not depend on an index,
    /// but a run that looked at nothing should not grow its `references` table.
    fn record_computed_run_target(&mut self, stmt_id: NodeId) {
        if !self.ctx.index_loaded {
            return;
        }
        self.references.insert(
            stmt_id,
            Resolution::Unresolved {
                name: OxablAtom::from(COMPUTED_TARGET),
                reason: UnresolvedReason::Unknowable,
            },
        );
    }

    /// Link every `DEFINE SHARED` consumer in this file to the file whose
    /// `DEFINE NEW [GLOBAL] SHARED` definition produces the same name.
    ///
    /// Runs **after** the statement walk, and that ordering is the whole reason
    /// this lives in the resolve pass rather than the declare pass. A `SHARED`
    /// name maps onto no path, so the index can only answer from files it has
    /// already read — which in practice means files a `RUN` in *this* file pulled
    /// in, and `RUN` targets are resolved by the walk. Asking during declare, or
    /// even mid-walk, would ask before the producer existed to be found, and
    /// would make the answer depend on whether the `DEFINE SHARED` happened to be
    /// written above or below the `RUN`.
    ///
    /// Records the link and nothing else. Notably absent: any comparison of the
    /// two declarations' types. See
    /// [`SymbolTable::shared_producer`](crate::SymbolTable::shared_producer).
    fn link_shared_consumers(&mut self) {
        if !self.ctx.index_loaded {
            return;
        }
        let index = self.ctx.index;
        // Collected first: the lookup borrows the symbol table immutably and the
        // record borrows it mutably. A `Vec` rather than a map because a file has
        // at most a handful of `SHARED` consumers and most have none — this
        // allocates only when one exists.
        let mut links: Vec<(SymbolId, IndexedFileId)> = Vec::new();
        for (sid, sym) in self.symbols.iter() {
            if !sym.flags.contains(SymbolFlags::SHARED) {
                continue;
            }
            // A producer is not its own consumer. The two spellings are already
            // exclusive in the AST, so this is belt-and-braces — but a link from
            // a `NEW SHARED` declaration back to the file it was found in would
            // invent a cross-file relationship out of a single declaration.
            if sym
                .flags
                .intersects(SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED)
            {
                continue;
            }
            // The symbol's name is already folded, and `shared_producer` derives
            // no path from it, so there is nothing here that needs the original
            // spelling.
            match index.shared_producer(&IndexName::new(&sym.name)) {
                IndexAnswer::Found(file) => links.push((sid, file)),
                // Nothing to link, and nothing to say about it: an unlinked
                // consumer is still a perfectly good local declaration, and two
                // producers is the same ambiguity the literal-`RUN` rule declines
                // to guess at.
                IndexAnswer::NotFound | IndexAnswer::Unknowable => {}
            }
        }
        for (sid, file) in links {
            self.symbols.record_shared_producer(sid, file);
        }
    }

    /// Resolve a member named through a receiver (`obj:method()`, `obj:prop`)
    /// against the receiver's class, when that class came from the workspace.
    ///
    /// Records a reference entry on the *composite* expression's node id, which
    /// nothing writes to today — so with no index attached this is a no-op and
    /// the file's `references` table is byte-for-byte what it was.
    ///
    /// Deliberately does **not** write the member's type into the `types` side
    /// table. `check_pass` types every `MethodCall` and `MemberAccess` as
    /// `Unknown`, and making a real type flow out of one would feed
    /// `type-mismatch-assignment` on shapes it is silent about today. The
    /// member's declared type is on the resolved symbol, where a consumer that
    /// wants it can read it without the type checker being changed underneath.
    fn resolve_member_reference(
        &mut self,
        object: &Expression,
        member: &Identifier,
        expr_id: NodeId,
        namespaces: &[NamespaceId],
        mode: AccessMode,
    ) {
        if !self.ctx.index_loaded {
            return;
        }
        let Some(class) = self.receiver_indexed_class(object) else {
            return;
        };
        let atom = fold_atom(&member.name);
        match self.lookup_inherited(class, &atom, namespaces, member) {
            InheritedLookup::Member(sym) => {
                self.references.insert(expr_id, Resolution::Resolved(sym));
                self.bump_count(sym, mode);
            }
            // Two different facts, one answer: the class declares the name but
            // not accessibly (`Inaccessible`), or offers nothing by that name in
            // this namespace at all (`Absent`). Either way the class *was*
            // consulted, so this is `NotFoundInWorkspace` — never `NotInScope`,
            // which would claim there is no such symbol and is what LINT0001
            // renders.
            InheritedLookup::Inaccessible | InheritedLookup::Absent => {
                self.references.insert(
                    expr_id,
                    Resolution::Unresolved {
                        name: atom,
                        reason: UnresolvedReason::NotFoundInWorkspace,
                    },
                );
            }
        }
    }

    /// The workspace class a receiver expression evaluates to, if any.
    ///
    /// Two shapes qualify, and both are already recorded in `references` by the
    /// time this runs:
    ///
    /// * the receiver resolved *to* an indexed class symbol — a type name used
    ///   as a receiver, or a `NEW` of one;
    /// * the receiver resolved to a declaration whose `AS CLASS` named an
    ///   indexed class — read off the declaration's own
    ///   `ResolvedType::Class`, which is where that link now lives.
    ///
    /// A locally declared class is deliberately not handled: its members live in
    /// the scope tree, not the index, and resolving them is a different job that
    /// does not resolve today either.
    fn receiver_indexed_class(&self, object: &Expression) -> Option<SymbolId> {
        let Some(Resolution::Resolved(sym)) = self.references.get(object.id) else {
            return None;
        };
        // No cross-file state means nothing was ever synthesized from an index,
        // so no receiver can be an indexed class — the same answer both maps
        // would give when empty.
        let cf = self.cross_file()?;
        if cf.indexed_class_of.contains_key(sym) {
            return Some(*sym);
        }
        // A declaration typed as a class: qualify only when that class came from
        // the index, which `indexed_class_of` is the record of. A locally
        // declared class's members live in the scope tree, and resolving them is
        // a different job that does not work today either.
        match self.symbols.get(*sym).data_type {
            Some(ResolvedType::Class(class)) if cf.indexed_class_of.contains_key(&class) => {
                Some(class)
            }
            _ => None,
        }
    }

    /// A receiver identifier that resolved to nothing local may still be a
    /// *type* name — a `USING`-imported class used as a static receiver. Try the
    /// workspace before [`Self::soften_unresolved_to_external`] files it as
    /// "we did not look".
    ///
    /// Acts **only** on a `NotInScope` entry, which is what makes it invisible to
    /// every rule: on the receiver path a `NotInScope` is unconditionally
    /// softened to `External` one line later, so overwriting it cannot change
    /// what any rule sees. A `NoSchema` or already-`External` entry is left
    /// alone for the same reason inverted — those *do* reach a rule, and quietly
    /// resolving one would remove a finding, which is as much a behavior change
    /// as adding one.
    ///
    /// Every non-hit answer is left in place, so the softening still produces
    /// today's `External`. A receiver that is not a type is very often a dynamic
    /// handle, and "no such class in the workspace" is the wrong thing to say
    /// about one.
    fn try_receiver_type(&mut self, expr_id: NodeId, raw: &str, use_span: VirtualSpan) {
        if !matches!(
            self.references.get(expr_id),
            Some(Resolution::Unresolved {
                reason: UnresolvedReason::NotInScope,
                ..
            })
        ) {
            return;
        }
        match self.resolve_type_in_workspace(raw, use_span) {
            TypeLookup::Indexed(sym) => {
                self.references.insert(expr_id, Resolution::Resolved(sym));
            }
            TypeLookup::Local(_)
            | TypeLookup::NotFound
            | TypeLookup::Unknowable
            | TypeLookup::NotLooked => {}
        }
    }

    /// Try to resolve a bare identifier as an unqualified field of a buffer
    /// visible from `scope`. Walks every buffer bound in `scope` and its
    /// ancestors (`FOR EACH` implicit buffers, `DEFINE BUFFER`, synthesized
    /// default buffers) and, for the first whose backing schema table has a
    /// field matching `atom`, synthesizes (or reuses) the typed `Field`
    /// symbol. Requires a loaded schema — field validation needs the table's
    /// column list; without one there is nothing to check the name against.
    /// A field shared by two in-scope buffers is technically ambiguous in
    /// ABL, but resolving to the first match is the right call for lint: it
    /// avoids a false `undefined-symbol`, and this pass does not adjudicate
    /// ambiguity.
    fn resolve_bare_block_field(
        &mut self,
        atom: &OxablAtom,
        id: &Identifier,
        scope: ScopeId,
    ) -> Option<SymbolId> {
        if !self.ctx.schema_loaded {
            return None;
        }
        // `ctx.schema` is a shared reference copied out of `self`, keeping the
        // field lookup independent of the `&mut self` synthesis below.
        let schema = self.ctx.schema;
        let mut hit: Option<(TableId, ResolvedType)> = None;
        'scan: for sid in self.tree.ancestors(scope) {
            let buffers = &self.tree.get(sid).bindings[NamespaceId::Buffers.index()];
            for (_name, bsym) in buffers.iter() {
                if let Some(tid) = self.symbols.get(bsym).table_id
                    && let Some(table) = schema.get_by_id(tid)
                    && let FieldResolution::Unique(f) = table.resolve_field(atom)
                {
                    hit = Some((tid, ResolvedType::from_schema_field(f)));
                    break 'scan;
                }
            }
        }
        let (tid, ty) = hit?;
        Some(self.synth_field_symbol(tid, atom, ty, id))
    }

    /// Walk an expression used as a method/member *receiver*. Unresolved
    /// identifiers and field-access chains become `External` rather than
    /// `NotInScope` — package-qualified static types and dynamic handles are
    /// not local symbols (#58 items C/D).
    fn walk_receiver(&mut self, expr: &Expression, scope: ScopeId) {
        match &expr.kind {
            ExpressionKind::Identifier(id) => {
                // The scope tree first, always: a locally declared name must not
                // be shadowed by a workspace class that happens to share it.
                self.walk_expression(expr, scope, AccessMode::Read);
                self.try_receiver_type(expr.id, &id.name, identifier_span(id));
                self.soften_unresolved_to_external(expr.id);
            }
            ExpressionKind::FieldAccess { qualifier, field } => {
                // Soften each link of a package path; still attempt real
                // buffer/table field resolution first.
                self.walk_receiver(qualifier, scope);
                self.resolve_field_access(qualifier, field, expr.id, scope, AccessMode::Read);
                self.soften_unresolved_to_external(expr.id);
                // resolve_field_access also records the bare-identifier
                // qualifier; soften that too when it was NotInScope.
                self.soften_unresolved_to_external(qualifier.id);
            }
            ExpressionKind::MemberAccess { object, .. }
            | ExpressionKind::MethodCall { object, .. } => {
                self.walk_receiver(object, scope);
            }
            ExpressionKind::ArrayAccess { array, index } => {
                self.walk_receiver(array, scope);
                self.walk_expression(index, scope, AccessMode::Read);
            }
            _ => {
                self.walk_expression(expr, scope, AccessMode::Read);
                self.soften_unresolved_to_external(expr.id);
            }
        }
    }

    fn soften_unresolved_to_external(&mut self, expr_id: NodeId) {
        if let Some(Resolution::Unresolved {
            name,
            reason: UnresolvedReason::NotInScope,
        }) = self.references.get(expr_id).cloned()
        {
            self.references.insert(
                expr_id,
                Resolution::Unresolved {
                    name,
                    reason: UnresolvedReason::External,
                },
            );
        }
    }

    /// Resolve an `Identifier` in a **statement** position (buffer name in
    /// `DELETE`, stream name in `DISPLAY STREAM`, etc.). Statement-level
    /// identifiers don't carry their own NodeId in v1, so no entry is
    /// recorded in the `references` side table — only read/write counts
    /// bump on resolution. Lint rules that need the statement-level
    /// identifier span walk the AST directly.
    fn resolve_statement_ident(
        &mut self,
        id: &Identifier,
        scope: ScopeId,
        namespaces: &[NamespaceId],
        mode: AccessMode,
    ) {
        if let Some(sym) = self.lookup_statement_ident(id, scope, namespaces) {
            self.bump_count(sym, mode);
            if self.track_block_vars {
                self.note_block_var_use(sym, scope, mode);
            }
        }
    }

    /// The namespace lookup half of [`Self::resolve_statement_ident`], with no
    /// side effects at all: no count bump, no block-var note, and — critically —
    /// no write to the `references` side table.
    ///
    /// That last property is what makes it safe for the best-effort harvest in
    /// [`Self::credit_unmodelled_names`]. Contrast `resolve_expr_ident` and
    /// `resolve_field_access`, which both record `Resolution::Unresolved` on a
    /// miss and thereby feed LINT0001 / LINT0003.
    fn lookup_statement_ident(
        &self,
        id: &Identifier,
        scope: ScopeId,
        namespaces: &[NamespaceId],
    ) -> Option<SymbolId> {
        let atom = fold_atom(&id.name);
        namespaces
            .iter()
            .find_map(|&ns| self.tree.resolve(scope, ns, &atom))
    }

    /// Best-effort-credit the names harvested from a recognized-but-unmodelled
    /// statement ([`StatementKind::Skipped`]).
    ///
    /// Only [`NamespaceId::Values`] is consulted: the three count-gated rules
    /// restrict themselves to `Variable` and `Parameter` symbols, so a flag set
    /// on a field, buffer or table could never suppress anything, and each extra
    /// namespace would cost a full scope-chain walk per harvested miss on a path
    /// that is already the expensive half of this feature.
    ///
    /// A name that resolves to nothing is dropped in silence — no diagnostic, no
    /// `references` entry. Over-crediting can only lose a diagnostic; it can
    /// never invent one.
    fn credit_unmodelled_names(&mut self, names: &[Identifier], scope: ScopeId) {
        for id in names {
            if let Some(sym) = self.lookup_statement_ident(id, scope, &[NamespaceId::Values]) {
                self.unmodelled_touch.insert(sym);
            }
        }
    }

    /// The table half of the same harvest, for a `Skipped` node the parser marked
    /// as naming a table (`DEFINE QUERY`, `OPEN QUERY`, `EMPTY TEMP-TABLE` — #130).
    ///
    /// Runs *in addition to* [`Self::credit_unmodelled_names`], never instead of
    /// it: a token can resolve in both namespaces when the program shadows names,
    /// and the two sides record different facts. The value side records that the
    /// symbol's counts cannot be judged; this side records a real read, because
    /// every form carrying the marker reads its table in every spelling its
    /// grammar admits.
    ///
    /// [`Self::resolve_statement_ident`] rather than hand-mutating the symbol: it
    /// owns folded lookup, count bumping, and block-use bookkeeping, and it stays
    /// silent on a miss — which the deliberately over-inclusive harvest requires,
    /// since most candidates in a query statement are not tables at all.
    fn credit_unmodelled_table_names(&mut self, names: &[Identifier], scope: ScopeId) {
        for id in names {
            self.resolve_statement_ident(
                id,
                scope,
                &[NamespaceId::Buffers, NamespaceId::Tables],
                AccessMode::Read,
            );
        }
    }

    /// Credit one read on the table or temp-table a buffer definition names
    /// (#130): `DEFINE BUFFER b FOR tt.` and `DEFINE PARAMETER BUFFER b FOR tt.`
    ///
    /// The guard is load-bearing. `DEFINE BUFFER Customer FOR Customer.` is the
    /// standard ABL block-scoping idiom, and by the time the use-walk runs, the
    /// declare pass has already bound the new buffer under that same folded name
    /// in the very namespace this lookup searches first. Without the guard the
    /// target resolves to the buffer being declared and credits it a read for
    /// existing — which would silence every count-gated rule for that symbol.
    fn credit_buffer_target(&mut self, buffer: &Identifier, target: &Identifier, scope: ScopeId) {
        if fold_atom(&buffer.name) == fold_atom(&target.name) {
            return;
        }
        self.resolve_statement_ident(
            target,
            scope,
            &[NamespaceId::Buffers, NamespaceId::Tables],
            AccessMode::Read,
        );
    }

    /// Resolve `qualifier.field`. When the qualifier is a bare identifier,
    /// try `Buffers` (the common `table.field` / `buffer.field` case) and
    /// fall through to `Tables` (implicit default buffer); if neither
    /// resolves, fall back to the schema so a bare table name binds a
    /// synthesized default buffer. A resolved qualifier with a `table_id`
    /// link drives real field lookup against the schema — valid fields
    /// resolve to a synthesized `Field` symbol (typed from the schema),
    /// invalid fields are `NotInScope`. When schema is absent, the
    /// composite expression is `Unresolved { NoSchema }`.
    fn resolve_field_access(
        &mut self,
        qualifier: &Expression,
        field: &Identifier,
        expr_id: NodeId,
        scope: ScopeId,
        mode: AccessMode,
    ) {
        // The qualifier is typically an Identifier (table/buffer name).
        // Non-identifier qualifiers (e.g. `foo():bar.baz`) are walked
        // normally; field is External and not recorded.
        let ExpressionKind::Identifier(qid) = &qualifier.kind else {
            self.walk_expression(qualifier, scope, AccessMode::Read);
            return;
        };

        let qatom = fold_atom(&qid.name);
        // 1. Try Buffers (DEFINE BUFFER, FOR EACH implicit, schema default
        //    buffer), then Tables.
        let qresolved = self
            .tree
            .resolve(scope, NamespaceId::Buffers, &qatom)
            .or_else(|| self.tree.resolve(scope, NamespaceId::Tables, &qatom));

        match qresolved {
            Some(qsym) => {
                // Qualifier bound to a local buffer/table symbol; record the
                // qualifier Expression's NodeId as resolved. The outer
                // `expr_id` (the FieldAccess Expression) is where the field
                // reference lives.
                self.references
                    .insert(qualifier.id, Resolution::Resolved(qsym));
                self.bump_count(qsym, AccessMode::Read);

                let resolution = self.field_resolution(qsym, field, mode);
                self.references.insert(expr_id, resolution);
            }
            None => {
                // Schema fallback for the bare `Customer.Name` case: the
                // qualifier is no local buffer/table, but if it names a
                // schema table it binds a synthesized default buffer and
                // the field resolves through the same lookup as the
                // resolved-qualifier arm. Without this, bare table.field
                // references were a double false positive (LINT0001 on the
                // qualifier *and* LINT0003 on the field).
                if self.ctx.schema_loaded
                    && let Some(tid) = self.ctx.schema.table_id(&qatom)
                {
                    let bsym = self.synth_table_buffer_symbol(tid, &qatom, qid);
                    self.references
                        .insert(qualifier.id, Resolution::Resolved(bsym));
                    self.bump_count(bsym, AccessMode::Read);
                    let resolution = self.field_resolution(bsym, field, mode);
                    self.references.insert(expr_id, resolution);
                    return;
                }
                // Unknown qualifier — either schema-absent buffer (NoSchema)
                // or truly undefined (NotInScope). Without schema, buffers
                // fall through to NoSchema; with schema they're NotInScope.
                let reason = if self.ctx.schema_loaded {
                    UnresolvedReason::NotInScope
                } else {
                    UnresolvedReason::NoSchema
                };
                self.references.insert(
                    qualifier.id,
                    Resolution::Unresolved {
                        name: qatom.clone(),
                        reason,
                    },
                );
                let field_atom = fold_atom(&field.name);
                self.references.insert(
                    expr_id,
                    Resolution::Unresolved {
                        name: field_atom,
                        reason,
                    },
                );
            }
        }
    }

    /// Resolve `field` against a resolved qualifier symbol. When the
    /// qualifier carries a `table_id` link and the schema is loaded, this
    /// performs real field lookup: a hit synthesizes (or reuses) a `Field`
    /// symbol typed from the schema; a miss is `NotInScope`. Qualifiers
    /// without a schema link (temp-table buffers, missing tables) keep the
    /// legacy `External` resolution; schema-absent stays `NoSchema`.
    ///
    /// `mode` is the access mode of the field reference; a resolved field
    /// symbol has its read/write count bumped accordingly so `oxabl analyze`
    /// reports real usage counts for schema fields (issue #60).
    fn field_resolution(
        &mut self,
        qsym: SymbolId,
        field: &Identifier,
        mode: AccessMode,
    ) -> Resolution {
        let field_atom = fold_atom(&field.name);
        match self.symbols.get(qsym).table_id {
            Some(tid) if self.ctx.schema_loaded => {
                // `ctx.schema` is a shared reference copied out of `self`,
                // so the field borrow is independent of the `&mut self`
                // needed by symbol synthesis below.
                let schema = self.ctx.schema;
                // ABL allows an unambiguous leading-substring abbreviation of a
                // field name, so resolve by exact-then-unique-prefix. An
                // ambiguous abbreviation (prefix matching 2+ fields) is not a
                // legal reference and stays unresolved, same as a true miss.
                match schema.get_by_id(tid).map(|t| t.resolve_field(&field_atom)) {
                    Some(FieldResolution::Unique(f)) => {
                        let resolved_ty = ResolvedType::from_schema_field(f);
                        let fsym = self.synth_field_symbol(tid, &field_atom, resolved_ty, field);
                        self.bump_count(fsym, mode);
                        Resolution::Resolved(fsym)
                    }
                    // Field not on the table, or an ambiguous abbreviation —
                    // genuinely unknown.
                    _ => Resolution::Unresolved {
                        name: field_atom,
                        reason: UnresolvedReason::NotInScope,
                    },
                }
            }
            // Qualifier resolved but has no schema link (temp-table buffer,
            // or buffer for a table absent from the schema) → preserve the
            // legacy behavior: External is skipped by every lint rule.
            _ if self.ctx.schema_loaded => Resolution::Unresolved {
                name: field_atom,
                reason: UnresolvedReason::External,
            },
            _ => Resolution::Unresolved {
                name: field_atom,
                reason: UnresolvedReason::NoSchema,
            },
        }
    }

    /// Return the synthesized `Field` symbol for `(tid, field_atom)`,
    /// minting one on first use. [`Symbol::synthesized`] carries the conventions
    /// — a dummy declaration marking it non-user-declared, the same convention as
    /// built-ins, and the symbol table only, so name resolution never observes it.
    fn synth_field_symbol(
        &mut self,
        tid: TableId,
        field_atom: &OxablAtom,
        data_type: ResolvedType,
        use_site: &Identifier,
    ) -> SymbolId {
        if let Some(sym) = self.synth_fields.get(&(tid, field_atom.clone())) {
            return *sym;
        }
        let sym = self.symbols.insert(Symbol {
            data_type: Some(data_type),
            ..Symbol::synthesized(
                field_atom.clone(),
                NamespaceId::Values,
                SymbolKind::Field,
                identifier_span(use_site),
            )
        });
        self.synth_fields.insert((tid, field_atom.clone()), sym);
        sym
    }

    /// Ask what `class`'s supertype chain says about `atom`, minting the member's
    /// symbol on first reference.
    ///
    /// [`InheritedLookup::Absent`] — leaving the caller's existing answer
    /// untouched — when no index is attached, when nothing in the chain
    /// contributes the name, or when the contributed member lives in a namespace
    /// the reference site is not looking in. The last case is what keeps `foo()`
    /// from resolving to an inherited *property* and a bare `foo` from resolving
    /// to an inherited *method*.
    fn lookup_inherited(
        &mut self,
        class: SymbolId,
        atom: &OxablAtom,
        namespaces: &[NamespaceId],
        use_site: &Identifier,
    ) -> InheritedLookup {
        // The one gate: with no index attached nothing was looked at, so a
        // cross-file name keeps exactly the answer it has today.
        if !self.ctx.index_loaded {
            return InheritedLookup::Absent;
        }
        self.walk_member_surface(class);

        let key = (class, atom.clone());
        // The walk above created the cross-file state if it found anything at all,
        // so `None` here means the chain contributed nothing — the same answer
        // both empty maps below would give.
        let Some(cf) = self.cross_file() else {
            return InheritedLookup::Absent;
        };
        // Copied out of the map rather than borrowed across the insert below,
        // which needs `&mut self.symbols`. Scoped in a block for the same reason:
        // the borrow of the cross-file state must end before that insert.
        let (kind, namespace, data_type, flags) = {
            let Some(member) = cf.inherited.get(&key) else {
                // Probed only in the namespaces the reference site is looking in,
                // so an inaccessible member cannot soften a reference that was
                // never asking about its namespace. Same gate as the
                // `namespaces.contains` check below, on the branch where nothing
                // accessible answered.
                let inaccessible = namespaces.iter().any(|&ns| {
                    cf.inherited_inaccessible
                        .contains(&(class, ns, atom.clone()))
                });
                return if inaccessible {
                    InheritedLookup::Inaccessible
                } else {
                    InheritedLookup::Absent
                };
            };
            if !namespaces.contains(&member.namespace) {
                return InheritedLookup::Absent;
            }
            if let Some(sym) = member.symbol {
                return InheritedLookup::Member(sym);
            }
            (
                member.kind,
                member.namespace,
                member.data_type.clone(),
                member.flags,
            )
        };
        // The declared type goes onto the symbol, exactly as a local
        // declaration's would.
        //
        // It used to be parked in a side map, so that attaching an index could be
        // merged on evidence that it changed no diagnostic: `Symbol::data_type`
        // feeds the type lattice through two readers that take it at face value —
        // `check.rs::type_from_reference`'s fallback arm and
        // `type_mismatch_assignment::target_type`, which reads `data_type`
        // directly — and either one turns an inherited `LONGCHAR` assigned into an
        // `INTEGER` from silence into a finding. That is now the *intent*: the
        // type is the one the declaring file wrote, and a cross-file resolution
        // being shape-identical to a local one is what lets every consumer read
        // one type channel instead of two.
        let sym = self.symbols.insert(Symbol {
            data_type,
            flags,
            ..Symbol::synthesized(atom.clone(), namespace, kind, identifier_span(use_site))
        });
        // Re-acquired rather than held across the insert above. The entry is
        // known to exist — the read that got us here found it.
        if let Some(member) = self.cross_file_mut().inherited.get_mut(&key) {
            member.symbol = Some(sym);
        }
        InheritedLookup::Member(sym)
    }

    /// Walk `class`'s member surface once and record what each level contributes
    /// to it.
    ///
    /// Breadth-first outward, so a member declared by a nearer level wins over
    /// the same name farther up — `or_insert` is what implements that, and it is
    /// the reason the queue is FIFO rather than a stack.
    ///
    /// **Two roles, one walk.** Which one applies is a property of the symbol,
    /// not of the call site, so it cannot be got wrong by a caller:
    ///
    /// * a class symbol this file *declares* is walked as a
    ///   [`MemberRole::Subclass`] — its own members are in the scope tree
    ///   already, so the walk starts at its supertypes, and `PROTECTED` is
    ///   reachable;
    /// * a class symbol *synthesized from the index* is walked as a
    ///   [`MemberRole::Outside`] — the walk starts at the class itself so its own
    ///   members are in play, and only `PUBLIC` ones are reachable.
    ///
    /// Sharing the walk is what keeps one memo behind both the unqualified
    /// (`calc-total()` inside a subclass) and qualified (`v-base:calc-total()`)
    /// paths, rather than two that could disagree.
    ///
    /// Termination is a **visited set**, not a depth bound. A class inheriting
    /// itself, directly or around a cycle, is a real shape in broken code, and a
    /// set says exactly what happens (each name is asked about once) where a bound
    /// would only say when we give up. In the subclass role the class's own name
    /// is seeded into it, so `CLASS a INHERITS a` neither recurses nor folds its
    /// own members in as inherited ones.
    ///
    /// Only the *first* call per class does any of this; the rest is a set probe.
    fn walk_member_surface(&mut self, class: SymbolId) {
        if !self.cross_file_mut().inherited_walked.insert(class) {
            return;
        }
        // Copied out so no borrow of `self` is held across the synthesis below.
        let index = self.ctx.index;
        let mut visited: FxHashSet<IndexName> = FxHashSet::default();
        let mut queue: VecDeque<IndexName> = VecDeque::new();
        // Cloned out rather than borrowed: the `None` arm below reads
        // `self.symbols`, and the loop after it takes `&mut self`.
        let indexed_name = self
            .cross_file()
            .and_then(|cf| cf.indexed_class_of.get(&class))
            .cloned();
        let role = match indexed_name {
            Some(name) => {
                queue.push_back(name);
                MemberRole::Outside
            }
            None => {
                visited.insert(IndexName::new(&self.symbols.get(class).name));
                if let Some(supers) = self.symbols.supertypes(class) {
                    queue.extend(
                        supers
                            .inherits
                            .iter()
                            .chain(&supers.implements)
                            .map(|s| s.name.clone()),
                    );
                }
                MemberRole::Subclass
            }
        };

        while let Some(name) = queue.pop_front() {
            if !visited.insert(name.clone()) {
                continue;
            }
            // The answer is recorded before it is acted on, on every arm. It is
            // pure bookkeeping — no pass or rule branches on it — but it is the
            // *only* record that this lookup happened: a supertype that resolved
            // mints no symbol of its own (its members do), so without this the
            // absent-parent case looks exactly like a parent that resolved and
            // contributed nothing. The analyze envelope's dependency-state
            // section is what reads it.
            match index.class(&name) {
                IndexAnswer::Found(descriptor) => {
                    self.symbols
                        .record_class_lookup(name.as_atom(), ClassLookup::Linked(descriptor.file));
                    // An interface's methods contribute exactly as a superclass's
                    // do, so the walk does not branch on `descriptor.kind`.
                    for supertype in descriptor.inherits.iter().chain(&descriptor.implements) {
                        queue.push_back(supertype.clone());
                    }
                }
                // A supertype no file on the paths declares, or one that cannot
                // be known statically, contributes nothing — and no diagnostic:
                // the unresolved name is recorded on the class symbol, and
                // reporting it is a rule's decision, not this pass's.
                IndexAnswer::NotFound => {
                    self.symbols
                        .record_class_lookup(name.as_atom(), ClassLookup::Absent);
                    continue;
                }
                IndexAnswer::Unknowable => {
                    self.symbols
                        .record_class_lookup(name.as_atom(), ClassLookup::Unknowable);
                    continue;
                }
            }
            match index.class_members(&name) {
                IndexAnswer::Found(members) => {
                    for member in members.iter() {
                        // Derived *before* the reachability check below, because
                        // both branches need it: the accessible one to gate the
                        // lookup, and the inaccessible one to key the set it
                        // records the name in. A member of a kind with no
                        // namespace is skipped by either branch — it has no
                        // namespace a reference site could be asking in, so
                        // recording it as inaccessible could only soften a
                        // reference that was never about it.
                        let namespace = match member.kind {
                            SymbolKind::Function => NamespaceId::Functions,
                            SymbolKind::Property => NamespaceId::Values,
                            // The index reports methods and properties and
                            // nothing else; a kind that gained membership later
                            // must choose its own namespace rather than
                            // defaulting into one.
                            _ => continue,
                        };
                        // A member the role cannot reach is not synthesized.
                        // Doing so would resolve a reference that ABL rejects,
                        // and would hand a later access-check rule an
                        // already-resolved reference instead of a violation. The
                        // *name* is still worth remembering, under its namespace:
                        // a reference **in that namespace** to it is an
                        // inaccessible member rather than an undefined symbol,
                        // and a reference in any other namespace is untouched.
                        if !member_reachable(member, role) {
                            self.cross_file_mut().inherited_inaccessible.insert((
                                class,
                                namespace,
                                member.name.as_atom().clone(),
                            ));
                            continue;
                        }
                        let data_type = match &member.ty {
                            MemberType::Portable(ty) => Some(ty.as_resolved().clone()),
                            // A class-typed member. `ResolvedType::Class` carries
                            // a `SymbolId` in *this* file's table and no symbol
                            // for a cross-file class exists yet — minting one is
                            // `USING`/`NEW` resolution's job — so the type stays
                            // `Unknown`. That is the lattice bottom, and exactly
                            // what a locally declared class-typed variable gets
                            // today when its class is declared elsewhere. It also
                            // avoids a second chain walk per member.
                            MemberType::Named(_) => Some(ResolvedType::Unknown),
                            // `VOID`, or a form naming no type: no type at all.
                            MemberType::Untyped => None,
                        };
                        self.cross_file_mut()
                            .inherited
                            .entry((class, member.name.as_atom().clone()))
                            .or_insert(InheritedMember {
                                kind: member.kind,
                                namespace,
                                data_type,
                                flags: access_flag(member.access)
                                    | flag_if(member.is_static, SymbolFlags::STATIC),
                                symbol: None,
                            });
                    }
                }
                IndexAnswer::NotFound | IndexAnswer::Unknowable => {}
            }
        }
    }

    /// Return the synthesized default-buffer symbol for schema table `tid`,
    /// minting one on first use. Same synthetic conventions as
    /// [`Self::synth_field_symbol`]; the symbol carries the `table_id` link
    /// so field accesses through it resolve against the schema.
    fn synth_table_buffer_symbol(
        &mut self,
        tid: TableId,
        table_atom: &OxablAtom,
        use_site: &Identifier,
    ) -> SymbolId {
        if let Some(sym) = self.synth_buffers.get(&tid) {
            return *sym;
        }
        let sym = self.symbols.insert(Symbol {
            // The one deviation from the convention, and the reason this helper
            // exists: the link is what makes field accesses through the default
            // buffer resolve against the schema.
            table_id: Some(tid),
            ..Symbol::synthesized(
                table_atom.clone(),
                NamespaceId::Buffers,
                SymbolKind::Buffer,
                identifier_span(use_site),
            )
        });
        self.synth_buffers.insert(tid, sym);
        sym
    }

    /// Linear-search for the unique child scope of `parent` created by the
    /// declare pass with `owner_node == owner && kind == kind`. Returns
    /// `None` if no such scope exists (error recovery, skipped body).
    fn find_child_scope(&self, parent: ScopeId, owner: NodeId, kind: ScopeKind) -> Option<ScopeId> {
        self.tree
            .iter()
            .find(|(_, s)| s.parent == Some(parent) && s.owner_node == owner && s.kind == kind)
            .map(|(id, _)| id)
    }

    fn bump_count(&mut self, sym: SymbolId, mode: AccessMode) {
        let entry = self.counts.entry(sym).or_insert((0, 0));
        match mode {
            AccessMode::Read => entry.0 += 1,
            AccessMode::Write => entry.1 += 1,
            AccessMode::ReadWrite => {
                entry.0 += 1;
                entry.1 += 1;
            }
        }
    }

    /// Note a reference to `sym` occurring in `scope`, for the
    /// `block-var-used-outside` analysis (LINT0005). Only block-hoisted
    /// variables (those recorded in `SymbolTable::block_defined`) are tracked;
    /// a reference whose `scope` is not lexically within the defining block is
    /// recorded as a read and/or write "outside" that block.
    fn note_block_var_use(&mut self, sym: SymbolId, scope: ScopeId, mode: AccessMode) {
        // Callers gate on `self.track_block_vars`, so this only runs when the
        // file hoisted at least one variable out of a block.
        let Some(block) = self.symbols.block_defined_scope(sym) else {
            return;
        };
        // Inside the defining block (or a nested descendant of it) is safe.
        if self.tree.ancestors(scope).any(|a| a == block) {
            return;
        }
        let entry = self.block_var_outside.entry(sym).or_insert((false, false));
        match mode {
            AccessMode::Read => entry.0 = true,
            AccessMode::Write => entry.1 = true,
            AccessMode::ReadWrite => {
                entry.0 = true;
                entry.1 = true;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn identifier_span(id: &Identifier) -> VirtualSpan {
    VirtualSpan::new(id.span.start, id.span.end)
}

/// Project one header identifier into a [`SupertypeRef`](crate::SupertypeRef).
///
/// [`IndexName::new`] folds for identity while keeping the source spelling, which
/// is what the index needs to derive a candidate file path on a case-sensitive
/// filesystem. Called once per name in a class header, so the two intern lookups
/// it costs are not on any hot path.
fn supertype_ref(id: &Identifier) -> crate::SupertypeRef {
    crate::SupertypeRef {
        name: crate::IndexName::new(&id.name),
        name_span: identifier_span(id),
    }
}

/// Whether a member is reachable in `role`.
///
/// The subclass answer defers to [`MemberDescriptor::inherited_by_subclass`] so
/// the index's own inheritance policy — including its conservative reading of
/// package-private, which it does not model — is stated in exactly one place.
/// The outside answer is spelled out here because it is this pass's policy, not
/// the index's: a class reached through a value or a type name offers only its
/// `PUBLIC` surface.
fn member_reachable(member: &MemberDescriptor, role: MemberRole) -> bool {
    match role {
        MemberRole::Subclass => member.inherited_by_subclass(),
        MemberRole::Outside => match member.access {
            AccessModifier::Public => true,
            AccessModifier::Protected
            | AccessModifier::Private
            | AccessModifier::PackagePrivate => false,
        },
    }
}

fn access_flag(access: AccessModifier) -> SymbolFlags {
    match access {
        AccessModifier::Public => SymbolFlags::PUBLIC,
        AccessModifier::Private => SymbolFlags::PRIVATE,
        AccessModifier::Protected => SymbolFlags::PROTECTED,
        AccessModifier::PackagePrivate => SymbolFlags::PACKAGE_PRIVATE,
    }
}

fn flag_if(cond: bool, f: SymbolFlags) -> SymbolFlags {
    if cond { f } else { SymbolFlags::empty() }
}

/// Map the mutually-exclusive `[NEW [GLOBAL]] SHARED` declaration booleans to
/// their `SymbolFlags`. At most one input is ever true (ast-invariants.md §12),
/// so at most one bit is set; all-false yields `SymbolFlags::empty()`.
fn shared_flags(is_shared: bool, is_new_shared: bool, is_new_global_shared: bool) -> SymbolFlags {
    flag_if(is_shared, SymbolFlags::SHARED)
        | flag_if(is_new_shared, SymbolFlags::NEW_SHARED)
        | flag_if(is_new_global_shared, SymbolFlags::NEW_GLOBAL_SHARED)
}

fn wrap_extent(ty: ResolvedType, extent: Option<u32>) -> ResolvedType {
    match extent {
        None => ty,
        Some(n) => ResolvedType::Array {
            element: Box::new(ty),
            // ABL extent `0` is dynamic; represent as `None`.
            extent: if n == 0 { None } else { Some(n) },
        },
    }
}

/// Case-fold and intern an identifier. Shared with [`crate::index`], whose
/// keys must fold identically to the symbol table's names.
pub(crate) fn fold_atom(s: &str) -> OxablAtom {
    let bytes = s.as_bytes();
    const INLINE: usize = 64;
    if bytes.len() <= INLINE {
        let mut buf = [0u8; INLINE];
        for (i, &b) in bytes.iter().enumerate() {
            buf[i] = b.to_ascii_lowercase();
        }
        // SAFETY: ASCII lowercasing preserves UTF-8; source was UTF-8.
        let folded = unsafe { std::str::from_utf8_unchecked(&buf[..bytes.len()]) };
        OxablAtom::from(folded)
    } else {
        OxablAtom::from(s.to_ascii_lowercase())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ClassDescriptor, IndexRevision, NamespaceId, SymbolKind, WorkspaceIndex};
    use oxabl_ast::{
        AccessModifier, BooleanLiteral, BufferTarget, DataType, Expression, ExpressionKind,
        Identifier, IntegerLiteral, Literal, LockType, ParameterDirection, ParameterType, Span,
        Statement, StatementKind, TempTableField, TypeSource, XmlSerializeOptions,
    };
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use smallvec::SmallVec;

    // ---- Helpers ---------------------------------------------------------

    fn id(name: &str) -> Identifier {
        Identifier {
            span: Span {
                start: 0,
                end: name.len() as u32,
            },
            name: name.into(),
        }
    }

    fn stmt(kind: StatementKind) -> Statement {
        Statement::new(kind)
    }

    fn ctx<'a>(src: &'a str, schema: &'a Schema) -> AnalysisContext<'a> {
        AnalysisContext::new(FileId::UNKNOWN, src, schema)
    }

    fn run(stmts: Vec<Statement>) -> (ScopeTree, SymbolTable, Vec<Diagnostic>) {
        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree, symbols, diags, _rev) = declare_pass(&stmts, &ctx);
        (tree, symbols, diags)
    }

    fn find_symbol<'t>(
        tree: &'t ScopeTree,
        symbols: &'t SymbolTable,
        ns: NamespaceId,
        name: &str,
    ) -> Option<&'t Symbol> {
        let sym = tree.resolve(ScopeId::ROOT, ns, &fold_atom(name))?;
        Some(symbols.get(sym))
    }

    fn var_stmt(name: &str, ty: DataType) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn var_stmt_extent(name: &str, ty: DataType, extent: u32) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: Some(extent),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn param_stmt(name: &str, dir: ParameterDirection, ty: DataType) -> Statement {
        stmt(StatementKind::DefineParameter {
            direction: dir,
            param_type: ParameterType::Variable {
                name: id(name),
                type_source: TypeSource::Explicit(ty),
                no_undo: false,
            },
        })
    }

    // ---- Tests -----------------------------------------------------------

    #[test]
    fn declares_simple_variable() {
        let (tree, symbols, diags) = run(vec![var_stmt("x", DataType::Integer)]);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "x").unwrap();
        assert_eq!(s.kind, SymbolKind::Variable);
        assert_eq!(
            s.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
    }

    #[test]
    fn variable_decl_is_case_insensitive() {
        let (tree, symbols, _) = run(vec![var_stmt("MyVar", DataType::Character)]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "myvar").is_some());
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "MYVAR").is_some());
    }

    #[test]
    fn variable_with_extent_wraps_in_array_type() {
        let (tree, symbols, _) = run(vec![var_stmt_extent("arr", DataType::Integer, 5)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "arr").unwrap();
        match s.data_type.as_ref().unwrap() {
            ResolvedType::Array { extent, .. } => assert_eq!(*extent, Some(5)),
            other => panic!("expected Array type, got {other:?}"),
        }
    }

    #[test]
    fn variable_with_dynamic_extent_carries_none() {
        let (tree, symbols, _) = run(vec![var_stmt_extent("arr", DataType::Integer, 0)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "arr").unwrap();
        match s.data_type.as_ref().unwrap() {
            ResolvedType::Array { extent, .. } => assert_eq!(*extent, None),
            other => panic!("expected Array type, got {other:?}"),
        }
    }

    #[test]
    fn variable_like_leaves_type_none() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::VariableDeclaration {
            name: id("x"),
            type_source: TypeSource::Like {
                source: id("Customer.CustNum"),
            },
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "x").unwrap();
        assert!(s.data_type.is_none());
    }

    #[test]
    fn duplicate_variable_in_same_scope_emits_sem0001() {
        let (_tree, _symbols, diags) = run(vec![
            var_stmt("x", DataType::Integer),
            var_stmt("x", DataType::Character),
        ]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, diagnostics::SEM0001);
    }

    #[test]
    fn blob_as_local_emits_sem0003() {
        let (_t, _s, diags) = run(vec![var_stmt("b", DataType::Blob)]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, diagnostics::SEM0003);
    }

    #[test]
    fn clob_as_local_emits_sem0003() {
        let (_t, _s, diags) = run(vec![var_stmt("c", DataType::Clob)]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, diagnostics::SEM0003);
    }

    #[test]
    fn input_parameter_declared_in_values() {
        let (tree, symbols, _) = run(vec![param_stmt(
            "p",
            ParameterDirection::Input,
            DataType::Integer,
        )]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "p").unwrap();
        assert_eq!(s.kind, SymbolKind::Parameter);
        assert!(s.flags.contains(SymbolFlags::PARAM_INPUT));
    }

    #[test]
    fn output_parameter_carries_output_flag() {
        let (tree, symbols, _) = run(vec![param_stmt(
            "o",
            ParameterDirection::Output,
            DataType::Integer,
        )]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "o").unwrap();
        assert!(s.flags.contains(SymbolFlags::PARAM_OUTPUT));
    }

    #[test]
    fn input_output_parameter_carries_inout_flag() {
        let (tree, symbols, _) = run(vec![param_stmt(
            "io",
            ParameterDirection::InputOutput,
            DataType::Integer,
        )]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "io").unwrap();
        assert!(s.flags.contains(SymbolFlags::PARAM_INPUT_OUT));
    }

    #[test]
    fn buffer_parameter_lives_in_buffers_namespace() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Buffer {
                name: id("buf"),
                target: id("Customer"),
            },
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Buffers, "buf").unwrap();
        assert_eq!(s.kind, SymbolKind::Buffer);
    }

    #[test]
    fn define_buffer_lives_in_buffers() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineBuffer {
            name: id("bCust"),
            target: BufferTarget::Table(id("Customer")),
            preselect: false,
            label: None,
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Buffers, "bCust").unwrap();
        assert_eq!(s.kind, SymbolKind::Buffer);
    }

    #[test]
    fn define_temp_table_declares_table_and_fields() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineTempTable {
            name: id("ttCust"),
            no_undo: false,
            like_table: None,
            validate: false,
            use_indexes: vec![],
            fields: vec![
                TempTableField {
                    name: id("CustNum"),
                    type_source: TypeSource::Explicit(DataType::Integer),
                    validate: false,
                    initial_value: None,
                    extent: None,
                },
                TempTableField {
                    name: id("Name"),
                    type_source: TypeSource::Explicit(DataType::Character),
                    validate: false,
                    initial_value: None,
                    extent: None,
                },
            ],
            indexes: vec![],
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Buffers, "ttCust").is_some());
        // Fields live in the temp-table's own child scope, not program scope.
        let (_, cust_num) = symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Field && s.name == fold_atom("custnum"))
            .expect("field CustNum should be declared");
        assert_eq!(cust_num.kind, SymbolKind::Field);
        assert_eq!(
            tree.get(cust_num.declared_in).kind,
            ScopeKind::TempTable,
            "temp-table fields must bind in a TempTable scope, not program scope"
        );
        assert_ne!(
            cust_num.declared_in,
            ScopeId::ROOT,
            "temp-table fields must not bind in the file/program scope"
        );
    }

    #[test]
    fn same_field_name_in_two_temp_tables_no_sem0001() {
        // Two temp-tables each with a `line-code` field: identical field
        // names across different temp-tables must not collide (#106).
        let field = |n: &str| TempTableField {
            name: id(n),
            type_source: TypeSource::Explicit(DataType::Character),
            validate: false,
            initial_value: None,
            extent: None,
        };
        let tt = |name: &str, fname: &str| {
            stmt(StatementKind::DefineTempTable {
                name: id(name),
                no_undo: true,
                like_table: None,
                validate: false,
                use_indexes: vec![],
                fields: vec![field(fname)],
                indexes: vec![],
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            })
        };
        let (tree, symbols, diags) = run(vec![
            tt("tt-order", "line-code"),
            tt("tt-line", "line-code"),
        ]);
        assert!(
            diags.iter().all(|d| d.code.0 != diagnostics::SEM0001),
            "same field name in different temp-tables must not SEM0001: {diags:?}"
        );
        // Both fields exist, each in its own distinct TempTable scope.
        let field_scopes: Vec<_> = symbols
            .iter()
            .filter(|(_, s)| s.kind == SymbolKind::Field && s.name == fold_atom("line-code"))
            .map(|(_, s)| {
                assert_eq!(tree.get(s.declared_in).kind, ScopeKind::TempTable);
                s.declared_in
            })
            .collect();
        assert_eq!(field_scopes.len(), 2, "both fields should be declared");
        assert_ne!(
            field_scopes[0], field_scopes[1],
            "each temp-table gets its own field scope"
        );
    }

    #[test]
    fn duplicate_field_in_same_temp_table_still_sem0001() {
        // A field declared twice in the SAME temp-table is a genuine
        // duplicate and must still raise SEM0001.
        let field = |n: &str| TempTableField {
            name: id(n),
            type_source: TypeSource::Explicit(DataType::Character),
            validate: false,
            initial_value: None,
            extent: None,
        };
        let (_, _, diags) = run(vec![stmt(StatementKind::DefineTempTable {
            name: id("tt-order"),
            no_undo: true,
            like_table: None,
            validate: false,
            use_indexes: vec![],
            fields: vec![field("line-code"), field("line-code")],
            indexes: vec![],
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })]);
        assert!(
            diags.iter().any(|d| d.code.0 == diagnostics::SEM0001),
            "duplicate field in one temp-table must SEM0001: {diags:?}"
        );
    }

    #[test]
    fn define_stream_lives_in_streams() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineStream { name: id("s1") })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Streams, "s1").unwrap();
        assert_eq!(s.kind, SymbolKind::Stream);
    }

    #[test]
    fn define_frame_lives_in_frames() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineFrame {
            name: id("f1"),
            raw_span: Span { start: 0, end: 0 },
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Frames, "f1").unwrap();
        assert_eq!(s.kind, SymbolKind::Frame);
    }

    #[test]
    fn define_event_lives_in_events() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineEvent {
            access: AccessModifier::Public,
            is_static: false,
            is_abstract: false,
            name: id("myEvent"),
            parameters: vec![],
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Events, "myEvent").unwrap();
        assert_eq!(s.kind, SymbolKind::Event);
    }

    #[test]
    fn procedure_declares_name_and_opens_scope() {
        let (tree, symbols, diags) = run(vec![stmt(StatementKind::Procedure {
            name: id("doThing"),
            body: vec![var_stmt("local", DataType::Integer)],
        })]);
        assert!(diags.is_empty());
        let proc = find_symbol(&tree, &symbols, NamespaceId::Procedures, "doThing").unwrap();
        assert_eq!(proc.kind, SymbolKind::Procedure);
        // `local` must live inside the procedure scope, not the file scope.
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "local").is_none());
        // Procedure scope exists.
        let proc_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Procedure)
            .expect("procedure scope");
        assert_eq!(proc_scope.1.owner_node, stmt(StatementKind::Empty).id);
    }

    #[test]
    fn function_declares_return_type() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Function {
            name: id("sum"),
            return_type: DataType::Integer,
            body: vec![],
        })]);
        let f = find_symbol(&tree, &symbols, NamespaceId::Functions, "sum").unwrap();
        assert_eq!(
            f.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
    }

    #[test]
    fn class_declares_type_and_opens_scope() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("Foo"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![],
        })]);
        let c = find_symbol(&tree, &symbols, NamespaceId::Types, "Foo").unwrap();
        assert_eq!(c.kind, SymbolKind::Class);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::Class));
    }

    #[test]
    fn abstract_final_class_flags_set() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("Base"),
            inherits: None,
            implements: vec![],
            is_abstract: true,
            is_final: true,
            body: vec![],
        })]);
        let c = find_symbol(&tree, &symbols, NamespaceId::Types, "Base").unwrap();
        assert!(c.flags.contains(SymbolFlags::ABSTRACT));
        assert!(c.flags.contains(SymbolFlags::FINAL));
    }

    #[test]
    fn interface_declares_type() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Interface {
            name: id("IFoo"),
            inherits: vec![],
            body: vec![],
        })]);
        let i = find_symbol(&tree, &symbols, NamespaceId::Types, "IFoo").unwrap();
        assert_eq!(i.kind, SymbolKind::Interface);
    }

    #[test]
    fn method_in_class_opens_method_scope() {
        let (tree, _symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: false,
                is_override: false,
                return_type: None,
                name: id("doIt"),
                parameters: vec![],
                body: vec![],
            })],
        })]);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::Method));
    }

    #[test]
    fn constructor_opens_constructor_scope() {
        let (tree, _symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Constructor {
                access: AccessModifier::Public,
                parameters: vec![],
                body: vec![],
            })],
        })]);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::Constructor));
    }

    #[test]
    fn destructor_opens_destructor_scope() {
        let (tree, _symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Destructor { body: vec![] })],
        })]);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::Destructor));
    }

    #[test]
    fn property_declares_symbol_and_accessor_scopes() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Property {
            access: AccessModifier::Public,
            is_static: false,
            name: id("Name"),
            data_type: DataType::Character,
            no_undo: false,
            get_body: Some(vec![]),
            set_body: Some(vec![]),
            set_parameters: vec![],
        })]);
        let p = find_symbol(&tree, &symbols, NamespaceId::Values, "Name").unwrap();
        assert_eq!(p.kind, SymbolKind::Property);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::PropertyGet));
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::PropertySet));
    }

    #[test]
    fn property_auto_getter_only_still_opens_get_scope() {
        let (tree, _symbols, _) = run(vec![stmt(StatementKind::Property {
            access: AccessModifier::Public,
            is_static: false,
            name: id("X"),
            data_type: DataType::Integer,
            no_undo: false,
            get_body: Some(vec![]),
            set_body: None,
            set_parameters: vec![],
        })]);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::PropertyGet));
        assert!(!tree.iter().any(|(_, s)| s.kind == ScopeKind::PropertySet));
    }

    #[test]
    fn catch_opens_scope_and_declares_error_var() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Catch {
            error_var: id("e"),
            error_type: "Progress.Lang.Error".into(),
            body: vec![],
        })]);
        let catch_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Catch)
            .expect("catch scope");
        let e = catch_scope.1.get_in(NamespaceId::Values, &fold_atom("e"));
        assert!(e.is_some());
        assert_eq!(symbols.get(e.unwrap()).kind, SymbolKind::Variable);
    }

    #[test]
    fn do_loop_counter_lives_in_block_scope() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Do {
            loop_var: Some(id("i")),
            from: Some(Expression::new(ExpressionKind::Literal(Literal::Integer(
                IntegerLiteral {
                    span: Span { start: 0, end: 1 },
                    value: 1,
                },
            )))),
            to: Some(Expression::new(ExpressionKind::Literal(Literal::Integer(
                IntegerLiteral {
                    span: Span { start: 0, end: 2 },
                    value: 10,
                },
            )))),
            by: None,
            while_condition: None,
            transaction: false,
            body: vec![],
        })]);
        // Counter must not leak into file scope.
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "i").is_none());
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        let counter = block.get_in(NamespaceId::Values, &fold_atom("i")).unwrap();
        assert_eq!(symbols.get(counter).kind, SymbolKind::Variable);
    }

    #[test]
    fn do_loop_counter_binds_existing_variable_and_counts_as_use() {
        // `DEF VAR i` then `DO i = 1 TO 10:` — ABL reuses the already-defined
        // variable as the counter. The loop must count as a use (no LINT0002)
        // and must NOT mint a shadow counter in the block scope, which would
        // otherwise leave the real `i` looking unused.
        let stmts = vec![
            var_stmt_n("i", DataType::Integer),
            stmt_n(StatementKind::Do {
                loop_var: Some(id("i")),
                from: Some(Expression::new(ExpressionKind::Literal(Literal::Integer(
                    IntegerLiteral {
                        span: Span { start: 0, end: 1 },
                        value: 1,
                    },
                )))),
                to: Some(Expression::new(ExpressionKind::Literal(Literal::Integer(
                    IntegerLiteral {
                        span: Span { start: 0, end: 2 },
                        value: 10,
                    },
                )))),
                by: None,
                while_condition: None,
                transaction: false,
                body: vec![],
            }),
        ];
        let (tree, symbols, _refs, _types) = run_full(&stmts);

        // Exactly one `i` Variable symbol (the outer def) — no block shadow.
        let i_syms: Vec<_> = symbols
            .iter()
            .filter(|(_, s)| s.name == fold_atom("i") && s.kind == SymbolKind::Variable)
            .collect();
        assert_eq!(i_syms.len(), 1, "expected no shadow counter: {i_syms:?}");
        // The loop counts as a use of the counter.
        assert!(
            symbols.get(i_syms[0].0).read_count > 0,
            "DO counter should read the loop variable"
        );

        // The block scope holds no `i` binding of its own.
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        assert!(block.get_in(NamespaceId::Values, &fold_atom("i")).is_none());
    }

    #[test]
    fn for_each_introduces_implicit_buffer_in_block_scope() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::ForEach {
            buffer: id("Customer"),
            of_relation: None,
            where_clause: None,
            lock_type: LockType::NoLock,
            body: vec![],
        })]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Buffers, "Customer").is_none());
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        assert!(
            block
                .get_in(NamespaceId::Buffers, &fold_atom("customer"))
                .is_some()
        );
    }

    #[test]
    fn block_variable_is_routine_scoped_not_block_local() {
        let (tree, _symbols, diags) = run(vec![
            var_stmt("x", DataType::Integer),
            stmt(StatementKind::Do {
                loop_var: None,
                from: None,
                to: None,
                by: None,
                while_condition: None,
                transaction: false,
                body: vec![var_stmt("x", DataType::Character)],
            }),
        ]);
        // ABL scopes `DEFINE VARIABLE` to the routine, not the block. The
        // block-nested `x` therefore hoists to the file scope and collides
        // with the file-level `x` — a genuine duplicate, matching ABL's
        // "x already defined" compile error.
        assert!(
            diags.iter().any(|d| d.code.0 == diagnostics::SEM0001),
            "block-nested DEFINE VARIABLE must duplicate the routine-scope name: {diags:?}"
        );
        // The name binds at the routine (file) scope...
        assert!(
            tree.get(ScopeId::ROOT)
                .get_in(NamespaceId::Values, &fold_atom("x"))
                .is_some()
        );
        // ...and NOT in the block scope itself (it was hoisted out).
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        assert!(
            block.get_in(NamespaceId::Values, &fold_atom("x")).is_none(),
            "hoisted variable must not bind in the block scope"
        );
    }

    #[test]
    fn same_name_in_separate_routines_is_not_duplicate() {
        // A genuine routine boundary (an internal procedure) is a real scope:
        // the same block-nested name in two different procedures must not
        // collide.
        let a_block = stmt(StatementKind::Do {
            loop_var: None,
            from: None,
            to: None,
            by: None,
            while_condition: None,
            transaction: false,
            body: vec![var_stmt("x", DataType::Integer)],
        });
        let b_block = stmt(StatementKind::Do {
            loop_var: None,
            from: None,
            to: None,
            by: None,
            while_condition: None,
            transaction: false,
            body: vec![var_stmt("x", DataType::Integer)],
        });
        let (_tree, _symbols, diags) = run(vec![
            stmt(StatementKind::Procedure {
                name: id("a"),
                body: vec![a_block],
            }),
            stmt(StatementKind::Procedure {
                name: id("b"),
                body: vec![b_block],
            }),
        ]);
        assert!(
            !diags.iter().any(|d| d.code.0 == diagnostics::SEM0001),
            "same name in separate routines must not collide: {diags:?}"
        );
    }

    #[test]
    fn multiple_params_ordered_in_scope() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Function {
            name: id("calc"),
            return_type: DataType::Integer,
            body: vec![
                param_stmt("a", ParameterDirection::Input, DataType::Integer),
                param_stmt("b", ParameterDirection::Input, DataType::Integer),
            ],
        })]);
        let fn_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Function)
            .unwrap()
            .1;
        assert!(
            fn_scope
                .get_in(NamespaceId::Values, &fold_atom("a"))
                .is_some()
        );
        assert!(
            fn_scope
                .get_in(NamespaceId::Values, &fold_atom("b"))
                .is_some()
        );
        // Function symbol in procedures namespace.
        assert!(find_symbol(&tree, &symbols, NamespaceId::Functions, "calc").is_some());
    }

    #[test]
    fn duplicate_procedure_emits_sem0001() {
        let (_t, _s, diags) = run(vec![
            stmt(StatementKind::Procedure {
                name: id("p"),
                body: vec![],
            }),
            stmt(StatementKind::Procedure {
                name: id("p"),
                body: vec![],
            }),
        ]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, diagnostics::SEM0001);
    }

    // ---- #69 FUNCTION prototype + definition reconciliation --------------

    #[test]
    fn function_forward_then_definition_no_sem0001() {
        let def = stmt(StatementKind::Function {
            name: id("getVal"),
            return_type: DataType::Character,
            body: vec![param_stmt(
                "sValue",
                ParameterDirection::Input,
                DataType::Character,
            )],
        });
        let def_id = def.id;
        let (tree, symbols, diags) = run(vec![
            stmt(StatementKind::Function {
                name: id("getVal"),
                return_type: DataType::Character,
                body: vec![], // FORWARD prototype
            }),
            def,
        ]);
        assert!(
            diags.iter().all(|d| d.code.0 != diagnostics::SEM0001),
            "FORWARD + definition must not SEM0001: {diags:?}"
        );
        let s = find_symbol(&tree, &symbols, NamespaceId::Functions, "getval").unwrap();
        assert_eq!(s.declaration, def_id, "symbol should point at definition");
        assert!(
            !s.flags.contains(SymbolFlags::PROTOTYPE),
            "PROTOTYPE flag cleared after merge"
        );
    }

    #[test]
    fn function_in_super_then_definition_no_sem0001() {
        let (_t, _s, diags) = run(vec![
            stmt(StatementKind::Function {
                name: id("getVal"),
                return_type: DataType::Character,
                body: vec![], // IN SUPER prototype
            }),
            stmt(StatementKind::Function {
                name: id("getVal"),
                return_type: DataType::Character,
                body: vec![param_stmt(
                    "s",
                    ParameterDirection::Input,
                    DataType::Character,
                )],
            }),
        ]);
        assert!(
            diags.iter().all(|d| d.code.0 != diagnostics::SEM0001),
            "IN SUPER + definition must not SEM0001: {diags:?}"
        );
    }

    #[test]
    fn function_prototype_only_no_sem0001() {
        let (tree, symbols, diags) = run(vec![stmt(StatementKind::Function {
            name: id("get-value"),
            return_type: DataType::Character,
            body: vec![], // IN hMisc — external only
        })]);
        assert!(diags.is_empty(), "{diags:?}");
        let s = find_symbol(&tree, &symbols, NamespaceId::Functions, "get-value").unwrap();
        assert!(s.flags.contains(SymbolFlags::PROTOTYPE));
    }

    #[test]
    fn function_map_to_prototype_only_no_sem0001() {
        let (tree, symbols, diags) = run(vec![stmt(StatementKind::Function {
            name: id("mappedFn"),
            return_type: DataType::Integer,
            body: vec![], // MAP TO … IN handle
        })]);
        assert!(diags.is_empty(), "{diags:?}");
        let s = find_symbol(&tree, &symbols, NamespaceId::Functions, "mappedfn").unwrap();
        assert!(s.flags.contains(SymbolFlags::PROTOTYPE));
    }

    #[test]
    fn function_two_full_definitions_still_sem0001() {
        let (_t, _s, diags) = run(vec![
            stmt(StatementKind::Function {
                name: id("f"),
                return_type: DataType::Integer,
                body: vec![var_stmt("x", DataType::Integer)],
            }),
            stmt(StatementKind::Function {
                name: id("f"),
                return_type: DataType::Integer,
                body: vec![var_stmt("y", DataType::Integer)],
            }),
        ]);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].code.0, diagnostics::SEM0001);
    }

    #[test]
    fn function_two_prototypes_no_sem0001() {
        let proto0 = stmt(StatementKind::Function {
            name: id("f"),
            return_type: DataType::Integer,
            body: vec![],
        });
        let proto0_id = proto0.id;
        let (tree, symbols, diags) = run(vec![
            proto0,
            stmt(StatementKind::Function {
                name: id("f"),
                return_type: DataType::Integer,
                body: vec![],
            }),
        ]);
        assert!(diags.is_empty(), "{diags:?}");
        let s = find_symbol(&tree, &symbols, NamespaceId::Functions, "f").unwrap();
        assert!(
            s.flags.contains(SymbolFlags::PROTOTYPE),
            "PROTOTYPE flag kept when no definition merges"
        );
        assert_eq!(
            s.declaration, proto0_id,
            "symbol keeps first prototype declaration"
        );
    }

    #[test]
    fn function_definition_then_forward_no_sem0001() {
        let def = stmt(StatementKind::Function {
            name: id("f"),
            return_type: DataType::Integer,
            body: vec![var_stmt("x", DataType::Integer)],
        });
        let def_id = def.id;
        let (tree, symbols, diags) = run(vec![
            def,
            stmt(StatementKind::Function {
                name: id("f"),
                return_type: DataType::Integer,
                body: vec![], // trailing FORWARD
            }),
        ]);
        assert!(
            diags.iter().all(|d| d.code.0 != diagnostics::SEM0001),
            "definition then FORWARD must not SEM0001: {diags:?}"
        );
        let s = find_symbol(&tree, &symbols, NamespaceId::Functions, "f").unwrap();
        assert_eq!(s.declaration, def_id);
        assert!(!s.flags.contains(SymbolFlags::PROTOTYPE));
    }

    #[test]
    fn function_forward_and_in_super_then_definition_no_sem0001() {
        // WebSpeed/ADM common pattern: FORWARD + IN SUPER + definition.
        let def = stmt(StatementKind::Function {
            name: id("getVal"),
            return_type: DataType::Character,
            body: vec![param_stmt(
                "s",
                ParameterDirection::Input,
                DataType::Character,
            )],
        });
        let def_id = def.id;
        let (tree, symbols, diags) = run(vec![
            stmt(StatementKind::Function {
                name: id("getVal"),
                return_type: DataType::Character,
                body: vec![], // FORWARD prototype
            }),
            stmt(StatementKind::Function {
                name: id("getVal"),
                return_type: DataType::Character,
                body: vec![], // IN SUPER prototype
            }),
            def,
        ]);
        assert!(
            diags.iter().all(|d| d.code.0 != diagnostics::SEM0001),
            "FORWARD + IN SUPER + definition must not SEM0001: {diags:?}"
        );
        let s = find_symbol(&tree, &symbols, NamespaceId::Functions, "getval").unwrap();
        assert_eq!(s.declaration, def_id, "symbol should point at definition");
        assert!(
            !s.flags.contains(SymbolFlags::PROTOTYPE),
            "PROTOTYPE flag cleared after definition merges"
        );
    }

    #[test]
    fn two_methods_same_name_still_sem0001() {
        let (_t, _s, diags) = run(vec![stmt(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![
                stmt(StatementKind::Method {
                    access: AccessModifier::Public,
                    is_static: false,
                    is_abstract: false,
                    is_override: false,
                    return_type: None,
                    name: id("m"),
                    parameters: vec![],
                    body: vec![],
                }),
                stmt(StatementKind::Method {
                    access: AccessModifier::Public,
                    is_static: false,
                    is_abstract: false,
                    is_override: false,
                    return_type: None,
                    name: id("m"),
                    parameters: vec![],
                    body: vec![],
                }),
            ],
        })]);
        assert!(
            diags.iter().any(|d| d.code.0 == diagnostics::SEM0001),
            "two same-name methods must still SEM0001: {diags:?}"
        );
    }

    #[test]
    fn builtins_seeded_into_root_scope() {
        let (tree, symbols, _) = run(vec![]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "session").unwrap();
        assert_eq!(s.kind, SymbolKind::BuiltIn);
    }

    #[test]
    fn builtin_name_can_be_redeclared_without_sem0001() {
        // `session` is seeded as a builtin; shadowing it with a user
        // variable is legal in ABL and must not trigger SEM0001.
        let (_tree, _symbols, diags) = run(vec![var_stmt("session", DataType::Integer)]);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    }

    #[test]
    fn dataset_declared_with_shared_flag() {
        let (_tree, symbols, _) = run(vec![stmt(StatementKind::DefineDataset {
            name: id("dsFoo"),
            access: None,
            is_static: false,
            is_new_shared: false,
            is_shared: true,
            serializable: false,
            non_serializable: false,
            xml_options: XmlSerializeOptions::default(),
            reference_only: false,
            buffers: vec![],
            data_relations: vec![],
            parent_id_relations: vec![],
            is_new_global_shared: false,
        })]);
        let (_, s) = symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Dataset)
            .unwrap();
        assert!(s.flags.contains(SymbolFlags::SHARED));
    }

    // ── SHARED / NEW [GLOBAL] SHARED symbol flags (ast-invariants §12) ──

    fn shared_var(
        name: &str,
        is_shared: bool,
        is_new_shared: bool,
        is_new_global: bool,
    ) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(DataType::Integer),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared,
            is_shared,
            is_new_global_shared: is_new_global,
        })
    }

    #[test]
    fn shared_variable_has_shared_flag() {
        let (tree, symbols, _) = run(vec![shared_var("x", true, false, false)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "x").unwrap();
        assert!(s.flags.contains(SymbolFlags::SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_GLOBAL_SHARED));
    }

    #[test]
    fn new_shared_variable_has_new_shared_flag() {
        let (tree, symbols, _) = run(vec![shared_var("y", false, true, false)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "y").unwrap();
        assert!(s.flags.contains(SymbolFlags::NEW_SHARED));
        assert!(!s.flags.contains(SymbolFlags::SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_GLOBAL_SHARED));
    }

    #[test]
    fn new_global_shared_variable_has_new_global_shared_flag() {
        let (tree, symbols, _) = run(vec![shared_var("z", false, false, true)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "z").unwrap();
        assert!(s.flags.contains(SymbolFlags::NEW_GLOBAL_SHARED));
        assert!(!s.flags.contains(SymbolFlags::SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_SHARED));
    }

    #[test]
    fn plain_variable_has_empty_flags() {
        let (tree, symbols, _) = run(vec![shared_var("n", false, false, false)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "n").unwrap();
        assert!(!s.flags.contains(SymbolFlags::SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_GLOBAL_SHARED));
    }

    #[test]
    fn shared_temp_table_has_shared_flag() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineTempTable {
            name: id("tt"),
            no_undo: false,
            like_table: None,
            validate: false,
            use_indexes: vec![],
            fields: vec![],
            indexes: vec![],
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: true,
            is_new_global_shared: false,
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Buffers, "tt").unwrap();
        assert!(s.flags.contains(SymbolFlags::SHARED));
    }

    #[test]
    fn shared_buffer_has_new_shared_flag() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineBuffer {
            name: id("b"),
            target: BufferTarget::Table(id("customer")),
            preselect: false,
            label: None,
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: true,
            is_shared: false,
            is_new_global_shared: false,
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Buffers, "b").unwrap();
        assert!(s.flags.contains(SymbolFlags::NEW_SHARED));
        assert!(!s.flags.contains(SymbolFlags::SHARED));
    }

    #[test]
    fn new_global_shared_dataset_has_new_global_shared_flag() {
        // Regression guard for the DefineDataset lockstep retrofit: the GLOBAL
        // form must set NEW_GLOBAL_SHARED and NOT collapse into NEW_SHARED.
        let (_tree, symbols, _) = run(vec![stmt(StatementKind::DefineDataset {
            name: id("dsGlobal"),
            access: None,
            is_static: false,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: true,
            serializable: false,
            non_serializable: false,
            xml_options: XmlSerializeOptions::default(),
            reference_only: false,
            buffers: vec![],
            data_relations: vec![],
            parent_id_relations: vec![],
        })]);
        let (_, s) = symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Dataset)
            .unwrap();
        assert!(s.flags.contains(SymbolFlags::NEW_GLOBAL_SHARED));
        assert!(!s.flags.contains(SymbolFlags::NEW_SHARED));
    }

    #[test]
    fn data_source_declared() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineDataSource {
            name: id("dsCust"),
            access: None,
            is_static: false,
            query: None,
            source_buffers: vec![],
        })]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "dsCust").unwrap();
        assert_eq!(s.kind, SymbolKind::DataSource);
    }

    #[test]
    fn assign_statement_not_treated_as_declaration() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Assign {
            assignments: SmallVec::new(),
        })]);
        // Only builtins should be present in root; no symbols introduced.
        let user_count = symbols
            .iter()
            .filter(|(_, s)| s.kind != SymbolKind::BuiltIn)
            .count();
        assert_eq!(user_count, 0);
        assert_eq!(tree.len(), 1);
    }

    #[test]
    fn preproc_if_walks_both_branches() {
        use oxabl_ast::PreprocIf;
        let stmts = vec![stmt(StatementKind::PreprocIf(PreprocIf {
            condition: Expression::new(ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
                span: Span { start: 0, end: 4 },
                value: true,
            }))),
            then_branch: vec![var_stmt("x", DataType::Integer)],
            elseif_branches: vec![],
            else_branch: Some(vec![var_stmt("y", DataType::Character)]),
        }))];
        let (tree, symbols, _) = run(stmts);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "x").is_some());
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "y").is_some());
    }

    #[test]
    fn on_ui_event_with_block_opens_trigger_scope() {
        use oxabl_ast::{OnAction, OnEventClause, OnKind};
        let (tree, _s, _d) = run(vec![stmt(StatementKind::On {
            kind: OnKind::UiEvent {
                clauses: vec![OnEventClause {
                    events: vec![id("CHOOSE")],
                    widgets: vec![],
                }],
                anywhere: false,
                action: OnAction::Block(Box::new(stmt(StatementKind::Block(vec![])))),
            },
        })]);
        assert!(tree.iter().any(|(_, s)| s.kind == ScopeKind::Trigger));
    }

    #[test]
    fn trigger_procedure_opens_implicit_scope() {
        use oxabl_ast::{DbTriggerEvent, TriggerReferencing};
        let (tree, _s, _d) = run(vec![stmt(StatementKind::TriggerProcedure {
            event: DbTriggerEvent::Write,
            target: id("Customer"),
            referencing: TriggerReferencing::default(),
            new_value: None,
            old_value_param: None,
        })]);
        assert!(
            tree.iter()
                .any(|(_, s)| s.kind == ScopeKind::TriggerProcedure)
        );
    }

    #[test]
    fn if_else_bodies_do_not_open_new_scopes() {
        // Per plan: control-flow-only statements don't introduce lexical
        // scopes. Declarations inside an IF/THEN body land in the
        // enclosing scope.
        let (tree, symbols, _) = run(vec![stmt(StatementKind::If {
            condition: Expression::new(ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
                span: Span { start: 0, end: 4 },
                value: true,
            }))),
            then_branch: Box::new(var_stmt("in_then", DataType::Integer)),
            else_branch: Some(Box::new(var_stmt("in_else", DataType::Integer))),
        })]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "in_then").is_some());
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "in_else").is_some());
        // Root scope + any scopes we *did* create (none from IF).
        assert_eq!(tree.len(), 1);
    }

    #[test]
    fn function_symbol_and_body_scope_distinct() {
        let (tree, _s, _d) = run(vec![stmt(StatementKind::Function {
            name: id("f"),
            return_type: DataType::Integer,
            body: vec![var_stmt("local", DataType::Integer)],
        })]);
        // Function scope exists; `local` must be inside it.
        let fn_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Function)
            .unwrap()
            .1;
        assert!(
            fn_scope
                .get_in(NamespaceId::Values, &fold_atom("local"))
                .is_some()
        );
    }

    #[test]
    fn two_functions_distinct_symbols() {
        let (tree, symbols, _) = run(vec![
            stmt(StatementKind::Function {
                name: id("f1"),
                return_type: DataType::Integer,
                body: vec![],
            }),
            stmt(StatementKind::Function {
                name: id("f2"),
                return_type: DataType::Integer,
                body: vec![],
            }),
        ]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Functions, "f1").is_some());
        assert!(find_symbol(&tree, &symbols, NamespaceId::Functions, "f2").is_some());
    }

    #[test]
    fn variable_no_undo_flag_not_tracked_on_symbol_yet() {
        // NO_UNDO flag on variables is not currently wired from the AST —
        // this test pins the current behaviour so a future wiring doesn't
        // silently break. Parameters do carry NO_UNDO already.
        let (tree, symbols, _) = run(vec![var_stmt("x", DataType::Integer)]);
        let s = find_symbol(&tree, &symbols, NamespaceId::Values, "x").unwrap();
        assert!(!s.flags.contains(SymbolFlags::NO_UNDO));
    }

    #[test]
    fn nested_classes_open_nested_class_scopes() {
        let (tree, _s, _d) = run(vec![stmt(StatementKind::Class {
            name: id("Outer"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: false,
                is_override: false,
                return_type: None,
                name: id("m"),
                parameters: vec![param_stmt(
                    "arg",
                    ParameterDirection::Input,
                    DataType::Integer,
                )],
                body: vec![var_stmt("local", DataType::Character)],
            })],
        })]);
        // Method scope holds both the parameter and the local.
        let method = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Method)
            .unwrap()
            .1;
        assert!(
            method
                .get_in(NamespaceId::Values, &fold_atom("arg"))
                .is_some()
        );
        assert!(
            method
                .get_in(NamespaceId::Values, &fold_atom("local"))
                .is_some()
        );
    }

    #[test]
    fn method_abstract_flag_tracked() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("A"),
            inherits: None,
            implements: vec![],
            is_abstract: true,
            is_final: false,
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: true,
                is_override: false,
                return_type: None,
                name: id("m"),
                parameters: vec![],
                body: vec![],
            })],
        })]);
        let class_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Class)
            .unwrap();
        let m = class_scope
            .1
            .get_in(NamespaceId::Functions, &fold_atom("m"))
            .unwrap();
        assert!(symbols.get(m).flags.contains(SymbolFlags::ABSTRACT));
    }

    #[test]
    fn method_override_flag_tracked() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("A"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Method {
                access: AccessModifier::Public,
                is_static: false,
                is_abstract: false,
                is_override: true,
                return_type: None,
                name: id("m"),
                parameters: vec![],
                body: vec![],
            })],
        })]);
        let class_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Class)
            .unwrap();
        let m = class_scope
            .1
            .get_in(NamespaceId::Functions, &fold_atom("m"))
            .unwrap();
        assert!(symbols.get(m).flags.contains(SymbolFlags::OVERRIDE));
    }

    #[test]
    fn static_class_member_carries_static_flag() {
        let (tree, symbols, _) = run(vec![stmt(StatementKind::Class {
            name: id("U"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![stmt(StatementKind::Property {
                access: AccessModifier::Public,
                is_static: true,
                name: id("Count"),
                data_type: DataType::Integer,
                no_undo: true,
                get_body: Some(vec![]),
                set_body: Some(vec![]),
                set_parameters: vec![],
            })],
        })]);
        let class_scope = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Class)
            .unwrap()
            .1;
        let p = class_scope
            .get_in(NamespaceId::Values, &fold_atom("count"))
            .unwrap();
        assert!(symbols.get(p).flags.contains(SymbolFlags::STATIC));
        assert!(symbols.get(p).flags.contains(SymbolFlags::NO_UNDO));
    }

    #[test]
    fn handle_parameter_typed_as_handle() {
        use oxabl_ast::{HandleParamKind, HandlePassingOptions};
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Handle {
                kind: HandleParamKind::Table,
                name: id("h"),
                passing: HandlePassingOptions::default(),
            },
        })]);
        let h = find_symbol(&tree, &symbols, NamespaceId::Values, "h").unwrap();
        assert_eq!(
            h.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Handle))
        );
    }

    /// Declare one handle-shaped parameter named `tt` and hand back its symbol,
    /// so the `PARAM_TABLE_LIKE` cases below differ only by `HandleParamKind`.
    fn handle_param_symbol(kind: HandleParamKind) -> Symbol {
        use oxabl_ast::HandlePassingOptions;
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Handle {
                kind,
                name: id("tt"),
                passing: HandlePassingOptions::default(),
            },
        })]);
        find_symbol(&tree, &symbols, NamespaceId::Values, "tt")
            .unwrap()
            .clone()
    }

    #[test]
    fn table_for_parameter_is_marked_table_like() {
        let tt = handle_param_symbol(HandleParamKind::Table);
        assert!(tt.flags.contains(SymbolFlags::PARAM_TABLE_LIKE));
        // The flag adds a fact; it must not re-model the declaration.
        assert_eq!(tt.kind, SymbolKind::Parameter);
        assert_eq!(tt.namespace, NamespaceId::Values);
        assert_eq!(
            tt.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Handle))
        );
    }

    #[test]
    fn dataset_for_parameter_is_marked_table_like() {
        assert!(
            handle_param_symbol(HandleParamKind::Dataset)
                .flags
                .contains(SymbolFlags::PARAM_TABLE_LIKE)
        );
    }

    #[test]
    fn table_handle_parameter_is_not_marked_table_like() {
        // `TABLE-HANDLE h` names a genuine handle value: reads land on this
        // symbol, so its own read_count is meaningful and must not be bypassed.
        assert!(
            !handle_param_symbol(HandleParamKind::TableHandle)
                .flags
                .contains(SymbolFlags::PARAM_TABLE_LIKE)
        );
    }

    #[test]
    fn dataset_handle_parameter_is_not_marked_table_like() {
        assert!(
            !handle_param_symbol(HandleParamKind::DatasetHandle)
                .flags
                .contains(SymbolFlags::PARAM_TABLE_LIKE)
        );
    }

    #[test]
    fn plain_handle_parameter_is_not_marked_table_like() {
        // Proves the flag is not a proxy for "typed HANDLE" — blanket-exempting
        // every HANDLE parameter would discard real unused-parameter findings.
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Variable {
                name: id("h"),
                type_source: TypeSource::Explicit(DataType::Handle),
                no_undo: false,
            },
        })]);
        let h = find_symbol(&tree, &symbols, NamespaceId::Values, "h").unwrap();
        assert_eq!(
            h.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Handle))
        );
        assert!(!h.flags.contains(SymbolFlags::PARAM_TABLE_LIKE));
    }

    #[test]
    fn buffer_parameter_stays_a_buffer_without_the_flag() {
        // Characterization: `DEFINE PARAMETER BUFFER b FOR t` is already immune
        // to LINT0002 because it declares `Buffer`, not `Parameter`. Pinned so
        // the redirect work does not need to touch it.
        let (tree, symbols, _) = run(vec![stmt(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Buffer {
                name: id("b"),
                target: id("t"),
            },
        })]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Values, "b").is_none());
        let b = find_symbol(&tree, &symbols, NamespaceId::Buffers, "b").unwrap();
        assert_eq!(b.kind, SymbolKind::Buffer);
        assert!(!b.flags.contains(SymbolFlags::PARAM_TABLE_LIKE));
    }

    #[test]
    fn diagnostic_label_points_to_prior_declaration() {
        let (_t, _s, diags) = run(vec![
            var_stmt("dup", DataType::Integer),
            var_stmt("dup", DataType::Integer),
        ]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].labels.len(), 1);
    }

    // =======================================================================
    // Resolve pass
    // =======================================================================
    //
    // Tests below exercise `resolve_pass` — Phase 4a of the semantic layer
    // plan. They construct small AST fragments with explicit NodeIds (so the
    // `references` side table has distinct keys), run both passes, and
    // inspect `references`, `symbols`, and `types`.

    use oxabl_ast::{
        AssignPair, CreateTarget, OnAction, OnKind, PreprocIf, RunArgument, RunTarget,
    };
    use std::sync::atomic::{AtomicU32, Ordering};

    /// Monotonic source of distinct `NodeId`s for test-authored AST. Starts
    /// at 1 so that `NodeId::from_u32(0)` / `PROGRAM` stay reserved.
    fn next_nid() -> NodeId {
        static COUNTER: AtomicU32 = AtomicU32::new(1);
        NodeId::from_u32(COUNTER.fetch_add(1, Ordering::Relaxed))
    }

    fn stmt_n(kind: StatementKind) -> Statement {
        Statement::with_id(next_nid(), oxabl_ast::Span::DUMMY, kind)
    }

    fn expr_n(kind: ExpressionKind) -> Expression {
        Expression::with_id(next_nid(), oxabl_ast::Span::DUMMY, kind)
    }

    fn id_expr(name: &str) -> Expression {
        expr_n(ExpressionKind::Identifier(id(name)))
    }

    fn int_lit(v: i64) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 0 },
            value: v,
        })))
    }

    fn var_stmt_n(name: &str, ty: DataType) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn var_stmt_with_init(name: &str, ty: DataType, init: Expression) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: Some(init),
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn run_full(
        stmts: &[Statement],
    ) -> (
        ScopeTree,
        SymbolTable,
        NodeIndexVec<Resolution>,
        NodeIndexVec<ResolvedType>,
    ) {
        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree, mut symbols, _diags, rev) = declare_pass(stmts, &ctx);
        let (refs, types, _rd) = resolve_pass(stmts, &ctx, &tree, &mut symbols, rev);
        (tree, symbols, refs, types)
    }

    fn run_full_with_schema_loaded(
        stmts: &[Statement],
        schema_loaded: bool,
    ) -> (
        ScopeTree,
        SymbolTable,
        NodeIndexVec<Resolution>,
        NodeIndexVec<ResolvedType>,
    ) {
        let schema = Schema::empty();
        let mut ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        ctx.schema_loaded = schema_loaded;
        let (tree, mut symbols, _diags, rev) = declare_pass(stmts, &ctx);
        let (refs, types, _rd) = resolve_pass(stmts, &ctx, &tree, &mut symbols, rev);
        (tree, symbols, refs, types)
    }

    // ---- Schema-backed resolution harness --------------------------------

    use oxabl_schema::test_support::customer_schema as test_schema;

    /// Run declare + resolve against a real loaded schema. Callers that
    /// need check-pass expression types use [`run_analyze_with_schema`].
    fn run_full_with_schema(
        stmts: &[Statement],
        schema: &Schema,
    ) -> (
        ScopeTree,
        SymbolTable,
        NodeIndexVec<Resolution>,
        NodeIndexVec<ResolvedType>,
    ) {
        let ctx = ctx("", schema);
        let (tree, mut symbols, _diags, rev) = declare_pass(stmts, &ctx);
        let (refs, types, _rd) = resolve_pass(stmts, &ctx, &tree, &mut symbols, rev);
        (tree, symbols, refs, types)
    }

    /// Full `analyze_file` (declare + resolve + check) against a loaded
    /// schema — used when a test asserts on check-pass expression types.
    fn run_analyze_with_schema(stmts: &[Statement], schema: &Schema) -> crate::Semantic {
        let ctx = ctx("", schema);
        crate::analyze_file(stmts, &ctx)
    }

    fn resolution_of(refs: &NodeIndexVec<Resolution>, id: NodeId) -> &Resolution {
        refs.get(id)
            .unwrap_or_else(|| panic!("expected reference at NodeId {id:?}"))
    }

    // ---- Bare-identifier resolution --------------------------------------

    #[test]
    fn resolve_bare_identifier_resolves_to_local_variable() {
        let use_x = id_expr("x");
        let use_id = use_x.id;
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(use_x)),
        ];
        let (tree, symbols, refs, _types) = run_full(&stmts);
        let expected = tree
            .resolve(ScopeId::ROOT, NamespaceId::Values, &fold_atom("x"))
            .unwrap();
        assert_eq!(
            resolution_of(&refs, use_id),
            &Resolution::Resolved(expected)
        );
        assert_eq!(symbols.get(expected).read_count, 1);
    }

    #[test]
    fn resolve_undefined_identifier_is_not_in_scope() {
        let ghost = id_expr("ghost");
        let gid = ghost.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(ghost))];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert_eq!(
            resolution_of(&refs, gid),
            &Resolution::Unresolved {
                name: fold_atom("ghost"),
                reason: UnresolvedReason::NotInScope,
            }
        );
    }

    #[test]
    fn resolve_bare_identifier_is_case_insensitive() {
        let use_x = id_expr("MyVar");
        let use_id = use_x.id;
        let stmts = vec![
            var_stmt_n("myvar", DataType::Character),
            stmt_n(StatementKind::ExpressionStatement(use_x)),
        ];
        let (_tree, _symbols, refs, _types) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, use_id),
            Resolution::Resolved(_)
        ));
    }

    // ---- Scope walking ---------------------------------------------------

    #[test]
    fn resolve_sees_outer_variable_from_inner_block() {
        let inner_use = id_expr("outer");
        let use_id = inner_use.id;
        let do_block = stmt_n(StatementKind::Do {
            loop_var: None,
            from: None,
            to: None,
            by: None,
            while_condition: None,
            transaction: false,
            body: vec![stmt_n(StatementKind::ExpressionStatement(inner_use))],
        });
        let stmts = vec![var_stmt_n("outer", DataType::Integer), do_block];
        let (_tree, symbols, refs, _) = run_full(&stmts);
        match resolution_of(&refs, use_id) {
            Resolution::Resolved(sym) => {
                assert_eq!(symbols.get(*sym).name, fold_atom("outer"));
            }
            other => panic!("expected resolved, got {other:?}"),
        }
    }

    #[test]
    fn do_loop_counter_reuses_outer_variable_not_a_shadow() {
        // Outer `x`, and `DO x = 1 TO 3:` — ABL never implicitly declares a
        // loop counter; it reuses the existing `x`. So the counter must NOT
        // mint a block-scoped shadow, and a use of `x` inside the body
        // resolves to the outer variable (its declared type). Minting an
        // implicit integer shadow here was the #83 false positive: it left the
        // real `x` looking unused. (This test previously asserted the shadow
        // behavior; corrected to ABL semantics.)
        let block_use = id_expr("x");
        let use_id = block_use.id;
        let do_block = stmt_n(StatementKind::Do {
            loop_var: Some(id("x")),
            from: Some(int_lit(1)),
            to: Some(int_lit(3)),
            by: None,
            while_condition: None,
            transaction: false,
            body: vec![stmt_n(StatementKind::ExpressionStatement(block_use))],
        });
        let stmts = vec![var_stmt_n("x", DataType::Character), do_block];
        let (tree, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, use_id) else {
            panic!("expected resolved");
        };
        // No block-scope shadow of `x`.
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        assert!(
            block.get_in(NamespaceId::Values, &fold_atom("x")).is_none(),
            "counter must not mint a block-scope shadow"
        );
        // The body use binds to the single, outer `x` (its Character type).
        assert_eq!(symbols.get(*sym).name, fold_atom("x"));
        assert_eq!(
            symbols.get(*sym).data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Character))
        );
        let x_count = symbols
            .iter()
            .filter(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .count();
        assert_eq!(x_count, 1, "expected exactly one `x` variable");
    }

    #[test]
    fn resolve_variable_shadows_builtin() {
        let use_session = id_expr("session");
        let use_id = use_session.id;
        let stmts = vec![
            var_stmt_n("session", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(use_session)),
        ];
        let (_tree, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, use_id) else {
            panic!("expected resolved");
        };
        // Must resolve to the user variable, not the BuiltIn.
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Variable);
    }

    #[test]
    fn resolve_builtin_session_when_not_shadowed() {
        let use_session = id_expr("session");
        let use_id = use_session.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(use_session))];
        let (_tree, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, use_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::BuiltIn);
    }

    // ---- Function calls --------------------------------------------------

    #[test]
    fn resolve_function_call_to_user_function() {
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("calc"),
            arguments: vec![],
        });
        let call_id = call.id;
        let stmts = vec![
            stmt_n(StatementKind::Function {
                name: id("calc"),
                return_type: DataType::Integer,
                body: vec![],
            }),
            stmt_n(StatementKind::ExpressionStatement(call)),
        ];
        let (_t, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, call_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).name, fold_atom("calc"));
    }

    #[test]
    fn resolve_function_call_unknown_is_not_in_scope() {
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("nope"),
            arguments: vec![],
        });
        let call_id = call.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(call))];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert_eq!(
            resolution_of(&refs, call_id),
            &Resolution::Unresolved {
                name: fold_atom("nope"),
                reason: UnresolvedReason::NotInScope,
            }
        );
    }

    #[test]
    fn resolve_function_call_arguments_recurse() {
        let arg = id_expr("a");
        let arg_id = arg.id;
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("calc"),
            arguments: vec![arg],
        });
        let stmts = vec![
            stmt_n(StatementKind::Function {
                name: id("calc"),
                return_type: DataType::Integer,
                body: vec![],
            }),
            var_stmt_n("a", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(call)),
        ];
        let (_t, symbols, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, arg_id),
            Resolution::Resolved(_)
        ));
        let a = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("a") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(a).read_count, 1);
    }

    // ---- NEW / class lookups --------------------------------------------

    #[test]
    fn resolve_new_class_local_resolves_to_type() {
        let new_expr = expr_n(ExpressionKind::New {
            class_name: "Foo".into(),
            arguments: vec![],
        });
        let new_id = new_expr.id;
        let stmts = vec![
            stmt_n(StatementKind::Class {
                name: id("Foo"),
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
                body: vec![],
            }),
            stmt_n(StatementKind::ExpressionStatement(new_expr)),
        ];
        let (_t, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, new_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Class);
    }

    #[test]
    fn resolve_new_class_unknown_is_external() {
        let new_expr = expr_n(ExpressionKind::New {
            class_name: "Nope".into(),
            arguments: vec![],
        });
        let new_id = new_expr.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(new_expr))];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert_eq!(
            resolution_of(&refs, new_id),
            &Resolution::Unresolved {
                name: fold_atom("nope"),
                reason: UnresolvedReason::External,
            }
        );
    }

    // ---- Buffers / CAN-FIND ---------------------------------------------

    #[test]
    fn resolve_can_find_buffer_resolves() {
        let cf = expr_n(ExpressionKind::CanFind {
            find_type: oxabl_ast::FindType::First,
            buffer: id("bCust"),
            where_clause: None,
            lock_type: LockType::NoLock,
            no_error: false,
        });
        let cf_id = cf.id;
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(cf)),
        ];
        let (_t, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, cf_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Buffer);
    }

    #[test]
    fn resolve_can_find_unknown_buffer_is_not_in_scope() {
        let cf = expr_n(ExpressionKind::CanFind {
            find_type: oxabl_ast::FindType::First,
            buffer: id("ghost"),
            where_clause: None,
            lock_type: LockType::NoLock,
            no_error: false,
        });
        let cf_id = cf.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(cf))];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, cf_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::NotInScope,
                ..
            }
        ));
    }

    // ---- Field access / schema ------------------------------------------

    #[test]
    fn resolve_field_access_no_schema_loaded_is_no_schema() {
        let qualifier = id_expr("Customer");
        let qual_id = qualifier.id;
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id("CustNum"),
        });
        let fa_id = fa.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(fa))];
        let (_t, _s, refs, _) = run_full_with_schema_loaded(&stmts, false);
        // Both qualifier and composite are Unresolved NoSchema.
        assert!(matches!(
            resolution_of(&refs, qual_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::NoSchema,
                ..
            }
        ));
        assert!(matches!(
            resolution_of(&refs, fa_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::NoSchema,
                ..
            }
        ));
    }

    #[test]
    fn resolve_field_access_schema_loaded_unknown_qualifier_is_not_in_scope() {
        let qualifier = id_expr("Customer");
        let qual_id = qualifier.id;
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id("CustNum"),
        });
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(fa))];
        let (_t, _s, refs, _) = run_full_with_schema_loaded(&stmts, true);
        assert!(matches!(
            resolution_of(&refs, qual_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::NotInScope,
                ..
            }
        ));
    }

    #[test]
    fn resolve_field_access_resolves_local_buffer_qualifier() {
        let qualifier = id_expr("bCust");
        let qual_id = qualifier.id;
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id("CustNum"),
        });
        let fa_id = fa.id;
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(fa)),
        ];
        // Empty schema with the loaded flag forced: `Customer` is not in the
        // schema, so the buffer has no `table_id` link and the field keeps
        // the legacy `External` resolution.
        let (_t, symbols, refs, _) = run_full_with_schema_loaded(&stmts, true);
        let Resolution::Resolved(sym) = resolution_of(&refs, qual_id) else {
            panic!("qualifier should resolve to buffer");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Buffer);
        assert_eq!(symbols.get(*sym).table_id, None);
        assert!(matches!(
            resolution_of(&refs, fa_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::External,
                ..
            }
        ));
    }

    // ---- Schema-backed resolution (real loaded schema) ------------------

    /// `DEFINE BUFFER bCust FOR Customer.` under a loaded schema.
    fn buffer_customer_stmts(field_name: &str) -> (Vec<Statement>, NodeId, NodeId) {
        let qualifier = id_expr("bCust");
        let qual_id = qualifier.id;
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id(field_name),
        });
        let fa_id = fa.id;
        (
            vec![
                stmt_n(StatementKind::DefineBuffer {
                    name: id("bCust"),
                    target: BufferTarget::Table(id("Customer")),
                    preselect: false,
                    label: None,
                    xml_options: XmlSerializeOptions::default(),
                    is_new_shared: false,
                    is_shared: false,
                    is_new_global_shared: false,
                }),
                stmt_n(StatementKind::ExpressionStatement(fa)),
            ],
            qual_id,
            fa_id,
        )
    }

    #[test]
    fn declare_buffer_links_schema_table_id() {
        let schema = test_schema();
        let (stmts, _, _) = buffer_customer_stmts("CustNum");
        let ctx = ctx("", &schema);
        let (tree, symbols, _d, _rev) = declare_pass(&stmts, &ctx);
        let bsym = tree
            .resolve(ScopeId::ROOT, NamespaceId::Buffers, &fold_atom("bcust"))
            .expect("buffer declared");
        assert_eq!(
            symbols.get(bsym).table_id,
            schema.table_id(&fold_atom("customer"))
        );
    }

    #[test]
    fn declare_buffer_for_missing_table_is_none() {
        let schema = test_schema();
        let stmts = vec![stmt_n(StatementKind::DefineBuffer {
            name: id("b"),
            target: BufferTarget::Table(id("Ghost")),
            preselect: false,
            label: None,
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })];
        let ctx = ctx("", &schema);
        let (tree, symbols, _d, _rev) = declare_pass(&stmts, &ctx);
        let bsym = tree
            .resolve(ScopeId::ROOT, NamespaceId::Buffers, &fold_atom("b"))
            .expect("buffer declared");
        assert_eq!(symbols.get(bsym).table_id, None);
    }

    #[test]
    fn declare_buffer_for_temp_table_is_none() {
        let schema = test_schema();
        let stmts = vec![stmt_n(StatementKind::DefineBuffer {
            name: id("b"),
            target: BufferTarget::TempTable(id("tt")),
            preselect: false,
            label: None,
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })];
        let ctx = ctx("", &schema);
        let (tree, symbols, _d, _rev) = declare_pass(&stmts, &ctx);
        let bsym = tree
            .resolve(ScopeId::ROOT, NamespaceId::Buffers, &fold_atom("b"))
            .expect("buffer declared");
        assert_eq!(symbols.get(bsym).table_id, None);
    }

    #[test]
    fn for_each_implicit_buffer_links_table_id() {
        let schema = test_schema();
        let stmts = vec![stmt_n(StatementKind::ForEach {
            buffer: id("Customer"),
            of_relation: None,
            where_clause: None,
            lock_type: LockType::NoLock,
            body: vec![],
        })];
        let ctx = ctx("", &schema);
        let (tree, symbols, _d, _rev) = declare_pass(&stmts, &ctx);
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .expect("block scope")
            .1;
        let bsym = block
            .get_in(NamespaceId::Buffers, &fold_atom("customer"))
            .expect("implicit buffer");
        assert_eq!(
            symbols.get(bsym).table_id,
            schema.table_id(&fold_atom("customer"))
        );
    }

    #[test]
    fn unqualified_table_name_resolves_via_schema() {
        // Bare `Customer` as a standalone reference (e.g. the argument of
        // `AVAILABLE(Customer)`): no local buffer, schema loaded → resolves
        // to a synthesized default-buffer symbol carrying the table link.
        let schema = test_schema();
        let use_c = id_expr("Customer");
        let use_id = use_c.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(use_c))];
        let (_t, symbols, refs, _) = run_full_with_schema(&stmts, &schema);
        let Resolution::Resolved(sym) = resolution_of(&refs, use_id) else {
            panic!("bare table name should resolve via schema");
        };
        let s = symbols.get(*sym);
        assert_eq!(s.kind, SymbolKind::Buffer);
        assert_eq!(s.table_id, schema.table_id(&fold_atom("customer")));
        assert_eq!(s.declaration, NodeId::DUMMY);
        assert_eq!(s.read_count, 1);
    }

    #[test]
    fn bare_field_access_qualifier_resolves_via_schema() {
        // CRITICAL-fix guard: `Customer.Name` with NO `DEFINE BUFFER`,
        // schema loaded. The qualifier must resolve (no LINT0001) and the
        // field must resolve with its schema type (no LINT0003).
        let schema = test_schema();
        let qualifier = id_expr("Customer");
        let qual_id = qualifier.id;
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id("Name"),
        });
        let fa_id = fa.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(fa))];
        let sem = run_analyze_with_schema(&stmts, &schema);

        let Resolution::Resolved(qsym) = resolution_of(&sem.references, qual_id) else {
            panic!("qualifier should resolve to synthesized default buffer");
        };
        assert_eq!(
            sem.symbols.get(*qsym).table_id,
            schema.table_id(&fold_atom("customer"))
        );

        let Resolution::Resolved(fsym) = resolution_of(&sem.references, fa_id) else {
            panic!("field should resolve via schema");
        };
        assert_eq!(sem.symbols.get(*fsym).kind, SymbolKind::Field);
        assert_eq!(
            sem.symbols.get(*fsym).data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Character))
        );
        // The check pass types the field-access node from the schema field.
        assert_eq!(
            sem.types.get(fa_id),
            Some(&ResolvedType::Primitive(crate::PrimitiveTy::Character))
        );
    }

    #[test]
    fn field_access_valid_field_resolves_and_types() {
        let schema = test_schema();
        let (stmts, qual_id, fa_id) = buffer_customer_stmts("CustNum");
        let sem = run_analyze_with_schema(&stmts, &schema);

        let Resolution::Resolved(qsym) = resolution_of(&sem.references, qual_id) else {
            panic!("qualifier should resolve to the DEFINE BUFFER symbol");
        };
        assert_eq!(
            sem.symbols.get(*qsym).table_id,
            schema.table_id(&fold_atom("customer"))
        );

        let Resolution::Resolved(fsym) = resolution_of(&sem.references, fa_id) else {
            panic!("valid field should resolve");
        };
        let f = sem.symbols.get(*fsym);
        assert_eq!(f.kind, SymbolKind::Field);
        assert_eq!(f.declaration, NodeId::DUMMY);
        assert_eq!(
            f.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
        assert_eq!(
            sem.types.get(fa_id),
            Some(&ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
    }

    #[test]
    fn field_access_accumulates_read_and_write_counts() {
        // Regression for #60: a schema-resolved field must accumulate
        // read/write counts on its synthesized symbol, threading the real
        // access mode through `field_resolution` instead of discarding it.
        // `ASSIGN bCust.CustNum = 1` is a write; a bare `bCust.CustNum`
        // expression is a read. Both fold onto the same (tid, field) synthetic.
        let schema = test_schema();
        let write_target = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id("CustNum"),
        });
        let read_use = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id("CustNum"),
        });
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::Assign {
                assignments: {
                    let mut v: SmallVec<[AssignPair; 4]> = SmallVec::new();
                    v.push(AssignPair {
                        target: write_target,
                        value: int_lit(1),
                    });
                    v
                },
            }),
            stmt_n(StatementKind::ExpressionStatement(read_use)),
        ];
        let (_t, symbols, _refs, _) = run_full_with_schema(&stmts, &schema);
        let f = symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Field && s.name == fold_atom("custnum"))
            .expect("synthesized field symbol")
            .1;
        assert_eq!(f.read_count, 1, "one bare field read");
        assert_eq!(f.write_count, 1, "one assignment-target write");
    }

    #[test]
    fn field_access_invalid_field_is_not_in_scope() {
        let schema = test_schema();
        let (stmts, _qual_id, fa_id) = buffer_customer_stmts("BadField");
        let (_t, _s, refs, _) = run_full_with_schema(&stmts, &schema);
        assert_eq!(
            resolution_of(&refs, fa_id),
            &Resolution::Unresolved {
                name: fold_atom("BadField"),
                reason: UnresolvedReason::NotInScope,
            }
        );
    }

    #[test]
    fn field_access_abbreviated_field_resolves_via_schema() {
        // ABL abbreviation: `bCust.CustN` is an unambiguous leading substring
        // of `CustNum`, so it must resolve (no LINT0003) with the schema type —
        // the same class of reference as the real corpus `scr-wiper.method`.
        let schema = test_schema();
        let (stmts, _qual_id, fa_id) = buffer_customer_stmts("CustN");
        let sem = run_analyze_with_schema(&stmts, &schema);

        let Resolution::Resolved(fsym) = resolution_of(&sem.references, fa_id) else {
            panic!("abbreviated field should resolve");
        };
        let f = sem.symbols.get(*fsym);
        assert_eq!(f.kind, SymbolKind::Field);
        assert_eq!(
            f.data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
    }

    #[test]
    fn field_access_buffer_for_unknown_table_is_external() {
        // `DEFINE BUFFER b FOR Ghost` under a loaded schema: the buffer
        // symbol resolves but has no schema link, so its fields keep the
        // legacy `External` resolution (silent) — no new false positives.
        let schema = test_schema();
        let qualifier = id_expr("b");
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id("Anything"),
        });
        let fa_id = fa.id;
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("b"),
                target: BufferTarget::Table(id("Ghost")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(fa)),
        ];
        let (_t, _s, refs, _) = run_full_with_schema(&stmts, &schema);
        assert!(matches!(
            resolution_of(&refs, fa_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::External,
                ..
            }
        ));
    }

    #[test]
    fn duplicate_field_access_reuses_one_synthetic_symbol() {
        let schema = test_schema();
        let fa1 = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id("CustNum"),
        });
        let fa2 = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id("CustNum"),
        });
        let fa1_id = fa1.id;
        let fa2_id = fa2.id;
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(fa1)),
            stmt_n(StatementKind::ExpressionStatement(fa2)),
        ];
        let (_t, symbols, refs, _) = run_full_with_schema(&stmts, &schema);
        let Resolution::Resolved(f1) = resolution_of(&refs, fa1_id) else {
            panic!("first field access should resolve");
        };
        let Resolution::Resolved(f2) = resolution_of(&refs, fa2_id) else {
            panic!("second field access should resolve");
        };
        // Both references share exactly one synthesized field symbol.
        assert_eq!(f1, f2);
        assert_eq!(
            symbols
                .iter()
                .filter(|(_, s)| s.kind == SymbolKind::Field && s.declaration == NodeId::DUMMY)
                .count(),
            1
        );
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "same schema revision")]
    fn stale_table_id_revision_guard() {
        // Declare against a loaded schema (revision ≥ 1), then run resolve
        // against a *different* (empty, revision 0) schema while passing the
        // stale declare revision. `Schema::get_by_id` is a bare `Vec` index,
        // so the tripwire in `resolve_pass` is the only thing standing
        // between a revision mismatch and a silently-wrong resolution.
        let schema_a = test_schema();
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(id_expr("x")))];
        let ctx_a = ctx("", &schema_a);
        let (tree, mut symbols, _d, declare_revision) = declare_pass(&stmts, &ctx_a);

        let schema_b = Schema::empty();
        let ctx_b = ctx("", &schema_b);
        assert_ne!(declare_revision, schema_b.revision());
        let _ = resolve_pass(&stmts, &ctx_b, &tree, &mut symbols, declare_revision);
    }

    // ---- Read / write counts --------------------------------------------

    #[test]
    fn resolve_assign_counts_reads_and_writes() {
        // ASSIGN x = x + 1.
        let lhs = id_expr("x");
        let rhs_x = id_expr("x");
        let one = int_lit(1);
        let sum = expr_n(ExpressionKind::Add(Box::new(rhs_x), Box::new(one)));
        let assign = stmt_n(StatementKind::Assign {
            assignments: {
                let mut v: SmallVec<[AssignPair; 4]> = SmallVec::new();
                v.push(AssignPair {
                    target: lhs,
                    value: sum,
                });
                v
            },
        });
        let stmts = vec![var_stmt_n("x", DataType::Integer), assign];
        let (_t, symbols, _refs, _types) = run_full(&stmts);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(x).read_count, 1);
        assert_eq!(symbols.get(x).write_count, 1);
    }

    #[test]
    fn resolve_unused_variable_has_zero_counts() {
        let stmts = vec![var_stmt_n("x", DataType::Integer)];
        let (_t, symbols, _refs, _) = run_full(&stmts);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(x).read_count, 0);
        assert_eq!(symbols.get(x).write_count, 0);
    }

    #[test]
    fn resolve_read_counts_on_display() {
        let use_x = id_expr("x");
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::Display {
                stream_name: None,
                items: vec![oxabl_ast::DisplayItem {
                    expression: use_x,
                    when_condition: None,
                }],
                except: vec![],
                frame: None,
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(x).read_count, 1);
    }

    #[test]
    fn resolve_message_set_counts_as_write() {
        let item = id_expr("greeting");
        let stmts = vec![
            var_stmt_n("greeting", DataType::Character),
            var_stmt_n("answer", DataType::Integer),
            stmt_n(StatementKind::Message {
                items: vec![item],
                set_targets: vec![id("answer")],
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let greeting = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("greeting"))
            .unwrap()
            .0;
        let answer = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("answer"))
            .unwrap()
            .0;
        assert_eq!(symbols.get(greeting).read_count, 1);
        assert_eq!(symbols.get(answer).write_count, 1);
        assert_eq!(symbols.get(answer).read_count, 0);
    }

    #[test]
    fn resolve_run_output_argument_counts_as_write() {
        // RUN proc (OUTPUT x).
        let arg_expr = id_expr("x");
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::Run {
                target: RunTarget::literal("proc"),
                arguments: vec![RunArgument {
                    direction: ParameterDirection::Output,
                    expression: arg_expr,
                }],
                in_handle: None,
                persistent: false,
                persistent_handle: None,
                asynchronous: false,
                async_handle: None,
                event_procedure: None,
                no_error: false,
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(x).write_count, 1);
        assert_eq!(symbols.get(x).read_count, 0);
    }

    #[test]
    fn resolve_run_input_output_argument_counts_both() {
        let arg_expr = id_expr("x");
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::Run {
                target: RunTarget::literal("proc"),
                arguments: vec![RunArgument {
                    direction: ParameterDirection::InputOutput,
                    expression: arg_expr,
                }],
                in_handle: None,
                persistent: false,
                persistent_handle: None,
                asynchronous: false,
                async_handle: None,
                event_procedure: None,
                no_error: false,
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(x).read_count, 1);
        assert_eq!(symbols.get(x).write_count, 1);
    }

    // ---- PASSED_AS_OUTPUT_ARG (LINT0002 write-back exemption) -----------

    /// `RUN proc (<direction> <arg>).` preceded by `DEFINE VARIABLE x`.
    fn run_with_arg(direction: ParameterDirection, arg: Expression) -> Vec<Statement> {
        vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::Run {
                target: RunTarget::literal("proc"),
                arguments: vec![RunArgument {
                    direction,
                    expression: arg,
                }],
                in_handle: None,
                persistent: false,
                persistent_handle: None,
                asynchronous: false,
                async_handle: None,
                event_procedure: None,
                no_error: false,
            }),
        ]
    }

    fn sym_x(symbols: &SymbolTable) -> SymbolId {
        symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0
    }

    #[test]
    fn resolve_run_output_argument_sets_passed_as_output_arg() {
        let stmts = run_with_arg(ParameterDirection::Output, id_expr("x"));
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert!(x.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG));
        assert_eq!(x.read_count, 0);
        assert_eq!(x.write_count, 1);
    }

    #[test]
    fn resolve_run_input_output_argument_sets_passed_as_output_arg() {
        let stmts = run_with_arg(ParameterDirection::InputOutput, id_expr("x"));
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert!(x.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG));
        assert_eq!(x.read_count, 1);
        assert_eq!(x.write_count, 1);
    }

    #[test]
    fn resolve_run_return_argument_sets_passed_as_output_arg() {
        let stmts = run_with_arg(ParameterDirection::Return, id_expr("x"));
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert!(x.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG));
        assert_eq!(x.read_count, 1);
        assert_eq!(x.write_count, 1);
    }

    #[test]
    fn resolve_run_input_argument_does_not_set_passed_as_output_arg() {
        // INPUT is an ordinary read; the flag means "a callee writes here".
        let stmts = run_with_arg(ParameterDirection::Input, id_expr("x"));
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert!(!x.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG));
        assert_eq!(x.read_count, 1);
    }

    #[test]
    fn resolve_local_assignment_does_not_set_passed_as_output_arg() {
        // Underpins LINT0002's R3: `x = 1` bumps write_count but must not
        // look like a callee write-back.
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::Assignment {
                target: id_expr("x"),
                value: int_lit(1),
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert!(!x.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG));
        assert_eq!(x.write_count, 1);
    }

    #[test]
    fn resolve_non_identifier_output_argument_is_a_no_op() {
        // A non-identifier write-back target (member/field lvalue) and an
        // unresolved name both miss the reference lookup: no panic, and no
        // flag lands on an unrelated symbol.
        let field = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("no-such-table")),
            field: id("fld"),
        });
        for arg in [field, id_expr("no-such-var")] {
            let stmts = run_with_arg(ParameterDirection::Output, arg);
            let (_t, symbols, _r, _) = run_full(&stmts);
            for (_, s) in symbols.iter() {
                assert!(
                    !s.flags.contains(SymbolFlags::PASSED_AS_OUTPUT_ARG),
                    "unexpected flag on {:?}",
                    s.name
                );
            }
        }
    }

    // ---- Unmodelled-statement crediting (#128) ---------------------------

    /// Build a `Skipped` node harvesting the given names, as the parser would
    /// for e.g. `PUT v-total.`.
    fn skipped_stmt_n(names: &[&str]) -> Statement {
        stmt_n(StatementKind::Skipped {
            names: names.iter().copied().map(id).collect(),
            may_reference_tables: false,
        })
    }

    fn touched(symbols: &SymbolTable, sid: SymbolId) -> bool {
        symbols
            .get(sid)
            .flags
            .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
    }

    #[test]
    fn unmodelled_statement_flags_the_variable_it_names() {
        let stmts = vec![var_stmt_n("x", DataType::Integer), skipped_stmt_n(&["x"])];
        let (_t, symbols, _r, _ty) = run_full(&stmts);
        assert!(touched(&symbols, sym_x(&symbols)));
    }

    /// R4: the flag is the *only* signal this adds. Inflating the counts would
    /// suppress every rule for free, but it would make a shared signal lie about
    /// what the code does — and #126's def-use work reads that signal.
    #[test]
    fn unmodelled_statement_leaves_the_counts_exact() {
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            skipped_stmt_n(&["x", "x", "x"]),
        ];
        let (_t, symbols, _r, _ty) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert_eq!(x.read_count, 0);
        assert_eq!(x.write_count, 0);
    }

    /// R3 / R8, and the property the whole design rests on: the harvest resolves
    /// through `lookup_statement_ident`, which cannot write `references`. If it
    /// ever routed through `resolve_expr_ident` instead, every garbage name in a
    /// skipped statement would land in the table as `Unresolved` and LINT0001
    /// would report it.
    #[test]
    fn unresolvable_harvested_names_touch_neither_references_nor_diagnostics() {
        let baseline = vec![var_stmt_n("x", DataType::Integer)];
        let with_skip = vec![
            var_stmt_n("x", DataType::Integer),
            skipped_stmt_n(&["no-such-name", "another-ghost"]),
        ];

        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree_a, mut sym_a, _d, rev_a) = declare_pass(&baseline, &ctx);
        let (refs_a, _t, diags_a) = resolve_pass(&baseline, &ctx, &tree_a, &mut sym_a, rev_a);
        let (tree_b, mut sym_b, _d, rev_b) = declare_pass(&with_skip, &ctx);
        let (refs_b, _t, diags_b) = resolve_pass(&with_skip, &ctx, &tree_b, &mut sym_b, rev_b);

        assert_eq!(refs_a.iter().count(), refs_b.iter().count());
        assert_eq!(refs_b.iter().count(), 0);
        assert!(diags_a.is_empty());
        assert!(diags_b.is_empty(), "unexpected diagnostics: {diags_b:?}");
    }

    /// KTD8: only `NamespaceId::Values` is credited. A flag on a buffer or table
    /// could never suppress anything — the three rules restrict themselves to
    /// `Variable` and `Parameter` — and each extra namespace would cost a full
    /// scope-chain walk per harvested miss.
    #[test]
    fn unmodelled_statement_credits_only_the_values_namespace() {
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("b-cust"),
                target: BufferTarget::Table(id("customer")),
                preselect: false,
                label: None,
                xml_options: Default::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            skipped_stmt_n(&["b-cust"]),
        ];
        let (_t, symbols, _r, _ty) = run_full(&stmts);
        for (_, s) in symbols.iter() {
            assert!(
                !s.flags
                    .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT),
                "buffer symbol {:?} should not be credited",
                s.name
            );
        }
    }

    #[test]
    fn unmodelled_statement_with_no_names_flags_nothing() {
        let stmts = vec![var_stmt_n("x", DataType::Integer), skipped_stmt_n(&[])];
        let (_t, symbols, _r, _ty) = run_full(&stmts);
        for (_, s) in symbols.iter() {
            assert!(
                !s.flags
                    .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
            );
        }
    }

    /// The accumulate-then-flush-once contract (the Salsa-ready invariant): a
    /// second run over the same AST must produce identical flags and counts.
    #[test]
    fn unmodelled_touch_flush_is_idempotent() {
        let stmts = vec![var_stmt_n("x", DataType::Integer), skipped_stmt_n(&["x"])];
        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree, mut symbols, _d, rev) = declare_pass(&stmts, &ctx);
        let _ = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);
        let first: Vec<_> = symbols
            .iter()
            .map(|(_, s)| (s.flags, s.read_count, s.write_count))
            .collect();
        let _ = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);
        let second: Vec<_> = symbols
            .iter()
            .map(|(_, s)| (s.flags, s.read_count, s.write_count))
            .collect();
        assert_eq!(first, second);
    }

    // ---- Idempotence ----------------------------------------------------

    #[test]
    fn resolve_pass_is_idempotent_for_counts() {
        let use_x = id_expr("x");
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(use_x)),
        ];
        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree, mut symbols, _d, rev) = declare_pass(&stmts, &ctx);
        let _ = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);
        let _ = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        // Running twice yields 1, not 2 — Salsa-ready precondition.
        assert_eq!(symbols.get(x).read_count, 1);
    }

    // ---- Statement-level buffer references ------------------------------

    #[test]
    fn resolve_delete_buffer_bumps_read_count() {
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::Delete {
                buffer: id("bCust"),
                no_error: false,
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let b = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("bcust"))
            .unwrap()
            .0;
        assert_eq!(symbols.get(b).read_count, 1);
    }

    #[test]
    fn resolve_for_each_implicit_buffer_counts() {
        let stmts = vec![stmt_n(StatementKind::ForEach {
            buffer: id("Customer"),
            of_relation: None,
            where_clause: None,
            lock_type: LockType::NoLock,
            body: vec![],
        })];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let b = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("customer") && s.kind == SymbolKind::Buffer)
            .unwrap()
            .0;
        // The FOR-EACH buffer name is itself the only resolving read.
        assert_eq!(symbols.get(b).read_count, 1);
    }

    // ---- Binary / unary expression recursion ----------------------------

    #[test]
    fn resolve_arithmetic_expression_walks_both_sides() {
        let lhs = id_expr("a");
        let rhs = id_expr("b");
        let sum = expr_n(ExpressionKind::Add(Box::new(lhs), Box::new(rhs)));
        let stmts = vec![
            var_stmt_n("a", DataType::Integer),
            var_stmt_n("b", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(sum)),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        for name in ["a", "b"] {
            let s = symbols
                .iter()
                .find(|(_, s)| s.name == fold_atom(name))
                .unwrap()
                .0;
            assert_eq!(symbols.get(s).read_count, 1);
        }
    }

    #[test]
    fn resolve_logical_and_walks_both_sides() {
        let lhs = id_expr("p");
        let rhs = id_expr("q");
        let and_expr = expr_n(ExpressionKind::And(Box::new(lhs), Box::new(rhs)));
        let stmts = vec![
            var_stmt_n("p", DataType::Logical),
            var_stmt_n("q", DataType::Logical),
            stmt_n(StatementKind::ExpressionStatement(and_expr)),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        for name in ["p", "q"] {
            let s = symbols
                .iter()
                .find(|(_, s)| s.name == fold_atom(name))
                .unwrap()
                .0;
            assert_eq!(symbols.get(s).read_count, 1);
        }
    }

    #[test]
    fn resolve_negate_walks_operand() {
        let x = id_expr("x");
        let x_id = x.id;
        let neg = expr_n(ExpressionKind::Negate(Box::new(x)));
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(neg)),
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, x_id),
            Resolution::Resolved(_)
        ));
    }

    #[test]
    fn resolve_array_access_walks_array_and_index() {
        let arr = id_expr("arr");
        let arr_id = arr.id;
        let idx = id_expr("i");
        let idx_id = idx.id;
        let ax = expr_n(ExpressionKind::ArrayAccess {
            array: Box::new(arr),
            index: Box::new(idx),
        });
        let stmts = vec![
            var_stmt_extent("arr", DataType::Integer, 5),
            var_stmt_n("i", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(ax)),
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, arr_id),
            Resolution::Resolved(_)
        ));
        assert!(matches!(
            resolution_of(&refs, idx_id),
            Resolution::Resolved(_)
        ));
    }

    // ---- Namespace shadowing --------------------------------------------

    #[test]
    fn resolve_bare_identifier_prefers_variable_over_buffer() {
        // Variable `customer` and buffer `customer` both declared at root.
        // A bare identifier `customer` in a value position resolves to the
        // variable (Values NS wins over Buffers for bare idents).
        let use_customer = id_expr("customer");
        let use_id = use_customer.id;
        let stmts = vec![
            var_stmt_n("customer", DataType::Integer),
            stmt_n(StatementKind::DefineBuffer {
                name: id("customer"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(use_customer)),
        ];
        let (_t, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, use_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Variable);
    }

    #[test]
    fn resolve_field_qualifier_finds_buffer_not_variable() {
        // Same name variable + buffer — `customer.field` must resolve the
        // qualifier via Buffers namespace.
        let qualifier = id_expr("customer");
        let qual_id = qualifier.id;
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: id("CustNum"),
        });
        let stmts = vec![
            var_stmt_n("customer", DataType::Integer),
            stmt_n(StatementKind::DefineBuffer {
                name: id("customer"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(fa)),
        ];
        let (_t, symbols, refs, _) = run_full_with_schema_loaded(&stmts, true);
        let Resolution::Resolved(sym) = resolution_of(&refs, qual_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Buffer);
    }

    // ---- Method / member external --------------------------------------

    #[test]
    fn resolve_method_call_recurses_into_object() {
        let obj = id_expr("svc");
        let obj_id = obj.id;
        let mc = expr_n(ExpressionKind::MethodCall {
            object: Box::new(obj),
            method: id("doIt"),
            arguments: vec![],
        });
        let stmts = vec![
            var_stmt_n("svc", DataType::Class("Foo".into())),
            stmt_n(StatementKind::ExpressionStatement(mc)),
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, obj_id),
            Resolution::Resolved(_)
        ));
    }

    #[test]
    fn resolve_member_access_recurses_into_object() {
        let obj = id_expr("thing");
        let obj_id = obj.id;
        let ma = expr_n(ExpressionKind::MemberAccess {
            object: Box::new(obj),
            member: id("X"),
        });
        let stmts = vec![
            var_stmt_n("thing", DataType::Handle),
            stmt_n(StatementKind::ExpressionStatement(ma)),
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, obj_id),
            Resolution::Resolved(_)
        ));
    }

    // ---- Types side table -----------------------------------------------

    #[test]
    fn resolve_types_side_table_contains_variable_decl() {
        let vdecl = var_stmt_n("x", DataType::Integer);
        let vid = vdecl.id;
        let stmts = vec![vdecl];
        let (_t, _s, _r, types) = run_full(&stmts);
        assert_eq!(
            types.get(vid),
            Some(&ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
    }

    #[test]
    fn resolve_types_side_table_contains_function_return_type() {
        let fdecl = stmt_n(StatementKind::Function {
            name: id("sum"),
            return_type: DataType::Decimal,
            body: vec![],
        });
        let fid = fdecl.id;
        let stmts = vec![fdecl];
        let (_t, _s, _r, types) = run_full(&stmts);
        assert_eq!(
            types.get(fid),
            Some(&ResolvedType::Primitive(crate::PrimitiveTy::Decimal))
        );
    }

    #[test]
    fn resolve_types_side_table_contains_property_type() {
        let pdecl = stmt_n(StatementKind::Property {
            access: AccessModifier::Public,
            is_static: false,
            name: id("Name"),
            data_type: DataType::Character,
            no_undo: false,
            get_body: Some(vec![]),
            set_body: Some(vec![]),
            set_parameters: vec![],
        });
        let pid = pdecl.id;
        let stmts = vec![stmt_n(StatementKind::Class {
            name: id("Foo"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![pdecl],
        })];
        let (_t, _s, _r, types) = run_full(&stmts);
        assert_eq!(
            types.get(pid),
            Some(&ResolvedType::Primitive(crate::PrimitiveTy::Character))
        );
    }

    #[test]
    fn resolve_variable_initializer_counts_rhs_read() {
        // DEFINE VARIABLE y AS INTEGER INITIAL x.
        let init_use = id_expr("x");
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            var_stmt_with_init("y", DataType::Integer, init_use),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let x = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("x") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(symbols.get(x).read_count, 1);
    }

    // ---- Class type upgrade ---------------------------------------------

    #[test]
    fn resolve_class_typed_variable_upgrades_to_class() {
        let stmts = vec![
            stmt_n(StatementKind::Class {
                name: id("Foo"),
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
                body: vec![],
            }),
            var_stmt_n("svc", DataType::Class("Foo".into())),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let foo_sym = symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Class && s.name == fold_atom("foo"))
            .unwrap()
            .0;
        let svc = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("svc") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        assert_eq!(
            symbols.get(svc).data_type,
            Some(ResolvedType::Class(foo_sym))
        );
    }

    #[test]
    fn resolve_class_typed_variable_unknown_class_stays_unknown() {
        let stmts = vec![var_stmt_n("svc", DataType::Class("Bar".into()))];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let svc = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("svc") && s.kind == SymbolKind::Variable)
            .unwrap()
            .0;
        // No Bar in Types namespace → stays Unknown.
        assert_eq!(symbols.get(svc).data_type, Some(ResolvedType::Unknown));
    }

    // ---- Control-flow walking -------------------------------------------

    #[test]
    fn resolve_if_condition_and_branches_walked() {
        let cond = id_expr("flag");
        let cond_id = cond.id;
        let then_use = id_expr("a");
        let then_id = then_use.id;
        let if_stmt = stmt_n(StatementKind::If {
            condition: cond,
            then_branch: Box::new(stmt_n(StatementKind::ExpressionStatement(then_use))),
            else_branch: None,
        });
        let stmts = vec![
            var_stmt_n("flag", DataType::Logical),
            var_stmt_n("a", DataType::Integer),
            if_stmt,
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, cond_id),
            Resolution::Resolved(_)
        ));
        assert!(matches!(
            resolution_of(&refs, then_id),
            Resolution::Resolved(_)
        ));
    }

    #[test]
    fn resolve_case_expression_and_when_values_walked() {
        let discr = id_expr("x");
        let discr_id = discr.id;
        let matcher = int_lit(1);
        let body_use = id_expr("y");
        let body_id = body_use.id;
        let case = stmt_n(StatementKind::Case {
            expression: discr,
            when_branches: vec![oxabl_ast::WhenBranch {
                values: vec![matcher],
                body: vec![stmt_n(StatementKind::ExpressionStatement(body_use))],
            }],
            otherwise: None,
        });
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            var_stmt_n("y", DataType::Integer),
            case,
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, discr_id),
            Resolution::Resolved(_)
        ));
        assert!(matches!(
            resolution_of(&refs, body_id),
            Resolution::Resolved(_)
        ));
    }

    #[test]
    fn resolve_preproc_if_walks_both_branches() {
        let then_use = id_expr("x");
        let then_id = then_use.id;
        let else_use = id_expr("y");
        let else_id = else_use.id;
        let cond = expr_n(ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
            span: Span { start: 0, end: 4 },
            value: true,
        })));
        let pif = stmt_n(StatementKind::PreprocIf(PreprocIf {
            condition: cond,
            then_branch: vec![stmt_n(StatementKind::ExpressionStatement(then_use))],
            elseif_branches: vec![],
            else_branch: Some(vec![stmt_n(StatementKind::ExpressionStatement(else_use))]),
        }));
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            var_stmt_n("y", DataType::Integer),
            pif,
        ];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, then_id),
            Resolution::Resolved(_)
        ));
        assert!(matches!(
            resolution_of(&refs, else_id),
            Resolution::Resolved(_)
        ));
    }

    // ---- Procedure / function body scope chain --------------------------

    #[test]
    fn resolve_inside_procedure_sees_file_scope_variable() {
        let body_use = id_expr("outer");
        let use_id = body_use.id;
        let proc = stmt_n(StatementKind::Procedure {
            name: id("p"),
            body: vec![stmt_n(StatementKind::ExpressionStatement(body_use))],
        });
        let stmts = vec![var_stmt_n("outer", DataType::Integer), proc];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, use_id),
            Resolution::Resolved(_)
        ));
    }

    #[test]
    fn resolve_inside_method_sees_method_parameter() {
        let body_use = id_expr("arg");
        let use_id = body_use.id;
        let method = stmt_n(StatementKind::Method {
            access: AccessModifier::Public,
            is_static: false,
            is_abstract: false,
            is_override: false,
            return_type: None,
            name: id("m"),
            parameters: vec![param_stmt(
                "arg",
                ParameterDirection::Input,
                DataType::Integer,
            )],
            body: vec![stmt_n(StatementKind::ExpressionStatement(body_use))],
        });
        let stmts = vec![stmt_n(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![method],
        })];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, use_id),
            Resolution::Resolved(_)
        ));
    }

    // ---- Literal / preproc reference — no entry ------------------------

    #[test]
    fn resolve_literal_has_no_reference_entry() {
        let lit = int_lit(42);
        let lit_id = lit.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(lit))];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(refs.get(lit_id).is_none());
    }

    #[test]
    fn resolve_preproc_reference_has_no_entry() {
        let pref = expr_n(ExpressionKind::PreprocReference("foo".into()));
        let pref_id = pref.id;
        let stmts = vec![stmt_n(StatementKind::ExpressionStatement(pref))];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(refs.get(pref_id).is_none());
    }

    // ---- CREATE buffer / handle ----------------------------------------

    #[test]
    fn resolve_create_buffer_bumps_write() {
        let stmts = vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::Create {
                target: CreateTarget::Name(id("bCust")),
                no_error: false,
            }),
        ];
        let (_t, symbols, _r, _) = run_full(&stmts);
        let b = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("bcust"))
            .unwrap()
            .0;
        assert_eq!(symbols.get(b).write_count, 1);
    }

    // ---- ON trigger block walks -----------------------------------------

    #[test]
    fn resolve_on_trigger_block_walks_body() {
        let body_use = id_expr("x");
        let use_id = body_use.id;
        let on = stmt_n(StatementKind::On {
            kind: OnKind::UiEvent {
                clauses: vec![oxabl_ast::OnEventClause {
                    events: vec![id("CHOOSE")],
                    widgets: vec![],
                }],
                anywhere: false,
                action: OnAction::Block(Box::new(stmt_n(StatementKind::Block(vec![stmt_n(
                    StatementKind::ExpressionStatement(body_use),
                )])))),
            },
        });
        let stmts = vec![var_stmt_n("x", DataType::Integer), on];
        let (_t, _s, refs, _) = run_full(&stmts);
        assert!(matches!(
            resolution_of(&refs, use_id),
            Resolution::Resolved(_)
        ));
    }

    // ---- Builtins visible inside nested scope ---------------------------

    #[test]
    fn resolve_builtin_visible_inside_procedure() {
        let use_s = id_expr("SESSION");
        let use_id = use_s.id;
        let proc = stmt_n(StatementKind::Procedure {
            name: id("p"),
            body: vec![stmt_n(StatementKind::ExpressionStatement(use_s))],
        });
        let stmts = vec![proc];
        let (_t, symbols, refs, _) = run_full(&stmts);
        let Resolution::Resolved(sym) = resolution_of(&refs, use_id) else {
            panic!("expected resolved");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::BuiltIn);
    }

    // ---- Table reads in buffer, empty-table, and query forms (#130) ------
    //
    // These forms name a table without reading a field of it, so nothing in the
    // expression walk ever sees them. Until #130 they left the backing symbol at
    // `read_count == 0`, which made LINT0002's table-parameter redirect report a
    // `TABLE FOR tt` parameter whose temp-table was used only this way.

    /// A marked `Skipped` node, as the parser builds for `DEFINE QUERY`,
    /// `OPEN QUERY`, and `EMPTY TEMP-TABLE`.
    fn skipped_table_stmt_n(names: &[&str]) -> Statement {
        stmt_n(StatementKind::Skipped {
            names: names.iter().copied().map(id).collect(),
            may_reference_tables: true,
        })
    }

    fn tt_stmt_n(name: &str) -> Statement {
        stmt_n(StatementKind::DefineTempTable {
            name: id(name),
            no_undo: false,
            like_table: None,
            validate: false,
            use_indexes: vec![],
            indexes: vec![],
            xml_options: XmlSerializeOptions::default(),
            fields: vec![],
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn buffer_stmt_n(name: &str, target: BufferTarget) -> Statement {
        stmt_n(StatementKind::DefineBuffer {
            name: id(name),
            target,
            preselect: false,
            label: None,
            xml_options: XmlSerializeOptions::default(),
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    /// Reads recorded on the `Buffers`-namespace symbol called `name` — the
    /// namespace both temp-tables and buffers declare into.
    #[track_caller]
    fn buffer_reads(tree: &ScopeTree, symbols: &SymbolTable, name: &str) -> u32 {
        find_symbol(tree, symbols, NamespaceId::Buffers, name)
            .unwrap_or_else(|| panic!("no Buffers symbol named {name}"))
            .read_count
    }

    /// R1 + R2: both `BufferTarget` spellings credit the temp-table they name.
    /// The buffer being *declared* is not itself read by its own definition, so
    /// it stays at zero — crediting it would resurrect the false positive one
    /// level up.
    #[test]
    fn define_buffer_credits_its_target_in_both_spellings() {
        for target in [
            BufferTarget::Table(id("ttItem")),
            BufferTarget::TempTable(id("ttItem")),
        ] {
            let stmts = vec![tt_stmt_n("ttItem"), buffer_stmt_n("bItem", target)];
            let (tree, symbols, _r, _ty) = run_full(&stmts);
            assert_eq!(buffer_reads(&tree, &symbols, "ttItem"), 1);
            assert_eq!(buffer_reads(&tree, &symbols, "bItem"), 0);
        }
    }

    #[test]
    fn two_buffer_definitions_credit_two_reads() {
        let stmts = vec![
            tt_stmt_n("ttItem"),
            buffer_stmt_n("bItem", BufferTarget::Table(id("ttItem"))),
            buffer_stmt_n("bItem2", BufferTarget::TempTable(id("ttItem"))),
        ];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "ttItem"), 2);
    }

    /// R3: `DEFINE PARAMETER BUFFER b FOR tt.` has the same defect and the same
    /// fix. No issue owns it but this one — it never routes through a skip
    /// helper, so #128's carrier could never have reached it.
    #[test]
    fn define_parameter_buffer_credits_its_target() {
        let stmts = vec![
            tt_stmt_n("ttItem"),
            stmt_n(StatementKind::DefineParameter {
                direction: ParameterDirection::Input,
                param_type: ParameterType::Buffer {
                    name: id("bItem"),
                    target: id("ttItem"),
                },
            }),
        ];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "ttItem"), 1);
    }

    /// Other parameter shapes are untouched: a scalar parameter names no table,
    /// and must not acquire a read on anything.
    #[test]
    fn scalar_parameter_credits_no_table() {
        let stmts = vec![
            tt_stmt_n("ttItem"),
            stmt_n(StatementKind::DefineParameter {
                direction: ParameterDirection::Input,
                param_type: ParameterType::Variable {
                    name: id("ttItem"),
                    type_source: TypeSource::Explicit(DataType::Integer),
                    no_undo: false,
                },
            }),
        ];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "ttItem"), 0);
    }

    /// KTD6's guard. `DEFINE BUFFER Customer FOR Customer.` is the standard ABL
    /// block-scoping idiom, and the declare pass has already bound the new buffer
    /// under that same folded name — so an unguarded lookup resolves the target
    /// to the buffer being declared and self-credits it. A buffer that reads
    /// itself into existence would silence every count-gated rule for free.
    #[test]
    fn same_name_buffer_idiom_does_not_self_credit() {
        let stmts = vec![buffer_stmt_n(
            "Customer",
            BufferTarget::Table(id("Customer")),
        )];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "Customer"), 0);
    }

    /// R10: an unresolvable target is silent — no diagnostic, no `references`
    /// entry. A raw `{&macro}` target that survived to the AST is the same case:
    /// it names nothing resolvable and must not be reported.
    #[test]
    fn unknown_and_preprocessor_buffer_targets_stay_silent() {
        let stmts = vec![
            buffer_stmt_n("bGhost", BufferTarget::Table(id("no-such-table"))),
            buffer_stmt_n("bMacro", BufferTarget::TempTable(id("{&tt-name}"))),
        ];
        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree, mut symbols, _d, rev) = declare_pass(&stmts, &ctx);
        let (refs, _t, diags) = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);
        assert_eq!(refs.iter().count(), 0);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    }

    /// R5: the marker is what earns the harvest a table-namespace lookup.
    #[test]
    fn marked_skipped_names_credit_an_in_scope_temp_table() {
        let stmts = vec![tt_stmt_n("ttItem"), skipped_table_stmt_n(&["q", "ttItem"])];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "ttItem"), 1);
    }

    /// R6: and an unmarked one does not. This is the whole reason the marker
    /// exists rather than crediting every skipped statement's harvest — an
    /// ordinary `PUT` naming a token that happens to match a temp-table must not
    /// silence the rules for that table.
    #[test]
    fn unmarked_skipped_names_credit_no_table() {
        let stmts = vec![tt_stmt_n("ttItem"), skipped_stmt_n(&["ttItem"])];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "ttItem"), 0);
    }

    /// R7: the two lookups are independent, not alternatives. A marked node
    /// still gives an in-scope scalar #128's uncertainty flag, and still leaves
    /// its counts exact — the parser cannot tell a read from a write inside a
    /// grammar it does not model, and that was never the table side's claim.
    #[test]
    fn marked_skipped_still_flags_values_and_leaves_their_counts_exact() {
        let stmts = vec![
            var_stmt_n("x", DataType::Integer),
            skipped_table_stmt_n(&["x"]),
        ];
        let (_t, symbols, _r, _ty) = run_full(&stmts);
        let x = symbols.get(sym_x(&symbols));
        assert!(
            x.flags
                .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
        );
        assert_eq!(x.read_count, 0);
        assert_eq!(x.write_count, 0);
    }

    /// R10 for the skipped half: the harvest is deliberately over-inclusive, so
    /// a candidate that resolves nowhere must produce nothing at all. Routing it
    /// through anything that records `Unresolved` would turn every stray token in
    /// a query into a LINT0001 report.
    #[test]
    fn unknown_marked_candidates_create_no_reference_and_no_diagnostic() {
        let stmts = vec![
            tt_stmt_n("ttItem"),
            skipped_table_stmt_n(&["no-such-name", "another-ghost"]),
        ];
        let schema = Schema::empty();
        let ctx = ctx("", &schema);
        let (tree, mut symbols, _d, rev) = declare_pass(&stmts, &ctx);
        let (refs, _t, diags) = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);
        assert_eq!(refs.iter().count(), 0);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    }

    /// KTD5's shadowing case, stated as a test because it looks like a bug
    /// otherwise: one token can resolve in both namespaces, and both sides fire.
    /// That ambiguity is precisely why the statement is still marked skipped —
    /// the value side records "we cannot judge this", the table side supplies the
    /// count the redirect needs, and neither can be inferred from the other.
    #[test]
    fn a_marked_candidate_may_credit_both_namespaces_under_shadowing() {
        let stmts = vec![
            tt_stmt_n("item"),
            var_stmt_n("item", DataType::Integer),
            skipped_table_stmt_n(&["item"]),
        ];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert_eq!(buffer_reads(&tree, &symbols, "item"), 1);
        let v = symbols
            .iter()
            .find(|(_, s)| s.name == fold_atom("item") && s.kind == SymbolKind::Variable)
            .expect("variable `item`")
            .1;
        assert!(
            v.flags
                .contains(SymbolFlags::TOUCHED_BY_UNMODELLED_STATEMENT)
        );
        assert_eq!(v.read_count, 0);
    }

    /// KTD8: a bare schema table is still credited by nobody. `FIND` and `FOR
    /// EACH` behave the same way today — `synth_table_buffer_symbol` inserts into
    /// the `SymbolTable` without binding into the `ScopeTree`, and nothing
    /// declares into `NamespaceId::Tables` at all. Fixing that is a semantic-model
    /// change with visible `oxabl analyze` consequences, deliberately out of
    /// scope here. This test pins the boundary so the follow-up has a starting
    /// point rather than a surprise.
    #[test]
    fn schema_only_targets_retain_current_no_credit_behavior() {
        let stmts = vec![buffer_stmt_n("bCust", BufferTarget::Table(id("Customer")))];
        let (tree, symbols, _r, _ty) = run_full(&stmts);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Tables, "Customer").is_none());
    }

    // -------------------------------------------------------------------
    // The workspace type-name memo is case-sensitive
    // -------------------------------------------------------------------

    /// An index that answers `Found` for exactly one **as-written** spelling of
    /// one class, and `NotFound` for every other spelling of the same name.
    ///
    /// That is not a contrived shape — it is what a real
    /// [`BatchIndex`](oxabl_index::BatchIndex) does over a case-sensitive
    /// filesystem: the search derives a relative path from the name as written,
    /// so `MyApp.Cache` finds `MyApp/Cache.cls` and `myapp.cache` does not. A
    /// stub is used rather than the real index because `oxabl_index` depends on
    /// this crate, and because the point under test is the memo *above* the
    /// index, not the search below it.
    struct OneSpellingIndex {
        /// The one spelling that resolves, compared byte-for-byte.
        written: &'static str,
    }

    impl WorkspaceIndex for OneSpellingIndex {
        fn class(&self, name: &IndexName) -> IndexAnswer<std::sync::Arc<ClassDescriptor>> {
            if name.as_written() != self.written {
                return IndexAnswer::NotFound;
            }
            IndexAnswer::Found(std::sync::Arc::new(ClassDescriptor {
                name: name.clone(),
                file: IndexedFileId::new(1),
                kind: ClassKind::Class,
                inherits: None,
                implements: Vec::new(),
            }))
        }

        fn class_members(
            &self,
            _class: &IndexName,
        ) -> IndexAnswer<std::sync::Arc<[MemberDescriptor]>> {
            IndexAnswer::Found(std::sync::Arc::from(Vec::new()))
        }

        fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
            IndexAnswer::NotFound
        }

        fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
            IndexAnswer::NotFound
        }

        fn revision(&self) -> IndexRevision {
            IndexRevision::new(1)
        }
    }

    /// Resolve `source` against [`OneSpellingIndex`] and report, per `NEW`
    /// expression in source order, whether its type name resolved.
    fn new_targets_resolved(source: &str, written: &'static str) -> Vec<bool> {
        let index = OneSpellingIndex { written };
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        assert!(
            program.errors.is_empty(),
            "fixture must parse cleanly: {:?}",
            program.errors
        );
        let stmts = program.statements;
        let (tree, mut symbols, _d, rev) = declare_pass(&stmts, &ctx);
        let (refs, _t, _diags) = resolve_pass(&stmts, &ctx, &tree, &mut symbols, rev);

        // Collected by walking the statements rather than by iterating `refs`, so
        // the answers come back in the order the file wrote the two `NEW`s — which
        // is the whole variable under test.
        let mut out = Vec::new();
        for stmt in &stmts {
            let StatementKind::Assignment { value, .. } = &stmt.kind else {
                continue;
            };
            if !matches!(value.kind, ExpressionKind::New { .. }) {
                continue;
            }
            out.push(matches!(refs.get(value.id), Some(Resolution::Resolved(_))));
        }
        out
    }

    /// The resolve-level mirror of `oxabl_index`'s
    /// `a_miss_under_one_spelling_does_not_poison_another`. The lower memo keys on
    /// the as-written spelling; a memo above it that folds the key would undo
    /// that, and this is the test that says so.
    #[test]
    fn a_type_lookup_miss_under_one_spelling_does_not_poison_another() {
        // The miss first. If `type_lookups` folded its key, the `myapp.cache`
        // miss would be served to `MyApp.Cache` and the second `NEW` would fail.
        let miss_first = "DEFINE VARIABLE v-a AS CHARACTER NO-UNDO.\n\
                          v-a = NEW myapp.cache().\n\
                          v-a = NEW MyApp.Cache().\n";
        assert_eq!(
            new_targets_resolved(miss_first, "MyApp.Cache"),
            vec![false, true],
            "the spelling that matches the filesystem resolves even when a \
             different spelling of the same name missed first"
        );

        // And the reverse order, which passes even with a folded key — included so
        // the pair shows the asymmetry is the bug rather than the fixture.
        let hit_first = "DEFINE VARIABLE v-a AS CHARACTER NO-UNDO.\n\
                         v-a = NEW MyApp.Cache().\n\
                         v-a = NEW myapp.cache().\n";
        assert_eq!(
            new_targets_resolved(hit_first, "MyApp.Cache"),
            vec![true, false],
            "and a recorded hit is likewise not handed to the spelling that \
             genuinely misses"
        );
    }
}
