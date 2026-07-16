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

use oxabl_ast::{
    AccessModifier, CreateTarget, DataType, Expression, ExpressionKind, Identifier, NodeId,
    OnAction, OnKind, ParameterDirection, ParameterType, RunTarget, Statement, StatementKind,
    StreamOperation, SubscribeTarget, TypeSource,
};
use oxabl_common::{Diagnostic, VirtualSpan};
use oxabl_lexer::oxabl_atom::OxablAtom;
use rustc_hash::FxHashMap;

use crate::{
    AnalysisContext, NamespaceId, NodeIndexVec, ResolvedType, ScopeId, ScopeKind, ScopeTree,
    Symbol, SymbolFlags, SymbolId, SymbolKind, SymbolTable, builtins, diagnostics, resolve_span,
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
    /// reference outside the single-file unit.
    External,
    /// Field / table reference that needs a schema we don't have loaded.
    NoSchema,
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

/// Run the declare pass over `program` and return the populated scope tree,
/// symbol table, and any declaration-level diagnostics.
pub fn declare_pass(
    program: &[Statement],
    ctx: &AnalysisContext,
) -> (ScopeTree, SymbolTable, Vec<Diagnostic>) {
    let mut walker = Walker::new(ctx);
    walker.walk_block(program, ScopeId::ROOT);
    (walker.tree, walker.symbols, walker.diagnostics)
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
                ..
            } => self.declare_variable(stmt, scope, name, type_source, *extent),

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
                );
                if let Some(body) = get_body {
                    let getter = self.tree.push(ScopeKind::PropertyGet, scope, stmt.id);
                    self.walk_block(body, getter);
                }
                if let Some(body) = set_body {
                    let setter = self.tree.push(ScopeKind::PropertySet, scope, stmt.id);
                    self.walk_block(body, setter);
                }
            }

            // ---- Temp-table -----------------------------------------------
            StatementKind::DefineTempTable { name, fields, .. } => {
                self.declare_temp_table(stmt, scope, name, fields);
            }

            // ---- Buffer ---------------------------------------------------
            StatementKind::DefineBuffer { name, .. } => {
                self.declare_simple(stmt, scope, name, NamespaceId::Buffers, SymbolKind::Buffer);
            }

            // ---- Dataset / Data-source ------------------------------------
            StatementKind::DefineDataset {
                name,
                is_shared,
                is_new_shared,
                ..
            } => {
                let flags = flag_if(*is_shared, SymbolFlags::SHARED)
                    | flag_if(*is_new_shared, SymbolFlags::NEW_SHARED);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::Dataset,
                    None,
                    flags,
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
                );
                let proc_scope = self.tree.push(ScopeKind::Procedure, scope, stmt.id);
                self.walk_block(body, proc_scope);
            }
            StatementKind::Function {
                name,
                return_type,
                body,
            } => {
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Functions,
                    SymbolKind::Function,
                    Some(ResolvedType::from_data_type(return_type)),
                    SymbolFlags::empty(),
                );
                let fn_scope = self.tree.push(ScopeKind::Function, scope, stmt.id);
                self.walk_block(body, fn_scope);
            }

            // ---- Class / Method / Constructor / Destructor / Interface ----
            StatementKind::Class {
                name,
                is_abstract,
                is_final,
                body,
                ..
            } => {
                let flags = flag_if(*is_abstract, SymbolFlags::ABSTRACT)
                    | flag_if(*is_final, SymbolFlags::FINAL);
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Types,
                    SymbolKind::Class,
                    None,
                    flags,
                );
                let class_scope = self.tree.push(ScopeKind::Class, scope, stmt.id);
                self.walk_block(body, class_scope);
            }
            StatementKind::Interface { name, body, .. } => {
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Types,
                    SymbolKind::Interface,
                    None,
                    SymbolFlags::empty(),
                );
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
                    self.declare(
                        stmt,
                        bs,
                        id,
                        NamespaceId::Values,
                        SymbolKind::Variable,
                        Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer)),
                        SymbolFlags::empty(),
                    );
                }
                self.walk_block(body, bs);
            }
            StatementKind::Repeat { body, .. } => {
                let bs = self.tree.push(ScopeKind::Block, scope, stmt.id);
                self.walk_block(body, bs);
            }
            StatementKind::ForEach { buffer, body, .. } => {
                let bs = self.tree.push(ScopeKind::Block, scope, stmt.id);
                // FOR EACH introduces an implicit buffer at block scope.
                self.declare(
                    stmt,
                    bs,
                    buffer,
                    NamespaceId::Buffers,
                    SymbolKind::Buffer,
                    None,
                    SymbolFlags::empty(),
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
        self.declare(
            stmt,
            scope,
            name,
            NamespaceId::Values,
            SymbolKind::Variable,
            data_type,
            SymbolFlags::empty(),
        );
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
                );
            }
            ParameterType::Buffer { name, .. } => {
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Buffers,
                    SymbolKind::Buffer,
                    None,
                    dir_flag,
                );
            }
            ParameterType::Handle { name, .. } => {
                self.declare(
                    stmt,
                    scope,
                    name,
                    NamespaceId::Values,
                    SymbolKind::Parameter,
                    Some(ResolvedType::Primitive(crate::PrimitiveTy::Handle)),
                    dir_flag,
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
    ) {
        let tt_sym = self.declare(
            stmt,
            scope,
            name,
            NamespaceId::Buffers,
            SymbolKind::TempTable,
            None,
            SymbolFlags::empty(),
        );
        // Fields get their own symbols scoped to the file; namespace
        // `Values` is a simplification for v1 — Phase 4a will likely
        // route field lookups through the temp-table symbol instead.
        let Some(tt_scope) = tt_sym else { return };
        let _ = tt_scope;
        for field in fields {
            let data_type = match &field.type_source {
                TypeSource::Explicit(dt) => {
                    Some(wrap_extent(ResolvedType::from_data_type(dt), field.extent))
                }
                TypeSource::Like { .. } => None,
            };
            self.declare(
                stmt,
                scope,
                &field.name,
                NamespaceId::Values,
                SymbolKind::Field,
                data_type,
                SymbolFlags::empty(),
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
        self.declare(stmt, scope, name, ns, kind, None, SymbolFlags::empty());
    }

    /// Core insertion routine. Returns the new `SymbolId` on success;
    /// returns `None` if a duplicate was suppressed (and emits `SEM0001`).
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
        };
        let id = self.symbols.insert(symbol);
        self.tree.get_mut(scope).bindings[ns.index()].insert(atom, id);
        Some(id)
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
/// Idempotent: per-symbol read/write counts are collected into a local
/// accumulator and written back at end-of-pass, so re-running the pass is a
/// no-op. This is the Salsa-ready invariant called out in plan §C7.
pub fn resolve_pass(
    program: &[Statement],
    ctx: &AnalysisContext,
    tree: &ScopeTree,
    symbols: &mut SymbolTable,
) -> (
    NodeIndexVec<Resolution>,
    NodeIndexVec<ResolvedType>,
    Vec<Diagnostic>,
) {
    let mut walker = ResolveWalker::new(ctx, tree);
    walker.upgrade_class_types(program, symbols);
    walker.walk_block(program, ScopeId::ROOT);

    // Flush symbol-level read/write counts exactly once at pass end.
    for (sym, (reads, writes)) in &walker.counts {
        let s = symbols.get_mut(*sym);
        s.read_count = *reads;
        s.write_count = *writes;
    }

    // Mirror every symbol's declared type into the `types` side table keyed
    // by the declaration's NodeId. Skips builtins (declaration = DUMMY).
    for (_, sym) in symbols.iter() {
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
    references: NodeIndexVec<Resolution>,
    types: NodeIndexVec<ResolvedType>,
    diagnostics: Vec<Diagnostic>,
    /// Per-symbol `(reads, writes)` accumulator. Written to `Symbol` exactly
    /// once at end-of-pass so the pass stays idempotent under Salsa re-run.
    counts: FxHashMap<SymbolId, (u32, u32)>,
}

impl<'a> ResolveWalker<'a> {
    fn new(ctx: &'a AnalysisContext<'a>, tree: &'a ScopeTree) -> Self {
        ResolveWalker {
            ctx,
            tree,
            references: NodeIndexVec::new(),
            types: NodeIndexVec::new(),
            diagnostics: Vec::new(),
            counts: FxHashMap::default(),
        }
    }

    /// Walk the program once to find declarations with `DataType::Class(name)`
    /// and upgrade their symbol's `data_type` from `ResolvedType::Unknown` to
    /// `ResolvedType::Class(SymbolId)` when `name` resolves in the local
    /// `Types` namespace. Class names that don't resolve locally are
    /// `External` (cross-file or USING-imported) and stay `Unknown` — the
    /// type-check pass treats `Unknown` as the lattice bottom.
    fn upgrade_class_types(&self, program: &[Statement], symbols: &mut SymbolTable) {
        let mut upgrades: Vec<(SymbolId, ResolvedType)> = Vec::new();
        self.collect_class_upgrades(program, ScopeId::ROOT, symbols, &mut upgrades);
        for (sid, rt) in upgrades {
            symbols.get_mut(sid).data_type = Some(rt);
        }
    }

    fn collect_class_upgrades(
        &self,
        stmts: &[Statement],
        scope: ScopeId,
        symbols: &SymbolTable,
        out: &mut Vec<(SymbolId, ResolvedType)>,
    ) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::VariableDeclaration {
                    name,
                    type_source: TypeSource::Explicit(DataType::Class(class_name)),
                    ..
                } => {
                    if let Some(up) = self.class_upgrade(class_name, scope, symbols, name) {
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
                    if let Some(up) = self.class_upgrade(class_name, scope, symbols, name) {
                        out.push(up);
                    }
                }
                StatementKind::Property {
                    name,
                    data_type: DataType::Class(class_name),
                    ..
                } => {
                    if let Some(up) = self.class_upgrade(class_name, scope, symbols, name) {
                        out.push(up);
                    }
                }
                StatementKind::Function {
                    name,
                    return_type: DataType::Class(class_name),
                    ..
                } => {
                    if let Some(up) = self.class_upgrade_in_ns(
                        class_name,
                        scope,
                        NamespaceId::Functions,
                        symbols,
                        name,
                    ) {
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
                        self.collect_class_upgrades(body, ps, symbols, out);
                    }
                }
                StatementKind::Function { body, .. } => {
                    if let Some(fs) = self.find_child_scope(scope, stmt.id, ScopeKind::Function) {
                        self.collect_class_upgrades(body, fs, symbols, out);
                    }
                }
                StatementKind::Class { body, .. } => {
                    if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Class) {
                        self.collect_class_upgrades(body, cs, symbols, out);
                    }
                }
                StatementKind::Interface { body, .. } => {
                    if let Some(is_) = self.find_child_scope(scope, stmt.id, ScopeKind::Interface) {
                        self.collect_class_upgrades(body, is_, symbols, out);
                    }
                }
                StatementKind::Method {
                    parameters, body, ..
                } => {
                    if let Some(ms) = self.find_child_scope(scope, stmt.id, ScopeKind::Method) {
                        self.collect_class_upgrades(parameters, ms, symbols, out);
                        self.collect_class_upgrades(body, ms, symbols, out);
                    }
                }
                StatementKind::Constructor {
                    parameters, body, ..
                } => {
                    if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Constructor)
                    {
                        self.collect_class_upgrades(parameters, cs, symbols, out);
                        self.collect_class_upgrades(body, cs, symbols, out);
                    }
                }
                StatementKind::Destructor { body } => {
                    if let Some(ds) = self.find_child_scope(scope, stmt.id, ScopeKind::Destructor) {
                        self.collect_class_upgrades(body, ds, symbols, out);
                    }
                }
                StatementKind::Do { body, .. } | StatementKind::Repeat { body, .. } => {
                    if let Some(bs) = self.find_child_scope(scope, stmt.id, ScopeKind::Block) {
                        self.collect_class_upgrades(body, bs, symbols, out);
                    }
                }
                StatementKind::ForEach { body, .. } => {
                    if let Some(bs) = self.find_child_scope(scope, stmt.id, ScopeKind::Block) {
                        self.collect_class_upgrades(body, bs, symbols, out);
                    }
                }
                StatementKind::Catch { body, .. } => {
                    if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Catch) {
                        self.collect_class_upgrades(body, cs, symbols, out);
                    }
                }
                StatementKind::Finally { body } => {
                    if let Some(fs) = self.find_child_scope(scope, stmt.id, ScopeKind::Finally) {
                        self.collect_class_upgrades(body, fs, symbols, out);
                    }
                }
                StatementKind::Block(body) => {
                    self.collect_class_upgrades(body, scope, symbols, out);
                }
                StatementKind::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    self.collect_class_upgrades(
                        std::slice::from_ref(&**then_branch),
                        scope,
                        symbols,
                        out,
                    );
                    if let Some(eb) = else_branch {
                        self.collect_class_upgrades(
                            std::slice::from_ref(&**eb),
                            scope,
                            symbols,
                            out,
                        );
                    }
                }
                StatementKind::Case {
                    when_branches,
                    otherwise,
                    ..
                } => {
                    for wb in when_branches {
                        self.collect_class_upgrades(&wb.body, scope, symbols, out);
                    }
                    if let Some(o) = otherwise {
                        self.collect_class_upgrades(o, scope, symbols, out);
                    }
                }
                StatementKind::Label { body, .. } => {
                    self.collect_class_upgrades(std::slice::from_ref(&**body), scope, symbols, out);
                }
                StatementKind::PreprocIf(pif) => {
                    self.collect_class_upgrades(&pif.then_branch, scope, symbols, out);
                    for (_, br) in &pif.elseif_branches {
                        self.collect_class_upgrades(br, scope, symbols, out);
                    }
                    if let Some(eb) = &pif.else_branch {
                        self.collect_class_upgrades(eb, scope, symbols, out);
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
        symbols: &SymbolTable,
        decl_name: &Identifier,
    ) -> Option<(SymbolId, ResolvedType)> {
        self.class_upgrade_in_ns(class_name, scope, NamespaceId::Values, symbols, decl_name)
    }

    fn class_upgrade_in_ns(
        &self,
        class_name: &str,
        scope: ScopeId,
        decl_ns: NamespaceId,
        symbols: &SymbolTable,
        decl_name: &Identifier,
    ) -> Option<(SymbolId, ResolvedType)> {
        let class_atom = fold_atom(class_name);
        let class_sym = self.tree.resolve(scope, NamespaceId::Types, &class_atom)?;
        let decl_atom = fold_atom(&decl_name.name);
        let decl_sym = self.tree.get(scope).get_in(decl_ns, &decl_atom)?;
        // Only upgrade if still Unknown.
        match symbols.get(decl_sym).data_type.as_ref() {
            Some(ResolvedType::Unknown) => Some((decl_sym, ResolvedType::Class(class_sym))),
            _ => None,
        }
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
            StatementKind::DefineParameter { .. } => {}
            StatementKind::DefineTempTable { fields, .. } => {
                for f in fields {
                    if let Some(init) = &f.initial_value {
                        for e in init {
                            self.walk_expression(e, scope, AccessMode::Read);
                        }
                    }
                }
            }
            StatementKind::DefineBuffer { .. } => {}
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
                get_body, set_body, ..
            } => {
                if let Some(body) = get_body
                    && let Some(gs) = self.find_child_scope(scope, stmt.id, ScopeKind::PropertyGet)
                {
                    self.walk_block(body, gs);
                }
                if let Some(body) = set_body
                    && let Some(ss) = self.find_child_scope(scope, stmt.id, ScopeKind::PropertySet)
                {
                    self.walk_block(body, ss);
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
            StatementKind::Class { body, .. } => {
                if let Some(cs) = self.find_child_scope(scope, stmt.id, ScopeKind::Class) {
                    self.walk_block(body, cs);
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
                    RunTarget::Literal(_) => {
                        // External procedure name — no statement-level NodeId
                        // to bind; lint rules treat as External when needed.
                    }
                    RunTarget::Dynamic(e) => {
                        self.walk_expression(e, scope, AccessMode::Read);
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
            StatementKind::Using { .. } => {}
            StatementKind::Leave(_) | StatementKind::Next(_) => {}
            StatementKind::IncludeReference { .. } | StatementKind::IncludeArgReference { .. } => {}
            StatementKind::Empty => {}
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
                object, arguments, ..
            } => {
                // Method is a cross-class member — External in v1. We don't
                // emit a reference entry for the call itself (the outer
                // Expression's NodeId is shared with no resolvable symbol);
                // recurse into `object` + `arguments` for their own idents.
                self.walk_expression(object, scope, AccessMode::Read);
                for a in arguments {
                    self.walk_expression(a, scope, AccessMode::Read);
                }
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.walk_expression(object, scope, AccessMode::Read);
                // Member is External; no reference entry in v1.
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
                match self.tree.resolve(scope, NamespaceId::Types, &atom) {
                    Some(sym) => {
                        self.references.insert(expr.id, Resolution::Resolved(sym));
                        self.bump_count(sym, AccessMode::Read);
                    }
                    None => {
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
                return;
            }
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
        let reason = if oxabl_lexer::is_builtin_function(&atom) {
            UnresolvedReason::External
        } else {
            UnresolvedReason::NotInScope
        };
        self.references
            .insert(expr_id, Resolution::Unresolved { name: atom, reason });
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
        let atom = fold_atom(&id.name);
        for &ns in namespaces {
            if let Some(sym) = self.tree.resolve(scope, ns, &atom) {
                self.bump_count(sym, mode);
                return;
            }
        }
    }

    /// Resolve `qualifier.field`. When the qualifier is a bare identifier,
    /// try `Buffers` (the common `table.field` / `buffer.field` case) and
    /// fall through to `Tables` (implicit default buffer). When schema is
    /// absent, the composite expression is `Unresolved { NoSchema }`.
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

                let field_atom = fold_atom(&field.name);
                // Field resolution: NoSchema (schema not loaded) vs
                // NotInScope (schema loaded, field absent) vs External
                // (schema loaded; no structured resolution in v1).
                let reason = if self.ctx.schema_loaded {
                    // v1: schema-backed field lookup requires knowing the
                    // target table for the buffer symbol. This indirection
                    // isn't cached on `Symbol` in v1, so we report External
                    // — the field is known-to-exist-or-not only via a
                    // schema query we haven't wired yet. `LINT0003` skips
                    // External by design.
                    UnresolvedReason::External
                } else {
                    UnresolvedReason::NoSchema
                };
                self.references.insert(
                    expr_id,
                    Resolution::Unresolved {
                        name: field_atom,
                        reason,
                    },
                );
                let _ = mode;
            }
            None => {
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
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn identifier_span(id: &Identifier) -> VirtualSpan {
    VirtualSpan::new(id.span.start, id.span.end)
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

fn fold_atom(s: &str) -> OxablAtom {
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
    use crate::{NamespaceId, SymbolKind};
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
        declare_pass(&stmts, &ctx)
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
        })
    }

    fn var_stmt_extent(name: &str, ty: DataType, extent: u32) -> Statement {
        stmt(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: Some(extent),
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
        })]);
        assert!(find_symbol(&tree, &symbols, NamespaceId::Buffers, "ttCust").is_some());
        let cust_num = find_symbol(&tree, &symbols, NamespaceId::Values, "custnum").unwrap();
        assert_eq!(cust_num.kind, SymbolKind::Field);
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
    fn nested_block_shadowing() {
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
        // Declaration in a nested block is not a duplicate of the file-
        // scope `x` — separate scope.
        assert!(
            diags.is_empty(),
            "shadowing across scopes must not emit SEM0001: {diags:?}"
        );
        // Root has outer x.
        assert!(
            tree.get(ScopeId::ROOT)
                .get_in(NamespaceId::Values, &fold_atom("x"))
                .is_some()
        );
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        assert!(block.get_in(NamespaceId::Values, &fold_atom("x")).is_some());
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
        })]);
        let (_, s) = symbols
            .iter()
            .find(|(_, s)| s.kind == SymbolKind::Dataset)
            .unwrap();
        assert!(s.flags.contains(SymbolFlags::SHARED));
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
        Statement::with_id(next_nid(), kind)
    }

    fn expr_n(kind: ExpressionKind) -> Expression {
        Expression::with_id(next_nid(), kind)
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
        })
    }

    fn var_stmt_with_init(name: &str, ty: DataType, init: Expression) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: id(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: Some(init),
            no_undo: false,
            extent: None,
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
        let (tree, mut symbols, _diags) = declare_pass(stmts, &ctx);
        let (refs, types, _rd) = resolve_pass(stmts, &ctx, &tree, &mut symbols);
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
        let (tree, mut symbols, _diags) = declare_pass(stmts, &ctx);
        let (refs, types, _rd) = resolve_pass(stmts, &ctx, &tree, &mut symbols);
        (tree, symbols, refs, types)
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
    fn resolve_inner_block_shadow_wins_over_outer() {
        // Outer `x: INTEGER`, block introduces a counter `x` — the use of
        // `x` inside the block must resolve to the block-scope counter, not
        // the outer variable.
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
        // The inner `x` is declared on the block scope with SymbolKind Variable.
        let block = tree
            .iter()
            .find(|(_, s)| s.kind == ScopeKind::Block)
            .unwrap()
            .1;
        let block_x = block
            .get_in(NamespaceId::Values, &fold_atom("x"))
            .expect("block-scope x");
        assert_eq!(*sym, block_x);
        // And it's Integer (counter), not Character.
        assert_eq!(
            symbols.get(*sym).data_type,
            Some(ResolvedType::Primitive(crate::PrimitiveTy::Integer))
        );
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
            }),
            stmt_n(StatementKind::ExpressionStatement(fa)),
        ];
        let (_t, symbols, refs, _) = run_full_with_schema_loaded(&stmts, true);
        let Resolution::Resolved(sym) = resolution_of(&refs, qual_id) else {
            panic!("qualifier should resolve to buffer");
        };
        assert_eq!(symbols.get(*sym).kind, SymbolKind::Buffer);
        // Field stays External (schema-backed field lookup not wired in v1).
        assert!(matches!(
            resolution_of(&refs, fa_id),
            Resolution::Unresolved {
                reason: UnresolvedReason::External,
                ..
            }
        ));
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
                target: RunTarget::Literal("proc".into()),
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
                target: RunTarget::Literal("proc".into()),
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
        let (tree, mut symbols, _d) = declare_pass(&stmts, &ctx);
        let _ = resolve_pass(&stmts, &ctx, &tree, &mut symbols);
        let _ = resolve_pass(&stmts, &ctx, &tree, &mut symbols);
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
}
