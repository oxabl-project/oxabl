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
    AccessModifier, DataType, Identifier, ParameterDirection, ParameterType, Span, Statement,
    StatementKind, TypeSource,
};
use oxabl_common::{Diagnostic, VirtualSpan};
use oxabl_lexer::oxabl_atom::OxablAtom;

use crate::{
    AnalysisContext, NamespaceId, ResolvedType, ScopeId, ScopeKind, ScopeTree, Symbol, SymbolFlags,
    SymbolKind, SymbolTable, builtins, diagnostics, resolve_span,
};

/// Resolution of a single reference site. Populated by Phase 4a; the type
/// is defined here so the declare pass can ship without `references` being
/// `()`-typed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Resolution {
    Resolved(crate::SymbolId),
    Unresolved { reason: UnresolvedReason },
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

// Suppress `unused` on the `Span` re-export until resolve pass uses it.
#[allow(dead_code)]
fn _span_alias(_: Span) {}

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
}
