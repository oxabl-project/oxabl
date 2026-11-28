//! Type-check pass — third of the three semantic passes.
//!
//! Walks every expression in the program and synthesizes a
//! [`ResolvedType`] into the `types` side table keyed by the expression's
//! [`NodeId`]. Declaration entries (variables, parameters, functions,
//! properties) were already populated by the resolve pass; this pass only
//! extends the side table with expression bodies.
//!
//! v1 emits **no diagnostics** from the semantic layer for type mismatches
//! — `LINT0004` is the single user-facing channel, and the lint rule reads
//! the populated `types` side table plus `Symbol::data_type` to surface
//! mismatches. See plan §"Type-mismatch diagnostics emit only from lint".
//!
//! The pass is **bottom-up type synthesis**: literals produce a type; an
//! identifier inherits its resolved symbol's type; binary/unary operators
//! consult the tables in [`crate::operators`]; unresolved references
//! default to `Unknown` so cascading diagnostics are suppressed.
//!
//! A member through a receiver (`obj:Name`) has the type of the member the
//! resolve pass found. A method call (`obj:Greet(1)`, or `Greet(1)` inside the
//! class) has the type of the overload its argument types pick
//! ([`crate::overload`]). A reference the resolve pass marked as untyped
//! (`SymbolTable::is_untyped_member`) is `Unknown`. See
//! `docs/design/class-members.md`.

use oxabl_ast::{
    AssignPair, AstPools, CreateTarget, ExprId, ExprList, Expression, ExpressionKind, Literal,
    OnAction, OnKind, RunTarget, Statement, StatementKind, StreamOperation, SubscribeTarget,
};
use oxabl_common::Diagnostic;
use smallvec::SmallVec;

use crate::{
    AnalysisContext, NodeIndexVec, PrimitiveTy, Resolution, ResolvedType, ScopeId, ScopeKind,
    ScopeTree, SymbolKind, SymbolTable, WorkspaceIndex, operators,
};

/// Run the type-check pass over `program` given a populated scope tree,
/// symbol table, and reference side table from declare + resolve. Extends
/// the `types` side table in place with a [`ResolvedType`] for every
/// expression NodeId.
///
/// Does not emit diagnostics. Evidence of type mismatches lives in the
/// populated `types` table, ready for `LINT0004` to consume.
pub fn check_pass(
    program: &[Statement],
    ctx: &AnalysisContext,
    tree: &ScopeTree,
    symbols: &SymbolTable,
    references: &NodeIndexVec<Resolution>,
    types: &mut NodeIndexVec<ResolvedType>,
) -> Vec<Diagnostic> {
    let mut walker = CheckWalker {
        pools: ctx.pools,
        index: ctx.index,
        tree,
        symbols,
        references,
        types,
    };
    walker.walk_block(program, ScopeId::ROOT);
    Vec::new()
}

struct CheckWalker<'a> {
    pools: &'a AstPools,
    index: &'a dyn WorkspaceIndex,
    tree: &'a ScopeTree,
    symbols: &'a SymbolTable,
    references: &'a NodeIndexVec<Resolution>,
    types: &'a mut NodeIndexVec<ResolvedType>,
}

impl<'a> CheckWalker<'a> {
    fn walk_block(&mut self, stmts: &[Statement], scope: ScopeId) {
        for stmt in stmts {
            self.index.checkpoint();
            self.walk_statement(stmt, scope);
        }
    }

    fn walk_statement(&mut self, stmt: &Statement, scope: ScopeId) {
        match &stmt.kind {
            // ---- Declarations with initializers --------------------------
            StatementKind::VariableDeclaration { initial_value, .. } => {
                if let Some(e) = *initial_value {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::DefineParameter { .. } => {}
            StatementKind::DefineTempTable { fields, .. } => {
                for f in fields {
                    if let Some(init) = f.initial_value {
                        for &e in self.pools.exprs(init) {
                            self.check_expression(e, scope);
                        }
                    }
                }
            }
            StatementKind::DefineBuffer { .. } => {}
            StatementKind::DefineDataset { .. } => {}
            StatementKind::DefineDataSource { .. } => {}
            StatementKind::DefineStream { .. } => {}
            StatementKind::DefineFrame { .. } | StatementKind::DefineWidget { .. } => {}
            StatementKind::DefineEvent { parameters, .. } => {
                let s = self
                    .find_child_scope(scope, stmt, ScopeKind::Method)
                    .unwrap_or(scope);
                self.walk_block(parameters, s);
            }
            StatementKind::Property {
                get_body, set_body, ..
            } => {
                if let Some(body) = get_body
                    && let Some(gs) = self.find_child_scope(scope, stmt, ScopeKind::PropertyGet)
                {
                    self.walk_block(body, gs);
                }
                if let Some(body) = set_body
                    && let Some(ss) = self.find_child_scope(scope, stmt, ScopeKind::PropertySet)
                {
                    self.walk_block(body, ss);
                }
            }

            // ---- Scope-opening declarations ------------------------------
            StatementKind::Procedure { body, .. } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Procedure) {
                    self.walk_block(body, s);
                }
            }
            StatementKind::Function { body, .. } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Function) {
                    self.walk_block(body, s);
                }
            }
            StatementKind::Class { body, .. } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Class) {
                    self.walk_block(body, s);
                }
            }
            StatementKind::Interface { body, .. } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Interface) {
                    self.walk_block(body, s);
                }
            }
            StatementKind::Method {
                parameters, body, ..
            } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Method) {
                    self.walk_block(parameters, s);
                    self.walk_block(body, s);
                }
            }
            StatementKind::Constructor {
                parameters, body, ..
            } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Constructor) {
                    self.walk_block(parameters, s);
                    self.walk_block(body, s);
                }
            }
            StatementKind::Destructor { body } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Destructor) {
                    self.walk_block(body, s);
                }
            }
            StatementKind::Catch { body, .. } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Catch) {
                    self.walk_block(body, s);
                }
            }
            StatementKind::Finally { body } => {
                if let Some(s) = self.find_child_scope(scope, stmt, ScopeKind::Finally) {
                    self.walk_block(body, s);
                }
            }

            // ---- Block / control flow ------------------------------------
            StatementKind::Do {
                from,
                to,
                by,
                while_condition,
                on_actions,
                body,
                ..
            } => {
                let bs = self
                    .find_child_scope(scope, stmt, ScopeKind::Block)
                    .unwrap_or(scope);
                for e in [*from, *to, *by, *while_condition].into_iter().flatten() {
                    self.check_expression(e, bs);
                }
                for &e in self.pools.exprs(*on_actions) {
                    self.check_expression(e, bs);
                }
                self.walk_block(body, bs);
            }
            StatementKind::Repeat {
                while_condition,
                on_actions,
                body,
            } => {
                let bs = self
                    .find_child_scope(scope, stmt, ScopeKind::Block)
                    .unwrap_or(scope);
                if let Some(e) = *while_condition {
                    self.check_expression(e, bs);
                }
                for &e in self.pools.exprs(*on_actions) {
                    self.check_expression(e, bs);
                }
                self.walk_block(body, bs);
            }
            StatementKind::ForEach {
                where_clause,
                on_actions,
                body,
                ..
            } => {
                let bs = self
                    .find_child_scope(scope, stmt, ScopeKind::Block)
                    .unwrap_or(scope);
                if let Some(w) = *where_clause {
                    self.check_expression(w, bs);
                }
                for &e in self.pools.exprs(*on_actions) {
                    self.check_expression(e, bs);
                }
                self.walk_block(body, bs);
            }
            StatementKind::Find {
                key_value,
                where_clause,
                ..
            } => {
                if let Some(k) = *key_value {
                    self.check_expression(k, scope);
                }
                if let Some(w) = *where_clause {
                    self.check_expression(w, scope);
                }
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expression(*condition, scope);
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
                self.check_expression(*expression, scope);
                for wb in when_branches {
                    for &v in self.pools.exprs(wb.values) {
                        self.check_expression(v, scope);
                    }
                    self.walk_block(&wb.body, scope);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o, scope);
                }
            }
            StatementKind::Block(body) | StatementKind::VariableDeclarationList(body) => self.walk_block(body, scope),
            StatementKind::Label { body, .. } => self.walk_statement(body, scope),

            // ---- Trigger / preprocessor ----------------------------------
            StatementKind::On { kind } => match kind {
                OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => match action {
                    OnAction::Block(body) => {
                        let s = self
                            .find_child_scope(scope, stmt, ScopeKind::Trigger)
                            .unwrap_or(scope);
                        self.walk_statement(body, s);
                    }
                    OnAction::PersistentRun { procedure, in_handle, arguments } => {
                        for arg in procedure
                            .expression()
                            .into_iter()
                            .chain(*in_handle)
                            .chain(self.pools.exprs(*arguments).iter().copied())
                        {
                            self.check_expression(arg, scope);
                        }
                    }
                    OnAction::Revert => {}
                },
                OnKind::KeyRemap { .. } => {}
            },
            StatementKind::TriggerProcedure { .. } => {}
            StatementKind::PreprocIf(pif) => {
                self.check_expression(pif.condition, scope);
                self.walk_block(&pif.then_branch, scope);
                for (c, br) in &pif.elseif_branches {
                    self.check_expression(*c, scope);
                    self.walk_block(br, scope);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb, scope);
                }
            }
            StatementKind::PreprocDefine { .. } | StatementKind::PreprocUndefine { .. } => {}
            StatementKind::PreprocMessage { expression } => {
                self.check_expression(*expression, scope);
            }

            // ---- Assignment / expression forms ---------------------------
            StatementKind::Assignment { target, value } => {
                self.check_expression(*target, scope);
                self.check_expression(*value, scope);
            }
            StatementKind::Assign { assignments } => {
                for AssignPair { target, value } in assignments {
                    self.check_expression(*target, scope);
                    self.check_expression(*value, scope);
                }
            }
            StatementKind::ExpressionStatement(expr) => {
                self.check_expression(*expr, scope);
            }
            StatementKind::Return(opt) => {
                if let Some(e) = *opt {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Throw(expr) => {
                self.check_expression(*expr, scope);
            }

            // ---- Output / display ----------------------------------------
            StatementKind::Display { items, .. } => {
                for item in items {
                    self.check_expression(item.expression, scope);
                    if let Some(w) = item.when_condition {
                        self.check_expression(w, scope);
                    }
                }
            }
            StatementKind::Message { items, .. }
            | StatementKind::Put { items, .. }
            | StatementKind::Form { items, .. }
            | StatementKind::Enable { items }
            | StatementKind::WaitFor { widgets: items }
            | StatementKind::Disable { items }
            | StatementKind::Hide { items, .. }
            | StatementKind::View { items, .. } => {
                for &e in self.pools.exprs(*items) {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Export {
                delimiter, items, ..
            }
            | StatementKind::Import {
                delimiter,
                targets: items,
                ..
            } => {
                for e in (*delimiter)
                    .into_iter()
                    .chain(self.pools.exprs(*items).iter().copied())
                {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Update {
                targets, options, ..
            }
            | StatementKind::Set {
                targets, options, ..
            }
            | StatementKind::PromptFor {
                targets, options, ..
            } => {
                for &e in self
                    .pools
                    .exprs(*targets)
                    .iter()
                    .chain(self.pools.exprs(*options))
                {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Pause {
                duration: a,
                message: b,
            }
            | StatementKind::Apply {
                event: a,
                target: b,
            } => {
                for e in (*a).into_iter().chain(*b) {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Down { lines, .. } | StatementKind::Up { lines, .. } => {
                if let Some(e) = *lines {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Get { query, .. } => {
                self.check_expression(*query, scope);
            }
            StatementKind::CopyLob {
                source,
                target,
                options,
                ..
            } => {
                for e in [*source, *target]
                    .into_iter()
                    .chain(self.pools.exprs(*options).iter().copied())
                {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Color { items, .. } => {
                for &e in self.pools.exprs(*items) {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Accumulate { expression, breaks } => {
                for e in std::iter::once(*expression)
                    .chain(self.pools.exprs(*breaks).iter().copied())
                {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Reposition {
                query, operands, ..
            } => {
                for e in std::iter::once(*query)
                    .chain(self.pools.exprs(*operands).iter().copied())
                {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Close {
                target,
                status,
                handle,
                ..
            } => {
                for e in std::iter::once(*target).chain(*status).chain(*handle) {
                    self.check_expression(e, scope);
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
                if let RunTarget::Dynamic(e) = target {
                    self.check_expression(*e, scope);
                }
                for arg in arguments {
                    self.check_expression(arg.expression, scope);
                }
                for e in [
                    *in_handle,
                    *persistent_handle,
                    *async_handle,
                    *event_procedure,
                ]
                .into_iter()
                .flatten()
                {
                    self.check_expression(e, scope);
                }
            }
            // Typed like any other expression statement: the operand is an
            // expression, and a handle-typed one is what makes `DELETE OBJECT
            // ttbl:HANDLE` legal.
            StatementKind::DeleteObject {
                target,
                extra_targets,
                ..
            } => {
                self.check_expression(*target, scope);
                for &t in self.pools.exprs(*extra_targets) {
                    self.check_expression(t, scope);
                }
            }
            StatementKind::CreateWidgetPool { name, .. }
            | StatementKind::DeleteWidgetPool { name, .. } => {
                if let Some(n) = *name {
                    self.check_expression(n, scope);
                }
            }
            StatementKind::ReleaseObject { target, .. } => {
                self.check_expression(*target, scope);
            }
            StatementKind::Delete { .. }
            | StatementKind::Release { .. }
            | StatementKind::Validate { .. } => {}
            StatementKind::BufferCopy { assignments, .. } => {
                for AssignPair { target, value } in assignments {
                    self.check_expression(*target, scope);
                    self.check_expression(*value, scope);
                }
            }
            StatementKind::BufferCompare { .. } => {}
            StatementKind::Create { target, .. } => {
                if let CreateTarget::Handle {
                    handle,
                    widget_pool,
                    class,
                    assignments,
                    triggers,
                    connect,
                    for_table,
                    ..
                } = target
                {
                    self.check_expression(*handle, scope);
                    if let Some(t) = *for_table {
                        self.check_expression(t, scope);
                    }
                    for e in [*widget_pool, *class].into_iter().flatten() {
                        self.check_expression(e, scope);
                    }
                    if let Some(file) = connect.as_ref().and_then(|c| c.file) {
                        self.check_expression(file, scope);
                    }
                    for a in assignments {
                        self.check_expression(a.value, scope);
                    }
                    if !triggers.is_empty() {
                        let ts = self
                            .find_child_scope(scope, stmt, ScopeKind::Trigger)
                            .unwrap_or(scope);
                        for trigger in triggers {
                            match &trigger.action {
                                OnAction::Block(body) => self.walk_statement(body, ts),
                                OnAction::PersistentRun { procedure, in_handle, arguments } => {
                                    for arg in procedure
                                        .expression()
                                        .into_iter()
                                        .chain(*in_handle)
                                        .chain(self.pools.exprs(*arguments).iter().copied())
                                    {
                                        self.check_expression(arg, scope);
                                    }
                                }
                                OnAction::Revert => {}
                            }
                        }
                    }
                }
            }

            StatementKind::Publish {
                event_name,
                from_handle,
                arguments,
            } => {
                self.check_expression(*event_name, scope);
                if let Some(fh) = *from_handle {
                    self.check_expression(fh, scope);
                }
                for arg in arguments {
                    self.check_expression(arg.expression, scope);
                }
            }
            StatementKind::Subscribe {
                subscriber,
                event_name,
                target,
                ..
            } => {
                if let Some(s) = *subscriber {
                    self.check_expression(s, scope);
                }
                self.check_expression(*event_name, scope);
                if let SubscribeTarget::InHandle(h) = target {
                    self.check_expression(*h, scope);
                }
            }
            StatementKind::Unsubscribe {
                subscriber,
                event_name,
                in_handle,
            } => {
                if let Some(s) = *subscriber {
                    self.check_expression(s, scope);
                }
                if let Some(e) = *event_name {
                    self.check_expression(e, scope);
                }
                if let Some(h) = *in_handle {
                    self.check_expression(h, scope);
                }
            }

            StatementKind::StreamIo { operation, .. } => match operation {
                StreamOperation::From(target) | StreamOperation::To { target, .. } => {
                    if let Some(e) = target.expression() {
                        self.check_expression(e, scope);
                    }
                }
                StreamOperation::Through(e) => {
                    self.check_expression(*e, scope);
                }
                StreamOperation::Close => {}
            },

            StatementKind::Using { .. }
            | StatementKind::Leave(_)
            | StatementKind::Next(_)
            | StatementKind::IncludeReference { .. }
            | StatementKind::IncludeArgReference { .. }
            | StatementKind::Empty
            // A frame name is not a typed expression.
            | StatementKind::Clear { .. }
            // Annotation names and attributes are metadata, not expressions.
            | StatementKind::Annotation(_)
            // Recognized-but-unmodelled: the harvested names are lexical
            // candidates, not typed expressions, so there is nothing here to
            // type-check.
            | StatementKind::Skipped { .. } => {}
        }
    }

    /// Synthesize the type of `expr` bottom-up and write it to
    /// `types[expr.id]`. Returns the synthesized type so callers can feed
    /// it into the operator tables without re-reading.
    #[allow(clippy::only_used_in_recursion)]
    fn check_expression(&mut self, expr_id: ExprId, scope: ScopeId) -> ResolvedType {
        let expr = self.pools.expr(expr_id);
        let ty = match &expr.kind {
            ExpressionKind::Literal(l) => literal_type(l),

            ExpressionKind::Identifier(_) => match self.symbols.expr_class(expr.id) {
                // `THIS-OBJECT` in a class body.
                Some(class) => ResolvedType::Class(class),
                None => self.type_from_reference(expr),
            },

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
                let lt = self.check_expression(*l, scope);
                let rt = self.check_expression(*r, scope);
                operators::binary_op_result(&expr.kind, &lt, &rt)
            }

            ExpressionKind::Negate(e) => {
                let t = self.check_expression(*e, scope);
                operators::unary_negate_result(&t)
            }
            ExpressionKind::Not(e) => {
                let t = self.check_expression(*e, scope);
                operators::unary_not_result(&t)
            }
            ExpressionKind::IfThenElse(c, t, f) => {
                self.check_expression(*c, scope);
                let tt = self.check_expression(*t, scope);
                let ft = self.check_expression(*f, scope);
                // Widen then/else to a common type. If incompatible,
                // result is Unknown — ternary is a late-checked place.
                widen_types(&tt, &ft)
            }

            ExpressionKind::FunctionCall { arguments, .. } => {
                let args = self.check_arguments(*arguments, scope);
                if let Some(class) = self.symbols.expr_class(expr.id) {
                    // `CAST(x, Cls)`.
                    ResolvedType::Class(class)
                } else if let Some(Resolution::Resolved(sym)) = self.references.get(expr.id)
                    && self.symbols.is_method(*sym)
                {
                    // A method called inside its class: the same pick as
                    // `THIS-OBJECT:Name(...)`.
                    self.method_call_type(expr, *sym, &args)
                } else {
                    // The call's return type comes from the resolved symbol,
                    // populated by resolve. If unresolved, Unknown.
                    self.type_from_reference(expr)
                }
            }
            ExpressionKind::MethodCall {
                object, arguments, ..
            } => {
                self.check_expression(*object, scope);
                let args = self.check_arguments(*arguments, scope);
                match self.references.get(expr.id) {
                    Some(Resolution::Resolved(sym)) => self.method_call_type(expr, *sym, &args),
                    // A receiver whose class oxabl does not know.
                    _ => ResolvedType::Unknown,
                }
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.check_expression(*object, scope);
                if self.symbols.is_untyped_member(expr.id) {
                    ResolvedType::Unknown
                } else {
                    self.type_from_reference(expr)
                }
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let at = self.check_expression(*array, scope);
                self.check_expression(*index, scope);
                match at {
                    ResolvedType::Array { element, .. } => *element,
                    ResolvedType::Unknown => ResolvedType::Unknown,
                    _ => ResolvedType::Error,
                }
            }
            ExpressionKind::FieldAccess { qualifier, .. } => {
                self.check_expression(*qualifier, scope);
                // The resolve pass records the field's resolution on the
                // composite node: a validated schema field resolves to a
                // synthesized `Field` symbol whose `data_type` is the schema
                // primitive, so the type flows out of the reference.
                self.type_from_reference(expr)
            }
            ExpressionKind::New { arguments, .. } => {
                for &a in self.pools.exprs(*arguments) {
                    self.check_expression(a, scope);
                }
                self.type_from_reference(expr)
            }
            ExpressionKind::CanFind { where_clause, .. } => {
                if let Some(w) = *where_clause {
                    self.check_expression(w, scope);
                }
                ResolvedType::Primitive(PrimitiveTy::Logical)
            }

            ExpressionKind::IncludeReference { .. }
            | ExpressionKind::IncludeArgReference { .. }
            | ExpressionKind::PreprocReference(_) => ResolvedType::Unknown,

            ExpressionKind::PreprocIf(id) => {
                let pif = self.pools.preproc_if(*id);
                self.check_expression(pif.condition, scope);
                let tt = self.check_expression(pif.then_branch, scope);
                for (c, br) in &pif.elseif_branches {
                    self.check_expression(*c, scope);
                    let bt = self.check_expression(*br, scope);
                    let _ = bt;
                }
                if let Some(eb) = pif.else_branch {
                    let et = self.check_expression(eb, scope);
                    widen_types(&tt, &et)
                } else {
                    tt
                }
            }
        };

        self.types.insert(expr.id, ty.clone());
        ty
    }

    /// Check each argument and collect its type.
    fn check_arguments(
        &mut self,
        arguments: ExprList,
        scope: ScopeId,
    ) -> SmallVec<[ResolvedType; 4]> {
        self.pools
            .exprs(arguments)
            .iter()
            .map(|&a| self.check_expression(a, scope))
            .collect()
    }

    /// The type of a call of the method overload set `anchor`: the pick over
    /// its candidates, or `Unknown` when the resolve pass marked the call.
    fn method_call_type(
        &self,
        expr: &Expression,
        anchor: crate::SymbolId,
        args: &[ResolvedType],
    ) -> ResolvedType {
        if self.symbols.is_untyped_member(expr.id) {
            return ResolvedType::Unknown;
        }
        crate::overload::call_type(self.symbols, anchor, args)
    }

    fn type_from_reference(&self, expr: &Expression) -> ResolvedType {
        match self.references.get(expr.id) {
            Some(Resolution::Resolved(sym)) => {
                // An overload set is judged by the type its members agree on,
                // and only goes to the lattice bottom when they disagree —
                // overloading on parameters alone is the common ABL idiom and
                // stays checkable.
                if let Some(ty) = self.symbols.overload_set_data_type(*sym) {
                    return ty;
                }
                let symbol = self.symbols.get(*sym);
                // Class / Interface symbols carry no `data_type`; an
                // expression that resolves *to* a class symbol (e.g. `NEW
                // Foo(...)`) has type `Class(Foo)`.
                match symbol.kind {
                    // A class symbol synthesized from the workspace index types
                    // as its class, exactly like a locally declared one. It used
                    // to type as the lattice bottom so that attaching an index
                    // could add no finding; judging the cross-file population is
                    // what re-enables the three assignment shapes that arm held
                    // back — a subclass into a parent (which `ClassLattice`
                    // widening already made correct), a class into a primitive,
                    // and a primitive into a class-typed variable. The last two
                    // are genuinely mismatched code.
                    SymbolKind::Class | SymbolKind::Interface => ResolvedType::Class(*sym),
                    SymbolKind::Buffer | SymbolKind::TempTable => ResolvedType::Buffer(*sym),
                    _ => symbol.data_type.clone().unwrap_or(ResolvedType::Unknown),
                }
            }
            _ => ResolvedType::Unknown,
        }
    }

    /// The child scope of `parent` that declare made with
    /// `owner_node == stmt.id && kind == kind`.
    fn find_child_scope(
        &self,
        parent: ScopeId,
        stmt: &Statement,
        kind: ScopeKind,
    ) -> Option<ScopeId> {
        self.tree.child(parent, stmt.id, kind)
    }
}

fn literal_type(lit: &Literal) -> ResolvedType {
    use PrimitiveTy::*;
    match lit {
        Literal::Integer(n) => {
            // ABL integer literals default to INTEGER unless the value
            // overflows 32-bit signed range; parser preserves the raw i64.
            if n.value > i32::MAX as i64 || n.value < i32::MIN as i64 {
                ResolvedType::Primitive(Int64)
            } else {
                ResolvedType::Primitive(Integer)
            }
        }
        Literal::Decimal(_) => ResolvedType::Primitive(Decimal),
        Literal::String(_) => ResolvedType::Primitive(Character),
        Literal::Boolean(_) => ResolvedType::Primitive(Logical),
        Literal::Date(_) => ResolvedType::Primitive(Date),
        Literal::Unknown(_) => ResolvedType::Unknown,
    }
}

/// Widen two types to a common type. If incompatible, Unknown — the type-
/// check pass does not cascade errors; `LINT0004` handles mismatch diagnostics.
fn widen_types(a: &ResolvedType, b: &ResolvedType) -> ResolvedType {
    use ResolvedType::*;
    if matches!(a, Unknown) {
        return b.clone();
    }
    if matches!(b, Unknown) {
        return a.clone();
    }
    if a == b {
        return a.clone();
    }
    if let (Primitive(pa), Primitive(pb)) = (a, b)
        && let Some(w) = crate::coercion::widen_primitive(*pa, *pb)
    {
        return Primitive(w);
    }
    Unknown
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The pools of a walker that reads no name.
    static NO_POOLS: AstPools = AstPools::new();

    #[test]
    fn literal_integer_in_i32_range_is_integer() {
        use oxabl_ast::{IntegerLiteral, Span};
        let lit = Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: 42,
        });
        assert_eq!(
            literal_type(&lit),
            ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn literal_integer_overflow_is_int64() {
        use oxabl_ast::{IntegerLiteral, Span};
        let lit = Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: (i32::MAX as i64) + 1,
        });
        assert_eq!(
            literal_type(&lit),
            ResolvedType::Primitive(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn literal_string_is_character() {
        use oxabl_ast::{NameId, Span, StringLiteral};
        let lit = Literal::String(StringLiteral {
            span: Span { start: 0, end: 1 },
            value: NameId::DUMMY,
        });
        assert_eq!(
            literal_type(&lit),
            ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn literal_boolean_is_logical() {
        use oxabl_ast::{BooleanLiteral, Span};
        let lit = Literal::Boolean(BooleanLiteral {
            span: Span { start: 0, end: 4 },
            value: true,
        });
        assert_eq!(
            literal_type(&lit),
            ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn literal_unknown_is_unknown() {
        use oxabl_ast::{Span, UnknownLiteral};
        let lit = Literal::Unknown(UnknownLiteral {
            span: Span { start: 0, end: 1 },
        });
        assert_eq!(literal_type(&lit), ResolvedType::Unknown);
    }

    #[test]
    fn widen_same_type() {
        let t = ResolvedType::Primitive(PrimitiveTy::Integer);
        assert_eq!(widen_types(&t, &t), t);
    }

    #[test]
    fn widen_integer_and_decimal_is_decimal() {
        assert_eq!(
            widen_types(
                &ResolvedType::Primitive(PrimitiveTy::Integer),
                &ResolvedType::Primitive(PrimitiveTy::Decimal)
            ),
            ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn widen_with_unknown_returns_other() {
        assert_eq!(
            widen_types(
                &ResolvedType::Unknown,
                &ResolvedType::Primitive(PrimitiveTy::Integer)
            ),
            ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn widen_incompatible_is_unknown() {
        assert_eq!(
            widen_types(
                &ResolvedType::Primitive(PrimitiveTy::Logical),
                &ResolvedType::Primitive(PrimitiveTy::Integer)
            ),
            ResolvedType::Unknown
        );
    }

    // =======================================================================
    // End-to-end check pass
    // =======================================================================
    //
    // These tests run the full `analyze_file` pipeline and inspect the
    // resulting `types` side table at expression NodeIds. They pin the
    // contract that `check_pass` populates every expression site with a
    // synthesized type — the raw material `LINT0004` consumes.

    use crate::{AnalysisContext, analyze_file};
    use oxabl_ast::{
        AstPools, BooleanLiteral, BufferTarget, DataType, DecimalLiteral, ExprId, ExprList,
        Expression, ExpressionKind, Identifier, IntegerLiteral, NameId, NodeId, ParameterDirection,
        ParameterType, Span, Statement, StatementKind, StringLiteral, TypeSource, UnknownLiteral,
        XmlSerializeOptions,
    };
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use std::sync::atomic::{AtomicU32, Ordering};

    fn next_nid() -> NodeId {
        static C: AtomicU32 = AtomicU32::new(1);
        NodeId::from_u32(C.fetch_add(1, Ordering::Relaxed))
    }

    struct B {
        pools: AstPools,
    }

    impl B {
        fn new() -> Self {
            Self {
                pools: AstPools::new(),
            }
        }

        fn name(&mut self, s: &str) -> NameId {
            self.pools.names.push(s)
        }

        fn ident(&mut self, name: &str) -> Identifier {
            Identifier {
                span: Span {
                    start: 0,
                    end: name.len() as u32,
                },
                name: self.name(name),
            }
        }

        fn expr_n(&mut self, kind: ExpressionKind) -> ExprId {
            self.pools.exprs.push(Expression::with_id(
                next_nid(),
                oxabl_ast::Span::DUMMY,
                kind,
            ))
        }

        fn nid(&self, expr: ExprId) -> NodeId {
            self.pools.expr(expr).id
        }

        fn int_lit(&mut self, v: i64) -> ExprId {
            self.expr_n(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
                span: Span { start: 0, end: 1 },
                value: v,
            })))
        }

        fn dec_lit(&mut self, d: &str) -> ExprId {
            use std::str::FromStr;
            self.expr_n(ExpressionKind::Literal(Literal::Decimal(DecimalLiteral {
                span: Span { start: 0, end: 1 },
                value: ::rust_decimal::Decimal::from_str(d).unwrap(),
            })))
        }

        fn str_lit(&mut self, s: &str) -> ExprId {
            let value = self.name(s);
            self.expr_n(ExpressionKind::Literal(Literal::String(StringLiteral {
                span: Span { start: 0, end: 1 },
                value,
            })))
        }

        fn bool_lit(&mut self, b: bool) -> ExprId {
            self.expr_n(ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
                span: Span { start: 0, end: 1 },
                value: b,
            })))
        }

        fn unknown_lit(&mut self) -> ExprId {
            self.expr_n(ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
                span: Span { start: 0, end: 1 },
            })))
        }

        fn id_expr(&mut self, name: &str) -> ExprId {
            let id = self.ident(name);
            self.expr_n(ExpressionKind::Identifier(id))
        }

        fn var_decl(&mut self, name: &str, ty: DataType) -> Statement {
            let name = self.ident(name);
            stmt_n(StatementKind::VariableDeclaration {
                access: None,
                is_static: false,
                name,
                type_source: TypeSource::Explicit(ty),
                initial_value: None,
                no_undo: false,
                extent: None,
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            })
        }

        fn analyze(&self, stmts: Vec<Statement>) -> crate::Semantic {
            let schema = Schema::empty();
            let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema, &self.pools);
            analyze_file(&stmts, &ctx)
        }
    }

    fn stmt_n(kind: StatementKind) -> Statement {
        Statement::with_id(next_nid(), oxabl_ast::Span::DUMMY, kind)
    }

    fn ty_of(sem: &crate::Semantic, id: NodeId) -> &ResolvedType {
        sem.types
            .get(id)
            .unwrap_or_else(|| panic!("expected type at NodeId {id:?}"))
    }

    #[test]
    fn check_integer_literal() {
        let mut b = B::new();
        let lit = b.int_lit(5);
        let lid = b.nid(lit);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_decimal_literal() {
        let mut b = B::new();
        let lit = b.dec_lit("3.14");
        let lid = b.nid(lit);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_string_literal() {
        let mut b = B::new();
        let lit = b.str_lit("hi");
        let lid = b.nid(lit);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn check_boolean_literal() {
        let mut b = B::new();
        let lit = b.bool_lit(true);
        let lid = b.nid(lit);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_unknown_literal_is_unknown_lattice_bottom() {
        let mut b = B::new();
        let lit = b.unknown_lit();
        let lid = b.nid(lit);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(ty_of(&sem, lid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_identifier_takes_symbol_type() {
        let mut b = B::new();
        let use_x = b.id_expr("x");
        let uid = b.nid(use_x);
        let decl = b.var_decl("x", DataType::Character);
        let sem = b.analyze(vec![
            decl,
            stmt_n(StatementKind::ExpressionStatement(use_x)),
        ]);
        assert_eq!(
            ty_of(&sem, uid),
            &ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn check_unresolved_identifier_is_unknown() {
        let mut b = B::new();
        let ghost = b.id_expr("ghost");
        let gid = b.nid(ghost);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(ghost))]);
        assert_eq!(ty_of(&sem, gid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_integer_plus_integer_is_integer() {
        let mut b = B::new();
        let l = b.int_lit(1);
        let r = b.int_lit(2);
        let sum = b.expr_n(ExpressionKind::Add(l, r));
        let sid = b.nid(sum);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(
            ty_of(&sem, sid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_integer_plus_decimal_is_decimal() {
        let mut b = B::new();
        let l = b.int_lit(1);
        let r = b.dec_lit("2.0");
        let sum = b.expr_n(ExpressionKind::Add(l, r));
        let sid = b.nid(sum);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(
            ty_of(&sem, sid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_integer_div_integer_is_decimal() {
        let mut b = B::new();
        let l = b.int_lit(10);
        let r = b.int_lit(2);
        let div = b.expr_n(ExpressionKind::Divide(l, r));
        let did = b.nid(div);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(div))]);
        assert_eq!(
            ty_of(&sem, did),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_string_concat_is_character() {
        let mut b = B::new();
        let l = b.str_lit("a");
        let r = b.str_lit("b");
        let cat = b.expr_n(ExpressionKind::Add(l, r));
        let cid = b.nid(cat);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(cat))]);
        assert_eq!(
            ty_of(&sem, cid),
            &ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn check_comparison_is_logical() {
        let mut b = B::new();
        let l = b.int_lit(1);
        let r = b.int_lit(2);
        let eq = b.expr_n(ExpressionKind::Equal(l, r));
        let eid = b.nid(eq);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(eq))]);
        assert_eq!(
            ty_of(&sem, eid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_and_of_logicals_is_logical() {
        let mut b = B::new();
        let l = b.bool_lit(true);
        let r = b.bool_lit(false);
        let and_e = b.expr_n(ExpressionKind::And(l, r));
        let aid = b.nid(and_e);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(and_e))]);
        assert_eq!(
            ty_of(&sem, aid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_begins_of_chars_is_logical() {
        let mut b = B::new();
        let l = b.str_lit("hello");
        let r = b.str_lit("he");
        let be = b.expr_n(ExpressionKind::Begins(l, r));
        let bid = b.nid(be);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(be))]);
        assert_eq!(
            ty_of(&sem, bid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_negate_integer_is_integer() {
        let mut b = B::new();
        let e = b.int_lit(5);
        let neg = b.expr_n(ExpressionKind::Negate(e));
        let nid = b.nid(neg);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(neg))]);
        assert_eq!(
            ty_of(&sem, nid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_not_of_logical_is_logical() {
        let mut b = B::new();
        let e = b.bool_lit(true);
        let not_e = b.expr_n(ExpressionKind::Not(e));
        let nid = b.nid(not_e);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(not_e))]);
        assert_eq!(
            ty_of(&sem, nid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_not_of_integer_is_error() {
        let mut b = B::new();
        let e = b.int_lit(5);
        let not_e = b.expr_n(ExpressionKind::Not(e));
        let nid = b.nid(not_e);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(not_e))]);
        assert_eq!(ty_of(&sem, nid), &ResolvedType::Error);
    }

    #[test]
    fn check_ternary_widens_to_common() {
        let mut b = B::new();
        let c = b.bool_lit(true);
        let t = b.int_lit(1);
        let f = b.dec_lit("2.0");
        let tern = b.expr_n(ExpressionKind::IfThenElse(c, t, f));
        let tid = b.nid(tern);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(tern))]);
        assert_eq!(
            ty_of(&sem, tid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_ternary_with_unknown_branch_takes_other() {
        let mut b = B::new();
        let c = b.bool_lit(true);
        let t = b.int_lit(1);
        let f = b.unknown_lit();
        let tern = b.expr_n(ExpressionKind::IfThenElse(c, t, f));
        let tid = b.nid(tern);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(tern))]);
        assert_eq!(
            ty_of(&sem, tid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_unknown_arithmetic_propagates_as_unknown() {
        let mut b = B::new();
        let l = b.unknown_lit();
        let r = b.int_lit(1);
        let sum = b.expr_n(ExpressionKind::Add(l, r));
        let sid = b.nid(sum);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(ty_of(&sem, sid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_can_find_is_logical() {
        let mut b = B::new();
        let buf = b.ident("bCust");
        let cf = b.expr_n(ExpressionKind::CanFind {
            find_type: oxabl_ast::FindType::First,
            buffer: buf,
            where_clause: None,
            lock_type: oxabl_ast::LockType::NoLock,
            no_error: false,
        });
        let cid = b.nid(cf);
        let bname = b.ident("bCust");
        let tname = b.ident("Customer");
        let sem = b.analyze(vec![
            stmt_n(StatementKind::DefineBuffer {
                access: None,
                is_static: false,
                name: bname,
                target: BufferTarget::Table(tname),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(cf)),
        ]);
        assert_eq!(
            ty_of(&sem, cid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_function_call_type_is_return_type() {
        let mut b = B::new();
        let call_name = b.ident("calc");
        let call = b.expr_n(ExpressionKind::FunctionCall {
            name: call_name,
            arguments: ExprList::EMPTY,
        });
        let cid = b.nid(call);
        let fn_name = b.ident("calc");
        let sem = b.analyze(vec![
            stmt_n(StatementKind::Function {
                prototype_parameters: Vec::new(),
                name: fn_name,
                return_type: DataType::Decimal,
                body: vec![],
            }),
            stmt_n(StatementKind::ExpressionStatement(call)),
        ]);
        assert_eq!(
            ty_of(&sem, cid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_array_access_returns_element_type() {
        let mut b = B::new();
        let arr = b.id_expr("arr");
        let idx = b.int_lit(0);
        let ax = b.expr_n(ExpressionKind::ArrayAccess {
            array: arr,
            index: idx,
        });
        let aid = b.nid(ax);
        let arr_name = b.ident("arr");
        let sem = b.analyze(vec![
            stmt_n(StatementKind::VariableDeclaration {
                access: None,
                is_static: false,
                name: arr_name,
                type_source: TypeSource::Explicit(DataType::Integer),
                initial_value: None,
                no_undo: false,
                extent: Some(5),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            stmt_n(StatementKind::ExpressionStatement(ax)),
        ]);
        assert_eq!(
            ty_of(&sem, aid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_date_plus_integer_is_date() {
        let mut b = B::new();
        let d = b.id_expr("d");
        let n = b.int_lit(7);
        let sum = b.expr_n(ExpressionKind::Add(d, n));
        let sid = b.nid(sum);
        let decl = b.var_decl("d", DataType::Date);
        let sem = b.analyze(vec![decl, stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(
            ty_of(&sem, sid),
            &ResolvedType::Primitive(PrimitiveTy::Date)
        );
    }

    #[test]
    fn check_datetime_minus_datetime_is_int64() {
        let mut b = B::new();
        let a = b.id_expr("a");
        let bv = b.id_expr("b");
        let diff = b.expr_n(ExpressionKind::Minus(a, bv));
        let did = b.nid(diff);
        let da = b.var_decl("a", DataType::DateTime);
        let db = b.var_decl("b", DataType::DateTime);
        let sem = b.analyze(vec![
            da,
            db,
            stmt_n(StatementKind::ExpressionStatement(diff)),
        ]);
        assert_eq!(
            ty_of(&sem, did),
            &ResolvedType::Primitive(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn check_logical_and_integer_comparison_is_error() {
        let mut b = B::new();
        let a = b.bool_lit(true);
        let bv = b.int_lit(1);
        let cmp = b.expr_n(ExpressionKind::Equal(a, bv));
        let cid = b.nid(cmp);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(cmp))]);
        assert_eq!(ty_of(&sem, cid), &ResolvedType::Error);
    }

    #[test]
    fn check_method_call_of_an_undeclared_method_is_unknown() {
        // `Foo` declares no `doIt`: the call resolves to nothing and has no
        // type, so LINT0004 skips it.
        let mut b = B::new();
        let obj = b.id_expr("svc");
        let method = b.ident("doIt");
        let mc = b.expr_n(ExpressionKind::MethodCall {
            object: obj,
            method,
            arguments: ExprList::EMPTY,
        });
        let mid = b.nid(mc);
        let cls = b.ident("Foo");
        let decl = b.var_decl("svc", DataType::Class("Foo".into()));
        let sem = b.analyze(vec![
            stmt_n(StatementKind::Class {
                name: cls,
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
                use_widget_pool: false,
                body: vec![],
            }),
            decl,
            stmt_n(StatementKind::ExpressionStatement(mc)),
        ]);
        assert_eq!(ty_of(&sem, mid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_field_access_is_unknown_without_schema_typing() {
        let mut b = B::new();
        let qualifier = b.id_expr("Customer");
        let field = b.ident("CustNum");
        let fa = b.expr_n(ExpressionKind::FieldAccess { qualifier, field });
        let fid = b.nid(fa);
        let sem = b.analyze(vec![stmt_n(StatementKind::ExpressionStatement(fa))]);
        // No schema loaded: the field reference is `Unresolved { NoSchema }`,
        // so the node types as Unknown.
        assert_eq!(ty_of(&sem, fid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_new_class_type_is_class_symbol() {
        let mut b = B::new();
        let foo = b.name("Foo");
        let new_e = b.expr_n(ExpressionKind::New {
            type_arguments: None,
            class_name: foo,
            arguments: ExprList::EMPTY,
        });
        let nid = b.nid(new_e);
        let cls = b.ident("Foo");
        let sem = b.analyze(vec![
            stmt_n(StatementKind::Class {
                name: cls,
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
                use_widget_pool: false,
                body: vec![],
            }),
            stmt_n(StatementKind::ExpressionStatement(new_e)),
        ]);
        match ty_of(&sem, nid) {
            ResolvedType::Class(_) => {}
            other => panic!("expected Class(sym), got {other:?}"),
        }
    }

    #[test]
    fn check_analyze_file_runs_all_three_passes() {
        // Smoke test: analyze_file returns a Semantic with non-empty side
        // tables when the program has both declarations and expressions.
        let mut b = B::new();
        let x = b.id_expr("x");
        let one = b.int_lit(1);
        let sum = b.expr_n(ExpressionKind::Add(x, one));
        let sid = b.nid(sum);
        let decl = b.var_decl("x", DataType::Integer);
        let sem = b.analyze(vec![decl, stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert!(!sem.references.is_empty());
        assert!(!sem.types.is_empty());
        // The sum expression type is synthesized by check.
        assert_eq!(
            sem.types.get(sid),
            Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
        );
    }

    #[test]
    fn check_semantic_emits_no_diagnostics_for_type_mismatch() {
        // The check pass must not emit diagnostics — LINT0004 owns them.
        // Construct a mismatch: `x: LOGICAL = 1` (integer into logical).
        let mut b = B::new();
        let x = b.ident("x");
        let init = b.int_lit(1);
        let sem = b.analyze(vec![stmt_n(StatementKind::VariableDeclaration {
            access: None,
            is_static: false,
            name: x,
            type_source: TypeSource::Explicit(DataType::Logical),
            initial_value: Some(init),
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })]);
        // No type-mismatch diagnostic here — lint handles it.
        assert!(sem.diagnostics.is_empty());
    }

    #[test]
    fn check_parameter_type_populated() {
        // Parameters get types in `types[decl_stmt.id]` via resolve, and
        // the check pass preserves them unchanged.
        let mut b = B::new();
        let n = b.ident("n");
        let param = stmt_n(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Variable {
                name: n,
                type_source: TypeSource::Explicit(DataType::Integer),
                no_undo: false,
                extent: None,
            },
        });
        let pid = param.id;
        let f = b.ident("f");
        let func = stmt_n(StatementKind::Function {
            prototype_parameters: Vec::new(),
            name: f,
            return_type: DataType::Integer,
            body: vec![param],
        });
        let sem = b.analyze(vec![func]);
        assert_eq!(
            sem.types.get(pid),
            Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
        );
    }

    // -----------------------------------------------------------------------
    // Every unresolved reason collapses to the lattice bottom
    // -----------------------------------------------------------------------

    /// The invariant that actually keeps `type-mismatch-assignment` (LINT0004)
    /// silent on a cross-file reference: **no** [`UnresolvedReason`] produces a
    /// type. `type_from_reference`'s `_ =>` arm answers
    /// [`ResolvedType::Unknown`] for every one of them, LINT0004's
    /// `Unknown | Error` early return fires on that, and its own exhaustive
    /// reason match downstream is therefore never reached.
    ///
    /// Pinned here because it was implicit and untested, and because it lives in
    /// a different crate from the rule that depends on it: someone giving one
    /// reason a real type — plausibly `AbsentFromWorkspace`, where the workspace
    /// genuinely answered — would turn LINT0004 back on for cross-file code
    /// without touching `oxabl_lint` at all, and nothing would have said so.
    /// The reason list is written out variant by variant rather than derived, so
    /// adding a variant makes this a compile error too.
    #[test]
    fn every_unresolved_reason_types_to_unknown() {
        use crate::{NodeIndexVec, ScopeTree, SymbolTable, UnresolvedReason};
        use oxabl_ast::{Expression, ExpressionKind, Identifier, Span};
        use oxabl_lexer::oxabl_atom::OxablAtom;

        let reasons = [
            UnresolvedReason::NotInScope,
            UnresolvedReason::External,
            UnresolvedReason::NoSchema,
            UnresolvedReason::AbsentFromWorkspace,
            UnresolvedReason::PresentButUnusable,
            UnresolvedReason::Unknowable,
        ];
        // Exhaustiveness, stated so the compiler enforces it: a new variant fails
        // to match here and the author has to decide what it types to.
        for reason in reasons {
            match reason {
                UnresolvedReason::NotInScope
                | UnresolvedReason::External
                | UnresolvedReason::NoSchema
                | UnresolvedReason::AbsentFromWorkspace
                | UnresolvedReason::PresentButUnusable
                | UnresolvedReason::Unknowable => {}
            }
        }

        let tree = ScopeTree::new();
        let symbols = SymbolTable::new();
        for reason in reasons {
            let expr = Expression::new(ExpressionKind::Identifier(Identifier {
                span: Span { start: 0, end: 4 },
                name: NameId::DUMMY,
            }));
            let mut references = NodeIndexVec::new();
            references.insert(
                expr.id,
                Resolution::Unresolved {
                    name: OxablAtom::from("name"),
                    reason,
                },
            );
            let mut types = NodeIndexVec::new();
            let walker = CheckWalker {
                pools: &NO_POOLS,
                index: &crate::NullIndex,
                tree: &tree,
                symbols: &symbols,
                references: &references,
                types: &mut types,
            };
            assert_eq!(
                walker.type_from_reference(&expr),
                ResolvedType::Unknown,
                "`{reason:?}` must type to the lattice bottom"
            );
        }

        // And the same for a node with no reference entry at all, which is the
        // other half of the `_ =>` arm: an expression resolution never looked at.
        let expr = Expression::new(ExpressionKind::Identifier(Identifier {
            span: Span { start: 0, end: 4 },
            name: NameId::DUMMY,
        }));
        let references = NodeIndexVec::new();
        let mut types = NodeIndexVec::new();
        let walker = CheckWalker {
            pools: &NO_POOLS,
            index: &crate::NullIndex,
            tree: &tree,
            symbols: &symbols,
            references: &references,
            types: &mut types,
        };
        assert_eq!(walker.type_from_reference(&expr), ResolvedType::Unknown);
    }
}
