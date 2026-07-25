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

use oxabl_ast::{
    AssignPair, CreateTarget, Expression, ExpressionKind, Literal, OnAction, OnKind, RunTarget,
    Statement, StatementKind, StreamOperation, SubscribeTarget,
};
use oxabl_common::Diagnostic;

use crate::{
    AnalysisContext, NodeIndexVec, PrimitiveTy, Resolution, ResolvedType, ScopeId, ScopeKind,
    ScopeTree, SymbolKind, SymbolTable, operators,
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
    _ctx: &AnalysisContext,
    tree: &ScopeTree,
    symbols: &SymbolTable,
    references: &NodeIndexVec<Resolution>,
    types: &mut NodeIndexVec<ResolvedType>,
) -> Vec<Diagnostic> {
    let mut walker = CheckWalker {
        tree,
        symbols,
        references,
        types,
    };
    walker.walk_block(program, ScopeId::ROOT);
    Vec::new()
}

struct CheckWalker<'a> {
    tree: &'a ScopeTree,
    symbols: &'a SymbolTable,
    references: &'a NodeIndexVec<Resolution>,
    types: &'a mut NodeIndexVec<ResolvedType>,
}

impl<'a> CheckWalker<'a> {
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
                    self.check_expression(e, scope);
                }
            }
            StatementKind::DefineParameter { .. } => {}
            StatementKind::DefineTempTable { fields, .. } => {
                for f in fields {
                    if let Some(init) = &f.initial_value {
                        for e in init {
                            self.check_expression(e, scope);
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
                body,
                ..
            } => {
                let bs = self
                    .find_child_scope(scope, stmt, ScopeKind::Block)
                    .unwrap_or(scope);
                for e in [from, to, by, while_condition].into_iter().flatten() {
                    self.check_expression(e, bs);
                }
                self.walk_block(body, bs);
            }
            StatementKind::Repeat {
                while_condition,
                body,
            } => {
                let bs = self
                    .find_child_scope(scope, stmt, ScopeKind::Block)
                    .unwrap_or(scope);
                if let Some(e) = while_condition {
                    self.check_expression(e, bs);
                }
                self.walk_block(body, bs);
            }
            StatementKind::ForEach {
                where_clause, body, ..
            } => {
                let bs = self
                    .find_child_scope(scope, stmt, ScopeKind::Block)
                    .unwrap_or(scope);
                if let Some(w) = where_clause {
                    self.check_expression(w, bs);
                }
                self.walk_block(body, bs);
            }
            StatementKind::Find {
                key_value,
                where_clause,
                ..
            } => {
                if let Some(k) = key_value {
                    self.check_expression(k, scope);
                }
                if let Some(w) = where_clause {
                    self.check_expression(w, scope);
                }
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expression(condition, scope);
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
                self.check_expression(expression, scope);
                for wb in when_branches {
                    for v in &wb.values {
                        self.check_expression(v, scope);
                    }
                    self.walk_block(&wb.body, scope);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o, scope);
                }
            }
            StatementKind::Block(body) => self.walk_block(body, scope),
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
                    OnAction::PersistentRun { arguments, .. } => {
                        for arg in arguments {
                            self.check_expression(arg, scope);
                        }
                    }
                    OnAction::Revert => {}
                },
                OnKind::KeyRemap { .. } => {}
            },
            StatementKind::TriggerProcedure { .. } => {}
            StatementKind::PreprocIf(pif) => {
                self.check_expression(&pif.condition, scope);
                self.walk_block(&pif.then_branch, scope);
                for (c, br) in &pif.elseif_branches {
                    self.check_expression(c, scope);
                    self.walk_block(br, scope);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb, scope);
                }
            }
            StatementKind::PreprocDefine { .. } | StatementKind::PreprocUndefine { .. } => {}
            StatementKind::PreprocMessage { expression } => {
                self.check_expression(expression, scope);
            }

            // ---- Assignment / expression forms ---------------------------
            StatementKind::Assignment { target, value } => {
                self.check_expression(target, scope);
                self.check_expression(value, scope);
            }
            StatementKind::Assign { assignments } => {
                for AssignPair { target, value } in assignments {
                    self.check_expression(target, scope);
                    self.check_expression(value, scope);
                }
            }
            StatementKind::ExpressionStatement(expr) => {
                self.check_expression(expr, scope);
            }
            StatementKind::Return(opt) => {
                if let Some(e) = opt {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Throw(expr) => {
                self.check_expression(expr, scope);
            }

            // ---- Output / display ----------------------------------------
            StatementKind::Display { items, .. } => {
                for item in items {
                    self.check_expression(&item.expression, scope);
                    if let Some(w) = &item.when_condition {
                        self.check_expression(w, scope);
                    }
                }
            }
            StatementKind::Message { items, .. } => {
                for e in items {
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
                    self.check_expression(e, scope);
                }
                for arg in arguments {
                    self.check_expression(&arg.expression, scope);
                }
                for e in [in_handle, persistent_handle, async_handle, event_procedure]
                    .into_iter()
                    .flatten()
                {
                    self.check_expression(e, scope);
                }
            }
            StatementKind::Delete { .. }
            | StatementKind::Release { .. }
            | StatementKind::Validate { .. } => {}
            StatementKind::BufferCopy { assignments, .. } => {
                for AssignPair { target, value } in assignments {
                    self.check_expression(target, scope);
                    self.check_expression(value, scope);
                }
            }
            StatementKind::BufferCompare { .. } => {}
            StatementKind::Create { target, .. } => {
                if let CreateTarget::Handle { widget_pool, .. } = target
                    && let Some(e) = widget_pool
                {
                    self.check_expression(e, scope);
                }
            }

            StatementKind::Publish {
                event_name,
                from_handle,
                arguments,
            } => {
                self.check_expression(event_name, scope);
                if let Some(fh) = from_handle {
                    self.check_expression(fh, scope);
                }
                for arg in arguments {
                    self.check_expression(&arg.expression, scope);
                }
            }
            StatementKind::Subscribe {
                subscriber,
                event_name,
                target,
                ..
            } => {
                if let Some(s) = subscriber {
                    self.check_expression(s, scope);
                }
                self.check_expression(event_name, scope);
                if let SubscribeTarget::InHandle(h) = target {
                    self.check_expression(h, scope);
                }
            }
            StatementKind::Unsubscribe {
                subscriber,
                event_name,
                in_handle,
            } => {
                if let Some(s) = subscriber {
                    self.check_expression(s, scope);
                }
                if let Some(e) = event_name {
                    self.check_expression(e, scope);
                }
                if let Some(h) = in_handle {
                    self.check_expression(h, scope);
                }
            }

            StatementKind::StreamIo { operation, .. } => match operation {
                StreamOperation::From(e) | StreamOperation::Through(e) => {
                    self.check_expression(e, scope);
                }
                StreamOperation::To { target, .. } => {
                    self.check_expression(target, scope);
                }
                StreamOperation::Close => {}
            },

            StatementKind::Using { .. }
            | StatementKind::Leave(_)
            | StatementKind::Next(_)
            | StatementKind::IncludeReference { .. }
            | StatementKind::IncludeArgReference { .. }
            | StatementKind::Empty
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
    fn check_expression(&mut self, expr: &Expression, scope: ScopeId) -> ResolvedType {
        let ty = match &expr.kind {
            ExpressionKind::Literal(l) => literal_type(l),

            ExpressionKind::Identifier(_) => self.type_from_reference(expr),

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
                let lt = self.check_expression(l, scope);
                let rt = self.check_expression(r, scope);
                operators::binary_op_result(&expr.kind, &lt, &rt)
            }

            ExpressionKind::Negate(e) => {
                let t = self.check_expression(e, scope);
                operators::unary_negate_result(&t)
            }
            ExpressionKind::Not(e) => {
                let t = self.check_expression(e, scope);
                operators::unary_not_result(&t)
            }
            ExpressionKind::IfThenElse(c, t, f) => {
                self.check_expression(c, scope);
                let tt = self.check_expression(t, scope);
                let ft = self.check_expression(f, scope);
                // Widen then/else to a common type. If incompatible,
                // result is Unknown — ternary is a late-checked place.
                widen_types(&tt, &ft)
            }

            ExpressionKind::FunctionCall { arguments, .. } => {
                for a in arguments {
                    self.check_expression(a, scope);
                }
                // The call's return type comes from the resolved symbol,
                // populated by resolve. If unresolved, Unknown.
                self.type_from_reference(expr)
            }
            ExpressionKind::MethodCall {
                object, arguments, ..
            } => {
                self.check_expression(object, scope);
                for a in arguments {
                    self.check_expression(a, scope);
                }
                // v1: method return types are cross-class — Unknown.
                ResolvedType::Unknown
            }
            ExpressionKind::MemberAccess { object, .. } => {
                self.check_expression(object, scope);
                ResolvedType::Unknown
            }
            ExpressionKind::ArrayAccess { array, index } => {
                let at = self.check_expression(array, scope);
                self.check_expression(index, scope);
                match at {
                    ResolvedType::Array { element, .. } => *element,
                    ResolvedType::Unknown => ResolvedType::Unknown,
                    _ => ResolvedType::Error,
                }
            }
            ExpressionKind::FieldAccess { qualifier, .. } => {
                self.check_expression(qualifier, scope);
                // The resolve pass records the field's resolution on the
                // composite node: a validated schema field resolves to a
                // synthesized `Field` symbol whose `data_type` is the schema
                // primitive, so the type flows out of the reference.
                self.type_from_reference(expr)
            }
            ExpressionKind::New { arguments, .. } => {
                for a in arguments {
                    self.check_expression(a, scope);
                }
                self.type_from_reference(expr)
            }
            ExpressionKind::CanFind { where_clause, .. } => {
                if let Some(w) = where_clause {
                    self.check_expression(w, scope);
                }
                ResolvedType::Primitive(PrimitiveTy::Logical)
            }

            ExpressionKind::IncludeReference { .. }
            | ExpressionKind::IncludeArgReference { .. }
            | ExpressionKind::PreprocReference(_) => ResolvedType::Unknown,

            ExpressionKind::PreprocIf(pif) => {
                self.check_expression(&pif.condition, scope);
                let tt = self.check_expression(&pif.then_branch, scope);
                for (c, br) in &pif.elseif_branches {
                    self.check_expression(c, scope);
                    let bt = self.check_expression(br, scope);
                    let _ = bt;
                }
                if let Some(eb) = &pif.else_branch {
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

    fn type_from_reference(&self, expr: &Expression) -> ResolvedType {
        match self.references.get(expr.id) {
            Some(Resolution::Resolved(sym)) => {
                let symbol = self.symbols.get(*sym);
                // Class / Interface symbols carry no `data_type`; an
                // expression that resolves *to* a class symbol (e.g. `NEW
                // Foo(...)`) has type `Class(Foo)`.
                match symbol.kind {
                    SymbolKind::Class | SymbolKind::Interface => ResolvedType::Class(*sym),
                    SymbolKind::Buffer | SymbolKind::TempTable => ResolvedType::Buffer(*sym),
                    _ => symbol.data_type.clone().unwrap_or(ResolvedType::Unknown),
                }
            }
            _ => ResolvedType::Unknown,
        }
    }

    /// Linear-search for the unique child scope of `parent` created by
    /// declare with `owner_node == stmt.id && kind == kind`.
    fn find_child_scope(
        &self,
        parent: ScopeId,
        stmt: &Statement,
        kind: ScopeKind,
    ) -> Option<ScopeId> {
        self.tree
            .iter()
            .find(|(_, s)| s.parent == Some(parent) && s.owner_node == stmt.id && s.kind == kind)
            .map(|(id, _)| id)
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
        use oxabl_ast::{Span, StringLiteral};
        let lit = Literal::String(StringLiteral {
            span: Span { start: 0, end: 1 },
            value: "hi".into(),
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
        BooleanLiteral, BufferTarget, DataType, DecimalLiteral, Expression, ExpressionKind,
        Identifier, IntegerLiteral, NodeId, ParameterDirection, ParameterType, Span, Statement,
        StatementKind, StringLiteral, TypeSource, UnknownLiteral, XmlSerializeOptions,
    };
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use std::sync::atomic::{AtomicU32, Ordering};

    fn next_nid() -> NodeId {
        static C: AtomicU32 = AtomicU32::new(1);
        NodeId::from_u32(C.fetch_add(1, Ordering::Relaxed))
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            span: Span {
                start: 0,
                end: name.len() as u32,
            },
            name: name.into(),
        }
    }

    fn stmt_n(kind: StatementKind) -> Statement {
        Statement::with_id(next_nid(), oxabl_ast::Span::DUMMY, kind)
    }

    fn expr_n(kind: ExpressionKind) -> Expression {
        Expression::with_id(next_nid(), oxabl_ast::Span::DUMMY, kind)
    }

    fn int_lit(v: i64) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: v,
        })))
    }

    fn dec_lit(d: &str) -> Expression {
        use std::str::FromStr;
        expr_n(ExpressionKind::Literal(Literal::Decimal(DecimalLiteral {
            span: Span { start: 0, end: 1 },
            value: ::rust_decimal::Decimal::from_str(d).unwrap(),
        })))
    }

    fn str_lit(s: &str) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::String(StringLiteral {
            span: Span { start: 0, end: 1 },
            value: s.into(),
        })))
    }

    fn bool_lit(b: bool) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Boolean(BooleanLiteral {
            span: Span { start: 0, end: 1 },
            value: b,
        })))
    }

    fn unknown_lit() -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Unknown(UnknownLiteral {
            span: Span { start: 0, end: 1 },
        })))
    }

    fn id_expr(name: &str) -> Expression {
        expr_n(ExpressionKind::Identifier(ident(name)))
    }

    fn var_decl(name: &str, ty: DataType) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: ident(name),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn analyze(stmts: Vec<Statement>) -> crate::Semantic {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        analyze_file(&stmts, &ctx)
    }

    fn ty_of(sem: &crate::Semantic, id: NodeId) -> &ResolvedType {
        sem.types
            .get(id)
            .unwrap_or_else(|| panic!("expected type at NodeId {id:?}"))
    }

    #[test]
    fn check_integer_literal() {
        let lit = int_lit(5);
        let lid = lit.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_decimal_literal() {
        let lit = dec_lit("3.14");
        let lid = lit.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_string_literal() {
        let lit = str_lit("hi");
        let lid = lit.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn check_boolean_literal() {
        let lit = bool_lit(true);
        let lid = lit.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(
            ty_of(&sem, lid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_unknown_literal_is_unknown_lattice_bottom() {
        let lit = unknown_lit();
        let lid = lit.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(lit))]);
        assert_eq!(ty_of(&sem, lid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_identifier_takes_symbol_type() {
        let use_x = id_expr("x");
        let uid = use_x.id;
        let sem = analyze(vec![
            var_decl("x", DataType::Character),
            stmt_n(StatementKind::ExpressionStatement(use_x)),
        ]);
        assert_eq!(
            ty_of(&sem, uid),
            &ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn check_unresolved_identifier_is_unknown() {
        let ghost = id_expr("ghost");
        let gid = ghost.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(ghost))]);
        assert_eq!(ty_of(&sem, gid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_integer_plus_integer_is_integer() {
        let l = int_lit(1);
        let r = int_lit(2);
        let sum = expr_n(ExpressionKind::Add(Box::new(l), Box::new(r)));
        let sid = sum.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(
            ty_of(&sem, sid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_integer_plus_decimal_is_decimal() {
        let l = int_lit(1);
        let r = dec_lit("2.0");
        let sum = expr_n(ExpressionKind::Add(Box::new(l), Box::new(r)));
        let sid = sum.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(
            ty_of(&sem, sid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_integer_div_integer_is_decimal() {
        let l = int_lit(10);
        let r = int_lit(2);
        let div = expr_n(ExpressionKind::Divide(Box::new(l), Box::new(r)));
        let did = div.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(div))]);
        assert_eq!(
            ty_of(&sem, did),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_string_concat_is_character() {
        let l = str_lit("a");
        let r = str_lit("b");
        let cat = expr_n(ExpressionKind::Add(Box::new(l), Box::new(r)));
        let cid = cat.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(cat))]);
        assert_eq!(
            ty_of(&sem, cid),
            &ResolvedType::Primitive(PrimitiveTy::Character)
        );
    }

    #[test]
    fn check_comparison_is_logical() {
        let l = int_lit(1);
        let r = int_lit(2);
        let eq = expr_n(ExpressionKind::Equal(Box::new(l), Box::new(r)));
        let eid = eq.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(eq))]);
        assert_eq!(
            ty_of(&sem, eid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_and_of_logicals_is_logical() {
        let l = bool_lit(true);
        let r = bool_lit(false);
        let and_e = expr_n(ExpressionKind::And(Box::new(l), Box::new(r)));
        let aid = and_e.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(and_e))]);
        assert_eq!(
            ty_of(&sem, aid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_begins_of_chars_is_logical() {
        let l = str_lit("hello");
        let r = str_lit("he");
        let b = expr_n(ExpressionKind::Begins(Box::new(l), Box::new(r)));
        let bid = b.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(b))]);
        assert_eq!(
            ty_of(&sem, bid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_negate_integer_is_integer() {
        let e = int_lit(5);
        let neg = expr_n(ExpressionKind::Negate(Box::new(e)));
        let nid = neg.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(neg))]);
        assert_eq!(
            ty_of(&sem, nid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_not_of_logical_is_logical() {
        let e = bool_lit(true);
        let not_e = expr_n(ExpressionKind::Not(Box::new(e)));
        let nid = not_e.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(not_e))]);
        assert_eq!(
            ty_of(&sem, nid),
            &ResolvedType::Primitive(PrimitiveTy::Logical)
        );
    }

    #[test]
    fn check_not_of_integer_is_error() {
        let e = int_lit(5);
        let not_e = expr_n(ExpressionKind::Not(Box::new(e)));
        let nid = not_e.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(not_e))]);
        assert_eq!(ty_of(&sem, nid), &ResolvedType::Error);
    }

    #[test]
    fn check_ternary_widens_to_common() {
        let c = bool_lit(true);
        let t = int_lit(1);
        let f = dec_lit("2.0");
        let tern = expr_n(ExpressionKind::IfThenElse(
            Box::new(c),
            Box::new(t),
            Box::new(f),
        ));
        let tid = tern.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(tern))]);
        assert_eq!(
            ty_of(&sem, tid),
            &ResolvedType::Primitive(PrimitiveTy::Decimal)
        );
    }

    #[test]
    fn check_ternary_with_unknown_branch_takes_other() {
        let c = bool_lit(true);
        let t = int_lit(1);
        let f = unknown_lit();
        let tern = expr_n(ExpressionKind::IfThenElse(
            Box::new(c),
            Box::new(t),
            Box::new(f),
        ));
        let tid = tern.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(tern))]);
        assert_eq!(
            ty_of(&sem, tid),
            &ResolvedType::Primitive(PrimitiveTy::Integer)
        );
    }

    #[test]
    fn check_unknown_arithmetic_propagates_as_unknown() {
        let l = unknown_lit();
        let r = int_lit(1);
        let sum = expr_n(ExpressionKind::Add(Box::new(l), Box::new(r)));
        let sid = sum.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(ty_of(&sem, sid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_can_find_is_logical() {
        let cf = expr_n(ExpressionKind::CanFind {
            find_type: oxabl_ast::FindType::First,
            buffer: ident("bCust"),
            where_clause: None,
            lock_type: oxabl_ast::LockType::NoLock,
            no_error: false,
        });
        let cid = cf.id;
        let sem = analyze(vec![
            stmt_n(StatementKind::DefineBuffer {
                name: ident("bCust"),
                target: BufferTarget::Table(ident("Customer")),
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
        let call = expr_n(ExpressionKind::FunctionCall {
            name: ident("calc"),
            arguments: vec![],
        });
        let cid = call.id;
        let sem = analyze(vec![
            stmt_n(StatementKind::Function {
                name: ident("calc"),
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
        let arr = id_expr("arr");
        let idx = int_lit(0);
        let ax = expr_n(ExpressionKind::ArrayAccess {
            array: Box::new(arr),
            index: Box::new(idx),
        });
        let aid = ax.id;
        let sem = analyze(vec![
            stmt_n(StatementKind::VariableDeclaration {
                name: ident("arr"),
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
        let d = id_expr("d");
        let n = int_lit(7);
        let sum = expr_n(ExpressionKind::Add(Box::new(d), Box::new(n)));
        let sid = sum.id;
        let sem = analyze(vec![
            var_decl("d", DataType::Date),
            stmt_n(StatementKind::ExpressionStatement(sum)),
        ]);
        assert_eq!(
            ty_of(&sem, sid),
            &ResolvedType::Primitive(PrimitiveTy::Date)
        );
    }

    #[test]
    fn check_datetime_minus_datetime_is_int64() {
        let a = id_expr("a");
        let b = id_expr("b");
        let diff = expr_n(ExpressionKind::Minus(Box::new(a), Box::new(b)));
        let did = diff.id;
        let sem = analyze(vec![
            var_decl("a", DataType::DateTime),
            var_decl("b", DataType::DateTime),
            stmt_n(StatementKind::ExpressionStatement(diff)),
        ]);
        assert_eq!(
            ty_of(&sem, did),
            &ResolvedType::Primitive(PrimitiveTy::Int64)
        );
    }

    #[test]
    fn check_logical_and_integer_comparison_is_error() {
        let a = bool_lit(true);
        let b = int_lit(1);
        let cmp = expr_n(ExpressionKind::Equal(Box::new(a), Box::new(b)));
        let cid = cmp.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(cmp))]);
        assert_eq!(ty_of(&sem, cid), &ResolvedType::Error);
    }

    #[test]
    fn check_method_call_type_is_unknown_v1() {
        // v1 intentionally does not type cross-class method calls; returns
        // Unknown so LINT0004 skips silently.
        let obj = id_expr("svc");
        let mc = expr_n(ExpressionKind::MethodCall {
            object: Box::new(obj),
            method: ident("doIt"),
            arguments: vec![],
        });
        let mid = mc.id;
        let sem = analyze(vec![
            stmt_n(StatementKind::Class {
                name: ident("Foo"),
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
                body: vec![],
            }),
            var_decl("svc", DataType::Class("Foo".into())),
            stmt_n(StatementKind::ExpressionStatement(mc)),
        ]);
        assert_eq!(ty_of(&sem, mid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_field_access_is_unknown_without_schema_typing() {
        let qualifier = id_expr("Customer");
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(qualifier),
            field: ident("CustNum"),
        });
        let fid = fa.id;
        let sem = analyze(vec![stmt_n(StatementKind::ExpressionStatement(fa))]);
        // No schema loaded: the field reference is `Unresolved { NoSchema }`,
        // so the node types as Unknown.
        assert_eq!(ty_of(&sem, fid), &ResolvedType::Unknown);
    }

    #[test]
    fn check_new_class_type_is_class_symbol() {
        let new_e = expr_n(ExpressionKind::New {
            class_name: "Foo".into(),
            arguments: vec![],
        });
        let nid = new_e.id;
        let sem = analyze(vec![
            stmt_n(StatementKind::Class {
                name: ident("Foo"),
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
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
        let sum = expr_n(ExpressionKind::Add(
            Box::new(id_expr("x")),
            Box::new(int_lit(1)),
        ));
        let sid = sum.id;
        let sem = analyze(vec![
            var_decl("x", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(sum)),
        ]);
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
        let sem = analyze(vec![stmt_n(StatementKind::VariableDeclaration {
            name: ident("x"),
            type_source: TypeSource::Explicit(DataType::Logical),
            initial_value: Some(int_lit(1)),
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
        let param = stmt_n(StatementKind::DefineParameter {
            direction: ParameterDirection::Input,
            param_type: ParameterType::Variable {
                name: ident("n"),
                type_source: TypeSource::Explicit(DataType::Integer),
                no_undo: false,
            },
        });
        let pid = param.id;
        let func = stmt_n(StatementKind::Function {
            name: ident("f"),
            return_type: DataType::Integer,
            body: vec![param],
        });
        let sem = analyze(vec![func]);
        assert_eq!(
            sem.types.get(pid),
            Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
        );
    }
}
