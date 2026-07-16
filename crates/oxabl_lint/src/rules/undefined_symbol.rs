//! `undefined-symbol` lint (LINT0001).
//!
//! Fires on every reference that resolves to
//! [`Resolution::Unresolved { reason: NotInScope }`] in a user-written
//! namespace (Values / Procedures / Functions / Streams / Frames / Events
//! / Types). Does *not* fire on `External` (cross-file / dynamic) or
//! `NoSchema` (schema-absent) unresolveds — those are by-design
//! skip-listed so the rule's signal stays high.

use oxabl_ast::{
    Expression, ExpressionKind, OnAction, OnKind, Statement, StatementKind, StreamOperation,
    SubscribeTarget,
};
use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{AnalysisContext, Resolution, Semantic, UnresolvedReason};

use super::LINT0001;

/// Entry point for the rule — returns every `undefined-symbol` diagnostic
/// produced by walking `program` against the resolved [`Semantic`].
pub fn run(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    let mut v = Visitor {
        sem,
        ctx,
        diags: Vec::new(),
    };
    v.walk_block(program);
    v.diags
}

struct Visitor<'a> {
    sem: &'a Semantic,
    ctx: &'a AnalysisContext<'a>,
    diags: Vec<Diagnostic>,
}

impl Visitor<'_> {
    fn walk_block(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            self.walk_statement(stmt);
        }
    }

    fn walk_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            // ---- Declarations with initializer expressions --------------
            StatementKind::VariableDeclaration { initial_value, .. } => {
                if let Some(e) = initial_value {
                    self.walk_expression(e);
                }
            }
            StatementKind::DefineTempTable { fields, .. } => {
                for f in fields {
                    if let Some(init) = &f.initial_value {
                        for e in init {
                            self.walk_expression(e);
                        }
                    }
                }
            }
            StatementKind::DefineEvent { parameters, .. } => {
                self.walk_block(parameters);
            }
            StatementKind::Property {
                get_body, set_body, ..
            } => {
                if let Some(b) = get_body {
                    self.walk_block(b);
                }
                if let Some(b) = set_body {
                    self.walk_block(b);
                }
            }

            // ---- Scope-opening declarations -----------------------------
            StatementKind::Procedure { body, .. }
            | StatementKind::Function { body, .. }
            | StatementKind::Class { body, .. }
            | StatementKind::Interface { body, .. }
            | StatementKind::Destructor { body }
            | StatementKind::Finally { body } => self.walk_block(body),
            StatementKind::Method {
                parameters, body, ..
            }
            | StatementKind::Constructor {
                parameters, body, ..
            } => {
                self.walk_block(parameters);
                self.walk_block(body);
            }
            StatementKind::Catch { body, .. } => self.walk_block(body),

            // ---- Control flow -------------------------------------------
            StatementKind::Do {
                from,
                to,
                by,
                while_condition,
                body,
                ..
            } => {
                for e in [from, to, by, while_condition].into_iter().flatten() {
                    self.walk_expression(e);
                }
                self.walk_block(body);
            }
            StatementKind::Repeat {
                while_condition,
                body,
            } => {
                if let Some(e) = while_condition {
                    self.walk_expression(e);
                }
                self.walk_block(body);
            }
            StatementKind::ForEach {
                where_clause, body, ..
            } => {
                if let Some(w) = where_clause {
                    self.walk_expression(w);
                }
                self.walk_block(body);
            }
            StatementKind::Find {
                key_value,
                where_clause,
                ..
            } => {
                if let Some(k) = key_value {
                    self.walk_expression(k);
                }
                if let Some(w) = where_clause {
                    self.walk_expression(w);
                }
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expression(condition);
                self.walk_statement(then_branch);
                if let Some(eb) = else_branch {
                    self.walk_statement(eb);
                }
            }
            StatementKind::Case {
                expression,
                when_branches,
                otherwise,
            } => {
                self.walk_expression(expression);
                for wb in when_branches {
                    for v in &wb.values {
                        self.walk_expression(v);
                    }
                    self.walk_block(&wb.body);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o);
                }
            }
            StatementKind::Block(body) => self.walk_block(body),
            StatementKind::Label { body, .. } => self.walk_statement(body),

            // ---- Trigger / preprocessor ---------------------------------
            StatementKind::On { kind } => match kind {
                OnKind::UiEvent { action, .. } | OnKind::DbEvent { action, .. } => match action {
                    OnAction::Block(body) => self.walk_statement(body),
                    OnAction::PersistentRun { arguments, .. } => {
                        for a in arguments {
                            self.walk_expression(a);
                        }
                    }
                    OnAction::Revert => {}
                },
                OnKind::KeyRemap { .. } => {}
            },
            StatementKind::PreprocIf(pif) => {
                self.walk_expression(&pif.condition);
                self.walk_block(&pif.then_branch);
                for (c, br) in &pif.elseif_branches {
                    self.walk_expression(c);
                    self.walk_block(br);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb);
                }
            }
            StatementKind::PreprocMessage { expression } => self.walk_expression(expression),

            // ---- Assignment / expression --------------------------------
            StatementKind::Assignment { target, value } => {
                self.walk_expression(target);
                self.walk_expression(value);
            }
            StatementKind::Assign { assignments } => {
                for p in assignments {
                    self.walk_expression(&p.target);
                    self.walk_expression(&p.value);
                }
            }
            StatementKind::ExpressionStatement(e) => self.walk_expression(e),
            StatementKind::Return(opt) => {
                if let Some(e) = opt {
                    self.walk_expression(e);
                }
            }
            StatementKind::Throw(e) => self.walk_expression(e),

            // ---- Output forms -------------------------------------------
            StatementKind::Display { items, .. } => {
                for item in items {
                    self.walk_expression(&item.expression);
                    if let Some(w) = &item.when_condition {
                        self.walk_expression(w);
                    }
                }
            }
            StatementKind::Message { items, .. } => {
                for e in items {
                    self.walk_expression(e);
                }
            }
            StatementKind::Run {
                target,
                arguments,
                in_handle,
                persistent_handle,
                async_handle,
                event_procedure,
                ..
            } => {
                if let oxabl_ast::RunTarget::Dynamic(e) = target {
                    self.walk_expression(e);
                }
                for arg in arguments {
                    self.walk_expression(&arg.expression);
                }
                for e in [in_handle, persistent_handle, async_handle, event_procedure]
                    .into_iter()
                    .flatten()
                {
                    self.walk_expression(e);
                }
            }
            StatementKind::Publish {
                event_name,
                from_handle,
                arguments,
            } => {
                self.walk_expression(event_name);
                if let Some(h) = from_handle {
                    self.walk_expression(h);
                }
                for arg in arguments {
                    self.walk_expression(&arg.expression);
                }
            }
            StatementKind::Subscribe {
                subscriber,
                event_name,
                target,
                ..
            } => {
                if let Some(s) = subscriber {
                    self.walk_expression(s);
                }
                self.walk_expression(event_name);
                if let SubscribeTarget::InHandle(h) = target {
                    self.walk_expression(h);
                }
            }
            StatementKind::Unsubscribe {
                subscriber,
                event_name,
                in_handle,
            } => {
                if let Some(s) = subscriber {
                    self.walk_expression(s);
                }
                if let Some(e) = event_name {
                    self.walk_expression(e);
                }
                if let Some(h) = in_handle {
                    self.walk_expression(h);
                }
            }
            StatementKind::BufferCopy { assignments, .. } => {
                for p in assignments {
                    self.walk_expression(&p.target);
                    self.walk_expression(&p.value);
                }
            }
            StatementKind::StreamIo { operation, .. } => match operation {
                StreamOperation::From(e) | StreamOperation::Through(e) => {
                    self.walk_expression(e);
                }
                StreamOperation::To { target, .. } => self.walk_expression(target),
                StreamOperation::Close => {}
            },
            StatementKind::Create { target, .. } => {
                if let oxabl_ast::CreateTarget::Handle { widget_pool, .. } = target
                    && let Some(e) = widget_pool
                {
                    self.walk_expression(e);
                }
            }

            // Nothing to walk in these.
            StatementKind::DefineParameter { .. }
            | StatementKind::DefineBuffer { .. }
            | StatementKind::DefineDataset { .. }
            | StatementKind::DefineDataSource { .. }
            | StatementKind::DefineStream { .. }
            | StatementKind::DefineFrame { .. }
            | StatementKind::PreprocDefine { .. }
            | StatementKind::PreprocUndefine { .. }
            | StatementKind::Using { .. }
            | StatementKind::TriggerProcedure { .. }
            | StatementKind::Delete { .. }
            | StatementKind::Release { .. }
            | StatementKind::Validate { .. }
            | StatementKind::BufferCompare { .. }
            | StatementKind::Leave(_)
            | StatementKind::Next(_)
            | StatementKind::IncludeReference { .. }
            | StatementKind::IncludeArgReference { .. }
            | StatementKind::Empty => {}
        }
    }

    fn walk_expression(&mut self, expr: &Expression) {
        match &expr.kind {
            ExpressionKind::Identifier(id) => {
                self.check(expr.id.to_owned_id(), &id.name, id.span);
            }
            ExpressionKind::FunctionCall { name, arguments } => {
                self.check(expr.id.to_owned_id(), &name.name, name.span);
                for a in arguments {
                    self.walk_expression(a);
                }
            }
            ExpressionKind::New {
                class_name: _,
                arguments,
            } => {
                // Class names in `NEW Foo(...)` are commonly USING-imported;
                // resolve reports them as `External`, which LINT0001 skips.
                // Still walk the arguments for nested undefined refs.
                self.maybe_check_expr_id(expr.id.to_owned_id(), "");
                for a in arguments {
                    self.walk_expression(a);
                }
            }
            ExpressionKind::CanFind {
                buffer,
                where_clause,
                ..
            } => {
                self.check(expr.id.to_owned_id(), &buffer.name, buffer.span);
                if let Some(w) = where_clause {
                    self.walk_expression(w);
                }
            }
            ExpressionKind::FieldAccess { qualifier, .. } => {
                // Field mismatches are LINT0003's responsibility; only flag
                // the qualifier (bare identifier) here if it's NotInScope.
                // The FieldAccess Expression's own id is NoSchema/NotInScope
                // on miss — *that* is LINT0003's to emit.
                self.walk_expression(qualifier);
            }
            ExpressionKind::MemberAccess { object, .. } => self.walk_expression(object),
            ExpressionKind::MethodCall {
                object, arguments, ..
            } => {
                self.walk_expression(object);
                for a in arguments {
                    self.walk_expression(a);
                }
            }
            ExpressionKind::ArrayAccess { array, index } => {
                self.walk_expression(array);
                self.walk_expression(index);
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
                self.walk_expression(l);
                self.walk_expression(r);
            }
            ExpressionKind::Negate(e) | ExpressionKind::Not(e) => self.walk_expression(e),
            ExpressionKind::IfThenElse(c, t, f) => {
                self.walk_expression(c);
                self.walk_expression(t);
                self.walk_expression(f);
            }
            ExpressionKind::PreprocIf(pif) => {
                self.walk_expression(&pif.condition);
                self.walk_expression(&pif.then_branch);
                for (c, br) in &pif.elseif_branches {
                    self.walk_expression(c);
                    self.walk_expression(br);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_expression(eb);
                }
            }
            ExpressionKind::Literal(_)
            | ExpressionKind::IncludeReference { .. }
            | ExpressionKind::IncludeArgReference { .. }
            | ExpressionKind::PreprocReference(_) => {}
        }
    }

    fn check(&mut self, expr_id: oxabl_ast::NodeId, name: &str, span: oxabl_ast::Span) {
        if let Some(Resolution::Unresolved {
            reason: UnresolvedReason::NotInScope,
            ..
        }) = self.sem.references.get(expr_id)
        {
            self.diags.push(Diagnostic::error(
                LINT0001,
                format!("undefined symbol `{name}`"),
                FileSpan {
                    file: self.ctx.file_id,
                    span,
                },
            ));
        }
    }

    fn maybe_check_expr_id(&mut self, _id: oxabl_ast::NodeId, _placeholder: &str) {
        // Reserved: NEW expression's own NodeId carries its resolution, but
        // we don't have a span for the class name on the AST — the parser
        // keeps class_name as a String without span metadata. Skip in v1.
    }
}

// Stand-in for `NodeId::to_owned_id` — `NodeId` is `Copy`, so passing it
// by value already works. The named helper keeps the call sites readable.
trait IdCopy {
    fn to_owned_id(self) -> oxabl_ast::NodeId;
}
impl IdCopy for oxabl_ast::NodeId {
    fn to_owned_id(self) -> oxabl_ast::NodeId {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{
        DataType, Expression, ExpressionKind, Identifier, IntegerLiteral, Literal, NodeId, Span,
        Statement, StatementKind, TypeSource,
    };
    use oxabl_common::{FileId, Severity};
    use oxabl_schema::Schema;
    use oxabl_semantic::analyze_file;
    use std::sync::atomic::{AtomicU32, Ordering};

    fn next_nid() -> NodeId {
        static C: AtomicU32 = AtomicU32::new(1);
        NodeId::from_u32(C.fetch_add(1, Ordering::Relaxed))
    }
    fn id(n: &str) -> Identifier {
        Identifier {
            span: Span {
                start: 0,
                end: n.len() as u32,
            },
            name: n.into(),
        }
    }
    fn stmt_n(k: StatementKind) -> Statement {
        Statement::with_id(next_nid(), k)
    }
    fn expr_n(k: ExpressionKind) -> Expression {
        Expression::with_id(next_nid(), k)
    }
    fn int_lit(v: i64) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: v,
        })))
    }
    fn id_expr(n: &str) -> Expression {
        expr_n(ExpressionKind::Identifier(id(n)))
    }
    fn var_decl(n: &str, ty: DataType) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
        })
    }
    fn analyze_and_lint(stmts: Vec<Statement>) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    #[test]
    fn fires_on_undefined_bare_identifier() {
        let u = id_expr("ghost");
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(u))]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, LINT0001);
        assert_eq!(diags[0].severity, Severity::Error);
        assert!(diags[0].message.contains("ghost"));
    }

    #[test]
    fn does_not_fire_on_resolved_identifier() {
        let u = id_expr("x");
        let diags = analyze_and_lint(vec![
            var_decl("x", DataType::Integer),
            stmt_n(StatementKind::ExpressionStatement(u)),
        ]);
        assert!(diags.is_empty());
    }

    #[test]
    fn fires_on_undefined_function_call() {
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("nope"),
            arguments: vec![],
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("nope"));
    }

    #[test]
    fn skip_list_new_class_is_external_not_undefined() {
        // `NEW Bar()` without a local `Bar` resolves to External (USING-
        // imported) — LINT0001 skips External.
        let new_e = expr_n(ExpressionKind::New {
            class_name: "Bar".into(),
            arguments: vec![],
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(new_e))]);
        assert!(diags.is_empty());
    }

    #[test]
    fn skip_list_no_schema_field_is_skipped() {
        // `Customer.CustNum` with no schema loaded → NoSchema, which
        // LINT0001 skips (LINT0003 would own that case).
        let q = id_expr("Customer");
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(q),
            field: id("CustNum"),
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(fa))]);
        assert!(diags.is_empty());
    }

    #[test]
    fn argument_inside_known_call_is_checked() {
        let arg = id_expr("ghost_arg");
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("calc"),
            arguments: vec![arg],
        });
        let diags = analyze_and_lint(vec![
            stmt_n(StatementKind::Function {
                name: id("calc"),
                return_type: DataType::Integer,
                body: vec![],
            }),
            stmt_n(StatementKind::ExpressionStatement(call)),
        ]);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("ghost_arg"));
    }

    #[test]
    fn arithmetic_with_undefined_operand_fires() {
        let ghost = id_expr("ghost");
        let sum = expr_n(ExpressionKind::Add(Box::new(ghost), Box::new(int_lit(1))));
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(sum))]);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn inside_procedure_sees_file_scope() {
        let body_use = id_expr("outer");
        let proc = stmt_n(StatementKind::Procedure {
            name: id("p"),
            body: vec![stmt_n(StatementKind::ExpressionStatement(body_use))],
        });
        let diags = analyze_and_lint(vec![var_decl("outer", DataType::Integer), proc]);
        assert!(diags.is_empty());
    }

    #[test]
    fn inside_method_fires_on_undefined_body_ident() {
        use oxabl_ast::AccessModifier;
        let body_use = id_expr("ghost");
        let method = stmt_n(StatementKind::Method {
            access: AccessModifier::Public,
            is_static: false,
            is_abstract: false,
            is_override: false,
            return_type: None,
            name: id("m"),
            parameters: vec![],
            body: vec![stmt_n(StatementKind::ExpressionStatement(body_use))],
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::Class {
            name: id("C"),
            inherits: None,
            implements: vec![],
            is_abstract: false,
            is_final: false,
            body: vec![method],
        })]);
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn builtin_session_does_not_fire() {
        let u = id_expr("session");
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(u))]);
        assert!(diags.is_empty());
    }

    #[test]
    fn builtin_function_calls_do_not_fire() {
        // The dominant #58 false positives: calls to built-in ABL functions
        // that are not declared locally. These now resolve via the built-in
        // registry (recorded as External) instead of NotInScope.
        for name in [
            "length",
            "entry",
            "substring",
            "trim",
            "round",
            "num-entries",
        ] {
            let call = expr_n(ExpressionKind::FunctionCall {
                name: id(name),
                arguments: vec![int_lit(1)],
            });
            let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
            assert!(
                diags.is_empty(),
                "built-in `{name}` should not fire LINT0001, got {diags:?}"
            );
        }
    }

    #[test]
    fn builtin_function_matching_is_case_insensitive() {
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("SUBSTRING"),
            arguments: vec![int_lit(1)],
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
        assert!(
            diags.is_empty(),
            "SUBSTRING (upper) should not fire, got {diags:?}"
        );
    }

    #[test]
    fn non_builtin_function_still_fires() {
        // A name that is not a built-in and not declared locally must still be
        // reported — the registry must not blanket-suppress unknown calls.
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("frobnicate"),
            arguments: vec![],
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("frobnicate"));
    }

    #[test]
    fn builtin_abbreviation_calls_do_not_fire() {
        // Reserved-keyword built-ins may be called by any prefix down to the
        // documented minimum abbreviation (e.g. `AVAIL(customer)`).
        for name in [
            "avail",
            "ambig",
            "dbrest",
            "dbvers",
            "gateway",
            "is-attr",
            "is-lead",
            "keyfunc",
            "line-count",
            "num-ali",
            "page-num",
            "proc-ha",
            "proc-st",
            "provers",
            "setuser",
            "term",
            // Data type conversion functions are reserved keywords too.
            "dec",
            "int",
            "log",
        ] {
            let call = expr_n(ExpressionKind::FunctionCall {
                name: id(name),
                arguments: vec![int_lit(1)],
            });
            let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
            assert!(
                diags.is_empty(),
                "abbreviated built-in `{name}` should not fire LINT0001, got {diags:?}"
            );
        }
        // Folding still applies to abbreviated forms.
        for name in ["AVAIL", "Avail"] {
            let call = expr_n(ExpressionKind::FunctionCall {
                name: id(name),
                arguments: vec![int_lit(1)],
            });
            let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
            assert!(
                diags.is_empty(),
                "mixed-case abbreviation `{name}` should not fire, got {diags:?}"
            );
        }
    }

    #[test]
    fn non_abbreviable_builtin_truncation_still_fires() {
        // `LENGTH` is a built-in but NOT a reserved keyword, so ABL does not
        // permit abbreviating it — a truncation like `LENGT` is simply an
        // undefined symbol and must keep firing.
        let call = expr_n(ExpressionKind::FunctionCall {
            name: id("LENGT"),
            arguments: vec![int_lit(1)],
        });
        let diags = analyze_and_lint(vec![stmt_n(StatementKind::ExpressionStatement(call))]);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("LENGT"));
    }
}
