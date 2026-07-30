//! `type-mismatch-assignment` lint (LINT0004).
//!
//! Fires on assignments whose RHS type is not [`assignable`] to the LHS
//! type, and on assignments whose RHS is a narrowing conversion flagged
//! by [`is_narrowing_warning`]. Covers:
//! - `target = value` (`StatementKind::Assignment`)
//! - `ASSIGN target = value ...` (`StatementKind::Assign`)
//! - `DEFINE VARIABLE ... INITIAL expr` (initial value vs declared type)
//!
//! Skips (captured as tests below):
//! - Either side `Unknown` or `Error` (suppresses cascades; `?` is ABL's
//!   universal bottom).
//! - The value side bound to a cross-file reference (`External`,
//!   `NotFoundInWorkspace`, `Unknowable`) — cross-file typing is not
//!   available, so no verdict is possible.

use oxabl_ast::{AssignPair, Expression, ExpressionKind, Statement, StatementKind, TypeSource};
use oxabl_common::{Diagnostic, FileSpan, Severity};
use oxabl_semantic::{
    AnalysisContext, Resolution, ResolvedType, Semantic, UnresolvedReason, assignable,
    is_narrowing_warning,
};

use super::LINT0004;

/// Entry point.
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
    #[allow(dead_code)]
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
            StatementKind::VariableDeclaration {
                type_source: TypeSource::Explicit(_),
                initial_value: Some(init),
                ..
            } => {
                // Target type = the symbol's data_type (on the declaration
                // stmt.id slot of the types side table).
                let target_ty = self.sem.types.get(stmt.id).cloned();
                let value_ty = self.sem.types.get(init.id).cloned();
                if let (Some(to), Some(from)) = (target_ty, value_ty) {
                    self.check_pair(&from, &to, init.id, &init_span(init));
                }
            }
            StatementKind::Assignment { target, value } => {
                self.check_assignment(target, value);
            }
            StatementKind::Assign { assignments } => {
                for AssignPair { target, value } in assignments {
                    self.check_assignment(target, value);
                }
            }

            // Recurse into bodies.
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
            StatementKind::Do { body, .. } | StatementKind::Repeat { body, .. } => {
                self.walk_block(body);
            }
            StatementKind::ForEach { body, .. } => self.walk_block(body),
            StatementKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_statement(then_branch);
                if let Some(eb) = else_branch {
                    self.walk_statement(eb);
                }
            }
            StatementKind::Case {
                when_branches,
                otherwise,
                ..
            } => {
                for wb in when_branches {
                    self.walk_block(&wb.body);
                }
                if let Some(o) = otherwise {
                    self.walk_block(o);
                }
            }
            StatementKind::Block(body) => self.walk_block(body),
            StatementKind::Label { body, .. } => self.walk_statement(body),
            StatementKind::PreprocIf(pif) => {
                self.walk_block(&pif.then_branch);
                for (_, br) in &pif.elseif_branches {
                    self.walk_block(br);
                }
                if let Some(eb) = &pif.else_branch {
                    self.walk_block(eb);
                }
            }
            _ => {}
        }
    }

    fn check_assignment(&mut self, target: &Expression, value: &Expression) {
        // Target type: look up the resolved symbol's declared type. For a
        // bare identifier target, that's the symbol's data_type.
        let target_ty = self.target_type(target);
        let value_ty = self.sem.types.get(value.id).cloned();
        if let (Some(to), Some(from)) = (target_ty, value_ty) {
            self.check_pair(&from, &to, value.id, &expr_span_or_default(target));
        }
    }

    fn target_type(&self, target: &Expression) -> Option<ResolvedType> {
        match &target.kind {
            ExpressionKind::Identifier(_) => match self.sem.references.get(target.id) {
                Some(Resolution::Resolved(sym)) => self.sem.symbols.get(*sym).data_type.clone(),
                _ => None,
            },
            _ => self.sem.types.get(target.id).cloned(),
        }
    }

    fn check_pair(
        &mut self,
        from: &ResolvedType,
        to: &ResolvedType,
        value_node: oxabl_ast::NodeId,
        span: &FileSpan,
    ) {
        // Skip if either is Unknown/Error (lattice bottom / poison).
        if matches!(from, ResolvedType::Unknown | ResolvedType::Error)
            || matches!(to, ResolvedType::Unknown | ResolvedType::Error)
        {
            return;
        }
        // Skip if the value expression is a cross-file reference — its type
        // is not known here, so any verdict would be guesswork. Named
        // exhaustively rather than with a wildcard: this early return is the
        // one place the compiler cannot flag a new reason for us, so an added
        // reason must be a compile error here too.
        if let Some(Resolution::Unresolved { reason, .. }) = self.sem.references.get(value_node) {
            match reason {
                UnresolvedReason::External
                | UnresolvedReason::NotFoundInWorkspace
                | UnresolvedReason::Unknowable => return,
                UnresolvedReason::NotInScope | UnresolvedReason::NoSchema => {}
            }
        }

        if !assignable(from, to) {
            self.diags.push(Diagnostic::error(
                LINT0004,
                format!("type mismatch: cannot assign `{:?}` to `{:?}`", from, to),
                *span,
            ));
        } else if is_narrowing_warning(from, to) {
            self.diags.push(Diagnostic::warning(
                LINT0004,
                format!(
                    "narrowing conversion: `{:?}` assigned to `{:?}` may discard data",
                    from, to
                ),
                *span,
            ));
        }
    }
}

fn init_span(expr: &Expression) -> FileSpan {
    FileSpan {
        file: oxabl_common::FileId::UNKNOWN,
        span: expr_span_inner(expr).unwrap_or(oxabl_ast::Span { start: 0, end: 0 }),
    }
}

fn expr_span_or_default(expr: &Expression) -> FileSpan {
    FileSpan {
        file: oxabl_common::FileId::UNKNOWN,
        span: expr_span_inner(expr).unwrap_or(oxabl_ast::Span { start: 0, end: 0 }),
    }
}

fn expr_span_inner(expr: &Expression) -> Option<oxabl_ast::Span> {
    match &expr.kind {
        ExpressionKind::Identifier(id) => Some(id.span),
        ExpressionKind::FieldAccess { field, .. } => Some(field.span),
        _ => None,
    }
}

// Silence an unused helper — severity enum is consumed only when a caller
// needs it for post-filter tests; keep the import stable for future use.
#[allow(dead_code)]
fn _keep_severity_in_scope(s: Severity) -> Severity {
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{
        DataType, Expression, ExpressionKind, Identifier, IntegerLiteral, Literal, NodeId, Span,
        Statement, StatementKind, StringLiteral, TypeSource,
    };
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use oxabl_semantic::analyze_file;
    use smallvec::SmallVec;
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
        Statement::with_id(next_nid(), oxabl_ast::Span::DUMMY, k)
    }
    fn expr_n(k: ExpressionKind) -> Expression {
        Expression::with_id(next_nid(), oxabl_ast::Span::DUMMY, k)
    }
    fn int_lit(v: i64) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: v,
        })))
    }
    fn str_lit(s: &str) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::String(StringLiteral {
            span: Span { start: 0, end: 1 },
            value: s.into(),
        })))
    }
    fn bool_lit(b: bool) -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Boolean(
            oxabl_ast::BooleanLiteral {
                span: Span { start: 0, end: 4 },
                value: b,
            },
        )))
    }
    fn unknown_lit() -> Expression {
        expr_n(ExpressionKind::Literal(Literal::Unknown(
            oxabl_ast::UnknownLiteral {
                span: Span { start: 0, end: 1 },
            },
        )))
    }
    fn id_expr(n: &str) -> Expression {
        expr_n(ExpressionKind::Identifier(id(n)))
    }

    fn analyze_and_lint(stmts: Vec<Statement>) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    fn var_with_init(n: &str, ty: DataType, init: Expression) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(ty),
            initial_value: Some(init),
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn var_decl(n: &str, ty: DataType) -> Statement {
        stmt_n(StatementKind::VariableDeclaration {
            name: id(n),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    #[test]
    fn widening_silent() {
        // Integer → Decimal is legal widening — no diagnostic.
        let diags = analyze_and_lint(vec![var_with_init("d", DataType::Decimal, int_lit(1))]);
        assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    }

    #[test]
    fn decimal_to_integer_is_silent_v1() {
        use rust_decimal::Decimal;
        let dec_expr = expr_n(ExpressionKind::Literal(Literal::Decimal(
            oxabl_ast::DecimalLiteral {
                span: Span { start: 0, end: 1 },
                value: Decimal::from(3),
            },
        )));
        let diags = analyze_and_lint(vec![var_with_init("n", DataType::Integer, dec_expr)]);
        assert!(
            diags.is_empty(),
            "Decimal→Integer is silent in v1: {diags:?}"
        );
    }

    #[test]
    fn type_mismatch_logical_to_integer_errors() {
        let diags = analyze_and_lint(vec![var_with_init("n", DataType::Integer, bool_lit(true))]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Severity::Error);
        assert_eq!(diags[0].code.0, LINT0004);
    }

    #[test]
    fn integer_to_logical_errors() {
        let diags = analyze_and_lint(vec![var_with_init("b", DataType::Logical, int_lit(1))]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Severity::Error);
    }

    #[test]
    fn narrowing_longchar_to_character_warns() {
        let diags = analyze_and_lint(vec![
            var_decl("lc", DataType::Longchar),
            stmt_n(StatementKind::Assignment {
                target: id_expr("c"),
                value: id_expr("lc"),
            }),
            var_decl("c", DataType::Character),
        ]);
        // c is declared *after*, but declare pass walks top-down and scope
        // is file-level for both; assignment target resolves `c` to the
        // Character var; rhs `lc` is Longchar.
        assert!(
            diags.iter().any(|d| d.severity == Severity::Warning),
            "expected narrowing warning: {diags:?}"
        );
    }

    #[test]
    fn skip_when_value_is_unknown_literal() {
        // `?` → any — universal bottom, no diagnostic.
        let diags = analyze_and_lint(vec![var_with_init("n", DataType::Integer, unknown_lit())]);
        assert!(diags.is_empty());
    }

    #[test]
    fn skip_when_value_is_unresolved_identifier() {
        // Unresolved identifier surfaces as Unknown type → no mismatch.
        let diags = analyze_and_lint(vec![var_with_init(
            "n",
            DataType::Integer,
            id_expr("ghost"),
        )]);
        assert!(diags.is_empty());
    }

    #[test]
    fn assign_statement_mismatch_errors() {
        let diags = analyze_and_lint(vec![
            var_decl("b", DataType::Logical),
            stmt_n(StatementKind::Assignment {
                target: id_expr("b"),
                value: int_lit(42),
            }),
        ]);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Severity::Error);
    }

    #[test]
    fn multi_assign_flags_each_bad_pair() {
        let mut v: SmallVec<[oxabl_ast::AssignPair; 4]> = SmallVec::new();
        v.push(oxabl_ast::AssignPair {
            target: id_expr("b"),
            value: int_lit(1),
        });
        v.push(oxabl_ast::AssignPair {
            target: id_expr("n"),
            value: str_lit("hi"),
        });
        let diags = analyze_and_lint(vec![
            var_decl("b", DataType::Logical),
            var_decl("n", DataType::Integer),
            stmt_n(StatementKind::Assign { assignments: v }),
        ]);
        assert_eq!(diags.len(), 2);
    }

    #[test]
    fn class_upcast_single_file_ok() {
        // Class identity match — no diagnostic on assigning Foo to Foo.
        let stmts = vec![
            stmt_n(StatementKind::Class {
                name: id("Foo"),
                inherits: None,
                implements: vec![],
                is_abstract: false,
                is_final: false,
                body: vec![],
            }),
            var_decl("a", DataType::Class("Foo".into())),
            var_decl("b", DataType::Class("Foo".into())),
            stmt_n(StatementKind::Assignment {
                target: id_expr("a"),
                value: id_expr("b"),
            }),
        ];
        let diags = analyze_and_lint(stmts);
        assert!(diags.is_empty(), "Foo = Foo must be silent: {diags:?}");
    }

    #[test]
    fn cross_file_class_assignment_silent() {
        // Unknown class (external) → data_type stays Unknown → skipped.
        let stmts = vec![
            var_decl("a", DataType::Class("External".into())),
            var_with_init("b", DataType::Integer, int_lit(1)),
        ];
        let diags = analyze_and_lint(stmts);
        assert!(diags.is_empty());
    }

    #[test]
    fn integer_widening_to_int64_silent() {
        let diags = analyze_and_lint(vec![var_with_init("n", DataType::Int64, int_lit(1))]);
        assert!(diags.is_empty());
    }

    #[test]
    fn no_diagnostic_when_no_initial_value() {
        let diags = analyze_and_lint(vec![var_decl("n", DataType::Integer)]);
        assert!(diags.is_empty());
    }

    #[test]
    fn character_assignment_ok() {
        let diags = analyze_and_lint(vec![var_with_init("s", DataType::Character, str_lit("hi"))]);
        assert!(diags.is_empty());
    }

    // ---- Schema-backed field types ---------------------------------------

    use oxabl_schema::test_support::customer_schema as test_schema;

    fn analyze_and_lint_with_schema(stmts: Vec<Statement>, schema: &Schema) -> Vec<Diagnostic> {
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", schema);
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    /// `DEFINE BUFFER bCust FOR Customer.` + `target = bCust.CustNum`.
    fn assign_from_custnum(target: &str, target_ty: DataType) -> Vec<Statement> {
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id("CustNum"),
        });
        vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: oxabl_ast::BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: oxabl_ast::XmlSerializeOptions::default(),
                is_new_shared: false,
                is_shared: false,
                is_new_global_shared: false,
            }),
            var_decl(target, target_ty),
            stmt_n(StatementKind::Assignment {
                target: id_expr(target),
                value: fa,
            }),
        ]
    }

    #[test]
    fn no_false_positive_on_matching_field_type() {
        // `i = bCust.CustNum` — the field types as INTEGER from the schema
        // (previously Unknown → skipped), so a matching assignment stays
        // silent for the *right* reason.
        let schema = test_schema();
        let diags =
            analyze_and_lint_with_schema(assign_from_custnum("i", DataType::Integer), &schema);
        assert!(diags.is_empty(), "unexpected diags: {diags:?}");
    }

    #[test]
    fn fires_on_mismatched_field_type() {
        // Companion proof that the field type flows: `c = bCust.CustNum`
        // (CHARACTER ← INTEGER) is a genuine mismatch — only diagnosable
        // because the field access no longer types as Unknown.
        let schema = test_schema();
        let diags =
            analyze_and_lint_with_schema(assign_from_custnum("c", DataType::Character), &schema);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, LINT0004);
    }
}
