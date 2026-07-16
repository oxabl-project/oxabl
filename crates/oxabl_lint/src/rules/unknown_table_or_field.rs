//! `unknown-table-or-field` lint (LINT0003).
//!
//! Fires on field-access references whose resolution is
//! [`Resolution::Unresolved { reason: NoSchema | NotInScope }`] *and* the
//! context has `schema_loaded == true`. When schema is absent, this rule
//! emits zero diagnostics — matching the R7 "schema is first-class but
//! optional" decision.
//!
//! The rule became live with schema-backed resolution: the resolve pass
//! validates fields against the loaded schema (buffer → `table_id` →
//! `Table::get_field`), emitting `Resolved` for real fields and
//! `NotInScope` for absent ones — so a `bCust.NoSuchField` or
//! `Ghost.Field` under a loaded schema now fires here. Fields the
//! resolver cannot check (temp-table buffers, `External` resolutions)
//! stay silent by design.

use oxabl_ast::{Expression, ExpressionKind, Statement, StatementKind, StreamOperation};
use oxabl_common::{Diagnostic, FileSpan};
use oxabl_semantic::{AnalysisContext, Resolution, Semantic, UnresolvedReason};

use super::LINT0003;

/// Entry point.
pub fn run(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    if !ctx.schema_loaded {
        return Vec::new();
    }
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
            StatementKind::VariableDeclaration {
                initial_value: Some(e),
                ..
            } => {
                self.walk_expression(e);
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
            StatementKind::DefineEvent { parameters, .. } => self.walk_block(parameters),
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
            StatementKind::Return(Some(e)) => self.walk_expression(e),
            StatementKind::Throw(e) => self.walk_expression(e),
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
                arguments, target, ..
            } => {
                if let oxabl_ast::RunTarget::Dynamic(e) = target {
                    self.walk_expression(e);
                }
                for arg in arguments {
                    self.walk_expression(&arg.expression);
                }
            }
            StatementKind::Publish {
                event_name,
                arguments,
                ..
            } => {
                self.walk_expression(event_name);
                for arg in arguments {
                    self.walk_expression(&arg.expression);
                }
            }
            StatementKind::Subscribe { event_name, .. } => self.walk_expression(event_name),
            StatementKind::StreamIo { operation, .. } => match operation {
                StreamOperation::From(e) | StreamOperation::Through(e) => self.walk_expression(e),
                StreamOperation::To { target, .. } => self.walk_expression(target),
                StreamOperation::Close => {}
            },
            StatementKind::BufferCopy { assignments, .. } => {
                for p in assignments {
                    self.walk_expression(&p.target);
                    self.walk_expression(&p.value);
                }
            }
            _ => {}
        }
    }

    fn walk_expression(&mut self, expr: &Expression) {
        // Target is *field access*: the composite Expression's id carries the
        // field's resolution (NoSchema in v1 when schema isn't loaded;
        // NotInScope when schema is loaded but the field isn't found).
        if let ExpressionKind::FieldAccess { qualifier, field } = &expr.kind {
            // Walk children first (nested FieldAccess / complex qualifier).
            self.walk_expression(qualifier);

            if let Some(Resolution::Unresolved { reason, .. }) = self.sem.references.get(expr.id)
                && matches!(
                    reason,
                    UnresolvedReason::NoSchema | UnresolvedReason::NotInScope
                )
            {
                self.diags.push(Diagnostic::error(
                    LINT0003,
                    format!("unknown field `{}`", field.name),
                    FileSpan {
                        file: self.ctx.file_id,
                        span: field.span,
                    },
                ));
            }
            return;
        }
        // Descend for other expression kinds.
        match &expr.kind {
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
            ExpressionKind::FunctionCall { arguments, .. } => {
                for a in arguments {
                    self.walk_expression(a);
                }
            }
            ExpressionKind::MethodCall {
                object, arguments, ..
            } => {
                self.walk_expression(object);
                for a in arguments {
                    self.walk_expression(a);
                }
            }
            ExpressionKind::MemberAccess { object, .. } => self.walk_expression(object),
            ExpressionKind::ArrayAccess { array, index } => {
                self.walk_expression(array);
                self.walk_expression(index);
            }
            ExpressionKind::New { arguments, .. } => {
                for a in arguments {
                    self.walk_expression(a);
                }
            }
            ExpressionKind::CanFind { where_clause, .. } => {
                if let Some(w) = where_clause {
                    self.walk_expression(w);
                }
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
            ExpressionKind::Identifier(_)
            | ExpressionKind::Literal(_)
            | ExpressionKind::IncludeReference { .. }
            | ExpressionKind::IncludeArgReference { .. }
            | ExpressionKind::PreprocReference(_)
            | ExpressionKind::FieldAccess { .. } => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{
        BufferTarget, DataType, Expression, ExpressionKind, Identifier, NodeId, Span, Statement,
        StatementKind, TypeSource, XmlSerializeOptions,
    };
    use oxabl_common::FileId;
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
    fn id_expr(n: &str) -> Expression {
        expr_n(ExpressionKind::Identifier(id(n)))
    }

    fn lint_with(stmts: Vec<Statement>, schema_loaded: bool) -> Vec<Diagnostic> {
        let schema = Schema::empty();
        let mut ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        ctx.schema_loaded = schema_loaded;
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    use oxabl_schema::test_support::customer_schema as test_schema;

    fn lint_with_schema(stmts: Vec<Statement>, schema: &Schema) -> Vec<Diagnostic> {
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", schema);
        let sem = analyze_file(&stmts, &ctx);
        run(&stmts, &sem, &ctx)
    }

    /// `DEFINE BUFFER bCust FOR Customer.` + `bCust.<field>`.
    fn buffer_customer_stmts(field_name: &str) -> Vec<Statement> {
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id(field_name),
        });
        vec![
            stmt_n(StatementKind::DefineBuffer {
                name: id("bCust"),
                target: BufferTarget::Table(id("Customer")),
                preselect: false,
                label: None,
                xml_options: XmlSerializeOptions::default(),
            }),
            stmt_n(StatementKind::ExpressionStatement(fa)),
        ]
    }

    #[test]
    fn skip_list_no_fire_when_schema_not_loaded() {
        // `Customer.CustNum` with no schema → rule emits nothing.
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("Customer")),
            field: id("CustNum"),
        });
        let diags = lint_with(vec![stmt_n(StatementKind::ExpressionStatement(fa))], false);
        assert!(diags.is_empty());
    }

    #[test]
    fn no_fire_when_qualifier_resolves_to_buffer_under_schema() {
        // DEFINE BUFFER bCust FOR Customer. + bCust.CustNum with the loaded
        // flag forced on an EMPTY schema → the buffer has no schema link,
        // the field keeps the legacy External resolution, LINT0003 stays
        // silent (unverifiable fields are not flagged).
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("bCust")),
            field: id("CustNum"),
        });
        let diags = lint_with(
            vec![
                stmt_n(StatementKind::DefineBuffer {
                    name: id("bCust"),
                    target: BufferTarget::Table(id("Customer")),
                    preselect: false,
                    label: None,
                    xml_options: XmlSerializeOptions::default(),
                }),
                stmt_n(StatementKind::ExpressionStatement(fa)),
            ],
            true,
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn fires_on_unknown_field_when_schema_loaded() {
        let schema = test_schema();
        let diags = lint_with_schema(buffer_customer_stmts("BadField"), &schema);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, LINT0003);
        assert!(diags[0].message.contains("BadField"));
    }

    #[test]
    fn no_fire_on_valid_field_when_schema_loaded() {
        let schema = test_schema();
        let diags = lint_with_schema(buffer_customer_stmts("CustNum"), &schema);
        assert!(diags.is_empty());
    }

    #[test]
    fn no_fire_on_temp_table_field() {
        // Temp-table fields resolve locally; the qualifier has no schema
        // link, so LINT0003 stays silent even under a loaded schema.
        let schema = test_schema();
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("tt")),
            field: id("f"),
        });
        let diags = lint_with_schema(
            vec![
                stmt_n(StatementKind::DefineTempTable {
                    name: id("tt"),
                    no_undo: false,
                    like_table: None,
                    validate: false,
                    use_indexes: vec![],
                    fields: vec![oxabl_ast::TempTableField {
                        name: id("f"),
                        type_source: TypeSource::Explicit(DataType::Integer),
                        validate: false,
                        initial_value: None,
                        extent: None,
                    }],
                    indexes: vec![],
                    xml_options: XmlSerializeOptions::default(),
                }),
                stmt_n(StatementKind::ExpressionStatement(fa)),
            ],
            &schema,
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn no_fire_on_bare_valid_qualifier_field() {
        // The CRITICAL fix, viewed from the lint side: `Customer.Name` with
        // NO `DEFINE BUFFER`, schema loaded → qualifier binds a synthesized
        // default buffer, field resolves — no LINT0003.
        let schema = test_schema();
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("Customer")),
            field: id("Name"),
        });
        let diags = lint_with_schema(
            vec![stmt_n(StatementKind::ExpressionStatement(fa))],
            &schema,
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn fires_on_bare_unknown_table_field() {
        // `Ghost.Field`, schema loaded, no buffer → qualifier is not a
        // schema table (stays NotInScope), field is NotInScope → fires.
        let schema = test_schema();
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("Ghost")),
            field: id("Field"),
        });
        let diags = lint_with_schema(
            vec![stmt_n(StatementKind::ExpressionStatement(fa))],
            &schema,
        );
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, LINT0003);
    }

    #[test]
    fn fires_on_unknown_qualifier_when_schema_loaded() {
        // `Ghost.Field` with schema loaded — qualifier is NotInScope, field
        // surfaces as NotInScope → LINT0003 emits.
        let fa = expr_n(ExpressionKind::FieldAccess {
            qualifier: Box::new(id_expr("Ghost")),
            field: id("Field"),
        });
        let diags = lint_with(vec![stmt_n(StatementKind::ExpressionStatement(fa))], true);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].code.0, LINT0003);
    }

    #[test]
    fn no_fire_on_non_field_expressions() {
        use oxabl_ast::{IntegerLiteral, Literal};
        let lit = expr_n(ExpressionKind::Literal(Literal::Integer(IntegerLiteral {
            span: Span { start: 0, end: 1 },
            value: 42,
        })));
        let diags = lint_with(vec![stmt_n(StatementKind::ExpressionStatement(lit))], true);
        assert!(diags.is_empty());
    }

    #[test]
    fn no_fire_on_local_variable_reference() {
        let u = id_expr("x");
        let diags = lint_with(
            vec![
                stmt_n(StatementKind::VariableDeclaration {
                    name: id("x"),
                    type_source: TypeSource::Explicit(DataType::Integer),
                    initial_value: None,
                    no_undo: false,
                    extent: None,
                }),
                stmt_n(StatementKind::ExpressionStatement(u)),
            ],
            true,
        );
        assert!(diags.is_empty());
    }
}
