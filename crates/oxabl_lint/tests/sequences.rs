//! Database sequences under a loaded schema.
//!
//! `NEXT-VALUE(seq)` and friends take a sequence name, not a symbol. A sequence
//! the schema declares is silent, one it does not declare is `LINT0003`, and
//! with no schema loaded nothing is reported at all.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0003, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::schema_from_df};
use oxabl_semantic::{
    AnalysisContext, Resolution, SymbolKind, UnresolvedReason, analyze_file, sequence_operand,
};

const DF: &str = r#"
ADD TABLE "Customer"
ADD FIELD "CustNum" OF "Customer" AS integer

ADD SEQUENCE "Cust-Seq"
  INITIAL 1
  INCREMENT 1
  CYCLE-ON-LIMIT no
"#;

fn parse(source: &str) -> Vec<Statement> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}",
        program.errors
    );
    program.statements
}

fn lint(source: &str, schema: &Schema, schema_loaded: bool) -> Vec<Diagnostic> {
    let stmts = parse(source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    ctx.schema_loaded = schema_loaded;
    let sem = analyze_file(&stmts, &ctx);
    lint_file(&stmts, &sem, &ctx)
}

fn with_schema(source: &str) -> Vec<Diagnostic> {
    lint(source, &schema_from_df(DF), true)
}

fn summary(diags: &[Diagnostic]) -> Vec<String> {
    diags
        .iter()
        .map(|d| format!("{}: {}", d.code.0, d.message))
        .collect()
}

#[test]
fn declared_sequence_is_silent_in_every_form() {
    let diags = with_schema(
        r#"DEFINE VARIABLE i AS INT64 NO-UNDO.
i = NEXT-VALUE(Cust-Seq).
i = CURRENT-VALUE(cust-seq).
i = NEXT-VALUE(Cust-Seq, mydb).
i = CURRENT-VALUE(mydb.Cust-Seq).
i = DYNAMIC-NEXT-VALUE("Cust-Seq", "mydb").
i = DYNAMIC-CURRENT-VALUE("CUST-SEQ", "mydb").
MESSAGE i.
"#,
    );
    assert!(diags.is_empty(), "{:#?}", summary(&diags));
}

#[test]
fn unknown_sequence_is_reported_once_as_unknown_table_or_field() {
    let diags = with_schema(
        r#"DEFINE VARIABLE i AS INT64 NO-UNDO.
i = NEXT-VALUE(Cust-Sq).
MESSAGE i.
"#,
    );
    assert_eq!(
        summary(&diags),
        vec![format!("{LINT0003}: unknown sequence `Cust-Sq`")]
    );
}

#[test]
fn unknown_sequence_in_each_other_form_is_reported() {
    for (call, name) in [
        ("CURRENT-VALUE(nope)", "nope"),
        ("NEXT-VALUE(mydb.nope)", "nope"),
        ("NEXT-VALUE(nope, mydb)", "nope"),
        ("DYNAMIC-NEXT-VALUE(\"nope\", \"mydb\")", "nope"),
        ("DYNAMIC-CURRENT-VALUE(\"nope\", \"mydb\")", "nope"),
    ] {
        let source = format!("DEFINE VARIABLE i AS INT64 NO-UNDO.\ni = {call}.\nMESSAGE i.\n");
        let diags = with_schema(&source);
        assert_eq!(
            summary(&diags),
            vec![format!("{LINT0003}: unknown sequence `{name}`")],
            "{call}"
        );
    }
}

#[test]
fn the_logical_database_name_is_not_an_undefined_symbol() {
    let diags = with_schema(
        "DEFINE VARIABLE i AS INT64 NO-UNDO.\ni = NEXT-VALUE(Cust-Seq, mydb).\nMESSAGE i.\n",
    );
    assert!(
        !diags.iter().any(|d| d.code.0 == LINT0001),
        "{:#?}",
        summary(&diags)
    );
}

#[test]
fn no_schema_means_no_finding() {
    let source = r#"DEFINE VARIABLE i AS INT64 NO-UNDO.
i = NEXT-VALUE(whatever).
i = CURRENT-VALUE(mydb.whatever).
i = NEXT-VALUE(whatever, mydb).
i = DYNAMIC-NEXT-VALUE("whatever").
MESSAGE i.
"#;
    let diags = lint(source, &Schema::empty(), false);
    assert!(diags.is_empty(), "{:#?}", summary(&diags));
}

#[test]
fn a_sequence_only_df_counts_as_a_loaded_schema() {
    let schema = schema_from_df("ADD SEQUENCE \"Only\"\n  INITIAL 1\n");
    let source = "DEFINE VARIABLE i AS INT64 NO-UNDO.\ni = NEXT-VALUE(only).\ni = NEXT-VALUE(typo).\nMESSAGE i.\n";
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    assert!(ctx.schema_loaded);
    let sem = analyze_file(&stmts, &ctx);
    let diags = lint_file(&stmts, &sem, &ctx);
    assert_eq!(
        summary(&diags),
        vec![format!("{LINT0003}: unknown sequence `typo`")]
    );
}

#[test]
fn a_variable_operand_of_a_dynamic_form_is_still_a_symbol() {
    let diags = with_schema(
        "DEFINE VARIABLE i AS INT64 NO-UNDO.\ni = DYNAMIC-NEXT-VALUE(undeclared-name, \"mydb\").\nMESSAGE i.\n",
    );
    assert_eq!(
        summary(&diags),
        vec![format!("{LINT0001}: undefined symbol `undeclared-name`")]
    );
}

/// The resolution recorded on each sequence operand of `source`, in order.
fn operand_resolutions(source: &str, schema: &Schema, schema_loaded: bool) -> Vec<Resolution> {
    let stmts = parse(source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    ctx.schema_loaded = schema_loaded;
    let sem = analyze_file(&stmts, &ctx);
    let mut out = Vec::new();
    for stmt in &stmts {
        if let oxabl_ast::StatementKind::Assignment { value, .. } = &stmt.kind
            && let oxabl_ast::ExpressionKind::FunctionCall { name, arguments } = &value.kind
            && let Some(op) = sequence_operand(name, arguments)
        {
            let r = sem.references.get(op.expr.id).expect("operand resolved");
            if let Resolution::Resolved(sym) = r {
                assert_eq!(sem.symbols.get(*sym).kind, SymbolKind::Sequence);
            }
            out.push(r.clone());
        }
    }
    out
}

#[test]
fn operands_resolve_to_one_sequence_symbol() {
    let rs = operand_resolutions(
        r#"DEFINE VARIABLE i AS INT64 NO-UNDO.
i = NEXT-VALUE(Cust-Seq).
i = DYNAMIC-CURRENT-VALUE("cust-seq").
i = NEXT-VALUE(nope).
"#,
        &schema_from_df(DF),
        true,
    );
    let [
        Resolution::Resolved(a),
        Resolution::Resolved(b),
        Resolution::Unresolved { reason, .. },
    ] = rs.as_slice()
    else {
        panic!("unexpected resolutions: {rs:?}");
    };
    assert_eq!(a, b);
    assert_eq!(*reason, UnresolvedReason::NotInScope);
}

#[test]
fn operands_without_a_schema_are_no_schema() {
    let rs = operand_resolutions(
        "DEFINE VARIABLE i AS INT64 NO-UNDO.
i = NEXT-VALUE(Cust-Seq).
",
        &Schema::empty(),
        false,
    );
    assert!(matches!(
        rs.as_slice(),
        [Resolution::Unresolved {
            reason: UnresolvedReason::NoSchema,
            ..
        }]
    ));
}
