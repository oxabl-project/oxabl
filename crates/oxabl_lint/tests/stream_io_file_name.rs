//! A bare unquoted name in an `OUTPUT`/`INPUT` I/O phrase is a *literal file
//! name*, not an expression.
//!
//! `OUTPUT TO report.txt.` must not read as the field `txt` of a table named
//! `report`. Before this, that is exactly what it did: the semantic pass
//! recorded a table reference and `unknown-table-or-field` fired.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::lint_file;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_schema::test_support::schema_from_df;
use oxabl_semantic::{AnalysisContext, Resolution, Semantic, analyze_file};

/// A schema with a table named `report` that has no field named `txt`.
const REPORT_DF: &str = r#"
ADD TABLE "report"
ADD FIELD "amount" OF "report" AS integer
"#;

/// Parse `source` cleanly and analyze it under `schema`.
fn analyze(schema: &Schema, source: &str) -> (Vec<Statement>, Semantic) {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}",
        program.errors
    );
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    let sem = analyze_file(&program.statements, &ctx);
    (program.statements, sem)
}

/// Every diagnostic `source` draws under `schema`.
fn lint(schema: &Schema, source: &str) -> Vec<Diagnostic> {
    let (stmts, sem) = analyze(schema, source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    lint_file(&stmts, &sem, &ctx)
}

/// Every name of a resolved reference in `sem`.
fn resolved_names(sem: &Semantic) -> Vec<String> {
    sem.references
        .iter()
        .filter_map(|(_, r)| match r {
            Resolution::Resolved(sym) => Some(sem.symbols.get(*sym).name.to_string()),
            _ => None,
        })
        .collect()
}

#[test]
fn output_to_unquoted_file_name_is_not_a_table_reference() {
    let schema = schema_from_df(REPORT_DF);
    let source = "OUTPUT TO report.txt.";

    let diags = lint(&schema, source);
    assert!(diags.is_empty(), "a file name must not lint: {diags:?}");

    let (_, sem) = analyze(&schema, source);
    let names = resolved_names(&sem);
    assert!(
        !names.iter().any(|n| n == "report"),
        "file name recorded a table reference: {names:?}"
    );
}

#[test]
fn input_from_unquoted_file_name_is_not_a_table_reference() {
    let schema = schema_from_df(REPORT_DF);
    let source = "INPUT FROM report.txt.";

    let diags = lint(&schema, source);
    assert!(diags.is_empty(), "a file name must not lint: {diags:?}");

    let (_, sem) = analyze(&schema, source);
    let names = resolved_names(&sem);
    assert!(
        !names.iter().any(|n| n == "report"),
        "file name recorded a table reference: {names:?}"
    );
}

#[test]
fn output_to_value_still_reads_its_expression() {
    let schema = schema_from_df(REPORT_DF);
    let source = "DEFINE VARIABLE v-file AS CHARACTER NO-UNDO.\nOUTPUT TO VALUE(v-file).";

    let diags = lint(&schema, source);
    assert!(diags.is_empty(), "VALUE(expr) must stay clean: {diags:?}");

    let (_, sem) = analyze(&schema, source);
    let names = resolved_names(&sem);
    assert!(
        names.iter().any(|n| n == "v-file"),
        "VALUE argument must resolve as a read: {names:?}"
    );
}
