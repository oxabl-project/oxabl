//! Which record a field reference reads from.
//!
//! A record phrase names a buffer, not a table. `FOR EACH foo` over a
//! `DEFINE BUFFER foo FOR bar` reads `bar`, even when a schema table is also
//! called `foo`. The same holds for a temp-table that shares a schema table's
//! name. Driven through the full pipeline.
//!
//! Every fixture here is synthetic.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0003, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::schema_from_df};
use oxabl_semantic::{AnalysisContext, Semantic, SymbolKind, analyze_file};

const DF: &str = r#"
ADD TABLE "foo"
ADD FIELD "alpha" OF "foo" AS character

ADD TABLE "bar"
ADD FIELD "gamma" OF "bar" AS character
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

fn analyze(source: &str, schema: &Schema) -> (Semantic, Vec<Diagnostic>) {
    let stmts = parse(source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    ctx.schema_loaded = !schema.is_empty();
    let sem = analyze_file(&stmts, &ctx);
    let diags = lint_file(&stmts, &sem, &ctx);
    (sem, diags)
}

fn lint(source: &str) -> Vec<Diagnostic> {
    analyze(source, &schema_from_df(DF)).1
}

fn messages(diags: &[Diagnostic], code: &str) -> Vec<String> {
    diags
        .iter()
        .filter(|d| d.code.0 == code)
        .map(|d| d.message.clone())
        .collect()
}

// ---------------------------------------------------------------------------
// A record phrase names a buffer
// ---------------------------------------------------------------------------

#[test]
fn for_each_over_a_buffer_named_like_another_table_reads_its_own_table() {
    let diags = lint(
        "DEFINE BUFFER foo FOR bar.
FOR EACH foo NO-LOCK:
    MESSAGE foo.gamma.
    MESSAGE foo.alpha.
END.
",
    );
    assert_eq!(
        messages(&diags, LINT0003),
        vec!["unknown field `alpha`".to_string()]
    );
}

#[test]
fn for_first_over_a_buffer_named_like_another_table_reads_its_own_table() {
    let diags = lint(
        "DEFINE BUFFER foo FOR bar.
FOR FIRST foo NO-LOCK:
    MESSAGE foo.gamma.
    MESSAGE foo.alpha.
END.
",
    );
    assert_eq!(
        messages(&diags, LINT0003),
        vec!["unknown field `alpha`".to_string()]
    );
}

#[test]
fn for_each_over_a_buffer_with_its_own_name_still_checks_its_fields() {
    let diags = lint(
        "DEFINE BUFFER b2 FOR bar.
FOR EACH b2 NO-LOCK:
    MESSAGE b2.gamma.
    MESSAGE b2.alpha.
END.
",
    );
    assert_eq!(
        messages(&diags, LINT0003),
        vec!["unknown field `alpha`".to_string()]
    );
}

#[test]
fn for_each_over_a_temp_table_named_like_a_schema_table_reads_the_temp_table() {
    let diags = lint(
        "DEFINE TEMP-TABLE foo NO-UNDO FIELD delta AS CHARACTER.
FOR EACH foo NO-LOCK:
    MESSAGE foo.delta.
END.
",
    );
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
}

#[test]
fn for_each_over_a_temp_table_credits_the_temp_table() {
    let (sem, _) = analyze(
        "DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS INTEGER.
FOR EACH tt NO-LOCK:
END.
",
        &Schema::empty(),
    );
    let tt = sem
        .symbols
        .iter()
        .filter(|(_, s)| s.name.as_ref() == "tt")
        .map(|(_, s)| s)
        .collect::<Vec<_>>();
    assert_eq!(tt.len(), 1, "FOR EACH must not mint a second `tt`");
    assert_eq!(tt[0].kind, SymbolKind::TempTable);
    assert_eq!(tt[0].read_count, 1);
}
