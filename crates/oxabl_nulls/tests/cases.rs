//! Small cases of the failed-`FIND` model, one claim each: the lines where
//! a field is reported, against `fixtures/schema.df`.

use oxabl_nulls::NullOptions;
use oxabl_scope::ScopeOptions;

const HEAD: &str = "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
                    DEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
                    DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS CHARACTER.\n\
                    DEFINE QUERY q FOR Customer.\n\
                    DEFINE FRAME f Customer.CustNum Customer.Name.\n";

/// The 1-based lines of `body` (after the shared definitions) with a
/// finding.
fn missing(body: &str) -> Vec<usize> {
    let source = format!("{HEAD}{body}");
    let df = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/schema.df"),
    )
    .expect("schema.df");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let tokens = oxabl_lexer::tokenize(&source);
    let program = oxabl_parser::Parser::new(&tokens, &source).parse_program();
    assert!(
        program.errors.is_empty(),
        "case does not parse cleanly: {:?}",
        program.errors
    );
    let options = ScopeOptions::default();
    let flow = oxabl_flow::build_file_tokens(&program.statements, &source, &tokens, &options.flow);
    let scopes = oxabl_scope::analyze_flow(
        &program.statements,
        &source,
        &tokens,
        &flow,
        Some(&schema),
        &options,
    );
    let nulls =
        oxabl_nulls::analyze_file(&source, &tokens, &flow, &scopes, &NullOptions::default());
    let head = HEAD.lines().count();
    nulls
        .missing()
        .iter()
        .map(|m| source[..m.at.start as usize].lines().count() - head)
        .collect()
}

#[test]
fn a_temp_table_in_a_statement_the_model_does_not_know_is_unknown() {
    let body = "FIND FIRST Item NO-LOCK.\n\
                FIND FIRST tt NO-ERROR.\n\
                RAW-TRANSFER Item TO tt.\n\
                c = tt.k.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
    let body = "FIND FIRST tt NO-ERROR.\n\
                tt-load tt.\n\
                c = tt.k.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
}
