//! A field spelled like a keyword counts as an operand after `+`.

const SCHEMA: &str = "ADD TABLE \"Item\"\nADD FIELD \"Title\" OF \"Item\" AS decimal\n";

fn refs_of_item(source: &str) -> String {
    let schema = oxabl_schema::test_support::schema_from_df(SCHEMA);
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let dump = oxabl_scope::analyze_file(&program.statements, source, Some(&schema)).dump(source);
    dump.lines()
        .find(|l| l.starts_with("buffer Item"))
        .expect("buffer line")
        .to_string()
}

#[test]
fn a_keyword_spelled_field_after_a_plus_is_a_reference() {
    let src =
        "DEFINE VARIABLE x AS DECIMAL NO-UNDO.\nFOR EACH Item NO-LOCK:\n  x = 1 + title.\nEND.\n";
    assert!(
        refs_of_item(src).ends_with("refs=2"),
        "{}",
        refs_of_item(src)
    );
}
