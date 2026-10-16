//! Snapshot tests: every `fixtures/<name>.p` has its dump in
//! `fixtures/<name>.nulls`. Set `OXABL_BLESS=1` to rewrite them. The schema
//! is `fixtures/schema.df`; a fixture whose first line is `/* no-schema */`
//! runs without one.

use std::path::Path;

use oxabl_nulls::NullOptions;
use oxabl_scope::ScopeOptions;

fn dump(source: &str, schema: Option<&oxabl_schema::Schema>) -> String {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture does not parse cleanly: {:?}",
        program.errors
    );
    let options = ScopeOptions::default();
    let flow = oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &options.flow);
    let scopes = oxabl_scope::analyze_flow(
        &program.statements,
        source,
        &tokens,
        &flow,
        schema,
        &options,
    );
    oxabl_nulls::analyze_file(source, &tokens, &flow, &scopes, &NullOptions::default())
        .with_values(
            source,
            &tokens,
            &program.statements,
            &flow,
            &NullOptions::default(),
        )
        .dump(source, &scopes)
}

#[test]
fn fixtures_match_their_snapshots() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
    let df = std::fs::read_to_string(dir.join("schema.df")).expect("schema.df");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let bless = std::env::var_os("OXABL_BLESS").is_some();
    let mut entries: Vec<_> = std::fs::read_dir(&dir)
        .expect("fixtures dir")
        .map(|e| e.expect("entry").path())
        .filter(|p| matches!(p.extension().and_then(|e| e.to_str()), Some("p" | "cls")))
        .collect();
    entries.sort();
    assert!(!entries.is_empty());
    let mut failures = Vec::new();
    for path in entries {
        let source = std::fs::read_to_string(&path).expect("fixture");
        let schema = (!source.starts_with("/* no-schema */")).then_some(&schema);
        let actual = dump(&source, schema);
        let expected_path = path.with_extension("nulls");
        if bless {
            std::fs::write(&expected_path, &actual).expect("write snapshot");
            continue;
        }
        let expected = std::fs::read_to_string(&expected_path).unwrap_or_default();
        if expected != actual {
            failures.push(format!(
                "{}: snapshot differs\n--- expected\n{expected}\n--- actual\n{actual}",
                path.display()
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
