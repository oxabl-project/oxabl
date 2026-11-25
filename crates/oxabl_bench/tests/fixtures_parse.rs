//! Every bench input parses with no error, so a bench measures the parse and
//! not the error recovery.

use oxabl_bench::fixtures;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_pipeline::LintPipeline;

fn errors(source: &str) -> Vec<String> {
    let tokens = tokenize(source);
    Parser::new(&tokens, source)
        .parse_program()
        .errors
        .iter()
        .map(|e| format!("{e:?}"))
        .collect()
}

#[test]
fn every_bench_fixture_of_resources_parses() {
    let mut found = Vec::new();
    let mut names: Vec<String> = std::fs::read_dir(fixtures::resources_dir())
        .expect("resources/")
        .map(|entry| {
            entry
                .expect("an entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .filter(|name| name.starts_with("bench_") && name.ends_with(".abl"))
        .collect();
    names.sort();
    assert!(names.len() >= 20, "{names:?}");
    for name in &names {
        let mut errors = errors(&fixtures::resource(name));
        if name == "bench_parser_temp_tables.abl" {
            // The input of the bench `parser/temp_tables`, kept unchanged: it
            // names a field `status`, a reserved keyword. Exactly that error,
            // and `bench_parser_temp_tables_v2.abl` is the corrected copy.
            assert_eq!(
                errors,
                [
                    "ParseError { message: \"Expected identifier\", span: Span { start: 477, end: 483 } }"
                ],
                "{name}"
            );
            errors.clear();
        }
        for error in errors {
            found.push(format!("{name}: {error}"));
        }
    }
    assert!(found.is_empty(), "{}", found.join("\n"));
}

#[test]
fn the_large_inputs_parse() {
    assert!(errors(&fixtures::large_mixed()).is_empty());
    assert!(errors(&fixtures::nested_blocks(400, 8)).is_empty());
    let (fs, config, path, source) = fixtures::lsp_bench();
    let expansion = LintPipeline::new(&config, &fs)
        .with_file(&path)
        .expand(&source);
    assert!(expansion.unresolved_includes().is_empty());
    assert!(errors(expansion.text().expect("it preprocesses")).is_empty());
}
