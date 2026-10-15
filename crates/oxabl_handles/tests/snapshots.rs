//! Snapshot tests: every `fixtures/<name>.p` (or `.cls`) has its lifetime dump
//! in `fixtures/<name>.handles`. Set `OXABL_BLESS=1` to rewrite them.

use std::path::Path;

fn dump(source: &str) -> String {
    let tokens = oxabl_lexer::tokenize(source);
    let mut parser = oxabl_parser::Parser::new(&tokens, source);
    let program = parser.parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture does not parse cleanly: {:?}",
        program.errors
    );
    oxabl_handles::analyze_file(&program.statements, source).dump(source)
}

#[test]
fn fixtures_match_their_snapshots() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");
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
        let actual = dump(&source);
        let expected_path = path.with_extension("handles");
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
