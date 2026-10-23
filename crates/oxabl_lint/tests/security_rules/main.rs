//! End-to-end coverage for the `security` rules (ABLSEC001-ABLSEC005).
//!
//! Synthetic ABL runs through the real preprocessor, lexer and parser, then
//! through the rule, so the tests pin what the parser really produces for each
//! sink. `corpus.rs` runs the fixture files under `tests/security_corpus`.

mod ablsec001_dynamic_query;
mod ablsec002_foreign_sql;
mod ablsec003_os_command;
mod ablsec004_dynamic_code;
mod ablsec005_path_traversal;
mod corpus;
mod group;
mod literal;

use std::path::PathBuf;

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::security::{dynamic_code, dynamic_query, foreign_sql, os_command, path_traversal};
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

pub type RuleFn =
    fn(&[oxabl_ast::Statement], &oxabl_semantic::Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// The five rules, in code order.
pub const RULES: [(&str, RuleFn); 5] = [
    ("ABLSEC001", dynamic_query::run),
    ("ABLSEC002", foreign_sql::run),
    ("ABLSEC003", os_command::run),
    ("ABLSEC004", dynamic_code::run),
    ("ABLSEC005", path_traversal::run),
];

/// Run one rule over already-expanded `text`, requiring a clean parse.
pub fn run_text(rule: RuleFn, text: &str) -> Vec<Diagnostic> {
    let tokens = tokenize(text);
    let program = Parser::new(&tokens, text).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}\n{text}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, text, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    rule(&program.statements, &sem, &ctx)
}

/// Run one rule over `source` with no preprocessing.
pub fn check(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    run_text(rule, source)
}

/// Preprocess `main` against in-memory include files under `/src`, then run
/// one rule over the expansion, the way every client does.
pub fn check_expanded(rule: RuleFn, files: &[(&str, &str)], main: &str) -> Vec<Diagnostic> {
    let mut fs = InMemoryFileSystem::new();
    for (name, contents) in files {
        fs.insert(PathBuf::from("/src").join(name), *contents);
    }
    let paths = [PathBuf::from("/src")];
    let expanded = Preprocessor::new(&fs, &paths)
        .process(FileId::new(1), main)
        .unwrap_or_else(|d| panic!("preprocess failed: {d:?}"));
    run_text(rule, &expanded.to_text())
}

/// The text each diagnostic points at.
pub fn flagged<'a>(source: &'a str, diags: &[Diagnostic]) -> Vec<&'a str> {
    diags
        .iter()
        .map(|d| &source[d.span.span.start as usize..d.span.span.end as usize])
        .collect()
}
