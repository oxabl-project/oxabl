//! End-to-end coverage for the `security` rules (ABLSEC001-ABLSEC009, ABLSEC013-ABLSEC017).
//!
//! Synthetic ABL runs through the real preprocessor, lexer and parser, then
//! through the rule, so the tests pin what the parser really produces for each
//! sink. `corpus.rs` runs the fixture files under `tests/security_corpus`.

mod ablsec001_dynamic_query;
mod ablsec002_foreign_sql;
mod ablsec003_os_command;
mod ablsec004_dynamic_code;
mod ablsec005_path_traversal;
mod ablsec006_xss_output;
mod ablsec007_header_injection;
mod ablsec008_hardcoded_secret;
mod ablsec009_weak_crypto;
mod ablsec010_client_identity;
mod ablsec013_information_leak;
mod ablsec014_unsafe_deserialization;
mod ablsec015_untrusted_xml;
mod ablsec016_resource_exhaustion;
mod ablsec017_integrity_bypass;
mod corpus;
mod group;
mod literal;
mod taint;
mod taint_calls;
mod taint_corpus;

use std::path::PathBuf;

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::security::{
    client_identity, dynamic_code, dynamic_query, foreign_sql, hardcoded_secret, header_injection,
    information_leak, integrity_bypass, os_command, path_traversal, resource_exhaustion,
    unsafe_deserialization, untrusted_xml, weak_crypto, xss_output,
};
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

pub type RuleFn =
    fn(&[oxabl_ast::Statement], &oxabl_semantic::Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// The rules, in code order.
pub const RULES: [(&str, RuleFn); 15] = [
    ("ABLSEC001", dynamic_query::run),
    ("ABLSEC002", foreign_sql::run),
    ("ABLSEC003", os_command::run),
    ("ABLSEC004", dynamic_code::run),
    ("ABLSEC005", path_traversal::run),
    ("ABLSEC006", xss_output::run),
    ("ABLSEC007", header_injection::run),
    ("ABLSEC008", hardcoded_secret::run),
    ("ABLSEC009", weak_crypto::run),
    ("ABLSEC010", client_identity::run),
    ("ABLSEC013", information_leak::run),
    ("ABLSEC014", unsafe_deserialization::run),
    ("ABLSEC015", untrusted_xml::run),
    ("ABLSEC016", resource_exhaustion::run),
    ("ABLSEC017", integrity_bypass::run),
];

/// Run one rule over already-expanded `text`, requiring a clean parse.
pub fn run_text(
    rule: impl Fn(
        &[oxabl_ast::Statement],
        &oxabl_semantic::Semantic,
        &AnalysisContext,
    ) -> Vec<Diagnostic>,
    text: &str,
) -> Vec<Diagnostic> {
    let tokens = tokenize(text);
    let program = Parser::new(&tokens, text).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}\n{text}",
        program.errors
    );
    run_program(rule, text, &program.statements)
}

/// Run one rule over `text` whatever the parser makes of it. For the
/// constructs that the parser rejects, where a rule has to read the tokens.
pub fn check_loose(
    rule: impl Fn(
        &[oxabl_ast::Statement],
        &oxabl_semantic::Semantic,
        &AnalysisContext,
    ) -> Vec<Diagnostic>,
    text: &str,
) -> Vec<Diagnostic> {
    let tokens = tokenize(text);
    let program = Parser::new(&tokens, text).parse_program();
    run_program(rule, text, &program.statements)
}

fn run_program(
    rule: impl Fn(
        &[oxabl_ast::Statement],
        &oxabl_semantic::Semantic,
        &AnalysisContext,
    ) -> Vec<Diagnostic>,
    text: &str,
    statements: &[oxabl_ast::Statement],
) -> Vec<Diagnostic> {
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, text, &schema);
    let sem = analyze_file(statements, &ctx);
    rule(statements, &sem, &ctx)
}

/// Run one rule over `source` with no preprocessing.
pub fn check(
    rule: impl Fn(
        &[oxabl_ast::Statement],
        &oxabl_semantic::Semantic,
        &AnalysisContext,
    ) -> Vec<Diagnostic>,
    source: &str,
) -> Vec<Diagnostic> {
    run_text(rule, source)
}

/// Preprocess `main` against in-memory include files under `/src`, then run
/// one rule over the expansion, the way every client does.
pub fn check_expanded(
    rule: impl Fn(
        &[oxabl_ast::Statement],
        &oxabl_semantic::Semantic,
        &AnalysisContext,
    ) -> Vec<Diagnostic>,
    files: &[(&str, &str)],
    main: &str,
) -> Vec<Diagnostic> {
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
