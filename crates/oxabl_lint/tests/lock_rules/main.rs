//! End-to-end coverage for the `strict.locks` rules: `default-lock` (LK001),
//! `lock-outlives-transaction` (LK002) and `lock-upgrade-in-loop` (LK003).
//!
//! Synthetic ABL runs through the real lexer, parser and semantic pass, with a
//! small schema loaded, then through the rule itself.

mod doc_examples;
mod lk001_default_lock;
mod lk002_lock_outlives_transaction;
mod lk003_lock_upgrade_in_loop;
mod registry_run;

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

type RuleFn =
    fn(&[oxabl_ast::Statement], &oxabl_semantic::Semantic, &AnalysisContext) -> Vec<Diagnostic>;

const SCHEMA: &str = "\
ADD TABLE \"Customer\"
ADD FIELD \"CustNum\" OF \"Customer\" AS integer
ADD FIELD \"Name\" OF \"Customer\" AS character
ADD FIELD \"Balance\" OF \"Customer\" AS decimal

ADD TABLE \"Order\"
ADD FIELD \"OrderNum\" OF \"Order\" AS integer
ADD FIELD \"CustNum\" OF \"Order\" AS integer

ADD TABLE \"Item\"
ADD FIELD \"ItemNum\" OF \"Item\" AS integer
ADD FIELD \"Price\" OF \"Item\" AS decimal
";

pub fn schema() -> Schema {
    oxabl_schema::test_support::schema_from_df(SCHEMA)
}

/// Run one rule over `source` with the test schema, requiring a clean parse.
pub fn check(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    run(rule, source, Some(&schema()), false)
}

/// [`check`] with no schema loaded.
pub fn check_no_schema(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    run(rule, source, None, false)
}

/// [`check`] for a file the parser must recover from.
pub fn check_recovered(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    run(rule, source, Some(&schema()), true)
}

fn run(rule: RuleFn, source: &str, schema: Option<&Schema>, recovered: bool) -> Vec<Diagnostic> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert_eq!(
        !program.errors.is_empty(),
        recovered,
        "parse errors: {:?}\n{source}",
        program.errors
    );
    let empty = Schema::empty();
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema.unwrap_or(&empty))
        .with_parse_recovered(recovered);
    ctx.schema_loaded = schema.is_some();
    let sem = analyze_file(&program.statements, &ctx);
    rule(&program.statements, &sem, &ctx)
}

/// The 1-based line each diagnostic is anchored on.
pub fn lines(source: &str, diags: &[Diagnostic]) -> Vec<usize> {
    diags
        .iter()
        .map(|d| {
            source[..d.span.span.start as usize]
                .bytes()
                .filter(|&b| b == b'\n')
                .count()
                + 1
        })
        .collect()
}

pub fn help(d: &Diagnostic) -> &str {
    d.help.as_deref().unwrap_or("")
}
