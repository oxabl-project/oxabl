//! End-to-end coverage for the `strict.handles` rules: `handle-not-deleted`
//! (HD001), `delete-not-in-finally` (HD002), `persistent-procedure-leak`
//! (HD003), `memptr-not-freed` (HD004) and `missing-widget-pool` (HD005).
//!
//! Synthetic ABL runs through the real lexer, parser and semantic pass, then
//! through the rule itself.

mod hd001_handle_not_deleted;
mod hd002_delete_not_in_finally;
mod hd003_persistent_procedure_leak;
mod hd004_memptr_not_freed;
mod hd005_missing_widget_pool;
mod registry_run;

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, SourceContext, analyze_file};

type RuleFn =
    fn(&[oxabl_ast::Statement], &oxabl_semantic::Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// Run one rule over `source`, requiring a clean parse.
pub fn check(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    check_in(rule, source, SourceContext::CompilationUnit)
}

/// [`check`] for a file opened as an include fragment.
pub fn check_fragment(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    check_in(rule, source, SourceContext::IncludeFragment)
}

/// [`check`] for a root whose first `include_len` bytes an include wrote.
pub fn check_with_include(rule: RuleFn, source: &str, include_len: u32) -> Vec<Diagnostic> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let regions = [oxabl_ast::Span {
        start: 0,
        end: include_len,
    }];
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_include_regions(&regions);
    let sem = analyze_file(&program.statements, &ctx);
    rule(&program.statements, &sem, &ctx)
}

fn check_in(rule: RuleFn, source: &str, context: SourceContext) -> Vec<Diagnostic> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}\n{source}",
        program.errors
    );
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_source_context(context);
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
