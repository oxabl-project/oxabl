//! End-to-end coverage for the syntactic strict rules: `run-value` (CL001),
//! `dynamic-invoke` (CL002), `shared-state` (CL005) and `missing-no-undo`
//! (TX006).
//!
//! Synthetic ABL runs through the real lexer and parser, then through the rule
//! itself, so the tests pin what the parser really produces for each form. The
//! rules are not part of `lint_file` yet; each is called directly.

mod cl001_run_value;
mod cl002_dynamic_invoke;
mod cl005_shared_state;
mod tx006_missing_no_undo;

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{dynamic_invoke, missing_no_undo, run_value, shared_state};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

type RuleFn =
    fn(&[oxabl_ast::Statement], &oxabl_semantic::Semantic, &AnalysisContext) -> Vec<Diagnostic>;

/// The four rules, in code order.
const RULES: [(&str, RuleFn); 4] = [
    ("CL001", run_value::run),
    ("CL002", dynamic_invoke::run),
    ("CL005", shared_state::run),
    ("TX006", missing_no_undo::run),
];

/// Run one rule over `source`, requiring a clean parse.
pub fn check(rule: RuleFn, source: &str) -> Vec<Diagnostic> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}\n{source}",
        program.errors
    );
    run_parsed(rule, source, &program.statements)
}

pub fn run_parsed(
    rule: RuleFn,
    source: &str,
    statements: &[oxabl_ast::Statement],
) -> Vec<Diagnostic> {
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(statements, &ctx);
    rule(statements, &sem, &ctx)
}

/// The text each diagnostic points at.
pub fn flagged<'a>(source: &'a str, diags: &[Diagnostic]) -> Vec<&'a str> {
    diags
        .iter()
        .map(|d| &source[d.span.span.start as usize..d.span.span.end as usize])
        .collect()
}

#[test]
fn every_diagnostic_carries_its_rule_code_and_help() {
    let source = "\
DEFINE NEW SHARED VARIABLE v AS INTEGER.
RUN VALUE(c).
x = DYNAMIC-FUNCTION(c).
";
    for (code, rule) in RULES {
        for d in check(rule, source) {
            assert_eq!(d.code.0, code);
            assert!(d.help.is_some(), "{code} carries a suggestion");
        }
    }
}

#[test]
fn metadata_follows_the_catalog() {
    use oxabl_common::Severity;
    let all = [
        (
            run_value::NAME,
            run_value::GROUP,
            run_value::DEFAULT_SEVERITY,
            run_value::DESCRIPTION,
        ),
        (
            dynamic_invoke::NAME,
            dynamic_invoke::GROUP,
            dynamic_invoke::DEFAULT_SEVERITY,
            dynamic_invoke::DESCRIPTION,
        ),
        (
            shared_state::NAME,
            shared_state::GROUP,
            shared_state::DEFAULT_SEVERITY,
            shared_state::DESCRIPTION,
        ),
        (
            missing_no_undo::NAME,
            missing_no_undo::GROUP,
            missing_no_undo::DEFAULT_SEVERITY,
            missing_no_undo::DESCRIPTION,
        ),
    ];
    let names: Vec<_> = all.iter().map(|m| m.0).collect();
    assert_eq!(
        names,
        [
            "run-value",
            "dynamic-invoke",
            "shared-state",
            "missing-no-undo"
        ]
    );
    let groups: Vec<_> = all.iter().map(|m| m.1).collect();
    assert_eq!(
        groups,
        [
            "strict.calls",
            "strict.calls",
            "strict.calls",
            "strict.transactions"
        ]
    );
    for (name, _, severity, description) in all {
        assert_eq!(severity, Severity::Warning, "{name}");
        assert!(
            !description.is_empty() && !description.contains('\n'),
            "{name}"
        );
    }
}
