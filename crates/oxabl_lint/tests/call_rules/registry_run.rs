//! The two rules through the registry: off until the group is enabled.

use std::path::PathBuf;

use oxabl_common::{FileId, LintSeverityMap};
use oxabl_index::BatchIndex;
use oxabl_lexer::tokenize;
use oxabl_lint::{Registry, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

const SOURCE: &str = "\
PROCEDURE p:
    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.
END PROCEDURE.
RUN p.
RUN missing-one.
";

fn codes(severities: LintSeverityMap) -> Vec<&'static str> {
    let fs = InMemoryFileSystem::new();
    let paths = [PathBuf::from("/src")];
    let known: [PathBuf; 0] = [];
    let index = BatchIndex::new(&fs, &paths).seeded_with(&known);
    let tokens = tokenize(SOURCE);
    let program = Parser::new(&tokens, SOURCE).parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, SOURCE, &schema)
        .with_tokens(&tokens)
        .with_index(&index)
        .with_lint_severities(severities);
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.code.0)
        .filter(|c| c.starts_with("CL"))
        .collect()
}

#[test]
fn the_default_run_reports_neither_rule() {
    assert!(codes(LintSeverityMap::new()).is_empty());
}

#[test]
fn enabling_the_group_runs_both() {
    let mut severities = LintSeverityMap::new();
    severities.base_mut().enable_group("strict.calls");
    assert_eq!(codes(severities), ["CL003", "CL004"]);
}

#[test]
fn each_rule_can_be_enabled_alone() {
    let mut severities = LintSeverityMap::new();
    severities
        .base_mut()
        .set_rule("CL004", Some(oxabl_common::Severity::Error));
    assert_eq!(codes(severities), ["CL004"]);
}

#[test]
fn both_rules_are_registered_in_the_calls_group() {
    let registry = Registry::builtin();
    for code in ["CL003", "CL004"] {
        let rule = registry.rules().find(|r| r.meta().code == code).unwrap();
        assert_eq!(rule.meta().group, "strict.calls");
    }
}
