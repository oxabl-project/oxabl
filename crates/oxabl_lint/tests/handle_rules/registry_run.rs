//! The five rules through the registry: one analysis, the group switch and the
//! metadata.

use oxabl_common::{FileId, LintSeverityMap, Severity};
use oxabl_lexer::tokenize;
use oxabl_lint::{Registry, Stability, groups};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

const SRC: &str = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
CREATE QUERY hQuery.
RUN lib.p PERSISTENT SET hLib.
SET-SIZE(m) = 16.
";

fn codes(map: LintSeverityMap) -> Vec<&'static str> {
    let tokens = tokenize(SRC);
    let program = Parser::new(&tokens, SRC).parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, SRC, &schema).with_lint_severities(map);
    let sem = analyze_file(&program.statements, &ctx);
    Registry::builtin()
        .run(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.code.0)
        .filter(|c| c.starts_with("HD"))
        .collect()
}

#[test]
fn nothing_runs_until_the_group_is_enabled() {
    assert!(codes(LintSeverityMap::new()).is_empty());
}

#[test]
fn the_group_enables_the_handle_rules() {
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group(groups::STRICT_HANDLES);
    assert_eq!(codes(map), ["HD001", "HD003", "HD004", "HD005"]);
}

#[test]
fn one_rule_can_be_enabled_alone() {
    let mut map = LintSeverityMap::new();
    map.set("memptr-not-freed", Some(Severity::Error));
    assert_eq!(codes(map), ["HD004"]);
}

#[test]
fn the_rules_are_preview_context_free_warnings() {
    let registry = Registry::builtin();
    let found: Vec<_> = registry
        .rules()
        .map(|r| *r.meta())
        .filter(|m| m.group == groups::STRICT_HANDLES)
        .collect();
    let codes: Vec<_> = found.iter().map(|m| m.code).collect();
    assert_eq!(codes, ["HD001", "HD002", "HD003", "HD004", "HD005"]);
    for m in found {
        assert_eq!(m.default_severity, Severity::Warning, "{}", m.code);
        assert_eq!(m.stability, Stability::Preview, "{}", m.code);
        assert!(m.context_free, "{}", m.code);
        assert!(!m.requires_compilation_unit, "{}", m.code);
    }
}
