//! The three rules through the registry: one analysis, the group switch and
//! the metadata.

use oxabl_common::{FileId, LintSeverityMap, Severity};
use oxabl_lexer::tokenize;
use oxabl_lint::{Registry, Stability, groups};
use oxabl_parser::Parser;
use oxabl_semantic::{AnalysisContext, analyze_file};

use super::schema;

const SRC: &str = "\
FIND FIRST Customer.
DO TRANSACTION:
    FIND FIRST Item EXCLUSIVE-LOCK.
    Item.Price = 1.
END.
FOR EACH Order NO-LOCK:
    FIND CURRENT Order EXCLUSIVE-LOCK.
END.
MESSAGE \"done\".
";

fn codes(map: LintSeverityMap) -> Vec<&'static str> {
    let tokens = tokenize(SRC);
    let program = Parser::new(&tokens, SRC).parse_program();
    let schema = schema();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, SRC, &schema).with_lint_severities(map);
    let sem = analyze_file(&program.statements, &ctx);
    Registry::builtin()
        .run(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.code.0)
        .filter(|c| c.starts_with("LK"))
        .collect()
}

#[test]
fn nothing_runs_until_the_group_is_enabled() {
    assert!(codes(LintSeverityMap::new()).is_empty());
}

#[test]
fn the_group_enables_the_lock_rules() {
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group(groups::STRICT_LOCKS);
    assert_eq!(codes(map), ["LK001", "LK002", "LK003"]);
}

#[test]
fn one_rule_can_be_enabled_alone() {
    let mut map = LintSeverityMap::new();
    map.set("lock-upgrade-in-loop", Some(Severity::Error));
    assert_eq!(codes(map), ["LK003"]);
}

#[test]
fn the_rules_are_preview_warnings_routed_like_the_handle_rules() {
    let registry = Registry::builtin();
    let found: Vec<_> = registry
        .rules()
        .map(|r| *r.meta())
        .filter(|m| m.group == groups::STRICT_LOCKS)
        .collect();
    let codes: Vec<_> = found.iter().map(|m| m.code).collect();
    assert_eq!(codes, ["LK001", "LK002", "LK003"]);
    for m in found {
        assert_eq!(m.default_severity, Severity::Warning, "{}", m.code);
        assert_eq!(m.stability, Stability::Preview, "{}", m.code);
        assert!(m.context_free, "{}", m.code);
        assert!(m.use_site_in_includer, "{}", m.code);
        assert!(!m.requires_compilation_unit, "{}", m.code);
    }
}
