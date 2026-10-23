//! The `security` group: registry metadata, opt-in behaviour, and severity.

use oxabl_common::{LintSeverityMap, Severity};
use oxabl_lint::security::{dynamic_code, dynamic_query, foreign_sql, os_command, path_traversal};
use oxabl_lint::{
    ABLSEC001, ABLSEC002, ABLSEC003, ABLSEC004, ABLSEC005, BUILTIN_GROUPS, Registry, Stability,
    groups,
};

use super::{RULES, check};

#[test]
fn the_group_exists_and_is_opt_in() {
    let group = BUILTIN_GROUPS
        .iter()
        .find(|g| g.name == groups::SECURITY)
        .expect("security group is registered");
    assert_eq!(group.name, "security");
    assert!(!group.enabled_by_default);
    assert!(group.toggleable);
}

#[test]
fn metadata_follows_the_catalog() {
    let registry = Registry::builtin();
    let expected = [
        (ABLSEC001, "dynamic-query", Severity::Error, "8.0"),
        (ABLSEC002, "foreign-sql", Severity::Error, "9.5"),
        (ABLSEC003, "os-command", Severity::Error, "9.5"),
        (ABLSEC004, "dynamic-code", Severity::Error, "9.5"),
        (ABLSEC005, "path-traversal", Severity::Warning, "8.0"),
    ];
    for (code, name, severity, security_severity) in expected {
        let rule = registry
            .rule(code)
            .unwrap_or_else(|| panic!("{code} registered"));
        let meta = rule.meta();
        assert_eq!(meta.name, name);
        assert_eq!(meta.group, "security");
        assert_eq!(meta.default_severity, severity, "{code}");
        assert_eq!(meta.stability, Stability::Preview, "{code}");
        assert_eq!(meta.security_severity, Some(security_severity), "{code}");
        assert!(meta.help.is_some_and(|h| h.contains("literal")), "{code}");
        assert!(meta.context_free, "{code}");
        assert!(!meta.description.is_empty() && !meta.description.contains('\n'));
        assert!(registry.rule(name).is_some());
    }
    assert_eq!(dynamic_query::NAME, "dynamic-query");
    assert_eq!(foreign_sql::NAME, "foreign-sql");
    assert_eq!(os_command::NAME, "os-command");
    assert_eq!(dynamic_code::NAME, "dynamic-code");
    assert_eq!(path_traversal::NAME, "path-traversal");
}

#[test]
fn the_rules_are_off_by_default() {
    let registry = Registry::builtin();
    let map = LintSeverityMap::default();
    for (code, _) in RULES {
        let rule = registry.rule(code).unwrap();
        assert_eq!(registry.severity_for(rule, &map), None, "{code}");
    }
}

#[test]
fn enabling_the_group_turns_them_on_at_their_default_severity() {
    let registry = Registry::builtin();
    let mut map = LintSeverityMap::default();
    map.base_mut().enable_group("security");
    for (code, _) in RULES {
        let rule = registry.rule(code).unwrap();
        assert_eq!(
            registry.severity_for(rule, &map),
            Some(rule.meta().default_severity),
            "{code}"
        );
    }
}

#[test]
fn enabling_strict_does_not_enable_security() {
    let registry = Registry::builtin();
    let mut map = LintSeverityMap::default();
    map.base_mut().enable_group("strict");
    for (code, _) in RULES {
        let rule = registry.rule(code).unwrap();
        assert_eq!(registry.severity_for(rule, &map), None, "{code}");
    }
}

#[test]
fn every_diagnostic_carries_its_rule_code_and_help() {
    let source = "\
hQ:QUERY-PREPARE(c).
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE (c).
OS-COMMAND SILENT VALUE(c).
RUN VALUE(c).
OUTPUT TO VALUE(c).
";
    let mut seen = 0;
    for (code, rule) in RULES {
        for d in check(rule, source) {
            seen += 1;
            assert_eq!(d.code.0, code);
            assert!(d.help.is_some(), "{code} carries a suggestion");
        }
    }
    assert_eq!(seen, 5, "each snippet line belongs to exactly one rule");
}

#[test]
fn the_call_rules_and_the_security_rule_both_report_a_dynamic_run() {
    use oxabl_common::FileId;
    use oxabl_lexer::tokenize;
    use oxabl_parser::Parser;
    use oxabl_schema::Schema;
    use oxabl_semantic::{AnalysisContext, analyze_file};

    let source = "RUN VALUE(c).\n";
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    let mut map = LintSeverityMap::default();
    map.base_mut().enable_group("strict.calls");
    map.base_mut().enable_group("security");
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_lint_severities(map);
    let sem = analyze_file(&program.statements, &ctx);
    let codes: Vec<_> = Registry::builtin()
        .run(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.code.0)
        .filter(|c| *c == "CL001" || *c == "ABLSEC004")
        .collect();
    assert_eq!(codes, ["CL001", "ABLSEC004"]);
}
