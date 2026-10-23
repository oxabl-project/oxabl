//! The `security` group: registry metadata, opt-in behaviour, and severity.

use oxabl_common::{LintSeverityMap, Severity};
use oxabl_lint::security::{
    dynamic_code, dynamic_query, foreign_sql, hardcoded_secret, header_injection, information_leak,
    integrity_bypass, os_command, path_traversal, resource_exhaustion, unsafe_deserialization,
    untrusted_xml, weak_crypto, xss_output,
};
use oxabl_lint::{
    ABLSEC001, ABLSEC002, ABLSEC003, ABLSEC004, ABLSEC005, ABLSEC006, ABLSEC007, ABLSEC008,
    ABLSEC009, ABLSEC013, ABLSEC014, ABLSEC015, ABLSEC016, ABLSEC017, BUILTIN_GROUPS, Registry,
    Stability, groups,
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
        (ABLSEC006, "xss-output", Severity::Warning, "8.0"),
        (ABLSEC007, "header-injection", Severity::Warning, "5.5"),
        (ABLSEC008, "hardcoded-secret", Severity::Error, "8.0"),
        (ABLSEC009, "weak-crypto", Severity::Warning, "8.0"),
        (ABLSEC013, "information-leak", Severity::Warning, "5.5"),
        (
            ABLSEC014,
            "unsafe-deserialization",
            Severity::Warning,
            "8.0",
        ),
        (ABLSEC015, "untrusted-xml", Severity::Info, "3.0"),
        (ABLSEC016, "resource-exhaustion", Severity::Info, "5.5"),
        (ABLSEC017, "integrity-bypass", Severity::Warning, "5.5"),
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
        assert!(meta.help.is_some_and(|h| h.len() > 40), "{code}");
        assert!(meta.context_free, "{code}");
        assert!(!meta.description.is_empty() && !meta.description.contains('\n'));
        assert!(registry.rule(name).is_some());
    }
    assert_eq!(dynamic_query::NAME, "dynamic-query");
    assert_eq!(foreign_sql::NAME, "foreign-sql");
    assert_eq!(os_command::NAME, "os-command");
    assert_eq!(dynamic_code::NAME, "dynamic-code");
    assert_eq!(path_traversal::NAME, "path-traversal");
    assert_eq!(xss_output::NAME, "xss-output");
    assert_eq!(header_injection::NAME, "header-injection");
    assert_eq!(hardcoded_secret::NAME, "hardcoded-secret");
    assert_eq!(weak_crypto::NAME, "weak-crypto");
    assert_eq!(information_leak::NAME, "information-leak");
    assert_eq!(unsafe_deserialization::NAME, "unsafe-deserialization");
    assert_eq!(untrusted_xml::NAME, "untrusted-xml");
    assert_eq!(resource_exhaustion::NAME, "resource-exhaustion");
    assert_eq!(integrity_bypass::NAME, "integrity-bypass");
}

#[test]
fn the_first_five_helps_say_what_a_literal_is() {
    let registry = Registry::builtin();
    for code in [ABLSEC001, ABLSEC002, ABLSEC003, ABLSEC004, ABLSEC005] {
        let help = registry.rule(code).unwrap().meta().help.unwrap();
        assert!(help.contains("literal"), "{code}");
    }
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
DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
hQ:QUERY-PREPARE(c).
RUN STORED-PROC send-sql-statement hP = PROC-HANDLE (c).
OS-COMMAND SILENT VALUE(c).
RUN VALUE(c).
OUTPUT TO VALUE(c).
{&OUT} c.
oResp:SetHeader(\"X\", c).
cPassword = \"hunter2\".
cHash = MD5-DIGEST(cPassword).
pcError = ERROR-STATUS:GET-MESSAGE(1).
oSer:Deserialize(c).
hDoc:LOAD(\"file\", c, FALSE).
SET-SIZE(m) = n.
DISABLE TRIGGERS FOR LOAD OF Customer.
";
    let mut seen = 0;
    for (code, rule) in RULES {
        for d in check(rule, source) {
            seen += 1;
            assert_eq!(d.code.0, code);
            assert!(d.help.is_some(), "{code} carries a suggestion");
        }
    }
    assert_eq!(seen, 14, "each snippet line belongs to exactly one rule");
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
