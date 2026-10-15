//! The public rule registry: enumeration, consumer rules, subset selection,
//! groups and severity resolution.

use oxabl_ast::{Span, Statement};
use oxabl_common::{Diagnostic, FileId, FileSpan, LintLayer, LintSeverityMap, Severity};
use oxabl_lexer::tokenize;
use oxabl_lint::{
    BUILTIN_GROUPS, FnRule, LINT0002, Registry, RegistryError, Rule, RuleMeta, Stability, groups,
    lint_file,
};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, Semantic, SourceContext, analyze_file};

const UNUSED: &str = "DEFINE VARIABLE v-unused AS INTEGER NO-UNDO.\n";

/// A consumer rule that flags every file once, at offset 0.
fn flag_file(_: &[Statement], _: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    vec![Diagnostic::error(
        "X001",
        "flagged".to_string(),
        FileSpan::from((ctx.file_id, Span { start: 0, end: 1 })),
    )]
}

const STRICT_RULE: FnRule = FnRule::new(
    RuleMeta::new(
        "X001",
        "flag-file",
        groups::STRICT_CALLS,
        "Flags every file.",
    )
    .with_stability(Stability::Preview),
    flag_file,
);

fn run_with(
    registry: &Registry,
    source: &str,
    severities: LintSeverityMap,
    context: SourceContext,
) -> Vec<(&'static str, Severity)> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema)
        .with_lint_severities(severities)
        .with_source_context(context);
    let sem = analyze_file(&program.statements, &ctx);
    registry
        .run(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0, d.severity))
        .collect()
}

fn run(
    registry: &Registry,
    source: &str,
    severities: LintSeverityMap,
) -> Vec<(&'static str, Severity)> {
    run_with(registry, source, severities, SourceContext::CompilationUnit)
}

fn registry_with_strict_rule() -> Registry {
    let mut registry = Registry::builtin();
    registry.register(STRICT_RULE).unwrap();
    registry
}

#[test]
fn the_builtin_registry_enumerates_every_rule_with_metadata() {
    let registry = Registry::builtin();
    let rules: Vec<(&str, &str, &str, Severity)> = registry
        .rules()
        .map(|r| {
            let m = r.meta();
            (m.code, m.name, m.group, m.default_severity)
        })
        .collect();
    let error = Severity::Error;
    let warning = Severity::Warning;
    assert_eq!(
        rules,
        [
            ("LINT0001", "undefined-symbol", "default", error),
            ("LINT0002", "unused-variable", "default", warning),
            ("LINT0003", "unknown-table-or-field", "default", warning),
            ("LINT0004", "type-mismatch-assignment", "default", warning),
            (
                "LINT0005",
                "block-var-used-outside",
                "default",
                Severity::Info
            ),
            ("LINT0006", "assigned-but-never-read", "default", warning),
            ("CL001", "run-value", "strict.calls", warning),
            ("CL002", "dynamic-invoke", "strict.calls", warning),
            ("CL005", "shared-state", "strict.calls", warning),
            ("TX006", "missing-no-undo", "strict.transactions", warning),
            ("HD001", "handle-not-deleted", "strict.handles", warning),
            ("HD002", "delete-not-in-finally", "strict.handles", warning),
            (
                "HD003",
                "persistent-procedure-leak",
                "strict.handles",
                warning
            ),
            ("HD004", "memptr-not-freed", "strict.handles", warning),
            ("HD005", "missing-widget-pool", "strict.handles", warning),
            ("LK001", "default-lock", "strict.locks", warning),
            (
                "LK002",
                "lock-outlives-transaction",
                "strict.locks",
                warning
            ),
            ("LK003", "lock-upgrade-in-loop", "strict.locks", warning),
            (
                "TX001",
                "implicit-transaction",
                "strict.transactions",
                warning
            ),
            (
                "TX002",
                "procedure-wide-transaction",
                "strict.transactions",
                warning
            ),
            (
                "TX003",
                "transaction-spans-ui",
                "strict.transactions",
                warning
            ),
            (
                "TX004",
                "transaction-spans-call",
                "strict.transactions",
                warning
            ),
            ("TX005", "scope-widened", "strict.transactions", warning),
            ("DB001", "find-in-loop", "strict.access", warning),
        ]
    );
    for rule in registry.rules() {
        let m = rule.meta();
        let strict = m.group.starts_with("strict.");
        assert!(!m.description.is_empty(), "{}", m.code);
        assert_eq!(m.context_free, strict, "{}", m.code);
        assert_eq!(
            m.use_site_in_includer,
            m.group == "strict.handles"
                || m.group == "strict.locks"
                || m.group == "strict.access"
                || (m.group == "strict.transactions" && m.code != "TX006"),
            "{}",
            m.code
        );
        let stability = if strict {
            Stability::Preview
        } else {
            Stability::Stable
        };
        assert_eq!(m.stability, stability, "{}", m.code);
    }
}

#[test]
fn the_strict_rules_run_only_when_their_group_or_rule_is_enabled() {
    let source = "DEFINE VARIABLE c AS CHARACTER NO-UNDO INITIAL \"x.p\".\n\
                  RUN VALUE(c).\nDEFINE VARIABLE v AS INTEGER.\nMESSAGE v.\n";
    let registry = Registry::builtin();
    let strict = |map| -> Vec<&str> {
        run(&registry, source, map)
            .into_iter()
            .map(|(c, _)| c)
            .filter(|c| !c.starts_with("LINT"))
            .collect()
    };
    assert!(strict(LintSeverityMap::new()).is_empty());

    let mut calls = LintSeverityMap::new();
    calls.base_mut().enable_group("strict.calls");
    assert_eq!(strict(calls), ["CL001"]);

    let mut all = LintSeverityMap::new();
    all.base_mut().enable_group("strict");
    assert_eq!(strict(all), ["CL001", "TX006"]);

    let mut by_name = LintSeverityMap::new();
    by_name.set("missing-no-undo", Some(Severity::Error));
    assert_eq!(strict(by_name), ["TX006"]);
}

#[test]
fn the_builtin_groups_are_default_plus_six_opt_in_strict_groups() {
    let names: Vec<_> = BUILTIN_GROUPS.iter().map(|g| g.name).collect();
    assert_eq!(
        names,
        [
            "default",
            "strict.transactions",
            "strict.locks",
            "strict.access",
            "strict.handles",
            "strict.nulls",
            "strict.calls",
        ]
    );
    for group in BUILTIN_GROUPS {
        let is_default = group.name == groups::DEFAULT;
        assert_eq!(group.enabled_by_default, is_default, "{}", group.name);
        assert_eq!(group.toggleable, !is_default, "{}", group.name);
    }
    let registry = Registry::empty();
    assert_eq!(registry.groups_selected_by("strict").len(), 6);
    assert_eq!(registry.groups_selected_by("STRICT.calls").len(), 1);
    assert!(registry.groups_selected_by("stri").is_empty());
}

#[test]
fn a_rule_is_found_by_code_or_name_in_any_case() {
    let registry = Registry::builtin();
    for selector in ["LINT0002", "lint0002", "unused-variable", "Unused-Variable"] {
        assert_eq!(
            registry.rule(selector).map(|r| r.meta().code),
            Some(LINT0002),
            "{selector}"
        );
    }
    assert!(registry.rule("no-such-rule").is_none());
}

#[test]
fn registration_rejects_duplicates_and_unknown_groups() {
    let mut registry = registry_with_strict_rule();
    assert_eq!(
        registry.register(STRICT_RULE),
        Err(RegistryError::DuplicateRule("X001".to_string()))
    );
    let renamed = FnRule::new(
        RuleMeta::new("X002", "Unused-Variable", groups::DEFAULT, "Clash."),
        flag_file,
    );
    assert_eq!(
        registry.register(renamed),
        Err(RegistryError::DuplicateRule("Unused-Variable".to_string()))
    );
    let orphan = FnRule::new(
        RuleMeta::new("X003", "orphan", "acme", "No group."),
        flag_file,
    );
    assert!(matches!(
        registry.register(orphan),
        Err(RegistryError::UnknownGroup { .. })
    ));
    registry
        .register_group(oxabl_lint::RuleGroup::opt_in("acme", "Local rules."))
        .unwrap();
    registry.register(orphan).unwrap();
}

#[test]
fn lint_file_is_the_builtin_registry() {
    let tokens = tokenize(UNUSED);
    let program = Parser::new(&tokens, UNUSED).parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, UNUSED, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    let via_lint_file: Vec<_> = lint_file(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0, d.severity))
        .collect();
    assert_eq!(via_lint_file, [(LINT0002, Severity::Warning)]);
    assert_eq!(
        via_lint_file,
        run(&Registry::builtin(), UNUSED, LintSeverityMap::new())
    );
}

#[test]
fn a_strict_rule_is_off_until_its_group_or_the_rule_is_enabled() {
    let registry = registry_with_strict_rule();
    assert!(!run(&registry, UNUSED, LintSeverityMap::new()).contains(&("X001", Severity::Warning)));

    for selector in ["strict.calls", "strict"] {
        let mut map = LintSeverityMap::new();
        map.base_mut().enable_group(selector);
        assert!(
            run(&registry, UNUSED, map).contains(&("X001", Severity::Warning)),
            "default severity, not the emitted error"
        );
    }

    let mut map = LintSeverityMap::new();
    map.set("flag-file", Some(Severity::Error));
    assert!(run(&registry, UNUSED, map).contains(&("X001", Severity::Error)));
}

#[test]
fn the_default_group_cannot_be_switched_off_as_a_group() {
    let registry = Registry::builtin();
    let mut map = LintSeverityMap::new();
    map.base_mut().disable_group("default");
    assert_eq!(run(&registry, UNUSED, map), [(LINT0002, Severity::Warning)]);

    let mut map = LintSeverityMap::new();
    map.set("unused-variable", None);
    assert!(run(&registry, UNUSED, map).is_empty(), "but each rule can");
}

#[test]
fn run_where_selects_a_subset() {
    let registry = registry_with_strict_rule();
    let mut map = LintSeverityMap::new();
    map.base_mut().enable_group("strict");
    let tokens = tokenize(UNUSED);
    let program = Parser::new(&tokens, UNUSED).parse_program();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, UNUSED, &schema).with_lint_severities(map);
    let sem = analyze_file(&program.statements, &ctx);
    let codes: Vec<_> = registry
        .run_where(&program.statements, &sem, &ctx, |m| {
            m.group.starts_with("strict.")
        })
        .into_iter()
        .map(|d| d.code.0)
        .collect();
    assert_eq!(codes, ["X001"]);
}

#[test]
fn whole_unit_rules_are_skipped_on_an_include_fragment() {
    let registry = Registry::builtin();
    assert!(
        run_with(
            &registry,
            UNUSED,
            LintSeverityMap::new(),
            SourceContext::IncludeFragment
        )
        .is_empty()
    );
}

#[test]
fn a_scoped_layer_applies_after_for_path() {
    let registry = registry_with_strict_rule();
    let mut map = LintSeverityMap::new();
    let mut layer = LintLayer::new();
    layer.enable_group("strict.calls");
    layer.set_rule("unused-variable", Some(Severity::Error));
    map.add_scope(std::path::Path::new("/ws"), &["new/**".to_string()], layer);

    let outside = run(&registry, UNUSED, map.for_path("/ws/old/a.p".as_ref()));
    assert_eq!(outside, [(LINT0002, Severity::Warning)]);
    let inside = run(&registry, UNUSED, map.for_path("/ws/new/a.p".as_ref()));
    assert_eq!(
        inside,
        [(LINT0002, Severity::Error), ("X001", Severity::Warning)]
    );
}

#[test]
fn a_consumer_rule_can_be_a_stateful_trait_object() {
    struct Counter(&'static str, RuleMeta);
    impl Rule for Counter {
        fn meta(&self) -> &RuleMeta {
            &self.1
        }
        fn run(&self, _: &[Statement], _: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
            vec![Diagnostic::warning(
                self.0,
                "custom".to_string(),
                FileSpan::from((ctx.file_id, Span { start: 0, end: 1 })),
            )]
        }
    }
    let mut registry = Registry::empty();
    registry
        .register(Counter(
            "ACME01",
            RuleMeta::new("ACME01", "acme-rule", groups::DEFAULT, "Custom."),
        ))
        .unwrap();
    assert_eq!(
        run(&registry, UNUSED, LintSeverityMap::new()),
        [("ACME01", Severity::Warning)]
    );
}
