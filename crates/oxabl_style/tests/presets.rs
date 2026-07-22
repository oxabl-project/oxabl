use oxabl_style::{
    BufferNaming, ClassNaming, FileNameCasing, IndentStyle, KeywordAbbreviation, KeywordCase,
    MethodCase, ParameterPrefix, Scope, StaticMemberRef, StyleGuide, SubstitutePolicy,
    TempTablePrefix, VariableCase, VariableDeclAlignment,
};

// =============================================================================
// Preset correctness tests
// =============================================================================

#[test]
fn default_base_has_standard_indent() {
    let base = StyleGuide::default_base();
    assert_eq!(base.indent_size, 4);
    assert_eq!(base.indent_style, IndentStyle::Spaces);
    assert_eq!(base.max_line_length, 120);
}

#[test]
fn default_base_keywords_uppercase_unabbreviated() {
    let base = StyleGuide::default_base();
    assert_eq!(base.keyword_case, KeywordCase::Uppercase);
    assert_eq!(
        base.keyword_abbreviation,
        KeywordAbbreviation::AbbreviateNothing
    );
}

#[test]
fn default_base_end_with_type() {
    assert!(StyleGuide::default_base().end_with_type);
}

#[test]
fn default_base_require_no_undo() {
    assert!(StyleGuide::default_base().require_no_undo);
}

// ---- oestandards preset ----

#[test]
fn oestandards_block_labels() {
    let oes = StyleGuide::oestandards();
    assert!(oes.require_block_labels);
    assert!(oes.disallow_unnecessary_blocks);
    assert!(oes.dot_colon_same_line);
}

#[test]
fn oestandards_assign_and_substitute() {
    let oes = StyleGuide::oestandards();
    assert!(oes.require_assign_statement);
    assert_eq!(oes.prefer_substitute, SubstitutePolicy::Always);
}

#[test]
fn oestandards_naming() {
    let oes = StyleGuide::oestandards();
    assert_eq!(oes.variable_case, VariableCase::CamelCase);
    assert_eq!(oes.global_prefix, Some('g'));
    assert_eq!(oes.parameter_prefix, ParameterPrefix::IpOpIop);
    assert_eq!(oes.buffer_naming, BufferNaming::BPrefix);
    assert_eq!(oes.temp_table_prefix, TempTablePrefix::TtBiPdsDs);
}

#[test]
fn oestandards_structure_orders() {
    let oes = StyleGuide::oestandards();
    assert!(!oes.class_structure_order.is_empty());
    assert!(
        oes.class_structure_order
            .contains(&"Constructors".to_string())
    );
    assert!(!oes.procedure_structure_order.is_empty());
    assert!(
        oes.procedure_structure_order
            .contains(&"Main Block".to_string())
    );
}

#[test]
fn oestandards_quality() {
    let oes = StyleGuide::oestandards();
    assert!(oes.require_symbolic_operators);
    assert!(oes.require_if_parentheses);
    assert!(oes.run_in_this_procedure);
    assert!(oes.disallow_commented_code);
}

// ---- consultingwerk preset ----

#[test]
fn consultingwerk_naming() {
    let cw = StyleGuide::consultingwerk();
    assert_eq!(cw.variable_case, VariableCase::LowerCase);
    assert_eq!(cw.method_case, MethodCase::PascalCase);
    assert_eq!(cw.parameter_prefix, ParameterPrefix::PLowercase);
    assert_eq!(cw.buffer_naming, BufferNaming::RoleTable);
    assert_eq!(cw.temp_table_prefix, TempTablePrefix::TtE);
    assert_eq!(cw.class_naming, ClassNaming::SuffixBaseClass);
}

#[test]
fn consultingwerk_oo() {
    let cw = StyleGuide::consultingwerk();
    assert_eq!(cw.interface_prefix, Some('I'));
    assert!(cw.using_sort);
    assert!(cw.require_this_object);
    assert_eq!(cw.static_member_ref, StaticMemberRef::ClassName);
}

#[test]
fn consultingwerk_file_and_var() {
    let cw = StyleGuide::consultingwerk();
    assert_eq!(cw.file_name_casing, FileNameCasing::LowerCamelCase);
    assert_eq!(cw.variable_decl_alignment, VariableDeclAlignment::Tabular);
}

#[test]
fn consultingwerk_constructs() {
    let cw = StyleGuide::consultingwerk();
    assert!(cw.require_widget_pool);
    assert!(cw.temp_table_in_include);
    assert!(cw.named_events_on_prefix);
}

// ---- Presets don't stomp shared base ----

#[test]
fn both_presets_inherit_base_keyword_case() {
    assert_eq!(
        StyleGuide::oestandards().keyword_case,
        StyleGuide::default_base().keyword_case
    );
    assert_eq!(
        StyleGuide::consultingwerk().keyword_case,
        StyleGuide::default_base().keyword_case
    );
}

#[test]
fn both_presets_inherit_base_indent() {
    assert_eq!(
        StyleGuide::oestandards().indent_size,
        StyleGuide::default_base().indent_size
    );
    assert_eq!(
        StyleGuide::consultingwerk().indent_size,
        StyleGuide::default_base().indent_size
    );
}

#[test]
fn both_presets_inherit_base_no_undo() {
    assert_eq!(
        StyleGuide::oestandards().require_no_undo,
        StyleGuide::default_base().require_no_undo
    );
    assert_eq!(
        StyleGuide::consultingwerk().require_no_undo,
        StyleGuide::default_base().require_no_undo
    );
}

#[test]
fn both_presets_inherit_base_end_with_type() {
    assert_eq!(
        StyleGuide::oestandards().end_with_type,
        StyleGuide::default_base().end_with_type
    );
    assert_eq!(
        StyleGuide::consultingwerk().end_with_type,
        StyleGuide::default_base().end_with_type
    );
}

// =============================================================================
// Scope classification tests
// =============================================================================

#[test]
fn scope_all_fields_covered() {
    // Serialize a synthetic instance with every Option field set to Some(_)
    // so that serialization-to-TOML doesn't drop None keys (which would
    // cause the coverage assertion to silently skip those fields).
    let all_some = StyleGuide {
        global_prefix: Some('x'),
        interface_prefix: Some('y'),
        class_structure_order: vec!["test".into()],
        procedure_structure_order: vec!["test".into()],
        ..StyleGuide::default_base()
    };
    let toml_str = all_some.to_toml().expect("serialize to TOML");

    // Parse back into a toml::Table so we can iterate keys.
    let table: toml::Table = toml::from_str(&toml_str).expect("parse TOML table");
    let mut missing = Vec::new();
    for key in table.keys() {
        if StyleGuide::scope(key).is_none() {
            missing.push(key.clone());
        }
    }
    assert!(
        missing.is_empty(),
        "Fields not classified by StyleGuide::scope(): {missing:?}"
    );
}

#[test]
fn scope_returns_none_for_unknown() {
    assert_eq!(StyleGuide::scope("nonexistent_field"), None);
}

#[test]
fn scope_formatting_rules_exist() {
    let formatting = [
        "keyword_case",
        "keyword_abbreviation",
        "indent_size",
        "indent_style",
        "do_placement",
        "dot_colon_same_line",
        "period_placement",
        "where_placement",
        "and_or_placement",
        "first_param_same_line",
        "multi_param_threshold",
        "max_line_length",
        "wrap_long_lines",
        "blank_lines_between_sections",
        "end_with_type",
        "using_sort",
        "comment_style",
        "class_structure_order",
        "procedure_structure_order",
    ]
    .iter()
    .filter(|&&name| StyleGuide::scope(name) == Some(Scope::Formatting))
    .count();
    assert!(
        formatting >= 19,
        "Expected at least 19 Formatting rules, got {formatting}"
    );
}

#[test]
fn scope_diagnostic_rules_exist() {
    let diagnostic = [
        "require_no_undo",
        "variable_case",
        "require_assign_statement",
        "disallow_commented_code",
    ]
    .iter()
    .filter(|&&name| StyleGuide::scope(name) == Some(Scope::Diagnostic))
    .count();
    assert_eq!(diagnostic, 4, "Expected 4 Diagnostic rules");
}

// =============================================================================
// TOML round-trip tests
// =============================================================================

#[test]
fn default_base_round_trips() {
    let base = StyleGuide::default_base();
    let toml_str = base.to_toml().expect("serialize");
    let parsed = StyleGuide::from_toml(&toml_str).expect("deserialize");
    // Compare as TOML strings (struct-level equality fails on Vec order).
    assert_eq!(
        base.to_toml().unwrap(),
        parsed.to_toml().unwrap(),
        "default_base() → to_toml → from_toml must reproduce identical TOML"
    );
}

#[test]
fn oestandards_round_trips() {
    let oes = StyleGuide::oestandards();
    let toml_str = oes.to_toml().expect("serialize");
    let parsed = StyleGuide::from_toml(&toml_str).expect("deserialize");
    assert_eq!(
        oes.to_toml().unwrap(),
        parsed.to_toml().unwrap(),
        "oestandards() → to_toml → from_toml must reproduce identical TOML"
    );
}

#[test]
fn consultingwerk_round_trips() {
    let cw = StyleGuide::consultingwerk();
    let toml_str = cw.to_toml().expect("serialize");
    let parsed = StyleGuide::from_toml(&toml_str).expect("deserialize");
    assert_eq!(
        cw.to_toml().unwrap(),
        parsed.to_toml().unwrap(),
        "consultingwerk() → to_toml → from_toml must reproduce identical TOML"
    );
}

#[test]
fn partial_toml_falls_back_to_base() {
    let partial = "keyword_case = \"Lowercase\"\n";
    let guide = StyleGuide::from_toml(partial).expect("parse partial TOML");
    // Overridden field
    assert_eq!(guide.keyword_case, KeywordCase::Lowercase);
    // Fallback field
    assert_eq!(guide.indent_size, StyleGuide::default_base().indent_size);
    assert_eq!(
        guide.require_no_undo,
        StyleGuide::default_base().require_no_undo
    );
}
