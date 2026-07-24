//! JSON Schema coverage for the `oxabl.toml` config graph (`WorkspaceConfig`
//! and the `[workspace.lint]` surface), backing the `oxabl schema` subcommand
//! and the VS Code extension's `oxabl.toml` autocomplete/validation.

use std::collections::BTreeSet;

use oxabl_workspace::{LintConfig, WorkspaceConfig};

fn object_keys(value: &serde_json::Value) -> BTreeSet<String> {
    value
        .as_object()
        .expect("expected a JSON object")
        .keys()
        .cloned()
        .collect()
}

#[test]
fn lint_schema_has_all_four_rules_with_severity_enum() {
    let schema = serde_json::to_value(schemars::schema_for!(LintConfig)).unwrap();

    let props = object_keys(&schema["properties"]);
    let expected: BTreeSet<String> = [
        "undefined-symbol",
        "unused-variable",
        "unknown-table-or-field",
        "type-mismatch-assignment",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    assert_eq!(
        props, expected,
        "lint schema must expose all four kebab keys"
    );

    let severities: Vec<&str> = schema["definitions"]["LintSeverity"]["enum"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap())
        .collect();
    assert_eq!(severities, ["off", "hint", "info", "warn", "error"]);
}

#[test]
fn lint_schema_denies_unknown_fields() {
    let schema = serde_json::to_value(schemars::schema_for!(LintConfig)).unwrap();
    assert_eq!(
        schema["additionalProperties"],
        serde_json::Value::Bool(false),
        "LintConfig schema must set additionalProperties: false"
    );
}

#[test]
fn lint_schema_covers_every_serialized_field() {
    // D1 auto-coverage guard for the lint surface (mirrors the style test):
    // schema properties must equal the struct's serialized field set.
    let schema = serde_json::to_value(schemars::schema_for!(LintConfig)).unwrap();
    let schema_props = object_keys(&schema["properties"]);
    let serde_fields = object_keys(&serde_json::to_value(LintConfig::default()).unwrap());
    assert_eq!(schema_props, serde_fields);
}

#[test]
fn workspace_schema_embeds_lint_and_style_definitions() {
    // The extension consumes the WorkspaceConfig schema; confirm the whole graph
    // (lint + style) is reachable from the top-level document.
    let schema = serde_json::to_value(schemars::schema_for!(WorkspaceConfig)).unwrap();
    let defs = &schema["definitions"];
    assert!(defs.get("LintConfig").is_some(), "must embed LintConfig");
    assert!(defs.get("StyleGuide").is_some(), "must embed StyleGuide");
    // Style enum reachable too (representative).
    assert!(defs.get("KeywordCase").is_some(), "must embed style enums");
}
