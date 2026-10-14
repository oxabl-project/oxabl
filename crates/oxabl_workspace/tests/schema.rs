//! JSON Schema coverage for the `oxabl.toml` config graph (`WorkspaceConfig`
//! and the `[workspace.lint]` surface), backing the `oxabl schema` subcommand
//! and the VS Code extension's `oxabl.toml` autocomplete/validation.

use std::collections::BTreeSet;

use oxabl_workspace::WorkspaceConfig;

fn object_keys(value: &serde_json::Value) -> BTreeSet<String> {
    value
        .as_object()
        .expect("expected a JSON object")
        .keys()
        .cloned()
        .collect()
}

fn workspace_schema() -> serde_json::Value {
    serde_json::to_value(schemars::schema_for!(WorkspaceConfig)).unwrap()
}

#[test]
fn lint_schema_declares_groups_and_overrides() {
    let schema = workspace_schema();
    let lint = &schema["definitions"]["LintConfig"];
    assert_eq!(
        object_keys(&lint["properties"]),
        ["disable", "enable", "overrides"]
            .into_iter()
            .map(String::from)
            .collect()
    );
    let entry = &schema["definitions"]["LintOverride"];
    assert_eq!(
        object_keys(&entry["properties"]),
        ["disable", "enable", "files"]
            .into_iter()
            .map(String::from)
            .collect()
    );
}

#[test]
fn every_other_lint_key_is_a_rule_holding_a_severity() {
    let schema = workspace_schema();
    for name in ["LintConfig", "LintOverride"] {
        assert_eq!(
            schema["definitions"][name]["additionalProperties"]["$ref"],
            "#/definitions/LintSeverity",
            "{name}"
        );
    }
    let severities: Vec<&str> = schema["definitions"]["LintSeverity"]["enum"]
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap())
        .collect();
    assert_eq!(
        severities,
        ["off", "hint", "info", "warn", "warning", "error"]
    );
}

#[test]
fn workspace_schema_embeds_lint_and_style_definitions() {
    // The extension consumes the WorkspaceConfig schema; confirm the whole graph
    // (lint + style) is reachable from the top-level document.
    let schema = workspace_schema();
    let defs = &schema["definitions"];
    assert!(defs.get("LintConfig").is_some(), "must embed LintConfig");
    assert!(defs.get("StyleGuide").is_some(), "must embed StyleGuide");
    // Style enum reachable too (representative).
    assert!(defs.get("KeywordCase").is_some(), "must embed style enums");
}
