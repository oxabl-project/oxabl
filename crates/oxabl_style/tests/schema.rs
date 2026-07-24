//! JSON Schema coverage for [`StyleGuide`] (backs the `oxabl schema` subcommand
//! and the VS Code extension's `oxabl.toml` autocomplete/validation).
//!
//! These tests pin the schemars-derived schema to the actual serde surface of
//! the config structs so the extension ships a drift-free schema (KTD1) and a
//! newly added style rule is covered automatically with no extra edit (D1).

use std::collections::BTreeSet;

use oxabl_style::StyleGuide;

/// Object-property key set of a `serde_json::Value::Object`.
fn object_keys(value: &serde_json::Value) -> BTreeSet<String> {
    value
        .as_object()
        .expect("expected a JSON object")
        .keys()
        .cloned()
        .collect()
}

#[test]
fn style_schema_denies_unknown_fields() {
    let schema = serde_json::to_value(schemars::schema_for!(StyleGuide)).unwrap();
    // Mirrors `#[serde(deny_unknown_fields)]`: a misspelled rule key must fail.
    assert_eq!(
        schema["additionalProperties"],
        serde_json::Value::Bool(false),
        "StyleGuide schema must set additionalProperties: false"
    );
}

#[test]
fn style_schema_has_representative_keys_with_types() {
    let schema = serde_json::to_value(schemars::schema_for!(StyleGuide)).unwrap();
    let props = &schema["properties"];

    // A plain integer field.
    assert_eq!(props["indent_size"]["type"], "integer");
    // A recently added integer field (guards it flows through the derive).
    assert_eq!(props["max_consecutive_blank_lines"]["type"], "integer");

    // An enum field references its definition; the definition carries the enum.
    let keyword_case = &schema["definitions"]["KeywordCase"]["enum"];
    let variants: Vec<&str> = keyword_case
        .as_array()
        .unwrap()
        .iter()
        .map(|v| v.as_str().unwrap())
        .collect();
    assert_eq!(variants, ["Uppercase", "Lowercase", "Preserve"]);
}

#[test]
fn style_schema_covers_every_serialized_field() {
    // D1 auto-coverage guard: the schema property set must equal the struct's
    // serde field set. Both are derived from `StyleGuide`, so adding a field
    // flows to both automatically — this test fails only if the two ever drift
    // (e.g. a `#[serde(skip)]` on one side but not reflected here).
    let schema = serde_json::to_value(schemars::schema_for!(StyleGuide)).unwrap();
    let schema_props = object_keys(&schema["properties"]);

    let serialized = serde_json::to_value(StyleGuide::default()).unwrap();
    let serde_fields = object_keys(&serialized);

    assert_eq!(
        schema_props, serde_fields,
        "the StyleGuide JSON Schema properties must match its serialized fields exactly"
    );
}
