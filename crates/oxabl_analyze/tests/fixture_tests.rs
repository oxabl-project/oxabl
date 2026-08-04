//! Fixture-driven tests for the `oxabl_analyze` JSON dump.
//!
//! Instead of brittle exact-JSON goldens (NodeIds aren't stable across
//! parser changes), these tests assert *structural* properties of the
//! dump for each fixture: envelope shape, expected symbols, expected
//! diagnostics by code, section invariants. Fixtures live in
//! `tests/fixtures/`.

use oxabl_analyze::dump_json;
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use serde_json::Value;
use std::path::Path;

fn dump_fixture(name: &str) -> Value {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
    let tokens = tokenize(&source);
    let mut parser = Parser::new(&tokens, &source);
    let program = parser
        .parse_statements()
        .unwrap_or_else(|e| panic!("parse {}: {}", path.display(), e.message));
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::new(1), &source, &schema);
    let sem = analyze_file(&program, &ctx);
    dump_json(
        &program,
        &sem,
        &ctx,
        true,
        &oxabl_analyze::DependencySection::default(),
    )
}

fn symbol_names(dump: &Value) -> Vec<String> {
    dump["symbols"]
        .as_array()
        .unwrap()
        .iter()
        .map(|s| s["name"].as_str().unwrap().to_string())
        .collect()
}

fn diagnostic_codes(dump: &Value) -> Vec<String> {
    dump["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].as_str().unwrap().to_string())
        .collect()
}

// ---------------------------------------------------------------------------
// Shared envelope checks
// ---------------------------------------------------------------------------

fn assert_envelope_sane(dump: &Value) {
    assert_eq!(dump["envelope"], 1);
    // Presence and numeric type only, deliberately: a section version *bumps*
    // whenever that section's rows change shape, so pinning the numbers here
    // would turn every legitimate bump into a fixture-test failure.
    let names = [
        "scopes",
        "symbols",
        "types",
        "references",
        "diagnostics",
        "preproc",
        "coverage",
        "dependencies",
    ];
    assert_eq!(names.len(), 8, "the envelope emits eight sections");
    for section in names {
        assert!(
            dump["sections"][section].is_number(),
            "{section} must carry a version"
        );
    }
    assert!(dump["scopes"].is_array());
    assert!(dump["symbols"].is_array());
    assert!(dump["references"].is_array());
    assert!(dump["types"].is_array());
    assert!(dump["diagnostics"].is_array());
    // `preproc` is an array of the same diagnostic rows; `coverage` is an object,
    // so the next coverage fact is an added key rather than a new section.
    assert!(dump["preproc"].is_array());
    assert!(dump["coverage"]["unjudged_symbols"].is_u64());
    // `dependencies` is an object for the same reason `coverage` is: the next
    // index fact is an added key, not a ninth section. Empty-but-present on a
    // file with nothing cross-file about it — these fixtures are all single-file
    // and analyzed with no index, so `index_revision` is 0 and both arrays are
    // empty, which is a shape a consumer can index into unconditionally.
    assert!(
        dump["dependencies"].is_object(),
        "dependencies is an object"
    );
    assert_eq!(dump["dependencies"]["index_revision"], 0);
    assert_eq!(
        dump["dependencies"]["files"].as_array().map(Vec::len),
        Some(0)
    );
    assert_eq!(
        dump["dependencies"]["unresolved"].as_array().map(Vec::len),
        Some(0)
    );
}

// ---------------------------------------------------------------------------
// Per-fixture tests
// ---------------------------------------------------------------------------

#[test]
fn simple_variable_has_x_and_y_and_no_diagnostics() {
    let d = dump_fixture("simple_variable.p");
    assert_envelope_sane(&d);
    let names = symbol_names(&d);
    assert!(names.contains(&"x".into()));
    assert!(names.contains(&"y".into()));
    // Only semantic diagnostics should appear here (none expected).
    let semantic_only: Vec<_> = d["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["source"] == "semantic")
        .collect();
    assert!(
        semantic_only.is_empty(),
        "unexpected semantic diagnostics: {semantic_only:?}"
    );
}

#[test]
fn simple_variable_both_x_and_y_used() {
    // MESSAGE reads them, so LINT0002 should not fire for either.
    let d = dump_fixture("simple_variable.p");
    let codes = diagnostic_codes(&d);
    assert!(
        !codes.contains(&"LINT0002".into()),
        "unexpected unused-variable lint: {codes:?}"
    );
}

#[test]
fn procedure_with_params_declares_procedure_symbol() {
    let d = dump_fixture("procedure_with_params.p");
    assert_envelope_sane(&d);
    let procs: Vec<_> = d["symbols"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|s| s["kind"] == "procedure")
        .collect();
    assert_eq!(procs.len(), 1);
    assert_eq!(procs[0]["name"], "add-numbers");
}

#[test]
fn procedure_params_are_scoped_inside_procedure() {
    let d = dump_fixture("procedure_with_params.p");
    // Find procedure scope.
    let proc_scope = d["scopes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["kind"] == "procedure")
        .expect("procedure scope");
    // Its bindings should include a, b, result in values ns.
    let binding_names: Vec<&str> = proc_scope["bindings"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|b| b["namespace"] == "values")
        .map(|b| b["name"].as_str().unwrap())
        .collect();
    for expected in ["a", "b", "result"] {
        assert!(
            binding_names.contains(&expected),
            "missing param {expected} in {binding_names:?}"
        );
    }
}

#[test]
fn procedure_no_unused_output_parameter_warning() {
    let d = dump_fixture("procedure_with_params.p");
    // `result` is OUTPUT — LINT0002 should skip it even though it's never read.
    let codes = diagnostic_codes(&d);
    assert!(
        !codes.contains(&"LINT0002".into()),
        "OUTPUT param must be skipped: {codes:?}"
    );
}

#[test]
fn function_with_return_has_decimal_return_type() {
    let d = dump_fixture("function_with_return.p");
    let f = d["symbols"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["kind"] == "function" && s["name"] == "calc")
        .expect("calc function symbol");
    assert_eq!(f["data_type"], "decimal");
}

#[test]
fn unused_variable_fixture_emits_lint0002() {
    let d = dump_fixture("unused_variable.p");
    let codes = diagnostic_codes(&d);
    // The `unused` variable should trigger LINT0002 once; `used` is read.
    let unused_count = codes.iter().filter(|c| *c == "LINT0002").count();
    assert!(unused_count >= 1, "expected LINT0002, got: {codes:?}");
}

#[test]
fn undefined_symbol_fixture_emits_lint0001() {
    let d = dump_fixture("undefined_symbol.p");
    let codes = diagnostic_codes(&d);
    assert!(
        codes.iter().any(|c| c == "LINT0001"),
        "expected LINT0001, got: {codes:?}"
    );
}

#[test]
fn every_fixture_round_trips_as_json_string() {
    for name in [
        "simple_variable.p",
        "procedure_with_params.p",
        "function_with_return.p",
        "unused_variable.p",
        "undefined_symbol.p",
    ] {
        let dump = dump_fixture(name);
        let s = serde_json::to_string(&dump).unwrap();
        let back: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(back["envelope"], 1, "{name} fails round-trip");
    }
}

#[test]
fn every_fixture_has_file_scope() {
    for name in [
        "simple_variable.p",
        "procedure_with_params.p",
        "function_with_return.p",
        "unused_variable.p",
        "undefined_symbol.p",
    ] {
        let d = dump_fixture(name);
        let has_file = d["scopes"]
            .as_array()
            .unwrap()
            .iter()
            .any(|s| s["kind"] == "file");
        assert!(has_file, "{name} missing file scope");
    }
}

#[test]
fn every_fixture_seeds_system_handle_builtins() {
    for name in [
        "simple_variable.p",
        "procedure_with_params.p",
        "function_with_return.p",
        "unused_variable.p",
        "undefined_symbol.p",
    ] {
        let d = dump_fixture(name);
        let builtins: Vec<_> = d["symbols"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|s| s["kind"] == "builtin")
            .collect();
        assert_eq!(
            builtins.len(),
            oxabl_semantic::SYSTEM_HANDLES.len(),
            "{name} should seed the full system-handle set"
        );
    }
}

#[test]
fn diagnostics_each_have_source_tag() {
    let d = dump_fixture("unused_variable.p");
    for diag in d["diagnostics"].as_array().unwrap() {
        let src = diag["source"].as_str().unwrap();
        assert!(matches!(src, "semantic" | "lint"), "bad source: {src}");
    }
}
