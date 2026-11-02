//! `oxabl build --plan --json`: the shape of the document.

mod build_support;

use build_support::Workspace;
use serde_json::Value;

fn workspace() -> Workspace {
    let workspace = Workspace::with_sources("include_paths = [\"inc\", \"src\"]");
    workspace.write("inc/inner.i", "DEFINE VARIABLE inner AS INTEGER NO-UNDO.\n");
    workspace.write("src/direct.p", "{inner.i}\n");
    workspace.write("src/alone.p", "MESSAGE \"a\".\n");
    workspace
}

fn document(run: &build_support::Run) -> Value {
    run.succeeded();
    serde_json::from_str(&run.stdout).unwrap_or_else(|e| panic!("{e}: {}", run.stdout))
}

fn keys(value: &Value) -> Vec<&str> {
    let mut keys: Vec<&str> = value
        .as_object()
        .unwrap()
        .keys()
        .map(String::as_str)
        .collect();
    keys.sort();
    keys
}

#[test]
fn the_document_has_the_documented_shape() {
    let workspace = workspace();
    let doc = document(&workspace.run(&["--plan", "--json"]));
    assert_eq!(
        keys(&doc),
        [
            "environment",
            "format",
            "removed",
            "store",
            "summary",
            "units",
            "unknown_files"
        ]
    );
    assert_eq!(doc["format"], 1);
    assert_eq!(keys(&doc["environment"]), ["at", "identity", "source"]);
    assert_eq!(doc["environment"]["source"], "none");
    assert!(doc["environment"]["identity"].is_null());
    assert!(doc["environment"]["at"].is_null());
    assert_eq!(keys(&doc["store"]), ["committed", "generation"]);
    assert_eq!(doc["store"]["committed"], true);
    assert!(doc["store"]["generation"].is_u64());
    assert_eq!(
        keys(&doc["summary"]),
        ["rebuild", "skipped", "unkeyable", "up_to_date"]
    );
    assert_eq!(doc["summary"]["rebuild"], 2);
    assert_eq!(doc["summary"]["up_to_date"], 0);
    assert!(doc["removed"].as_array().unwrap().is_empty());
    assert!(doc["unknown_files"].as_array().unwrap().is_empty());
    let units = doc["units"].as_array().unwrap();
    assert_eq!(units.len(), 2);
    let unit = &units[0];
    assert_eq!(
        keys(unit),
        [
            "action",
            "compile_name",
            "key",
            "path",
            "reasons",
            "unkeyable"
        ]
    );
    assert_eq!(unit["path"], "src/alone.p");
    assert_eq!(unit["action"], "rebuild");
    assert!(unit["compile_name"].is_null());
    assert!(unit["key"].is_null());
    assert!(unit["unkeyable"].as_array().unwrap().is_empty());
    assert_eq!(unit["reasons"][0]["kind"], "new");
    assert!(unit["reasons"][0]["paths"].as_array().unwrap().is_empty());
}

#[test]
fn a_dependency_reason_lists_its_files() {
    let workspace = workspace();
    workspace.plan().succeeded();
    workspace.write("inc/inner.i", "DEFINE VARIABLE other AS INTEGER NO-UNDO.\n");
    let doc = document(&workspace.run(&["--plan", "--json"]));
    let units = doc["units"].as_array().unwrap();
    assert_eq!(units.len(), 1);
    assert_eq!(units[0]["path"], "src/direct.p");
    assert_eq!(units[0]["reasons"][0]["kind"], "dependency-changed");
    assert_eq!(
        units[0]["reasons"][0]["paths"],
        serde_json::json!(["inc/inner.i"])
    );
    assert_eq!(doc["summary"]["rebuild"], 1);
    assert_eq!(doc["summary"]["up_to_date"], 1);
}

#[test]
fn all_lists_the_up_to_date_units() {
    let workspace = workspace();
    workspace.plan().succeeded();
    workspace.write("src/alone.p", "MESSAGE \"b\".\n");
    let doc = document(&workspace.run(&["--plan", "--json", "--all"]));
    let rows: Vec<(&str, &str)> = doc["units"]
        .as_array()
        .unwrap()
        .iter()
        .map(|u| (u["path"].as_str().unwrap(), u["action"].as_str().unwrap()))
        .collect();
    assert_eq!(
        rows,
        [("src/alone.p", "rebuild"), ("src/direct.p", "up-to-date")]
    );
    let up_to_date = &doc["units"][1];
    assert!(up_to_date["reasons"].as_array().unwrap().is_empty());
}

#[test]
fn named_reasons_carry_the_way_and_closure_changes_carry_both_sides() {
    let workspace = workspace();
    workspace.plan().succeeded();
    let named = document(&workspace.run(&["--plan", "--json", "--files", "inc/inner.i", "nope.p"]));
    let reason = &named["units"][0]["reasons"][0];
    assert_eq!(reason["kind"], "named");
    assert_eq!(reason["paths"], serde_json::json!(["inc/inner.i"]));
    assert_eq!(
        reason["via"],
        serde_json::json!([["src/direct.p", "inc/inner.i"]])
    );
    assert_eq!(named["unknown_files"], serde_json::json!(["nope.p"]));

    workspace.write("src/direct.p", "MESSAGE \"x\".\n");
    let changed = document(&workspace.run(&["--plan", "--json"]));
    let reasons = changed["units"][0]["reasons"].as_array().unwrap();
    let closure = reasons
        .iter()
        .find(|r| r["kind"] == "closure-changed")
        .unwrap();
    assert_eq!(closure["lost"], serde_json::json!(["inc/inner.i"]));
    assert_eq!(closure["gained"], serde_json::json!([]));
    assert_eq!(closure["paths"], serde_json::json!(["inc/inner.i"]));
}

#[test]
fn removed_units_are_listed() {
    let workspace = workspace();
    workspace.plan().succeeded();
    workspace.remove("src/alone.p");
    let doc = document(&workspace.run(&["--plan", "--json"]));
    assert_eq!(doc["removed"], serde_json::json!(["src/alone.p"]));
}

#[test]
fn json_goes_with_since() {
    let workspace = workspace();
    workspace.commit_all("base");
    workspace.plan().succeeded();
    workspace.write("src/alone.p", "MESSAGE \"c\".\n");
    let doc = document(&workspace.run(&["--plan", "--since", "HEAD", "--json"]));
    assert_eq!(doc["units"][0]["reasons"][0]["kind"], "named");
    assert_eq!(doc["units"][0]["path"], "src/alone.p");
}

#[test]
fn a_plan_that_could_not_take_the_lock_says_it_did_not_commit() {
    let workspace = workspace();
    workspace.plan().succeeded();
    std::fs::create_dir_all(workspace.state_dir()).unwrap();
    let lock = std::fs::File::create(workspace.state_dir().join("build.lock")).unwrap();
    lock.lock().unwrap();
    let doc = document(&workspace.run(&["--plan", "--json"]));
    assert_eq!(doc["store"]["committed"], false);
}

#[test]
fn json_needs_plan() {
    let workspace = workspace();
    assert_eq!(workspace.run(&["--json"]).code, 2);
}
