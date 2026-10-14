//! `oxabl check --baseline` / `--update-baseline`: exit codes, counts, and
//! matching that survives edits which move a finding to another line.
//!
//! All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// Run `oxabl check --no-format <args>` in `dir`; `(exit, stdout, stderr)`.
fn check(dir: &Path, args: &[&str]) -> (Option<i32>, String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .arg("check")
        .arg("--no-format")
        .args(args)
        .output()
        .unwrap();
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

/// A workspace with an `oxabl.toml` and two files, each with one finding.
fn workspace() -> TempDir {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n",
    );
    write(
        &dir.path().join("src/a.p"),
        "DEFINE VARIABLE v-old AS INTEGER NO-UNDO.\nMESSAGE \"a\".\n",
    );
    write(
        &dir.path().join("src/b.p"),
        "DEFINE VARIABLE v-other AS INTEGER NO-UNDO.\n",
    );
    dir
}

fn json(stdout: &str) -> serde_json::Value {
    serde_json::from_str(stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"))
}

#[test]
fn a_baselined_tree_passes_and_only_a_new_finding_fails() {
    let dir = workspace();
    let root = dir.path();

    let (exit, _, stderr) = check(root, &["src"]);
    assert_eq!(exit, Some(1), "two findings without a baseline: {stderr}");

    let (exit, stdout, stderr) = check(root, &["src", "--baseline", "b.json", "--update-baseline"]);
    assert_eq!(exit, Some(0), "an update reports nothing: {stdout}{stderr}");
    assert!(
        stderr.contains("baseline: updated b.json (2 added, 0 removed)"),
        "{stderr}"
    );
    let written = fs::read_to_string(root.join("b.json")).unwrap();
    let doc = json(&written);
    assert_eq!(doc["version"], 1);
    let paths: Vec<_> = doc["entries"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e["path"].as_str().unwrap().to_string())
        .collect();
    assert_eq!(paths, ["src/a.p", "src/b.p"], "workspace-relative, sorted");

    let (exit, stdout, stderr) = check(root, &["src", "--baseline", "b.json"]);
    assert_eq!(exit, Some(0), "{stdout}{stderr}");
    assert!(
        stderr.contains("baseline: 0 new, 2 baselined, 0 stale"),
        "{stderr}"
    );

    // Shift the old finding down and add a new one: only the new one fails.
    write(
        &root.join("src/a.p"),
        "/* a header */\n\nMESSAGE \"first\".\nDEFINE VARIABLE   v-old AS INTEGER NO-UNDO.\n\
         DEFINE VARIABLE v-new AS INTEGER NO-UNDO.\nMESSAGE \"a\".\n",
    );
    let (exit, stdout, stderr) = check(root, &["src", "--baseline", "b.json", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let report = json(&stdout);
    let names: Vec<_> = report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["message"].as_str().unwrap().to_string())
        .collect();
    assert_eq!(names.len(), 1, "{names:?}");
    assert!(names[0].contains("v-new"), "{names:?}");
    assert_eq!(report["baseline"]["new"], 1);
    assert_eq!(report["baseline"]["baselined"], 2);
    assert_eq!(report["baseline"]["stale"], 0);
    assert_eq!(report["baseline"]["updated"], false);
}

#[test]
fn a_fixed_finding_is_stale_and_an_update_shrinks_the_baseline() {
    let dir = workspace();
    let root = dir.path();
    check(root, &["src", "--baseline", "b.json", "--update-baseline"]);

    write(&root.join("src/b.p"), "MESSAGE \"fixed\".\n");
    let (exit, _, stderr) = check(root, &["src", "--baseline", "b.json"]);
    assert_eq!(exit, Some(0), "a fixed finding does not fail: {stderr}");
    assert!(stderr.contains("0 new, 1 baselined, 1 stale"), "{stderr}");

    // A run over one file keeps the other file's entries.
    let (exit, _, stderr) = check(
        root,
        &["src/a.p", "--baseline", "b.json", "--update-baseline"],
    );
    assert_eq!(exit, Some(0), "{stderr}");
    let doc = json(&fs::read_to_string(root.join("b.json")).unwrap());
    assert_eq!(
        doc["entries"].as_array().unwrap().len(),
        2,
        "b.p not checked"
    );

    let (_, _, stderr) = check(root, &["src", "--baseline", "b.json", "--update-baseline"]);
    assert!(stderr.contains("(0 added, 1 removed)"), "{stderr}");
    let doc = json(&fs::read_to_string(root.join("b.json")).unwrap());
    assert_eq!(doc["entries"].as_array().unwrap().len(), 1);
}

#[test]
fn a_missing_or_malformed_baseline_is_a_usage_error() {
    let dir = workspace();
    let root = dir.path();
    let (exit, _, stderr) = check(root, &["src", "--baseline", "missing.json"]);
    assert_eq!(exit, Some(2), "{stderr}");
    assert!(stderr.contains("cannot read baseline"), "{stderr}");

    write(&root.join("bad.json"), "{\"version\": 9, \"entries\": []}");
    let (exit, _, stderr) = check(root, &["src", "--baseline", "bad.json"]);
    assert_eq!(exit, Some(2), "{stderr}");
    assert!(stderr.contains("not supported"), "{stderr}");
}

#[test]
fn update_baseline_needs_a_file_and_lint_findings() {
    let dir = workspace();
    let root = dir.path();
    let (exit, _, _) = check(root, &["src", "--update-baseline"]);
    assert_eq!(exit, Some(2), "requires --baseline");
    let (exit, _, _) = check(
        root,
        &[
            "src",
            "--baseline",
            "b.json",
            "--update-baseline",
            "--no-lint",
        ],
    );
    assert_eq!(exit, Some(2), "would drop every lint entry");
}

#[test]
fn format_drift_still_fails_a_baselined_run() {
    let dir = workspace();
    let root = dir.path();
    write(&root.join("src/c.p"), "DO:\nMESSAGE \"x\".\nEND.\n");
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(root)
        .args(["check", "src", "--baseline", "b.json", "--update-baseline"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
}
