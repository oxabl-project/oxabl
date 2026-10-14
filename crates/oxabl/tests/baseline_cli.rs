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
    assert_eq!(doc["version"], 2);
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

/// A `RUN VALUE` with no other finding: the variable is read by the `RUN`.
const DYNAMIC_RUN: &str =
    "DEFINE VARIABLE c AS CHARACTER NO-UNDO INITIAL \"x.p\".\nRUN VALUE(c).\n";

/// A workspace that turns on `strict.calls` only under `strict/`.
fn strict_workspace() -> TempDir {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n\
         [[workspace.lint.overrides]]\nfiles = [\"strict\"]\nenable = [\"strict.calls\"]\n",
    );
    write(&dir.path().join("strict/a.p"), DYNAMIC_RUN);
    write(&dir.path().join("legacy/a.p"), DYNAMIC_RUN);
    dir
}

/// `(path, code)` of every finding in a `--json` report.
fn findings(stdout: &str) -> Vec<(String, String)> {
    json(stdout)["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            let path = d["path"].as_str().unwrap().replace('\\', "/");
            (path, d["code"].as_str().unwrap().to_string())
        })
        .collect()
}

#[test]
fn a_strict_group_enabled_under_one_directory_fires_only_there() {
    let dir = strict_workspace();
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    assert!(!stderr.contains("warning: lint"), "known names: {stderr}");
    let found = findings(&stdout);
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("strict/a.p"), "{found:?}");
    assert_eq!(found[0].1, "CL001");
}

#[test]
fn a_strict_finding_round_trips_through_a_baseline() {
    let dir = strict_workspace();
    let root = dir.path();
    let (exit, _, stderr) = check(root, &[".", "--baseline", "b.json", "--update-baseline"]);
    assert_eq!(exit, Some(0), "{stderr}");
    let doc = json(&fs::read_to_string(root.join("b.json")).unwrap());
    let entries = doc["entries"].as_array().unwrap();
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0]["code"], "CL001");
    assert_eq!(entries[0]["path"], "strict/a.p");

    // Move the call down and add a second, different one.
    write(
        &root.join("strict/a.p"),
        "/* moved */\n\nDEFINE VARIABLE c AS CHARACTER NO-UNDO INITIAL \"x.p\".\n\
         DEFINE VARIABLE d AS CHARACTER NO-UNDO INITIAL \"y.p\".\n\nRUN VALUE(c).\nRUN VALUE(d).\n",
    );
    let (exit, stdout, stderr) = check(root, &[".", "--baseline", "b.json", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let report = json(&stdout);
    assert_eq!(report["baseline"]["new"], 1, "{report}");
    assert_eq!(report["baseline"]["baselined"], 1);
    assert_eq!(report["baseline"]["stale"], 0);
    let message = report["diagnostics"][0]["message"].as_str().unwrap();
    assert_eq!(report["diagnostics"][0]["code"], "CL001");
    assert!(
        report["diagnostics"][0]["start"]["line"] == 7,
        "the new call on line 7 is the one reported: {message}"
    );
}

/// Run `check` on `files` from `dir` with `--baseline b.json`, as JSON.
fn baselined(dir: &Path, files: &[&str]) -> (Option<i32>, serde_json::Value, String) {
    let mut args = vec!["--baseline", "b.json", "--json"];
    args.extend_from_slice(files);
    let (exit, stdout, stderr) = check(dir, &args);
    (exit, json(&stdout), stderr)
}

#[test]
fn a_keyword_case_change_keeps_the_baseline() {
    let dir = strict_workspace();
    let root = dir.path();
    check(root, &[".", "--baseline", "b.json", "--update-baseline"]);

    write(
        &root.join("strict/a.p"),
        "define variable c as character no-undo initial \"x.p\".\nrun value(c).\n",
    );
    let (exit, report, stderr) = baselined(root, &["."]);
    assert_eq!(exit, Some(0), "{stderr}");
    assert_eq!(report["baseline"]["new"], 0, "{report}");
    assert_eq!(report["baseline"]["baselined"], 1);
    assert_eq!(report["baseline"]["stale"], 0);
}

#[test]
fn a_path_with_dot_dot_is_the_same_file() {
    let dir = workspace();
    let root = dir.path();
    check(root, &["src", "--baseline", "b.json", "--update-baseline"]);

    // `sub` must exist for the OS to resolve `sub/..`.
    fs::create_dir_all(root.join("src/sub")).unwrap();
    for spelling in ["src/sub/../a.p", "./src/a.p", "src/../src/a.p"] {
        let (exit, report, stderr) = baselined(root, &[spelling]);
        assert_eq!(exit, Some(0), "{spelling}: {stderr}");
        assert_eq!(report["baseline"]["new"], 0, "{spelling}: {report}");
        assert_eq!(report["baseline"]["baselined"], 1, "{spelling}");
        assert_eq!(report["baseline"]["stale"], 0, "{spelling}");
    }
}

#[cfg(unix)]
#[test]
fn dot_dot_after_a_symlink_is_not_the_file_the_lexical_path_names() {
    let dir = workspace();
    let root = dir.path();
    check(root, &["src", "--baseline", "b.json", "--update-baseline"]);

    // `src/link/../a.p` reads `elsewhere/a.p`, which has the same text as
    // `src/a.p`. It must not borrow that file's baseline entry.
    let elsewhere = TempDir::new().unwrap();
    fs::create_dir(elsewhere.path().join("x")).unwrap();
    fs::copy(root.join("src/a.p"), elsewhere.path().join("a.p")).unwrap();
    std::os::unix::fs::symlink(elsewhere.path().join("x"), root.join("src/link")).unwrap();

    let (exit, report, stderr) = baselined(root, &["src/link/../a.p"]);
    assert_eq!(exit, Some(1), "{stderr}");
    assert_eq!(report["baseline"]["baselined"], 0, "{report}");
    assert_eq!(report["baseline"]["outside_root"], 1, "{report}");

    let (exit, report, stderr) = baselined(root, &["src/a.p"]);
    assert_eq!(exit, Some(0), "{stderr}");
    assert_eq!(report["baseline"]["baselined"], 1, "{report}");
}

#[test]
fn a_crlf_file_matches_its_lf_baseline() {
    let dir = workspace();
    let root = dir.path();
    check(root, &["src", "--baseline", "b.json", "--update-baseline"]);

    let crlf = fs::read_to_string(root.join("src/a.p"))
        .unwrap()
        .replace('\n', "\r\n");
    write(&root.join("src/a.p"), &crlf);
    let (exit, report, stderr) = baselined(root, &["src"]);
    assert_eq!(exit, Some(0), "{stderr}");
    assert_eq!(report["baseline"]["new"], 0, "{report}");
    assert_eq!(report["baseline"]["stale"], 0);
}

#[test]
fn a_file_outside_the_root_is_reported_and_never_recorded() {
    // The target has no `oxabl.toml` above it, so the baseline file's own
    // directory is the root, and the target is not under it.
    let dir = workspace();
    let root = dir.path();
    let outside = TempDir::new().unwrap();
    write(
        &outside.path().join("o.p"),
        "DEFINE VARIABLE v-out AS INTEGER NO-UNDO.\n",
    );
    let target = outside.path().join("o.p");
    let target = target.to_str().unwrap();

    let (exit, _, stderr) = check(root, &[target, "--baseline", "b.json", "--update-baseline"]);
    assert_eq!(exit, Some(1), "the outside finding is reported: {stderr}");
    assert!(stderr.contains("outside the baseline root"), "{stderr}");
    let doc = json(&fs::read_to_string(root.join("b.json")).unwrap());
    assert!(
        doc["entries"].as_array().unwrap().is_empty(),
        "no machine path is stored: {doc}"
    );

    let (exit, report, _) = baselined(root, &[target]);
    assert_eq!(exit, Some(1));
    assert_eq!(report["baseline"]["outside_root"], 1);
    assert_eq!(report["baseline"]["new"], 0);
}

#[test]
fn a_version_1_baseline_is_refused_with_the_fix() {
    let dir = workspace();
    let root = dir.path();
    write(&root.join("old.json"), "{\"version\": 1, \"entries\": []}");
    let (exit, _, stderr) = check(root, &["src", "--baseline", "old.json"]);
    assert_eq!(exit, Some(2), "{stderr}");
    assert!(stderr.contains("--update-baseline"), "{stderr}");
}
