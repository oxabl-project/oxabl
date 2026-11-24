//! `oxabl check --summary`: counts per rule and per file, as text and in the
//! `--json` report.
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

/// `(exit, stdout, stderr)` of `oxabl check --no-format <args>` in `dir`.
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
        String::from_utf8_lossy(&output.stdout).replace('\\', "/"),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn json(stdout: &str) -> serde_json::Value {
    serde_json::from_str(stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"))
}

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\n\
     include_paths = [\".\", \"src\"]\n[workspace.lint]\nenable = [\"strict\"]\n";

const VALUE_PROGRAM: &str = "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\nRUN VALUE(v).\n";

/// Three CL001 and one CL005 in `many.p`, one CL001 in `one.p` and in
/// `two.p`, one CL001 in `inc.i`, two programs that include it.
fn tree() -> TempDir {
    let dir = TempDir::new().unwrap();
    let root = dir.path();
    write(&root.join("oxabl.toml"), CONFIG);
    write(
        &root.join("src/many.p"),
        "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\nRUN VALUE(v).\nRUN VALUE(v).\nRUN VALUE(v).\n\
         DEFINE NEW SHARED VARIABLE s AS CHARACTER NO-UNDO.\n",
    );
    write(&root.join("src/one.p"), VALUE_PROGRAM);
    write(&root.join("src/two.p"), VALUE_PROGRAM);
    write(
        &root.join("src/inc.i"),
        "DEFINE VARIABLE w AS CHARACTER NO-UNDO.\nRUN VALUE(w).\n",
    );
    write(&root.join("src/u.p"), "{inc.i}\n");
    write(&root.join("src/v.p"), "{inc.i}\n");
    dir
}

#[test]
fn text_counts_per_rule_and_per_file_and_prints_no_finding() {
    let dir = tree();
    let (exit, text, stderr) = check(dir.path(), &[".", "--summary"]);
    assert_eq!(exit, Some(1), "{stderr}");
    assert!(!text.contains("warning["), "no finding line: {text}");
    assert!(!text.contains("RUN VALUE(v)"), "no snippet: {text}");

    let lines: Vec<&str> = text.lines().map(str::trim).collect();
    assert_eq!(lines[0], "7 findings in 4 files", "{text}");
    let rule = |code: &str| {
        lines
            .iter()
            .find(|l| l.ends_with(code))
            .unwrap_or_else(|| panic!("no {code}: {text}"))
            .split_whitespace()
            .next()
            .unwrap()
            .to_string()
    };
    assert_eq!(rule("CL001"), "6", "{text}");
    assert_eq!(rule("CL005"), "1", "{text}");
    // Most findings first.
    let at = |needle: &str| lines.iter().position(|l| l.ends_with(needle)).unwrap();
    assert!(at("CL001") < at("CL005"), "{text}");
    assert!(
        at("./src/many.p") < at("./src/inc.i") && at("./src/inc.i") < at("./src/one.p"),
        "by count, then by name: {text}"
    );
}

#[test]
fn the_top_option_limits_the_files_and_says_so() {
    let dir = tree();
    let (_, text, stderr) = check(dir.path(), &[".", "--summary", "--summary-top", "2"]);
    assert!(text.contains("by file (top 2 of 4):"), "{stderr}{text}");
    assert!(text.contains("./src/many.p"), "{text}");
    assert!(text.contains("./src/inc.i"), "{text}");
    assert!(!text.contains("./src/two.p"), "{text}");
}

#[test]
fn json_adds_a_summary_and_keeps_the_findings() {
    let dir = tree();
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--summary", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let report = json(&stdout);
    assert_eq!(report["diagnostics"].as_array().unwrap().len(), 7);

    let summary = &report["summary"];
    assert_eq!(summary["findings"], 7, "{summary}");
    assert_eq!(summary["files"], 4);
    assert_eq!(
        summary["by_rule"],
        serde_json::json!([
            { "code": "CL001", "count": 6 },
            { "code": "CL005", "count": 1 },
        ])
    );
    assert_eq!(
        summary["by_file"][0],
        serde_json::json!({ "path": "./src/many.p", "count": 4 })
    );
    assert_eq!(summary["by_file"].as_array().unwrap().len(), 4);

    let (_, stdout, _) = check(
        dir.path(),
        &[".", "--summary", "--summary-top", "1", "--json"],
    );
    let report = json(&stdout);
    assert_eq!(report["summary"]["by_file"].as_array().unwrap().len(), 1);
    assert_eq!(report["summary"]["files"], 4, "the total is not cut");
}

#[test]
fn json_has_no_summary_without_the_flag() {
    let dir = tree();
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    assert!(json(&stdout).get("summary").is_none());
}

#[test]
fn the_summary_counts_only_the_findings_the_baseline_does_not_hold() {
    let dir = tree();
    let root = dir.path();
    let (exit, _, stderr) = check(root, &[".", "--baseline", "b.json", "--update-baseline"]);
    assert_eq!(exit, Some(0), "{stderr}");
    write(&root.join("src/new.p"), VALUE_PROGRAM);

    let (exit, stdout, stderr) = check(root, &[".", "--baseline", "b.json", "--summary", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let summary = &json(&stdout)["summary"];
    assert_eq!(summary["findings"], 1, "{summary}");
    assert_eq!(summary["by_file"][0]["path"], "./src/new.p");
}

#[test]
fn a_clean_run_prints_the_pass_line_and_a_zero_summary_in_json() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("oxabl.toml"), CONFIG);
    write(&dir.path().join("a.p"), "MESSAGE \"a\".\n");

    let (exit, text, stderr) = check(dir.path(), &[".", "--summary"]);
    assert_eq!(exit, Some(0), "{stderr}");
    assert!(text.starts_with("checked 1 file"), "{text}");
    assert!(!text.contains("by rule"), "{text}");

    let (_, stdout, _) = check(dir.path(), &[".", "--summary", "--json"]);
    let summary = &json(&stdout)["summary"];
    assert_eq!(summary["findings"], 0, "{summary}");
    assert_eq!(summary["by_rule"].as_array().unwrap().len(), 0);
}

#[test]
fn the_summary_does_not_combine_with_watch_and_the_top_needs_the_summary() {
    let dir = tree();
    let (exit, _, stderr) = check(dir.path(), &[".", "--summary", "--watch"]);
    assert_eq!(exit, Some(2), "{stderr}");
    let (exit, _, stderr) = check(dir.path(), &[".", "--summary-top", "3"]);
    assert_eq!(exit, Some(2), "{stderr}");
}
