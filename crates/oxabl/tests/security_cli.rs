//! `oxabl check` and the `security` rule group.
//!
//! The group is off until enabled, a sink in an include is reported once in the
//! include, and severities follow the rule defaults. All fixtures are
//! synthetic ABL.

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
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn json(stdout: &str) -> serde_json::Value {
    serde_json::from_str(stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"))
}

/// `(path, code, severity)` of every finding in a `--json` report.
fn findings(report: &serde_json::Value) -> Vec<(String, String, String)> {
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            (
                d["path"].as_str().unwrap().replace('\\', "/"),
                d["code"].as_str().unwrap().to_string(),
                d["severity"].as_str().unwrap().to_string(),
            )
        })
        .collect()
}

fn config(lint: &str) -> String {
    format!(
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\", \"src\"]\n{lint}"
    )
}

const SINKS: &str = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE \" + c).
OUTPUT TO VALUE(c).
";

#[test]
fn the_group_is_off_by_default_and_strict_does_not_enable_it() {
    for lint in ["", "[workspace.lint]\nenable = [\"strict\"]\n"] {
        let dir = TempDir::new().unwrap();
        write(&dir.path().join("oxabl.toml"), &config(lint));
        write(&dir.path().join("a.p"), SINKS);
        let (_, stdout, stderr) = check(dir.path(), &[".", "--json"]);
        let report = json(&stdout);
        assert!(
            findings(&report)
                .iter()
                .all(|(_, c, _)| !c.starts_with("ABLSEC")),
            "{report} {stderr}"
        );
    }
}

#[test]
fn enabling_the_group_reports_each_sink_at_its_default_severity() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("a.p"), SINKS);
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let mut sec: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c.starts_with("ABLSEC"))
        .map(|(_, c, s)| (c, s))
        .collect();
    sec.sort();
    assert_eq!(
        sec,
        [
            ("ABLSEC001".to_string(), "error".to_string()),
            ("ABLSEC005".to_string(), "warning".to_string()),
        ]
    );
}

#[test]
fn a_rule_can_be_turned_off_inside_the_group() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\nABLSEC005 = \"off\"\n"),
    );
    write(&dir.path().join("a.p"), SINKS);
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let codes: Vec<_> = findings(&json(&stdout)).into_iter().map(|f| f.1).collect();
    assert!(codes.contains(&"ABLSEC001".to_string()), "{codes:?}");
    assert!(!codes.contains(&"ABLSEC005".to_string()), "{codes:?}");
}

#[test]
fn a_sink_in_an_include_is_reported_once_in_the_include() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(&dir.path().join("src/run.i"), "RUN VALUE(c).\n");
    write(
        &dir.path().join("src/b.p"),
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n{run.i}\n",
    );
    write(
        &dir.path().join("src/d.p"),
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n{run.i}\n",
    );
    let (exit, stdout, stderr) = check(dir.path(), &[".", "--json"]);
    assert_eq!(exit, Some(1), "{stderr}");
    let found: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c == "ABLSEC004")
        .collect();
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("src/run.i"), "{found:?}");
}

#[test]
fn a_sink_from_a_define_is_reported_where_the_define_is_used() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("oxabl.toml"),
        &config("[workspace.lint]\nenable = [\"security\"]\n"),
    );
    write(
        &dir.path().join("a.p"),
        "&GLOBAL-DEFINE PROG cProg\nDEFINE VARIABLE cProg AS CHARACTER NO-UNDO.\nRUN VALUE({&PROG}).\n",
    );
    write(
        &dir.path().join("safe.p"),
        "&GLOBAL-DEFINE PROG \"post.p\"\nRUN VALUE({&PROG}).\n",
    );
    let (_, stdout, _) = check(dir.path(), &[".", "--json"]);
    let found: Vec<_> = findings(&json(&stdout))
        .into_iter()
        .filter(|(_, c, _)| c == "ABLSEC004")
        .collect();
    assert_eq!(found.len(), 1, "{found:?}");
    assert!(found[0].0.ends_with("a.p"), "{found:?}");
}
