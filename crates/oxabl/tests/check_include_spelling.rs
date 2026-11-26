//! `oxabl check --include-findings` spells each file path of a report once for
//! the run, whatever the number of findings, expansions and chain steps that
//! name it. A path spelled for each chain step of each expansion of each
//! finding makes the report cost the findings times the length of the chains.
//!
//! A test build of the binary counts the spellings of the include findings
//! and prints them when `OXABL_TEST_TALLY` is set. Each test runs one input
//! with few findings or expansions and one with many, and wants the same
//! count. All fixtures are synthetic ABL written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

/// `count` unused variables: each is one finding in the include.
fn declarations(count: usize) -> String {
    (0..count)
        .map(|i| format!("DEFINE VARIABLE spare-{i} AS INTEGER NO-UNDO.\n"))
        .collect()
}

/// A root that reaches `inner.i` through `outer.i`, `times` times, each in a
/// procedure of its own, so each finding has `times` expansions of two steps.
fn root(times: usize) -> String {
    (0..times)
        .map(|i| format!("PROCEDURE p{i}:\n{{outer.i}}\nEND PROCEDURE.\n"))
        .collect()
}

fn tree(findings: usize, times: usize) -> TempDir {
    let dir = TempDir::new().unwrap();
    let files = [
        ("oxabl.toml", CONFIG.to_string()),
        ("inner.i", declarations(findings)),
        ("outer.i", "{inner.i}\n".to_string()),
        ("prog.p", root(times)),
        ("other.p", root(1)),
    ];
    for (name, contents) in files {
        fs::write(dir.path().join(name), contents).unwrap();
    }
    dir
}

/// What one run of `oxabl check --include-findings` gave.
struct Run {
    stdout: String,
    /// The spellings the run made.
    spelled: usize,
}

fn check(dir: &Path, args: &[&str]) -> Run {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .env("OXABL_TEST_TALLY", "1")
        .args(["check", "--no-format", "-j", "1", "--include-findings"])
        .args(args)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "stderr:\n{stderr}");
    let spelled = stderr
        .lines()
        .filter_map(|line| line.strip_prefix("oxabl-test: tally display.spell "))
        .map(|line| line.split(' ').next().unwrap().parse::<usize>().unwrap())
        .sum();
    Run {
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        spelled,
    }
}

/// The run of `args` over a tree of `findings` and `times`, for the roots
/// `prog.p` and `other.p`.
fn run(findings: usize, times: usize, args: &[&str]) -> Run {
    let dir = tree(findings, times);
    check(dir.path(), &[args, &["."]].concat())
}

fn rows(stdout: &str) -> usize {
    let report: serde_json::Value = serde_json::from_str(stdout)
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}"));
    report["include_diagnostics"].as_array().unwrap().len()
}

#[test]
fn json_spells_each_path_once_whatever_the_findings() {
    let few = run(2, 1, &["--json"]);
    let many = run(60, 1, &["--json"]);
    assert_eq!(rows(&few.stdout), 2 * 2);
    assert_eq!(rows(&many.stdout), 60 * 2);
    assert!(few.spelled > 0, "the build counted no spelling");
    assert_eq!(few.spelled, many.spelled);
}

#[test]
fn json_spells_each_path_once_whatever_the_expansions() {
    let once = run(3, 1, &["--json"]);
    let many = run(3, 12, &["--json"]);
    assert!(once.spelled > 0, "the build counted no spelling");
    assert_eq!(once.spelled, many.spelled);
}

#[test]
fn text_spells_each_path_once_whatever_the_findings_and_expansions() {
    let few = run(2, 1, &[]);
    let many = run(60, 12, &[]);
    assert!(few.stdout.contains("reached through"), "{}", few.stdout);
    assert!(few.spelled > 0, "the build counted no spelling");
    assert_eq!(few.spelled, many.spelled);
}

/// Every `root` and chain path of the rows of `args` over a small tree, with
/// the path of the walked directory `walked`.
fn spelled_paths(walked: impl Fn(&Path) -> String) -> Vec<String> {
    let dir = tree(2, 2);
    let run = check(dir.path(), &["--json", &walked(dir.path())]);
    let report: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
    let mut paths = Vec::new();
    for row in report["include_diagnostics"].as_array().unwrap() {
        paths.push(row["root"].as_str().unwrap().to_string());
        paths.push(row["path"].as_str().unwrap().to_string());
        for expansion in row["expansions"].as_array().unwrap() {
            for step in expansion["chain"].as_array().unwrap() {
                paths.push(step["path"].as_str().unwrap().to_string());
            }
        }
    }
    assert!(!paths.is_empty());
    paths
}

#[test]
fn a_relative_walk_keeps_every_path_relative() {
    for path in spelled_paths(|_| ".".to_string()) {
        assert!(Path::new(&path).is_relative(), "{path}");
    }
}

#[test]
fn an_absolute_walk_keeps_every_path_absolute() {
    for path in spelled_paths(|dir| dir.display().to_string()) {
        assert!(Path::new(&path).is_absolute(), "{path}");
    }
}
