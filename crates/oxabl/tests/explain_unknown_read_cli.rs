//! A read of a table that the loaded schema does not have makes the effects
//! summary partial, as a write to one does. The tests run `oxabl explain` and
//! `oxabl check`. All fixtures are synthetic ABL.

use std::path::Path;
use std::process::Command;

use serde_json::Value;

const SCHEMA: &str = "ADD TABLE \"customer\"\nADD FIELD \"id\" OF \"customer\" AS integer\nADD INDEX \"id\" ON \"customer\"\n  UNIQUE\n  PRIMARY\n  INDEX-FIELD \"id\" ASCENDING\n";

fn workspace(files: &[(&str, &str)]) -> tempfile::TempDir {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"fixture\"\n[workspace.sources]\ninclude_paths = [\".\"]\n[workspace.schema]\nfiles = [\"schema.df\"]\n[workspace.lint]\nenable = [\"strict.transactions\"]\n",
    )
    .unwrap();
    std::fs::write(tmp.path().join("schema.df"), SCHEMA).unwrap();
    for (name, text) in files {
        std::fs::write(tmp.path().join(name), text).unwrap();
    }
    tmp
}

fn explain(dir: &Path, file: &str) -> Value {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["explain", file, "--json"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap()
}

/// `(complete, partial reasons)` of the unit `name` (`None` for the main block).
#[track_caller]
fn summary(value: &Value, name: Option<&str>) -> (bool, Vec<String>) {
    let unit = value["units"]
        .as_array()
        .unwrap()
        .iter()
        .find(|u| u["name"].as_str() == name)
        .unwrap_or_else(|| panic!("no unit {name:?} in {value:#}"));
    let summary = &unit["summary"];
    (
        summary["complete"].as_bool().unwrap(),
        summary["partial"]
            .as_array()
            .unwrap()
            .iter()
            .map(|r| r.as_str().unwrap().to_string())
            .collect(),
    )
}

#[track_caller]
fn assert_unknown_table(value: &Value, name: Option<&str>) {
    assert_eq!(
        summary(value, name),
        (false, vec!["unit:unknown-table".to_string()]),
        "{value:#}"
    );
}

#[test]
fn a_find_of_an_unknown_table_is_partial() {
    let tmp = workspace(&[("r.p", "FIND FIRST nosuch NO-LOCK NO-ERROR.\n")]);
    assert_unknown_table(&explain(tmp.path(), "r.p"), None);
}

#[test]
fn every_form_of_a_read_of_an_unknown_table_is_partial() {
    for (i, statement) in [
        "FOR EACH nosuch NO-LOCK:\nEND.",
        "DEFINE QUERY q FOR nosuch.\nOPEN QUERY q FOR EACH nosuch NO-LOCK.",
        "DEFINE VARIABLE l AS LOGICAL NO-UNDO.\nl = CAN-FIND(FIRST nosuch).",
        "DEFINE BUFFER b FOR nosuch.\nFIND FIRST b NO-LOCK NO-ERROR.",
    ]
    .iter()
    .enumerate()
    {
        let file = format!("form{i}.p");
        let tmp = workspace(&[(file.as_str(), &format!("{statement}\n"))]);
        assert_unknown_table(&explain(tmp.path(), &file), None);
    }
}

#[test]
fn a_read_of_an_unknown_table_in_a_class_is_partial() {
    let tmp = workspace(&[(
        "c.cls",
        "CLASS c:\n  METHOD PUBLIC VOID m():\n    FIND FIRST nosuch NO-LOCK NO-ERROR.\n  END METHOD.\nEND CLASS.\n",
    )]);
    let value = explain(tmp.path(), "c.cls");
    assert_unknown_table(&value, Some("m"));
    assert_eq!(summary(&value, None), (true, vec![]), "{value:#}");
}

#[test]
fn a_read_of_a_table_of_the_schema_stays_complete() {
    let tmp = workspace(&[("r.p", "FIND FIRST customer NO-LOCK NO-ERROR.\n")]);
    assert_eq!(
        summary(&explain(tmp.path(), "r.p"), None),
        (true, vec![]),
        "a table the schema has must not be marked unknown"
    );
}

#[test]
fn a_caller_of_a_unit_with_an_unknown_read_is_partial_too() {
    let tmp = workspace(&[
        ("mid.p", "RUN callee.p.\n"),
        ("callee.p", "FIND FIRST nosuch NO-LOCK NO-ERROR.\n"),
        (
            "top.p",
            "PROCEDURE p:\n  RUN mid.p.\nEND PROCEDURE.\nRUN p.\n",
        ),
    ]);
    let value = explain(tmp.path(), "top.p");
    assert_unknown_table(&value, Some("p"));
    assert_unknown_table(&value, None);
    assert_unknown_table(&explain(tmp.path(), "mid.p"), None);
}

#[test]
fn a_callee_with_an_unknown_read_does_not_clear_the_transaction_finding() {
    for (callee, silent) in [
        ("FIND FIRST customer NO-LOCK NO-ERROR.\n", true),
        ("FIND FIRST nosuch NO-LOCK NO-ERROR.\n", false),
    ] {
        let tmp = workspace(&[
            ("callee.p", callee),
            ("main.p", "DO TRANSACTION:\n  RUN callee.p.\nEND.\n"),
        ]);
        let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .current_dir(tmp.path())
            .args(["check", "--no-format", "main.p"])
            .output()
            .unwrap();
        let text = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            text.contains("TX004"),
            !silent,
            "callee {callee:?}:\n{text}\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
}
