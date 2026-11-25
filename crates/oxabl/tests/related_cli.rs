//! `oxabl check` and the related locations of the unknown-value and
//! dynamic-handle findings: a `note:` line for each in the text output, and a
//! `related` array in `--json`, with the place in an include in the include
//! file.
//!
//! All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use serde_json::{Value, json};
use tempfile::TempDir;

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n\
     [workspace.lint]\nenable = [\"strict.handles\", \"strict.nulls\"]\n";

/// NL001: `x` is set to `?` in `reset.i`, then compared in the root.
const RESET: &str = "/* reset */\nx = ?.\n";
const COMPARES: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
                        {reset.i}\n\
                        IF x > 5 THEN MESSAGE \"big\".\n";

/// HD001: a query that is created again while it holds the first one.
const OVERWRITE: &str = "DEFINE VARIABLE hq AS HANDLE NO-UNDO.\n\
                         CREATE QUERY hq.\n\
                         CREATE QUERY hq.\n\
                         DELETE OBJECT hq.\n";

fn tree() -> TempDir {
    let dir = TempDir::new().unwrap();
    for (name, text) in [
        ("oxabl.toml", CONFIG),
        ("reset.i", RESET),
        ("compares.p", COMPARES),
        ("overwrite.p", OVERWRITE),
    ] {
        fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

fn check(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["check", "--no-format"])
        .args(args)
        .output()
        .unwrap()
}

fn finding(report: &Value, code: &str) -> Value {
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["code"] == json!(code))
        .unwrap_or_else(|| panic!("no {code} in {report:#}"))
        .clone()
}

#[test]
fn the_text_output_notes_each_related_location_before_the_help() {
    let dir = tree();
    let out = check(dir.path(), &["compares.p"]);
    let text = String::from_utf8(out.stdout).unwrap();
    assert!(
        text.contains(
            "compares.p:3:4: warning[NL001]: `x` may hold `?`: the comparison or condition can \
             skip code\n    3 | IF x > 5 THEN MESSAGE \"big\".\n  \
             note: reset.i:2:1: this may assign `?` to `x`\n  help: "
        ),
        "{text}"
    );

    let out = check(dir.path(), &["overwrite.p"]);
    let text = String::from_utf8(out.stdout).unwrap();
    assert!(
        text.contains(
            "overwrite.p:3:1: warning[HD001]: `hq` is assigned here while it still holds a \
             query, which nothing deletes\n    3 | CREATE QUERY hq.\n  \
             note: overwrite.p:2:1: the query in `hq` is created here\n  help: "
        ),
        "{text}"
    );
}

#[test]
fn the_json_report_carries_the_related_locations() {
    let dir = tree();
    let out = check(dir.path(), &["--json", "."]);
    let report: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(report["version"], 6);

    let nulls = finding(&report, "NL001");
    assert_eq!(
        nulls["related"],
        json!([{
            "path": "reset.i",
            "message": "this may assign `?` to `x`",
            "span": {"start": 12, "end": 17},
            "start": {"byte": 12, "line": 2, "column": 1},
            "end": {"byte": 17, "line": 2, "column": 6}
        }]),
        "{nulls:#}"
    );

    let handles = finding(&report, "HD001");
    assert_eq!(
        handles["related"],
        json!([{
            "path": handles["path"],
            "message": "the query in `hq` is created here",
            "span": {"start": 38, "end": 54},
            "start": {"byte": 38, "line": 2, "column": 1},
            "end": {"byte": 54, "line": 2, "column": 17}
        }]),
        "{handles:#}"
    );
}

#[test]
fn a_main_block_that_starts_with_an_include_relates_no_place_in_it() {
    let dir = TempDir::new().unwrap();
    let config = format!("{CONFIG}[workspace.schema]\nfiles = [\"db.df\"]\n");
    for (name, text) in [
        ("oxabl.toml", config.as_str()),
        (
            "db.df",
            "ADD TABLE \"Parcel\"\nADD FIELD \"ShipDate\" OF \"Parcel\" AS date\n  INITIAL ?\n",
        ),
        ("hdr.i", "DEFINE VARIABLE banner AS CHARACTER NO-UNDO.\n"),
        (
            "main.p",
            "{hdr.i}\n\
             DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
             IF n = 1 THEN FIND FIRST Parcel NO-LOCK.\n\
             IF Parcel.ShipDate < TODAY THEN MESSAGE banner.\n",
        ),
    ] {
        fs::write(dir.path().join(name), text).unwrap();
    }
    let out = check(dir.path(), &["--json", "main.p"]);
    let report: Value = serde_json::from_slice(&out.stdout).unwrap();
    let field = finding(&report, "NL004");
    assert_eq!(
        field["related"],
        json!([{
            "path": "main.p",
            "message": "`Parcel.ShipDate` may be `?` in the record read here",
            "span": {"start": 67, "end": 93},
            "start": {"byte": 67, "line": 3, "column": 15},
            "end": {"byte": 93, "line": 3, "column": 41}
        }]),
        "{field:#}"
    );
}
