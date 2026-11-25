//! A parse error inside an include is never dropped by `oxabl check`: the root
//! reports it at the include site, and the gate fails. All fixtures are
//! synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// A `CREATE` whose trigger block is never closed.
const BROKEN: &str = "DEF VAR x AS INT NO-UNDO.\n\
                      CREATE BUTTON h TRIGGERS: ON CHOOSE DO:\n  MESSAGE x.\n";

const CLEAN: &str = "DEF VAR y AS INT NO-UNDO.\nMESSAGE y.\n";

fn write(path: &Path, contents: &str) {
    fs::write(path, contents).unwrap();
}

struct Run {
    code: Option<i32>,
    stdout: String,
    stderr: String,
}

fn check(dir: &Path, args: &[&str]) -> Run {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .arg("check")
        .args(args)
        .output()
        .unwrap();
    Run {
        code: out.status.code(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

fn parse_errors(run: &Run) -> Vec<Value> {
    let v: Value = serde_json::from_str(&run.stdout)
        .unwrap_or_else(|e| panic!("not JSON ({e}):\n{}", run.stdout));
    v["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "PARSE001")
        .cloned()
        .collect()
}

#[test]
fn a_parse_error_in_an_include_fails_the_gate_at_the_include_site() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("crtail.i"), BROKEN);
    write(
        &tmp.path().join("rootcr.p"),
        "MESSAGE \"a\".\n{crtail.i}\nMESSAGE \"b\".\n",
    );
    let run = check(tmp.path(), &["-I", ".", "rootcr.p"]);
    assert_eq!(run.code, Some(1), "{}\n{}", run.stdout, run.stderr);
    assert!(
        run.stdout.contains("rootcr.p:2:1: error[PARSE001]"),
        "{}\n{}",
        run.stdout,
        run.stderr
    );
    assert!(run.stdout.contains("crtail.i:2:1"), "{}", run.stdout);

    let json = check(tmp.path(), &["--json", "-I", ".", "rootcr.p"]);
    let errors = parse_errors(&json);
    assert_eq!(errors.len(), 1, "{}", json.stdout);
    assert_eq!(errors[0]["path"], "rootcr.p");
    assert_eq!(errors[0]["start"]["line"], 2);
    assert_eq!(errors[0]["source"], "parse");
}

#[test]
fn a_parse_error_in_a_nested_include_is_reported_at_the_root_site() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("crtail.i"), BROKEN);
    write(&tmp.path().join("mid.i"), "MESSAGE \"m\".\n{crtail.i}\n");
    write(&tmp.path().join("rootmid.p"), "MESSAGE \"a\".\n{mid.i}\n");
    let run = check(tmp.path(), &["-I", ".", "rootmid.p"]);
    assert_eq!(run.code, Some(1), "{}\n{}", run.stdout, run.stderr);
    assert!(
        run.stdout.contains("rootmid.p:2:1: error[PARSE001]"),
        "{}",
        run.stdout
    );
    assert!(run.stdout.contains("crtail.i:2:1"), "{}", run.stdout);
}

#[test]
fn the_include_findings_view_places_the_error_in_the_include_file() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("crtail.i"), BROKEN);
    write(&tmp.path().join("rootcr.p"), "{crtail.i}\n");
    let run = check(tmp.path(), &["--include-findings", "-I", ".", "rootcr.p"]);
    assert_eq!(run.code, Some(1), "{}\n{}", run.stdout, run.stderr);
    assert!(
        run.stdout.contains("crtail.i:2:1: error[PARSE001]"),
        "{}",
        run.stdout
    );
}

#[test]
fn an_error_made_by_the_root_arguments_is_reported_at_the_site() {
    let tmp = TempDir::new().unwrap();
    // Parses alone: `mode` is unset, so the branch is not taken.
    write(
        &tmp.path().join("mode.i"),
        "&IF \"{&mode}\" = \"bad\" &THEN\nCREATE BUTTON h TRIGGERS: ON CHOOSE DO:\n&ENDIF\nMESSAGE \"x\".\n",
    );
    write(&tmp.path().join("rootmode.p"), "{mode.i &mode=bad}\n");
    let run = check(tmp.path(), &["-I", ".", "rootmode.p"]);
    assert_eq!(run.code, Some(1), "{}\n{}", run.stdout, run.stderr);
    assert!(
        run.stdout.contains("rootmode.p:1:1: error[PARSE001]"),
        "{}",
        run.stdout
    );
}

#[test]
fn a_clean_include_reports_no_parse_error() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("ok.i"), CLEAN);
    write(&tmp.path().join("rootok.p"), "{ok.i}\n");
    let run = check(tmp.path(), &["--json", "-I", ".", "rootok.p"]);
    assert_eq!(run.code, Some(0), "{}\n{}", run.stdout, run.stderr);
    assert!(parse_errors(&run).is_empty(), "{}", run.stdout);
}

#[test]
fn an_error_in_a_name_value_of_the_root_is_reported_at_the_reference() {
    let tmp = TempDir::new().unwrap();
    write(
        &tmp.path().join("c6.p"),
        "&GLOBAL-DEFINE bad DISPLAY ) .\nMESSAGE \"a\".\n{&bad}\n",
    );
    let run = check(tmp.path(), &["c6.p"]);
    assert_eq!(run.code, Some(1), "{}\n{}", run.stdout, run.stderr);
    assert!(
        run.stdout.contains("c6.p:3:1: error[PARSE001]"),
        "{}",
        run.stdout
    );
    assert!(
        run.stdout
            .contains("(in the value of a preprocessor name or an include argument)"),
        "{}",
        run.stdout
    );
    assert!(!run.stdout.contains("an include)"), "{}", run.stdout);
}

#[test]
fn an_error_in_a_name_value_inside_an_include_is_reported_at_the_root_site() {
    let tmp = TempDir::new().unwrap();
    write(
        &tmp.path().join("c7.i"),
        "&SCOPED-DEFINE bad DISPLAY ) .\nMESSAGE \"a\".\n{&bad}\n",
    );
    write(&tmp.path().join("c7.p"), "MESSAGE \"r\".\n{c7.i}\n");
    let run = check(tmp.path(), &["-I", ".", "c7.p"]);
    assert_eq!(run.code, Some(1), "{}\n{}", run.stdout, run.stderr);
    assert!(
        run.stdout.contains("c7.p:2:1: error[PARSE001]"),
        "{}",
        run.stdout
    );
}
