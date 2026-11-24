//! `oxabl check` and CL004 on `RUN name IN THIS-PROCEDURE`: a super procedure
//! may supply the name, so an optional probe with `NO-ERROR` is not reported.
//!
//! All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

const CONFIG: &str = "[workspace]\nname = \"probe\"\n\n[workspace.sources]\n\
                      include_paths = [\".\"]\n\n[workspace.lint]\nenable = [\"strict.calls\"]\n";

fn tree(program: &str) -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    fs::write(dir.path().join("p.p"), program).unwrap();
    dir
}

/// `(exit, stdout)` of `oxabl check --no-format --json p.p` in `dir`.
fn check(dir: &Path) -> (Option<i32>, Vec<serde_json::Value>) {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["check", "--no-format", "--json", "p.p"])
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let report: serde_json::Value =
        serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("{e}: {stdout}"));
    (
        output.status.code(),
        report["diagnostics"].as_array().unwrap().clone(),
    )
}

fn cl004_lines(found: &[serde_json::Value]) -> Vec<u64> {
    found
        .iter()
        .filter(|d| d["code"] == "CL004")
        .map(|d| d["start"]["line"].as_u64().unwrap())
        .collect()
}

#[test]
fn an_optional_hook_probe_in_this_procedure_is_not_reported() {
    let dir = tree(
        "PROCEDURE get-attribute:\n\
         \x20 DEFINE INPUT PARAMETER pName AS CHARACTER NO-UNDO.\n\
         \x20 RUN optional-hook IN THIS-PROCEDURE (pName) NO-ERROR.\n\
         \x20 IF NOT ERROR-STATUS:ERROR THEN RETURN RETURN-VALUE.\n\
         \x20 RETURN ?.\n\
         END PROCEDURE.\n",
    );
    let (exit, found) = check(dir.path());
    assert_eq!(exit, Some(0), "{found:?}");
    assert!(found.is_empty(), "{found:?}");
}

#[test]
fn a_run_in_this_procedure_with_no_no_error_is_still_reported_with_a_fitting_help() {
    let dir = tree("RUN optional-hook IN THIS-PROCEDURE (\"x\").\n");
    let (exit, found) = check(dir.path());
    assert_eq!(exit, Some(1), "{found:?}");
    assert_eq!(cl004_lines(&found), [1], "{found:?}");
    let help = found[0]["help"].as_str().unwrap();
    assert!(help.contains("NO-ERROR"), "{help}");
    assert!(!help.contains("run it `IN`"), "already in a handle: {help}");
}

#[test]
fn a_missing_procedure_is_still_reported_when_the_run_has_no_handle() {
    let dir = tree(
        "RUN missing-proc NO-ERROR.\n\
         RUN missing-proc.\n\
         RUN other IN THIS-PROCEDURE NO-ERROR.\n",
    );
    let (_, found) = check(dir.path());
    assert_eq!(cl004_lines(&found), [1, 2], "{found:?}");
}

#[test]
fn a_probe_of_a_procedure_the_file_defines_stays_silent() {
    let dir = tree(
        "PROCEDURE hook:\nEND PROCEDURE.\nRUN hook IN THIS-PROCEDURE NO-ERROR.\n\
         RUN hook IN THIS-PROCEDURE.\n",
    );
    let (exit, found) = check(dir.path());
    assert_eq!(exit, Some(0), "{found:?}");
}

#[test]
fn a_run_in_a_member_of_this_procedure_is_not_reported() {
    let dir = tree("RUN hook IN THIS-PROCEDURE:INSTANTIATING-PROCEDURE.\n");
    let (exit, found) = check(dir.path());
    assert_eq!(exit, Some(0), "{found:?}");
}
