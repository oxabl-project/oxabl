//! End-to-end tests for `[workspace.features]` in `oxabl.toml`, as `oxabl check`
//! and `oxabl format` read it.
//!
//! `diagnostics = false` turns the diagnostics channel of `check` off and
//! `format = false` turns the drift channel off, each with a note on stderr.
//! A run with no channel left must not pass. `oxabl format` refuses to run
//! with `format = false`, unless `--style` skips `oxabl.toml`.
//!
//! These drive the *built binary* over `tempfile` dirs. All fixtures are
//! synthetic ABL written for this file.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// Mis-indented *and* holding an unused variable: one lint finding and format
/// drift.
const BOTH: &str = "DO:\nDEFINE VARIABLE v-x AS INTEGER NO-UNDO.\nEND.\n";
/// A hard parse error and nothing else that the formatter can change.
const PARSE_ERROR: &str = "@ @ @\n";

const DIAGNOSTICS_NOTE: &str =
    "note: diagnostics are off in oxabl.toml ([workspace.features]); not checked";
const FORMAT_NOTE: &str =
    "note: formatting is off in oxabl.toml ([workspace.features]); not checked";

struct Run {
    code: Option<i32>,
    stdout: String,
    stderr: String,
}

impl Run {
    fn json(&self) -> Value {
        serde_json::from_str(&self.stdout)
            .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{}", self.stdout))
    }
}

fn run(args: &[&str]) -> Run {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(args)
        .output()
        .unwrap();
    Run {
        code: out.status.code(),
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

/// A project with `source` in `a.p` and an `oxabl.toml` holding `features` as
/// the body of `[workspace.features]`; `None` writes no such table.
fn project(source: &str, features: Option<&str>) -> (TempDir, PathBuf) {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    fs::write(&file, source).unwrap();
    let mut toml = "[workspace]\nname = \"t\"\n".to_string();
    if let Some(features) = features {
        toml.push_str("[workspace.features]\n");
        toml.push_str(features);
    }
    fs::write(tmp.path().join("oxabl.toml"), toml).unwrap();
    (tmp, file)
}

fn path(file: &Path) -> &str {
    file.to_str().unwrap()
}

#[test]
fn by_default_check_reports_both_channels() {
    let (_tmp, file) = project(BOTH, None);

    let run = run(&["check", path(&file)]);

    assert_eq!(run.code, Some(1), "stderr:\n{}", run.stderr);
    assert!(run.stdout.contains("LINT0002"), "stdout:\n{}", run.stdout);
    assert!(
        run.stdout.contains("would be reformatted"),
        "stdout:\n{}",
        run.stdout
    );
    assert!(
        !run.stderr.contains("[workspace.features]"),
        "{}",
        run.stderr
    );
}

#[test]
fn diagnostics_off_reports_drift_only_with_a_note() {
    let (_tmp, file) = project(BOTH, Some("diagnostics = false\n"));

    let run = run(&["check", path(&file)]);

    assert_eq!(run.code, Some(1), "drift still fails the gate");
    assert!(!run.stdout.contains("LINT0002"), "stdout:\n{}", run.stdout);
    assert!(
        run.stdout.contains("would be reformatted"),
        "stdout:\n{}",
        run.stdout
    );
    assert_eq!(
        run.stderr.matches(DIAGNOSTICS_NOTE).count(),
        1,
        "{}",
        run.stderr
    );
    assert!(!run.stderr.contains(FORMAT_NOTE), "{}", run.stderr);
}

#[test]
fn format_off_reports_findings_only_with_a_note() {
    let (_tmp, file) = project(BOTH, Some("format = false\n"));

    let run = run(&["check", path(&file)]);

    assert_eq!(run.code, Some(1));
    assert!(run.stdout.contains("LINT0002"), "stdout:\n{}", run.stdout);
    assert!(
        !run.stdout.contains("reformatted"),
        "stdout:\n{}",
        run.stdout
    );
    assert_eq!(run.stderr.matches(FORMAT_NOTE).count(), 1, "{}", run.stderr);
    assert!(!run.stderr.contains(DIAGNOSTICS_NOTE), "{}", run.stderr);
}

#[test]
fn both_off_is_an_error_not_a_pass() {
    let (_tmp, file) = project(BOTH, Some("diagnostics = false\nformat = false\n"));

    let run = run(&["check", path(&file)]);

    assert_eq!(run.code, Some(2), "stdout:\n{}", run.stdout);
    assert!(
        run.stderr.contains("[workspace.features]"),
        "{}",
        run.stderr
    );
    assert!(run.stderr.contains("nothing to check"), "{}", run.stderr);
}

#[test]
fn diagnostics_off_with_no_format_flag_is_an_error_not_a_pass() {
    let (_tmp, file) = project(BOTH, Some("diagnostics = false\n"));

    let run = run(&["check", "--no-format", path(&file)]);

    assert_eq!(run.code, Some(2), "stdout:\n{}", run.stdout);
    assert!(run.stderr.contains("nothing to check"), "{}", run.stderr);
}

#[test]
fn diagnostics_off_also_silences_parse_errors() {
    let (_tmp, file) = project(PARSE_ERROR, Some("diagnostics = false\n"));

    let run = run(&["check", path(&file)]);

    assert_eq!(run.code, Some(0), "stdout:\n{}", run.stdout);
    assert!(!run.stdout.contains("PARSE"), "stdout:\n{}", run.stdout);
    assert!(run.stderr.contains(DIAGNOSTICS_NOTE), "{}", run.stderr);
}

#[test]
fn json_carries_no_findings_from_a_channel_that_is_off() {
    let (_tmp, file) = project(BOTH, Some("diagnostics = false\n"));
    let report = run(&["check", "--json", path(&file)]);
    assert_eq!(report.code, Some(1));
    let json = report.json();
    assert_eq!(json["lint_enabled"], false, "{json}");
    assert!(json["diagnostics"].as_array().unwrap().is_empty(), "{json}");
    assert_eq!(json["format"]["drifted_count"], 1, "{json}");

    let (_tmp, file) = project(BOTH, Some("format = false\n"));
    let report = run(&["check", "--json", path(&file)]);
    assert_eq!(report.code, Some(1));
    let json = report.json();
    assert_eq!(json["format_enabled"], false, "{json}");
    assert_eq!(json["format"]["drifted_count"], 0, "{json}");
    assert_eq!(json["diagnostics"].as_array().unwrap().len(), 1, "{json}");
}

#[test]
fn updating_a_baseline_needs_diagnostics() {
    let (tmp, file) = project(BOTH, Some("diagnostics = false\n"));
    let baseline = tmp.path().join("baseline.json");

    let run = run(&[
        "check",
        "--baseline",
        baseline.to_str().unwrap(),
        "--update-baseline",
        path(&file),
    ]);

    assert_eq!(run.code, Some(2), "stdout:\n{}", run.stdout);
    assert!(
        run.stderr.contains("[workspace.features]"),
        "{}",
        run.stderr
    );
    assert!(!baseline.exists(), "nothing may be recorded");
}

#[test]
fn an_unknown_features_key_is_a_config_error() {
    let (_tmp, file) = project(BOTH, Some("diagnostic = false\n"));

    let check = run(&["check", path(&file)]);
    assert_eq!(check.code, Some(2), "stdout:\n{}", check.stdout);
    assert!(check.stderr.contains("diagnostic"), "{}", check.stderr);

    let format = run(&["format", "--check", path(&file)]);
    assert_eq!(format.code, Some(2), "stdout:\n{}", format.stdout);
}

#[test]
fn format_refuses_to_run_with_format_off_and_leaves_the_file_alone() {
    let (_tmp, file) = project(BOTH, Some("format = false\n"));

    for args in [
        vec!["format", path(&file)],
        vec!["format", "--check", path(&file)],
        vec!["format", "--stdout", path(&file)],
    ] {
        let run = run(&args);
        assert_eq!(run.code, Some(2), "{args:?} stderr:\n{}", run.stderr);
        assert!(
            run.stderr.contains("[workspace.features] format = false"),
            "{args:?} stderr:\n{}",
            run.stderr
        );
        assert!(run.stdout.is_empty(), "{args:?} stdout:\n{}", run.stdout);
        assert_eq!(fs::read_to_string(&file).unwrap(), BOTH, "{args:?}");
    }
}

#[test]
fn format_with_diagnostics_off_still_formats() {
    let (_tmp, file) = project(BOTH, Some("diagnostics = false\n"));

    let run = run(&["format", path(&file)]);

    assert_eq!(run.code, Some(0), "stderr:\n{}", run.stderr);
    assert_ne!(fs::read_to_string(&file).unwrap(), BOTH);
}

#[test]
fn style_flag_skips_the_format_switch() {
    let (_tmp, file) = project(BOTH, Some("format = false\n"));

    let run = run(&["format", "--style", "oestandards", path(&file)]);

    assert_eq!(run.code, Some(0), "stderr:\n{}", run.stderr);
    assert_ne!(fs::read_to_string(&file).unwrap(), BOTH);
}
