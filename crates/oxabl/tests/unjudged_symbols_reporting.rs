//! Coverage reporting for symbols the count-gated lint rules could not fully
//! judge (#128, R16).
//!
//! Statement forms the parser recognizes but does not model credit no reads and
//! no writes, so `unused-variable`, `assigned-but-never-read` and
//! `block-var-used-outside` decline to fire for any symbol one of them names.
//! That is the right call, but silently going blind makes a file look clean when
//! it was only partly checked. These tests pin the honest line — same treatment
//! the `PREPROC007` unresolvable-include warning gets, and the same stream.
//!
//! These drive `analyze`, which is where the note originated. `check` now runs
//! the same rules and carries the same count (R26) — its own coverage of the
//! note lives in `check_cli.rs`, since the two commands render it into different
//! shapes (a scalar in `check`'s report versus a key in the `analyze` envelope)
//! and each shape needs its own pin.

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

/// Run `oxabl analyze` over `source`, returning `(stdout, stderr)`.
fn analyze(source: &str, format: &str) -> (String, String) {
    let tmp = TempDir::new().unwrap();
    let main = tmp.path().join("main.p");
    write(&main, source);
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("analyze")
        .arg("--format")
        .arg(format)
        .arg(&main)
        .output()
        .unwrap();
    assert!(out.status.success(), "analyze failed: {out:?}");
    (
        String::from_utf8(out.stdout).unwrap(),
        String::from_utf8(out.stderr).unwrap(),
    )
}

const ONE_SKIPPED: &str = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
PUT v-total.
";

const NO_SKIPPED: &str = "\
DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
MESSAGE v-total.
";

#[test]
fn a_partly_unjudged_file_says_so() {
    let (_stdout, stderr) = analyze(ONE_SKIPPED, "text");
    assert!(
        stderr.contains("1 symbol could not be fully checked"),
        "expected the coverage note, got stderr: {stderr:?}"
    );
}

/// Silent at zero. A line that always appears is a line users learn to skip,
/// which would defeat the point of printing it at all.
#[test]
fn a_fully_judged_file_says_nothing() {
    let (_stdout, stderr) = analyze(NO_SKIPPED, "text");
    assert!(
        !stderr.contains("could not be fully checked"),
        "expected no coverage note, got stderr: {stderr:?}"
    );
}

/// The note goes to stderr — the same stream as the existing preprocessor
/// coverage warning — so it never contaminates a piped JSON or text dump.
#[test]
fn the_note_goes_to_stderr_not_stdout() {
    let (stdout, stderr) = analyze(ONE_SKIPPED, "json");
    assert!(stderr.contains("could not be fully checked"), "{stderr:?}");
    assert!(
        !stdout.contains("could not be fully checked"),
        "the note must not land in the JSON dump: {stdout:?}"
    );
    // And the dump must still be valid JSON.
    serde_json::from_str::<serde_json::Value>(&stdout).expect("stdout is JSON");
}

/// Machine consumers get a count, not prose to scrape out of stderr.
#[test]
fn json_output_carries_the_count_as_a_field() {
    let (stdout, _stderr) = analyze(ONE_SKIPPED, "json");
    let v: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    assert_eq!(v["unjudged_symbols"], 1, "{stdout}");

    let (stdout, _stderr) = analyze(NO_SKIPPED, "json");
    let v: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    assert_eq!(
        v["unjudged_symbols"], 0,
        "the field is always present; only the prose is conditional"
    );
}
