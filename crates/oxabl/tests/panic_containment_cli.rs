//! A panic in the shared pipelines must not crash a CLI subcommand.
//!
//! `analyze` had no guard at all before this: a panic reached the runtime's
//! default handler and the user saw a Rust backtrace instead of a diagnostic.
//! `format` and `parse` had guards but rolled their own; these tests pin that
//! their recovery behavior survived the migration onto the shared guard.
//!
//! No ABL input panics, so every panic here is injected: `oxabl_common`'s
//! test-only `test-panics` feature (enabled through this crate's
//! dev-dependencies, so it is off in any real build) makes a guarded site panic
//! when the source carries `OXABL-TEST-PANIC:<site>` in a comment. All fixtures
//! are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::Command;

use oxabl_common::panic_sites;
use tempfile::TempDir;

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

fn write(path: &Path, contents: &str) {
    fs::write(path, contents).unwrap();
}

/// Source that panics at `site`, marker inside an ABL block comment.
fn panicking_source(site: &str) -> String {
    format!("/* OXABL-TEST-PANIC:{site} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n")
}

/// A panic during `analyze` is reported as a failure for that file, with a
/// readable message and a non-zero exit code — not an unwind out of the
/// subcommand.
#[test]
fn analyze_reports_a_panic_instead_of_crashing() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("panics.p");
    write(&file, &panicking_source(panic_sites::ANALYZE));

    let output = oxabl().arg("analyze").arg(&file).output().unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success(),
        "a failed analysis must not report success"
    );
    assert!(
        stderr.contains("analysis failed"),
        "expected a readable failure report, got: {stderr}"
    );
    assert!(
        stderr.contains("internal error"),
        "expected the panic message to be surfaced (R2), got: {stderr}"
    );
    // The exit code is the analyze failure code, not a panic-abort — the guard
    // contained the unwind and `main` returned normally.
    assert_eq!(
        output.status.code(),
        Some(4),
        "expected the guarded analyze failure code, got {:?}",
        output.status.code()
    );
    // The default panic hook still writes its own line before the guard reports.
    // That is deliberate: an `InternalPanic` is an oxabl bug, and the hook's
    // location plus `RUST_BACKTRACE` is exactly what a bug report needs. The
    // guard is what keeps the process from dying, not what silences the hook.
}

/// The same file analyzes fine without the marker, so the test above is measuring
/// the injected panic rather than some unrelated failure.
#[test]
fn analyze_succeeds_without_the_marker() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("fine.p");
    write(&file, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");

    let output = oxabl().arg("analyze").arg(&file).output().unwrap();
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// `format` treats a panic as a bail: the file is reported and its bytes are
/// left exactly as they were. Other files in the same walk are still formatted,
/// so one pathological file cannot abort the run.
#[test]
fn format_leaves_a_panicking_file_untouched_and_finishes_the_walk() {
    let tmp = TempDir::new().unwrap();
    let panicking = tmp.path().join("panics.p");
    let reformattable = tmp.path().join("messy.p");

    let panicking_before = panicking_source(panic_sites::FORMAT);
    write(&panicking, &panicking_before);
    // Mis-indented, so a completed walk visibly rewrites it.
    write(&reformattable, "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n");

    let output = oxabl().arg("format").arg(tmp.path()).output().unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(
        fs::read_to_string(&panicking).unwrap(),
        panicking_before,
        "a panicking file must be left byte-identical"
    );
    assert!(
        fs::read_to_string(&reformattable)
            .unwrap()
            .contains("    MESSAGE"),
        "the walk must continue past the panicking file and reformat the rest"
    );
    assert!(
        stdout.contains("internal panic while formatting") || stderr.contains("internal panic"),
        "the panic must be reported. stdout: {stdout}\nstderr: {stderr}"
    );
}
