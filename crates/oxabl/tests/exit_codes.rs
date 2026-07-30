//! The CLI's exit-code contract, pinned per subcommand (R15, R17).
//!
//! Exit codes are the only part of a CLI a script can depend on, and this
//! contract is **not** uniformly 0/1/2: `check`, `conformance`, and `format` are,
//! but `analyze` also uses 3, 4, 6, and 7, and those are as much a contract as
//! the low three. Before this file nothing covered `check`'s codes at all.
//!
//! Every code the subcommands document is either asserted here or recorded as
//! unreachable **with its reason** — see `documented_unreachable_exit_codes` at
//! the bottom. A vacuous test for a code nothing can produce would be worse than
//! the note, because it would look like coverage.
//!
//! Panic paths are driven by `oxabl_common`'s test-only `test-panics` feature
//! (enabled through this crate's dev-dependencies, so it is off in any real
//! build), which makes a guarded site panic when the source carries an
//! `OXABL-TEST-PANIC:<site>` comment marker. No ABL input panics. All fixtures
//! are synthetic ABL.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use oxabl_common::panic_sites;
use tempfile::TempDir;

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// A temp dir holding one file, returned with the dir kept alive.
fn one_file(name: &str, contents: &str) -> (TempDir, PathBuf) {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join(name);
    write(&path, contents);
    (dir, path)
}

/// Run and return `(exit code, stdout, stderr)`.
fn run(args: &[&std::ffi::OsStr]) -> (Option<i32>, String, String) {
    let output = oxabl().args(args).output().unwrap();
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stdout).to_string(),
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

fn code(args: &[&std::ffi::OsStr]) -> Option<i32> {
    run(args).0
}

/// `&OsStr` shorthand, so an argument list can mix literals and paths.
fn s(value: &str) -> &std::ffi::OsStr {
    std::ffi::OsStr::new(value)
}

/// A source with no lint findings and no format drift.
const CLEAN: &str = "MESSAGE \"hello\".\n";
/// A source with a lint finding (unused variable) and no format drift.
const LINT_FINDING: &str = "DEFINE VARIABLE unusedVar AS INTEGER NO-UNDO.\n";
/// A source that is lint-clean but mis-indented, so only the drift channel fires.
const FORMAT_DRIFT: &str = "DO:\nMESSAGE \"x\".\nEND.\n";
/// The `default_base` formatting of [`FORMAT_DRIFT`].
const FORMAT_DRIFT_FIXED: &str = "DO:\n    MESSAGE \"x\".\nEND.\n";
/// A source the parser cannot accept, so the formatter refuses it.
const UNPARSEABLE: &str = "@ @ @\n";

/// Source that panics at `site`, marker inside an ABL block comment.
fn panicking(site: &str) -> String {
    format!("/* OXABL-TEST-PANIC:{site} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n")
}

// ===========================================================================
// check — 0 / 1 / 2, and 1-not-4 on a per-file panic (R15, R24)
// ===========================================================================

#[test]
fn check_exits_0_with_no_findings_and_no_drift() {
    let (_dir, file) = one_file("a.p", CLEAN);
    assert_eq!(code(&[s("check"), file.as_os_str()]), Some(0));
}

#[test]
fn check_exits_1_when_the_lint_channel_has_a_finding() {
    let (_dir, file) = one_file("a.p", LINT_FINDING);
    assert_eq!(code(&[s("check"), file.as_os_str()]), Some(1));
    // …and 0 again once that channel is suppressed, proving the code came from
    // the lint channel rather than from the format one.
    assert_eq!(
        code(&[s("check"), s("--no-lint"), file.as_os_str()]),
        Some(0)
    );
}

#[test]
fn check_exits_1_when_the_format_channel_reports_drift() {
    let (_dir, file) = one_file("a.p", FORMAT_DRIFT);
    assert_eq!(code(&[s("check"), file.as_os_str()]), Some(1));
    assert_eq!(
        code(&[s("check"), s("--no-format"), file.as_os_str()]),
        Some(0),
        "the drift channel alone drove the failure"
    );
}

/// A coverage warning is not a finding: the loud `PREPROC007` must not fail the
/// gate. This is the one case where the exit code deliberately does *not* follow
/// the presence of a diagnostic.
#[test]
fn check_exits_0_when_the_only_diagnostic_is_a_coverage_warning() {
    let (_dir, file) = one_file("a.p", "{missing.i}\nMESSAGE \"hello\".\n");
    let (exit, _stdout, stderr) = run(&[s("check"), s("--preprocess"), file.as_os_str()]);
    assert!(stderr.contains("PREPROC007"), "got: {stderr}");
    assert_eq!(exit, Some(0));
}

#[test]
fn check_exits_2_on_a_nonexistent_path() {
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("nope.p");
    assert_eq!(code(&[s("check"), missing.as_os_str()]), Some(2));
}

#[test]
fn check_exits_2_on_a_directory_with_no_abl_files() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("notes.txt"), "not ABL\n");
    assert_eq!(code(&[s("check"), dir.path().as_os_str()]), Some(2));
}

/// R24, the code this plan **decided** rather than preserved: a contained panic
/// during one file's analysis is a per-file failure, so `check` exits **1**, not
/// `analyze`'s 4. `check` walks a tree; aborting the walk over one file's bug
/// would cost the caller every other file's findings.
#[test]
fn check_exits_1_not_4_when_one_files_analysis_panics() {
    let (_dir, file) = one_file("panics.p", &panicking(panic_sites::ANALYZE));
    let (exit, _stdout, stderr) = run(&[s("check"), file.as_os_str()]);

    assert_eq!(
        exit,
        Some(1),
        "a contained panic is a per-file failure (1), never analyze's 4: {stderr}"
    );
    assert!(
        stderr.contains("analysis failed"),
        "the failure must be reported against the path: {stderr}"
    );
}

/// And the walk really does continue: a panicking file plus a clean one still
/// reports the clean file, and a panicking file plus a *finding* file still
/// reports the finding.
#[test]
fn a_panicking_file_does_not_abort_the_rest_of_the_walk() {
    let dir = TempDir::new().unwrap();
    write(
        &dir.path().join("panics.p"),
        &panicking(panic_sites::ANALYZE),
    );
    write(&dir.path().join("finding.p"), LINT_FINDING);

    let (exit, stdout, stderr) = run(&[s("check"), s("--json"), dir.path().as_os_str()]);
    assert_eq!(exit, Some(1));
    let report: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    assert_eq!(
        report["files_checked"], 2,
        "both files were visited: {stderr}"
    );
    assert_eq!(report["failures"].as_array().map(Vec::len), Some(1));
    assert!(
        report["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .any(|d| d["code"] == "LINT0002"),
        "the healthy file's finding survived: {report}"
    );
}

/// A formatter panic is an oxabl bug rather than a property of the input, so it
/// counts as a failure in `check` — unlike a deliberate bail, which stays neutral.
#[test]
fn check_exits_1_when_formatting_panics_and_0_when_it_merely_bails() {
    let (_dir, panics) = one_file("panics.p", &panicking(panic_sites::FORMAT));
    let (exit, _stdout, stderr) = run(&[s("check"), s("--no-lint"), panics.as_os_str()]);
    assert_eq!(exit, Some(1), "a contained format panic is a failure");
    assert!(stderr.contains("formatting failed"), "got: {stderr}");

    // A refusal on unparseable input is expected behavior, not drift and not a
    // failure. The exit code cannot show that on its own — the only input the
    // formatter reliably bails on is unparseable, and `PARSE001` gates on its own
    // now that `--no-lint` no longer skips the run (A1) — so the assertion is on
    // the two format-channel keys: neither drift nor a failure entry.
    let (_dir2, bails) = one_file("bails.p", UNPARSEABLE);
    let (_exit, stdout, _stderr) = run(&[s("check"), s("--json"), bails.as_os_str()]);
    let report: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    assert_eq!(
        report["format"]["drifted_count"], 0,
        "a deliberate bail is not drift: {report}"
    );
    assert_eq!(
        report["failures"].as_array().map(Vec::len),
        Some(0),
        "and not a per-file failure either: {report}"
    );
}

// ===========================================================================
// conformance — 0 / 1 / 2 (R17)
// ===========================================================================

#[test]
fn conformance_exits_0_when_every_file_parses() {
    let (_dir, file) = one_file("a.p", CLEAN);
    assert_eq!(code(&[s("conformance"), file.as_os_str()]), Some(0));
}

#[test]
fn conformance_exits_1_when_any_file_fails_to_parse() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("ok.p"), CLEAN);
    write(&dir.path().join("bad.p"), UNPARSEABLE);
    assert_eq!(code(&[s("conformance"), dir.path().as_os_str()]), Some(1));
}

#[test]
fn conformance_exits_2_on_a_nonexistent_path() {
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("nope.p");
    assert_eq!(code(&[s("conformance"), missing.as_os_str()]), Some(2));
}

#[test]
fn conformance_exits_2_on_a_directory_with_no_abl_files() {
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("notes.txt"), "not ABL\n");
    assert_eq!(code(&[s("conformance"), dir.path().as_os_str()]), Some(2));
}

/// `--debug` is a dump, not a gate: it exits 0 even over a file that fails to
/// parse, which is the whole reason it lives on this subcommand.
#[test]
fn conformance_debug_exits_0_regardless_of_parse_failures() {
    let (_dir, file) = one_file("bad.p", UNPARSEABLE);
    assert_eq!(
        code(&[s("conformance"), s("--debug"), file.as_os_str()]),
        Some(0)
    );
}

// ===========================================================================
// analyze — 0 / 2 / 4 / 7 asserted; 3 and 6 documented unreachable
// ===========================================================================

/// KTD9: `analyze` is introspection, not a gate — a file full of findings and a
/// clean one are the same success.
#[test]
fn analyze_exits_0_even_with_findings() {
    let (_dir, file) = one_file("a.p", LINT_FINDING);
    let (exit, stdout, _stderr) = run(&[s("analyze"), file.as_os_str()]);
    assert_eq!(exit, Some(0));
    let dump: serde_json::Value = serde_json::from_str(&stdout).unwrap();
    assert!(
        dump["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .any(|d| d["code"] == "LINT0002"),
        "the findings are in the dump, they just do not move the code: {dump}"
    );
}

#[test]
fn analyze_exits_2_when_the_file_cannot_be_read() {
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("nope.p");
    assert_eq!(code(&[s("analyze"), missing.as_os_str()]), Some(2));
}

/// Single-file by design, so a contained panic aborts *here* rather than being
/// reported-and-skipped — there is no rest-of-the-walk to protect.
#[test]
fn analyze_exits_4_on_a_contained_panic() {
    let (_dir, file) = one_file("panics.p", &panicking(panic_sites::ANALYZE));
    let (exit, _stdout, stderr) = run(&[s("analyze"), file.as_os_str()]);
    assert_eq!(exit, Some(4), "got: {stderr}");
    assert!(stderr.contains("analysis failed"), "got: {stderr}");
}

#[test]
fn analyze_exits_7_on_an_unsupported_format() {
    let (_dir, file) = one_file("a.p", CLEAN);
    let (exit, _stdout, stderr) = run(&[s("analyze"), s("--format"), s("yaml"), file.as_os_str()]);
    assert_eq!(exit, Some(7));
    assert!(stderr.contains("unsupported format"), "got: {stderr}");

    // Both supported spellings succeed, so 7 is about the value and not about
    // the flag being present.
    for format in ["json", "text"] {
        assert_eq!(
            code(&[s("analyze"), s("--format"), s(format), file.as_os_str()]),
            Some(0)
        );
    }
}

// ===========================================================================
// format — per-mode (R12)
// ===========================================================================

#[test]
fn format_check_exits_1_if_any_file_would_change_and_writes_nothing() {
    let (_dir, file) = one_file("a.p", FORMAT_DRIFT);
    assert_eq!(
        code(&[s("format"), s("--check"), file.as_os_str()]),
        Some(1)
    );
    assert_eq!(
        fs::read_to_string(&file).unwrap(),
        FORMAT_DRIFT,
        "--check must not write"
    );
}

#[test]
fn format_check_exits_0_when_no_file_would_change() {
    let (_dir, file) = one_file("a.p", FORMAT_DRIFT_FIXED);
    assert_eq!(
        code(&[s("format"), s("--check"), file.as_os_str()]),
        Some(0)
    );
}

/// A refusal is neutral in **every** mode, and only the `Reformatted` arm ever
/// writes — so an unformattable file can never fail a batch reformat, and can
/// never be left half-written.
#[test]
fn a_refusal_is_neutral_in_every_format_mode_and_writes_nothing() {
    for mode in [None, Some("--check"), Some("--stdout")] {
        let (_dir, file) = one_file("bails.p", UNPARSEABLE);
        let mut args = vec![s("format")];
        if let Some(mode) = mode {
            args.push(s(mode));
        }
        args.push(file.as_os_str());
        let (exit, stdout, stderr) = run(&args);

        assert_eq!(
            exit,
            Some(0),
            "a refusal must stay neutral in mode {mode:?}: {stderr}"
        );
        assert_eq!(
            fs::read_to_string(&file).unwrap(),
            UNPARSEABLE,
            "no arm but Reformatted writes (mode {mode:?})"
        );
        if mode == Some("--stdout") {
            assert_eq!(
                stdout, UNPARSEABLE,
                "--stdout must still emit the original bytes"
            );
        }
    }
}

#[test]
fn format_write_mode_exits_0_and_rewrites_only_the_drifting_file() {
    let dir = TempDir::new().unwrap();
    let drifting = dir.path().join("drift.p");
    let conforming = dir.path().join("ok.p");
    write(&drifting, FORMAT_DRIFT);
    write(&conforming, CLEAN);

    assert_eq!(code(&[s("format"), dir.path().as_os_str()]), Some(0));
    assert_eq!(fs::read_to_string(&drifting).unwrap(), FORMAT_DRIFT_FIXED);
    assert_eq!(fs::read_to_string(&conforming).unwrap(), CLEAN);
}

#[test]
fn format_exits_2_on_a_usage_or_config_error() {
    let (_dir, file) = one_file("a.p", CLEAN);
    // An unresolvable `--style` is neither a preset nor a readable file.
    assert_eq!(
        code(&[
            s("format"),
            s("--style"),
            s("no-such-preset"),
            file.as_os_str()
        ]),
        Some(2)
    );

    // `--stdout` is single-file only.
    let dir = TempDir::new().unwrap();
    write(&dir.path().join("a.p"), CLEAN);
    assert_eq!(
        code(&[s("format"), s("--stdout"), dir.path().as_os_str()]),
        Some(2)
    );

    // Nonexistent path and an empty tree are the same usage error.
    let empty = TempDir::new().unwrap();
    assert_eq!(
        code(&[s("format"), empty.path().join("nope.p").as_os_str()]),
        Some(2)
    );
    assert_eq!(code(&[s("format"), empty.path().as_os_str()]), Some(2));
}

// ===========================================================================
// Documented unreachable
// ===========================================================================

/// Two enumerated codes have **no reachable trigger**, and this test records why
/// rather than faking one. A test that fabricated the state would pin the
/// fabrication, not the CLI.
///
/// * **`analyze` exit 3 — fatal preprocessing failure.**
///   `Preprocessor::process` returns `Err` only when it emits an error diagnostic
///   *and* produces an empty span tree. Every loud case in the workspace — an
///   unresolvable include, an unclosed `&IF`, an undefined macro reference —
///   still yields text, so the combination is unreachable from ABL source. Six
///   candidates were probed in an earlier unit and all returned `Ok`. The arm is
///   nonetheless live in the code and is pinned at the pipeline level, from a
///   *fabricated* expansion, by
///   `oxabl_pipeline`'s `fatal_preprocessing_failure_is_computed_without_a_model`
///   — which is the right altitude for it, since only a fabricated value can get
///   there.
///
/// * **Exit 6 — serialize failure** (`check --json`, `conformance --json`,
///   `analyze --format json`, `schema`). Every document is assembled from
///   `serde_json::Value`s and plain derives over `String`/number/bool/array/map;
///   `serde_json` cannot fail on any of those. There is no input, valid or
///   malformed, that reaches the error arm. What *was* worth fixing — a panic on
///   that arm — is fixed: each site returns 6 instead of `expect`ing, which this
///   test pins by construction below.
///
/// The positive half of the 6 story is asserted: the JSON paths that would report
/// it succeed and emit parseable JSON, so the arm is dead because serialization
/// works, not because the flag is broken.
#[test]
fn documented_unreachable_exit_codes() {
    let (_dir, file) = one_file("a.p", LINT_FINDING);

    // `check --json`: emits parseable JSON (exit 1 for the finding, not 6).
    let (exit, stdout, _stderr) = run(&[s("check"), s("--json"), file.as_os_str()]);
    assert_eq!(exit, Some(1));
    serde_json::from_str::<serde_json::Value>(&stdout).expect("check --json serializes");

    // `conformance --json`: same, over a parseable file (exit 0).
    let (exit, stdout, _stderr) = run(&[s("conformance"), s("--json"), file.as_os_str()]);
    assert_eq!(exit, Some(0));
    serde_json::from_str::<serde_json::Value>(&stdout).expect("conformance --json serializes");

    // `analyze --format json`: same (exit 0 whatever it finds).
    let (exit, stdout, _stderr) = run(&[s("analyze"), file.as_os_str()]);
    assert_eq!(exit, Some(0));
    serde_json::from_str::<serde_json::Value>(&stdout).expect("analyze --format json serializes");

    // `schema`: the fourth site that documents 6.
    let (exit, stdout, _stderr) = run(&[s("schema")]);
    assert_eq!(exit, Some(0));
    serde_json::from_str::<serde_json::Value>(&stdout).expect("schema serializes");
}
