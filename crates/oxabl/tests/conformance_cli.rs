//! End-to-end tests for the hidden `oxabl conformance` subcommand — the
//! parse-conformance instrument relocated out of `check` in U11 (R17).
//!
//! `main.rs`'s helpers are private to the binary target, so these drive the
//! *built binary* via `CARGO_BIN_EXE_oxabl` over `tempfile` dirs. All fixtures
//! are synthetic ABL (CC-1).
//!
//! Two scenarios the plan lists are deliberately not covered here: a contained
//! lexer panic reported as a panicked file (no ABL input panics today — the
//! `test-panics` marker feature is the only way in, and it is not wired into
//! this walk), and a serialization failure yielding exit 6 (the report is
//! plain integers and strings, so `serde_json` cannot fail on it from the
//! outside). Reaching either from an integration test would mean contorting
//! the design.

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

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

/// Parses cleanly.
const CLEAN: &str = "DEFINE VARIABLE i-count AS INTEGER NO-UNDO.\ni-count = 1.\nDISPLAY i-count.\n";
/// A missing right-hand operand: the parser reports `Unexpected token Then` at 2:8.
const BROKEN: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nIF x > THEN DISPLAY x.\n";

/// A tree with three passing roots and one failing one, plus a `.i` fragment
/// that is deliberately *not* a root (mirrors the pre-move baseline fixture).
fn fixture() -> TempDir {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("clean.p"), CLEAN);
    write(&root.join("broken.p"), BROKEN);
    write(
        &root.join("sub/nested.w"),
        "DEFINE VARIABLE v-name AS CHARACTER NO-UNDO.\nv-name = \"abc\".\n",
    );
    write(
        &root.join("upper.CLS"),
        "CLASS upper:\n  METHOD PUBLIC VOID go():\n  END METHOD.\nEND CLASS.\n",
    );
    write(
        &root.join("frag.i"),
        "DEFINE VARIABLE frag-var AS INTEGER NO-UNDO.\n",
    );
    tmp
}

#[test]
fn directory_walk_reports_counts_failures_and_error_patterns() {
    let tmp = fixture();

    let out = oxabl().arg("conformance").arg(tmp.path()).output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);

    // The discovery line lives on stderr; the report on stdout. Both halves of
    // the human report matter, so the split is asserted, not just the text.
    assert!(
        stderr.contains("Found 4 ABL files"),
        "discovery count belongs on stderr, got:\n{stderr}"
    );
    assert!(
        stdout.contains("Results: 3 passed, 1 failed (75.0% success rate)"),
        "summary line, got:\n{stdout}"
    );
    assert!(stdout.contains("Failures:"), "got:\n{stdout}");
    assert!(
        stdout.contains("broken.p:2:8  Unexpected token Then"),
        "failure entry carries path:line:col + message, got:\n{stdout}"
    );
    assert!(stdout.contains("Top error patterns:"), "got:\n{stdout}");
    assert!(
        stdout.contains("    1  Unexpected token Then"),
        "error-pattern aggregation is right-aligned count + message, got:\n{stdout}"
    );
    assert!(stdout.contains("Total time:"), "got:\n{stdout}");
    // `frag.i` is not a root, so it never appears in the walk.
    assert!(!stdout.contains("frag.i"), "got:\n{stdout}");
}

#[test]
fn json_emits_every_field_of_the_report_shape() {
    let tmp = fixture();

    let out = oxabl()
        .arg("conformance")
        .arg("--json")
        .arg(tmp.path())
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1), "a failing file → exit 1");

    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v["total"], 4);
    assert_eq!(v["passed"], 3);
    assert_eq!(v["failed"], 1);
    assert_eq!(v["io_errors"], 0);
    assert_eq!(v["lexer_panics"], 0);
    assert_eq!(v["success_rate"], 75.0);
    // Timing values vary run to run; assert only that both keys exist.
    assert!(v.get("elapsed_secs").is_some(), "elapsed_secs present");
    assert!(v.get("files_per_sec").is_some(), "files_per_sec present");

    let failures = v["failures"].as_array().unwrap();
    assert_eq!(failures.len(), 1);
    assert!(
        failures[0]["path"].as_str().unwrap().ends_with("broken.p"),
        "got {:?}",
        failures[0]["path"]
    );
    assert_eq!(failures[0]["line"], 2);
    assert_eq!(failures[0]["col"], 8);
    assert_eq!(failures[0]["message"], "Unexpected token Then");

    let patterns = v["error_patterns"].as_array().unwrap();
    assert_eq!(patterns.len(), 1);
    assert_eq!(patterns[0]["pattern"], "Unexpected token Then");
    assert_eq!(patterns[0]["count"], 1);

    // --json suppresses the human discovery line so stdout is the whole report.
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("Found 4 ABL files"),
        "--json is machine-readable-only, got:\n{stderr}"
    );
}

#[test]
fn all_files_passing_exits_0() {
    let tmp = TempDir::new().unwrap();
    write(&tmp.path().join("clean.p"), CLEAN);

    let out = oxabl().arg("conformance").arg(tmp.path()).output().unwrap();
    assert_eq!(out.status.code(), Some(0), "no failures → exit 0");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("Results: 1 passed, 0 failed (100.0% success rate)"),
        "got:\n{stdout}"
    );
    // Nothing failed, so neither optional block is printed.
    assert!(!stdout.contains("Failures:"), "got:\n{stdout}");
    assert!(!stdout.contains("Top error patterns:"), "got:\n{stdout}");
}

#[test]
fn nonexistent_path_exits_2() {
    let tmp = TempDir::new().unwrap();
    let missing = tmp.path().join("nope");

    let out = oxabl().arg("conformance").arg(&missing).output().unwrap();
    assert_eq!(out.status.code(), Some(2), "usage error → exit 2");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("Error: Path does not exist:"),
        "got:\n{stderr}"
    );
}

#[test]
fn empty_directory_exits_2() {
    let tmp = TempDir::new().unwrap();

    let out = oxabl().arg("conformance").arg(tmp.path()).output().unwrap();
    assert_eq!(out.status.code(), Some(2), "nothing to walk → exit 2");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("No ABL files found in"), "got:\n{stderr}");
}

/// The visible CLI is **exactly** `check`, `format`, `lsp`, `schema` (R23), with
/// both instruments — `conformance` and `analyze` — reachable but hidden.
///
/// Asserted as an exact set rather than a list of `contains` checks: the point of
/// R23 is a settled surface, and a test that only checks the four are *present*
/// would pass just as happily if a fifth appeared.
#[test]
fn the_visible_surface_is_exactly_the_four_user_facing_commands() {
    let out = oxabl().arg("--help").output().unwrap();
    assert!(out.status.success());
    let help = String::from_utf8_lossy(&out.stdout);

    // The `Commands:` block, one command name per line, up to the first blank
    // line — clap lists options after it.
    let listed: Vec<&str> = help
        .lines()
        .skip_while(|l| !l.starts_with("Commands:"))
        .skip(1)
        .take_while(|l| !l.trim().is_empty())
        .filter_map(|l| l.split_whitespace().next())
        .collect();

    assert_eq!(
        listed,
        vec!["check", "format", "lsp", "schema", "help"],
        "the advertised surface must not grow, got:\n{help}"
    );
    for hidden in ["conformance", "analyze"] {
        assert!(
            !help.contains(hidden),
            "`{hidden}` is an instrument, not part of the advertised surface, got:\n{help}"
        );
    }
}

#[test]
fn conformance_is_hidden_from_help_but_reachable() {
    // Hidden, not gone: its own --help works.
    let sub = oxabl().arg("conformance").arg("--help").output().unwrap();
    assert!(sub.status.success(), "`conformance --help` must work");
    let sub_help = String::from_utf8_lossy(&sub.stdout);
    assert!(sub_help.contains("--preprocess"), "got:\n{sub_help}");
    assert!(sub_help.contains("--debug"), "got:\n{sub_help}");
}

#[test]
fn parse_error_inside_an_include_is_prefixed_and_positioned() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    // The root file is fine; the include it expands is not. The error therefore
    // resolves to a different FileId and is reported against the *expanded*
    // source with an `[in include]` prefix.
    write(&root.join("main.p"), "{broken.i}\n");
    write(&root.join("broken.i"), BROKEN);

    let out = oxabl()
        .arg("conformance")
        .arg("--json")
        .arg("--preprocess")
        .arg("-I")
        .arg(root)
        .arg(root.join("main.p"))
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));

    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let failures = v["failures"].as_array().unwrap();
    assert_eq!(failures.len(), 1);
    assert_eq!(
        failures[0]["message"], "[in include] Unexpected token Then",
        "an include-origin error keeps its prefix"
    );
    // Positioned in the expanded text, where the include body starts at line 1.
    assert_eq!(failures[0]["line"], 2);
    assert_eq!(failures[0]["col"], 8);
    assert!(
        failures[0]["path"].as_str().unwrap().ends_with("main.p"),
        "reported against the root file, got {:?}",
        failures[0]["path"]
    );
}

#[test]
fn debug_dumps_ast_context_and_exits_0_despite_failures() {
    let tmp = fixture();

    let out = oxabl()
        .arg("conformance")
        .arg("--debug")
        .arg(tmp.path())
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(0),
        "--debug is an inspection mode, not a gate"
    );

    let stdout = String::from_utf8_lossy(&out.stdout);
    // Passing files report OK; the failing one dumps statements + error context.
    assert!(stdout.contains("OK: "), "got:\n{stdout}");
    assert!(
        stdout.contains("statement(s) before error"),
        "AST context dump, got:\n{stdout}"
    );
    assert!(stdout.contains("error(s) ---"), "got:\n{stdout}");
    assert!(stdout.contains("Unexpected token Then"), "got:\n{stdout}");
    assert!(
        stdout.contains(">>>"),
        "source context marks the error line, got:\n{stdout}"
    );
}

#[test]
fn check_no_longer_accepts_debug() {
    let tmp = fixture();

    // `--debug` moved to `conformance` with the rest of the instrument, so
    // `check` rejects it as an unknown flag (clap usage error, exit 2).
    let out = oxabl()
        .arg("check")
        .arg("--debug")
        .arg(tmp.path())
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(2),
        "unknown flag → clap usage error"
    );
}
