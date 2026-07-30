//! End-to-end tests for the `oxabl format` subcommand (write / `--check` /
//! `--stdout`, `--style`, and `oxabl.toml [workspace.style]` discovery).
//!
//! `main.rs`'s helpers are private to the binary target, so these drive the
//! *built binary* via `CARGO_BIN_EXE_oxabl` over `tempfile` dirs. All fixtures
//! are synthetic ABL (CC-1).

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

/// A mis-indented but valid DO block; `default_base` fixes the indentation.
const MIS_INDENTED: &str = "DO:\nMESSAGE \"x\".\nEND.\n";
/// The `default_base` (4-space) formatting of [`MIS_INDENTED`].
const FORMATTED_4: &str = "DO:\n    MESSAGE \"x\".\nEND.\n";

#[test]
fn write_mode_reformats_then_is_idempotent() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    write(&file, MIS_INDENTED);

    // First run rewrites the file, exit 0.
    let out = oxabl().arg("format").arg(&file).output().unwrap();
    assert!(out.status.success(), "expected exit 0 on write");
    assert_eq!(fs::read_to_string(&file).unwrap(), FORMATTED_4);

    // Second run is a no-op (idempotent), exit 0, file unchanged.
    let out2 = oxabl().arg("format").arg(&file).output().unwrap();
    assert!(out2.status.success());
    assert_eq!(fs::read_to_string(&file).unwrap(), FORMATTED_4);
}

#[test]
fn check_nonconforming_exits_1_and_does_not_modify() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--check")
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(1),
        "non-conforming --check → exit 1"
    );
    // File must not be touched, and nothing is printed to stdout.
    assert_eq!(fs::read_to_string(&file).unwrap(), MIS_INDENTED);
    assert!(out.stdout.is_empty(), "--check writes nothing to stdout");
}

#[test]
fn check_conforming_exits_0() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    write(&file, FORMATTED_4);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--check")
        .output()
        .unwrap();
    assert!(out.status.success(), "conforming --check → exit 0");
    assert_eq!(fs::read_to_string(&file).unwrap(), FORMATTED_4);
    assert!(out.stdout.is_empty());
}

#[test]
fn stdout_prints_and_leaves_disk_unchanged() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(String::from_utf8_lossy(&out.stdout), FORMATTED_4);
    // Disk is untouched by --stdout.
    assert_eq!(fs::read_to_string(&file).unwrap(), MIS_INDENTED);
}

#[test]
fn directory_formats_every_abl_file_and_ignores_others() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("a.p"), MIS_INDENTED);
    write(&root.join("sub").join("b.cls"), MIS_INDENTED);
    // A non-ABL file must be left alone.
    let readme = root.join("README.md");
    write(&readme, MIS_INDENTED);

    let out = oxabl().arg("format").arg(root).output().unwrap();
    assert!(out.status.success());
    assert_eq!(fs::read_to_string(root.join("a.p")).unwrap(), FORMATTED_4);
    assert_eq!(
        fs::read_to_string(root.join("sub").join("b.cls")).unwrap(),
        FORMATTED_4
    );
    // README is not an ABL extension → untouched.
    assert_eq!(fs::read_to_string(&readme).unwrap(), MIS_INDENTED);
}

#[test]
fn parse_error_bails_unchanged_and_reports_write_mode() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("bad.p");
    // An unterminated DO block (no matching END) → parse errors → bail.
    let bad = "DO:\n  MESSAGE \"hi\".\n";
    write(&file, bad);

    let out = oxabl().arg("format").arg(&file).output().unwrap();
    // A bail is not a failure in write mode (KTD5).
    assert!(
        out.status.success(),
        "bail is not a hard failure in write mode"
    );
    // File left byte-for-byte unchanged (R7.1b).
    assert_eq!(fs::read_to_string(&file).unwrap(), bad);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("parse errors"),
        "bail reason should be reported to stderr, got:\n{stderr}"
    );
}

/// `--stdout` emits the **original** bytes on both leave-it-alone arms, so a
/// caller piping through `oxabl format --stdout` never loses a file it could not
/// reformat. Only the reformatted arm carries new bytes.
#[test]
fn stdout_emits_original_bytes_on_unchanged_and_on_a_refusal() {
    let tmp = TempDir::new().unwrap();

    // Already conforming: the pipeline answers `Unchanged`, which carries no
    // bytes of its own.
    let good = tmp.path().join("good.p");
    write(&good, FORMATTED_4);
    let out = oxabl()
        .arg("format")
        .arg(&good)
        .arg("--stdout")
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(String::from_utf8_lossy(&out.stdout), FORMATTED_4);

    // A refusal (unterminated DO → parse errors): still the original bytes, and
    // still exit 0 — declining is expected behavior, not a failure.
    let bad_source = "DO:\n  MESSAGE \"hi\".\n";
    let bad = tmp.path().join("bad.p");
    write(&bad, bad_source);
    let out = oxabl()
        .arg("format")
        .arg(&bad)
        .arg("--stdout")
        .output()
        .unwrap();
    assert!(out.status.success(), "a refusal is neutral: {out:?}");
    assert_eq!(String::from_utf8_lossy(&out.stdout), bad_source);
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("parse errors"),
        "the reason still goes to stderr: {out:?}"
    );
    assert_eq!(fs::read_to_string(&bad).unwrap(), bad_source);
}

#[test]
fn batch_continues_past_a_bailing_file() {
    // KTD4 non-abort guarantee: a file that cannot be formatted (here a parse
    // error, sharing the format_one bail/continue path with the lexer-panic
    // guard) must not abort the walk — the valid files are still formatted.
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("a_good.p"), MIS_INDENTED);
    write(&root.join("b_bad.p"), "DO:\n  MESSAGE \"unterminated\".\n");
    write(&root.join("c_good.p"), MIS_INDENTED);

    let out = oxabl().arg("format").arg(root).output().unwrap();
    assert!(out.status.success());
    // Both valid files formatted despite the bad file between/around them.
    assert_eq!(
        fs::read_to_string(root.join("a_good.p")).unwrap(),
        FORMATTED_4
    );
    assert_eq!(
        fs::read_to_string(root.join("c_good.p")).unwrap(),
        FORMATTED_4
    );
    // The bad file is untouched and reported.
    assert_eq!(
        fs::read_to_string(root.join("b_bad.p")).unwrap(),
        "DO:\n  MESSAGE \"unterminated\".\n"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("b_bad.p"), "bailing file must be reported");
}

#[test]
fn check_treats_bail_as_no_change() {
    // A directory with one bailing file and one already-conforming file: --check
    // must exit 0 (the bail counts as "no change", the good file conforms).
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("good.p"), FORMATTED_4);
    write(&root.join("bad.p"), "DO:\n  MESSAGE \"x\".\n");

    let out = oxabl()
        .arg("format")
        .arg(root)
        .arg("--check")
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "bail counts as no-change under --check → exit 0"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("bad.p"), "bail reason still reported");
}

#[test]
fn style_preset_reaches_engine() {
    // oestandards sets end_with_type: a bare END on a PROCEDURE becomes
    // `END PROCEDURE`, proving the preset reached the engine.
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("p.p");
    write(&file, "PROCEDURE foo:\nMESSAGE \"x\".\nEND.\n");

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .arg("--style")
        .arg("oestandards")
        .output()
        .unwrap();
    assert!(out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("END PROCEDURE."),
        "oestandards end_with_type should reach the engine"
    );
}

#[test]
fn style_file_path_loads() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    // A file-based guide overriding indent_size to 2.
    let style = root.join("my-style.toml");
    write(&style, "indent_size = 2\n");
    let file = root.join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .arg("--style")
        .arg(&style)
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "DO:\n  MESSAGE \"x\".\nEND.\n"
    );
}

#[test]
fn bogus_style_is_usage_error_exit_2() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--style")
        .arg("definitely-not-a-preset")
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(2), "unresolvable --style → exit 2");
    // The file is never touched when style resolution fails up front.
    assert_eq!(fs::read_to_string(&file).unwrap(), MIS_INDENTED);
}

#[test]
fn workspace_style_discovered_and_cli_overrides_it() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(
        &root.join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.style]\nindent_size = 2\n",
    );
    let file = root.join("a.p");
    write(&file, MIS_INDENTED);

    // No --style: the discovered table applies (indent 2).
    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .output()
        .unwrap();
    assert!(out.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "DO:\n  MESSAGE \"x\".\nEND.\n",
        "oxabl.toml [workspace.style] indent_size=2 should apply"
    );

    // --style oestandards ignores the table (CLI wins wholesale, indent 4).
    let out2 = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .arg("--style")
        .arg("oestandards")
        .output()
        .unwrap();
    assert!(out2.status.success());
    assert_eq!(
        String::from_utf8_lossy(&out2.stdout),
        FORMATTED_4,
        "CLI --style must override the discovered [workspace.style] table"
    );
}

/// `format` reads no schema and reports no schema problem (D2).
///
/// The formatter takes a style and nothing else, so a `.df` it cannot use is a
/// fact it cannot act on: parsing every configured schema file per invocation
/// bought nothing but a `warning: schema:` line the caller can do nothing about,
/// on the command most likely to be run on save.
#[test]
fn format_does_no_schema_io_and_prints_no_schema_warning() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(
        &root.join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.schema]\nfiles = [\"absent.df\"]\n\
         [workspace.style]\nindent_size = 2\n",
    );
    let file = root.join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert!(
        !stderr.contains("schema"),
        "format must not report a schema problem it cannot act on, got:\n{stderr}"
    );
    // …and the style half of the same config still applies, so the schema-free
    // path is a narrower resolution rather than no resolution.
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "DO:\n  MESSAGE \"x\".\nEND.\n",
        "[workspace.style] must still be honored. stderr:\n{stderr}"
    );
}

/// `--style` short-circuits config discovery entirely (D2), so it works in a tree
/// whose `oxabl.toml` cannot be parsed: the flag names a whole guide, which
/// leaves nothing in the file for the run to need.
#[test]
fn style_flag_works_with_an_unparseable_config_present() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("oxabl.toml"), "this is not valid toml {{{");
    let file = root.join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--stdout")
        .arg("--style")
        .arg("oestandards")
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert!(out.status.success(), "stderr:\n{stderr}");
    assert_eq!(String::from_utf8_lossy(&out.stdout), FORMATTED_4);
    assert!(
        !stderr.contains("oxabl.toml"),
        "a config that is never read cannot be complained about, got:\n{stderr}"
    );
}

#[test]
fn check_and_stdout_conflict_is_usage_error() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    write(&file, MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(&file)
        .arg("--check")
        .arg("--stdout")
        .output()
        .unwrap();
    assert!(
        !out.status.success(),
        "--check and --stdout together must be a clap usage error"
    );
}

#[test]
fn stdout_on_directory_is_usage_error_exit_2() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("a.p"), MIS_INDENTED);

    let out = oxabl()
        .arg("format")
        .arg(root)
        .arg("--stdout")
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(2),
        "--stdout on a directory → usage error exit 2"
    );
    // Nothing written; the file is untouched.
    assert_eq!(fs::read_to_string(root.join("a.p")).unwrap(), MIS_INDENTED);
}

#[test]
fn path_not_found_exits_2() {
    let tmp = TempDir::new().unwrap();
    let missing = tmp.path().join("nope.p");
    let out = oxabl().arg("format").arg(&missing).output().unwrap();
    assert_eq!(out.status.code(), Some(2));
}

#[test]
fn empty_directory_exits_2() {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    // A directory with no ABL files.
    write(&root.join("notes.txt"), "hello\n");
    let out = oxabl().arg("format").arg(root).output().unwrap();
    assert_eq!(out.status.code(), Some(2));
}
