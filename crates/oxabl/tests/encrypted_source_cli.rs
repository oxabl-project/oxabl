//! XCODE-encrypted source through the CLI: `check` reports it once as
//! "encrypted, not checked" with no other finding, `format` never writes to it,
//! and an include that is encrypted gives its includer one message. Fixtures are
//! synthetic: a 10-byte header and random bytes made here.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use serde_json::Value;
use tempfile::TempDir;

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

/// 10 header bytes and 400 pseudo-random bytes.
fn encrypted_bytes() -> Vec<u8> {
    let mut bytes = vec![0x13, 0x59, 0x2f, 0x25, 0x07, 0x09, 0x31, 0x49, 0x3b, 0x25];
    let mut state = 0x9e37_79b9_7f4a_7c15_u64;
    for _ in 0..400 {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        bytes.push((state >> 24) as u8);
    }
    bytes
}

/// 1 KiB of pseudo-random bytes with no header: only its density names it.
fn headerless_bytes() -> Vec<u8> {
    let mut state = 0x2545_f491_4f6c_dd1d_u64;
    let mut bytes: Vec<u8> = (0..1024)
        .map(|_| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state >> 24) as u8
        })
        .collect();
    bytes[0] = b'/';
    bytes
}

fn tree(encoding: &str) -> TempDir {
    let tmp = TempDir::new().unwrap();
    fs::write(
        tmp.path().join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\".\"]\n{encoding}"
        ),
    )
    .unwrap();
    fs::write(tmp.path().join("enc.p"), encrypted_bytes()).unwrap();
    fs::write(tmp.path().join("enc.i"), encrypted_bytes()).unwrap();
    fs::write(tmp.path().join("root.p"), "{enc.i}\nMESSAGE \"x\".\n").unwrap();
    tmp
}

fn run(dir: &Path, args: &[&str]) -> Output {
    oxabl().args(args).current_dir(dir).output().unwrap()
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// Every setting under which the issue's file was once reported as garbage.
const ENCODINGS: [&str; 2] = ["", "encoding = \"windows-1252\"\n"];

#[track_caller]
fn assert_no_garbage_findings(text: &str) {
    for code in ["PARSE", "PREPROC", "LINT", "cannot read", "Invalid"] {
        assert!(!text.contains(code), "unexpected `{code}` in:\n{text}");
    }
}

#[test]
fn check_reports_an_encrypted_root_once_and_nothing_else() {
    for encoding in ENCODINGS {
        let tmp = tree(encoding);
        let out = run(tmp.path(), &["check", "enc.p"]);
        let err = stderr(&out);
        assert_eq!(out.status.code(), Some(0), "{encoding}: {err}");
        assert_eq!(err.matches("SOURCE001").count(), 1, "{encoding}: {err}");
        assert!(err.contains("encrypted, not checked"), "{err}");
        assert_no_garbage_findings(&err);
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert!(stdout.contains("1 encrypted, not checked"), "{stdout}");
    }
}

#[test]
fn check_json_lists_the_encrypted_file_as_a_warning_not_a_failure() {
    for encoding in ENCODINGS {
        let tmp = tree(encoding);
        let out = run(tmp.path(), &["check", "--json", "enc.p"]);
        assert_eq!(out.status.code(), Some(0), "{}", stderr(&out));
        let report: Value = serde_json::from_slice(&out.stdout).unwrap();
        assert_eq!(report["diagnostics"], serde_json::json!([]));
        assert_eq!(report["failures"], serde_json::json!([]));
        assert_eq!(report["encrypted_files"], 1);
        let preproc = report["preproc"].as_array().unwrap();
        assert_eq!(preproc.len(), 1, "{report}");
        assert_eq!(preproc[0]["code"], "SOURCE001");
        assert_eq!(preproc[0]["severity"], "warning");
        assert_eq!(preproc[0]["path"], "enc.p");
    }
}

#[test]
fn check_of_a_walk_reports_each_encrypted_file_once_and_keeps_going() {
    let tmp = tree("");
    fs::write(
        tmp.path().join("plain.p"),
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n",
    )
    .unwrap();
    let out = run(tmp.path(), &["check", "--json", "."]);
    let report: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(report["encrypted_files"], 1, "{report}");
    let codes: Vec<&str> = report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|d| d["code"].as_str())
        .collect();
    assert!(
        codes.contains(&"LINT0002"),
        "the plain file is still checked: {codes:?}"
    );
    assert!(!codes.iter().any(|c| c.starts_with("PARSE")), "{codes:?}");
}

#[test]
fn check_of_an_encrypted_include_file_reports_it_once() {
    let tmp = tree("");
    let out = run(tmp.path(), &["check", "enc.i"]);
    let err = stderr(&out);
    assert_eq!(out.status.code(), Some(0), "{err}");
    assert_eq!(err.matches("SOURCE001").count(), 1, "{err}");
    assert_no_garbage_findings(&err);
}

#[test]
fn an_encrypted_include_gives_its_includer_one_clear_message() {
    for encoding in ENCODINGS {
        let tmp = tree(encoding);
        let out = run(tmp.path(), &["check", "root.p"]);
        let err = stderr(&out);
        assert_eq!(out.status.code(), Some(0), "{encoding}: {err}");
        assert_eq!(err.matches("SOURCE001").count(), 1, "{encoding}: {err}");
        assert!(
            err.contains("root.p:1:1") && err.contains("include 'enc.i' is encrypted, not checked"),
            "{err}"
        );
        assert_no_garbage_findings(&err);
    }
}

#[test]
fn format_never_writes_to_an_encrypted_file() {
    for encoding in ENCODINGS {
        let tmp = tree(encoding);
        let before = fs::read(tmp.path().join("enc.p")).unwrap();
        for args in [
            &["format", "enc.p"][..],
            &["format", "--check", "enc.p"][..],
        ] {
            let out = run(tmp.path(), args);
            let err = stderr(&out);
            assert_eq!(out.status.code(), Some(0), "{args:?}: {err}");
            assert!(err.contains("enc.p: encrypted, not checked"), "{err}");
            assert_no_garbage_findings(&err);
            assert_eq!(
                fs::read(tmp.path().join("enc.p")).unwrap(),
                before,
                "{args:?}"
            );
        }
    }
}

#[test]
fn format_stdout_hands_back_the_encrypted_bytes_unchanged() {
    let tmp = tree("");
    let out = run(tmp.path(), &["format", "--stdout", "enc.p"]);
    assert_eq!(out.status.code(), Some(0), "{}", stderr(&out));
    assert_eq!(out.stdout, encrypted_bytes());
}

#[test]
fn a_command_that_needs_the_text_says_the_file_is_encrypted() {
    let tmp = tree("");
    let out = run(tmp.path(), &["analyze", "enc.p"]);
    let err = stderr(&out);
    assert_eq!(out.status.code(), Some(2), "{err}");
    assert!(
        err.contains("cannot read enc.p: encrypted, not checked"),
        "{err}"
    );
    assert_no_garbage_findings(&err.replace("cannot read", ""));
}

#[test]
fn text_with_a_utf_16_byte_order_mark_is_not_taken_for_encrypted() {
    let tmp = tree("");
    let mut utf16 = vec![0xFF, 0xFE];
    for unit in "MESSAGE 1.\n".encode_utf16() {
        utf16.extend(unit.to_le_bytes());
    }
    fs::write(tmp.path().join("wide.p"), utf16).unwrap();
    let out = run(tmp.path(), &["check", "--no-format", "wide.p"]);
    assert!(!stderr(&out).contains("SOURCE001"), "{}", stderr(&out));
}

#[test]
fn a_file_without_the_header_is_found_by_its_bytes() {
    let tmp = tree("");
    fs::write(tmp.path().join("bare.p"), headerless_bytes()).unwrap();
    let out = run(tmp.path(), &["check", "--json", "bare.p"]);
    let report: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(report["encrypted_files"], 1, "{report}");
    let preproc = report["preproc"].as_array().unwrap();
    assert_eq!(preproc.len(), 1, "every counted file is reported: {report}");
    assert_eq!(preproc[0]["code"], "SOURCE001");
}

/// Readable source with a printer escape in a string is not encrypted: it is
/// checked, and a finding in it still fails the run.
#[test]
fn source_with_a_stray_control_byte_is_still_checked() {
    for encoding in ENCODINGS {
        let tmp = tree(encoding);
        fs::write(
            tmp.path().join("esc.p"),
            b"DEFINE VARIABLE x AS CHARACTER NO-UNDO.\nx = \"\x1bE\".\nPUT CONTROL x.\nDISPLAY y.\n",
        )
        .unwrap();
        let out = run(tmp.path(), &["check", "--json", "--no-format", "esc.p"]);
        let report: Value = serde_json::from_slice(&out.stdout).unwrap();
        assert!(report.get("encrypted_files").is_none(), "{report}");
        assert_eq!(report["files_checked"], 1);
        let codes: Vec<&str> = report["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|d| d["code"].as_str())
            .collect();
        assert!(codes.contains(&"LINT0001"), "{codes:?}");
        assert_eq!(out.status.code(), Some(1), "{}", stderr(&out));
    }
}

#[test]
fn an_encrypted_file_is_not_counted_as_checked() {
    let tmp = tree("");
    fs::write(tmp.path().join("plain.p"), "MESSAGE 1.\n").unwrap();
    let plain = run(tmp.path(), &["check", "--no-format", "plain.p"]);
    assert!(
        String::from_utf8_lossy(&plain.stdout).contains("checked 1 file:"),
        "{}",
        String::from_utf8_lossy(&plain.stdout)
    );
    let only = run(tmp.path(), &["check", "enc.p"]);
    assert!(
        String::from_utf8_lossy(&only.stdout)
            .contains("checked 0 files (1 encrypted, not checked)"),
        "{}",
        String::from_utf8_lossy(&only.stdout)
    );
    let json = run(tmp.path(), &["check", "--json", "enc.p"]);
    let report: Value = serde_json::from_slice(&json.stdout).unwrap();
    assert_eq!(report["files_checked"], 0, "{report}");
}

#[test]
fn fail_on_encrypted_makes_a_skipped_root_fail_the_run() {
    let tmp = tree("");
    let out = run(tmp.path(), &["check", "--fail-on-encrypted", "enc.p"]);
    assert_eq!(out.status.code(), Some(1), "{}", stderr(&out));
    assert!(
        stderr(&out).contains("--fail-on-encrypted"),
        "{}",
        stderr(&out)
    );
    let json = run(
        tmp.path(),
        &["check", "--json", "--fail-on-encrypted", "enc.p"],
    );
    let report: Value = serde_json::from_slice(&json.stdout).unwrap();
    assert_eq!(report["failures"][0]["reason"], "encrypted, not checked");
    assert_eq!(json.status.code(), Some(1));
}

#[test]
fn the_root_warning_does_not_depend_on_no_preprocess() {
    let tmp = tree("");
    let out = run(tmp.path(), &["check", "--no-preprocess", "enc.p"]);
    assert_eq!(
        stderr(&out).matches("SOURCE001").count(),
        1,
        "{}",
        stderr(&out)
    );
}

/// The include may declare any name, so no name is called undefined.
#[test]
fn an_encrypted_include_does_not_make_the_includer_report_undefined_symbols() {
    let tmp = tree("");
    fs::write(tmp.path().join("uses.p"), "{enc.i}\nMESSAGE some-name.\n").unwrap();
    let out = run(tmp.path(), &["check", "--no-format", "uses.p"]);
    let err = stderr(&out);
    let all = format!("{err}{}", String::from_utf8_lossy(&out.stdout));
    assert_eq!(out.status.code(), Some(0), "{all}");
    assert!(err.contains("SOURCE001"), "{all}");
    assert!(!all.contains("LINT0001"), "{all}");
    // Without the encrypted include the same name is undefined.
    fs::write(tmp.path().join("plain-uses.p"), "MESSAGE some-name.\n").unwrap();
    let plain = run(tmp.path(), &["check", "--no-format", "plain-uses.p"]);
    let plain_all = format!(
        "{}{}",
        stderr(&plain),
        String::from_utf8_lossy(&plain.stdout)
    );
    assert!(plain_all.contains("LINT0001"), "{plain_all}");
}
