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
