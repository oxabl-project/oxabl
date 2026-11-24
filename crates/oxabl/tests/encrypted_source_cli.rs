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

/// Random bytes without the header are not called encrypted: when in doubt the
/// file is read, and its problems are reported as for any unreadable source.
#[test]
fn a_file_without_the_header_is_read_normally() {
    let tmp = tree("");
    fs::write(tmp.path().join("bare.p"), headerless_bytes()).unwrap();
    let out = run(tmp.path(), &["check", "--json", "bare.p"]);
    assert!(!stderr(&out).contains("SOURCE001"), "{}", stderr(&out));
    let report: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert!(report.get("encrypted_files").is_none(), "{report}");
    assert_eq!(out.status.code(), Some(1), "an unreadable file still fails");
}

/// A short include of raw printer escapes is readable: its names reach the
/// includer, and it is checked as source.
#[test]
fn an_include_of_printer_escapes_is_not_taken_for_encrypted() {
    let tmp = tree("");
    let prn: Vec<u8> = (0..8)
        .flat_map(|i| format!("&GLOBAL-DEFINE P{i} \"\x1b{}\"\n", char::from(65 + i)).into_bytes())
        .collect();
    fs::write(tmp.path().join("prn.i"), prn).unwrap();
    fs::write(
        tmp.path().join("c.p"),
        "{prn.i}\nDEFINE VARIABLE q AS CHARACTER NO-UNDO.\nq = {&P1}.\nMESSAGE q.\n",
    )
    .unwrap();
    for target in ["prn.i", "c.p"] {
        let out = run(tmp.path(), &["check", "--no-format", target]);
        let all = format!("{}{}", stderr(&out), String::from_utf8_lossy(&out.stdout));
        assert!(!all.contains("SOURCE001"), "{target}: {all}");
        assert!(!all.contains("PARSE"), "{target}: {all}");
        assert_eq!(out.status.code(), Some(0), "{target}: {all}");
    }
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

fn strict_walk(roots: &[(&str, &str)], includes: &[&str]) -> TempDir {
    let tmp = TempDir::new().unwrap();
    fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\"src\"]\n\
         [workspace.lint]\nenable = [\"strict\"]\n",
    )
    .unwrap();
    fs::create_dir(tmp.path().join("src")).unwrap();
    for (name, text) in roots {
        fs::write(tmp.path().join("src").join(name), text).unwrap();
    }
    for name in includes {
        fs::write(tmp.path().join("src").join(name), encrypted_bytes()).unwrap();
    }
    tmp
}

/// Encrypted include files of a walk are warned about once each. They are not
/// roots: they are not counted, and they do not fail the run.
#[test]
fn encrypted_include_files_of_a_walk_are_warned_about_and_not_counted() {
    for fail_flag in [false, true] {
        let tmp = strict_walk(&[("r.p", "MESSAGE 1.\n")], &["a.i", "b.i"]);
        let mut args = vec!["check", "--json", "--no-format"];
        if fail_flag {
            args.push("--fail-on-encrypted");
        }
        args.push("src");
        let out = run(tmp.path(), &args);
        assert_eq!(out.status.code(), Some(0), "{}", stderr(&out));
        let report: Value = serde_json::from_slice(&out.stdout).unwrap();
        assert_eq!(report["files_checked"], 1, "{report}");
        assert!(report.get("encrypted_files").is_none(), "{report}");
        let rows = report["preproc"].as_array().unwrap();
        assert_eq!(rows.len(), 2, "{report}");
        assert!(rows.iter().all(|r| r["code"] == "SOURCE001"));
        assert_eq!(report["failures"], serde_json::json!([]));
    }
    let tmp = strict_walk(&[("r.p", "MESSAGE 1.\n")], &["a.i", "b.i"]);
    let out = run(tmp.path(), &["check", "--no-format", "src"]);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("checked 1 file"), "{stdout}");
    assert!(!stdout.contains("encrypted, not checked)"), "{stdout}");
}

/// Roots and include files together: only the roots are counted.
#[test]
fn a_walk_counts_encrypted_roots_apart_from_encrypted_include_files() {
    let tmp = strict_walk(&[("r.p", "MESSAGE 1.\n")], &["a.i"]);
    fs::write(tmp.path().join("src/e.p"), encrypted_bytes()).unwrap();
    let out = run(tmp.path(), &["check", "--json", "--no-format", "src"]);
    let report: Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(report["files_checked"], 1, "{report}");
    assert_eq!(report["encrypted_files"], 1, "{report}");
    assert_eq!(report["preproc"].as_array().unwrap().len(), 2, "{report}");
}

#[test]
fn the_warning_of_an_encrypted_root_shows_no_empty_source_line() {
    let tmp = tree("");
    let err = stderr(&run(tmp.path(), &["check", "enc.p"]));
    assert!(err.contains("enc.p:1:1: warning[SOURCE001]"), "{err}");
    assert!(!err.contains(" | "), "{err}");
}

fn check_text(dir: &Path, file: &str) -> (Option<i32>, String) {
    let out = run(dir, &["check", "--no-format", file]);
    let all = format!("{}{}", stderr(&out), String::from_utf8_lossy(&out.stdout));
    (out.status.code(), all)
}

/// The include may read the variables of its includer and write to them, so
/// nothing is called unused or never read.
#[test]
fn an_encrypted_include_makes_no_unused_or_never_read_claim() {
    let tmp = tree("");
    fs::write(
        tmp.path().join("d.p"),
        "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\nDEFINE VARIABLE w AS CHARACTER NO-UNDO.\n\
         w = \"x\".\n{enc.i}\n",
    )
    .unwrap();
    let (code, all) = check_text(tmp.path(), "d.p");
    assert_eq!(code, Some(0), "{all}");
    assert!(all.contains("SOURCE001"), "{all}");
    for lint in ["LINT0002", "LINT0005", "LINT0006"] {
        assert!(!all.contains(lint), "{lint}: {all}");
    }
    fs::write(
        tmp.path().join("e.p"),
        "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\nw = \"x\".\n",
    )
    .unwrap();
    let (_, all) = check_text(tmp.path(), "e.p");
    assert!(all.contains("LINT0002"), "the control: {all}");
}

/// A name used before the include cannot come from it.
#[test]
fn a_name_used_before_an_encrypted_include_is_still_undefined() {
    let tmp = tree("");
    fs::write(
        tmp.path().join("b.p"),
        "MESSAGE before-name.\n{enc.i}\nMESSAGE after-name.\n",
    )
    .unwrap();
    let (code, all) = check_text(tmp.path(), "b.p");
    assert_eq!(code, Some(1), "{all}");
    assert!(all.contains("before-name"), "{all}");
    assert!(!all.contains("after-name"), "{all}");
}
