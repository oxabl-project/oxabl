//! End-to-end tests for `[workspace.sources] encoding`: a single-byte source
//! tree is read by `check`, `analyze` and the include path, and `format` writes
//! it back in the encoding it was stored in. All fixtures are synthetic ABL.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use tempfile::TempDir;

fn oxabl() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oxabl"))
}

fn config(tmp: &Path, sources: &str) {
    fs::write(
        tmp.join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\".\"]\n{sources}"
        ),
    )
    .unwrap();
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// Windows-1252 bytes: `é` is 0xE9 and the curly quotes are 0x93 / 0x94.
const LATIN_ROOT: &[u8] =
    b"DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\nMESSAGE \"caf\xe9 \x93x\x94\" v-total.\n";

#[test]
fn a_single_byte_root_is_unreadable_by_default_and_checks_clean_when_configured() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("a.p"), LATIN_ROOT).unwrap();
    config(tmp.path(), "");

    let default = oxabl()
        .args(["check", "--no-format", "."])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert_eq!(default.status.code(), Some(1), "{}", stderr(&default));
    assert!(
        stderr(&default).contains("cannot read"),
        "{}",
        stderr(&default)
    );
    assert!(
        stderr(&default).contains("encoding"),
        "{}",
        stderr(&default)
    );

    config(tmp.path(), "encoding = \"windows-1252\"\n");
    let configured = oxabl()
        .args(["check", "--no-format", "."])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert_eq!(configured.status.code(), Some(0), "{}", stderr(&configured));
}

#[test]
fn a_single_byte_include_resolves_when_configured() {
    let tmp = TempDir::new().unwrap();
    fs::write(
        tmp.path().join("inc.i"),
        b"DEFINE VARIABLE v-inc AS CHARACTER NO-UNDO. /* caf\xe9 */\n",
    )
    .unwrap();
    fs::write(tmp.path().join("root.p"), "{inc.i}\nMESSAGE v-inc.\n").unwrap();
    config(tmp.path(), "");

    let default = oxabl()
        .args(["check", "--no-format", "root.p"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert_eq!(default.status.code(), Some(1), "{}", stderr(&default));

    config(tmp.path(), "encoding = \"iso-8859-1\"\n");
    let configured = oxabl()
        .args(["check", "--no-format", "root.p"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert_eq!(configured.status.code(), Some(0), "{}", stderr(&configured));
}

#[test]
fn a_fallback_encoding_reads_a_tree_that_mixes_utf8_and_single_byte() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("latin.p"), LATIN_ROOT).unwrap();
    fs::write(
        tmp.path().join("utf8.p"),
        "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\nMESSAGE \"caf\u{e9}\" v-total.\n",
    )
    .unwrap();
    config(tmp.path(), "fallback_encoding = \"windows-1252\"\n");

    let out = oxabl()
        .args(["check", "--no-format", "."])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(0), "{}", stderr(&out));
}

#[test]
fn a_utf8_byte_order_mark_is_not_part_of_the_source() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("bom.p"), b"\xef\xbb\xbfMESSAGE \"hi\".\n").unwrap();
    config(tmp.path(), "");

    let out = oxabl()
        .args(["check", "--no-format", "."])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(0), "{}", stderr(&out));
}

#[test]
fn format_writes_the_file_back_in_its_own_encoding() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    fs::write(&file, b"DO:\nMESSAGE \"caf\xe9\".\nEND.\n").unwrap();
    config(tmp.path(), "encoding = \"windows-1252\"\n");

    let out = oxabl()
        .args(["format", "a.p"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(
        fs::read(&file).unwrap(),
        b"DO:\n    MESSAGE \"caf\xe9\".\nEND.\n"
    );
}

#[test]
fn format_keeps_a_byte_order_mark_and_stdout_matches_the_file() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("a.p");
    fs::write(&file, b"\xef\xbb\xbfDO:\nMESSAGE \"x\".\nEND.\n").unwrap();
    config(tmp.path(), "");

    let printed = oxabl()
        .args(["format", "--stdout", "a.p"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(printed.status.success(), "{}", stderr(&printed));
    assert_eq!(
        printed.stdout,
        b"\xef\xbb\xbfDO:\n    MESSAGE \"x\".\nEND.\n"
    );

    let out = oxabl()
        .args(["format", "a.p"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(fs::read(&file).unwrap(), printed.stdout);
}

#[test]
fn analyze_reads_a_single_byte_file_when_configured() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("a.p"), LATIN_ROOT).unwrap();
    config(tmp.path(), "encoding = \"windows-1252\"\n");

    let out = oxabl()
        .args(["analyze", "a.p"])
        .current_dir(tmp.path())
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", stderr(&out));
}
