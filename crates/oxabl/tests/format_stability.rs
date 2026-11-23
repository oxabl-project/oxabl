//! `oxabl format --stdout` on inputs where a second pass once changed the
//! output: each case has an exact expected output, and formatting that output
//! again must give the same bytes. The inputs of `fuzz/findings/format` are
//! replayed the same way. All fixtures are synthetic ABL.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::TempDir;

/// Run `oxabl format --stdout` on `bytes`, written to a file named `name`.
fn format_once(dir: &Path, name: &str, bytes: &[u8]) -> Vec<u8> {
    let file = dir.join(name);
    fs::write(&file, bytes).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("format")
        .arg("--stdout")
        .arg(&file)
        .output()
        .unwrap();
    assert!(out.status.success(), "oxabl format failed: {out:?}");
    out.stdout
}

/// `input` formats to `expected`, and formatting `expected` changes nothing.
fn assert_formats(name: &str, input: &[u8], expected: &[u8]) {
    let tmp = TempDir::new().unwrap();
    let once = format_once(tmp.path(), name, input);
    assert_eq!(
        String::from_utf8_lossy(&once),
        String::from_utf8_lossy(expected),
        "first format of {input:?}"
    );
    let twice = format_once(tmp.path(), name, &once);
    assert_eq!(
        String::from_utf8_lossy(&twice),
        String::from_utf8_lossy(&once),
        "second format of {input:?} changed the output"
    );
}

// --- a carriage return that no line feed follows --------------------------

#[test]
fn carriage_returns_before_a_line_feed_end_the_line() {
    assert_formats("only-blanks.p", b"\r\r\n\n\n", b"");
    assert_formats("one-statement.p", b"D\n\r\r\n\n\n", b"D\n");
}

#[test]
fn carriage_returns_in_a_crlf_file_do_not_make_new_lines() {
    assert_formats(
        "crlf.p",
        b"DO:\r\r\n\rMESSAGE 1.\r\n\r\r\nEND.\r\n",
        b"DO:\r\n    MESSAGE 1.\r\nEND.\r\n",
    );
}

#[test]
fn every_format_fuzz_finding_is_stable() {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../fuzz/findings/format");
    let mut inputs: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("{}: {e}", dir.display()))
        .map(|e| e.unwrap().path())
        .collect();
    inputs.sort();
    assert!(!inputs.is_empty(), "no inputs in {}", dir.display());
    let tmp = TempDir::new().unwrap();
    for input in &inputs {
        let name = input.file_name().unwrap().to_string_lossy();
        let once = format_once(tmp.path(), &name, &fs::read(input).unwrap());
        let twice = format_once(tmp.path(), &name, &once);
        assert_eq!(
            String::from_utf8_lossy(&twice),
            String::from_utf8_lossy(&once),
            "{}: the second format changed the output",
            input.display()
        );
    }
}
