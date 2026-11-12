//! A large or damaged file costs memory and time in line with its size.
//!
//! Each input here is plausible in a real tree: a run of `{` in a damaged file,
//! JSON saved with an ABL extension, many statements on one generated line.
//! Each once took gigabytes or minutes (a JSON file of 940 KB asked for 14.7 GB),
//! because something per finding or per `{` copied or scanned the rest of the
//! line. The tests run `oxabl check` under a memory cap and a time limit that
//! the old behaviour cannot meet.

#![cfg(unix)]

use std::path::Path;
use std::process::{Command, Output};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// Address space for one run, in KiB: far below what the old behaviour asked.
const MEMORY_KIB: u32 = 1 << 20;
/// Seconds for one run of a debug build.
const SECONDS: u32 = 120;

/// `oxabl check <path>` under the memory cap and the time limit, with no core
/// dump. Fails the test unless it ends with exit 1 (findings).
fn check_capped(path: &Path) -> Output {
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!(
            r#"ulimit -c 0 && ulimit -v {MEMORY_KIB} && exec timeout {SECONDS} "$@""#
        ))
        .arg("sh")
        .arg(OXABL_BIN)
        .arg("check")
        .arg(path)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    let tail = &stderr[stderr.floor_char_boundary(stderr.len().saturating_sub(1000))..];
    assert_eq!(
        output.status.code(),
        Some(1),
        "{}: expected findings (exit 1) within {MEMORY_KIB} KiB and {SECONDS} s \
         (124 is the time limit), got {:?}\n{tail}",
        path.display(),
        output.status
    );
    output
}

fn write(dir: &Path, name: &str, text: &str) -> std::path::PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, text).unwrap();
    path
}

/// The longest line `check` prints.
fn longest_line(output: &Output) -> usize {
    [&output.stdout, &output.stderr]
        .iter()
        .flat_map(|bytes| {
            String::from_utf8_lossy(bytes)
                .lines()
                .map(str::len)
                .collect::<Vec<_>>()
        })
        .max()
        .unwrap_or(0)
}

#[test]
fn a_run_of_unclosed_braces() {
    let dir = tempfile::tempdir().unwrap();
    let path = write(
        dir.path(),
        "braces.p",
        &format!("{}\n", "{".repeat(200_000)),
    );
    check_capped(&path);
}

#[test]
fn json_saved_as_abl() {
    let dir = tempfile::tempdir().unwrap();
    let items: Vec<String> = (0..12_000)
        .map(|i| {
            format!(
                r#"{{"id": {i}, "name": "item {i}", "tags": ["a", "b"], "nested": {{"x": {i}}}}}"#
            )
        })
        .collect();
    let path = write(dir.path(), "data.p", &format!("[{}]", items.join(", ")));
    check_capped(&path);
}

/// Each finding shows a window of a long line, not the whole line.
#[test]
fn many_findings_on_one_long_line() {
    let dir = tempfile::tempdir().unwrap();
    let path = write(
        dir.path(),
        "generated.p",
        &format!("{}\n", "a = 1. ".repeat(20_000)),
    );
    let output = check_capped(&path);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("undefined symbol `a`") || stderr.contains("undefined symbol `a`"),
        "expected the undefined-symbol findings"
    );
    let longest = longest_line(&output);
    assert!(
        longest < 400,
        "a snippet line of {longest} bytes: long lines must be cut"
    );
}
