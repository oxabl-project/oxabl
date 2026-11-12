//! `oxabl check` on sources whose preprocessor expansion grows without bound.
//!
//! A define or an include that expands its predecessor twice doubles at each
//! level, so a file of a few hundred bytes asks for terabytes. The run must end
//! quickly, report `PREPROC011` in the root file, and still lint the text the
//! file holds after the expansion.

use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use serde_json::Value;
use tempfile::TempDir;

/// Without the limit, every bomb here runs for hours or exhausts memory.
const DEADLINE: Duration = Duration::from_secs(60);

/// Declared after the bomb and never read: the LINT0002 that proves the rest of
/// the file was still analysed.
const AFTER: &str = "DEFINE VARIABLE v-after AS INTEGER NO-UNDO.\n";

fn check_json(dir: &Path, file: &str) -> Value {
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["check", "--json", "-I", ".", file])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let started = Instant::now();
    while child.try_wait().unwrap().is_none() {
        if started.elapsed() > DEADLINE {
            child.kill().unwrap();
            panic!("oxabl check {file} did not finish within {DEADLINE:?}");
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let out = child.wait_with_output().unwrap();
    serde_json::from_slice(&out.stdout).unwrap_or_else(|e| {
        panic!(
            "no JSON report ({e}); status {:?}, stderr:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        )
    })
}

fn codes<'a>(json: &'a Value, key: &str) -> Vec<&'a str> {
    json[key]
        .as_array()
        .unwrap_or_else(|| panic!("no `{key}` array in:\n{json}"))
        .iter()
        .map(|d| d["code"].as_str().unwrap())
        .collect()
}

fn assert_limited(json: &Value) {
    let preproc = codes(json, "preproc");
    assert_eq!(
        preproc.iter().filter(|c| **c == "PREPROC011").count(),
        1,
        "the limit is reported once, in the root file, got:\n{json}"
    );
    assert!(
        codes(json, "diagnostics").contains(&"LINT0002"),
        "the text after the expansion is still linted, got:\n{json}"
    );
}

#[test]
fn a_define_chain_that_doubles_stops_at_the_limit() {
    let tmp = TempDir::new().unwrap();
    let mut src = String::from("&SCOPED-DEFINE a0 x\n");
    for i in 0..40 {
        src.push_str(&format!("&SCOPED-DEFINE a{} {{&a{i}}}{{&a{i}}}\n", i + 1));
    }
    src.push_str("{&a40}\n");
    src.push_str(AFTER);
    fs::write(tmp.path().join("macro.p"), src).unwrap();

    assert_limited(&check_json(tmp.path(), "macro.p"));
}

#[test]
fn an_include_chain_that_doubles_stops_at_the_limit() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("a0.i"), "x\n").unwrap();
    for k in 1..=60 {
        let prev = k - 1;
        fs::write(
            tmp.path().join(format!("a{k}.i")),
            format!("{{a{prev}.i}}{{a{prev}.i}}\n"),
        )
        .unwrap();
    }
    fs::write(tmp.path().join("main.p"), format!("{{a60.i}}.\n{AFTER}")).unwrap();

    assert_limited(&check_json(tmp.path(), "main.p"));
}

#[test]
fn empty_includes_repeated_without_end_stop_at_the_limit() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("a0.i"), "").unwrap();
    for k in 1..=60 {
        let prev = k - 1;
        fs::write(
            tmp.path().join(format!("a{k}.i")),
            format!("{{a{prev}.i}}{{a{prev}.i}}\n"),
        )
        .unwrap();
    }
    fs::write(tmp.path().join("main.p"), format!("{{a60.i}}\n{AFTER}")).unwrap();

    assert_limited(&check_json(tmp.path(), "main.p"));
}

#[test]
fn a_large_ordinary_expansion_is_not_limited() {
    // About 8 MB of expanded text over 4096 include expansions and a define
    // chain of 16 levels: large for real code, well inside the limit.
    let tmp = TempDir::new().unwrap();
    let line = format!("/* {} */\n", "c".repeat(2000));
    fs::write(tmp.path().join("body.i"), line).unwrap();
    let mut src = String::from("&SCOPED-DEFINE a0 x\n");
    for i in 0..16 {
        src.push_str(&format!("&SCOPED-DEFINE a{} {{&a{i}}}{{&a{i}}}\n", i + 1));
    }
    src.push_str("DEFINE VARIABLE v-s AS CHARACTER NO-UNDO INITIAL \"{&a16}\".\n");
    src.push_str("MESSAGE v-s.\n");
    for _ in 0..4096 {
        src.push_str("{body.i}\n");
    }
    src.push_str(AFTER);
    fs::write(tmp.path().join("big.p"), src).unwrap();

    let json = check_json(tmp.path(), "big.p");
    assert!(
        !codes(&json, "preproc").contains(&"PREPROC011"),
        "an expansion inside the limit is not reported, got:\n{json}"
    );
    assert!(
        codes(&json, "diagnostics").contains(&"LINT0002"),
        "got:\n{json}"
    );
}
