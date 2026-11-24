//! `oxabl check` on sources whose preprocessor expansion grows without bound.
//!
//! A define or an include that expands its predecessor twice doubles at each
//! level, so a file of a few hundred bytes asks for terabytes. The run must end
//! quickly, report `PREPROC011` in the root file, and still lint the text the
//! file holds after the expansion.

use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

use serde_json::Value;
use tempfile::TempDir;

/// Without the limit, every bomb here runs for hours or exhausts memory.
const DEADLINE: Duration = Duration::from_secs(60);

/// Declared after the bomb and never read: the LINT0002 that proves the rest of
/// the file was still analysed.
const AFTER: &str = "DEFINE VARIABLE v-after AS INTEGER NO-UNDO.\n";

fn check_json(dir: &Path, file: &str) -> Value {
    // Files, not pipes: a report larger than a pipe buffer would block the
    // child while this side waits for it to exit.
    let out_path = dir.join("report.out");
    let err_path = dir.join("report.err");
    let mut child = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .args(["check", "--json", "-I", ".", file])
        .stdout(fs::File::create(&out_path).unwrap())
        .stderr(fs::File::create(&err_path).unwrap())
        .spawn()
        .unwrap();
    let started = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if started.elapsed() > DEADLINE {
            child.kill().unwrap();
            panic!("oxabl check {file} did not finish within {DEADLINE:?}");
        }
        std::thread::sleep(Duration::from_millis(50));
    };
    serde_json::from_slice(&fs::read(&out_path).unwrap()).unwrap_or_else(|e| {
        panic!(
            "no JSON report ({e}); status {status:?}, stderr:\n{}",
            fs::read_to_string(&err_path).unwrap_or_default()
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

#[test]
fn a_nested_argument_that_names_itself_twice_stops_at_the_limit() {
    // `~{1~}` is read where `z.i` uses `{1}`, and names the argument of `z.i`
    // again: each read doubles the text.
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("z.i"), "{1}\n").unwrap();
    fs::write(
        tmp.path().join("main.p"),
        format!("{{z.i ~{{1~}}~{{1~}}}}.\n{AFTER}"),
    )
    .unwrap();

    assert_limited(&check_json(tmp.path(), "main.p"));
}

#[test]
fn a_damaged_argument_used_many_times_is_reported_once() {
    // The include reads `{1}` 300 times; one damaged reference is one finding.
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("rep.i"), "{1}\n".repeat(300)).unwrap();
    for (file, argument, code) in [
        ("open.p", "a~{b", "PREPROC012"),
        ("tilde.p", "a~~", "PREPROC013"),
    ] {
        fs::write(
            tmp.path().join(file),
            format!("{{rep.i {argument}}}\n{AFTER}"),
        )
        .unwrap();
        let json = check_json(tmp.path(), file);
        let found = codes(&json, "preproc");
        assert_eq!(
            found.iter().filter(|c| **c == code).count(),
            1,
            "for {file}, got:\n{json}"
        );
    }
}

#[test]
fn many_damaged_references_are_reported_up_to_a_cap() {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("one.i"), "{1}\n").unwrap();
    let src = "{one.i a~{b}\n".repeat(300) + AFTER;
    fs::write(tmp.path().join("main.p"), src).unwrap();
    let json = check_json(tmp.path(), "main.p");
    let n = codes(&json, "preproc")
        .iter()
        .filter(|c| **c == "PREPROC012")
        .count();
    assert!((1..=100).contains(&n), "{n} reports, got:\n{json}");
}

#[test]
fn deeply_nested_argument_references_are_reported_not_overflowed() {
    // 100000 levels of `~{` would overflow the stack of a recursive reader.
    let tmp = TempDir::new().unwrap();
    let depth = 100_000;
    fs::write(tmp.path().join("p.i"), "MESSAGE \"{1}\".\n").unwrap();
    let src = format!(
        "{{p.i {}&n{}}}\n{AFTER}",
        "~{".repeat(depth),
        "~}".repeat(depth)
    );
    fs::write(tmp.path().join("main.p"), src).unwrap();

    let json = check_json(tmp.path(), "main.p");
    assert!(
        codes(&json, "preproc").contains(&"PREPROC003"),
        "the depth limit is reported, got:\n{json}"
    );
    assert!(
        codes(&json, "diagnostics").contains(&"LINT0002"),
        "the text after the reference is still linted, got:\n{json}"
    );
}
