//! Where an include reference ends, and how a `~` in its arguments reads, end
//! to end through `oxabl analyze --preprocess`.
//!
//! As in the compiler (`COMPILE ... PREPROCESS`, OpenEdge 12.8.9), the first
//! `}` ends the reference, also inside quotes, so the text after it is code.
//! `~}` does not end it. A `~` in an argument is removed and does not escape
//! the quote after it. A `~{` in an argument opens a nested reference; one that
//! is never closed raises `PREPROC012`.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

/// Runs argument 1. Argument 2 goes into a comment that the text after the
/// reference closes.
const RUNNER: &str = "RUN {1}. /* {2}";

/// Runs argument 1 and shows argument 2.
const MESSAGE: &str = "RUN {1}.\nMESSAGE \"{2}\".\n";

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, contents).unwrap();
}

/// `oxabl <args> -I <root>/inc main.p` over `source`, as JSON.
fn run(source: &str, args: &[&str]) -> serde_json::Value {
    let tmp = TempDir::new().unwrap();
    let root = tmp.path();
    write(&root.join("inc/r.i"), RUNNER);
    write(&root.join("inc/m.i"), MESSAGE);
    write(&root.join("main.p"), source);
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(root)
        .args(args)
        .args(["-I", "inc", "main.p"])
        .output()
        .unwrap();
    assert!(out.status.success(), "{out:?}");
    serde_json::from_slice(&out.stdout).expect("valid JSON")
}

/// `analyze --preprocess` of `source`, as JSON.
fn analyze(source: &str) -> serde_json::Value {
    run(source, &["analyze", "--format", "json", "--preprocess"])
}

/// The codes of the preprocessor findings that `check` reports for `source`.
fn check_preproc_codes(source: &str) -> Vec<String> {
    let v = run(source, &["check", "--json"]);
    v["preproc"]
        .as_array()
        .unwrap_or_else(|| panic!("no preproc array in {v:#}"))
        .iter()
        .map(|d| d["code"].as_str().unwrap().to_string())
        .collect()
}

fn programs(v: &serde_json::Value) -> Vec<String> {
    v["dependencies"]["unresolved"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|row| row["via"] == "program")
        .map(|row| row["name"].as_str().unwrap().to_string())
        .collect()
}

fn preprocess_and_parse_codes(v: &serde_json::Value) -> Vec<String> {
    v["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].as_str().unwrap_or_default().to_string())
        .filter(|c| c.starts_with("PARSE") || c.starts_with("PREPROC"))
        .collect()
}

#[test]
fn a_quoted_brace_ends_the_reference_and_the_rest_is_code() {
    let v = analyze("{r.i one.p '}' */\nRUN two.p. /* } */\n");
    assert_eq!(programs(&v), ["one.p", "two.p"], "{v:#}");
    assert_eq!(preprocess_and_parse_codes(&v), Vec::<String>::new());
}

#[test]
fn a_tilde_before_a_quote_in_an_argument_is_removed() {
    let v = analyze("{r.i \"three~\".p\"} */\n");
    assert_eq!(programs(&v), ["three.p"], "{v:#}");
    assert_eq!(preprocess_and_parse_codes(&v), Vec::<String>::new());
}

#[test]
fn an_escaped_closing_brace_does_not_end_the_reference() {
    let v = analyze("{r.i \"four~}.p\" x} */\nRUN five.p.\n");
    assert_eq!(programs(&v), ["five.p", "four}.p"], "{v:#}");
    assert_eq!(preprocess_and_parse_codes(&v), Vec::<String>::new());
}

#[test]
fn an_escaped_open_brace_opens_a_nested_reference() {
    let v = analyze("&SCOPED-DEFINE p seven\n{r.i \"~{&p~}.p\"} */\n");
    assert_eq!(programs(&v), ["seven.p"], "{v:#}");
    assert_eq!(preprocess_and_parse_codes(&v), Vec::<String>::new());
}

#[test]
fn an_unclosed_escaped_open_brace_in_an_argument_is_reported() {
    assert_eq!(check_preproc_codes("{m.i six.p a~{b}\n"), ["PREPROC012"]);
    assert_eq!(
        check_preproc_codes("{m.i six.p a~}b}\n"),
        Vec::<String>::new()
    );
}
