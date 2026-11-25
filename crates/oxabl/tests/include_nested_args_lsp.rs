//! A `{N}` in the arguments of an include reference inside an include, through
//! the real `oxabl lsp` and `oxabl/expandedUnit`.
//!
//! The compiler replaces the `{N}` with the argument of the including file
//! before it reads the reference, so the nested include gets the value. All
//! fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

/// The expanded text of `main` in a project of `files`, without the header
/// comment and the region markers, and the whole answer.
fn expand(files: &[(&str, &str)]) -> (String, Value) {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    for (name, text) in files {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    let mut session = Session::start(dir.path());
    let main = std::fs::canonicalize(dir.path().join("main.p")).unwrap();
    let answer = answer_for(&mut session, &main);
    let text = answer["text"].as_str().unwrap();
    let lines: Vec<&str> = text.lines().collect();
    let header_end = lines.iter().position(|l| l.ends_with("*/")).unwrap();
    let body = lines[header_end + 1..]
        .iter()
        .filter(|l| !l.starts_with("/* ▼") && !l.starts_with("/* ▲") && !l.starts_with("/* ✖"))
        .map(|l| format!("{l}\n"))
        .collect();
    (body, answer)
}

fn answer_for(session: &mut Session, path: &Path) -> Value {
    let answer = session.request(
        "oxabl/expandedUnit",
        json!({"textDocument": {"uri": file_uri(path)}}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    answer["result"].clone()
}

#[test]
fn a_positional_argument_passes_through_a_nested_reference() {
    let (body, answer) = expand(&[
        ("b.i", "B[{1}]\n"),
        ("pass.i", "{b.i {1}}\n"),
        ("main.p", "{pass.i hello}\n"),
    ]);
    assert_eq!(answer["status"], json!("complete"), "{answer}");
    assert_eq!(body, "B[hello]\n\n\n", "{answer}");
}

#[test]
fn a_positional_argument_passes_through_as_a_named_argument() {
    let (body, answer) = expand(&[
        ("b.i", "B[{&x}]\n"),
        ("pass.i", "{b.i &x={1}}\n"),
        ("main.p", "{pass.i hello}\n"),
    ]);
    assert_eq!(body, "B[hello]\n\n\n", "{answer}");
}

#[test]
fn a_named_argument_of_the_includer_does_not_pass_as_a_name() {
    // `{&x}` in the arguments reads the name the includer got, as before.
    let (body, answer) = expand(&[
        ("b.i", "B[{1}]\n"),
        ("pass.i", "{b.i {&x}}\n"),
        ("main.p", "{pass.i &x=hello}\n"),
    ]);
    assert_eq!(body, "B[hello]\n\n\n", "{answer}");
}

#[test]
fn a_missing_argument_passes_as_nothing() {
    let (body, answer) = expand(&[
        ("b.i", "B[{1}]\n"),
        ("pass.i", "{b.i {2}}\n"),
        ("main.p", "{pass.i hello}\n"),
    ]);
    assert_eq!(body, "B[]\n\n\n", "{answer}");
}

#[test]
fn the_argument_of_a_nested_reference_is_the_one_of_its_own_includer() {
    let (body, answer) = expand(&[
        ("c.i", "C[{1}]\n"),
        ("b.i", "{c.i {1}-b}\n"),
        ("pass.i", "{b.i {1}-pass}\n"),
        ("main.p", "{pass.i hello}\n"),
    ]);
    assert_eq!(body, "C[hello-pass-b]\n\n\n\n", "{answer}");
}

#[test]
fn text_that_is_not_ascii_stays_whole() {
    let (body, answer) = expand(&[
        ("b.i", "B[{1}|{2}]\n"),
        ("pass.i", "{b.i \"é ü\" {1}}\n"),
        ("main.p", "{pass.i hello}\n"),
    ]);
    assert_eq!(body, "B[é ü|hello]\n\n\n", "{answer}");
}

#[test]
fn a_reference_outside_an_include_keeps_its_argument_reference() {
    // The unit has no arguments, so nothing fills the `{1}`.
    let (body, answer) = expand(&[("b.i", "B[{1}]\n"), ("main.p", "{b.i {1}}\n")]);
    assert_eq!(body, "B[{1}]\n\n", "{answer}");
}
