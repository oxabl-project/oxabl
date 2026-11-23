//! `oxabl/expandedUnit` through the real `oxabl lsp`: the source oxabl parsed
//! for a compilation unit, with each include delimited, traced back to its
//! physical source, and the expansion's gaps stated.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

const SHARED: &str = "DEFINE VARIABLE used-v AS INTEGER NO-UNDO.\n\
                      DEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n";
const MID: &str = "/* mid */\n{shared.i}\n";
const NESTED: &str = "{mid.i}\nMESSAGE used-v.\n";

fn tree() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let files = [
        ("oxabl.toml", CONFIG),
        ("shared.i", SHARED),
        ("mid.i", MID),
        ("nested.p", NESTED),
        ("missing.p", "{nope.i}\nMESSAGE 1.\n"),
        ("a.i", "/* a */\n{b.i}\n"),
        ("b.i", "/* b */\n{a.i}\n"),
        ("cycle.p", "{a.i}\nMESSAGE 2.\n"),
    ];
    for (name, text) in files {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

fn canonical(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap()
}

#[track_caller]
fn expanded(session: &mut Session, path: &Path) -> Value {
    let answer = session.request(
        "oxabl/expandedUnit",
        json!({"textDocument": {"uri": file_uri(path)}}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    answer["result"].clone()
}

fn text_lines(answer: &Value) -> Vec<String> {
    answer["text"]
        .as_str()
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect()
}

#[track_caller]
fn line_of(answer: &Value, needle: &str) -> usize {
    text_lines(answer)
        .iter()
        .position(|line| line.contains(needle))
        .unwrap_or_else(|| panic!("no line with `{needle}` in:\n{}", answer["text"]))
}

/// The physical `(uri, line)` that output line `line` came from.
fn source_of(answer: &Value, line: usize) -> Option<(String, u64)> {
    let entry = &answer["lines"][line];
    let file = entry[0].as_u64()? as usize;
    Some((
        answer["files"][file].as_str().unwrap().to_string(),
        entry[1].as_u64().unwrap(),
    ))
}

/// The source text: the output without the header comment and the marker lines.
fn body(answer: &Value) -> String {
    let lines = text_lines(answer);
    let header_end = lines.iter().position(|l| l.ends_with("*/")).unwrap();
    lines[header_end + 1..]
        .iter()
        .filter(|l| !l.starts_with("/* ▼") && !l.starts_with("/* ▲") && !l.starts_with("/* ✖"))
        .map(|l| format!("{l}\n"))
        .collect()
}

#[test]
fn a_root_shows_each_include_delimited_and_traced_to_its_source() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let nested = canonical(&dir.path().join("nested.p"));
    let mid = canonical(&dir.path().join("mid.i"));
    let shared = canonical(&dir.path().join("shared.i"));

    let answer = expanded(&mut session, &nested);
    assert_eq!(answer["status"], json!("complete"), "{answer}");
    assert_eq!(answer["mode"], json!("root"));
    assert_eq!(answer["root"], json!(nested));
    assert_eq!(
        body(&answer),
        format!("/* mid */\n{SHARED}\n\nMESSAGE used-v.\n")
    );

    let regions = answer["regions"].as_array().unwrap();
    assert_eq!(regions.len(), 2, "{answer}");
    let (outer, inner) = (&regions[0], &regions[1]);
    assert_eq!(outer["path"], json!(mid));
    assert_eq!(inner["path"], json!(shared));
    assert_eq!(
        (outer["depth"].clone(), inner["depth"].clone()),
        (json!(1), json!(2))
    );
    let span = |r: &Value| {
        (
            r["startLine"].as_u64().unwrap(),
            r["endLine"].as_u64().unwrap(),
        )
    };
    assert!(
        span(outer).0 < span(inner).0 && span(inner).1 < span(outer).1,
        "{regions:#?}"
    );
    // Each marker line sits where the region says.
    let lines = text_lines(&answer);
    assert!(lines[span(inner).0 as usize].starts_with("/* ▼ shared.i"));
    assert!(lines[span(inner).1 as usize].starts_with("/* ▲ shared.i"));
    // The chain: `{mid.i}` on line 1 of nested.p, then `{shared.i}` on line 2 of mid.i.
    assert_eq!(
        inner["chain"],
        json!([
            {"uri": file_uri(&nested), "range": {"start": {"line": 0, "character": 0}, "end": {"line": 0, "character": 7}}},
            {"uri": file_uri(&mid), "range": {"start": {"line": 1, "character": 0}, "end": {"line": 1, "character": 10}}},
        ])
    );

    // Each line goes back to the line that wrote it.
    let spare = line_of(&answer, "DEFINE VARIABLE spare-v");
    assert_eq!(source_of(&answer, spare), Some((file_uri(&shared), 1)));
    let message = line_of(&answer, "MESSAGE used-v.");
    assert_eq!(source_of(&answer, message), Some((file_uri(&nested), 1)));
    assert_eq!(
        source_of(&answer, line_of(&answer, "/* mid */")),
        Some((file_uri(&mid), 0))
    );

    // The unused variable is placed in the expanded text and points at shared.i.
    let finding = answer["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["code"] == json!("LINT0002"))
        .unwrap_or_else(|| panic!("no LINT0002 in {answer}"));
    assert_eq!(
        finding["range"]["start"],
        json!({"line": spare, "character": 16})
    );
    assert_eq!(
        finding["source"],
        json!({"uri": file_uri(&shared), "range": {"start": {"line": 1, "character": 16}, "end": {"line": 1, "character": 23}}})
    );
    let deps: Vec<&str> = answer["dependencies"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d.as_str().unwrap())
        .collect();
    for path in [&nested, &mid, &shared] {
        assert!(deps.contains(&file_uri(path).as_str()), "{deps:?}");
    }
}

#[test]
fn unsaved_text_and_the_selected_context_are_what_is_expanded() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let nested = canonical(&dir.path().join("nested.p"));
    let shared = dir.path().join("shared.i");
    let edited = format!("{SHARED}DEFINE VARIABLE fresh-v AS INTEGER NO-UNDO.\n");
    session.open(&shared, &edited);

    // No context selected: the include alone.
    let alone = expanded(&mut session, &shared);
    assert_eq!(alone["mode"], json!("fragment"));
    assert_eq!(alone["root"], json!(canonical(&shared)));
    assert_eq!(body(&alone), edited);

    let selected = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&shared)}, "root": nested}),
    );
    assert!(selected.get("error").is_none(), "{selected}");
    let answer = expanded(&mut session, &shared);
    assert_eq!(answer["mode"], json!("root"));
    assert_eq!(answer["root"], json!(nested));
    assert_eq!(
        body(&answer),
        format!("/* mid */\n{edited}\n\nMESSAGE used-v.\n")
    );

    // An unsaved root.
    session.open(&nested, "{mid.i}\nMESSAGE used-v spare-v fresh-v.\n");
    let answer = expanded(&mut session, &shared);
    assert!(
        body(&answer).ends_with("MESSAGE used-v spare-v fresh-v.\n"),
        "{answer}"
    );
    assert!(
        !answer["diagnostics"]
            .as_array()
            .unwrap()
            .iter()
            .any(|d| d["code"] == json!("LINT0002")),
        "{answer}"
    );
    assert_eq!(
        std::fs::read_to_string(&shared).unwrap(),
        SHARED,
        "disk is untouched"
    );
}

#[test]
fn a_missing_include_makes_a_reduced_expansion_that_says_so() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let missing = canonical(&dir.path().join("missing.p"));
    let answer = expanded(&mut session, &missing);
    assert_eq!(answer["status"], json!("reduced"), "{answer}");
    let problem = &answer["problems"][0];
    assert_eq!(problem["message"], json!("include `nope.i` was not found"));
    assert_eq!(problem["location"]["uri"], json!(file_uri(&missing)));
    assert_eq!(
        problem["location"]["range"]["start"],
        json!({"line": 0, "character": 0})
    );
    assert!(
        text_lines(&answer).iter().any(|l| l.starts_with("/* ✖")),
        "{answer}"
    );
    assert!(
        answer["text"]
            .as_str()
            .unwrap()
            .contains("Expansion: reduced.")
    );
}

#[test]
fn a_cyclic_include_makes_a_reduced_expansion_that_says_so() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let cycle = canonical(&dir.path().join("cycle.p"));
    let answer = expanded(&mut session, &cycle);
    assert_eq!(answer["status"], json!("reduced"), "{answer}");
    assert!(
        answer["problems"]
            .as_array()
            .unwrap()
            .iter()
            .any(|p| p["message"].as_str().unwrap().contains("PREPROC006")),
        "{answer}"
    );
    assert!(answer["text"].as_str().unwrap().contains("PREPROC006"));
}

#[test]
fn a_root_that_cannot_be_read_fails_and_says_why() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let gone = dir.path().join("gone.p");
    let answer = expanded(&mut session, &gone);
    assert_eq!(answer["status"], json!("failed"), "{answer}");
    assert!(
        answer["problems"][0]["message"]
            .as_str()
            .unwrap()
            .contains("cannot read it"),
        "{answer}"
    );
}
