//! Go to definition at every offset, and hover at every offset that can give
//! another answer, of files full of awkward names, through the real
//! `oxabl lsp` and its daemon: tables and fields that share a name, names
//! that are keywords, hyphens, digits, non-ASCII and very long names, a field
//! the table lacks, a `.df` that breaks off, and fragments opened alone.
//!
//! Every answer must be a well-formed response with no error, and every
//! location must name a file of the workspace and a range inside it. The
//! targets themselves are `definition_lsp.rs`'s business, the hover text
//! `hover_lsp.rs`'s.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const NAMES: &[&str] = &[
    "company",
    "warehouse",
    "cust-num",
    "for",
    "Größe",
    "a#b",
    "pct%",
    "t1",
    "T1",
    "name",
    "nam",
];

fn long_name() -> String {
    format!("l{}", "o".repeat(200))
}

fn schema() -> String {
    let long = long_name();
    let mut df = String::new();
    for table in NAMES.iter().copied().chain([long.as_str()]) {
        df.push_str(&format!("ADD TABLE \"{table}\"\n  AREA \"Data\"\n\n"));
        for field in NAMES.iter().take(4).chain([&table]) {
            df.push_str(&format!(
                "ADD FIELD \"{field}\" OF \"{table}\" AS character\n  FORMAT \"x(8)\"\n\n"
            ));
        }
    }
    df
}

fn uses() -> String {
    let long = long_name();
    let mut text = String::from("/* ü é: two-byte characters before every line's names */\n");
    for (i, n) in NAMES.iter().copied().chain([long.as_str()]).enumerate() {
        text.push_str(&format!(
            "FIND FIRST {n} NO-LOCK NO-ERROR.\n\
             DISPLAY {n}.{n} {n}.company {n}.nosuch.\n\
             DEFINE TEMP-TABLE tt{i} NO-UNDO\n  FIELD {n} LIKE {n}.{n}\n  FIELD f{i} LIKE {n}.nosuch.\n\
             DEFINE BUFFER b{i} FOR {n}.\n\
             DEFINE VARIABLE v{i} LIKE {n}.company NO-UNDO.\n\
             FOR EACH {n} NO-LOCK: DISPLAY b{i}.{n} tt{i}.{n} v{i}. END.\n"
        ));
    }
    text
}

const CALLS: &str = "USING app.*.\nUSING app.Thing.\nUSING app.Missing.\n\
    {frag.i}\n{frag.i &x=1 \"arg\"}\n{missing.i}\n\
    DEFINE VARIABLE o AS CLASS app.Thing NO-UNDO.\n\
    o = NEW app.Thing().\no:Run().\n\
    RUN prog.p.\nRUN prog.\nRUN \"prog.p\".\nRUN twice.p.\nRUN nowhere.p.\nRUN VALUE(\"prog.p\").\n\
    RUN Local (INPUT 1).\n\
    PROCEDURE Local:\n  DEFINE INPUT PARAMETER p AS INTEGER NO-UNDO.\n  MESSAGE p.\nEND PROCEDURE.\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
    files: Vec<(PathBuf, String)>,
}

impl Workspace {
    fn new() -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        // A second `.df` that redefines every table and then breaks off mid-record.
        let mut broken = schema();
        broken.push_str("ADD TABLE \"company\"\nADD FIELD \"company\" OF \"comp");
        let files = vec![
            (
                "oxabl.toml",
                "[workspace]\nname = \"sweep\"\n[workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
                 [workspace.schema]\nfiles = [\"db\"]\n"
                    .to_string(),
            ),
            ("db/a.df", schema()),
            ("db/b.df", broken),
            ("uses.p", uses()),
            ("calls.p", CALLS.to_string()),
            ("frag.i", format!("DEFINE VARIABLE fromFrag AS INTEGER NO-UNDO.\n{}", uses())),
            ("prog.p", "MESSAGE 1.\n".to_string()),
            ("twice.p", "MESSAGE 1.\n".to_string()),
            ("lib/twice.p", "MESSAGE 2.\n".to_string()),
            (
                "app/Thing.cls",
                "CLASS app.Thing:\n  METHOD PUBLIC VOID Run():\n  END METHOD.\nEND CLASS.\n".to_string(),
            ),
        ];
        let files = files
            .into_iter()
            .map(|(name, text)| {
                let path = root.join(name);
                std::fs::create_dir_all(path.parent().unwrap()).unwrap();
                std::fs::write(&path, &text).unwrap();
                (path, text)
            })
            .collect();
        Workspace {
            _dir: dir,
            root,
            files,
        }
    }

    fn text(&self, path: &Path) -> &str {
        &self.files.iter().find(|(p, _)| p == path).unwrap().1
    }
}

/// Each `(line, UTF-16 character)` of `text` with a character there, plus the
/// end of each line.
fn positions(text: &str) -> Vec<(usize, usize)> {
    let mut all = Vec::new();
    for (line, content) in text.split('\n').enumerate() {
        let width = content.encode_utf16().count();
        all.extend((0..=width).map(|character| (line, character)));
    }
    all
}

/// The positions of `text` a hover sweep asks about: each start and end of a
/// run of ASCII name characters, and every other position but those inside a
/// run of spaces. A position inside a name answers as its start does. The
/// server answers one hover at a time, so the sweep sends them one by one.
fn hover_positions(text: &str) -> Vec<(usize, usize)> {
    let word =
        |c: Option<&char>| c.is_some_and(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_');
    let mut kept = Vec::new();
    for (line, content) in text.split('\n').enumerate() {
        let chars: Vec<char> = content.chars().collect();
        let mut utf16 = 0;
        for at in 0..=chars.len() {
            let before = at.checked_sub(1).and_then(|k| chars.get(k));
            let here = chars.get(at);
            let inside_word = word(before) && word(here);
            let inside_space = before == Some(&' ') && here == Some(&' ');
            if !inside_word && !inside_space {
                kept.push((line, utf16));
            }
            if let Some(c) = here {
                utf16 += c.len_utf16();
            }
        }
    }
    kept
}

/// The UTF-16 width of each line of the file `uri` names.
#[track_caller]
fn line_widths(ws: &Workspace, uri: &str) -> Vec<usize> {
    let (_, text) = ws
        .files
        .iter()
        .find(|(path, _)| file_uri(path) == uri)
        .unwrap_or_else(|| panic!("a location in a file outside the workspace: {uri}"));
    text.split('\n').map(|l| l.encode_utf16().count()).collect()
}

#[track_caller]
fn assert_inside(ws: &Workspace, location: &Value, context: &str) {
    let uri = location["uri"].as_str().unwrap();
    let widths = line_widths(ws, uri);
    for end in ["start", "end"] {
        let line = location["range"][end]["line"].as_u64().unwrap() as usize;
        let character = location["range"][end]["character"].as_u64().unwrap() as usize;
        assert!(
            line < widths.len() && character <= widths[line],
            "{context}: a range outside {uri}: {location}"
        );
    }
}

#[test]
fn every_position_gets_a_well_formed_answer() {
    let ws = Workspace::new();
    let mut session = Session::start(&ws.root);
    for name in ["uses.p", "calls.p", "frag.i"] {
        let path = ws.root.join(name);
        let text = ws.text(&path).to_string();
        session.open(&path, &text);
        let positions = positions(&text);
        let params = positions
            .iter()
            .map(|(line, character)| {
                json!({
                    "textDocument": {"uri": file_uri(&path)},
                    "position": {"line": line, "character": character}
                })
            })
            .collect();
        let answers = session.request_all("textDocument/definition", params);
        for ((line, character), answer) in positions.iter().zip(answers) {
            let context = format!("{name}:{line}:{character}");
            assert!(answer.get("error").is_none(), "{context}: {answer}");
            match &answer["result"] {
                Value::Null => {}
                Value::Array(locations) => {
                    assert!(!locations.is_empty(), "{context}: an empty list, not null");
                    for location in locations {
                        assert_inside(&ws, location, &context);
                    }
                }
                other => panic!("{context}: {other}"),
            }
        }
        let hovered = hover_positions(&text);
        let answers: Vec<Value> = hovered
            .iter()
            .map(|(line, character)| {
                session.request(
                    "textDocument/hover",
                    json!({
                        "textDocument": {"uri": file_uri(&path)},
                        "position": {"line": line, "character": character}
                    }),
                )
            })
            .collect();
        for ((line, character), answer) in hovered.iter().zip(answers) {
            let context = format!("{name}:{line}:{character} hover");
            assert!(answer.get("error").is_none(), "{context}: {answer}");
            let result = &answer["result"];
            if result.is_null() {
                continue;
            }
            assert_eq!(result["contents"]["kind"], json!("markdown"), "{context}");
            let value = result["contents"]["value"].as_str().unwrap_or_default();
            assert!(
                !value.trim().is_empty(),
                "{context}: an empty hover, not null"
            );
            if !result["range"].is_null() {
                assert_inside(
                    &ws,
                    &json!({"uri": file_uri(&path), "range": result["range"]}),
                    &context,
                );
            }
        }
        let answer = session.request(
            "textDocument/documentLink",
            json!({"textDocument": {"uri": file_uri(&path)}}),
        );
        assert!(answer.get("error").is_none(), "{name} links: {answer}");
        for link in answer["result"].as_array().unwrap() {
            assert_inside(
                &ws,
                &json!({"uri": file_uri(&path), "range": link["range"]}),
                &format!("{name} links"),
            );
            line_widths(&ws, link["target"].as_str().unwrap());
        }
    }
}
