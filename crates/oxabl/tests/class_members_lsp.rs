//! The types of class members through the real `oxabl lsp` and its daemon:
//! the diagnostics agree with `oxabl check`, and the hover and the definition
//! of a member work at the end of a chain, through `CAST`, `THIS-OBJECT` and
//! `SUPER`. A member of a receiver that does not resolve gives no answer, and
//! a change of a class file on disk reaches an open program.

#![cfg(unix)]

#[path = "class_members_ws/mod.rs"]
mod ws;

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Session, file_uri};
use ws::{BASIC_ROWS, EMPLOYEE, PERSON, Workspace, basic_program, expected, mismatch};

/// The UTF-16 `(line, character)` of byte `at` in `text`.
fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

/// The byte of the `nth` (0-based) occurrence of `needle` in `text`.
#[track_caller]
fn nth(text: &str, needle: &str, nth: usize) -> usize {
    text.match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("no occurrence {nth} of `{needle}`"))
        .0
}

fn position(text: &str, at: usize) -> Value {
    let (line, character) = utf16_at(text, at);
    json!({"line": line, "character": character})
}

/// The answer to `method` with the cursor at byte `at` of `file`.
#[track_caller]
fn ask(session: &mut Session, method: &str, file: &Path, text: &str, at: usize) -> Value {
    let answer = session.request(
        method,
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": position(text, at)
        }),
    );
    assert!(
        answer.get("error").is_none(),
        "`{method}` gave an error: {}",
        answer["error"]
    );
    answer["result"].clone()
}

/// The hover with the cursor one character into occurrence `occurrence` of
/// `needle`: `None` for a `null` result.
#[track_caller]
fn hover(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Option<Value> {
    let at = nth(text, needle, occurrence) + 1;
    match ask(session, "textDocument/hover", file, text, at) {
        Value::Null => None,
        result => Some(result),
    }
}

/// A location: the file's URI and the byte range of text it covers.
#[derive(Debug, PartialEq)]
struct Place {
    uri: String,
    range: Value,
}

/// The place of the word `word` on the first line of `text` that holds `line`.
#[track_caller]
fn word_in_line(file: &Path, text: &str, line: &str, word: &str) -> Place {
    let line_at = nth(text, line, 0);
    let start = line_at
        + line
            .find(word)
            .unwrap_or_else(|| panic!("no `{word}` in `{line}`"));
    Place {
        uri: file_uri(file),
        range: json!({
            "start": position(text, start),
            "end": position(text, start + word.len())
        }),
    }
}

/// The places `textDocument/definition` gives with the cursor at `at`. Accepts
/// a `Location`, or an array of locations or location links; none for `null`
/// or an empty array.
#[track_caller]
fn definition_at(session: &mut Session, file: &Path, text: &str, at: usize) -> Vec<Place> {
    let place = |value: &Value| {
        if let Some(uri) = value["targetUri"].as_str() {
            Place {
                uri: uri.to_string(),
                range: value["targetSelectionRange"].clone(),
            }
        } else {
            Place {
                uri: value["uri"].as_str().expect("a uri").to_string(),
                range: value["range"].clone(),
            }
        }
    };
    match ask(session, "textDocument/definition", file, text, at) {
        Value::Null => Vec::new(),
        Value::Array(places) => places.iter().map(place).collect(),
        other => vec![place(&other)],
    }
}

#[track_caller]
fn definition(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Vec<Place> {
    let at = nth(text, needle, occurrence) + 1;
    definition_at(session, file, text, at)
}

// ---------------------------------------------------------------------------
// K1. The diagnostics match the CLI
// ---------------------------------------------------------------------------

/// The `(line, message)` of the `LINT0004` findings that `oxabl check` gives
/// for `file`, with 0-based lines.
fn cli_lint0004(ws: &Workspace, file: &str) -> Vec<(u64, String)> {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--json", "--no-format", file])
        .current_dir(&ws.root)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let report: Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}"));
    assert!(
        report["failures"].as_array().is_some_and(|f| f.is_empty()),
        "the file was not checked:\n{stdout}"
    );
    let mut found: Vec<_> = report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["code"] == "LINT0004")
        .map(|d| {
            (
                d["start"]["line"].as_u64().unwrap() - 1,
                d["message"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    found.sort();
    found
}

/// The `(line, message)` of the `LINT0004` diagnostics of a publish.
fn lsp_lint0004(diagnostics: &[Value]) -> Vec<(u64, String)> {
    let mut found: Vec<_> = diagnostics
        .iter()
        .filter(|d| d["code"] == "LINT0004")
        .map(|d| {
            (
                d["range"]["start"]["line"].as_u64().unwrap(),
                d["message"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    found.sort();
    found
}

/// The diagnostics of the last publish for `path` that arrives while the
/// server is quiet for `quiet` after the first one.
#[track_caller]
fn settled_diagnostics(session: &Session, path: &Path, quiet: Duration) -> Vec<Value> {
    let uri = json!(file_uri(path));
    let is_publish = |m: &Value| {
        m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == uri
    };
    let mut last = session.wait_for(|m| is_publish(m));
    while let Some(next) = session.try_next(quiet) {
        if is_publish(&next) {
            last = next;
        }
    }
    last["params"]["diagnostics"].as_array().unwrap().clone()
}

#[test]
fn the_diagnostics_of_an_open_program_match_the_cli() {
    let ws = Workspace::new();
    let text = basic_program(BASIC_ROWS);
    ws.write("basic.p", &text);
    let wanted: Vec<(u64, String)> = {
        let mut rows: Vec<_> = expected(BASIC_ROWS)
            .into_iter()
            .map(|(line, message, _)| {
                let at = text.lines().position(|l| l == line).unwrap() as u64;
                (at, message)
            })
            .collect();
        rows.sort();
        rows
    };
    assert_eq!(wanted.len(), 19, "the table of case A holds 19 findings");

    let basic = ws.path("basic.p");
    let mut session = Session::start(&ws.root);
    session.open(&basic, &text);
    let published = settled_diagnostics(&session, &basic, Duration::from_secs(2));
    assert_eq!(
        lsp_lint0004(&published),
        wanted,
        "the LINT0004 diagnostics of the server"
    );
    assert_eq!(
        cli_lint0004(&ws, "basic.p"),
        wanted,
        "the LINT0004 findings of `oxabl check`"
    );
}

// ---------------------------------------------------------------------------
// K2 and K3. Hover and definition at the end of a chain
// ---------------------------------------------------------------------------

const HOVER: &str = r#"DEFINE VARIABLE oP AS app.Person NO-UNDO.
MESSAGE oP:Greet().
MESSAGE oP:Partner():Greet().
MESSAGE oP:Name.
MESSAGE oP:Partner():Name.
MESSAGE app.Person:Create("x"):Name.
MESSAGE CAST(oP, app.Person):Greet().
"#;

const NAME_LINE: &str = "DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.";
const CREATE_LINE: &str = "METHOD PUBLIC STATIC app.Person Create(INPUT pcName AS CHARACTER):";
const GREET_LINE: &str = "METHOD PUBLIC CHARACTER Greet():";

fn open_hover_program() -> (Workspace, Session) {
    let ws = Workspace::new();
    ws.write("hover.p", HOVER);
    let mut session = Session::start(&ws.root);
    session.open(&ws.path("hover.p"), HOVER);
    (ws, session)
}

#[test]
fn a_hover_at_the_end_of_a_chain_equals_the_hover_on_the_plain_member() {
    let (ws, mut session) = open_hover_program();
    let file = ws.path("hover.p");
    for (needle, occurrences) in [(":Greet", [1, 2]), (":Name", [1, 2])] {
        let plain = hover(&mut session, &file, HOVER, needle, 0)
            .unwrap_or_else(|| panic!("no hover on `{needle}` through a variable"));
        for occurrence in occurrences {
            let chained = hover(&mut session, &file, HOVER, needle, occurrence)
                .unwrap_or_else(|| panic!("no hover on `{needle}`, occurrence {occurrence}"));
            assert_eq!(
                chained["contents"], plain["contents"],
                "hover on `{needle}`, occurrence {occurrence}"
            );
        }
    }
}

#[test]
fn a_definition_at_the_end_of_a_chain_is_the_declaration_in_the_class_file() {
    let (ws, mut session) = open_hover_program();
    let file = ws.path("hover.p");
    let person = ws.path("app/Person.cls");

    let name = word_in_line(&person, PERSON, NAME_LINE, "Name");
    for occurrence in 0..3 {
        assert_eq!(
            definition(&mut session, &file, HOVER, ":Name", occurrence),
            vec![Place {
                uri: name.uri.clone(),
                range: name.range.clone()
            }],
            "definition of `:Name`, occurrence {occurrence}"
        );
    }
    assert_eq!(
        definition(&mut session, &file, HOVER, ":Create", 0),
        vec![word_in_line(&person, PERSON, CREATE_LINE, "Create")],
        "definition of `:Create`"
    );
    assert_eq!(
        definition(&mut session, &file, HOVER, ":Greet", 2),
        vec![word_in_line(&person, PERSON, GREET_LINE, "Greet")],
        "definition of `:Greet` after CAST"
    );
}

// ---------------------------------------------------------------------------
// K4. THIS-OBJECT and SUPER in a class
// ---------------------------------------------------------------------------

#[test]
fn this_object_and_super_members_go_to_the_declaring_class() {
    let ws = Workspace::new();
    let employee = ws.path("app/Employee.cls");
    let person = ws.path("app/Person.cls");
    let mut session = Session::start(&ws.root);
    session.open(&employee, EMPLOYEE);

    // (context, word in it, target file and text, declaration line, the word)
    let cases: [(&str, &str, &Path, &str, &str, &str); 4] = [
        (
            "THIS-OBJECT:Salary",
            "Salary",
            &employee,
            EMPLOYEE,
            "DEFINE PUBLIC PROPERTY Salary AS DECIMAL NO-UNDO GET. SET.",
            "Salary",
        ),
        (
            "THIS-OBJECT:Age",
            "Age",
            &person,
            PERSON,
            "DEFINE PUBLIC PROPERTY Age AS INTEGER NO-UNDO GET. SET.",
            "Age",
        ),
        (
            "SUPER:Greet",
            "Greet",
            &person,
            PERSON,
            "METHOD PUBLIC CHARACTER Greet():",
            "Greet",
        ),
        (
            "THIS-OBJECT:Greet",
            "Greet",
            &employee,
            EMPLOYEE,
            "METHOD PUBLIC OVERRIDE CHARACTER Greet():",
            "Greet",
        ),
    ];
    for (context, in_context, target, target_text, line, word) in cases {
        let at = nth(EMPLOYEE, context, 0) + context.rfind(in_context).unwrap() + 1;
        assert_eq!(
            definition_at(&mut session, &employee, EMPLOYEE, at),
            vec![word_in_line(target, target_text, line, word)],
            "definition at `{context}`"
        );
        let answer = ask(&mut session, "textDocument/hover", &employee, EMPLOYEE, at);
        assert!(!answer.is_null(), "no hover at `{context}`");
    }
}

// ---------------------------------------------------------------------------
// K5. No answer for an unresolved receiver
// ---------------------------------------------------------------------------

#[test]
fn a_member_of_an_unresolved_receiver_gives_no_answer() {
    let ws = Workspace::new();
    let text = "DEFINE VARIABLE oX AS app.Missing NO-UNDO.\nMESSAGE oX:Name.\n";
    ws.write("nores.p", text);
    let file = ws.path("nores.p");
    let mut session = Session::start(&ws.root);
    session.open(&file, text);

    assert_eq!(hover(&mut session, &file, text, ":Name", 0), None);
    assert_eq!(definition(&mut session, &file, text, ":Name", 0), vec![]);
}

// ---------------------------------------------------------------------------
// K6. A change on disk reaches the open program
// ---------------------------------------------------------------------------

/// The `LINT0004` list of the next publish for `path` that satisfies `wanted`,
/// within `limit`; fails with the last list seen.
#[track_caller]
fn publish_where(
    session: &Session,
    path: &Path,
    limit: Duration,
    wanted: impl Fn(&[(u64, String)]) -> bool,
) {
    let uri = json!(file_uri(path));
    let deadline = Instant::now() + limit;
    let mut last = None;
    while let Some(left) = deadline.checked_duration_since(Instant::now()) {
        let Some(message) = session.try_next(left) else {
            break;
        };
        if message["method"] == json!("textDocument/publishDiagnostics")
            && message["params"]["uri"] == uri
        {
            let found = lsp_lint0004(message["params"]["diagnostics"].as_array().unwrap());
            if wanted(&found) {
                return;
            }
            last = Some(found);
        }
    }
    panic!(
        "no publish for {path:?} within {limit:?} satisfied the wait; the last LINT0004 list: {last:?}"
    );
}

fn notify_changed(session: &mut Session, path: &Path) {
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(path), "type": 2}]}
    }));
}

#[test]
fn a_change_of_a_class_file_on_disk_reaches_an_open_program() {
    let ws = Workspace::new();
    let text = "DEFINE VARIABLE oP AS app.Person NO-UNDO.\n\
                DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
                i = oP:Name.\n\
                MESSAGE i.\n";
    ws.write("inc.p", text);
    let file = ws.path("inc.p");
    let person = ws.path("app/Person.cls");
    let mut session = Session::start(&ws.root);
    session.open(&file, text);

    let limit = Duration::from_secs(20);
    let finding = vec![(2, mismatch("CHARACTER", "INTEGER"))];
    publish_where(&session, &file, limit, |found| *found == finding);

    let changed = PERSON.replace("Name AS CHARACTER", "Name AS INTEGER");
    assert_ne!(changed, PERSON);
    std::fs::write(&person, &changed).unwrap();
    notify_changed(&mut session, &person);
    publish_where(&session, &file, limit, |found| found.is_empty());

    std::fs::write(&person, PERSON).unwrap();
    notify_changed(&mut session, &person);
    publish_where(&session, &file, limit, |found| *found == finding);
}
