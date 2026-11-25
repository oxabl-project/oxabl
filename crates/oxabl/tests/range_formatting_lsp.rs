//! `textDocument/rangeFormatting` and `textDocument/rangesFormatting` through
//! the real `oxabl lsp` over stdio. All ABL fixtures are synthetic.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

/// A procedure with every line flush left.
const FLAT: &str = "\
PROCEDURE greet:
DEFINE VARIABLE n AS INTEGER NO-UNDO.
IF n > 0 THEN DO:
MESSAGE \"positive 😀\".
MESSAGE
\"wrapped\".
END.
MESSAGE \"after\".
END PROCEDURE.
MESSAGE \"top\".
";

/// The edits and notifications one request produced.
struct Answered {
    edits: Vec<Value>,
    shown: Vec<String>,
}

#[track_caller]
fn ask(session: &mut Session, method: &str, params: Value) -> Answered {
    let id = session.next_request_id();
    session.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
    let mut shown = Vec::new();
    loop {
        let message = session.wait_for(|_| true);
        if message["method"] == json!("window/showMessage") {
            shown.push(message["params"]["message"].as_str().unwrap().to_string());
        }
        if message["id"] == json!(id) && message.get("method").is_none() {
            assert!(
                message.get("error").is_none(),
                "`{method}` gave an error: {message}"
            );
            let edits = match &message["result"] {
                Value::Null => Vec::new(),
                Value::Array(edits) => edits.clone(),
                other => panic!("expected edits, got {other}"),
            };
            return Answered { edits, shown };
        }
    }
}

fn range(from: (u32, u32), to: (u32, u32)) -> Value {
    json!({
        "start": {"line": from.0, "character": from.1},
        "end": {"line": to.0, "character": to.1}
    })
}

fn options() -> Value {
    json!({"tabSize": 2, "insertSpaces": false})
}

#[track_caller]
fn format_range(session: &mut Session, path: &Path, range: Value) -> Answered {
    ask(
        session,
        "textDocument/rangeFormatting",
        json!({"textDocument": {"uri": file_uri(path)}, "range": range, "options": options()}),
    )
}

/// The byte offset of an LSP position (UTF-16 columns) in `text`.
fn offset(text: &str, line: u64, character: u64) -> usize {
    let mut start = 0;
    for _ in 0..line {
        start += text[start..].find('\n').expect("a line") + 1;
    }
    let mut units = 0;
    for (at, ch) in text[start..].char_indices() {
        if units >= character {
            return start + at;
        }
        units += ch.len_utf16() as u64;
    }
    text.len()
}

fn apply(text: &str, edits: &[Value]) -> String {
    let mut spans: Vec<(usize, usize, &str)> = edits
        .iter()
        .map(|edit| {
            let at = |p: &Value| {
                offset(
                    text,
                    p["line"].as_u64().unwrap(),
                    p["character"].as_u64().unwrap(),
                )
            };
            (
                at(&edit["range"]["start"]),
                at(&edit["range"]["end"]),
                edit["newText"].as_str().unwrap(),
            )
        })
        .collect();
    spans.sort_by_key(|span| span.0);
    let mut out = String::new();
    let mut at = 0;
    for (start, end, new_text) in spans {
        assert!(at <= start, "edits overlap: {edits:?}");
        out.push_str(&text[at..start]);
        out.push_str(new_text);
        at = end;
    }
    out.push_str(&text[at..]);
    out
}

fn project() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(
        dir.path().join("oxabl.toml"),
        "[workspace]\nname = \"range\"\n",
    )
    .unwrap();
    dir
}

fn open(text: &str) -> (tempfile::TempDir, Session, std::path::PathBuf) {
    let dir = project();
    let mut session = Session::start(dir.path());
    let path = dir.path().join("greet.p");
    session.open(&path, text);
    (dir, session, path)
}

#[test]
fn the_capability_is_advertised_with_ranges_support() {
    let dir = project();
    let session = Session::start(dir.path());
    let provider =
        &session.initialized["result"]["capabilities"]["documentRangeFormattingProvider"];
    assert_eq!(provider["rangesSupport"], json!(true), "{provider}");
}

#[test]
fn a_selection_inside_a_procedure_formats_only_its_statement() {
    let (_dir, mut session, path) = open(FLAT);
    // Line 3: the `MESSAGE` with the emoji, inside the IF block of a procedure.
    let answer = format_range(&mut session, &path, range((3, 0), (3, 20)));
    assert!(answer.shown.is_empty(), "{:?}", answer.shown);
    assert_eq!(
        apply(FLAT, &answer.edits),
        FLAT.replace("\nMESSAGE \"positive", "\n        MESSAGE \"positive"),
    );
}

#[test]
fn a_selection_that_starts_mid_statement_takes_the_whole_statement() {
    let (_dir, mut session, path) = open(FLAT);
    // From inside `MESSAGE` on line 4 to inside the string on line 5: one
    // statement over two lines, both indented.
    let answer = format_range(&mut session, &path, range((4, 3), (5, 2)));
    assert_eq!(
        apply(FLAT, &answer.edits),
        FLAT.replace(
            "\nMESSAGE\n\"wrapped\".",
            "\n        MESSAGE\n        \"wrapped\"."
        ),
    );
}

#[test]
fn a_selection_across_statements_formats_each_and_leaves_the_rest() {
    let (_dir, mut session, path) = open(FLAT);
    // Lines 2 to 6: the whole IF block.
    let answer = format_range(&mut session, &path, range((2, 0), (7, 0)));
    let expected = "\
PROCEDURE greet:
DEFINE VARIABLE n AS INTEGER NO-UNDO.
    IF n > 0 THEN DO:
        MESSAGE \"positive 😀\".
        MESSAGE
        \"wrapped\".
    END.
MESSAGE \"after\".
END PROCEDURE.
MESSAGE \"top\".
";
    assert_eq!(apply(FLAT, &answer.edits), expected);
}

#[test]
fn an_empty_range_stands_for_its_line() {
    let (_dir, mut session, path) = open(FLAT);
    let answer = format_range(&mut session, &path, range((7, 4), (7, 4)));
    assert_eq!(
        apply(FLAT, &answer.edits),
        FLAT.replace("\nMESSAGE \"after", "\n    MESSAGE \"after"),
    );
}

#[test]
fn several_ranges_format_each_and_not_the_lines_between() {
    let (_dir, mut session, path) = open(FLAT);
    let answer = ask(
        &mut session,
        "textDocument/rangesFormatting",
        json!({
            "textDocument": {"uri": file_uri(&path)},
            "ranges": [range((1, 0), (1, 5)), range((7, 0), (7, 5)), range((9, 0), (9, 5))],
            "options": options()
        }),
    );
    let expected = FLAT
        .replace("\nDEFINE", "\n    DEFINE")
        .replace("\nMESSAGE \"after", "\n    MESSAGE \"after");
    assert_eq!(apply(FLAT, &answer.edits), expected);
}

#[test]
fn a_range_in_a_statement_already_formatted_gives_no_edits() {
    let text = "PROCEDURE p:\n    MESSAGE \"a\".\nEND PROCEDURE.\n";
    let (_dir, mut session, path) = open(text);
    let answer = format_range(&mut session, &path, range((1, 0), (1, 10)));
    assert!(answer.edits.is_empty(), "{:?}", answer.edits);
    assert!(answer.shown.is_empty(), "{:?}", answer.shown);
}

#[test]
fn a_file_the_formatter_refuses_gives_no_edits_and_a_message() {
    // The unterminated block elsewhere makes the whole file unformattable.
    let text = "MESSAGE \"a\".\nDO:\nMESSAGE \"b\".\n";
    let (_dir, mut session, path) = open(text);
    let answer = format_range(&mut session, &path, range((0, 0), (0, 5)));
    assert!(answer.edits.is_empty(), "{:?}", answer.edits);
    assert_eq!(answer.shown.len(), 1, "{:?}", answer.shown);
    assert!(
        answer.shown[0].contains("parse errors"),
        "the message names the reason: {:?}",
        answer.shown
    );
}

#[test]
fn a_selection_with_no_statement_gives_no_edits_and_a_message() {
    let text = "MESSAGE \"a\". /* trailing */\n\n\nMESSAGE \"b\".\n";
    let (_dir, mut session, path) = open(text);
    // Only blank lines.
    let blank = format_range(&mut session, &path, range((1, 0), (2, 0)));
    assert!(blank.edits.is_empty());
    assert!(
        blank.shown.iter().any(|m| m.contains("no statement")),
        "{:?}",
        blank.shown
    );
    // Only a comment that follows code.
    let trailing = format_range(&mut session, &path, range((0, 15), (0, 25)));
    assert!(trailing.edits.is_empty());
    assert!(
        trailing.shown.iter().any(|m| m.contains("no statement")),
        "{:?}",
        trailing.shown
    );
}

#[test]
fn a_comment_on_its_own_line_is_placed_with_its_statement() {
    let text = "DO:\n/* note */\nMESSAGE \"a\".\nEND.\n";
    let (_dir, mut session, path) = open(text);
    let answer = format_range(&mut session, &path, range((1, 2), (1, 4)));
    assert_eq!(
        apply(text, &answer.edits),
        "DO:\n    /* note */\nMESSAGE \"a\".\nEND.\n"
    );
}

#[test]
fn a_selection_in_an_include_reference_or_a_directive_stays_in_its_line() {
    let text = "&SCOPED-DEFINE a 1\nPROCEDURE p:\n{inc.i &x=1}\nMESSAGE \"a\".\nEND.\n";
    let (_dir, mut session, path) = open(text);
    let include = format_range(&mut session, &path, range((2, 3), (2, 6)));
    assert_eq!(
        apply(text, &include.edits),
        "&SCOPED-DEFINE a 1\nPROCEDURE p:\n    {inc.i &x=1}\nMESSAGE \"a\".\nEND.\n"
    );
    let directive = format_range(&mut session, &path, range((0, 3), (0, 6)));
    assert!(directive.edits.is_empty(), "{:?}", directive.edits);
}

#[test]
fn a_blank_run_inside_the_selection_shrinks_and_the_ones_outside_stay() {
    let text = "\n\n\nMESSAGE \"a\".\n\n\n\n\nMESSAGE \"b\".\n\n\n\nMESSAGE \"c\".\n";
    let (_dir, mut session, path) = open(text);
    let answer = format_range(&mut session, &path, range((3, 0), (8, 5)));
    assert_eq!(
        apply(text, &answer.edits),
        "\n\n\nMESSAGE \"a\".\n\nMESSAGE \"b\".\n\n\n\nMESSAGE \"c\".\n"
    );
}

#[test]
fn a_range_then_the_whole_document_is_the_whole_document() {
    let (_dir, mut session, path) = open(FLAT);
    let whole = ask(
        &mut session,
        "textDocument/formatting",
        json!({"textDocument": {"uri": file_uri(&path)}, "options": options()}),
    );
    let expected = apply(FLAT, &whole.edits);
    let part = format_range(&mut session, &path, range((3, 0), (3, 1)));
    let half = apply(FLAT, &part.edits);
    assert_ne!(half, FLAT);

    // The editor applies the edits; the next whole-document format agrees.
    let second = path.with_file_name("second.p");
    session.open(&second, &half);
    let rest = ask(
        &mut session,
        "textDocument/formatting",
        json!({"textDocument": {"uri": file_uri(&second)}, "options": options()}),
    );
    assert_eq!(apply(&half, &rest.edits), expected);
}

#[test]
fn a_closed_buffer_gives_no_edits() {
    let dir = project();
    let mut session = Session::start(dir.path());
    let path = dir.path().join("never-opened.p");
    let answer = format_range(&mut session, &path, range((0, 0), (1, 0)));
    assert!(answer.edits.is_empty());
}
