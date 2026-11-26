//! Completion cases from the first review: the server stays free while a base
//! is refreshed, a root edit does not stall a request in an include, the
//! accessor and shadowing scopes, the keywords of method and function bodies,
//! the trigger characters, a document closed during a refresh, and a static
//! method. The marker `OXABL-TEST-HOLD:completion.refresh` holds the
//! background analysis for three seconds, in test builds only.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use std::thread::sleep;
use std::time::{Duration, Instant};

use completion::{Answer, Case, Files, KEYWORD, PROPERTY, VARIABLE, Ws};
use serde_json::json;
use session::file_uri;

const HOLD: &str = "/* OXABL-TEST-HOLD:completion.refresh */\n";

/// How long a request that the server answers at once takes, after `edit`.
fn trivial_request_after(ws: &mut Ws, edit: impl FnOnce(&mut Ws)) -> Duration {
    edit(ws);
    let missing = file_uri(&ws.path("not-open.p"));
    let started = Instant::now();
    let response = ws.session.request(
        "textDocument/completion",
        json!({
            "textDocument": {"uri": missing},
            "position": {"line": 0, "character": 0},
            "context": {"triggerKind": 1}
        }),
    );
    let took = started.elapsed();
    assert!(
        response.get("error").is_some(),
        "a file that is not open has no completion: {response}"
    );
    took
}

/// The answer a request gives after `wait`, asked until it is complete.
fn settled(ws: &mut Ws, rel: &str, cursor: completion::Cursor) -> Answer {
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        let answer = ws.complete_at(rel, cursor, None);
        if !answer.list().incomplete || Instant::now() > deadline {
            return answer;
        }
        sleep(Duration::from_millis(250));
    }
}

#[test]
fn an_edit_during_a_held_refresh_does_not_stop_the_server() {
    let mut ws = Files::config_n(Case::Upper).start();
    let text = format!("{HOLD}DEFINE VARIABLE iKeep AS INTEGER NO-UNDO.\ni|");
    let cursor = ws.open_marked("main.p", &text);
    ws.complete_at("main.p", cursor, None).list();
    // The edit makes the base stale; the next request starts a held refresh.
    ws.insert_at_line_start("main.p", 2, 2, "iKeep = 1.\n");
    let after = completion::Cursor {
        line: cursor.line + 1,
        ..cursor
    };
    let stale = ws.complete_at("main.p", after, None);
    assert!(stale.list().incomplete, "{}", stale.summary());
    // A second edit while the refresh is held, then a request the server
    // answers at once.
    let took = trivial_request_after(&mut ws, |ws| {
        ws.insert_at_line_start("main.p", 3, 3, "iKeep = 2.\n");
    });
    assert!(
        took < Duration::from_millis(300),
        "the server answered after {took:?}: an edit waited for the refresh"
    );
}

#[test]
fn a_root_edit_does_not_make_a_request_in_its_include_wait() {
    let mut root = String::from(HOLD);
    root.push_str("DEFINE VARIABLE gcRoot AS CHARACTER NO-UNDO.\n");
    for k in 0..10_000 {
        root.push_str(&format!(
            "PROCEDURE p{k}:\n    DEFINE VARIABLE i{k} AS INTEGER NO-UNDO.\n    i{k} = i{k} + 1.\nEND PROCEDURE.\n"
        ));
    }
    root.push_str("{inc/body.i}\n");
    // Diagnostics off: their analysis of the large root runs under a
    // snapshot, and the edit would wait for it, whatever completion does.
    let mut ws = Files::config_a(Case::Upper)
        .file(
            "oxabl.toml",
            "[workspace]\nname = \"complete\"\n\
             [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
             [workspace.schema]\nfiles = [\"db/shop.df\"]\n\
             [workspace.features]\ndiagnostics = false\n\
             [workspace.style]\nkeyword_case = \"Uppercase\"\n",
        )
        .file("big.p", &root)
        .start();
    ws.session.open(&ws.path("big.p"), &root);
    let cursor = ws.open_marked(
        "inc/body.i",
        "DEFINE VARIABLE iInInclude AS INTEGER NO-UNDO.\n|\n",
    );
    let selected = ws.session.request(
        "oxabl/selectIncluderContext",
        json!({
            "textDocument": {"uri": file_uri(&ws.path("inc/body.i"))},
            "root": ws.canonical("big.p")
        }),
    );
    assert!(selected.get("error").is_none(), "{selected}");
    let first = ws.complete_at("inc/body.i", cursor, None);
    first.list().contains(VARIABLE, &["gcRoot", "iInInclude"]);

    // An edit of the root, then a request in the include.
    ws.insert_at_line_start(
        "big.p",
        2,
        1,
        "DEFINE VARIABLE gcAdded AS CHARACTER NO-UNDO.\n",
    );
    let started = Instant::now();
    let after = ws.complete_at("inc/body.i", cursor, None);
    let took = started.elapsed();
    assert!(
        took < Duration::from_millis(1500),
        "the request after a root edit took {took:?}"
    );
    let list = after.list();
    assert!(list.incomplete, "{}", after.summary());
    list.contains(VARIABLE, &["gcRoot", "iInInclude"]);

    // The refresh brings the new name.
    sleep(Duration::from_secs(3));
    let fresh = settled(&mut ws, "inc/body.i", cursor);
    fresh
        .list()
        .contains(VARIABLE, &["gcRoot", "gcAdded", "iInInclude"]);
}

#[test]
fn a_getter_offers_its_own_locals() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "Box.cls",
        "CLASS Box:\n    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO\n        GET():\n            DEFINE VARIABLE xGet AS INTEGER NO-UNDO.\n            xGet = x|\n        END GET.\n        SET(INPUT arg AS INTEGER):\n            DEFINE VARIABLE xSet AS INTEGER NO-UNDO.\n        END SET.\nEND CLASS.\n",
    );
    let list = answer.list();
    list.contains(VARIABLE, &["xGet"]);
    list.lacks(&["xSet", "arg"]);
    // The same with each accessor closed by a bare `END.`.
    let answer = ws.complete(
        "Bare.cls",
        "CLASS Bare:\n    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO\n        GET():\n            DEFINE VARIABLE xGet AS INTEGER NO-UNDO.\n            xGet = x|\n        END.\n        SET(INPUT arg AS INTEGER):\n            DEFINE VARIABLE xSet AS INTEGER NO-UNDO.\n        END.\nEND CLASS.\n",
    );
    let list = answer.list();
    list.contains(VARIABLE, &["xGet"]);
    list.lacks(&["xSet", "arg"]);
}

#[test]
fn a_later_inner_declaration_does_not_hide_the_outer_name() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE xVal AS INTEGER NO-UNDO.\nPROCEDURE p:\n    DEFINE VARIABLE iY AS INTEGER NO-UNDO.\n    iY = x|\n    DEFINE VARIABLE xVal AS CHARACTER NO-UNDO.\nEND PROCEDURE.\n",
    );
    let list = answer.list();
    list.contains(VARIABLE, &["xVal"]);
    assert_eq!(
        completion::new_text(list.item(VARIABLE, "xVal")),
        "xVal",
        "{}",
        answer.summary()
    );
    let detail = list.item(VARIABLE, "xVal")["detail"].clone();
    assert_eq!(
        detail,
        json!("INTEGER"),
        "the outer variable: {}",
        answer.summary()
    );
}

#[test]
fn a_method_body_offers_no_shared_or_parameter_words() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "Box.cls",
        "CLASS Box:\n    METHOD PUBLIC VOID Fill():\n        DEFINE |\n    END METHOD.\nEND CLASS.\n",
    );
    let list = answer.list();
    list.contains(KEYWORD, &["VARIABLE", "BUFFER", "TEMP-TABLE"]);
    list.lacks_kind(
        KEYWORD,
        &[
            "NEW",
            "SHARED",
            "GLOBAL",
            "INPUT",
            "OUTPUT",
            "INPUT-OUTPUT",
            "RETURN",
            "PARAMETER",
        ],
    );
}

#[test]
fn a_function_body_offers_no_parameter_words() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "FUNCTION f RETURNS INTEGER ():\n    DEFINE |\n    RETURN 1.\nEND FUNCTION.\n",
    );
    let list = answer.list();
    list.contains(KEYWORD, &["VARIABLE", "BUFFER"]);
    list.lacks_kind(KEYWORD, &["PARAMETER", "INPUT", "OUTPUT", "INPUT-OUTPUT"]);
}

#[test]
fn the_statement_after_then_is_not_a_file_statement() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE x AS LOGICAL NO-UNDO.\nIF x THEN |",
    );
    let list = answer.list();
    list.contains(KEYWORD, &["MESSAGE", "RUN", "DO"]);
    list.lacks_kind(
        KEYWORD,
        &[
            "PROCEDURE",
            "FUNCTION",
            "USING",
            "BLOCK-LEVEL",
            "ROUTINE-LEVEL",
        ],
    );
}

#[test]
fn a_slash_with_nothing_before_it_gives_no_list() {
    let mut ws = Files::config_n(Case::Upper).start();
    // The start of a comment at a statement start.
    ws.complete_trigger("a.p", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n/|", "/")
        .assert_null();
    // A division typed with no space.
    ws.complete_trigger(
        "b.p",
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = x /|",
        "/",
    )
    .assert_null();
    // A period typed inside a name in an expression asks for a qualified name,
    // which has no list yet; a period at the end of a statement gives none.
    ws.complete_trigger("c.p", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = 1.|", ".")
        .assert_null();
}

#[test]
fn a_document_closed_during_a_refresh_starts_again_fresh() {
    let mut ws = Files::config_n(Case::Upper).start();
    let text = format!("{HOLD}DEFINE VARIABLE iOld AS INTEGER NO-UNDO.\ni|");
    let cursor = ws.open_marked("main.p", &text);
    ws.complete_at("main.p", cursor, None).list();
    ws.insert_at_line_start("main.p", 2, 1, "DEFINE VARIABLE iMid AS INTEGER NO-UNDO.\n");
    let after = completion::Cursor {
        line: cursor.line + 1,
        ..cursor
    };
    assert!(ws.complete_at("main.p", after, None).list().incomplete);
    // Close while the refresh is held, and wait for it to end.
    ws.session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didClose",
        "params": {"textDocument": {"uri": file_uri(&ws.path("main.p"))}}
    }));
    sleep(Duration::from_secs(4));
    // Open the file again with other text: the first answer is of that text.
    let answer = ws.complete("main.p", "DEFINE VARIABLE iNew AS INTEGER NO-UNDO.\ni|");
    let list = answer.list();
    assert!(!list.incomplete, "{}", answer.summary());
    list.contains(VARIABLE, &["iNew"]);
    list.lacks(&["iOld", "iMid"]);
}

#[test]
fn a_static_method_offers_no_instance_member() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "Box.cls",
        "CLASS Box:\n    DEFINE PRIVATE VARIABLE iInst AS INTEGER NO-UNDO.\n    DEFINE PRIVATE STATIC VARIABLE iShared AS INTEGER NO-UNDO.\n    METHOD PUBLIC STATIC VOID m():\n        DEFINE VARIABLE o AS INTEGER NO-UNDO.\n        o = |\n    END METHOD.\nEND CLASS.\n",
    );
    let list = answer.list();
    list.contains(PROPERTY, &["iShared"]);
    list.contains(VARIABLE, &["o"]);
    list.lacks(&["iInst"]);
}

/// A `.` typed at the end of a statement answers `null` with no analysis:
/// the first request on a document that never had one is a trigger at a
/// statement end, and it answers far below the cost of an analysis. A hold
/// marker makes the first analysis of the document take 1.5 s, whatever its
/// size, so the test does not depend on how fast the analysis is.
/// Diagnostics are off: the server computes those of a newly opened file on
/// its main loop, and the request would wait for them, whatever completion
/// does.
#[test]
fn a_period_at_a_statement_end_answers_without_an_analysis() {
    let mut ws = Files::config_n(Case::Upper)
        .file(
            "oxabl.toml",
            "[workspace]\nname = \"bare\"\n[workspace.features]\ndiagnostics = false\n",
        )
        .start();
    let mut text = String::from("/* OXABL-TEST-HOLD:completion.base@1500 */\n");
    for k in 0..100 {
        text.push_str(&format!(
            "PROCEDURE p{k}:\n    DEFINE VARIABLE i{k} AS INTEGER NO-UNDO.\n    i{k} = i{k} + 1.\nEND PROCEDURE.\n"
        ));
    }
    text.push_str("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = 1.|");
    let cursor = ws.open_marked("large.p", &text);
    let started = Instant::now();
    let answer = ws.complete_at("large.p", cursor, Some("."));
    let took = started.elapsed();
    answer.assert_null();
    assert!(
        took < Duration::from_millis(1000),
        "a `.` at a statement end took {took:?}: it waited for an analysis"
    );
    // The control: a request that needs the analysis waits for the hold.
    let started = Instant::now();
    ws.complete_at(
        "large.p",
        completion::Cursor {
            line: cursor.line,
            character: 0,
        },
        None,
    )
    .list();
    let analysed = started.elapsed();
    assert!(
        analysed >= Duration::from_millis(1500),
        "the control took {analysed:?}: the hold of the analysis did not act"
    );
}
