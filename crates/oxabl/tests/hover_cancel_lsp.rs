//! Hover requests through the real `oxabl lsp` run one at a time: a hover
//! that a newer one replaced before it started, or that the client cancelled,
//! answers `RequestCanceled` (-32800) and computes nothing. The last hover
//! always gets its answer.
//!
//! A hover on `held.p` waits three seconds before it computes
//! (`OXABL-TEST-HOLD:hover`, `oxabl_common::HOLD_TIME`, armed only in test
//! builds), so the hovers sent after it are still waiting for the gate when the
//! next message arrives.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::thread::sleep;
use std::time::Duration;

use serde_json::{Value, json};
use session::{Session, file_uri};

const REQUEST_CANCELED: i64 = -32800;

/// Long enough for the server to start the hover just sent, far shorter than
/// the hold.
const SETTLE: Duration = Duration::from_millis(300);

const HELD: &str = "/* OXABL-TEST-HOLD:hover */\n\
    DEFINE VARIABLE v0 AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE v1 AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE v2 AS INTEGER NO-UNDO.\n";

fn hover_params(uri: &str, line: usize) -> Value {
    json!({
        "textDocument": {"uri": uri},
        "position": {"line": line, "character": 17}
    })
}

fn hover(session: &mut Session, id: u64, uri: &str, line: usize) {
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/hover",
        "params": hover_params(uri, line)
    }));
}

/// The answers to `ids`, in that order, whatever order they come in.
fn answers(session: &Session, ids: &[u64]) -> Vec<Value> {
    let mut found: Vec<Option<Value>> = vec![None; ids.len()];
    while found.iter().any(Option::is_none) {
        let answer = session.wait_for(|m| ids.iter().any(|id| m["id"] == json!(id)));
        let at = ids.iter().position(|id| answer["id"] == json!(id)).unwrap();
        found[at] = Some(answer);
    }
    found.into_iter().map(Option::unwrap).collect()
}

fn answered(answer: &Value, variable: &str) -> bool {
    answer["result"]["contents"]["value"]
        .as_str()
        .is_some_and(|v| v.contains(&format!("DEFINE VARIABLE {variable} ")))
}

fn cancelled(answer: &Value) -> bool {
    answer["error"]["code"].as_i64() == Some(REQUEST_CANCELED)
}

fn open_held(dir: &Path) -> (Session, String) {
    let path = dir.join("held.p");
    std::fs::write(&path, HELD).unwrap();
    let mut session = Session::start(dir);
    session.open(&path, HELD);
    (session, file_uri(&path))
}

#[test]
fn a_hover_replaced_before_it_started_is_cancelled() {
    let dir = tempfile::tempdir().unwrap();
    let (mut session, uri) = open_held(dir.path());

    // The first hover holds the gate; the next three queue behind it.
    hover(&mut session, 9001, &uri, 1);
    sleep(SETTLE);
    for (id, line) in [(9002, 2), (9003, 3), (9004, 1)] {
        hover(&mut session, id, &uri, line);
    }
    let found = answers(&session, &[9001, 9002, 9003, 9004]);
    assert!(answered(&found[0], "v0"), "the running hover: {}", found[0]);
    assert!(cancelled(&found[1]), "a replaced hover: {}", found[1]);
    assert!(cancelled(&found[2]), "a replaced hover: {}", found[2]);
    assert!(answered(&found[3], "v0"), "the newest hover: {}", found[3]);
}

#[test]
fn a_hover_the_client_cancels_is_not_computed() {
    let dir = tempfile::tempdir().unwrap();
    let (mut session, uri) = open_held(dir.path());

    // The first hover holds the gate; the second waits for it and is
    // cancelled while it waits.
    hover(&mut session, 9001, &uri, 1);
    sleep(SETTLE);
    hover(&mut session, 9002, &uri, 2);
    session.send(json!({
        "jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": 9002}
    }));
    let found = answers(&session, &[9001, 9002]);
    assert!(answered(&found[0], "v0"), "the running hover: {}", found[0]);
    assert!(cancelled(&found[1]), "the cancelled hover: {}", found[1]);
}
