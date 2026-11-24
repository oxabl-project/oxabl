//! Hover requests through the real `oxabl lsp` run one at a time: a hover
//! that a newer one replaced before it started, or that the client cancelled,
//! answers `RequestCanceled` (-32800) and computes nothing. The last hover
//! always gets its answer.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use serde_json::{Value, json};
use session::{Session, file_uri};

const REQUEST_CANCELED: i64 = -32800;

/// A file large enough that one hover takes a while to analyse.
fn large() -> String {
    let mut text = String::new();
    for n in 0..40_000 {
        text.push_str(&format!("DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    text.push_str("v1 = v2 + 1.\n");
    text
}

fn hover_params(uri: &str, line: usize) -> Value {
    json!({
        "textDocument": {"uri": uri},
        "position": {"line": line, "character": 17}
    })
}

#[test]
fn a_hover_replaced_before_it_started_is_cancelled() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("large.p");
    let text = large();
    std::fs::write(&path, &text).unwrap();
    let mut session = Session::start(dir.path());
    session.open(&path, &text);
    let uri = file_uri(&path);

    let params = (0..10).map(|line| hover_params(&uri, line)).collect();
    let answers = session.request_all("textDocument/hover", params);
    let (last, earlier) = answers.split_last().unwrap();
    assert!(
        last["result"]["contents"]["value"]
            .as_str()
            .is_some_and(|v| v.contains("DEFINE VARIABLE v9 ")),
        "the last hover must be answered: {last}"
    );
    let cancelled = earlier
        .iter()
        .filter(|a| a["error"]["code"].as_i64() == Some(REQUEST_CANCELED))
        .count();
    assert!(
        cancelled > 0,
        "hovers replaced while another ran must be cancelled: {earlier:?}"
    );
    for answer in earlier {
        let answered = answer["result"]["contents"]["value"].is_string();
        let cancelled = answer["error"]["code"].as_i64() == Some(REQUEST_CANCELED);
        assert!(answered || cancelled, "{answer}");
    }
}

#[test]
fn a_hover_the_client_cancels_is_not_computed() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("large.p");
    let text = large();
    std::fs::write(&path, &text).unwrap();
    let mut session = Session::start(dir.path());
    session.open(&path, &text);
    let uri = file_uri(&path);

    // The server has read the document once this answer is in.
    let warm = session.request("textDocument/hover", hover_params(&uri, 2));
    assert!(warm["result"]["contents"]["value"].is_string(), "{warm}");

    // The first hover starts and runs; the second waits for it and is
    // cancelled while it waits.
    for (id, line) in [(9001, 0), (9002, 1)] {
        session.send(json!({
            "jsonrpc": "2.0", "id": id, "method": "textDocument/hover",
            "params": hover_params(&uri, line)
        }));
        if id == 9001 {
            std::thread::sleep(std::time::Duration::from_millis(300));
        }
    }
    session.send(json!({
        "jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": 9002}
    }));
    // Either answer can come first; `wait_for` drops what it does not want.
    let (mut first, mut second) = (None, None);
    while first.is_none() || second.is_none() {
        let answer = session.wait_for(|m| m["id"] == json!(9001) || m["id"] == json!(9002));
        if answer["id"] == json!(9001) {
            first = Some(answer);
        } else {
            second = Some(answer);
        }
    }
    let (first, second) = (first.unwrap(), second.unwrap());
    assert!(
        first["result"]["contents"]["value"].is_string(),
        "the running hover is answered: {first}"
    );
    assert_eq!(
        second["error"]["code"].as_i64(),
        Some(REQUEST_CANCELED),
        "the cancelled hover: {second}"
    );
}
