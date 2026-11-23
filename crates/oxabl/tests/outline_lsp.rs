//! The document outline through the real `oxabl lsp` and its daemon.
//!
//! The daemon sends its answer to the proxy as JSON, and the proxy reads it
//! with a recursion limit. A tree that is too deep must never end the session,
//! so these tests send documents that nest far more symbols than an editor
//! shows, as a client that asks for a hierarchical outline.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::Session;

/// The answer to `textDocument/documentSymbol` for `path`, which is open.
fn outline(session: &mut Session, path: &Path) -> Value {
    session.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": session::file_uri(path)}}),
    )
}

/// The symbols of a successful answer. Fails on an error or a `null`.
#[track_caller]
fn symbols_of(answer: &Value) -> &Vec<Value> {
    assert!(
        answer.get("error").is_none(),
        "documentSymbol gave an error: {}",
        answer["error"]
    );
    answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("documentSymbol gave no list: {}", answer["result"]))
}

/// The number of levels of `symbols`, and the number of symbols in them.
fn shape(symbols: &[Value]) -> (usize, usize) {
    let mut levels = 0;
    let mut count = 0;
    for symbol in symbols {
        let children = symbol["children"].as_array().map_or(&[][..], |c| c);
        let (below, inside) = shape(children);
        levels = levels.max(1 + below);
        count += 1 + inside;
    }
    (levels, count)
}

fn labelled_blocks(depth: usize) -> String {
    let mut source = String::new();
    for i in 1..=depth {
        source.push_str(&format!("l{i}: DO:\n"));
    }
    source.push_str("DEFINE VARIABLE bottom AS INTEGER NO-UNDO.\n");
    source.push_str(&"END.\n".repeat(depth));
    source
}

fn interface_with_methods(count: usize) -> String {
    let mut source = String::from("INTERFACE I:\n");
    for i in 1..=count {
        source.push_str(&format!("METHOD PUBLIC VOID M{i}(INPUT x AS INTEGER).\n"));
    }
    source.push_str("END INTERFACE.\n");
    source
}

#[test]
fn a_deep_outline_is_answered_and_the_session_goes_on() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"outline\"\n",
    )
    .unwrap();
    let mut session = Session::start_with(
        root.path(),
        json!({"textDocument": {"documentSymbol": {
            "hierarchicalDocumentSymbolSupport": true
        }}}),
    );

    let labelled = root.path().join("labelled.p");
    session.open(&labelled, &labelled_blocks(70));
    let answer = outline(&mut session, &labelled);
    // 32 labels and the variable: the labels below the limit give no symbol.
    assert_eq!(shape(symbols_of(&answer)), (33, 33));

    let interface = root.path().join("iface.cls");
    session.open(&interface, &interface_with_methods(70));
    let answer = outline(&mut session, &interface);
    let symbols = symbols_of(&answer);
    assert_eq!(symbols.len(), 1, "one interface");
    // The interface, its 70 methods and the parameter of each.
    assert_eq!(shape(symbols), (3, 141));

    let small = root.path().join("small.p");
    session.open(&small, "PROCEDURE p:\nEND PROCEDURE.\n");
    let answer = outline(&mut session, &small);
    let symbols = symbols_of(&answer);
    assert_eq!(symbols.len(), 1);
    assert_eq!(symbols[0]["name"], json!("p"));
}
