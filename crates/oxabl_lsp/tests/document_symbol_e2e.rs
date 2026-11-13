//! `textDocument/documentSymbol` end to end, over an in-memory connection:
//! the capability, both response shapes, the position encodings, and the
//! error answers. All fixtures are synthetic ABL.
//!
//! Tests that read real symbols need the outline walker in `oxabl_pipeline`.

use std::str::FromStr;
use std::thread;

use lsp_server::{Connection, ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbol,
    DocumentSymbolClientCapabilities, DocumentSymbolParams, DocumentSymbolResponse,
    GeneralClientCapabilities, InitializeParams, InitializeResult, OneOf, Position,
    PositionEncodingKind, SymbolInformation, SymbolKind, TextDocumentClientCapabilities,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Uri,
    VersionedTextDocumentIdentifier,
};
use oxabl_common::panic_sites;

fn handshake(
    client: &Connection,
    encoding: PositionEncodingKind,
    hierarchical: bool,
) -> InitializeResult {
    let params = InitializeParams {
        capabilities: ClientCapabilities {
            general: Some(GeneralClientCapabilities {
                position_encodings: Some(vec![encoding]),
                ..Default::default()
            }),
            text_document: Some(TextDocumentClientCapabilities {
                document_symbol: Some(DocumentSymbolClientCapabilities {
                    hierarchical_document_symbol_support: Some(hierarchical),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            ..Default::default()
        },
        ..Default::default()
    };
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(1),
            method: "initialize".to_string(),
            params: serde_json::to_value(params).unwrap(),
        }))
        .unwrap();
    let response = wait_response(client, 1);
    notify(client, "initialized", serde_json::json!({}));
    serde_json::from_value(response.result.unwrap()).unwrap()
}

fn notify(client: &Connection, method: &str, params: serde_json::Value) {
    client
        .sender
        .send(Message::Notification(Notification {
            method: method.to_string(),
            params,
        }))
        .unwrap();
}

fn wait_response(client: &Connection, id: i32) -> Response {
    loop {
        if let Message::Response(r) = client.receiver.recv().unwrap()
            && r.id == RequestId::from(id)
        {
            return r;
        }
    }
}

fn open(client: &Connection, uri: &Uri, text: &str) {
    notify(
        client,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "abl".to_string(),
                version: 1,
                text: text.to_string(),
            },
        })
        .unwrap(),
    );
}

fn replace(client: &Connection, uri: &Uri, version: i32, text: &str) {
    notify(
        client,
        "textDocument/didChange",
        serde_json::to_value(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: text.to_string(),
            }],
        })
        .unwrap(),
    );
}

fn request(client: &Connection, id: i32, uri: &Uri) -> Response {
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(id),
            method: "textDocument/documentSymbol".to_string(),
            params: serde_json::to_value(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })
            .unwrap(),
        }))
        .unwrap();
    wait_response(client, id)
}

fn nested(response: Response) -> Vec<DocumentSymbol> {
    assert!(response.error.is_none(), "unexpected error: {response:?}");
    match serde_json::from_value(response.result.unwrap()).unwrap() {
        DocumentSymbolResponse::Nested(symbols) => symbols,
        DocumentSymbolResponse::Flat(_) => panic!("expected a tree"),
    }
}

fn flat(response: Response) -> Vec<SymbolInformation> {
    assert!(response.error.is_none(), "unexpected error: {response:?}");
    match serde_json::from_value(response.result.unwrap()).unwrap() {
        DocumentSymbolResponse::Flat(symbols) => symbols,
        DocumentSymbolResponse::Nested(_) => panic!("expected a flat list"),
    }
}

fn finish(client: &Connection, handle: thread::JoinHandle<anyhow::Result<bool>>) {
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(99),
            method: "shutdown".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    wait_response(client, 99);
    notify(client, "exit", serde_json::Value::Null);
    assert!(handle.join().unwrap().unwrap());
}

fn start() -> (Connection, thread::JoinHandle<anyhow::Result<bool>>) {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));
    (client, handle)
}

#[test]
fn advertises_the_document_symbol_provider() {
    let (client, handle) = start();
    let init = handshake(&client, PositionEncodingKind::UTF16, true);
    assert_eq!(
        init.capabilities.document_symbol_provider,
        Some(OneOf::Left(true))
    );
    finish(&client, handle);
}

#[test]
fn an_unopened_document_answers_null() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///never-opened.p").unwrap();
    let response = request(&client, 2, &uri);
    assert!(response.error.is_none(), "{response:?}");
    assert_eq!(response.result, Some(serde_json::Value::Null));
    finish(&client, handle);
}

#[test]
fn bad_params_answer_invalid_params() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(2),
            method: "textDocument/documentSymbol".to_string(),
            params: serde_json::json!({ "textDocument": 7 }),
        }))
        .unwrap();
    let response = wait_response(&client, 2);
    let error = response.error.expect("bad params must be an error");
    assert_eq!(error.code, ErrorCode::InvalidParams as i32);
    finish(&client, handle);
}

#[test]
fn a_contained_panic_is_a_request_failure_not_an_empty_list() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///panic-outline.p").unwrap();
    open(
        &client,
        &uri,
        &format!(
            "/* OXABL-TEST-PANIC:{} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n",
            panic_sites::OUTLINE
        ),
    );
    let response = request(&client, 2, &uri);
    let error = response.error.as_ref().expect("a panic must be an error");
    assert_eq!(error.code, ErrorCode::RequestFailed as i32);
    assert!(response.result.is_none(), "{response:?}");

    // The server survives, and the buffer recovers once the marker is gone.
    replace(&client, &uri, 2, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    assert!(request(&client, 3, &uri).error.is_none());
    finish(&client, handle);
}

#[test]
fn a_hierarchical_client_gets_a_tree_with_ranges() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///tree.p").unwrap();
    open(
        &client,
        &uri,
        "PROCEDURE p:\n  DEFINE VARIABLE v AS INTEGER NO-UNDO.\nEND PROCEDURE.\n",
    );
    let symbols = nested(request(&client, 2, &uri));
    assert_eq!(symbols.len(), 1, "{symbols:?}");
    let p = &symbols[0];
    assert_eq!(p.name, "p");
    assert_eq!(p.kind, SymbolKind::FUNCTION);
    assert_eq!(p.range.start, Position::new(0, 0));
    assert_eq!(p.range.end.line, 2);
    assert_eq!(p.selection_range.start, Position::new(0, 10));
    assert_eq!(p.selection_range.end, Position::new(0, 11));
    let children = p.children.as_ref().expect("the variable is a child");
    assert_eq!(children[0].name, "v");
    assert_eq!(children[0].kind, SymbolKind::VARIABLE);
    assert_eq!(children[0].detail.as_deref(), Some("INTEGER"));
    finish(&client, handle);
}

#[test]
fn a_flat_client_gets_symbol_information_with_container_names() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, false);
    let uri = Uri::from_str("file:///flat.p").unwrap();
    open(
        &client,
        &uri,
        "PROCEDURE p:\n  DEFINE VARIABLE v AS INTEGER NO-UNDO.\nEND PROCEDURE.\n",
    );
    let symbols = flat(request(&client, 2, &uri));
    assert_eq!(symbols.len(), 2, "{symbols:?}");
    assert_eq!(
        (
            symbols[0].name.as_str(),
            symbols[0].container_name.as_deref()
        ),
        ("p", None)
    );
    assert_eq!(
        (
            symbols[1].name.as_str(),
            symbols[1].container_name.as_deref()
        ),
        ("v", Some("p"))
    );
    assert_eq!(symbols[1].location.uri, uri);
    assert_eq!(symbols[1].location.range.start, Position::new(1, 2));
    finish(&client, handle);
}

// "😀" is 4 UTF-8 bytes and 2 UTF-16 units, so the column of the name after
// it tells the encodings apart.
const MULTIBYTE: &str = "/* 😀 */ PROCEDURE p:\nEND PROCEDURE.\n";

#[test]
fn ranges_follow_utf16_on_a_line_with_multibyte_text() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///utf16.p").unwrap();
    open(&client, &uri, MULTIBYTE);
    let symbols = nested(request(&client, 2, &uri));
    let p = &symbols[0];
    // `/* 😀 */ ` is 8 characters and 9 UTF-16 units; `PROCEDURE ` adds 10.
    assert_eq!(p.range.start, Position::new(0, 9));
    assert_eq!(p.selection_range.start, Position::new(0, 19));
    finish(&client, handle);
}

#[test]
fn ranges_follow_utf8_on_a_line_with_multibyte_text() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF8, true);
    let uri = Uri::from_str("file:///utf8.p").unwrap();
    open(&client, &uri, MULTIBYTE);
    let symbols = nested(request(&client, 2, &uri));
    let p = &symbols[0];
    // `/* 😀 */ ` is 11 bytes.
    assert_eq!(p.range.start, Position::new(0, 11));
    assert_eq!(p.selection_range.start, Position::new(0, 21));
    finish(&client, handle);
}

#[test]
fn an_edit_changes_the_answer() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///edit.p").unwrap();
    open(&client, &uri, "PROCEDURE a:\nEND PROCEDURE.\n");
    assert_eq!(nested(request(&client, 2, &uri))[0].name, "a");
    replace(&client, &uri, 2, "PROCEDURE b:\nEND PROCEDURE.\n");
    assert_eq!(nested(request(&client, 3, &uri))[0].name, "b");
    finish(&client, handle);
}

// A client that repeats a version number must not get the old outline.
#[test]
fn an_edit_that_repeats_the_version_changes_the_answer() {
    let (client, handle) = start();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///same-version.p").unwrap();
    open(&client, &uri, "PROCEDURE a:\nEND PROCEDURE.\n");
    assert_eq!(nested(request(&client, 2, &uri))[0].name, "a");
    replace(&client, &uri, 1, "PROCEDURE b:\nEND PROCEDURE.\n");
    assert_eq!(nested(request(&client, 3, &uri))[0].name, "b");
    finish(&client, handle);
}

// LSP breaks lines at `\n`, `\r\n` and `\r` only. These characters are line
// breaks to a Unicode-aware rope, but not to a client.
const NOT_LINE_BREAKS: [char; 4] = ['\u{2028}', '\u{2029}', '\u{0085}', '\u{000C}'];

#[test]
fn output_positions_ignore_unicode_line_breaks() {
    for c in NOT_LINE_BREAKS {
        let (client, handle) = start();
        handshake(&client, PositionEncodingKind::UTF16, true);
        let uri = Uri::from_str("file:///unicode-lines.p").unwrap();
        open(
            &client,
            &uri,
            &format!("MESSAGE \"a{c}b\".\nPROCEDURE p:\nEND PROCEDURE.\n"),
        );
        let symbols = nested(request(&client, 2, &uri));
        assert_eq!(symbols[0].name, "p", "U+{:04X}", c as u32);
        assert_eq!(
            symbols[0].range.start,
            Position::new(1, 0),
            "U+{:04X}",
            c as u32
        );
        assert_eq!(
            symbols[0].selection_range.start,
            Position::new(1, 10),
            "U+{:04X}",
            c as u32
        );
        finish(&client, handle);
    }
}

#[test]
fn edit_ranges_ignore_unicode_line_breaks() {
    for c in NOT_LINE_BREAKS {
        let (client, handle) = start();
        handshake(&client, PositionEncodingKind::UTF16, true);
        let uri = Uri::from_str("file:///unicode-edit.p").unwrap();
        open(
            &client,
            &uri,
            &format!("MESSAGE \"a{c}b\".\nPROCEDURE p:\nEND PROCEDURE.\n"),
        );
        // Rename `p` on line 1 to `q`.
        notify(
            &client,
            "textDocument/didChange",
            serde_json::to_value(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_types::Range::new(
                        Position::new(1, 10),
                        Position::new(1, 11),
                    )),
                    range_length: None,
                    text: "q".to_string(),
                }],
            })
            .unwrap(),
        );
        let symbols = nested(request(&client, 2, &uri));
        assert_eq!(symbols[0].name, "q", "U+{:04X}", c as u32);
        finish(&client, handle);
    }
}

#[test]
fn crlf_and_lone_cr_still_break_lines() {
    for eol in ["\r\n", "\r"] {
        let (client, handle) = start();
        handshake(&client, PositionEncodingKind::UTF16, true);
        let uri = Uri::from_str("file:///eol.p").unwrap();
        open(
            &client,
            &uri,
            &format!("MESSAGE \"a\".{eol}PROCEDURE p:{eol}END PROCEDURE.{eol}"),
        );
        let symbols = nested(request(&client, 2, &uri));
        assert_eq!(symbols[0].range.start, Position::new(1, 0), "{eol:?}");
        finish(&client, handle);
    }
}

// The server runs on an analysis stack, as it does in the daemon.
#[test]
fn a_deeply_nested_document_gets_an_answer_and_the_server_survives() {
    let (server, client) = Connection::memory();
    let handle = oxabl_common::stack::builder()
        .spawn(move || oxabl_lsp::serve(&server))
        .unwrap();
    handshake(&client, PositionEncodingKind::UTF16, true);
    let uri = Uri::from_str("file:///deep.p").unwrap();
    let depth = 5000;
    let mut text = "IF TRUE THEN DO:\n".repeat(depth);
    text.push_str("DEFINE VARIABLE v AS INTEGER NO-UNDO.\n");
    text.push_str(&"END.\n".repeat(depth));
    text.push_str("PROCEDURE p:\nEND PROCEDURE.\n");
    open(&client, &uri, &text);
    let response = request(&client, 2, &uri);
    assert!(response.error.is_none(), "{response:?}");
    // The server answers the next request too.
    let next = Uri::from_str("file:///after.p").unwrap();
    open(&client, &next, "PROCEDURE q:\nEND PROCEDURE.\n");
    assert_eq!(nested(request(&client, 3, &next))[0].name, "q");
    finish(&client, handle);
}
