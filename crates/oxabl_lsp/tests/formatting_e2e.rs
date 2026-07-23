//! U4 end-to-end: `textDocument/formatting` driven through the real server
//! message loop over an in-memory connection — capability advertisement, the
//! success round-trip and its idempotence, the unopened-URI tolerance (R6), and
//! survival of a bail-inducing buffer (R7). Plus the `oxabl.toml [workspace.style]`
//! discovery path (R3) end-to-end.
//!
//! All fixtures are synthetic ABL.

use std::str::FromStr;
use std::thread;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, DocumentFormattingParams, FormattingOptions,
    GeneralClientCapabilities, InitializeParams, InitializeResult, OneOf, PositionEncodingKind,
    TextDocumentIdentifier, TextDocumentItem, TextEdit, Uri,
};

/// Drive the initialize handshake and return the server's advertised
/// capabilities, negotiating UTF-8.
fn handshake(client: &Connection) -> InitializeResult {
    let params = InitializeParams {
        capabilities: ClientCapabilities {
            general: Some(GeneralClientCapabilities {
                position_encodings: Some(vec![PositionEncodingKind::UTF8]),
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
    let response = loop {
        if let Message::Response(r) = client.receiver.recv().unwrap() {
            break r;
        }
    };
    client
        .sender
        .send(Message::Notification(Notification {
            method: "initialized".to_string(),
            params: serde_json::json!({}),
        }))
        .unwrap();
    serde_json::from_value(response.result.unwrap()).unwrap()
}

fn did_open(client: &Connection, uri: &Uri, text: &str) {
    client
        .sender
        .send(Message::Notification(Notification {
            method: "textDocument/didOpen".to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abl".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            })
            .unwrap(),
        }))
        .unwrap();
}

/// Send a `textDocument/formatting` request and return the edits from the
/// matching response (ignoring interleaved notifications such as
/// `publishDiagnostics` and the server's `client/registerCapability` request).
fn format(client: &Connection, id: i32, uri: &Uri) -> Vec<TextEdit> {
    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            ..Default::default()
        },
        work_done_progress_params: Default::default(),
    };
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(id),
            method: "textDocument/formatting".to_string(),
            params: serde_json::to_value(params).unwrap(),
        }))
        .unwrap();
    let response = loop {
        if let Message::Response(r) = client.receiver.recv().unwrap()
            && r.id == RequestId::from(id)
        {
            break r;
        }
    };
    assert!(
        response.error.is_none(),
        "formatting must not error: {response:?}"
    );
    // `null` result (no edits) and `[]` both mean "no edits".
    match response.result {
        Some(serde_json::Value::Null) | None => Vec::new(),
        Some(v) => serde_json::from_value(v).unwrap(),
    }
}

fn shutdown(client: &Connection, id: i32) {
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(id),
            method: "shutdown".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    loop {
        if let Message::Response(r) = client.receiver.recv().unwrap()
            && r.id == RequestId::from(id)
        {
            break;
        }
    }
    client
        .sender
        .send(Message::Notification(Notification {
            method: "exit".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
}

#[test]
fn initialize_advertises_document_formatting() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(20)));
    let result = handshake(&client);
    assert_eq!(
        result.capabilities.document_formatting_provider,
        Some(OneOf::Left(true))
    );
    assert!(
        result
            .capabilities
            .document_range_formatting_provider
            .is_none()
    );
    shutdown(&client, 99);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn formatting_roundtrip_is_correct_and_idempotent() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(20)));
    handshake(&client);

    let src = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";
    let uri = Uri::from_str("file:///buf.p").unwrap();
    did_open(&client, &uri, src);

    // First format: one whole-document edit that changes the buffer.
    let edits = format(&client, 2, &uri);
    assert_eq!(edits.len(), 1, "expected one whole-document edit");
    let formatted = &edits[0].new_text;
    assert_ne!(
        formatted, src,
        "the mis-formatted buffer must be reformatted"
    );
    assert!(!formatted.is_empty());
    // Whole-document range starts at (0,0).
    assert_eq!(edits[0].range.start.line, 0);
    assert_eq!(edits[0].range.start.character, 0);

    // Idempotence: opening the formatted output and formatting again yields no
    // edits (proves the returned text is a fixed point — formatter-safety
    // metric, end-to-end).
    let uri2 = Uri::from_str("file:///buf2.p").unwrap();
    did_open(&client, &uri2, formatted);
    let edits2 = format(&client, 3, &uri2);
    assert!(
        edits2.is_empty(),
        "already-formatted buffer must yield no edits"
    );

    shutdown(&client, 99);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn formatting_unopened_uri_returns_empty_no_error() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(20)));
    handshake(&client);

    let uri = Uri::from_str("file:///never-opened.p").unwrap();
    let edits = format(&client, 2, &uri);
    assert!(
        edits.is_empty(),
        "unopened URI → empty edits, no protocol error (R6)"
    );

    shutdown(&client, 99);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn bail_buffer_yields_no_edits_and_server_survives() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(20)));
    handshake(&client);

    // Unterminated DO → parse errors → the formatter bails → no edits.
    let bad = Uri::from_str("file:///bad.p").unwrap();
    did_open(&client, &bad, "DO:\n  MESSAGE \"hi\".\n");
    let edits = format(&client, 2, &bad);
    assert!(edits.is_empty(), "bailing buffer must yield no edits");

    // The server thread is still alive: a subsequent good format still works.
    let good = Uri::from_str("file:///good.p").unwrap();
    did_open(&client, &good, "IF TRUE THEN DO:\nMESSAGE \"ok\".\nEND.\n");
    let edits2 = format(&client, 3, &good);
    assert_eq!(
        edits2.len(),
        1,
        "server survived the bail and still formats (R7)"
    );

    shutdown(&client, 99);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn formatting_uses_workspace_style() {
    // R3 end-to-end: an `oxabl.toml [workspace.style]` under the file's
    // directory drives the applied style. Use 2-space indent (base is 4) and
    // assert the reformatted output uses two leading spaces on the nested line.
    let tmp = tempfile::TempDir::new().unwrap();
    std::fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 2\n",
    )
    .unwrap();
    let file = tmp.path().join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(20)));
    handshake(&client);

    let uri = Uri::from_str(&format!("file://{}", file.display())).unwrap();
    did_open(&client, &uri, "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n");
    let edits = format(&client, 2, &uri);
    assert_eq!(edits.len(), 1);
    let formatted = &edits[0].new_text;
    assert!(
        formatted.contains("\n  MESSAGE"),
        "2-space workspace indent must be applied, got:\n{formatted}"
    );

    shutdown(&client, 99);
    assert!(handle.join().unwrap().unwrap());
}
