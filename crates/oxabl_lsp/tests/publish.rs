//! U5 end-to-end: an LSP client opens a known-bad file and receives
//! `publishDiagnostics` at the expected range; closing clears them.

use std::thread;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GeneralClientCapabilities, InitializeParams, NumberOrString, PositionEncodingKind,
    PublishDiagnosticsParams, TextDocumentIdentifier, TextDocumentItem, Uri,
};
use std::str::FromStr;

fn handshake(client: &Connection) {
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
    loop {
        if let Message::Response(_) = client.receiver.recv().unwrap() {
            break;
        }
    }
    client
        .sender
        .send(Message::Notification(Notification {
            method: "initialized".to_string(),
            params: serde_json::json!({}),
        }))
        .unwrap();
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

fn recv_publish(client: &Connection) -> PublishDiagnosticsParams {
    loop {
        match client.receiver.recv().unwrap() {
            Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => {
                return serde_json::from_value(n.params).unwrap();
            }
            _ => continue,
        }
    }
}

fn shutdown(client: &Connection) {
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(99),
            method: "shutdown".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    loop {
        if let Message::Response(_) = client.receiver.recv().unwrap() {
            break;
        }
    }
    notify(client, "exit", serde_json::Value::Null);
}

#[test]
fn open_known_bad_file_publishes_diagnostics_then_close_clears() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));

    handshake(&client);

    let uri = Uri::from_str("file:///unused.p").unwrap();
    // An unused variable → LINT0002 (warning) on line 0.
    notify(
        &client,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "abl".to_string(),
                version: 1,
                text: "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
            },
        })
        .unwrap(),
    );

    let published = recv_publish(&client);
    assert_eq!(published.uri, uri);
    let lint = published
        .diagnostics
        .iter()
        .find(|d| d.code == Some(NumberOrString::String("LINT0002".to_string())))
        .expect("expected an unused-variable diagnostic");
    // The diagnostic points at the variable on line 0 (0-indexed).
    assert_eq!(lint.range.start.line, 0);
    assert_eq!(lint.source.as_deref(), Some("oxabl"));

    // Closing the document clears diagnostics (empty publish).
    notify(
        &client,
        "textDocument/didClose",
        serde_json::to_value(DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
        })
        .unwrap(),
    );
    let cleared = recv_publish(&client);
    assert_eq!(cleared.uri, uri);
    assert!(
        cleared.diagnostics.is_empty(),
        "close must clear diagnostics, got {:?}",
        cleared.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}
