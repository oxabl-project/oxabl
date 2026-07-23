//! U7 end-to-end: an `oxabl.toml` `[workspace.lint]` setting reaches the
//! diagnostics the LSP publishes (here, turning `unused-variable` off).

use std::str::FromStr;
use std::thread;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, GeneralClientCapabilities, InitializeParams,
    NumberOrString, PositionEncodingKind, PublishDiagnosticsParams, TextDocumentItem, Uri,
};
use tempfile::TempDir;

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

fn recv_publish(client: &Connection) -> PublishDiagnosticsParams {
    loop {
        if let Message::Notification(n) = client.receiver.recv().unwrap()
            && n.method == "textDocument/publishDiagnostics"
        {
            return serde_json::from_value(n.params).unwrap();
        }
    }
}

#[test]
fn lint_table_turns_off_unused_variable() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.lint]\nunused-variable = \"off\"\n",
    )
    .unwrap();
    let file = tmp.path().join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(50)));
    handshake(&client);

    let uri = Uri::from_str(&format!("file://{}", file.display())).unwrap();
    client
        .sender
        .send(Message::Notification(Notification {
            method: "textDocument/didOpen".to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abl".to_string(),
                    version: 1,
                    text: "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n".to_string(),
                },
            })
            .unwrap(),
        }))
        .unwrap();

    let published = recv_publish(&client);
    assert!(
        !published
            .diagnostics
            .iter()
            .any(|d| d.code == Some(NumberOrString::String("LINT0002".to_string()))),
        "unused-variable = off must suppress LINT0002, got {:?}",
        published.diagnostics
    );

    // shutdown
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
    client
        .sender
        .send(Message::Notification(Notification {
            method: "exit".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    assert!(handle.join().unwrap().unwrap());
}
