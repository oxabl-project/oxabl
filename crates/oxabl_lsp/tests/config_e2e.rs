//! End-to-end: one resolution of `oxabl.toml` reaches everything the server
//! does with it — a `[workspace.lint]` setting reaches the published diagnostics
//! (here, turning `unused-variable` off), and a *malformed* file reaches the
//! client's log instead of being swallowed.

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

/// Every `window/logMessage` the server sent, drained until the first
/// `publishDiagnostics` arrives — config is resolved before the open publishes,
/// so a warning that is going to appear has appeared by then.
fn logs_until_first_publish(client: &Connection) -> Vec<String> {
    let mut logs = Vec::new();
    loop {
        match client.receiver.recv().unwrap() {
            Message::Notification(n) if n.method == "window/logMessage" => {
                logs.push(n.params["message"].as_str().unwrap_or_default().to_string());
            }
            Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => {
                return logs;
            }
            _ => {}
        }
    }
}

/// R7: a malformed `oxabl.toml` is surfaced, not swallowed.
///
/// The pre-refactor server called three `resolved_*` helpers and dropped every
/// error slot, so the editor degraded to default configuration in silence while
/// the CLI printed a `warning:` line for the same file. It now logs — **once**
/// per resolution rather than once per configuration surface, which is what
/// resolving the file a single time buys (KTD3).
#[test]
fn malformed_oxabl_toml_is_logged_once_to_the_client() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(tmp.path().join("oxabl.toml"), "this is not valid toml {{{").unwrap();
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

    let logs = logs_until_first_publish(&client);
    let about_config: Vec<&String> = logs.iter().filter(|m| m.contains("oxabl.toml")).collect();
    assert_eq!(
        about_config.len(),
        1,
        "expected exactly one config warning, got {logs:?}"
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// A well-formed config produces no warning at all: an unconfigured or
/// correctly-configured project must not chatter in the editor's log.
#[test]
fn a_valid_config_logs_nothing() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.style]\nindent_size = 2\n",
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

    assert!(
        logs_until_first_publish(&client).is_empty(),
        "a valid config must not warn"
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
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
    client
        .sender
        .send(Message::Notification(Notification {
            method: "exit".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
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

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}
