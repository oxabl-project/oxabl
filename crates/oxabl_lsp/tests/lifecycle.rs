//! U1 lifecycle tests: handshake capability advertisement, position-encoding
//! negotiation end-to-end, and clean vs. unclean shutdown — driven over an
//! in-memory `lsp-server` connection (no real editor / stdio).

use std::thread;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, GeneralClientCapabilities, InitializeParams, InitializeResult,
    PositionEncodingKind, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

/// Drive the client half of the initialize handshake and return the server's
/// advertised capabilities. Leaves the connection ready for the caller to send
/// lifecycle messages.
fn do_handshake(
    client: &Connection,
    position_encodings: Option<Vec<PositionEncodingKind>>,
) -> ServerCapabilities {
    let params = InitializeParams {
        capabilities: ClientCapabilities {
            general: Some(GeneralClientCapabilities {
                position_encodings,
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

    // The InitializeResult response comes back before `initialized` is sent.
    let response = loop {
        match client.receiver.recv().unwrap() {
            Message::Response(r) => break r,
            _ => continue,
        }
    };
    let result: InitializeResult = serde_json::from_value(response.result.unwrap()).unwrap();

    client
        .sender
        .send(Message::Notification(Notification {
            method: "initialized".to_string(),
            params: serde_json::json!({}),
        }))
        .unwrap();

    result.capabilities
}

#[test]
fn handshake_advertises_only_incremental_sync_and_push_diagnostics() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));

    let caps = do_handshake(&client, Some(vec![PositionEncodingKind::UTF8]));

    // Incremental sync with open/close.
    match caps.text_document_sync {
        Some(TextDocumentSyncCapability::Options(opts)) => {
            assert_eq!(opts.open_close, Some(true));
            assert_eq!(opts.change, Some(TextDocumentSyncKind::INCREMENTAL));
        }
        other => panic!("expected incremental sync options, got {other:?}"),
    }
    // Push diagnostics need no capability; pull diagnostics must NOT be offered.
    assert!(caps.diagnostic_provider.is_none());
    // No other feature advertised.
    assert!(caps.hover_provider.is_none());
    assert!(caps.completion_provider.is_none());
    assert!(caps.definition_provider.is_none());
    assert!(caps.references_provider.is_none());
    assert!(caps.rename_provider.is_none());

    // Clean shutdown.
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(2),
            method: "shutdown".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    let shutdown_resp = loop {
        match client.receiver.recv().unwrap() {
            Message::Response(r) => break r,
            _ => continue,
        }
    };
    assert_eq!(shutdown_resp.id, RequestId::from(2));
    assert!(shutdown_resp.error.is_none());
    assert_eq!(shutdown_resp.result, Some(serde_json::Value::Null));
    client
        .sender
        .send(Message::Notification(Notification {
            method: "exit".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();

    let clean = handle.join().unwrap().unwrap();
    assert!(clean, "shutdown-before-exit is a clean shutdown");
}

#[test]
fn negotiates_utf8_when_offered_utf16_otherwise() {
    // UTF-8 offered → UTF-8 selected.
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));
    let caps = do_handshake(
        &client,
        Some(vec![
            PositionEncodingKind::UTF8,
            PositionEncodingKind::UTF16,
        ]),
    );
    assert_eq!(caps.position_encoding, Some(PositionEncodingKind::UTF8));
    shutdown(&client, 2);
    assert!(handle.join().unwrap().unwrap());

    // Only UTF-16 offered → UTF-16.
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));
    let caps = do_handshake(&client, Some(vec![PositionEncodingKind::UTF16]));
    assert_eq!(caps.position_encoding, Some(PositionEncodingKind::UTF16));
    shutdown(&client, 2);
    assert!(handle.join().unwrap().unwrap());

    // Field omitted → UTF-16.
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));
    let caps = do_handshake(&client, None);
    assert_eq!(caps.position_encoding, Some(PositionEncodingKind::UTF16));
    shutdown(&client, 2);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn exit_without_shutdown_is_unclean() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));
    let _ = do_handshake(&client, None);

    // Exit with no prior shutdown.
    client
        .sender
        .send(Message::Notification(Notification {
            method: "exit".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();

    let clean = handle.join().unwrap().unwrap();
    assert!(!clean, "exit without shutdown must be reported as unclean");
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
        match client.receiver.recv().unwrap() {
            Message::Response(_) => break,
            _ => continue,
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
