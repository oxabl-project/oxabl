//! U6 end-to-end: a burst of `didChange` collapses to exactly one debounced
//! publish reflecting the final text; no stale intermediate publishes.

use std::str::FromStr;
use std::thread;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    GeneralClientCapabilities, InitializeParams, NumberOrString, PositionEncodingKind,
    PublishDiagnosticsParams, TextDocumentContentChangeEvent, TextDocumentItem, Uri,
    VersionedTextDocumentIdentifier,
};

const WINDOW: Duration = Duration::from_millis(80);

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
    notify(client, "initialized", serde_json::json!({}));
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

fn recv_publish(client: &Connection, timeout: Duration) -> Option<PublishDiagnosticsParams> {
    let deadline = std::time::Instant::now() + timeout;
    loop {
        let now = std::time::Instant::now();
        if now >= deadline {
            return None;
        }
        match client.receiver.recv_timeout(deadline - now) {
            Ok(Message::Notification(n)) if n.method == "textDocument/publishDiagnostics" => {
                return Some(serde_json::from_value(n.params).unwrap());
            }
            Ok(_) => continue,
            Err(_) => return None,
        }
    }
}

fn full_change(text: &str) -> TextDocumentContentChangeEvent {
    TextDocumentContentChangeEvent {
        range: None,
        range_length: None,
        text: text.to_string(),
    }
}

#[test]
fn edit_burst_collapses_to_single_final_publish() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));

    handshake(&client);

    let uri = Uri::from_str("file:///burst.p").unwrap();
    // Open with an unused variable → LINT0002; the open publish is immediate.
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
    let open_pub = recv_publish(&client, Duration::from_secs(2)).expect("open publishes");
    assert!(
        open_pub
            .diagnostics
            .iter()
            .any(|d| d.code == Some(NumberOrString::String("LINT0002".to_string()))),
        "open buffer has the unused-variable diagnostic"
    );

    // Rapid burst of edits (versions 2..5), all within one debounce window.
    // The final text *reads* x, so the unused-variable diagnostic must clear.
    let steps = [
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMES",
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE",
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x",
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nMESSAGE x.\n",
    ];
    for (i, text) in steps.iter().enumerate() {
        notify(
            &client,
            "textDocument/didChange",
            serde_json::to_value(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: (i as i32) + 2,
                },
                content_changes: vec![full_change(text)],
            })
            .unwrap(),
        );
    }

    // Exactly one debounced publish should arrive, reflecting the final text.
    let debounced = recv_publish(&client, Duration::from_secs(2)).expect("debounced publish");
    assert_eq!(
        debounced.version,
        Some(5),
        "reflects the final edit version"
    );
    assert!(
        !debounced
            .diagnostics
            .iter()
            .any(|d| d.code == Some(NumberOrString::String("LINT0002".to_string()))),
        "final text reads x, so no unused-variable diagnostic: {:?}",
        debounced.diagnostics
    );

    // No further (stale) publishes for the superseded intermediate versions.
    let extra = recv_publish(&client, WINDOW * 3);
    assert!(
        extra.is_none(),
        "burst must yield exactly one publish, got an extra: {extra:?}"
    );

    // Clean shutdown.
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
    notify(&client, "exit", serde_json::Value::Null);
    assert!(handle.join().unwrap().unwrap());
}
