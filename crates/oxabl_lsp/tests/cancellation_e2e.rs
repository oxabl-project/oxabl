//! End-to-end: an edit in one buffer must not leave *another* buffer showing
//! stale diagnostics.
//!
//! Salsa cancellation is global — a write to any buffer's input flags every live
//! snapshot — so editing file A cancels file B's in-flight debounced computation
//! even though B never changed. B's own version is unchanged and its timer was
//! consumed when the worker was spawned, so unless the cancelled arm re-arms the
//! timer, B's edit never produces a publish and the editor keeps rendering
//! pre-edit diagnostics until the user touches B again.
//!
//! The forcing is probabilistic in *whether the cancellation happens*, never in
//! the assertion: B is large enough that its computation spans several of the
//! rapid A edits that follow, and if no cancellation happened the server
//! publishes B on the ordinary path and the test still passes. Only the broken
//! behavior — a dropped cancelled result — can fail it.

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

const WINDOW: Duration = Duration::from_millis(40);

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

fn did_open(client: &Connection, uri: &Uri, text: &str) {
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

fn did_change(client: &Connection, uri: &Uri, version: i32, text: &str) {
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

/// Wait for a `publishDiagnostics` for `uri` at `version`, ignoring every other
/// notification, until `timeout` elapses.
fn await_publish(
    client: &Connection,
    uri: &Uri,
    version: i32,
    timeout: Duration,
) -> Option<PublishDiagnosticsParams> {
    let deadline = std::time::Instant::now() + timeout;
    loop {
        let now = std::time::Instant::now();
        if now >= deadline {
            return None;
        }
        match client.receiver.recv_timeout(deadline - now) {
            Ok(Message::Notification(n)) if n.method == "textDocument/publishDiagnostics" => {
                let params: PublishDiagnosticsParams = serde_json::from_value(n.params).unwrap();
                if &params.uri == uri && params.version == Some(version) {
                    return Some(params);
                }
            }
            Ok(_) => continue,
            Err(_) => return None,
        }
    }
}

/// A body big enough that one computation spans several main-loop writes.
/// Synthetic ABL: N declarations, each read once, so the baseline is clean.
fn large_body(lines: usize) -> String {
    let mut s = String::new();
    for i in 0..lines {
        s.push_str(&format!("DEFINE VARIABLE v{i} AS INTEGER NO-UNDO.\n"));
    }
    for i in 0..lines {
        s.push_str(&format!("MESSAGE v{i}.\n"));
    }
    s
}

#[test]
fn an_edit_in_another_buffer_does_not_leave_this_one_stale() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client);

    let a = Uri::from_str("file:///churn.p").unwrap();
    let b = Uri::from_str("file:///quiet.p").unwrap();

    let a_text = "DEFINE VARIABLE a AS INTEGER NO-UNDO.\nMESSAGE a.\n";
    let b_clean = large_body(400);
    // The edit adds one *unused* variable, so the awaited publish is verifiable
    // by content and not only by version.
    let b_edited = format!("{b_clean}DEFINE VARIABLE stale AS INTEGER NO-UNDO.\n");

    did_open(&client, &a, a_text);
    did_open(&client, &b, &b_clean);
    assert!(
        await_publish(&client, &b, 1, Duration::from_secs(10)).is_some(),
        "the open of the quiet buffer publishes"
    );

    // Edit B once (version 2), then churn A while B's debounced worker runs.
    // Every A edit writes a salsa input on the main loop, which cancels B's
    // in-flight snapshot read.
    did_change(&client, &b, 2, &b_edited);
    for step in 0..40 {
        thread::sleep(Duration::from_millis(5));
        did_change(
            &client,
            &a,
            step + 2,
            &format!("DEFINE VARIABLE a AS INTEGER NO-UNDO.\nMESSAGE a. /* {step} */\n"),
        );
    }

    // B was edited once and never touched again: it must still receive fresh
    // diagnostics for that edit.
    let published = await_publish(&client, &b, 2, Duration::from_secs(20))
        .expect("the cancelled buffer must be recomputed and published without another edit");
    assert!(
        published
            .diagnostics
            .iter()
            .any(|d| d.code == Some(NumberOrString::String("LINT0002".to_string()))),
        "the republished set must reflect the edit (the unused `stale` variable)"
    );

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
