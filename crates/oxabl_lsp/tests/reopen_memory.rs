//! A document closed and opened again does not grow the server's memory.
//!
//! Salsa cannot remove an input, and a closed buffer's analysis results stayed
//! in memory for good, so each reopen of a document added a copy of its text,
//! expansion and diagnostics: an editor session grew without end. The server
//! runs in this process, so the process's resident memory is the server's.

#![cfg(target_os = "linux")]

use std::fmt::Write as _;
use std::str::FromStr;
use std::thread;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GeneralClientCapabilities, InitializeParams, PositionEncodingKind, PublishDiagnosticsParams,
    TextDocumentIdentifier, TextDocumentItem, Uri,
};

/// Open and close cycles before the first measure, so caches and the
/// allocator settle.
const WARM_UP: usize = 20;
/// Open and close cycles between the two measures.
const CYCLES: usize = 120;
/// Growth allowed over [`CYCLES`]: far below one kept copy per cycle.
const ALLOWED_GROWTH_KIB: u64 = 16 * 1024;

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

fn resident_kib() -> u64 {
    let status = std::fs::read_to_string("/proc/self/status").unwrap();
    let line = status
        .lines()
        .find(|line| line.starts_with("VmRSS:"))
        .unwrap();
    line.split_whitespace().nth(1).unwrap().parse().unwrap()
}

/// Open the document, wait for its analysis, close it and wait for the clear.
fn cycle(client: &Connection, uri: &Uri, text: &str, version: i32) {
    notify(
        client,
        "textDocument/didOpen",
        serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "abl".to_string(),
                version,
                text: text.to_string(),
            },
        })
        .unwrap(),
    );
    let published = recv_publish(client);
    assert!(
        !published.diagnostics.is_empty(),
        "expected the unused-variable findings"
    );
    notify(
        client,
        "textDocument/didClose",
        serde_json::to_value(DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
        })
        .unwrap(),
    );
    let cleared = recv_publish(client);
    assert!(
        cleared.diagnostics.is_empty(),
        "close must clear diagnostics"
    );
}

/// Memory growth over [`CYCLES`] open and close cycles, after [`WARM_UP`]
/// cycles; `uri` names the document of each cycle.
fn growth_kib(client: &Connection, text: &str, uri: impl Fn(usize) -> Uri) -> (u64, u64) {
    for n in 0..WARM_UP {
        cycle(client, &uri(n), text, n as i32);
    }
    let before = resident_kib();
    for n in WARM_UP..WARM_UP + CYCLES {
        cycle(client, &uri(n), text, n as i32);
    }
    (before, resident_kib())
}

#[test]
fn closing_documents_does_not_grow_memory() {
    let mut text = String::new();
    for i in 0..2000 {
        writeln!(
            text,
            "DEFINE VARIABLE v{i} AS CHARACTER NO-UNDO INITIAL \"{i:0>40}\"."
        )
        .unwrap();
    }

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve(&server));
    handshake(&client);

    // One document opened again and again, then many documents opened once.
    let same = growth_kib(&client, &text, |_| {
        Uri::from_str("file:///reopened.p").unwrap()
    });
    let distinct = growth_kib(&client, &text, |n| {
        Uri::from_str(&format!("file:///opened-{n}.p")).unwrap()
    });

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
    for (what, (before, after)) in [
        ("one document reopened", same),
        ("distinct documents", distinct),
    ] {
        let growth = after.saturating_sub(before);
        assert!(
            growth <= ALLOWED_GROWTH_KIB,
            "{what}: memory grew by {growth} KiB over {CYCLES} cycles ({before} to {after} KiB), \
             more than {ALLOWED_GROWTH_KIB} KiB"
        );
    }
}
