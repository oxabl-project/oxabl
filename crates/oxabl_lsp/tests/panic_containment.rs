//! A panic inside analysis must not kill the language server.
//!
//! The two diagnostics paths — `compute_and_publish` on the **main loop** and
//! the debounced **worker** thread — each wrap `compute_diagnostics` *and*
//! `buffer_dependencies` in one `catch_panic`, so a panic in either query
//! degrades to "no diagnostics, no trustworthy dependency set" while the server
//! keeps serving — and the buffer's previously recorded dependencies stand,
//! since a panic says nothing about what the file includes.
//! The formatting path degrades to zero edits, never a partial rewrite.
//!
//! No ABL input panics, so every panic here is injected: `oxabl_common`'s
//! test-only `test-panics` feature (enabled through this crate's
//! dev-dependencies) makes a guarded site panic when the source carries
//! `OXABL-TEST-PANIC:<site>` in a comment. All fixtures are synthetic ABL.

use std::str::FromStr;
use std::thread;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DocumentFormattingParams, FormattingOptions, GeneralClientCapabilities, InitializeParams,
    NumberOrString, PositionEncodingKind, PublishDiagnosticsParams, TextDocumentContentChangeEvent,
    TextDocumentIdentifier, TextDocumentItem, TextEdit, Uri, VersionedTextDocumentIdentifier,
};
use oxabl_common::panic_sites;

const WINDOW: Duration = Duration::from_millis(80);
const WAIT: Duration = Duration::from_secs(2);

/// A buffer that panics at `site`, wrapped in an ABL block comment so the text
/// is otherwise ordinary source. The unused variable would yield LINT0002 if
/// analysis ever completed, which is what makes "no diagnostics arrived" a
/// meaningful assertion.
fn panicking_source(site: &str) -> String {
    format!("/* OXABL-TEST-PANIC:{site} */\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n")
}

/// A buffer with an unused variable and no marker → one LINT0002.
const GOOD_SOURCE: &str = "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n";

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

fn change(client: &Connection, uri: &Uri, version: i32, text: &str) {
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

/// Send `shutdown` and `exit`, returning whether the server saw the shutdown —
/// which is only possible if the main loop was still alive to answer it.
fn shutdown(client: &Connection) {
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(99),
            method: "shutdown".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    let deadline = std::time::Instant::now() + WAIT;
    loop {
        let now = std::time::Instant::now();
        assert!(now < deadline, "server never answered `shutdown`");
        if let Ok(Message::Response(_)) = client.receiver.recv_timeout(deadline - now) {
            break;
        }
    }
    notify(client, "exit", serde_json::Value::Null);
}

fn has_lint(published: &PublishDiagnosticsParams, code: &str) -> bool {
    published
        .diagnostics
        .iter()
        .any(|d| d.code == Some(NumberOrString::String(code.to_string())))
}

/// Wait for a publish for `uri` specifically, skipping publishes for other
/// buffers. `None` means none arrived inside the timeout.
fn recv_publish_for(
    client: &Connection,
    uri: &Uri,
    timeout: Duration,
) -> Option<PublishDiagnosticsParams> {
    let deadline = std::time::Instant::now() + timeout;
    loop {
        let remaining = deadline.checked_duration_since(std::time::Instant::now())?;
        let published = recv_publish(client, remaining)?;
        if &published.uri == uri {
            return Some(published);
        }
    }
}

/// The main-loop path: `didOpen` publishes synchronously on the loop, so an
/// unguarded panic there takes the whole server down. The server must instead
/// keep serving — proven by a second buffer still getting diagnostics and by
/// `shutdown` still being answered.
#[test]
fn main_loop_survives_a_panic_during_diagnostics() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));

    handshake(&client);

    let bad = Uri::from_str("file:///panic-open.p").unwrap();
    open(
        &client,
        &bad,
        &panicking_source(panic_sites::LSP_DIAGNOSTICS),
    );

    // A subsequent buffer is still analyzed and published: the loop is alive.
    let good = Uri::from_str("file:///after-panic.p").unwrap();
    open(&client, &good, GOOD_SOURCE);

    // The *first* publish must be the healthy buffer's. The server handles
    // notifications in order, so a publish for `bad` would mean the injected
    // panic never fired and this test proves nothing.
    let published =
        recv_publish(&client, WAIT).expect("the server must still publish after a panic");
    assert_eq!(
        published.uri, good,
        "the panicking buffer must not publish diagnostics"
    );
    assert!(
        has_lint(&published, "LINT0002"),
        "expected LINT0002 on the healthy buffer, got {:?}",
        published.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// The same containment for `buffer_dependencies`, which runs one line past
/// `compute_diagnostics`. A guard that only spanned the first call would let
/// this panic through. Its own `Cancelled::catch` is not a substitute: that
/// catch resumes any non-`Cancelled` unwind, so a genuine panic there is still a
/// panic and still needs this guard.
#[test]
fn main_loop_survives_a_panic_in_buffer_dependencies() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));

    handshake(&client);

    let bad = Uri::from_str("file:///panic-deps.p").unwrap();
    open(
        &client,
        &bad,
        &panicking_source(panic_sites::LSP_DEPENDENCIES),
    );

    let good = Uri::from_str("file:///after-deps-panic.p").unwrap();
    open(&client, &good, GOOD_SOURCE);
    // A dependencies panic aborts the guarded closure before the diagnostics it
    // computed can be published, so again the first publish must be `good`'s.
    let published =
        recv_publish(&client, WAIT).expect("a dependencies panic must not kill the loop either");
    assert_eq!(published.uri, good);
    assert!(has_lint(&published, "LINT0002"));

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// The debounced worker path: a panic there must not kill the worker thread, and
/// the guarded closure must still `send`, or that buffer's diagnostics stall
/// forever with no timeout. Other buffers must keep publishing.
#[test]
fn worker_keeps_publishing_other_buffers_after_a_panic() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));

    handshake(&client);

    // Open clean, then edit *into* a panicking state so the panic happens on the
    // debounced worker path rather than the immediate open path.
    let bad = Uri::from_str("file:///panic-worker.p").unwrap();
    open(&client, &bad, GOOD_SOURCE);
    recv_publish_for(&client, &bad, WAIT).expect("the clean open publishes");
    change(
        &client,
        &bad,
        2,
        &panicking_source(panic_sites::LSP_DIAGNOSTICS),
    );

    // The panicking buffer gets no diagnostics publish — the guarded closure
    // still sends, but with `None` diagnostics — while the server is otherwise
    // unaffected.
    let good = Uri::from_str("file:///worker-sibling.p").unwrap();
    open(&client, &good, GOOD_SOURCE);
    let published = recv_publish(&client, WAIT)
        .expect("a worker panic must not stop other buffers from publishing");
    assert_eq!(
        published.uri, good,
        "the panicking buffer's debounced recompute must not publish"
    );
    assert!(has_lint(&published, "LINT0002"));
    // Nothing further arrives for the panicking buffer: it neither publishes
    // stale results nor retries forever.
    assert!(
        recv_publish_for(&client, &bad, WINDOW * 4).is_none(),
        "the panicking buffer must not publish"
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// The worker snapshot shares salsa storage with the main database, so a panic
/// unwinding out of a query could leave that buffer's memo mid-computation and
/// re-panic on every later request. Editing the *same* buffer back to healthy
/// text must therefore either succeed or fail cleanly — never wedge.
#[test]
fn the_panicking_buffer_recovers_when_edited_back() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));

    handshake(&client);

    let uri = Uri::from_str("file:///panic-then-heal.p").unwrap();
    open(
        &client,
        &uri,
        &panicking_source(panic_sites::LSP_DIAGNOSTICS),
    );

    // Edit the marker away; the debounced recompute must now succeed.
    change(&client, &uri, 2, GOOD_SOURCE);
    let published = recv_publish_for(&client, &uri, WAIT)
        .expect("the same buffer must analyze again after its panic");
    assert!(
        has_lint(&published, "LINT0002"),
        "expected the healed buffer's diagnostics, got {:?}",
        published.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// The never-mangle contract through the migration: a formatter panic yields an
/// **empty** edit list, not a partial rewrite.
#[test]
fn formatting_returns_no_edits_when_the_formatter_panics() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));

    handshake(&client);

    let uri = Uri::from_str("file:///panic-format.p").unwrap();
    // Mis-indented *and* marked: without the marker this buffer would produce a
    // real whole-document edit, so an empty result can only come from the guard.
    let text = format!(
        "/* OXABL-TEST-PANIC:{} */\nIF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n",
        panic_sites::FORMAT
    );
    open(&client, &uri, &text);

    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(7),
            method: "textDocument/formatting".to_string(),
            params: serde_json::to_value(DocumentFormattingParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                options: FormattingOptions::default(),
                work_done_progress_params: Default::default(),
            })
            .unwrap(),
        }))
        .unwrap();

    let deadline = std::time::Instant::now() + WAIT;
    let edits: Vec<TextEdit> = loop {
        let now = std::time::Instant::now();
        assert!(now < deadline, "no formatting response arrived");
        match client.receiver.recv_timeout(deadline - now) {
            Ok(Message::Response(resp)) if resp.id == RequestId::from(7) => {
                assert!(resp.error.is_none(), "formatting must not error: {resp:?}");
                break serde_json::from_value(resp.result.unwrap_or(serde_json::Value::Null))
                    .unwrap_or_default();
            }
            Ok(_) => continue,
            Err(_) => panic!("no formatting response arrived"),
        }
    };
    assert!(
        edits.is_empty(),
        "a formatter panic must yield zero edits, got {edits:?}"
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}
