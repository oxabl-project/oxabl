//! U11 end-to-end: a **dependency edit on disk** reaches the buffers that
//! resolved against it, and reaches nothing else (R8, R10).
//!
//! This is the acceptance case for the language server's incremental leg. Every
//! test here drives the real server loop over an in-memory connection and a real
//! temporary directory, because the thing under test is the whole chain: a watched
//! `.cls` event → the one per-file index input it names → salsa's dependency graph
//! → a fresh `publishDiagnostics` for the child.
//!
//! The handshake declares a **workspace root**, which is what the live editor
//! does and what lets the server resolve configuration and register its watch
//! before any document is opened.
//!
//! Fixtures are synthetic ABL: a parent class with one public method, and children
//! that call it. The observable is `LINT0001` (undefined-symbol) on the inherited
//! call — present when the parent cannot supply the member, absent when it can. A
//! stale cached answer shows up as the *absence* of a change, which is exactly what
//! each test refuses to accept.

use std::path::Path;
use std::str::FromStr;
use std::thread;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, FileChangeType, FileEvent,
    GeneralClientCapabilities, InitializeParams, NumberOrString, PositionEncodingKind,
    PublishDiagnosticsParams, TextDocumentItem, Uri,
};

const WINDOW: Duration = Duration::from_millis(40);

/// A parent class with one public method. Synthetic.
const PARENT: &str = "CLASS orders.calc-base:
    METHOD PUBLIC INTEGER calc-total():
        RETURN 0.
    END METHOD.
END CLASS.";

/// The same parent with the method removed — the dependency edit.
const PARENT_WITHOUT_MEMBER: &str = "CLASS orders.calc-base:
END CLASS.";

const CHILD: &str = "CLASS orders.child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-total AS INTEGER NO-UNDO.
        v-total = calc-total().
        MESSAGE v-total.
    END METHOD.
END CLASS.";

/// A second, independent subclass of the same parent, so "one parent, two
/// dependents" has two askers.
const SECOND_CHILD: &str = "CLASS orders.other-child INHERITS orders.calc-base:
    METHOD PUBLIC VOID run-it():
        DEFINE VARIABLE v-sum AS INTEGER NO-UNDO.
        v-sum = calc-total().
        MESSAGE v-sum.
    END METHOD.
END CLASS.";

/// A workspace file nothing references, for the negative case.
const UNRELATED: &str = "CLASS orders.audit-log:
    METHOD PUBLIC VOID note():
    END METHOD.
END CLASS.";

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

fn file_uri(path: &Path) -> Uri {
    Uri::from_str(&format!("file://{}", path.display())).unwrap()
}

/// Handshake declaring `root` as the workspace root (the live-editor shape).
fn handshake(client: &Connection, root: &Path) {
    let params = serde_json::json!({
        "capabilities": serde_json::to_value(ClientCapabilities {
            general: Some(GeneralClientCapabilities {
                position_encodings: Some(vec![PositionEncodingKind::UTF8]),
                ..Default::default()
            }),
            ..Default::default()
        })
        .unwrap(),
        "rootUri": file_uri(root).as_str(),
    });
    // Round-trips through `InitializeParams` so a field name that stopped
    // deserializing would fail here rather than silently leave the root unset.
    let params: InitializeParams = serde_json::from_value(params).unwrap();
    #[allow(deprecated)] // `rootUri` is exactly the field under test here.
    let declared = params.root_uri.is_some();
    assert!(declared, "the harness must declare a root");

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

/// The next publish for a specific URI, skipping publishes for other buffers.
fn recv_publish_for(
    client: &Connection,
    uri: &Uri,
    timeout: Duration,
) -> Option<PublishDiagnosticsParams> {
    let deadline = std::time::Instant::now() + timeout;
    loop {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return None;
        }
        match recv_publish(client, remaining) {
            Some(p) if p.uri == *uri => return Some(p),
            Some(_) => continue,
            None => return None,
        }
    }
}

fn watched_change(client: &Connection, path: &Path) {
    notify(
        client,
        "workspace/didChangeWatchedFiles",
        serde_json::to_value(lsp_types::DidChangeWatchedFilesParams {
            changes: vec![FileEvent {
                uri: file_uri(path),
                typ: FileChangeType::CHANGED,
            }],
        })
        .unwrap(),
    );
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

fn close(client: &Connection, uri: &Uri) {
    notify(
        client,
        "textDocument/didClose",
        serde_json::json!({ "textDocument": { "uri": uri.as_str() } }),
    );
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

fn has_lint(p: &PublishDiagnosticsParams, code: &str) -> bool {
    p.diagnostics
        .iter()
        .any(|d| d.code == Some(NumberOrString::String(code.to_string())))
}

/// A workspace whose `oxabl.toml` puts the root itself on the search path, so a
/// qualified class name maps onto `<root>/orders/<name>.cls`.
fn workspace(files: &[(&str, &str)]) -> tempfile::TempDir {
    let tmp = tempfile::TempDir::new().unwrap();
    std::fs::write(
        tmp.path().join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    std::fs::create_dir_all(tmp.path().join("orders")).unwrap();
    for (relative, source) in files {
        std::fs::write(tmp.path().join(relative), source).unwrap();
    }
    tmp
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// AE4. The child is open and resolved against the parent; the parent changes on
/// disk; the child is recomputed and its diagnostics reflect the change.
///
/// Nothing about the child's own buffer moved, so a server without a per-file
/// index input publishes nothing at all here — and one that published from its
/// memo would publish the *old* answer.
#[test]
fn a_parent_change_on_disk_recomputes_the_open_child() {
    let tmp = workspace(&[
        ("orders/calc-base.cls", PARENT),
        ("orders/child.cls", CHILD),
    ]);
    let root = tmp.path();
    let parent = root.join("orders/calc-base.cls");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client, root);

    let uri = file_uri(&root.join("orders/child.cls"));
    open(&client, &uri, CHILD);
    let first = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(
        !has_lint(&first, "LINT0001"),
        "the inherited call resolves through the workspace index: {:?}",
        first.diagnostics
    );

    // The dependency edit: the parent no longer declares the method.
    std::fs::write(&parent, PARENT_WITHOUT_MEMBER).unwrap();
    watched_change(&client, &parent);

    let after = recv_publish_for(&client, &uri, Duration::from_secs(2))
        .expect("a dependency edit must recompute the dependent buffer");
    assert!(
        has_lint(&after, "LINT0001"),
        "the recomputed set must reflect the parent's new shape, not the memo: {:?}",
        after.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// Two open buffers sharing one parent are both recomputed when it changes — the
/// dependency graph fans out, it does not stop at whichever buffer asked first.
#[test]
fn two_buffers_sharing_a_parent_are_both_recomputed() {
    let tmp = workspace(&[
        ("orders/calc-base.cls", PARENT),
        ("orders/child.cls", CHILD),
        ("orders/other-child.cls", SECOND_CHILD),
    ]);
    let root = tmp.path();
    let parent = root.join("orders/calc-base.cls");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client, root);

    let first_uri = file_uri(&root.join("orders/child.cls"));
    let second_uri = file_uri(&root.join("orders/other-child.cls"));
    open(&client, &first_uri, CHILD);
    assert!(!has_lint(
        &recv_publish_for(&client, &first_uri, Duration::from_secs(2)).unwrap(),
        "LINT0001"
    ));
    open(&client, &second_uri, SECOND_CHILD);
    assert!(!has_lint(
        &recv_publish_for(&client, &second_uri, Duration::from_secs(2)).unwrap(),
        "LINT0001"
    ));

    std::fs::write(&parent, PARENT_WITHOUT_MEMBER).unwrap();
    watched_change(&client, &parent);

    // Both fire from independent worker threads, so the order is not fixed —
    // collect until each URI has been seen rather than waiting on one and then the
    // other, which would drop whichever arrived first.
    let mut seen: Vec<Uri> = Vec::new();
    let deadline = std::time::Instant::now() + Duration::from_secs(2);
    while seen.len() < 2 {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        let publish = recv_publish(&client, remaining)
            .unwrap_or_else(|| panic!("both dependents must recompute; saw only {seen:?}"));
        assert!(
            has_lint(&publish, "LINT0001"),
            "every dependent's recomputed set must reflect the parent's new shape: {:?}",
            publish.diagnostics
        );
        if !seen.contains(&publish.uri) {
            seen.push(publish.uri);
        }
    }
    assert!(
        seen.contains(&first_uri) && seen.contains(&second_uri),
        "{seen:?}"
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// Closing the child and then changing the parent recomputes nothing: there is no
/// buffer left to answer for, and the index must not keep working on behalf of a
/// document the editor has forgotten.
#[test]
fn closing_the_child_and_changing_the_parent_recomputes_nothing() {
    let tmp = workspace(&[
        ("orders/calc-base.cls", PARENT),
        ("orders/child.cls", CHILD),
    ]);
    let root = tmp.path();
    let parent = root.join("orders/calc-base.cls");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client, root);

    let uri = file_uri(&root.join("orders/child.cls"));
    open(&client, &uri, CHILD);
    assert!(recv_publish(&client, Duration::from_secs(2)).is_some());

    // Closing publishes one empty set to clear the client's squiggles; drain it so
    // the assertion below is about the watcher and not about the close.
    close(&client, &uri);
    let cleared = recv_publish_for(&client, &uri, Duration::from_secs(2)).unwrap();
    assert!(cleared.diagnostics.is_empty(), "close clears diagnostics");

    std::fs::write(&parent, PARENT_WITHOUT_MEMBER).unwrap();
    watched_change(&client, &parent);
    assert!(
        recv_publish(&client, WINDOW * 5).is_none(),
        "no buffer is open, so a dependency edit must publish nothing"
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// A watched change to a workspace file **no lookup has ever reached** publishes
/// nothing. This is the registry early-out: a file that is nobody's dependency
/// cannot invalidate anything, so the server does not even schedule a recompute.
#[test]
fn a_change_to_an_unreferenced_source_file_publishes_nothing() {
    let tmp = workspace(&[
        ("orders/calc-base.cls", PARENT),
        ("orders/child.cls", CHILD),
        ("orders/audit-log.cls", UNRELATED),
    ]);
    let root = tmp.path();
    let unrelated = root.join("orders/audit-log.cls");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client, root);

    let uri = file_uri(&root.join("orders/child.cls"));
    open(&client, &uri, CHILD);
    assert!(!has_lint(
        &recv_publish(&client, Duration::from_secs(2)).unwrap(),
        "LINT0001"
    ));

    std::fs::write(&unrelated, "CLASS orders.audit-log:\nEND CLASS.").unwrap();
    watched_change(&client, &unrelated);
    assert!(
        recv_publish(&client, WINDOW * 5).is_none(),
        "nothing references that file, so its change is not this buffer's business"
    );

    // And the control: the parent it *does* reference still gets through, so the
    // silence above is a filter rather than a broken dispatch.
    std::fs::write(root.join("orders/calc-base.cls"), PARENT_WITHOUT_MEMBER).unwrap();
    watched_change(&client, &root.join("orders/calc-base.cls"));
    let after = recv_publish_for(&client, &uri, Duration::from_secs(2))
        .expect("the referenced parent still invalidates");
    assert!(has_lint(&after, "LINT0001"), "{:?}", after.diagnostics);

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}
