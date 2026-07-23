//! U8 end-to-end: `workspace/didChangeWatchedFiles` drives recompute for
//! `oxabl.toml` changes (R17) and idle `*.i` changes (R17), and does nothing
//! for an include no buffer depends on.

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

fn file_uri(path: &Path) -> Uri {
    Uri::from_str(&format!("file://{}", path.display())).unwrap()
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

#[test]
fn oxabl_toml_change_reresolves_lint_config() {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    let toml = root.join("oxabl.toml");
    // Start with unused-variable ON (default warn).
    std::fs::write(&toml, "[workspace]\nname = \"p\"\n").unwrap();
    let main = root.join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client);

    let uri = file_uri(&main);
    open(&client, &uri, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    let first = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(
        has_lint(&first, "LINT0002"),
        "unused-variable on by default"
    );

    // Turn unused-variable off and notify the watcher.
    std::fs::write(
        &toml,
        "[workspace]\nname = \"p\"\n[workspace.lint]\nunused-variable = \"off\"\n",
    )
    .unwrap();
    watched_change(&client, &toml);

    let after = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(
        !has_lint(&after, "LINT0002"),
        "oxabl.toml change must re-resolve [lint] and drop LINT0002: {:?}",
        after.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn schema_change_hot_reloads_diagnostics() {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    let df = root.join("schema.df");
    // Initial schema: Customer with only CustNum (no Name field).
    std::fs::write(
        &df,
        "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\n",
    )
    .unwrap();
    std::fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.schema]\nfiles = [\"schema.df\"]\n",
    )
    .unwrap();
    let main = root.join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client);

    let uri = file_uri(&main);
    // `Customer.Name` is unknown under the initial schema → LINT0003.
    open(
        &client,
        &uri,
        "FIND FIRST Customer.\nMESSAGE Customer.Name.\n",
    );
    let first = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(
        has_lint(&first, "LINT0003"),
        "unknown field must flag LINT0003 under the initial schema: {:?}",
        first.diagnostics
    );

    // Add the Name field and notify the watcher — the diagnostic must clear
    // without a restart (R16).
    std::fs::write(
        &df,
        "ADD TABLE \"Customer\"\nADD FIELD \"CustNum\" OF \"Customer\" AS integer\nADD FIELD \"Name\" OF \"Customer\" AS character\n",
    )
    .unwrap();
    watched_change(&client, &df);

    let after = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(
        !has_lint(&after, "LINT0003"),
        "schema hot-reload must clear LINT0003 once Name is valid: {:?}",
        after.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn idle_include_change_retriggers_dependent_buffer_only() {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path();
    std::fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    let dep = root.join("dep.i");
    let unrelated = root.join("other.i");
    std::fs::write(&dep, "DEFINE VARIABLE foo AS INTEGER NO-UNDO.\n").unwrap();
    std::fs::write(&unrelated, "DEFINE VARIABLE zzz AS INTEGER NO-UNDO.\n").unwrap();
    let main = root.join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    handshake(&client);

    let uri = file_uri(&main);
    // `foo` is defined by the include and read here → no undefined-symbol.
    open(&client, &uri, "{dep.i}\nMESSAGE foo.\n");
    let first = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(!has_lint(&first, "LINT0001"), "foo resolves via include");

    // A change to an include NO buffer depends on must not recompute.
    watched_change(&client, &unrelated);
    assert!(
        recv_publish(&client, WINDOW * 3).is_none(),
        "unrelated include change must not trigger a publish"
    );

    // Change dep.i so foo is no longer defined, then notify the watcher.
    std::fs::write(&dep, "DEFINE VARIABLE bar AS INTEGER NO-UNDO.\n").unwrap();
    watched_change(&client, &dep);
    let after = recv_publish(&client, Duration::from_secs(2)).unwrap();
    assert!(
        has_lint(&after, "LINT0001"),
        "idle include change must re-trigger the dependent buffer (foo now undefined): {:?}",
        after.diagnostics
    );

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}
