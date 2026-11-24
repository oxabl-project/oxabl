//! A version whose analysis panics has no inactive regions: the server clears
//! the regions of the older text, as no publish follows for that version.
//!
//! The panic is injected (`oxabl_common`'s `test-panics` feature, enabled
//! through this crate's dev-dependencies). All fixtures are synthetic ABL.

mod support;

use std::thread;
use std::time::{Duration, Instant};

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use oxabl_common::panic_sites;
use serde_json::{Value, json};

const WINDOW: Duration = Duration::from_millis(80);

/// A branch that is never compiled, on a condition with no name in it.
const DIMMED: &str = "&IF 1 = 2 &THEN\nMESSAGE \"never\".\n&ENDIF\n";

fn send(client: &Connection, message: Message) {
    client.sender.send(message).unwrap();
}

fn notify(client: &Connection, method: &str, params: Value) {
    send(
        client,
        Message::Notification(Notification {
            method: method.to_string(),
            params,
        }),
    );
}

/// The next `textDocument/inactiveRegions` for `version`, or `None` in 10 s.
fn regions_for(client: &Connection, version: i32) -> Option<Value> {
    let end = Instant::now() + Duration::from_secs(10);
    while let Some(left) = end.checked_duration_since(Instant::now()) {
        let Ok(Message::Notification(n)) = client.receiver.recv_timeout(left) else {
            continue;
        };
        if n.method == "textDocument/inactiveRegions"
            && n.params["textDocument"]["version"] == json!(version)
        {
            return Some(n.params["regions"].clone());
        }
    }
    None
}

fn run(edit_to_panic: impl FnOnce(&Connection, &str)) {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    let project = support::project();
    let root = format!("file://{}", project.path().display());
    send(
        &client,
        Message::Request(Request {
            id: RequestId::from(1),
            method: "initialize".to_string(),
            params: json!({
                "processId": null,
                "workspaceFolders": support::folders(project.path()),
                "capabilities": {
                    "textDocument": {"inactiveRegionsCapabilities": {"inactiveRegions": true}}
                }
            }),
        }),
    );
    while !matches!(client.receiver.recv().unwrap(), Message::Response(_)) {}
    notify(&client, "initialized", json!({}));

    let uri = format!("{root}/dimmed.p");
    notify(
        &client,
        "textDocument/didOpen",
        json!({"textDocument": {"uri": uri, "languageId": "abl", "version": 1, "text": DIMMED}}),
    );
    let first = regions_for(&client, 1).expect("the regions of the first version");
    assert_eq!(first.as_array().map(Vec::len), Some(1), "{first}");

    edit_to_panic(&client, &uri);
    let after = regions_for(&client, 2).expect("regions for the version that panicked");
    assert_eq!(after, json!([]));

    send(
        &client,
        Message::Request(Request {
            id: RequestId::from(2),
            method: "shutdown".to_string(),
            params: Value::Null,
        }),
    );
    while !matches!(client.receiver.recv().unwrap(), Message::Response(_)) {}
    notify(&client, "exit", Value::Null);
    assert!(handle.join().unwrap().unwrap());
}

#[test]
fn an_edit_whose_analysis_panics_clears_the_regions() {
    run(|client, uri| {
        let text = format!(
            "/* OXABL-TEST-PANIC:{} */\n{DIMMED}",
            panic_sites::LSP_DIAGNOSTICS
        );
        notify(
            client,
            "textDocument/didChange",
            json!({
                "textDocument": {"uri": uri, "version": 2},
                "contentChanges": [{"text": text}]
            }),
        );
    });
}

#[test]
fn a_reopen_whose_analysis_panics_clears_the_regions() {
    run(|client, uri| {
        notify(
            client,
            "textDocument/didClose",
            json!({"textDocument": {"uri": uri}}),
        );
        let text = format!(
            "/* OXABL-TEST-PANIC:{} */\n{DIMMED}",
            panic_sites::LSP_DIAGNOSTICS
        );
        notify(
            client,
            "textDocument/didOpen",
            json!({"textDocument": {"uri": uri, "languageId": "abl", "version": 2, "text": text}}),
        );
    });
}

/// The regions of an include in a selected root, after the root's analysis
/// panics: cleared, as the root gives no answer.
#[test]
fn a_root_whose_analysis_panics_clears_the_includes_regions() {
    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
    let project = tempfile::TempDir::new().unwrap();
    let files = [
        (
            "oxabl.toml",
            "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
        ),
        ("shared.i", DIMMED),
        ("root.p", "{shared.i}\n"),
    ];
    for (name, text) in files {
        std::fs::write(project.path().join(name), text).unwrap();
    }
    let dir = std::fs::canonicalize(project.path()).unwrap();
    let root = format!("file://{}", dir.display());
    send(
        &client,
        Message::Request(Request {
            id: RequestId::from(1),
            method: "initialize".to_string(),
            params: json!({
                "processId": null,
                "rootUri": root,
                "capabilities": {
                    "textDocument": {"inactiveRegionsCapabilities": {"inactiveRegions": true}}
                }
            }),
        }),
    );
    while !matches!(client.receiver.recv().unwrap(), Message::Response(_)) {}
    notify(&client, "initialized", json!({}));

    let shared = format!("{root}/shared.i");
    notify(
        &client,
        "textDocument/didOpen",
        json!({"textDocument": {"uri": shared, "languageId": "abl", "version": 1, "text": DIMMED}}),
    );
    regions_for(&client, 1).expect("the regions of the include alone");
    send(
        &client,
        Message::Request(Request {
            id: RequestId::from(2),
            method: "oxabl/selectIncluderContext".to_string(),
            params: json!({
                "textDocument": {"uri": shared},
                "root": dir.join("root.p"),
            }),
        }),
    );
    let in_root = regions_for(&client, 1).expect("the regions of the include in the root");
    assert_eq!(in_root.as_array().map(Vec::len), Some(1), "{in_root}");

    // The root's open buffer now panics in its expansion.
    let text = format!(
        "/* OXABL-TEST-PANIC:{} */\n{{shared.i}}\n",
        panic_sites::ANALYZE
    );
    notify(
        &client,
        "textDocument/didOpen",
        json!({"textDocument": {"uri": format!("{root}/root.p"), "languageId": "abl", "version": 1, "text": text}}),
    );
    let end = Instant::now() + Duration::from_secs(10);
    let mut cleared = false;
    while let Some(left) = end.checked_duration_since(Instant::now()) {
        let Ok(Message::Notification(n)) = client.receiver.recv_timeout(left) else {
            continue;
        };
        if n.method == "textDocument/inactiveRegions"
            && n.params["textDocument"]["uri"] == json!(shared)
            && n.params["regions"] == json!([])
        {
            cleared = true;
            break;
        }
    }
    assert!(cleared, "the include's regions must be cleared");

    send(
        &client,
        Message::Request(Request {
            id: RequestId::from(3),
            method: "shutdown".to_string(),
            params: Value::Null,
        }),
    );
    while !matches!(client.receiver.recv().unwrap(), Message::Response(_)) {}
    notify(&client, "exit", Value::Null);
    assert!(handle.join().unwrap().unwrap());
}
