//! End-to-end: which document the session's *one* configuration resolution
//! anchors to.
//!
//! `ensure_config` is a one-shot, and it used to be spent by the first `didOpen`
//! of any kind. A scratch buffer (`untitled:`), a VS Code `git:` diff view, or a
//! `file://` path carrying a percent-escape all failed to yield an anchor — and
//! because the one-shot was already marked spent, the session stayed on default
//! severities, default include paths, and the default formatter style for as long
//! as the editor was open, with no watcher event able to recover it
//! (`handle_watched_files` early-returns without an anchor).
//!
//! Both tests assert through *two* surfaces, because one resolution feeds both
//! (KTD3): a `[workspace.lint]` severity reaching published diagnostics, and a
//! `[workspace.style]` setting reaching `textDocument/formatting`.
//!
//! All fixtures are synthetic ABL.

use std::str::FromStr;
use std::thread;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, DocumentFormattingParams, FormattingOptions,
    GeneralClientCapabilities, InitializeParams, NumberOrString, PositionEncodingKind,
    PublishDiagnosticsParams, TextDocumentIdentifier, TextDocumentItem, TextEdit, Uri,
};

/// An `oxabl.toml` that is detectable through both surfaces: it turns
/// `unused-variable` off and sets a 2-space indent (the built-in default is 4).
const CONFIG: &str = "[workspace]\nname = \"p\"\n\
    [workspace.lint]\nunused-variable = \"off\"\n\
    [workspace.style]\nindent_size = 2\n";

/// A buffer that would report LINT0002 under default severities and that
/// reformats visibly under a 2-space indent.
const SOURCE: &str =
    "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nIF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";

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

fn did_open(client: &Connection, uri: &Uri, text: &str) {
    client
        .sender
        .send(Message::Notification(Notification {
            method: "textDocument/didOpen".to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abl".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            })
            .unwrap(),
        }))
        .unwrap();
}

fn await_publish_for(client: &Connection, uri: &Uri) -> PublishDiagnosticsParams {
    loop {
        if let Message::Notification(n) = client.receiver.recv().unwrap()
            && n.method == "textDocument/publishDiagnostics"
        {
            let params: PublishDiagnosticsParams = serde_json::from_value(n.params).unwrap();
            if &params.uri == uri {
                return params;
            }
        }
    }
}

fn format(client: &Connection, id: i32, uri: &Uri) -> Vec<TextEdit> {
    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            ..Default::default()
        },
        work_done_progress_params: Default::default(),
    };
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(id),
            method: "textDocument/formatting".to_string(),
            params: serde_json::to_value(params).unwrap(),
        }))
        .unwrap();
    let response = loop {
        if let Message::Response(r) = client.receiver.recv().unwrap()
            && r.id == RequestId::from(id)
        {
            break r;
        }
    };
    match response.result {
        Some(serde_json::Value::Null) | None => Vec::new(),
        Some(v) => serde_json::from_value(v).unwrap(),
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
        if let Message::Response(r) = client.receiver.recv().unwrap()
            && r.id == RequestId::from(99)
        {
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

/// Assert that `uri` — an already-opened document in a directory carrying
/// [`CONFIG`] — is being judged and formatted under that configuration.
fn assert_config_applies(client: &Connection, uri: &Uri) {
    let published = await_publish_for(client, uri);
    assert!(
        !published
            .diagnostics
            .iter()
            .any(|d| d.code == Some(NumberOrString::String("LINT0002".to_string()))),
        "`unused-variable = off` from the workspace config must be in effect, got {:?}",
        published.diagnostics
    );

    let edits = format(client, 2, uri);
    assert_eq!(edits.len(), 1, "the buffer reformats");
    assert!(
        edits[0].new_text.contains("\n  MESSAGE"),
        "the workspace 2-space indent must be in effect, got:\n{}",
        edits[0].new_text
    );
}

/// A scratch buffer opened first must not consume the one-shot: the next real
/// file anchors the configuration.
#[test]
fn an_untitled_first_document_leaves_the_config_resolvable() {
    let tmp = tempfile::TempDir::new().unwrap();
    std::fs::write(tmp.path().join("oxabl.toml"), CONFIG).unwrap();
    let file = tmp.path().join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(50)));
    handshake(&client);

    // First: a scratch buffer with no filesystem path at all.
    let scratch = Uri::from_str("untitled:Untitled-1").unwrap();
    did_open(&client, &scratch, SOURCE);
    let _ = await_publish_for(&client, &scratch);

    // Then the real file, which is the session's first genuine anchor.
    let uri = Uri::from_str(&format!("file://{}", file.display())).unwrap();
    did_open(&client, &uri, SOURCE);
    assert_config_applies(&client, &uri);

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}

/// A path with a space arrives percent-escaped from every real client, and it
/// must still anchor the configuration.
#[test]
fn a_percent_escaped_path_anchors_the_config() {
    let tmp = tempfile::TempDir::new().unwrap();
    let dir = tmp.path().join("my project");
    std::fs::create_dir(&dir).unwrap();
    std::fs::write(dir.join("oxabl.toml"), CONFIG).unwrap();
    let file = dir.join("main.p");

    let (server, client) = Connection::memory();
    let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, Duration::from_millis(50)));
    handshake(&client);

    let escaped = file.display().to_string().replace(' ', "%20");
    let uri = Uri::from_str(&format!("file://{escaped}")).unwrap();
    did_open(&client, &uri, SOURCE);
    assert_config_applies(&client, &uri);

    shutdown(&client);
    assert!(handle.join().unwrap().unwrap());
}
