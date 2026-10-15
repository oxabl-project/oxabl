use std::{str::FromStr, thread, time::Duration};

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use serde_json::{Value, json};
use tempfile::TempDir;

struct Client {
    connection: Connection,
    handle: thread::JoinHandle<anyhow::Result<bool>>,
    next: i32,
    server_requests: Vec<String>,
}

impl Client {
    fn new(root: &TempDir, supported: bool, dynamic: bool) -> (Self, Value) {
        let (server, connection) = Connection::memory();
        let handle = thread::spawn(move || oxabl_lsp::serve(&server));
        let mut client = Self {
            connection,
            handle,
            next: 1,
            server_requests: Vec::new(),
        };
        let caps = if supported {
            json!({"textDocument": {"codeLens": {"dynamicRegistration": dynamic}},
            "workspace": {"codeLens": {"refreshSupport": true}}})
        } else {
            json!({})
        };
        let result = client.request("initialize", json!({
            "capabilities": caps,
            "workspaceFolders": [{"uri": format!("file://{}", root.path().display()), "name": "test"}]
        }));
        client.notify("initialized", json!({}));
        (client, result)
    }

    fn notify(&self, method: &str, params: Value) {
        self.connection
            .sender
            .send(Message::Notification(Notification::new(
                method.into(),
                params,
            )))
            .unwrap();
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = RequestId::from(self.next);
        self.next += 1;
        self.connection
            .sender
            .send(Message::Request(Request::new(
                id.clone(),
                method.into(),
                params,
            )))
            .unwrap();
        loop {
            match self
                .connection
                .receiver
                .recv_timeout(Duration::from_secs(15))
                .unwrap()
            {
                Message::Response(response) if response.id == id => {
                    assert!(response.error.is_none(), "{response:?}");
                    return response.result.unwrap();
                }
                Message::Request(req) => {
                    self.server_requests.push(req.method);
                    self.connection
                        .sender
                        .send(Message::Response(Response::new_ok(req.id, Value::Null)))
                        .unwrap();
                }
                _ => {}
            }
        }
    }

    fn open(&self, root: &TempDir, name: &str, source: &str) -> Value {
        let uri = format!("file://{}", root.path().join(name).display());
        self.notify(
            "textDocument/didOpen",
            json!({"textDocument": {
                "uri": uri, "languageId": "abl", "version": 1, "text": source
            }}),
        );
        json!({"textDocument": {"uri": uri}})
    }

    fn ready(&mut self, params: Value) -> Vec<lsp_types::CodeLens> {
        for _ in 0..100 {
            let result = self.request("textDocument/codeLens", params.clone());
            let lenses: Vec<lsp_types::CodeLens> = serde_json::from_value(result).unwrap();
            if !lenses.is_empty() {
                return lenses;
            }
            thread::sleep(Duration::from_millis(10));
        }
        panic!("code lenses did not become ready");
    }

    fn close(mut self) {
        self.request("shutdown", Value::Null);
        self.notify("exit", Value::Null);
        assert!(self.handle.join().unwrap().unwrap());
    }
}

fn fixture(lint: &str) -> TempDir {
    let root = TempDir::new().unwrap();
    let fixtures =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/effect_lenses");
    for name in ["schema.df", "routines.i", "main.p"] {
        std::fs::copy(fixtures.join(name), root.path().join(name)).unwrap();
    }
    std::fs::write(root.path().join("oxabl.toml"), format!(
        "[workspace]\nname = \"test\"\n[workspace.sources]\ninclude_paths = [\".\"]\n[workspace.schema]\nfiles = [\"schema.df\"]\n[workspace.lint]\n{lint}\n"
    )).unwrap();
    root
}

#[test]
fn renders_effects_calls_partial_and_root_headers() {
    let root = fixture("enable = [\"strict.access\"]");
    let (mut client, init) = Client::new(&root, true, false);
    assert_eq!(
        init["capabilities"]["codeLensProvider"]["resolveProvider"],
        false
    );
    let source = include_str!("fixtures/effect_lenses/main.p");
    let params = client.open(&root, "main.p", source);
    let lenses = client.ready(params);
    let lines: Vec<_> = lenses.iter().map(|l| l.range.start.line).collect();
    assert_eq!(lines, [0, 4, 15, 20]);
    let titles: Vec<_> = lenses
        .iter()
        .map(|l| l.command.as_ref().unwrap().title.as_str())
        .collect();
    assert!(titles[0].contains("reads customer"), "{titles:?}");
    assert!(titles[0].contains("writes order-line"), "{titles:?}");
    assert_eq!(
        titles[1],
        "reads customer · writes order-line · 2 calls (2 unknown) · partial"
    );
    assert_eq!(titles[2], "reads customer");
    assert_eq!(titles[3], "reads customer");
    assert!(
        client
            .server_requests
            .iter()
            .any(|m| m == "workspace/codeLens/refresh")
    );
    client.close();
}

#[test]
fn capability_and_lenses_are_off_by_default_and_without_client_support() {
    for (lint, supported) in [("", true), ("enable = [\"strict.access\"]", false)] {
        let root = fixture(lint);
        let (mut client, init) = Client::new(&root, supported, false);
        assert!(init["capabilities"].get("codeLensProvider").is_none());
        let params = client.open(
            &root,
            "main.p",
            include_str!("fixtures/effect_lenses/main.p"),
        );
        assert_eq!(client.request("textDocument/codeLens", params), json!([]));
        client.close();
    }
}

#[test]
fn dynamic_registration_uses_file_overrides_and_accepts_unsaved_methods() {
    let root =
        fixture("[[workspace.lint.overrides]]\nfiles = [\"*.cls\"]\nenable = [\"strict.access\"]");
    let (mut client, init) = Client::new(&root, true, true);
    assert!(init["capabilities"].get("codeLensProvider").is_none());
    let params = client.open(
        &root,
        "main.p",
        include_str!("fixtures/effect_lenses/main.p"),
    );
    assert_eq!(client.request("textDocument/codeLens", params), json!([]));
    let params = client.open(&root, "sample.cls", "CLASS sample:\n  METHOD PUBLIC VOID load():\n    FIND FIRST customer NO-LOCK NO-ERROR.\n  END METHOD.\nEND CLASS.\n");
    let lenses = client.ready(params);
    assert_eq!(lenses.len(), 1);
    assert_eq!(lenses[0].range.start.line, 1);
    assert_eq!(lenses[0].command.as_ref().unwrap().title, "reads customer");
    assert!(
        client
            .server_requests
            .iter()
            .any(|m| m == "client/registerCapability")
    );
    client.close();
}

#[test]
fn unopened_documents_have_no_lenses() {
    let root = fixture("");
    let (mut client, _) = Client::new(&root, true, false);
    let uri = lsp_types::Uri::from_str("file:///missing.p").unwrap();
    assert_eq!(
        client.request(
            "textDocument/codeLens",
            json!({"textDocument": {"uri": uri}})
        ),
        json!([])
    );
    client.close();
}

#[test]
fn edits_and_callee_watch_events_discard_ready_lenses() {
    let root = fixture("enable = [\"strict.access\"]");
    let callee = root.path().join("callee.p");
    std::fs::write(&callee, "FIND FIRST customer NO-LOCK NO-ERROR.\n").unwrap();
    let (mut client, _) = Client::new(&root, true, false);
    let params = client.open(&root, "caller.p", "RUN callee.p.\n");
    assert_eq!(
        client.request("textDocument/codeLens", params.clone()),
        json!([])
    );
    let lenses = client.ready(params.clone());
    assert_eq!(
        lenses[0].command.as_ref().unwrap().title,
        "reads customer · 1 call"
    );

    std::fs::write(&callee, "CREATE order-line.\n").unwrap();
    client.notify(
        "workspace/didChangeWatchedFiles",
        json!({"changes": [{
            "uri": format!("file://{}", callee.display()), "type": 2
        }]}),
    );
    assert_eq!(
        client.request("textDocument/codeLens", params.clone()),
        json!([])
    );
    let lenses = client.ready(params.clone());
    assert_eq!(
        lenses[0].command.as_ref().unwrap().title,
        "writes order-line · 1 call"
    );

    client.notify(
        "textDocument/didChange",
        json!({
            "textDocument": {"uri": params["textDocument"]["uri"], "version": 2},
            "contentChanges": [{"text": "FIND FIRST customer NO-LOCK NO-ERROR.\n"}]
        }),
    );
    assert_eq!(
        client.request("textDocument/codeLens", params.clone()),
        json!([])
    );
    let lenses = client.ready(params);
    assert_eq!(lenses[0].command.as_ref().unwrap().title, "reads customer");
    client.close();
}

#[test]
fn transactions_enable_lenses_and_include_first_main_keeps_its_root_position() {
    let root = fixture("enable = [\"strict.transactions\"]");
    let (mut client, init) = Client::new(&root, true, false);
    assert!(init["capabilities"].get("codeLensProvider").is_some());
    let params = client.open(&root, "caller.p", "{routines.i}\nRUN included.\n");
    let lenses = client.ready(params);
    assert_eq!(lenses.len(), 1);
    assert_eq!(lenses[0].range.start.line, 0);
    assert_eq!(
        lenses[0].command.as_ref().unwrap().title,
        "reads customer · 1 call"
    );
    client.close();
}
