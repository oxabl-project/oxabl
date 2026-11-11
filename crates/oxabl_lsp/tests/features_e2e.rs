//! End-to-end: `[workspace.features]` in `oxabl.toml` turns the server's
//! diagnostics and formatting on and off, and a rejected `oxabl.toml` turns both
//! off until it is fixed.

use std::path::Path;
use std::str::FromStr;
use std::thread;
use std::time::{Duration, Instant};

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{PublishDiagnosticsParams, Uri};
use serde_json::{Value, json};
use tempfile::TempDir;

const WINDOW: Duration = Duration::from_millis(40);
const WAIT: Duration = Duration::from_secs(5);

/// One unused variable (LINT0002) and layout the formatter changes.
const UNUSED: &str = "DO:\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\nEND.\n";

const ON: &str = "[workspace]\nname = \"p\"\n";
const DIAGNOSTICS_OFF: &str =
    "[workspace]\nname = \"p\"\n[workspace.features]\ndiagnostics = false\n";
const FORMAT_OFF: &str = "[workspace]\nname = \"p\"\n[workspace.features]\nformat = false\n";
const REJECTED: &str = "this is not valid toml {{{";

struct Client {
    connection: Connection,
    handle: thread::JoinHandle<anyhow::Result<bool>>,
    next: i32,
    /// The `window/showMessage` texts seen so far.
    shown: Vec<String>,
    /// The publishes seen while waiting for something else.
    publishes: Vec<PublishDiagnosticsParams>,
}

impl Client {
    /// A server over `root`, declared as the workspace root.
    fn new(root: &Path, capabilities: Value) -> (Self, Value) {
        let (server, connection) = Connection::memory();
        let handle = thread::spawn(move || oxabl_lsp::serve_with(&server, WINDOW));
        let mut client = Client {
            connection,
            handle,
            next: 1,
            shown: Vec::new(),
            publishes: Vec::new(),
        };
        let init = client.request(
            "initialize",
            json!({
                "capabilities": capabilities,
                "workspaceFolders": [{"uri": uri(root).as_str(), "name": "test"}],
            }),
        );
        client.notify("initialized", json!({}));
        (client, init)
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

    fn take(&mut self, message: Message) -> Option<Response> {
        match message {
            Message::Notification(n) if n.method == "window/showMessage" => {
                self.shown
                    .push(n.params["message"].as_str().unwrap_or_default().to_string());
            }
            Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => {
                self.publishes
                    .push(serde_json::from_value(n.params).unwrap());
            }
            Message::Request(req) => {
                self.connection
                    .sender
                    .send(Message::Response(Response::new_ok(req.id, Value::Null)))
                    .unwrap();
            }
            Message::Response(response) => return Some(response),
            Message::Notification(_) => {}
        }
        None
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
            let message = self.connection.receiver.recv_timeout(WAIT).unwrap();
            if let Some(response) = self.take(message)
                && response.id == id
            {
                assert!(response.error.is_none(), "{response:?}");
                return response.result.unwrap_or(Value::Null);
            }
        }
    }

    fn open(&self, file: &Path, text: &str) -> Uri {
        let uri = uri(file);
        self.notify(
            "textDocument/didOpen",
            json!({"textDocument": {
                "uri": uri.as_str(), "languageId": "abl", "version": 1, "text": text
            }}),
        );
        uri
    }

    /// The next publish, or `None` when none arrives within `timeout`.
    fn publish_within(&mut self, timeout: Duration) -> Option<PublishDiagnosticsParams> {
        if !self.publishes.is_empty() {
            return Some(self.publishes.remove(0));
        }
        let deadline = Instant::now() + timeout;
        loop {
            let left = deadline.checked_duration_since(Instant::now())?;
            let message = self.connection.receiver.recv_timeout(left).ok()?;
            self.take(message);
            if !self.publishes.is_empty() {
                return Some(self.publishes.remove(0));
            }
        }
    }

    fn publish(&mut self) -> PublishDiagnosticsParams {
        self.publish_within(WAIT).expect("a publishDiagnostics")
    }

    /// The edits `textDocument/formatting` answers with.
    fn format(&mut self, uri: &Uri) -> Vec<Value> {
        let result = self.request(
            "textDocument/formatting",
            json!({
                "textDocument": {"uri": uri.as_str()},
                "options": {"tabSize": 4, "insertSpaces": true},
            }),
        );
        match result {
            Value::Null => Vec::new(),
            other => serde_json::from_value(other).unwrap(),
        }
    }

    /// Save `toml` over `oxabl.toml` and tell the server it changed.
    fn change_toml(&self, root: &Path, toml: &str) {
        let path = root.join("oxabl.toml");
        std::fs::write(&path, toml).unwrap();
        self.notify(
            "workspace/didChangeWatchedFiles",
            json!({"changes": [{"uri": uri(&path).as_str(), "type": 2}]}),
        );
    }

    fn close(mut self) {
        self.request("shutdown", Value::Null);
        self.notify("exit", Value::Null);
        assert!(self.handle.join().unwrap().unwrap());
    }
}

fn uri(path: &Path) -> Uri {
    Uri::from_str(&format!("file://{}", path.display())).unwrap()
}

fn root(toml: &str) -> TempDir {
    let root = TempDir::new().unwrap();
    std::fs::write(root.path().join("oxabl.toml"), toml).unwrap();
    root
}

fn has_lint(p: &PublishDiagnosticsParams) -> bool {
    p.diagnostics
        .iter()
        .any(|d| d.code == Some(lsp_types::NumberOrString::String("LINT0002".to_string())))
}

#[test]
fn diagnostics_are_published_by_default() {
    let root = root(ON);
    let (mut client, _) = Client::new(root.path(), json!({}));
    client.open(&root.path().join("main.p"), UNUSED);

    assert!(has_lint(&client.publish()));
    client.close();
}

#[test]
fn diagnostics_off_publishes_an_empty_list_on_open_and_on_edit() {
    let root = root(DIAGNOSTICS_OFF);
    let (mut client, _) = Client::new(root.path(), json!({}));
    let uri = client.open(&root.path().join("main.p"), UNUSED);

    let opened = client.publish();
    assert!(
        opened.diagnostics.is_empty(),
        "open: {:?}",
        opened.diagnostics
    );

    client.notify(
        "textDocument/didChange",
        json!({
            "textDocument": {"uri": uri.as_str(), "version": 2},
            "contentChanges": [{"text": "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n"}],
        }),
    );
    let edited = client.publish();
    assert!(
        edited.diagnostics.is_empty(),
        "edit: {:?}",
        edited.diagnostics
    );
    client.close();
}

#[test]
fn format_is_answered_by_default() {
    let root = root(ON);
    let (mut client, _) = Client::new(root.path(), json!({}));
    let uri = client.open(&root.path().join("main.p"), UNUSED);

    assert!(!client.format(&uri).is_empty());
    client.close();
}

#[test]
fn format_off_returns_no_edits_and_keeps_diagnostics() {
    let root = root(FORMAT_OFF);
    let (mut client, init) = Client::new(root.path(), json!({}));
    assert_eq!(
        init["capabilities"]["documentFormattingProvider"],
        json!(true),
        "the capability stays: the switch can change while the server runs"
    );
    let uri = client.open(&root.path().join("main.p"), UNUSED);

    assert_eq!(client.format(&uri), Vec::<Value>::new());
    assert!(has_lint(&client.publish()));
    client.close();
}

#[test]
fn an_oxabl_toml_edit_turns_diagnostics_off_and_on_again() {
    let root = root(ON);
    let (mut client, _) = Client::new(root.path(), json!({}));
    client.open(&root.path().join("main.p"), UNUSED);
    assert!(has_lint(&client.publish()));

    client.change_toml(root.path(), DIAGNOSTICS_OFF);
    let cleared = client.publish();
    assert!(
        cleared.diagnostics.is_empty(),
        "on to off must clear what was published: {:?}",
        cleared.diagnostics
    );

    client.change_toml(root.path(), ON);
    assert!(
        has_lint(&client.publish()),
        "off to on must publish the findings again"
    );
    client.close();
}

#[test]
fn an_oxabl_toml_edit_turns_formatting_off_and_on_again() {
    let root = root(ON);
    let (mut client, _) = Client::new(root.path(), json!({}));
    let uri = client.open(&root.path().join("main.p"), UNUSED);
    assert!(!client.format(&uri).is_empty());

    client.change_toml(root.path(), FORMAT_OFF);
    client.publish();
    assert!(client.format(&uri).is_empty());

    client.change_toml(root.path(), ON);
    client.publish();
    assert!(!client.format(&uri).is_empty());
    client.close();
}

#[test]
fn a_rejected_oxabl_toml_turns_both_off_and_says_so_once() {
    let root = root(REJECTED);
    let (mut client, _) = Client::new(root.path(), json!({}));
    let uri = client.open(&root.path().join("main.p"), UNUSED);

    let opened = client.publish();
    assert!(
        opened.diagnostics.is_empty(),
        "open: {:?}",
        opened.diagnostics
    );
    assert_eq!(client.format(&uri), Vec::<Value>::new());
    assert_eq!(client.shown.len(), 1, "{:?}", client.shown);
    let shown = &client.shown[0];
    assert!(shown.contains("oxabl.toml"), "{shown}");
    assert!(
        shown.contains("Diagnostics and formatting are off until oxabl.toml is fixed"),
        "{shown}"
    );
    client.close();
}

#[test]
fn an_oxabl_toml_edited_into_an_error_turns_both_off_until_it_is_fixed() {
    let root = root(ON);
    let (mut client, _) = Client::new(root.path(), json!({}));
    let uri = client.open(&root.path().join("main.p"), UNUSED);
    assert!(has_lint(&client.publish()));
    assert!(!client.format(&uri).is_empty());
    assert!(client.shown.is_empty(), "{:?}", client.shown);

    client.change_toml(root.path(), REJECTED);
    let cleared = client.publish();
    assert!(cleared.diagnostics.is_empty(), "{:?}", cleared.diagnostics);
    assert_eq!(client.format(&uri), Vec::<Value>::new());
    assert_eq!(client.shown.len(), 1, "{:?}", client.shown);

    client.change_toml(root.path(), ON);
    assert!(has_lint(&client.publish()));
    assert!(!client.format(&uri).is_empty());
    client.close();
}

#[test]
fn no_oxabl_toml_keeps_both_on() {
    let root = TempDir::new().unwrap();
    let (mut client, _) = Client::new(root.path(), json!({}));
    let uri = client.open(&root.path().join("main.p"), UNUSED);

    assert!(has_lint(&client.publish()));
    assert!(!client.format(&uri).is_empty());
    assert!(client.shown.is_empty(), "{:?}", client.shown);
    client.close();
}

/// A project with a strict database rule on, so the effect code lenses are
/// available, and `features` as the body of `[workspace.features]`.
fn lens_root(features: &str) -> TempDir {
    let root = TempDir::new().unwrap();
    std::fs::write(
        root.path().join("schema.df"),
        include_str!("fixtures/effect_lenses/schema.df"),
    )
    .unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"t\"\n[workspace.schema]\nfiles = [\"schema.df\"]\n\
             [workspace.lint]\nenable = [\"strict.access\"]\n\
             [workspace.features]\n{features}"
        ),
    )
    .unwrap();
    root
}

const LENS_SOURCE: &str = "FIND FIRST customer NO-LOCK NO-ERROR.\n";

fn lens_capabilities() -> Value {
    json!({
        "textDocument": {"codeLens": {"dynamicRegistration": false}},
        "workspace": {"codeLens": {"refreshSupport": true}},
    })
}

#[test]
fn effect_lenses_follow_the_diagnostics_switch() {
    let root = lens_root("diagnostics = true\n");
    let (mut client, init) = Client::new(root.path(), lens_capabilities());
    assert!(init["capabilities"].get("codeLensProvider").is_some());
    let uri = client.open(&root.path().join("main.p"), LENS_SOURCE);
    let params = json!({"textDocument": {"uri": uri.as_str()}});
    let deadline = Instant::now() + WAIT;
    let lenses = loop {
        let lenses = client.request("textDocument/codeLens", params.clone());
        if lenses != json!([]) || Instant::now() >= deadline {
            break lenses;
        }
        thread::sleep(Duration::from_millis(10));
    };
    assert_ne!(lenses, json!([]), "the control must produce a lens");
    client.close();

    let root = lens_root("diagnostics = false\n");
    let (mut client, init) = Client::new(root.path(), lens_capabilities());
    assert!(
        init["capabilities"].get("codeLensProvider").is_none(),
        "no lens capability with diagnostics off"
    );
    let uri = client.open(&root.path().join("main.p"), LENS_SOURCE);
    let params = json!({"textDocument": {"uri": uri.as_str()}});
    let deadline = Instant::now() + Duration::from_secs(1);
    while Instant::now() < deadline {
        assert_eq!(
            client.request("textDocument/codeLens", params.clone()),
            json!([])
        );
        thread::sleep(Duration::from_millis(20));
    }
    client.close();
}
