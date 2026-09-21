//! R26 on the LSP entrance to a daemon socket.
//!
//! `oxabl/handshake` is not the only door onto a daemon's socket: an editor
//! speaks `initialize` instead, and that request carries a client-chosen
//! workspace root too. These tests hold that door to the same rule the
//! handshake holds — a bound daemon serves its own root and refuses any other —
//! and hold the unbound in-process server to the opposite one, because there the
//! client and the server are the same program.

use std::str::FromStr;
use std::time::Duration;
use std::{path::Path, thread};

use lsp_server::{Connection, Message, Request, RequestId};
use lsp_types::{InitializeParams, Uri};
use oxabl_daemon::SessionHost;

const WINDOW: Duration = Duration::from_millis(20);

fn initialize_naming(root: &Path) -> Request {
    #[allow(deprecated)] // `root_uri` is how a large share of clients still speak.
    let params = InitializeParams {
        root_uri: Some(Uri::from_str(&format!("file://{}", root.display())).unwrap()),
        ..Default::default()
    };
    Request {
        id: RequestId::from(1),
        method: "initialize".to_string(),
        params: serde_json::to_value(params).unwrap(),
    }
}

/// The attack this closes: a client connects to a daemon bound to one project
/// and names another tree, which the shared daemon would then read and index on
/// every other client's behalf.
#[test]
fn an_initialize_naming_another_root_is_refused_on_a_bound_socket() {
    let bound = tempfile::TempDir::new().unwrap();
    let foreign = tempfile::TempDir::new().unwrap();
    let host = SessionHost::new();
    host.bind_root(bound.path()).unwrap();

    let (server, client) = Connection::memory();
    let request = initialize_naming(foreign.path());
    let served = {
        let host = host.clone();
        thread::spawn(move || oxabl_lsp::serve_with_first(&server, WINDOW, request, host))
    };

    let response = loop {
        match client.receiver.recv().unwrap() {
            Message::Response(response) => break response,
            _ => continue,
        }
    };
    let error = response.error.expect("the initialize is refused");
    assert!(response.result.is_none());
    assert_eq!(error.code, -32600);
    assert!(
        error.message.contains(&bound.path().display().to_string())
            && error
                .message
                .contains(&foreign.path().display().to_string()),
        "the refusal names both roots: {}",
        error.message
    );

    served
        .join()
        .unwrap()
        .expect_err("the connection ends on the refusal");

    // The point of the refusal: no session, so nothing of the foreign tree was
    // ever read into this daemon.
    host.with(|sessions| {
        assert!(sessions.get(foreign.path()).is_none());
        assert_eq!(sessions.len(), 0);
    });
}

/// The same root, spelled with a `.` and a `..` that resolve back to it, is the
/// bound root — the refusal must not fire on a spelling (R2).
#[test]
fn an_initialize_spelling_the_bound_root_differently_is_served() {
    let bound = tempfile::TempDir::new().unwrap();
    let inner = bound.path().join("inner");
    std::fs::create_dir(&inner).unwrap();
    let host = SessionHost::new();
    host.bind_root(bound.path()).unwrap();

    let (server, client) = Connection::memory();
    let request = initialize_naming(&inner.join(".."));
    let served = {
        let host = host.clone();
        thread::spawn(move || oxabl_lsp::serve_with_first(&server, WINDOW, request, host))
    };

    let response = loop {
        match client.receiver.recv().unwrap() {
            Message::Response(response) => break response,
            _ => continue,
        }
    };
    assert!(response.error.is_none(), "{:?}", response.error);

    // One session, keyed on the resolved root rather than the spelling that came
    // in — which is what lets a second client on the same tree share it.
    host.with(|sessions| {
        assert_eq!(sessions.len(), 1);
        assert!(sessions.get(bound.path()).is_some());
    });

    shutdown(&client);
    assert!(served.join().unwrap().unwrap());
}

/// The in-process server has no bound root, and must keep anchoring on whatever
/// the client declares — this is the ordinary `oxabl lsp` editor integration.
#[test]
fn an_unbound_host_still_serves_the_root_the_client_declares() {
    let declared = tempfile::TempDir::new().unwrap();
    let host = SessionHost::new();
    assert!(host.bound_root().is_none());

    let (server, client) = Connection::memory();
    let request = initialize_naming(declared.path());
    let served = {
        let host = host.clone();
        thread::spawn(move || oxabl_lsp::serve_with_first(&server, WINDOW, request, host))
    };

    let response = loop {
        match client.receiver.recv().unwrap() {
            Message::Response(response) => break response,
            _ => continue,
        }
    };
    assert!(response.error.is_none(), "{:?}", response.error);
    assert!(response.result.is_some());

    host.with(|sessions| {
        assert!(sessions.get(declared.path()).is_some());
    });

    shutdown(&client);
    assert!(served.join().unwrap().unwrap());
}

fn shutdown(client: &Connection) {
    client
        .sender
        .send(Message::Notification(lsp_server::Notification {
            method: "initialized".to_string(),
            params: serde_json::json!({}),
        }))
        .unwrap();
    client
        .sender
        .send(Message::Request(Request {
            id: RequestId::from(99),
            method: "shutdown".to_string(),
            params: serde_json::Value::Null,
        }))
        .unwrap();
    loop {
        match client.receiver.recv().unwrap() {
            Message::Response(response) if response.id == RequestId::from(99) => break,
            _ => continue,
        }
    }
    client
        .sender
        .send(Message::Notification(lsp_server::Notification {
            method: "exit".to_string(),
            params: serde_json::json!({}),
        }))
        .unwrap();
}
