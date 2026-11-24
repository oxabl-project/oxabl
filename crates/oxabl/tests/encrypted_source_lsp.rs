//! XCODE-encrypted source through the real `oxabl lsp`: opening one gives one
//! warning and no parse, preprocessor or lint finding, and every request still
//! gets an answer. Fixtures are synthetic: a 10-byte header and random bytes
//! made here.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::fs;
use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

fn encrypted_bytes() -> Vec<u8> {
    let mut bytes = vec![0x13, 0x59, 0x2f, 0x25, 0x07, 0x09, 0x31, 0x49, 0x3b, 0x25];
    let mut state = 0x2545_f491_4f6c_dd1d_u64;
    for _ in 0..400 {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        bytes.push((state >> 24) as u8);
    }
    bytes
}

/// The text an editor shows for the bytes: invalid UTF-8 becomes U+FFFD.
fn editor_text() -> String {
    String::from_utf8_lossy(&encrypted_bytes()).into_owned()
}

const CONFIG: &str = "[workspace]\nname = \"t\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

fn tree() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    fs::write(dir.path().join("enc.i"), encrypted_bytes()).unwrap();
    dir
}

/// The first diagnostics published for `path`.
fn published(session: &Session, path: &Path) -> Vec<Value> {
    let uri = json!(file_uri(path));
    let message = session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == uri
    });
    message["params"]["diagnostics"].as_array().unwrap().clone()
}

#[track_caller]
fn assert_one_encrypted_warning(diagnostics: &[Value]) {
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0]["code"], "SOURCE001");
    assert_eq!(diagnostics[0]["severity"], 2, "a warning");
    assert!(
        diagnostics[0]["message"]
            .as_str()
            .unwrap()
            .contains("encrypted, not checked"),
        "{diagnostics:?}"
    );
}

#[test]
fn an_encrypted_buffer_gets_one_warning_and_every_request_an_answer() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let path = dir.path().join("enc.p");
    session.open(&path, &editor_text());
    assert_one_encrypted_warning(&published(&session, &path));

    let uri = file_uri(&path);
    let symbols = session.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": uri}}),
    );
    assert!(symbols.get("error").is_none(), "{symbols}");
    let edits = session.request(
        "textDocument/formatting",
        json!({"textDocument": {"uri": uri}, "options": {"tabSize": 4, "insertSpaces": true}}),
    );
    assert!(edits.get("error").is_none(), "{edits}");
    assert!(
        edits["result"].is_null() || edits["result"] == json!([]),
        "an encrypted buffer is never reformatted: {edits}"
    );

    // The session stays usable.
    let plain = dir.path().join("plain.p");
    session.open(&plain, "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    let diagnostics = published(&session, &plain);
    assert!(
        diagnostics.iter().any(|d| d["code"] == "LINT0002"),
        "{diagnostics:?}"
    );
}

#[test]
fn an_encrypted_include_gives_the_includer_buffer_one_warning() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let root = dir.path().join("root.p");
    session.open(&root, "{enc.i}\nMESSAGE \"x\".\n");
    let diagnostics = published(&session, &root);
    assert_one_encrypted_warning(&diagnostics);
    assert!(
        diagnostics[0]["message"]
            .as_str()
            .unwrap()
            .contains("include 'enc.i'"),
        "{diagnostics:?}"
    );
}

/// Text that parses and would be reformatted, but for the control byte in its
/// comment, which makes the detector name it encrypted.
#[test]
fn formatting_leaves_alone_text_that_is_named_encrypted() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let format = |session: &mut Session, path: &Path| {
        session.request(
            "textDocument/formatting",
            json!({
                "textDocument": {"uri": file_uri(path)},
                "options": {"tabSize": 4, "insertSpaces": true}
            }),
        )
    };
    let body = "IF TRUE THEN DO:\nMESSAGE \"hi\".\nEND.\n";

    let plain = dir.path().join("plain.p");
    session.open(&plain, &format!("/* note */\n{body}"));
    let edits = format(&mut session, &plain);
    assert_eq!(
        edits["result"].as_array().map(Vec::len),
        Some(1),
        "the control: without the byte the text is reformatted: {edits}"
    );

    let marked = dir.path().join("marked.p");
    session.open(&marked, &format!("/* \u{7} */\n{body}"));
    let edits = format(&mut session, &marked);
    assert!(
        edits["result"].is_null() || edits["result"] == json!([]),
        "{edits}"
    );
}
