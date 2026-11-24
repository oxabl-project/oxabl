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

const HEADER: [u8; 10] = [0x13, 0x59, 0x2f, 0x25, 0x07, 0x09, 0x31, 0x49, 0x3b, 0x25];

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

/// An open buffer of a root that includes an include of an include that is
/// encrypted: one warning at the root's reference, no other finding.
#[test]
fn a_nested_encrypted_include_gives_the_includer_buffer_one_warning() {
    let dir = tree();
    fs::write(dir.path().join("mid.i"), "{enc.i}\n").unwrap();
    let mut session = Session::start(dir.path());
    let root = dir.path().join("nest.p");
    session.open(
        &root,
        "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n{mid.i}\nMESSAGE typo.\n",
    );
    let diagnostics = published(&session, &root);
    assert_one_encrypted_warning(&diagnostics);
    assert_eq!(
        diagnostics[0]["range"]["start"]["line"], 1,
        "{diagnostics:?}"
    );
    assert!(
        diagnostics[0]["message"]
            .as_str()
            .unwrap()
            .contains("include 'enc.i'"),
        "{diagnostics:?}"
    );
}

/// The header with plain text after it, in a file long enough to show that it
/// is not random, is not encrypted: the buffer is analysed, not skipped.
#[test]
fn the_header_with_plain_text_after_it_is_not_skipped() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let path = dir.path().join("marked.p");
    let header = String::from_utf8(HEADER.to_vec()).unwrap();
    session.open(&path, &format!("{header}\n{}", "DISPLAY y.\n".repeat(30)));
    let diagnostics = published(&session, &path);
    assert!(!diagnostics.is_empty(), "the buffer is analysed");
    assert!(
        diagnostics.iter().all(|d| d["code"] != "SOURCE001"),
        "{diagnostics:?}"
    );
}

/// One escape byte in a string of an open buffer does not turn its findings
/// into a skip.
#[test]
fn a_stray_control_byte_in_a_buffer_keeps_its_diagnostics_and_formatting() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let path = dir.path().join("esc.p");
    session.open(
        &path,
        "DEFINE VARIABLE x AS CHARACTER NO-UNDO.\nx = \"\u{1b}E\".\nIF TRUE THEN DO:\nMESSAGE x.\nEND.\nDISPLAY y.\n",
    );
    let diagnostics = published(&session, &path);
    assert!(
        diagnostics.iter().any(|d| d["code"] == "LINT0001"),
        "{diagnostics:?}"
    );
    assert!(diagnostics.iter().all(|d| d["code"] != "SOURCE001"));
    let edits = session.request(
        "textDocument/formatting",
        json!({
            "textDocument": {"uri": file_uri(&path)},
            "options": {"tabSize": 4, "insertSpaces": true}
        }),
    );
    assert_eq!(edits["result"].as_array().map(Vec::len), Some(1), "{edits}");
}
