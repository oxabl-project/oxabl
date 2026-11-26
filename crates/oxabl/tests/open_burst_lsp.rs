//! Several large files that open at once through the real `oxabl lsp` and its
//! daemon. An editor that restores a session sends `didOpen` for each file and,
//! without waiting, the requests it makes for a shown file. The answers are large
//! and the files are large, so both directions of the socket between `oxabl lsp`
//! and the daemon fill. Every request must get its answer: neither process may
//! stop reading one direction while it waits to write the other.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::collections::BTreeMap;
use std::path::Path;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Session, file_uri};

/// The files to open together.
const FILES: usize = 3;

/// The procedures in each file. About 216 KB of source, and answers of several
/// hundred KB, which is more than a socket buffer holds in each direction.
const PROCEDURES: usize = 2000;

/// The time for every answer. A deadlock shows as a missing answer at this limit,
/// not as a test that the runner stops.
const LIMIT: Duration = Duration::from_secs(60);

/// The LSP error code of an answer whose document changed while it was computed.
const CONTENT_MODIFIED: i64 = -32801;

/// The requests an editor makes when it shows a file.
const BURST: [&str; 6] = [
    "textDocument/documentSymbol",
    "textDocument/foldingRange",
    "textDocument/semanticTokens/full",
    "textDocument/codeLens",
    "textDocument/inlayHint",
    "textDocument/documentLink",
];

fn large_source() -> String {
    (0..PROCEDURES)
        .map(|i| {
            format!(
                "DEFINE VARIABLE v{i} AS INTEGER NO-UNDO.\nv{i} = {i} + 1.\n\
                 PROCEDURE p{i}:\n    MESSAGE v{i}.\nEND PROCEDURE.\n"
            )
        })
        .collect()
}

fn capabilities() -> Value {
    json!({
        "textDocument": {
            "documentSymbol": {"hierarchicalDocumentSymbolSupport": true},
            "semanticTokens": {
                "requests": {"full": {"delta": true}, "range": true},
                "tokenTypes": [], "tokenModifiers": [], "formats": ["relative"]
            },
            "inlayHint": {}, "codeLens": {}, "foldingRange": {}, "documentLink": {}
        },
        "workspace": {"configuration": true}
    })
}

fn params(method: &str, uri: &str) -> Value {
    let document = json!({"textDocument": {"uri": uri}});
    if method == "textDocument/inlayHint" {
        let mut params = document;
        params["range"] = json!({
            "start": {"line": 0, "character": 0},
            "end": {"line": 80, "character": 0}
        });
        params
    } else {
        document
    }
}

#[test]
fn large_files_opened_together_get_every_answer() {
    let root = tempfile::tempdir().unwrap();
    let root = root.path();
    std::fs::write(root.join("oxabl.toml"), "[workspace]\nname = \"burst\"\n").unwrap();
    let text = large_source();
    let paths: Vec<_> = (1..=FILES)
        .map(|n| root.join(format!("big{n}.p")))
        .collect();
    for path in &paths {
        std::fs::write(path, &text).unwrap();
    }

    let mut session = Session::start_with_options(
        root,
        capabilities(),
        json!({"inlayHints": true, "semanticTokens": true}),
    );

    let mut messages = Vec::new();
    let mut wanted = BTreeMap::new();
    for path in &paths {
        let uri = file_uri(path);
        messages.push(json!({
            "jsonrpc": "2.0", "method": "textDocument/didOpen",
            "params": {"textDocument": {
                "uri": uri, "languageId": "abl", "version": 1, "text": text
            }}
        }));
        for method in BURST {
            let id = session.next_request_id();
            messages.push(json!({
                "jsonrpc": "2.0", "id": id, "method": method, "params": params(method, &uri)
            }));
            wanted.insert(id, format!("{method} of {}", display_name(path)));
        }
    }
    let _writer = session.send_detached(messages);

    let deadline = Instant::now() + LIMIT;
    while !wanted.is_empty() {
        let left = deadline.saturating_duration_since(Instant::now());
        let Some(message) = session.try_next(left) else {
            assert!(
                Instant::now() >= deadline,
                "`oxabl lsp` closed its output with {} requests not answered",
                wanted.len()
            );
            break;
        };
        if message.get("method").is_some() {
            continue;
        }
        if let Some(id) = message["id"].as_u64()
            && let Some(request) = wanted.remove(&id)
        {
            // The open of the next file can replace an analysis in flight. The
            // server then says so (`ContentModified`) and the editor asks again.
            let replaced = message["error"]["code"] == json!(CONTENT_MODIFIED);
            assert!(
                message.get("result").is_some() != message.get("error").is_some()
                    && (message.get("error").is_none() || replaced),
                "the answer to {request} must be a result or `ContentModified`: {message}"
            );
        }
    }
    assert!(
        wanted.is_empty(),
        "no answer within {LIMIT:?} to {} of {} requests: {:?}",
        wanted.len(),
        FILES * BURST.len(),
        wanted.values().collect::<Vec<_>>()
    );
}

fn display_name(path: &Path) -> String {
    path.file_name().unwrap().to_string_lossy().into_owned()
}
