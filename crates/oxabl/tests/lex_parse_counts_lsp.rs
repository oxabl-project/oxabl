//! One lex and one parse of an open buffer for each revision, for any number
//! of feature requests, through the real `oxabl lsp` and its daemon
//! (`docs/design/shared-unit.md`, "The daemon").
//!
//! A test build counts what it lexes and parses, and the daemon answers the
//! test-only request `oxabl/testLexParseCounts` with the counts of its
//! process. The workspace holds only the files of the test and no request
//! starts a workspace pass, so every lex and parse between two probes is one
//! of the buffer and its include. The outline with includes outlines the
//! include file alone, which is a text of its own.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::fmt::Write as _;
use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

/// A program that expands to itself: no include, no directive, no reference.
fn program(procedures: usize) -> String {
    let mut text = String::from(
        "DEFINE VARIABLE total AS INTEGER NO-UNDO.\n\
         FUNCTION Twice RETURNS INTEGER (INPUT piValue AS INTEGER, INPUT pcLabel AS CHARACTER):\n\
         \x20   RETURN piValue * 2.\n\
         END FUNCTION.\n",
    );
    for k in 0..procedures {
        write!(
            text,
            "PROCEDURE p{k}:\n\
             \x20   DEFINE INPUT PARAMETER piLimit AS INTEGER NO-UNDO.\n\
             \x20   DEFINE VARIABLE cLocal AS CHARACTER NO-UNDO.\n\
             \x20   total = Twice(piLimit, cLocal) + {k}.\n\
             \x20   IF total > piLimit THEN\n\
             \x20       RUN p{next} (INPUT total).\n\
             END PROCEDURE.\n",
            next = (k + 1) % procedures
        )
        .unwrap();
    }
    text
}

const INCLUDE: &str = "DEFINE VARIABLE fromInclude AS INTEGER NO-UNDO.\n";

const SCHEMA_DF: &str = "ADD TABLE \"Customer\"\n\
  ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\
  ADD INDEX \"pk\" ON \"Customer\" IS PRIMARY UNIQUE\n\
  INDEX-FIELD \"CustNum\" ASCENDING\n";

#[derive(Debug, Clone, Copy)]
struct Counts {
    lexed: u64,
    parses: u64,
    parsed: u64,
}

impl Counts {
    fn since(self, before: Counts) -> Counts {
        Counts {
            lexed: self.lexed - before.lexed,
            parses: self.parses - before.parses,
            parsed: self.parsed - before.parsed,
        }
    }
}

fn counts(session: &mut Session) -> Counts {
    let answer = session.request("oxabl/testLexParseCounts", Value::Null);
    let result = &answer["result"];
    let field = |name: &str| {
        result[name]
            .as_u64()
            .unwrap_or_else(|| panic!("no `{name}` in the counts: {answer}"))
    };
    Counts {
        lexed: field("lexedBytes"),
        parses: field("parses"),
        parsed: field("parsedBytes"),
    }
}

fn init_params(outline_includes: bool) -> Value {
    json!({
        "capabilities": {
            "textDocument": {
                "inlayHint": {},
                "codeLens": {},
                "semanticTokens": {
                    "requests": {"range": true, "full": {"delta": true}},
                    "tokenTypes": [], "tokenModifiers": [], "formats": ["relative"]
                }
            }
        },
        "initializationOptions": {
            "inlayHints": {
                "parameters": true
            },
            "outline": {
                "includes": outline_includes
            }
        }
    })
}

/// Open `path` with `text`, wait for its diagnostics, and ask for every
/// feature that reads the units of the buffer, `rounds` times. Gives what the
/// daemon lexed and parsed from just before the open.
fn features_of_one_revision(
    root: &Path,
    path: &Path,
    text: &str,
    rounds: usize,
    outline_includes: bool,
) -> Counts {
    std::fs::write(path, text).unwrap();
    let (mut session, _) = Session::start_with_params(root, init_params(outline_includes));
    let before = counts(&mut session);
    session.open(path, text);
    let uri = file_uri(path);
    session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == json!(uri)
    });
    let document = json!({"uri": uri});
    let whole = json!({"start": {"line": 0, "character": 0},
                       "end": {"line": 100_000, "character": 0}});
    let at = json!({"line": 5, "character": 8});
    let requests = [
        (
            "textDocument/documentSymbol",
            json!({"textDocument": document}),
        ),
        (
            "textDocument/foldingRange",
            json!({"textDocument": document}),
        ),
        (
            "textDocument/selectionRange",
            json!({"textDocument": document, "positions": [at]}),
        ),
        (
            "textDocument/semanticTokens/full",
            json!({"textDocument": document}),
        ),
        (
            "textDocument/semanticTokens/range",
            json!({"textDocument": document, "range": whole}),
        ),
        (
            "textDocument/documentLink",
            json!({"textDocument": document}),
        ),
        (
            "textDocument/documentHighlight",
            json!({"textDocument": document, "position": at}),
        ),
        (
            "textDocument/inlayHint",
            json!({"textDocument": document, "range": whole}),
        ),
        ("textDocument/codeLens", json!({"textDocument": document})),
        (
            "textDocument/formatting",
            json!({"textDocument": document,
                   "options": {"tabSize": 4, "insertSpaces": true}}),
        ),
    ];
    for round in 0..rounds {
        for (method, params) in &requests {
            let answer = session.request(method, params.clone());
            assert!(
                answer.get("result").is_some(),
                "`{method}` failed: {answer}"
            );
            if round == 0 && *method == "textDocument/inlayHint" {
                let hints = answer["result"]
                    .as_array()
                    .expect("inlayHint result is an array");
                assert!(
                    hints.iter().any(|h| h["label"] == json!("piValue:")),
                    "expected parameter-name hints to be active and non-empty: {hints:?}"
                );
            }
            if round == 0 && *method == "textDocument/codeLens" {
                let mut lenses = answer["result"]
                    .as_array()
                    .expect("codeLens result is an array")
                    .clone();
                for _ in 0..100 {
                    if !lenses.is_empty() {
                        break;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(20));
                    let retry = session.request(method, params.clone());
                    lenses = retry["result"]
                        .as_array()
                        .expect("codeLens result is an array")
                        .clone();
                }
                assert!(
                    !lenses.is_empty(),
                    "expected effect lenses (build_effect_summaries_of) to be active"
                );
            }
        }
    }
    counts(&mut session).since(before)
}

/// A new workspace, so that no daemon of an earlier round keeps the buffer.
fn workspace(include: bool) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let config = "[workspace]\n\
                  name = \"units\"\n\
                  [workspace.sources]\n\
                  include_paths = [\".\"]\n\
                  [workspace.schema]\n\
                  files = [\"db.df\"]\n\
                  [workspace.lint]\n\
                  enable = [\"strict.access\", \"strict.transactions\"]\n";
    std::fs::write(dir.path().join("oxabl.toml"), config).unwrap();
    std::fs::write(dir.path().join("db.df"), SCHEMA_DF).unwrap();
    if include {
        std::fs::write(dir.path().join("defs.i"), INCLUDE).unwrap();
    }
    dir
}

#[test]
fn a_buffer_that_expands_to_itself_is_lexed_and_parsed_once() {
    let mut text = program(200);
    text.push_str(
        "PROCEDURE dbWork:\n\
         \x20   DO TRANSACTION:\n\
         \x20       FIND FIRST Customer NO-LOCK.\n\
         \x20   END.\n\
         END PROCEDURE.\n",
    );
    let len = text.len() as u64;
    for (rounds, outline_includes) in [(1, true), (3, true), (1, false)] {
        let dir = workspace(false);
        let path = dir.path().join("main.p");
        let counts = features_of_one_revision(dir.path(), &path, &text, rounds, outline_includes);
        assert_eq!(
            counts.parses, 1,
            "{rounds} rounds (outline_includes={outline_includes}) parse the buffer once: {counts:?}"
        );
        assert_eq!(counts.parsed, len, "the parse is of the buffer: {counts:?}");
        assert!(
            counts.lexed >= len && counts.lexed < len + len / 2,
            "{rounds} rounds (outline_includes={outline_includes}) lex the buffer of {len} bytes once: {counts:?}"
        );
    }
}

#[test]
fn a_buffer_with_an_include_is_lexed_and_parsed_once_as_written_and_once_expanded() {
    const REFERENCE: &str = "{defs.i}";
    let mut text = format!("{REFERENCE}\n{}", program(200));
    text.push_str(
        "PROCEDURE dbWork:\n\
         \x20   DO TRANSACTION:\n\
         \x20       FIND FIRST Customer NO-LOCK.\n\
         \x20   END.\n\
         END PROCEDURE.\n",
    );
    let len = text.len() as u64;
    let expanded = len - REFERENCE.len() as u64 + INCLUDE.len() as u64;
    // The outline with includes outlines the include file too: a text of its own.
    let texts = len + expanded + INCLUDE.len() as u64;
    for rounds in [1, 3] {
        let dir = workspace(true);
        let path = dir.path().join("main.p");
        let counts = features_of_one_revision(dir.path(), &path, &text, rounds, true);
        assert_eq!(
            (counts.parses, counts.parsed),
            (3, texts),
            "{rounds} rounds of requests parse the buffer as written ({len} bytes), its \
             expansion ({expanded} bytes) and the include once each: {counts:?}"
        );
        assert!(
            counts.lexed >= texts && counts.lexed < texts + len / 2,
            "{rounds} rounds of requests lex the buffer as written, its expansion and the \
             include once each ({texts} bytes): {counts:?}"
        );
    }
}

/// A preprocessor panic (`OXABL-TEST-PANIC:analyze`) inside `expanded_text`
/// must not crash the LSP main loop or break raw-text features (outline,
/// folding, selection ranges, formatting).
#[test]
fn a_preprocessor_panic_does_not_crash_raw_unit_features() {
    let dir = workspace(false);
    let path = dir.path().join("panic_analyze.p");
    let text = "/* OXABL-TEST-PANIC:analyze */\n\
                PROCEDURE p1:\n\
                \x20   MESSAGE \"ok\".\n\
                END PROCEDURE.\n";
    let (mut session, _) = Session::start_with_params(dir.path(), init_params(false));
    session.open(&path, text);
    let uri = file_uri(&path);
    let document = json!({"uri": uri});
    let at = json!({"line": 2, "character": 6});
    for (method, params) in [
        (
            "textDocument/documentSymbol",
            json!({"textDocument": document}),
        ),
        (
            "textDocument/foldingRange",
            json!({"textDocument": document}),
        ),
        (
            "textDocument/selectionRange",
            json!({"textDocument": document, "positions": [at]}),
        ),
        (
            "textDocument/formatting",
            json!({"textDocument": document,
                   "options": {"tabSize": 4, "insertSpaces": true}}),
        ),
    ] {
        let answer = session.request(method, params);
        assert!(
            answer.get("result").is_some(),
            "`{method}` should succeed despite an analyze panic: {answer}"
        );
    }
}
