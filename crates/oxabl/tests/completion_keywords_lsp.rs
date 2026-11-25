//! `textDocument/completion` of statement keywords through the real `oxabl lsp`:
//! the capability, the keyword case, the word that filters the list, the replace
//! range, the keywords of each scope kind, the places that give no list, and
//! `completionItem/resolve` of a keyword.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Case, Files, KEYWORD, List, documentation, label};
use serde_json::{Value, json};
use session::file_uri;

/// The list answered for `marked` in `rel` of the workspace `files`; a `null`
/// answer fails the test.
#[track_caller]
fn keywords(files: Files, rel: &str, marked: &str) -> List {
    match files.start().complete(rel, marked) {
        completion::Answer::List(list) => list,
        completion::Answer::Null => panic!("a list expected for {marked:?}, got `null`"),
    }
}

/// K1.
#[test]
fn the_completion_capability_is_advertised() {
    let ws = Files::config_n(Case::Unset).start();
    let provider = &ws.session.initialized["result"]["capabilities"]["completionProvider"];
    assert!(provider.is_object(), "{}", ws.session.initialized);
    assert_eq!(provider["resolveProvider"], json!(true), "{provider}");
    let triggers: Vec<&str> = provider["triggerCharacters"]
        .as_array()
        .unwrap_or_else(|| panic!("no `triggerCharacters` in {provider}"))
        .iter()
        .map(|t| t.as_str().unwrap())
        .collect();
    for wanted in [":", ".", "{", "&", "/"] {
        assert!(
            triggers.contains(&wanted),
            "trigger `{wanted}` missing in {triggers:?}"
        );
    }
}

const K2: &str = "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n|";

/// K2.
#[test]
fn a_statement_start_in_a_procedure_file_offers_the_statements() {
    let list = keywords(Files::config_n(Case::Upper), "main.p", K2);
    list.contains(
        KEYWORD,
        &[
            "DEFINE",
            "FIND",
            "FOR",
            "RUN",
            "MESSAGE",
            "IF",
            "DO",
            "REPEAT",
            "ASSIGN",
            "PROCEDURE",
            "FUNCTION",
        ],
    );
    list.lacks_kind(
        KEYWORD,
        &[
            "METHOD",
            "CONSTRUCTOR",
            "DESTRUCTOR",
            "CLASS",
            "INTERFACE",
            "WHEN",
            "OTHERWISE",
        ],
    );
    for word in list.labels_of(KEYWORD) {
        assert_eq!(word, word.to_uppercase(), "a keyword in upper case");
    }
}

/// K3.
#[test]
fn the_word_filters_the_keywords() {
    let list = keywords(
        Files::config_n(Case::Upper),
        "main.p",
        "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\nfi|",
    );
    assert!(!list.incomplete, "{}", list.summary());
    list.contains(KEYWORD, &["FIND"]);
    list.lacks_kind(KEYWORD, &["DEFINE", "RUN", "MESSAGE"]);
}

fn range(start: u32, end: u32) -> Value {
    json!({
        "start": {"line": 1, "character": start},
        "end": {"line": 1, "character": end}
    })
}

/// K4.
#[test]
fn the_edit_replaces_the_word() {
    let list = keywords(
        Files::config_n(Case::Upper),
        "main.p",
        "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\nfi|",
    );
    let edit = &list.item(KEYWORD, "FIND")["textEdit"];
    assert_eq!(edit["newText"], json!("FIND"), "{edit}");
    assert_eq!(edit["insert"], range(0, 2), "{edit}");
    assert_eq!(edit["replace"], range(0, 2), "{edit}");

    let list = keywords(
        Files::config_n(Case::Upper),
        "main.p",
        "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\nfi|xx",
    );
    let edit = &list.item(KEYWORD, "FIND")["textEdit"];
    assert_eq!(edit["newText"], json!("FIND"), "{edit}");
    assert_eq!(edit["insert"], range(0, 2), "{edit}");
    assert_eq!(edit["replace"], range(0, 4), "{edit}");
}

/// K5.
#[test]
fn lower_case_keywords_follow_the_project_style() {
    let list = keywords(Files::config_n(Case::Lower), "main.p", K2);
    list.contains(KEYWORD, &["define", "find"]);
    for word in list.labels_of(KEYWORD) {
        assert_eq!(word, word.to_lowercase(), "a keyword in lower case");
    }
}

/// K6.
#[test]
fn preserve_follows_the_typed_word_then_the_nearest_keyword() {
    let list = keywords(Files::config_n(Case::Preserve), "main.p", "def|");
    list.contains(KEYWORD, &["define"]);
    list.lacks_kind(KEYWORD, &["DEFINE"]);

    let list = keywords(Files::config_n(Case::Preserve), "main.p", "DEF|");
    list.contains(KEYWORD, &["DEFINE"]);
    list.lacks_kind(KEYWORD, &["define"]);

    let list = keywords(
        Files::config_n(Case::Preserve),
        "main.p",
        "define variable x as integer no-undo.\n|",
    );
    list.contains(KEYWORD, &["define"]);
    list.lacks_kind(KEYWORD, &["DEFINE"]);

    let list = keywords(Files::config_n(Case::Preserve), "main.p", "|");
    list.contains(KEYWORD, &["DEFINE"]);
}

/// K7.
#[test]
fn no_style_table_keeps_the_case_of_the_code() {
    let list = keywords(Files::config_n(Case::Unset), "main.p", "MESSAGE \"a\".\n|");
    list.contains(KEYWORD, &["DEFINE"]);
    list.lacks_kind(KEYWORD, &["define"]);

    let list = keywords(Files::config_n(Case::Unset), "main.p", "message \"a\".\n|");
    list.contains(KEYWORD, &["define"]);
    list.lacks_kind(KEYWORD, &["DEFINE"]);
}

/// K8.
#[test]
fn the_keywords_after_an_abbreviated_define() {
    let list = keywords(Files::config_n(Case::Upper), "main.p", "DEF |");
    list.contains(
        KEYWORD,
        &[
            "VARIABLE",
            "BUFFER",
            "TEMP-TABLE",
            "PARAMETER",
            "NEW",
            "INPUT",
        ],
    );
    list.lacks_kind(KEYWORD, &["FIND", "METHOD", "DEFINE"]);

    let list = keywords(Files::config_n(Case::Preserve), "main.p", "def va|");
    list.contains(KEYWORD, &["variable"]);

    let list = keywords(
        Files::config_n(Case::Upper),
        "main.p",
        "DEFINE NEW SHARED |",
    );
    list.contains(KEYWORD, &["VARIABLE"]);
    list.lacks_kind(KEYWORD, &["NEW"]);
}

/// K9.
#[test]
fn a_class_body_offers_the_member_keywords() {
    let list = keywords(
        Files::config_n(Case::Upper),
        "Box.cls",
        "CLASS Box:\n    |\nEND CLASS.\n",
    );
    list.contains(KEYWORD, &["DEFINE", "METHOD", "CONSTRUCTOR", "DESTRUCTOR"]);
    list.lacks_kind(
        KEYWORD,
        &[
            "FIND",
            "FOR",
            "RUN",
            "PROCEDURE",
            "FUNCTION",
            "CLASS",
            "USING",
        ],
    );
}

/// K10.
#[test]
fn a_class_file_before_the_class_offers_the_header_keywords() {
    let list = keywords(
        Files::config_n(Case::Upper),
        "Box.cls",
        "|\nCLASS Box:\nEND CLASS.\n",
    );
    list.contains(KEYWORD, &["USING", "CLASS", "BLOCK-LEVEL"]);
    list.lacks_kind(KEYWORD, &["FIND", "METHOD", "DEFINE"]);
}

/// K11.
#[test]
fn a_method_body_offers_the_statements() {
    let list = keywords(
        Files::config_n(Case::Upper),
        "Box.cls",
        "CLASS Box:\n    METHOD PUBLIC VOID Fill():\n        |\n    END METHOD.\nEND CLASS.\n",
    );
    list.contains(KEYWORD, &["FIND", "DEFINE", "RETURN"]);
    list.lacks_kind(
        KEYWORD,
        &[
            "METHOD",
            "CONSTRUCTOR",
            "PROCEDURE",
            "FUNCTION",
            "CLASS",
            "USING",
        ],
    );
}

/// K12.
#[test]
fn a_procedure_body_offers_the_statements_with_or_without_its_end() {
    let list = keywords(
        Files::config_n(Case::Upper),
        "main.p",
        "PROCEDURE p:\n    |\nEND PROCEDURE.\n",
    );
    list.contains(KEYWORD, &["FIND"]);
    list.lacks_kind(KEYWORD, &["PROCEDURE", "FUNCTION", "METHOD"]);

    let list = keywords(
        Files::config_n(Case::Upper),
        "main.p",
        "PROCEDURE p:\n    DEFINE VARIABLE x AS INTEGER NO-UNDO.\n    |",
    );
    list.contains(KEYWORD, &["FIND"]);
    list.lacks_kind(KEYWORD, &["PROCEDURE"]);
}

/// K13.
#[test]
fn where_oxabl_does_not_know_the_context_there_is_no_list() {
    let cases: [(&str, Option<&str>); 6] = [
        ("/* fi| */", None),
        ("MESSAGE \"fi|\".", None),
        (
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nFIND FIRST x NO-LOCK |",
            None,
        ),
        ("DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = x |", None),
        ("MESSAGE \"a\".|", Some(".")),
        ("DO:|\nEND.\n", Some(":")),
    ];
    for (marked, trigger) in cases {
        let mut ws = Files::config_n(Case::Upper).start();
        let answer = match trigger {
            None => ws.complete("main.p", marked),
            Some(t) => ws.complete_trigger("main.p", marked, t),
        };
        assert!(
            matches!(answer, completion::Answer::Null),
            "`null` expected for {marked:?}, got {}",
            answer.summary()
        );
    }
}

/// K14.
#[test]
fn a_keyword_resolves_to_its_documentation() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", K2);
    let item = answer.list().item(KEYWORD, "FIND").clone();
    let resolved = ws.resolve(&item);
    let text = documentation(&resolved);
    assert!(text.contains("FIND statement"), "{text}");
    assert!(text.contains("https://docs-be.progress.com/"), "{text}");
    for field in ["label", "kind", "textEdit"] {
        assert_eq!(resolved[field], item[field], "`{field}` is not changed");
    }
    assert_eq!(label(&resolved), "FIND");
}

/// K15.
#[test]
fn a_cancelled_request_answers_normally_or_with_request_cancelled() {
    let mut ws = Files::config_n(Case::Upper).start();
    let cursor = ws.open_marked("main.p", K2);
    let id = ws.session.next_request_id();
    ws.session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/completion",
        "params": {
            "textDocument": {"uri": file_uri(&ws.path("main.p"))},
            "position": cursor.json(),
            "context": {"triggerKind": 1}
        }
    }));
    ws.session.send(json!({
        "jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}
    }));
    let response = ws.session.wait_for(|m| m["id"] == json!(id));
    match response.get("error") {
        None => assert!(
            response.get("result").is_some(),
            "a response holds a result or an error: {response}"
        ),
        Some(error) => assert_eq!(error["code"], json!(-32800), "{response}"),
    }
}
