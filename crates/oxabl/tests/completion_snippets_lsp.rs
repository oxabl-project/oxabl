//! The snippets of the server in the completion list: where each is offered,
//! the project's keyword case in their text, and the client that cannot use
//! them.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Answer, Case, Files, KEYWORD, SNIPPET, capabilities_with_snippets, new_text};
use serde_json::json;

/// S1.
#[test]
fn a_procedure_file_statement_start_offers_the_snippets() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", "|");
    let list = answer.list();
    list.contains(
        SNIPPET,
        &[
            "find", "foreach", "defvar", "deftt", "proc", "function", "trycatch",
        ],
    );
    list.lacks(&["method", "class"]);
    for item in list.items.iter().filter(|i| i["kind"] == json!(SNIPPET)) {
        assert_eq!(item["insertTextFormat"], json!(2), "{item}");
    }
    let text = new_text(list.item(SNIPPET, "defvar"));
    assert!(
        text.starts_with("DEFINE VARIABLE ${1:cName} AS "),
        "the snippet in the project's case: {text}"
    );
    assert!(
        !text.contains("<<") && !text.contains(">>"),
        "no keyword marker is left: {text}"
    );
}

/// S2.
#[test]
fn the_snippets_follow_the_lower_case_style() {
    let mut ws = Files::config_n(Case::Lower).start();
    let answer = ws.complete("main.p", "|");
    let text = new_text(answer.list().item(SNIPPET, "defvar")).to_string();
    assert!(text.starts_with("define variable ${1:cName} as "), "{text}");
}

/// S3.
#[test]
fn a_class_body_offers_the_method_snippet_only() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("Box.cls", "CLASS Box:\n    |\nEND CLASS.\n");
    let list = answer.list();
    list.contains(SNIPPET, &["method"]);
    list.lacks(&["find", "proc", "class", "function"]);
}

/// S4.
#[test]
fn a_class_file_header_offers_the_class_snippet() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("Box.cls", "|\nCLASS Box:\nEND CLASS.\n");
    let list = answer.list();
    list.contains(SNIPPET, &["class"]);
    list.lacks(&["method"]);
}

/// S5.
#[test]
fn a_client_without_snippet_support_gets_no_snippet() {
    let mut ws = Files::config_n(Case::Upper).start_with(capabilities_with_snippets(false));
    let answer = ws.complete("main.p", "|");
    let list = answer.list();
    assert!(
        list.find(KEYWORD, "DEFINE").is_some(),
        "the keywords are still offered: {}",
        list.summary()
    );
    list.has_no_kind(SNIPPET);
    for item in &list.items {
        assert_ne!(item["insertTextFormat"], json!(2), "{item}");
    }
}

/// S6.
#[test]
fn a_snippet_is_not_offered_away_from_a_statement_start() {
    let mut ws = Files::config_a(Case::Upper).start();
    match ws.complete("main.p", "FIND FIRST |") {
        Answer::Null => {}
        Answer::List(list) => list.has_no_kind(SNIPPET),
    }
}

/// S7.
#[test]
fn a_snippet_is_one_item_of_the_list() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", "|");
    let count = answer
        .list()
        .items
        .iter()
        .filter(|i| i["label"] == json!("trycatch"))
        .count();
    assert_eq!(count, 1, "{}", answer.summary());
}
