//! `textDocument/completion` in an include file through the real `oxabl lsp`:
//! the names of the selected includer context, the fragment mode, an include
//! that a root reads twice, a root that stops reading the include, and an
//! include that starts inside a statement.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use std::path::Path;

use completion::{Case, Files, VARIABLE, Ws};
use serde_json::json;
use session::file_uri;

/// The selection of `root` as the includer context of `include`, or none.
#[track_caller]
fn select(ws: &mut Ws, include: &str, root: Option<&Path>) {
    let answer = ws.session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&ws.path(include))}, "root": root}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
}

const DEFS_USER: &str = "DEFINE VARIABLE gcRoot AS CHARACTER NO-UNDO.\n{inc/body.i}\n";
const BODY: &str = "DEFINE VARIABLE iInInclude AS INTEGER NO-UNDO.\n|";

fn body_workspace() -> Ws {
    Files::config_a(Case::Upper)
        .file("defs_user.p", DEFS_USER)
        .file("inc/body.i", &BODY.replace('|', ""))
        .start()
}

/// I1.
#[test]
fn an_include_in_a_selected_root_context_sees_the_names_of_the_root() {
    let mut ws = body_workspace();
    let cursor = ws.open_marked("inc/body.i", BODY);
    let root = ws.canonical("defs_user.p");
    select(&mut ws, "inc/body.i", Some(&root));
    let answer = ws.complete_at("inc/body.i", cursor, None);
    answer.list().contains(VARIABLE, &["gcRoot", "iInInclude"]);
}

/// I2.
#[test]
fn an_include_in_fragment_mode_gives_an_incomplete_list() {
    let mut ws = body_workspace();
    let answer = ws.complete("inc/body.i", BODY);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(VARIABLE, &["iInInclude"]);
    list.lacks(&["gcRoot"]);

    // A selection of `null` is the fragment mode too.
    let mut ws = body_workspace();
    let cursor = ws.open_marked("inc/body.i", BODY);
    select(&mut ws, "inc/body.i", None);
    let answer = ws.complete_at("inc/body.i", cursor, None);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(VARIABLE, &["iInInclude"]);
    list.lacks(&["gcRoot"]);
}

/// I3.
#[test]
fn an_include_that_one_root_reads_twice_gives_the_names_of_both_places() {
    let mut ws = Files::config_a(Case::Upper)
        .file(
            "twice.p",
            "PROCEDURE a:
    DEFINE VARIABLE iA AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBoth AS INTEGER NO-UNDO.
    {inc/use.i}
END PROCEDURE.
PROCEDURE b:
    DEFINE VARIABLE iB AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBoth AS INTEGER NO-UNDO.
    {inc/use.i}
END PROCEDURE.
",
        )
        .file("inc/use.i", "iBoth = 1.\n")
        .start();
    let cursor = ws.open_marked("inc/use.i", "iBoth = 1.\n|");
    let root = ws.canonical("twice.p");
    select(&mut ws, "inc/use.i", Some(&root));
    let answer = ws.complete_at("inc/use.i", cursor, None);
    let list = answer.list();
    list.contains(VARIABLE, &["iBoth"]);
    list.lacks(&["iA", "iB"]);
}

/// I4.
#[test]
fn a_root_that_no_longer_reads_the_include_gives_no_list() {
    let mut ws = body_workspace();
    let cursor = ws.open_marked("inc/body.i", BODY);
    let root = ws.canonical("defs_user.p");
    select(&mut ws, "inc/body.i", Some(&root));
    ws.complete_at("inc/body.i", cursor, None)
        .list()
        .contains(VARIABLE, &["gcRoot", "iInInclude"]);

    let path = ws.path("defs_user.p");
    ws.session
        .open(&path, "DEFINE VARIABLE gcRoot AS CHARACTER NO-UNDO.\n");
    ws.complete_at("inc/body.i", cursor, None).assert_null();
}

/// I5.
#[test]
fn an_include_that_starts_inside_a_statement_gives_no_list() {
    let mut ws = Files::config_a(Case::Upper)
        .file(
            "mid.p",
            "FIND FIRST Customer\n  {inc/where.i}\n  NO-LOCK NO-ERROR.\n",
        )
        .file("inc/where.i", "WHERE Customer.\n")
        .start();
    let cursor = ws.open_marked("inc/where.i", "WHERE Customer.|\n");
    ws.complete_at("inc/where.i", cursor, None).assert_null();

    let root = ws.canonical("mid.p");
    select(&mut ws, "inc/where.i", Some(&root));
    ws.complete_at("inc/where.i", cursor, None).assert_null();
}
