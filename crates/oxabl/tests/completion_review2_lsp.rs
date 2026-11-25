//! Completion cases from the review of records, paths and preprocessor names:
//! a field that two records of a `WHERE` share, a name an `&UNDEFINE` leaves
//! visible, a procedure in an `&IF` branch that is not compiled, a procedure
//! library on the include path, a define the current text removed, and a
//! match at the start of a name before a fitting one in its middle.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Case, FIELD, FUNCTION, Files, VARIABLE, new_text};

const CONSTANT: u64 = 21;
const FILE: u64 = 17;
const HOLD: &str = "/* OXABL-TEST-HOLD:completion.refresh */\n";

#[test]
fn a_field_two_records_of_a_where_share_is_inserted_with_its_record() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "FOR EACH Customer NO-LOCK, EACH Order NO-LOCK WHERE Order.CustNum = |",
    );
    let list = answer.list();
    list.contains(
        FIELD,
        &["Customer.CustNum", "Order.CustNum", "Name", "OrderNum"],
    );
    list.lacks_kind(FIELD, &["CustNum"]);
    assert_eq!(new_text(list.item(FIELD, "Order.CustNum")), "Order.CustNum");
    assert_eq!(new_text(list.item(FIELD, "Name")), "Name");
}

#[test]
fn a_field_of_an_outer_record_makes_the_bare_name_ambiguous() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "FOR EACH Customer NO-LOCK:\n    IF CAN-FIND(FIRST Order WHERE |",
    );
    let list = answer.list();
    list.contains(FIELD, &["Order.CustNum", "OrderNum"]);
    list.lacks_kind(FIELD, &["CustNum"]);
}

#[test]
fn an_undefine_shows_the_global_name_under_the_scoped_one() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "&GLOBAL-DEFINE g1 one\n&SCOPED-DEFINE g1 two\n&UNDEFINE g1\nx = {&g1}.\nx = {&|}.\n",
    );
    let list = answer.list();
    let item = list.item(CONSTANT, "g1");
    assert_eq!(item["detail"], "one", "{}", list.summary());
}

#[test]
fn run_offers_no_procedure_of_a_branch_that_is_not_compiled() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "&IF FALSE &THEN\nPROCEDURE ghost:\nEND PROCEDURE.\n&ENDIF\nPROCEDURE real:\nEND PROCEDURE.\nRUN |",
    );
    let list = answer.list();
    list.contains(FUNCTION, &["real"]);
    list.lacks(&["ghost"]);
}

#[test]
fn a_procedure_library_on_the_include_path_makes_the_list_incomplete() {
    let mut ws = Files::config_n(Case::Upper)
        .file(
            "oxabl.toml",
            "[workspace]\nname = \"lib\"\n[workspace.sources]\n\
             include_paths = [\"src\", \"lib/app.pl\", \"nodir\"]\n",
        )
        .file("src/a.p", "MESSAGE 1.")
        .file("lib/app.pl", "not a directory")
        .start();
    let answer = ws.complete("main.p", "RUN |");
    let list = answer.list();
    list.contains(FILE, &["a.p"]);
    assert!(
        list.incomplete,
        "a library is not an empty directory: {}",
        list.summary()
    );

    let mut ws = Files::config_n(Case::Upper)
        .file(
            "oxabl.toml",
            "[workspace]\nname = \"lib\"\n[workspace.sources]\n\
             include_paths = [\"src\", \"nodir\"]\n",
        )
        .file("src/a.p", "MESSAGE 1.")
        .start();
    let answer = ws.complete("main.p", "RUN |");
    assert!(
        !answer.list().incomplete,
        "a missing directory is empty: {}",
        answer.summary()
    );
}

#[test]
fn a_stale_base_offers_no_define_the_text_removed() {
    let mut ws = Files::config_n(Case::Upper).start();
    let cursor = ws.open_marked(
        "main.p",
        &format!("{HOLD}&GLOBAL-DEFINE gone 1\n&GLOBAL-DEFINE kept 2\nx = {{&|}}.\n"),
    );
    ws.complete_at("main.p", cursor, None)
        .list()
        .contains(CONSTANT, &["gone", "kept"]);
    // Remove the first define; the refresh of the base is held.
    ws.change("main.p", 2, (1, 0), (2, 0), "");
    let after = completion::Cursor {
        line: cursor.line - 1,
        ..cursor
    };
    let answer = ws.complete_at("main.p", after, None);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(CONSTANT, &["kept"]);
    list.lacks(&["gone"]);
}

#[test]
fn a_match_at_the_start_of_a_name_comes_before_a_fitting_inner_one() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE cName AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n\
         DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.\n\
         iTotal = c|",
    );
    let list = answer.list();
    list.contains(VARIABLE, &["cName", "iCount"]);
    list.assert_before("cName", "iCount");
}
