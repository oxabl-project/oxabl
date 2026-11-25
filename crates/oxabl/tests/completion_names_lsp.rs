//! `textDocument/completion` of the names in scope through the real
//! `oxabl lsp`: variables, parameters, functions, built-in functions, the
//! members of the class of the file, the order of the items, the cases that make
//! a list incomplete, and a stale base.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Case, FIELD, FUNCTION, Files, METHOD, PROPERTY, VARIABLE, documentation};
use serde_json::json;
use session::file_uri;

const N1: &str = "DEFINE VARIABLE gTotal AS DECIMAL NO-UNDO.
PROCEDURE calc:
    DEFINE INPUT PARAMETER piQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLocal AS INTEGER NO-UNDO.
    |
    DEFINE VARIABLE iLater AS INTEGER NO-UNDO.
END PROCEDURE.
PROCEDURE other:
    DEFINE VARIABLE iOther AS INTEGER NO-UNDO.
END PROCEDURE.
";

fn detail(list: &completion::List, kind: u64, label: &str) -> String {
    list.item(kind, label)["detail"]
        .as_str()
        .unwrap_or_else(|| panic!("`{label}` has no detail in {}", list.summary()))
        .to_string()
}

/// N1.
#[test]
fn locals_and_parameters_follow_the_definition_before_use_rule() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", N1);
    let list = answer.list();
    assert_eq!(detail(list, VARIABLE, "gTotal"), "DECIMAL");
    assert_eq!(detail(list, VARIABLE, "piQty"), "INPUT INTEGER");
    assert_eq!(detail(list, VARIABLE, "iLocal"), "INTEGER");
    list.lacks(&["iLater", "iOther"]);
    list.assert_before("iLocal", "gTotal");
    list.assert_before("piQty", "gTotal");
}

/// N2.
#[test]
fn functions_are_offered_when_declared_before_or_forwarded() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "FUNCTION fnEarly RETURNS INTEGER (INPUT p AS INTEGER):
    RETURN p.
END FUNCTION.
FUNCTION fnFwd RETURNS INTEGER (INPUT p AS INTEGER) FORWARD.
DEFINE VARIABLE x AS INTEGER NO-UNDO.
x = fn|
FUNCTION fnLate RETURNS INTEGER (INPUT p AS INTEGER):
    RETURN p.
END FUNCTION.
FUNCTION fnFwd RETURNS INTEGER (INPUT p AS INTEGER):
    RETURN p.
END FUNCTION.
",
    );
    let list = answer.list();
    list.contains(FUNCTION, &["fnEarly", "fnFwd"]);
    list.lacks(&["fnLate"]);
}

/// N3.
#[test]
fn built_in_functions_use_the_keyword_case() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = ENT|",
    );
    answer.list().contains(FUNCTION, &["ENTRY"]);

    let mut ws = Files::config_n(Case::Lower).start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = ent|",
    );
    answer.list().contains(FUNCTION, &["entry"]);
}

const N4: &str = "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMax AS INTEGER NO-UNDO.
DEFINE VARIABLE cCode AS CHARACTER NO-UNDO.
";

/// N4.
#[test]
fn the_type_that_fits_comes_first() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{N4}iCount = |"));
    let list = answer.list();
    list.assert_before("iMax", "cName");
    list.assert_before("iMax", "cCode");

    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{N4}cName = |"));
    let list = answer.list();
    list.assert_before("cCode", "iMax");
    list.assert_before("cCode", "iCount");
}

/// N5.
#[test]
fn a_statement_start_offers_the_variables() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\niC|");
    answer.list().contains(VARIABLE, &["iCount"]);
}

/// N6.
#[test]
fn the_members_of_the_own_class_are_offered_anywhere_in_the_class() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "Box.cls",
        "CLASS Box:
    METHOD PUBLIC VOID Fill():
        |
    END METHOD.
    DEFINE PRIVATE VARIABLE iWidth AS INTEGER NO-UNDO.
    METHOD PRIVATE INTEGER Measure():
        RETURN 1.
    END METHOD.
END CLASS.
",
    );
    let list = answer.list();
    list.contains(PROPERTY, &["iWidth"]);
    list.contains(METHOD, &["Measure", "Fill"]);
}

const SQUARE: &str = "USING app.shapes.*.
CLASS Square INHERITS Shape:
    DEFINE PRIVATE VARIABLE iMine AS INTEGER NO-UNDO.
    METHOD PUBLIC VOID Test():
        |
    END METHOD.
END CLASS.
";

/// N7.
#[test]
fn a_class_with_a_workspace_parent_gives_an_incomplete_list() {
    let mut ws = Files::config_a(Case::Upper).shapes().start();
    let answer = ws.complete("Square.cls", SQUARE);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(PROPERTY, &["iMine"]);
    list.contains(METHOD, &["Test"]);
}

/// N8.
#[test]
fn fields_that_the_code_used_are_not_names() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "FIND FIRST Customer NO-LOCK NO-ERROR.\nDISPLAY Customer.Name.\n|",
    );
    answer.list().has_no_kind(FIELD);
}

/// N9.
#[test]
fn a_parse_error_before_the_cursor_gives_an_incomplete_list() {
    let mut ws = Files::config_n(Case::Upper).start();
    let cursor = ws.open_marked(
        "main.p",
        "DEFINE VARIABLE iA AS INTEGER NO-UNDO.
iA = (1 + .
DEFINE VARIABLE iB AS INTEGER NO-UNDO.
i|",
    );
    let uri = json!(file_uri(&ws.path("main.p")));
    let published = ws.session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics")
            && m["params"]["uri"] == uri
            && m["params"]["diagnostics"]
                .as_array()
                .is_some_and(|all| all.iter().any(|d| d["range"]["start"]["line"] == json!(1)))
    });
    assert!(published["params"]["diagnostics"].is_array());

    let answer = ws.complete_at("main.p", cursor, None);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(VARIABLE, &["iA"]);
}

/// N10.
#[test]
fn a_branch_that_the_preprocessor_drops_gives_no_names() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete(
        "main.p",
        "&IF FALSE &THEN
DEFINE VARIABLE iHidden AS INTEGER NO-UNDO.
&ENDIF
DEFINE VARIABLE iShown AS INTEGER NO-UNDO.
i|
",
    );
    let list = answer.list();
    list.contains(VARIABLE, &["iShown"]);
    list.lacks(&["iHidden"]);

    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", "&IF FALSE &THEN\ni|\n&ENDIF\n");
    answer.assert_null();
}

/// N11.
#[test]
fn a_variable_resolves_to_its_declaration() {
    let mut ws = Files::config_n(Case::Upper).start();
    let answer = ws.complete("main.p", N1);
    let item = answer.list().item(VARIABLE, "iLocal").clone();
    let resolved = ws.resolve(&item);
    let text = documentation(&resolved);
    assert!(text.contains("DEFINE VARIABLE iLocal AS INTEGER"), "{text}");
}

/// N12.
#[test]
fn a_stale_base_drops_what_the_edit_removed() {
    let mut ws = Files::config_n(Case::Upper).start();
    let cursor = ws.open_marked(
        "main.p",
        "/* OXABL-TEST-HOLD:completion.refresh */
DEFINE VARIABLE iKeep AS INTEGER NO-UNDO.
DEFINE VARIABLE iGone AS INTEGER NO-UNDO.
i|",
    );
    assert_eq!(cursor.line, 3);
    let first = ws.complete_at("main.p", cursor, None);
    first.list().contains(VARIABLE, &["iKeep", "iGone"]);

    // Delete the line of `iGone`.
    ws.change("main.p", 2, (2, 0), (3, 0), "");
    let cursor = completion::Cursor {
        line: 2,
        character: 1,
    };
    let answer = ws.complete_at("main.p", cursor, None);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(VARIABLE, &["iKeep"]);
    list.lacks(&["iGone"]);
}
