//! `textDocument/completion` of records and fields through the real
//! `oxabl lsp`: the tables, buffers and temp-tables after the record
//! keywords, the fields after `table.`, the fields first in a `WHERE`, the
//! database names, the resolve of a table and a field, and the table choice of
//! the server snippets.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use std::collections::BTreeSet;

use completion::{
    Answer, Case, FIELD, Files, List, SHOP_DF, SNIPPET, STRUCT, VARIABLE, documentation, new_text,
};
use serde_json::{Value, json};

const PRELUDE: &str = "DEFINE BUFFER bCust FOR Customer.
DEFINE TEMP-TABLE ttLine NO-UNDO FIELD LineNo AS INTEGER.
";

const HR_DF: &str = "ADD TABLE \"Employee\"
  AREA \"Data\"

ADD FIELD \"EmpNum\" OF \"Employee\" AS integer

ADD FIELD \"LastName\" OF \"Employee\" AS character
  FORMAT \"x(20)\"

ADD TABLE \"Dept\"
  AREA \"Data\"

ADD FIELD \"DeptCode\" OF \"Dept\" AS character

";

/// Config B: config A with a second database.
fn config_b(case: Case) -> Files {
    let style = match case {
        Case::Upper => "[workspace.style]\nkeyword_case = \"Uppercase\"\n",
        _ => "",
    };
    Files::config_n(case)
        .file(
            "oxabl.toml",
            &format!(
                "[workspace]\nname = \"complete\"\n\
                 [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
                 [workspace.schema]\nfiles = [\"db/shop.df\", \"db/hr.df\"]\n{style}"
            ),
        )
        .file("db/shop.df", SHOP_DF)
        .file("db/hr.df", HR_DF)
}

fn ask(files: Files, marked: &str) -> Answer {
    files.start().complete("main.p", marked)
}

fn ask_trigger(files: Files, marked: &str, trigger: &str) -> Answer {
    files.start().complete_trigger("main.p", marked, trigger)
}

/// The set of `(kind, label)` pairs is exactly `wanted`, with no duplicate.
#[track_caller]
fn exactly(list: &List, wanted: &[(u64, &str)]) {
    let got: BTreeSet<(u64, String)> = list
        .items
        .iter()
        .map(|i| {
            (
                i["kind"].as_u64().unwrap(),
                completion::label(i).to_string(),
            )
        })
        .collect();
    let want: BTreeSet<(u64, String)> = wanted.iter().map(|(k, l)| (*k, l.to_string())).collect();
    assert_eq!(got, want, "the items of {}", list.summary());
    assert_eq!(
        list.items.len(),
        want.len(),
        "an item is listed twice in {}",
        list.summary()
    );
}

#[track_caller]
fn detail(list: &List, kind: u64, label: &str) -> String {
    list.item(kind, label)["detail"]
        .as_str()
        .unwrap_or_else(|| panic!("`{label}` has no detail in {}", list.summary()))
        .to_string()
}

fn range(line: u32, from: u32, to: u32) -> Value {
    json!({"start": {"line": line, "character": from}, "end": {"line": line, "character": to}})
}

/// R1.
#[test]
fn records_after_find_first_are_the_buffers_temp_tables_and_tables() {
    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}FIND FIRST |"),
    );
    let list = answer.list();
    assert!(!list.incomplete, "{}", list.summary());
    exactly(
        list,
        &[
            (STRUCT, "bCust"),
            (STRUCT, "ttLine"),
            (STRUCT, "Customer"),
            (STRUCT, "Order"),
            (STRUCT, "Item"),
        ],
    );
    assert_eq!(detail(list, STRUCT, "Customer"), "Database table");
    assert_eq!(detail(list, STRUCT, "ttLine"), "Temp-table");
    assert_eq!(detail(list, STRUCT, "bCust"), "Buffer for Customer");
}

/// R2.
#[test]
fn the_word_filters_the_records_by_part_starts() {
    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}FOR EACH Cu|"),
    );
    exactly(answer.list(), &[(STRUCT, "Customer"), (STRUCT, "bCust")]);
}

/// R3.
#[test]
fn define_buffer_for_offers_tables_and_temp_tables_but_no_buffer() {
    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}DEFINE BUFFER bX FOR |"),
    );
    exactly(
        answer.list(),
        &[
            (STRUCT, "Customer"),
            (STRUCT, "Order"),
            (STRUCT, "Item"),
            (STRUCT, "ttLine"),
        ],
    );

    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}DEFINE BUFFER bX FOR TEMP-TABLE |"),
    );
    exactly(answer.list(), &[(STRUCT, "ttLine")]);
}

/// R4.
#[test]
fn other_record_positions_offer_the_records() {
    for last in [
        "FOR EACH Order NO-LOCK, EACH |",
        "IF CAN-FIND(FIRST |",
        "CREATE |",
        "RELEASE |",
    ] {
        let answer = ask(Files::config_a(Case::Upper), &format!("{PRELUDE}{last}"));
        let list = answer.list();
        list.contains(STRUCT, &["Customer", "ttLine"]);
    }
    let answer = ask(
        Files::config_a(Case::Lower),
        &format!("{PRELUDE}find first |"),
    );
    answer.list().contains(STRUCT, &["Customer", "ttLine"]);
}

/// R5.
#[test]
fn metaschema_tables_are_offered_only_after_an_underscore() {
    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}FIND FIRST |"),
    );
    for item in &answer.list().items {
        assert!(
            !completion::label(item).starts_with('_'),
            "no metaschema table without `_`: {item}"
        );
    }

    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}FIND FIRST _Fi|"),
    );
    let list = answer.list();
    assert!(
        list.items
            .iter()
            .any(|i| i["kind"] == json!(STRUCT)
                && completion::label(i).eq_ignore_ascii_case("_File")),
        "a Struct `_File` in {}",
        list.summary()
    );
}

/// R6.
#[test]
fn the_fields_of_a_table_carry_their_type_and_format() {
    let answer = ask(Files::config_a(Case::Upper), "DISPLAY Customer.|");
    let list = answer.list();
    exactly(
        list,
        &[
            (FIELD, "CustNum"),
            (FIELD, "Name"),
            (FIELD, "Balance"),
            (FIELD, "Phone"),
        ],
    );
    assert_eq!(detail(list, FIELD, "CustNum"), "INTEGER FORMAT \">>>>9\"");
    assert_eq!(detail(list, FIELD, "Name"), "CHARACTER FORMAT \"x(30)\"");
    assert_eq!(
        detail(list, FIELD, "Balance"),
        "DECIMAL FORMAT \"->>,>>9.99\""
    );
    assert_eq!(detail(list, FIELD, "Phone"), "CHARACTER");
}

/// R7.
#[test]
fn the_fields_through_a_buffer_replace_only_the_word_after_the_dot() {
    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{PRELUDE}DISPLAY bCust.Na|"),
    );
    let list = answer.list();
    exactly(list, &[(FIELD, "Name")]);
    let edit = &list.item(FIELD, "Name")["textEdit"];
    assert_eq!(edit["newText"], json!("Name"), "{edit}");
    assert_eq!(edit["replace"], range(2, 14, 16), "{edit}");
    assert_eq!(edit["insert"], range(2, 14, 16), "{edit}");
}

/// R8.
#[test]
fn temp_table_fields_include_the_fields_of_the_like_table() {
    let text = "DEFINE TEMP-TABLE ttLine NO-UNDO
    FIELD LineNo AS INTEGER
    FIELD Note AS CHARACTER.
DEFINE TEMP-TABLE ttCust NO-UNDO LIKE Customer
    FIELD Extra AS LOGICAL.
";
    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{text}DISPLAY ttLine.|"),
    );
    let list = answer.list();
    exactly(list, &[(FIELD, "LineNo"), (FIELD, "Note")]);
    assert_eq!(detail(list, FIELD, "LineNo"), "INTEGER");
    assert_eq!(detail(list, FIELD, "Note"), "CHARACTER");

    let answer = ask(
        Files::config_a(Case::Upper),
        &format!("{text}DISPLAY ttCust.|"),
    );
    exactly(
        answer.list(),
        &[
            (FIELD, "CustNum"),
            (FIELD, "Name"),
            (FIELD, "Balance"),
            (FIELD, "Phone"),
            (FIELD, "Extra"),
        ],
    );
}

/// R9.
#[test]
fn a_like_that_oxabl_cannot_follow_gives_an_incomplete_list() {
    let answer = ask(
        Files::config_a(Case::Upper),
        "DEFINE TEMP-TABLE ttX NO-UNDO LIKE NoSuchTable
    FIELD Own AS INTEGER.
DISPLAY ttX.|",
    );
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    list.contains(FIELD, &["Own"]);
}

/// R10.
#[test]
fn where_oxabl_does_not_know_the_record_there_is_no_list() {
    ask(Files::config_a(Case::Upper), "DISPLAY nothing.|").assert_null();
    ask_trigger(Files::config_a(Case::Upper), "RELEASE Customer.|", ".").assert_null();
    ask_trigger(
        Files::config_a(Case::Upper),
        "FIND FIRST Customer NO-LOCK.|",
        ".",
    )
    .assert_null();
}

/// R11.
#[test]
fn the_fields_of_the_named_records_come_first_in_a_where() {
    let answer = ask(
        Files::config_a(Case::Upper),
        "DEFINE VARIABLE iMin AS INTEGER NO-UNDO.
FOR EACH Customer NO-LOCK WHERE |",
    );
    let list = answer.list();
    list.contains(FIELD, &["CustNum", "Name", "Balance", "Phone"]);
    list.contains(VARIABLE, &["iMin"]);
    for field in ["CustNum", "Name", "Balance", "Phone"] {
        list.assert_before(field, "iMin");
    }
    assert_eq!(new_text(list.item(FIELD, "CustNum")), "CustNum");
}

/// R12.
#[test]
fn the_type_that_fits_comes_first_in_a_where() {
    let answer = ask(
        Files::config_a(Case::Upper),
        "DEFINE VARIABLE iMin AS INTEGER NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
FOR EACH Customer NO-LOCK WHERE Customer.CustNum > |",
    );
    answer.list().assert_before("iMin", "cName");
}

/// R13.
#[test]
fn with_no_schema_only_the_temp_tables_and_buffers_are_offered_incompletely() {
    let answer = ask(
        Files::config_n(Case::Upper),
        "DEFINE TEMP-TABLE ttLine NO-UNDO FIELD LineNo AS INTEGER.
FIND FIRST |",
    );
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    exactly(list, &[(STRUCT, "ttLine")]);
}

/// R14.
#[test]
fn two_databases_name_the_database_and_qualify_the_tables() {
    let answer = ask(config_b(Case::Upper), "FIND FIRST |");
    let list = answer.list();
    assert_eq!(detail(list, STRUCT, "Customer"), "Database table, shop");
    assert_eq!(detail(list, STRUCT, "Employee"), "Database table, hr");

    for qualifier in ["hr", "HR"] {
        let answer = ask(config_b(Case::Upper), &format!("FIND FIRST {qualifier}.|"));
        exactly(answer.list(), &[(STRUCT, "Employee"), (STRUCT, "Dept")]);
    }

    let answer = ask(config_b(Case::Upper), "DISPLAY shop.Customer.|");
    exactly(
        answer.list(),
        &[
            (FIELD, "CustNum"),
            (FIELD, "Name"),
            (FIELD, "Balance"),
            (FIELD, "Phone"),
        ],
    );

    let answer = ask(Files::config_a(Case::Upper), "FIND FIRST |");
    assert_eq!(detail(answer.list(), STRUCT, "Customer"), "Database table");
}

/// R15.
#[test]
fn a_table_and_a_field_resolve_to_their_schema_text() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete("main.p", &format!("{PRELUDE}FIND FIRST |"));
    let item = answer.list().item(STRUCT, "Customer").clone();
    let text = documentation(&ws.resolve(&item));
    assert!(text.contains("TABLE Customer"), "{text}");
    assert!(text.contains("Fields: 4"), "{text}");

    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete("main.p", "DISPLAY Customer.|");
    let item = answer.list().item(FIELD, "Name").clone();
    let text = documentation(&ws.resolve(&item));
    assert!(text.contains("FIELD Customer.Name AS CHARACTER"), "{text}");
}

/// R16.
#[test]
fn the_server_snippets_offer_the_tables_up_to_thirty() {
    let mut ws = Files::config_a(Case::Upper).start();
    let answer = ws.complete("main.p", "|");
    let text = new_text(answer.list().item(SNIPPET, "find")).to_string();
    assert!(text.contains("${1|Customer,Item,Order|}"), "{text}");

    let mut big = String::new();
    for n in 1..=31 {
        big.push_str(&format!(
            "ADD TABLE \"T{n:02}\"\n  AREA \"Data\"\n\nADD FIELD \"F\" OF \"T{n:02}\" AS integer\n\n"
        ));
    }
    let mut ws = Files::config_a(Case::Upper)
        .file("db/shop.df", &big)
        .start();
    let answer = ws.complete("main.p", "|");
    let text = new_text(answer.list().item(SNIPPET, "find")).to_string();
    assert!(text.contains("${1:Customer}"), "{text}");
    assert!(!text.contains("${1|"), "{text}");
}
