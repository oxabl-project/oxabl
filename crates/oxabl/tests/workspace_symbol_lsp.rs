//! `workspace/symbol` through the real `oxabl lsp` and its daemon: symbols of
//! files that are not open, where they are declared, and what an open buffer
//! adds before it is saved.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

/// The symbols of a successful answer. Fails on an error or a `null`.
#[track_caller]
fn symbols(session: &mut Session, query: &str) -> Vec<Value> {
    let answer = session.request("workspace/symbol", json!({"query": query}));
    assert!(
        answer.get("error").is_none(),
        "workspace/symbol gave an error: {}",
        answer["error"]
    );
    answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("workspace/symbol gave no list: {}", answer["result"]))
        .clone()
}

#[track_caller]
fn named<'a>(symbols: &'a [Value], name: &str) -> &'a Value {
    symbols
        .iter()
        .find(|s| s["name"] == json!(name))
        .unwrap_or_else(|| panic!("no symbol `{name}` in {symbols:#?}"))
}

/// The `(line, character)` of `needle` in `text`, in UTF-16 code units.
fn utf16_position(text: &str, needle: &str) -> (u64, u64) {
    let at = text.find(needle).unwrap();
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

#[track_caller]
fn assert_declared_at(symbol: &Value, path: &Path, text: &str, name: &str) {
    assert_eq!(symbol["location"]["uri"], json!(file_uri(path)), "{symbol}");
    let (line, character) = utf16_position(text, name);
    let range = &symbol["location"]["range"];
    assert_eq!(
        range["start"],
        json!({"line": line, "character": character}),
        "{symbol}"
    );
    assert_eq!(
        range["end"],
        json!({"line": line, "character": character + name.len() as u64}),
        "{symbol}"
    );
}

const ORDERS: &str = "/* Bestellungen für Kunden */\n\
    DEFINE NEW SHARED VARIABLE gcShop AS CHARACTER NO-UNDO.\n\
    \n\
    /* ü, é */ PROCEDURE CalcTotals:\n\
    END PROCEDURE.\n\
    \n\
    FUNCTION FormatTotal RETURNS CHARACTER (INPUT pd AS DECIMAL):\n\
    \x20   RETURN STRING(pd).\n\
    END FUNCTION.\n";

const GREETER: &str = "CLASS app.Greeter:\n\
    \x20   METHOD PUBLIC VOID Hello():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

fn workspace() -> tempfile::TempDir {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"symbols\"\n",
    )
    .unwrap();
    let orders = root.path().join("my orders");
    std::fs::create_dir_all(&orders).unwrap();
    std::fs::write(orders.join("orders.p"), ORDERS).unwrap();
    let app = root.path().join("app");
    std::fs::create_dir_all(&app).unwrap();
    std::fs::write(app.join("Greeter.cls"), GREETER).unwrap();
    root
}

#[test]
fn symbols_of_files_that_are_not_open_point_at_their_declaration() {
    let root = workspace();
    let mut session = Session::start(root.path());
    let orders = root.path().join("my orders").join("orders.p");
    let greeter = root.path().join("app").join("Greeter.cls");

    let found = symbols(&mut session, "calctot");
    let procedure = named(&found, "CalcTotals");
    assert_eq!(
        procedure["kind"],
        json!(12),
        "a procedure is a function symbol"
    );
    assert_eq!(procedure["containerName"], json!("orders.p"));
    // The comment before it on its line holds characters that are two UTF-8
    // bytes and one UTF-16 unit, and the client negotiated no encoding, so
    // UTF-16 applies.
    assert_declared_at(procedure, &orders, ORDERS, "CalcTotals");

    let found = symbols(&mut session, "FORMATTOTAL");
    assert_declared_at(named(&found, "FormatTotal"), &orders, ORDERS, "FormatTotal");

    let found = symbols(&mut session, "gcshop");
    let shared = named(&found, "gcShop");
    assert_eq!(
        shared["kind"],
        json!(13),
        "a shared variable is a variable symbol"
    );
    assert_declared_at(shared, &orders, ORDERS, "gcShop");

    let found = symbols(&mut session, "greeter");
    let class = found
        .iter()
        .find(|s| s["kind"] == json!(5))
        .unwrap_or_else(|| panic!("no class in {found:#?}"));
    assert_eq!(class["location"]["uri"], json!(file_uri(&greeter)));
    let file = found
        .iter()
        .find(|s| s["kind"] == json!(1))
        .unwrap_or_else(|| panic!("no file symbol in {found:#?}"));
    assert_eq!(file["location"]["uri"], json!(file_uri(&greeter)));

    assert!(symbols(&mut session, "NoSuchThing").is_empty());
}

#[test]
fn an_open_buffer_adds_its_unsaved_symbols() {
    let root = workspace();
    let mut session = Session::start(root.path());
    let orders = root.path().join("my orders").join("orders.p");
    // Search once, so a workspace pass is already in place before the edit.
    assert!(!symbols(&mut session, "calctotals").is_empty());

    let edited = format!("{ORDERS}\nPROCEDURE ShipOrders:\nEND PROCEDURE.\n");
    session.open(&orders, &edited);
    let found = symbols(&mut session, "shiporders");
    assert_declared_at(named(&found, "ShipOrders"), &orders, &edited, "ShipOrders");
}

#[test]
fn the_answer_is_capped() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"many\"\n",
    )
    .unwrap();
    for i in 0..300 {
        std::fs::write(
            root.path().join(format!("p{i}.p")),
            format!("PROCEDURE proc{i}:\nEND PROCEDURE.\n"),
        )
        .unwrap();
    }
    let mut session = Session::start(root.path());
    let found = symbols(&mut session, "proc");
    assert_eq!(found.len(), 256, "the documented cap");
    assert!(
        found
            .iter()
            .all(|s| s["name"].as_str().unwrap().starts_with("proc"))
    );
}

/// The workspace pass asks the producer map when a file has a `SHARED`
/// consumer. A producer with a `TRIGGERS` phrase holds a `DEFINE` of its own,
/// and the pass must end and answer.
#[test]
fn the_pass_ends_over_a_producer_that_holds_a_define() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"triggers\"\n",
    )
    .unwrap();
    std::fs::write(
        root.path().join("producer.p"),
        "DEFINE NEW SHARED VARIABLE gcTrig AS CHARACTER NO-UNDO\n\
         \x20   VIEW-AS FILL-IN\n\
         \x20   TRIGGERS:\n\
         \x20       ON LEAVE DO:\n\
         \x20           DEFINE VARIABLE inner AS INTEGER NO-UNDO.\n\
         \x20       END.\n\
         \x20   END TRIGGERS.\n",
    )
    .unwrap();
    std::fs::write(
        root.path().join("consumer.p"),
        "DEFINE SHARED VARIABLE gcTrig AS CHARACTER NO-UNDO.\nMESSAGE gcTrig.\n",
    )
    .unwrap();
    let mut session = Session::start(root.path());
    let found = symbols(&mut session, "gctrig");
    assert!(
        found.iter().any(|s| s["name"] == json!("gcTrig")),
        "{found:#?}"
    );
}
