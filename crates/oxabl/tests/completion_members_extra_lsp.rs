//! `textDocument/completion` of the members after `receiver:`: the cases that
//! `completion_members_lsp.rs` does not state. A receiver of the class of the
//! open file, the inherited names of a class body, the detail of a method, the
//! enum members, the receivers that give `null`, a chain that stops, and the
//! class facts a base keeps until a file changes.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Case, Files, METHOD, PROPERTY, Ws, label};
use serde_json::json;
use session::file_uri;

const ENUM_MEMBER: u64 = 20;
const INTERFACE: &str = "INTERFACE app.IPart:
    METHOD PUBLIC VOID Attach().
END INTERFACE.
";

const WIDGET: &str = "CLASS app.Widget:
    DEFINE PUBLIC PROPERTY Tag AS CHARACTER NO-UNDO GET. SET.
    DEFINE PRIVATE VARIABLE cSecret AS CHARACTER NO-UNDO.
    DEFINE PRIVATE STATIC VARIABLE iSecretCount AS INTEGER NO-UNDO.
    DEFINE PUBLIC STATIC VARIABLE iBuilt AS INTEGER NO-UNDO.
    DEFINE PUBLIC PROPERTY Slot AS app.IPart NO-UNDO GET. SET.
    METHOD PUBLIC INTEGER Add(INPUT a AS INTEGER, OUTPUT b AS CHARACTER):
        RETURN 0.
    END METHOD.
    METHOD PUBLIC app.Widget Pick(INPUT i AS INTEGER):
        RETURN THIS-OBJECT.
    END METHOD.
    METHOD PUBLIC app.IPart Pick(INPUT c AS CHARACTER):
        RETURN ?.
    END METHOD.
    METHOD PUBLIC VOID Describe():
    END METHOD.
    METHOD PROTECTED VOID Refill():
    END METHOD.
    METHOD PRIVATE VOID Wipe():
    END METHOD.
    METHOD PUBLIC STATIC app.Widget Build():
        RETURN ?.
    END METHOD.
END CLASS.
";

const COLOR: &str = "ENUM app.Color:
    DEFINE ENUM Red Green.
END ENUM.
";

fn files() -> Files {
    Files::config_a(Case::Upper)
        .file("lib/app/Widget.cls", WIDGET)
        .file("lib/app/IPart.cls", INTERFACE)
        .file("lib/app/Color.cls", COLOR)
}

/// A class `app.Gadget` that inherits `app.Widget`, with the cursor in a
/// method (or a static method) that holds `body`.
fn gadget(header: &str, body: &str) -> String {
    format!(
        "CLASS app.Gadget INHERITS app.Widget:
    DEFINE PRIVATE VARIABLE cOwn AS CHARACTER NO-UNDO.
    {header} VOID Run():
        {body}
    END METHOD.
END CLASS.
"
    )
}

/// X1.
#[test]
fn a_value_of_the_class_of_the_file_reaches_its_private_members() {
    let mut ws = files().start();
    let source = "CLASS app.Self:
    DEFINE PRIVATE VARIABLE cHidden AS CHARACTER NO-UNDO.
    DEFINE PUBLIC PROPERTY Open AS CHARACTER NO-UNDO GET. SET.
    DEFINE PRIVATE STATIC VARIABLE iHidden AS INTEGER NO-UNDO.
    METHOD PUBLIC VOID Run():
        DEFINE VARIABLE oOther AS app.Self NO-UNDO.
        oOther:|
    END METHOD.
END CLASS.
";
    let answer = ws.complete("lib/app/Self.cls", source);
    let list = answer.list();
    list.contains(PROPERTY, &["cHidden", "Open"]);
    list.lacks(&["iHidden"]);
    // The type name of the class of the file reaches its static members.
    let source = source.replace("oOther:|", "app.Self:|");
    let answer = ws.complete("lib/app/Self.cls", &source);
    let list = answer.list();
    list.contains(PROPERTY, &["iHidden"]);
    list.lacks(&["cHidden", "Open"]);
}

/// X2.
#[test]
fn another_class_gives_only_its_public_members_also_inside_a_subclass() {
    let mut ws = files().start();
    let source = gadget(
        "METHOD PUBLIC",
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\n        oW:|",
    );
    let answer = ws.complete("lib/app/Gadget.cls", &source);
    let list = answer.list();
    list.contains(METHOD, &["Describe", "Add"]);
    list.lacks(&["Refill", "Wipe", "cSecret", "cOwn"]);
}

/// X3.
#[test]
fn a_class_body_offers_the_names_its_parent_passes_on() {
    let mut ws = files().start();
    let source = gadget("METHOD PUBLIC", "|");
    let answer = ws.complete("lib/app/Gadget.cls", &source);
    let list = answer.list();
    list.contains(METHOD, &["Describe", "Refill", "Add", "Pick"]);
    list.contains(PROPERTY, &["Tag", "cOwn"]);
    // A private member of the parent is not passed on, and a static one is
    // reached by the type name and by its own name.
    list.lacks(&["Wipe", "cSecret", "iSecretCount"]);
    list.contains(METHOD, &["Build"]);
    assert!(list.incomplete, "{}", list.summary());
}

/// X4.
#[test]
fn a_static_routine_offers_only_the_static_names_of_the_parent() {
    let mut ws = files().start();
    let source = gadget("METHOD PUBLIC STATIC", "|");
    let answer = ws.complete("lib/app/Gadget.cls", &source);
    let list = answer.list();
    list.contains(METHOD, &["Build"]);
    list.contains(PROPERTY, &["iBuilt"]);
    list.lacks(&["Describe", "Add", "Tag", "Refill"]);
}

/// X5.
#[test]
fn a_method_detail_gives_the_return_type_and_the_modes_and_types_of_the_parameters() {
    let mut ws = files().start();
    let answer = ws.complete("main.p", "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:|");
    let list = answer.list();
    let detail = |kind, name: &str| {
        list.item(kind, name)["detail"]
            .as_str()
            .unwrap_or_else(|| panic!("`{name}` has no detail"))
            .to_string()
    };
    assert_eq!(
        detail(METHOD, "Add"),
        "METHOD INTEGER (INPUT INTEGER, OUTPUT CHARACTER)"
    );
    assert_eq!(
        detail(METHOD, "Pick"),
        "METHOD app.Widget (INPUT INTEGER) (+1 overload)"
    );
    assert_eq!(detail(PROPERTY, "Tag"), "CHARACTER");
    assert_eq!(detail(PROPERTY, "Slot"), "app.IPart");
    let answer = ws.complete("main.p", "app.Widget:|");
    assert_eq!(
        detail_of(answer.list(), METHOD, "Build"),
        "METHOD STATIC app.Widget ()"
    );
}

fn detail_of(list: &completion::List, kind: u64, name: &str) -> String {
    list.item(kind, name)["detail"]
        .as_str()
        .expect("a detail")
        .to_string()
}

/// X6.
#[test]
fn enum_members_are_enum_members_of_the_enum() {
    let mut ws = files().start();
    let answer = ws.complete("main.p", "app.Color:|");
    let list = answer.list();
    list.contains(ENUM_MEMBER, &["Red", "Green"]);
    assert_eq!(detail_of(list, ENUM_MEMBER, "Red"), "app.Color");
}

/// X7.
#[test]
fn a_receiver_oxabl_cannot_type_gives_null() {
    let cases = [
        // A member that is not a class, and a method with no single class.
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:Tag:|",
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:Pick(1):|",
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:Add(1, \"\"):|",
        // A name declared after the cursor, a class that does not exist, and
        // a built-in class that oxabl holds no members of.
        "oW:|\nDEFINE VARIABLE oW AS app.Widget NO-UNDO.",
        "DEFINE VARIABLE oN AS app.NoSuchClass NO-UNDO.\noN:|",
        "DEFINE VARIABLE oAny AS Progress.Lang.Object NO-UNDO.\noAny:|",
        // `THIS-OBJECT` and `SUPER` outside a class body.
        "THIS-OBJECT:|",
        "SUPER:|",
        // A `CAST` to a type that is no class, and a parenthesized value.
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\nCAST(oW, CHARACTER):|",
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\n(oW):|",
    ];
    for source in cases {
        for trigger in [Some(":"), None] {
            let mut ws = files().start();
            let cursor = ws.open_marked("main.p", source);
            let answer = ws.complete_at("main.p", cursor, trigger);
            assert!(
                matches!(answer, completion::Answer::Null),
                "{source:?}: {}",
                answer.summary()
            );
        }
    }
}

/// X8.
#[test]
fn an_interface_value_gives_the_methods_of_the_interface() {
    let mut ws = files().start();
    let answer = ws.complete("main.p", "DEFINE VARIABLE oP AS app.IPart NO-UNDO.\noP:|");
    let list = answer.list();
    list.contains(METHOD, &["Attach"]);
    list.lacks(&["Describe", "Tag"]);
}

/// X9.
#[test]
fn a_chain_through_a_property_of_an_interface_type_reaches_the_interface() {
    let mut ws = files().start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:Slot:|",
    );
    answer.list().contains(METHOD, &["Attach"]);
}

/// X10.
#[test]
fn the_members_come_in_one_list_with_the_matching_ones_first() {
    let mut ws = files().start();
    let answer = ws.complete(
        "main.p",
        "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:De|",
    );
    let list = answer.list();
    list.contains(METHOD, &["Describe", "Add"]);
    list.assert_before("Describe", "Add");
    let names: Vec<&str> = list.items.iter().map(label).collect();
    assert_eq!(names.iter().filter(|name| **name == "Describe").count(), 1);
}

fn open_and_complete(ws: &mut Ws) -> completion::Answer {
    ws.complete("main.p", "DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:|")
}

/// X11.
#[test]
fn a_class_file_that_changed_shows_after_its_watched_change_and_not_before() {
    let mut ws = files().start();
    let first = open_and_complete(&mut ws);
    first.list().lacks(&["Polish"]);
    let changed = WIDGET.replace(
        "    METHOD PUBLIC VOID Describe():",
        "    METHOD PUBLIC VOID Polish():\n    END METHOD.\n    METHOD PUBLIC VOID Describe():",
    );
    std::fs::write(ws.path("lib/app/Widget.cls"), changed).unwrap();
    // The base keeps what it read of the class until a file event says it
    // changed: a request reads no class file again.
    let cursor = completion::split_cursor("DEFINE VARIABLE oW AS app.Widget NO-UNDO.\noW:|").1;
    ws.complete_at("main.p", cursor, None)
        .list()
        .lacks(&["Polish"]);
    ws.session.send(json!({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&ws.path("lib/app/Widget.cls")), "type": 2}]}
    }));
    ws.complete_at("main.p", cursor, None)
        .list()
        .contains(METHOD, &["Polish"]);
}
