//! `textDocument/completion` of the members after `receiver:` through the real
//! `oxabl lsp`: the receiver forms, the access and static rules, the cases
//! that give `null`, overloads, resolve and the keyword case.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use completion::{Answer, Case, Files, List, METHOD, PROPERTY, documentation, label};
use serde_json::Value;

const EVENT: u64 = 23;

const UTIL: &str = "CLASS app.Util:
    DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Child AS app.Part NO-UNDO GET. SET.
    DEFINE PRIVATE VARIABLE cHidden AS CHARACTER NO-UNDO.
    DEFINE PROTECTED VARIABLE iGuarded AS INTEGER NO-UNDO.
    DEFINE PUBLIC STATIC VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE PUBLIC EVENT Changed SIGNATURE VOID (INPUT n AS INTEGER).
    METHOD PUBLIC VOID Greet():
    END METHOD.
    METHOD PUBLIC VOID Greet(INPUT pcName AS CHARACTER):
    END METHOD.
    METHOD PUBLIC VOID Greet(INPUT piTimes AS INTEGER):
    END METHOD.
    METHOD PUBLIC app.Part Make():
        RETURN ?.
    END METHOD.
    METHOD PRIVATE VOID Secret():
    END METHOD.
    METHOD PROTECTED VOID Guard():
    END METHOD.
    METHOD PUBLIC STATIC app.Util Shared():
        RETURN ?.
    END METHOD.
END CLASS.
";

const PART: &str = "CLASS app.Part:
    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC INTEGER Measure():
        RETURN 1.
    END METHOD.
END CLASS.
";

const COLOR: &str = "ENUM app.Color:
    DEFINE ENUM Red Green Blue.
END ENUM.
";

const SPECIAL_HEAD: &str = "CLASS app.Special INHERITS app.Util:
    DEFINE PRIVATE VARIABLE cOwn AS CHARACTER NO-UNDO.
    DEFINE PROTECTED VARIABLE iOwnGuarded AS INTEGER NO-UNDO.
    METHOD PUBLIC VOID Extra():
    END METHOD.
";

fn files(case: Case) -> Files {
    Files::config_a(case)
        .file("lib/app/Util.cls", UTIL)
        .file("lib/app/Part.cls", PART)
        .file("lib/app/Color.cls", COLOR)
}

/// A `.p` that declares `oUtil`, with `tail` after it.
fn main_p(tail: &str) -> String {
    format!("DEFINE VARIABLE oUtil AS app.Util NO-UNDO.\n{tail}")
}

/// Run `marked` with the `:` trigger and again invoked: both must answer
/// as `check` says.
#[track_caller]
fn both(case: Case, file: &str, marked: &str, check: impl Fn(&Answer)) {
    check(&files(case).start().complete_trigger(file, marked, ":"));
    check(&files(case).start().complete(file, marked));
}

fn has(list: &List, label_: &str) -> bool {
    list.items.iter().any(|i| label(i) == label_)
}

#[track_caller]
fn must_have(list: &List, labels: &[&str]) {
    for l in labels {
        assert!(has(list, l), "`{l}` is missing in {}", list.summary());
    }
}

#[track_caller]
fn instance_members(list: &List) {
    list.contains(PROPERTY, &["Title", "Child"]);
    list.contains(METHOD, &["Greet", "Make"]);
    list.contains(EVENT, &["Changed"]);
}

fn start_of_replace(item: &Value) -> u64 {
    let edit = &item["textEdit"];
    let range = if edit.get("replace").is_some() {
        &edit["replace"]
    } else {
        &edit["range"]
    };
    range["start"]["character"].as_u64().expect("a range start")
}

/// M1.
#[test]
fn a_variable_of_a_class_gives_its_public_instance_members() {
    both(Case::Upper, "main.p", &main_p("oUtil:|"), |answer| {
        let list = answer.list();
        instance_members(list);
        list.lacks(&["cHidden", "Secret", "iGuarded", "Guard", "iCount", "Shared"]);
        assert!(list.incomplete, "{}", list.summary());
    });
}

/// M2.
#[test]
fn a_partly_typed_member_still_gives_the_members_and_replaces_the_word() {
    let mut ws = files(Case::Upper).start();
    let answer = ws.complete("main.p", &main_p("oUtil:Gr|"));
    let list = answer.list();
    instance_members(list);
    // The line is `oUtil:Gr`: the word `Gr` starts at character 6.
    assert_eq!(start_of_replace(list.item(METHOD, "Greet")), 6);
}

/// M3.
#[test]
fn this_object_gives_the_private_and_protected_members_and_the_inherited_ones() {
    let source = format!(
        "{SPECIAL_HEAD}    METHOD PUBLIC VOID Run():
        THIS-OBJECT:|
    END METHOD.
END CLASS.
"
    );
    both(Case::Upper, "lib/app/Special.cls", &source, |answer| {
        let list = answer.list();
        list.contains(
            PROPERTY,
            &["cOwn", "iOwnGuarded", "Title", "Child", "iGuarded"],
        );
        list.contains(METHOD, &["Extra", "Run", "Greet", "Guard", "Make"]);
        list.contains(EVENT, &["Changed"]);
        // A private member of the parent is not reached, and no static one.
        list.lacks(&["cHidden", "Secret", "iCount", "Shared"]);
        assert!(list.incomplete, "{}", list.summary());
    });
}

/// M4.
#[test]
fn super_gives_only_what_the_parent_gives() {
    let source = format!(
        "{SPECIAL_HEAD}    METHOD PUBLIC VOID Run():
        SUPER:|
    END METHOD.
END CLASS.
"
    );
    both(Case::Upper, "lib/app/Special.cls", &source, |answer| {
        let list = answer.list();
        list.contains(PROPERTY, &["Title", "iGuarded"]);
        list.contains(METHOD, &["Greet", "Guard"]);
        list.lacks(&["cOwn", "iOwnGuarded", "Extra", "Run", "cHidden", "Secret"]);
    });
}

/// M5.
#[test]
fn a_type_name_gives_only_static_members() {
    both(Case::Upper, "main.p", "app.Util:|", |answer| {
        let list = answer.list();
        list.contains(METHOD, &["Shared"]);
        list.contains(PROPERTY, &["iCount"]);
        list.lacks(&[
            "Greet", "Title", "Child", "Changed", "Make", "cHidden", "Guard",
        ]);
    });
    // A simple name that a `USING` gives.
    both(Case::Upper, "main.p", "USING app.*.\nUtil:|", |answer| {
        let list = answer.list();
        list.contains(METHOD, &["Shared"]);
        list.lacks(&["Greet", "Title"]);
    });
}

/// M6.
#[test]
fn new_and_cast_give_the_instance_members() {
    both(
        Case::Upper,
        "main.p",
        "DEFINE VARIABLE iX AS INTEGER NO-UNDO.\niX = NEW app.Util():|",
        |answer| {
            let list = answer.list();
            instance_members(list);
            list.lacks(&["cHidden", "iCount"]);
        },
    );
    both(
        Case::Upper,
        "main.p",
        "DEFINE VARIABLE oAny AS Progress.Lang.Object NO-UNDO.\nCAST(oAny, app.Util):|",
        |answer| {
            let list = answer.list();
            instance_members(list);
            list.lacks(&["cHidden", "iCount"]);
        },
    );
}

/// M7.
#[test]
fn a_chain_follows_the_type_of_a_property_or_a_method() {
    both(Case::Upper, "main.p", &main_p("oUtil:Child:|"), |answer| {
        let list = answer.list();
        list.contains(PROPERTY, &["Size"]);
        list.contains(METHOD, &["Measure"]);
        list.lacks(&["Greet", "Title"]);
    });
    both(Case::Upper, "main.p", &main_p("oUtil:Make():|"), |answer| {
        let list = answer.list();
        list.contains(METHOD, &["Measure"]);
        list.lacks(&["Greet", "Title"]);
    });
}

/// M8.
#[test]
fn an_overloaded_method_is_one_item_with_the_count_of_the_others() {
    let mut ws = files(Case::Upper).start();
    let answer = ws.complete("main.p", &main_p("oUtil:|"));
    let list = answer.list();
    let greets = list.items.iter().filter(|i| label(i) == "Greet").count();
    assert_eq!(greets, 1, "{}", list.summary());
    let detail = list.item(METHOD, "Greet")["detail"]
        .as_str()
        .expect("a detail")
        .to_string();
    assert!(detail.starts_with("METHOD VOID"), "{detail}");
    assert!(detail.contains("(+2 overloads)"), "{detail}");
    let single = list.item(METHOD, "Make")["detail"]
        .as_str()
        .expect("a detail");
    assert!(!single.contains("overload"), "{single}");
}

/// M9.
#[test]
fn an_enum_type_name_gives_its_members() {
    both(Case::Upper, "main.p", "app.Color:|", |answer| {
        must_have(answer.list(), &["Red", "Green", "Blue"]);
    });
}

/// M10.
#[test]
fn a_receiver_that_is_not_a_class_gives_null() {
    let cases = [
        "DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.\nhBuf:|",
        "SESSION:|",
        "FOR EACH Customer:|",
        "oMissing:|",
    ];
    for source in cases {
        for trigger in [Some(":"), None] {
            let mut ws = files(Case::Upper).start();
            let cursor = ws.open_marked("main.p", source);
            ws.complete_at("main.p", cursor, trigger).assert_null();
        }
    }
}

/// M11.
#[test]
fn resolve_gives_each_overload_and_the_declaring_class() {
    let mut ws = files(Case::Upper).start();
    let answer = ws.complete("main.p", &main_p("oUtil:|"));
    let item = answer.list().item(METHOD, "Greet").clone();
    let resolved = ws.resolve(&item);
    assert_eq!(resolved["label"], item["label"]);
    let text = documentation(&resolved);
    assert!(text.matches("Greet").count() >= 3, "{text}");
    assert!(text.contains("pcName AS CHARACTER"), "{text}");
    assert!(text.contains("piTimes AS INTEGER"), "{text}");
    assert!(text.contains("app.Util"), "{text}");
}

/// M12.
#[test]
fn lower_case_and_abbreviated_keywords_find_the_same_members() {
    let source = "class app.Special inherits app.Util:
    define private variable cOwn as character no-undo.
    method public void run():
        this-object:|
    end method.
end class.
";
    both(Case::Lower, "lib/app/Special.cls", source, |answer| {
        let list = answer.list();
        list.contains(PROPERTY, &["cOwn", "Title"]);
        list.contains(METHOD, &["Greet", "run"]);
    });
    let source = "def var oUtil as app.Util no-undo.\noUtil:|";
    both(Case::Lower, "main.p", source, |answer| {
        instance_members(answer.list());
    });
}

/// M13.
#[test]
fn this_object_without_a_colon_is_one_item() {
    for (case, word) in [(Case::Upper, "THIS-OBJ"), (Case::Lower, "this-obj")] {
        let source = format!(
            "{SPECIAL_HEAD}    METHOD PUBLIC VOID Run():\n        {word}|\n    END METHOD.\nEND CLASS.\n"
        );
        let answer = files(case).start().complete("lib/app/Special.cls", &source);
        let list = answer.list();
        let count = list
            .items
            .iter()
            .filter(|i| label(i).eq_ignore_ascii_case("this-object"))
            .count();
        assert_eq!(count, 1, "{}", list.summary());
    }
}

/// M14.
#[test]
fn a_member_access_inside_an_expression_finds_the_receiver() {
    let answer = files(Case::Upper)
        .start()
        .complete("main.p", &main_p("IF oUtil:|"));
    instance_members(answer.list());
}
