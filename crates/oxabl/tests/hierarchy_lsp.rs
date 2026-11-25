//! Go to implementation, go to super method and the type hierarchy through the
//! real `oxabl lsp` and its daemon: interfaces, abstract methods and override
//! chains across files that are not open, supertypes that do not resolve, and
//! answers that can be short.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::time::Duration;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"hierarchy\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

const IPAYMENT: &str = "INTERFACE app.IPayment:\n\
    \x20   METHOD PUBLIC DECIMAL Charge(INPUT pdAmount AS DECIMAL).\n\
    \x20   METHOD PUBLIC VOID Refund(INPUT pdAmount AS DECIMAL).\n\
    END INTERFACE.\n";

const IAUDITED: &str = "INTERFACE app.IAudited:\n\
    \x20   METHOD PUBLIC VOID Audit().\n\
    END INTERFACE.\n";

const IREFUNDABLE: &str = "INTERFACE app.IRefundable INHERITS app.IPayment:\n\
    \x20   METHOD PUBLIC VOID Reverse().\n\
    END INTERFACE.\n";

const CARD: &str = "USING app.*.\n\
    CLASS app.Card IMPLEMENTS IPayment, IAudited:\n\
    \x20   METHOD PUBLIC DECIMAL Charge(INPUT pdAmount AS DECIMAL):\n\
    \x20       RETURN pdAmount.\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Refund(INPUT pdAmount AS DECIMAL):\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Audit():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const GOLD_CARD: &str = "CLASS app.GoldCard INHERITS app.Card:\n\
    \x20   METHOD PUBLIC OVERRIDE DECIMAL Charge(INPUT pdAmount AS DECIMAL):\n\
    \x20       RETURN pdAmount * 2.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CASH: &str = "CLASS app.Cash IMPLEMENTS app.IPayment:\n\
    \x20   METHOD PUBLIC DECIMAL Charge(INPUT pdAmount AS DECIMAL):\n\
    \x20       RETURN pdAmount.\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC DECIMAL Charge(INPUT pcNote AS CHARACTER):\n\
    \x20       RETURN 0.\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Refund(INPUT pdAmount AS DECIMAL):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const BASE_PAYMENT: &str = "CLASS ABSTRACT app.BasePayment IMPLEMENTS app.IPayment:\n\
    \x20   METHOD PUBLIC DECIMAL Charge(INPUT pdAmount AS DECIMAL):\n\
    \x20       RETURN pdAmount.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const SHAPE: &str = "CLASS app.Shape:\n\
    \x20   METHOD PUBLIC CHARACTER Describe():\n\
    \x20       RETURN \"shape\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const POLYGON: &str = "CLASS app.Polygon INHERITS app.Shape:\n\
    \x20   METHOD PUBLIC OVERRIDE CHARACTER Describe():\n\
    \x20       RETURN \"polygon\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const SQUARE: &str = "CLASS app.Square INHERITS app.Polygon:\n\
    \x20   METHOD PUBLIC OVERRIDE CHARACTER Describe():\n\
    \x20       RETURN \"square\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const PLAIN: &str = "CLASS app.Plain INHERITS app.Shape:\n\
    END CLASS.\n";

const UNRELATED: &str = "CLASS app.Unrelated:\n\
    \x20   METHOD PUBLIC CHARACTER Describe():\n\
    \x20       RETURN \"unrelated\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const ACCOUNT: &str = "CLASS ABSTRACT app.Account:\n\
    \x20   METHOD PUBLIC ABSTRACT DECIMAL Fee().\n\
    END CLASS.\n";

const SAVINGS: &str = "CLASS app.Savings INHERITS app.Account:\n\
    \x20   METHOD PUBLIC OVERRIDE DECIMAL Fee():\n\
    \x20       RETURN 1.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CHECKING: &str = "CLASS app.Checking INHERITS app.Account:\n\
    \x20   METHOD PUBLIC OVERRIDE DECIMAL Fee():\n\
    \x20       RETURN 2.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const SHOP: &str = "USING app.*.\n\
    DEFINE VARIABLE pay AS IPayment NO-UNDO.\n\
    DEFINE VARIABLE oCard AS Card NO-UNDO.\n\
    pay = NEW Card().\n\
    pay:Charge(10).\n\
    oCard = NEW Card().\n";

/// A class with a parent, two interfaces and an interface that extends one.
const PREMIUM: &str = "CLASS app.Premium INHERITS app.Card IMPLEMENTS app.IRefundable, app.IAudited:\n\
    END CLASS.\n";

/// A class with a built-in parent, a workspace interface and an interface
/// that no file declares.
const PAY_ERROR: &str = "USING Progress.Lang.*.\n\
    CLASS app.PayError INHERITS AppError IMPLEMENTS app.IPayment, app.IMissing:\n\
    END CLASS.\n";

/// A class that names a supertype spelled like `IPayment` that no file
/// declares: it can implement the interface.
const ORPHAN: &str = "CLASS app.Orphan IMPLEMENTS vendor.IPayment:\n\
    END CLASS.\n";

/// A class whose parent no file declares, spelled like nothing asked about.
const LOST: &str = "CLASS app.Lost INHERITS app.Gone:\n\
    END CLASS.\n";

/// A class file that does not parse.
const BROKEN: &str = "CLASS app.Broken INHERITS app.Card\n\
    METHOD PUBLIC VOID (:\n\
    END CLASS.\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new(files: &[(&str, &str)]) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        for (name, text) in files {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
    }

    /// Every class above, none open.
    fn standard() -> Workspace {
        Workspace::new(&[
            ("oxabl.toml", CONFIG),
            ("app/IPayment.cls", IPAYMENT),
            ("app/IAudited.cls", IAUDITED),
            ("app/IRefundable.cls", IREFUNDABLE),
            ("app/Card.cls", CARD),
            ("app/GoldCard.cls", GOLD_CARD),
            ("app/Cash.cls", CASH),
            ("app/BasePayment.cls", BASE_PAYMENT),
            ("app/Shape.cls", SHAPE),
            ("app/Polygon.cls", POLYGON),
            ("app/Square.cls", SQUARE),
            ("app/Plain.cls", PLAIN),
            ("app/Unrelated.cls", UNRELATED),
            ("app/Account.cls", ACCOUNT),
            ("app/Savings.cls", SAVINGS),
            ("app/Checking.cls", CHECKING),
            ("shop.p", SHOP),
        ])
    }

    /// The standard classes and the ones with several supertypes.
    fn with_extras(extras: &[(&str, &str)]) -> Workspace {
        let ws = Workspace::standard();
        for (name, text) in extras {
            let path = ws.root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        ws
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }
}

/// The UTF-16 `(line, character)` of byte `at` in `text`.
fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

/// The byte of the `nth` (0-based) occurrence of `needle` in `text`.
#[track_caller]
fn nth(text: &str, needle: &str, nth: usize) -> usize {
    text.match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("no occurrence {nth} of `{needle}`"))
        .0
}

/// The cursor one character into occurrence `occurrence` of `needle`.
fn position(file: &Path, text: &str, needle: &str, occurrence: usize) -> Value {
    let (line, character) = utf16_at(text, nth(text, needle, occurrence) + 1);
    json!({
        "textDocument": {"uri": file_uri(file)},
        "position": {"line": line, "character": character}
    })
}

fn range(text: &str, needle: &str, occurrence: usize) -> Value {
    let start = nth(text, needle, occurrence);
    let (line, character) = utf16_at(text, start);
    let (end_line, end_character) = utf16_at(text, start + needle.len());
    json!({
        "start": {"line": line, "character": character},
        "end": {"line": end_line, "character": end_character}
    })
}

fn location(file: &Path, text: &str, needle: &str, occurrence: usize) -> Value {
    json!({"uri": file_uri(file), "range": range(text, needle, occurrence)})
}

fn sorted(mut values: Vec<Value>) -> Vec<Value> {
    values.sort_by_key(Value::to_string);
    values
}

/// What one request produced besides its answer: every `window/showMessage`
/// and every `window/logMessage` sent before it.
struct Answered {
    result: Value,
    shown: Vec<String>,
    logged: Vec<String>,
}

#[track_caller]
fn ask(session: &mut Session, method: &str, params: Value) -> Answered {
    let id = session.next_request_id();
    session.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
    let mut shown = Vec::new();
    let mut logged = Vec::new();
    loop {
        let message = session.wait_for(|_| true);
        let text = message["params"]["message"].as_str().unwrap_or_default();
        if message["method"] == json!("window/showMessage") {
            shown.push(text.to_string());
        }
        if message["method"] == json!("window/logMessage") && text.contains("may be incomplete") {
            logged.push(text.to_string());
        }
        if message["id"] == json!(id) && message.get("method").is_none() {
            assert!(
                message.get("error").is_none(),
                "`{method}` gave an error: {message}"
            );
            return Answered {
                result: message["result"].clone(),
                shown,
                logged,
            };
        }
    }
}

fn locations(result: &Value) -> Vec<Value> {
    match result {
        Value::Null => Vec::new(),
        Value::Array(found) => sorted(found.clone()),
        other => panic!("expected locations, got {other}"),
    }
}

/// The places `textDocument/implementation` gives.
#[track_caller]
fn implementation(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> (Vec<Value>, Vec<String>) {
    let answer = ask(
        session,
        "textDocument/implementation",
        position(file, text, needle, occurrence),
    );
    (locations(&answer.result), answer.shown)
}

#[test]
fn an_interface_is_implemented_by_the_classes_in_other_files() {
    let ws = Workspace::standard();
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);

    // From a type name in a program: every class that implements it, directly
    // or through a class that does. An interface that extends it is not a
    // class; an unrelated class is not listed.
    let (found, warnings) = implementation(&mut session, &shop, SHOP, "IPayment", 0);
    let expected = sorted(vec![
        location(&ws.path("app/Card.cls"), CARD, "app.Card", 0),
        location(&ws.path("app/GoldCard.cls"), GOLD_CARD, "app.GoldCard", 0),
        location(&ws.path("app/Cash.cls"), CASH, "app.Cash", 0),
        location(
            &ws.path("app/BasePayment.cls"),
            BASE_PAYMENT,
            "app.BasePayment",
            0,
        ),
    ]);
    assert_eq!(found, expected);
    assert_eq!(warnings, Vec::<String>::new());

    // The same from the interface's own declaration, which is not open.
    let interface = ws.path("app/IPayment.cls");
    session.open(&interface, IPAYMENT);
    let (found, warnings) = implementation(&mut session, &interface, IPAYMENT, "IPayment", 0);
    assert_eq!(found, expected, "from the declaration");
    assert_eq!(warnings, Vec::<String>::new());
}

#[test]
fn a_class_is_implemented_by_its_subclasses() {
    let ws = Workspace::standard();
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);

    let (found, warnings) = implementation(&mut session, &shop, SHOP, "Card NO-UNDO", 0);
    assert_eq!(
        found,
        vec![location(
            &ws.path("app/GoldCard.cls"),
            GOLD_CARD,
            "app.GoldCard",
            0
        )]
    );
    assert_eq!(warnings, Vec::<String>::new());
}

#[test]
fn an_interface_method_is_implemented_by_the_methods_with_its_parameters() {
    let ws = Workspace::standard();
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);

    // From a call through the interface: Cash's overload with a CHARACTER
    // parameter is another method, so it is not listed.
    let expected = sorted(vec![
        location(&ws.path("app/Card.cls"), CARD, "Charge", 0),
        location(&ws.path("app/GoldCard.cls"), GOLD_CARD, "Charge", 0),
        location(&ws.path("app/Cash.cls"), CASH, "Charge", 0),
        location(&ws.path("app/BasePayment.cls"), BASE_PAYMENT, "Charge", 0),
    ]);
    let (found, warnings) = implementation(&mut session, &shop, SHOP, "Charge", 0);
    assert_eq!(found, expected);
    assert_eq!(warnings, Vec::<String>::new());

    // From the declaration in the interface.
    let interface = ws.path("app/IPayment.cls");
    session.open(&interface, IPAYMENT);
    let (found, _) = implementation(&mut session, &interface, IPAYMENT, "Charge", 0);
    assert_eq!(found, expected, "from the declaration");

    // Another method of the interface has its own implementations.
    let (found, warnings) = implementation(&mut session, &interface, IPAYMENT, "Refund", 0);
    assert_eq!(
        found,
        sorted(vec![
            location(&ws.path("app/Card.cls"), CARD, "Refund", 0),
            location(&ws.path("app/Cash.cls"), CASH, "Refund", 0),
        ])
    );
    assert_eq!(warnings, Vec::<String>::new());
}

#[test]
fn an_abstract_method_is_overridden_by_each_subclass() {
    let ws = Workspace::standard();
    let account = ws.path("app/Account.cls");
    let mut session = Session::start(&ws.root);
    session.open(&account, ACCOUNT);

    let (found, warnings) = implementation(&mut session, &account, ACCOUNT, "Fee", 0);
    assert_eq!(
        found,
        sorted(vec![
            location(&ws.path("app/Savings.cls"), SAVINGS, "Fee", 0),
            location(&ws.path("app/Checking.cls"), CHECKING, "Fee", 0),
        ])
    );
    assert_eq!(warnings, Vec::<String>::new());
}

#[test]
fn an_override_chain_of_three_levels_is_walked_down_and_up() {
    let ws = Workspace::standard();
    let shape = ws.path("app/Shape.cls");
    let polygon = ws.path("app/Polygon.cls");
    let square = ws.path("app/Square.cls");
    let mut session = Session::start(&ws.root);
    session.open(&shape, SHAPE);
    session.open(&polygon, POLYGON);
    session.open(&square, SQUARE);

    // Down: both levels, though the second does not name the first.
    let (found, warnings) = implementation(&mut session, &shape, SHAPE, "Describe", 0);
    assert_eq!(
        found,
        sorted(vec![
            location(&polygon, POLYGON, "Describe", 0),
            location(&square, SQUARE, "Describe", 0),
        ])
    );
    assert_eq!(warnings, Vec::<String>::new());

    // Up: the nearest supertype that declares it.
    let up = |session: &mut Session, file: &Path, text: &str| {
        let answer = ask(
            session,
            "oxabl/superMethod",
            position(file, text, "Describe", 0),
        );
        (
            answer.result["locations"].clone(),
            answer.result["gaps"].clone(),
        )
    };
    let (found, gaps) = up(&mut session, &square, SQUARE);
    assert_eq!(found, json!([location(&polygon, POLYGON, "Describe", 0)]));
    assert_eq!(gaps, json!([]));
    let (found, gaps) = up(&mut session, &polygon, POLYGON);
    assert_eq!(found, json!([location(&shape, SHAPE, "Describe", 0)]));
    assert_eq!(gaps, json!([]));
    // The top of the chain overrides nothing.
    let (found, gaps) = up(&mut session, &shape, SHAPE);
    assert_eq!(found, json!([]));
    assert_eq!(gaps, json!([]));
}

#[test]
fn a_method_that_nothing_overrides_has_an_empty_answer() {
    let ws = Workspace::standard();
    let unrelated = ws.path("app/Unrelated.cls");
    let mut session = Session::start(&ws.root);
    session.open(&unrelated, UNRELATED);

    // A class of its own with a method of the same name as another's.
    let (found, warnings) = implementation(&mut session, &unrelated, UNRELATED, "Describe", 0);
    assert_eq!(found, Vec::<Value>::new());
    assert_eq!(warnings, Vec::<String>::new());

    // A class with no subclass.
    let (found, warnings) = implementation(&mut session, &unrelated, UNRELATED, "app.Unrelated", 0);
    assert_eq!(found, Vec::<Value>::new());
    assert_eq!(warnings, Vec::<String>::new());

    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&unrelated, UNRELATED, "Describe", 0),
    );
    assert_eq!(answer.result, json!({"locations": [], "gaps": []}));
}

const CLASS: u64 = 5;
const INTERFACE: u64 = 11;
const NULL: u64 = 21;

/// One item of a type hierarchy answer.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Node {
    name: String,
    kind: u64,
    detail: String,
    uri: String,
}

fn node(name: &str, kind: u64, detail: &str, file: &Path) -> Node {
    Node {
        name: name.to_string(),
        kind,
        detail: detail.to_string(),
        uri: file_uri(file),
    }
}

fn nodes(result: &Value) -> Vec<Node> {
    let mut found: Vec<Node> = result
        .as_array()
        .unwrap_or_else(|| panic!("expected items, got {result}"))
        .iter()
        .map(|item| Node {
            name: item["name"].as_str().unwrap().to_string(),
            kind: item["kind"].as_u64().unwrap(),
            detail: item["detail"].as_str().unwrap_or_default().to_string(),
            uri: item["uri"].as_str().unwrap().to_string(),
        })
        .collect();
    found.sort();
    found
}

/// The one item prepared at occurrence `occurrence` of `needle`.
#[track_caller]
fn prepared(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Value {
    let answer = ask(
        session,
        "textDocument/prepareTypeHierarchy",
        position(file, text, needle, occurrence),
    );
    assert_eq!(answer.shown, Vec::<String>::new());
    let items = answer.result.as_array().expect("items");
    assert_eq!(items.len(), 1, "items at `{needle}`: {items:?}");
    items[0].clone()
}

fn supertypes(session: &mut Session, item: &Value) -> Answered {
    ask(session, "typeHierarchy/supertypes", json!({"item": item}))
}

fn subtypes(session: &mut Session, item: &Value) -> Answered {
    ask(session, "typeHierarchy/subtypes", json!({"item": item}))
}

/// The `incomplete` item that opens an answer, and the rest of it.
fn without_marker(result: &Value) -> (Option<String>, Vec<Node>) {
    let mut found = nodes(result);
    match found.iter().position(|n| n.name == "incomplete") {
        Some(at) => {
            let marker = found.remove(at);
            assert_eq!(marker.kind, NULL);
            (Some(marker.detail), found)
        }
        None => (None, found),
    }
}

#[test]
fn a_class_shows_its_supertype_and_its_interfaces() {
    let ws = Workspace::with_extras(&[("app/Premium.cls", PREMIUM)]);
    let premium = ws.path("app/Premium.cls");
    let mut session = Session::start(&ws.root);
    session.open(&premium, PREMIUM);

    // The item is the class, with its name and kind.
    let item = prepared(&mut session, &premium, PREMIUM, "app.Premium", 0);
    assert_eq!(item["name"], json!("app.Premium"));
    assert_eq!(item["kind"], json!(CLASS));
    assert_eq!(item["selectionRange"], range(PREMIUM, "app.Premium", 0));

    let answer = supertypes(&mut session, &item);
    assert_eq!(answer.shown, Vec::<String>::new());
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    assert_eq!(
        found,
        vec![
            node("app.Card", CLASS, "INHERITS", &ws.path("app/Card.cls")),
            node(
                "app.IAudited",
                INTERFACE,
                "IMPLEMENTS",
                &ws.path("app/IAudited.cls")
            ),
            node(
                "app.IRefundable",
                INTERFACE,
                "IMPLEMENTS",
                &ws.path("app/IRefundable.cls")
            ),
        ]
    );

    // An interface that extends another shows it, and the item goes on to its
    // own supertypes.
    let interface = ws.path("app/IRefundable.cls");
    session.open(&interface, IREFUNDABLE);
    let refundable = prepared(&mut session, &interface, IREFUNDABLE, "app.IRefundable", 0);
    assert_eq!(refundable["kind"], json!(INTERFACE));
    let answer = supertypes(&mut session, &refundable);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    assert_eq!(
        found,
        vec![node(
            "app.IPayment",
            INTERFACE,
            "INHERITS",
            &ws.path("app/IPayment.cls")
        )]
    );
    let top = &answer.result[0];
    let answer = supertypes(&mut session, top);
    assert_eq!(
        nodes(&answer.result),
        Vec::<Node>::new(),
        "the top has none"
    );
}

#[test]
fn subtypes_are_found_in_files_that_are_not_open() {
    let ws = Workspace::with_extras(&[("app/Premium.cls", PREMIUM)]);
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);

    // The interface, from a type name in a program.
    let item = prepared(&mut session, &shop, SHOP, "IPayment", 0);
    assert_eq!(item["name"], json!("app.IPayment"));
    let answer = subtypes(&mut session, &item);
    assert_eq!(answer.shown, Vec::<String>::new());
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    assert_eq!(
        found,
        vec![
            node(
                "app.BasePayment",
                CLASS,
                "IMPLEMENTS",
                &ws.path("app/BasePayment.cls")
            ),
            node("app.Card", CLASS, "IMPLEMENTS", &ws.path("app/Card.cls")),
            node("app.Cash", CLASS, "IMPLEMENTS", &ws.path("app/Cash.cls")),
            node(
                "app.IRefundable",
                INTERFACE,
                "INHERITS",
                &ws.path("app/IRefundable.cls")
            ),
        ],
        "the direct subtypes: classes and the interface that extends it"
    );

    // One level at a time: the subtypes of a subtype.
    let card = &answer
        .result
        .as_array()
        .unwrap()
        .iter()
        .find(|i| i["name"] == "app.Card")
        .unwrap()
        .clone();
    let answer = subtypes(&mut session, card);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    assert_eq!(
        found,
        vec![
            node(
                "app.GoldCard",
                CLASS,
                "INHERITS",
                &ws.path("app/GoldCard.cls")
            ),
            node(
                "app.Premium",
                CLASS,
                "INHERITS",
                &ws.path("app/Premium.cls")
            ),
        ]
    );

    // A class with no subtype has an empty list.
    let unrelated = ws.path("app/Unrelated.cls");
    session.open(&unrelated, UNRELATED);
    let item = prepared(&mut session, &unrelated, UNRELATED, "app.Unrelated", 0);
    let answer = subtypes(&mut session, &item);
    assert_eq!(answer.result, json!([]));
    assert_eq!(answer.shown, Vec::<String>::new());
}

#[test]
fn a_supertype_that_oxabl_cannot_resolve_is_shown_as_unresolved() {
    let ws = Workspace::with_extras(&[("app/PayError.cls", PAY_ERROR)]);
    let error = ws.path("app/PayError.cls");
    let mut session = Session::start(&ws.root);
    session.open(&error, PAY_ERROR);
    let item = prepared(&mut session, &error, PAY_ERROR, "app.PayError", 0);

    let answer = supertypes(&mut session, &item);
    assert_eq!(answer.shown, Vec::<String>::new());
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    assert_eq!(found.len(), 3, "{found:?}");
    // A supertype of the workspace.
    assert!(found.contains(&node(
        "app.IPayment",
        INTERFACE,
        "IMPLEMENTS",
        &ws.path("app/IPayment.cls")
    )));
    // A name no file declares: an item that says so, at the class that names it.
    let missing = found.iter().find(|n| n.name == "app.IMissing").unwrap();
    assert_eq!(missing.kind, NULL);
    assert!(
        missing.detail.starts_with("unresolved: no file"),
        "{}",
        missing.detail
    );
    assert_eq!(missing.uri, file_uri(&error));
    // A built-in type: by name, with no file.
    let builtin = found
        .iter()
        .find(|n| n.name == "Progress.Lang.AppError")
        .unwrap();
    assert_eq!(builtin.kind, CLASS);
    assert!(builtin.detail.contains("built-in"), "{}", builtin.detail);
    assert_eq!(builtin.uri, file_uri(&error));
}

/// A class that overrides a method of a parent no file declares, and one that
/// overrides a method of `Progress.Lang.Object`.
const HEIR: &str = "CLASS app.Heir INHERITS app.Gone:\n\
    \x20   METHOD PUBLIC OVERRIDE CHARACTER Describe():\n\
    \x20       RETURN \"heir\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const NAMED: &str = "CLASS app.Named:\n\
    \x20   METHOD PUBLIC OVERRIDE CHARACTER ToString():\n\
    \x20       RETURN \"named\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_supertype_spelled_like_the_asked_class_that_does_not_resolve_is_a_gap() {
    let ws = Workspace::with_extras(&[("app/Orphan.cls", ORPHAN), ("app/Lost.cls", LOST)]);
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);

    // The list of classes that implement the interface is found, and the
    // answer says that `Orphan` can be one more.
    let (found, warnings) = implementation(&mut session, &shop, SHOP, "IPayment", 0);
    assert_eq!(found.len(), 4, "the classes found stay: {found:?}");
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert!(
        warnings[0].contains("`app.Orphan` names `vendor.IPayment`")
            && warnings[0].contains("it may extend `app.IPayment`"),
        "{}",
        warnings[0]
    );

    // The same in the type hierarchy: a first item `incomplete` and the warning.
    let item = prepared(&mut session, &shop, SHOP, "IPayment", 0);
    let answer = subtypes(&mut session, &item);
    assert_eq!(answer.shown.len(), 1, "{:?}", answer.shown);
    let (marker, found) = without_marker(&answer.result);
    let marker = marker.expect("the answer must be marked incomplete");
    assert!(
        marker.contains("`app.Orphan` names `vendor.IPayment`"),
        "{marker}"
    );
    assert_eq!(found.len(), 4, "the subtypes found stay: {found:?}");
    assert_eq!(answer.result[0]["name"], json!("incomplete"));

    // The marker item takes no part in the hierarchy.
    let answer = ask(
        &mut session,
        "typeHierarchy/subtypes",
        json!({"item": answer.result[0]}),
    );
    assert_eq!(answer.result, json!([]));

    // A class that names a missing parent, spelled like nothing asked about,
    // is a standing gap: it goes to the log, once, and into the marker.
    let item = prepared(&mut session, &shop, SHOP, "Card NO-UNDO", 0);
    let answer = subtypes(&mut session, &item);
    assert_eq!(answer.shown, Vec::<String>::new());
    let (marker, _) = without_marker(&answer.result);
    assert!(
        marker.is_some_and(|reason| reason.contains("`app.Lost` naming `app.Gone`")),
        "the standing gap is in the marker"
    );
}

#[test]
fn a_class_file_that_does_not_parse_is_a_standing_gap() {
    let ws = Workspace::with_extras(&[("app/Broken.cls", BROKEN)]);
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);

    let answer = ask(
        &mut session,
        "textDocument/implementation",
        position(&shop, SHOP, "IPayment", 0),
    );
    assert_eq!(locations(&answer.result).len(), 4);
    assert_eq!(answer.shown, Vec::<String>::new());
    assert_eq!(answer.logged.len(), 1, "{:?}", answer.logged);
    assert!(
        answer.logged[0].contains("1 class file(s) could not be read, such as")
            && answer.logged[0].contains("Broken.cls"),
        "{}",
        answer.logged[0]
    );
    // Once per session.
    let answer = ask(
        &mut session,
        "textDocument/implementation",
        position(&shop, SHOP, "IPayment", 0),
    );
    assert_eq!(answer.logged, Vec::<String>::new());
}

#[test]
fn a_workspace_with_no_include_path_does_not_search_its_classes() {
    let ws = Workspace::standard();
    std::fs::remove_file(ws.path("oxabl.toml")).unwrap();
    let interface = ws.path("app/IPayment.cls");
    let mut session = Session::start(&ws.root);
    session.open(&interface, IPAYMENT);

    let item = prepared(&mut session, &interface, IPAYMENT, "app.IPayment", 0);
    let answer = subtypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(found, Vec::<Node>::new(), "nothing was searched");
    assert!(
        marker.is_some_and(|reason| reason.contains("no include path")),
        "an empty list that looks complete is the fault"
    );
    assert_eq!(answer.shown.len(), 1, "{:?}", answer.shown);
}

#[test]
fn an_open_buffer_is_read_over_the_file_on_disk() {
    let ws = Workspace::standard();
    let shape = ws.path("app/Shape.cls");
    let plain = ws.path("app/Plain.cls");
    let polygon = ws.path("app/Polygon.cls");
    let mut session = Session::start(&ws.root);
    session.open(&shape, SHAPE);
    session.open(&polygon, POLYGON);
    let shape_item = prepared(&mut session, &shape, SHAPE, "app.Shape", 0);
    let polygon_item = prepared(&mut session, &polygon, POLYGON, "app.Polygon", 0);

    let names = |answer: &Answered| {
        without_marker(&answer.result)
            .1
            .into_iter()
            .map(|n| n.name)
            .collect::<Vec<_>>()
    };
    assert_eq!(
        names(&subtypes(&mut session, &shape_item)),
        vec!["app.Plain", "app.Polygon"]
    );

    // Unsaved: `Plain` now extends `Polygon`. The disk still says `Shape`.
    let edited = PLAIN.replace("app.Shape", "app.Polygon");
    session.open(&plain, &edited);
    assert_eq!(
        names(&subtypes(&mut session, &shape_item)),
        vec!["app.Polygon"]
    );
    assert_eq!(
        names(&subtypes(&mut session, &polygon_item)),
        vec!["app.Plain", "app.Square"]
    );
}

#[test]
fn a_class_file_that_changes_on_disk_changes_the_answer() {
    let ws = Workspace::standard();
    let shape = ws.path("app/Shape.cls");
    let square = ws.path("app/Square.cls");
    let mut session = Session::start(&ws.root);
    session.open(&shape, SHAPE);
    let item = prepared(&mut session, &shape, SHAPE, "app.Shape", 0);
    let names = |answer: &Answered| {
        without_marker(&answer.result)
            .1
            .into_iter()
            .map(|n| n.name)
            .collect::<Vec<_>>()
    };
    assert_eq!(
        names(&subtypes(&mut session, &item)),
        vec!["app.Plain", "app.Polygon"]
    );

    // A header that changes.
    std::fs::write(&square, SQUARE.replace("app.Polygon", "app.Shape")).unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&square), "type": 2}]}
    }));
    // A class file that is new.
    let triangle = ws.path("app/Triangle.cls");
    std::fs::write(
        &triangle,
        "CLASS app.Triangle INHERITS app.Shape:\nEND CLASS.\n",
    )
    .unwrap();
    std::thread::sleep(Duration::from_millis(1_200));
    assert_eq!(
        names(&subtypes(&mut session, &item)),
        vec!["app.Plain", "app.Polygon", "app.Square", "app.Triangle"]
    );
}

#[test]
fn a_super_method_that_may_be_in_an_unread_class_is_a_gap() {
    let ws = Workspace::with_extras(&[("app/Heir.cls", HEIR), ("app/Named.cls", NAMED)]);
    let heir = ws.path("app/Heir.cls");
    let named = ws.path("app/Named.cls");
    let mut session = Session::start(&ws.root);
    session.open(&heir, HEIR);
    session.open(&named, NAMED);

    // The parent is missing: no super method is found, and the answer says why.
    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&heir, HEIR, "Describe", 0),
    );
    assert_eq!(answer.result["locations"], json!([]));
    let gaps = answer.result["gaps"].as_array().unwrap();
    assert_eq!(gaps.len(), 1, "{gaps:?}");
    let gap = gaps[0].as_str().unwrap();
    assert!(
        gap.contains("`app.Heir` names `app.Gone`") && gap.contains("the method may be there"),
        "{gap}"
    );

    // A method of `Progress.Lang.Object`, which oxabl does not know.
    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&named, NAMED, "ToString", 0),
    );
    assert_eq!(answer.result["locations"], json!([]));
    let gap = answer.result["gaps"][0].as_str().unwrap();
    assert!(gap.contains("`Progress.Lang.Object`"), "{gap}");
}

#[test]
fn the_server_advertises_the_requests_it_answers() {
    let ws = Workspace::standard();
    let session = Session::start(&ws.root);
    let capabilities = &session.initialized["result"]["capabilities"];
    assert_eq!(capabilities["implementationProvider"], json!(true));
    assert_eq!(capabilities["typeHierarchyProvider"], json!(true));
}

const IDESCRIBE: &str = "INTERFACE app.IDescribe:\n\
    \x20   METHOD PUBLIC CHARACTER Describe().\n\
    END INTERFACE.\n";

/// Its parent chain reaches `app.Shape.Describe` two levels up, and its
/// interface declares `Describe` one level up.
const HYBRID: &str = "CLASS app.Hybrid INHERITS app.Plain IMPLEMENTS app.IDescribe:\n\
    \x20   METHOD PUBLIC OVERRIDE CHARACTER Describe():\n\
    \x20       RETURN \"hybrid\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_method_overrides_the_nearest_class_method_and_implements_the_interface_method() {
    let ws =
        Workspace::with_extras(&[("app/IDescribe.cls", IDESCRIBE), ("app/Hybrid.cls", HYBRID)]);
    let hybrid = ws.path("app/Hybrid.cls");
    let mut session = Session::start(&ws.root);
    session.open(&hybrid, HYBRID);

    // The interface is one level up. `Shape` is two levels up through `Plain`,
    // which does not declare it, and it is the method that `OVERRIDE`
    // overrides. Both are answers: the compiler accepts this class.
    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&hybrid, HYBRID, "Describe():", 0),
    );
    assert_eq!(
        answer.result,
        json!({
            "locations": sorted(vec![
                location(&ws.path("app/IDescribe.cls"), IDESCRIBE, "Describe", 1),
                location(&ws.path("app/Shape.cls"), SHAPE, "Describe", 0),
            ]),
            "gaps": []
        })
    );
}

const VAULT: &str = "CLASS app.Vault:\n\
    \x20   METHOD PRIVATE VOID Seal():\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC STATIC VOID Make():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const SAFE: &str = "CLASS app.Safe INHERITS app.Vault:\n\
    \x20   METHOD PUBLIC VOID Seal():\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC STATIC VOID Make():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_private_or_static_method_has_no_override() {
    let ws = Workspace::with_extras(&[("app/Vault.cls", VAULT), ("app/Safe.cls", SAFE)]);
    let vault = ws.path("app/Vault.cls");
    let mut session = Session::start(&ws.root);
    session.open(&vault, VAULT);

    for method in ["Seal", "Make"] {
        let (found, warnings) = implementation(&mut session, &vault, VAULT, method, 0);
        assert_eq!(found, Vec::<Value>::new(), "{method}");
        assert_eq!(warnings, Vec::<String>::new(), "{method}");
    }
}

#[test]
fn the_item_is_the_class_that_holds_the_cursor_when_the_name_is_no_class() {
    let ws = Workspace::standard();
    let card = ws.path("app/Card.cls");
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&card, CARD);
    session.open(&shop, SHOP);

    // In a method body of a class file.
    let item = prepared(&mut session, &card, CARD, "RETURN pdAmount", 0);
    assert_eq!(item["name"], json!("app.Card"));
    assert_eq!(item["uri"], json!(file_uri(&card)));

    // In a program that is no class: no item, and no error.
    let answer = ask(
        &mut session,
        "textDocument/prepareTypeHierarchy",
        position(&shop, SHOP, "pay = NEW", 0),
    );
    assert!(
        answer.result.is_null() || answer.result == json!([]),
        "{}",
        answer.result
    );
}

#[test]
fn a_file_that_can_no_longer_be_read_is_a_standing_gap() {
    let ws = Workspace::standard();
    let shop = ws.path("shop.p");
    let mut session = Session::start(&ws.root);
    session.open(&shop, SHOP);
    let (found, _) = implementation(&mut session, &shop, SHOP, "IPayment", 0);
    assert_eq!(found.len(), 4);

    // A class file that cannot be read after the pass: the pass runs again,
    // and the answer says that a file could not be analysed.
    let cash = ws.path("app/Cash.cls");
    std::fs::set_permissions(&cash, std::os::unix::fs::PermissionsExt::from_mode(0o000)).unwrap();
    let answer = ask(
        &mut session,
        "textDocument/implementation",
        position(&shop, SHOP, "IPayment", 0),
    );
    std::fs::set_permissions(&cash, std::os::unix::fs::PermissionsExt::from_mode(0o644)).unwrap();
    assert!(
        answer
            .logged
            .iter()
            .any(|line| line.contains("1 file(s) of the workspace could not be analysed")),
        "{:?}",
        answer.logged
    );
}

#[test]
fn an_index_that_stops_at_its_limit_says_so() {
    // One class file more than the index reads.
    let names: Vec<String> = (0..20_001).map(|n| format!("app/B{n:05}.cls")).collect();
    let texts: Vec<String> = (0..20_001)
        .map(|n| format!("CLASS app.B{n:05} INHERITS app.Base:\nEND CLASS.\n"))
        .collect();
    let mut files: Vec<(&str, &str)> = vec![
        ("oxabl.toml", CONFIG),
        ("app/Base.cls", "CLASS app.Base:\nEND CLASS.\n"),
    ];
    files.extend(
        names
            .iter()
            .map(String::as_str)
            .zip(texts.iter().map(String::as_str)),
    );
    let ws = Workspace::new(&files);
    let base = ws.path("app/Base.cls");
    let mut session = Session::start(&ws.root);
    session.open(&base, "CLASS app.Base:\nEND CLASS.\n");

    let item = prepared(
        &mut session,
        &base,
        "CLASS app.Base:\nEND CLASS.\n",
        "app.Base",
        0,
    );
    let answer = subtypes(&mut session, &item);
    assert_eq!(answer.shown.len(), 1, "{:?}", answer.shown);
    assert!(
        answer.shown[0].contains("the index reads the first 20000"),
        "{}",
        answer.shown[0]
    );
    let (marker, found) = without_marker(&answer.result);
    assert!(marker.is_some(), "the answer must be marked incomplete");
    assert!(found.len() < 20_001, "{}", found.len());
}

// ---------------------------------------------------------------------------
// The compiler's rules for an inherited method, an override, and an interface
// ---------------------------------------------------------------------------

/// The location of `name`, inside the first occurrence of `context`.
#[track_caller]
fn name_in(file: &Path, text: &str, context: &str, name: &str) -> Value {
    let start = nth(text, context, 0) + context.find(name).expect("name in context");
    let (line, character) = utf16_at(text, start);
    let (end_line, end_character) = utf16_at(text, start + name.len());
    json!({
        "uri": file_uri(file),
        "range": {
            "start": {"line": line, "character": character},
            "end": {"line": end_line, "character": end_character}
        }
    })
}

/// A workspace of exactly these files.
fn small(files: &[(&str, &str)]) -> Workspace {
    let mut all = vec![("oxabl.toml", CONFIG)];
    all.extend_from_slice(files);
    Workspace::new(&all)
}

const IFACE_I: &str = "INTERFACE app.I:\n\
    \x20   METHOD PUBLIC VOID M().\n\
    END INTERFACE.\n";

const CLASS_BASE: &str = "CLASS app.Base:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// Declares nothing: `app.Base` supplies the method of the interface.
const CLASS_C: &str = "CLASS app.C INHERITS app.Base IMPLEMENTS app.I:\n\
    END CLASS.\n";

const CLASS_C_SUB: &str = "CLASS app.CSub INHERITS app.C:\n\
    END CLASS.\n";

#[test]
fn an_interface_method_is_implemented_by_the_method_a_class_inherits() {
    let ws = small(&[
        ("app/I.cls", IFACE_I),
        ("app/Base.cls", CLASS_BASE),
        ("app/C.cls", CLASS_C),
        ("app/CSub.cls", CLASS_C_SUB),
    ]);
    let iface = ws.path("app/I.cls");
    let mut session = Session::start(&ws.root);
    session.open(&iface, IFACE_I);

    // `C` and `CSub` declare nothing; the one implementation is `Base.M`,
    // which is no subtype of the interface.
    let (found, warnings) = implementation(&mut session, &iface, IFACE_I, "M()", 0);
    assert_eq!(
        found,
        vec![name_in(
            &ws.path("app/Base.cls"),
            CLASS_BASE,
            "VOID M(",
            "M"
        )]
    );
    assert_eq!(warnings, Vec::<String>::new());
}

#[test]
fn an_inherited_method_that_cannot_be_read_is_a_gap_of_the_implementations() {
    let ws = small(&[
        ("app/I.cls", IFACE_I),
        (
            "app/Lost.cls",
            "CLASS app.Lost INHERITS app.Gone IMPLEMENTS app.I:\nEND CLASS.\n",
        ),
    ]);
    let iface = ws.path("app/I.cls");
    let mut session = Session::start(&ws.root);
    session.open(&iface, IFACE_I);
    let (found, warnings) = implementation(&mut session, &iface, IFACE_I, "M()", 0);
    assert_eq!(found, Vec::<Value>::new());
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert!(
        warnings[0].contains("`app.Lost` extends `app.Gone`")
            && warnings[0].contains("may declare `M`"),
        "{}",
        warnings[0]
    );
}

const CLASS_A: &str = "CLASS app.A:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CLASS_B: &str = "CLASS app.B INHERITS app.A:\n\
    END CLASS.\n";

const IFACE_J: &str = "INTERFACE app.J:\n\
    \x20   METHOD PUBLIC VOID M().\n\
    END INTERFACE.\n";

/// The compiler accepts it: `OVERRIDE` overrides `app.A.M`, and the method
/// also implements `app.J.M`.
const CLASS_D: &str = "CLASS app.D INHERITS app.B IMPLEMENTS app.J:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// The compiler refuses it (the method hides `app.A.M` with no `OVERRIDE`).
/// The answer for it is the same.
const CLASS_E: &str = "CLASS app.E INHERITS app.B IMPLEMENTS app.J:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn an_override_that_implements_an_interface_has_both_super_methods() {
    let ws = small(&[
        ("app/A.cls", CLASS_A),
        ("app/B.cls", CLASS_B),
        ("app/J.cls", IFACE_J),
        ("app/D.cls", CLASS_D),
        ("app/E.cls", CLASS_E),
    ]);
    let mut session = Session::start(&ws.root);
    let both = json!({
        "locations": sorted(vec![
            name_in(&ws.path("app/A.cls"), CLASS_A, "VOID M(", "M"),
            name_in(&ws.path("app/J.cls"), IFACE_J, "VOID M(", "M"),
        ]),
        "gaps": []
    });
    for (file, text) in [("app/D.cls", CLASS_D), ("app/E.cls", CLASS_E)] {
        let path = ws.path(file);
        session.open(&path, text);
        let answer = ask(
            &mut session,
            "oxabl/superMethod",
            position(&path, text, "M()", 0),
        );
        assert_eq!(answer.result, both, "{file}");
    }

    // From above: both classes declare a method like `app.A.M` and like
    // `app.J.M`, and `app.B` declares none.
    let a = ws.path("app/A.cls");
    session.open(&a, CLASS_A);
    let (found, warnings) = implementation(&mut session, &a, CLASS_A, "M()", 0);
    assert_eq!(
        found,
        sorted(vec![
            name_in(&ws.path("app/D.cls"), CLASS_D, "VOID M(", "M"),
            name_in(&ws.path("app/E.cls"), CLASS_E, "VOID M(", "M"),
        ])
    );
    assert_eq!(warnings, Vec::<String>::new());
    let j = ws.path("app/J.cls");
    session.open(&j, IFACE_J);
    let (found, _) = implementation(&mut session, &j, IFACE_J, "M()", 0);
    assert_eq!(
        found,
        sorted(vec![
            name_in(&ws.path("app/D.cls"), CLASS_D, "VOID M(", "M"),
            name_in(&ws.path("app/E.cls"), CLASS_E, "VOID M(", "M"),
        ])
    );
}

// ---------------------------------------------------------------------------
// A class-typed parameter means the class its own file names
// ---------------------------------------------------------------------------

const PERSON_1: &str = "CLASS p1.Person:\nEND CLASS.\n";
const PERSON_2: &str = "CLASS p2.Person:\nEND CLASS.\n";

const CLASS_K: &str = "USING p1.*.\n\
    CLASS app.K:\n\
    \x20   METHOD PUBLIC VOID M(INPUT x AS Person):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// `Person` here is another class: this is an overload of `app.K.M`.
const CLASS_L: &str = "USING p2.*.\n\
    CLASS app.L INHERITS app.K:\n\
    \x20   METHOD PUBLIC VOID M(INPUT x AS Person):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// The same `Person`: an override.
const CLASS_N: &str = "USING p1.*.\n\
    CLASS app.N INHERITS app.K:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID M(INPUT x AS Person):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn class_typed_parameters_are_compared_by_the_class_not_the_spelling() {
    let ws = small(&[
        ("p1/Person.cls", PERSON_1),
        ("p2/Person.cls", PERSON_2),
        ("app/K.cls", CLASS_K),
        ("app/L.cls", CLASS_L),
        ("app/N.cls", CLASS_N),
    ]);
    let k = ws.path("app/K.cls");
    let l = ws.path("app/L.cls");
    let n = ws.path("app/N.cls");
    let mut session = Session::start(&ws.root);
    session.open(&k, CLASS_K);

    // Only the method with the same `Person` overrides.
    let (found, warnings) = implementation(&mut session, &k, CLASS_K, "M(", 0);
    assert_eq!(found, vec![name_in(&n, CLASS_N, "VOID M(", "M")]);
    assert_eq!(warnings, Vec::<String>::new());

    // The overload has no super method, and says nothing of doubt.
    session.open(&l, CLASS_L);
    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&l, CLASS_L, "M(", 0),
    );
    assert_eq!(answer.result, json!({"locations": [], "gaps": []}));
    session.open(&n, CLASS_N);
    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&n, CLASS_N, "M(", 0),
    );
    assert_eq!(
        answer.result,
        json!({"locations": [name_in(&k, CLASS_K, "VOID M(", "M")], "gaps": []})
    );
}

const GONE_PARENT: &str = "CLASS app.GoneParent:\n\
    \x20   METHOD PUBLIC VOID M(INPUT x AS vendor.Missing):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const GONE_CHILD: &str = "CLASS app.GoneChild INHERITS app.GoneParent:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID M(INPUT x AS vendor.Missing):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_class_name_that_resolves_nowhere_is_one_name_when_both_lists_write_it_alike() {
    let ws = small(&[
        ("app/GoneParent.cls", GONE_PARENT),
        ("app/GoneChild.cls", GONE_CHILD),
    ]);
    let parent = ws.path("app/GoneParent.cls");
    let mut session = Session::start(&ws.root);
    session.open(&parent, GONE_PARENT);
    let (found, warnings) = implementation(&mut session, &parent, GONE_PARENT, "M(", 0);
    assert_eq!(
        found,
        vec![name_in(
            &ws.path("app/GoneChild.cls"),
            GONE_CHILD,
            "VOID M(",
            "M"
        )]
    );
    assert_eq!(warnings, Vec::<String>::new());
}

// ---------------------------------------------------------------------------
// An open buffer is read for the supertypes too
// ---------------------------------------------------------------------------

const PARENT_DISK: &str = "CLASS app.Parent:\n\
    \x20   METHOD PUBLIC VOID Other():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const PARENT_BUFFER: &str = "CLASS app.Parent:\n\
    \x20   METHOD PUBLIC VOID Other():\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Fresh():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CHILD: &str = "CLASS app.Child INHERITS app.Parent:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID Fresh():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_super_method_is_read_from_the_open_buffer_of_the_parent() {
    let ws = small(&[("app/Parent.cls", PARENT_DISK), ("app/Child.cls", CHILD)]);
    let parent = ws.path("app/Parent.cls");
    let child = ws.path("app/Child.cls");
    let mut session = Session::start(&ws.root);
    session.open(&parent, PARENT_BUFFER);
    session.open(&child, CHILD);

    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&child, CHILD, "Fresh", 0),
    );
    assert_eq!(
        answer.result,
        json!({
            "locations": [name_in(&parent, PARENT_BUFFER, "VOID Fresh(", "Fresh")],
            "gaps": []
        })
    );
}

// ---------------------------------------------------------------------------
// Root-relative search paths
// ---------------------------------------------------------------------------

const ROOT_RELATIVE: &str = "[workspace]\nname = \"hierarchy\"\n\
    [workspace.sources]\nroot_relative_include_paths = [\".\"]\n";

const ROOT_BASE: &str = "CLASS app.RootBase:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const ROOT_HEIR: &str = "CLASS app.RootHeir INHERITS app.RootBase:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_class_found_through_a_root_relative_path_is_resolved() {
    let ws = Workspace::new(&[
        ("oxabl.toml", ROOT_RELATIVE),
        ("app/RootBase.cls", ROOT_BASE),
        ("app/RootHeir.cls", ROOT_HEIR),
    ]);
    let base = ws.path("app/RootBase.cls");
    let heir = ws.path("app/RootHeir.cls");
    let mut session = Session::start(&ws.root);
    session.open(&base, ROOT_BASE);
    session.open(&heir, ROOT_HEIR);

    let item = prepared(&mut session, &base, ROOT_BASE, "app.RootBase", 0);
    let answer = subtypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    assert_eq!(found, vec![node("app.RootHeir", CLASS, "INHERITS", &heir)]);

    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&heir, ROOT_HEIR, "M()", 0),
    );
    assert_eq!(
        answer.result,
        json!({"locations": [name_in(&base, ROOT_BASE, "VOID M(", "M")], "gaps": []})
    );
}

// ---------------------------------------------------------------------------
// Built-in types, and a cursor on no method
// ---------------------------------------------------------------------------

const MY_ERR: &str = "USING Progress.Lang.*.\n\
    CLASS app.MyErr INHERITS AppError:\n\
    END CLASS.\n";

const SUB_ERR: &str = "CLASS app.SubErr INHERITS app.MyErr:\n\
    END CLASS.\n";

const ERR_USE: &str = "DEFINE VARIABLE e AS Progress.Lang.AppError NO-UNDO.\n\
    DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\n\
    DEFINE VARIABLE x AS app.Gone NO-UNDO.\n\
    o:ToString().\n\
    x:Foo().\n";

#[test]
fn a_built_in_type_lists_the_workspace_classes_that_extend_it() {
    let ws = small(&[
        ("app/MyErr.cls", MY_ERR),
        ("app/SubErr.cls", SUB_ERR),
        ("use.p", ERR_USE),
    ]);
    let user = ws.path("use.p");
    let mut session = Session::start(&ws.root);
    session.open(&user, ERR_USE);

    let answer = ask(
        &mut session,
        "textDocument/implementation",
        position(&user, ERR_USE, "AppError", 0),
    );
    assert_eq!(
        locations(&answer.result),
        sorted(vec![
            location(&ws.path("app/MyErr.cls"), MY_ERR, "app.MyErr", 0),
            location(&ws.path("app/SubErr.cls"), SUB_ERR, "app.SubErr", 0),
        ])
    );
    assert_eq!(answer.shown.len(), 1, "{:?}", answer.shown);
    assert!(
        answer.shown[0].contains("only the classes of the workspace are listed"),
        "{}",
        answer.shown[0]
    );

    // The item of the built-in type is the name at the cursor, with no file
    // of its own; its subtypes are the workspace classes that name it.
    let item = prepared(&mut session, &user, ERR_USE, "AppError", 0);
    assert_eq!(item["name"], json!("Progress.Lang.AppError"));
    assert_eq!(item["kind"], json!(CLASS));
    assert_eq!(item["uri"], json!(file_uri(&user)));
    let answer = subtypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert!(marker.is_some(), "only workspace classes are listed");
    assert_eq!(
        found,
        vec![node(
            "app.MyErr",
            CLASS,
            "INHERITS",
            &ws.path("app/MyErr.cls")
        )]
    );
}

#[test]
fn the_name_of_a_built_in_type_in_a_header_is_the_item_asked_about() {
    let ws = small(&[("app/MyErr.cls", MY_ERR)]);
    let my_err = ws.path("app/MyErr.cls");
    let mut session = Session::start(&ws.root);
    session.open(&my_err, MY_ERR);
    let item = prepared(&mut session, &my_err, MY_ERR, "AppError", 0);
    assert_eq!(item["name"], json!("Progress.Lang.AppError"));
    assert_eq!(item["uri"], json!(file_uri(&my_err)));
    assert_eq!(item["selectionRange"], range(MY_ERR, "AppError", 0));
}

#[test]
fn a_method_of_a_built_in_class_has_no_implementations_that_oxabl_lists() {
    let ws = small(&[("use.p", ERR_USE)]);
    let user = ws.path("use.p");
    let mut session = Session::start(&ws.root);
    session.open(&user, ERR_USE);
    let (found, warnings) = implementation(&mut session, &user, ERR_USE, "ToString", 0);
    assert_eq!(found, Vec::<Value>::new());
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert!(
        warnings[0].contains("no class or method of the workspace at the cursor"),
        "{}",
        warnings[0]
    );
}

#[test]
fn a_cursor_on_no_method_is_not_an_answer_about_overrides() {
    let ws = small(&[("use.p", ERR_USE)]);
    let user = ws.path("use.p");
    let mut session = Session::start(&ws.root);
    session.open(&user, ERR_USE);

    // On a variable, on a call through a type that does not resolve, and on
    // a call of a built-in method.
    for needle in ["e AS", "Foo", "ToString"] {
        let answer = ask(
            &mut session,
            "oxabl/superMethod",
            position(&user, ERR_USE, needle, 0),
        );
        assert_eq!(answer.result["locations"], json!([]), "{needle}");
        assert_eq!(answer.result["gaps"], json!([]), "{needle}");
        let reason = answer.result["notAMethod"].as_str().unwrap_or_else(|| {
            panic!("`{needle}` gave no reason: {}", answer.result);
        });
        assert!(reason.contains("no method at the cursor"), "{reason}");
    }
}

// ---------------------------------------------------------------------------
// Smaller answers
// ---------------------------------------------------------------------------

const CYCLE_X: &str = "CLASS app.X INHERITS app.Y:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CYCLE_Y: &str = "CLASS app.Y INHERITS app.X:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_class_in_an_inheritance_cycle_is_not_its_own_implementation() {
    let ws = small(&[("app/X.cls", CYCLE_X), ("app/Y.cls", CYCLE_Y)]);
    let x = ws.path("app/X.cls");
    let mut session = Session::start(&ws.root);
    session.open(&x, CYCLE_X);
    let (found, _) = implementation(&mut session, &x, CYCLE_X, "M()", 0);
    assert_eq!(
        found,
        vec![name_in(&ws.path("app/Y.cls"), CYCLE_Y, "VOID M(", "M")]
    );
}

const FRESH: &str = "CLASS app.Fresh INHERITS app.Shape:\n\
    END CLASS.\n";

#[test]
fn a_class_that_is_not_saved_yet_is_a_subtype() {
    let ws = Workspace::standard();
    let shape = ws.path("app/Shape.cls");
    let fresh = ws.path("app/Fresh.cls");
    assert!(!fresh.exists());
    let mut session = Session::start(&ws.root);
    session.open(&shape, SHAPE);
    session.open(&fresh, FRESH);

    let item = prepared(&mut session, &shape, SHAPE, "app.Shape", 0);
    let answer = subtypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None);
    let names: Vec<&str> = found.iter().map(|n| n.name.as_str()).collect();
    assert_eq!(names, ["app.Fresh", "app.Plain", "app.Polygon"]);
}

#[test]
fn a_class_with_no_parent_shows_progress_lang_object() {
    let ws = Workspace::standard();
    let unrelated = ws.path("app/Unrelated.cls");
    let mut session = Session::start(&ws.root);
    session.open(&unrelated, UNRELATED);

    let item = prepared(&mut session, &unrelated, UNRELATED, "app.Unrelated", 0);
    let answer = supertypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(marker, None, "{}", answer.result);
    assert_eq!(
        found,
        vec![node(
            "Progress.Lang.Object",
            CLASS,
            "INHERITS (built-in type, no source file)",
            &unrelated
        )]
    );

    // And the class is below it.
    let object = answer.result[0].clone();
    let answer = subtypes(&mut session, &object);
    let (marker, found) = without_marker(&answer.result);
    assert!(marker.is_some(), "only workspace classes are listed");
    let names: Vec<&str> = found.iter().map(|n| n.name.as_str()).collect();
    assert!(names.contains(&"app.Unrelated"), "{names:?}");
    assert!(names.contains(&"app.Shape"), "{names:?}");
    assert!(!names.contains(&"app.Polygon"), "has a parent: {names:?}");
}

// ---------------------------------------------------------------------------
// The header resolves as go to definition does
// ---------------------------------------------------------------------------

/// What a supertype name stands for.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Means {
    /// A class of the workspace: go to definition finds its file.
    Class,
    /// A built-in type: no file, and an item with a name and no place of its own.
    Builtin,
    /// A name that does not resolve.
    Nothing,
}

#[test]
fn a_supertype_name_means_what_go_to_definition_says_it_means() {
    // (file, text, the written name at the cursor, what it means)
    let cases: [(&str, &str, &str, Means); 8] = [
        (
            "par/A.cls",
            "CLASS par.A INHERITS app.Shape:\nEND CLASS.\n",
            "app.Shape:",
            Means::Class,
        ),
        (
            "par/B.cls",
            "USING app.Shape.\nCLASS par.B INHERITS Shape:\nEND CLASS.\n",
            "Shape:",
            Means::Class,
        ),
        (
            "par/C.cls",
            "USING app.*.\nCLASS par.C INHERITS Shape:\nEND CLASS.\n",
            "Shape:",
            Means::Class,
        ),
        (
            "par/D.cls",
            "CLASS par.D INHERITS Progress.Lang.AppError:\nEND CLASS.\n",
            "AppError:",
            Means::Builtin,
        ),
        (
            "par/E.cls",
            "USING Progress.Lang.*.\nCLASS par.E INHERITS AppError:\nEND CLASS.\n",
            "AppError:",
            Means::Builtin,
        ),
        (
            "par/F.cls",
            "CLASS par.F INHERITS AppError:\nEND CLASS.\n",
            "AppError:",
            Means::Builtin,
        ),
        (
            "par/G.cls",
            "USING app.*.\nCLASS par.G INHERITS Gone:\nEND CLASS.\n",
            "Gone:",
            Means::Nothing,
        ),
        (
            "par/H.cls",
            "CLASS par.H INHERITS APP.shape:\nEND CLASS.\n",
            "shape:",
            Means::Nothing,
        ),
    ];
    let mut files: Vec<(&str, &str)> = vec![("oxabl.toml", CONFIG), ("app/Shape.cls", SHAPE)];
    files.extend(cases.iter().map(|(file, text, ..)| (*file, *text)));
    let ws = Workspace::new(&files);
    let mut session = Session::start(&ws.root);
    for (file, text, needle, means) in cases {
        let path = ws.path(file);
        session.open(&path, text);
        let defined = ask(
            &mut session,
            "textDocument/definition",
            position(&path, text, needle, 0),
        );
        let defined = locations(&defined.result);
        let class_name = text
            .lines()
            .find_map(|line| line.strip_prefix("CLASS "))
            .and_then(|line| line.split_whitespace().next())
            .unwrap();
        let item = prepared(&mut session, &path, text, class_name, 0);
        let answer = supertypes(&mut session, &item);
        let found = nodes(&answer.result);
        assert_eq!(found.len(), 1, "{file}: {:?}", answer.result);
        let found = &found[0];
        match means {
            Means::Class => {
                assert_eq!(defined.len(), 1, "{file}: go to definition: {defined:?}");
                assert_eq!(found.uri, defined[0]["uri"].as_str().unwrap(), "{file}");
                assert_eq!(found.kind, CLASS, "{file}");
            }
            Means::Builtin => {
                assert_eq!(defined, Vec::<Value>::new(), "{file}");
                assert_eq!(found.name, "Progress.Lang.AppError", "{file}");
                assert!(found.detail.contains("built-in type"), "{file}: {found:?}");
            }
            Means::Nothing => {
                assert_eq!(defined, Vec::<Value>::new(), "{file}");
                assert_eq!(found.kind, NULL, "{file}: {found:?}");
                assert!(found.detail.starts_with("unresolved"), "{file}: {found:?}");
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Fail-closed cases: built-in names, cut lists, enums and access
// ---------------------------------------------------------------------------

const BROKEN_APP_ERROR: &str = "CLASS app.AppError INHERITS Progress.Lang.AppError:\n\
    \x20   METHOD PUBLIC VOID (:\n\
    END CLASS.\n";

const OTHER_ERR: &str = "CLASS app.Other INHERITS Progress.Lang.AppError:\n\
    END CLASS.\n";

const SHADOW_USE: &str = "USING app.*.\n\
    DEFINE VARIABLE e AS AppError NO-UNDO.\n";

#[test]
fn a_workspace_class_that_oxabl_cannot_read_is_not_taken_for_the_built_in_type() {
    // `app.AppError` does not parse, so it is not known; the compiler would
    // not pick `Progress.Lang.AppError` for the name either.
    let ws = small(&[
        ("app/AppError.cls", BROKEN_APP_ERROR),
        ("app/Other.cls", OTHER_ERR),
        ("use.p", SHADOW_USE),
    ]);
    let user = ws.path("use.p");
    let mut session = Session::start(&ws.root);
    session.open(&user, SHADOW_USE);

    let answer = ask(
        &mut session,
        "textDocument/prepareTypeHierarchy",
        position(&user, SHADOW_USE, "AppError", 0),
    );
    assert_eq!(
        answer.result.as_array().map_or(0, Vec::len),
        0,
        "no item: {}",
        answer.result
    );
    let (found, _) = implementation(&mut session, &user, SHADOW_USE, "AppError", 0);
    assert_eq!(found, Vec::<Value>::new());
}

const CT_IFACE: &str = "INTERFACE app.IT:\n\
    \x20   METHOD PUBLIC CHARACTER ToString().\n\
    END INTERFACE.\n";

const CT_CLASS: &str = "CLASS app.CT IMPLEMENTS app.IT:\n\
    \x20   METHOD PUBLIC OVERRIDE CHARACTER ToString():\n\
    \x20       RETURN \"ct\".\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn an_interface_match_does_not_hide_an_override_of_an_object_method() {
    let ws = small(&[("app/IT.cls", CT_IFACE), ("app/CT.cls", CT_CLASS)]);
    let ct = ws.path("app/CT.cls");
    let mut session = Session::start(&ws.root);
    session.open(&ct, CT_CLASS);
    let answer = ask(
        &mut session,
        "oxabl/superMethod",
        position(&ct, CT_CLASS, "ToString()", 0),
    );
    assert_eq!(
        answer.result["locations"],
        json!([name_in(
            &ws.path("app/IT.cls"),
            CT_IFACE,
            "CHARACTER ToString(",
            "ToString"
        )])
    );
    let gaps = answer.result["gaps"].as_array().expect("gaps");
    assert!(
        gaps.iter().any(|gap| gap
            .as_str()
            .unwrap_or_default()
            .contains("Progress.Lang.Object")),
        "{gaps:?}"
    );
}

const ENUM_COLOR: &str = "ENUM app.Color:\n\
    \x20   DEFINE ENUM Red Green.\n\
    END ENUM.\n";

#[test]
fn an_enum_says_that_its_built_in_parent_is_not_listed() {
    let ws = small(&[("app/Color.cls", ENUM_COLOR)]);
    let color = ws.path("app/Color.cls");
    let mut session = Session::start(&ws.root);
    session.open(&color, ENUM_COLOR);
    let item = prepared(&mut session, &color, ENUM_COLOR, "app.Color", 0);
    let answer = supertypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(found, Vec::new(), "{}", answer.result);
    let marker = marker.expect("an enum with no listed parent must say so");
    assert!(marker.contains("is an enum"), "{marker}");
}

const PROTECTED_BASE: &str = "CLASS app.PBase:\n\
    \x20   METHOD PROTECTED VOID M():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const PROTECTED_HEIR: &str = "CLASS app.PHeir INHERITS app.PBase IMPLEMENTS app.I:\n\
    END CLASS.\n";

#[test]
fn a_method_that_is_not_public_does_not_implement_an_interface_method() {
    let ws = small(&[
        ("app/I.cls", IFACE_I),
        ("app/Base.cls", CLASS_BASE),
        ("app/C.cls", CLASS_C),
        ("app/PBase.cls", PROTECTED_BASE),
        ("app/PHeir.cls", PROTECTED_HEIR),
    ]);
    let i = ws.path("app/I.cls");
    let mut session = Session::start(&ws.root);
    session.open(&i, IFACE_I);
    let (found, _) = implementation(&mut session, &i, IFACE_I, "M()", 0);
    // `app.C` has the public `app.Base.M`; `app.PHeir` has only a protected
    // one.
    assert_eq!(
        found,
        vec![name_in(
            &ws.path("app/Base.cls"),
            CLASS_BASE,
            "VOID M(",
            "M"
        )]
    );
}

#[test]
fn a_list_of_many_classes_is_cut_and_says_so() {
    let base = "CLASS app.Base:\nEND CLASS.\n";
    let names: Vec<String> = (0..1_100).map(|n| format!("app/B{n:04}.cls")).collect();
    let texts: Vec<String> = (0..1_100)
        .map(|n| format!("CLASS app.B{n:04} INHERITS app.Base:\nEND CLASS.\n"))
        .collect();
    let mut files: Vec<(&str, &str)> = vec![("oxabl.toml", CONFIG), ("app/Base.cls", base)];
    files.extend(
        names
            .iter()
            .map(String::as_str)
            .zip(texts.iter().map(String::as_str)),
    );
    let ws = Workspace::new(&files);
    let base_path = ws.path("app/Base.cls");
    let mut session = Session::start(&ws.root);
    session.open(&base_path, base);
    let item = prepared(&mut session, &base_path, base, "app.Base", 0);

    let answer = subtypes(&mut session, &item);
    let (marker, found) = without_marker(&answer.result);
    assert_eq!(found.len(), 1_000);
    let marker = marker.expect("a cut list must be marked");
    assert!(marker.contains("the list is cut"), "{marker}");

    let (found, warnings) = implementation(&mut session, &base_path, base, "app.Base", 0);
    assert_eq!(found.len(), 1_000);
    assert!(
        warnings.iter().any(|w| w.contains("the list is cut")),
        "{warnings:?}"
    );
}

// ---------------------------------------------------------------------------
// Cancel
// ---------------------------------------------------------------------------

#[test]
fn a_request_that_is_cancelled_stops_the_index_between_two_files() {
    let held = |name: &str| {
        format!(
            "/* OXABL-TEST-HOLD:daemon.hierarchy-index@1500 */\n\
             CLASS app.{name} INHERITS app.Base:\nEND CLASS.\n"
        )
    };
    let (h1, h2, h3) = (held("H1"), held("H2"), held("H3"));
    let base = "CLASS app.Base:\nEND CLASS.\n";
    let ws = small(&[
        ("app/Base.cls", base),
        ("app/H1.cls", &h1),
        ("app/H2.cls", &h2),
        ("app/H3.cls", &h3),
    ]);
    let base_path = ws.path("app/Base.cls");
    let mut session = Session::start(&ws.root);
    session.open(&base_path, base);
    let item = prepared(&mut session, &base_path, base, "app.Base", 0);
    // A references request waits for the workspace pass without reading the
    // held files, so the cancel below meets the index and not the pass.
    let mut params = position(&base_path, base, "app.Base", 0);
    params["context"] = json!({"includeDeclaration": true});
    ask(&mut session, "textDocument/references", params);

    let id = session.next_request_id();
    let started = std::time::Instant::now();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "typeHierarchy/subtypes", "params": {"item": item}
    }));
    std::thread::sleep(Duration::from_millis(800));
    session.send(json!({"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}}));
    let answer = session.wait_for(|message| message["id"] == json!(id));
    let elapsed = started.elapsed();
    assert_eq!(answer["error"]["code"], json!(-32800), "{answer}");
    // One file was held when the cancel came; the two others were not read.
    assert!(
        elapsed >= Duration::from_millis(1400) && elapsed < Duration::from_millis(3200),
        "{elapsed:?}"
    );
}

const NEW_ERR: &str = "DEFINE VARIABLE e AS Progress.Lang.Error NO-UNDO.\n\
    e = NEW Progress.Lang.AppError().\n\
    MESSAGE e.\n";

/// A workspace file named like a `Progress.*` type does not replace it: the
/// compiler binds the built-in type, and so do the analysis and the hierarchy.
const SHADOW_ERR: &str = "CLASS Progress.Lang.AppError:\nEND CLASS.\n";

#[test]
fn the_type_hierarchy_of_a_resolved_built_in_name_is_the_built_in_type() {
    let ws = small(&[
        ("use.p", NEW_ERR),
        ("Progress/Lang/AppError.cls", SHADOW_ERR),
    ]);
    let user = ws.path("use.p");
    let mut session = Session::start(&ws.root);
    session.open(&user, NEW_ERR);

    // A name after `NEW`, which the analysis resolves to the built-in type.
    let item = prepared(&mut session, &user, NEW_ERR, "Progress.Lang.AppError", 0);
    assert_eq!(item["name"], json!("Progress.Lang.AppError"));
    assert_eq!(item["uri"], json!(file_uri(&user)));
    let answer = supertypes(&mut session, &item);
    let (_, found) = without_marker(&answer.result);
    let names: Vec<&str> = found.iter().map(|n| n.name.as_str()).collect();
    assert_eq!(names, ["Progress.Lang.ProError"], "{found:?}");

    // A built-in interface in a declared type.
    let item = prepared(&mut session, &user, NEW_ERR, "Progress.Lang.Error", 0);
    assert_eq!(item["name"], json!("Progress.Lang.Error"));
    assert_eq!(item["kind"], json!(INTERFACE));
}
