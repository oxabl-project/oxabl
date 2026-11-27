//! Signature help and the parameter-name inlay hints through the real
//! `oxabl lsp`: a user function, a method of another class, an overloaded
//! method, a constructor, `RUN` of an internal and an external procedure, and
//! a built-in function. A call that oxabl does not resolve gets no signature.
//!
//! Each case is a small synthetic program with `‸` where the cursor is.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::PathBuf;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CURSOR: char = '‸';

const CONFIG: &str = "[workspace]\nname = \"signatures\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

const SCORER: &str = r#"CLASS app.Scorer:
    CONSTRUCTOR PUBLIC Scorer():
    END CONSTRUCTOR.
    CONSTRUCTOR PUBLIC Scorer(INPUT seed AS INTEGER):
    END CONSTRUCTOR.
    CONSTRUCTOR PRIVATE Scorer(INPUT seed AS INTEGER, INPUT label AS CHARACTER):
    END CONSTRUCTOR.
    METHOD PUBLIC INTEGER Total(INPUT x AS INTEGER):
        RETURN x.
    END METHOD.
    METHOD PUBLIC INTEGER Score(INPUT p AS INTEGER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC CHARACTER Score(INPUT p AS CHARACTER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC DECIMAL Score(INPUT p AS INTEGER, INPUT q AS INTEGER):
        RETURN p + q.
    END METHOD.
    METHOD PUBLIC VOID Fill(OUTPUT result AS CHARACTER):
        result = "x".
    END METHOD.
    METHOD PRIVATE VOID Hidden(INPUT z AS INTEGER):
    END METHOD.
    METHOD PRIVATE VOID Mixed(INPUT z AS INTEGER):
    END METHOD.
    METHOD PUBLIC VOID Mixed(INPUT z AS CHARACTER):
    END METHOD.
END CLASS.
"#;

const GRAND: &str = r#"CLASS app.Grand:
    METHOD PUBLIC CHARACTER Name(INPUT n AS INTEGER):
        RETURN "".
    END METHOD.
    METHOD PUBLIC CHARACTER Name(INPUT n AS CHARACTER):
        RETURN n.
    END METHOD.
END CLASS.
"#;

const CHILD: &str = r#"CLASS app.Child INHERITS app.Grand:
    METHOD PUBLIC CHARACTER Name(INPUT n AS DATE):
        RETURN "".
    END METHOD.
END CLASS.
"#;

const PARTS: &str = "    METHOD PUBLIC VOID Extra(INPUT e AS INTEGER):\n    END METHOD.\n";

const OPEN_SET: &str = r#"CLASS app.OpenSet:
{app/parts.i}
    METHOD PUBLIC VOID Known(INPUT k AS INTEGER):
    END METHOD.
END CLASS.
"#;

const PROG: &str = r#"DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pnCount AS INTEGER NO-UNDO.
pnCount = LENGTH(pcName).
"#;

/// The head of each case program: a function, a procedure and variables.
const PRELUDE: &str = r#"DEFINE VARIABLE iResult AS INTEGER NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE VARIABLE oScorer AS app.Scorer NO-UNDO.
DEFINE VARIABLE oChild AS app.Child NO-UNDO.
DEFINE VARIABLE oOpen AS app.OpenSet NO-UNDO.
DEFINE VARIABLE oUnknown AS nowhere.Thing NO-UNDO.
FUNCTION Calc RETURNS INTEGER (INPUT a AS INTEGER, INPUT b AS CHARACTER):
    RETURN a.
END FUNCTION.
PROCEDURE DoWork:
    DEFINE INPUT PARAMETER piCount AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcName AS CHARACTER NO-UNDO.
    pcName = STRING(piCount).
END PROCEDURE.
"#;

struct Workspace {
    root: tempfile::TempDir,
    cases: usize,
}

impl Workspace {
    fn new() -> Workspace {
        let root = tempfile::tempdir().unwrap();
        let ws = Workspace { root, cases: 0 };
        ws.write("oxabl.toml", CONFIG);
        ws.write("app/Scorer.cls", SCORER);
        ws.write("app/Grand.cls", GRAND);
        ws.write("app/Child.cls", CHILD);
        ws.write("app/OpenSet.cls", OPEN_SET);
        ws.write("app/parts.i", PARTS);
        ws.write("prog.p", PROG);
        ws.write(
            "app/BadCtor.cls",
            "CLASS app.BadCtor:\n    CONSTRUCTOR PUBLIC BadCtor(INPUT a AS INTEGER):\n\
             \x20   END CONSTRUCTOR.\n    METHOD PUBLIC oops oops oops\nEND CLASS.\n",
        );
        ws.write(
            "app/IncCtor.cls",
            "CLASS app.IncCtor:\n    CONSTRUCTOR PUBLIC IncCtor():\n    END CONSTRUCTOR.\n\
             {app/ctor.i}\nEND CLASS.\n",
        );
        ws.write(
            "app/ctor.i",
            "    CONSTRUCTOR PUBLIC IncCtor(INPUT a AS INTEGER):\n    END CONSTRUCTOR.\n",
        );
        ws.write(
            "incparams.p",
            "{app/ctor.i}\nDEFINE INPUT PARAMETER x AS INTEGER NO-UNDO.\n",
        );
        ws
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.path().join(name)
    }

    fn write(&self, name: &str, text: &str) {
        let path = self.path(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
}

fn position(text: &str, at: usize) -> Value {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    json!({
        "line": text[..at].matches('\n').count(),
        "character": text[line_start..at].encode_utf16().count(),
    })
}

/// A file of `text` with the cursor where `‸` is, opened in the session.
struct Case {
    file: PathBuf,
    text: String,
    at: usize,
}

impl Workspace {
    /// Write `marked` as an open `.p` file, with `PRELUDE` before it.
    fn program(&mut self, session: &mut Session, marked: &str) -> Case {
        self.cases += 1;
        let name = format!("case{}.p", self.cases);
        self.open(session, &name, &format!("{PRELUDE}{marked}"))
    }

    fn open(&mut self, session: &mut Session, name: &str, marked: &str) -> Case {
        let at = marked.find(CURSOR).expect("a cursor in the case");
        let text = marked.replacen(CURSOR, "", 1);
        self.write(name, &text);
        let file = self.path(name);
        session.open(&file, &text);
        Case { file, text, at }
    }
}

/// The answer to `textDocument/signatureHelp` at the cursor of `case`.
#[track_caller]
fn help(session: &mut Session, case: &Case) -> Value {
    let answer = session.request(
        "textDocument/signatureHelp",
        json!({
            "textDocument": {"uri": file_uri(&case.file)},
            "position": position(&case.text, case.at),
        }),
    );
    assert!(
        answer.get("error").is_none(),
        "signatureHelp gave an error: {}",
        answer["error"]
    );
    answer["result"].clone()
}

fn labels(help: &Value) -> Vec<String> {
    help["signatures"]
        .as_array()
        .expect("signatures")
        .iter()
        .map(|s| s["label"].as_str().unwrap().to_string())
        .collect()
}

/// The text of parameter `at` of signature `signature`.
fn parameter_text(help: &Value, signature: usize, at: usize) -> String {
    let sig = &help["signatures"][signature];
    let label = sig["label"].as_str().unwrap();
    let range = &sig["parameters"][at]["label"];
    let (start, end) = (
        range[0].as_u64().unwrap() as usize,
        range[1].as_u64().unwrap() as usize,
    );
    label[start..end].to_string()
}

fn session_for(ws: &Workspace) -> Session {
    Session::start(ws.root.path())
}

// ---------------------------------------------------------------------------
// A user function, and the active parameter
// ---------------------------------------------------------------------------

#[test]
fn a_user_function_shows_its_parameters_and_the_active_one() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "iResult = Calc(1, ‸\"x\").\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Calc(INPUT a AS INTEGER, INPUT b AS CHARACTER): INTEGER"]
    );
    assert_eq!(answer["activeSignature"], 0, "{answer}");
    assert_eq!(answer["signatures"][0]["activeParameter"], 1, "{answer}");
    assert_eq!(parameter_text(&answer, 0, 1), "INPUT b AS CHARACTER");

    let first = ws.program(&mut session, "iResult = Calc(‸1, \"x\").\n");
    let answer = help(&mut session, &first);
    assert_eq!(answer["signatures"][0]["activeParameter"], 0, "{answer}");
    assert_eq!(parameter_text(&answer, 0, 0), "INPUT a AS INTEGER");
}

#[test]
fn the_active_parameter_ignores_commas_in_strings_and_nested_calls() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    // (marked statement, the parameter that is active in `Calc`)
    let cases = [
        // A comma in a string argument.
        ("iResult = Calc(1, \"a,b,‸c\").\n", 1),
        ("iResult = Calc(1, \"a,b\"‸).\n", 1),
        ("iResult = Calc(‸\"a,b\", \"c\").\n", 0),
        // After a nested call with commas of its own.
        ("iResult = Calc(Calc(1, \"x\"), ‸\"y\").\n", 1),
        ("iResult = Calc(Calc(1, \"x\")‸, \"y\").\n", 0),
        // A nested parenthesis that is no call.
        ("iResult = Calc((1 + 2) * 3, ‸\"y\").\n", 1),
        ("iResult = Calc((1 + ‸2) * 3, \"y\").\n", 0),
    ];
    for (marked, wanted) in cases {
        let case = ws.program(&mut session, marked);
        let answer = help(&mut session, &case);
        assert_eq!(
            answer["signatures"][0]["activeParameter"], wanted,
            "{marked}: {answer}"
        );
        assert!(
            labels(&answer)[0].starts_with("Calc("),
            "{marked}: {answer}"
        );
    }
}

#[test]
fn a_nested_call_shows_the_signature_of_the_innermost_call() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "iResult = Calc(Calc(1, ‸\"x\"), \"y\").\n");
    let answer = help(&mut session, &case);
    assert_eq!(answer["signatures"][0]["activeParameter"], 1, "{answer}");
    let outer = ws.program(&mut session, "iResult = Calc(Calc(1, \"x\"), ‸\"y\").\n");
    assert_eq!(
        help(&mut session, &outer)["signatures"][0]["activeParameter"],
        1
    );
}

// ---------------------------------------------------------------------------
// A method of another class, overloads
// ---------------------------------------------------------------------------

#[test]
fn a_method_of_another_class_shows_its_signature() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "iResult = oScorer:Total(‸1).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Total(INPUT x AS INTEGER): INTEGER"],
        "{answer}"
    );
    assert_eq!(answer["activeSignature"], 0);
    assert_eq!(answer["signatures"][0]["activeParameter"], 0);
    assert_eq!(
        answer["signatures"][0]["documentation"]["value"],
        "Declared in app.Scorer."
    );
}

#[test]
fn an_overloaded_method_shows_every_overload_and_the_one_the_arguments_pick_first() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let all = [
        "Score(INPUT p AS INTEGER): INTEGER",
        "Score(INPUT p AS CHARACTER): CHARACTER",
        "Score(INPUT p AS INTEGER, INPUT q AS INTEGER): DECIMAL",
    ];

    // An integer picks the first overload.
    let case = ws.program(&mut session, "iResult = oScorer:Score(1‸).\n");
    let answer = help(&mut session, &case);
    assert_eq!(labels(&answer), all, "{answer}");
    assert_eq!(answer["activeSignature"], 0, "{answer}");

    // A string picks the second: it comes first, the others keep their order.
    let case = ws.program(&mut session, "cName = oScorer:Score(\"a\"‸).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        [all[1], all[0], all[2]],
        "the picked overload is first: {answer}"
    );
    assert_eq!(answer["activeSignature"], 0);
    assert_eq!(answer["signatures"][0]["activeParameter"], 0);

    // Two arguments pick the third.
    let case = ws.program(&mut session, "iResult = oScorer:Score(1, ‸2).\n");
    let answer = help(&mut session, &case);
    assert_eq!(labels(&answer)[0], all[2], "{answer}");
    assert_eq!(answer["activeSignature"], 0);
    assert_eq!(answer["signatures"][0]["activeParameter"], 1);
}

#[test]
fn arguments_that_pick_no_overload_give_every_candidate_and_no_active_one() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for marked in [
        // No argument yet.
        "iResult = oScorer:Score(‸).\n",
        // An unknown value fits both one-parameter overloads.
        "iResult = oScorer:Score(?‸).\n",
        // The second argument is still to come.
        "iResult = oScorer:Score(1, ‸).\n",
    ] {
        let case = ws.program(&mut session, marked);
        let answer = help(&mut session, &case);
        assert_eq!(labels(&answer).len(), 3, "{marked}: {answer}");
        assert!(
            answer.get("activeSignature").is_none() || answer["activeSignature"].is_null(),
            "{marked}: no signature is active: {answer}"
        );
    }
}

#[test]
fn a_private_overload_is_not_shown_beside_a_public_one() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oScorer:Mixed(‸\"a\").\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Mixed(INPUT z AS CHARACTER): VOID"],
        "{answer}"
    );
}

#[test]
fn an_inherited_overload_set_lists_the_declarations_of_every_level() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "cName = oChild:Name(‸).\n");
    let answer = help(&mut session, &case);
    let mut shown = labels(&answer);
    shown.sort();
    assert_eq!(
        shown,
        [
            "Name(INPUT n AS CHARACTER): CHARACTER",
            "Name(INPUT n AS DATE): CHARACTER",
            "Name(INPUT n AS INTEGER): CHARACTER",
        ],
        "{answer}"
    );
}

#[test]
fn the_own_and_the_inherited_overloads_of_a_method_are_listed_together() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.open(
        &mut session,
        "app/Sub2.cls",
        r#"CLASS app.Sub2 INHERITS app.Scorer:
    METHOD PUBLIC DATE Score(INPUT p AS DATE):
        RETURN p.
    END METHOD.
    METHOD PUBLIC VOID Go():
        DEFINE VARIABLE n AS INTEGER NO-UNDO.
        n = Score(1‸).
    END METHOD.
END CLASS.
"#,
    );
    let answer = help(&mut session, &case);
    let shown = labels(&answer);
    assert_eq!(shown.len(), 4, "{answer}");
    assert_eq!(
        shown[0], "Score(INPUT p AS INTEGER): INTEGER",
        "the picked one first"
    );
    assert!(
        shown.contains(&"Score(INPUT p AS DATE): DATE".to_string()),
        "{answer}"
    );
    assert_eq!(answer["activeSignature"], 0);
}

#[test]
fn a_method_of_the_unit_s_own_class_picks_among_its_overloads() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.open(
        &mut session,
        "app/Self.cls",
        r#"CLASS app.Self:
    METHOD PUBLIC INTEGER Twice(INPUT a AS INTEGER):
        RETURN a.
    END METHOD.
    METHOD PUBLIC CHARACTER Twice(INPUT a AS CHARACTER):
        RETURN a.
    END METHOD.
    METHOD PUBLIC VOID Go():
        DEFINE VARIABLE c AS CHARACTER NO-UNDO.
        c = Twice(c‸).
    END METHOD.
END CLASS.
"#,
    );
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        [
            "Twice(INPUT a AS CHARACTER): CHARACTER",
            "Twice(INPUT a AS INTEGER): INTEGER"
        ],
        "{answer}"
    );
    assert_eq!(answer["activeSignature"], 0);
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

#[test]
fn new_shows_the_constructors_the_caller_can_reach() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oScorer = NEW app.Scorer(‸).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Scorer()", "Scorer(INPUT seed AS INTEGER)"],
        "the private constructor is not shown: {answer}"
    );
    let case = ws.program(&mut session, "oScorer = NEW app.Scorer(1‸).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        answer["signatures"][0]["label"],
        "Scorer(INPUT seed AS INTEGER)"
    );
    assert_eq!(answer["activeSignature"], 0);
}

#[test]
fn super_and_this_object_show_the_constructors_they_call() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let class = |name: &str, body: &str| {
        format!(
            "CLASS app.{name} INHERITS app.Scorer:\n\
             CONSTRUCTOR PUBLIC {name}():\n{body}\nEND CONSTRUCTOR.\n\
             CONSTRUCTOR PUBLIC {name}(INPUT n AS INTEGER):\nEND CONSTRUCTOR.\n\
             END CLASS.\n"
        )
    };
    // The parent's private constructor is not reachable from a subclass.
    let case = ws.open(&mut session, "app/KidA.cls", &class("KidA", "SUPER(‸)."));
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Scorer()", "Scorer(INPUT seed AS INTEGER)"],
        "{answer}"
    );
    // Its own constructors all are.
    let case = ws.open(
        &mut session,
        "app/KidB.cls",
        &class("KidB", "THIS-OBJECT(‸)."),
    );
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["KidB()", "KidB(INPUT n AS INTEGER)"],
        "{answer}"
    );
}

// ---------------------------------------------------------------------------
// RUN
// ---------------------------------------------------------------------------

#[test]
fn run_of_an_internal_procedure_shows_its_parameters() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "RUN DoWork (INPUT 1, OUTPUT ‸cName).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["DoWork(INPUT piCount AS INTEGER, OUTPUT pcName AS CHARACTER)"],
        "{answer}"
    );
    assert_eq!(answer["signatures"][0]["activeParameter"], 1);
    assert_eq!(parameter_text(&answer, 0, 1), "OUTPUT pcName AS CHARACTER");
}

#[test]
fn run_of_an_external_procedure_shows_the_parameters_of_its_program() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "RUN prog.p (INPUT \"x\", ‸OUTPUT iResult).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["prog.p(INPUT pcName AS CHARACTER, OUTPUT pnCount AS INTEGER)"],
        "{answer}"
    );
    assert_eq!(answer["signatures"][0]["activeParameter"], 1);
}

// ---------------------------------------------------------------------------
// Built-in functions
// ---------------------------------------------------------------------------

#[test]
fn a_built_in_function_shows_its_syntax() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "iResult = SUBSTRING(cName, 1, ‸2).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["SUBSTRING(string, position [, length [, type]])"],
        "{answer}"
    );
    assert_eq!(answer["signatures"][0]["activeParameter"], 2);
    assert_eq!(parameter_text(&answer, 0, 2), "length");
    assert!(
        answer["signatures"][0]["documentation"]["value"]
            .as_str()
            .unwrap()
            .contains("SUBSTRING function")
    );

    // A function with a repeated parameter keeps the last one active.
    let case = ws.program(&mut session, "cName = SUBSTITUTE(\"&1&2&3\", 1, 2, ‸3).\n");
    let answer = help(&mut session, &case);
    assert_eq!(answer["signatures"][0]["activeParameter"], 1, "{answer}");
}

#[test]
fn a_built_in_function_with_two_syntaxes_shows_both_and_no_active_one() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "iResult = DATE(‸1, 2, 2026).\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["DATE(month, day, year)", "DATE(expression)"]
    );
    assert!(
        answer.get("activeSignature").is_none() || answer["activeSignature"].is_null(),
        "{answer}"
    );
}

#[test]
fn a_user_function_hides_a_built_in_of_the_same_name() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.open(
        &mut session,
        "shadow.p",
        r#"FUNCTION SUBSTRING RETURNS INTEGER (INPUT only AS INTEGER):
    RETURN only.
END FUNCTION.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
i = SUBSTRING(‸1).
"#,
    );
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["SUBSTRING(INPUT only AS INTEGER): INTEGER"]
    );
}

#[test]
fn an_include_read_alone_may_hide_a_function_that_a_built_in_of_the_name_would_not() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    // The includer may declare a function `LENGTH`: no reserved word forbids it.
    let case = ws.open(&mut session, "frag1.i", "i = LENGTH(‸cName).\n");
    assert_eq!(help(&mut session, &case), Value::Null);
    // `LOOKUP` is reserved: no routine can have its name.
    let case = ws.open(&mut session, "frag2.i", "i = LOOKUP(‸\"a\", \"a,b\").\n");
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["LOOKUP(expression, list [, delimiter])"]
    );
}

#[test]
fn a_declaration_that_oxabl_may_not_read_whole_gives_no_signature() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for marked in [
        // A class file that does not parse.
        "oScorer = NEW app.BadCtor(‸).\n",
        // A constructor that an include may declare.
        "oScorer = NEW app.IncCtor(‸).\n",
        // A program whose parameters an include may define.
        "RUN incparams.p (INPUT ‸1).\n",
    ] {
        let case = ws.program(&mut session, marked);
        assert_eq!(help(&mut session, &case), Value::Null, "{marked}");
    }
}

// ---------------------------------------------------------------------------
// A call that oxabl cannot resolve has no signature
// ---------------------------------------------------------------------------

#[test]
fn a_call_oxabl_cannot_resolve_has_no_signature() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for marked in [
        // A function nobody declares.
        "iResult = Nobody(‸1).\n",
        // A method of a receiver of an unknown class.
        "iResult = oUnknown:Anything(‸1).\n",
        // A method the class does not have.
        "iResult = oScorer:Missing(‸1).\n",
        // A private method of another class.
        "oScorer:Hidden(‸1).\n",
        // A program nobody supplies.
        "RUN absent.p (INPUT ‸1).\n",
        // A computed target.
        "RUN VALUE(cName) (INPUT ‸1).\n",
        // A constructor of a class nobody declares.
        "oUnknown = NEW nowhere.Thing(‸).\n",
        // A built-in function that the table does not hold.
        "iResult = ENCODE(‸cName).\n",
        // The members of a built-in class are not read yet.
        "DEFINE VARIABLE oError AS Progress.Lang.AppError NO-UNDO.\noError:AddMessage(‸\"x\", 1).\n",
        // The cursor is in no call.
        "iResult = Calc(1, \"x\")‸.\n",
        // Parentheses that are no call.
        "iResult = (1 + ‸2).\n",
    ] {
        let case = ws.program(&mut session, marked);
        assert_eq!(
            help(&mut session, &case),
            Value::Null,
            "no signature for: {marked}"
        );
    }
}

#[test]
fn a_class_whose_members_oxabl_may_not_all_know_has_no_signature() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    // `app/parts.i` is read, so the set of `OpenSet` is whole.
    let case = ws.program(&mut session, "oOpen:Known(‸1).\n");
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Known(INPUT k AS INTEGER): VOID"]
    );

    // Remove the include: a member may hide in it.
    std::fs::remove_file(ws.path("app/parts.i")).unwrap();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oOpen:Known(‸1).\n");
    assert_eq!(help(&mut session, &case), Value::Null);
}

// ---------------------------------------------------------------------------
// A call that is still being typed
// ---------------------------------------------------------------------------

#[test]
fn a_call_that_is_still_being_typed_has_a_signature() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    // (the text, the label the signature starts with, the active parameter)
    let cases = [
        // The editor closed the parenthesis; the statement has no period yet,
        // so the next statement would swallow it.
        ("iResult = Calc(1, ‸)\nMESSAGE \"x\".\n", "Calc(", 1),
        // The parenthesis is not closed, at the end of the file or before
        // another statement, also in a nested call.
        ("iResult = Calc(1, ‸", "Calc(", 1),
        ("iResult = Calc(1, ‸\nMESSAGE \"x\".\n", "Calc(", 1),
        ("iResult = Calc(Calc(1, ‸\nMESSAGE \"x\".\n", "Calc(", 1),
        // An empty argument in a `RUN`.
        ("RUN DoWork (INPUT 1, ‸)\nMESSAGE \"x\".\n", "DoWork(", 1),
        ("RUN DoWork (INPUT 1, ‸\nMESSAGE \"x\".\n", "DoWork(", 1),
        ("iResult = oScorer:Total(‸\n", "Total(", 0),
    ];
    for (marked, label, active) in cases {
        let case = ws.program(&mut session, marked);
        let answer = help(&mut session, &case);
        assert!(
            answer["signatures"][0]["label"]
                .as_str()
                .is_some_and(|l| l.starts_with(label)),
            "{marked:?}: {answer}"
        );
        assert_eq!(
            answer["signatures"][0]["activeParameter"], active,
            "{marked:?}"
        );
    }
    // Closing the call never makes a name resolve.
    let case = ws.program(&mut session, "iResult = Nobody(‸\nMESSAGE \"x\".\n");
    assert_eq!(help(&mut session, &case), Value::Null);
}

#[test]
fn a_run_through_a_handle_shows_the_procedure_the_handle_runs() {
    let mut ws = Workspace::new();
    ws.write(
        "lib.p",
        "PROCEDURE DoLib:\n    DEFINE INPUT PARAMETER pcKey AS CHARACTER NO-UNDO.\n\
         END PROCEDURE.\n",
    );
    let mut session = session_for(&ws);
    let case = ws.open(
        &mut session,
        "handle.p",
        "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\nRUN lib.p PERSISTENT SET hLib.\n\
         RUN DoLib IN hLib (INPUT ‸\"k\").\n",
    );
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["DoLib(INPUT pcKey AS CHARACTER)"],
        "{answer}"
    );

    // A handle that nothing is known about runs no known procedure.
    let case = ws.open(
        &mut session,
        "handle2.p",
        "DEFINE INPUT PARAMETER hAny AS HANDLE NO-UNDO.\nRUN DoLib IN hAny (INPUT ‸\"k\").\n",
    );
    assert_eq!(help(&mut session, &case), Value::Null);
}

#[test]
fn an_edit_of_an_open_class_reaches_the_signature_and_the_hints() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.program(&mut session, "iResult = oScorer:Total(‸5).\n");
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Total(INPUT x AS INTEGER): INTEGER"]
    );
    let hinted = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(
        hinted.iter().map(|h| h.0.as_str()).collect::<Vec<_>>(),
        ["x:"]
    );

    // The class is open and unsaved: its text, not the disk's, is the truth.
    let class = ws.path("app/Scorer.cls");
    let edited = SCORER.replace(
        "Total(INPUT x AS INTEGER)",
        "Total(INPUT amount AS INTEGER)",
    );
    session.open(&class, &edited);
    let mut labels_now = Vec::new();
    for _ in 0..50 {
        labels_now = labels(&help(&mut session, &case));
        if labels_now[0].contains("amount") {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    assert_eq!(labels_now, ["Total(INPUT amount AS INTEGER): INTEGER"]);
    let mut names = Vec::new();
    for _ in 0..50 {
        names = hints(&mut session, &case, 0, case.text.len());
        if names.first().is_some_and(|h| h.0 == "amount:") {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    assert_eq!(
        names.iter().map(|h| h.0.as_str()).collect::<Vec<_>>(),
        ["amount:"]
    );
}

// ---------------------------------------------------------------------------
// Parameter-name hints
// ---------------------------------------------------------------------------

fn hints(session: &mut Session, case: &Case, from: usize, to: usize) -> Vec<(String, String)> {
    let answer = session.request(
        "textDocument/inlayHint",
        json!({
            "textDocument": {"uri": file_uri(&case.file)},
            "range": {"start": position(&case.text, from), "end": position(&case.text, to)},
        }),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let text = &case.text;
    answer["result"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|hint| hint["kind"] == 2)
        .map(|hint| {
            let line = hint["position"]["line"].as_u64().unwrap() as usize;
            let column = hint["position"]["character"].as_u64().unwrap() as usize;
            let source_line = text.lines().nth(line).unwrap();
            let at: usize = source_line.chars().take(column).map(char::len_utf8).sum();
            (
                hint["label"].as_str().unwrap().to_string(),
                source_line[at..].to_string(),
            )
        })
        .collect()
}

fn capabilities() -> Value {
    json!({"textDocument": {"inlayHint": {}, "signatureHelp": {}}})
}

/// A session with the parameter-name hints on, as the client turns them on;
/// `options` replaces the initialization options when it is not null.
fn hint_session(ws: &Workspace, options: Value) -> Session {
    let options = if options.is_null() {
        json!({"inlayHints": {"parameters": true}})
    } else {
        options
    };
    Session::start_with_options(ws.root.path(), capabilities(), options)
}

#[test]
fn a_call_with_one_known_target_gets_parameter_name_hints() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let marked = "iResult = Calc(1, \"x\").\n\
                  iResult = oScorer:Total(a).\n\
                  iResult = oScorer:Total(2).\n\
                  RUN DoWork (INPUT 1, OUTPUT cName).\n\
                  RUN prog.p (INPUT \"x\", OUTPUT iResult).\n\
                  oScorer = NEW app.Scorer(7).‸\n";
    let case = ws.program(&mut session, marked);
    let found = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(
        found,
        [
            ("a:", "1, \"x\").".to_string()),
            ("b:", "\"x\").".to_string()),
            ("x:", "a).".to_string()),
            ("x:", "2).".to_string()),
            ("piCount:", "INPUT 1, OUTPUT cName).".to_string()),
            ("pcName:", "INPUT \"x\", OUTPUT iResult).".to_string()),
            ("pnCount:", "OUTPUT iResult).".to_string()),
            ("seed:", "7).".to_string()),
        ]
        .map(|(label, rest)| (label.to_string(), rest)),
        "the hints of the program"
    );
}

#[test]
fn an_argument_that_is_the_parameter_name_gets_no_hint() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.program(
        &mut session,
        "DEFINE VARIABLE b AS CHARACTER NO-UNDO.\niResult = Calc(a, b).‸\n",
    );
    assert_eq!(hints(&mut session, &case, 0, case.text.len()), []);
}

#[test]
fn a_call_with_several_possible_targets_gets_no_hint() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.program(
        &mut session,
        "iResult = oScorer:Score(?).\niResult = oScorer:Score(1).\niResult = oScorer:Score(\"a\").‸\n",
    );
    let found = hints(&mut session, &case, 0, case.text.len());
    // The unknown value fits two overloads: no hint. The others pick one.
    assert_eq!(
        found
            .iter()
            .map(|(label, rest)| (label.as_str(), rest.as_str()))
            .collect::<Vec<_>>(),
        [("p:", "1)."), ("p:", "\"a\").")],
        "{found:?}"
    );
}

#[test]
fn a_call_oxabl_cannot_resolve_gets_no_hint() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.program(
        &mut session,
        "iResult = Nobody(1).\niResult = oUnknown:Anything(1).\niResult = LENGTH(cName).\nRUN absent.p (INPUT 1).‸\n",
    );
    assert_eq!(hints(&mut session, &case, 0, case.text.len()), []);
}

#[test]
fn a_parameter_the_call_does_not_write_as_output_gets_its_mode_in_the_hint() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.program(
        &mut session,
        "DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.\nRUN DoWork (1, cResult).‸\n",
    );
    let found = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(
        found,
        [
            ("piCount:".to_string(), "1, cResult).".to_string()),
            ("OUTPUT pcName:".to_string(), "cResult).".to_string()),
        ]
    );
}

#[test]
fn the_hints_follow_the_range_of_the_request() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let marked = "iResult = Calc(1, \"x\").\niResult = oScorer:Total(2).‸\n";
    let case = ws.program(&mut session, marked);
    let first = case.text.find("iResult = Calc").unwrap();
    let second = case.text.find("iResult = oScorer").unwrap();
    let only_first = hints(&mut session, &case, first, second - 1);
    assert_eq!(only_first.len(), 2, "{only_first:?}");
    let only_second = hints(&mut session, &case, second, case.text.len());
    assert_eq!(only_second.len(), 1, "{only_second:?}");
    assert_eq!(only_second[0].0, "x:");
}

#[test]
fn the_setting_turns_the_parameter_hints_off_and_on() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, json!({"inlayHints": {"parameters": false}}));
    let case = ws.program(&mut session, "iResult = Calc(1, \"x\").‸\n");
    assert_eq!(hints(&mut session, &case, 0, case.text.len()), []);

    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"inlayHints": {"parameters": true}}}}
    }));
    let found = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(found.len(), 2, "{found:?}");

    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"inlayHints": {"parameters": false}}}}
    }));
    assert_eq!(hints(&mut session, &case, 0, case.text.len()), []);
}

// ---------------------------------------------------------------------------
// A declaration that the text of its file does not show whole
// ---------------------------------------------------------------------------

#[test]
fn a_procedure_with_an_include_that_declares_no_parameter_gives_no_signature_and_no_hint() {
    let mut ws = Workspace::new();
    ws.write(
        "lib_inc.p",
        "PROCEDURE hp:\n    {hp_params.i}\n    DEFINE INPUT PARAMETER z AS INTEGER NO-UNDO.\n\
         END PROCEDURE.\n",
    );
    ws.write(
        "hp_params.i",
        "DEFINE VARIABLE cLocal AS CHARACTER NO-UNDO.\n",
    );
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.open(
        &mut session,
        "handle_inc.p",
        "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\nDEFINE VARIABLE iOut AS INTEGER NO-UNDO.\n\
         RUN lib_inc.p PERSISTENT SET hLib.\nRUN hp IN hLib (\"a\", OUTPUT iOut‸, 3).\n",
    );
    assert_eq!(help(&mut session, &case), Value::Null);
    assert_eq!(hints(&mut session, &case, 0, case.text.len()), []);
}

/// A `{&name}` reference may expand to a declaration, as an include does.
fn reference_workspace() -> Workspace {
    let ws = Workspace::new();
    ws.write(
        "ref_params.p",
        "&SCOPED-DEFINE P DEFINE INPUT PARAMETER a AS CHARACTER NO-UNDO.\n{&P}\n\
         DEFINE INPUT PARAMETER z AS INTEGER NO-UNDO.\n",
    );
    ws.write(
        "ref_lib.p",
        "&SCOPED-DEFINE P DEFINE INPUT PARAMETER a AS CHARACTER NO-UNDO.\n\
         PROCEDURE hp:\n    {&P}\n    DEFINE INPUT PARAMETER z AS INTEGER NO-UNDO.\n\
         END PROCEDURE.\n",
    );
    ws.write(
        "app/RefMethod.cls",
        "&SCOPED-DEFINE G METHOD PUBLIC VOID Go(INPUT a AS CHARACTER): END METHOD.\n\
         CLASS app.RefMethod:\n{&G}\n\
         \x20   METHOD PUBLIC VOID Go(INPUT z AS INTEGER):\n    END METHOD.\nEND CLASS.\n",
    );
    ws.write(
        "app/RefCtor.cls",
        "&SCOPED-DEFINE C CONSTRUCTOR PUBLIC RefCtor(INPUT a AS CHARACTER): END CONSTRUCTOR.\n\
         CLASS app.RefCtor:\n{&C}\n\
         \x20   CONSTRUCTOR PUBLIC RefCtor(INPUT z AS INTEGER):\n    END CONSTRUCTOR.\nEND CLASS.\n",
    );
    // A reference inside a method, accessor or destructor body declares
    // nothing.
    ws.write(
        "app/RefBody.cls",
        "CLASS app.RefBody:\n    METHOD PUBLIC VOID Go(INPUT z AS INTEGER):\n\
         \x20       MESSAGE {&OPSYS}.\n    END METHOD.\n\
         \x20   DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO\n\
         \x20       GET():\n            MESSAGE {&OPSYS}.\n            RETURN \"\".\n        END GET.\n\
         \x20       SET(INPUT v AS CHARACTER):\n            MESSAGE {&OPSYS}.\n        END SET.\n\
         \x20   DESTRUCTOR PUBLIC RefBody():\n        MESSAGE {&OPSYS}.\n    END DESTRUCTOR.\n\
         END CLASS.\n",
    );
    ws.write(
        "c_proc.p",
        "DEFINE INPUT PARAMETER z AS INTEGER NO-UNDO.\nPROCEDURE other:\n\
         \x20   MESSAGE {&OPSYS}.\nEND PROCEDURE.\n",
    );
    ws
}

#[test]
fn a_preprocessor_reference_that_may_declare_gives_no_signature_and_no_hint() {
    let mut ws = reference_workspace();
    let mut session = hint_session(&ws, Value::Null);
    for marked in [
        "RUN ref_params.p (\"a\", ‸3).\n",
        "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\nRUN ref_lib.p PERSISTENT SET hLib.\n\
         RUN hp IN hLib (\"a\", ‸3).\n",
        "DEFINE VARIABLE o AS app.RefMethod NO-UNDO.\no = NEW app.RefMethod().\no:Go(‸\"a\").\n",
        "DEFINE VARIABLE o AS app.RefCtor NO-UNDO.\no = NEW app.RefCtor(‸\"a\").\n",
    ] {
        let case = ws.program(&mut session, marked);
        assert_eq!(help(&mut session, &case), Value::Null, "{marked}");
        assert_eq!(
            hints(&mut session, &case, 0, case.text.len()),
            [],
            "{marked}"
        );
    }
    // A reference in a procedure of a program does not hide its parameters.
    let case = ws.program(&mut session, "RUN c_proc.p (‸1).\n");
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["c_proc.p(INPUT z AS INTEGER)"]
    );
    // A reference inside a method body, a property accessor or a destructor
    // does not hide the method.
    let case = ws.program(
        &mut session,
        "DEFINE VARIABLE o AS app.RefBody NO-UNDO.\no = NEW app.RefBody().\no:Go(‸1).\n",
    );
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Go(INPUT z AS INTEGER): VOID"]
    );
}

const FORWARD: &str = "\
FUNCTION Fw RETURNS INTEGER (INPUT x AS INTEGER, INPUT y AS CHARACTER) FORWARD.
FUNCTION Zero RETURNS INTEGER () FORWARD.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
";

const FORWARD_BODIES: &str = "\
FUNCTION Fw RETURNS INTEGER:
    RETURN x + LENGTH(y).
END FUNCTION.
FUNCTION Zero RETURNS INTEGER:
    RETURN 0.
END FUNCTION.
";

#[test]
fn a_definition_after_a_forward_takes_its_parameters_from_the_prototype() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let fw = "Fw(INPUT x AS INTEGER, INPUT y AS CHARACTER): INTEGER";
    for (name, call) in [
        // The call is between the prototype and the definition.
        (
            "fwd_before.p",
            format!("{FORWARD}i = Fw(1, ‸\"abc\").\n{FORWARD_BODIES}"),
        ),
        (
            "fwd_after.p",
            format!("{FORWARD}{FORWARD_BODIES}i = Fw(1, ‸\"abc\").\n"),
        ),
    ] {
        let case = ws.open(&mut session, name, &call);
        let answer = help(&mut session, &case);
        assert_eq!(labels(&answer), [fw], "{name}: {answer}");
        assert_eq!(answer["signatures"][0]["activeParameter"], 1, "{name}");
        let found = hints(&mut session, &case, 0, case.text.len());
        assert_eq!(
            found.iter().map(|h| h.0.as_str()).collect::<Vec<_>>(),
            ["x:", "y:"],
            "{name}"
        );
    }
    // A prototype with no parameters says that the function has none.
    let case = ws.open(
        &mut session,
        "fwd_zero.p",
        &format!("{FORWARD}{FORWARD_BODIES}i = Zero(‸).\n"),
    );
    assert_eq!(labels(&help(&mut session, &case)), ["Zero(): INTEGER"]);
}

// ---------------------------------------------------------------------------
// Package access of a constructor
// ---------------------------------------------------------------------------

const PACKAGED: &str = "\
CLASS app.Pk:
    CONSTRUCTOR PUBLIC Pk():
    END CONSTRUCTOR.
    CONSTRUCTOR PACKAGE-PRIVATE Pk(INPUT n AS INTEGER):
    END CONSTRUCTOR.
    CONSTRUCTOR PACKAGE-PROTECTED Pk(INPUT n AS INTEGER, INPUT m AS INTEGER):
    END CONSTRUCTOR.
END CLASS.
";

#[test]
fn a_package_constructor_is_shown_to_the_classes_of_its_package_only() {
    let mut ws = Workspace::new();
    ws.write("app/Pk.cls", PACKAGED);
    ws.write(
        "bare/Bare.cls",
        "CLASS Bare:\n    CONSTRUCTOR PUBLIC Bare():\n    END CONSTRUCTOR.\n\
         \x20   CONSTRUCTOR PACKAGE-PRIVATE Bare(INPUT n AS INTEGER):\n    END CONSTRUCTOR.\n\
         END CLASS.\n",
    );
    let mut session = session_for(&ws);
    let using = |package: &str, call: &str| {
        format!(
            "CLASS {package}.User:\n    METHOD PUBLIC VOID Go():\n\
             \x20       DEFINE VARIABLE o AS app.Pk NO-UNDO.\n        {call}\n    END METHOD.\n\
             END CLASS.\n"
        )
    };
    let all = [
        "Pk()",
        "Pk(INPUT n AS INTEGER)",
        "Pk(INPUT n AS INTEGER, INPUT m AS INTEGER)",
    ];
    // The argument picks one of them, which then comes first.
    let sorted = |answer: Value| {
        let mut shown = labels(&answer);
        shown.sort();
        shown
    };
    // The same package reaches both.
    let case = ws.open(
        &mut session,
        "app/User.cls",
        &using("app", "o = NEW app.Pk(‸1)."),
    );
    assert_eq!(sorted(help(&mut session, &case)), all);
    // Another package, and a procedure, reach the public one only.
    let case = ws.open(
        &mut session,
        "other/User.cls",
        &using("other", "o = NEW app.Pk(‸1)."),
    );
    assert_eq!(labels(&help(&mut session, &case)), ["Pk()"]);
    let case = ws.program(&mut session, "oScorer = NEW app.Pk(‸1).\n");
    assert_eq!(labels(&help(&mut session, &case)), ["Pk()"]);
    // `SUPER(` of a subclass: the package-protected one always, the
    // package-private one in the package only.
    let kid = |package: &str| {
        format!(
            "CLASS {package}.Kid INHERITS app.Pk:\n    CONSTRUCTOR PUBLIC Kid():\n\
             \x20       SUPER(‸).\n    END CONSTRUCTOR.\nEND CLASS.\n"
        )
    };
    let case = ws.open(&mut session, "app/Kid.cls", &kid("app"));
    assert_eq!(sorted(help(&mut session, &case)), all);
    let case = ws.open(&mut session, "other/Kid.cls", &kid("other"));
    assert_eq!(
        sorted(help(&mut session, &case)),
        ["Pk()", "Pk(INPUT n AS INTEGER, INPUT m AS INTEGER)"]
    );
    // Both in the unnamed package: a package access there does not compile
    // (error 19477), and oxabl gives no answer.
    let case = ws.program(&mut session, "oScorer = NEW Bare(‸1).\n");
    assert_eq!(help(&mut session, &case), Value::Null);
    let case = ws.open(
        &mut session,
        "bare/User.cls",
        "CLASS User:\n    METHOD PUBLIC VOID Go():\n        DEFINE VARIABLE o AS Bare NO-UNDO.\n\
         \x20       o = NEW Bare(‸1).\n    END METHOD.\nEND CLASS.\n",
    );
    assert_eq!(help(&mut session, &case), Value::Null);
}

// ---------------------------------------------------------------------------
// The table of built-in functions
// ---------------------------------------------------------------------------

#[test]
fn the_built_in_table_has_the_arguments_the_reference_gives() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let mut active = |marked: &str| {
        let case = ws.program(&mut session, marked);
        let answer = help(&mut session, &case);
        let at = answer["signatures"][0]["activeParameter"].clone();
        (
            labels(&answer),
            at.as_u64()
                .map(|at| parameter_text(&answer, 0, at as usize)),
        )
    };
    // The second argument is the quote character, the third the string for
    // the unknown value.
    let (labels, parameter) = active("cName = QUOTER(cName, \"'\", ‸\"nul\").\n");
    assert_eq!(
        labels,
        ["QUOTER(expression [, quote-char [, null-string]])"]
    );
    assert_eq!(parameter.as_deref(), Some("null-string"));
    let (_, parameter) = active("cName = QUOTER(cName, ‸\"'\").\n");
    assert_eq!(parameter.as_deref(), Some("quote-char"));
    // `COMPARE` takes a collation.
    let (_, parameter) = active("iResult = COMPARE(cName, \"=\", cName, \"RAW\", ‸\"x\").\n");
    assert_eq!(parameter.as_deref(), Some("collation"));
    // `SUBSTITUTE` takes a string and nine arguments.
    let nine = "SUBSTITUTE(\"&1\", 1, 2, 3, 4, 5, 6, 7, 8, ‸9)";
    assert_eq!(
        active(&format!("cName = {nine}.\n")).1.as_deref(),
        Some("arg")
    );
    let ten = "SUBSTITUTE(\"&1\", 1, 2, 3, 4, 5, 6, 7, 8, 9, ‸10)";
    assert_eq!(active(&format!("cName = {ten}.\n")).1, None);
    // `DATETIME` has a form with a string.
    let (labels, _) = active("iResult = DATETIME(‸cName).\n");
    assert!(
        labels.contains(&"DATETIME(string)".to_string()),
        "{labels:?}"
    );
}

// ---------------------------------------------------------------------------
// A keyword in front of a parenthesis opens no call
// ---------------------------------------------------------------------------

#[test]
fn a_parenthesis_after_a_keyword_leaves_the_call_around_it() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for marked in [
        "iResult = Calc(1, IF (a > ‸1) THEN \"a\" ELSE \"b\").\n",
        "iResult = Calc(1, STRING(NOT (‸TRUE))).\n",
        "iResult = Calc(1, STRING(a > 0 AND (‸a < 9))).\n",
    ] {
        let case = ws.program(&mut session, marked);
        let answer = help(&mut session, &case);
        // `STRING(` is a call of its own; the others show `Calc`.
        let first = answer["signatures"][0]["label"]
            .as_str()
            .unwrap_or_default();
        if marked.contains("STRING(") {
            assert!(first.starts_with("STRING("), "{marked}: {answer}");
        } else {
            assert_eq!(
                first, "Calc(INPUT a AS INTEGER, INPUT b AS CHARACTER): INTEGER",
                "{marked}"
            );
            assert_eq!(answer["signatures"][0]["activeParameter"], 1, "{marked}");
        }
    }
}

#[test]
fn a_user_function_may_have_the_name_of_a_keyword() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let define = |name: &str| {
        format!(
            "FUNCTION {name} RETURNS INTEGER (INPUT x AS INTEGER, INPUT y AS CHARACTER):\n\
             \x20   RETURN x.\nEND FUNCTION.\n"
        )
    };
    for name in [
        "Total", "Size", "Count", "Row", "Column", "Initial", "Average",
    ] {
        let label = format!("{name}(INPUT x AS INTEGER, INPUT y AS CHARACTER): INTEGER");
        for call in [
            format!("iResult = {name}(2, ‸\"x\").\n"),
            format!("iResult = Calc(1, {name}(2, ‸\"x\")).\n"),
            format!("iResult = Calc(1, {name}(2, \"x\", ‸yes)).\n"),
        ] {
            let case = ws.program(&mut session, &format!("{}{call}", define(name)));
            let answer = help(&mut session, &case);
            assert_eq!(answer["signatures"][0]["label"], label, "{call}");
        }
    }
}

// ---------------------------------------------------------------------------
// The setting, and the names that repeat the parameter
// ---------------------------------------------------------------------------

#[test]
fn the_parameter_hints_are_off_until_the_setting_turns_them_on() {
    let mut ws = Workspace::new();
    let mut session = Session::start_with_options(ws.root.path(), capabilities(), Value::Null);
    let case = ws.program(&mut session, "iResult = Calc(1, \"x\").‸\n");
    assert_eq!(hints(&mut session, &case, 0, case.text.len()), []);

    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"inlayHints": {"parameters": true}}}}
    }));
    assert_eq!(hints(&mut session, &case, 0, case.text.len()).len(), 2);
}

#[test]
fn an_argument_that_repeats_the_parameter_name_gets_no_hint() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.program(
        &mut session,
        "DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n\
         DEFINE VARIABLE cCustLabel AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE qty AS INTEGER NO-UNDO.\n\
         DEFINE VARIABLE iOther AS INTEGER NO-UNDO.\n\
         PROCEDURE Prefixed:\n\
         DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.\n\
         DEFINE INPUT PARAMETER piCount AS INTEGER NO-UNDO.\n\
         DEFINE INPUT PARAMETER pcLabel AS CHARACTER NO-UNDO.\n\
         DEFINE INPUT PARAMETER pdQty AS INTEGER NO-UNDO.\n\
         DEFINE INPUT PARAMETER pcNote AS CHARACTER NO-UNDO.\n\
         DEFINE INPUT PARAMETER piTotal AS INTEGER NO-UNDO.\n\
         END PROCEDURE.\n\
         RUN Prefixed (cName, iCount, cCustLabel, qty, \"x\", iOther).‸\n",
    );
    // `cName` is `pcName` and `iCount` is `piCount` without their prefix,
    // `cCustLabel` ends with the word of `pcLabel`, and `pdQty` ends with `qty`.
    let found = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(
        found,
        [
            ("pcNote:".to_string(), "\"x\", iOther).".to_string()),
            ("piTotal:".to_string(), "iOther).".to_string()),
        ]
    );
}

// ---------------------------------------------------------------------------
// A write while the hints are computed
// ---------------------------------------------------------------------------

/// The analysis of the parameter-name hints holds its snapshot for 1.5 s, and
/// tells the client when it starts to hold it.
const HELD_PARAMETERS: &str = "/* OXABL-TEST-HOLD:lsp.parameter-hints@1500 */\n\
    FUNCTION Calc RETURNS INTEGER (INPUT a AS INTEGER, INPUT b AS CHARACTER):\n\
    \x20   RETURN a.\nEND FUNCTION.\nDEFINE VARIABLE iResult AS INTEGER NO-UNDO.\n\
    iResult = Calc(1, \"x\").‸\n";

fn ask_for_hints(session: &mut Session, case: &Case) -> u64 {
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/inlayHint",
        "params": {
            "textDocument": {"uri": file_uri(&case.file)},
            "range": {"start": position(&case.text, 0), "end": position(&case.text, case.text.len())},
        }
    }));
    id
}

/// Wait until the analysis of the parameter-name hints holds its snapshot.
fn wait_for_hold(session: &Session) {
    session.wait_for(|m| {
        m["method"] == "window/logMessage"
            && m["params"]["message"]
                .as_str()
                .is_some_and(|text| text.contains("OXABL-TEST-HOLD started: lsp.parameter-hints"))
    });
}

fn change_text(session: &mut Session, file: &std::path::Path, version: i32, text: &str) {
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(file), "version": version},
            "contentChanges": [{"text": text}]
        }
    }));
}

#[test]
fn an_edit_of_the_file_while_its_hints_are_computed_answers_content_modified() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let case = ws.open(&mut session, "held_edit.p", HELD_PARAMETERS);
    let slow = ask_for_hints(&mut session, &case);
    wait_for_hold(&session);
    let edited = format!("{}/* edit */\n", case.text);
    change_text(&mut session, &case.file, 2, &edited);

    let old = session.wait_for(|m| m["id"] == json!(slow));
    assert_eq!(
        old["error"]["code"].as_i64(),
        Some(-32801),
        "the old request answers ContentModified: {old}"
    );
    assert!(
        old.get("result").is_none(),
        "no hints of the old text: {old}"
    );

    // The client asks again for the new text.
    let case = Case {
        text: edited,
        ..case
    };
    let found = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(
        found.iter().map(|h| h.0.as_str()).collect::<Vec<_>>(),
        ["a:", "b:"]
    );
}

#[test]
fn an_edit_of_another_file_while_the_hints_are_computed_does_not_lose_them() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let other = ws.open(&mut session, "other_file.p", "MESSAGE 1.‸\n");
    let case = ws.open(&mut session, "held_other.p", HELD_PARAMETERS);
    let slow = ask_for_hints(&mut session, &case);
    wait_for_hold(&session);
    // The text of the file is the same, and the client does not ask again
    // after `ContentModified`: the analysis runs again.
    change_text(&mut session, &other.file, 2, "MESSAGE 2.\n");

    let answer = session.wait_for(|m| m["id"] == json!(slow));
    assert!(answer.get("error").is_none(), "{answer}");
    let labels: Vec<_> = answer["result"]
        .as_array()
        .unwrap()
        .iter()
        .map(|h| h["label"].as_str().unwrap().to_string())
        .collect();
    assert_eq!(labels, ["a:", "b:"], "{answer}");
}

#[test]
fn the_rerun_after_an_edit_of_another_file_reads_the_edited_text() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws, Value::Null);
    let scorer = ws.open(&mut session, "app/Scorer.cls", &format!("{SCORER}\u{2038}"));
    let case = ws.open(
        &mut session,
        "held_rerun.p",
        "/* OXABL-TEST-HOLD:lsp.parameter-hints@1500 */\n\
         DEFINE VARIABLE oScorer AS app.Scorer NO-UNDO.\n\
         DEFINE VARIABLE i AS INTEGER NO-UNDO.\ni = oScorer:Total(2).\u{2038}\n",
    );
    let slow = ask_for_hints(&mut session, &case);
    wait_for_hold(&session);
    let renamed = SCORER.replace(
        "Total(INPUT x AS INTEGER):\n        RETURN x.",
        "Total(INPUT amount AS INTEGER):\n        RETURN amount.",
    );
    assert_ne!(renamed, SCORER);
    change_text(&mut session, &scorer.file, 2, &renamed);

    let answer = session.wait_for(|m| m["id"] == json!(slow));
    assert!(answer.get("error").is_none(), "{answer}");
    assert_eq!(answer["result"][0]["label"], "amount:", "{answer}");
}

/// Whether the server asks the client for new hints within `within`.
fn asks_for_new_hints(session: &Session, within: std::time::Duration) -> bool {
    let deadline = std::time::Instant::now() + within;
    while let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) {
        if let Some(message) = session.try_next(left)
            && message["method"] == "workspace/inlayHint/refresh"
        {
            return true;
        }
    }
    false
}

#[test]
fn an_edit_of_one_open_file_sends_no_hint_refresh() {
    let capabilities = json!({
        "textDocument": {"inlayHint": {}},
        "workspace": {"inlayHint": {"refreshSupport": true}}
    });
    let quiet = std::time::Duration::from_millis(1500);
    let mut ws = Workspace::new();
    let mut session = Session::start_with_options(
        ws.root.path(),
        capabilities,
        json!({"inlayHints": {"parameters": true}}),
    );
    ws.open(&mut session, "first.p", "MESSAGE 1.\u{2038}\n");
    let second = ws.open(&mut session, "second.p", "MESSAGE 2.\u{2038}\n");
    // Everything the opening sets going has settled.
    while asks_for_new_hints(&session, quiet) {}
    // Each refresh makes the client ask every visible file again, and each
    // request is a new analysis on the one worker: an edit must not send one.
    for version in 2..5 {
        change_text(&mut session, &second.file, version, "MESSAGE 3.\n");
    }
    assert!(
        !asks_for_new_hints(&session, quiet),
        "an edit sent a refresh"
    );
}

#[test]
fn the_server_advertises_signature_help() {
    let ws = Workspace::new();
    let session = session_for(&ws);
    let provider = &session.initialized["result"]["capabilities"]["signatureHelpProvider"];
    assert_eq!(provider["triggerCharacters"], json!(["(", ","]));
}
