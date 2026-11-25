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

fn hint_session(ws: &Workspace, options: Value) -> Session {
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
            ("pcName:", "OUTPUT cName).".to_string()),
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
    let case = ws.program(&mut session, "RUN DoWork (1, cName).‸\n");
    let found = hints(&mut session, &case, 0, case.text.len());
    assert_eq!(
        found,
        [
            ("piCount:".to_string(), "1, cName).".to_string()),
            ("OUTPUT pcName:".to_string(), "cName).".to_string()),
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

#[test]
fn the_server_advertises_signature_help() {
    let ws = Workspace::new();
    let session = session_for(&ws);
    let provider = &session.initialized["result"]["capabilities"]["signatureHelpProvider"];
    assert_eq!(provider["triggerCharacters"], json!(["(", ","]));
}
