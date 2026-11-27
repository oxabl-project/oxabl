//! Signature help, parameter-name hints and hover for a parameter list that
//! holds an include, through the real `oxabl lsp`.
//!
//! Many code bases start each parameter list with an include that declares a
//! block of shared parameters, and each caller passes a matching include of
//! arguments. The include is one entry of the signature, written as in the
//! declaration, with the count of the parameters it declares. A declaration
//! whose expansion is not known has no signature.
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

const STD_PARAMS: &str = "INPUT pcCompany AS CHARACTER,\nINPUT pcUser AS CHARACTER\n";
const STD_INPUT: &str = "DEFINE INPUT PARAMETER pcCompany AS CHARACTER NO-UNDO.\n\
                         DEFINE INPUT PARAMETER pcUser AS CHARACTER NO-UNDO.\n";
const STD_ARGS: &str = "cCompany, cUser";

const DOC: &str = r#"CLASS app.Doc:
    CONSTRUCTOR PUBLIC Doc({inc/std-params.i}, INPUT pcNote AS CHARACTER):
    END CONSTRUCTOR.
    METHOD PUBLIC VOID SetData({inc/std-params.i}, INPUT pcDocNum AS CHARACTER, OUTPUT plOk AS LOGICAL):
    END METHOD.
    METHOD PUBLIC VOID Single({inc/one.i}, INPUT pcDocNum AS CHARACTER):
    END METHOD.
    METHOD PUBLIC VOID Plain(INPUT pcDocNum AS CHARACTER):
    END METHOD.
END CLASS.
"#;

const PROG: &str = "{inc/std-input.i}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n";

/// The head of each case program: variables the callers use.
const PRELUDE: &str = r#"DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDone AS LOGICAL NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE oDoc AS app.Doc NO-UNDO.
"#;

struct Workspace {
    root: tempfile::TempDir,
    cases: usize,
}

impl Workspace {
    fn new() -> Workspace {
        let ws = Workspace {
            root: tempfile::tempdir().unwrap(),
            cases: 0,
        };
        ws.write("oxabl.toml", CONFIG);
        ws.write("inc/std-params.i", STD_PARAMS);
        ws.write("inc/std-input.i", STD_INPUT);
        ws.write("inc/std-args.i", STD_ARGS);
        ws.write("inc/one.i", "INPUT pcOnly AS INTEGER\n");
        ws.write("app/Doc.cls", DOC);
        ws.write("prog.p", PROG);
        ws.write(
            "lib.p",
            "PROCEDURE hp:\n    {inc/std-input.i}\n\
             \x20   DEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\nEND PROCEDURE.\n",
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
        let at = marked.find(CURSOR).unwrap_or(0);
        let text = marked.replacen(CURSOR, "", 1);
        self.write(name, &text);
        let file = self.path(name);
        session.open(&file, &text);
        Case { file, text, at }
    }
}

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

fn parameter_count(help: &Value) -> usize {
    help["signatures"][0]["parameters"]
        .as_array()
        .unwrap()
        .len()
}

fn session_for(ws: &Workspace) -> Session {
    Session::start(ws.root.path())
}

fn hint_session(ws: &Workspace) -> Session {
    Session::start_with_options(
        ws.root.path(),
        json!({"textDocument": {"inlayHint": {}, "signatureHelp": {}}}),
        json!({"inlayHints": {"parameters": true}}),
    )
}

/// The parameter hints of `case`, each with the text that follows it on its
/// line.
fn hints(session: &mut Session, case: &Case) -> Vec<(String, String)> {
    let answer = session.request(
        "textDocument/inlayHint",
        json!({
            "textDocument": {"uri": file_uri(&case.file)},
            "range": {
                "start": position(&case.text, 0),
                "end": position(&case.text, case.text.len()),
            },
        }),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    answer["result"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|hint| hint["kind"] == 2)
        .map(|hint| {
            let line = hint["position"]["line"].as_u64().unwrap() as usize;
            let column = hint["position"]["character"].as_u64().unwrap() as usize;
            let source_line = case.text.lines().nth(line).unwrap();
            let at: usize = source_line.chars().take(column).map(char::len_utf8).sum();
            (
                hint["label"].as_str().unwrap().to_string(),
                source_line[at..].to_string(),
            )
        })
        .collect()
}

/// The hover text at the cursor of `case`.
fn hover(session: &mut Session, case: &Case) -> Option<String> {
    let answer = session.request(
        "textDocument/hover",
        json!({
            "textDocument": {"uri": file_uri(&case.file)},
            "position": position(&case.text, case.at),
        }),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    answer["result"]["contents"]["value"]
        .as_str()
        .map(str::to_string)
}

const SET_DATA: &str = "SetData({inc/std-params.i} (2 parameters), \
                        INPUT pcDocNum AS CHARACTER, OUTPUT plOk AS LOGICAL): VOID";

// ---------------------------------------------------------------------------
// A method, a constructor, a program, a procedure
// ---------------------------------------------------------------------------

#[test]
fn a_method_shows_its_parameter_include_as_one_entry() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oDoc:SetData(‸\n");
    let answer = help(&mut session, &case);
    assert_eq!(labels(&answer), [SET_DATA]);
    assert_eq!(parameter_count(&answer), 3);
    assert_eq!(
        parameter_text(&answer, 0, 0),
        "{inc/std-params.i} (2 parameters)"
    );
    assert_eq!(parameter_text(&answer, 0, 1), "INPUT pcDocNum AS CHARACTER");
    assert_eq!(parameter_text(&answer, 0, 2), "OUTPUT plOk AS LOGICAL");
}

#[test]
fn an_include_of_one_parameter_says_one_parameter() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oDoc:Single(‸\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Single({inc/one.i} (1 parameter), INPUT pcDocNum AS CHARACTER): VOID"]
    );
}

#[test]
fn a_constructor_shows_its_parameter_include_as_one_entry() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oDoc = NEW app.Doc(‸\n");
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Doc({inc/std-params.i} (2 parameters), INPUT pcNote AS CHARACTER)"]
    );
}

#[test]
fn a_program_that_starts_with_an_include_of_parameters_shows_it_as_one_entry() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "RUN prog.p (‸\n");
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["prog.p({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)"]
    );
    assert_eq!(parameter_count(&answer), 2);
}

#[test]
fn a_procedure_of_another_program_shows_its_parameter_include_as_one_entry() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(
        &mut session,
        "RUN lib.p PERSISTENT SET hLib.\nRUN hp IN hLib (‸\n",
    );
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["hp({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)"]
    );
}

#[test]
fn a_procedure_and_a_function_of_the_unit_show_their_parameter_include_as_one_entry() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(
        &mut session,
        "PROCEDURE Local:\n    {inc/std-input.i}\n\
         \x20   DEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\nEND PROCEDURE.\n\
         FUNCTION Fn RETURNS INTEGER ({inc/std-params.i}, INPUT x AS INTEGER):\n\
         \x20   RETURN x.\nEND FUNCTION.\n\
         RUN Local (‸\n",
    );
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Local({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)"]
    );
    let case = ws.program(
        &mut session,
        "FUNCTION Fn RETURNS INTEGER ({inc/std-params.i}, INPUT x AS INTEGER):\n\
         \x20   RETURN x.\nEND FUNCTION.\n\
         DEFINE VARIABLE i AS INTEGER NO-UNDO.\ni = Fn(‸\n",
    );
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Fn({inc/std-params.i} (2 parameters), INPUT x AS INTEGER): INTEGER"]
    );
}

#[test]
fn a_method_next_to_a_plain_one_keeps_its_own_label() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(&mut session, "oDoc:Plain(‸\n");
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Plain(INPUT pcDocNum AS CHARACTER): VOID"]
    );
}

// ---------------------------------------------------------------------------
// The active parameter counts through the include
// ---------------------------------------------------------------------------

/// The active parameter of the first signature at the cursor of `marked`.
fn active(ws: &mut Workspace, session: &mut Session, marked: &str) -> Value {
    let case = ws.program(session, marked);
    let answer = help(session, &case);
    assert!(answer.is_object(), "no signature for {marked}");
    answer["signatures"][0]["activeParameter"].clone()
}

#[test]
fn the_active_parameter_counts_through_the_include_of_plain_arguments() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for (marked, wanted) in [
        ("oDoc:SetData(‸\n", json!(0)),
        ("oDoc:SetData(cCompany‸\n", json!(0)),
        ("oDoc:SetData(cCompany, ‸\n", json!(0)),
        ("oDoc:SetData(cCompany, cUser‸\n", json!(0)),
        ("oDoc:SetData(cCompany, cUser, ‸\n", json!(1)),
        ("oDoc:SetData(cCompany, cUser, \"D1\", ‸\n", json!(2)),
        (
            "oDoc:SetData(cCompany, cUser, \"D1\", OUTPUT lDone, ‸\n",
            Value::Null,
        ),
    ] {
        assert_eq!(active(&mut ws, &mut session, marked), wanted, "{marked}");
    }
}

#[test]
fn a_caller_with_an_include_of_arguments_counts_it_as_the_arguments_it_expands_to() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for (marked, wanted) in [
        (
            "oDoc:SetData({inc/std-args.i}‸, \"D1\", OUTPUT lDone).\n",
            json!(0),
        ),
        (
            "oDoc:SetData({inc/std-args.i}, ‸\"D1\", OUTPUT lDone).\n",
            json!(1),
        ),
        (
            "oDoc:SetData({inc/std-args.i}, \"D1\", ‸OUTPUT lDone).\n",
            json!(2),
        ),
        ("RUN prog.p ({inc/std-args.i}, ‸\"D1\").\n", json!(1)),
    ] {
        assert_eq!(active(&mut ws, &mut session, marked), wanted, "{marked}");
    }
}

#[test]
fn the_active_parameter_of_a_program_counts_through_the_include() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    for (marked, wanted) in [
        ("RUN prog.p (cCompany, ‸\n", json!(0)),
        ("RUN prog.p (cCompany, cUser, ‸\n", json!(1)),
    ] {
        assert_eq!(active(&mut ws, &mut session, marked), wanted, "{marked}");
    }
}

// ---------------------------------------------------------------------------
// Parameter-name hints
// ---------------------------------------------------------------------------

#[test]
fn plain_arguments_for_the_include_get_one_hint_naming_it() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws);
    let case = ws.program(
        &mut session,
        "oDoc:SetData(cCompany, cUser, \"D1\", OUTPUT lDone).\n\
         RUN prog.p (cCompany, cUser, \"D1\").\n‸",
    );
    assert_eq!(
        hints(&mut session, &case),
        [
            (
                "{inc/std-params.i}:",
                "cCompany, cUser, \"D1\", OUTPUT lDone)."
            ),
            ("pcDocNum:", "\"D1\", OUTPUT lDone)."),
            ("plOk:", "OUTPUT lDone)."),
            ("{inc/std-input.i}:", "cCompany, cUser, \"D1\")."),
            ("pcDocNum:", "\"D1\")."),
        ]
        .map(|(label, rest)| (label.to_string(), rest.to_string()))
    );
}

#[test]
fn an_argument_that_is_an_include_reference_gets_no_hint() {
    let mut ws = Workspace::new();
    let mut session = hint_session(&ws);
    let case = ws.program(
        &mut session,
        "oDoc:SetData({inc/std-args.i}, \"D1\", OUTPUT lDone).\n\
         RUN prog.p ({inc/std-args.i}, \"D1\").\n‸",
    );
    assert_eq!(
        hints(&mut session, &case),
        [
            ("pcDocNum:", "\"D1\", OUTPUT lDone)."),
            ("plOk:", "OUTPUT lDone)."),
            ("pcDocNum:", "\"D1\")."),
        ]
        .map(|(label, rest)| (label.to_string(), rest.to_string()))
    );
}

// ---------------------------------------------------------------------------
// Hover
// ---------------------------------------------------------------------------

#[test]
fn hover_shows_the_include_where_it_stands_with_its_count() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(
        &mut session,
        "oDoc:‸SetData(cCompany, cUser, \"D1\", OUTPUT lDone).\n",
    );
    let text = hover(&mut session, &case).expect("a hover");
    assert!(
        text.contains("{inc/std-params.i} (2 parameters)")
            && text.contains("INPUT pcDocNum AS CHARACTER")
            && !text.contains("pcCompany"),
        "{text}"
    );
    // The program of a handle: the procedure the handle runs.
    let case = ws.program(
        &mut session,
        "RUN lib.p PERSISTENT SET hLib.\nRUN ‸hp IN hLib (cCompany, cUser, \"D1\").\n",
    );
    let text = hover(&mut session, &case).expect("a hover");
    assert!(
        text.contains(
            "PROCEDURE hp ({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)"
        ) && !text.contains("pcCompany"),
        "{text}"
    );
}

#[test]
fn hover_shows_the_include_of_a_routine_of_the_unit() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(
        &mut session,
        "PROCEDURE Local:\n    {inc/std-input.i}\n\
         \x20   DEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\nEND PROCEDURE.\n\
         RUN ‸Local (cCompany, cUser, \"D1\").\n",
    );
    let text = hover(&mut session, &case).expect("a hover");
    assert!(
        text.contains(
            "PROCEDURE Local ({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)"
        ) && !text.contains("pcCompany"),
        "{text}"
    );
}

// ---------------------------------------------------------------------------
// Where the expansion is not known, there is no signature
// ---------------------------------------------------------------------------

fn closed_workspace() -> Workspace {
    let ws = Workspace::new();
    // A `{&name}` value and an `&IF` in the parameter list.
    ws.write(
        "app/RefList.cls",
        "&SCOPED-DEFINE P INPUT pcA AS CHARACTER,\n\
         CLASS app.RefList:\n\
         \x20   METHOD PUBLIC VOID Go({&P} INPUT pcB AS INTEGER):\n    END METHOD.\n\
         END CLASS.\n",
    );
    ws.write(
        "app/IfList.cls",
        "CLASS app.IfList:\n\
         \x20   METHOD PUBLIC VOID Go(&IF DEFINED(X) = 0 &THEN INPUT pcA AS CHARACTER, &ENDIF \
         INPUT pcB AS INTEGER):\n    END METHOD.\n\
         END CLASS.\n",
    );
    // An include that holds a `{&name}` reference or an `&IF`.
    ws.write("inc/with-ref.i", "INPUT pcA AS {&TYPE}\n");
    ws.write(
        "inc/with-if.i",
        "&IF DEFINED(X) = 0 &THEN INPUT pcA AS CHARACTER &ELSE INPUT pcA AS INTEGER &ENDIF\n",
    );
    for (class, include) in [("InRef", "with-ref"), ("InIf", "with-if")] {
        ws.write(
            &format!("app/{class}.cls"),
            &format!(
                "CLASS app.{class}:\n\
                 \x20   METHOD PUBLIC VOID Go({{inc/{include}.i}}, INPUT pcB AS INTEGER):\n\
                 \x20   END METHOD.\nEND CLASS.\n"
            ),
        );
    }
    // An include that did not arrive.
    ws.write(
        "app/Missing.cls",
        "CLASS app.Missing:\n\
         \x20   METHOD PUBLIC VOID Go({inc/not-there.i}, INPUT pcB AS INTEGER):\n    END METHOD.\n\
         END CLASS.\n",
    );
    // An include that holds an include that did not arrive.
    ws.write(
        "inc/nested.i",
        "INPUT pcA AS CHARACTER,\n{inc/not-there.i}\n",
    );
    ws.write(
        "app/Nested.cls",
        "CLASS app.Nested:\n\
         \x20   METHOD PUBLIC VOID Go({inc/nested.i} INPUT pcB AS INTEGER):\n    END METHOD.\n\
         END CLASS.\n",
    );
    ws.write(
        "app/NestedCtor.cls",
        "CLASS app.NestedCtor:\n\
         \x20   CONSTRUCTOR PUBLIC NestedCtor({inc/nested.i} INPUT pcB AS INTEGER):\n\
         \x20   END CONSTRUCTOR.\nEND CLASS.\n",
    );
    ws.write(
        "inc/nested-in.i",
        "DEFINE INPUT PARAMETER pcA AS CHARACTER NO-UNDO.\n{inc/not-there.i}\n",
    );
    ws.write(
        "nested-include.p",
        "{inc/nested-in.i}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n",
    );
    // An include that does not parse.
    ws.write("inc/bad.i", "INPUT pcA AS CHARACTER, oops oops oops,\n");
    ws.write(
        "app/BadCtor.cls",
        "CLASS app.BadCtor:\n\
         \x20   CONSTRUCTOR PUBLIC BadCtor({inc/bad.i} INPUT pcB AS INTEGER):\n\
         \x20   END CONSTRUCTOR.\nEND CLASS.\n",
    );
    // An include of members that does not parse, beside a parameter include.
    ws.write("inc/broken.i", "METHOD PUBLIC VOID oops oops oops\n");
    ws.write(
        "app/Broken.cls",
        "CLASS app.Broken:\n{inc/broken.i}\n\
         \x20   METHOD PUBLIC VOID Go({inc/std-params.i}, INPUT pcB AS INTEGER):\n\
         \x20   END METHOD.\nEND CLASS.\n",
    );
    // An include in the parameter list of a constructor.
    ws.write(
        "app/MissingCtor.cls",
        "CLASS app.MissingCtor:\n\
         \x20   CONSTRUCTOR PUBLIC MissingCtor({inc/not-there.i}, INPUT pcB AS INTEGER):\n\
         \x20   END CONSTRUCTOR.\nEND CLASS.\n",
    );
    // An include before the constructors may declare one.
    ws.write(
        "inc/ctor.i",
        "CONSTRUCTOR PUBLIC Mixed(INPUT a AS INTEGER):\nEND CONSTRUCTOR.\n",
    );
    ws.write(
        "app/Mixed.cls",
        "CLASS app.Mixed:\n{inc/ctor.i}\n\
         \x20   CONSTRUCTOR PUBLIC Mixed({inc/std-params.i}, INPUT pcB AS INTEGER):\n\
         \x20   END CONSTRUCTOR.\nEND CLASS.\n",
    );
    // A main block with an include that declares no parameter.
    ws.write(
        "inc/vars.i",
        "DEFINE VARIABLE cLocal AS CHARACTER NO-UNDO.\n",
    );
    ws.write(
        "vars-first.p",
        "{inc/vars.i}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n",
    );
    ws.write(
        "vars-and-params.p",
        "{inc/std-input.i}\n{inc/vars.i}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n",
    );
    ws.write(
        "ref-and-params.p",
        "&SCOPED-DEFINE P DEFINE INPUT PARAMETER pcA AS CHARACTER NO-UNDO.\n\
         {inc/std-input.i}\n{&P}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n",
    );
    ws.write("inc/in-ref.i", "DEFINE INPUT PARAMETER pcA AS {&TYPE}.\n");
    ws.write(
        "include-with-ref.p",
        "{inc/in-ref.i}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n",
    );
    ws.write(
        "missing-include.p",
        "{inc/not-there.i}\nDEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\n",
    );
    ws
}

#[test]
fn a_declaration_whose_expansion_is_not_known_has_no_signature_and_no_hint() {
    let mut ws = closed_workspace();
    let mut session = hint_session(&ws);
    for marked in [
        "DEFINE VARIABLE o AS app.RefList NO-UNDO.\no = NEW app.RefList().\no:Go(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.IfList NO-UNDO.\no = NEW app.IfList().\no:Go(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.InRef NO-UNDO.\no = NEW app.InRef().\no:Go(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.InIf NO-UNDO.\no = NEW app.InIf().\no:Go(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.Missing NO-UNDO.\no = NEW app.Missing().\no:Go(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.Nested NO-UNDO.\no = NEW app.Nested().\no:Go(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.NestedCtor NO-UNDO.\no = NEW app.NestedCtor(‸\"a\", 1).\n",
        "RUN nested-include.p (‸\"a\", \"b\").\n",
        "DEFINE VARIABLE o AS app.BadCtor NO-UNDO.\no = NEW app.BadCtor(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.Broken NO-UNDO.\no = NEW app.Broken().\no:Go(‸\"a\", \"b\", 1).\n",
        "DEFINE VARIABLE o AS app.MissingCtor NO-UNDO.\no = NEW app.MissingCtor(‸\"a\", 1).\n",
        "DEFINE VARIABLE o AS app.Mixed NO-UNDO.\no = NEW app.Mixed(‸\"a\", \"b\", 1).\n",
        "RUN vars-first.p (‸\"a\").\n",
        "RUN vars-and-params.p (‸\"a\", \"b\", \"c\").\n",
        "RUN ref-and-params.p (‸\"a\", \"b\", \"c\", \"d\").\n",
        "RUN include-with-ref.p (‸\"a\", \"b\").\n",
        "RUN missing-include.p (‸\"a\").\n",
    ] {
        let case = ws.program(&mut session, marked);
        assert_eq!(help(&mut session, &case), Value::Null, "{marked}");
        assert_eq!(hints(&mut session, &case), [], "{marked}");
    }
}

#[test]
fn an_include_in_a_parameter_list_hides_no_other_method_of_the_class() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    // `Plain` of the class that also has `SetData`.
    let case = ws.program(&mut session, "oDoc:Plain(‸\n");
    assert!(help(&mut session, &case).is_object());
}

#[test]
fn hover_of_a_declaration_that_is_not_known_whole_does_not_name_the_include() {
    let mut ws = closed_workspace();
    let mut session = session_for(&ws);
    let case = ws.program(
        &mut session,
        "DEFINE VARIABLE o AS app.Missing NO-UNDO.\no = NEW app.Missing().\no:‸Go(\"a\", 1).\n",
    );
    let text = hover(&mut session, &case).unwrap_or_default();
    assert!(!text.contains("(2 parameters)"), "{text}");
}

#[test]
fn a_member_that_an_include_declares_has_no_signature_beside_a_parameter_include() {
    let mut ws = Workspace::new();
    ws.write(
        "inc/part-method.i",
        "METHOD PUBLIC VOID Extra(INPUT e AS INTEGER):\nEND METHOD.\n",
    );
    ws.write(
        "app/Parts.cls",
        "CLASS app.Parts:\n{inc/part-method.i}\n\
         \x20   METHOD PUBLIC VOID Go({inc/std-params.i}, INPUT pcB AS INTEGER):\n\
         \x20   END METHOD.\nEND CLASS.\n",
    );
    let mut session = session_for(&ws);
    let marked = "DEFINE VARIABLE o AS app.Parts NO-UNDO.\no = NEW app.Parts().\n";
    let case = ws.program(&mut session, &format!("{marked}o:Extra(‸1).\n"));
    assert_eq!(help(&mut session, &case), Value::Null);
    let case = ws.program(&mut session, &format!("{marked}o:Go(‸\"a\", \"b\", 1).\n"));
    assert_eq!(
        labels(&help(&mut session, &case)),
        ["Go({inc/std-params.i} (2 parameters), INPUT pcB AS INTEGER): VOID"]
    );
}

#[test]
fn a_call_that_is_still_being_typed_reaches_a_procedure_with_a_parameter_include_below_it() {
    let mut ws = Workspace::new();
    let mut session = session_for(&ws);
    let case = ws.program(
        &mut session,
        "RUN Local (cCompany, ‸\n\
         PROCEDURE Local:\n    {inc/std-input.i}\n\
         \x20   DEFINE INPUT PARAMETER pcDocNum AS CHARACTER NO-UNDO.\nEND PROCEDURE.\n",
    );
    let answer = help(&mut session, &case);
    assert_eq!(
        labels(&answer),
        ["Local({inc/std-input.i} (2 parameters), INPUT pcDocNum AS CHARACTER)"]
    );
    assert_eq!(answer["signatures"][0]["activeParameter"], json!(0));
}
