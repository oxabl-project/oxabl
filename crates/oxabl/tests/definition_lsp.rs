//! `textDocument/definition` through the real `oxabl lsp` and its daemon: each
//! kind of name goes to its declaration, in the physical file that writes it.
//! A name with several candidates gives all of them, and a name that does not
//! resolve, or is computed at run time, gives none.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"navigation\"\n\
    [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
    [workspace.schema]\nfiles = [\"db/sports.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n  FORMAT \">>>>9\"\n\n\
    ADD FIELD \"Name\" OF \"Customer\" AS character\n  FORMAT \"x(30)\"\n\n\
    ADD TABLE \"company\"\n\n\
    ADD FIELD \"company\" OF \"company\" AS character\n\n\
    ADD TABLE \"warehouse\"\n\n\
    ADD FIELD \"warehouse\" OF \"warehouse\" AS character\n\n";

/// Temp-table fields that copy a schema field whose name is its table's name.
const TT_INCLUDE: &str = "def {&DefType} temp-table ttDeco no-undo\n\
    \x20  field company     like company.company\n\
    \x20  field warehouse   like warehouse.warehouse\n\
    \x20  field custName    like Customer.Name\n\
    \x20  field orphan      like company.nosuch\n\
    \x20  index iMain is primary unique company warehouse\n\
    .\n";

const DEFS: &str = "/* shared definitions */\n\
    DEFINE VARIABLE gcFromInclude AS CHARACTER NO-UNDO.\n";

const MAIN: &str = "/* Größe: ü */ USING app.Greeter.\n\
    USING app.*.\n\
    {defs.i}\n\
    /* ü é */ DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE oGreeter AS CLASS app.Greeter NO-UNDO.\n\
    DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.\n\
    DEFINE BUFFER bCust FOR Customer.\n\
    DEFINE TEMP-TABLE ttOrder NO-UNDO\n\
    \x20   FIELD OrderId AS INTEGER.\n\
    iCount = iCount + 1.\n\
    gcFromInclude = \"x\".\n\
    RUN util/helper.p.\n\
    RUN helper2.\n\
    RUN LocalProc (INPUT \"a\").\n\
    RUN VALUE(cProg).\n\
    RUN dup.p.\n\
    RUN missing.p.\n\
    oGreeter = NEW app.Greeter().\n\
    oGreeter:Hello().\n\
    FOR EACH Customer NO-LOCK:\n\
    \x20   DISPLAY Customer.Name.\n\
    END.\n\
    FIND FIRST bCust NO-LOCK NO-ERROR.\n\
    CREATE ttOrder.\n\
    ttOrder.OrderId = fnDouble(iCount).\n\
    iCount = noSuchThing.\n\
    iCount = fnDouble(noSuchArg).\n\
    PROCEDURE LocalProc:\n\
    \x20   DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.\n\
    \x20   MESSAGE pcName.\n\
    END PROCEDURE.\n\
    FUNCTION fnDouble RETURNS INTEGER (INPUT piValue AS INTEGER):\n\
    \x20   RETURN piValue * 2.\n\
    END FUNCTION.\n";

const GREETER: &str = "CLASS app.Greeter INHERITS app.Base IMPLEMENTS app.IGreeter:\n\
    \x20   METHOD PUBLIC VOID Hello():\n\
    \x20       Greet().\n\
    \x20       Polite().\n\
    \x20   END METHOD.\n\
    \x20   METHOD PRIVATE VOID Polite():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const BASE: &str = "CLASS app.Base:\n\
    \x20   METHOD PUBLIC VOID Greet():\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Greet(INPUT pc AS CHARACTER):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const IGREETER: &str = "INTERFACE app.IGreeter:\n\
    \x20   METHOD PUBLIC VOID Hello().\n\
    END INTERFACE.\n";

const FACTORY: &str = "DEFINE VARIABLE oAny AS Progress.Lang.Object NO-UNDO.\n\
    DEFINE VARIABLE cName AS CHARACTER NO-UNDO.\n\
    oAny = DYNAMIC-NEW cName ().\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new() -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let files: &[(&str, &str)] = &[
            ("oxabl.toml", CONFIG),
            ("db/sports.df", SCHEMA),
            ("defs.i", DEFS),
            ("deco_tt.i", TT_INCLUDE),
            ("main.p", MAIN),
            ("factory.p", FACTORY),
            ("util/helper.p", "MESSAGE \"helper\".\n"),
            ("helper2.p", "MESSAGE \"helper2\".\n"),
            ("dup.p", "MESSAGE \"one\".\n"),
            ("lib/dup.p", "MESSAGE \"two\".\n"),
            ("app/Greeter.cls", GREETER),
            ("app/Base.cls", BASE),
            ("app/IGreeter.cls", IGREETER),
        ];
        for (name, text) in files {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
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

/// The locations `textDocument/definition` gives for the cursor one character
/// into occurrence `occurrence` of `needle` in `file`. Fails on an error.
#[track_caller]
fn definition(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Vec<Value> {
    let (line, character) = utf16_at(text, nth(text, needle, occurrence) + 1);
    let answer = session.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": {"line": line, "character": character}
        }),
    );
    assert!(
        answer.get("error").is_none(),
        "definition of `{needle}` gave an error: {}",
        answer["error"]
    );
    match &answer["result"] {
        Value::Null => Vec::new(),
        Value::Array(locations) => locations.clone(),
        other => panic!("definition of `{needle}` gave {other}"),
    }
}

/// A location at occurrence `occurrence` of `needle` in `target`, spanning it.
fn at(target: &Path, text: &str, needle: &str, occurrence: usize) -> Value {
    let start = nth(text, needle, occurrence);
    let (line, character) = utf16_at(text, start);
    let (end_line, end_character) = utf16_at(text, start + needle.len());
    json!({
        "uri": file_uri(target),
        "range": {
            "start": {"line": line, "character": character},
            "end": {"line": end_line, "character": end_character}
        }
    })
}

/// A location naming a whole file.
fn file(target: &Path) -> Value {
    json!({
        "uri": file_uri(target),
        "range": {"start": {"line": 0, "character": 0}, "end": {"line": 0, "character": 0}}
    })
}

#[track_caller]
fn starts_on_line(location: &Value, target: &Path, line: u64) {
    assert_eq!(location["uri"], json!(file_uri(target)), "{location}");
    assert_eq!(
        location["range"]["start"]["line"],
        json!(line),
        "{location}"
    );
}

#[test]
fn local_names_go_to_their_declaration() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let cases: &[(&str, usize, &str, usize)] = &[
        // (use, occurrence, declaration, occurrence)
        ("iCount + 1", 0, "iCount", 0),
        ("oGreeter = NEW", 0, "oGreeter", 0),
        ("bCust NO-LOCK", 0, "bCust", 0),
        ("ttOrder.\n", 0, "ttOrder", 0),
        ("ttOrder.OrderId", 0, "ttOrder", 0),
        ("OrderId = ", 0, "OrderId", 0),
        ("LocalProc (", 0, "LocalProc:", 0),
        ("fnDouble(", 0, "fnDouble RETURNS", 0),
        ("pcName.", 0, "pcName", 0),
        ("piValue * 2", 0, "piValue", 0),
    ];
    for (use_site, occurrence, declaration, declared) in cases {
        let found = definition(&mut session, &main, MAIN, use_site, *occurrence);
        let name = declaration.split([' ', ':', '\n']).next().unwrap();
        let expected = at(
            &main,
            MAIN,
            name,
            nth_of(MAIN, declaration, *declared, name),
        );
        assert_eq!(found, vec![expected], "definition of `{use_site}`");
    }
}

/// Which occurrence of `name` starts occurrence `occurrence` of `context`.
fn nth_of(text: &str, context: &str, occurrence: usize, name: &str) -> usize {
    let start = nth(text, context, occurrence);
    text.match_indices(name)
        .position(|(at, _)| at == start)
        .unwrap()
}

#[test]
fn a_declaration_in_an_include_is_placed_in_the_include() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = definition(&mut session, &main, MAIN, "gcFromInclude = ", 0);
    assert_eq!(
        found,
        vec![at(&ws.path("defs.i"), DEFS, "gcFromInclude", 0)]
    );

    let found = definition(&mut session, &main, MAIN, "{defs.i}", 0);
    assert_eq!(found, vec![file(&ws.path("defs.i"))]);
}

#[test]
fn run_targets_go_to_the_program() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = definition(&mut session, &main, MAIN, "util/helper.p", 0);
    assert_eq!(found, vec![file(&ws.path("util/helper.p"))]);
    // Without an extension, through the configured run extensions.
    let found = definition(&mut session, &main, MAIN, "helper2.", 0);
    assert_eq!(found, vec![file(&ws.path("helper2.p"))]);
    // Two path entries supply it: both, not a guess.
    let mut found = definition(&mut session, &main, MAIN, "dup.p", 0);
    found.sort_by_key(|location| location["uri"].to_string());
    assert_eq!(
        found,
        vec![file(&ws.path("dup.p")), file(&ws.path("lib/dup.p"))]
    );
}

#[test]
fn class_names_go_to_the_class_file() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let greeter = ws.path("app/Greeter.cls");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let class_name = at(&greeter, GREETER, "app.Greeter", 0);

    for (needle, occurrence) in [("app.Greeter", 0), ("app.Greeter", 1), ("app.Greeter", 2)] {
        let found = definition(&mut session, &main, MAIN, needle, occurrence);
        assert_eq!(
            found,
            vec![class_name.clone()],
            "occurrence {occurrence} of `{needle}`"
        );
    }

    session.open(&greeter, GREETER);
    let found = definition(&mut session, &greeter, GREETER, "app.Base", 0);
    assert_eq!(
        found,
        vec![at(&ws.path("app/Base.cls"), BASE, "app.Base", 0)]
    );
    let found = definition(&mut session, &greeter, GREETER, "app.IGreeter", 0);
    assert_eq!(
        found,
        vec![at(
            &ws.path("app/IGreeter.cls"),
            IGREETER,
            "app.IGreeter",
            0
        )]
    );
}

#[test]
fn methods_go_to_their_declaration_in_the_class_that_declares_them() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let greeter = ws.path("app/Greeter.cls");
    let base = ws.path("app/Base.cls");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    session.open(&greeter, GREETER);

    let found = definition(&mut session, &main, MAIN, "Hello()", 0);
    assert_eq!(found, vec![at(&greeter, GREETER, "Hello", 0)]);
    let found = definition(&mut session, &greeter, GREETER, "Polite().", 0);
    assert_eq!(found, vec![at(&greeter, GREETER, "Polite", 1)]);
    // Inherited and overloaded: each declaration.
    let found = definition(&mut session, &greeter, GREETER, "Greet()", 0);
    assert_eq!(
        found,
        vec![at(&base, BASE, "Greet", 0), at(&base, BASE, "Greet", 1)]
    );
}

#[test]
fn tables_and_fields_go_to_the_schema() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let df = ws.path("db/sports.df");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    for (needle, occurrence) in [
        ("Customer NO-LOCK:", 0),
        ("Customer.Name", 0),
        ("Customer.\n", 0),
    ] {
        let found = definition(&mut session, &main, MAIN, needle, occurrence);
        assert_eq!(found.len(), 1, "definition of `{needle}`: {found:?}");
        starts_on_line(&found[0], &df, 0);
    }
    let field = &definition(&mut session, &main, MAIN, "Name.\n", 0);
    assert_eq!(field.len(), 1, "{field:?}");
    starts_on_line(&field[0], &df, 6);
}

#[test]
fn names_that_do_not_resolve_or_are_computed_have_no_location() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let factory = ws.path("factory.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    session.open(&factory, FACTORY);

    // `noSuchArg` sits inside a call that does resolve: the call is not its answer.
    for needle in [
        "missing.p",
        "noSuchThing",
        "noSuchArg",
        "VALUE(cProg)",
        "app.*",
    ] {
        let found = definition(&mut session, &main, MAIN, needle, 0);
        assert_eq!(found, Vec::<Value>::new(), "definition of `{needle}`");
    }
    let found = definition(&mut session, &factory, FACTORY, "DYNAMIC-NEW", 0);
    assert_eq!(found, Vec::<Value>::new());
}

#[test]
fn positions_follow_the_negotiated_utf8_encoding() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start_with(
        &ws.root,
        json!({"general": {"positionEncodings": ["utf-8", "utf-16"]}}),
    );
    session.open(&main, MAIN);
    // The line of `iCount` holds two-byte characters before it.
    let use_at = nth(MAIN, "iCount + 1", 0) + 1;
    let line_start = MAIN[..use_at].rfind('\n').map_or(0, |n| n + 1);
    let answer = session.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": file_uri(&main)},
            "position": {
                "line": MAIN[..use_at].matches('\n').count(),
                "character": use_at - line_start
            }
        }),
    );
    let declared = nth(MAIN, "iCount", 0);
    let declared_line_start = MAIN[..declared].rfind('\n').map_or(0, |n| n + 1);
    let character = declared - declared_line_start;
    assert_eq!(
        answer["result"][0]["range"]["start"],
        json!({"line": MAIN[..declared].matches('\n').count(), "character": character}),
        "{answer}"
    );
    assert_ne!(
        character as u64,
        utf16_at(MAIN, declared).1,
        "the fixture must tell the encodings apart"
    );
}

#[test]
fn like_fields_go_to_the_schema_field_also_when_it_is_named_like_its_table() {
    let ws = Workspace::new();
    let include = ws.path("deco_tt.i");
    let df = ws.path("db/sports.df");
    let mut session = Session::start(&ws.root);
    // Opened alone, as an editor does: fragment mode.
    session.open(&include, TT_INCLUDE);

    let line_of =
        |needle: &str| SCHEMA[..SCHEMA.find(needle).unwrap()].matches('\n').count() as u64;
    let cases = [
        ("company.company", 0, "ADD TABLE \"company\""),
        ("company\n", 0, "ADD FIELD \"company\""),
        ("warehouse.warehouse", 0, "ADD TABLE \"warehouse\""),
        ("warehouse\n", 0, "ADD FIELD \"warehouse\""),
        ("Name\n", 0, "ADD FIELD \"Name\""),
    ];
    for (needle, occurrence, declared) in cases {
        let found = definition(&mut session, &include, TT_INCLUDE, needle, occurrence);
        assert_eq!(
            found.len(),
            1,
            "definition of `{needle}` ({occurrence}): {found:?}"
        );
        starts_on_line(&found[0], &df, line_of(declared));
    }
    // A field the table does not have: no location, and a well-formed answer.
    let found = definition(&mut session, &include, TT_INCLUDE, "nosuch", 0);
    assert_eq!(found, Vec::<Value>::new());
}
