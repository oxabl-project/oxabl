//! `textDocument/hover` through the real `oxabl lsp` and its daemon: each kind
//! of name answers with what oxabl knows about it, as short Markdown, and a name
//! that does not resolve answers with nothing.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"hover\"\n\
    [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
    [workspace.schema]\nfiles = [\"db/shop.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Item\"\n  AREA \"Data\"\n  LABEL \"Stock item\"\n\
    \x20 DESCRIPTION \"One thing the shop sells\"\n\n\
    ADD FIELD \"ItemNum\" OF \"Item\" AS integer\n  FORMAT \">>>>9\"\n  LABEL \"Item no.\"\n\
    \x20 INITIAL \"0\"\n  MANDATORY\n\n\
    ADD FIELD \"Descr\" OF \"Item\" AS character\n  FORMAT \"x(30)\"\n  LABEL \"Description\"\n\n\
    ADD FIELD \"Price\" OF \"Item\" AS decimal\n  FORMAT \">>,>>9.99\"\n  DECIMALS 2\n\n\
    ADD INDEX \"ItemNum\" ON \"Item\"\n  AREA \"Index\"\n  UNIQUE\n  PRIMARY\n\
    \x20 INDEX-FIELD \"ItemNum\" ASCENDING\n\n\
    ADD INDEX \"DescrNum\" ON \"Item\"\n  AREA \"Index\"\n\
    \x20 INDEX-FIELD \"Descr\" ASCENDING\n  INDEX-FIELD \"ItemNum\" ASCENDING\n\n";

const DEFS: &str = "/* shared definitions */\n\
    DEFINE VARIABLE gcShared AS CHARACTER NO-UNDO INITIAL \"abc\".\n";

const MAIN: &str = "{defs.i}\n\
    DEFINE VARIABLE iCount AS INTEGER EXTENT 3 NO-UNDO INITIAL [1, 2, 3].\n\
    DEFINE VARIABLE cName LIKE Item.Descr NO-UNDO.\n\
    DEFINE VARIABLE dTotal AS DECIMAL INITIAL -1.5.\n\
    DEFINE VARIABLE oShape AS CLASS app.Circle NO-UNDO.\n\
    DEFINE BUFFER bItem FOR Item.\n\
    DEFINE TEMP-TABLE ttLine NO-UNDO\n\
    \x20   FIELD LineNo AS INTEGER INITIAL 1.\n\
    iCount[1] = LENGTH(gcShared).\n\
    cName = SUBSTRING(gcShared, 1, 2).\n\
    dTotal = fnTotal(iCount[1]).\n\
    RUN AddLine (INPUT cName, OUTPUT dTotal).\n\
    RUN util/helper.p.\n\
    RUN dup.p.\n\
    RUN missing.p.\n\
    oShape = NEW app.Circle().\n\
    dTotal = oShape:Area().\n\
    oShape:Describe().\n\
    FOR EACH Item NO-LOCK:\n\
    \x20   DISPLAY Item.Descr Item.Price.\n\
    END.\n\
    FIND FIRST bItem NO-LOCK NO-ERROR.\n\
    CREATE ttLine.\n\
    ttLine.LineNo = 2.\n\
    dTotal = noSuchThing.\n\
    def var iShort as int no-undo.\n\
    DEFINE VARIABLE hTools AS HANDLE NO-UNDO.\n\
    RUN jobs/tools.p PERSISTENT SET hTools.\n\
    RUN Compute IN hTools (INPUT 1).\n\
    MESSAGE SESSION:DATE-FORMAT.\n\
    /* a comment */\n\
    PROCEDURE AddLine:\n\
    \x20   DEFINE INPUT  PARAMETER pcName  AS CHARACTER NO-UNDO.\n\
    \x20   DEFINE OUTPUT PARAMETER pdTotal AS DECIMAL   NO-UNDO.\n\
    \x20   pdTotal = LENGTH(pcName).\n\
    END PROCEDURE.\n\
    FUNCTION fnTotal RETURNS DECIMAL (INPUT piCount AS INTEGER):\n\
    \x20   RETURN piCount * 2.\n\
    END FUNCTION.\n";

const BASE: &str = "CLASS app.Base:\n\
    \x20   METHOD PUBLIC VOID Describe():\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Describe(INPUT pcHow AS CHARACTER):\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CIRCLE: &str = "CLASS app.Circle INHERITS app.Base:\n\
    \x20   DEFINE PUBLIC PROPERTY Radius AS DECIMAL INITIAL 1 NO-UNDO\n\
    \x20       GET.\n\
    \x20       SET.\n\
    \x20   METHOD PUBLIC DECIMAL Area():\n\
    \x20       RETURN Radius * Radius * 3.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// A root that declares a name an include then uses.
const CTX: &str = "DEFINE VARIABLE iBefore AS INTEGER NO-UNDO.\n{uses.i}\n";
const USES: &str = "iBefore = 1.\n";

const TOOLS: &str = "PROCEDURE Compute:\n\
    \x20   DEFINE INPUT PARAMETER piN AS INTEGER NO-UNDO.\n\
    END PROCEDURE.\n";

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
            ("db/shop.df", SCHEMA),
            ("defs.i", DEFS),
            ("main.p", MAIN),
            ("util/helper.p", "MESSAGE \"helper\".\n"),
            ("jobs/tools.p", TOOLS),
            ("ctx.p", CTX),
            ("uses.i", USES),
            ("dup.p", "MESSAGE \"one\".\n"),
            ("lib/dup.p", "MESSAGE \"two\".\n"),
            ("app/Base.cls", BASE),
            ("app/Circle.cls", CIRCLE),
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

/// The hover for the cursor one character into occurrence `occurrence` of
/// `needle` in `file`: `None` for a `null` result. Fails on an error.
#[track_caller]
fn hover(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Option<Value> {
    let (line, character) = utf16_at(text, nth(text, needle, occurrence) + 1);
    let answer = session.request(
        "textDocument/hover",
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": {"line": line, "character": character}
        }),
    );
    assert!(
        answer.get("error").is_none(),
        "hover on `{needle}` gave an error: {}",
        answer["error"]
    );
    match &answer["result"] {
        Value::Null => None,
        result => Some(result.clone()),
    }
}

/// The Markdown of the hover on `needle`. Fails when there is none, and when
/// the text is not Markdown or names an internal identifier.
#[track_caller]
fn markdown(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> String {
    let result = hover(session, file, text, needle, occurrence)
        .unwrap_or_else(|| panic!("no hover on `{needle}`"));
    assert_eq!(
        result["contents"]["kind"],
        json!("markdown"),
        "hover on `{needle}`: {result}"
    );
    let value = result["contents"]["value"].as_str().unwrap().to_string();
    for internal in [
        "SymbolId", "NodeId", "TableId", "FileId", "DUMMY", "Some(", "None",
    ] {
        assert!(
            !value.contains(internal),
            "hover on `{needle}` names an internal identifier: {value}"
        );
    }
    value
}

/// An ABL code block.
fn code(text: &str) -> String {
    format!("```abl\n{text}\n```")
}

#[test]
fn variables_show_type_extent_initial_and_no_undo() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let cases: &[(&str, usize, String)] = &[
        (
            "iCount[1] =",
            0,
            code("DEFINE VARIABLE iCount AS INTEGER EXTENT 3 INITIAL [1, 2, 3] NO-UNDO"),
        ),
        // At the declaration itself.
        (
            "iCount AS",
            0,
            code("DEFINE VARIABLE iCount AS INTEGER EXTENT 3 INITIAL [1, 2, 3] NO-UNDO"),
        ),
        (
            "dTotal = fnTotal",
            0,
            code("DEFINE VARIABLE dTotal AS DECIMAL INITIAL -1.5"),
        ),
        (
            "iShort as",
            0,
            code("DEFINE VARIABLE iShort AS INTEGER NO-UNDO"),
        ),
        (
            "cName = SUBSTRING",
            0,
            code("DEFINE VARIABLE cName LIKE Item.Descr NO-UNDO")
                + "\n\n`LIKE Item.Descr` is `CHARACTER`.",
        ),
        (
            "LineNo = 2",
            0,
            code("FIELD LineNo AS INTEGER INITIAL 1") + "\n\nField of temp-table `ttLine`.",
        ),
    ];
    for (needle, occurrence, expected) in cases {
        let found = markdown(&mut session, &main, MAIN, needle, *occurrence);
        assert_eq!(&found, expected, "hover on `{needle}`");
    }
}

#[test]
fn parameters_show_their_direction_and_owner() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = markdown(&mut session, &main, MAIN, "pcName)", 0);
    assert_eq!(
        found,
        code("DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO")
            + "\n\nParameter of `PROCEDURE AddLine`."
    );
    let found = markdown(&mut session, &main, MAIN, "piCount * 2", 0);
    assert_eq!(
        found,
        code("INPUT piCount AS INTEGER") + "\n\nParameter of `FUNCTION fnTotal`."
    );
}

#[test]
fn routines_show_their_signature() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = markdown(&mut session, &main, MAIN, "AddLine (", 0);
    assert_eq!(
        found,
        code("PROCEDURE AddLine (INPUT pcName AS CHARACTER, OUTPUT pdTotal AS DECIMAL)")
    );
    let found = markdown(&mut session, &main, MAIN, "fnTotal(", 0);
    assert_eq!(
        found,
        code("FUNCTION fnTotal RETURNS DECIMAL (INPUT piCount AS INTEGER)")
    );
    // A method of the variable's class.
    let found = markdown(&mut session, &main, MAIN, "Area()", 0);
    assert_eq!(
        found,
        code("METHOD PUBLIC DECIMAL Area") + "\n\nDeclared in `app.Circle`."
    );
    // An inherited method shows each overload, in the class that declares it.
    let found = markdown(&mut session, &main, MAIN, "Describe()", 0);
    assert_eq!(
        found,
        code("METHOD PUBLIC VOID Describe")
            + "\n\n"
            + &code("METHOD PUBLIC VOID Describe (INPUT pcHow AS CHARACTER)")
            + "\n\nDeclared in `app.Base`."
    );
    // A class name in a type position.
    let found = markdown(&mut session, &main, MAIN, "Circle().", 0);
    assert_eq!(found, code("CLASS app.Circle INHERITS app.Base"));
    // A program a RUN names.
    let found = markdown(&mut session, &main, MAIN, "util/helper.p", 0);
    assert_eq!(found, code("RUN util/helper.p"));
    // An internal procedure of the program a handle runs.
    let found = markdown(&mut session, &main, MAIN, "Compute IN", 0);
    assert_eq!(
        found,
        code("PROCEDURE Compute (INPUT piN AS INTEGER)") + "\n\nIn `jobs/tools.p`."
    );
}

#[test]
fn a_property_shows_its_declaration() {
    let ws = Workspace::new();
    let circle = ws.path("app/Circle.cls");
    let mut session = Session::start(&ws.root);
    session.open(&circle, CIRCLE);

    let found = markdown(&mut session, &circle, CIRCLE, "Radius *", 0);
    assert_eq!(
        found,
        code("DEFINE PUBLIC PROPERTY Radius AS DECIMAL INITIAL 1 NO-UNDO")
    );
}

#[test]
fn built_in_functions_and_statements_link_to_the_documentation() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let docs = "https://docs-be.progress.com/bundle/abl-reference/page/";
    let cases: &[(&str, usize, &str, &str, &str)] = &[
        // (needle, occurrence, title, kind, page)
        (
            "SUBSTRING(",
            0,
            "SUBSTRING function",
            "built-in function",
            "SUBSTRING-function.html",
        ),
        (
            "LENGTH(gc",
            0,
            "LENGTH function",
            "built-in function",
            "LENGTH-function.html",
        ),
        (
            "DEFINE BUFFER",
            0,
            "DEFINE BUFFER statement",
            "statement",
            "DEFINE-BUFFER-statement.html",
        ),
        // A later word of a statement's head, and the words between.
        (
            "TEMP-TABLE ttLine",
            0,
            "DEFINE TEMP-TABLE statement",
            "statement",
            "DEFINE-TEMP-TABLE-statement.html",
        ),
        (
            "PARAMETER pcName",
            0,
            "DEFINE PARAMETER statement",
            "statement",
            "DEFINE-PARAMETER-statement.html",
        ),
        // Abbreviated keywords.
        (
            "def var",
            0,
            "DEFINE VARIABLE statement",
            "statement",
            "DEFINE-VARIABLE-statement.html",
        ),
        (
            "FOR EACH",
            0,
            "FOR statement",
            "statement",
            "FOR-statement.html",
        ),
        (
            "FIND FIRST",
            0,
            "FIND statement",
            "statement",
            "FIND-statement.html",
        ),
        (
            "DISPLAY Item",
            0,
            "DISPLAY statement",
            "statement",
            "DISPLAY-statement.html",
        ),
        (
            "RUN AddLine",
            0,
            "RUN statement",
            "statement",
            "RUN-statement.html",
        ),
        (
            "SESSION:",
            0,
            "SESSION system handle",
            "system handle",
            "SESSION-system-handle.html",
        ),
    ];
    for (needle, occurrence, title, kind, page) in cases {
        let found = markdown(&mut session, &main, MAIN, needle, *occurrence);
        assert_eq!(
            found,
            format!("**{title}**\n\nAn ABL {kind}. [Progress documentation]({docs}{page})"),
            "hover on `{needle}`"
        );
    }
}

#[test]
fn tables_and_fields_come_from_the_schema() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let table = code("TABLE Item")
        + "\n\nLabel: `Stock item` · Fields: 3\n\nOne thing the shop sells\n\nIndexes:\n\
           - `ItemNum` (primary, unique): ItemNum\n\
           - `DescrNum`: Descr, ItemNum";
    let found = markdown(&mut session, &main, MAIN, "Item NO-LOCK", 0);
    assert_eq!(found, table, "a table in FOR EACH");
    let found = markdown(&mut session, &main, MAIN, "Item.\n", 0);
    assert_eq!(found, table, "the table of DEFINE BUFFER ... FOR");

    let found = markdown(&mut session, &main, MAIN, "Descr Item.Price", 0);
    assert_eq!(
        found,
        code("FIELD Item.Descr AS CHARACTER")
            + "\n\nFormat: `x(30)` · Label: `Description`\n\nIndexes:\n\
               - `DescrNum`: Descr, ItemNum"
    );
    let found = markdown(&mut session, &main, MAIN, "Price.", 0);
    assert_eq!(
        found,
        code("FIELD Item.Price AS DECIMAL") + "\n\nFormat: `>>,>>9.99`\n\nIn no index."
    );
    // A buffer with its own name is the buffer, not the table.
    let found = markdown(&mut session, &main, MAIN, "bItem NO-LOCK", 0);
    assert_eq!(found, code("DEFINE BUFFER bItem FOR Item"));
}

#[test]
fn a_name_declared_in_an_include_shows_its_declaration() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let defs = ws.path("defs.i");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    session.open(&defs, DEFS);

    let expected = code("DEFINE VARIABLE gcShared AS CHARACTER INITIAL \"abc\" NO-UNDO");
    let found = markdown(&mut session, &main, MAIN, "gcShared)", 0);
    assert_eq!(found, expected, "a use in the root");
    let found = markdown(&mut session, &defs, DEFS, "gcShared", 0);
    assert_eq!(found, expected, "the declaration in the include");
}

#[test]
fn an_include_in_a_root_context_resolves_through_the_root() {
    let ws = Workspace::new();
    let uses = ws.path("uses.i");
    let mut session = Session::start(&ws.root);
    session.open(&uses, USES);

    // Alone, the include cannot know the name.
    assert_eq!(hover(&mut session, &uses, USES, "iBefore", 0), None);

    let selected = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&uses)}, "root": ws.path("ctx.p")}),
    );
    assert!(selected.get("error").is_none(), "{selected}");
    let found = markdown(&mut session, &uses, USES, "iBefore", 0);
    assert_eq!(found, code("DEFINE VARIABLE iBefore AS INTEGER NO-UNDO"));
}

#[test]
fn a_name_that_does_not_resolve_has_no_hover() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    for (needle, why) in [
        ("noSuchThing", "an undefined name"),
        ("missing.p", "a program no file supplies"),
        ("dup.p", "a program two files supply"),
        ("a comment", "a comment"),
        ("{defs.i}", "an include reference"),
    ] {
        let found = hover(&mut session, &main, MAIN, needle, 0);
        assert_eq!(found, None, "{why} (`{needle}`) must have no hover");
    }
}

#[test]
fn the_range_is_the_hovered_word() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let result = hover(&mut session, &main, MAIN, "fnTotal(", 0).expect("a hover on fnTotal");
    let start = nth(MAIN, "fnTotal(", 0);
    let (line, character) = utf16_at(MAIN, start);
    assert_eq!(
        result["range"],
        json!({
            "start": {"line": line, "character": character},
            "end": {"line": line, "character": character + "fnTotal".len() as u64}
        })
    );
}
