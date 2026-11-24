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
    [workspace.schema]\nfiles = [\"db/shop.df\", \"db/extra.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Item\"\n  AREA \"Data\"\n  LABEL \"Stock item\"\n\
    \x20 DESCRIPTION \"One thing the shop sells\"\n\n\
    ADD FIELD \"ItemNum\" OF \"Item\" AS integer\n  FORMAT \">>>>9\"\n  LABEL \"Item no.\"\n\
    \x20 INITIAL \"0\"\n  MANDATORY\n\n\
    ADD FIELD \"Descr\" OF \"Item\" AS character\n  FORMAT \"x(30)\"\n  LABEL \"Description\"\n\n\
    ADD FIELD \"Price\" OF \"Item\" AS decimal\n  FORMAT \">>,>>9.99\"\n  DECIMALS 2\n\n\
    ADD INDEX \"ItemNum\" ON \"Item\"\n  AREA \"Index\"\n  UNIQUE\n  PRIMARY\n\
    \x20 INDEX-FIELD \"ItemNum\" ASCENDING\n\n\
    ADD INDEX \"DescrNum\" ON \"Item\"\n  AREA \"Index\"\n\
    \x20 INDEX-FIELD \"Descr\" ASCENDING\n  INDEX-FIELD \"ItemNum\" ASCENDING\n\n\
    ADD TABLE \"Ledger\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"Code\" OF \"Ledger\" AS character\n  LABEL \"``a`b\"\n\n\
    ADD FIELD \"Poison\" OF \"Ledger\" AS character\n\n";

/// A second `.df` that gives `Ledger.Poison` another type, so the loader
/// knows no type for it.
const EXTRA_SCHEMA: &str = "ADD FIELD \"Poison\" OF \"Ledger\" AS integer\n\n";

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
    FIND FIRST Ledger NO-LOCK NO-ERROR.\n\
    MESSAGE Ledger.Code Ledger.Poison.\n\
    DEFINE VARIABLE cFence AS CHARACTER INITIAL \"```\" NO-UNDO.\n\
    cFence = \"x\".\n\
    dTotal = fnSup(1).\n\
    RUN LoadLines (INPUT TABLE ttLine BIND).\n\
    oShape:Tag = \"x\".\n\
    oShape:Size = 1.\n\
    FUNCTION fnSup RETURNS INTEGER (INPUT p AS INTEGER) IN SUPER.\n\
    PROCEDURE LoadLines:\n\
    \x20   DEFINE INPUT PARAMETER TABLE FOR ttLine BIND.\n\
    END PROCEDURE.\n\
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
    \x20   DEFINE PUBLIC PROPERTY Tag AS CHARACTER NO-UNDO\n\
    \x20       GET.\n\
    \x20       PRIVATE SET.\n\
    \x20   DEFINE PUBLIC PROPERTY Size AS &IF DEFINED(WIDE) &THEN INT64 &ELSE INTEGER &ENDIF NO-UNDO\n\
    \x20       GET.\n\
    \x20       SET.\n\
    \x20   DEFINE PUBLIC PROPERTY Radius AS DECIMAL INITIAL 1 NO-UNDO\n\
    \x20       GET.\n\
    \x20       SET.\n\
    \x20   METHOD PUBLIC DECIMAL Area():\n\
    \x20       RETURN Radius * Radius * 3.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// An include opened alone that uses names of its includer.
const FRAG: &str = "interval = 1.\ncompare = 2.\ncText = SUBSTRING(cText, 1, 2).\n\
    i = LOOKUP(\"a\", cText).\n";

/// A class whose ancestor no file supplies, using names that the ancestor may
/// declare.
const OPAQUE: &str = "CLASS app.Opaque INHERITS app.Missing:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20       DEFINE VARIABLE aList AS INTEGER EXTENT 3 NO-UNDO.\n\
    \x20       interval = Extent(aList).\n\
    \x20       interval = 1.\n\
    \x20       compare = 2.\n\
    \x20       interval = SUBSTRING(\"ab\", 1, 1).\n\
    \x20       compare = Compare(1, \"=\", 2, \"CASE-SENSITIVE\").\n\
    \x20       interval = LOOKUP(\"a\", \"a,b\").\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// Classes whose ancestors were all read, so no inherited method hides a
/// built-in function.
const KNOWN: &str = "CLASS app.Known INHERITS app.Circle:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20       DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    \x20       c = SUBSTRING(\"ab\", 1, 1).\n\
    \x20       c = STRING(Compare(1, \"=\", 2, \"CASE-SENSITIVE\")).\n\
    \x20   END METHOD.\n\
    END CLASS.\n";
const PLAIN: &str = "USING Progress.Lang.*.\n\
    CLASS app.Plain INHERITS Object:\n\
    \x20   METHOD PUBLIC VOID M():\n\
    \x20       DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
    \x20       c = SUBSTRING(\"ab\", 1, 1).\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// A program with an include that no file supplies, which may declare a
/// function named like a built-in.
const MISSING_INCLUDE: &str = "{missing.i}\n\
    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
    i = Compare(1, \"=\", 2, \"CASE-SENSITIVE\").\n\
    i = LOOKUP(\"a\", \"a,b\").\n";

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
            ("db/extra.df", EXTRA_SCHEMA),
            ("defs.i", DEFS),
            ("main.p", MAIN),
            ("util/helper.p", "MESSAGE \"helper\".\n"),
            ("jobs/tools.p", TOOLS),
            ("ctx.p", CTX),
            ("frag.i", FRAG),
            ("uses.i", USES),
            ("dup.p", "MESSAGE \"one\".\n"),
            ("lib/dup.p", "MESSAGE \"two\".\n"),
            ("app/Base.cls", BASE),
            ("app/Circle.cls", CIRCLE),
            ("app/Known.cls", KNOWN),
            ("app/Plain.cls", PLAIN),
            ("miss.p", MISSING_INCLUDE),
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

/// What a hover adds for a routine that touches no table
/// (`hover_effects_lsp.rs` tests the effects).
const NO_EFFECTS: &str = "\n\n---\n\n**Database effects**\n\nNo database effects.";

/// The same for a method that a subclass may override.
const OVERRIDABLE_NO_EFFECTS: &str = "\n\n---\n\n**Database effects**\n\nNo known database effects.\n\n\
    **Incomplete:** effects may be missing: an override in a subclass may run instead of this method.";

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
            + NO_EFFECTS
    );
    let found = markdown(&mut session, &main, MAIN, "fnTotal(", 0);
    assert_eq!(
        found,
        code("FUNCTION fnTotal RETURNS DECIMAL (INPUT piCount AS INTEGER)") + NO_EFFECTS
    );
    // A method of the variable's class.
    let found = markdown(&mut session, &main, MAIN, "Area()", 0);
    assert_eq!(
        found,
        code("METHOD PUBLIC DECIMAL Area ()")
            + "\n\nDeclared in `app.Circle`."
            + OVERRIDABLE_NO_EFFECTS
    );
    // An inherited method shows each overload, in the class that declares it.
    let found = markdown(&mut session, &main, MAIN, "Describe()", 0);
    assert_eq!(
        found,
        code("METHOD PUBLIC VOID Describe ()")
            + "\n\n"
            + &code("METHOD PUBLIC VOID Describe (INPUT pcHow AS CHARACTER)")
            + "\n\nDeclared in `app.Base`."
    );
    // A class name in a type position.
    let found = markdown(&mut session, &main, MAIN, "Circle().", 0);
    assert_eq!(found, code("CLASS app.Circle INHERITS app.Base"));
    // A program a RUN names.
    let found = markdown(&mut session, &main, MAIN, "util/helper.p", 0);
    assert_eq!(found, code("RUN util/helper.p") + NO_EFFECTS);
    // An internal procedure of the program a handle runs.
    let found = markdown(&mut session, &main, MAIN, "Compute IN", 0);
    assert_eq!(
        found,
        code("PROCEDURE Compute (INPUT piN AS INTEGER)") + "\n\nIn `jobs/tools.p`." + NO_EFFECTS
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
        code("DEFINE PUBLIC PROPERTY Radius AS DECIMAL INITIAL 1 NO-UNDO GET. SET.")
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
fn qualifiers_that_change_a_call_are_kept() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = markdown(&mut session, &main, MAIN, "fnSup(1)", 0);
    assert_eq!(
        found,
        code("FUNCTION fnSup RETURNS INTEGER (INPUT p AS INTEGER) IN SUPER")
    );
    let found = markdown(&mut session, &main, MAIN, "LoadLines (", 0);
    assert!(
        found.starts_with(&code("PROCEDURE LoadLines (INPUT TABLE FOR ttLine BIND)")),
        "{found}"
    );
    let found = markdown(&mut session, &main, MAIN, "Tag = ", 0);
    assert_eq!(
        found,
        code("DEFINE PUBLIC PROPERTY Tag AS CHARACTER NO-UNDO GET. PRIVATE SET.")
            + "\n\nDeclared in `app.Circle`."
    );
}

#[test]
fn a_type_the_preprocessor_chooses_is_not_shown_as_a_type() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = markdown(&mut session, &main, MAIN, "Size = ", 0);
    assert_eq!(
        found,
        code("DEFINE PUBLIC PROPERTY Size NO-UNDO GET. SET.")
            + "\n\nAn `&IF` chooses the type."
            + "\n\nDeclared in `app.Circle`."
    );
    // A schema field whose two `.df` files disagree on its type.
    let found = markdown(&mut session, &main, MAIN, "Poison.", 0);
    assert_eq!(
        found,
        code("FIELD Ledger.Poison") + "\n\nType: unknown\n\nIn no index."
    );
}

#[test]
fn backticks_in_source_and_schema_text_do_not_break_the_markdown() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = markdown(&mut session, &main, MAIN, "cFence = ", 0);
    assert_eq!(
        found,
        "````abl\nDEFINE VARIABLE cFence AS CHARACTER INITIAL \"```\" NO-UNDO\n````"
    );
    let found = markdown(&mut session, &main, MAIN, "Code Ledger", 0);
    assert_eq!(
        found,
        code("FIELD Ledger.Code AS CHARACTER") + "\n\nLabel: ``` ``a`b ```\n\nIn no index."
    );
}

#[test]
fn an_include_alone_does_not_guess_built_in_functions() {
    let ws = Workspace::new();
    let frag = ws.path("frag.i");
    let mut session = Session::start(&ws.root);
    session.open(&frag, FRAG);

    // Names of the includer that are also built-in function names.
    for needle in ["interval =", "compare ="] {
        assert_eq!(
            hover(&mut session, &frag, FRAG, needle, 0),
            None,
            "`{needle}` may be a variable of the includer"
        );
    }
    // A call of an unreserved name may be a function of the includer.
    assert_eq!(hover(&mut session, &frag, FRAG, "SUBSTRING(", 0), None);
    // A reserved name is still the built-in function.
    let found = markdown(&mut session, &frag, FRAG, "LOOKUP(", 0);
    assert!(found.starts_with("**LOOKUP function**"), "{found}");
}

#[test]
fn a_class_with_an_unread_ancestor_does_not_guess_built_in_functions() {
    let ws = Workspace::new();
    let opaque = ws.path("app/Opaque.cls");
    std::fs::write(&opaque, OPAQUE).unwrap();
    let mut session = Session::start(&ws.root);
    session.open(&opaque, OPAQUE);

    // Names an ancestor that no file supplies may declare: a property, or a
    // method with the name of an unreserved built-in function.
    for needle in [
        "interval =",
        "compare =",
        "Compare(",
        "SUBSTRING(",
        "Extent(",
    ] {
        assert_eq!(
            hover(&mut session, &opaque, OPAQUE, needle, 0),
            None,
            "`{needle}` may be a member of the unread ancestor"
        );
    }
    // A reserved name is no routine's.
    let found = markdown(&mut session, &opaque, OPAQUE, "LOOKUP(", 0);
    assert!(found.starts_with("**LOOKUP function**"), "{found}");
}

#[test]
fn a_class_whose_ancestors_were_read_shows_built_in_functions() {
    let ws = Workspace::new();
    let mut session = Session::start(&ws.root);
    for (file, text, needles) in [
        ("app/Known.cls", KNOWN, &["SUBSTRING(", "Compare("][..]),
        ("app/Plain.cls", PLAIN, &["SUBSTRING("][..]),
    ] {
        let path = ws.path(file);
        session.open(&path, text);
        for needle in needles {
            let found = markdown(&mut session, &path, text, needle, 0);
            let title = format!(
                "**{} function**",
                needle.trim_end_matches('(').to_uppercase()
            );
            assert!(found.starts_with(&title), "{file} `{needle}`: {found}");
        }
    }
}

#[test]
fn a_program_with_an_unread_include_does_not_guess_built_in_functions() {
    let ws = Workspace::new();
    let miss = ws.path("miss.p");
    let mut session = Session::start(&ws.root);
    session.open(&miss, MISSING_INCLUDE);

    assert_eq!(
        hover(&mut session, &miss, MISSING_INCLUDE, "Compare(", 0),
        None,
        "the missing include may declare `Compare`"
    );
    let found = markdown(&mut session, &miss, MISSING_INCLUDE, "LOOKUP(", 0);
    assert!(found.starts_with("**LOOKUP function**"), "{found}");
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
