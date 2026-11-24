//! `textDocument/documentHighlight` through the real `oxabl lsp` and its daemon:
//! the uses of the name under the cursor in the file, each a read or a write,
//! from the resolution that go to definition uses. A name that does not resolve
//! has none, and text that only spells the name is not a use.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"highlight\"\n\
    [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
    [workspace.schema]\nfiles = [\"db/sports.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n  FORMAT \">>>>9\"\n\n\
    ADD FIELD \"Name\" OF \"Customer\" AS character\n  FORMAT \"x(30)\"\n\n";

const DEFS: &str = "DEFINE VARIABLE gcShared AS CHARACTER NO-UNDO.\n\
    gcShared = gcShared + \"x\".\n";

const MAIN: &str = "/* Größe iCount */ DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.\n\
    DEFINE BUFFER bCust FOR Customer.\n\
    {defs.i}\n\
    iCount = 1.\n\
    iCount = iCount + 1.\n\
    MESSAGE iCount \"iCount\".\n\
    gcShared = \"root\".\n\
    RUN Twice.\n\
    RUN Twice (OUTPUT cText).\n\
    FOR EACH Customer NO-LOCK:\n\
    \x20   DISPLAY Customer.Name.\n\
    END.\n\
    FIND FIRST bCust NO-LOCK NO-ERROR.\n\
    DISPLAY bCust.Name.\n\
    iCount = noSuchThing.\n\
    MESSAGE noSuchThing.\n\
    iCount = Double(iCount).\n\
    PROCEDURE Twice:\n\
    \x20   DEFINE OUTPUT PARAMETER pcName AS CHARACTER NO-UNDO.\n\
    \x20   pcName = \"x\".\n\
    END PROCEDURE.\n\
    PROCEDURE Other:\n\
    \x20   DEFINE INPUT PARAMETER pcName AS CHARACTER NO-UNDO.\n\
    \x20   MESSAGE pcName.\n\
    END PROCEDURE.\n\
    FUNCTION Double RETURNS INTEGER (INPUT piValue AS INTEGER):\n\
    \x20   RETURN piValue * 2.\n\
    END FUNCTION.\n";

/// One include that declares a local variable, read in two procedures.
const LOCAL_INCLUDE: &str = "DEFINE VARIABLE iLocal AS INTEGER NO-UNDO.\n";

const SCOPES: &str = "PROCEDURE a:\n\
    \x20   {loc.i}\n\
    \x20   iLocal = 1.\n\
    END PROCEDURE.\n\
    PROCEDURE b:\n\
    \x20   {loc.i}\n\
    \x20   iLocal = 2.\n\
    END PROCEDURE.\n";

/// A program that two entries of the include path supply, and one that only
/// one supplies.
const AMBIGUOUS: &str = "RUN dup.p.\nRUN dup.p.\nRUN once.p.\nRUN once.p.\n";

/// A buffer the code declares, named like its table, next to the table's own
/// default buffer.
const BUFFERS: &str = "FIND FIRST Customer NO-LOCK.\n\
    DISPLAY Customer.Name.\n\
    PROCEDURE c:\n\
    \x20   DEFINE BUFFER Customer FOR Customer.\n\
    \x20   FIND FIRST Customer NO-LOCK.\n\
    \x20   DISPLAY Customer.Name.\n\
    END PROCEDURE.\n";

/// `FOR EACH` of a database table uses the table's default buffer, as the
/// `FIND` before it and the `DISPLAY` after it do.
const LOOPS: &str = "FIND FIRST Customer NO-LOCK.\n\
    FOR EACH Customer NO-LOCK:\n\
    \x20   DISPLAY Customer.Name.\n\
    END.\n\
    DISPLAY Customer.Name.\n\
    FOR EACH Customer NO-LOCK: END.\n";

/// A program whose variable an include declares only in its unsaved buffer.
const UNSAVED_MAIN: &str = "{unsaved.i}\nb = 1.\nb = b + 1.\n";

const KINDS: &str = "DEFINE TEMP-TABLE ttOrder NO-UNDO\n\
    \x20   FIELD OrderId AS INTEGER\n\
    \x20   FIELD Qty AS INTEGER.\n\
    DEFINE VARIABLE iA AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE iB AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE oGreeter AS CLASS app.Greeter NO-UNDO.\n\
    CREATE ttOrder.\n\
    ttOrder.OrderId = 1.\n\
    ASSIGN ttOrder.Qty = ttOrder.OrderId\n\
    \x20      iA = iB.\n\
    iB = ttOrder.Qty.\n\
    oGreeter = NEW app.Greeter().\n\
    oGreeter:Hello().\n\
    oGreeter:Hello().\n\
    oGreeter:Greet().\n\
    oGreeter:Greet().\n\
    DELETE ttOrder.\n";

const GREETER: &str = "CLASS app.Greeter INHERITS app.Base:\n\
    \x20   METHOD PUBLIC VOID Hello():\n\
    \x20       Greet().\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const BASE: &str = "CLASS app.Base:\n\
    \x20   METHOD PUBLIC VOID Greet():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

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
            ("main.p", MAIN),
            ("kinds.p", KINDS),
            ("loc.i", LOCAL_INCLUDE),
            ("scopes.p", SCOPES),
            ("ambiguous.p", AMBIGUOUS),
            ("dup.p", "MESSAGE \"one\".\n"),
            ("lib/dup.p", "MESSAGE \"two\".\n"),
            ("once.p", "MESSAGE \"once\".\n"),
            ("buffers.p", BUFFERS),
            ("loops.p", LOOPS),
            ("unsaved.p", UNSAVED_MAIN),
            ("unsaved.i", "/* nothing yet */\n"),
            ("app/Greeter.cls", GREETER),
            ("app/Base.cls", BASE),
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
    (
        line as u64,
        text[line_start..at].encode_utf16().count() as u64,
    )
}

/// The byte of the `nth` (0-based) occurrence of `needle` in `text`.
#[track_caller]
fn nth(text: &str, needle: &str, nth: usize) -> usize {
    text.match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("no occurrence {nth} of `{needle}`"))
        .0
}

/// The text of the UTF-16 range `range` of `text`.
fn slice(text: &str, range: &Value) -> String {
    let line = |value: &Value| value["line"].as_u64().unwrap() as usize;
    let character = |value: &Value| value["character"].as_u64().unwrap() as usize;
    let (start, end) = (&range["start"], &range["end"]);
    assert_eq!(line(start), line(end), "a name is on one line: {range}");
    let line_text = text.lines().nth(line(start)).unwrap();
    let units: Vec<u16> = line_text.encode_utf16().collect();
    String::from_utf16(&units[character(start)..character(end)]).unwrap()
}

/// What a highlight shows: its line, its text and `R` or `W`.
type Use = (u64, String, char);

/// The uses `textDocument/documentHighlight` gives for the cursor one
/// character into occurrence `occurrence` of `needle` in `file`, as
/// `(line, text, kind)`. Fails on an error. Empty for a `null` answer.
#[track_caller]
fn highlights(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Vec<Use> {
    highlights_at(session, file, text, nth(text, needle, occurrence) + 1)
}

/// [`highlights`] for the cursor at byte `at` of `text`.
#[track_caller]
fn highlights_at(session: &mut Session, file: &Path, text: &str, at: usize) -> Vec<Use> {
    let needle = &text[at..text.len().min(at + 12)];
    let (line, character) = utf16_at(text, at);
    let answer = session.request(
        "textDocument/documentHighlight",
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": {"line": line, "character": character}
        }),
    );
    assert!(
        answer.get("error").is_none(),
        "highlights of `{needle}` gave an error: {}",
        answer["error"]
    );
    match &answer["result"] {
        Value::Null => Vec::new(),
        Value::Array(found) => found
            .iter()
            .map(|highlight| {
                let kind = match highlight["kind"].as_u64() {
                    Some(2) => 'R',
                    Some(3) => 'W',
                    other => panic!("a highlight has the kind Read or Write, not {other:?}"),
                };
                let range = &highlight["range"];
                (
                    range["start"]["line"].as_u64().unwrap(),
                    slice(text, range),
                    kind,
                )
            })
            .collect(),
        other => panic!("highlights of `{needle}` gave {other}"),
    }
}

fn expected(uses: &[(u64, &str, char)]) -> Vec<Use> {
    uses.iter()
        .map(|(line, text, kind)| (*line, text.to_string(), *kind))
        .collect()
}

#[test]
fn a_variable_shows_its_reads_and_writes() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let all = expected(&[
        (0, "iCount", 'W'),
        (4, "iCount", 'W'),
        (5, "iCount", 'W'),
        (5, "iCount", 'R'),
        (6, "iCount", 'R'),
        (15, "iCount", 'W'),
        (17, "iCount", 'W'),
        (17, "iCount", 'R'),
    ]);
    // The same answer from the declaration, a write, a read and a call argument.
    // The comment and the string that spell the name are not uses.
    for (needle, occurrence) in [
        ("iCount AS", 0),
        ("iCount = 1", 0),
        ("iCount + 1", 0),
        ("iCount \"", 0),
        ("iCount)", 0),
    ] {
        assert_eq!(
            highlights(&mut session, &main, MAIN, needle, occurrence),
            all,
            "highlights of `{needle}`"
        );
    }
}

#[test]
fn the_end_of_a_name_still_means_the_name() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let (line, character) = utf16_at(MAIN, nth(MAIN, "iCount + 1", 0) + "iCount".len());
    let answer = session.request(
        "textDocument/documentHighlight",
        json!({
            "textDocument": {"uri": file_uri(&main)},
            "position": {"line": line, "character": character}
        }),
    );
    assert_eq!(
        answer["result"].as_array().map(Vec::len),
        Some(8),
        "{answer}"
    );
}

#[test]
fn a_buffer_shows_where_it_is_named() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = highlights(&mut session, &main, MAIN, "bCust NO-LOCK", 0);
    let lines: Vec<u64> = found.iter().map(|(line, _, _)| *line).collect();
    assert_eq!(lines, vec![2, 13, 14], "{found:?}");
    assert!(found.iter().all(|(_, text, _)| text == "bCust"));
}

#[test]
fn a_field_shows_its_uses_through_every_buffer() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = highlights(&mut session, &main, MAIN, "Name.\n", 0);
    assert_eq!(
        found,
        expected(&[(11, "Name", 'R'), (14, "Name", 'R')]),
        "the field, not the buffer in front of it"
    );
    // The qualifier is the table, and it names the table's default buffer,
    // which `FOR EACH` uses.
    let table = expected(&[
        (2, "Customer", 'R'),
        (10, "Customer", 'R'),
        (11, "Customer", 'R'),
    ]);
    for needle in ["Customer.Name", "Customer.\n", "Customer NO-LOCK"] {
        let found = highlights(&mut session, &main, MAIN, needle, 0);
        assert_eq!(found, table, "highlights of `{needle}`");
    }
}

#[test]
fn the_buffer_of_for_each_is_the_default_buffer_of_its_table() {
    let ws = Workspace::new();
    let loops = ws.path("loops.p");
    let mut session = Session::start(&ws.root);
    session.open(&loops, LOOPS);

    let all = expected(&[
        (0, "Customer", 'R'),
        (1, "Customer", 'R'),
        (2, "Customer", 'R'),
        (4, "Customer", 'R'),
        (5, "Customer", 'R'),
    ]);
    for (needle, occurrence) in [
        ("Customer NO-LOCK", 0),
        ("Customer NO-LOCK", 1),
        ("Customer NO-LOCK", 2),
        ("Customer.Name", 0),
        ("Customer.Name", 1),
    ] {
        let found = highlights(&mut session, &loops, LOOPS, needle, occurrence);
        assert_eq!(found, all, "highlights of `{needle}` #{occurrence}");
    }
}

#[test]
fn a_procedure_called_twice_shows_both_calls_and_its_declaration() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let all = expected(&[(8, "Twice", 'R'), (9, "Twice", 'R'), (18, "Twice", 'W')]);
    for (needle, occurrence) in [("Twice.", 0), ("Twice (", 0), ("Twice:", 0)] {
        assert_eq!(
            highlights(&mut session, &main, MAIN, needle, occurrence),
            all,
            "highlights of `{needle}`"
        );
    }
}

#[test]
fn an_output_argument_is_a_write() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = highlights(&mut session, &main, MAIN, "cText)", 0);
    assert_eq!(found, expected(&[(1, "cText", 'W'), (9, "cText", 'W')]));
}

#[test]
fn a_function_shows_its_call_and_declaration() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = highlights(&mut session, &main, MAIN, "Double(iCount)", 0);
    assert_eq!(found, expected(&[(17, "Double", 'R'), (26, "Double", 'W')]));
}

#[test]
fn a_name_in_another_scope_is_another_name() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let found = highlights(&mut session, &main, MAIN, "pcName AS", 0);
    assert_eq!(found, expected(&[(19, "pcName", 'W'), (20, "pcName", 'W')]));
    let found = highlights(&mut session, &main, MAIN, "pcName.", 0);
    assert_eq!(found, expected(&[(23, "pcName", 'W'), (24, "pcName", 'R')]));
}

#[test]
fn a_name_that_does_not_resolve_has_no_highlight_and_no_text_match() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    // Two spellings of one unknown name: neither shows the other.
    assert_eq!(highlights(&mut session, &main, MAIN, "noSuchThing.", 0), []);
    assert_eq!(highlights(&mut session, &main, MAIN, "noSuchThing.", 1), []);
    // A keyword, a string, a comment and a blank give none.
    assert_eq!(highlights(&mut session, &main, MAIN, "NO-UNDO", 0), []);
    assert_eq!(highlights(&mut session, &main, MAIN, "\"root\"", 0), []);
    assert_eq!(highlights(&mut session, &main, MAIN, "Größe", 0), []);
}

#[test]
fn a_name_from_an_include_shows_its_uses_in_the_file_under_the_cursor() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let defs = ws.path("defs.i");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    session.open(&defs, DEFS);

    // In the program: the uses written in the program, not those of the include.
    let found = highlights(&mut session, &main, MAIN, "gcShared = \"root\"", 0);
    assert_eq!(found, expected(&[(7, "gcShared", 'W')]));
    // In the include: its own uses, the declaration among them.
    let found = highlights(&mut session, &defs, DEFS, "gcShared + ", 0);
    assert_eq!(
        found,
        expected(&[
            (0, "gcShared", 'W'),
            (1, "gcShared", 'W'),
            (1, "gcShared", 'R'),
        ])
    );
}

#[test]
fn a_name_in_an_include_with_a_selected_root_shows_its_uses_in_the_include() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let defs = ws.path("defs.i");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    session.open(&defs, DEFS);
    let selected = session.request(
        "oxabl/selectIncluderContext",
        json!({
            "textDocument": {"uri": file_uri(&defs)},
            "root": main.canonicalize().unwrap()
        }),
    );
    assert!(selected.get("error").is_none(), "{selected}");

    let found = highlights(&mut session, &defs, DEFS, "gcShared + ", 0);
    assert_eq!(
        found,
        expected(&[
            (0, "gcShared", 'W'),
            (1, "gcShared", 'W'),
            (1, "gcShared", 'R'),
        ])
    );
}

#[test]
fn each_statement_reads_or_writes_what_it_names() {
    let ws = Workspace::new();
    let kinds = ws.path("kinds.p");
    let mut session = Session::start(&ws.root);
    session.open(&kinds, KINDS);
    let mut at = |needle: &str| highlights(&mut session, &kinds, KINDS, needle, 0);

    // A plain and an ASSIGN target write; an operand reads.
    assert_eq!(
        at("iB AS"),
        expected(&[(4, "iB", 'W'), (9, "iB", 'R'), (10, "iB", 'W')])
    );
    assert_eq!(at("iA AS"), expected(&[(3, "iA", 'W'), (9, "iA", 'W')]));
    // A temp-table field, in the declaration and through the table name.
    assert_eq!(
        at("OrderId AS"),
        expected(&[
            (1, "OrderId", 'W'),
            (7, "OrderId", 'W'),
            (8, "OrderId", 'R')
        ])
    );
    assert_eq!(
        at("Qty AS"),
        expected(&[(2, "Qty", 'W'), (8, "Qty", 'W'), (10, "Qty", 'R')])
    );
    // CREATE changes the record; naming the table in a field reads it.
    let table = at("ttOrder NO-UNDO");
    assert_eq!(table.len(), 7, "{table:?}");
    assert!(
        table.contains(&(0, "ttOrder".to_string(), 'W')),
        "{table:?}"
    );
    assert!(
        table.contains(&(6, "ttOrder".to_string(), 'W')),
        "{table:?}"
    );
    assert!(
        table.contains(&(7, "ttOrder".to_string(), 'R')),
        "{table:?}"
    );
    // DELETE changes the record.
    assert!(
        table.contains(&(16, "ttOrder".to_string(), 'W')),
        "{table:?}"
    );
}

#[test]
fn class_names_and_methods_show_their_uses() {
    let ws = Workspace::new();
    let kinds = ws.path("kinds.p");
    let greeter = ws.path("app/Greeter.cls");
    let mut session = Session::start(&ws.root);
    session.open(&kinds, KINDS);
    session.open(&greeter, GREETER);

    // A type in a declaration and after NEW.
    assert_eq!(
        highlights(&mut session, &kinds, KINDS, "app.Greeter()", 0),
        expected(&[(5, "app.Greeter", 'R'), (11, "app.Greeter", 'R')])
    );
    assert_eq!(
        highlights(&mut session, &kinds, KINDS, "Hello()", 0),
        expected(&[(12, "Hello", 'R'), (13, "Hello", 'R')])
    );
    // An inherited method, called through the object and from the subclass.
    assert_eq!(
        highlights(&mut session, &kinds, KINDS, "Greet()", 0),
        expected(&[(14, "Greet", 'R'), (15, "Greet", 'R')])
    );
    assert_eq!(
        highlights(&mut session, &greeter, GREETER, "Greet()", 0),
        expected(&[(2, "Greet", 'R')])
    );
    // The declaration of a method in its own class.
    assert_eq!(
        highlights(&mut session, &greeter, GREETER, "Hello", 0),
        expected(&[(1, "Hello", 'W')])
    );
}

#[test]
fn an_include_read_in_two_procedures_declares_two_variables() {
    let ws = Workspace::new();
    let scopes = ws.path("scopes.p");
    let mut session = Session::start(&ws.root);
    session.open(&scopes, SCOPES);

    // Each procedure has its own `iLocal`: the other one is not marked.
    let found = highlights(&mut session, &scopes, SCOPES, "iLocal = 1", 0);
    assert_eq!(found, expected(&[(2, "iLocal", 'W')]));
    let found = highlights(&mut session, &scopes, SCOPES, "iLocal = 2", 0);
    assert_eq!(found, expected(&[(6, "iLocal", 'W')]));
}

#[test]
fn a_name_with_several_possible_declarations_has_no_highlight() {
    let ws = Workspace::new();
    let ambiguous = ws.path("ambiguous.p");
    let mut session = Session::start(&ws.root);
    session.open(&ambiguous, AMBIGUOUS);

    // `dup.p` is on two entries of the include path: oxabl does not choose.
    assert_eq!(
        highlights(&mut session, &ambiguous, AMBIGUOUS, "dup.p", 0),
        []
    );
    // `once.p` is on one: both runs show.
    assert_eq!(
        highlights(&mut session, &ambiguous, AMBIGUOUS, "once.p", 0),
        expected(&[(2, "once.p", 'R'), (3, "once.p", 'R')])
    );
}

#[test]
fn a_buffer_the_code_declares_is_not_the_default_buffer_of_its_table() {
    let ws = Workspace::new();
    let buffers = ws.path("buffers.p");
    let mut session = Session::start(&ws.root);
    session.open(&buffers, BUFFERS);

    // The default buffer of the table, used outside the procedure.
    let found = highlights(&mut session, &buffers, BUFFERS, "Customer NO-LOCK", 0);
    assert_eq!(
        found,
        expected(&[(0, "Customer", 'R'), (1, "Customer", 'R')])
    );
    // The buffer the procedure declares: its declaration and its uses.
    let found = highlights(&mut session, &buffers, BUFFERS, "Customer.Name", 1);
    assert_eq!(
        found,
        expected(&[
            (3, "Customer", 'R'),
            (4, "Customer", 'R'),
            (5, "Customer", 'R')
        ])
    );
}

#[test]
fn the_end_of_an_argument_means_the_argument_and_not_the_call() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    // The cursor is on the `)` that follows `iCount` in `Double(iCount)`.
    let at = nth(MAIN, "iCount).", 0) + "iCount".len();
    let found = highlights_at(&mut session, &main, MAIN, at);
    let from_the_name = highlights(&mut session, &main, MAIN, "iCount)", 0);
    assert_eq!(found, from_the_name);
    assert_eq!(found.len(), 8, "{found:?}");
    assert!(found.iter().all(|(_, text, _)| text == "iCount"));
}

#[test]
fn an_edit_is_seen_by_the_next_highlight() {
    let ws = Workspace::new();
    let scopes = ws.path("scopes.p");
    let mut session = Session::start(&ws.root);
    let text = "DEFINE VARIABLE iA AS INTEGER NO-UNDO.\niA = 1.\n";
    session.open(&scopes, text);
    assert_eq!(
        highlights(&mut session, &scopes, text, "iA = 1", 0).len(),
        2
    );

    let edited = format!("{text}MESSAGE iA.\n");
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&scopes), "version": 2},
            "contentChanges": [{"text": edited}]
        }
    }));
    assert_eq!(
        highlights(&mut session, &scopes, &edited, "iA = 1", 0),
        expected(&[(0, "iA", 'W'), (1, "iA", 'W'), (2, "iA", 'R'),])
    );
}

#[test]
fn an_unsaved_include_counts_as_it_does_for_go_to_definition() {
    let ws = Workspace::new();
    let main = ws.path("unsaved.p");
    let include = ws.path("unsaved.i");
    let mut session = Session::start(&ws.root);
    session.open(&main, UNSAVED_MAIN);
    // The memoized analysis of the program, before the include is open.
    assert_eq!(
        highlights(&mut session, &main, UNSAVED_MAIN, "b = 1", 0),
        Vec::new()
    );
    session.open(&include, "DEFINE VARIABLE b AS INTEGER NO-UNDO.\n");

    let found = highlights(&mut session, &main, UNSAVED_MAIN, "b = 1", 0);
    assert_eq!(
        found,
        expected(&[(1, "b", 'W'), (2, "b", 'W'), (2, "b", 'R')])
    );
}

#[test]
fn a_burst_of_highlights_answers_the_newest() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let wanted = highlights(&mut session, &main, MAIN, "iCount = 1", 0);
    assert!(!wanted.is_empty());

    let (line, character) = utf16_at(MAIN, nth(MAIN, "iCount = 1", 0) + 1);
    let params = json!({
        "textDocument": {"uri": file_uri(&main)},
        "position": {"line": line, "character": character}
    });
    let answers = session.request_all("textDocument/documentHighlight", vec![params; 30]);
    let (last, older) = answers.split_last().unwrap();
    assert_eq!(
        last["result"].as_array().map(Vec::len),
        Some(wanted.len()),
        "the newest request is answered: {last}"
    );
    for answer in older {
        assert!(
            answer["result"].as_array().map(Vec::len) == Some(wanted.len())
                || answer["error"]["code"] == json!(-32800),
            "an older request is answered or cancelled: {answer}"
        );
    }
}
