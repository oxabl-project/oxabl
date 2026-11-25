//! `textDocument/semanticTokens` through the real `oxabl lsp` and its daemon:
//! a token for each name that oxabl resolves, by what the name is. A name that
//! does not resolve, a string, a comment and a keyword have none. The delta and
//! range requests, the setting that turns the tokens off, and the stale
//! request: an edit that comes while a request runs gives no token of the old
//! text.
//!
//! A source with the marker `OXABL-TEST-HOLD:lsp.semantic-tokens` makes a
//! request wait three seconds before it computes, in test builds only
//! (`oxabl_common::hold_if_injected`), so the order of events does not depend
//! on the speed of the machine. The marker `OXABL-TEST-HOLD:lsp.semantic-snapshot`
//! makes it wait while it holds its database snapshot, so that a write to
//! another file cancels it. A number of milliseconds after `@` in a marker
//! (`lsp.semantic-snapshot@600`) gives another hold time.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::thread::sleep;
use std::time::Duration;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"tokens\"\n\
    [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n\
    [workspace.schema]\nfiles = [\"db/sports.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n  FORMAT \">>>>9\"\n\n\
    ADD FIELD \"Name\" OF \"Customer\" AS character\n  FORMAT \"x(30)\"\n\n";

/// One name of each kind, with a name that does not resolve and text that only
/// spells names.
const KINDS: &str = "/* iCount in a comment */\n\
    DEFINE TEMP-TABLE ttOrder NO-UNDO\n\
    \x20   FIELD OrderId AS INTEGER.\n\
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n\
    DEFINE BUFFER bCust FOR Customer.\n\
    DEFINE DATASET dsOrders FOR ttOrder.\n\
    DEFINE INPUT PARAMETER ipName AS CHARACTER NO-UNDO.\n\
    CREATE ttOrder.\n\
    ttOrder.OrderId = iCount.\n\
    MESSAGE \"iCount\" noSuchThing.\n\
    FOR EACH Customer NO-LOCK:\n\
    \x20   DISPLAY Customer.Name.\n\
    END.\n\
    FIND FIRST bCust NO-LOCK NO-ERROR.\n\
    DISPLAY bCust.CustNum.\n\
    iCount = Double(iCount).\n\
    RUN Twice.\n\
    PROCEDURE Twice:\n\
    END PROCEDURE.\n\
    FUNCTION Double RETURNS INTEGER (INPUT piValue AS INTEGER):\n\
    \x20   RETURN piValue * 2.\n\
    END FUNCTION.\n";

/// A class and the interface it implements, and a program that uses them.
const GREETER: &str = "CLASS app.Greeter IMPLEMENTS app.IThing:\n\
    \x20   DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO GET. SET.\n\
    \x20   DEFINE PUBLIC EVENT Changed SIGNATURE VOID (INPUT n AS INTEGER).\n\
    \x20   METHOD PUBLIC VOID Hello():\n\
    \x20       Greet().\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Greet():\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC STATIC VOID Make():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const THING: &str = "INTERFACE app.IThing:\n\
    \x20   METHOD PUBLIC VOID Hello().\n\
    END INTERFACE.\n";

const USER: &str = "USING app.*.\n\
    DEFINE VARIABLE oGreeter AS CLASS app.Greeter NO-UNDO.\n\
    oGreeter = NEW app.Greeter().\n\
    oGreeter:Hello().\n\
    oGreeter:Title = \"x\".\n\
    oGreeter:Nothing().\n";

/// An include that declares a variable, read by a program.
const DEFS: &str = "DEFINE VARIABLE gcShared AS CHARACTER NO-UNDO.\n";
const WITH_INCLUDE: &str =
    "{defs.i}\ngcShared = \"x\".\nDEFINE VARIABLE iLocal AS INTEGER NO-UNDO.\n";

/// Text with a non-ASCII character before names on the same line.
const WIDE: &str = "/* Größe 日本 */ DEFINE VARIABLE iWide AS INTEGER NO-UNDO.\n\
    /* Größe 日本 */ iWide = iWide + 1.\n";

/// Arguments that the analysis spans with their keyword (`BUFFER b`).
const ARGUMENTS: &str = "DEFINE BUFFER bCust FOR Customer.\n\
    DEFINE VARIABLE iA AS INTEGER NO-UNDO.\n\
    PROCEDURE Fill:\n\
    \x20   DEFINE PARAMETER BUFFER pbCust FOR Customer.\n\
    END PROCEDURE.\n\
    RUN Fill (BUFFER bCust).\n\
    RUN Fill (INPUT iA).\n";

/// An include that two procedures read, so its text is expanded twice.
const TWICE_HOST: &str = "PROCEDURE a:\n    {loc.i}\n    iLocal = 1.\nEND PROCEDURE.\n\
    PROCEDURE b:\n    {loc.i}\n    iLocal = 2.\nEND PROCEDURE.\n";
const LOC: &str = "DEFINE VARIABLE iLocal AS INTEGER NO-UNDO.\n";

/// An include that names a variable that only its includer declares.
const USES: &str = "iFromHost = iFromHost + 1.\n";
const HOST: &str = "DEFINE VARIABLE iFromHost AS INTEGER NO-UNDO.\n{uses.i}\n";

/// An include that two places read with two meanings of the same name: a
/// variable in the program, a parameter in the procedure.
const X_INC: &str = "x = x.\n";
const X_TWICE: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n{x.i}\n\
    PROCEDURE p:\n  DEFINE INPUT PARAMETER x AS INTEGER NO-UNDO.\n  {x.i}\nEND PROCEDURE.\n";

const X_HALF: &str = "PROCEDURE p:\n  DEFINE VARIABLE x AS INTEGER NO-UNDO.\n  {x.i}\nEND PROCEDURE.\n\
    PROCEDURE q:\n  FIND FIRST nosuchtable NO-LOCK.\n  {x.i}\nEND PROCEDURE.\n";

/// An include that uses a variable that another include of its includer declares.
const USES_SHARED: &str = "gcShared = \"x\".\n";
const HOST_SHARED: &str = "{defs.i}\n{uses_shared.i}\n";

/// An include that two procedures read, and that one of them reads without a
/// part: the bytes after the part are kept by both reads, the bytes of the part
/// by one.
const Y_INC: &str = "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n\
    &IF {&more} = 1 &THEN\n  y = 0.\n&ENDIF\ny = y + 1.\n";
const Y_TWICE: &str = "PROCEDURE p:\n  {y.i &more=1}\nEND PROCEDURE.\n\
    PROCEDURE q:\n  {y.i &more=0}\nEND PROCEDURE.\n";

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
            ("kinds.p", KINDS),
            ("app/Greeter.cls", GREETER),
            ("app/IThing.cls", THING),
            ("user.p", USER),
            ("defs.i", DEFS),
            ("with_include.p", WITH_INCLUDE),
            ("wide.p", WIDE),
            ("uses.i", USES),
            ("host.p", HOST),
            ("twice_host.p", TWICE_HOST),
            ("loc.i", LOC),
            ("x.i", X_INC),
            ("x_twice.p", X_TWICE),
            ("x_half.p", X_HALF),
            ("y.i", Y_INC),
            ("y_twice.p", Y_TWICE),
            ("uses_shared.i", USES_SHARED),
            ("host_shared.p", HOST_SHARED),
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

fn capabilities() -> Value {
    json!({
        "textDocument": {"semanticTokens": {
            "requests": {"range": true, "full": {"delta": true}},
            "tokenTypes": [], "tokenModifiers": [], "formats": ["relative"]
        }},
        "workspace": {"semanticTokens": {"refreshSupport": true}}
    })
}

/// One token: its line, its UTF-16 column, its text, its type and its modifiers.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Token {
    line: u64,
    column: u64,
    text: String,
    kind: String,
    modifiers: Vec<String>,
}

fn token(
    line: u64,
    text: &str,
    kind: &str,
    modifiers: &[&str],
) -> (u64, String, String, Vec<String>) {
    (
        line,
        text.to_string(),
        kind.to_string(),
        modifiers.iter().map(|m| m.to_string()).collect(),
    )
}

fn legend(session: &Session, key: &str) -> Vec<String> {
    session.initialized["result"]["capabilities"]["semanticTokensProvider"]["legend"][key]
        .as_array()
        .unwrap_or_else(|| {
            panic!(
                "the server has no semantic token legend: {}",
                session.initialized
            )
        })
        .iter()
        .map(|name| name.as_str().unwrap().to_string())
        .collect()
}

/// The tokens that the relative `data` of a response holds, read against
/// `text` (UTF-16 columns).
#[track_caller]
fn decode(session: &Session, text: &str, data: &Value) -> Vec<Token> {
    let types = legend(session, "tokenTypes");
    let modifiers = legend(session, "tokenModifiers");
    let numbers: Vec<u64> = data
        .as_array()
        .expect("the data is an array")
        .iter()
        .map(|n| n.as_u64().expect("the data holds integers"))
        .collect();
    assert_eq!(numbers.len() % 5, 0, "five integers for each token");
    let lines: Vec<&str> = text.split('\n').collect();
    let (mut line, mut column) = (0, 0);
    numbers
        .chunks(5)
        .map(|token| {
            line += token[0];
            column = if token[0] == 0 {
                column + token[1]
            } else {
                token[1]
            };
            let units: Vec<u16> = lines[line as usize].encode_utf16().collect();
            let text =
                String::from_utf16(&units[column as usize..(column + token[2]) as usize]).unwrap();
            Token {
                line,
                column,
                text,
                kind: types[token[3] as usize].clone(),
                modifiers: (0..modifiers.len())
                    .filter(|bit| token[4] & (1 << bit) != 0)
                    .map(|bit| modifiers[bit].clone())
                    .collect(),
            }
        })
        .collect()
}

/// The full tokens of `file`, as `(line, text, type, modifiers)`.
#[track_caller]
fn full(session: &mut Session, file: &Path, text: &str) -> Vec<Token> {
    let answer = session.request(
        "textDocument/semanticTokens/full",
        json!({"textDocument": {"uri": file_uri(file)}}),
    );
    assert!(
        answer.get("error").is_none(),
        "an error: {}",
        answer["error"]
    );
    decode(session, text, &answer["result"]["data"])
}

fn summary(tokens: &[Token]) -> Vec<(u64, String, String, Vec<String>)> {
    tokens
        .iter()
        .map(|t| (t.line, t.text.clone(), t.kind.clone(), t.modifiers.clone()))
        .collect()
}

#[test]
fn every_kind_of_name_has_its_token() {
    let ws = Workspace::new();
    let file = ws.path("kinds.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, KINDS);

    let got = summary(&full(&mut session, &file, KINDS));
    let d = &["declaration"][..];
    let none = &[][..];
    let want = vec![
        token(1, "ttOrder", "tempTable", d),
        token(2, "OrderId", "tempTableField", d),
        token(3, "iCount", "variable", d),
        token(4, "bCust", "buffer", d),
        token(4, "Customer", "table", none),
        token(5, "dsOrders", "dataset", d),
        token(5, "ttOrder", "tempTable", none),
        token(6, "ipName", "parameter", d),
        token(7, "ttOrder", "tempTable", none),
        token(8, "ttOrder", "tempTable", none),
        token(8, "OrderId", "tempTableField", none),
        token(8, "iCount", "variable", none),
        token(10, "Customer", "table", none),
        token(11, "Customer", "table", none),
        token(11, "Name", "databaseField", none),
        token(13, "bCust", "buffer", none),
        token(14, "bCust", "buffer", none),
        token(14, "CustNum", "databaseField", none),
        token(15, "iCount", "variable", none),
        token(15, "Double", "function", none),
        token(15, "iCount", "variable", none),
        token(16, "Twice", "function", none),
        token(17, "Twice", "function", d),
        token(19, "Double", "function", d),
        token(19, "piValue", "parameter", d),
        token(20, "piValue", "parameter", none),
    ];
    assert_eq!(got, want);
}

#[test]
fn a_name_that_does_not_resolve_has_no_token() {
    let ws = Workspace::new();
    let file = ws.path("kinds.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, KINDS);

    let tokens = full(&mut session, &file, KINDS);
    for text in ["noSuchThing", "MESSAGE", "DEFINE", "NO-LOCK", "\"iCount\""] {
        assert!(
            tokens.iter().all(|t| t.text != text),
            "`{text}` has no token: {tokens:?}"
        );
    }
    // The comment and the string that spell `iCount` have no token: line 0 is
    // the comment and line 9 holds the string and the unresolved name.
    assert!(
        tokens.iter().all(|t| t.line != 0 && t.line != 9),
        "{tokens:?}"
    );

    // A file that names nothing it can resolve has no token at all.
    let lonely = ws.path("lonely.p");
    let text = "MESSAGE unknownA unknownB \"text\".\n/* unknownC */\n";
    std::fs::write(&lonely, text).unwrap();
    session.open(&lonely, text);
    assert_eq!(full(&mut session, &lonely, text), Vec::new());
}

#[test]
fn a_span_with_a_keyword_in_it_has_no_token() {
    let ws = Workspace::new();
    let file = ws.path("arguments.p");
    std::fs::write(&file, ARGUMENTS).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, ARGUMENTS);

    let tokens = full(&mut session, &file, ARGUMENTS);
    for token in &tokens {
        assert!(
            !token.text.contains(' '),
            "a token is one name, not `{}`",
            token.text
        );
    }
    // `BUFFER bCust` is spanned whole, with its keyword, and the name in it has
    // no token; `iA` in `INPUT iA` has its own span.
    let d = &["declaration"][..];
    let none = &[][..];
    assert_eq!(
        summary(&tokens),
        vec![
            token(0, "bCust", "buffer", d),
            token(0, "Customer", "table", none),
            token(1, "iA", "variable", d),
            token(2, "Fill", "function", d),
            token(3, "pbCust", "buffer", d),
            token(3, "Customer", "table", none),
            token(5, "Fill", "function", none),
            token(6, "Fill", "function", none),
            token(6, "iA", "variable", none),
        ]
    );
}

#[test]
fn class_members_have_their_kinds() {
    let ws = Workspace::new();
    let class = ws.path("app/Greeter.cls");
    let user = ws.path("user.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&class, GREETER);
    session.open(&user, USER);

    let got = summary(&full(&mut session, &class, GREETER));
    let d = &["declaration"][..];
    let ds = &["declaration", "static"][..];
    let none = &[][..];
    assert_eq!(
        got,
        vec![
            token(0, "app.Greeter", "class", d),
            token(0, "app.IThing", "interface", none),
            token(1, "Title", "property", d),
            token(2, "Changed", "event", d),
            token(2, "n", "parameter", d),
            token(3, "Hello", "method", d),
            token(4, "Greet", "method", none),
            token(6, "Greet", "method", d),
            token(8, "Make", "method", ds),
        ]
    );

    // A wildcard import and a method that no class declares have no token.
    let got = summary(&full(&mut session, &user, USER));
    assert_eq!(
        got,
        vec![
            token(1, "oGreeter", "variable", d),
            token(1, "app.Greeter", "class", none),
            token(2, "oGreeter", "variable", none),
            token(2, "app.Greeter", "class", none),
            token(3, "oGreeter", "variable", none),
            token(3, "Hello", "method", none),
            token(4, "oGreeter", "variable", none),
            token(4, "Title", "property", none),
            token(5, "oGreeter", "variable", none),
        ]
    );
}

#[test]
fn an_include_is_its_own_file() {
    let ws = Workspace::new();
    let program = ws.path("with_include.p");
    let include = ws.path("defs.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&program, WITH_INCLUDE);
    session.open(&include, DEFS);

    // The program has its own names; the one the include declares is the
    // include's, though the program uses it.
    let got = summary(&full(&mut session, &program, WITH_INCLUDE));
    assert_eq!(
        got,
        vec![
            token(1, "gcShared", "variable", &[]),
            token(2, "iLocal", "variable", &["declaration"]),
        ]
    );
    let got = summary(&full(&mut session, &include, DEFS));
    assert_eq!(
        got,
        vec![token(0, "gcShared", "variable", &["declaration"])]
    );
}

#[test]
fn columns_follow_the_position_encoding() {
    let ws = Workspace::new();
    let file = ws.path("wide.p");
    for encoding in ["utf-16", "utf-8"] {
        let mut caps = capabilities();
        caps["general"] = json!({"positionEncodings": [encoding]});
        let mut session = Session::start_with(&ws.root, caps);
        session.open(&file, WIDE);
        let answer = session.request(
            "textDocument/semanticTokens/full",
            json!({"textDocument": {"uri": file_uri(&file)}}),
        );
        // The comment is 17 characters; in UTF-8 `ö`, `ß` and each of the two
        // other characters take more bytes than in UTF-16.
        let prefix = "/* Größe 日本 */ DEFINE VARIABLE ";
        let comment = "/* Größe 日本 */ ";
        let units = |text: &str| {
            (if encoding == "utf-8" {
                text.len()
            } else {
                text.encode_utf16().count()
            }) as u64
        };
        let data: Vec<u64> = integers(&answer["result"]["data"]);
        // `iWide` declared, then read twice on the next line.
        assert_eq!(
            data,
            vec![
                0,
                units(prefix),
                5,
                0,
                1,
                1,
                units(comment),
                5,
                0,
                0,
                0,
                8,
                5,
                0,
                0,
            ],
            "{encoding}: {answer}"
        );
    }
}

#[test]
fn a_file_of_ten_thousand_lines_is_answered_whole() {
    let ws = Workspace::new();
    let file = ws.path("large.p");
    let mut text = String::new();
    for k in 0..2_500 {
        text += &format!(
            "PROCEDURE p{k}:\n    DEFINE VARIABLE i{k} AS INTEGER NO-UNDO.\n    i{k} = i{k} + 1.\nEND PROCEDURE.\n"
        );
    }
    assert_eq!(text.lines().count(), 10_000);
    std::fs::write(&file, &text).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, &text);

    let tokens = full(&mut session, &file, &text);
    // The name of each procedure, the variable it declares and its two uses.
    assert_eq!(tokens.len(), 4 * 2_500);
    assert_eq!(tokens[0].text, "p0");
    assert_eq!(tokens[1].text, "i0");
    assert_eq!(tokens.last().unwrap().text, "i2499");
    assert_eq!(tokens.last().unwrap().line, 9_998);
}

#[test]
fn an_include_that_is_expanded_twice_gives_each_name_once() {
    let ws = Workspace::new();
    let include = ws.path("loc.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, LOC);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": ws.path("twice_host.p")}),
    );
    assert!(answer.get("error").is_none(), "{answer}");

    let got = summary(&full(&mut session, &include, LOC));
    assert_eq!(got, vec![token(0, "iLocal", "variable", &["declaration"])]);
}

#[test]
fn an_include_expanded_twice_with_two_meanings_has_no_token() {
    let ws = Workspace::new();
    let include = ws.path("x.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, X_INC);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": ws.path("x_twice.p")}),
    );
    assert!(answer.get("error").is_none(), "{answer}");

    // The first expansion reads the variable, the second the parameter.
    assert_eq!(
        summary(&full(&mut session, &include, X_INC)),
        Vec::new(),
        "a name with two meanings in one include has no token"
    );
}

#[test]
fn an_include_expanded_twice_with_one_reading_that_does_not_resolve_has_no_token() {
    let ws = Workspace::new();
    let include = ws.path("x.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, X_INC);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": ws.path("x_half.p")}),
    );
    assert!(answer.get("error").is_none(), "{answer}");

    // The first expansion reads a variable; in the second `x` resolves to nothing.
    assert_eq!(
        summary(&full(&mut session, &include, X_INC)),
        Vec::new(),
        "a name that one expansion does not resolve has no token"
    );
}

#[test]
fn an_include_that_one_read_cuts_short_keeps_the_names_both_reads_agree_on() {
    let ws = Workspace::new();
    let include = ws.path("y.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, Y_INC);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": ws.path("y_twice.p")}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let got = summary(&full(&mut session, &include, Y_INC));
    assert_eq!(
        got,
        vec![
            token(0, "y", "variable", &["declaration"]),
            token(2, "y", "variable", &[]),
            token(4, "y", "variable", &[]),
            token(4, "y", "variable", &[]),
        ]
    );
}

#[test]
fn an_include_read_in_a_context_uses_the_names_of_its_includer() {
    let ws = Workspace::new();
    let include = ws.path("uses.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, USES);

    // Alone, the include does not know the variable.
    assert_eq!(full(&mut session, &include, USES), Vec::new());

    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": ws.path("host.p")}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let got = summary(&full(&mut session, &include, USES));
    assert_eq!(
        got,
        vec![
            token(0, "iFromHost", "variable", &[]),
            token(0, "iFromHost", "variable", &[]),
        ]
    );
}

#[test]
fn a_range_gives_only_its_names() {
    let ws = Workspace::new();
    let file = ws.path("kinds.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, KINDS);
    let all = full(&mut session, &file, KINDS);

    let answer = session.request(
        "textDocument/semanticTokens/range",
        json!({
            "textDocument": {"uri": file_uri(&file)},
            "range": {"start": {"line": 10, "character": 0},
                      "end": {"line": 12, "character": 0}}
        }),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let got = decode(&session, KINDS, &answer["result"]["data"]);
    let want: Vec<Token> = all
        .into_iter()
        .filter(|t| (10..12).contains(&t.line))
        .collect();
    assert_eq!(got, want);
    assert_eq!(got.len(), 3, "{got:?}");
    assert!(answer["result"].get("resultId").is_none());
}

/// Apply the `edits` of a delta answer to the `old` integers.
fn apply(old: &[u64], edits: &Value) -> Vec<u64> {
    let mut data = old.to_vec();
    let mut edits: Vec<&Value> = edits.as_array().unwrap().iter().collect();
    edits.sort_by_key(|e| std::cmp::Reverse(e["start"].as_u64().unwrap()));
    for edit in edits {
        let start = edit["start"].as_u64().unwrap() as usize;
        let delete = edit["deleteCount"].as_u64().unwrap() as usize;
        let insert: Vec<u64> = edit["data"]
            .as_array()
            .map(|data| data.iter().map(|n| n.as_u64().unwrap()).collect())
            .unwrap_or_default();
        data.splice(start..start + delete, insert);
    }
    data
}

fn integers(data: &Value) -> Vec<u64> {
    data.as_array()
        .unwrap()
        .iter()
        .map(|n| n.as_u64().unwrap())
        .collect()
}

fn change(session: &mut Session, file: &Path, version: u32, line: u32, text: &str) {
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(file), "version": version},
            "contentChanges": [{
                "range": {"start": {"line": line, "character": 0},
                          "end": {"line": line, "character": 0}},
                "text": text
            }]
        }
    }));
}

#[test]
fn a_delta_turns_the_old_tokens_into_the_new_ones() {
    let ws = Workspace::new();
    let file = ws.path("kinds.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, KINDS);

    let first = session.request(
        "textDocument/semanticTokens/full",
        json!({"textDocument": {"uri": file_uri(&file)}}),
    );
    let previous = first["result"]["resultId"].as_str().unwrap().to_string();
    let old = integers(&first["result"]["data"]);

    // A new variable on line 3, used on line 16.
    let added = "DEFINE VARIABLE iExtra AS INTEGER NO-UNDO.\n";
    change(&mut session, &file, 2, 3, added);
    let edited = KINDS.replacen(
        "DEFINE VARIABLE iCount",
        &format!("{added}DEFINE VARIABLE iCount"),
        1,
    );

    let delta = session.request(
        "textDocument/semanticTokens/full/delta",
        json!({"textDocument": {"uri": file_uri(&file)}, "previousResultId": previous}),
    );
    assert!(delta.get("error").is_none(), "{delta}");
    assert_ne!(
        delta["result"]["resultId"].as_str(),
        Some(previous.as_str())
    );
    let edits = &delta["result"]["edits"];
    assert!(edits.is_array(), "a delta, not the whole tokens: {delta}");

    // The edited text, asked for whole, is what the edits make.
    let again = session.request(
        "textDocument/semanticTokens/full",
        json!({"textDocument": {"uri": file_uri(&file)}}),
    );
    let new = integers(&again["result"]["data"]);
    assert_eq!(apply(&old, edits), new);
    assert_ne!(old, new);
    let tokens = decode(&session, &edited, &again["result"]["data"]);
    assert!(tokens.iter().any(|t| t.text == "iExtra" && t.line == 3));

    // A result id the server does not know gets the whole tokens.
    let unknown = session.request(
        "textDocument/semanticTokens/full/delta",
        json!({"textDocument": {"uri": file_uri(&file)}, "previousResultId": "no-such-id"}),
    );
    assert_eq!(integers(&unknown["result"]["data"]), new, "{unknown}");
}

#[test]
fn the_setting_turns_the_tokens_off_and_on() {
    let ws = Workspace::new();
    let file = ws.path("kinds.p");
    let mut session = Session::start_with_options(
        &ws.root,
        capabilities(),
        json!({"semanticTokens": {"enable": false}}),
    );
    session.open(&file, KINDS);
    let answer = session.request(
        "textDocument/semanticTokens/full",
        json!({"textDocument": {"uri": file_uri(&file)}}),
    );
    assert_eq!(answer["result"]["data"], json!([]), "off at the start");

    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"semanticTokens": {"enable": true}}}}
    }));
    // The client is asked for the tokens again.
    session.wait_for(|m| m["method"] == json!("workspace/semanticTokens/refresh"));
    let on = full(&mut session, &file, KINDS);
    assert!(!on.is_empty(), "on after the change");

    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"semanticTokens": {"enable": false}}}}
    }));
    session.wait_for(|m| m["method"] == json!("workspace/semanticTokens/refresh"));
    assert_eq!(full(&mut session, &file, KINDS), Vec::new(), "off again");
}

/// The types that the client knows without help.
const STANDARD: &[&str] = &[
    "variable",
    "parameter",
    "property",
    "function",
    "method",
    "class",
    "interface",
    "event",
    "type",
];

#[test]
fn the_vs_code_extension_contributes_each_type_that_is_not_standard() {
    let ws = Workspace::new();
    let session = Session::start_with(&ws.root, capabilities());
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../clients/vscode/package.json");
    let manifest: Value = serde_json::from_slice(&std::fs::read(manifest).unwrap()).unwrap();
    let contributed: Vec<String> = manifest["contributes"]["semanticTokenTypes"]
        .as_array()
        .expect("the extension contributes token types")
        .iter()
        .map(|t| t["id"].as_str().unwrap().to_string())
        .collect();
    let mut wanted: Vec<String> = legend(&session, "tokenTypes")
        .into_iter()
        .filter(|name| !STANDARD.contains(&name.as_str()))
        .collect();
    let mut contributed = contributed;
    wanted.sort();
    contributed.sort();
    assert_eq!(contributed, wanted);
    assert!(
        manifest["contributes"]["configuration"]["properties"]["oxabl.semanticTokens.enable"]
            .is_object()
    );
}

#[test]
fn a_client_without_semantic_tokens_is_not_offered_them() {
    let ws = Workspace::new();
    let session = Session::start(&ws.root);
    assert!(
        session.initialized["result"]["capabilities"]
            .get("semanticTokensProvider")
            .is_none(),
        "{}",
        session.initialized
    );
}

const HELD: &str = "/* OXABL-TEST-HOLD:lsp.semantic-tokens */\n\
    DEFINE VARIABLE iOld AS INTEGER NO-UNDO.\n\
    iOld = 1.\n";

#[test]
fn an_edit_during_a_slow_request_gives_no_stale_tokens() {
    let ws = Workspace::new();
    let file = ws.path("held.p");
    std::fs::write(&file, HELD).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, HELD);

    let slow = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": slow, "method": "textDocument/semanticTokens/full",
        "params": {"textDocument": {"uri": file_uri(&file)}}
    }));
    // Long enough for the worker to start the request, far shorter than the hold.
    sleep(Duration::from_millis(300));
    // The edit renames the variable, in both places.
    let edited = HELD.replace("iOld", "iNew");
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&file), "version": 2},
            "contentChanges": [{"text": edited}]
        }
    }));
    let fresh = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": fresh, "method": "textDocument/semanticTokens/full",
        "params": {"textDocument": {"uri": file_uri(&file)}}
    }));

    let old = session.wait_for(|m| m["id"] == json!(slow));
    assert_eq!(
        old["error"]["code"].as_i64(),
        Some(-32801),
        "the old request answers ContentModified: {old}"
    );
    assert!(
        old.get("result").is_none(),
        "no tokens of the old text: {old}"
    );

    let new = session.wait_for(|m| m["id"] == json!(fresh));
    assert!(new.get("error").is_none(), "{new}");
    let tokens = decode(&session, &edited, &new["result"]["data"]);
    assert_eq!(
        tokens.iter().map(|t| t.text.as_str()).collect::<Vec<_>>(),
        ["iNew", "iNew"],
        "the tokens of the new text"
    );
}

/// The analysis is done when the edit comes: the answer is held back until the
/// edit has come, and must not be sent.
const HELD_AFTER: &str = "/* OXABL-TEST-HOLD:lsp.semantic-answer */\n\
    DEFINE VARIABLE iOld AS INTEGER NO-UNDO.\n\
    iOld = 1.\n";

#[test]
fn an_edit_after_the_analysis_gives_no_stale_tokens() {
    let ws = Workspace::new();
    let file = ws.path("held_after.p");
    std::fs::write(&file, HELD_AFTER).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, HELD_AFTER);

    let slow = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": slow, "method": "textDocument/semanticTokens/full",
        "params": {"textDocument": {"uri": file_uri(&file)}}
    }));
    sleep(Duration::from_millis(500));
    change(&mut session, &file, 2, 1, "/* edit */\n");

    let old = session.wait_for(|m| m["id"] == json!(slow));
    assert_eq!(
        old["error"]["code"].as_i64(),
        Some(-32801),
        "the old request answers ContentModified: {old}"
    );
    assert!(
        old.get("result").is_none(),
        "no tokens of the old text: {old}"
    );
}

/// The analysis holds its snapshot while a write to another file comes.
const HELD_SNAPSHOT: &str = "/* OXABL-TEST-HOLD:lsp.semantic-snapshot */\n\
    DEFINE VARIABLE iOld AS INTEGER NO-UNDO.\n\
    iOld = 1.\n";

#[test]
fn a_write_to_another_file_does_not_fail_the_request() {
    let ws = Workspace::new();
    let file = ws.path("held_snapshot.p");
    let other = ws.path("wide.p");
    std::fs::write(&file, HELD_SNAPSHOT).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&other, WIDE);
    session.open(&file, HELD_SNAPSHOT);

    let slow = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": slow, "method": "textDocument/semanticTokens/full",
        "params": {"textDocument": {"uri": file_uri(&file)}}
    }));
    // The worker holds its snapshot; the text of its file does not change.
    sleep(Duration::from_millis(500));
    change(&mut session, &other, 2, 1, "/* edit */\n");

    let answer = session.wait_for(|m| m["id"] == json!(slow));
    assert!(
        answer.get("error").is_none(),
        "a write to another file gives the tokens, not an error: {answer}"
    );
    let tokens = decode(&session, HELD_SNAPSHOT, &answer["result"]["data"]);
    assert_eq!(
        tokens.iter().map(|t| t.text.as_str()).collect::<Vec<_>>(),
        ["iOld", "iOld"],
        "the tokens of the unchanged text"
    );
}

/// The snapshot is held for 600 ms, so that a write and a request fit in it.
const HELD_SHORT: &str = "/* OXABL-TEST-HOLD:lsp.semantic-snapshot@600 */\n\
    DEFINE VARIABLE iOld AS INTEGER NO-UNDO.\n\
    iOld = 1.\n";

/// A cancelled analysis of an unchanged text runs again behind the requests
/// that came after it, not at once: the request for another file does not
/// wait for the retry.
#[test]
fn a_cancelled_request_runs_again_behind_the_newer_requests() {
    let ws = Workspace::new();
    let held = ws.path("held_short.p");
    let other = ws.path("wide.p");
    std::fs::write(&held, HELD_SHORT).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&other, WIDE);
    session.open(&held, HELD_SHORT);

    let first = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": first, "method": "textDocument/semanticTokens/full",
        "params": {"textDocument": {"uri": file_uri(&held)}}
    }));
    sleep(Duration::from_millis(100));
    let second = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": second, "method": "textDocument/semanticTokens/full",
        "params": {"textDocument": {"uri": file_uri(&other)}}
    }));
    sleep(Duration::from_millis(100));
    // The write lands while the first analysis holds its snapshot.
    change(&mut session, &other, 2, 1, "/* edit */\n");

    let early = session.wait_for(|m| m["id"] == json!(first) || m["id"] == json!(second));
    assert_eq!(
        early["id"],
        json!(second),
        "the newer request is answered before the retry of the cancelled one: {early}"
    );
    let answer = session.wait_for(|m| m["id"] == json!(first));
    assert!(
        answer.get("error").is_none(),
        "the retry gives the tokens, not an error: {answer}"
    );
    let tokens = decode(&session, HELD_SHORT, &answer["result"]["data"]);
    assert_eq!(
        tokens.iter().map(|t| t.text.as_str()).collect::<Vec<_>>(),
        ["iOld", "iOld"],
        "the tokens of the unchanged text"
    );
}

/// Requests that come together wait for no delay between them: the delay is
/// for a job that runs again.
#[test]
fn requests_that_come_together_are_not_delayed_one_after_another() {
    let ws = Workspace::new();
    let file = ws.path("kinds.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, KINDS);
    // The first answer fills the cache.
    full(&mut session, &file, KINDS);

    let count: u64 = 30;
    let params = (0..count)
        .map(|_| json!({"textDocument": {"uri": file_uri(&file)}}))
        .collect();
    let start = std::time::Instant::now();
    let answers = session.request_all("textDocument/semanticTokens/full", params);
    let took = start.elapsed();
    assert!(
        answers.iter().all(|a| a.get("error").is_none()),
        "{answers:?}"
    );
    assert!(
        took < Duration::from_millis(50 * count),
        "{count} requests together took {took:?}; a delay before each would take over {:?}",
        Duration::from_millis(100 * (count - 1))
    );
}

#[test]
fn an_edit_to_an_open_include_asks_the_client_for_the_tokens_again() {
    let ws = Workspace::new();
    let include = ws.path("defs.i");
    let program = ws.path("with_include.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, DEFS);
    session.open(&program, WITH_INCLUDE);
    // The server knows what the program includes once it has analysed it.
    let uri = file_uri(&program);
    session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == json!(uri)
    });
    let before = full(&mut session, &program, WITH_INCLUDE);
    assert!(
        before.iter().any(|t| t.text == "gcShared" && t.line == 1),
        "the include declares the name: {before:?}"
    );

    // The include, not saved, declares another variable.
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&include), "version": 2},
            "contentChanges": [{
                "text": "DEFINE VARIABLE gcOther AS CHARACTER NO-UNDO.\n"
            }]
        }
    }));
    session.wait_for(|m| m["method"] == json!("workspace/semanticTokens/refresh"));
    let after = full(&mut session, &program, WITH_INCLUDE);
    assert!(
        !after.iter().any(|t| t.text == "gcShared" && t.line == 1),
        "the name no longer resolves: {after:?}"
    );
}

#[test]
fn an_include_edit_asks_for_the_tokens_again_with_the_diagnostics_off() {
    let ws = Workspace::new();
    std::fs::write(
        ws.path("oxabl.toml"),
        format!("{CONFIG}[workspace.features]\ndiagnostics = false\n"),
    )
    .unwrap();
    let include = ws.path("defs.i");
    let program = ws.path("with_include.p");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&include, DEFS);
    session.open(&program, WITH_INCLUDE);
    let before = full(&mut session, &program, WITH_INCLUDE);
    assert!(
        before.iter().any(|t| t.text == "gcShared" && t.line == 1),
        "the include declares the name: {before:?}"
    );

    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&include), "version": 2},
            "contentChanges": [{
                "text": "DEFINE VARIABLE gcOther AS CHARACTER NO-UNDO.\n"
            }]
        }
    }));
    session.wait_for(|m| m["method"] == json!("workspace/semanticTokens/refresh"));
    let after = full(&mut session, &program, WITH_INCLUDE);
    assert!(
        !after.iter().any(|t| t.text == "gcShared" && t.line == 1),
        "the name no longer resolves: {after:?}"
    );
}

/// An open include with a selected context gets the tokens asked for again
/// when another open include that the context reads is edited.
fn context_include_edit_asks_again(diagnostics: bool) {
    let ws = Workspace::new();
    if !diagnostics {
        std::fs::write(
            ws.path("oxabl.toml"),
            format!("{CONFIG}[workspace.features]\ndiagnostics = false\n"),
        )
        .unwrap();
    }
    let defs = ws.path("defs.i");
    let uses = ws.path("uses_shared.i");
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&defs, DEFS);
    session.open(&uses, USES_SHARED);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&uses)}, "root": ws.path("host_shared.p")}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let before = full(&mut session, &uses, USES_SHARED);
    assert!(
        before.iter().any(|t| t.text == "gcShared"),
        "the context declares the name: {before:?}"
    );

    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&defs), "version": 2},
            "contentChanges": [{
                "text": "DEFINE VARIABLE gcOther AS CHARACTER NO-UNDO.\n"
            }]
        }
    }));
    session.wait_for(|m| m["method"] == json!("workspace/semanticTokens/refresh"));
    let after = full(&mut session, &uses, USES_SHARED);
    assert!(
        !after.iter().any(|t| t.text == "gcShared"),
        "the name no longer resolves: {after:?}"
    );
}

#[test]
fn an_edit_to_an_include_of_the_context_asks_again() {
    context_include_edit_asks_again(true);
}

#[test]
fn an_edit_to_an_include_of_the_context_asks_again_with_the_diagnostics_off() {
    context_include_edit_asks_again(false);
}

/// An open include with a selected context gets the tokens asked for again
/// when the open context root itself is edited.
fn context_root_edit_asks_again(diagnostics: bool) {
    let ws = Workspace::new();
    if !diagnostics {
        std::fs::write(
            ws.path("oxabl.toml"),
            format!("{CONFIG}[workspace.features]\ndiagnostics = false\n"),
        )
        .unwrap();
    }
    let host = ws.path("host_root.p");
    let uses = ws.path("uses_root.i");
    let host_text = "DEFINE VARIABLE iFromHost AS INTEGER NO-UNDO.\n{uses_root.i}\n";
    let uses_text = "iFromHost = iFromHost + 1.\n";
    std::fs::write(&host, host_text).unwrap();
    std::fs::write(&uses, uses_text).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&host, host_text);
    session.open(&uses, uses_text);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&uses)}, "root": host}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let before = full(&mut session, &uses, uses_text);
    assert!(
        before.iter().any(|t| t.text == "iFromHost"),
        "the context root declares the name: {before:?}"
    );

    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&host), "version": 2},
            "contentChanges": [{"text": "{uses_root.i}\n"}]
        }
    }));
    session.wait_for(|m| m["method"] == json!("workspace/semanticTokens/refresh"));
    let after = full(&mut session, &uses, uses_text);
    assert!(
        !after.iter().any(|t| t.text == "iFromHost"),
        "the name no longer resolves: {after:?}"
    );
}

#[test]
fn an_edit_to_the_context_root_asks_again() {
    context_root_edit_asks_again(true);
}

#[test]
fn an_edit_to_the_context_root_asks_again_with_the_diagnostics_off() {
    context_root_edit_asks_again(false);
}

#[test]
fn a_request_for_an_old_version_that_waited_is_dropped() {
    let ws = Workspace::new();
    let file = ws.path("held.p");
    std::fs::write(&file, HELD).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, HELD);

    // Three requests queue behind the held one; an edit comes before they run.
    let ids: Vec<u64> = (0..3).map(|_| session.next_request_id()).collect();
    for id in &ids {
        session.send(json!({
            "jsonrpc": "2.0", "id": id, "method": "textDocument/semanticTokens/full",
            "params": {"textDocument": {"uri": file_uri(&file)}}
        }));
    }
    sleep(Duration::from_millis(300));
    change(&mut session, &file, 2, 1, "/* edit */\n");
    for id in &ids {
        let answer = session.wait_for(|m| m["id"] == json!(id));
        assert_eq!(
            answer["error"]["code"].as_i64(),
            Some(-32801),
            "request {id}: {answer}"
        );
    }
}

#[test]
fn a_panic_in_the_analysis_fails_one_request_only() {
    let ws = Workspace::new();
    let file = ws.path("panics.p");
    let panicking =
        "/* OXABL-TEST-PANIC:lsp.semantic-tokens */\nDEFINE VARIABLE i AS INTEGER NO-UNDO.\n";
    std::fs::write(&file, panicking).unwrap();
    let mut session = Session::start_with(&ws.root, capabilities());
    session.open(&file, panicking);

    let answer = session.request(
        "textDocument/semanticTokens/full",
        json!({"textDocument": {"uri": file_uri(&file)}}),
    );
    assert_eq!(answer["error"]["code"].as_i64(), Some(-32603), "{answer}");

    // The server is alive: the next file gives its tokens.
    let kinds = ws.path("kinds.p");
    session.open(&kinds, KINDS);
    assert!(!full(&mut session, &kinds, KINDS).is_empty());
}
