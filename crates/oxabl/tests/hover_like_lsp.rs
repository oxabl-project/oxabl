//! `textDocument/hover` on a temp-table through the real `oxabl lsp`: a table
//! defined `LIKE` another shows the fields and indexes it inherits, and says so
//! when it cannot know them.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"like\"\n\
    [workspace.schema]\nfiles = [\"db/shop.df\"]\n";

const CONFIG_NO_SCHEMA: &str = "[workspace]\nname = \"like\"\n";

const SCHEMA: &str = "ADD TABLE \"Customer\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\n\
    ADD FIELD \"CustName\" OF \"Customer\" AS character\n\n\
    ADD FIELD \"Balance\" OF \"Customer\" AS decimal\n\n\
    ADD INDEX \"CustNum\" ON \"Customer\"\n  AREA \"Index\"\n  UNIQUE\n  PRIMARY\n\
    \x20 INDEX-FIELD \"CustNum\" ASCENDING\n\n\
    ADD INDEX \"CustName\" ON \"Customer\"\n  AREA \"Index\"\n\
    \x20 INDEX-FIELD \"CustName\" ASCENDING\n\n";

const MAIN: &str = "\
DEFINE TEMP-TABLE ttOwn NO-UNDO FIELD a AS INTEGER FIELD b AS CHARACTER\n\
\x20   INDEX ia IS PRIMARY UNIQUE a INDEX ib b.\n\
DEFINE TEMP-TABLE ttBare NO-UNDO FIELD q AS INTEGER.\n\
DEFINE TEMP-TABLE ttFlat NO-UNDO FIELD x AS INTEGER INDEX i1 x INDEX i2 x.\n\
DEFINE TEMP-TABLE ttCopy NO-UNDO LIKE ttOwn.\n\
DEFINE TEMP-TABLE ttTwo NO-UNDO LIKE ttOwn USE-INDEX IA USE-INDEX Ib AS PRIMARY.\n\
DEFINE TEMP-TABLE ttNewPrimary NO-UNDO LIKE ttOwn INDEX ic IS PRIMARY b.\n\
DEFINE TEMP-TABLE ttUse NO-UNDO LIKE ttOwn USE-INDEX ib.\n\
DEFINE TEMP-TABLE ttMore NO-UNDO LIKE ttOwn FIELD c AS LOGICAL INDEX ic c.\n\
DEFINE TEMP-TABLE ttMix NO-UNDO LIKE ttOwn USE-INDEX ia FIELD c AS LOGICAL INDEX ic c.\n\
DEFINE TEMP-TABLE ttChain NO-UNDO LIKE ttCopy FIELD d AS DECIMAL.\n\
DEFINE TEMP-TABLE ttSeq NO-UNDO LIKE-SEQUENTIAL ttOwn FIELD c AS LOGICAL.\n\
DEFINE TEMP-TABLE ttPrimary NO-UNDO LIKE ttOwn USE-INDEX ib AS PRIMARY.\n\
DEFINE TEMP-TABLE ttDb NO-UNDO LIKE Customer FIELD extra AS CHARACTER.\n\
DEFINE TEMP-TABLE ttDbUse NO-UNDO LIKE Customer USE-INDEX CustName.\n\
DEFINE TEMP-TABLE ttGhost NO-UNDO LIKE Ghost FIELD x AS INTEGER INDEX ix x.\n\
DEFINE TEMP-TABLE ttGhostChain NO-UNDO LIKE ttGhost FIELD y AS INTEGER.\n\
MESSAGE ttCopy.a ttChain.b ttChain.d ttDb.CustName ttDb.extra ttMore.c ttMore.a.\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new(config: &str) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        for (name, text) in [
            ("oxabl.toml", config),
            ("db/shop.df", SCHEMA),
            ("main.p", MAIN),
        ] {
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

fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

/// The Markdown of the hover one character into the first occurrence of
/// `needle` in `file`. Fails when there is none.
#[track_caller]
fn markdown(session: &mut Session, file: &Path, needle: &str) -> String {
    let at = MAIN
        .find(needle)
        .unwrap_or_else(|| panic!("no `{needle}` in the program"));
    let (line, character) = utf16_at(MAIN, at + 1);
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
        Value::Null => panic!("no hover on `{needle}`"),
        result => result["contents"]["value"].as_str().unwrap().to_string(),
    }
}

fn code(text: &str) -> String {
    format!("```abl\n{text}\n```")
}

/// The hover text under the code block of the table `name`.
#[track_caller]
fn shape(session: &mut Session, file: &Path, name: &str) -> String {
    let found = markdown(session, file, &format!("{name} NO-UNDO"));
    found
        .split_once("\n```\n\n")
        .unwrap_or_else(|| panic!("no text under the code of `{name}`: {found}"))
        .1
        .to_string()
}

#[test]
fn own_fields_and_indexes_only() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    assert_eq!(
        shape(&mut session, &main, "ttOwn"),
        "Fields: 2 · Indexes: 2\n\nIndexes:\n- `ia` (primary, unique): a\n- `ib`: b"
    );
    assert_eq!(
        shape(&mut session, &main, "ttBare"),
        "Fields: 1 · Indexes: 0"
    );
    // No index is marked primary: the first one is.
    assert_eq!(
        shape(&mut session, &main, "ttFlat"),
        "Fields: 1 · Indexes: 2\n\nIndexes:\n- `i1` (primary): x\n- `i2`: x"
    );
}

#[test]
fn a_like_table_takes_the_fields_and_indexes_of_its_source() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let all = "Indexes:\n- `ia` (primary, unique): a\n- `ib`: b";
    assert_eq!(
        shape(&mut session, &main, "ttCopy"),
        format!("Fields: 2 · Indexes: 2\n\n{all}")
    );
    // The chain follows: ttChain is LIKE ttCopy is LIKE ttOwn.
    assert_eq!(
        shape(&mut session, &main, "ttChain"),
        format!("Fields: 3 · Indexes: 2\n\n{all}")
    );
}

#[test]
fn use_index_takes_only_that_index_and_makes_it_primary() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    assert_eq!(
        shape(&mut session, &main, "ttUse"),
        "Fields: 2 · Indexes: 1\n\nIndexes:\n- `ib` (primary): b"
    );
    // Two are taken, and the one marked is primary.
    assert_eq!(
        shape(&mut session, &main, "ttTwo"),
        "Fields: 2 · Indexes: 2\n\nIndexes:\n- `ia` (unique): a\n- `ib` (primary): b"
    );
    assert_eq!(
        shape(&mut session, &main, "ttPrimary"),
        "Fields: 2 · Indexes: 1\n\nIndexes:\n- `ib` (primary): b"
    );
}

#[test]
fn own_fields_and_indexes_come_after_the_inherited_ones() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    assert_eq!(
        shape(&mut session, &main, "ttMore"),
        "Fields: 3 · Indexes: 3\n\nIndexes:\n- `ia` (primary, unique): a\n- `ib`: b\n- `ic`: c"
    );
    // A table has one primary index: the own one replaces the inherited one.
    assert_eq!(
        shape(&mut session, &main, "ttNewPrimary"),
        "Fields: 2 · Indexes: 3\n\nIndexes:\n- `ia` (unique): a\n- `ib`: b\n- `ic` (primary): b"
    );
    assert_eq!(
        shape(&mut session, &main, "ttMix"),
        "Fields: 3 · Indexes: 2\n\nIndexes:\n- `ia` (primary, unique): a\n- `ic`: c"
    );
    assert_eq!(
        shape(&mut session, &main, "ttSeq"),
        "Fields: 3 · Indexes: 2\n\nIndexes:\n- `ia` (primary, unique): a\n- `ib`: b"
    );
}

#[test]
fn a_like_database_table_takes_its_shape_from_the_schema() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    assert_eq!(
        shape(&mut session, &main, "ttDb"),
        "Fields: 4 · Indexes: 2\n\nIndexes:\n- `CustNum` (primary, unique): CustNum\n- `CustName`: CustName"
    );
    assert_eq!(
        shape(&mut session, &main, "ttDbUse"),
        "Fields: 3 · Indexes: 1\n\nIndexes:\n- `CustName` (primary): CustName"
    );
}

#[test]
fn a_source_the_schema_lacks_is_not_counted_as_empty() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    assert_eq!(
        shape(&mut session, &main, "ttGhost"),
        "Fields: 1 own + those of `Ghost` (not in the loaded schema) · \
         Indexes: 1 own + those of `Ghost` (not in the loaded schema)\n\n\
         Own indexes:\n- `ix`: x"
    );
    assert_eq!(
        shape(&mut session, &main, "ttGhostChain"),
        "Fields: 1 own + those of `ttGhost` (`Ghost`: not in the loaded schema) · \
         Indexes: 0 own + those of `ttGhost` (`Ghost`: not in the loaded schema)"
    );
}

#[test]
fn without_a_schema_a_database_source_is_not_known() {
    let ws = Workspace::new(CONFIG_NO_SCHEMA);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let found = shape(&mut session, &main, "ttDb");
    assert_eq!(
        found,
        "Fields: 1 own + those of `Customer` (no schema is loaded) · \
         Indexes: 0 own + those of `Customer` (no schema is loaded)"
    );
    // A temp-table source still resolves without a schema.
    assert_eq!(
        shape(&mut session, &main, "ttCopy"),
        "Fields: 2 · Indexes: 2\n\nIndexes:\n- `ia` (primary, unique): a\n- `ib`: b"
    );
}

#[test]
fn an_inherited_field_shows_its_type() {
    let ws = Workspace::new(CONFIG);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let own = |field: &str, ty: &str, table: &str| {
        code(&format!("FIELD {field} AS {ty}")) + &format!("\n\nField of temp-table `{table}`.")
    };
    let cases = [
        // Inherited through a chain, from the table that declares it.
        ("a ttChain.b", own("a", "INTEGER", "ttOwn")),
        ("b ttChain.d", own("b", "CHARACTER", "ttOwn")),
        ("d ttDb", own("d", "DECIMAL", "ttChain")),
        ("extra ttMore", own("extra", "CHARACTER", "ttDb")),
        ("c ttMore.a", own("c", "LOGICAL", "ttMore")),
        (
            "CustName ttDb.extra",
            code("FIELD Customer.CustName AS CHARACTER") + "\n\nIndexes:\n- `CustName`: CustName",
        ),
    ];
    for (needle, expected) in cases {
        assert_eq!(
            markdown(&mut session, &main, needle),
            expected,
            "hover on `{needle}`"
        );
    }
}
