//! The database effects that `textDocument/hover` adds for a procedure,
//! function, method or `RUN` target, through the real `oxabl lsp`: each table
//! with its operations and lock, direct effects apart from those through a
//! call, and a summary that may miss effects marked incomplete.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"effects\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n\
    [workspace.schema]\nfiles = [\"db/shop.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Item\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"ItemNum\" OF \"Item\" AS integer\n  FORMAT \">>>>9\"\n\n\
    ADD FIELD \"Price\" OF \"Item\" AS decimal\n  FORMAT \">>,>>9.99\"\n\n\
    ADD TABLE \"Ledger\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"Note\" OF \"Ledger\" AS character\n  FORMAT \"x(40)\"\n\n";

const MAIN: &str = "PROCEDURE ReadItems:\n\
    \x20   FOR EACH Item NO-LOCK:\n\
    \x20   END.\n\
    END PROCEDURE.\n\
    PROCEDURE Restock:\n\
    \x20   FIND FIRST Item EXCLUSIVE-LOCK NO-ERROR.\n\
    \x20   IF AVAILABLE Item THEN Item.Price = 1.\n\
    \x20   RUN ReadItems.\n\
    \x20   RUN jobs/archive.p.\n\
    END PROCEDURE.\n\
    PROCEDURE Unclear:\n\
    \x20   DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.\n\
    \x20   FIND FIRST Item NO-LOCK NO-ERROR.\n\
    \x20   RUN VALUE(cProg).\n\
    \x20   RUN missing.p.\n\
    END PROCEDURE.\n\
    PROCEDURE Quiet:\n\
    END PROCEDURE.\n\
    PROCEDURE Opaque:\n\
    \x20   DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.\n\
    \x20   RUN VALUE(cProg).\n\
    END PROCEDURE.\n\
    FUNCTION fnCount RETURNS INTEGER ():\n\
    \x20   FIND FIRST Ledger NO-LOCK NO-ERROR.\n\
    \x20   RETURN 1.\n\
    END FUNCTION.\n\
    RUN Restock.\n\
    RUN Unclear.\n\
    RUN Quiet.\n\
    RUN Opaque.\n\
    RUN jobs/archive.p.\n\
    DISPLAY fnCount().\n\
    DEFINE VARIABLE oStock AS CLASS app.Stock NO-UNDO.\n\
    oStock = NEW app.Stock().\n\
    oStock:Count().\n";

const STOCK: &str = "CLASS app.Stock:\n\
    \x20   METHOD PUBLIC INTEGER Count():\n\
    \x20       FIND FIRST Item SHARE-LOCK NO-ERROR.\n\
    \x20       RETURN 1.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const ARCHIVE: &str = "CREATE Ledger.\n\
    Ledger.Note = \"archived\".\n\
    FOR EACH Item EXCLUSIVE-LOCK:\n\
    \x20   DELETE Item.\n\
    END.\n";

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
            ("main.p", MAIN),
            ("jobs/archive.p", ARCHIVE),
            ("app/Stock.cls", STOCK),
        ];
        for (name, text) in files {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
    }
}

/// The UTF-16 `(line, character)` of byte `at` in `text`.
fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

/// The Markdown of the hover one character into the first `needle` of
/// `text`. Fails when there is none.
#[track_caller]
fn markdown(session: &mut Session, file: &Path, text: &str, needle: &str) -> String {
    let at = text.find(needle).unwrap_or_else(|| panic!("no `{needle}`"));
    let (line, character) = utf16_at(text, at + 1);
    let answer = session.request(
        "textDocument/hover",
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": {"line": line, "character": character}
        }),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    match &answer["result"]["contents"]["value"] {
        Value::String(value) => value.clone(),
        _ => panic!("no hover on `{needle}`: {answer}"),
    }
}

/// The effects part of a hover: what follows the rule.
#[track_caller]
fn effects(hover: &str) -> &str {
    hover
        .split_once("\n\n---\n\n")
        .unwrap_or_else(|| panic!("no effects in the hover: {hover}"))
        .1
}

#[test]
fn direct_effects_are_listed_per_table() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let hover = markdown(&mut session, &main, MAIN, "ReadItems:");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Item`: read NO-LOCK"
    );
    let hover = markdown(&mut session, &main, MAIN, "fnCount()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Ledger`: read NO-LOCK"
    );
    // A method of another class, from the summaries of its file.
    let hover = markdown(&mut session, &main, MAIN, ":Count()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Item`: read SHARE-LOCK"
    );
    let hover = markdown(&mut session, &main, MAIN, "Quiet.");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\nNo database effects."
    );
}

#[test]
fn effects_through_a_run_are_marked_apart() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    // An effect the routine has itself is not repeated for a call: the
    // archive's EXCLUSIVE-LOCK read of `Item` shows as direct.
    let hover = markdown(&mut session, &main, MAIN, "Restock.");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\
         - `Item`: read EXCLUSIVE-LOCK, update\n\
         - `Item` through `ReadItems`: read NO-LOCK\n\
         - `Item` through `archive.p`: delete\n\
         - `Ledger` through `archive.p`: create, update"
    );
    // A RUN target program: its own effects are direct.
    let hover = markdown(&mut session, &main, MAIN, "jobs/archive.p.\nDISPLAY");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\
         - `Item`: read EXCLUSIVE-LOCK, delete\n\
         - `Ledger`: create, update"
    );
}

#[test]
fn an_unresolved_call_makes_the_summary_incomplete() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let hover = markdown(&mut session, &main, MAIN, "Unclear.");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\
         - `Item`: read NO-LOCK\n\n\
         **Incomplete:** effects may be missing: a dynamic call (`RUN VALUE`, \
         `DYNAMIC-FUNCTION`, ...); a program no file on the include path supplies."
    );
    // No effect found is not "no effects" when a call is not known.
    let hover = markdown(&mut session, &main, MAIN, "Opaque.");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\nNo known database effects.\n\n\
         **Incomplete:** effects may be missing: a dynamic call (`RUN VALUE`, \
         `DYNAMIC-FUNCTION`, ...)."
    );
}
