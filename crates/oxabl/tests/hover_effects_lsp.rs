//! The database effects that `textDocument/hover` adds for a procedure,
//! function, method or `RUN` target, through the real `oxabl lsp`: each table
//! with its operations and lock, direct effects apart from those through a
//! call, and a summary that may miss effects marked incomplete.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

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
    oStock:Count().\n\
    DEFINE VARIABLE oSealed AS CLASS app.Sealed NO-UNDO.\n\
    oSealed = NEW app.Sealed().\n\
    oSealed:Tally().\n\
    oStock:Fixed().\n\
    DEFINE VARIABLE oBase AS CLASS app.Base NO-UNDO.\n\
    oBase = NEW app.Kid().\n\
    oBase:Go().\n\
    DEFINE VARIABLE oThing AS CLASS app.IThing NO-UNDO.\n\
    oThing:Ping().\n\
    DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\n\
    PROCEDURE pSup IN SUPER:\n\
    END PROCEDURE.\n\
    PROCEDURE pLib IN hLib:\n\
    END PROCEDURE.\n\
    PROCEDURE pRem:\n\
    \x20   DEFINE VARIABLE hServer AS HANDLE NO-UNDO.\n\
    \x20   RUN jobs/archive.p ON SERVER hServer.\n\
    END PROCEDURE.\n\
    PROCEDURE pOuter:\n\
    \x20   RUN pRem.\n\
    END PROCEDURE.\n\
    RUN pSup.\n\
    RUN pLib.\n\
    RUN pRem.\n\
    RUN pOuter.\n";

/// An interface: the class that implements it supplies the body.
const ITHING: &str = "INTERFACE app.IThing:\n\
    \x20   METHOD PUBLIC VOID Ping().\n\
    END INTERFACE.\n";

/// A `FINAL` class: no subclass can override its methods.
const SEALED: &str = "CLASS app.Sealed FINAL:\n\
    \x20   METHOD PUBLIC INTEGER Tally():\n\
    \x20       FIND FIRST Ledger NO-LOCK NO-ERROR.\n\
    \x20       RETURN 1.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// An empty method that a subclass overrides with one that writes.
const BASE: &str = "CLASS app.Base:\n\
    \x20   METHOD PUBLIC VOID Go():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const KID: &str = "CLASS app.Kid INHERITS app.Base:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID Go():\n\
    \x20       FIND FIRST Item EXCLUSIVE-LOCK NO-ERROR.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const STOCK: &str = "CLASS app.Stock:\n\
    \x20   METHOD PUBLIC INTEGER Count():\n\
    \x20       FIND FIRST Item SHARE-LOCK NO-ERROR.\n\
    \x20       RETURN 1.\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC FINAL INTEGER Fixed():\n\
    \x20       FIND FIRST Item NO-LOCK NO-ERROR.\n\
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
            ("app/Sealed.cls", SEALED),
            ("app/Base.cls", BASE),
            ("app/Kid.cls", KID),
            ("app/IThing.cls", ITHING),
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
    // A method no subclass can override, from the summaries of its file.
    let hover = markdown(&mut session, &main, MAIN, ":Tally()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Ledger`: read NO-LOCK"
    );
    let hover = markdown(&mut session, &main, MAIN, ":Fixed()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Item`: read NO-LOCK"
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

    // Each route lists all that its routine does, also what the caller does
    // itself: the archive's EXCLUSIVE-LOCK read of `Item`.
    let hover = markdown(&mut session, &main, MAIN, "Restock.");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\
         - `Item`: read EXCLUSIVE-LOCK, update\n\
         - `Item` through `ReadItems`: read NO-LOCK\n\
         - `Item` through `archive.p`: read EXCLUSIVE-LOCK, delete\n\
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

#[test]
fn a_method_a_subclass_may_override_is_incomplete() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    // `app.Base:Go` has no effects, but `app.Kid` overrides it with a write.
    let hover = markdown(&mut session, &main, MAIN, ":Go()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\nNo known database effects.\n\n\
         **Incomplete:** effects may be missing: an override in a subclass may run \
         instead of this method."
    );
    let hover = markdown(&mut session, &main, MAIN, ":Count()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Item`: read SHARE-LOCK\n\n\
         **Incomplete:** effects may be missing: an override in a subclass may run \
         instead of this method."
    );
}

/// The hover on the first `needle` of `text`, and how long it took.
fn timed(session: &mut Session, file: &Path, text: &str, needle: &str) -> (String, Duration) {
    let started = Instant::now();
    let hover = markdown(session, file, text, needle);
    (hover, started.elapsed())
}

#[test]
fn the_summaries_of_another_file_are_built_once() {
    let ws = Workspace::new();
    // A class whose method runs a program with many procedures, so building
    // its summaries takes a while.
    let mut big = String::new();
    for n in 0..3000 {
        big.push_str(&format!(
            "PROCEDURE p{n}:\n    FIND FIRST Item NO-LOCK NO-ERROR.\nEND PROCEDURE.\n"
        ));
    }
    std::fs::write(ws.root.join("jobs/big.p"), big).unwrap();
    std::fs::write(
        ws.root.join("app/Big.cls"),
        "CLASS app.Big FINAL:\n    METHOD PUBLIC VOID Go():\n        RUN jobs/big.p.\n    \
         END METHOD.\nEND CLASS.\n",
    )
    .unwrap();
    let text = "DEFINE VARIABLE o AS CLASS app.Big NO-UNDO.\no = NEW app.Big().\no:Go().\n";
    let main = ws.root.join("big_user.p");
    std::fs::write(&main, text).unwrap();
    let mut session = Session::start(&ws.root);
    session.open(&main, text);

    let (first, cold) = timed(&mut session, &main, text, ":Go()");
    assert_eq!(
        effects(&first),
        "**Database effects**\n\nNo database effects."
    );
    for _ in 0..3 {
        let (again, warm) = timed(&mut session, &main, text, ":Go()");
        assert_eq!(again, first);
        assert!(
            warm * 3 < cold,
            "a second hover must reuse the summaries: {warm:?} against {cold:?} for the first"
        );
    }
}

#[test]
fn a_change_on_disk_reaches_the_summaries_of_another_file() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let hover = markdown(&mut session, &main, MAIN, ":Tally()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Ledger`: read NO-LOCK"
    );

    let sealed = ws.root.join("app/Sealed.cls");
    std::fs::write(
        &sealed,
        SEALED.replace("FIND FIRST Ledger NO-LOCK", "FIND FIRST Item NO-LOCK"),
    )
    .unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&sealed), "type": 2}]}
    }));
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        let hover = markdown(&mut session, &main, MAIN, ":Tally()");
        if effects(&hover) == "**Database effects**\n- `Item`: read NO-LOCK" {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "the hover still shows the old effects: {hover}"
        );
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// The effects part for a routine with no known effects and these reasons.
fn unknown_because(reasons: &str) -> String {
    format!(
        "**Database effects**\n\nNo known database effects.\n\n\
         **Incomplete:** effects may be missing: {reasons}."
    )
}

#[test]
fn a_body_that_runs_elsewhere_is_incomplete() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let cases = [
        ("pSup.", "the body runs in a super procedure"),
        // At the declaration: `RUN pLib` itself gives no hover.
        (
            "pLib IN",
            "the body runs in the procedure that the handle names",
        ),
        (
            ":Ping()",
            "the class that implements the interface supplies the body",
        ),
    ];
    for (needle, reason) in cases {
        let hover = markdown(&mut session, &main, MAIN, needle);
        assert_eq!(
            effects(&hover),
            unknown_because(reason),
            "hover on `{needle}`"
        );
    }
}

#[test]
fn a_program_run_on_an_appserver_makes_the_summary_incomplete() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let reason = "a program run on an AppServer (`RUN ... ON SERVER`)";
    // Itself, and through a routine it calls.
    for needle in ["pRem.\nRUN pOuter", "pOuter."] {
        let hover = markdown(&mut session, &main, MAIN, needle);
        assert_eq!(
            effects(&hover),
            unknown_because(reason),
            "hover on `{needle}`"
        );
    }
}

#[test]
fn unsaved_changes_in_a_file_read_from_disk_give_no_effects() {
    let ws = Workspace::new();
    let main = ws.root.join("main.p");
    let stock = ws.root.join("app/Stock.cls");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    // The class is open with a change that is not saved.
    session.open(
        &stock,
        &STOCK.replace("FIND FIRST Item SHARE-LOCK", "DELETE Item"),
    );

    let hover = markdown(&mut session, &main, MAIN, ":Count()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n\nNot known: `Stock.cls` has unsaved changes. Save to see \
         the effects."
    );
    // Open with the saved text, the effects are known.
    let main_only = ws.root.join("app/Sealed.cls");
    session.open(&main_only, SEALED);
    let hover = markdown(&mut session, &main, MAIN, ":Tally()");
    assert_eq!(
        effects(&hover),
        "**Database effects**\n- `Ledger`: read NO-LOCK"
    );
}
