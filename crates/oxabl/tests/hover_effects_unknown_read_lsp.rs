//! The hover of a routine whose summary holds a read of a table that the
//! loaded schema does not have says the effects may be incomplete, through the
//! real `oxabl lsp`. All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"effects\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n\
    [workspace.schema]\nfiles = [\"shop.df\"]\n";

const SCHEMA: &str = "ADD TABLE \"Item\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"ItemNum\" OF \"Item\" AS integer\n  FORMAT \">>>>9\"\n\n";

const MAIN: &str = "PROCEDURE Known:\n\
    \x20   FIND FIRST Item NO-LOCK NO-ERROR.\n\
    END PROCEDURE.\n\
    PROCEDURE Unknown:\n\
    \x20   FIND FIRST Missing NO-LOCK NO-ERROR.\n\
    END PROCEDURE.\n\
    PROCEDURE Outer:\n\
    \x20   RUN Unknown.\n\
    END PROCEDURE.\n\
    RUN Known.\n\
    RUN Unknown.\n\
    RUN Outer.\n";

fn hover(session: &mut Session, file: &Path, text: &str, needle: &str) -> String {
    let at = text
        .rfind(needle)
        .unwrap_or_else(|| panic!("no `{needle}`"))
        + 1;
    let line = text[..at].matches('\n').count();
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let character = text[line_start..at].encode_utf16().count();
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

fn effects(hover: &str) -> &str {
    hover
        .split_once("\n\n---\n\n")
        .unwrap_or_else(|| panic!("no effects in the hover: {hover}"))
        .1
}

const INCOMPLETE: &str = "\n\n**Incomplete:** effects may be missing: a read or write of a table the schema does not have.";

#[test]
fn a_read_of_an_unknown_table_makes_the_hover_incomplete() {
    let dir = tempfile::tempdir().unwrap();
    let main = dir.path().join("main.p");
    for (name, text) in [
        ("oxabl.toml", CONFIG),
        ("shop.df", SCHEMA),
        ("main.p", MAIN),
    ] {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    let mut session = Session::start(dir.path());
    session.open(&main, MAIN);

    let known = hover(&mut session, &main, MAIN, "Known.");
    assert_eq!(
        effects(&known),
        "**Database effects**\n- `Item`: read NO-LOCK"
    );

    let unknown = hover(&mut session, &main, MAIN, "Unknown.\nRUN Outer");
    assert_eq!(
        effects(&unknown),
        format!("**Database effects**\n- `Missing` (not in the schema): read NO-LOCK{INCOMPLETE}")
    );

    let outer = hover(&mut session, &main, MAIN, "Outer.");
    assert_eq!(
        effects(&outer),
        format!(
            "**Database effects**\n- `Missing` (not in the schema) through `Unknown`: read NO-LOCK{INCOMPLETE}"
        )
    );
}
