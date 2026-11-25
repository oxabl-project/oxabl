//! Reference-count code lenses through the real `oxabl lsp` and its daemon:
//! the count equals the answer of find references, the lens resolves lazily,
//! a count that can be short says so, and the setting is the user's own, apart
//! from the effect lenses.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"lenses\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

const MAIN: &str = "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\n\
    DEFINE VARIABLE oCalc AS CLASS app.Calc NO-UNDO.\n\
    RUN lib/util.p PERSISTENT SET hLib.\n\
    RUN doWork IN hLib.\n\
    RUN localStep.\n\
    oCalc = NEW app.Calc().\n\
    oCalc:Add(1).\n\
    oCalc:Total = 2.\n\
    PROCEDURE localStep:\n\
    \x20   RUN lib/util.p.\n\
    END PROCEDURE.\n\
    FUNCTION fnTwice RETURNS INTEGER (INPUT piValue AS INTEGER):\n\
    \x20   RETURN piValue * 2.\n\
    END FUNCTION.\n";

const UTIL: &str = "PROCEDURE doWork:\n\
    \x20   RUN helper.\n\
    END PROCEDURE.\n\
    PROCEDURE helper:\n\
    END PROCEDURE.\n\
    PROCEDURE unused:\n\
    END PROCEDURE.\n";

const OTHER: &str = "DEFINE VARIABLE hUtil AS HANDLE NO-UNDO.\n\
    DEFINE VARIABLE oOther AS CLASS app.Calc NO-UNDO.\n\
    DEFINE VARIABLE oShape AS CLASS app.IShape NO-UNDO.\n\
    RUN lib/util.p PERSISTENT SET hUtil.\n\
    RUN doWork IN hUtil.\n\
    oOther = NEW app.Calc().\n\
    oOther:Add(2).\n";

const THIRD: &str = "RUN lib/util.p.\n";

const CALC: &str = "CLASS app.Calc:\n\
    \x20   DEFINE PUBLIC PROPERTY Total AS INTEGER NO-UNDO GET. SET.\n\
    \x20   METHOD PUBLIC INTEGER Add(INPUT piValue AS INTEGER):\n\
    \x20       RETURN Twice(piValue).\n\
    \x20   END METHOD.\n\
    \x20   METHOD PRIVATE INTEGER Twice(INPUT piValue AS INTEGER):\n\
    \x20       RETURN piValue * 2.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const SHAPE: &str = "INTERFACE app.IShape:\n\
    \x20   METHOD PUBLIC INTEGER Area().\n\
    END INTERFACE.\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new(files: &[(&str, &str)]) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        for (name, text) in files {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
    }

    fn standard() -> Workspace {
        Workspace::new(&[
            ("oxabl.toml", CONFIG),
            ("main.p", MAIN),
            ("lib/util.p", UTIL),
            ("other.p", OTHER),
            ("third.p", THIRD),
            ("app/Calc.cls", CALC),
            ("app/IShape.cls", SHAPE),
        ])
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }
}

fn client_capabilities() -> Value {
    json!({
        "textDocument": {"codeLens": {"dynamicRegistration": false}},
        "workspace": {"codeLens": {"refreshSupport": true}}
    })
}

fn start(ws: &Workspace) -> Session {
    Session::start_with_options(
        &ws.root,
        client_capabilities(),
        json!({"referenceLenses": {"enable": true}}),
    )
}

fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

#[track_caller]
fn nth(text: &str, needle: &str, nth: usize) -> usize {
    text.match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("no occurrence {nth} of `{needle}`"))
        .0
}

/// The text document params for `file`.
fn document(file: &Path) -> Value {
    json!({"textDocument": {"uri": file_uri(file)}})
}

/// The lenses `textDocument/codeLens` gives for an open `file`.
fn lenses(session: &mut Session, file: &Path) -> Vec<Value> {
    let answer = session.request("textDocument/codeLens", document(file));
    assert!(answer.get("error").is_none(), "codeLens gave {answer}");
    answer["result"].as_array().unwrap().clone()
}

/// The answer to `codeLens/resolve` for `lens`: the whole response.
fn resolve_raw(session: &mut Session, lens: &Value) -> Value {
    session.request("codeLens/resolve", lens.clone())
}

/// The resolved lens.
#[track_caller]
fn resolve(session: &mut Session, lens: &Value) -> Value {
    let answer = resolve_raw(session, lens);
    assert!(answer.get("error").is_none(), "resolve gave {answer}");
    answer["result"].clone()
}

fn title(lens: &Value) -> String {
    lens["command"]["title"]
        .as_str()
        .unwrap_or_else(|| panic!("the lens has no title: {lens}"))
        .to_string()
}

/// The line of `needle`, in `text`.
fn line_of(text: &str, needle: &str) -> u64 {
    utf16_at(text, nth(text, needle, 0)).0
}

/// The titles of the resolved lenses of `file`, by the line they sit on.
fn resolved_titles(session: &mut Session, file: &Path) -> Vec<(u64, String)> {
    let mut found = Vec::new();
    for lens in lenses(session, file) {
        let line = lens["range"]["start"]["line"].as_u64().unwrap();
        let resolved = resolve(session, &lens);
        found.push((line, title(&resolved)));
    }
    found
}

/// The length of the find references answer at the name that `needle` starts
/// (one character in), without and with the declaration.
fn find_references(session: &mut Session, file: &Path, text: &str, needle: &str) -> (usize, usize) {
    let mut counts = [0, 0];
    for (at, include_declaration) in [false, true].into_iter().enumerate() {
        let (line, character) = utf16_at(text, nth(text, needle, 0) + 1);
        let answer = session.request(
            "textDocument/references",
            json!({
                "textDocument": {"uri": file_uri(file)},
                "position": {"line": line, "character": character},
                "context": {"includeDeclaration": include_declaration}
            }),
        );
        assert!(answer.get("error").is_none(), "references gave {answer}");
        counts[at] = answer["result"].as_array().map_or(0, Vec::len);
    }
    (counts[0], counts[1])
}

/// The number at the start of a title (`3 references`, `12+ references`).
#[track_caller]
fn count_of(title: &str) -> usize {
    title
        .split(|c: char| !c.is_ascii_digit())
        .next()
        .and_then(|digits| digits.parse().ok())
        .unwrap_or_else(|| panic!("`{title}` starts with no count"))
}

/// A file, its text, and the declaration lines and names of its lenses.
type Case<'a> = (&'a str, &'a str, &'a [(&'a str, &'a str)]);

#[test]
fn the_count_of_each_lens_is_the_length_of_find_references() {
    let ws = Workspace::standard();
    let mut session = start(&ws);
    let cases: [Case; 5] = [
        (
            "lib/util.p",
            UTIL,
            &[
                ("PROCEDURE doWork:", "doWork:"),
                ("PROCEDURE helper:", "helper:"),
                ("PROCEDURE unused:", "unused:"),
            ],
        ),
        (
            "main.p",
            MAIN,
            &[
                ("PROCEDURE localStep:", "localStep:"),
                ("FUNCTION fnTwice", "fnTwice"),
            ],
        ),
        (
            "app/Calc.cls",
            CALC,
            &[
                ("CLASS app.Calc", "Calc:"),
                ("DEFINE PUBLIC PROPERTY Total", "Total AS"),
                ("METHOD PUBLIC INTEGER Add", "Add("),
                ("METHOD PRIVATE INTEGER Twice", "Twice("),
            ],
        ),
        (
            "app/IShape.cls",
            SHAPE,
            &[
                ("INTERFACE app.IShape", "IShape:"),
                ("METHOD PUBLIC INTEGER Area", "Area()"),
            ],
        ),
        ("third.p", THIRD, &[]),
    ];
    for (name, text, symbols) in cases {
        let file = ws.path(name);
        session.open(&file, text);
        let found = resolved_titles(&mut session, &file);
        let is_program = name.ends_with(".p");
        let skip = usize::from(is_program);
        // One lens for each declaration, and one for a program, in order.
        let lines: Vec<u64> = found.iter().skip(skip).map(|(line, _)| *line).collect();
        let want: Vec<u64> = symbols
            .iter()
            .map(|(header, _)| line_of(text, header))
            .collect();
        assert_eq!(lines, want, "{name}: the lenses {found:?}");
        if is_program {
            assert_eq!(found[0].0, 0, "{name}: the program lens is on line 0");
        }
        for ((_, symbol), (_, got)) in symbols.iter().zip(found.iter().skip(skip)) {
            let (uses, with_declaration) = find_references(&mut session, &file, text, symbol);
            assert_eq!(count_of(got), uses, "{name} `{symbol}`: `{got}`");
            assert!(
                !got.contains('+') && !got.contains("incomplete"),
                "{name} `{symbol}`: `{got}` is exact"
            );
            assert_eq!(
                with_declaration - uses,
                1,
                "{name} `{symbol}`: the lens leaves out the declaration"
            );
        }
    }
}

#[test]
fn counts_are_the_uses_in_the_workspace() {
    let ws = Workspace::standard();
    let mut session = start(&ws);
    let util = ws.path("lib/util.p");
    session.open(&util, UTIL);
    let titles: Vec<String> = resolved_titles(&mut session, &util)
        .into_iter()
        .map(|(_, title)| title)
        .collect();
    // The program is run four times in three files; `doWork` from two
    // files; `helper` once; `unused` never.
    assert_eq!(
        titles,
        [
            "4 references · used by 3 files",
            "2 references",
            "1 reference",
            "0 references",
        ]
    );
    let calc = ws.path("app/Calc.cls");
    session.open(&calc, CALC);
    let titles: Vec<String> = resolved_titles(&mut session, &calc)
        .into_iter()
        .map(|(_, title)| title)
        .collect();
    // A class: the places that name it.
    assert_eq!(count_of(&titles[0]), 4, "{titles:?}");
    assert_eq!(titles[1..], ["1 reference", "2 references", "1 reference"]);
}

#[test]
fn the_lenses_have_no_count_until_they_resolve() {
    let ws = Workspace::standard();
    let mut session = start(&ws);
    assert_eq!(
        session.initialized["result"]["capabilities"]["codeLensProvider"]["resolveProvider"],
        true
    );
    let util = ws.path("lib/util.p");
    session.open(&util, UTIL);
    let listed = lenses(&mut session, &util);
    assert_eq!(listed.len(), 4, "{listed:?}");
    for lens in &listed {
        assert!(
            lens.get("command").is_none_or(Value::is_null),
            "a listed lens has a count: {lens}"
        );
        assert!(!lens["data"].is_null(), "a listed lens has no data: {lens}");
    }
    let resolved = resolve(&mut session, &listed[1]);
    assert_eq!(resolved["range"], listed[1]["range"]);
    assert_eq!(title(&resolved), "2 references");
}

#[test]
fn a_resolved_lens_opens_the_references_of_its_symbol() {
    let ws = Workspace::standard();
    let mut session = start(&ws);
    let util = ws.path("lib/util.p");
    session.open(&util, UTIL);
    let listed = lenses(&mut session, &util);
    let resolved = resolve(&mut session, &listed[2]);
    assert_eq!(resolved["command"]["command"], "oxabl.showReferences");
    let (line, character) = utf16_at(UTIL, nth(UTIL, "helper:", 0));
    assert_eq!(
        resolved["command"]["arguments"],
        json!([file_uri(&util), {"line": line, "character": character}])
    );
}

#[test]
fn a_count_that_can_be_short_says_so() {
    // No include path: other files cannot be searched.
    let ws = Workspace::new(&[("main.p", MAIN), ("lib/util.p", UTIL), ("other.p", OTHER)]);
    let main = ws.path("main.p");
    let mut session = start(&ws);
    session.open(&main, MAIN);
    let titles = resolved_titles(&mut session, &main);
    let local_step = &titles[1].1;
    assert!(
        local_step.starts_with("1+ references · incomplete: other files are not searched"),
        "{titles:?}"
    );
    assert!(local_step.contains("no include path"), "{titles:?}");
    // The same workspace with an include path is exact.
    let ws = Workspace::standard();
    let main = ws.path("main.p");
    let mut session = start(&ws);
    session.open(&main, MAIN);
    let titles = resolved_titles(&mut session, &main);
    assert_eq!(titles[1].1, "1 reference", "{titles:?}");
}

#[test]
fn a_searched_file_with_a_parse_error_makes_the_count_incomplete() {
    let damaged = "RUN lib/util.p.\nFIND FIRST Item. Item.ItemNum = 2.\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib/util.p", UTIL),
        ("damaged.p", damaged),
    ]);
    let util = ws.path("lib/util.p");
    let mut session = start(&ws);
    session.open(&util, UTIL);
    let titles = resolved_titles(&mut session, &util);
    assert!(
        titles[0].1.contains("+ references") && titles[0].1.contains("incomplete"),
        "{titles:?}"
    );
    assert!(titles[0].1.contains("damaged.p"), "{titles:?}");
}

#[test]
fn a_stale_lens_is_refused() {
    let ws = Workspace::standard();
    let mut session = start(&ws);
    let util = ws.path("lib/util.p");
    session.open(&util, UTIL);
    let listed = lenses(&mut session, &util);
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&util), "version": 2},
            "contentChanges": [{"text": format!("\n{UTIL}")}]
        }
    }));
    let answer = resolve_raw(&mut session, &listed[1]);
    // ContentModified.
    assert_eq!(answer["error"]["code"], -32801, "{answer}");
    // The lenses of the new text resolve.
    let found = resolved_titles(&mut session, &util);
    assert_eq!(found[1].1, "2 references");
}

#[test]
fn the_setting_turns_the_lenses_on_and_off() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");

    // Off until the user turns it on.
    let mut session = Session::start_with(&ws.root, client_capabilities());
    assert!(
        session.initialized["result"]["capabilities"]
            .get("codeLensProvider")
            .is_none()
    );
    session.open(&util, UTIL);
    assert!(lenses(&mut session, &util).is_empty());
    for off in [
        json!({"referenceLenses": false}),
        json!({"referenceLenses": {"enable": false}}),
    ] {
        let mut session = Session::start_with_options(&ws.root, client_capabilities(), off.clone());
        session.open(&util, UTIL);
        assert!(lenses(&mut session, &util).is_empty(), "{off}");
    }

    // On from the initialization options, off and on again with the settings.
    let mut session = start(&ws);
    session.open(&util, UTIL);
    assert_eq!(lenses(&mut session, &util).len(), 4);
    let change = |session: &mut Session, enable: bool| {
        session.send(json!({
            "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
            "params": {"settings": {"oxabl": {"referenceLenses": {"enable": enable}}}}
        }));
    };
    change(&mut session, false);
    session.wait_for(|m| m["method"] == "workspace/codeLens/refresh");
    assert!(lenses(&mut session, &util).is_empty());
    change(&mut session, true);
    session.wait_for(|m| m["method"] == "workspace/codeLens/refresh");
    assert_eq!(lenses(&mut session, &util).len(), 4);
}

#[test]
fn a_client_that_registers_lenses_gets_a_resolving_registration() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");
    let capabilities = json!({
        "textDocument": {"codeLens": {"dynamicRegistration": true}},
        "workspace": {"codeLens": {"refreshSupport": true}}
    });
    let mut session = Session::start_with_options(
        &ws.root,
        capabilities,
        json!({"referenceLenses": {"enable": true}}),
    );
    assert!(
        session.initialized["result"]["capabilities"]
            .get("codeLensProvider")
            .is_none()
    );
    session.open(&util, UTIL);
    let registration = session.wait_for(|m| {
        m["method"] == "client/registerCapability"
            && m["params"]["registrations"][0]["method"] == "textDocument/codeLens"
    });
    let registration = &registration["params"]["registrations"][0];
    assert_eq!(registration["method"], "textDocument/codeLens");
    assert_eq!(registration["registerOptions"]["resolveProvider"], true);
    let pattern = registration["registerOptions"]["documentSelector"][0]["pattern"]
        .as_str()
        .unwrap();
    assert_eq!(pattern, util.to_string_lossy());
}

#[test]
fn effect_lenses_and_reference_lenses_are_apart() {
    const SCHEMA: &str = "ADD TABLE \"customer\"\n\
        ADD FIELD \"id\" OF \"customer\" AS integer\n\
        ADD INDEX \"pk\" ON \"customer\" IS PRIMARY UNIQUE\n\
        INDEX-FIELD \"id\" ASCENDING\n";
    const EFFECTS: &str = "[workspace]\nname = \"lenses\"\n\
        [workspace.sources]\ninclude_paths = [\".\"]\n\
        [workspace.schema]\nfiles = [\"schema.df\"]\n\
        [workspace.lint]\nenable = [\"strict.access\"]\n";
    const PROG: &str =
        "PROCEDURE load:\n    FIND FIRST customer NO-LOCK NO-ERROR.\nEND PROCEDURE.\nRUN load.\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", EFFECTS),
        ("schema.df", SCHEMA),
        ("prog.p", PROG),
    ]);
    let prog = ws.path("prog.p");

    let titles = |session: &mut Session| -> Vec<String> {
        // The effect lenses come when their worker is done.
        for _ in 0..200 {
            let found = lenses(session, &prog);
            let resolved: Vec<Value> = found
                .iter()
                .map(|lens| {
                    if lens["command"].is_null() {
                        resolve(session, lens)
                    } else {
                        lens.clone()
                    }
                })
                .collect();
            if resolved
                .iter()
                .any(|lens| title(lens).starts_with("reads customer"))
            {
                return resolved.iter().map(title).collect();
            }
            std::thread::sleep(std::time::Duration::from_millis(20));
        }
        panic!("the effect lenses did not come");
    };

    // The effect rule alone gives effect lenses, no count.
    let mut session = Session::start_with(&ws.root, client_capabilities());
    session.open(&prog, PROG);
    let only_effects = titles(&mut session);
    assert!(
        only_effects.iter().all(|t| !t.contains("reference")),
        "{only_effects:?}"
    );

    // Reference lenses add their own and keep the effect lenses.
    let mut session = start(&ws);
    session.open(&prog, PROG);
    let both = titles(&mut session);
    assert!(
        both.iter().any(|t| t.starts_with("reads customer")),
        "{both:?}"
    );
    assert!(both.iter().any(|t| t == "1 reference"), "{both:?}");
    assert!(
        both.iter().any(|t| t.starts_with("reads customer")),
        "{both:?}"
    );

    // Turning the rule off leaves the reference lenses alone.
    let plain = Workspace::new(&[("oxabl.toml", CONFIG), ("prog.p", PROG)]);
    let prog = plain.path("prog.p");
    let mut session = start(&plain);
    session.open(&prog, PROG);
    let found = resolved_titles(&mut session, &prog);
    assert!(
        found.iter().all(|(_, title)| title.contains("reference")),
        "{found:?}"
    );
}

#[test]
fn a_declaration_oxabl_does_not_resolve_has_no_count() {
    // The branch is not compiled, so the preprocessor drops the procedure.
    const SKIPPED: &str =
        "&IF 1 = 0 &THEN\nPROCEDURE skipped:\nEND PROCEDURE.\n&ENDIF\nRUN skipped.\n";
    let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("skipped.p", SKIPPED)]);
    let file = ws.path("skipped.p");
    let mut session = start(&ws);
    session.open(&file, SKIPPED);
    let listed = lenses(&mut session, &file);
    let resolved = resolve(&mut session, &listed[1]);
    assert_eq!(
        title(&resolved),
        "references not known: oxabl does not resolve this name"
    );
    // It opens nothing.
    assert_eq!(resolved["command"]["command"], "");
}
