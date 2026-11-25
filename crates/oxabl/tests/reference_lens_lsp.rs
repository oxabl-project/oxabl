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

/// The places find references gives at the name that `needle` starts, without
/// the declaration, as `(file, line)` pairs, sorted.
fn find_places(session: &mut Session, file: &Path, text: &str, needle: &str) -> Vec<(String, u64)> {
    let (line, character) = utf16_at(text, nth(text, needle, 0) + 1);
    let answer = session.request(
        "textDocument/references",
        json!({
            "textDocument": {"uri": file_uri(file)},
            "position": {"line": line, "character": character},
            "context": {"includeDeclaration": false}
        }),
    );
    assert!(answer.get("error").is_none(), "references gave {answer}");
    let mut found: Vec<(String, u64)> = answer["result"]
        .as_array()
        .unwrap()
        .iter()
        .map(|place| {
            (
                place["uri"].as_str().unwrap().to_string(),
                place["range"]["start"]["line"].as_u64().unwrap(),
            )
        })
        .collect();
    found.sort();
    found
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

/// What the count of a routine says when a call through a handle can name it.
const HANDLES: &str = "calls through a handle are not followed";

/// The title of a lens with `n` references and no gap.
fn exact(n: usize) -> String {
    format!("{n} reference{}", if n == 1 { "" } else { "s" })
}

/// The title of a lens with `n` found references and a reason that the count
/// can be short.
fn short(n: usize, reason: &str) -> String {
    format!("{n}+ references · incomplete: {reason}")
}

/// The places of a resolved lens, as `(file, line)` pairs, sorted.
fn listed_places(lens: &Value) -> Vec<(String, u64)> {
    let places = lens["command"]["arguments"][2]
        .as_array()
        .unwrap_or_else(|| panic!("the lens lists no places: {lens}"));
    let mut found: Vec<(String, u64)> = places
        .iter()
        .map(|place| {
            (
                place["uri"].as_str().unwrap().to_string(),
                place["range"]["start"]["line"].as_u64().unwrap(),
            )
        })
        .collect();
    found.sort();
    found
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
            // The program lens opens the list that it counts: one place for
            // each RUN of the program.
            let first = lenses(&mut session, &file).remove(0);
            let program = resolve(&mut session, &first);
            assert_eq!(
                listed_places(&program).len(),
                count_of(&found[0].1),
                "{name}: the program list has the length of the program count"
            );
            if name == "lib/util.p" {
                let mut want = vec![
                    (file_uri(&ws.path("main.p")), 2),
                    (file_uri(&ws.path("main.p")), 9),
                    (file_uri(&ws.path("other.p")), 3),
                    (file_uri(&ws.path("third.p")), 0),
                ];
                want.sort();
                assert_eq!(listed_places(&program), want, "{name}: the RUN places");
            }
        }
        for (((_, symbol), (_, got)), lens) in symbols
            .iter()
            .zip(found.iter().skip(skip))
            .zip(lenses(&mut session, &file).iter().skip(skip))
        {
            let (uses, with_declaration) = find_references(&mut session, &file, text, symbol);
            assert_eq!(count_of(got), uses, "{name} `{symbol}`: `{got}`");
            // The workspace has no call that a search cannot follow: every
            // count is exact.
            assert_eq!(got, &exact(uses), "{name} `{symbol}`");
            // The list the lens opens is the answer of find references.
            let listed = listed_places(&resolve(&mut session, lens));
            assert_eq!(listed.len(), uses, "{name} `{symbol}`: the list");
            assert_eq!(
                listed,
                find_places(&mut session, &file, text, symbol),
                "{name} `{symbol}`: the list is the find references answer"
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
            "4 references · used by 3 files".to_string(),
            exact(2),
            exact(1),
            exact(0),
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
    assert_eq!(title(&resolved), exact(2));
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
    let arguments = &resolved["command"]["arguments"];
    assert_eq!(arguments[0], json!(file_uri(&util)));
    assert_eq!(arguments[1], json!({"line": line, "character": character}));
    // The one use of `helper`: the `RUN helper.` of `doWork`.
    let (use_line, use_character) = utf16_at(UTIL, nth(UTIL, "helper.", 0));
    assert_eq!(
        arguments[2],
        json!([{
            "uri": file_uri(&util),
            "range": {
                "start": {"line": use_line, "character": use_character},
                "end": {"line": use_line, "character": use_character + 6},
            }
        }])
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
    assert_eq!(titles[1].1, exact(1), "{titles:?}");
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
    assert_eq!(found[1].1, exact(2));
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
    assert!(both.iter().any(|t| *t == exact(1)), "{both:?}");
    assert!(
        both.iter().any(|t| t == "0 references · used by 0 files"),
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

/// Workspace of a persistent library, a super procedure and two callers, one
/// through a handle that comes in as a parameter.
fn library_workspace() -> Workspace {
    Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib.p", "PROCEDURE doWork:\nEND PROCEDURE.\n"),
        (
            "a.p",
            "DEFINE VARIABLE h AS HANDLE NO-UNDO.\n\
             RUN lib.p PERSISTENT SET h.\n\
             SESSION:ADD-SUPER-PROCEDURE(h).\n\
             RUN b.p (INPUT h).\n\
             RUN c.p.\n",
        ),
        (
            "b.p",
            "DEFINE INPUT PARAMETER h AS HANDLE NO-UNDO.\nRUN doWork IN h.\n",
        ),
        ("c.p", "RUN doWork.\n"),
    ])
}

#[test]
fn a_routine_that_a_handle_can_call_never_has_an_exact_count() {
    let ws = library_workspace();
    let lib = ws.path("lib.p");
    let mut session = start(&ws);
    session.open(&lib, "PROCEDURE doWork:\nEND PROCEDURE.\n");
    let titles = resolved_titles(&mut session, &lib);
    // The program is run once, so its count is exact.
    assert_eq!(titles[0].1, "1 reference · used by 1 file", "{titles:?}");
    // `b.p` calls `doWork` through a handle it gets as a parameter, and `c.p`
    // through a super procedure: no bare number.
    let routine = &titles[1].1;
    assert!(
        routine.contains('+') && routine.contains(&format!("incomplete: {HANDLES}")),
        "a bare number for a routine a handle can call: {routine}"
    );
}

#[test]
fn a_standing_gap_is_written_to_the_log_once() {
    let ws = library_workspace();
    let lib = ws.path("lib.p");
    let mut session = start(&ws);
    session.open(&lib, "PROCEDURE doWork:\nEND PROCEDURE.\n");
    let listed = lenses(&mut session, &lib);
    let mut logged = Vec::new();
    for _ in 0..2 {
        let id = session.next_request_id();
        session.send(json!({
            "jsonrpc": "2.0", "id": id, "method": "codeLens/resolve", "params": listed[1]
        }));
        loop {
            let message = session
                .try_next(std::time::Duration::from_secs(60))
                .expect("no answer to the resolve");
            if message["id"] == json!(id) {
                break;
            }
            if message["method"] == "window/logMessage" {
                logged.push(message["params"]["message"].as_str().unwrap().to_string());
            }
        }
    }
    let standing: Vec<&String> = logged
        .iter()
        .filter(|text| text.contains("references and calls may be incomplete"))
        .collect();
    assert_eq!(standing.len(), 1, "the log holds {logged:?}");
    assert!(
        standing[0].contains("a RUN ... IN a handle"),
        "{standing:?}"
    );
}

/// The text of `util.p` with a marker that holds each search of its lenses.
fn held_util() -> String {
    format!("{UTIL}/* OXABL-TEST-HOLD:lsp.reference-lens@1500 */\n")
}

#[test]
fn a_second_resolve_of_an_unchanged_lens_runs_no_new_search() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");
    let mut session = start(&ws);
    session.open(&util, &held_util());
    let listed = lenses(&mut session, &util);
    let timed = |session: &mut Session| {
        let started = std::time::Instant::now();
        let lens = resolve(session, &listed[1]);
        (started.elapsed(), title(&lens))
    };
    // The marker holds a search for 1.5 s; an answer from the cache does not.
    let (first, first_title) = timed(&mut session);
    assert!(
        first >= std::time::Duration::from_millis(1400),
        "the first resolve ran no search: {first:?}"
    );
    let (second, second_title) = timed(&mut session);
    assert!(
        second < std::time::Duration::from_millis(1000),
        "the second resolve of an unchanged lens ran a search again: {second:?}"
    );
    assert_eq!(first_title, second_title);
}

#[test]
fn a_change_in_another_file_makes_a_new_search() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");
    let other = ws.path("other.p");
    let mut session = start(&ws);
    session.open(&util, UTIL);
    session.open(&other, OTHER);
    let listed = lenses(&mut session, &util);
    assert_eq!(title(&resolve(&mut session, &listed[1])), exact(2));
    // `other.p` stops calling `doWork`; the lens of `util.p` has the same
    // version and the same text.
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&other), "version": 2},
            "contentChanges": [{"text": "RUN lib/util.p.\n"}]
        }
    }));
    assert_eq!(title(&resolve(&mut session, &listed[1])), exact(1));
}

#[test]
fn a_static_client_that_was_not_told_lenses_resolve_gets_no_reference_lenses() {
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
    // Effect lenses are on, reference lenses are off: the client is told that
    // lenses do not resolve.
    let mut session = Session::start_with(&ws.root, client_capabilities());
    assert_eq!(
        session.initialized["result"]["capabilities"]["codeLensProvider"]["resolveProvider"],
        false
    );
    session.open(&prog, PROG);
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"referenceLenses": {"enable": true}}}}
    }));
    session.wait_for(|m| m["method"] == "workspace/codeLens/refresh");
    // Such a client would never resolve a lens that has no count.
    for lens in lenses(&mut session, &prog) {
        assert!(
            lens["data"].is_null(),
            "a lens that the client cannot resolve: {lens}"
        );
    }
}

#[test]
fn a_change_of_a_file_on_disk_makes_a_new_search() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");
    let other = ws.path("other.p");
    let mut session = start(&ws);
    session.open(&util, UTIL);
    let listed = lenses(&mut session, &util);
    assert_eq!(title(&resolve(&mut session, &listed[1])), exact(2));
    // `other.p` is not open: only the watcher tells of the change.
    std::fs::write(&other, "RUN lib/util.p.\n").unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&other), "type": 2}]}
    }));
    assert_eq!(title(&resolve(&mut session, &listed[1])), exact(1));
}

/// What a count shows for a call that no search follows.
const RUN_VALUE: &str = "a RUN VALUE can run it";
const DYNAMIC_NEW: &str = "a DYNAMIC-NEW can create it";
const DYNAMIC_INVOKE_CLASS: &str = "a DYNAMIC-INVOKE can name it";
const DYNAMIC_INVOKE_MEMBER: &str = "a DYNAMIC-INVOKE can call it";
const DYNAMIC_FUNCTION: &str = "a DYNAMIC-FUNCTION can call it";

/// The titles of the lenses of `name` once it is open.
fn titles_of(session: &mut Session, ws: &Workspace, name: &str, text: &str) -> Vec<String> {
    let file = ws.path(name);
    session.open(&file, text);
    resolved_titles(session, &file)
        .into_iter()
        .map(|(_, title)| title)
        .collect()
}

#[test]
fn a_program_that_a_run_value_can_run_has_no_exact_count() {
    const PROG: &str = "PROCEDURE hello:\nEND PROCEDURE.\n";
    const MENU: &str =
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO INITIAL \"prog.p\".\nRUN VALUE(c).\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("prog.p", PROG),
        ("menu.p", MENU),
        ("caller.p", "RUN prog.p.\n"),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "prog.p", PROG);
    assert_eq!(
        titles[0],
        format!("1+ references · used by 1+ files · incomplete: {RUN_VALUE}")
    );
    // The `RUN VALUE` of another file cannot run an internal procedure of
    // `prog.p`.
    assert_eq!(titles[1], exact(0), "{titles:?}");

    // The call goes: the counts are exact again.
    std::fs::write(ws.path("menu.p"), "RUN caller.p.\n").unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&ws.path("menu.p")), "type": 2}]}
    }));
    let file = ws.path("prog.p");
    let found = resolved_titles(&mut session, &file);
    assert_eq!(found[0].1, "1 reference · used by 1 file", "{found:?}");
}

#[test]
fn a_routine_that_a_run_value_in_its_file_can_run_has_no_exact_count() {
    const DISPATCH: &str = "PROCEDURE a:\nEND PROCEDURE.\nPROCEDURE b:\nEND PROCEDURE.\n\
        RUN VALUE(\"a\").\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("dispatch.p", DISPATCH),
        ("plain.p", "PROCEDURE c:\nEND PROCEDURE.\n"),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "dispatch.p", DISPATCH);
    assert_eq!(titles[1], short(0, RUN_VALUE), "{titles:?}");
    assert_eq!(titles[2], short(0, RUN_VALUE), "{titles:?}");
    // A file with no `RUN VALUE` of its own stays exact.
    let titles = titles_of(
        &mut session,
        &ws,
        "plain.p",
        "PROCEDURE c:\nEND PROCEDURE.\n",
    );
    assert_eq!(titles[1], exact(0), "{titles:?}");
}

#[test]
fn a_routine_is_exact_unless_a_call_through_a_handle_can_name_it() {
    const LIB: &str = "PROCEDURE doWork:\nEND PROCEDURE.\nPROCEDURE quiet:\nEND PROCEDURE.\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib.p", LIB),
        (
            "b.p",
            "DEFINE INPUT PARAMETER h AS HANDLE NO-UNDO.\nRUN doWork IN h.\n",
        ),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "lib.p", LIB);
    // A call through a handle names `doWork`, and no call names `quiet`.
    assert_eq!(titles[1], short(0, HANDLES), "{titles:?}");
    assert_eq!(titles[2], exact(0), "{titles:?}");

    // A call through a handle with a computed name, or with no name to read,
    // can reach any routine.
    for call in [
        "DEFINE INPUT PARAMETER h AS HANDLE NO-UNDO.\nDEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
         RUN VALUE(c) IN h.\n",
        "PROCEDURE over:\n    RUN SUPER.\nEND PROCEDURE.\n",
    ] {
        let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("lib.p", LIB), ("b.p", call)]);
        let mut session = start(&ws);
        let titles = titles_of(&mut session, &ws, "lib.p", LIB);
        assert_eq!(titles[1], short(0, HANDLES), "{call}: {titles:?}");
        assert_eq!(titles[2], short(0, HANDLES), "{call}: {titles:?}");
    }
}

#[test]
fn a_dynamic_new_or_invoke_makes_a_class_or_member_count_incomplete() {
    for (call, class, member) in [
        (
            "DEFINE VARIABLE o AS CLASS app.Calc NO-UNDO.\no = DYNAMIC-NEW \"app.Calc\" ().\n",
            Some(DYNAMIC_NEW),
            None,
        ),
        (
            "DEFINE VARIABLE o AS CLASS app.Calc NO-UNDO.\n\
             DYNAMIC-INVOKE(o, \"Add\", 1).\n",
            Some(DYNAMIC_INVOKE_CLASS),
            Some(DYNAMIC_INVOKE_MEMBER),
        ),
        (
            "DEFINE VARIABLE o AS CLASS app.Calc NO-UNDO.\no = NEW app.Calc().\no:Add(1).\n",
            None,
            None,
        ),
    ] {
        let ws = Workspace::new(&[
            ("oxabl.toml", CONFIG),
            ("app/Calc.cls", CALC),
            ("dyn.p", call),
        ]);
        let mut session = start(&ws);
        let titles = titles_of(&mut session, &ws, "app/Calc.cls", CALC);
        let file = ws.path("app/Calc.cls");
        // The class, its property, `Add` and `Twice`, in order.
        let uses =
            |needle: &str, session: &mut Session| find_references(session, &file, CALC, needle).0;
        let wanted = [
            ("Calc:", class),
            ("Total AS", member),
            ("Add(", member),
            ("Twice(", member),
        ];
        for ((needle, reason), got) in wanted.into_iter().zip(&titles) {
            let n = uses(needle, &mut session);
            let want = reason.map_or(exact(n), |reason| short(n, reason));
            assert_eq!(got, &want, "{call}: `{needle}` in {titles:?}");
        }
    }
}

#[test]
fn a_dynamic_function_makes_a_routine_count_incomplete() {
    const LIB: &str = "FUNCTION twice RETURNS INTEGER (INPUT i AS INTEGER):\n    RETURN i * 2.\n\
        END FUNCTION.\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib.p", LIB),
        (
            "dyn.p",
            "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = DYNAMIC-FUNCTION(\"twice\", 2).\n",
        ),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "lib.p", LIB);
    assert_eq!(titles[1], short(0, DYNAMIC_FUNCTION), "{titles:?}");
    // No `RUN VALUE`: the program count is exact.
    assert_eq!(titles[0], "0 references · used by 0 files", "{titles:?}");
}

const CALLER: &str = "DEFINE VARIABLE o AS CLASS app.Calc NO-UNDO.\no = NEW app.Calc().\n\
    o:Add(1).\n";

/// The index of the lens of `Add` among those of `app/Calc.cls`.
const ADD: usize = 2;

#[test]
fn a_count_follows_a_change_on_disk_that_no_watcher_reports() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Calc.cls", CALC),
        ("a.p", CALLER),
    ]);
    let calc = ws.path("app/Calc.cls");
    let mut session = start(&ws);
    session.open(&calc, CALC);
    let listed = lenses(&mut session, &calc);
    assert_eq!(title(&resolve(&mut session, &listed[ADD])), exact(1));
    // A second call in `a.p` and a new file, with no notification: a client
    // with no file watcher says nothing.
    std::fs::write(ws.path("a.p"), format!("{CALLER}o:Add(2).\n")).unwrap();
    std::fs::write(ws.path("b.p"), CALLER).unwrap();
    // The workspace looks for new files once each second.
    std::thread::sleep(std::time::Duration::from_millis(1500));
    let (uses, _) = find_references(&mut session, &calc, CALC, "Add(");
    assert_eq!(uses, 3, "find references");
    assert_eq!(title(&resolve(&mut session, &listed[ADD])), exact(uses));
}

/// `Calc.cls` with a marker that holds each search of its lenses after the
/// answer, before the title is kept.
fn held_calc() -> String {
    format!("{CALC}/* OXABL-TEST-HOLD:lsp.answered-reference-lens@1500 */\n")
}

#[test]
fn a_change_on_disk_during_a_search_is_not_kept_as_its_answer() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Calc.cls", CALC),
        ("a.p", CALLER),
    ]);
    let calc = ws.path("app/Calc.cls");
    let mut session = start(&ws);
    session.open(&calc, &held_calc());
    let listed = lenses(&mut session, &calc);
    // The workspace pass exists once a first lens has its answer.
    assert_eq!(title(&resolve(&mut session, &listed[ADD + 1])), exact(1));
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "codeLens/resolve", "params": listed[ADD]
    }));
    // The search has its answer and the marker holds it.
    std::thread::sleep(std::time::Duration::from_millis(500));
    std::fs::write(ws.path("b.p"), CALLER).unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&ws.path("b.p")), "type": 1}]}
    }));
    let first = session.wait_for(|m| m["id"] == json!(id));
    assert_eq!(title(&first["result"]), exact(1));
    // The answer is as old as the change: the next resolve looks again.
    assert_eq!(title(&resolve(&mut session, &listed[ADD])), exact(2));
}

/// Whether the server asks for the lenses again within `wait`.
fn refresh_comes(session: &Session, wait: std::time::Duration) -> bool {
    let deadline = std::time::Instant::now() + wait;
    while let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) {
        match session.try_next(left) {
            Some(message) if message["method"] == "workspace/codeLens/refresh" => return true,
            Some(_) => {}
            None => return false,
        }
    }
    false
}

#[test]
fn the_client_is_asked_for_the_lenses_again_when_another_file_can_have_changed_them() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");
    let other = ws.path("other.p");
    let mut session = start(&ws);
    session.open(&util, UTIL);
    let _ = lenses(&mut session, &util);
    let wait = std::time::Duration::from_secs(10);
    let quiet = std::time::Duration::from_millis(2500);
    // Nothing changed yet.
    assert!(!refresh_comes(&session, quiet), "a refresh with no change");

    // An edit of the one file with lenses: the client asks for that file.
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&util), "version": 2},
            "contentChanges": [{"text": format!("\n{UTIL}")}]
        }
    }));
    assert!(
        !refresh_comes(&session, quiet),
        "a refresh for the edited file"
    );

    // A save.
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didSave",
        "params": {"textDocument": {"uri": file_uri(&util)}}
    }));
    assert!(refresh_comes(&session, wait), "no refresh after a save");

    // A change on disk.
    std::fs::write(&other, "RUN lib/util.p.\n").unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&other), "type": 2}]}
    }));
    assert!(
        refresh_comes(&session, wait),
        "no refresh after a disk change"
    );

    // An edit of another open file.
    session.open(&other, "RUN lib/util.p.\n");
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&other), "version": 2},
            "contentChanges": [{"text": "RUN lib/util.p.\nRUN lib/util.p.\n"}]
        }
    }));
    assert!(
        refresh_comes(&session, wait),
        "no refresh after an edit of another file"
    );
}
