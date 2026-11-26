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

/// The title of a lens of a procedure or function with `n` found references:
/// a call that no search follows can reach any of them.
fn routine_title(n: usize) -> String {
    short(n, HANDLES)
}

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
        for (((header, symbol), (_, got)), lens) in symbols
            .iter()
            .zip(found.iter().skip(skip))
            .zip(lenses(&mut session, &file).iter().skip(skip))
        {
            let (uses, with_declaration) = find_references(&mut session, &file, text, symbol);
            assert_eq!(count_of(got), uses, "{name} `{symbol}`: `{got}`");
            // The workspace has no dynamic call: only a routine is marked.
            let routine = header.starts_with("PROCEDURE") || header.starts_with("FUNCTION");
            let want = if routine {
                routine_title(uses)
            } else {
                exact(uses)
            };
            assert_eq!(got, &want, "{name} `{symbol}`");
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
            routine_title(2),
            routine_title(1),
            routine_title(0),
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
    assert_eq!(title(&resolved), routine_title(2));
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
    assert_eq!(titles[1].1, routine_title(1), "{titles:?}");
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
    // The gap of the damaged file comes first and names it; the mark of an
    // unknown file follows.
    assert!(titles[0].1.contains("damaged.p"), "{titles:?}");
    assert!(titles[0].1.contains("(+1 more)"), "{titles:?}");
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
    assert_eq!(found[1].1, routine_title(2));
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
    assert!(both.iter().any(|t| *t == routine_title(1)), "{both:?}");
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
    assert_eq!(title(&resolve(&mut session, &listed[1])), routine_title(2));
    // `other.p` stops calling `doWork`; the lens of `util.p` has the same
    // version and the same text.
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&other), "version": 2},
            "contentChanges": [{"text": "RUN lib/util.p.\n"}]
        }
    }));
    assert_eq!(title(&resolve(&mut session, &listed[1])), routine_title(1));
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
    assert_eq!(title(&resolve(&mut session, &listed[1])), routine_title(2));
    // `other.p` is not open: only the watcher tells of the change.
    std::fs::write(&other, "RUN lib/util.p.\n").unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&other), "type": 2}]}
    }));
    assert_eq!(title(&resolve(&mut session, &listed[1])), routine_title(1));
}

/// What a count shows for a call that no search follows.
const RUN_VALUE: &str = "a RUN VALUE can run it";
const DYNAMIC_NEW: &str = "a DYNAMIC-NEW can create it";
const DYNAMIC_INVOKE_CLASS: &str = "a DYNAMIC-INVOKE can name it";
const DYNAMIC_INVOKE_MEMBER: &str = "a DYNAMIC-INVOKE can call it";
const DYNAMIC_PROPERTY: &str = "a DYNAMIC-PROPERTY can name it";
const REFLECTION: &str = "a class reflection call can reach it";
const UNKNOWN_FILE: &str = "a file oxabl cannot read can reach it";

/// The titles of the lenses of `name` once it is open.
fn titles_of(session: &mut Session, ws: &Workspace, name: &str, text: &str) -> Vec<String> {
    let file = ws.path(name);
    session.open(&file, text);
    resolved_titles(session, &file)
        .into_iter()
        .map(|(_, title)| title)
        .collect()
}

/// The library that the calls of the next tests can reach: a program with two
/// procedures and a function.
const LIB: &str = "PROCEDURE helper:\nEND PROCEDURE.\nPROCEDURE onEvt:\nEND PROCEDURE.\n\
    FUNCTION fx RETURNS INTEGER ():\n    RETURN 1.\nEND FUNCTION.\n";

#[test]
fn a_routine_that_a_call_no_search_follows_can_reach_never_has_an_exact_count() {
    // Each file runs a routine of `lib.p` in a way that the search does not
    // follow. The program count stays exact where no `RUN VALUE` exists.
    let cases: [(&str, &[(&str, &str)]); 5] = [
        (
            "a static RUN that a super procedure answers",
            &[(
                "main.p",
                "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nRUN lib.p PERSISTENT SET h.\n\
                 SESSION:ADD-SUPER-PROCEDURE(h).\nRUN helper.\n",
            )],
        ),
        (
            "an event that a PUBLISH sends to a SUBSCRIBE",
            &[
                ("lib2.p", "SUBSCRIBE TO \"onEvt\" ANYWHERE.\n"),
                ("main.p", "PUBLISH \"onEvt\".\n"),
            ],
        ),
        (
            "a persistent trigger",
            &[(
                "main.p",
                "DEFINE BUTTON b LABEL \"x\".\nDEFINE VARIABLE h AS HANDLE NO-UNDO.\n\
                 ON CHOOSE OF b PERSISTENT RUN helper IN h.\n",
            )],
        ),
        (
            "a callback",
            &[(
                "main.p",
                "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nDEFINE VARIABLE q AS HANDLE NO-UNDO.\n\
                 q:SET-CALLBACK-PROCEDURE(\"OFF-END\", \"helper\", h).\n",
            )],
        ),
        (
            "a RUN VALUE in another file",
            &[(
                "main.p",
                "DEFINE VARIABLE c AS CHARACTER NO-UNDO INITIAL \"helper\".\nRUN VALUE(c).\n",
            )],
        ),
    ];
    for (what, files) in cases {
        let mut all = vec![("oxabl.toml", CONFIG), ("lib.p", LIB)];
        all.extend_from_slice(files);
        let ws = Workspace::new(&all);
        let mut session = start(&ws);
        let titles = titles_of(&mut session, &ws, "lib.p", LIB);
        // The program, `helper`, `onEvt` and `fx`.
        assert_eq!(titles.len(), 4, "{what}: {titles:?}");
        for routine in &titles[1..] {
            assert!(
                routine.starts_with("0+ references")
                    && routine.contains(&format!("incomplete: {HANDLES}")),
                "{what}: a routine that a call no search follows can reach shows `{routine}`"
            );
        }
    }
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
    assert_eq!(titles[1], routine_title(0), "{titles:?}");

    // The call goes: the program count is exact again.
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
fn a_call_object_makes_a_program_count_incomplete() {
    const PROG: &str = "PROCEDURE hello:\nEND PROCEDURE.\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("prog.p", PROG),
        (
            "call.p",
            "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nCREATE CALL h.\n",
        ),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "prog.p", PROG);
    assert_eq!(
        titles[0],
        format!("0+ references · used by 0+ files · incomplete: {RUN_VALUE}")
    );
}

#[test]
fn a_file_oxabl_cannot_read_makes_a_program_class_and_member_count_incomplete() {
    const PROG: &str = "PROCEDURE hello:\nEND PROCEDURE.\n";
    // An include that resolves to no file can hold any of the calls.
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("prog.p", PROG),
        ("app/Calc.cls", CALC),
        ("broken.p", "{missing.i}\n"),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "prog.p", PROG);
    assert_eq!(
        titles[0],
        format!("0+ references · used by 0+ files · incomplete: {UNKNOWN_FILE}")
    );
    let titles = titles_of(&mut session, &ws, "app/Calc.cls", CALC);
    // The class, its property, `Add` and `Twice`.
    for (title, needle) in titles.iter().zip(["Calc:", "Total AS", "Add(", "Twice("]) {
        assert!(
            title.contains('+') && title.ends_with(&format!("incomplete: {UNKNOWN_FILE}")),
            "`{needle}`: `{title}`"
        );
    }
}

#[test]
fn a_file_that_cannot_be_analysed_makes_a_program_count_incomplete() {
    const PROG: &str = "PROCEDURE hello:\nEND PROCEDURE.\n";
    let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("prog.p", PROG)]);
    // Bytes that are no text: the file is read and cannot be analysed.
    std::fs::write(ws.path("binary.p"), [0xff, 0xfe, 0x00, 0xff]).unwrap();
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "prog.p", PROG);
    assert_eq!(
        titles[0],
        format!("0+ references · used by 0+ files · incomplete: {UNKNOWN_FILE}")
    );
}

#[test]
fn a_dynamic_call_makes_a_class_or_member_count_incomplete() {
    const OBJECT: &str = "DEFINE VARIABLE o AS CLASS app.Calc NO-UNDO.\n";
    let with_object = |call: &str| format!("{OBJECT}{call}");
    // The call, and the reason for the class and for the members.
    let cases: [(String, Option<&str>, Option<&str>); 7] = [
        (
            with_object("o = DYNAMIC-NEW \"app.Calc\" ().\n"),
            Some(DYNAMIC_NEW),
            None,
        ),
        (
            with_object("DYNAMIC-INVOKE(o, \"Add\", 1).\n"),
            Some(DYNAMIC_INVOKE_CLASS),
            Some(DYNAMIC_INVOKE_MEMBER),
        ),
        (
            with_object(
                "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = DYNAMIC-PROPERTY(o, \"Total\").\n",
            ),
            Some(DYNAMIC_PROPERTY),
            Some(DYNAMIC_PROPERTY),
        ),
        (
            "DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\n\
             o = Progress.Lang.Class:GetClass(\"app.Calc\"):New().\n"
                .to_string(),
            Some(REFLECTION),
            Some(REFLECTION),
        ),
        (
            with_object(
                "DEFINE VARIABLE m AS HANDLE NO-UNDO.\nm = o:GetClass():GetMethod(\"Add\").\n",
            ),
            Some(REFLECTION),
            Some(REFLECTION),
        ),
        (
            "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nCREATE CALL h.\n".to_string(),
            Some(DYNAMIC_INVOKE_CLASS),
            Some(DYNAMIC_INVOKE_MEMBER),
        ),
        // Nothing computed: every count is exact.
        (with_object("o = NEW app.Calc().\no:Add(1).\n"), None, None),
    ];
    for (call, class, member) in cases {
        let ws = Workspace::new(&[
            ("oxabl.toml", CONFIG),
            ("app/Calc.cls", CALC),
            ("dyn.p", &call),
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
            // A member can have two reasons: the first is the one shown.
            assert!(
                got == &want || got.starts_with(&format!("{want} (+")),
                "{call}: `{needle}` shows `{got}`, wanted `{want}`"
            );
        }
    }
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
    let text = held_calc();
    session.open(&calc, &text);
    let listed = lenses(&mut session, &calc);
    // The workspace pass exists once find references has its answer. A lens
    // would count every lens of the file, and `Add` would have a kept title.
    assert_eq!(find_references(&mut session, &calc, &text, "Twice(").0, 1);
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

#[test]
fn a_burst_of_changes_asks_for_the_lenses_once() {
    let ws = Workspace::standard();
    let util = ws.path("lib/util.p");
    let mut session = start(&ws);
    session.open(&util, UTIL);
    let _ = lenses(&mut session, &util);
    assert!(!refresh_comes(
        &session,
        std::time::Duration::from_millis(1500)
    ));
    for _ in 0..3 {
        session.send(json!({
            "jsonrpc": "2.0", "method": "textDocument/didSave",
            "params": {"textDocument": {"uri": file_uri(&util)}}
        }));
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    // The three saves are one burst: one request, and no second one after it.
    let mut refreshes = 0;
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(3500);
    while let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) {
        if let Some(message) = session.try_next(left)
            && message["method"] == "workspace/codeLens/refresh"
        {
            refreshes += 1;
        }
    }
    assert_eq!(refreshes, 1, "refresh requests for one burst of saves");
}

/// `Calc.cls` with markers that hold the start of each search for 1 s and
/// each answer for 1.5 s before its title is kept.
fn held_twice_calc() -> String {
    format!(
        "{CALC}/* OXABL-TEST-HOLD:lsp.reference-lens@1000 */\n\
         /* OXABL-TEST-HOLD:lsp.answered-reference-lens@1500 */\n"
    )
}

#[test]
fn a_title_that_a_clear_overtook_is_not_kept_though_the_pass_is_the_same() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Calc.cls", CALC),
        ("a.p", CALLER),
    ]);
    let calc = ws.path("app/Calc.cls");
    let mut session = start(&ws);
    session.open(&calc, &held_twice_calc());
    let listed = lenses(&mut session, &calc);
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "codeLens/resolve", "params": listed[ADD]
    }));
    // The event names a file that no search reads, so the workspace pass is the
    // same one after it: only the epoch of the cache can refuse the title.
    std::thread::sleep(std::time::Duration::from_millis(300));
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&ws.path("notes.txt")), "type": 1}]}
    }));
    let first = session.wait_for(|m| m["id"] == json!(id));
    assert_eq!(title(&first["result"]), exact(1));
    // A kept title answers at once. A search waits 1 s at its start.
    let started = std::time::Instant::now();
    assert_eq!(title(&resolve(&mut session, &listed[ADD])), exact(1));
    assert!(
        started.elapsed() >= std::time::Duration::from_millis(900),
        "the answer that the clear overtook was kept: {:?}",
        started.elapsed()
    );
}

#[test]
fn an_answer_that_a_later_request_can_change_is_not_kept() {
    // No include path: the other files are not searched, and a later request
    // can say otherwise with no edit between.
    let ws = Workspace::new(&[("app/Calc.cls", CALC), ("a.p", CALLER)]);
    let calc = ws.path("app/Calc.cls");
    let mut session = start(&ws);
    session.open(
        &calc,
        &format!("{CALC}/* OXABL-TEST-HOLD:lsp.reference-lens@1000 */\n"),
    );
    let listed = lenses(&mut session, &calc);
    for time in 0..2 {
        let started = std::time::Instant::now();
        let lens = resolve(&mut session, &listed[ADD]);
        assert!(
            title(&lens).contains("incomplete: other files are not searched"),
            "{}",
            title(&lens)
        );
        assert!(
            started.elapsed() >= std::time::Duration::from_millis(900),
            "resolve {time} came from the cache: {:?}",
            started.elapsed()
        );
    }
}

/// The file of a class, its text, the caller, and the members to check with
/// the index of their lens.
type OverrideCase<'a> = (&'a str, &'a str, &'a str, &'a [(&'a str, usize)]);

const SUPERTYPE: &str = "calls through a supertype are not counted";

/// A workspace with the classes of `files`, and a program `use.p` with `text`.
fn class_workspace(files: &[(&str, &str)], user: &str) -> Workspace {
    let mut all = vec![("oxabl.toml", CONFIG), ("use.p", user)];
    all.extend_from_slice(files);
    Workspace::new(&all)
}

#[test]
fn a_member_that_overrides_or_implements_another_never_has_an_exact_count() {
    const BASE: &str = "CLASS app.Base:\n  METHOD PUBLIC VOID M():\n  END METHOD.\n\
        END CLASS.\n";
    const OVER: &str = "CLASS app.Derived INHERITS app.Base:\n\
        \x20 METHOD PUBLIC OVERRIDE VOID M():\n  END METHOD.\nEND CLASS.\n";
    const IFOO: &str = "INTERFACE app.IFoo:\n  METHOD PUBLIC VOID Bar().\nEND INTERFACE.\n";
    const IMPL: &str = "CLASS app.C IMPLEMENTS app.IFoo:\n  METHOD PUBLIC VOID Bar():\n\
        \x20 END METHOD.\nEND CLASS.\n";
    const TEMPLATE: &str = "CLASS app.TBase:\n  METHOD PUBLIC VOID Run():\n\
        \x20   THIS-OBJECT:Hook().\n  END METHOD.\n  METHOD PROTECTED VOID Hook():\n\
        \x20 END METHOD.\nEND CLASS.\n";
    const HOOK: &str = "CLASS app.THook INHERITS app.TBase:\n\
        \x20 METHOD PROTECTED OVERRIDE VOID Hook():\n  END METHOD.\nEND CLASS.\n";
    const OBJECT: &str = "CLASS app.D:\n  METHOD PUBLIC OVERRIDE CHARACTER ToString():\n\
        \x20   RETURN \"d\".\n  END METHOD.\n  METHOD PUBLIC OVERRIDE LOGICAL \
        Equals(INPUT o AS Progress.Lang.Object):\n    RETURN TRUE.\n  END METHOD.\n\
        END CLASS.\n";
    // The file, its text, the caller, and the lens of the overriding member:
    // the class has the lens 0, then its methods.
    let cases: [OverrideCase; 4] = [
        (
            "app/Derived.cls",
            OVER,
            "DEFINE INPUT PARAMETER b AS app.Base NO-UNDO.\nb:M().\n",
            &[("M", 1)],
        ),
        (
            "app/C.cls",
            IMPL,
            "DEFINE INPUT PARAMETER f AS app.IFoo NO-UNDO.\nf:Bar().\n",
            &[("Bar", 1)],
        ),
        (
            "app/THook.cls",
            HOOK,
            "DEFINE VARIABLE d AS app.THook NO-UNDO.\nd = NEW app.THook().\nd:Run().\n",
            &[("Hook", 1)],
        ),
        (
            "app/D.cls",
            OBJECT,
            "DEFINE VARIABLE d AS app.D NO-UNDO.\nd = NEW app.D().\nMESSAGE STRING(d).\n",
            &[("ToString", 1), ("Equals", 2)],
        ),
    ];
    for (file, text, user, members) in cases {
        let ws = class_workspace(
            &[
                ("app/Base.cls", BASE),
                ("app/IFoo.cls", IFOO),
                ("app/TBase.cls", TEMPLATE),
                (file, text),
            ],
            user,
        );
        let mut session = start(&ws);
        let titles = titles_of(&mut session, &ws, file, text);
        for (name, at) in members {
            assert!(
                titles[*at].starts_with("0+ references")
                    && titles[*at].contains(&format!("incomplete: {SUPERTYPE}")),
                "{file} `{name}` shows `{}` in {titles:?}",
                titles[*at]
            );
        }
    }
}

#[test]
fn a_member_that_overrides_nothing_stays_exact() {
    const PLAIN: &str = "CLASS app.Plain INHERITS app.Base:\n  METHOD PUBLIC VOID Own():\n\
        \x20 END METHOD.\n  METHOD PRIVATE VOID Hidden():\n  END METHOD.\nEND CLASS.\n";
    const BASE: &str = "CLASS app.Base:\n  METHOD PUBLIC VOID M():\n  END METHOD.\n\
        END CLASS.\n";
    let ws = class_workspace(
        &[("app/Base.cls", BASE), ("app/Plain.cls", PLAIN)],
        "MESSAGE 1.\n",
    );
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "app/Plain.cls", PLAIN);
    assert_eq!(titles[1], exact(0), "{titles:?}");
    assert_eq!(titles[2], exact(0), "{titles:?}");
}

#[test]
fn a_property_of_a_class_with_a_parent_is_marked() {
    const BASE: &str = "CLASS app.Base:\nEND CLASS.\n";
    const CHILD: &str = "CLASS app.Child INHERITS app.Base:\n\
        \x20 DEFINE PUBLIC PROPERTY Total AS INTEGER NO-UNDO GET. SET.\nEND CLASS.\n";
    let ws = class_workspace(
        &[("app/Base.cls", BASE), ("app/Child.cls", CHILD)],
        "MESSAGE 1.\n",
    );
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "app/Child.cls", CHILD);
    assert_eq!(titles[1], short(0, SUPERTYPE), "{titles:?}");
    // The class lens itself is not a member.
    assert_eq!(titles[0], exact(0), "{titles:?}");
}

#[test]
fn text_the_preprocessor_cannot_decide_makes_a_program_count_incomplete() {
    const PROG: &str = "PROCEDURE hello:\nEND PROCEDURE.\n";
    // No `proversion` is set, so the condition has no value and a branch that
    // holds the `RUN VALUE` is dropped.
    const MAIN: &str = "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
        &IF PROVERSION >= \"12\" &THEN\nRUN VALUE(c).\n&ELSE\nMESSAGE 1.\n&ENDIF\n";
    let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("prog.p", PROG), ("main.p", MAIN)]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "prog.p", PROG);
    assert_eq!(
        titles[0],
        format!("0+ references · used by 0+ files · incomplete: {UNKNOWN_FILE}")
    );
}

#[test]
fn a_persistent_trigger_with_a_computed_program_makes_a_program_count_incomplete() {
    const PROG: &str = "PROCEDURE hello:\nEND PROCEDURE.\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("prog.p", PROG),
        (
            "main.p",
            "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nDEFINE BUTTON b.\n\
             ON CHOOSE OF b PERSISTENT RUN VALUE(c).\n",
        ),
    ]);
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "prog.p", PROG);
    assert_eq!(
        titles[0],
        format!("0+ references · used by 0+ files · incomplete: {RUN_VALUE}")
    );
}

const SUBSCRIBE: &str = "a class event Subscribe can call it";

const PUB: &str = "CLASS app.Pub:\n  DEFINE PUBLIC EVENT Changed SIGNATURE VOID ().\n\
    \x20 METHOD PUBLIC VOID Fire():\n    Changed:Publish().\n  END METHOD.\nEND CLASS.\n";

/// A class that subscribes to `app.Pub` with `subscribe`, and has the methods
/// `OnChanged`, `OnSym` and `Quiet`.
fn subscriber(subscribe: &str) -> String {
    format!(
        "CLASS app.Sub:\n  CONSTRUCTOR PUBLIC Sub(INPUT p AS app.Pub):\n    {subscribe}\n\
         \x20 END CONSTRUCTOR.\n  METHOD PUBLIC VOID OnChanged():\n  END METHOD.\n\
         \x20 METHOD PRIVATE VOID OnSym():\n  END METHOD.\n  METHOD PUBLIC VOID Quiet():\n\
         \x20 END METHOD.\nEND CLASS.\n"
    )
}

#[test]
fn a_method_that_a_class_event_subscribes_by_name_never_has_an_exact_count() {
    // The lenses of `Sub`: the class, then `OnChanged`, `OnSym` and `Quiet`.
    let titles_for = |subscribe: &str| {
        let sub = subscriber(subscribe);
        let ws = Workspace::new(&[
            ("oxabl.toml", CONFIG),
            ("app/Pub.cls", PUB),
            ("app/Sub.cls", &sub),
        ]);
        let mut session = start(&ws);
        titles_of(&mut session, &ws, "app/Sub.cls", &sub)
    };
    // The string form names `OnChanged`: only that method is marked.
    let titles = titles_for("p:Changed:Subscribe(THIS-OBJECT, \"OnChanged\").");
    assert_eq!(titles[1], short(0, SUBSCRIBE), "{titles:?}");
    assert_eq!(titles[3], exact(0), "{titles:?}");
    // The name is computed: any method can run.
    let titles = titles_for(
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n    p:Changed:Subscribe(THIS-OBJECT, c).",
    );
    for method in &titles[1..] {
        assert_eq!(method, &short(0, SUBSCRIBE), "{titles:?}");
    }
    // The symbolic form: the method is marked by its unresolved name.
    let titles = titles_for("p:Changed:Subscribe(OnSym).");
    assert!(
        titles[2].starts_with("0+ references · incomplete:"),
        "{titles:?}"
    );
    // No Subscribe: all exact.
    let titles = titles_for("MESSAGE 1.");
    for method in &titles[1..] {
        assert_eq!(method, &exact(0), "{titles:?}");
    }
}

#[test]
fn a_parameter_list_from_an_include_is_compared_after_preprocessing() {
    // `MODE` is `OUTPUT` in `app.Other`: its `Init` is an overload.
    const MODE_I: &str = "{&MODE} pcA AS CHARACTER\n";
    const BASE: &str = "&SCOPED-DEFINE MODE INPUT\nCLASS app.Base:\n\
        \x20 METHOD PUBLIC VOID Init({mode.i}):\n  END METHOD.\nEND CLASS.\n";
    const SAME: &str = "&SCOPED-DEFINE MODE INPUT\nCLASS app.Same INHERITS app.Base:\n\
        \x20 METHOD PUBLIC OVERRIDE VOID Init({mode.i}):\n  END METHOD.\nEND CLASS.\n";
    const OTHER: &str = "&SCOPED-DEFINE MODE OUTPUT\nCLASS app.Other INHERITS app.Base:\n\
        \x20 METHOD PUBLIC VOID Init({mode.i}):\n  END METHOD.\nEND CLASS.\n";
    let ws = class_workspace(
        &[
            ("mode.i", MODE_I),
            ("app/Base.cls", BASE),
            ("app/Same.cls", SAME),
            ("app/Other.cls", OTHER),
        ],
        "MESSAGE 1.\n",
    );
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "app/Same.cls", SAME);
    assert_eq!(titles[1], short(0, SUPERTYPE), "{titles:?}");
    let titles = titles_of(&mut session, &ws, "app/Other.cls", OTHER);
    assert_eq!(titles[1], exact(0), "{titles:?}");
}

#[test]
fn a_parameter_list_from_an_include_that_does_not_resolve_may_override() {
    const BASE: &str = "CLASS app.Base:\n  METHOD PUBLIC VOID Init({nope.i}):\n  END METHOD.\n\
        END CLASS.\n";
    const CHILD: &str = "CLASS app.Child INHERITS app.Base:\n\
        \x20 METHOD PUBLIC VOID Init(INPUT pcA AS CHARACTER):\n  END METHOD.\nEND CLASS.\n";
    let ws = class_workspace(
        &[("app/Base.cls", BASE), ("app/Child.cls", CHILD)],
        "MESSAGE 1.\n",
    );
    let mut session = start(&ws);
    let titles = titles_of(&mut session, &ws, "app/Child.cls", CHILD);
    // The unresolved include is also a file oxabl cannot read, which comes
    // first; the other reason is the supertype, which the class has not.
    let unread = short(0, UNKNOWN_FILE);
    assert_eq!(titles[0], unread, "{titles:?}");
    assert_eq!(titles[1], format!("{unread} (+1 more)"), "{titles:?}");
}

const SHAPES: &str = "CLASS app.Shapes:\n\
    \x20   DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO GET. SET.\n\
    \x20   METHOD PUBLIC INTEGER Add(INPUT piValue AS INTEGER):\n\
    \x20       RETURN Twice(piValue).\n\
    \x20   END METHOD.\n\
    \x20   METHOD PRIVATE INTEGER Twice(INPUT piValue AS INTEGER):\n\
    \x20       RETURN piValue * 2.\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Reset():\n\
    \x20       Count = 0.\n\
    \x20   END METHOD.\n\
    \x20   METHOD PUBLIC VOID Unused():\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const CIRCLE: &str = "CLASS app.Circle INHERITS app.Shapes:\n\
    \x20   METHOD PUBLIC OVERRIDE VOID Reset():\n\
    \x20       SUPER:Reset().\n\
    \x20       Add(1).\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// A class with many lenses, a subclass, two callers and a caller with a
/// parse error.
fn shapes_workspace() -> Workspace {
    Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Shapes.cls", SHAPES),
        ("app/Circle.cls", CIRCLE),
        (
            "a.p",
            "DEFINE VARIABLE o AS CLASS app.Shapes NO-UNDO.\no = NEW app.Shapes().\n\
             o:Add(1).\no:Count = 2.\no:Reset().\n",
        ),
        (
            "b.p",
            "DEFINE VARIABLE o AS CLASS app.Shapes NO-UNDO.\no = NEW app.Shapes().\n\
             o:Add(2).\no:Add(3).\n",
        ),
        (
            "c.p",
            "DEFINE VARIABLE o AS CLASS app.Shapes NO-UNDO.\no = NEW app.Shapes().\n\
             o:Reset().\nIF THEN ELSE.\n",
        ),
        ("d.p", "MESSAGE \"names no class\".\n"),
    ])
}

/// Send `codeLens/resolve` for each of `listed` before reading any answer.
/// The answers in order, and the lines of the test log of the lens searches.
fn resolve_at_once(session: &mut Session, listed: &[Value]) -> (Vec<Value>, Vec<String>) {
    let ids: Vec<u64> = listed
        .iter()
        .map(|lens| {
            let id = session.next_request_id();
            session.send(json!({
                "jsonrpc": "2.0", "id": id, "method": "codeLens/resolve", "params": lens
            }));
            id
        })
        .collect();
    let mut answers = vec![Value::Null; ids.len()];
    let mut logged = Vec::new();
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
    while answers.iter().any(Value::is_null) {
        let left = deadline
            .checked_duration_since(std::time::Instant::now())
            .expect("every resolve has an answer in 60 s");
        let Some(message) = session.try_next(left) else {
            continue;
        };
        if let Some(line) = message["params"]["message"]
            .as_str()
            .filter(|line| line.starts_with("oxabl-test:"))
        {
            logged.push(line.to_string());
        } else if let Some(at) = ids.iter().position(|id| message["id"] == json!(id)) {
            assert!(message.get("error").is_none(), "resolve gave {message}");
            answers[at] = message["result"].clone();
        }
    }
    (answers, logged)
}

#[test]
fn lenses_resolved_at_once_have_the_counts_and_lists_of_lenses_resolved_one_at_a_time() {
    let ws = shapes_workspace();
    let shapes = ws.path("app/Shapes.cls");
    let mut session = start(&ws);
    session.open(&shapes, SHAPES);
    let listed = lenses(&mut session, &shapes);
    assert_eq!(listed.len(), 6, "a lens for the class and each member");
    let (at_once, _) = resolve_at_once(&mut session, &listed);
    let titles: Vec<String> = at_once.iter().map(title).collect();
    let counts: Vec<usize> = titles.iter().map(|title| count_of(title)).collect();
    assert_eq!(counts, [7, 2, 3, 1, 3, 0], "{titles:#?}");
    // `c.p` has a parse error and names the class: no count is exact.
    assert!(
        titles
            .iter()
            .all(|title| title.contains("+ references · incomplete: c.p has parse errors")),
        "{titles:#?}"
    );
    // Each lens alone, in a session of its own, so that no other lens of the
    // file was asked for.
    for (n, lens) in at_once.iter().enumerate() {
        let mut alone = start(&ws);
        alone.open(&shapes, SHAPES);
        let own = lenses(&mut alone, &shapes).remove(n);
        assert_eq!(
            resolve(&mut alone, &own)["command"],
            lens["command"],
            "lens {n} alone"
        );
    }
    // Each list is the answer of find references for its symbol.
    for (n, symbol) in [
        "Shapes:", "Count AS", "Add(", "Twice(", "Reset()", "Unused()",
    ]
    .into_iter()
    .enumerate()
    {
        assert_eq!(
            listed_places(&at_once[n]),
            find_places(&mut session, &shapes, SHAPES, symbol),
            "`{symbol}`"
        );
    }
}

#[test]
fn one_search_analyses_each_file_once_for_all_the_lenses_of_a_file() {
    let ws = shapes_workspace();
    let shapes = ws.path("app/Shapes.cls");
    let mut session = start(&ws);
    let text = format!("{SHAPES}/* OXABL-TEST-COUNT:lsp.reference-lens */\n");
    session.open(&shapes, &text);
    let listed = lenses(&mut session, &shapes);
    let (answers, logged) = resolve_at_once(&mut session, &listed);
    assert!(answers.iter().all(|lens| !title(lens).is_empty()));
    // One search for the six lenses, and each file that can name a member of
    // the class once in it.
    assert_eq!(logged.len(), 1, "the searches: {logged:#?}");
    let (lenses_searched, files) = logged[0]
        .strip_prefix("oxabl-test: ")
        .and_then(|rest| rest.split_once(" reference lenses searched "))
        .unwrap_or_else(|| panic!("an unknown log line: {}", logged[0]));
    assert_eq!(lenses_searched, "6");
    let mut files: Vec<&str> = files.split('|').collect();
    files.sort_unstable();
    let mut want: Vec<String> = ["a.p", "app/Circle.cls", "b.p", "c.p"]
        .iter()
        .map(|name| ws.path(name).display().to_string())
        .collect();
    want.sort();
    assert_eq!(files, want, "the files the search analysed");
}

/// The threads of the process `pid`.
#[cfg(target_os = "linux")]
fn threads_of(pid: u32) -> usize {
    std::fs::read_to_string(format!("/proc/{pid}/status"))
        .unwrap()
        .lines()
        .find_map(|line| line.strip_prefix("Threads:"))
        .and_then(|count| count.trim().parse().ok())
        .unwrap()
}

/// Three classes, each named by `callers` programs whose analysis in a
/// search waits 300 ms.
#[cfg(target_os = "linux")]
fn held_callers_workspace(callers: usize) -> Workspace {
    let mut files: Vec<(String, String)> = vec![("oxabl.toml".into(), CONFIG.into())];
    for class in ["A", "B", "C"] {
        files.push((
            format!("app/{class}.cls"),
            format!(
                "CLASS app.{class}:\n\
                 \x20   DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO GET. SET.\n\
                 \x20   METHOD PUBLIC VOID Run():\n    END METHOD.\n\
                 \x20   METHOD PUBLIC VOID Stop():\n    END METHOD.\n\
                 END CLASS.\n"
            ),
        ));
        for n in 0..callers {
            files.push((
                format!("{class}{n}.p"),
                format!(
                    "/* OXABL-TEST-HOLD:daemon.reference-unit@300 */\n\
                     DEFINE VARIABLE o AS CLASS app.{class} NO-UNDO.\n\
                     o = NEW app.{class}().\no:Run().\n"
                ),
            ));
        }
    }
    let borrowed: Vec<(&str, &str)> = files
        .iter()
        .map(|(name, text)| (name.as_str(), text.as_str()))
        .collect();
    Workspace::new(&borrowed)
}

#[cfg(target_os = "linux")]
#[test]
fn waiting_resolves_and_searches_hold_no_threads_of_their_own() {
    let ws = held_callers_workspace(12);
    let mut session = start(&ws);
    let pid = session.daemon_pid().expect("the daemon registered");
    let mut listed = Vec::new();
    let mut texts = Vec::new();
    for class in ["A", "B", "C"] {
        let path = ws.path(&format!("app/{class}.cls"));
        let text = std::fs::read_to_string(&path).unwrap();
        session.open(&path, &text);
        listed.extend(lenses(&mut session, &path));
        texts.push((path, text));
    }
    assert_eq!(listed.len(), 12);
    let idle = threads_of(pid);
    // The workspace pass and the threads that analyse other files exist.
    let (uses, _) = find_references(&mut session, &texts[0].0, &texts[0].1, "Stop()");
    assert_eq!(uses, 0);
    let before = threads_of(pid);

    // Twelve resolves of three files and three find references requests at
    // once, while each analysis of a caller waits.
    let mut ids = Vec::new();
    for lens in &listed {
        let id = session.next_request_id();
        session.send(json!({
            "jsonrpc": "2.0", "id": id, "method": "codeLens/resolve", "params": lens
        }));
        ids.push(id);
    }
    for (path, text) in &texts {
        let id = session.next_request_id();
        let (line, character) = utf16_at(text, nth(text, "Run()", 0) + 1);
        session.send(json!({
            "jsonrpc": "2.0", "id": id, "method": "textDocument/references",
            "params": {
                "textDocument": {"uri": file_uri(path)},
                "position": {"line": line, "character": character},
                "context": {"includeDeclaration": false}
            }
        }));
        ids.push(id);
    }
    let mut most = before;
    let mut left = ids.len();
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(120);
    while left > 0 {
        most = most.max(threads_of(pid));
        assert!(
            std::time::Instant::now() < deadline,
            "{left} answers missing"
        );
        while let Some(message) = session.try_next(std::time::Duration::from_millis(20)) {
            if ids.iter().any(|id| message["id"] == json!(id)) && message.get("method").is_none() {
                assert!(message.get("error").is_none(), "{message}");
                left -= 1;
            }
        }
    }
    // A runner for each of the three batches, a thread for each find
    // references request, and a thread for each of these six that waits for
    // the workspace pass. No thread for a resolve that waits, and no new
    // thread to analyse a file: the searches share the ones that exist.
    let allowed = 3 + 3 + 6;
    eprintln!("the daemon went from {before} to {most} threads");
    assert!(
        most - before <= allowed,
        "the daemon went from {before} to {most} threads (allowed {allowed} more)"
    );
    // The threads of the searches end soon after them, and the memory that
    // they freed goes back with them.
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(15);
    while threads_of(pid) > idle {
        assert!(
            std::time::Instant::now() < deadline,
            "{} threads 15 s after the searches, {idle} before them",
            threads_of(pid)
        );
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}

/// A class `app.<name>` with a lens for the class, `Size`, `Run` and `Stop`,
/// and `callers` programs that name it and call `Run`, with `marker` in each.
fn held_class(files: &mut Vec<(String, String)>, name: &str, callers: usize, marker: &str) {
    files.push((
        format!("app/{name}.cls"),
        format!(
            "CLASS app.{name}:\n\
             \x20   DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO GET. SET.\n\
             \x20   METHOD PUBLIC VOID Run():\n    END METHOD.\n\
             \x20   METHOD PUBLIC VOID Stop():\n    END METHOD.\n\
             END CLASS.\n"
        ),
    ));
    for n in 0..callers {
        files.push((
            format!("{name}{n}.p"),
            format!(
                "{marker}\nDEFINE VARIABLE o AS CLASS app.{name} NO-UNDO.\n\
                 o = NEW app.{name}().\no:Run().\n"
            ),
        ));
    }
}

fn workspace_of(files: &[(String, String)]) -> Workspace {
    let mut borrowed: Vec<(&str, &str)> = vec![("oxabl.toml", CONFIG)];
    borrowed.extend(
        files
            .iter()
            .map(|(name, text)| (name.as_str(), text.as_str())),
    );
    Workspace::new(&borrowed)
}

/// Open `app/<name>.cls`, with `extra` after its text, and give its lenses.
fn open_class(session: &mut Session, ws: &Workspace, name: &str, extra: &str) -> Vec<Value> {
    let path = ws.path(&format!("app/{name}.cls"));
    let text = std::fs::read_to_string(&path).unwrap() + extra;
    session.open(&path, &text);
    lenses(session, &path)
}

fn send_resolve(session: &mut Session, lens: &Value) -> u64 {
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "codeLens/resolve", "params": lens
    }));
    id
}

/// The messages until `done` says that all are in, or `limit` passes.
fn messages_until(
    session: &Session,
    limit: std::time::Duration,
    mut done: impl FnMut(&[Value]) -> bool,
) -> Vec<Value> {
    let deadline = std::time::Instant::now() + limit;
    let mut seen = Vec::new();
    while !done(&seen) {
        let left = deadline
            .checked_duration_since(std::time::Instant::now())
            .unwrap_or_else(|| panic!("no end in {limit:?}: {seen:#?}"));
        if let Some(message) = session.try_next(left) {
            seen.push(message);
        }
    }
    seen
}

fn answer_to(seen: &[Value], id: u64) -> Option<&Value> {
    seen.iter()
        .find(|message| message["id"] == json!(id) && message.get("method").is_none())
}

/// The files of the test log line of a lens search.
fn searched_files(seen: &[Value]) -> Option<Vec<String>> {
    seen.iter().find_map(|message| {
        let line = message["params"]["message"].as_str()?;
        let (_, files) = line
            .strip_prefix("oxabl-test: ")?
            .split_once(" reference lenses searched ")?;
        Some(
            files
                .split('|')
                .filter(|file| !file.is_empty())
                .map(str::to_string)
                .collect(),
        )
    })
}

#[test]
fn a_batch_whose_resolves_are_all_cancelled_stops() {
    let callers = 24;
    let mut files = Vec::new();
    held_class(
        &mut files,
        "A",
        callers,
        "/* OXABL-TEST-HOLD:daemon.reference-unit@1000 */",
    );
    held_class(&mut files, "B", 1, "");
    let ws = workspace_of(&files);
    let mut session = start(&ws);
    let listed = open_class(
        &mut session,
        &ws,
        "A",
        "/* OXABL-TEST-COUNT:lsp.reference-lens */\n",
    );
    assert_eq!(listed.len(), 4);
    // The workspace pass is current, so the batch is in its search of the
    // callers when the cancels come.
    let b = ws.path("app/B.cls");
    let b_text = std::fs::read_to_string(&b).unwrap();
    session.open(&b, &b_text);
    assert_eq!(find_references(&mut session, &b, &b_text, "Run()").0, 1);

    let ids: Vec<u64> = listed
        .iter()
        .map(|lens| send_resolve(&mut session, lens))
        .collect();
    std::thread::sleep(std::time::Duration::from_millis(300));
    for id in &ids {
        session.send(json!({"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}}));
    }
    let seen = messages_until(&session, std::time::Duration::from_secs(60), |seen| {
        ids.iter().all(|&id| answer_to(seen, id).is_some()) && searched_files(seen).is_some()
    });
    for &id in &ids {
        let answer = answer_to(&seen, id).unwrap();
        assert_eq!(
            answer["error"]["code"],
            json!(-32800),
            "a cancelled resolve gave {answer}"
        );
    }
    // The search stopped: it did not analyse every caller.
    let analysed = searched_files(&seen).unwrap();
    assert!(
        analysed.len() < callers,
        "the search analysed {} of {callers} callers after every resolve was cancelled",
        analysed.len()
    );
}

#[test]
fn a_kept_title_and_a_short_batch_do_not_wait_for_a_long_batch() {
    let mut files = Vec::new();
    held_class(
        &mut files,
        "A",
        12,
        "/* OXABL-TEST-HOLD:daemon.reference-unit@3000 */",
    );
    held_class(
        &mut files,
        "B",
        12,
        "/* OXABL-TEST-HOLD:daemon.reference-unit@3000 */",
    );
    held_class(&mut files, "C", 1, "");
    // No other file names `E`: its batch needs no thread of the pool, only
    // a runner.
    held_class(&mut files, "E", 0, "");
    let ws = workspace_of(&files);
    let mut session = start(&ws);
    let count = "/* OXABL-TEST-COUNT:lsp.reference-lens */\n";
    let a = open_class(&mut session, &ws, "A", "");
    let b = open_class(&mut session, &ws, "B", "");
    let c = open_class(&mut session, &ws, "C", count);
    let e = open_class(&mut session, &ws, "E", "");
    // A title of `C` that read the workspace pass is kept.
    let (first, logged) = resolve_at_once(&mut session, &c[2..3]);
    assert_eq!(logged.len(), 1, "{logged:?}");
    assert_eq!(title(&first[0]), exact(1));

    // Two long batches, then a kept title and a new file.
    let long: Vec<u64> = [&a[0], &b[0]]
        .into_iter()
        .map(|lens| send_resolve(&mut session, lens))
        .collect();
    std::thread::sleep(std::time::Duration::from_millis(300));
    let kept = send_resolve(&mut session, &c[2]);
    let short = send_resolve(&mut session, &e[2]);
    let seen = messages_until(&session, std::time::Duration::from_secs(60), |seen| {
        answer_to(seen, kept).is_some() && answer_to(seen, short).is_some()
    });
    for id in &long {
        assert!(
            answer_to(&seen, *id).is_none(),
            "a long batch ended before the kept title and the short batch: {seen:#?}"
        );
    }
    assert_eq!(title(&answer_to(&seen, kept).unwrap()["result"]), exact(1));
    assert_eq!(title(&answer_to(&seen, short).unwrap()["result"]), exact(0));
    // The kept title made no search.
    assert!(
        searched_files(&seen).is_none(),
        "the kept title made a new search: {seen:#?}"
    );
    messages_until(&session, std::time::Duration::from_secs(120), |seen| {
        long.iter().all(|&id| answer_to(seen, id).is_some())
    });
}

#[test]
fn a_panic_for_one_lens_in_a_file_costs_only_that_lens() {
    let mut files = Vec::new();
    held_class(&mut files, "A", 0, "");
    files.push((
        "user.p".to_string(),
        "/* OXABL-TEST-PANIC:daemon.reference-part:Run */\n\
         DEFINE VARIABLE o AS CLASS app.A NO-UNDO.\n\
         o = NEW app.A().\no:Run().\no:Stop().\no:Size = 1.\n"
            .to_string(),
    ));
    let ws = workspace_of(&files);
    let mut session = start(&ws);
    let listed = open_class(&mut session, &ws, "A", "");
    let (answers, _) = resolve_at_once(&mut session, &listed);
    let titles: Vec<String> = answers.iter().map(title).collect();
    assert_eq!(titles[0], exact(2), "the class: {titles:#?}");
    assert_eq!(titles[1], exact(1), "`Size`: {titles:#?}");
    assert!(
        titles[2].starts_with("0+ references · incomplete: 1 file(s) that can name `Run`"),
        "`Run`: {titles:#?}"
    );
    assert_eq!(titles[3], exact(1), "`Stop`: {titles:#?}");
}
