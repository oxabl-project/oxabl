//! The `&IF` branches the preprocessor does not compile, as `oxabl lsp` pushes
//! them in `textDocument/inactiveRegions`.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Session, file_uri};

/// A project that states none of the compile environment.
const UNSET: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

/// A project that states `opsys`.
const BASE: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n\
[workspace.preprocessor]\nopsys = \"UNIX\"\n";

fn asks() -> Value {
    json!({"textDocument": {"inactiveRegionsCapabilities": {"inactiveRegions": true}}})
}

fn project(config: &str, files: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), config).unwrap();
    for (name, text) in files {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

/// A region as `(start line, start character, end line, end character)`.
type Region = (u64, u64, u64, u64);

fn regions_of(message: &Value) -> Vec<Region> {
    message["params"]["regions"]
        .as_array()
        .unwrap()
        .iter()
        .map(|r| {
            let at = |p: &str, f: &str| r[p][f].as_u64().unwrap();
            (
                at("start", "line"),
                at("start", "character"),
                at("end", "line"),
                at("end", "character"),
            )
        })
        .collect()
}

/// The next inactive regions of `path` that satisfy `wanted`, with the version
/// they were sent for.
#[track_caller]
fn regions_where(
    session: &Session,
    path: &Path,
    wanted: impl Fn(&[Region]) -> bool,
) -> (Vec<Region>, Value) {
    let uri = json!(file_uri(path));
    let message = session.wait_for(|m| {
        m["method"] == json!("textDocument/inactiveRegions")
            && m["params"]["textDocument"]["uri"] == uri
            && wanted(&regions_of(m))
    });
    (
        regions_of(&message),
        message["params"]["textDocument"]["version"].clone(),
    )
}

/// The next inactive regions of `path`, whatever they are.
#[track_caller]
fn next_regions(session: &Session, path: &Path) -> Vec<Region> {
    regions_where(session, path, |_| true).0
}

const NESTED: &str = "&SCOPED-DEFINE mode fast\n\
&IF \"{&OPSYS}\" = \"WIN32\" &THEN\n\
MESSAGE \"windows\".\n\
&IF \"{&mode}\" = \"slow\" &THEN\n\
MESSAGE \"slow\".\n\
&ELSE\n\
MESSAGE \"fast\".\n\
&ENDIF\n\
&ELSE\n\
MESSAGE \"other\".\n\
&IF \"{&mode}\" = \"fast\" &THEN\n\
MESSAGE \"inner\".\n\
&ENDIF\n\
&ENDIF\n";

#[test]
fn nested_branches_follow_the_configured_compile_environment() {
    let windows = format!("{UNSET}[workspace.preprocessor]\nopsys = \"WIN32\"\n");
    let dir = project(&windows, &[("nested.p", NESTED)]);
    let path = dir.path().join("nested.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, NESTED);
    // The `slow` branch inside the taken one, and the whole `&ELSE`, with
    // the chain nested in it. The directive lines stay.
    assert_eq!(
        next_regions(&session, &path),
        vec![(4, 0, 5, 0), (9, 0, 13, 0)]
    );
    drop(session);

    // A Unix compile takes the other branch.
    let dir = project(BASE, &[("nested.p", NESTED)]);
    let path = dir.path().join("nested.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, NESTED);
    assert_eq!(next_regions(&session, &path), vec![(2, 0, 8, 0)]);
}

#[test]
fn an_edit_sends_the_regions_of_the_new_version() {
    let dir = project(BASE, &[("nested.p", NESTED)]);
    let path = dir.path().join("nested.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, NESTED);
    let (regions, version) = regions_where(&session, &path, |_| true);
    assert_eq!(regions, vec![(2, 0, 8, 0)]);
    assert_eq!(version, json!(1));

    let edited = NESTED.replace("mode fast", "mode slow");
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&path), "version": 2},
            "contentChanges": [{"text": edited}]
        }
    }));
    let (regions, version) = regions_where(&session, &path, |r| r != [(2, 0, 8, 0)]);
    // With `mode` slow, the branch nested in the taken `&ELSE` is not taken.
    assert_eq!(regions, vec![(2, 0, 8, 0), (11, 0, 12, 0)]);
    assert_eq!(version, json!(2));
}

const GUESSES: &str = "&IF PROVERSION >= \"12\" &THEN\n\
&GLOBAL-DEFINE new-ui yes\n\
&IF \"{&OPSYS}\" = \"VMS\" &THEN\n\
MESSAGE \"inner\".\n\
&ENDIF\n\
&ELSE\n\
MESSAGE \"old\".\n\
&ENDIF\n\
&IF DEFINED(new-ui) &THEN\n\
MESSAGE \"maybe\".\n\
&ENDIF\n\
&IF SEARCH(\"x.p\") <> ? &THEN\n\
MESSAGE \"searched\".\n\
&ENDIF\n\
&IF \"{&OPSYS}\" = \"VMS\" &THEN\n\
MESSAGE \"never\".\n\
&ENDIF\n";

#[test]
fn a_condition_oxabl_cannot_decide_leaves_its_branches_undimmed() {
    // No `proversion`: the first chain is a guess, and so is everything in it,
    // the define it may or may not make, and the `SEARCH`. The last chain reads
    // only facts.
    let dir = project(BASE, &[("guesses.p", GUESSES)]);
    let path = dir.path().join("guesses.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, GUESSES);
    assert_eq!(next_regions(&session, &path), vec![(15, 0, 16, 0)]);
    drop(session);

    // With the version stated, the first chain and what depends on it are
    // decided. The `SEARCH` is still a guess.
    let stated = format!("{BASE}proversion = \"12.8\"\n");
    let dir = project(&stated, &[("guesses.p", GUESSES)]);
    let path = dir.path().join("guesses.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, GUESSES);
    assert_eq!(
        next_regions(&session, &path),
        vec![(3, 0, 4, 0), (6, 0, 7, 0), (15, 0, 16, 0)]
    );
}

#[test]
fn a_name_an_unread_include_may_define_is_not_known() {
    let text = "{missing.i}\n\
&IF DEFINED(from-missing) &THEN\n\
MESSAGE \"maybe\".\n\
&ENDIF\n\
&IF OPSYS = \"VMS\" &THEN\n\
MESSAGE \"never\".\n\
&ENDIF\n";
    let dir = project(BASE, &[("reads.p", text)]);
    let path = dir.path().join("reads.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    // The `OPSYS` function reads the environment, which no include changes.
    assert_eq!(next_regions(&session, &path), vec![(5, 0, 6, 0)]);
}

#[test]
fn an_undefine_or_the_end_of_an_include_shows_what_was_known_before() {
    let text = "&IF PROVERSION >= \"12\" &THEN\n\
&GLOBAL-DEFINE colour blue\n\
&ENDIF\n\
&SCOPED-DEFINE colour red\n\
&IF \"{&colour}\" = \"red\" &THEN\n\
MESSAGE \"red\".\n\
&ELSE\n\
MESSAGE \"never\".\n\
&ENDIF\n\
&UNDEFINE colour\n\
&IF \"{&colour}\" = \"blue\" &THEN\n\
MESSAGE \"maybe\".\n\
&ENDIF\n\
{setter.i}\n\
&IF \"{&shade}\" = \"light\" &THEN\n\
MESSAGE \"never\".\n\
&ENDIF\n";
    let setter = "&IF PROVERSION >= \"12\" &THEN\n&SCOPED-DEFINE shade light\n&ENDIF\n";
    let dir = project(BASE, &[("scope.p", text), ("setter.i", setter)]);
    let path = dir.path().join("scope.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    // The undefine shows the global define again, which may or may not have
    // happened. The scoped name of the include ends with it, guess and all.
    assert_eq!(
        next_regions(&session, &path),
        vec![(7, 0, 8, 0), (15, 0, 16, 0)]
    );
}

const SHARED: &str = "&IF \"{&flavour}\" = \"red\" &THEN\n\
MESSAGE \"red\".\n\
&ELSE\n\
MESSAGE \"other\".\n\
&ENDIF\n\
&IF OPSYS = \"VMS\" &THEN\n\
MESSAGE \"never\".\n\
&ENDIF\n";

fn select(session: &mut Session, include: &Path, root: Option<&Path>) {
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(include)}, "root": root}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
}

#[test]
fn an_include_follows_the_selected_includer_context() {
    let dir = project(
        BASE,
        &[
            ("shared.i", SHARED),
            ("red.p", "&SCOPED-DEFINE flavour red\n{shared.i}\n"),
            ("blue.p", "&SCOPED-DEFINE flavour blue\n{shared.i}\n"),
            (
                "both.p",
                "&SCOPED-DEFINE flavour red\n{shared.i}\n\
                 &UNDEFINE flavour\n&SCOPED-DEFINE flavour blue\n{shared.i}\n",
            ),
        ],
    );
    let shared = dir.path().join("shared.i");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&shared, SHARED);

    // Alone, the include does not know `flavour`: its includer sets it. The
    // `OPSYS` function is the environment's, whoever includes the file.
    let vms = (6, 0, 7, 0);
    assert_eq!(next_regions(&session, &shared), vec![vms]);

    select(&mut session, &shared, Some(&dir.path().join("red.p")));
    regions_where(&session, &shared, |r| r == [(3, 0, 4, 0), vms]);

    select(&mut session, &shared, Some(&dir.path().join("blue.p")));
    regions_where(&session, &shared, |r| r == [(1, 0, 2, 0), vms]);

    // Expanded twice, once each way: only what neither expansion compiles.
    select(&mut session, &shared, Some(&dir.path().join("both.p")));
    regions_where(&session, &shared, |r| r == [vms]);

    select(&mut session, &shared, Some(&dir.path().join("red.p")));
    regions_where(&session, &shared, |r| r == [(3, 0, 4, 0), vms]);
    select(&mut session, &shared, None);
    regions_where(&session, &shared, |r| r == [vms]);
}

#[test]
fn an_include_alone_knows_only_what_it_sets_itself() {
    let text = "&SCOPED-DEFINE local yes\n\
&IF \"{&local}\" = \"yes\" &THEN\n\
MESSAGE \"own\".\n\
&ELSE\n\
MESSAGE \"never\".\n\
&ENDIF\n\
&SCOPED-DEFINE copy {&outer}\n\
&IF \"{&copy}\" = \"\" &THEN\n\
MESSAGE \"copied\".\n\
&ENDIF\n\
&IF \"{1}\" = \"x\" &THEN\n\
MESSAGE \"argument\".\n\
&ENDIF\n\
&IF \"{&OPSYS}\" = \"VMS\" &THEN\n\
MESSAGE \"shadowed\".\n\
&ENDIF\n";
    let dir = project(BASE, &[("alone.i", text)]);
    let path = dir.path().join("alone.i");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    // A value copied from the includer's name, an argument, and a predefined
    // name the includer can define again are the includer's to decide.
    assert_eq!(next_regions(&session, &path), vec![(4, 0, 5, 0)]);
}

#[test]
fn a_compile_environment_name_the_project_does_not_set_decides_nothing() {
    let text = "&IF \"{&WINDOW-SYSTEM}\" = \"TTY\" &THEN\n\
MESSAGE \"tty\".\n\
&ELSE\n\
MESSAGE \"gui\".\n\
&ENDIF\n\
&IF OPSYS = \"WIN32\" &THEN\n\
MESSAGE \"windows\".\n\
&ENDIF\n\
&IF \"{&BATCH-MODE}\" = \"no\" &THEN\n\
MESSAGE \"interactive\".\n\
&ENDIF\n\
&IF \"{&PROCESS-ARCHITECTURE}\" = \"32\" &THEN\n\
MESSAGE \"32-bit\".\n\
&ENDIF\n";
    // The defaults describe a common compile, not this project's.
    let dir = project(UNSET, &[("env.p", text)]);
    let path = dir.path().join("env.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    assert_eq!(next_regions(&session, &path), vec![]);
    drop(session);

    // The same values, stated, are facts.
    let stated = format!(
        "{UNSET}[workspace.preprocessor]\nopsys = \"UNIX\"\nwindow-system = \"TTY\"\n\
         batch-mode = true\nprocess-architecture = 64\n"
    );
    let dir = project(&stated, &[("env.p", text)]);
    let path = dir.path().join("env.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    assert_eq!(
        next_regions(&session, &path),
        vec![(3, 0, 4, 0), (6, 0, 7, 0), (9, 0, 10, 0), (12, 0, 13, 0)]
    );
}

#[test]
fn a_reference_inside_a_reference_is_expanded_from_the_inside_out() {
    let text = "&GLOBAL-DEFINE OPEN-QUERY-F-Main yes\n\
&SCOPED-DEFINE FRAME-NAME F-Main\n\
&IF \"{&OPEN-QUERY-{&FRAME-NAME}}\" = \"yes\" &THEN\n\
MESSAGE \"a\".\n\
&ELSE\n\
MESSAGE \"b\".\n\
&ENDIF\n\
&IF PROVERSION >= \"12\" &THEN\n\
&SCOPED-DEFINE PART F-Main\n\
&ENDIF\n\
&IF \"{&OPEN-QUERY-{&PART}}\" = \"yes\" &THEN\n\
MESSAGE \"c\".\n\
&ELSE\n\
MESSAGE \"d\".\n\
&ENDIF\n";
    let dir = project(BASE, &[("nested-ref.p", text)]);
    let path = dir.path().join("nested-ref.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    // The first chain takes `a`. The second reads `PART`, which a guessed
    // branch defines, through the inner reference.
    assert_eq!(next_regions(&session, &path), vec![(5, 0, 6, 0)]);
}

#[test]
fn a_chain_inside_an_unclosed_if_is_not_decided() {
    let text = "&IF OPSYS = \"VMS\" &THEN\n\
MESSAGE \"before\".\n\
&ENDIF\n\
&IF OPSYS = \"UNIX\" &THEN\n\
&IF OPSYS = \"VMS\" &THEN\n\
MESSAGE \"x\".\n\
&ENDIF\n\
MESSAGE \"y\".\n";
    let dir = project(BASE, &[("unclosed.p", text)]);
    let path = dir.path().join("unclosed.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    assert_eq!(next_regions(&session, &path), vec![(1, 0, 2, 0)]);
}

#[test]
fn an_include_the_run_did_not_expand_leaves_the_includes_regions_undecided() {
    let inc = "&IF \"{&mode}\" = \"b\" &THEN\nMESSAGE \"b\".\n&ENDIF\n";
    let dir = project(
        BASE,
        &[
            ("inc.i", inc),
            ("once.p", "{inc.i &mode=a}\n"),
            (
                "guessed.p",
                "{inc.i &mode=a}\n&IF PROVERSION >= \"12\" &THEN\n{inc.i &mode=b}\n&ENDIF\n",
            ),
            (
                "searched.p",
                "{mid.i}\n&IF SEARCH(\"x.p\") <> ? &THEN\n{inc.i &mode=b}\n&ENDIF\n",
            ),
            ("mid.i", "{inc.i &mode=a}\n"),
            (
                "computed.p",
                "{inc.i &mode=a}\n&IF PROVERSION >= \"12\" &THEN\n&SCOPED-DEFINE f wrap\n\
                 &ELSE\n&SCOPED-DEFINE f other\n&ENDIF\n{{&f}.i}\n",
            ),
            ("wrap.i", "{inc.i &mode=b}\n"),
            ("other.i", "MESSAGE \"o\".\n"),
            (
                "whole.p",
                "{inc.i &mode=a}\n&IF PROVERSION >= \"12\" &THEN\n\
                 &SCOPED-DEFINE x inc.i &mode=b\n&ENDIF\n{{&x}}\n",
            ),
            ("notfound.p", "{inc.i &mode=a}\n{notfound.i}\n"),
            ("env.p", "{inc.i &mode=a}\nMESSAGE \"{&WINDOW-SYSTEM}\".\n"),
        ],
    );
    let inc_path = dir.path().join("inc.i");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&inc_path, inc);
    next_regions(&session, &inc_path);

    // Expanded once with `mode` a: the branch is not compiled.
    select(&mut session, &inc_path, Some(&dir.path().join("once.p")));
    regions_where(&session, &inc_path, |r| r == [(1, 0, 2, 0)]);

    // A 12.x compile expands it again with `mode` b, so the branch may compile.
    // So may an include whose name a guessed branch chose, and one that oxabl
    // cannot find but the compiler's PROPATH may.
    for root in [
        "guessed.p",
        "searched.p",
        "computed.p",
        "whole.p",
        "notfound.p",
    ] {
        select(&mut session, &inc_path, Some(&dir.path().join(root)));
        assert_eq!(next_regions(&session, &inc_path), vec![], "in {root}");
    }

    // An unset environment name in the root's text is one of a few fixed
    // words, never an include, so it misses no expansion.
    select(&mut session, &inc_path, Some(&dir.path().join("env.p")));
    assert_eq!(
        next_regions(&session, &inc_path),
        vec![(1, 0, 2, 0)],
        "in env.p"
    );
}

#[test]
fn an_include_argument_inside_a_reference_is_read_as_passed() {
    let inc = "&IF \"{&a-{1}}\" = \"yes\" &THEN\nMESSAGE 1.\n&ENDIF\n\
               &IF OPSYS = \"VMS\" &THEN\nMESSAGE 2.\n&ENDIF\n";
    let root = "&IF PROVERSION >= \"12\" &THEN\n&SCOPED-DEFINE a-4 yes\n&ENDIF\n{inc.i 4}\n";
    let dir = project(BASE, &[("inc.i", inc), ("root.p", root)]);
    let inc_path = dir.path().join("inc.i");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&inc_path, inc);
    next_regions(&session, &inc_path);
    // In root.p the name read is `a-4`, which a guessed branch defines.
    select(&mut session, &inc_path, Some(&dir.path().join("root.p")));
    assert_eq!(next_regions(&session, &inc_path), vec![(4, 0, 5, 0)]);
}

#[test]
fn a_computed_name_inside_a_reference_is_read_as_computed() {
    let text = "&IF PROVERSION >= \"12\" &THEN\n\
&SCOPED-DEFINE a-4 yes\n\
&ENDIF\n\
&IF \"{&a-{&LINE-NUMBER}}\" = \"yes\" &THEN\n\
MESSAGE 1.\n\
&ENDIF\n\
&IF OPSYS = \"VMS\" &THEN\n\
MESSAGE 2.\n\
&ENDIF\n";
    let dir = project(BASE, &[("computed.p", text)]);
    let path = dir.path().join("computed.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    // `a-4` is defined on a guess, so line 5 may compile.
    assert_eq!(next_regions(&session, &path), vec![(7, 0, 8, 0)]);
}

#[test]
fn a_database_type_the_project_does_not_name_decides_nothing() {
    let text = "&IF DBTYPE(\"ledger\") = \"ORACLE\" &THEN\n\
MESSAGE \"dataserver\".\n\
&ENDIF\n\
&IF CONNECTED(\"stock\") &THEN\n\
MESSAGE \"stock\".\n\
&ENDIF\n";
    // With one environment key stated, and with all four, so that every other
    // name is known.
    let all_four =
        format!("{BASE}window-system = \"TTY\"\nbatch-mode = true\nprocess-architecture = 64\n");
    for stated in [BASE, all_four.as_str()] {
        let listed = format!("{stated}databases = [\"ledger\"]\n");
        let dir = project(&listed, &[("db.p", text)]);
        let path = dir.path().join("db.p");
        let mut session = Session::start_with(dir.path(), asks());
        session.open(&path, text);
        // `PROGRESS` is only the default type; the stated list does decide
        // `CONNECTED`.
        assert_eq!(
            next_regions(&session, &path),
            vec![(4, 0, 5, 0)],
            "with {stated}"
        );
    }
    let listed = format!("{BASE}databases = [\"ledger\"]\n");

    let typed = format!("{listed}database-types = {{ ledger = \"PROGRESS\" }}\n");
    let dir = project(&typed, &[("db.p", text)]);
    let path = dir.path().join("db.p");
    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, text);
    assert_eq!(
        next_regions(&session, &path),
        vec![(1, 0, 2, 0), (4, 0, 5, 0)]
    );
}

/// A value that holds a reference to its own name, read inside another
/// reference: `oxabl check` and the language server both finish.
const SELF_REFERENCE: &str = "&SCOPED-DEFINE b ~{&b}\n\
&SCOPED-DEFINE c {&c}{&c}\n\
&IF \"{&a-{&b}}\" = \"x\" &THEN\n\
MESSAGE 1.\n\
&ENDIF\n\
&IF \"{&a-{&c}}\" = \"x\" &THEN\n\
MESSAGE 2.\n\
&ENDIF\n";

#[test]
fn a_value_that_names_itself_inside_a_reference_ends() {
    let dir = project(BASE, &[("self.p", SELF_REFERENCE)]);
    let path = dir.path().join("self.p");
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .arg("check")
        .arg(&path)
        .current_dir(dir.path())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            let _ = child.kill();
            panic!("`oxabl check` did not finish in 20 s");
        }
        std::thread::sleep(Duration::from_millis(50));
    }

    let mut session = Session::start_with(dir.path(), asks());
    session.open(&path, SELF_REFERENCE);
    next_regions(&session, &path);
}

#[test]
fn a_client_that_does_not_ask_gets_no_regions() {
    let dir = project(BASE, &[("nested.p", NESTED)]);
    let path = dir.path().join("nested.p");
    let mut session = Session::start(dir.path());
    session.open(&path, NESTED);
    let uri = json!(file_uri(&path));
    let no_regions = |message: &Value| {
        assert_ne!(
            message["method"],
            json!("textDocument/inactiveRegions"),
            "{message}"
        );
    };
    // The open, then an edit with its debounced analysis, then a quiet second.
    let mut publishes = 0;
    let end = Instant::now() + Duration::from_secs(60);
    while publishes < 2 {
        let left = end.saturating_duration_since(Instant::now());
        let message = session
            .try_next(left)
            .expect("the diagnostics of the buffer");
        no_regions(&message);
        if message["method"] == json!("textDocument/publishDiagnostics")
            && message["params"]["uri"] == uri
        {
            publishes += 1;
            if publishes == 1 {
                session.send(json!({
                    "jsonrpc": "2.0", "method": "textDocument/didChange",
                    "params": {
                        "textDocument": {"uri": file_uri(&path), "version": 2},
                        "contentChanges": [{"text": format!("{NESTED}MESSAGE \"more\".\n")}]
                    }
                }));
            }
        }
    }
    let quiet = Instant::now() + Duration::from_secs(1);
    while let Some(left) = quiet.checked_duration_since(Instant::now()) {
        match session.try_next(left) {
            Some(message) => no_regions(&message),
            None => break,
        }
    }
}
