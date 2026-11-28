//! Scripted series of edits through the real `oxabl lsp` and its daemon: a
//! buffer edit, a write on disk, a file created or removed, a buffer opened or
//! closed. After each step the references that the daemon gives must be those
//! of a new daemon on the same text, and a test states some of them as well.
//! The workspace symbols and the incoming calls are compared too: the pass
//! keeps their rows, and a search over files does not read them.
//!
//! The tests here are the ones that reach what the workspace pass keeps of the
//! paths it read: a path that no file reads for a while, a file that is
//! removed and made again, a path that was looked for and was not there, a
//! directory that was listed, and a buffer at a path that does not exist.
//! `incremental_pass_lsp.rs` tests the pass on one fixed workspace.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Launch, Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"script\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

/// The files every script starts with: two procedures and a caller of both.
const BASE: [(&str, &str); 3] = [
    ("b.p", "MESSAGE \"b\".\n"),
    ("c.p", "MESSAGE \"c\".\n"),
    ("q.p", "RUN b.p.\nRUN c.p.\n"),
];

type Answers = BTreeMap<String, Vec<String>>;

/// One change of the workspace.
#[derive(Clone, Copy)]
enum Step {
    /// A new text of an open buffer.
    Edit(&'static str, &'static str),
    /// A write on disk, of a new file or an old one.
    Write(&'static str, &'static str),
    Remove(&'static str),
    /// Open a buffer of a file that is not on disk.
    OpenNew(&'static str, &'static str),
    Close(&'static str),
}

struct Script {
    config: &'static str,
    files: Vec<(&'static str, &'static str)>,
    /// The files open at the start.
    opened: Vec<&'static str>,
    /// The references are asked at the first `needle` of `file`, which must
    /// stay open.
    queries: Vec<(&'static str, &'static str)>,
}

struct Run {
    _dir: tempfile::TempDir,
    root: PathBuf,
    disk: BTreeMap<&'static str, String>,
    buffers: BTreeMap<&'static str, String>,
    versions: BTreeMap<&'static str, u64>,
    session: Session,
    queries: Vec<(&'static str, &'static str)>,
}

fn write(root: &Path, name: &str, text: &str) {
    let path = root.join(name);
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, text).unwrap();
}

fn start(root: &Path) -> Session {
    let launch = Launch {
        env: &[("OXABL_TEST_PASS_COUNTS", "1")],
        ..Launch::default()
    };
    Session::launch_with(root, json!({"capabilities": {}}), launch, |_| {})
        .await_initialize()
        .0
}

impl Run {
    fn new(script: &Script) -> Run {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        write(&root, "oxabl.toml", script.config);
        let mut disk = BTreeMap::new();
        for &(name, text) in &script.files {
            write(&root, name, text);
            disk.insert(name, text.to_string());
        }
        let mut session = start(&root);
        let mut buffers = BTreeMap::new();
        let mut versions = BTreeMap::new();
        for &name in &script.opened {
            session.open(&root.join(name), &disk[name]);
            buffers.insert(name, disk[name].clone());
            versions.insert(name, 1);
        }
        Run {
            _dir: dir,
            root,
            disk,
            buffers,
            versions,
            session,
            queries: script.queries.clone(),
        }
    }

    fn text(&self, name: &str) -> &str {
        self.buffers
            .get(name)
            .or_else(|| self.disk.get(name))
            .unwrap_or_else(|| panic!("no text of {name}"))
    }

    fn apply(&mut self, step: Step) {
        match step {
            Step::Edit(name, text) => {
                let version = self.versions.get_mut(name).expect("an open buffer");
                *version += 1;
                self.session.send(json!({
                    "jsonrpc": "2.0", "method": "textDocument/didChange",
                    "params": {
                        "textDocument": {
                            "uri": file_uri(&self.root.join(name)),
                            "version": *version
                        },
                        "contentChanges": [{"text": text}]
                    }
                }));
                self.buffers.insert(name, text.to_string());
            }
            Step::Write(name, text) => {
                write(&self.root, name, text);
                self.disk.insert(name, text.to_string());
                wait_for_the_walk();
            }
            Step::Remove(name) => {
                std::fs::remove_file(self.root.join(name)).unwrap();
                self.disk.remove(name);
                wait_for_the_walk();
            }
            Step::OpenNew(name, text) => {
                self.session.open(&self.root.join(name), text);
                self.buffers.insert(name, text.to_string());
                self.versions.insert(name, 1);
            }
            Step::Close(name) => {
                self.session.send(json!({
                    "jsonrpc": "2.0", "method": "textDocument/didClose",
                    "params": {"textDocument": {"uri": file_uri(&self.root.join(name))}}
                }));
                self.buffers.remove(name);
                self.versions.remove(name);
            }
        }
    }

    /// The references of every query, by a name for each.
    fn answers(&mut self) -> Answers {
        let mut found = BTreeMap::new();
        for (name, needle) in self.queries.clone() {
            let text = self.text(name).to_string();
            let at = text
                .find(needle)
                .unwrap_or_else(|| panic!("no `{needle}` in {name}"))
                + 1;
            let line = text[..at].matches('\n').count();
            let start = text[..at].rfind('\n').map_or(0, |n| n + 1);
            let character = text[start..at].encode_utf16().count();
            let answer = self.session.request(
                "textDocument/references",
                json!({
                    "textDocument": {"uri": file_uri(&self.root.join(name))},
                    "position": {"line": line, "character": character},
                    "context": {"includeDeclaration": true}
                }),
            );
            assert!(answer.get("error").is_none(), "references gave {answer}");
            found.insert(format!("{name}:{needle}"), locations(&answer["result"]));
        }
        found.insert("workspace symbols".to_string(), self.symbols());
        for name in self.buffers.keys().copied().collect::<Vec<_>>() {
            found.insert(format!("incoming calls {name}"), self.incoming_calls(name));
        }
        found
    }

    /// Every symbol of the workspace, as one line each, in a fixed order. The
    /// symbol rows are kept by the pass, and a search over files does not read
    /// them, so a row of a file that is gone shows only here.
    fn symbols(&mut self) -> Vec<String> {
        let answer = self
            .session
            .request("workspace/symbol", json!({"query": ""}));
        assert!(answer.get("error").is_none(), "symbol gave {answer}");
        let mut rows: Vec<String> = answer["result"]
            .as_array()
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(|row| {
                let uri = row["location"]["uri"].as_str().unwrap_or_default();
                format!(
                    "{} {} {} {}",
                    row["name"],
                    row["kind"],
                    row["containerName"],
                    uri.rsplit('/').next().unwrap_or_default()
                )
            })
            .collect();
        rows.sort();
        rows
    }

    /// The callers of the program in `name`, from the dependency graph that
    /// the pass keeps.
    fn incoming_calls(&mut self, name: &str) -> Vec<String> {
        let items = self.session.request(
            "textDocument/prepareCallHierarchy",
            json!({
                "textDocument": {"uri": file_uri(&self.root.join(name))},
                "position": {"line": 0, "character": 0}
            }),
        );
        assert!(items.get("error").is_none(), "prepare gave {items}");
        let mut found = Vec::new();
        for item in items["result"].as_array().cloned().unwrap_or_default() {
            let incoming = self
                .session
                .request("callHierarchy/incomingCalls", json!({"item": item.clone()}));
            assert!(incoming.get("error").is_none(), "incoming gave {incoming}");
            for call in incoming["result"].as_array().cloned().unwrap_or_default() {
                let uri = call["from"]["uri"].as_str().unwrap_or_default();
                found.push(format!(
                    "{} {} {}",
                    item["name"],
                    call["from"]["name"],
                    uri.rsplit('/').next().unwrap_or_default()
                ));
            }
        }
        found.sort();
        found
    }

    /// The answers of a new daemon with the same buffers open.
    fn fresh_answers(&self) -> Answers {
        let mut fresh = Run {
            _dir: tempfile::tempdir().unwrap(),
            root: self.root.clone(),
            disk: self.disk.clone(),
            buffers: self.buffers.clone(),
            versions: BTreeMap::new(),
            session: start(&self.root),
            queries: self.queries.clone(),
        };
        for (name, text) in self.buffers.clone() {
            fresh.session.open(&self.root.join(name), &text);
        }
        let found = fresh.answers();
        let runs = passes(&fresh.session);
        assert!(
            runs.first()
                .is_some_and(|&(analysed, files)| analysed == files),
            "the first pass of a new daemon analyses every file: {runs:?}"
        );
        found
    }

    /// Apply `step`, and check the answers against a new daemon.
    #[track_caller]
    fn step(&mut self, what: &str, step: Step) -> Answers {
        self.apply(step);
        let got = self.answers();
        let want = self.fresh_answers();
        assert_eq!(
            got, want,
            "after {what}, the answers differ from a new daemon"
        );
        got
    }
}

/// A file that appears on disk is found by a walk of the tree that the daemon
/// runs at most once in each second.
fn wait_for_the_walk() {
    std::thread::sleep(std::time::Duration::from_millis(1_200));
}

fn locations(result: &Value) -> Vec<String> {
    let mut found: Vec<String> = result
        .as_array()
        .cloned()
        .unwrap_or_default()
        .iter()
        .map(|place| {
            let uri = place["uri"].as_str().unwrap();
            let name = uri.rsplit('/').next().unwrap();
            let start = &place["range"]["start"];
            format!("{name}@{}:{}", start["line"], start["character"])
        })
        .collect();
    found.sort();
    found
}

/// `(analysed, files)` of each pass the daemon of `session` ran so far.
fn passes(session: &Session) -> Vec<(usize, usize)> {
    session
        .stderr()
        .lines()
        .filter_map(|line| line.strip_prefix("oxabl-test: workspace pass analysed "))
        .map(|rest| {
            let words: Vec<&str> = rest.split_whitespace().collect();
            (words[0].parse().unwrap(), words[2].parse().unwrap())
        })
        .collect()
}

/// The bytes that each pass kept, in order.
fn kept_bytes(session: &Session) -> Vec<usize> {
    session
        .stderr()
        .lines()
        .filter(|line| line.starts_with("oxabl-test: workspace pass analysed "))
        .filter_map(|line| line.rsplit_once(", kept ")?.1.split_once(" bytes"))
        .filter_map(|(bytes, _)| bytes.parse().ok())
        .collect()
}

fn script(files: &[(&'static str, &'static str)], opened: &[&'static str]) -> Script {
    Script {
        config: CONFIG,
        files: BASE.iter().chain(files).copied().collect(),
        opened: opened.to_vec(),
        queries: vec![("q.p", "b.p"), ("q.p", "c.p")],
    }
}

fn has(answers: &Answers, query: &str, place: &str) -> bool {
    answers[query].iter().any(|found| found == place)
}

#[test]
fn an_include_that_nobody_read_for_a_while_is_read_again_as_it_is() {
    let mut run = Run::new(&script(
        &[("a.p", "{x.i}\nMESSAGE 1.\n"), ("x.i", "RUN b.p.\n")],
        &["a.p", "x.i", "q.p"],
    ));
    let first = run.step("the start", Step::Edit("q.p", BASE[2].1));
    assert!(has(&first, "q.p:b.p", "x.i@0:4"), "{first:?}");

    run.step("a.p drops the include", Step::Edit("a.p", "MESSAGE 1.\n"));
    run.step(
        "x.i changes while no file reads it",
        Step::Edit("x.i", "RUN c.p.\n"),
    );
    run.step(
        "a.p includes x.i again",
        Step::Edit("a.p", "{x.i}\nMESSAGE 1.\n"),
    );
    let last = run.step(
        "x.i is as the first pass read it",
        Step::Edit("x.i", "RUN b.p.\n"),
    );
    assert!(
        has(&last, "q.p:b.p", "x.i@0:4"),
        "the RUN of x.i is in the graph: {last:?}"
    );
}

#[test]
fn an_include_on_disk_that_nobody_read_for_a_while_is_read_again_as_it_is() {
    let mut run = Run::new(&script(
        &[("a.p", "{x.i}\nMESSAGE 1.\n"), ("x.i", "RUN b.p.\n")],
        &["a.p", "q.p"],
    ));
    let first = run.step("the start", Step::Edit("a.p", "{x.i}\nMESSAGE 1.\n"));
    assert!(has(&first, "q.p:b.p", "x.i@0:4"), "{first:?}");
    run.step("a.p drops the include", Step::Edit("a.p", "MESSAGE 1.\n"));
    run.step("x.i changes on disk", Step::Write("x.i", "RUN c.p.\n"));
    run.step(
        "a.p includes x.i again",
        Step::Edit("a.p", "{x.i}\nMESSAGE 1.\n"),
    );
    let last = run.step(
        "x.i is as the first pass read it",
        Step::Write("x.i", "RUN b.p.\n"),
    );
    assert!(has(&last, "q.p:b.p", "x.i@0:4"), "{last:?}");
}

#[test]
fn a_file_removed_and_made_again_is_analysed_as_it_is() {
    let mut run = Run::new(&script(&[("d.p", "RUN b.p.\n")], &["q.p"]));
    let first = run.step("the start", Step::Write("d.p", "RUN b.p.\n"));
    assert!(has(&first, "q.p:b.p", "d.p@0:4"), "{first:?}");
    run.step("d.p is removed", Step::Remove("d.p"));
    run.step(
        "d.p is made with other text",
        Step::Write("d.p", "RUN c.p.\n"),
    );
    let last = run.step("d.p has the first text", Step::Write("d.p", "RUN b.p.\n"));
    assert!(has(&last, "q.p:b.p", "d.p@0:4"), "{last:?}");
}

#[test]
fn a_removed_file_is_gone_from_the_symbols_and_the_calls() {
    let mut run = Run::new(&script(&[("d.p", "RUN b.p.\n")], &["q.p", "b.p"]));
    let first = run.step("the start", Step::Edit("q.p", BASE[2].1));
    assert!(
        first["workspace symbols"]
            .iter()
            .any(|row| row.contains("d.p")),
        "d.p has a symbol: {first:?}"
    );
    let gone = run.step("d.p is removed", Step::Remove("d.p"));
    assert!(
        !gone["workspace symbols"]
            .iter()
            .any(|row| row.contains("d.p")),
        "d.p has no symbol: {gone:?}"
    );
    assert!(
        gone.values().flatten().all(|row| !row.contains("d.p")),
        "no answer names d.p: {gone:?}"
    );
}

#[test]
fn a_file_made_where_a_run_target_was_not_found_is_seen() {
    let mut run = Run::new(&Script {
        queries: vec![("q.p", "b.p"), ("q.p", "n.p")],
        ..script(
            &[("a.p", "RUN n.p.\n"), ("q.p", "RUN b.p.\nRUN n.p.\n")],
            &["q.p"],
        )
    });
    let made = run.step("n.p is made", Step::Write("n.p", "RUN b.p.\n"));
    assert!(
        has(&made, "q.p:n.p", "a.p@0:4"),
        "a.p runs the new file: {made:?}"
    );
}

#[test]
fn a_class_made_where_none_was_found_is_seen() {
    let mut run = Run::new(&Script {
        queries: vec![("q.p", "b.p"), ("a.p", "pkg.Foo")],
        ..script(
            &[(
                "a.p",
                "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
            )],
            &["q.p", "a.p"],
        )
    });
    let made = run.step(
        "pkg/Foo.cls is made",
        Step::Write("pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n"),
    );
    assert!(
        made["a.p:pkg.Foo"]
            .iter()
            .any(|place| place.starts_with("Foo.cls@")),
        "the class is found: {made:?}"
    );
}

#[test]
fn an_include_made_where_none_was_found_is_seen() {
    let mut run = Run::new(&script(&[("a.p", "{x.i}\nMESSAGE 1.\n")], &["a.p", "q.p"]));
    run.step("the start", Step::Edit("a.p", "{x.i}\nMESSAGE 1.\n"));
    // A new include is no root file, so no count of files sees it: the path
    // was looked for, and it is there now.
    let made = run.step("x.i is made", Step::Write("x.i", "RUN b.p.\n"));
    assert!(has(&made, "q.p:b.p", "x.i@0:4"), "a.p reads x.i: {made:?}");
}

#[test]
fn an_include_made_ahead_of_the_one_in_use_is_seen() {
    let mut run = Run::new(&Script {
        config: "[workspace]\nname = \"script\"\n\
            [workspace.sources]\ninclude_paths = [\"inc1\", \"inc2\", \".\"]\n",
        ..script(
            &[
                ("a.p", "{x.i}\nMESSAGE 1.\n"),
                ("inc1/keep.txt", ""),
                ("inc2/x.i", "RUN b.p.\n"),
            ],
            &["a.p", "q.p"],
        )
    });
    let first = run.step("the start", Step::Edit("a.p", "{x.i}\nMESSAGE 1.\n"));
    assert!(has(&first, "q.p:b.p", "x.i@0:4"), "{first:?}");
    let made = run.step("inc1/x.i is made", Step::Write("inc1/x.i", "RUN c.p.\n"));
    assert!(
        has(&made, "q.p:c.p", "x.i@0:4"),
        "a.p reads inc1/x.i: {made:?}"
    );
    run.step("inc1/x.i is removed", Step::Remove("inc1/x.i"));
}

#[test]
fn a_class_made_in_a_directory_that_was_listed_is_found_by_the_next_pass() {
    let class = "CLASS pkg.Foo:\nEND CLASS.\n";
    let mut run = Run::new(&Script {
        config: "[workspace]\nname = \"script\"\n\
            [workspace.sources]\ninclude_paths = [\".\"]\ncase_insensitive_lookup = true\n",
        ..script(
            &[(
                "a.p",
                "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
            )],
            &["a.p", "q.p"],
        )
    });
    run.answers();
    // `pkg/Foo.cls` is not there, and `Pkg/Foo.cls` is the file that the lookup
    // finds. a.p does not name it, so only the listing of the directory says
    // that a.p must be read again.
    run.apply(Step::Write("Pkg/Foo.cls", class));
    run.apply(Step::OpenNew("Pkg/Foo.cls", class));
    run.queries.push(("Pkg/Foo.cls", "pkg.Foo"));
    let made = run.step(
        "a pass after Pkg/Foo.cls is made",
        Step::Edit("q.p", "RUN b.p.\nRUN c.p.\n\n"),
    );
    assert!(
        made["Pkg/Foo.cls:pkg.Foo"]
            .iter()
            .any(|place| place.starts_with("a.p@")),
        "a.p uses the class: {made:?}"
    );
}

#[test]
fn a_buffer_at_a_path_that_was_looked_for_is_read() {
    // `inc/x.i` is searched first and is not there; the buffer is that file.
    let mut run = Run::new(&Script {
        config: "[workspace]\nname = \"script\"\n\
            [workspace.sources]\ninclude_paths = [\"inc\", \".\"]\n",
        ..script(
            &[("a.p", "{x.i}\n"), ("x.i", "RUN b.p.\n")],
            &["q.p", "a.p"],
        )
    });
    let first = run.step("the start", Step::Edit("a.p", "{x.i}\n"));
    assert!(has(&first, "q.p:b.p", "x.i@0:4"), "{first:?}");
    let opened = run.step(
        "a buffer at inc/x.i",
        Step::OpenNew("inc/x.i", "RUN c.p.\n"),
    );
    assert!(
        has(&opened, "q.p:c.p", "x.i@0:4"),
        "the include is the buffer: {opened:?}"
    );
    run.step("the buffer closes", Step::Close("inc/x.i"));
}

#[test]
fn a_lookup_that_ignores_case_finds_a_file_the_pass_sees() {
    let mut run = Run::new(&Script {
        config: "[workspace]\nname = \"script\"\n\
            [workspace.sources]\ninclude_paths = [\".\"]\ncase_insensitive_lookup = true\n",
        queries: vec![("e.p", "lib.p")],
        ..script(
            &[
                ("a.p", "RUN Lib.p.\n"),
                ("e.p", "RUN lib.p.\n"),
                ("lib.p", "MESSAGE 1.\n"),
            ],
            &["e.p"],
        )
    });
    // `Lib.p` and `lib.p` name one file, so each runs it.
    let first = run.step("the start", Step::Edit("e.p", "RUN lib.p.\n"));
    assert!(
        has(&first, "e.p:lib.p", "a.p@0:4"),
        "a.p runs lib.p: {first:?}"
    );
    // The directory that was listed changes: lib.p goes, LIB.p comes.
    run.apply(Step::Remove("lib.p"));
    let seen = passes(&run.session).len();
    let renamed = run.step(
        "lib.p is replaced by LIB.p",
        Step::Write("LIB.p", "MESSAGE 2.\n"),
    );
    assert!(
        has(&renamed, "e.p:lib.p", "a.p@0:4"),
        "a.p runs LIB.p: {renamed:?}"
    );
    let runs = passes(&run.session);
    assert!(
        runs[seen..]
            .iter()
            .any(|&(analysed, files)| analysed > 0 && analysed < files),
        "the change analysed the files that listed the directory: {runs:?}"
    );
}

#[test]
fn the_kept_paths_do_not_grow_with_the_paths_that_were_read_once() {
    let mut run = Run::new(&script(&[("a.p", "MESSAGE 1.\n")], &["a.p", "q.p"]));
    run.answers();
    for round in 0..30 {
        // Each round a.p includes names that exist nowhere, and then drops
        // them: the paths of the include search are read and forgotten.
        let with = format!("{{n{round}a.i}}\n{{n{round}b.i}}\nMESSAGE 1.\n");
        run.apply(Step::Edit("a.p", Box::leak(with.into_boxed_str())));
        run.answers();
        run.apply(Step::Edit("a.p", "MESSAGE 1.\n"));
        run.answers();
    }
    let sizes = kept_bytes(&run.session);
    assert!(sizes.len() >= 60, "a pass for each edit: {}", sizes.len());
    let (early, late) = (sizes[6], sizes[sizes.len() - 1]);
    assert!(
        late <= early + early / 20,
        "the kept bytes grew from {early} to {late}: {sizes:?}"
    );
}

/// A schema that the cache gives back and one that was parsed are one schema:
/// a pass after the cache is gone reads the configuration again, and is not
/// full for it. A schema that changed is another one.
#[test]
fn a_schema_from_the_cache_and_a_parsed_one_do_not_make_a_pass_full() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().canonicalize().unwrap();
    let cache = tempfile::tempdir().unwrap();
    let cache_dir = cache.path().join("schema");
    write(
        &root,
        "oxabl.toml",
        "[workspace]\nname = \"shop\"\n[workspace.schema]\nfiles = [\"db/shop.df\"]\n\
         [workspace.sources]\ninclude_paths = [\".\"]\n",
    );
    let mut schema = String::new();
    for table in 0..40 {
        schema.push_str(&format!("ADD TABLE \"T{table}\"\n"));
        for field in 0..8 {
            schema.push_str(&format!(
                "ADD FIELD \"F{field}\" OF \"T{table}\" AS character\n"
            ));
        }
        schema.push_str(&format!(
            "ADD INDEX \"Ix\" ON \"T{table}\"\nINDEX-FIELD \"F0\" ASCENDING\n"
        ));
    }
    write(&root, "db/shop.df", &schema);
    write(&root, "q.p", "RUN b.p.\nRUN c.p.\n");
    write(&root, "b.p", "FIND FIRST T1.\n");
    write(&root, "c.p", "FIND FIRST T2.\n");
    for n in 0..20 {
        write(&root, &format!("f{n}.p"), "MESSAGE 1.\n");
    }
    let launch = Launch {
        env: &[("OXABL_TEST_PASS_COUNTS", "1")],
        schema_cache: Some(&cache_dir),
        ..Launch::default()
    };
    let mut session = Session::launch_with(&root, json!({}), launch, |_| {})
        .await_initialize()
        .0;
    let text = "RUN b.p.\nRUN c.p.\n";
    session.open(&root.join("q.p"), text);
    let ask = |session: &mut Session| {
        let answer = session.request(
            "textDocument/references",
            json!({
                "textDocument": {"uri": file_uri(&root.join("q.p"))},
                "position": {"line": 0, "character": 5},
                "context": {"includeDeclaration": true}
            }),
        );
        assert!(answer.get("error").is_none(), "references gave {answer}");
    };
    let edit = |session: &mut Session, text: &str, version: u64| {
        session.send(json!({
            "jsonrpc": "2.0", "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": file_uri(&root.join("q.p")), "version": version},
                "contentChanges": [{"text": text}]
            }
        }));
    };
    ask(&mut session);
    assert!(
        std::fs::read_dir(&cache_dir).is_ok_and(|mut entries| entries.next().is_some()),
        "the schema is in the cache"
    );

    // The cache is gone: the next pass parses the schema it read from the
    // cache before.
    for entry in std::fs::read_dir(&cache_dir).unwrap().flatten() {
        std::fs::remove_file(entry.path()).unwrap();
    }
    edit(&mut session, &format!("{text}\n"), 2);
    ask(&mut session);
    // The cache is full again: the next pass reads the schema from it.
    edit(&mut session, &format!("{text}\n\n"), 3);
    ask(&mut session);
    let runs = passes(&session);
    assert!(runs.len() >= 3, "a pass for each edit: {runs:?}");
    assert!(
        runs[1..].iter().all(|&(analysed, files)| analysed < files),
        "a schema from the cache and a parsed one made a pass full: {runs:?}"
    );

    // A field of the schema changes on disk, and no client says so: the next
    // pass is full.
    write(
        &root,
        "db/shop.df",
        &schema.replace(
            "\"F0\" OF \"T1\" AS character",
            "\"F0\" OF \"T1\" AS integer",
        ),
    );
    edit(&mut session, &format!("{text}\n\n\n"), 4);
    ask(&mut session);
    let runs = passes(&session);
    let last = runs.last().unwrap();
    assert_eq!(
        last.0, last.1,
        "a changed schema made no full pass: {runs:?}"
    );
}
