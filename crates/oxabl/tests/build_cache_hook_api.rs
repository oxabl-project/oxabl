//! What a build offers a cache, through the public `oxabl::build` API with a
//! recording cache and the fake AVM: the hook that the cache slice fills.

mod common;

use std::time::{Duration, Instant};

use common::built::Built;
use common::remote_double::Remote;
use oxabl::build::config::BuildWorkspace;
use oxabl::build::run::{BuildReport, BuildRequest, build};
use oxabl::build::worker::Cancel;

/// The recording cache of these tests is the shared remote double.
struct Seen(Remote);

impl Seen {
    fn paths(&self) -> Vec<String> {
        self.0.pushed_paths()
    }

    fn finished(&self) -> usize {
        self.0.finished()
    }
}

fn run_build(workspace: &Built, cancel: Cancel) -> (BuildReport, Seen) {
    let config = BuildWorkspace::open(Some(workspace.root())).expect("a usable configuration");
    let remote = Remote::new();
    let report = build(
        &config,
        BuildRequest {
            cancel,
            cache: remote.cache(),
            ..BuildRequest::default()
        },
    )
    .expect("the build runs");
    (report, Seen(remote))
}

#[test]
fn every_keyable_unit_that_compiled_is_offered_once_and_the_uploader_finishes_once() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    workspace.write("src/bad.p", "{missing.i}\n");

    let (report, seen) = run_build(&workspace, Cancel::new());

    assert_eq!(report.exit, 1, "an unresolved include fails the compile");
    assert_eq!(seen.paths(), ["r/a.r", "r/b.r"]);
    assert_eq!(seen.finished(), 1);
    assert_eq!(
        seen.0.outcomes().len(),
        2,
        "each pushed set is stored: its size and hash matched the file"
    );
}

#[test]
fn units_without_a_key_are_not_offered_and_the_others_are() {
    let workspace = Built::with(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    let user = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n";
    workspace.write("a/x.p", user);
    workspace.write("b/y.p", user);
    workspace.write("a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("b/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("a/plain.p", "MESSAGE \"plain\".\n");

    let (report, seen) = run_build(&workspace, Cancel::new());

    assert_eq!(report.exit, 0);
    assert_eq!(
        seen.paths(),
        ["r/plain.r"],
        "only the unit with a key is offered"
    );
}

#[test]
fn a_unit_that_compiled_after_a_failure_in_its_closure_is_not_offered() {
    let workspace = Built::new();
    workspace.write(
        "src/pkg/Base.cls",
        "/* FAKE-FAIL */\nCLASS pkg.Base:\nEND CLASS.\n",
    );
    workspace.write(
        "src/pkg/User.cls",
        "CLASS pkg.User INHERITS pkg.Base:\nEND CLASS.\n",
    );
    workspace.write("src/solo.p", "MESSAGE \"solo\".\n");

    let (report, seen) = run_build(&workspace, Cancel::new());

    assert_eq!(report.exit, 1);
    assert!(
        workspace.exists("build/r/pkg/User.r"),
        "the user still compiles"
    );
    assert_eq!(
        seen.paths(),
        ["r/solo.r"],
        "a tainted unit is recorded but never offered"
    );
}

#[test]
fn a_cancel_offers_what_was_released_before_it_and_nothing_still_compiling() {
    let workspace = Built::new();
    workspace.workers("count = 1\ncancel_grace = \"30s\"");
    for name in ["a1", "a2"] {
        workspace.write(&format!("src/{name}.p"), "MESSAGE \"fast\".\n");
    }
    for name in ["z1", "z2", "z3"] {
        workspace.write(
            &format!("src/{name}.p"),
            "/* FAKE-SLOW */\nMESSAGE \"slow\".\n",
        );
    }
    let cancel = Cancel::new();
    let watcher = {
        let cancel = cancel.clone();
        let log = workspace.log();
        std::thread::spawn(move || {
            let deadline = Instant::now() + Duration::from_secs(30);
            while Instant::now() < deadline {
                let text = std::fs::read_to_string(&log).unwrap_or_default();
                if text.lines().any(|line| line.ends_with(" z1.p")) {
                    cancel.cancel();
                    return;
                }
                std::thread::sleep(Duration::from_millis(10));
            }
        })
    };

    let (report, seen) = run_build(&workspace, cancel);
    watcher.join().unwrap();

    assert_eq!(report.exit, 130);
    assert_eq!(
        seen.paths(),
        ["r/a1.r", "r/a2.r"],
        "z1 was still compiling at the signal and is recorded but not offered"
    );
    assert_eq!(seen.finished(), 1, "finish runs once");
    assert!(
        workspace.exists("build/r/z1.r"),
        "the unit in flight was installed"
    );
    assert!(!workspace.exists("build/r/z2.r"));
}

#[test]
fn a_cancel_releases_nothing_that_no_schema_check_released() {
    let workspace = Built::new();
    workspace.workers("count = 1\ncancel_grace = \"30s\"");
    workspace.databases(&["ledger"]);
    let records: Vec<Vec<String>> =
        serde_json::from_str(r#"[["db","ledger"],["file","customer"]]"#).unwrap();
    workspace.set_environment(&common::env_with_schema(records));
    for name in ["a1", "a2"] {
        workspace.write(&format!("src/{name}.p"), "MESSAGE \"fast\".\n");
    }
    workspace.write("src/z1.p", "/* FAKE-SLOW */\nMESSAGE \"slow\".\n");
    let cancel = Cancel::new();
    let watcher = {
        let cancel = cancel.clone();
        let log = workspace.log();
        std::thread::spawn(move || {
            let deadline = Instant::now() + Duration::from_secs(30);
            while Instant::now() < deadline {
                let text = std::fs::read_to_string(&log).unwrap_or_default();
                if text.lines().any(|line| line.ends_with(" z1.p")) {
                    cancel.cancel();
                    return;
                }
                std::thread::sleep(Duration::from_millis(10));
            }
        })
    };

    let (report, seen) = run_build(&workspace, cancel);
    watcher.join().unwrap();

    assert_eq!(report.exit, 130);
    assert!(
        seen.paths().is_empty(),
        "the default batch holds both units for a check that never ran: {:?}",
        seen.paths()
    );
    assert_eq!(seen.finished(), 1);
}

#[test]
fn a_slow_cache_does_not_hold_up_the_compiles() {
    let workspace = Built::new();
    let names = ["a", "b", "c", "d"];
    for name in names {
        workspace.write(&format!("src/{name}.p"), "MESSAGE \"x\".\n");
    }
    let config = BuildWorkspace::open(Some(workspace.root())).expect("a usable configuration");
    let remote = Remote::new();
    let waited = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    {
        let (log, waited) = (workspace.log(), waited.clone());
        // The first push waits until every unit compiled, or ten seconds.
        remote.on_push_read(move |_| {
            let mut waited = waited.lock().unwrap();
            if !waited.is_empty() {
                return;
            }
            let deadline = Instant::now() + Duration::from_secs(10);
            let all = loop {
                let text = std::fs::read_to_string(&log).unwrap_or_default();
                let compiled = names
                    .iter()
                    .filter(|name| {
                        text.lines()
                            .any(|line| line.ends_with(&format!(" {name}.p")))
                    })
                    .count();
                if compiled == names.len() || Instant::now() >= deadline {
                    break compiled;
                }
                std::thread::sleep(Duration::from_millis(10));
            };
            waited.push(all);
        });
    }

    let report = build(
        &config,
        BuildRequest {
            cache: remote.cache(),
            ..BuildRequest::default()
        },
    )
    .expect("the build runs");

    assert_eq!(report.exit, 0);
    assert_eq!(
        *waited.lock().unwrap(),
        [names.len()],
        "the compiles waited for the cache to take a set"
    );
    assert_eq!(remote.pushed_paths(), ["r/a.r", "r/b.r", "r/c.r", "r/d.r"]);
}
