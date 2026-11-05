//! What a build offers a cache, through the public `oxabl::build` API with a
//! recording cache and the fake AVM: the hook that the cache slice fills.

mod common;

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use common::built::Built;
use oxabl::build::cache::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, CompileKey, StoreOutcome,
};
use oxabl::build::config::BuildWorkspace;
use oxabl::build::run::{BuildReport, BuildRequest, build};
use oxabl::build::worker::Cancel;

#[derive(Default)]
struct Seen {
    pushed: Mutex<Vec<ArtifactSetRef>>,
    finished: AtomicUsize,
}

struct Recording(Arc<Seen>);

struct Uploader(Arc<Seen>);

impl ArtifactCache for Recording {
    fn label(&self) -> &str {
        "recording"
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        Box::new(Uploader(self.0.clone()))
    }
}

impl ArtifactUploader for Uploader {
    fn push(&mut self, set: ArtifactSetRef) {
        self.0.pushed.lock().unwrap().push(set);
    }

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        self.0.finished.fetch_add(1, Ordering::SeqCst);
        Vec::new()
    }
}

impl Seen {
    /// The artifact paths of everything pushed, sorted.
    fn paths(&self) -> Vec<String> {
        let mut paths: Vec<String> = self
            .pushed
            .lock()
            .unwrap()
            .iter()
            .flat_map(|set| set.files.iter().map(|file| file.path.0.clone()))
            .collect();
        paths.sort();
        paths
    }
}

fn run_build(workspace: &Built, cancel: Cancel) -> (BuildReport, Arc<Seen>) {
    let config = BuildWorkspace::open(Some(workspace.root())).expect("a usable configuration");
    let seen = Arc::new(Seen::default());
    let report = build(
        &config,
        BuildRequest {
            cancel,
            cache: Box::new(Recording(seen.clone())),
            ..BuildRequest::default()
        },
    )
    .expect("the build runs");
    (report, seen)
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
    assert_eq!(seen.finished.load(Ordering::SeqCst), 1);
    for set in seen.pushed.lock().unwrap().iter() {
        let file = &set.files[0];
        assert!(file.disk_path.is_file(), "{:?}", file.disk_path);
        assert_eq!(file.size, std::fs::metadata(&file.disk_path).unwrap().len());
        assert_eq!(
            file.blake3,
            *blake3::hash(&std::fs::read(&file.disk_path).unwrap()).as_bytes()
        );
    }
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
    assert_eq!(seen.finished.load(Ordering::SeqCst), 1, "finish runs once");
    assert!(
        workspace.exists("build/r/z1.r"),
        "the unit in flight was installed"
    );
    assert!(!workspace.exists("build/r/z2.r"));
}
