//! A fetch of more sets than one install batch holds, through the public
//! `oxabl::build` API with the remote double: every key gets one answer, a
//! set is answered as installed only when its files are at their final
//! paths, and a cancel in the middle leaves nothing that the next build
//! trusts without a record.

mod common;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

use common::built::Built;
use common::cache::tree;
use common::remote_double::Remote;
use oxabl::build::cache::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, FetchOutcome, FetchRequest, NoCache,
    RemoteOrigin,
};
use oxabl::build::config::BuildWorkspace;
use oxabl::build::run::{BuildReport, BuildRequest, UnitStatus, build};
use oxabl::build::worker::Cancel;

/// More units than two install batches of 500 hold.
const UNITS: usize = 1100;

fn run(workspace: &Built, cache: Box<dyn ArtifactCache>, cancel: Cancel) -> BuildReport {
    let config = BuildWorkspace::open(Some(workspace.root())).expect("a usable configuration");
    build(
        &config,
        BuildRequest {
            cache,
            cancel,
            ..BuildRequest::default()
        },
    )
    .expect("the build runs")
}

fn units() -> Built {
    let workspace = Built::new();
    workspace.workers("count = 8");
    for index in 0..UNITS {
        workspace.write(
            &format!("src/u{index}.p"),
            &format!("MESSAGE \"unit {index}\".\n"),
        );
    }
    workspace
}

/// The outputs of a workspace, by path below `build`.
fn outputs(workspace: &Built) -> BTreeMap<String, Vec<u8>> {
    tree(&workspace.path("build"))
        .into_iter()
        .filter(|(path, _)| !path.rsplit('/').next().unwrap_or("").starts_with('.'))
        .collect()
}

/// The double, filled by a first build that compiled every unit, and the
/// outputs of that build.
fn filled() -> (Remote, BTreeMap<String, Vec<u8>>) {
    let remote = Remote::new();
    let first = units();
    let report = run(&first, remote.cache(), Cancel::new());
    assert_eq!(report.summary.compiled, UNITS);
    assert_eq!(remote.stored(), UNITS);
    (remote, outputs(&first))
}

/// The output of the unit at `path` (`src/u7.p` gives `r/u7.r`).
fn output_of(path: &str) -> String {
    let name = path.rsplit('/').next().unwrap().trim_end_matches(".p");
    format!("r/{name}.r")
}

#[test]
fn every_set_of_a_fetch_larger_than_a_batch_is_installed_answered_and_filled() {
    let (remote, written) = filled();
    let fresh = units();

    let report = run(&fresh, remote.cache(), Cancel::new());

    assert_eq!(report.exit, 0);
    assert_eq!(report.summary.fetched, UNITS, "every key has one answer");
    assert_eq!(report.summary.compiled, 0);
    assert!(fresh.compiled().is_empty(), "{:?}", fresh.compiled());
    assert_eq!(
        outputs(&fresh),
        written,
        "every fetched set is at its final paths"
    );
    assert_eq!(remote.fills(), UNITS, "every remote hit is filled once");
    let again = run(&fresh, Box::new(NoCache), Cancel::new());
    assert_eq!(
        again.summary.compiled, 0,
        "the store records every fetched set"
    );
}

/// The double, which cancels the build once `after` answers are out and an
/// output is at its final path, then goes on answering, as a download that
/// is still arriving.
struct CancelAfter {
    remote: Remote,
    cancel: Cancel,
    after: usize,
    answered: AtomicUsize,
    outputs: PathBuf,
}

impl ArtifactCache for CancelAfter {
    fn label(&self) -> &str {
        "cancel-after"
    }

    fn fetch(&self, requests: Vec<FetchRequest>, stage: &Path, on: &mut dyn FnMut(FetchOutcome)) {
        self.remote.fetch(requests, stage, &mut |outcome| {
            on(outcome);
            if self.answered.fetch_add(1, Ordering::SeqCst) + 1 == self.after {
                let deadline = Instant::now() + Duration::from_secs(30);
                while !tree(&self.outputs).keys().any(|path| path.ends_with(".r")) {
                    assert!(Instant::now() < deadline, "no set was ever installed");
                    std::thread::sleep(Duration::from_millis(5));
                }
                self.cancel.cancel();
            }
        });
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        NoCache.uploader()
    }

    fn fill(&self, sets: Vec<(ArtifactSetRef, RemoteOrigin)>) {
        self.remote.fill(sets);
    }
}

#[test]
fn a_cancel_in_the_middle_of_a_fetch_leaves_nothing_the_next_build_trusts_without_a_record() {
    let (remote, written) = filled();
    let fresh = units();
    let cancel = Cancel::new();
    let cache = CancelAfter {
        remote,
        cancel: cancel.clone(),
        after: 600,
        answered: AtomicUsize::new(0),
        outputs: fresh.path("build/r"),
    };

    let report = run(&fresh, Box::new(cache), cancel);

    assert_eq!(report.exit, 130);
    let fetched: Vec<String> = report
        .units
        .iter()
        .filter(|unit| matches!(unit.status, UnitStatus::Fetched(_)))
        .map(|unit| output_of(&unit.path))
        .collect();
    assert!(
        !fetched.is_empty() && fetched.len() < UNITS,
        "the cancel came in the middle of the fetch: {} fetched",
        fetched.len()
    );
    let now = outputs(&fresh);
    for output in &fetched {
        assert_eq!(
            now.get(output),
            written.get(output),
            "{output} was answered as installed"
        );
    }

    fresh.clear_log();
    let next = run(&fresh, Box::new(NoCache), Cancel::new());

    assert_eq!(next.exit, 0);
    let mut compiled: Vec<String> = fresh.compiled().iter().map(|p| output_of(p)).collect();
    compiled.sort();
    let mut unanswered: Vec<String> = written
        .keys()
        .filter(|output| !fetched.contains(output))
        .cloned()
        .collect();
    unanswered.sort();
    assert_eq!(
        compiled, unanswered,
        "exactly the units the cancelled build did not record compile again"
    );
    let now = outputs(&fresh);
    assert!(now.keys().eq(written.keys()), "every unit has its output");
    for output in &fetched {
        assert_eq!(now.get(output), written.get(output), "{output} was kept");
    }
}
