//! The cache seam through the public `oxabl::build` API: a test-only cache
//! stands in for a remote service, and the local cache is wrapped where a
//! test needs a fault that a command line cannot make.

mod common;

use std::path::Path;
use std::sync::{Arc, Mutex};

use common::built::Built;
use common::cache::{entries, tree};
use common::remote_double::Remote;
use oxabl::build::cache::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, CompileKey, FetchOutcome, FetchRequest,
    Layered, LocalCache, NotStoredReason, StoreOutcome,
};
use oxabl::build::config::BuildWorkspace;
use oxabl::build::run::{BuildReport, BuildRequest, build};
use tempfile::TempDir;

fn run(workspace: &Built, cache: Box<dyn ArtifactCache>) -> BuildReport {
    let config = BuildWorkspace::open(Some(workspace.root())).expect("a usable configuration");
    build(
        &config,
        BuildRequest {
            cache,
            ..BuildRequest::default()
        },
    )
    .expect("the build runs")
}

fn four_units() -> Built {
    let workspace = Built::new();
    for name in ["a", "b", "c", "d"] {
        workspace.write(&format!("src/{name}.p"), &format!("MESSAGE \"{name}\".\n"));
    }
    workspace
}

fn local(directory: &TempDir) -> Box<dyn ArtifactCache> {
    Box::new(LocalCache::open(directory.path(), 1 << 30).expect("a usable cache directory"))
}

#[test]
fn uploads_stream_while_the_build_runs_and_the_cache_gets_paths_not_bytes() {
    let workspace = Built::new();
    workspace.worker_env(
        "FAKE_AVM_REQUEST_LOG",
        &workspace.path("requests.log").to_string_lossy(),
    );
    for name in ["a1", "a2"] {
        workspace.write(&format!("src/{name}.p"), "MESSAGE \"fast\".\n");
    }
    workspace.write("src/z.p", "/* FAKE-SLOW */\nMESSAGE \"slow\".\n");
    let remote = Remote::new();

    let report = run(&workspace, remote.cache());

    assert_eq!(report.exit, 0);
    let requests = std::fs::read_to_string(workspace.path("requests.log")).unwrap();
    let last_compile: u128 = requests
        .lines()
        .filter(|line| line.contains(" COMPILE"))
        .map(|line| line.split(' ').next().unwrap().parse::<u128>().unwrap())
        .max()
        .expect("a compile request");
    let pushes = remote.pushes();
    assert_eq!(pushes.len(), 3);
    let first = pushes.iter().map(|push| push.at_nanos).min().unwrap();
    let last = pushes.iter().map(|push| push.at_nanos).max().unwrap();
    let one_second = 1_000_000_000u128;
    assert!(
        first < last_compile + one_second,
        "the first push came after the last unit had compiled for a second"
    );
    assert!(
        last > last_compile + 1_500_000_000,
        "the last unit is pushed when it has finished, two seconds after its request"
    );
    for push in &pushes {
        for disk in &push.disk_paths {
            assert!(disk.is_absolute() && disk.starts_with(workspace.root().join("build/r")));
        }
    }
}

#[test]
fn an_output_changed_after_its_verification_is_dropped_by_the_size_and_hash_check() {
    let workspace = four_units();
    let remote = Remote::new();
    remote.on_push_read(|set: &ArtifactSetRef| {
        if set.files[0].path.0 == "r/b.r" {
            std::fs::write(&set.files[0].disk_path, b"changed after the check").unwrap();
        }
    });

    let report = run(&workspace, remote.cache());

    assert_eq!(report.exit, 0);
    assert_eq!(remote.stored(), 3);
    let dropped: Vec<_> = remote
        .outcomes()
        .into_iter()
        .filter(|(_, outcome)| *outcome == StoreOutcome::NotStored(NotStoredReason::ChangedOnDisk))
        .collect();
    assert_eq!(dropped.len(), 1);
    assert_eq!(
        report.cache.not_stored,
        [(NotStoredReason::ChangedOnDisk, 1)]
    );
}

struct Tamper(LocalCache);

struct TamperingUploader(Box<dyn ArtifactUploader>);

impl ArtifactUploader for TamperingUploader {
    fn push(&mut self, set: ArtifactSetRef) {
        if set.files[0].path.0 == "r/b.r" {
            std::fs::remove_file(&set.files[0].disk_path).unwrap();
            std::fs::write(&set.files[0].disk_path, b"changed after the check").unwrap();
        }
        self.0.push(set);
    }

    fn finish(self: Box<Self>) -> Vec<(CompileKey, StoreOutcome)> {
        self.0.finish()
    }
}

impl ArtifactCache for Tamper {
    fn label(&self) -> &str {
        "local"
    }

    fn fetch(&self, requests: Vec<FetchRequest>, stage: &Path, on: &mut dyn FnMut(FetchOutcome)) {
        self.0.fetch(requests, stage, on);
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        Box::new(TamperingUploader(self.0.uploader()))
    }
}

#[test]
fn the_local_cache_drops_a_set_whose_file_changed_after_its_verification() {
    let directory = TempDir::new().unwrap();
    let workspace = four_units();
    let cache = LocalCache::open(directory.path(), 1 << 30).unwrap();

    let report = run(&workspace, Box::new(Tamper(cache)));

    assert_eq!(report.exit, 0);
    assert_eq!(entries(directory.path()).len(), 3);
    assert_eq!(
        report.cache.not_stored,
        [(NotStoredReason::ChangedOnDisk, 1)]
    );
    let next = four_units();
    let report = run(&next, local(&directory));
    assert_eq!(report.summary.fetched, 3);
    assert_eq!(report.summary.compiled, 1, "the dropped unit compiles");
}

#[test]
fn a_fetch_that_stops_after_half_the_keys_keeps_the_installed_half_recorded_and_compiles_the_rest()
{
    let remote = Remote::new();
    let first = four_units();
    run(&first, remote.cache());
    assert_eq!(remote.stored(), 4);
    let second = four_units();
    remote.stop_fetch_after(2);

    let report = run(&second, remote.cache());

    assert_eq!(report.exit, 0);
    assert_eq!(report.summary.fetched, 2);
    assert_eq!(report.summary.compiled, 2);
    assert_eq!(second.compiled(), ["c.p", "d.p"]);
    second.clear_log();
    let again = run(&second, Box::new(oxabl::build::cache::NoCache));
    assert_eq!(again.summary.compiled, 0, "the installed half was recorded");
    assert!(second.compiled().is_empty());
}

#[test]
fn a_remote_hit_is_stored_in_the_local_cache_with_its_origin_and_later_reported_as_unverified() {
    let remote = Remote::new();
    let (near_one, near_two) = (TempDir::new().unwrap(), TempDir::new().unwrap());
    let first = four_units();
    run(
        &first,
        Box::new(Layered::new(local(&near_one), remote.cache())),
    );
    assert_eq!(remote.stored(), 4);
    assert_eq!(entries(near_one.path()).len(), 4);

    let second = four_units();
    let report = run(
        &second,
        Box::new(Layered::new(local(&near_two), remote.cache())),
    );

    assert_eq!(report.summary.fetched, 4);
    assert!(second.compiled().is_empty());
    let files = tree(near_two.path());
    let manifests: Vec<&Vec<u8>> = files
        .iter()
        .filter(|(path, _)| path.ends_with("/manifest"))
        .map(|(_, bytes)| bytes)
        .collect();
    assert_eq!(
        manifests.len(),
        4,
        "the remote hits were filled into the local cache"
    );
    for manifest in manifests {
        assert!(
            String::from_utf8_lossy(manifest).contains("\norigin remote shared double\n"),
            "{}",
            String::from_utf8_lossy(manifest)
        );
    }

    let third = four_units();
    let report = run(
        &third,
        Box::new(Layered::new(local(&near_two), Remote::new().cache())),
    );
    let json = report.to_json();
    assert_eq!(report.summary.fetched, 4);
    for unit in json["units"].as_array().unwrap() {
        assert_eq!(unit["source"], "local-cache");
        assert_eq!(unit["origin_unverified"], "remote shared double");
    }
}

#[test]
fn a_remote_hit_is_reported_as_remote_and_recorded() {
    let remote = Remote::new();
    run(&four_units(), remote.cache());
    let second = four_units();

    let report = run(&second, remote.cache());

    let json = report.to_json();
    for unit in json["units"].as_array().unwrap() {
        assert_eq!(unit["source"], "remote-cache");
    }
    assert_eq!(
        remote.fills(),
        4,
        "the cache is told to fill its nearer tier"
    );
}

#[test]
fn a_cache_that_panics_in_fetch_gives_misses_and_a_correct_build() {
    let remote = Remote::new();
    run(&four_units(), remote.cache());
    let second = four_units();
    remote.panic_in_fetch();

    let report = run(&second, remote.cache());

    assert_eq!(report.exit, 0);
    assert_eq!(report.summary.compiled, 4);
    assert_eq!(report.summary.fetched, 0);
}

#[test]
fn a_cache_that_panics_in_push_never_fails_the_build() {
    let workspace = four_units();
    let remote = Remote::new();
    remote.panic_in_push();

    let report = run(&workspace, remote.cache());

    assert_eq!(report.exit, 0);
    assert_eq!(report.summary.compiled, 4);
    assert_eq!(report.cache.offered, 0);
    assert!(
        report
            .notes
            .iter()
            .any(|note| note.contains("the cache failed")),
        "{:?}",
        report.notes
    );
    assert_eq!(remote.finished(), 1, "finish still runs once");
}

#[test]
fn a_cache_that_does_not_write_gets_no_verification_and_the_summary_says_read_only() {
    let workspace = four_units();
    let remote = Remote::read_only();

    let report = run(&workspace, remote.cache());

    assert_eq!(report.exit, 0);
    assert_eq!(remote.pushes().len(), 0);
    assert_eq!(
        report.cache.line().as_deref(),
        Some("remote-double: read-only, 0 sets offered")
    );
    assert_eq!(
        remote.finished(),
        0,
        "no uploader was started for a read-only cache"
    );
}

/// Records what the stage holds after the local cache answered, and lets a
/// test change what a cache reports.
struct Inspect {
    inner: LocalCache,
    stage_files: Arc<Mutex<Vec<String>>>,
    report_extra: bool,
}

impl ArtifactCache for Inspect {
    fn label(&self) -> &str {
        "local"
    }

    fn fetch(&self, requests: Vec<FetchRequest>, stage: &Path, on: &mut dyn FnMut(FetchOutcome)) {
        let extra = self.report_extra;
        self.inner
            .fetch(requests, stage, &mut |outcome| match outcome {
                FetchOutcome::Installed {
                    key,
                    mut files,
                    origin,
                } if extra => {
                    files.push(oxabl::build::cache::ArtifactPath("r/extra.r".to_string()));
                    on(FetchOutcome::Installed { key, files, origin });
                }
                other => on(other),
            });
        *self.stage_files.lock().unwrap() = tree(stage).into_keys().collect();
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        self.inner.uploader()
    }
}

fn units_with_xref() -> Built {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"",
    );
    for name in ["a", "b", "c", "d"] {
        workspace.write(&format!("src/{name}.p"), &format!("MESSAGE \"{name}\".\n"));
    }
    workspace
}

#[test]
fn a_miss_leaves_nothing_of_the_damaged_entry_in_the_stage() {
    let directory = TempDir::new().unwrap();
    run(&units_with_xref(), local(&directory));
    let entry = common::cache::entry_with(directory.path(), "r/b.r");
    std::fs::remove_file(entry.join("files/xref/b.p.xref.xml")).unwrap();
    std::fs::write(entry.join("files/xref/b.p.xref.xml"), b"damaged").unwrap();
    let stage_files = Arc::new(Mutex::new(Vec::new()));
    let inspect = Inspect {
        inner: LocalCache::open(directory.path(), 1 << 30).unwrap(),
        stage_files: stage_files.clone(),
        report_extra: false,
    };

    let report = run(&units_with_xref(), Box::new(inspect));

    assert_eq!(report.summary.compiled, 1);
    assert_eq!(report.summary.fetched, 3);
    let left = stage_files.lock().unwrap().clone();
    assert!(
        left.iter().all(|path| !path.contains("b.")),
        "the r-code of the damaged entry stayed in the stage: {left:?}"
    );
}

#[test]
fn a_set_with_other_files_than_the_build_expects_is_a_miss() {
    let directory = TempDir::new().unwrap();
    run(&four_units(), local(&directory));
    let inspect = Inspect {
        inner: LocalCache::open(directory.path(), 1 << 30).unwrap(),
        stage_files: Default::default(),
        report_extra: true,
    };

    let report = run(&four_units(), Box::new(inspect));

    assert_eq!(report.exit, 0);
    assert_eq!(
        report.summary.fetched, 0,
        "no set with a foreign file list is installed"
    );
    assert_eq!(report.summary.compiled, 4);
}

/// A cache that reports every key as installed and installs nothing.
struct Liar;

impl ArtifactCache for Liar {
    fn label(&self) -> &str {
        "liar"
    }

    fn fetch(&self, requests: Vec<FetchRequest>, _: &Path, on: &mut dyn FnMut(FetchOutcome)) {
        for request in requests {
            on(FetchOutcome::Installed {
                key: request.key,
                files: request.expect,
                origin: oxabl::build::cache::Origin::Local { recorded: None },
            });
        }
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        oxabl::build::cache::NoCache.uploader()
    }
}

#[test]
fn a_set_that_is_not_in_the_stage_after_all_is_a_miss_and_the_unit_compiles() {
    let workspace = four_units();

    let report = run(&workspace, Box::new(Liar));

    assert_eq!(report.exit, 0);
    assert_eq!(report.summary.fetched, 0);
    assert_eq!(report.summary.compiled, 4);
    assert!(workspace.exists("build/r/a.r"));
}
