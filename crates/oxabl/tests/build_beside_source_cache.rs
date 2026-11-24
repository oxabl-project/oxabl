//! R-code beside the source and the caches: a cache entry holds the r-code by
//! the unit path, a fetch installs it beside the source of the workspace that
//! fetches, and the keys of the two modes never meet. The oracle is the fake
//! runtime with a runtime PROPATH of the source directories.

mod common;

use std::collections::BTreeMap;
use std::path::Path;
use std::thread;
use std::time::Duration;

use common::built::{Built, of_kind};
use common::cache::entries;
use common::fake_runtime;
use common::remote_double::Remote;
use common::runtime_cases::Start;
use oxabl::build::cache::{
    ArtifactCache, ArtifactSetRef, ArtifactUploader, FetchOutcome, FetchRequest, NoCache,
    RemoteOrigin,
};
use oxabl::build::config::BuildWorkspace;
use oxabl::build::run::{BuildReport, BuildRequest, build};
use serde_json::Value;
use tempfile::TempDir;

/// A program, a program with an include, a class and a user of the class,
/// each printing `<what> <version>`.
fn write_sources(workspace: &Built, version: &str) {
    workspace.write(
        "src/hello.p",
        &format!("/* FAKE-SAY program {version} */\n"),
    );
    workspace.write("src/withinc.p", "{msg.i}\n");
    workspace.write("src/msg.i", &format!("/* FAKE-SAY include {version} */\n"));
    workspace.write(
        "src/pkg/Greet.cls",
        &format!("CLASS pkg.Greet:\n/* FAKE-SAY class {version} */\nEND CLASS.\n"),
    );
    workspace.write(
        "src/greet.p",
        "DEFINE VARIABLE o AS pkg.Greet NO-UNDO.\no = NEW pkg.Greet().\n",
    );
}

const PROGRAMS: [&str; 3] = ["hello.p", "withinc.p", "greet.p"];

fn beside() -> Built {
    let workspace = Built::new();
    workspace.beside_source();
    workspace
}

/// What each program prints on the runtime PROPATH `src`, with the file the
/// runtime loaded for it.
fn runs(workspace: &Built) -> Vec<(Vec<String>, String)> {
    PROGRAMS
        .iter()
        .map(|program| {
            let ran = fake_runtime::run(workspace.root(), &["src"], program);
            assert_eq!(ran.code, 0, "{program}: {}{}", ran.stdout, ran.stderr);
            let (file, how) = ran.loaded(program).unwrap();
            (ran.said(), format!("{file} {how}"))
        })
        .collect()
}

/// Every artifact path that the entries of a local cache hold.
fn artifacts(cache: &Path) -> Vec<String> {
    let mut paths: Vec<String> = entries(cache)
        .iter()
        .flat_map(|entry| common::cache::entry_files(entry))
        .map(|(path, _)| path)
        .collect();
    paths.sort();
    paths
}

/// A second copy of the project fetches every unit into its own source
/// directories, over the r-code of the project's own tool, and its runtime
/// runs the fetched code.
#[test]
fn a_second_workspace_fetches_into_place_and_runs_what_the_first_built() {
    let cache = TempDir::new().unwrap();
    let first = beside();
    first.cache_in(cache.path());
    write_sources(&first, "v2");
    first.build().succeeded();
    assert_eq!(first.compiled().len(), 4, "{:?}", first.compiled());
    assert_eq!(
        artifacts(cache.path()),
        [
            "r/src/greet.r",
            "r/src/hello.r",
            "r/src/pkg/Greet.r",
            "r/src/withinc.r"
        ],
        "an entry holds the r-code by the unit path"
    );

    let second = beside();
    write_sources(&second, "v1");
    for name in ["pkg/Greet.cls", "hello.p", "withinc.p", "greet.p"] {
        fake_runtime::compile(second.root(), &["src"], name, "src");
    }
    write_sources(&second, "v2");
    second.cache_in(cache.path());
    let run = second.build_with(&["--events"]);

    run.succeeded();
    let events = run.events();
    assert_eq!(of_kind(&events, "unit-fetched").len(), 4, "{events:?}");
    assert!(second.compiled().is_empty(), "{:?}", second.compiled());
    assert_eq!(
        runs(&second),
        [
            (
                vec!["program v2".to_string()],
                "src/hello.r rcode".to_string()
            ),
            (
                vec!["include v2".to_string()],
                "src/withinc.r rcode".to_string()
            ),
            (
                vec!["class v2".to_string()],
                "src/greet.r rcode".to_string()
            ),
        ]
    );
    let ran = fake_runtime::run(second.root(), &["src"], "greet.p");
    assert_eq!(
        ran.loaded("pkg.Greet"),
        Some(("src/pkg/Greet.r".to_string(), "rcode".to_string())),
        "{}",
        ran.stdout
    );
    assert!(!second.exists("build"), "no output directory");
    assert!(!second.exists(".oxabl/r"), "no output directory");

    second.clear_log();
    let run = second.build_with(&["--events"]);
    run.succeeded();
    assert!(second.compiled().is_empty(), "{:?}", second.compiled());
    assert!(
        of_kind(&run.events(), "unit-fetched").is_empty(),
        "the store records each fetched set"
    );
}

/// Two classes of one name below two package roots: each entry holds its own
/// r-code, and a fetch puts each beside its own source.
#[test]
fn classes_of_one_name_in_two_roots_are_fetched_each_to_its_own_place() {
    let cache = TempDir::new().unwrap();
    let project = || {
        let workspace = Built::with("root_relative_include_paths = [\".\"]", "");
        workspace.beside_source();
        workspace.cache_in(cache.path());
        for root in ["a", "b"] {
            workspace.write(
                &format!("src/{root}/pkg/Foo.cls"),
                &format!("CLASS pkg.Foo:\n/* FAKE-SAY {root} */\nEND CLASS.\n"),
            );
        }
        workspace
    };
    project().build().succeeded();

    let second = project();
    let run = second.build_with(&["--events"]);

    run.succeeded();
    for root in ["a", "b"] {
        let ran = fake_runtime::start(
            second.root(),
            &[&format!("src/{root}")],
            Start::Class("pkg.Foo"),
        );
        assert_eq!(ran.said(), [root], "{}{}", ran.stdout, ran.stderr);
    }
    assert_eq!(
        of_kind(&run.events(), "unit-fetched").len(),
        2,
        "{}",
        run.stdout
    );
}

/// The remote double, which answers each fetch only after `delay`.
struct Slow {
    remote: Remote,
    delay: Duration,
}

impl ArtifactCache for Slow {
    fn label(&self) -> &str {
        "slow"
    }

    fn fetch(&self, requests: Vec<FetchRequest>, stage: &Path, on: &mut dyn FnMut(FetchOutcome)) {
        thread::sleep(self.delay);
        self.remote.fetch(requests, stage, on);
    }

    fn uploader(&self) -> Box<dyn ArtifactUploader> {
        NoCache.uploader()
    }

    fn fill(&self, sets: Vec<(ArtifactSetRef, RemoteOrigin)>) {
        self.remote.fill(sets);
    }
}

fn run_with(workspace: &Built, cache: Box<dyn ArtifactCache>) -> BuildReport {
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

/// Text that the analysis cannot parse: the unit has no key until its
/// compiler reported what it read.
const UNREADABLE: &str = "§ ¤ 0x9f ((( %%% .\n";

/// A class with a key and a user that has none: the user compiles only after
/// the fetch put the class's r-code beside its source, and binds that r-code,
/// not the r-code of the project's own tool and not the class source.
#[test]
fn a_user_with_no_key_compiles_after_its_class_is_fetched_into_place() {
    let project = |say: &str| {
        let workspace = beside();
        workspace.write(
            "src/pkg/Used.cls",
            &format!("CLASS pkg.Used:\n/* FAKE-SAY {say} */\nEND CLASS.\n"),
        );
        workspace.write("src/enc.p", &format!("{UNREADABLE}/* NEW pkg.Used() */\n"));
        workspace
    };
    let remote = Remote::new();
    let first = project("used");
    let report = run_with(&first, remote.cache());
    assert_eq!(report.exit, 0, "{:?}", report.notes);
    assert_eq!(remote.stored(), 1, "only the class has a key");

    let second = project("old");
    fake_runtime::compile(second.root(), &["src"], "pkg/Used.cls", "src");
    let stale = second.sha256("src/pkg/Used.r");
    second.write(
        "src/pkg/Used.cls",
        "CLASS pkg.Used:\n/* FAKE-SAY used */\nEND CLASS.\n",
    );
    let report = run_with(
        &second,
        Box::new(Slow {
            remote: remote.clone(),
            delay: Duration::from_millis(1_500),
        }),
    );

    assert_eq!(report.exit, 0, "{:?}", report.notes);
    assert_eq!(
        (report.summary.fetched, report.summary.compiled),
        (1, 1),
        "{:?}",
        report.summary
    );
    let fetched = second.sha256("src/pkg/Used.r");
    assert_ne!(fetched, stale);
    let bound: BTreeMap<String, String> = second.reads("src/enc.r").into_iter().collect();
    assert_eq!(
        bound.get("src/pkg/Used.r"),
        Some(&fetched),
        "the user binds the fetched r-code of its class: {bound:?}"
    );
    let ran = fake_runtime::run(second.root(), &["src"], "enc.p");
    assert_eq!(ran.code, 0, "{}{}", ran.stdout, ran.stderr);
    assert_eq!(ran.said(), ["used"], "{}", ran.stdout);
}

/// The keys of every unit of a project, by path.
fn keys(workspace: &Built) -> BTreeMap<String, String> {
    workspace.plan()["units"]
        .as_array()
        .unwrap()
        .iter()
        .map(|unit| {
            (
                unit["path"].as_str().unwrap().to_string(),
                unit["key"]
                    .as_str()
                    .unwrap_or_else(|| panic!("no key: {unit}"))
                    .to_string(),
            )
        })
        .collect()
}

/// The keys that oxabl 5.6.0 printed for this project in `directory` mode,
/// before the r-code could go beside the source: that mode's keys, and so
/// every cache entry made with them, stay valid.
#[test]
fn keys_of_directory_mode_do_not_move() {
    let workspace = Built::new();
    write_sources(&workspace, "v1");

    let keys = keys(&workspace);

    let released: BTreeMap<String, String> = [
        (
            "src/greet.p",
            "f4183898842693a8d0a3f7515a412ecc0a448b780b641499f70ff3fd634a573f",
        ),
        (
            "src/hello.p",
            "15163eb3b840babc6134677bf2dcf0edbe5eb4fc2d027d9bc16d226eb66478fd",
        ),
        (
            "src/pkg/Greet.cls",
            "3c01b3f3260acd11359e8ecf23ae977962b887393f67b52d49164487cebf618e",
        ),
        (
            "src/withinc.p",
            "f2ecd6d21795f623f8784897d66645fd43bebdb38fa71b4b0cb5f71d3424b285",
        ),
    ]
    .into_iter()
    .map(|(path, key)| (path.to_string(), key.to_string()))
    .collect();
    assert_eq!(keys, released);
}

/// The unit PROPATH of the two modes differs, so no key of one mode is a
/// key of the other: a cache never gives r-code made for `$OUT` to a
/// workspace whose r-code is beside the source.
#[test]
fn keys_differ_between_the_modes() {
    let workspace = Built::new();
    write_sources(&workspace, "v1");
    let directory = keys(&workspace);

    workspace.beside_source();
    let in_place = keys(&workspace);

    assert_eq!(
        directory.keys().collect::<Vec<_>>(),
        in_place.keys().collect::<Vec<_>>()
    );
    for (path, key) in &directory {
        assert_ne!(&in_place[path], key, "{path}");
    }
    let plan: Value = workspace.plan();
    assert!(!plan["propaths"].to_string().contains("$OUT"), "{plan}");
}
