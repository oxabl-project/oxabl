//! The remote cache against a real cache server: one workspace writes, others
//! fetch, and the outputs are the same files.

#![cfg(feature = "remote-cache")]

mod common;

use std::collections::BTreeMap;
use std::os::unix::fs::symlink;
use std::time::{Duration, Instant};

use common::built::Built;
use common::cache::{entries, tree};
use common::env_with_schema;
use common::remote::world::{UNITS, World, project};
use serde_json::Value;
use tempfile::TempDir;

const OUTPUTS: &str = "[build.outputs]\nxref = \"build/xref\"\ndebug_list = \"build/dbg\"\n";

/// The files below `directory`, markers and locks left out.
fn files(workspace: &Built, directory: &str) -> BTreeMap<String, Vec<u8>> {
    tree(&workspace.path(directory))
        .into_iter()
        .filter(|(path, _)| !path.rsplit('/').next().unwrap_or("").starts_with('.'))
        .collect()
}

fn report(workspace: &Built, run: &common::built::Run) -> Value {
    run.succeeded();
    serde_json::from_str(&workspace.read("report.json")).expect("a JSON report")
}

fn sources(report: &Value) -> Vec<String> {
    report["units"]
        .as_array()
        .unwrap()
        .iter()
        .map(|unit| unit["source"].as_str().unwrap().to_string())
        .collect()
}

fn developer(world: &World, local: &TempDir, extra: &str) -> Built {
    let workspace = project(extra);
    world.attach(&workspace, &world.developer, "developer");
    workspace.cache_in(local.path());
    workspace
}

#[test]
fn ci_writes_and_a_second_workspace_with_a_fresh_local_cache_fetches_everything() {
    let world = World::start();
    let ci = project(OUTPUTS);
    world.attach(&ci, &world.ci, "ci");

    let run = ci.build_with(&["--report", "report.json"]);

    let written = report(&ci, &run);
    assert_eq!(ci.compiled().len(), UNITS, "{:?}", ci.compiled());
    assert!(
        run.stdout.contains("remote: 4 sets offered, 4 stored"),
        "{}",
        run.stdout
    );
    assert_eq!(written["cache"]["stored"], UNITS);
    let local = TempDir::new().unwrap();
    let second = developer(&world, &local, OUTPUTS);

    let run = second.build_with(&["--report", "report.json"]);

    let fetched = report(&second, &run);
    assert!(second.compiled().is_empty(), "{:?}", second.compiled());
    assert_eq!(sources(&fetched), vec!["remote-cache"; UNITS]);
    assert_eq!(files(&second, "build"), files(&ci, "build"));
    assert!(!files(&second, "build/r").is_empty());
    assert!(!files(&second, "build/xref").is_empty());
}

#[test]
fn a_remote_hit_fills_the_local_cache_and_says_where_it_came_from() {
    let mut world = World::start();
    let ci = project("");
    world.attach(&ci, &world.ci, "ci");
    ci.build().succeeded();
    let local = TempDir::new().unwrap();
    developer(&world, &local, "").build().succeeded();
    assert_eq!(entries(local.path()).len(), UNITS);
    world.server.stop();
    let third = developer(&world, &local, "");

    let run = third.build_with(&["--report", "report.json"]);

    let fetched = report(&third, &run);
    assert!(third.compiled().is_empty(), "{:?}", third.compiled());
    assert_eq!(sources(&fetched), vec!["local-cache"; UNITS]);
    let signer = format!("remote trusted {}", world.ci.member.key_text());
    for entry in fetched["units"].as_array().unwrap() {
        assert_eq!(entry["origin_unverified"], signer.as_str(), "{entry}");
    }
}

#[test]
fn a_non_default_output_layout_uploads_and_installs_at_the_paths_of_the_fetcher() {
    let world = World::start();
    let ci = project("[build.outputs]\nxref = \"reports/xref\"\ndebug_list = \"reports/dbg\"\n");
    ci.output("rcode");
    world.attach(&ci, &world.ci, "ci");
    ci.build().succeeded();
    let local = TempDir::new().unwrap();
    let second = developer(&world, &local, OUTPUTS);

    let run = second.build();

    run.succeeded();
    assert!(second.compiled().is_empty(), "{:?}", second.compiled());
    for (from, to) in [
        ("rcode", "build/r"),
        ("reports/xref", "build/xref"),
        ("reports/dbg", "build/dbg"),
    ] {
        let wanted = files(&ci, from);
        assert!(!wanted.is_empty(), "{from} is empty");
        assert_eq!(files(&second, to), wanted, "{from} against {to}");
    }
}

#[test]
fn outputs_below_a_symbolic_link_are_uploaded() {
    let world = World::start();
    let ci = project("");
    std::fs::create_dir(ci.path("real")).unwrap();
    symlink(ci.path("real"), ci.path("linked")).unwrap();
    ci.output("linked/r");
    world.attach(&ci, &world.ci, "ci");

    let run = ci.build();

    run.succeeded();
    assert!(
        run.stdout.contains("remote: 4 sets offered, 4 stored"),
        "{}",
        run.stdout
    );
    assert!(ci.exists("real/r"));
}

#[test]
fn an_output_that_becomes_a_symbolic_link_before_the_upload_is_not_stored_and_the_build_succeeds() {
    let world = World::start();
    let ci = project("");
    ci.write(
        "src/pkg/User.cls",
        "CLASS pkg.User INHERITS pkg.Base:\nFAKE-SLOW\nEND CLASS.\n",
    );
    ci.databases(&["ledger"]);
    ci.set_environment(&env_with_schema(vec![vec!["db".into(), "ledger".into()]]));
    world.attach(&ci, &world.ci, "ci");
    let child = ci.spawn(&[]);
    let output = ci.path("build/r/pkg/Base.r");
    let deadline = Instant::now() + Duration::from_secs(30);
    while !output.exists() {
        assert!(Instant::now() < deadline, "the build never wrote Base.r");
        std::thread::sleep(Duration::from_millis(5));
    }
    let target = ci.path("elsewhere.r");
    std::fs::copy(&output, &target).unwrap();
    let link = ci.path("link.r");
    symlink(&target, &link).unwrap();
    std::fs::rename(&link, &output).unwrap();

    let run = child.wait_with_output().unwrap();

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(run.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.contains("remote: 4 sets offered, 3 stored, 1 not stored (Unreadable)"),
        "{stdout}"
    );
}

#[test]
fn a_read_only_workspace_fetches_and_offers_nothing() {
    let world = World::start();
    let ci = project("");
    world.attach(&ci, &world.ci, "ci");
    ci.build().succeeded();
    let reader = project("");
    world.attach(&reader, &world.reader, "read-only");
    reader.write("src/extra.p", "MESSAGE \"extra\".\n");
    world.proxy.clear();

    let run = reader.build();

    run.succeeded();
    assert_eq!(reader.compiled(), vec!["extra.p"]);
    assert!(
        run.stdout.contains("remote: read-only, 0 sets offered"),
        "{}",
        run.stdout
    );
    let requests = world.proxy.requests();
    assert!(
        requests.iter().any(|request| request.contains("/lookup")),
        "{requests:?}"
    );
    assert!(
        !requests.iter().any(|request| request.contains("/uploads")),
        "{requests:?}"
    );
}

#[test]
fn a_read_only_remote_beside_a_writing_job_directory_offers_it_nothing_and_warns_of_nothing() {
    let world = World::start();
    let ci = project("");
    world.attach(&ci, &world.ci, "ci");
    ci.build().succeeded();
    let reader = project("[build.cache]\nlocal_job_dir = \".job-cache\"\n");
    world.attach(&reader, &world.reader, "read-only");
    reader.write("src/extra.p", "MESSAGE \"extra\".\n");
    world.proxy.clear();

    let run = reader.build();

    run.succeeded();
    assert_eq!(reader.compiled(), vec!["extra.p"]);
    assert!(
        run.stdout
            .contains("local+remote: 1 sets offered, 1 stored"),
        "{}",
        run.stdout
    );
    assert!(!run.stderr.contains("takes no uploads"), "{}", run.stderr);
    let requests = world.proxy.requests();
    assert!(
        !requests.iter().any(|request| request.contains("/uploads")),
        "{requests:?}"
    );
}

#[test]
fn a_ci_identity_that_may_not_write_gives_one_warning_and_every_set_unavailable() {
    let world = World::start();
    let workspace = project("");
    world.attach(&workspace, &world.developer, "ci");

    let run = workspace.build_with(&["--report", "report.json"]);

    let written = report(&workspace, &run);
    assert_eq!(workspace.compiled().len(), UNITS);
    assert!(
        run.stdout
            .contains("remote: 4 sets offered, 4 not stored (Unavailable)"),
        "{}",
        run.stdout
    );
    assert!(!run.stdout.contains("read-only"), "{}", run.stdout);
    assert_eq!(
        run.stderr.matches("takes no uploads").count(),
        1,
        "{}",
        run.stderr
    );
    assert_eq!(written["cache"]["writes"], true);
}
