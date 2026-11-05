//! A remote cache that misbehaves costs time, never correctness: every key
//! the cache cannot give is compiled, and the build exits 0 with one warning.

#![cfg(feature = "remote-cache")]

mod common;

use std::process::Command;
use std::time::{Duration, Instant};

use common::built::Built;
use common::cache::tree;
use common::remote::proxy::Mode;
use common::remote::world::{UNITS, World, project};
use tempfile::TempDir;

const IDLE: &str = "[build.cache]\nidle_timeout = \"2s\"\n";

/// A world whose cache holds the units of `project`, written by CI.
fn filled() -> World {
    let world = World::start();
    let ci = project("");
    world.attach(&ci, &world.ci, "ci");
    ci.build().succeeded();
    world
}

fn fetcher(world: &World, extra: &str) -> (Built, TempDir) {
    let local = TempDir::new().unwrap();
    let workspace = project(extra);
    world.attach(&workspace, &world.developer, "developer");
    workspace.cache_in(local.path());
    (workspace, local)
}

/// Every output of a build that compiled for itself names the files it read
/// with the hashes that the files have now.
fn assert_correct(workspace: &Built) {
    let rcode = tree(&workspace.path("build/r"));
    let wanted = ["ar/post.r", "main.r", "pkg/Base.r", "pkg/User.r"];
    for name in wanted {
        assert!(
            rcode.contains_key(name),
            "{name} is missing: {:?}",
            rcode.keys()
        );
        for (path, sha) in workspace.reads(&format!("build/r/{name}")) {
            assert_eq!(workspace.sha256(&path), sha, "{name} read {path}");
        }
    }
}

fn warnings(stderr: &str, about: &str) -> usize {
    stderr
        .lines()
        .filter(|line| line.starts_with("warning:") && line.contains(about))
        .count()
}

#[test]
fn garbage_instead_of_the_packs_gives_misses_a_correct_build_and_one_warning() {
    let world = filled();
    world.proxy.set(Mode::Garbage {
        marker: "/objects/",
    });
    let (workspace, _local) = fetcher(&world, "");

    let run = workspace.build();

    run.succeeded();
    assert_eq!(
        workspace.compiled().len(),
        UNITS,
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(warnings(&run.stderr, "remote cache"), 1, "{}", run.stderr);
    assert_correct(&workspace);
}

#[test]
fn garbage_instead_of_the_lookup_gives_misses_a_correct_build_and_one_warning() {
    let world = filled();
    world.proxy.set(Mode::Garbage { marker: "/lookup" });
    let (workspace, _local) = fetcher(&world, "");

    let run = workspace.build();

    run.succeeded();
    assert_eq!(
        workspace.compiled().len(),
        UNITS,
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(warnings(&run.stderr, "remote cache"), 1, "{}", run.stderr);
    assert_correct(&workspace);
}

#[test]
fn a_server_that_is_down_gives_a_correct_build_and_one_warning() {
    let mut world = filled();
    world.server.stop();
    let (workspace, _local) = fetcher(&world, "");

    let run = workspace.build();

    run.succeeded();
    assert_eq!(
        workspace.compiled().len(),
        UNITS,
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(warnings(&run.stderr, "remote cache"), 1, "{}", run.stderr);
    assert!(
        run.stderr.contains("the remote cache is not used"),
        "{}",
        run.stderr
    );
    assert_correct(&workspace);
}

#[test]
fn a_server_that_sends_slowly_without_a_long_gap_is_not_cut_off() {
    let world = filled();
    world.proxy.set(Mode::Slow {
        marker: "/objects/",
        piece: 16,
        gap: Duration::from_millis(60),
    });
    let (workspace, _local) = fetcher(&world, IDLE);
    let started = Instant::now();

    let run = workspace.build();

    let took = started.elapsed();
    run.succeeded();
    assert!(
        took > Duration::from_secs(3),
        "the transfer took {took:?}, not clearly longer than the idle timeout"
    );
    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(warnings(&run.stderr, "remote cache"), 0, "{}", run.stderr);
}

#[test]
fn a_server_that_stops_sending_leaves_misses_after_the_idle_timeout_and_a_correct_build() {
    let world = filled();
    world.proxy.set(Mode::Stall {
        marker: "/objects/",
        sent: 64,
    });
    let (workspace, _local) = fetcher(&world, IDLE);
    let started = Instant::now();

    let run = workspace.build();

    let took = started.elapsed();
    run.succeeded();
    assert!(took >= Duration::from_secs(2), "{took:?}");
    assert!(took < Duration::from_secs(25), "{took:?}");
    assert_eq!(
        workspace.compiled().len(),
        UNITS,
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(warnings(&run.stderr, "remote cache"), 1, "{}", run.stderr);
    assert_correct(&workspace);
}

#[test]
fn a_cancel_does_not_wait_for_a_remote_fetch_that_is_still_sending() {
    let world = filled();
    world.proxy.set(Mode::Slow {
        marker: "/objects/",
        piece: 8,
        gap: Duration::from_millis(100),
    });
    let (workspace, _local) = fetcher(&world, "");
    world.proxy.clear();
    let child = workspace.spawn(&[]);
    let deadline = Instant::now() + Duration::from_secs(30);
    while !world
        .proxy
        .requests()
        .iter()
        .any(|r| r.contains("/objects/"))
    {
        assert!(Instant::now() < deadline, "the build never fetched a pack");
        std::thread::sleep(Duration::from_millis(20));
    }
    std::thread::sleep(Duration::from_millis(300));

    let status = Command::new("kill")
        .args(["-INT", &child.id().to_string()])
        .status()
        .unwrap();
    assert!(status.success());
    let signalled = Instant::now();
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(130));
    assert!(
        signalled.elapsed() < Duration::from_secs(8),
        "the cancel took {:?}",
        signalled.elapsed()
    );
}
