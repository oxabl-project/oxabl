//! A signal during a build: the workers stop, the store stays consistent and a
//! later build compiles what was not recorded. The worker is the fake AVM.

mod common;

use std::process::{Child, Command};
use std::time::{Duration, Instant};

use common::built::{Built, of_kind};
use common::{process_exists, started_pids};

fn signal(child: &Child, name: &str) {
    let status = Command::new("kill")
        .args([&format!("-{name}"), &child.id().to_string()])
        .status()
        .unwrap();
    assert!(status.success());
}

/// Waits until the fake has started compiling `count` units.
fn wait_for_compiles(workspace: &Built, count: usize) {
    let deadline = Instant::now() + Duration::from_secs(30);
    while workspace.compiled().len() < count && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(25));
    }
    assert!(
        workspace.compiled().len() >= count,
        "the build never compiled"
    );
}

fn slow_units(workspace: &Built, count: usize) {
    for n in 1..=count {
        workspace.write(
            &format!("src/u{n}.p"),
            &format!("/* FAKE-SLOW */\nMESSAGE \"{n}\".\n"),
        );
    }
}

fn assert_workers_gone(workspace: &Built, within: Duration) {
    let deadline = Instant::now() + within;
    let pids = started_pids(&workspace.log());
    assert!(!pids.is_empty());
    while pids.iter().any(|pid| process_exists(*pid)) && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(50));
    }
    for pid in pids {
        assert!(!process_exists(pid), "worker {pid} is still alive");
    }
}

fn cancel_with(name: &str) {
    let workspace = Built::new();
    workspace.workers("count = 2\ncancel_grace = \"30s\"");
    slow_units(&workspace, 6);
    let child = workspace.spawn(&["--events"]);
    wait_for_compiles(&workspace, 1);

    signal(&child, name);
    let signalled = Instant::now();
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(130));
    assert!(
        signalled.elapsed() < Duration::from_secs(30 + 10),
        "the cancel took {:?}",
        signalled.elapsed()
    );
    assert_workers_gone(&workspace, Duration::from_secs(1));
    let events: Vec<serde_json::Value> = String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect();
    assert_eq!(of_kind(&events, "summary")[0]["exit"], 130);
    let finished: Vec<String> = of_kind(&events, "unit-finished")
        .iter()
        .map(|event| event["path"].as_str().unwrap().to_string())
        .collect();
    assert!(!finished.is_empty() && finished.len() < 6, "{finished:?}");

    workspace.clear_log();
    let next = workspace.build_with(&["--events"]);

    next.succeeded();
    let mut again: Vec<String> = workspace
        .compiled()
        .into_iter()
        .map(|name| format!("src/{name}"))
        .collect();
    again.sort();
    let mut expected: Vec<String> = (1..=6)
        .map(|n| format!("src/u{n}.p"))
        .filter(|path| !finished.contains(path))
        .collect();
    expected.sort();
    assert_eq!(
        again, expected,
        "only the units that were not recorded compile"
    );
}

#[test]
fn sigint_stops_the_workers_and_a_later_build_compiles_what_was_not_recorded() {
    cancel_with("INT");
}

#[test]
fn sigterm_stops_the_workers_like_sigint() {
    cancel_with("TERM");
}

#[test]
fn a_busy_worker_is_stopped_when_the_grace_runs_out() {
    let workspace = Built::new();
    workspace.workers("count = 1\ncancel_grace = \"2s\"");
    workspace.write("src/hang.p", "/* FAKE-HANG */\nMESSAGE \"h\".\n");
    let child = workspace.spawn(&[]);
    wait_for_compiles(&workspace, 1);

    signal(&child, "INT");
    let signalled = Instant::now();
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(130));
    assert!(
        signalled.elapsed() < Duration::from_secs(2 + 10),
        "{:?}",
        signalled.elapsed()
    );
    assert_workers_gone(&workspace, Duration::from_secs(1));
}

#[test]
fn a_second_signal_kills_every_worker_at_once() {
    let workspace = Built::new();
    workspace.workers("count = 2\ncancel_grace = \"60s\"");
    for n in 1..=2 {
        workspace.write(&format!("src/h{n}.p"), "/* FAKE-HANG */\nMESSAGE \"h\".\n");
    }
    let child = workspace.spawn(&[]);
    wait_for_compiles(&workspace, 2);

    signal(&child, "INT");
    std::thread::sleep(Duration::from_millis(500));
    signal(&child, "INT");
    let signalled = Instant::now();
    let output = child.wait_with_output().unwrap();

    assert_eq!(output.status.code(), Some(130));
    assert!(signalled.elapsed() < Duration::from_secs(10));
    assert_workers_gone(&workspace, Duration::from_secs(2));
}

#[test]
fn the_store_is_consistent_after_a_cancel() {
    let workspace = Built::new();
    workspace.workers("count = 1\ncancel_grace = \"30s\"");
    slow_units(&workspace, 3);
    let child = workspace.spawn(&[]);
    wait_for_compiles(&workspace, 1);
    signal(&child, "INT");
    child.wait_with_output().unwrap();

    let plan = workspace.plan();

    let rebuild = plan["summary"]["rebuild"].as_u64().unwrap();
    assert!((1..=3).contains(&rebuild), "{plan}");
    for unit in plan["units"].as_array().unwrap() {
        let path = unit["path"].as_str().unwrap();
        let name = path.trim_start_matches("src/").trim_end_matches(".p");
        let has_output = workspace.exists(&format!("build/r/{name}.r"));
        assert_eq!(
            unit["action"] == "up-to-date",
            has_output,
            "{path}: a recorded unit has its output and the others have none"
        );
    }
}

#[test]
fn workers_quit_after_their_current_unit_when_oxabl_is_killed() {
    let workspace = Built::new();
    workspace.workers("count = 2");
    slow_units(&workspace, 4);
    let mut child = workspace.spawn(&[]);
    wait_for_compiles(&workspace, 2);

    child.kill().unwrap();
    child.wait().unwrap();

    assert_workers_gone(&workspace, Duration::from_secs(15));
    let lines = workspace.log_lines();
    for pid in started_pids(&workspace.log()) {
        assert!(
            lines.iter().any(|(p, what)| *p == pid && what == "exit"),
            "worker {pid} never logged exit"
        );
    }
}
