//! `oxabl build --yield`: a build that steps aside for a newer one. The worker
//! is the fake AVM.

mod common;

use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use common::built::Built;

const STOPPING: &str = "that yields to a newer one";

fn slow_workspace() -> Built {
    let workspace = Built::new();
    workspace.workers("count = 1");
    for n in 1..=3 {
        workspace.write(
            &format!("src/u{n}.p"),
            &format!("/* FAKE-SLOW */\nMESSAGE \"{n}\".\n"),
        );
    }
    workspace
}

/// Waits until the fake has started compiling a unit.
fn wait_for_compile(workspace: &Built) {
    let deadline = Instant::now() + Duration::from_secs(30);
    while workspace.compiled().is_empty() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(25));
    }
    assert!(!workspace.compiled().is_empty(), "the build never compiled");
}

fn exit_code(child: Child) -> (Option<i32>, String) {
    let output = child.wait_with_output().unwrap();
    (
        output.status.code(),
        String::from_utf8_lossy(&output.stdout).into_owned(),
    )
}

#[test]
fn a_plain_build_stops_a_yielding_one_and_then_succeeds() {
    let workspace = slow_workspace();
    let yielding = workspace.spawn(&["--events", "--yield"]);
    wait_for_compile(&workspace);

    let plain = workspace.build();

    plain.succeeded();
    assert!(plain.stderr.contains(STOPPING), "{}", plain.stderr);
    let (code, stdout) = exit_code(yielding);
    assert_eq!(code, Some(130));
    let summary = stdout
        .lines()
        .map(|line| serde_json::from_str::<serde_json::Value>(line).unwrap())
        .find(|event| event["event"] == "summary")
        .expect("a summary event");
    assert_eq!(summary["exit"], 130);
}

#[test]
fn a_yielding_build_does_not_stop_a_plain_one() {
    let workspace = slow_workspace();
    let plain = workspace.spawn(&[]);
    wait_for_compile(&workspace);

    let newcomer = workspace.build_with(&["--yield"]);

    assert_eq!(newcomer.code, 3, "{}", newcomer.stderr);
    assert!(!newcomer.stderr.contains(STOPPING), "{}", newcomer.stderr);
    assert_eq!(exit_code(plain).0, Some(0));
}

#[test]
fn a_yielding_build_stops_an_older_yielding_one() {
    let workspace = slow_workspace();
    let older = workspace.spawn(&["--events", "--yield"]);
    wait_for_compile(&workspace);

    let newer = workspace.build_with(&["--yield"]);

    newer.succeeded();
    assert!(newer.stderr.contains(STOPPING), "{}", newer.stderr);
    assert_eq!(exit_code(older).0, Some(130));
}

#[test]
fn a_lock_naming_another_process_is_not_signalled() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    let mut bystander = Command::new("sleep")
        .arg("60")
        .stdin(Stdio::null())
        .spawn()
        .unwrap();
    std::fs::create_dir_all(workspace.path(".oxabl/build")).unwrap();
    let lock = std::fs::File::create(workspace.path(".oxabl/build/build.lock")).unwrap();
    lock.lock().unwrap();
    std::fs::write(
        workspace.path(".oxabl/build/build.lock"),
        format!("{} yield", bystander.id()),
    )
    .unwrap();

    let refused = workspace.build();

    let alive = bystander.try_wait().unwrap().is_none();
    let _ = bystander.kill();
    let _ = bystander.wait();
    assert_eq!(refused.code, 3, "{}", refused.stderr);
    assert!(!refused.stderr.contains(STOPPING), "{}", refused.stderr);
    assert!(alive, "the bystander was signalled");
}
