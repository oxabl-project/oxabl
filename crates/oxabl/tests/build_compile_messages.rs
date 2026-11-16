//! Compiler messages that carry no position, and the count that closes each
//! unit's messages. The worker is the fake AVM.

mod common;

use common::built::{Built, of_kind, unit};
use serde_json::Value;

const NO_POSITION: &str = "/* FAKE-WARN-NOPOS */\nMESSAGE \"w\".\n";

fn report(run: &common::built::Run) -> Value {
    serde_json::from_str(&run.stdout).unwrap_or_else(|e| panic!("{e}: {}", run.stdout))
}

#[test]
fn a_warning_without_a_position_is_an_event_of_its_unit() {
    let workspace = Built::new();
    workspace.write("src/w.p", NO_POSITION);

    let run = workspace.build_with(&["--events"]);

    run.succeeded();
    let events = run.events();
    let messages = of_kind(&events, "message");
    assert_eq!(messages.len(), 1, "{events:?}");
    assert_eq!(messages[0]["type"], "warning");
    assert_eq!(messages[0]["number"], 214);
    assert_eq!(messages[0]["path"], "src/w.p");
    assert_eq!(
        messages[0]["file"], "src/w.p",
        "the file is the unit, not empty"
    );
    assert_eq!(messages[0]["row"], 0);
    assert_eq!(messages[0]["column"], 0);
    assert_eq!(of_kind(&events, "unit-finished")[0]["status"], "ok");
}

#[test]
fn a_warning_without_a_position_is_in_the_json_report_and_the_unit_is_ok() {
    let workspace = Built::new();
    workspace.write("src/w.p", NO_POSITION);

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    let w = unit(&report(&run), "src/w.p").clone();
    assert_eq!(w["status"], "ok");
    assert_eq!(w["messages"][0]["type"], "warning");
    assert_eq!(w["messages"][0]["number"], 214);
    assert_eq!(w["messages"][0]["file"], "src/w.p");
    assert_eq!(w["messages"][0]["row"], 0);
    assert_eq!(w["messages"][0]["column"], 0);
}

#[test]
fn the_text_report_prints_the_warnings_of_a_compiled_unit_without_a_position() {
    let workspace = Built::new();
    workspace.write("src/w.p", NO_POSITION);
    workspace.write("src/ok.p", "MESSAGE \"fine\".\n");
    workspace.write("src/p.p", "/* FAKE-WARN */\nMESSAGE \"p\".\n");

    let run = workspace.build();

    run.succeeded();
    assert!(run.stdout.contains("compiled  src/w.p\n"), "{}", run.stdout);
    assert!(
        run.stdout
            .contains("  warning 214 src/w.p: FAKE-WARN-NOPOS\n"),
        "{}",
        run.stdout
    );
    assert!(
        run.stdout
            .contains("  warning 2345 src/p.p:1:1: FAKE-WARN\n"),
        "a warning with a position keeps it: {}",
        run.stdout
    );
    assert!(!run.stdout.contains(":0:0"), "{}", run.stdout);
    assert!(!run.stdout.contains("src/ok.p"), "{}", run.stdout);
}

#[test]
fn a_unit_whose_done_counts_a_message_that_never_came_is_a_protocol_loss() {
    let workspace = Built::new();
    workspace.worker_env("FAKE_AVM_DROP_MSG", "1");
    workspace.write("src/w.p", "/* FAKE-WARN */\nMESSAGE \"w\".\n");
    workspace.write("src/fine.p", "MESSAGE \"f\".\n");

    let run = workspace.build_with(&["--events"]);

    assert_eq!(run.code, 1, "{}", run.stderr);
    let events = run.events();
    let lost = of_kind(&events, "worker-lost");
    assert_eq!(lost.len(), 2, "lost once, retried, lost twice: {events:?}");
    assert_eq!(lost[0]["reason"], "protocol");
    assert_eq!(lost[0]["unit"], "src/w.p");
    let finished: Vec<_> = of_kind(&events, "unit-finished")
        .into_iter()
        .filter(|e| e["path"] == "src/w.p")
        .collect();
    assert_eq!(finished.len(), 1);
    assert_eq!(finished[0]["status"], "fail");
    assert!(workspace.exists("build/r/fine.r"));
    assert!(!workspace.exists("build/r/w.r"));
}
