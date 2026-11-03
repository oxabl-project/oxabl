//! Failures during a build: compile errors, tainting, retries, lost workers
//! and timeouts. The worker is the fake AVM.

mod common;

use std::time::{Duration, Instant};

use common::built::{Built, of_kind, unit};
use serde_json::Value;

fn class_chain() -> Built {
    let workspace = Built::new();
    workspace.write("src/pkg/Base.cls", "CLASS pkg.Base:\nEND CLASS.\n");
    workspace.write(
        "src/pkg/User.cls",
        "CLASS pkg.User INHERITS pkg.Base:\nEND CLASS.\n",
    );
    workspace.write(
        "src/main.p",
        "DEFINE VARIABLE o AS pkg.User NO-UNDO.\no = NEW pkg.User().\n",
    );
    workspace
}

fn report(run: &common::built::Run) -> Value {
    serde_json::from_str(&run.stdout).unwrap_or_else(|e| panic!("{e}: {}", run.stdout))
}

#[test]
fn a_failure_deletes_the_old_output_and_taints_the_users_that_compile() {
    let workspace = class_chain();
    workspace.build().succeeded();
    assert!(workspace.exists("build/r/pkg/Base.r"));
    workspace.write(
        "src/pkg/Base.cls",
        "/* FAKE-FAIL */\nCLASS pkg.Base:\nEND CLASS.\n",
    );
    workspace.clear_log();

    let run = workspace.build_with(&["--json"]);

    assert_eq!(run.code, 1, "{}\n{}", run.stdout, run.stderr);
    let report = report(&run);
    assert!(
        !workspace.exists("build/r/pkg/Base.r"),
        "the old output stays"
    );
    assert_eq!(unit(&report, "src/pkg/Base.cls")["status"], "fail");
    let user = unit(&report, "src/pkg/User.cls");
    assert_eq!(
        user["status"], "ok",
        "a user of a failed class still compiles"
    );
    assert_eq!(user["tainted"], true);
    assert_eq!(unit(&report, "src/main.p")["tainted"], true);
    assert!(workspace.exists("build/r/pkg/User.r"));
    let message = &unit(&report, "src/pkg/Base.cls")["messages"][0];
    assert_eq!(message["type"], "error");
    assert_eq!(message["number"], 1234);
    assert_eq!(message["row"], 1);
    assert_eq!(report["summary"]["failed"], 1);
    assert_eq!(report["summary"]["tainted"], 2);
    assert_eq!(report["exit"], 1);
}

#[test]
fn a_failed_unit_compiles_again_in_the_retry_pass_and_stays_failed() {
    let workspace = Built::new();
    workspace.write("src/bad.p", "/* FAKE-FAIL */\nMESSAGE \"bad\".\n");
    workspace.write("src/good.p", "MESSAGE \"good\".\n");

    let run = workspace.build_with(&["--json"]);

    assert_eq!(run.code, 1);
    let attempts = workspace
        .compiled()
        .iter()
        .filter(|n| *n == "bad.p")
        .count();
    assert_eq!(attempts, 2, "{:?}", workspace.compiled());
    assert_eq!(
        workspace
            .compiled()
            .iter()
            .filter(|n| *n == "good.p")
            .count(),
        1
    );
    assert_eq!(report(&run)["summary"]["compiled"], 1);
}

#[test]
fn a_unit_that_fails_once_and_then_compiles_counts_as_one_success() {
    let workspace = Built::new();
    workspace.write("src/flaky.p", "/* FAKE-FAIL-ONCE */\nMESSAGE \"flaky\".\n");

    let run = workspace.build_with(&["--events"]);

    run.succeeded();
    assert_eq!(workspace.compiled(), ["flaky.p", "flaky.p"]);
    assert!(workspace.exists("build/r/flaky.r"));
    let events = run.events();
    let summary = &of_kind(&events, "summary")[0];
    assert_eq!(summary["compiled"], 1);
    assert_eq!(summary["failed"], 0);
    let statuses: Vec<_> = of_kind(&events, "unit-finished")
        .iter()
        .map(|e| e["status"].as_str().unwrap().to_string())
        .collect();
    assert_eq!(statuses, ["fail", "ok"]);
}

#[test]
fn a_failed_unit_is_a_failed_result_until_it_compiles() {
    let workspace = Built::new();
    workspace.write("src/bad.p", "/* FAKE-FAIL */\nMESSAGE \"bad\".\n");
    workspace.build();

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/bad.p"), ["failed"]);
    workspace.write("src/bad.p", "MESSAGE \"fixed\".\n");
    workspace.build().succeeded();
    assert!(workspace.exists("build/r/bad.r"));
    workspace.clear_log();
    workspace.build().succeeded();
    assert!(workspace.compiled().is_empty());
}

#[test]
fn a_worker_that_crashes_once_is_replaced_and_the_unit_compiles() {
    let workspace = Built::new();
    workspace.write("src/crashy.p", "/* FAKE-CRASH-ONCE */\nMESSAGE \"c\".\n");
    workspace.write("src/fine.p", "MESSAGE \"f\".\n");

    let run = workspace.build_with(&["--events"]);

    run.succeeded();
    let events = run.events();
    let lost = of_kind(&events, "worker-lost");
    assert_eq!(lost.len(), 1, "{events:?}");
    assert_eq!(lost[0]["unit"], "src/crashy.p");
    assert_eq!(lost[0]["reason"], "exited");
    assert!(lost[0]["worker"].is_u64());
    assert_eq!(
        workspace
            .compiled()
            .iter()
            .filter(|n| *n == "crashy.p")
            .count(),
        2
    );
    assert!(workspace.exists("build/r/crashy.r") && workspace.exists("build/r/fine.r"));
}

#[test]
fn a_unit_that_loses_its_worker_twice_fails_and_the_others_compile() {
    let workspace = Built::new();
    workspace.write("src/crashy.p", "/* FAKE-CRASH */\nMESSAGE \"c\".\n");
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/z.p", "MESSAGE \"z\".\n");

    let run = workspace.build_with(&["--events"]);

    assert_eq!(run.code, 1, "{}", run.stderr);
    let events = run.events();
    assert_eq!(of_kind(&events, "worker-lost").len(), 2);
    let crashed: Vec<_> = of_kind(&events, "unit-finished")
        .into_iter()
        .filter(|e| e["path"] == "src/crashy.p")
        .collect();
    assert_eq!(crashed.len(), 1, "a lost-twice unit is not tried again");
    assert_eq!(crashed[0]["status"], "fail");
    assert!(workspace.exists("build/r/a.r") && workspace.exists("build/r/z.r"));
    assert!(!workspace.exists("build/r/crashy.r"));
    assert!(
        run.stderr
            .contains("worker lost twice while compiling this unit"),
        "{}",
        run.stderr
    );
}

#[test]
fn the_report_names_the_unit_that_lost_its_worker_twice() {
    let workspace = Built::new();
    workspace.write("src/crashy.p", "/* FAKE-CRASH */\nMESSAGE \"c\".\n");

    let run = workspace.build_with(&["--json"]);

    assert_eq!(run.code, 1);
    let crashy = unit(&report(&run), "src/crashy.p").clone();
    assert_eq!(crashy["status"], "fail");
    assert_eq!(
        crashy["note"],
        "worker lost twice while compiling this unit"
    );
}

#[test]
fn a_unit_that_hangs_past_the_unit_timeout_fails_and_the_build_ends() {
    let workspace = Built::new();
    workspace.workers("count = 1\nunit_timeout = \"2s\"");
    workspace.write("src/hang.p", "/* FAKE-HANG */\nMESSAGE \"h\".\n");
    workspace.write("src/fine.p", "MESSAGE \"f\".\n");
    let started = Instant::now();

    let run = workspace.build_with(&["--events"]);

    assert!(
        started.elapsed() < Duration::from_secs(60),
        "the build did not end"
    );
    assert_eq!(run.code, 1, "{}", run.stderr);
    let events = run.events();
    let lost = of_kind(&events, "worker-lost");
    assert_eq!(lost.len(), 2);
    assert_eq!(lost[0]["reason"], "timeout");
    assert_eq!(lost[0]["unit"], "src/hang.p");
    assert!(workspace.exists("build/r/fine.r"));
}

#[test]
fn fail_fast_stops_dispatch_at_the_first_failure() {
    let workspace = Built::new();
    workspace.write("src/a.p", "/* FAKE-FAIL */\nMESSAGE \"a\".\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    workspace.write("src/c.p", "MESSAGE \"c\".\n");

    let run = workspace.build_with(&["--fail-fast", "--json"]);

    assert_eq!(run.code, 1, "{}", run.stderr);
    assert_eq!(
        workspace.compiled(),
        ["a.p"],
        "dispatch went on after the failure"
    );
    let report = report(&run);
    assert_eq!(unit(&report, "src/a.p")["status"], "fail");
    assert_eq!(unit(&report, "src/b.p")["status"], "not-run");
    assert_eq!(unit(&report, "src/c.p")["status"], "not-run");
    assert_eq!(report["summary"]["not_run"], 2);
}

#[test]
fn without_fail_fast_every_other_unit_compiles() {
    let workspace = Built::new();
    workspace.write("src/a.p", "/* FAKE-FAIL */\nMESSAGE \"a\".\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    workspace.write("src/c.p", "MESSAGE \"c\".\n");

    let run = workspace.build();

    assert_eq!(run.code, 1);
    assert!(workspace.exists("build/r/b.r") && workspace.exists("build/r/c.r"));
    assert!(run.stdout.contains("failed    src/a.p"), "{}", run.stdout);
    assert!(run.stdout.contains("FAKE-FAIL"), "{}", run.stdout);
}

#[test]
fn a_compile_that_reports_success_without_its_outputs_is_a_failed_unit() {
    let workspace = Built::new();
    workspace.write("src/ghost.p", "/* FAKE-NO-OUTPUT */\nMESSAGE \"g\".\n");
    workspace.write("src/fine.p", "MESSAGE \"f\".\n");

    let run = workspace.build_with(&["--json"]);

    assert_eq!(run.code, 1, "{}", run.stderr);
    let ghost = unit(&report(&run), "src/ghost.p").clone();
    assert_eq!(ghost["status"], "fail");
    assert!(
        ghost["note"]
            .as_str()
            .unwrap()
            .contains("produced no regular file r/ghost.r"),
        "{ghost}"
    );
    assert!(!workspace.exists("build/r/ghost.r"));
    assert!(workspace.exists("build/r/fine.r"));
}

#[test]
fn a_unit_whose_name_does_not_fit_the_worker_code_page_fails_and_the_others_compile() {
    let workspace = Built::new();
    workspace.set_environment(&serde_json::json!({
        "env": { "cpinternal": "ISO8859-1", "cpstream": "ISO8859-1" }
    }));
    workspace.write("src/caf\u{e9}.p", "MESSAGE \"accent\".\n");
    workspace.write("src/plain.p", "MESSAGE \"plain\".\n");

    let run = workspace.build_with(&["--json"]);

    assert_eq!(run.code, 1, "{}", run.stderr);
    let accent = unit(&report(&run), "src/caf\u{e9}.p").clone();
    assert_eq!(accent["status"], "fail");
    assert!(
        accent["note"]
            .as_str()
            .unwrap()
            .starts_with("path-encoding:"),
        "{accent}"
    );
    assert!(workspace.exists("build/r/plain.r"));
    assert_eq!(
        workspace.compiled(),
        ["plain.p"],
        "no worker was lost over it"
    );
}
