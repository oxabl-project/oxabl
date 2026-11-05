//! What the plan says from the results of earlier builds. The worker is the
//! fake AVM.

mod common;

use common::built::{Built, unit};
use rusqlite::Connection;

fn store(workspace: &Built) -> Connection {
    Connection::open(workspace.path(".oxabl/build/state.db")).unwrap()
}

fn one_unit() -> Built {
    let workspace = Built::new();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    workspace.write("src/a.p", "{inc/a.i}\nMESSAGE \"a\".\n");
    workspace.build().succeeded();
    workspace
}

fn plain_plan(workspace: &Built) -> String {
    let output = common::run({
        let mut command = workspace.command(&["--plan"]);
        command.env_remove("FAKE_AVM_LOG");
        command
    });
    assert_eq!(output.status.code(), Some(0), "{}", common::stderr(&output));
    common::stdout(&output)
}

#[test]
fn a_plan_after_a_build_has_nothing_to_rebuild_with_and_without_a_probe() {
    let workspace = one_unit();

    let plan = workspace.plan();

    assert_eq!(plan["summary"]["rebuild"], 0, "{plan}");
    assert!(plain_plan(&workspace).contains("0 to rebuild"));
}

#[test]
fn a_plan_without_a_probe_names_a_source_edit_by_generation() {
    let workspace = one_unit();
    workspace.write("src/a.p", "{inc/a.i}\nMESSAGE \"edited\".\n");

    let text = plain_plan(&workspace);

    assert!(text.contains("source-changed"), "{text}");
    assert!(text.contains("1 to rebuild"), "{text}");
}

#[test]
fn a_plan_without_a_probe_names_the_include_that_changed() {
    let workspace = one_unit();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");

    let first = plain_plan(&workspace);
    let second = plain_plan(&workspace);

    assert!(first.contains("dependency-changed: src/inc/a.i"), "{first}");
    assert_eq!(
        first.lines().next(),
        second.lines().next(),
        "the second plan forgot the change"
    );
}

#[test]
fn a_unit_the_build_never_recorded_is_new_in_the_plan() {
    let workspace = one_unit();
    workspace.write("src/b.p", "MESSAGE \"b\".\n");

    let text = plain_plan(&workspace);

    assert!(text.contains("src/b.p"), "{text}");
    assert!(text.contains("new"), "{text}");
}

#[test]
fn an_include_edit_gives_dependency_changed_in_the_keyed_plan() {
    let workspace = one_unit();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["dependency-changed"]);
    let reason = &unit(&plan, "src/a.p")["reasons"][0];
    assert_eq!(reason["paths"][0], "src/inc/a.i");
}

#[test]
fn a_new_include_that_shadows_another_gives_a_closure_change() {
    let workspace = Built::with("include_paths = [\"first\", \"second\"]", "");
    workspace.write("second/inc.i", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    workspace.write("src/a.p", "{inc.i}\n");
    workspace.reconfigure("include_paths = [\"first\", \"second\", \"src\"]", "");
    workspace.build().succeeded();
    workspace.write("first/inc.i", "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");

    let plan = workspace.plan();

    let reasons = Built::reasons(&plan, "src/a.p");
    assert!(
        reasons.contains(&"closure-changed".to_string()),
        "{reasons:?}"
    );
}

#[test]
fn a_changed_worker_program_is_named_worker_changed() {
    let workspace = one_unit();
    let connection = store(&workspace);
    let mut components: Vec<u8> = connection
        .query_row("SELECT components FROM results", [], |row| row.get(0))
        .unwrap();
    let last = components.len() - 32;
    components[last..].fill(0xAA);
    connection
        .execute(
            "UPDATE results SET key = zeroblob(32), components = ?1",
            [components],
        )
        .unwrap();
    drop(connection);

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["worker-changed"]);
}

#[test]
fn a_result_with_no_key_and_a_file_in_its_note_is_unverified() {
    let workspace = one_unit();
    store(&workspace)
        .execute(
            "UPDATE results SET key = NULL, components = x'', note = 'src/hidden.i'",
            [],
        )
        .unwrap();

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["unverified"]);
    let reason = &unit(&plan, "src/a.p")["reasons"][0];
    assert_eq!(reason["details"][0], "src/hidden.i");
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["a.p"]);
}

#[test]
fn a_deleted_output_is_output_missing_and_the_next_build_makes_it_again() {
    let workspace = one_unit();
    workspace.remove("build/r/a.r");

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["output-missing"]);
    let reason = &unit(&plan, "src/a.p")["reasons"][0];
    assert_eq!(reason["paths"][0], "build/r/a.r");
    workspace.build().succeeded();
    assert!(workspace.exists("build/r/a.r"));
}

#[test]
fn an_output_changed_outside_oxabl_is_output_changed() {
    let workspace = one_unit();
    workspace.write("build/r/a.r", "tampered\n");

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["output-changed"]);
    workspace.build().succeeded();
    assert!(workspace.read("build/r/a.r").starts_with("oxabl-fake-r"));
}

#[test]
fn an_output_touched_without_a_change_is_still_current() {
    let workspace = one_unit();
    let text = workspace.read("build/r/a.r");
    workspace.write("build/r/a.r", &text);

    let plan = workspace.plan();

    assert_eq!(plan["summary"]["rebuild"], 0, "{plan}");
}

#[test]
fn the_environment_of_the_last_build_is_kept_for_the_plan() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    let built = workspace.build_with(&["--json"]);
    built.succeeded();
    let report: serde_json::Value = serde_json::from_str(&built.stdout).unwrap();

    let output = common::run({
        let mut command = workspace.command(&["--plan", "--json"]);
        command.env_remove("FAKE_AVM_LOG");
        command
    });

    let plan: serde_json::Value =
        serde_json::from_str(&common::stdout(&output)).expect("one JSON document");
    assert_eq!(plan["environment"]["source"], "stored");
    assert_eq!(plan["environment"]["identity"], report["environment"]);
    assert!(plan["environment"]["at"].is_string());
    assert!(plain_plan(&workspace).contains("environment from build at"));
}
