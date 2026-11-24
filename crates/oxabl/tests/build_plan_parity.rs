//! `scripts/plan-parity.sh` and the synthetic scenarios under
//! `scripts/parity/`.

mod build_support;

use std::path::PathBuf;
use std::process::Command;

use build_support::{Run, Workspace, run_of};

fn scripts() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../scripts")
}

fn parity(plan: &str, list: &str, dir: &Workspace) -> Run {
    dir.write("plan.json", plan);
    dir.write("list.txt", list);
    run_of(
        Command::new("bash")
            .arg(scripts().join("plan-parity.sh"))
            .arg(dir.path("plan.json"))
            .arg(dir.path("list.txt"))
            .output()
            .unwrap(),
    )
}

fn plan_document() -> String {
    let workspace = Workspace::with_sources("include_paths = [\"inc\", \"src\"]");
    workspace.write("inc/i.i", "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n");
    workspace.write("src/a.p", "{i.i}\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    let run = workspace.run(&["--plan", "--json"]);
    run.succeeded();
    run.stdout
}

#[test]
fn the_scenarios_on_the_synthetic_corpus_miss_nothing() {
    let output = Command::new("bash")
        .arg(scripts().join("parity/run.sh"))
        .env("OXABL", env!("CARGO_BIN_EXE_oxabl"))
        .output()
        .unwrap();
    let run = run_of(output);
    assert_eq!(run.code, 0, "{}\n{}", run.stdout, run.stderr);
    assert_eq!(run.stdout.matches("missed: 0").count(), 6, "{}", run.stdout);
    assert!(
        run.stdout.contains("dependency-changed: 1\n    src/user.p"),
        "{}",
        run.stdout
    );
}

#[test]
fn a_path_the_list_has_and_oxabl_lacks_is_missed() {
    let dir = Workspace::with_config("");
    let run = parity(&plan_document(), "src/a.p\nsrc/b.p\nsrc/gone.p\n", &dir);
    assert_eq!(run.code, 1, "{}", run.stdout);
    assert!(
        run.stdout.contains("missed: 1\n  src/gone.p"),
        "{}",
        run.stdout
    );
    assert!(run.stdout.contains("extras: 0"), "{}", run.stdout);
}

#[test]
fn extras_are_listed_by_reason_and_do_not_fail() {
    let dir = Workspace::with_config("");
    let run = parity(&plan_document(), "./src/a.p\n\n", &dir);
    assert_eq!(run.code, 0, "{}", run.stdout);
    assert!(run.stdout.contains("missed: 0"), "{}", run.stdout);
    assert!(run.stdout.contains("extras: 1"), "{}", run.stdout);
    assert!(run.stdout.contains("new: 1\n    src/b.p"), "{}", run.stdout);
}

#[test]
fn a_document_that_is_not_a_plan_is_a_usage_error() {
    let dir = Workspace::with_config("");
    assert_eq!(parity("not json", "src/a.p\n", &dir).code, 2);
    assert_eq!(parity("{\"format\": 2}", "src/a.p\n", &dir).code, 2);
    assert_eq!(parity("{}", "", &dir).code, 2);
}

#[test]
fn an_empty_plan_with_a_non_empty_list_misses_everything() {
    let dir = Workspace::with_config("");
    let run = parity("{\"format\":1,\"units\":[]}", "src/a.p\n", &dir);
    assert_eq!(run.code, 1, "{}", run.stdout);
}

#[test]
fn units_that_are_up_to_date_are_not_in_the_rebuild_set() {
    let workspace = Workspace::with_sources("");
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    workspace.plan().succeeded();
    workspace.write("src/a.p", "MESSAGE \"a2\".\n");
    let plan = workspace.run(&["--plan", "--json", "--all"]);
    plan.succeeded();
    let dir = Workspace::with_config("");
    let run = parity(&plan.stdout, "src/a.p\n", &dir);
    assert_eq!(run.code, 0, "{}", run.stdout);
    assert!(run.stdout.contains("extras: 0"), "{}", run.stdout);
}
