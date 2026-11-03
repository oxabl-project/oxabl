//! `oxabl build` compiles in dependency order, installs the outputs and
//! compiles only what a change reaches. The worker is the fake AVM.

mod common;

use common::built::{Built, of_kind, unit};
use serde_json::json;

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

#[test]
fn a_class_compiles_before_its_user_and_the_user_reads_the_new_class() {
    let workspace = class_chain();
    workspace.workers("count = 3");

    let run = workspace.build();

    run.succeeded();
    let order = workspace.compiled();
    let at = |name: &str| order.iter().position(|n| n == name).unwrap();
    assert!(at("pkg/Base.cls") < at("pkg/User.cls"), "{order:?}");
    assert!(at("pkg/User.cls") < at("main.p"), "{order:?}");
    let reads = workspace.reads("build/r/pkg/User.r");
    let base = reads
        .iter()
        .find(|(path, _)| path == "build/r/pkg/Base.r")
        .unwrap_or_else(|| panic!("User.r does not read the new Base.r: {reads:?}"));
    assert_eq!(base.1, workspace.sha256("build/r/pkg/Base.r"));
}

#[test]
fn two_classes_in_a_cycle_compile_in_one_request() {
    let workspace = Built::new();
    workspace.workers("count = 2");
    workspace.write(
        "src/pkg/A.cls",
        "/* FAKE-SLOW */\nCLASS pkg.A:\n  DEFINE VARIABLE b AS pkg.B NO-UNDO.\n  b = NEW pkg.B().\nEND CLASS.\n",
    );
    workspace.write(
        "src/pkg/B.cls",
        "CLASS pkg.B:\n  DEFINE VARIABLE a AS pkg.A NO-UNDO.\n  a = NEW pkg.A().\nEND CLASS.\n",
    );

    workspace.build().succeeded();

    let lines: Vec<_> = workspace
        .log_lines()
        .into_iter()
        .filter(|(_, what)| what.starts_with("pkg/"))
        .collect();
    assert_eq!(lines.len(), 2, "{lines:?}");
    assert_eq!(
        lines[0].0, lines[1].0,
        "the cycle went to two workers while A was slow: {lines:?}"
    );
    assert!(workspace.exists("build/r/pkg/A.r") && workspace.exists("build/r/pkg/B.r"));
}

#[test]
fn a_second_build_compiles_nothing() {
    let workspace = class_chain();
    workspace.build().succeeded();
    workspace.clear_log();

    let run = workspace.build();

    run.succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
    assert!(run.stdout.contains("compiled 0"), "{}", run.stdout);
}

#[test]
fn an_include_edit_compiles_only_its_includers() {
    let workspace = Built::new();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    workspace.write("src/one.p", "{inc/a.i}\n");
    workspace.write("src/two.p", "{inc/a.i}\n");
    workspace.write("src/three.p", "MESSAGE \"three\".\n");
    workspace.build().succeeded();
    workspace.clear_log();
    workspace.write("src/inc/a.i", "DEFINE VARIABLE y AS INTEGER NO-UNDO.\n");

    workspace.build().succeeded();

    let mut compiled = workspace.compiled();
    compiled.sort();
    assert_eq!(compiled, ["one.p", "two.p"]);
}

#[test]
fn a_source_edit_compiles_that_unit_and_the_plan_names_it() {
    let workspace = class_chain();
    workspace.build().succeeded();
    workspace.write(
        "src/main.p",
        "/* edited */\nDEFINE VARIABLE o AS pkg.User NO-UNDO.\no = NEW pkg.User().\n",
    );

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/main.p"), ["source-changed"]);
    assert_eq!(unit(&plan, "src/pkg/User.cls")["action"], "up-to-date");
}

#[test]
fn the_build_writes_the_outputs_the_configuration_asks_for() {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"\ndebug_list = \"build/dbg\"\nlisting = \"build/lst\"\n",
    );
    workspace.write("src/ar/post.p", "MESSAGE \"post\".\n");

    workspace.build().succeeded();

    for file in [
        "build/r/ar/post.r",
        "build/xref/ar/post.p.xref.xml",
        "build/dbg/ar/post.p.dbg",
        "build/lst/ar/post.p.lst",
    ] {
        assert!(workspace.exists(file), "{file} is missing");
    }
    assert!(
        workspace
            .read("build/xref/ar/post.p.xref.xml")
            .contains("src/ar/post.p")
    );
}

#[test]
fn a_text_xref_is_written_when_the_configuration_asks_for_it() {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"\nxref_format = \"text\"\n",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    workspace.build().succeeded();

    assert!(workspace.exists("build/xref/a.p.xref"));
}

#[test]
fn files_the_worker_wrote_beyond_the_outputs_are_discarded() {
    let workspace = class_chain();
    workspace.workers("count = 1");

    workspace.build().succeeded();

    // main.p names pkg.User, which the fake compiles from source on the side
    // when no r-code exists yet and saves into the stage; none of that may
    // reach the output directory except the units' own r-code.
    let mut files: Vec<String> = walk(&workspace.path("build/r"));
    files.sort();
    assert_eq!(files, ["main.r", "pkg/Base.r", "pkg/User.r"]);
}

fn walk(root: &std::path::Path) -> Vec<String> {
    fn go(dir: &std::path::Path, base: &std::path::Path, out: &mut Vec<String>) {
        for entry in std::fs::read_dir(dir).unwrap().flatten() {
            let path = entry.path();
            if path.is_dir() {
                go(&path, base, out);
            } else if !entry.file_name().to_string_lossy().starts_with(".oxabl") {
                out.push(
                    path.strip_prefix(base)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                );
            }
        }
    }
    let mut out = Vec::new();
    go(root, root, &mut out);
    out
}

#[test]
fn x_p_and_x_w_in_one_directory_compile_one_and_skip_the_other() {
    let workspace = Built::new();
    workspace.write("src/ar/x.p", "MESSAGE \"p\".\n");
    workspace.write("src/ar/x.w", "MESSAGE \"w\".\n");

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    let report: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
    assert_eq!(unit(&report, "src/ar/x.p")["status"], "ok");
    let skipped = unit(&report, "src/ar/x.w");
    assert_eq!(skipped["status"], "skipped");
    assert_eq!(skipped["skipped"], "output-collision: src/ar/x.p");
    assert_eq!(workspace.compiled(), ["ar/x.p"]);
}

#[test]
fn a_deleted_unit_loses_its_output() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/b.p", "MESSAGE \"b\".\n");
    workspace.build().succeeded();
    assert!(workspace.exists("build/r/b.r"));
    workspace.remove("src/b.p");
    workspace.clear_log();

    workspace.build().succeeded();

    assert!(!workspace.exists("build/r/b.r"));
    assert!(workspace.exists("build/r/a.r"));
    assert!(workspace.compiled().is_empty());
}

#[test]
fn events_describe_the_build_as_it_runs() {
    let workspace = class_chain();

    let run = workspace.build_with(&["--events"]);

    run.succeeded();
    let events = run.events();
    let started = &of_kind(&events, "build-started")[0];
    assert_eq!(started["format"], 1);
    assert_eq!(started["units"], 3);
    assert_eq!(started["rebuild"], 3);
    assert_eq!(started["workers"], 1);
    assert!(started["build"].as_str().is_some_and(|id| id.len() == 26));
    let finished = of_kind(&events, "unit-finished");
    assert_eq!(
        finished.len(),
        3,
        "one unit-finished for each compiled unit"
    );
    for event in &finished {
        assert_eq!(event["status"], "ok");
        assert!(event["ms"].is_u64());
    }
    assert_eq!(of_kind(&events, "unit-started").len(), 3);
    let summary = &of_kind(&events, "summary")[0];
    assert_eq!(summary["compiled"], 3);
    assert_eq!(summary["failed"], 0);
    assert_eq!(summary["exit"], 0);
    assert_eq!(events.last().unwrap()["event"], "summary");
    assert!(
        run.stderr.contains("compiled 3"),
        "the human output goes to stderr: {}",
        run.stderr
    );
}

#[test]
fn a_warning_is_an_event_and_does_not_fail_the_unit() {
    let workspace = Built::new();
    workspace.write("src/w.p", "/* FAKE-WARN */\nMESSAGE \"w\".\n");

    let run = workspace.build_with(&["--events"]);

    run.succeeded();
    let events = run.events();
    let messages = of_kind(&events, "message");
    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0]["type"], "warning");
    assert_eq!(messages[0]["path"], "src/w.p");
    assert_eq!(messages[0]["row"], 1);
    assert_eq!(of_kind(&events, "unit-finished")[0]["status"], "ok");
}

#[test]
fn the_report_lists_each_unit_with_its_result() {
    let workspace = class_chain();
    workspace.write("src/w.p", "/* FAKE-WARN */\nMESSAGE \"w\".\n");

    let run = workspace.build_with(&["--report", "report.json"]);

    run.succeeded();
    let report: serde_json::Value = serde_json::from_str(&workspace.read("report.json")).unwrap();
    assert_eq!(report["format"], 1);
    assert_eq!(report["exit"], 0);
    assert_eq!(report["summary"]["compiled"], 4);
    assert_eq!(report["summary"]["failed"], 0);
    let w = unit(&report, "src/w.p");
    assert_eq!(w["status"], "ok");
    assert_eq!(w["source"], "compiled");
    assert!(w["ms"].is_u64());
    assert_eq!(w["messages"][0]["type"], "warning");
    assert_eq!(w["messages"][0]["row"], 1);
    assert_eq!(unit(&report, "src/main.p")["reason"], "new");
    assert!(report["timing"]["setup_ms"].is_u64());
    assert!(
        report["environment"]
            .as_str()
            .is_some_and(|id| id.len() == 64)
    );
}

#[test]
fn json_prints_the_report_instead_of_the_text() {
    let workspace = class_chain();

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    let report: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
    assert_eq!(report["summary"]["compiled"], 3);
}

#[test]
fn a_build_without_an_output_directory_is_a_configuration_error() {
    let workspace = Built::new();
    let config = workspace
        .read("oxabl.toml")
        .replace("output = \"build/r\"", "");
    workspace.write("oxabl.toml", &config);
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(run.stderr.contains("[build].output"), "{}", run.stderr);
    assert!(workspace.compiled().is_empty());
}

#[test]
fn compile_options_reach_the_worker_and_change_what_the_plan_says() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.build().succeeded();
    workspace.reconfigure(
        "include_paths = [\"src\"]",
        "[build.compile]\nmin_size = true\n",
    );

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["options-changed"]);
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["a.p"]);
}

#[test]
fn an_environment_change_recompiles_everything_and_the_plan_names_the_field() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.build().succeeded();
    workspace.set_environment(&json!({ "env": { "cpcoll": "ICU-UCA" } }));

    let plan = workspace.plan();

    assert_eq!(Built::reasons(&plan, "src/a.p"), ["environment-changed"]);
    let reason = &unit(&plan, "src/a.p")["reasons"][0];
    assert_eq!(reason["details"], json!(["cpcoll"]));
    workspace.clear_log();
    workspace.build().succeeded();
    assert_eq!(workspace.compiled(), ["a.p"]);
}

#[test]
fn the_unit_that_the_longest_chain_waits_for_compiles_first() {
    let workspace = class_chain();
    workspace.write("src/a.p", "MESSAGE \"first by name\".\n");

    workspace.build().succeeded();

    let order = workspace.compiled();
    let at = |name: &str| order.iter().position(|n| n == name).unwrap();
    assert!(at("pkg/Base.cls") < at("a.p"), "{order:?}");
    assert!(at("pkg/User.cls") < at("a.p"), "{order:?}");
}

#[test]
fn the_xref_is_written_although_the_state_directory_starts_with_a_dot() {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.outputs]\nxref = \"build/xref\"\n",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    workspace.build().succeeded();
    assert!(workspace.exists("build/xref/a.p.xref.xml"));
}
