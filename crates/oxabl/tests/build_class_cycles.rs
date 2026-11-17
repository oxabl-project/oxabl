//! Class units that read each other (a cycle) compile in one job. A compile
//! binds the first r-code it finds for the other class, also r-code that an
//! older compile left beside the source or in `$OUT`, so the job compiles in
//! passes: each later pass binds the classes again and reads the r-code of
//! the pass before from `$OUT`, and only the last pass counts.

mod common;

use common::built::{Built, unit};
use serde_json::{Value, json};

fn class(me: &str, other: &str, extra: &str) -> String {
    format!(
        "{extra}CLASS pkg.{me}:\n  DEFINE VARIABLE o AS pkg.{other} NO-UNDO.\n  o = NEW pkg.{other}().\nEND CLASS.\n"
    )
}

/// `pkg.A` and `pkg.B` read each other, a user reads `pkg.A`, and r-code an
/// older compile left lies beside both sources.
fn cycle(extra_a: &str) -> Built {
    let workspace = Built::new();
    workspace.write("src/pkg/A.cls", &class("A", "B", extra_a));
    workspace.write("src/pkg/B.cls", &class("B", "A", ""));
    workspace.write("src/pkg/A.r", "stray r-code of A");
    workspace.write("src/pkg/B.r", "stray r-code of B");
    workspace.write(
        "src/user.p",
        "DEFINE VARIABLE o AS pkg.A NO-UNDO.\no = NEW pkg.A().\n",
    );
    workspace
}

fn compiled(workspace: &Built, name: &str) -> usize {
    workspace.compiled().iter().filter(|n| *n == name).count()
}

/// The r-code `rcode` read this build's r-code of `pkg/<class>.r` and no
/// r-code beside a source.
fn binds_out(workspace: &Built, rcode: &str, class: &str) {
    let reads = workspace.reads(rcode);
    assert!(
        reads
            .iter()
            .any(|(path, _)| path.ends_with(&format!("build/r/pkg/{class}.r"))),
        "{rcode} reads $OUT: {reads:?}"
    );
    assert!(
        !reads
            .iter()
            .any(|(path, _)| path.starts_with("src/") && path.ends_with(".r")),
        "{rcode} reads no stray r-code: {reads:?}"
    );
}

#[test]
fn a_cycle_with_stray_rcode_is_keyed_and_its_last_pass_reads_this_builds_rcode() {
    let workspace = cycle("");

    let plan = workspace.plan();
    for path in ["src/pkg/A.cls", "src/pkg/B.cls", "src/user.p"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
    }
    let run = workspace.build_with(&["--json"]);
    run.succeeded();
    let report: Value = serde_json::from_str(&run.stdout).unwrap();

    assert_eq!(report["summary"]["compiled"], 3, "{report}");
    assert_eq!(
        compiled(&workspace, "pkg/A.cls"),
        2,
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(
        compiled(&workspace, "pkg/B.cls"),
        2,
        "{:?}",
        workspace.compiled()
    );
    assert_eq!(compiled(&workspace, "user.p"), 1);
    binds_out(&workspace, "build/r/pkg/A.r", "B");
    binds_out(&workspace, "build/r/pkg/B.r", "A");
    binds_out(&workspace, "build/r/user.r", "A");

    workspace.clear_log();
    workspace.build().succeeded();
    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
}

#[test]
fn a_cycle_that_fails_in_the_first_pass_compiles_a_third_time_and_keeps_its_key() {
    let workspace = cycle("/* FAKE-FAIL-ONCE */\n");

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    assert_eq!(
        compiled(&workspace, "pkg/A.cls"),
        3,
        "{:?}",
        workspace.compiled()
    );
    binds_out(&workspace, "build/r/pkg/A.r", "B");
    let plan = workspace.plan();
    for path in ["src/pkg/A.cls", "src/pkg/B.cls"] {
        assert_eq!(unit(&plan, path)["action"], "up-to-date", "{path}: {plan}");
    }
}

#[test]
fn a_cycle_that_does_not_settle_is_recorded_without_a_key() {
    let cache = tempfile::TempDir::new().unwrap();
    let workspace = cycle("/* FAKE-FAIL-TWICE */\n");
    workspace.cache_in(cache.path());

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    assert_eq!(
        compiled(&workspace, "pkg/A.cls"),
        3,
        "{:?}",
        workspace.compiled()
    );
    let report: Value = serde_json::from_str(&run.stdout).unwrap();
    assert_eq!(unit(&report, "src/pkg/A.cls")["status"], "ok");
    let plan = workspace.plan();
    let note = json!([
        "a class of its cycle did not compile in the pass before, so it may have read r-code of another build"
    ]);
    for path in ["src/pkg/A.cls", "src/pkg/B.cls"] {
        assert_eq!(unit(&plan, path)["action"], "rebuild", "{path}: {plan}");
        assert_eq!(
            unit(&plan, path)["reasons"][0]["details"],
            note,
            "{path}: {plan}"
        );
    }
    assert_eq!(
        common::cache::entries(cache.path()).len(),
        1,
        "only user.p is written to the cache"
    );
}
