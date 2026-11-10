//! Classes under per-unit PROPATHs, class conflicts, skipped units and the
//! ownership of the output directories. The worker is the fake AVM.

mod common;

use common::built::{Built, unit};
use serde_json::json;

const USER: &str = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n";
const FOO: &str = "CLASS pkg.Foo:\nEND CLASS.\n";

fn per_unit_propaths() -> Built {
    let workspace = Built::with(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("b/pkg/Foo.cls", FOO);
    workspace.write("a/plain.p", "MESSAGE \"plain\".\n");
    workspace
}

fn unkeyable(plan: &serde_json::Value, path: &str) -> Vec<String> {
    unit(plan, path)["unkeyable"]
        .as_array()
        .unwrap()
        .iter()
        .map(|reason| reason.as_str().unwrap().to_string())
        .collect()
}

#[test]
fn per_unit_propath_classes_build_with_one_class_skipped_and_three_units_unkeyable() {
    let workspace = per_unit_propaths();

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    let report: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
    for compiled in ["a/pkg/Foo.cls", "a/x.p", "b/y.p", "a/plain.p"] {
        assert_eq!(unit(&report, compiled)["status"], "ok", "{compiled}");
    }
    let skipped = unit(&report, "b/pkg/Foo.cls");
    assert_eq!(skipped["status"], "skipped");
    assert_eq!(skipped["skipped"], "output-collision: a/pkg/Foo.cls");
    assert_eq!(report["summary"]["skipped"], 1);
    assert!(
        run.stderr
            .contains("3 units that use classes are compiled without a key"),
        "one warning counts the units: {}",
        run.stderr
    );
    assert_eq!(run.stderr.matches("warning:").count(), 1, "{}", run.stderr);
    let plan = workspace.plan();
    assert_eq!(
        plan["propaths"].as_array().unwrap().len(),
        2,
        "two PROPATH ids"
    );
    for path in ["a/x.p", "b/y.p", "a/pkg/Foo.cls"] {
        assert_eq!(
            unkeyable(&plan, path),
            ["per-unit-propath-classes"],
            "{path}"
        );
    }
    assert!(unit(&plan, "a/plain.p")["key"].is_string());
    assert_eq!(unit(&plan, "a/plain.p")["action"], "up-to-date");
    assert!(workspace.exists("build/r/pkg/Foo.r"));
    assert!(workspace.exists("build/r/x.r") && workspace.exists("build/r/y.r"));
}

#[test]
fn one_propath_gives_keyable_users_and_no_warning() {
    let workspace = Built::with("directories = [\"a\"]\ninclude_paths = [\"a\"]", "");
    workspace.write("a/x.p", USER);
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("a/plain.p", "MESSAGE \"plain\".\n");

    let run = workspace.build();

    run.succeeded();
    assert!(!run.stderr.contains("warning"), "{}", run.stderr);
    let plan = workspace.plan();
    for path in ["a/x.p", "a/pkg/Foo.cls", "a/plain.p"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}");
        assert_eq!(unit(&plan, path)["action"], "up-to-date", "{path}");
    }
}

#[test]
fn a_per_unit_propath_unit_stays_unkeyable_when_only_another_unit_changes() {
    let workspace = per_unit_propaths();
    workspace.build().succeeded();
    workspace.write("b/y.p", &format!("{USER}MESSAGE \"edit\".\n"));

    let plan = workspace.plan();

    assert_eq!(unkeyable(&plan, "b/y.p"), ["per-unit-propath-classes"]);
    assert_eq!(unit(&plan, "a/plain.p")["action"], "up-to-date");
}

fn acme(with_class: bool) -> Built {
    let workspace = Built::with(
        "include_paths = [\"src\", \"$LIB:acme\"]\ncase_insensitive_lookup = false",
        "[build.libraries]\nacme = \"libs/acme.pl\"\n[build.external_packages]\n\"Acme.Util.\" = [\"$LIB:acme\"]",
    );
    workspace.write("libs/acme.pl", "acme bytes");
    workspace.write(
        "src/q.p",
        "DEFINE VARIABLE o AS acme.util.x NO-UNDO.\no = NEW acme.util.x().\n",
    );
    if with_class {
        workspace.write("src/Acme/Util/X.cls", "CLASS Acme.Util.X:\nEND CLASS.\n");
    }
    workspace
}

#[test]
fn a_class_conflict_with_a_library_gives_no_key_and_one_warning_but_compiles() {
    let workspace = acme(true);

    let run = workspace.build();

    run.succeeded();
    assert!(
        run.stderr.contains("class Acme.Util.X is accepted as external ($LIB:acme) but src/Acme/Util/X.cls also has that name"),
        "{}",
        run.stderr
    );
    assert!(workspace.exists("build/r/q.r"));
    let plan = workspace.plan();
    assert_eq!(unkeyable(&plan, "src/q.p"), ["class-conflict: Acme.Util.X"]);
}

#[test]
fn without_the_workspace_class_the_library_class_is_keyable() {
    let workspace = acme(false);

    let run = workspace.build();

    run.succeeded();
    assert!(!run.stderr.contains("warning"), "{}", run.stderr);
    assert!(workspace.plan()["units"][0]["key"].is_string());
}

#[test]
fn a_class_conflict_stays_when_only_the_user_changes() {
    let workspace = acme(true);
    workspace.build().succeeded();
    workspace.write(
        "src/q.p",
        "DEFINE VARIABLE o AS acme.util.x NO-UNDO.\no = NEW acme.util.x().\nMESSAGE \"edit\".\n",
    );

    let plan = workspace.plan();

    assert_eq!(unkeyable(&plan, "src/q.p"), ["class-conflict: Acme.Util.X"]);
    assert_eq!(unit(&plan, "src/Acme/Util/X.cls")["action"], "up-to-date");
}

#[test]
fn a_class_in_the_wrong_package_directory_is_skipped_and_the_exit_is_zero() {
    let workspace = Built::new();
    workspace.write("src/x/Foo.cls", FOO);
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    let report: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
    let skipped = unit(&report, "src/x/Foo.cls");
    assert_eq!(skipped["status"], "skipped");
    assert_eq!(skipped["skipped"], "package-mismatch: pkg.Foo");
    assert_eq!(workspace.compiled(), ["a.p"]);
}

#[test]
fn a_class_whose_package_root_is_not_on_the_propath_is_skipped() {
    let workspace = Built::with("directories = [\"a\"]\ninclude_paths = [\"lib\"]", "");
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("lib/other.p", "MESSAGE \"o\".\n");

    let run = workspace.build_with(&["--json"]);

    run.succeeded();
    let report: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
    let skipped = unit(&report, "a/pkg/Foo.cls");
    assert_eq!(skipped["status"], "skipped");
    assert_eq!(skipped["skipped"], "package-root-not-on-propath: a");
    assert!(workspace.compiled().is_empty());
}

#[test]
fn the_plan_without_a_probe_shows_the_skips_that_need_no_environment() {
    let workspace = Built::with("directories = [\"a\"]\ninclude_paths = [\"lib\"]", "");
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("a/x/Bar.cls", "CLASS pkg.Bar:\nEND CLASS.\n");
    workspace.write("lib/other.p", "MESSAGE \"o\".\n");
    workspace.write("a/b.p", "MESSAGE \"b\".\n");
    workspace.build().succeeded();

    for _ in 0..2 {
        let run = workspace.build_with(&["--plan", "--json"]);
        run.succeeded();
        let plan: serde_json::Value = serde_json::from_str(&run.stdout).unwrap();
        let foo = unit(&plan, "a/pkg/Foo.cls");
        assert_eq!(foo["action"], "skip", "{foo}");
        assert_eq!(foo["unkeyable"], json!(["package-root-not-on-propath: a"]));
        let bar = unit(&plan, "a/x/Bar.cls");
        assert_eq!(bar["action"], "skip", "{bar}");
        assert_eq!(bar["unkeyable"], json!(["package-mismatch: pkg.Bar"]));
        let b = unit(&plan, "a/b.p");
        assert_eq!(b["action"], "skip", "{b}");
        assert_eq!(b["unkeyable"], json!(["not-on-propath"]));
        assert_eq!(plan["summary"]["rebuild"], 0, "{plan}");
        assert_eq!(plan["summary"]["skipped"], 3, "{plan}");
    }

    let text = workspace.build_with(&["--plan"]);
    text.succeeded();
    assert!(
        text.stdout.lines().any(|line| line.starts_with("skip")
            && line.contains("a/pkg/Foo.cls")
            && line.ends_with("package-root-not-on-propath: a")),
        "{}",
        text.stdout
    );
}

#[test]
fn an_output_directory_with_a_foreign_file_and_no_marker_stops_the_build() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("build/r/precious.txt", "keep me");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(
        run.stderr
            .contains("build/r is not empty and is not managed by oxabl"),
        "{}",
        run.stderr
    );
    assert_eq!(workspace.read("build/r/precious.txt"), "keep me");
    assert!(!workspace.exists("build/r/.oxabl-output"));
    assert!(workspace.compiled().is_empty());
}

#[test]
fn an_empty_output_directory_is_claimed_with_a_marker() {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    std::fs::create_dir_all(workspace.path("build/r")).unwrap();

    workspace.build().succeeded();

    assert!(workspace.exists("build/r/.oxabl-output"));
}

#[test]
fn a_foreign_r_file_in_the_managed_directory_is_deleted_and_no_unit_reads_it() {
    let workspace = Built::new();
    workspace.write("src/user.p", USER);
    workspace.build().succeeded();
    workspace.write("build/r/pkg/Foo.r", "a planted class\n");
    workspace.write("build/r/other.txt", "stray");
    workspace.write("src/user.p", &format!("{USER}MESSAGE \"edit\".\n"));

    workspace.build().succeeded();

    assert!(
        !workspace.exists("build/r/pkg/Foo.r"),
        "the planted r-code stays"
    );
    assert!(!workspace.exists("build/r/other.txt"));
    let reads = workspace.reads("build/r/user.r");
    assert!(
        reads.iter().all(|(path, _)| !path.ends_with("pkg/Foo.r")),
        "the unit read the planted file: {reads:?}"
    );
}

fn wait_for_a_compile(workspace: &Built) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(20);
    while workspace.compiled().is_empty() && std::time::Instant::now() < deadline {
        std::thread::sleep(std::time::Duration::from_millis(25));
    }
    assert!(
        !workspace.compiled().is_empty(),
        "the first build never compiled"
    );
}

#[test]
fn a_second_build_in_one_workspace_stops_with_the_pid_of_the_first() {
    let workspace = Built::new();
    workspace.write("src/slow.p", "/* FAKE-SLOW */\nMESSAGE \"s\".\n");
    let first = workspace.spawn(&[]);
    wait_for_a_compile(&workspace);

    let again = workspace.build();

    assert_eq!(again.code, 3, "{}", again.stderr);
    assert!(
        again
            .stderr
            .contains(&format!("another oxabl build (pid {}) holds", first.id())),
        "{}",
        again.stderr
    );
    first.wait_with_output().unwrap();
}

#[test]
fn two_workspaces_that_share_an_output_directory_cannot_build_at_once() {
    let first = Built::new();
    first.write("src/slow.p", "/* FAKE-SLOW */\nMESSAGE \"s\".\n");
    let second = Built::new();
    second.write("src/a.p", "MESSAGE \"a\".\n");
    std::fs::create_dir_all(first.path("build/r")).unwrap();
    std::os::unix::fs::symlink(first.path("build"), second.path("build")).unwrap();
    let running = first.spawn(&[]);
    wait_for_a_compile(&first);

    let blocked = second.build();

    assert_eq!(blocked.code, 3, "{}", blocked.stderr);
    assert!(
        blocked.stderr.contains(".oxabl-build.lock"),
        "{}",
        blocked.stderr
    );
    running.wait_with_output().unwrap();
}
