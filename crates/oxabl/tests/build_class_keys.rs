//! Keys for units that use classes when the units have different PROPATHs
//! (K9). A user of a class unit reads its r-code from `$OUT`, which the build
//! fills before the user compiles, so the user keeps its key unless a name it
//! reaches resolves to different files.

mod common;

use std::path::Path;

use common::built::{Built, unit};
use common::cache::entries;
use serde_json::{Value, json};
use tempfile::TempDir;

const USER: &str = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n";
const FOO: &str = "CLASS pkg.Foo:\n{v.i}\nEND CLASS.\n";

/// `a/x.p` and `b/y.p` use the class unit `lib/pkg/Foo.cls`, which includes
/// `{v.i}`: three PROPATH ids.
fn per_directory(cache: &Path) -> Built {
    let workspace = Built::with(
        "directories = [\"a\", \"b\", \"lib\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"lib\"]",
        "",
    );
    workspace.cache_in(cache);
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("lib/pkg/Foo.cls", FOO);
    workspace.write("lib/v.i", "/* v */\n");
    workspace.write("a/plain.p", "MESSAGE \"plain\".\n");
    workspace
}

fn report(workspace: &Built, args: &[&str]) -> Value {
    let run = workspace.build_with(&[&["--json"], args].concat());
    run.succeeded();
    serde_json::from_str(&run.stdout).unwrap_or_else(|e| panic!("{e}: {}", run.stdout))
}

/// The r-code of `user` records the class's r-code in `$OUT` with the hash of
/// the file there now.
fn binds_out(workspace: &Built, user: &str) {
    let reads = workspace.reads(user);
    let out = workspace.sha256("build/r/pkg/Foo.r");
    assert!(
        reads
            .iter()
            .any(|(path, hash)| path.ends_with("build/r/pkg/Foo.r") && *hash == out),
        "{user} binds the class unit's r-code: {reads:?}"
    );
    assert!(
        !reads.iter().any(|(path, _)| path.ends_with(".cls")),
        "{user} reads no class source: {reads:?}"
    );
}

#[test]
fn users_of_a_shared_class_unit_are_keyed_and_fetched_into_a_second_workspace() {
    let cache = TempDir::new().unwrap();
    let first = per_directory(cache.path());

    let plan = first.plan();
    assert_eq!(plan["propaths"].as_array().unwrap().len(), 3);
    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls", "a/plain.p"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
    }
    assert_eq!(
        plan["class_resolution"],
        json!({ "units": 3, "uniform": 3, "released": 3 })
    );
    let run = first.build();
    run.succeeded();
    assert!(!run.stderr.contains("warning"), "{}", run.stderr);
    binds_out(&first, "build/r/a/x.r");
    binds_out(&first, "build/r/b/y.r");

    first.clear_log();
    let again = report(&first, &[]);
    assert_eq!(again["summary"]["compiled"], 0);
    assert!(first.compiled().is_empty(), "{:?}", first.compiled());

    let second = per_directory(cache.path());
    let fetched = report(&second, &[]);
    assert_eq!(fetched["summary"]["fetched"], 4, "{fetched}");
    assert_eq!(fetched["summary"]["compiled"], 0, "{fetched}");
    binds_out(&second, "build/r/a/x.r");
    binds_out(&second, "build/r/b/y.r");
}

#[test]
fn stray_rcode_beside_a_class_unit_keeps_every_key() {
    let cache = TempDir::new().unwrap();
    let workspace = per_directory(cache.path());
    workspace.write("lib/pkg/Foo.r", "stray r-code");
    workspace.write("a/pkg/Foo.r", "stray r-code");

    let plan = workspace.plan();
    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
    }
    workspace.build().succeeded();
    binds_out(&workspace, "build/r/a/x.r");
    binds_out(&workspace, "build/r/b/y.r");
}

#[test]
fn an_include_of_a_class_unit_that_a_unit_directory_shadows_keeps_the_user_keys() {
    let cache = TempDir::new().unwrap();
    let workspace = per_directory(cache.path());
    workspace.write("a/v.i", "/* shadow */\n");

    let plan = workspace.plan();

    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
    }
    workspace.build().succeeded();
    let reads = workspace.reads("build/r/pkg/Foo.r");
    assert!(
        reads.iter().any(|(path, _)| path == "lib/v.i"),
        "the class compiles on its own PROPATH: {reads:?}"
    );
    binds_out(&workspace, "build/r/a/x.r");
}

#[test]
fn a_class_source_added_in_a_unit_directory_later_takes_the_key_of_the_other_users() {
    let cache = TempDir::new().unwrap();
    let workspace = per_directory(cache.path());
    workspace.build().succeeded();
    let before = entries(cache.path()).len();

    workspace.write("a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    let plan = workspace.plan();

    assert_eq!(
        unit(&plan, "lib/pkg/Foo.cls")["unkeyable"],
        json!(["output-collision: a/pkg/Foo.cls"])
    );
    assert_eq!(
        unit(&plan, "b/y.p")["unkeyable"],
        json!(["per-unit-propath-classes: pkg.Foo (a/pkg/Foo.cls, lib/pkg/Foo.cls)"]),
        "{plan}"
    );
    assert!(unit(&plan, "a/x.p")["key"].is_string(), "{plan}");
    let run = workspace.build_with(&["--json"]);
    run.succeeded();
    assert!(
        run.stderr.contains(
            "1 units that use classes are compiled without a key: their PROPATHs resolve a name to different files (pkg.Foo (a/pkg/Foo.cls, lib/pkg/Foo.cls))"
        ),
        "{}",
        run.stderr
    );
    let report: Value = serde_json::from_str(&run.stdout).unwrap();
    assert_eq!(report["summary"]["fetched"], 0, "{report}");
    assert_eq!(unit(&report, "b/y.p")["status"], "ok");
    assert_eq!(
        entries(cache.path()).len(),
        before + 2,
        "only a/x.p and a/pkg/Foo.cls are written"
    );
}

#[test]
fn users_of_a_class_unit_that_failed_are_not_stored_with_a_key() {
    let cache = TempDir::new().unwrap();
    let workspace = per_directory(cache.path());
    workspace.write("lib/pkg/Foo.cls", &format!("/* FAKE-FAIL */\n{FOO}"));
    // With no r-code in `$OUT`, the users bind this one and compile.
    workspace.write("lib/pkg/Foo.r", "stray r-code");

    let run = workspace.build_with(&["--json"]);

    assert_eq!(run.code, 1, "{}", run.stderr);
    let report: Value = serde_json::from_str(&run.stdout).unwrap();
    for path in ["a/x.p", "b/y.p"] {
        assert_eq!(unit(&report, path)["status"], "ok", "{path}");
        assert_eq!(unit(&report, path)["tainted"], true, "{path}");
    }
    assert_eq!(
        entries(cache.path()).len(),
        1,
        "only a/plain.p is written to the cache"
    );
    let plan = workspace.plan();
    for path in ["a/x.p", "b/y.p"] {
        assert_eq!(unit(&plan, path)["action"], "rebuild", "{path}: {plan}");
        assert_eq!(
            unit(&plan, path)["reasons"][0]["details"],
            json!(["a unit it reads failed in this build"]),
            "{path}"
        );
    }
}

#[test]
fn a_user_inherits_the_class_propath_reason_of_a_class_unit_it_reads_from_out() {
    let workspace = Built::with(
        "directories = [\"a\", \"lib\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"ext\", \"lib\"]",
        "",
    );
    workspace.write(
        "a/x.p",
        "DEFINE VARIABLE o AS pkg.A NO-UNDO.\no = NEW pkg.A().\n",
    );
    workspace.write(
        "lib/pkg/A.cls",
        "CLASS pkg.A:\nDEFINE VARIABLE b AS pkg.B NO-UNDO.\nEND CLASS.\n",
    );
    workspace.write("ext/pkg/B.cls", "CLASS pkg.B:\n{w.i}\nEND CLASS.\n");
    workspace.write("ext/w.i", "/* ext */\n");
    workspace.write("lib/w.i", "/* lib */\n");

    let plan = workspace.plan();

    let reason = json!("class-propath: w.i in ext/pkg/B.cls finds lib/w.i, analysed ext/w.i");
    for path in ["lib/pkg/A.cls", "a/x.p"] {
        assert!(
            unit(&plan, path)["unkeyable"]
                .as_array()
                .unwrap()
                .contains(&reason),
            "{path}: the r-code of A in $OUT holds lib/w.i: {plan}"
        );
    }
}
