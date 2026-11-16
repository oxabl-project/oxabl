//! How each class name, and each include inside a class, resolves under every
//! PROPATH id: the `class_resolution` of `oxabl build --plan --probe --json`.
//! Reported only; K9 still withholds every key here.

mod common;

use common::built::{Built, unit};
use serde_json::{Value, json};

const USER: &str = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n";
const FOO: &str = "CLASS pkg.Foo:\nEND CLASS.\n";
const FOO_WITH_INCLUDE: &str = "CLASS pkg.Foo:\n{v.i}\nEND CLASS.\n";

/// `a/x.p` and `b/y.p` use `pkg.Foo`, which only `lib/pkg/Foo.cls` holds:
/// three PROPATH ids that agree on the class.
fn shared_class(class: &str) -> Built {
    let workspace = Built::with(
        "directories = [\"a\", \"b\", \"lib\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"lib\"]",
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("lib/pkg/Foo.cls", class);
    workspace.write("a/plain.p", "MESSAGE \"plain\".\n");
    workspace
}

fn resolution<'a>(plan: &'a Value, path: &str) -> &'a Value {
    &unit(plan, path)["class_resolution"]
}

fn unkeyable(plan: &Value, path: &str) -> Value {
    unit(plan, path)["unkeyable"].clone()
}

#[test]
fn a_class_that_every_propath_resolves_to_one_file_is_uniform() {
    let workspace = shared_class(FOO);

    let plan = workspace.plan();

    assert_eq!(plan["propaths"].as_array().unwrap().len(), 3, "{plan}");
    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls"] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": true, "differing": [] }),
            "{path}"
        );
        assert_eq!(
            unkeyable(&plan, path),
            json!(["per-unit-propath-classes"]),
            "{path}: no key changes"
        );
    }
    assert!(
        unit(&plan, "a/plain.p").get("class_resolution").is_none(),
        "a unit that reaches no class has none"
    );
    assert_eq!(
        plan["class_resolution"],
        json!({ "units": 3, "uniform": 3, "released": 3 })
    );
}

#[test]
fn two_files_of_one_class_name_are_not_uniform_and_both_are_named() {
    let workspace = Built::with(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("b/pkg/Foo.cls", FOO);

    let plan = workspace.plan();

    let both = json!(["a/pkg/Foo.cls", "b/pkg/Foo.cls"]);
    for (path, via) in [
        ("a/x.p", "a/x.p"),
        ("b/y.p", "b/y.p"),
        ("a/pkg/Foo.cls", "a/pkg/Foo.cls"),
    ] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": false, "differing": [{
                "kind": "class",
                "name": "pkg.Foo",
                "via": via,
                "files": both,
                "failed": ["U1"],
            }]}),
            "{path}"
        );
    }
    assert!(
        unit(&plan, "b/pkg/Foo.cls")
            .get("class_resolution")
            .is_none(),
        "a skipped unit has none"
    );
    assert_eq!(
        plan["class_resolution"],
        json!({ "units": 3, "uniform": 0, "released": 0 })
    );
}

#[test]
fn an_include_of_a_class_that_a_unit_directory_shadows_is_not_uniform() {
    let workspace = shared_class(FOO_WITH_INCLUDE);
    workspace.write("lib/v.i", "/* shared */\n");
    workspace.write("a/v.i", "/* shadow */\n");

    let plan = workspace.plan();

    let differing = |failed: Value| {
        json!({ "uniform": false, "differing": [{
            "kind": "include",
            "name": "v.i",
            "via": "lib/pkg/Foo.cls",
            "files": ["a/v.i", "lib/v.i"],
            "failed": failed,
        }]})
    };
    assert_eq!(
        resolution(&plan, "a/x.p"),
        &differing(json!(["U1", "U3"])),
        "the own PROPATH of a/x.p finds the shadow"
    );
    assert_eq!(resolution(&plan, "b/y.p"), &differing(json!(["U1"])));
    assert_eq!(
        resolution(&plan, "lib/pkg/Foo.cls"),
        &differing(json!(["U1"]))
    );
    assert_eq!(
        plan["class_resolution"],
        json!({ "units": 3, "uniform": 0, "released": 0 })
    );
}

#[test]
fn class_rcode_in_a_source_directory_is_not_uniform() {
    let workspace = shared_class(FOO);
    workspace.write("a/pkg/Foo.r", "stale r-code");

    let plan = workspace.plan();

    for (path, via) in [
        ("a/x.p", "a/x.p"),
        ("b/y.p", "b/y.p"),
        ("lib/pkg/Foo.cls", "lib/pkg/Foo.cls"),
    ] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": false, "differing": [{
                "kind": "class",
                "name": "pkg.Foo",
                "via": via,
                "files": ["a/pkg/Foo.r", "lib/pkg/Foo.cls"],
                "failed": ["U2"],
            }]}),
            "{path}"
        );
    }
}

#[test]
fn an_include_that_matches_only_without_case_is_not_uniform() {
    let workspace = shared_class(FOO_WITH_INCLUDE);
    workspace.write("lib/v.i", "/* shared */\n");
    workspace.write("b/V.i", "/* other case */\n");

    let plan = workspace.plan();

    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls"] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": false, "differing": [{
                "kind": "include",
                "name": "v.i",
                "via": "lib/pkg/Foo.cls",
                "files": ["b/V.i", "lib/v.i"],
                "failed": ["case"],
            }]}),
            "{path}"
        );
    }
}
