//! How each class name, and each include inside a class whose source the
//! compiler reads, resolves under every PROPATH id: the `class_resolution` of
//! `oxabl build --plan --probe --json`. The compiler searches `$OUT` first,
//! which holds the r-code of every class unit; then the first entry with the
//! class r-code or source, r-code first, exact in case.

mod common;

use common::built::{Built, unit};
use serde_json::{Value, json};

const USER: &str = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n";
const FOO: &str = "CLASS pkg.Foo:\nEND CLASS.\n";
const FOO_WITH_INCLUDE: &str = "CLASS pkg.Foo:\n{v.i}\nEND CLASS.\n";

/// `a/x.p` and `b/y.p` use `pkg.Foo`, which only `lib/pkg/Foo.cls` holds:
/// three PROPATH ids that agree on the class, a class unit.
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

/// The same, with `lib` only on the include paths: `lib/pkg/Foo.cls` is not
/// a unit, so each user's compile reads its source.
fn shared_source(class: &str) -> Built {
    let workspace = Built::with(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"lib\"]",
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("lib/pkg/Foo.cls", class);
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
        assert_eq!(unkeyable(&plan, path), json!([]), "{path}: keyed");
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
fn a_user_of_the_class_that_out_does_not_hold_is_not_uniform() {
    let workspace = Built::with(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("b/pkg/Foo.cls", FOO);

    let plan = workspace.plan();

    for path in ["a/x.p", "a/pkg/Foo.cls"] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": true, "differing": [] }),
            "{path}: $OUT holds a/pkg/Foo.cls, the class its analysis found"
        );
    }
    assert_eq!(
        resolution(&plan, "b/y.p"),
        &json!({ "uniform": false, "differing": [{
            "kind": "class",
            "name": "pkg.Foo",
            "via": "b/y.p",
            "files": ["a/pkg/Foo.cls", "b/pkg/Foo.cls"],
            "failed": ["U1"],
        }]}),
        "b/y.p binds the r-code of a/pkg/Foo.cls, not b/pkg/Foo.cls"
    );
    assert!(
        unit(&plan, "b/pkg/Foo.cls")
            .get("class_resolution")
            .is_none(),
        "a skipped unit has none"
    );
}

#[test]
fn an_include_of_a_class_that_is_not_a_unit_and_that_a_unit_directory_shadows_is_not_uniform() {
    let workspace = shared_source(FOO_WITH_INCLUDE);
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
}

#[test]
fn an_include_of_a_class_unit_counts_only_for_its_own_compile() {
    let workspace = shared_class(FOO_WITH_INCLUDE);
    workspace.write("lib/v.i", "/* shared */\n");
    workspace.write("a/v.i", "/* shadow */\n");

    let plan = workspace.plan();

    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls"] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": true, "differing": [] }),
            "{path}: users read the class r-code from $OUT, not its includes"
        );
    }
}

#[test]
fn class_rcode_beside_a_class_unit_is_uniform() {
    let workspace = shared_class(FOO);
    workspace.write("a/pkg/Foo.r", "stale r-code");
    workspace.write("lib/pkg/Foo.r", "stale r-code");

    let plan = workspace.plan();

    for path in ["a/x.p", "b/y.p", "lib/pkg/Foo.cls"] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": true, "differing": [] }),
            "{path}: $OUT comes first"
        );
    }
}

#[test]
fn class_rcode_before_a_class_that_is_not_a_unit_is_not_uniform() {
    let workspace = shared_source(FOO);
    workspace.write("a/pkg/Foo.r", "stale r-code");

    let plan = workspace.plan();

    assert_eq!(
        resolution(&plan, "a/x.p"),
        &json!({ "uniform": false, "differing": [{
            "kind": "class",
            "name": "pkg.Foo",
            "via": "a/x.p",
            "files": ["a/pkg/Foo.r", "lib/pkg/Foo.cls"],
            "failed": ["rcode"],
        }]}),
        "the own PROPATH of a/x.p binds the stray r-code"
    );
    assert_eq!(
        resolution(&plan, "b/y.p"),
        &json!({ "uniform": false, "differing": [{
            "kind": "class",
            "name": "pkg.Foo",
            "via": "b/y.p",
            "files": ["a/pkg/Foo.r", "lib/pkg/Foo.cls"],
            "failed": ["U1"],
        }]}),
        "another PROPATH id binds the stray r-code"
    );
}

#[test]
fn a_file_that_matches_only_without_case_is_not_found() {
    let workspace = shared_source(FOO_WITH_INCLUDE);
    workspace.write("lib/v.i", "/* shared */\n");
    workspace.write("b/V.i", "/* other case */\n");
    workspace.write("a/Pkg/foo.r", "other case");

    let plan = workspace.plan();

    for path in ["a/x.p", "b/y.p"] {
        assert_eq!(
            resolution(&plan, path),
            &json!({ "uniform": true, "differing": [] }),
            "{path}: the compiler matches case exactly"
        );
    }
}
