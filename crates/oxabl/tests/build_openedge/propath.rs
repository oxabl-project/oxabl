//! Classes under per-unit PROPATHs, on a real AVM.

use serde_json::Value;

use crate::common::built::unit;
use crate::common::openedge::compile::{Compile, Options, compile_workspace_with};
use crate::compile::run_from_rcode;
use crate::setup;

const USER: &str = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.
o = NEW pkg.Foo().
PUT UNFORMATTED o:Place() SKIP.
";

/// A `Foo` that returns `place`; `extra` adds a method, so that the r-code of
/// two classes with one name has two signatures.
fn foo(place: &str, extra: bool) -> String {
    let extra = match extra {
        true => {
            "    METHOD PUBLIC CHARACTER Extra():\n        RETURN \"extra\".\n    END METHOD.\n"
        }
        false => "",
    };
    format!(
        "CLASS pkg.Foo:\n    METHOD PUBLIC CHARACTER Place():\n        RETURN \"{place}\".\n    END METHOD.\n{extra}END CLASS.\n"
    )
}

fn unkeyable(plan: &Value, path: &str) -> Vec<String> {
    unit(plan, path)["unkeyable"]
        .as_array()
        .unwrap()
        .iter()
        .map(|reason| reason.as_str().unwrap().to_string())
        .collect()
}

fn layout(workspace: &Compile) {
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("a/pkg/Foo.cls", &foo("a", false));
    workspace.write("b/pkg/Foo.cls", &foo("b", true));
    workspace.write("a/plain.p", "PUT UNFORMATTED \"plain\" SKIP.\n");
}

/// Two directories that each hold a `pkg/Foo.cls`, with each unit searching its
/// own directory first: the units that reach a class have no key, one `Foo`
/// compiles and the other is skipped, and nothing but the plain unit is cached.
/// The two `Foo` have different signatures, so running each user from the one
/// `pkg/Foo.r` records which `Foo` the AVM compiled the user against.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn per_unit_propath_classes() {
    let target = setup();
    let workspace = compile_workspace_with(
        &target,
        Options {
            sources: "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]".into(),
            workers: 1,
            ..Options::default()
        },
    );
    layout(&workspace);
    let cache = tempfile::TempDir::new().unwrap();

    let output = workspace.build_with_cache(&["--json"], 0, "developer", Some(cache.path()));

    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    for compiled in ["a/pkg/Foo.cls", "a/x.p", "b/y.p", "a/plain.p"] {
        assert_eq!(unit(&report, compiled)["status"], "ok", "{compiled}");
    }
    let skipped = unit(&report, "b/pkg/Foo.cls");
    assert_eq!(skipped["status"], "skipped");
    assert_eq!(skipped["skipped"], "output-collision: a/pkg/Foo.cls");
    assert!(
        stderr.contains("3 units that use classes are compiled without a key"),
        "{stderr}"
    );
    assert_eq!(
        stderr.matches("compiled without a key").count(),
        1,
        "{stderr}"
    );
    assert_eq!(report["cache"]["offered"], 1, "{report}");
    assert_eq!(report["cache"]["stored"], 1, "{report}");

    let plan = workspace.plan();
    assert_eq!(plan["propaths"].as_array().unwrap().len(), 2);
    for path in ["a/x.p", "b/y.p", "a/pkg/Foo.cls"] {
        assert_eq!(
            unkeyable(&plan, path),
            ["per-unit-propath-classes"],
            "{path}"
        );
    }
    assert!(unit(&plan, "a/plain.p")["key"].is_string());
    assert_eq!(unit(&plan, "a/plain.p")["action"], "up-to-date");

    for user in ["x", "y"] {
        let printed = workspace.run_program(&target, &run_from_rcode(&format!("{user}.p")));
        eprintln!("EVIDENCE per_unit_propath_classes {user}.p ran: {printed:?}");
    }

    workspace.write("b/y.p", &format!("{USER}PUT UNFORMATTED \"edit\" SKIP.\n"));
    let plan = workspace.plan();
    assert_eq!(unkeyable(&plan, "b/y.p"), ["per-unit-propath-classes"]);
    assert_eq!(unit(&plan, "a/plain.p")["action"], "up-to-date");
}

/// With one PROPATH id the users of the class are keyable and the build has no
/// warning.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn one_propath_gives_keyable_class_users() {
    let target = setup();
    let workspace = compile_workspace_with(
        &target,
        Options {
            sources: "directories = [\"a\"]\ninclude_paths = [\"a\"]".into(),
            workers: 1,
            ..Options::default()
        },
    );
    workspace.write("a/x.p", USER);
    workspace.write("a/pkg/Foo.cls", &foo("a", false));
    workspace.write("a/plain.p", "PUT UNFORMATTED \"plain\" SKIP.\n");

    let output = workspace.build(&["--json"], 0);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!stderr.contains("compiled without a key"), "{stderr}");
    let plan = workspace.plan();
    for path in ["a/x.p", "a/pkg/Foo.cls", "a/plain.p"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
    }
}

/// Records which `Foo` a user is compiled against when each directory has its
/// own: the user in `b` calls a method that only `b/pkg/Foo.cls` has. The
/// result is evidence and is not asserted.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn per_unit_propath_class_binding() {
    let target = setup();
    let workspace = compile_workspace_with(
        &target,
        Options {
            sources: "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]".into(),
            workers: 1,
            ..Options::default()
        },
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", &format!("{USER}PUT UNFORMATTED o:Extra() SKIP.\n"));
    workspace.write("a/pkg/Foo.cls", &foo("a", false));
    workspace.write("b/pkg/Foo.cls", &foo("b", true));

    let output = workspace.oxabl(&["--json"]).output().unwrap();

    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    for path in ["a/pkg/Foo.cls", "a/x.p", "b/y.p"] {
        let found = unit(&report, path);
        eprintln!(
            "EVIDENCE per_unit_propath_class_binding {path}: {} {}",
            found["status"], found["messages"]
        );
    }
}
