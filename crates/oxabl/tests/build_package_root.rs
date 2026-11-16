//! The includes and classes a class reads are searched from where the
//! compiler searches them: a class unit from its package root, a class that is
//! not a unit from the PROPATH of the unit that uses it.

mod common;

use common::built::{Built, unit};
use serde_json::{Value, json};

const USER: &str = "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n";
const FOO: &str = "CLASS pkg.Foo:\n{x.i}\nEND CLASS.\n";

/// `a/pkg/Foo.cls` includes `x.i`, which both its own directory and its
/// package root hold.
fn class_unit(directories: &str) -> Built {
    let workspace = Built::with(
        &format!("directories = [{directories}]\nroot_relative_include_paths = [\".\"]"),
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("a/pkg/Foo.cls", FOO);
    workspace.write("a/pkg/x.i", "/* beside the class */\n");
    workspace.write("a/x.i", "/* package root */\n");
    workspace
}

fn unkeyable(plan: &Value, path: &str) -> Value {
    unit(plan, path)["unkeyable"].clone()
}

#[test]
fn an_include_of_a_class_unit_is_read_from_its_package_root() {
    let workspace = class_unit("\"a\"");
    let plan = workspace.plan();
    for path in ["a/x.p", "a/pkg/Foo.cls"] {
        assert_eq!(unit(&plan, path)["closure"], "parsed", "{path}: {plan}");
    }
    workspace.build_with(&["--json"]).succeeded();
    assert!(
        workspace
            .reads("build/r/pkg/Foo.r")
            .contains(&("a/x.i".to_string(), workspace.sha256("a/x.i"))),
        "the compiler reads the include from the package root"
    );

    workspace.write("a/x.i", "/* package root, edited */\n");
    let plan = workspace.plan();
    for path in ["a/x.p", "a/pkg/Foo.cls"] {
        assert_eq!(
            unit(&plan, path)["action"],
            "rebuild",
            "{path}: an edit of the include the compiler reads is a reason: {plan}"
        );
    }
    workspace.clear_log();
    workspace.build_with(&["--json"]).succeeded();
    let compiled = workspace.compiled();
    for name in ["x.p", "pkg/Foo.cls"] {
        assert!(compiled.iter().any(|c| c == name), "{name}: {compiled:?}");
    }
    assert!(
        workspace
            .reads("build/r/pkg/Foo.r")
            .contains(&("a/x.i".to_string(), workspace.sha256("a/x.i"))),
        "the r-code holds the edited include"
    );

    workspace.write("a/pkg/x.i", "/* beside the class, edited */\n");
    let plan = workspace.plan();
    for path in ["a/x.p", "a/pkg/Foo.cls"] {
        assert_eq!(
            unit(&plan, path)["action"],
            "up-to-date",
            "{path}: the compiler does not read the file beside the class: {plan}"
        );
    }
}

#[test]
fn a_class_that_reads_its_includes_from_its_package_root_is_uniform() {
    let workspace = class_unit("\"a\", \"b\"");
    workspace.write("b/y.p", "MESSAGE \"plain\".\n");

    let plan = workspace.plan();

    assert_eq!(plan["propaths"].as_array().unwrap().len(), 2, "{plan}");
    for path in ["a/x.p", "a/pkg/Foo.cls"] {
        assert_eq!(
            unit(&plan, path)["class_resolution"],
            json!({ "uniform": true, "differing": [] }),
            "{path}: the analysis and the own PROPATH agree on x.i"
        );
    }
}

/// `a/x.p` uses `pkg.Bar`, which only `lib/pkg/Bar.cls` holds; `lib` is an
/// include path and not a source directory, so the compiler compiles the
/// class under the PROPATH of `a/x.p`.
fn class_that_is_not_a_unit() -> Built {
    let workspace = Built::with(
        "directories = [\"a\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"lib\"]",
        "",
    );
    workspace.write(
        "a/x.p",
        "DEFINE VARIABLE o AS pkg.Bar NO-UNDO.\no = NEW pkg.Bar().\n",
    );
    workspace.write("lib/pkg/Bar.cls", "CLASS pkg.Bar:\n{v.i}\nEND CLASS.\n");
    workspace.write("lib/v.i", "/* shared */\n");
    workspace
}

#[test]
fn a_class_that_is_not_a_unit_and_finds_its_include_on_the_user_propath_keeps_the_key() {
    let workspace = class_that_is_not_a_unit();

    let plan = workspace.plan();

    assert_eq!(plan["propaths"].as_array().unwrap().len(), 1, "{plan}");
    assert_eq!(unkeyable(&plan, "a/x.p"), json!([]), "{plan}");
    assert_eq!(unit(&plan, "a/x.p")["closure"], "parsed");
}

#[test]
fn an_include_of_a_class_that_is_not_a_unit_is_searched_on_the_user_propath() {
    let workspace = class_that_is_not_a_unit();
    workspace.write("a/v.i", "/* the user's directory */\n");

    let plan = workspace.plan();
    assert_eq!(plan["propaths"].as_array().unwrap().len(), 1, "{plan}");
    assert_eq!(
        unkeyable(&plan, "a/x.p"),
        json!(["class-propath: v.i in lib/pkg/Bar.cls finds a/v.i, analysed lib/v.i"]),
        "{plan}"
    );
    workspace.build_with(&["--json"]).succeeded();
    assert!(
        workspace
            .reads("build/r/a/x.r")
            .contains(&("a/v.i".to_string(), workspace.sha256("a/v.i"))),
        "the compiler reads the include on the PROPATH of a/x.p"
    );

    workspace.write("a/v.i", "/* the user's directory, edited */\n");
    let plan = workspace.plan();
    assert_eq!(unit(&plan, "a/x.p")["action"], "rebuild", "{plan}");
    workspace.clear_log();
    workspace.build_with(&["--json"]).succeeded();
    assert!(
        workspace.compiled().iter().any(|c| c == "x.p"),
        "{:?}",
        workspace.compiled()
    );
    assert!(
        workspace
            .reads("build/r/a/x.r")
            .contains(&("a/v.i".to_string(), workspace.sha256("a/v.i"))),
        "the r-code holds the edited include"
    );
}

#[test]
fn a_class_whose_name_is_not_written_out_and_whose_package_root_is_derived_has_no_key() {
    // The only possible package root is `a`, so oxabl derives `pkg.Foo`. The
    // compile searches `a/rf` first, the analysis `a/pkg/rf`: no key.
    let workspace = Built::with(
        "directories = [\"a\"]\nroot_relative_include_paths = [\"rf\"]\ninclude_paths = [\"a\"]",
        "",
    );
    workspace.write(
        "a/pkg/Foo.cls",
        "&GLOBAL-DEFINE n pkg.Foo\nCLASS {&n}:\n{x.i}\nEND CLASS.\n",
    );
    workspace.write("a/rf/x.i", "/* below the package root */\n");
    workspace.write("a/pkg/rf/x.i", "/* below the class directory */\n");

    let plan = workspace.plan();

    assert_eq!(
        unit(&plan, "a/pkg/Foo.cls")["compile_name"],
        "pkg/Foo.cls",
        "{plan}"
    );
    assert_eq!(
        unkeyable(&plan, "a/pkg/Foo.cls"),
        json!(["computed-name: class statement in a/pkg/Foo.cls"]),
        "{plan}"
    );
}

#[test]
fn a_listed_class_whose_include_the_user_propath_finds_elsewhere_gives_no_observed_key() {
    // `a/enc.p` cannot be read, so only its observed closure can key it; the
    // class part of its PROPATH lists `lib/pkg/Bar.cls`, which `a/x.p`
    // makes the analysis read.
    let workspace = class_that_is_not_a_unit();
    workspace.write("a/v.i", "/* the user's directory */\n");
    workspace.write("a/enc.p", "§ ¤ 0x9f ((( %%% .\n");
    std::thread::sleep(std::time::Duration::from_millis(2_200));
    workspace.build().succeeded();

    let plan = workspace.plan();

    let enc = unit(&plan, "a/enc.p");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert!(
        enc["unkeyable"].as_array().unwrap().contains(&json!(
            "observed-class: lib/pkg/Bar.cls: class-propath: v.i in lib/pkg/Bar.cls finds a/v.i, analysed lib/v.i"
        )),
        "{plan}"
    );
}
