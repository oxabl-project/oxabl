//! Class r-code that the compiler binds before a class source (K11): the
//! first PROPATH entry that holds `pkg/Bar.r` or `pkg/Bar.cls` decides, the
//! r-code first. R-code that oxabl does not hash takes the key away; r-code
//! beside a class unit does not, because `$OUT` comes first.

mod common;

use common::built::{Built, unit};
use serde_json::{Value, json};

const USER: &str = "DEFINE VARIABLE o AS pkg.Bar NO-UNDO.\no = NEW pkg.Bar().\n";
const BAR: &str = "CLASS pkg.Bar:\nEND CLASS.\n";

fn unkeyable(plan: &Value, path: &str) -> Value {
    unit(plan, path)["unkeyable"].clone()
}

/// One PROPATH: `src/x.p` uses `pkg.Bar`, a class that is not a unit
/// (`lib` is only an include path), with r-code beside its source.
fn single_propath() -> Built {
    let workspace = Built::with("include_paths = [\"src\", \"lib\"]", "");
    workspace.write("src/x.p", USER);
    workspace.write("lib/pkg/Bar.cls", BAR);
    workspace.write("lib/pkg/Bar.r", "stray r-code 1");
    workspace.write("src/plain.p", "MESSAGE \"plain\".\n");
    workspace
}

#[test]
fn rcode_beside_a_class_that_is_not_a_unit_takes_the_key_away() {
    let workspace = single_propath();

    let plan = workspace.plan();

    assert_eq!(plan["propaths"].as_array().unwrap().len(), 1);
    assert_eq!(
        unkeyable(&plan, "src/x.p"),
        json!(["class-rcode: lib/pkg/Bar.r"])
    );
    assert!(unit(&plan, "src/plain.p")["key"].is_string());
    assert!(
        unit(&plan, "src/x.p").get("class_resolution").is_none(),
        "one PROPATH id: no class resolution in the plan"
    );
}

#[test]
fn an_edit_of_rcode_beside_a_class_that_is_not_a_unit_compiles_the_user_again() {
    let workspace = single_propath();
    let run = workspace.build();
    run.succeeded();
    assert!(
        run.stderr.contains(
            "1 units that use classes are compiled without a key: the compiler binds class r-code that oxabl does not hash (lib/pkg/Bar.r)"
        ),
        "{}",
        run.stderr
    );
    assert!(
        workspace.reads("build/r/x.r").contains(&(
            "lib/pkg/Bar.r".to_string(),
            workspace.sha256("lib/pkg/Bar.r")
        )),
        "the compile binds the r-code: {:?}",
        workspace.reads("build/r/x.r")
    );

    workspace.write("lib/pkg/Bar.r", "stray r-code 2");
    workspace.clear_log();
    workspace.build().succeeded();

    assert_eq!(workspace.compiled(), ["x.p"]);
    assert!(
        workspace.reads("build/r/x.r").contains(&(
            "lib/pkg/Bar.r".to_string(),
            workspace.sha256("lib/pkg/Bar.r")
        )),
        "the new r-code is bound: {:?}",
        workspace.reads("build/r/x.r")
    );
}

#[test]
fn rcode_beside_a_class_unit_keeps_the_key_and_out_is_bound() {
    let workspace = Built::new();
    workspace.write("src/x.p", USER);
    workspace.write("src/pkg/Bar.cls", BAR);
    workspace.write("src/pkg/Bar.r", "stray r-code");

    workspace.build().succeeded();

    let plan = workspace.plan();
    for path in ["src/x.p", "src/pkg/Bar.cls"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
        assert_eq!(unit(&plan, path)["action"], "up-to-date", "{path}");
    }
    let reads = workspace.reads("build/r/x.r");
    assert!(
        reads
            .iter()
            .any(|(path, hash)| path.ends_with("build/r/pkg/Bar.r")
                && *hash == workspace.sha256("build/r/pkg/Bar.r")),
        "x.p binds the class unit's r-code in $OUT: {reads:?}"
    );
    assert!(
        !reads.iter().any(|(path, _)| path == "src/pkg/Bar.r"),
        "{reads:?}"
    );
}

/// Two directories with their own PROPATHs, `lib` only an include path.
fn per_directory() -> Built {
    let workspace = Built::with(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"lib\"]",
        "",
    );
    workspace.write("a/x.p", USER);
    workspace.write("b/y.p", USER);
    workspace.write("lib/pkg/Bar.cls", BAR);
    workspace
}

#[test]
fn rcode_beside_a_class_that_is_not_a_unit_takes_the_key_away_under_every_propath() {
    let workspace = per_directory();
    workspace.write("lib/pkg/Bar.r", "stray r-code");

    let plan = workspace.plan();

    assert_eq!(plan["propaths"].as_array().unwrap().len(), 2);
    for path in ["a/x.p", "b/y.p"] {
        let reasons = unkeyable(&plan, path);
        assert!(
            reasons
                .as_array()
                .unwrap()
                .contains(&json!("class-rcode: lib/pkg/Bar.r")),
            "{path}: {reasons}"
        );
    }
}

#[test]
fn rcode_in_one_unit_directory_takes_the_key_away_there() {
    let workspace = per_directory();
    workspace.write("a/pkg/Bar.r", "stray r-code");

    let plan = workspace.plan();

    let reasons = unkeyable(&plan, "a/x.p");
    assert!(
        reasons
            .as_array()
            .unwrap()
            .contains(&json!("class-rcode: a/pkg/Bar.r")),
        "{reasons}"
    );
    let reasons = unkeyable(&plan, "b/y.p");
    assert!(
        !reasons
            .as_array()
            .unwrap()
            .iter()
            .any(|r| r.as_str().unwrap().starts_with("class-rcode")),
        "b/y.p binds the source: {reasons}"
    );
}

#[test]
fn rcode_beside_a_listed_class_gives_no_observed_key() {
    let workspace = single_propath();
    workspace.write("src/enc.p", "§ ¤ 0x9f ((( %%% .\nMESSAGE \"enc\".\n");
    workspace.build().succeeded();

    let plan = workspace.plan();

    let enc = unit(&plan, "src/enc.p");
    assert_eq!(enc["key"], Value::Null, "{plan}");
    assert!(
        enc["unkeyable"].as_array().unwrap().contains(&json!(
            "observed-class: lib/pkg/Bar.cls: class-rcode: lib/pkg/Bar.r"
        )),
        "{plan}"
    );
}
