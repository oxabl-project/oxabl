//! The fail-closed rules of the compile key: a unit that cannot be keyed has
//! `key: null` and each reason in `unkeyable`; a unit that cannot compile
//! where it is has the action `skip`. The tests drive `oxabl build --plan
//! --probe` with the fake AVM.

mod common;

use common::keyed::Keyed;
use common::{run, stderr, stdout};
use serde_json::json;

fn assert_unkeyable(plan: &common::keyed::Plan, unit: &str, reasons: &[&str]) {
    assert_eq!(plan.key(unit), None, "{unit} has a key");
    let listed = plan.unkeyable(unit);
    for reason in reasons {
        assert!(
            listed.iter().any(|l| l == reason),
            "{unit}: no `{reason}` in {listed:?}"
        );
    }
}

#[test]
fn a_degraded_edge_set_gives_no_key_k1() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "DEFINE VARIABLE x AS .\nMESSAGE \"a\".\n");
    workspace.write("src/ok.p", "MESSAGE \"ok\".\n");

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/a.p", &["degraded: src/a.p: parse-recovered"]);
    assert!(plan.key("src/ok.p").is_some());
}

#[test]
fn a_degraded_include_in_the_closure_gives_no_key_k1() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "{inc/b.i}\n");
    workspace.write("src/ok.p", "{inc/c.i}\n");
    workspace.write("src/inc/c.i", "MESSAGE \"c\".\n");
    workspace.write("src/inc/b.i", "{inc/c.i}\nDEFINE VARIABLE x AS .\n");

    let plan = workspace.plan();

    assert_eq!(plan.key("src/a.p"), None, "{}", plan.unit("src/a.p"));
    assert!(plan.key("src/ok.p").is_some());
}

#[test]
fn an_unresolved_include_gives_no_key_k2() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "{inc/b.i}\nMESSAGE \"a\".\n");
    workspace.write("src/inc/b.i", "{missing.i}\n");

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &["unresolved-include: missing.i in src/a.p"],
    );
}

#[test]
fn an_unresolved_conditional_include_gives_no_key_k2() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/a.p",
        "&IF DEFINED(neverdefined) &THEN\n{missing.i}\n&ENDIF\nMESSAGE \"a\".\n",
    );

    let plan = workspace.plan();

    assert_eq!(plan.key("src/a.p"), None, "{}", plan.unit("src/a.p"));
}

#[test]
fn an_unresolved_class_gives_no_key_k3() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/a.p",
        "DEFINE VARIABLE o AS Acme.Missing NO-UNDO.\no = NEW Acme.Missing().\n",
    );

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &["unresolved-class: acme.missing in src/a.p"],
    );
}

#[test]
fn a_computed_name_in_a_conditional_include_gives_no_key_k4() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/a.p",
        "&IF DEFINED(neverdefined) &THEN\n{{&prefix}.i}\n&ENDIF\nMESSAGE \"a\".\n",
    );

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/a.p", &["computed-name: {&prefix}.i in src/a.p"]);
}

#[test]
fn a_computed_include_name_in_the_code_gives_no_key() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "{{&prefix}.i}\nMESSAGE \"a\".\n");

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/a.p", &["unresolved-include: .i in src/a.p"]);
}

#[test]
fn a_class_whose_statement_comes_from_an_include_gives_no_key() {
    let workspace = Keyed::simple();
    workspace.write("src/inc/head.i", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("src/Foo.cls", "{inc/head.i}\n");

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/Foo.cls",
        &["computed-name: class statement in src/Foo.cls"],
    );
}

#[test]
fn a_class_statement_after_an_endif_gets_its_declared_name() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/pkg/Price.cls",
        "&IF DEFINED(order) = 0 &THEN\n  &SCOPED-DEFINE order web-order\n&ENDIF\n\nCLASS pkg.Price:\nEND CLASS.\n",
    );

    let plan = workspace.plan();

    assert_eq!(
        plan.unit("src/pkg/Price.cls")["compile_name"],
        "pkg/Price.cls"
    );
    assert!(
        plan.key("src/pkg/Price.cls").is_some(),
        "{}",
        plan.unit("src/pkg/Price.cls")
    );
}

#[test]
fn a_type_statement_after_a_directive_gets_its_declared_name() {
    let workspace = Keyed::simple();
    let sources = [
        (
            "src/pkg/Api.cls",
            "&SCOPED-DEFINE x true\nINTERFACE pkg.Api:\nEND INTERFACE.\n",
        ),
        (
            "src/pkg/Color.cls",
            "&UNDEFINE x\nENUM pkg.Color:\nEND ENUM.\n",
        ),
        (
            "src/pkg/Base.cls",
            "&MESSAGE hi\nFINAL CLASS pkg.Base:\nEND CLASS.\n",
        ),
        (
            "src/pkg/Lead.cls",
            "&IF 1 = 1 &THEN\n&ENDIF CLASS pkg.Lead:\nEND CLASS.\n",
        ),
    ];
    for (path, body) in sources {
        workspace.write(path, body);
    }

    let plan = workspace.plan();

    for (path, _) in sources {
        assert!(plan.key(path).is_some(), "{}", plan.unit(path));
    }
}

#[test]
fn a_class_name_chosen_by_a_condition_gives_no_key() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/pkg/Order.cls",
        "&IF DEFINED(web_variant) = 0 &THEN\nCLASS pkg.Order:\n&ELSE\nCLASS pkg.WebOrder:\n&ENDIF\nEND CLASS.\n",
    );
    workspace.write(
        "src/pkg/Only.cls",
        "&IF DEFINED(web_variant) = 0 &THEN\nCLASS pkg.Only:\n&ENDIF\nEND CLASS.\n",
    );
    workspace.write(
        "src/pkg/Part.cls",
        "CLASS pkg.&IF DEFINED(web_variant) = 0 &THEN Part &ELSE WebPart &ENDIF:\nEND CLASS.\n",
    );

    let plan = workspace.plan();

    for unit in ["Order", "Only", "Part"] {
        let path = format!("src/pkg/{unit}.cls");
        assert_unkeyable(
            &plan,
            &path,
            &[&format!("conditional-name: class statement in {path}")],
        );
    }
}

#[test]
fn a_closure_file_outside_the_workspace_gives_no_key_k5() {
    let outside = tempfile::TempDir::new().unwrap();
    let external = outside.path().join("ext.i");
    std::fs::write(&external, "MESSAGE \"ext\".\n").unwrap();
    let workspace = Keyed::simple();
    workspace.write(
        "src/a.p",
        &format!("{{{}}}\nMESSAGE \"a\".\n", external.display()),
    );

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &[&format!("outside-workspace: {}", external.display())],
    );
}

#[test]
fn a_unit_that_cannot_be_read_gives_no_key_k5() {
    use std::os::unix::fs::PermissionsExt;
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/ok.p", "MESSAGE \"ok\".\n");
    let path = workspace.path("src/a.p");
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o000)).unwrap();
    if std::fs::read(&path).is_ok() {
        return;
    }

    let plan = workspace.plan();

    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();
    assert_unkeyable(&plan, "src/a.p", &["unreadable: src/a.p"]);
    assert!(plan.key("src/ok.p").is_some());
}

#[test]
fn without_a_probe_no_unit_has_a_key_and_the_reason_is_environment_unknown_k6() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let plan = workspace.plan_without_probe();

    assert_eq!(plan.key("src/a.p"), None);
    assert_eq!(plan.unkeyable("src/a.p"), ["environment-unknown"]);
    assert_eq!(plan.document["environment"]["source"], "none");
    assert_eq!(plan.document["summary"]["unkeyable"], 0);
}

#[test]
fn a_startup_parameter_that_names_a_directory_makes_every_unit_unkeyable_k6() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("assemblies/readme.txt", "x");
    workspace.edit_environment(|e| e["parameters"] = json!(["-assemblies assemblies"]));

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/a.p", &["unhashable-parameter: -assemblies"]);
}

#[test]
fn a_library_entry_that_is_a_directory_makes_its_users_unkeyable_k6() {
    let workspace = Keyed::new(
        "include_paths = [\"src\", \"$LIB:acme\"]",
        "[build.libraries]\nacme = \"acme-r-code\"",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("acme-r-code/x.r", "r-code");

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/a.p", &["unhashable-library: $LIB:acme"]);
}

#[test]
fn a_library_directory_stays_unkeyable_when_a_package_prefix_maps_to_it_k6() {
    let workspace = Keyed::new(
        "include_paths = [\"src\", \"$LIB:acme\"]",
        "[build.libraries]\nacme = \"acme-r-code\"\n[build.external_packages]\n\"Acme.Util.\" = [\"$LIB:acme\"]",
    );
    workspace.write("acme-r-code/x.r", "r-code");
    workspace.write(
        "src/a.p",
        "DEFINE VARIABLE o AS Acme.Util.Thing NO-UNDO.\no = NEW Acme.Util.Thing().\n",
    );

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &[
            "unhashable-library: $LIB:acme",
            "unresolved-class: acme.util.thing in src/a.p",
        ],
    );
}

#[test]
fn a_dlc_include_that_differs_from_the_worker_copy_makes_its_users_unkeyable_k6() {
    let workspace = Keyed::example();
    workspace.write(
        "worker-dlc/src/adecomm/x.i",
        "DEFINE VARIABLE other AS INTEGER NO-UNDO.\n",
    );
    workspace.write("worker-dlc/tty/netlib/OpenEdge.Net.pl", "library bytes");
    workspace.edit_environment(|e| e["dlc_dir"] = json!("worker-dlc"));

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/ar/post.p",
        &["dlc-mirror-mismatch: $DLC/src/adecomm/x.i"],
    );
}

#[test]
fn a_dlc_include_that_both_copies_change_together_changes_the_key() {
    let workspace = Keyed::example();
    workspace.write(
        "worker-dlc/src/adecomm/x.i",
        "DEFINE VARIABLE y AS CHARACTER NO-UNDO.\n",
    );
    workspace.write("worker-dlc/tty/netlib/OpenEdge.Net.pl", "library bytes");
    let host_key = workspace.plan().key_of("src/ar/post.p");

    workspace.edit_environment(|e| e["dlc_dir"] = json!("worker-dlc"));
    workspace.write(
        "dlc/src/adecomm/x.i",
        "DEFINE VARIABLE y AS CHARACTER NO-UNDO.\n",
    );
    let agreed = workspace.plan().key_of("src/ar/post.p");
    assert_eq!(host_key, agreed, "equal copies, equal bytes, equal key");

    let changed = "DEFINE VARIABLE y AS LOGICAL NO-UNDO.\n";
    workspace.write("worker-dlc/src/adecomm/x.i", changed);
    workspace.write("dlc/src/adecomm/x.i", changed);
    assert_ne!(agreed, workspace.plan().key_of("src/ar/post.p"));
}

#[test]
fn a_unit_that_no_propath_entry_holds_is_skipped_k7() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("src/elsewhere/b.p", "MESSAGE \"b\".\n");
    workspace.reconfigure("include_paths = [\"src/other\"]", "");
    workspace.write("src/other/c.p", "MESSAGE \"c\".\n");

    let plan = workspace.plan();

    assert_eq!(plan.action("src/a.p"), "skip");
    assert_unkeyable(&plan, "src/a.p", &["not-on-propath"]);
    assert_eq!(plan.unit("src/a.p")["compile_name"], json!(null));
    assert_eq!(plan.action("src/other/c.p"), "rebuild");
    assert_eq!(plan.unit("src/other/c.p")["compile_name"], "c.p");
}

#[test]
fn a_unit_that_an_earlier_entry_shadows_is_skipped_k7() {
    let workspace = Keyed::new("include_paths = [\"src/other\", \"src\"]", "");
    workspace.write("src/ar/post.p", "MESSAGE \"a\".\n");
    workspace.write("src/other/ar/post.p", "MESSAGE \"b\".\n");

    let plan = workspace.plan();

    assert_eq!(plan.action("src/ar/post.p"), "skip");
    assert_unkeyable(
        &plan,
        "src/ar/post.p",
        &["shadowed-by: src/other/ar/post.p"],
    );
    assert_eq!(
        plan.unit("src/other/ar/post.p")["compile_name"],
        "ar/post.p"
    );
    assert!(plan.key("src/other/ar/post.p").is_some());
}

#[test]
fn two_units_with_one_output_collide_and_the_smaller_path_compiles_k7() {
    let workspace = Keyed::simple();
    workspace.write("src/x.p", "MESSAGE \"p\".\n");
    workspace.write("src/x.w", "MESSAGE \"w\".\n");
    workspace.write("src/ar/Y.p", "MESSAGE \"y\".\n");
    workspace.write("src/ar/y.w", "MESSAGE \"w\".\n");

    let plan = workspace.plan();

    assert!(plan.key("src/x.p").is_some());
    assert_eq!(plan.action("src/x.w"), "skip");
    assert_unkeyable(&plan, "src/x.w", &["output-collision: src/x.p"]);
    assert_unkeyable(&plan, "src/ar/y.w", &["output-collision: src/ar/Y.p"]);
    assert_eq!(plan.document["summary"]["skipped"], 2);
}

#[test]
fn a_class_in_a_directory_that_is_not_its_package_is_skipped_k7() {
    let workspace = Keyed::simple();
    workspace.write("src/x/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");

    let plan = workspace.plan();

    assert_eq!(plan.action("src/x/Foo.cls"), "skip");
    assert_unkeyable(&plan, "src/x/Foo.cls", &["package-mismatch: pkg.Foo"]);
}

#[test]
fn a_class_whose_package_root_is_not_on_the_propath_is_skipped_k7() {
    let workspace = Keyed::simple();
    workspace.write("src/a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");

    let plan = workspace.plan();

    assert_eq!(plan.action("src/a/pkg/Foo.cls"), "skip");
    assert_unkeyable(
        &plan,
        "src/a/pkg/Foo.cls",
        &["package-root-not-on-propath: src/a"],
    );
}

#[test]
fn a_class_with_its_package_root_on_the_propath_compiles_with_the_package_path() {
    let workspace = Keyed::new("include_paths = [\"src/a\"]", "");
    workspace.write("src/a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");

    let plan = workspace.plan();

    assert_eq!(
        plan.unit("src/a/pkg/Foo.cls")["compile_name"],
        "pkg/Foo.cls"
    );
    assert!(plan.key("src/a/pkg/Foo.cls").is_some());
}

const DLC_PREFIX: &str = "[build.external_packages]\n\"Acme.Dlc.\" = [\"$DLC\"]";
const DLC_CLASS_USER: &str =
    "DEFINE VARIABLE o AS Acme.Dlc.String NO-UNDO.\no = NEW Acme.Dlc.String(\"x\").\n";

#[test]
fn a_prefix_mapped_to_dlc_is_accepted_only_through_a_hashed_library() {
    // No library on the PROPATH.
    let workspace = Keyed::new("include_paths = [\"src\"]", DLC_PREFIX);
    workspace.write("src/a.p", DLC_CLASS_USER);
    let without = workspace.plan();
    assert_unkeyable(
        &without,
        "src/a.p",
        &["unresolved-class: acme.dlc.string in src/a.p"],
    );

    // The installation's library is on the worker's own PROPATH.
    workspace.write("dlc/Acme.Dlc.pl", "dlc library");
    workspace.edit_environment(|e| e["env"]["propath"] = json!("/usr/dlc/Acme.Dlc.pl"));
    let with = workspace.plan();
    let key = with.key_of("src/a.p");
    assert!(with.unkeyable("src/a.p").is_empty());

    // Its bytes are in the key.
    workspace.write("dlc/Acme.Dlc.pl", "another dlc library");
    assert_ne!(key, workspace.plan().key_of("src/a.p"));
}

#[test]
fn a_dlc_directory_on_the_propath_does_not_supply_a_prefix_mapped_to_dlc() {
    let workspace = Keyed::new("include_paths = [\"src\"]", DLC_PREFIX);
    workspace.write("src/a.p", DLC_CLASS_USER);
    workspace.write("dlc/tty/readme", "a directory");
    workspace.edit_environment(|e| e["env"]["propath"] = json!("/usr/dlc/tty"));

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &["unresolved-class: acme.dlc.string in src/a.p"],
    );
    assert_eq!(
        plan.propath("src/a.p").last().unwrap(),
        &("$DLC/tty".to_string(), "dir".to_string())
    );
}

#[test]
fn a_dlc_library_in_include_paths_supplies_a_prefix_mapped_to_dlc() {
    let workspace = Keyed::new(
        "include_paths = [\"src\", \"$DLC/tty/acme.pl\"]",
        DLC_PREFIX,
    );
    workspace.write("src/a.p", DLC_CLASS_USER);
    workspace.write("dlc/tty/acme.pl", "acme dlc library");

    let plan = workspace.plan();

    assert!(plan.key("src/a.p").is_some(), "{}", plan.unit("src/a.p"));
}

#[test]
fn a_workspace_library_supplies_its_own_prefix() {
    let workspace = Keyed::new(
        "include_paths = [\"src\", \"$LIB:acme\"]",
        "[build.libraries]\nacme = \"libs/acme.pl\"\n[build.external_packages]\n\"Acme.Util.\" = [\"$LIB:acme\"]",
    );
    workspace.write("libs/acme.pl", "acme bytes");
    workspace.write(
        "src/a.p",
        "DEFINE VARIABLE o AS Acme.Util.Thing NO-UNDO.\no = NEW Acme.Util.Thing().\n",
    );
    let plan = workspace.plan();
    let key = plan.key_of("src/a.p");
    assert!(plan.propath("src/a.p").contains(&(
        "$LIB:acme".to_string(),
        format!("sha256:{}", sha256_hex(b"acme bytes"))
    )));

    workspace.write("libs/acme.pl", "changed acme bytes");
    assert_ne!(key, workspace.plan().key_of("src/a.p"));
}

fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    Sha256::digest(bytes)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

#[test]
fn a_prefix_not_listed_for_any_library_is_never_trusted_by_name() {
    let workspace = Keyed::new(
        "include_paths = [\"src\", \"$LIB:acme\"]",
        "[build.libraries]\nacme = \"libs/acme.pl\"",
    );
    workspace.write("libs/acme.pl", "acme bytes");
    workspace.write(
        "src/a.p",
        "DEFINE VARIABLE o AS Acme.Util.Thing NO-UNDO.\no = NEW Acme.Util.Thing().\n",
    );

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &["unresolved-class: acme.util.thing in src/a.p"],
    );
}

#[test]
fn a_library_changes_only_the_keys_of_the_units_that_have_it_on_their_propath() {
    let workspace = Keyed::new(
        "include_paths = []\nroot_relative_include_paths = [\".\", \"libs/acme.pl\"]",
        "",
    );
    workspace.write("src/a/one.p", "MESSAGE \"1\".\n");
    workspace.write("src/b/two.p", "MESSAGE \"2\".\n");
    workspace.write("src/a/libs/acme.pl", "acme bytes");
    let before = workspace.plan();

    workspace.write("src/a/libs/acme.pl", "changed bytes");
    let after = workspace.plan();

    assert_ne!(before.key_of("src/a/one.p"), after.key_of("src/a/one.p"));
    assert_eq!(before.key_of("src/b/two.p"), after.key_of("src/b/two.p"));
}

#[test]
fn the_dlc_default_entries_are_on_every_unit_propath() {
    let workspace = Keyed::new(
        "include_paths = []\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.write("src/a/one.p", "MESSAGE \"1\".\n");
    workspace.write("src/b/two.p", "MESSAGE \"2\".\n");
    workspace.write("dlc/OpenEdge.Core.pl", "core");
    workspace.write("dlc/netlib/OpenEdge.Net.pl", "net");
    workspace.edit_environment(|e| {
        e["env"]["propath"] =
            json!("/usr/dlc/OpenEdge.Core.pl,/opt/other/stuff.pl,/usr/dlc/netlib/OpenEdge.Net.pl");
    });

    let plan = workspace.plan();

    let entries = |path: &str| -> Vec<String> {
        plan.propath(path)
            .into_iter()
            .map(|(entry, _)| entry)
            .collect()
    };
    assert_eq!(
        entries("src/a/one.p"),
        [
            "$OUT",
            "src/a",
            "$DLC/OpenEdge.Core.pl",
            "$DLC/netlib/OpenEdge.Net.pl"
        ]
    );
    assert_eq!(
        entries("src/b/two.p"),
        [
            "$OUT",
            "src/b",
            "$DLC/OpenEdge.Core.pl",
            "$DLC/netlib/OpenEdge.Net.pl"
        ]
    );
    assert!(
        plan.stderr.contains("/opt/other/stuff.pl"),
        "the dropped entry is named: {}",
        plan.stderr
    );
}

#[test]
fn units_with_different_propaths_that_reach_a_class_are_unkeyable_k9() {
    let workspace = Keyed::new(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.write(
        "a/x.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
    );
    workspace.write(
        "b/y.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
    );
    workspace.write("a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("b/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("a/plain.p", "MESSAGE \"plain\".\n");

    let plan = workspace.plan();

    for unit in ["a/x.p", "b/y.p", "a/pkg/Foo.cls"] {
        assert_unkeyable(&plan, unit, &["per-unit-propath-classes"]);
    }
    assert!(plan.key("a/plain.p").is_some());
    assert_eq!(plan.action("b/pkg/Foo.cls"), "skip");
    assert_unkeyable(&plan, "b/pkg/Foo.cls", &["output-collision: a/pkg/Foo.cls"]);
    assert!(
        plan.stderr.contains("3 units"),
        "one warning counts the units: {}",
        plan.stderr
    );
}

#[test]
fn with_one_propath_the_same_classes_are_keyable() {
    let workspace = Keyed::new("include_paths = [\"a\"]\ndirectories = [\"a\"]", "");
    workspace.write(
        "a/x.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
    );
    workspace.write("a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");

    let plan = workspace.plan();

    assert!(plan.key("a/x.p").is_some(), "{}", plan.unit("a/x.p"));
    assert!(plan.key("a/pkg/Foo.cls").is_some());
    assert!(!plan.stderr.contains("warning"), "{}", plan.stderr);
}

#[test]
fn a_k9_unit_stays_unkeyable_when_only_another_unit_changes() {
    let workspace = Keyed::new(
        "directories = [\"a\", \"b\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.write(
        "a/x.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
    );
    workspace.write(
        "b/y.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
    );
    workspace.write("a/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.write("b/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.plan();

    workspace.write(
        "b/y.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\nMESSAGE \"edit\".\n",
    );
    let plan = workspace.plan();

    assert_unkeyable(&plan, "b/y.p", &["per-unit-propath-classes"]);
    assert_unkeyable(&plan, "a/x.p", &["per-unit-propath-classes"]);
}

fn acme_workspace(with_class: bool) -> Keyed {
    let workspace = Keyed::new(
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
fn an_external_name_that_a_workspace_class_also_has_is_a_class_conflict_k8() {
    let workspace = acme_workspace(true);

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/q.p", &["class-conflict: Acme.Util.X"]);
    assert!(plan.key("src/Acme/Util/X.cls").is_some());
    assert!(
        plan.stderr.contains("class Acme.Util.X is accepted as external ($LIB:acme) but src/Acme/Util/X.cls also has that name; 1 units that use it are compiled without a key"),
        "{}",
        plan.stderr
    );
}

#[test]
fn without_the_workspace_class_the_external_name_is_keyable() {
    let workspace = acme_workspace(false);

    let plan = workspace.plan();

    assert!(plan.key("src/q.p").is_some(), "{}", plan.unit("src/q.p"));
    assert!(!plan.stderr.contains("warning"), "{}", plan.stderr);
}

#[test]
fn a_class_conflict_stays_when_only_the_user_changes() {
    let workspace = acme_workspace(true);
    workspace.plan();
    workspace.write(
        "src/q.p",
        "DEFINE VARIABLE o AS acme.util.x NO-UNDO.\no = NEW acme.util.x().\nMESSAGE \"edit\".\n",
    );

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/q.p", &["class-conflict: Acme.Util.X"]);
}

#[test]
fn unkeyable_units_are_in_the_rebuild_set_at_every_plan() {
    let workspace = Keyed::simple();
    workspace.write("src/ok.p", "MESSAGE \"ok\".\n");
    workspace.write("src/bad.p", "{missing.i}\n");
    workspace.plan();

    let plan = workspace.plan();

    assert_eq!(plan.action("src/ok.p"), "up-to-date");
    assert_eq!(plan.action("src/bad.p"), "rebuild");
    let reasons = plan.unit("src/bad.p")["reasons"]
        .as_array()
        .unwrap()
        .clone();
    assert_eq!(reasons[0]["kind"], "unkeyable");
    assert_eq!(plan.document["summary"]["unkeyable"], 1);
    assert_eq!(plan.document["summary"]["rebuild"], 1);
}

#[test]
fn the_text_plan_names_unkeyable_and_skipped_units() {
    let workspace = Keyed::simple();
    workspace.write("src/ok.p", "MESSAGE \"ok\".\n");
    workspace.write("src/bad.p", "{missing.i}\n");
    workspace.write("src/x.p", "MESSAGE \"p\".\n");
    workspace.write("src/x.w", "MESSAGE \"w\".\n");
    workspace.plan();

    let mut text = std::process::Command::new(env!("CARGO_BIN_EXE_oxabl"));
    text.current_dir(workspace.root())
        .args(["build", "--plan", "--probe"]);
    let output = run(text);

    let out = stdout(&output);
    assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
    assert!(
        out.contains("unresolved-include: missing.i in src/bad.p"),
        "{out}"
    );
    assert!(out.contains("output-collision: src/x.p"), "{out}");
    assert!(out.contains("(1 unkeyable)"), "{out}");
    assert!(out.contains("1 skipped"), "{out}");
}

#[test]
fn an_include_path_without_a_portable_form_is_a_configuration_error() {
    let outside = tempfile::TempDir::new().unwrap();
    let workspace = Keyed::new(
        &format!("include_paths = [\"src\", {:?}]", outside.path()),
        "",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let output = run(workspace.command(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("portable form"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn a_library_entry_without_a_configured_library_is_a_configuration_error() {
    let workspace = Keyed::new("include_paths = [\"src\", \"$LIB:nope\"]", "");
    workspace.write("src/a.p", "MESSAGE \"a\".\n");

    let output = run(workspace.command(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(stderr(&output).contains("$LIB:nope"), "{}", stderr(&output));
}

#[test]
fn a_probe_that_fails_fails_the_keyed_plan() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.reconfigure("include_paths = [\"src\"]", "");
    workspace.preprocessor_line("opsys = \"WIN32\"");

    let output = run(workspace.command(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(stderr(&output).contains("opsys"), "{}", stderr(&output));
}

#[test]
fn the_workers_stop_when_the_plan_is_done() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    let log = workspace.path("fake.log");

    let mut command = workspace.command(&[]);
    command.env("FAKE_AVM_LOG", &log);
    let output = run(command);

    assert_eq!(output.status.code(), Some(0), "{}", stderr(&output));
    let pids = common::started_pids(&log);
    assert_eq!(pids.len(), 1);
    for pid in pids {
        assert!(!common::process_exists(pid), "worker {pid} is alive");
    }
}

#[test]
fn a_keyed_plan_needs_a_worker_command() {
    let workspace = Keyed::simple();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.edit_environment(|_| {});
    let config = std::fs::read_to_string(workspace.path("oxabl.toml")).unwrap();
    let without_command = config
        .lines()
        .take_while(|line| !line.starts_with("[build.workers]"))
        .collect::<Vec<_>>()
        .join("\n");
    workspace.write("oxabl.toml", &without_command);

    let output = run(workspace.command(&[]));

    assert_eq!(output.status.code(), Some(2), "{}", stderr(&output));
    assert!(
        stderr(&output).contains("needs a worker command"),
        "{}",
        stderr(&output)
    );
}

#[test]
fn the_keyed_document_adds_the_propaths_and_the_environment() {
    let workspace = Keyed::example();

    let plan = workspace.plan();

    let mut keys: Vec<&str> = plan
        .document
        .as_object()
        .unwrap()
        .keys()
        .map(String::as_str)
        .collect();
    keys.sort();
    assert_eq!(
        keys,
        [
            "environment",
            "format",
            "propaths",
            "removed",
            "store",
            "summary",
            "units",
            "unknown_files"
        ]
    );
    let environment = &plan.document["environment"];
    assert_eq!(environment["source"], "probe");
    assert_eq!(environment["identity"].as_str().unwrap().len(), 64);
    let at = environment["at"].as_str().unwrap();
    assert!(
        at.len() == 20 && at.ends_with('Z') && at.as_bytes()[10] == b'T',
        "{at}"
    );
    let unit = plan.unit("src/ar/post.p");
    assert_eq!(unit["key"].as_str().unwrap().len(), 64);
    assert_eq!(unit["compile_name"], "ar/post.p");
    assert_eq!(unit["unkeyable"], json!([]));
    assert_eq!(unit["propath"], 0);
}

#[test]
fn a_class_that_is_present_but_unusable_is_not_accepted_as_external_k3() {
    let workspace = Keyed::new(
        "include_paths = [\"src\", \"$LIB:acme\"]",
        "[build.libraries]\nacme = \"libs/acme.pl\"\n[build.external_packages]\n\"Acme.Util.\" = [\"$LIB:acme\"]",
    );
    workspace.write("libs/acme.pl", "acme bytes");
    workspace.write(
        "src/Acme/Util/Broken.cls",
        "CLASS Acme.Util.Broken:\n  DEFINE PUBLIC PROPERTY x AS .\nEND CLASS.\n",
    );
    workspace.write(
        "src/q.p",
        "DEFINE VARIABLE o AS Acme.Util.Broken NO-UNDO.\no = NEW Acme.Util.Broken().\n",
    );

    let plan = workspace.plan();

    assert_eq!(plan.key("src/q.p"), None, "{}", plan.unit("src/q.p"));
    assert_unkeyable(
        &plan,
        "src/q.p",
        &["unresolved-class: acme.util.broken in src/q.p"],
    );
}

#[test]
fn an_include_is_judged_in_its_includers_context_not_as_a_root_k1() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/lib/tt.v",
        "DEFINE TEMP-TABLE {&table} NO-UNDO LIKE customer.\n",
    );
    workspace.write("src/a.p", "{lib/tt.v &table=\"ttX\"}\nMESSAGE \"a\".\n");

    let plan = workspace.plan();

    assert!(plan.key("src/a.p").is_some(), "{}", plan.unit("src/a.p"));
    assert_unkeyable(
        &plan,
        "src/lib/tt.v",
        &["degraded: src/lib/tt.v: parse-recovered"],
    );
}

#[test]
fn a_class_that_is_degraded_on_its_own_makes_its_users_unkeyable_k1() {
    let workspace = Keyed::simple();
    workspace.write(
        "src/a.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\n",
    );
    workspace.write(
        "src/pkg/Foo.cls",
        "CLASS pkg.Foo:\n  DEFINE VARIABLE x AS .\nEND CLASS.\n",
    );

    let plan = workspace.plan();

    assert_unkeyable(
        &plan,
        "src/a.p",
        &["degraded: src/pkg/Foo.cls: parse-recovered"],
    );
}

#[test]
fn an_include_that_is_degraded_in_its_includers_context_makes_it_unkeyable_k1() {
    let workspace = Keyed::simple();
    workspace.write("src/inc/b.i", "DEFINE VARIABLE {&name} AS .\n");
    workspace.write("src/a.p", "{inc/b.i &name=x}\n");
    workspace.write("src/ok.p", "MESSAGE \"ok\".\n");

    let plan = workspace.plan();

    assert_unkeyable(&plan, "src/a.p", &["degraded: src/a.p: parse-recovered"]);
    assert!(plan.key("src/ok.p").is_some());
}
