//! Each input of the compile key changes the key, except the startup
//! parameters on the drop list. The tests drive `oxabl build --plan --probe`
//! with the fake AVM and know nothing of the key module.

mod common;

use common::keyed::{Keyed, copy_workspace};
use serde_json::{Value, json};

const UNIT: &str = "src/ar/post.p";

/// The key of the example unit, after `change`, differs from the key before.
fn changes_the_key(change: impl FnOnce(&Keyed)) {
    let workspace = Keyed::example();
    let before = workspace.plan().key_of(UNIT);
    change(&workspace);
    let after = workspace.plan().key_of(UNIT);
    assert_ne!(before, after, "the key did not change");
}

/// The key stays the same after `change`.
fn keeps_the_key(change: impl FnOnce(&Keyed)) {
    let workspace = Keyed::example();
    let before = workspace.plan().key_of(UNIT);
    change(&workspace);
    let after = workspace.plan().key_of(UNIT);
    assert_eq!(before, after, "the key changed");
}

fn with_parameters(workspace: &Keyed, parameters: &[&str]) {
    workspace.edit_environment(|environment| {
        environment["parameters"] = json!(parameters);
    });
}

#[test]
fn the_unit_bytes_change_the_key() {
    changes_the_key(|w| w.write(UNIT, "{inc/ledger.i}\n{adecomm/x.i}\nMESSAGE \"posted\".\n"));
}

#[test]
fn a_closure_file_changes_the_key() {
    changes_the_key(|w| w.write("src/inc/ledger.i", "DEFINE VARIABLE x AS INT64 NO-UNDO.\n"));
}

#[test]
fn a_dlc_file_in_the_closure_changes_the_key() {
    changes_the_key(|w| {
        w.write(
            "dlc/src/adecomm/x.i",
            "DEFINE VARIABLE y AS LOGICAL NO-UNDO.\n",
        )
    });
}

#[test]
fn the_path_of_a_closure_file_changes_the_key() {
    let workspace = Keyed::new("include_paths = [\"src/a\", \"src/b\"]", "");
    workspace.write("src/a/ledger.i", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    workspace.write("src/b/keep.i", "\n");
    workspace.write("src/a/post.p", "{ledger.i}\n");
    let before = workspace.plan().key_of("src/a/post.p");

    workspace.remove("src/a/ledger.i");
    workspace.write("src/b/ledger.i", "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    let after = workspace.plan().key_of("src/a/post.p");

    assert_ne!(
        before, after,
        "an include at another path has the same bytes"
    );
}

#[test]
fn a_changed_unit_propath_changes_the_key() {
    changes_the_key(|w| {
        w.write("src/extra/x.i", "\n");
        w.reconfigure("include_paths = [\"src\", \"$DLC/src\", \"src/extra\"]", "");
    });
}

#[test]
fn the_order_of_the_unit_propath_changes_the_key() {
    changes_the_key(|w| {
        w.reconfigure("include_paths = [\"$DLC/src\", \"src\"]", "");
    });
}

#[test]
fn a_library_on_the_propath_changes_the_key_when_its_bytes_change() {
    changes_the_key(|w| w.write("dlc/tty/netlib/OpenEdge.Net.pl", "other library bytes"));
}

#[test]
fn a_library_that_no_propath_names_does_not_change_the_key() {
    keeps_the_key(|w| {
        w.write("elsewhere/acme.pl", "bytes");
    });
    let workspace = Keyed::example();
    workspace.write("elsewhere/acme.pl", "bytes");
    workspace.reconfigure(
        "include_paths = [\"src\", \"$DLC/src\"]",
        "[build.libraries]\nacme = \"elsewhere/acme.pl\"",
    );
    let before = workspace.plan().key_of(UNIT);
    workspace.write("elsewhere/acme.pl", "other bytes");
    assert_eq!(before, workspace.plan().key_of(UNIT));
}

#[test]
fn the_openedge_version_changes_the_key() {
    changes_the_key(|w| {
        w.edit_environment(|e| e["env"]["dlc_version"] = json!("OpenEdge Release 12.8.10"));
    });
}

#[test]
fn the_dlc_version_ignores_trailing_white_space() {
    keeps_the_key(|w| {
        w.edit_environment(|e| e["env"]["dlc_version"] = json!("OpenEdge Release 12.8.9   "));
    });
}

#[test]
fn the_platform_changes_the_key() {
    changes_the_key(|w| {
        w.edit_environment(|e| e["env"]["opsys"] = json!("WIN32"));
        w.preprocessor_line("opsys = \"WIN32\"");
    });
}

#[test]
fn the_session_changes_the_key() {
    changes_the_key(|w| w.edit_environment(|e| e["env"]["cpcoll"] = json!("BASIC")));
    changes_the_key(|w| w.edit_environment(|e| e["env"]["numeric_separator"] = json!(".")));
}

#[test]
fn a_startup_parameter_that_stays_changes_the_key() {
    changes_the_key(|w| {
        with_parameters(
            w,
            &[
                "-cpinternal UTF-8",
                "-cpstream UTF-8",
                "-d mdy",
                "-inp 32001",
            ],
        );
    });
    changes_the_key(|w| {
        with_parameters(
            w,
            &[
                "-cpinternal UTF-8",
                "-cpstream UTF-8",
                "-d mdy",
                "-inp 32000",
                "-mmax 8192",
            ],
        );
    });
}

#[test]
fn a_dropped_startup_parameter_does_not_change_the_key() {
    keeps_the_key(|w| {
        with_parameters(
            w,
            &[
                "-cpinternal UTF-8",
                "-cpstream UTF-8",
                "-d mdy",
                "-inp 32000",
                "-U someone",
                "-P secret",
                "-db /data/ledger",
                "-H db.example",
                "-S 5000",
                "-clientlog /tmp/log",
                "-b",
                "-q",
            ],
        );
    });
}

#[test]
fn startup_parameters_count_by_name_and_value_not_by_order_or_repeats() {
    keeps_the_key(|w| {
        with_parameters(
            w,
            &[
                "-inp 1",
                "-d mdy",
                "-cpstream UTF-8",
                "-inp 32000",
                "-cpinternal UTF-8",
            ],
        );
    });
}

#[test]
fn a_file_valued_startup_parameter_counts_by_the_bytes_of_the_file() {
    let workspace = Keyed::example();
    workspace.write("options.txt", "first");
    workspace.edit_environment(|e| {
        e["parameters"] = json!([
            "-cpinternal UTF-8",
            "-cpstream UTF-8",
            "-compileroptionsfile options.txt"
        ]);
    });
    let before = workspace.plan().key_of(UNIT);
    workspace.write("options.txt", "second");
    let after = workspace.plan().key_of(UNIT);
    assert_ne!(before, after);
}

#[test]
fn the_database_list_changes_the_key() {
    changes_the_key(|w| {
        w.edit_environment(|e| e["databases"][0]["aliases"] = json!(["dictdb", "other"]));
    });
    changes_the_key(|w| {
        w.edit_environment(|e| e["databases"][0]["dbtype"] = json!("ORACLE"));
        w.preprocessor_line("database-types = { ledger = \"ORACLE\" }");
    });
}

#[test]
fn the_alias_order_and_case_of_a_database_do_not_change_the_key() {
    let workspace = Keyed::example();
    workspace.edit_environment(|e| e["databases"][0]["aliases"] = json!(["B", "a"]));
    let before = workspace.plan().key_of(UNIT);
    workspace.edit_environment(|e| e["databases"][0]["aliases"] = json!(["A", "b"]));
    assert_eq!(before, workspace.plan().key_of(UNIT));
}

#[test]
fn the_schema_image_changes_the_key() {
    changes_the_key(|w| {
        w.edit_environment(|e| {
            e["databases"][0]["schema"][2][7] = json!("_label=Customer Number");
        });
    });
}

#[test]
fn a_database_added_changes_the_key() {
    changes_the_key(|w| {
        w.edit_environment(|e| {
            let list = e["databases"].as_array_mut().unwrap();
            list.push(json!({ "ldb": "audit", "schema": [["db", "_db-type=PROGRESS"]] }));
        });
        w.set_preprocessor("databases = [\"ledger\", \"audit\"]");
    });
}

fn compile_option(line: &str) -> impl FnOnce(&Keyed) + '_ {
    move |workspace| {
        workspace.reconfigure(
            "include_paths = [\"src\", \"$DLC/src\"]",
            &format!("[build.compile]\n{line}"),
        );
    }
}

#[test]
fn each_compile_option_changes_the_key() {
    for line in [
        "min_size = true",
        "generate_md5 = true",
        "stream_io = true",
        "v6frame = true",
        "attr_space = true",
        "languages = [\"fr\"]",
        "options = [\"require-full-names\"]",
    ] {
        let workspace = Keyed::example();
        let before = workspace.plan().key_of(UNIT);
        compile_option(line)(&workspace);
        assert_ne!(before, workspace.plan().key_of(UNIT), "{line}");
    }
}

#[test]
fn the_order_of_languages_changes_the_key_but_the_order_and_case_of_options_do_not() {
    let workspace = Keyed::example();
    compile_option("languages = [\"fr\", \"de\"]")(&workspace);
    let french_first = workspace.plan().key_of(UNIT);
    compile_option("languages = [\"de\", \"fr\"]")(&workspace);
    assert_ne!(french_first, workspace.plan().key_of(UNIT));

    compile_option("options = [\"Require-Full-Names\", \"require-field-qualifiers\"]")(&workspace);
    let one = workspace.plan().key_of(UNIT);
    compile_option(
        "options = [\"require-field-qualifiers\", \"require-full-names\", \"REQUIRE-FULL-NAMES\"]",
    )(&workspace);
    assert_eq!(one, workspace.plan().key_of(UNIT));
}

#[test]
fn each_requested_output_changes_the_key() {
    for table in [
        "xref = \"build/xref\"",
        "debug_list = \"build/dbg\"",
        "listing = \"build/lst\"",
    ] {
        let workspace = Keyed::example();
        let before = workspace.plan().key_of(UNIT);
        workspace.reconfigure(
            "include_paths = [\"src\", \"$DLC/src\"]",
            &format!("[build.outputs]\n{table}"),
        );
        assert_ne!(before, workspace.plan().key_of(UNIT), "{table}");
    }
    let workspace = Keyed::example();
    workspace.reconfigure(
        "include_paths = [\"src\", \"$DLC/src\"]",
        "[build.outputs]\nxref = \"build/xref\"\nxref_format = \"xml\"",
    );
    let xml = workspace.plan().key_of(UNIT);
    workspace.reconfigure(
        "include_paths = [\"src\", \"$DLC/src\"]",
        "[build.outputs]\nxref = \"build/xref\"\nxref_format = \"text\"",
    );
    assert_ne!(xml, workspace.plan().key_of(UNIT));
}

#[test]
fn the_output_directories_do_not_change_the_key() {
    let workspace = Keyed::example();
    workspace.reconfigure(
        "include_paths = [\"src\", \"$DLC/src\"]",
        "[build]\noutput = \"build/r\"\n[build.outputs]\nxref = \"build/xref\"",
    );
    let before = workspace.plan().key_of(UNIT);
    workspace.reconfigure(
        "include_paths = [\"src\", \"$DLC/src\"]",
        "[build]\noutput = \"elsewhere/r\"\n[build.outputs]\nxref = \"elsewhere/xref\"",
    );
    assert_eq!(before, workspace.plan().key_of(UNIT));
}

#[test]
fn an_edit_to_another_unit_does_not_change_the_key() {
    keeps_the_key(|w| w.write("src/other.p", "MESSAGE \"other\".\n"));
}

#[test]
fn a_workspace_copied_to_another_path_keeps_every_key() {
    let workspace = Keyed::example();
    workspace.write("src/ar/other.p", "{inc/ledger.i}\n");
    workspace.write("src/pkg/Foo.cls", "CLASS pkg.Foo:\nEND CLASS.\n");
    workspace.reconfigure(
        "include_paths = [\"src\", \"$DLC/src\"]\nroot_relative_include_paths = []",
        "",
    );
    let original = workspace.plan();
    let copy = copy_workspace(workspace.root());

    let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_oxabl"));
    command
        .current_dir(copy.path())
        .args(["build", "--plan", "--probe", "--json", "--all"]);
    let output = common::run(command);
    assert_eq!(output.status.code(), Some(0), "{}", common::stderr(&output));
    let copied: Value = serde_json::from_str(&common::stdout(&output)).unwrap();

    let keys = |document: &Value| -> Vec<(String, Value)> {
        document["units"]
            .as_array()
            .unwrap()
            .iter()
            .map(|u| (u["path"].as_str().unwrap().to_string(), u["key"].clone()))
            .collect()
    };
    let before = keys(&original.document);
    assert!(before.iter().filter(|(_, key)| !key.is_null()).count() >= 2);
    assert_eq!(before, keys(&copied));
    assert_eq!(
        original.document["environment"]["identity"],
        copied["environment"]["identity"]
    );
}

#[test]
fn the_key_does_not_depend_on_the_order_in_which_the_store_learned_the_files() {
    let write = |workspace: &Keyed, unit: &str, includes: &[&str]| {
        for include in includes {
            workspace.write(
                &format!("src/{include}.i"),
                &format!("MESSAGE \"{include}\".\n"),
            );
        }
        workspace.write("src/a.p", unit);
    };
    let fresh = Keyed::simple();
    write(&fresh, "{z.i}\n{m.i}\n{b.i}\n", &["z", "m", "b"]);
    let expected = fresh.plan().key_of("src/a.p");

    let grown = Keyed::simple();
    write(&grown, "{z.i}\n{m.i}\n", &["z", "m"]);
    grown.plan();
    write(&grown, "{z.i}\n{m.i}\n{b.i}\n", &["b"]);

    assert_eq!(expected, grown.plan().key_of("src/a.p"));
}
