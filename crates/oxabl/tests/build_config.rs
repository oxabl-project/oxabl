//! The configuration `oxabl build` reads: strict tables, the DLC source
//! directory and the `$DLC/` include path entries.

mod build_support;

use std::process::Command;

use build_support::{Workspace, run_of};

#[test]
fn a_misspelled_table_is_a_usage_error_that_names_it() {
    let workspace =
        Workspace::with_config("[workspace]\nname = \"t\"\n\n[biuld]\noutput = \"r\"\n");
    let run = workspace.plan();
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(run.stderr.contains("biuld"), "{}", run.stderr);
}

#[test]
fn an_unknown_key_in_the_build_table_is_a_usage_error_that_names_it() {
    let workspace =
        Workspace::with_config("[workspace]\nname = \"t\"\n\n[build]\noutptu = \"r\"\n");
    let run = workspace.plan();
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(run.stderr.contains("outptu"), "{}", run.stderr);
}

#[test]
fn an_unknown_key_in_a_build_subtable_is_a_usage_error_that_names_it() {
    for (table, key) in [
        ("build.workers", "cuont"),
        ("build.cache", "enabeld"),
        ("build.compile", "min-size"),
        ("build.outputs", "xrf"),
        ("build.verify", "read"),
    ] {
        let workspace = Workspace::with_config(&format!(
            "[workspace]\nname = \"t\"\n\n[{table}]\n{key} = 1\n"
        ));
        let run = workspace.plan();
        assert_eq!(run.code, 2, "[{table}] {key}: {}{}", run.stdout, run.stderr);
        assert!(run.stderr.contains(key), "[{table}] {key}: {}", run.stderr);
    }
}

#[test]
fn an_unknown_key_in_the_workspace_table_is_a_usage_error_that_names_it() {
    let workspace = Workspace::with_config("[workspace]\nname = \"t\"\nsorces = 1\n");
    let run = workspace.plan();
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(run.stderr.contains("sorces"), "{}", run.stderr);
}

#[test]
fn durations_and_sizes_are_an_integer_and_a_unit() {
    let valid = Workspace::with_config(
        "[workspace]\nname = \"t\"\n\n[build.workers]\nstart_timeout = \"90s\"\n\
         unit_timeout = \"10m\"\ncancel_grace = \"1h\"\n\n[build.cache]\n\
         idle_timeout = \"30s\"\nlocal_max_size = \"512MiB\"\n",
    );
    valid.plan().succeeded();

    for setting in [
        "[build.workers]\nstart_timeout = \"5x\"",
        "[build.workers]\nunit_timeout = 600",
        "[build.cache]\nlocal_max_size = \"20 gigabytes\"",
        "[build.cache]\nlocal_max_size = \"20GB\"",
    ] {
        let workspace =
            Workspace::with_config(&format!("[workspace]\nname = \"t\"\n\n{setting}\n"));
        let run = workspace.plan();
        assert_eq!(run.code, 2, "{setting}: {}{}", run.stdout, run.stderr);
    }
}

#[test]
fn a_state_directory_outside_the_root_is_a_usage_error() {
    for (key, value) in [
        ("state", "../elsewhere"),
        ("output", "/somewhere/else"),
        ("output", "../r"),
    ] {
        let workspace = Workspace::with_config(&format!(
            "[workspace]\nname = \"t\"\n\n[build]\n{key} = \"{value}\"\n"
        ));
        let run = workspace.plan();
        assert_eq!(run.code, 2, "{key} = {value}: {}{}", run.stdout, run.stderr);
        assert!(run.stderr.contains(key), "{}", run.stderr);
    }
}

#[test]
fn a_store_directory_of_its_own_holds_the_store() {
    let workspace = Workspace::with_config(
        "[workspace]\nname = \"t\"\n\n[workspace.sources]\ndirectories = [\"src\"]\n\n\
         [build]\nstore = \"elsewhere\"\n",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.plan().succeeded();
    assert!(workspace.path("elsewhere/state.db").exists());
    assert!(!workspace.state_dir().join("state.db").exists());
}

#[test]
fn a_missing_unit_directory_is_a_usage_error() {
    let workspace = Workspace::with_sources("");
    std::fs::remove_dir_all(workspace.path("src")).ok();
    let run = workspace.plan();
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(run.stderr.contains("src"), "{}", run.stderr);
}

#[test]
fn no_oxabl_toml_is_a_usage_error() {
    let dir = tempfile::tempdir().unwrap();
    let run = run_of(
        Command::new(env!("CARGO_BIN_EXE_oxabl"))
            .args(["build", "--plan"])
            .current_dir(dir.path())
            .output()
            .unwrap(),
    );
    // A directory above the temporary one can hold an oxabl.toml; then the run
    // is that workspace's and not this test's concern.
    if !run.stderr.contains("no oxabl.toml") {
        return;
    }
    assert_eq!(run.code, 2);
}

#[test]
fn build_without_plan_and_without_a_worker_command_says_what_is_missing() {
    let workspace = Workspace::with_sources("");
    let run = workspace.run(&[]);
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(
        run.stderr
            .contains("oxabl build needs a worker command; set [build.workers].command"),
        "{}",
        run.stderr
    );
}

fn dlc_workspace() -> (Workspace, tempfile::TempDir) {
    let dlc = tempfile::tempdir().unwrap();
    let workspace = Workspace::with_sources("include_paths = [\"$DLC/src\", \"inc\"]");
    std::fs::create_dir_all(dlc.path().join("src")).unwrap();
    std::fs::write(
        dlc.path().join("src/vendor.i"),
        "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n",
    )
    .unwrap();
    workspace.write("src/uses.p", "{vendor.i}\n");
    workspace.write("src/plain.p", "MESSAGE \"p\".\n");
    (workspace, dlc)
}

#[test]
fn an_include_under_the_dlc_directory_is_found_through_the_environment() {
    let (workspace, dlc) = dlc_workspace();
    let plan = |workspace: &Workspace| {
        run_of(
            workspace
                .command(&["--plan"])
                .env("OXABL_DLC_SOURCES", dlc.path())
                .output()
                .unwrap(),
        )
    };
    plan(&workspace).succeeded();

    std::fs::write(
        dlc.path().join("src/vendor.i"),
        "DEFINE VARIABLE v AS DECIMAL NO-UNDO.\n",
    )
    .unwrap();
    let run = plan(&workspace);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/uses.p dependency-changed: $DLC/src/vendor.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn an_include_under_the_dlc_directory_is_found_through_the_dlc_key() {
    let dlc = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dlc.path().join("src")).unwrap();
    std::fs::write(
        dlc.path().join("src/vendor.i"),
        "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n",
    )
    .unwrap();
    let workspace = Workspace::with_sources(&format!(
        "include_paths = [\"$DLC/src\"]\ndlc = \"{}\"",
        dlc.path().display()
    ));
    workspace.write("src/uses.p", "{vendor.i}\n");
    workspace.plan().succeeded();

    std::fs::write(
        dlc.path().join("src/vendor.i"),
        "DEFINE VARIABLE v AS DECIMAL NO-UNDO.\n",
    )
    .unwrap();
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/uses.p dependency-changed: $DLC/src/vendor.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn an_include_under_the_dlc_directory_does_not_resolve_without_one() {
    let (workspace, dlc) = dlc_workspace();
    workspace.plan().succeeded();
    std::fs::write(
        dlc.path().join("src/vendor.i"),
        "DEFINE VARIABLE v AS DECIMAL NO-UNDO.\n",
    )
    .unwrap();
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(run.rebuild(), Vec::<String>::new(), "{}", run.stdout);
}

#[test]
fn check_resolves_an_include_under_the_dlc_directory_too() {
    let (workspace, dlc) = dlc_workspace();
    let check = |with_dlc: bool| {
        let mut command = Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .args(["check", "--no-format"])
            .arg(workspace.path("src/uses.p"))
            .env_remove("OXABL_DLC_SOURCES");
        if with_dlc {
            command.env("OXABL_DLC_SOURCES", dlc.path());
        }
        run_of(command.output().unwrap())
    };
    let without = check(false);
    assert!(without.stderr.contains("PREPROC007"), "{}", without.stderr);
    let with = check(true);
    assert!(!with.stderr.contains("PREPROC007"), "{}", with.stderr);
}
