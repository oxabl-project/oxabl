//! The root of trust of the remote cache is `.oxabl/cache.toml` and nothing
//! that the environment, `oxabl.toml`, an overlay or a flag can set.

#![cfg(feature = "remote-cache")]

mod common;

use common::built::{Built, Run};
use common::cache::entries;
use common::remote::world::{UNITS, World, project};
use tempfile::TempDir;

/// A developer workspace with its own local cache, attached to the world.
fn workspace(world: &World, extra: &str) -> (Built, TempDir) {
    let local = TempDir::new().unwrap();
    let workspace = project(extra);
    world.attach(&workspace, &world.developer, "developer");
    workspace.cache_in(local.path());
    (workspace, local)
}

fn assert_refused(workspace: &Built, run: &Run, local: &TempDir, named: &str) {
    assert_eq!(
        run.code, 2,
        "stdout:\n{}\nstderr:\n{}",
        run.stdout, run.stderr
    );
    assert!(run.stderr.contains(named), "{}", run.stderr);
    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
    assert!(entries(local.path()).is_empty());
    assert!(!workspace.exists("build"));
}

#[test]
fn each_trust_variable_stops_the_build_with_exit_code_2_naming_it_before_any_cache_use() {
    let world = World::start();
    for variable in [
        "OXABL_CACHE_OWNER_KEYS",
        "OXABL_CACHE_MIN_ROSTER_VERSION",
        "OXABL_CACHE_CACHE",
        "OXABL_CACHE_TENANT",
    ] {
        let (workspace, local) = workspace(&world, "");
        workspace.env(variable, Some("1"));
        world.proxy.clear();

        let run = workspace.build();

        assert_refused(&workspace, &run, &local, variable);
        assert!(world.proxy.requests().is_empty(), "{variable}");
    }
}

#[test]
fn a_trust_key_in_oxabl_toml_is_exit_code_2() {
    let world = World::start();
    for key in ["tenant", "cache", "owner_keys", "min_roster_version"] {
        let line = format!("{key} = \"x\"\n");
        let (workspace, local) = workspace(&world, &format!("[build.cache]\n{line}"));

        let run = workspace.build();

        assert_refused(&workspace, &run, &local, key);
    }
}

#[test]
fn a_table_for_the_remote_settings_in_oxabl_toml_is_exit_code_2() {
    let world = World::start();
    let (workspace, local) = workspace(&world, "[build.cache.remote]\nowner_keys = [\"x\"]\n");

    let run = workspace.build();

    assert_refused(&workspace, &run, &local, "remote");
}

#[test]
fn a_trust_flag_is_exit_code_2() {
    let world = World::start();
    for flag in [
        "--tenant",
        "--owner-keys",
        "--min-roster-version",
        "--cache",
    ] {
        let (workspace, local) = workspace(&world, "");

        let run = workspace.build_with(&[flag, "x"]);

        assert_refused(&workspace, &run, &local, flag);
    }
}

#[test]
fn the_server_variable_changes_the_server_and_nothing_else() {
    let world = World::start();
    let ci = project("");
    world.attach(&ci, &world.ci, "ci");
    ci.build().succeeded();
    let (moved, _local) = workspace(&world, "");
    moved.write(
        ".oxabl/cache.toml",
        &world
            .cache_toml("")
            .replace(&world.proxy.url(), "http://127.0.0.1:9"),
    );
    moved.env("OXABL_CACHE_SERVER", Some(&world.proxy.url()));

    let run = moved.build();

    run.succeeded();
    assert!(moved.compiled().is_empty(), "{:?}", moved.compiled());
    let (stranded, _local) = workspace(&world, "");
    stranded.write(
        ".oxabl/cache.toml",
        &world
            .cache_toml("")
            .replace(&world.proxy.url(), "http://127.0.0.1:9"),
    );

    let run = stranded.build();

    run.succeeded();
    assert_eq!(stranded.compiled().len(), UNITS);
}

#[test]
fn a_cache_file_that_the_client_cannot_use_is_one_warning_and_a_build_without_the_remote_cache() {
    let world = World::start();
    let (workspace, _local) = workspace(&world, "");
    workspace.write(".oxabl/cache.toml", &world.cache_toml("bogus = 1\n"));

    let run = workspace.build();

    run.succeeded();
    assert_eq!(workspace.compiled().len(), UNITS);
    assert_eq!(
        run.stderr.matches("the remote cache is not used").count(),
        1,
        "{}",
        run.stderr
    );
    assert!(world.proxy.requests().is_empty());
}

#[test]
fn remote_true_without_a_cache_file_is_exit_code_2() {
    let workspace = project("[build.cache]\nremote = true\n");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(run.stderr.contains(".oxabl/cache.toml"), "{}", run.stderr);
    assert!(workspace.compiled().is_empty());
}

#[test]
fn remote_false_and_the_no_remote_cache_flag_leave_the_server_alone() {
    let world = World::start();
    let (switched_off, _local) = workspace(&world, "[build.cache]\nremote = false\n");
    let (flagged, _other) = workspace(&world, "");

    switched_off.build().succeeded();
    flagged.build_with(&["--no-remote-cache"]).succeeded();

    assert_eq!(switched_off.compiled().len(), UNITS);
    assert_eq!(flagged.compiled().len(), UNITS);
    assert!(
        world.proxy.requests().is_empty(),
        "{:?}",
        world.proxy.requests()
    );
}
