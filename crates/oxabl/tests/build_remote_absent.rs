//! A binary built without the `remote-cache` feature refuses a workspace that
//! turns the remote cache on, instead of running slowly with no signal.

#![cfg(not(feature = "remote-cache"))]

mod common;

use common::built::Built;

fn project() -> Built {
    let workspace = Built::new();
    for name in ["a", "b", "c"] {
        workspace.write(&format!("src/{name}.p"), &format!("MESSAGE \"{name}\".\n"));
    }
    workspace
}

fn assert_refused(workspace: &Built) {
    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(
        run.stderr.contains(
            "this oxabl was built without remote-cache; rebuild with --features remote-cache or pass --no-remote-cache"
        ),
        "{}",
        run.stderr
    );
    assert!(workspace.compiled().is_empty());
}

#[test]
fn a_workspace_with_a_cache_file_needs_a_binary_with_the_remote_cache() {
    let workspace = project();
    workspace.write(".oxabl/cache.toml", "server = \"https://cache.example\"\n");

    assert_refused(&workspace);
    workspace.build_with(&["--no-remote-cache"]).succeeded();
    assert_eq!(workspace.compiled().len(), 3);
}

#[test]
fn a_workspace_that_sets_remote_true_needs_a_binary_with_the_remote_cache() {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.cache]\nremote = true\n",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write(".oxabl/cache.toml", "server = \"https://cache.example\"\n");

    assert_refused(&workspace);
    workspace.build_with(&["--no-remote-cache"]).succeeded();
}

#[test]
fn a_workspace_that_switches_the_remote_cache_off_builds_with_any_binary() {
    let workspace = Built::with(
        "include_paths = [\"src\"]",
        "[build.cache]\nremote = false\n",
    );
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write(".oxabl/cache.toml", "server = \"https://cache.example\"\n");

    workspace.build().succeeded();
}
