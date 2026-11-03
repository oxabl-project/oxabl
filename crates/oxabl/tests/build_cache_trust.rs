//! Which processes the local cache trusts, and where it lives.

mod common;

use std::os::unix::fs::PermissionsExt;
use std::path::Path;

use common::built::Built;
use common::cache::entries;
use tempfile::TempDir;

fn project() -> Built {
    let workspace = Built::new();
    for name in ["a", "b", "c"] {
        workspace.write(&format!("src/{name}.p"), &format!("MESSAGE \"{name}\".\n"));
    }
    workspace
}

/// A cache that a developer build filled.
fn filled() -> TempDir {
    let cache = TempDir::new().unwrap();
    let workspace = project();
    workspace.cache_in(cache.path());
    workspace.build().succeeded();
    assert_eq!(entries(cache.path()).len(), 3);
    cache
}

fn with_env(cache: &Path, mode: Option<&str>, ci: Option<&str>) -> Built {
    let workspace = project();
    workspace.env("OXABL_BUILD_CACHE_DIR", Some(&cache.to_string_lossy()));
    workspace.env("OXABL_CACHE_MODE", mode);
    workspace.env("CI", ci);
    workspace
}

#[test]
fn with_ci_set_and_no_mode_the_shared_local_cache_is_never_read_or_written() {
    let cache = filled();
    let workspace = with_env(cache.path(), None, Some("1"));

    workspace.build().succeeded();

    assert_eq!(workspace.compiled().len(), 3, "a planted entry was read");
    assert_eq!(entries(cache.path()).len(), 3);
}

#[test]
fn in_read_only_and_ci_mode_the_shared_local_cache_is_off() {
    let cache = filled();
    for mode in ["read-only", "ci"] {
        let workspace = with_env(cache.path(), Some(mode), None);

        workspace.build().succeeded();

        assert_eq!(workspace.compiled().len(), 3, "mode {mode} read the cache");
        assert_eq!(entries(cache.path()).len(), 3, "mode {mode} wrote to it");
    }
}

#[test]
fn a_mode_beats_the_ci_variable_and_developer_mode_reads_the_shared_cache() {
    let cache = filled();
    let workspace = with_env(cache.path(), Some("developer"), Some("1"));

    workspace.build().succeeded();

    assert!(
        workspace.compiled().is_empty(),
        "{:?}",
        workspace.compiled()
    );
}

#[test]
fn without_ci_and_without_a_mode_the_build_is_a_developer_build() {
    let cache = filled();
    let workspace = with_env(cache.path(), None, None);

    workspace.build().succeeded();

    assert!(workspace.compiled().is_empty());
}

#[test]
fn a_mode_that_is_not_exactly_one_of_the_three_is_a_usage_error() {
    let cache = filled();
    for mode in ["Developer", "dev", "", "ci "] {
        let workspace = with_env(cache.path(), Some(mode), None);

        let run = workspace.build();

        assert_eq!(run.code, 2, "mode {mode:?}: {}", run.stderr);
        assert!(run.stderr.contains("OXABL_CACHE_MODE"), "{}", run.stderr);
        assert!(workspace.compiled().is_empty(), "nothing compiled");
    }
}

#[test]
fn the_cache_directory_is_created_private_to_the_user() {
    let parent = TempDir::new().unwrap();
    let cache = parent.path().join("new/cache");
    let workspace = with_env(&cache, None, None);

    workspace.build().succeeded();

    let mode = std::fs::metadata(&cache).unwrap().permissions().mode() & 0o777;
    assert_eq!(mode, 0o700);
    assert_eq!(entries(&cache).len(), 3);
}

#[test]
fn the_user_cache_directory_is_the_default_and_the_environment_beats_the_configuration() {
    let home = TempDir::new().unwrap();
    let workspace = project();
    workspace.env("OXABL_CACHE_MODE", Some("developer"));
    workspace.env("XDG_CACHE_HOME", Some(&home.path().to_string_lossy()));
    workspace.env("HOME", Some("/nonexistent"));
    workspace.build().succeeded();
    assert_eq!(entries(&home.path().join("oxabl/artifacts")).len(), 3);

    let configured = TempDir::new().unwrap();
    let overridden = TempDir::new().unwrap();
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.reconfigure(
        "include_paths = [\"src\"]",
        &format!(
            "[build.cache]\nlocal = {:?}",
            configured.path().to_string_lossy()
        ),
    );
    workspace.env("OXABL_CACHE_MODE", Some("developer"));
    workspace.build().succeeded();
    assert_eq!(entries(configured.path()).len(), 1, "[build.cache].local");

    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.reconfigure(
        "include_paths = [\"src\"]",
        &format!(
            "[build.cache]\nlocal = {:?}",
            configured.path().to_string_lossy()
        ),
    );
    workspace.env("OXABL_CACHE_MODE", Some("developer"));
    workspace.env(
        "OXABL_BUILD_CACHE_DIR",
        Some(&overridden.path().to_string_lossy()),
    );
    workspace.build().succeeded();
    assert_eq!(entries(overridden.path()).len(), 1, "OXABL_BUILD_CACHE_DIR");
}

fn job_workspace(directory: &Path, mode: &str) -> Built {
    let workspace = Built::new();
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.reconfigure(
        "include_paths = [\"src\"]",
        &format!(
            "[build.cache]\nlocal_job_dir = {:?}",
            directory.to_string_lossy()
        ),
    );
    workspace.env("OXABL_CACHE_MODE", Some(mode));
    workspace
}

#[test]
fn a_job_directory_with_files_and_no_marker_stops_the_build_and_nothing_is_deleted() {
    let job = TempDir::new().unwrap();
    std::fs::write(job.path().join("precious.txt"), "keep\n").unwrap();
    std::fs::create_dir_all(job.path().join("v1")).unwrap();
    std::fs::write(job.path().join("v1/data"), "keep\n").unwrap();
    let workspace = job_workspace(job.path(), "ci");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(run.stderr.contains("local_job_dir"), "{}", run.stderr);
    assert!(job.path().join("precious.txt").is_file());
    assert!(job.path().join("v1/data").is_file(), "v1 was not touched");
    assert!(workspace.compiled().is_empty());
}

/// A directory that looks like a job cache: a marker and a `v1` that holds a
/// file which must survive.
fn marked(directory: &Path) {
    std::fs::write(directory.join(".oxabl-cache-job"), "").unwrap();
    std::fs::create_dir_all(directory.join("v1")).unwrap();
    std::fs::write(directory.join("v1/precious"), "keep\n").unwrap();
}

#[test]
fn a_job_directory_may_not_be_the_workspace_root_an_ancestor_or_home() {
    let outer = TempDir::new().unwrap();
    let home = TempDir::new().unwrap();
    for what in ["the workspace root", "its parent", "$HOME"] {
        let workspace = Built::new_in(outer.path());
        workspace.write("src/a.p", "MESSAGE \"a\".\n");
        let directory = match what {
            "the workspace root" => workspace.root().to_path_buf(),
            "its parent" => outer.path().to_path_buf(),
            _ => home.path().to_path_buf(),
        };
        marked(&directory);
        workspace.reconfigure(
            "include_paths = [\"src\"]",
            &format!(
                "[build.cache]\nlocal_job_dir = {:?}",
                directory.to_string_lossy()
            ),
        );
        workspace.env("OXABL_CACHE_MODE", Some("ci"));
        workspace.env("HOME", Some(&home.path().to_string_lossy()));

        let run = workspace.build();

        assert_eq!(run.code, 2, "{what}: {}", run.stderr);
        assert!(
            run.stderr.contains("local_job_dir"),
            "{what}: {}",
            run.stderr
        );
        assert!(
            directory.join("v1/precious").is_file(),
            "{what}: its v1 was deleted although the directory is refused"
        );
        assert!(workspace.compiled().is_empty(), "{what}");
    }
}

#[test]
fn the_file_system_root_is_not_a_job_directory() {
    let workspace = job_workspace(Path::new("/"), "ci");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(run.stderr.contains("local_job_dir"), "{}", run.stderr);
    assert!(workspace.compiled().is_empty());
}

#[test]
fn a_job_directory_with_a_marker_loses_only_its_v1_directory_at_the_start_of_a_build() {
    let job = TempDir::new().unwrap();
    std::fs::write(job.path().join(".oxabl-cache-job"), "").unwrap();
    std::fs::write(job.path().join("notes.txt"), "stay\n").unwrap();
    std::fs::create_dir_all(job.path().join("v1/entries/zz")).unwrap();
    std::fs::write(job.path().join("v1/entries/zz/old"), "go\n").unwrap();
    let workspace = job_workspace(job.path(), "ci");

    workspace.build().succeeded();

    assert!(
        job.path().join("notes.txt").is_file(),
        "a file beside v1 survives"
    );
    assert!(
        !job.path().join("v1/entries/zz/old").exists(),
        "the old v1 is gone"
    );
    assert_eq!(entries(job.path()).len(), 1, "the build filled a fresh v1");
}

#[test]
fn a_new_job_directory_is_marked_and_used_in_ci_and_read_only_mode_but_not_in_developer_mode() {
    for (mode, used) in [("ci", true), ("read-only", true), ("developer", false)] {
        let parent = TempDir::new().unwrap();
        let job = parent.path().join("job");
        let workspace = job_workspace(&job, mode);

        workspace.build().succeeded();

        assert_eq!(job.join(".oxabl-cache-job").exists(), used, "{mode}");
        assert_eq!(entries(&job).len(), usize::from(used), "{mode}");
    }
}

#[test]
fn a_workspace_that_turns_the_remote_cache_on_needs_a_binary_with_it() {
    let workspace = project();
    workspace.write(".oxabl/cache.toml", "server = \"https://cache.example\"\n");

    let run = workspace.build();

    assert_eq!(run.code, 2, "{}", run.stderr);
    assert!(
        run.stderr.contains("built without remote-cache")
            && run.stderr.contains("--no-remote-cache"),
        "{}",
        run.stderr
    );
    assert!(workspace.compiled().is_empty());
    workspace.build_with(&["--no-remote-cache"]).succeeded();
    assert_eq!(workspace.compiled().len(), 3);
}

#[test]
fn waiting_for_the_lock_lets_a_second_build_follow_the_first() {
    let workspace = project();
    workspace.write("src/slow.p", "/* FAKE-SLOW */\nMESSAGE \"s\".\n");
    let child = workspace.spawn(&[]);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
    while !workspace.log().exists() && std::time::Instant::now() < deadline {
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    let refused = workspace.build();
    assert_eq!(refused.code, 3, "{}", refused.stderr);
    assert!(
        refused.stderr.contains("another oxabl build"),
        "{}",
        refused.stderr
    );

    let waited = workspace.build_with(&["--wait-lock", "60"]);

    waited.succeeded();
    assert_eq!(child.wait_with_output().unwrap().status.code(), Some(0));
}

#[test]
fn a_developer_build_that_knows_no_cache_directory_says_so_and_builds() {
    let workspace = project();
    workspace.env("OXABL_CACHE_MODE", Some("developer"));
    workspace.env("HOME", None);
    workspace.env("XDG_CACHE_HOME", None);

    let run = workspace.build();

    run.succeeded();
    assert!(
        run.stderr.contains("no directory is known"),
        "{}",
        run.stderr
    );
    assert_eq!(workspace.compiled().len(), 3);
}
