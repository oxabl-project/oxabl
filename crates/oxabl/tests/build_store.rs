//! The build store as a user meets it: format changes, damage, a foreign file
//! and a lock held by another run.

mod build_support;

use std::fs::{self, File};
use std::time::{Duration, Instant};

use build_support::Workspace;

fn workspace() -> Workspace {
    let workspace = Workspace::with_sources("include_paths = [\"inc\"]");
    workspace.write("inc/a.i", "DEFINE VARIABLE fromA AS INTEGER NO-UNDO.\n");
    workspace.write("src/one.p", "{a.i}\nMESSAGE \"one\".\n");
    workspace.write("src/two.p", "MESSAGE \"two\".\n");
    workspace
}

fn store_path(workspace: &Workspace) -> std::path::PathBuf {
    workspace.state_dir().join("state.db")
}

fn user_version(workspace: &Workspace) -> i64 {
    let connection = rusqlite::Connection::open(store_path(workspace)).unwrap();
    connection
        .query_row("PRAGMA user_version", [], |row| row.get(0))
        .unwrap()
}

fn damaged_sets(workspace: &Workspace) -> Vec<String> {
    let mut names: Vec<String> = fs::read_dir(workspace.state_dir())
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .filter(|name| name.starts_with("state.db.corrupt-"))
        .collect();
    names.sort();
    names
}

#[test]
fn a_store_with_another_format_number_is_rebuilt() {
    let workspace = workspace();
    let fresh = workspace.plan();
    fresh.succeeded();
    let current = user_version(&workspace);
    assert_ne!(current, 0);

    let connection = rusqlite::Connection::open(store_path(&workspace)).unwrap();
    connection
        .pragma_update(None, "user_version", 4242)
        .unwrap();
    connection.close().unwrap();

    let rebuilt = workspace.plan();
    rebuilt.succeeded();
    assert_eq!(rebuilt.rebuild(), fresh.rebuild(), "{}", rebuilt.stdout);
    assert!(rebuilt.stderr.contains("4242"), "{}", rebuilt.stderr);
    assert_eq!(user_version(&workspace), current);

    let settled = workspace.plan();
    settled.succeeded();
    assert_eq!(settled.rebuild(), Vec::<String>::new());
}

#[test]
fn a_truncated_store_file_is_moved_aside_and_rebuilt() {
    let workspace = workspace();
    let fresh = workspace.plan();
    fresh.succeeded();
    // A store this size has pages past the first quarter of the file.
    for i in 0..300 {
        workspace.write(&format!("src/extra{i}.p"), &format!("MESSAGE \"{i}\".\n"));
    }
    workspace.plan().succeeded();
    let length = fs::metadata(store_path(&workspace)).unwrap().len();
    File::options()
        .write(true)
        .open(store_path(&workspace))
        .unwrap()
        .set_len(length / 3)
        .unwrap();

    let rebuilt = workspace.plan();
    rebuilt.succeeded();
    assert!(
        rebuilt.stderr.contains("moved it to state.db.corrupt-"),
        "{}",
        rebuilt.stderr
    );
    assert_eq!(rebuilt.rebuild().len(), 302, "{}", rebuilt.stdout);
    assert_eq!(damaged_sets(&workspace).len(), 1);

    let settled = workspace.plan();
    settled.succeeded();
    assert_eq!(settled.rebuild(), Vec::<String>::new());
    assert_eq!(damaged_sets(&workspace).len(), 1);
}

#[test]
fn a_store_cut_to_a_few_bytes_is_moved_aside_too() {
    let workspace = workspace();
    workspace.plan().succeeded();
    File::options()
        .write(true)
        .open(store_path(&workspace))
        .unwrap()
        .set_len(10)
        .unwrap();

    let rebuilt = workspace.plan();
    rebuilt.succeeded();
    assert_eq!(rebuilt.rebuild().len(), 2, "{}", rebuilt.stdout);
    assert_eq!(damaged_sets(&workspace).len(), 1);
}

#[test]
fn a_file_that_is_not_a_store_is_left_alone() {
    let workspace = workspace();
    workspace.plan().succeeded();
    let path = store_path(&workspace);
    fs::write(&path, "these are my notes, not a store\n").unwrap();

    let run = workspace.plan();
    assert_eq!(run.code, 3, "{}{}", run.stdout, run.stderr);
    assert!(
        run.stderr.contains("not an oxabl build store"),
        "{}",
        run.stderr
    );
    assert_eq!(
        fs::read_to_string(&path).unwrap(),
        "these are my notes, not a store\n"
    );
    assert!(damaged_sets(&workspace).is_empty());
}

#[test]
fn another_database_at_the_store_path_is_left_alone() {
    let workspace = workspace();
    workspace.plan().succeeded();
    let path = store_path(&workspace);
    fs::remove_file(&path).unwrap();
    let connection = rusqlite::Connection::open(&path).unwrap();
    connection
        .execute_batch("CREATE TABLE notes (text TEXT); INSERT INTO notes VALUES ('mine');")
        .unwrap();
    connection.close().unwrap();

    let run = workspace.plan();
    assert_eq!(run.code, 3, "{}{}", run.stdout, run.stderr);
    let connection = rusqlite::Connection::open(&path).unwrap();
    let text: String = connection
        .query_row("SELECT text FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(text, "mine");
}

fn hold_lock(workspace: &Workspace) -> File {
    fs::create_dir_all(workspace.state_dir()).unwrap();
    let file = File::options()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(workspace.state_dir().join("build.lock"))
        .unwrap();
    file.lock().unwrap();
    file
}

#[test]
fn two_plans_during_one_locked_build_both_succeed() {
    let workspace = workspace();
    let first = workspace.plan();
    first.succeeded();

    let lock = hold_lock(&workspace);
    workspace.write("inc/a.i", "DEFINE VARIABLE fromA AS CHARACTER NO-UNDO.\n");
    let started = Instant::now();
    let one = workspace.plan();
    let two = workspace.plan();
    assert!(
        started.elapsed() < Duration::from_secs(20),
        "a plan waited for the lock"
    );
    for run in [&one, &two] {
        run.succeeded();
        assert_eq!(
            run.rebuild(),
            ["rebuild src/one.p dependency-changed: inc/a.i"],
            "{}",
            run.stdout
        );
        assert!(
            run.summary().ends_with("(not committed)"),
            "{}",
            run.summary()
        );
        assert_eq!(run.generation(), first.generation());
    }

    drop(lock);
    let committed = workspace.plan();
    committed.succeeded();
    assert_eq!(
        committed.rebuild(),
        ["rebuild src/one.p dependency-changed: inc/a.i"],
        "nothing was written while the lock was held: {}",
        committed.stdout
    );
    assert!(
        !committed.summary().contains("not committed"),
        "{}",
        committed.summary()
    );
    assert!(committed.generation() > first.generation());
}

#[test]
fn a_plan_with_no_store_yet_runs_while_the_lock_is_held() {
    let workspace = workspace();
    let _lock = hold_lock(&workspace);
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(run.rebuild().len(), 2, "{}", run.stdout);
    assert!(
        !store_path(&workspace).exists(),
        "a plan without the lock wrote a store"
    );
}
