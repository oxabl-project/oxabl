//! The store keeps analyses only for the executable that made them: two
//! executables that differ in any byte never share an analysis.

mod build_support;

use std::fs::{self, File};
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use build_support::{Run, Workspace, run_of};
use tempfile::TempDir;

fn workspace() -> Workspace {
    let workspace = Workspace::with_sources("include_paths = [\"inc\"]");
    workspace.write("inc/a.i", "DEFINE VARIABLE fromA AS INTEGER NO-UNDO.\n");
    workspace.write("src/one.p", "{a.i}\nMESSAGE \"one\".\n");
    workspace.write("src/two.p", "MESSAGE \"two\".\n");
    workspace
}

/// A copy of the oxabl executable with `tail` appended: trailing bytes do not
/// stop an executable from running.
fn copy_with_tail(dir: &TempDir, name: &str, tail: &[u8]) -> PathBuf {
    let copy = dir.path().join(name);
    fs::copy(env!("CARGO_BIN_EXE_oxabl"), &copy).unwrap();
    let mut file = File::options().append(true).open(&copy).unwrap();
    file.write_all(tail).unwrap();
    copy
}

fn plan_with(workspace: &Workspace, program: &Path) -> Run {
    let run = run_of(workspace.command_of(program, &["--plan"]).output().unwrap());
    run.succeeded();
    run
}

/// The generation in which each stored analysis was made.
fn analysed_generations(workspace: &Workspace) -> Vec<u64> {
    let connection = rusqlite::Connection::open(workspace.state_dir().join("state.db")).unwrap();
    let mut statement = connection
        .prepare("SELECT analysed_gen FROM analyses ORDER BY path_id")
        .unwrap();
    statement
        .query_map([], |row| row.get::<_, i64>(0))
        .unwrap()
        .map(|generation| generation.unwrap() as u64)
        .collect()
}

#[test]
fn an_executable_with_other_bytes_analyses_every_node_again() {
    let workspace = workspace();
    let dir = TempDir::new().unwrap();
    let other = copy_with_tail(&dir, "oxabl-other", b"other build");

    let first = workspace.plan();
    first.succeeded();
    let made = analysed_generations(&workspace);
    assert!(!made.is_empty());
    assert!(made.iter().all(|g| *g == first.generation()), "{made:?}");

    let second = plan_with(&workspace, &other);
    assert!(
        second.generation() > first.generation(),
        "{}",
        second.stdout
    );
    assert_eq!(
        analysed_generations(&workspace),
        vec![second.generation(); made.len()],
        "every analysis must be made again by the other executable"
    );

    let third = plan_with(&workspace, &other);
    assert_eq!(
        third.generation(),
        second.generation(),
        "the same executable must not analyse again"
    );
    assert_eq!(
        analysed_generations(&workspace),
        vec![second.generation(); made.len()]
    );
    assert_eq!(third.rebuild(), second.rebuild());
}

#[test]
fn going_back_to_the_first_executable_analyses_again() {
    let workspace = workspace();
    let dir = TempDir::new().unwrap();
    let other = copy_with_tail(&dir, "oxabl-other", b"other build");

    let first = workspace.plan();
    first.succeeded();
    let second = plan_with(&workspace, &other);
    let back = workspace.plan();
    back.succeeded();
    assert!(back.generation() > second.generation(), "{}", back.stdout);
}

#[test]
fn an_executable_changed_in_place_with_the_same_size_and_mtime_is_seen() {
    let workspace = workspace();
    let dir = TempDir::new().unwrap();
    let copy = copy_with_tail(&dir, "oxabl-copy", b"AAAAAAAAAAAAAAAA");

    plan_with(&workspace, &copy);
    // Let the first stat stop being racy, so the stored hash is trusted.
    std::thread::sleep(Duration::from_millis(2100));
    let settled = plan_with(&workspace, &copy);
    let count = analysed_generations(&workspace).len();
    assert_ne!(count, 0);

    let modified = fs::metadata(&copy).unwrap().modified().unwrap();
    let mut file = File::options().write(true).open(&copy).unwrap();
    let length = file.metadata().unwrap().len();
    std::io::Seek::seek(&mut file, std::io::SeekFrom::Start(length - 16)).unwrap();
    file.write_all(b"BBBBBBBBBBBBBBBB").unwrap();
    file.set_modified(modified).unwrap();
    drop(file);

    let run = plan_with(&workspace, &copy);
    assert!(run.generation() > settled.generation(), "{}", run.stdout);
    assert_eq!(
        analysed_generations(&workspace),
        vec![run.generation(); count]
    );
}

/// A copy of the oxabl executable that can be run but not read.
fn execute_only_copy(dir: &TempDir) -> PathBuf {
    let copy = dir.path().join("oxabl-execute-only");
    fs::copy(env!("CARGO_BIN_EXE_oxabl"), &copy).unwrap();
    fs::set_permissions(&copy, fs::Permissions::from_mode(0o111)).unwrap();
    copy
}

#[test]
fn an_executable_that_cannot_be_read_analyses_every_node_again() {
    let uid = Command::new("id").arg("-u").output().unwrap().stdout;
    assert_ne!(
        String::from_utf8_lossy(&uid).trim(),
        "0",
        "this test needs a user that file modes apply to: root can read an execute-only file"
    );
    let workspace = workspace();
    let dir = TempDir::new().unwrap();
    let blind = execute_only_copy(&dir);

    let first = plan_with(&workspace, &blind);
    let made = analysed_generations(&workspace);
    assert!(!made.is_empty());
    let second = plan_with(&workspace, &blind);
    assert!(
        second.generation() > first.generation(),
        "{}",
        second.stdout
    );
    assert_eq!(
        analysed_generations(&workspace),
        vec![second.generation(); made.len()],
        "an executable that cannot be read must not keep its analyses"
    );
}
