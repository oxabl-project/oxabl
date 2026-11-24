//! `oxabl build --plan` on a workspace that changes between runs: which units
//! the plan lists, and why.

mod build_support;

use build_support::Workspace;

const INCLUDES: &str = "include_paths = [\"inc\", \"src\"]";

/// Three programs over two includes: `one.p` and `three.p` include `a.i`,
/// `two.p` and `three.p` include `b.i`.
fn includes_workspace() -> Workspace {
    let workspace = Workspace::with_sources(INCLUDES);
    workspace.write("inc/a.i", "DEFINE VARIABLE fromA AS INTEGER NO-UNDO.\n");
    workspace.write("inc/b.i", "DEFINE VARIABLE fromB AS INTEGER NO-UNDO.\n");
    workspace.write("src/one.p", "{a.i}\nMESSAGE \"one\".\n");
    workspace.write("src/two.p", "{b.i}\nMESSAGE \"two\".\n");
    workspace.write("src/three.p", "{a.i}\n{b.i}\nMESSAGE \"three\".\n");
    workspace
}

#[test]
fn a_fresh_workspace_lists_every_unit_as_new() {
    let workspace = includes_workspace();
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/one.p new",
            "rebuild src/three.p new",
            "rebuild src/two.p new"
        ]
    );
    assert!(
        run.summary().starts_with("3 to rebuild, 0 unchanged"),
        "{}",
        run.summary()
    );
}

#[test]
fn an_include_edit_names_its_includers() {
    let workspace = includes_workspace();
    workspace.plan().succeeded();

    workspace.write("inc/a.i", "DEFINE VARIABLE fromA AS CHARACTER NO-UNDO.\n");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/one.p dependency-changed: inc/a.i",
            "rebuild src/three.p dependency-changed: inc/a.i"
        ],
        "{}",
        run.stdout
    );
    assert!(
        run.summary().starts_with("2 to rebuild, 1 unchanged"),
        "{}",
        run.summary()
    );
}

#[test]
fn a_second_run_with_no_change_reports_no_change() {
    let workspace = includes_workspace();
    let first = workspace.plan();
    first.succeeded();

    let second = workspace.plan();
    second.succeeded();
    assert_eq!(second.rebuild(), Vec::<String>::new(), "{}", second.stdout);
    assert!(
        second.summary().starts_with("0 to rebuild, 3 unchanged"),
        "{}",
        second.summary()
    );
    assert_eq!(
        second.generation(),
        first.generation(),
        "a run that changed nothing must not move the store generation"
    );
}

#[test]
fn an_edit_moves_the_generation_once_and_is_then_settled() {
    let workspace = includes_workspace();
    let first = workspace.plan();
    first.succeeded();

    workspace.write("inc/b.i", "DEFINE VARIABLE fromB AS DECIMAL NO-UNDO.\n");
    let edited = workspace.plan();
    edited.succeeded();
    assert!(edited.generation() > first.generation());

    let settled = workspace.plan();
    settled.succeeded();
    assert_eq!(settled.rebuild(), Vec::<String>::new());
    assert_eq!(settled.generation(), edited.generation());
}

#[test]
fn an_edit_that_keeps_size_and_mtime_is_still_seen() {
    let workspace = includes_workspace();
    workspace.plan().succeeded();
    // Let the first stat stop being racy, so the stored hash is trusted.
    std::thread::sleep(std::time::Duration::from_millis(2100));
    workspace.plan().succeeded();

    let path = workspace.path("inc/a.i");
    let modified = std::fs::metadata(&path).unwrap().modified().unwrap();
    std::fs::write(&path, "DEFINE VARIABLE fromX AS INTEGER NO-UNDO.\n").unwrap();
    std::fs::File::options()
        .write(true)
        .open(&path)
        .unwrap()
        .set_modified(modified)
        .unwrap();

    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/one.p dependency-changed: inc/a.i",
            "rebuild src/three.p dependency-changed: inc/a.i"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_unit_edit_is_a_source_change_of_that_unit_alone() {
    let workspace = includes_workspace();
    workspace.plan().succeeded();

    workspace.write("src/two.p", "{b.i}\nMESSAGE \"changed\".\n");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/two.p source-changed"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_new_unit_is_new_and_a_deleted_unit_is_counted() {
    let workspace = includes_workspace();
    workspace.plan().succeeded();

    workspace.write("src/four.p", "{a.i}\n");
    workspace.remove("src/two.p");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(run.rebuild(), ["rebuild src/four.p new"], "{}", run.stdout);
    assert!(run.summary().contains(", 1 removed"), "{}", run.summary());
}

/// `x.i` is found in the second directory; a file of that name appears in the
/// first.
fn shadow_workspace() -> Workspace {
    let workspace = Workspace::with_sources("include_paths = [\"first\", \"second\"]");
    workspace.write("second/x.i", "MESSAGE \"second\".\n");
    workspace.write("src/uses.p", "{x.i}\n");
    workspace.write("src/other.p", "MESSAGE \"other\".\n");
    workspace
}

#[test]
fn a_new_file_that_shadows_an_include_analyses_its_includers_again() {
    let workspace = shadow_workspace();
    workspace.plan().succeeded();

    workspace.write("first/x.i", "MESSAGE \"first\".\n");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/uses.p closure-changed: +first/x.i, -second/x.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_deleted_include_lists_its_includers() {
    let workspace = shadow_workspace();
    workspace.plan().succeeded();

    workspace.remove("second/x.i");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/uses.p closure-changed: -second/x.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn an_include_that_appears_later_reopens_the_unit_that_could_not_find_it() {
    let workspace = Workspace::with_sources(INCLUDES);
    workspace.write("src/early.p", "{late.i}\nMESSAGE \"x\".\n");
    workspace.plan().succeeded();

    workspace.write("inc/late.i", "DEFINE VARIABLE late AS INTEGER NO-UNDO.\n");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/early.p closure-changed: +inc/late.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn nested_includes_reach_the_unit_that_includes_the_outer_one() {
    let workspace = Workspace::with_sources(INCLUDES);
    workspace.write("inc/outer.i", "{inner.i}\n");
    workspace.write("inc/inner.i", "DEFINE VARIABLE inner AS INTEGER NO-UNDO.\n");
    workspace.write("src/deep.p", "{outer.i}\n");
    workspace.write("src/shallow.p", "MESSAGE \"s\".\n");
    workspace.plan().succeeded();

    workspace.write("inc/inner.i", "DEFINE VARIABLE inner AS DECIMAL NO-UNDO.\n");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/deep.p dependency-changed: inc/inner.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_class_edit_names_the_units_that_use_the_class() {
    let workspace = Workspace::with_sources(INCLUDES);
    workspace.write("src/app/Base.cls", "CLASS app.Base:\nEND CLASS.\n");
    workspace.write(
        "src/app/Child.cls",
        "CLASS app.Child INHERITS app.Base:\nEND CLASS.\n",
    );
    workspace.write("src/user.p", "DEFINE VARIABLE c AS app.Child NO-UNDO.\n");
    workspace.write("src/alone.p", "MESSAGE \"a\".\n");
    workspace.plan().succeeded();

    workspace.write(
        "src/app/Base.cls",
        "CLASS app.Base:\n  DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO GET. SET.\nEND CLASS.\n",
    );
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/app/Base.cls source-changed",
            "rebuild src/app/Child.cls dependency-changed: src/app/Base.cls",
            "rebuild src/user.p dependency-changed: src/app/Base.cls"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn an_include_of_a_class_reaches_every_unit_that_uses_the_class() {
    let workspace = Workspace::with_sources(INCLUDES);
    workspace.write(
        "inc/members.i",
        "DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO GET. SET.\n",
    );
    workspace.write(
        "src/app/Base.cls",
        "CLASS app.Base:\n{members.i}\nEND CLASS.\n",
    );
    workspace.write("src/user.p", "DEFINE VARIABLE c AS app.Base NO-UNDO.\n");
    workspace.plan().succeeded();

    workspace.write(
        "inc/members.i",
        "DEFINE PUBLIC PROPERTY Q AS INTEGER NO-UNDO GET. SET.\n",
    );
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/app/Base.cls dependency-changed: inc/members.i",
            "rebuild src/user.p dependency-changed: inc/members.i"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_changed_source_setting_analyses_everything_again() {
    let workspace = Workspace::with_sources("include_paths = [\"one\"]");
    workspace.write("one/x.i", "MESSAGE \"one\".\n");
    workspace.write("two/x.i", "MESSAGE \"two\".\n");
    workspace.write("src/uses.p", "{x.i}\n");
    workspace.write("src/plain.p", "MESSAGE \"p\".\n");
    workspace.plan().succeeded();

    workspace.write(
        "oxabl.toml",
        "[workspace]\nname = \"t\"\n[workspace.sources]\ndirectories = [\"src\"]\ninclude_paths = [\"two\"]\n",
    );
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/uses.p closure-changed: +two/x.i, -one/x.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn all_lists_the_units_that_need_nothing() {
    let workspace = includes_workspace();
    workspace.plan().succeeded();
    let run = workspace.run(&["--plan", "--all"]);
    run.succeeded();
    assert_eq!(
        run.lines_starting("unchanged"),
        [
            "unchanged src/one.p",
            "unchanged src/three.p",
            "unchanged src/two.p"
        ]
    );
}

/// A file edited in the clock tick of the previous refresh keeps its size, its
/// mtime and its ctime: the store holds a stat that fits the new bytes next to
/// the hash of the old ones. The test writes that state into the store.
#[test]
fn a_stat_that_was_racy_when_taken_is_not_trusted() {
    use std::os::unix::fs::MetadataExt;

    let workspace = includes_workspace();
    workspace.plan().succeeded();

    let path = workspace.path("inc/a.i");
    std::fs::write(&path, "DEFINE VARIABLE fromX AS INTEGER NO-UNDO.\n").unwrap();
    let metadata = std::fs::metadata(&path).unwrap();
    let nanoseconds = |seconds: i64, nanoseconds: i64| seconds * 1_000_000_000 + nanoseconds;
    let connection = rusqlite::Connection::open(workspace.state_dir().join("state.db")).unwrap();
    let updated = connection
        .execute(
            "UPDATE files SET size = ?1, mtime_ns = ?2, ctime_ns = ?3, ino = ?4, dev = ?5
             WHERE path_id = (SELECT id FROM paths WHERE path = 'inc/a.i')",
            rusqlite::params![
                metadata.size() as i64,
                nanoseconds(metadata.mtime(), metadata.mtime_nsec()),
                nanoseconds(metadata.ctime(), metadata.ctime_nsec()),
                metadata.ino() as i64,
                metadata.dev() as i64,
            ],
        )
        .unwrap();
    assert_eq!(updated, 1);
    connection.close().unwrap();

    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/one.p dependency-changed: inc/a.i",
            "rebuild src/three.p dependency-changed: inc/a.i"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_class_outside_the_unit_directories_is_followed_to_its_includes() {
    let workspace = Workspace::with_sources("include_paths = [\"lib\", \"inc\"]");
    workspace.write(
        "inc/members.i",
        "DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO GET. SET.\n",
    );
    workspace.write(
        "lib/app/Base.cls",
        "CLASS app.Base:\n{members.i}\nEND CLASS.\n",
    );
    workspace.write("src/user.p", "DEFINE VARIABLE b AS app.Base NO-UNDO.\n");
    workspace.write("src/alone.p", "MESSAGE \"a\".\n");
    workspace.plan().succeeded();

    workspace.write(
        "inc/members.i",
        "DEFINE PUBLIC PROPERTY Q AS INTEGER NO-UNDO GET. SET.\n",
    );
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/user.p dependency-changed: inc/members.i"],
        "{}",
        run.stdout
    );

    workspace.write(
        "lib/app/Base.cls",
        "CLASS app.Base:\n{members.i}\n  DEFINE PUBLIC PROPERTY R AS INTEGER NO-UNDO GET. SET.\nEND CLASS.\n",
    );
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/user.p dependency-changed: lib/app/Base.cls"],
        "{}",
        run.stdout
    );
}

#[test]
fn excluded_files_and_hidden_directories_are_not_units() {
    let workspace = Workspace::with_config(
        "[workspace]\nname = \"t\"\n\n[workspace.sources]\ndirectories = [\"src\"]\n\n\
         [build]\nexclude = [\"**/fixtures/**\", \"src/legacy\", \"src/skip.p\"]\n",
    );
    for path in [
        "src/kept.p",
        "src/skip.p",
        "src/legacy/old.p",
        "src/deep/fixtures/sample.p",
        "src/deep/fixtures/inner/sample2.p",
        "src/deep/kept2.p",
        "src/.hidden/ignored.p",
    ] {
        workspace.write(path, "MESSAGE \"x\".\n");
    }
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/deep/kept2.p new", "rebuild src/kept.p new"],
        "{}",
        run.stdout
    );
}

#[test]
fn the_root_extensions_decide_which_files_are_units() {
    let workspace = Workspace::with_sources("root_extensions = [\"p\", \".pp\"]");
    for path in ["src/a.p", "src/b.pp", "src/c.w", "src/d.cls", "src/e.i"] {
        workspace.write(path, "MESSAGE \"x\".\n");
    }
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/a.p new", "rebuild src/b.pp new"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_unit_that_cannot_be_read_is_listed_and_the_plan_goes_on() {
    let workspace = includes_workspace();
    std::fs::write(workspace.path("src/broken.p"), b"MESSAGE \"\xff\xfe\".\n").unwrap();
    workspace.plan().succeeded();

    workspace.write("src/broken.p", "MESSAGE \"fixed\".\n");
    let run = workspace.plan();
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/broken.p source-changed"],
        "{}",
        run.stdout
    );
}
