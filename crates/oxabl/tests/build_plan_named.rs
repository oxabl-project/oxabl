//! `oxabl build --plan --files`: files treated as changed.

mod build_support;

use build_support::Workspace;

fn workspace() -> Workspace {
    let workspace = Workspace::with_sources("include_paths = [\"inc\", \"src\"]");
    workspace.write("inc/outer.i", "{inner.i}\n");
    workspace.write("inc/inner.i", "DEFINE VARIABLE inner AS INTEGER NO-UNDO.\n");
    workspace.write("src/deep.p", "{outer.i}\n");
    workspace.write("src/direct.p", "{inner.i}\n");
    workspace.write("src/alone.p", "MESSAGE \"a\".\n");
    workspace.write(
        "inc/members.i",
        "DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO GET. SET.\n",
    );
    workspace.write(
        "src/app/Holder.cls",
        "CLASS app.Holder:\n{members.i}\nEND CLASS.\n",
    );
    workspace.write("src/user.p", "DEFINE VARIABLE h AS app.Holder NO-UNDO.\n");
    workspace.plan().succeeded();
    workspace
}

#[test]
fn a_named_include_lists_the_units_that_reach_it() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--files", "inc/inner.i"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/deep.p named: inc/inner.i",
            "rebuild src/direct.p named: inc/inner.i"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_file_reached_through_a_class_names_the_way() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--files", "inc/members.i"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/app/Holder.cls named: inc/members.i",
            "rebuild src/user.p named: inc/members.i (src/user.p > src/app/Holder.cls > inc/members.i)"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_named_unit_is_listed_itself() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--files", "src/alone.p"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/alone.p named: src/alone.p"],
        "{}",
        run.stdout
    );
}

#[test]
fn several_named_files_are_listed_together() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--files", "inc/outer.i", "src/alone.p"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/alone.p named: src/alone.p",
            "rebuild src/deep.p named: inc/outer.i"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_named_file_outside_the_graph_changes_nothing() {
    let workspace = workspace();
    workspace.write("notes.txt", "not part of any build\n");
    let run = workspace.run(&["--plan", "--files", "notes.txt", "missing.i"]);
    run.succeeded();
    assert_eq!(run.rebuild(), Vec::<String>::new(), "{}", run.stdout);
    assert_eq!(
        run.lines_starting("unknown"),
        [
            "unknown notes.txt unknown-file",
            "unknown missing.i unknown-file"
        ]
    );
}

#[test]
fn naming_a_file_does_not_move_the_store_generation() {
    let workspace = workspace();
    let before = workspace.plan();
    let named = workspace.run(&["--plan", "--files", "inc/inner.i"]);
    named.succeeded();
    assert_eq!(named.generation(), before.generation());
    let after = workspace.plan();
    assert_eq!(after.rebuild(), Vec::<String>::new());
}

#[test]
fn a_file_changed_on_disk_and_named_is_listed_once() {
    let workspace = workspace();
    workspace.write("inc/inner.i", "DEFINE VARIABLE inner AS DECIMAL NO-UNDO.\n");
    let run = workspace.run(&["--plan", "--files", "inc/inner.i"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        [
            "rebuild src/deep.p named: inc/inner.i",
            "rebuild src/direct.p named: inc/inner.i"
        ],
        "{}",
        run.stdout
    );
}

#[test]
fn a_path_that_was_only_looked_for_is_not_in_the_graph() {
    let workspace = Workspace::with_sources("include_paths = [\"first\", \"second\"]");
    workspace.write("second/x.i", "MESSAGE \"second\".\n");
    workspace.write("src/uses.p", "{x.i}\n");
    workspace.plan().succeeded();

    let run = workspace.run(&["--plan", "--files", "first/x.i"]);
    run.succeeded();
    assert_eq!(run.rebuild(), Vec::<String>::new(), "{}", run.stdout);
    assert_eq!(
        run.lines_starting("unknown"),
        ["unknown first/x.i unknown-file"]
    );
}
