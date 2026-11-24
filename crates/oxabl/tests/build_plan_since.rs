//! `oxabl build --plan --since <rev>`: the files git says differ from a
//! revision count as changed.

mod build_support;

use build_support::Workspace;

fn workspace() -> Workspace {
    let workspace = Workspace::with_sources("include_paths = [\"inc\", \"src\"]");
    workspace.write("inc/outer.i", "{inner.i}\n");
    workspace.write("inc/inner.i", "DEFINE VARIABLE inner AS INTEGER NO-UNDO.\n");
    workspace.write(
        "inc/members.i",
        "DEFINE PUBLIC PROPERTY P AS INTEGER NO-UNDO GET. SET.\n",
    );
    workspace.write("src/deep.p", "{outer.i}\n");
    workspace.write("src/direct.p", "{inner.i}\n");
    workspace.write("src/alone.p", "MESSAGE \"a\".\n");
    workspace.write(
        "src/app/Holder.cls",
        "CLASS app.Holder:\n{members.i}\nEND CLASS.\n",
    );
    workspace.write("src/user.p", "DEFINE VARIABLE h AS app.Holder NO-UNDO.\n");
    workspace.commit_all("base");
    workspace.plan().succeeded();
    workspace
}

#[test]
fn a_tree_equal_to_the_revision_rebuilds_nothing() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert!(run.rebuild().is_empty(), "{}", run.stdout);
}

#[test]
fn an_uncommitted_edit_names_the_units_that_reach_the_file() {
    let workspace = workspace();
    workspace.write("inc/inner.i", "DEFINE VARIABLE other AS INTEGER NO-UNDO.\n");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
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
fn a_staged_edit_counts_too() {
    let workspace = workspace();
    workspace.write("src/alone.p", "MESSAGE \"b\".\n");
    workspace.git(&["add", "src/alone.p"]);
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/alone.p named: src/alone.p"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_committed_edit_counts_when_the_revision_is_older() {
    let workspace = workspace();
    workspace.write("inc/inner.i", "DEFINE VARIABLE other AS INTEGER NO-UNDO.\n");
    workspace.commit_all("edit the include");
    workspace.plan().succeeded();
    let head = workspace.run(&["--plan", "--since", "HEAD"]);
    head.succeeded();
    assert!(head.rebuild().is_empty(), "{}", head.stdout);
    let older = workspace.run(&["--plan", "--since", "HEAD~1"]);
    older.succeeded();
    assert_eq!(
        older.rebuild(),
        [
            "rebuild src/deep.p named: inc/inner.i",
            "rebuild src/direct.p named: inc/inner.i"
        ],
        "{}",
        older.stdout
    );
}

#[test]
fn an_untracked_include_that_a_unit_now_finds_names_that_unit() {
    let workspace = workspace();
    workspace.write("src/late.p", "{late.i}\n");
    workspace.commit_all("a unit with a missing include");
    workspace.plan().succeeded();
    workspace.write("inc/late.i", "DEFINE VARIABLE late AS INTEGER NO-UNDO.\n");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert_eq!(run.rebuild().len(), 1, "{}", run.stdout);
    assert!(
        run.rebuild()[0].starts_with("rebuild src/late.p "),
        "{}",
        run.stdout
    );
}

#[test]
fn an_untracked_unit_is_new() {
    let workspace = workspace();
    workspace.write("src/fresh.p", "MESSAGE \"fresh\".\n");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert_eq!(run.rebuild(), ["rebuild src/fresh.p new"], "{}", run.stdout);
}

#[test]
fn an_ignored_untracked_file_is_not_named() {
    let workspace = workspace();
    workspace.write(".gitignore", ".oxabl/\nsrc/skip.p\n");
    workspace.git(&["add", ".gitignore"]);
    workspace.git(&["commit", "--quiet", "-m", "ignore"]);
    workspace.write("src/skip.p", "MESSAGE \"skip\".\n");
    workspace.plan().succeeded();
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert!(run.rebuild().is_empty(), "{}", run.stdout);
}

#[test]
fn a_deleted_class_is_removed_and_its_users_rebuild() {
    let workspace = workspace();
    workspace.remove("src/app/Holder.cls");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert!(run.summary().contains("1 removed"), "{}", run.stdout);
    assert!(
        run.rebuild()
            .iter()
            .any(|l| l.starts_with("rebuild src/user.p ")),
        "{}",
        run.stdout
    );
}

#[test]
fn a_deleted_include_rebuilds_its_includers() {
    let workspace = workspace();
    workspace.remove("inc/outer.i");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/deep.p closure-changed: -inc/inner.i, -inc/outer.i"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_renamed_unit_is_the_old_one_removed_and_the_new_one_new() {
    let workspace = workspace();
    workspace.git(&["mv", "src/alone.p", "src/renamed.p"]);
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/renamed.p new"],
        "{}",
        run.stdout
    );
    assert!(run.summary().contains("1 removed"), "{}", run.stdout);
}

#[test]
fn a_renamed_include_rebuilds_its_includers() {
    let workspace = workspace();
    workspace.git(&["mv", "inc/inner.i", "inc/inner2.i"]);
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    let rebuilt: Vec<String> = run
        .rebuild()
        .iter()
        .map(|l| l.split(' ').take(2).collect::<Vec<_>>().join(" "))
        .collect();
    assert_eq!(
        rebuilt,
        ["rebuild src/deep.p", "rebuild src/direct.p"],
        "{}",
        run.stdout
    );
}

#[test]
fn a_file_that_is_not_in_the_graph_changes_nothing() {
    let workspace = workspace();
    workspace.write("README.md", "notes\n");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert!(run.rebuild().is_empty(), "{}", run.stdout);
    assert!(!run.stdout.contains("README"), "{}", run.stdout);
}

#[test]
fn outside_a_git_work_tree_it_is_a_usage_error() {
    let workspace = Workspace::with_sources("");
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    let run = workspace.run(&["--plan", "--since", "HEAD"]);
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(run.stderr.contains("git work tree"), "{}", run.stderr);
}

#[test]
fn an_unknown_revision_is_a_usage_error() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--since", "no-such-rev"]);
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(run.stderr.contains("not a git revision"), "{}", run.stderr);
}

#[test]
fn a_revision_that_looks_like_an_option_is_a_usage_error() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--since=--output=x"]);
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
    assert!(!workspace.path("x").exists());
}

#[test]
fn since_and_files_cannot_be_combined() {
    let workspace = workspace();
    let run = workspace.run(&["--plan", "--since", "HEAD", "--files", "src/alone.p"]);
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
}

#[test]
fn since_needs_plan() {
    let workspace = workspace();
    let run = workspace.run(&["--since", "HEAD"]);
    assert_eq!(run.code, 2, "{}{}", run.stdout, run.stderr);
}

#[test]
fn a_workspace_below_the_git_root_sees_only_its_own_files() {
    let workspace = Workspace::with_sources("");
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace.write("other/b.p", "MESSAGE \"b\".\n");
    workspace.commit_all("base");
    let root = workspace.path("sub");
    std::fs::create_dir_all(&root).unwrap();
    std::fs::rename(workspace.path("oxabl.toml"), root.join("oxabl.toml")).unwrap();
    std::fs::rename(workspace.path("src"), root.join("src")).unwrap();
    workspace.git(&["add", "-A"]);
    workspace.git(&["commit", "--quiet", "-m", "move"]);
    let command = |args: &[&str]| {
        let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_oxabl"));
        command
            .arg("build")
            .arg("--root")
            .arg(&root)
            .args(args)
            .current_dir(&root);
        build_support::run_of(command.output().unwrap())
    };
    command(&["--plan"]).succeeded();
    workspace.write("sub/src/a.p", "MESSAGE \"a2\".\n");
    workspace.write("other/b.p", "MESSAGE \"b2\".\n");
    let run = command(&["--plan", "--since", "HEAD"]);
    run.succeeded();
    assert_eq!(
        run.rebuild(),
        ["rebuild src/a.p named: src/a.p"],
        "{}",
        run.stdout
    );
}
