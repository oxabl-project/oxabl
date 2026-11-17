//! `oxabl build` keeps version control off the directory that holds its state
//! and output: it writes a `.gitignore` holding `*` there, and never anywhere
//! it would hide a file of the user or break the ownership of an output.

mod common;

use common::built::Built;
use common::{run, stdout};
use std::process::Command;

fn git(workspace: &Built, args: &[&str]) -> String {
    let mut command = Command::new("git");
    command.current_dir(workspace.root()).args(args);
    let output = run(command);
    assert!(output.status.success(), "git {args:?} failed");
    stdout(&output)
}

/// A git repository with one unit, which writes its r-code to `output`.
fn repository(output: &str) -> Built {
    let workspace = Built::new();
    workspace.output(output);
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    git(&workspace, &["init", "-q"]);
    workspace
}

fn untracked(workspace: &Built) -> Vec<String> {
    git(
        workspace,
        &["status", "--porcelain", "--untracked-files=all"],
    )
    .lines()
    .map(str::to_string)
    .collect()
}

#[test]
fn git_ignores_the_state_and_the_output_with_no_change_to_the_code_base() {
    let workspace = repository(".oxabl/r");
    workspace.build().succeeded();
    assert!(workspace.exists(".oxabl/r/a.r"), "the build compiled a.p");
    let status = untracked(&workspace);
    assert!(
        status.iter().all(|line| !line.contains(".oxabl")),
        "git lists the oxabl tree: {status:?}"
    );
    assert!(
        workspace
            .read(".oxabl/.gitignore")
            .lines()
            .any(|l| l == "*"),
        "the file ignores everything"
    );
    assert!(
        !workspace.exists(".oxabl/r/.gitignore") && !workspace.exists(".oxabl/build/.gitignore"),
        "no file inside a directory that oxabl owns"
    );
    assert!(!workspace.exists(".gitignore"), "the root is left alone");

    workspace.build().succeeded();
    assert!(!untracked(&workspace).iter().any(|l| l.contains(".oxabl")));
}

#[test]
fn an_existing_gitignore_is_left_as_it_is() {
    let workspace = repository(".oxabl/r");
    workspace.write(".oxabl/.gitignore", "keep-me\n");
    workspace.build().succeeded();
    assert_eq!(workspace.read(".oxabl/.gitignore"), "keep-me\n");
}

#[test]
fn an_output_directory_that_is_the_top_directory_gets_no_gitignore() {
    let workspace = repository("build");
    workspace.build().succeeded();
    assert!(workspace.exists("build/a.r"), "the build compiled a.p");
    assert!(
        !workspace.exists("build/.gitignore"),
        "a file in an output directory is a file oxabl did not make"
    );
    assert!(
        workspace.exists(".oxabl/.gitignore"),
        "the state directory has its own"
    );
    workspace.build().succeeded();
}

#[test]
fn a_directory_with_a_file_of_the_user_gets_no_gitignore() {
    let workspace = repository("out/r");
    workspace.write("out/notes.txt", "mine\n");
    workspace.build().succeeded();
    assert!(
        !workspace.exists("out/.gitignore"),
        "a `*` there would hide notes.txt"
    );
}
