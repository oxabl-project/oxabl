//! The fake AVM's runtime (`-p`, `-new`) and its compile outside the worker
//! protocol (`-c`), started in a workspace with the runtime PROPATH in the
//! `PROPATH` variable, as a project starts its runtime.

#![allow(dead_code)]

use std::path::Path;
use std::process::Command;

use super::fake_avm;
use super::runtime_cases::Start;

/// What one start of the fake runtime gave.
pub struct Ran {
    pub code: i32,
    pub stdout: String,
    pub stderr: String,
}

impl Ran {
    /// The `SAYS` texts, in order.
    pub fn said(&self) -> Vec<String> {
        self.stdout
            .lines()
            .filter_map(|line| line.strip_prefix("SAYS "))
            .map(String::from)
            .collect()
    }

    /// The file the runtime loaded for the program or class `name`, and how:
    /// `rcode` or `compiled-on-the-fly`.
    pub fn loaded(&self, name: &str) -> Option<(String, String)> {
        self.stdout.lines().find_map(|line| {
            let mut words = line.split(' ');
            let kind = words.next()?;
            if !matches!(kind, "RUN" | "NEW") || words.next()? != name {
                return None;
            }
            Some((words.next()?.to_string(), words.next()?.to_string()))
        })
    }

    /// The hash that the running code records for a file whose path ends in
    /// `suffix`: the file it was compiled from.
    pub fn read_hash(&self, suffix: &str) -> Option<String> {
        self.stdout.lines().find_map(|line| {
            let mut words = line.strip_prefix("read ")?.split(' ');
            let path = words.next()?;
            path.ends_with(suffix)
                .then(|| words.next().map(String::from))
                .flatten()
        })
    }
}

fn command(root: &Path, propath: &[&str]) -> Command {
    let mut command = Command::new(fake_avm());
    command
        .current_dir(root)
        .env("PROPATH", propath.join(","))
        .env_remove("FAKE_AVM_DLC");
    command
}

fn ran(mut command: Command) -> Ran {
    let output = command.output().expect("the fake runtime starts");
    Ran {
        code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

/// Starts a program or makes an object of a class in `root`, with `propath`
/// (entries relative to `root`) as the runtime PROPATH.
pub fn start(root: &Path, propath: &[&str], start: Start) -> Ran {
    let mut command = command(root, propath);
    match start {
        Start::Name(name) => command.args(["-p", name]),
        Start::FullPath(file) => command.arg("-p").arg(root.join(file)),
        Start::Class(class) => command.args(["-new", class]),
    };
    ran(command)
}

/// Starts the program `name` with `-p`.
pub fn run(root: &Path, propath: &[&str], name: &str) -> Ran {
    let mut command = command(root, propath);
    command.args(["-p", name]);
    ran(command)
}

/// `COMPILE <name> SAVE` (`into` empty) or `SAVE INTO <into>` on `propath`,
/// as a project's own build tool does; asserts that it compiled.
pub fn compile(root: &Path, propath: &[&str], name: &str, into: &str) {
    let mut command = command(root, propath);
    command.args(["-c", name]);
    if !into.is_empty() {
        command.args(["-save-into", into]);
    }
    let ran = ran(command);
    assert_eq!(ran.code, 0, "compile {name}: {}{}", ran.stdout, ran.stderr);
}
