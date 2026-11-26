//! Project automation. `cargo xtask ci` runs the jobs of `.github/workflows/ci.yml` here.
//!
//! Each CI job keeps only its runner setup and calls `cargo xtask ci <job>`, so a local run
//! and a CI run execute the same code.

mod checks;
mod jobs;
mod runner;
mod tools;

use std::path::PathBuf;
use std::process::ExitCode;

const USAGE: &str = "usage: cargo xtask ci [--list] [--verbose] [JOB...]";

/// What every job and step needs to know about the run.
pub struct Context {
    /// The checkout: the parent of the `xtask` directory.
    pub root: PathBuf,
    /// Stream the output of each step to the terminal as well as to its log.
    pub verbose: bool,
    /// Logs live here, one directory for each job.
    pub log_dir: PathBuf,
}

fn main() -> ExitCode {
    let arguments: Vec<String> = std::env::args().skip(1).collect();
    match arguments.split_first() {
        Some((command, rest)) if command == "ci" => ci(rest),
        _ => {
            eprintln!("{USAGE}");
            ExitCode::from(2)
        }
    }
}

fn ci(arguments: &[String]) -> ExitCode {
    let mut list = false;
    let mut verbose = std::env::var_os("GITHUB_ACTIONS").is_some();
    let mut wanted: Vec<&str> = Vec::new();
    for argument in arguments {
        match argument.as_str() {
            "--list" => list = true,
            "--verbose" => verbose = true,
            flag if flag.starts_with('-') => {
                eprintln!("unknown flag {flag}\n{USAGE}");
                return ExitCode::from(2);
            }
            name => wanted.push(name),
        }
    }

    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let root = root.canonicalize().unwrap_or(root);
    let context = Context {
        log_dir: root.join("target/xtask-ci/logs"),
        root,
        verbose,
    };
    let all = jobs::all(&context);
    if list {
        for job in &all {
            println!("{}", job.name);
        }
        return ExitCode::SUCCESS;
    }
    for name in &wanted {
        if !all.iter().any(|job| job.name == *name) {
            let known: Vec<&str> = all.iter().map(|job| job.name).collect();
            eprintln!("unknown job {name}; known jobs: {}", known.join(", "));
            return ExitCode::from(2);
        }
    }
    let selected: Vec<&jobs::Job> = all
        .iter()
        .filter(|job| wanted.is_empty() || wanted.contains(&job.name))
        .collect();
    assert!(!selected.is_empty(), "the job list is never empty");
    if runner::run(&context, &selected) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
