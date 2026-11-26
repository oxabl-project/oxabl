//! Project automation. `cargo xtask ci` runs the jobs of `.github/workflows/ci.yml` here.
//!
//! Each CI job keeps only its runner setup and calls `cargo xtask ci <job>`, so a local run
//! and a CI run execute the same code.

mod changed;
mod checks;
mod jobs;
mod lost_content;
mod runner;
mod tools;

use changed::{HEAVY_JOBS, Tests};
use std::path::PathBuf;
use std::process::ExitCode;

const USAGE: &str = "usage: cargo xtask ci [--list] [--verbose] [--changed [--base REF] [--lint-only]] [JOB...]\n       cargo xtask lost-content <old-head> <new-head> [--base <ref>]";
const DEFAULT_BASE: &str = "origin/master";

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
        Some((command, rest)) if command == "lost-content" => lost_content::run(rest),
        _ => {
            eprintln!("{USAGE}");
            ExitCode::from(2)
        }
    }
}

/// What the command line asked for.
struct Request<'a> {
    list: bool,
    verbose: bool,
    changed: bool,
    lint_only: bool,
    base: String,
    wanted: Vec<&'a str>,
}

fn parse(arguments: &[String]) -> Result<Request<'_>, String> {
    let mut request = Request {
        list: false,
        verbose: std::env::var_os("GITHUB_ACTIONS").is_some(),
        changed: false,
        lint_only: false,
        base: DEFAULT_BASE.to_owned(),
        wanted: Vec::new(),
    };
    let mut base_given = false;
    let mut iterator = arguments.iter();
    while let Some(argument) = iterator.next() {
        match argument.as_str() {
            "--list" => request.list = true,
            "--verbose" => request.verbose = true,
            "--changed" => request.changed = true,
            "--lint-only" => request.lint_only = true,
            "--base" => {
                request.base = iterator.next().ok_or("--base needs a ref")?.clone();
                base_given = true;
            }
            flag if flag.starts_with('-') => return Err(format!("unknown flag {flag}")),
            name => request.wanted.push(name),
        }
    }
    if !request.changed && (base_given || request.lint_only) {
        return Err("--base and --lint-only need --changed".into());
    }
    Ok(request)
}

fn ci(arguments: &[String]) -> ExitCode {
    let request = match parse(arguments) {
        Ok(request) => request,
        Err(message) => {
            eprintln!("{message}\n{USAGE}");
            return ExitCode::from(2);
        }
    };
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let root = root.canonicalize().unwrap_or(root);
    let context = Context {
        log_dir: root.join("target/xtask-ci/logs"),
        root,
        verbose: request.verbose,
    };
    let selection = if request.changed {
        match select(&context, &request) {
            Ok(selection) => Some(selection),
            Err(message) => {
                eprintln!("cannot select the jobs of the changes: {message}");
                return ExitCode::FAILURE;
            }
        }
    } else {
        None
    };
    let tests = selection.as_ref().map_or(Tests::Full, |s| s.tests.clone());
    let all = jobs::all(&context, &tests);
    if request.list {
        for job in &all {
            if selection.as_ref().is_none_or(|s| s.jobs.contains(job.name)) {
                println!("{}", job.name);
            }
        }
        return ExitCode::SUCCESS;
    }
    for name in &request.wanted {
        if !all.iter().any(|job| job.name == *name) {
            let known: Vec<&str> = all.iter().map(|job| job.name).collect();
            eprintln!("unknown job {name}; known jobs: {}", known.join(", "));
            return ExitCode::from(2);
        }
    }
    let names = jobs_to_run(&request, selection.as_ref());
    let selected: Vec<&jobs::Job> = all.iter().filter(|job| names.contains(&job.name)).collect();
    if selected.is_empty() {
        println!("The changed paths select no job.");
        return ExitCode::SUCCESS;
    }
    if runner::run(&context, &selected) {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

/// The paths of the branch, and the selection they make.
fn select(context: &Context, request: &Request<'_>) -> Result<changed::Selection, String> {
    let workspace = changed::Workspace::load(&context.root)?;
    let tests = changed::UmbrellaTests::load(&context.root)?;
    let paths = changed::changed_paths(&context.root, &request.base)?;
    eprintln!("{} changed path(s) since {}", paths.len(), request.base);
    let selection = changed::select(&workspace, &tests, &paths);
    Ok(if request.lint_only {
        selection.lint_only()
    } else {
        selection
    })
}

/// Named jobs run as named. With `--changed` and no name, a local run takes the selected jobs
/// and leaves the heavy ones to CI. A change that selects every job (a lock file, a workflow,
/// `xtask`) runs the lint jobs here: the full suite runs in the merge queue.
fn jobs_to_run(request: &Request<'_>, selection: Option<&changed::Selection>) -> Vec<&'static str> {
    let all = jobs::JOB_NAMES;
    if !request.wanted.is_empty() {
        return all
            .iter()
            .copied()
            .filter(|name| request.wanted.contains(name))
            .collect();
    }
    let Some(selection) = selection else {
        return all.to_vec();
    };
    let everything = selection.tests == Tests::Full;
    let mut names = Vec::new();
    for name in all {
        if !selection.jobs.contains(name) {
            continue;
        }
        if HEAVY_JOBS.contains(&name) || (everything && !changed::LINT_JOBS.contains(&name)) {
            eprintln!("Not run here, CI runs it: {name}");
            continue;
        }
        names.push(name);
    }
    names
}
