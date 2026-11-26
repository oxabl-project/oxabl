//! Runs jobs and prints the summary. A job stops at its first failed step. The run goes on
//! to the next job. Anything that is not a pass makes the run fail.

use crate::Context;
use crate::jobs::{Action, Command, Job, Step};
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::{Duration, Instant};

const FAILURE_TAIL_LINES: usize = 30;
const SNAPSHOT_DEPTH_MAX: u32 = 8;
const SNAPSHOT_FILE_COUNT_MAX: usize = 4096;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Status {
    Pass,
    Fail,
    Skip,
}

struct Result {
    job: &'static str,
    step: String,
    status: Status,
    elapsed: Duration,
    log: Option<PathBuf>,
}

/// Run the jobs and print the summary. True when every step passed.
pub fn run(context: &Context, jobs: &[&Job]) -> bool {
    let mut results: Vec<Result> = Vec::new();
    for job in jobs {
        run_job(context, job, &mut results);
    }
    print_summary(context, &results);
    results.iter().all(|result| result.status == Status::Pass)
}

fn run_job(context: &Context, job: &Job, results: &mut Vec<Result>) {
    println!("\n== {} ==", job.name);
    let snapshot = Snapshot::take(&context.root, &job.restore);
    let mut failed = snapshot.is_err();
    if let Err(message) = &snapshot {
        println!("   FAIL  cannot save the files the job rewrites: {message}");
        results.push(Result {
            job: job.name,
            step: "save the checkout files".into(),
            status: Status::Fail,
            elapsed: Duration::ZERO,
            log: None,
        });
    }
    for (index, step) in job.steps.iter().enumerate() {
        if failed {
            println!("   SKIP  {}", step.name);
            results.push(Result {
                job: job.name,
                step: step.name.into(),
                status: Status::Skip,
                elapsed: Duration::ZERO,
                log: None,
            });
            continue;
        }
        let result = run_step(context, job, index, step);
        failed = result.status != Status::Pass;
        results.push(result);
    }
    if let Ok(snapshot) = snapshot {
        let started = Instant::now();
        if let Err(message) = snapshot.restore() {
            println!("   FAIL  cannot restore the checkout files: {message}");
            results.push(Result {
                job: job.name,
                step: "restore the checkout files".into(),
                status: Status::Fail,
                elapsed: started.elapsed(),
                log: None,
            });
        }
    }
}

fn run_step(context: &Context, job: &Job, index: usize, step: &Step) -> Result {
    let started = Instant::now();
    let directory = context.log_dir.join(job.name);
    let log = directory.join(format!("{:02}-{}.log", index + 1, slug(step.name)));
    println!("   RUN   {}", step.name);
    let outcome = std::fs::create_dir_all(&directory)
        .map_err(|error| format!("cannot create {}: {error}", directory.display()))
        .and_then(|()| {
            std::fs::write(&log, "").map_err(|error| format!("cannot write the log: {error}"))
        })
        .and_then(|()| step.needs.iter().try_for_each(|need| need.check()))
        .and_then(|()| execute(context, step, &log));
    let (status, message) = match outcome {
        Ok(()) => (Status::Pass, String::new()),
        Err(message) => (Status::Fail, message),
    };
    if status == Status::Fail {
        append_message(&log, &message);
        report_failure(&log, &message);
    }
    Result {
        job: job.name,
        step: step.name.into(),
        status,
        elapsed: started.elapsed(),
        log: Some(log),
    }
}

fn execute(context: &Context, step: &Step, log: &Path) -> std::result::Result<(), String> {
    match &step.action {
        Action::Run(command) => run_command(context, command, log),
        Action::Check(check) => {
            let (text, outcome) = match check(context) {
                Ok(text) => (text, Ok(())),
                Err(text) => (text.clone(), Err("the check failed".to_owned())),
            };
            std::fs::write(log, text).map_err(|error| format!("cannot write the log: {error}"))?;
            outcome
        }
    }
}

fn run_command(
    context: &Context,
    command: &Command,
    log: &Path,
) -> std::result::Result<(), String> {
    for directory in &command.make_directories {
        std::fs::create_dir_all(directory)
            .map_err(|error| format!("cannot create {}: {error}", directory.display()))?;
    }
    let mut log_file =
        File::create(log).map_err(|error| format!("cannot create the log: {error}"))?;
    let line = format!("$ {} {}\n", command.program, command.args.join(" "));
    log_file
        .write_all(line.as_bytes())
        .map_err(|error| error.to_string())?;

    // Both streams go to the log file through one shared offset, so it keeps their order. A
    // pipe would not end while a background process that the command started holds it open.
    let mut process = std::process::Command::new(command.program);
    process
        .args(&command.args)
        .current_dir(context.root.join(&command.directory))
        .stdin(Stdio::null())
        .stderr(log_file.try_clone().map_err(|error| error.to_string())?);
    if !context.verbose {
        // A log file has no use for colour codes.
        process.env("CARGO_TERM_COLOR", "never");
    }
    for (name, value) in &command.env {
        process.env(name, value);
    }
    match &command.stdout_to {
        Some(file) => {
            let target = File::create(context.root.join(file))
                .map_err(|error| format!("cannot write {}: {error}", file.display()))?;
            process.stdout(target);
        }
        None => {
            process.stdout(log_file.try_clone().map_err(|error| error.to_string())?);
        }
    }
    let mut child = process.spawn().map_err(|error| match error.kind() {
        std::io::ErrorKind::NotFound => format!(
            "cannot start `{}`: not found (a missing program or a missing file of the command)",
            command.program
        ),
        _ => format!("cannot start `{}`: {error}", command.program),
    })?;
    let status = wait(&mut child, log, context.verbose)?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("`{}` exited with {status}", command.program))
    }
}

/// Wait for the child. When verbose, show the log as it grows.
fn wait(
    child: &mut std::process::Child,
    log: &Path,
    verbose: bool,
) -> std::result::Result<std::process::ExitStatus, String> {
    if !verbose {
        return child.wait().map_err(|error| error.to_string());
    }
    let mut reader = File::open(log).map_err(|error| error.to_string())?;
    let mut shown = 0;
    loop {
        let exited = child.try_wait().map_err(|error| error.to_string())?;
        shown += show_new_bytes(&mut reader)?;
        if let Some(status) = exited {
            assert!(shown > 0, "the log holds at least the command line");
            return Ok(status);
        }
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// Print what the log gained since the last call; returns the byte count.
fn show_new_bytes(reader: &mut File) -> std::result::Result<u64, String> {
    let mut text = Vec::new();
    reader
        .read_to_end(&mut text)
        .map_err(|error| error.to_string())?;
    // A closed terminal must not stop the step; the log still has the output.
    std::io::stdout().write_all(&text).ok();
    u64::try_from(text.len()).map_err(|error| error.to_string())
}

/// A failure before the command started, such as a missing tool, still leaves a log.
fn append_message(log: &Path, message: &str) {
    let opened = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(log);
    if let Ok(mut file) = opened {
        // The message is also printed, so a failed write loses nothing.
        file.write_all(format!("\n{message}\n").as_bytes()).ok();
    }
}

fn report_failure(log: &Path, message: &str) {
    println!("   FAIL  {message}");
    let text = std::fs::read_to_string(log).unwrap_or_default();
    let lines: Vec<&str> = text.lines().collect();
    let start = lines.len().saturating_sub(FAILURE_TAIL_LINES);
    for line in &lines[start..] {
        println!("         | {line}");
    }
}

fn slug(name: &str) -> String {
    let slug: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect();
    assert!(!slug.is_empty(), "a step has a name");
    slug
}

fn print_summary(context: &Context, results: &[Result]) {
    println!("\n== summary ==");
    let width = results.iter().map(|r| r.job.len()).max().unwrap_or(0);
    let step_width = results.iter().map(|r| r.step.len()).max().unwrap_or(0);
    for result in results {
        let status = match result.status {
            Status::Pass => "PASS",
            Status::Fail => "FAIL",
            Status::Skip => "SKIP",
        };
        let log = result
            .log
            .as_ref()
            .map(|log| {
                log.strip_prefix(&context.root)
                    .unwrap_or(log)
                    .display()
                    .to_string()
            })
            .unwrap_or_default();
        println!(
            "{status}  {:<width$}  {:<step_width$}  {:>7.1}s  {log}",
            result.job,
            result.step,
            result.elapsed.as_secs_f64()
        );
    }
    let count = |wanted: Status| results.iter().filter(|r| r.status == wanted).count();
    println!(
        "\n{} passed, {} failed, {} skipped",
        count(Status::Pass),
        count(Status::Fail),
        count(Status::Skip)
    );
}

/// The contents of the files a job rewrites, so that a run leaves the checkout as it found it.
struct Snapshot {
    files: Vec<(PathBuf, Vec<u8>)>,
}

impl Snapshot {
    fn take(root: &Path, paths: &[PathBuf]) -> std::result::Result<Snapshot, String> {
        let mut files = Vec::new();
        for path in paths {
            collect(&root.join(path), 0, &mut files)?;
        }
        assert!(
            paths.is_empty() || !files.is_empty(),
            "a path to restore names a file"
        );
        Ok(Snapshot { files })
    }

    fn restore(&self) -> std::result::Result<(), String> {
        for (path, contents) in &self.files {
            let current = std::fs::read(path).ok();
            if current.as_ref() != Some(contents) {
                std::fs::write(path, contents)
                    .map_err(|error| format!("cannot restore {}: {error}", path.display()))?;
            }
        }
        Ok(())
    }
}

fn collect(
    path: &Path,
    depth: u32,
    files: &mut Vec<(PathBuf, Vec<u8>)>,
) -> std::result::Result<(), String> {
    if files.len() > SNAPSHOT_FILE_COUNT_MAX || depth > SNAPSHOT_DEPTH_MAX {
        return Err(format!("{} holds too many files to save", path.display()));
    }
    let metadata = std::fs::metadata(path)
        .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
    if metadata.is_dir() {
        let entries = std::fs::read_dir(path).map_err(|error| error.to_string())?;
        for entry in entries {
            collect(
                &entry.map_err(|error| error.to_string())?.path(),
                depth + 1,
                files,
            )?;
        }
    } else {
        let contents = std::fs::read(path)
            .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
        files.push((path.to_path_buf(), contents));
    }
    Ok(())
}
