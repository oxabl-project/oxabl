//! `oxabl build` without `--plan`: compile, and report.

use std::io::Write;
use std::path::Path;
use std::process::ExitCode;

use oxabl_build::config::BuildWorkspace;
use oxabl_build::error::BuildError;
use oxabl_build::events::Event;
use oxabl_build::run::{BuildReport, BuildRequest, build};
use oxabl_build::worker::Cancel;

use crate::build_probe;

/// What the command line asked of a build.
pub struct Options<'a> {
    pub workers: Option<usize>,
    pub json: bool,
    pub events: bool,
    pub report: Option<&'a Path>,
    pub fail_fast: bool,
}

fn fail(error: &BuildError) -> ExitCode {
    if !matches!(error, BuildError::Cancelled) {
        eprintln!("error: {error}");
    }
    ExitCode::from(error.exit_code())
}

/// Writes each event as one JSON line to stdout, flushed at once.
fn print_event(event: &Event<'_>) {
    let mut out = std::io::stdout().lock();
    let _ = writeln!(out, "{}", event.to_json());
    let _ = out.flush();
}

fn write_report(path: &Path, report: &BuildReport) -> Result<(), BuildError> {
    let mut text = serde_json::to_string_pretty(&report.to_json()).expect("a report is JSON");
    text.push('\n');
    std::fs::write(path, text).map_err(|error| {
        BuildError::Infrastructure(format!("cannot write {}: {error}", path.display()))
    })
}

pub fn run(workspace: &BuildWorkspace, options: Options<'_>) -> ExitCode {
    let cancel = Cancel::new();
    build_probe::install_signal_handler(&cancel);
    let request = BuildRequest {
        workers: options.workers,
        fail_fast: options.fail_fast,
        cancel,
        sink: options.events.then(|| Box::new(print_event) as _),
        ..BuildRequest::default()
    };
    let report = match build(workspace, request) {
        Ok(report) => report,
        Err(error) => return fail(&error),
    };
    for note in &report.notes {
        eprintln!("warning: {note}");
    }
    if let Some(path) = options.report
        && let Err(error) = write_report(path, &report)
    {
        return fail(&error);
    }
    let text = report.to_text();
    if options.events {
        eprint!("{text}");
    } else if options.json {
        let mut json = serde_json::to_string_pretty(&report.to_json()).expect("a report is JSON");
        json.push('\n');
        print!("{json}");
    } else {
        print!("{text}");
    }
    ExitCode::from(report.exit)
}
