//! `oxabl build` without `--plan`: compile, and report.

use std::io::Write;
use std::path::Path;
use std::process::ExitCode;
use std::time::Duration;

use oxabl_build::cache::{Environment, Flags, RemoteOpener, select};
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
    pub no_cache: bool,
    pub no_remote_cache: bool,
    pub wait_lock: Option<u64>,
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

/// The remote cache of this binary: none without the `remote-cache` feature.
#[cfg(feature = "remote-cache")]
fn remote() -> Option<&'static dyn RemoteOpener> {
    Some(&oxabl_build_remote::RemoteCacheOpener)
}

#[cfg(not(feature = "remote-cache"))]
fn remote() -> Option<&'static dyn RemoteOpener> {
    None
}

pub fn run(workspace: &BuildWorkspace, options: Options<'_>) -> ExitCode {
    let cancel = Cancel::new();
    build_probe::install_signal_handler(&cancel);
    let selection = match select(
        workspace,
        Flags {
            no_cache: options.no_cache,
            no_remote_cache: options.no_remote_cache,
        },
        &Environment::from_process(),
        remote(),
    ) {
        Ok(selection) => selection,
        Err(error) => return fail(&error),
    };
    for note in &selection.notes {
        eprintln!("warning: {note}");
    }
    let request = BuildRequest {
        workers: options.workers,
        fail_fast: options.fail_fast,
        cancel,
        sink: options.events.then(|| Box::new(print_event) as _),
        cache: selection.cache,
        wait_lock: options.wait_lock.map(Duration::from_secs),
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
