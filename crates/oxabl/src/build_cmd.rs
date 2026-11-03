//! `oxabl build`: the command line over [`oxabl_build`].

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use oxabl_build::config::BuildWorkspace;
use oxabl_build::error::BuildError;
use oxabl_build::plan::{PlanRequest, Probe, plan, render, render_json};
use oxabl_build::worker::pool::NO_COMMAND;

use crate::build_probe;

fn fail(error: &BuildError) -> ExitCode {
    if !matches!(error, BuildError::Cancelled) {
        eprintln!("error: {error}");
    }
    ExitCode::from(error.exit_code())
}

/// Absolute paths for `--files`, which the user typed relative to the current
/// directory.
fn named(files: &[PathBuf]) -> Result<Vec<PathBuf>, BuildError> {
    files
        .iter()
        .map(|file| {
            std::path::absolute(file)
                .map_err(|e| BuildError::Usage(format!("cannot resolve {}: {e}", file.display())))
        })
        .collect()
}

/// What the command line asked `oxabl build` to do.
pub struct Options<'a> {
    pub plan: bool,
    pub probe: bool,
    pub all: bool,
    pub files: &'a [PathBuf],
    pub since: Option<String>,
    pub json: bool,
    pub root: Option<&'a Path>,
    pub workers: Option<usize>,
}

pub fn run(options: Options<'_>) -> ExitCode {
    let Options {
        plan: plan_only,
        probe,
        all,
        files,
        since,
        json,
        root,
        workers,
    } = options;
    let workspace = match BuildWorkspace::open(root) {
        Ok(workspace) => workspace,
        Err(error) => return fail(&error),
    };
    if probe && !plan_only {
        return build_probe::run(&workspace, json, workers);
    }
    if !plan_only {
        eprintln!(
            "error: {}",
            if workspace.settings.worker_command.is_empty() {
                NO_COMMAND
            } else {
                "oxabl build compiles in a later release; run oxabl build --plan or --probe"
            }
        );
        return ExitCode::from(2);
    }
    let probe = probe.then(|| {
        let probe = Probe {
            workers,
            ..Probe::default()
        };
        build_probe::install_signal_handler(&probe.cancel);
        probe
    });
    let request = match named(files) {
        Ok(named) => PlanRequest {
            named,
            since,
            probe,
        },
        Err(error) => return fail(&error),
    };
    match plan(&workspace, &request) {
        Ok(report) => {
            for note in &report.notes {
                eprintln!("warning: {note}");
            }
            let render = if json { render_json } else { render };
            print!("{}", render(&report.plan, report.committed, all));
            ExitCode::SUCCESS
        }
        Err(error) => fail(&error),
    }
}
