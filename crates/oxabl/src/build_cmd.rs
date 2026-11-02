//! `oxabl build`: the command line over [`oxabl_build`].

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use oxabl_build::config::BuildWorkspace;
use oxabl_build::error::BuildError;
use oxabl_build::plan::{PlanRequest, plan, render, render_json};

const NO_WORKER_COMMAND: &str = "oxabl build needs a worker command; set [build.workers].command \
     (see docs/design/build-tool/config.md) or run oxabl build --plan";

fn fail(error: &BuildError) -> ExitCode {
    eprintln!("error: {error}");
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

pub fn run(
    plan_only: bool,
    all: bool,
    files: &[PathBuf],
    since: Option<String>,
    json: bool,
    root: Option<&Path>,
) -> ExitCode {
    let workspace = match BuildWorkspace::open(root) {
        Ok(workspace) => workspace,
        Err(error) => return fail(&error),
    };
    if !plan_only {
        eprintln!(
            "error: {}",
            if workspace.settings.worker_command.is_empty() {
                NO_WORKER_COMMAND
            } else {
                "oxabl build compiles in a later release; run oxabl build --plan"
            }
        );
        return ExitCode::from(2);
    }
    let request = match named(files) {
        Ok(named) => PlanRequest { named, since },
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
