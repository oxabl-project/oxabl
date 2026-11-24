//! `oxabl build`: the command line over [`oxabl_build`].

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use oxabl_build::cache::{Environment, Mode};
use oxabl_build::config::{
    BuildWorkspace, Overlay, OverlayOrigin, Overrides, effective, render_config,
    render_config_json, verification,
};
use oxabl_build::error::BuildError;
use oxabl_build::plan::{PlanRequest, Probe, plan, render, render_json};
use oxabl_build::verify::policy::Policy;

use crate::{build_probe, build_run};

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

/// The value of an environment variable that is set and not empty.
fn non_empty_variable(name: &str) -> Option<String> {
    std::env::var_os(name)
        .filter(|value| !value.is_empty())
        .map(|value| value.to_string_lossy().into_owned())
}

/// The overlay that `--config-overlay` or, failing that, `OXABL_BUILD_OVERLAY`
/// names.
fn overlay_file(flag: Option<&Path>) -> Option<Overlay> {
    match (flag, non_empty_variable("OXABL_BUILD_OVERLAY")) {
        (Some(path), _) => Some(Overlay {
            path: path.to_path_buf(),
            origin: OverlayOrigin::Flag,
        }),
        (None, Some(path)) => Some(Overlay {
            path: PathBuf::from(path),
            origin: OverlayOrigin::Environment,
        }),
        (None, None) => None,
    }
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
    pub config_overlay: Option<&'a Path>,
    pub print_config: bool,
    pub workers: Option<usize>,
    pub events: bool,
    pub report: Option<&'a Path>,
    pub fail_fast: bool,
    pub no_cache: bool,
    pub no_remote_cache: bool,
    pub wait_lock: Option<u64>,
    pub yield_to_newer: bool,
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
        config_overlay,
        print_config,
        workers,
        events,
        report,
        fail_fast,
        no_cache,
        no_remote_cache,
        wait_lock,
        yield_to_newer,
    } = options;
    let overlay = overlay_file(config_overlay);
    let workspace = match BuildWorkspace::open_with(root, overlay.as_ref()) {
        Ok(workspace) => workspace,
        Err(error) => return fail(&error),
    };
    if print_config {
        let overrides = Overrides {
            workers: workers.map(|n| n as u32),
            no_cache,
            no_remote_cache,
            dlc_sources: non_empty_variable("OXABL_DLC_SOURCES"),
            cache_dir: non_empty_variable("OXABL_BUILD_CACHE_DIR"),
        };
        let mut entries = effective(&workspace.config, &workspace.layers, &overrides);
        let environment = Environment::from_process();
        let policy = Mode::resolve(environment.cache_mode.as_deref(), environment.ci.as_deref())
            .and_then(|mode| Policy::resolve(mode, &workspace.config.build.verify));
        match policy {
            Ok(policy) => entries.push(verification(&policy, &environment)),
            Err(error) => return fail(&error),
        }
        let render = if json {
            render_config_json
        } else {
            render_config
        };
        print!(
            "{}",
            render(&workspace.settings.root, &workspace.layers, &entries)
        );
        return ExitCode::SUCCESS;
    }
    if probe && !plan_only {
        return build_probe::run(&workspace, json, workers);
    }
    if !plan_only {
        return build_run::run(
            &workspace,
            build_run::Options {
                workers,
                json,
                events,
                report,
                fail_fast,
                no_cache,
                no_remote_cache,
                wait_lock,
                yield_to_newer,
            },
        );
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
