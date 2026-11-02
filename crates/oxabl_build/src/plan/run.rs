use std::path::PathBuf;

use super::decide::{Plan, decide};
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::lock::{LockOutcome, try_acquire};
use crate::paths::PathMap;
use crate::refresh::{self, Outcome, RefreshError, Request};
use crate::store::{Store, StoreError};

/// What `oxabl build --plan` is asked.
#[derive(Debug, Default)]
pub struct PlanRequest {
    /// The files to treat as changed, as host paths.
    pub named: Vec<PathBuf>,
}

/// The plan, and how the refresh behind it ran.
#[derive(Debug)]
pub struct PlanReport {
    pub plan: Plan,
    /// Whether the refresh was written to the store. It is not when another
    /// process holds the workspace lock.
    pub committed: bool,
    /// Repairs of the store and problems with the configuration.
    pub notes: Vec<String>,
}

fn infrastructure(error: impl std::fmt::Display) -> BuildError {
    BuildError::Infrastructure(error.to_string())
}

fn refresh_failure(error: RefreshError) -> BuildError {
    match error {
        RefreshError::Store(error) => infrastructure(error),
        RefreshError::Build(error) => error,
    }
}

/// Plan the rebuild of the workspace: refresh the graph, then decide which
/// units must compile and why. With the workspace lock the refresh is
/// committed to the store; without it the refresh runs on a snapshot and
/// commits nothing.
pub fn plan(workspace: &BuildWorkspace, request: &PlanRequest) -> Result<PlanReport, BuildError> {
    let settings = &workspace.settings;
    let refresh_request = Request {
        need_graph: !request.named.is_empty(),
    };
    let (outcome, committed, mut notes) = match try_acquire(&settings.state_dir) {
        LockOutcome::Held(_lock) => {
            let (outcome, notes) = refresh_and_commit(workspace, refresh_request)?;
            (outcome, true, notes)
        }
        LockOutcome::Busy(_) | LockOutcome::Unavailable(_) => (
            refresh_snapshot(workspace, refresh_request)?,
            false,
            Vec::new(),
        ),
    };
    let paths = PathMap::new(&settings.root, settings.dlc.as_deref());
    let mut plan = decide(&outcome, &paths, &request.named);
    if !committed {
        plan.generation = outcome.stored_generation;
    }
    notes.extend(outcome.warnings);
    Ok(PlanReport {
        plan,
        committed,
        notes,
    })
}

/// Refresh under the lock and commit. A store found damaged while reading it
/// is moved aside once and the refresh starts again on a new one.
fn refresh_and_commit(
    workspace: &BuildWorkspace,
    request: Request,
) -> Result<(Outcome, Vec<String>), BuildError> {
    let directory = &workspace.settings.store_dir;
    let mut notes = Vec::new();
    let mut retried = false;
    loop {
        let opened = Store::open_writer(directory).map_err(infrastructure)?;
        notes.extend(opened.notes);
        let mut store = opened.store;
        let result = {
            let reading = store.read().map_err(infrastructure)?;
            refresh::run(workspace, Some(&reading), request)
        };
        match result {
            Ok(outcome) => {
                store.commit(&outcome.changes).map_err(infrastructure)?;
                store.close().map_err(infrastructure)?;
                return Ok((outcome, notes));
            }
            Err(RefreshError::Store(StoreError::Corrupt(reason))) if !retried => {
                retried = true;
                drop(store);
                notes.push(Store::move_aside(directory, &reason).map_err(infrastructure)?);
            }
            Err(error) => return Err(refresh_failure(error)),
        }
    }
}

/// Refresh on a snapshot of the store without writing. A store this process
/// cannot read is the same as none.
fn refresh_snapshot(workspace: &BuildWorkspace, request: Request) -> Result<Outcome, BuildError> {
    let store = Store::open_reader(&workspace.settings.store_dir).map_err(infrastructure)?;
    let result = match store {
        Some(mut store) => {
            let reading = store.read().map_err(infrastructure)?;
            refresh::run(workspace, Some(&reading), request)
        }
        None => refresh::run(workspace, None, request),
    };
    match result {
        Err(RefreshError::Store(StoreError::Corrupt(_))) => {
            refresh::run(workspace, None, request).map_err(refresh_failure)
        }
        other => other.map_err(refresh_failure),
    }
}
