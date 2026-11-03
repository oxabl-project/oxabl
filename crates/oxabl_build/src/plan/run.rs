use std::path::PathBuf;
use std::thread;

use super::decide::{Plan, decide};
use super::since::changed_since;
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::key::compute::compute;
use crate::lock::{LockOutcome, try_acquire};
use crate::paths::PathMap;
use crate::refresh::{self, Outcome, RefreshError, Request};
use crate::store::{Store, StoreError};
use crate::worker::{Cancel, Pool, PoolSpec, random};

/// What `oxabl build --plan` is asked.
#[derive(Debug, Default)]
pub struct PlanRequest {
    /// The files to treat as changed, as host paths.
    pub named: Vec<PathBuf>,
    /// A git revision: every file that differs from it counts as changed.
    /// Excludes `named`.
    pub since: Option<String>,
    /// Start the worker pool and key the units. Without it no unit has a key.
    pub probe: Option<Probe>,
}

/// How the pool of a keyed plan starts.
#[derive(Debug, Default)]
pub struct Probe {
    /// Replaces `[build.workers].count`.
    pub workers: Option<usize>,
    /// Stops the start and the key phase.
    pub cancel: Cancel,
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

/// A refresh: its outcome, whether it was written to the store, and the notes
/// it made.
type Refreshed = (Outcome, bool, Vec<String>);

/// Plan the rebuild of the workspace: refresh the graph, then decide which
/// units must compile and why. With the workspace lock the refresh is
/// committed to the store; without it the refresh runs on a snapshot and
/// commits nothing. With a probe the workers start while the refresh runs, and
/// every unit gets its compile key, or the reasons it has none.
pub fn plan(workspace: &BuildWorkspace, request: &PlanRequest) -> Result<PlanReport, BuildError> {
    let settings = &workspace.settings;
    let since = match (&request.since, request.named.is_empty()) {
        (Some(_), false) => {
            return Err(BuildError::Usage(
                "--since and --files cannot be combined".to_string(),
            ));
        }
        (Some(rev), true) => Some(changed_since(&settings.root, rev)?),
        (None, _) => None,
    };
    let named = since.as_deref().unwrap_or(&request.named);
    let spec = match &request.probe {
        Some(probe) => Some(PoolSpec::from_config(
            workspace,
            probe.workers,
            random::build_id(),
        )?),
        None => None,
    };
    let refresh_request = Request {
        need_graph: !named.is_empty(),
        need_keys: spec.is_some(),
    };
    let no_cancel = Cancel::new();
    let cancel = request.probe.as_ref().map_or(&no_cancel, |p| &p.cancel);
    let (refreshed, started) = thread::scope(|scope| {
        let starting = spec
            .as_ref()
            .map(|spec| scope.spawn(|| Pool::start(spec, cancel)));
        let refreshed = refresh(workspace, refresh_request);
        let started = starting.map(|handle| handle.join().expect("the pool start panicked"));
        (refreshed, started)
    });
    let (outcome, committed, mut notes) = match refreshed {
        Ok(refreshed) => refreshed,
        Err(error) => {
            if let Some(Ok(started)) = started {
                started.pool.shutdown();
            }
            return Err(error);
        }
    };
    let keyed = match started {
        None => None,
        Some(started) => {
            let mut started = started?;
            let keyed = compute(workspace, &outcome, &started.environment, &mut started.pool);
            started.pool.shutdown();
            let keyed = keyed?;
            notes.extend(started.warnings);
            notes.extend(keyed.warnings.iter().cloned());
            Some(keyed)
        }
    };
    let paths = PathMap::new(&settings.root, settings.dlc.as_deref());
    let mut plan = decide(&outcome, &paths, named, keyed);
    if since.is_some() {
        plan.unknown.clear();
    }
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

/// Refresh under the workspace lock and commit, or on a snapshot without it.
fn refresh(workspace: &BuildWorkspace, request: Request) -> Result<Refreshed, BuildError> {
    match try_acquire(&workspace.settings.state_dir) {
        LockOutcome::Held(_lock) => {
            let (outcome, notes) = refresh_and_commit(workspace, request)?;
            Ok((outcome, true, notes))
        }
        LockOutcome::Busy(_) | LockOutcome::Unavailable(_) => {
            Ok((refresh_snapshot(workspace, request)?, false, Vec::new()))
        }
    }
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
