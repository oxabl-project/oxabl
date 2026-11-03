//! The start of a build: locks, output directories, refresh, workers, keys and
//! the plan.

use std::thread;

use super::BuildRequest;
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::key::compute::compute;
use crate::lock::{LockOutcome, WorkspaceLock, try_acquire, try_acquire_file};
use crate::output::{OutputLayout, ownership, stage};
use crate::paths::PathMap;
use crate::plan::run::{Committed, refresh_and_commit};
use crate::plan::{Plan, StoredInputs, decide};
use crate::refresh::Request;
use crate::worker::{Pool, PoolSpec, Started, random};

/// A build that is ready to compile: its locks, its workers and its plan.
pub struct Prepared {
    pub spec: PoolSpec,
    pub layout: OutputLayout,
    pub committed: Committed,
    pub started: Started,
    pub plan: Plan,
    pub notes: Vec<String>,
    /// The workspace lock and the output lock, held to the end.
    pub _locks: (WorkspaceLock, WorkspaceLock),
}

fn lock(outcome: LockOutcome, what: &str) -> Result<WorkspaceLock, BuildError> {
    match outcome {
        LockOutcome::Held(lock) => Ok(lock),
        LockOutcome::Busy(Some(pid)) => Err(BuildError::Infrastructure(format!(
            "another oxabl build (pid {pid}) holds {what}"
        ))),
        LockOutcome::Busy(None) => Err(BuildError::Infrastructure(format!(
            "another oxabl build holds {what}"
        ))),
        LockOutcome::Unavailable(error) => Err(BuildError::Infrastructure(format!(
            "cannot lock {what}: {error}"
        ))),
    }
}

/// Everything up to the plan. Workers that started are stopped again when a
/// later step fails.
pub fn prepare(
    workspace: &BuildWorkspace,
    request: &BuildRequest<'_>,
) -> Result<Prepared, BuildError> {
    let settings = &workspace.settings;
    let spec = PoolSpec::from_config(workspace, request.workers, random::build_id())?;
    let layout = OutputLayout::lower(&workspace.config, &settings.root)?;
    let workspace_lock = lock(
        try_acquire(&settings.state_dir),
        &format!("{}/build.lock", settings.state_dir.display()),
    )?;
    ownership::claim(&settings.root, &layout.directories())?;
    let output_dir = settings.root.join(&layout.rcode);
    let output_lock = lock(
        try_acquire_file(&output_dir, ownership::LOCK),
        &format!("{}/{}", layout.rcode, ownership::LOCK),
    )?;
    stage::remove_stale(&settings.state_dir);

    let refresh_request = Request {
        need_graph: true,
        need_keys: true,
    };
    let cancel = &request.cancel;
    let (committed, started) = thread::scope(|scope| {
        let starting = scope.spawn(|| Pool::start(&spec, cancel));
        let committed = refresh_and_commit(workspace, refresh_request);
        (committed, starting.join().expect("the pool start panicked"))
    });
    let committed = match committed {
        Ok(committed) => committed,
        Err(error) => {
            if let Ok(started) = started {
                started.pool.shutdown();
            }
            return Err(error);
        }
    };
    let mut started = started?;
    let keyed = match compute(
        workspace,
        &committed.outcome,
        &started.environment,
        &mut started.pool,
    ) {
        Ok(keyed) => keyed,
        Err(error) => {
            started.pool.shutdown();
            return Err(error.into());
        }
    };
    let mut notes = committed.notes.clone();
    notes.extend(started.warnings.iter().cloned());
    notes.extend(keyed.warnings.iter().cloned());
    notes.extend(committed.outcome.warnings.iter().cloned());
    let paths = PathMap::new(&settings.root, settings.dlc.as_deref());
    let environment = keyed.environment_record.clone();
    let plan = decide(
        &committed.outcome,
        &paths,
        &[],
        Some(keyed),
        Some(StoredInputs {
            results: &committed.results,
            layout: Some(&layout),
            environment: environment.as_deref(),
        }),
    );
    Ok(Prepared {
        spec,
        layout,
        committed,
        started,
        plan,
        notes,
        _locks: (workspace_lock, output_lock),
    })
}
