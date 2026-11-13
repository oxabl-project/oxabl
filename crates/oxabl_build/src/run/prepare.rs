//! The start of a build: locks, output directories, refresh, workers, keys and
//! the plan.

use std::thread;
use std::time::{Duration, Instant};

use super::{BuildRequest, Setup, ms};
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::key::compute::compute;
use crate::key::image_store;
use crate::lock::{LockOutcome, WorkspaceLock, holder, try_acquire_file, try_acquire_marked};
use crate::output::{OutputLayout, ownership, stage};
use crate::paths::PathMap;
use crate::plan::run::{Committed, refresh_and_commit};
use crate::plan::{Plan, StoredInputs, decide};
use crate::refresh::Request;
use crate::worker::{Cancel, Pool, PoolSpec, Started, random};

/// A build that is ready to compile: its locks, its workers and its plan.
pub struct Prepared {
    pub spec: PoolSpec,
    pub layout: OutputLayout,
    pub committed: Committed,
    pub started: Started,
    pub plan: Plan,
    pub notes: Vec<String>,
    pub setup: Setup,
    /// The workspace lock and the output lock, held to the end.
    pub _locks: (WorkspaceLock, WorkspaceLock),
}

/// How long past the cancel grace a build waits for the one it stopped.
const YIELD_MARGIN: Duration = Duration::from_secs(30);

/// Takes a lock with `attempt`, trying again every 100 ms for up to `wait`.
fn acquire(
    attempt: impl Fn() -> LockOutcome,
    what: &str,
    wait: Option<Duration>,
    cancel: &Cancel,
) -> Result<WorkspaceLock, BuildError> {
    acquire_by(attempt, what, wait, cancel, &mut |_| None)
}

/// Like [`acquire`]. Each time the lock is busy `stop_holder` may stop the
/// holder; it returns the time to wait for the lock from then on.
fn acquire_by(
    attempt: impl Fn() -> LockOutcome,
    what: &str,
    wait: Option<Duration>,
    cancel: &Cancel,
    stop_holder: &mut dyn FnMut(&LockOutcome) -> Option<Duration>,
) -> Result<WorkspaceLock, BuildError> {
    let mut deadline = Instant::now() + wait.unwrap_or_default();
    loop {
        match attempt() {
            outcome @ LockOutcome::Busy(_) => {
                if let Some(longer) = stop_holder(&outcome) {
                    deadline = deadline.max(Instant::now() + longer);
                }
                if Instant::now() >= deadline {
                    return lock(outcome, what);
                }
                if cancel.is_cancelled() {
                    return Err(BuildError::Cancelled);
                }
                thread::sleep(Duration::from_millis(100));
            }
            outcome => return lock(outcome, what),
        }
    }
}

/// Stops the holder of the workspace lock once, when it marked itself as a
/// build that yields and its process is one: a reused pid is left alone.
/// Returns the longest the caller waits for the lock to go free.
fn stop_yielding_holder(
    state_dir: &std::path::Path,
    grace: Duration,
    sent: &mut bool,
) -> Option<Duration> {
    if *sent {
        return None;
    }
    let holder = holder(state_dir).filter(|holder| holder.yielding)?;
    if holder.pid == std::process::id() || !is_yielding_build(holder.pid) {
        return None;
    }
    let pid = rustix::process::Pid::from_raw(holder.pid as i32)?;
    rustix::process::kill_process(pid, rustix::process::Signal::INT).ok()?;
    *sent = true;
    eprintln!(
        "stopping the build (pid {}) that yields to a newer one",
        holder.pid
    );
    Some(grace + YIELD_MARGIN)
}

/// Whether `pid` runs `oxabl build --yield`, by its command line.
fn is_yielding_build(pid: u32) -> bool {
    std::fs::read(format!("/proc/{pid}/cmdline")).is_ok_and(|bytes| {
        let args: Vec<&[u8]> = bytes.split(|b| *b == 0).collect();
        let program = args.first().and_then(|a| a.rsplit(|b| *b == b'/').next());
        program.is_some_and(|name| name.starts_with(b"oxabl"))
            && args.contains(&&b"build"[..])
            && args.contains(&&b"--yield"[..])
    })
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
    let mut spec = PoolSpec::from_config(workspace, request.workers, random::build_id())?;
    spec.images = Some(settings.state_dir.join(image_store::FILE));
    let layout = OutputLayout::lower(&workspace.config, &settings.root)?;
    let grace = workspace.config.build.workers.cancel_grace.0;
    let mut stopped = false;
    let workspace_lock = acquire_by(
        || try_acquire_marked(&settings.state_dir, request.yield_to_newer),
        &format!("{}/build.lock", settings.state_dir.display()),
        request.wait_lock,
        &request.cancel,
        &mut |_| stop_yielding_holder(&settings.state_dir, grace, &mut stopped),
    )?;
    ownership::claim(&settings.root, &layout.directories())?;
    let output_dir = settings.root.join(&layout.rcode);
    // The stopped build frees its locks one after the other: the output lock
    // may stay busy a moment after the workspace lock.
    let output_wait = if stopped {
        request
            .wait_lock
            .map(|w| w.max(grace + YIELD_MARGIN))
            .or(Some(grace + YIELD_MARGIN))
    } else {
        request.wait_lock
    };
    let output_lock = acquire(
        || try_acquire_file(&output_dir, ownership::LOCK),
        &format!("{}/{}", layout.rcode, ownership::LOCK),
        output_wait,
        &request.cancel,
    )?;
    stage::remove_stale(&settings.state_dir);

    let refresh_request = Request {
        need_graph: true,
        need_keys: true,
    };
    let cancel = &request.cancel;
    let mut setup = Setup::default();
    let (committed, started) = thread::scope(|scope| {
        let starting = scope.spawn(|| Pool::start(&spec, cancel));
        let refreshing = Instant::now();
        let committed = refresh_and_commit(workspace, refresh_request);
        setup.refresh_ms = ms(refreshing);
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
    setup.workers_ms = started.timing.workers_ms;
    setup.imaging_ms = started.timing.imaging_ms;
    setup.raw_digest_ms = started.timing.raw_ms;
    let keying = Instant::now();
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
    setup.keys_ms = ms(keying);
    setup.dlc_hash_ms = started.pool.dlc_hash_ms();
    let planning = Instant::now();
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
        None,
        Some(StoredInputs {
            results: &committed.results,
            layout: Some(&layout),
            environment: environment.as_deref(),
            judge_all: true,
        }),
    );
    setup.plan_ms = ms(planning);
    Ok(Prepared {
        spec,
        layout,
        committed,
        started,
        plan,
        notes,
        setup,
        _locks: (workspace_lock, output_lock),
    })
}
