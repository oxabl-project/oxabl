//! One pass over a set of units: schedule, dispatch, collect.

use std::sync::Mutex;
use std::sync::mpsc;
use std::thread;

use rustc_hash::FxHashMap;

use super::context::{Ctx, State};
use super::coordinate::Coordinator;
use super::fetch;
use super::shared::Shared;
use super::slot;
use crate::paths::propath::is_class_unit;
use crate::schedule::{Queue, Schedule};
use crate::worker::Pool;

/// How a pass ended.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PassEnd {
    Complete,
    Cancelled,
    FailFast,
    /// Every worker was lost with jobs left.
    NoWorkers,
    /// The store could not be written.
    Fatal,
}

/// A unit oxabl cannot read compiles after every class unit of `targets`:
/// its compile can read their r-code from `$OUT`, which must be this build's.
fn observed_after_classes(ctx: &Ctx<'_>, targets: &[u32]) -> FxHashMap<u32, Vec<u32>> {
    let classes: Vec<u32> = targets
        .iter()
        .copied()
        .filter(|unit| is_class_unit(&ctx.infos[unit].path))
        .collect();
    if classes.is_empty() {
        return FxHashMap::default();
    }
    targets
        .iter()
        .filter(|unit| ctx.infos[unit].observable)
        .map(|unit| (*unit, classes.clone()))
        .collect()
}

/// Compiles `targets` on the workers of `pool`, classes before their users.
/// With `fetching`, the keyable units are first looked up in the cache, while
/// the units that wait on no lookup compile.
pub fn run(
    ctx: &Ctx<'_>,
    state: &mut State<'_>,
    pool: &mut Pool,
    targets: &[u32],
    fetching: bool,
) -> PassEnd {
    let outcome = ctx.outcome;
    let schedule = Schedule::build(
        &outcome.after,
        &observed_after_classes(ctx, targets),
        outcome.names.len(),
        targets,
        |unit| ctx.weights.get(&unit).copied().unwrap_or(1),
        |unit| outcome.name(unit),
    );
    if schedule.is_empty() {
        return PassEnd::Complete;
    }
    let jobs: Vec<Vec<u32>> = (0..schedule.len())
        .map(|job| schedule.job(job).units.clone())
        .collect();
    let shared = Shared {
        root: ctx.root,
        outcome,
        infos: &ctx.infos,
        priorities: (0..schedule.len())
            .map(|job| schedule.job(job).priority)
            .collect(),
        remaining: Mutex::new(jobs.clone()),
        jobs,
        queue: Queue::new(),
        propaths: &ctx.propaths,
        options: &ctx.options,
        xref: ctx.xref,
        debug_list: ctx.debug_list,
        listing: ctx.listing,
        limits: ctx.limits,
        cancel: ctx.cancel,
        verify: ctx.verify.as_ref(),
        observing: ctx.observer.is_some(),
        user_xref: ctx.user_xref,
        imaging: ctx.imaging.as_ref(),
        fail_fast: ctx.fail_fast,
        restart: pool.restart(),
        lost: &ctx.lost,
        next_request: &ctx.next_request,
    };
    let plan = ctx.fetch.as_ref().filter(|_| fetching);
    let awaiting: Vec<u32> = plan
        .map(|plan| plan.unit_of.values().copied().collect())
        .unwrap_or_default();
    let (tx, rx) = mpsc::channel();
    let workers = pool.workers_mut().iter().filter(|w| !w.is_dead()).count();
    let mut end = PassEnd::Complete;
    // The ready jobs are queued before a worker can take one, so the first
    // job a worker takes is the one of highest priority.
    let coordinator = Coordinator::new(ctx, &shared, schedule, &awaiting, workers);
    thread::scope(|scope| {
        for worker in pool.workers_mut().iter_mut().filter(|w| !w.is_dead()) {
            let (tx, shared) = (tx.clone(), &shared);
            scope.spawn(move || slot::run(worker, shared, &tx));
        }
        if let Some(plan) = plan.filter(|_| !awaiting.is_empty()) {
            let tx = tx.clone();
            scope.spawn(move || fetch::run(ctx, plan, tx));
        }
        drop(tx);
        let _stop = StopOnDrop(&shared.queue);
        end = coordinator.run(state, &rx);
    });
    shared.queue.clear_tasks();
    if let Some(release) = state.release.as_mut() {
        release.abandon_round();
    }
    end
}

/// Stops the queue when the coordinating thread leaves, even by a panic, so
/// that the workers leave too and the scope can end.
struct StopOnDrop<'a>(&'a Queue);

impl Drop for StopOnDrop<'_> {
    fn drop(&mut self) {
        self.0.stop();
    }
}
