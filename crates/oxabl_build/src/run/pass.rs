//! One pass over a set of units: schedule, dispatch, collect.

use std::sync::Mutex;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use super::context::{Ctx, State, UnitState};
use super::shared::Shared;
use super::slot;
use super::types::{Msg, UnitDone};
use crate::cache::{ArtifactFileRef, ArtifactPath, ArtifactSetRef, CompileKey};
use crate::error::BuildError;
use crate::events::Event;
use crate::schedule::{Queue, Schedule};
use crate::store::{COMPILED, FAILED, ResultRow, SUCCESS};
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

fn seconds() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| elapsed.as_secs() as i64)
}

/// Compiles `targets` on the workers of `pool`, classes before their users.
pub fn run(ctx: &Ctx<'_>, state: &mut State<'_>, pool: &mut Pool, targets: &[u32]) -> PassEnd {
    let outcome = ctx.outcome;
    let mut schedule = Schedule::build(
        &outcome.after,
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
        fail_fast: ctx.fail_fast,
        restart: pool.restart(),
        lost: &ctx.lost,
        next_request: &ctx.next_request,
    };
    for job in schedule.ready() {
        push(&shared, job);
    }
    let (tx, rx) = mpsc::channel();
    let mut end = PassEnd::Complete;
    thread::scope(|scope| {
        for worker in pool.workers_mut().iter_mut().filter(|w| !w.is_dead()) {
            let (tx, shared) = (tx.clone(), &shared);
            scope.spawn(move || slot::run(worker, shared, &tx));
        }
        drop(tx);
        let _stop = StopOnDrop(&shared.queue);
        end = coordinate(ctx, state, &mut schedule, &shared, &rx);
    });
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

fn push(shared: &Shared<'_>, job: usize) {
    let first = &shared.infos[&shared.jobs[job][0]].path;
    shared.queue.push(job, shared.priorities[job], first);
}

fn coordinate(
    ctx: &Ctx<'_>,
    state: &mut State<'_>,
    schedule: &mut Schedule,
    shared: &Shared<'_>,
    rx: &Receiver<Msg>,
) -> PassEnd {
    let mut finished = vec![0usize; schedule.len()];
    let mut jobs_left = schedule.len();
    let mut end = None;
    let stop = |end: &mut Option<PassEnd>, cause: PassEnd| {
        if end.is_none() {
            *end = Some(cause);
            shared.queue.stop();
        }
    };
    loop {
        match rx.recv_timeout(Duration::from_millis(100)) {
            Ok(Msg::UnitStarted { unit, worker }) => {
                let path = ctx.infos[&unit].path.as_str();
                state.emit(&Event::UnitStarted { path, worker });
            }
            Ok(Msg::UnitDone(done)) => {
                let job = done.job;
                let failed = !done.ok;
                complete(ctx, state, done);
                if failed && ctx.fail_fast {
                    stop(&mut end, PassEnd::FailFast);
                }
                finished[job] += 1;
                if finished[job] == shared.jobs[job].len() {
                    jobs_left -= 1;
                    for ready in schedule.finish(job) {
                        push(shared, ready);
                    }
                    if jobs_left == 0 {
                        shared.queue.finish();
                    }
                }
            }
            Ok(Msg::WorkerLost {
                worker,
                reason,
                detail,
                unit,
            }) => {
                let path = unit.map(|unit| ctx.infos[&unit].path.as_str());
                state.emit(&Event::WorkerLost {
                    worker,
                    reason,
                    unit: path,
                });
                state.notes.push(format!("worker {worker} lost: {detail}"));
            }
            Ok(Msg::SlotEnded {
                worker,
                error: Some(message),
            }) => state
                .notes
                .push(format!("worker {worker} could not be replaced: {message}")),
            Ok(Msg::SlotEnded { error: None, .. }) => {}
            Err(RecvTimeoutError::Timeout) => {
                if end.is_none() && jobs_left > 0 && shared.queue.idle() {
                    state.fatal = Some(BuildError::Infrastructure(format!(
                        "the schedule stopped with {jobs_left} jobs left and nothing to run; this is a bug in oxabl"
                    )));
                }
            }
            Err(RecvTimeoutError::Disconnected) => break,
        }
        if ctx.cancel.is_cancelled() {
            stop(&mut end, PassEnd::Cancelled);
        }
        if let Err(error) = state.recorder.flush_due() {
            state.fatal = Some(error);
        }
        if state.fatal.is_some() {
            stop(&mut end, PassEnd::Fatal);
        }
    }
    match end {
        Some(end) => end,
        None if jobs_left > 0 => PassEnd::NoWorkers,
        None => PassEnd::Complete,
    }
}

/// Records the end of one unit, tells the events and offers the outputs.
fn complete(ctx: &Ctx<'_>, state: &mut State<'_>, done: UnitDone) {
    let info = &ctx.infos[&done.unit];
    state.failed.remove(&done.unit);
    let tainted = done.ok && !state.failed.is_empty() && {
        let closure = ctx.outcome.after.closure(done.unit, &mut state.scratch);
        closure.iter().any(|node| state.failed.contains(node))
    };
    if !done.ok {
        state.failed.insert(done.unit);
    }
    for message in &done.messages {
        state.emit(&Event::Message {
            path: &info.path,
            message,
        });
    }
    state.emit(&Event::UnitFinished {
        path: &info.path,
        ok: done.ok,
        ms: done.ms,
    });
    let result = ResultRow {
        status: if done.ok { SUCCESS } else { FAILED },
        key: info.key,
        components: info.components,
        compiled_gen: ctx.generation,
        origin: COMPILED,
        duration_ms: done.ms,
        finished_at: seconds(),
        note: None,
    };
    if done.ok
        && let (Some(key), false, Some(uploader)) = (info.key, tainted, state.uploader.as_mut())
        && !ctx.cancel.is_cancelled()
    {
        uploader.push(ArtifactSetRef {
            key: CompileKey(key),
            files: info
                .expected
                .iter()
                .zip(&done.outputs)
                .map(|(expected, row)| ArtifactFileRef {
                    path: ArtifactPath(expected.staged.clone()),
                    disk_path: ctx.root.join(&row.path),
                    size: row.size,
                    blake3: row.hash,
                })
                .collect(),
        });
    }
    state.recorder.push(done.unit, result, done.outputs);
    state.units.insert(
        done.unit,
        UnitState {
            ok: done.ok,
            ms: done.ms,
            messages: done.messages,
            note: done.note,
            tainted,
            lost: done.lost,
        },
    );
}
