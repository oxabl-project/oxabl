//! The thread of one worker: take a job, compile it, promote what it made.

use std::sync::atomic::Ordering;
use std::sync::mpsc::Sender;

use super::shared::Shared;
use super::types::{Msg, UnitDone};
use crate::output::{Stage, promote};
use crate::schedule::Take;
use crate::worker::compile::CompiledUnit;
use crate::worker::{Definitions, Job, Worker, WorkerError};

/// What a worker does after a job.
enum Flow {
    Continue,
    /// A cancel arrived: the worker is stopped.
    Stop,
    /// No replacement could start.
    Dead(String),
}

/// Runs jobs until the queue closes.
pub fn run(worker: &mut Worker, shared: &Shared<'_>, tx: &Sender<Msg>) {
    let mut error = None;
    while let Take::Job(job) = shared.queue.take() {
        if shared.cancel.is_cancelled() {
            break;
        }
        let flow = run_job(worker, shared, tx, job);
        shared.queue.release();
        match flow {
            Flow::Continue => {}
            Flow::Stop => break,
            Flow::Dead(message) => {
                error = Some(message);
                break;
            }
        }
    }
    let _ = tx.send(Msg::SlotEnded {
        worker: worker.number,
        error,
    });
}

fn run_job(worker: &mut Worker, shared: &Shared<'_>, tx: &Sender<Msg>, job: usize) -> Flow {
    let mut units = std::mem::take(&mut shared.remaining.lock().unwrap()[job]);
    units.retain(|unit| match unsendable(worker, shared, *unit) {
        Some(note) => {
            promote::remove(shared.root, &shared.infos[unit].expected);
            let _ = tx.send(Msg::UnitDone(refused(job, *unit, note)));
            false
        }
        None => true,
    });
    while !units.is_empty() {
        let first = shared.infos[&units[0]].propath;
        let split = units
            .iter()
            .position(|unit| shared.infos[unit].propath != first)
            .unwrap_or(units.len());
        let rest = units.split_off(split);
        match compile_group(worker, shared, tx, job, &units) {
            Ok(()) => units = rest,
            Err((error, done)) => {
                let mut undone = units.split_off(done);
                undone.extend(rest);
                return lost(worker, shared, tx, job, undone, &error);
            }
        }
    }
    Flow::Continue
}

/// Why a unit cannot be sent to this worker: its name or the entries of its
/// PROPATH do not fit the worker's code page.
fn unsendable(worker: &Worker, shared: &Shared<'_>, unit: u32) -> Option<String> {
    let info = &shared.infos[&unit];
    std::iter::once(info.name.as_str())
        .chain(shared.propaths[info.propath].iter().map(String::as_str))
        .find_map(|text| worker.check_encoding(text).err())
}

fn refused(job: usize, unit: u32, note: String) -> UnitDone {
    UnitDone {
        job,
        unit,
        ok: false,
        ms: 0,
        messages: Vec::new(),
        note: Some(note),
        outputs: Vec::new(),
        lost: true,
    }
}

/// One request: the units of a job that share a PROPATH. On a failure it
/// gives the number of units that were finished.
fn compile_group(
    worker: &mut Worker,
    shared: &Shared<'_>,
    tx: &Sender<Msg>,
    job: usize,
    group: &[u32],
) -> Result<(), (WorkerError, usize)> {
    let names: Vec<String> = group
        .iter()
        .map(|unit| shared.infos[unit].name.clone())
        .collect();
    let propath = shared.infos[&group[0]].propath;
    let stage = worker.stage().clone();
    let request = Job {
        req: shared.next_request.fetch_add(1, Ordering::SeqCst),
        definitions: Definitions {
            propath: propath as u64 + 1,
            entries: &shared.propaths[propath],
            options: 1,
            pairs: shared.options,
        },
        xref: shared.xref,
        debug_list: shared.debug_list,
        listing: shared.listing,
        units: &names,
    };
    let _ = tx.send(Msg::UnitStarted {
        unit: group[0],
        worker: worker.number,
    });
    let number = worker.number;
    let mut done = 0;
    let result = worker.run(&request, shared.limits, &mut |compiled| {
        let unit = group[done];
        done += 1;
        let _ = tx.send(Msg::UnitDone(finish_unit(
            shared, &stage, job, unit, compiled,
        )));
        if let Some(next) = group.get(done) {
            let _ = tx.send(Msg::UnitStarted {
                unit: *next,
                worker: number,
            });
        }
    });
    stage.clear();
    result.map_err(|error| (error, done))
}

/// Installs the outputs of a unit that compiled, or removes the ones it had.
fn finish_unit(
    shared: &Shared<'_>,
    stage: &Stage,
    job: usize,
    unit: u32,
    compiled: CompiledUnit,
) -> UnitDone {
    let info = &shared.infos[&unit];
    let silent = !compiled.ok && compiled.messages.is_empty();
    let mut done = UnitDone {
        job,
        unit,
        ok: compiled.ok,
        ms: compiled.ms,
        messages: compiled.messages,
        note: silent.then(|| "the compile failed and the worker gave no message".to_string()),
        outputs: Vec::new(),
        lost: false,
    };
    if compiled.ok {
        match promote::promote(shared.root, stage.host(), &info.expected) {
            Ok(rows) => done.outputs = rows,
            Err(message) => {
                done.ok = false;
                done.note = Some(message);
            }
        }
    }
    if !done.ok {
        promote::remove(shared.root, &info.expected);
        if shared.fail_fast {
            shared.queue.stop();
        }
    }
    done
}

fn reason(error: &WorkerError) -> &'static str {
    match error {
        WorkerError::Timeout(_) => "timeout",
        WorkerError::Protocol(_) => "protocol",
        WorkerError::Lost(_) => "exited",
        WorkerError::Spawn(_) => "spawn",
        WorkerError::Cancelled => "cancelled",
    }
}

/// A worker that is lost: the unit in flight counts against it; the units that
/// had not finished go back to the queue; a new worker starts in the slot.
fn lost(
    worker: &mut Worker,
    shared: &Shared<'_>,
    tx: &Sender<Msg>,
    job: usize,
    mut undone: Vec<u32>,
    error: &WorkerError,
) -> Flow {
    if matches!(error, WorkerError::Cancelled) || shared.cancel.is_cancelled() {
        worker.kill();
        if !undone.is_empty() {
            shared.requeue(job, undone);
        }
        return Flow::Stop;
    }
    let in_flight = undone.first().copied();
    let _ = tx.send(Msg::WorkerLost {
        worker: worker.number,
        reason: reason(error),
        detail: error.to_string(),
        unit: in_flight,
    });
    if let Some(unit) = in_flight {
        let count = {
            let mut lost = shared.lost.lock().unwrap();
            let count = lost.entry(unit).or_insert(0);
            *count += 1;
            *count
        };
        if count >= 2 {
            undone.remove(0);
            crate::output::promote::remove(shared.root, &shared.infos[&unit].expected);
            let _ = tx.send(Msg::UnitDone(UnitDone {
                job,
                unit,
                ok: false,
                ms: 0,
                messages: Vec::new(),
                note: Some("worker lost twice while compiling this unit".to_string()),
                outputs: Vec::new(),
                lost: true,
            }));
        }
    }
    if !undone.is_empty() {
        shared.requeue(job, undone);
    }
    match worker.replace(&shared.restart) {
        Ok(()) => Flow::Continue,
        Err(message) => Flow::Dead(message),
    }
}
