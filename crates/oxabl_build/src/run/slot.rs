//! The thread of one worker: take a job, compile it, promote what it made.

use std::sync::atomic::Ordering;
use std::sync::mpsc::Sender;
use std::time::Instant;

use super::shared::Shared;
use super::types::{Msg, UnitDone};
use crate::graph::Scratch;
use crate::observe::xref;
use crate::output::{Stage, promote};
use crate::schedule::{Take, Task};
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

/// Runs jobs and schema checks until the queue closes.
pub fn run(worker: &mut Worker, shared: &Shared<'_>, tx: &Sender<Msg>) {
    let mut error = None;
    let mut scratch = Scratch::new(shared.outcome.names.len());
    loop {
        let flow = match shared.queue.take() {
            Take::Closed => break,
            Take::Task(task) => {
                if shared.cancel.is_cancelled() {
                    break;
                }
                run_task(worker, shared, tx, task)
            }
            Take::Job(job) => {
                if shared.cancel.is_cancelled() {
                    break;
                }
                let flow = run_job(worker, shared, tx, job, &mut scratch);
                shared.queue.release();
                flow
            }
        };
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

/// One part of a schema check. A worker that fails it is lost.
fn run_task(worker: &mut Worker, shared: &Shared<'_>, tx: &Sender<Msg>, task: Task) -> Flow {
    let Some(imaging) = shared.imaging else {
        return Flow::Continue;
    };
    let result = worker.image_share(&imaging.databases, task.part, task.parts, imaging.timeout);
    let failed = result
        .as_ref()
        .err()
        .map(|error| (reason(error), error.to_string()));
    let _ = tx.send(Msg::Image {
        round: task.round,
        part: task.part,
        result: result.map_err(|error| error.to_string()),
    });
    let Some((why, detail)) = failed else {
        return Flow::Continue;
    };
    if shared.cancel.is_cancelled() {
        worker.kill();
        return Flow::Stop;
    }
    let _ = tx.send(Msg::WorkerLost {
        worker: worker.number,
        reason: why,
        detail,
        unit: None,
    });
    match worker.replace(&shared.restart) {
        Ok(()) => Flow::Continue,
        Err(message) => Flow::Dead(message),
    }
}

fn run_job(
    worker: &mut Worker,
    shared: &Shared<'_>,
    tx: &Sender<Msg>,
    job: usize,
    scratch: &mut Scratch,
) -> Flow {
    let mut units = std::mem::take(&mut shared.remaining.lock().unwrap()[job]);
    units.retain(|unit| match unsendable(worker, shared, *unit) {
        Some(note) => {
            promote::remove(shared.root, &shared.infos[unit].expected);
            shared.send_done(tx, refused(job, *unit, note));
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
        match compile_group(worker, shared, tx, job, &units, scratch) {
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
        finished: Instant::now(),
        reads: None,
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
    scratch: &mut Scratch,
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
    let mut verifiable: Vec<(u32, Instant)> = Vec::new();
    let result = worker.run(&request, shared.limits, &mut |compiled| {
        let unit = group[done];
        done += 1;
        let finished = finish_unit(shared, &stage, job, unit, compiled);
        if shared.verify.is_some() && finished.ok && shared.infos[&unit].key.is_some() {
            verifiable.push((unit, finished.finished));
        }
        shared.send_done(tx, finished);
        if let Some(next) = group.get(done) {
            let _ = tx.send(Msg::UnitStarted {
                unit: *next,
                worker: number,
            });
        }
    });
    stage.clear();
    if result.is_ok() {
        rehash(worker, shared, tx, &verifiable, scratch);
    }
    result.map_err(|error| (error, done))
}

/// V1 for the units of a request that compiled: hashes the unit and its
/// closure again on this worker, which is idle.
fn rehash(
    worker: &mut Worker,
    shared: &Shared<'_>,
    tx: &Sender<Msg>,
    units: &[(u32, Instant)],
    scratch: &mut Scratch,
) {
    let Some(verify) = shared.verify else { return };
    for (unit, done_at) in units {
        let closure = super::closure_of(shared.outcome, &shared.infos[unit], *unit, scratch);
        let result = verify.rehasher.check(*unit, &closure, *done_at, worker);
        let _ = tx.send(Msg::Rehashed {
            unit: *unit,
            result,
        });
    }
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
        finished: Instant::now(),
        reads: None,
    };
    let verified = info.key.is_some() && shared.verify.is_some();
    if compiled.ok && (verified || (info.observable && shared.observing)) {
        let file = match shared.user_xref {
            true => format!("xref/{}.xref.xml", info.name),
            false => format!("verify/{}.xref.xml", info.name),
        };
        done.reads = Some(xref::read(&stage.host().join(file)));
    }
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
            shared.send_done(
                tx,
                UnitDone {
                    job,
                    unit,
                    ok: false,
                    ms: 0,
                    messages: Vec::new(),
                    note: Some("worker lost twice while compiling this unit".to_string()),
                    outputs: Vec::new(),
                    lost: true,
                    finished: Instant::now(),
                    reads: None,
                },
            );
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
