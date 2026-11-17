//! The thread of one worker: take a job, compile it, promote what it made.

use std::sync::atomic::Ordering;
use std::sync::mpsc::Sender;
use std::time::Instant;

use super::shared::Shared;
use super::types::{Msg, UnitDone};
use crate::graph::Scratch;
use crate::observe::xref::{self, Format};
use crate::output::{Stage, promote};
use crate::schedule::{Take, Task};
use crate::verify::release::Share;
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
    let result = match imaging.raw {
        true => worker
            .raw_share(&imaging.databases, task.part, task.parts, imaging.timeout)
            .map(Share::Raw),
        false => worker
            .image_share(&imaging.databases, task.part, task.parts, imaging.timeout)
            .map(Share::Image),
    };
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

/// The passes over a job of more than one unit at most.
const PASSES: usize = 3;

/// The id of the empty PROPATH that makes a session bind its classes again.
/// The ids of unit PROPATHs start at 1.
const REBIND: u64 = 0;

/// How a pass over the units of a job reports.
#[derive(Debug, Clone, Copy)]
enum Pass {
    /// The outputs are installed for the next pass; nothing is reported.
    Hold,
    /// The results count. `settled` when every unit of the job compiled in
    /// the pass before, so that each one read this build's r-code of the
    /// others.
    Final { settled: bool },
}

/// Compiles the units of `job`. The units of a job of more than one unit
/// read each other (a cycle of classes), and a compile binds the r-code it
/// finds first, also r-code that an older compile left: so they compile in
/// passes, each pass in a session that binds its classes again and reads
/// the r-code of the pass before from `$OUT`, and only the last pass counts.
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
    if units.len() < 2 {
        let pass = Pass::Final { settled: true };
        return match run_pass(worker, shared, tx, job, (&units, pass, false), scratch) {
            Ok(_) => Flow::Continue,
            Err((error, undone)) => lost(worker, shared, tx, job, undone, &error),
        };
    }
    let mut settled = false;
    let mut flow = Flow::Continue;
    for number in 1..=PASSES {
        let last = settled || number == PASSES;
        let pass = match last {
            true => Pass::Final { settled },
            false => Pass::Hold,
        };
        match run_pass(worker, shared, tx, job, (&units, pass, number > 1), scratch) {
            Ok(all_ok) if !last => settled = all_ok,
            Ok(_) => {
                // A later job must not keep a class this job bound.
                if let Err(error) = rebind_session(worker, shared) {
                    flow = lost(worker, shared, tx, job, Vec::new(), &error);
                }
                break;
            }
            Err((error, undone)) => {
                // A lost pass that reported nothing starts the job again.
                let undone = match last {
                    true => undone,
                    false => {
                        let in_flight = undone[0];
                        std::iter::once(in_flight)
                            .chain(units.iter().copied().filter(|u| *u != in_flight))
                            .collect()
                    }
                };
                if last && !settled {
                    shared
                        .unsettled
                        .lock()
                        .unwrap()
                        .extend(undone.iter().copied());
                }
                return lost(worker, shared, tx, job, undone, &error);
            }
        }
    }
    flow
}

/// One pass over `units`, a request for each run of units that share a
/// PROPATH; with `rebind`, each request comes after one that makes the
/// session bind its classes again. Whether every unit compiled, or the error
/// and the units not finished, the one in flight first.
fn run_pass(
    worker: &mut Worker,
    shared: &Shared<'_>,
    tx: &Sender<Msg>,
    job: usize,
    (units, pass, rebind): (&[u32], Pass, bool),
    scratch: &mut Scratch,
) -> Result<bool, (WorkerError, Vec<u32>)> {
    let mut all_ok = true;
    let mut at = 0;
    while at < units.len() {
        let first = shared.infos[&units[at]].propath;
        let end = units[at..]
            .iter()
            .position(|unit| shared.infos[unit].propath != first)
            .map_or(units.len(), |split| at + split);
        if rebind && let Err(error) = rebind_session(worker, shared) {
            return Err((error, units[at..].to_vec()));
        }
        match compile_group(worker, shared, tx, job, &units[at..end], pass, scratch) {
            Ok(ok) => all_ok &= ok,
            Err((error, done)) => return Err((error, units[at + done..].to_vec())),
        }
        at = end;
    }
    Ok(all_ok)
}

/// Makes the session bind every class again: a request with no unit on the
/// empty PROPATH changes the PROPATH, and the next request sets it back.
fn rebind_session(worker: &mut Worker, shared: &Shared<'_>) -> Result<(), WorkerError> {
    let request = Job {
        req: shared.next_request.fetch_add(1, Ordering::SeqCst),
        definitions: Definitions {
            propath: REBIND,
            entries: &[],
            options: 1,
            pairs: shared.options,
        },
        xref: shared.xref,
        debug_list: false,
        listing: false,
        units: &[],
    };
    worker.run(&request, shared.limits, &mut |_| {})
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
        unsettled: false,
    }
}

/// One request: the units of a job that share a PROPATH. Whether every unit
/// compiled; on a failure, the number of units that were finished.
fn compile_group(
    worker: &mut Worker,
    shared: &Shared<'_>,
    tx: &Sender<Msg>,
    job: usize,
    group: &[u32],
    pass: Pass,
    scratch: &mut Scratch,
) -> Result<bool, (WorkerError, usize)> {
    let report = matches!(pass, Pass::Final { .. });
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
        xref: match group.iter().any(|unit| shared.infos[unit].observable) {
            true => shared.observed_xref,
            false => shared.xref,
        },
        debug_list: shared.debug_list,
        listing: shared.listing,
        units: &names,
    };
    if report {
        let _ = tx.send(Msg::UnitStarted {
            unit: group[0],
            worker: worker.number,
        });
    }
    let number = worker.number;
    let mut done = 0;
    let mut all_ok = true;
    let mut verifiable: Vec<(u32, Instant)> = Vec::new();
    let result = worker.run(&request, shared.limits, &mut |compiled| {
        let unit = group[done];
        done += 1;
        let finished = finish_unit(shared, &stage, (job, unit, pass), compiled);
        all_ok &= finished.ok;
        if !report {
            return;
        }
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
    result.map(|()| all_ok).map_err(|error| (error, done))
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
    (job, unit, pass): (usize, u32, Pass),
    compiled: CompiledUnit,
) -> UnitDone {
    let info = &shared.infos[&unit];
    let (report, settled) = match pass {
        Pass::Hold => (false, true),
        Pass::Final { settled } => (true, settled),
    };
    let silent = !compiled.ok && compiled.messages.is_empty();
    let mut done = UnitDone {
        job,
        unit,
        ok: compiled.ok,
        ms: compiled.ms,
        messages: compiled
            .messages
            .into_iter()
            .map(|mut message| {
                if message.file.is_empty() {
                    message.file = info.path.clone();
                }
                message
            })
            .collect(),
        note: silent.then(|| "the compile failed and the worker gave no message".to_string()),
        outputs: Vec::new(),
        lost: false,
        finished: Instant::now(),
        reads: None,
        unsettled: !settled || shared.unsettled.lock().unwrap().contains(&unit),
    };
    let verified = info.key.is_some() && shared.verify.is_some();
    if report && compiled.ok && (verified || (info.observable && shared.observing)) {
        let (file, format) = match shared.user_xref {
            Some(Format::Xml) => (format!("xref/{}.xref.xml", info.name), Format::Xml),
            Some(Format::Text) => (format!("xref/{}.xref", info.name), Format::Text),
            None => (format!("verify/{}.xref", info.name), Format::Text),
        };
        done.reads = Some(xref::read(&stage.host().join(file), format));
    }
    if compiled.ok {
        match promote::promote(shared.root, stage.host(), &info.expected, |e| &e.staged) {
            Ok(rows) => done.outputs = rows,
            Err(message) => {
                done.ok = false;
                done.note = Some(message);
            }
        }
    }
    if !done.ok {
        promote::remove(shared.root, &info.expected);
        if report && shared.fail_fast {
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
                    unsettled: false,
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
