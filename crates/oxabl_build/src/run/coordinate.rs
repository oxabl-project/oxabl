//! The coordinating thread of a pass: it collects what the workers and the
//! fetch report, and decides which job becomes ready.

use std::sync::mpsc::{Receiver, RecvTimeoutError};
use std::time::{Duration, Instant};

use rustc_hash::{FxHashMap, FxHashSet};

use super::complete::{complete, fetched};
use super::context::{Ctx, State};
use super::pass::PassEnd;
use super::release;
use super::shared::Shared;
use super::types::{Msg, UnitDone};
use crate::error::BuildError;
use crate::events::Event;
use crate::schedule::{Schedule, Task};

/// Where each job stands.
pub struct Coordinator<'a, 'b> {
    ctx: &'a Ctx<'a>,
    shared: &'a Shared<'b>,
    schedule: Schedule,
    /// Units of each job that finished: compiled or fetched.
    finished: Vec<usize>,
    /// Keyed units of each job that the fetch has not answered for.
    pending: Vec<usize>,
    /// Every job it waits for is finished.
    deps_done: Vec<bool>,
    closed: Vec<bool>,
    pushed: Vec<bool>,
    jobs_left: usize,
    job_of: FxHashMap<u32, usize>,
    fetched: FxHashSet<u32>,
    unresolved: FxHashSet<u32>,
    fetch_open: bool,
    /// Workers that take schema checks.
    workers: usize,
    end: Option<PassEnd>,
}

impl<'a, 'b> Coordinator<'a, 'b> {
    pub fn new(
        ctx: &'a Ctx<'a>,
        shared: &'a Shared<'b>,
        schedule: Schedule,
        awaiting: &[u32],
        workers: usize,
    ) -> Self {
        let count = schedule.len();
        let mut job_of = FxHashMap::default();
        for job in 0..count {
            for unit in &shared.jobs[job] {
                job_of.insert(*unit, job);
            }
        }
        let mut pending = vec![0; count];
        for unit in awaiting {
            pending[job_of[unit]] += 1;
        }
        let mut coordinator = Coordinator {
            ctx,
            shared,
            finished: vec![0; count],
            pending,
            deps_done: vec![false; count],
            closed: vec![false; count],
            pushed: vec![false; count],
            jobs_left: count,
            job_of,
            fetched: FxHashSet::default(),
            unresolved: awaiting.iter().copied().collect(),
            fetch_open: !awaiting.is_empty(),
            workers,
            end: None,
            schedule,
        };
        let ready = coordinator.schedule.ready();
        for job in &ready {
            coordinator.deps_done[*job] = true;
        }
        coordinator.advance(ready);
        coordinator
    }

    fn stop(&mut self, cause: PassEnd) {
        if self.end.is_none() {
            self.end = Some(cause);
            self.shared.queue.stop();
        }
    }

    /// Pushes the jobs of `start` that can run, and closes the ones that need
    /// no compile; closing a job can make others ready.
    fn advance(&mut self, start: Vec<usize>) {
        let mut stack = start;
        while let Some(job) = stack.pop() {
            if self.closed[job] || self.pending[job] > 0 {
                continue;
            }
            let left: Vec<u32> = self.shared.jobs[job]
                .iter()
                .filter(|unit| !self.fetched.contains(unit))
                .copied()
                .collect();
            if left.is_empty() {
                self.closed[job] = true;
                self.jobs_left -= 1;
                for ready in self.schedule.finish(job) {
                    self.deps_done[ready] = true;
                    stack.push(ready);
                }
                if self.jobs_left == 0 {
                    self.shared.queue.finish();
                }
            } else if self.deps_done[job] && !self.pushed[job] {
                self.pushed[job] = true;
                self.shared.requeue(job, left);
            }
        }
    }

    fn unit_done(&mut self, state: &mut State<'_>, done: UnitDone) {
        let job = done.job;
        let failed = !done.ok;
        complete(self.ctx, state, done);
        self.tick(state);
        if failed && self.ctx.fail_fast {
            self.stop(PassEnd::FailFast);
        }
        self.finished[job] += 1;
        if self.finished[job] == self.shared.jobs[job].len() {
            self.advance_after_finish(job);
        }
    }

    fn advance_after_finish(&mut self, job: usize) {
        if self.closed[job] {
            return;
        }
        self.closed[job] = true;
        self.jobs_left -= 1;
        let mut ready = Vec::new();
        for next in self.schedule.finish(job) {
            self.deps_done[next] = true;
            ready.push(next);
        }
        self.advance(ready);
        if self.jobs_left == 0 {
            self.shared.queue.finish();
        }
    }

    fn answered(&mut self, state: &mut State<'_>, unit: u32, hit: Option<super::types::Hit>) {
        if !self.unresolved.remove(&unit) {
            return;
        }
        let job = self.job_of[&unit];
        self.pending[job] -= 1;
        if let Some(hit) = hit {
            fetched(self.ctx, state, unit, hit);
            self.fetched.insert(unit);
            self.finished[job] += 1;
        }
        self.advance(vec![job]);
    }

    fn fetch_ended(&mut self) {
        self.fetch_open = false;
        let rest: Vec<u32> = self.unresolved.drain().collect();
        let mut jobs = Vec::new();
        for unit in rest {
            let job = self.job_of[&unit];
            self.pending[job] -= 1;
            jobs.push(job);
        }
        self.advance(jobs);
    }

    /// Starts a schema check when one is due.
    fn tick(&mut self, state: &mut State<'_>) {
        let cancelled = self.ctx.cancel.is_cancelled();
        let Some(release) = state.release.as_mut() else {
            return;
        };
        if cancelled || self.end.is_some() || self.workers == 0 {
            return;
        }
        if release.wants_round(Instant::now()) {
            let round = release.begin(self.workers);
            for part in 0..self.workers {
                self.shared.queue.push_task(Task {
                    round,
                    part,
                    parts: self.workers,
                });
            }
        }
    }

    /// Runs until every worker has left and every message is handled.
    pub fn run(mut self, state: &mut State<'_>, rx: &Receiver<Msg>) -> PassEnd {
        loop {
            match rx.recv_timeout(Duration::from_millis(100)) {
                Ok(Msg::UnitStarted { unit, worker }) => {
                    let path = self.ctx.infos[&unit].path.as_str();
                    state.emit(&Event::UnitStarted { path, worker });
                }
                Ok(Msg::UnitDone(done)) => self.unit_done(state, done),
                Ok(Msg::Rehashed { unit, result }) => {
                    if let Some(release) = state.release.as_mut() {
                        let sets = release.rehashed(unit, result);
                        release::push(state, sets);
                    }
                }
                Ok(Msg::Image {
                    round,
                    part,
                    result,
                }) => {
                    if let Some(release) = state.release.as_mut() {
                        let sets = release.share(round, part, result);
                        release::push(state, sets);
                    }
                }
                Ok(Msg::Fetched { unit, hit }) => self.answered(state, unit, hit),
                Ok(Msg::FetchEnded) => self.fetch_ended(),
                Ok(Msg::WorkerLost {
                    worker,
                    reason,
                    detail,
                    unit,
                }) => {
                    let path = unit.map(|unit| self.ctx.infos[&unit].path.as_str());
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
                    if self.end.is_none()
                        && self.jobs_left > 0
                        && !self.fetch_open
                        && self.shared.queue.idle()
                    {
                        state.fatal = Some(BuildError::Infrastructure(format!(
                            "the schedule stopped with {} jobs left and nothing to run; this is a bug in oxabl",
                            self.jobs_left
                        )));
                    }
                }
                Err(RecvTimeoutError::Disconnected) => break,
            }
            if self.ctx.cancel.is_cancelled() {
                self.stop(PassEnd::Cancelled);
            }
            self.tick(state);
            if let Err(error) = state.recorder.flush_due() {
                state.fatal = Some(error);
            }
            if state.fatal.is_some() {
                self.stop(PassEnd::Fatal);
            }
        }
        match self.end {
            Some(end) => end,
            None if self.jobs_left > 0 => PassEnd::NoWorkers,
            None => PassEnd::Complete,
        }
    }
}
