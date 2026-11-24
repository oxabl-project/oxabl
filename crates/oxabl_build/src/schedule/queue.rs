//! The ready queue that idle workers take jobs from.

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::sync::{Condvar, Mutex};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Entry {
    priority: u64,
    /// The path of the first unit: ties go to the smaller path.
    tie: Reverse<String>,
    job: usize,
}

#[derive(Default)]
struct State {
    heap: BinaryHeap<Entry>,
    tasks: Vec<Task>,
    /// No more jobs will be pushed.
    done: bool,
    /// Stop handing out jobs.
    stopped: bool,
    /// Jobs that were taken and not released yet.
    busy: usize,
    /// Completions that a worker reported and the coordinator has not handled.
    unsettled: usize,
}

/// One part of a schema check that an idle worker does between jobs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Task {
    pub round: u64,
    pub part: usize,
    pub parts: usize,
}

/// What a worker gets when it asks for work.
#[derive(Debug, PartialEq, Eq)]
pub enum Take {
    Job(usize),
    Task(Task),
    /// Nothing more to do.
    Closed,
}

/// Jobs that are ready, highest priority first.
#[derive(Default)]
pub struct Queue {
    state: Mutex<State>,
    wake: Condvar,
}

impl Queue {
    pub fn new() -> Queue {
        Queue::default()
    }

    /// Makes `jobs` ready together, each with its priority and the path of its
    /// first unit, so that no worker takes one before the others are in.
    pub fn push_all(&self, jobs: impl IntoIterator<Item = (usize, u64, String)>) {
        let mut state = self.state.lock().unwrap();
        for (job, priority, path) in jobs {
            state.heap.push(Entry {
                priority,
                tie: Reverse(path),
                job,
            });
            self.wake.notify_one();
        }
    }

    /// Offers a task to the next idle worker; tasks go before jobs.
    pub fn push_task(&self, task: Task) {
        let mut state = self.state.lock().unwrap();
        state.tasks.push(task);
        self.wake.notify_one();
    }

    /// Drops the tasks no worker has taken.
    pub fn clear_tasks(&self) {
        self.state.lock().unwrap().tasks.clear();
    }

    /// No job will be pushed any more: workers leave when the queue runs dry.
    pub fn finish(&self) {
        self.state.lock().unwrap().done = true;
        self.wake.notify_all();
    }

    /// Stops handing out jobs; workers leave at once.
    pub fn stop(&self) {
        self.state.lock().unwrap().stopped = true;
        self.wake.notify_all();
    }

    /// A worker is about to report a completion. No job goes out until the
    /// coordinator has handled it, so the jobs that it made ready are in the
    /// queue before any worker takes its next job.
    pub fn completion_sent(&self) {
        self.state.lock().unwrap().unsettled += 1;
    }

    /// The coordinator handled one reported completion.
    pub fn completion_handled(&self) {
        let mut state = self.state.lock().unwrap();
        state.unsettled -= 1;
        self.wake.notify_all();
    }

    /// Says that a worker is done with the job it took.
    pub fn release(&self) {
        self.state.lock().unwrap().busy -= 1;
    }

    /// Whether nothing is ready and no worker holds a job: if jobs are left,
    /// nothing will ever make one ready.
    pub fn idle(&self) -> bool {
        let state = self.state.lock().unwrap();
        state.heap.is_empty() && state.busy == 0
    }

    /// Waits for the next job.
    pub fn take(&self) -> Take {
        let mut state = self.state.lock().unwrap();
        loop {
            if state.stopped {
                return Take::Closed;
            }
            if !state.tasks.is_empty() {
                return Take::Task(state.tasks.remove(0));
            }
            if state.unsettled == 0
                && let Some(entry) = state.heap.pop()
            {
                state.busy += 1;
                return Take::Job(entry.job);
            }
            if state.done && state.heap.is_empty() {
                return Take::Closed;
            }
            state = self.wake.wait(state).unwrap();
        }
    }
}
