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
    /// No more jobs will be pushed.
    done: bool,
    /// Stop handing out jobs.
    stopped: bool,
    /// Jobs that were taken and not released yet.
    busy: usize,
}

/// What a worker gets when it asks for work.
#[derive(Debug, PartialEq, Eq)]
pub enum Take {
    Job(usize),
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

    pub fn push(&self, job: usize, priority: u64, path: &str) {
        let mut state = self.state.lock().unwrap();
        state.heap.push(Entry {
            priority,
            tie: Reverse(path.to_string()),
            job,
        });
        self.wake.notify_one();
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
            if let Some(entry) = state.heap.pop() {
                state.busy += 1;
                return Take::Job(entry.job);
            }
            if state.done {
                return Take::Closed;
            }
            state = self.wake.wait(state).unwrap();
        }
    }
}
