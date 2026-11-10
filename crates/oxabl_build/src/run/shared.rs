//! What every worker thread of one pass reads.

use std::path::Path;
use std::sync::atomic::AtomicU64;
use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};

use rustc_hash::FxHashMap;

use super::context::{Imaging, Verify};
use super::types::{Msg, UnitDone, UnitInfo};
use crate::refresh::Outcome;
use crate::schedule::Queue;
use crate::worker::compile::Limits;
use crate::worker::{Cancel, Restart};

/// The same for all jobs of a pass.
pub struct Shared<'a> {
    pub root: &'a Path,
    pub outcome: &'a Outcome,
    pub infos: &'a FxHashMap<u32, UnitInfo>,
    /// The units of each job.
    pub jobs: Vec<Vec<u32>>,
    pub priorities: Vec<u64>,
    pub queue: Queue,
    /// The worker-side PROPATH entries, by PROPATH id.
    pub propaths: &'a [Vec<String>],
    pub options: &'a [(String, String)],
    pub xref: &'static str,
    pub observed_xref: &'static str,
    pub debug_list: bool,
    pub listing: bool,
    pub limits: Limits,
    pub cancel: &'a Cancel,
    pub verify: Option<&'a Verify<'a>>,
    /// The compiler writes an `XREF-XML` for the units whose closure is
    /// observed.
    pub observing: bool,
    /// The user asked for `XREF-XML`: it serves as the private one.
    pub user_xref: bool,
    pub imaging: Option<&'a Imaging>,
    /// Stop handing out jobs at the first failed unit.
    pub fail_fast: bool,
    pub restart: Arc<Restart>,
    /// How often each unit lost its worker in this build.
    pub lost: &'a Mutex<FxHashMap<u32, u8>>,
    /// The units of each job that no worker has finished or lost yet.
    pub remaining: Mutex<Vec<Vec<u32>>>,
    pub next_request: &'a AtomicU64,
}

impl Shared<'_> {
    /// Reports the completion of a unit to the coordinator.
    pub fn send_done(&self, tx: &Sender<Msg>, done: UnitDone) {
        self.queue.completion_sent();
        if tx.send(Msg::UnitDone(done)).is_err() {
            self.queue.completion_handled();
        }
    }

    /// Puts each `(job, units)` back as the open part of the job and makes the
    /// jobs ready together.
    pub fn requeue_all(&self, jobs: Vec<(usize, Vec<u32>)>) {
        let mut ready = Vec::with_capacity(jobs.len());
        {
            let mut remaining = self.remaining.lock().unwrap();
            for (job, units) in jobs {
                ready.push((
                    job,
                    self.priorities[job],
                    self.infos[&units[0]].path.clone(),
                ));
                remaining[job] = units;
            }
        }
        self.queue.push_all(ready);
    }

    /// Puts `units` back as the open part of `job` and makes the job ready.
    pub fn requeue(&self, job: usize, units: Vec<u32>) {
        self.requeue_all(vec![(job, units)]);
    }
}
