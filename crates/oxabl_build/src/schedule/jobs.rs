//! Jobs: the components that hold units to compile, and what each waits for.

use rustc_hash::FxHashMap;

use super::components::{NONE, Order, components};
use crate::graph::Graph;

/// One request's worth of units: a component of the ordering graph that holds
/// units of the build, compiled together on one worker.
#[derive(Debug, Clone)]
pub struct Job {
    /// The units, sorted by path.
    pub units: Vec<u32>,
    /// The longest chain of jobs that waits for this one, by duration.
    pub priority: u64,
}

/// The jobs of a build, who waits for whom, and how far each has come.
#[derive(Debug)]
pub struct Schedule {
    jobs: Vec<Job>,
    remaining: Vec<usize>,
    waiting: Vec<Vec<usize>>,
}

impl Schedule {
    /// The jobs for compiling `targets`. A component with no target is
    /// pass-through: a job waits for the jobs it reaches through it.
    /// `after` gives further edges, from a unit to the units that must compile
    /// before it; inside one job, a unit with such edges comes last.
    /// `weight` is the duration of a unit and `path` sorts units.
    pub fn build<'a>(
        graph: &Graph,
        after: &FxHashMap<u32, Vec<u32>>,
        nodes: usize,
        targets: &[u32],
        weight: impl Fn(u32) -> u64,
        path: impl Fn(u32) -> &'a str,
    ) -> Schedule {
        let graph = Order {
            graph,
            extra: after,
        };
        let found = components(&graph, nodes, targets);
        let mut is_target = vec![false; nodes];
        for &unit in targets {
            is_target[unit as usize] = true;
        }
        let count = found.members.len();
        let is_job: Vec<bool> = found
            .members
            .iter()
            .map(|members| members.iter().any(|node| is_target[*node as usize]))
            .collect();

        let mut reach: Vec<Vec<usize>> = vec![Vec::new(); count];
        for (c, members) in found.members.iter().enumerate() {
            let mut reached: Vec<usize> = Vec::new();
            for &node in members {
                for to in graph.edges(node) {
                    let other = found.of[to as usize];
                    if other == NONE || other as usize == c {
                        continue;
                    }
                    let other = other as usize;
                    if is_job[other] {
                        reached.push(other);
                    } else {
                        reached.extend(reach[other].iter().copied());
                    }
                }
            }
            reached.sort_unstable();
            reached.dedup();
            reach[c] = reached;
        }

        let mut job_of = vec![usize::MAX; count];
        let mut jobs: Vec<Job> = Vec::new();
        for (c, members) in found.members.iter().enumerate().filter(|(c, _)| is_job[*c]) {
            let mut units: Vec<u32> = members
                .iter()
                .copied()
                .filter(|node| is_target[*node as usize])
                .collect();
            units.sort_by(|a, b| {
                (after.contains_key(a), path(*a)).cmp(&(after.contains_key(b), path(*b)))
            });
            job_of[c] = jobs.len();
            jobs.push(Job { units, priority: 0 });
        }
        let mut remaining = vec![0usize; jobs.len()];
        let mut waiting: Vec<Vec<usize>> = vec![Vec::new(); jobs.len()];
        for (c, _) in found.members.iter().enumerate().filter(|(c, _)| is_job[*c]) {
            let job = job_of[c];
            remaining[job] = reach[c].len();
            for dependency in &reach[c] {
                waiting[job_of[*dependency]].push(job);
            }
        }
        // Components come after the ones they point at, so jobs that wait for
        // a job have a higher number: walk from the end.
        for job in (0..jobs.len()).rev() {
            let own: u64 = jobs[job].units.iter().map(|unit| weight(*unit)).sum();
            let longest = waiting[job]
                .iter()
                .map(|waiter| jobs[*waiter].priority)
                .max()
                .unwrap_or(0);
            jobs[job].priority = own + longest;
        }
        Schedule {
            jobs,
            remaining,
            waiting,
        }
    }

    pub fn len(&self) -> usize {
        self.jobs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.jobs.is_empty()
    }

    pub fn job(&self, job: usize) -> &Job {
        &self.jobs[job]
    }

    /// The jobs that wait for nothing.
    pub fn ready(&self) -> Vec<usize> {
        (0..self.jobs.len())
            .filter(|job| self.remaining[*job] == 0)
            .collect()
    }

    /// Marks `job` finished and returns the jobs that this makes ready.
    pub fn finish(&mut self, job: usize) -> Vec<usize> {
        let mut ready = Vec::new();
        for &waiter in &self.waiting[job] {
            self.remaining[waiter] -= 1;
            if self.remaining[waiter] == 0 {
                ready.push(waiter);
            }
        }
        ready
    }
}
