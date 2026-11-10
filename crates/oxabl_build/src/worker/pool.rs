//! The worker pool: parallel start, agreement, the `$DLC` hash phase, shutdown.

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use oxabl_workspace::PreprocessorConfig;

use super::cancel::Cancel;
use super::command;
use super::error::{PoolError, WorkerError};
use super::imaging;
use super::logs::{self, Logs};
use super::program;
use super::session::Session;
use super::slot::{Restart, Worker};
use super::start::{Slot, SlotFailure, start_slot};
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::key::FileDigest;
use crate::key::digest::hash_set_digest;
use crate::key::{Environment, Profile, SchemaImage};

/// Lines of a worker log shown when no worker starts.
const LOG_TAIL_LINES: usize = 20;

/// Everything the pool needs to start workers.
#[derive(Clone)]
pub struct PoolSpec {
    pub command: Vec<String>,
    pub host_root: PathBuf,
    /// The workspace root as the worker sees it, without a trailing `/`.
    pub worker_root: String,
    /// The state directory on the host: worker programs, logs and stages.
    pub state_dir: PathBuf,
    pub build_id: String,
    pub count: usize,
    pub start_timeout: Duration,
    pub unit_timeout: Duration,
    pub env: BTreeMap<String, String>,
    pub preprocessor: PreprocessorConfig,
    /// The logical names the `[workspace.schema]` files stand for.
    pub schema_names: Vec<String>,
}

/// The number of workers when none is configured: the CPUs, at most 8.
fn default_count() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
        .min(8)
}

/// The message `oxabl build` prints when no worker command is set.
pub const NO_COMMAND: &str = "oxabl build needs a worker command; set [build.workers].command (see docs/design/build-tool/config.md) or run oxabl build --plan";

impl PoolSpec {
    /// Lowers the configuration; fails with a usage error when no worker
    /// command is set or the settings are unusable. `workers` replaces
    /// `[build.workers].count`.
    pub fn from_config(
        workspace: &BuildWorkspace,
        workers: Option<usize>,
        build_id: String,
    ) -> Result<PoolSpec, BuildError> {
        let settings = &workspace.settings;
        let configured = &workspace.config.build.workers;
        if settings.worker_command.is_empty() {
            return Err(BuildError::Usage(NO_COMMAND.into()));
        }
        command::validate(&settings.worker_command)
            .map_err(|message| BuildError::Usage(format!("[build.workers].command: {message}")))?;
        if workers == Some(0) || configured.count == Some(0) {
            return Err(BuildError::Usage(
                "[build.workers].count must be at least 1".into(),
            ));
        }
        let command = settings.worker_command.clone();
        let host_root = settings.root.clone();
        let worker_root = configured
            .root
            .clone()
            .unwrap_or_else(|| host_root.to_string_lossy().to_string());
        let worker_root = worker_root.trim_end_matches('/').to_string();
        let count = workers
            .or(configured.count.map(|n| n as usize))
            .unwrap_or_else(default_count);
        let schema_names = workspace
            .config
            .workspace
            .schema
            .files
            .iter()
            .filter_map(|file| file.file_stem())
            .map(|stem| stem.to_string_lossy().into_owned())
            .collect();
        Ok(PoolSpec {
            command,
            host_root,
            worker_root,
            state_dir: settings.state_dir.clone(),
            build_id,
            count,
            start_timeout: configured.start_timeout.0,
            unit_timeout: configured.unit_timeout.0,
            env: configured.env.clone(),
            preprocessor: workspace.config.workspace.preprocessor.clone(),
            schema_names,
        })
    }
}

/// Workers that finished their handshake and agree on one environment.
pub struct Pool {
    workers: Vec<Worker>,
    unit_timeout: Duration,
    restart: Arc<Restart>,
    /// The time spent in the `$DLC` hash phase so far.
    dlc_hashing: Duration,
}

/// A started pool with what the workers reported.
pub struct Started {
    pub pool: Pool,
    /// The environment all workers agree on.
    pub environment: Environment,
    pub warnings: Vec<String>,
    pub timing: StartTiming,
}

/// How long the start took, in milliseconds.
#[derive(Debug, Clone, Copy, Default)]
pub struct StartTiming {
    /// Starting the workers and their handshakes.
    pub workers_ms: u64,
    /// Imaging every database.
    pub imaging_ms: u64,
}

fn describe_differences(a: &Worker, b: &Worker, left: &Profile, right: &Profile) -> String {
    let lines: Vec<String> = left
        .differences(right)
        .into_iter()
        .map(|line| format!("    {line}"))
        .collect();
    format!(
        "  worker {} vs worker {}:\n{}",
        a.number,
        b.number,
        lines.join("\n")
    )
}

/// Which workers see which `$DLC` files: the workers grouped by the digest
/// of their `HASHSET`, the group that matches the shared answers first.
fn disagreement(sets: &[(usize, String)], expected: &str) -> String {
    let mut groups: Vec<(&str, Vec<String>)> = Vec::new();
    for (number, set) in sets {
        match groups
            .iter_mut()
            .find(|(digest, _)| *digest == set.as_str())
        {
            Some((_, numbers)) => numbers.push(number.to_string()),
            None => groups.push((set.as_str(), vec![number.to_string()])),
        }
    }
    groups.sort_by_key(|(digest, _)| *digest != expected);
    let named: Vec<String> = groups
        .iter()
        .map(|(_, numbers)| format!("worker {}", numbers.join(", ")))
        .collect();
    match named.len() {
        1 => format!("{} hashed the files differently in two passes", named[0]),
        _ => format!("{} differs from {}", named[1..].join(" and "), named[0]),
    }
}

impl Pool {
    /// Starts `spec.count` workers at the same time and checks that they
    /// agree. A configuration error, a cancel or an environment mismatch
    /// stops every worker again.
    pub fn start(spec: &PoolSpec, cancel: &Cancel) -> Result<Started, PoolError> {
        let starting = Instant::now();
        let program = program::materialize(&spec.state_dir, &spec.host_root, &spec.worker_root)
            .map_err(|error| {
                PoolError::Infra(format!("cannot write the worker program: {error}"))
            })?;
        let logs = Logs::create(&spec.state_dir, &spec.build_id)
            .map_err(|error| PoolError::Infra(format!("cannot create the worker logs: {error}")))?;
        let slots: Vec<Slot> = thread::scope(|scope| {
            let handles: Vec<_> = (1..=spec.count)
                .map(|number| {
                    let (program, logs) = (&program, &logs);
                    scope.spawn(move || start_slot(spec, program, logs, cancel, number))
                })
                .collect();
            handles
                .into_iter()
                .map(|handle| handle.join().expect("a start thread panicked"))
                .collect()
        });
        let mut started: Vec<(Worker, Profile, Vec<String>)> = Vec::new();
        let mut failures: Vec<(usize, String)> = Vec::new();
        let mut config_error = None;
        let mut cancelled = false;
        for (at, slot) in slots.into_iter().enumerate() {
            match slot {
                Ok((session, profile, warnings)) => {
                    started.push((Worker::new(at + 1, session, spec), profile, warnings));
                }
                Err(SlotFailure::Config(message)) => config_error = config_error.or(Some(message)),
                Err(SlotFailure::Cancelled) => cancelled = true,
                Err(SlotFailure::Infra { number, message }) => failures.push((number, message)),
            }
        }
        if cancelled || cancel.is_cancelled() {
            return Err(PoolError::Cancelled);
        }
        if let Some(message) = config_error {
            return Err(PoolError::Config(message));
        }
        if started.is_empty() {
            let (number, message) = failures.last().cloned().unwrap_or_default();
            let tail = logs::tail(&logs.path(number), LOG_TAIL_LINES);
            return Err(PoolError::Infra(format!(
                "no worker could start; the last error was: {message}\nlast {LOG_TAIL_LINES} lines of {}:\n{tail}",
                logs.path(number).display()
            )));
        }
        let mut warnings: Vec<String> = Vec::new();
        for (_, _, worker_warnings) in &started {
            for warning in worker_warnings {
                if !warnings.contains(warning) {
                    warnings.push(warning.clone());
                }
            }
        }
        if !failures.is_empty() {
            warnings.push(format!(
                "{} of {} workers could not start; the build goes on with {}: {}",
                failures.len(),
                spec.count,
                started.len(),
                failures
                    .iter()
                    .map(|(number, message)| format!("worker {number}: {message}"))
                    .collect::<Vec<_>>()
                    .join("; ")
            ));
        }
        let first = &started[0];
        let mut mismatches = Vec::new();
        for other in &started[1..] {
            if !first.1.differences(&other.1).is_empty() {
                mismatches.push(describe_differences(&first.0, &other.0, &first.1, &other.1));
            }
        }
        if !mismatches.is_empty() {
            return Err(PoolError::Config(format!(
                "workers see different environments:\n{}",
                mismatches.join("\n")
            )));
        }
        let profile = started[0].1.clone();
        let databases: Vec<String> = profile
            .facts
            .databases
            .iter()
            .map(|d| d.ldb.clone())
            .collect();
        let workers_ms = starting.elapsed().as_millis() as u64;
        let imaging = Instant::now();
        let imaged = {
            let mut sessions: Vec<&mut Session> = started
                .iter_mut()
                .map(|(worker, _, _)| &mut worker.session)
                .collect();
            imaging::image_databases(&mut sessions, &databases, spec.unit_timeout)
        };
        let schemas = match imaged {
            Ok(schemas) => schemas,
            Err(WorkerError::Cancelled) => return Err(PoolError::Cancelled),
            Err(error) => {
                return Err(PoolError::Infra(format!(
                    "cannot image the databases: {error}"
                )));
            }
        };
        let imaging_ms = imaging.elapsed().as_millis() as u64;
        let environment = Environment::assemble(profile.clone(), schemas);
        let workers = started.into_iter().map(|(worker, _, _)| worker).collect();
        let restart = Arc::new(Restart {
            spec: spec.clone(),
            program,
            logs,
            cancel: cancel.clone(),
            profile,
        });
        Ok(Started {
            pool: Pool {
                workers,
                unit_timeout: spec.unit_timeout,
                restart,
                dlc_hashing: Duration::ZERO,
            },
            environment,
            warnings,
            timing: StartTiming {
                workers_ms,
                imaging_ms,
            },
        })
    }

    pub fn len(&self) -> usize {
        self.workers.len()
    }

    /// The workers, for a compile phase that runs one thread for each.
    pub fn workers_mut(&mut self) -> &mut [Worker] {
        &mut self.workers
    }

    /// What replaces a lost worker.
    pub fn restart(&self) -> Arc<Restart> {
        self.restart.clone()
    }

    /// The pool's limits and the `PoolSpec` it started from.
    pub fn spec(&self) -> &PoolSpec {
        &self.restart.spec
    }

    /// The time spent in the `$DLC` hash phase so far, in milliseconds.
    pub fn dlc_hash_ms(&self) -> u64 {
        self.dlc_hashing.as_millis() as u64
    }

    pub fn is_empty(&self) -> bool {
        self.workers.is_empty()
    }

    /// The process ids of the workers.
    pub fn pids(&self) -> Vec<u32> {
        self.workers
            .iter()
            .map(|worker| worker.session.pid())
            .collect()
    }

    /// The `$DLC` hash phase: each worker hashes a share of the paths, then
    /// every worker answers one `HASHSET` over the whole sorted list, which
    /// must equal the digest of the shares. A difference is a configuration
    /// error. Returns the digests in path order.
    pub fn hash_dlc_files(
        &mut self,
        paths: &[String],
    ) -> Result<Vec<(String, FileDigest)>, PoolError> {
        let started = Instant::now();
        let hashed = self.hash_shares(paths);
        self.dlc_hashing += started.elapsed();
        hashed
    }

    fn hash_shares(&mut self, paths: &[String]) -> Result<Vec<(String, FileDigest)>, PoolError> {
        let mut sorted: Vec<String> = paths.to_vec();
        sorted.sort_by(|a, b| a.as_bytes().cmp(b.as_bytes()));
        sorted.dedup();
        if self.workers.is_empty() {
            return Err(PoolError::Infra("the pool has no worker".into()));
        }
        let timeout = self.unit_timeout;
        let infra = |error: WorkerError| match error {
            WorkerError::Cancelled => PoolError::Cancelled,
            other => PoolError::Infra(other.to_string()),
        };
        let shares = self.workers.len();
        type Answer = Result<(Vec<(usize, FileDigest)>, Option<String>), WorkerError>;
        let answers: Vec<(usize, Answer)> = thread::scope(|scope| {
            let sorted = &sorted;
            let handles: Vec<_> = self
                .workers
                .iter_mut()
                .enumerate()
                .map(|(share, worker)| {
                    scope.spawn(move || {
                        let mut run = || -> Answer {
                            let mut digests = Vec::new();
                            for at in (share..sorted.len()).step_by(shares) {
                                let deadline = Instant::now() + timeout;
                                digests.push((at, worker.session.hash(&sorted[at], deadline)?));
                            }
                            let set = match shares {
                                1 => None,
                                _ => Some(
                                    worker.session.hash_set(sorted, Instant::now() + timeout)?,
                                ),
                            };
                            Ok((digests, set))
                        };
                        (worker.number, run())
                    })
                })
                .collect();
            handles
                .into_iter()
                .map(|handle| handle.join().expect("a hash thread panicked"))
                .collect()
        });
        let mut slots: Vec<Option<FileDigest>> = vec![None; sorted.len()];
        let mut sets: Vec<(usize, String)> = Vec::new();
        for (number, answer) in answers {
            let (digests, set) = answer.map_err(infra)?;
            for (at, digest) in digests {
                slots[at] = Some(digest);
            }
            sets.extend(set.map(|set| (number, set)));
        }
        let digests: Vec<(String, FileDigest)> = sorted
            .into_iter()
            .zip(slots)
            .map(|(path, digest)| (path, digest.expect("every path is in one share")))
            .collect();
        let expected = hash_set_digest(&digests);
        if sets.iter().all(|(_, set)| *set == expected) {
            return Ok(digests);
        }
        Err(PoolError::Config(format!(
            "workers see different $DLC files: {}",
            disagreement(&sets, &expected)
        )))
    }

    /// Images every database across the live workers, as the probe does.
    pub fn image(&mut self, databases: &[String]) -> Result<Vec<SchemaImage>, WorkerError> {
        let mut sessions: Vec<&mut Session> = self
            .workers
            .iter_mut()
            .filter(|worker| !worker.is_dead())
            .map(|worker| &mut worker.session)
            .collect();
        if sessions.is_empty() {
            return Err(WorkerError::Lost("no worker is left".into()));
        }
        imaging::image_databases(&mut sessions, databases, self.unit_timeout)
    }

    /// Sends `QUIT` to every worker and stops what does not quit.
    pub fn shutdown(self) {
        let Pool { mut workers, .. } = self;
        thread::scope(|scope| {
            for worker in &mut workers {
                scope.spawn(move || worker.session.quit());
            }
        });
    }
}
