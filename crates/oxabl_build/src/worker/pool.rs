//! The worker pool: parallel start, agreement, the `$DLC` hash phase, shutdown.

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::thread;
use std::time::{Duration, Instant};

use oxabl_workspace::PreprocessorConfig;

use super::cancel::Cancel;
use super::command::{self, Values};
use super::error::{PoolError, WorkerError};
use super::handshake::{self, Context, Failure};
use super::imaging;
use super::logs::{self, Logs};
use super::program::{self, Program};
use super::random;
use super::session::{Session, SpawnSpec};
use crate::config::BuildWorkspace;
use crate::error::BuildError;
use crate::key::FileDigest;
use crate::key::digest::hash_set_digest;
use crate::key::{Environment, Profile};

/// A worker that fails to start is started again at most this often.
const RESTARTS_PER_SLOT: usize = 2;
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

struct Worker {
    number: usize,
    session: Session,
}

/// Workers that finished their handshake and agree on one environment.
pub struct Pool {
    workers: Vec<Worker>,
    unit_timeout: Duration,
}

/// A started pool with what the workers reported.
pub struct Started {
    pub pool: Pool,
    /// The environment all workers agree on.
    pub environment: Environment,
    pub warnings: Vec<String>,
}

enum SlotFailure {
    Config(String),
    Infra { number: usize, message: String },
    Cancelled,
}

type Slot = Result<(Worker, Profile, Vec<String>), SlotFailure>;

fn try_start(
    spec: &PoolSpec,
    program: &Program,
    logs: &Logs,
    cancel: &Cancel,
    number: usize,
) -> Result<(Session, Profile, Vec<String>), Failure> {
    let argv = command::expand(
        &spec.command,
        &Values {
            program: &program.worker_path,
            root: &spec.worker_root,
            worker: number,
            build: &spec.build_id,
        },
    )
    .map_err(Failure::Config)?;
    let io =
        |error: std::io::Error| WorkerError::Spawn(format!("cannot open the worker log: {error}"));
    let stderr = logs.open(number).map_err(io)?;
    let noise = logs.open(number).map_err(io)?;
    let mut session = Session::spawn(
        SpawnSpec {
            argv: &argv,
            cwd: &spec.host_root,
            env: &spec.env,
            stderr,
            noise,
        },
        cancel,
    )?;
    let context = Context {
        worker_root: &spec.worker_root,
        preprocessor: &spec.preprocessor,
        schema_names: &spec.schema_names,
    };
    let deadline = Instant::now() + spec.start_timeout;
    match handshake::run(&mut session, &context, &random::token(), deadline) {
        Ok((environment, warnings)) => Ok((session, environment, warnings)),
        Err(failure) => {
            session.kill();
            Err(failure)
        }
    }
}

fn start_slot(
    spec: &PoolSpec,
    program: &Program,
    logs: &Logs,
    cancel: &Cancel,
    number: usize,
) -> Slot {
    let mut last = String::new();
    for attempt in 1..=RESTARTS_PER_SLOT + 1 {
        if cancel.is_cancelled() {
            return Err(SlotFailure::Cancelled);
        }
        match try_start(spec, program, logs, cancel, number) {
            Ok((session, environment, warnings)) => {
                return Ok((Worker { number, session }, environment, warnings));
            }
            Err(Failure::Config(message)) => return Err(SlotFailure::Config(message)),
            Err(Failure::Worker(WorkerError::Cancelled)) => return Err(SlotFailure::Cancelled),
            Err(Failure::Worker(error)) => {
                last = error.to_string();
                use std::io::Write;
                if let Ok(mut file) = logs.open(number) {
                    let _ = writeln!(
                        file,
                        "oxabl: start {attempt} of worker {number} failed: {last}"
                    );
                }
            }
        }
    }
    Err(SlotFailure::Infra {
        number,
        message: last,
    })
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

impl Pool {
    /// Starts `spec.count` workers at the same time and checks that they
    /// agree. A configuration error, a cancel or an environment mismatch
    /// stops every worker again.
    pub fn start(spec: &PoolSpec, cancel: &Cancel) -> Result<Started, PoolError> {
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
        for slot in slots {
            match slot {
                Ok(ok) => started.push(ok),
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
        let environment = Environment::assemble(profile, schemas);
        let workers = started.into_iter().map(|(worker, _, _)| worker).collect();
        Ok(Started {
            pool: Pool {
                workers,
                unit_timeout: spec.unit_timeout,
            },
            environment,
            warnings,
        })
    }

    pub fn len(&self) -> usize {
        self.workers.len()
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

    /// The `$DLC` hash phase: one worker hashes every path, then every other
    /// worker answers one `HASHSET` over the same sorted list. A difference
    /// is a configuration error. Returns the digests in path order.
    pub fn hash_dlc_files(
        &mut self,
        paths: &[String],
    ) -> Result<Vec<(String, FileDigest)>, PoolError> {
        let mut sorted: Vec<String> = paths.to_vec();
        sorted.sort_by(|a, b| a.as_bytes().cmp(b.as_bytes()));
        sorted.dedup();
        let timeout = self.unit_timeout;
        let infra = |error: WorkerError| match error {
            WorkerError::Cancelled => PoolError::Cancelled,
            other => PoolError::Infra(other.to_string()),
        };
        let (first, rest) = self
            .workers
            .split_first_mut()
            .ok_or_else(|| PoolError::Infra("the pool has no worker".into()))?;
        let mut digests = Vec::with_capacity(sorted.len());
        for path in &sorted {
            let digest = first
                .session
                .hash(path, Instant::now() + timeout)
                .map_err(infra)?;
            digests.push((path.clone(), digest));
        }
        let expected = hash_set_digest(&digests);
        let answers: Vec<(usize, Result<String, WorkerError>)> = thread::scope(|scope| {
            let sorted = &sorted;
            let handles: Vec<_> = rest
                .iter_mut()
                .map(|worker| {
                    scope.spawn(move || {
                        let deadline = Instant::now() + timeout;
                        (worker.number, worker.session.hash_set(sorted, deadline))
                    })
                })
                .collect();
            handles
                .into_iter()
                .map(|handle| handle.join().expect("a hash thread panicked"))
                .collect()
        });
        let mut differing = Vec::new();
        for (number, answer) in answers {
            if answer.map_err(infra)? != expected {
                differing.push(number.to_string());
            }
        }
        if differing.is_empty() {
            Ok(digests)
        } else {
            Err(PoolError::Config(format!(
                "workers see different $DLC files: worker {} differs from worker {}",
                differing.join(", "),
                first.number
            )))
        }
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
