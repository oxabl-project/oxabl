//! One worker slot: its process, its stage and what it has been told.

use std::collections::HashSet;
use std::time::{Duration, Instant};

use super::cancel::Cancel;
use super::compile::{CompiledUnit, Limits};
use super::error::WorkerError;
use super::frame::CompileRequest;
use super::logs::Logs;
use super::pool::PoolSpec;
use super::program::Program;
use super::session::Session;
use super::start::{SlotFailure, start_slot};
use super::{raw_fetch, schema_fetch};
use crate::key::{FileDigest, Profile, RawPart, SchemaPart};
use crate::output::Stage;

/// What a replacement worker is started with and checked against.
pub struct Restart {
    pub(super) spec: PoolSpec,
    pub(super) program: Program,
    pub(super) logs: Logs,
    pub(super) cancel: Cancel,
    pub(super) profile: Profile,
}

/// The PROPATH and the options a request names, by the ids oxabl gave them.
pub struct Definitions<'a> {
    pub propath: u64,
    pub entries: &'a [String],
    pub options: u64,
    pub pairs: &'a [(String, String)],
}

/// What one request compiles.
pub struct Job<'a> {
    pub req: u64,
    pub definitions: Definitions<'a>,
    pub xref: &'a str,
    pub debug_list: bool,
    pub listing: bool,
    /// The compile names, in compile order.
    pub units: &'a [String],
}

/// A worker process that finished its handshake.
pub struct Worker {
    pub number: usize,
    pub(super) session: Session,
    stage: Stage,
    starts: usize,
    dead: bool,
    propaths: HashSet<u64>,
    options: HashSet<u64>,
}

impl Worker {
    pub(super) fn new(number: usize, session: Session, spec: &PoolSpec) -> Worker {
        Worker {
            number,
            session,
            stage: Stage::new(&spec.state_dir, &spec.host_root, &spec.build_id, number, 1),
            starts: 1,
            dead: false,
            propaths: HashSet::new(),
            options: HashSet::new(),
        }
    }

    /// Whether no replacement could start after this worker was lost.
    pub fn is_dead(&self) -> bool {
        self.dead
    }

    /// Whether the worker's code page can carry `text`: outside UTF-8 only
    /// ASCII paths can be sent.
    pub fn check_encoding(&self, text: &str) -> Result<(), String> {
        self.session.codec.encode(text).map(|_| ())
    }

    pub fn pid(&self) -> u32 {
        self.session.pid()
    }

    /// The stage this process writes into.
    pub fn stage(&self) -> &Stage {
        &self.stage
    }

    /// Sends `job` and reads its answers up to `END`; `on_done` hears each unit.
    pub fn run(
        &mut self,
        job: &Job<'_>,
        limits: Limits,
        on_done: &mut dyn FnMut(CompiledUnit),
    ) -> Result<(), WorkerError> {
        let definitions = &job.definitions;
        let deadline = std::time::Instant::now() + limits.unit_timeout;
        if !self.propaths.contains(&definitions.propath) {
            self.session
                .define_propath(definitions.propath, definitions.entries, deadline)?;
            self.propaths.insert(definitions.propath);
        }
        if !self.options.contains(&definitions.options) {
            self.session
                .define_options(definitions.options, definitions.pairs, deadline)?;
            self.options.insert(definitions.options);
        }
        self.session.compile(
            &CompileRequest {
                req: job.req,
                propath: definitions.propath,
                options: definitions.options,
                stage: self.stage.worker_path(),
                xref: job.xref,
                debug_list: job.debug_list,
                listing: job.listing,
                units: job.units,
            },
            limits,
            on_done,
        )
    }

    /// Hashes one worker-side file with `HASH`.
    pub fn hash_file(&mut self, path: &str, timeout: Duration) -> Result<FileDigest, WorkerError> {
        self.session.hash(path, Instant::now() + timeout)
    }

    /// Images share `part` of `parts` of every database in `databases`, one
    /// share per database, in that order.
    pub fn image_share(
        &mut self,
        databases: &[String],
        part: usize,
        parts: usize,
        timeout: Duration,
    ) -> Result<Vec<SchemaPart>, WorkerError> {
        databases
            .iter()
            .map(|ldb| {
                schema_fetch::fetch(
                    &mut self.session,
                    ldb,
                    part,
                    parts,
                    Instant::now() + timeout,
                )
            })
            .collect()
    }

    /// Makes share `part` of `parts` of the raw digest of every database.
    pub fn raw_share(
        &mut self,
        databases: &[String],
        part: usize,
        parts: usize,
        timeout: Duration,
    ) -> Result<Vec<RawPart>, WorkerError> {
        databases
            .iter()
            .map(|ldb| {
                raw_fetch::fetch(
                    &mut self.session,
                    ldb,
                    part,
                    parts,
                    crate::key::raw_digest::BUCKETS,
                    Instant::now() + timeout,
                )
            })
            .collect()
    }

    /// Stops the process of a lost worker and starts another in its place,
    /// with a stage of its own. Fails when the replacement cannot start or
    /// sees another environment.
    pub fn replace(&mut self, restart: &Restart) -> Result<(), String> {
        self.dead = true;
        self.session.kill();
        match start_slot(
            &restart.spec,
            &restart.program,
            &restart.logs,
            &restart.cancel,
            self.number,
        ) {
            Ok((session, profile, _)) => {
                let differences = restart.profile.differences(&profile);
                if !differences.is_empty() {
                    let mut session = session;
                    session.kill();
                    return Err(format!(
                        "the replacement worker sees another environment: {}",
                        differences.join("; ")
                    ));
                }
                self.session = session;
                self.starts += 1;
                self.stage = Stage::new(
                    &restart.spec.state_dir,
                    &restart.spec.host_root,
                    &restart.spec.build_id,
                    self.number,
                    self.starts,
                );
                self.propaths.clear();
                self.options.clear();
                self.dead = false;
                Ok(())
            }
            Err(SlotFailure::Cancelled) => Err("cancelled".to_string()),
            Err(SlotFailure::Config(message)) => Err(message),
            Err(SlotFailure::Infra { message, .. }) => Err(message),
        }
    }

    /// `QUIT`, a wait of up to ten seconds, then the process group is stopped.
    pub fn quit(&mut self) {
        self.session.quit();
    }

    /// `SIGTERM` to the process group, `SIGKILL` later.
    pub fn kill(&mut self) {
        self.session.kill();
    }

    /// Stops a worker that is out of step and marks it lost.
    pub fn retire(&mut self) {
        self.dead = true;
        self.session.kill();
    }

    /// Waits up to `wait` for the process to be gone.
    pub fn gone(&mut self, wait: Duration) -> bool {
        let deadline = std::time::Instant::now() + wait;
        while self.session.is_alive() {
            if std::time::Instant::now() >= deadline {
                return false;
            }
            std::thread::sleep(Duration::from_millis(10));
        }
        true
    }
}
